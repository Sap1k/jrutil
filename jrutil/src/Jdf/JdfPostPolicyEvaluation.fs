// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Replay of heuristic and learned post-inference policies over evidence.
module JrUtil.JdfPostPolicyEvaluation

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open Parquet
open Parquet.Schema
open JrUtil.JdfPostEvidenceReader

let internal consolidateReplayRows (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                                  (points:ReplayRoutePoint array)
                                  (rows:ReplayScore array) =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "consolidation"
    if points.Length=0 then rows,Map.empty else
    let contextIdentity (row:ReplayScore) =
        row.contextId,row.variantRank
    let rowsByPoint =
        rows |> Array.groupBy(fun value -> value.candidateId)
             |> Array.map(fun (candidate,values) ->
                 candidate,values |> Array.map(fun value -> contextIdentity value,value) |> Map.ofArray)
             |> Map.ofArray
    let metresBetween (left:ReplayRoutePoint) (right:ReplayRoutePoint) =
        let latitude=(left.latitude+right.latitude)*0.5*Math.PI/180.0
        let dx=(left.longitude-right.longitude)*111320.0*Math.Cos(latitude)
        let dy=(left.latitude-right.latitude)*110540.0
        Math.Sqrt(dx*dx+dy*dy),dx,dy
    let headingDifference left right =
        let difference=abs(left-right)%360.0
        min difference (360.0-difference)
    let compatiblePoints (left:ReplayRoutePoint) (right:ReplayRoutePoint) =
        let distance,dx,dy=metresBetween left right
        if not(JdfPostInference.withinConsolidationDiameter policy distance) then false
        elif (left.hasCurrentLifecycle<>right.hasCurrentLifecycle
              || left.hasObsoleteLifecycle<>right.hasObsoleteLifecycle)
             && (left.hasObsoleteLifecycle || right.hasObsoleteLifecycle) then false
        else
            let leftRows=rowsByPoint |> Map.tryFind left.routePointId |> Option.defaultValue Map.empty
            let rightRows=rowsByPoint |> Map.tryFind right.routePointId |> Option.defaultValue Map.empty
            let mutable incompatible=false
            let mutable distinguished=0
            for KeyValue(context,leftRow) in leftRows do
                match rightRows |> Map.tryFind context with
                | None -> ()
                | Some rightRow ->
                    match leftRow.corridorFaceId,rightRow.corridorFaceId with
                    | Some leftFace,Some rightFace when leftFace<>rightFace -> incompatible<-true
                    | _ -> ()
                    if not(JdfPostInference.consolidationSidesCompatible policy
                               leftRow.signedLateralOffset rightRow.signedLateralOffset) then
                        incompatible<-true
                    match leftRow.corridorHeading,rightRow.corridorHeading with
                    | Some leftHeading,Some rightHeading ->
                        if not(JdfPostInference.consolidationHeadingCompatible policy
                                   (headingDifference leftHeading rightHeading)) then incompatible<-true
                        let radians=leftHeading*Math.PI/180.0
                        let chainage=abs(dx*Math.Sin(radians)+dy*Math.Cos(radians))
                        if not(JdfPostInference.consolidationChainageCompatible policy chainage) then
                            incompatible<-true
                    | _ -> ()
                    let score (row:ReplayScore) =
                        let routed=row.routedExcess |> Option.map(JdfPostInference.routedFit policy) |> Option.defaultValue 0.0
                        JdfPostInference.evidenceGeometryScore policy row.corridorDistance
                            row.signedLateralOffset row.corridorHeading row.attachmentHeading routed
                    let leftScore=score leftRow
                    let rightScore=score rightRow
                    if JdfPostInference.consolidationContextDistinguishes policy leftScore rightScore then
                        distinguished<-distinguished+1
            not incompatible
            && JdfPostInference.consolidationDistinguishedCountCompatible policy distinguished
    let clusters=ResizeArray<ReplayRoutePoint array>()
    for point in points |> Array.sortBy(fun value -> value.routePointId) do clusters.Add([|point|])
    let mutable changed=true
    while changed do
        changed<-false
        let selected =
            seq {
                for leftIndex=0 to clusters.Count-1 do
                    for rightIndex=leftIndex+1 to clusters.Count-1 do
                        let left=clusters.[leftIndex]
                        let right=clusters.[rightIndex]
                        if left |> Array.forall(fun leftPoint ->
                            right |> Array.forall(compatiblePoints leftPoint)) then
                            let diameter =
                                seq {
                                    for leftPoint in left do
                                        for rightPoint in right do
                                            let distance,_,_=metresBetween leftPoint rightPoint
                                            yield distance
                                }
                                |> Seq.max
                            let identity =
                                Array.append left right |> Array.map _.routePointId |> Array.sort
                            yield diameter,String.Join("|",identity),leftIndex,rightIndex
            }
            |> Seq.sortBy(fun (diameter,identity,_,_) -> diameter,identity)
            |> Seq.tryHead
        match selected with
        | Some(_,_,leftIndex,rightIndex) ->
            let merged=Array.append clusters.[leftIndex] clusters.[rightIndex]
                       |> Array.sortBy(fun value -> value.routePointId)
            clusters.[leftIndex]<-merged
            clusters.RemoveAt(rightIndex)
            changed<-true
        | None -> ()
    let pointToHypothesis =
        clusters
        |> Seq.collect(fun cluster ->
            let identities=cluster |> Array.collect(fun point -> point.observationIds)
                                  |> Array.distinct |> Array.sort
            let identities=if identities.Length=0 then cluster |> Array.map(fun point -> point.routePointId) else identities
            let joined=String.Join("|",identities)
            let payload = $"{cluster.[0].stopId}|{joined}"
            let hypothesis=SHA256.HashData(Encoding.UTF8.GetBytes(payload)) |> Convert.ToHexString
                           |> fun value -> value.ToLowerInvariant()
            cluster |> Seq.map(fun point -> point.routePointId,hypothesis))
        |> Map.ofSeq
    let hypothesisDetails =
        clusters
        |> Seq.map(fun cluster ->
            let hypothesis=pointToHypothesis.[cluster.[0].routePointId]
            let medoid=cluster |> Array.minBy(fun candidate ->
                cluster |> Array.sumBy(fun other -> let distance,_,_=metresBetween candidate other in distance),
                candidate.routePointId)
            hypothesis,(medoid,
                        cluster |> Array.exists _.hasCurrentLifecycle,
                        cluster |> Array.exists _.hasObsoleteLifecycle,
                        cluster |> Array.sumBy _.supportWeight,
                        cluster |> Array.collect _.explicitModes |> Array.distinct,
                        cluster |> Array.collect _.deniedModes |> Array.distinct,
                        cluster))
        |> Map.ofSeq
    let aggregated =
      rows
      |> Array.groupBy(fun row ->
        contextIdentity row,row.movementFamilyId,
        (pointToHypothesis |> Map.tryFind row.candidateId |> Option.defaultValue row.candidateId))
      |> Array.map(fun ((_,_,hypothesis),values) ->
        let first=values.[0]
        let medianOption selector=values |> Seq.choose selector |> medianFloat |> Some
        let uniqueOption selector=values |> Seq.choose selector |> Seq.distinct |> Seq.toArray
                                 |> function [|value|] -> Some value | _ -> None
        { first with candidateId=hypothesis
                     alignment=values |> Seq.map(fun value -> value.alignment) |> medianFloat
                     side=values |> Seq.map(fun value -> value.side) |> medianFloat
                     proximity=values |> Seq.map(fun value -> value.proximity) |> medianFloat
                     routedExcess=if values |> Array.exists(fun value -> value.routedExcess.IsSome)
                                  then medianOption(fun value -> value.routedExcess) else None
                     corridorFaceId=uniqueOption(fun value -> value.corridorFaceId)
                     corridorDistance=if values |> Array.exists(fun value -> value.corridorDistance.IsSome)
                                      then medianOption(fun value -> value.corridorDistance) else None
                     signedLateralOffset=if values |> Array.exists(fun value -> value.signedLateralOffset.IsSome)
                                         then medianOption(fun value -> value.signedLateralOffset) else None
                     corridorHeading=uniqueOption(fun value -> value.corridorHeading)
                     attachmentHeading=uniqueOption(fun value -> value.attachmentHeading)
                     alternativeCorridorCount=values |> Array.maxBy(fun value -> value.alternativeCorridorCount)
                                                     |> fun value -> value.alternativeCorridorCount
                     alternativeCostGap=values |> Seq.choose _.alternativeCostGap |> Seq.sort |> Seq.tryHead
                     tiedCorridorsAgree=values |> Array.forall(fun value -> value.tiedCorridorsAgree)
                     topologyFailureReason=
                        if values |> Array.exists(fun value -> value.topologyFailureReason.IsNone) then None
                        else values |> Seq.choose(fun value -> value.topologyFailureReason) |> Seq.sort |> Seq.tryHead
                     })
    aggregated,hypothesisDetails

let internal replaySupportAdjustments (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                                     mode supportWeight explicitModes deniedModes =
    JdfPostInference.modalitySupportAdjustments policy mode supportWeight explicitModes deniedModes

let internal evaluateReplayPolicy capturedHorizon
                                (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                                (points:ReplayRoutePoint array)
                                (rows:ReplayScore array) =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "scoring"
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "resolution"
    let policy=JdfPostInferencePolicy.validatePolicy capturedHorizon policy
    let rows,hypothesisDetails=consolidateReplayRows policy points rows
    let rows =
        rows
        |> Array.groupBy _.contextId
        |> Array.collect(fun (_,contextRows) ->
            let selectedRanks =
                contextRows
                |> Array.map(fun row ->
                    ({ variantRank=row.variantRank
                       relativeCostMetres=row.relativeCostMetres
                       relativeCostFraction=row.relativeCostFraction }
                     : JdfPostInference.AlternativeCorridorFact))
                |> JdfPostInference.selectedCorridorVariantRanks policy
                |> Set.ofArray
            contextRows
            |> Array.groupBy _.candidateId
            |> Array.choose(fun (_,candidateRows) ->
                candidateRows |> Array.tryFind(fun row -> row.variantRank=0)
                |> Option.map(fun baseline ->
                    let selected = candidateRows |> Array.filter(fun row -> selectedRanks.Contains row.variantRank)
                    let complete=selected.Length=selectedRanks.Count
                    let valid=complete && selected |> Array.forall(fun row ->
                        row.topologyFailureReason.IsNone && row.routingAvailability="available"
                        && row.corridorFaceId.IsSome)
                    let faces=selected |> Array.choose _.corridorFaceId |> Array.distinct
                    { baseline with
                        alternativeCorridorCount=selectedRanks.Count
                        alternativeCostGap=selected |> Seq.choose _.relativeCostMetres |> Seq.filter(fun value -> value>0.0) |> Seq.sort |> Seq.tryHead
                        tiedCorridorsAgree=valid && faces.Length=1
                        topologyFailureReason=
                            if baseline.topologyFailureReason.IsSome then baseline.topologyFailureReason
                            elif valid && faces.Length=1 then None
                            else Some "alternative-corridor-invariant" })))
    let coordinates candidateId =
        hypothesisDetails |> Map.tryFind candidateId
        |> Option.map(fun (value,_,_,_,_,_,_) -> Some value.latitude,Some value.longitude)
        |> Option.defaultValue(None,None)
    let isolationAdjustment candidateId =
        match hypothesisDetails |> Map.tryFind candidateId with
        | None -> 0.0
        | Some(point,_,_,_,_,_,_) ->
            let nearest =
                hypothesisDetails
                |> Seq.choose(fun pair ->
                    if pair.Key=candidateId then None else
                    let other,_,_,_,_,_,_=pair.Value
                    let dlat=(point.latitude-other.latitude)*111_320.0
                    let dlon=(point.longitude-other.longitude)*111_320.0
                             *Math.Cos((point.latitude+other.latitude)*Math.PI/360.0)
                    let distance=Math.Sqrt(dlat*dlat+dlon*dlon)
                    Some distance)
                |> Seq.sort |> Seq.tryHead
            JdfPostInference.spatialIsolationAdjustment policy nearest
    let alternativeApplies (row:ReplayScore) =
        row.alternativeCorridorCount>1
    let knownModes =
        hypothesisDetails
        |> Seq.map(fun pair ->
            let point,_,_,_,explicitModes,_,_=pair.Value
            pair.Key,point.stopId,point.latitude,point.longitude,explicitModes)
        |> Seq.toArray
        |> JdfPostInference.effectiveModes
    let evaluated =
        rows
        |> Array.map(fun row ->
            let lifecycleFailure =
                hypothesisDetails |> Map.tryFind row.candidateId
                |> Option.bind(fun (_,hasCurrent,hasObsolete,_,_,_,_) ->
                    if hasObsolete && not hasCurrent then Some "obsolete-lifecycle" else None)
                |> Option.orElse(
                    knownModes |> Map.tryFind row.candidateId
                    |> Option.bind(fun modes ->
                        if JdfPostInference.modeIncompatible row.mode modes then Some "mode-incompatible" else None))
            let rejection=JdfPostInference.hardGateReason policy (lifecycleFailure |> Option.orElse row.topologyFailureReason)
                              row.corridorDistance row.signedLateralOffset row.routedExcess
            let routedFit=row.routedExcess |> Option.map(JdfPostInference.routedFit policy) |> Option.defaultValue 0.0
            let geometry=JdfPostInference.evidenceGeometryScore policy row.corridorDistance
                             row.signedLateralOffset row.corridorHeading row.attachmentHeading routedFit
            let sourceAdjustment,modalityAdjustment =
                match hypothesisDetails |> Map.tryFind row.candidateId with
                | Some(_,_,_,supportWeight,explicitModes,deniedModes,_) ->
                    replaySupportAdjustments policy row.mode supportWeight explicitModes deniedModes
                | _ -> 0.0,0.0
            let support=JdfPostInference.combinedSupportAdjustment policy sourceAdjustment modalityAdjustment
            row,rejection,geometry,JdfPostInference.supportingScore geometry support,routedFit)
    let contextKey (row:ReplayScore) =
        row.stopId,row.mode,row.lineId,row.routeDistinction,row.direction,row.patternHash,row.position,row.role,row.movementFamilyId
    let contextWinners =
        evaluated
        |> Array.groupBy(fun (row,_,_,_,_) -> contextKey row)
        |> Array.choose(fun (key,values) ->
            let ranked=values |> Array.filter(fun (_,rejection,_,_,_) -> rejection.IsNone)
                              |> Array.sortBy(fun (row,_,geometry,final,_) -> -final,-geometry,row.candidateId)
            if ranked.Length=0 then None else
            let row,_,geometry,final,routed=ranked.[0]
            let runner=ranked |> Array.tryItem 1
            let lead=runner |> Option.map(fun (_,_,_,value,_) -> final-value) |> Option.defaultValue final
            Some(key,(row.candidateId,geometry,final,routed,lead,row.corridorFaceId,
                      (if alternativeApplies row then row.alternativeCorridorCount else 1),
                      (not(alternativeApplies row) || row.tiedCorridorsAgree))))
        |> Map.ofSeq
    let jointCandidates=Dictionary<struct(int64*string),ReplayJointCandidate array>()
    let ordinaryDecisions =
        evaluated
        |> Array.groupBy(fun (row,_,_,_,_) -> row.stopId,row.movementFamilyId)
        |> Array.map(fun ((stopId,familyId),familyRows) ->
        let first,_,_,_,_=familyRows.[0]
        let contexts=familyRows |> Array.map(fun (row,_,_,_,_) -> contextKey row) |> Array.distinct
        let winners=contexts |> Array.choose(fun key -> contextWinners |> Map.tryFind key)
        let winnerCounts=winners |> Array.countBy(fun (candidate,_,_,_,_,_,_,_) -> candidate)
                                |> Array.sortBy(fun (candidate,count) -> -count,candidate)
        let externalSupport =
            contextWinners
            |> Seq.filter(fun pair -> let _,_,_,_,_,_,_,_,movementFamilyId=pair.Key in movementFamilyId<>familyId)
            |> Seq.groupBy(fun pair -> let candidate,_,_,_,_,_,_,_=pair.Value in candidate)
            |> Seq.map(fun (candidate,values) -> candidate,min 5 (Seq.length values))
            |> Map.ofSeq
        let candidateScores =
            familyRows
            |> Array.filter(fun (_,rejection,_,_,_) -> rejection.IsNone)
            |> Array.groupBy(fun (row,_,_,_,_) -> row.candidateId)
            |> Array.map(fun (candidate,values) ->
                let geometry=values |> Seq.map(fun (_,_,value,_,_) -> value) |> medianFloat
                let final=values |> Seq.map(fun (_,_,_,value,_) -> value) |> medianFloat
                let establishedAdjustment =
                    externalSupport |> Map.tryFind candidate |> Option.defaultValue 0
                    |> JdfPostInference.establishedPopularityAdjustment policy
                let final=JdfPostInference.supportingScore final establishedAdjustment
                let final=JdfPostInference.supportingScore final (isolationAdjustment candidate)
                let routed=values |> Seq.map(fun (_,_,_,_,value) -> value) |> medianFloat
                let faces=values |> Seq.choose(fun (row,_,_,_,_) -> row.corridorFaceId) |> Set
                let alternatives=values |> Seq.exists(fun (row,_,_,_,_) -> alternativeApplies row)
                let agree=values |> Seq.forall(fun (row,_,_,_,_) -> not(alternativeApplies row) || row.tiedCorridorsAgree)
                candidate,geometry,final,routed,faces,alternatives,agree)
            |> Array.sortBy(fun (candidate,geometry,final,_,_,_,_) -> -final,-geometry,candidate)
        if candidateScores.Length=0 then
            { stopId=stopId;movementFamilyId=familyId;mode=first.mode;lineId=first.lineId;role=first.role
              resolution="Centroid";candidateId=None;corridorFaceId=None;score=None;margin=None;unsafePhysical=false
              representativeCandidateId=None
              latitude=None;longitude=None }
        else
            let candidate,geometry,final,routed,faces,alternatives,alternativesAgree=candidateScores.[0]
            let runner=candidateScores |> Array.tryItem 1
            let margin=runner |> Option.map(fun (_,_,value,_,_,_,_) -> final-value) |> Option.defaultValue final
            let geometryMargin=runner |> Option.map(fun (_,value,_,_,_,_,_) -> geometry-value) |> Option.defaultValue geometry
            let routedAdvantage=runner |> Option.map(fun (_,_,_,value,_,_,_) -> routed-value) |> Option.defaultValue routed
            let winnerSupport=winnerCounts |> Array.tryFind(fun (value,_) -> value=candidate) |> Option.map snd |> Option.defaultValue 0
            let winnerShare=if contexts.Length=0 then 0.0 else float winnerSupport/float contexts.Length
            let contradictory =
                winners |> Array.exists(fun (other,_,score,_,lead,_,_,_) ->
                    other<>candidate && JdfPostInference.contradictoryContext policy score lead)
            let established=externalSupport |> Map.tryFind candidate |> Option.defaultValue 0
            let establishedRunner =
                externalSupport |> Seq.filter(fun pair -> pair.Key<>candidate)
                |> Seq.map(fun pair -> pair.Value) |> Seq.sortDescending |> Seq.tryHead |> Option.defaultValue 0
            let establishedStrong =
                JdfPostInference.establishedPostStrong policy established establishedRunner
                    geometryMargin contradictory alternatives alternativesAgree
            // Same-stop preference is a joint tie-break between candidates
            // that have already passed these ordinary gates; it must never
            // make an otherwise ineligible candidate publishable.
            let physical =
                JdfPostInference.physicalResolutionEligible policy {
                    finalScore=final;margin=margin;routedAdvantage=routedAdvantage
                    geometryMargin=geometryMargin;contradictory=contradictory
                    contextCount=contexts.Length;winnerShare=winnerShare
                    establishedStrong=establishedStrong
                    hasAlternativeCorridors=alternatives;alternativesAgree=alternativesAgree }
            if physical then
                let tiedCandidates =
                    candidateScores
                    |> Array.filter(fun (_,otherGeometry,otherFinal,_,_,_,_) ->
                        abs(otherFinal-final)<=1e-12 && abs(otherGeometry-geometry)<=1e-12)
                    |> Array.choose(fun (other,otherGeometry,otherFinal,otherRouted,otherFaces,
                                          otherAlternatives,otherAlternativesAgree) ->
                        let lower =
                            candidateScores
                            |> Array.tryFind(fun (_,candidateGeometry,candidateFinal,_,_,_,_) ->
                                candidateFinal<otherFinal-1e-12
                                || (abs(candidateFinal-otherFinal)<=1e-12
                                    && candidateGeometry<otherGeometry-1e-12))
                        let individualMargin =
                            lower
                            |> Option.map(fun (_,_,candidateFinal,_,_,_,_) -> otherFinal-candidateFinal)
                            |> Option.defaultValue otherFinal
                        let otherWinnerSupport =
                            winnerCounts |> Array.tryFind(fun (value,_) -> value=other)
                            |> Option.map snd |> Option.defaultValue 0
                        let otherWinnerShare =
                            if contexts.Length=0 then 0.0
                            else float otherWinnerSupport/float contexts.Length
                        let otherContradictory =
                            winners |> Array.exists(fun (candidateValue,_,score,_,lead,_,_,_) ->
                                candidateValue<>other && score>=policy.consensus.minimumPerContextScore
                                && lead>=policy.consensus.minimumPerContextLead)
                        let otherConsensus =
                            contexts.Length>=policy.consensus.minimumContexts
                            && otherWinnerShare>=policy.consensus.minimumWinningShare
                            && not otherContradictory
                        let otherEstablished = externalSupport |> Map.tryFind other |> Option.defaultValue 0
                        let otherEstablishedRunner =
                            externalSupport |> Seq.filter(fun pair -> pair.Key<>other)
                            |> Seq.map(fun pair -> pair.Value) |> Seq.sortDescending
                            |> Seq.tryHead |> Option.defaultValue 0
                        let otherEstablishedStrong =
                            JdfPostInference.establishedPostStrong policy otherEstablished
                                otherEstablishedRunner 0.0 otherContradictory
                                otherAlternatives otherAlternativesAgree
                        let otherMaterialRouted =
                            match lower with
                            | Some(_,lowerGeometry,_,lowerRouted,_,_,_) ->
                                otherRouted-lowerRouted>=policy.resolution.materialRoutedAdvantage
                                && otherGeometry-lowerGeometry>= -0.000001
                            | None -> otherRouted>=policy.resolution.materialRoutedAdvantage
                        let independentlyPublishable =
                            otherFinal>=policy.resolution.minimumPhysicalScore
                            && (individualMargin>=policy.resolution.minimumPhysicalMargin
                                || otherMaterialRouted)
                            && not otherContradictory
                            && (contexts.Length<policy.consensus.minimumContexts
                                || otherConsensus || otherEstablishedStrong)
                            && (not otherAlternatives || otherAlternativesAgree)
                        if independentlyPublishable
                           && otherFinal>=policy.sameStopPairs.minimumIndividualScore
                           && individualMargin>=policy.sameStopPairs.minimumIndividualMargin then
                            let latitude,longitude=coordinates other
                            Some { candidateId=other;corridorFaceId=otherFaces |> Seq.tryHead
                                   score=otherFinal;geometry=otherGeometry;margin=individualMargin
                                   latitude=latitude;longitude=longitude }
                        else None)
                jointCandidates.[struct(stopId,familyId)]<-tiedCandidates
                let latitude,longitude=coordinates candidate
                { stopId=stopId;movementFamilyId=familyId;mode=first.mode;lineId=first.lineId;role=first.role
                  resolution="Physical";candidateId=Some candidate;corridorFaceId=faces |> Seq.tryHead
                  representativeCandidateId=Some candidate
                  score=Some final;margin=Some margin;unsafePhysical=alternatives && not alternativesAgree
                  latitude=latitude;longitude=longitude }
            else
                let _,topGeometry,_,_,_,_,_=candidateScores.[0]
                let plausible=candidateScores |> Array.filter(fun (_,geometry,final,_,_,_,_) ->
                    JdfPostInference.plausibleCandidate policy topGeometry geometry final)
                let plausibleFaces=plausible |> Seq.collect(fun (_,_,_,_,values,_,_) -> values) |> Set
                let conflictingAlternative =
                    plausible |> Array.exists(fun (_,_,_,_,_,hasAlternatives,agrees) -> hasAlternatives && not agrees)
                let plausibleCoordinates =
                    plausible |> Array.choose(fun (id,_,_,_,_,_,_) ->
                        match coordinates id with Some lat,Some lon -> Some(lat,lon) | _ -> None)
                let compactness =
                    seq {
                        for left in plausibleCoordinates do
                            for right in plausibleCoordinates do
                                let leftLat,leftLon=left
                                let rightLat,rightLon=right
                                let dlat=(leftLat-rightLat)*111_320.0
                                let dlon=(leftLon-rightLon)*111_320.0*Math.Cos((leftLat+rightLat)*Math.PI/360.0)
                                yield Math.Sqrt(dlat*dlat+dlon*dlon)
                    } |> Seq.append [0.0] |> Seq.max
                if JdfPostInference.sideResolutionEligible policy plausible.Length plausibleFaces.Count
                       conflictingAlternative plausibleFaces.Count contexts.Length compactness then
                    let latitude,longitude=coordinates candidate
                    { stopId=stopId;movementFamilyId=familyId;mode=first.mode;lineId=first.lineId;role=first.role
                      resolution="Side";candidateId=None;corridorFaceId=plausibleFaces |> Seq.tryHead
                      representativeCandidateId=Some candidate
                      score=Some final;margin=Some margin;unsafePhysical=false
                      latitude=latitude;longitude=longitude }
                else
                    { stopId=stopId;movementFamilyId=familyId;mode=first.mode;lineId=first.lineId;role=first.role
                      resolution="Centroid";candidateId=None;corridorFaceId=None
                      representativeCandidateId=None
                      score=None;margin=None;unsafePhysical=false;latitude=None;longitude=None })
    let decisionsByFamily =
        ordinaryDecisions
        |> Array.mapi(fun index value -> struct(value.stopId,value.movementFamilyId),(index,value))
        |> Map.ofArray
    let mutable sameStopBlocks=0
    let mutable distinctPairChoices=0
    let mutable unresolvedBlockEdges=0
    if policy.sameStopPairs.enabled then
        rows
        |> Array.groupBy _.sameStopBlockId
        |> Array.choose(fun (blockId,blockRows) -> blockId |> Option.map(fun value -> value,blockRows))
        |> Array.sortBy fst
        |> Array.iter(fun (_,blockRows) ->
            sameStopBlocks<-sameStopBlocks+1
            let contexts = blockRows |> Array.distinctBy _.contextId
            if contexts.Length<>2 then unresolvedBlockEdges<-unresolvedBlockEdges+1 else
            let left,right=contexts.[0],contexts.[1]
            match decisionsByFamily |> Map.tryFind(struct(left.stopId,left.movementFamilyId)),
                  decisionsByFamily |> Map.tryFind(struct(right.stopId,right.movementFamilyId)) with
            | Some(leftIndex,leftDecision),Some(rightIndex,rightDecision)
                when JdfPostInference.sameStopDistinctnessEligible {
                         contextCount=contexts.Length
                         leftAssignmentKind=left.assignmentKind
                         rightAssignmentKind=right.assignmentKind
                         leftMovementFamilyId=left.movementFamilyId
                         rightMovementFamilyId=right.movementFamilyId
                         leftResolution=leftDecision.resolution
                         rightResolution=rightDecision.resolution
                         leftCandidateId=leftDecision.candidateId
                         rightCandidateId=rightDecision.candidateId } ->
                    let leftChoices =
                        match jointCandidates.TryGetValue(struct(left.stopId,left.movementFamilyId)) with
                        | true,values -> values | _ -> [||]
                    let rightChoices =
                        match jointCandidates.TryGetValue(struct(right.stopId,right.movementFamilyId)) with
                        | true,values -> values | _ -> [||]
                    let policyChoices values =
                        values |> Array.map(fun value ->
                            ({ candidateId=value.candidateId;ordinaryScore=value.score
                               ordinaryGeometry=value.geometry;individualMargin=value.margin
                               independentlyPublishable=true }
                             : JdfPostInference.SameStopPairChoice))
                    let pair =
                        JdfPostInference.selectDistinctSameStopPair policy.sameStopPairs
                            (policyChoices leftChoices) (policyChoices rightChoices)
                        |> Option.bind(fun (leftId,rightId) ->
                            match leftChoices |> Array.tryFind(fun value -> value.candidateId=leftId),
                                  rightChoices |> Array.tryFind(fun value -> value.candidateId=rightId) with
                            | Some leftChoice,Some rightChoice -> Some(leftChoice,rightChoice)
                            | _ -> None)
                    match pair with
                    | Some(leftChoice,rightChoice) ->
                        let replace decision choice =
                            { decision with candidateId=Some choice.candidateId
                                            representativeCandidateId=Some choice.candidateId
                                            corridorFaceId=choice.corridorFaceId
                                            score=Some choice.score;margin=Some choice.margin
                                            latitude=choice.latitude;longitude=choice.longitude }
                        ordinaryDecisions.[leftIndex]<-replace leftDecision leftChoice
                        ordinaryDecisions.[rightIndex]<-replace rightDecision rightChoice
                        distinctPairChoices<-distinctPairChoices+1
                    | None -> unresolvedBlockEdges<-unresolvedBlockEdges+1
            | _ -> () )
    ordinaryDecisions
    |> Array.sortBy(fun value -> value.stopId,value.mode,value.lineId,value.movementFamilyId),
    { sameStopBlocks=sameStopBlocks;distinctPairChoices=distinctPairChoices
      unresolvedBlockEdges=unresolvedBlockEdges },hypothesisDetails

[<Literal>]
let internal LearnedExcessCapMetres = 1000.0

[<Literal>]
let internal MinimumNumberedBays = 3

let internal roadModes = set [ "ROAD"; "BUS"; "TROLLEYBUS"; "SHARED" ]

/// The learned scorer's candidate table and decisions for one stop, reproducing
/// post_scorer.features.build_candidate_table exactly: candidates only stop-level
/// sources know about are dropped, candidates are clustered into physical posts and
/// areas, post modes exclude incompatible candidates per context, and the frozen
/// feature columns are computed by name.
let internal learnedStopDecisions (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                                 (model:JdfPostScorer.Model) (facts:ObservationFacts)
                                 (points:ReplayRoutePoint array) (values:ReplayScore array) =
    let rows,details=consolidateReplayRows policy points values
    if rows.Length=0 then Map.empty else
    let stopId=values.[0].stopId
    // Hypotheses a post-level source knows about, with their tags and sources.
    let hypotheses =
        details
        |> Seq.choose(fun pair ->
            let medoid,_,_,_,_,_,members=pair.Value
            let observations=
                members |> Array.collect _.observationIds |> Array.distinct |> Array.sortWith(fun a b -> String.CompareOrdinal(a,b))
                |> Array.filter(fun id -> not(facts.stopLevelSources.Contains(observationSource id)))
            if observations.Length=0 then None else
            let tagOf id = match facts.tags.TryGetValue id with | true,value -> value | _ -> struct(null,false)
            let localRef=observations |> Array.tryPick(fun id -> let struct(value,_)=tagOf id in Option.ofObj value)
            let platform=observations |> Array.exists(fun id -> let struct(_,value)=tagOf id in value)
            let sources=observations |> Array.map observationSource |> Array.distinct |> Array.sort
            let modes=members |> Array.collect _.explicitModes |> Array.filter((<>) "") |> Array.distinct
            Some(pair.Key,(medoid.latitude,medoid.longitude,localRef,platform,sources,modes)))
        |> Seq.sortWith(fun (a,_) (b,_) -> String.CompareOrdinal(a,b))
        |> Seq.toArray
    if hypotheses.Length=0 then Map.empty else
    let ids=hypotheses |> Array.map fst
    let info=hypotheses |> Array.map snd
    let postIndex=
        JdfPostScorer.clusterPosts
            (info |> Array.map(fun (lat,_,_,_,_,_) -> lat)) (info |> Array.map(fun (_,lon,_,_,_,_) -> lon))
            (info |> Array.map(fun (_,_,_,_,sources,_) -> sources)) (info |> Array.map(fun (_,_,ref,_,_,_) -> ref))
    let postOf=Array.map2(fun id index -> id,$"{stopId}:{index}") ids postIndex |> dict
    let hypothesisInfo=Array.zip ids info |> dict
    let postMembers=ids |> Array.groupBy(fun id -> postOf.[id]) |> dict
    let postSupport=
        postMembers |> Seq.map(fun pair ->
            pair.Key, pair.Value |> Array.collect(fun id -> let _,_,_,_,sources,_=hypothesisInfo.[id] in sources) |> Array.distinct |> Array.length)
        |> dict
    let maximumSupport=postSupport.Values |> Seq.max
    let postModes=
        postMembers |> Seq.map(fun pair ->
            pair.Key, pair.Value |> Array.collect(fun id -> let _,_,_,_,_,modes=hypothesisInfo.[id] in modes) |> Array.distinct)
        |> dict
    let numbered id = let _,_,ref,platform,_,_=hypothesisInfo.[id] in ref.IsSome && platform
    let numberedCount=ids |> Array.filter numbered |> Array.length
    let postIsBay=
        postMembers |> Seq.map(fun pair -> pair.Key, numberedCount>=MinimumNumberedBays && (pair.Value |> Array.exists numbered)) |> dict
    // Areas over post centres, ordered by post id string as in the reference.
    let orderedPosts=postMembers.Keys |> Seq.sortWith(fun a b -> String.CompareOrdinal(a,b)) |> Seq.toArray
    let centre post =
        let members=postMembers.[post] |> Array.map(fun id -> hypothesisInfo.[id])
        members |> Array.averageBy(fun (lat,_,_,_,_,_) -> lat), members |> Array.averageBy(fun (_,lon,_,_,_,_) -> lon)
    let modeClass post =
        let modes=postModes.[post]
        if modes.Length=0 then "any" elif modes |> Array.exists roadModes.Contains then "road" else "tram"
    let areaIndex=
        JdfPostScorer.clusterAreas (orderedPosts |> Array.map(centre >> fst)) (orderedPosts |> Array.map(centre >> snd))
                                   (orderedPosts |> Array.map modeClass)
    let areaOf=Array.map2(fun post index -> post,$"{stopId}:a{index}") orderedPosts areaIndex |> dict
    let stopPostCount=orderedPosts.Length
    // Variant-0 diagnostic-equivalent rows per context, restricted to post candidates
    // whose post modes the context mode can use.
    let baseRows=
        rows |> Array.filter(fun row ->
            row.variantRank=0 && postOf.ContainsKey row.candidateId
            && (let modes=postModes.[postOf.[row.candidateId]]
                modes.Length=0 || not(JdfPostInference.modeIncompatible row.mode modes)))
    let featureValue (row:ReplayScore) (contextMinimum:float) (name:string) =
        let alignment,side,proximity =
            JdfPostInference.geometryComponents policy row.corridorDistance row.signedLateralOffset
                row.corridorHeading row.attachmentHeading
        let capped=row.routedExcess |> Option.map(fun value -> Math.Clamp(value,0.0,LearnedExcessCapMetres))
                   |> Option.defaultValue LearnedExcessCapMetres
        let lifecycleFailure=
            details |> Map.tryFind row.candidateId
            |> Option.bind(fun (_,hasCurrent,hasObsolete,_,_,_,_) ->
                if hasObsolete && not hasCurrent then Some "obsolete-lifecycle" else None)
        let rejection=
            JdfPostInference.hardGateReason policy (lifecycleFailure |> Option.orElse row.topologyFailureReason)
                row.corridorDistance row.signedLateralOffset row.routedExcess
        let post=postOf.[row.candidateId]
        let flag value = if value then 1.0 else 0.0
        match name with
        | "side" -> side
        | "alignment" -> alignment
        | "proximity" -> proximity
        | "corridor_distance" -> row.corridorDistance |> Option.defaultValue 0.0
        | "routed_fit" -> row.routedExcess |> Option.map(JdfPostInference.routedFit policy) |> Option.defaultValue 0.0
        | "routed_excess_metres_capped" -> capped
        | "excess_minus_context_min" -> capped-contextMinimum
        | "eligible" -> flag rejection.IsNone
        | "has_local_ref" -> let _,_,ref,_,_,_=hypothesisInfo.[row.candidateId] in flag ref.IsSome
        | "is_bay" -> flag postIsBay.[post]
        | "post_support" -> float postSupport.[post]
        | "post_support_deficit" -> float(maximumSupport-postSupport.[post])
        | "is_tram" -> flag(row.mode="E")
        | "is_trolleybus" -> flag(row.mode="T")
        | "anchor_previous_missing" -> flag row.previousStopId.IsNone
        | "anchor_next_missing" -> flag row.nextStopId.IsNone
        | "service_edge_count" -> float row.serviceEdgeCount
        | other -> invalidOp $"Learned scorer feature {other} is not available in JrUtil"
    let cappedExcess (row:ReplayScore) =
        row.routedExcess |> Option.map(fun value -> Math.Clamp(value,0.0,LearnedExcessCapMetres))
        |> Option.defaultValue LearnedExcessCapMetres
    let byContext=baseRows |> Array.groupBy _.contextId
    let candidates=
        [| for _,contextRows in byContext do
               let minimum=contextRows |> Array.map cappedExcess |> Array.min
               for row in contextRows do
                   let _,_,_,platform,_,_=hypothesisInfo.[row.candidateId]
                   yield ({ contextId=row.contextId;hypothesisId=row.candidateId
                            postId=postOf.[row.candidateId];areaId=areaOf.[postOf.[row.candidateId]]
                            isPlatform=platform
                            features=model.stage1.features |> Array.map(featureValue row minimum) }
                          : JdfPostScorer.Candidate) |]
    let stopText (value:int64 option) = value |> Option.map(fun id -> $"jdf:stop:{id}")
    let contexts=
        byContext |> Array.map(fun (contextId,contextRows) ->
            let first=contextRows.[0]
            ({ contextId=contextId;previousStop=stopText first.previousStopId;nextStop=stopText first.nextStopId
               stopPostCount=stopPostCount } : JdfPostScorer.ContextKey))
    JdfPostScorer.scoreStop model contexts candidates
    |> Array.map(fun decision -> decision.contextId,decision)
    |> Map.ofArray
