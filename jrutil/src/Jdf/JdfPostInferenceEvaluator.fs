// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfPostInferenceEvaluator

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions

open Parquet
open Parquet.Schema
open JrUtil.JdfPostEvidenceReader
open JrUtil.JdfPostPolicyEvaluation
open JrUtil.JdfPostSideGroups


[<Literal>]
let private ResultRowMemoryBudgetBytes=256L*1024L*1024L

/// The sole production policy-evaluation boundary. The store has already
/// passed structural, hash, ordering, key, FK, sentinel, and coverage checks;
/// no JDF, graph, GTFS object, path, or unvalidated row collection can enter.
let evaluateWithScorer includeDiagnostics
                       (store:JdfPostEvidenceStore.PostEvidenceStore)
                       (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                       (scorer:JdfPostInferencePolicy.ScorerChoice)
                       : JdfPostInference.PostInferenceResult =
    // Validation happens before callers can obtain a PostEvidenceStore, so a
    // rejected pack never records this phase.
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "evaluator-entry"
    let manifest=store.Manifest
    let policy =
        JdfPostInferencePolicy.validatePolicyForEvidence
            manifest.captureCeilings.routedExcessMetres
            manifest.captureCeilings.maximumCorridorVariants policy
    let evidencePath=store.Directory
    let routePointsByStop=readReplayRoutePoints evidencePath None
    // The learned scorer replaces the heuristic's decision for unlabelled contexts;
    // authored posts, same-stop pairs and counters stay on the heuristic path.
    let learned =
        match scorer with
        | JdfPostInferencePolicy.LearnedScorer model -> Some(model,readObservationFacts evidencePath)
        | JdfPostInferencePolicy.HeuristicScorer -> None
    let temporaryDirectory=Path.Combine(Path.GetTempPath(),"jrutil-post-inference-results")
    let decisions=ResizeArray<ReplayDecision>()
    let hypotheses=ResizeArray<JdfPostInference.ConsolidatedPostHypothesis>()
    let mutable policyCounters = {
        sameStopBlocks=0;distinctPairChoices=0;unresolvedBlockEdges=0 }
    let mutable evidenceRows=0L
    // The assignment store drives the one and only joined evidence traversal
    // in publication mode.  Per-stop evaluation completes before the next
    // stop is read, so consolidation, decisions, hypotheses, side groups and
    // assignments derive from the same bounded stop-major buffer.
    let sideGroups=ResizeArray<JdfPostInference.GlobalPostSideGroup>()
    let assignments:JdfPostInference.IReplayableRowStore<JdfPostInference.ContextPostAssignment> =
        JdfPostInference.ReplayableRowStore<JdfPostInference.ContextPostAssignment>.Create(
            ResultRowMemoryBudgetBytes,temporaryDirectory,
            seq {
                let stopRows=ResizeArray<ReplayScore>()
                let contextRows=ResizeArray<ReplayScore>()
                let mutable currentStop:int64 option=None
                let mutable previousContext:string option=None
                let flush () = seq {
                    if stopRows.Count>0 then
                        let values=stopRows.ToArray()
                        let stopId=values.[0].stopId
                        let points=routePointsByStop |> Map.tryFind stopId |> Option.defaultValue [||]
                        let stopDecisions,stopCounters,details =
                            evaluateReplayPolicy manifest.captureCeilings.routedExcessMetres policy points values
                        decisions.AddRange(stopDecisions)
                        policyCounters <- {
                            sameStopBlocks=policyCounters.sameStopBlocks+stopCounters.sameStopBlocks
                            distinctPairChoices=policyCounters.distinctPairChoices+stopCounters.distinctPairChoices
                            unresolvedBlockEdges=policyCounters.unresolvedBlockEdges+stopCounters.unresolvedBlockEdges }
                        for pair in details do
                            let medoid,_,_,_,_,_,members=pair.Value
                            hypotheses.Add({
                                hypothesisId=pair.Key
                                stopId=stopId
                                memberObservationIds=
                                    members |> Array.collect _.observationIds |> Array.distinct |> Array.sort
                                memberRoutePointIds=members |> Array.map _.routePointId |> Array.sort
                                representativeRoutePointId=medoid.routePointId
                                latitude=medoid.latitude
                                longitude=medoid.longitude })
                        let stopSideGroups=sideGroupCatalog policy routePointsByStop stopDecisions
                        sideGroups.AddRange(stopSideGroups)
                        let sideGroupsByFamily =
                            stopDecisions
                            |> Array.choose(fun decision ->
                                match decision.resolution,decision.corridorFaceId with
                                | "Side",Some face ->
                                    stopSideGroups
                                    |> Array.tryFind(fun group ->
                                        group.stopId=decision.stopId && group.mode=decision.mode
                                        && group.corridorFaceId=face)
                                    |> Option.map(fun group ->
                                        struct(decision.stopId,decision.movementFamilyId),group)
                                | _ -> None)
                            |> Map.ofArray
                        let decisionsByFamily =
                            stopDecisions
                            |> Array.map(fun value ->
                                struct(value.stopId,value.movementFamilyId),value)
                            |> Map.ofArray
                        let learnedDecisions =
                            learned |> Option.map(fun (model,facts) ->
                                learnedStopDecisions policy model facts points values)
                        for context in contextRows do
                            let decision=decisionsByFamily.[struct(context.stopId,context.movementFamilyId)]
                            let learnedDecision =
                                match learnedDecisions with
                                | Some decisions when context.assignmentKind="unlabelled" ->
                                    Some(decisions |> Map.tryFind context.contextId)
                                | _ -> None
                            let decision =
                                match learnedDecision with
                                | Some(Some value) ->
                                    { decision with score=Some value.postProbability
                                                    margin=Some value.areaProbability }
                                | _ -> decision
                            let resolution,locationId,hypothesisId,sideGroupId =
                                if context.assignmentKind<>"unlabelled" then
                                    decision.resolution,None,None,None
                                elif learnedDecision.IsSome then
                                    match learnedDecision.Value with
                                    | Some { resolution=JdfPostScorer.Physical; hypothesisId=Some candidate } ->
                                        "Physical",Some(replayPhysicalLocationId context.stopId candidate),Some candidate,None
                                    | Some { resolution=JdfPostScorer.Area; hypothesisId=Some candidate } ->
                                        "Area",Some(replayPhysicalLocationId context.stopId candidate),Some candidate,None
                                    | _ -> "Centroid",None,None,None
                                elif decision.resolution="Physical" then
                                    match decision.candidateId with
                                    | Some candidate ->
                                        "Physical",Some(replayPhysicalLocationId context.stopId candidate),
                                        Some candidate,None
                                    | None -> "Centroid",None,None,None
                                elif decision.resolution="Side" then
                                    match sideGroupsByFamily |> Map.tryFind(struct(context.stopId,context.movementFamilyId)) with
                                    | Some group ->
                                        "Side",Some(replaySideLocationId context.stopId group.sideGroupId),
                                        Some group.representativeHypothesisId,Some group.sideGroupId
                                    | None -> "Centroid",None,None,None
                                else "Centroid",None,None,None
                            let assignment:JdfPostInference.ContextPostAssignment = {
                                contextId=context.contextId
                                stopId=context.stopId;mode=context.mode;lineId=context.lineId
                                routeDistinction=context.routeDistinction;direction=context.direction
                                patternHash=context.patternHash;patternPosition=context.position
                                previousStopId=context.previousStopId;nextStopId=context.nextStopId
                                assignmentKind=context.assignmentKind;authoredPostKey=context.authoredPostKey
                                sameStopBlockId=context.sameStopBlockId;sameStopBlockRole=context.role
                                movementFamilyId=context.movementFamilyId;resolution=resolution
                                selectedLocationId=locationId;selectedHypothesisId=hypothesisId
                                selectedSideGroupId=sideGroupId;score=decision.score;margin=decision.margin }
                            yield assignment
                        stopRows.Clear()
                        contextRows.Clear()
                    }
                for row in replayScoreRows includeDiagnostics evidencePath None do
                    match currentStop with
                    | Some stopId when stopId<>row.stopId -> yield! flush()
                    | _ -> ()
                    currentStop<-Some row.stopId
                    stopRows.Add(row)
                    evidenceRows<-evidenceRows+1L
                    if previousContext<>Some row.contextId then
                        previousContext<-Some row.contextId
                        contextRows.Add(row)
                yield! flush() })
    let hypotheses=hypotheses.ToArray() |> Array.sortBy(fun value -> value.stopId,value.hypothesisId)
    let hypothesisById=hypotheses |> Array.map(fun value -> value.hypothesisId,value) |> Map.ofArray
    let decisions=decisions.ToArray()
    let decisionsByFamily =
        decisions |> Array.map(fun value -> struct(value.stopId,value.movementFamilyId),value) |> Map.ofArray
    let sideGroups=sideGroups.ToArray() |> Array.sortBy(fun value -> value.stopId,value.mode,value.sideGroupId)
    try
        let publishedPhysical =
            assignments.ReadRows()
            |> Seq.choose(fun value ->
                match value.assignmentKind,value.selectedHypothesisId,value.selectedLocationId with
                | "unlabelled",Some hypothesis,Some location
                    when value.resolution="Physical" || value.resolution="Area" ->
                    Some(struct(value.stopId,hypothesis),location)
                | _ -> None)
            |> Map.ofSeq
        let authoredPositions:JdfPostInference.AuthoredPostPosition array =
            JdfPostInferencePolicy.PostInferencePhaseProbe.record "authored-selection"
            assignments.ReadRows()
            |> Seq.choose(fun value ->
                match value.assignmentKind,value.authoredPostKey with
                | "authored",Some key -> Some((value.stopId,key),value)
                | _ -> None)
            |> Seq.groupBy fst
            |> Seq.choose(fun (authoredKey,values) ->
                let contexts=values |> Seq.map snd |> Seq.toArray
                let relevant =
                    contexts
                    |> Array.choose(fun context ->
                        decisionsByFamily |> Map.tryFind(struct(context.stopId,context.movementFamilyId)))
                let physical =
                    relevant |> Array.choose(fun value ->
                        match value.resolution,value.candidateId,value.score,value.margin with
                        | "Physical",Some candidate,Some score,margin ->
                            Some(candidate,score,margin)
                        | _ -> None)
                let candidates=physical |> Array.map(fun (candidate,_,_) -> candidate) |> Array.distinct
                match candidates with
                | [|candidate|]
                    when JdfPostInference.authoredResolutionEligible policy relevant.Length
                             physical.Length candidates.Length
                             (physical |> Array.map(fun (_,score,margin) -> score,margin)) ->
                    match publishedPhysical |> Map.tryFind(struct(fst authoredKey,candidate)),
                          hypothesisById |> Map.tryFind candidate with
                    | Some location,Some hypothesis ->
                        let position:JdfPostInference.AuthoredPostPosition = {
                            stopId=fst authoredKey
                            authoredPostKey=snd authoredKey;locationId=location
                            latitude=Some hypothesis.latitude;longitude=Some hypothesis.longitude }
                        Some position
                    | _ -> None
                | _ -> None)
            |> Seq.sortBy(fun value -> value.stopId,value.authoredPostKey)
            |> Seq.toArray
        let diagnosticRows:seq<JdfPostInference.PostInferenceDiagnosticScore> = seq {
            let routePointsById =
                routePointsByStop
                |> Seq.collect(fun pair -> pair.Value)
                |> Seq.map(fun value -> value.routePointId,value)
                |> Map.ofSeq
            let stopRows=ResizeArray<ReplayScore>()
            let mutable currentStop:int64 option=None
            let emit () = seq {
                if stopRows.Count>0 then
                    let stopId=stopRows.[0].stopId
                    let points=routePointsByStop |> Map.tryFind stopId |> Option.defaultValue [||]
                    let rows,details=consolidateReplayRows policy points (stopRows.ToArray())
                    let selectedRanksByContext =
                        rows
                        |> Array.groupBy _.contextId
                        |> Array.map(fun (contextId,values) ->
                            let ranks =
                                values
                                |> Array.map(fun row ->
                                    let fact:JdfPostInference.AlternativeCorridorFact = {
                                        variantRank=row.variantRank
                                        relativeCostMetres=row.relativeCostMetres
                                        relativeCostFraction=row.relativeCostFraction }
                                    fact)
                                |> JdfPostInference.selectedCorridorVariantRanks policy
                            contextId,ranks)
                        |> Map.ofArray
                    for row in rows do
                        let lifecycleFailure =
                            details |> Map.tryFind row.candidateId
                            |> Option.bind(fun (_,hasCurrent,hasObsolete,_,_,_,_) ->
                                if hasObsolete && not hasCurrent then Some "obsolete-lifecycle" else None)
                        let rejection =
                            JdfPostInference.hardGateReason policy
                                (lifecycleFailure |> Option.orElse row.topologyFailureReason)
                                row.corridorDistance row.signedLateralOffset row.routedExcess
                        let routed =
                            row.routedExcess
                            |> Option.map(JdfPostInference.routedFit policy)
                            |> Option.defaultValue 0.0
                        let alignment,side,proximity =
                            JdfPostInference.geometryComponents policy row.corridorDistance
                                row.signedLateralOffset row.corridorHeading row.attachmentHeading
                        let geometry=JdfPostInference.geometryScore policy alignment side proximity routed
                        let sourceAdjustment,modalityAdjustment =
                            match details |> Map.tryFind row.candidateId with
                            | Some(_,_,_,supportWeight,explicitModes,deniedModes,_) ->
                                replaySupportAdjustments policy row.mode supportWeight
                                    explicitModes deniedModes
                            | _ ->
                                routePointsById |> Map.tryFind row.candidateId
                                |> Option.map(fun point ->
                                    replaySupportAdjustments policy row.mode point.supportWeight
                                        point.explicitModes point.deniedModes)
                                |> Option.defaultValue(0.0,0.0)
                        let support=JdfPostInference.combinedSupportAdjustment
                                        policy sourceAdjustment modalityAdjustment
                        let diagnostic:JdfPostInference.PostInferenceDiagnosticScore = {
                            contextId=row.contextId
                            candidateId=row.candidateId;variantRank=row.variantRank
                            eligible=rejection.IsNone;alignment=alignment;side=side
                            proximity=proximity;routedExcess=routed
                            routedExcessMetres=row.routedExcess;corridorId=row.corridorId
                            ingressThreadId=row.ingressThreadId;egressThreadId=row.egressThreadId
                            corridorFaceId=row.corridorFaceId;routingAvailability=row.routingAvailability
                            alternativeCorridorCount=selectedRanksByContext.[row.contextId].Length
                            alternativeCostGap=row.relativeCostMetres
                            selectedCorridorRanks=selectedRanksByContext.[row.contextId]
                            tiedCorridorsAgree=row.tiedCorridorsAgree
                            corridorDistance=row.corridorDistance
                            signedLateralOffset=row.signedLateralOffset
                            corridorHeading=row.corridorHeading
                            attachmentHeading=row.attachmentHeading
                            snapEdgeId=row.snapEdgeId;snapFraction=row.snapFraction
                            topologyFailureReason=row.topologyFailureReason
                            sourceAdjustment=sourceAdjustment
                            modalityAdjustment=modalityAdjustment;popularityAdjustment=0.0
                            total=JdfPostInference.supportingScore geometry support
                            rejectionReason=rejection }
                        yield diagnostic
                    stopRows.Clear()
            }
            for row in replayScoreRows true evidencePath None do
                match currentStop with
                | Some stopId when stopId<>row.stopId ->
                    yield! emit()
                | _ -> ()
                currentStop<-Some row.stopId
                stopRows.Add(row)
            yield! emit()
        }
        let diagnostics:JdfPostInference.IReplayableRowStore<JdfPostInference.PostInferenceDiagnosticScore> =
            JdfPostInference.ReplayableRowStore<JdfPostInference.PostInferenceDiagnosticScore>.Create(
                ResultRowMemoryBudgetBytes,temporaryDirectory,
                if includeDiagnostics then diagnosticRows else Seq.empty)
        let unresolved=assignments.ReadRows() |> Seq.filter(fun value -> value.assignmentKind="unlabelled" && value.selectedLocationId.IsNone) |> Seq.length
        let counters:JdfPostInference.PostInferenceCounters = {
            evidenceRows=evidenceRows
            contextCount=int assignments.Count;candidateStopCount=routePointsByStop.Count
            unresolvedContexts=unresolved;authoredPositions=authoredPositions.Length
            sameStopBlocks=policyCounters.sameStopBlocks
            distinctPairChoices=policyCounters.distinctPairChoices
            unresolvedBlockEdges=policyCounters.unresolvedBlockEdges
            physicalResolutions=decisions |> Array.filter(fun value -> value.resolution="Physical") |> Array.length
            sideResolutions=decisions |> Array.filter(fun value -> value.resolution="Side") |> Array.length
            centroidResolutions=decisions |> Array.filter(fun value -> value.resolution="Centroid") |> Array.length }
        new JdfPostInference.PostInferenceResult(
            hypotheses,sideGroups,assignments,authoredPositions,diagnostics,counters)
    with _ ->
        assignments.Dispose()
        reraise()

/// The tuned heuristic scorer (policy v2 behaviour).
let evaluateWithDiagnostics includeDiagnostics store policy =
    evaluateWithScorer includeDiagnostics store policy JdfPostInferencePolicy.HeuristicScorer

let evaluate (store:JdfPostEvidenceStore.PostEvidenceStore)
             (policy:JdfPostInferencePolicy.PostInferencePolicyV2) =
    evaluateWithDiagnostics true store policy
