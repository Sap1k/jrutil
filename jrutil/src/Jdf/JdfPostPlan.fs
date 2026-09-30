// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Estimated-post plans derived from routed post inference.
module JrUtil.JdfPostPlan

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open FSharp.Data
open NetTopologySuite.Geometries
open NodaTime
open NodaTime.Calendars
open Serilog
open JrUtil
open JrUtil.Holidays
open JrUtil.GeoData.Common
open JrUtil.GeoData.Osm
open JrUtil.JdfGtfsRules

type PostResolution =
    | Physical of candidateId: string
    | Side of sideGroupId: string * representativeCandidateId: string
    | Centroid

type DerivedPostSelection = {
    locationId: string
    stopId: int64
    candidateIds: string array
    sideGroupId: string option
    representativeCandidateId: string option
    resolution: PostResolution
    lat: decimal
    lon: decimal
    selectionKind: string
    score: float
    margin: float option
}

type PostSideGroup = {
    sideGroupId: string
    stopId: int64
    mode: JdfModel.TransportMode
    corridorFaceId: string
    sector: string
    representativeCandidateId: string
    memberCandidateIds: string array
    compactnessMetres: float
    repeatedPatternSupport: int
}

type DerivedPostContext = {
    previousStopId: int64 option
    nextStopId: int64 option
    sameStopBlockRole: string
}

[<Struct>]
type PostPatternContextKey = {
    stopId: int64
    mode: JdfModel.TransportMode
    lineId: string
    routeDistinction: int
    direction: int
    patternHash: string
    position: int
    sameStopBlockRole: string
}

type CandidateModalityEstimate = {
    candidateId: string
    supportedModes: string array
    explicitlyDeniedModes: string array
    status: string
    roadSupport: int
    trolleybusSupport: int
    tramSupport: int
    distinctSupportingPatterns: int
    evidenceMethod: string
    confidence: float
}

type PhysicalPostHypothesis = {
    hypothesisId: string
    stopId: int64
    memberObservationIds: string array
    memberCandidateIds: string array
    representativeCandidateId: string
    lat: decimal
    lon: decimal
    sources: string array
}

type DerivedPostScore = {
    context: PostPatternContextKey
    movementFamilyId: string
    candidateId: string
    eligible: bool
    alignment: float
    side: float
    proximity: float
    routedFit: float
    routedExcessMetres: float option
    corridorId: string option
    ingressThreadId: string option
    egressThreadId: string option
    corridorFaceId: string option
    routingAvailability: string
    alternativeCorridorCount: int
    alternativeCostGap: float option
    snapEdgeId: int option
    snapFraction: float option
    corridorDistance: float option
    signedLateralOffset: float option
    corridorHeading: float option
    attachmentHeading: float option
    tiedCorridorsAgree: bool
    topologyFailureReason: string option
    routeDistinction: int
    sourceAdjustment: float
    modalityAdjustment: float
    popularityPrior: float
    total: float
    rejectionReason: string option
}

type PostEstimationPlan = {
    authored: Map<int64 * string, DerivedPostSelection>
    calls: IReadOnlyDictionary<PostPatternContextKey, DerivedPostSelection>
    authoredContexts: Map<int64 * string, DerivedPostContext array>
    callContexts: IReadOnlyDictionary<PostPatternContextKey, DerivedPostContext>
    movementFamilyIds: IReadOnlyDictionary<PostPatternContextKey,string>
    locations: DerivedPostSelection array
    inferredLocations: DerivedPostSelection array
    inferredLocationOrdinals: Map<int64 * string, int>
    physicalHypotheses: PhysicalPostHypothesis array
    modalityEstimates: CandidateModalityEstimate array
    sideGroups: PostSideGroup array
    scoreCount: int64
    scoreRows: unit -> seq<DerivedPostScore>
    cleanupScoreRows: unit -> unit
    unresolvedPatternContexts: PostPatternContextKey array
    tripPatternHashes: IReadOnlyDictionary<struct (string * int * int64), string>
    candidateStopCount: int
    singleCandidateSkips: int
    unresolvedContexts: int
    sameStopBlocks: int
    distinctPairChoices: int
    unresolvedBlockEdges: int
}

let internal emptyPostEstimationPlan = {
    authored = Map.empty
    calls = Dictionary<PostPatternContextKey, DerivedPostSelection>()
    authoredContexts = Map.empty
    callContexts = Dictionary<PostPatternContextKey, DerivedPostContext>()
    movementFamilyIds = Dictionary<PostPatternContextKey,string>()
    locations = [||]
    inferredLocations = [||]
    inferredLocationOrdinals = Map.empty
    physicalHypotheses = [||]
    modalityEstimates = [||]
    sideGroups = [||]
    scoreCount = 0L
    scoreRows = fun () -> Seq.empty
    cleanupScoreRows = ignore
    unresolvedPatternContexts = [||]
    tripPatternHashes = Dictionary<struct (string * int * int64), string>()
    candidateStopCount = 0; singleCandidateSkips = 0
    unresolvedContexts = 0; sameStopBlocks = 0; distinctPairChoices = 0
    unresolvedBlockEdges = 0
}

/// Adapts the inference-domain result to the converter's publishing plan.
/// Policy evaluation remains entirely in JdfPostInferenceEvaluator; this
/// function only translates already-decided assignments and diagnostics.
let postEstimationPlanFromInferenceResult
        (result:JdfPostInference.PostInferenceResult) : PostEstimationPlan =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "result-adaptation"
    let disposeResult () = (result :> IDisposable).Dispose()
    try
        let hypothesesById =
            result.Hypotheses
            |> Array.map(fun value -> value.hypothesisId,value)
            |> Map.ofArray
        let sideGroupsById =
            result.SideGroups
            |> Array.map(fun value -> value.sideGroupId,value)
            |> Map.ofArray
        let physicalHypotheses =
            result.Hypotheses
            |> Array.map(fun value ->
                ({ hypothesisId=value.hypothesisId
                   stopId=value.stopId
                   memberObservationIds=value.memberObservationIds
                   memberCandidateIds=value.memberRoutePointIds
                   representativeCandidateId=value.representativeRoutePointId
                   lat=decimal value.latitude
                   lon=decimal value.longitude
                   sources=[||] } : PhysicalPostHypothesis))
        let sideGroups =
            result.SideGroups
            |> Array.map(fun value ->
                ({ sideGroupId=value.sideGroupId
                   stopId=value.stopId
                   mode=parseTransportMode "inference result mode" value.mode
                   corridorFaceId=value.corridorFaceId
                   sector=value.sector
                   representativeCandidateId=value.representativeHypothesisId
                   memberCandidateIds=value.memberHypothesisIds
                   compactnessMetres=value.compactnessMetres
                   repeatedPatternSupport=value.support } : PostSideGroup))
        let selectionFor (assignment:JdfPostInference.ContextPostAssignment) =
            match assignment.selectedLocationId,assignment.selectedHypothesisId with
            | Some locationId,Some hypothesisId ->
                match assignment.selectedSideGroupId with
                | Some groupId ->
                    let group=sideGroupsById.[groupId]
                    Some ({ locationId=locationId;stopId=assignment.stopId
                            candidateIds=[|group.representativeHypothesisId|]
                            sideGroupId=Some groupId
                            representativeCandidateId=Some group.representativeHypothesisId
                            resolution=Side(groupId,group.representativeHypothesisId)
                            lat=decimal group.latitude;lon=decimal group.longitude
                            selectionKind="side"
                            score=assignment.score |> Option.defaultValue 0.0
                            margin=assignment.margin } : DerivedPostSelection)
                | None ->
                    let hypothesis=hypothesesById.[hypothesisId]
                    Some ({ locationId=locationId;stopId=assignment.stopId
                            candidateIds=[|hypothesisId|];sideGroupId=None
                            representativeCandidateId=Some hypothesisId
                            resolution=Physical hypothesisId
                            lat=decimal hypothesis.latitude;lon=decimal hypothesis.longitude
                            selectionKind="physical"
                            score=assignment.score |> Option.defaultValue 0.0
                            margin=assignment.margin } : DerivedPostSelection)
            | _ -> None
        let calls=Dictionary<PostPatternContextKey,DerivedPostSelection>()
        let callContexts=Dictionary<PostPatternContextKey,DerivedPostContext>()
        let movementFamilyIds=Dictionary<PostPatternContextKey,string>()
        let unresolved=ResizeArray<PostPatternContextKey>()
        let selectionsByLocation=Dictionary<string,DerivedPostSelection>(StringComparer.Ordinal)
        let authoredContexts=ResizeArray<(int64*string)*DerivedPostContext>()
        for assignment in result.Assignments.ReadRows() do
            let contextKey:PostPatternContextKey = {
                stopId=assignment.stopId
                mode=parseTransportMode "inference result mode" assignment.mode
                lineId=assignment.lineId;routeDistinction=assignment.routeDistinction
                direction=assignment.direction;patternHash=assignment.patternHash
                position=assignment.patternPosition
                sameStopBlockRole=assignment.sameStopBlockRole }
            let context:DerivedPostContext = {
                previousStopId=assignment.previousStopId
                nextStopId=assignment.nextStopId
                sameStopBlockRole=assignment.sameStopBlockRole }
            match assignment.assignmentKind,assignment.authoredPostKey with
            | "authored",Some authoredKey ->
                authoredContexts.Add((assignment.stopId,authoredKey),context)
            | "unlabelled",_ ->
                callContexts.[contextKey] <- context
                movementFamilyIds.[contextKey] <- assignment.movementFamilyId
                match selectionFor assignment with
                | Some selection ->
                    calls.[contextKey] <- selection
                    selectionsByLocation.TryAdd(selection.locationId,selection) |> ignore
                | None -> unresolved.Add(contextKey)
            | _ -> ()
        let inferredLocations =
            selectionsByLocation.Values |> Seq.sortBy _.locationId |> Seq.toArray
        let inferredLocationOrdinals =
            inferredLocations
            |> Seq.groupBy _.stopId
            |> Seq.collect (fun (stopId, selections) ->
                selections
                |> Seq.map _.locationId
                |> Seq.distinct
                |> Seq.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))
                |> Seq.mapi (fun index locationId ->
                    (stopId, locationId), index + 1))
            |> Map.ofSeq
        let authored =
            result.AuthoredPositions
            |> Array.choose(fun value ->
                match selectionsByLocation.TryGetValue(value.locationId) with
                | true,selection -> Some((value.stopId,value.authoredPostKey),selection)
                | _ -> None)
            |> Map.ofArray
        let authoredContextMap =
            authoredContexts
            |> Seq.groupBy fst
            |> Seq.map(fun (key,values) ->
                key,
                (values
                 |> Seq.map snd
                 |> Seq.distinct
                 |> Seq.sortBy(fun value ->
                     value.sameStopBlockRole,value.previousStopId,value.nextStopId)
                 |> Seq.toArray))
            |> Map.ofSeq
        let scoreRows () = seq {
            use assignments=result.Assignments.ReadRows().GetEnumerator()
            let mutable hasAssignment=assignments.MoveNext()
            for score in result.DiagnosticScores.ReadRows() do
                while hasAssignment && assignments.Current.contextId<>score.contextId do
                    hasAssignment<-assignments.MoveNext()
                if not hasAssignment then
                    invalidOp $"Derived post score references an unknown context: {score.contextId}"
                let assignment=assignments.Current
                let context:PostPatternContextKey = {
                    stopId=assignment.stopId
                    mode=parseTransportMode "inference result mode" assignment.mode
                    lineId=assignment.lineId;routeDistinction=assignment.routeDistinction
                    direction=assignment.direction;patternHash=assignment.patternHash
                    position=assignment.patternPosition
                    sameStopBlockRole=assignment.sameStopBlockRole }
                yield ({ context=context;movementFamilyId=assignment.movementFamilyId
                         candidateId=score.candidateId;eligible=score.eligible
                         alignment=score.alignment;side=score.side;proximity=score.proximity
                         routedFit=score.routedExcess;routedExcessMetres=score.routedExcessMetres
                         corridorId=score.corridorId;ingressThreadId=score.ingressThreadId
                         egressThreadId=score.egressThreadId;corridorFaceId=score.corridorFaceId
                         routingAvailability=score.routingAvailability
                         alternativeCorridorCount=score.alternativeCorridorCount
                         alternativeCostGap=score.alternativeCostGap
                         snapEdgeId=score.snapEdgeId;snapFraction=score.snapFraction
                         corridorDistance=score.corridorDistance
                         signedLateralOffset=score.signedLateralOffset
                         corridorHeading=score.corridorHeading
                         attachmentHeading=score.attachmentHeading
                         tiedCorridorsAgree=score.tiedCorridorsAgree
                         topologyFailureReason=score.topologyFailureReason
                         routeDistinction=assignment.routeDistinction
                         sourceAdjustment=score.sourceAdjustment
                         modalityAdjustment=score.modalityAdjustment
                         popularityPrior=score.popularityAdjustment
                         total=score.total;rejectionReason=score.rejectionReason }
                       : DerivedPostScore)
        }
        { authored=authored;calls=calls;authoredContexts=authoredContextMap
          callContexts=callContexts;movementFamilyIds=movementFamilyIds
          locations=inferredLocations;inferredLocations=inferredLocations
          inferredLocationOrdinals=inferredLocationOrdinals
          physicalHypotheses=physicalHypotheses;modalityEstimates=[||]
          sideGroups=sideGroups;scoreCount=result.DiagnosticScores.Count
          scoreRows=scoreRows;cleanupScoreRows=disposeResult
          unresolvedPatternContexts=unresolved.ToArray()
          tripPatternHashes=Dictionary<struct(string*int*int64),string>()
          candidateStopCount=result.Counters.candidateStopCount;singleCandidateSkips=0
          unresolvedContexts=result.Counters.unresolvedContexts
          sameStopBlocks=result.Counters.sameStopBlocks
          distinctPairChoices=result.Counters.distinctPairChoices
          unresolvedBlockEdges=result.Counters.unresolvedBlockEdges }
    with _ ->
        disposeResult()
        reraise()

let internal authoredPostKey (call: JdfModel.TripStop) =
    match call.stopPostId, call.stopPostNum |> Option.bind nonEmptyTrimmed with
    | Some value, _ -> Some $"id:{value}"
    | None, Some value -> Some $"num:{value}"
    | _ -> None

let internal completePatternHash (calls: JdfModel.TripStop array) =
    calls
    |> Array.map (fun call -> string call.stopId)
    |> String.concat ","
    |> Encoding.UTF8.GetBytes
    |> SHA256.HashData
    |> Convert.ToHexString
    |> fun value -> value.ToLowerInvariant()

let internal callIsUsable (call: JdfModel.TripStop) =
    match call.departureTime, call.arrivalTime with
    | Some JdfModel.Passing, _ | Some JdfModel.NotPassing, _ -> false
    | None, None -> false
    | _ -> true
