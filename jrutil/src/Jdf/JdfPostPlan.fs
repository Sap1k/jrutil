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
    | Centroid

type DerivedPostSelection = {
    locationId: string
    stopId: int64
    candidateIds: string array
    representativeCandidateId: string option
    resolution: PostResolution
    lat: decimal
    lon: decimal
    selectionKind: string
    score: float
    margin: float option
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
    unresolvedPatternContexts: PostPatternContextKey array
    candidateStopCount: int
    unresolvedContexts: int
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
    unresolvedPatternContexts = [||]
    candidateStopCount = 0
    unresolvedContexts = 0
}

/// Adapts the inference-domain result to the converter's publishing plan.
/// Policy evaluation remains entirely in JdfPostInferenceEvaluator; this
/// function only translates already-decided assignments, and releases the
/// result's spooled rows once the plan is materialized.
let postEstimationPlanFromInferenceResult
        (result:JdfPostInference.PostInferenceResult) : PostEstimationPlan =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "result-adaptation"
    try
        let hypothesesById =
            result.Hypotheses
            |> Array.map(fun value -> value.hypothesisId,value)
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
        let selectionFor (assignment:JdfPostInference.ContextPostAssignment) =
            match assignment.selectedLocationId,assignment.selectedHypothesisId with
            | Some locationId,Some hypothesisId ->
                let hypothesis=hypothesesById.[hypothesisId]
                Some ({ locationId=locationId;stopId=assignment.stopId
                        candidateIds=[|hypothesisId|]
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
        { authored=authored;calls=calls;authoredContexts=authoredContextMap
          callContexts=callContexts;movementFamilyIds=movementFamilyIds
          locations=inferredLocations;inferredLocations=inferredLocations
          inferredLocationOrdinals=inferredLocationOrdinals
          physicalHypotheses=physicalHypotheses
          unresolvedPatternContexts=unresolved.ToArray()
          candidateStopCount=result.Counters.candidateStopCount
          unresolvedContexts=result.Counters.unresolvedContexts }
    finally
        (result :> IDisposable).Dispose()

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
