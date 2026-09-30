// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Trip signatures, pattern alignment and candidate time scoring.
module internal JrUtil.RegionalOverlay.TripScoring

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Microsoft.VisualBasic.FileIO
open NodaTime
open NodaTime.Text
open JrUtil.GtfsModel
open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Values

let matchingTimeNormalizer (policy: TripMatchPolicy) =
    if policy.timeResolutionSeconds <= 0 then invalidOp "trip_match.time_resolution_seconds must be positive"
    fun (value: string) ->
        if String.IsNullOrWhiteSpace(value) then ""
        else
            let parts = value.Split(':') |> Array.map int64
            if parts.Length <> 3 then invalidOp $"Invalid GTFS time: {value}"
            let seconds = parts.[0] * 3600L + parts.[1] * 60L + parts.[2]
            let resolution = int64 policy.timeResolutionSeconds
            string ((seconds / resolution) * resolution)

let signature (normalizeTime: string -> string) (lineId: string) (calls: CallValue array) (includeAllTimes: bool) (includeLast: bool) =
    let builder = StringBuilder(lineId).Append('|')
    for call in calls do
        builder.Append(call.stopPlaceId).Append(';') |> ignore
        if includeAllTimes then
            builder.Append(normalizeTime call.arrival).Append('/').Append(normalizeTime call.departure).Append(';') |> ignore
    if not includeAllTimes && calls.Length > 0 then
        builder.Append('|').Append(normalizeTime calls.[0].departure) |> ignore
        if includeLast then builder.Append('|').Append(normalizeTime calls.[calls.Length - 1].arrival) |> ignore
    sha256Text (builder.ToString())

let stopPatternKey (pattern: string array) =
    pattern |> String.concat "\u001f" |> sha256Text

let timeSeconds (value: string) =
    if String.IsNullOrWhiteSpace(value) then -1
    else
        let parts = value.Split(':')
        if parts.Length <> 3 then invalidOp $"Invalid GTFS time: {value}"
        parseInt parts.[0] * 3600 + parseInt parts.[1] * 60 + parseInt parts.[2]

let overlayEquivalenceDigest (calls: CallValue array) =
    let builder = StringBuilder()
    for call in calls do
        builder
            .Append(call.stopPlaceId).Append('/')
            .Append(timeSeconds call.arrival).Append('/')
            .Append(timeSeconds call.departure).Append(';')
        |> ignore
    sha256Text (builder.ToString())

let identityAlignment length = Array.init length Some

let patternEditAlignment (policy: PatternEditPolicy) (source: string array) (target: string array) =
    if not policy.enabled || source.Length = 0 || target.Length = 0 then None
    elif policy.requireSameEndpoints && (source.[0] <> target.[0] || source.[source.Length - 1] <> target.[target.Length - 1]) then None
    elif abs (source.Length - target.Length) > policy.maximumEdits then None
    else
        let positionalMatches =
            Array.zip source.[0 .. min source.Length target.Length - 1] target.[0 .. min source.Length target.Length - 1]
            |> Array.sumBy (fun (left, right) -> if left = right then 1 else 0)
        let mutable commonPrefix = 0
        while commonPrefix < min source.Length target.Length && source.[commonPrefix] = target.[commonPrefix] do
            commonPrefix <- commonPrefix + 1
        let mutable commonSuffix = 0
        while commonSuffix < min source.Length target.Length - commonPrefix
              && source.[source.Length - commonSuffix - 1] = target.[target.Length - commonSuffix - 1] do
            commonSuffix <- commonSuffix + 1
        let quickSupport = max positionalMatches (commonPrefix + commonSuffix)
        if quickSupport < min source.Length target.Length - policy.maximumEdits then None
        else
            let rows = source.Length + 1
            let columns = target.Length + 1
            let costs = Array2D.zeroCreate<int> rows columns
            for sourceIndex in 0 .. source.Length do costs.[sourceIndex, 0] <- sourceIndex
            for targetIndex in 0 .. target.Length do costs.[0, targetIndex] <- targetIndex
            for sourceIndex in 1 .. source.Length do
                for targetIndex in 1 .. target.Length do
                    let substitution = costs.[sourceIndex - 1, targetIndex - 1] + (if source.[sourceIndex - 1] = target.[targetIndex - 1] then 0 else 1)
                    costs.[sourceIndex, targetIndex] <- min substitution (min (costs.[sourceIndex - 1, targetIndex] + 1) (costs.[sourceIndex, targetIndex - 1] + 1))
            let edits = costs.[source.Length, target.Length]
            let agreement = 1.0 - float edits / float (max source.Length target.Length)
            if edits > policy.maximumEdits || agreement < policy.minimumAgreement then None
            else
                let alignment = Array.create target.Length None
                let mutable sourceIndex = source.Length
                let mutable targetIndex = target.Length
                while sourceIndex > 0 || targetIndex > 0 do
                    if sourceIndex > 0 && targetIndex > 0
                       && costs.[sourceIndex, targetIndex] = costs.[sourceIndex - 1, targetIndex - 1] + (if source.[sourceIndex - 1] = target.[targetIndex - 1] then 0 else 1) then
                        if source.[sourceIndex - 1] = target.[targetIndex - 1] then alignment.[targetIndex - 1] <- Some (sourceIndex - 1)
                        sourceIndex <- sourceIndex - 1
                        targetIndex <- targetIndex - 1
                    elif sourceIndex > 0 && costs.[sourceIndex, targetIndex] = costs.[sourceIndex - 1, targetIndex] + 1 then
                        sourceIndex <- sourceIndex - 1
                    else
                        targetIndex <- targetIndex - 1
                Some (edits, alignment)

let sourceTimeArrays (sourceCalls: CallValue array) =
    sourceCalls |> Array.map (fun call -> timeSeconds call.arrival),
    sourceCalls |> Array.map (fun call -> timeSeconds call.departure)

let candidateTimeScore (sourceArrivals: int array) (sourceDepartures: int array) (target: BaseSignature) (sourceOrdinalByTarget: int option array) =
    let firstSource = sourceDepartures |> Array.tryFind ((<=) 0) |> Option.defaultValue -1
    let firstTarget = target.departures |> Array.tryFind ((<=) 0) |> Option.defaultValue -1
    let lastSource = sourceArrivals |> Array.tryFindBack ((<=) 0) |> Option.defaultValue -1
    let lastTarget = target.arrivals |> Array.tryFindBack ((<=) 0) |> Option.defaultValue -1
    let firstDelta = if firstSource < 0 || firstTarget < 0 then Int32.MaxValue / 4 else abs (firstSource - firstTarget)
    let durationDelta =
        if firstSource < 0 || firstTarget < 0 || lastSource < 0 || lastTarget < 0 then Int32.MaxValue / 4
        else abs ((lastSource - firstSource) - (lastTarget - firstTarget))
    let mutable aggregate = 0L
    let mutable maximum = 0
    let mutable squared = 0L
    let mutable comparable = 0
    for targetIndex in 0 .. sourceOrdinalByTarget.Length - 1 do
        match sourceOrdinalByTarget.[targetIndex] with
        | Some sourceIndex when sourceIndex < sourceArrivals.Length && targetIndex < target.arrivals.Length ->
            if sourceArrivals.[sourceIndex] >= 0 && target.arrivals.[targetIndex] >= 0 then
                let delta = abs (sourceArrivals.[sourceIndex] - target.arrivals.[targetIndex])
                aggregate <- aggregate + int64 delta
                maximum <- max maximum delta
                squared <- squared + int64 delta * int64 delta
                comparable <- comparable + 1
            if sourceDepartures.[sourceIndex] >= 0 && target.departures.[targetIndex] >= 0 then
                let delta = abs (sourceDepartures.[sourceIndex] - target.departures.[targetIndex])
                aggregate <- aggregate + int64 delta
                maximum <- max maximum delta
                squared <- squared + int64 delta * int64 delta
                comparable <- comparable + 1
        | _ -> ()
    let aggregate = if comparable = 0 then Int64.MaxValue / 4L else aggregate
    let squared = if comparable = 0 then Int64.MaxValue / 4L else squared
    let alignedCalls = sourceOrdinalByTarget |> Array.sumBy (function Some _ -> 1 | None -> 0)
    firstDelta, aggregate, durationDelta, maximum, squared, alignedCalls

let candidateRank (candidate: CandidateMatch) =
    struct (
        candidate.editCount,
        candidate.firstDepartureDelta,
        candidate.aggregateTimeDelta,
        candidate.durationDelta,
        candidate.maximumTimeDelta,
        candidate.squaredTimeDelta,
        -candidate.alignedCallCount)

let candidateRankMargin (best: CandidateMatch) (runnerUp: CandidateMatch) =
    if best.editCount <> runnerUp.editCount then int64 (runnerUp.editCount - best.editCount)
    elif best.firstDepartureDelta <> runnerUp.firstDepartureDelta then int64 (runnerUp.firstDepartureDelta - best.firstDepartureDelta)
    elif best.aggregateTimeDelta <> runnerUp.aggregateTimeDelta then runnerUp.aggregateTimeDelta - best.aggregateTimeDelta
    elif best.durationDelta <> runnerUp.durationDelta then int64 (runnerUp.durationDelta - best.durationDelta)
    elif best.maximumTimeDelta <> runnerUp.maximumTimeDelta then int64 (runnerUp.maximumTimeDelta - best.maximumTimeDelta)
    elif best.squaredTimeDelta <> runnerUp.squaredTimeDelta then runnerUp.squaredTimeDelta - best.squaredTimeDelta
    else int64 (best.alignedCallCount - runnerUp.alignedCallCount)
