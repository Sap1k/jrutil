// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.RouteMatching

open System
open System.Collections.Generic
open System.IO
open NodaTime

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay

type Input = {
    prepared: InputPreparation.Result
    stopMatching: StopMatching.Result
    sourceRouteRows: CsvRow array
    sourceRoutes: IDictionary<string, CsvRow>
    sourceTripsById: IDictionary<string, CsvRow>
    sourceTripsByRoute: IDictionary<string, CsvRow array>
}

type Result = {
    joinedValues: Dictionary<string, ResizeArray<string>>
    cisForSourceTrip: CsvRow -> string option
    directBaseRoutesForSourceTrip: CsvRow -> string array
    routeCandidates: Dictionary<string, (string array * string)>
    structuralRouteCandidates: Dictionary<string, string array>
    baseSignatures: Dictionary<string, BaseSignature>
    sourceCalls: Dictionary<string, CallValue array>
    baseTripIdsByRoute: IDictionary<string, string array>
    baseTripIdsByRouteAndLength: Dictionary<struct (string * int), ResizeArray<string>>
    baseTripIdsByRouteAndPattern: Dictionary<struct (string * string), ResizeArray<string>>
    candidateEquivalenceKey: CandidateMatch -> string
    sourcePattern: CallValue array -> string option array
}

/// Build route candidates and streamed call indexes used by contextual and trip matching.
let buildCandidates ({
    prepared = prepared
    stopMatching = stopMatching
    sourceRouteRows = sourceRouteRows
    sourceRoutes = sourceRoutes
    sourceTripsById = sourceTripsById
    sourceTripsByRoute = sourceTripsByRoute
}: Input) : Result =
    let joinPolicy = prepared.policy.source.routeJoin
    let joinRows = if isNull (box joinPolicy) then [||] else requireTable prepared.binding.payloadPath joinPolicy.table
    let joinKey (columns: string array) (row: CsvRow) = columns |> Array.map (rowValue row) |> String.concat "\u001f"
    let joinedValues = Dictionary<string, ResizeArray<string>>(StringComparer.Ordinal)
    for row in joinRows do
        let key = joinKey joinPolicy.lookupKeys row
        let value = rowValue row joinPolicy.valueColumn
        if not (String.IsNullOrWhiteSpace(value)) then
            match joinedValues.TryGetValue(key) with
            | true, values -> values.Add(value)
            | _ ->
                let values = ResizeArray<string>()
                values.Add(value)
                joinedValues.[key] <- values
    let cisForSourceTrip (row: CsvRow) =
        if isNull (box joinPolicy) then None
        else
            let key = joinKey joinPolicy.sourceKeys row
            match joinedValues.TryGetValue(key) with
            | true, values ->
                let unique = values |> Seq.distinct |> Seq.toArray
                if unique.Length = 1 then Some unique.[0] else None
            | _ -> None
    let directBaseRoutesForSourceTrip (row: CsvRow) =
        let sourceRoute = sourceRoutes.[rowValue row "route_id"]
        cisForSourceTrip row
        |> Option.map (fun cis ->
            match prepared.baseRoutesByCis.TryGetValue(cis) with
            | true, ids ->
                ids
                |> Array.filter (fun id ->
                    prepared.baseRoutes.ContainsKey(id)
                    && (let baseMode = modeClass (rowValue prepared.baseRoutes.[id] "route_type")
                        let sourceMode = modeClass (rowValue sourceRoute "route_type")
                        baseMode = sourceMode || (Set.ofList ["bus"; "trolleybus"] |> fun road -> road.Contains(baseMode) && road.Contains(sourceMode))))
                |> Array.distinct
                |> Array.sort
            | _ -> [||])
        |> Option.defaultValue [||]

    let routeOverridesBySource = prepared.routeOverrides |> Array.groupBy (fun value -> value.sourceId) |> dict
    let routeCandidates = Dictionary<string, string array * string>(StringComparer.Ordinal)
    let structuralRouteCandidates = Dictionary<string, string array>(StringComparer.Ordinal)
    for sourceRoute in sourceRouteRows do
        let sourceRouteId = rowValue sourceRoute "route_id"
        let sourceTrips = match sourceTripsByRoute.TryGetValue(sourceRouteId) with | true, rows -> rows | _ -> [||]
        let assertedCis = sourceTrips |> Array.choose cisForSourceTrip |> Array.distinct
        let direct = sourceTrips |> Array.collect directBaseRoutesForSourceTrip |> Array.distinct
        let reviewed =
            match routeOverridesBySource.TryGetValue(sourceRouteId) with
            | true, values ->
                values
                |> Array.filter (fun value -> value.validFrom <= prepared.window.endDate && value.validTo >= prepared.window.startDate)
                |> Array.map (fun value -> value.targetId)
                |> Array.filter prepared.baseRoutes.ContainsKey
                |> Array.distinct
            | _ -> [||]
        let label = rowValue sourceRoute "route_short_name"
        let structural =
            prepared.baseRouteRows
            |> Array.filter (fun target ->
                modeClass (rowValue target "route_type") = modeClass (rowValue sourceRoute "route_type")
                && (String.IsNullOrWhiteSpace(label) || rowValue target "route_short_name" = label))
            |> Array.map (fun row -> rowValue row "route_id")
            |> Array.distinct
        structuralRouteCandidates.[sourceRouteId] <- structural
        if direct.Length > 0 && prepared.policy.source.routeMatchTiers |> Array.contains "companion_assertion" then
            routeCandidates.[sourceRouteId] <- direct, "companion_assertion"
        elif reviewed.Length > 0 && prepared.policy.source.routeMatchTiers |> Array.contains "reviewed_override" then
            routeCandidates.[sourceRouteId] <- reviewed, "reviewed_override"
        else
            let candidates =
                if prepared.policy.source.routeMatchTiers |> Array.contains "structural_trip_evidence" then structural else [||]
            routeCandidates.[sourceRouteId] <- candidates, "structural_trip_evidence"
    logProgress "build-route-candidates" (int64 sourceRouteRows.Length) (Some (int64 sourceRouteRows.Length))

    let candidateRouteIds =
        Seq.append (routeCandidates.Values |> Seq.collect fst) (structuralRouteCandidates.Values |> Seq.collect id)
        |> Set.ofSeq
    let candidateBaseTripIds =
        prepared.baseTripValues
        |> Array.choose (fun trip ->
            let routeId = trip.routeId
            let serviceId = trip.serviceId
            match prepared.baseDates.TryGetValue(serviceId) with
            | true, dates when candidateRouteIds.Contains(routeId) && anyDate dates -> Some trip.id
            | _ -> None)
        |> Set.ofArray
    let basePlaceMap = prepared.basePlaceByStop |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
    let orderedCalls (values: ResizeArray<CallValue>) = values |> Seq.sortBy (fun call -> call.sequence) |> Seq.toArray
    let baseSignatures = Dictionary<string, BaseSignature>(StringComparer.Ordinal)
    let mutable currentBaseTripId = ""
    let currentBaseCalls = ResizeArray<CallValue>()
    let flushBaseSignature () =
        if currentBaseCalls.Count > 0 then
            let calls = orderedCalls currentBaseCalls
            baseSignatures.[currentBaseTripId] <- {
                tripId = currentBaseTripId
                full = signature prepared.normalizeMatchingTime "" calls true true
                patternEndpoints = signature prepared.normalizeMatchingTime "" calls false true
                patternFirst = signature prepared.normalizeMatchingTime "" calls false false
                pattern = calls |> Array.map (fun call -> call.stopPlaceId)
                arrivals = calls |> Array.map (fun call -> timeSeconds call.arrival)
                departures = calls |> Array.map (fun call -> timeSeconds call.departure)
                overlayEquivalenceDigest = overlayEquivalenceDigest calls
            }
            currentBaseCalls.Clear()
    // The vocabulary is small (stops, clock times, headsigns), unlike the call table.
    // This pool is local to loading, so it cannot grow across overlay executions.
    let callText = Dictionary<string, string>(StringComparer.Ordinal)
    let shareText value =
        match callText.TryGetValue(value) with
        | true, existing -> existing
        | _ -> callText.Add(value, value); value
    let readCall places row =
        let call = callValues places row
        { call with stopId = shareText call.stopId; arrival = shareText call.arrival; departure = shareText call.departure
                    stopHeadsign = call.stopHeadsign |> Option.map shareText }
    let mutable scannedBaseCalls = 0L
    for row in csvValues prepared.baseGtfs "stop_times.txt" callColumns do
        let tripId = row.[0]
        if tripId <> currentBaseTripId then
            flushBaseSignature ()
            currentBaseTripId <- tripId
            if candidateBaseTripIds.Contains(tripId) && baseSignatures.ContainsKey(tripId) then
                invalidOp "Base stop_times.txt must keep each trip's calls contiguous for streaming overlay matching"
        if candidateBaseTripIds.Contains(tripId) then
            currentBaseCalls.Add(readCall basePlaceMap row)
        scannedBaseCalls <- scannedBaseCalls + 1L
        if scannedBaseCalls % 1000000L = 0L then logProgress "scan-base-call-signatures" scannedBaseCalls None
    flushBaseSignature ()
    logProgress "scan-base-call-signatures" scannedBaseCalls (Some scannedBaseCalls)

    let sourceCalls = Dictionary<string, CallValue array>(StringComparer.Ordinal)
    let mutable currentSourceTripId = ""
    let currentSourceCalls = ResizeArray<CallValue>()
    let flushSourceCalls () =
        if currentSourceCalls.Count > 0 then
            sourceCalls.[currentSourceTripId] <- orderedCalls currentSourceCalls
            currentSourceCalls.Clear()
    let mutable scannedSourceCalls = 0L
    for row in csvValues prepared.binding.payloadPath "stop_times.txt" callColumns do
        let tripId = row.[0]
        if tripId <> currentSourceTripId then
            flushSourceCalls ()
            currentSourceTripId <- tripId
            if sourceTripsById.ContainsKey(tripId) && sourceCalls.ContainsKey(tripId) then
                invalidOp "Source stop_times.txt must keep each trip's calls contiguous for streaming overlay matching"
        if sourceTripsById.ContainsKey(tripId) then
            currentSourceCalls.Add(readCall stopMatching.mappedPlaceBySourceStop row)
        scannedSourceCalls <- scannedSourceCalls + 1L
        if scannedSourceCalls % 250000L = 0L then logProgress "load-source-calls" scannedSourceCalls None
    flushSourceCalls ()
    logProgress "load-source-calls" scannedSourceCalls (Some scannedSourceCalls)

    let baseTripIdsByRoute =
        prepared.baseTripValues
        |> Array.filter (fun trip -> candidateBaseTripIds.Contains(trip.id))
        |> Array.groupBy (fun trip -> trip.routeId)
        |> Array.map (fun (routeId, trips) -> routeId, trips |> Array.map (fun trip -> trip.id))
        |> dict
    let baseTripIdsByRouteAndLength = Dictionary<struct (string * int), ResizeArray<string>>()
    let baseTripIdsByRouteAndPattern = Dictionary<struct (string * string), ResizeArray<string>>()
    for KeyValue(tripId, fingerprint) in baseSignatures do
        let routeId = prepared.baseTrips.[tripId].routeId
        let key = struct (routeId, fingerprint.pattern.Length)
        match baseTripIdsByRouteAndLength.TryGetValue(key) with
        | true, values -> values.Add(tripId)
        | _ ->
            let values = ResizeArray<string>()
            values.Add(tripId)
            baseTripIdsByRouteAndLength.[key] <- values
        let patternIndexKey = struct (routeId, stopPatternKey fingerprint.pattern)
        match baseTripIdsByRouteAndPattern.TryGetValue(patternIndexKey) with
        | true, values -> values.Add(tripId)
        | _ ->
            let values = ResizeArray<string>()
            values.Add(tripId)
            baseTripIdsByRouteAndPattern.[patternIndexKey] <- values
    let candidateEquivalenceKey (candidate: CandidateMatch) =
        let fingerprint = baseSignatures.[candidate.tripId]
        let trip = prepared.baseTrips.[candidate.tripId]
        let stableLine =
            match prepared.baseCisByRoute.TryGetValue(trip.routeId) with
            | true, cisLineId -> "cis:" + cisLineId
            | _ -> "route:" + trip.routeId
        String.concat "|" [
            stableLine
            fingerprint.overlayEquivalenceDigest
        ]

    let sourcePattern (calls: CallValue array) =
        calls
        |> Array.map (fun call -> stopMatching.mappedPlaceBySourceStop |> Map.tryFind call.stopId)
    {
        joinedValues = joinedValues
        cisForSourceTrip = cisForSourceTrip
        directBaseRoutesForSourceTrip = directBaseRoutesForSourceTrip
        routeCandidates = routeCandidates
        structuralRouteCandidates = structuralRouteCandidates
        baseSignatures = baseSignatures
        sourceCalls = sourceCalls
        baseTripIdsByRoute = baseTripIdsByRoute
        baseTripIdsByRouteAndLength = baseTripIdsByRouteAndLength
        baseTripIdsByRouteAndPattern = baseTripIdsByRouteAndPattern
        candidateEquivalenceKey = candidateEquivalenceKey
        sourcePattern = sourcePattern
    }
