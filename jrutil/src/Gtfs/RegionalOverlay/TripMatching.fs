// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.TripMatching

open System
open System.Collections.Generic
open System.IO
open System.Diagnostics
open NodaTime
open Serilog

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay

type Input = {
    prepared: InputPreparation.Result
    source: SourceAnalysis.Result
    indexes: SourceAnalysis.MatchingIndexes
}

type Result = {
    bindings: ResizeArray<MatchBinding>
    candidateScoreReports: seq<string array>
    ambiguousCandidateRows: seq<string array>
    resolvedAmbiguityKeys: HashSet<string>
    sharedDateCandidateSourceTrips: HashSet<string>
    patternCompatibleSourceTrips: HashSet<string>
    snapshotGapSourceTrips: HashSet<string>
    capacityGapSourceTrips: HashSet<string>
    unresolvedPending: PendingAmbiguity array
    matchedAfterAvailability: HashSet<string>
    sourceCallCountByTrip: IDictionary<string, int>
    coverageTotalSourceCalls: int
    coverageTotalSourceCallDates: int
    coverageMatchedSourceCalls: int
    modeForSourceTrip: string -> string
    addCount: Dictionary<string, int> -> string -> int -> unit
    stopCoverageByMode: (string * int * int) array
    callCoverageByMode: (string * int * int) array
}

type CandidateDecision =
    | Accepted of CandidateMatch array * int64 option
    | Ambiguous of CandidateMatch array * int
    | NoMatch

let chooseCandidates tierRank equivalenceKey (candidates: CandidateMatch array) =
    if candidates.Length = 0 then NoMatch
    else
        let bestTier = candidates |> Array.minBy (fun candidate -> tierRank candidate.tier) |> fun candidate -> tierRank candidate.tier
        let ranked = candidates |> Array.filter (fun candidate -> tierRank candidate.tier = bestTier) |> Array.sortBy candidateRank
        let best = ranked |> Array.takeWhile (fun candidate -> candidateRank candidate = candidateRank ranked.[0])
        let classes = best |> Array.map equivalenceKey |> Array.distinct |> Array.length
        if classes > 1 then Ambiguous(best, classes)
        else
            let runner = ranked |> Array.tryFind (fun candidate -> candidateRank candidate > candidateRank ranked.[0])
            Accepted(best, runner |> Option.map (candidateRankMargin ranked.[0]))

/// Match source trips and return accepted bindings, quarantine and coverage evidence.
let matchTrips ({ prepared = prepared; source = source; indexes = indexes }: Input) : Result =
    let identityAlignments = Dictionary<int, int option array>()
    let alignmentForLength length =
        match identityAlignments.TryGetValue(length) with
        | true, alignment -> alignment
        | _ -> let alignment = identityAlignment length in identityAlignments.Add(length, alignment); alignment
    let dateSets = DateSet.Pool()
    let tierRanks = prepared.policy.source.tripMatchTiers |> Array.mapi (fun i tier -> tier, i) |> dict
    let tierEnabled tier = tierRanks.ContainsKey(tier)
    let tripOverridesBySource = prepared.tripOverrides |> Array.groupBy (fun value -> value.sourceId) |> dict
    let bindings = ResizeArray<MatchBinding>()
    let candidateScoreReports = new Scratch.RowLog(prepared.scratch)
    let addCandidateScore (value: CandidateScoreReport) =
        candidateScoreReports.Add [|
            value.sourceTripId; value.targetTripId; dateString value.date; value.routeMethod; value.tripMethod
            string value.editCount; string value.alignedCallCount; string value.firstDepartureDelta; string value.aggregateTimeDelta; string value.durationDelta
            string value.maximumTimeDelta; string value.squaredTimeDelta
            value.runnerUpMargin |> Option.map string |> Option.defaultValue ""
        |]
    let ambiguousCandidateRows = new Scratch.RowLog(prepared.scratch)
    let pendingAmbiguities = ResizeArray<PendingAmbiguity>()
    let resolvedAmbiguityKeys = HashSet<string>(StringComparer.Ordinal)
    let sharedDateCandidateSourceTrips = HashSet<string>(StringComparer.Ordinal)
    let patternCompatibleSourceTrips = HashSet<string>(StringComparer.Ordinal)
    let snapshotGapSourceTrips = HashSet<string>(StringComparer.Ordinal)
    let capacityGapSourceTrips = HashSet<string>(StringComparer.Ordinal)
    let capacityGapDates = HashSet<string>(StringComparer.Ordinal)
    let mutable matchedSourceTripsProcessed = 0L
    for sourceTripRow in source.tripRows do
        let sourceTripId = rowValue sourceTripRow "trip_id"
        let sourceRouteId = rowValue sourceTripRow "route_id"
        let sourceServiceId = rowValue sourceTripRow "service_id"
        match source.dates.TryGetValue(sourceServiceId), indexes.sourceCalls.TryGetValue(sourceTripId) with
        | (true, sourceActive), (true, sourceCallValues) when anyDate sourceActive ->
            let calls = sourceCallValues
            let fullyMapped = calls |> Array.forall (fun call -> source.mappedPlaceBySourceStop.ContainsKey(call.stopId))
            if not fullyMapped then
                addDiagnostic prepared.diagnostics "trip_stop_unresolved" sourceTripId "At least one complete-pattern stop place is unresolved"
            else
                let primaryRoutes, primaryRouteMethod = indexes.routeCandidates.[sourceRouteId]
                let structuralRoutes = indexes.structuralRouteCandidates.[sourceRouteId] |> Array.filter (fun route -> not (primaryRoutes |> Array.contains route))
                let tripsForRoutes routes =
                    routes
                    |> Array.collect (fun routeId -> match indexes.baseTripIdsByRoute.TryGetValue(routeId) with | true, ids -> ids | _ -> [||])
                    |> Array.distinct
                let primaryTrips = tripsForRoutes primaryRoutes
                let structuralTrips = tripsForRoutes structuralRoutes
                let candidateTrips = Array.append primaryTrips structuralTrips |> Array.distinct
                let sourceFull = signature prepared.normalizeMatchingTime "" calls true true
                let sourceEndpoints = signature prepared.normalizeMatchingTime "" calls false true
                let sourceFirst = signature prepared.normalizeMatchingTime "" calls false false
                let sourceStopPattern = calls |> Array.map (fun call -> call.stopPlaceId)
                let sourceArrivals, sourceDepartures = sourceTimeArrays calls
                let sourcePatternKey = stopPatternKey sourceStopPattern
                let exactTripsForRoutes routes =
                    routes
                    |> Array.collect (fun routeId ->
                        match indexes.baseTripIdsByRouteAndPattern.TryGetValue(struct (routeId, sourcePatternKey)) with
                        | true, values -> values |> Seq.toArray
                        | _ -> [||])
                    |> Array.distinct
                let primaryExactTrips = exactTripsForRoutes primaryRoutes
                let structuralExactTrips = exactTripsForRoutes structuralRoutes
                let primaryMatchTrips = if primaryExactTrips.Length > 0 then primaryExactTrips else primaryTrips
                let structuralMatchTrips = if structuralExactTrips.Length > 0 then structuralExactTrips else structuralTrips
                let matchingCandidate tripId =
                    match indexes.baseSignatures.TryGetValue(tripId) with
                    | false, _ -> None
                    | true, target ->
                        let exactPattern = target.pattern = sourceStopPattern
                        let tierAndAlignment =
                            if exactPattern && target.full = sourceFull && tierEnabled "full_signature" then
                                Some ("full_signature", 0, alignmentForLength target.pattern.Length)
                            elif exactPattern && target.patternEndpoints = sourceEndpoints && tierEnabled "pattern_endpoints" then
                                Some ("pattern_endpoints", 0, alignmentForLength target.pattern.Length)
                            elif exactPattern && target.patternFirst = sourceFirst && tierEnabled "pattern_first" then
                                Some ("pattern_first", 0, alignmentForLength target.pattern.Length)
                            elif exactPattern && prepared.policy.source.tripMatch.exactPatternProximity && tierEnabled "pattern_nearest" then
                                Some ("pattern_nearest", 0, alignmentForLength target.pattern.Length)
                            elif tierEnabled "pattern_edit_nearest" then
                                patternEditAlignment prepared.policy.source.tripMatch.patternEdit sourceStopPattern target.pattern
                                |> Option.map (fun (edits, alignment) -> "pattern_edit_nearest", edits, alignment)
                            else None
                        tierAndAlignment
                        |> Option.map (fun (tier, edits, alignment) ->
                            let firstDelta, aggregateDelta, durationDelta, maximumDelta, squaredDelta, alignedCalls =
                                candidateTimeScore sourceArrivals sourceDepartures target alignment
                            {
                                tripId = tripId
                                tier = tier
                                sourceOrdinalByTarget = alignment
                                editCount = edits
                                alignedCallCount = alignedCalls
                                firstDepartureDelta = firstDelta
                                aggregateTimeDelta = aggregateDelta
                                durationDelta = durationDelta
                                maximumTimeDelta = maximumDelta
                                squaredTimeDelta = squaredDelta
                            })
                let tierRank tier = tierRanks.[tier]
                let primaryMatches = primaryMatchTrips |> Array.choose matchingCandidate
                let structuralMatches = structuralMatchTrips |> Array.choose matchingCandidate
                let signatureCandidates = Array.append primaryMatches structuralMatches
                let reviewedCandidates =
                    match tripOverridesBySource.TryGetValue(sourceTripId) with
                    | true, values when tierEnabled "reviewed_override" ->
                        values
                        |> Array.filter (fun value -> prepared.baseTrips.ContainsKey(value.targetId))
                        |> Array.choose (fun value ->
                            match indexes.baseSignatures.TryGetValue(value.targetId) with
                            | true, target ->
                                let alignment =
                                    if calls.Length = target.pattern.Length then alignmentForLength target.pattern.Length
                                    else patternEditAlignment prepared.policy.source.tripMatch.patternEdit sourceStopPattern target.pattern |> Option.map snd |> Option.defaultValue [||]
                                let firstDelta, aggregateDelta, durationDelta, maximumDelta, squaredDelta, alignedCalls =
                                    candidateTimeScore sourceArrivals sourceDepartures target alignment
                                Some (value.validFrom, value.validTo, {
                                    tripId = value.targetId
                                    tier = "reviewed_override"
                                    sourceOrdinalByTarget = alignment
                                    editCount = if calls.Length = target.pattern.Length then 0 else abs (calls.Length - target.pattern.Length)
                                    alignedCallCount = alignedCalls
                                    firstDepartureDelta = firstDelta
                                    aggregateTimeDelta = aggregateDelta
                                    durationDelta = durationDelta
                                    maximumTimeDelta = maximumDelta
                                    squaredTimeDelta = squaredDelta
                                })
                            | _ -> None)
                    | _ -> [||]
                let reviewedForDate date =
                    reviewedCandidates |> Array.choose (fun (first, last, candidate) ->
                        if first <= date && last >= date then Some candidate else None)
                let assignments = Dictionary<string, CandidateMatch * string * bool array * int64 option>(StringComparer.Ordinal)
                let mutable sourceHadMatch = false
                let mutable sourceHadSharedCandidate = false
                let mutable sourceHadCompatibleCandidate = false
                for dateIndex in 0 .. prepared.window.dates.Length - 1 do
                    if sourceActive.[dateIndex] then
                        let active candidates =
                            candidates
                            |> Array.filter (fun candidate ->
                                let serviceId = prepared.baseTrips.[candidate.tripId].serviceId
                                match prepared.baseDates.TryGetValue(serviceId) with
                                | true, dates -> dates.[dateIndex]
                                | _ -> false)
                        let primaryActive = active primaryMatches
                        let structuralActive = active structuralMatches
                        if primaryActive.Length > 0 || structuralActive.Length > 0 then sourceHadSharedCandidate <- true
                        let routeMethod, candidates =
                            if primaryActive.Length > 0 then primaryRouteMethod, primaryActive
                            elif structuralActive.Length > 0 then "structural_trip_evidence", structuralActive
                            else
                                let reviewed = reviewedForDate prepared.window.dates.[dateIndex] |> active
                                "reviewed_override", reviewed
                        if candidates.Length > 0 then sourceHadCompatibleCandidate <- true
                        match chooseCandidates tierRank indexes.candidateEquivalenceKey candidates with
                        | Accepted(acceptedBest, runnerMargin) ->
                            sourceHadMatch <- true
                            if acceptedBest.Length > 1 then
                                addDiagnostic prepared.diagnostics "trip_equivalent_tie_expanded" sourceTripId $"{dateString prepared.window.dates.[dateIndex]} overlays {acceptedBest.Length} semantically equivalent target trips"
                            for candidate in acceptedBest do
                                let targetTripId = candidate.tripId
                                let methodName = routeMethod + "+" + candidate.tier
                                let key =
                                    String.concat "\u001f" [
                                        targetTripId; methodName; string candidate.editCount; string candidate.firstDepartureDelta
                                        string candidate.aggregateTimeDelta; string candidate.durationDelta
                                    ]
                                match assignments.TryGetValue(key) with
                                | true, (_, _, dates, _) -> dates.[dateIndex] <- true
                                | _ ->
                                    let dates = emptyDates prepared.window
                                    dates.[dateIndex] <- true
                                    assignments.[key] <- candidate, methodName, dates, runnerMargin
                                    addCandidateScore {
                                        sourceTripId = sourceTripId
                                        targetTripId = targetTripId
                                        date = prepared.window.dates.[dateIndex]
                                        routeMethod = routeMethod
                                        tripMethod = candidate.tier
                                        editCount = candidate.editCount
                                        alignedCallCount = candidate.alignedCallCount
                                        firstDepartureDelta = candidate.firstDepartureDelta
                                        aggregateTimeDelta = candidate.aggregateTimeDelta
                                        durationDelta = candidate.durationDelta
                                        maximumTimeDelta = candidate.maximumTimeDelta
                                        squaredTimeDelta = candidate.squaredTimeDelta
                                        runnerUpMargin = runnerMargin
                                    }
                        | Ambiguous(best, semanticClasses) ->
                            addDiagnostic prepared.diagnostics "trip_same_date_ambiguous" sourceTripId $"{dateString prepared.window.dates.[dateIndex]} has {best.Length} equal-best candidates across {semanticClasses} semantic classes"
                            pendingAmbiguities.Add({
                                sourceTripId = sourceTripId
                                dateIndex = dateIndex
                                routeMethod = routeMethod
                                candidates = best
                            })
                        | NoMatch -> ()
                if sourceHadSharedCandidate then sharedDateCandidateSourceTrips.Add(sourceTripId) |> ignore
                if sourceHadCompatibleCandidate then patternCompatibleSourceTrips.Add(sourceTripId) |> ignore
                if not sourceHadMatch then
                    if candidateTrips.Length = 0 then
                        addDiagnostic prepared.diagnostics "trip_route_candidate_unresolved" sourceTripId "No target trips exist on the candidate routes inside the GVD"
                    elif signatureCandidates.Length = 0 then
                        addDiagnostic prepared.diagnostics "trip_signature_unresolved" sourceTripId "No candidate has an exact configured stop/time signature"
                    elif not sourceHadSharedCandidate then
                        snapshotGapSourceTrips.Add(sourceTripId) |> ignore
                        addDiagnostic prepared.diagnostics "base_snapshot_gap" sourceTripId "Compatible target evidence exists, but no target trip operates on a shared source date"
                    else
                        addDiagnostic prepared.diagnostics "trip_validity_unresolved" sourceTripId "Signature candidates exist, but none is uniquely active on a shared operating date"
                for KeyValue(_, (candidate, methodName, dates, runnerMargin)) in assignments do
                    bindings.Add({
                        sourceId = sourceIdentity prepared.binding.sourceId sourceTripRow
                        sourceTripId = sourceTripId
                        targetTripId = candidate.tripId
                        dates = dateSets.Intern dates
                        method = methodName
                        sourceCalls = calls
                        sourceShapeId = optionText (rowValue sourceTripRow "shape_id")
                        sourceOrdinalByTarget = candidate.sourceOrdinalByTarget
                        editCount = candidate.editCount
                        firstDepartureDelta = candidate.firstDepartureDelta
                        aggregateTimeDelta = candidate.aggregateTimeDelta
                        durationDelta = candidate.durationDelta
                        runnerUpMargin = runnerMargin
                        completePattern = candidate.editCount = 0
                    })
        | (true, sourceActive), _ when anyDate sourceActive ->
            addDiagnostic prepared.diagnostics "trip_call_pattern_unavailable" sourceTripId "The active source trip has no eligible parsed call pattern"
        | _ -> ()
        matchedSourceTripsProcessed <- matchedSourceTripsProcessed + 1L
        if matchedSourceTripsProcessed % 1000L = 0L then
            logProgress "match-source-trips" matchedSourceTripsProcessed (Some (int64 source.tripRows.Length))
    logProgress "match-source-trips" matchedSourceTripsProcessed (Some (int64 source.tripRows.Length))

    let targetClaims = Dictionary<struct (string * int), HashSet<string>>()
    let addTargetClaim targetTripId dateIndex sourceTripId =
        let key = struct (targetTripId, dateIndex)
        match targetClaims.TryGetValue(key) with
        | true, values -> values.Add(sourceTripId) |> ignore
        | _ ->
            let values = HashSet<string>(StringComparer.Ordinal)
            values.Add(sourceTripId) |> ignore
            targetClaims.[key] <- values
    let pendingTargets =
        pendingAmbiguities
        |> Seq.collect (fun pending -> pending.candidates |> Seq.map (fun candidate -> struct (candidate.tripId, pending.dateIndex)))
        |> HashSet
    for matchBinding in bindings do
        for dateIndex in 0 .. matchBinding.dates.Length - 1 do
            if matchBinding.dates.[dateIndex] && pendingTargets.Contains(struct (matchBinding.targetTripId, dateIndex)) then addTargetClaim matchBinding.targetTripId dateIndex matchBinding.sourceTripId

    let resolveAvailabilityPass (pendingValues: PendingAmbiguity array) =
        let proposals = Dictionary<struct (string * int), ResizeArray<PendingAmbiguity * CandidateMatch>>()
        for pending in pendingValues do
            let unclaimed =
                pending.candidates
                |> Array.filter (fun candidate ->
                    match targetClaims.TryGetValue(struct (candidate.tripId, pending.dateIndex)) with
                    | false, _ -> true
                    | true, claims -> claims |> Seq.forall ((=) pending.sourceTripId))
            if unclaimed.Length = 1 then
                let candidate = unclaimed.[0]
                let key = struct (candidate.tripId, pending.dateIndex)
                match proposals.TryGetValue(key) with
                | true, values -> values.Add(pending, candidate)
                | _ ->
                    let values = ResizeArray<PendingAmbiguity * CandidateMatch>()
                    values.Add(pending, candidate)
                    proposals.[key] <- values
        let acceptedKeys = HashSet<string>(StringComparer.Ordinal)
        let mutable acceptedThisPass = 0
        for KeyValue(_, proposalsForTarget) in proposals do
            if proposalsForTarget.Count = 1 then
                let pending, candidate = proposalsForTarget.[0]
                let dates = emptyDates prepared.window
                dates.[pending.dateIndex] <- true
                let sourceTripRow = source.tripsById.[pending.sourceTripId]
                let methodName = pending.routeMethod + "+" + candidate.tier
                bindings.Add({
                    sourceId = sourceIdentity prepared.binding.sourceId sourceTripRow
                    sourceTripId = pending.sourceTripId
                    targetTripId = candidate.tripId
                    dates = dateSets.Intern dates
                    method = methodName
                    sourceCalls = indexes.sourceCalls.[pending.sourceTripId]
                    sourceShapeId = optionText (rowValue sourceTripRow "shape_id")
                    sourceOrdinalByTarget = candidate.sourceOrdinalByTarget
                    editCount = candidate.editCount
                    firstDepartureDelta = candidate.firstDepartureDelta
                    aggregateTimeDelta = candidate.aggregateTimeDelta
                    durationDelta = candidate.durationDelta
                    runnerUpMargin = None
                    completePattern = candidate.editCount = 0
                })
                addCandidateScore {
                    sourceTripId = pending.sourceTripId
                    targetTripId = candidate.tripId
                    date = prepared.window.dates.[pending.dateIndex]
                    routeMethod = pending.routeMethod + "+target_availability"
                    tripMethod = candidate.tier
                    editCount = candidate.editCount
                    alignedCallCount = candidate.alignedCallCount
                    firstDepartureDelta = candidate.firstDepartureDelta
                    aggregateTimeDelta = candidate.aggregateTimeDelta
                    durationDelta = candidate.durationDelta
                    maximumTimeDelta = candidate.maximumTimeDelta
                    squaredTimeDelta = candidate.squaredTimeDelta
                    runnerUpMargin = None
                }
                addTargetClaim candidate.tripId pending.dateIndex pending.sourceTripId
                let pendingKey = pending.sourceTripId + "|" + string pending.dateIndex
                acceptedKeys.Add(pendingKey) |> ignore
                resolvedAmbiguityKeys.Add(pending.sourceTripId + "|" + dateString prepared.window.dates.[pending.dateIndex]) |> ignore
                addDiagnostic prepared.diagnostics "trip_target_availability_resolved" pending.sourceTripId $"{dateString prepared.window.dates.[pending.dateIndex]} selected the only target not already claimed by another source trip"
                acceptedThisPass <- acceptedThisPass + 1
        let remaining = pendingValues |> Array.filter (fun pending -> not (acceptedKeys.Contains(pending.sourceTripId + "|" + string pending.dateIndex)))
        remaining, acceptedThisPass

    let mutable unresolvedPending = pendingAmbiguities.ToArray()
    let mutable availabilityPass = 0
    let mutable changed = true
    while changed do
        availabilityPass <- availabilityPass + 1
        let remaining, accepted = resolveAvailabilityPass unresolvedPending
        unresolvedPending <- remaining
        logProgress $"resolve-trip-target-availability-{availabilityPass}" (int64 accepted) (Some (int64 remaining.Length))
        changed <- accepted > 0

    pendingAmbiguities.Clear()
    pendingAmbiguities.AddRange(unresolvedPending)
    for pending in unresolvedPending do
        let allClaimed =
            pending.candidates
            |> Array.forall (fun candidate ->
                match targetClaims.TryGetValue(struct (candidate.tripId, pending.dateIndex)) with
                | true, claims -> claims |> Seq.exists ((<>) pending.sourceTripId)
                | _ -> false)
        if allClaimed then
            capacityGapDates.Add(pending.sourceTripId + "|" + string pending.dateIndex) |> ignore
            addDiagnostic prepared.diagnostics "base_snapshot_capacity_gap" pending.sourceTripId $"{dateString prepared.window.dates.[pending.dateIndex]} has no unclaimed equal-best national trip"
        for candidate in pending.candidates do
            let trip = prepared.baseTrips.[candidate.tripId]
            let stableLine =
                match prepared.baseCisByRoute.TryGetValue(trip.routeId) with
                | true, cisLineId -> "cis:" + cisLineId
                | _ -> "route:" + trip.routeId
            ambiguousCandidateRows.Add [|
                pending.sourceTripId; dateString prepared.window.dates.[pending.dateIndex]; candidate.tripId; trip.routeId; stableLine
                pending.routeMethod; candidate.tier; string candidate.editCount; string candidate.alignedCallCount
                string candidate.firstDepartureDelta; string candidate.aggregateTimeDelta; string candidate.durationDelta
                string candidate.maximumTimeDelta; string candidate.squaredTimeDelta; indexes.candidateEquivalenceKey candidate
            |]

    let matchedAfterAvailability = bindings |> Seq.map (fun value -> value.sourceTripId) |> HashSet<string>
    for sourceTripId, pendingForSource in unresolvedPending |> Array.groupBy (fun value -> value.sourceTripId) do
        if not (matchedAfterAvailability.Contains(sourceTripId))
           && (pendingForSource |> Array.forall (fun pending -> capacityGapDates.Contains(sourceTripId + "|" + string pending.dateIndex))) then
            capacityGapSourceTrips.Add(sourceTripId) |> ignore
    let unmatchedTrips = source.tripRows.Length - matchedAfterAvailability.Count
    let ambiguousTrips = unresolvedPending |> Seq.map (fun value -> value.sourceTripId) |> Seq.distinct |> Seq.length
    Log.Information(
        "Regional overlay trip matching: matched_source_trips={MatchedTrips}; unmatched_source_trips={UnmatchedTrips}; ambiguous_source_trips={AmbiguousTrips}",
        (bindings |> Seq.map (fun value -> value.sourceTripId) |> Seq.distinct |> Seq.length), unmatchedTrips, ambiguousTrips)

    let acceptedSourceTripIds =
        Seq.append (bindings |> Seq.map (fun binding -> binding.sourceTripId)) (source.tripProjections |> Seq.map (fun projection -> projection.sourceTripId))
        |> Set.ofSeq
    let activeSourceTripIds =
        source.tripRows
        |> Array.choose (fun row ->
            match source.dates.TryGetValue(rowValue row "service_id") with
            | true, dates when anyDate dates -> Some (rowValue row "trip_id")
            | _ -> None)
        |> Set.ofArray
    let sourceCallCountByTrip = indexes.sourceCalls |> Seq.map (fun pair -> pair.Key, pair.Value.Length) |> dict
    let coverageTotalSourceCalls =
        indexes.sourceCalls
        |> Seq.sumBy (fun pair -> if activeSourceTripIds.Contains(pair.Key) then pair.Value.Length else 0)
    let coverageTotalSourceCallDates =
        source.tripRows
        |> Seq.sumBy (fun row ->
            let sourceTripId = rowValue row "trip_id"
            match source.dates.TryGetValue(rowValue row "service_id"), sourceCallCountByTrip.TryGetValue(sourceTripId) with
            | (true, dates), (true, callCount) -> (dates.Count) * callCount
            | _ -> 0)
    let coverageMatchedSourceCalls =
        indexes.sourceCalls
        |> Seq.sumBy (fun pair -> if acceptedSourceTripIds.Contains(pair.Key) then pair.Value.Length else 0)
    let modeForSourceTrip sourceTripId =
        let routeId = rowValue source.tripsById.[sourceTripId] "route_id"
        modeClass (rowValue source.routes.[routeId] "route_type")
    let totalCallsByMode = Dictionary<string, int>(StringComparer.Ordinal)
    let matchedCallsByMode = Dictionary<string, int>(StringComparer.Ordinal)
    let stopGroupsByMode = Dictionary<string, HashSet<string>>(StringComparer.Ordinal)
    let addCount (values: Dictionary<string, int>) key count =
        match values.TryGetValue(key) with
        | true, current -> values.[key] <- current + count
        | _ -> values.[key] <- count
    for sourceTripId in activeSourceTripIds do
        let mode = modeForSourceTrip sourceTripId
        match indexes.sourceCalls.TryGetValue(sourceTripId) with
        | true, calls ->
            addCount totalCallsByMode mode calls.Length
            if acceptedSourceTripIds.Contains(sourceTripId) then addCount matchedCallsByMode mode calls.Length
            let groups =
                match stopGroupsByMode.TryGetValue(mode) with
                | true, values -> values
                | _ ->
                    let values = HashSet<string>(StringComparer.Ordinal)
                    stopGroupsByMode.[mode] <- values
                    values
            for call in calls do
                match source.groupByMember.TryGetValue(call.stopId) with
                | true, groupId -> groups.Add(groupId) |> ignore
                | _ -> ()
        | _ -> ()
    let stopCoverageByMode =
        stopGroupsByMode
        |> Seq.map (fun pair ->
            pair.Key,
            (pair.Value |> Seq.filter source.stopGroupMatches.ContainsKey |> Seq.length),
            pair.Value.Count)
        |> Seq.toArray
    let callCoverageByMode =
        totalCallsByMode
        |> Seq.map (fun pair ->
            let matched = match matchedCallsByMode.TryGetValue(pair.Key) with | true, value -> value | _ -> 0
            pair.Key, matched, pair.Value)
        |> Seq.toArray
    prepared.baseStopSpatial.Clear()
    logProgress "release-matching-indexes" 1L (Some 1L)
    {
        bindings = bindings
        candidateScoreReports = candidateScoreReports.Rows
        ambiguousCandidateRows = ambiguousCandidateRows.Rows
        resolvedAmbiguityKeys = resolvedAmbiguityKeys
        sharedDateCandidateSourceTrips = sharedDateCandidateSourceTrips
        patternCompatibleSourceTrips = patternCompatibleSourceTrips
        snapshotGapSourceTrips = snapshotGapSourceTrips
        capacityGapSourceTrips = capacityGapSourceTrips
        unresolvedPending = unresolvedPending
        matchedAfterAvailability = matchedAfterAvailability
        sourceCallCountByTrip = sourceCallCountByTrip
        coverageTotalSourceCalls = coverageTotalSourceCalls
        coverageTotalSourceCallDates = coverageTotalSourceCallDates
        coverageMatchedSourceCalls = coverageMatchedSourceCalls
        modeForSourceTrip = modeForSourceTrip
        addCount = addCount
        stopCoverageByMode = stopCoverageByMode
        callCoverageByMode = callCoverageByMode
    }
