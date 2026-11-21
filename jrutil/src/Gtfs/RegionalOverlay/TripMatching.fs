// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.TripMatching

open System
open System.Collections.Generic
open Serilog

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Values
open JrUtil.RegionalOverlay.GtfsFiles
open JrUtil.RegionalOverlay.TripScoring
open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.OutputIds
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay

type Input = {
    prepared: InputPreparation.Result
    source: SourceAnalysis.Result
    indexes: SourceAnalysis.MatchingIndexes
}

/// Source call counts behind the coverage reports, taken while the call index is alive.
type Coverage = {
    sourceCallCountByTrip: IDictionary<string, int>
    totalSourceCalls: int
    totalSourceCallDates: int
    matchedSourceCalls: int
    stopCoverageByMode: (string * int * int) array
    callCoverageByMode: (string * int * int) array
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
    coverage: Coverage
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

/// What every source trip is matched with.
type private Matcher = {
    prepared: InputPreparation.Result
    source: SourceAnalysis.Result
    indexes: SourceAnalysis.MatchingIndexes
    tierRank: TripTier -> int
    tierEnabled: TripTier -> bool
    alignmentForLength: int -> int option array
}

/// Everything matching collects across source trips.
type private Collected = {
    bindings: ResizeArray<MatchBinding>
    dateSets: DateSet.Pool
    candidateScores: Scratch.RowLog
    ambiguousCandidateRows: Scratch.RowLog
    pending: ResizeArray<PendingAmbiguity>
    resolvedAmbiguityKeys: HashSet<string>
    sharedDateCandidateSourceTrips: HashSet<string>
    patternCompatibleSourceTrips: HashSet<string>
    snapshotGapSourceTrips: HashSet<string>
    capacityGapSourceTrips: HashSet<string>
}

/// A source trip's calls as the tiers compare them.
type private SourceView = {
    full: string
    endpoints: string
    first: string
    pattern: string array
    patternKey: string
    arrivals: int array
    departures: int array
}

let private sourceView normalizeTime (calls: CallValue array) =
    let pattern = calls |> Array.map (fun call -> call.stopPlaceId)
    let arrivals, departures = sourceTimeArrays calls
    {
        full = signature normalizeTime "" calls true true
        endpoints = signature normalizeTime "" calls false true
        first = signature normalizeTime "" calls false false
        pattern = pattern
        patternKey = stopPatternKey pattern
        arrivals = arrivals
        departures = departures
    }

/// The strongest enabled tier under which a base trip agrees with the source view.
let private candidateFor (matcher: Matcher) (view: SourceView) tripId =
    match matcher.indexes.baseSignatures.TryGetValue(tripId) with
    | false, _ -> None
    | true, target ->
        let policy = matcher.prepared.policy.source.tripMatch
        let exactPattern = target.pattern = view.pattern
        let exact tier = Some (tier, 0, matcher.alignmentForLength target.pattern.Length)
        let tierAndAlignment =
            if exactPattern && target.full = view.full && matcher.tierEnabled FullSignature then exact FullSignature
            elif exactPattern && target.patternEndpoints = view.endpoints && matcher.tierEnabled PatternEndpoints then exact PatternEndpoints
            elif exactPattern && target.patternFirst = view.first && matcher.tierEnabled PatternFirst then exact PatternFirst
            elif exactPattern && policy.exactPatternProximity && matcher.tierEnabled PatternNearest then exact PatternNearest
            elif matcher.tierEnabled PatternEditNearest then
                patternEditAlignment policy.patternEdit view.pattern target.pattern
                |> Option.map (fun (edits, alignment) -> PatternEditNearest, edits, alignment)
            else None
        tierAndAlignment
        |> Option.map (fun (tier, edits, alignment) ->
            let firstDelta, aggregateDelta, durationDelta, maximumDelta, squaredDelta, alignedCalls =
                candidateTimeScore view.arrivals view.departures target alignment
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

/// Candidates on the source route's asserted (primary) routes and on routes that
/// only share its label and mode (structural), regardless of operating dates.
type private TripCandidates = {
    primaryRouteMethod: RouteMethod
    primary: CandidateMatch array
    structural: CandidateMatch array
    /// Base trips on any candidate route, whether or not their calls agree.
    routeTripCount: int
}

let private tripCandidates (matcher: Matcher) sourceRouteId (view: SourceView) =
    let indexes = matcher.indexes
    let primaryRoutes, primaryRouteMethod = indexes.routeCandidates.[sourceRouteId]
    let structuralRoutes = indexes.structuralRouteCandidates.[sourceRouteId] |> Array.filter (fun route -> not (primaryRoutes |> Array.contains route))
    let tripsForRoutes routes =
        routes
        |> Array.collect (fun routeId -> match indexes.baseTripIdsByRoute.TryGetValue(routeId) with | true, ids -> ids | _ -> [||])
        |> Array.distinct
    let exactTripsForRoutes routes =
        routes
        |> Array.collect (fun routeId ->
            match indexes.baseTripIdsByRouteAndPattern.TryGetValue(struct (routeId, view.patternKey)) with
            | true, values -> values |> Seq.toArray
            | _ -> [||])
        |> Array.distinct
    let primaryTrips = tripsForRoutes primaryRoutes
    let structuralTrips = tripsForRoutes structuralRoutes
    // Trips with the very same stop pattern make every other trip of the route irrelevant.
    let matching routes trips =
        let exact = exactTripsForRoutes routes
        (if exact.Length > 0 then exact else trips) |> Array.choose (candidateFor matcher view)
    {
        primaryRouteMethod = primaryRouteMethod
        primary = matching primaryRoutes primaryTrips
        structural = matching structuralRoutes structuralTrips
        routeTripCount = (Array.append primaryTrips structuralTrips |> Array.distinct).Length
    }

let private addCandidateScore (collected: Collected) sourceTripId (date: NodaTime.LocalDate) (routeMethod: string) (candidate: CandidateMatch) (runnerUpMargin: int64 option) =
    collected.candidateScores.Add [|
        sourceTripId; candidate.tripId; dateString date; routeMethod; TripTier.name candidate.tier
        string candidate.editCount; string candidate.alignedCallCount; string candidate.firstDepartureDelta; string candidate.aggregateTimeDelta; string candidate.durationDelta
        string candidate.maximumTimeDelta; string candidate.squaredTimeDelta
        runnerUpMargin |> Option.map string |> Option.defaultValue ""
    |]

let private bindingFor (matcher: Matcher) (collected: Collected) (sourceTripRow: CsvRow) (calls: CallValue array) routeMethod (candidate: CandidateMatch) (dates: bool array) runnerUpMargin : MatchBinding =
    {
        sourceId = sourceIdentity matcher.prepared.binding.sourceId sourceTripRow
        sourceTripId = rowValue sourceTripRow "trip_id"
        targetTripId = candidate.tripId
        dates = collected.dateSets.Intern dates
        routeMethod = routeMethod
        tier = candidate.tier
        sourceCalls = calls
        sourceShapeId = optionText (rowValue sourceTripRow "shape_id")
        sourceOrdinalByTarget = candidate.sourceOrdinalByTarget
        editCount = candidate.editCount
        firstDepartureDelta = candidate.firstDepartureDelta
        aggregateTimeDelta = candidate.aggregateTimeDelta
        durationDelta = candidate.durationDelta
        runnerUpMargin = runnerUpMargin
    }

/// Choose a base trip for each operating date of one source trip; equal-best
/// candidates of different meaning stay pending for the availability passes.
let private matchSourceTrip (matcher: Matcher) (collected: Collected) (sourceTripRow: CsvRow) (sourceActive: DateSet.Dates) (calls: CallValue array) =
    let prepared = matcher.prepared
    let sourceTripId = rowValue sourceTripRow "trip_id"
    let candidates = tripCandidates matcher (rowValue sourceTripRow "route_id") (sourceView prepared.normalizeMatchingTime calls)
    let assignments = Dictionary<string, CandidateMatch * RouteMethod * bool array * int64 option>(StringComparer.Ordinal)
    let mutable sourceHadMatch = false
    let mutable sourceHadSharedCandidate = false
    let mutable sourceHadCompatibleCandidate = false
    for dateIndex in 0 .. prepared.window.dates.Length - 1 do
        if sourceActive.[dateIndex] then
            let active (values: CandidateMatch array) =
                values
                |> Array.filter (fun candidate ->
                    let serviceId = prepared.baseTrips.[candidate.tripId].serviceId
                    match prepared.baseDates.TryGetValue(serviceId) with
                    | true, dates -> dates.[dateIndex]
                    | _ -> false)
            let primaryActive = active candidates.primary
            let structuralActive = active candidates.structural
            if primaryActive.Length > 0 || structuralActive.Length > 0 then sourceHadSharedCandidate <- true
            let routeMethod, dateCandidates =
                if primaryActive.Length > 0 then candidates.primaryRouteMethod, primaryActive
                else StructuralTripEvidence, structuralActive
            if dateCandidates.Length > 0 then sourceHadCompatibleCandidate <- true
            let date = prepared.window.dates.[dateIndex]
            match chooseCandidates matcher.tierRank matcher.indexes.candidateEquivalenceKey dateCandidates with
            | Accepted(acceptedBest, runnerMargin) ->
                sourceHadMatch <- true
                if acceptedBest.Length > 1 then
                    addDiagnostic prepared.diagnostics "trip_equivalent_tie_expanded" sourceTripId $"{dateString date} overlays {acceptedBest.Length} semantically equivalent target trips"
                for candidate in acceptedBest do
                    let key =
                        String.concat "\u001f" [
                            candidate.tripId; RouteMethod.name routeMethod + "+" + TripTier.name candidate.tier
                            string candidate.editCount; string candidate.firstDepartureDelta
                            string candidate.aggregateTimeDelta; string candidate.durationDelta
                        ]
                    match assignments.TryGetValue(key) with
                    | true, (_, _, dates, _) -> dates.[dateIndex] <- true
                    | _ ->
                        let dates = emptyDates prepared.window
                        dates.[dateIndex] <- true
                        assignments.[key] <- (candidate, routeMethod, dates, runnerMargin)
                        addCandidateScore collected sourceTripId date (RouteMethod.name routeMethod) candidate runnerMargin
            | Ambiguous(best, semanticClasses) ->
                addDiagnostic prepared.diagnostics "trip_same_date_ambiguous" sourceTripId $"{dateString date} has {best.Length} equal-best candidates across {semanticClasses} semantic classes"
                collected.pending.Add({ sourceTripId = sourceTripId; dateIndex = dateIndex; routeMethod = routeMethod; candidates = best })
            | NoMatch -> ()
    if sourceHadSharedCandidate then collected.sharedDateCandidateSourceTrips.Add(sourceTripId) |> ignore
    if sourceHadCompatibleCandidate then collected.patternCompatibleSourceTrips.Add(sourceTripId) |> ignore
    if not sourceHadMatch then
        if candidates.routeTripCount = 0 then
            addDiagnostic prepared.diagnostics "trip_route_candidate_unresolved" sourceTripId "No target trips exist on the candidate routes inside the GVD"
        elif candidates.primary.Length = 0 && candidates.structural.Length = 0 then
            addDiagnostic prepared.diagnostics "trip_signature_unresolved" sourceTripId "No candidate has an exact configured stop/time signature"
        elif not sourceHadSharedCandidate then
            collected.snapshotGapSourceTrips.Add(sourceTripId) |> ignore
            addDiagnostic prepared.diagnostics "base_snapshot_gap" sourceTripId "Compatible target evidence exists, but no target trip operates on a shared source date"
        else
            addDiagnostic prepared.diagnostics "trip_validity_unresolved" sourceTripId "Signature candidates exist, but none is uniquely active on a shared operating date"
    for KeyValue(_, (candidate, routeMethod, dates, runnerMargin)) in assignments do
        collected.bindings.Add(bindingFor matcher collected sourceTripRow calls routeMethod candidate dates runnerMargin)

/// Give a pending source trip the only equal-best base trip no other source trip
/// claims on that date, repeating while that settles anything. Returns what stays
/// ambiguous and the source trips matched in the end.
let private resolveByAvailability (matcher: Matcher) (collected: Collected) =
    let prepared = matcher.prepared
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
        collected.pending
        |> Seq.collect (fun pending -> pending.candidates |> Seq.map (fun candidate -> struct (candidate.tripId, pending.dateIndex)))
        |> HashSet
    for matchBinding in collected.bindings do
        for dateIndex in 0 .. matchBinding.dates.Length - 1 do
            if matchBinding.dates.[dateIndex] && pendingTargets.Contains(struct (matchBinding.targetTripId, dateIndex)) then addTargetClaim matchBinding.targetTripId dateIndex matchBinding.sourceTripId

    let claimedByAnother (pending: PendingAmbiguity) (candidate: CandidateMatch) =
        match targetClaims.TryGetValue(struct (candidate.tripId, pending.dateIndex)) with
        | true, claims -> claims |> Seq.exists ((<>) pending.sourceTripId)
        | _ -> false

    let resolvePass (pendingValues: PendingAmbiguity array) =
        let proposals = Dictionary<struct (string * int), ResizeArray<PendingAmbiguity * CandidateMatch>>()
        for pending in pendingValues do
            let unclaimed = pending.candidates |> Array.filter (claimedByAnother pending >> not)
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
                let date = prepared.window.dates.[pending.dateIndex]
                let sourceTripRow = matcher.source.tripsById.[pending.sourceTripId]
                collected.bindings.Add(
                    bindingFor matcher collected sourceTripRow matcher.indexes.sourceCalls.[pending.sourceTripId] pending.routeMethod candidate dates None)
                addCandidateScore collected pending.sourceTripId date (RouteMethod.name pending.routeMethod + "+target_availability") candidate None
                addTargetClaim candidate.tripId pending.dateIndex pending.sourceTripId
                acceptedKeys.Add(pending.sourceTripId + "|" + string pending.dateIndex) |> ignore
                collected.resolvedAmbiguityKeys.Add(pending.sourceTripId + "|" + dateString date) |> ignore
                addDiagnostic prepared.diagnostics "trip_target_availability_resolved" pending.sourceTripId $"{dateString date} selected the only target not already claimed by another source trip"
                acceptedThisPass <- acceptedThisPass + 1
        let remaining = pendingValues |> Array.filter (fun pending -> not (acceptedKeys.Contains(pending.sourceTripId + "|" + string pending.dateIndex)))
        remaining, acceptedThisPass

    let mutable unresolved = collected.pending.ToArray()
    let mutable pass = 0
    let mutable changed = true
    while changed do
        pass <- pass + 1
        let remaining, accepted = resolvePass unresolved
        unresolved <- remaining
        logProgress $"resolve-trip-target-availability-{pass}" (int64 accepted) (Some (int64 remaining.Length))
        changed <- accepted > 0

    // What stays ambiguous goes to the review rows; a date whose every equal-best
    // base trip another source trip already took is a capacity gap of the snapshot.
    let capacityGapDates = HashSet<string>(StringComparer.Ordinal)
    for pending in unresolved do
        if pending.candidates |> Array.forall (claimedByAnother pending) then
            capacityGapDates.Add(pending.sourceTripId + "|" + string pending.dateIndex) |> ignore
            addDiagnostic prepared.diagnostics "base_snapshot_capacity_gap" pending.sourceTripId $"{dateString prepared.window.dates.[pending.dateIndex]} has no unclaimed equal-best national trip"
        for candidate in pending.candidates do
            let trip = prepared.baseTrips.[candidate.tripId]
            let stableLine =
                match prepared.baseCisByRoute.TryGetValue(trip.routeId) with
                | true, cisLineId -> "cis:" + cisLineId
                | _ -> "route:" + trip.routeId
            collected.ambiguousCandidateRows.Add [|
                pending.sourceTripId; dateString prepared.window.dates.[pending.dateIndex]; candidate.tripId; trip.routeId; stableLine
                RouteMethod.name pending.routeMethod; TripTier.name candidate.tier; string candidate.editCount; string candidate.alignedCallCount
                string candidate.firstDepartureDelta; string candidate.aggregateTimeDelta; string candidate.durationDelta
                string candidate.maximumTimeDelta; string candidate.squaredTimeDelta; matcher.indexes.candidateEquivalenceKey candidate
            |]
    let matched = collected.bindings |> Seq.map (fun value -> value.sourceTripId) |> HashSet<string>
    for sourceTripId, pendingForSource in unresolved |> Array.groupBy (fun value -> value.sourceTripId) do
        if not (matched.Contains(sourceTripId))
           && (pendingForSource |> Array.forall (fun pending -> capacityGapDates.Contains(sourceTripId + "|" + string pending.dateIndex))) then
            collected.capacityGapSourceTrips.Add(sourceTripId) |> ignore
    unresolved, matched

/// Count source calls and stop groups for the coverage reports.
let private coverage (source: SourceAnalysis.Result) (indexes: SourceAnalysis.MatchingIndexes) (bindings: ResizeArray<MatchBinding>) : Coverage =
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
    let totalSourceCalls =
        indexes.sourceCalls
        |> Seq.sumBy (fun pair -> if activeSourceTripIds.Contains(pair.Key) then pair.Value.Length else 0)
    let totalSourceCallDates =
        source.tripRows
        |> Seq.sumBy (fun row ->
            let sourceTripId = rowValue row "trip_id"
            match source.dates.TryGetValue(rowValue row "service_id"), sourceCallCountByTrip.TryGetValue(sourceTripId) with
            | (true, dates), (true, callCount) -> (dates.Count) * callCount
            | _ -> 0)
    let matchedSourceCalls =
        indexes.sourceCalls
        |> Seq.sumBy (fun pair -> if acceptedSourceTripIds.Contains(pair.Key) then pair.Value.Length else 0)
    let totalCallsByMode = Dictionary<string, int>(StringComparer.Ordinal)
    let matchedCallsByMode = Dictionary<string, int>(StringComparer.Ordinal)
    let stopGroupsByMode = Dictionary<string, HashSet<string>>(StringComparer.Ordinal)
    for sourceTripId in activeSourceTripIds do
        let mode = SourceAnalysis.modeOfTrip source sourceTripId
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
    {
        sourceCallCountByTrip = sourceCallCountByTrip
        totalSourceCalls = totalSourceCalls
        totalSourceCallDates = totalSourceCallDates
        matchedSourceCalls = matchedSourceCalls
        stopCoverageByMode =
            stopGroupsByMode
            |> Seq.map (fun pair ->
                pair.Key,
                (pair.Value |> Seq.filter source.stopGroupMatches.ContainsKey |> Seq.length),
                pair.Value.Count)
            |> Seq.toArray
        callCoverageByMode =
            totalCallsByMode
            |> Seq.map (fun pair ->
                let matched = match matchedCallsByMode.TryGetValue(pair.Key) with | true, value -> value | _ -> 0
                pair.Key, matched, pair.Value)
            |> Seq.toArray
    }

/// Match source trips and return accepted bindings, quarantine and coverage evidence.
let matchTrips ({ prepared = prepared; source = source; indexes = indexes }: Input) : Result =
    let identityAlignments = Dictionary<int, int option array>()
    let tierRanks = prepared.policy.source.tripMatchTiers |> Array.mapi (fun index tier -> TripTier.parse tier, index) |> dict
    let matcher = {
        prepared = prepared
        source = source
        indexes = indexes
        tierRank = fun tier -> tierRanks.[tier]
        tierEnabled = tierRanks.ContainsKey
        alignmentForLength = fun length ->
            match identityAlignments.TryGetValue(length) with
            | true, alignment -> alignment
            | _ -> let alignment = identityAlignment length in identityAlignments.Add(length, alignment); alignment
    }
    let collected = {
        bindings = ResizeArray<MatchBinding>()
        dateSets = DateSet.Pool()
        candidateScores = new Scratch.RowLog(prepared.scratch)
        ambiguousCandidateRows = new Scratch.RowLog(prepared.scratch)
        pending = ResizeArray<PendingAmbiguity>()
        resolvedAmbiguityKeys = HashSet<string>(StringComparer.Ordinal)
        sharedDateCandidateSourceTrips = HashSet<string>(StringComparer.Ordinal)
        patternCompatibleSourceTrips = HashSet<string>(StringComparer.Ordinal)
        snapshotGapSourceTrips = HashSet<string>(StringComparer.Ordinal)
        capacityGapSourceTrips = HashSet<string>(StringComparer.Ordinal)
    }
    let mutable processed = 0L
    for sourceTripRow in source.tripRows do
        let sourceTripId = rowValue sourceTripRow "trip_id"
        match source.dates.TryGetValue(rowValue sourceTripRow "service_id"), indexes.sourceCalls.TryGetValue(sourceTripId) with
        | (true, sourceActive), (true, calls) when anyDate sourceActive ->
            if calls |> Array.forall (fun call -> source.mappedPlaceBySourceStop.ContainsKey(call.stopId)) then
                matchSourceTrip matcher collected sourceTripRow sourceActive calls
            else
                addDiagnostic prepared.diagnostics "trip_stop_unresolved" sourceTripId "At least one complete-pattern stop place is unresolved"
        | (true, sourceActive), _ when anyDate sourceActive ->
            addDiagnostic prepared.diagnostics "trip_call_pattern_unavailable" sourceTripId "The active source trip has no eligible parsed call pattern"
        | _ -> ()
        processed <- processed + 1L
        if processed % 1000L = 0L then
            logProgress "match-source-trips" processed (Some (int64 source.tripRows.Length))
    logProgress "match-source-trips" processed (Some (int64 source.tripRows.Length))

    let unresolvedPending, matchedAfterAvailability = resolveByAvailability matcher collected
    let ambiguousTrips = unresolvedPending |> Seq.map (fun value -> value.sourceTripId) |> Seq.distinct |> Seq.length
    Log.Information(
        "Regional overlay trip matching: matched_source_trips={MatchedTrips}; unmatched_source_trips={UnmatchedTrips}; ambiguous_source_trips={AmbiguousTrips}",
        matchedAfterAvailability.Count, source.tripRows.Length - matchedAfterAvailability.Count, ambiguousTrips)

    let coverage = coverage source indexes collected.bindings
    prepared.baseStopSpatial.Clear()
    logProgress "release-matching-indexes" 1L (Some 1L)
    {
        bindings = collected.bindings
        candidateScoreReports = collected.candidateScores.Rows
        ambiguousCandidateRows = collected.ambiguousCandidateRows.Rows
        resolvedAmbiguityKeys = collected.resolvedAmbiguityKeys
        sharedDateCandidateSourceTrips = collected.sharedDateCandidateSourceTrips
        patternCompatibleSourceTrips = collected.patternCompatibleSourceTrips
        snapshotGapSourceTrips = collected.snapshotGapSourceTrips
        capacityGapSourceTrips = collected.capacityGapSourceTrips
        unresolvedPending = unresolvedPending
        matchedAfterAvailability = matchedAfterAvailability
        coverage = coverage
    }
