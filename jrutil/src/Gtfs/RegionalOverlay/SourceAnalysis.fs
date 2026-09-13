// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.SourceAnalysis

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text.RegularExpressions
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
}

type Result = {
    sourceId: string
    agencies: IDictionary<string, CsvRow>
    stopRows: CsvRow array
    excludedTypes: Set<string>
    routeRows: CsvRow array
    routes: IDictionary<string, CsvRow>
    tripRows: CsvRow array
    tripsById: IDictionary<string, CsvRow>
    revision: string -> DateTime
    dates: Dictionary<string, DateSet.Dates>
    stopGroups: StopGroup array
    stopGroupById: IDictionary<string, StopGroup>
    groupByMember: Dictionary<string, string>
    stopGroupMatches: Dictionary<string, string>
    stopMatchMethods: Dictionary<string, string>
    stopMatchDistances: Dictionary<string, float>
    mappedPlaceBySourceStop: Map<string, string>
    stopInferenceEvidence: ResizeArray<StopInferenceEvidence>
    authoritativeGtfsStopCorrections: Dictionary<string, StopGroup>
    nativeStopPlaces: Dictionary<string, StopGroup>
    nativeRoutes: Dictionary<string, (CsvRow * string)>
    authorityDatesByCis: Dictionary<string, DateSet.Dates>
    tripProjections: SourceTripProjection array
}

/// Temporary indexes live only through contextual inference and trip matching.
/// They must not be retained by projection or output/report records.
type MatchingIndexes = {
    joinedValues: Dictionary<string, ResizeArray<string>>
    routeCandidates: Dictionary<string, (string array * string)>
    structuralRouteCandidates: Dictionary<string, string array>
    baseSignatures: Dictionary<string, BaseSignature>
    sourceCalls: Dictionary<string, CallValue array>
    baseTripIdsByRoute: IDictionary<string, string array>
    baseTripIdsByRouteAndPattern: Dictionary<struct (string * string), ResizeArray<string>>
    candidateEquivalenceKey: CandidateMatch -> string
}

/// Collect source-qualified stop, route and authority evidence without writing output.
let analyze ({ prepared = prepared }: Input) : Result * MatchingIndexes =
    let sourceAgencyRows = requireTable prepared.binding.payloadPath "agency.txt"
    let sourceAgencies = sourceAgencyRows |> Array.map (fun row -> rowValue row "agency_id", row) |> dict
    let sourceStopRows = requireTable prepared.binding.payloadPath "stops.txt"
    let sourceRouteRows = requireTable prepared.binding.payloadPath "routes.txt"
    let excludedTypes = prepared.policy.source.excludedRouteTypes |> Set.ofArray
    let sourceRouteRows =
        sourceRouteRows
        |> Array.filter (fun row ->
            let routeType = rowValue row "route_type"
            not (excludedTypes.Contains(routeType)) && modeClass routeType <> "heavy-rail")
    let sourceRoutes = sourceRouteRows |> Array.map (fun row -> rowValue row "route_id", row) |> dict
    let sourceTripRows =
        requireTable prepared.binding.payloadPath "trips.txt"
        |> Array.filter (fun row -> sourceRoutes.ContainsKey(rowValue row "route_id"))
    let sourceTripsById = sourceTripRows |> Array.map (fun row -> rowValue row "trip_id", row) |> dict
    let revisionPolicy = prepared.policy.source.tripMatch.sourceRevision
    let sourceRevisionRegex =
        if isNull (box revisionPolicy) then None
        else Some (Regex(revisionPolicy.regex, RegexOptions.CultureInvariant))
    let sourceRevisionCache = Dictionary<string, DateTime>(StringComparer.Ordinal)
    let sourceRevision sourceTripId =
        match sourceRevisionCache.TryGetValue(sourceTripId) with
        | _ when sourceRevisionRegex.IsNone -> DateTime.MinValue
        | true, revision -> revision
        | _ ->
            let sourceRow = sourceTripsById.[sourceTripId]
            let value = rowValue sourceRow prepared.policy.source.tripMatch.sourceRevision.column
            let matched = sourceRevisionRegex.Value.Match(value)
            let revision =
                if not matched.Success then
                    addDiagnostic prepared.diagnostics "source_revision_unresolved" sourceTripId $"No revision matched {prepared.policy.source.tripMatch.sourceRevision.column}"
                    DateTime.MinValue
                else
                    match DateTime.TryParseExact(
                        matched.Groups.["revision"].Value,
                        prepared.policy.source.tripMatch.sourceRevision.dateFormat,
                        CultureInfo.InvariantCulture,
                        DateTimeStyles.None) with
                    | true, parsed -> parsed.Date
                    | _ ->
                        addDiagnostic prepared.diagnostics "source_revision_unresolved" sourceTripId "Captured revision is not a valid configured date"
                        DateTime.MinValue
            sourceRevisionCache.[sourceTripId] <- revision
            revision
    let sourceTripsByRoute =
        sourceTripRows
        |> Array.groupBy (fun row -> rowValue row "route_id")
        |> dict
    let sourceDates =
        parseCalendar prepared.window (csvRows prepared.binding.payloadPath "calendar.txt") (csvRows prepared.binding.payloadPath "calendar_dates.txt")

    let activeSourceTripIdsForStops =
        sourceTripRows
        |> Array.choose (fun row ->
            match sourceDates.TryGetValue(rowValue row "service_id") with
            | true, dates when anyDate dates -> Some (rowValue row "trip_id")
            | _ -> None)
        |> Set.ofArray
    let scheduledSourceStopIds = HashSet<string>(StringComparer.Ordinal)
    let mutable scheduledCallRows = 0L
    for row in csvRows prepared.binding.payloadPath "stop_times.txt" do
        if activeSourceTripIdsForStops.Contains(rowValue row "trip_id") then
            scheduledSourceStopIds.Add(rowValue row "stop_id") |> ignore
        scheduledCallRows <- scheduledCallRows + 1L
        if scheduledCallRows % 500000L = 0L then logProgress "index-scheduled-source-stops" scheduledCallRows None
    logProgress "index-scheduled-source-stops" scheduledCallRows (Some scheduledCallRows)
    let sourceStopGroups = groupSourceStops prepared.policy (scheduledSourceStopIds |> Set.ofSeq) sourceStopRows
    let sourceStopGroupById = sourceStopGroups |> Array.map (fun group -> group.groupId, group) |> dict
    let sourceGroupByMember = Dictionary<string, string>(StringComparer.Ordinal)
    for group in sourceStopGroups do
        for stopMember in group.members do
            sourceGroupByMember.[rowValue stopMember "stop_id"] <- group.groupId
    let stopMatching = StopMatching.matchStops {
        prepared = prepared
        sourceStopRows = sourceStopRows
        sourceStopGroups = sourceStopGroups
        sourceGroupByMember = sourceGroupByMember
    }
    let mutable mappedPlaceBySourceStop = stopMatching.mappedPlaceBySourceStop

    let routeMatching = RouteMatching.buildCandidates {
        prepared = prepared
        stopMatching = stopMatching
        sourceRouteRows = sourceRouteRows
        sourceRoutes = sourceRoutes
        sourceTripsById = sourceTripsById
        sourceTripsByRoute = sourceTripsByRoute
    }

    let refreshSourceCallPlaces () =
        for calls in routeMatching.sourceCalls.Values do
            for index in 0 .. calls.Length - 1 do
                let call = calls.[index]
                let place = mappedPlaceBySourceStop |> Map.tryFind call.stopId |> Option.defaultValue call.stopId
                if place <> call.stopPlaceId then calls.[index] <- { call with stopPlaceId = place }

    let stopInferenceEvidence = ResizeArray<StopInferenceEvidence>()
    let inferenceGap (calls: CallValue array) =
        let groups = calls |> Array.choose (fun call ->
            match sourceGroupByMember.TryGetValue(call.stopId) with | true, group -> Some group | _ -> None)
        let unresolved = groups |> Array.filter (stopMatching.stopGroupMatches.ContainsKey >> not)
        let unresolvedGroups = Array.distinct unresolved
        let policy = prepared.policy.source.stopMatch.contextualInference
        if unresolvedGroups.Length <> policy.maximumUnresolvedGroupsPerTrip || calls.Length - unresolved.Length < policy.minimumMappedCalls then None
        else
            let group = unresolvedGroups.[0]
            let ordinals = calls |> Array.indexed |> Array.choose (fun (index, call) ->
                match sourceGroupByMember.TryGetValue(call.stopId) with
                | true, candidate when candidate = group -> Some index
                | _ -> None)
            match ordinals with
            | [| ordinal |] -> Some (group, ordinal, routeMatching.sourcePattern calls)
            | _ -> None

    let collectStopProposals () =
        let proposed = Dictionary<string, ResizeArray<StopInferenceEvidence>>(StringComparer.Ordinal)
        for sourceTripRow in sourceTripRows do
            let sourceTripId = rowValue sourceTripRow "trip_id"
            let sourceRouteId = rowValue sourceTripRow "route_id"
            let sourceServiceId = rowValue sourceTripRow "service_id"
            match sourceDates.TryGetValue(sourceServiceId), routeMatching.sourceCalls.TryGetValue(sourceTripId) with
            | (true, sourceActive), (true, calls) when anyDate sourceActive ->
                match inferenceGap calls with
                | None -> ()
                | Some (unresolvedGroup, unresolvedOrdinal, sourcePlaces) ->
                    let sourceArrivals, sourceDepartures = sourceTimeArrays calls
                    let primaryRoutes, _ = routeMatching.routeCandidates.[sourceRouteId]
                    let candidateRoutes =
                        Seq.append primaryRoutes routeMatching.structuralRouteCandidates.[sourceRouteId]
                        |> Seq.distinct
                        |> Seq.toArray
                    let candidates =
                        candidateRoutes
                        |> Array.collect (fun routeId ->
                            match routeMatching.baseTripIdsByRouteAndLength.TryGetValue(struct (routeId, calls.Length)) with
                            | true, values -> values |> Seq.toArray
                            | _ -> [||])
                        |> Array.distinct
                        |> Array.choose (fun tripId ->
                            let fingerprint = routeMatching.baseSignatures.[tripId]
                            let targetDates = prepared.baseDates.[prepared.baseTrips.[tripId].serviceId]
                            let knownPositionsMatch =
                                sourcePlaces
                                |> Array.mapi (fun index place ->
                                    match place with
                                    | Some sourcePlace -> sourcePlace = fingerprint.pattern.[index]
                                    | None -> true)
                                |> Array.forall id
                            if datesOverlap sourceActive targetDates && knownPositionsMatch then
                                let alignment = identityAlignment calls.Length
                                let firstDelta, aggregateDelta, durationDelta, maximumDelta, squaredDelta, alignedCalls =
                                    candidateTimeScore sourceArrivals sourceDepartures fingerprint alignment
                                Some ({
                                    tripId = tripId
                                    tier = "context_stop_inference"
                                    sourceOrdinalByTarget = alignment
                                    editCount = 0
                                    alignedCallCount = alignedCalls
                                    firstDepartureDelta = firstDelta
                                    aggregateTimeDelta = aggregateDelta
                                    durationDelta = durationDelta
                                    maximumTimeDelta = maximumDelta
                                    squaredTimeDelta = squaredDelta
                                }, fingerprint.pattern.[unresolvedOrdinal])
                            else None)
                    if candidates.Length > 0 then
                        let bestScore = candidates |> Array.map (fst >> candidateRank) |> Array.min
                        let best = candidates |> Array.filter (fst >> candidateRank >> (=) bestScore)
                        let targets = best |> Array.map snd |> Array.distinct
                        if targets.Length = 1 then
                            let evidence = {
                                sourceGroupId = unresolvedGroup
                                sourceTripId = sourceTripId
                                targetStopPlaceId = targets.[0]
                                sourceCallOrdinal = unresolvedOrdinal + 1
                                method = "trip_context_unique"
                            }
                            match proposed.TryGetValue(unresolvedGroup) with
                            | true, values -> values.Add(evidence)
                            | _ ->
                                let values = ResizeArray<StopInferenceEvidence>()
                                values.Add(evidence)
                                proposed.[unresolvedGroup] <- values
            | _ -> ()
        proposed

    let resolveStopProposals (proposed: Dictionary<string, ResizeArray<StopInferenceEvidence>>) = [|
        for KeyValue(groupId, evidence) in proposed do
            let targets = evidence |> Seq.map (fun value -> value.targetStopPlaceId) |> Seq.distinct |> Seq.toArray
            let sourceGroup = sourceStopGroupById.[groupId]
            match targets with
            | [| targetId |] when prepared.baseStopGroupById.ContainsKey(targetId) ->
                let target = prepared.baseStopGroupById.[targetId]
                if prepared.approximateStopName target.name || stopNameMatchRank sourceGroup.name target.name |> Option.isSome then
                    yield sourceGroup, targetId, target, evidence
                else
                    addDiagnostic prepared.diagnostics "stop_context_name_conflict" groupId $"Context target {target.name} is incompatible with {sourceGroup.name}"
            | targets when targets.Length > 1 ->
                addDiagnostic prepared.diagnostics "stop_context_conflict" groupId $"Context proposed {targets.Length} target stop places"
            | _ -> ()
    |]

    let applyStopProposals accepted =
        for sourceGroup, targetId, target, evidence in accepted do
            let groupId = sourceGroup.groupId
            stopMatching.stopGroupMatches.[groupId] <- targetId
            stopMatching.stopMatchMethods.[groupId] <- if prepared.approximateStopName target.name then "trip_context_gtfs_authoritative" else "trip_context_unique"
            prepared.stopGroupDistance sourceGroup target |> Option.iter (fun distance ->
                stopMatching.stopMatchDistances.[groupId] <- distance
                if distance > prepared.policy.source.stopMatch.maximumDistanceMetres && not (prepared.approximateStopName target.name) then
                    addDiagnostic prepared.diagnostics "stop_context_distance_warning" groupId $"Strong route/call context accepted a compatible established stop at {distance:F1} m")
            for memberRow in sourceGroup.members do
                mappedPlaceBySourceStop <- mappedPlaceBySourceStop |> Map.add (rowValue memberRow "stop_id") targetId
            stopInferenceEvidence.AddRange(evidence)

    if prepared.policy.source.stopMatch.contextualInference.enabled then
        let mutable iteration = 0
        let mutable changed = true
        while changed do
            iteration <- iteration + 1
            let accepted = collectStopProposals () |> resolveStopProposals
            applyStopProposals accepted
            changed <- accepted.Length > 0
            if changed then refreshSourceCallPlaces ()
            logProgress $"infer-stop-context-{iteration}" (int64 accepted.Length) None
        Log.Information(
            "Regional overlay contextual stop inference: inferred_groups={InferredGroups}; evidence_rows={EvidenceRows}",
            stopInferenceEvidence |> Seq.map (fun value -> value.sourceGroupId) |> Seq.distinct |> Seq.length,
            stopInferenceEvidence.Count)
    let authoritativeGtfsStopCorrections = Dictionary<string, StopGroup>(StringComparer.Ordinal)
    let approximateMatchedGroups =
        stopMatching.stopGroupMatches
        |> Seq.choose (fun pair ->
            if prepared.baseStopGroupById.ContainsKey(pair.Value) && prepared.approximateStopName prepared.baseStopGroupById.[pair.Value].name then
                Some (pair.Value, sourceStopGroupById.[pair.Key])
            else None)
        |> Seq.groupBy fst
    for targetId, matches in approximateMatchedGroups do
        let sourceGroups = matches |> Seq.map snd |> Seq.distinctBy (fun group -> group.groupId) |> Seq.toArray
        if sourceGroups.Length = 1 then
            authoritativeGtfsStopCorrections.[targetId] <- sourceGroups.[0]
            let groupId = sourceGroups.[0].groupId
            if not (stopMatching.stopMatchMethods.[groupId].Contains("gtfs_authoritative", StringComparison.Ordinal)) then
                stopMatching.stopMatchMethods.[groupId] <- stopMatching.stopMatchMethods.[groupId] + "+gtfs_authoritative"
            addDiagnostic prepared.diagnostics "stop_approximate_corrected" targetId $"{prepared.binding.sourceId} GTFS supplied authoritative name and geodata: {sourceGroups.[0].name}"
        else
            // A provisional JDF row cannot collapse multiple distinct source places.
            for sourceGroup in sourceGroups do
                stopMatching.stopGroupMatches.Remove(sourceGroup.groupId) |> ignore
                stopMatching.stopMatchMethods.Remove(sourceGroup.groupId) |> ignore
                stopMatching.stopMatchDistances.Remove(sourceGroup.groupId) |> ignore
                for memberRow in sourceGroup.members do
                    mappedPlaceBySourceStop <- mappedPlaceBySourceStop |> Map.remove (rowValue memberRow "stop_id")
            addDiagnostic prepared.diagnostics "stop_approximate_split" targetId $"{sourceGroups.Length} {prepared.binding.sourceId} places remain distinct instead of sharing one approximate JDF stop"
    refreshSourceCallPlaces ()
    let tripSetAuthorityModes = prepared.policy.source.tripSetAuthority.modes |> Set.ofArray
    let sourceNativeModes = prepared.policy.source.tripSetAuthority.sourceNativeModes |> Set.ofArray
    let tripSetAuthorityAllowed (row: CsvRow) mode =
        match row.TryGetValue("overlay_trip_set_authority_allowed") with
        | true, value -> value = "1"
        | _ -> tripSetAuthorityModes.Contains(mode)
    let sourceNativeAllowed (row: CsvRow) mode =
        match row.TryGetValue("overlay_source_native_allowed") with
        | true, value -> value = "1"
        | _ -> sourceNativeModes.Contains(mode)
    let sourceNativeStopPlaces = Dictionary<string, StopGroup>(StringComparer.Ordinal)
    for sourceTripRow in sourceTripRows do
        let sourceRouteId = rowValue sourceTripRow "route_id"
        let sourceMode = modeClass (rowValue sourceRoutes.[sourceRouteId] "route_type")
        match sourceDates.TryGetValue(rowValue sourceTripRow "service_id"), routeMatching.sourceCalls.TryGetValue(rowValue sourceTripRow "trip_id") with
        | (true, activeDates), (true, calls)
            when sourceNativeAllowed sourceTripRow sourceMode && anyDate activeDates ->
            for call in calls do
                if not (mappedPlaceBySourceStop.ContainsKey(call.stopId)) then
                    let groupId = sourceGroupByMember.[call.stopId]
                    let group = sourceStopGroupById.[groupId]
                    let sourceId = sourceIdentity prepared.binding.sourceId group.members.[0]
                    let outputPlaceId = sourceStopPlaceId sourceId groupId
                    sourceNativeStopPlaces.[outputPlaceId] <- group
                    stopMatching.stopGroupMatches.[groupId] <- outputPlaceId
                    stopMatching.stopMatchMethods.[groupId] <- "source_native"
                    for memberRow in group.members do
                        mappedPlaceBySourceStop <- mappedPlaceBySourceStop |> Map.add (rowValue memberRow "stop_id") outputPlaceId
        | _ -> ()
    let sourceNativeRoutes = Dictionary<string, CsvRow * string>(StringComparer.Ordinal)
    let authorityCandidates = ResizeArray<string * string * string array * DateSet.Dates * CallValue array * CsvRow>()
    let blockedAuthorityDates = HashSet<struct (string * int)>()
    let authorityIdentity (sourceTripRow: CsvRow) =
        match routeMatching.cisForSourceTrip sourceTripRow with
        | Some cisLineId -> Some (cisLineId, routeMatching.directBaseRoutesForSourceTrip sourceTripRow)
        | None ->
            let sourceRouteId = rowValue sourceTripRow "route_id"
            match routeMatching.routeCandidates.TryGetValue(sourceRouteId) with
            | true, (routes, "structural_trip_evidence") when routes.Length = 1 ->
                match prepared.baseCisByRoute.TryGetValue(routes.[0]) with
                | true, cisLineId -> Some (cisLineId, routes)
                | _ -> None
            | _ ->
                let sourceMode = modeClass (rowValue sourceRoutes.[sourceRouteId] "route_type")
                if sourceNativeAllowed sourceTripRow sourceMode then
                    let sourceId = sourceIdentity prepared.binding.sourceId sourceTripRow
                    Some ("source:" + sourceId + ":" + originalIdentity "route_id" sourceRoutes.[sourceRouteId], [||])
                else None
    let assertedCisBySourceRoute =
        sourceTripRows
        |> Seq.choose (fun row -> authorityIdentity row |> Option.map (fun (cis, _) -> rowValue row "route_id", cis))
        |> Seq.groupBy fst
        |> Seq.map (fun (sourceRouteId, values) -> sourceRouteId, values |> Seq.map snd |> Seq.distinct |> Seq.toArray)
        |> dict
    let blockAuthorityDates cisLineId (activeDates: DateSet.Dates) =
        for dateIndex in 0 .. activeDates.Length - 1 do
            if activeDates.[dateIndex] then blockedAuthorityDates.Add(struct (cisLineId, dateIndex)) |> ignore
    for sourceTripRow in sourceTripRows do
        let sourceTripId = rowValue sourceTripRow "trip_id"
        let sourceRouteId = rowValue sourceTripRow "route_id"
        let sourceMode = modeClass (rowValue sourceRoutes.[sourceRouteId] "route_type")
        if tripSetAuthorityAllowed sourceTripRow sourceMode then
            match authorityIdentity sourceTripRow, sourceDates.TryGetValue(rowValue sourceTripRow "service_id"), routeMatching.sourceCalls.TryGetValue(sourceTripId) with
            | Some (cisLineId, directTargetRoutes), (true, activeDates), (true, calls) when anyDate activeDates ->
                let targetRoutes =
                    if directTargetRoutes.Length = 1
                       && modeClass (rowValue prepared.baseRoutes.[directTargetRoutes.[0]] "route_type") = sourceMode then directTargetRoutes
                    elif directTargetRoutes.Length = 1
                         && compatibleModeClasses (modeClass (rowValue prepared.baseRoutes.[directTargetRoutes.[0]] "route_type")) sourceMode then
                        // JDF represents trolleybus service as road/bus. Preserve the
                        // source mode on a derived route while retaining the proven
                        // national CIS identity and replacing its baseline dates.
                        let sourceId = sourceIdentity prepared.binding.sourceId sourceTripRow
                        let outputRouteId = sourceRouteOutputId sourceId (originalIdentity "route_id" sourceRoutes.[sourceRouteId]) cisLineId
                        sourceNativeRoutes.[outputRouteId] <- sourceRoutes.[sourceRouteId], cisLineId
                        [| outputRouteId |]
                    elif sourceNativeAllowed sourceTripRow sourceMode then
                        let sourceId = sourceIdentity prepared.binding.sourceId sourceTripRow
                        let outputRouteId = sourceRouteOutputId sourceId (originalIdentity "route_id" sourceRoutes.[sourceRouteId]) cisLineId
                        sourceNativeRoutes.[outputRouteId] <- sourceRoutes.[sourceRouteId], cisLineId
                        [| outputRouteId |]
                    else [||]
                if targetRoutes.Length = 0 then
                    blockAuthorityDates cisLineId activeDates
                    addDiagnostic prepared.diagnostics "trip_set_authority_withheld" sourceTripId "The asserted CIS line has no compatible base route"
                elif calls |> Array.forall (fun call -> mappedPlaceBySourceStop.ContainsKey(call.stopId)) then
                    authorityCandidates.Add(sourceTripId, cisLineId, targetRoutes, activeDates, calls, sourceTripRow)
                else
                    blockAuthorityDates cisLineId activeDates
                    addDiagnostic prepared.diagnostics "trip_set_authority_withheld" sourceTripId "At least one source call has no resolved stop place"
            | Some (cisLineId, _), (true, activeDates), _ when anyDate activeDates ->
                blockAuthorityDates cisLineId activeDates
                addDiagnostic prepared.diagnostics "trip_set_authority_withheld" sourceTripId "The active source trip has no call pattern"
            | None, (true, activeDates), _ when anyDate activeDates ->
                match assertedCisBySourceRoute.TryGetValue(sourceRouteId) with
                | true, cisLineIds ->
                    for cisLineId in cisLineIds do blockAuthorityDates cisLineId activeDates
                | _ -> ()
                addDiagnostic prepared.diagnostics "trip_set_authority_withheld" sourceTripId "The active source trip has no unique companion CIS line assertion"
            | _ -> ()
    let authorityDatesByCis = Dictionary<string, bool array>(StringComparer.Ordinal)
    for _, cisLineId, _, activeDates, _, _ in authorityCandidates do
        let dates =
            match authorityDatesByCis.TryGetValue(cisLineId) with
            | true, values -> values
            | _ ->
                let values = emptyDates prepared.window
                authorityDatesByCis.[cisLineId] <- values
                values
        for dateIndex in 0 .. activeDates.Length - 1 do
            if activeDates.[dateIndex] then dates.[dateIndex] <- true
    for struct (cisLineId, dateIndex) in blockedAuthorityDates do
        match authorityDatesByCis.TryGetValue(cisLineId) with
        | true, dates -> dates.[dateIndex] <- false
        | _ -> ()
    let sourceTripProjections =
        authorityCandidates
        |> Seq.choose (fun (sourceTripId, cisLineId, targetRoutes, activeDates, calls, sourceTripRow) ->
            let dates = emptyDates prepared.window
            match authorityDatesByCis.TryGetValue(cisLineId) with
            | true, authoritativeDates ->
                for dateIndex in 0 .. activeDates.Length - 1 do
                    dates.[dateIndex] <- activeDates.[dateIndex] && authoritativeDates.[dateIndex]
            | _ -> ()
            if anyDate dates then
                Some {
                    sourceId = sourceIdentity prepared.binding.sourceId sourceTripRow
                    sourceIds = [| sourceIdentity prepared.binding.sourceId sourceTripRow |]
                    sourceTripReferences = [| sourceIdentity prepared.binding.sourceId sourceTripRow, originalIdentity "trip_id" sourceTripRow |]
                    sourceTripId = sourceTripId
                    sourceRouteId = rowValue sourceTripRow "route_id"
                    targetRouteId = targetRoutes.[0]
                    cisLineId = cisLineId
                    dates = DateSet.Dates.Of dates
                    sourceCalls = calls
                    sourceShapeId = optionText (rowValue sourceTripRow "shape_id")
                    sourceRow = sourceTripRow
                }
            else None)
        |> Seq.toArray
    Log.Information(
        "Regional overlay trip-set authority: candidate_trips={CandidateTrips}; projected_trips={ProjectedTrips}; authoritative_lines={AuthoritativeLines}; blocked_line_dates={BlockedLineDates}",
        authorityCandidates.Count, sourceTripProjections.Length, authorityDatesByCis.Count, blockedAuthorityDates.Count)
    let evidence: Result =
        {
            sourceId = prepared.binding.sourceId
            agencies = sourceAgencies
            stopRows = sourceStopRows
            excludedTypes = excludedTypes
            routeRows = sourceRouteRows
            routes = sourceRoutes
            tripRows = sourceTripRows
            tripsById = sourceTripsById
            revision = sourceRevision
            dates = sourceDates
            stopGroups = sourceStopGroups
            stopGroupById = sourceStopGroupById
            groupByMember = sourceGroupByMember
            stopGroupMatches = stopMatching.stopGroupMatches
            stopMatchMethods = stopMatching.stopMatchMethods
            stopMatchDistances = stopMatching.stopMatchDistances
            mappedPlaceBySourceStop = mappedPlaceBySourceStop
            stopInferenceEvidence = stopInferenceEvidence
            authoritativeGtfsStopCorrections = authoritativeGtfsStopCorrections
            nativeStopPlaces = sourceNativeStopPlaces
            nativeRoutes = sourceNativeRoutes
            authorityDatesByCis = Dictionary<string, DateSet.Dates>(authorityDatesByCis |> Seq.map (fun pair -> KeyValuePair(pair.Key, DateSet.Dates.Of pair.Value)), StringComparer.Ordinal)
            tripProjections = sourceTripProjections
        }
    evidence, {
        joinedValues = routeMatching.joinedValues
        routeCandidates = routeMatching.routeCandidates
        structuralRouteCandidates = routeMatching.structuralRouteCandidates
        baseSignatures = routeMatching.baseSignatures
        sourceCalls = routeMatching.sourceCalls
        baseTripIdsByRoute = routeMatching.baseTripIdsByRoute
        baseTripIdsByRouteAndPattern = routeMatching.baseTripIdsByRouteAndPattern
        candidateEquivalenceKey = routeMatching.candidateEquivalenceKey
    }
