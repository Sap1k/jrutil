// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.Projection

open System
open System.Collections.Generic
open System.Globalization
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
    source: SourceAnalysis.Result
    matches: TripMatching.Result
}

type Result = {
    bindingTier: MatchBinding -> string
    bindingEligible: string -> MatchBinding -> bool
    acceptedSourceStopIds: Set<string>
    sourceStops: IDictionary<string, CsvRow>
    headsignProvenance: Scratch.RowLog
    fullHeadsign: string -> int -> string -> string
    outputStopForSource: Dictionary<string, string>
    outputShapeBySource: Dictionary<string, string>
    shapes: Shapes.Store
    bindingKey: MatchBinding -> string
    selectionByBinding: Dictionary<string, OverlaySelection>
    slicesByBaseTrip: Dictionary<string, TripSlice array>
    projectionsBySource: IDictionary<string, SourceTripProjection>
    serviceDates: Dictionary<string, DateSet.Dates>
    sourceTripAdditions: SourceTripAddition array
    sourceTripAdditionsBySourceId: IDictionary<string, SourceTripAddition>
    outputParentCoordinates: Dictionary<string, ResizeArray<decimal * decimal>>
}

/// Resolve claims and construct complete trip/date slices before serialization.
let resolve ({ prepared = prepared; source = source; matches = matches }: Input) : Result =
    let bindingTier (value: MatchBinding) =
        value.method.Split('+') |> Array.last
    let ranks = prepared.policy.source.tripMatchTiers |> Array.mapi (fun index tier -> tier, index) |> dict
    let bindingEligible capabilityName (value: MatchBinding) =
        match prepared.policy.source.tripMatch.minimumCapabilityTier.TryGetValue(capabilityName) with
        | false, _ -> true
        | true, minimumTier ->
            ranks.ContainsKey(bindingTier value)
            && ranks.ContainsKey(minimumTier)
            && ranks.[bindingTier value] <= ranks.[minimumTier]
    let acceptedSourceStopIds =
        Seq.append
            (matches.bindings
             |> Seq.filter (bindingEligible "call_boarding_points")
             |> Seq.collect (fun binding ->
                 binding.sourceOrdinalByTarget
                 |> Seq.choose id
                 |> Seq.map (fun ordinal -> binding.sourceCalls.[ordinal].stopId)))
            (source.tripProjections |> Seq.collect (fun projection -> projection.sourceCalls |> Seq.map (fun call -> call.stopId)))
        |> Set.ofSeq
    let sourceStops = source.stopRows |> Array.map (fun row -> rowValue row "stop_id", row) |> dict
    let headsignProvenance = new Scratch.RowLog(prepared.scratch)
    let headsignCalls = source.tripProjections |> Array.map (fun projection -> projection.sourceTripId, projection.sourceCalls) |> dict
    let canonicalDestinationName (call: CallValue) =
        match source.authoritativeGtfsStopCorrections.TryGetValue(call.stopPlaceId) with
        | true, sourceGroup -> sourceGroup.name
        | _ ->
            match prepared.baseStopGroupById.TryGetValue(call.stopPlaceId) with
            | true, targetGroup -> targetGroup.name
            | _ ->
                match source.groupByMember.TryGetValue(call.stopId) with
                | true, sourceGroupId -> source.stopGroupById.[sourceGroupId].name
                | _ -> rowValue sourceStops.[call.stopId] "stop_name"
    let fullHeadsign sourceTripId sequence raw =
        let calls = headsignCalls.[sourceTripId]
        let downstream = calls |> Array.filter (fun call -> call.sequence >= sequence)
        // Expand a display-shortened terminal to the full canonical place name. Preserve text
        // which is genuinely different (bilingual, via, or an intermediate destination).
        let terminalCall = downstream |> Array.tryLast
        let value, status, reason, destinationStopId =
            match terminalCall with
            | None -> raw, "unresolved", "missing_terminal_stop", ""
            | Some terminalCall ->
                let terminalName = canonicalDestinationName terminalCall
                if String.IsNullOrWhiteSpace(raw) then
                    terminalName, "full_stop_name", "terminal_default", terminalCall.stopId
                else
                    match resolveFullHeadsignDetailed raw [| terminalName |] with
                    | Some _, _ -> terminalName, "full_stop_name", "terminal_stop", terminalCall.stopId
                    | None, _ -> raw, "preserved", "distinct_source_headsign", ""
        let auditDay =
            let sourceTrip = source.tripsById.[sourceTripId]
            source.dates.[rowValue sourceTrip "service_id"].[prepared.window.index.[prepared.auditDate]]
        headsignProvenance.Add([| sourceTripId; string sequence; raw; value; status; reason; destinationStopId; string auditDay |])
        if status = "unresolved" then
            addDiagnostic prepared.diagnostics "headsign_unresolved" sourceTripId $"Call {sequence}: destination identity is ambiguous or unsupported: {raw}"
        value
    let outputStopForSource = Dictionary<string, string>(StringComparer.Ordinal)
    for sourceStopId in acceptedSourceStopIds do
        match source.mappedPlaceBySourceStop |> Map.tryFind sourceStopId with
        | Some targetPlace ->
            let sourceStop = sourceStops.[sourceStopId]
            if rowValue sourceStop "location_type" = "1"
               || not (enabled prepared.policy "boarding_points" && enabled prepared.policy "call_boarding_points") then
                outputStopForSource.[sourceStopId] <- targetPlace
            else
                outputStopForSource.[sourceStopId] <- sourcePostId prepared.binding.sourceId sourceStopId
        | None -> ()

    let selectedShapeIds =
        if enabled prepared.policy "shapes" then
            Seq.append
                (matches.bindings
                 |> Seq.filter (bindingEligible "shapes")
                 |> Seq.choose (fun value -> value.sourceShapeId))
                (source.tripProjections |> Seq.choose (fun value -> value.sourceShapeId))
            |> Set.ofSeq
        else Set.empty
    logProgress "prepare-shapes" 0L None
    let shapes = Shapes.prepare prepared.scratch prepared.shapeRows selectedShapeIds prepared.diagnostics
    let outputShapeBySource = shapes.outputBySource
    logProgress "prepare-shapes" (int64 shapes.locations.Count) None

    let scheduleValues = Dictionary<string, string option>(StringComparer.Ordinal)
    let scheduleValue text =
        match scheduleValues.TryGetValue(text) with
        | true, value -> value
        | _ -> let value = Some text in scheduleValues.Add(text, value); value
    let selectionForBinding (value: MatchBinding) =
        let selectSchedules = bindingEligible "schedules" value
        let selectBoardingPoints = bindingEligible "call_boarding_points" value
        let outputStops =
            value.sourceOrdinalByTarget
            |> Array.map (fun sourceOrdinal ->
                match sourceOrdinal with
                | Some ordinal when selectBoardingPoints ->
                    let call = value.sourceCalls.[ordinal]
                    match outputStopForSource.TryGetValue(call.stopId) with
                    | true, outputStop -> outputStop
                    | _ -> call.stopPlaceId
                | _ -> "")
        let allDistances =
            value.sourceOrdinalByTarget
            |> Array.map (fun sourceOrdinal -> sourceOrdinal |> Option.bind (fun ordinal -> value.sourceCalls.[ordinal].distance))
        let monotonicCallDistances =
            let supplied = allDistances |> Array.choose id
            supplied.Length = allDistances.Length
            && supplied |> Array.pairwise |> Array.forall (fun (left, right) -> right >= left)
        let outputShape =
            match value.sourceShapeId with
            | Some shapeId when bindingEligible "shapes" value && outputShapeBySource.ContainsKey(shapeId) && monotonicCallDistances -> Some outputShapeBySource.[shapeId]
            | Some shapeId when outputShapeBySource.ContainsKey(shapeId) ->
                addDiagnostic prepared.diagnostics "shape_call_distance_invalid" value.sourceTripId "Complete monotonic stop distances are required"
                None
            | _ -> None
        let selection =
            {
                factKey = ""
                sourceId = value.sourceId
                sourceTripId = value.sourceTripId
                sourceStopIds = value.sourceCalls |> Array.map (fun call -> call.stopId)
                outputStopIds = outputStops
                sourceShapeId = value.sourceShapeId
                outputShapeId = outputShape
                distances = if outputShape.IsSome then allDistances else Array.create allDistances.Length None
                sourceOrdinalByTarget = value.sourceOrdinalByTarget
                arrivals =
                    value.sourceOrdinalByTarget
                    |> Array.map (fun sourceOrdinal ->
                        if selectSchedules then sourceOrdinal |> Option.bind (fun ordinal -> scheduleValue value.sourceCalls.[ordinal].arrival)
                        else None)
                departures =
                    value.sourceOrdinalByTarget
                    |> Array.map (fun sourceOrdinal ->
                        if selectSchedules then sourceOrdinal |> Option.bind (fun ordinal -> scheduleValue value.sourceCalls.[ordinal].departure)
                        else None)
            }
        { selection with factKey = sha256Text (selectionEncoding selection) }

    let bindingKey (value: MatchBinding) =
        value.sourceTripId + "\u001f" + value.targetTripId + "\u001f" + value.method + "\u001f" + dateKey value.dates
    let selectionByBinding = Dictionary<string, OverlaySelection>(StringComparer.Ordinal)
    for value in matches.bindings do selectionByBinding.[bindingKey value] <- selectionForBinding value

    let representedDatesBySourceTrip = Dictionary<string, bool array>(StringComparer.Ordinal)
    let authoritativeClaims = Dictionary<string, System.Collections.BitArray>(StringComparer.Ordinal)
    let projectionsBySource = source.tripProjections |> Array.map (fun projection -> projection.sourceTripId, projection) |> dict
    let bindingsByTarget = matches.bindings |> Seq.groupBy (fun binding -> binding.targetTripId) |> dict
    let resolveTarget targetTripId =
        let bindings = match bindingsByTarget.TryGetValue(targetTripId) with | true, values -> values | _ -> Seq.empty
        let byDate = Dictionary<int, ResizeArray<OverlaySelection>>()
        for binding in bindings do
            let selection = selectionByBinding.[bindingKey binding]
            for index in 0 .. binding.dates.Length - 1 do
                if binding.dates.[index] then
                    match byDate.TryGetValue(index) with
                    | true, values -> values.Add(selection)
                    | _ -> byDate.[index] <- ResizeArray([| selection |])
        let resolved = Dictionary<int, OverlaySelection>()
        for KeyValue(dateIndex, selections) in byDate do
            let unique = selections |> Seq.distinctBy (Some >> selectionKey) |> Seq.toArray
            if unique.Length = 1 then resolved.[dateIndex] <- unique.[0]
            else
                let newestRevision = unique |> Array.map (fun value -> source.revision value.sourceTripId) |> Array.max
                let newest = unique |> Array.filter (fun value -> source.revision value.sourceTripId = newestRevision)
                if newest.Length = 1 then
                    resolved.[dateIndex] <- newest.[0]
                    let superseded =
                        unique
                        |> Array.filter (fun value -> value.sourceTripId <> newest.[0].sourceTripId)
                        |> Array.map (fun value -> value.sourceTripId)
                        |> Array.distinct
                        |> String.concat ";"
                    addDiagnostic prepared.diagnostics "overlay_newer_source_selected" targetTripId $"{dateString prepared.window.dates.[dateIndex]} selected {newest.[0].sourceTripId} over older claims {superseded}"
                else
                    addDiagnostic prepared.diagnostics "overlay_fact_conflict" targetTripId $"Conflicting same-revision source facts on {prepared.window.dates.[dateIndex]}"

        // Partial evidence cannot represent a complete authoritative source instance.
        for binding in bindings do
            match projectionsBySource.TryGetValue(binding.sourceTripId) with
            | true, projection ->
                let complete =
                    binding.sourceOrdinalByTarget = identityAlignment projection.sourceCalls.Length
                    && bindingEligible "schedules" binding && bindingEligible "call_boarding_points" binding
                if not complete then
                    for dateIndex in 0 .. projection.dates.Length - 1 do
                        match resolved.TryGetValue(dateIndex) with
                        | true, selected when projection.dates.[dateIndex] && selected.sourceTripId = projection.sourceTripId ->
                            resolved.Remove(dateIndex) |> ignore
                        | _ -> ()
            | _ -> ()
        for KeyValue(dateIndex, selected) in resolved |> Seq.toArray do
            match projectionsBySource.TryGetValue(selected.sourceTripId) with
            | true, projection when projection.dates.[dateIndex] ->
                let claimed =
                    match authoritativeClaims.TryGetValue(selected.sourceTripId) with
                    | true, dates -> dates
                    | _ ->
                        let dates = System.Collections.BitArray(prepared.window.dates.Length)
                        authoritativeClaims.[selected.sourceTripId] <- dates
                        dates
                if claimed.[dateIndex] then
                    resolved.Remove(dateIndex) |> ignore
                    addDiagnostic prepared.diagnostics "equivalent_baseline_instance_removed" targetTripId $"One {prepared.binding.sourceId} instance on {dateString prepared.window.dates.[dateIndex]} must not produce multiple baseline variants"
                else claimed.[dateIndex] <- true
            | _ -> ()
        for binding in bindings do
            let fingerprint = selectionKey (Some selectionByBinding.[bindingKey binding])
            for dateIndex in 0 .. binding.dates.Length - 1 do
                match resolved.TryGetValue(dateIndex) with
                | true, accepted when binding.dates.[dateIndex] && selectionKey (Some accepted) = fingerprint ->
                    let dates =
                        match representedDatesBySourceTrip.TryGetValue(binding.sourceTripId) with
                        | true, values -> values
                        | _ ->
                            let values = emptyDates prepared.window
                            representedDatesBySourceTrip.[binding.sourceTripId] <- values
                            values
                    dates.[dateIndex] <- true
                | _ -> ()
        resolved

    let authorityByRoute =
        prepared.baseCisByRoute
        |> Seq.choose (fun pair ->
            match source.authorityDatesByCis.TryGetValue(pair.Value) with
            | true, dates -> Some (pair.Key, dates)
            | _ -> None)
        |> dict

    let slicesByBaseTrip = Dictionary<string, TripSlice array>(StringComparer.Ordinal)
    let serviceDates = Dictionary<string, DateSet.Dates>(StringComparer.Ordinal)
    let serviceIds = Dictionary<string, string>(StringComparer.Ordinal)
    let outputService (dates: seq<bool>) =
        let packed = match dates with | :? DateSet.Dates as packed -> packed | _ -> DateSet.Dates.Of dates
        match serviceIds.TryGetValue(packed.Key) with
        | true, id -> id
        | _ ->
            let id = serviceId packed
            serviceIds.Add(packed.Key, id)
            serviceDates.Add(id, packed)
            id
    let mutable slicedTripCount = 0L
    for baseTrip in prepared.baseTripValues |> Array.sortBy (fun trip -> trip.id) do
        let baseTripId = baseTrip.id
        let sourceServiceId = baseTrip.serviceId
        match prepared.baseDates.TryGetValue(sourceServiceId) with
        | true, activeDates when anyDate activeDates ->
            let selections = resolveTarget baseTripId
            let authority = match authorityByRoute.TryGetValue(baseTrip.routeId) with | true, dates -> Some dates | _ -> None
            let retainedDates =
                match authority with
                | None -> activeDates
                | Some authorityDates ->
                    activeDates
                    |> Seq.mapi (fun index active -> active && (not authorityDates.[index] || selections.ContainsKey(index)))
                    |> DateSet.Dates.Of
            let slices =
                if not (anyDate retainedDates) then [||]
                elif selections.Count = 0 then
                    let outputServiceId = outputService retainedDates
                    [| {
                        trip = { baseTrip with serviceId = outputServiceId; shapeId = None }
                        dates = serviceDates.[outputServiceId]
                        selection = None
                    } |]
                else
                    let grouped = Dictionary<string, ResizeArray<int> * OverlaySelection option>(StringComparer.Ordinal)
                    for index in 0 .. retainedDates.Length - 1 do
                        if retainedDates.[index] then
                            let selection = match selections.TryGetValue(index) with | true, value -> Some value | _ -> None
                            let fingerprint = selectionKey selection
                            match grouped.TryGetValue(fingerprint) with
                            | true, (indices, _) -> indices.Add(index)
                            | _ ->
                                let indices = ResizeArray<int>()
                                indices.Add(index)
                                grouped.[fingerprint] <- indices, selection
                    let split = grouped.Count > 1
                    grouped
                    |> Seq.map (fun (KeyValue(fingerprint, (indices, selection))) ->
                        let dates = emptyDates prepared.window
                        for index in indices do dates.[index] <- true
                        let outputTripId =
                            if split then splitTripId baseTripId dates (selection |> Option.map selectionEncoding |> Option.defaultValue "base")
                            else baseTripId
                        let outputServiceId = outputService dates
                        {
                            trip = {
                                baseTrip with
                                    id = outputTripId
                                    routeId =
                                        match selection with
                                        | Some selected when projectionsBySource.ContainsKey(selected.sourceTripId) ->
                                            let projection = projectionsBySource.[selected.sourceTripId]
                                            let sourceMode = modeClass (rowValue source.routes.[projection.sourceRouteId] "route_type")
                                            if modeClass (rowValue prepared.baseRoutes.[baseTrip.routeId] "route_type") <> sourceMode then projection.targetRouteId
                                            else baseTrip.routeId
                                        | _ -> baseTrip.routeId
                                    serviceId = outputServiceId
                                    shapeId = selection |> Option.bind (fun value -> value.outputShapeId)
                                    headsign =
                                        match selection with
                                        | Some selected when projectionsBySource.ContainsKey(selected.sourceTripId) ->
                                            let projection = projectionsBySource.[selected.sourceTripId]
                                            let sourceTrip = tripRow projection.sourceRow
                                            Some (fullHeadsign selected.sourceTripId -1 (sourceTrip.headsign |> Option.defaultValue ""))
                                        | _ -> baseTrip.headsign
                            }
                            dates = serviceDates.[outputServiceId]
                            selection = selection
                        })
                    |> Seq.sortBy (fun slice -> slice.trip.id)
                    |> Seq.toArray
            slicesByBaseTrip.[baseTripId] <- slices
        | _ -> ()
        slicedTripCount <- slicedTripCount + 1L
        if slicedTripCount % 100000L = 0L then logProgress "slice-base-trips" slicedTripCount (Some (int64 prepared.baseTripValues.Length))
    logProgress "slice-base-trips" slicedTripCount (Some (int64 prepared.baseTripValues.Length))
    let pendingSourceTripAdditions =
        source.tripProjections
        |> Array.choose (fun projection ->
            let dates = Seq.toArray projection.dates
            match representedDatesBySourceTrip.TryGetValue(projection.sourceTripId) with
            | true, represented ->
                for dateIndex in 0 .. dates.Length - 1 do
                    if represented.[dateIndex] then dates.[dateIndex] <- false
            | _ -> ()
            if anyDate dates then Some { projection with dates = DateSet.Dates.Of dates } else None)
    let sourceTripAdditions =
        pendingSourceTripAdditions
        |> Array.map (fun projection ->
            let outputServiceId = outputService projection.dates
            let sourceTrip = tripRow projection.sourceRow
            let outputShapeId =
                match projection.sourceShapeId with
                | Some shapeId when outputShapeBySource.ContainsKey(shapeId) -> Some outputShapeBySource.[shapeId]
                | _ -> None
            {
                projection = projection
                trip = {
                    sourceTrip with
                        headsign = Some (fullHeadsign projection.sourceTripId -1 (sourceTrip.headsign |> Option.defaultValue ""))
                        id = addedSourceTripId prepared.binding.sourceId projection.sourceTripId projection.dates
                        routeId = projection.targetRouteId
                        serviceId = outputServiceId
                        shapeId = outputShapeId
                }
            })
    let sourceTripAdditionsBySourceId = sourceTripAdditions |> Array.map (fun value -> value.projection.sourceTripId, value) |> dict
    prepared.baseDates.Clear()
    logProgress "release-slicing-indexes" 1L (Some 1L)

    let outputParentCoordinates = Dictionary<string, ResizeArray<decimal * decimal>>(StringComparer.Ordinal)
    let validCoordinate latitude longitude =
        latitude >= -90M && latitude <= 90M && longitude >= -180M && longitude <= 180M
    if enabled prepared.policy "stop_coordinates" then
        for sourceGroup in source.stopGroups do
            let contextInferred =
                match source.stopMatchMethods.TryGetValue(sourceGroup.groupId) with
                | true, methodName -> methodName.StartsWith("trip_context_", StringComparison.Ordinal)
                | _ -> false
            if contextInferred || sourceGroup.members |> Array.exists (fun row -> acceptedSourceStopIds.Contains(rowValue row "stop_id")) then
                match source.stopGroupMatches.TryGetValue(sourceGroup.groupId), sourceGroup.lat, sourceGroup.lon with
                | (true, target), Some lat, Some lon when validCoordinate lat lon ->
                    match outputParentCoordinates.TryGetValue(target) with
                    | true, values -> values.Add((lat, lon))
                    | _ ->
                        let values = ResizeArray<decimal * decimal>()
                        values.Add((lat, lon))
                        outputParentCoordinates.[target] <- values
                | (true, target), Some lat, Some lon ->
                    addDiagnostic prepared.diagnostics "stop_coordinate_invalid" sourceGroup.groupId $"Refusing invalid GTFS centroid {lat},{lon} for {target}"
                | _ -> ()
    {
        bindingTier = bindingTier
        bindingEligible = bindingEligible
        acceptedSourceStopIds = acceptedSourceStopIds
        sourceStops = sourceStops
        headsignProvenance = headsignProvenance
        fullHeadsign = fullHeadsign
        outputStopForSource = outputStopForSource
        outputShapeBySource = outputShapeBySource
        shapes = shapes
        bindingKey = bindingKey
        selectionByBinding = selectionByBinding
        slicesByBaseTrip = slicesByBaseTrip
        projectionsBySource = projectionsBySource
        serviceDates = serviceDates
        sourceTripAdditions = sourceTripAdditions
        sourceTripAdditionsBySourceId = sourceTripAdditionsBySourceId
        outputParentCoordinates = outputParentCoordinates
    }
