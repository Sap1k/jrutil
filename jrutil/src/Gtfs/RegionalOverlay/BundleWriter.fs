// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BundleWriter

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open NodaTime

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay

type Input = {
    prepared: InputPreparation.Result
    projection: Projection.Result
    source: SourceAnalysis.Result
    matches: TripMatching.Result
    gvdYear: int
    diagnosticsOutput: string option
    diagnosticTraces: bool
}

/// Stream the resolved bundle, write reports and atomically activate the output.
let write ({
    prepared = prepared
    projection = projection
    source = source
    matches = matches
    gvdYear = gvdYear
    diagnosticsOutput = diagnosticsOutput
    diagnosticTraces = diagnosticTraces
}: Input) =
    let sibling = Path.GetDirectoryName(prepared.outputBundle)
    let temporary = Path.Combine(sibling, "." + Path.GetFileName(prepared.outputBundle) + ".tmp-" + Guid.NewGuid().ToString("N"))
    let gtfsOutput = Path.Combine(temporary, "gtfs-intermediate")
    let extensionsOutput = Path.Combine(temporary, "extensions")
    let usedStopIds = HashSet<string>(StringComparer.Ordinal)
    let usedRouteIds = HashSet<string>(StringComparer.Ordinal)
    let usedAgencyIds = HashSet<string>(StringComparer.Ordinal)
    let mutable writtenCallCount = 0L
    try
        Directory.CreateDirectory(gtfsOutput) |> ignore
        Directory.CreateDirectory(extensionsOutput) |> ignore
        let selectedFieldsPath = Path.Combine(temporary, "provenance", "selected_fields.csv")
        Directory.CreateDirectory(Path.GetDirectoryName(selectedFieldsPath)) |> ignore
        use selectedFieldsWriter = new StreamWriter(selectedFieldsPath, false, new UTF8Encoding(false))
        selectedFieldsWriter.NewLine <- "\n"
        selectedFieldsWriter.WriteLine(
            [| "output_object_id"; "field"; "source_id"; "value"; "capability_mode" |]
            |> Array.map csvEscape
            |> String.concat ",")
        let mutable selectedFieldCount = 0L
        let writeSelectedField (values: string array) =
            writeCsvRow selectedFieldsWriter values
            selectedFieldCount <- selectedFieldCount + 1L
            if selectedFieldCount % 1000000L = 0L then
                logProgress "write-selected-field-provenance" selectedFieldCount None
        logProgress "write-output-calls" 0L None
        let stopTimeColumns = columnsOf prepared.baseGtfs "stop_times.txt"
        let stopTimeIndexes = stopTimeColumns |> Array.mapi (fun index column -> column, index) |> dict
        let getCallField (row: string array) column =
            match stopTimeIndexes.TryGetValue(column) with | true, index -> row.[index] | _ -> ""
        let setCallField (row: string array) column value =
            match stopTimeIndexes.TryGetValue(column) with | true, index -> row.[index] <- value | _ -> ()
        let stopTimeOutput = Path.Combine(gtfsOutput, "stop_times.txt")
        use stopTimeWriter = new StreamWriter(stopTimeOutput, false, new UTF8Encoding(false))
        stopTimeWriter.NewLine <- "\n"
        writeCsvRow stopTimeWriter stopTimeColumns
        let retainedCall ordinal copy (row: string array) (slice: TripSlice) =
            let outputRow = if copy then Array.copy row else row
            setCallField outputRow "trip_id" <| slice.trip.id
            match slice.selection with
            | Some selection when ordinal < selection.outputStopIds.Length ->
                if not (String.IsNullOrEmpty(selection.outputStopIds.[ordinal])) then
                    setCallField outputRow "stop_id" <| selection.outputStopIds.[ordinal]
                selection.distances.[ordinal]
                    |> Option.map (fun value -> value.ToString("G29", CultureInfo.InvariantCulture))
                    |> Option.defaultValue ""
                    |> setCallField outputRow "shape_dist_traveled"
                selection.arrivals.[ordinal] |> Option.iter (fun value -> setCallField outputRow "arrival_time" <| value)
                selection.departures.[ordinal] |> Option.iter (fun value -> setCallField outputRow "departure_time" <| value)
                match selection.sourceOrdinalByTarget.[ordinal] with
                | Some sourceOrdinal when projection.projectionsBySource.ContainsKey(selection.sourceTripId) ->
                    let sourceCall = projection.projectionsBySource.[selection.sourceTripId].sourceCalls.[sourceOrdinal]
                    sourceCall.stopHeadsign
                    |> Option.iter (fun raw -> setCallField outputRow "stop_headsign" <| projection.fullHeadsign selection.sourceTripId sourceCall.sequence raw)
                | Some _ -> ()
                | None -> ()
            | _ -> ()
            outputRow
        let addedCall sourceTripId (sourceRow: string array) (addition: SourceTripAddition) =
            let outputRow = sourceRow
            setCallField outputRow "trip_id" <| addition.trip.id
            let rawHeadsign = getCallField sourceRow "stop_headsign"
            if not (String.IsNullOrWhiteSpace(rawHeadsign)) then
                setCallField outputRow "stop_headsign" <| projection.fullHeadsign sourceTripId (Int32.Parse(getCallField sourceRow "stop_sequence", CultureInfo.InvariantCulture)) rawHeadsign
            let sourceStopId = getCallField sourceRow "stop_id"
            setCallField outputRow "stop_id" <| projection.outputStopForSource.[sourceStopId]
            if addition.trip.shapeId.IsNone then setCallField outputRow "shape_dist_traveled" <| ""
            outputRow
        let mutable previousTrip = ""
        let mutable ordinal = 0
        let mutable outputBaseCallsRead = 0L
        let outputCalls = seq {
            for row in csvValues prepared.baseGtfs "stop_times.txt" stopTimeColumns do
                let baseTripId = getCallField row "trip_id"
                if baseTripId <> previousTrip then
                    previousTrip <- baseTripId
                    ordinal <- 0
                match projection.slicesByBaseTrip.TryGetValue(baseTripId) with
                | true, slices ->
                    for slice in slices do
                        let outputRow = retainedCall ordinal (slices.Length > 1) row slice
                        usedStopIds.Add(getCallField outputRow "stop_id") |> ignore
                        writtenCallCount <- writtenCallCount + 1L
                        yield outputRow
                | _ -> ()
                ordinal <- ordinal + 1
                outputBaseCallsRead <- outputBaseCallsRead + 1L
                if outputBaseCallsRead % 1000000L = 0L then logProgress "write-output-calls" outputBaseCallsRead None
            for sourceRow in csvValues prepared.binding.payloadPath "stop_times.txt" stopTimeColumns do
                let sourceTripId = getCallField sourceRow "trip_id"
                match projection.sourceTripAdditionsBySourceId.TryGetValue(sourceTripId) with
                | true, addition when sourceTripId = addition.projection.sourceTripId ->
                    let outputRow = addedCall sourceTripId sourceRow addition
                    usedStopIds.Add(getCallField outputRow "stop_id") |> ignore
                    writtenCallCount <- writtenCallCount + 1L
                    yield outputRow
                | _ -> ()
        }
        // Validity slices interleave output trip IDs. Normalize native output
        // once, before serialization, so every downstream call reader can stream.
        let compareCalls left right =
            let trip = StringComparer.Ordinal.Compare(getCallField left "trip_id", getCallField right "trip_id")
            if trip <> 0 then trip
            else compare (Int32.Parse(getCallField left "stop_sequence", CultureInfo.InvariantCulture))
                         (Int32.Parse(getCallField right "stop_sequence", CultureInfo.InvariantCulture))
        do
            use scratch = new Scratch.Storage(sibling)
            logProgress "sort-output-calls" 0L None
            let ordered = Scratch.sortRows scratch compareCalls Scratch.defaultBufferBytes outputCalls
            let mutable written = 0L
            for row in ordered do
                writeCsvRow stopTimeWriter row
                written <- written + 1L
                if written % 1000000L = 0L then logProgress "write-sorted-output-calls" written (Some writtenCallCount)
            logProgress "write-sorted-output-calls" written (Some writtenCallCount)
        stopTimeWriter.Flush()
        stopTimeWriter.Dispose()
        logProgress "write-output-calls" outputBaseCallsRead (Some outputBaseCallsRead)

        let mutable outputTripCount = 0
        let outputTripRows = seq {
            for baseTrip in prepared.baseTripValues do
                match projection.slicesByBaseTrip.TryGetValue(baseTrip.id) with
                | true, slices ->
                    for slice in slices do
                        usedRouteIds.Add(slice.trip.routeId) |> ignore
                        outputTripCount <- outputTripCount + 1
                        yield tripToRow slice.trip
                | _ -> ()
            for addition in projection.sourceTripAdditions |> Array.sortBy (fun value -> value.trip.id) do
                usedRouteIds.Add(addition.trip.routeId) |> ignore
                outputTripCount <- outputTripCount + 1
                yield tripToRow addition.trip
        }
        writeRows (Path.Combine(gtfsOutput, "trips.txt")) (columnsOf prepared.baseGtfs "trips.txt") outputTripRows
        logProgress "write-trips" (int64 outputTripCount) (Some (int64 outputTripCount))

        let acceptedRouteSources = Dictionary<string, ResizeArray<CsvRow>>(StringComparer.Ordinal)
        for matchBinding in matches.bindings do
            let dateStillAccepted =
                match projection.slicesByBaseTrip.TryGetValue(matchBinding.targetTripId) with
                | true, slices -> slices |> Array.exists (fun slice -> slice.selection.IsSome && datesOverlap slice.dates matchBinding.dates)
                | _ -> false
            if dateStillAccepted then
                let targetRouteId = prepared.baseTrips.[matchBinding.targetTripId].routeId
                let sourceRoute = source.routes.[rowValue source.tripsById.[matchBinding.sourceTripId] "route_id"]
                match acceptedRouteSources.TryGetValue(targetRouteId) with
                | true, values -> values.Add(sourceRoute)
                | _ ->
                    let values = ResizeArray<CsvRow>()
                    values.Add(sourceRoute)
                    acceptedRouteSources.[targetRouteId] <- values
        for addition in projection.sourceTripAdditions do
            let sourceRoute = source.routes.[addition.projection.sourceRouteId]
            match acceptedRouteSources.TryGetValue(addition.trip.routeId) with
            | true, values -> values.Add(sourceRoute)
            | _ ->
                let values = ResizeArray<CsvRow>()
                values.Add(sourceRoute)
                acceptedRouteSources.[addition.trip.routeId] <- values
        let displayFields = [|
            "route_short_name", "route_short_name"
            "route_long_name", "route_long_name"
            "route_color", "route_color"
            "route_text_color", "route_text_color"
        |]
        for baseTrip in prepared.baseTripValues do
            match projection.slicesByBaseTrip.TryGetValue(baseTrip.id) with
            | true, slices ->
                for slice in slices do
                    match slice.selection with
                    | Some selection ->
                        let provenanceSources = String.concat ";" selection.sourceIds
                        selection.outputShapeId
                        |> Option.iter (fun value ->
                            writeSelectedField [| slice.trip.id; "shape_id"; provenanceSources; value; (capability prepared.policy "shapes").mode |])
                        for index in 0 .. selection.outputStopIds.Length - 1 do
                            if not (String.IsNullOrEmpty(selection.outputStopIds.[index])) then
                                writeSelectedField [|
                                    slice.trip.id + "#" + string (index + 1); "stop_id"; provenanceSources
                                    selection.outputStopIds.[index]; (capability prepared.policy "call_boarding_points").mode
                                |]
                            selection.distances.[index]
                            |> Option.iter (fun value ->
                                writeSelectedField [|
                                    slice.trip.id + "#" + string (index + 1); "shape_dist_traveled"; provenanceSources
                                    value.ToString("G29", CultureInfo.InvariantCulture); (capability prepared.policy "shapes").mode
                                |])
                            selection.arrivals.[index]
                            |> Option.iter (fun value ->
                                writeSelectedField [|
                                    slice.trip.id + "#" + string (index + 1); "arrival_time"; provenanceSources
                                    value; (capability prepared.policy "schedules").mode
                                |])
                            selection.departures.[index]
                            |> Option.iter (fun value ->
                                writeSelectedField [|
                                    slice.trip.id + "#" + string (index + 1); "departure_time"; provenanceSources
                                    value; (capability prepared.policy "schedules").mode
                                |])
                    | None -> ()
            | _ -> ()
        for addition in projection.sourceTripAdditions do
            let additionSource = addition.projection.sourceId
            addition.trip.shapeId
            |> Option.iter (fun value ->
                writeSelectedField [| addition.trip.id; "shape_id"; additionSource; value; (capability prepared.policy "shapes").mode |])
            for index in 0 .. addition.projection.sourceCalls.Length - 1 do
                let call = addition.projection.sourceCalls.[index]
                let outputStopId = projection.outputStopForSource.[call.stopId]
                writeSelectedField [|
                    addition.trip.id + "#" + string (index + 1); "stop_id"; additionSource
                    outputStopId; (capability prepared.policy "call_boarding_points").mode
                |]
                if addition.trip.shapeId.IsSome then
                    call.distance
                    |> Option.iter (fun value ->
                        writeSelectedField [|
                            addition.trip.id + "#" + string (index + 1); "shape_dist_traveled"; additionSource
                            value.ToString("G29", CultureInfo.InvariantCulture); (capability prepared.policy "shapes").mode
                        |])
                writeSelectedField [|
                    addition.trip.id + "#" + string (index + 1); "arrival_time"; additionSource
                    call.arrival; (capability prepared.policy "schedules").mode
                |]
                writeSelectedField [|
                    addition.trip.id + "#" + string (index + 1); "departure_time"; additionSource
                    call.departure; (capability prepared.policy "schedules").mode
                |]
        let routeColumns = columnsOf prepared.baseGtfs "routes.txt"
        let agencyColumns = columnsOf prepared.baseGtfs "agency.txt"
        let sourceNativeAgencyRows = Dictionary<string, CsvRow>(StringComparer.Ordinal)
        let outputRouteRows =
            seq {
                for baseRow in prepared.baseRouteRows do
                    if usedRouteIds.Contains(rowValue baseRow "route_id") then
                        let row = cloneRow baseRow
                        let routeId = rowValue row "route_id"
                        optionText (rowValue row "agency_id") |> Option.iter (fun value -> usedAgencyIds.Add(value) |> ignore)
                        match acceptedRouteSources.TryGetValue(routeId) with
                        | true, sourceRows ->
                            for capabilityName, column in displayFields do
                                if enabled prepared.policy capabilityName then
                                    let values = sourceRows |> Seq.map (fun source -> rowValue source column) |> Seq.filter (String.IsNullOrWhiteSpace >> not) |> Seq.distinct |> Seq.toArray
                                    if values.Length = 1 then
                                        row.[column] <- values.[0]
                                        let sources = sourceRows |> Seq.filter (fun source -> rowValue source column = values.[0]) |> Seq.map (sourceIdentity prepared.binding.sourceId) |> Seq.distinct |> Seq.sort |> String.concat ";"
                                        writeSelectedField [| routeId; column; sources; values.[0]; (capability prepared.policy capabilityName).mode |]
                                    elif values.Length > 1 then
                                        addDiagnostic prepared.diagnostics "route_display_conflict" routeId $"Conflicting {column} values"
                        | _ -> ()
                        yield row
                for KeyValue(outputRouteId, (sourceRoute, _)) in source.nativeRoutes do
                    if usedRouteIds.Contains(outputRouteId) then
                        let row = CsvRow(StringComparer.Ordinal)
                        for column in routeColumns do
                            row.[column] <- if sourceRoute.ContainsKey(column) then rowValue sourceRoute column else ""
                        row.["route_id"] <- outputRouteId
                        let sourceAgencyId = rowValue sourceRoute "agency_id"
                        if not (source.agencies.ContainsKey(sourceAgencyId)) then
                            let missingAgencyRouteId = rowValue sourceRoute "route_id"
                            invalidOp $"Source-native route {missingAgencyRouteId} refers to missing agency {sourceAgencyId}"
                        let sourceId = sourceIdentity prepared.binding.sourceId sourceRoute
                        let outputAgencyId = sourceAgencyOutputId sourceId (originalIdentity "agency_id" sourceRoute)
                        row.["agency_id"] <- outputAgencyId
                        usedAgencyIds.Add(outputAgencyId) |> ignore
                        if not (sourceNativeAgencyRows.ContainsKey(outputAgencyId)) then
                            let sourceAgency = source.agencies.[sourceAgencyId]
                            let agencyRow = CsvRow(StringComparer.Ordinal)
                            for column in agencyColumns do
                                agencyRow.[column] <- if sourceAgency.ContainsKey(column) then rowValue sourceAgency column else ""
                            agencyRow.["agency_id"] <- outputAgencyId
                            sourceNativeAgencyRows.[outputAgencyId] <- agencyRow
                        yield row
            }
            |> Seq.sortBy (fun row -> rowValue row "route_id")
            |> Seq.toArray
        writeRows (Path.Combine(gtfsOutput, "routes.txt")) routeColumns outputRouteRows

        let baseStopRowsById = prepared.baseStopRows |> Array.map (fun row -> rowValue row "stop_id", row) |> dict
        let sourceStopByOutput = projection.outputStopForSource |> Seq.map (fun pair -> pair.Value, pair.Key) |> dict
        let sourcesForPlace targetPlace =
            source.stopGroups
            |> Seq.choose (fun group ->
                match source.stopGroupMatches.TryGetValue(group.groupId) with
                | true, mapped when mapped = targetPlace -> Some (sourceIdentity prepared.binding.sourceId group.members.[0])
                | _ -> None)
            |> Seq.distinct
            |> Seq.sort
            |> String.concat ";"
        let regionalZonesByPlace = Dictionary<string, string array>(StringComparer.Ordinal)
        if enabled prepared.policy "stop_zones" then
            for group in source.stopGroups do
                if sourceIdentity prepared.binding.sourceId group.members.[0] = "ids-jmk-gtfs" then
                    let targetPlace =
                        match source.stopGroupMatches.TryGetValue(group.groupId) with
                        | true, value -> Some value
                        | _ -> source.mappedPlaceBySourceStop |> Map.tryFind (rowValue group.members.[0] "stop_id")
                    match targetPlace with
                    | Some targetPlace ->
                        let zones = group.members |> Array.map (fun row -> rowValue row "zone_id") |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Array.distinct |> Array.sort
                        if zones.Length > 0 then regionalZonesByPlace.[targetPlace] <- zones
                    | _ -> ()
        let outputZoneIdentity code = "overlay:ids-jmk-gtfs:zone:" + (sha256Text code).Substring(0, 16)
        let isUsedPlace targetPlace =
            usedStopIds.Contains(targetPlace)
            || (projection.outputStopForSource
                |> Seq.exists (fun pair ->
                    usedStopIds.Contains(pair.Value)
                    && (source.mappedPlaceBySourceStop |> Map.tryFind pair.Key) = Some targetPlace))
        let setUniqueZone targetPlace (row: CsvRow) =
            let existing = optionText (rowValue row "zone_id") |> Option.toArray
            let regional = match regionalZonesByPlace.TryGetValue(targetPlace) with | true, values -> values |> Array.map outputZoneIdentity | _ -> [||]
            let effective = Array.append existing regional |> Array.distinct
            row.["zone_id"] <- if effective.Length = 1 then effective.[0] else ""
        let parentIds =
            usedStopIds
            |> Seq.choose (fun stopId ->
                if baseStopRowsById.ContainsKey(stopId) then optionText (rowValue baseStopRowsById.[stopId] "parent_station")
                elif sourceStopByOutput.ContainsKey(stopId) then
                    source.mappedPlaceBySourceStop |> Map.tryFind sourceStopByOutput.[stopId]
                else None)
            |> Set.ofSeq
        for parentId in parentIds do usedStopIds.Add(parentId) |> ignore
        let stopColumns = columnsOf prepared.baseGtfs "stops.txt"
        let sourceNativeParentRows = ResizeArray<CsvRow>()
        for KeyValue(outputPlaceId, group) in source.nativeStopPlaces do
            if usedStopIds.Contains(outputPlaceId) then
                let row = CsvRow(StringComparer.Ordinal)
                for column in stopColumns do row.[column] <- ""
                row.["stop_id"] <- outputPlaceId
                row.["stop_name"] <- group.name
                group.lat |> Option.iter (fun value -> row.["stop_lat"] <- value.ToString("G29", CultureInfo.InvariantCulture))
                group.lon |> Option.iter (fun value -> row.["stop_lon"] <- value.ToString("G29", CultureInfo.InvariantCulture))
                row.["location_type"] <- "1"
                setUniqueZone outputPlaceId row
                sourceNativeParentRows.Add(row)
        let newPostRows = ResizeArray<CsvRow>()
        for sourceStopId in projection.acceptedSourceStopIds |> Set.toArray |> Array.sort do
            match projection.outputStopForSource.TryGetValue(sourceStopId) with
            | true, outputStopId when outputStopId <> (source.mappedPlaceBySourceStop |> Map.find sourceStopId) && usedStopIds.Contains(outputStopId) ->
                let sourceRow = projection.sourceStops.[sourceStopId]
                let targetParent = source.mappedPlaceBySourceStop |> Map.find sourceStopId
                let parentName =
                    if source.authoritativeGtfsStopCorrections.ContainsKey(targetParent) then source.authoritativeGtfsStopCorrections.[targetParent].name
                    elif baseStopRowsById.ContainsKey(targetParent) then rowValue baseStopRowsById.[targetParent] "stop_name"
                    elif source.nativeStopPlaces.ContainsKey(targetParent) then source.nativeStopPlaces.[targetParent].name
                    else rowValue sourceRow "stop_name"
                let row = CsvRow(StringComparer.Ordinal)
                for column in stopColumns do row.[column] <- ""
                row.["stop_id"] <- outputStopId
                row.["stop_name"] <- parentName
                row.["stop_lat"] <- if enabled prepared.policy "boarding_points" then rowValue sourceRow "stop_lat" else ""
                row.["stop_lon"] <- if enabled prepared.policy "boarding_points" then rowValue sourceRow "stop_lon" else ""
                row.["location_type"] <- "0"
                row.["parent_station"] <- targetParent
                row.["wheelchair_boarding"] <- rowValue sourceRow "wheelchair_boarding"
                row.["platform_code"] <- rowValue sourceRow "platform_code"
                setUniqueZone targetParent row
                newPostRows.Add(row)
            | _ -> ()
        let outputStopRows =
            seq {
                for baseRow in prepared.baseStopRows do
                    let stopId = rowValue baseRow "stop_id"
                    if usedStopIds.Contains(stopId) then
                        let row = cloneRow baseRow
                        setUniqueZone stopId row
                        if source.authoritativeGtfsStopCorrections.ContainsKey(stopId) then
                            row.["stop_name"] <- source.authoritativeGtfsStopCorrections.[stopId].name
                            writeSelectedField [| stopId; "stop_name"; sourcesForPlace stopId; row.["stop_name"]; "authoritative_approximate_correction" |]
                        match projection.outputParentCoordinates.TryGetValue(stopId) with
                        | true, coordinates ->
                            let unique = coordinates |> Seq.distinct |> Seq.toArray
                            if unique.Length = 1 then
                                let lat, lon = unique.[0]
                                row.["stop_lat"] <- lat.ToString("G29", CultureInfo.InvariantCulture)
                                row.["stop_lon"] <- lon.ToString("G29", CultureInfo.InvariantCulture)
                                writeSelectedField [| stopId; "stop_lat,stop_lon"; sourcesForPlace stopId; row.["stop_lat"] + "," + row.["stop_lon"]; (capability prepared.policy "stop_coordinates").mode |]
                            elif unique.Length > 1 then addDiagnostic prepared.diagnostics "stop_coordinate_conflict" stopId "Multiple authoritative coordinates"
                        | _ -> ()
                        yield row
                yield! sourceNativeParentRows
                yield! newPostRows
            }
            |> Seq.distinctBy (fun row -> rowValue row "stop_id")
            |> Seq.sortBy (fun row -> rowValue row "stop_id")
            |> Seq.toArray
        writeRows (Path.Combine(gtfsOutput, "stops.txt")) stopColumns outputStopRows

        let agencyRows =
            seq {
                yield!
                    csvRows prepared.baseGtfs "agency.txt"
                    |> Seq.filter (fun row ->
                        match optionText (rowValue row "agency_id") with
                        | Some id -> usedAgencyIds.Contains(id)
                        | None -> true)
                yield! sourceNativeAgencyRows.Values
            }
            |> Seq.sortBy (fun row -> rowValue row "agency_id")
            |> Seq.toArray
        writeRows (Path.Combine(gtfsOutput, "agency.txt")) agencyColumns agencyRows

        logProgress "write-calendar" 0L None
        let calendarDateRows =
            projection.serviceDates
            |> Seq.sortBy (fun pair -> pair.Key)
            |> Seq.collect (fun pair ->
                datesFor prepared.window pair.Value
                |> Seq.map (fun date -> [| pair.Key; dateString date; "1" |]))
        writeValues (Path.Combine(gtfsOutput, "calendar_dates.txt")) [| "service_id"; "date"; "exception_type" |] calendarDateRows
        let feedInfoRows =
            csvRows prepared.baseGtfs "feed_info.txt"
            |> Seq.map (fun baseRow ->
                cloneRow baseRow
                |> setRow <| "feed_start_date" <| dateString prepared.window.startDate
                |> setRow <| "feed_end_date" <| dateString prepared.window.endDate
                |> setRow <| "feed_version" <| $"regional-overlay-v1-gvd-{gvdYear}")
            |> Seq.toArray
        writeRows (Path.Combine(gtfsOutput, "feed_info.txt")) (columnsOf prepared.baseGtfs "feed_info.txt") feedInfoRows

        logProgress "write-shapes" 0L None
        let usedOutputShapeIds =
            Seq.append
                (projection.slicesByBaseTrip.Values |> Seq.collect id |> Seq.choose (fun slice -> slice.trip.shapeId))
                (projection.sourceTripAdditions |> Seq.choose (fun addition -> addition.trip.shapeId))
            |> Set.ofSeq
        if enabled prepared.policy "shapes" && usedOutputShapeIds.Count > 0 then
            let shapeColumns = [| "shape_id"; "shape_pt_lat"; "shape_pt_lon"; "shape_pt_sequence"; "shape_dist_traveled" |]
            usedOutputShapeIds
            |> Seq.sort
            |> Seq.collect (Shapes.rows projection.shapes)
            |> writeValues (Path.Combine(gtfsOutput, "shapes.txt")) shapeColumns

        let bindingsBySourceTrip = matches.bindings |> Seq.groupBy (fun value -> value.sourceTripId) |> dict
        let slicesForBinding (matchBinding: MatchBinding) =
            match projection.slicesByBaseTrip.TryGetValue(matchBinding.targetTripId) with
            | true, slices ->
                let bindingSelection = projection.selectionByBinding.[projection.bindingKey matchBinding]
                slices
                |> Array.choose (fun slice ->
                    match slice.selection with
                    | Some selected when projection.selectionsCompatible selected bindingSelection ->
                        let dates = dateIntersection matchBinding.dates slice.dates
                        if anyDate dates then Some (slice, dates) else None
                    | _ -> None)
            | _ -> [||]
        logProgress "write-transfers" 0L None
        let transferColumns = [|
            "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"
            "from_trip_id"; "to_trip_id"; "transfer_type"; "min_transfer_time"; "max_waiting_time"
        |]
        let transferRows = ResizeArray<CsvRow>()
        let transferProvenance = ResizeArray<string array>()
        for baseTransfer in csvRows prepared.baseGtfs "transfers.txt" do
            let fromTrip = optionText (rowValue baseTransfer "from_trip_id")
            let toTrip = optionText (rowValue baseTransfer "to_trip_id")
            let fromSlices =
                match fromTrip with
                | Some id when projection.slicesByBaseTrip.ContainsKey(id) -> projection.slicesByBaseTrip.[id] |> Array.map Some
                | Some _ -> [||]
                | None -> [| None |]
            let toSlices =
                match toTrip with
                | Some id when projection.slicesByBaseTrip.ContainsKey(id) -> projection.slicesByBaseTrip.[id] |> Array.map Some
                | Some _ -> [||]
                | None -> [| None |]
            for fromSlice in fromSlices do
                for toSlice in toSlices do
                    let row = cloneRow baseTransfer
                    fromSlice |> Option.iter (fun slice -> row.["from_trip_id"] <- slice.trip.id)
                    toSlice |> Option.iter (fun slice -> row.["to_trip_id"] <- slice.trip.id)
                    let stopsSurvive =
                        [ "from_stop_id"; "to_stop_id" ]
                        |> List.forall (fun column ->
                            match optionText (rowValue row column) with
                            | Some id -> usedStopIds.Contains(id)
                            | None -> true)
                    if stopsSurvive then transferRows.Add(row)

        let transferOutputsForSourceTrip sourceTripId =
            seq {
                match bindingsBySourceTrip.TryGetValue(sourceTripId) with
                | true, sourceBindings ->
                    for sourceBinding in sourceBindings do
                        if projection.bindingEligible "transfers" sourceBinding then
                            for slice, dates in slicesForBinding sourceBinding do
                                yield slice.trip.id, dates
                | _ -> ()
                match projection.sourceTripAdditionsBySourceId.TryGetValue(sourceTripId) with
                | true, addition -> yield addition.trip.id, addition.projection.dates
                | _ -> ()
            }
            |> Seq.toArray
        if enabled prepared.policy "transfers" then
            for sourceTransfer in csvRows prepared.binding.payloadPath "transfers.txt" do
                let fromSourceTrip = optionText (rowValue sourceTransfer "from_trip_id")
                let toSourceTrip = optionText (rowValue sourceTransfer "to_trip_id")
                match fromSourceTrip, toSourceTrip with
                | None, None ->
                    transferProvenance.Add([|
                        rowValue sourceTransfer "from_stop_id"; rowValue sourceTransfer "to_stop_id"
                        "stop_only_not_projected"; "Standard GTFS cannot express date-bounded stop-only validity"
                    |])
                | Some fromId, Some toId ->
                    let fromOutputStop =
                        optionText (rowValue sourceTransfer "from_stop_id")
                        |> Option.bind (fun id -> match projection.outputStopForSource.TryGetValue(id) with | true, value -> Some value | _ -> None)
                        |> Option.filter usedStopIds.Contains
                    let toOutputStop =
                        optionText (rowValue sourceTransfer "to_stop_id")
                        |> Option.bind (fun id -> match projection.outputStopForSource.TryGetValue(id) with | true, value -> Some value | _ -> None)
                        |> Option.filter usedStopIds.Contains
                    let fromOutputs = transferOutputsForSourceTrip fromId
                    let toOutputs = transferOutputsForSourceTrip toId
                    if fromOutputs.Length = 0 || toOutputs.Length = 0 then
                        addDiagnostic prepared.diagnostics "transfer_reference_unresolved" (fromId + "->" + toId) "Both non-rail source trips and their output slices must resolve"
                    else
                        for fromTripId, fromDates in fromOutputs do
                            for toTripId, toDates in toOutputs do
                                let applicableDates = dateIntersection fromDates toDates
                                if anyDate applicableDates && fromOutputStop.IsSome && toOutputStop.IsSome then
                                    let row = CsvRow(StringComparer.Ordinal)
                                    for column in transferColumns do row.[column] <- ""
                                    row.["from_stop_id"] <- fromOutputStop |> Option.defaultValue ""
                                    row.["to_stop_id"] <- toOutputStop |> Option.defaultValue ""
                                    row.["from_trip_id"] <- fromTripId
                                    row.["to_trip_id"] <- toTripId
                                    row.["transfer_type"] <- rowValue sourceTransfer "transfer_type"
                                    row.["min_transfer_time"] <- rowValue sourceTransfer "min_transfer_time"
                                    row.["max_waiting_time"] <- rowValue sourceTransfer "max_waiting_time"
                                    transferRows.Add(row)
                                    transferProvenance.Add([|
                                        rowValue sourceTransfer "from_stop_id"; rowValue sourceTransfer "to_stop_id"
                                        "trip_specific_projected"; fromTripId + "->" + toTripId
                                    |])
                | _ ->
                    addDiagnostic prepared.diagnostics "transfer_reference_unresolved"
                        (rowValue sourceTransfer "from_trip_id" + "->" + rowValue sourceTransfer "to_trip_id")
                        "Both non-rail source trips and their output slices must resolve"
        if transferRows.Count > 0 then
            transferRows
            |> Seq.distinctBy (fun row -> transferColumns |> Array.map (rowValue row) |> String.concat "\u001f")
            |> Seq.sortBy (fun row -> rowValue row "from_trip_id", rowValue row "to_trip_id", rowValue row "from_stop_id", rowValue row "to_stop_id")
            |> writeRows (Path.Combine(gtfsOutput, "transfers.txt")) transferColumns

        let outputCzRouteRows =
            seq {
                yield! prepared.baseCzRouteRows |> Seq.filter (fun row -> usedRouteIds.Contains(rowValue row "route_id"))
                for KeyValue(outputRouteId, (sourceRoute, cisLineId)) in source.nativeRoutes do
                    if usedRouteIds.Contains(outputRouteId) then
                        let row = CsvRow(StringComparer.Ordinal)
                        for column in columnsOf prepared.baseExtensions "cz_routes.txt" do row.[column] <- ""
                        row.["route_id"] <- outputRouteId
                        row.["cis_line_id"] <- cisLineId
                        row.["public_line_number"] <- rowValue sourceRoute "route_short_name"
                        row.["source_provenance"] <- sourceIdentity prepared.binding.sourceId sourceRoute
                        yield row
            }
            |> Seq.sortBy (fun row -> rowValue row "route_id")
            |> Seq.toArray
        writeRows (Path.Combine(extensionsOutput, "cz_routes.txt")) (columnsOf prepared.baseExtensions "cz_routes.txt") outputCzRouteRows
        let mutable writtenCzTrips = 0L
        let outputCzTripRows = seq {
            for baseRow in csvRows prepared.baseExtensions "cz_trips.txt" do
                let baseTripId = rowValue baseRow "trip_id"
                match projection.slicesByBaseTrip.TryGetValue(baseTripId) with
                | true, slices ->
                    for slice in slices do
                        writtenCzTrips <- writtenCzTrips + 1L
                        if writtenCzTrips % 100000L = 0L then
                            logProgress "write-cz-trips" writtenCzTrips None
                        yield cloneRow baseRow |> setRow <| "trip_id" <| slice.trip.id
                | _ -> ()
            for addition in projection.sourceTripAdditions |> Array.sortBy (fun value -> value.trip.id) do
                writtenCzTrips <- writtenCzTrips + 1L
                let row = CsvRow(StringComparer.Ordinal)
                for column in columnsOf prepared.baseExtensions "cz_trips.txt" do row.[column] <- ""
                row.["trip_id"] <- addition.trip.id
                row.["cis_line_id"] <- if addition.projection.cisLineId.StartsWith("source:", StringComparison.Ordinal) then "" else addition.projection.cisLineId
                row.["source_trip_ids"] <- addition.projection.sourceTripReferences |> Array.map (fun (sourceId, tripId) -> sourceId + ":" + tripId) |> String.concat ";"
                row.["coverage_sources"] <- String.concat ";" addition.projection.sourceIds
                yield row
        }
        writeRows (Path.Combine(extensionsOutput, "cz_trips.txt")) (columnsOf prepared.baseExtensions "cz_trips.txt") outputCzTripRows
        logProgress "write-cz-trips" writtenCzTrips (Some writtenCzTrips)
        let outputCzStopRows = ResizeArray<CsvRow>()
        for baseRow in csvRows prepared.baseExtensions "cz_stops.txt" do
            if usedStopIds.Contains(rowValue baseRow "stop_id") then outputCzStopRows.Add(baseRow)
        for KeyValue(outputPlaceId, group) in source.nativeStopPlaces do
            if usedStopIds.Contains(outputPlaceId) then
                let row = CsvRow(StringComparer.Ordinal)
                for column in columnsOf prepared.baseExtensions "cz_stops.txt" do row.[column] <- ""
                row.["stop_id"] <- outputPlaceId
                row.["stop_place_id"] <- outputPlaceId
                row.["asw_id"] <- group.members |> Array.map (fun memberRow -> rowValue memberRow prepared.policy.source.stopMatch.groupColumn) |> Array.distinct |> String.concat ";"
                row.["source_ids"] <- group.members |> Array.map (fun memberRow -> sourceIdentity prepared.binding.sourceId memberRow + ":" + originalIdentity "stop_id" memberRow) |> String.concat ";"
                outputCzStopRows.Add(row)
        for sourceStopId in projection.acceptedSourceStopIds |> Set.toArray |> Array.sort do
            match projection.outputStopForSource.TryGetValue(sourceStopId) with
            | true, outputStopId when usedStopIds.Contains(outputStopId) && outputStopId <> source.mappedPlaceBySourceStop.[sourceStopId] ->
                let row = CsvRow(StringComparer.Ordinal)
                for column in columnsOf prepared.baseExtensions "cz_stops.txt" do row.[column] <- ""
                row.["stop_id"] <- outputStopId
                row.["stop_place_id"] <- source.mappedPlaceBySourceStop.[sourceStopId]
                row.["post_id"] <- optionText (rowValue projection.sourceStops.[sourceStopId] prepared.policy.source.stopMatch.postColumn) |> Option.defaultValue (originalIdentity "stop_id" projection.sourceStops.[sourceStopId])
                row.["asw_id"] <- rowValue projection.sourceStops.[sourceStopId] prepared.policy.source.stopMatch.groupColumn
                row.["source_ids"] <- sourceIdentity prepared.binding.sourceId projection.sourceStops.[sourceStopId] + ":" + originalIdentity "stop_id" projection.sourceStops.[sourceStopId]
                outputCzStopRows.Add(row)
            | _ -> ()
        outputCzStopRows
        |> Seq.distinctBy (fun row -> rowValue row "stop_id")
        |> Seq.sortBy (fun row -> rowValue row "stop_id")
        |> writeRows (Path.Combine(extensionsOutput, "cz_stops.txt")) (columnsOf prepared.baseExtensions "cz_stops.txt")

        let stopZonePath = Path.Combine(prepared.baseExtensions, "cz_stop_zones.txt")
        if File.Exists(stopZonePath) then
            let stopZoneColumns =
                seq {
                    yield! columnsOf prepared.baseExtensions "cz_stop_zones.txt"
                    yield! [ "stop_place_id"; "zone_id"; "zone_code"; "route_id"; "ids_system_id"; "source_provenance" ]
                }
                |> Seq.distinct
                |> Seq.toArray
            seq {
                yield!
                    csvRows prepared.baseExtensions "cz_stop_zones.txt"
                    |> Seq.filter (fun row -> usedStopIds.Contains(rowValue row "stop_place_id") && (String.IsNullOrWhiteSpace(rowValue row "route_id") || usedRouteIds.Contains(rowValue row "route_id")))
                for KeyValue(stopPlaceId, zones) in regionalZonesByPlace do
                    if isUsedPlace stopPlaceId then
                        for code in zones do
                            let row = CsvRow(StringComparer.Ordinal)
                            for column in stopZoneColumns do row.[column] <- ""
                            row.["stop_place_id"] <- stopPlaceId
                            row.["zone_id"] <- outputZoneIdentity code
                            row.["zone_code"] <- code
                            row.["ids_system_id"] <- "ids-jmk"
                            row.["source_provenance"] <- "ids-jmk-gtfs"
                            yield row
            }
            |> Seq.distinctBy (fun row -> String.concat "\u001f" [ rowValue row "stop_place_id"; rowValue row "zone_id"; rowValue row "route_id"; rowValue row "ids_system_id" ])
            |> Seq.sortBy (fun row -> rowValue row "stop_place_id", rowValue row "zone_id", rowValue row "route_id")
            |> writeRows (Path.Combine(extensionsOutput, "cz_stop_zones.txt")) stopZoneColumns
        let tripStopZonePath = Path.Combine(prepared.baseExtensions, "cz_trip_stop_zones.txt")
        if File.Exists(tripStopZonePath) then
            seq {
                for row in csvRows prepared.baseExtensions "cz_trip_stop_zones.txt" do
                    let baseTripId = rowValue row "trip_id"
                    match projection.slicesByBaseTrip.TryGetValue(baseTripId) with
                    | true, slices ->
                        for slice in slices do yield cloneRow row |> setRow <| "trip_id" <| slice.trip.id
                    | _ -> ()
            }
            |> writeRows (Path.Combine(extensionsOutput, "cz_trip_stop_zones.txt")) (columnsOf prepared.baseExtensions "cz_trip_stop_zones.txt")

        selectedFieldsWriter.Flush()
        selectedFieldsWriter.Dispose()
        logProgress "write-selected-field-provenance" selectedFieldCount (Some selectedFieldCount)
        logProgress "write-reports" 0L None
        let reports = Reports.write {
            prepared = prepared
            projection = projection
            source = source
            matches = matches
            temporary = temporary
            usedStopIds = usedStopIds
            usedRouteIds = usedRouteIds
            usedOutputShapeIds = usedOutputShapeIds
            slicesForBinding = slicesForBinding
            transferProvenance = transferProvenance
        }
        let fullyCoveredSourceTrips = reports.fullyCoveredSourceTrips
        let matchedSourceTripSet = reports.matchedSourceTripSet
        let activeSourceTrips = reports.activeSourceTrips

        let manifestRouteCount = outputRouteRows.Length
        let manifestTripCount = outputTripCount
        let manifestStopCount = outputStopRows.Length
        let manifestCallCount = writtenCallCount
        let manifestServiceCount = projection.serviceDates.Count
        let manifestShapeCount = usedOutputShapeIds.Count
        let manifestTransferCount = transferRows.Count
        let manifestMatchedTripCount = matchedSourceTripSet.Count
        let manifestUnmatchedTripCount = activeSourceTrips.Length - manifestMatchedTripCount
        let manifestAmbiguousTripCount =
            matches.unresolvedPending
            |> Seq.map (fun value -> value.sourceTripId)
            |> Seq.filter (fullyCoveredSourceTrips.Contains >> not)
            |> Seq.distinct
            |> Seq.length
        let finalResult = {
            outputPath = prepared.outputBundle
            matchedTrips = manifestMatchedTripCount
            unmatchedTrips = manifestUnmatchedTripCount
            ambiguousTrips = manifestAmbiguousTripCount
            matchedStopGroups = source.stopGroupMatches.Count
            unmatchedStopGroups = source.stopGroups.Length - source.stopGroupMatches.Count
            selectedShapes = manifestShapeCount
            selectedTransfers = manifestTransferCount
        }
        let perSourceCounts =
            let sourceIds =
                source.tripRows
                |> Seq.map (sourceIdentity prepared.binding.sourceId)
                |> Seq.distinct
                |> Seq.sort
                |> Seq.toArray
            sourceIds
            |> Array.map (fun sourceId ->
                let active = activeSourceTrips |> Array.filter (fun row -> sourceIdentity prepared.binding.sourceId row = sourceId)
                let matched =
                    matchedSourceTripSet
                    |> Seq.filter (fun tripId -> source.tripsById.ContainsKey(tripId) && sourceIdentity prepared.binding.sourceId source.tripsById.[tripId] = sourceId)
                    |> Seq.length
                let ambiguous =
                    matches.unresolvedPending
                    |> Seq.map (fun value -> value.sourceTripId)
                    |> Seq.filter (fun tripId -> source.tripsById.ContainsKey(tripId) && sourceIdentity prepared.binding.sourceId source.tripsById.[tripId] = sourceId)
                    |> Seq.filter (fullyCoveredSourceTrips.Contains >> not)
                    |> Seq.distinct
                    |> Seq.length
                let groups = source.stopGroups |> Array.filter (fun group -> sourceIdentity prepared.binding.sourceId group.members.[0] = sourceId)
                let matchedGroups = groups |> Array.filter (fun group -> source.stopGroupMatches.ContainsKey(group.groupId)) |> Array.length
                let item = Dictionary<string, obj>()
                item.["source_id"] <- box sourceId
                item.["matched_source_trips"] <- box matched
                item.["unmatched_source_trips"] <- box (max 0 (active.Length - matched))
                item.["ambiguous_source_trips"] <- box ambiguous
                item.["matched_stop_groups"] <- box matchedGroups
                item.["unmatched_stop_groups"] <- box (groups.Length - matchedGroups)
                item.["selected_shapes"] <- box (projection.outputShapeBySource |> Seq.filter (fun pair -> pair.Key.StartsWith(sourceId + ":", StringComparison.Ordinal)) |> Seq.length)
                item :> obj)
        projection.selectionByBinding.Clear()
        matches.bindings.Clear()
        source.stopInferenceEvidence.Clear()
        projection.slicesByBaseTrip.Clear()
        projection.serviceDates.Clear()
        transferRows.Clear()
        transferProvenance.Clear()
        outputCzStopRows.Clear()
        logProgress "compact-before-evidence" 1L (Some 1L)

        let baseEvidence = Path.Combine(temporary, "base-evidence")
        Directory.CreateDirectory(baseEvidence) |> ignore
        logProgress "copy-base-evidence" 0L None
        let mutable copiedEvidenceFiles = 0L
        if File.Exists(Path.Combine(prepared.baseBundle, "gtfs.zip")) then
            // The production base can be read in place until finalization.  Do
            // not duplicate its (potentially national-scale) serving payloads
            // in compiler scratch merely to pass them to the package writer.
            File.WriteAllText(Path.Combine(temporary, "base-package.path"), prepared.baseBundle, new UTF8Encoding(false))
            File.Copy(Path.Combine(prepared.baseBundle, "manifest.json"), Path.Combine(baseEvidence, "manifest.json"), false)
            copiedEvidenceFiles <- 1L
        else
            for file in Directory.EnumerateFiles(prepared.baseBundle) do
                File.Copy(file, Path.Combine(baseEvidence, Path.GetFileName(file)), false)
                copiedEvidenceFiles <- copiedEvidenceFiles + 1L
                logProgress "copy-base-evidence" copiedEvidenceFiles None
            for directory in Directory.EnumerateDirectories(prepared.baseBundle) do
                let name = Path.GetFileName(directory)
                if name <> "gtfs-intermediate" && name <> "extensions" then
                    copyDirectory directory (Path.Combine(baseEvidence, name))
        logProgress "copy-base-evidence" copiedEvidenceFiles (Some copiedEvidenceFiles)
        let policyOutput = Path.Combine(temporary, "policy")
        Directory.CreateDirectory(policyOutput) |> ignore
        let multiSourceMetadataPath = Path.Combine(prepared.binding.payloadPath, "overlay_sources.json")
        let isMultiSource = File.Exists(multiSourceMetadataPath)
        if isMultiSource then
            copyDirectory (Path.Combine(prepared.binding.payloadPath, "_overlay", "policies")) policyOutput
        else
            File.Copy(prepared.policyPath, Path.Combine(policyOutput, "overlay-policy.json"), false)
        for configuredPath in [ prepared.policy.source.overrides.routes; prepared.policy.source.overrides.trips; prepared.policy.source.overrides.stops ] do
            match parseOverridePath prepared.policyPath configuredPath with
            | Some path -> File.Copy(path, Path.Combine(policyOutput, Path.GetFileName(path)), false)
            | None -> ()
        let sourceMetadata = Path.Combine(temporary, "source-metadata")
        Directory.CreateDirectory(sourceMetadata) |> ignore
        if isMultiSource then
            copyDirectory (Path.Combine(prepared.binding.payloadPath, "_overlay", "descriptors")) sourceMetadata
        else
            File.Copy(prepared.binding.descriptorPath, Path.Combine(sourceMetadata, prepared.binding.sourceId + "-descriptor.json"), false)

        let filesToHash =
            Directory.EnumerateFiles(temporary, "*", SearchOption.AllDirectories)
            |> Seq.filter (fun path -> Path.GetFileName(path) <> "manifest.json")
            |> Seq.sort
            |> Seq.toArray
        let mutable hashedFiles = 0L
        let fileEntries =
            filesToHash
            |> Seq.map (fun path ->
                let relative = Path.GetRelativePath(temporary, path).Replace('\\', '/')
                let info = FileInfo(path)
                let entry = Dictionary<string, obj>()
                entry.["path"] <- box relative
                entry.["bytes"] <- box info.Length
                entry.["sha256"] <- box (sha256File path)
                hashedFiles <- hashedFiles + 1L
                logProgress "hash-output-files" hashedFiles (Some (int64 filesToHash.Length))
                entry :> obj)
            |> Seq.sortBy (fun entry -> (entry :?> Dictionary<string, obj>).["path"] :?> string)
            |> Seq.toArray
        let counts = Dictionary<string, obj>()
        counts.["routes"] <- box manifestRouteCount
        counts.["trips"] <- box manifestTripCount
        counts.["stops"] <- box manifestStopCount
        counts.["calls"] <- box manifestCallCount
        counts.["services"] <- box manifestServiceCount
        counts.["shapes"] <- box manifestShapeCount
        counts.["transfers"] <- box manifestTransferCount
        counts.["matched_source_trips"] <- box manifestMatchedTripCount
        counts.["unmatched_source_trips"] <- box manifestUnmatchedTripCount
        counts.["ambiguous_source_trips"] <- box manifestAmbiguousTripCount
        if isMultiSource then counts.["per_source"] <- box perSourceCounts
        let sourceManifest = Dictionary<string, obj>()
        sourceManifest.["source_id"] <- box prepared.binding.sourceId
        sourceManifest.["payload_sha256"] <- box prepared.actualSourceHash
        sourceManifest.["descriptor_sha256"] <- box (sha256File prepared.binding.descriptorPath)
        sourceManifest.["retrieved_at"] <- box (prepared.descriptor.retrievedAt.ToString("O", CultureInfo.InvariantCulture))
        let gvd = Dictionary<string, obj>()
        gvd.["year"] <- box gvdYear
        gvd.["start_date"] <- box (dateString prepared.window.startDate)
        gvd.["end_date"] <- box (dateString prepared.window.endDate)
        let manifest = Dictionary<string, obj>()
        manifest.["bundle_format"] <- box "obehy-jrutil-regional-gtfs-overlay"
        manifest.["bundle_version"] <- box (if isMultiSource then MultiSourceOverlayBundleVersion else OverlayBundleVersion)
        manifest.["calibration"] <- box prepared.policy.calibration
        manifest.["audit_date"] <- box (dateString prepared.auditDate)
        manifest.["base_snapshot_date"] <- box (dateString prepared.snapshotDate)
        manifest.["publishable"] <- box (prepared.policy.publicationEnabled && not prepared.policy.calibration)
        manifest.["base_manifest_sha256"] <- box (sha256File (Path.Combine(prepared.baseBundle, "manifest.json")) )
        if isMultiSource then
            use metadataDocument = JsonDocument.Parse(File.ReadAllText(multiSourceMetadataPath))
            let metadataRoot = metadataDocument.RootElement
            manifest.["sources"] <- box (JsonSerializer.Deserialize<obj>(metadataRoot.GetProperty("sources").GetRawText(), jsonOptions))
            manifest.["policy_sha256"] <- box (metadataRoot.GetProperty("policy_sha256").GetString())
        else
            manifest.["source"] <- box sourceManifest
            manifest.["policy_sha256"] <- box (sha256File prepared.policyPath)
        manifest.["gvd"] <- box gvd
        manifest.["jrutil_commit"] <- box (currentCommit ())
        manifest.["counts"] <- box counts
        manifest.["files"] <- box fileEntries
        File.WriteAllText(Path.Combine(temporary, "manifest.json"), JsonSerializer.Serialize(manifest, jsonOptions), new UTF8Encoding(false))

        prepared.scratch.Flush()
        let productionTemporary = temporary + ".production"
        JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) temporary productionTemporary
        diagnosticsOutput
        |> Option.iter (fun path ->
            JrUtil.Serving.PackageWriter.writeDiagnosticArtifact temporary path diagnosticTraces)
        Directory.Delete(temporary, true)
        Directory.Move(productionTemporary, prepared.outputBundle)
        finalResult
    with error ->
        if Directory.Exists(temporary) then Directory.Delete(temporary, true)
        let productionTemporary = temporary + ".production"
        if Directory.Exists(productionTemporary) then Directory.Delete(productionTemporary, true)
        reraise ()
