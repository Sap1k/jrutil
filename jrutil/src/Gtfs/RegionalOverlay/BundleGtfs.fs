// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BundleGtfs

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Values
open JrUtil.RegionalOverlay.GtfsFiles
open JrUtil.RegionalOverlay.GtfsRows
open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.OutputIds
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay
open JrUtil.RegionalOverlay.BundleOutput

/// Write trips and mark their routes as used.
let writeTrips (context: Context) =
    let prepared = context.prepared
    let projection = context.projection
    let gtfsOutput = context.gtfsOutput
    let usedRouteIds = context.usedRouteIds
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

/// Write used routes with inherited display fields; returns source-native agencies to add.
let writeRoutes (context: Context) =
    let prepared = context.prepared
    let projection = context.projection
    let source = context.source
    let matches = context.matches
    let gtfsOutput = context.gtfsOutput
    let usedRouteIds = context.usedRouteIds
    let usedAgencyIds = context.usedAgencyIds
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
                        // Detour routes keep their colours: the text colour marks
                        // the non-regular timetable and was chosen for that background.
                        let detour = prepared.baseDetourRoutes.Contains(routeId)
                        for capabilityName, column in displayFields do
                            let keepsBaseColor = detour && (column = "route_color" || column = "route_text_color")
                            if enabled prepared.policy capabilityName && not keepsBaseColor then
                                let values = sourceRows |> Seq.map (fun source -> rowValue source column) |> Seq.filter (String.IsNullOrWhiteSpace >> not) |> Seq.distinct |> Seq.toArray
                                if values.Length = 1 then
                                    row.[column] <- values.[0]
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
    agencyColumns, sourceNativeAgencyRows

/// Write used stops, parents and new posts; returns the IDS JMK zone claims.
let writeStops (context: Context) =
    let prepared = context.prepared
    let projection = context.projection
    let source = context.source
    let gtfsOutput = context.gtfsOutput
    let usedStopIds = context.usedStopIds
    let baseStopRowsById = prepared.baseStopRows |> Array.map (fun row -> rowValue row "stop_id", row) |> dict
    let sourceStopByOutput = projection.outputStopForSource |> Seq.map (fun pair -> pair.Value, pair.Key) |> dict
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
                    match projection.outputParentCoordinates.TryGetValue(stopId) with
                    | true, coordinates ->
                        let unique = coordinates |> Seq.distinct |> Seq.toArray
                        if unique.Length = 1 then
                            let lat, lon = unique.[0]
                            row.["stop_lat"] <- lat.ToString("G29", CultureInfo.InvariantCulture)
                            row.["stop_lon"] <- lon.ToString("G29", CultureInfo.InvariantCulture)
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
    { regionalZonesByPlace = regionalZonesByPlace; outputZoneIdentity = outputZoneIdentity; isUsedPlace = isUsedPlace }

/// Write used national agencies and the source-native ones.
let writeAgencies (context: Context) agencyColumns (sourceNativeAgencyRows: Dictionary<string, CsvRow>) =
    let prepared = context.prepared
    let gtfsOutput = context.gtfsOutput
    let usedAgencyIds = context.usedAgencyIds
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

/// Write calendar dates and feed info for the GVD window.
let writeCalendar (context: Context) gvdYear =
    let prepared = context.prepared
    let projection = context.projection
    let gtfsOutput = context.gtfsOutput
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

/// Write the shapes of output trips; returns their IDs.
let writeShapes (context: Context) =
    let prepared = context.prepared
    let projection = context.projection
    let gtfsOutput = context.gtfsOutput
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
    usedOutputShapeIds
