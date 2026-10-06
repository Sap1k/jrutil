// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open JrUtil
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text.Json
open System.Threading

open JrUtil.RegionalOverlay.GtfsRows
open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.Model

open JrUtil.Serving.PackageRows
open JrUtil.Serving.PackageBaseRelations

/// Serving relations derived from the compiler's GTFS tables.
module PackageGtfsRelations =
    /// One CZPTT path point from the `operational_calls` sidecar.
    [<Struct>]
    type internal OperationalCall = {
        sequence: int; location: string; passenger: bool
        arrival: Nullable<int>; departure: Nullable<int>
        subsidiaryCode: string; subsidiaryName: string; activeLineCode: string
    }

    /// CZPTT path facts that complete the GTFS calls of each trip part.
    type internal OperationalPaths = {
        /// PA id -> its points in sequence order.
        calls: IDictionary<string, OperationalCall array>
        /// Trip -> (PA id, part number).
        parts: IDictionary<string, struct(string * int16)>
    }

    let private optionalText value = if String.IsNullOrWhiteSpace(value) then null else value
    let private optionalSeconds value =
        match seconds value with | null -> Nullable() | parsed -> Nullable(unbox<int> parsed)

    /// The PA id of each CZPTT trip (`PA=` in cz_trips.txt source_trip_ids);
    /// the part number is the trip id's `:<n>` suffix.
    let internal czpttParts (input: CompilerOutput.Output) =
        let result = Dictionary<string, struct(string * int16)>(StringComparer.Ordinal)
        for row in CompilerOutput.values input.czech "cz_trips.txt" [| "trip_id"; "source_trip_ids" |] do
            let pa =
                row.[1].Split('|', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                |> Array.tryPick (fun token -> if token.StartsWith("PA=", StringComparison.Ordinal) then Some (token.Substring(3)) else None)
            match pa with
            | Some pa ->
                let suffix = row.[0].Substring(row.[0].LastIndexOf(':') + 1)
                result.[row.[0]] <- struct(pa, Int16.Parse(suffix, CultureInfo.InvariantCulture))
            | None -> ()
        result

    let internal operationalPaths (input: CompilerOutput.Output) =
        if not (CompilerOutput.has input.sidecars "operational_calls") then None else
        let calls =
            CompilerOutput.values input.sidecars "operational_calls"
                [| "source_pa_id"; "source_sequence"; "source_location_id"; "passenger_call"; "arrival_seconds"; "departure_seconds"
                   "subsidiary_code"; "subsidiary_name"; "active_line_code" |]
            |> Seq.groupBy (fun row -> row.[0])
            |> Seq.map (fun (pa, rows) ->
                pa, rows |> Seq.map (fun row -> {
                    sequence = integer row.[1]; location = "czptt:stop:" + row.[2]; passenger = Boolean.Parse row.[3]
                    arrival = (if row.[4] = "" then Nullable() else Nullable(integer row.[4]))
                    departure = (if row.[5] = "" then Nullable() else Nullable(integer row.[5]))
                    subsidiaryCode = optionalText row.[6]; subsidiaryName = optionalText row.[7]; activeLineCode = optionalText row.[8] })
                |> Seq.sortBy _.sequence |> Seq.toArray)
            |> dict
        Some { calls = calls; parts = czpttParts input }

    let internal gtfsRelations (input: CompilerOutput.Output) =
        let agencies = CompilerOutput.rows input.gtfs "agency.txt" |> Seq.map (fun row -> objectRow [
            "agency_id", box (value "agency_id" row); "name", box (value "agency_name" row)
            "url", nullableString (value "agency_url" row); "timezone", box (value "agency_timezone" row)
            "language", nullableString (value "agency_lang" row); "phone", nullableString (value "agency_phone" row)
            "fare_url", nullableString (value "agency_fare_url" row); "email", nullableString (value "agency_email" row) ])
        // Location metadata in `location` column order: municipality, district name, district
        // code (okres), nearby place, country, coordinate precision, coordinate source.
        let locationMetadataColumns =
            [| "municipality_name"; "district_name"; "district_code"; "nearby_place"; "country_code"; "coordinate_precision"; "coordinate_source" |]
        let stopMetadata =
            if CompilerOutput.has input.sidecars "source_stop_metadata" then
                CompilerOutput.values input.sidecars "source_stop_metadata"
                    [| "gtfs_stop_id"; "town"; "district"; "okres"; "nearby_place"; "country"; "coordinate_precision"; "coordinate_source" |]
                |> Seq.map (fun row ->
                    let values = row.[1..]
                    values.[5] <- coordinatePrecision values.[5]
                    values.[6] <- coordinateSource values.[6] |> Option.ofObj |> Option.defaultValue ""
                    row.[0], values) |> dict
            else
                // A regional overlay keeps base location ids, so inherit the base package's metadata.
                match input.basePackage with
                | Some package when File.Exists(Path.Combine(package, "serving", "location.parquet")) ->
                    PackageReader.readTextRows (Path.Combine(package, "serving", "location.parquet"))
                        (Array.append [| "location_id" |] locationMetadataColumns)
                    |> Seq.map (fun row -> row.[0], row.[1..]) |> dict
                | _ -> dict []
        // CZPTT points: coordinate provenance by location id.
        let pointSources =
            if CompilerOutput.has input.sidecars "operational_points" then
                CompilerOutput.values input.sidecars "operational_points" [| "source_location_id"; "coordinate_source" |]
                |> Seq.map (fun row -> "czptt:stop:" + row.[0], coordinateSource row.[1])
                |> dict
            else dict []
        let domain = if input.feed = "czptt" then "heavy_rail" else "surface"
        let stopIds = HashSet<string>(CompilerOutput.values input.gtfs "stops.txt" [| "stop_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
        let gtfsLocations = CompilerOutput.rows input.gtfs "stops.txt" |> Seq.map (fun row ->
            let id = value "stop_id" row
            let locationType = value "location_type" row
            let parent = value "parent_station" row
            let kind = if locationType = "1" then "stop_place" elif not (String.IsNullOrEmpty(parent)) then "boarding_point" else "stop_place"
            // A boarding point without its own metadata inherits its stop place's.
            let metadata =
                match stopMetadata.TryGetValue(id) with
                | true, result -> Some result
                | _ when parent <> "" -> (match stopMetadata.TryGetValue(parent) with | true, result -> Some result | _ -> None)
                | _ -> None
            let field index = metadata |> Option.map (fun value -> nullableString value.[index]) |> Option.defaultValue null
            let longitude, latitude = nullableParsed number (value "stop_lon" row), nullableParsed number (value "stop_lat" row)
            let positioned = not (isNull longitude || isNull latitude) && not (unbox<double> longitude = 0.0 && unbox<double> latitude = 0.0)
            let precision =
                if not positioned then "missing"
                else metadata |> Option.map (fun value -> coordinatePrecision value.[5]) |> Option.defaultValue "exact"
            let source =
                match field 6 with
                | null ->
                    match pointSources.TryGetValue(id), pointSources.TryGetValue(parent) with
                    | (true, value), _ | _, (true, value) -> box value
                    | _ when id.StartsWith("overlay:", StringComparison.Ordinal) -> box "overlay"
                    | _ -> null
                | value -> value
            objectRow [
                "location_id", box id; "kind", box kind; "domain", box domain
                "parent_location_id", nullableString parent; "name", box (value "stop_name" row)
                "public_code", nullableString (value "platform_code" row); "description", nullableString (value "stop_desc" row)
                "municipality_name", field 0; "district_name", field 1; "district_code", field 2
                "nearby_place", field 3; "country_code", field 4
                "coordinate_precision", box precision; "coordinate_source", (if positioned then source else null)
                "longitude", (if positioned then longitude else null); "latitude", (if positioned then latitude else null)
                "url", nullableString (value "stop_url" row); "timezone", nullableString (value "stop_timezone" row)
                "wheelchair_boarding", nullableParsed int16 (value "wheelchair_boarding" row) ])
        // CZPTT railway points that are not GTFS stops (timing and
        // non-passenger points of trip parts).
        let operationalLocations =
            if not (CompilerOutput.has input.sidecars "operational_points") then Seq.empty else
            CompilerOutput.values input.sidecars "operational_points"
                [| "source_location_id"; "country_code"; "source_name"; "latitude"; "longitude"; "coordinate_source" |]
            |> Seq.filter (fun row -> not (stopIds.Contains("czptt:stop:" + row.[0])))
            |> Seq.map (fun row ->
                let positioned = row.[3] <> "" && row.[4] <> ""
                objectRow [
                    "location_id", box ("czptt:stop:" + row.[0]); "kind", box "operational_point"; "domain", box domain
                    "parent_location_id", null; "name", box row.[2]; "public_code", null; "description", null
                    "municipality_name", null; "district_name", null; "district_code", null; "nearby_place", null
                    "country_code", nullableString row.[1]
                    "coordinate_precision", box (if positioned then "exact" else "missing")
                    "coordinate_source", (if positioned then box (coordinateSource row.[5]) else null)
                    "longitude", (if positioned then box (number row.[4]) else null)
                    "latitude", (if positioned then box (number row.[3]) else null)
                    "url", null; "timezone", null; "wheelchair_boarding", null ])
        let locations = Seq.append gtfsLocations operationalLocations
        let routeType value = integer value
        let mode value = servingMode (routeType value)
        // JDF routes are regular or detour (výluka) timetables; all others are regular.
        let timetableKinds =
            if CompilerOutput.has input.sidecars "source_route_metadata" then
                CompilerOutput.values input.sidecars "source_route_metadata" [| "gtfs_route_id"; "detour" |]
                |> Seq.filter (fun row -> row.[1] <> "")
                |> Seq.map (fun row -> row.[0], (if row.[1] = "True" then "detour" else "regular"))
                |> Seq.distinct
                |> dict
            else
                // A regional overlay keeps base route ids, so inherit the base package's kinds.
                match input.basePackage with
                | Some package when File.Exists(Path.Combine(package, "serving", "route.parquet")) ->
                    PackageReader.readTextRows (Path.Combine(package, "serving", "route.parquet")) [| "route_id"; "timetable_kind" |]
                    |> Seq.map (fun row -> row.[0], row.[1])
                    |> dict
                | _ -> dict []
        let routes = CompilerOutput.rows input.gtfs "routes.txt" |> Seq.map (fun row -> objectRow [
            "route_id", box (value "route_id" row); "agency_id", box (value "agency_id" row)
            "mode", box (mode (value "route_type" row)); "gtfs_route_type", box (routeType (value "route_type" row))
            "short_name", nullableString (value "route_short_name" row); "long_name", nullableString (value "route_long_name" row)
            "description", nullableString (value "route_desc" row); "url", nullableString (value "route_url" row)
            "color", nullableString (value "route_color" row); "text_color", nullableString (value "route_text_color" row)
            "sort_order", nullableParsed integer (value "route_sort_order" row)
            "timetable_kind",
                (match timetableKinds.TryGetValue(value "route_id" row) with
                 | true, kind -> box kind
                 | _ -> box "regular") ])
        let calendarRows = CompilerOutput.rows input.gtfs "calendar.txt" |> Seq.map (fun row ->
            let mask =
                [| "monday"; "tuesday"; "wednesday"; "thursday"; "friday"; "saturday"; "sunday" |]
                |> Array.mapi (fun index name -> if value name row = "1" then 1 <<< index else 0)
                |> Array.sum
            objectRow [ "service_id", box (value "service_id" row); "valid_from", box (date (value "start_date" row)); "valid_to", box (date (value "end_date" row)); "weekday_mask", box (Convert.ToInt16(mask)) ])
        let existingServices = calendarRows |> Seq.map (fun row -> row.["service_id"] :?> string) |> Set.ofSeq
        let exceptionCalendars =
            seq {
                let bounds = Dictionary<string, struct(DateOnly * DateOnly)>(StringComparer.Ordinal)
                for row in CompilerOutput.values input.gtfs "calendar_dates.txt" [| "service_id"; "date" |] do
                    if not (existingServices.Contains row.[0]) then
                        let day = date row.[1]
                        bounds.[row.[0]] <-
                            match bounds.TryGetValue(row.[0]) with
                            | true, struct(first, last) -> struct(min first day, max last day)
                            | _ -> struct(day, day)
                for KeyValue(service, struct(first, last)) in bounds do
                    yield objectRow [ "service_id", box service; "valid_from", box first; "valid_to", box last; "weekday_mask", box 0s ]
            }
        let calendars = Seq.append calendarRows exceptionCalendars
        let exceptions = CompilerOutput.rows input.gtfs "calendar_dates.txt" |> Seq.map (fun row -> objectRow [
            "service_id", box (value "service_id" row); "service_date", box (date (value "date" row)); "added", box (value "exception_type" row = "1") ])
        // Overlay shapes come from the regional source feeds; others are generated.
        let generation (shape: string) = if shape.StartsWith("overlay:", StringComparison.Ordinal) then "source" else "compiler"
        let shapeIds = CompilerOutput.rows input.gtfs "shapes.txt" |> Seq.map (value "shape_id") |> Seq.distinct |> Seq.map (fun id -> objectRow [ "shape_id", box id; "generation_method", box (generation id) ])
        let shapePoints = CompilerOutput.rows input.gtfs "shapes.txt" |> Seq.map (fun row -> objectRow [
            "shape_id", box (value "shape_id" row); "sequence", box (integer (value "shape_pt_sequence" row))
            "longitude", box (number (value "shape_pt_lon" row)); "latitude", box (number (value "shape_pt_lat" row))
            "distance_traveled", nullableParsed number (value "shape_dist_traveled" row) ])
        let transferRows = CompilerOutput.rows input.gtfs "transfers.txt" |> Seq.map (fun row ->
            let selectors = [ "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" ] |> List.map (fun name -> name, value name row)
            objectRow [
                "transfer_key", box (Identity.feedId input.feed "transfer" selectors)
                "from_location_id", nullableString (value "from_stop_id" row); "to_location_id", nullableString (value "to_stop_id" row)
                "from_route_id", nullableString (value "from_route_id" row); "to_route_id", nullableString (value "to_route_id" row)
                "from_trip_id", nullableString (value "from_trip_id" row); "to_trip_id", nullableString (value "to_trip_id" row)
                "transfer_type", box (int16 (value "transfer_type" row)); "minimum_transfer_time", nullableParsed integer (value "min_transfer_time" row)
                "maximum_waiting_time", nullableParsed integer (value "max_waiting_time" row) ])
        Map [
            "agency", agencies; "location", locations; "route", routes; "service_calendar", calendars
            "service_exception", exceptions; "shape", shapeIds; "shape_point", shapePoints; "transfer", transferRows
        ]

    /// Stream GTFS stop_times as trip_call. CZPTT trip parts also get their
    /// path's non-passenger points: a rail part those between its first and
    /// last call, the first (last) part of the path the points before (after)
    /// it when it is a rail part; a bus (NAD) part gets none.
    let internal writeOrderedTripCalls progress (input: CompilerOutput.Output) path
                                      (tripCallSummaries: IDictionary<string, TripCallSummary>)
                                      (nonContiguousTripSequences: IDictionary<string, HashSet<int>>)
                                      (operational: OperationalPaths option) =
        let columns = [|
            "trip_id"; "arrival_time"; "departure_time"; "stop_id"; "stop_sequence"
            "pickup_type"; "drop_off_type"; "timepoint"; "stop_headsign"; "shape_dist_traveled"
        |]
        let parentByStop =
            CompilerOutput.values input.gtfs "stops.txt" [| "stop_id"; "parent_station" |]
            |> Seq.map (fun row -> row.[0], row.[1]) |> dict
        let parsedNumber value = if String.IsNullOrWhiteSpace(value) then Nullable() else Nullable(number value)
        // Per CZPTT trip part: (first, last) GTFS sequence, whether it is a
        // rail part, and whether it opens or closes its path.
        let partBounds =
            match operational with
            | None -> Dictionary<string, struct(int * int * bool * bool * bool)>(StringComparer.Ordinal)
            | Some paths ->
                let rail =
                    let routeModes =
                        CompilerOutput.values input.gtfs "routes.txt" [| "route_id"; "route_type" |]
                        |> Seq.map (fun row -> row.[0], servingMode (integer row.[1])) |> dict
                    CompilerOutput.values input.gtfs "trips.txt" [| "trip_id"; "route_id" |]
                    |> Seq.map (fun row -> row.[0], routeModes.[row.[1]] = "rail") |> dict
                let bounds = Dictionary<string, struct(int * int)>(StringComparer.Ordinal)
                for row in CompilerOutput.values input.gtfs "stop_times.txt" [| "trip_id"; "stop_sequence" |] do
                    let sequence = integer row.[1]
                    bounds.[row.[0]] <-
                        match bounds.TryGetValue(row.[0]) with
                        | true, struct(first, last) -> struct(min first sequence, max last sequence)
                        | _ -> struct(sequence, sequence)
                let pathBounds = Dictionary<string, struct(int * int)>(StringComparer.Ordinal)
                for KeyValue(trip, struct(first, last)) in bounds do
                    match paths.parts.TryGetValue(trip) with
                    | true, struct(pa, _) ->
                        pathBounds.[pa] <-
                            match pathBounds.TryGetValue(pa) with
                            | true, struct(low, high) -> struct(min low first, max high last)
                            | _ -> struct(first, last)
                    | _ -> ()
                let result = Dictionary<string, struct(int * int * bool * bool * bool)>(StringComparer.Ordinal)
                for KeyValue(trip, struct(first, last)) in bounds do
                    match paths.parts.TryGetValue(trip) with
                    | true, struct(pa, _) ->
                        let struct(low, high) = pathBounds.[pa]
                        result.[trip] <- struct(first, last, rail.[trip], first = low, last = high)
                    | _ -> ()
                result
        let mutable count = 0L
        use output = new TripCallWriter.Writer(path, CancellationToken.None, bufferedOutput = true)
        let pending = ResizeArray<TripCallWriter.Row>()
        let mutable currentTrip = ""
        let finish () =
            if currentTrip <> "" then
                let sequences = pending |> Seq.map _.sequence |> Seq.toArray
                let path =
                    match operational with
                    | Some paths ->
                        match paths.parts.TryGetValue(currentTrip) with
                        | true, struct(pa, _) -> (match paths.calls.TryGetValue(pa) with | true, calls -> calls | _ -> [||])
                        | _ -> [||]
                    | None -> [||]
                let pointAt = Dictionary<int, OperationalCall>()
                for call in path do pointAt.[call.sequence] <- call
                // Passenger calls get their path point's operational columns.
                let calls =
                    pending |> Seq.map (fun call ->
                        match pointAt.TryGetValue(call.sequence) with
                        | true, point ->
                            { call with subsidiaryCode = point.subsidiaryCode; subsidiaryName = point.subsidiaryName
                                        activeLineCode = point.activeLineCode }
                        | _ -> call)
                    |> Seq.toArray
                let extra =
                    match partBounds.TryGetValue(currentTrip) with
                    | true, struct(low, high, true, opens, closes) ->
                        path |> Array.filter (fun point ->
                            not point.passenger
                            && ((point.sequence > low && point.sequence < high)
                                || (opens && point.sequence < low) || (closes && point.sequence > high)))
                        |> Array.map (fun point -> {
                            tripId = currentTrip; sequence = point.sequence; locationId = point.location; passengerService = false
                            boardingPointId = null; routeStopId = null; arrival = point.arrival; departure = point.departure
                            pickup = 1s; dropoff = 1s; timepoint = point.arrival.HasValue || point.departure.HasValue
                            headsign = null; distance = Nullable()
                            subsidiaryCode = point.subsidiaryCode; subsidiaryName = point.subsidiaryName
                            activeLineCode = point.activeLineCode } : TripCallWriter.Row)
                    | _ -> [||]
                let merged = Array.append calls extra |> Array.sortBy _.sequence
                for call in merged do
                    output.Append(call)
                    count <- count + 1L
                    if count % 100000L = 0L then progress count
                let first, last = merged.[0].sequence, merged.[merged.Length - 1].sequence
                tripCallSummaries.Add(currentTrip, { firstSequence = first; lastSequence = last })
                if last - first + 1 <> merged.Length then
                    nonContiguousTripSequences.Add(currentTrip, HashSet<int>(merged |> Seq.map _.sequence))
                ignore sequences
                pending.Clear()
        for row in CompilerOutput.values input.gtfs "stop_times.txt" columns do
            let tripId, stopId = row.[0], row.[3]
            let sequence = integer row.[4]
            if tripId <> currentTrip then
                finish ()
                currentTrip <- tripId
            elif sequence <= pending.[pending.Count - 1].sequence then
                invalidOp $"stop_times.txt is not ordered by stop_sequence for trip {tripId}"
            let parent = match parentByStop.TryGetValue(stopId) with | true, value -> value | _ -> ""
            let location, boarding = if String.IsNullOrEmpty(parent) then stopId, null else parent, stopId
            pending.Add({
                tripId = tripId; sequence = sequence; locationId = location; passengerService = true
                boardingPointId = boarding; routeStopId = null
                arrival = optionalSeconds row.[1]; departure = optionalSeconds row.[2]
                pickup = if String.IsNullOrEmpty(row.[5]) then 0s else int16 row.[5]
                dropoff = if String.IsNullOrEmpty(row.[6]) then 0s else int16 row.[6]
                timepoint = row.[7] <> "0"; headsign = optionalText row.[8]
                distance = parsedNumber row.[9]
                subsidiaryCode = null; subsidiaryName = null; activeLineCode = null })
        finish ()
        let written = output.Complete()
        progress written
        int written

    let internal writeOrderedShapes progress (input: CompilerOutput.Output) serving =
        let shapeSchema = Schema.relations |> Array.find (fun relation -> relation.name = "shape")
        let pointSchema = Schema.relations |> Array.find (fun relation -> relation.name = "shape_point")
        use shapes = new ColumnWriter.Writer(Path.Combine(serving, "shape.parquet"), shapeSchema, 8192, CancellationToken.None)
        use points = new ColumnWriter.Writer(Path.Combine(serving, "shape_point.parquet"), pointSchema, 65536, CancellationToken.None)
        let pointBuffer = ResizeArray<struct(string * int * double * double * Nullable<double>)>(65536)
        let shapeIds = ResizeArray<string>(8192)
        let generation (shape: string) = if shape.StartsWith("overlay:", StringComparison.Ordinal) then "source" else "compiler"
        let flushShapes () =
            if shapeIds.Count > 0 then
                let values = shapeIds.ToArray()
                shapes.Append [| ColumnWriter.Text values; ColumnWriter.Text(values |> Array.map generation) |]
                shapeIds.Clear()
        let flushPoints () =
            if pointBuffer.Count > 0 then
                let values = pointBuffer.ToArray()
                points.Append [|
                    ColumnWriter.Text(values |> Array.map (fun struct(shape, _, _, _, _) -> shape))
                    ColumnWriter.Int32(values |> Array.map (fun struct(_, sequence, _, _, _) -> sequence))
                    ColumnWriter.Float64(values |> Array.map (fun struct(_, _, longitude, _, _) -> longitude))
                    ColumnWriter.Float64(values |> Array.map (fun struct(_, _, _, latitude, _) -> latitude))
                    ColumnWriter.OptionalFloat64(values |> Array.map (fun struct(_, _, _, _, distance) -> distance))
                |]
                pointBuffer.Clear()
                progress points.RowCount
        let mutable previousShape: string = null
        let mutable previousSequence = 0
        for row in CompilerOutput.values input.gtfs "shapes.txt"
                       [| "shape_id"; "shape_pt_sequence"; "shape_pt_lon"; "shape_pt_lat"; "shape_dist_traveled" |] do
            let shape, sequence = row.[0], integer row.[1]
            if not (isNull previousShape) then
                let order = StringComparer.Ordinal.Compare(previousShape, shape)
                if order > 0 || (order = 0 && sequence <= previousSequence) then
                    invalidOp $"shapes.txt is not strictly ordered at {shape}/{sequence}"
            if shape <> previousShape then
                shapeIds.Add(shape)
                if shapeIds.Count = 8192 then flushShapes ()
            pointBuffer.Add(struct(
                shape, sequence, number row.[2], number row.[3],
                if String.IsNullOrWhiteSpace(row.[4]) then Nullable() else Nullable(number row.[4])))
            if pointBuffer.Count = 65536 then flushPoints ()
            previousShape <- shape
            previousSequence <- sequence
        flushShapes (); flushPoints ()
        int shapes.RowCount, int points.RowCount

    let internal writeOrderedTrips progress (input: CompilerOutput.Output) path =
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "trip")
        let optionalInt16 value = if String.IsNullOrWhiteSpace(value) then Nullable() else Nullable(int16 value)
        let parts = czpttParts input
        let rows =
            CompilerOutput.values input.gtfs "trips.txt"
                [| "trip_id"; "route_id"; "service_id"; "direction_id"; "trip_headsign"; "trip_short_name"
                   "block_id"; "wheelchair_accessible"; "bikes_allowed"; "shape_id" |]
            |> Seq.map (fun row ->
                let runKey, runPart =
                    match parts.TryGetValue(row.[0]) with
                    | true, struct(pa, part) -> pa, Nullable part
                    | _ -> null, Nullable()
                {
                    tripId = row.[0]; routeId = row.[1]; serviceId = row.[2]; direction = optionalInt16 row.[3]
                    headsign = optionalText row.[4]; shortName = optionalText row.[5]; blockKey = optionalText row.[6]
                    runKey = runKey; runPart = runPart
                    wheelchair = optionalInt16 row.[7]; bikes = optionalInt16 row.[8]; shapeId = optionalText row.[9]
                })
        let strings row = [| row.tripId; row.routeId; row.serviceId; row.headsign; row.shortName; row.blockKey; row.runKey; row.shapeId |]
        let size row = 200L + (strings row |> Array.sumBy (fun value -> if isNull value then 0L else 24L + 2L * int64 value.Length))
        let writeNullable (output: BinaryWriter) (value: Nullable<int16>) =
            output.Write(value.HasValue); if value.HasValue then output.Write(value.Value)
        let readNullable (input: BinaryReader) = if input.ReadBoolean() then Nullable(input.ReadInt16()) else Nullable()
        let encode (output: BinaryWriter) row =
            for value in strings row do output.Write(if isNull value then "" else value)
            writeNullable output row.direction; writeNullable output row.runPart
            writeNullable output row.wheelchair; writeNullable output row.bikes
        let decode (input: BinaryReader) =
            let values = Array.init 8 (fun _ -> input.ReadString())
            { tripId = values.[0]; routeId = values.[1]; serviceId = values.[2]
              headsign = optionalText values.[3]; shortName = optionalText values.[4]; blockKey = optionalText values.[5]
              runKey = optionalText values.[6]; shapeId = optionalText values.[7]
              direction = readNullable input; runPart = readNullable input; wheelchair = readNullable input; bikes = readNullable input }
        let columns (values: ServingTripRow array) =
            let column project = values |> Array.map project
            [| ColumnWriter.Text(column _.tripId); ColumnWriter.Text(column _.routeId); ColumnWriter.Text(column _.serviceId)
               ColumnWriter.OptionalInt16(column _.direction); ColumnWriter.Text(column _.headsign)
               ColumnWriter.Text(column _.shortName); ColumnWriter.Text(column _.blockKey)
               ColumnWriter.Text(column _.runKey); ColumnWriter.OptionalInt16(column _.runPart)
               ColumnWriter.OptionalInt16(column _.wheelchair); ColumnWriter.OptionalInt16(column _.bikes)
               ColumnWriter.Text(column _.shapeId) |]
        RelationWriter.write path schema (128L * 1024L * 1024L)
            (16L * 1024L * 1024L) 65536 CancellationToken.None (fun _ count -> progress count)
            size (fun row -> row.tripId) encode decode columns rows

    let internal serviceBounds (input: CompilerOutput.Output) =
        let result = Dictionary<string, DateOnly * DateOnly>(StringComparer.Ordinal)
        for row in CompilerOutput.rows input.gtfs "calendar.txt" do
            result.[value "service_id" row] <- date (value "start_date" row), date (value "end_date" row)
        for row in CompilerOutput.values input.gtfs "calendar_dates.txt" [| "service_id"; "date" |] do
            let service, day = row.[0], date row.[1]
            match result.TryGetValue(service) with
            | true, (first, last) -> result.[service] <- min first day, max last day
            | _ -> result.[service] <- day, day
        result

    let internal defaultSourceId (manifest: JsonElement) (snapshots: IDictionary<string, string>) =
        match snapshots.Keys |> Seq.sort |> Seq.tryHead with
        | Some value -> value
        | None when manifest.TryGetProperty("source_format") |> fst
                    && manifest.GetProperty("source_format").GetString() = "czptt" -> "national-czptt"
        | None -> "unknown-source"
