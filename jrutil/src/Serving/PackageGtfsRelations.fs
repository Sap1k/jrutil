// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open JrUtil
open JrUtil.Hashing
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Runtime
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Parquet
open Parquet.Schema

open JrUtil.RegionalOverlay.GtfsRows
open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.OutputIds
open JrUtil.RegionalOverlay.Model

open JrUtil.Serving.PackageRows
open JrUtil.Serving.PackageBaseRelations

/// Serving relations derived from the compiler's GTFS tables.
module PackageGtfsRelations =
    let internal gtfsRelations (input: CompilerOutput.Output) (tripCallSummaries: IDictionary<string, TripCallSummary>)
                              (nonContiguousTripSequences: IDictionary<string, HashSet<int>>) =
        let agencies = CompilerOutput.rows input.gtfs "agency.txt" |> Seq.map (fun row -> objectRow [
            "agency_id", box (value "agency_id" row); "name", box (value "agency_name" row)
            "url", nullableString (value "agency_url" row); "timezone", box (value "agency_timezone" row)
            "language", nullableString (value "agency_lang" row); "phone", nullableString (value "agency_phone" row)
            "fare_url", nullableString (value "agency_fare_url" row); "email", nullableString (value "agency_email" row) ])
        // Location metadata in `location` column order: municipality, district name, district
        // code (okres), nearby place, country, coordinate precision.
        let locationMetadataColumns =
            [| "municipality_name"; "district_name"; "district_code"; "nearby_place"; "country_code"; "coordinate_precision" |]
        let stopMetadata =
            if CompilerOutput.has input.sidecars "source_stop_metadata" then
                CompilerOutput.values input.sidecars "source_stop_metadata" [| "gtfs_stop_id"; "town"; "district"; "okres"; "nearby_place"; "country"; "coordinate_precision" |]
                |> Seq.map (fun row -> row.[0], row.[1..]) |> dict
            else
                // A regional overlay keeps base location ids, so inherit the base package's metadata.
                match input.basePackage with
                | Some package when File.Exists(Path.Combine(package, "serving", "location.parquet")) ->
                    PackageReader.readTextRowsWithOptional (List.ofArray locationMetadataColumns)
                        (Path.Combine(package, "serving", "location.parquet"))
                        (Array.append [| "location_id" |] locationMetadataColumns)
                    |> Seq.filter (fun row -> row.[1..] |> Array.exists (fun value -> value <> ""))
                    |> Seq.map (fun row -> row.[0], row.[1..]) |> dict
                | _ -> dict []
        let locations = CompilerOutput.rows input.gtfs "stops.txt" |> Seq.map (fun row ->
            let locationType = value "location_type" row
            let parent = value "parent_station" row
            let kind = if locationType = "1" then "stop_place" elif not (String.IsNullOrEmpty(parent)) then "boarding_point" else "stop_place"
            let metadata = match stopMetadata.TryGetValue(value "stop_id" row) with | true, result -> Some result | _ -> None
            objectRow [
                "location_id", box (value "stop_id" row); "kind", box kind; "domain", box "scheduled"
                "parent_location_id", nullableString parent; "name", box (value "stop_name" row)
                "public_code", nullableString (value "stop_code" row); "description", nullableString (value "stop_desc" row)
                "municipality_name", metadata |> Option.map (fun value -> nullableString value.[0]) |> Option.defaultValue null
                "district_name", metadata |> Option.map (fun value -> nullableString value.[1]) |> Option.defaultValue null
                "district_code", metadata |> Option.map (fun value -> nullableString value.[2]) |> Option.defaultValue null
                "nearby_place", metadata |> Option.map (fun value -> nullableString value.[3]) |> Option.defaultValue null
                "country_code", metadata |> Option.map (fun value -> nullableString value.[4]) |> Option.defaultValue null
                "coordinate_precision", metadata |> Option.map (fun value -> nullableString value.[5]) |> Option.defaultValue null
                "longitude", nullableParsed number (value "stop_lon" row); "latitude", nullableParsed number (value "stop_lat" row)
                "url", nullableString (value "stop_url" row); "timezone", nullableString (value "stop_timezone" row)
                "wheelchair_boarding", nullableParsed int16 (value "wheelchair_boarding" row) ])
        let routeType value = integer value
        let mode value = servingMode (routeType value)
        // JDF routes are regular or detour (výluka) timetables; other sources carry no kind.
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
                    PackageReader.readTextRowsWithOptional [ "timetable_kind" ] (Path.Combine(package, "serving", "route.parquet")) [| "route_id"; "timetable_kind" |]
                    |> Seq.filter (fun row -> row.[1] <> "")
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
                 | _ -> null) ])
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
        let shapeIds = CompilerOutput.rows input.gtfs "shapes.txt" |> Seq.map (value "shape_id") |> Seq.distinct |> Seq.map (fun id -> objectRow [ "shape_id", box id; "generation_method", box "source_or_compiler" ])
        let shapePoints = CompilerOutput.rows input.gtfs "shapes.txt" |> Seq.map (fun row -> objectRow [
            "shape_id", box (value "shape_id" row); "sequence", box (integer (value "shape_pt_sequence" row))
            "longitude", box (number (value "shape_pt_lon" row)); "latitude", box (number (value "shape_pt_lat" row))
            "distance_traveled", nullableParsed number (value "shape_dist_traveled" row) ])
        let trips = CompilerOutput.rows input.gtfs "trips.txt" |> Seq.map (fun row -> objectRow [
            "trip_id", box (value "trip_id" row); "route_id", box (value "route_id" row); "service_id", box (value "service_id" row)
            "direction", nullableParsed int16 (value "direction_id" row); "headsign", nullableString (value "trip_headsign" row)
            "short_name", nullableString (value "trip_short_name" row); "block_key", nullableString (value "block_id" row)
            "wheelchair_accessible", nullableParsed int16 (value "wheelchair_accessible" row); "bikes_allowed", nullableParsed int16 (value "bikes_allowed" row)
            "shape_id", nullableString (value "shape_id" row) ])
        let parentByStop = CompilerOutput.rows input.gtfs "stops.txt" |> Seq.map (fun row -> value "stop_id" row, value "parent_station" row) |> dict
        let calls = seq {
            let completed = HashSet<string>(StringComparer.Ordinal)
            let mutable currentTrip = ""
            let mutable firstSequence = 0
            let mutable lastSequence = 0
            let mutable firstStop = ""
            let mutable lastStop = ""
            let mutable scheduledStart = Nullable<int>()
            let mutable scheduledEnd = Nullable<int>()
            use pattern = IncrementalHash.CreateHash(HashAlgorithmName.SHA256)
            let separator = [| byte '\n' |]
            let mutable hasPatternRows = false
            let currentSequences = ResizeArray<int>()
            let mutable contiguousSequences = true
            let parsedSeconds value =
                match seconds value with | null -> Nullable() | parsed -> Nullable(unbox<int> parsed)
            let firstTime row =
                let departure = value "departure_time" row
                parsedSeconds (if String.IsNullOrWhiteSpace(departure) then value "arrival_time" row else departure)
            let lastTime row =
                let arrival = value "arrival_time" row
                parsedSeconds (if String.IsNullOrWhiteSpace(arrival) then value "departure_time" row else arrival)
            let finish () =
                if currentTrip <> "" then
                    tripCallSummaries.Add(currentTrip, {
                        firstSequence = firstSequence; lastSequence = lastSequence
                        firstStopId = firstStop; lastStopId = lastStop
                        scheduledStart = scheduledStart; scheduledEnd = scheduledEnd
                        callPatternSha256 = "v1:" + (pattern.GetHashAndReset() |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()) })
                    if not contiguousSequences then
                        nonContiguousTripSequences.Add(currentTrip, HashSet<int>(currentSequences))
                    completed.Add(currentTrip) |> ignore
            for row in CompilerOutput.rows input.gtfs "stop_times.txt" do
                let tripId = value "trip_id" row
                let sequence = integer (value "stop_sequence" row)
                if tripId <> currentTrip then
                    finish ()
                    if completed.Contains(tripId) then invalidOp $"stop_times.txt is not grouped by trip_id: {tripId}"
                    currentTrip <- tripId
                    firstSequence <- sequence
                    firstStop <- value "stop_id" row
                    scheduledStart <- firstTime row
                    hasPatternRows <- false
                    currentSequences.Clear()
                    contiguousSequences <- true
                elif sequence <= lastSequence then
                    invalidOp $"stop_times.txt is not ordered by stop_sequence for trip {tripId}"
                elif sequence <> lastSequence + 1 then
                    contiguousSequences <- false
                currentSequences.Add(sequence)
                if hasPatternRows then pattern.AppendData(separator)
                [ value "stop_sequence" row; value "stop_id" row; value "arrival_time" row
                  value "departure_time" row; value "pickup_type" row; value "drop_off_type" row ]
                |> Identity.compositeKey |> Encoding.UTF8.GetBytes |> pattern.AppendData
                hasPatternRows <- true
                lastSequence <- sequence
                lastStop <- value "stop_id" row
                scheduledEnd <- lastTime row
                let stopId = value "stop_id" row
                let parent = match parentByStop.TryGetValue(stopId) with | true, result -> result | _ -> ""
                let location, boarding = if String.IsNullOrEmpty(parent) then stopId, null else parent, box stopId
                yield objectRow [
                    "trip_id", box tripId; "sequence", box sequence; "location_id", box location
                    "passenger_service", box true; "boarding_point_id", boarding; "route_stop_id", null
                    "scheduled_arrival", seconds (value "arrival_time" row); "scheduled_departure", seconds (value "departure_time" row); "scheduled_passage", null
                    "pickup_type", box (if String.IsNullOrEmpty(value "pickup_type" row) then 0s else int16 (value "pickup_type" row))
                    "dropoff_type", box (if String.IsNullOrEmpty(value "drop_off_type" row) then 0s else int16 (value "drop_off_type" row))
                    "timepoint", box (value "timepoint" row <> "0"); "stop_headsign", nullableString (value "stop_headsign" row)
                    "shape_distance_traveled", nullableParsed number (value "shape_dist_traveled" row) ]
            finish ()
        }
        let transferRows = CompilerOutput.rows input.gtfs "transfers.txt" |> Seq.map (fun row ->
            let selectors = [ "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" ] |> List.map (fun name -> name, value name row)
            let key = Identity.bindingId "transfer" selectors
            objectRow [
                "transfer_key", box key; "from_location_id", box (value "from_stop_id" row); "to_location_id", box (value "to_stop_id" row)
                "from_route_id", nullableString (value "from_route_id" row); "to_route_id", nullableString (value "to_route_id" row)
                "from_trip_id", nullableString (value "from_trip_id" row); "to_trip_id", nullableString (value "to_trip_id" row)
                "transfer_type", box (int16 (value "transfer_type" row)); "minimum_transfer_time", nullableParsed integer (value "min_transfer_time" row)
                "maximum_waiting_time", nullableParsed integer (value "max_waiting_time" row) ])
        Map [
            "agency", agencies; "location", locations; "route", routes; "service_calendar", calendars
            "service_exception", exceptions; "shape", shapeIds; "shape_point", shapePoints
            "trip", trips; "trip_call", calls; "transfer", transferRows
        ]

    let internal writeOrderedTripCalls progress (input: CompilerOutput.Output) path
                                      (tripCallSummaries: IDictionary<string, TripCallSummary>)
                                      (nonContiguousTripSequences: IDictionary<string, HashSet<int>>)
                                      (wantedTargetTrips: HashSet<string>)
                                      (targetCallSchedules: IDictionary<string, TargetCallSchedule>) =
        let columns = [|
            "trip_id"; "arrival_time"; "departure_time"; "stop_id"; "stop_sequence"
            "pickup_type"; "drop_off_type"; "timepoint"; "stop_headsign"; "shape_dist_traveled"
        |]
        let parentByStop =
            CompilerOutput.values input.gtfs "stops.txt" [| "stop_id"; "parent_station" |]
            |> Seq.map (fun row -> row.[0], row.[1]) |> dict
        let parsedSeconds value =
            if String.IsNullOrWhiteSpace(value) then Nullable()
            else
                let parts = value.Split(':')
                Nullable(integer parts.[0] * 3600 + integer parts.[1] * 60 + integer parts.[2])
        let parsedNumber value = if String.IsNullOrWhiteSpace(value) then Nullable() else Nullable(number value)
        let mutable currentTrip = ""
        let mutable firstSequence = 0
        let mutable lastSequence = 0
        let mutable firstStop = ""
        let mutable lastStop = ""
        let mutable scheduledStart = Nullable<int>()
        let mutable scheduledEnd = Nullable<int>()
        let mutable sparseSequences: HashSet<int> = null
        let targetTimes = ResizeArray<struct(int * Nullable<int> * Nullable<int>)>()
        let mutable hasPatternRows = false
        let mutable count = 0L
        use pattern = IncrementalHash.CreateHash(HashAlgorithmName.SHA256)
        use output = new TripCallWriter.Writer(path, CancellationToken.None, bufferedOutput = true)
        let finish () =
            if currentTrip <> "" then
                tripCallSummaries.Add(currentTrip, {
                    firstSequence = firstSequence; lastSequence = lastSequence
                    firstStopId = firstStop; lastStopId = lastStop
                    scheduledStart = scheduledStart; scheduledEnd = scheduledEnd
                    callPatternSha256 = "v1:" + (pattern.GetHashAndReset() |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()) })
                if not (isNull sparseSequences) then nonContiguousTripSequences.Add(currentTrip, sparseSequences)
                if wantedTargetTrips.Contains(currentTrip) then
                    targetCallSchedules.Add(currentTrip, {
                        firstSequence = firstSequence
                        calls = targetTimes.ToArray()
                    })
        for row in CompilerOutput.values input.gtfs "stop_times.txt" columns do
            let tripId, stopId = row.[0], row.[3]
            let sequence = integer row.[4]
            if tripId <> currentTrip then
                finish ()
                currentTrip <- tripId
                firstSequence <- sequence
                firstStop <- stopId
                scheduledStart <- parsedSeconds (if String.IsNullOrWhiteSpace(row.[2]) then row.[1] else row.[2])
                sparseSequences <- null
                targetTimes.Clear()
                hasPatternRows <- false
            elif sequence <= lastSequence then
                invalidOp $"stop_times.txt is not ordered by stop_sequence for trip {tripId}"
            elif sequence <> lastSequence + 1 && isNull sparseSequences then
                sparseSequences <- HashSet<int>()
                for prior in firstSequence .. lastSequence do sparseSequences.Add(prior) |> ignore
            if not (isNull sparseSequences) then sparseSequences.Add(sequence) |> ignore
            if hasPatternRows then pattern.AppendData([| 10uy |])
            [ row.[4]; stopId; row.[1]; row.[2]; row.[5]; row.[6] ]
            |> Identity.compositeKey |> Encoding.UTF8.GetBytes |> pattern.AppendData
            hasPatternRows <- true
            lastSequence <- sequence
            lastStop <- stopId
            scheduledEnd <- parsedSeconds (if String.IsNullOrWhiteSpace(row.[1]) then row.[2] else row.[1])
            let parent = match parentByStop.TryGetValue(stopId) with | true, value -> value | _ -> ""
            let location, boarding = if String.IsNullOrEmpty(parent) then stopId, null else parent, stopId
            let arrival, departure = parsedSeconds row.[1], parsedSeconds row.[2]
            if wantedTargetTrips.Contains(tripId) then targetTimes.Add(struct(sequence, arrival, departure))
            output.Append({
                tripId = tripId; sequence = sequence; locationId = location; passengerService = true
                boardingPointId = boarding; routeStopId = null
                arrival = arrival; departure = departure; passage = Nullable()
                pickup = if String.IsNullOrEmpty(row.[5]) then 0s else int16 row.[5]
                dropoff = if String.IsNullOrEmpty(row.[6]) then 0s else int16 row.[6]
                timepoint = row.[7] <> "0"; headsign = if String.IsNullOrWhiteSpace(row.[8]) then null else row.[8]
                distance = parsedNumber row.[9] })
            count <- count + 1L
            if count % 100000L = 0L then progress count
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
        let flushShapes () =
            if shapeIds.Count > 0 then
                let values = shapeIds.ToArray()
                shapes.Append [| ColumnWriter.Text values; ColumnWriter.Text(Array.create values.Length "source_or_compiler") |]
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
        let optionalText value = if String.IsNullOrWhiteSpace(value) then null else value
        let optionalInt16 value = if String.IsNullOrWhiteSpace(value) then Nullable() else Nullable(int16 value)
        let rows =
            CompilerOutput.values input.gtfs "trips.txt"
                [| "trip_id"; "route_id"; "service_id"; "direction_id"; "trip_headsign"; "trip_short_name"
                   "block_id"; "wheelchair_accessible"; "bikes_allowed"; "shape_id" |]
            |> Seq.map (fun row -> {
                tripId = row.[0]; routeId = row.[1]; serviceId = row.[2]; direction = optionalInt16 row.[3]
                headsign = optionalText row.[4]; shortName = optionalText row.[5]; blockKey = optionalText row.[6]
                wheelchair = optionalInt16 row.[7]; bikes = optionalInt16 row.[8]; shapeId = optionalText row.[9]
            })
        let strings row = [| row.tripId; row.routeId; row.serviceId; row.headsign; row.shortName; row.blockKey; row.shapeId |]
        let size row = 192L + (strings row |> Array.sumBy (fun value -> if isNull value then 0L else 24L + 2L * int64 value.Length))
        let writeNullable (output: BinaryWriter) (value: Nullable<int16>) =
            output.Write(value.HasValue); if value.HasValue then output.Write(value.Value)
        let readNullable (input: BinaryReader) = if input.ReadBoolean() then Nullable(input.ReadInt16()) else Nullable()
        let encode (output: BinaryWriter) row =
            for value in strings row do output.Write(if isNull value then "" else value)
            writeNullable output row.direction; writeNullable output row.wheelchair; writeNullable output row.bikes
        let decode (input: BinaryReader) =
            let values = Array.init 7 (fun _ -> input.ReadString())
            { tripId = values.[0]; routeId = values.[1]; serviceId = values.[2]
              headsign = optionalText values.[3]; shortName = optionalText values.[4]; blockKey = optionalText values.[5]
              shapeId = optionalText values.[6]; direction = readNullable input; wheelchair = readNullable input; bikes = readNullable input }
        let columns (values: ServingTripRow array) =
            let column project = values |> Array.map project
            [| ColumnWriter.Text(column _.tripId); ColumnWriter.Text(column _.routeId); ColumnWriter.Text(column _.serviceId)
               ColumnWriter.OptionalInt16(column _.direction); ColumnWriter.Text(column _.headsign)
               ColumnWriter.Text(column _.shortName); ColumnWriter.Text(column _.blockKey)
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

    let internal summarizeTripCalls (input: CompilerOutput.Output) =
        let result = Dictionary<string, TripCallSummary>(StringComparer.Ordinal)
        let completed = HashSet<string>(StringComparer.Ordinal)
        let mutable currentTrip = ""
        let mutable firstSequence = 0
        let mutable lastSequence = 0
        let mutable firstStop = ""
        let mutable lastStop = ""
        let mutable scheduledStart = Nullable<int>()
        let mutable scheduledEnd = Nullable<int>()
        use pattern = IncrementalHash.CreateHash(HashAlgorithmName.SHA256)
        let mutable hasPatternRows = false
        let separator = [| byte '\n' |]
        let firstTime row =
            let departure = value "departure_time" row
            let parsed = if String.IsNullOrWhiteSpace(departure) then seconds (value "arrival_time" row) else seconds departure
            if isNull parsed then Nullable() else Nullable(unbox<int> parsed)
        let lastTime row =
            let arrival = value "arrival_time" row
            let parsed = if String.IsNullOrWhiteSpace(arrival) then seconds (value "departure_time" row) else seconds arrival
            if isNull parsed then Nullable() else Nullable(unbox<int> parsed)
        let finish () =
            if currentTrip <> "" then
                result.Add(currentTrip, {
                    firstSequence = firstSequence; lastSequence = lastSequence
                    firstStopId = firstStop; lastStopId = lastStop
                    scheduledStart = scheduledStart; scheduledEnd = scheduledEnd
                    callPatternSha256 = "v1:" + (pattern.GetHashAndReset() |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()) })
                completed.Add(currentTrip) |> ignore
        for row in CompilerOutput.rows input.gtfs "stop_times.txt" do
            let trip = value "trip_id" row
            let sequence = integer (value "stop_sequence" row)
            if trip <> currentTrip then
                finish ()
                if completed.Contains(trip) then
                    invalidOp $"stop_times.txt is not grouped by trip_id: {trip}"
                currentTrip <- trip
                firstSequence <- sequence
                firstStop <- value "stop_id" row
                scheduledStart <- firstTime row
                hasPatternRows <- false
            elif sequence <= lastSequence then
                invalidOp $"stop_times.txt is not ordered by stop_sequence for trip {trip}"
            if hasPatternRows then pattern.AppendData(separator)
            [ value "stop_sequence" row; value "stop_id" row; value "arrival_time" row
              value "departure_time" row; value "pickup_type" row; value "drop_off_type" row ]
            |> Identity.compositeKey |> Encoding.UTF8.GetBytes |> pattern.AppendData
            hasPatternRows <- true
            lastSequence <- sequence
            lastStop <- value "stop_id" row
            scheduledEnd <- lastTime row
        finish ()
        result

    let internal defaultSourceId (manifest: JsonElement) (snapshots: IDictionary<string, string>) =
        match snapshots.Keys |> Seq.sort |> Seq.tryHead with
        | Some value -> value
        | None when manifest.TryGetProperty("source_format") |> fst
                    && manifest.GetProperty("source_format").GetString() = "czptt" -> "national-czptt"
        | None -> "unknown-source"
