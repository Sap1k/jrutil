// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Parquet
open Parquet.Schema

open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay.Model

module PackageWriter =
    type private TripCallSummary = TripCallWriter.Summary

    type private TargetCallSchedule = {
        firstSequence: int
        calls: struct(int * Nullable<int> * Nullable<int>) array
    }

    [<Struct>]
    type private ServingTripRow = {
        tripId: string; routeId: string; serviceId: string
        direction: Nullable<int16>; headsign: string; shortName: string; blockKey: string
        wheelchair: Nullable<int16>; bikes: Nullable<int16>; shapeId: string
    }

    [<Struct>]
    type private SourceTripProjection = {
        sourceId: string; sourceTripId: string; outputTripId: string
        validFrom: string; validTo: string; methodName: string
    }

    [<Struct>]
    type private BaseTripProjection = {
        baseTripId: string; outputTripId: string; validFrom: string; validTo: string
    }

    // Retain native binding facts, not a dictionary and boxed values for every trip.
    type private TripBinding = JrUtil.Serving.Model.TripBinding

    let private objectRow (values: (string * obj) seq) =
        let result = Dictionary<string, obj>(StringComparer.Ordinal)
        for name, value in values do result.Add(name, value)
        result :> IDictionary<string, obj>

    let private nullableString value =
        if String.IsNullOrWhiteSpace(value) then null else box value

    let private bindingRow (binding: TripBinding) =
        objectRow [
            "binding_id", box binding.binding_id
            "source_id", box binding.source_id
            "trip_namespace", box binding.trip_namespace
            "source_trip_id", box binding.source_trip_id
            "trip_id", box binding.trip_id
            "service_id", box binding.service_id
            "valid_from", box binding.valid_from
            "valid_to", box binding.valid_to
            "binding_status", box binding.binding_status
            "scheduled_start", box binding.scheduled_start
            "scheduled_end", box binding.scheduled_end
            "source_route_id", nullableString binding.source_route_id
            "source_direction_id", nullableString binding.source_direction_id
            "source_start_location_id", nullableString binding.source_start_location_id
            "source_end_location_id", nullableString binding.source_end_location_id
            "source_block_id", nullableString binding.source_block_id
            "source_run_id", nullableString binding.source_run_id
            "source_duty_id", nullableString binding.source_duty_id
            "call_pattern_sha256", box binding.call_pattern_sha256
            "variant_key", nullableString binding.variant_key
        ]

    let private nullableParsed parse value =
        if String.IsNullOrWhiteSpace(value) then null else box (parse value)

    let private integer (value: string) = Int32.Parse(value, CultureInfo.InvariantCulture)
    let private integer64 (value: string) = Int64.Parse(value, CultureInfo.InvariantCulture)
    let private int16 (value: string) = Int16.Parse(value, CultureInfo.InvariantCulture)
    let private number (value: string) = Double.Parse(value, CultureInfo.InvariantCulture)
    let private date (value: string) =
        let compact = value.Replace("-", "")
        match DateOnly.TryParseExact(compact, "yyyyMMdd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, parsed -> parsed
        | _ -> DateOnly.FromDateTime(DateTime.Parse(value, CultureInfo.InvariantCulture, DateTimeStyles.AllowWhiteSpaces))

    let private seconds value =
        if String.IsNullOrWhiteSpace(value) then null
        else
            let parts = value.Split(':') |> Array.map integer
            box (parts.[0] * 3600 + parts.[1] * 60 + parts.[2])

    let private optionalColumn<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> ValueType> (values: obj array) =
        values |> Array.map (fun value -> if isNull value then Nullable() else Nullable(unbox<'T> value))

    let private columnFromRows (field: JrUtil.Serving.Schema.Field) (rows: IDictionary<string,obj> array) =
        let values = rows |> Array.map (fun row -> row.[field.name])
        match field.dataType, field.nullable with
        | Schema.Text, _ -> ColumnWriter.Text(values |> Array.map (fun value -> if isNull value then null else unbox<string> value))
        | Schema.Int16, false -> ColumnWriter.Int16(values |> Array.map unbox<int16>)
        | Schema.Int16, true -> ColumnWriter.OptionalInt16(optionalColumn<int16> values)
        | Schema.Int32, false -> ColumnWriter.Int32(values |> Array.map unbox<int>)
        | Schema.Int32, true -> ColumnWriter.OptionalInt32(optionalColumn<int> values)
        | Schema.Int64, false -> ColumnWriter.Int64(values |> Array.map unbox<int64>)
        | Schema.Int64, true -> ColumnWriter.OptionalInt64(optionalColumn<int64> values)
        | Schema.Float64, false -> ColumnWriter.Float64(values |> Array.map unbox<double>)
        | Schema.Float64, true -> ColumnWriter.OptionalFloat64(optionalColumn<double> values)
        | Schema.Boolean, false -> ColumnWriter.Boolean(values |> Array.map unbox<bool>)
        | Schema.Boolean, true -> ColumnWriter.OptionalBoolean(optionalColumn<bool> values)
        | Schema.Date, false -> ColumnWriter.Date(values |> Array.map unbox<DateOnly>)
        | Schema.Date, true -> ColumnWriter.OptionalDate(optionalColumn<DateOnly> values)

    let private writeParquet progress path (relation: Schema.Relation) (rows: seq<IDictionary<string,obj>>) =
        use writer = new ColumnWriter.Writer(path, relation, 65536, CancellationToken.None)
        let buffer = ResizeArray<IDictionary<string,obj>>(65536)
        let maximumBufferBytes = 16L * 1024L * 1024L
        let mutable bufferBytes = int64 relation.fields.Length * 24L
        let flush () =
            if buffer.Count > 0 then
                let values = buffer.ToArray()
                writer.Append(relation.fields |> Array.map (fun field -> columnFromRows field values))
                progress writer.RowCount
                buffer.Clear()
                bufferBytes <- int64 relation.fields.Length * 24L
        for row in rows do
            // This transitional producer still owns dictionaries. Account for
            // them as well as the typed columns built during a synchronous flush.
            let bytes =
                128L + (row |> Seq.sumBy (fun pair ->
                    96L + (match pair.Value with | :? string as value -> 24L + 2L * int64 value.Length | _ -> 16L)))
            if bytes + int64 relation.fields.Length * 24L > maximumBufferBytes then
                invalidOp $"{relation.name}: one row exceeds the {maximumBufferBytes}-byte buffer limit"
            if buffer.Count > 0 && bufferBytes + bytes > maximumBufferBytes then flush ()
            buffer.Add(row)
            bufferBytes <- bufferBytes + bytes
            if buffer.Count = 65536 then flush ()
        flush ()
        int writer.RowCount

    let private writeRequiredTextRelation progress path (relation: Schema.Relation) rows =
        if relation.fields |> Array.exists (fun field -> field.dataType <> Schema.Text || field.nullable) then
            invalidArg "relation" $"{relation.name} is not an all-required-text relation"
        let indexes =
            relation.fields
            |> Array.mapi (fun index field -> field.name, index)
            |> dict
        let keyIndexes = relation.sortKey |> Array.map (fun name -> indexes.[name])
        let typed =
            rows |> Seq.map (fun (row: IDictionary<string,obj>) ->
                relation.fields |> Array.map (fun field -> unbox<string> row.[field.name]))
        let compareRows (left: string array) (right: string array) =
            let mutable result, index = 0, 0
            while result = 0 && index < keyIndexes.Length do
                let field = keyIndexes.[index]
                result <- StringComparer.Ordinal.Compare(left.[field], right.[field])
                index <- index + 1
            result
        let size (row: string array) =
            48L + int64 row.Length * 32L + (row |> Array.sumBy (fun value -> 2L * int64 value.Length))
        let encode (output: BinaryWriter) (row: string array) =
            for value in row do output.Write(value)
        let decode (input: BinaryReader) = Array.init relation.fields.Length (fun _ -> input.ReadString())
        let columns (values: string array array) =
            relation.fields
            |> Array.mapi (fun index _ -> ColumnWriter.Text(values |> Array.map (fun row -> row.[index])))
        RelationWriter.write path relation (256L * 1024L * 1024L) 1048576
            (16L * 1024L * 1024L) 65536 CancellationToken.None progress size compareRows (=) encode decode columns typed

    let private writeSelectedFieldProvenance progress (path: string) (relation: Schema.Relation) rows =
        let indexes = relation.fields |> Array.mapi (fun index field -> field.name, index) |> dict
        let objectTypeIndex, objectKeyIndex = indexes.["object_type"], indexes.["object_key"]
        let keyIndexes = relation.sortKey |> Array.map (fun name -> indexes.[name])
        let compareRows (left: string array) (right: string array) =
            let mutable result, index = 0, 0
            while result = 0 && index < keyIndexes.Length do
                let field = keyIndexes.[index]
                result <- StringComparer.Ordinal.Compare(left.[field], right.[field])
                index <- index + 1
            result
        let size (row: string array) =
            48L + int64 row.Length * 32L + (row |> Array.sumBy (fun value -> 2L * int64 value.Length))
        let encode (output: BinaryWriter) (row: string array) = for value in row do output.Write(value)
        let decode (input: BinaryReader) = Array.init relation.fields.Length (fun _ -> input.ReadString())
        let columns (values: string array array) =
            relation.fields |> Array.mapi (fun index _ -> ColumnWriter.Text(values |> Array.map (fun row -> row.[index])))
        let typed = rows |> Seq.map (fun (row: IDictionary<string,obj>) -> relation.fields |> Array.map (fun field -> unbox<string> row.[field.name]))
        let groupKey (row: string array) =
            let key = row.[objectKeyIndex]
            if row.[objectTypeIndex] = "call" then
                let separator = key.LastIndexOf('/')
                if separator < 0 then key else key.Substring(0, separator)
            else key
        let root = Path.Combine(Path.GetDirectoryName(path), ".selected-field-partitions-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let writers = Dictionary<string, BinaryWriter>(StringComparer.Ordinal)
            let paths = Dictionary<string, string>(StringComparer.Ordinal)
            let previousGroups = Dictionary<string, string>(StringComparer.Ordinal)
            let monotonic = Dictionary<string, bool>(StringComparer.Ordinal)
            try
                let mutable count = 0L
                for row in typed do
                    let objectType = row.[objectTypeIndex]
                    let writer =
                        match writers.TryGetValue(objectType) with
                        | true, writer -> writer
                        | _ ->
                            let file = Path.Combine(root, writers.Count.ToString(CultureInfo.InvariantCulture) + ".bin")
                            let writer = new BinaryWriter(File.Create(file), Encoding.UTF8)
                            writers.Add(objectType, writer); paths.Add(objectType, file); monotonic.Add(objectType, true)
                            writer
                    let group = groupKey row
                    match previousGroups.TryGetValue(objectType) with
                    | true, previous when StringComparer.Ordinal.Compare(previous, group) > 0 -> monotonic.[objectType] <- false
                    | _ -> ()
                    previousGroups.[objectType] <- group
                    encode writer row
                    count <- count + 1L
                    if count % 100000L = 0L then progress "partition-input" count
            finally
                for writer in writers.Values do writer.Dispose()
            let ordered = seq {
                for objectType in paths.Keys |> Seq.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right)) do
                    let fileRows = seq {
                        use input = new BinaryReader(File.OpenRead(paths.[objectType]), Encoding.UTF8)
                        while input.BaseStream.Position < input.BaseStream.Length do yield decode input
                    }
                    if monotonic.[objectType] then
                        use input = fileRows.GetEnumerator()
                        let mutable available = input.MoveNext()
                        while available do
                            let group = groupKey input.Current
                            let buffer = ResizeArray<string array>()
                            while available && groupKey input.Current = group do
                                buffer.Add(input.Current)
                                available <- input.MoveNext()
                            let values = buffer.ToArray()
                            Array.sortInPlaceWith compareRows values
                            yield! values
                    else
                        yield! BinarySort.sort root (128L * 1024L * 1024L) 524288 CancellationToken.None
                            (fun operation count -> progress (objectType + "-" + operation) count)
                            size compareRows encode decode fileRows
            }
            RelationWriter.writeOrdered path relation (16L * 1024L * 1024L) 65536 CancellationToken.None
                progress size compareRows (=) columns ordered
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    let private csv path name =
        let file = Path.Combine(path, name)
        if File.Exists(file) then csvRows path name else Seq.empty

    // TextFieldParser is intentionally general, but its per-field machinery is
    // disproportionately expensive on the 13M-row stop_times hot path.  This
    // parser implements the same RFC-style comma/quote rules, including doubled
    // quotes and embedded newlines, while projecting only after one header map.
    let private fastCsvValues path name (columns: string array) = seq {
        let file = Path.Combine(path, name)
        if File.Exists(file) then
            use reader = new StreamReader(file, Encoding.UTF8, true, 1024 * 1024)
            let parse (record: string) =
                let values = ResizeArray<string>()
                let field = StringBuilder()
                let mutable quoted, index = false, 0
                while index < record.Length do
                    let character = record.[index]
                    if quoted then
                        if character = '"' then
                            if index + 1 < record.Length && record.[index + 1] = '"' then
                                field.Append('"') |> ignore; index <- index + 1
                            else quoted <- false
                        else field.Append(character) |> ignore
                    elif character = ',' then
                        values.Add(field.ToString()); field.Clear() |> ignore
                    elif character = '"' && field.Length = 0 then quoted <- true
                    else field.Append(character) |> ignore
                    index <- index + 1
                if quoted then invalidOp $"{name} contains an unterminated quoted field"
                values.Add(field.ToString())
                values.ToArray()
            let hasOpenQuote (record: string) =
                let mutable quoted, index = false, 0
                while index < record.Length do
                    if record.[index] = '"' then
                        if quoted && index + 1 < record.Length && record.[index + 1] = '"' then index <- index + 1
                        else quoted <- not quoted
                    index <- index + 1
                quoted
            let readRecord () =
                let first = reader.ReadLine()
                if isNull first then null else
                if not (hasOpenQuote first) then first else
                let builder = StringBuilder(first)
                let mutable openQuote = true
                while openQuote && not reader.EndOfStream do
                    builder.Append('\n').Append(reader.ReadLine()) |> ignore
                    openQuote <- hasOpenQuote (builder.ToString())
                builder.ToString()
            let headerText = readRecord ()
            if not (isNull headerText) then
                let header = parse headerText
                if header.Length > 0 then header.[0] <- header.[0].TrimStart('\uFEFF')
                let indexes = columns |> Array.map (fun column -> header |> Array.tryFindIndex ((=) column) |> Option.defaultValue -1)
                while not reader.EndOfStream do
                    let record = readRecord ()
                    if not (isNull record) && record <> "" then
                        let fields = parse record
                        yield indexes |> Array.map (fun index -> if index >= 0 && index < fields.Length then fields.[index] else "")
    }

    let private value column (row: CsvRow) = rowValue row column

    let private sourceManifest (manifest: JsonElement) =
        let descriptor (source: JsonElement) =
            let text (names: string list) =
                names |> Seq.tryPick (fun (name: string) -> match source.TryGetProperty(name) with | true, value -> Some(value.GetString()) | _ -> None)
                |> Option.defaultValue null
            text [ "source_id" ], text [ "payload_sha256"; "sha256" ]
        seq {
            match manifest.TryGetProperty("sources") with
            | true, sources ->
                for source in sources.EnumerateArray() do
                    yield descriptor source
            | _ ->
                match manifest.TryGetProperty("source") with
                | true, source -> yield descriptor source
                | _ ->
                    match manifest.TryGetProperty("source_snapshot") with
                    | true, source -> yield descriptor source
                    | _ -> ()
        }
        |> Seq.choose (fun (source, digest) ->
            if isNull source || isNull digest then None else Some (source, digest))
        |> dict

    let private sha256File path =
        use stream = File.OpenRead(path)
        SHA256.HashData(stream) |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()

    let private callSequenceKey trip sequence = Identity.compositeKey [ trip; sequence ]

    let private fieldText (value: obj) =
        if isNull value then "" else
        match value with
        | :? DateOnly as date -> date.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
        | _ -> Convert.ToString(value, CultureInfo.InvariantCulture)

    let private parseField (field: JrUtil.Serving.Schema.Field) (value: string) =
        if field.nullable && String.IsNullOrEmpty(value) then null else
        match field.dataType with
        | Schema.Text -> box value
        | Schema.Int16 -> box (int16 value)
        | Schema.Int32 -> box (integer value)
        | Schema.Int64 -> box (integer64 value)
        | Schema.Float64 -> box (number value)
        | Schema.Boolean -> box (Boolean.Parse value)
        | Schema.Date -> box (date value)

    let private tryBasePackage legacy =
        let reference = Path.Combine(legacy, "base-package.path")
        if File.Exists(reference) then
            let package = File.ReadAllText(reference).Trim()
            if Directory.Exists(package) then Some package else None
        else
            let copied = Path.Combine(legacy, "base-evidence")
            if File.Exists(Path.Combine(copied, "manifest.json")) then Some copied else None

    let private combinedSourceManifest legacy (manifest: JsonElement) =
        let result = Dictionary<string,string>(StringComparer.Ordinal)
        match tryBasePackage legacy with
        | Some package ->
            let path = Path.Combine(package, "manifest.json")
            if File.Exists(path) then
                use document = JsonDocument.Parse(File.ReadAllText(path))
                for pair in sourceManifest document.RootElement do result.[pair.Key] <- pair.Value
        | None -> ()
        for pair in sourceManifest manifest do result.[pair.Key] <- pair.Value
        result :> IDictionary<string,string>

    let private packageRelationRows package name =
        let relation = Schema.relations |> Array.find (fun relation -> relation.name = name)
        let path = Path.Combine(package, "serving", name + ".parquet")
        if not (File.Exists(path)) then Seq.empty else
        PackageReader.readTextRows path (relation.fields |> Array.map _.name)
        |> Seq.map (fun values ->
            objectRow (
                relation.fields
                |> Seq.mapi (fun index field -> field.name, parseField field values.[index])))

    let private copyRow (row: IDictionary<string,obj>) =
        objectRow (row |> Seq.map (fun pair -> pair.Key, pair.Value))

    let private setField name value (row: IDictionary<string,obj>) =
        let result = copyRow row
        result.[name] <- value
        result

    /// A regional overlay is a semantic projection of its production base
    /// package, not a fresh generic-GTFS conversion.  Project public base
    /// semantics through the explicit base-to-output mapping so native JDF
    /// meaning survives trip slicing and replacement without depending on
    /// private compiler staging files.
    let private projectedBaseRelations legacy gtfs =
        match tryBasePackage legacy with
        | None -> Map.empty
        | Some package ->
            let targetTrips =
                csvValues gtfs "trips.txt" [| "trip_id"; "service_id" |]
                |> Seq.map (fun row -> row.[0], row.[1]) |> dict
            let targetRoutes = HashSet<string>(csvValues gtfs "routes.txt" [| "route_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
            let targetLocations = HashSet<string>(csvValues gtfs "stops.txt" [| "stop_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
            let projections =
                csvValues (Path.Combine(legacy, "mappings")) "base_to_output_trips.csv"
                    [| "base_trip_id"; "output_trip_id" |]
                |> Seq.choose (fun row ->
                    match targetTrips.TryGetValue(row.[1]) with
                    | true, service -> Some (row.[0], struct(row.[1], service))
                    | _ -> None)
                |> Seq.groupBy fst
                |> Seq.map (fun (trip, values) ->
                    trip, values |> Seq.map snd |> Seq.distinct |> Seq.toArray)
                |> dict
            let projectionsByObjectKey =
                projections
                |> Seq.map (fun pair -> Identity.compositeKey [ pair.Key ], pair.Value)
                |> dict
            let projectTrip trip =
                match projections.TryGetValue(trip) with
                | true, targets -> targets :> seq<_>
                | _ -> Seq.empty
            let projectObjectTrip key =
                match projectionsByObjectKey.TryGetValue(key) with
                | true, targets -> targets :> seq<_>
                | _ -> Seq.empty
            let nullableText name (row: IDictionary<string,obj>) =
                match row.[name] with | null -> "" | value -> unbox<string> value
            let direct name = packageRelationRows package name
            let notes = direct "service_note"
            let noteAssignments = direct "service_note_assignment" |> Seq.collect (fun row ->
                let trip = nullableText "trip_id" row
                if trip = "" then
                    let route = nullableText "route_id" row
                    if route = "" || targetRoutes.Contains(route) then Seq.singleton row else Seq.empty
                else
                    projectTrip trip |> Seq.map (fun struct(target, service) ->
                        if target = trip && (isNull row.["service_id"] || unbox<string> row.["service_id"] = service) then row else
                        let result = copyRow row
                        result.["trip_id"] <- box target
                        if not (isNull result.["service_id"]) then result.["service_id"] <- box service
                        result.["assignment_id"] <- box (Identity.bindingId "note-assignment" [
                            "note", unbox<string> row.["note_id"]
                            "scope", unbox<string> row.["scope"]
                            "route", nullableText "route_id" row
                            "trip", target ])
                        result))
            let features = direct "service_feature_assignment" |> Seq.collect (fun row ->
                let trip = nullableText "trip_id" row
                if trip = "" then
                    let route = nullableText "route_id" row
                    if route = "" || targetRoutes.Contains(route) then Seq.singleton row else Seq.empty
                else
                    projectTrip trip |> Seq.map (fun struct(target, service) ->
                        if target = trip && (isNull row.["service_id"] || unbox<string> row.["service_id"] = service) then row else
                        let result = copyRow row
                        result.["trip_id"] <- box target
                        if not (isNull result.["service_id"]) then result.["service_id"] <- box service
                        result.["feature_id"] <- box (Identity.bindingId "service-feature" [
                            "trip", target; "code", unbox<string> row.["source_code"]
                            "source", unbox<string> row.["source_id"] ])
                        result))
            let locations = direct "location_feature" |> Seq.filter (fun row -> targetLocations.Contains(unbox<string> row.["location_id"]))
            let connections = direct "connection_claim" |> Seq.collect (fun row ->
                let origin = unbox<string> row.["origin_trip_id"]
                projectTrip origin |> Seq.map (fun struct(target, service) ->
                    if target = origin && (isNull row.["service_id"] || unbox<string> row.["service_id"] = service) then row else
                    let result = copyRow row
                    result.["origin_trip_id"] <- box target
                    if not (isNull result.["service_id"]) then result.["service_id"] <- box service
                    let originalId = unbox<string> row.["connection_id"]
                    if target <> origin then
                        result.["connection_id"] <- box (Identity.bindingId "connection-projection" [ "connection", originalId; "trip", target ])
                    let targetTrip = nullableText "target_trip_id" row
                    if targetTrip <> "" then
                        match projectTrip targetTrip |> Seq.tryHead with
                        | Some struct(projected, _) -> result.["target_trip_id"] <- box projected
                        | None -> result.["target_trip_id"] <- null
                    result))
            let restrictions = direct "travel_restriction_assignment" |> Seq.collect (fun row ->
                let trip = nullableText "trip_id" row
                if trip = "" then
                    let route = nullableText "route_id" row
                    if route = "" || targetRoutes.Contains(route) then Seq.singleton row else Seq.empty
                else
                    projectTrip trip |> Seq.map (fun struct(target, service) ->
                        if target = trip && (isNull row.["service_id"] || unbox<string> row.["service_id"] = service) then row else
                        let result = copyRow row
                        result.["trip_id"] <- box target
                        if not (isNull result.["service_id"]) then result.["service_id"] <- box service
                        result.["assignment_id"] <- box (Identity.bindingId "restriction" [
                            "scope", unbox<string> row.["scope"]
                            "route", nullableText "route_id" row
                            "trip", target
                            "route_stop", unbox<string> row.["source_route_stop_id"]
                            "group", unbox<string> row.["group_code"] ])
                        result))
            let routeStops = direct "route_stop" |> Seq.filter (fun row ->
                targetRoutes.Contains(unbox<string> row.["route_id"])
                && targetLocations.Contains(unbox<string> row.["location_id"]))
            let routeStopZones = direct "route_stop_zone" |> Seq.filter (fun row -> targetRoutes.Contains(unbox<string> row.["route_id"]))
            let entities = direct "source_entity_map" |> Seq.filter (fun row ->
                let target = unbox<string> row.["public_id"]
                match unbox<string> row.["entity_kind"] with
                | "route" -> targetRoutes.Contains(target)
                | "stop_place" | "boarding_point" | "location" -> targetLocations.Contains(target)
                | _ -> true)
            let routeKeys = direct "road_route_key" |> Seq.filter (fun row -> targetRoutes.Contains(unbox<string> row.["route_id"]))
            let origins = direct "object_origin" |> Seq.collect (fun row ->
                let kind = unbox<string> row.["object_type"]
                let key = unbox<string> row.["object_key"]
                if kind = "trip" then
                    projectObjectTrip key |> Seq.map (fun struct(target, _) ->
                        let projected = Identity.compositeKey [ target ]
                        if projected = key then row else setField "object_key" (box projected) row)
                elif kind = "call" then
                    let separator = key.LastIndexOf('/')
                    if separator < 0 then Seq.empty else
                    let trip, suffix = key.Substring(0, separator), key.Substring(separator)
                    projectObjectTrip trip |> Seq.map (fun struct(target, _) ->
                        let projected = Identity.compositeKey [ target ] + suffix
                        if projected = key then row else setField "object_key" (box projected) row)
                else Seq.singleton row)
            let selected = direct "selected_field_provenance" |> Seq.collect (fun row ->
                let kind = unbox<string> row.["object_type"]
                let key = unbox<string> row.["object_key"]
                if kind = "trip" then
                    projectObjectTrip key |> Seq.map (fun struct(target, _) ->
                        let projected = Identity.compositeKey [ target ]
                        if projected = key then row else setField "object_key" (box projected) row)
                elif kind = "call" then
                    let separator = key.LastIndexOf('/')
                    if separator < 0 then Seq.empty else
                    let trip, suffix = key.Substring(0, separator), key.Substring(separator)
                    projectObjectTrip trip |> Seq.map (fun struct(target, _) ->
                        let projected = Identity.compositeKey [ target ] + suffix
                        if projected = key then row else setField "object_key" (box projected) row)
                else Seq.singleton row)
            Map [
                "fare_system", direct "fare_system"
                "fare_zone", direct "fare_zone"
                "service_note", notes
                "service_note_assignment", noteAssignments
                "service_feature_assignment", features
                "location_feature", locations
                "connection_claim", connections
                "travel_restriction_assignment", restrictions
                "source_entity_map", entities
                "road_route_key", routeKeys
                "selected_field_provenance", selected
                "object_origin", origins
                "route_stop", routeStops
                "route_stop_zone", routeStopZones
            ]

    let private compareField (dataType: JrUtil.Serving.Schema.FieldType) (left: string) (right: string) =
        if String.IsNullOrEmpty(left) || String.IsNullOrEmpty(right) then
            if String.IsNullOrEmpty(left) then (if String.IsNullOrEmpty(right) then 0 else -1) else 1
        else
            match dataType with
            | Schema.Text -> StringComparer.Ordinal.Compare(left, right)
            | Schema.Int16 -> compare (int16 left) (int16 right)
            | Schema.Int32 -> compare (integer left) (integer right)
            | Schema.Int64 -> compare (integer64 left) (integer64 right)
            | Schema.Float64 -> compare (number left) (number right)
            | Schema.Boolean -> compare (Boolean.Parse left) (Boolean.Parse right)
            | Schema.Date -> compare (date left) (date right)

    let private externallySorted (storage: JrUtil.RegionalOverlay.Scratch.Storage)
                                 (relation: JrUtil.Serving.Schema.Relation) (rows: seq<IDictionary<string, obj>>) =
        let indexes = relation.fields |> Array.mapi (fun index (field: JrUtil.Serving.Schema.Field) -> field.name, (index, field)) |> dict
        let compareRows (left: string array) (right: string array) =
            let mutable result, index = 0, 0
            while result = 0 && index < relation.sortKey.Length do
                let fieldIndex, field = indexes.[relation.sortKey.[index]]
                result <- compareField field.dataType left.[fieldIndex] right.[fieldIndex]
                index <- index + 1
            result
        let serialized = rows |> Seq.map (fun row -> relation.fields |> Array.map (fun (field: JrUtil.Serving.Schema.Field) -> fieldText row.[field.name]))
        let sortedRows = JrUtil.RegionalOverlay.Scratch.sortRows storage compareRows JrUtil.RegionalOverlay.Scratch.defaultBufferBytes serialized
        seq {
            let mutable previousKey: string array option = None
            let mutable previousRow: string array option = None
            for row in sortedRows do
                let key = relation.primaryKey |> Array.map (fun name -> row.[fst indexes.[name]])
                match previousKey, previousRow with
                | Some priorKey, Some prior when priorKey = key ->
                    if prior <> row then
                        let keyText = String.concat "/" key
                        invalidOp $"Relation {relation.name} contains conflicting rows for primary key {keyText}"
                | _ ->
                    previousKey <- Some key
                    previousRow <- Some row
                    yield objectRow (Array.map2 (fun (field: JrUtil.Serving.Schema.Field) value -> field.name, parseField field value) relation.fields row)
        }

    let private gtfsRelations legacy gtfs (tripCallSummaries: IDictionary<string, TripCallSummary>)
                              (nonContiguousTripSequences: IDictionary<string, HashSet<int>>) =
        let agencies = csv gtfs "agency.txt" |> Seq.map (fun row -> objectRow [
            "agency_id", box (value "agency_id" row); "name", box (value "agency_name" row)
            "url", nullableString (value "agency_url" row); "timezone", box (value "agency_timezone" row)
            "language", nullableString (value "agency_lang" row); "phone", nullableString (value "agency_phone" row)
            "fare_url", nullableString (value "agency_fare_url" row); "email", nullableString (value "agency_email" row) ])
        let stopMetadata =
            let path = Path.Combine(legacy, "source_stop_metadata.parquet")
            if File.Exists(path) then
                PackageReader.readTextRows path [| "gtfs_stop_id"; "town"; "district"; "nearby_place"; "country"; "coordinate_precision" |]
                |> Seq.map (fun row -> row.[0], row) |> dict
            else dict []
        let locations = csv gtfs "stops.txt" |> Seq.map (fun row ->
            let locationType = value "location_type" row
            let parent = value "parent_station" row
            let kind = if locationType = "1" then "stop_place" elif not (String.IsNullOrEmpty(parent)) then "boarding_point" else "stop_place"
            let metadata = match stopMetadata.TryGetValue(value "stop_id" row) with | true, result -> Some result | _ -> None
            objectRow [
                "location_id", box (value "stop_id" row); "kind", box kind; "domain", box "scheduled"
                "parent_location_id", nullableString parent; "name", box (value "stop_name" row)
                "public_code", nullableString (value "stop_code" row); "description", nullableString (value "stop_desc" row)
                "municipality_name", metadata |> Option.map (fun value -> nullableString value.[1]) |> Option.defaultValue null
                "district_name", metadata |> Option.map (fun value -> nullableString value.[2]) |> Option.defaultValue null; "district_code", null
                "nearby_place", metadata |> Option.map (fun value -> nullableString value.[3]) |> Option.defaultValue null
                "country_code", metadata |> Option.map (fun value -> nullableString value.[4]) |> Option.defaultValue null
                "coordinate_precision", metadata |> Option.map (fun value -> nullableString value.[5]) |> Option.defaultValue null
                "longitude", nullableParsed number (value "stop_lon" row); "latitude", nullableParsed number (value "stop_lat" row)
                "url", nullableString (value "stop_url" row); "timezone", nullableString (value "stop_timezone" row)
                "wheelchair_boarding", nullableParsed int16 (value "wheelchair_boarding" row) ])
        let routeType value = integer value
        let mode value =
            match routeType value with
            | 0 | 900 -> "tram"
            | 1 -> "metro"
            | 2 | 100 | 101 | 102 | 103 | 106 | 109 | 202 | 401 -> "rail"
            | 4 | 1000 -> "water"
            | 5 | 6 | 7 | 1300 -> "cable"
            | 11 | 800 -> "trolleybus"
            | _ -> "bus"
        let routes = csv gtfs "routes.txt" |> Seq.map (fun row -> objectRow [
            "route_id", box (value "route_id" row); "agency_id", box (value "agency_id" row)
            "mode", box (mode (value "route_type" row)); "gtfs_route_type", box (routeType (value "route_type" row))
            "short_name", nullableString (value "route_short_name" row); "long_name", nullableString (value "route_long_name" row)
            "description", nullableString (value "route_desc" row); "url", nullableString (value "route_url" row)
            "color", nullableString (value "route_color" row); "text_color", nullableString (value "route_text_color" row)
            "sort_order", nullableParsed integer (value "route_sort_order" row) ])
        let calendarRows = csv gtfs "calendar.txt" |> Seq.map (fun row ->
            let mask =
                [| "monday"; "tuesday"; "wednesday"; "thursday"; "friday"; "saturday"; "sunday" |]
                |> Array.mapi (fun index name -> if value name row = "1" then 1 <<< index else 0)
                |> Array.sum
            objectRow [ "service_id", box (value "service_id" row); "valid_from", box (date (value "start_date" row)); "valid_to", box (date (value "end_date" row)); "weekday_mask", box (Convert.ToInt16(mask)) ])
        let existingServices = calendarRows |> Seq.map (fun row -> row.["service_id"] :?> string) |> Set.ofSeq
        let exceptionCalendars =
            seq {
                let bounds = Dictionary<string, struct(DateOnly * DateOnly)>(StringComparer.Ordinal)
                for row in csvValues gtfs "calendar_dates.txt" [| "service_id"; "date" |] do
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
        let exceptions = csv gtfs "calendar_dates.txt" |> Seq.map (fun row -> objectRow [
            "service_id", box (value "service_id" row); "service_date", box (date (value "date" row)); "added", box (value "exception_type" row = "1") ])
        let shapeIds = csv gtfs "shapes.txt" |> Seq.map (value "shape_id") |> Seq.distinct |> Seq.map (fun id -> objectRow [ "shape_id", box id; "generation_method", box "source_or_compiler" ])
        let shapePoints = csv gtfs "shapes.txt" |> Seq.map (fun row -> objectRow [
            "shape_id", box (value "shape_id" row); "sequence", box (integer (value "shape_pt_sequence" row))
            "longitude", box (number (value "shape_pt_lon" row)); "latitude", box (number (value "shape_pt_lat" row))
            "distance_traveled", nullableParsed number (value "shape_dist_traveled" row) ])
        let trips = csv gtfs "trips.txt" |> Seq.map (fun row -> objectRow [
            "trip_id", box (value "trip_id" row); "route_id", box (value "route_id" row); "service_id", box (value "service_id" row)
            "direction", nullableParsed int16 (value "direction_id" row); "headsign", nullableString (value "trip_headsign" row)
            "short_name", nullableString (value "trip_short_name" row); "block_key", nullableString (value "block_id" row)
            "wheelchair_accessible", nullableParsed int16 (value "wheelchair_accessible" row); "bikes_allowed", nullableParsed int16 (value "bikes_allowed" row)
            "shape_id", nullableString (value "shape_id" row) ])
        let parentByStop = csv gtfs "stops.txt" |> Seq.map (fun row -> value "stop_id" row, value "parent_station" row) |> dict
        let jdfCallMetadata = Path.Combine(legacy, "source_call_metadata.parquet")
        let hasJdfCallMetadata =
            File.Exists(jdfCallMetadata)
            && (File.Exists(Path.Combine(legacy, "source_route_stop_zone_metadata.parquet"))
                || File.Exists(Path.Combine(legacy, "source_notice_metadata.parquet")))
        let routeByTrip =
            if hasJdfCallMetadata then
                csv gtfs "trips.txt" |> Seq.map (fun row -> value "trip_id" row, value "route_id" row) |> dict
            else dict []
        let metadataRows =
            if hasJdfCallMetadata then
                PackageReader.readTextRows jdfCallMetadata
                    [| "gtfs_trip_id"; "stop_sequence"; "source_route_stop_id"; "gtfs_stop_id" |]
            else Seq.empty
        let pairedCalls = seq {
            use calls = (csv gtfs "stop_times.txt").GetEnumerator()
            use metadata = metadataRows.GetEnumerator()
            let mutable reading = true
            while reading do
                match calls.MoveNext(), metadata.MoveNext() with
                | false, false -> reading <- false
                | true, false when not hasJdfCallMetadata -> yield calls.Current, None
                | true, true -> yield calls.Current, Some metadata.Current
                | _ -> invalidOp "GTFS stop_times and JDF call metadata row counts differ"
        }
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
            for row, metadata in pairedCalls do
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
                let routeStop =
                    match metadata with
                    | None -> null
                    | Some fact ->
                        let stopSequence = value "stop_sequence" row
                        if fact.[0] <> tripId
                           || integer fact.[1] <> integer stopSequence
                           || fact.[3] <> stopId then
                            invalidOp $"GTFS stop_times and JDF call metadata ordering differs at {tripId}/{stopSequence}"
                        box (Identity.compositeKey [ routeByTrip.[fact.[0]]; fact.[2] ])
                yield objectRow [
                    "trip_id", box tripId; "sequence", box sequence; "location_id", box location
                    "passenger_service", box true; "boarding_point_id", boarding; "route_stop_id", routeStop
                    "scheduled_arrival", seconds (value "arrival_time" row); "scheduled_departure", seconds (value "departure_time" row); "scheduled_passage", null
                    "pickup_type", box (if String.IsNullOrEmpty(value "pickup_type" row) then 0s else int16 (value "pickup_type" row))
                    "dropoff_type", box (if String.IsNullOrEmpty(value "drop_off_type" row) then 0s else int16 (value "drop_off_type" row))
                    "timepoint", box (value "timepoint" row <> "0"); "stop_headsign", nullableString (value "stop_headsign" row)
                    "shape_distance_traveled", nullableParsed number (value "shape_dist_traveled" row) ]
            finish ()
        }
        let transferRows = csv gtfs "transfers.txt" |> Seq.map (fun row ->
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

    let private writeOrderedTripCalls progress legacy gtfs path
                                      (tripCallSummaries: IDictionary<string, TripCallSummary>)
                                      (nonContiguousTripSequences: IDictionary<string, HashSet<int>>)
                                      (wantedTargetTrips: HashSet<string>)
                                      (targetCallSchedules: IDictionary<string, TargetCallSchedule>) =
        let columns = [|
            "trip_id"; "arrival_time"; "departure_time"; "stop_id"; "stop_sequence"
            "pickup_type"; "drop_off_type"; "timepoint"; "stop_headsign"; "shape_dist_traveled"
        |]
        let parentByStop =
            csvValues gtfs "stops.txt" [| "stop_id"; "parent_station" |]
            |> Seq.map (fun row -> row.[0], row.[1]) |> dict
        let metadataPath = Path.Combine(legacy, "source_call_metadata.parquet")
        let hasMetadata =
            File.Exists(metadataPath)
            && (File.Exists(Path.Combine(legacy, "source_route_stop_zone_metadata.parquet"))
                || File.Exists(Path.Combine(legacy, "source_notice_metadata.parquet")))
        let routeByTrip =
            if hasMetadata then
                csvValues gtfs "trips.txt" [| "trip_id"; "route_id" |]
                |> Seq.map (fun row -> row.[0], row.[1]) |> dict
            else dict []
        let metadataRows =
            if hasMetadata then
                PackageReader.readTextRows metadataPath
                    [| "gtfs_trip_id"; "stop_sequence"; "source_route_stop_id"; "gtfs_stop_id" |]
            else Seq.empty
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
        use metadata = metadataRows.GetEnumerator()
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
        for row in fastCsvValues gtfs "stop_times.txt" columns do
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
            let routeStop =
                if not hasMetadata then null else
                if not (metadata.MoveNext()) then invalidOp "GTFS stop_times and JDF call metadata row counts differ"
                let fact = metadata.Current
                if fact.[0] <> tripId || integer fact.[1] <> sequence || fact.[3] <> stopId then
                    invalidOp $"GTFS stop_times and JDF call metadata ordering differs at {tripId}/{sequence}"
                Identity.compositeKey [ routeByTrip.[tripId]; fact.[2] ]
            let arrival, departure = parsedSeconds row.[1], parsedSeconds row.[2]
            if wantedTargetTrips.Contains(tripId) then targetTimes.Add(struct(sequence, arrival, departure))
            output.Append({
                tripId = tripId; sequence = sequence; locationId = location; passengerService = true
                boardingPointId = boarding; routeStopId = routeStop
                arrival = arrival; departure = departure; passage = Nullable()
                pickup = if String.IsNullOrEmpty(row.[5]) then 0s else int16 row.[5]
                dropoff = if String.IsNullOrEmpty(row.[6]) then 0s else int16 row.[6]
                timepoint = row.[7] <> "0"; headsign = if String.IsNullOrWhiteSpace(row.[8]) then null else row.[8]
                distance = parsedNumber row.[9] })
            count <- count + 1L
            if count % 100000L = 0L then progress count
        finish ()
        if hasMetadata && metadata.MoveNext() then invalidOp "GTFS stop_times and JDF call metadata row counts differ"
        let written = output.Complete()
        progress written
        int written

    let private writeOrderedShapes progress gtfs serving =
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
        for row in csvValues gtfs "shapes.txt"
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

    let private writeOrderedTrips progress gtfs path =
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "trip")
        let optionalText value = if String.IsNullOrWhiteSpace(value) then null else value
        let optionalInt16 value = if String.IsNullOrWhiteSpace(value) then Nullable() else Nullable(int16 value)
        let rows =
            csvValues gtfs "trips.txt"
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
        RelationWriter.write path schema (128L * 1024L * 1024L) 524288
            (16L * 1024L * 1024L) 65536 CancellationToken.None (fun _ count -> progress count)
            size (fun left right -> StringComparer.Ordinal.Compare(left.tripId, right.tripId)) (=)
            encode decode columns rows

    let private serviceBounds gtfs =
        let result = Dictionary<string, DateOnly * DateOnly>(StringComparer.Ordinal)
        for row in csv gtfs "calendar.txt" do
            result.[value "service_id" row] <- date (value "start_date" row), date (value "end_date" row)
        for row in csvValues gtfs "calendar_dates.txt" [| "service_id"; "date" |] do
            let service, day = row.[0], date row.[1]
            match result.TryGetValue(service) with
            | true, (first, last) -> result.[service] <- min first day, max last day
            | _ -> result.[service] <- day, day
        result

    let private summarizeTripCalls gtfs =
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
        for row in csv gtfs "stop_times.txt" do
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

    let private defaultSourceId (manifest: JsonElement) (snapshots: IDictionary<string, string>) =
        match snapshots.Keys |> Seq.sort |> Seq.tryHead with
        | Some value -> value
        | None when manifest.TryGetProperty("source_format") |> fst
                    && manifest.GetProperty("source_format").GetString() = "czptt" -> "national-czptt"
        | None -> "unknown-source"

    let private bindingRows (nativeCalls: JrUtil.Serving.Model.NativeCallArtifacts option) legacy gtfs (manifest: JsonElement)
                            (targetCalls: IDictionary<string, TripCallSummary>)
                            (nonContiguousTripSequences: IDictionary<string, HashSet<int>>)
                            (targetCallSchedules: IDictionary<string, TargetCallSchedule>) =
        let stringPool = Dictionary<string,string>(StringComparer.Ordinal)
        let intern (value: string) =
            if isNull value then null else
            match stringPool.TryGetValue(value) with
            | true, existing -> existing
            | _ -> stringPool.Add(value, value); value
        let targetTrips =
            csvValues gtfs "trips.txt" [| "trip_id"; "service_id"; "direction_id"; "block_id" |]
            |> Seq.map (fun row -> intern row.[0], struct(intern row.[1], intern row.[2], intern row.[3])) |> dict
        let mappings =
            let mappingRoot = Path.Combine(legacy, "mappings")
            let mappingPath = Path.Combine(mappingRoot, "source_to_output_trips.csv")
            let rows =
                if File.Exists(mappingPath) then
                    csvValues mappingRoot "source_to_output_trips.csv"
                        [| "source_id"; "source_trip_id"; "output_trip_id"; "valid_from"; "valid_to"; "method" |]
                else Seq.empty
            rows
            |> Seq.map (fun row -> {
                sourceId = intern row.[0]; sourceTripId = intern row.[1]; outputTripId = intern row.[2]
                validFrom = intern row.[3]; validTo = intern row.[4]; methodName = intern row.[5] })
            |> Seq.toArray
        let mappingsBySourceRange =
            mappings
            |> Seq.groupBy (fun row ->
                struct(row.sourceId, row.sourceTripId, row.validFrom, row.validTo))
            |> Seq.map (fun (key, rows) -> key, rows |> Seq.toArray)
            |> dict
        let bounds = serviceBounds gtfs
        let snapshots = combinedSourceManifest legacy manifest
        let defaultSource = defaultSourceId manifest snapshots
        let binding sourceId sourceTrip target first last variant : TripBinding =
                let sourceId, sourceTrip, target = intern sourceId, intern sourceTrip, intern target
                let first, last, variant = intern first, intern last, intern variant
                let struct(service, direction, block) = targetTrips.[target]
                let calls = targetCalls.[target]
                let fields = [ "source_id", sourceId; "namespace", "gtfs_trip_id"; "source_trip_id", sourceTrip; "trip_id", target; "service_id", service; "valid_from", first; "valid_to", last ]
                let bindingId = Identity.bindingId "trip" fields
                { binding_id = bindingId; source_id = sourceId; trip_namespace = "gtfs_trip_id"; source_trip_id = sourceTrip
                  trip_id = target; service_id = service; valid_from = date first; valid_to = date last
                  binding_status = "confirmed"; scheduled_start = calls.scheduledStart; scheduled_end = calls.scheduledEnd
                  source_route_id = null; source_direction_id = direction; source_start_location_id = intern calls.firstStopId
                  source_end_location_id = intern calls.lastStopId; source_block_id = block; source_run_id = null; source_duty_id = null
                  call_pattern_sha256 = intern calls.callPatternSha256; variant_key = variant }
        let compilerBindings =
            if mappings.Length > 0 then
                mappings |> Seq.map (fun row ->
                    binding row.sourceId row.sourceTripId row.outputTripId row.validFrom row.validTo row.methodName)
            else
                csv gtfs "trips.txt" |> Seq.map (fun row ->
                    let trip, service = value "trip_id" row, value "service_id" row
                    let first, last = bounds.[service]
                    binding defaultSource trip trip (first.ToString("yyyyMMdd")) (last.ToString("yyyyMMdd")) "source_native")
            |> Seq.toArray
        let basePackage = tryBasePackage legacy
        let baseSlices =
            let mappingRoot = Path.Combine(legacy, "mappings")
            let mappingPath = Path.Combine(mappingRoot, "base_to_output_trips.csv")
            let rows =
                if File.Exists(mappingPath) then
                    csvValues mappingRoot "base_to_output_trips.csv"
                        [| "base_trip_id"; "output_trip_id"; "valid_from"; "valid_to" |]
                else Seq.empty
            rows
            |> Seq.map (fun row -> {
                baseTripId = row.[0]; outputTripId = row.[1]; validFrom = row.[2]; validTo = row.[3] })
            |> Seq.groupBy _.baseTripId
            |> Seq.map (fun (trip, rows) -> trip, rows |> Seq.toArray)
            |> dict
        let baseBindingPairs =
            match basePackage with
            | None -> [||]
            | Some package when not (File.Exists(Path.Combine(package, "serving", "source_trip_map.parquet"))) -> [||]
            | Some package ->
                PackageReader.readTextRows (Path.Combine(package, "serving", "source_trip_map.parquet"))
                    [| "binding_id"; "source_id"; "trip_namespace"; "source_trip_id"; "trip_id"; "valid_from"; "valid_to"; "binding_status"; "source_route_id"; "source_direction_id"; "source_start_location_id"; "source_end_location_id"; "source_block_id"; "source_run_id"; "source_duty_id"; "variant_key" |]
                |> Seq.collect (fun row ->
                    match baseSlices.TryGetValue(row.[4]) with
                    | false, _ -> Seq.empty
                    | true, slices -> slices |> Seq.choose (fun slice ->
                        let first = max (date row.[5]) (date slice.validFrom)
                        let last = min (date row.[6]) (date slice.validTo)
                        if first > last then None else
                        let target = slice.outputTripId
                        let result = binding row.[1] row.[3] target (first.ToString("yyyyMMdd")) (last.ToString("yyyyMMdd")) row.[15]
                        let fields = [ "source_id", row.[1]; "namespace", row.[2]; "source_trip_id", row.[3]; "trip_id", target; "service_id", result.service_id; "valid_from", first.ToString("yyyyMMdd"); "valid_to", last.ToString("yyyyMMdd") ]
                        let result = { result with
                                        trip_namespace = row.[2]; binding_status = row.[7]
                                        source_route_id = row.[8]; source_direction_id = row.[9]
                                        source_start_location_id = row.[10]; source_end_location_id = row.[11]
                                        source_block_id = row.[12]; source_run_id = row.[13]; source_duty_id = row.[14]
                                        binding_id = Identity.bindingId "trip" fields }
                        Some (row.[0], result)))
                |> Seq.toArray
        let primaryBindings =
            Seq.append compilerBindings (baseBindingPairs |> Seq.map snd)
            |> Seq.distinctBy (fun row -> row.binding_id)
            |> Seq.toArray
        let primaryByTarget = lazy (
            let result = Dictionary<string, TripBinding>(StringComparer.Ordinal)
            for row in primaryBindings do result.TryAdd(row.trip_id, row) |> ignore
            result)
        let czpttBindings =
            csv (Path.Combine(legacy, "extensions")) "cz_trips.txt"
            |> Seq.collect (fun row ->
                let target = value "trip_id" row
                match targetTrips.TryGetValue(target) with
                | false, _ -> Seq.empty
                | true, struct(service, _, _) ->
                    let first, last = bounds.[service]
                    (value "source_trip_ids" row).Split(
                        '|', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                    |> Seq.choose (fun token ->
                        let separator = token.IndexOf('=')
                        if separator <= 0 || separator = token.Length - 1 then None else
                        let prefix, identifier = token.Substring(0, separator), token.Substring(separator + 1)
                        let namespaceName = if prefix = "PA" then "czptt_pa_id" elif prefix = "TR" then "czptt_tr_id" else ""
                        if namespaceName = "" then None else
                        let fields = [ "source_id", defaultSource; "namespace", namespaceName; "source_trip_id", identifier; "trip_id", target; "service_id", service; "valid_from", first.ToString("yyyyMMdd"); "valid_to", last.ToString("yyyyMMdd") ]
                        let context = primaryByTarget.Value.[target]
                        Some { context with
                                binding_id = Identity.bindingId "trip" fields; source_id = defaultSource
                                trip_namespace = namespaceName; source_trip_id = identifier; trip_id = target; service_id = service
                                valid_from = first; valid_to = last; binding_status = "confirmed"
                                source_route_id = null; source_run_id = null; source_duty_id = null; variant_key = "czptt-source-identity-v1" }))
        let bindings = Seq.append primaryBindings czpttBindings |> Seq.toArray
        let bindingByMapping = lazy (
            bindings |> Seq.map (fun row ->
                let key = row.source_id, row.source_trip_id, row.trip_id
                key, row) |> Seq.groupBy fst |> Seq.map (fun (key, values) -> key, values |> Seq.map snd |> Seq.toArray) |> dict)
        let compilerCalls =
            if nativeCalls.IsSome then Seq.empty
            elif mappings.Length > 0 then
                csvValues (Path.Combine(legacy, "mappings")) "source_to_output_calls.csv"
                    [| "source_id"; "source_trip_id"; "output_trip_id"; "source_call_ordinal"; "output_call_ordinal"; "source_stop_id" |]
                |> Seq.collect (fun row ->
                    let key = row.[0], row.[1], row.[2]
                    match bindingByMapping.Value.TryGetValue(key) with
                    | false, _ -> Seq.empty
                    | true, matching ->
                        let sequence = integer row.[4]
                        let schedule = targetCallSchedules.[row.[2]]
                        let offset = sequence - schedule.firstSequence
                        let direct =
                            if offset >= 0 && offset < schedule.calls.Length then Some schedule.calls.[offset]
                            else None
                        let binaryFind () =
                            let mutable low, high = 0, schedule.calls.Length - 1
                            let mutable result = None
                            while result.IsNone && low <= high do
                                let middle = low + (high - low) / 2
                                let struct(candidate, _, _) as call = schedule.calls.[middle]
                                if candidate = sequence then result <- Some call
                                elif candidate < sequence then low <- middle + 1
                                else high <- middle - 1
                            result
                        let matched =
                            match direct with
                            | Some (struct(candidate, _, _) as call) when candidate = sequence -> Some call
                            | _ -> binaryFind ()
                        let arrival, departure =
                            match matched with
                            | Some struct(_, arrival, departure) -> arrival, departure
                            | None -> Nullable(), Nullable()
                        matching |> Seq.map (fun binding ->
                            ({ binding = binding.binding_id;
                              callNamespace = "gtfs_stop_sequence"; sourceSequence = row.[3]; sequence = sequence;
                              stop = row.[5]; arrival = arrival; departure = departure } : SourceCallWriter.MappedRow)))
            else
                let bindingByTrip = bindings |> Seq.map (fun row -> row.trip_id, row) |> dict
                csv gtfs "stop_times.txt" |> Seq.map (fun row ->
                    let sequence = value "stop_sequence" row
                    let binding = bindingByTrip.[value "trip_id" row]
                    let optional value = let parsed = seconds value in if isNull parsed then Nullable() else Nullable(unbox<int> parsed)
                    ({ binding = binding.binding_id;
                      callNamespace = "gtfs_stop_sequence"; sourceSequence = sequence; sequence = integer sequence;
                      stop = value "stop_id" row; arrival = optional (value "arrival_time" row);
                      departure = optional (value "departure_time" row) } : SourceCallWriter.MappedRow))
        let baseBindingReplacements =
            baseBindingPairs |> Seq.groupBy fst |> Seq.map (fun (key, values) -> key, values |> Seq.map snd |> Seq.toArray) |> dict
        let baseCalls =
            match basePackage with
            | None -> Seq.empty
            | Some package when not (File.Exists(Path.Combine(package, "serving", "source_call_map.parquet"))) -> Seq.empty
            | Some package ->
                PackageReader.readTextRows (Path.Combine(package, "serving", "source_call_map.parquet"))
                    [| "binding_id"; "call_namespace"; "source_sequence"; "call_sequence"; "source_stop_id"; "scheduled_arrival"; "scheduled_departure" |]
                |> Seq.collect (fun row ->
                    match baseBindingReplacements.TryGetValue(row.[0]) with
                    | false, _ -> Seq.empty
                    | true, targetBindings -> targetBindings |> Seq.choose (fun targetBinding ->
                        let target = targetBinding.trip_id
                        let sequence = integer row.[3]
                        match targetCalls.TryGetValue(target) with
                        | false, _ -> None
                        | true, targetRows
                            when sequence >= targetRows.firstSequence && sequence <= targetRows.lastSequence
                                 && (match nonContiguousTripSequences.TryGetValue(target) with
                                     | true, sequences -> sequences.Contains(sequence)
                                     | _ -> true) ->
                            let optional value = if String.IsNullOrWhiteSpace(value) then Nullable() else Nullable(integer value)
                            Some ({ binding = targetBinding.binding_id; callNamespace = row.[1];
                                   sourceSequence = row.[2]; sequence = sequence; stop = row.[4];
                                   arrival = optional row.[5]; departure = optional row.[6] } : SourceCallWriter.MappedRow)
                        | _ -> None))
        let baseCoverage =
            match basePackage with
            | None -> Seq.empty
            | Some package when not (File.Exists(Path.Combine(package, "serving", "source_trip_coverage.parquet"))) -> Seq.empty
            | Some package ->
                PackageReader.readTextRows (Path.Combine(package, "serving", "source_trip_coverage.parquet"))
                    [| "binding_id"; "coverage_id"; "from_sequence"; "to_sequence"; "coverage_type"; "system_id"; "coverage_role" |]
                |> Seq.collect (fun row ->
                    match baseBindingReplacements.TryGetValue(row.[0]) with
                    | false, _ -> Seq.empty
                    | true, targetBindings -> targetBindings |> Seq.map (fun targetBinding -> objectRow [
                        "binding_id", box targetBinding.binding_id; "coverage_id", box row.[1]
                        "service_id", box targetBinding.service_id; "from_sequence", box (integer row.[2]); "to_sequence", box (integer row.[3])
                        "coverage_type", box row.[4]; "system_id", nullableString row.[5]; "coverage_role", nullableString row.[6] ]))
        let calls = Seq.append compilerCalls baseCalls
        let operational = csv (Path.Combine(legacy, "mappings")) "operational_to_source_trips.csv" |> Seq.collect (fun row ->
            let key =
                struct(value "source_id" row, value "source_trip_id" row,
                       value "valid_from" row, value "valid_to" row)
            match mappingsBySourceRange.TryGetValue(key) with
            | false, _ -> Seq.empty
            | true, candidates -> candidates |> Seq.map (fun mapping ->
                    let sourceId, target = value "source_id" row, mapping.outputTripId
                    let sourceTrip = Identity.compositeKey [ value "operational_line_id" row; value "operational_trip_id" row ]
                    let struct(service, _, _) = targetTrips.[target]
                    let fields = [ "source_id", sourceId; "namespace", "operational_line_course"; "source_trip_id", sourceTrip; "trip_id", target; "service_id", service; "valid_from", value "valid_from" row; "valid_to", value "valid_to" row ]
                    let context = primaryByTarget.Value.[target]
                    { context with
                        binding_id = Identity.bindingId "trip" fields; source_id = sourceId
                        trip_namespace = "operational_line_course"; source_trip_id = sourceTrip; trip_id = target; service_id = service
                        valid_from = date (value "valid_from" row); valid_to = date (value "valid_to" row)
                        binding_status = "confirmed"; source_route_id = null; source_run_id = null; source_duty_id = null
                        variant_key = "ids-jmk-api-v1" }) )
        Seq.append bindings operational, calls, baseCoverage

    let private identityRelations legacy gtfs (bindings: TripBinding array) (manifest: JsonElement) =
        let firstDate, lastDate =
            if bindings.Length = 0 then DateOnly(1970, 1, 1), DateOnly(1970, 1, 1)
            else
                bindings |> Array.map (fun row -> row.valid_from) |> Array.min,
                bindings |> Array.map (fun row -> row.valid_to) |> Array.max
        let locationKind =
            csv gtfs "stops.txt"
            |> Seq.map (fun row ->
                let kind = if value "location_type" row = "1" || String.IsNullOrEmpty(value "parent_station" row) then "stop_place" else "boarding_point"
                value "stop_id" row, kind)
            |> dict
        let snapshots = combinedSourceManifest legacy manifest
        let defaultSource = defaultSourceId manifest snapshots
        let entityRows kind namespaceName mappingName sourceColumn targetColumn =
            let mapped =
                csv (Path.Combine(legacy, "mappings")) mappingName
                |> Seq.map (fun row -> value "source_id" row, value sourceColumn row, value targetColumn row)
            let native =
                if tryBasePackage legacy |> Option.isSome then Seq.empty
                elif kind = "route" then
                    csv (Path.Combine(legacy, "extensions")) "cz_routes.txt"
                    |> Seq.map (fun row ->
                        let owner = value "source_provenance" row |> fun value -> if String.IsNullOrWhiteSpace(value) then defaultSource else value
                        owner, value "route_id" row, value "route_id" row)
                else
                    csv (Path.Combine(legacy, "extensions")) "cz_stops.txt"
                    |> Seq.map (fun row -> defaultSource, value "stop_id" row, value "stop_id" row)
            Seq.append mapped native |> Seq.distinct |> Seq.map (fun (sourceId, sourceObject, target) ->
                let effectiveKind = if kind = "location" then locationKind.[target] else kind
                let fields = [ "source_id", sourceId; "namespace", namespaceName; "kind", effectiveKind; "source_object_id", sourceObject; "public_id", target; "valid_from", firstDate.ToString("yyyyMMdd"); "valid_to", lastDate.ToString("yyyyMMdd") ]
                objectRow [
                    "entity_binding_id", box (Identity.bindingId "entity" fields); "source_id", box sourceId
                    "identifier_namespace", box namespaceName; "entity_kind", box effectiveKind
                    "source_object_id", box sourceObject; "public_id", box target
                    "valid_from", box firstDate; "valid_to", box lastDate ])
        let routeEntities = entityRows "route" "gtfs_route_id" "source_to_output_routes.csv" "source_route_id" "output_route_id"
        let stopEntities = entityRows "location" "gtfs_stop_id" "source_to_output_stops.csv" "source_stop_id" "output_stop_id"
        let entities = Seq.append routeEntities stopEntities |> Seq.toArray
        let routeBindingByTarget =
            entities |> Seq.filter (fun row -> row.["entity_kind"] :?> string = "route")
            |> Seq.groupBy (fun row -> row.["public_id"] :?> string)
            |> Seq.map (fun (key, values) -> key, values |> Seq.head) |> dict
        let routeKeys =
            csv (Path.Combine(legacy, "extensions")) "cz_routes.txt"
            |> Seq.choose (fun row ->
                let target, cis = value "route_id" row, value "cis_line_id" row
                match routeBindingByTarget.TryGetValue(target) with
                | true, binding when not (String.IsNullOrWhiteSpace(cis)) -> Some (objectRow [
                    "entity_binding_id", binding.["entity_binding_id"]; "cis_line_id", box cis; "route_id", box target
                    "valid_from", binding.["valid_from"]; "valid_to", binding.["valid_to"] ])
                | _ -> None)
        let tripKeys keyName keyType = seq {
            let tripBindingsByTarget = lazy (
                bindings |> Seq.groupBy (fun row -> row.trip_id)
                |> Seq.map (fun (key, values) -> key, values |> Seq.toArray) |> dict)
            yield! csv (Path.Combine(legacy, "extensions")) "cz_trips.txt"
            |> Seq.collect (fun row ->
                let target, key = value "trip_id" row, value keyName row
                if String.IsNullOrWhiteSpace(key) then Seq.empty else
                match tripBindingsByTarget.Value.TryGetValue(target) with
                | true, targetBindings when not (String.IsNullOrWhiteSpace(key)) ->
                    let preferredNamespace = if keyType = "rail" then "czptt_pa_id" else "gtfs_trip_id"
                    let preferred = targetBindings |> Array.filter (fun binding -> binding.trip_namespace = preferredNamespace)
                    (if preferred.Length > 0 then preferred else targetBindings) |> Seq.map (fun binding ->
                        if keyType = "road" then objectRow [
                            "binding_id", box binding.binding_id; "cis_line_id", box (value "cis_line_id" row)
                            "cis_trip_id", box (integer64 key); "trip_id", box target
                            "valid_from", box binding.valid_from; "valid_to", box binding.valid_to ]
                        else objectRow [
                            "binding_id", box binding.binding_id; "train_number", box key; "trip_id", box target
                            "valid_from", box binding.valid_from; "valid_to", box binding.valid_to ])
                | _ -> Seq.empty)
        }
        let evidence = bindings |> Seq.choose (fun binding ->
            let sourceId = binding.source_id
            match snapshots.TryGetValue(sourceId) with
            | true, digest -> Some (objectRow [
                "binding_kind", box "trip"; "binding_id", box binding.binding_id
                "evidence_source_id", box sourceId; "source_snapshot_sha256", box digest
                "identifier_namespace", box binding.trip_namespace; "source_object_id", box binding.source_trip_id
                "selection_rule", box (if binding.binding_status = "candidate" then "admissible_static_candidate" else "accepted_static_match") ])
            | _ -> None)
        let selected = seq {
            if File.Exists(Path.Combine(legacy, "provenance", "selected_fields.csv")) then
                let routeIds = csv gtfs "routes.txt" |> Seq.map (value "route_id") |> Set.ofSeq
                let tripIds = csv gtfs "trips.txt" |> Seq.map (value "trip_id") |> Set.ofSeq
                let locationIds = csv gtfs "stops.txt" |> Seq.map (value "stop_id") |> Set.ofSeq
                let sourceObjects = Dictionary<struct(string * string), string>()
                let registerObjects file sourceColumn outputColumn =
                    for row in csv (Path.Combine(legacy, "mappings")) file do
                        sourceObjects.[struct(value "source_id" row, value outputColumn row)] <- value sourceColumn row
                registerObjects "source_to_output_routes.csv" "source_route_id" "output_route_id"
                registerObjects "source_to_output_stops.csv" "source_stop_id" "output_stop_id"
                registerObjects "source_to_output_trips.csv" "source_trip_id" "output_trip_id"
                for row in csv (Path.Combine(legacy, "mappings")) "source_to_output_calls.csv" do
                    let outputObject = value "output_trip_id" row + "#" + value "output_call_ordinal" row
                    sourceObjects.[struct(value "source_id" row, outputObject)] <-
                        Identity.compositeKey [ value "source_trip_id" row; value "source_call_ordinal" row ]
                yield!
                    csv (Path.Combine(legacy, "provenance")) "selected_fields.csv"
                    |> Seq.collect (fun row ->
                        let sources = value "source_id" row |> fun value -> value.Split(';', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                        let outputObject = value "output_object_id" row
                        let objectType, objectKey =
                            let separator = outputObject.LastIndexOf('#')
                            if separator > 0 then "call", Identity.compositeKey [ outputObject.Substring(0, separator); outputObject.Substring(separator + 1) ]
                            elif routeIds.Contains(outputObject) then "route", Identity.compositeKey [ outputObject ]
                            elif tripIds.Contains(outputObject) then "trip", Identity.compositeKey [ outputObject ]
                            elif locationIds.Contains(outputObject) then "location", Identity.compositeKey [ outputObject ]
                            else "compiler_object", Identity.compositeKey [ outputObject ]
                        let fields =
                            if value "field" row = "stop_lat,stop_lon" then [| "latitude"; "longitude" |]
                            else [| value "field" row |]
                        Seq.allPairs sources fields |> Seq.choose (fun (sourceId, fieldName) ->
                            match snapshots.TryGetValue(sourceId) with
                            | true, digest -> Some (objectRow [
                                "object_type", box objectType; "object_key", box objectKey
                                "field_name", box fieldName; "source_id", box sourceId; "source_snapshot_sha256", box digest
                                "source_object_id", box (match sourceObjects.TryGetValue(struct(sourceId, outputObject)) with | true, sourceObject -> sourceObject | _ -> outputObject)
                                "selection_rule", box (value "capability_mode" row) ])
                            | _ -> None))
        }
        let origins = seq {
            let rows kind namespaceName table idColumn =
                csv gtfs table |> Seq.map (fun row ->
                    let id = value idColumn row
                    objectRow [
                        "object_type", box kind; "object_key", box (Identity.compositeKey [ id ])
                        "source_id", box defaultSource; "source_snapshot_sha256", box (match snapshots.TryGetValue(defaultSource) with | true, value -> value | _ -> String.replicate 64 "0")
                        "identifier_namespace", box namespaceName; "source_object_id", box id; "selection_rule", box "source_or_compiler_origin" ])
            yield! rows "route" "gtfs_route_id" "routes.txt" "route_id"
            yield! rows "location" "gtfs_stop_id" "stops.txt" "stop_id"
            yield! rows "trip" "gtfs_trip_id" "trips.txt" "trip_id"
            yield! rows "shape" "gtfs_shape_id" "shapes.txt" "shape_id" |> Seq.distinctBy (fun row -> row.["object_key"])
            for transfer in csv gtfs "transfers.txt" do
                let selectors = [ "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" ]
                let key = selectors |> Seq.map (fun name -> value name transfer) |> Identity.compositeKey
                yield objectRow [
                    "object_type", box "transfer"; "object_key", box key; "source_id", box defaultSource
                    "source_snapshot_sha256", box (match snapshots.TryGetValue(defaultSource) with | true, value -> value | _ -> String.replicate 64 "0")
                    "identifier_namespace", box "gtfs_transfer_selectors"; "source_object_id", box key; "selection_rule", box "source_or_compiler_origin" ]
        }
        Map [
            "source_entity_map", entities :> seq<_>
            "road_route_key", routeKeys
            "road_trip_key", tripKeys "cis_trip_id" "road"
            "rail_trip_key", tripKeys "train_number" "rail"
            "binding_evidence", evidence
            "selected_field_provenance", selected
            "object_origin", origins
        ]

    let private semanticRelations (nativeCalls: JrUtil.Serving.Model.NativeCallArtifacts option) legacy gtfs (manifest: JsonElement) =
        let read name columns =
            let path = Path.Combine(legacy, name)
            if File.Exists(path) then PackageReader.readTextRows path columns else Seq.empty
        let snapshots = combinedSourceManifest legacy manifest
        let sourceId = defaultSourceId manifest snapshots
        let digest = match snapshots.TryGetValue(sourceId) with | true, value -> value | _ -> String.replicate 64 "0"
        let callFacts () =
            read "source_call_metadata.parquet"
                [| "gtfs_trip_id"; "stop_sequence"; "source_route_stop_id"; "gtfs_stop_id" |]
        let routeStops = seq {
            let routeByTrip = csvValues gtfs "trips.txt" [| "trip_id"; "route_id" |] |> Seq.map (fun row -> row.[0], row.[1]) |> dict
            for row in callFacts () do
                match routeByTrip.TryGetValue(row.[0]) with
                | true, route ->
                    yield objectRow [
                        "route_id", box route; "route_stop_id", box (Identity.compositeKey [ route; row.[2] ]); "location_id", box row.[3] ]
                | _ -> ()
        }
        let routeStopZonesRaw =
            read "source_route_stop_zone_metadata.parquet" [| "gtfs_route_id"; "source_route_stop_id"; "zone_id"; "zone_order" |]
            |> Seq.toArray
        let routeStopZones = routeStopZonesRaw |> Seq.map (fun row -> objectRow [
            "route_id", box row.[0]; "route_stop_id", box (Identity.compositeKey [ row.[0]; row.[1] ])
            "zone_id", box row.[2]; "source_order", box (integer row.[3]) ])
        let semanticZones = routeStopZonesRaw |> Seq.map (fun row -> objectRow [
            "zone_id", box row.[2]; "fare_system_id", null; "zone_code", box row.[2]; "name", null
            "source_id", box sourceId; "source_scope", box "route_stop" ])
        let notesRaw () =
            read "source_notice_metadata.parquet"
                [| "source_notice_id"; "notice_kind"; "gtfs_route_id"; "gtfs_trip_id"; "label"; "text"; "valid_from"; "valid_to"; "service_note_type" |]
        let notes = notesRaw () |> Seq.map (fun row -> objectRow [
            "note_id", box row.[0]; "kind", box row.[1]; "label", nullableString row.[4]; "text", nullableString row.[5]
            "valid_from", nullableParsed date row.[6]; "valid_to", nullableParsed date row.[7]; "service_note_type", nullableString row.[8]
            "source_id", box sourceId; "source_snapshot_sha256", box digest; "source_object_id", box row.[0] ])
        let noteAssignments = notesRaw () |> Seq.choose (fun row ->
            let scope, route, trip =
                if not (String.IsNullOrEmpty row.[3]) then "trip", null, box row.[3]
                elif not (String.IsNullOrEmpty row.[2]) then "route", box row.[2], null
                else "source", null, null
            Some (objectRow [
                "assignment_id", box (Identity.bindingId "note-assignment" [ "note", row.[0]; "scope", scope; "route", row.[2]; "trip", row.[3] ])
                "note_id", box row.[0]; "scope", box scope; "route_id", route; "trip_id", trip; "service_id", null ]))
        let features =
            read "source_trip_feature_metadata.parquet" [| "gtfs_trip_id"; "source_code"; "feature_kind"; "source_object_id" |]
            |> Seq.map (fun row -> objectRow [
                "feature_id", box (Identity.bindingId "service-feature" [ "trip", row.[0]; "code", row.[1]; "source", sourceId ])
                "scope", box "trip"; "kind", box row.[2]; "route_id", null; "trip_id", box row.[0]
                "call_sequence", null; "service_id", null; "source_code", box row.[1]; "note_id", null
                "source_id", box sourceId; "source_snapshot_sha256", box digest; "source_object_id", box row.[3] ])
        let locationFeatures =
            read "source_location_feature_metadata.parquet" [| "gtfs_stop_id"; "source_code"; "feature_kind"; "source_object_id" |]
            |> Seq.map (fun row -> objectRow [
                "feature_id", box (Identity.bindingId "location-feature" [ "location", row.[0]; "code", row.[1]; "source", sourceId ])
                "location_id", box row.[0]; "kind", box row.[2]; "source_code", box row.[1]
                "source_id", box sourceId; "source_snapshot_sha256", box digest; "source_object_id", box row.[3] ])
        let transferFacts =
            read "source_transfer_metadata.parquet"
                [| "source_transfer_id"; "gtfs_trip_id"; "source_route_stop_id"; "transfer_type"; "transfer_route_id"; "transfer_stop_id"; "transfer_stop_post_id"; "transfer_end_stop_id"; "transfer_end_stop_post_id"; "wait_minutes"; "note" |]
            |> Seq.toArray
        let transferCalls =
            let wanted =
                transferFacts
                |> Seq.map (fun row -> struct(row.[1], row.[2]))
                |> HashSet
            let result = Dictionary<struct(string * string), int>()
            match nativeCalls with
            | Some native ->
                for struct(trip, routeStop) as key in wanted do
                    match native.transferSequences.TryGetValue(struct(trip, integer64 routeStop)) with
                    | true, sequence -> result.Add(key, sequence)
                    | _ -> ()
            | None ->
                for call in (if wanted.Count = 0 then Seq.empty else callFacts ()) do
                    let key = struct(call.[0], call.[2])
                    if wanted.Contains(key) && not (result.ContainsKey(key)) then
                        result.Add(key, integer call.[1])
            result
        let transfers =
            transferFacts |> Seq.map (fun row ->
                let key = struct(row.[1], row.[2])
                let sequence = match transferCalls.TryGetValue(key) with | true, value -> value | _ -> 0
                objectRow [
                    "connection_id", box row.[0]; "direction", box row.[3]; "origin_trip_id", box row.[1]; "origin_sequence", box sequence
                    "service_id", null; "target_source_route_id", nullableString row.[4]; "target_source_trip_id", null
                    "target_source_stop_id", nullableString row.[5]; "target_source_post_id", nullableString row.[6]
                    "target_source_end_stop_id", nullableString row.[7]; "target_source_end_post_id", nullableString row.[8]
                    "wait_minutes", nullableParsed integer row.[9]; "note", nullableString row.[10]; "target_public_line", null; "target_destination_text", null
                    "target_derivation", box "jdf_structured"; "resolution_status", box "unresolved"; "target_route_id", null; "target_trip_id", null; "target_location_id", null
                    "source_id", box sourceId; "source_snapshot_sha256", box digest; "source_object_id", box row.[0] ])
        let restrictions =
            read "source_travel_restriction_metadata.parquet"
                [| "assignment_scope"; "gtfs_route_id"; "gtfs_trip_id"; "source_route_stop_id"; "group_code" |]
            |> Seq.map (fun row ->
                let routeStop = if String.IsNullOrEmpty row.[1] then null else box (Identity.compositeKey [ row.[1]; row.[3] ])
                objectRow [
                    "assignment_id", box (Identity.bindingId "restriction" [ "scope", row.[0]; "route", row.[1]; "trip", row.[2]; "route_stop", row.[3]; "group", row.[4] ])
                    "scope", box row.[0]; "route_id", nullableString row.[1]; "trip_id", nullableString row.[2]
                    "source_route_stop_id", box row.[3]; "route_stop_id", routeStop; "call_sequence", null; "service_id", null
                    "group_code", box row.[4]; "source_id", box sourceId; "source_snapshot_sha256", box digest
                    "source_object_id", box (Identity.compositeKey [ row.[0]; row.[1]; row.[2]; row.[3]; row.[4] ]) ])
        Map [
            "route_stop", routeStops
            "route_stop_zone", routeStopZones
            "fare_zone", semanticZones
            "service_note", notes
            "service_note_assignment", noteAssignments
            "service_feature_assignment", features
            "location_feature", locationFeatures
            "connection_claim", transfers
            "travel_restriction_assignment", restrictions
        ]

    let private czpttRelations legacy (bindings: TripBinding array) (manifest: JsonElement) =
        let read name columns =
            let path = Path.Combine(legacy, name)
            if File.Exists(path) then PackageReader.readTextRows path columns else Seq.empty
        let snapshots = combinedSourceManifest legacy manifest
        let sourceId = defaultSourceId manifest snapshots
        let digest = match snapshots.TryGetValue(sourceId) with | true, value -> value | _ -> String.replicate 64 "0"
        let operationalCallsRaw =
            read "operational_calls.parquet"
                [| "source_pa_id"; "source_sequence"; "source_location_id"; "passenger_call"; "arrival_seconds"; "departure_seconds"; "subsidiary_code"; "subsidiary_name"; "active_line_code" |]
            |> Seq.toArray
        let locations =
            read "operational_points.parquet"
                [| "source_location_id"; "country_code"; "primary_code"; "source_name"; "latitude"; "longitude"; "coordinate_source"; "coordinate_source_object_id"; "coordinate_match_method" |]
            |> Seq.map (fun row -> objectRow [
                "source_id", box sourceId; "source_location_id", box row.[0]; "source_snapshot_sha256", box digest
                "country_code", box row.[1]; "primary_code", box row.[2]; "name", box row.[3]
                "latitude", nullableParsed number row.[4]; "longitude", nullableParsed number row.[5]
                "coordinate_source", nullableString row.[6]; "coordinate_source_object_id", nullableString row.[7]; "coordinate_match_method", nullableString row.[8] ])
        let journeys =
            operationalCallsRaw |> Seq.map (fun row -> row.[0]) |> Seq.distinct |> Seq.map (fun pa -> objectRow [
                "source_id", box sourceId; "source_journey_id", box pa; "source_snapshot_sha256", box digest; "domain", box "czptt"; "mode", box "rail" ])
        let operationalCalls = operationalCallsRaw |> Seq.map (fun row -> objectRow [
            "source_id", box sourceId; "source_journey_id", box row.[0]; "sequence", box (integer row.[1]); "source_location_id", box row.[2]
            "passenger_service", box (Boolean.Parse row.[3]); "scheduled_arrival", nullableParsed integer row.[4]; "scheduled_departure", nullableParsed integer row.[5]
            "scheduled_passage", null; "subsidiary_code", nullableString row.[6]; "subsidiary_name", nullableString row.[7]; "active_line_code", nullableString row.[8] ])
        let paBindings =
            bindings |> Seq.filter (fun row -> row.trip_namespace = "czptt_pa_id")
            |> Seq.groupBy (fun row -> row.source_trip_id, row.trip_id)
            |> Seq.map (fun (key, values) -> key, values |> Seq.toArray) |> dict
        let operationalLocationByCall =
            operationalCallsRaw |> Seq.map (fun row -> struct(row.[0], row.[1]), row.[2]) |> dict
        let sourceCalls =
            read "source_call_metadata.parquet" [| "gtfs_trip_id"; "stop_sequence"; "source_pa_id"; "source_sequence" |]
            |> Seq.collect (fun row ->
                match paBindings.TryGetValue((row.[2], row.[0])) with
                | false, _ -> Seq.empty
                | true, candidates -> candidates |> Seq.map (fun binding -> objectRow [
                    "binding_id", box binding.binding_id; "call_namespace", box "czptt_pa_sequence"; "source_sequence", box row.[3]
                    "call_sequence", box (integer row.[1]); "source_stop_id", (match operationalLocationByCall.TryGetValue(struct(row.[2], row.[3])) with | true, value -> box value | _ -> null)
                    "scheduled_arrival", null; "scheduled_departure", null ]))
        let coverageFacts =
            read "source_ids_coverage_metadata.parquet"
                [| "source_coverage_id"; "source_pa_id"; "record_type"; "ids_system_id"; "coverage_role"; "from_location_code"; "from_occurrence"; "to_location_code"; "to_occurrence" |]
            |> Seq.map (fun row -> row.[0], row) |> dict
        let callsByPa =
            operationalCallsRaw |> Seq.groupBy (fun row -> row.[0])
            |> Seq.map (fun (pa, rows) -> pa, rows |> Seq.sortBy (fun row -> integer row.[1]) |> Seq.toArray) |> dict
        let endpoint (calls: string array array) (code: string) (occurrence: string) fallback =
            let matching = calls |> Array.filter (fun row ->
                let primary = row.[2].Split(':') |> Array.last
                String.Equals(primary, code, StringComparison.OrdinalIgnoreCase)
                || String.Equals(row.[2], code, StringComparison.OrdinalIgnoreCase))
            let index = match Int32.TryParse occurrence with | true, value when value > 0 -> value - 1 | _ -> fallback matching
            matching |> Array.tryItem index |> Option.map (fun row -> integer row.[1])
        let coverage =
            read "source_ids_coverage_trip_metadata.parquet" [| "source_coverage_id"; "gtfs_trip_id" |]
            |> Seq.collect (fun row ->
                match coverageFacts.TryGetValue(row.[0]) with
                | false, _ -> Seq.empty
                | true, fact ->
                    match paBindings.TryGetValue((fact.[1], row.[1])), callsByPa.TryGetValue(fact.[1]) with
                    | (true, candidates), (true, calls) when calls.Length > 0 ->
                        let first, last =
                            if fact.[2] = "CZIPTS" then
                                endpoint calls fact.[5] fact.[6] (fun _ -> 0), endpoint calls fact.[7] fact.[8] (fun values -> values.Length - 1)
                            else Some (integer calls.[0].[1]), Some (integer calls.[calls.Length - 1].[1])
                        match first, last with
                        | Some fromSequence, Some toSequence when fromSequence <= toSequence -> candidates |> Seq.map (fun binding -> objectRow [
                            "binding_id", box binding.binding_id; "coverage_id", box row.[0]; "service_id", box binding.service_id
                            "from_sequence", box fromSequence; "to_sequence", box toSequence; "coverage_type", box (if fact.[2] = "CZIPTS" then "segment" else "calendar")
                            "system_id", nullableString fact.[3]; "coverage_role", nullableString fact.[4] ])
                        | _ -> Seq.empty
                    | _ -> Seq.empty)
        Map [
            "operational_location", locations
            "operational_journey", journeys
            "operational_call", operationalCalls
            "source_trip_coverage", coverage
        ], sourceCalls

    let private extensionRows legacy =
        let extensions = Path.Combine(legacy, "extensions")
        let stopZones = csv extensions "cz_stop_zones.txt" |> Seq.toArray
        let callZones = csv extensions "cz_trip_stop_zones.txt" |> Seq.toArray
        let zones =
            Seq.append
                (stopZones |> Seq.map (fun row -> value "zone_id" row, value "zone_code" row, value "ids_system_id" row, value "source_provenance" row, "route_stop"))
                (callZones |> Seq.map (fun row -> value "zone_id" row, value "zone_code" row, value "ids_system_id" row, value "source_provenance" row, "call"))
            |> Seq.filter (fun (id, _, _, _, _) -> not (String.IsNullOrEmpty id))
            |> Seq.distinctBy (fun (id, _, _, _, _) -> id)
            |> Seq.map (fun (id, code, system, source, scope) -> objectRow [
                "zone_id", box id; "fare_system_id", nullableString system; "zone_code", box code; "name", null
                "source_id", box (if String.IsNullOrEmpty source then "national-jdf-vld-drahy" else source); "source_scope", box scope ])
        let callZoneRows = callZones |> Seq.map (fun row -> objectRow [
            "trip_id", box (value "trip_id" row); "sequence", box (integer (value "stop_sequence" row)); "zone_id", box (value "zone_id" row); "source_order", box 0 ])
        zones, callZoneRows

    let private writeCsv (path: string) (columns: string array) (rows: seq<string array>) =
        Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
        use writer = new StreamWriter(path, false, new UTF8Encoding(false))
        writer.NewLine <- "\n"
        writeCsvRow writer columns
        for row: string array in rows do writeCsvRow writer row

    let private writeExtensions (compiled: Map<string, string * int>) legacy output =
        let extensions = Path.Combine(legacy, "extensions")
        let destination = Path.Combine(output, "extensions")
        Directory.CreateDirectory(destination) |> ignore
        let baseExtensions =
            tryBasePackage legacy
            |> Option.map (fun package -> Path.Combine(package, "extensions"))
            |> Option.filter Directory.Exists
        let baseRows name columns =
            match baseExtensions with
            | Some root when File.Exists(Path.Combine(root, name)) -> csvValues root name columns
            | _ -> Seq.empty
        let gtfs = Path.Combine(legacy, "gtfs-intermediate")
        let targetTrips = HashSet<string>(csvValues gtfs "trips.txt" [| "trip_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
        let targetRoutes = HashSet<string>(csvValues gtfs "routes.txt" [| "route_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
        let targetStops = HashSet<string>(csvValues gtfs "stops.txt" [| "stop_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
        let tripProjections =
            let root = Path.Combine(legacy, "mappings")
            let path = Path.Combine(root, "base_to_output_trips.csv")
            if not (File.Exists(path)) then dict [] else
            csvValues root "base_to_output_trips.csv" [| "base_trip_id"; "output_trip_id" |]
            |> Seq.filter (fun row -> targetTrips.Contains(row.[1]))
            |> Seq.groupBy (fun row -> row.[0])
            |> Seq.map (fun (trip, rows) -> trip, rows |> Seq.map (fun row -> row.[1]) |> Seq.distinct |> Seq.toArray)
            |> dict
        let projectTrip trip =
            if String.IsNullOrEmpty(trip) then Seq.singleton ""
            else
                match tripProjections.TryGetValue(trip) with
                | true, values -> values :> seq<string>
                | _ when targetTrips.Contains(trip) -> Seq.singleton trip
                | _ -> Seq.empty
        let stopZones = csv extensions "cz_stop_zones.txt"
        let callZones = csv extensions "cz_trip_stop_zones.txt"
        let regionalZoneRows =
            Seq.append stopZones callZones
            |> Seq.map (fun row -> [| value "zone_id" row; value "zone_code" row; value "ids_system_id" row; value "source_provenance" row; if String.IsNullOrEmpty(value "trip_id" row) then "route_stop" else "call" |])
            |> Seq.filter (fun row -> not (String.IsNullOrEmpty row.[0]))
        let zoneColumns = [| "zone_id"; "zone_code"; "fare_system_id"; "source_id"; "source_scope" |]
        let zoneRows =
            Seq.append (baseRows "cz_zones.txt" zoneColumns) regionalZoneRows
            |> Seq.distinctBy (fun row -> row.[0]) |> Seq.sortBy (fun row -> row.[0])
        writeCsv (Path.Combine(destination, "cz_zones.txt")) [| "zone_id"; "zone_code"; "fare_system_id"; "source_id"; "source_scope" |] zoneRows
        let regionalRouteRows =
            let callMetadata = Path.Combine(legacy, "source_call_metadata.parquet")
            let zoneMetadata = Path.Combine(legacy, "source_route_stop_zone_metadata.parquet")
            if compiled.ContainsKey("route_stop") && File.Exists(zoneMetadata) then
                let locations =
                    PackageReader.readTextRows (fst compiled.["route_stop"]) [| "route_id"; "route_stop_id"; "location_id" |]
                    |> Seq.map (fun row -> struct(row.[0], row.[1]), row.[2]) |> dict
                PackageReader.readTextRows zoneMetadata [| "gtfs_route_id"; "source_route_stop_id"; "zone_id"; "zone_order" |]
                |> Seq.choose (fun row ->
                    let routeStop = Identity.compositeKey [ row.[0]; row.[1] ]
                    match locations.TryGetValue(struct(row.[0], routeStop)) with
                    | true, stop -> Some [| row.[0]; routeStop; stop; row.[2]; row.[3] |]
                    | _ -> None)
            elif File.Exists(callMetadata) && File.Exists(zoneMetadata) then
                let routes = csv (Path.Combine(legacy, "gtfs-intermediate")) "trips.txt" |> Seq.map (fun row -> value "trip_id" row, value "route_id" row) |> dict
                let locations =
                    PackageReader.readTextRows callMetadata [| "gtfs_trip_id"; "gtfs_stop_id"; "source_route_stop_id" |]
                    |> Seq.choose (fun row ->
                        match routes.TryGetValue(row.[0]) with
                        | true, route -> Some (struct(route, row.[2]), row.[1])
                        | _ -> None) |> Seq.distinct |> dict
                PackageReader.readTextRows zoneMetadata [| "gtfs_route_id"; "source_route_stop_id"; "zone_id"; "zone_order" |]
                |> Seq.choose (fun row ->
                    match locations.TryGetValue(struct(row.[0], row.[1])) with
                    | true, stop -> Some [| row.[0]; Identity.compositeKey [ row.[0]; row.[1] ]; stop; row.[2]; row.[3] |]
                    | _ -> None)
            else
                stopZones |> Seq.mapi (fun index row ->
                    let route, stop = value "route_id" row, value "stop_place_id" row
                    [| route; Identity.compositeKey [ route; stop ]; stop; value "zone_id" row; string index |])
        let routeColumns = [| "route_id"; "route_stop_id"; "stop_id"; "zone_id"; "source_order" |]
        let routeRows =
            Seq.append (baseRows "cz_route_stop_zones.txt" routeColumns) regionalRouteRows
            |> Seq.filter (fun row -> targetRoutes.Contains(row.[0]) && targetStops.Contains(row.[2]))
            |> Seq.distinct
            |> Seq.sortBy (fun row -> row.[0], row.[1], row.[3], integer row.[4])
        writeCsv (Path.Combine(destination, "cz_route_stop_zones.txt")) [| "route_id"; "route_stop_id"; "stop_id"; "zone_id"; "source_order" |] routeRows
        let callColumns = [| "trip_id"; "stop_sequence"; "zone_id"; "source_order" |]
        let baseCallRows =
            baseRows "cz_call_zones.txt" callColumns
            |> Seq.collect (fun row -> projectTrip row.[0] |> Seq.map (fun trip -> [| trip; row.[1]; row.[2]; row.[3] |]))
        let regionalCallRows = callZones |> Seq.map (fun row -> [| value "trip_id" row; value "stop_sequence" row; value "zone_id" row; "0" |])
        let callRows =
            Seq.append baseCallRows regionalCallRows
            |> Seq.filter (fun row -> targetTrips.Contains(row.[0]))
            |> Seq.distinct
            |> Seq.sortBy (fun row -> row.[0], integer row.[1], row.[2], integer row.[3])
        writeCsv (Path.Combine(destination, "cz_call_zones.txt")) [| "trip_id"; "stop_sequence"; "zone_id"; "source_order" |] callRows
        let regionalTransferRows = csv (Path.Combine(legacy, "gtfs-intermediate")) "transfers.txt" |> Seq.filter (fun row -> not (String.IsNullOrEmpty(value "max_waiting_time" row))) |> Seq.map (fun row ->
            let columns = [| "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" |]
            let values = columns |> Array.map (fun name -> value name row)
            Array.concat [ [| Identity.bindingId "transfer" (Array.zip columns values); |]; values; [| value "max_waiting_time" row |] ])
        let transferColumns = [| "transfer_key"; "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id"; "max_waiting_time" |]
        let baseTransferRows =
            baseRows "cz_transfer_constraints.txt" transferColumns
            |> Seq.collect (fun row ->
                Seq.allPairs (projectTrip row.[5]) (projectTrip row.[6])
                |> Seq.map (fun (fromTrip, toTrip) ->
                    let selectors = [| row.[1]; row.[2]; row.[3]; row.[4]; fromTrip; toTrip |]
                    let names = [| "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" |]
                    Array.concat [ [| Identity.bindingId "transfer" (Array.zip names selectors) |]; selectors; [| row.[7] |] ]))
        let transferRows =
            Seq.append baseTransferRows regionalTransferRows
            |> Seq.filter (fun row ->
                targetStops.Contains(row.[1]) && targetStops.Contains(row.[2])
                && (String.IsNullOrEmpty(row.[3]) || targetRoutes.Contains(row.[3]))
                && (String.IsNullOrEmpty(row.[4]) || targetRoutes.Contains(row.[4])))
            |> Seq.distinctBy (fun row -> row.[0])
            |> Seq.sortBy (fun row -> row.[0])
        writeCsv (Path.Combine(destination, "cz_transfer_constraints.txt")) [| "transfer_key"; "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id"; "max_waiting_time" |] transferRows

    let private writeGtfsZip gtfs output =
        let zipPath = Path.Combine(output, "gtfs.zip")
        use stream = File.Open(zipPath, FileMode.CreateNew, FileAccess.Write, FileShare.None)
        use archive = new ZipArchive(stream, ZipArchiveMode.Create, false, Encoding.UTF8)
        let standardTransfer = Path.Combine(output, ".transfers.txt")
        if File.Exists(Path.Combine(gtfs, "transfers.txt")) then
            let columns = [| "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id"; "transfer_type"; "min_transfer_time" |]
            writeCsv standardTransfer columns (csv gtfs "transfers.txt" |> Seq.map (fun row -> columns |> Array.map (fun name -> value name row)))
        let files = Directory.EnumerateFiles(gtfs, "*.txt") |> Seq.map (fun path -> Path.GetFileName(path), path) |> Seq.sortBy fst
        for name, original in files do
            let source = if name = "transfers.txt" && File.Exists(standardTransfer) then standardTransfer else original
            // The ZIP is a transport copy of already canonical text. Fastest
            // compression materially reduces package wall/CPU time; contents
            // and deterministic entry metadata remain unchanged.
            let entry = archive.CreateEntry(name, CompressionLevel.Fastest)
            entry.LastWriteTime <- DateTimeOffset(1980, 1, 1, 0, 0, 0, TimeSpan.Zero)
            use input = File.OpenRead(source)
            use target = entry.Open()
            input.CopyTo(target)
        if File.Exists(standardTransfer) then File.Delete(standardTransfer)

    let private writeDiagnosticsSummary legacy output =
        let events = ResizeArray<string * string * string * string>()
        let legacyJson = Path.Combine(legacy, "diagnostics.json")
        if File.Exists(legacyJson) then
            use document = JsonDocument.Parse(File.ReadAllText(legacyJson))
            match document.RootElement.TryGetProperty("diagnostics") with
            | true, values when values.ValueKind = JsonValueKind.Array ->
                for item in values.EnumerateArray() do
                    let property (name: string) (fallback: string) =
                        match item.TryGetProperty(name) with
                        | true, value when value.ValueKind = JsonValueKind.String -> value.GetString()
                        | _ -> fallback
                    events.Add(property "severity" "warning", property "code" "unknown", property "source_object_id" "", property "message" "")
            | _ -> ()
        for row in csv (Path.Combine(legacy, "reports")) "diagnostics.csv" do
            events.Add("warning", value "code" row, value "source_object_id" row, value "message" row)
        let counts =
            events |> Seq.countBy (fun (severity, code, _, _) -> severity, code)
            |> Seq.sortBy (fun ((severity, code), _) -> code, severity)
            |> Seq.map (fun ((severity, code), count) -> dict [ "severity", box severity; "code", box code; "count", box count ])
            |> Seq.toArray
        let examples =
            events |> Seq.groupBy (fun (_, code, _, _) -> code) |> Seq.sortBy fst
            |> Seq.map (fun (code, values) ->
                code, box (values |> Seq.sortBy (fun (_, _, sourceObject, message) -> sourceObject, message) |> Seq.truncate 10
                           |> Seq.map (fun (severity, _, sourceObject, message) -> dict [ "severity", box severity; "source_object_id", box sourceObject; "message", box message ]) |> Seq.toArray))
            |> dict
        let coverage = Dictionary<string,obj>(StringComparer.Ordinal)
        for fileName in [| "coverage.csv"; "coverage_by_mode.csv"; "coverage_by_tier.csv"; "trip_coverage_populations.csv"; "trip_coverage_populations_by_mode.csv"; "snapshot_day_coverage.csv"; "exclusions.csv" |] do
            let rows =
                csv (Path.Combine(legacy, "reports")) fileName
                |> Seq.map (fun row ->
                    row |> Seq.map (fun pair -> pair.Key, box pair.Value) |> dict :> obj)
                |> Seq.toArray
            if rows.Length > 0 then coverage.[Path.GetFileNameWithoutExtension(fileName)] <- box rows
        let diagnostics = dict [
            "schema_version", box Schema.DiagnosticsSchemaVersion
            "counts_by_code", box counts
            "examples_by_code", box examples
            "coverage_populations", box coverage ]
        File.WriteAllText(Path.Combine(output, "diagnostics.json"), JsonSerializer.Serialize(diagnostics, JsonSerializerOptions(WriteIndented = true)) + "\n", new UTF8Encoding(false))

    let private writeManifest legacyManifest output (relationCounts: IDictionary<string,int>) =
        use sourceDocument = JsonDocument.Parse(File.ReadAllText(legacyManifest))
        let source = sourceDocument.RootElement
        let files =
            Directory.EnumerateFiles(output, "*", SearchOption.AllDirectories)
            |> Seq.filter (fun path -> Path.GetFileName(path) <> "manifest.json")
            |> Seq.map (fun path ->
                let info = FileInfo(path)
                dict [ "path", box (Path.GetRelativePath(output, path).Replace('\\', '/')); "size_bytes", box info.Length; "sha256", box (sha256File path) ] :> obj)
            |> Seq.sortBy (fun item -> (item :?> IDictionary<string,obj>).["path"] :?> string)
            |> Seq.toArray
        let fieldType value =
            match value with
            | Schema.Text -> "string" | Schema.Int16 -> "int16" | Schema.Int32 -> "int32" | Schema.Int64 -> "int64"
            | Schema.Float64 -> "double" | Schema.Boolean -> "bool" | Schema.Date -> "date32"
        let relations = Schema.relations |> Array.map (fun relation ->
            dict [
                "name", box relation.name; "path", box ("serving/" + relation.name + ".parquet")
                "schema", box (relation.fields |> Array.map (fun field -> dict [ "name", box field.name; "type", box (fieldType field.dataType); "nullable", box field.nullable ]))
                "primary_key", box relation.primaryKey; "sort_key", box relation.sortKey
                "foreign_keys", box (relation.foreignKeys |> Array.map (fun key -> dict [ "fields", box key.fields; "relation", box key.relation; "target_fields", box key.targetFields ]))
                "row_count", box relationCounts.[relation.name]
            ] :> obj)
        let manifest = Dictionary<string,obj>()
        manifest.["bundle_format"] <- box Schema.BundleFormat
        manifest.["bundle_version"] <- box Schema.BundleVersion
        manifest.["serving_schema_version"] <- box Schema.ServingSchemaVersion
        manifest.["extension_schema_version"] <- box Schema.ExtensionSchemaVersion
        manifest.["diagnostics_schema_version"] <- box Schema.DiagnosticsSchemaVersion
        manifest.["identity_contract"] <- box "jrutil-identity-v1"
        let payloadIdentity =
            files
            |> Seq.map (fun item ->
                let value = item :?> IDictionary<string,obj>
                (value.["path"] :?> string) + "=" + (value.["sha256"] :?> string))
            |> String.concat "\n" |> Identity.sha256
        manifest.["feed_version"] <- box ("sha256:" + payloadIdentity)
        manifest.["build_spec_sha256"] <- box (Identity.sha256(source.GetRawText()))
        if source.TryGetProperty("conversion") |> fst then
            manifest.["compiler"] <- box (JsonSerializer.Deserialize<obj>(source.GetProperty("conversion").GetRawText()))
        manifest.["contract_valid"] <- box true
        manifest.["publication_eligible"] <- box (source.TryGetProperty("publishable") |> function | true, value -> value.GetBoolean() | _ -> true)
        manifest.["calibration"] <- box (source.TryGetProperty("calibration") |> function | true, value -> value.GetBoolean() | _ -> false)
        if source.TryGetProperty("gvd") |> fst then manifest.["service_horizon"] <- box (JsonSerializer.Deserialize<obj>(source.GetProperty("gvd").GetRawText()))
        let combinedSources = ResizeArray<obj>()
        let sourceIds = HashSet<string>(StringComparer.Ordinal)
        let addSources (root: JsonElement) =
            let add (item: JsonElement) =
                let id = if item.TryGetProperty("source_id") |> fst then item.GetProperty("source_id").GetString() else null
                if not (isNull id) && sourceIds.Add(id) then
                    combinedSources.Add(JsonSerializer.Deserialize<obj>(item.GetRawText()))
            match root.TryGetProperty("sources") with
            | true, values -> for item in values.EnumerateArray() do add item
            | _ ->
                match root.TryGetProperty("source") with
                | true, item -> add item
                | _ -> match root.TryGetProperty("source_snapshot") with | true, item -> add item | _ -> ()
        match tryBasePackage (Path.GetDirectoryName(legacyManifest)) with
        | Some package ->
            let baseManifest = Path.Combine(package, "manifest.json")
            if File.Exists(baseManifest) then
                use baseDocument = JsonDocument.Parse(File.ReadAllText(baseManifest))
                addSources baseDocument.RootElement
        | None -> ()
        addSources source
        if combinedSources.Count > 0 then manifest.["sources"] <- box (combinedSources.ToArray())
        manifest.["namespaces"] <- box [|
            dict [ "name", box "gtfs_trip_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "gtfs_route_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "gtfs_stop_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "gtfs_stop_sequence"; "normalization_version", box 1; "component_encoding", box "decimal-text" ]
            dict [ "name", box "operational_line_course"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8-components/slash" ]
            dict [ "name", box "czptt_pa_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "czptt_tr_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "czptt_pa_sequence"; "normalization_version", box 1; "component_encoding", box "decimal-text" ]
            dict [ "name", box "cis_line_id"; "normalization_version", box 1; "component_encoding", box "opaque-string" ]
            dict [ "name", box "cis_trip_id"; "normalization_version", box 1; "component_encoding", box "decimal-int64" ]
            dict [ "name", box "train_number"; "normalization_version", box 1; "component_encoding", box "opaque-string" ]
        |]
        manifest.["relations"] <- box relations
        manifest.["files"] <- box files
        File.WriteAllText(Path.Combine(output, "manifest.json"), JsonSerializer.Serialize(manifest, JsonSerializerOptions(WriteIndented = true)) + "\n", new UTF8Encoding(false))

    /// Convert the compiler's private staging representation to the only public
    /// production package shape.  The staging tree is never a supported API.
    let finalizeLegacyStaging (compiled: Map<string, string * int>) (nativeSummaries: JrUtil.Serving.Model.NativeCallArtifacts option) (progress: string -> int64 -> unit) legacy output =
        if Directory.Exists(output) || File.Exists(output) then invalidArg "output" "Production package output already exists"
        Directory.CreateDirectory(output) |> ignore
        let progressLock = obj()
        let mutable currentPhase = "production-package"
        let mutable completed = 0L
        let currentProcess = Process.GetCurrentProcess()
        let mutable phaseStarted = Stopwatch.GetTimestamp()
        let mutable phaseCpuStarted = currentProcess.TotalProcessorTime
        let mutable phasePeakPrivate = currentProcess.PrivateMemorySize64
        let emit () = lock progressLock (fun () ->
            currentProcess.Refresh()
            phasePeakPrivate <- max phasePeakPrivate currentProcess.PrivateMemorySize64
            progress currentPhase completed)
        let completePhase () =
            currentProcess.Refresh()
            phasePeakPrivate <- max phasePeakPrivate currentProcess.PrivateMemorySize64
            let elapsed = Stopwatch.GetElapsedTime(phaseStarted)
            let cpu = currentProcess.TotalProcessorTime - phaseCpuStarted
            Serilog.Log.Information(
                "Production package phase complete: {Phase}; rows={Rows}; elapsed_ms={ElapsedMs}; cpu_ms={CpuMs}; private_peak_bytes={PrivatePeakBytes}",
                currentPhase, completed, int64 elapsed.TotalMilliseconds, int64 cpu.TotalMilliseconds, phasePeakPrivate)
        let phase name =
            lock progressLock (fun () ->
                completePhase ()
                currentPhase <- name
                completed <- 0L
                phaseStarted <- Stopwatch.GetTimestamp()
                phaseCpuStarted <- currentProcess.TotalProcessorTime
                currentProcess.Refresh()
                phasePeakPrivate <- currentProcess.PrivateMemorySize64)
            Serilog.Log.Information("Production package phase: {Phase}", name)
            emit ()
        let advance count = lock progressLock (fun () -> completed <- count)
        use heartbeat = new Timer((fun _ -> emit ()), null, 1000, 1000)
        try
            let gtfs = Path.Combine(legacy, "gtfs-intermediate")
            if not (Directory.Exists(gtfs)) then invalidArg "legacy" "Compiler staging has no gtfs-intermediate directory"
            let serving = Path.Combine(output, "serving")
            Directory.CreateDirectory(serving) |> ignore
            phase "start-independent-output-jobs"
            let startJob name action =
                Task.Run(fun () ->
                    let started = Stopwatch.StartNew()
                    action ()
                    Serilog.Log.Information(
                        "Production package job complete: {Job}; elapsed_ms={ElapsedMs}",
                        name, int64 started.Elapsed.TotalMilliseconds))
            let gtfsZipJob = startJob "zip-gtfs" (fun () -> writeGtfsZip gtfs output)
            let extensionsJob = startJob "write-public-extensions" (fun () -> writeExtensions compiled legacy output)
            let counts = Dictionary<string,int>()
            let mutable sourceCallJob: Task<int> option = None
            let bindings, initialRelations =
                phase "prepare-serving-core"
                let tripCallSummaries = Dictionary<string, TripCallSummary>(StringComparer.Ordinal)
                let nonContiguousTripSequences = Dictionary<string, HashSet<int>>(StringComparer.Ordinal)
                let targetCallSchedules = Dictionary<string, TargetCallSchedule>(StringComparer.Ordinal)
                let wantedTargetTrips =
                    let mappingPath = Path.Combine(legacy, "mappings", "source_to_output_trips.csv")
                    if nativeSummaries.IsSome || not (File.Exists(mappingPath)) then HashSet<string>(StringComparer.Ordinal) else
                    HashSet<string>(
                        csvValues (Path.Combine(legacy, "mappings")) "source_to_output_trips.csv" [| "output_trip_id" |]
                        |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
                let mutable core = gtfsRelations legacy gtfs tripCallSummaries nonContiguousTripSequences
                if nativeSummaries.IsNone then
                    phase "write-ordered-trip_call"
                    let relation = Schema.relations |> Array.find (fun value -> value.name = "trip_call")
                    let target = Path.Combine(serving, "trip_call.parquet")
                    counts.[relation.name] <- writeOrderedTripCalls advance legacy gtfs target tripCallSummaries nonContiguousTripSequences wantedTargetTrips targetCallSchedules
                    core <- core |> Map.remove relation.name
                    phase "write-ordered-shapes"
                    let shapeCount, pointCount = writeOrderedShapes advance gtfs serving
                    counts.["shape"] <- shapeCount
                    counts.["shape_point"] <- pointCount
                    core <- core |> Map.remove "shape" |> Map.remove "shape_point"
                    phase "write-ordered-trip"
                    counts.["trip"] <- writeOrderedTrips advance gtfs (Path.Combine(serving, "trip.parquet"))
                    core <- core |> Map.remove "trip"
                use legacyManifest = JsonDocument.Parse(File.ReadAllText(Path.Combine(legacy, "manifest.json")))
                phase "prepare-source-bindings"
                let bindingsSequence, calls, baseCoverage =
                    match nativeSummaries with
                    | Some native -> BindingWriter.readNativeFacts native.tripFacts native.summaries, Seq.empty, Seq.empty
                    | None -> bindingRows None legacy gtfs legacyManifest.RootElement tripCallSummaries nonContiguousTripSequences targetCallSchedules
                let bindings = bindingsSequence |> Seq.toArray
                let zones, callZones = extensionRows legacy
                let projectedBase = projectedBaseRelations legacy gtfs
                phase "prepare-serving-identities"
                let identities = identityRelations legacy gtfs bindings legacyManifest.RootElement
                phase "prepare-native-semantics"
                let semantics =
                    if File.Exists(Path.Combine(legacy, "source_route_stop_zone_metadata.parquet"))
                       || File.Exists(Path.Combine(legacy, "source_notice_metadata.parquet")) then
                        semanticRelations nativeSummaries legacy gtfs legacyManifest.RootElement
                    else Map.empty
                let czptt, czpttCalls =
                    if File.Exists(Path.Combine(legacy, "operational_calls.parquet")) then
                        czpttRelations legacy bindings legacyManifest.RootElement
                    else Map.empty, Seq.empty
                if nativeSummaries.IsNone then
                    let target = Path.Combine(serving, "source_call_map.parquet")
                    counts.["source_call_map"] <- 0
                    let rows = Seq.append calls (SourceCallWriter.fromModelRows czpttCalls)
                    sourceCallJob <- Some (Task.Run(fun () ->
                        let started = Stopwatch.StartNew()
                        let count = SourceCallWriter.writeMappedTyped target rows CancellationToken.None (fun _ _ -> ())
                        targetCallSchedules.Clear()
                        Serilog.Log.Information(
                            "Production package job complete: {Job}; rows={Rows}; elapsed_ms={ElapsedMs}",
                            "typed-source_call_map", count, int64 started.Elapsed.TotalMilliseconds)
                        count))
                let appendRows name rows state =
                    let existing = state |> Map.tryFind name |> Option.defaultValue Seq.empty
                    state |> Map.add name (Seq.append existing rows)
                let generated =
                    identities |> Map.fold (fun state name rows -> state |> Map.add name rows) core
                    |> fun state -> semantics |> Map.fold (fun current name rows -> current |> Map.add name rows) state
                    |> fun state -> czptt |> Map.fold (fun current name rows -> current |> Map.add name rows) state
                    |> Map.add "source_trip_map" (bindings |> Seq.map bindingRow)
                    |> fun state -> if nativeSummaries.IsSome then state |> Map.add "source_call_map" czpttCalls else state
                    |> Map.add "source_trip_coverage" (Seq.append (czptt |> Map.tryFind "source_trip_coverage" |> Option.defaultValue Seq.empty) baseCoverage)
                    |> Map.add "fare_zone" zones
                    |> Map.add "call_zone" callZones
                let preferBase name baseRows currentRows = seq {
                    let relation = Schema.relations |> Array.find (fun relation -> relation.name = name)
                    let seen = HashSet<string>(StringComparer.Ordinal)
                    let key (row: IDictionary<string,obj>) =
                        relation.sortKey |> Seq.map (fun field -> fieldText row.[field]) |> Identity.compositeKey
                    for row in baseRows do
                        if seen.Add(key row) then yield row
                    for row in currentRows do
                        if seen.Add(key row) then yield row
                }
                let supplied =
                    projectedBase
                    |> Map.fold (fun state name baseRows ->
                        let current = state |> Map.tryFind name |> Option.defaultValue Seq.empty
                        state |> Map.add name (preferBase name baseRows current)) generated
                Task.WaitAll([| gtfsZipJob; extensionsJob |])
                bindings, supplied
            let mutable supplied = initialRelations
            let mutable selectedProvenanceJob: Task<int> option = None
            if nativeSummaries.IsNone && not (compiled.ContainsKey("selected_field_provenance")) then
                match supplied |> Map.tryFind "selected_field_provenance" with
                | Some rows ->
                    let relation = Schema.relations |> Array.find (fun value -> value.name = "selected_field_provenance")
                    let target = Path.Combine(serving, relation.name + ".parquet")
                    counts.[relation.name] <- 0
                    supplied <- supplied |> Map.remove relation.name
                    selectedProvenanceJob <- Some (Task.Run(fun () ->
                        let started = Stopwatch.StartNew()
                        let count = writeSelectedFieldProvenance (fun _ _ -> ()) target relation rows
                        Serilog.Log.Information(
                            "Production package job complete: {Job}; rows={Rows}; elapsed_ms={ElapsedMs}",
                            "grouped-selected_field_provenance", count, int64 started.Elapsed.TotalMilliseconds)
                        count))
                | None -> ()
            use sortStorage = new JrUtil.RegionalOverlay.Scratch.Storage(output)
            for relation in Schema.relations do
                let target = Path.Combine(serving, relation.name + ".parquet")
                if counts.ContainsKey(relation.name) then () else
                match compiled |> Map.tryFind relation.name with
                | Some (path, count) ->
                    phase ("finalize-" + relation.name)
                    File.Move(path, target)
                    counts.[relation.name] <- count
                    advance (int64 count)
                | None ->
                    match nativeSummaries with
                    | _ when relation.name = "source_trip_map" ->
                        let report operation count =
                            lock progressLock (fun () -> currentPhase <- "source-trips-" + operation; completed <- count)
                        counts.[relation.name] <- BindingWriter.write target CancellationToken.None report bindings
                    | None when relation.name = "source_call_map" ->
                        phase "typed-source_call_map"
                        let report operation count =
                            lock progressLock (fun () -> currentPhase <- "source-calls-" + operation; completed <- count)
                        let rows = supplied |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        counts.[relation.name] <- SourceCallWriter.writeMapped target rows CancellationToken.None report
                    | Some native when relation.name = "source_call_map" ->
                        phase "sort-and-write-native-source-calls"
                        let byTrip = bindings |> Seq.map (fun binding -> binding.trip_id, binding.binding_id) |> dict
                        let report operation count =
                            lock progressLock (fun () -> currentPhase <- "source-calls-" + operation; completed <- count)
                        counts.[relation.name] <- SourceCallWriter.write target native.sourceCalls byTrip CancellationToken.None report
                    | None when relation.name = "shape" || relation.name = "shape_point" ->
                        phase ("write-ordered-" + relation.name)
                        let rows = supplied |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        counts.[relation.name] <- writeParquet advance target relation rows
                    | _ when relation.name = "selected_field_provenance" ->
                        phase "grouped-selected_field_provenance"
                        let rows = supplied |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        counts.[relation.name] <- writeSelectedFieldProvenance (fun _ count -> advance count) target relation rows
                    | _ when relation.name = "object_origin"
                             || relation.name = "binding_evidence"
                             || relation.name = "route_stop" ->
                        phase ("typed-" + relation.name)
                        let rows = supplied |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        counts.[relation.name] <- writeRequiredTextRelation (fun _ count -> advance count) target relation rows
                    | _ ->
                        phase ("sort-" + relation.name)
                        let sourceRows = supplied |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        let mutable read = 0L
                        let rows = externallySorted sortStorage relation (sourceRows |> Seq.map (fun row -> read <- read + 1L; advance read; row))
                        phase ("write-" + relation.name)
                        counts.[relation.name] <- writeParquet advance target relation rows
                supplied <- supplied |> Map.remove relation.name
            match sourceCallJob with
            | Some job ->
                phase "await-source_call_map"
                counts.["source_call_map"] <- job.GetAwaiter().GetResult()
            | None -> ()
            match selectedProvenanceJob with
            | Some job ->
                phase "await-selected_field_provenance"
                counts.["selected_field_provenance"] <- job.GetAwaiter().GetResult()
            | None -> ()
            phase "write-diagnostics-summary"
            writeDiagnosticsSummary legacy output
            phase "hash-production-payloads"
            writeManifest (Path.Combine(legacy, "manifest.json")) output counts
            phase "validate-production-package"
            Validation.validatePackage output |> ignore
            lock progressLock completePhase
        with error ->
            Serilog.Log.Error(error, "Production package failed in {Phase}; diagnostic staging retained at {StagingPath}", currentPhase, output)
            reraise ()

    /// Write the optional, explicitly addressed diagnostic artifact.  This is
    /// deliberately separate from the production package inventory.
    let writeDiagnosticArtifact legacy output includeTraces =
        if Directory.Exists(output) || File.Exists(output) then
            invalidArg "output" "Diagnostic output already exists"
        let temporary = output + ".tmp-" + Guid.NewGuid().ToString("N")
        Directory.CreateDirectory(temporary) |> ignore
        try
            let copyTree source destination =
                if Directory.Exists(source) then
                    let obsoleteReports = set [
                        "ambiguities.csv"; "conflicts.csv"; "equivalent_ties.csv"; "quarantine.csv"; "substitutions.csv"; "semantic_inheritance.csv"
                        "coverage.csv"; "coverage_by_mode.csv"; "coverage_by_tier.csv"; "trip_coverage_populations.csv"
                        "trip_coverage_populations_by_mode.csv"; "snapshot_day_coverage.csv"; "exclusions.csv" ]
                    for path in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
                        let relative = Path.GetRelativePath(source, path)
                        if not (obsoleteReports.Contains(Path.GetFileName(relative)))
                           && (includeTraces || not (relative.StartsWith("base_to_output_", StringComparison.Ordinal))) then
                            let target = Path.Combine(destination, relative)
                            Directory.CreateDirectory(Path.GetDirectoryName(target)) |> ignore
                            File.Copy(path, target)
            copyTree (Path.Combine(legacy, "reports")) (Path.Combine(temporary, "events"))
            copyTree (Path.Combine(legacy, "policies")) (Path.Combine(temporary, "inputs", "policies"))
            copyTree (Path.Combine(legacy, "source-descriptors")) (Path.Combine(temporary, "inputs", "source-descriptors"))
            copyTree (Path.Combine(legacy, "extensions")) (Path.Combine(temporary, "projection", "legacy-extensions"))
            let legacyDiagnostics = Path.Combine(legacy, "diagnostics.json")
            if File.Exists(legacyDiagnostics) then
                let target = Path.Combine(temporary, "events", "diagnostics.json")
                Directory.CreateDirectory(Path.GetDirectoryName(target)) |> ignore
                File.Copy(legacyDiagnostics, target)
            if includeTraces then copyTree (Path.Combine(legacy, "mappings")) (Path.Combine(temporary, "traces"))
            let evidenceReferences = ResizeArray<obj>()
            let baseEvidenceManifest = Path.Combine(legacy, "base-evidence", "manifest.json")
            if File.Exists(baseEvidenceManifest) then
                evidenceReferences.Add(dict [
                    "kind", box "base-package"; "manifest_sha256", box (sha256File baseEvidenceManifest) ] :> obj)
            let entries =
                Directory.EnumerateFiles(temporary, "*", SearchOption.AllDirectories)
                |> Seq.map (fun path -> dict [
                    "path", box (Path.GetRelativePath(temporary, path).Replace('\\', '/'))
                    "size_bytes", box (FileInfo(path).Length)
                    "sha256", box (sha256File path) ] :> obj)
                |> Seq.sortBy (fun item -> (item :?> IDictionary<string,obj>).["path"] :?> string)
                |> Seq.toArray
            let manifest = dict [
                "diagnostics_format", box "jrutil-diagnostics"
                "diagnostics_schema_version", box Schema.DiagnosticsSchemaVersion
                "traces_included", box includeTraces
                "evidence_references", box (evidenceReferences.ToArray())
                "files", box entries ]
            File.WriteAllText(Path.Combine(temporary, "manifest.json"), JsonSerializer.Serialize(manifest, JsonSerializerOptions(WriteIndented = true)) + "\n", new UTF8Encoding(false))
            Directory.Move(temporary, output)
        with _ ->
            if Directory.Exists(temporary) then Directory.Delete(temporary, true)
            reraise ()
