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
open System.IO.Hashing
open Parquet
open Parquet.Schema

module Validation =
    type Result = {
        errors: string array
        fileCount: int
        relationCount: int
    }
    with member value.isValid = value.errors.Length = 0

    // Parquet validation allocates bounded row-group arrays, but the runtime
    // otherwise keeps their committed segments across every relation. Release
    // those dead phase buffers between files and row-group batches. This is
    // reclamation only; it does not impose a GC heap hard limit. The gate
    // prevents repeated full collections once the footprint stays above the
    // threshold without further growth.
    let private reclaimGate = MemoryReclaim.gate 3_000_000_000L

    let private reclaimValidationMemory () = reclaimGate.Check() |> ignore

    let private relative (root: string) (path: string) =
        Path.GetRelativePath(root, path).Replace('\\', '/')

    let private expectedFiles =
        seq {
            yield "gtfs.zip"
            yield "manifest.json"
            yield "diagnostics.json"
            for relation in Schema.relations do yield "serving/" + relation.name + ".parquet"
        }
        |> Set.ofSeq

    let private expectedClrType = function
        | Schema.Text -> typeof<string>
        | Schema.Int16 -> typeof<int16>
        | Schema.Int32 -> typeof<int>
        | Schema.Int64 -> typeof<int64>
        | Schema.Float64 -> typeof<double>
        | Schema.Boolean -> typeof<bool>
        | Schema.Date -> typeof<DateOnly>

    let private metadataValue (metadata: IReadOnlyDictionary<string,string>) key =
        if isNull metadata then null
        else match metadata.TryGetValue key with | true, value -> value | _ -> null

    let private keyValues<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> ValueType>
                         (group: ParquetRowGroupReader) (field: DataField) count =
        if field.IsNullable then
            let values = Array.zeroCreate<Nullable<'T>> count
            group.ReadAsync<'T>(field, values.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
            values |> Array.map (fun value -> if value.HasValue then box value.Value else null)
        else
            let values = Array.zeroCreate<'T> count
            group.ReadAsync<'T>(field, values.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
            values |> Array.map box

    let private keyColumn (group: ParquetRowGroupReader) (field: DataField) count =
        if field.ClrType = typeof<string> then
            let values = Array.zeroCreate<string> count
            group.ReadAsync(field, values.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
            values |> Array.map box
        elif field.ClrType = typeof<int> then keyValues<int> group field count
        elif field.ClrType = typeof<int64> then keyValues<int64> group field count
        elif field.ClrType = typeof<int16> then keyValues<int16> group field count
        elif field.ClrType = typeof<DateOnly> then keyValues<DateOnly> group field count
        elif field.ClrType = typeof<DateTime> then keyValues<DateTime> group field count
        elif field.ClrType = typeof<bool> then keyValues<bool> group field count
        elif field.ClrType = typeof<double> then keyValues<double> group field count
        else invalidOp $"Unsupported primary-key type {field.ClrType}"

    /// Type-tagged invariant text of a key cell.
    let private keyToken (value: obj) =
        match value with
        | :? string as value -> "s" + value
        | :? DateTime as value -> "d" + DateOnly.FromDateTime(value).ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
        | :? DateOnly as value -> "d" + value.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
        | :? double as value -> "f" + value.ToString("R", CultureInfo.InvariantCulture)
        | value -> "v" + Convert.ToString(value, CultureInfo.InvariantCulture)

    /// 64-bit key hashes, sorted and scanned for equal neighbours: 8 bytes per
    /// row for one relation at a time, independent of the row order.
    let private duplicateKeyHashes (hashes: ResizeArray<uint64>) =
        hashes.Sort()
        let mutable duplicates = 0L
        for index in 1 .. hashes.Count - 1 do
            if hashes.[index] = hashes.[index - 1] then duplicates <- duplicates + 1L
        duplicates

    let private validateParquet (errors: ResizeArray<string>) (directory: string) (relation: Schema.Relation) (expectedRows: int64) =
        let path = Path.Combine(directory, "serving", relation.name + ".parquet")
        if File.Exists(path) then
            use stream = File.OpenRead(path)
            let reader = ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
            try
                let actual = reader.Schema.DataFields
                if actual.Length <> relation.fields.Length then
                    errors.Add($"{relative directory path}: expected {relation.fields.Length} fields, found {actual.Length}")
                else
                    Array.iter2 (fun (field: DataField) (expected: JrUtil.Serving.Schema.Field) ->
                        let typeMatches =
                            field.ClrType = expectedClrType expected.dataType
                            || (expected.dataType = Schema.Date && field.ClrType = typeof<DateTime>)
                        if field.Name <> expected.name
                           || not typeMatches
                           || field.IsNullable <> expected.nullable then
                            errors.Add($"{relative directory path}: field {expected.name} type/nullability mismatch")) actual relation.fields
                if Schema.schemaMajor (metadataValue reader.CustomMetadata "obehy.schema_version") <> Some Schema.ServingSchemaMajor then
                    errors.Add($"{relative directory path}: serving schema metadata mismatch")
                if metadataValue reader.CustomMetadata "obehy.relation" <> relation.name then
                    errors.Add($"{relative directory path}: relation metadata mismatch")
                let mutable rows = 0L
                let mutable keyFailure = false
                let mutable groupsSinceReclaim = 0
                let keyHashes = ResizeArray<uint64>()
                let keyFields = relation.primaryKey |> Array.map (fun name -> actual |> Array.tryFind (fun field -> field.Name = name))
                for index in 0 .. reader.RowGroupCount - 1 do
                    if groupsSinceReclaim = 16 then
                        groupsSinceReclaim <- 0
                        reclaimValidationMemory ()
                    use group = reader.OpenRowGroupReader(index)
                    // Writers emit bounded groups. Reject oversized consumer input before
                    // allocating its column arrays; never materialize a whole relation.
                    if group.RowCount > 65536L then
                        errors.Add($"{relative directory path}: row group exceeds the 65,536-row validation budget")
                    elif not keyFailure && (keyFields |> Array.forall Option.isSome) then
                        let columns = keyFields |> Array.map (fun field -> keyColumn group field.Value (int group.RowCount))
                        for row in 0 .. int group.RowCount - 1 do
                            if not keyFailure then
                                if columns |> Array.exists (fun column -> isNull column.[row]) then
                                    errors.Add($"{relative directory path}: null primary key at row {rows + int64 row}")
                                    keyFailure <- true
                                else
                                    let key = columns |> Array.map (fun column -> keyToken column.[row]) |> String.concat "\u001f"
                                    keyHashes.Add(XxHash64.HashToUInt64(Encoding.UTF8.GetBytes(key).AsSpan()))
                    rows <- rows + group.RowCount
                    groupsSinceReclaim <- groupsSinceReclaim + 1
                if not keyFailure then
                    let duplicates = duplicateKeyHashes keyHashes
                    if duplicates > 0L then
                        errors.Add($"{relative directory path}: {duplicates} duplicate primary keys")
                if rows <> expectedRows then
                    errors.Add($"{relative directory path}: manifest row count {expectedRows}, physical row count {rows}")
            finally
                reader.DisposeAsync().AsTask().GetAwaiter().GetResult()

    /// Hash every non-null key of `fields` in a relation file.
    let private scanKeys (directory: string) (relation: Schema.Relation) (fields: string array) (consume: uint64 -> (unit -> string) -> unit) =
        let path = Path.Combine(directory, "serving", relation.name + ".parquet")
        use stream = File.OpenRead(path)
        let reader = ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let dataFields = fields |> Array.map (fun name -> reader.Schema.DataFields |> Array.find (fun field -> field.Name = name))
            for index in 0 .. reader.RowGroupCount - 1 do
                use group = reader.OpenRowGroupReader(index)
                let columns = dataFields |> Array.map (fun field -> keyColumn group field (int group.RowCount))
                for row in 0 .. int group.RowCount - 1 do
                    if columns |> Array.forall (fun column -> not (isNull column.[row])) then
                        let key = columns |> Array.map (fun column -> keyToken column.[row]) |> String.concat "\u001f"
                        let shown () = columns |> Array.map (fun column -> sprintf "%A" column.[row]) |> String.concat ", "
                        consume (XxHash64.HashToUInt64(Encoding.UTF8.GetBytes(key).AsSpan())) shown
        finally
            reader.DisposeAsync().AsTask().GetAwaiter().GetResult()

    /// Every declared foreign key must resolve. Null references are allowed;
    /// target keys are held as 64-bit hashes, one target at a time.
    let private validateForeignKeys (errors: ResizeArray<string>) (directory: string) =
        let byName = Schema.relations |> Array.map (fun relation -> relation.name, relation) |> dict
        let uses =
            Schema.relations
            |> Array.collect (fun relation -> relation.foreignKeys |> Array.map (fun key -> relation, key))
            |> Array.groupBy (fun (_, key) -> key.relation, String.concat "," key.targetFields)
        for (target, _), references in uses do
            let targetFields = (snd references.[0]).targetFields
            let keys = HashSet<uint64>()
            scanKeys directory byName.[target] targetFields (fun hash _ -> keys.Add(hash) |> ignore)
            for relation, key in references do
                let mutable missing = 0L
                let mutable example = null
                scanKeys directory relation key.fields (fun hash text ->
                    if not (keys.Contains(hash)) then
                        missing <- missing + 1L
                        if isNull example then example <- text ())
                if missing > 0L then
                    let fields = String.concat "," key.fields
                    errors.Add($"serving/{relation.name}.parquet: {missing} rows have ({fields}) missing from {target}, e.g. {example}")
            reclaimValidationMemory ()

    let private validateZip (errors: ResizeArray<string>) (directory: string) =
        let path = Path.Combine(directory, "gtfs.zip")
        if File.Exists(path) then
            use archive = ZipFile.OpenRead(path)
            let names = archive.Entries |> Seq.map (fun entry -> entry.FullName) |> Seq.toArray
            if names |> Array.exists (fun (name: string) -> name.Contains('/') || name.Contains('\\')) then
                errors.Add("gtfs.zip: entries must be flat GTFS table names")
            if names <> (names |> Array.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))) then
                errors.Add("gtfs.zip: entries are not in deterministic ordinal order")
            let transfers = archive.GetEntry("transfers.txt")
            if not (isNull transfers) then
                use stream = transfers.Open()
                use reader = new StreamReader(stream, Encoding.UTF8, true)
                let header = reader.ReadLine()
                if header.Split(',') |> Array.contains "max_waiting_time" then
                    errors.Add("gtfs.zip/transfers.txt: max_waiting_time is not a standard GTFS column")

    /// Feed prefixes a generated or public identifier may start with. The JDF
    /// package also holds the regional overlay's `overlay:` entities.
    let feedPrefixes = [| "jdf:"; "overlay:"; "czptt:" |]

    let private cell (column: ColumnWriter.Column) row : string =
        let text (value: 'T) = Convert.ToString(value, CultureInfo.InvariantCulture)
        let optional (value: Nullable<'T>) = if value.HasValue then text value.Value else null
        match column with
        | ColumnWriter.Text values -> values.[row]
        | ColumnWriter.Int16 values -> text values.[row]
        | ColumnWriter.OptionalInt16 values -> optional values.[row]
        | ColumnWriter.Int32 values -> text values.[row]
        | ColumnWriter.OptionalInt32 values -> optional values.[row]
        | ColumnWriter.Int64 values -> text values.[row]
        | ColumnWriter.OptionalInt64 values -> optional values.[row]
        | ColumnWriter.Float64 values -> values.[row].ToString("R", CultureInfo.InvariantCulture)
        | ColumnWriter.OptionalFloat64 values -> if values.[row].HasValue then values.[row].Value.ToString("R", CultureInfo.InvariantCulture) else null
        | ColumnWriter.Boolean values -> if values.[row] then "true" else "false"
        | ColumnWriter.OptionalBoolean values -> if values.[row].HasValue then (if values.[row].Value then "true" else "false") else null
        | ColumnWriter.Date values -> values.[row].ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
        | ColumnWriter.OptionalDate values -> if values.[row].HasValue then values.[row].Value.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture) else null

    /// Rows of a relation as text cells of the named fields (null for null).
    let private relationCells (directory: string) (relation: Schema.Relation) (fields: string array) = seq {
        let indexes = fields |> Array.map (fun name -> relation.fields |> Array.findIndex (fun field -> field.name = name))
        for columns in ColumnReader.groups (Path.Combine(directory, "serving", relation.name + ".parquet")) relation do
            let picked = indexes |> Array.map (fun index -> columns.[index])
            let rows = if columns.Length = 0 then 0 else ColumnWriter.rowCount columns.[0]
            for row in 0 .. rows - 1 do
                yield picked |> Array.map (fun column -> cell column row)
    }

    let private relationByName name = Schema.relations |> Array.find (fun relation -> relation.name = name)

    /// Coded values, identifier prefixes and key encodings.
    let private validateValues (errors: ResizeArray<string>) (directory: string) =
        let report (relation: string) (problem: string) (count: int64) (example: string) =
            if count > 0L then errors.Add($"serving/{relation}.parquet: {count} rows {problem}, e.g. {example}")
        for relation in Schema.relations do
            let coded = relation.fields |> Array.filter (fun field -> not (isNull field.enumeration))
            for field in coded do
                let allowed = HashSet<string>(Schema.enumerations.[field.enumeration], StringComparer.Ordinal)
                let mutable count, example = 0L, null
                for row in relationCells directory relation [| field.name |] do
                    let value = row.[0]
                    if not (isNull value) && not (allowed.Contains(value)) then
                        count <- count + 1L
                        if isNull example then example <- value
                report relation.name $"have {field.name} outside {field.enumeration}" count example
            match relation.primaryKey with
            | [| key |] when (relation.fields |> Array.find (fun field -> field.name = key)).dataType = Schema.Text ->
                let mutable count, example = 0L, null
                for row in relationCells directory relation [| key |] do
                    if not (feedPrefixes |> Array.exists (fun prefix -> row.[0].StartsWith(prefix, StringComparison.Ordinal))) then
                        count <- count + 1L
                        if isNull example then example <- row.[0]
                report relation.name $"have {key} without a feed prefix" count example
            | _ -> ()
        let namespaces = Schema.namespaces |> Array.map (fun value -> value.name, value) |> dict
        let checkKeys relation (fields: string array) (entityField: int option) =
            let mutable count, example = 0L, null
            for row in relationCells directory (relationByName relation) fields do
                match namespaces.TryGetValue(row.[0]) with
                | true, declared ->
                    let kindMatches = entityField |> Option.forall (fun index -> row.[index] = declared.entityKind)
                    if not kindMatches || not (Text.RegularExpressions.Regex.IsMatch(row.[1], declared.pattern)) then
                        count <- count + 1L
                        if isNull example then example <- row.[0] + " " + row.[1]
                | _ -> ()
            report relation "do not match their namespace's entity kind or identifier encoding" count example
        checkKeys "source_key" [| "namespace"; "identifier"; "entity_kind" |] (Some 2)
        checkKeys "call_key" [| "namespace"; "identifier" |] None

    /// Order-independent multiset fingerprint of canonical rows.
    type private Fingerprint() =
        member val Count = 0L with get, set
        member val Sum = 0UL with get, set
        member val Xor = 0UL with get, set
        member this.Add(row: string seq) =
            let hash = XxHash64.HashToUInt64(Encoding.UTF8.GetBytes(String.Join("\u001f", row)).AsSpan())
            this.Count <- this.Count + 1L
            this.Sum <- this.Sum + hash
            this.Xor <- this.Xor ^^^ hash
        member this.Equals(other: Fingerprint) = this.Count = other.Count && this.Sum = other.Sum && this.Xor = other.Xor

    let private gtfsSeconds (value: string) =
        if String.IsNullOrWhiteSpace(value) then null
        else
            let parts = value.Split(':')
            string (Int32.Parse(parts.[0], CultureInfo.InvariantCulture) * 3600
                    + Int32.Parse(parts.[1], CultureInfo.InvariantCulture) * 60
                    + Int32.Parse(parts.[2], CultureInfo.InvariantCulture))

    let private gtfsDate (value: string) =
        DateOnly.ParseExact(value, "yyyyMMdd", CultureInfo.InvariantCulture).ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)

    /// gtfs.zip must be the projection of the relations: passenger calls are
    /// stop_times, and the calendar relations are calendar(.txt|_dates.txt).
    let private validateGtfsProjection (errors: ResizeArray<string>) (directory: string) =
        let zip = Path.Combine(directory, "gtfs.zip")
        let compare (name: string) (serving: Fingerprint) (gtfs: Fingerprint) =
            if not (serving.Equals(gtfs)) then
                errors.Add($"gtfs.zip/{name}: {gtfs.Count} rows are not the projection of {serving.Count} serving rows")
        let blankZero value = if String.IsNullOrEmpty(value) then "0" else value
        let nullText (value: string) = if isNull value then "" else value
        let calls, stopTimes = Fingerprint(), Fingerprint()
        for row in relationCells directory (relationByName "trip_call")
                       [| "trip_id"; "sequence"; "location_id"; "boarding_point_id"; "scheduled_arrival"; "scheduled_departure"; "pickup_type"; "dropoff_type"; "passenger_service" |] do
            if row.[8] = "true" then
                calls.Add([ row.[0]; row.[1]; (if isNull row.[3] then row.[2] else row.[3]); nullText row.[4]; nullText row.[5]; row.[6]; row.[7] ])
        for row in JrUtil.RegionalOverlay.GtfsFiles.csvValues zip "stop_times.txt"
                       [| "trip_id"; "stop_sequence"; "stop_id"; "arrival_time"; "departure_time"; "pickup_type"; "drop_off_type" |] do
            stopTimes.Add([ row.[0]; row.[1]; row.[2]; nullText (gtfsSeconds row.[3]); nullText (gtfsSeconds row.[4]); blankZero row.[5]; blankZero row.[6] ])
        compare "stop_times.txt" calls stopTimes
        let trips, gtfsTrips = Fingerprint(), Fingerprint()
        for row in relationCells directory (relationByName "trip") [| "trip_id"; "route_id"; "service_id" |] do trips.Add(row)
        for row in JrUtil.RegionalOverlay.GtfsFiles.csvValues zip "trips.txt" [| "trip_id"; "route_id"; "service_id" |] do gtfsTrips.Add(row)
        compare "trips.txt" trips gtfsTrips
        let calendars, gtfsCalendars = Fingerprint(), Fingerprint()
        for row in relationCells directory (relationByName "service_calendar") [| "service_id"; "valid_from"; "valid_to"; "weekday_mask" |] do
            calendars.Add([ row.[0]; row.[1]; row.[2]; row.[3] ])
        let days = [| "monday"; "tuesday"; "wednesday"; "thursday"; "friday"; "saturday"; "sunday" |]
        for row in JrUtil.RegionalOverlay.GtfsFiles.csvValues zip "calendar.txt" (Array.append [| "service_id"; "start_date"; "end_date" |] days) do
            let mask = days |> Array.mapi (fun index _ -> if row.[3 + index] = "1" then 1 <<< index else 0) |> Array.sum
            gtfsCalendars.Add([ row.[0]; gtfsDate row.[1]; gtfsDate row.[2]; string mask ])
        compare "calendar.txt" calendars gtfsCalendars
        let exceptions, gtfsExceptions = Fingerprint(), Fingerprint()
        for row in relationCells directory (relationByName "service_exception") [| "service_id"; "service_date"; "added" |] do
            exceptions.Add([ row.[0]; row.[1]; (if row.[2] = "true" then "1" else "2") ])
        for row in JrUtil.RegionalOverlay.GtfsFiles.csvValues zip "calendar_dates.txt" [| "service_id"; "date"; "exception_type" |] do
            gtfsExceptions.Add([ row.[0]; gtfsDate row.[1]; row.[2] ])
        compare "calendar_dates.txt" exceptions gtfsExceptions

    /// Consecutive parts of a run share exactly one boundary call with
    /// identical times; a bus part carries no non-passenger calls.
    let private validateTripParts (errors: ResizeArray<string>) (directory: string) =
        let modes = Dictionary<string, string>(StringComparer.Ordinal)
        for row in relationCells directory (relationByName "route") [| "route_id"; "mode" |] do modes.[row.[0]] <- row.[1]
        let parts = Dictionary<string, struct(string * int * string)>(StringComparer.Ordinal)
        for row in relationCells directory (relationByName "trip") [| "trip_id"; "run_key"; "run_part"; "route_id" |] do
            if not (isNull row.[1]) then
                parts.[row.[0]] <- struct(row.[1], Int32.Parse(row.[2], CultureInfo.InvariantCulture), modes.[row.[3]])
        if parts.Count > 0 then
            let first = Dictionary<string, string array>(StringComparer.Ordinal)
            let last = Dictionary<string, string array>(StringComparer.Ordinal)
            let mutable busOperational = 0L
            for row in relationCells directory (relationByName "trip_call")
                           [| "trip_id"; "sequence"; "location_id"; "scheduled_arrival"; "scheduled_departure"; "passenger_service" |] do
                match parts.TryGetValue(row.[0]) with
                | true, struct(_, _, mode) ->
                    let sequence = Int32.Parse(row.[1], CultureInfo.InvariantCulture)
                    match first.TryGetValue(row.[0]) with
                    | true, current when Int32.Parse(current.[1], CultureInfo.InvariantCulture) <= sequence -> ()
                    | _ -> first.[row.[0]] <- row
                    match last.TryGetValue(row.[0]) with
                    | true, current when Int32.Parse(current.[1], CultureInfo.InvariantCulture) >= sequence -> ()
                    | _ -> last.[row.[0]] <- row
                    if mode = "bus" && row.[5] = "false" then busOperational <- busOperational + 1L
                | _ -> ()
            if busOperational > 0L then
                errors.Add($"serving/trip_call.parquet: {busOperational} non-passenger calls belong to bus trip parts")
            let mutable broken, example = 0L, null
            for run in parts |> Seq.groupBy (fun pair -> let struct(key, _, _) = pair.Value in key) do
                let ordered = snd run |> Seq.sortBy (fun pair -> let struct(_, part, _) = pair.Value in part) |> Seq.map _.Key |> Seq.toArray
                for index in 1 .. ordered.Length - 1 do
                    match last.TryGetValue(ordered.[index - 1]), first.TryGetValue(ordered.[index]) with
                    | (true, ending), (true, starting) when ending.[1] = starting.[1] && ending.[2] = starting.[2]
                                                            && ending.[3] = starting.[3] && ending.[4] = starting.[4] -> ()
                    | _ ->
                        broken <- broken + 1L
                        if isNull example then example <- ordered.[index]
            if broken > 0L then
                errors.Add($"serving/trip.parquet: {broken} trip parts do not share one boundary call with the previous part, e.g. {example}")

    let private validateSources (errors: ResizeArray<string>) (manifest: JsonElement) =
        match manifest.TryGetProperty("sources") with
        | true, sources when sources.ValueKind = JsonValueKind.Array && sources.GetArrayLength() > 0 ->
            for source in sources.EnumerateArray() do
                let id = match source.TryGetProperty("source_id") with | true, value -> value.GetString() | _ -> "?"
                match source.TryGetProperty("payload_sha256") with
                | true, value when value.ValueKind = JsonValueKind.String
                                   && Text.RegularExpressions.Regex.IsMatch(value.GetString(), "^[0-9a-f]{64}$")
                                   && value.GetString() <> String.replicate 64 "0" -> ()
                | _ -> errors.Add($"manifest.json: source {id} has no payload_sha256 digest")
        | _ -> errors.Add("manifest.json: sources are missing")

    let inspect (directory: string) =
        let root = Path.GetFullPath(directory)
        let errors = ResizeArray<string>()
        if not (Directory.Exists(root)) then
            { errors = [| $"Package directory does not exist: {root}" |]; fileCount = 0; relationCount = 0 }
        else
            let actualFiles =
                Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
                |> Seq.map (relative root)
                |> Set.ofSeq
            for path in Set.difference expectedFiles actualFiles do errors.Add($"Missing declared production file: {path}")
            for path in Set.difference actualFiles expectedFiles do errors.Add($"Undeclared production file: {path}")
            let manifestPath = Path.Combine(root, "manifest.json")
            if File.Exists(manifestPath) then
                use document = JsonDocument.Parse(File.ReadAllText(manifestPath))
                let manifest = document.RootElement
                let requireString (name: string) (expected: string) =
                    match manifest.TryGetProperty(name) with
                    | true, value when value.ValueKind = JsonValueKind.String && value.GetString() = expected -> ()
                    | _ -> errors.Add($"manifest.json: {name} must be {expected}")
                let requireInt (name: string) (expected: int) =
                    match manifest.TryGetProperty(name) with
                    | true, value when value.ValueKind = JsonValueKind.Number && value.GetInt32() = expected -> ()
                    | _ -> errors.Add($"manifest.json: {name} must be {expected}")
                requireString "bundle_format" Schema.BundleFormat
                requireInt "bundle_version" Schema.BundleVersion
                match manifest.TryGetProperty("serving_schema_version") with
                | true, value when value.ValueKind = JsonValueKind.String
                                   && Schema.schemaMajor (value.GetString()) = Some Schema.ServingSchemaMajor -> ()
                | _ -> errors.Add($"manifest.json: serving_schema_version must be {Schema.ServingSchemaMajor}.<minor>")
                requireInt "diagnostics_schema_version" Schema.DiagnosticsSchemaVersion
                requireString "identity_contract" "jrutil-identity-v1"
                let inventory = Dictionary<string, struct(int64 * string)>(StringComparer.Ordinal)
                match manifest.TryGetProperty("files") with
                | true, files when files.ValueKind = JsonValueKind.Array ->
                    for item in files.EnumerateArray() do
                        inventory.Add(item.GetProperty("path").GetString(), struct(item.GetProperty("size_bytes").GetInt64(), item.GetProperty("sha256").GetString()))
                | _ -> errors.Add("manifest.json: files inventory is missing")
                let expectedInventory = Set.remove "manifest.json" expectedFiles
                let actualInventory = inventory.Keys |> Set.ofSeq
                for path in Set.difference expectedInventory actualInventory do errors.Add($"manifest.json: payload is not inventoried: {path}")
                for path in Set.difference actualInventory expectedInventory do errors.Add($"manifest.json: undeclared inventory entry: {path}")
                let payloadPaths = Set.remove "manifest.json" expectedFiles |> Set.toArray
                let payloadDigests =
                    payloadPaths
                    |> Array.Parallel.map (fun path ->
                        if inventory.ContainsKey(path) && actualFiles.Contains(path) then
                            sha256File (Path.Combine(root, path.Replace('/', Path.DirectorySeparatorChar)))
                        else null)
                for path, actualDigest in Array.zip payloadPaths payloadDigests do
                    match inventory.TryGetValue(path) with
                    | false, _ -> errors.Add($"manifest.json: payload is not inventoried: {path}")
                    | true, struct(size, digest) when actualFiles.Contains(path) ->
                        let absolute = Path.Combine(root, path.Replace('/', Path.DirectorySeparatorChar))
                        if FileInfo(absolute).Length <> size then errors.Add($"{path}: size does not match manifest")
                        if actualDigest <> digest then errors.Add($"{path}: SHA-256 does not match manifest")
                    | _ -> ()
                let rowCounts = Dictionary<string,int64>(StringComparer.Ordinal)
                match manifest.TryGetProperty("relations") with
                | true, relations when relations.ValueKind = JsonValueKind.Array ->
                    for item in relations.EnumerateArray() do
                        rowCounts.Add(item.GetProperty("name").GetString(), item.GetProperty("row_count").GetInt64())
                | _ -> errors.Add("manifest.json: relations are missing")
                let declaredRelations = rowCounts.Keys |> Set.ofSeq
                let expectedRelations = Schema.relationNames |> Set.ofArray
                for name in Set.difference expectedRelations declaredRelations do errors.Add($"manifest.json: relation is not declared: {name}")
                for name in Set.difference declaredRelations expectedRelations do errors.Add($"manifest.json: unknown relation declaration: {name}")
                for relation in Schema.relations do
                    match rowCounts.TryGetValue(relation.name) with
                    | true, count -> validateParquet errors root relation count
                    | _ -> errors.Add($"manifest.json: relation is not declared: {relation.name}")
                    reclaimValidationMemory ()
                validateSources errors manifest
                // Reference and semantic checks need structurally valid relation files.
                if errors.Count = 0 then
                    validateForeignKeys errors root
                    validateValues errors root
                    reclaimValidationMemory ()
                    validateTripParts errors root
                    reclaimValidationMemory ()
                    if File.Exists(Path.Combine(root, "gtfs.zip")) then validateGtfsProjection errors root
                    reclaimValidationMemory ()
            validateZip errors root
            let diagnosticsPath = Path.Combine(root, "diagnostics.json")
            if File.Exists(diagnosticsPath) && FileInfo(diagnosticsPath).Length > 1024L * 1024L then
                errors.Add("diagnostics.json: bounded production summary exceeds 1 MiB")
            let packageBytes =
                Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
                |> Seq.sumBy (fun path -> FileInfo(path).Length)
            if packageBytes > 2L * 1024L * 1024L * 1024L then
                errors.Add("production package exceeds the 2 GiB acceptance limit")
            { errors = errors.ToArray(); fileCount = actualFiles.Count; relationCount = Schema.relations.Length }

    let validatePackage (directory: string) =
        let result = inspect directory
        if not result.isValid then
            invalidArg "directory" ("Invalid JrUtil production package:" + Environment.NewLine + String.Join(Environment.NewLine, result.errors))
        result

    let compareByteIdentical (leftDirectory: string) (rightDirectory: string) =
        let files root =
            Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
            |> Seq.map (fun path -> relative root path, path)
            |> Map.ofSeq
        let leftRoot, rightRoot = Path.GetFullPath(leftDirectory), Path.GetFullPath(rightDirectory)
        validatePackage leftRoot |> ignore
        validatePackage rightRoot |> ignore
        let left, right = files leftRoot, files rightRoot
        if (left |> Map.toSeq |> Seq.map fst |> Set.ofSeq) <> (right |> Map.toSeq |> Seq.map fst |> Set.ofSeq) then
            invalidArg "rightDirectory" "Package file inventories differ"
        for KeyValue(path, leftFile) in left do
            let rightFile = right.[path]
            if FileInfo(leftFile).Length <> FileInfo(rightFile).Length
               || sha256File leftFile <> sha256File rightFile then
                invalidArg "rightDirectory" $"Package payload differs: {path}"

