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
                         (group: ParquetRowGroupReader) field count =
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
                if metadataValue reader.CustomMetadata "obehy.schema_version" <> string Schema.ServingSchemaVersion then
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
                requireInt "serving_schema_version" Schema.ServingSchemaVersion
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

