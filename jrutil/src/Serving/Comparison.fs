// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open Parquet
open Parquet.Schema

open JrUtil.RegionalOverlay.Support

/// Semantic comparison of two production packages. Serving relations are
/// compared row by row on their declared primary keys, GTFS tables as multisets
/// of rows (column order and empty-versus-absent columns ignored), and the
/// manifest and diagnostics as canonical JSON without payload hashes. Physical
/// details such as Parquet row groups, compression and ZIP layout are ignored.
module Comparison =
    type Difference = {
        /// file, manifest, relation, gtfs or diagnostics
        kind: string
        /// Path, relation name, GTFS table or JSON key the difference belongs to
        subject: string
        summary: string
        samples: string list
    }

    type Report = {
        differences: Difference list
        unexpected: Difference list
        compared: string list
    }
    with member value.isEquivalent = List.isEmpty value.unexpected

    /// An expectation line is `<kind> <glob>`, e.g. `file extensions/*` or
    /// `relation location`. `column <glob>` ignores matching serving columns
    /// in every relation, including primary-key columns. `#` starts a comment.
    type Expectation = { kind: string; pattern: string }

    let parseExpectations (lines: string seq) =
        lines
        |> Seq.map (fun line -> match line.IndexOf('#') with -1 -> line | index -> line.Substring(0, index))
        |> Seq.map (fun line -> line.Trim())
        |> Seq.filter (fun line -> line <> "")
        |> Seq.map (fun line ->
            match line.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries) with
            | [| kind; pattern |] -> { kind = kind; pattern = pattern.Trim() }
            | _ -> invalidArg "lines" $"Malformed expectation line: {line}")
        |> Seq.toList

    let private globMatches (pattern: string) (value: string) =
        let regex = "^" + (Text.RegularExpressions.Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".")) + "$"
        Text.RegularExpressions.Regex.IsMatch(value, regex)

    let private isExpected expectations (difference: Difference) =
        expectations |> List.exists (fun expectation ->
            expectation.kind = difference.kind && globMatches expectation.pattern difference.subject)

    [<Literal>]
    let private SampleLimit = 5

    // ---- JSON documents -------------------------------------------------

    /// Manifest keys that are a function of payload bytes rather than meaning.
    let private volatileManifestKeys = set [ "files"; "feed_version" ]

    let rec private canonicalJson (node: JsonNode) : string =
        match node with
        | null -> "null"
        | :? JsonObject as value ->
            value
            |> Seq.sortWith (fun left right -> StringComparer.Ordinal.Compare(left.Key, right.Key))
            |> Seq.map (fun pair -> JsonSerializer.Serialize(pair.Key) + ":" + canonicalJson pair.Value)
            |> String.concat ","
            |> sprintf "{%s}"
        | :? JsonArray as value -> value |> Seq.map canonicalJson |> String.concat "," |> sprintf "[%s]"
        | value -> value.ToJsonString()

    let private readJsonObject (path: string) =
        if File.Exists(path) then
            match JsonNode.Parse(File.ReadAllText(path)) with
            | :? JsonObject as value -> Some value
            | _ -> invalidOp $"{path} is not a JSON object"
        else None

    let private truncate (value: string) = if value.Length > 300 then value.Substring(0, 300) + "…" else value

    let private compareJsonObjects kind (ignored: Set<string>) (left: JsonObject option) (right: JsonObject option) =
        match left, right with
        | None, None -> []
        | Some _, None | None, Some _ ->
            [ { kind = kind; subject = "*"; summary = "present in only one package"; samples = [] } ]
        | Some left, Some right ->
            let keys (value: JsonObject) = value |> Seq.map (fun pair -> pair.Key) |> Set.ofSeq
            Set.union (keys left) (keys right)
            |> Set.filter (fun key -> not (ignored.Contains key))
            |> Seq.choose (fun key ->
                let value (document: JsonObject) =
                    let mutable node: JsonNode = null
                    if document.TryGetPropertyValue(key, &node) then Some (canonicalJson node) else None
                match value left, value right with
                | l, r when l = r -> None
                | l, r ->
                    let show = Option.map truncate >> Option.defaultValue "<absent>"
                    Some { kind = kind; subject = key; summary = "value differs"
                           samples = [ "left:  " + show l; "right: " + show r ] })
            |> Seq.toList

    // ---- Serving relations ------------------------------------------------

    let private readBoxed<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> ValueType>
                         (group: ParquetRowGroupReader) (field: DataField) count : obj array =
        if field.IsNullable then
            let values = Array.zeroCreate<Nullable<'T>> count
            group.ReadAsync<'T>(field, values.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
            values |> Array.map (fun value -> if value.HasValue then box value.Value else null)
        else
            let values = Array.zeroCreate<'T> count
            group.ReadAsync<'T>(field, values.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
            values |> Array.map box

    let private readColumn (group: ParquetRowGroupReader) (field: DataField) count : obj array =
        let normalizeDate (values: obj array) =
            values |> Array.map (function :? DateTime as value -> box (DateOnly.FromDateTime value) | value -> value)
        if field.ClrType = typeof<string> then
            let values = Array.zeroCreate<string> count
            group.ReadAsync(field, values.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
            values |> Array.map box
        elif field.ClrType = typeof<int> then readBoxed<int> group field count
        elif field.ClrType = typeof<int64> then readBoxed<int64> group field count
        elif field.ClrType = typeof<int16> then readBoxed<int16> group field count
        elif field.ClrType = typeof<double> then readBoxed<double> group field count
        elif field.ClrType = typeof<bool> then readBoxed<bool> group field count
        elif field.ClrType = typeof<DateOnly> then readBoxed<DateOnly> group field count
        elif field.ClrType = typeof<DateTime> then readBoxed<DateTime> group field count |> normalizeDate
        else invalidOp $"Unsupported serving field type {field.ClrType.FullName} for {field.Name}"

    /// Rows of one relation projected onto `columns`, in file (= primary key) order.
    let private relationRows (path: string) (columns: string array) : seq<obj array> = seq {
        use stream = File.OpenRead(path)
        let reader = ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let fields = reader.Schema.DataFields |> Seq.map (fun field -> field.Name, field) |> dict
            for groupIndex in 0 .. reader.RowGroupCount - 1 do
                use group = reader.OpenRowGroupReader(groupIndex)
                let count = int group.RowCount
                let values =
                    columns |> Array.map (fun column ->
                        match fields.TryGetValue(column) with
                        | true, field -> readColumn group field count
                        | _ -> Array.create count null)
                for row in 0 .. count - 1 do
                    yield values |> Array.map (fun column -> column.[row])
        finally
            reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
    }

    let private showCell (value: obj) =
        match value with
        | null -> "∅"
        | :? double as value -> value.ToString("R", Globalization.CultureInfo.InvariantCulture)
        | :? DateOnly as value -> value.ToString("yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture)
        | value -> Convert.ToString(value, Globalization.CultureInfo.InvariantCulture)

    let private showRow (columns: string array) (row: obj array) =
        Array.map2 (fun column value -> column + "=" + showCell value) columns row |> String.concat " " |> truncate

    /// Type-tagged cell text, so that e.g. a null and the string "∅" differ.
    let private cellToken (value: obj) =
        match value with
        | null -> "\u0000"
        | :? string as value -> "s" + value
        | value -> "v" + showCell value

    let private hash128 (text: string) =
        let digest = SHA256.HashData(Encoding.UTF8.GetBytes(text))
        struct(BitConverter.ToUInt64(digest, 0), BitConverter.ToUInt64(digest, 8))

    /// Order-independent: rows are matched on a hash of their primary key, so
    /// packages whose relations are written in any order compare equal.
    let private compareRelation (ignoredColumn: string -> bool) (leftRoot: string) (rightRoot: string) (relation: Schema.Relation) =
        let path root = Path.Combine(root, "serving", relation.name + ".parquet")
        let columns = relation.fields |> Array.map (fun field -> field.name)
        let keyIndexes =
            relation.primaryKey
            |> Array.map (fun key -> Array.IndexOf(columns, key))
            |> Array.filter (fun index -> not (ignoredColumn columns.[index]))
        let valueIndexes = [| 0 .. columns.Length - 1 |] |> Array.filter (fun index -> not (ignoredColumn columns.[index]))
        let keyHash (row: obj array) = keyIndexes |> Array.map (fun index -> cellToken row.[index]) |> String.concat "\u001f" |> hash128
        let rowHash (row: obj array) = valueIndexes |> Array.map (fun index -> cellToken row.[index]) |> String.concat "\u001f" |> hash128
        let showKey (row: obj array) = keyIndexes |> Array.map (fun index -> columns.[index] + "=" + showCell row.[index]) |> String.concat " "
        let sample (samples: ResizeArray<string>) text = if samples.Count < SampleLimit then samples.Add(text)
        let duplicates = ResizeArray<string>()

        // Pass 1: remember every left row by key.
        let left = Dictionary<struct(uint64 * uint64), struct(struct(uint64 * uint64) * bool)>()
        for row in relationRows (path leftRoot) columns do
            let key = keyHash row
            if left.ContainsKey(key) then sample duplicates ("duplicate key in left: " + showKey row)
            else left.[key] <- struct(rowHash row, false)

        // Pass 2: match right rows, keeping a few changed keys for field-level samples.
        let added, rightSamples = ResizeArray<string>(), Dictionary<struct(uint64 * uint64), obj array>()
        let seenRight = HashSet<struct(uint64 * uint64)>()
        let mutable addedCount, changedCount, matched = 0L, 0L, 0L
        for row in relationRows (path rightRoot) columns do
            let key = keyHash row
            if not (seenRight.Add(key)) then sample duplicates ("duplicate key in right: " + showKey row)
            else
                match left.TryGetValue(key) with
                | true, struct(leftHash, _) ->
                    matched <- matched + 1L
                    left.[key] <- struct(leftHash, true)
                    if leftHash <> rowHash row then
                        changedCount <- changedCount + 1L
                        if rightSamples.Count < SampleLimit then rightSamples.[key] <- row
                | _ ->
                    addedCount <- addedCount + 1L
                    sample added ("+ " + showRow columns row)
        let removedCount = left.Values |> Seq.filter (fun struct(_, seen) -> not seen) |> Seq.length |> int64

        // Pass 3 (only when needed): recover left rows for readable samples.
        let removed, changed = ResizeArray<string>(), ResizeArray<string>()
        if removedCount > 0L || rightSamples.Count > 0 then
            for row in relationRows (path leftRoot) columns do
                let key = keyHash row
                match rightSamples.TryGetValue(key) with
                | true, right ->
                    let fields =
                        [ for index in valueIndexes do
                            if cellToken row.[index] <> cellToken right.[index] then
                                yield $"{columns.[index]}: {showCell row.[index]} -> {showCell right.[index]}" ]
                        |> String.concat "; "
                    sample changed ("~ " + showKey row + " | " + truncate fields)
                | _ ->
                    match left.TryGetValue(key) with
                    | true, struct(_, false) -> sample removed ("- " + showRow columns row)
                    | _ -> ()

        if removedCount + addedCount + changedCount = 0L && duplicates.Count = 0 then None
        else
            Some { kind = "relation"; subject = relation.name
                   summary = $"{removedCount} removed, {addedCount} added, {changedCount} changed ({matched} rows matched by key)"
                             + (if duplicates.Count > 0 then "; duplicate primary keys" else "")
                   samples = List.ofSeq duplicates @ List.ofSeq removed @ List.ofSeq added @ List.ofSeq changed }

    // ---- GTFS -----------------------------------------------------------

    /// Order-independent row identity: non-empty cells sorted by column name.
    let private canonicalGtfsRow (header: string array) (fields: string array) =
        let pairs = ResizeArray<string>(header.Length)
        for index in 0 .. header.Length - 1 do
            let value = if index < fields.Length then fields.[index] else ""
            if value <> "" then pairs.Add(header.[index] + "=" + value)
        pairs.Sort(StringComparer.Ordinal)
        String.Join("\u001f", pairs)

    let private rowHash (row: string) =
        let digest = SHA256.HashData(Encoding.UTF8.GetBytes(row))
        struct(BitConverter.ToUInt64(digest, 0), BitConverter.ToUInt64(digest, 8))

    let private gtfsEntries (zipPath: string) =
        if File.Exists(zipPath) then
            use archive = ZipFile.OpenRead(zipPath)
            archive.Entries |> Seq.map (fun entry -> entry.FullName) |> Set.ofSeq
        else Set.empty

    /// `leftSource`/`rightSource` are ZIP archives or directories holding `table`.
    let private compareCsvTable kind (leftSource: string) (rightSource: string) (table: string) =
        // Counting by row hash keeps memory at ~one dictionary entry per
        // distinct row; a second pass recovers readable samples.
        let counts = Dictionary<struct(uint64 * uint64), int>()
        let mutable leftRows, rightRows = 0L, 0L
        for header, fields in csvFields leftSource table do
            let key = rowHash (canonicalGtfsRow header fields)
            leftRows <- leftRows + 1L
            match counts.TryGetValue(key) with
            | true, count -> counts.[key] <- count + 1
            | _ -> counts.[key] <- 1
        for header, fields in csvFields rightSource table do
            let key = rowHash (canonicalGtfsRow header fields)
            rightRows <- rightRows + 1L
            match counts.TryGetValue(key) with
            | true, count -> counts.[key] <- count - 1
            | _ -> counts.[key] <- -1
        let differing = counts |> Seq.filter (fun pair -> pair.Value <> 0) |> Seq.map (fun pair -> pair.Key, pair.Value) |> dict
        if differing.Count = 0 then None
        else
            let removedRows = differing.Values |> Seq.filter (fun count -> count > 0) |> Seq.sumBy int64
            let addedRows = differing.Values |> Seq.filter (fun count -> count < 0) |> Seq.sumBy (fun count -> int64 -count)
            let samples source sign keep =
                csvFields source table
                |> Seq.map (fun (header, fields) -> canonicalGtfsRow header fields)
                |> Seq.filter (fun row ->
                    match differing.TryGetValue(rowHash row) with
                    | true, count -> keep count
                    | _ -> false)
                |> Seq.distinct
                |> Seq.truncate SampleLimit
                |> Seq.map (fun row -> sign + " " + truncate (row.Replace("\u001f", " ")))
                |> Seq.toList
            Some { kind = kind; subject = table
                   summary = $"{removedRows} rows only in left, {addedRows} rows only in right (left {leftRows}, right {rightRows})"
                   samples = samples leftSource "-" (fun count -> count > 0) @ samples rightSource "+" (fun count -> count < 0) }

    // ---- Package ----------------------------------------------------------

    let private relativeFiles (root: string) =
        Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
        |> Seq.map (fun path -> Path.GetRelativePath(root, path).Replace('\\', '/'))
        |> Set.ofSeq

    let comparePackages (expectations: Expectation list) (leftDirectory: string) (rightDirectory: string) =
        let leftRoot, rightRoot = Path.GetFullPath(leftDirectory), Path.GetFullPath(rightDirectory)
        let ignoredColumn column =
            expectations |> List.exists (fun expectation -> expectation.kind = "column" && globMatches expectation.pattern column)
        for root in [ leftRoot; rightRoot ] do
            if not (Directory.Exists(root)) then invalidArg "directory" $"Package directory does not exist: {root}"
        let differences = ResizeArray<Difference>()
        let compared = ResizeArray<string>()

        let leftFiles, rightFiles = relativeFiles leftRoot, relativeFiles rightRoot
        for path in Set.difference leftFiles rightFiles do
            differences.Add { kind = "file"; subject = path; summary = "only in left package"; samples = [] }
        for path in Set.difference rightFiles leftFiles do
            differences.Add { kind = "file"; subject = path; summary = "only in right package"; samples = [] }

        compared.Add("manifest.json")
        differences.AddRange(
            compareJsonObjects "manifest" volatileManifestKeys
                (readJsonObject (Path.Combine(leftRoot, "manifest.json")))
                (readJsonObject (Path.Combine(rightRoot, "manifest.json"))))
        compared.Add("diagnostics.json")
        differences.AddRange(
            compareJsonObjects "diagnostics" Set.empty
                (readJsonObject (Path.Combine(leftRoot, "diagnostics.json")))
                (readJsonObject (Path.Combine(rightRoot, "diagnostics.json"))))

        for relation in Schema.relations do
            let file = "serving/" + relation.name + ".parquet"
            if leftFiles.Contains file && rightFiles.Contains file then
                compared.Add(file)
                compareRelation ignoredColumn leftRoot rightRoot relation |> Option.iter differences.Add

        let leftZip, rightZip = Path.Combine(leftRoot, "gtfs.zip"), Path.Combine(rightRoot, "gtfs.zip")
        let leftTables, rightTables = gtfsEntries leftZip, gtfsEntries rightZip
        for table in Set.difference leftTables rightTables do
            differences.Add { kind = "gtfs"; subject = table; summary = "table only in left gtfs.zip"; samples = [] }
        for table in Set.difference rightTables leftTables do
            differences.Add { kind = "gtfs"; subject = table; summary = "table only in right gtfs.zip"; samples = [] }
        for table in Set.intersect leftTables rightTables do
            compared.Add("gtfs.zip/" + table)
            compareCsvTable "gtfs" leftZip rightZip table |> Option.iter differences.Add

        for file in Set.intersect leftFiles rightFiles do
            if file.StartsWith("extensions/", StringComparison.Ordinal) && file.EndsWith(".txt", StringComparison.Ordinal) then
                compared.Add(file)
                let table = file.Substring("extensions/".Length)
                compareCsvTable "extension" (Path.Combine(leftRoot, "extensions")) (Path.Combine(rightRoot, "extensions")) table
                |> Option.iter differences.Add

        let all = List.ofSeq differences
        { differences = all
          unexpected = all |> List.filter (isExpected expectations >> not)
          compared = List.ofSeq compared }
