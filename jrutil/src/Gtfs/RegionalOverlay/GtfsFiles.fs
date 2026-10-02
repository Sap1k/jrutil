// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Reading GTFS archives and descriptors; writing overlay CSV files.
module internal JrUtil.RegionalOverlay.GtfsFiles

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open System.Security.Cryptography
open System.Text
open System.Text.Json
open NodaTime
open NodaTime.Text
open JrUtil.GtfsModel
open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Values

let csvFields archivePath fileName =
    seq {
        let openStream () =
            if Directory.Exists(archivePath) then
                let path = Path.Combine(archivePath, fileName)
                if not (File.Exists(path)) then None
                else Some (File.OpenRead(path) :> Stream, None)
            else
                let archive = ZipFile.OpenRead(archivePath)
                let entry =
                    archive.Entries
                    |> Seq.tryFind (fun value ->
                        value.FullName.Replace('\\', '/').TrimStart('/').Equals(
                            fileName.Replace('\\', '/').TrimStart('/'),
                            StringComparison.OrdinalIgnoreCase))
                match entry with
                | Some value -> Some (value.Open(), Some archive)
                | None -> archive.Dispose(); None
        match openStream () with
        | None -> ()
        | Some (stream, owner) ->
            use stream = stream
            use _owner = owner |> Option.map (fun value -> value :> IDisposable) |> Option.toObj
            use reader = new StreamReader(stream, Encoding.UTF8, true, 64 * 1024)
            let records = JrUtil.DelimitedText.CsvRecordReader(reader, fileName)
            let header = records.Read()
            if not (isNull header) then
                if header |> Array.exists String.IsNullOrWhiteSpace then
                    invalidOp $"{fileName} has an empty column name"
                if header |> Array.distinct |> Array.length <> header.Length then
                    invalidOp $"{fileName} has duplicate column names"
                for fields in records.Records() do
                    if not (fields.Length = 1 && fields.[0] = "") then
                        yield header, fields
    }

let textLines archivePath fileName = seq {
    let openStream () =
        if Directory.Exists(archivePath) then
            let path = Path.Combine(archivePath, fileName)
            if File.Exists(path) then Some (File.OpenRead(path) :> Stream, None) else None
        else
            let archive = ZipFile.OpenRead(archivePath)
            match archive.Entries |> Seq.tryFind (fun entry -> entry.FullName.Replace('\\', '/').Equals(fileName, StringComparison.OrdinalIgnoreCase)) with
            | Some entry -> Some (entry.Open(), Some archive)
            | None -> archive.Dispose(); None
    match openStream () with
    | None -> ()
    | Some (stream, owner) ->
        use stream = stream
        use _owner = owner |> Option.map (fun value -> value :> IDisposable) |> Option.toObj
        use reader = new StreamReader(stream, Encoding.UTF8, true)
        while not reader.EndOfStream do yield reader.ReadLine()
}

let csvRows archivePath fileName =
    csvFields archivePath fileName
    |> Seq.map (fun (header, fields) ->
        let row = CsvRow(StringComparer.Ordinal)
        for index in 0 .. header.Length - 1 do
            row.[header.[index]] <- if index < fields.Length then fields.[index] else ""
        row)

/// Project a table into a fixed header once, without a dictionary per input row.
let csvValues archivePath fileName (columns: string array) = seq {
    let mutable indexes = [||]
    for header, fields in csvFields archivePath fileName do
        if indexes.Length = 0 then
            indexes <- columns |> Array.map (fun column -> header |> Array.tryFindIndex ((=) column) |> Option.defaultValue -1)
        yield indexes |> Array.map (fun index -> if index >= 0 && index < fields.Length then fields.[index] else "")
}

let requireTable archivePath fileName =
    let rows = csvRows archivePath fileName |> Seq.toArray
    if rows.Length = 0 then invalidOp $"Required GTFS table is empty or missing: {fileName}"
    rows

let readDescriptor (path: string) =
    use document = JsonDocument.Parse(File.ReadAllText(path))
    let root = document.RootElement
    let property (names: string list) : JsonElement =
        names
        |> Seq.tryPick (fun name ->
            match root.TryGetProperty(name) with
            | true, value -> Some value
            | _ -> None)
        |> Option.defaultWith (fun () ->
            let joinedNames = String.concat "/" names
            invalidOp $"Descriptor is missing {joinedNames}")
    {
        retrievedAt = property ["retrieved_at"; "retrievedAt"] |> fun value -> value.GetString() |> DateTimeOffset.Parse
        payloadSha256 = property ["payload_sha256"; "payloadSha256"; "sha256"] |> fun value -> value.GetString().ToLowerInvariant()
    }

let rec copyDirectory source target =
    Directory.CreateDirectory(target) |> ignore
    for file in Directory.EnumerateFiles(source) do
        File.Copy(file, Path.Combine(target, Path.GetFileName(file)), false)
    for directory in Directory.EnumerateDirectories(source) do
        copyDirectory directory (Path.Combine(target, Path.GetFileName(directory)))

let writeCsvRow (writer: TextWriter) (fields: seq<string>) =
    let mutable first = true
    for field in fields do
        if not first then writer.Write(',')
        first <- false
        writer.Write('"')
        if not (isNull field) then
            if field.Contains('"') then writer.Write(field.Replace("\"", "\"\""))
            else writer.Write(field)
        writer.Write('"')
    writer.WriteLine()

let writeValues (path: string) (columns: string array) (rows: string array seq) =
    Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
    use writer = new StreamWriter(path, false, new UTF8Encoding(false))
    writer.NewLine <- "\n"
    writeCsvRow writer columns
    for row in rows do writeCsvRow writer row

let writeRows (path: string) (columns: string array) (rows: CsvRow seq) =
    writeValues path columns (rows |> Seq.map (fun row -> columns |> Array.map (rowValue row)))

let columnsOf archivePath fileName =
    let row = csvRows archivePath fileName |> Seq.tryHead
    match row with
    | Some value -> value.Keys |> Seq.toArray
    | None -> [||]

let optionText (value: string) =
    if String.IsNullOrWhiteSpace(value) then None else Some value

let modeClass (routeType: string) =
    match Int32.TryParse(routeType, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | false, _ -> "other:" + routeType
    | true, value when value = 0 || (value >= 900 && value <= 906) -> "tram"
    | true, value when value = 1 || (value >= 400 && value <= 405) -> "metro"
    | true, value when value = 2 || (value >= 100 && value <= 117) -> "heavy-rail"
    | true, value when value = 3 || (value >= 700 && value <= 716) -> "bus"
    | true, value when value = 4 || (value >= 1000 && value <= 1021) -> "ferry"
    | true, value when value = 11 || value = 800 -> "trolleybus"
    | true, value when value = 12 || (value >= 1200 && value <= 1207) -> "other-guided"
    | true, value -> "other:" + string value

let datesFor (window: DateWindow) (values: seq<bool>) =
    values
    |> Seq.mapi (fun index active -> if active then Some window.dates.[index] else None)
    |> Seq.choose id
    |> Seq.toArray
