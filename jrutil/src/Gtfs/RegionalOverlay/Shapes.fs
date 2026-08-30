// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
module internal JrUtil.RegionalOverlay.Shapes

open System
open System.IO
open System.Text
open System.Globalization
open System.Security.Cryptography
open System.Collections.Generic
open JrUtil.RegionalOverlay.Support

let columns = [| "shape_id"; "shape_pt_lat"; "shape_pt_lon"; "shape_pt_sequence"; "shape_dist_traveled" |]

type Store = {
    path: string
    locations: Dictionary<string, int64 * int64>
    outputBySource: Dictionary<string, string>
}

/// Sort before source-call analysis, when its large indexes do not exist yet.
let spool scratch payloadPath =
    let comparePoints (a: string array) (b: string array) =
        let shape = StringComparer.Ordinal.Compare(a.[0], b.[0])
        if shape <> 0 then shape
        else
            let sequence (value: string) =
                match Int32.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                | true, number -> number
                | _ -> Int32.MinValue // Only selected shapes are validated during projection.
            compare (sequence a.[3]) (sequence b.[3])
    Scratch.sortRows scratch comparePoints Scratch.defaultBufferBytes (csvValues payloadPath "shapes.txt" columns)

let prepare (scratch: Scratch.Storage) sortedPoints (selected: Set<string>) diagnostics =
    let path = scratch.NewFile()
    let locations = Dictionary<string, int64 * int64>(StringComparer.Ordinal)
    let outputBySource = Dictionary<string, string>(StringComparer.Ordinal)
    use writer = new BinaryWriter(File.Create(path), Encoding.UTF8)
    let input = sortedPoints |> Seq.filter (fun (row: string array) -> selected.Contains(row.[0]))
    use points = input.GetEnumerator()
    let mutable available = points.MoveNext()
    for shapeId in selected |> Seq.sort do
        if not available || points.Current.[0] <> shapeId then
            addDiagnostic diagnostics "shape_missing" shapeId "Referenced shape has no points"
        else
            use hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256)
            let start = writer.BaseStream.Position
            let mutable count = 0
            let mutable valid = true
            let mutable previousSequence = Int32.MinValue
            let mutable previousDistance = None
            let mutable suppliedDistances = 0
            while available && points.Current.[0] = shapeId do
                let row = points.Current
                let sequence = parseInt row.[3]
                let latitude, longitude, distance = parseDecimalOpt row.[1], parseDecimalOpt row.[2], parseDecimalOpt row.[4]
                valid <- valid && (count = 0 || sequence > previousSequence)
                previousSequence <- sequence
                match latitude, longitude with
                | Some lat, Some lon when lat >= -90m && lat <= 90m && lon >= -180m && lon <= 180m -> ()
                | _ -> valid <- false
                match distance with
                | Some value ->
                    suppliedDistances <- suppliedDistances + 1
                    match previousDistance with
                    | Some previous when value < previous -> valid <- false
                    | _ -> ()
                    previousDistance <- Some value
                | None -> ()
                let canonical value = value |> Option.map (fun (v: decimal) -> v.ToString("G29", CultureInfo.InvariantCulture)) |> Option.defaultValue ""
                let encoded = (if count = 0 then "" else ";") + canonical latitude + "," + canonical longitude + "," + canonical distance
                hash.AppendData(Encoding.UTF8.GetBytes(encoded))
                Scratch.writeRow writer row
                count <- count + 1
                available <- points.MoveNext()
            if not valid || count < 2 || (suppliedDistances <> 0 && suppliedDistances <> count) then
                addDiagnostic diagnostics "shape_invalid" shapeId "Points, ordering, coordinates, or distances are invalid"
            else
                let outputId = "overlay:shape:" + Convert.ToHexString(hash.GetHashAndReset()).ToLowerInvariant().Substring(0, 20)
                outputBySource.[shapeId] <- outputId
                if not (locations.ContainsKey(outputId)) then locations.[outputId] <- start, writer.BaseStream.Position
    { path = path; locations = locations; outputBySource = outputBySource }

let rows store outputId = seq {
    use reader = new BinaryReader(File.OpenRead(store.path), Encoding.UTF8)
    let start, finish = store.locations.[outputId]
    reader.BaseStream.Position <- start
    while reader.BaseStream.Position < finish do
        let row = Scratch.readRow reader
        row.[0] <- outputId
        yield row
}
