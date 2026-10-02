// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
module internal JrUtil.RegionalOverlay.Scratch

open System
open System.IO
open System.Diagnostics
open System.Runtime
open System.Text
open System.Collections.Generic

/// One execution owns all scratch files, including files left by a failed spill.
type Storage(parent: string) =
    let directory = Path.Combine(parent, ".overlay-scratch-" + Guid.NewGuid().ToString("N"))
    let resources = ResizeArray<IDisposable>()
    do Directory.CreateDirectory(directory) |> ignore
    member _.Own(resource: IDisposable) = resources.Add(resource)
    member _.NewFile() = Path.Combine(directory, Guid.NewGuid().ToString("N"))
    member _.Directory = directory
    member _.Flush() =
        for resource in resources do
            match resource with | :? BinaryWriter as writer -> writer.Flush() | _ -> ()
    interface IDisposable with
        member _.Dispose() =
            let mutable failure = None
            for resource in resources do
                try resource.Dispose() with error -> failure <- Some error
            try
                if Directory.Exists(directory) then Directory.Delete(directory, true)
            finally
                match failure with | Some error -> raise error | None -> ()

let writeRow (writer: BinaryWriter) (row: string array) =
    writer.Write(row.Length)
    for field in row do writer.Write(field)

let readRow (reader: BinaryReader) =
    Array.init (reader.ReadInt32()) (fun _ -> reader.ReadString())

let private streamBufferBytes = 64 * 1024

/// Rows flushed before enumeration starts; the length is read once, not per row.
let rows path = seq {
    use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, streamBufferBytes, FileOptions.SequentialScan)
    use reader = new BinaryReader(stream, Encoding.UTF8)
    let length = stream.Length
    while stream.Position < length do yield readRow reader
}

/// Append-only evidence. Readers see a flushed snapshot; no formatted rows are retained.
type RowLog(storage: Storage) =
    let path = storage.NewFile()
    let writer = new BinaryWriter(new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.Read), Encoding.UTF8)
    do storage.Own(writer)
    member _.Add(row: string array) = writeRow writer row
    member _.Rows = writer.Flush(); rows path
    interface IDisposable with
        member _.Dispose() = writer.Dispose()

let private createSpill path =
    new BinaryWriter(new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.None, streamBufferBytes), Encoding.UTF8)

/// At most this many runs are merged at once, so one level suffices up to
/// this many spills.
let private mergeFanIn = 128

/// Stable external sort with a row-size budget, ordered by a key extracted
/// once per row (per run read), not once per comparison. Equal keys keep
/// their input order; one oversized input row is allowed.
let sortRowsBy (storage: Storage) (key: string array -> 'Key) (compareKeys: 'Key -> 'Key -> int) bufferBytes (input: seq<string array>) =
    let reclaimGate =
        JrUtil.MemoryReclaimGate(
            3_250_000_000L, JrUtil.MemoryReclaim.DefaultMinimumGrowthBytes, JrUtil.MemoryReclaim.compactOnce)
    let spill (values: ResizeArray<struct('Key * int64 * string array)>) =
        let ordered = values.ToArray()
        Array.Sort(ordered, Comparison(fun (struct(leftKey, leftOrdinal, _)) (struct(rightKey, rightOrdinal, _)) ->
            let compared = compareKeys leftKey rightKey
            if compared <> 0 then compared else compare leftOrdinal rightOrdinal))
        let path = storage.NewFile()
        use writer = createSpill path
        for struct(_, _, row) in ordered do writeRow writer row
        path
    // Ties go to the earlier run; runs hold consecutive input ranges, so the merge is stable.
    let runOrder =
        { new IComparer<struct('Key * int)> with
            member _.Compare(struct(leftKey, leftRun), struct(rightKey, rightRun)) =
                let compared = compareKeys leftKey rightKey
                if compared <> 0 then compared else compare leftRun rightRun }
    let merge (paths: string array) =
        let output = storage.NewFile()
        let readers = paths |> Array.map (fun path -> (rows path).GetEnumerator())
        try
            use writer = createSpill output
            let queue = PriorityQueue<int, struct('Key * int)>(runOrder)
            for index in 0 .. readers.Length - 1 do
                if readers.[index].MoveNext() then
                    queue.Enqueue(index, struct(key readers.[index].Current, index))
            let mutable index = 0
            let mutable priority = Unchecked.defaultof<struct('Key * int)>
            while queue.TryDequeue(&index, &priority) do
                writeRow writer readers.[index].Current
                if readers.[index].MoveNext() then
                    queue.Enqueue(index, struct(key readers.[index].Current, index))
        finally
            for reader in readers do reader.Dispose()
        for path in paths do File.Delete(path)
        output
    let buffer = ResizeArray<struct('Key * int64 * string array)>()
    let runs = ResizeArray<string>()
    let mutable size = 0L
    let mutable ordinal = 0L
    for row in input do
        buffer.Add(struct(key row, ordinal, row))
        ordinal <- ordinal + 1L
        let mutable characters = 0L
        for value in row do characters <- characters + int64 value.Length
        size <- size + 48L + int64 row.Length * 32L + characters * 2L
        if size >= bufferBytes then
            runs.Add(spill buffer)
            buffer.Clear()
            size <- 0L
            // The just-written run no longer needs its row/string arrays.
            // Return them before admitting the next national-sized chunk.
            reclaimGate.Check() |> ignore
    if buffer.Count > 0 then runs.Add(spill buffer)
    let mutable paths = runs.ToArray()
    while paths.Length > 1 do paths <- paths |> Array.chunkBySize mergeFanIn |> Array.map merge
    if paths.Length = 0 then Seq.empty
    else seq { try yield! rows paths.[0] finally File.Delete(paths.[0]) }

/// `sortRowsBy` comparing whole rows.
let sortRows (storage: Storage) (compareRows: string array -> string array -> int) bufferBytes input =
    sortRowsBy storage id compareRows bufferBytes input

let defaultBufferBytes = 64L * 1024L * 1024L
