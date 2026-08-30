// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
module internal JrUtil.RegionalOverlay.Scratch

open System
open System.IO
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

let rows path = seq {
    use reader = new BinaryReader(new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite), Encoding.UTF8)
    while reader.BaseStream.Position < reader.BaseStream.Length do yield readRow reader
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

/// Stable external sorting, with a 64 MiB row budget and at most 16 open merge inputs.
/// The estimate includes row/string overhead; one oversized input row is allowed.
let sortRows (storage: Storage) (compareRows: string array -> string array -> int) bufferBytes input =
    let spill (values: ResizeArray<int64 * string array>) =
        let ordered = values.ToArray()
        Array.sortInPlaceWith (fun (ai, a) (bi, b) ->
            let compared = compareRows a b
            if compared = 0 then compare ai bi else compared) ordered
        let path = storage.NewFile()
        use writer = new BinaryWriter(File.Create(path), Encoding.UTF8)
        for _, row in ordered do writeRow writer row
        path
    let merge (paths: string array) =
        let output = storage.NewFile()
        let readers = paths |> Array.map (fun path -> (rows path).GetEnumerator())
        try
            use writer = new BinaryWriter(File.Create(output), Encoding.UTF8)
            let active = readers |> Array.map (fun reader -> reader.MoveNext())
            let mutable running = true
            while running do
                let mutable best = -1
                for i in 0 .. readers.Length - 1 do
                    if active.[i] && (best < 0 || compareRows readers.[i].Current readers.[best].Current < 0) then best <- i
                if best < 0 then running <- false
                else
                    writeRow writer readers.[best].Current
                    active.[best] <- readers.[best].MoveNext()
        finally
            for reader in readers do reader.Dispose()
        for path in paths do File.Delete(path)
        output
    let buffer = ResizeArray<int64 * string array>()
    let runs = ResizeArray<string>()
    let mutable size = 0L
    let mutable ordinal = 0L
    for row in input do
        buffer.Add(ordinal, row)
        ordinal <- ordinal + 1L
        size <- size + 48L + int64 row.Length * 32L + (row |> Array.sumBy (fun value -> int64 value.Length * 2L))
        if size >= bufferBytes then
            runs.Add(spill buffer)
            buffer.Clear()
            size <- 0L
    if buffer.Count > 0 then runs.Add(spill buffer)
    let mutable paths = runs.ToArray()
    while paths.Length > 1 do paths <- paths |> Array.chunkBySize 16 |> Array.map merge
    if paths.Length = 0 then Seq.empty
    else seq { try yield! rows paths.[0] finally File.Delete(paths.[0]) }

let defaultBufferBytes = 64L * 1024L * 1024L
