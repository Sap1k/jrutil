// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// Disk spool for merged JDF trip stops, grouped by route.
module JrUtil.JdfTripStopSpool

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open NodaTime
open Serilog
open NetTopologySuite.Geometries
open JrUtil.JdfModel
open JrUtil.JdfStopReconciliation
open JrUtil.JdfParser
open JrUtil.JdfSerializer
open JrUtil.DateUtils
open JrUtil.ParallelUtils
open JrUtil.Utils
open JrUtil.GeoData.Common

type internal SpoolChunk = {
    path: string
    offset: int64
    length: int64
}

type internal SpoolRoute = {
    storedDistinction: int
    chunks: ResizeArray<SpoolChunk>
}

type internal SerializedBuffer = {
    stream: MemoryStream
    chunks: ((string * int) * int64 * int64) array
}

type internal TripStopSpool(path: string) =
    let directory = Path.GetDirectoryName(path)
    do if not (String.IsNullOrEmpty(directory)) then Directory.CreateDirectory(directory) |> ignore

    let stream =
        new FileStream(
            path,
            FileMode.Create,
            FileAccess.ReadWrite,
            FileShare.Read,
            1024 * 1024,
            FileOptions.SequentialScan)
    let writer = new StreamWriter(stream, jdfEncoding, 1024 * 1024, true)
    let writeRecord = getJdfRecordWriter<TripStop>
    let routes = Dictionary<string * int, SpoolRoute>()
    let segmentPaths = HashSet<string>(StringComparer.OrdinalIgnoreCase)
    let mutable currentKey: (string * int) option = None
    let mutable currentOffset = 0L
    let mutable finalized = false
    let mutable peakLength = 0L

    let storedLength () =
        stream.Length
        + (segmentPaths
           |> Seq.sumBy (fun segmentPath ->
               if File.Exists(segmentPath) then FileInfo(segmentPath).Length else 0L))

    let finishChunk () =
        match currentKey with
        | Some key ->
            writer.Flush()
            let length = stream.Position - currentOffset
            if length > 0L then
                routes.[key].chunks.Add({ path = path; offset = currentOffset; length = length })
            currentKey <- None
        | None -> ()

    let ensureRoute key distinction =
        match routes.TryGetValue(key) with
        | true, route -> route
        | false, _ ->
            let route = { storedDistinction = distinction; chunks = ResizeArray() }
            routes.Add(key, route)
            route

    let finalize () =
        if not finalized then
            finishChunk ()
            writer.Flush()
            finalized <- true

    let copyChunk (input: Stream) (output: Stream) (chunk: SpoolChunk) =
        input.Position <- chunk.offset
        let buffer = Array.zeroCreate<byte> (1024 * 1024)
        let mutable remaining = chunk.length
        while remaining > 0L do
            let requested = int (min remaining (int64 buffer.Length))
            let read = input.Read(buffer, 0, requested)
            if read = 0 then raise (EndOfStreamException("Unexpected end of trip-stop spool"))
            output.Write(buffer, 0, read)
            remaining <- remaining - int64 read

    let rewriteChunk
        (input: Stream) (output: Stream) storedDistinction outputDistinction (chunk: SpoolChunk) =
        input.Position <- chunk.offset
        let oldSuffix = $"\",\"{storedDistinction}\";\r\n"
        let newSuffix = $"\",\"{outputDistinction}\";\r\n"
        let buffer = Array.zeroCreate<byte> (1024 * 1024)
        let mutable remaining = chunk.length
        let mutable carry = ""
        let mutable replaced = false
        let writeText (text: string) =
            let bytes = jdfEncoding.GetBytes(text)
            output.Write(bytes, 0, bytes.Length)
        while remaining > 0L do
            let requested = int (min remaining (int64 buffer.Length))
            let read = input.Read(buffer, 0, requested)
            if read = 0 then raise (EndOfStreamException("Unexpected end of trip-stop spool"))
            remaining <- remaining - int64 read
            let text = carry + jdfEncoding.GetString(buffer, 0, read)
            let safeLimit =
                if remaining = 0L then text.Length
                else max 0 (text.Length - oldSuffix.Length + 1)
            let mutable position = 0
            let mutable searching = true
            while searching do
                let found = text.IndexOf(oldSuffix, position, StringComparison.Ordinal)
                if found >= 0 && (remaining = 0L || found < safeLimit) then
                    writeText (text.Substring(position, found - position))
                    writeText newSuffix
                    replaced <- true
                    position <- found + oldSuffix.Length
                else
                    searching <- false
            let writeUntil = max position safeLimit
            writeText (text.Substring(position, writeUntil - position))
            carry <- text.Substring(writeUntil)
        if not replaced then
            failwithf "Trip-stop spool did not contain distinction %d" storedDistinction

    member _.Add(key: string * int, row: TripStop) =
        if finalized then invalidOp "Cannot append to a finalized trip-stop spool"
        ensureRoute key (snd key) |> ignore
        if currentKey <> Some key then
            finishChunk ()
            currentKey <- Some key
            currentOffset <- stream.Position
        writeRecord writer row

    member _.BeginMappedBatch(
        rows: IReadOnlyList<TripStop>,
        workers: int,
        mapRow: TripStop -> TripStop) =
        if finalized then invalidOp "Cannot append to a finalized trip-stop spool"
        Threading.Tasks.Task.Run<unit -> unit>(Func<unit -> unit>(fun () ->
            let usefulPartitions = max 1 ((rows.Count + 2047) / 2048)
            let partitionCount =
                min workers (min Environment.ProcessorCount usefulPartitions)
            let partitionSize = (rows.Count + partitionCount - 1) / partitionCount
            let partitions =
                if rows.Count = 0 then [||]
                else [| for start in 0 .. partitionSize .. rows.Count - 1 ->
                          start, min rows.Count (start + partitionSize) |]
            let serializeBuffer (startIndex, endIndex) =
                let bufferStream = new MemoryStream()
                use partWriter = new StreamWriter(bufferStream, jdfEncoding, 64 * 1024, true)
                let chunks = ResizeArray<_>()
                let mutable chunkKey: (string * int) option = None
                let mutable chunkOffset = 0L
                let finishBufferChunk () =
                    match chunkKey with
                    | Some key ->
                        partWriter.Flush()
                        let length = bufferStream.Position - chunkOffset
                        if length > 0L then chunks.Add(key, chunkOffset, length)
                        chunkKey <- None
                    | None -> ()
                for index in startIndex .. endIndex - 1 do
                    let mapped = mapRow rows.[index]
                    let key = mapped.routeId, mapped.routeDistinction
                    if chunkKey <> Some key then
                        finishBufferChunk ()
                        chunkKey <- Some key
                        chunkOffset <- bufferStream.Position
                    writeRecord partWriter mapped
                finishBufferChunk ()
                partWriter.Flush()
                {
                    stream = bufferStream
                    chunks = chunks.ToArray()
                }
            let buffers =
                partitions
                |> mapParallelOrderedBatches partitionCount serializeBuffer
                |> Seq.toArray
            fun () ->
                if finalized then invalidOp "Cannot register into a finalized trip-stop spool"
                finishChunk ()
                writer.Flush()
                try
                    for buffer in buffers do
                        let baseOffset = stream.Position
                        buffer.stream.Position <- 0L
                        buffer.stream.CopyTo(stream)
                        for key, offset, length in buffer.chunks do
                            let route = ensureRoute key (snd key)
                            route.chunks.Add({
                                path = path
                                offset = baseOffset + offset
                                length = length
                            })
                    peakLength <- max peakLength (storedLength ())
                finally
                    for buffer in buffers do buffer.stream.Dispose()))

    member this.AddMappedBatch(
        rows: TripStop array,
        workers: int,
        mapRow: TripStop -> TripStop) =
        let register =
            this.BeginMappedBatch(rows, workers, mapRow).GetAwaiter().GetResult()
        register ()

    member _.Delete(key) =
        finishChunk ()
        routes.Remove(key) |> ignore

    member _.Copy(sourceKey, destinationKey) =
        finishChunk ()
        let source = ensureRoute sourceKey (snd sourceKey)
        routes.[destinationKey] <- source

    member _.WriteTo(output: Stream) =
        finalize ()
        let mutable segmentInput: FileStream = null
        let mutable segmentInputPath = ""
        let inputFor chunkPath =
            if String.Equals(chunkPath, path, StringComparison.OrdinalIgnoreCase) then
                stream :> Stream
            else
                if segmentInputPath <> chunkPath then
                    if not (isNull segmentInput) then segmentInput.Dispose()
                    segmentInput <- File.OpenRead(chunkPath)
                    segmentInputPath <- chunkPath
                segmentInput :> Stream
        try
            for KeyValue(key, route) in routes do
                for chunk in route.chunks do
                    let input = inputFor chunk.path
                    if route.storedDistinction = snd key then copyChunk input output chunk
                    else rewriteChunk input output route.storedDistinction (snd key) chunk
        finally
            if not (isNull segmentInput) then segmentInput.Dispose()

    member this.ToArray() =
        use memory = new MemoryStream()
        this.WriteTo(memory)
        memory.Position <- 0L
        let parser: Stream -> TripStop seq = getJdfParser
        parser memory |> Seq.toArray

    member _.Length =
        writer.Flush()
        storedLength ()

    member _.PeakLength =
        writer.Flush()
        max peakLength (storedLength ())

    interface IDisposable with
        member _.Dispose() =
            try
                try writer.Dispose()
                finally stream.Dispose()
            finally
                if File.Exists(path) then File.Delete(path)
                for segmentPath in segmentPaths do
                    if File.Exists(segmentPath) then File.Delete(segmentPath)
