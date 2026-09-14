// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
module JrUtil.JdfCallStore

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.IO.MemoryMappedFiles
open System.Runtime.InteropServices
open System.Threading
open NodaTime
open JrUtil.JdfModel

// Private native scratch representation; never a public package relation.
[<Struct; StructLayout(LayoutKind.Sequential, Pack = 1)>]
type private StoredCall = {
    route: int
    trip: int64
    routeStop: int64
    stop: int64
    post: int64
    postNumber: int
    attribute0: int
    attribute1: int
    attribute2: int
    kilometer: decimal
    arrival: int64
    departure: int64
    minArrival: int64
    maxDeparture: int64
    distinction: int
    flags: int
}

type private IFilterableCalls =
    abstract Filter: (TripStop -> bool) -> IReadOnlyList<TripStop>

type private ITripSpans =
    abstract TripSpans: unit -> seq<struct(string * int * int64 * int * int)>

type private TripIndexWriter(path: string) =
    let output = new BinaryWriter(File.Create(path))
    let mutable current = None
    let mutable start = 0
    let mutable count = 0
    let flush () =
        match current with
        | Some struct(route: string, distinction: int, trip: int64) ->
            output.Write(route); output.Write(distinction); output.Write(trip)
            output.Write(start); output.Write(count - start)
        | None -> ()
    member _.Append(row: TripStop) =
        let key = struct(row.routeId, row.routeDistinction, row.tripId)
        if current <> Some key then
            flush ()
            current <- Some key
            start <- count
        count <- count + 1
    interface IDisposable with
        member _.Dispose() = flush (); output.Dispose()

let private readTripSpans path = seq {
    use input = new BinaryReader(File.OpenRead(path))
    while input.BaseStream.Position < input.BaseStream.Length do
        let route = input.ReadString()
        let distinction = input.ReadInt32()
        let trip = input.ReadInt64()
        let start = input.ReadInt32()
        yield struct(route, distinction, trip, start, input.ReadInt32())
}

let tripSpans (calls: IReadOnlyList<TripStop>) =
    match calls with
    | :? ITripSpans as indexed -> indexed.TripSpans()
    | _ -> seq {
        let mutable start = 0
        while start < calls.Count do
            let first = calls.[start]
            let mutable finish = start + 1
            let mutable same = true
            while finish < calls.Count && same do
                let next = calls.[finish]
                if next.routeId = first.routeId && next.routeDistinction = first.routeDistinction && next.tripId = first.tripId then
                    finish <- finish + 1
                else same <- false
            yield struct(first.routeId, first.routeDistinction, first.tripId, start, finish - start)
            start <- finish
      }

/// Owns native calls and all filtered indexes. No call objects survive parsing.
/// Readers are independent and may safely use the immutable storage concurrently.
type Store(parent: string, token: CancellationToken) =
    let root = Path.Combine(parent, ".jdf-calls-" + Guid.NewGuid().ToString("N"))
    let resources = ResizeArray<IDisposable>()
    let strings = ResizeArray<string>()
    let stringIds = Dictionary<string, int>(StringComparer.Ordinal)
    let mutable stringBytes = 0L
    let mutable count = 0
    let mutable view: MemoryMappedViewAccessor option = None
    let mutable disposed = false
    let mutable written = false
    let recordBytes = Marshal.SizeOf<StoredCall>()
    do Directory.CreateDirectory(root) |> ignore

    let check () =
        if disposed then raise (ObjectDisposedException("JdfCallStore"))
        token.ThrowIfCancellationRequested()

    let intern (value: string) =
        match stringIds.TryGetValue(value) with
        | true, index -> index
        | _ ->
            let bytes = 80L + 2L * int64 value.Length
            if stringBytes + bytes > 32L * 1024L * 1024L then
                invalidOp "JDF call route/post identities exceed the 32 MiB index budget"
            let index = strings.Count
            strings.Add(value)
            stringIds.Add(value, index)
            stringBytes <- stringBytes + bytes
            index

    let encodeTime = function
        | None -> -1L
        | Some Passing -> -2L
        | Some NotPassing -> -3L
        | Some (StopTime value) -> value.NanosecondOfDay

    let decodeTime = function
        | -1L -> None
        | -2L -> Some Passing
        | -3L -> Some NotPassing
        | value -> Some (StopTime (LocalTime.FromNanosecondsSinceMidnight(value)))

    let mapFile path =
        let mapped = MemoryMappedFile.CreateFromFile(path, FileMode.Open, null, 0L, MemoryMappedFileAccess.Read)
        resources.Add(mapped)
        let accessor = mapped.CreateViewAccessor(0L, 0L, MemoryMappedFileAccess.Read)
        resources.Add(accessor)
        accessor

    let read index =
        check ()
        if index < 0 || index >= count then raise (ArgumentOutOfRangeException("index"))
        let mutable packed = Unchecked.defaultof<StoredCall>
        view.Value.Read(int64 index * int64 recordBytes, &packed)
        let optional flag value = if packed.flags &&& flag <> 0 then Some value else None
        { routeId = strings.[packed.route]; tripId = packed.trip
          routeStopId = packed.routeStop; stopId = packed.stop
          stopPostId = optional 1 packed.post
          stopPostNum = if packed.postNumber < 0 then None else Some strings.[packed.postNumber]
          attributes = [| optional 2 packed.attribute0; optional 4 packed.attribute1; optional 8 packed.attribute2 |]
          kilometer = optional 16 packed.kilometer
          arrivalTime = decodeTime packed.arrival; departureTime = decodeTime packed.departure
          minArrivalTime = decodeTime packed.minArrival; maxDepartureTime = decodeTime packed.maxDeparture
          routeDistinction = packed.distinction }

    let rec filterCalls (source: IReadOnlyList<TripStop>) predicate =
        check ()
        let path = Path.Combine(root, Guid.NewGuid().ToString("N") + ".index")
        let mutable retained = 0
        do
            use writer = new BinaryWriter(File.Create(path))
            use trips = new TripIndexWriter(path + ".trips")
            for index = 0 to source.Count - 1 do
                let row = source.[index]
                if predicate row then
                    writer.Write(index)
                    trips.Append(row)
                    retained <- retained + 1
            if retained = 0 then writer.Write(0)
        let indexView = mapFile path
        let get index =
            check ()
            if index < 0 || index >= retained then raise (ArgumentOutOfRangeException("index"))
            source.[indexView.ReadInt32(int64 index * 4L)]
        { new IReadOnlyList<TripStop> with
            member _.Count = retained
            member _.Item with get index = get index
          interface IEnumerable<TripStop> with
            member _.GetEnumerator() = (seq { for index = 0 to retained - 1 do yield get index }).GetEnumerator()
          interface IEnumerable with
            member _.GetEnumerator() = (seq { for index = 0 to retained - 1 do yield get index }).GetEnumerator() :> IEnumerator
          interface IFilterableCalls with
            member this.Filter(predicate) = filterCalls (box this :?> IReadOnlyList<TripStop>) predicate
          interface ITripSpans with
            member _.TripSpans() = check (); readTripSpans (path + ".trips") }

    member _.Count = count
    member _.ScratchBytes = Directory.EnumerateFiles(root) |> Seq.sumBy (fun path -> FileInfo(path).Length)

    member this.Write(rows: seq<TripStop>, progress: int64 -> unit) =
        check ()
        if written then invalidOp "JDF call store is immutable after writing"
        written <- true
        let path = Path.Combine(root, "calls.bin")
        do
            use output = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.None, 1024 * 1024)
            use trips = new TripIndexWriter(Path.Combine(root, "trips.bin"))
            let buffer = Array.zeroCreate<StoredCall> 4096
            let mutable buffered = 0
            let flush () =
                if buffered > 0 then
                    output.Write(MemoryMarshal.AsBytes<StoredCall>(ReadOnlySpan<StoredCall>(buffer, 0, buffered)))
                    buffered <- 0
            for row in rows do
                check ()
                trips.Append(row)
                if row.attributes.Length <> 3 then invalidArg "rows" "A JDF call must contain three attribute slots"
                if count = Int32.MaxValue then invalidOp "JDF call count exceeds the indexed storage limit"
                let flag bit value = if Option.isSome value then bit else 0
                buffer.[buffered] <- {
                    route = intern row.routeId; trip = row.tripId; routeStop = row.routeStopId; stop = row.stopId
                    post = Option.defaultValue 0L row.stopPostId
                    postNumber = row.stopPostNum |> Option.map intern |> Option.defaultValue -1
                    attribute0 = Option.defaultValue 0 row.attributes.[0]
                    attribute1 = Option.defaultValue 0 row.attributes.[1]
                    attribute2 = Option.defaultValue 0 row.attributes.[2]
                    kilometer = Option.defaultValue 0M row.kilometer
                    arrival = encodeTime row.arrivalTime; departure = encodeTime row.departureTime
                    minArrival = encodeTime row.minArrivalTime; maxDeparture = encodeTime row.maxDepartureTime
                    distinction = row.routeDistinction
                    flags = flag 1 row.stopPostId ||| flag 2 row.attributes.[0] ||| flag 4 row.attributes.[1]
                            ||| flag 8 row.attributes.[2] ||| flag 16 row.kilometer }
                buffered <- buffered + 1
                count <- count + 1
                if buffered = buffer.Length then flush ()
                if count % 250000 = 0 then progress (int64 count)
            flush ()
            if count = 0 then output.WriteByte(0uy)
        view <- Some (mapFile path)
        stringIds.Clear()
        progress (int64 count)
        this :> IReadOnlyList<TripStop>

    interface IReadOnlyList<TripStop> with
        member _.Count = count
        member _.Item with get index = read index
    interface IEnumerable<TripStop> with
        member _.GetEnumerator() = (seq { for index = 0 to count - 1 do yield read index }).GetEnumerator()
    interface IEnumerable with
        member _.GetEnumerator() = (seq { for index = 0 to count - 1 do yield read index }).GetEnumerator() :> IEnumerator
    interface IFilterableCalls with
        member this.Filter(predicate) = filterCalls (this :> IReadOnlyList<TripStop>) predicate
    interface ITripSpans with
        member _.TripSpans() = check (); readTripSpans (Path.Combine(root, "trips.bin"))
    interface IDisposable with
        member _.Dispose() =
            if not disposed then
                disposed <- true
                try
                    for index = resources.Count - 1 downto 0 do resources.[index].Dispose()
                finally
                    if Directory.Exists(root) then Directory.Delete(root, true)

let filter predicate (calls: IReadOnlyList<TripStop>) =
    match calls with
    | :? IFilterableCalls as stored -> stored.Filter(predicate)
    | _ -> calls |> Seq.filter predicate |> Seq.toArray :> IReadOnlyList<TripStop>
