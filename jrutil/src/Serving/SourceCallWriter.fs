// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO
open System.Text
open System.Globalization
open System.Threading
open System.Collections.Generic
open JrUtil.GtfsModel

module SourceCallWriter =
    [<Struct>]
    type MappedRow = {
        binding: string; callNamespace: string; sequence: int; sourceSequence: string; stop: string
        arrival: Nullable<int>; departure: Nullable<int>
    }

    let private writeTime (output: BinaryWriter) (value: Nullable<int>) =
        output.Write(value.HasValue)
        if value.HasValue then output.Write(value.Value)

    let private readTime (input: BinaryReader) =
        if input.ReadBoolean() then Nullable(input.ReadInt32()) else Nullable()

    /// Native facts captured with the GTFS sink; never reconstructed from text output.
    type Spool(path: string) =
        let output = new BinaryWriter(File.Create(path), Encoding.UTF8)
        let index = new BinaryWriter(File.Create(path + ".index"), Encoding.UTF8)
        let mutable trip: string = null
        let mutable start = 0L
        let mutable completed = false
        let finishTrip () =
            if not (isNull trip) then
                index.Write(trip); index.Write(start); index.Write(output.BaseStream.Position)
        let time = Option.map (fun (value: NodaTime.Period) -> Nullable(int (value.ToDuration().TotalSeconds))) >> Option.defaultValue (Nullable())
        member _.Append(call: StopTime) =
            if completed then invalidOp "Source call spool is complete"
            if call.tripId <> trip then
                if not (isNull trip) && StringComparer.Ordinal.Compare(trip, call.tripId) >= 0 then
                    invalidOp "Native source call trips must arrive in ordinal order"
                finishTrip ()
                trip <- call.tripId
                start <- output.BaseStream.Position
            output.Write(call.stopSequence); output.Write(call.stopId)
            writeTime output (time call.arrivalTime); writeTime output (time call.departureTime)
        member _.Complete() =
            if not completed then
                finishTrip ()
                completed <- true
                output.Dispose()
                index.Dispose()
        interface IDisposable with
            member this.Dispose() = this.Complete()

    let write (path: string) spool (bindings: IDictionary<string, string>) token (progress: string -> int64 -> unit) =
        let encode (output: BinaryWriter) row =
            output.Write(row.binding); output.Write(row.callNamespace); output.Write(row.sequence)
            output.Write(row.sourceSequence); output.Write(row.stop)
            writeTime output row.arrival; writeTime output row.departure
        let decode (input: BinaryReader) =
            let binding = input.ReadString()
            let callNamespace = input.ReadString()
            let sequence = input.ReadInt32()
            { binding = binding; callNamespace = callNamespace; sequence = sequence; sourceSequence = input.ReadString()
              stop = input.ReadString(); arrival = readTime input; departure = readTime input }
        let compareRows left right =
            let order = StringComparer.Ordinal.Compare(left.binding, right.binding)
            if order <> 0 then order else
            let namespaceOrder = StringComparer.Ordinal.Compare(left.callNamespace, right.callNamespace)
            if namespaceOrder <> 0 then namespaceOrder else
            let sourceOrder = StringComparer.Ordinal.Compare(left.sourceSequence, right.sourceSequence)
            if sourceOrder <> 0 then sourceOrder else compare left.sequence right.sequence
        let bytes row =
            184L + 2L * int64 (row.binding.Length + row.callNamespace.Length + row.sourceSequence.Length + row.stop.Length)
        let root = Path.GetDirectoryName(path)
        let blocks = seq {
            use input = new BinaryReader(File.OpenRead(spool + ".index"), Encoding.UTF8)
            while input.BaseStream.Position < input.BaseStream.Length do
                let binding = bindings.[input.ReadString()]
                let first = input.ReadInt64()
                let last = input.ReadInt64()
                yield struct(binding, first, last)
        }
        let writeBlock (output: BinaryWriter) (struct(binding: string, first: int64, last: int64)) =
            output.Write(binding); output.Write(first); output.Write(last)
        let readBlock (input: BinaryReader) =
            let binding = input.ReadString()
            let first = input.ReadInt64()
            struct(binding, first, input.ReadInt64())
        let orderedBlocks =
            blocks |> BinarySort.sort root (16L * 1024L * 1024L) 65536 token
                (fun phase count -> progress ("trip-blocks-" + phase) count)
                (fun (struct(binding, _, _)) -> 64L + 2L * int64 binding.Length)
                (fun (struct(left, _, _)) (struct(right, _, _)) -> StringComparer.Ordinal.Compare(left, right))
                writeBlock readBlock
        let rows = seq {
            use input = new BinaryReader(File.OpenRead(spool), Encoding.UTF8)
            for struct(binding, first, last) in orderedBlocks do
                input.BaseStream.Seek(first, SeekOrigin.Begin) |> ignore
                let calls = seq {
                    while input.BaseStream.Position < last do
                        let sequence = input.ReadInt32()
                        yield { binding = binding; callNamespace = "gtfs_stop_sequence"; sequence = sequence
                                sourceSequence = sequence.ToString(CultureInfo.InvariantCulture); stop = input.ReadString()
                                arrival = readTime input; departure = readTime input }
                }
                yield! calls |> BinarySort.sort root (4L * 1024L * 1024L) 16384 token
                    (fun phase count -> progress ("trip-calls-" + phase) count) bytes compareRows encode decode
        }
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "source_call_map")
        let columns (rows: MappedRow array) =
            let column project = rows |> Array.map project
            [| ColumnWriter.Text(column _.binding)
               ColumnWriter.Text(column _.callNamespace)
               ColumnWriter.Text(column _.sourceSequence); ColumnWriter.Int32(column _.sequence)
               ColumnWriter.Text(column (fun row -> if String.IsNullOrWhiteSpace(row.stop) then null else row.stop))
               ColumnWriter.OptionalInt32(column _.arrival); ColumnWriter.OptionalInt32(column _.departure) |]
        RelationWriter.writeOrdered path schema (4L * 1024L * 1024L) 16384
            token progress bytes compareRows (=) columns rows

    /// Sort transitional mapped rows without converting the whole corpus to
    /// string arrays and dictionaries at spill boundaries.
    let fromModelRows (rows: seq<Model.Row>) =
        let text (value: obj) = if isNull value then "" else unbox<string> value
        let optionalInt (value: obj) = if isNull value then Nullable() else Nullable(unbox<int> value)
        rows |> Seq.map (fun row -> {
            binding = text row.["binding_id"]
            callNamespace = text row.["call_namespace"]
            sourceSequence = text row.["source_sequence"]
            sequence = unbox<int> row.["call_sequence"]
            stop = text row.["source_stop_id"]
            arrival = optionalInt row.["scheduled_arrival"]
            departure = optionalInt row.["scheduled_departure"] })

    /// Write already-typed mapped calls without allocating one dictionary and
    /// seven boxed values per call in the production overlay path.
    let writeMappedTyped (path: string) (typed: seq<MappedRow>) (token: CancellationToken) (progress: string -> int64 -> unit) =
        let encode (output: BinaryWriter) row =
            output.Write(row.binding); output.Write(row.callNamespace); output.Write(row.sequence)
            output.Write(row.sourceSequence); output.Write(row.stop)
            writeTime output row.arrival; writeTime output row.departure
        let decode (input: BinaryReader) = {
            binding = input.ReadString(); callNamespace = input.ReadString(); sequence = input.ReadInt32()
            sourceSequence = input.ReadString(); stop = input.ReadString()
            arrival = readTime input; departure = readTime input }
        let compareRows left right =
            let binding = StringComparer.Ordinal.Compare(left.binding, right.binding)
            if binding <> 0 then binding else
            let namespaceOrder = StringComparer.Ordinal.Compare(left.callNamespace, right.callNamespace)
            if namespaceOrder <> 0 then namespaceOrder else
            let source = StringComparer.Ordinal.Compare(left.sourceSequence, right.sourceSequence)
            if source <> 0 then source else compare left.sequence right.sequence
        let bytes row =
            184L + 2L * int64 (row.binding.Length + row.callNamespace.Length + row.sourceSequence.Length + row.stop.Length)
        let columns (values: MappedRow array) =
            let column project = values |> Array.map project
            [| ColumnWriter.Text(column _.binding); ColumnWriter.Text(column _.callNamespace)
               ColumnWriter.Text(column _.sourceSequence); ColumnWriter.Int32(column _.sequence)
               ColumnWriter.Text(column (fun row -> if String.IsNullOrWhiteSpace(row.stop) then null else row.stop))
               ColumnWriter.OptionalInt32(column _.arrival); ColumnWriter.OptionalInt32(column _.departure) |]
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "source_call_map")
        // Binding IDs are v1:trip:<hex>.  Two digest nibbles make each range
        // small enough to sort in memory on production corpora. Concatenating
        // 00..ff still preserves the full declared lexical ordering and avoids
        // writing and rereading a second generation of external-sort runs.
        let root = Path.Combine(Path.GetDirectoryName(path), ".source-call-partitions-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let paths = Array.init 256 (fun index -> Path.Combine(root, index.ToString("x2", CultureInfo.InvariantCulture) + ".bin"))
            let writers = paths |> Array.map (fun file -> new BinaryWriter(File.Create(file), Encoding.UTF8))
            let nibble value =
                if value >= '0' && value <= '9' then int value - int '0'
                elif value >= 'a' && value <= 'f' then int value - int 'a' + 10
                elif value >= 'A' && value <= 'F' then int value - int 'A' + 10
                else -1
            try
                let mutable count = 0L
                for row in typed do
                    token.ThrowIfCancellationRequested()
                    if not (row.binding.StartsWith("v1:trip:", StringComparison.Ordinal)) || row.binding.Length <= 9 then
                        invalidOp $"Unexpected source-call binding ID: {row.binding}"
                    let high, low = nibble row.binding.[8], nibble row.binding.[9]
                    if high < 0 || low < 0 then invalidOp $"Unexpected source-call binding ID: {row.binding}"
                    let partition = high * 16 + low
                    encode writers.[partition] row
                    count <- count + 1L
                    if count % 100000L = 0L then progress "partition-input" count
            finally
                for writer in writers do writer.Dispose()
            let ordered = seq {
                for partition in 0 .. 255 do
                    let partitionRows = seq {
                        use input = new BinaryReader(File.OpenRead(paths.[partition]), Encoding.UTF8)
                        while input.BaseStream.Position < input.BaseStream.Length do yield decode input
                    }
                    yield! BinarySort.sort root (64L * 1024L * 1024L) 262144 token
                        (fun operation count -> progress ($"partition-{partition:x2}-{operation}") count)
                        bytes compareRows encode decode partitionRows
            }
            RelationWriter.writeOrdered path schema (16L * 1024L * 1024L) 65536
                token progress bytes compareRows (=) columns ordered
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    let writeMapped (path: string) (rows: seq<Model.Row>) (token: CancellationToken) (progress: string -> int64 -> unit) =
        writeMappedTyped path (fromModelRows rows) token progress
