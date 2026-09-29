// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open JrUtil
open System.IO
open System.Diagnostics
open System.Runtime
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

    let private encode (output: BinaryWriter) row =
        output.Write(row.binding); output.Write(row.callNamespace); output.Write(row.sequence)
        output.Write(row.sourceSequence); output.Write(row.stop)
        writeTime output row.arrival; writeTime output row.departure

    let private decode (input: BinaryReader) = {
        binding = input.ReadString(); callNamespace = input.ReadString(); sequence = input.ReadInt32()
        sourceSequence = input.ReadString(); stop = input.ReadString()
        arrival = readTime input; departure = readTime input }

    let private bytes row =
        184L + 2L * int64 (row.binding.Length + row.callNamespace.Length + row.sourceSequence.Length + row.stop.Length)

    let private key row =
        row.binding + "\u001f" + row.callNamespace + "\u001f" + row.sourceSequence + "\u001f" + row.sequence.ToString(CultureInfo.InvariantCulture)

    let private columns (rows: MappedRow array) =
        let column project = rows |> Array.map project
        [| ColumnWriter.Text(column _.binding)
           ColumnWriter.Text(column _.callNamespace)
           ColumnWriter.Text(column _.sourceSequence); ColumnWriter.Int32(column _.sequence)
           ColumnWriter.Text(column (fun row -> if String.IsNullOrWhiteSpace(row.stop) then null else row.stop))
           ColumnWriter.OptionalInt32(column _.arrival); ColumnWriter.OptionalInt32(column _.departure) |]

    let private schema () = Schema.relations |> Array.find (fun relation -> relation.name = "source_call_map")

    /// Native calls are unique by construction (one binding per trip, one row
    /// per trip call), so they stream from the spool in trip order.
    let write (path: string) spool (bindings: IDictionary<string, string>) token (progress: string -> int64 -> unit) =
        let rows = seq {
            use index = new BinaryReader(File.OpenRead(spool + ".index"), Encoding.UTF8)
            use input = new BinaryReader(File.OpenRead(spool), Encoding.UTF8)
            while index.BaseStream.Position < index.BaseStream.Length do
                let binding = bindings.[index.ReadString()]
                let first = index.ReadInt64()
                let last = index.ReadInt64()
                input.BaseStream.Seek(first, SeekOrigin.Begin) |> ignore
                while input.BaseStream.Position < last do
                    let sequence = input.ReadInt32()
                    yield { binding = binding; callNamespace = "gtfs_stop_sequence"; sequence = sequence
                            sourceSequence = sequence.ToString(CultureInfo.InvariantCulture); stop = input.ReadString()
                            arrival = readTime input; departure = readTime input }
        }
        RelationWriter.writeRows path (schema ()) (4L * 1024L * 1024L) 16384 token progress bytes columns rows

    /// Convert transitional mapped rows to typed rows.
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
    /// seven boxed values per call in the production overlay path. Mapped calls
    /// can repeat, so they are deduplicated by primary key.
    let writeMappedTyped (path: string) (typed: seq<MappedRow>) (token: CancellationToken) (progress: string -> int64 -> unit) =
        let reclaimGate =
            MemoryReclaimGate(3_000_000_000L, MemoryReclaim.DefaultMinimumGrowthBytes, MemoryReclaim.compactOnce)
        // Binding preparation builds several large lookup tables whose
        // construction garbage is dead before call projection begins.
        reclaimGate.Check() |> ignore
        RelationWriter.write path (schema ()) (64L * 1024L * 1024L) (16L * 1024L * 1024L) 65536
            token progress bytes key encode decode columns typed

    let writeMapped (path: string) (rows: seq<Model.Row>) (token: CancellationToken) (progress: string -> int64 -> unit) =
        writeMappedTyped path (fromModelRows rows) token progress
