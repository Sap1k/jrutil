// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open System.IO
open System.Text
open System.Security.Cryptography
open System.Globalization
open System.Collections.Generic
open JrUtil.GtfsModel

/// Native call facts shared by compiler producers and their serving sink.
module TripCallWriter =
    type Summary = {
        firstSequence: int; lastSequence: int
        firstStopId: string; lastStopId: string
        scheduledStart: Nullable<int>; scheduledEnd: Nullable<int>
        callPatternSha256: string
    }

    [<Struct>]
    type Row = {
        tripId: string
        sequence: int
        locationId: string
        passengerService: bool
        boardingPointId: string
        routeStopId: string
        arrival: Nullable<int>
        departure: Nullable<int>
        passage: Nullable<int>
        pickup: int16
        dropoff: int16
        timepoint: bool
        headsign: string
        distance: Nullable<double>
    }

    let fromGtfs (call: StopTime) location boarding routeStop =
        let seconds = Option.map (fun (value: NodaTime.Period) -> int (value.ToDuration().TotalSeconds)) >> Option.map Nullable >> Option.defaultValue (Nullable())
        let service = function
            | None | Some RegularlyScheduled -> 0s
            | Some NoService -> 1s | Some PhoneBefore -> 2s | Some CoordinationWithDriver -> 3s
        { tripId = call.tripId; sequence = call.stopSequence; locationId = location
          passengerService = true; boardingPointId = boarding; routeStopId = routeStop
          arrival = seconds call.arrivalTime; departure = seconds call.departureTime; passage = Nullable()
          pickup = service call.pickupType; dropoff = service call.dropoffType
          timepoint = call.timepoint <> Some Approximate
          headsign = call.headsign |> Option.filter (String.IsNullOrWhiteSpace >> not) |> Option.defaultValue null
          distance = call.shapeDistTraveled |> Option.map (double >> Nullable) |> Option.defaultValue (Nullable()) }

    /// Append-only typed compiler metadata, outside the production inventory.
    type SummaryWriter(path: string) =
        let output = new BinaryWriter(File.Create(path), Encoding.UTF8)
        let hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256)
        let mutable trip = ""
        let mutable first = Unchecked.defaultof<StopTime>
        let mutable last = Unchecked.defaultof<StopTime>
        let mutable closed = false
        let periodText (value: NodaTime.Period) =
            let period = value.Normalize()
            let format (value: int64) = if value < 0L then sprintf "%02d" value else value.ToString("00", CultureInfo.InvariantCulture)
            String.Concat(format (period.Hours + int64 period.Days * 24L), ":", format period.Minutes, ":", format period.Seconds)
        let time value = value |> Option.map periodText |> Option.defaultValue ""
        let service = function
            | None -> "" | Some RegularlyScheduled -> "0" | Some NoService -> "1"
            | Some PhoneBefore -> "2" | Some CoordinationWithDriver -> "3"
        let writeTime (value: NodaTime.Period option) =
            output.Write(value.IsSome)
            value |> Option.iter (fun value -> output.Write(int (value.ToDuration().TotalSeconds)))
        let finish () =
            if trip <> "" then
                output.Write(trip)
                output.Write(first.stopSequence); output.Write(last.stopSequence)
                output.Write(first.stopId); output.Write(last.stopId)
                writeTime (first.departureTime |> Option.orElse first.arrivalTime)
                writeTime (last.arrivalTime |> Option.orElse last.departureTime)
                output.Write("v1:" + (hash.GetHashAndReset() |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()))
        member _.Append(call: StopTime) =
            if closed then raise (ObjectDisposedException("CallSummaryWriter"))
            if trip <> call.tripId then
                finish ()
                trip <- call.tripId
                first <- call
            else hash.AppendData([| 10uy |])
            [ call.stopSequence.ToString(CultureInfo.InvariantCulture); call.stopId
              time call.arrivalTime; time call.departureTime; service call.pickupType; service call.dropoffType ]
            |> Identity.compositeKey |> Encoding.UTF8.GetBytes |> hash.AppendData
            last <- call
        member _.Complete() =
            if not closed then
                finish ()
                output.Dispose()
                hash.Dispose()
                closed <- true
        interface IDisposable with
            member _.Dispose() =
                if not closed then
                    closed <- true
                    output.Dispose()
                    hash.Dispose()

    let readSummaries path =
        let result = Dictionary<string, Summary>(StringComparer.Ordinal)
        use input = new BinaryReader(File.OpenRead(path), Encoding.UTF8)
        let time () = if input.ReadBoolean() then Nullable(input.ReadInt32()) else Nullable()
        while input.BaseStream.Position < input.BaseStream.Length do
            let trip = input.ReadString()
            let value = {
                firstSequence = input.ReadInt32(); lastSequence = input.ReadInt32()
                firstStopId = input.ReadString(); lastStopId = input.ReadString()
                scheduledStart = time (); scheduledEnd = time (); callPatternSha256 = input.ReadString() }
            result.Add(trip, value)
        result

    type Writer(path: string, token: CancellationToken, ?bufferedOutput: bool) =
        let schema = Schema.relations |> Array.find (fun value -> value.name = "trip_call")
        // Large relations pay a substantial fixed Parquet row-group/column cost.
        // Keep the producer bounded, but amortize that cost across enough calls.
        let capacity = 65536
        let batchByteLimit = 32L * 1024L * 1024L
        let buffer = Array.zeroCreate<Row> capacity
        let mutable count = 0
        let mutable bytes = 0L
        let mutable previousTrip: string = null
        let mutable previousSequence = 0
        let mutable closed = false
        let writer = new ColumnWriter.Writer(path, schema, capacity, token, maximumBytes = 64L * 1024L * 1024L)
        let mutable compressionActive = 0
        let writeBatch (batch: Row array) =
                let column project = batch |> Array.map project
                writer.Append [|
                    ColumnWriter.Text(column _.tripId); ColumnWriter.Int32(column _.sequence)
                    ColumnWriter.Text(column _.locationId); ColumnWriter.Boolean(column _.passengerService)
                    ColumnWriter.Text(column _.boardingPointId); ColumnWriter.Text(column _.routeStopId)
                    ColumnWriter.OptionalInt32(column _.arrival); ColumnWriter.OptionalInt32(column _.departure)
                    ColumnWriter.OptionalInt32(column _.passage); ColumnWriter.Int16(column _.pickup)
                    ColumnWriter.Int16(column _.dropoff); ColumnWriter.Boolean(column _.timepoint)
                    ColumnWriter.Text(column _.headsign); ColumnWriter.OptionalFloat64(column _.distance) |]
        // One compression consumer and at most two queued batches (each <= 8 MiB).
        // The producer waits when full; worker failure completes the channel so
        // a blocked producer cannot hang indefinitely.
        let queue =
            if defaultArg bufferedOutput false then
                Some (Channel.CreateBounded<Row array>(BoundedChannelOptions(2, SingleReader = true, SingleWriter = true, FullMode = BoundedChannelFullMode.Wait)))
            else None
        let worker = queue |> Option.map (fun channel -> Task.Run(Action(fun () ->
            try
                while channel.Reader.WaitToReadAsync(token).AsTask().GetAwaiter().GetResult() do
                    let mutable batch = Unchecked.defaultof<Row array>
                    while channel.Reader.TryRead(&batch) do
                        Interlocked.Exchange(&compressionActive, 1) |> ignore
                        try writeBatch batch
                        finally Interlocked.Exchange(&compressionActive, 0) |> ignore
            with error ->
                channel.Writer.TryComplete(error) |> ignore
                reraise ())))
        let flush () =
            if count > 0 then
                let batch = Array.sub buffer 0 count
                match queue with
                | Some channel -> channel.Writer.WriteAsync(batch, token).AsTask().GetAwaiter().GetResult()
                | None -> writeBatch batch
                Array.Clear(buffer, 0, count)
                count <- 0
                bytes <- 0L

        member _.ActiveCompressionWorkers = Volatile.Read(&compressionActive)

        member _.Append(row: Row) =
            if closed then raise (ObjectDisposedException("TripCallWriter"))
            token.ThrowIfCancellationRequested()
            if not (isNull previousTrip) then
                let order = StringComparer.Ordinal.Compare(previousTrip, row.tripId)
                if order > 0 || (order = 0 && row.sequence <= previousSequence) then
                    invalidArg "row" $"Trip calls are not strictly ordered at {row.tripId}/{row.sequence}"
            let textBytes (value: string) = if isNull value then 0L else 24L + int64 value.Length * 2L
            let rowBytes = 192L + textBytes row.tripId + textBytes row.locationId + textBytes row.boardingPointId + textBytes row.routeStopId + textBytes row.headsign
            if rowBytes > batchByteLimit then invalidArg "row" "One trip call exceeds the 32 MiB buffer budget"
            if count = capacity || bytes + rowBytes > batchByteLimit then flush ()
            buffer.[count] <- row
            count <- count + 1
            bytes <- bytes + rowBytes
            previousTrip <- row.tripId
            previousSequence <- row.sequence

        member _.Complete() =
            if not closed then
                flush ()
                queue |> Option.iter (fun channel -> channel.Writer.TryComplete() |> ignore)
                worker |> Option.iter (fun task -> task.GetAwaiter().GetResult())
                (writer :> IDisposable).Dispose()
                closed <- true
            writer.RowCount

        interface IDisposable with
            member _.Dispose() =
                if not closed then
                    closed <- true
                    queue |> Option.iter (fun channel -> channel.Writer.TryComplete() |> ignore)
                    try worker |> Option.iter (fun task -> task.GetAwaiter().GetResult())
                    finally (writer :> IDisposable).Dispose()
