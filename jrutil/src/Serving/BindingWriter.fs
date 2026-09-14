// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO
open Model
open System.Collections.Generic
open JrUtil.GtfsModel

module BindingWriter =
    /// Capture retained native trip/calendar facts before the GTFS text sink.
    let writeNativeFacts path sourceId (feed: GtfsFeed) =
        let day (value: NodaTime.LocalDate) = DateOnly(value.Year, value.Month, value.Day)
        let bounds = Dictionary<string, struct(DateOnly * DateOnly)>(StringComparer.Ordinal)
        for entry in feed.calendar |> Option.defaultValue [||] do
            bounds.[entry.id] <- struct(day entry.startDate, day entry.endDate)
        for entry in feed.calendarExceptions |> Option.defaultValue [||] do
            let date = day entry.date
            bounds.[entry.id] <-
                match bounds.TryGetValue(entry.id) with
                | true, struct(first, last) -> struct(min first date, max last date)
                | _ -> struct(date, date)
        use output = new BinaryWriter(File.Create(path))
        output.Write(sourceId: string)
        for trip in feed.trips do
            let struct(first, last) = bounds.[trip.serviceId]
            output.Write(trip.id); output.Write(trip.serviceId)
            output.Write(trip.directionId |> Option.defaultValue "")
            output.Write(trip.blockId |> Option.defaultValue "")
            output.Write(first.DayNumber); output.Write(last.DayNumber)

    let readNativeFacts path summaries = seq {
        let calls = TripCallWriter.readSummaries summaries
        use input = new BinaryReader(File.OpenRead(path))
        let source = input.ReadString()
        while input.BaseStream.Position < input.BaseStream.Length do
            let trip = input.ReadString()
            let service = input.ReadString()
            let direction = input.ReadString()
            let block = input.ReadString()
            let first = DateOnly.FromDayNumber(input.ReadInt32())
            let last = DateOnly.FromDayNumber(input.ReadInt32())
            let call = calls.[trip]
            yield {
                binding_id = Identity.bindingId "trip" [ "source_id", source; "namespace", "gtfs_trip_id";
                                                        "source_trip_id", trip; "trip_id", trip; "service_id", service;
                                                        "valid_from", first.ToString("yyyyMMdd"); "valid_to", last.ToString("yyyyMMdd") ]
                source_id = source; trip_namespace = "gtfs_trip_id"; source_trip_id = trip; trip_id = trip; service_id = service
                valid_from = first; valid_to = last; binding_status = "confirmed"
                scheduled_start = call.scheduledStart; scheduled_end = call.scheduledEnd
                source_route_id = null; source_direction_id = direction; source_start_location_id = call.firstStopId
                source_end_location_id = call.lastStopId; source_block_id = block; source_run_id = null; source_duty_id = null
                call_pattern_sha256 = call.callPatternSha256; variant_key = "source_native" }
    }

    let write path token progress (rows: seq<TripBinding>) =
        let optional value = if String.IsNullOrWhiteSpace(value) then "" else value
        let rows = rows |> Seq.map (fun row ->
            { row with source_route_id = optional row.source_route_id
                       source_direction_id = optional row.source_direction_id
                       source_start_location_id = optional row.source_start_location_id
                       source_end_location_id = optional row.source_end_location_id
                       source_block_id = optional row.source_block_id
                       source_run_id = optional row.source_run_id
                       source_duty_id = optional row.source_duty_id
                       variant_key = optional row.variant_key })
        let strings row = [|
            row.binding_id; row.source_id; row.trip_namespace; row.source_trip_id; row.trip_id; row.service_id
            row.binding_status; row.source_route_id; row.source_direction_id; row.source_start_location_id
            row.source_end_location_id; row.source_block_id; row.source_run_id; row.source_duty_id
            row.call_pattern_sha256; row.variant_key |]
        let size row = 384L + (strings row |> Array.sumBy (fun value -> if isNull value then 0L else 24L + int64 value.Length * 2L))
        let encode (output: BinaryWriter) row =
            for value in strings row do output.Write(if isNull value then "" else value)
            output.Write(row.valid_from.DayNumber); output.Write(row.valid_to.DayNumber)
            for time in [| row.scheduled_start; row.scheduled_end |] do
                output.Write(time.HasValue)
                if time.HasValue then output.Write(time.Value)
        let decode (input: BinaryReader) =
            let values = Array.init 16 (fun _ -> input.ReadString())
            let first, last = DateOnly.FromDayNumber(input.ReadInt32()), DateOnly.FromDayNumber(input.ReadInt32())
            let time () = if input.ReadBoolean() then Nullable(input.ReadInt32()) else Nullable()
            { binding_id = values.[0]; source_id = values.[1]; trip_namespace = values.[2]; source_trip_id = values.[3]
              trip_id = values.[4]; service_id = values.[5]; binding_status = values.[6]; source_route_id = values.[7]
              source_direction_id = values.[8]; source_start_location_id = values.[9]; source_end_location_id = values.[10]
              source_block_id = values.[11]; source_run_id = values.[12]; source_duty_id = values.[13]
              call_pattern_sha256 = values.[14]; variant_key = values.[15]; valid_from = first; valid_to = last
              scheduled_start = time (); scheduled_end = time () }
        let columns rows =
            let column f = Array.map f rows
            let text f = ColumnWriter.Text(column f)
            let optional f = text (fun row -> let value = f row in if String.IsNullOrWhiteSpace(value) then null else value)
            [| text _.binding_id; text _.source_id; text _.trip_namespace; text _.source_trip_id; text _.trip_id; text _.service_id
               ColumnWriter.Date(column _.valid_from); ColumnWriter.Date(column _.valid_to); text _.binding_status
               ColumnWriter.OptionalInt32(column _.scheduled_start); ColumnWriter.OptionalInt32(column _.scheduled_end)
               optional _.source_route_id; optional _.source_direction_id; optional _.source_start_location_id
               optional _.source_end_location_id; optional _.source_block_id; optional _.source_run_id; optional _.source_duty_id
               text _.call_pattern_sha256; optional _.variant_key |]
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "source_trip_map")
        RelationWriter.write path schema (16L * 1024L * 1024L) 32768 (4L * 1024L * 1024L) 8192 token progress
            size (fun left right -> StringComparer.Ordinal.Compare(left.binding_id, right.binding_id)) (=)
            encode decode columns rows
