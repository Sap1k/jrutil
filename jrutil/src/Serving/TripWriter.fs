// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO
open System.Globalization
open JrUtil.GtfsModel

/// The same typed trip projection is available to every compiler producer.
module TripWriter =
    type private Row = {
        id: string; route: string; service: string; direction: Nullable<int16>
        headsign: string; shortName: string; block: string; wheelchair: Nullable<int16>
        bikes: Nullable<int16>; shape: string
    }

    let write path token progress (trips: seq<Trip>) =
        let text value = value |> Option.filter (String.IsNullOrWhiteSpace >> not) |> Option.defaultValue ""
        let number value =
            match text value with
            | "" -> Nullable()
            | value -> Nullable(Int16.Parse(value, CultureInfo.InvariantCulture))
        let rows = trips |> Seq.map (fun trip -> {
            id = trip.id; route = trip.routeId; service = trip.serviceId; direction = number trip.directionId
            headsign = text trip.headsign; shortName = text trip.shortName; block = text trip.blockId
            wheelchair = number trip.wheelchairAccessible; shape = text trip.shapeId
            bikes = match trip.bikesAllowed with
                    | None -> Nullable()
                    | Some NoInformation -> Nullable 0s
                    | Some OneOrMore -> Nullable 1s
                    | Some NoBicycles -> Nullable 2s })
        let strings row = [| row.id; row.route; row.service; row.headsign; row.shortName; row.block; row.shape |]
        let encode (output: BinaryWriter) row =
            for value in strings row do output.Write(value)
            for value in [| row.direction; row.wheelchair; row.bikes |] do
                output.Write(value.HasValue)
                if value.HasValue then output.Write(value.Value)
        let decode (input: BinaryReader) =
            let values = Array.init 7 (fun _ -> input.ReadString())
            let number () = if input.ReadBoolean() then Nullable(input.ReadInt16()) else Nullable()
            { id = values.[0]; route = values.[1]; service = values.[2]; headsign = values.[3]
              shortName = values.[4]; block = values.[5]; shape = values.[6]
              direction = number (); wheelchair = number (); bikes = number () }
        let bytes row = 224L + (strings row |> Array.sumBy (fun value -> 2L * int64 value.Length))
        let columns rows =
            let column project = Array.map project rows
            let optional project = ColumnWriter.Text(column (fun row -> let value = project row in if value = "" then null else value))
            [| ColumnWriter.Text(column _.id); ColumnWriter.Text(column _.route); ColumnWriter.Text(column _.service)
               ColumnWriter.OptionalInt16(column _.direction); optional _.headsign; optional _.shortName; optional _.block
               ColumnWriter.OptionalInt16(column _.wheelchair); ColumnWriter.OptionalInt16(column _.bikes); optional _.shape |]
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "trip")
        RelationWriter.write path schema (16L * 1024L * 1024L) 32768 (4L * 1024L * 1024L) 8192 token progress
            bytes (fun left right -> StringComparer.Ordinal.Compare(left.id, right.id)) (=) encode decode columns rows
