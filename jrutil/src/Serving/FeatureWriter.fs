// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO

module FeatureWriter =
    type Row = { trip: string; code: string; kind: string; sourceObject: string }

    let write path sourceId digest token progress (rows: seq<Row>) =
        let keyed = rows |> Seq.map (fun row ->
            struct(Identity.bindingId "service-feature" [ "trip", row.trip; "code", row.code; "source", sourceId ], row))
        let size struct((key: string), row) =
            192L + int64 (key.Length + row.trip.Length + row.code.Length + row.kind.Length + row.sourceObject.Length) * 2L
        let encode (output: BinaryWriter) struct(key, row) =
            output.Write(key: string); output.Write(row.trip); output.Write(row.code)
            output.Write(row.kind); output.Write(row.sourceObject)
        let decode (input: BinaryReader) =
            let key = input.ReadString()
            struct(key, { trip = input.ReadString(); code = input.ReadString(); kind = input.ReadString(); sourceObject = input.ReadString() })
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "service_feature_assignment")
        let columns rows =
            let column f = rows |> Array.map (fun struct(_, row) -> f row)
            let text value = ColumnWriter.Text(Array.create rows.Length value)
            [| ColumnWriter.Text(rows |> Array.map (fun struct(key, _) -> key)); text "trip"
               ColumnWriter.Text(column _.kind); text null; ColumnWriter.Text(column _.trip)
               ColumnWriter.OptionalInt32(Array.create rows.Length (Nullable())); text null
               ColumnWriter.Text(column _.code); text null; text sourceId; text digest
               ColumnWriter.Text(column _.sourceObject) |]
        RelationWriter.write path schema (16L * 1024L * 1024L) 32768 (4L * 1024L * 1024L) 8192 token progress
            size (fun struct(left, _) struct(right, _) -> StringComparer.Ordinal.Compare(left, right)) (=)
            encode decode columns keyed
