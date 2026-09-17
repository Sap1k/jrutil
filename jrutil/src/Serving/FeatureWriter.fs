// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO

module FeatureWriter =
    type Row = {
        scope: string; route: string; trip: string; callSequence: Nullable<int>
        service: string; code: string; kind: string; note: string; sourceObject: string
    }

    let write path sourceId digest token progress (rows: seq<Row>) =
        let keyed = rows |> Seq.map (fun row ->
            struct(Identity.bindingId "service-feature" [
                "scope", row.scope; "route", row.route; "trip", row.trip
                "call", if row.callSequence.HasValue then string row.callSequence.Value else ""
                "service", row.service; "code", row.code; "kind", row.kind
                "note", row.note; "source", sourceId ], row))
        let size struct((key: string), row) =
            256L + int64 (key.Length + row.scope.Length + row.route.Length + row.trip.Length
                          + row.service.Length + row.code.Length + row.kind.Length
                          + row.note.Length + row.sourceObject.Length) * 2L
        let encode (output: BinaryWriter) struct(key, row) =
            output.Write(key: string)
            for value in [| row.scope; row.route; row.trip; row.service; row.code; row.kind; row.note; row.sourceObject |] do
                output.Write(value)
            output.Write(row.callSequence.HasValue)
            if row.callSequence.HasValue then output.Write(row.callSequence.Value)
        let decode (input: BinaryReader) =
            let key = input.ReadString()
            let values = Array.init 8 (fun _ -> input.ReadString())
            let call = if input.ReadBoolean() then Nullable(input.ReadInt32()) else Nullable()
            struct(key, { scope = values.[0]; route = values.[1]; trip = values.[2]
                          service = values.[3]; code = values.[4]; kind = values.[5]
                          note = values.[6]; sourceObject = values.[7]; callSequence = call })
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "service_feature_assignment")
        let columns rows =
            let column f = rows |> Array.map (fun struct(_, row) -> f row)
            let normalize value = if String.IsNullOrWhiteSpace(value) then null else value
            let text value = ColumnWriter.Text(Array.create rows.Length value)
            [| ColumnWriter.Text(rows |> Array.map (fun struct(key, _) -> key)); ColumnWriter.Text(column _.scope)
               ColumnWriter.Text(column _.kind); ColumnWriter.Text(column (fun row -> normalize row.route)); ColumnWriter.Text(column (fun row -> normalize row.trip))
               ColumnWriter.OptionalInt32(column _.callSequence); ColumnWriter.Text(column (fun row -> normalize row.service))
               ColumnWriter.Text(column _.code); ColumnWriter.Text(column (fun row -> normalize row.note)); text sourceId; text digest
               ColumnWriter.Text(column _.sourceObject) |]
        RelationWriter.write path schema (16L * 1024L * 1024L) 32768 (4L * 1024L * 1024L) 8192 token progress
            size (fun struct(left, _) struct(right, _) -> StringComparer.Ordinal.Compare(left, right)) (=)
            encode decode columns keyed
