// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO

/// Typed `assignment` rows: note links, service features, location features
/// and calendar designations, written once and ordered by id.
module AssignmentWriter =
    type Row = {
        id: string; scope: string; kind: string
        route: string; trip: string; callSequence: Nullable<int>; callSequenceTo: Nullable<int>
        service: string; location: string; code: string; note: string; sourceObject: string
    }

    let empty = {
        id = ""; scope = ""; kind = ""; route = ""; trip = ""; callSequence = Nullable(); callSequenceTo = Nullable()
        service = ""; location = ""; code = ""; note = ""; sourceObject = "" }

    let private strings row = [| row.id; row.scope; row.kind; row.route; row.trip; row.service; row.location; row.code; row.note; row.sourceObject |]

    let write path token progress (rows: seq<Row>) =
        let text value = if isNull value then "" else value
        let size row = 256L + (strings row |> Array.sumBy (fun value -> 2L * int64 (text value).Length))
        let writeOptional (output: BinaryWriter) (value: Nullable<int>) =
            output.Write(value.HasValue)
            if value.HasValue then output.Write(value.Value)
        let readOptional (input: BinaryReader) = if input.ReadBoolean() then Nullable(input.ReadInt32()) else Nullable()
        let encode (output: BinaryWriter) row =
            for value in strings row do output.Write(text value)
            writeOptional output row.callSequence; writeOptional output row.callSequenceTo
        let decode (input: BinaryReader) =
            let values = Array.init 10 (fun _ -> input.ReadString())
            { id = values.[0]; scope = values.[1]; kind = values.[2]; route = values.[3]; trip = values.[4]
              service = values.[5]; location = values.[6]; code = values.[7]; note = values.[8]; sourceObject = values.[9]
              callSequence = readOptional input; callSequenceTo = readOptional input }
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "assignment")
        let columns rows =
            let column f = rows |> Array.map f
            let optional f = ColumnWriter.Text(column (fun row -> let value = f row in if String.IsNullOrWhiteSpace(value) then null else value))
            [| ColumnWriter.Text(column _.id); ColumnWriter.Text(column _.scope); ColumnWriter.Text(column _.kind)
               optional _.route; optional _.trip
               ColumnWriter.OptionalInt32(column _.callSequence); ColumnWriter.OptionalInt32(column _.callSequenceTo)
               optional _.service; optional _.location; optional _.code; optional _.note; optional _.sourceObject |]
        RelationWriter.write path schema (16L * 1024L * 1024L) (4L * 1024L * 1024L) 8192 token progress
            size (fun row -> row.id) encode decode columns rows
