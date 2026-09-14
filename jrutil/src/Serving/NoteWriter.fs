// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO
open System.Threading

module NoteWriter =
    type Row = {
        id: string; kind: string; route: string; trip: string
        label: string; text: string; validFrom: Nullable<DateOnly>; validTo: Nullable<DateOnly>
        serviceNoteType: string
    }

    let private scope row =
        if not (String.IsNullOrEmpty(row.trip)) then "trip"
        elif not (String.IsNullOrEmpty(row.route)) then "route" else "source"
    let private assignmentId row =
        Identity.bindingId "note-assignment" [ "note", row.id; "scope", scope row;
                                             "route", row.route; "trip", row.trip ]

    let write (prefix: string) sourceId digest token (progress: string -> int64 -> unit) (rows: unit -> seq<Row>) =
        let normalize text = if String.IsNullOrWhiteSpace(text) then null else text
        let writeDate (output: BinaryWriter) (value: Nullable<DateOnly>) =
            output.Write(value.HasValue)
            if value.HasValue then output.Write(value.Value.DayNumber)
        let readDate (input: BinaryReader) = if input.ReadBoolean() then Nullable(DateOnly.FromDayNumber(input.ReadInt32())) else Nullable()
        let encode (output: BinaryWriter) row =
            for text in [| row.id; row.kind; row.route; row.trip; row.label; row.text; row.serviceNoteType |] do
                output.Write(if isNull text then "" else text)
            writeDate output row.validFrom; writeDate output row.validTo
        let decode (input: BinaryReader) =
            let values = Array.init 7 (fun _ -> input.ReadString())
            { id = values.[0]; kind = values.[1]; route = values.[2]; trip = values.[3]; label = values.[4]
              text = values.[5]; serviceNoteType = values.[6]; validFrom = readDate input; validTo = readDate input }
        let bytes row =
            192L + ([| row.id; row.kind; row.route; row.trip; row.label; row.text; row.serviceNoteType |]
                    |> Array.sumBy (fun value -> if isNull value then 0L else 24L + int64 value.Length * 2L))
        let relation name (key: Row -> string) columns =
            let path = prefix + "." + name + ".parquet"
            let schema = Schema.relations |> Array.find (fun relation -> relation.name = name)
            let keyed = rows () |> Seq.map (fun row -> struct(key row, row))
            let count =
                RelationWriter.write path schema (16L * 1024L * 1024L) 32768 (4L * 1024L * 1024L) 8192 token
                    (fun phase count -> progress (name + "-" + phase) count)
                    (fun struct((key: string), row) -> bytes row + 48L + int64 key.Length * 2L)
                    (fun struct(left, _) struct(right, _) -> StringComparer.Ordinal.Compare(left, right)) (=)
                    (fun output struct(key, row) -> output.Write(key: string); encode output row)
                    (fun input -> let key = input.ReadString() in struct(key, decode input)) columns keyed
            name, (path, count)
        let note = relation "service_note" _.id (fun rows ->
            let column f = rows |> Array.map (fun struct(_, row) -> f row)
            [| ColumnWriter.Text(column _.id); ColumnWriter.Text(column _.kind)
               ColumnWriter.Text(column (fun row -> normalize row.label)); ColumnWriter.Text(column (fun row -> normalize row.text))
               ColumnWriter.OptionalDate(column _.validFrom); ColumnWriter.OptionalDate(column _.validTo)
               ColumnWriter.Text(column (fun row -> normalize row.serviceNoteType))
               ColumnWriter.Text(Array.create rows.Length sourceId); ColumnWriter.Text(Array.create rows.Length digest)
               ColumnWriter.Text(column _.id) |])
        let assignments = relation "service_note_assignment" assignmentId (fun rows ->
            let column f = rows |> Array.map (fun struct(_, row) -> f row)
            [| ColumnWriter.Text(rows |> Array.map (fun struct(key, _) -> key)); ColumnWriter.Text(column _.id)
               ColumnWriter.Text(column scope); ColumnWriter.Text(column (fun row -> normalize row.route))
               ColumnWriter.Text(column (fun row -> normalize row.trip)); ColumnWriter.Text(Array.create rows.Length null) |])
        Map [note; assignments]
