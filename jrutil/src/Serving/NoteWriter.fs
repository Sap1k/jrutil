// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO

module NoteWriter =
    type Row = {
        id: string; kind: string; route: string; trip: string
        label: string; text: string; validFrom: Nullable<DateOnly>; validTo: Nullable<DateOnly>
        serviceNoteType: string
    }

    /// The `assignment` that links a note to its trip or route.
    let assignment (feed: string) (row: Row) : AssignmentWriter.Row =
        let scope = if not (String.IsNullOrEmpty(row.trip)) then "trip" else "route"
        { AssignmentWriter.empty with
            id = Identity.feedId feed "note-assignment" [ "note", row.id; "scope", scope; "route", row.route; "trip", row.trip ]
            scope = scope; kind = "note"; route = row.route; trip = row.trip; note = row.id }

    /// Write `service_note`; its links are `assignment` rows.
    let write (path: string) token (progress: string -> int64 -> unit) (rows: seq<Row>) =
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
        let schema = Schema.relations |> Array.find (fun relation -> relation.name = "service_note")
        RelationWriter.write path schema (16L * 1024L * 1024L) (4L * 1024L * 1024L) 8192 token
            (fun phase count -> progress ("service_note-" + phase) count)
            bytes _.id encode decode
            (fun rows ->
                let column f = rows |> Array.map f
                [| ColumnWriter.Text(column _.id); ColumnWriter.Text(column _.kind)
                   ColumnWriter.Text(column (fun row -> normalize row.label)); ColumnWriter.Text(column (fun row -> normalize row.text))
                   ColumnWriter.OptionalDate(column _.validFrom); ColumnWriter.OptionalDate(column _.validTo)
                   ColumnWriter.Text(column (fun row -> normalize row.serviceNoteType))
                   ColumnWriter.Text(column _.id) |])
            rows
