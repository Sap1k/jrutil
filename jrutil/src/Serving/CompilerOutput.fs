// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Json
open System.Threading
open Parquet

open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Support

/// A compiler's complete hand-off to the package writer: replayable row
/// sources that a compiler backs with memory or with its own scratch spools,
/// plus build facts. Nothing about its on-disk layout is a contract.
module CompilerOutput =
    /// A replayable text table. Every enumeration of `rows` yields the same rows.
    /// Cells of absent columns read as "". `file` is the table's canonical CSV
    /// text when a compiler already spooled it, so it can be copied verbatim.
    type Table = {
        columns: string array
        rows: unit -> seq<string array>
        file: string option
    }

    type Output = {
        /// Standard GTFS tables by file name, e.g. "trips.txt".
        gtfs: IReadOnlyDictionary<string, Table>
        /// Compiler-internal Czech tables (cz_trips.txt, cz_routes.txt, ...).
        czech: IReadOnlyDictionary<string, Table>
        /// Overlay projection mappings (base_to_output_trips.csv, ...).
        mappings: IReadOnlyDictionary<string, Table>
        /// Compiler reports (diagnostics.csv, coverage.csv, ...).
        reports: IReadOnlyDictionary<string, Table>
        /// Source metadata tables by relation name (source_call_metadata, operational_calls, ...).
        sidecars: IReadOnlyDictionary<string, Table>
        /// Build facts: sources, conversion options, publication flags.
        manifest: JsonElement
        /// Compiler diagnostics (`{"diagnostics": [...]}`), if any.
        diagnostics: JsonElement option
        /// The validated base package an overlay was compiled against.
        basePackage: string option
        /// Files copied verbatim into the optional diagnostics artifact, keyed
        /// by artifact-relative path.
        diagnosticFiles: IReadOnlyDictionary<string, string>
    }

    let private tryTable (tables: IReadOnlyDictionary<string, Table>) name =
        match tables.TryGetValue(name) with
        | true, table -> Some table
        | _ -> None

    let has (tables: IReadOnlyDictionary<string, Table>) name = (tryTable tables name).IsSome

    /// Rows projected onto `columns`, "" for columns the table lacks.
    let values (tables: IReadOnlyDictionary<string, Table>) name (columns: string array) =
        match tryTable tables name with
        | None -> Seq.empty
        | Some table ->
            let indexes = columns |> Array.map (fun column -> Array.IndexOf(table.columns, column))
            table.rows () |> Seq.map (fun row ->
                indexes |> Array.map (fun index -> if index >= 0 && index < row.Length then row.[index] else ""))

    /// Rows as column-name dictionaries (the former CSV row shape).
    let rows (tables: IReadOnlyDictionary<string, Table>) name : seq<CsvRow> =
        match tryTable tables name with
        | None -> Seq.empty
        | Some table ->
            table.rows () |> Seq.map (fun row ->
                let result = CsvRow(StringComparer.Ordinal)
                for index in 0 .. table.columns.Length - 1 do
                    result.[table.columns.[index]] <- if index < row.Length then row.[index] else ""
                result)

    let columnsOf (tables: IReadOnlyDictionary<string, Table>) name =
        match tryTable tables name with
        | Some table -> table.columns
        | None -> [||]

    let tables (entries: seq<string * Table>) : IReadOnlyDictionary<string, Table> =
        Dictionary<string, Table>(dict entries, StringComparer.Ordinal) :> IReadOnlyDictionary<_, _>

    let noTables = tables []

    let memoryTable (columns: string array) (rows: unit -> seq<string array>) =
        { columns = columns; rows = rows; file = None }

    // TextFieldParser is intentionally general, but its per-field machinery is
    // disproportionately expensive on the 13M-row stop_times hot path.  This
    // parser implements the same RFC-style comma/quote rules, including doubled
    // quotes and embedded newlines.
    let private parseRecord (name: string) (record: string) =
        let values = ResizeArray<string>()
        let field = StringBuilder()
        let mutable quoted, index = false, 0
        while index < record.Length do
            let character = record.[index]
            if quoted then
                if character = '"' then
                    if index + 1 < record.Length && record.[index + 1] = '"' then
                        field.Append('"') |> ignore; index <- index + 1
                    else quoted <- false
                else field.Append(character) |> ignore
            elif character = ',' then
                values.Add(field.ToString()); field.Clear() |> ignore
            elif character = '"' && field.Length = 0 then quoted <- true
            else field.Append(character) |> ignore
            index <- index + 1
        if quoted then invalidOp $"{name} contains an unterminated quoted field"
        values.Add(field.ToString())
        values.ToArray()

    let private hasOpenQuote (record: string) =
        let mutable quoted, index = false, 0
        while index < record.Length do
            if record.[index] = '"' then
                if quoted && index + 1 < record.Length && record.[index + 1] = '"' then index <- index + 1
                else quoted <- not quoted
            index <- index + 1
        quoted

    let private readRecord (reader: StreamReader) =
        let first = reader.ReadLine()
        if isNull first || not (hasOpenQuote first) then first else
        let builder = StringBuilder(first)
        let mutable openQuote = true
        while openQuote && not reader.EndOfStream do
            builder.Append('\n').Append(reader.ReadLine()) |> ignore
            openQuote <- hasOpenQuote (builder.ToString())
        builder.ToString()

    let private csvHeader (path: string) =
        use reader = new StreamReader(path, Encoding.UTF8, true)
        match readRecord reader with
        | null -> [||]
        | header ->
            let columns = parseRecord path header
            if columns.Length > 0 then columns.[0] <- columns.[0].TrimStart('﻿')
            columns

    /// A CSV file (a compiler's own scratch spool) read as a table.
    let csvFileTable (path: string) =
        { columns = csvHeader path
          file = Some path
          rows = fun () -> seq {
              use reader = new StreamReader(path, Encoding.UTF8, true, 1024 * 1024)
              readRecord reader |> ignore
              while not reader.EndOfStream do
                  let record = readRecord reader
                  if not (isNull record) && record <> "" then yield parseRecord path record
          } }

    /// A Parquet file read as a text table with all of its columns.
    let parquetFileTable (path: string) =
        let columns =
            use stream = File.OpenRead(path)
            let reader = ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
            try reader.Schema.DataFields |> Array.map (fun field -> field.Name)
            finally reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
        { columns = columns; file = None; rows = fun () -> PackageReader.readTextRows path columns }

    /// Write a table as canonical CSV: every field quoted, LF line endings.
    let writeCsv (writer: TextWriter) (table: Table) =
        writeCsvRow writer table.columns
        for row in table.rows () do writeCsvRow writer row
