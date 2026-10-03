// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open JrUtil
open JrUtil.Hashing
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Runtime
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Parquet
open Parquet.Schema

open JrUtil.RegionalOverlay.Values
open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.OutputIds
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay.Model


/// Shared row, Parquet and manifest helpers for package relations.
module PackageRows =
    type internal TripCallSummary = TripCallWriter.Summary

    type internal TargetCallSchedule = {
        firstSequence: int
        calls: struct(int * Nullable<int> * Nullable<int>) array
    }

    [<Struct>]
    type internal ServingTripRow = {
        tripId: string; routeId: string; serviceId: string
        direction: Nullable<int16>; headsign: string; shortName: string; blockKey: string
        wheelchair: Nullable<int16>; bikes: Nullable<int16>; shapeId: string
    }

    [<Struct>]
    type internal SourceTripProjection = {
        sourceId: string; sourceTripId: string; outputTripId: string
        validFrom: string; validTo: string; methodName: string
    }

    [<Struct>]
    type internal BaseTripProjection = {
        baseTripId: string; outputTripId: string; validFrom: string; validTo: string
    }

    // Retain native binding facts, not a dictionary and boxed values for every trip.
    type internal TripBinding = JrUtil.Serving.Model.TripBinding

    let internal objectRow (values: (string * obj) seq) =
        let result = Dictionary<string, obj>(StringComparer.Ordinal)
        for name, value in values do result.Add(name, value)
        result :> IDictionary<string, obj>

    /// Package finalization follows compilation in the same short-lived CLI
    /// process.  Large compiler graphs and Parquet row-group buffers can
    /// otherwise remain committed while the next nationwide relation is
    /// sorted, even though they are no longer reachable.  This is a phase
    /// boundary reclamation, not a heap limit: live data is never rejected.
    let internal reclaimManagedPhaseMemory = MemoryReclaim.compactingCollection

    let internal nullableString value =
        if String.IsNullOrWhiteSpace(value) then null else box value

    let internal bindingRow (binding: TripBinding) =
        objectRow [
            "binding_id", box binding.binding_id
            "source_id", box binding.source_id
            "trip_namespace", box binding.trip_namespace
            "source_trip_id", box binding.source_trip_id
            "trip_id", box binding.trip_id
            "service_id", box binding.service_id
            "valid_from", box binding.valid_from
            "valid_to", box binding.valid_to
            "binding_status", box binding.binding_status
            "scheduled_start", box binding.scheduled_start
            "scheduled_end", box binding.scheduled_end
            "source_route_id", nullableString binding.source_route_id
            "source_direction_id", nullableString binding.source_direction_id
            "source_start_location_id", nullableString binding.source_start_location_id
            "source_end_location_id", nullableString binding.source_end_location_id
            "source_block_id", nullableString binding.source_block_id
            "call_pattern_sha256", box binding.call_pattern_sha256
            "variant_key", nullableString binding.variant_key
        ]

    let internal nullableParsed parse value =
        if String.IsNullOrWhiteSpace(value) then null else box (parse value)

    let internal integer (value: string) = Int32.Parse(value, CultureInfo.InvariantCulture)
    let internal integer64 (value: string) = Int64.Parse(value, CultureInfo.InvariantCulture)
    let internal int16 (value: string) = Int16.Parse(value, CultureInfo.InvariantCulture)
    let internal number (value: string) = Double.Parse(value, CultureInfo.InvariantCulture)
    let internal date (value: string) =
        let compact = value.Replace("-", "")
        match DateOnly.TryParseExact(compact, "yyyyMMdd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, parsed -> parsed
        | _ -> DateOnly.FromDateTime(DateTime.Parse(value, CultureInfo.InvariantCulture, DateTimeStyles.AllowWhiteSpaces))

    let internal seconds value =
        if String.IsNullOrWhiteSpace(value) then null
        else
            let parts = value.Split(':') |> Array.map integer
            box (parts.[0] * 3600 + parts.[1] * 60 + parts.[2])

    let internal optionalColumn<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> ValueType> (values: obj array) =
        values |> Array.map (fun value -> if isNull value then Nullable() else Nullable(unbox<'T> value))

    let internal columnFromRows (field: JrUtil.Serving.Schema.Field) (rows: IDictionary<string,obj> array) =
        let values = rows |> Array.map (fun row -> row.[field.name])
        match field.dataType, field.nullable with
        | Schema.Text, _ -> ColumnWriter.Text(values |> Array.map (fun value -> if isNull value then null else unbox<string> value))
        | Schema.Int16, false -> ColumnWriter.Int16(values |> Array.map unbox<int16>)
        | Schema.Int16, true -> ColumnWriter.OptionalInt16(optionalColumn<int16> values)
        | Schema.Int32, false -> ColumnWriter.Int32(values |> Array.map unbox<int>)
        | Schema.Int32, true -> ColumnWriter.OptionalInt32(optionalColumn<int> values)
        | Schema.Int64, false -> ColumnWriter.Int64(values |> Array.map unbox<int64>)
        | Schema.Int64, true -> ColumnWriter.OptionalInt64(optionalColumn<int64> values)
        | Schema.Float64, false -> ColumnWriter.Float64(values |> Array.map unbox<double>)
        | Schema.Float64, true -> ColumnWriter.OptionalFloat64(optionalColumn<double> values)
        | Schema.Boolean, false -> ColumnWriter.Boolean(values |> Array.map unbox<bool>)
        | Schema.Boolean, true -> ColumnWriter.OptionalBoolean(optionalColumn<bool> values)
        | Schema.Date, false -> ColumnWriter.Date(values |> Array.map unbox<DateOnly>)
        | Schema.Date, true -> ColumnWriter.OptionalDate(optionalColumn<DateOnly> values)

    let internal writeParquet progress path (relation: Schema.Relation) (rows: seq<IDictionary<string,obj>>) =
        use writer = new ColumnWriter.Writer(path, relation, 65536, CancellationToken.None)
        let buffer = ResizeArray<IDictionary<string,obj>>(65536)
        let maximumBufferBytes = 16L * 1024L * 1024L
        let mutable bufferBytes = int64 relation.fields.Length * 24L
        let flush () =
            if buffer.Count > 0 then
                let values = buffer.ToArray()
                writer.Append(relation.fields |> Array.map (fun field -> columnFromRows field values))
                progress writer.RowCount
                buffer.Clear()
                bufferBytes <- int64 relation.fields.Length * 24L
        for row in rows do
            // This transitional producer still owns dictionaries. Account for
            // them as well as the typed columns built during a synchronous flush.
            let bytes =
                128L + (row |> Seq.sumBy (fun pair ->
                    96L + (match pair.Value with | :? string as value -> 24L + 2L * int64 value.Length | _ -> 16L)))
            if bytes + int64 relation.fields.Length * 24L > maximumBufferBytes then
                invalidOp $"{relation.name}: one row exceeds the {maximumBufferBytes}-byte buffer limit"
            if buffer.Count > 0 && bufferBytes + bytes > maximumBufferBytes then flush ()
            buffer.Add(row)
            bufferBytes <- bufferBytes + bytes
            if buffer.Count = 65536 then flush ()
        flush ()
        int writer.RowCount

    let internal value column (row: CsvRow) = rowValue row column

    let internal sourceManifest (manifest: JsonElement) =
        let descriptor (source: JsonElement) =
            let text (names: string list) =
                names |> Seq.tryPick (fun (name: string) -> match source.TryGetProperty(name) with | true, value -> Some(value.GetString()) | _ -> None)
                |> Option.defaultValue null
            text [ "source_id" ], text [ "payload_sha256"; "sha256" ]
        seq {
            match manifest.TryGetProperty("sources") with
            | true, sources ->
                for source in sources.EnumerateArray() do
                    yield descriptor source
            | _ ->
                match manifest.TryGetProperty("source") with
                | true, source -> yield descriptor source
                | _ ->
                    match manifest.TryGetProperty("source_snapshot") with
                    | true, source -> yield descriptor source
                    | _ -> ()
        }
        |> Seq.choose (fun (source, digest) ->
            if isNull source || isNull digest then None else Some (source, digest))
        |> dict

    let internal callSequenceKey trip sequence = Identity.compositeKey [ trip; sequence ]

    let internal fieldText (value: obj) =
        if isNull value then "" else
        match value with
        | :? DateOnly as date -> date.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
        | _ -> Convert.ToString(value, CultureInfo.InvariantCulture)

    let internal parseField (field: JrUtil.Serving.Schema.Field) (value: string) =
        if field.nullable && String.IsNullOrEmpty(value) then null else
        match field.dataType with
        | Schema.Text -> box value
        | Schema.Int16 -> box (int16 value)
        | Schema.Int32 -> box (integer value)
        | Schema.Int64 -> box (integer64 value)
        | Schema.Float64 -> box (number value)
        | Schema.Boolean -> box (Boolean.Parse value)
        | Schema.Date -> box (date value)

    let internal combinedSourceManifest (input: CompilerOutput.Output) (manifest: JsonElement) =
        let result = Dictionary<string,string>(StringComparer.Ordinal)
        match input.basePackage with
        | Some package ->
            let path = Path.Combine(package, "manifest.json")
            if File.Exists(path) then
                use document = JsonDocument.Parse(File.ReadAllText(path))
                for pair in sourceManifest document.RootElement do result.[pair.Key] <- pair.Value
        | None -> ()
        for pair in sourceManifest manifest do result.[pair.Key] <- pair.Value
        result :> IDictionary<string,string>

    let internal packageRelationRows package name =
        let relation = Schema.relations |> Array.find (fun relation -> relation.name = name)
        let path = Path.Combine(package, "serving", name + ".parquet")
        if not (File.Exists(path)) then Seq.empty else
        PackageReader.readTextRows path (relation.fields |> Array.map _.name)
        |> Seq.map (fun values ->
            objectRow (
                relation.fields
                |> Seq.mapi (fun index field -> field.name, parseField field values.[index])))

    let internal copyRow (row: IDictionary<string,obj>) =
        objectRow (row |> Seq.map (fun pair -> pair.Key, pair.Value))

    let internal setField name value (row: IDictionary<string,obj>) =
        let result = copyRow row
        result.[name] <- value
        result
