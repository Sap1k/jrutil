// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text
open System.Threading
open System.Globalization
open Parquet
open Parquet.Schema

open JrUtil.RegionalOverlay.Support

module PackageReader =
    let private readValues<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> ValueType>
                          (group: ParquetRowGroupReader) field count =
        let result = Array.zeroCreate<'T> count
        group.ReadAsync<'T>(field, result.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
        result |> Array.map (fun value -> Convert.ToString(value, CultureInfo.InvariantCulture))

    let private readNullableValues<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> ValueType>
                                  (group: ParquetRowGroupReader) field count =
        let result = Array.zeroCreate<Nullable<'T>> count
        group.ReadAsync<'T>(field, result.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
        result |> Array.map (fun value -> if value.HasValue then Convert.ToString(value.Value, CultureInfo.InvariantCulture) else "")

    let private readColumn (group: ParquetRowGroupReader) (field: DataField) count =
        if field.ClrType = typeof<string> then
            let result = Array.zeroCreate<string> count
            group.ReadAsync(field, result.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
            result |> Array.map (Option.ofObj >> Option.defaultValue "")
        elif field.ClrType = typeof<int> then
            if field.IsNullable then readNullableValues<int> group field count else readValues<int> group field count
        elif field.ClrType = typeof<int64> then
            if field.IsNullable then readNullableValues<int64> group field count else readValues<int64> group field count
        elif field.ClrType = typeof<int16> then
            if field.IsNullable then readNullableValues<int16> group field count else readValues<int16> group field count
        elif field.ClrType = typeof<double> then
            if field.IsNullable then readNullableValues<double> group field count else readValues<double> group field count
        elif field.ClrType = typeof<bool> then
            if field.IsNullable then readNullableValues<bool> group field count else readValues<bool> group field count
        elif field.ClrType = typeof<DateTime> then
            if field.IsNullable then readNullableValues<DateTime> group field count else readValues<DateTime> group field count
        else invalidOp $"Unsupported compiler-view field type {field.ClrType.FullName} for {field.Name}"

    let internal readTextRows path (columns: string array) = seq {
        use stream = File.OpenRead(path)
        let reader = ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let fields = reader.Schema.DataFields |> Seq.map (fun field -> field.Name, field) |> dict
            for groupIndex in 0 .. reader.RowGroupCount - 1 do
                use group = reader.OpenRowGroupReader(groupIndex)
                let count = int group.RowCount
                let values =
                    columns |> Array.map (fun column ->
                        readColumn group fields.[column] count)
                for index in 0 .. count - 1 do
                    yield values |> Array.map (fun column -> Option.ofObj column.[index] |> Option.defaultValue "")
        finally
            reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
    }

    let private writeCsv (path: string) (columns: string array) (rows: seq<string array>) =
        Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
        use writer = new StreamWriter(path, false, new UTF8Encoding(false))
        writer.NewLine <- "\n"
        writeCsvRow writer columns
        for row in rows do writeCsvRow writer row

    /// Materialize a private compiler view from a validated production package.
    /// The compatibility CSVs never escape scratch; public identity stays in
    /// serving relations.
    let prepareCompilerView packageDirectory scratchDirectory =
        Validation.validatePackage packageDirectory |> ignore
        let root = Path.Combine(scratchDirectory, "base-production-view")
        let gtfs, extensions = Path.Combine(root, "gtfs"), Path.Combine(root, "extensions")
        Directory.CreateDirectory(gtfs) |> ignore
        Directory.CreateDirectory(extensions) |> ignore
        use archive = ZipFile.OpenRead(Path.Combine(packageDirectory, "gtfs.zip"))
        for entry in archive.Entries do
            if entry.FullName.Contains('/') || entry.FullName.Contains('\\') then
                invalidArg "packageDirectory" "GTFS ZIP entries must be flat"
            entry.ExtractToFile(Path.Combine(gtfs, entry.FullName), false)

        let serving relation = Path.Combine(packageDirectory, "serving", relation + ".parquet")
        let routeKeys = readTextRows (serving "road_route_key") [| "route_id"; "cis_line_id" |] |> Seq.distinct |> Seq.sortBy id
        writeCsv (Path.Combine(extensions, "cz_routes.txt"))
            [| "route_id"; "cis_line_id"; "public_line_number"; "source_provenance" |]
            (routeKeys |> Seq.map (fun row -> [| row.[0]; row.[1]; ""; "serving-v2" |]))

        let locations = readTextRows (serving "location") [| "location_id"; "parent_location_id" |]
        writeCsv (Path.Combine(extensions, "cz_stops.txt"))
            [| "stop_id"; "stop_place_id"; "cis_stop_id"; "post_id"; "asw_id"; "source_ids" |]
            (locations |> Seq.map (fun row -> [| row.[0]; if row.[1] = "" then row.[0] else row.[1]; ""; ""; ""; "" |]))

        let road =
            readTextRows (serving "road_trip_key") [| "trip_id"; "cis_line_id"; "cis_trip_id" |]
            |> Seq.map (fun row -> row.[0], (row.[1], row.[2], ""))
        let rail =
            readTextRows (serving "rail_trip_key") [| "trip_id"; "train_number" |]
            |> Seq.map (fun row -> row.[0], ("", "", row.[1]))
        let tripKeys = Seq.append road rail |> Seq.groupBy fst |> Seq.map (fun (trip, values) ->
            let values = values |> Seq.map snd |> Seq.toArray
            let cisLine = values |> Seq.map (fun (line, _, _) -> line) |> Seq.tryFind ((<>) "") |> Option.defaultValue ""
            let cisTrip = values |> Seq.map (fun (_, trip, _) -> trip) |> Seq.tryFind ((<>) "") |> Option.defaultValue ""
            let train = values |> Seq.map (fun (_, _, train) -> train) |> Seq.tryFind ((<>) "") |> Option.defaultValue ""
            [| trip; cisLine; cisTrip; train; ""; "" |])
        writeCsv (Path.Combine(extensions, "cz_trips.txt"))
            [| "trip_id"; "cis_line_id"; "cis_trip_id"; "train_number"; "source_trip_ids"; "coverage_sources" |]
            tripKeys

        let routeStopLocations =
            readTextRows (serving "route_stop") [| "route_id"; "route_stop_id"; "location_id" |]
            |> Seq.map (fun row -> struct(row.[0], row.[1]), row.[2]) |> dict
        let zoneFacts =
            readTextRows (serving "fare_zone") [| "zone_id"; "zone_code"; "fare_system_id"; "source_id" |]
            |> Seq.map (fun row -> row.[0], row.[1..3]) |> dict
        let routeStopZones =
            readTextRows (serving "route_stop_zone") [| "route_id"; "route_stop_id"; "zone_id"; "source_order" |]
            |> Seq.choose (fun row ->
                match routeStopLocations.TryGetValue(struct(row.[0], row.[1])), zoneFacts.TryGetValue(row.[2]) with
                | (true, location), (true, zone) -> Some [| location; row.[2]; zone.[0]; row.[0]; zone.[1]; zone.[2] |]
                | _ -> None)
        writeCsv (Path.Combine(extensions, "cz_stop_zones.txt"))
            [| "stop_place_id"; "zone_id"; "zone_code"; "route_id"; "ids_system_id"; "source_provenance" |]
            routeStopZones
        writeCsv (Path.Combine(extensions, "cz_trip_stop_zones.txt"))
            [| "trip_id"; "stop_sequence"; "zone_id"; "zone_code"; "ids_system_id"; "source_provenance" |]
            (readTextRows (serving "call_zone") [| "trip_id"; "sequence"; "zone_id" |]
             |> Seq.map (fun row -> [| row.[0]; row.[1]; row.[2]; ""; ""; "serving-v2" |]))
        gtfs, extensions
