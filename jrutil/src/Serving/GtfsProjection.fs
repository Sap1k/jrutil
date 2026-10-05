// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text

/// gtfs.zip as a projection of the finished serving relations: standard GTFS
/// only, passenger calls as stop_times, calendars as calendar.txt and
/// calendar_dates.txt. feed_info.txt is build metadata and is passed through.
module GtfsProjection =
    let private relation name = Schema.relations |> Array.find (fun value -> value.name = name)

    /// Typed cells of the named fields, row by row, formatted for GTFS.
    let private cells (serving: string) (name: string) (fields: string array) = seq {
        let schema = relation name
        let indexes = fields |> Array.map (fun field -> schema.fields |> Array.findIndex (fun value -> value.name = field))
        for columns in ColumnReader.groups (Path.Combine(serving, name + ".parquet")) schema do
            let count = if columns.Length = 0 then 0 else ColumnWriter.rowCount columns.[0]
            let picked = indexes |> Array.map (fun index -> columns.[index])
            for row in 0 .. count - 1 do
                yield picked |> Array.map (fun column ->
                    let invariant (value: 'T) = Convert.ToString(value, CultureInfo.InvariantCulture)
                    let optional (value: Nullable<'T>) = if value.HasValue then invariant value.Value else ""
                    match column with
                    | ColumnWriter.Text values -> if isNull values.[row] then "" else values.[row]
                    | ColumnWriter.Int16 values -> invariant values.[row]
                    | ColumnWriter.OptionalInt16 values -> optional values.[row]
                    | ColumnWriter.Int32 values -> invariant values.[row]
                    | ColumnWriter.OptionalInt32 values -> optional values.[row]
                    | ColumnWriter.Int64 values -> invariant values.[row]
                    | ColumnWriter.OptionalInt64 values -> optional values.[row]
                    | ColumnWriter.Float64 values -> values.[row].ToString("R", CultureInfo.InvariantCulture)
                    | ColumnWriter.OptionalFloat64 values ->
                        if values.[row].HasValue then values.[row].Value.ToString("R", CultureInfo.InvariantCulture) else ""
                    | ColumnWriter.Boolean values -> if values.[row] then "1" else "0"
                    | ColumnWriter.OptionalBoolean values -> if values.[row].HasValue then (if values.[row].Value then "1" else "0") else ""
                    | ColumnWriter.Date values -> values.[row].ToString("yyyyMMdd", CultureInfo.InvariantCulture)
                    | ColumnWriter.OptionalDate values ->
                        if values.[row].HasValue then values.[row].Value.ToString("yyyyMMdd", CultureInfo.InvariantCulture) else "")
    }

    let private time (value: string) =
        if value = "" then "" else
        let seconds = Int32.Parse(value, CultureInfo.InvariantCulture)
        let sign = if seconds < 0 then "-" else ""
        let seconds = abs seconds
        $"{sign}{seconds / 3600:D2}:{seconds / 60 % 60:D2}:{seconds % 60:D2}"

    let private table columns (rows: unit -> seq<string array>) : CompilerOutput.Table =
        { columns = columns; rows = rows; file = None }

    let private nonEmpty (serving: string) name =
        cells serving name [| (relation name).fields.[0].name |] |> Seq.isEmpty |> not

    /// The GTFS tables, by file name, projected from `serving`.
    let tables (serving: string) (feedInfo: CompilerOutput.Table option) =
        let parents =
            lazy (HashSet<string>(cells serving "location" [| "parent_location_id" |] |> Seq.map (fun row -> row.[0]) |> Seq.filter ((<>) ""), StringComparer.Ordinal))
        let result = Dictionary<string, CompilerOutput.Table>(StringComparer.Ordinal)
        result.["agency.txt"] <-
            table [| "agency_id"; "agency_name"; "agency_url"; "agency_timezone"; "agency_lang"; "agency_phone"; "agency_fare_url"; "agency_email" |]
                (fun () -> cells serving "agency" [| "agency_id"; "name"; "url"; "timezone"; "language"; "phone"; "fare_url"; "email" |])
        result.["stops.txt"] <-
            table [| "stop_id"; "stop_name"; "stop_desc"; "stop_lat"; "stop_lon"; "stop_url"; "location_type"; "parent_station"; "stop_timezone"; "wheelchair_boarding"; "platform_code" |]
                (fun () ->
                    cells serving "location"
                        [| "location_id"; "name"; "description"; "latitude"; "longitude"; "url"; "kind"; "parent_location_id"; "timezone"; "wheelchair_boarding"; "public_code" |]
                    |> Seq.filter (fun row -> row.[6] <> "operational_point")
                    |> Seq.map (fun row ->
                        // A stop place that groups boarding points is a station.
                        row.[6] <- if row.[6] = "stop_place" && parents.Value.Contains(row.[0]) then "1" else "0"
                        row))
        result.["routes.txt"] <-
            table [| "route_id"; "agency_id"; "route_short_name"; "route_long_name"; "route_desc"; "route_type"; "route_url"; "route_color"; "route_text_color"; "route_sort_order" |]
                (fun () -> cells serving "route" [| "route_id"; "agency_id"; "short_name"; "long_name"; "description"; "gtfs_route_type"; "url"; "color"; "text_color"; "sort_order" |])
        result.["trips.txt"] <-
            table [| "route_id"; "service_id"; "trip_id"; "trip_headsign"; "trip_short_name"; "direction_id"; "block_id"; "shape_id"; "wheelchair_accessible"; "bikes_allowed" |]
                (fun () -> cells serving "trip" [| "route_id"; "service_id"; "trip_id"; "headsign"; "short_name"; "direction"; "block_key"; "shape_id"; "wheelchair_accessible"; "bikes_allowed" |])
        result.["stop_times.txt"] <-
            table [| "trip_id"; "arrival_time"; "departure_time"; "stop_id"; "stop_sequence"; "stop_headsign"; "pickup_type"; "drop_off_type"; "shape_dist_traveled"; "timepoint" |]
                (fun () ->
                    cells serving "trip_call"
                        [| "trip_id"; "scheduled_arrival"; "scheduled_departure"; "boarding_point_id"; "location_id"; "sequence"; "stop_headsign"
                           "pickup_type"; "dropoff_type"; "shape_distance_traveled"; "timepoint"; "passenger_service" |]
                    |> Seq.filter (fun row -> row.[11] = "1")
                    |> Seq.map (fun row ->
                        [| row.[0]; time row.[1]; time row.[2]; (if row.[3] = "" then row.[4] else row.[3]); row.[5]; row.[6]
                           row.[7]; row.[8]; row.[9]; row.[10] |]))
        result.["calendar.txt"] <-
            table [| "service_id"; "monday"; "tuesday"; "wednesday"; "thursday"; "friday"; "saturday"; "sunday"; "start_date"; "end_date" |]
                (fun () ->
                    cells serving "service_calendar" [| "service_id"; "weekday_mask"; "valid_from"; "valid_to" |]
                    |> Seq.map (fun row ->
                        let mask = Int32.Parse(row.[1], CultureInfo.InvariantCulture)
                        Array.concat [| [| row.[0] |]; Array.init 7 (fun day -> if mask &&& (1 <<< day) <> 0 then "1" else "0"); [| row.[2]; row.[3] |] |]))
        result.["calendar_dates.txt"] <-
            table [| "service_id"; "date"; "exception_type" |]
                (fun () ->
                    cells serving "service_exception" [| "service_id"; "service_date"; "added" |]
                    |> Seq.map (fun row -> [| row.[0]; row.[1]; (if row.[2] = "1" then "1" else "2") |]))
        if nonEmpty serving "shape_point" then
            result.["shapes.txt"] <-
                table [| "shape_id"; "shape_pt_lat"; "shape_pt_lon"; "shape_pt_sequence"; "shape_dist_traveled" |]
                    (fun () -> cells serving "shape_point" [| "shape_id"; "latitude"; "longitude"; "sequence"; "distance_traveled" |])
        if nonEmpty serving "transfer" then
            result.["transfers.txt"] <-
                table [| "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id"; "transfer_type"; "min_transfer_time" |]
                    (fun () -> cells serving "transfer" [| "from_location_id"; "to_location_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id"; "transfer_type"; "minimum_transfer_time" |])
        feedInfo |> Option.iter (fun info -> result.["feed_info.txt"] <- info)
        result

    /// Write gtfs.zip: flat, ordinal entry order, fixed timestamps.
    let write (serving: string) (feedInfo: CompilerOutput.Table option) (path: string) =
        let tables = tables serving feedInfo
        use stream = File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
        use archive = new ZipArchive(stream, ZipArchiveMode.Create, false, Encoding.UTF8)
        for name in tables.Keys |> Seq.sortWith (fun left right -> String.CompareOrdinal(left, right)) do
            // Fastest compression materially reduces package wall/CPU time;
            // contents and deterministic entry metadata are unaffected.
            let entry = archive.CreateEntry(name, CompressionLevel.Fastest)
            entry.LastWriteTime <- DateTimeOffset(1980, 1, 1, 0, 0, 0, TimeSpan.Zero)
            use target = entry.Open()
            use writer = new StreamWriter(target, UTF8Encoding(false))
            writer.NewLine <- "\n"
            CompilerOutput.writeCsv writer tables.[name]
