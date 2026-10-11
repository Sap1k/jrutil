// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading

/// The optional route-shapes package phase (Oběhy BASE_PLAN.md section 17).
/// Trips still without a shape are reduced to distinct match-point patterns,
/// map-matched by the route-shapes companion (`obehy-motis-shapes`, osr), and
/// the shapes that pass validation are written into the serving relations
/// before gtfs.zip is projected. Source shapes are never replaced, and a
/// pattern that cannot be shaped leaves its trips unshaped: the phase never
/// fails a build over a trip, nor over the companion itself.
module RouteShapes =
    let private relation name = Schema.relations |> Array.find (fun value -> value.name = name)
    let private fieldIndex (schema: Schema.Relation) name = schema.fields |> Array.findIndex (fun value -> value.name = name)
    let private invariant = CultureInfo.InvariantCulture
    let private coordinate (value: double) = value.ToString("0.0000000", invariant)
    let private rowGroupRows = 8192

    /// The companion's routing class of a route mode (its osr profile); None:
    /// no profile, the route's trips stay unshaped.
    let shapeClass mode =
        match mode with
        | "bus" | "trolleybus" -> Some "bus"
        | "tram" | "metro" | "rail" -> Some "rail"
        | "water" -> Some "ferry"
        | _ -> None

    type MatchPoint = { latitude: double; longitude: double; waypoint: bool }

    type Pattern = { id: string; shapeClass: string; points: MatchPoint array }

    /// A trip's pattern and, per match point, the trip_call sequence it stands for.
    type TripPattern = { pattern: string; callSequences: int array }

    type Scan = {
        patterns: SortedDictionary<string, Pattern>
        trips: Dictionary<string, TripPattern>
        tripCount: int
        alreadyShaped: int
        unsupportedMode: SortedDictionary<string, int>
        /// Passenger calls left out as match points: no exact coordinate.
        estimatedCalls: int
    }

    [<Struct>]
    type private Call = { sequence: int; location: int; boarding: int; passenger: bool }

    let private patternKey (shapeClass: string) (points: MatchPoint array) =
        let builder = StringBuilder(shapeClass)
        for point in points do
            builder.Append('\n').Append(coordinate point.latitude).Append(',')
                .Append(coordinate point.longitude).Append(',').Append(if point.waypoint then '1' else '0') |> ignore
        builder.ToString()

    /// Every trip without a shape in a `serving` directory, reduced to its
    /// routing class and ordered match points: calls with exact coordinates
    /// (boarding point first), railway points among them as waypoints. Calls
    /// with estimated or missing coordinates are left out and get no offset.
    let scan (serving: string) =
        let read name = ColumnReader.groups (Path.Combine(serving, name + ".parquet")) (relation name)

        let routeSchema = relation "route"
        let modes = Dictionary<string, string>(StringComparer.Ordinal)
        for columns in read "route" do
            let ids, values = ColumnReader.text columns.[fieldIndex routeSchema "route_id"], ColumnReader.text columns.[fieldIndex routeSchema "mode"]
            for row in 0 .. ids.Length - 1 do modes.[ids.[row]] <- values.[row]

        let locationSchema = relation "location"
        let locationIndex = Dictionary<string, int>(StringComparer.Ordinal)
        let positions = ResizeArray<struct(double * double)>()  // NaN: not exact
        for columns in read "location" do
            let ids = ColumnReader.text columns.[fieldIndex locationSchema "location_id"]
            let precision = ColumnReader.text columns.[fieldIndex locationSchema "coordinate_precision"]
            let optional name =
                match columns.[fieldIndex locationSchema name] with
                | ColumnWriter.OptionalFloat64 values -> values
                | _ -> invalidOp $"Expected location.{name}"
            let latitudes, longitudes = optional "latitude", optional "longitude"
            for row in 0 .. ids.Length - 1 do
                locationIndex.[ids.[row]] <- positions.Count
                positions.Add(
                    if precision.[row] = "exact" && latitudes.[row].HasValue && longitudes.[row].HasValue
                    then struct(latitudes.[row].Value, longitudes.[row].Value)
                    else struct(nan, nan))

        let tripSchema = relation "trip"
        let classes = Dictionary<string, string>(StringComparer.Ordinal)
        let unsupported = SortedDictionary<string, int>(StringComparer.Ordinal)
        let mutable tripCount, alreadyShaped = 0, 0
        for columns in read "trip" do
            let ids = ColumnReader.text columns.[fieldIndex tripSchema "trip_id"]
            let routes = ColumnReader.text columns.[fieldIndex tripSchema "route_id"]
            let shapes = ColumnReader.text columns.[fieldIndex tripSchema "shape_id"]
            for row in 0 .. ids.Length - 1 do
                tripCount <- tripCount + 1
                if not (isNull shapes.[row]) then alreadyShaped <- alreadyShaped + 1
                else
                    let mode = modes.[routes.[row]]
                    match shapeClass mode with
                    | Some value -> classes.[ids.[row]] <- value
                    | None -> unsupported.[mode] <- (match unsupported.TryGetValue(mode) with | true, n -> n + 1 | _ -> 1)

        let callSchema = relation "trip_call"
        let calls = Dictionary<string, ResizeArray<Call>>(StringComparer.Ordinal)
        for columns in read "trip_call" do
            let trips = ColumnReader.text columns.[fieldIndex callSchema "trip_id"]
            let sequences = ColumnReader.int32 columns.[fieldIndex callSchema "sequence"]
            let locations = ColumnReader.text columns.[fieldIndex callSchema "location_id"]
            let boardings = ColumnReader.text columns.[fieldIndex callSchema "boarding_point_id"]
            let passenger =
                match columns.[fieldIndex callSchema "passenger_service"] with
                | ColumnWriter.Boolean values -> values
                | _ -> invalidOp "Expected trip_call.passenger_service"
            for row in 0 .. trips.Length - 1 do
                if classes.ContainsKey(trips.[row]) then
                    let list =
                        match calls.TryGetValue(trips.[row]) with
                        | true, list -> list
                        | _ -> let list = ResizeArray<Call>() in calls.Add(trips.[row], list); list
                    list.Add({ sequence = sequences.[row]
                               location = locationIndex.[locations.[row]]
                               boarding = if isNull boardings.[row] then -1 else locationIndex.[boardings.[row]]
                               passenger = passenger.[row] })

        let patterns = SortedDictionary<string, Pattern>(StringComparer.Ordinal)
        let tripPatterns = Dictionary<string, TripPattern>(StringComparer.Ordinal)
        let mutable estimated = 0
        let exact index =
            index >= 0 && (let struct(lat, _) = positions.[index] in not (Double.IsNaN lat))
        for KeyValue(trip, shapeClass) in classes |> Seq.sortWith (fun a b -> String.CompareOrdinal(a.Key, b.Key)) do
            let ordered = match calls.TryGetValue(trip) with | true, list -> list.ToArray() | _ -> [||]
            Array.sortInPlaceBy (fun (call: Call) -> call.sequence) ordered
            let points = ResizeArray<MatchPoint>()
            let sequences = ResizeArray<int>()
            for call in ordered do
                let position =
                    if exact call.boarding then Some call.boarding
                    elif exact call.location then Some call.location
                    else None
                match position with
                | Some index ->
                    let struct(lat, lon) = positions.[index]
                    points.Add({ latitude = Math.Round(lat, 7); longitude = Math.Round(lon, 7); waypoint = not call.passenger })
                    sequences.Add(call.sequence)
                | None -> if call.passenger then estimated <- estimated + 1
            let points = points.ToArray()
            let id = (Identity.sha256 (patternKey shapeClass points)).Substring(0, 20)
            if not (patterns.ContainsKey(id)) then
                patterns.Add(id, { id = id; shapeClass = shapeClass; points = points })
            tripPatterns.Add(trip, { pattern = id; callSequences = sequences.ToArray() })
        { patterns = patterns; trips = tripPatterns; tripCount = tripCount; alreadyShaped = alreadyShaped
          unsupportedMode = unsupported; estimatedCalls = estimated }

    let private counts (scan: Scan) =
        let excluded = JsonObject()
        let modes = JsonObject()
        for KeyValue(mode, count) in scan.unsupportedMode do modes.[mode] <- JsonValue.Create(count)
        excluded.["unsupported_mode"] <- modes
        let result = JsonObject()
        result.["trips"] <- JsonValue.Create(scan.tripCount)
        result.["already_shaped"] <- JsonValue.Create(scan.alreadyShaped)
        result.["routed_trips"] <- JsonValue.Create(scan.trips.Count)
        result.["patterns"] <- JsonValue.Create(scan.patterns.Count)
        result.["not_routed"] <- excluded
        result.["estimated_calls_skipped"] <- JsonValue.Create(scan.estimatedCalls)
        result

    /// patterns.csv (`pattern_id,class,seq,lat,lon,waypoint`, by pattern id):
    /// the companion's input.
    let writePatternsCsv (scan: Scan) (path: string) =
        use writer = new StreamWriter(path, false, UTF8Encoding(false))
        writer.NewLine <- "\n"
        writer.WriteLine("pattern_id,class,seq,lat,lon,waypoint")
        for pattern in scan.patterns.Values do
            pattern.points |> Array.iteri (fun index point ->
                writer.WriteLine(
                    $"{pattern.id},{pattern.shapeClass},{index},{coordinate point.latitude},{coordinate point.longitude},{(if point.waypoint then 1 else 0)}"))

    /// `shape-patterns`: the patterns of a finished package without routing
    /// them (pattern counts, companion benchmarks): patterns.csv and
    /// patterns-summary.json into `output`.
    let writePatterns (package: string) (output: string) =
        let scan = scan (Path.Combine(package, "serving"))
        Directory.CreateDirectory(output) |> ignore
        writePatternsCsv scan (Path.Combine(output, "patterns.csv"))
        File.WriteAllText(Path.Combine(output, "patterns-summary.json"),
                          (counts scan).ToJsonString(JsonSerializerOptions(WriteIndented = true)) + "\n", UTF8Encoding(false))
        scan

    type private Routed = {
        status: string
        points: ResizeArray<struct(double * double * double)>  // lat, lon, metres
        offsets: ResizeArray<struct(int * double)>             // point index, metres
    }

    /// The companion's output directory (route-shapes/README.md).
    let private readCompanion (directory: string) =
        let rows name = File.ReadLines(Path.Combine(directory, name)) |> Seq.skip 1 |> Seq.filter ((<>) "") |> Seq.map (fun line -> line.Split(','))
        let number (value: string) = Double.Parse(value, NumberStyles.Float, invariant)
        let routed = Dictionary<string, Routed>(StringComparer.Ordinal)
        for f in rows "pattern_status.csv" do
            routed.[f.[0]] <- { status = f.[2]; points = ResizeArray(); offsets = ResizeArray() }
        for f in rows "shapes.csv" do
            routed.[f.[0]].points.Add(struct(number f.[2], number f.[3], number f.[4]))
        for f in rows "offsets.csv" do
            routed.[f.[0]].offsets.Add(struct(int f.[2], number f.[3]))
        routed


    let private replace (path: string) (write: string -> unit) =
        let temporary = path + ".rewrite"
        if File.Exists(temporary) then File.Delete(temporary)
        write temporary
        File.Move(temporary, path, true)

    let private rebatch (output: ColumnWriter.Writer) rows (columns: ColumnWriter.Column array) batch =
        for start in 0 .. batch .. rows - 1 do
            output.Append(ColumnReader.slice start (min batch (rows - start)) columns)

    /// Run `obehy-motis-shapes route`; false (logged) when it fails.
    let private runCompanion (options: JrUtil.RouteShapeOptions) (patterns: string) (output: string) =
        let start = ProcessStartInfo(options.companion, UseShellExecute = false, RedirectStandardError = true, RedirectStandardOutput = true)
        for argument in [ "route"; "--graph"; options.graph; "--patterns"; patterns; "--out"; output ] do
            start.ArgumentList.Add(argument)
        options.cache |> Option.iter (fun cache -> start.ArgumentList.Add("--cache"); start.ArgumentList.Add(cache))
        try
            use child = Process.Start(start)
            let log (line: DataReceivedEventArgs) =
                if not (isNull line.Data) then Serilog.Log.Information("route-shapes: {Line}", line.Data)
            child.OutputDataReceived.Add(log)
            child.ErrorDataReceived.Add(log)
            child.BeginOutputReadLine()
            child.BeginErrorReadLine()
            child.WaitForExit()
            if child.ExitCode <> 0 then
                Serilog.Log.Warning("Route-shapes companion exited with {Code}; no shapes generated", child.ExitCode)
            child.ExitCode = 0
        with error ->
            Serilog.Log.Warning(error, "Route-shapes companion could not run; no shapes generated")
            false

    type Summary = {
        /// The `route_shapes` section of diagnostics.json.
        diagnostics: JsonObject
        /// Rows appended to shape and shape_point.
        shapeRows: int
        pointRows: int
    }

    let private writeShapes serving (shapeIds: ResizeArray<string>) (shapePoints: ResizeArray<ResizeArray<struct(double * double * double)>>) =
        let token = CancellationToken.None
        // New shapes after the existing ones, in shape id order.
        let order = [| 0 .. shapeIds.Count - 1 |] |> Array.sortWith (fun a b -> String.CompareOrdinal(shapeIds.[a], shapeIds.[b]))
        let shapeSchema, pointSchema = relation "shape", relation "shape_point"
        let shapePath, pointPath = Path.Combine(serving, "shape.parquet"), Path.Combine(serving, "shape_point.parquet")
        replace shapePath (fun target ->
            use output = new ColumnWriter.Writer(target, shapeSchema, rowGroupRows, token)
            for columns in ColumnReader.groups shapePath shapeSchema do
                rebatch output (ColumnWriter.rowCount columns.[0]) columns rowGroupRows
            for start in 0 .. rowGroupRows .. order.Length - 1 do
                let ids = Array.sub order start (min rowGroupRows (order.Length - start)) |> Array.map (fun index -> shapeIds.[index])
                output.Append [| ColumnWriter.Text ids; ColumnWriter.Text(Array.create ids.Length "motis") |])
        let pointBatch = 16384
        replace pointPath (fun target ->
            use output = new ColumnWriter.Writer(target, pointSchema, pointBatch, token)
            for columns in ColumnReader.groups pointPath pointSchema do
                rebatch output (ColumnWriter.rowCount columns.[0]) columns pointBatch
            let buffer = ResizeArray<struct(string * int * double * double * double)>(pointBatch)
            let flush () =
                if buffer.Count > 0 then
                    let rows = buffer.ToArray()
                    output.Append [|
                        ColumnWriter.Text(rows |> Array.map (fun struct(id, _, _, _, _) -> id))
                        ColumnWriter.Int32(rows |> Array.map (fun struct(_, sequence, _, _, _) -> sequence))
                        ColumnWriter.Float64(rows |> Array.map (fun struct(_, _, _, lon, _) -> lon))
                        ColumnWriter.Float64(rows |> Array.map (fun struct(_, _, lat, _, _) -> lat))
                        ColumnWriter.OptionalFloat64(rows |> Array.map (fun struct(_, _, _, _, metres) -> Nullable metres)) |]
                    buffer.Clear()
            for index in order do
                shapePoints.[index] |> Seq.iteri (fun sequence (struct(lat, lon, metres)) ->
                    buffer.Add(struct(shapeIds.[index], sequence, lat, lon, metres))
                    if buffer.Count = pointBatch then flush ())
            flush ())

    /// Set trip.shape_id and trip_call.shape_distance_traveled of the trips in
    /// `assigned` (trip → shape id, offset in metres per match point).
    let private assignTrips serving (scan: Scan) (assigned: string -> (string * double array) option) =
        let token = CancellationToken.None
        let tripSchema = relation "trip"
        let tripPath = Path.Combine(serving, "trip.parquet")
        let shapeField = fieldIndex tripSchema "shape_id"
        replace tripPath (fun target ->
            use output = new ColumnWriter.Writer(target, tripSchema, rowGroupRows, token)
            for columns in ColumnReader.groups tripPath tripSchema do
                let trips = ColumnReader.text columns.[fieldIndex tripSchema "trip_id"]
                let shapes = Array.copy (ColumnReader.text columns.[shapeField])
                for row in 0 .. trips.Length - 1 do
                    assigned trips.[row] |> Option.iter (fun (shape, _) -> shapes.[row] <- shape)
                let copy = Array.copy columns
                copy.[shapeField] <- ColumnWriter.Text shapes
                rebatch output trips.Length copy rowGroupRows)
        let callSchema = relation "trip_call"
        let callPath = Path.Combine(serving, "trip_call.parquet")
        let distanceField = fieldIndex callSchema "shape_distance_traveled"
        replace callPath (fun target ->
            use output = new ColumnWriter.Writer(target, callSchema, rowGroupRows, token, 64L * 1024L * 1024L)
            for columns in ColumnReader.groups callPath callSchema do
                let trips = ColumnReader.text columns.[fieldIndex callSchema "trip_id"]
                let sequences = ColumnReader.int32 columns.[fieldIndex callSchema "sequence"]
                let distances =
                    match columns.[distanceField] with
                    | ColumnWriter.OptionalFloat64 values -> Array.copy values
                    | _ -> invalidOp "Expected trip_call.shape_distance_traveled"
                for row in 0 .. trips.Length - 1 do
                    match assigned trips.[row] with
                    | Some (_, offsets) ->
                        // A call that is no match point (a railway point without
                        // coordinates) has no position on the shape.
                        let k = Array.IndexOf(scan.trips.[trips.[row]].callSequences, sequences.[row])
                        distances.[row] <- if k >= 0 then Nullable offsets.[k] else Nullable()
                    | None -> ()
                let copy = Array.copy columns
                copy.[distanceField] <- ColumnWriter.OptionalFloat64 distances
                rebatch output trips.Length copy rowGroupRows)

    /// The route-shapes phase over a package's `serving` directory, after
    /// trip_call is final and before gtfs.zip is projected. Generated shapes are
    /// `<feed>:shape:motis:<hash>` with distances in metres.
    let write (serving: string) (feed: string) (options: JrUtil.RouteShapeOptions) =
        let scan = scan serving
        let work = Path.Combine(Path.GetTempPath(), "jrutil-route-shapes-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(work) |> ignore
        try
            let patternsPath, companionOutput = Path.Combine(work, "patterns.csv"), Path.Combine(work, "out")
            writePatternsCsv scan patternsPath
            // A failed companion leaves every trip unshaped; it never fails the package.
            let ran = scan.patterns.Count = 0 || runCompanion options patternsPath companionOutput
            let routed = if ran && scan.patterns.Count > 0 then readCompanion companionOutput else Dictionary()
            let unshaped = SortedDictionary<string, int>(StringComparer.Ordinal)
            // Accepted pattern → shape index; shapes deduplicated by class and geometry.
            let shapeOf = Dictionary<string, int>(StringComparer.Ordinal)
            let shapeIds = ResizeArray<string>()
            let shapeIndex = Dictionary<string, int>(StringComparer.Ordinal)
            let shapePoints = ResizeArray<ResizeArray<struct(double * double * double)>>()
            let offsetsOf = Dictionary<string, double array>(StringComparer.Ordinal)
            // Best effort: every shape the companion wrote is taken, partly beelined or not.
            for pattern in scan.patterns.Values do
                match routed.TryGetValue(pattern.id) with
                | true, r when r.points.Count > 0 ->
                    let geometry =
                        let builder = StringBuilder(pattern.shapeClass)
                        for struct(lat, lon, _) in r.points do
                            builder.Append('\n').Append(coordinate lat).Append(',').Append(coordinate lon) |> ignore
                        builder.ToString()
                    let id = feed + ":shape:motis:" + (Identity.sha256 geometry).Substring(0, 20)
                    let index =
                        match shapeIndex.TryGetValue(id) with
                        | true, index -> index
                        | _ ->
                            shapeIndex.Add(id, shapeIds.Count)
                            shapeIds.Add(id)
                            shapePoints.Add(r.points)
                            shapeIds.Count - 1
                    shapeOf.Add(pattern.id, index)
                    offsetsOf.Add(pattern.id, r.offsets |> Seq.map (fun struct(_, metres) -> metres) |> Seq.toArray)
                | found, r ->
                    let reason = if not ran then "companion_failed" elif found then r.status else "missing"
                    unshaped.[reason] <- (match unshaped.TryGetValue(reason) with | true, n -> n + 1 | _ -> 1)

            let assigned (trip: string) =
                match scan.trips.TryGetValue(trip) with
                | true, tp when shapeOf.ContainsKey(tp.pattern) -> Some (shapeIds.[shapeOf.[tp.pattern]], offsetsOf.[tp.pattern])
                | _ -> None
            let shapedTrips = scan.trips.Values |> Seq.filter (fun tp -> shapeOf.ContainsKey(tp.pattern)) |> Seq.length
            if shapeIds.Count > 0 then
                writeShapes serving shapeIds shapePoints
                assignTrips serving scan assigned

            let section = counts scan
            section.["shaped_patterns"] <- JsonValue.Create(shapeOf.Count)
            section.["shapes"] <- JsonValue.Create(shapeIds.Count)
            section.["shaped_trips"] <- JsonValue.Create(shapedTrips)
            let reasons = JsonObject()
            for KeyValue(reason, count) in unshaped do reasons.[reason] <- JsonValue.Create(count)
            section.["unshaped_patterns"] <- reasons
            let companion = Path.Combine(companionOutput, "manifest.json")
            if File.Exists(companion) then section.["companion"] <- JsonNode.Parse(File.ReadAllText(companion))
            Serilog.Log.Information(
                "Route shapes: patterns={Patterns}; shaped={Shaped}; shapes={Shapes}; shaped_trips={Trips}",
                scan.patterns.Count, shapeOf.Count, shapeIds.Count, shapedTrips)
            { diagnostics = section; shapeRows = shapeIds.Count; pointRows = shapePoints |> Seq.sumBy (fun points -> points.Count) }
        finally
            if Directory.Exists(work) then Directory.Delete(work, true)
