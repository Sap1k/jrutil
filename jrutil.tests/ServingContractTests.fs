namespace JrUtil.Tests

open System
open System.IO
open System.IO.Compression
open System.Text
open System.Text.Json
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parquet

[<TestClass>]
type ServingContractTests() =
    let write (path: string) (text: string) =
        Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
        File.WriteAllText(path, text.Replace("\r\n", "\n"), new UTF8Encoding(false))

    // Fixture identifiers carry the JDF feed prefix; `ids-jmk-gtfs` is a
    // regional source with declared key namespaces.
    let route, trip, first, second = "jdf:route:000001", "jdf:trip:out", "jdf:stop:1", "jdf:stop:2"

    let staging root =
        let stage = Path.Combine(root, "stage")
        let gtfs, ext, mappings = Path.Combine(stage, "gtfs-intermediate"), Path.Combine(stage, "extensions"), Path.Combine(stage, "mappings")
        write (Path.Combine(stage, "manifest.json")) """{"publishable":true,"calibration":false,"source":{"source_id":"ids-jmk-gtfs","payload_sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","retrieved_at":"2026-09-01T00:00:00+02:00"}}"""
        write (Path.Combine(gtfs, "agency.txt")) "agency_id,agency_name,agency_url,agency_timezone\njdf:agency:a,Agency,https://example.test,Europe/Prague\n"
        write (Path.Combine(gtfs, "stops.txt")) "stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station,wheelchair_boarding\njdf:stop:1,One,50,14,0,,0\njdf:stop:2,Two,50.1,14.1,0,,0\n"
        write (Path.Combine(gtfs, "routes.txt")) "route_id,agency_id,route_short_name,route_long_name,route_type\njdf:route:000001,jdf:agency:a,1,Route,3\n"
        write (Path.Combine(gtfs, "trips.txt")) "route_id,service_id,trip_id,trip_headsign,direction_id,wheelchair_accessible,bikes_allowed\njdf:route:000001,jdf:service:svc,jdf:trip:out,Two,0,2,2\n"
        write (Path.Combine(gtfs, "stop_times.txt")) "trip_id,arrival_time,departure_time,stop_id,stop_sequence,pickup_type,drop_off_type,timepoint\njdf:trip:out,25:00:00,25:00:00,jdf:stop:1,1,0,0,1\njdf:trip:out,25:10:00,25:10:00,jdf:stop:2,2,0,0,1\n"
        write (Path.Combine(gtfs, "calendar_dates.txt")) "service_id,date,exception_type\njdf:service:svc,20260913,1\n"
        write (Path.Combine(gtfs, "transfers.txt")) "from_stop_id,to_stop_id,transfer_type,min_transfer_time,max_waiting_time\njdf:stop:1,jdf:stop:2,2,60,300\n"
        write (Path.Combine(ext, "cz_routes.txt")) "route_id,cis_line_id,public_line_number,source_provenance\njdf:route:000001,000001,1,ids-jmk-gtfs\n"
        write (Path.Combine(ext, "cz_trips.txt")) "trip_id,cis_line_id,cis_trip_id,train_number,source_trip_ids,coverage_sources\njdf:trip:out,000001,7,,,ids-jmk-gtfs\n"
        write (Path.Combine(ext, "cz_stops.txt")) "stop_id,stop_place_id,cis_stop_id,post_id,asw_id,source_ids\njdf:stop:1,jdf:stop:1,,,,\njdf:stop:2,jdf:stop:2,,,,\n"
        write (Path.Combine(ext, "cz_stop_zones.txt")) "stop_place_id,zone_id,zone_code,route_id,ids_system_id,source_provenance\n"
        write (Path.Combine(ext, "cz_trip_stop_zones.txt")) "trip_id,stop_sequence,zone_id,zone_code,ids_system_id,source_provenance\n"
        write (Path.Combine(mappings, "source_to_output_routes.csv")) "source_id,source_route_id,output_route_id,method\nids-jmk-gtfs,source-route,jdf:route:000001,exact\n"
        write (Path.Combine(mappings, "source_to_output_stops.csv")) "source_id,source_stop_id,output_stop_id,target_stop_place_id,method\nids-jmk-gtfs,A,jdf:stop:1,jdf:stop:1,exact\nids-jmk-gtfs,B,jdf:stop:2,jdf:stop:2,exact\n"
        write (Path.Combine(mappings, "source_to_output_trips.csv")) "source_id,source_trip_id,base_trip_id,output_trip_id,valid_from,valid_to,method,pattern_edits,first_departure_delta_seconds,aggregate_time_delta_seconds,duration_delta_seconds,runner_up_margin\nids-jmk-gtfs,source-trip,,jdf:trip:out,20260913,20260913,structural_trip_evidence+full_signature,0,0,0,0,\n"
        write (Path.Combine(mappings, "source_to_output_calls.csv")) "source_id,source_trip_id,source_call_ordinal,source_stop_id,output_trip_id,output_call_ordinal,output_stop_id\nids-jmk-gtfs,source-trip,0,A,jdf:trip:out,1,jdf:stop:1\nids-jmk-gtfs,source-trip,10,B,jdf:trip:out,2,jdf:stop:2\n"
        write (Path.Combine(mappings, "operational_to_source_trips.csv")) "source_id,operational_line_id,operational_trip_id,source_trip_id,valid_from,valid_to\nids-jmk-gtfs,L/1,C 2,source-trip,20260913,20260913\n"
        stage

    let parquetStrings path column =
        use stream = File.OpenRead(path)
        let reader = ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let field = reader.Schema.DataFields |> Array.find (fun value -> value.Name = column)
            [| for index in 0 .. reader.RowGroupCount - 1 do
                   use group = reader.OpenRowGroupReader(index)
                   let values = Array.zeroCreate<string> (int group.RowCount)
                   group.ReadAsync(field, values.AsMemory(), Nullable(), Threading.CancellationToken.None).AsTask().GetAwaiter().GetResult()
                   yield! values |]
        finally reader.DisposeAsync().AsTask().GetAwaiter().GetResult()

    let textRelation name (columns: string array) : JrUtil.Serving.Schema.Relation = {
        name = name
        fields = columns |> Array.map (fun name -> { name = name; dataType = JrUtil.Serving.Schema.Text; nullable = false; enumeration = null })
        primaryKey = [| columns.[0] |]; foreignKeys = [||] }

    let writeTextParquet (path: string) (columns: string array) (rows: string array array) =
        let relation = textRelation (Path.GetFileNameWithoutExtension(path)) columns
        use writer = new JrUtil.Serving.ColumnWriter.Writer(path, relation, 1024, Threading.CancellationToken.None)
        writer.Append(columns |> Array.mapi (fun index _ -> JrUtil.Serving.ColumnWriter.Text(rows |> Array.map (fun row -> row.[index]))))

    let relationRows (output: string) name columns =
        JrUtil.Serving.PackageReader.readTextRows (Path.Combine(output, "serving", name + ".parquet")) columns |> Seq.toArray

    /// Replace a serving relation file and re-inventory it in the manifest.
    let replaceRelation (output: string) (name: string) (rows: int) =
        let path = Path.Combine(output, "serving", name + ".parquet")
        let manifestPath = Path.Combine(output, "manifest.json")
        let manifest = System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText(manifestPath))
        let entry = manifest.["files"].AsArray() |> Seq.find (fun entry -> entry.["path"].GetValue<string>() = "serving/" + name + ".parquet")
        let digest =
            use input = File.OpenRead(path)
            Security.Cryptography.SHA256.HashData(input) |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()
        entry.["sha256"] <- System.Text.Json.Nodes.JsonValue.Create(digest)
        entry.["size_bytes"] <- System.Text.Json.Nodes.JsonValue.Create(FileInfo(path).Length)
        let declaration = manifest.["relations"].AsArray() |> Seq.find (fun entry -> entry.["name"].GetValue<string>() = name)
        declaration.["row_count"] <- System.Text.Json.Nodes.JsonValue.Create(rows)
        File.WriteAllText(manifestPath, manifest.ToJsonString())

    [<TestMethod>]
    member _.``Memory reclaim gate does not repeat collections without further growth``() =
        let gib = 1024L * 1024L * 1024L
        // A compacting collection releases some memory but stays above the threshold.
        let mutable current = 0L
        let reclaim () = current <- current - gib / 4L
        let gate = JrUtil.MemoryReclaimGate(3L * gib, gib / 2L, reclaim, fun () -> current)
        let observe value =
            current <- value
            gate.Check()
        Assert.IsFalse(observe (2L * gib))                  // below threshold
        Assert.IsTrue(observe (4L * gib))                   // first crossing; baseline 3.75 GiB
        Assert.IsFalse(observe (4L * gib))                  // no growth: the old check reclaimed here every time
        Assert.IsFalse(observe (4L * gib + gib / 8L))       // growth under the minimum
        Assert.IsTrue(observe (4L * gib + gib / 2L))        // 4.5 >= 3.75 + 0.5
        Assert.IsFalse(observe (2L * gib))                  // back below threshold
        Assert.AreEqual(2, gate.Reclaims)

    [<TestMethod>]
    member _.``Serving modes follow basic and extended GTFS route types``() =
        let mode = JrUtil.Serving.PackageBaseRelations.servingMode
        Assert.AreEqual("bus", mode 202, "National coach")
        Assert.AreEqual("bus", mode 704)
        Assert.AreEqual("metro", mode 401)
        Assert.AreEqual("rail", mode 105, "Night train")
        Assert.AreEqual("rail", mode 106)
        Assert.AreEqual("tram", mode 900)
        Assert.AreEqual("trolleybus", mode 800)
        Assert.AreEqual("water", mode 1000)
        Assert.AreEqual("cable", mode 1701, "JDF cable car")

    [<TestMethod>]
    member _.``Composite public keys preserve identifier colons and escape delimiters``() =
        Assert.AreEqual(
            "jdf:route:000645:1/10%2Fwest",
            JrUtil.Serving.Identity.compositeKey [ "jdf:route:000645:1"; "10/west" ])

    [<TestMethod>]
    member _.``Serving versions are major.minor and consumers accept any minor``() =
        Assert.AreEqual(Some 5, JrUtil.Serving.Schema.schemaMajor "5.0")
        Assert.AreEqual(Some 5, JrUtil.Serving.Schema.schemaMajor "5.12")
        Assert.AreEqual(None, JrUtil.Serving.Schema.schemaMajor "5")
        Assert.AreEqual(None, JrUtil.Serving.Schema.schemaMajor "five.0")

    [<TestMethod>]
    member _.``Route stops are ordered slots with readable identifiers and call zones``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-readable-route-stop-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            write (Path.Combine(stage, "extensions", "cz_stop_zones.txt"))
                "stop_place_id,zone_id,zone_code,route_id,ids_system_id,source_provenance\njdf:stop:2,z,100,,ids-jmk,ids-jmk-gtfs\n"
            write (Path.Combine(stage, "extensions", "cz_trip_stop_zones.txt"))
                "trip_id,stop_sequence,zone_id,zone_code,ids_system_id,source_provenance\njdf:trip:out,1,,P,pid,pid-gtfs\njdf:trip:out,1,,0,pid,pid-gtfs\n"
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage output
            let serving name = Path.Combine(output, "serving", name + ".parquet")
            let routeStops = parquetStrings (serving "route_stop") "route_stop_id"
            let slot stop = $"{route}/0/{stop}/1"
            CollectionAssert.AreEqual([| slot first; slot second |], routeStops)
            let schema name = JrUtil.Serving.Schema.relations |> Array.find (fun relation -> relation.name = name)
            let sequences =
                JrUtil.Serving.ColumnReader.groups (serving "route_stop") (schema "route_stop")
                |> Seq.collect (fun columns -> JrUtil.Serving.ColumnReader.int32 columns.[3]) |> Seq.toArray
            CollectionAssert.AreEqual([| 1; 2 |], sequences)
            CollectionAssert.AreEqual(routeStops, parquetStrings (serving "trip_call") "route_stop_id")
            let rows name columns = relationRows output name columns |> Array.map (String.concat "|")
            // One trip agrees with itself, so every zone sits on its slot.
            CollectionAssert.AreEqual(
                [| $"{slot first}|0|P|pid"; $"{slot first}|1|0|pid"; $"{slot second}|0|100|ids-jmk" |],
                rows "route_stop_zone" [| "route_stop_id"; "source_order"; "zone_code"; "zone_system" |])
            Assert.AreEqual(0, (rows "call_zone" [| "trip_id" |]).Length)
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Zones fall back to calls where trips at a slot disagree``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-zone-fallback-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let gtfs = Path.Combine(stage, "gtfs-intermediate")
            write (Path.Combine(gtfs, "trips.txt"))
                $"route_id,service_id,trip_id,trip_headsign,direction_id,wheelchair_accessible,bikes_allowed\n{route},jdf:service:svc,jdf:trip:late,Two,0,2,2\n{route},jdf:service:svc,{trip},Two,0,2,2\n"
            write (Path.Combine(gtfs, "stop_times.txt"))
                ("trip_id,arrival_time,departure_time,stop_id,stop_sequence,pickup_type,drop_off_type,timepoint\n"
                 + $"jdf:trip:late,26:00:00,26:00:00,{first},1,0,0,1\njdf:trip:late,26:10:00,26:10:00,{second},2,0,0,1\n"
                 + $"{trip},25:00:00,25:00:00,{first},1,0,0,1\n{trip},25:10:00,25:10:00,{second},2,0,0,1\n")
            write (Path.Combine(stage, "extensions", "cz_trip_stop_zones.txt"))
                ("trip_id,stop_sequence,zone_id,zone_code,ids_system_id,source_provenance\n"
                 + $"{trip},1,,P,pid,x\njdf:trip:late,1,,0,pid,x\n{trip},2,,1,pid,x\njdf:trip:late,2,,1,pid,x\n")
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage output
            let rows name columns = relationRows output name columns |> Array.map (String.concat "|") |> Array.sort
            CollectionAssert.AreEqual([| $"{route}/0/{second}/1|1" |], rows "route_stop_zone" [| "route_stop_id"; "zone_code" |])
            CollectionAssert.AreEqual([| "jdf:trip:late|1|0"; $"{trip}|1|P" |], rows "call_zone" [| "trip_id"; "sequence"; "zone_code" |])
            // The overlay compiler view expands slot zones back onto every call.
            let _, extensions = JrUtil.Serving.PackageReader.prepareCompilerView output (Path.Combine(root, "scratch"))
            let view = File.ReadAllText(Path.Combine(extensions, "cz_trip_stop_zones.txt")).Replace("\"", "")
            for expected in [ $"{trip},2,,1,pid"; "jdf:trip:late,2,,1,pid"; $"{trip},1,,P,pid"; "jdf:trip:late,1,,0,pid" ] do
                StringAssert.Contains(view, expected)
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Trips without a direction get their own route stop order``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-null-direction-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let trips = Path.Combine(stage, "gtfs-intermediate", "trips.txt")
            write trips (File.ReadAllText(trips).Replace(",Two,0,", ",Two,,"))
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage output
            let rows = relationRows output "route_stop" [| "route_stop_id"; "direction"; "sequence" |]
            CollectionAssert.AreEqual([| $"{route}/-/{first}/1|||1"; $"{route}/-/{second}/1|||2" |], rows |> Array.map (fun row -> $"{row.[0]}|||{row.[1]}{row.[2]}"))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Bounded compression preserves deterministic trip call bytes``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-call-compression-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let write path buffered =
                use writer = new JrUtil.Serving.TripCallWriter.Writer(path, Threading.CancellationToken.None, bufferedOutput = buffered)
                for sequence in 1 .. 40000 do
                    writer.Append({ tripId = "trip"; sequence = sequence; locationId = "location"
                                    passengerService = true; boardingPointId = null; routeStopId = null
                                    arrival = Nullable sequence; departure = Nullable(sequence + 1)
                                    pickup = 0s; dropoff = 0s; timepoint = true; headsign = null; distance = Nullable()
                                    subsidiaryCode = null; subsidiaryName = null; activeLineCode = null })
                Assert.AreEqual(40000L, writer.Complete())
                Assert.AreEqual(0, writer.ActiveCompressionWorkers)
            let first, second = Path.Combine(root, "sync.parquet"), Path.Combine(root, "buffered.parquet")
            write first false
            write second true
            CollectionAssert.AreEqual(File.ReadAllBytes(first), File.ReadAllBytes(second))
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Hash dedup collapses duplicates, rejects conflicts and cleans up after spilling``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-hash-dedup-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let dedup budget token (rows: seq<struct(int * string)>) =
                JrUtil.Serving.HashDedup.dedup root budget token (fun _ _ -> ()) "fixture" (fun _ -> 16L)
                    (fun struct(key, _) -> string key)
                    (fun writer struct(key: int, value: string) -> writer.Write(key); writer.Write(value))
                    (fun reader -> let key = reader.ReadInt32() in struct(key, reader.ReadString())) rows
            let input = [| for key in 1000 .. -1 .. 0 do yield struct(key, string key); yield struct(key, string key) |]
            let expected = [| for key in 1000 .. -1 .. 0 -> struct(key, string key) |]
            // In memory the input order is kept; after a spill only the set is defined.
            CollectionAssert.AreEqual(expected, dedup 1_000_000L Threading.CancellationToken.None input |> Seq.toArray)
            let spilled = dedup 64L Threading.CancellationToken.None input |> Seq.toArray
            CollectionAssert.AreEquivalent(expected, spilled)
            CollectionAssert.AreEqual(spilled, dedup 64L Threading.CancellationToken.None input |> Seq.toArray)
            Assert.AreEqual(0, Directory.GetDirectories(root).Length)
            let conflicting = [| struct(1, "a"); struct(2, "b"); struct(1, "c") |]
            Assert.ThrowsExactly<InvalidOperationException>(fun () ->
                dedup 1_000_000L Threading.CancellationToken.None conflicting |> Seq.toArray |> ignore) |> ignore
            Assert.ThrowsExactly<InvalidOperationException>(fun () ->
                dedup 16L Threading.CancellationToken.None conflicting |> Seq.toArray |> ignore) |> ignore
            Assert.AreEqual(0, Directory.GetDirectories(root).Length)
            use cancellation = new Threading.CancellationTokenSource()
            let interrupted = seq { yield struct(3, "3"); cancellation.Cancel(); yield struct(2, "2") }
            Assert.ThrowsExactly<OperationCanceledException>(fun () -> dedup 16L cancellation.Token interrupted |> Seq.toArray |> ignore) |> ignore
            Assert.AreEqual(0, Directory.GetDirectories(root).Length)
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Native trips equal the GTFS projection``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-native-trips-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let expected = Path.Combine(root, "expected")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage expected
            let actual = Path.Combine(root, "trips.parquet")
            let value: JrUtil.GtfsModel.Trip = {
                id = trip; routeId = route; serviceId = "jdf:service:svc"; headsign = Some "Two"; shortName = None
                directionId = Some "0"; blockId = Some " "; shapeId = None
                wheelchairAccessible = Some "2"; bikesAllowed = Some JrUtil.GtfsModel.NoBicycles }
            Assert.AreEqual(1, JrUtil.Serving.TripWriter.write actual Threading.CancellationToken.None (fun _ _ -> ()) [value])
            let columns = JrUtil.Serving.Schema.relations |> Array.find (fun relation -> relation.name = "trip") |> fun relation -> relation.fields |> Array.map _.name
            let rows path = JrUtil.Serving.PackageReader.readTextRows path columns |> Seq.toArray
            Asserts.assertEqual (rows (Path.Combine(expected, "serving", "trip.parquet"))) (rows actual)
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Native trip calls equal the existing GTFS projection for every field``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-native-serving-calls-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let expected = Path.Combine(root, "expected")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage expected
            let actual = Path.Combine(root, "calls.parquet")
            do
                use writer = new JrUtil.Serving.TripCallWriter.Writer(actual, Threading.CancellationToken.None)
                for sequence, stop, seconds in [1, first, 90000L; 2, second, 90600L] do
                    let call: JrUtil.GtfsModel.StopTime = {
                        tripId = trip; stopSequence = sequence; stopId = stop
                        arrivalTime = Some (NodaTime.Period.FromSeconds(seconds))
                        departureTime = Some (NodaTime.Period.FromSeconds(seconds))
                        headsign = None; pickupType = Some JrUtil.GtfsModel.RegularlyScheduled
                        dropoffType = Some JrUtil.GtfsModel.RegularlyScheduled
                        timepoint = Some JrUtil.GtfsModel.Exact; shapeDistTraveled = None; stopZoneIds = None }
                    writer.Append(JrUtil.Serving.TripCallWriter.fromGtfs call stop null null)
                Assert.AreEqual(2L, writer.Complete())
            let fields =
                JrUtil.Serving.Schema.relations |> Array.find (fun relation -> relation.name = "trip_call")
                |> fun relation -> relation.fields |> Array.map _.name
            // route_stop_id is assigned by the package post-pass, not the call writer.
            let fields = fields |> Array.filter ((<>) "route_stop_id")
            let rows path = JrUtil.Serving.PackageReader.readTextRows path fields |> Seq.toArray
            Asserts.assertEqual (rows (Path.Combine(expected, "serving", "trip_call.parquet"))) (rows actual)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Typed sink rejects oversized batches before writing and supports cancellation``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-column-budget-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        let relation = textRelation "bounded_text" [| "id" |]
        let path = Path.Combine(root, "test.parquet")
        try
            use cancellation = new Threading.CancellationTokenSource()
            do
                use writer = new JrUtil.Serving.ColumnWriter.Writer(path, relation, 2, cancellation.Token, maximumBytes = 256L)
                Assert.ThrowsExactly<ArgumentException>(fun () ->
                    writer.Append [| JrUtil.Serving.ColumnWriter.Text [| String('x', 200) |] |]) |> ignore
                Assert.AreEqual(0L, writer.RowCount)
                Assert.ThrowsExactly<ArgumentException>(fun () ->
                    writer.Append [| JrUtil.Serving.ColumnWriter.Text [| "a"; "b"; "c" |] |]) |> ignore
                writer.Append [| JrUtil.Serving.ColumnWriter.Text [| "a"; "b" |] |]
                writer.Append [| JrUtil.Serving.ColumnWriter.Text [| "c" |] |]
                Assert.AreEqual(3L, writer.RowCount)
                cancellation.Cancel()
                Assert.ThrowsExactly<OperationCanceledException>(fun () ->
                    writer.Append [| JrUtil.Serving.ColumnWriter.Text [| "d" |] |]) |> ignore
            CollectionAssert.AreEqual([| "a"; "b"; "c" |], parquetStrings path "id")
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Validation rejects duplicate keys across row groups with matching hashes``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-invalid-serving-order-" + Guid.NewGuid().ToString("N"))
        try
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) (staging root) output
            let path = Path.Combine(output, "serving", "shape.parquet")
            File.Delete(path)
            let relation = JrUtil.Serving.Schema.relations |> Array.find (fun relation -> relation.name = "shape")
            do
                use writer = new JrUtil.Serving.ColumnWriter.Writer(path, relation, 1, Threading.CancellationToken.None)
                for key in ["jdf:shape:z"; "jdf:shape:z"] do
                    writer.Append [| JrUtil.Serving.ColumnWriter.Text [|key|]; JrUtil.Serving.ColumnWriter.Text [|"source"|] |]
            replaceRelation output "shape" 2
            let result = JrUtil.Serving.Validation.inspect output
            Assert.AreEqual(1, result.errors.Length, String.Join("; ", result.errors))
            Assert.IsTrue(result.errors.[0].Contains("duplicate primary keys"), result.errors.[0])
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Validation rejects coded values, unprefixed ids and malformed keys``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-invalid-serving-values-" + Guid.NewGuid().ToString("N"))
        try
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) (staging root) output
            let rewrite name (columns: JrUtil.Serving.ColumnWriter.Column array) =
                let path = Path.Combine(output, "serving", name + ".parquet")
                File.Delete(path)
                let relation = JrUtil.Serving.Schema.relations |> Array.find (fun relation -> relation.name = name)
                do
                    use writer = new JrUtil.Serving.ColumnWriter.Writer(path, relation, 16, Threading.CancellationToken.None)
                    writer.Append columns
                replaceRelation output name 1
            let text value = JrUtil.Serving.ColumnWriter.Text [| value |]
            // An unknown shape generation and a shape id without a feed prefix.
            rewrite "shape" [| text "shape-1"; text "drawn" |]
            // A CIS line-trip key without the line part.
            rewrite "source_key" [|
                text "trip"; text "cis:line_trip"; text "143"; text trip
                JrUtil.Serving.ColumnWriter.Date [| DateOnly(2026, 9, 13) |]; JrUtil.Serving.ColumnWriter.Date [| DateOnly(2026, 9, 13) |]
                text "identity" |]
            let errors = (JrUtil.Serving.Validation.inspect output).errors
            let has (text: string) = errors |> Array.exists (fun error -> error.Contains(text))
            Assert.IsTrue(has "generation_method outside shape_generation", String.Join("; ", errors))
            Assert.IsTrue(has "shape_id without a feed prefix", String.Join("; ", errors))
            Assert.IsTrue(has "identifier encoding", String.Join("; ", errors))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Validation requires gtfs.zip to project the relations and a source digest``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-invalid-serving-gtfs-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let manifest = Path.Combine(stage, "manifest.json")
            write manifest (File.ReadAllText(manifest).Replace(String('a', 64), String('0', 64)))
            let output = Path.Combine(root, "output")
            let error = Assert.ThrowsExactly<ArgumentException>(fun () -> StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage output)
            StringAssert.Contains(error.Message, "has no payload_sha256 digest")
            let valid = Path.Combine(root, "valid")
            write manifest (File.ReadAllText(manifest).Replace(String('0', 64), String('a', 64)))
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage valid
            // Rewrite gtfs.zip with a shifted departure.
            let zipPath = Path.Combine(valid, "gtfs.zip")
            let entries =
                use zip = ZipFile.OpenRead(zipPath)
                zip.Entries |> Seq.map (fun entry ->
                    use reader = new StreamReader(entry.Open())
                    entry.FullName, reader.ReadToEnd()) |> Seq.toArray
            File.Delete(zipPath)
            do
                use zip = ZipFile.Open(zipPath, ZipArchiveMode.Create)
                for name, content in entries do
                    let entry = zip.CreateEntry(name)
                    use writer = new StreamWriter(entry.Open())
                    writer.Write(if name = "stop_times.txt" then content.Replace("25:10:00", "25:11:00") else content)
            let manifestPath = Path.Combine(valid, "manifest.json")
            let node = System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText(manifestPath))
            let entry = node.["files"].AsArray() |> Seq.find (fun entry -> entry.["path"].GetValue<string>() = "gtfs.zip")
            entry.["sha256"] <- System.Text.Json.Nodes.JsonValue.Create(JrUtil.Hashing.sha256File zipPath)
            entry.["size_bytes"] <- System.Text.Json.Nodes.JsonValue.Create(FileInfo(zipPath).Length)
            File.WriteAllText(manifestPath, node.ToJsonString())
            let errors = (JrUtil.Serving.Validation.inspect valid).errors
            Assert.IsTrue(errors |> Array.exists (fun error -> error.Contains("stop_times.txt") && error.Contains("not the projection")), String.Join("; ", errors))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Production package has closed validated inventory and deterministic bytes``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-contract-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let first, second = Path.Combine(root, "first"), Path.Combine(root, "second")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage first
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage second
            let result = JrUtil.Serving.Validation.validatePackage first
            Assert.AreEqual(19, result.relationCount)
            Assert.AreEqual(22, result.fileCount)
            JrUtil.Serving.Validation.compareByteIdentical first second
            Assert.IsFalse(Directory.Exists(Path.Combine(first, "gtfs-intermediate")))
            Assert.IsFalse(Directory.Exists(Path.Combine(first, "extensions")))
            use zip = ZipFile.OpenRead(Path.Combine(first, "gtfs.zip"))
            use transfers = new StreamReader(zip.GetEntry("transfers.txt").Open())
            Assert.IsFalse(transfers.ReadLine().Contains("max_waiting_time"))
            let waits = relationRows first "transfer" [| "maximum_waiting_time" |]
            Assert.IsTrue(waits |> Array.exists (fun row -> row.[0] = "300"))
            use manifest = JsonDocument.Parse(File.ReadAllText(Path.Combine(first, "manifest.json")))
            Assert.AreEqual("5.0", manifest.RootElement.GetProperty("serving_schema_version").GetString())
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``GTFS calendars are the projection of the calendar relations``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-calendar-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let gtfs = Path.Combine(stage, "gtfs-intermediate")
            write (Path.Combine(gtfs, "calendar.txt")) "service_id,monday,tuesday,wednesday,thursday,friday,saturday,sunday,start_date,end_date\njdf:service:svc,1,0,1,0,1,0,0,20260907,20260927\n"
            write (Path.Combine(gtfs, "calendar_dates.txt")) "service_id,date,exception_type\njdf:service:svc,20260913,1\njdf:service:svc,20260914,2\n"
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage output
            // Bit 0 is Monday.
            CollectionAssert.AreEqual([| "jdf:service:svc|21" |], relationRows output "service_calendar" [| "service_id"; "weekday_mask" |] |> Array.map (String.concat "|"))
            use zip = ZipFile.OpenRead(Path.Combine(output, "gtfs.zip"))
            let read (name: string) =
                use reader = new StreamReader(zip.GetEntry(name).Open())
                reader.ReadToEnd().Replace("\"", "").Split('\n', StringSplitOptions.RemoveEmptyEntries)
            CollectionAssert.AreEqual(
                [| "service_id,monday,tuesday,wednesday,thursday,friday,saturday,sunday,start_date,end_date"
                   "jdf:service:svc,1,0,1,0,1,0,0,20260907,20260927" |], read "calendar.txt")
            CollectionAssert.AreEquivalent(
                [| "service_id,date,exception_type"; "jdf:service:svc,20260913,1"; "jdf:service:svc,20260914,2" |], read "calendar_dates.txt")
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Compiler view restores transfer waiting times from serving relations``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-view-" + Guid.NewGuid().ToString("N"))
        try
            let package = Path.Combine(root, "package")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) (staging root) package
            use zip = ZipFile.OpenRead(Path.Combine(package, "gtfs.zip"))
            use published = new StreamReader(zip.GetEntry("transfers.txt").Open())
            Assert.IsFalse(published.ReadToEnd().Contains("max_waiting_time"))
            let gtfs, _ = JrUtil.Serving.PackageReader.prepareCompilerView package (Path.Combine(root, "scratch"))
            let lines = File.ReadAllLines(Path.Combine(gtfs, "transfers.txt"))
            StringAssert.EndsWith(lines.[0], "max_waiting_time\"")
            StringAssert.EndsWith(lines.[1], "\"300\"")
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Semantic comparison ignores physical layout and reports keyed differences``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-compare-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let first, second, renamed = Path.Combine(root, "first"), Path.Combine(root, "second"), Path.Combine(root, "renamed")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage first
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage second
            let same = JrUtil.Serving.Comparison.comparePackages [] first second
            Assert.IsTrue(same.isEquivalent)
            Assert.AreEqual(0, same.differences.Length)
            Assert.IsTrue(same.compared |> List.contains "serving/trip_call.parquet")
            Assert.IsTrue(same.compared |> List.contains "gtfs.zip/stop_times.txt")

            let stops = Path.Combine(stage, "gtfs-intermediate", "stops.txt")
            File.WriteAllText(stops, File.ReadAllText(stops).Replace(",Two,", ",Deux,"))
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage renamed
            let changed = JrUtil.Serving.Comparison.comparePackages [] first renamed
            Assert.IsFalse(changed.isEquivalent)
            let subjects = changed.unexpected |> List.map (fun difference -> difference.kind, difference.subject)
            CollectionAssert.Contains(List.toArray subjects, ("relation", "location"))
            CollectionAssert.Contains(List.toArray subjects, ("gtfs", "stops.txt"))
            let location = changed.unexpected |> List.find (fun difference -> difference.subject = "location")
            Assert.IsTrue(location.summary.StartsWith("0 removed, 0 added, 1 changed"), location.summary)
            Assert.IsTrue(location.samples |> List.exists (fun sample -> sample.Contains("name: Two -> Deux")))

            let expectations =
                JrUtil.Serving.Comparison.parseExpectations [ "# renamed stop"; "relation location"; "gtfs stop*.txt"; "manifest *" ]
            let allowed = JrUtil.Serving.Comparison.comparePackages expectations first renamed
            Assert.IsTrue(allowed.unexpected |> List.forall (fun difference -> difference.subject <> "location" && difference.subject <> "stops.txt"))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Source keys use source namespaces and call keys only differing sequences``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-identity-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            // The second source call keeps its output sequence and needs no key.
            let calls = Path.Combine(stage, "mappings", "source_to_output_calls.csv")
            write calls (File.ReadAllText(calls).Replace("source-trip,10,B", "source-trip,2,B"))
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage output
            let keys = relationRows output "source_key" [| "entity_kind"; "namespace"; "identifier"; "public_id"; "binding_method" |] |> Array.map (String.concat "|")
            CollectionAssert.AreEquivalent(
                [| $"trip|ids-jmk:gtfs_trip_id|source-trip|{trip}|structural_match"
                   $"trip|ids-jmk:line_course|L%%2F1/C%%202|{trip}|operator_crosswalk"
                   $"trip|cis:line_trip|000001:7|{trip}|identity"
                   $"route|cis:line|000001|{route}|identity"
                   $"route|ids-jmk:gtfs_route_id|source-route|{route}|structural_match"
                   $"location|ids-jmk:gtfs_stop_id|A|{first}|structural_match"
                   $"location|ids-jmk:gtfs_stop_id|B|{second}|structural_match" |], keys)
            CollectionAssert.AreEqual(
                [| $"ids-jmk:gtfs_trip_id|source-trip|0|{trip}|1" |],
                relationRows output "call_key" [| "namespace"; "identifier"; "source_sequence"; "trip_id"; "sequence" |] |> Array.map (String.concat "|"))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Carried base call keys require exact target sequence membership``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-call-gap-" + Guid.NewGuid().ToString("N"))
        try
            let baseStage = staging (Path.Combine(root, "base-work"))
            let basePackage = Path.Combine(root, "base")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) baseStage basePackage
            let overlayStage = staging (Path.Combine(root, "overlay-work"))
            write (Path.Combine(overlayStage, "gtfs-intermediate", "stop_times.txt"))
                $"trip_id,arrival_time,departure_time,stop_id,stop_sequence,pickup_type,drop_off_type,timepoint\n{trip},25:00:00,25:00:00,{first},1,0,0,1\n{trip},25:10:00,25:10:00,{second},3,0,0,1\n"
            write (Path.Combine(overlayStage, "mappings", "source_to_output_calls.csv"))
                "source_id,source_trip_id,source_call_ordinal,source_stop_id,output_trip_id,output_call_ordinal,output_stop_id\n"
            write (Path.Combine(overlayStage, "mappings", "base_to_output_trips.csv"))
                $"base_trip_id,output_trip_id,valid_from,valid_to\n{trip},{trip},20260913,20260913\n"
            write (Path.Combine(overlayStage, "base-package.path")) basePackage
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) overlayStage output
            // Base call 2 has no output call; call 1 still exists.
            CollectionAssert.AreEqual([| "0" |], parquetStrings (Path.Combine(output, "serving", "call_key.parquet")) "source_sequence")
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Contract file declares every serving relation, enumeration and namespace``() =
        let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
        use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(root, "contracts", "serving-v5.json")))
        let contract = document.RootElement
        Assert.AreEqual(JrUtil.Serving.Schema.ServingSchemaVersion, contract.GetProperty("serving_schema_version").GetString())
        let strings (item: JsonElement) = item.EnumerateArray() |> Seq.map (fun value -> value.GetString()) |> Seq.toArray
        let typeName = function
            | JrUtil.Serving.Schema.Text -> "string" | JrUtil.Serving.Schema.Int16 -> "int16"
            | JrUtil.Serving.Schema.Int32 -> "int32" | JrUtil.Serving.Schema.Int64 -> "int64"
            | JrUtil.Serving.Schema.Float64 -> "double" | JrUtil.Serving.Schema.Boolean -> "bool"
            | JrUtil.Serving.Schema.Date -> "date32"
        let declared =
            contract.GetProperty("relations").EnumerateArray()
            |> Seq.map (fun item ->
                let fields =
                    item.GetProperty("fields").EnumerateArray()
                    |> Seq.map (fun field ->
                        let enumeration = match field.TryGetProperty("enum") with | true, value -> value.GetString() | _ -> ""
                        $"""{field.GetProperty("name").GetString()}:{field.GetProperty("type").GetString()}:{field.GetProperty("nullable").GetBoolean()}:{enumeration}""")
                let keys =
                    item.GetProperty("foreign_keys").EnumerateArray()
                    |> Seq.map (fun key -> $"""{String.Join(",", strings (key.GetProperty("fields")))}->{key.GetProperty("relation").GetString()}({String.Join(",", strings (key.GetProperty("target_fields")))})""")
                $"""{item.GetProperty("name").GetString()} [{String.Join("; ", fields)}] pk({String.Join(",", strings (item.GetProperty("primary_key")))}) fk[{String.Join("; ", keys)}]""")
            |> Seq.toArray
        let expected =
            JrUtil.Serving.Schema.relations
            |> Array.map (fun relation ->
                let fields = relation.fields |> Array.map (fun field -> $"""{field.name}:{typeName field.dataType}:{field.nullable}:{(if isNull field.enumeration then "" else field.enumeration)}""")
                let keys = relation.foreignKeys |> Array.map (fun key -> $"""{String.Join(",", key.fields)}->{key.relation}({String.Join(",", key.targetFields)})""")
                $"""{relation.name} [{String.Join("; ", fields)}] pk({String.Join(",", relation.primaryKey)}) fk[{String.Join("; ", keys)}]""")
        CollectionAssert.AreEqual(expected, declared)
        let enumerations =
            contract.GetProperty("enumerations").EnumerateObject()
            |> Seq.map (fun item -> item.Name + "=" + String.Join(",", item.Value.GetProperty("values").EnumerateObject() |> Seq.map _.Name))
            |> Seq.toArray
        CollectionAssert.AreEqual(
            JrUtil.Serving.Schema.enumerations |> Map.toArray |> Array.map (fun (name, values) -> name + "=" + String.Join(",", values)) |> Array.sort,
            enumerations |> Array.sort)
        for item in contract.GetProperty("namespaces").EnumerateObject() do
            let declared = JrUtil.Serving.Schema.namespaces |> Array.find (fun value -> value.name = item.Name)
            Assert.AreEqual(item.Value.GetProperty("entity_kind").GetString(), declared.entityKind, item.Name)
            Assert.IsTrue(Text.RegularExpressions.Regex.IsMatch(item.Value.GetProperty("example").GetString(), declared.pattern), item.Name)
        Assert.AreEqual(contract.GetProperty("namespaces").EnumerateObject() |> Seq.length, JrUtil.Serving.Schema.namespaces.Length)

    [<TestMethod>]
    member _.``CZPTT trip parts share boundary calls and carry railway points only on rail``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-trip-parts-" + Guid.NewGuid().ToString("N"))
        try
            let stage = Path.Combine(root, "stage")
            let gtfs, ext = Path.Combine(stage, "gtfs-intermediate"), Path.Combine(stage, "extensions")
            write (Path.Combine(stage, "manifest.json")) """{"source_format":"czptt","source":{"source_id":"national-czptt","payload_sha256":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"}}"""
            write (Path.Combine(gtfs, "agency.txt")) "agency_id,agency_name,agency_url,agency_timezone\nczptt:agency:a,Rail,https://example.test,Europe/Prague\n"
            write (Path.Combine(gtfs, "stops.txt"))
                ("stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station,platform_code\n"
                 + "czptt:stop:CZ:A,A,50,14,0,,1\nczptt:stop:CZ:B,B,50.1,14,0,,\nczptt:stop:CZ:C,C,50.2,14,0,,\nczptt:stop:CZ:D,D,50.3,14,0,,\n")
            write (Path.Combine(gtfs, "routes.txt")) "route_id,agency_id,route_short_name,route_type\nczptt:route:rail,czptt:agency:a,Os,2\nczptt:route:nad,czptt:agency:a,NAD,3\n"
            let part n = $"czptt:trip:Pa%%3A1:{n}"
            write (Path.Combine(gtfs, "trips.txt"))
                $"route_id,service_id,trip_id\nczptt:route:rail,czptt:service:s,{part 1}\nczptt:route:nad,czptt:service:s,{part 2}\nczptt:route:rail,czptt:service:s,{part 3}\n"
            write (Path.Combine(gtfs, "stop_times.txt"))
                ("trip_id,arrival_time,departure_time,stop_id,stop_sequence\n"
                 + $"{part 1},08:00:00,08:00:00,czptt:stop:CZ:A,2\n{part 1},08:10:00,08:12:00,czptt:stop:CZ:B,4\n"
                 + $"{part 2},08:10:00,08:12:00,czptt:stop:CZ:B,4\n{part 2},08:30:00,08:32:00,czptt:stop:CZ:C,6\n"
                 + $"{part 3},08:30:00,08:32:00,czptt:stop:CZ:C,6\n{part 3},08:50:00,08:50:00,czptt:stop:CZ:D,8\n")
            write (Path.Combine(gtfs, "calendar_dates.txt")) "service_id,date,exception_type\nczptt:service:s,20260913,1\n"
            write (Path.Combine(ext, "cz_trips.txt"))
                ("trip_id,cis_line_id,cis_trip_id,train_number,source_trip_ids,coverage_sources\n"
                 + String.Join("", [ for n in 1 .. 3 -> $"{part n},,,1234,PA=Pa:1|TR=Tr:1,czptt\n" ]))
            let point code passenger (seconds: int) = [| "Pa:1"; ""; "CZ:" + code; passenger; string seconds; string seconds; ""; ""; "" |]
            let calls =
                [| point "X0" "False" 28700; point "A" "True" 28800; point "X1" "False" 29100; point "B" "True" 29400
                   point "X2" "False" 30000; point "C" "True" 30600; point "X3" "False" 31000; point "D" "True" 31800
                   point "X4" "False" 32000 |]
                |> Array.mapi (fun index row -> row.[1] <- string (index + 1); row)
            writeTextParquet (Path.Combine(stage, "operational_calls.parquet"))
                [| "source_pa_id"; "source_sequence"; "source_location_id"; "passenger_call"; "arrival_seconds"; "departure_seconds"
                   "subsidiary_code"; "subsidiary_name"; "active_line_code" |] calls
            writeTextParquet (Path.Combine(stage, "operational_points.parquet"))
                [| "source_location_id"; "country_code"; "primary_code"; "source_name"; "latitude"; "longitude"; "coordinate_source" |]
                [| for code in [ "X0"; "A"; "X1"; "B"; "X2"; "C"; "X3"; "D"; "X4" ] -> [| "CZ:" + code; "CZ"; code; code; "50"; "14"; "sz_sr70" |] |]
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage output
            let calls = relationRows output "trip_call" [| "trip_id"; "sequence"; "passenger_service" |]
            let sequences n = calls |> Array.filter (fun row -> row.[0] = part n) |> Array.map (fun row -> int row.[1])
            CollectionAssert.AreEqual([| 1; 2; 3; 4 |], sequences 1, "Leading and interior points go on the first rail part")
            CollectionAssert.AreEqual([| 4; 6 |], sequences 2, "A rail-replacement bus carries passenger calls only")
            CollectionAssert.AreEqual([| 6; 7; 8; 9 |], sequences 3, "Interior and trailing points go on the last rail part")
            CollectionAssert.AreEquivalent(
                [| $"{part 1}|Pa:1|1"; $"{part 2}|Pa:1|2"; $"{part 3}|Pa:1|3" |],
                relationRows output "trip" [| "trip_id"; "run_key"; "run_part" |] |> Array.map (String.concat "|"))
            let kinds = relationRows output "location" [| "location_id"; "kind"; "coordinate_source"; "public_code" |] |> Array.map (String.concat "|")
            CollectionAssert.Contains(kinds, "czptt:stop:CZ:X2|operational_point|sr70|")
            CollectionAssert.Contains(kinds, "czptt:stop:CZ:A|stop_place|sr70|1")
            // Railway points have no route stop; GTFS keeps passenger calls only.
            Assert.IsTrue(relationRows output "trip_call" [| "passenger_service"; "route_stop_id"; "pickup_type" |]
                          |> Array.forall (fun row -> row.[0] = "True" || (row.[1] = "" && row.[2] = "1")))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Overlay compiler view is rebuilt from GTFS ZIP and serving identities``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-reader-" + Guid.NewGuid().ToString("N"))
        try
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) (staging root) output
            let gtfs, extensions = JrUtil.Serving.PackageReader.prepareCompilerView output (Path.Combine(root, "scratch"))
            Assert.IsTrue(File.ReadAllText(Path.Combine(gtfs, "trips.txt")).Contains(trip))
            Assert.IsTrue(File.ReadAllText(Path.Combine(extensions, "cz_routes.txt")).Contains("000001"))
            Assert.IsTrue(File.ReadAllText(Path.Combine(extensions, "cz_trips.txt")).Contains("\"7\""))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Overlay slicing carries regional base keys and calls``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-base-carry-" + Guid.NewGuid().ToString("N"))
        try
            let baseStage = staging (Path.Combine(root, "base-work"))
            let basePackage = Path.Combine(root, "base")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) baseStage basePackage

            let overlayStage = staging (Path.Combine(root, "overlay-work"))
            for name in [ "source_to_output_trips.csv"; "source_to_output_calls.csv"; "operational_to_source_trips.csv" ] do
                let path = Path.Combine(overlayStage, "mappings", name)
                write path (File.ReadAllLines(path).[0] + "\n")
            write (Path.Combine(overlayStage, "mappings", "base_to_output_trips.csv"))
                $"base_trip_id,output_trip_id,valid_from,valid_to\n{trip},{trip},20260913,20260913\n"
            write (Path.Combine(overlayStage, "base-package.path")) basePackage
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) overlayStage output

            let keys = relationRows output "source_key" [| "namespace"; "identifier"; "public_id" |] |> Array.map (String.concat "|")
            CollectionAssert.Contains(keys, $"ids-jmk:gtfs_trip_id|source-trip|{trip}")
            CollectionAssert.Contains(keys, $"ids-jmk:line_course|L%%2F1/C%%202|{trip}")
            let sequences = parquetStrings (Path.Combine(output, "serving", "call_key.parquet")) "source_sequence"
            CollectionAssert.AreEquivalent([| "0"; "10" |], sequences)
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Overlay projects native base semantics onto replacement trips``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-semantic-carry-" + Guid.NewGuid().ToString("N"))
        try
            let baseStage = staging (Path.Combine(root, "base-work"))
            writeTextParquet (Path.Combine(baseStage, "source_call_metadata.parquet"))
                [| "gtfs_trip_id"; "stop_sequence"; "source_route_stop_id"; "gtfs_stop_id" |]
                [| [| trip; "1"; "11"; first |]; [| trip; "2"; "12"; second |] |]
            writeTextParquet (Path.Combine(baseStage, "source_route_stop_zone_metadata.parquet"))
                [| "gtfs_route_id"; "source_route_stop_id"; "zone_id"; "zone_order" |]
                [| [| route; "11"; "zone"; "0" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_notice_metadata.parquet"))
                [| "source_notice_id"; "notice_kind"; "gtfs_route_id"; "gtfs_trip_id"; "label"; "text"; "valid_from"; "valid_to"; "service_note_type" |]
                [| [| "jdf:notice:1"; "service_note"; ""; trip; "Label"; "Text"; "20260913"; "20260913"; "" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_trip_feature_metadata.parquet"))
                [| "gtfs_trip_id"; "source_code"; "feature_kind"; "source_object_id" |]
                [| [| trip; "@"; "wheelchair_accessible_vehicle"; "feature-source" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_location_feature_metadata.parquet"))
                [| "gtfs_stop_id"; "source_code"; "feature_kind"; "source_object_id" |]
                [| [| first; "@"; "wheelchair_accessible"; "location-source" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_transfer_metadata.parquet"))
                [| "source_transfer_id"; "gtfs_trip_id"; "source_route_stop_id"; "transfer_type"; "transfer_route_id"; "transfer_stop_id"; "transfer_stop_post_id"; "transfer_end_stop_id"; "transfer_end_stop_post_id"; "wait_minutes"; "note" |]
                [| [| "jdf:transfer:1"; trip; "11"; "m"; ""; ""; ""; ""; ""; "5"; "Connection" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_travel_restriction_metadata.parquet"))
                [| "assignment_scope"; "gtfs_route_id"; "gtfs_trip_id"; "source_route_stop_id"; "group_code" |]
                [| [| "trip_call"; route; trip; "11"; "A" |] |]
            write (Path.Combine(baseStage, "extensions", "cz_trip_stop_zones.txt"))
                $"trip_id,stop_sequence,zone_id,zone_code,ids_system_id,source_provenance\n{trip},1,call-zone,CZ,,x\n"
            write (Path.Combine(baseStage, "gtfs-intermediate", "transfers.txt"))
                $"from_stop_id,to_stop_id,from_route_id,to_route_id,from_trip_id,to_trip_id,transfer_type,min_transfer_time,max_waiting_time\n{first},{second},{route},{route},{trip},{trip},2,60,300\n"
            let basePackage = Path.Combine(root, "base")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) baseStage basePackage
            CollectionAssert.AreEqual([| "waits_for|structured" |], relationRows basePackage "connection_claim" [| "direction"; "target_derivation" |] |> Array.map (String.concat "|"))
            CollectionAssert.AreEqual([| "timetable_note" |], relationRows basePackage "service_note" [| "kind" |] |> Array.map (fun row -> row.[0]))

            let replacement = "jdf:trip:replacement"
            let overlayStage = staging (Path.Combine(root, "overlay-work"))
            for relative in [ "gtfs-intermediate/trips.txt"; "gtfs-intermediate/stop_times.txt"; "extensions/cz_trips.txt"
                              "mappings/source_to_output_trips.csv"; "mappings/source_to_output_calls.csv" ] do
                let path = Path.Combine(overlayStage, relative)
                write path (File.ReadAllText(path).Replace(trip, replacement))
            write (Path.Combine(overlayStage, "mappings", "base_to_output_trips.csv"))
                $"base_trip_id,output_trip_id,valid_from,valid_to\n{trip},{replacement},20260913,20260913\n"
            write (Path.Combine(overlayStage, "base-package.path")) basePackage
            let overlayManifest = Path.Combine(overlayStage, "manifest.json")
            write overlayManifest (File.ReadAllText(overlayManifest).Replace("ids-jmk-gtfs", "pid-gtfs"))
            let output = Path.Combine(root, "output")
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) overlayStage output

            let assignments = relationRows output "assignment" [| "scope"; "kind"; "trip_id"; "location_id" |] |> Array.map (String.concat "|")
            CollectionAssert.AreEquivalent(
                [| $"trip|note|{replacement}|"; $"trip|wheelchair_accessible_vehicle|{replacement}|"; $"location|wheelchair_accessible||{first}" |], assignments)
            CollectionAssert.AreEqual([| replacement |], relationRows output "connection_claim" [| "origin_trip_id" |] |> Array.map (fun row -> row.[0]))
            CollectionAssert.AreEqual([| replacement |], relationRows output "travel_restriction" [| "trip_id" |] |> Array.map (fun row -> row.[0]))
            Assert.AreEqual(2, relationRows output "route_stop" [| "route_stop_id" |] |> Array.length)
            Assert.IsTrue(relationRows output "route_stop" [| "route_stop_id" |] |> Array.forall (fun row -> not (row.[0].Contains("%3A"))))
            Assert.IsFalse(relationRows output "call_zone" [| "trip_id" |] |> Array.exists (fun row -> row.[0] = trip))
            Assert.IsFalse(relationRows output "transfer" [| "from_trip_id" |] |> Array.exists (fun row -> row.[0] = trip))
            use manifest = JsonDocument.Parse(File.ReadAllText(Path.Combine(output, "manifest.json")))
            let sourceIds = manifest.RootElement.GetProperty("sources").EnumerateArray() |> Seq.map (fun item -> item.GetProperty("source_id").GetString()) |> Seq.toArray
            CollectionAssert.AreEquivalent([| "ids-jmk-gtfs"; "pid-gtfs" |], sourceIds)
        finally if Directory.Exists(root) then Directory.Delete(root, true)
