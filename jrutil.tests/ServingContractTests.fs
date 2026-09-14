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

    let staging root =
        let stage = Path.Combine(root, "stage")
        let gtfs, ext, mappings = Path.Combine(stage, "gtfs-intermediate"), Path.Combine(stage, "extensions"), Path.Combine(stage, "mappings")
        write (Path.Combine(stage, "manifest.json")) """{"publishable":true,"calibration":false,"source":{"source_id":"provider","payload_sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","retrieved_at":"2026-09-01T00:00:00+02:00"}}"""
        write (Path.Combine(gtfs, "agency.txt")) "agency_id,agency_name,agency_url,agency_timezone\na,Agency,https://example.test,Europe/Prague\n"
        write (Path.Combine(gtfs, "stops.txt")) "stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station,wheelchair_boarding\ns1,One,50,14,0,,0\ns2,Two,50.1,14.1,0,,0\n"
        write (Path.Combine(gtfs, "routes.txt")) "route_id,agency_id,route_short_name,route_long_name,route_type\nr,a,1,Route,3\n"
        write (Path.Combine(gtfs, "trips.txt")) "route_id,service_id,trip_id,trip_headsign,direction_id,wheelchair_accessible,bikes_allowed\nr,svc,out,Two,0,2,2\n"
        write (Path.Combine(gtfs, "stop_times.txt")) "trip_id,arrival_time,departure_time,stop_id,stop_sequence,pickup_type,drop_off_type,timepoint\nout,25:00:00,25:00:00,s1,1,0,0,1\nout,25:10:00,25:10:00,s2,2,0,0,1\n"
        write (Path.Combine(gtfs, "calendar_dates.txt")) "service_id,date,exception_type\nsvc,20260913,1\n"
        write (Path.Combine(gtfs, "transfers.txt")) "from_stop_id,to_stop_id,transfer_type,min_transfer_time,max_waiting_time\ns1,s2,2,60,300\n"
        write (Path.Combine(ext, "cz_routes.txt")) "route_id,cis_line_id,public_line_number,source_provenance\nr,001,1,provider\n"
        write (Path.Combine(ext, "cz_trips.txt")) "trip_id,cis_line_id,cis_trip_id,train_number,source_trip_ids,coverage_sources\nout,001,7,,,provider\n"
        write (Path.Combine(ext, "cz_stops.txt")) "stop_id,stop_place_id,cis_stop_id,post_id,asw_id,source_ids\ns1,s1,,,,\ns2,s2,,,,\n"
        write (Path.Combine(ext, "cz_stop_zones.txt")) "stop_place_id,zone_id,zone_code,route_id,ids_system_id,source_provenance\n"
        write (Path.Combine(ext, "cz_trip_stop_zones.txt")) "trip_id,stop_sequence,zone_id,zone_code,ids_system_id,source_provenance\n"
        write (Path.Combine(mappings, "source_to_output_routes.csv")) "source_id,source_route_id,output_route_id,method\nprovider,source-route,r,exact\n"
        write (Path.Combine(mappings, "source_to_output_stops.csv")) "source_id,source_stop_id,output_stop_id,target_stop_place_id,method\nprovider,A,s1,s1,exact\nprovider,B,s2,s2,exact\n"
        write (Path.Combine(mappings, "source_to_output_trips.csv")) "source_id,source_trip_id,base_trip_id,output_trip_id,valid_from,valid_to,method,pattern_edits,first_departure_delta_seconds,aggregate_time_delta_seconds,duration_delta_seconds,runner_up_margin\nprovider,source-trip,,out,20260913,20260913,exact,0,0,0,0,\n"
        write (Path.Combine(mappings, "source_to_output_calls.csv")) "source_id,source_trip_id,source_call_ordinal,source_stop_id,output_trip_id,output_call_ordinal,output_stop_id\nprovider,source-trip,0,A,out,1,s1\nprovider,source-trip,10,B,out,2,s2\n"
        write (Path.Combine(mappings, "operational_to_source_trips.csv")) "source_id,operational_line_id,operational_trip_id,source_trip_id,valid_from,valid_to\nprovider,L/1,C 2,source-trip,20260913,20260913\n"
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

    let writeTextParquet (path: string) (columns: string array) (rows: string array array) =
        let relation: JrUtil.Serving.Schema.Relation = {
            name = Path.GetFileNameWithoutExtension(path)
            fields = columns |> Array.map (fun name -> { name = name; dataType = JrUtil.Serving.Schema.Text; nullable = false })
            primaryKey = [| columns.[0] |]; sortKey = [| columns.[0] |]; foreignKeys = [||] }
        use writer = new JrUtil.Serving.ColumnWriter.Writer(path, relation, 1024, Threading.CancellationToken.None)
        writer.Append(columns |> Array.mapi (fun index _ -> JrUtil.Serving.ColumnWriter.Text(rows |> Array.map (fun row -> row.[index]))))

    [<TestMethod>]
    member _.``Composite public keys preserve identifier colons and escape delimiters``() =
        Assert.AreEqual(
            "jdf:route:000645:1/10%2Fwest",
            JrUtil.Serving.Identity.compositeKey [ "jdf:route:000645:1"; "10/west" ])

    [<TestMethod>]
    member _.``Fresh JDF route stop extensions keep public identifier colons readable``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-readable-route-stop-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let replaceRoute relative =
                let path = Path.Combine(stage, relative)
                write path (File.ReadAllText(path).Replace("r,", "jdf:route:000645:1,"))
            for relative in [ "gtfs-intermediate/routes.txt"; "gtfs-intermediate/trips.txt"; "extensions/cz_routes.txt" ] do
                replaceRoute relative
            let mapping = Path.Combine(stage, "mappings", "source_to_output_routes.csv")
            write mapping (File.ReadAllText(mapping).Replace(",r,", ",jdf:route:000645:1,"))
            writeTextParquet (Path.Combine(stage, "source_call_metadata.parquet"))
                [| "gtfs_trip_id"; "stop_sequence"; "source_route_stop_id"; "gtfs_stop_id" |]
                [| [| "out"; "1"; "11"; "s1" |]; [| "out"; "2"; "12"; "s2" |] |]
            writeTextParquet (Path.Combine(stage, "source_route_stop_zone_metadata.parquet"))
                [| "gtfs_route_id"; "source_route_stop_id"; "zone_id"; "zone_order" |]
                [| [| "jdf:route:000645:1"; "11"; "zone"; "0" |] |]
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) stage output
            let routeStops = parquetStrings (Path.Combine(output, "serving", "route_stop.parquet")) "route_stop_id"
            CollectionAssert.Contains(routeStops, "jdf:route:000645:1/11")
            Assert.IsFalse(routeStops |> Array.exists (fun value -> value.Contains("%3A")))
            let extension = File.ReadAllText(Path.Combine(output, "extensions", "cz_route_stop_zones.txt"))
            StringAssert.Contains(extension, "jdf:route:000645:1/11")
            Assert.IsFalse(extension.Contains("%3A"))
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
                                    arrival = Nullable sequence; departure = Nullable(sequence + 1); passage = Nullable()
                                    pickup = 0s; dropoff = 0s; timepoint = true; headsign = null; distance = Nullable() })
                Assert.AreEqual(40000L, writer.Complete())
                Assert.AreEqual(0, writer.ActiveCompressionWorkers)
            let first, second = Path.Combine(root, "sync.parquet"), Path.Combine(root, "buffered.parquet")
            write first false
            write second true
            CollectionAssert.AreEqual(File.ReadAllBytes(first), File.ReadAllBytes(second))
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Native route stops deduplicate across bounded windows and reject conflicts``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-native-route-stops-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let output = Path.Combine(root, "routes.parquet")
            do
                use writer = new JrUtil.Serving.RouteStopWriter.Writer(output, Threading.CancellationToken.None, maximumBufferBytes = 400L)
                for _ in 1 .. 20 do
                    for stop in ["3"; "1"; "2"] do writer.Append("r", stop, "location")
                Assert.AreEqual(3, writer.Complete(fun _ _ -> ()))
            CollectionAssert.AreEqual([| "1"; "2"; "3" |], parquetStrings output "route_stop_id")
            do
                use writer = new JrUtil.Serving.RouteStopWriter.Writer(Path.Combine(root, "conflict.parquet"), Threading.CancellationToken.None, maximumBufferBytes = 200L)
                writer.Append("r", "1", "first")
                writer.Append("r", "2", "middle")
                writer.Append("r", "1", "last")
                Assert.ThrowsExactly<InvalidOperationException>(fun () -> writer.Complete(fun _ _ -> ()) |> ignore) |> ignore
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Typed binary sort spills deterministically and cleans up on cancellation``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-binary-sort-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let sort budget token rows =
                JrUtil.Serving.BinarySort.sort root budget 7 token (fun _ _ -> ()) (fun (_: int) -> 16L) compare
                    (fun writer value -> writer.Write(value: int)) (fun reader -> reader.ReadInt32()) rows
            let input = [| 1000 .. -1 .. 0 |]
            CollectionAssert.AreEqual(Array.sort input, sort 64L Threading.CancellationToken.None input |> Seq.toArray)
            Assert.AreEqual(0, Directory.GetDirectories(root).Length)
            use cancellation = new Threading.CancellationTokenSource()
            let interrupted = seq { yield 3; cancellation.Cancel(); yield 2 }
            Assert.ThrowsExactly<OperationCanceledException>(fun () -> sort 64L cancellation.Token interrupted |> Seq.toArray |> ignore) |> ignore
            Assert.AreEqual(0, Directory.GetDirectories(root).Length)
            Assert.ThrowsExactly<InvalidOperationException>(fun () -> sort 8L Threading.CancellationToken.None [1] |> Seq.toArray |> ignore) |> ignore
            Assert.AreEqual(0, Directory.GetDirectories(root).Length)
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Native source calls preserve nullable times and lexicographic source sequences``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-native-source-calls-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let spool = Path.Combine(root, "calls.bin")
            do
                use writer = new JrUtil.Serving.SourceCallWriter.Spool(spool)
                for sequence in [2; 10; 1] do
                    writer.Append({ tripId = "trip"; stopSequence = sequence; stopId = "stop"
                                    arrivalTime = None; departureTime = Some (NodaTime.Period.FromSeconds(90000L))
                                    headsign = None; pickupType = None; dropoffType = None; timepoint = None
                                    shapeDistTraveled = None; stopZoneIds = None })
            let output = Path.Combine(root, "calls.parquet")
            Assert.AreEqual(3, JrUtil.Serving.SourceCallWriter.write output spool (dict ["trip", "binding"]) Threading.CancellationToken.None (fun _ _ -> ()))
            let rows = JrUtil.Serving.PackageReader.readTextRows output [| "source_sequence"; "call_sequence"; "source_stop_id"; "scheduled_arrival"; "scheduled_departure" |] |> Seq.toArray
            Asserts.assertEqual [| [|"1"; "1"; "stop"; ""; "90000"|]; [|"10"; "10"; "stop"; ""; "90000"|]; [|"2"; "2"; "stop"; ""; "90000"|] |] rows
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Native source call blocks reorder bindings and spill oversized trips``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-source-blocks-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let spool = Path.Combine(root, "calls.bin")
            do
                use writer = new JrUtil.Serving.SourceCallWriter.Spool(spool)
                for trip, count in ["a", 20000; "b", 3] do
                    for sequence = count downto 1 do
                        writer.Append({ tripId = trip; stopSequence = sequence; stopId = trip
                                        arrivalTime = Some (NodaTime.Period.FromSeconds(int64 sequence)); departureTime = None
                                        headsign = None; pickupType = None; dropoffType = None; timepoint = None
                                        shapeDistTraveled = None; stopZoneIds = None })
                writer.Complete()
            let output = Path.Combine(root, "calls.parquet")
            let phases = Collections.Generic.HashSet<string>()
            Assert.AreEqual(20003, JrUtil.Serving.SourceCallWriter.write output spool (dict ["a", "z"; "b", "a"])
                                      Threading.CancellationToken.None (fun phase _ -> phases.Add(phase) |> ignore))
            Assert.IsTrue(phases.Contains("trip-calls-sort-run"))
            let rows = JrUtil.Serving.PackageReader.readTextRows output [| "binding_id"; "source_sequence"; "source_stop_id"; "scheduled_arrival" |] |> Seq.toArray
            let expected = [| for binding, trip, count in ["a", "b", 3; "z", "a", 20000] do
                                 for sequence in [|1 .. count|] |> Array.map string |> Array.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right)) do
                                     yield [|binding; sequence; trip; sequence|] |]
            Asserts.assertEqual expected rows
            Assert.AreEqual(0, Directory.GetDirectories(root).Length)
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Native binding facts preserve calendar bounds identities and call fingerprints``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-native-bindings-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            Directory.Delete(Path.Combine(stage, "mappings"), true)
            let gtfs = Path.Combine(stage, "gtfs-intermediate")
            write (Path.Combine(gtfs, "calendar.txt")) "service_id,monday,tuesday,wednesday,thursday,friday,saturday,sunday,start_date,end_date\nsvc,1,1,1,1,1,0,0,20260913,20260913\n"
            write (Path.Combine(gtfs, "calendar_dates.txt")) "service_id,date,exception_type\nsvc,20260912,1\nsvc,20260914,2\n"
            let expected = Path.Combine(root, "expected")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) stage expected
            let feed = JrUtil.Gtfs.gtfsParseFolder () gtfs
            let summaries = Path.Combine(root, "summaries.bin")
            do
                use writer = new JrUtil.Serving.TripCallWriter.SummaryWriter(summaries)
                for call in feed.stopTimes do writer.Append(call)
                writer.Complete()
            let facts = Path.Combine(root, "trips.bin")
            JrUtil.Serving.BindingWriter.writeNativeFacts facts "provider" feed
            let actual = Path.Combine(root, "bindings.parquet")
            let rows = JrUtil.Serving.BindingWriter.readNativeFacts facts summaries
            Assert.AreEqual(1, JrUtil.Serving.BindingWriter.write actual Threading.CancellationToken.None (fun _ _ -> ()) rows)
            let columns = JrUtil.Serving.Schema.relations |> Array.find (fun relation -> relation.name = "source_trip_map") |> fun relation -> relation.fields |> Array.map _.name
            let values path = JrUtil.Serving.PackageReader.readTextRows path columns |> Seq.toArray
            Asserts.assertEqual (values (Path.Combine(expected, "serving", "source_trip_map.parquet"))) (values actual)
        finally Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Native trips equal the GTFS projection``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-native-trips-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let expected = Path.Combine(root, "expected")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) stage expected
            let actual = Path.Combine(root, "trips.parquet")
            let trip: JrUtil.GtfsModel.Trip = {
                id = "out"; routeId = "r"; serviceId = "svc"; headsign = Some "Two"; shortName = None
                directionId = Some "0"; blockId = Some " "; shapeId = None
                wheelchairAccessible = Some "2"; bikesAllowed = Some JrUtil.GtfsModel.NoBicycles }
            Assert.AreEqual(1, JrUtil.Serving.TripWriter.write actual Threading.CancellationToken.None (fun _ _ -> ()) [trip])
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
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) stage expected
            let actual = Path.Combine(root, "calls.parquet")
            let summaries = Path.Combine(root, "summaries.bin")
            do
                use writer = new JrUtil.Serving.TripCallWriter.Writer(actual, Threading.CancellationToken.None)
                use summaryWriter = new JrUtil.Serving.TripCallWriter.SummaryWriter(summaries)
                for sequence, stop, seconds in [1, "s1", 90000L; 2, "s2", 90600L] do
                    let call: JrUtil.GtfsModel.StopTime = {
                        tripId = "out"; stopSequence = sequence; stopId = stop
                        arrivalTime = Some (NodaTime.Period.FromSeconds(seconds))
                        departureTime = Some (NodaTime.Period.FromSeconds(seconds))
                        headsign = None; pickupType = Some JrUtil.GtfsModel.RegularlyScheduled
                        dropoffType = Some JrUtil.GtfsModel.RegularlyScheduled
                        timepoint = Some JrUtil.GtfsModel.Exact; shapeDistTraveled = None; stopZoneIds = None }
                    writer.Append(JrUtil.Serving.TripCallWriter.fromGtfs call stop null null)
                    summaryWriter.Append(call)
                Assert.AreEqual(2L, writer.Complete())
                summaryWriter.Complete()
            let fields =
                JrUtil.Serving.Schema.relations |> Array.find (fun relation -> relation.name = "trip_call")
                |> fun relation -> relation.fields |> Array.map _.name
            let rows path = JrUtil.Serving.PackageReader.readTextRows path fields |> Seq.toArray
            Asserts.assertEqual (rows (Path.Combine(expected, "serving", "trip_call.parquet"))) (rows actual)
            let summary = (JrUtil.Serving.TripCallWriter.readSummaries summaries).["out"]
            Assert.AreEqual(Nullable 90000, summary.scheduledStart)
            Assert.AreEqual(Nullable 90600, summary.scheduledEnd)
            for fingerprint in parquetStrings (Path.Combine(expected, "serving", "source_trip_map.parquet")) "call_pattern_sha256" do
                Assert.AreEqual(fingerprint, summary.callPatternSha256)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Typed sink rejects oversized batches before writing and supports cancellation``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-column-budget-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        let relation: JrUtil.Serving.Schema.Relation = {
            name = "bounded_text"
            fields = [| { name = "id"; dataType = JrUtil.Serving.Schema.Text; nullable = false } |]
            primaryKey = [| "id" |]; sortKey = [| "id" |]; foreignKeys = [||] }
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
    member _.``Validation rejects unordered keys across row groups with matching hashes``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-invalid-serving-order-" + Guid.NewGuid().ToString("N"))
        try
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) (staging root) output
            let path = Path.Combine(output, "serving", "shape.parquet")
            File.Delete(path)
            let relation = JrUtil.Serving.Schema.relations |> Array.find (fun relation -> relation.name = "shape")
            do
                use writer = new JrUtil.Serving.ColumnWriter.Writer(path, relation, 1, Threading.CancellationToken.None)
                for key in ["z"; "a"] do
                    writer.Append [| JrUtil.Serving.ColumnWriter.Text [|key|]; JrUtil.Serving.ColumnWriter.Text [|"source"|] |]
            let manifestPath = Path.Combine(output, "manifest.json")
            let manifest = System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText(manifestPath))
            let entry = manifest.["files"].AsArray() |> Seq.find (fun entry -> entry.["path"].GetValue<string>() = "serving/shape.parquet")
            let digest =
                use input = File.OpenRead(path)
                Security.Cryptography.SHA256.HashData(input) |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()
            entry.["sha256"] <- System.Text.Json.Nodes.JsonValue.Create(digest)
            entry.["size_bytes"] <- System.Text.Json.Nodes.JsonValue.Create(FileInfo(path).Length)
            let declaration = manifest.["relations"].AsArray() |> Seq.find (fun entry -> entry.["name"].GetValue<string>() = "shape")
            declaration.["row_count"] <- System.Text.Json.Nodes.JsonValue.Create(2)
            File.WriteAllText(manifestPath, manifest.ToJsonString())
            let result = JrUtil.Serving.Validation.inspect output
            Assert.AreEqual(1, result.errors.Length, String.Join("; ", result.errors))
            Assert.IsTrue(result.errors.[0].Contains("duplicate or unordered primary key"))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Production package has closed validated inventory and deterministic bytes``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-contract-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            let first, second = Path.Combine(root, "first"), Path.Combine(root, "second")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) stage first
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) stage second
            let result = JrUtil.Serving.Validation.validatePackage first
            Assert.AreEqual(37, result.relationCount)
            Assert.AreEqual(44, result.fileCount)
            JrUtil.Serving.Validation.compareByteIdentical first second
            Assert.IsFalse(Directory.Exists(Path.Combine(first, "gtfs-intermediate")))
            Assert.IsFalse(File.Exists(Path.Combine(first, "extensions", "cz_trips.txt")))
            use zip = ZipFile.OpenRead(Path.Combine(first, "gtfs.zip"))
            use transfers = new StreamReader(zip.GetEntry("transfers.txt").Open())
            Assert.IsFalse(transfers.ReadLine().Contains("max_waiting_time"))
            let extensionText = File.ReadAllText(Path.Combine(first, "extensions", "cz_transfer_constraints.txt"))
            Assert.IsTrue(extensionText.Contains("300"))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Bindings normalize operational keys and preserve actual source sequence``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-identity-" + Guid.NewGuid().ToString("N"))
        try
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) (staging root) output
            let tripKeys = parquetStrings (Path.Combine(output, "serving", "source_trip_map.parquet")) "source_trip_id"
            CollectionAssert.Contains(tripKeys, "source-trip")
            CollectionAssert.Contains(tripKeys, "L%2F1/C%202")
            let sequences = parquetStrings (Path.Combine(output, "serving", "source_call_map.parquet")) "source_sequence"
            CollectionAssert.AreEquivalent([| "0"; "10" |], sequences)
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Mapped calls retain overnight target timestamps``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-call-times-" + Guid.NewGuid().ToString("N"))
        try
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) (staging root) output
            let rows = JrUtil.Serving.PackageReader.readTextRows
                           (Path.Combine(output, "serving", "source_call_map.parquet"))
                           [| "source_sequence"; "scheduled_arrival"; "scheduled_departure" |]
                       |> Seq.toArray
            CollectionAssert.AreEquivalent([| "0:90000:90000"; "10:90600:90600" |],
                rows |> Array.map (String.concat ":"))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Selected call provenance keeps lexical multi-digit ordinal order``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-selected-order-" + Guid.NewGuid().ToString("N"))
        try
            let stage = staging root
            write (Path.Combine(stage, "gtfs-intermediate", "stop_times.txt"))
                "trip_id,arrival_time,departure_time,stop_id,stop_sequence,pickup_type,drop_off_type,timepoint\nout,25:00:00,25:00:00,s1,1,0,0,1\nout,25:05:00,25:05:00,s1,2,0,0,1\nout,25:10:00,25:10:00,s2,10,0,0,1\n"
            write (Path.Combine(stage, "provenance", "selected_fields.csv"))
                "output_object_id,field,source_id,value,capability_mode\nout#1,stop_id,provider,s1,authoritative\nout#2,stop_id,provider,s1,authoritative\nout#10,stop_id,provider,s2,authoritative\n"
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) stage output
            let keys = parquetStrings (Path.Combine(output, "serving", "selected_field_provenance.parquet")) "object_key"
            CollectionAssert.AreEqual(
                [| JrUtil.Serving.Identity.compositeKey [ "out"; "1" ]
                   JrUtil.Serving.Identity.compositeKey [ "out"; "10" ]
                   JrUtil.Serving.Identity.compositeKey [ "out"; "2" ] |], keys)
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Carried base calls require exact target sequence membership``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-call-gap-" + Guid.NewGuid().ToString("N"))
        try
            let baseStage = staging (Path.Combine(root, "base-work"))
            let basePackage = Path.Combine(root, "base")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) baseStage basePackage
            let overlayStage = staging (Path.Combine(root, "overlay-work"))
            write (Path.Combine(overlayStage, "gtfs-intermediate", "stop_times.txt"))
                "trip_id,arrival_time,departure_time,stop_id,stop_sequence,pickup_type,drop_off_type,timepoint\nout,25:00:00,25:00:00,s1,1,0,0,1\nout,25:10:00,25:10:00,s2,3,0,0,1\n"
            write (Path.Combine(overlayStage, "mappings", "source_to_output_calls.csv"))
                "source_id,source_trip_id,source_call_ordinal,source_stop_id,output_trip_id,output_call_ordinal,output_stop_id\nprovider,source-trip,0,A,out,1,s1\n"
            write (Path.Combine(overlayStage, "mappings", "base_to_output_trips.csv"))
                "base_trip_id,output_trip_id,valid_from,valid_to\nout,out,20260913,20260913\n"
            write (Path.Combine(overlayStage, "base-package.path")) basePackage
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) overlayStage output
            let sequences = parquetStrings (Path.Combine(output, "serving", "source_call_map.parquet")) "source_sequence"
            CollectionAssert.AreEqual([| "0" |], sequences)
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Contract files declare every serving relation``() =
        let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
        use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(root, "contracts", "serving-v2.json")))
        let names = document.RootElement.GetProperty("relations").EnumerateArray() |> Seq.map (fun item -> item.GetProperty("name").GetString()) |> Seq.toArray
        CollectionAssert.AreEqual(JrUtil.Serving.Schema.relationNames, names)

    [<TestMethod>]
    member _.``Overlay compiler view is rebuilt from GTFS ZIP and serving identities``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-reader-" + Guid.NewGuid().ToString("N"))
        try
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) (staging root) output
            let gtfs, extensions = JrUtil.Serving.PackageReader.prepareCompilerView output (Path.Combine(root, "scratch"))
            Assert.IsTrue(File.ReadAllText(Path.Combine(gtfs, "trips.txt")).Contains("out"))
            Assert.IsTrue(File.ReadAllText(Path.Combine(extensions, "cz_routes.txt")).Contains("001"))
            Assert.IsTrue(File.ReadAllText(Path.Combine(extensions, "cz_trips.txt")).Contains("7"))
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Overlay slicing carries production base identities and calls``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-base-carry-" + Guid.NewGuid().ToString("N"))
        try
            let baseStage = staging (Path.Combine(root, "base-work"))
            write (Path.Combine(baseStage, "extensions", "cz_trips.txt"))
                "trip_id,cis_line_id,cis_trip_id,train_number,source_trip_ids,coverage_sources\nout,001,7,123,PA=pa-1|TR=tr-1,provider\n"
            let basePackage = Path.Combine(root, "base")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) baseStage basePackage

            let overlayStage = staging (Path.Combine(root, "overlay-work"))
            write (Path.Combine(overlayStage, "extensions", "cz_trips.txt"))
                "trip_id,cis_line_id,cis_trip_id,train_number,source_trip_ids,coverage_sources\nout,001,7,123,,provider\n"
            write (Path.Combine(overlayStage, "mappings", "base_to_output_trips.csv"))
                "base_trip_id,output_trip_id,valid_from,valid_to\nout,out,20260913,20260913\n"
            write (Path.Combine(overlayStage, "base-package.path")) basePackage
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) overlayStage output

            let tripKeys = parquetStrings (Path.Combine(output, "serving", "source_trip_map.parquet")) "source_trip_id"
            CollectionAssert.Contains(tripKeys, "pa-1")
            CollectionAssert.Contains(tripKeys, "tr-1")
            let sequences = parquetStrings (Path.Combine(output, "serving", "source_call_map.parquet")) "source_sequence"
            Assert.IsTrue(sequences |> Array.contains "0")
            Assert.IsTrue(sequences |> Array.contains "10")
        finally if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Overlay projects native base semantics onto replacement trips``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-serving-semantic-carry-" + Guid.NewGuid().ToString("N"))
        try
            let baseStage = staging (Path.Combine(root, "base-work"))
            writeTextParquet (Path.Combine(baseStage, "source_call_metadata.parquet"))
                [| "gtfs_trip_id"; "stop_sequence"; "source_route_stop_id"; "gtfs_stop_id" |]
                [| [| "out"; "1"; "11"; "s1" |]; [| "out"; "2"; "12"; "s2" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_route_stop_zone_metadata.parquet"))
                [| "gtfs_route_id"; "source_route_stop_id"; "zone_id"; "zone_order" |]
                [| [| "r"; "11"; "zone"; "0" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_notice_metadata.parquet"))
                [| "source_notice_id"; "notice_kind"; "gtfs_route_id"; "gtfs_trip_id"; "label"; "text"; "valid_from"; "valid_to"; "service_note_type" |]
                [| [| "notice"; "information"; ""; "out"; "Label"; "Text"; "20260913"; "20260913"; "" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_trip_feature_metadata.parquet"))
                [| "gtfs_trip_id"; "source_code"; "feature_kind"; "source_object_id" |]
                [| [| "out"; "W"; "wheelchair_accessible"; "feature-source" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_location_feature_metadata.parquet"))
                [| "gtfs_stop_id"; "source_code"; "feature_kind"; "source_object_id" |]
                [| [| "s1"; "W"; "wheelchair_accessible"; "location-source" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_transfer_metadata.parquet"))
                [| "source_transfer_id"; "gtfs_trip_id"; "source_route_stop_id"; "transfer_type"; "transfer_route_id"; "transfer_stop_id"; "transfer_stop_post_id"; "transfer_end_stop_id"; "transfer_end_stop_post_id"; "wait_minutes"; "note" |]
                [| [| "connection"; "out"; "11"; "wait"; ""; ""; ""; ""; ""; "5"; "Connection" |] |]
            writeTextParquet (Path.Combine(baseStage, "source_travel_restriction_metadata.parquet"))
                [| "assignment_scope"; "gtfs_route_id"; "gtfs_trip_id"; "source_route_stop_id"; "group_code" |]
                [| [| "trip"; "r"; "out"; "11"; "G" |] |]
            write (Path.Combine(baseStage, "extensions", "cz_trip_stop_zones.txt"))
                "trip_id,stop_sequence,zone_id,zone_code,ids_system_id,source_provenance\nout,1,call-zone,CZ,,provider\n"
            write (Path.Combine(baseStage, "gtfs-intermediate", "transfers.txt"))
                "from_stop_id,to_stop_id,from_route_id,to_route_id,from_trip_id,to_trip_id,transfer_type,min_transfer_time,max_waiting_time\ns1,s2,r,r,out,out,2,60,300\n"
            let basePackage = Path.Combine(root, "base")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) baseStage basePackage

            let overlayStage = staging (Path.Combine(root, "overlay-work"))
            for relative in [ "trips.txt"; "stop_times.txt" ] do
                let path = Path.Combine(overlayStage, "gtfs-intermediate", relative)
                write path (File.ReadAllText(path).Replace("out", "replacement"))
            let tripExtension = Path.Combine(overlayStage, "extensions", "cz_trips.txt")
            write tripExtension (File.ReadAllText(tripExtension).Replace("out", "replacement"))
            let tripMappings = Path.Combine(overlayStage, "mappings", "source_to_output_trips.csv")
            write tripMappings (File.ReadAllText(tripMappings).Replace(",out,", ",replacement,"))
            let callMappings = Path.Combine(overlayStage, "mappings", "source_to_output_calls.csv")
            write callMappings (File.ReadAllText(callMappings).Replace(",out,", ",replacement,"))
            write (Path.Combine(overlayStage, "mappings", "base_to_output_trips.csv"))
                "base_trip_id,output_trip_id,valid_from,valid_to\nout,replacement,20260913,20260913\n"
            write (Path.Combine(overlayStage, "base-package.path")) basePackage
            let overlayManifest = Path.Combine(overlayStage, "manifest.json")
            write overlayManifest (File.ReadAllText(overlayManifest).Replace("provider", "regional"))
            let output = Path.Combine(root, "output")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) overlayStage output

            let relation name columns =
                JrUtil.Serving.PackageReader.readTextRows (Path.Combine(output, "serving", name + ".parquet")) columns |> Seq.toArray
            CollectionAssert.AreEqual([| "replacement" |], relation "service_note_assignment" [| "trip_id" |] |> Array.map (fun row -> row.[0]))
            CollectionAssert.AreEqual([| "replacement" |], relation "service_feature_assignment" [| "trip_id" |] |> Array.map (fun row -> row.[0]))
            CollectionAssert.AreEqual([| "replacement" |], relation "connection_claim" [| "origin_trip_id" |] |> Array.map (fun row -> row.[0]))
            CollectionAssert.AreEqual([| "replacement" |], relation "travel_restriction_assignment" [| "trip_id" |] |> Array.map (fun row -> row.[0]))
            Assert.AreEqual(2, relation "route_stop" [| "route_stop_id" |] |> Array.length)
            Assert.IsTrue(relation "route_stop" [| "route_stop_id" |] |> Array.forall (fun row -> not (row.[0].Contains("%3A"))))
            Assert.AreEqual(1, relation "route_stop_zone" [| "zone_id" |] |> Array.length)
            Assert.AreEqual(1, relation "location_feature" [| "location_id" |] |> Array.length)
            let extensionLines name = File.ReadAllLines(Path.Combine(output, "extensions", name))
            let zoneLines = extensionLines "cz_zones.txt"
            Assert.IsTrue(zoneLines |> Array.exists (fun line -> line.Contains("\"call-zone\",\"CZ\"")))
            Assert.IsTrue(extensionLines "cz_route_stop_zones.txt" |> Array.exists (fun line -> line.Contains("\"zone\"")))
            Assert.IsTrue(extensionLines "cz_call_zones.txt" |> Array.exists (fun line -> line.Contains("\"replacement\",\"1\",\"call-zone\",\"0\"")))
            Assert.IsFalse(extensionLines "cz_call_zones.txt" |> Array.exists (fun line -> line.StartsWith("\"out\",")))
            Assert.IsTrue(extensionLines "cz_transfer_constraints.txt" |> Array.exists (fun line -> line.Contains("\"replacement\",\"replacement\",\"300\"")))
            Assert.IsFalse(extensionLines "cz_transfer_constraints.txt" |> Array.exists (fun line -> line.Contains("\"out\",\"out\"")))
            use manifest = JsonDocument.Parse(File.ReadAllText(Path.Combine(output, "manifest.json")))
            let sourceIds = manifest.RootElement.GetProperty("sources").EnumerateArray() |> Seq.map (fun item -> item.GetProperty("source_id").GetString()) |> Seq.toArray
            CollectionAssert.AreEquivalent([| "provider"; "regional" |], sourceIds)
        finally if Directory.Exists(root) then Directory.Delete(root, true)
