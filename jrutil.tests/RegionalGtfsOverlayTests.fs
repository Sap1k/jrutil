namespace JrUtil.Tests

open System
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Microsoft.VisualStudio.TestTools.UnitTesting

open JrUtil
open JrUtil.RegionalOverlay.Types

[<TestClass>]
type RegionalGtfsOverlayTests() =
    let write (path: string) (text: string) =
        Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
        File.WriteAllText(path, text.Replace("\r\n", "\n"), new UTF8Encoding(false))

    let sha path =
        use stream = File.OpenRead(path)
        Convert.ToHexString(SHA256.HashData(stream)).ToLowerInvariant()

    let diagnosticPath output section file = Path.Combine(output + ".diagnostics", section, file)

    let readGtfs output file =
        use archive = ZipFile.OpenRead(Path.Combine(output, "gtfs.zip"))
        use reader = new StreamReader(archive.GetEntry(file).Open(), Encoding.UTF8)
        reader.ReadToEnd()

    /// Fixtures describe the base as compiler tables; the overlay only accepts
    /// a production package. Translate the fixture shorthand into what the JDF
    /// compiler hands over, on a copy, and finalize it into a package.
    let productionBase (basePath: string) =
        if File.Exists(Path.Combine(basePath, "gtfs.zip")) then basePath
        else
            let unique = Guid.NewGuid().ToString("N")
            let stage = basePath + ".stage-" + unique
            for path in Directory.EnumerateFiles(basePath, "*", SearchOption.AllDirectories) do
                let target = Path.Combine(stage, Path.GetRelativePath(basePath, path))
                Directory.CreateDirectory(Path.GetDirectoryName(target)) |> ignore
                File.Copy(path, target)
            // Compilers emit calls grouped by trip in stop_sequence order.
            let stopTimes = Path.Combine(stage, "gtfs-intermediate", "stop_times.txt")
            if File.Exists(stopTimes) then
                let lines = File.ReadAllLines(stopTimes) |> Array.filter (String.IsNullOrWhiteSpace >> not)
                let header = lines.[0].Split(',')
                let trip, sequence = Array.IndexOf(header, "trip_id"), Array.IndexOf(header, "stop_sequence")
                let ordered =
                    lines.[1..]
                    |> Array.sortBy (fun line -> let fields = line.Split(',') in fields.[trip], int fields.[sequence])
                File.WriteAllText(stopTimes, String.concat "\n" (Array.append [| lines.[0] |] ordered) + "\n")
            // A real base names its source and records its GVD as `gvd`.
            let manifestPath = Path.Combine(stage, "manifest.json")
            let manifest = System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText(manifestPath)).AsObject()
            match manifest.["source_snapshot"] with
            | :? System.Text.Json.Nodes.JsonObject as snapshot when isNull snapshot.["source_id"] ->
                snapshot.["source_id"] <- System.Text.Json.Nodes.JsonValue.Create("national-jdf-vld-drahy")
            | _ -> ()
            match manifest.["service_horizon"] with
            | null -> ()
            | horizon ->
                manifest.Remove("service_horizon") |> ignore
                manifest.["gvd"] <- horizon.DeepClone()
            File.WriteAllText(manifestPath, manifest.ToJsonString())
            // JDF records detour (výluka) timetables as route metadata.
            let czRoutes = Path.Combine(stage, "extensions", "cz_routes.txt")
            if File.Exists(czRoutes) && (File.ReadLines(czRoutes) |> Seq.head).Contains("timetable_kind") then
                let lines = File.ReadAllLines(czRoutes) |> Array.filter (String.IsNullOrWhiteSpace >> not)
                let header = lines.[0].Split(',')
                let route, kind = Array.IndexOf(header, "route_id"), Array.IndexOf(header, "timetable_kind")
                let rows = lines.[1..] |> Array.map (fun line -> let fields = line.Split(',') in fields.[route], (if fields.[kind] = "detour" then "True" else "False"))
                let relation: JrUtil.Serving.Schema.Relation = {
                    name = "source_route_metadata"
                    fields = [| for name in [ "gtfs_route_id"; "detour" ] -> { name = name; dataType = JrUtil.Serving.Schema.Text; nullable = false } |]
                    primaryKey = [| "gtfs_route_id" |]; foreignKeys = [||] }
                use writer = new JrUtil.Serving.ColumnWriter.Writer(Path.Combine(stage, "source_route_metadata.parquet"), relation, 1024, Threading.CancellationToken.None)
                writer.Append([| JrUtil.Serving.ColumnWriter.Text(rows |> Array.map fst); JrUtil.Serving.ColumnWriter.Text(rows |> Array.map snd) |])
            let package = basePath + ".package-" + unique
            StagingFixture.finalize Map.empty None (fun _ _ -> ()) stage package
            package

    /// Run one PID-shaped source through the combined path with a one-source
    /// overlay-all policy next to its profile.
    let execute auditDate (profile: string) gvdYear (binding: SourceBinding) basePath output =
        let policy = Path.Combine(Path.GetDirectoryName(profile), "overlay-all-" + Guid.NewGuid().ToString("N") + ".json")
        write policy ($"{{\"schema_version\":1,\"sources\":[{{\"source_id\":\"{binding.sourceId}\",\"policy\":\"{Path.GetFileName(profile)}\",\"adapter\":\"pid-v1\"}}]}}")
        (RegionalGtfsOverlay.compile {
            auditDate = auditDate; policyPath = policy; gvdYear = gvdYear
            bindings = [| binding |]; baseBundle = productionBase basePath; outputBundle = output; converterVersion = "test-commit"
            diagnosticsOutput = Some (output + ".diagnostics"); diagnosticTraces = true }).aggregate

    let executeAll policy gvdYear bindings basePath output =
        RegionalGtfsOverlay.compile {
            auditDate = None; policyPath = policy; gvdYear = gvdYear
            bindings = bindings; baseBundle = productionBase basePath; outputBundle = output; converterVersion = "test-commit"
            diagnosticsOutput = Some (output + ".diagnostics"); diagnosticTraces = true }

    let policyJson = """{
      "schema_version": 4,
      "source": {
        "source_id": "pid-gtfs",
        "excluded_route_types": ["2"],
        "maximum_snapshot_skew_days": 7,
        "route_join": {
          "table": "route_sub_agencies.txt",
          "source_keys": ["route_id", "sub_agency_id"],
          "lookup_keys": ["route_id", "sub_agency_id"],
          "value_column": "route_licence_number",
          "target_namespace": "cis_line_id"
        },
        "stop_match": {
          "group_column": "asw_node_id",
          "post_column": "asw_stop_id",
          "maximum_distance_metres": 125,
          "split_flat_groups_by_name": true,
          "contextual_inference": {
            "enabled": true,
            "minimum_mapped_calls": 1
          }
        },
        "trip_match": {
          "time_resolution_seconds": 60,
          "exact_pattern_proximity": true,
          "pattern_edit": {
            "enabled": true,
            "maximum_edits": 3,
            "minimum_agreement": 0.7,
            "require_same_endpoints": true
          },
          "source_revision": {
            "column": "trip_id",
            "regex": "_(?<revision>\\d{6})(?:_|$)",
            "date_format": "yyMMdd"
          },
          "minimum_capability_tier": {
            "call_boarding_points": "pattern_edit_nearest",
            "shapes": "pattern_edit_nearest",
            "schedules": "pattern_nearest",
            "transfers": "pattern_nearest"
          }
        },
        "trip_set_authority": {
          "modes": [],
          "source_native_modes": []
        },
        "route_match_tiers": ["companion_assertion", "structural_trip_evidence"],
        "trip_match_tiers": ["full_signature", "pattern_endpoints", "pattern_first", "pattern_nearest", "pattern_edit_nearest"],
        "capabilities": [
          "stop_coordinates", "boarding_points", "call_boarding_points", "shapes",
          "route_short_name", "route_long_name", "route_color", "route_text_color",
          "transfers", "schedules"
        ],
        "overrides": {"stops":"stops.csv"}
      }
    }"""

    let makeFixture root =
        let basePath = Path.Combine(root, "base")
        let gtfs = Path.Combine(basePath, "gtfs-intermediate")
        let ext = Path.Combine(basePath, "extensions")
        write (Path.Combine(basePath, "manifest.json")) """{"source_snapshot":{"retrieved_at":"2026-08-20T00:00:00+02:00"}}"""
        write (Path.Combine(basePath, "evidence.bin")) "national-evidence"
        write (Path.Combine(gtfs, "agency.txt")) "agency_id,agency_name,agency_url,agency_timezone\na,National,https://example.test,Europe/Prague\n"
        write (Path.Combine(gtfs, "stops.txt")) "stop_id,stop_code,stop_name,stop_desc,stop_lat,stop_lon,zone_id,stop_url,location_type,parent_station,stop_timezone,wheelchair_boarding,platform_code\nbp1,,City Alpha,,50.0000,14.0000,,,1,,Europe/Prague,0,\nbp2,,Beta,,50.0010,14.0010,,,1,,Europe/Prague,0,\nbp3,,City Clinic Alpha,,50.0001,14.0001,,,1,,Europe/Prague,0,\nbp4,,Gamma,,50.0005,14.0005,,,1,,Europe/Prague,0,\n"
        write (Path.Combine(gtfs, "routes.txt")) "route_id,agency_id,route_short_name,route_long_name,route_desc,route_type,route_url,route_color,route_text_color,route_sort_order\nr1,a,100,National bus,,701,,111111,ffffff,\nr1v,a,100,National bus variant,,701,,111111,ffffff,\nr2,a,10,National tram,,900,,222222,ffffff,\nr3,a,101,Edited-pattern bus,,701,,333333,ffffff,\nr4,a,102,Availability bus,,701,,444444,ffffff,\n"
        write (Path.Combine(gtfs, "trips.txt")) "route_id,service_id,trip_id,trip_headsign,trip_short_name,direction_id,block_id,shape_id,wheelchair_accessible,bikes_allowed\nr1,bd,bt1,National Alpha,National short,0,,,1,2\nr1v,bd,bt1dup,Protected variant headsign,Protected variant short,0,,,2,1\nr2,td,bt2,National Beta,National tram short,0,,,1,2\nr3,ed,bt3good,Good aligned schedule,,0,,,1,2\nr3,ed,bt3bad,Bad aligned schedule,,0,,,1,2\nr4,ad,bt4a,Availability A,,0,,,1,2\nr4,ad,bt4b,Availability B,,0,,,1,2\n"
        write (Path.Combine(gtfs, "calendar_dates.txt")) "service_id,date,exception_type\nbd,20251215,1\nbd,20251216,1\ntd,20251215,1\ned,20251215,1\nad,20251215,1\nold,20240101,1\n"
        write (Path.Combine(gtfs, "feed_info.txt")) "feed_publisher_name,feed_publisher_url,feed_lang,feed_start_date,feed_end_date,feed_version,feed_contact_email\nNational,https://example.test,cs,20240101,20270101,base,\n"
        write (Path.Combine(ext, "cz_routes.txt")) "route_id,cis_line_id,public_line_number,source_provenance\nr1,100,100,jdf\nr1v,100,100,jdf\nr2,199010,10,jdf\nr3,101,101,jdf\nr4,102,102,jdf\n"
        write (Path.Combine(gtfs, "stop_times.txt")) "trip_id,arrival_time,departure_time,stop_id,stop_sequence,stop_headsign,pickup_type,drop_off_type,shape_dist_traveled,timepoint\nbt1,08:00:00,08:00:00,bp1,1,,0,0,,1\nbt1,08:05:00,08:05:00,bp4,2,,0,0,,1\nbt1,08:10:00,08:10:00,bp2,3,,0,0,,1\nbt1dup,08:00:00,08:00:00,bp1,1,,0,0,,1\nbt1dup,08:05:00,08:05:00,bp4,2,,0,0,,1\nbt1dup,08:10:00,08:10:00,bp2,3,,0,0,,1\nbt2,09:00:00,09:00:00,bp1,1,,0,0,,1\nbt2,09:10:00,09:10:00,bp2,2,,0,0,,1\nbt3good,10:00:00,10:00:00,bp1,1,,0,0,,1\nbt3good,10:05:00,10:05:00,bp4,2,,0,0,,1\nbt3good,10:06:00,10:06:00,bp3,3,,0,0,,1\nbt3good,10:10:00,10:10:00,bp2,4,,0,0,,1\nbt3bad,10:00:00,10:00:00,bp1,1,,0,0,,1\nbt3bad,10:02:00,10:02:00,bp3,2,,0,0,,1\nbt3bad,10:08:00,10:08:00,bp4,3,,0,0,,1\nbt3bad,10:10:00,10:10:00,bp2,4,,0,0,,1\nbt4a,11:00:00,11:00:00,bp1,1,,0,0,,1\nbt4a,11:10:00,11:10:00,bp2,2,,0,0,,1\nbt4b,11:10:00,11:10:00,bp1,1,,0,0,,1\nbt4b,11:20:00,11:20:00,bp2,2,,0,0,,1\n"
        write (Path.Combine(ext, "cz_trips.txt")) "trip_id,cis_line_id,cis_trip_id,train_number,source_trip_ids,coverage_sources\nbt1,100,1,,bt1,jdf\nbt1dup,100,1,,bt1dup,jdf\nbt2,,2,,bt2,jdf\nbt3good,101,3,,bt3good,jdf\nbt3bad,101,4,,bt3bad,jdf\nbt4a,102,5,,bt4a,jdf\nbt4b,102,6,,bt4b,jdf\n"
        write (Path.Combine(ext, "cz_stops.txt")) "stop_id,stop_place_id,cis_stop_id,post_id,asw_id,source_ids\nbp1,bp1,,,,bp1\nbp2,bp2,,,,bp2\nbp3,bp3,,,,bp3\nbp4,bp4,,,,bp4\n"

        let sourceDir = Path.Combine(root, "source")
        write (Path.Combine(sourceDir, "agency.txt")) "agency_id,agency_name,agency_url,agency_timezone\npid,PID,https://pid.cz,Europe/Prague\n"
        write (Path.Combine(sourceDir, "stops.txt")) "stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station,wheelchair_boarding,platform_code,asw_node_id,asw_stop_id\nsa,Alpha,50.0001,14.0001,0,,1,N,A1\nsb,Beta,50.0011,14.0011,0,,1,N,B1\nsg,Unrelated source label,51.0000,15.0000,0,,1,G,G1\nunused-node,Alpha,51.0000,15.0000,3,,0,,N,\n"
        let fixtureStops = Path.Combine(sourceDir, "stops.txt")
        File.ReadAllText(fixtureStops)
            .Replace("sa,Alpha,", "sa,City Alpha,")
            .Replace("Unrelated source label", "Gamma") |> write fixtureStops
        write (Path.Combine(sourceDir, "routes.txt")) "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\nsrbus,pid,X,PID bus,3,abcdef,000000\nsrtram,pid,10,PID tram,0,fedcba,111111\nsredit,pid,101,PID edited bus,3,aaaaaa,ffffff\nsravail,pid,102,PID availability bus,3,bbbbbb,ffffff\nsrrail,pid,R,Rail,2,999999,ffffff\n"
        write (Path.Combine(sourceDir, "trips.txt")) "route_id,service_id,trip_id,trip_headsign,trip_short_name,direction_id,shape_id,sub_agency_id\nsrbus,sd,st1_251201,PID headsign,PID short,1,sh1,sub1\nsrbus,sd,st1_251215,PID newer headsign,PID newer short,1,sh1,sub1\nsrtram,td,st2_251201,PID tram headsign,PID tram short,1,sh2,sub2\nsredit,ed,st3_251201,PID edited headsign,,0,,sub3\nsravail,ad,st4claim_251201,PID availability exact,,0,,sub4\nsravail,ad,st4amb_251201,PID availability ambiguous,,0,,sub4\nsrrail,rd,railtrip_251201,Rail,Rail,0,railshape,rail\n"
        write (Path.Combine(sourceDir, "stop_times.txt")) "trip_id,arrival_time,departure_time,stop_id,stop_sequence,pickup_type,drop_off_type,shape_dist_traveled\nst1_251201,08:03:00,08:03:00,sa,1,0,0,0\nst1_251201,08:08:00,08:08:00,sg,2,0,0,500\nst1_251201,08:13:00,08:13:00,sb,3,0,0,1000\nst1_251215,08:04:00,08:04:00,sa,1,0,0,0\nst1_251215,08:09:00,08:09:00,sg,2,0,0,500\nst1_251215,08:14:00,08:14:00,sb,3,0,0,1000\nst2_251201,09:00:45,09:00:45,sa,1,0,0,0\nst2_251201,09:10:05,09:10:05,sb,2,0,0,1000\nst3_251201,10:00:00,10:00:00,sa,1,0,0,0\nst3_251201,10:05:00,10:05:00,sg,2,0,0,500\nst3_251201,10:10:00,10:10:00,sb,3,0,0,1000\nst4claim_251201,11:00:00,11:00:00,sa,1,0,0,0\nst4claim_251201,11:10:00,11:10:00,sb,2,0,0,1000\nst4amb_251201,11:05:00,11:05:00,sa,1,0,0,0\nst4amb_251201,11:15:00,11:15:00,sb,2,0,0,1000\nrailtrip_251201,10:00:00,10:00:00,sa,1,0,0,0\nrailtrip_251201,10:10:00,10:10:00,sb,2,0,0,1000\n"
        write (Path.Combine(sourceDir, "calendar_dates.txt")) "service_id,date,exception_type\nsd,20251215,1\ntd,20251215,1\ned,20251215,1\nad,20251215,1\nrd,20251215,1\n"
        write (Path.Combine(sourceDir, "route_sub_agencies.txt")) "route_id,route_licence_number,sub_agency_id,sub_agency_name\nsrbus,100,sub1,One\nsrbus,999,sub9,Other licence\nsrtram,199010,sub2,Tram\nsredit,101,sub3,Edited\nsravail,102,sub4,Availability\nsrrail,777,rail,Rail\n"
        write (Path.Combine(sourceDir, "shapes.txt")) "shape_id,shape_pt_lat,shape_pt_lon,shape_pt_sequence,shape_dist_traveled\nsh1,50.0001,14.0001,1,0\nsh1,50.0005,14.0005,2,500\nsh1,50.0011,14.0011,3,1000\nsh2,50.0001,14.0001,1,0\nsh2,50.0012,14.0012,2,1000\nrailshape,50,14,1,0\nrailshape,51,15,2,1000\n"
        write (Path.Combine(sourceDir, "transfers.txt")) "from_stop_id,to_stop_id,transfer_type,min_transfer_time,from_trip_id,to_trip_id,max_waiting_time\nsb,sa,2,60,st1_251215,st2_251201,300\nsa,sb,2,30,,,120\nsa,sb,2,30,railtrip_251201,st1_251215,120\n"
        write (Path.Combine(sourceDir, "pathways.txt")) "pathway_id,from_stop_id,to_stop_id,pathway_mode,is_bidirectional\np,sa,sb,1,1\n"
        write (Path.Combine(sourceDir, "levels.txt")) "level_id,level_index\nl,0\n"
        let sourceZip = Path.Combine(root, "PID_GTFS.zip")
        ZipFile.CreateFromDirectory(sourceDir, sourceZip)
        let policy = Path.Combine(root, "policy.json")
        write policy policyJson
        write (Path.Combine(root, "stops.csv")) "source_namespace,source_id,target_namespace,target_id,valid_from,valid_to,review_note\npid-stop-group,group:B1:beta,jdf-stop-place-name,Beta,20251214,20261212,Stable target name test\n"
        let descriptor = Path.Combine(root, "descriptor.json")
        write descriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha sourceZip}\"}}")
        basePath, sourceZip, descriptor, policy

    [<TestMethod>]
    member _.``Headsigns resolve full names conservatively``() =
        Assert.AreEqual(Some "Praha,Černý Most", RegionalGtfsOverlay.resolveFullHeadsign "Černý Most" [| "Praha,Černý Most" |])
        Assert.AreEqual(Some "Smíchovské nádraží", RegionalGtfsOverlay.resolveFullHeadsign "Praha,Smíchovské nádraží" [| "Smíchovské nádraží" |])
        Assert.AreEqual(Some "Dobřichovice,železniční stanice", RegionalGtfsOverlay.resolveFullHeadsign "Dobřichovice,žel. st." [| "Dobřichovice,železniční stanice" |])
        Assert.AreEqual(Some "Libuš", RegionalGtfsOverlay.resolveFullHeadsign "Libuš" [| "Sídliště Libuš"; "Libuš" |])
        Assert.AreEqual(Some "Zahradní Město", RegionalGtfsOverlay.resolveFullHeadsign "Zahradní Město" [| "Nádraží Zahradní Město"; "Zahradní Město" |])
        Assert.AreEqual(Some "Obec B,Centrum", RegionalGtfsOverlay.resolveFullHeadsign "Centrum" [| "Obec A,Centrum"; "Obec B,Centrum" |])
        Assert.AreEqual(None, RegionalGtfsOverlay.resolveFullHeadsign "Budějovická" [| "Poliklinika Budějovická" |])
        Assert.AreEqual(None, RegionalGtfsOverlay.resolveFullHeadsign "Letiště / Airport ✈" [| "Praha,Letiště" |])
        Assert.AreEqual(None, RegionalGtfsOverlay.resolveFullHeadsign "Centrum přes Nádraží" [| "Centrum" |])

    [<TestMethod>]
    member _.``Detour base routes take source display fields but never line authority``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-detour-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let czRoutes = Path.Combine(basePath, "extensions", "cz_routes.txt")
            write czRoutes "route_id,cis_line_id,public_line_number,source_provenance,timetable_kind\nr1,100,100,jdf,regular\nr1v,100,100,jdf,detour\nr2,199010,10,jdf,regular\nr3,101,101,jdf,regular\nr4,102,102,jdf,regular\n"
            let json = System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText(policy))
            json.["source"].["trip_set_authority"].["modes"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"bus\"]")
            json.["source"].["trip_set_authority"].["source_native_modes"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"bus\"]")
            write policy (json.ToJsonString())
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            execute None policy 2026 binding basePath output |> ignore
            let routes = readGtfs output "routes.txt"
            let routeLine id =
                routes.Split('\n') |> Array.find (fun line -> line.StartsWith(id + ",", StringComparison.Ordinal) || line.StartsWith("\"" + id + "\",", StringComparison.Ordinal))
            Assert.IsTrue((routeLine "r1").Contains("abcdef"), "The regular route takes source display fields")
            Assert.IsFalse(
                routes.Contains("overlay:pid-gtfs:route:"),
                "A line with one regular and one detour route must not get a source-native route:
" + routes)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Overlay rejects a base bounded to another GVD``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-gvd-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            write (Path.Combine(basePath, "manifest.json")) """{"source_snapshot":{"retrieved_at":"2026-08-20T00:00:00+02:00"},"service_horizon":{"year":2025,"start_date":"20241215","end_date":"20251213"}}"""
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                execute None policy 2026 binding basePath (Path.Combine(root, "output")) |> ignore) |> ignore
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Overlay enriches non-rail facts and deterministically splits validity``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-test-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let binding: SourceBinding = {
                sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor
            }
            let inputHashes =
                Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
                |> Seq.map (fun path -> path, sha path)
                |> Seq.toArray
            let output1 = Path.Combine(root, "output-1")
            let result = execute None policy 2026 binding basePath output1
            Assert.AreEqual(5, result.matchedTrips)
            Assert.AreEqual(2, result.selectedShapes)
            // The fixture's reviewed override names the flat PID group "group:B1:beta".
            let betaMatch = File.ReadAllLines(diagnosticPath output1 "events" "stop_group_matches.csv") |> Array.find (fun line -> line.Contains("\"pid-gtfs:sb\""))
            Assert.IsTrue(betaMatch.Contains("reviewed_override"), betaMatch)
            Assert.IsTrue(File.ReadAllText(diagnosticPath output1 "events" "stop_context_inference.csv").Contains("trip_context_unique"))
            Assert.IsTrue(File.ReadAllText(diagnosticPath output1 "events" "trip_candidate_scores.csv").Contains("pattern_nearest"))
            let diagnosticEvents = File.ReadAllText(diagnosticPath output1 "events" "diagnostics.csv")
            Assert.IsTrue(diagnosticEvents.Contains("trip_equivalent_tie_expanded"))
            Assert.IsFalse(diagnosticEvents.Split('\n') |> Array.exists (fun line -> line.Contains("ambiguous") && line.Contains("st1_")))
            Assert.IsTrue(File.ReadAllText(diagnosticPath output1 "traces" "source_to_output_trips.csv").Contains("bt1dup"))
            let tripMappings = File.ReadAllText(diagnosticPath output1 "traces" "source_to_output_trips.csv")
            Assert.IsTrue(tripMappings.Contains("st3_251201") && tripMappings.Contains("bt3good"), "Aligned intermediate call times must resolve an edited-pattern tie")
            Assert.IsFalse(tripMappings.Split('\n') |> Array.exists (fun line -> line.Contains("st3_251201") && line.Contains("bt3bad")))
            Assert.IsTrue(tripMappings.Split('\n') |> Array.exists (fun line -> line.Contains("st4amb_251201") && line.Contains("bt4b")))
            Assert.IsTrue(File.ReadAllText(diagnosticPath output1 "events" "diagnostics.csv").Contains("trip_target_availability_resolved"))
            let baseTripMappings = File.ReadAllLines(diagnosticPath output1 "traces" "base_to_output_trips.csv") |> Array.skip 1
            Assert.AreEqual(
                baseTripMappings.Length,
                baseTripMappings
                |> Array.map (fun row ->
                    let fields = row.Split(',')
                    fields.[0], fields.[1])
                |> Array.distinct
                |> Array.length,
                "Each base/output slice must use one enclosing validity range instead of one mapping per service-date run")
            let trips = readGtfs output1 "trips.txt"
            Assert.IsFalse(trips.Contains("railtrip"))
            Assert.IsFalse(trips.Contains("PID headsign"))
            Assert.IsTrue(trips.Contains("National Alpha"))
            Assert.IsTrue(trips.Contains("Protected variant headsign"), "Equal-claim expansion must preserve target-specific national trip fields")
            Assert.AreEqual(2, trips.Split('\n') |> Array.filter (fun line -> line.Contains("bt1:overlay:")) |> Array.length)
            let stopTimes = readGtfs output1 "stop_times.txt"
            Assert.IsTrue(stopTimes.Contains("08:04:00"), "The newest overlapping source revision must provide authoritative times")
            Assert.IsFalse(stopTimes.Contains("08:03:00"), "An older overlapping source revision must be superseded")
            Assert.IsTrue(File.ReadAllText(diagnosticPath output1 "events" "diagnostics.csv").Contains("overlay_newer_source_selected"))
            let routes = readGtfs output1 "routes.txt"
            Assert.IsTrue(routes.Contains("PID bus"))
            Assert.IsTrue(routes.Contains("abcdef"))
            let stops = readGtfs output1 "stops.txt"
            Assert.IsTrue(stops.Contains("overlay:pid-gtfs:post:"))
            Assert.IsFalse(stops.Contains("PID headsign"))
            let inferredParent = stops.Split('\n') |> Array.find (fun line -> line.StartsWith("bp4,", StringComparison.Ordinal) || line.StartsWith("\"bp4\",", StringComparison.Ordinal))
            Assert.IsTrue(inferredParent.Contains("51") && inferredParent.Contains("15"), "A context-inferred stop place must receive the valid source centroid")
            let sourcePostIds =
                stops.Split('\n')
                |> Array.filter (fun line -> line.Contains("overlay:pid-gtfs:post:"))
                |> Array.map (fun line -> line.Split(',').[0].Trim('"'))
            Assert.IsTrue(sourcePostIds.Length > 0)
            for postId in sourcePostIds do
                Assert.IsTrue(stopTimes.Contains(postId), $"Source-created post {postId} must be referenced by an output call")
            Assert.IsTrue(File.ReadAllText(diagnosticPath output1 "events" "post_pruning.csv").Contains("disposition"))
            let transfers = readGtfs output1 "transfers.txt"
            Assert.IsFalse(transfers.Contains("max_waiting_time"))
            let waits = (JrUtil.Serving.PackageReader.readTextRows (Path.Combine(output1, "serving", "transfer.parquet")) [| "maximum_waiting_time" |] |> Seq.toArray)
            Assert.IsTrue(waits |> Array.exists (fun row -> row.[0] = "300"))
            Assert.IsFalse(File.Exists(Path.Combine(output1, "gtfs-intermediate", "pathways.txt")))
            Assert.IsFalse(File.Exists(Path.Combine(output1, "gtfs-intermediate", "levels.txt")))
            let diagnosticManifest = File.ReadAllText(Path.Combine(output1 + ".diagnostics", "manifest.json"))
            Assert.IsTrue(diagnosticManifest.Contains("base-package"), "Diagnostics must reference base evidence by digest instead of copying it")
            use manifest = JsonDocument.Parse(File.ReadAllText(Path.Combine(output1, "manifest.json")))
            Assert.IsTrue(manifest.RootElement.GetProperty("publication_eligible").GetBoolean())
            Assert.AreEqual("20251214", manifest.RootElement.GetProperty("service_horizon").GetProperty("start_date").GetString())
            Assert.AreEqual("20261212", manifest.RootElement.GetProperty("service_horizon").GetProperty("end_date").GetString())

            let output2 = Path.Combine(root, "output-2")
            execute None policy 2026 binding basePath output2 |> ignore
            let files1 = Directory.EnumerateFiles(output1, "*", SearchOption.AllDirectories) |> Seq.map (fun path -> Path.GetRelativePath(output1, path)) |> Seq.sort |> Seq.toArray
            let files2 = Directory.EnumerateFiles(output2, "*", SearchOption.AllDirectories) |> Seq.map (fun path -> Path.GetRelativePath(output2, path)) |> Seq.sort |> Seq.toArray
            CollectionAssert.AreEqual(files1, files2)
            for relative in files1 do CollectionAssert.AreEqual(File.ReadAllBytes(Path.Combine(output1, relative)), File.ReadAllBytes(Path.Combine(output2, relative)), relative)
            for path, expected in inputHashes do Assert.AreEqual(expected, sha path, path)
            Assert.ThrowsExactly<ArgumentException>(fun () -> execute None policy 2026 binding basePath output1 |> ignore) |> ignore
            for relative in files1 do CollectionAssert.AreEqual(File.ReadAllBytes(Path.Combine(output1, relative)), File.ReadAllBytes(Path.Combine(output2, relative)), relative)

        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Projected transfer revisions merge conservatively or are quarantined``() =
        let transfer fromTrip toTrip transferType minimum maximum =
            let row = Collections.Generic.Dictionary<string, string>(StringComparer.Ordinal)
            for column, value in [ "from_stop_id", "A"; "to_stop_id", "B"; "from_route_id", ""; "to_route_id", ""
                                   "from_trip_id", fromTrip; "to_trip_id", toTrip; "transfer_type", transferType
                                   "min_transfer_time", minimum; "max_waiting_time", maximum ] do
                row.[column] <- value
            row
        let rows = [|
            // PID revisions 260901/261001 of one output trip: wait shortened from 600 s to 300 s.
            transfer "t1" "t2" "1" "" "600"
            transfer "t1" "t2" "1" "" "300"
            transfer "t1" "t2" "1" "" "600"
            // Exact duplicates collapse silently.
            transfer "t3" "t4" "2" "120" ""
            transfer "t3" "t4" "2" "120" ""
            // Revisions of one minimum-time transfer keep the longest walk.
            transfer "t5" "t6" "2" "120" ""
            transfer "t5" "t6" "2" "180" ""
            // Different transfer types have no safe merge.
            transfer "t7" "t8" "1" "" "300"
            transfer "t7" "t8" "3" "" "" |]
        let reconciled, diagnostics = JrUtil.RegionalOverlay.Support.reconcileTransferRows rows
        let byTrips =
            reconciled
            |> Array.map (fun row -> (row.["from_trip_id"], row.["to_trip_id"]), row)
            |> Map.ofArray
        Assert.AreEqual(3, reconciled.Length)
        Assert.AreEqual("300", byTrips.[("t1", "t2")].["max_waiting_time"])
        Assert.AreEqual("", byTrips.[("t1", "t2")].["min_transfer_time"])
        Assert.AreEqual("120", byTrips.[("t3", "t4")].["min_transfer_time"])
        Assert.AreEqual("180", byTrips.[("t5", "t6")].["min_transfer_time"])
        Assert.IsFalse(byTrips.ContainsKey(("t7", "t8")))
        let codes = diagnostics |> Array.map (fun (code, _, _) -> code)
        CollectionAssert.AreEqual(
            [| "transfer_date_variants_merged"; "transfer_date_variants_merged"; "transfer_conflict_quarantined" |], codes)
        let keys = reconciled |> Array.map (fun row -> JrUtil.RegionalOverlay.Support.TransferSelectorColumns |> Array.map (fun column -> row.[column]))
        Assert.AreEqual(keys.Length, keys |> Array.distinct |> Array.length)
        // Input order does not change the result.
        let reversed, reversedDiagnostics = JrUtil.RegionalOverlay.Support.reconcileTransferRows (Array.rev rows)
        CollectionAssert.AreEqual(
            reconciled |> Array.map (fun row -> String.Join("|", row.Values)),
            reversed |> Array.map (fun row -> String.Join("|", row.Values)))
        CollectionAssert.AreEqual(diagnostics, reversedDiagnostics)

    [<TestMethod>]
    member _.``Overlay refuses checksum mismatch without activating output``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-failure-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            write descriptor "{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"deadbeef\"}"
            let output = Path.Combine(root, "output")
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            Assert.ThrowsExactly<InvalidOperationException>(fun () -> execute None policy 2026 binding basePath output |> ignore) |> ignore
            Assert.IsFalse(Directory.Exists(output))
            Assert.AreEqual(0, Directory.EnumerateDirectories(root, ".output.tmp-*", SearchOption.TopDirectoryOnly) |> Seq.length)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Trip-set authority emits complete source pattern and removes stale national instance``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-authority-test-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let sourceStopTimes = Path.Combine(root, "source", "stop_times.txt")
            let divergentCalls =
                "st2_251201,09:00:45,09:00:45,sa,1,0,0,0\n" +
                "st2_251201,09:02:00,09:02:00,sg,2,0,0,250\n" +
                "st2_251201,09:04:00,09:04:00,sg,3,0,0,500\n" +
                "st2_251201,09:06:00,09:06:00,sg,4,0,0,750\n" +
                "st2_251201,09:10:05,09:10:05,sb,5,0,0,1000"
            File.ReadAllText(sourceStopTimes)
                .Replace("st2_251201,09:00:45,09:00:45,sa,1,0,0,0\nst2_251201,09:10:05,09:10:05,sb,2,0,0,1000", divergentCalls)
                .TrimEnd() + "\n" + divergentCalls.Replace("st2_251201", "st2dup_251201") + "\n"
            |> write sourceStopTimes
            let sourceTrips = Path.Combine(root, "source", "trips.txt")
            File.ReadAllText(sourceTrips)
                .Replace(
                    "srtram,td,st2_251201,PID tram headsign,PID tram short,1,sh2,sub2",
                    "srtram,td,st2_251201,PID tram headsign,PID tram short,1,sh2,sub2\n" +
                    "srtram,td,st2dup_251201,PID tram headsign,PID tram short,1,sh2,sub2")
            |> write sourceTrips
            File.Delete(sourceZip)
            ZipFile.CreateFromDirectory(Path.Combine(root, "source"), sourceZip)
            write descriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha sourceZip}\"}}")
            File.ReadAllText(policy).Replace("\"modes\": []", "\"modes\": [\"tram\"]") |> write policy
            let binding: SourceBinding = {
                sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor
            }
            let output = Path.Combine(root, "output")
            let result = execute None policy 2026 binding basePath output
            let tripMappings = File.ReadAllText(diagnosticPath output "traces" "source_to_output_trips.csv")
            Assert.AreEqual(6, result.matchedTrips)
            let additionMapping =
                tripMappings.Split('\n')
                |> Array.find (fun line -> line.Contains("st2_251201") && line.Contains("authoritative_source_trip_set"))
            let additionTripId = additionMapping.Split(',').[3].Trim('"')
            Assert.IsTrue(
                tripMappings.Split('\n')
                |> Array.exists (fun line -> line.Contains("st2dup_251201") && line.Contains(additionTripId)),
                "Equivalent authoritative source trips must map to one output trip")
            let unmatchedReasons = File.ReadAllText(diagnosticPath output "events" "unmatched_trip_reasons.csv")
            Assert.IsTrue(
                unmatchedReasons.Split('\n')
                |> Array.filter (fun line -> line.Contains("\"st2_251201\"", StringComparison.Ordinal))
                |> Array.forall (fun line -> not (line.Contains("\"withheld\"", StringComparison.Ordinal))),
                "An authoritative source addition must not be classified as withheld")
            Assert.IsTrue(
                File.ReadAllText(Path.Combine(output, "diagnostics.json")).Contains("\"trip\"", StringComparison.Ordinal),
                "Authoritative additions must reach coverage report generation")
            Assert.IsFalse((readGtfs output "trips.txt").Split('\n') |> Array.exists (fun line -> line.Contains(",bt2,")))
            let additionCalls =
                (readGtfs output "stop_times.txt").Split('\n')
                |> Array.filter (fun line -> line.StartsWith(additionTripId + ",", StringComparison.Ordinal) || line.StartsWith("\"" + additionTripId + "\",", StringComparison.Ordinal))
            Assert.AreEqual(5, additionCalls.Length)
            Assert.IsTrue(additionCalls |> Array.exists (fun line -> line.Contains("09:04:00")))
            Assert.IsTrue(File.ReadAllText(diagnosticPath output "events" "source_trip_additions.csv").Contains("st2_251201"))
            let czTrip =
                File.ReadAllText(diagnosticPath output "projection/czech" "cz_trips.txt").Split('\n')
                |> Array.find (fun line -> line.StartsWith(additionTripId + ",", StringComparison.Ordinal) || line.StartsWith("\"" + additionTripId + "\",", StringComparison.Ordinal))
            Assert.IsTrue(czTrip.Contains("\"199010\",\"\",\"\""), $"A PID-native pattern must not inherit a fabricated CIS trip identity: {czTrip}")
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<DataTestMethod>]
    [<DataRow("ferry", "4", "199691")>]
    [<DataRow("tram", "0", "199010")>]
    [<DataRow("trolleybus", "11", "199010")>]
    member _.``Source-native mode imports route agency and unmatched stop place``(mode: string, routeType: string, cis: string) =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-native-route-test-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let sourceDir = Path.Combine(root, "source")
            let routesPath = Path.Combine(sourceDir, "routes.txt")
            if mode = "trolleybus" then
                let nationalRoutes = Path.Combine(basePath, "gtfs-intermediate", "routes.txt")
                File.ReadAllText(nationalRoutes).Replace("National tram,,900", "National bus,,704") |> write nationalRoutes
            File.ReadAllText(routesPath).Replace("srtram,pid,10,PID tram,0", $"srtram,pid,P1,PID ferry,{routeType}") |> write routesPath
            let stopsPath = Path.Combine(sourceDir, "stops.txt")
            File.ReadAllText(stopsPath).TrimEnd() + "\nsf,Ferry Island,49.9000,14.4000,0,,1,1,F,F1\n" |> write stopsPath
            let stopTimesPath = Path.Combine(sourceDir, "stop_times.txt")
            File.ReadAllText(stopTimesPath).Replace("st2_251201,09:10:05,09:10:05,sb,2,0,0,1000", "st2_251201,09:10:05,09:10:05,sf,2,0,0,1000") |> write stopTimesPath
            File.ReadAllLines(stopTimesPath)
            |> Array.mapi (fun index line -> line + (if index = 0 then ",stop_headsign" elif line.StartsWith("st2_251201,") then ",Ferry Island" else ","))
            |> String.concat "\n"
            |> write stopTimesPath
            let joinsPath = Path.Combine(sourceDir, "route_sub_agencies.txt")
            File.ReadAllText(joinsPath).Replace("srtram,199010,sub2,Tram", $"srtram,{cis},sub2,Ferry") |> write joinsPath
            let tripsPath = Path.Combine(sourceDir, "trips.txt")
            File.ReadAllText(tripsPath).Replace("PID tram headsign", "Ferry Island") |> write tripsPath
            File.Delete(sourceZip)
            ZipFile.CreateFromDirectory(sourceDir, sourceZip)
            write descriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha sourceZip}\"}}")
            File.ReadAllText(policy)
                .Replace("\"modes\": []", $"\"modes\": [\"{mode}\"]")
                .Replace("\"source_native_modes\": []", $"\"source_native_modes\": [\"{mode}\"]")
                .Replace("\"enabled\": true", "\"enabled\": false")
            |> write policy
            let binding: SourceBinding = {
                sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor
            }
            let output = Path.Combine(root, "output")
            let result = execute (Some (NodaTime.LocalDate(2025, 12, 15))) policy 2026 binding basePath output
            Assert.AreEqual(3, result.matchedTrips, "Context inference is disabled in this native-stop fixture")
            let routeMapping =
                File.ReadAllText(diagnosticPath output "traces" "source_to_output_routes.csv").Split('\n')
                |> Array.find (fun line -> line.Contains("srtram") && line.Contains("authoritative_source_trip_set"))
            let outputRouteId = routeMapping.Split(',').[2].Trim('"')
            Assert.IsTrue(mode = "tram" || outputRouteId.StartsWith("overlay:pid-gtfs:route:", StringComparison.Ordinal))
            let routes = readGtfs output "routes.txt"
            Assert.IsTrue(routes.Contains(outputRouteId))
            if mode <> "tram" then Assert.IsTrue((readGtfs output "agency.txt").Contains("overlay:pid-gtfs:agency:"))
            Assert.IsTrue((readGtfs output "trips.txt").Contains("Ferry Island"))
            Assert.IsTrue((readGtfs output "stop_times.txt").Contains("Ferry Island"))
            Assert.IsTrue(File.ReadAllText(diagnosticPath output "events" "headsigns.csv").Contains("Island"))
            Assert.IsTrue(File.ReadAllText(Path.Combine(output, "diagnostics.json")).Contains("audit_day"))
            Assert.IsTrue((readGtfs output "stops.txt").Contains("overlay:pid-gtfs:stop-place:"))
            let czRoutes = File.ReadAllText(diagnosticPath output "projection/czech" "cz_routes.txt")
            Assert.IsTrue(czRoutes.Contains(outputRouteId) && czRoutes.Contains(cis))
            let stopMappings = File.ReadAllText(diagnosticPath output "traces" "source_to_output_stops.csv")
            Assert.IsTrue(stopMappings.Split('\n') |> Array.exists (fun line -> line.Contains("\"sf\"") && line.Contains("source_native")))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Authoritative service deduplicates baseline variants and scopes semantic evidence``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-dedup-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            File.ReadAllText(policy)
                .Replace("\"modes\": []", "\"modes\": [\"bus\"]")
                .Replace("\"source_native_modes\": []", "\"source_native_modes\": [\"bus\"]") |> write policy
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            execute None policy 2026 binding basePath output |> ignore
            let mappings = File.ReadAllLines(diagnosticPath output "traces" "source_to_output_trips.csv")
            let selected = mappings |> Array.filter (fun row -> row.Contains("\"st1_251215\""))
            Assert.AreEqual(1, selected.Length, "One PID trip must not inherit two baseline instances")
            let noteAssignments =
                JrUtil.Serving.PackageReader.readTextRows
                    (Path.Combine(output, "serving", "service_note_assignment.parquet")) [|"assignment_id"|]
                |> Seq.length
            Assert.AreEqual(0, noteAssignments, "Snapshot-only evidence must not become an unbounded semantic assignment")
            Assert.IsTrue(File.ReadAllText(Path.Combine(output + ".diagnostics", "manifest.json")).Contains("base-package"))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Matched trolleybus slice retains its source-mode route``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-mode-route-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let nationalRoutes = Path.Combine(basePath, "gtfs-intermediate", "routes.txt")
            File.ReadAllText(nationalRoutes).Replace("National tram,,900", "National bus,,704") |> write nationalRoutes
            let sourceDir = Path.Combine(root, "source")
            let sourceRoutes = Path.Combine(sourceDir, "routes.txt")
            File.ReadAllText(sourceRoutes).Replace("PID tram,0", "PID trolleybus,11") |> write sourceRoutes
            File.Delete(sourceZip)
            ZipFile.CreateFromDirectory(sourceDir, sourceZip)
            write descriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha sourceZip}\"}}")
            File.ReadAllText(policy)
                .Replace("\"modes\": []", "\"modes\": [\"trolleybus\"]")
                .Replace("\"source_native_modes\": []", "\"source_native_modes\": [\"trolleybus\"]") |> write policy
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            execute None policy 2026 binding basePath output |> ignore
            let trips = (readGtfs output "trips.txt").Split('\n')
            let trip = trips |> Array.find (fun row -> row.Contains("\"bt2\""))
            let routeId = trip.Split(',').[0].Trim('"')
            Assert.IsTrue(routeId.StartsWith("overlay:pid-gtfs:route:"))
            let routes = (readGtfs output "routes.txt").Split('\n')
            Assert.IsTrue(routes |> Array.exists (fun row -> row.Contains(routeId) && row.Contains("\"11\"")), "Every mode-split trip route must survive pruning")
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Configured radius finds a unique stop beyond the old 125 metre limit``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-radius-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let baseStops = Path.Combine(basePath, "gtfs-intermediate", "stops.txt")
            File.ReadAllText(baseStops).Replace("bp2,,Beta,,50.0010", "bp2,,Beta,,50.0030") |> write baseStops
            write (Path.Combine(root, "stops.csv")) "source_namespace,source_id,target_namespace,target_id,valid_from,valid_to,review_note\n"
            File.ReadAllText(policy).Replace("\"maximum_distance_metres\": 125", "\"maximum_distance_metres\": 300") |> write policy
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            execute None policy 2026 binding basePath output |> ignore
            let report = File.ReadAllText(diagnosticPath output "events" "stop_group_matches.csv")
            Assert.IsTrue(report.Split('\n') |> Array.exists (fun row -> row.Contains("\"pid-gtfs:sb\"") && row.Contains("name_geo_unique")))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Wider radius does not merge a stop with a different facility prefix``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-prefix-collision-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let baseStops = Path.Combine(basePath, "gtfs-intermediate", "stops.txt")
            File.ReadAllText(baseStops).Replace("bp4,,Gamma,,50.0005,14.0005", "bp4,,Poliklinika Budějovická,,50.0005,14.0005") |> write baseStops
            let sourceDir = Path.Combine(root, "source")
            let sourceStops = Path.Combine(sourceDir, "stops.txt")
            File.ReadAllText(sourceStops).Replace("sg,Gamma,51.0000,15.0000", "sg,Budějovická,50.0005,14.0005") |> write sourceStops
            File.Delete(sourceZip)
            ZipFile.CreateFromDirectory(sourceDir, sourceZip)
            write descriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha sourceZip}\"}}")
            File.ReadAllText(policy)
                .Replace("\"maximum_distance_metres\": 125", "\"maximum_distance_metres\": 300")
                .Replace("\"modes\": []", "\"modes\": [\"bus\"]")
                .Replace("\"source_native_modes\": []", "\"source_native_modes\": [\"bus\"]") |> write policy
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            execute None policy 2026 binding basePath output |> ignore
            let row = File.ReadAllLines(diagnosticPath output "events" "stop_group_matches.csv") |> Array.find (fun line -> line.Contains("\"pid-gtfs:sg\""))
            Assert.IsTrue(row.Contains("source_native") && not (row.Contains("jdf:stop:bp4")), row)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``PID name and geodata replace a context-matched approximate JDF stop``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-approximate-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let baseStops = Path.Combine(basePath, "gtfs-intermediate", "stops.txt")
            File.ReadAllText(baseStops).Replace("bp2,,Beta,,50.0010,14.0010", "bp2,,Beta [?],,49.0000,13.0000") |> write baseStops
            write (Path.Combine(root, "stops.csv")) "source_namespace,source_id,target_namespace,target_id,valid_from,valid_to,review_note\n"
            File.ReadAllText(policy).Replace("\"modes\": []", "\"modes\": [\"tram\"]") |> write policy
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            execute None policy 2026 binding basePath output |> ignore
            let stop = (readGtfs output "stops.txt").Split('\n') |> Array.find (fun row -> row.StartsWith("bp2,", StringComparison.Ordinal) || row.StartsWith("\"bp2\",", StringComparison.Ordinal))
            Assert.IsTrue(stop.Contains("Beta") && not (stop.Contains("[?]")) && stop.Contains("50.0011") && stop.Contains("14.0011"), stop)
            let report = File.ReadAllText(diagnosticPath output "events" "stop_group_matches.csv")
            Assert.IsTrue(report.Contains("gtfs_authoritative") && report.Contains("pid_name_and_coordinates"))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<DataTestMethod>]
    [<DataRow(false)>]
    [<DataRow(true)>]
    member _.``Generic feed needs neither companion table nor revision identifiers``(authority: bool) =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-generic-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            let json = System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText(policy))
            let source = json.["source"]
            source.["source_id"] <- System.Text.Json.Nodes.JsonValue.Create("generic-gtfs")
            source.AsObject().Remove("route_join") |> ignore
            source.["trip_match"].AsObject().Remove("source_revision") |> ignore
            source.["stop_match"].AsObject().Remove("group_column") |> ignore
            source.["stop_match"].AsObject().Remove("post_column") |> ignore
            source.["overrides"].["stops"] <- System.Text.Json.Nodes.JsonValue.Create("")
            if authority then
                source.["trip_set_authority"].["modes"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"bus\",\"tram\"]")
                source.["trip_set_authority"].["source_native_modes"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"bus\",\"tram\"]")
            write policy (json.ToJsonString())
            let sourceDir = Path.Combine(root, "source")
            File.Delete(Path.Combine(sourceDir, "route_sub_agencies.txt"))
            write (Path.Combine(sourceDir, "stops.txt")) "stop_id,stop_name,stop_lat,stop_lon,location_type,parent_station\nparent,City Alpha,50.0001,14.0001,1,\nsa,City Alpha,50.0001,14.0001,0,parent\nsb,Beta,50.0011,14.0011,0,\nsg,Gamma,51.0000,15.0000,0,\n"
            let routes = Path.Combine(sourceDir, "routes.txt")
            write routes (File.ReadAllText(routes).Replace("srbus,pid,X,", "srbus,pid,100,"))
            for table in [ "trips.txt"; "stop_times.txt"; "transfers.txt" ] do
                let path = Path.Combine(sourceDir, table)
                write path (File.ReadAllText(path).Replace("_251201", "-first").Replace("_251215", "-second"))
            File.Delete(sourceZip)
            ZipFile.CreateFromDirectory(sourceDir, sourceZip)
            write descriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha sourceZip}\"}}")
            let binding: SourceBinding = { sourceId = "generic-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            let result = execute None policy 2026 binding basePath output
            Assert.IsTrue(result.matchedTrips > 0, "Structural matching must work without proprietary columns")
            let diagnostics = File.ReadAllText(diagnosticPath output "events" "diagnostics.csv")
            Assert.IsTrue(diagnostics.Contains("overlay_fact_conflict"), "Conflicting unversioned claims must be quarantined")
            Assert.IsFalse(diagnostics.Contains("source_revision_unresolved"))
            Assert.IsFalse(diagnostics.Contains("overlay_newer_source_selected"))
            let calls = readGtfs output "stop_times.txt"
            Assert.IsFalse(calls.Contains("08:03:00") || calls.Contains("08:04:00"))
            let additions = File.ReadAllLines(diagnosticPath output "events" "source_trip_additions.csv")
            Assert.AreEqual((if authority then 2 else 1), additions.Length, "Structural route/date proof may authorize the configured complete regional trip set without fabricating a CIS trip identity")
            Assert.IsTrue((readGtfs output "stops.txt").Contains("overlay:generic-gtfs:post:"))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Configured companion join validates namespace and requires its table``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-join-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            write policy (policyJson.Replace("\"target_namespace\": \"cis_line_id\"", "\"target_namespace\": \"invented\""))
            Assert.ThrowsExactly<ArgumentException>(fun () -> RegionalGtfsOverlay.loadPolicy policy |> ignore) |> ignore
            write policy policyJson
            File.Delete(Path.Combine(root, "source", "route_sub_agencies.txt"))
            File.Delete(sourceZip)
            ZipFile.CreateFromDirectory(Path.Combine(root, "source"), sourceZip)
            write descriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha sourceZip}\"}}")
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            Assert.ThrowsExactly<InvalidOperationException>(fun () -> execute None policy 2026 binding basePath output |> ignore) |> ignore
            Assert.IsFalse(Directory.Exists(output))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<DataTestMethod>]
    [<DataRow(false)>]
    [<DataRow(true)>]
    member _.``Equal or unresolved revisions quarantine conflicting schedules``(unresolved: bool) =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-revision-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, sourceZip, descriptor, policy = makeFixture root
            for table in [ "trips.txt"; "stop_times.txt"; "transfers.txt" ] do
                let path = Path.Combine(root, "source", table)
                let text = File.ReadAllText(path)
                let text =
                    if unresolved then text.Replace("st1_251201", "st1_first").Replace("st1_251215", "st1_second")
                    else text.Replace("st1_251215", "st1new_251201")
                write path text
            File.Delete(sourceZip)
            ZipFile.CreateFromDirectory(Path.Combine(root, "source"), sourceZip)
            write descriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha sourceZip}\"}}")
            let binding: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = sourceZip; descriptorPath = descriptor }
            let output = Path.Combine(root, "output")
            execute None policy 2026 binding basePath output |> ignore
            let diagnostics = File.ReadAllText(diagnosticPath output "events" "diagnostics.csv")
            Assert.IsTrue(diagnostics.Contains("overlay_fact_conflict"))
            // The PID adapter dates an unparseable revision 700101, so both
            // cases are equal revisions rather than unresolved ones.
            Assert.IsFalse(diagnostics.Contains("source_revision_unresolved"))
            Assert.IsFalse(diagnostics.Contains("overlay_newer_source_selected"))
            let calls = readGtfs output "stop_times.txt"
            Assert.IsFalse(calls.Contains("08:03:00") || calls.Contains("08:04:00"))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Czech stop-name abbreviations match their verbose IDS JMK forms``() =
        let rank left right = JrUtil.RegionalOverlay.Names.stopNameMatchRank left right
        Assert.IsTrue(rank "Ochoz u Brna, obecní úřad" "Ochoz u Brna,ObÚ" |> Option.isSome)
        Assert.IsTrue(rank "Bílovice nad Svitavou, železniční stanice" "Bílovice n.Svit.,žel.st." |> Option.isSome)
        Assert.IsTrue(rank "Moravský Krumlov, náměstí" "Moravský Krumlov,nám." |> Option.isSome)
        Assert.IsTrue(rank "Svatobořice-Mistřín, restaurace" "Svatobořice-Mistřín,rest." |> Option.isSome)
        Assert.IsTrue(rank "Ochoz u Brna, obecní úřad" "Ochoz u Brna, železniční stanice" |> Option.isNone)
        Assert.IsTrue(JrUtil.RegionalOverlay.Names.compatibleModeClasses "trolleybus" "bus")
        Assert.IsTrue(JrUtil.RegionalOverlay.Names.structurallyCompatibleRouteLabels "H4" "H")
        Assert.IsFalse(JrUtil.RegionalOverlay.Names.structurallyCompatibleRouteLabels "25" "2")

    [<TestMethod>]
    member _.``Combined PID and IDS JMK overlay is order independent and preserves JMK metadata``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-overlay-all-" + Guid.NewGuid().ToString("N"))
        try
            let basePath, pidZip, pidDescriptor, pidPolicy = makeFixture root
            write (Path.Combine(basePath, "extensions", "cz_stop_zones.txt")) "stop_place_id,zone_id,route_id,ids_system_id,source_provenance\n"
            let pidJson = System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText(pidPolicy))
            pidJson.["source"].["trip_set_authority"].["modes"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"trolleybus\"]")
            pidJson.["source"].["trip_set_authority"].["source_native_modes"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"trolleybus\"]")
            write pidPolicy (pidJson.ToJsonString())

            let jmkDir = Path.Combine(root, "jmk")
            write (Path.Combine(jmkDir, "agency.txt")) "agency_id,agency_name,agency_url,agency_timezone\njmk,IDS JMK,https://www.idsjmk.cz,Europe/Prague\n"
            write (Path.Combine(jmkDir, "stops.txt")) "stop_id,stop_name,stop_lat,stop_lon,zone_id,location_type,parent_station,platform_code\npa,City Alpha,50.0001,14.0001,100,1,,\nja,City Alpha,50.0001,14.0001,100,0,pa,N\npb,\"Beta, verbose regional name\",50.0011,14.0011,101,1,,\njb,\"Beta, verbose regional name\",50.0011,14.0011,101,0,pb,N\n"
            write (Path.Combine(jmkDir, "routes.txt")) "route_id,agency_id,route_short_name,route_long_name,route_type,route_color,route_text_color\njtram,jmk,10,JMK tram,0,ff0000,ffffff\njferry,jmk,F,JMK ferry,4,0000ff,ffffff\njtrolley,jmk,T,JMK trolleybus,800,00aa00,ffffff\njrail,jmk,R,JMK rail,2,333333,ffffff\n"
            write (Path.Combine(jmkDir, "trips.txt")) "route_id,service_id,trip_id,trip_headsign,direction_id\njtram,jd1,j1,Beta,0\njtram,jd2,j2,Beta,0\njferry,jd1,jf,Beta,0\njtrolley,jd1,jt,Beta,0\njrail,jd1,jr,Beta,0\n"
            write (Path.Combine(jmkDir, "stop_times.txt")) "trip_id,arrival_time,departure_time,stop_id,stop_sequence,pickup_type,drop_off_type\nj1,09:00:45,09:00:45,ja,1,0,0\nj1,09:10:05,09:10:05,jb,2,0,0\nj2,09:00:45,09:00:45,ja,1,0,0\nj2,09:10:05,09:10:05,jb,2,0,0\njf,12:00:00,12:00:00,ja,1,0,0\njf,12:10:00,12:10:00,jb,2,0,0\njt,12:30:00,12:30:00,ja,1,0,0\njt,12:40:00,12:40:00,jb,2,0,0\njr,13:00:00,13:00:00,ja,1,0,0\njr,13:10:00,13:10:00,jb,2,0,0\n"
            write (Path.Combine(jmkDir, "calendar_dates.txt")) "service_id,date,exception_type\njd1,20251215,1\njd2,20251216,1\n"
            write (Path.Combine(jmkDir, "transfers.txt")) "from_stop_id,to_stop_id,transfer_type,min_transfer_time,from_trip_id,to_trip_id\njb,ja,2,90,j1,jf\n"
            write (Path.Combine(jmkDir, "api.txt")) "Linka/CVlaku = trip_id: 10/42 = j1\nLinka/CVlaku = trip_id: 10/42 = j2\nLinka/CVlaku = trip_id: F/7 = jf\nLinka/CVlaku = trip_id: T/9 = jt\nLinka/CVlaku = trip_id: R/8 = jr\n"
            let jmkZip = Path.Combine(root, "IDSJMK_GTFS.zip")
            ZipFile.CreateFromDirectory(jmkDir, jmkZip)
            let jmkDescriptor = Path.Combine(root, "jmk-descriptor.json")
            write jmkDescriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha jmkZip}\"}}")

            let jmkPolicy = Path.Combine(root, "jmk-policy.json")
            let jmkJson = System.Text.Json.Nodes.JsonNode.Parse(policyJson)
            let jmkSource = jmkJson.["source"]
            jmkSource.["source_id"] <- System.Text.Json.Nodes.JsonValue.Create("ids-jmk-gtfs")
            jmkSource.["route_join"] <- null
            jmkSource.["stop_match"].["group_column"] <- System.Text.Json.Nodes.JsonValue.Create("")
            jmkSource.["stop_match"].["post_column"] <- System.Text.Json.Nodes.JsonValue.Create("")
            jmkSource.["stop_match"].["maximum_distance_metres"] <- System.Text.Json.Nodes.JsonValue.Create(300)
            jmkSource.["stop_match"].["coordinate_identity_maximum_metres"] <- System.Text.Json.Nodes.JsonValue.Create(25)
            jmkSource.["stop_match"].["coordinate_identity_minimum_margin_metres"] <- System.Text.Json.Nodes.JsonValue.Create(10)
            jmkSource.["trip_match"].["source_revision"] <- null
            jmkSource.["trip_set_authority"].["modes"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"tram\",\"ferry\",\"trolleybus\"]")
            jmkSource.["trip_set_authority"].["source_native_modes"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"ferry\"]")
            jmkSource.["capabilities"] <- System.Text.Json.Nodes.JsonNode.Parse("[\"stop_coordinates\",\"boarding_points\",\"call_boarding_points\",\"route_short_name\",\"route_long_name\",\"route_color\",\"route_text_color\",\"transfers\",\"stop_zones\",\"schedules\"]")
            jmkSource.["overrides"] <- System.Text.Json.Nodes.JsonNode.Parse("{\"stops\":\"\"}")
            write jmkPolicy (jmkJson.ToJsonString())
            let combinedPolicy = Path.Combine(root, "combined-policy.json")
            write combinedPolicy ($"{{\"schema_version\":1,\"sources\":[{{\"source_id\":\"pid-gtfs\",\"policy\":\"{Path.GetFileName(pidPolicy)}\",\"adapter\":\"pid-v1\"}},{{\"source_id\":\"ids-jmk-gtfs\",\"policy\":\"{Path.GetFileName(jmkPolicy)}\",\"adapter\":\"ids-jmk-v1\"}}]}}")

            let pid: SourceBinding = { sourceId = "pid-gtfs"; payloadPath = pidZip; descriptorPath = pidDescriptor }
            let jmk: SourceBinding = { sourceId = "ids-jmk-gtfs"; payloadPath = jmkZip; descriptorPath = jmkDescriptor }
            let output1 = Path.Combine(root, "combined-1")
            let output2 = Path.Combine(root, "combined-2")
            let result = executeAll combinedPolicy 2026 [| pid; jmk |] basePath output1
            executeAll combinedPolicy 2026 [| jmk; pid |] basePath output2 |> ignore
            CollectionAssert.AreEqual([| "ids-jmk-gtfs"; "pid-gtfs" |], result.sources)
            let files1 = Directory.EnumerateFiles(output1, "*", SearchOption.AllDirectories) |> Seq.map (fun path -> Path.GetRelativePath(output1, path)) |> Seq.sort |> Seq.toArray
            let files2 = Directory.EnumerateFiles(output2, "*", SearchOption.AllDirectories) |> Seq.map (fun path -> Path.GetRelativePath(output2, path)) |> Seq.sort |> Seq.toArray
            CollectionAssert.AreEqual(files1, files2)
            for relative in files1 do CollectionAssert.AreEqual(File.ReadAllBytes(Path.Combine(output1, relative)), File.ReadAllBytes(Path.Combine(output2, relative)), relative)
            use manifest = JsonDocument.Parse(File.ReadAllText(Path.Combine(output1, "manifest.json")))
            Assert.AreEqual(2, manifest.RootElement.GetProperty("bundle_version").GetInt32())
            Assert.AreEqual(3, manifest.RootElement.GetProperty("serving_schema_version").GetInt32())
            // The production base contributes its own source.
            Assert.AreEqual(3, manifest.RootElement.GetProperty("sources").GetArrayLength())
            JrUtil.Serving.Validation.validatePackage output1 |> ignore
            let mappings = File.ReadAllText(diagnosticPath output1 "traces" "operational_to_source_trips.csv")
            Assert.IsTrue(mappings.Contains("\"10\",\"42\",\"j1\"") && mappings.Contains("\"10\",\"42\",\"j2\""), mappings)
            let authoritativeMappings = File.ReadAllText(diagnosticPath output1 "traces" "source_to_output_trips.csv")
            Assert.IsTrue(
                authoritativeMappings.Split('\n')
                |> Array.exists (fun line -> line.Contains("\"ids-jmk-gtfs\"") && line.Contains("authoritative_source_trip_set")),
                "The combined overlay must retain IDS JMK authoritative additions")
            let unmatchedReasons = File.ReadAllText(diagnosticPath output1 "events" "unmatched_trip_reasons.csv")
            Assert.IsFalse(
                unmatchedReasons.Split('\n')
                |> Array.exists (fun line -> line.Contains("\"jf\"") && line.Contains("\"withheld\"")),
                "A combined-source authoritative addition must not be classified as withheld")
            let trips = readGtfs output1 "trips.txt"
            Assert.IsTrue(trips.Contains("overlay:ids-jmk-gtfs:trip:"), "An unmatched ferry must be imported with a deterministic source namespace")
            Assert.IsFalse(trips.Contains("jr"), "Heavy rail must remain excluded")
            let jmkTripBindings =
                JrUtil.Serving.PackageReader.readTextRows
                    (Path.Combine(output1, "serving", "source_trip_map.parquet"))
                    [| "source_id"; "source_trip_id" |]
                |> Seq.toArray
            Assert.IsFalse(
                jmkTripBindings |> Array.exists (fun row -> row.[0] = "ids-jmk-gtfs" && row.[1] = "jt"),
                "PID native-mode permissions must not make an unmatched JMK trolleybus native")
            let zones = File.ReadAllText(diagnosticPath output1 "projection/czech" "cz_stop_zones.txt")
            Assert.IsTrue(zones.Contains("ids-jmk-gtfs") && zones.Contains("overlay:ids-jmk-gtfs:zone:"), zones)
            Assert.IsTrue(File.ReadAllText(diagnosticPath output1 "events" "diagnostics.csv").Contains("cross_source_fact_coalesced"))
            Assert.IsTrue(File.ReadAllText(diagnosticPath output1 "events" "stop_group_matches.csv").Contains("coordinate_identity_unique"))

            Assert.ThrowsExactly<ArgumentException>(fun () -> executeAll combinedPolicy 2026 [| pid; pid |] basePath (Path.Combine(root, "duplicate")) |> ignore) |> ignore
            Assert.ThrowsExactly<ArgumentException>(fun () -> executeAll combinedPolicy 2026 [| pid |] basePath (Path.Combine(root, "missing")) |> ignore) |> ignore

            let badChecksumDescriptor = Path.Combine(root, "jmk-bad-checksum.json")
            write badChecksumDescriptor "{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"deadbeef\"}"
            let badChecksum = { jmk with descriptorPath = badChecksumDescriptor }
            Assert.ThrowsExactly<InvalidOperationException>(fun () -> executeAll combinedPolicy 2026 [| pid; badChecksum |] basePath (Path.Combine(root, "bad-checksum")) |> ignore) |> ignore

            let skewDescriptor = Path.Combine(root, "jmk-skew.json")
            write skewDescriptor ($"{{\"retrieved_at\":\"2026-09-20T00:00:00+02:00\",\"payload_sha256\":\"{sha jmkZip}\"}}")
            let skewed = { jmk with descriptorPath = skewDescriptor }
            Assert.ThrowsExactly<InvalidOperationException>(fun () -> executeAll combinedPolicy 2026 [| pid; skewed |] basePath (Path.Combine(root, "skew")) |> ignore) |> ignore

            let jmkStopTimes = Path.Combine(jmkDir, "stop_times.txt")
            File.ReadAllText(jmkStopTimes).Replace("j1,09:00:45,09:00:45", "j1,09:01:45,09:01:45") |> write jmkStopTimes
            let conflictingZip = Path.Combine(root, "IDSJMK-conflicting.zip")
            ZipFile.CreateFromDirectory(jmkDir, conflictingZip)
            let conflictingDescriptor = Path.Combine(root, "jmk-conflicting.json")
            write conflictingDescriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha conflictingZip}\"}}")
            let conflicting = { jmk with payloadPath = conflictingZip; descriptorPath = conflictingDescriptor }
            let conflictOutput = Path.Combine(root, "conflicting")
            executeAll combinedPolicy 2026 [| pid; conflicting |] basePath conflictOutput |> ignore
            Assert.IsTrue(File.ReadAllText(diagnosticPath conflictOutput "events" "diagnostics.csv").Contains("cross_source_fact_conflict"))
            Assert.IsFalse((readGtfs conflictOutput "stop_times.txt").Contains("09:01:45"), "A cross-source conflict must retain the national schedule")

            write (Path.Combine(jmkDir, "api.txt")) "Linka/CVlaku = trip_id: malformed\n"
            let malformedZip = Path.Combine(root, "IDSJMK-malformed.zip")
            ZipFile.CreateFromDirectory(jmkDir, malformedZip)
            let malformedDescriptor = Path.Combine(root, "jmk-malformed.json")
            write malformedDescriptor ($"{{\"retrieved_at\":\"2026-08-20T00:00:00+02:00\",\"payload_sha256\":\"{sha malformedZip}\"}}")
            let malformed = { jmk with payloadPath = malformedZip; descriptorPath = malformedDescriptor }
            Assert.ThrowsExactly<InvalidOperationException>(fun () -> executeAll combinedPolicy 2026 [| pid; malformed |] basePath (Path.Combine(root, "malformed")) |> ignore) |> ignore
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
