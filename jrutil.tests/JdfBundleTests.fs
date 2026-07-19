// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open System.IO.Compression
open System.Text.Json
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parquet.Serialization

open JrUtil
open JrUtil.Tests.Asserts

[<TestClass>]
type JdfBundleTests() =
    let fixturePath =
        Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")

    let descriptor path kind sha bytes =
        File.WriteAllText(path,
            $$"""{
  "schema_version": 1,
  "source_id": "national-jdf",
  "retrieved_at": "2026-07-18T12:00:00+02:00",
  "retrieval_method": "fixture",
  "source_uri": null,
  "licence": "synthetic-test-data",
  "payload_kind": "{{kind}}",
  "payload_sha256": "{{sha}}",
  "payload_bytes": {{bytes}}
}""")

    let readParquet path =
        use stream = File.OpenRead(path)
        ParquetSerializer.DeserializeUntypedAsync(stream).GetAwaiter().GetResult()

    let assertSchema root fileName expected =
        let result = readParquet (Path.Combine(root, fileName))
        let actual =
            result.Schema.DataFields
            |> Array.map (fun field -> field.Name, field.ClrType, field.IsNullable)
        assertEqual expected actual
        result

    [<TestMethod>]
    member _.``JDF bundle is deterministic and contains normalized sidecars``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-bundle-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let sha, bytes = JdfBundle.directoryTreeIdentity fixturePath
            let descriptorPath = Path.Combine(root, "snapshot.json")
            descriptor descriptorPath "directory-tree" sha bytes
            let first = Path.Combine(root, "first")
            let second = Path.Combine(root, "second")
            JdfBundle.writeBundle descriptorPath "test-commit" false fixturePath first
            JdfBundle.writeBundle descriptorPath "test-commit" false fixturePath second

            let expectedFiles = [|
                "source_route_metadata.parquet"; "source_stop_metadata.parquet"
                "source_call_metadata.parquet"; "source_route_stop_zone_metadata.parquet"
                "source_notice_metadata.parquet"; "source_transfer_metadata.parquet"
                "source_travel_restriction_metadata.parquet"
                "diagnostics.json"; "manifest.json"
                Path.Combine("gtfs-intermediate", "stop_times.txt")
                Path.Combine("extensions", "cz_stop_zones.txt")
            |]
            expectedFiles |> Array.iter (fun relative ->
                let firstPath = Path.Combine(first, relative)
                let secondPath = Path.Combine(second, relative)
                assertEqual true (File.Exists(firstPath))
                CollectionAssert.AreEqual(File.ReadAllBytes(firstPath), File.ReadAllBytes(secondPath)))

            let stopTimesHeader =
                File.ReadLines(Path.Combine(first, "gtfs-intermediate", "stop_times.txt"))
                |> Seq.head
            assertEqual false (stopTimesHeader.Contains("stop_zone_ids"))
            assertEqual
                "stop_place_id,zone_id,zone_code,route_id,ids_system_id,source_provenance"
                (File.ReadLines(Path.Combine(first, "extensions", "cz_stop_zones.txt")) |> Seq.head)

            let schemas = [|
                "source_route_metadata.parquet", [|
                    "gtfs_route_id", typeof<string>, false; "source_route_id", typeof<string>, false
                    "route_distinction", typeof<int>, false; "source_agency_id", typeof<string>, false
                    "source_agency_distinction", typeof<int>, false
                    "valid_from", typeof<string>, false; "valid_to", typeof<string>, false |]
                "source_stop_metadata.parquet", [|
                    "gtfs_stop_id", typeof<string>, false; "town", typeof<string>, false
                    "district", typeof<string>, true; "nearby_place", typeof<string>, true
                    "country", typeof<string>, true; "coordinates_missing", typeof<bool>, false |]
                "source_call_metadata.parquet", [|
                    "gtfs_trip_id", typeof<string>, false; "stop_sequence", typeof<int>, false
                    "source_route_stop_id", typeof<int64>, false |]
                "source_route_stop_zone_metadata.parquet", [|
                    "gtfs_route_id", typeof<string>, false
                    "source_route_stop_id", typeof<int64>, false
                    "zone_id", typeof<string>, false; "zone_order", typeof<int>, false |]
                "source_notice_metadata.parquet", [|
                    "source_notice_id", typeof<string>, false; "notice_kind", typeof<string>, false
                    "gtfs_route_id", typeof<string>, true; "gtfs_trip_id", typeof<string>, true
                    "label", typeof<string>, true; "text", typeof<string>, true
                    "valid_from", typeof<string>, true; "valid_to", typeof<string>, true
                    "service_note_type", typeof<string>, true |]
                "source_transfer_metadata.parquet", [|
                    "source_transfer_id", typeof<string>, false; "gtfs_trip_id", typeof<string>, false
                    "source_route_stop_id", typeof<int64>, false; "transfer_type", typeof<string>, false
                    "transfer_route_id", typeof<int64>, true; "transfer_stop_id", typeof<int64>, true
                    "transfer_stop_post_id", typeof<int64>, true
                    "transfer_end_stop_id", typeof<int64>, true
                    "transfer_end_stop_post_id", typeof<int64>, true
                    "wait_minutes", typeof<int>, true; "note", typeof<string>, true |]
                "source_travel_restriction_metadata.parquet", [|
                    "assignment_scope", typeof<string>, false
                    "gtfs_route_id", typeof<string>, true; "gtfs_trip_id", typeof<string>, true
                    "source_route_stop_id", typeof<int64>, false
                    "group_code", typeof<string>, false |]
            |]
            let parquet =
                schemas
                |> Array.map (fun (fileName, schema) -> fileName, assertSchema first fileName schema)
                |> Map
            parquet |> Map.iter (fun _ value ->
                assertEqual "1" (string value.CustomMetadata.["obehy.schema_version"])
                assertEqual "national-jdf" (string value.CustomMetadata.["obehy.source_id"])
                assertEqual ($"sha256:{sha}") (string value.CustomMetadata.["obehy.snapshot_id"]))

            let mirroredFiles = [|
                "source_routes.parquet"; "source_stop_places.parquet"
                "source_boarding_points.parquet"; "source_trips.parquet"
                "source_calls.parquet"; "fare_zones.parquet"; "source_stop_zones.parquet"
                "source_stop_zone_metadata.parquet"
            |]
            mirroredFiles |> Array.iter (fun fileName ->
                assertEqual false (File.Exists(Path.Combine(first, fileName))))

            let memberships = parquet.["source_route_stop_zone_metadata.parquet"]
            assertEqual 5 memberships.Data.Count
            let calls = parquet.["source_call_metadata.parquet"]
            assertEqual true (calls.Data.Count > 0)
            assertEqual 3 parquet.["source_notice_metadata.parquet"].Data.Count
            assertEqual 1 parquet.["source_transfer_metadata.parquet"].Data.Count
            assertEqual 5 parquet.["source_travel_restriction_metadata.parquet"].Data.Count

            let parsed = Gtfs.gtfsParseFolder () (Path.Combine(first, "gtfs-intermediate"))
            parsed.trips |> Seq.iter (fun trip ->
                assertEqual true (trip.serviceId.StartsWith("gtfs:service:")))
            let routeIds = parsed.routes |> Seq.map (fun route -> route.id) |> set
            parquet.["source_route_metadata.parquet"].Data |> Seq.iter (fun value ->
                assertEqual true (routeIds.Contains(string value.["gtfs_route_id"])))
            let stopIds = parsed.stops |> Seq.map (fun stop -> stop.id) |> set
            parquet.["source_stop_metadata.parquet"].Data |> Seq.iter (fun value ->
                assertEqual true (stopIds.Contains(string value.["gtfs_stop_id"])))
            let callKeys = parsed.stopTimes |> Seq.map (fun call -> call.tripId, call.stopSequence) |> set
            calls.Data |> Seq.iter (fun value ->
                assertEqual true (callKeys.Contains(string value.["gtfs_trip_id"],
                                                        Convert.ToInt32(value.["stop_sequence"]))))

            let extensionZoneKeys =
                GtfsParser.getGtfsFileParser<GtfsModel.CzStopZone>
                    (Path.Combine(first, "extensions", "cz_stop_zones.txt"))
                |> Seq.map (fun zone -> zone.routeId, zone.zoneId)
                |> set
            memberships.Data |> Seq.iter (fun value ->
                assertEqual true (extensionZoneKeys.Contains(string value.["gtfs_route_id"],
                                                               string value.["zone_id"])))

            let tripIds = parsed.trips |> Seq.map (fun trip -> trip.id) |> set
            parquet.["source_notice_metadata.parquet"].Data |> Seq.iter (fun value ->
                if value.ContainsKey("gtfs_route_id") && not (isNull value.["gtfs_route_id"]) then
                    assertEqual true (routeIds.Contains(string value.["gtfs_route_id"]))
                if value.ContainsKey("gtfs_trip_id") && not (isNull value.["gtfs_trip_id"]) then
                    assertEqual true (tripIds.Contains(string value.["gtfs_trip_id"])))
            let sourceCallKeys =
                calls.Data
                |> Seq.map (fun value -> string value.["gtfs_trip_id"],
                                         Convert.ToInt64(value.["source_route_stop_id"]))
                |> set
            parquet.["source_transfer_metadata.parquet"].Data |> Seq.iter (fun value ->
                assertEqual true (sourceCallKeys.Contains(string value.["gtfs_trip_id"],
                                                           Convert.ToInt64(value.["source_route_stop_id"]))))
            parquet.["source_travel_restriction_metadata.parquet"].Data |> Seq.iter (fun value ->
                let scope = string value.["assignment_scope"]
                assertEqual true (scope = "route_stop" || scope = "trip_call")
                if scope = "route_stop" then
                    assertEqual true (routeIds.Contains(string value.["gtfs_route_id"]))
                    assertEqual false (value.ContainsKey("gtfs_trip_id"))
                else
                    assertEqual false (value.ContainsKey("gtfs_route_id"))
                    assertEqual true (sourceCallKeys.Contains(string value.["gtfs_trip_id"],
                                                               Convert.ToInt64(value.["source_route_stop_id"]))))
            let noticeKinds =
                parquet.["source_notice_metadata.parquet"].Data
                |> Seq.map (fun value -> string value.["notice_kind"])
                |> set
            assertEqual (set ["route_information"; "service_note"; "reservation"]) noticeKinds
            let noticeIds =
                parquet.["source_notice_metadata.parquet"].Data
                |> Seq.map (fun value -> string value.["source_notice_id"])
                |> set
            assertEqual true (noticeIds |> Seq.forall (fun value -> value.StartsWith("jdf:notice:")))
            assertEqual false
                (noticeIds.Contains "jdf:notice:trip:586001:1:1:2")
            assertEqual false
                (noticeIds.Contains "jdf:notice:trip:586001:1:3:1")
            parquet.["source_notice_metadata.parquet"].Data
            |> Seq.find (fun value -> string value.["notice_kind"] = "service_note")
            |> fun value -> assertEqual "Poznámka ke spoji" (string value.["text"])
            let transfer = parquet.["source_transfer_metadata.parquet"].Data |> Seq.exactlyOne
            assertEqual true ((string transfer.["source_transfer_id"]).StartsWith("jdf:transfer:"))
            assertEqual 5 (Convert.ToInt32(transfer.["wait_minutes"]))
            assertEqual "Vycka na pripoj" (string transfer.["note"])
            let restrictionScopes =
                parquet.["source_travel_restriction_metadata.parquet"].Data
                |> Seq.countBy (fun value -> string value.["assignment_scope"])
                |> Map
            assertEqual 3 restrictionScopes.["route_stop"]
            assertEqual 2 restrictionScopes.["trip_call"]

            use diagnostics = JsonDocument.Parse(File.ReadAllBytes(Path.Combine(first, "diagnostics.json")))
            let diagnosticCodes =
                diagnostics.RootElement.GetProperty("diagnostics").EnumerateArray()
                |> Seq.map (fun value -> value.GetProperty("code").GetString())
                |> set
            assertEqual true (diagnosticCodes.Contains "filtered_enrichment")
            assertEqual true (diagnosticCodes.Contains "unjoinable_call_enrichment")
            assertEqual true (diagnosticCodes.Contains "blank_notice")
            assertEqual true (diagnosticCodes.Contains "singleton_travel_restriction")
            assertEqual false (diagnosticCodes.Contains "unhandled_service_note")
            let filteredEnrichmentIds =
                diagnostics.RootElement.GetProperty("diagnostics").EnumerateArray()
                |> Seq.filter (fun value -> value.GetProperty("code").GetString() = "filtered_enrichment")
                |> Seq.map (fun value -> value.GetProperty("source_object_id").GetString())
                |> set
            assertEqual true
                (filteredEnrichmentIds.Contains "jdf:restriction:586001:1:3:1")

            use manifest = JsonDocument.Parse(File.ReadAllBytes(Path.Combine(first, "manifest.json")))
            assertEqual "obehy-jrutil-jdf" (manifest.RootElement.GetProperty("bundle_format").GetString())
            assertEqual "test-commit" (manifest.RootElement.GetProperty("conversion").GetProperty("version").GetString())
            manifest.RootElement.GetProperty("files").EnumerateArray()
            |> Seq.iter (fun entry ->
                let relative = entry.GetProperty("path").GetString()
                use stream = File.OpenRead(Path.Combine(first, relative))
                let sha = Security.Cryptography.SHA256.HashData(stream)
                          |> Convert.ToHexString
                          |> fun value -> value.ToLowerInvariant()
                assertEqual (entry.GetProperty("sha256").GetString()) sha)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``JDF bundle validates snapshots and ZIP packaging atomically``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-bundle-errors-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let descriptorPath = Path.Combine(root, "snapshot.json")
            let output = Path.Combine(root, "invalid")
            descriptor descriptorPath "directory-tree" (String.replicate 64 "0") 0L
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfBundle.writeBundle descriptorPath "test-commit" false fixturePath output)
            |> ignore
            assertEqual false (Directory.Exists(output))

            let zipPath = Path.Combine(root, "fixture.zip")
            ZipFile.CreateFromDirectory(fixturePath, zipPath)
            let zipSha =
                use stream = File.OpenRead(zipPath)
                Security.Cryptography.SHA256.HashData(stream)
                |> Convert.ToHexString
                |> fun value -> value.ToLowerInvariant()
            descriptor descriptorPath "zip" zipSha (FileInfo(zipPath).Length)
            let zipOutput = Path.Combine(root, "zip")
            JdfBundle.writeBundle descriptorPath "test-commit" true zipPath zipOutput
            assertEqual true (File.Exists(Path.Combine(zipOutput, "manifest.json")))

            let unsafeZip = Path.Combine(root, "unsafe.zip")
            use unsafeArchive = ZipFile.Open(unsafeZip, ZipArchiveMode.Create)
            unsafeArchive.CreateEntry("../VerzeJDF.txt") |> ignore
            unsafeArchive.Dispose()
            let unsafeSha =
                use stream = File.OpenRead(unsafeZip)
                Security.Cryptography.SHA256.HashData(stream)
                |> Convert.ToHexString
                |> fun value -> value.ToLowerInvariant()
            descriptor descriptorPath "zip" unsafeSha (FileInfo(unsafeZip).Length)
            let unsafeOutput = Path.Combine(root, "unsafe")
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfBundle.writeBundle descriptorPath "test-commit" false unsafeZip unsafeOutput)
            |> ignore
            assertEqual false (Directory.Exists(unsafeOutput))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
