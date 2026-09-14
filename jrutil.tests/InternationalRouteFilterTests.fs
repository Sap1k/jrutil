// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text.Json
open Microsoft.VisualStudio.TestTools.UnitTesting

open JrUtil
open JrUtil.JdfModel
open JrUtil.JdfToGtfs
open JrUtil.Tests.Asserts

[<TestClass>]
type InternationalRouteFilterTests() =
    let fixturePath =
        Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")

    let fixture () = Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)

    let routeKey = "586001", 1

    let syntheticRoute foreignCountry span foreignDepth integrated routeType passing =
        let source = fixture ()
        let route =
            source.routes
            |> Array.find (fun value -> (value.id, value.idDistinction) = routeKey)
            |> fun value -> { value with routeType = routeType }
        let trip =
            source.trips
            |> Array.find (fun value -> value.routeId = fst routeKey && value.id = 1L)
        let sourceCalls =
            source.tripStops |> Seq.toArray
            |> Array.filter (fun value ->
                value.routeId = fst routeKey && value.tripId = 1L
                && (value.routeStopId = 1L || value.routeStopId = 2L))
            |> Array.sortBy (fun value -> value.routeStopId)
        let firstCall = { sourceCalls.[0] with kilometer = Some 0m }
        let secondCall = { sourceCalls.[1] with kilometer = Some (span - foreignDepth) }
        let thirdCall = {
            sourceCalls.[1] with
                routeStopId = 3L
                stopId = 300L
                kilometer = Some span
                departureTime =
                    if passing then Some Passing else sourceCalls.[1].departureTime
        }
        let routeStops =
            source.routeStops
            |> Array.filter (fun value ->
                value.routeId = fst routeKey && value.routeDistinction = snd routeKey)
            |> Array.filter (fun value -> value.routeStopId <= 2L)
        let thirdRouteStop = { routeStops.[1] with routeStopId = 3L; stopId = 300L }
        let baseStops =
            source.stops
            |> Array.filter (fun value -> value.id = 100L || value.id = 200L)
            |> Array.map (fun value -> { value with country = Some "CZ" })
        let foreignStop = {
            baseStops.[1] with
                id = 300L
                town = "Foreign"
                regionId = None
                country = Some foreignCountry
        }
        {
            source with
                stops = Array.append baseStops [| foreignStop |]
                stopPosts = [||]
                routes = [| route |]
                routeIntegrations =
                    if integrated then
                        source.routeIntegrations
                        |> Array.filter (fun value ->
                            value.routeId = fst routeKey
                            && value.routeDistinction = snd routeKey)
                    else [||]
                routeStops = Array.append routeStops [| thirdRouteStop |]
                trips = [| trip |]
                tripGroups = [||]
                tripStops = [| firstCall; secondCall; thirdCall |]
                routeInfo = [||]
                serviceNotes =
                    source.serviceNotes
                    |> Array.filter (fun value ->
                        value.routeId = fst routeKey && value.tripId = 1L)
                transfers = [||]
                agencyAlternations = [||]
                alternateRouteNames = [||]
                reservationOptions = [||]
                stopLocations = [||]
                stopLocationSources = [||]
        }

    let classify batch overrides =
        applyInternationalRoutePolicy RegionalAdjacent overrides batch

    [<TestMethod>]
    member _.``Keep-all preserves backward-compatible conversion``() =
        let source = syntheticRoute "UA" 1000m 500m false City false
        let result = applyInternationalRoutePolicy KeepAll [||] source
        assertEqual source.routes.Length result.batch.routes.Length
        assertEqual 0 result.decisions.Length

    [<TestMethod>]
    member _.``Short neighboring route is retained even when mislabeled city``() =
        let decision = (classify (syntheticRoute "DE" 114m 44m false City false) [||]).decisions |> Array.exactlyOne
        assertEqual true decision.keep
        assertEqual "regional_adjacent" decision.reason

    [<TestMethod>]
    member _.``Integrated route uses relaxed envelope``() =
        let retained = (classify (syntheticRoute "A" 180m 70m true International false) [||]).decisions |> Array.exactlyOne
        let rejected = (classify (syntheticRoute "A" 180m 70m false International false) [||]).decisions |> Array.exactlyOne
        assertEqual true retained.keep
        assertEqual false rejected.keep
        assertEqual "trip_span_exceeds_limit" rejected.reason

    [<TestMethod>]
    member _.``Long neighboring and non-neighboring routes are rejected``() =
        let vienna = (classify (syntheticRoute "AT" 133m 84m false International false) [||]).decisions |> Array.exactlyOne
        let ukraine = (classify (syntheticRoute "UA" 80m 20m false Regional false) [||]).decisions |> Array.exactlyOne
        assertEqual "trip_span_exceeds_limit" vienna.reason
        assertEqual "non_adjacent_country" ukraine.reason

    [<TestMethod>]
    member _.``Foreign-only and missing-kilometre trips fail closed``() =
        let foreignOnly = syntheticRoute "D" 30m 10m false International false
        let foreignOnly = {
            foreignOnly with
                stops = foreignOnly.stops |> Array.map (fun stop -> { stop with country = Some "D"; regionId = None })
        }
        let missingKm = syntheticRoute "D" 30m 10m false International false
        let missingKm = {
            missingKm with
                tripStops =
                    missingKm.tripStops |> Seq.toArray
                    |> Array.map (fun call -> if call.stopId = 300L then { call with kilometer = None } else call)
        }
        assertEqual "foreign_only_trip" ((classify foreignOnly [||]).decisions |> Array.exactlyOne).reason
        assertEqual "missing_timetable_kilometres" ((classify missingKm [||]).decisions |> Array.exactlyOne).reason

    [<TestMethod>]
    member _.``Passing foreign stop does not make a domestic route international``() =
        let decision =
            (classify (syntheticRoute "UA" 1000m 500m false Regional true) [||]).decisions
            |> Array.exactlyOne
        assertEqual true decision.keep
        assertEqual "domestic" decision.reason

    [<TestMethod>]
    member _.``001398 regional trip survives while its foreign-only continuation is pruned``() =
        let routeId = "001398"
        let original = syntheticRoute "D" 50m 20m false International false
        let source = {
            original with
                routes = original.routes |> Array.map (fun value -> { value with id = routeId })
                routeStops = original.routeStops |> Array.map (fun value -> { value with routeId = routeId })
                trips = original.trips |> Array.map (fun value -> { value with routeId = routeId })
                tripStops = original.tripStops |> Seq.toArray |> Array.map (fun value -> { value with routeId = routeId })
                serviceNotes = original.serviceNotes |> Array.map (fun value -> { value with routeId = routeId })
        }
        let foreignTrip = { source.trips.[0] with id = 2L }
        let foreignCalls =
            source.tripStops |> Seq.toArray
            |> Array.map (fun call -> { call with tripId = 2L; stopId = 300L })
        let mixed = {
            source with
                trips = Array.append source.trips [| foreignTrip |]
                tripStops = Array.append (source.tripStops |> Seq.toArray) foreignCalls
        }

        let result = classify mixed [||]
        let decision = result.decisions |> Array.exactlyOne

        assertEqual true decision.keep
        assertEqual routeId decision.routeId
        assertEqual 1 decision.qualifyingCrossBorderTrips
        assertEqual 1 decision.foreignOnlyTrips
        assertEqual [| 1L |] (result.batch.trips |> Array.map (fun trip -> trip.id))
        assertEqual RegionalInternational (result.batch.routes |> Array.exactlyOne).routeType

    [<TestMethod>]
    member _.``Foreign calls on a trip without retained service dates are ignored``() =
        let source = fixture ()
        let domestic = syntheticRoute "CZ" 20m 10m false Regional false
        let filteredTrip =
            source.trips
            |> Array.find (fun value -> value.routeId = fst routeKey && value.id = 3L)
        let filteredCalls =
            source.tripStops |> Seq.toArray
            |> Array.filter (fun value -> value.routeId = fst routeKey && value.tripId = 3L)
            |> Array.sortBy (fun value -> value.routeStopId)
        let foreignStop = {
            domestic.stops.[0] with
                id = 400L; town = "Filtered foreign"; regionId = None; country = Some "UA"
        }
        let foreignRouteStop = {
            domestic.routeStops.[0] with routeStopId = 4L; stopId = 400L
        }
        let filteredForeignCall = {
            filteredCalls.[1] with routeStopId = 4L; stopId = 400L
        }
        let batch = {
            domestic with
                stops = Array.append domestic.stops [| foreignStop |]
                routeStops = Array.append domestic.routeStops [| foreignRouteStop |]
                trips = Array.append domestic.trips [| filteredTrip |]
                tripStops = Array.concat [domestic.tripStops |> Seq.toArray; [| filteredCalls.[0]; filteredForeignCall |]]
                serviceNotes =
                    source.serviceNotes
                    |> Array.filter (fun value ->
                        value.routeId = fst routeKey
                        && (value.tripId = 1L || value.tripId = 3L))
        }
        let decision = (classify batch [||]).decisions |> Array.exactlyOne
        assertEqual true decision.keep
        assertEqual "domestic" decision.reason

    [<TestMethod>]
    member _.``Overrides take precedence and rejected route dependents are pruned``() =
        let source = syntheticRoute "UA" 1000m 500m false City false
        let keepOverride = [|
            { routeId = fst routeKey; routeDistinction = snd routeKey
              decision = KeepRoute; reason = "reviewed regional exception" }
        |]
        let kept = classify source keepOverride
        assertEqual true (kept.decisions |> Array.exactlyOne).keep

        let dropped = classify source [||]
        assertEqual 0 dropped.batch.routes.Length
        assertEqual 0 dropped.batch.trips.Length
        assertEqual 0 dropped.batch.tripStops.Count
        assertEqual 0 dropped.batch.routeStops.Length
        assertEqual 0 dropped.batch.routeIntegrations.Length
        assertEqual 0 dropped.batch.stops.Length

    [<TestMethod>]
    member _.``Global overrides may belong to another fix-jdf batch``() =
        let source = syntheticRoute "UA" 1000m 500m false City false
        let otherBatchOverride = [|
            { routeId = "other-batch"; routeDistinction = 1
              decision = KeepRoute; reason = "reviewed elsewhere" }
        |]

        let result = classify source otherBatchOverride

        assertEqual false (result.decisions |> Array.exactlyOne).keep
        validateInternationalRouteOverrides
            (set [routeKey; ("other-batch", 1)]) otherBatchOverride
        Assert.ThrowsExactly<ArgumentException>(fun () ->
            validateInternationalRouteOverrides (set [routeKey]) otherBatchOverride)
        |> ignore

    [<TestMethod>]
    member _.``Override CSV rejects conflicting decisions``() =
        let path = Path.Combine(Path.GetTempPath(), "jrutil-route-overrides-" + Guid.NewGuid().ToString("N") + ".csv")
        try
            File.WriteAllLines(path, [|
                "route_id,route_distinction,decision,reason"
                "586001,1,keep,first review"
                "586001,1,drop,second review"
            |])
            Assert.ThrowsExactly<ArgumentException>(fun () -> loadInternationalRouteOverrides path |> ignore)
            |> ignore
        finally
            File.Delete(path)

    [<TestMethod>]
    member _.``Rejected route bundle has diagnostics manifest metadata and no dangling rows``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-international-bundle-" + Guid.NewGuid().ToString("N"))
        let input = Path.Combine(root, "jdf")
        let zipPath = Path.Combine(root, "jdf.zip")
        let descriptorPath = Path.Combine(root, "snapshot.json")
        let output = Path.Combine(root, "bundle")
        Directory.CreateDirectory(root) |> ignore
        try
            syntheticRoute "UA" 1000m 500m false City false
            |> Jdf.jdfBatchDirWriter () (Jdf.FsPath input)
            ZipFile.CreateFromDirectory(input, zipPath)
            let digest =
                use stream = File.OpenRead(zipPath)
                SHA256.HashData(stream) |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()
            let descriptor =
                $"""{{
  "schema_version": 1,
  "source_id": "test-international-filter",
  "retrieved_at": "2026-01-01T00:00:00Z",
  "retrieval_method": "test",
  "source_uri": null,
  "licence": "test",
  "payload_kind": "zip",
  "payload_sha256": "{digest}",
  "payload_bytes": {FileInfo(zipPath).Length}
}}"""
            File.WriteAllText(descriptorPath, descriptor)
            JrUtil.JdfBundle.writeBundleWithPolicy
                descriptorPath "test-commit" false RegionalAdjacent [||] zipPath output

            let gtfs, _ = JrUtil.Serving.PackageReader.prepareCompilerView output (Path.Combine(root, "compiler-view"))
            assertEqual 1 (File.ReadAllLines(Path.Combine(gtfs, "routes.txt")).Length)
            assertEqual 1 (File.ReadAllLines(Path.Combine(gtfs, "stops.txt")).Length)
            use diagnostics = JsonDocument.Parse(File.ReadAllBytes(Path.Combine(output, "diagnostics.json")))
            let filtered =
                diagnostics.RootElement.GetProperty("examples_by_code").GetProperty("filtered_international_route").EnumerateArray()
                |> Seq.exactlyOne
            assertEqual "jdf:route:586001:1" (filtered.GetProperty("source_object_id").GetString())
            use manifest = JsonDocument.Parse(File.ReadAllBytes(Path.Combine(output, "manifest.json")))
            let policy =
                manifest.RootElement.GetProperty("compiler").GetProperty("international_route_filter")
            assertEqual "regional-adjacent" (policy.GetProperty("policy").GetString())
            assertEqual 1 (policy.GetProperty("dropped_route_distinctions").GetInt32())
            let routeRelation =
                manifest.RootElement.GetProperty("relations").EnumerateArray()
                |> Seq.find (fun value -> value.GetProperty("name").GetString() = "route")
            assertEqual 0 (routeRelation.GetProperty("row_count").GetInt32())
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
