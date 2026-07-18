// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open JrUtil
open JrUtil.Tests.Asserts

[<TestClass>]
type JdfToGtfsTests() =
    let fixturePath =
        Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")

    let batch () =
        Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)

    let route routeId (feed: GtfsModel.GtfsFeed) =
        feed.routes |> Array.find (fun item -> item.id = routeId)

    [<TestMethod>]
    member _.``JDF conversion emits Oběhy identities and public line numbers``() =
        let feed = batch () |> JdfToGtfs.getGtfsFeed false

        route "-CISR-586001-1" feed
        |> fun item -> assertEqual (Some "10") item.shortName
        route "-CISR-446002-1" feed
        |> fun item -> assertEqual (Some "N2") item.shortName
        route "-CISR-582486-1" feed
        |> fun item -> assertEqual (Some "486") item.shortName

        let czRoutes = feed.czRoutes |> Option.get
        let numericRoute =
            czRoutes
            |> Array.find (fun item -> item.routeId = "-CISR-586001-1")
        assertEqual (Some "586001") numericRoute.cisLineId
        assertEqual (Some "10") numericRoute.publicLineNumber
        assertEqual None numericRoute.idsSystemId
        assertEqual (Some "6,193") numericRoute.idsZoneIds
        assertEqual "jdf:1.11" numericRoute.sourceProvenance

        let czTrip =
            feed.czTrips
            |> Option.get
            |> Array.find (fun item -> item.tripId = "CIST-586001-1-1")
        assertEqual "CIST-586001-1-1" czTrip.tripId
        assertEqual (Some "586001") czTrip.cisLineId
        assertEqual (Some 1L) czTrip.cisTripId
        assertEqual (Some "CIST-586001-1-1") czTrip.sourceTripIds
        assertEqual (Some "jdf") czTrip.coverageSources

        let routeIds = feed.routes |> Array.map (fun item -> item.id) |> set
        let tripIds = feed.trips |> Array.map (fun item -> item.id) |> set
        feed.czRoutes |> Option.get |> Array.iter (fun item ->
            assertEqual true (routeIds.Contains item.routeId))
        feed.czTrips |> Option.get |> Array.iter (fun item ->
            assertEqual true (tripIds.Contains item.tripId))

    [<TestMethod>]
    member _.``Ambiguous or malformed public line identity is not guessed``() =
        let source = batch ()
        let firstRoute = source.routes.[0]
        let firstIntegration = source.routeIntegrations.[0]
        let ambiguous = {
            source with
                routes = [| firstRoute |]
                routeIntegrations = [|
                    firstIntegration
                    { firstIntegration with entryNum = 2; routeName = "999" }
                |]
        }
        let malformed = {
            source with
                routes = [| { firstRoute with id = "BUS-1" } |]
                routeIntegrations = [||]
        }
        assertEqual
            None
            ((JdfToGtfs.getPublicLineNumbers ambiguous).["586001", 1])
        assertEqual
            None
            ((JdfToGtfs.getPublicLineNumbers malformed).["BUS-1", 1])

    [<TestMethod>]
    member _.``JDF conversion normalizes zones and preserves both post forms``() =
        let feed = batch () |> JdfToGtfs.getGtfsFeed false
        let stopIds = feed.stops |> Array.map (fun stop -> stop.id) |> set

        assertEqual true (stopIds.Contains "JDFS-100-N-1")
        assertEqual true (stopIds.Contains "JDFS-200-7")
        feed.stops
        |> Array.find (fun stop -> stop.id = "JDFS-100")
        |> fun stop -> assertEqual (Some "6,193") stop.zoneId
        feed.stops
        |> Array.find (fun stop -> stop.id = "JDFS-100-N-1")
        |> fun stop -> assertEqual (Some "1") stop.platformCode
        feed.stops
        |> Array.find (fun stop -> stop.id = "JDFS-200-7")
        |> fun stop -> assertEqual (Some "B") stop.platformCode

        let numericStopTimes =
            feed.stopTimes
            |> Array.filter (fun stopTime ->
                stopTime.tripId = "CIST-586001-1-1")
            |> Array.sortBy (fun stopTime -> stopTime.stopSequence)
        assertEqual "JDFS-100-N-1" numericStopTimes.[0].stopId
        assertEqual (Some "6,193") numericStopTimes.[0].stopZoneIds
        assertEqual "JDFS-200" numericStopTimes.[1].stopId
        let formalStopTimes =
            feed.stopTimes
            |> Array.filter (fun stopTime ->
                stopTime.tripId = "CIST-446002-1-1")
            |> Array.sortBy (fun stopTime -> stopTime.stopSequence)
        assertEqual "JDFS-100-8" formalStopTimes.[0].stopId
        assertEqual "JDFS-200-7" formalStopTimes.[1].stopId
        feed.stopTimes
        |> Array.iter (fun stopTime ->
            assertEqual true (stopIds.Contains stopTime.stopId))

        let czStops = feed.czStops |> Option.get
        assertEqual
            (feed.stops |> Array.map (fun stop -> stop.id) |> set)
            (czStops |> Array.map (fun stop -> stop.stopId) |> set)
        let numberedPost =
            czStops |> Array.find (fun stop -> stop.stopId = "JDFS-100-N-1")
        assertEqual "JDFS-100" numberedPost.stopPlaceId
        assertEqual (Some "1") numberedPost.postId
        assertEqual (Some "jdf-stop-post-num:100/1") numberedPost.sourceIds
        let formalPost =
            czStops |> Array.find (fun stop -> stop.stopId = "JDFS-200-7")
        assertEqual (Some "7") formalPost.postId
        assertEqual
            (Some "jdf-stop-post-id:200/7,jdf-stop-post-num:200/A")
            formalPost.sourceIds

    [<TestMethod>]
    member _.``CIS stop mode and extension serialization are deterministic``() =
        let feed = batch () |> JdfToGtfs.getGtfsFeed true
        let czStops = feed.czStops |> Option.get
        czStops |> Array.iter (fun stop -> assertEqual true stop.cisStopId.IsSome)
        assertEqual true (feed.stops |> Array.exists (fun stop ->
            stop.id = "-CISS-100-N-1"))

        let root =
            Path.Combine(Path.GetTempPath(), "jrutil-obehy-" + Guid.NewGuid().ToString("N"))
        let first = Path.Combine(root, "first")
        let second = Path.Combine(root, "second")
        try
            let write = Gtfs.gtfsFeedToFolder ()
            write first feed
            write second feed

            let expectedHeaders = [|
                "cz_routes.txt", "route_id,cis_line_id,public_line_number,ids_system_id,ids_zone_ids,source_provenance"
                "cz_trips.txt", "trip_id,cis_line_id,cis_trip_id,train_number,source_trip_ids,coverage_sources"
                "cz_stops.txt", "stop_id,stop_place_id,cis_stop_id,post_id,asw_id,source_ids"
            |]
            expectedHeaders |> Array.iter (fun (fileName, expectedHeader) ->
                let firstPath = Path.Combine(first, fileName)
                let secondPath = Path.Combine(second, fileName)
                assertEqual expectedHeader (File.ReadLines(firstPath) |> Seq.head)
                CollectionAssert.AreEqual(File.ReadAllBytes(firstPath),
                                          File.ReadAllBytes(secondPath))
                let bytes = File.ReadAllBytes(firstPath)
                let hasUtf8Bom =
                    bytes.Length >= 3
                    && bytes.[0] = 0xEFuy
                    && bytes.[1] = 0xBBuy
                    && bytes.[2] = 0xBFuy
                assertEqual false hasUtf8Bom)

            let parsed = Gtfs.gtfsParseFolder () first
            assertEqual (Some 3) (parsed.czRoutes |> Option.map Array.length)
            assertEqual (Some 2) (parsed.czTrips |> Option.map Array.length)
            assertEqual (Some 5) (parsed.czStops |> Option.map Array.length)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
