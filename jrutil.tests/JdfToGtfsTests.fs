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
    member _.``JDF writer creates a missing filesystem output directory``() =
        let root =
            Path.Combine(
                Path.GetTempPath(),
                "jrutil-jdf-writer-" + Guid.NewGuid().ToString("N"))
        let output = Path.Combine(root, "missing", "nested")
        try
            Jdf.jdfBatchDirWriter () (Jdf.FsPath output) (batch ())

            assertEqual true (File.Exists(Path.Combine(output, "VerzeJDF.txt")))
            assertEqual true (File.Exists(Path.Combine(output, "Zastavky.txt")))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``JDF conversion emits Oběhy identities and public line numbers``() =
        let feed = batch () |> JdfToGtfs.getGtfsFeed false

        route "jdf:route:586001:1" feed
        |> fun item ->
            assertEqual (Some "10") item.shortName
            assertEqual (Some "0076a3") item.color
            assertEqual (Some "ffffff") item.textColor
        route "jdf:route:446002:1" feed
        |> fun item ->
            assertEqual (Some "N2") item.shortName
            assertEqual (Some "80166f") item.color
            assertEqual (Some "ffffff") item.textColor
        route "jdf:route:582486:1" feed
        |> fun item -> assertEqual (Some "486") item.shortName

        let czRoutes = feed.czRoutes |> Option.get
        let numericRoute =
            czRoutes
            |> Array.find (fun item -> item.routeId = "jdf:route:586001:1")
        assertEqual (Some "586001") numericRoute.cisLineId
        assertEqual (Some "10") numericRoute.publicLineNumber
        assertEqual "jdf:1.11" numericRoute.sourceProvenance

        let czTrip =
            feed.czTrips
            |> Option.get
            |> Array.find (fun item -> item.tripId = "jdf:trip:586001:1:1")
        assertEqual "jdf:trip:586001:1:1" czTrip.tripId
        assertEqual (Some "586001") czTrip.cisLineId
        assertEqual (Some 1L) czTrip.cisTripId
        assertEqual (Some "jdf:trip:586001:1:1") czTrip.sourceTripIds
        assertEqual (Some "jdf") czTrip.coverageSources

        let routeIds = feed.routes |> Array.map (fun item -> item.id) |> set
        let tripIds = feed.trips |> Array.map (fun item -> item.id) |> set
        feed.agencies |> Array.iter (fun item ->
            assertEqual true (item.id |> Option.exists (fun id -> id.StartsWith("jdf:agency:"))))
        feed.routes |> Array.iter (fun item ->
            assertEqual true (item.id.StartsWith("jdf:route:")))
        feed.trips |> Array.iter (fun item ->
            assertEqual true (item.id.StartsWith("jdf:trip:")))
        feed.stops |> Array.iter (fun item ->
            assertEqual true (item.id.StartsWith("jdf:stop:")))
        feed.czStopZones |> Option.get |> Array.iter (fun item ->
            assertEqual true (item.zoneId.StartsWith("jdf:zone:")))
        feed.czRoutes |> Option.get |> Array.iter (fun item ->
            assertEqual true (routeIds.Contains item.routeId))
        feed.czTrips |> Option.get |> Array.iter (fun item ->
            assertEqual true (tripIds.Contains item.tripId))

    [<TestMethod>]
    member _.``JDF routes receive default colors by transport mode``() =
        let sourceRoute = (batch ()).routes.[0]
        let colors mode publicLineNumber =
            { sourceRoute with transportMode = mode }
            |> JdfToGtfs.getGtfsRouteColors publicLineNumber

        assertEqual (Some "0076a3", Some "ffffff")
                    (colors JdfModel.Bus None)
        assertEqual (Some "7a0200", Some "ffffff")
                    (colors JdfModel.Tram None)
        assertEqual (Some "80166f", Some "ffffff")
                    (colors JdfModel.Trolleybus None)
        assertEqual (Some "c8d021", Some "1c1745")
                    (colors JdfModel.CableCar None)
        assertEqual (Some "00b274", Some "ffffff")
                    (colors JdfModel.Metro (Some "A"))
        assertEqual (Some "fbaf33", Some "1c1745")
                    (colors JdfModel.Metro (Some "b"))
        assertEqual (Some "d31245", Some "ffffff")
                    (colors JdfModel.Metro (Some "C"))
        assertEqual (Some "1c1745", Some "ffffff")
                    (colors JdfModel.Metro (Some "D"))
        assertEqual (Some "00b3cb", Some "1c1745")
                    (colors JdfModel.Ferry None)

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

        assertEqual true (stopIds.Contains "jdf:stop:100:post:1")
        assertEqual true (stopIds.Contains "jdf:stop:200:post:id:7")
        assertEqual true (stopIds.Contains "jdf:stop:200:unspecified")
        feed.stops
        |> Array.find (fun stop -> stop.id = "jdf:stop:100")
        |> fun stop ->
            assertEqual None stop.zoneId
            assertEqual (Some GtfsModel.Station) stop.locationType
        feed.stops
        |> Array.find (fun stop -> stop.id = "jdf:stop:200")
        |> fun stop -> assertEqual (Some "193") stop.zoneId
        feed.stops
        |> Array.find (fun stop -> stop.id = "jdf:stop:200:unspecified")
        |> fun stop ->
            assertEqual (Some GtfsModel.Stop) stop.locationType
            assertEqual (Some "jdf:stop:200") stop.parentStation
        feed.stops
        |> Array.find (fun stop -> stop.id = "jdf:stop:100:post:1")
        |> fun stop ->
            assertEqual (Some "1") stop.platformCode
            assertEqual (Some "jdf:stop:100") stop.parentStation
        feed.stops
        |> Array.find (fun stop -> stop.id = "jdf:stop:200:post:id:7")
        |> fun stop ->
            assertEqual (Some "B") stop.platformCode
            assertEqual (Some "jdf:stop:200") stop.parentStation

        let numericStopTimes =
            feed.stopTimes
            |> Array.filter (fun stopTime ->
                stopTime.tripId = "jdf:trip:586001:1:1")
            |> Array.sortBy (fun stopTime -> stopTime.stopSequence)
        assertEqual "jdf:stop:100:post:1" numericStopTimes.[0].stopId
        assertEqual None numericStopTimes.[0].stopZoneIds
        assertEqual "jdf:stop:200:unspecified" numericStopTimes.[1].stopId
        let formalStopTimes =
            feed.stopTimes
            |> Array.filter (fun stopTime ->
                stopTime.tripId = "jdf:trip:446002:1:1")
            |> Array.sortBy (fun stopTime -> stopTime.stopSequence)
        assertEqual "jdf:stop:100:post:id:8" formalStopTimes.[0].stopId
        assertEqual "jdf:stop:200:post:id:7" formalStopTimes.[1].stopId
        feed.stopTimes
        |> Array.iter (fun stopTime ->
            assertEqual true (stopIds.Contains stopTime.stopId))
        let referencedStopIds = feed.stopTimes |> Seq.map (fun stopTime -> stopTime.stopId) |> set
        let requiredParentIds =
            feed.stops
            |> Seq.filter (fun stop -> referencedStopIds.Contains stop.id)
            |> Seq.choose (fun stop -> stop.parentStation)
            |> set
        assertEqual (Set.union referencedStopIds requiredParentIds) stopIds

        let czStops = feed.czStops |> Option.get
        assertEqual
            (feed.stops |> Array.map (fun stop -> stop.id) |> set)
            (czStops |> Array.map (fun stop -> stop.stopId) |> set)
        let numberedPost =
            czStops |> Array.find (fun stop -> stop.stopId = "jdf:stop:100:post:1")
        assertEqual "jdf:stop:100" numberedPost.stopPlaceId
        assertEqual (Some "1") numberedPost.postId
        assertEqual (Some "jdf:stop:100:post:1") numberedPost.sourceIds
        let formalPost =
            czStops |> Array.find (fun stop -> stop.stopId = "jdf:stop:200:post:id:7")
        assertEqual (Some "7") formalPost.postId
        assertEqual
            (Some "jdf:stop:200:post:id:7,jdf:stop:200:post:A")
            formalPost.sourceIds
        let czStopZones = feed.czStopZones |> Option.get
        assertEqual 5 czStopZones.Length
        assertEqual 4
            (czStopZones
             |> Array.filter (fun zone -> zone.stopPlaceId = "jdf:stop:100")
             |> Array.length)
        czStopZones |> Array.iter (fun zone -> assertEqual None zone.idsSystemId)

    [<TestMethod>]
    member _.``Estimated coordinates mark stop places and boarding points with question mark``() =
        let source = batch ()
        let withLocations = {
            source with
                stopLocations = [|
                    {
                        stopId = 100L
                        lat = 50.0M
                        lon = 14.0M
                        precision = JdfModel.Estimated
                    }
                    {
                        stopId = 200L
                        lat = 50.1M
                        lon = 14.1M
                        precision = JdfModel.StopPrecise
                    }
                |]
        }
        let feed = withLocations |> JdfToGtfs.getGtfsFeed false
        let stop100Names =
            feed.stops
            |> Array.filter (fun stop -> stop.id.StartsWith("jdf:stop:100"))
            |> Array.map (fun stop -> stop.name)
        assertEqual true (stop100Names.Length > 1)
        stop100Names
        |> Array.iter (fun name ->
            assertEqual true (name.EndsWith(" [?]"))
            assertEqual false (name.EndsWith(" [?] [?]")))
        feed.stops
        |> Array.filter (fun stop -> stop.id.StartsWith("jdf:stop:200"))
        |> Array.iter (fun stop ->
            assertEqual false (stop.name.EndsWith(" [?]")))

    [<TestMethod>]
    member _.``CIS stop mode and extension serialization are deterministic``() =
        let feed = batch () |> JdfToGtfs.getGtfsFeed true
        let czStops = feed.czStops |> Option.get
        czStops |> Array.iter (fun stop -> assertEqual true stop.cisStopId.IsSome)
        assertEqual true (feed.stops |> Array.exists (fun stop ->
            stop.id = "cis:stop:100:post:1"))

        let root =
            Path.Combine(Path.GetTempPath(), "jrutil-obehy-" + Guid.NewGuid().ToString("N"))
        let first = Path.Combine(root, "first")
        let second = Path.Combine(root, "second")
        try
            let write = Gtfs.gtfsFeedToFolder ()
            write first feed
            write second feed

            let expectedHeaders = [|
                "cz_routes.txt", "route_id,cis_line_id,public_line_number,source_provenance"
                "cz_trips.txt", "trip_id,cis_line_id,cis_trip_id,train_number,source_trip_ids,coverage_sources"
                "cz_stops.txt", "stop_id,stop_place_id,cis_stop_id,post_id,asw_id,source_ids"
                "cz_stop_zones.txt", "stop_place_id,zone_id,zone_code,route_id,ids_system_id,source_provenance"
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
            assertEqual (Some 6) (parsed.czStops |> Option.map Array.length)
            assertEqual (Some 5) (parsed.czStopZones |> Option.map Array.length)
            assertEqual false
                ((File.ReadLines(Path.Combine(first, "stop_times.txt")) |> Seq.head)
                    .Contains("stop_zone_ids"))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
