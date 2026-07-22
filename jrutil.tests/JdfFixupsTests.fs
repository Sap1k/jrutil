// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open NodaTime

open JrUtil
open JrUtil.GeoData.Common
open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.StopMatcher
open JrUtil.JdfFixups
open JrUtil.JdfModel
open JrUtil.Tests.Asserts

[<TestClass>]
type JdfFixupsTests() =
    let sourceStop region country =
        { id = 1L
          town = "Exact stop"
          district = None
          nearbyPlace = None
          regionId = Some region
          country = Some country
          attributes = Array.empty }

    let candidate region country point =
        { stop =
            { name = "Exact stop"
              data =
                { regionId = Some region
                  country = Some country
                  point = point
                  source = None } }
          score = 1.0f }

    let withSource source (match_: StopMatch<JdfStopGeodata>) =
        { match_ with
            stop =
                { match_.stop with
                    data = { match_.stop.data with source = Some source } } }

    let polygons = czechRegionPolygons () |> Map.toArray

    let adjacentPair =
        lazy (
            seq {
                for leftCode, leftPolygon in polygons do
                    for rightCode, rightPolygon in polygons do
                        if leftCode < rightCode && leftPolygon.Distance(rightPolygon) <= 100.0 then
                            yield leftCode, leftPolygon, rightCode, rightPolygon
            }
            |> Seq.head)

    [<TestMethod>]
    member _.``Historical OL code matches canonical OC strictly``() =
        let point = (czechRegionPolygons ()).["OC"].Centroid
        exactMatches (sourceStop "OL" "CZ") [| candidate "OC" "CZ" point |]
        |> Array.length
        |> assertEqual 1

    [<TestMethod>]
    member _.``Adjacent okres candidate on expected boundary is accepted``() =
        let expected, expectedPolygon, actual, _ = adjacentPair.Value
        let point = expectedPolygon.Factory.CreatePoint(expectedPolygon.Boundary.Coordinate)
        exactMatches (sourceStop expected "CZ") [| candidate actual "CZ" point |]
        |> Array.length
        |> assertEqual 1

    [<TestMethod>]
    member _.``Strict okres candidate wins over an adjacent boundary candidate``() =
        let expected, expectedPolygon, actual, _ = adjacentPair.Value
        let boundaryPoint =
            expectedPolygon.Factory.CreatePoint(expectedPolygon.Boundary.Coordinate)
        let strictPoint = expectedPolygon.Centroid

        let matches =
            exactMatches
                (sourceStop expected "CZ")
                [| candidate actual "CZ" boundaryPoint
                   candidate expected "CZ" strictPoint |]

        assertEqual 1 matches.Length
        assertEqual (Some expected) matches.[0].stop.data.regionId

    [<TestMethod>]
    member _.``Adjacent okres candidate outside tolerance is rejected``() =
        let expected, expectedPolygon, actual, actualPolygon = adjacentPair.Value
        let coordinate =
            actualPolygon.Coordinates
            |> Array.maxBy (fun coordinate ->
                expectedPolygon.Boundary.Distance(
                    expectedPolygon.Factory.CreatePoint(coordinate)))
        let point = actualPolygon.Factory.CreatePoint(coordinate)
        exactMatches (sourceStop expected "CZ") [| candidate actual "CZ" point |]
        |> Array.length
        |> assertEqual 0
        exactMatches (sourceStop expected "CZ") [| candidate expected "D" point |]
        |> Array.length
        |> assertEqual 0

    [<TestMethod>]
    member _.``Non-adjacent okres and wrong country candidates are rejected``() =
        let expected, expectedPolygon = polygons.[0]
        let actual, actualPolygon =
            polygons
            |> Array.find (fun (_, polygon) -> expectedPolygon.Distance(polygon) > 100.0)
        let point = actualPolygon.Centroid
        exactMatches (sourceStop expected "CZ") [| candidate actual "CZ" point |]
        |> Array.length
        |> assertEqual 0

    [<TestMethod>]
    member _.``Foreign exact match does not require a Czech okres code``() =
        let point = polygons.[0] |> snd |> fun polygon -> polygon.Centroid
        let foreignStop = sourceStop "PU" "SK"
        let foreignCandidate = candidate "" "SK" point

        exactMatches foreignStop [| foreignCandidate |]
        |> Array.length
        |> assertEqual 1

    [<TestMethod>]
    member _.``Checked exact match wins over an OSM exact match``() =
        let region, polygon = polygons.[0]
        let checkedPoint = polygon.Centroid
        let osmPoint =
            polygon.Factory.CreatePoint(
                NetTopologySuite.Geometries.Coordinate(
                    checkedPoint.X + 5000.0, checkedPoint.Y))
        let selected =
            topStopMatch
                (sourceStop region "CZ")
                [| candidate region "CZ" checkedPoint |> withSource "external:manual"
                   candidate region "CZ" osmPoint |> withSource "osm:czech-pbf" |]
            |> Option.get

        assertEqual checkedPoint selected.data.point
        assertEqual (Some "external:manual") selected.data.source

    [<TestMethod>]
    member _.``Impossible middle match is rejected and estimated by scheduled time``() =
        let fixturePath =
            Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")
        let source = Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)
        let first = source.stops.[0]
        let last = source.stops.[1]
        let middle = { first with id = 300L; nearbyPlace = Some "middle" }
        let notPassing = { first with id = 400L; nearbyPlace = Some "not passing" }
        let template = source.tripStops.[0]
        let timedCall stopId routeStopId minute =
            { template with
                stopId = stopId
                routeStopId = routeStopId
                arrivalTime = Some (StopTime(LocalTime(8, minute)))
                departureTime = Some (StopTime(LocalTime(8, minute))) }
        let calls = [|
            timedCall first.id 1L 0
            timedCall middle.id 2L 5
            { timedCall notPassing.id 3L 6 with
                arrivalTime = Some NotPassing
                departureTime = Some NotPassing }
            timedCall last.id 4L 10
        |]
        let factory = (czechRegionPolygons () |> Map.toSeq |> Seq.head |> snd).Factory
        let point x = factory.CreatePoint(NetTopologySuite.Geometries.Coordinate(x, 0.0))
        let matchAt stop point =
            Some {
                name = Jdf.jdfStopNameString stop
                data =
                    { regionId = stop.regionId; country = stop.country; point = point
                      source = Some "external:test" }
            }
        let plausible =
            [| first, matchAt first (point 0.0)
               middle, matchAt middle (point 1_000_000.0)
               notPassing, matchAt notPassing (point 8_000.0)
               last, matchAt last (point 10_000.0) |]
            |> rejectImplausibleMatches calls

        assertEqual true (plausible.[0] |> snd |> Option.isSome)
        assertEqual None (plausible.[1] |> snd)
        assertEqual true (plausible.[2] |> snd |> Option.isSome)
        assertEqual true (plausible.[3] |> snd |> Option.isSome)

        let batch = {
            source with
                stops = [| first; middle; notPassing; last |]
                tripStops = calls
        }
        let estimated =
            plausible
            |> addStopLocations batch
            |> estimateMissingStopLocations
        let middleLocation =
            estimated.stopLocations |> Array.find (fun value -> value.stopId = middle.id)
        let firstLocation =
            estimated.stopLocations |> Array.find (fun value -> value.stopId = first.id)
        let lastLocation =
            estimated.stopLocations |> Array.find (fun value -> value.stopId = last.id)

        assertEqual Estimated middleLocation.precision
        let expectedMidpoint = (firstLocation.lon + lastLocation.lon) / 2m
        assertEqual true (abs (middleLocation.lon - expectedMidpoint) < 0.001m)
        let source =
            estimated.stopLocationSources
            |> Array.find (fun value -> value.stopId = middle.id)
        assertEqual "estimated:route-time" source.source

    [<TestMethod>]
    member _.``Unmatched route origin is offset north of its first known stop``() =
        let fixturePath =
            Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")
        let source = Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)
        let origin = source.stops.[0]
        let known = source.stops.[1]
        let template = source.tripStops.[0]
        let calls = [|
            { template with
                stopId = origin.id; routeStopId = 1L
                arrivalTime = Some (StopTime(LocalTime(8, 0)))
                departureTime = Some (StopTime(LocalTime(8, 0))) }
            { template with
                stopId = known.id; routeStopId = 2L
                arrivalTime = Some (StopTime(LocalTime(8, 5)))
                departureTime = Some (StopTime(LocalTime(8, 5))) }
        |]
        let knownLocation = {
            stopId = known.id
            lat = 50.0m
            lon = 14.0m
            precision = StopPrecise
        }
        let estimated =
            { source with
                stops = [| origin; known |]
                tripStops = calls
                stopLocations = [| knownLocation |]
                stopLocationSources = [||] }
            |> estimateMissingStopLocations
        let originLocation =
            estimated.stopLocations |> Array.find (fun value -> value.stopId = origin.id)

        assertEqual Estimated originLocation.precision
        assertEqual true (originLocation.lat > knownLocation.lat)
        let locationSource =
            estimated.stopLocationSources
            |> Array.find (fun value -> value.stopId = origin.id)
        assertEqual "estimated:route-end-north" locationSource.source

    [<TestMethod>]
    member _.``Batch with one distinct called stop is dropped completely``() =
        let fixturePath =
            Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")
        let source = Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)
        let template = source.tripStops.[0]
        let oneStopBatch = {
            source with
                tripStops = [|
                    { template with stopId = source.stops.[0].id; routeStopId = 1L }
                    { template with stopId = source.stops.[0].id; routeStopId = 2L }
                |]
        }
        let dropped = dropDegenerateBatch oneStopBatch |> Option.get

        assertEqual 0 dropped.routes.Length
        assertEqual 0 dropped.stops.Length
        assertEqual 0 dropped.trips.Length
        assertEqual 0 dropped.tripStops.Length
