// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

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
                  precision = StopPrecise
                  source = None } }
          score = 1.0f }

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
    member _.``Post-merge reconciliation adds an exact missing location with provenance``() =
        let fixturePath =
            Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")
        let source = Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)
        let stop = source.stops.[0]
        let point =
            stop.regionId
            |> Option.map (fun region -> (czechRegionPolygons ()).[region].Centroid)
            |> Option.defaultValue (czechRegionPolygons () |> Map.toSeq |> Seq.head |> snd |> fun p -> p.Centroid)
        let external = [|
            { name = Jdf.jdfStopNameString stop
              data =
                { regionId = stop.regionId; country = stop.country; point = point
                  precision = StopPrecise; source = Some "external:manual" } }
        |]
        use matcher = new StopMatcher<JdfStopGeodata>(external)
        let batch = {
            source with
                stops = [| stop |]
                stopLocations = [||]
                stopLocationSources = [||]
        }

        let reconciled = reconcileMissingStopLocations matcher batch

        assertEqual stop.id (reconciled.stopLocations |> Array.exactlyOne).stopId
        assertEqual "external:manual" (reconciled.stopLocationSources |> Array.exactlyOne).source
