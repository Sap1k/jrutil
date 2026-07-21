// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

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
        exactMatches (sourceStop expected "CZ") [| candidate expected "D" point |]
        |> Array.length
        |> assertEqual 0
