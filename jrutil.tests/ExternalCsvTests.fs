// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
namespace JrUtil.Tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open JrUtil
open JrUtil.GeoData.ExternalCsv
open JrUtil.Tests.Asserts

[<TestClass>]
type ExternalCsvTests() =
    let makeRoot () =
        let root =
            Path.Combine(
                Path.GetTempPath(),
                "jrutil-external-csv-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        root

    [<TestMethod>]
    member this.``Loads one geodata CSV file``() =
        let root = makeRoot ()
        try
            let path = Path.Combine(root, "stops.csv")
            File.WriteAllText(path, "Alpha,50.0,14.0,AB,CZ\n")

            let rows = otherStopsFromPath path

            assertEqual 1 rows.Length
            assertEqual "Alpha" rows.[0].Name
        finally
            Directory.Delete(root, true)

    [<TestMethod>]
    member this.``Loads stop rows with source provenance``() =
        let root = makeRoot ()
        try
            let path = Path.Combine(root, "legacy.csv")
            File.WriteAllText(
                path,
                "Stop precise,50.0,14.0,AB,CZ\n")

            let matches = otherStopsFromPathForJdfMatch root

            assertEqual (Some "external:legacy") matches.[0].data.source
        finally
            Directory.Delete(root, true)

    [<TestMethod>]
    member this.``Rejects obsolete six-column town rows``() =
        let root = makeRoot ()
        try
            File.WriteAllText(Path.Combine(root, "town.csv"), "Town,49.0,15.0,BE,CZ,T\n")
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                otherStopsFromPath root |> ignore)
            |> ignore
        finally
            Directory.Delete(root, true)

    [<TestMethod>]
    member this.``Loads nested directory deterministically and removes exact duplicates``() =
        let root = makeRoot ()
        try
            let nested = Path.Combine(root, "nested")
            Directory.CreateDirectory(nested) |> ignore
            File.WriteAllText(
                Path.Combine(nested, "b.csv"),
                "Duplicate,50.0,14.0,AB,CZ\nBeta,49.0,15.0,BE,CZ\n")
            File.WriteAllText(
                Path.Combine(root, "a.csv"),
                "Alpha,51.0,13.0,AA,CZ\nDuplicate,50.0,14.0,AB,CZ\n")

            let rows = otherStopsFromPath root

            rows |> Seq.map (fun row -> row.Name)
            |> assertSeqEqual ["Alpha"; "Duplicate"; "Beta"]
        finally
            Directory.Delete(root, true)

    [<TestMethod>]
    member this.``Rejects geodata directory without CSV files``() =
        let root = makeRoot ()
        try
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                otherStopsFromPath root |> ignore)
            |> ignore
        finally
            Directory.Delete(root, true)

    [<TestMethod>]
    member this.``Rejects malformed geodata CSV``() =
        let root = makeRoot ()
        try
            File.WriteAllText(Path.Combine(root, "bad.csv"), "Missing columns\n")

            Assert.ThrowsExactly<ArgumentException>(fun () ->
                otherStopsFromPath root |> ignore)
            |> ignore
        finally
            Directory.Delete(root, true)

    [<TestMethod>]
    member this.``Rejects non-finite and out-of-range geodata coordinates``() =
        let root = makeRoot ()
        try
            File.WriteAllText(Path.Combine(root, "bad.csv"), "Invalid,NaN,181,AB,CZ\n")

            Assert.ThrowsExactly<ArgumentException>(fun () ->
                otherStopsFromPath root |> ignore)
            |> ignore
        finally
            Directory.Delete(root, true)
