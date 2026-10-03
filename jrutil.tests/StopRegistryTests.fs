// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open JrUtil
open JrUtil.Tests.Asserts

[<TestClass>]
type StopRegistryTests() =
    let withRegistry (files: (string * string) list) action =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-stop-registry-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            for name, content in files do
                File.WriteAllText(Path.Combine(root, name), content)
            action root
        finally
            Directory.Delete(root, true)

    let stopsHeader = "id,town,district,nearby_place,okres,country,lat,lon,status,note\n"

    let est stopId ordinal lat lon : StopRegistry.RegistryPost =
        { stopId = stopId; postKey = $"est:{ordinal}"; reference = Some (lat, lon); retired = false }

    [<TestMethod>]
    member _.``Registry files load with aliases merges and posts``() =
        withRegistry [
            "stops.csv",
            stopsHeader
            + "1,Most,nádraží,,MO,CZ,50.5,13.6,active,\n"
            + "2,Most,\"hlavní nádraží, vlak\",,MO,CZ,,,retired,merged_into:1\n"
            "posts.csv", "stop_id,post_key,lat,lon,status,note\n1,est:1,50.5,13.6,active,\n1,post:A,,,active,\n"
            "overlay_places.csv", "source_id,group_key,place_id,status,note\npid,group:x,overlay:pid:stop-place:abc,active,\n"
        ] (fun root ->
            let registry = StopRegistry.load root
            assertEqual 2 registry.stops.Length
            assertEqual (Some "hlavní nádraží, vlak") registry.stops.[1].district
            assertEqual 1L registry.stops.[1].resolvedId
            assertEqual (Some (50.5, 13.6)) registry.stops.[0].reference
            assertEqual 2 registry.posts.Length
            assertEqual "overlay:pid:stop-place:abc" registry.overlayPlaces.[0].placeId
            assertEqual 64 registry.sha256.Length)

    [<TestMethod>]
    member _.``Invalid registries are rejected``() =
        let rejects files =
            withRegistry files (fun root ->
                Assert.Throws<ArgumentException>(fun () -> StopRegistry.load root |> ignore) |> ignore)
        rejects [ "stops.csv", "id,town\n1,Most\n" ]
        rejects [ "stops.csv", stopsHeader + "1000000000,Most,,,MO,CZ,,,active,\n" ]
        rejects [ "stops.csv", stopsHeader + "1,Most,,,MO,CZ,50.5,,active,\n" ]
        rejects [ "stops.csv", stopsHeader + "1,Most,,,MO,CZ,,,active,merged_into:7\n" ]
        rejects [ "stops.csv", stopsHeader + "1,Most,,,MO,CZ,,,active,\n"
                  "posts.csv", "stop_id,post_key,lat,lon,status,note\n1,est:1,,,active,\n" ]
        rejects [ "stops.csv", stopsHeader + "1,Most,,,MO,CZ,,,active,\n"
                  "posts.csv", "stop_id,post_key,lat,lon,status,note\n2,post:1,,,active,\n" ]

    [<TestMethod>]
    member _.``Inferred posts keep registered ordinals despite new locations``() =
        let registered = [| est 5L 2 50.0 14.0; est 5L 4 50.001 14.0 |]
        // Location IDs sort in a different order than the registered ordinals,
        // and a new location appears between them.
        let locations = [|
            "estimated:a", 50.00095, 14.0
            "estimated:b", 50.00001, 14.0
            "estimated:c", 50.0005, 14.0
        |]
        let ordinals, fresh = StopRegistry.assignInferredPostOrdinals registered locations
        assertEqual 4 ordinals.["estimated:a"]
        assertEqual 2 ordinals.["estimated:b"]
        assertEqual 5 ordinals.["estimated:c"]
        assertEqual [| "estimated:c", 5, 50.0005, 14.0 |] fresh

    [<TestMethod>]
    member _.``A registered post is reused by its nearest location only``() =
        let registered = [| est 5L 1 50.0 14.0 |]
        let locations = [| "estimated:far", 50.0001, 14.0; "estimated:near", 50.00001, 14.0; "estimated:out", 50.01, 14.0 |]
        let ordinals, fresh = StopRegistry.assignInferredPostOrdinals registered locations
        assertEqual 1 ordinals.["estimated:near"]
        assertEqual 2 ordinals.["estimated:far"]
        assertEqual 3 ordinals.["estimated:out"]
        assertEqual 2 fresh.Length

    [<TestMethod>]
    member _.``Review CSV quotes separators``() =
        let path = Path.Combine(Path.GetTempPath(), "jrutil-registry-csv-" + Guid.NewGuid().ToString("N") + ".csv")
        try
            StopRegistry.writeCsv path [| "a"; "b" |] [ [| "x,y"; "say \"hi\"" |] ]
            assertEqual "a,b\n\"x,y\",\"say \"\"hi\"\"\"\n" (File.ReadAllText(path))
        finally
            File.Delete(path)
