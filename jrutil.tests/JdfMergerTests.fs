// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open NodaTime

open JrUtil
open JrUtil.JdfModel
open JrUtil.JdfStopReconciliation
open JrUtil.Tests.Asserts

[<TestClass>]
type JdfMergerTests() =
    let fixturePath =
        Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")

    let template =
        lazy (Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath))

    let emptyBatch stops locations posts =
        { template.Value with
            stops = stops
            stopLocations = locations
            stopLocationSources = [||]
            stopPosts = posts
            agencies = [||]
            routes = [||]
            routeIntegrations = [||]
            routeStops = [||]
            trips = [||]
            tripGroups = [||]
            tripStops = [||]
            routeInfo = [||]
            serviceNotes = [||]
            transfers = [||]
            agencyAlternations = [||]
            alternateRouteNames = [||]
            reservationOptions = [||] }

    let stop id town district nearby attributes = {
        id = id
        town = town
        district = district
        nearbyPlace = nearby
        regionId = Some "UL"
        country = Some "CZ"
        attributes = attributes
    }

    let precise id lat lon = {
        stopId = id
        lat = lat
        lon = lon
        precision = StopPrecise
    }

    let post stopId postId = {
        stopId = stopId
        stopPostId = postId
        name = None
        description = None
        postName = Some (string postId)
        reserved1 = None
        reserved2 = None
    }

    let merge batches =
        let merger = JdfMerger.JdfMerger(JdfMerger.MergeStopsByName)
        batches |> Seq.iter merger.add
        merger

    [<TestMethod>]
    member _.``Post observations remap to canonical stops deterministically``() =
        let firstStop = stop 1L "Candidate stop" None None [||]
        let secondStop = stop 2L "Candidate stop" None None [||]
        let observation stopId id source : PostCandidateEvidence = {
            stopId=stopId;candidateId=id;observationId=id;sourceKind=source
            sourceObjectId=Some id;observedAt=None;lat=50M;lon=14M;supportWeight=1M
            rawTags="";explicitModes="road";deniedModes="";lifecycle="active" }
        let first = {
            emptyBatch [| firstStop |] [| precise 1L 50M 14M |] [||] with
                postCandidateEvidence = [| observation 1L "catalogue:a" "catalogue" |]
        }
        let second = {
            emptyBatch [| secondStop |] [| precise 2L 50M 14M |] [||] with
                postCandidateEvidence = [| observation 2L "osm:b" "osm" |]
        }
        let result = (merge [ first; second ]).batch
        assertEqual 2 result.postCandidateEvidence.Length
        result.postCandidateEvidence |> Array.iter(fun observation ->
            assertEqual result.stops.[0].id observation.stopId)

    [<TestMethod>]
    member _.``Expanded CIS stop name replaces abbreviated alias``() =
        let abbreviated = stop 1L "Ústí n.L." (Some "hl.nádr.") None [| Some 1; None; None; None; None; None |]
        let expanded = stop 2L "Ústí nad Labem" (Some "Hlavní nádraží") None [| Some 2; None; None; None; None; None |]
        let merger =
            merge [
                emptyBatch [| abbreviated |] [| precise 1L 50.6595592M 14.0436811M |] [| post 1L 1L |]
                emptyBatch [| expanded |] [| precise 2L 50.6594901M 14.0445786M |] [| post 2L 2L |]
            ]
        let result = merger.batch
        assertEqual 1 result.stops.Length
        assertEqual "Ústí nad Labem,Hlavní nádraží" (stopDisplayName result.stops.[0])
        assertEqual (set [1; 2]) (result.stops.[0].attributes |> Array.choose id |> set)
        assertEqual 2 result.stopPosts.Length
        assertEqual 1 (result.stopPosts |> Array.map (fun value -> value.stopId) |> Array.distinct |> Array.length)
        assertEqual 1L merger.stopMergeStatistics.exact

    [<TestMethod>]
    member _.``Clean alias wins over a redundant three-component alias``() =
        let duplicated =
            stop 1L "Chomutov" (Some "Chomutov") (Some "žel.st.") [||]
        let clean = stop 2L "Chomutov" (Some "žel.st.") None [||]
        let merger =
            merge [
                emptyBatch [| duplicated |] [| precise 1L 50.4562M 13.3993M |] [||]
                emptyBatch [| clean |] [| precise 2L 50.4562M 13.3993M |] [||]
            ]
        assertEqual 1 merger.batch.stops.Length
        assertEqual "Chomutov,žel.st." (stopDisplayName merger.batch.stops.[0])

    [<TestMethod>]
    member _.``Hierarchical CIS name replaces shortened MHD form``() =
        let short = stop 1L "Dolní Jiřetín" (Some "rozc.") None [||]
        let full = stop 2L "Horní Jiřetín" (Some "Dolní Jiřetín") (Some "rozcestí") [||]
        let merger =
            merge [
                emptyBatch [| short |] [| precise 1L 50.5550505M 13.5900450M |] [||]
                emptyBatch [| full |] [| precise 2L 50.5550388M 13.5899196M |] [||]
            ]
        assertEqual 1 merger.batch.stops.Length
        assertEqual
            "Horní Jiřetín,Dolní Jiřetín,rozcestí"
            (stopDisplayName merger.batch.stops.[0])
        assertEqual 1L merger.stopMergeStatistics.suffix

    [<TestMethod>]
    member _.``Compatible names beyond 75 metres remain separate``() =
        let first = stop 1L "Ústí n.L." (Some "hl.nádr.") None [||]
        let second = stop 2L "Ústí nad Labem" (Some "Hlavní nádraží") None [||]
        let merger =
            merge [
                emptyBatch [| first |] [| precise 1L 50.6595M 14.0436M |] [||]
                emptyBatch [| second |] [| precise 2L 50.6595M 14.0460M |] [||]
            ]
        assertEqual 2 merger.batch.stops.Length

    [<TestMethod>]
    member _.``A later precise location updates an existing alias``() =
        let withoutLocation = stop 1L "Same name" (Some "station") None [||]
        let withLocation = { withoutLocation with id = 2L }
        let distant = { withoutLocation with id = 3L }
        let merger =
            merge [
                emptyBatch [| withoutLocation |] [||] [||]
                emptyBatch [| withLocation |] [| precise 2L 50.0000M 14.0000M |] [||]
                emptyBatch [| distant |] [| precise 3L 50.0000M 14.0020M |] [||]
            ]
        assertEqual 2 merger.batch.stops.Length

    [<TestMethod>]
    member _.``Balanced fuzzy match accepts a minor spelling difference nearby``() =
        let first = stop 1L "Testov" (Some "stanice u stare nemocnice") None [||]
        let second = stop 2L "Testov" (Some "stanice u staré nemocnice") None [||]
        let merger =
            merge [
                emptyBatch [| first |] [| precise 1L 50.0000M 14.0000M |] [||]
                emptyBatch [| second |] [| precise 2L 50.0001M 14.0001M |] [||]
            ]
        assertEqual 1 merger.batch.stops.Length
        assertEqual 1L merger.stopMergeStatistics.fuzzy

    [<TestMethod>]
    member _.``Nearby unrelated names and conflicting regions remain separate``() =
        let first = stop 1L "Testov" (Some "škola") None [||]
        let unrelated = stop 2L "Testov" (Some "nemocnice") None [||]
        let otherRegion = { first with id = 3L; regionId = Some "MO" }
        let merger =
            merge [
                emptyBatch [| first |] [| precise 1L 50.0000M 14.0000M |] [||]
                emptyBatch [| unrelated |] [| precise 2L 50.0001M 14.0001M |] [||]
                emptyBatch [| otherRegion |] [| precise 3L 50.0001M 14.0001M |] [||]
            ]
        assertEqual 3 merger.batch.stops.Length

    [<TestMethod>]
    member _.``Missing coordinates merge only a unique suffix locality``() =
        let full = stop 1L "Horní Jiřetín" (Some "Dolní Jiřetín") (Some "rozcestí") [||]
        let short = stop 2L "Dolní Jiřetín" (Some "rozc.") None [||]
        let merger =
            merge [ emptyBatch [| full |] [||] [||]; emptyBatch [| short |] [||] [||] ]
        assertEqual 1 merger.batch.stops.Length

    [<TestMethod>]
    member _.``Ambiguous suffix candidates remain separate``() =
        let first = stop 1L "Horní Jiřetín" (Some "Dolní Jiřetín") (Some "rozcestí") [||]
        let second = stop 2L "Most" (Some "Dolní Jiřetín") (Some "rozcestí") [||]
        let short = stop 3L "Dolní Jiřetín" (Some "rozc.") None [||]
        let merger =
            merge [
                emptyBatch [| first; second |] [||] [||]
                emptyBatch [| short |] [||] [||]
            ]
        assertEqual 3 merger.batch.stops.Length
        assertEqual 1L merger.stopMergeStatistics.ambiguous

    [<TestMethod>]
    member _.``Exact candidate wins over an older suffix candidate``() =
        let short = stop 1L "Dětkovice" None None [||]
        let full = stop 2L "Ludmírov" (Some "Dětkovice") None [||]
        let repeatedFull = { full with id = 3L }
        let merger =
            merge [
                emptyBatch [| short |] [| precise 1L 49.7000M 16.9500M |] [||]
                emptyBatch [| full |] [| precise 2L 49.7100M 16.9500M |] [||]
                emptyBatch [| repeatedFull |] [||] [||]
            ]
        assertEqual 2 merger.batch.stops.Length
        assertEqual 0L merger.stopMergeStatistics.ambiguous
        assertEqual 1L merger.stopMergeStatistics.exact

    [<TestMethod>]
    member _.``Canonical name is independent of input order``() =
        let abbreviated = stop 1L "Ústí n.L." (Some "hl.nádr.") None [||]
        let expanded = stop 2L "Ústí nad Labem" (Some "Hlavní nádraží") None [||]
        let abbreviatedBatch =
            emptyBatch [| abbreviated |] [| precise 1L 50.6595592M 14.0436811M |] [||]
        let expandedBatch =
            emptyBatch [| expanded |] [| precise 2L 50.6594901M 14.0445786M |] [||]
        let forward = merge [ abbreviatedBatch; expandedBatch ]
        let reverse = merge [ expandedBatch; abbreviatedBatch ]
        assertEqual
            (stopDisplayName forward.batch.stops.[0])
            (stopDisplayName reverse.batch.stops.[0])

    [<TestMethod>]
    member _.``By-ID strategy does not reconcile distinct IDs``() =
        let first = stop 100L "Ústí n.L." (Some "hl.nádr.") None [||]
        let second = stop 200L "Ústí nad Labem" (Some "Hlavní nádraží") None [||]
        let merger = JdfMerger.JdfMerger(JdfMerger.MergeStopsById)
        merger.add(emptyBatch [| first; second |] [||] [||])
        assertEqual 2 merger.batch.stops.Length
        assertEqual 0L merger.stopMergeStatistics.candidateComparisons

    [<TestMethod>]
    member _.``Spatial candidate comparisons stay bounded for dispersed stops``() =
        let reconciler = StopReconciler()
        for index in 0..1999 do
            let id = int64 index + 1L
            let value = stop id $"Synthetic {index}" (Some "terminal station platform") None [||]
            let location = precise id (48M + decimal index * 0.01M) 14M
            match reconciler.FindMatch(value, Some location) with
            | Choice1Of2 _ -> Assert.Fail("Dispersed synthetic stop unexpectedly matched")
            | Choice2Of2 _ -> ()
            reconciler.AddAlias(id, value, Some location)
        assertEqual 0L reconciler.Statistics.candidateComparisons
        assertEqual 0L reconciler.Statistics.fuzzyComparisons

    [<TestMethod>]
    member _.``Dense local clusters do not trigger global comparisons``() =
        let reconciler = StopReconciler()
        for cluster in 0..99 do
            for item in 0..9 do
                let id = int64 (cluster * 10 + item + 1)
                let value =
                    stop id $"Synthetic {cluster} {item} terminal" None None [||]
                let location =
                    precise
                        id
                        (48M + decimal cluster * 0.01M)
                        (14M + decimal item * 0.00001M)
                let targetId =
                    match reconciler.FindMatch(value, Some location) with
                    | Choice1Of2 candidate -> candidate.stopId
                    | Choice2Of2 _ -> id
                reconciler.AddAlias(targetId, value, Some location)
        Assert.IsTrue(
            reconciler.Statistics.candidateComparisons < 10_000L,
            $"Expected local candidate bounds, got {reconciler.Statistics.candidateComparisons}")

    [<TestMethod>]
    member _.``Fuzzy candidates do not depend on global token rarity``() =
        let reconciler = StopReconciler()
        let local = stop 1L "Central common shared public terminal" None None [||]
        let sharedTokenFiller = stop 2L "Central common shared public terminal elsewhere" None None [||]
        let typoTokenFiller = stop 3L "Termnial unrelated remote place" None None [||]
        reconciler.AddAlias(1L, local, Some (precise 1L 50.0000M 14.0000M))
        reconciler.AddAlias(2L, sharedTokenFiller, Some (precise 2L 51.0000M 15.0000M))
        reconciler.AddAlias(3L, typoTokenFiller, Some (precise 3L 52.0000M 16.0000M))
        let query = stop 4L "Central common shared public termnial" None None [||]
        match reconciler.FindMatch(query, Some (precise 4L 50.0001M 14.0001M)) with
        | Choice1Of2 candidate -> assertEqual 1L candidate.stopId
        | Choice2Of2 _ -> Assert.Fail("Expected the nearby fuzzy candidate to match")

    [<TestMethod>]
    member _.``Spill-backed output is byte-identical across interleaved rows and route splits``() =
        let root = Path.Combine(Path.GetTempPath(), $"jrutil-merge-{Guid.NewGuid():N}")
        let memoryOutput = Path.Combine(root, "memory")
        let spillOutput = Path.Combine(root, "spill")
        let spillPath = Path.Combine(root, "trip-stops.tmp")
        Directory.CreateDirectory(root) |> ignore
        try
            let interleavedTripStops =
                template.Value.tripStops
                |> Array.groupBy (fun value -> value.routeId)
                |> Array.map snd
                |> fun groups -> [|
                    for index in 0 .. (groups |> Array.map Array.length |> Array.max) - 1 do
                        for group in groups do
                            if index < group.Length then yield group.[index]
                |]
                |> fun rows -> Array.init 3000 (fun _ -> rows) |> Array.concat
            let batch creationDate validFrom validTo =
                { template.Value with
                    version = { template.Value.version with creationDate = Some creationDate }
                    routes =
                        template.Value.routes
                        |> Array.map (fun route -> {
                            route with
                                timetableValidFrom = validFrom
                                timetableValidTo = validTo
                        })
                    tripStops = interleavedTripStops }
            let older = batch (LocalDate(2026, 1, 1)) (LocalDate(2026, 1, 1)) (LocalDate(2026, 12, 31))
            let inner = batch (LocalDate(2026, 2, 1)) (LocalDate(2026, 4, 1)) (LocalDate(2026, 6, 30))

            use memory = new JdfMerger.JdfMerger(JdfMerger.MergeStopsById)
            memory.add(older)
            memory.add(inner)
            memory.resolveRouteOverlaps()
            memory.write(memoryOutput)

            do
                use spill =
                    new JdfMerger.JdfMerger(
                        JdfMerger.MergeStopsById,
                        spillPath,
                        tripStopTransformWorkers = 24)
                spill.add(older)
                spill.add(inner)
                spill.resolveRouteOverlaps()
                spill.write(spillOutput)
                Assert.IsTrue(spill.tripStopSpillBytes > 0L)

            Assert.IsFalse(File.Exists(spillPath), "The spill file survived merger disposal")
            assertEqual 0 (Directory.EnumerateFiles(root, "trip-stops.tmp*") |> Seq.length)
            let memoryFiles = Directory.GetFiles(memoryOutput) |> Array.map Path.GetFileName |> Array.sort
            let spillFiles = Directory.GetFiles(spillOutput) |> Array.map Path.GetFileName |> Array.sort
            assertEqual memoryFiles spillFiles
            for name in memoryFiles do
                let expected = File.ReadAllBytes(Path.Combine(memoryOutput, name))
                let actual = File.ReadAllBytes(Path.Combine(spillOutput, name))
                CollectionAssert.AreEqual(expected, actual, name)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
