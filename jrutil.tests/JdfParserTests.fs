// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open System.IO.Compression
open Microsoft.VisualStudio.TestTools.UnitTesting

open JrUtil
open JrUtil.Tests.Asserts

[<TestClass>]
type JdfParserTests() =
    let parseRouteInfo (text: string) =
        use stream = new MemoryStream(JdfParser.jdfEncoding.GetBytes(text))
        JdfParser.getJdfParser<JdfModel.RouteInfo> stream |> Seq.toArray

    [<TestMethod>]
    member _.``Native call storage preserves exact values and filtered ordering``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-native-calls-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            use store = new JdfCallStore.Store(root, Threading.CancellationToken.None)
            let sample: JdfModel.TripStop = {
                routeId = "česká"; tripId = Int64.MaxValue; routeStopId = -77L; stopId = 98L
                stopPostId = Some 0L; stopPostNum = Some ""; attributes = [| None; Some 0; Some Int32.MinValue |]
                kilometer = Some -123.4567890123456789012345678M
                arrivalTime = Some (JdfModel.StopTime (NodaTime.LocalTime.FromNanosecondsSinceMidnight(123456789L)))
                departureTime = Some JdfModel.Passing; minArrivalTime = Some JdfModel.NotPassing
                maxDepartureTime = None; routeDistinction = 7 }
            let input = [| sample; { sample with tripId = 7L; routeDistinction = 8; stopId = 99L; stopPostNum = None; kilometer = None }; sample |]
            let calls = store.Write(input, ignore)
            let spans rows = JdfCallStore.tripSpans rows |> Seq.toArray
            assertEqual (spans input) (spans calls)
            assertEqual input (calls |> Seq.toArray)
            assertEqual sample calls.[2]
            let filtered = calls |> JdfCallStore.filter (fun call -> call.stopId = 98L)
            assertEqual [| sample; sample |] (filtered |> Seq.toArray)
            let beforeSecondFilter = store.ScratchBytes
            let twice = filtered |> JdfCallStore.filter (fun call -> call.stopId = 98L)
            assertEqual [| sample; sample |] (twice |> Seq.toArray)
            assertEqual (spans [| sample; sample |]) (spans twice)
            Assert.IsTrue(store.ScratchBytes > beforeSecondFilter, "Repeated filtering must retain disk-backed indexes")
            assertEqual 0 (calls |> JdfCallStore.filter (fun _ -> false)).Count
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``JDF parser retains a terminator split across read buffers``() =
        let bufferSize = 1024 * 1024
        let prefix = "\"R\",\"1\",\""
        let suffix = "\",\"1"
        let payloadLength = bufferSize - 2 - prefix.Length - suffix.Length
        let payload = String('x', payloadLength)
        let first = prefix + payload + suffix + "\";\r\n"
        let second = "\"R\",\"2\",\"after\",\"1\";\r\n"
        let rows = parseRouteInfo (first + second)
        assertEqual 2 rows.Length
        assertEqual payload rows.[0].text
        assertEqual "after" rows.[1].text

    [<TestMethod>]
    member _.``JDF parser preserves empty fields and undoubled quotes``() =
        let rows =
            parseRouteInfo (
                "\"R\",\"1\",\"\",\"1\";\r\n"
                + "\"R\",\"2\",\"text with \"quote\"\",\"1\";\r\n")
        assertEqual "" rows.[0].text
        assertEqual "text with \"quote\"" rows.[1].text

    [<TestMethod>]
    member _.``JDF parser rejects records without an opening quote``() =
        Assert.ThrowsExactly<Exception>(fun () ->
            parseRouteInfo "R,\"1\",\"bad\",\"1\";\r\n" |> ignore)
        |> ignore

    [<TestMethod>]
    member _.``JDF ZIP writer is deterministic uncompressed and path parser compatible``() =
        let fixturePath =
            Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")
        let parsedFixture = Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)
        let source = {
            parsedFixture with
                postCandidateEvidence = [| ({
                    stopId = 100L; candidateId = "candidate-a"; observationId = "osm:node:1"
                    sourceKind = "osm"; sourceObjectId = Some "osm:node:1"
                    observedAt = Some "2026-08-10"; lat = 50M; lon = 14M
                    supportWeight = 1M; rawTags = "highway=bus_stop"
                    explicitModes = "road"; deniedModes = ""; lifecycle = "active"
                } : JdfModel.PostCandidateEvidence) |]
                routingDemands = [| ({
                    demandId = "demand-a"; modeFamily = "both"
                    previousStopId = Some 100L; nextStopId = Some 200L
                    previousLat = Some 50M; previousLon = Some 14M
                    nextLat = Some 50.1M; nextLon = Some 14.1M; searchClass = "local"
                } : JdfModel.RoutingDemand) |]
        }
        let root =
            Path.Combine(Path.GetTempPath(), "jrutil-jdf-zip-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let write name =
                let path = Path.Combine(root, name)
                use archive = ZipFile.Open(path, ZipArchiveMode.Create)
                Jdf.jdfBatchDirWriter () (Jdf.ZipArchive archive) source
                path
            let first = write "first.zip"
            let second = write "second.zip"
            CollectionAssert.AreEqual(File.ReadAllBytes(first), File.ReadAllBytes(second))
            use archive = ZipFile.OpenRead(first)
            assertEqual true (archive.GetEntry("JrutilPostCandidates.txt") = null)
            assertEqual true (archive.GetEntry("JrutilPostCandidateEvidence.txt") <> null)
            assertEqual true (archive.GetEntry("JrutilPostCandidateGeometry.txt") = null)
            assertEqual true (archive.GetEntry("JrutilRoutingDemands.txt") <> null)
            for entry in archive.Entries do
                assertEqual entry.Length entry.CompressedLength
                assertEqual 1980 entry.LastWriteTime.Year
            let parsed = Jdf.parseJdfBatchPath (Jdf.jdfBatchDirParser ()) first
            assertEqual source.version parsed.version
            assertEqual source.stops parsed.stops
            assertEqual source.tripStops parsed.tripStops
            assertEqual source.postCandidateEvidence parsed.postCandidateEvidence
            assertEqual source.routingDemands parsed.routingDemands
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
