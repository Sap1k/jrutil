// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting
open NodaTime
open OsmSharp
open OsmSharp.Streams
open OsmSharp.Tags

open JrUtil
open JrUtil.GeoData.Common
open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.StopMatcher
open JrUtil.JdfFixups
open JrUtil.JdfModel
open JrUtil.Tests.Asserts

[<TestClass>]
type JdfFixupsTests() =
    let osmTags pairs =
        let result = TagsCollection()
        for key, value in pairs do result.Add(key, value)
        result

    let osmNode id latitude longitude =
        Node(
            Id = Nullable id,
            Version = Nullable 1L,
            ChangeSetId = Nullable 0L,
            Visible = Nullable true,
            TimeStamp = Nullable DateTime.UnixEpoch,
            UserId = Nullable 0L,
            UserName = "",
            Latitude = Nullable latitude,
            Longitude = Nullable longitude,
            Tags = osmTags [])

    let osmWay id nodes tags =
        Way(
            Id = Nullable id,
            Version = Nullable 1L,
            ChangeSetId = Nullable 0L,
            Visible = Nullable true,
            TimeStamp = Nullable DateTime.UnixEpoch,
            UserId = Nullable 0L,
            UserName = "",
            Nodes = nodes,
            Tags = osmTags tags)

    let osmRestriction id fromWay toWay viaNode =
        Relation(
            Id = Nullable id,
            Version = Nullable 1L,
            ChangeSetId = Nullable 0L,
            Visible = Nullable true,
            TimeStamp = Nullable DateTime.UnixEpoch,
            UserId = Nullable 0L,
            UserName = "",
            Members = [|
                RelationMember(Id = fromWay, Role = "from", Type = OsmGeoType.Way)
                RelationMember(Id = toWay, Role = "to", Type = OsmGeoType.Way)
                RelationMember(Id = viaNode, Role = "via", Type = OsmGeoType.Node)
            |],
            Tags = osmTags [ "type","restriction"; "restriction","no_left_turn" ])

    let writeOsmPbf path (objects: OsmGeo array) =
        use stream = File.Create(path)
        let target = PBFOsmStreamTarget(stream, true, Nullable<int>(), Nullable<int>())
        target.RegisterSource(new OsmEnumerableStreamSource(objects))
        target.Pull()

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
                  source = None
                  candidateObservation = None } }
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
    member _.``Reverse coordinate transform is safe under parallel fix workers``() =
        let source =
            wgs84Factory.CreatePoint(
                NetTopologySuite.Geometries.Coordinate(14.4378, 50.0755))
        let projected = pointWgs84ToEtrs89Ex source
        let conversions =
            [| 1..32 |]
            |> Array.map (fun _ ->
                Task.Run(fun () ->
                    [| 1..128 |]
                    |> Array.map (fun _ -> pointEtrs89ExToWgs84 projected)))
            |> Task.WhenAll
            |> fun task -> task.GetAwaiter().GetResult()
            |> Array.collect id
        for converted in conversions do
            Assert.AreEqual(source.X, converted.X, 1e-6)
            Assert.AreEqual(source.Y, converted.Y, 1e-6)

    [<TestMethod>]
    member _.``Packed router respects direction mode and access precedence``() =
        let path = Path.Combine(Path.GetTempPath(), $"jrutil-routing-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.0 14.0 :> OsmGeo
                osmNode 2L 50.001 14.0 :> OsmGeo
                osmNode 3L 50.002 14.0 :> OsmGeo
                osmNode 4L 50.003 14.0 :> OsmGeo
                osmWay 10L [|1L;2L;3L|]
                    [ "highway","residential"; "oneway","yes"
                      "access","no"; "bus","yes" ] :> OsmGeo
                osmWay 11L [|3L;4L|]
                    [ "railway","tram" ] :> OsmGeo
            |]
            use graph = JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            match graph.Route(JrUtil.GeoData.Osm.RoadBus, 14.0, 50.0, 14.0, 50.002) with
            | JrUtil.GeoData.Osm.Routed distance -> Assert.IsTrue(distance > 0.0)
            | value -> Assert.Fail($"Expected routed bus path, got {value}")
            match graph.Route(JrUtil.GeoData.Osm.RoadBus, 14.0, 50.002, 14.0, 50.0) with
            | JrUtil.GeoData.Osm.RoutingUnavailable _ -> ()
            | value -> Assert.Fail($"Reverse one-way route must be unavailable, got {value}")
            match graph.Route(JrUtil.GeoData.Osm.TramRouting, 14.0, 50.002, 14.0, 50.003) with
            | JrUtil.GeoData.Osm.Routed _ -> ()
            | value -> Assert.Fail($"Expected tram topology, got {value}")
            match graph.Route(JrUtil.GeoData.Osm.RoadBus, 14.0, 50.002, 14.0, 50.003) with
            | JrUtil.GeoData.Osm.RoutingUnavailable _ -> ()
            | value -> Assert.Fail($"Tram-only edge must not route a bus, got {value}")
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Packed router hard-blocks barriers``() =
        let path = Path.Combine(Path.GetTempPath(), $"jrutil-routing-barrier-{Guid.NewGuid():N}.osm.pbf")
        try
            let barrier = osmNode 2L 50.001 14.0
            barrier.Tags <- osmTags [ "barrier","bollard" ]
            writeOsmPbf path [|
                osmNode 1L 50.0 14.0 :> OsmGeo
                barrier :> OsmGeo
                osmNode 3L 50.002 14.0 :> OsmGeo
                osmWay 10L [|1L;2L;3L|] [ "highway","residential" ] :> OsmGeo
            |]
            use graph = JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            match graph.Route(JrUtil.GeoData.Osm.RoadBus, 14.0, 50.0, 14.0, 50.002) with
            | JrUtil.GeoData.Osm.RoutingUnavailable _ -> ()
            | value -> Assert.Fail($"Barrier route must be unavailable, got {value}")
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Query-specific snaps match the global snap index``() =
        let path = Path.Combine(Path.GetTempPath(), $"jrutil-routing-known-snaps-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.0 14.0 :> OsmGeo
                osmNode 2L 50.001 14.0 :> OsmGeo
                osmNode 3L 50.002 14.0 :> OsmGeo
                osmWay 10L [|1L;2L;3L|] [ "highway","residential" ] :> OsmGeo
            |]
            use globalGraph = JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            use knownGraph =
                JrUtil.GeoData.Osm.PackedRoutingGraph.OpenWithProgress(
                    path, (fun _ _ _ -> ()), buildGlobalSnaps=false)
            knownGraph.MaximumWorkers <- 2
            knownGraph.PrepareKnownSnaps(
                [|struct (14.0,50.0); struct (14.0,50.002)|], fun _ _ -> ())
            match globalGraph.Route(JrUtil.GeoData.Osm.RoadBus,14.0,50.0,14.0,50.002),
                  knownGraph.Route(JrUtil.GeoData.Osm.RoadBus,14.0,50.0,14.0,50.002) with
            | JrUtil.GeoData.Osm.Routed expected, JrUtil.GeoData.Osm.Routed actual ->
                Assert.AreEqual(expected,actual,0.001)
            | expected,actual -> Assert.Fail($"Routing mismatch: global={expected}; known={actual}")
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Packed router indexes unrelated turn restrictions``() =
        let path = Path.Combine(Path.GetTempPath(), $"jrutil-routing-restrictions-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.0 14.0 :> OsmGeo
                osmNode 2L 50.001 14.0 :> OsmGeo
                osmNode 3L 50.002 14.0 :> OsmGeo
                osmWay 10L [|1L;2L|] [ "highway","residential" ] :> OsmGeo
                osmWay 11L [|2L;3L|] [ "highway","residential" ] :> OsmGeo
                for index = 1 to 1_000 do
                    osmRestriction (int64 (100+index)) (int64 (1_000+index))
                                   (int64 (2_000+index)) 2L :> OsmGeo
            |]
            use graph = JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            match graph.Route(JrUtil.GeoData.Osm.RoadBus, 14.0, 50.0, 14.0, 50.002) with
            | JrUtil.GeoData.Osm.Routed _ -> ()
            | value -> Assert.Fail($"Expected route through unrelated restrictions, got {value}")
            Assert.IsTrue(graph.RestrictionLookups > 0L)
            assertEqual 0L graph.RestrictionRulesExamined
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``RouteViaMany shares the directed baseline across candidates``() =
        let path = Path.Combine(Path.GetTempPath(), $"jrutil-routing-via-many-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.0 14.0 :> OsmGeo
                osmNode 2L 50.001 14.0 :> OsmGeo
                osmNode 3L 50.002 14.0 :> OsmGeo
                osmNode 4L 50.003 14.0 :> OsmGeo
                osmWay 10L [|1L;2L;3L;4L|] [ "highway","residential" ] :> OsmGeo
            |]
            use graph = JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            let beforeMany = graph.Searches
            let results =
                graph.RouteViaMany(
                    JrUtil.GeoData.Osm.RoadBus, 14.0, 50.0,
                    [| struct(14.0,50.001); struct(14.0,50.002) |],
                    14.0, 50.003)
            results |> Array.iter (function
                | JrUtil.GeoData.Osm.Routed _ -> ()
                | value -> Assert.Fail($"Expected routed candidate, got {value}"))
            let sharedSearches = graph.Searches - beforeMany
            let beforeSeparate = graph.Searches
            graph.RouteVia(JrUtil.GeoData.Osm.RoadBus,14.0,50.0,14.0,50.001,14.0,50.003) |> ignore
            graph.RouteVia(JrUtil.GeoData.Osm.RoadBus,14.0,50.0,14.0,50.002,14.0,50.003) |> ignore
            let separateSearches = graph.Searches - beforeSeparate
            Assert.IsTrue(sharedSearches < separateSearches,
                          $"Shared baseline used {sharedSearches} searches versus {separateSearches}")
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Local routed face is stable across degree two OSM way splits``() =
        let unsplit=Path.Combine(Path.GetTempPath(),$"jrutil-routing-thread-unsplit-{Guid.NewGuid():N}.osm.pbf")
        let split=Path.Combine(Path.GetTempPath(),$"jrutil-routing-thread-split-{Guid.NewGuid():N}.osm.pbf")
        let nodes = [|
            osmNode 1L 50.000 14.0 :> OsmGeo
            osmNode 2L 50.001 14.0 :> OsmGeo
            osmNode 3L 50.002 14.0 :> OsmGeo
            osmNode 4L 50.003 14.0 :> OsmGeo |]
        try
            writeOsmPbf unsplit (Array.append nodes [|
                osmWay 10L [|1L;2L;3L;4L|] ["highway","residential"] :> OsmGeo |])
            writeOsmPbf split (Array.append nodes [|
                osmWay 20L [|1L;2L;3L|] ["highway","residential"] :> OsmGeo
                osmWay 21L [|3L;4L|] ["highway","residential"] :> OsmGeo |])
            use first=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(unsplit)
            use second=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(split)
            let evaluate (graph:JrUtil.GeoData.Osm.PackedRoutingGraph) =
                graph.EvaluateCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.0,50.0,
                    [|struct(14.00005,50.0015)|],14.0,50.003).candidates.[0]
            let firstEvidence=evaluate first
            let secondEvidence=evaluate second
            Assert.IsTrue(firstEvidence.localFaceId.IsSome)
            Assert.AreEqual(firstEvidence.localFaceId,secondEvidence.localFaceId)
        finally
            if File.Exists(unsplit) then File.Delete(unsplit)
            if File.Exists(split) then File.Delete(split)

    [<TestMethod>]
    member _.``Neighbouring candidate anchors recover the directed corridor``() =
        let path=Path.Combine(Path.GetTempPath(),$"jrutil-routing-multi-anchor-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.000 14.0000 :> OsmGeo
                osmNode 2L 50.001 14.0000 :> OsmGeo
                osmNode 3L 50.002 14.0000 :> OsmGeo
                osmNode 4L 50.000 14.0010 :> OsmGeo
                osmNode 5L 50.001 14.0010 :> OsmGeo
                osmNode 6L 50.002 14.0010 :> OsmGeo
                osmWay 10L [|1L;2L;3L|] ["highway","residential"] :> OsmGeo
                osmWay 11L [|4L;5L;6L|] ["highway","residential"] :> OsmGeo |]
            use graph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            let scalar =
                graph.EvaluateCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.0000,50.000,
                    [|struct(14.00105,50.001)|],14.0010,50.002)
            Assert.AreEqual("unavailable",scalar.corridor.availability)
            let anchored =
                graph.EvaluateCandidatesWithAnchors(
                    JrUtil.GeoData.Osm.RoadBus,
                    [|struct(14.0000,50.000);struct(14.0010,50.000)|],
                    [|struct(14.00105,50.001)|],
                    [|struct(14.0010,50.002)|])
            Assert.AreEqual("available",anchored.corridor.availability)
            match anchored.candidates.[0].routed with
            | JrUtil.GeoData.Osm.Routed _ -> ()
            | value -> Assert.Fail($"Expected multi-anchor route, got {value}")
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Paired anchor alternatives expose tied station corridors``() =
        let path=Path.Combine(Path.GetTempPath(),$"jrutil-routing-paired-anchors-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.000 14.0000 :> OsmGeo
                osmNode 2L 50.001 14.0000 :> OsmGeo
                osmNode 3L 50.002 14.0000 :> OsmGeo
                osmNode 4L 50.000 14.0010 :> OsmGeo
                osmNode 5L 50.001 14.0010 :> OsmGeo
                osmNode 6L 50.002 14.0010 :> OsmGeo
                osmWay 10L [|1L;2L;3L|] ["highway","residential"] :> OsmGeo
                osmWay 11L [|4L;5L;6L|] ["highway","residential"] :> OsmGeo |]
            use graph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            let evidence =
                graph.EvaluateCandidatesWithAnchors(
                    JrUtil.GeoData.Osm.RoadBus,
                    [|struct(14.0000,50.000);struct(14.0010,50.000)|],
                    [|struct(14.00005,50.001);struct(14.00105,50.001)|],
                    [|struct(14.0000,50.002);struct(14.0010,50.002)|])
            Assert.AreEqual("ambiguous",evidence.corridor.availability)
            Assert.AreEqual(2,evidence.corridor.alternativeCorridorCount)
            Assert.IsTrue(
                evidence.candidates
                |> Array.exists (fun candidate ->
                    not candidate.tiedCorridorsAgree
                    && candidate.rejectionReason.IsNone
                    && candidate.corridorDistance.IsSome))
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Raw corridor capture is deterministic complete and truncated to three``() =
        let path=Path.Combine(Path.GetTempPath(),$"jrutil-routing-raw-variants-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                for lane=0 to 3 do
                    let longitude=14.0+float lane*0.001
                    let first=int64(lane*3+1)
                    yield osmNode first 50.000 longitude :> OsmGeo
                    yield osmNode (first+1L) 50.001 longitude :> OsmGeo
                    yield osmNode (first+2L) 50.002 longitude :> OsmGeo
                    yield osmWay (10L+int64 lane) [|first;first+1L;first+2L|]
                              ["highway","residential"] :> OsmGeo |]
            use graph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            let previous=[|for lane=0 to 3 do yield struct(14.0+float lane*0.001,50.000)|]
            let points=[|for lane=0 to 3 do yield struct(14.00005+float lane*0.001,50.001)|]
            let next=[|for lane=0 to 3 do yield struct(14.0+float lane*0.001,50.002)|]
            graph.MaximumWorkers<-1
            let first=graph.CaptureContextEvidence(JrUtil.GeoData.Osm.RoadBus,previous,points,next)
            graph.MaximumWorkers<-4
            let second=graph.CaptureContextEvidence(
                JrUtil.GeoData.Osm.RoadBus,Array.rev previous,Array.rev points,Array.rev next)
            Assert.AreEqual(3,first.variants.Length)
            CollectionAssert.AreEqual([|0;1;2|],first.variants |> Array.map _.variantRank)
            Assert.IsTrue(first.variants |> Array.forall(fun value -> value.routingAvailability="available"))
            Assert.IsTrue(first.attachments |> Array.forall(fun rows -> rows.Length=points.Length))
            CollectionAssert.AreEqual(
                first.variants |> Array.map(fun value -> value.absoluteCostMetres,value.corridorId),
                second.variants |> Array.map(fun value -> value.absoluteCostMetres,value.corridorId))
            for variantIndex=0 to first.variants.Length-1 do
                Assert.IsTrue(first.attachments.[variantIndex] |> Array.forall(fun row -> row.variantRank=variantIndex))
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Packed router uses fractional edge positions and directed right side``() =
        let path = Path.Combine(Path.GetTempPath(), $"jrutil-routing-fractional-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.000 14.0 :> OsmGeo
                osmNode 2L 50.001 14.0 :> OsmGeo
                osmNode 3L 50.002 14.0 :> OsmGeo
                osmNode 4L 50.003 14.0 :> OsmGeo
                osmNode 5L 50.005 14.000 :> OsmGeo
                osmNode 6L 50.005 14.001 :> OsmGeo
                osmNode 7L 50.005 14.002 :> OsmGeo
                osmNode 8L 50.005 14.003 :> OsmGeo
                osmWay 10L [|1L;2L;3L;4L|] [ "highway","residential" ] :> OsmGeo
                osmWay 11L [|5L;6L;7L;8L|] [ "highway","residential" ] :> OsmGeo
            |]
            use graph = JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            match graph.Route(JrUtil.GeoData.Osm.RoadBus,14.0,50.0005,14.0,50.0015) with
            | JrUtil.GeoData.Osm.Routed distance ->
                Assert.IsTrue(distance > 80.0 && distance < 140.0,
                              $"Expected approximately one fractional-edge kilometre tenth, got {distance}")
            | value -> Assert.Fail($"Expected fractional route, got {value}")
            let northbound =
                graph.EvaluateCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.0,50.000,
                    [|struct(14.00005,50.0015); struct(13.99995,50.0015)|],
                    14.0,50.003)
            Assert.AreEqual("available",northbound.corridor.availability)
            Assert.IsTrue(northbound.candidates.[0].signedLateralOffset.Value < 0.0)
            Assert.AreEqual(1.0,northbound.candidates.[0].side,0.000001)
            Assert.IsTrue(northbound.candidates.[1].signedLateralOffset.Value > 0.0)
            Assert.AreEqual(0.0,northbound.candidates.[1].side,0.000001)
            Assert.AreNotEqual(northbound.candidates.[0].localFaceId,northbound.candidates.[1].localFaceId)
            let southbound =
                graph.EvaluateCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.0,50.003,
                    [|struct(14.00005,50.0015); struct(13.99995,50.0015)|],
                    14.0,50.000)
            Assert.AreEqual(0.0,southbound.candidates.[0].side,0.000001)
            Assert.AreEqual(1.0,southbound.candidates.[1].side,0.000001)
            Assert.AreEqual(northbound.candidates.[0].localFaceId,southbound.candidates.[0].localFaceId)
            Assert.AreEqual(northbound.candidates.[1].localFaceId,southbound.candidates.[1].localFaceId)
            let incomingBoundary =
                graph.EvaluateBoundaryCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.0,50.000,
                    [|struct(14.00005,50.0025); struct(13.99995,50.0025)|],false)
            let struct(_,incomingRight)=incomingBoundary.[0]
            let struct(_,incomingLeft)=incomingBoundary.[1]
            Assert.AreEqual(1.0,incomingRight.side,0.000001)
            Assert.AreEqual(0.0,incomingLeft.side,0.000001)
            let outgoingBoundary =
                graph.EvaluateBoundaryCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.0,50.003,
                    [|struct(14.00005,50.0005); struct(13.99995,50.0005)|],true)
            let struct(_,outgoingRight)=outgoingBoundary.[0]
            let struct(_,outgoingLeft)=outgoingBoundary.[1]
            Assert.AreEqual(1.0,outgoingRight.side,0.000001)
            Assert.AreEqual(0.0,outgoingLeft.side,0.000001)
            let eastbound =
                graph.EvaluateCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.000,50.005,
                    [|struct(14.0015,50.00495); struct(14.0015,50.00505)|],
                    14.003,50.005)
            Assert.IsTrue(eastbound.candidates.[0].signedLateralOffset |> Option.exists (fun value -> value < 0.0),
                          $"Expected south side to be right eastbound, corridor={eastbound.corridor}; candidate={eastbound.candidates.[0]}")
            Assert.AreEqual(1.0,eastbound.candidates.[0].side,0.000001)
            Assert.AreEqual(0.0,eastbound.candidates.[1].side,0.000001)
            let westbound =
                graph.EvaluateCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.003,50.005,
                    [|struct(14.0015,50.00495); struct(14.0015,50.00505)|],
                    14.000,50.005)
            Assert.AreEqual(0.0,westbound.candidates.[0].side,0.000001)
            Assert.AreEqual(1.0,westbound.candidates.[1].side,0.000001)
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Candidate geometry does not resolve tied parallel corridors``() =
        let path = Path.Combine(Path.GetTempPath(), $"jrutil-routing-shared-attachment-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.000 14.0000 :> OsmGeo
                osmNode 2L 50.001 14.0000 :> OsmGeo
                osmNode 3L 50.002 14.0000 :> OsmGeo
                osmNode 4L 50.000 14.0002 :> OsmGeo
                osmNode 5L 50.001 14.0002 :> OsmGeo
                osmNode 6L 50.002 14.0002 :> OsmGeo
                osmWay 10L [|1L;2L;3L|] [ "highway","residential" ] :> OsmGeo
                osmWay 11L [|4L;5L;6L|] [ "highway","residential" ] :> OsmGeo
            |]
            use graph = JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            let evidence =
                graph.EvaluateCandidates(
                    JrUtil.GeoData.Osm.RoadBus,14.0000,50.000,
                    [|struct(14.0002,50.001)|],14.0000,50.002)
            let candidate=evidence.candidates.[0]
            Assert.AreEqual(None,candidate.rejectionReason)
            Assert.IsFalse(candidate.tiedCorridorsAgree)
            Assert.IsTrue(candidate.corridorDistance.IsSome)
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``Local thread identity keeps a service bay distinct from its through road``() =
        let path = Path.Combine(Path.GetTempPath(), $"jrutil-routing-service-thread-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 50.0000 14.0000 :> OsmGeo
                osmNode 2L 50.0010 14.0000 :> OsmGeo
                osmNode 3L 50.0020 14.0000 :> OsmGeo
                osmNode 4L 50.0010 14.0003 :> OsmGeo
                osmNode 5L 50.0015 14.0003 :> OsmGeo
                osmWay 10L [|1L;2L;3L|] [ "highway","secondary" ] :> OsmGeo
                osmWay 11L [|2L;4L;5L;3L|] [ "highway","service" ] :> OsmGeo
            |]
            use graph = JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            let road = graph.InspectSnaps(JrUtil.GeoData.Osm.RoadBus,14.0000,50.0010)
            let bay = graph.InspectSnaps(JrUtil.GeoData.Osm.RoadBus,14.0003,50.00125)
            Assert.IsTrue(road.Length > 0)
            Assert.IsTrue(bay.Length > 0)
            Assert.AreNotEqual(road.[0].localThreadId.Split(':').[0],
                               bay.[0].localThreadId.Split(':').[0])
        finally
            if File.Exists(path) then File.Delete(path)

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
                      source = Some "external:test"
                      candidateObservation = None }
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
