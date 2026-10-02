// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open System.Globalization
open Microsoft.VisualStudio.TestTools.UnitTesting
open NodaTime
open OsmSharp
open OsmSharp.Streams
open OsmSharp.Tags

open JrUtil
open JrUtil.Tests.Asserts

[<TestClass>]
type JdfToGtfsTests() =
    let fixturePath =
        Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")

    let batch () =
        Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)

    /// The production streaming conversion, materialized for assertions.
    let convert (source: JdfModel.JdfBatch) =
        let preparation =
            JdfToGtfs.prepareGtfsFeedForStreamingBundleWithCalendar (JdfCalendar.prepareGtfsCalendar source) source
        let stopTimes = JdfToGtfs.getStreamingBundleStopTimes preparation |> Seq.toArray
        let feed =
            JdfToGtfs.finishStreamingFeedWithUniqueCalendars preparation
                (stopTimes |> Seq.map (fun stopTime -> stopTime.stopId) |> Set.ofSeq)
        { feed with stopTimes = stopTimes }
    let route routeId (feed: GtfsModel.GtfsFeed) =
        feed.routes |> Array.find (fun item -> item.id = routeId)

    let postEvidence stopId candidateId lat lon : JdfModel.PostCandidateEvidence = {
        stopId=stopId;candidateId=candidateId;observationId=candidateId
        sourceKind="test";sourceObjectId=Some candidateId;observedAt=None
        lat=lat;lon=lon;supportWeight=1M;rawTags="";explicitModes=""
        deniedModes="";lifecycle="active" }

    let osmTags pairs =
        let result=TagsCollection()
        for key,value in pairs do result.Add(key,value)
        result
    let osmNode id latitude longitude =
        Node(Id=Nullable id,Version=Nullable 1L,ChangeSetId=Nullable 0L,
             Visible=Nullable true,TimeStamp=Nullable DateTime.UnixEpoch,
             UserId=Nullable 0L,UserName="",Latitude=Nullable latitude,
             Longitude=Nullable longitude,Tags=osmTags [])
    let osmWay id nodes tags =
        Way(Id=Nullable id,Version=Nullable 1L,ChangeSetId=Nullable 0L,
            Visible=Nullable true,TimeStamp=Nullable DateTime.UnixEpoch,
            UserId=Nullable 0L,UserName="",Nodes=nodes,Tags=osmTags tags)
    let writeOsmPbf path (objects:OsmGeo array) =
        use stream=File.Create(path)
        let target=PBFOsmStreamTarget(stream,true,Nullable<int>(),Nullable<int>())
        target.RegisterSource(new OsmEnumerableStreamSource(objects))
        target.Pull()

    [<TestMethod>]
    member _.``Specialized stop-time writer is byte-identical to generic serialization``() =
        let rows: GtfsModel.StopTime array = [|
            {
                tripId = "trip\"quoted"
                arrivalTime = Some (Period.FromSeconds(25L * 3600L + 61L))
                departureTime = None
                stopId = "stop\"quoted"
                stopSequence = 7
                headsign = Some "head\"sign"
                pickupType = Some GtfsModel.CoordinationWithDriver
                dropoffType = Some GtfsModel.NoService
                shapeDistTraveled = Some 12.50M
                timepoint = Some GtfsModel.Exact
                stopZoneIds = Some "ignored-extension"
            }
        |]
        let projections: GtfsModel.StandardStopTime array =
            rows
            |> Array.map (fun value -> {
                tripId = value.tripId
                arrivalTime = value.arrivalTime
                departureTime = value.departureTime
                stopId = value.stopId
                stopSequence = value.stopSequence
                headsign = value.headsign
                pickupType = value.pickupType
                dropoffType = value.dropoffType
                shapeDistTraveled = value.shapeDistTraveled
                timepoint = value.timepoint
            })
        let oldCulture = CultureInfo.CurrentCulture
        try
            CultureInfo.CurrentCulture <- CultureInfo("cs-CZ")
            use expected = new MemoryStream()
            GtfsCsvSerializer.getRowsSerializerWriter<GtfsModel.StandardStopTime>
                expected projections
            use actual = new MemoryStream()
            GtfsCsvSerializer.writeStandardStopTimes actual rows
            CollectionAssert.AreEqual(expected.ToArray(), actual.ToArray())
        finally
            CultureInfo.CurrentCulture <- oldCulture

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
        let feed = batch () |> convert

        route "jdf:route:586001" feed
        |> fun item ->
            assertEqual (Some "10") item.shortName
            assertEqual (Some "0076a3") item.color
            assertEqual (Some "ffffff") item.textColor
        route "jdf:route:446002" feed
        |> fun item ->
            assertEqual (Some "N2") item.shortName
            assertEqual (Some "80166f") item.color
            assertEqual (Some "ffffff") item.textColor
        route "jdf:route:582486" feed
        |> fun item -> assertEqual (Some "486") item.shortName

        let czRoutes = feed.czRoutes |> Option.get
        let numericRoute =
            czRoutes
            |> Array.find (fun item -> item.routeId = "jdf:route:586001")
        assertEqual (Some "586001") numericRoute.cisLineId
        assertEqual (Some "10") numericRoute.publicLineNumber
        assertEqual "jdf:1.11" numericRoute.sourceProvenance

        let czTrip =
            feed.czTrips
            |> Option.get
            |> Array.find (fun item -> item.tripId = "jdf:trip:586001:260101:1")
        assertEqual "jdf:trip:586001:260101:1" czTrip.tripId
        assertEqual (Some "586001") czTrip.cisLineId
        assertEqual (Some 1L) czTrip.cisTripId
        assertEqual (Some "jdf:trip:586001:260101:1") czTrip.sourceTripIds
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

        let info = feed.feedInfo |> Option.get
        assertEqual "Oběhy project (via JrUtil)" info.publisherName
        assertEqual "https://obehy.cz" info.publisherUrl
        assertEqual "cs" info.lang
        assertEqual (Some (LocalDate(2026, 1, 1))) info.startDate
        assertEqual (Some (LocalDate(2026, 12, 31))) info.endDate
        assertEqual (Some "jdf-1.11:OBEHY-EXT:20260718") info.version
        assertEqual (Some "admin@obehy.cz") info.contactEmail

    [<TestMethod>]
    member _.``Feed contact round-trips and legacy feed info remains readable``() =
        let feed = batch () |> convert
        let root =
            Path.Combine(Path.GetTempPath(), "jrutil-feed-info-" + Guid.NewGuid().ToString("N"))
        try
            StagingFixture.writeFeed root feed
            let parsed = Gtfs.gtfsParseFolder () root
            assertEqual
                (Some "admin@obehy.cz")
                (parsed.feedInfo |> Option.bind (fun info -> info.contactEmail))
            assertEqual
                (Some "jdf-1.11:OBEHY-EXT:20260718")
                (parsed.feedInfo |> Option.bind (fun info -> info.version))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Post inference policy round-trips and enforces the evidence horizon``() =
        let root=Path.Combine(Path.GetTempPath(),$"jrutil-post-policy-{Guid.NewGuid():N}")
        Directory.CreateDirectory(root) |> ignore
        try
            let path=Path.Combine(root,"policy.json")
            let expected={ JdfPostInferencePolicy.conservativeRoutedV4 with policyId="test-policy" }
            JdfPostInferencePolicy.writePolicy path expected
            assertEqual expected ((JdfPostInferencePolicy.loadPolicyWithScorer path).policy)
            let invalid={ expected with hardGates={ expected.hardGates with maximumRoutedExcessMetres=1001.0 } }
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostInferencePolicy.validatePolicy 1000.0 invalid |> ignore)
            |> ignore
        finally
            Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Post inference policy keeps topology failures invariant``() =
        let policy=JdfPostInferencePolicy.conservativeRoutedV4
        assertEqual (Some "search-limit")
            (JdfPostInference.hardGateReason policy (Some "search-limit") None None (Some 0.0))
        assertEqual (Some "routed-excess")
            (JdfPostInference.hardGateReason policy None (Some 5.0) (Some -5.0) (Some 501.0))

    [<TestMethod>]
    member _.``Post inference policy v2 rejects unsafe numeric domains``() =
        let policy=JdfPostInferencePolicy.conservativeRoutedV4
        let zeroExcess={ policy with hardGates={policy.hardGates with maximumRoutedExcessMetres=0.0} }
        Assert.ThrowsExactly<ArgumentException>(fun () ->
            JdfPostInferencePolicy.validatePolicy 1000.0 zeroExcess |> ignore) |> ignore
        let inverted={ policy with resolution={policy.resolution with
                                                   minimumPlausibleScore=0.8
                                                   minimumPhysicalScore=0.7} }
        Assert.ThrowsExactly<ArgumentException>(fun () ->
            JdfPostInferencePolicy.validatePolicy 1000.0 inverted |> ignore) |> ignore
        let nonFinite={ policy with spatialIsolation={policy.spatialIsolation with
                                                          maximumAdjustment=Double.NaN} }
        Assert.ThrowsExactly<ArgumentException>(fun () ->
            JdfPostInferencePolicy.validatePolicy 1000.0 nonFinite |> ignore) |> ignore

    [<TestMethod>]
    member _.``Post observation validation rejects duplicates and malformed facts``() =
        let observation:JdfModel.PostCandidateEvidence = {
            stopId=1L;candidateId="candidate";observationId="source:1"
            sourceKind="test";sourceObjectId=Some "source:1";observedAt=None
            lat=50M;lon=14M;supportWeight=1M;rawTags=""
            explicitModes="road";deniedModes="";lifecycle="active" }
        Assert.ThrowsExactly<ArgumentException>(fun () ->
            JdfModel.validatePostCandidateEvidence [|observation;observation|]) |> ignore
        Assert.ThrowsExactly<ArgumentException>(fun () ->
            JdfModel.validatePostCandidateEvidence [|{observation with supportWeight=0M}|]) |> ignore

    [<TestMethod>]
    member _.``Shared inactive schedules delete every source trip``() =
        let source = batch ()
        let first = source.trips.[0]
        let second = { first with id = 987654L }
        let route = source.routes |> Array.find (fun route -> route.id = first.routeId && route.idDistinction = first.routeDistinction)
        let note: JdfModel.ServiceNote = {
            routeId = first.routeId; routeDistinction = first.routeDistinction; tripId = first.id
            id = 1L; designation = "closed"; noteType = Some JdfModel.NoService
            dateFrom = Some route.timetableValidFrom; dateTo = Some route.timetableValidTo; note = None }
        let prepared =
            { source with trips = [| first; second |]; serviceNotes = [| note; { note with tripId = second.id } |] }
            |> JdfCalendar.prepareGtfsCalendarWithWorkers 4
        assertEqual
            (set [JdfGtfsRules.jdfTripId source first.routeId first.routeDistinction first.id
                  JdfGtfsRules.jdfTripId source second.routeId second.routeDistinction second.id])
            prepared.tripsToDelete
        Assert.IsTrue(prepared.schedules.IsEmpty)

    [<TestMethod>]
    member _.``Shared calendars are identical across workers``() =
        let source = batch ()
        // More than one preparation chunk, repeated schedules and reversed IDs.
        let trips =
            [| for index in 0 .. 4999 do
                   yield { source.trips.[index % source.trips.Length] with id = int64 (5000 - index) } |]
        let calls =
            [| for index in 0 .. trips.Length - 1 do
                   let original = source.trips.[index % source.trips.Length]
                   for call in source.tripStops do
                       if call.routeId = original.routeId && call.routeDistinction = original.routeDistinction
                          && call.tripId = original.id then
                           yield { call with tripId = trips.[index].id } |]
        let notes =
            [| for index in 0 .. trips.Length - 1 do
                   let original = source.trips.[index % source.trips.Length]
                   for note in source.serviceNotes do
                       if note.routeId = original.routeId && note.routeDistinction = original.routeDistinction
                          && note.tripId = original.id then
                           yield { note with tripId = trips.[index].id } |]
        let expanded = { source with trips = trips; tripStops = calls; serviceNotes = notes }
        let unique workers =
            let calendar = JdfCalendar.prepareGtfsCalendarWithWorkers workers expanded
            JdfToGtfs.finishStreamingFeedWithUniqueCalendars
                (JdfToGtfs.prepareGtfsFeedForStreamingBundleWithCalendar calendar expanded) Set.empty
        let legacy = unique 1
        for workers in [1; 4] do
            let actual = unique workers
            assertEqual legacy.calendar actual.calendar
            assertEqual legacy.calendarExceptions actual.calendarExceptions
            assertEqual
                (legacy.trips |> Array.map (fun trip -> trip.id, trip.serviceId))
                (actual.trips |> Array.map (fun trip -> trip.id, trip.serviceId))



    [<TestMethod>]
    member _.``Timed calls at border-only stops are kept without boarding or alighting``() =
        let source = batch ()
        let borderStop =
            source.tripStops
            |> Seq.find (fun call ->
                match call.departureTime, call.arrivalTime with
                | Some (JdfModel.StopTime _), _ | _, Some (JdfModel.StopTime _) -> true
                | _ -> false)
            |> fun call -> call.stopId
        let attributeId = 999
        let border = {
            source with
                attributeRefs =
                    Array.append source.attributeRefs [|
                        { attributeId = attributeId; value = JdfModel.BorderStopOnly
                          reserved1 = None }
                    |]
                stops =
                    source.stops
                    |> Array.map (fun stop ->
                        if stop.id = borderStop then
                            { stop with attributes = Array.append [| Some attributeId |] (Array.create 5 None) }
                        else stop)
        }
        let key (call: GtfsModel.StopTime) = call.tripId, call.stopSequence
        let before = (convert source).stopTimes
        let after = (convert border).stopTimes
        assertEqual (before |> Array.map key) (after |> Array.map key)
        let isBorderCall (call: GtfsModel.StopTime) =
            let prefix = JdfGtfsRules.jdfStopId borderStop
            call.stopId = prefix || call.stopId.StartsWith(prefix + ":")
        let borderCalls = after |> Array.filter isBorderCall
        Assert.IsTrue(borderCalls.Length > 0)
        for call in borderCalls do
            assertEqual (Some GtfsModel.NoService) call.pickupType
            assertEqual (Some GtfsModel.NoService) call.dropoffType
        let unchanged calls = calls |> Array.filter (isBorderCall >> not)
        assertEqual (unchanged before) (unchanged after)

    [<TestMethod>]
    member _.``JDF routes receive default colors by transport mode``() =
        let sourceRoute = (batch ()).routes.[0]
        let colors mode publicLineNumber =
            { sourceRoute with transportMode = mode }
            |> JdfGtfsRules.getGtfsRouteColors publicLineNumber

        assertEqual (Some "0076a3", Some "ffffff")
                    (colors JdfModel.Bus None)
        let classifiedBusColors routeType =
            { sourceRoute with transportMode = JdfModel.Bus; routeType = routeType }
            |> JdfGtfsRules.getGtfsRouteColors None
        assertEqual (Some "004f71", Some "ffffff")
                    (classifiedBusColors JdfModel.International)
        assertEqual (Some "004f71", Some "ffffff")
                    (classifiedBusColors JdfModel.InternationalNoNational)
        assertEqual (Some "004f71", Some "ffffff")
                    (classifiedBusColors JdfModel.InternationalOrNational)
        assertEqual (Some "004f71", Some "ffffff")
                    (classifiedBusColors JdfModel.LongDistanceNational)
        assertEqual (Some "00695c", Some "ffffff")
                    (classifiedBusColors JdfModel.RegionalInternational)
        assertEqual (Some "0076a3", Some "ffffff")
                    (classifiedBusColors JdfModel.Regional)
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
    member _.``Extended bus types reserve coach for long-distance routes``() =
        let route = (batch ()).routes.[0]
        let routeType value = { route with routeType = value } |> JdfGtfsRules.getGtfsRouteType
        assertEqual "704" (routeType JdfModel.City)
        assertEqual "704" (routeType JdfModel.CityAndAdjacent)
        assertEqual "701" (routeType JdfModel.Regional)
        assertEqual "701" (routeType JdfModel.ExtraDistrict)
        assertEqual "701" (routeType JdfModel.ExtraRegional)
        assertEqual "701" (routeType JdfModel.RegionalInternational)
        assertEqual "202" (routeType JdfModel.LongDistanceNational)

    [<TestMethod>]
    member _.``Reviewed transport mode rule requires every guard``() =
        let source = batch ()
        let route = { source.routes.[0] with id = "915001"; agencyId = "61974757"; transportMode = JdfModel.Bus }
        let rule: JdfGtfsRules.TransportModeRule = {
            agencyId = "61974757"; routeIdFrom = 915001; routeIdTo = 915019
            publicLineFrom = 1; publicLineTo = 19; expectedMode = JdfModel.Bus
            effectiveMode = JdfModel.Tram; reason = "reviewed Ostrava tram family" }
        let corrected, decisions =
            JdfGtfsRules.applyTransportModeRules { sha256 = None; rules = [| rule |] }
                { source with routes = [| route |]; routeIntegrations = [||] }
        assertEqual JdfModel.Tram corrected.routes.[0].transportMode
        assertEqual true (decisions |> Array.exactlyOne).corrected

        let unchanged, mismatch =
            JdfGtfsRules.applyTransportModeRules { sha256 = None; rules = [| rule |] }
                { source with routes = [| { route with transportMode = JdfModel.Trolleybus } |]; routeIntegrations = [||] }
        assertEqual JdfModel.Trolleybus unchanged.routes.[0].transportMode
        assertEqual false (mismatch |> Array.exactlyOne).corrected

    [<TestMethod>]
    member _.``Transport mode rule CSV is validated and checksummed``() =
        let path = Path.Combine(Path.GetTempPath(), $"transport-mode-rules-{Guid.NewGuid():N}.csv")
        try
            File.WriteAllText(path,
                "agency_id,route_id_from,route_id_to,public_line_from,public_line_to,expected_mode,effective_mode,reason\n"
                + "61974757,915001,915019,1,19,A,E,Reviewed Ostrava tram family\n")
            let loaded = JdfGtfsRules.loadTransportModeRules path
            let rule = loaded.rules |> Array.exactlyOne
            assertEqual true loaded.sha256.IsSome
            assertEqual "61974757" rule.agencyId
            assertEqual JdfModel.Bus rule.expectedMode
            assertEqual JdfModel.Tram rule.effectiveMode
        finally
            if File.Exists(path) then File.Delete(path)

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
            ((JdfGtfsRules.getPublicLineNumbers ambiguous).["586001", 1])
        assertEqual
            None
            ((JdfGtfsRules.getPublicLineNumbers malformed).["BUS-1", 1])

    [<TestMethod>]
    member _.``JDF conversion normalizes zones and preserves both post forms``() =
        let feed = batch () |> convert
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
                stopTime.tripId = "jdf:trip:586001:260101:1")
            |> Array.sortBy (fun stopTime -> stopTime.stopSequence)
        assertEqual "jdf:stop:100:post:1" numericStopTimes.[0].stopId
        assertEqual None numericStopTimes.[0].stopZoneIds
        assertEqual "jdf:stop:200:unspecified" numericStopTimes.[1].stopId
        let formalStopTimes =
            feed.stopTimes
            |> Array.filter (fun stopTime ->
                stopTime.tripId = "jdf:trip:446002:260101:1")
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
        let feed = withLocations |> convert
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
    member _.``Legacy geometry cannot position authored posts without routed evidence``() =
        let source = batch ()
        let inferred = {
            source with
                stopLocations = [|
                    { stopId = 100L; lat = 50.0M; lon = 14.0M; precision = JdfModel.StopPrecise }
                    { stopId = 200L; lat = 50.01M; lon = 14.0M; precision = JdfModel.StopPrecise }
                |]
        }

        let feed = inferred |> convert
        let authored = feed.stops |> Array.find (fun stop -> stop.id = "jdf:stop:100:post:1")
        Assert.AreEqual(50.0, float authored.lat.Value, 0.000001)
        Assert.AreEqual(14.0, float authored.lon.Value, 0.000001)
        assertEqual (Some "1") authored.platformCode

    [<TestMethod>]
    member _.``Raw capture is identical across input order workers and forced spill``() =
        let path=Path.Combine(Path.GetTempPath(),$"jrutil-post-capture-determinism-{Guid.NewGuid():N}.osm.pbf")
        try
            writeOsmPbf path [|
                osmNode 1L 49.99 14.0 :> OsmGeo
                osmNode 2L 50.00 14.0 :> OsmGeo
                osmNode 3L 50.01 14.0 :> OsmGeo
                osmNode 4L 50.02 14.0 :> OsmGeo
                osmWay 10L [|1L;2L;3L;4L|] ["highway","residential"] :> OsmGeo |]
            use graph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(path)
            let source=batch()
            let fixture={ source with
                            stopLocations=[|
                                {stopId=100L;lat=50.0M;lon=14.0M;precision=JdfModel.StopPrecise}
                                {stopId=200L;lat=50.01M;lon=14.0M;precision=JdfModel.StopPrecise}|]
                            postCandidateEvidence=[|
                                postEvidence 100L "east" 50.0M 14.00005M
                                postEvidence 100L "west" 50.0M 13.99995M|] }
            let reversed={fixture with
                            tripStops=Array.rev (fixture.tripStops |> Seq.toArray)
                            postCandidateEvidence=Array.rev fixture.postCandidateEvidence}
            let capture workers budget input =
                JdfPostEvidence.captureToStore
                    {maximumWorkers=workers;memoryBudgetBytes=budget
                     preflight=ignore
                     progress=(fun _ _ _ _ -> ());routingCache=None} graph input
            use baseline=capture 1 Int64.MaxValue fixture
            use manyWorkers=capture 4 Int64.MaxValue fixture
            use reordered=capture 4 Int64.MaxValue reversed
            use spilled=capture 4 1L reversed
            let compare (left:JdfPostEvidence.CapturedPostEvidence)
                        (right:JdfPostEvidence.CapturedPostEvidence) =
                assertEqual (left.observations.ReadRows() |> Seq.toArray)
                            (right.observations.ReadRows() |> Seq.toArray)
                assertEqual (left.routePoints.ReadRows() |> Seq.toArray)
                            (right.routePoints.ReadRows() |> Seq.toArray)
                assertEqual (left.contexts.ReadRows() |> Seq.toArray)
                            (right.contexts.ReadRows() |> Seq.toArray)
                assertEqual (left.corridorVariants.ReadRows() |> Seq.toArray)
                            (right.corridorVariants.ReadRows() |> Seq.toArray)
                assertEqual (left.routePointEvidence.ReadRows() |> Seq.toArray)
                            (right.routePointEvidence.ReadRows() |> Seq.toArray)
            compare baseline manyWorkers
            compare baseline reordered
            compare baseline spilled
            assertEqual 0L baseline.CurrentSpillBytes
            Assert.IsTrue(spilled.CurrentSpillBytes>0L)
            Assert.IsTrue(spilled.PeakSpillBytes>=spilled.CurrentSpillBytes)
        finally
            if File.Exists(path) then File.Delete(path)

    [<TestMethod>]
    member _.``A sole coordinate candidate does not create an inferred child``() =
        let source = batch ()
        let inferred = {
            source with
                postCandidateEvidence = [| postEvidence 200L "only" 50.01M 14.0M |]
        }
        let feed = inferred |> convert
        assertEqual false (feed.stops |> Array.exists (fun stop -> stop.id.Contains(":estimated:")))





    member private _.routeVersions (versions: (bool * string * LocalDate) list) =
        let template = batch ()
        let original = template.routes |> Array.find (fun route -> route.id = "586001")
        let routes =
            versions
            |> List.mapi (fun index (detour, name, validFrom) ->
                { original with
                    idDistinction = index + 1
                    detour = detour
                    name = name
                    timetableValidFrom = validFrom })
            |> List.toArray
        { template with
            routes = Array.append (template.routes |> Array.filter (fun route -> route.id <> "586001")) routes
            routeIntegrations = template.routeIntegrations |> Array.filter (fun value -> value.routeId <> "586001") }

    [<TestMethod>]
    member this.``Route versions collapse into one regular and one detour route``() =
        let name = (batch ()).routes |> Array.find (fun route -> route.id = "586001") |> fun route -> route.name
        let versions = this.routeVersions [
            false, name, LocalDate(2026, 1, 1)
            true, name, LocalDate(2026, 2, 1)
            true, name, LocalDate(2026, 3, 1)
            false, name, LocalDate(2026, 4, 1)
            true, name, LocalDate(2026, 5, 1) ]
        let grouping = JdfGtfsRules.getRouteGrouping versions
        let idOf distinction = grouping.routeIds.[struct ("586001", distinction)]
        assertEqual [ "jdf:route:586001"; "jdf:route:586001:detour"; "jdf:route:586001:detour"
                      "jdf:route:586001"; "jdf:route:586001:detour" ]
                    ([ 1 .. 5 ] |> List.map idOf)
        let routes =
            JdfToGtfs.getGtfsRoutes versions
            |> Array.filter (fun route -> route.id.StartsWith("jdf:route:586001"))
            |> Array.sortBy (fun route -> route.id)
        assertEqual [| "jdf:route:586001"; "jdf:route:586001:detour" |] (routes |> Array.map (fun route -> route.id))
        assertEqual routes.[0].color routes.[1].color
        assertEqual routes.[0].textColor routes.[1].textColor
        assertEqual None routes.[0].description
        assertEqual (Some JdfGtfsRules.detourRouteDescription) routes.[1].description

    [<TestMethod>]
    member this.``A later version with different semantics gets a hashed route id``() =
        let versions = this.routeVersions [
            false, "Old name", LocalDate(2026, 1, 1)
            false, "New name", LocalDate(2026, 6, 1) ]
        let grouping = JdfGtfsRules.getRouteGrouping versions
        assertEqual "jdf:route:586001" grouping.routeIds.[struct ("586001", 1)]
        let renamed = grouping.routeIds.[struct ("586001", 2)]
        Assert.IsTrue(
            Text.RegularExpressions.Regex.IsMatch(renamed, "^jdf:route:586001:[0-9a-f]{8}$"),
            renamed)

    [<TestMethod>]
    member _.``Route stop keys stay unique across versions of one route``() =
        let first = JrUtil.Serving.Identity.routeStopKey "jdf:route:586001" "1" "3"
        let second = JrUtil.Serving.Identity.routeStopKey "jdf:route:586001" "2" "3"
        Assert.AreNotEqual(first, second)
        assertEqual "jdf:route:586001/3" (JrUtil.Serving.Identity.routeStopKey "jdf:route:586001" "" "3")
