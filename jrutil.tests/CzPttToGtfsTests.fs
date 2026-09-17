// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open System.Text.Json
open Microsoft.VisualStudio.TestTools.UnitTesting
open NodaTime
open OsmSharp
open OsmSharp.Streams
open OsmSharp.Tags
open Parquet.Serialization

open JrUtil

[<TestClass>]
type CzPttToGtfsTests() =
    let osmTags pairs =
        let result = TagsCollection()
        for key, value in pairs do result.Add(key, value)
        result

    let osmNode id latitude longitude nodeTags =
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
            Tags = osmTags nodeTags)

    let writeOsmPbf path (objects: OsmGeo array) =
        use stream = File.Create(path)
        let target =
            PBFOsmStreamTarget(
                stream, true, Nullable<int>(), Nullable<int>())
        target.RegisterSource(new OsmEnumerableStreamSource(objects))
        target.Pull()

    let writeMessage (path: string) (value: CzPttXml.CzpttcisMessage) =
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        use writer = new StreamWriter(path)
        serializer.Serialize(writer, value)

    let location code name time activities subsidiary parameters =
        let activityXml =
            activities
            |> List.map (fun activity ->
                $"<TrainActivity><TrainActivityType>{activity}</TrainActivityType></TrainActivity>")
            |> String.concat ""
        let subsidiaryXml =
            subsidiary
            |> Option.map (fun (rawCode, friendlyName) ->
                let nameXml =
                    friendlyName
                    |> Option.map (fun value ->
                        $"<LocationSubsidiaryName>{value}</LocationSubsidiaryName>")
                    |> Option.defaultValue ""
                $"<LocationSubsidiaryIdentification><LocationSubsidiaryCode " +
                $"LocationSubsidiaryTypeCode=\"01\">{rawCode}</LocationSubsidiaryCode>" +
                $"{nameXml}</LocationSubsidiaryIdentification>")
            |> Option.defaultValue ""
        let parameterXml =
            parameters
            |> List.map (fun (parameterName, value) ->
                $"<NetworkSpecificParameter><Name>{parameterName}</Name>" +
                $"<Value>{value}</Value></NetworkSpecificParameter>")
            |> String.concat ""
        $"<CZPTTLocation><Location><CountryCodeISO>CZ</CountryCodeISO>" +
        $"<LocationPrimaryCode>{code}</LocationPrimaryCode>" +
        $"<PrimaryLocationName>{name}</PrimaryLocationName>{subsidiaryXml}</Location>" +
        $"<TimingAtLocation><Timing TimingQualifierCode=\"ALA\"><Time>{time}.0000000+01:00</Time>" +
        "<Offset>0</Offset></Timing><Timing TimingQualifierCode=\"ALD\">" +
        $"<Time>{time}.0000000+01:00</Time><Offset>0</Offset></Timing></TimingAtLocation>" +
        "<ResponsibleRU>54</ResponsibleRU><OperationalTrainNumber>01234</OperationalTrainNumber>" +
        $"{activityXml}{parameterXml}</CZPTTLocation>"

    let message locations rootParameters =
        let rootXml =
            rootParameters
            |> List.map (fun (parameterName, value) ->
                $"<NetworkSpecificParameter><Name>{parameterName}</Name>" +
                $"<Value>{value}</Value></NetworkSpecificParameter>")
            |> String.concat ""
        let xml =
            "<CZPTTCISMessage><Identifiers>" +
            "<PlannedTransportIdentifiers><ObjectType>TR</ObjectType><Company>54</Company>" +
            "<Core>000000001234</Core><Variant>00</Variant><TimetableYear>2026</TimetableYear>" +
            "</PlannedTransportIdentifiers>" +
            "<PlannedTransportIdentifiers><ObjectType>PA</ObjectType><Company>54</Company>" +
            "<Core>000000001234</Core><Variant>00</Variant><TimetableYear>2026</TimetableYear>" +
            "</PlannedTransportIdentifiers></Identifiers><CZPTTCreation>2025-12-01T00:00:00</CZPTTCreation>" +
            "<CZPTTInformation><PlannedCalendar><BitmapDays>1</BitmapDays><ValidityPeriod>" +
            "<StartDateTime>2025-12-14T00:00:00</StartDateTime>" +
            "<EndDateTime>2025-12-14T00:00:00</EndDateTime></ValidityPeriod></PlannedCalendar>" +
            String.concat "" locations + "</CZPTTInformation>" + rootXml + "</CZPTTCISMessage>"
        match CzPtt.parseText xml with
        | CzPtt.Timetable value -> value
        | _ -> failwith "Expected timetable"

    let setCommercialType code (value: CzPttXml.CzpttcisMessage) =
        value.CzpttInformation.CzpttLocation
        |> Array.iter (fun location -> location.CommercialTrafficType <- code)
        value

    let setCore core (value: CzPttXml.CzpttcisMessage) =
        value.Identifiers
        |> Array.iter (fun identifier -> identifier.Core <- core)
        value

    let setOperationalTrainNumber number (value: CzPttXml.CzpttcisMessage) =
        value.CzpttInformation.CzpttLocation
        |> Array.iter (fun location -> location.OperationalTrainNumber <- number)
        value

    let setCalendarBitmap bitmap (value: CzPttXml.CzpttcisMessage) =
        value.CzpttInformation.PlannedCalendar.BitmapDays <- bitmap
        value

    let catalog = {
        CzPttToGtfs.emptyCatalog with
            lines = [|
                {
                    code = "101"
                    mark = "S1"
                    name = "Praha – Kolín"
                    validFrom = Some (LocalDate(2025, 12, 14))
                    validTo = Some (LocalDate(2026, 12, 12))
                }
                {
                    code = "112"
                    mark = "S12"
                    name = "Poříčany – Nymburk"
                    validFrom = Some (LocalDate(2025, 12, 14))
                    validTo = Some (LocalDate(2026, 12, 12))
                }
            |]
            companies = [|
                { code = "54"; name = "České dráhy"; url = None }
                { code = "80"; name = "DB"; url = None }
            |]
            ids = [|
                {
                    code = "11"
                    abbreviation = "PID_PrahaP"
                    name = "PID"
                    note = Some "PID pásmo P"
                    validFrom = Some (LocalDate(2025, 12, 14))
                    validTo = Some (LocalDate(2026, 12, 12))
                }
                {
                    code = "12"
                    abbreviation = "PID_PrahaB"
                    name = "PID"
                    note = Some "PID pásmo B"
                    validFrom = Some (LocalDate(2025, 12, 14))
                    validTo = Some (LocalDate(2026, 12, 12))
                }
            |]
            commercialTrainTypes =
                Array.append
                    [| { code = "84"; abbreviation = "Os" } |]
                    ([|
                        "EC"; "IC"; "LE"; "RJ"; "rj"; "SC"; "AEx"
                        "EN"; "NJ"; "ES"
                        "Ex"; "Rx"; "R"
                        "Os"; "Sp"; "TLX"; "TL"; "LET"
                    |]
                    |> Array.map (fun abbreviation ->
                        { code = abbreviation; abbreviation = abbreviation }))
            trainTypes =
                [| "Os"; "Sp"; "R"; "Ex" |]
                |> Array.map (fun abbreviation ->
                    { code = abbreviation; abbreviation = abbreviation })
    }

    [<TestMethod>]
    member _.``Catalog note list is additive and old snapshots remain readable``() =
        let root = Path.Combine(Path.GetTempPath(), $"jrutil-catalog-{Guid.NewGuid():N}")
        Directory.CreateDirectory(root) |> ignore
        try
            let oldPath = Path.Combine(root, "old.json")
            File.WriteAllText(
                oldPath,
                "{\"schema_version\":1,\"companies\":[],\"ids\":[],\"lines\":[]}")
            Assert.AreEqual(0, (CzPttToGtfs.loadCatalogSnapshot oldPath).centralNotes.Length)

            let currentPath = Path.Combine(root, "current.json")
            File.WriteAllText(
                currentPath,
                "{\"schema_version\":1,\"companies\":[],\"ids\":[],\"lines\":[]," +
                "\"central_notes\":[{\"code\":\"36\",\"name\":\"Bicycles prohibited\"," +
                "\"text\":\"No bicycle carriage\",\"valid_from\":\"2025-12-14\"," +
                "\"valid_to\":\"2026-12-12\"}]}")
            let notes = (CzPttToGtfs.loadCatalogSnapshot currentPath).centralNotes
            Assert.AreEqual(1, notes.Length)
            Assert.AreEqual("36", notes.[0].code)
            Assert.AreEqual(Some "No bicycle carriage", notes.[0].text)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Repeated root and location network parameters survive parsing``() =
        let value =
            message
                [ location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112"; "Other", "value" ] ]
                [ "CZIPTS", "11|CZ57076||CZ57076||"; "CZCalendarIPTS", "1|x" ]
        Assert.AreEqual(2, value.NetworkSpecificParameter.Length)
        Assert.AreEqual(2, value.CzpttInformation.CzpttLocation.[0].NetworkSpecificParameter.Length)

    [<TestMethod>]
    member _.``Operational point mode controls only internal GTFS timing points``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Výhybna" "08:05:00" [] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Kolín" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57017" "Deadhead" "08:25:00" [] None []
            ] []
        let gtfs = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        let sidecar = CzPttToGtfs.convert catalog CzPttToGtfs.Sidecar [ value ]
        Assert.AreEqual(3, gtfs.feed.stopTimes.Length)
        Assert.AreEqual(2, sidecar.feed.stopTimes.Length)
        Assert.AreEqual(4, gtfs.operationalCalls.Length)
        let internalTime = gtfs.feed.stopTimes.[1]
        Assert.AreEqual(Some GtfsModel.NoService, internalTime.pickupType)
        Assert.AreEqual(Some GtfsModel.NoService, internalTime.dropoffType)

    [<TestMethod>]
    member _.``Platform name stays unchanged and friendly code is platform_code``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"]
                    (Some ("12", Some "12S")) [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] []
        let feed = (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        let platform = feed.stops |> Array.find (fun stop -> stop.platformCode.IsSome)
        Assert.AreEqual("Praha hl.n.", platform.name)
        Assert.AreEqual(Some "12S", platform.platformCode)

    [<TestMethod>]
    member _.``Stops and generated entities use the CZPTT namespace and JDF hierarchy``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"]
                    (Some ("12", Some "12S")) []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] []
        let feed = (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual(4, feed.stops.Length)
        let stationIds =
            feed.stops
            |> Array.filter (fun stop -> stop.locationType = Some GtfsModel.Station)
            |> Array.map (fun stop -> stop.id)
            |> Set
        let children =
            feed.stops
            |> Array.filter (fun stop -> stop.locationType = Some GtfsModel.Stop)
        Assert.AreEqual(2, stationIds.Count)
        Assert.IsTrue(
            children
            |> Array.forall (fun stop ->
                stop.id.StartsWith("czptt:stop:", StringComparison.Ordinal)
                && stop.parentStation |> Option.exists stationIds.Contains))
        Assert.IsTrue(
            feed.stopTimes
            |> Array.forall (fun stopTime ->
                children |> Array.exists (fun stop -> stop.id = stopTime.stopId)))
        Assert.IsTrue(
            feed.agencies
            |> Array.forall (fun agency ->
                agency.id
                |> Option.exists (fun id ->
                    id.StartsWith("czptt:agency:", StringComparison.Ordinal))))
        Assert.IsTrue(
            feed.routes
            |> Array.forall (fun route ->
                route.id.StartsWith("czptt:route:", StringComparison.Ordinal)))
        Assert.IsTrue(
            feed.trips
            |> Array.forall (fun trip ->
                trip.id.StartsWith("czptt:trip:", StringComparison.Ordinal)
                && trip.serviceId.StartsWith("czptt:service:", StringComparison.Ordinal)
                && trip.blockId.Value.StartsWith("czptt:block:", StringComparison.Ordinal)))

    [<TestMethod>]
    member _.``Rail categories receive the selected colors``() =
        let cases =
            [|
                [| "Os"; "Sp"; "TL"; "TLX"; "LET" |], "106", "1c1745"
                [| "EC"; "IC"; "LE"; "RJ"; "rj"; "SC"; "AEx" |], "102", "b91c1c"
                [| "Ex"; "Rx"; "R" |], "103", "b45309"
                [| "EN"; "NJ"; "ES" |], "105", "4c1d95"
            |]
        for categories, expectedRouteType, expectedColor in cases do
            for category in categories do
                let value =
                    message [
                        location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                        location "57016" "Kolín" "08:20:00" ["0001"] None []
                    ] []
                    |> setCommercialType category
                let route =
                    (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ])
                        .feed.routes.[0]
                Assert.AreEqual(expectedRouteType, route.routeType, category)
                Assert.AreEqual(Some expectedColor, route.color, category)
                Assert.AreEqual(Some "ffffff", route.textColor, category)
        let unknown =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] []
        let unknownRoute =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ unknown ]).feed.routes.[0]
        Assert.AreEqual("100", unknownRoute.routeType)
        Assert.AreEqual(Some "475569", unknownRoute.color)
        Assert.AreEqual(Some "ffffff", unknownRoute.textColor)

    [<TestMethod>]
    member _.``Fallback routes use train designations and retain raw train numbers``() =
        let outbound =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57050" "Pardubice" "09:00:00" ["0001"] None []
                location "54357" "Břeclav" "10:00:00" ["0001"] None []
            ] []
            |> setCommercialType "RJ"
            |> setCore "000000000001"
        let inbound =
            message [
                location "54357" "Břeclav" "11:00:00" ["0001"] None []
                location "57076" "Praha hl.n." "13:00:00" ["0001"] None []
            ] []
            |> setCommercialType "RJ"
            |> setCore "000000000002"
        let feed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ inbound; outbound ]).feed
        Assert.AreEqual(1, feed.routes.Length)
        let route = feed.routes.[0]
        Assert.IsTrue(
            route.id.StartsWith("czptt:route:fallback:", StringComparison.Ordinal))
        Assert.AreEqual(Some "RJ 01234", route.shortName)
        Assert.AreEqual(None, route.longName)
        Assert.IsTrue(
            feed.trips
            |> Array.forall (fun trip ->
                trip.routeId = route.id && trip.shortName = Some "RJ 01234"))
        CollectionAssert.AreEquivalent(
            [| "Praha hl.n."; "Břeclav" |],
            feed.trips |> Array.choose (fun trip -> trip.headsign))
        Assert.IsTrue(
            feed.czTrips.Value
            |> Array.forall (fun trip -> trip.trainNumber = Some "01234"))
        Assert.AreEqual(None, feed.czRoutes.Value.[0].publicLineNumber)

    [<TestMethod>]
    member _.``Mapped route names are right-trimmed and feed contact is serialized``() =
        let trailingCatalog = {
            catalog with
                lines =
                    catalog.lines
                    |> Array.map (fun line ->
                        if line.code = "101"
                        then { line with name = "  Praha  –  Kolín  " }
                        else line)
        }
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Kolín" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
        let feed =
            (CzPttToGtfs.convert trailingCatalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual(Some "  Praha  –  Kolín", feed.routes.[0].longName)
        let info = feed.feedInfo.Value
        Assert.AreEqual("Oběhy project (via JrUtil)", info.publisherName)
        Assert.AreEqual("https://obehy.cz", info.publisherUrl)
        Assert.AreEqual(Some "czptt:20251201T000000", info.version)
        Assert.AreEqual(Some "admin@obehy.cz", info.contactEmail)

        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-feed-info-{Guid.NewGuid():N}")
        try
            Gtfs.gtfsFeedToFolder () root feed
            let lines = File.ReadAllLines(Path.Combine(root, "feed_info.txt"))
            Assert.IsTrue(lines.[0].Contains("feed_contact_email"))
            Assert.IsTrue(lines.[1].Contains("\"czptt:20251201T000000\""))
            Assert.IsTrue(lines.[1].Contains("\"admin@obehy.cz\""))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Spill-backed CZPTT merger replays messages and removes its spool``() =
        let root = Path.Combine(Path.GetTempPath(), $"jrutil-czptt-spill-{Guid.NewGuid():N}")
        Directory.CreateDirectory(root) |> ignore
        let spill = Path.Combine(root, "messages.tmp")
        try
            let value =
                message
                    [ location "57076" "Praha" "10:00:00" ["0001"] None []
                      location "57016" "Kolín" "11:00:00" ["0001"] None [] ]
                    []
            let paid =
                value.Identifiers
                |> Seq.find (fun identifier -> identifier.ObjectType = CzPttXml.ObjectType.Pa)
                |> CzPtt.identifierStr
            do
                use merger = new CzPttMerge.CzPttMerger(spill)
                merger.Add(value)
                Assert.IsTrue(merger.SpillBytes > 0L)
                let replayed = merger.SurvivingMessages |> Seq.exactlyOne
                let replayedPaid =
                    replayed.Identifiers
                    |> Seq.find (fun identifier -> identifier.ObjectType = CzPttXml.ObjectType.Pa)
                    |> CzPtt.identifierStr
                Assert.AreEqual(paid, replayedPaid)
            Assert.IsFalse(File.Exists(spill))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Category changes split linked trips with the real terminus``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57050" "Poříčany" "08:10:00" ["0001"] None []
                location "57016" "Nymburk" "08:20:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[0].CommercialTrafficType <- "Os"
        value.CzpttInformation.CzpttLocation.[1].CommercialTrafficType <- "RJ"
        value.CzpttInformation.CzpttLocation.[2].CommercialTrafficType <- "RJ"
        let feed = (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual(2, feed.trips.Length)
        CollectionAssert.AreEquivalent(
            [| "Os 01234"; "RJ 01234" |],
            feed.trips |> Array.choose (fun trip -> trip.shortName))
        Assert.IsTrue(
            feed.trips
            |> Array.forall (fun trip -> trip.headsign = Some "Nymburk"))
        Assert.AreEqual(feed.trips.[0].blockId, feed.trips.[1].blockId)
        Assert.AreEqual(1, feed.transfers.Value.Length)
        CollectionAssert.AreEquivalent(
            [| "1c1745"; "b91c1c" |],
            feed.routes |> Array.choose (fun route -> route.color))

    [<TestMethod>]
    member _.``Line change creates linked trips without changing train number``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112" ]
                location "57016" "Nymburk" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112" ]
            ] []
        let feed = (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual(2, feed.trips.Length)
        CollectionAssert.AreEquivalent(
            [| "S1"; "S12" |],
            feed.routes |> Array.choose (fun route -> route.shortName))
        Assert.IsTrue(
            feed.trips
            |> Array.forall (fun trip -> trip.shortName = Some "Vlak 01234"))
        Assert.IsTrue(
            feed.trips
            |> Array.forall (fun trip -> trip.headsign = Some "Nymburk"))
        Assert.AreEqual(feed.trips.[0].blockId, feed.trips.[1].blockId)
        Assert.AreEqual(1, feed.transfers.Value.Length)

    [<TestMethod>]
    member _.``Alternative transport segments use bus routes and timed transfers``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Poříčany" "08:10:00" ["0001"; "0030"]
                    (Some ("2", None))
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "1" ]
                location "93001" "NAD operational" "08:15:00" []
                    (Some ("99", None))
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "1" ]
                location "57016" "Kolín" "08:20:00" ["0001"]
                    (Some ("3", None))
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "0" ]
                location "54357" "Břeclav" "09:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] [
                "CZCentralPTTNote", "17|CZ57076||CZ54357||0|"
                "CZCentralPTTNote", "36|CZ57076||CZ54357||0|"
            ]
        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(3, result.feed.trips.Length)
        let nadRoute = result.feed.routes |> Array.find (fun route -> route.routeType = "714")
        Assert.AreEqual(Some "S1 (NAD)", nadRoute.shortName)
        let nadTrip = result.feed.trips |> Array.find (fun trip -> trip.routeId = nadRoute.id)
        Assert.AreEqual(Some "Vlak 01234", nadTrip.shortName)
        Assert.AreEqual(None, nadTrip.wheelchairAccessible)
        Assert.AreEqual(None, nadTrip.bikesAllowed)
        Assert.IsFalse(
            result.features
            |> Array.exists (fun feature ->
                feature.tripId = nadTrip.id && feature.noteId.IsSome))
        Assert.IsTrue(
            result.features
            |> Array.exists (fun feature ->
                feature.tripId = nadTrip.id && feature.kind = "on_request"))
        Assert.IsTrue(
            result.notes
            |> Array.filter (fun note -> note.kind = "czptt_central_note")
            |> Array.forall (fun note -> note.rawValue <> ""))
        let nadStopTimes =
            result.feed.stopTimes
            |> Array.filter (fun stopTime -> stopTime.tripId = nadTrip.id)
        Assert.AreEqual(2, nadStopTimes.Length)
        Assert.IsTrue(
            nadStopTimes
            |> Array.forall (fun stopTime ->
                stopTime.stopId.EndsWith(":platform:BUS")))
        Assert.IsFalse(
            nadStopTimes
            |> Array.exists (fun stopTime -> stopTime.stopId.Contains("93001")))
        let busStops =
            result.feed.stops
            |> Array.filter (fun stop -> stop.platformCode = Some "BUS")
        Assert.AreEqual(2, busStops.Length)
        Assert.IsTrue(
            busStops
            |> Array.forall (fun stop ->
                stop.description =
                    Some "Pro přesné informace k nástupišti náhradní dopravy sledujte informace dopravce."))
        Assert.AreEqual(2, result.feed.transfers.Value.Length)
        let transfers =
            result.feed.transfers.Value
            |> Array.filter (fun transfer -> transfer.transferType = 2)
        Assert.AreEqual(2, transfers.Length)
        Assert.IsTrue(
            transfers
            |> Array.forall (fun transfer ->
                transfer.fromTripId.IsNone
                && transfer.toTripId.IsNone
                && transfer.minTransferTime = Some 0))
        let intoNad =
            transfers
            |> Array.find (fun transfer ->
                transfer.toStopId = Some "czptt:stop:CZ:57050:platform:BUS")
        let outOfNad =
            transfers
            |> Array.find (fun transfer ->
                transfer.fromStopId = Some "czptt:stop:CZ:57016:platform:BUS")
        Assert.AreEqual(
            Some "czptt:stop:CZ:57050:platform:2",
            intoNad.fromStopId)
        Assert.AreEqual(
            Some "czptt:stop:CZ:57050:platform:BUS",
            intoNad.toStopId)
        Assert.AreEqual(
            Some "czptt:stop:CZ:57016:platform:BUS",
            outOfNad.fromStopId)
        Assert.AreEqual(
            Some "czptt:stop:CZ:57016:platform:3",
            outOfNad.toStopId)
        Assert.IsTrue(
            transfers
            |> Array.forall (fun transfer ->
                transfer.transferType = 2
                && transfer.minTransferTime = Some 0))
        Assert.AreEqual(
            3,
            Set.count (result.feed.trips |> Array.choose (fun trip -> trip.blockId) |> Set))

    [<TestMethod>]
    member _.``Line-less NAD keeps the train designation as its route label``() =
        let value =
            message [
                location "53001" "Kadaň-Prunéřov" "08:00:00" ["0001"] None
                    [ "CZAlternativeTransport", "1" ]
                location "53003" "Kadaň předm." "08:10:00" ["0001"] None
                    [ "CZAlternativeTransport", "1" ]
            ] []
            |> setCommercialType "84"
            |> setOperationalTrainNumber "363784"
        let feed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual("714", feed.routes.[0].routeType)
        Assert.AreEqual(Some "Os 363784 (NAD)", feed.routes.[0].shortName)

    [<TestMethod>]
    member _.``NAD runs without two passenger calls are omitted``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "93001" "First operational point" "08:05:00" [] None
                    [ "CZAlternativeTransport", "1" ]
                location "93002" "Second operational point" "08:10:00" [] None
                    [ "CZAlternativeTransport", "0" ]
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] []
        let feed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual(2, feed.trips.Length)
        Assert.IsFalse(feed.routes |> Array.exists (fun route -> route.routeType = "714"))
        Assert.IsFalse(
            feed.stopTimes
            |> Array.exists (fun stopTime ->
                stopTime.stopId.EndsWith(":platform:BUS")))
        Assert.AreEqual(1, feed.transfers.Value.Length)
        Assert.AreEqual(4, feed.transfers.Value.[0].transferType)

    [<TestMethod>]
    member _.``No blocks still separates rail replacement mode runs``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112"
                      "CZAlternativeTransport", "1" ]
                location "57016" "Kolín" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "0" ]
                location "54357" "Břeclav" "09:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
        let result =
            CzPttToGtfs.convertWithOptions catalog {
                operationalPointMode = CzPttToGtfs.Gtfs
                blockMode = CzPttToGtfs.NoBlocks
            } [ value ]
        Assert.AreEqual(3, result.feed.trips.Length)
        CollectionAssert.AreEquivalent(
            [| "100"; "714" |],
            result.feed.routes |> Array.map (fun route -> route.routeType) |> Array.distinct)
        Assert.IsTrue(
            result.feed.trips |> Array.forall (fun trip -> trip.blockId.IsNone))
        Assert.IsTrue(
            result.feed.routes
            |> Array.exists (fun route -> route.shortName = Some "S12 (NAD)"))
        CollectionAssert.AreEquivalent(
            [| 2; 2 |],
            result.feed.transfers.Value |> Array.map (fun transfer -> transfer.transferType))

    [<TestMethod>]
    member _.``Separate NAD trip matches a continuing train by passenger line``() =
        let train =
            message [
                location "57076" "Praha hl.n." "20:30:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "53001" "Kadaň-Prunéřov" "20:42:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "53002" "Klášterec n.O." "20:52:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
            |> setCore "000000006562"
            |> setOperationalTrainNumber "06562"
        let nad =
            message [
                location "53001" "Kadaň-Prunéřov" "20:47:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "1" ]
                location "53003" "Kadaň předm." "20:58:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "1" ]
            ] []
            |> setCore "000000016162"
            |> setOperationalTrainNumber "16162"
        let feed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ nad; train ]).feed
        let nadRoute = feed.routes |> Array.find (fun route -> route.routeType = "714")
        let nadTrip = feed.trips |> Array.find (fun trip -> trip.routeId = nadRoute.id)
        let trainTrip =
            feed.trips
            |> Array.find (fun trip -> trip.shortName = Some "Vlak 06562")
        Assert.AreEqual(1, feed.transfers.Value.Length)
        let transfer =
            feed.transfers.Value
            |> Array.find (fun transfer -> transfer.transferType = 2)
        Assert.AreEqual(None, transfer.fromTripId)
        Assert.AreEqual(None, transfer.toTripId)
        Assert.AreEqual(
            Some "czptt:stop:CZ:53001:unspecified", transfer.fromStopId)
        Assert.AreEqual(
            Some "czptt:stop:CZ:53001:platform:BUS", transfer.toStopId)
        Assert.AreEqual(Some 0, transfer.minTransferTime)
        Assert.IsTrue(
            feed.stopTimes
            |> Array.exists (fun stopTime ->
                stopTime.tripId = trainTrip.id
                && Some stopTime.stopId = transfer.fromStopId))
        Assert.IsTrue(
            feed.stopTimes
            |> Array.exists (fun stopTime ->
                stopTime.tripId = nadTrip.id
                && Some stopTime.stopId = transfer.toStopId))
        Assert.AreEqual(2, transfer.transferType)

    [<TestMethod>]
    member _.``Separate NAD trip matches by the 300000 train-number prefix``() =
        let train =
            message [
                location "57076" "Praha hl.n." "07:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "53001" "Kadaň-Prunéřov" "07:41:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
            |> setCore "000000006802"
            |> setOperationalTrainNumber "6802"
        let closerLineTrain =
            message [
                location "57076" "Praha hl.n." "07:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "53001" "Kadaň-Prunéřov" "07:44:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
            |> setCore "000000009999"
            |> setOperationalTrainNumber "9999"
        let nad =
            message [
                location "53001" "Kadaň-Prunéřov" "07:46:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "1" ]
                location "53003" "Kadaň předm." "07:58:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "1" ]
            ] []
            |> setCore "000000306802"
            |> setOperationalTrainNumber "306802"
        let feed =
            (CzPttToGtfs.convert
                catalog CzPttToGtfs.Gtfs [ train; closerLineTrain; nad ]).feed
        Assert.AreEqual(1, feed.transfers.Value.Length)
        let exactTransfer =
            feed.transfers.Value
            |> Array.find (fun transfer -> transfer.transferType = 2)
        Assert.AreEqual(None, exactTransfer.fromTripId)
        Assert.AreEqual(None, exactTransfer.toTripId)
        Assert.IsTrue(
            feed.routes
            |> Array.exists (fun route ->
                route.routeType = "714"
                && route.shortName = Some "S1 (NAD)"))
        Assert.AreEqual(
            Some "czptt:stop:CZ:53001:unspecified",
            exactTransfer.fromStopId)
        Assert.AreEqual(
            Some "czptt:stop:CZ:53001:platform:BUS",
            exactTransfer.toStopId)
        Assert.AreEqual(Some 0, exactTransfer.minTransferTime)

    [<TestMethod>]
    member _.``Disjoint calendar variants each connect to the same NAD trip``() =
        let train core bitmap =
            message [
                location "57076" "Praha hl.n." "07:00:00" ["0001"] None []
                location "53001" "Kadaň-Prunéřov" "07:41:00" ["0001"] None []
            ] []
            |> setCore core
            |> setOperationalTrainNumber "6802"
            |> setCalendarBitmap bitmap
        let nad =
            message [
                location "53001" "Kadaň-Prunéřov" "07:46:00" ["0001"] None
                    [ "CZAlternativeTransport", "1" ]
                location "53003" "Kadaň předm." "07:58:00" ["0001"] None
                    [ "CZAlternativeTransport", "1" ]
            ] []
            |> setCore "000000306802"
            |> setOperationalTrainNumber "306802"
            |> setCalendarBitmap "11"
        let feed =
            (CzPttToGtfs.convert
                catalog CzPttToGtfs.Gtfs
                [ train "000000006802" "10"
                  train "000000106802" "01"
                  nad ]).feed
        Assert.AreEqual(1, feed.transfers.Value.Length)
        Assert.AreEqual(
            1,
            feed.transfers.Value
            |> Array.filter (fun transfer -> transfer.transferType = 2)
            |> Array.length)

    [<TestMethod>]
    member _.``Separate NAD trip connects onward to a train by passenger line``() =
        let nad =
            message [
                location "53003" "Kadaň předm." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "1" ]
                location "53001" "Kadaň-Prunéřov" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101"
                      "CZAlternativeTransport", "1" ]
            ] []
            |> setCore "000000016163"
            |> setOperationalTrainNumber "16163"
        let train =
            message [
                location "53001" "Kadaň-Prunéřov" "08:15:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "53002" "Klášterec n.O." "08:22:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
            |> setCore "000000006563"
            |> setOperationalTrainNumber "06563"
        let feed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ nad; train ]).feed
        let nadTrip =
            feed.trips
            |> Array.find (fun trip -> trip.shortName = Some "Vlak 16163")
        let trainTrip =
            feed.trips
            |> Array.find (fun trip -> trip.shortName = Some "Vlak 06563")
        Assert.AreEqual(1, feed.transfers.Value.Length)
        let transfer =
            feed.transfers.Value
            |> Array.find (fun transfer -> transfer.transferType = 2)
        Assert.AreEqual(None, transfer.fromTripId)
        Assert.AreEqual(None, transfer.toTripId)
        Assert.AreEqual(
            Some "czptt:stop:CZ:53001:platform:BUS", transfer.fromStopId)
        Assert.AreEqual(
            Some "czptt:stop:CZ:53001:unspecified", transfer.toStopId)
        Assert.AreEqual(Some 0, transfer.minTransferTime)
        Assert.IsTrue(
            feed.stopTimes
            |> Array.exists (fun stopTime ->
                stopTime.tripId = nadTrip.id
                && Some stopTime.stopId = transfer.fromStopId))
        Assert.IsTrue(
            feed.stopTimes
            |> Array.exists (fun stopTime ->
                stopTime.tripId = trainTrip.id
                && Some stopTime.stopId = transfer.toStopId))

    [<TestMethod>]
    member _.``No blocks combines unique labels into one trip``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112" ]
                location "57016" "Nymburk" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112" ]
                location "54357" "Břeclav" "09:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "54358" "Lanžhot" "09:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
        let result =
            CzPttToGtfs.convertWithOptions catalog {
                operationalPointMode = CzPttToGtfs.Gtfs
                blockMode = CzPttToGtfs.NoBlocks
            } [ value ]
        Assert.AreEqual(1, result.feed.trips.Length)
        Assert.AreEqual(None, result.feed.trips.[0].blockId)
        Assert.AreEqual(0, result.feed.transfers.Value.Length)
        Assert.AreEqual(Some "S1/S12", result.feed.routes.[0].shortName)
        Assert.AreEqual(None, result.feed.routes.[0].longName)
        Assert.AreEqual(
            Some "S1/S12", result.feed.czRoutes.Value.[0].publicLineNumber)
        Assert.IsTrue(
            result.operationalCalls
            |> Array.forall (fun call ->
                call.generatedTripIds = [| result.feed.trips.[0].id |]))

    [<TestMethod>]
    member _.``No blocks combines lines fallbacks and operators``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Nymburk" "08:20:00" ["0001"] None []
                location "54357" "Břeclav" "09:00:00" ["0001"] None []
            ] [ "CZIPTS", "11|CZ57076||CZ54357||" ]
            |> setCommercialType "Os"
        value.CzpttInformation.CzpttLocation.[1].ResponsibleRu <- "80"
        value.CzpttInformation.CzpttLocation.[2].ResponsibleRu <- "80"
        value.CzpttInformation.CzpttLocation.[3].ResponsibleRu <- "54"
        let result =
            CzPttToGtfs.convertWithOptions catalog {
                operationalPointMode = CzPttToGtfs.Gtfs
                blockMode = CzPttToGtfs.NoBlocks
            } [ value ]
        Assert.AreEqual(Some "S1/Os 01234", result.feed.routes.[0].shortName)
        let routeAgency = result.feed.routes.[0].agencyId.Value
        let composite =
            result.feed.agencies
            |> Array.find (fun value -> value.id = Some routeAgency)
        Assert.AreEqual("České dráhy / DB", composite.name)
        Assert.IsTrue(result.feed.czTripStopZones.Value.Length > 0)
        Assert.IsTrue(
            result.feed.czTripStopZones.Value
            |> Array.forall (fun zone -> zone.tripId = result.feed.trips.[0].id))

    [<TestMethod>]
    member _.``Internal line changes move to the next passenger call``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "93001" "Praha hl.n. LC601" "08:01:00" [] None
                    [ "CZPassengerServiceNumber", "112" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112" ]
                location "57016" "Nymburk" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112" ]
            ] []
        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        let oldRoute = result.feed.routes |> Array.find (fun r -> r.shortName = Some "S1")
        let oldTrip = result.feed.trips |> Array.find (fun t -> t.routeId = oldRoute.id)
        let internalStopId = "czptt:stop:CZ:93001:unspecified"
        Assert.IsTrue(
            result.feed.stopTimes
            |> Array.exists (fun call ->
                call.tripId = oldTrip.id && call.stopId = internalStopId))
        Assert.IsTrue(
            result.boundaryAdjustments
            |> Array.exists (fun adjustment ->
                adjustment.sourceSequence = 2
                && adjustment.appliedSequence = Some 3
                && adjustment.reason = "deferred-to-next-passenger-call"))

    [<TestMethod>]
    member _.``Line and operator changes in one interstop span share a boundary``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "93001" "Border" "08:05:00" [] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "93002" "Line marker" "08:06:00" [] None
                    [ "CZPassengerServiceNumber", "112" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112" ]
                location "57016" "Nymburk" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "112" ]
            ] []
        for index in 1 .. 4 do
            value.CzpttInformation.CzpttLocation.[index].ResponsibleRu <- "80"
        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(2, result.feed.trips.Length)
        CollectionAssert.AreEquivalent(
            [| "S1"; "S12" |],
            result.feed.routes |> Array.choose (fun route -> route.shortName))
        Assert.IsTrue(
            result.boundaryAdjustments
            |> Array.exists (fun adjustment ->
                adjustment.sourceSequence = 3
                && adjustment.appliedSequence = Some 2
                && adjustment.reason = "coalesced-with-operator-boundary"))

    [<TestMethod>]
    member _.``No blocks selects the highest service route type``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Nymburk" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
        value.CzpttInformation.CzpttLocation.[0].CommercialTrafficType <- "Os"
        value.CzpttInformation.CzpttLocation.[1].CommercialTrafficType <- "R"
        value.CzpttInformation.CzpttLocation.[2].CommercialTrafficType <- "R"
        let result =
            CzPttToGtfs.convertWithOptions catalog {
                operationalPointMode = CzPttToGtfs.Gtfs
                blockMode = CzPttToGtfs.NoBlocks
            } [ value ]
        Assert.AreEqual("103", result.feed.routes.[0].routeType)
        Assert.AreEqual(Some "b45309", result.feed.routes.[0].color)

    [<TestMethod>]
    member _.``Unknown catalog lines use the train designation fallback``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "999" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "999" ]
                location "57016" "Nymburk" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "999" ]
            ] []
            |> setCommercialType "Os"
        let result =
            CzPttToGtfs.convertWithOptions catalog {
                operationalPointMode = CzPttToGtfs.Gtfs
                blockMode = CzPttToGtfs.NoBlocks
            } [ value ]
        Assert.AreEqual(Some "Os 01234", result.feed.routes.[0].shortName)
        Assert.AreEqual(None, result.feed.czRoutes.Value.[0].publicLineNumber)

    [<TestMethod>]
    member _.``Agency-only internal boundaries stay exact or are diagnosed``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "93001" "Border" "08:05:00" [] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Nymburk" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
        for index in 1 .. 3 do
            value.CzpttInformation.CzpttLocation.[index].ResponsibleRu <- "80"
        let gtfs = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        let sidecar = CzPttToGtfs.convert catalog CzPttToGtfs.Sidecar [ value ]
        let internalStopId = "czptt:stop:CZ:93001:unspecified"
        Assert.AreEqual(2, gtfs.feed.trips.Length)
        Assert.AreEqual(2, sidecar.feed.trips.Length)
        Assert.AreEqual(
            2,
            gtfs.feed.stopTimes
            |> Array.filter (fun call -> call.stopId = internalStopId)
            |> Array.length)
        Assert.IsFalse(
            sidecar.feed.stopTimes
            |> Array.exists (fun call -> call.stopId = internalStopId))
        Assert.IsTrue(
            sidecar.sidecarBoundaryApproximations
            |> Array.exists (fun diagnostic ->
                diagnostic.Contains("operator boundary moved")))

    [<TestMethod>]
    member _.``Location line applicability clears in both directions``() =
        let lineThenFallback =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Kolín" "08:20:00" ["0001"] None []
                location "54357" "Břeclav" "09:00:00" ["0001"] None []
            ] []
        let cleared =
            CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ lineThenFallback ]
        CollectionAssert.AreEqual(
            [| Some "101"; Some "101"; None; None |],
            cleared.operationalCalls |> Array.map (fun call -> call.activeLineCode))
        CollectionAssert.AreEquivalent(
            [| "S1"; "Vlak 01234" |],
            cleared.feed.routes |> Array.choose (fun route -> route.shortName))
        Assert.AreEqual(2, cleared.feed.trips.Length)
        Assert.AreEqual(1, cleared.feed.transfers.Value.Length)
        let clearedLineRoute =
            cleared.feed.routes
            |> Array.find (fun route -> route.shortName = Some "S1")
        let clearedFallbackRoute =
            cleared.feed.routes
            |> Array.find (fun route ->
                route.shortName = Some "Vlak 01234")
        let clearedLineTrip =
            cleared.feed.trips
            |> Array.find (fun trip -> trip.routeId = clearedLineRoute.id)
        let clearedFallbackTrip =
            cleared.feed.trips
            |> Array.find (fun trip -> trip.routeId = clearedFallbackRoute.id)
        let callsFor tripId =
            cleared.feed.stopTimes
            |> Array.filter (fun call -> call.tripId = tripId)
        Assert.AreEqual(
            "czptt:stop:CZ:57016:unspecified",
            (callsFor clearedLineTrip.id |> Array.last).stopId)
        Assert.AreEqual(
            "czptt:stop:CZ:57016:unspecified",
            (callsFor clearedFallbackTrip.id |> Array.head).stopId)

        let fallbackThenLine =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57050" "Poříčany" "08:10:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Kolín" "08:20:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
            ] []
        let activated =
            CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ fallbackThenLine ]
        CollectionAssert.AreEqual(
            [| None; Some "101"; Some "101" |],
            activated.operationalCalls |> Array.map (fun call -> call.activeLineCode))
        CollectionAssert.AreEquivalent(
            [| "Vlak 01234"; "S1" |],
            activated.feed.routes |> Array.choose (fun route -> route.shortName))
        let activatedFallbackRoute =
            activated.feed.routes
            |> Array.find (fun route ->
                route.shortName = Some "Vlak 01234")
        let activatedLineRoute =
            activated.feed.routes
            |> Array.find (fun route -> route.shortName = Some "S1")
        let activatedFallbackTrip =
            activated.feed.trips
            |> Array.find (fun trip -> trip.routeId = activatedFallbackRoute.id)
        let activatedLineTrip =
            activated.feed.trips
            |> Array.find (fun trip -> trip.routeId = activatedLineRoute.id)
        let activatedCalls tripId =
            activated.feed.stopTimes
            |> Array.filter (fun call -> call.tripId = tripId)
        Assert.AreEqual(
            "czptt:stop:CZ:57050:unspecified",
            (activatedCalls activatedFallbackTrip.id |> Array.last).stopId)
        Assert.AreEqual(
            "czptt:stop:CZ:57050:unspecified",
            (activatedCalls activatedLineTrip.id |> Array.head).stopId)

        let rootOnly =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] [ "CZPassengerServiceNumber", "101" ]
        let rootFeed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ rootOnly ]).feed
        Assert.AreEqual(Some "S1", rootFeed.routes.[0].shortName)

    [<TestMethod>]
    member _.``Line expiry before the terminus is deferred to the terminus``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "53039" "Perštejn" "09:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "93001" "hr. VÚSC km 123" "09:05:00" [] None []
                location "53040" "Kadaň" "09:20:00" ["0001"] None []
            ] []
        for mode in [| CzPttToGtfs.Gtfs; CzPttToGtfs.Sidecar |] do
            let result = CzPttToGtfs.convert catalog mode [ value ]
            Assert.AreEqual(1, result.feed.trips.Length, string mode)
            Assert.AreEqual(Some "S1", result.feed.routes.[0].shortName, string mode)
            Assert.IsTrue(
                result.boundaryAdjustments
                |> Array.exists (fun adjustment ->
                    adjustment.sourceSequence = 3
                    && adjustment.appliedSequence = Some 4
                    && adjustment.reason = "deferred-to-next-passenger-call"))
            if mode = CzPttToGtfs.Gtfs then
                Assert.IsTrue(
                    result.feed.stopTimes
                    |> Array.exists (fun call ->
                        call.stopId = "czptt:stop:CZ:93001:unspecified"))
            else
                Assert.IsFalse(
                    result.feed.stopTimes
                    |> Array.exists (fun call ->
                        call.stopId = "czptt:stop:CZ:93001:unspecified"))

    [<TestMethod>]
    member _.``Provided point names apply to stops and headsigns``() =
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-name20-{Guid.NewGuid():N}")
        let name20 = Path.Combine(root, "SR70_Nazev20.csv")
        Directory.CreateDirectory(root) |> ignore
        try
            File.WriteAllLines(name20, [|
                "533398,\"Duchcov z\",50.61737,13.755146"
                "541292,\"Háj u Duchcova nz \",50.636308,13.731383"
            |])
            let names = CzPttToGtfs.loadSr70Name20 name20
            Assert.AreEqual(Some "Duchcov", Map.tryFind ("CZ", "53339") names)
            Assert.AreEqual(
                Some "Háj u Duchcova",
                Map.tryFind ("CZ", "54129") names)

            let value =
                message [
                    location "53339" "Source Duchcov" "08:00:00" ["0001"] None
                        [ "CZPassengerServiceNumber", "101" ]
                    location "57050" "Poříčany" "08:10:00" ["0001"] None
                        [ "CZPassengerServiceNumber", "101" ]
                    location "54129" "Source Háj" "08:20:00" ["0001"] None []
                ] []
                |> setCommercialType "Os"
            let feed =
                (CzPttToGtfs.convertWithPointNames
                    catalog CzPttToGtfs.Gtfs names [ value ]).feed
            CollectionAssert.AreEquivalent(
                [| "S1" |],
                feed.routes |> Array.choose (fun route -> route.shortName))
            Assert.IsTrue(
                feed.trips
                |> Array.forall (fun trip -> trip.headsign = Some "Háj u Duchcova"))
            Assert.IsTrue(
                feed.stops
                |> Array.filter (fun stop ->
                    stop.id.StartsWith("czptt:stop:CZ:54129"))
                |> Array.forall (fun stop -> stop.name = "Háj u Duchcova"))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Headsign remains the terminus when an intermediate point has the same name``() =
        let value =
            message [
                location "10001" "Origin" "08:00:00" ["0001"] None []
                location "10002" "Shared station" "08:10:00" ["0001"] None []
                location "10003" "Shared station" "08:20:00" ["0001"] None []
            ] []
        let feed = (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual(Some "Shared station", feed.trips.[0].headsign)
        let stopNames =
            feed.stopTimes
            |> Array.map (fun call ->
                feed.stops |> Array.find (fun stop -> stop.id = call.stopId) |> fun stop -> stop.name)
        Assert.AreEqual("Shared station", stopNames.[1])
        Assert.AreEqual("Shared station", stopNames.[2])

    [<TestMethod>]
    member _.``Ambiguous Nazev20 prefixes fall back to source names``() =
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-name20-amb-{Guid.NewGuid():N}")
        let name20 = Path.Combine(root, "SR70_Nazev20.csv")
        Directory.CreateDirectory(root) |> ignore
        try
            File.WriteAllLines(name20, [|
                "533398,\"First\",50.0,14.0"
                "533399,\"Second\",50.0,14.0"
            |])
            let names = CzPttToGtfs.loadSr70Name20 name20
            Assert.AreEqual(None, Map.tryFind ("CZ", "53339") names)
            let value =
                message [
                    location "53339" "Source origin" "08:00:00" ["0001"] None []
                    location "54129" "Source destination" "08:20:00" ["0001"] None []
                ] []
            let feed =
                (CzPttToGtfs.convertWithPointNames
                    catalog CzPttToGtfs.Gtfs names [ value ]).feed
            Assert.AreEqual(
                Some "Vlak 01234",
                feed.routes.[0].shortName)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``SR70 names override Czech source names throughout bundle output``() =
        let value =
            message [
                location "57076" "Source Praha" "08:00:00" ["0001"] None []
                location "53339" "Ambiguous source" "08:10:00" ["0001"] None []
                location "54129" "Duplicate source" "08:20:00" ["0001"] None []
                location "50051" "Dash source" "08:30:00" ["0001"] None []
                location "99999" "Missing source" "08:40:00" ["0001"] None []
                location "76534" "Foreign source" "08:50:00" ["0001"] None []
                location "76534" "Kraslice-P.vlekem z" "09:00:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[5].Location.CountryCodeIso <- "DE"
        let sourceFeed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.IsTrue(
            sourceFeed.stops
            |> Array.filter (fun stop -> stop.id.StartsWith("czptt:stop:CZ:76534"))
            |> Array.forall (fun stop -> stop.name = "Kraslice-P.vlekem z"))

        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-sr70-names-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        Directory.CreateDirectory(root) |> ignore
        try
            writeMessage input value
            File.WriteAllLines(sr70, [|
                "570768,\"Praha, hlavní\",50.083,14.435"
                "533398,First official,50.6,13.7"
                "533399,Second official,50.6,13.7"
                "541292,Shared official,50.63,13.73"
                "541299,Shared official,50.63,13.73"
                "500512,-,50.5,14.0"
                "765347,Kraslice-Pod vlekem,50.340188,12.49709"
            |])
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None None None

            let namesByIdentity =
                result.operationalCalls
                |> Seq.map (fun call ->
                    (call.countryCode, call.primaryCode), call.name)
                |> Map
            Assert.AreEqual("Praha, hlavní", namesByIdentity.[("CZ", "57076")])
            Assert.AreEqual("Ambiguous source", namesByIdentity.[("CZ", "53339")])
            Assert.AreEqual("Shared official", namesByIdentity.[("CZ", "54129")])
            Assert.AreEqual("Dash source", namesByIdentity.[("CZ", "50051")])
            Assert.AreEqual("Missing source", namesByIdentity.[("CZ", "99999")])
            Assert.AreEqual("Foreign source", namesByIdentity.[("DE", "76534")])
            Assert.AreEqual("Kraslice-Pod vlekem", namesByIdentity.[("CZ", "76534")])
            Assert.IsTrue(
                result.feed.stops
                |> Array.filter (fun stop -> stop.id.StartsWith("czptt:stop:CZ:76534"))
                |> Array.forall (fun stop -> stop.name = "Kraslice-Pod vlekem"))
            Assert.IsTrue(
                result.feed.trips
                |> Array.forall (fun trip ->
                    trip.headsign = Some "Kraslice-Pod vlekem"))

            use stream =
                File.OpenRead(Path.Combine(output, "operational_points.parquet"))
            let points =
                ParquetSerializer.DeserializeUntypedAsync(stream)
                    .GetAwaiter().GetResult()
            let kraslice =
                points.Data
                |> Seq.find (fun row ->
                    string row.["source_location_id"] = "CZ:76534")
            Assert.AreEqual(
                "Kraslice-Pod vlekem",
                string kraslice.["source_name"])
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Missing Nazev20 gets a compact route label without changing stop text``() =
        let firstNames =
            Map.ofList [ ("CZ", "53000"), "Karlovy Vary" ]
        let secondNames =
            Map.ofList [
                ("CZ", "53000"), "Completely different provenance name"
                ("DE", "12345"), "Another provenance name"
            ]
        let value =
            message [
                location "53000" "Karlovy Vary source" "08:00:00" ["0001"] None []
                location "12345" "Johanngeorgenstadt" "08:20:00" ["0001"] None []
            ] []
            |> setCommercialType "Os"
        value.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "DE"

        let first =
            (CzPttToGtfs.convertWithPointNames
                catalog CzPttToGtfs.Gtfs firstNames [ value ]).feed
        let second =
            (CzPttToGtfs.convertWithPointNames
                catalog CzPttToGtfs.Gtfs secondNames [ value ]).feed

        Assert.AreEqual(
            Some "Os 01234",
            first.routes.[0].shortName)
        Assert.AreEqual(first.routes.[0].shortName, second.routes.[0].shortName)
        Assert.AreEqual(Some "Johanngeorgenstadt", first.trips.[0].headsign)
        let foreignStops =
            first.stops
            |> Array.filter (fun stop ->
                stop.id.StartsWith("czptt:stop:DE:12345"))
        Assert.AreEqual(2, foreignStops.Length)
        Assert.IsTrue(
            foreignStops
            |> Array.forall (fun stop -> stop.name = "Johanngeorgenstadt"))

    [<TestMethod>]
    member _.``Fallback route naming does not alter stop municipalities``() =
        let cityDistricts =
            message [
                location "10001" "Praha-Holešovice" "08:00:00" ["0001"] None []
                location "10002" "Ostrava-Svinov" "08:20:00" ["0001"] None []
            ] []
        let compounds =
            message [
                location "10003" "Frýdek-Místek" "09:00:00" ["0001"] None []
                location "10004" "Rájec-Jestřebí" "09:20:00" ["0001"] None []
            ] []
            |> setCore "000000005678"

        let districtFeed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ cityDistricts ]).feed
        let compoundFeed =
            (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ compounds ]).feed

        Assert.AreEqual(
            Some "Vlak 01234",
            districtFeed.routes.[0].shortName)
        Assert.AreEqual(
            Some "Vlak 01234",
            compoundFeed.routes.[0].shortName)
        CollectionAssert.Contains(
            districtFeed.stops |> Array.map (fun stop -> stop.name),
            "Praha-Holešovice")
        CollectionAssert.Contains(
            districtFeed.stops |> Array.map (fun stop -> stop.name),
            "Ostrava-Svinov")

    [<TestMethod>]
    member _.``Backward chronology rejects the complete PA``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:10:00" ["0001"] None []
                location "57016" "Kolín" "08:05:00" ["0001"] None []
            ] []
        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(0, result.feed.trips.Length)
        Assert.AreEqual(1, result.rejectedJourneys.Length)

    [<TestMethod>]
    member _.``Passenger activity variants and times over 24 hours are preserved``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"; "0028"; "0030"] None []
                location "57050" "Poříčany" "08:10:00" ["0001"; "0030"] None []
                location "57016" "Kolín" "08:20:00" ["0001"; "0029"; "0030"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[2].TimingAtLocation.Timing
        |> Array.iter (fun timing -> timing.Offset <- "1")
        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        let feed = result.feed
        Assert.AreEqual(Some GtfsModel.NoService, feed.stopTimes.[0].dropoffType)
        Assert.AreEqual(Some GtfsModel.CoordinationWithDriver, feed.stopTimes.[0].pickupType)
        Assert.AreEqual(
            Some GtfsModel.CoordinationWithDriver,
            feed.stopTimes.[1].pickupType)
        Assert.AreEqual(
            Some GtfsModel.CoordinationWithDriver,
            feed.stopTimes.[1].dropoffType)
        Assert.AreEqual(Some GtfsModel.NoService, feed.stopTimes.[2].pickupType)
        Assert.AreEqual(Some GtfsModel.CoordinationWithDriver, feed.stopTimes.[2].dropoffType)
        Assert.AreEqual(
            32.0 * 3600.0 + 20.0 * 60.0,
            feed.stopTimes.[2].arrivalTime.Value.ToDuration().TotalSeconds)
        Assert.AreEqual(3, result.features |> Array.filter (fun feature -> feature.kind = "on_request") |> Array.length)
        let request =
            result.features
            |> Array.find (fun feature -> feature.kind = "on_request" && feature.callSequence = Some 2)
        Assert.AreEqual("0030", request.sourceCode)
        Assert.AreEqual(Some 2, request.callSequence)

    [<TestMethod>]
    member _.``Central bike notes retain distinctions and project only complete claims``() =
        let expectedKinds = [|
            "22", [| "bicycle_transport"; "bicycle_carry_on" |]
            "26", [| "bicycle_transport"; "bicycle_storage"; "bicycle_reservation_available" |]
            "27", [| "bicycle_transport"; "bicycle_storage"; "bicycle_reservation_required" |]
            "28", [| "bicycle_transport"; "bicycle_carry_on"; "bicycle_reservation_available" |]
            "29", [| "bicycle_transport"; "bicycle_carry_on"; "bicycle_reservation_required" |]
        |]
        for code, kinds in expectedKinds do
            let value =
                message [
                    location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                    location "57016" "Kolín" "08:20:00" ["0001"] None []
                ] [ "CZCentralPTTNote", $"{code}|CZ57076||CZ57016||0|" ]
            let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
            Assert.AreEqual(Some GtfsModel.OneOrMore, result.feed.trips.[0].bikesAllowed, code)
            CollectionAssert.AreEquivalent(
                kinds,
                result.features |> Array.map (fun feature -> feature.kind),
                code)
            Assert.IsTrue(result.features |> Array.forall (fun feature -> feature.callSequence.IsNone))

        let prohibited =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] [ "CZCentralPTTNote", "36|CZ57076||CZ57016||0|" ]
            |> fun value -> CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(Some GtfsModel.NoBicycles, prohibited.feed.trips.[0].bikesAllowed)
        CollectionAssert.AreEqual(
            [| "bicycle_transport_prohibited" |],
            prohibited.features |> Array.map (fun feature -> feature.kind))

    [<TestMethod>]
    member _.``Partial repeated-location and calendar bike notes remain unknown``() =
        let repeated =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
                location "57076" "Praha hl.n." "08:40:00" ["0001"] None []
                location "57017" "Kutná Hora" "09:00:00" ["0001"] None []
            ] [ "CZCentralPTTNote", "36|CZ57076|1|CZ57017||0|" ]
            |> fun value -> CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(None, repeated.feed.trips.[0].bikesAllowed)
        CollectionAssert.AreEqual(
            [| Some 3; Some 4 |],
            repeated.features |> Array.map (fun feature -> feature.callSequence))

        let calendarLimited =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] [
                "CZCentralPTTNote", "22|CZ57076||CZ57016||0|1"
                "CZCalendarPTTNote", "1|20251214|20251215|10|"
            ]
            |> setCalendarBitmap "11"
            |> fun value -> CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(None, calendarLimited.feed.trips.[0].bikesAllowed)
        Assert.IsTrue(calendarLimited.features |> Array.exists (fun feature -> feature.kind = "bicycle_transport"))

    [<TestMethod>]
    member _.``Wheelchair notes conflicts and malformed notes are conservative``() =
        for code in [| "17"; "34" |] do
            let result =
                message [
                    location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                    location "57016" "Kolín" "08:20:00" ["0001"] None []
                ] [ "CZCentralPTTNote", $"{code}|CZ57076||CZ57016||0|" ]
                |> fun value -> CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
            Assert.AreEqual(Some "1", result.feed.trips.[0].wheelchairAccessible, code)

        let conflict =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] [
                "CZCentralPTTNote", "22|CZ57076||CZ57016||0|"
                "CZCentralPTTNote", "36|CZ57076||CZ57016||0|"
            ]
            |> fun value -> CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(None, conflict.feed.trips.[0].bikesAllowed)
        Assert.IsTrue(conflict.idsDiagnostics |> Array.exists (fun value -> value.Contains("conflicting")))

        let malformed =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] [ "CZCentralPTTNote", "36|missing" ]
            |> fun value -> CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(None, malformed.feed.trips.[0].bikesAllowed)
        Assert.AreEqual(1, malformed.notes.Length)
        Assert.IsFalse(malformed.notes.[0].resolved)
        Assert.AreEqual("36|missing", malformed.notes.[0].rawValue)
        Assert.IsTrue(malformed.idsDiagnostics |> Array.exists (fun value -> value.Contains("unresolved CZCentralPTTNote")))

    [<TestMethod>]
    member _.``Negative source-day offsets shift only GTFS times``() =
        let value =
            message [
                location "57076" "Praha hl.n." "23:58:00" ["0001"] None []
                location "57016" "Kolín" "00:10:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[0].TimingAtLocation.Timing
        |> Array.iter (fun timing -> timing.Offset <- "-1")

        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(
            23.0 * 3600.0 + 58.0 * 60.0,
            result.feed.stopTimes.[0].arrivalTime.Value.ToDuration().TotalSeconds)
        Assert.AreEqual(
            24.0 * 3600.0 + 10.0 * 60.0,
            result.feed.stopTimes.[1].arrivalTime.Value.ToDuration().TotalSeconds)
        Assert.AreEqual(Some -120, result.operationalCalls.[0].arrivalSeconds)
        Assert.AreEqual(Some 600, result.operationalCalls.[1].arrivalSeconds)
        Assert.AreEqual(
            Some "https://portal.cisjr.cz/",
            result.feed.agencies.[0].url)
        let catalogWithSchemelessUrl = {
            catalog with
                companies =
                    [| { code = "54"; name = "České dráhy"; url = Some "www.cd.cz" } |]
        }
        let withCatalogUrl =
            CzPttToGtfs.convert
                catalogWithSchemelessUrl CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(
            Some "https://www.cd.cz",
            withCatalogUrl.feed.agencies.[0].url)

    [<TestMethod>]
    member _.``Untimed GTFS calls are explicitly approximate``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57050" "Poříčany" "08:10:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[1].TimingAtLocation <- null

        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(None, result.feed.stopTimes.[1].arrivalTime)
        Assert.AreEqual(None, result.feed.stopTimes.[1].departureTime)
        Assert.AreEqual(
            Some GtfsModel.Approximate,
            result.feed.stopTimes.[1].timepoint)
        Assert.AreEqual(None, result.operationalCalls.[1].arrivalSeconds)
        Assert.AreEqual(None, result.operationalCalls.[1].departureSeconds)

    [<TestMethod>]
    member _.``PA without activity 0001 is omitted completely``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0040"] None []
                location "57016" "Kolín" "08:20:00" [] None []
            ] []
        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(0, result.feed.trips.Length)
        Assert.AreEqual(0, result.operationalCalls.Length)
        Assert.AreEqual("no activity 0001 passenger call", result.rejectedJourneys.[0].reason)

    [<TestMethod>]
    member _.``PA with one selected GTFS call is rejected completely``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
            ] []
        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(0, result.feed.routes.Length)
        Assert.AreEqual(0, result.feed.trips.Length)
        Assert.AreEqual(0, result.feed.stopTimes.Length)
        Assert.AreEqual(0, result.feed.calendarExceptions.Value.Length)
        Assert.AreEqual(0, result.feed.transfers.Value.Length)
        Assert.AreEqual(0, result.feed.czTrips.Value.Length)
        Assert.AreEqual(0, result.operationalCalls.Length)
        Assert.AreEqual(0, result.acceptedPaIds.Length)
        Assert.AreEqual(1, result.rejectedJourneys.Length)
        Assert.AreEqual(
            "fewer than two selected GTFS calls",
            result.rejectedJourneys.[0].reason)

    [<TestMethod>]
    member _.``CZInconsistentTime root parameter rejects the complete PA``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] [ "CZInconsistentTime", "1" ]
        let result = CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]
        Assert.AreEqual(0, result.feed.trips.Length)
        Assert.AreEqual("CZInconsistentTime=1", result.rejectedJourneys.[0].reason)

    [<TestMethod>]
    member _.``Explicit IDS fare band creates trip-stop zones``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None
                    [ "CZPassengerServiceNumber", "101" ]
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] [ "CZIPTS", "11|CZ57076||CZ57016||" ]
        let feed = (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual(2, feed.czTripStopZones.Value.Length)
        Assert.IsTrue(
            feed.czTripStopZones.Value
            |> Array.forall (fun zone ->
                zone.zoneCode = "P" && zone.idsSystemId = "PID"))
        Assert.IsTrue(
            feed.stops
            |> Array.filter (fun stop -> stop.locationType = Some GtfsModel.Stop)
            |> Array.forall (fun stop -> stop.zoneId.IsSome))

    [<TestMethod>]
    member _.``Overlapping IDS bands remain lossless and do not set stops zone_id``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] [
                "CZIPTS", "11|CZ57076||CZ57016||"
                "CZIPTS", "12|CZ57076||CZ57016||"
            ]
        let feed = (CzPttToGtfs.convert catalog CzPttToGtfs.Gtfs [ value ]).feed
        Assert.AreEqual(4, feed.czTripStopZones.Value.Length)
        Assert.IsTrue(feed.stops |> Array.forall (fun stop -> stop.zoneId.IsNone))

    [<TestMethod>]
    member _.``Memory and spill-backed CZPTT sidecars are byte-identical``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] []
        let root = Path.Combine(Path.GetTempPath(), $"jrutil-czptt-identity-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let sr70 = Path.Combine(root, "SR70.csv")
        let memoryOutput = Path.Combine(root, "memory")
        let spillOutput = Path.Combine(root, "spill")
        Directory.CreateDirectory(root) |> ignore
        try
            writeMessage input value
            File.WriteAllLines(sr70, [|
                "57076,Praha hl.n.,50.083,14.435"
                "57016,Kolín,50.026,15.214"
            |])
            let options: CzPttToGtfs.ConversionOptions = {
                operationalPointMode = CzPttToGtfs.Gtfs
                blockMode = CzPttToGtfs.Blocks
            }
            CzPttBundle.writeSidecarsWithStorageAndProgressAndOptions
                CzPttBundle.MemoryBacked catalog options input memoryOutput
                (Some sr70) None None None (fun _ _ -> ())
            |> ignore
            CzPttBundle.writeSidecarsWithStorageAndProgressAndOptions
                CzPttBundle.SpillBacked catalog options input spillOutput
                (Some sr70) None None None (fun _ _ -> ())
            |> ignore
            let memoryFiles =
                Directory.EnumerateFiles(memoryOutput)
                |> Seq.map Path.GetFileName
                |> Seq.sort
                |> Seq.toArray
            let spillFiles =
                Directory.EnumerateFiles(spillOutput)
                |> Seq.map Path.GetFileName
                |> Seq.sort
                |> Seq.toArray
            CollectionAssert.AreEqual(memoryFiles, spillFiles)
            for name in memoryFiles do
                CollectionAssert.AreEqual(
                    File.ReadAllBytes(Path.Combine(memoryOutput, name)),
                    File.ReadAllBytes(Path.Combine(spillOutput, name)),
                    name)
            Assert.AreEqual(0, Directory.EnumerateFiles(root, "*.tmp") |> Seq.length)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``CZPTT bundle writer emits all operational Parquet sidecars``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"; "0030"] None []
            ] [
                "CZCentralPTTNote", "22|CZ57076||CZ57016||0|"
                "CZNonCentralPTTNote", "CZ57076||CZ57016||Tarifní text|2|3|1|0|"
            ]
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            File.WriteAllLines(sr70, [|
                "57076,Praha hl.n.,50.083,14.435"
                "57016,Kolín,50.026,15.214"
            |])
            let phases = ResizeArray<string>()
            let result =
                CzPttBundle.writeSidecarsWithProgress
                    catalog CzPttToGtfs.Gtfs input output (Some sr70) None
                    None None
                    (fun name state -> phases.Add($"{name}:{state}"))
            Assert.AreEqual(1, result.acceptedPaIds.Length)
            let praha =
                result.feed.stops
                |> Array.filter (fun stop -> stop.name = "Praha hl.n.")
            Assert.AreEqual(2, praha.Length)
            Assert.IsTrue(
                praha
                |> Array.forall (fun stop ->
                    stop.lat = Some 50.083M && stop.lon = Some 14.435M))
            Assert.AreEqual(2, result.coordinateDiagnostics.resolvedPointCount)
            Assert.AreEqual(4, result.coordinateDiagnostics.resolvedStopCount)
            Assert.AreEqual(0, result.coordinateDiagnostics.unresolvedPointIds.Length)
            Assert.AreEqual("parse-input:started", phases.[0])
            Assert.IsTrue(phases.Contains("index-stop-times:completed"))
            Assert.AreEqual(
                "write-ids-trip-projection:completed",
                phases.[phases.Count - 1])
            for name in [|
                "operational_points.parquet"
                "operational_calls.parquet"
                "source_call_metadata.parquet"
                "source_note_metadata.parquet"
                "source_feature_metadata.parquet"
                "source_ids_coverage_metadata.parquet"
                "source_ids_coverage_trip_metadata.parquet"
            |] do
                let path = Path.Combine(output, name)
                Assert.IsTrue(File.Exists(path), name)
                CollectionAssert.AreEqual(
                    [| byte 'P'; byte 'A'; byte 'R'; byte '1' |],
                    File.ReadAllBytes(path).[0..3])
            let expectedSchemas = [|
                "operational_points.parquet", [|
                    "source_location_id"; "country_code"; "primary_code"; "source_name"
                    "latitude"; "longitude"; "coordinate_source"
                    "coordinate_source_object_id"; "coordinate_match_method" |]
                "operational_calls.parquet", [|
                    "source_pa_id"; "source_sequence"; "source_location_id"
                    "passenger_call"; "arrival_seconds"; "departure_seconds"
                    "subsidiary_code"; "subsidiary_name"; "active_line_code" |]
                "source_call_metadata.parquet", [|
                    "gtfs_trip_id"; "stop_sequence"; "source_pa_id"; "source_sequence" |]
                "source_note_metadata.parquet", [|
                    "source_note_id"; "source_pa_id"; "note_kind"; "source_code"
                    "gtfs_trip_id"; "label"; "raw_value"; "valid_from"; "valid_to"; "resolved" |]
                "source_feature_metadata.parquet", [|
                    "source_feature_id"; "gtfs_trip_id"; "call_sequence"; "source_code"
                    "feature_kind"; "note_id"; "source_object_id" |]
                "source_ids_coverage_metadata.parquet", [|
                    "source_coverage_id"; "source_pa_id"; "source_sequence"
                    "record_type"; "source_code"; "ids_system_id"; "coverage_role"
                    "from_location_code"; "from_occurrence"; "to_location_code"
                    "to_occurrence"; "calendar_id"; "source_value" |]
                "source_ids_coverage_trip_metadata.parquet", [|
                    "source_coverage_id"; "gtfs_trip_id" |]
            |]
            for fileName, expected in expectedSchemas do
                use stream = File.OpenRead(Path.Combine(output, fileName))
                let parquet =
                    ParquetSerializer.DeserializeUntypedAsync(stream)
                        .GetAwaiter().GetResult()
                CollectionAssert.AreEqual(
                    expected,
                    parquet.Schema.DataFields |> Array.map (fun field -> field.Name))
                Assert.AreEqual(
                    "1",
                    string parquet.CustomMetadata.["obehy.schema_version"])
                Assert.AreEqual(
                    "czptt-v1",
                    string parquet.CustomMetadata.["obehy.bundle_format"])
            for oldName in [|
                "source_trip_metadata.parquet"
                "source_operational_point_metadata.parquet"
                "source_operational_call_metadata.parquet"
            |] do
                Assert.IsFalse(File.Exists(Path.Combine(output, oldName)), oldName)
            CzPttBundle.writeManifest output
            use manifest =
                JsonDocument.Parse(
                    File.ReadAllBytes(Path.Combine(output, "manifest.json")))
            let manifestFiles =
                manifest.RootElement.GetProperty("files").EnumerateArray()
                |> Seq.toArray
            Assert.AreEqual(7, manifestFiles.Length)
            Assert.IsTrue(
                manifestFiles
                |> Array.forall (fun item ->
                    item.GetProperty("rows").ValueKind = JsonValueKind.Number))

            result.feed
            |> Gtfs.deduplicateCalendar
            |> Gtfs.fillStandardRequiredFields
            |> Gtfs.gtfsFeedToFolder () (Path.Combine(output, "gtfs-intermediate"))
            let extensions = Path.Combine(output, "extensions")
            Directory.CreateDirectory(extensions) |> ignore
            for fileName in [| "cz_routes.txt"; "cz_trips.txt"; "cz_trip_stop_zones.txt" |] do
                let source = Path.Combine(output, "gtfs-intermediate", fileName)
                if File.Exists(source) then File.Move(source, Path.Combine(extensions, fileName))
            CzPttBundle.writeManifest output
            let production = Path.Combine(root, "production")
            JrUtil.Serving.PackageWriter.finalizeLegacyStaging Map.empty None (fun _ _ -> ()) output production
            JrUtil.Serving.Validation.validatePackage production |> ignore
            let rows relation columns =
                JrUtil.Serving.PackageReader.readTextRows
                    (Path.Combine(production, "serving", relation + ".parquet")) columns
                |> Seq.toArray
            Assert.AreEqual(2, (rows "operational_location" [|"source_location_id"|]).Length)
            Assert.AreEqual(1, (rows "operational_journey" [|"source_journey_id"|]).Length)
            Assert.AreEqual(2, (rows "operational_call" [|"source_journey_id"; "sequence"|]).Length)
            Assert.AreEqual(2, (rows "service_note" [|"note_id"|]).Length)
            Assert.AreEqual(2, (rows "service_note_assignment" [|"assignment_id"|]).Length)
            Assert.AreEqual(3, (rows "service_feature_assignment" [|"feature_id"|]).Length)
            let namespaces = rows "source_trip_map" [|"trip_namespace"|] |> Array.map (fun row -> row.[0])
            CollectionAssert.Contains(namespaces, "czptt_pa_id")
            CollectionAssert.Contains(namespaces, "czptt_tr_id")
            Assert.IsTrue((rows "source_call_map" [|"call_namespace"; "source_sequence"|]) |> Array.exists (fun row -> row.[0] = "czptt_pa_sequence"))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``SR70 coordinates never leak to a foreign point with the same code``() =
        let value =
            message [
                location "57076" "Praha hl.n." "08:00:00" ["0001"] None []
                location "57076" "Foreign" "08:20:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "DE"
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-country-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            File.WriteAllLines(sr70, [| "57076,Praha hl.n.,50.083,14.435" |])
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output (Some sr70) None
                    None None
            let czech =
                result.feed.stops
                |> Array.filter (fun stop -> stop.id.StartsWith("czptt:stop:CZ:"))
            let german =
                result.feed.stops
                |> Array.filter (fun stop -> stop.id.StartsWith("czptt:stop:DE:"))
            Assert.IsTrue(
                czech
                |> Array.forall (fun stop ->
                    stop.lat = Some 50.083M && stop.lon = Some 14.435M))
            Assert.IsTrue(
                german
                |> Array.forall (fun stop ->
                    stop.lat.IsSome
                    && stop.lon.IsSome
                    && stop.lat <> Some 50.083M))
            Assert.AreEqual(0, result.coordinateDiagnostics.unresolvedByCountry.Length)
            Assert.AreEqual(1, result.coordinateDiagnostics.estimatedResolutions.Length)
            Assert.AreEqual(
                "DE:57076",
                result.coordinateDiagnostics.estimatedResolutions.[0].sourceLocationId)
            Assert.AreEqual(
                "route_end_north",
                result.coordinateDiagnostics.estimatedResolutions.[0].coordinateMatchMethod)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``OSM nodes resolve PLC name and reviewed alias matches``() =
        let value =
            message [
                location "11111" "Node station" "08:00:00" ["0001"] None []
                location "22222" "Way station" "08:10:00" ["0001"] None []
                location "33333" "Relation station" "08:20:00" ["0001"] None []
            ] []
        for point in value.CzpttInformation.CzpttLocation do
            point.Location.CountryCodeIso <- "AT"
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-osm-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let pbf = Path.Combine(root, "fixture.osm.pbf")
        let aliases = Path.Combine(root, "aliases.json")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            let objects: OsmGeo array = [|
                osmNode 1L 48.20 16.30 [
                    "railway", "station"
                    "name", "Node station"
                    "ref:EU:PLC", "AT11111"
                ]
                osmNode 2L 48.30 16.40 [
                    "railway", "station"
                    "name", "Way station"
                ]
                osmNode 3L 48.40 16.50 [
                    "railway", "station"
                    "name", "Relation station"
                    "addr:country", "AT"
                ]
            |]
            writeOsmPbf pbf objects
            File.WriteAllText(
                aliases,
                """{"AT:33333":"osm:node:3"}""")

            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output None None
                    (Some pbf) (Some aliases)
            Assert.AreEqual(3, result.coordinateDiagnostics.osmGapFills.Length)
            CollectionAssert.AreEquivalent(
                [| "AT:11111"; "AT:22222"; "AT:33333" |],
                result.coordinateDiagnostics.osmGapFills
                |> Array.map (fun resolution -> resolution.sourceLocationId))
            CollectionAssert.AreEquivalent(
                [| "ref_eu_plc"; "normalized_exact_name"; "reviewed_alias" |],
                result.coordinateDiagnostics.osmGapFills
                |> Array.map (fun resolution -> resolution.coordinateMatchMethod))
            Assert.IsTrue(
                result.coordinateDiagnostics.osmGapFills
                |> Array.forall (fun resolution ->
                    resolution.coordinateSource = "osm"))
            Assert.IsTrue(
                result.feed.stops
                |> Array.forall (fun stop -> stop.lat.IsSome && stop.lon.IsSome))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Authoritative SR70 survives agreeing and closer disagreeing OSM PLC``() =
        let value =
            message [
                location "57076" "Praha" "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] []
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-authority-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        let pbf = Path.Combine(root, "fixture.osm.pbf")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            File.WriteAllLines(sr70, [|
                "57076,Praha,50.083,14.435"
                "57016,Kolín,50.026,15.214"
            |])
            writeOsmPbf pbf [|
                osmNode 1L 50.083 14.435 [
                    "railway", "station"; "ref:EU:PLC", "CZ57076"
                ]
                osmNode 2L 50.100 14.500 [
                    "railway", "station"; "ref:EU:PLC", "CZ57016"
                ]
            |]
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None (Some pbf) None
            Assert.AreEqual(
                2,
                result.coordinateDiagnostics.authoritativeSr70Resolutions.Length)
            Assert.AreEqual(
                1,
                result.coordinateDiagnostics.osmSr70Disagreements.Length)
            Assert.AreEqual(
                "CZ:57016",
                result.coordinateDiagnostics.osmSr70Disagreements.[0].sourceLocationId)
            let kolin =
                result.feed.stops
                |> Array.filter (fun stop -> stop.id.Contains(":57016"))
            Assert.IsTrue(
                kolin
                |> Array.forall (fun stop ->
                    stop.lat = Some 50.026M && stop.lon = Some 15.214M))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Invalid and conflicting SR70 identities fall back to exact OSM PLC``() =
        let value =
            message [
                location "57076" "Praha" "08:00:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
            ] []
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-sr70-gap-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        let pbf = Path.Combine(root, "fixture.osm.pbf")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            File.WriteAllLines(sr70, [|
                "57076,Praha,invalid,invalid"
                "57016,Kolín,50.026,15.214"
                "57016,Kolín,50.027,15.215"
            |])
            writeOsmPbf pbf [|
                osmNode 1L 50.083 14.435 [
                    "railway", "station"; "ref:EU:PLC", "CZ57076"
                ]
                osmNode 2L 50.026 15.214 [
                    "railway", "station"; "ref:EU:PLC", "CZ57016"
                ]
            |]
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None (Some pbf) None
            Assert.AreEqual(1, result.coordinateDiagnostics.osmGapFills.Length)
            Assert.AreEqual(1, result.coordinateDiagnostics.estimatedResolutions.Length)
            CollectionAssert.Contains(
                result.coordinateDiagnostics.invalidSr70Identities,
                "CZ:57076")
            CollectionAssert.Contains(
                result.coordinateDiagnostics.conflictingSr70Codes,
                "57016")
            Assert.IsTrue(
                result.coordinateDiagnostics.osmGapFills
                |> Array.forall (fun resolution ->
                    resolution.coordinateMatchMethod = "ref_eu_plc"))
            Assert.IsTrue(
                result.coordinateDiagnostics.corridorRejectedOsmCandidates
                |> Array.exists (fun value ->
                    value.StartsWith("CZ:57016:ref_eu_plc:")))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``OSM name matching rejects wrong country ambiguity and corridor breaks``() =
        let value =
            message [
                location "57076" "Praha" "08:00:00" ["0001"] None []
                location "44444" "Far Name" "08:10:00" ["0001"] None []
                location "57016" "Kolín" "08:20:00" ["0001"] None []
                location "55555" "Ambiguous" "08:30:00" ["0001"] None []
                location "66666" "Wrong Country" "08:40:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "AT"
        value.CzpttInformation.CzpttLocation.[3].Location.CountryCodeIso <- "DE"
        value.CzpttInformation.CzpttLocation.[4].Location.CountryCodeIso <- "SK"
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-osm-veto-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        let pbf = Path.Combine(root, "fixture.osm.pbf")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            File.WriteAllLines(sr70, [|
                "57076,Praha,50.083,14.435"
                "57016,Kolín,50.026,15.214"
            |])
            writeOsmPbf pbf [|
                osmNode 1L 0.0 0.0 [
                    "railway", "station"
                    "name", "Far Name"
                    "addr:country", "AT"
                ]
                osmNode 2L 50.03 15.22 [
                    "railway", "station"
                    "name", "Other A"
                    "ref:EU:PLC", "DE55555"
                ]
                osmNode 3L 50.04 15.22 [
                    "railway", "station"
                    "name", "Other B"
                    "ref:EU:PLC", "DE55555"
                ]
                osmNode 4L 48.1 17.1 [
                    "railway", "station"
                    "name", "Wrong Country"
                    "addr:country", "PL"
                ]
            |]
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None (Some pbf) None
            Assert.AreEqual(
                0,
                result.coordinateDiagnostics.unresolvedPassengerPointIds.Length)
            CollectionAssert.AreEquivalent(
                [| "AT:44444"; "SK:66666" |],
                result.coordinateDiagnostics.estimatedResolutions
                |> Array.map (fun resolution -> resolution.sourceLocationId))
            Assert.IsTrue(
                result.coordinateDiagnostics.osmGapFills
                |> Array.exists (fun resolution ->
                    resolution.sourceLocationId = "DE:55555"
                    && resolution.coordinateMatchMethod = "ref_eu_plc"))
            Assert.IsTrue(
                result.coordinateDiagnostics.corridorRejectedOsmCandidates
                |> Array.exists (fun value -> value.StartsWith("AT:44444:")))
            Assert.IsTrue(
                result.coordinateDiagnostics.ambiguousOsmCandidates
                |> Array.exists (fun value -> value.StartsWith("DE:55555:ref_eu_plc:")))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Implausible OSM PLC falls through to a plausible reviewed alias``() =
        let value =
            message [
                location "57076" "Praha" "08:00:00" ["0001"] None []
                location "44444" "Foreign" "08:15:00" ["0001"] None []
                location "57016" "Kolín" "08:30:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "AT"
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-osm-secondary-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        let pbf = Path.Combine(root, "fixture.osm.pbf")
        let aliases = Path.Combine(root, "aliases.json")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            File.WriteAllLines(sr70, [|
                "57076,Praha,50.083,14.435"
                "57016,Kolín,50.026,15.214"
            |])
            writeOsmPbf pbf [|
                osmNode 1L 0.0 0.0 [
                    "railway", "station"
                    "ref:EU:PLC", "AT44444"
                ]
                osmNode 2L 50.05 14.82 [
                    "railway", "station"
                    "name", "Foreign"
                    "addr:country", "AT"
                ]
            |]
            File.WriteAllText(aliases, """{"AT:44444":"osm:node:2"}""")
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None (Some pbf) (Some aliases)
            Assert.AreEqual(1, result.coordinateDiagnostics.osmGapFills.Length)
            Assert.AreEqual(
                "reviewed_alias",
                result.coordinateDiagnostics.osmGapFills.[0].coordinateMatchMethod)
            Assert.AreEqual(
                Some "osm:node:2",
                result.coordinateDiagnostics.osmGapFills.[0].coordinateSourceObjectId)
            Assert.IsTrue(
                result.coordinateDiagnostics.corridorRejectedOsmCandidates
                |> Array.exists (fun value ->
                    value.StartsWith("AT:44444:ref_eu_plc:osm:node:1")))
            Assert.AreEqual(0, result.coordinateDiagnostics.estimatedResolutions.Length)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``One anomalous timing occurrence does not veto an otherwise plausible name match``() =
        let value =
            message [
                location "57076" "Anchor A" "08:00:00" ["0001"] None []
                location "44444" "Foreign" "08:10:00" ["0001"] None []
                location "57016" "Anchor B" "08:20:00" ["0001"] None []
                location "44444" "Foreign" "08:20:30" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "AT"
        value.CzpttInformation.CzpttLocation.[3].Location.CountryCodeIso <- "AT"
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-osm-occurrence-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        let pbf = Path.Combine(root, "fixture.osm.pbf")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            File.WriteAllLines(sr70, [|
                "57076,Anchor A,50.08,14.44"
                "57016,Anchor B,50.08,14.60"
            |])
            writeOsmPbf pbf [|
                osmNode 1L 50.08 14.52 [
                    "railway", "station"
                    "name", "Foreign"
                ]
            |]
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None (Some pbf) None
            Assert.AreEqual(1, result.coordinateDiagnostics.osmGapFills.Length)
            Assert.AreEqual(
                "normalized_exact_name",
                result.coordinateDiagnostics.osmGapFills.[0].coordinateMatchMethod)
            Assert.AreEqual(0, result.coordinateDiagnostics.estimatedResolutions.Length)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Fuzzy matching preserves Ost and estimated GTFS names are marked``() =
        let value =
            message [
                location "57076" "Schmilka-Hirschmühle" "08:00:00" ["0001"] None []
                location "10535" "Bad Schandau Ost" "08:10:00" ["0001"] None []
                location "57016" "Krippen Hp" "08:20:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "DE"
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-osm-qualifier-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        let pbf = Path.Combine(root, "fixture.osm.pbf")
        Directory.CreateDirectory(root) |> ignore
        try
            writeMessage input value
            File.WriteAllLines(sr70, [|
                "57076,Schmilka-Hirschmühle,50.900,14.180"
                "57016,Krippen Hp,50.920,14.220"
            |])
            writeOsmPbf pbf [|
                osmNode 1L 50.9189 14.1389 [
                    "railway", "station"
                    "name", "Bad Schandau"
                    "addr:country", "DE"
                ]
            |]

            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None (Some pbf) None
            Assert.IsFalse(
                result.coordinateDiagnostics.osmGapFills
                |> Array.exists (fun resolution ->
                    resolution.sourceLocationId = "DE:10535"))
            Assert.IsTrue(
                result.coordinateDiagnostics.estimatedResolutions
                |> Array.exists (fun resolution ->
                    resolution.sourceLocationId = "DE:10535"
                    && resolution.coordinateMatchMethod = "route_time"))

            let estimatedStops =
                result.feed.stops
                |> Array.filter (fun stop ->
                    stop.id.StartsWith("czptt:stop:DE:10535"))
            Assert.AreEqual(2, estimatedStops.Length)
            Assert.IsTrue(
                estimatedStops
                |> Array.forall (fun stop ->
                    stop.name = "Bad Schandau Ost [?]"
                    && not (stop.name.EndsWith(" [?] [?]"))))
            Assert.IsTrue(
                result.feed.stops
                |> Array.filter (fun stop ->
                    stop.id.StartsWith("czptt:stop:CZ:"))
                |> Array.forall (fun stop -> not (stop.name.EndsWith(" [?]"))))

            use stream =
                File.OpenRead(Path.Combine(output, "operational_points.parquet"))
            let points =
                ParquetSerializer.DeserializeUntypedAsync(stream)
                    .GetAwaiter().GetResult()
            let sourcePoint =
                points.Data
                |> Seq.find (fun row ->
                    string row.["source_location_id"] = "DE:10535")
            Assert.AreEqual("Bad Schandau Ost", string sourcePoint.["source_name"])
            Assert.AreEqual("estimated", string sourcePoint.["coordinate_source"])
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Dense passenger corridor wins independent of input order``() =
        let dense =
            message [
                location "10001" "Dense passenger A" "08:00:00" ["0001"] None []
                location "10003" "Dense operational A" "08:05:00" [] None []
                location "10535" "Missing point" "08:10:00" ["0001"] None []
                location "10004" "Dense operational B" "08:15:00" [] None []
                location "10002" "Dense passenger B" "08:20:00" ["0001"] None []
            ] []
            |> setCore "000000000001"
        dense.CzpttInformation.CzpttLocation.[2].Location.CountryCodeIso <- "DE"
        let sparse =
            message [
                location "10005" "Sparse passenger A" "09:00:00" ["0001"] None []
                location "10003" "Sparse operational A1" "09:05:00" [] None []
                location "10003" "Sparse operational A2" "09:10:00" [] None []
                location "10535" "Missing point" "09:20:00" ["0001"] None []
                location "10004" "Sparse operational B1" "09:30:00" [] None []
                location "10004" "Sparse operational B2" "09:35:00" [] None []
                location "10006" "Sparse passenger B" "09:40:00" ["0001"] None []
            ] []
            |> setCore "000000000002"
        sparse.CzpttInformation.CzpttLocation.[3].Location.CountryCodeIso <- "DE"
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-dense-{Guid.NewGuid():N}")
        let sr70 = Path.Combine(root, "SR70.csv")
        Directory.CreateDirectory(root) |> ignore
        try
            File.WriteAllLines(sr70, [|
                "10001,Dense passenger A,50.0,14.0"
                "10002,Dense passenger B,50.0,14.4"
                "10003,Operational A,49.0,10.0"
                "10004,Operational B,49.0,12.0"
                "10005,Sparse passenger A,49.0,10.0"
                "10006,Sparse passenger B,49.0,12.0"
            |])
            let run name denseFile sparseFile =
                let input = Path.Combine(root, name, "input")
                let output = Path.Combine(root, name, "output")
                Directory.CreateDirectory(input) |> ignore
                writeMessage (Path.Combine(input, denseFile)) dense
                writeMessage (Path.Combine(input, sparseFile)) sparse
                let result =
                    CzPttBundle.writeSidecars
                        catalog CzPttToGtfs.Gtfs input output
                        (Some sr70) None None None
                let stop =
                    result.feed.stops
                    |> Array.find (fun candidate ->
                        candidate.id.StartsWith("czptt:stop:DE:10535")
                        && candidate.parentStation.IsNone)
                result, float stop.lat.Value, float stop.lon.Value

            let first, firstLatitude, firstLongitude =
                run "first" "z-dense.xml" "a-sparse.xml"
            let second, secondLatitude, secondLongitude =
                run "second" "a-dense.xml" "z-sparse.xml"
            Assert.AreEqual(50.0, firstLatitude, 0.000001)
            Assert.AreEqual(14.2, firstLongitude, 0.000001)
            Assert.AreEqual(firstLatitude, secondLatitude, 0.000001)
            Assert.AreEqual(firstLongitude, secondLongitude, 0.000001)
            Assert.AreEqual(
                "route_time",
                first.coordinateDiagnostics.estimatedResolutions
                |> Array.find (fun resolution ->
                    resolution.sourceLocationId = "DE:10535")
                |> fun resolution -> resolution.coordinateMatchMethod)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Estimated coordinates never become anchors``() =
        let anchored =
            message [
                location "10001" "Anchor A" "08:00:00" ["0001"] None []
                location "10535" "Estimated first" "08:10:00" ["0001"] None []
                location "10002" "Anchor B" "08:20:00" ["0001"] None []
            ] []
            |> setCore "000000000001"
        anchored.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "DE"
        let unanchored =
            message [
                location "10535" "Estimated first" "09:00:00" ["0001"] None []
                location "10536" "Must remain unresolved" "09:10:00" ["0001"] None []
            ] []
            |> setCore "000000000002"
        for call in unanchored.CzpttInformation.CzpttLocation do
            call.Location.CountryCodeIso <- "DE"
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-no-estimate-anchor-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        Directory.CreateDirectory(input) |> ignore
        try
            writeMessage (Path.Combine(input, "anchored.xml")) anchored
            writeMessage (Path.Combine(input, "unanchored.xml")) unanchored
            File.WriteAllLines(sr70, [|
                "10001,Anchor A,50.0,14.0"
                "10002,Anchor B,50.0,14.2"
            |])
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None None None
            CollectionAssert.Contains(
                result.coordinateDiagnostics.estimatedResolutions
                |> Array.map (fun resolution -> resolution.sourceLocationId),
                "DE:10535")
            let unresolved =
                String.concat "," result.coordinateDiagnostics.unresolvedPassengerPointIds
            let estimated =
                result.coordinateDiagnostics.estimatedResolutions
                |> Array.map (fun value -> value.sourceLocationId)
                |> String.concat ","
            Assert.IsTrue(
                result.coordinateDiagnostics.unresolvedPassengerPointIds
                |> Array.contains "czptt:stop:DE:10536",
                $"unresolved={unresolved}; estimated={estimated}")
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Unresolved timed-only points are retained only in operational Parquet``() =
        let value =
            message [
                location "10001" "Passenger A" "08:00:00" ["0001"] None []
                location "10535" "Unresolved timing point" "08:10:00" [] None []
                location "10002" "Passenger B" "08:20:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "DE"
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-drop-timing-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        Directory.CreateDirectory(root) |> ignore
        try
            writeMessage input value
            File.WriteAllLines(sr70, [|
                "10001,Passenger A,50.0,14.0"
                "10002,Passenger B,50.0,14.2"
            |])
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None None None
            Assert.IsFalse(
                result.coordinateDiagnostics.estimatedResolutions
                |> Array.exists (fun resolution ->
                    resolution.sourceLocationId = "DE:10535"))
            Assert.IsFalse(
                result.feed.stops
                |> Array.exists (fun stop ->
                    stop.id.StartsWith("czptt:stop:DE:10535")))
            Assert.IsFalse(
                result.feed.stopTimes
                |> Array.exists (fun stopTime ->
                    stopTime.stopId.StartsWith("czptt:stop:DE:10535")))
            let operationalCall =
                result.operationalCalls
                |> Array.find (fun call ->
                    call.countryCode = "DE" && call.primaryCode = "10535")
            Assert.AreEqual(0, operationalCall.generatedTripIds.Length)

            use stream =
                File.OpenRead(Path.Combine(output, "operational_points.parquet"))
            let points =
                ParquetSerializer.DeserializeUntypedAsync(stream)
                    .GetAwaiter().GetResult()
            let sourcePoint =
                points.Data
                |> Seq.find (fun row ->
                    string row.["source_location_id"] = "DE:10535")
            Assert.AreEqual("Unresolved timing point", string sourcePoint.["source_name"])
            Assert.IsFalse(sourcePoint.ContainsKey("coordinate_source"))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Unknown-country OSM names allow railway suffix and fuzzy matching``() =
        let value =
            message [
                location "57076" "Praha" "08:00:00" ["0001"] None []
                location "44444" "Unterretzbach z" "08:10:00" ["0001"] None []
                location "55555" "Głuchołas.F-ka Mebli" "08:20:00" ["0001"] None []
                location "66666" "Bärenstein" "08:25:00" ["0001"] None []
                location "57016" "Kolín" "08:30:00" ["0001"] None []
            ] []
        value.CzpttInformation.CzpttLocation.[1].Location.CountryCodeIso <- "AT"
        value.CzpttInformation.CzpttLocation.[2].Location.CountryCodeIso <- "PL"
        value.CzpttInformation.CzpttLocation.[3].Location.CountryCodeIso <- "DE"
        let serializer =
            System.Xml.Serialization.XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        let root =
            Path.Combine(Path.GetTempPath(), $"jrutil-czptt-osm-aggressive-{Guid.NewGuid():N}")
        let input = Path.Combine(root, "input.xml")
        let output = Path.Combine(root, "output")
        let sr70 = Path.Combine(root, "SR70.csv")
        let pbf = Path.Combine(root, "fixture.osm.pbf")
        Directory.CreateDirectory(root) |> ignore
        try
            use writer = new StreamWriter(input)
            serializer.Serialize(writer, value)
            writer.Close()
            File.WriteAllLines(sr70, [|
                "57076,Praha,50.083,14.435"
                "57016,Kolín,50.026,15.214"
            |])
            writeOsmPbf pbf [|
                osmNode 1L 50.07 14.70 [
                    "railway", "station"
                    "name", "Unterretzbach"
                ]
                osmNode 2L 50.05 14.95 [
                    "railway", "halt"
                    "name", "Głuchołazy Fabryka Mebli"
                    "addr:country", "DE"
                ]
                osmNode 3L 50.04 15.10 [
                    "railway", "halt"
                    "name", "Bärenstein (Annaberg)"
                ]
            |]
            let result =
                CzPttBundle.writeSidecars
                    catalog CzPttToGtfs.Gtfs input output
                    (Some sr70) None (Some pbf) None
            Assert.AreEqual(3, result.coordinateDiagnostics.osmGapFills.Length)
            Assert.IsTrue(
                result.coordinateDiagnostics.osmGapFills
                |> Array.exists (fun resolution ->
                    resolution.sourceLocationId = "AT:44444"
                    && resolution.coordinateMatchMethod = "normalized_railway_name"))
            Assert.IsTrue(
                result.coordinateDiagnostics.osmGapFills
                |> Array.exists (fun resolution ->
                    resolution.sourceLocationId = "PL:55555"
                    && resolution.coordinateMatchMethod = "normalized_fuzzy_name"))
            Assert.IsTrue(
                result.coordinateDiagnostics.osmGapFills
                |> Array.exists (fun resolution ->
                    resolution.sourceLocationId = "DE:66666"
                    && resolution.coordinateMatchMethod = "normalized_railway_name"))
            Assert.AreEqual(0, result.coordinateDiagnostics.estimatedResolutions.Length)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
