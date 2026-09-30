// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfToGtfs

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open FSharp.Data
open NetTopologySuite.Geometries
open NodaTime
open NodaTime.Calendars
open Serilog

open JrUtil
open JrUtil.Holidays
open JrUtil.GeoData.Common
open JrUtil.GeoData.Osm
open JrUtil.JdfGtfsRules
open JrUtil.JdfPostPlan
open JrUtil.JdfCalendar
open JrUtil.JdfInternationalFilter


// TODO: Naming in this whole module
let convertToGtfsAgency: JdfModel.Agency -> GtfsModel.Agency = fun jdfAgency ->
    {
        // Unfortunately in JDF most primary keys are split
        // between two fields, so we just concatenate them
        id = Some (jdfAgencyId jdfAgency.id jdfAgency.idDistinction)
        name = jdfAgency.name
        url =
            jdfAgency.website
            |> Option.map (fun url ->
                if not <| Regex.IsMatch(url, @"https?://")
                then "http://" + url
                else url
            )
        // This will have to be adjusted for slovak datasets
        timezone = "Europe/Prague"
        lang = Some "cs"
        phone = Some jdfAgency.officePhoneNum
        fareUrl = None
        email = jdfAgency.email
    }

let inferredPostId (plan: PostEstimationPlan) (selection: DerivedPostSelection) =
    match plan.inferredLocationOrdinals |> Map.tryFind (selection.stopId, selection.locationId) with
    | Some ordinal -> $"{jdfStopId selection.stopId}:est:{ordinal}"
    | None ->
        invalidOp
            $"Inferred JDF post has no deterministic ordinal: stop={selection.stopId}; location={selection.locationId}"

let internal getGtfsStopsWithPlan (plan: PostEstimationPlan)
                                    (jdfBatch: JdfModel.JdfBatch) =
    let stopLocationsById =
        jdfBatch.stopLocations
        |> Seq.groupBy (fun sl -> sl.stopId)
        |> Seq.map (fun (stopId, locations) ->
            let location =
                locations
                |> Seq.sortBy (fun sl ->
                    match sl.precision with
                    | JdfModel.StopPrecise -> 0
                    | JdfModel.Estimated -> 1)
                |> Seq.head
            stopId, location)
        |> Map

    let zonesByStop =
        jdfBatch.routeStops
        |> Seq.collect (fun routeStop ->
            Jdf.normalizeZoneTokens [routeStop.zone]
            |> Seq.map (fun zoneCode ->
                routeStop.stopId,
                jdfSourceZoneId routeStop.routeId
                                routeStop.routeDistinction
                                zoneCode,
                zoneCode))
        |> Seq.groupBy (fun (stopId, _, _) -> stopId)
        |> Seq.map (fun (stopId, memberships) ->
            let memberships =
                memberships
                |> Seq.map (fun (_, zoneId, zoneCode) -> zoneId, zoneCode)
                |> Seq.distinct
                |> Seq.toArray
            stopId,
            match memberships with
            | [| _, zoneCode |] -> Some zoneCode
            | _ -> None)
        |> Map

    let gtfsStops =
        jdfBatch.stops |> Array.map (fun jdfStop ->
            let location =
                stopLocationsById
                |> Map.tryFind jdfStop.id
            let stopName =
                let name = getStopName jdfStop
                match location with
                | Some value when value.precision <> JdfModel.StopPrecise ->
                    Gtfs.markApproximateStopName name
                | _ -> name
            {
                id = jdfStopId jdfStop.id
                code = None
                name = stopName
                description = None
                lat = location |> Option.map (fun value -> value.lat)
                lon = location |> Option.map (fun value -> value.lon)
                // GTFS only permits one zone_id. Plural memberships are
                // represented losslessly in cz_stop_zones.txt.
                zoneId = zonesByStop |> Map.tryFind jdfStop.id |> Option.flatten
                url = None
                locationType = Some GtfsModel.Station
                parentStation = None
                // TODO: Try to guess from jdfStop.country
                timezone = Some "Europe/Prague"

                wheelchairBoarding =
                    // This doesn't use "2" ("not possible"), because there's no
                    // corresponding JDF attribute
                    if jdfStop.attributes
                       |> Jdf.parseAttributes jdfBatch
                       |> Set.contains JdfModel.WheelchairAccessible
                    then Some 1
                    else Some 0

                platformCode = None
            }: GtfsModel.Stop)
    let gtfsStopsById = Map <| seq { for s in gtfsStops -> s.id, s }
    let gtfsUnspecifiedStops =
        jdfBatch.stops |> Array.map (fun jdfStop ->
            let parentStop = gtfsStopsById.[jdfStopId jdfStop.id]
            { parentStop with
                id = jdfUnspecifiedStopId jdfStop.id
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
            }: GtfsModel.Stop)
    let postNumbersById = stopPostNumbersById jdfBatch
    let gtfsStopPosts =
        jdfBatch.stopPosts |> Array.map (fun jdfStopPost ->
            let parentStop = gtfsStopsById.[jdfStopId jdfStopPost.stopId]
            let selection = plan.authored |> Map.tryFind (jdfStopPost.stopId, $"id:{jdfStopPost.stopPostId}")
            { parentStop with

                id = jdfStopPostId 
                                   jdfStopPost.stopId
                                   jdfStopPost.stopPostId
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
                platformCode =
                    jdfStopPost.postName
                    |> Option.bind nonEmptyTrimmed
                    |> Option.orElseWith (fun () ->
                        postNumbersById
                        |> Map.tryFind (jdfStopPost.stopId,
                                        jdfStopPost.stopPostId))
                    |> Option.orElse (Some (string jdfStopPost.stopPostId))
                lat = selection |> Option.map (fun value -> value.lat) |> Option.orElse parentStop.lat
                lon = selection |> Option.map (fun value -> value.lon) |> Option.orElse parentStop.lon
            }: GtfsModel.Stop)
    let gtfsNumberedStopPosts =
        derivedStopPosts jdfBatch
        |> Array.map (fun (stopId, stopPostNum) ->
            let parentStop = gtfsStopsById.[jdfStopId stopId]
            let selection = plan.authored |> Map.tryFind (stopId, $"num:{stopPostNum}")
            { parentStop with
                id = jdfStopPostNumId stopId stopPostNum
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
                platformCode = Some stopPostNum
                lat = selection |> Option.map (fun value -> value.lat) |> Option.orElse parentStop.lat
                lon = selection |> Option.map (fun value -> value.lon) |> Option.orElse parentStop.lon
            }: GtfsModel.Stop)
    let inferredPosts =
        plan.inferredLocations
        |> Seq.sortBy (fun value -> value.stopId, value.locationId)
        |> Seq.map (fun selection ->
            let parentStop = gtfsStopsById.[jdfStopId selection.stopId]
            { parentStop with
                id = inferredPostId plan selection
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
                platformCode = None
                lat = Some selection.lat
                lon = Some selection.lon
            }: GtfsModel.Stop)
        |> Seq.toArray

    Array.concat [ gtfsStops; gtfsUnspecifiedStops
                   gtfsStopPosts; gtfsNumberedStopPosts; inferredPosts ]

let getGtfsRoutesWithPublicLines
        (publicLineNumbers: Map<string * int, string option>)
                                (jdfBatch: JdfModel.JdfBatch) =
    (getRouteGrouping jdfBatch).groups
    |> Array.map (fun (routeId, jdfRoute) ->
    let publicLineNumber =
        publicLineNumbers.[jdfRoute.id, jdfRoute.idDistinction]
    let routeColor, routeTextColor =
        getGtfsRouteColorsWithDetour publicLineNumber jdfRoute
    {
        id = routeId
        agencyId = Some (jdfAgencyId jdfRoute.agencyId
                                     jdfRoute.agencyDistinction)
        shortName = publicLineNumber
        longName = Some jdfRoute.name
        description = None
        // TODO: Deal with routes that don't allow national service
        // (only international)
        routeType = getGtfsRouteType jdfRoute
        url = None
        color = routeColor
        textColor = routeTextColor
        sortOrder = None
    }: GtfsModel.Route)

let getGtfsRoutes (jdfBatch: JdfModel.JdfBatch) =
    getGtfsRoutesWithPublicLines (getPublicLineNumbers jdfBatch) jdfBatch

let private getGtfsTripsWithEndpoints (endpoints: IDictionary<struct(string * int * int64), int64> option) (jdfBatch: JdfModel.JdfBatch) =
    let lastStopPerTrip = Dictionary<struct (string * int * int64), struct (int64 * int64)>()
    for call in (if endpoints.IsSome then Seq.empty else jdfBatch.tripStops :> seq<_>) do
        let key = struct (call.routeId, call.routeDistinction, call.tripId)
        let order = call.routeStopId * (if Jdf.tripIsReverse call.tripId then -1L else 1L)
        match lastStopPerTrip.TryGetValue(key) with
        | true, struct (oldOrder, _) when oldOrder >= order -> ()
        | _ -> lastStopPerTrip.[key] <- struct (order, call.stopId)
    let stopById = jdfBatch.stops |> Seq.map (fun s -> s.id, s) |> Map
    jdfBatch.trips
    |> Seq.map (fun jdfTrip ->
        let id = jdfTripId jdfTrip.routeId jdfTrip.routeDistinction jdfTrip.id
        let attrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes
        let wheelchairAccessible =
                attrs |> Set.contains JdfModel.WheelchairAccessible
                || attrs |> Set.contains JdfModel.PartlyWheelchairAccessible
        ({
            routeId = gtfsRouteId jdfBatch jdfTrip.routeId jdfTrip.routeDistinction
            serviceId = id
            id = id
            headsign =
                let key = struct (jdfTrip.routeId, jdfTrip.routeDistinction, jdfTrip.id)
                let stopId =
                    match endpoints with
                    | Some values -> values.[key]
                    | None -> let struct(_, stop) = lastStopPerTrip.[key] in stop
                stopById.[stopId]
                |> getStopName
                |> Some
            // This is kind of arbitrary
            // TODO: Customizability?
            shortName = Some (sprintf "%s %d" jdfTrip.routeId jdfTrip.id)
            directionId = Some (if Jdf.tripIsReverse jdfTrip.id
                                then "0" else "1")
            blockId = None
            shapeId = None
            wheelchairAccessible =
                // JDF is a closed-world source for trip accessibility.  An
                // absent @/{ designation means the trip is not accessible,
                // rather than the GTFS "unknown" value.
                Some (if wheelchairAccessible then "1" else "2")
            bikesAllowed =
                Some (if attrs |> Set.contains JdfModel.BicycleTransport
                      then GtfsModel.OneOrMore
                      else GtfsModel.NoBicycles)
        }: GtfsModel.Trip))

[<Flags>]
type private StopTimeAttributeFlags =
    | NoStopTimeFlags = 0
    | RequestStopFlag = 1
    | ConditionalServiceFlag = 2
    | CommissionServiceFlag = 4
    | ExitOnlyFlag = 8
    | BoardingOnlyFlag = 16
    | BorderStopOnlyFlag = 32

type StreamingStopTimeRow = {
    stopTime: GtfsModel.StopTime
    sourceRouteStopId: int64
    /// Merged route version (distinction); route stop numbers are unique only within one
    sourceRouteVersion: int
    routeId: string
}

let private getGtfsStopTimeRowsInternal
                                        (postPlan: PostEstimationPlan)
                                        (jdfBatch: JdfModel.JdfBatch)
                                        (recordEndpoint: struct(string * int * int64) -> int64 -> unit) =
    let periods = Dictionary<int64, Period>()
    let periodOptions = Dictionary<int64, Period option>()
    let periodForSeconds seconds =
        match periods.TryGetValue(seconds) with
        | true, value -> value
        | _ ->
            let value = Period.FromSeconds(seconds)
            periods.[seconds] <- value
            value

    let periodOptionForSeconds seconds =
        match periodOptions.TryGetValue(seconds) with
        | true, value -> value
        | _ ->
            let value = Some (periodForSeconds seconds)
            periodOptions.[seconds] <- value
            value

    // F# options are reference objects. Reusing these values avoids retaining
    // several fresh option wrappers for every stop-time row in national feeds.
    let noService = Some GtfsModel.NoService
    let regularService = Some GtfsModel.RegularlyScheduled
    let coordinationWithDriver = Some GtfsModel.CoordinationWithDriver
    let phoneBefore = Some GtfsModel.PhoneBefore
    let exactTimepoint = Some GtfsModel.Exact

    let unspecifiedStopIds = Dictionary<int64, string>()
    let stopPostIds = Dictionary<struct (int64 * int64), string>()
    let stopPostNumberIds = Dictionary<struct (int64 * string), string>()
    let convertedStopId (inferredSelection: DerivedPostSelection option)
                        (call: JdfModel.TripStop) =
        match call.stopPostId, call.stopPostNum |> Option.bind nonEmptyTrimmed with
        | Some stopPostId, _ ->
            let key = struct (call.stopId, stopPostId)
            match stopPostIds.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = jdfStopPostId call.stopId stopPostId
                stopPostIds.[key] <- value
                value
        | None, Some stopPostNumber ->
            let key = struct (call.stopId, stopPostNumber)
            match stopPostNumberIds.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = jdfStopPostNumId call.stopId stopPostNumber
                stopPostNumberIds.[key] <- value
                value
        | None, None ->
            match inferredSelection with
            | Some selection -> inferredPostId postPlan selection
            | None ->
                match unspecifiedStopIds.TryGetValue(call.stopId) with
                | true, value -> value
                | _ ->
                    let value = jdfUnspecifiedStopId call.stopId
                    unspecifiedStopIds.[call.stopId] <- value
                    value

    let attributeFlagsById = Dictionary<int, StopTimeAttributeFlags>()
    for attribute in jdfBatch.attributeRefs do
        let flag =
            match attribute.value with
            | JdfModel.RequestStop -> StopTimeAttributeFlags.RequestStopFlag
            | JdfModel.ConditionalService -> StopTimeAttributeFlags.ConditionalServiceFlag
            | JdfModel.CommisionServiceOnly -> StopTimeAttributeFlags.CommissionServiceFlag
            | JdfModel.ExitOnly -> StopTimeAttributeFlags.ExitOnlyFlag
            | JdfModel.BoardingOnly -> StopTimeAttributeFlags.BoardingOnlyFlag
            // `$` marks a border/customs stop where passengers may neither board nor alight.
            | JdfModel.BorderStopOnly -> StopTimeAttributeFlags.BorderStopOnlyFlag
            | _ -> StopTimeAttributeFlags.NoStopTimeFlags
        if flag <> StopTimeAttributeFlags.NoStopTimeFlags then
            attributeFlagsById.[attribute.attributeId] <- flag

    let flagsForAttributes (attributes: int option array) =
        let mutable flags = StopTimeAttributeFlags.NoStopTimeFlags
        for attributeId in attributes do
            match attributeId with
            | Some id ->
                match attributeFlagsById.TryGetValue(id) with
                | true, value -> flags <- flags ||| value
                | _ -> ()
            | None -> ()
        flags

    let stopFlagsById = Dictionary<int64, StopTimeAttributeFlags>()
    for stop in jdfBatch.stops do
        stopFlagsById.[stop.id] <- flagsForAttributes stop.attributes

    let modesByRoute = Dictionary<struct (string * int), JdfModel.TransportMode>()
    for route in jdfBatch.routes do
        modesByRoute.[struct (route.id, route.idDistinction)] <- route.transportMode

    let inferredSelectionsForTrip routeId routeDistinction mode
                                      (orderedCalls: JdfModel.TripStop array) =
        let result = Dictionary<int64, DerivedPostSelection>()
        if postPlan.calls.Count > 0 then
            let usable = orderedCalls |> Array.filter callIsUsable
            let patternHash = completePatternHash usable
            let direction = if usable.Length > 0 && Jdf.tripIsReverse usable.[0].tripId then 1 else 0
            let mutable start = 0
            while start < usable.Length do
                let stopId = usable.[start].stopId
                let mutable finish = start + 1
                while finish < usable.Length && usable.[finish].stopId = stopId do
                    finish <- finish + 1
                let blockLength = finish - start
                for index = start to finish - 1 do
                    let call = usable.[index]
                    if authoredPostKey call |> Option.isNone then
                        let role =
                            if blockLength = 1 then "through"
                            elif index = start then "incoming"
                            elif index = finish - 1 then "outgoing"
                            else "interior"
                        let contextKey = {
                            stopId = stopId; mode = mode; lineId = routeId
                            routeDistinction = routeDistinction; direction = direction
                            patternHash = patternHash; position = index
                            sameStopBlockRole = role
                        }
                        match postPlan.calls.TryGetValue(contextKey) with
                        | true, selection -> result.[call.routeStopId] <- selection
                        | _ -> ()
                start <- finish
        result

    // Calls of one trip are contiguous (JdfCallStore spans).
    let tripStopGroups =
        let calls = jdfBatch.tripStops
        let spans = ResizeArray<struct (string * int * int64 * int * int * string)>()
        for struct(route, distinction, trip, start, count) in JdfCallStore.tripSpans calls do
            spans.Add(struct(route, distinction, trip, start, count, jdfTripId route distinction trip))
        spans.Sort(Comparer<struct (string * int * int64 * int * int * string)>.Create(
            fun struct (leftRoute,leftDistinction,leftTrip,_,_,leftId)
                struct (rightRoute,rightDistinction,rightTrip,_,_,rightId) ->
                let byId = StringComparer.Ordinal.Compare(leftId,rightId)
                if byId <> 0 then byId
                else compare (leftRoute,leftDistinction,leftTrip)
                             (rightRoute,rightDistinction,rightTrip)))
        spans
        |> Seq.map (fun struct (routeId,distinction,tripId,start,count,_) ->
            let tripCalls = Array.zeroCreate<JdfModel.TripStop> count
            for index = 0 to count - 1 do tripCalls.[index] <- calls.[start + index]
            (routeId,distinction,tripId),tripCalls)

    tripStopGroups
    // We have to deal with stop times for each trip separately,
    // because we have to count 23:59 -> 00:00 crossings
    // to even attempt to comply with GTFS and distinquish days
    // Not even this is enough, though. Imagine a trip that sets out
    // at 8:00 and, without any intermediate stops, arrives at 9:00
    // the next day.
    |> Seq.collect (fun ((routeId, routeDistinction, tripId), jdfTripStops) ->
        assert (jdfTripStops.Length >= 2)
        let isReverseTrip = jdfTripStops.[0].tripId % 2L = 0L
        let gtfsTripId = jdfTripId routeId routeDistinction tripId
        let orderedCalls =
            jdfTripStops
            |> Array.sortBy (fun call ->
                call.routeStopId * (if isReverseTrip then -1L else 1L))
        let mutable endpointIndex = orderedCalls.Length - 1
        while endpointIndex > 0 && orderedCalls.[endpointIndex - 1].routeStopId = orderedCalls.[endpointIndex].routeStopId do
            endpointIndex <- endpointIndex - 1
        recordEndpoint (struct(routeId, routeDistinction, tripId)) orderedCalls.[endpointIndex].stopId
        let inferredSelections =
            inferredSelectionsForTrip
                routeId routeDistinction
                modesByRoute.[struct (routeId, routeDistinction)] orderedCalls

        let mutable lastTimeDT: LocalTime option = None
        let mutable dayOffsetSeconds = 0L

        orderedCalls
        |> Seq.mapi (fun i jdfTripStop ->
            match jdfTripStop.departureTime with
            | Some JdfModel.Passing | Some JdfModel.NotPassing -> None
            // The JDF specification allows stops that aren't served to have a
            // blank arrival and departure time. In GTFS, such stops are just
            // omitted.
            | None when jdfTripStop.arrivalTime = None -> None
            | _ ->
                let tripStopTimeExtract tst =
                    tst
                    |> Option.map (fun x ->
                        match x with
                        | JdfModel.StopTime dt -> dt
                        | _ -> failwith "Invalid data"
                    )

                let adjustTime dtOpt =
                    match dtOpt with
                    | None -> None
                    | Some dt ->
                        match lastTimeDT with
                        | Some lt ->
                            if lt > dt
                            then dayOffsetSeconds <- dayOffsetSeconds + 86400L
                        | None -> ()
                        lastTimeDT <- Some dt
                        let seconds =
                            int64 (dt.Hour * 3600 + dt.Minute * 60 + dt.Second)
                            + dayOffsetSeconds
                        periodOptionForSeconds seconds

                let arrTime =
                    tripStopTimeExtract jdfTripStop.arrivalTime
                    |> adjustTime
                let depTime =
                    tripStopTimeExtract jdfTripStop.departureTime
                    |> adjustTime

                let combinedFlags =
                    stopFlagsById.[jdfTripStop.stopId]
                    ||| flagsForAttributes jdfTripStop.attributes
                let hasFlag flag = (combinedFlags &&& flag) <> StopTimeAttributeFlags.NoStopTimeFlags

                let service =
                    if hasFlag StopTimeAttributeFlags.RequestStopFlag then
                        coordinationWithDriver
                    else if hasFlag StopTimeAttributeFlags.ConditionalServiceFlag then
                    // "ConditionalService" is a very broad attribute
                    // which basically says "look at the description to
                    // find out". GTFS doesn't have such an option,
                    // and PhoneBefore implies some human interaction.
                    // so that's my choice.
                        phoneBefore
                    else if hasFlag StopTimeAttributeFlags.CommissionServiceFlag then
                        phoneBefore
                    else
                        regularService

                let stopTime: GtfsModel.StopTime = {
                    tripId = gtfsTripId
                    arrivalTime = arrTime |> Option.orElse depTime
                    departureTime = depTime |> Option.orElse arrTime
                    stopId =
                        match inferredSelections.TryGetValue(jdfTripStop.routeStopId) with
                        | true, selection -> convertedStopId (Some selection) jdfTripStop
                        | _ -> convertedStopId None jdfTripStop
                    stopSequence = i
                    headsign = None
                    pickupType =
                        if hasFlag StopTimeAttributeFlags.ExitOnlyFlag
                           || hasFlag StopTimeAttributeFlags.BorderStopOnlyFlag
                        then noService
                        else service
                    dropoffType =
                        if hasFlag StopTimeAttributeFlags.BoardingOnlyFlag
                           || hasFlag StopTimeAttributeFlags.BorderStopOnlyFlag
                        then noService
                        else service
                    shapeDistTraveled = jdfTripStop.kilometer
                    // This will be dynamic when support for JDF's
                    // min/max times comes.
                    timepoint = exactTimepoint
                    stopZoneIds = None
                }
                Some { stopTime = stopTime; sourceRouteStopId = jdfTripStop.routeStopId
                       sourceRouteVersion = routeDistinction
                       routeId = gtfsRouteId jdfBatch routeId routeDistinction }
        )
        |> Seq.choose id
    )

let getCzRoutes (publicLineNumbers: Map<string * int, string option>)
                (jdfBatch: JdfModel.JdfBatch) =
    (getRouteGrouping jdfBatch).groups
    |> Array.map (fun (routeId, route) ->
        {
            routeId = routeId
            cisLineId = Some route.id
            publicLineNumber =
                publicLineNumbers.[route.id, route.idDistinction]
            sourceProvenance = sprintf "jdf:%s" jdfBatch.version.version
        }: GtfsModel.CzRoute)

let getCzTrips (tripsToDelete: Set<string>)
               (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.trips
    |> Seq.choose (fun trip ->
        let sourceTripId =
            jdfTripId trip.routeId trip.routeDistinction trip.id
        if tripsToDelete |> Set.contains sourceTripId then None
        else
            Some ({
                tripId = sourceTripId
                cisLineId = Some trip.routeId
                cisTripId = Some trip.id
                trainNumber = None
                sourceTripIds = Some sourceTripId
                coverageSources = Some "jdf"
            }: GtfsModel.CzTrip))
    |> Seq.toArray

let private getCzStopsWithPlan (plan: PostEstimationPlan)
                               (jdfBatch: JdfModel.JdfBatch) =
    let cisStopId (_: int64) : int64 option = None
    let sourceStopId stopId = sprintf "jdf:stop:%d" stopId
    let sourcePostId stopId stopPostId =
        sprintf "jdf:stop:%d:post:id:%d" stopId stopPostId
    let sourcePostNum (stopId: int64) (stopPostNum: string) =
        sprintf "jdf:stop:%d:post:%s"
                stopId
                (Uri.EscapeDataString(stopPostNum))
    let postNumbersById = stopPostNumbersById jdfBatch

    let stops =
        jdfBatch.stops
        |> Array.map (fun stop ->
            let stopId = jdfStopId stop.id
            {
                stopId = stopId
                stopPlaceId = stopId
                cisStopId = cisStopId stop.id
                postId = None
                aswId = None
                sourceIds = Some (sourceStopId stop.id)
            }: GtfsModel.CzStop)
    let unspecifiedStops =
        jdfBatch.stops
        |> Array.map (fun stop ->
            {
                stopId = jdfUnspecifiedStopId stop.id
                stopPlaceId = jdfStopId stop.id
                cisStopId = cisStopId stop.id
                postId = None
                aswId = None
                sourceIds = None
            }: GtfsModel.CzStop)
    let stopPosts =
        jdfBatch.stopPosts
        |> Array.map (fun stopPost ->
            let sourceIds =
                match postNumbersById
                      |> Map.tryFind (stopPost.stopId, stopPost.stopPostId) with
                | Some stopPostNum ->
                    String.Join(",", [|
                        sourcePostId stopPost.stopId stopPost.stopPostId
                        sourcePostNum stopPost.stopId stopPostNum
                    |])
                | None -> sourcePostId stopPost.stopId stopPost.stopPostId
            {
                stopId = jdfStopPostId 
                                           stopPost.stopId
                                           stopPost.stopPostId
                stopPlaceId = jdfStopId stopPost.stopId
                cisStopId = cisStopId stopPost.stopId
                postId = Some (string stopPost.stopPostId)
                aswId = None
                sourceIds = Some sourceIds
            }: GtfsModel.CzStop)
    let numberedStopPosts =
        derivedStopPosts jdfBatch
        |> Array.map (fun (stopId, stopPostNum) ->
            {
                stopId = jdfStopPostNumId stopId stopPostNum
                stopPlaceId = jdfStopId stopId
                cisStopId = cisStopId stopId
                postId = Some stopPostNum
                aswId = None
                sourceIds = Some (sourcePostNum stopId stopPostNum)
            }: GtfsModel.CzStop)
    let inferredPosts =
        plan.inferredLocations
        |> Seq.sortBy (fun value -> value.stopId, value.locationId)
        |> Seq.map (fun (selection: DerivedPostSelection) ->
            let inferredCisStopId = cisStopId selection.stopId
            ({
                stopId = inferredPostId plan selection
                stopPlaceId = jdfStopId selection.stopId
                cisStopId = inferredCisStopId
                postId = None
                aswId = None
                sourceIds = None
            }: GtfsModel.CzStop))
        |> Seq.toArray
    Array.concat [stops; unspecifiedStops; stopPosts; numberedStopPosts; inferredPosts]

let getCzStopZones (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.routeStops
    |> Seq.collect (fun routeStop ->
        Jdf.normalizeZoneTokens [routeStop.zone]
        |> Seq.map (fun zoneCode ->
            {
                stopPlaceId = jdfStopId routeStop.stopId
                zoneId = jdfSourceZoneId routeStop.routeId
                                             routeStop.routeDistinction
                                             zoneCode
                zoneCode = zoneCode
                routeId = gtfsRouteId jdfBatch routeStop.routeId
                                      routeStop.routeDistinction
                idsSystemId = None
                sourceProvenance = sprintf "jdf:%s" jdfBatch.version.version
            }: GtfsModel.CzStopZone))
    |> Seq.distinct
    |> Seq.sortBy (fun zone -> zone.stopPlaceId, zone.routeId, zone.zoneId)
    |> Seq.toArray

let private feedInfo (jdfVersion: JdfModel.JdfVersion)
                     (calendar: GtfsModel.CalendarEntry array)
                     (calendarExceptions: GtfsModel.CalendarException array) =
    let addedDates =
        calendarExceptions
        |> Array.choose (fun exceptionDate ->
            if exceptionDate.exceptionType = GtfsModel.ServiceAdded
            then Some exceptionDate.date else None)
    let starts =
        Array.append (calendar |> Array.map (fun entry -> entry.startDate)) addedDates
    let ends =
        Array.append (calendar |> Array.map (fun entry -> entry.endDate)) addedDates
    let startDate = if starts.Length = 0 then None else Some (Array.min starts)
    let endDate = if ends.Length = 0 then None else Some (Array.max ends)
    let versionParts = [
        Some (sprintf "jdf-%s" jdfVersion.version)
        jdfVersion.batchId |> Option.filter (String.IsNullOrWhiteSpace >> not)
        jdfVersion.creationDate
        |> Option.map (fun date ->
            sprintf "%04d%02d%02d" date.Year date.Month date.Day)
    ]
    let version = versionParts |> List.choose id |> String.concat ":" |> Some
    Gtfs.obehyFeedInfo version startDate endDate

let private assembleGtfsFeed (jdfBatch: JdfModel.JdfBatch)
                             (postPlan: PostEstimationPlan)
                             tripsToDelete calendar calendarExceptions publicLineNumbers
                             (referencedStopIds: Set<string>) stopTimes endpoints =
    let allStops = getGtfsStopsWithPlan postPlan jdfBatch
    let requiredParentIds =
        allStops
        |> Seq.filter (fun stop -> referencedStopIds.Contains stop.id)
        |> Seq.choose (fun stop -> stop.parentStation)
        |> Set
    let retainedStopIds = Set.union referencedStopIds requiredParentIds
    let stops = allStops |> Array.filter (fun stop -> retainedStopIds.Contains stop.id)
    let feed: GtfsModel.GtfsFeed = {
        agencies = jdfBatch.agencies |> Array.map convertToGtfsAgency
        stops = stops
        routes = getGtfsRoutesWithPublicLines publicLineNumbers jdfBatch
        trips = getGtfsTripsWithEndpoints endpoints jdfBatch
            |> Seq.filter (fun t ->
                tripsToDelete |> Set.contains t.id |> not)
            |> Seq.toArray
        stopTimes = stopTimes
        shapes = None
        calendar = Some calendar
        calendarExceptions = Some calendarExceptions
        feedInfo = Some (feedInfo jdfBatch.version calendar calendarExceptions)
        transfers = None
        czRoutes = Some (getCzRoutes publicLineNumbers jdfBatch)
        czTrips = Some (getCzTrips tripsToDelete jdfBatch)
        czStops =
            getCzStopsWithPlan postPlan jdfBatch
            |> Array.filter (fun stop -> retainedStopIds.Contains stop.stopId)
            |> Some
        czStopZones =
            getCzStopZones jdfBatch
            |> Array.filter (fun zone -> retainedStopIds.Contains zone.stopPlaceId)
            |> Some
        czTripStopZones = None
    }
    feed

type StreamingFeedPreparation = {
    batch: JdfModel.JdfBatch
    tripsToDelete: Set<string>
    calendarPreparation: CalendarPreparation
    publicLineNumbers: Map<string * int, string option>
    postPlan: PostEstimationPlan
    tripEndpoints: Dictionary<struct(string * int * int64), int64>
}

let prepareGtfsFeedForStreamingBundleWithCalendar (calendar: CalendarPreparation) (batch: JdfModel.JdfBatch) =
    {
        batch = batch
        tripsToDelete = calendar.tripsToDelete
        calendarPreparation = calendar
        publicLineNumbers = getPublicLineNumbers batch
        postPlan = emptyPostEstimationPlan
        tripEndpoints = Dictionary()
    }

// Evidence replay has already performed every post-inference decision.  Keep
// construction of the ordinary GTFS preparation separate so a replay-backed
// conversion cannot accidentally invoke the legacy geometry estimator while
// replacing its result afterwards.
let prepareGtfsFeedForStreamingBundleWithCalendarAndPostPlan
        (calendar:CalendarPreparation) (postPlan:PostEstimationPlan) batch =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "gtfs-conversion"
    {
        batch = batch
        tripsToDelete = calendar.tripsToDelete
        calendarPreparation = calendar
        publicLineNumbers = getPublicLineNumbers batch
        postPlan = postPlan
        tripEndpoints = Dictionary()
    }

let getStreamingBundleStopTimes preparation =
    getGtfsStopTimeRowsInternal
        preparation.postPlan preparation.batch
        (fun key stop -> preparation.tripEndpoints.[key] <- stop)
    |> Seq.map _.stopTime
    |> Seq.filter (fun stopTime ->
        not (preparation.tripsToDelete.Contains stopTime.tripId))

let getStreamingBundleStopTimeRows preparation =
    getGtfsStopTimeRowsInternal
        preparation.postPlan preparation.batch
        (fun key stop -> preparation.tripEndpoints.[key] <- stop)
    |> Seq.filter (fun value ->
        not (preparation.tripsToDelete.Contains value.stopTime.tripId))

/// Finalize for production sinks without expanding schedules once per trip.
let finishStreamingFeedWithUniqueCalendars preparation (referencedStopIds: Set<string>) =
    let services, calendar, exceptions = materializeUniqueCalendars preparation.calendarPreparation
    let feed = assembleGtfsFeed
                   preparation.batch preparation.postPlan preparation.tripsToDelete
                   calendar exceptions preparation.publicLineNumbers referencedStopIds [||]
                   (if preparation.tripEndpoints.Count <> preparation.batch.trips.Length then None else Some preparation.tripEndpoints)
    { feed with trips = feed.trips |> Array.map (fun trip -> { trip with serviceId = services.[trip.serviceId] }) }

// Bundle sidecars retain otherwise-unhandled textual service notes, so the
// standalone conversion warning would be misleading while building a bundle.
