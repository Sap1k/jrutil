// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfToGtfs

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open FSharp.Data
open NodaTime
open NodaTime.Calendars
open Serilog

open JrUtil
open JrUtil.Holidays

type InternationalRoutePolicy =
    | KeepAll
    | RegionalAdjacent

type InternationalRouteOverrideDecision =
    | KeepRoute
    | DropRoute

type InternationalRouteOverride = {
    routeId: string
    routeDistinction: int
    decision: InternationalRouteOverrideDecision
    reason: string
}

type InternationalRouteDecision = {
    routeId: string
    routeDistinction: int
    keep: bool
    reason: string
    countries: string array
    maximumTripSpanKm: decimal option
    maximumForeignDepthKm: decimal option
    integrated: bool
    overrideDecision: InternationalRouteOverrideDecision option
}

type InternationalRouteFilterResult = {
    batch: JdfModel.JdfBatch
    decisions: InternationalRouteDecision array
}

let internationalRoutePolicyName = function
    | KeepAll -> "keep-all"
    | RegionalAdjacent -> "regional-adjacent"

let parseInternationalRoutePolicy = function
    | null | "" | "keep-all" -> KeepAll
    | "regional-adjacent" -> RegionalAdjacent
    | value -> invalidArg "internationalRoutePolicy" $"Unknown international route policy: {value}"

let loadInternationalRouteOverrides (path: string) =
    let expected = [| "route_id"; "route_distinction"; "decision"; "reason" |]
    let csv: CsvFile = CsvFile.Parse(File.ReadAllText(path), hasHeaders = true)
    if csv.Headers <> Some expected then
        let expectedHeader = String.Join(",", expected)
        invalidArg "internationalRouteOverrides"
            $"International route override CSV must have header {expectedHeader}"
    let parsed =
        csv.Rows
        |> Seq.map (fun (row: CsvRow) ->
            let routeId = row.[0].Trim()
            let distinction =
                match Int32.TryParse(row.[1].Trim()) with
                | true, value -> value
                | _ -> invalidArg "internationalRouteOverrides"
                           $"Invalid route_distinction {row.[1]} for route {routeId}"
            let decision =
                match row.[2].Trim().ToLowerInvariant() with
                | "keep" -> KeepRoute
                | "drop" -> DropRoute
                | value -> invalidArg "internationalRouteOverrides"
                               $"Invalid decision {value} for route {routeId}/{distinction}"
            let reason = row.[3].Trim()
            if String.IsNullOrWhiteSpace(routeId) || String.IsNullOrWhiteSpace(reason) then
                invalidArg "internationalRouteOverrides" "Override route_id and reason are required"
            { routeId = routeId; routeDistinction = distinction
              decision = decision; reason = reason })
        |> Seq.toArray
    parsed
    |> Seq.groupBy (fun item -> item.routeId, item.routeDistinction)
    |> Seq.iter (fun ((routeId, distinction), values) ->
        let decisions = values |> Seq.map (fun value -> value.decision) |> Seq.distinct |> Seq.length
        if decisions > 1 then
            invalidArg "internationalRouteOverrides"
                $"Conflicting overrides for route {routeId}/{distinction}")
    parsed
    |> Seq.distinctBy (fun item -> item.routeId, item.routeDistinction, item.decision)
    |> Seq.toArray

// Information that is lost in the conversion:
// Textual notes about routes
// "Označení časového kódu" - attributes stored in ServiceNote
// Details of accessibility attributes
// Transfer attributes
// "Stop exclusivity" attributes (can't go A->C, or C->B, but B->C is fine)
// TripGroups
// And probably even more things. These are just the ones that are likely
// to become a problem.

let jdfAgencyId (id: string) idDistinction =
    sprintf "jdf:agency:%s:%d" (Uri.EscapeDataString(id)) idDistinction

let jdfStopId cis id =
    if cis
    // Stop IDs which are global (from the CIS database)
    then sprintf "cis:stop:%d" id
    // Stop IDs which are local to the file
    else sprintf "jdf:stop:%d" id

let jdfUnspecifiedStopId cis id =
    sprintf "%s:unspecified" (jdfStopId cis id)

let jdfStopPostId cis stopId stopPostId =
    sprintf "%s:post:id:%d" (jdfStopId cis stopId) stopPostId

let jdfStopPostNumId cis stopId (stopPostNum: string) =
    sprintf "%s:post:%s"
            (jdfStopId cis stopId)
            (Uri.EscapeDataString(stopPostNum))

let jdfRouteId (id: string) idDistinction =
    sprintf "jdf:route:%s:%d" (Uri.EscapeDataString(id)) idDistinction

let jdfSourceRouteId (id: string) idDistinction =
    jdfRouteId id idDistinction

let jdfSourceZoneId (routeId: string) routeDistinction (zoneCode: string) =
    sprintf "jdf:zone:%s:%d:%s"
            (Uri.EscapeDataString(routeId))
            routeDistinction
            (Uri.EscapeDataString(zoneCode))

let jdfTripId (routeId: string) routeDistinction id =
    sprintf "jdf:trip:%s:%d:%d"
            (Uri.EscapeDataString(routeId)) routeDistinction id

let getGtfsRouteType (jdfRoute: JdfModel.Route) =
    match (jdfRoute.transportMode, jdfRoute.routeType) with
    | (JdfModel.Bus, JdfModel.City)
    | (JdfModel.Bus, JdfModel.CityAndAdjacent) -> "704" // Local bus
    | (JdfModel.Bus, JdfModel.International)
    | (JdfModel.Bus, JdfModel.InternationalNoNational) // International coach
    | (JdfModel.Bus, JdfModel.InternationalOrNational) -> "201"
    | (JdfModel.Bus, JdfModel.ExtraDistrict)
    | (JdfModel.Bus, JdfModel.Regional) -> "701" // Regional bus
    | (JdfModel.Bus, JdfModel.ExtraRegional)
    | (JdfModel.Bus, JdfModel.LongDistanceNational) -> "202" // National coach
    | (JdfModel.Tram, _) -> "900" // Tram
    | (JdfModel.CableCar, _) -> "1701" // Cable car
    | (JdfModel.Metro, _) -> "401" // Metro (TODO?)
    | (JdfModel.Ferry, _) -> "1000" // Water transport
    | (JdfModel.Trolleybus, _) -> "800" // Trolleybus

let getGtfsRouteColors (publicLineNumber: string option)
                       (jdfRoute: JdfModel.Route) =
    let colors background text = Some background, Some text
    let colorsWithWhiteText background = colors background "ffffff"
    match jdfRoute.transportMode with
    | JdfModel.Bus -> colorsWithWhiteText "0076a3"
    | JdfModel.Tram -> colorsWithWhiteText "7a0200"
    | JdfModel.CableCar -> colors "c8d021" "1c1745"
    | JdfModel.Trolleybus -> colorsWithWhiteText "80166f"
    | JdfModel.Metro ->
        match publicLineNumber |> Option.map (fun value -> value.ToUpperInvariant()) with
        | Some "A" -> colorsWithWhiteText "00b274"
        | Some "B" -> colors "fbaf33" "1c1745"
        | Some "C" -> colorsWithWhiteText "d31245"
        | _ -> colorsWithWhiteText "1c1745"
    | JdfModel.Ferry -> colors "00b3cb" "1c1745"

let getStopName (jdfStop: JdfModel.Stop) =
    // This tries to mimic how IDOS displays these names
    match (jdfStop.district, jdfStop.nearbyPlace) with
    | (None, None) -> jdfStop.town
    | (Some d, None) -> sprintf "%s,%s" jdfStop.town d
    | (None, Some np) -> sprintf "%s,,%s" jdfStop.town np
    | (Some d, Some np) -> sprintf "%s,%s,%s" jdfStop.town d np

let markApproximateStopName (name: string) =
    let suffix = " [?]"
    if name.EndsWith(suffix, StringComparison.Ordinal) then name
    else name + suffix

let nonEmptyTrimmed (value: string) =
    if String.IsNullOrWhiteSpace(value) then None
    else Some (value.Trim())

let normalizeNumericDesignation (value: string) =
    let withoutZeros = value.TrimStart('0')
    if withoutZeros = "" then "0" else withoutZeros

let getPublicLineNumbers (jdfBatch: JdfModel.JdfBatch) =
    let integrationsByRoute =
        jdfBatch.routeIntegrations
        |> Seq.groupBy (fun ri -> ri.routeId, ri.routeDistinction)
        |> Map

    jdfBatch.routes
    |> Seq.map (fun route ->
        let key = route.id, route.idDistinction
        let preferred =
            integrationsByRoute
            |> Map.tryFind key
            |> Option.defaultValue Seq.empty
            |> Seq.filter (fun ri -> ri.preferential)
            |> Seq.toArray

        let normalizeExplicit value =
            value
            |> nonEmptyTrimmed
            |> Option.map (fun designation ->
                if Regex.IsMatch(designation, "^[0-9]+$")
                then normalizeNumericDesignation designation
                else designation)

        let publicLineNumber =
            match preferred with
            | [| integration |] ->
                match normalizeExplicit integration.routeName with
                | Some value -> Some value
                | None ->
                    Log.Warning(
                        "JDF route {RouteId}/{RouteDistinction} has an empty preferred LinExt designation",
                        route.id, route.idDistinction)
                    None
            | [||] ->
                if Regex.IsMatch(route.id, "^[0-9]{6}$") then
                    route.id.Substring(3)
                    |> normalizeNumericDesignation
                    |> Some
                else
                    Log.Warning(
                        "JDF route {RouteId}/{RouteDistinction} has no preferred LinExt designation and its CIS line ID is not six digits",
                        route.id, route.idDistinction)
                    None
            | _ ->
                Log.Warning(
                    "JDF route {RouteId}/{RouteDistinction} has {PreferredCount} preferred LinExt designations",
                    route.id, route.idDistinction, preferred.Length)
                None
        key, publicLineNumber)
    |> Map

let derivedStopPosts (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.tripStops
    |> Seq.choose (fun tripStop ->
        match tripStop.stopPostId, tripStop.stopPostNum |> Option.bind nonEmptyTrimmed with
        | None, Some stopPostNum -> Some (tripStop.stopId, stopPostNum)
        | _ -> None)
    |> Seq.distinct
    |> Seq.toArray

let stopPostNumbersById (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.tripStops
    |> Seq.choose (fun tripStop ->
        match tripStop.stopPostId, tripStop.stopPostNum |> Option.bind nonEmptyTrimmed with
        | Some stopPostId, Some stopPostNum ->
            Some ((tripStop.stopId, stopPostId), stopPostNum)
        | _ -> None)
    |> Seq.groupBy fst
    |> Seq.choose (fun (key, values) ->
        let numbers = values |> Seq.map snd |> Seq.distinct |> Seq.toArray
        match numbers with
        | [| number |] -> Some (key, number)
        | [||] -> None
        | _ ->
            let stopId, stopPostId = key
            Log.Warning(
                "JDF stop post {StopId}/{StopPostId} has conflicting station numbers {StationNumbers}",
                stopId, stopPostId, String.Join(",", numbers))
            None)
    |> Map

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

let getGtfsStops stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
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
                    markApproximateStopName name
                | _ -> name
            {
                id = jdfStopId stopIdsCis jdfStop.id
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
            let parentStop = gtfsStopsById.[jdfStopId stopIdsCis jdfStop.id]
            { parentStop with
                id = jdfUnspecifiedStopId stopIdsCis jdfStop.id
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
            }: GtfsModel.Stop)
    let postNumbersById = stopPostNumbersById jdfBatch
    let gtfsStopPosts =
        jdfBatch.stopPosts |> Array.map (fun jdfStopPost ->
            let parentStop = gtfsStopsById.[jdfStopId stopIdsCis jdfStopPost.stopId]
            { parentStop with

                id = jdfStopPostId stopIdsCis
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
            }: GtfsModel.Stop)
    let gtfsNumberedStopPosts =
        derivedStopPosts jdfBatch
        |> Array.map (fun (stopId, stopPostNum) ->
            let parentStop = gtfsStopsById.[jdfStopId stopIdsCis stopId]
            { parentStop with
                id = jdfStopPostNumId stopIdsCis stopId stopPostNum
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
                platformCode = Some stopPostNum
            }: GtfsModel.Stop)

    Array.concat [ gtfsStops; gtfsUnspecifiedStops
                   gtfsStopPosts; gtfsNumberedStopPosts ]

let getGtfsRoutesWithPublicLines
        (publicLineNumbers: Map<string * int, string option>)
                                (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.routes
    |> Array.map (fun jdfRoute ->
    let publicLineNumber =
        publicLineNumbers.[jdfRoute.id, jdfRoute.idDistinction]
    let routeColor, routeTextColor =
        getGtfsRouteColors publicLineNumber jdfRoute
    {
        id = jdfRouteId jdfRoute.id jdfRoute.idDistinction
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


/// Returns a boolean array, where each item represents a day in the route's
/// validity interval, true if the trip should run, false otherwise.
let tripDateBitmap (route: JdfModel.Route)
                   (trip: JdfModel.Trip)
                   (tripServiceNotes: JdfModel.ServiceNote seq)
                   (tripAttributes: JdfModel.Attribute Set) =
    let jdfNotes =
        tripServiceNotes
        // Pre-process for ease of use
        |> Seq.choose (fun sn ->
            sn.noteType |> Option.map (fun nt ->
                let df = sn.dateFrom
                         |> Option.defaultValue route.timetableValidFrom
                {|
                    noteType = nt
                    dateFrom = df
                    dateTo = sn.dateTo |> Option.defaultValue df
                |}))
        |> Seq.toArray
    let holidays =
        czechHolidayDates route.timetableValidFrom route.timetableValidTo
        |> set
    let hasDateAttribute =
        tripAttributes |> Seq.exists (fun a ->
            match a with
            | JdfModel.WeekdayService
            | JdfModel.HolidaySundayService
            | JdfModel.DayOfWeekService _ -> true
            | _ -> false)
    let hasServiceOnlyNote =
        jdfNotes |> Seq.exists (fun n -> n.noteType = JdfModel.ServiceOnly)
    let hasServiceNote =
        jdfNotes |> Seq.exists (fun n -> n.noteType = JdfModel.Service)
    Utils.dateRange route.timetableValidFrom route.timetableValidTo
    |> Seq.map (fun d ->
        let applicableNoteTypes =
            jdfNotes
            |> Seq.filter (fun sn ->
                DateInterval(sn.dateFrom, sn.dateTo).Contains(d))
            |> Seq.map (fun sn -> sn.noteType)
            |> set
        let hasNote noteType = applicableNoteTypes |> Set.contains noteType
        if hasNote JdfModel.ServiceOnly then true
        else if hasServiceOnlyNote then false
        else if hasNote JdfModel.NoService then false
        else if hasNote JdfModel.ServiceAlso then true
        else if (hasNote JdfModel.ServiceOddWeeks
                 || hasNote JdfModel.ServiceOddWeeksFromTo)
             && WeekYearRules.Iso.GetWeekOfWeekYear(d) % 2 = 0 then false
        else if (hasNote JdfModel.ServiceEvenWeeks
                 || hasNote JdfModel.ServiceEvenWeeksFromTo)
             && WeekYearRules.Iso.GetWeekOfWeekYear(d) % 2 = 1 then false
        else if (not <| hasNote JdfModel.Service)
             && hasServiceNote then false
        else if tripAttributes |> Set.contains JdfModel.HolidaySundayService
             && (holidays |> Set.contains d
                 || d.DayOfWeek = IsoDayOfWeek.Sunday) then true
        else if tripAttributes |> Set.contains JdfModel.WeekdayService
             && holidays |> Set.contains d |> not
             && d.DayOfWeek <> IsoDayOfWeek.Saturday
             && d.DayOfWeek <> IsoDayOfWeek.Sunday then true
        else if tripAttributes
                |> Set.contains (JdfModel.DayOfWeekService (
                    LanguagePrimitives.EnumToValue d.DayOfWeek)) then true
        else not hasDateAttribute)
    |> Seq.toArray

let gtfsCalendarBitmap (calendar: GtfsModel.CalendarEntry) =
    Utils.dateRange calendar.startDate calendar.endDate
    |> Seq.map (fun d ->
        let dow = LanguagePrimitives.EnumToValue d.DayOfWeek - 1
        calendar.weekdayService.[dow])
    |> Seq.toArray

// Returns triple of trips with empty calendar (to delete), calendar entries
// and calendar exceptions
let getGtfsCalendar (jdfBatch: JdfModel.JdfBatch) =
    let tripsByRoute =
        jdfBatch.trips
        |> Array.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Map

    let notesByTrip =
        jdfBatch.serviceNotes
        |> Array.groupBy (fun sn -> sn.routeId, sn.routeDistinction, sn.tripId)
        |> Map

    let routeById =
        jdfBatch.routes
        |> Array.map (fun r -> (r.id, r.idDistinction), r)
        |> Map

    jdfBatch.trips
    |> Seq.map (fun jdfTrip ->
        let jdfRoute = routeById.[(jdfTrip.routeId, jdfTrip.routeDistinction)]
        let attrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes

        let servicedDays =
            attrs
            |> Set.toList
            |> List.collect (fun a ->
                match a with
                | JdfModel.WeekdayService -> [1; 2; 3; 4; 5]
                | JdfModel.HolidaySundayService -> [7]
                | JdfModel.DayOfWeekService(d) -> [d]
                | _ -> [])
        let weekdays =
            if servicedDays.Length = 0
            then [| for i in [1..7] -> true |]
            else [| for i in [1..7] -> servicedDays
                                       |> List.contains i |]
        let tripId = jdfTripId jdfTrip.routeId
                               jdfTrip.routeDistinction
                               jdfTrip.id

        let calendarEntry: GtfsModel.CalendarEntry = {
            id = tripId
            weekdayService = weekdays
            startDate = jdfRoute.timetableValidFrom
            endDate = jdfRoute.timetableValidTo
        }

        let bitmap =
            tripDateBitmap
                jdfRoute jdfTrip
                (notesByTrip
                 |> Map.tryFind (jdfTrip.routeId,
                                 jdfTrip.routeDistinction,
                                 jdfTrip.id)
                 |> Option.defaultValue [||])
                attrs
        let calendarBitmap = gtfsCalendarBitmap calendarEntry
        let bitmapDiffCount =
            Seq.zip bitmap calendarBitmap
            |> Seq.sumBy (fun (s1, s2) -> if s1 = s2 then 0 else 1)
        let bitmapTrueCount =
            bitmap |> Array.sumBy (fun s -> if s then 1 else 0)

        // Pick the most efficient representation (calendar + exceptions vs
        // just exceptions)
        if bitmapTrueCount = 0 then
            seq { tripId }, Seq.empty, Seq.empty
        elif bitmapDiffCount > bitmapTrueCount then
            Seq.empty, Seq.empty,
            bitmap
            |> Seq.indexed
            |> Seq.choose (fun (i, s) ->
                if s then Some ({
                    id = tripId
                    date = jdfRoute.timetableValidFrom.PlusDays(i)
                    exceptionType = GtfsModel.ServiceAdded
                }: GtfsModel.CalendarException)
                else None)
        else
            Seq.empty, seq { calendarEntry },
            Seq.zip bitmap calendarBitmap
            |> Seq.indexed
            |> Seq.choose (fun (i, (s, sc)) ->
                if s <> sc then Some {
                    id = tripId
                    date = jdfRoute.timetableValidFrom.PlusDays(i)
                    exceptionType = if s then GtfsModel.ServiceAdded
                                    else GtfsModel.ServiceRemoved
                }
                else None)
    )
    |> Utils.concatTo3
    |> (fun (ets, ces, cexs) -> set ets, ces.ToArray(), cexs.ToArray())

let private callIsEmitted (call: JdfModel.TripStop) =
    match call.departureTime with
    | Some JdfModel.Passing | Some JdfModel.NotPassing -> false
    | None when call.arrivalTime = None -> false
    | _ -> true

let private canonicalCountry (value: string) =
    match value.Trim().ToUpperInvariant() with
    | "AT" | "A" -> "A"
    | "DE" | "D" -> "D"
    | normalized -> normalized

let private routeIsDeclaredInternational (route: JdfModel.Route) =
    match route.routeType with
    | JdfModel.International
    | JdfModel.InternationalNoNational
    | JdfModel.InternationalOrNational -> true
    | _ -> false

let private validateInternationalRouteOverrideConflicts
        (overrides: InternationalRouteOverride array) =
    overrides
    |> Seq.groupBy (fun item -> item.routeId, item.routeDistinction)
    |> Seq.iter (fun ((routeId, distinction), values) ->
        if values |> Seq.map (fun value -> value.decision) |> Seq.distinct |> Seq.length > 1 then
            invalidArg "internationalRouteOverrides"
                $"Conflicting overrides for route {routeId}/{distinction}")

let validateInternationalRouteOverrides sourceRouteKeys
                                                (overrides: InternationalRouteOverride array) =
    validateInternationalRouteOverrideConflicts overrides
    overrides
    |> Seq.iter (fun item ->
        if not (sourceRouteKeys |> Set.contains (item.routeId, item.routeDistinction)) then
            invalidArg "internationalRouteOverrides"
                $"Override refers to unknown route {item.routeId}/{item.routeDistinction}")

let applyInternationalRoutePolicy (policy: InternationalRoutePolicy)
                                  (overrides: InternationalRouteOverride array)
                                  (batch: JdfModel.JdfBatch) =
    if policy = KeepAll then { batch = batch; decisions = [||] } else

    validateInternationalRouteOverrideConflicts overrides

    let tripsToDelete, _, _ = getGtfsCalendar batch
    let activeTripKeys =
        batch.trips
        |> Seq.choose (fun trip ->
            let gtfsId = jdfTripId trip.routeId trip.routeDistinction trip.id
            if tripsToDelete.Contains gtfsId then None
            else Some (trip.routeId, trip.routeDistinction, trip.id))
        |> Set
    let stopCountries =
        batch.stops
        |> Seq.map (fun stop ->
            let country =
                match stop.country with
                | Some value when not (String.IsNullOrWhiteSpace(value)) ->
                    Some (canonicalCountry value)
                | _ when stop.regionId.IsSome -> Some "CZ"
                | _ -> None
            stop.id, country)
        |> Map
    let callsByRoute =
        batch.tripStops
        |> Seq.filter (fun call ->
            activeTripKeys.Contains(call.routeId, call.routeDistinction, call.tripId)
            && callIsEmitted call)
        |> Seq.groupBy (fun call -> call.routeId, call.routeDistinction)
        |> Map
    let integratedRoutes =
        batch.routeIntegrations
        |> Seq.map (fun integration -> integration.routeId, integration.routeDistinction)
        |> Set
    let overridesByRoute =
        overrides
        |> Seq.map (fun item -> (item.routeId, item.routeDistinction), item)
        |> Map
    let adjacentCountries = set ["A"; "D"; "PL"; "SK"]

    let classify (route: JdfModel.Route) =
        let routeKey = route.id, route.idDistinction
        let calls = callsByRoute |> Map.tryFind routeKey |> Option.defaultValue Seq.empty |> Seq.toArray
        let countriesWithUnknown =
            calls
            |> Seq.map (fun call -> stopCountries |> Map.tryFind call.stopId |> Option.flatten)
            |> Seq.toArray
        let countries = countriesWithUnknown |> Array.choose id |> Array.distinct |> Array.sort
        let foreignCountries = countries |> Set.ofArray |> Set.remove "CZ"
        let integrated = integratedRoutes.Contains routeKey
        let isPotentiallyInternational =
            not foreignCountries.IsEmpty || routeIsDeclaredInternational route

        let calculated =
            if not isPotentiallyInternational then
                true, "domestic", None, None
            elif foreignCountries.IsEmpty then
                false, "international_metadata_without_foreign_geography", None, None
            elif countriesWithUnknown |> Array.exists Option.isNone then
                false, "unknown_stop_country", None, None
            elif not (Set.isSubset foreignCountries adjacentCountries) then
                false, "non_adjacent_country", None, None
            else
                let internationalTrips =
                    calls
                    |> Seq.groupBy (fun call -> call.tripId)
                    |> Seq.map (fun (_, tripCalls) -> tripCalls |> Seq.toArray)
                    |> Seq.filter (fun tripCalls ->
                        tripCalls
                        |> Seq.exists (fun call -> stopCountries.[call.stopId] <> Some "CZ"))
                    |> Seq.toArray
                let foreignOnly =
                    internationalTrips
                    |> Array.exists (fun tripCalls ->
                        tripCalls
                        |> Array.exists (fun call -> stopCountries.[call.stopId] = Some "CZ")
                        |> not)
                let missingKilometres =
                    internationalTrips
                    |> Array.exists (Array.exists (fun call -> call.kilometer.IsNone))
                if foreignOnly then false, "foreign_only_trip", None, None
                elif missingKilometres || internationalTrips.Length = 0 then
                    false, "missing_timetable_kilometres", None, None
                else
                    let metrics =
                        internationalTrips
                        |> Array.map (fun tripCalls ->
                            let czechKm =
                                tripCalls
                                |> Array.choose (fun call ->
                                    if stopCountries.[call.stopId] = Some "CZ" then call.kilometer else None)
                            let foreignKm =
                                tripCalls
                                |> Array.choose (fun call ->
                                    if stopCountries.[call.stopId] <> Some "CZ" then call.kilometer else None)
                            let allKm = tripCalls |> Array.choose (fun call -> call.kilometer)
                            let span = Array.max allKm - Array.min allKm
                            let depth =
                                foreignKm
                                |> Array.maxBy (fun km -> czechKm |> Array.map (fun czech -> abs (km - czech)) |> Array.min)
                                |> fun km -> czechKm |> Array.map (fun czech -> abs (km - czech)) |> Array.min
                            span, depth)
                    let maximumSpan = metrics |> Array.map fst |> Array.max
                    let maximumDepth = metrics |> Array.map snd |> Array.max
                    let spanLimit, depthLimit =
                        if integrated then 200m, 80m else 120m, 60m
                    if maximumSpan > spanLimit then
                        false, "trip_span_exceeds_limit", Some maximumSpan, Some maximumDepth
                    elif maximumDepth > depthLimit then
                        false, "foreign_depth_exceeds_limit", Some maximumSpan, Some maximumDepth
                    else true, "regional_adjacent", Some maximumSpan, Some maximumDepth

        let calculatedKeep, calculatedReason, maximumSpan, maximumDepth = calculated
        match overridesByRoute |> Map.tryFind routeKey with
        | Some (routeOverride: InternationalRouteOverride) ->
            { routeId = route.id; routeDistinction = route.idDistinction
              keep = routeOverride.decision = KeepRoute
              reason = $"override: {routeOverride.reason}"
              countries = countries; maximumTripSpanKm = maximumSpan
              maximumForeignDepthKm = maximumDepth; integrated = integrated
              overrideDecision = Some routeOverride.decision }
        | None ->
            { routeId = route.id; routeDistinction = route.idDistinction
              keep = calculatedKeep; reason = calculatedReason
              countries = countries; maximumTripSpanKm = maximumSpan
              maximumForeignDepthKm = maximumDepth; integrated = integrated
              overrideDecision = None }

    let decisions = batch.routes |> Array.map classify
    let keptRouteKeys =
        decisions
        |> Seq.filter (fun decision -> decision.keep)
        |> Seq.map (fun decision -> decision.routeId, decision.routeDistinction)
        |> Set
    let routeKept routeId routeDistinction = keptRouteKeys.Contains(routeId, routeDistinction)
    let keptTrips =
        batch.trips
        |> Array.filter (fun trip -> routeKept trip.routeId trip.routeDistinction)
    let keptTripKeys =
        keptTrips
        |> Seq.map (fun trip -> trip.routeId, trip.routeDistinction, trip.id)
        |> Set
    let tripKept routeId routeDistinction tripId =
        keptTripKeys.Contains(routeId, routeDistinction, tripId)
    let routeStops =
        batch.routeStops
        |> Array.filter (fun stop -> routeKept stop.routeId stop.routeDistinction)
    let retainedStopIds = routeStops |> Seq.map (fun stop -> stop.stopId) |> Set
    let agencyAlternations =
        batch.agencyAlternations
        |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
    let retainedAgencies =
        seq {
            yield! batch.routes
                   |> Seq.filter (fun route -> routeKept route.id route.idDistinction)
                   |> Seq.map (fun route -> route.agencyId, route.agencyDistinction)
            yield! agencyAlternations
                   |> Seq.map (fun value -> value.agencyId, value.agencyDistinction)
        }
        |> Set
    let usedTripGroups = keptTrips |> Seq.choose (fun trip -> trip.tripGroupId) |> Set
    let filteredBatch = {
        batch with
            stops = batch.stops |> Array.filter (fun stop -> retainedStopIds.Contains stop.id)
            stopPosts = batch.stopPosts |> Array.filter (fun stop -> retainedStopIds.Contains stop.stopId)
            agencies = batch.agencies |> Array.filter (fun agency -> retainedAgencies.Contains(agency.id, agency.idDistinction))
            routes = batch.routes |> Array.filter (fun route -> routeKept route.id route.idDistinction)
            routeIntegrations = batch.routeIntegrations |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            routeStops = routeStops
            trips = keptTrips
            tripGroups = batch.tripGroups |> Array.filter (fun group -> usedTripGroups.Contains group.id)
            tripStops = batch.tripStops |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            routeInfo = batch.routeInfo |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            serviceNotes = batch.serviceNotes |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            transfers = batch.transfers |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            agencyAlternations = agencyAlternations
            alternateRouteNames = batch.alternateRouteNames |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            reservationOptions = batch.reservationOptions |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            stopLocations = batch.stopLocations |> Array.filter (fun value -> retainedStopIds.Contains value.stopId)
            stopLocationSources = batch.stopLocationSources |> Array.filter (fun value -> retainedStopIds.Contains value.stopId)
    }
    { batch = filteredBatch; decisions = decisions }

let logInternationalRouteDecisions (policy: InternationalRoutePolicy)
                                   (decisions: InternationalRouteDecision array) =
    if policy <> KeepAll then
        let crossBorder = decisions |> Seq.filter (fun value -> value.countries |> Array.exists ((<>) "CZ"))
        let retained = crossBorder |> Seq.filter (fun value -> value.keep) |> Seq.length
        let dropped = crossBorder |> Seq.filter (fun value -> not value.keep) |> Seq.length
        Log.Information(
            "International route policy {Policy}: retained {RetainedRoutes} and dropped {DroppedRoutes} cross-border route distinctions",
            internationalRoutePolicyName policy, retained, dropped)

let getGtfsTrips (jdfBatch: JdfModel.JdfBatch) =
    let lastStopPerTrip = Dictionary<struct (string * int * int64), struct (int64 * int64)>()
    for call in jdfBatch.tripStops do
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
            routeId = jdfRouteId jdfTrip.routeId jdfTrip.routeDistinction
            serviceId = id
            id = id
            headsign =
                let struct (_, stopId) =
                    lastStopPerTrip.[struct (jdfTrip.routeId, jdfTrip.routeDistinction, jdfTrip.id)]
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
                Some (if wheelchairAccessible then "1" else "0")
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

let private getGtfsStopTimesInternal adjacentTripGroups stopIdCis
                                     (jdfBatch: JdfModel.JdfBatch) =
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
    let convertedStopId (call: JdfModel.TripStop) =
        match call.stopPostId, call.stopPostNum |> Option.bind nonEmptyTrimmed with
        | Some stopPostId, _ ->
            let key = struct (call.stopId, stopPostId)
            match stopPostIds.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = jdfStopPostId stopIdCis call.stopId stopPostId
                stopPostIds.[key] <- value
                value
        | None, Some stopPostNumber ->
            let key = struct (call.stopId, stopPostNumber)
            match stopPostNumberIds.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = jdfStopPostNumId stopIdCis call.stopId stopPostNumber
                stopPostNumberIds.[key] <- value
                value
        | None, None ->
            match unspecifiedStopIds.TryGetValue(call.stopId) with
            | true, value -> value
            | _ ->
                let value = jdfUnspecifiedStopId stopIdCis call.stopId
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

    let tripKey (call: JdfModel.TripStop) =
        call.routeId, call.routeDistinction, call.tripId
    let tripStopGroups =
        if adjacentTripGroups then
            jdfBatch.tripStops |> Utils.groupAdjacentBy tripKey
        else
            jdfBatch.tripStops
            |> Seq.groupBy tripKey
            |> Seq.map (fun (key, calls) -> key, calls |> Seq.toArray)

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

        let mutable lastTimeDT: LocalTime option = None
        let mutable dayOffsetSeconds = 0L

        jdfTripStops
        |> Seq.sortBy (fun ts ->
            ts.routeStopId * (if isReverseTrip then -1L else 1L))
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
                    stopId = convertedStopId jdfTripStop
                    stopSequence = i
                    headsign = None
                    pickupType =
                        if hasFlag StopTimeAttributeFlags.ExitOnlyFlag
                        then noService
                        else service
                    dropoffType =
                        if hasFlag StopTimeAttributeFlags.BoardingOnlyFlag
                        then noService
                        else service
                    shapeDistTraveled = jdfTripStop.kilometer
                    // This will be dynamic when support for JDF's
                    // min/max times comes.
                    timepoint = exactTimepoint
                    stopZoneIds = None
                }
                Some stopTime
        )
        |> Seq.choose id
    )

// Standalone conversion preserves support for unusual JDF files whose trip
// calls are not contiguous. The merged national bundle has canonical adjacent
// trip groups and uses the bounded implementation directly.
let getGtfsStopTimes stopIdCis (jdfBatch: JdfModel.JdfBatch) =
    getGtfsStopTimesInternal false stopIdCis jdfBatch

let getCzRoutes (publicLineNumbers: Map<string * int, string option>)
                (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.routes
    |> Array.map (fun route ->
        {
            routeId = jdfRouteId route.id route.idDistinction
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

let getCzStops stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    let cisStopId stopId = if stopIdsCis then Some stopId else None
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
            let stopId = jdfStopId stopIdsCis stop.id
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
                stopId = jdfUnspecifiedStopId stopIdsCis stop.id
                stopPlaceId = jdfStopId stopIdsCis stop.id
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
                stopId = jdfStopPostId stopIdsCis
                                           stopPost.stopId
                                           stopPost.stopPostId
                stopPlaceId = jdfStopId stopIdsCis stopPost.stopId
                cisStopId = cisStopId stopPost.stopId
                postId = Some (string stopPost.stopPostId)
                aswId = None
                sourceIds = Some sourceIds
            }: GtfsModel.CzStop)
    let numberedStopPosts =
        derivedStopPosts jdfBatch
        |> Array.map (fun (stopId, stopPostNum) ->
            {
                stopId = jdfStopPostNumId stopIdsCis stopId stopPostNum
                stopPlaceId = jdfStopId stopIdsCis stopId
                cisStopId = cisStopId stopId
                postId = Some stopPostNum
                aswId = None
                sourceIds = Some (sourcePostNum stopId stopPostNum)
            }: GtfsModel.CzStop)
    Array.concat [stops; unspecifiedStops; stopPosts; numberedStopPosts]

let getCzStopZones stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.routeStops
    |> Seq.collect (fun routeStop ->
        Jdf.normalizeZoneTokens [routeStop.zone]
        |> Seq.map (fun zoneCode ->
            {
                stopPlaceId = jdfStopId stopIdsCis routeStop.stopId
                zoneId = jdfSourceZoneId routeStop.routeId
                                             routeStop.routeDistinction
                                             zoneCode
                zoneCode = zoneCode
                routeId = jdfRouteId routeStop.routeId
                                     routeStop.routeDistinction
                idsSystemId = None
                sourceProvenance = sprintf "jdf:%s" jdfBatch.version.version
            }: GtfsModel.CzStopZone))
    |> Seq.distinct
    |> Seq.sortBy (fun zone -> zone.stopPlaceId, zone.routeId, zone.zoneId)
    |> Seq.toArray

let warnUnhandledServiceNotes (jdfBatch: JdfModel.JdfBatch) () =
    jdfBatch.serviceNotes
    |> Seq.filter (fun sn -> sn.noteType = None)
    |> Seq.iter (fun sn ->
        Log.Warning("Unhandled JDF ServiceNote {Designation} {Note}", sn.designation, sn.note))

let private assembleGtfsFeed stopIdsCis (jdfBatch: JdfModel.JdfBatch)
                             tripsToDelete calendar calendarExceptions publicLineNumbers
                             (referencedStopIds: Set<string>) stopTimes =
    let allStops = getGtfsStops stopIdsCis jdfBatch
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
        trips = getGtfsTrips jdfBatch
            |> Seq.filter (fun t ->
                tripsToDelete |> Set.contains t.id |> not)
            |> Seq.toArray
        stopTimes = stopTimes
        calendar = Some calendar
        calendarExceptions = Some calendarExceptions
        feedInfo = None
        czRoutes = Some (getCzRoutes publicLineNumbers jdfBatch)
        czTrips = Some (getCzTrips tripsToDelete jdfBatch)
        czStops =
            getCzStops stopIdsCis jdfBatch
            |> Array.filter (fun stop -> retainedStopIds.Contains stop.stopId)
            |> Some
        czStopZones =
            getCzStopZones stopIdsCis jdfBatch
            |> Array.filter (fun zone -> retainedStopIds.Contains zone.stopPlaceId)
            |> Some
    }
    feed

// Some JDF feeds have only local IDs for stops, some have global IDs for the
// whole CIS. Set stopIdsCis accordingly.
let private getGtfsFeedInternal warnUnhandledNotes adjacentTripGroups stopIdsCis
                                (jdfBatch: JdfModel.JdfBatch) =
    if warnUnhandledNotes then warnUnhandledServiceNotes jdfBatch ()

    let tripsToDelete, calendar, calendarExceptions = getGtfsCalendar jdfBatch
    let publicLineNumbers = getPublicLineNumbers jdfBatch
    let stopTimes =
        getGtfsStopTimesInternal adjacentTripGroups stopIdsCis jdfBatch
        |> Seq.filter (fun ts ->
            tripsToDelete |> Set.contains ts.tripId |> not)
        |> Seq.toArray
    let referencedStopIds = stopTimes |> Seq.map (fun stopTime -> stopTime.stopId) |> Set
    assembleGtfsFeed stopIdsCis jdfBatch tripsToDelete calendar calendarExceptions
                     publicLineNumbers referencedStopIds stopTimes

type internal BundleFeedPreparation = {
    stopIdsCis: bool
    batch: JdfModel.JdfBatch
    tripsToDelete: Set<string>
    calendar: GtfsModel.CalendarEntry array
    calendarExceptions: GtfsModel.CalendarException array
    publicLineNumbers: Map<string * int, string option>
}

let internal prepareGtfsFeedForStreamingBundle stopIdsCis (batch: JdfModel.JdfBatch) =
    let tripsToDelete, calendar, calendarExceptions = getGtfsCalendar batch
    {
        stopIdsCis = stopIdsCis
        batch = batch
        tripsToDelete = tripsToDelete
        calendar = calendar
        calendarExceptions = calendarExceptions
        publicLineNumbers = getPublicLineNumbers batch
    }

let internal getStreamingBundleStopTimes preparation =
    getGtfsStopTimesInternal true preparation.stopIdsCis preparation.batch
    |> Seq.filter (fun stopTime ->
        not (preparation.tripsToDelete.Contains stopTime.tripId))

let internal finishStreamingBundleFeed preparation (referencedStopIds: Set<string>) =
    assembleGtfsFeed
        preparation.stopIdsCis preparation.batch preparation.tripsToDelete
        preparation.calendar preparation.calendarExceptions preparation.publicLineNumbers
        referencedStopIds [||]

let getGtfsFeed stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    getGtfsFeedInternal true false stopIdsCis jdfBatch

// Bundle sidecars retain otherwise-unhandled textual service notes, so the
// standalone conversion warning would be misleading while building a bundle.
let getGtfsFeedForBundle stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    getGtfsFeedInternal false true stopIdsCis jdfBatch
