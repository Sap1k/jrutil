// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// JDF-to-GTFS identifiers, reviewed rules and route presentation.
module JrUtil.JdfGtfsRules

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

type InternationalRoutePolicy =
    | KeepAll
    | RegionalAdjacent

type InternationalRouteDecision = {
    routeId: string
    routeDistinction: int
    keep: bool
    reason: string
    countries: string array
    maximumTripSpanKm: decimal option
    maximumForeignDepthKm: decimal option
    integrated: bool
    retainedDomesticTrips: int
    qualifyingCrossBorderTrips: int
    rejectedCrossBorderTrips: int
    foreignOnlyTrips: int
}

type InternationalRouteFilterResult = {
    batch: JdfModel.JdfBatch
    decisions: InternationalRouteDecision array
}

type TransportModeRule = {
    agencyId: string
    routeIdFrom: int
    routeIdTo: int
    publicLineFrom: int
    publicLineTo: int
    expectedMode: JdfModel.TransportMode
    effectiveMode: JdfModel.TransportMode
    reason: string
}

type TransportModeRuleSet = {
    sha256: string option
    rules: TransportModeRule array
}

type TransportModeDecision = {
    routeId: string
    routeDistinction: int
    corrected: bool
    message: string
    effectiveMode: JdfModel.TransportMode
}

let emptyTransportModeRules = { sha256 = None; rules = [||] }

let internal parseTransportMode argument = function
    | "A" -> JdfModel.Bus
    | "E" -> JdfModel.Tram
    | "T" -> JdfModel.Trolleybus
    | "L" -> JdfModel.CableCar
    | "M" -> JdfModel.Metro
    | "P" -> JdfModel.Ferry
    | value -> invalidArg argument $"Unknown JDF transport mode: {value}"

let loadTransportModeRules (path: string) =
    let expected = [| "agency_id"; "route_id_from"; "route_id_to"; "public_line_from";
                      "public_line_to"; "expected_mode"; "effective_mode"; "reason" |]
    let bytes = File.ReadAllBytes(path)
    let csv: CsvFile = CsvFile.Parse(System.Text.Encoding.UTF8.GetString(bytes), hasHeaders = true)
    if csv.Headers <> Some expected then
        let expectedHeader = String.Join(",", expected)
        invalidArg "transportModeRules" $"Transport mode rule CSV must have header {expectedHeader}"
    let integer (field: string) (value: string) =
        match Int32.TryParse(value.Trim()) with
        | true, parsed -> parsed
        | _ -> invalidArg "transportModeRules" $"Invalid {field}: {value}"
    let rules =
        csv.Rows
        |> Seq.map (fun row ->
            let reason = row.[7].Trim()
            if String.IsNullOrWhiteSpace(row.[0]) || String.IsNullOrWhiteSpace(reason) then
                invalidArg "transportModeRules" "Rule agency_id and reason are required"
            let rule = {
                agencyId = row.[0].Trim()
                routeIdFrom = integer "route_id_from" row.[1]
                routeIdTo = integer "route_id_to" row.[2]
                publicLineFrom = integer "public_line_from" row.[3]
                publicLineTo = integer "public_line_to" row.[4]
                expectedMode = parseTransportMode "transportModeRules" (row.[5].Trim())
                effectiveMode = parseTransportMode "transportModeRules" (row.[6].Trim())
                reason = reason }
            if rule.routeIdFrom > rule.routeIdTo || rule.publicLineFrom > rule.publicLineTo then
                invalidArg "transportModeRules" "Rule ranges must be ascending"
            rule)
        |> Seq.toArray
    { sha256 = Some (Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant()); rules = rules }

let internationalRoutePolicyName = function
    | KeepAll -> "keep-all"
    | RegionalAdjacent -> "regional-adjacent"

let parseInternationalRoutePolicy = function
    | null | "" | "keep-all" -> KeepAll
    | "regional-adjacent" -> RegionalAdjacent
    | value -> invalidArg "internationalRoutePolicy" $"Unknown international route policy: {value}"

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

/// JDF stop numbers are local to the export, never global CIS identifiers.
let jdfStopId (id: int64) = sprintf "jdf:stop:%d" id

let jdfUnspecifiedStopId id =
    sprintf "%s:unspecified" (jdfStopId id)

let jdfStopPostId stopId (stopPostId: int64) =
    sprintf "%s:post:id:%d" (jdfStopId stopId) stopPostId

let jdfStopPostNumId stopId (stopPostNum: string) =
    sprintf "%s:post:%s"
            (jdfStopId stopId)
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

let getGtfsRouteType (jdfRoute: JdfModel.Route) =
    match (jdfRoute.transportMode, jdfRoute.routeType) with
    | (JdfModel.Bus, JdfModel.City)
    | (JdfModel.Bus, JdfModel.CityAndAdjacent) -> "704" // Local bus
    | (JdfModel.Bus, JdfModel.International)
    | (JdfModel.Bus, JdfModel.InternationalNoNational) // International coach
    | (JdfModel.Bus, JdfModel.InternationalOrNational) -> "201"
    | (JdfModel.Bus, JdfModel.ExtraDistrict)
    | (JdfModel.Bus, JdfModel.Regional)
    | (JdfModel.Bus, JdfModel.ExtraRegional)
    | (JdfModel.Bus, JdfModel.RegionalInternational) -> "701" // Regional bus
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
    | JdfModel.Bus ->
        match jdfRoute.routeType with
        | JdfModel.International
        | JdfModel.InternationalNoNational
        | JdfModel.InternationalOrNational
        | JdfModel.LongDistanceNational -> colorsWithWhiteText "004f71"
        | JdfModel.RegionalInternational -> colorsWithWhiteText "00695c"
        | _ -> colorsWithWhiteText "0076a3"
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

let nonEmptyTrimmed (value: string) =
    if String.IsNullOrWhiteSpace(value) then None
    else Some (value.Trim())

let normalizeNumericDesignation (value: string) =
    let withoutZeros = value.TrimStart('0')
    if withoutZeros = "" then "0" else withoutZeros

let internal computePublicLineNumbers (jdfBatch: JdfModel.JdfBatch) =
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

let internal publicLineNumberCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<JdfModel.JdfBatch, Map<string * int, string option>>()

/// Public line numbers per (licence, distinction), computed once per batch.
let getPublicLineNumbers (jdfBatch: JdfModel.JdfBatch) =
    publicLineNumberCache.GetValue(jdfBatch, fun batch -> computePublicLineNumbers batch)

/// route_desc of detour (výluka) routes; they keep the line's own colours.
let detourRouteDescription = "Výlukový jízdní řád"

/// Output GTFS routes group all merged versions of a CIS line that share route
/// semantics, keeping detour timetables apart. The group holding the line's
/// earliest-starting version (after merge-jdf bounding, the one in force) gets the
/// plain id; other groups get a suffix hashed from their semantics.
type RouteGrouping = {
    routeIds: IReadOnlyDictionary<struct (string * int), string>
    /// Output route id and the representative version, one per group
    groups: (string * JdfModel.Route) array
}

let internal routeGroupingCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<JdfModel.JdfBatch, RouteGrouping>()

let internal buildRouteGrouping (jdfBatch: JdfModel.JdfBatch) =
    let publicLineNumbers = getPublicLineNumbers jdfBatch
    let semanticKey (route: JdfModel.Route) =
        let publicLine = publicLineNumbers.[route.id, route.idDistinction]
        let color, textColor = getGtfsRouteColors publicLine route
        String.Join("", [|
            route.id; jdfAgencyId route.agencyId route.agencyDistinction
            defaultArg publicLine ""; route.name; getGtfsRouteType route
            defaultArg color ""; defaultArg textColor ""; string route.detour |])
    let routeIds = Dictionary<struct (string * int), string>()
    let groups = ResizeArray<string * JdfModel.Route>()
    for licence, versions in jdfBatch.routes |> Array.groupBy (fun route -> route.id) |> Array.sortBy fst do
        let baseId = sprintf "jdf:route:%s" (Uri.EscapeDataString(licence))
        for detour in [ false; true ] do
            let kindGroups =
                versions
                |> Array.filter (fun route -> route.detour = detour)
                |> Array.groupBy semanticKey
                |> Array.map (fun (key, members) ->
                    key, members |> Array.sortBy (fun route -> route.timetableValidFrom, route.idDistinction))
                |> Array.sortBy (fun (key, members) -> members.[0].timetableValidFrom, key)
            kindGroups
            |> Array.iteri (fun index (key, members) ->
                let kindId = if detour then baseId + ":detour" else baseId
                let routeId =
                    if index = 0 then kindId
                    else
                        let hash = SHA256.HashData(Encoding.UTF8.GetBytes(key))
                        kindId + ":" + Convert.ToHexString(hash, 0, 4).ToLowerInvariant()
                for route in members do routeIds.[struct (route.id, route.idDistinction)] <- routeId
                groups.Add((routeId, members.[0])))
    { routeIds = routeIds; groups = groups.ToArray() }

let getRouteGrouping (jdfBatch: JdfModel.JdfBatch) =
    routeGroupingCache.GetValue(jdfBatch, fun batch -> buildRouteGrouping batch)

/// Output GTFS route id of one merged route version.
let gtfsRouteId (jdfBatch: JdfModel.JdfBatch) (routeId: string) routeDistinction =
    (getRouteGrouping jdfBatch).routeIds.[struct (routeId, routeDistinction)]

/// Trip ids name the published schedule rather than the merge-assigned
/// distinction: `<licence>:<yymmdd>[:det][:<hash>][:pN]`. The date is the
/// version's original validity start. Versions of one licence sharing it get
/// a hash of their original validity and batch date, except the one ending
/// first; chunks the merger split from one version get `:p2`, `:p3`… by date.
let internal tripVersionKeyCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<JdfModel.JdfBatch, IReadOnlyDictionary<struct (string * int), string>>()

let internal buildTripVersionKeys (jdfBatch: JdfModel.JdfBatch) =
    let versions =
        jdfBatch.routeVersions
        |> Array.map (fun value -> struct (value.routeId, value.routeDistinction), value)
        |> dict
    let sourceOf (route: JdfModel.Route) =
        match versions.TryGetValue(struct (route.id, route.idDistinction)) with
        | true, value -> value.originalValidFrom, value.originalValidTo, value.sourceCreationDate
        | _ -> route.timetableValidFrom, route.timetableValidTo, jdfBatch.version.creationDate
    let dateText (date: LocalDate) =
        date.ToString("yyMMdd", Globalization.CultureInfo.InvariantCulture)
    let keys = Dictionary<struct (string * int), string>()
    for licence, routes in jdfBatch.routes |> Array.groupBy (fun route -> route.id) do
        let seen = HashSet<string>(StringComparer.Ordinal)
        for (validFrom, detour), starting in
                routes |> Array.groupBy (fun route -> (let from, _, _ = sourceOf route in from), route.detour) do
            let baseKey =
                Uri.EscapeDataString(licence) + ":" + dateText validFrom + (if detour then ":det" else "")
            starting
            |> Array.groupBy sourceOf
            |> Array.sortBy (fun ((_, validTo, created), _) -> validTo, created)
            |> Array.iteri (fun sourceIndex ((_, validTo, created), chunks) ->
                let sourceKey =
                    if sourceIndex = 0 then baseKey
                    else
                        let createdText = created |> Option.map dateText |> Option.defaultValue ""
                        let payload = $"{dateText validFrom}|{dateText validTo}|{createdText}"
                        let hash = SHA256.HashData(Encoding.UTF8.GetBytes(payload))
                        baseKey + ":" + Convert.ToHexString(hash, 0, 4).ToLowerInvariant()
                chunks
                |> Array.sortBy (fun route -> route.timetableValidFrom, route.idDistinction)
                |> Array.iteri (fun part route ->
                    let key = if part = 0 then sourceKey else $"{sourceKey}:p{part + 1}"
                    if not (seen.Add(key)) then
                        invalidOp $"JDF route {route.id}/{route.idDistinction} repeats trip version key {key}"
                    keys.[struct (route.id, route.idDistinction)] <- key))
    keys :> IReadOnlyDictionary<_, _>

/// Stable trip-version key of each merged route version, computed once per batch.
let getTripVersionKeys (jdfBatch: JdfModel.JdfBatch) =
    tripVersionKeyCache.GetValue(jdfBatch, fun batch -> buildTripVersionKeys batch)

/// A batch derived by dropping route versions keeps its parent's keys, so trip
/// ids prepared on the parent stay valid for the derived batch.
let inheritTripVersionKeys (parent: JdfModel.JdfBatch) (derived: JdfModel.JdfBatch) =
    tripVersionKeyCache.AddOrUpdate(derived, getTripVersionKeys parent)

let jdfTripId (jdfBatch: JdfModel.JdfBatch) (routeId: string) routeDistinction (id: int64) =
    sprintf "jdf:trip:%s:%d" (getTripVersionKeys jdfBatch).[struct (routeId, routeDistinction)] id

let applyTransportModeRules (ruleSet: TransportModeRuleSet) (batch: JdfModel.JdfBatch) =
    let publicLines = getPublicLineNumbers batch
    let decisions = ResizeArray<TransportModeDecision>()
    let routes =
        batch.routes
        |> Array.map (fun route ->
            let numericRoute = match Int32.TryParse(route.id) with true, value -> Some value | _ -> None
            let publicLine =
                publicLines.[route.id, route.idDistinction]
                |> Option.bind (fun value -> match Int32.TryParse(value) with true, parsed -> Some parsed | _ -> None)
            let candidates =
                ruleSet.rules
                |> Array.filter (fun rule ->
                    rule.agencyId = route.agencyId
                    && numericRoute |> Option.exists (fun value -> value >= rule.routeIdFrom && value <= rule.routeIdTo))
            let matched =
                candidates
                |> Array.tryFind (fun rule ->
                    route.transportMode = rule.expectedMode
                    && publicLine |> Option.exists (fun value -> value >= rule.publicLineFrom && value <= rule.publicLineTo))
            match matched with
            | Some rule ->
                decisions.Add {
                    routeId = route.id; routeDistinction = route.idDistinction; corrected = true
                    message = rule.reason; effectiveMode = rule.effectiveMode }
                { route with transportMode = rule.effectiveMode }
            | None when candidates.Length > 0 ->
                decisions.Add {
                    routeId = route.id; routeDistinction = route.idDistinction; corrected = false
                    message = "reviewed rule guard mismatch"; effectiveMode = route.transportMode }
                route
            | None -> route)
    { batch with routes = routes }, decisions.ToArray()

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
