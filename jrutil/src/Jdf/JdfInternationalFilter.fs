// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// International route and trip filtering policy.
module JrUtil.JdfInternationalFilter

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

let internal callIsEmitted (call: JdfModel.TripStop) =
    match call.departureTime with
    | Some JdfModel.Passing | Some JdfModel.NotPassing -> false
    | None when call.arrivalTime = None -> false
    | _ -> true

let internal canonicalCountry (value: string) =
    match value.Trim().ToUpperInvariant() with
    | "AT" | "A" -> "A"
    | "DE" | "D" -> "D"
    | normalized -> normalized

let internal routeIsDeclaredInternational (route: JdfModel.Route) =
    match route.routeType with
    | JdfModel.International
    | JdfModel.InternationalNoNational
    | JdfModel.InternationalOrNational -> true
    | _ -> false

type internal InternationalTripDisposition =
    | DomesticTrip
    | QualifyingCrossBorderTrip of span: decimal * depth: decimal
    | RejectedCrossBorderTrip of reason: string * span: decimal option * depth: decimal option
    | ForeignOnlyTrip

let internal applyInternationalRoutePolicyInternal
        maximumWorkers
        (progress: string -> int64 -> int64 option -> unit)
        (tripsToDelete: Set<string>)
        (policy: InternationalRoutePolicy)
        (batch: JdfModel.JdfBatch) =
    if policy = KeepAll then { batch = batch; decisions = [||] } else

    let activeTripKeys = HashSet<struct (string * int * int64)>()
    for trip in batch.trips do
        let gtfsId = jdfTripId batch trip.routeId trip.routeDistinction trip.id
        if not (tripsToDelete.Contains gtfsId) then
            activeTripKeys.Add(struct (trip.routeId, trip.routeDistinction, trip.id)) |> ignore
    let activeTripContains routeId distinction tripId =
        activeTripKeys.Contains(struct (routeId, distinction, tripId))
    let activeTripCountsByRoute =
        activeTripKeys
        |> Seq.map (fun struct (routeId, distinction, _) -> routeId, distinction)
        |> Seq.countBy id
        |> Map
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
    // Only these routes need expensive trip-call country classification.
    // Unknown stop countries deliberately remain candidates for inspection.
    let potentiallyInternationalRoutes =
        let values = HashSet<struct (string * int)>()
        for route in batch.routes do
            if routeIsDeclaredInternational route then
                values.Add(struct (route.id, route.idDistinction)) |> ignore
        for routeStop in batch.routeStops do
            if stopCountries |> Map.tryFind routeStop.stopId |> Option.flatten <> Some "CZ" then
                values.Add(struct (routeStop.routeId, routeStop.routeDistinction)) |> ignore
        values
    let isPotentialRoute routeId distinction =
        potentiallyInternationalRoutes.Contains(struct (routeId, distinction))
    let activeTripsByRoute =
        activeTripKeys
        |> Seq.choose (fun struct (routeId, distinction, tripId) ->
            if isPotentialRoute routeId distinction then
                Some ((routeId, distinction), (routeId, distinction, tripId))
            else None)
        |> Seq.groupBy fst
        |> Seq.map (fun (routeKey, trips) -> routeKey, trips |> Seq.map snd |> Seq.toArray)
        |> Map
    let callsByTrip = Dictionary<struct (string * int * int64), ResizeArray<JdfModel.TripStop>>()
    for callIndex = 0 to batch.tripStops.Count-1 do
        let call = batch.tripStops.[callIndex]
        if isPotentialRoute call.routeId call.routeDistinction
           && activeTripContains call.routeId call.routeDistinction call.tripId
           && callIsEmitted call then
            let key = struct (call.routeId, call.routeDistinction, call.tripId)
            match callsByTrip.TryGetValue(key) with
            | true, values -> values.Add(call)
            | _ ->
                let values = ResizeArray()
                values.Add(call)
                callsByTrip.[key] <- values
        if (callIndex + 1) % 1_000_000 = 0 then
            progress "international-calls" (int64 (callIndex + 1)) (Some (int64 batch.tripStops.Count))
    progress "international-calls" (int64 batch.tripStops.Count) (Some (int64 batch.tripStops.Count))
    let callsForTrip (routeId, distinction, tripId) =
        match callsByTrip.TryGetValue(struct (routeId, distinction, tripId)) with
        | true, values -> values.ToArray()
        | _ -> [||]
    let integratedRoutes =
        batch.routeIntegrations
        |> Seq.map (fun integration -> integration.routeId, integration.routeDistinction)
        |> Set
    let adjacentCountries = set ["A"; "D"; "PL"; "SK"]

    let classifyTrip integrated (calls: JdfModel.TripStop array) =
        let countriesWithUnknown =
            calls
            |> Array.map (fun call -> stopCountries |> Map.tryFind call.stopId |> Option.flatten)
        let countries = countriesWithUnknown |> Array.choose id |> Set.ofArray
        let hasCzech = countries.Contains "CZ"
        let foreign = countries.Remove "CZ"
        if foreign.IsEmpty then
            if hasCzech && not (countriesWithUnknown |> Array.exists Option.isNone) then DomesticTrip
            else RejectedCrossBorderTrip("unknown_stop_country", None, None)
        elif not hasCzech then ForeignOnlyTrip
        elif countriesWithUnknown |> Array.exists Option.isNone then
            RejectedCrossBorderTrip("unknown_stop_country", None, None)
        elif not (Set.isSubset foreign adjacentCountries) then
            RejectedCrossBorderTrip("non_adjacent_country", None, None)
        elif calls |> Array.exists (fun call -> call.kilometer.IsNone) then
            RejectedCrossBorderTrip("missing_timetable_kilometres", None, None)
        else
            let czechKm =
                calls
                |> Array.choose (fun call ->
                    if stopCountries.[call.stopId] = Some "CZ" then call.kilometer else None)
            let foreignKm =
                calls
                |> Array.choose (fun call ->
                    if stopCountries.[call.stopId] <> Some "CZ" then call.kilometer else None)
            let allKm = calls |> Array.choose (fun call -> call.kilometer)
            let span = Array.max allKm - Array.min allKm
            let depth =
                foreignKm
                |> Array.map (fun km -> czechKm |> Array.map (fun czech -> abs (km - czech)) |> Array.min)
                |> Array.max
            let spanLimit, depthLimit = if integrated then 200m, 80m else 120m, 60m
            if span > spanLimit then
                RejectedCrossBorderTrip("trip_span_exceeds_limit", Some span, Some depth)
            elif depth > depthLimit then
                RejectedCrossBorderTrip("foreign_depth_exceeds_limit", Some span, Some depth)
            else QualifyingCrossBorderTrip(span, depth)

    let classify (route: JdfModel.Route) =
        let routeKey = route.id, route.idDistinction
        let routeNeedsClassification = isPotentialRoute route.id route.idDistinction
        let routeTrips =
            activeTripsByRoute
            |> Map.tryFind routeKey
            |> Option.defaultValue [||]
            |> Seq.map (fun key -> key, classifyTrip (integratedRoutes.Contains routeKey)
                                            (callsForTrip key))
            |> Seq.toArray
        let calls = routeTrips |> Seq.collect (fun (key, _) -> callsForTrip key) |> Seq.toArray
        let countriesWithUnknown =
            calls
            |> Seq.map (fun call -> stopCountries |> Map.tryFind call.stopId |> Option.flatten)
            |> Seq.toArray
        let countries = countriesWithUnknown |> Array.choose id |> Array.distinct |> Array.sort
        let foreignCountries = countries |> Set.ofArray |> Set.remove "CZ"
        let integrated = integratedRoutes.Contains routeKey
        let isPotentiallyInternational =
            routeNeedsClassification
            && (not foreignCountries.IsEmpty || routeIsDeclaredInternational route)
        let domesticCount =
            if routeNeedsClassification then
                routeTrips |> Array.sumBy (fun (_, value) -> if value = DomesticTrip then 1 else 0)
            else activeTripCountsByRoute |> Map.tryFind routeKey |> Option.defaultValue 0
        let qualifying = routeTrips |> Array.choose (fun (_, value) -> match value with QualifyingCrossBorderTrip(span, depth) -> Some(span, depth) | _ -> None)
        let rejectedCount = routeTrips |> Array.sumBy (fun (_, value) -> match value with RejectedCrossBorderTrip _ -> 1 | _ -> 0)
        let rejectionReasons =
            routeTrips
            |> Array.choose (fun (_, value) -> match value with RejectedCrossBorderTrip(reason, _, _) -> Some reason | _ -> None)
            |> Array.distinct
        let foreignOnlyCount = routeTrips |> Array.sumBy (fun (_, value) -> if value = ForeignOnlyTrip then 1 else 0)
        let maximumSpan = if qualifying.Length = 0 then None else Some (qualifying |> Array.map fst |> Array.max)
        let maximumDepth = if qualifying.Length = 0 then None else Some (qualifying |> Array.map snd |> Array.max)
        let calculatedKeep = not isPotentiallyInternational || qualifying.Length > 0
        let calculatedReason =
            if not isPotentiallyInternational then "domestic"
            elif qualifying.Length > 0 then "regional_adjacent"
            elif foreignCountries.IsEmpty then "international_metadata_without_foreign_geography"
            elif foreignOnlyCount > 0 && rejectedCount = 0 then "foreign_only_trip"
            elif rejectionReasons.Length = 1 then rejectionReasons.[0]
            else "no_qualifying_cross_border_trip"
        let decision =
                { routeId = route.id; routeDistinction = route.idDistinction
                  keep = calculatedKeep; reason = calculatedReason
                  countries = countries; maximumTripSpanKm = maximumSpan
                  maximumForeignDepthKm = maximumDepth; integrated = integrated
                  retainedDomesticTrips = domesticCount
                  qualifyingCrossBorderTrips = qualifying.Length
                  rejectedCrossBorderTrips = rejectedCount
                  foreignOnlyTrips = foreignOnlyCount }
        routeKey, routeTrips, decision

    let classifications = Array.zeroCreate batch.routes.Length
    let mutable classifiedRoutes = 0L
    let classificationProgressLock = obj()
    let mutable lastClassificationProgress = 0L
    let classifyAt index =
        classifications.[index] <- classify batch.routes.[index]
        let count = Interlocked.Increment(&classifiedRoutes)
        if count % 250L = 0L || count = int64 batch.routes.Length then
            lock classificationProgressLock (fun () ->
                if count > lastClassificationProgress then
                    lastClassificationProgress <- count
                    progress "international-routes" count (Some (int64 batch.routes.Length)))
    if maximumWorkers <= 1 || batch.routes.Length <= 1 then
        for index = 0 to batch.routes.Length-1 do
            classifyAt index
    else
        let parallelOptions = ParallelOptions(MaxDegreeOfParallelism = maximumWorkers)
        Parallel.For(0, batch.routes.Length, parallelOptions, classifyAt)
        |> ignore
    let decisions = classifications |> Array.map (fun (_, _, decision) -> decision)
    let decisionsByRoute =
        decisions
        |> Seq.map (fun decision -> (decision.routeId, decision.routeDistinction), decision)
        |> Map
    let keptRouteKeys =
        decisions
        |> Seq.filter (fun decision -> decision.keep)
        |> Seq.map (fun decision -> decision.routeId, decision.routeDistinction)
        |> Set
    let routeKept routeId routeDistinction = keptRouteKeys.Contains(routeId, routeDistinction)
    let dispositionByTrip = classifications |> Seq.collect (fun (_, trips, _) -> trips) |> Map
    let tripAllowed (trip: JdfModel.Trip) =
        if not (routeKept trip.routeId trip.routeDistinction) then false
        elif not (activeTripContains trip.routeId trip.routeDistinction trip.id) then true
        elif not (isPotentialRoute trip.routeId trip.routeDistinction) then true
        else
            match dispositionByTrip.[trip.routeId, trip.routeDistinction, trip.id] with
            | DomesticTrip | QualifyingCrossBorderTrip _ -> true
            | _ -> false
    let keptTrips = batch.trips |> Array.filter tripAllowed
    let keptTripKeys = HashSet<struct (string * int * int64)>()
    for trip in keptTrips do
        keptTripKeys.Add(struct (trip.routeId, trip.routeDistinction, trip.id)) |> ignore
    let tripKept routeId routeDistinction tripId =
        keptTripKeys.Contains(struct (routeId, routeDistinction, tripId))
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
            routes =
                batch.routes
                |> Array.filter (fun route -> routeKept route.id route.idDistinction)
                |> Array.map (fun route ->
                    let decision = decisionsByRoute.[route.id, route.idDistinction]
                    if decision.qualifyingCrossBorderTrips > 0 then
                        { route with routeType = JdfModel.RegionalInternational }
                    else route)
            routeIntegrations = batch.routeIntegrations |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            routeStops = routeStops
            trips = keptTrips
            tripGroups = batch.tripGroups |> Array.filter (fun group -> usedTripGroups.Contains group.id)
            tripStops = batch.tripStops |> JdfCallStore.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            routeInfo = batch.routeInfo |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            serviceNotes = batch.serviceNotes |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            transfers = batch.transfers |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            agencyAlternations = agencyAlternations
            alternateRouteNames = batch.alternateRouteNames |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            reservationOptions = batch.reservationOptions |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            stopLocations = batch.stopLocations |> Array.filter (fun value -> retainedStopIds.Contains value.stopId)
            stopLocationSources = batch.stopLocationSources |> Array.filter (fun value -> retainedStopIds.Contains value.stopId)
            postCandidateEvidence =
                batch.postCandidateEvidence
                |> Array.filter (fun value -> retainedStopIds.Contains value.stopId)
            routingDemands =
                batch.routingDemands
                |> Array.filter (fun value ->
                    value.previousStopId
                    |> Option.map retainedStopIds.Contains
                    |> Option.defaultValue true
                    && (value.nextStopId
                        |> Option.map retainedStopIds.Contains
                        |> Option.defaultValue true))
            routeVersions = batch.routeVersions |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
        }
    inheritTripVersionKeys batch filteredBatch
    { batch = filteredBatch; decisions = decisions }

let applyInternationalRoutePolicyWithCalendar
        (policy: InternationalRoutePolicy)
        (calendar: CalendarPreparation)
        (batch: JdfModel.JdfBatch) =
    applyInternationalRoutePolicyInternal 1 (fun _ _ _ -> ()) calendar.tripsToDelete policy batch

let applyInternationalRoutePolicyWithCalendarWorkersAndProgress
        maximumWorkers
        (progress: string -> int64 -> int64 option -> unit)
        (policy: InternationalRoutePolicy)
        (calendar: CalendarPreparation)
        (batch: JdfModel.JdfBatch) =
    if maximumWorkers <= 0 then invalidArg "maximumWorkers" "Worker count must be positive"
    applyInternationalRoutePolicyInternal maximumWorkers progress calendar.tripsToDelete policy batch

let applyInternationalRoutePolicy (policy: InternationalRoutePolicy)
                                  (batch: JdfModel.JdfBatch) =
    let calendar = prepareGtfsCalendar batch
    applyInternationalRoutePolicyWithCalendar policy calendar batch

let logInternationalRouteDecisions (policy: InternationalRoutePolicy)
                                   (decisions: InternationalRouteDecision array) =
    if policy <> KeepAll then
        let crossBorder = decisions |> Seq.filter (fun value -> value.countries |> Array.exists ((<>) "CZ"))
        let retained = crossBorder |> Seq.filter (fun value -> value.keep) |> Seq.length
        let dropped = crossBorder |> Seq.filter (fun value -> not value.keep) |> Seq.length
        Log.Information(
            "International route policy {Policy}: retained {RetainedRoutes} and dropped {DroppedRoutes} cross-border route distinctions",
            internationalRoutePolicyName policy, retained, dropped)
