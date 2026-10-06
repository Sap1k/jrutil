// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfMerger

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open NodaTime
open Serilog
open NetTopologySuite.Geometries

open JrUtil.JdfModel
open JrUtil.JdfStopReconciliation
open JrUtil.JdfParser
open JrUtil.JdfSerializer
open JrUtil.DateUtils
open JrUtil.ParallelUtils
open JrUtil.Utils
open JrUtil.GeoData.Common
open JrUtil.JdfTripStopSpool


type StopMergeStrategy =
    // The default, should work on any valid JDF
    | MergeStopsByName
    // Can be convenient if you know all batches have consistent IDs
    | MergeStopsById

type JdfMerger(
    stopMergeStrategy: StopMergeStrategy,
    ?tripStopSpoolPath: string,
    ?tripStopTransformWorkers: int,
    ?stopRegistry: StopRegistry.StopRegistry) =
    // The merged batch is dated by the build's reference date, not the wall
    // clock, so identical inputs produce identical merged JDF bytes.
    let mutable referenceDate: LocalDate option = None
    let stops = ResizeArray()
    let stopPosts = ResizeArray()
    let agenciesByIco = MultiDict()
    let routesByLicNum = MultiDict()
    let routeIntegrationsByRoute = MultiDict()
    let routeStopsByRoute = MultiDict<string * int, RouteStop>()
    let tripsByRoute = MultiDict()
    let tripGroups = ResizeArray()
    let tripStopsByRoute = MultiDict()
    let tripStopSpool = tripStopSpoolPath |> Option.map (fun path -> new TripStopSpool(path))
    let tripStopTransformWorkers = defaultArg tripStopTransformWorkers 1
    do
        if tripStopTransformWorkers <= 0 then
            invalidArg "tripStopTransformWorkers" "Trip-stop transform workers must be positive"
    let routeInfoByRoute = MultiDict()
    let attributeRefs = ResizeArray()
    let serviceNotesByRoute = MultiDict()
    let transfersByRoute = MultiDict()
    let agencyAlternationsByRoute = MultiDict()
    let alternateRouteNamesByRoute = MultiDict()
    let reservationOptionsByRoute = MultiDict()
    let stopLocationsByStop = Dictionary<int64, StopLocation>()
    let stopLocationSourcesByStop = Dictionary<int64, string>()
    let postCandidateEvidence =
        Dictionary<struct (int64 * string), PostCandidateEvidence>()
    let stopIndexesById = Dictionary<int64, int>()
    let stopReconciler = StopReconciler()
    // Registered numbers (after merged_into resolution), their reference
    // coordinates and the identities already recorded for them.
    let registryIds = HashSet<int64>()
    let registryReferences = Dictionary<int64, float * float>()
    let registryIdentities = HashSet<struct (int64 * string)>()
    let registryStopsByTown = Dictionary<string, ResizeArray<Stop>>(StringComparer.Ordinal)
    let provisionalReasons = Dictionary<int64, string>()
    let provisionalSpellings = Dictionary<int64, Dictionary<string, Stop>>()
    let unregisteredAliases = Dictionary<string, int64 * Stop>(StringComparer.Ordinal)
    let localityKey (stop: Stop) =
        String.concat "\u001f" [|
            stop.regionId |> Option.defaultValue "" |> fun value -> value.Trim().ToUpperInvariant()
            stop.country |> Option.defaultValue "" |> fun value -> value.Trim().ToUpperInvariant()
        |]
    let spellingKey (stop: Stop) = stopDisplayName stop + "\u001f" + localityKey stop
    do
        stopRegistry
        |> Option.iter (fun registry ->
            if stopMergeStrategy <> MergeStopsByName then
                invalidArg "stopRegistry" "A stop registry requires the name-based merge strategy"
            for row in registry.stops do
                let id = row.resolvedId
                let stop = {
                    id = id
                    town = row.town
                    district = row.district
                    nearbyPlace = row.nearbyPlace
                    regionId = row.regionId
                    country = row.country
                    attributes = Array.create 6 None
                }
                registryIds.Add(id) |> ignore
                registryIdentities.Add(struct (id, stopIdentityKey stop)) |> ignore
                row.reference
                |> Option.iter (fun point -> registryReferences.TryAdd(id, point) |> ignore)
                // Registered identities are known before any batch, so they
                // resolve the same way whatever the merge order. Reference
                // coordinates apply the usual 75 m gate to precise locations.
                let reference =
                    row.reference
                    |> Option.map (fun (lat, lon) ->
                        { stopId = id; lat = decimal lat; lon = decimal lon; precision = StopPrecise })
                stopReconciler.AddAlias(id, stop, reference)
                let town = row.town.Trim().ToLowerInvariant()
                match registryStopsByTown.TryGetValue(town) with
                | true, values -> values.Add(stop)
                | false, _ -> registryStopsByTown.[town] <- ResizeArray([stop])
            Log.Information(
                "Loaded stop registry {Sha256}: {Rows} rows, {Ids} numbers",
                registry.sha256, registry.stops.Length, registryIds.Count))

    let mutable lastStopId = 0L
    let mutable lastAttributeRefId = 0
    let mutable lastTripGroupId = 0

    let attributeRefsByValue = Dictionary()
    let attributeArrays =
        Dictionary<int option array, int option array>(HashIdentity.Structural)
    let stopsByIds = Dictionary()
    let stopPostsSet = HashSet()
    let batchDateByRoute = Dictionary<string * int, LocalDate option>()
    // Validity as published, before overlap resolution cuts it
    let originalValidityByRoute = Dictionary<string * int, LocalDate * LocalDate>()

    let locationDistance (loc1: StopLocation) (loc2: StopLocation) =
        let locToPt (loc: StopLocation) =
            wgs84Factory.CreatePoint(
                Coordinate(float loc.lon, float loc.lat))
            |> pointWgs84ToEtrs89Ex
        (locToPt loc1).Distance(locToPt loc2)
    let locationDistanceThresh = 1000

    let mergeAttributes stopId (left: int option array) (right: int option array) =
        let merged =
            Seq.append (left |> Array.choose id) (right |> Array.choose id)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray
        if merged.Length > 6 then
            Log.Warning(
                "Merged stop {StopId} has {AttributeCount} attributes; JDF can retain only six",
                stopId,
                merged.Length)
        Array.init 6 (fun index ->
            if index < min 6 merged.Length then Some merged.[index] else None)

    member private this.batchWithTripStops tripStops =
        let locations =
            stopLocationsByStop
            |> Seq.map (fun pair -> pair.Key, pair.Value)
            |> Map
        let candidateStops: HashSet<int64> =
            postCandidateEvidence.Keys
            |> Seq.map (fun struct (stopId, _) -> stopId)
            |> HashSet
        let demandId mode previous next searchClass =
            let payload = $"{mode}|{previous}|{next}|{searchClass}"
            SHA256.HashData(Encoding.UTF8.GetBytes(payload))
            |> Convert.ToHexString
            |> fun value -> value.ToLowerInvariant()
        let demand mode searchClass previous next =
            let previousLocation = previous |> Option.bind (fun id -> locations |> Map.tryFind id)
            let nextLocation = next |> Option.bind (fun id -> locations |> Map.tryFind id)
            {
                demandId = demandId mode previous next searchClass
                modeFamily = mode
                previousStopId = previous
                nextStopId = next
                previousLat = previousLocation |> Option.map (fun value -> value.lat)
                previousLon = previousLocation |> Option.map (fun value -> value.lon)
                nextLat = nextLocation |> Option.map (fun value -> value.lat)
                nextLon = nextLocation |> Option.map (fun value -> value.lon)
                searchClass = searchClass
            }: RoutingDemand
        let pairDemands =
            routeStopsByRoute.Values
            |> Seq.collect (fun (values: ResizeArray<RouteStop>) ->
                let ordered = values |> Seq.sortBy (fun value -> value.routeStopId) |> Seq.toArray
                ordered
                |> Seq.pairwise
                |> Seq.choose (fun (left: RouteStop, right: RouteStop) ->
                    if candidateStops.Contains(left.stopId)
                       || candidateStops.Contains(right.stopId) then
                        Some (demand "both" "local" (Some left.stopId) (Some right.stopId))
                    else None))
        let terminalDemands =
            candidateStops
            |> Seq.choose (fun stopId ->
                if locations.ContainsKey(stopId) then
                    Some (demand "both" "terminal" (Some stopId) None)
                else None)
        let routingDemands =
            Seq.append pairDemands terminalDemands
            |> Seq.filter (fun value ->
                (value.previousLat.IsSome && value.previousLon.IsSome)
                || (value.nextLat.IsSome && value.nextLon.IsSome))
            |> Seq.distinctBy (fun value -> value.demandId)
            |> Seq.sortBy (fun value -> value.modeFamily, value.previousStopId, value.nextStopId,
                                         value.searchClass, value.demandId)
            |> Seq.toArray
        {
        version = {
            version = "1.11"
            duNum = None
            region = None
            batchId = None
            creationDate = referenceDate |> Option.orElseWith (fun () -> Some (dateToday ()))
            generator = Some "JrUtil JdfMerger"
        }
        stops = stops |> Seq.toArray
        stopPosts = stopPosts |> Seq.toArray
        agencies = agenciesByIco.Values |> Seq.collect id |> Seq.toArray
        routes = routesByLicNum.Values |> Seq.collect id |> Seq.toArray
        routeIntegrations =
            routeIntegrationsByRoute.Values |> Seq.collect id |> Seq.toArray
        routeStops =
            routeStopsByRoute.Values |> Seq.collect id |> Seq.toArray
        trips = tripsByRoute.Values |> Seq.collect id |> Seq.toArray
        tripGroups = tripGroups |> Seq.toArray
        tripStops = tripStops
        routeInfo = routeInfoByRoute.Values |> Seq.collect id |> Seq.toArray
        attributeRefs = attributeRefs |> Seq.toArray
        serviceNotes =
            serviceNotesByRoute.Values |> Seq.collect id |> Seq.toArray
        transfers = transfersByRoute.Values |> Seq.collect id |> Seq.toArray
        agencyAlternations =
            agencyAlternationsByRoute.Values |> Seq.collect id |> Seq.toArray
        alternateRouteNames =
            alternateRouteNamesByRoute.Values |> Seq.collect id |> Seq.toArray
        reservationOptions =
            reservationOptionsByRoute.Values |> Seq.collect id |> Seq.toArray
        stopLocations = stopLocationsByStop.Values |> Seq.toArray
        stopLocationSources =
            stopLocationSourcesByStop
            |> Seq.map (fun pair -> { stopId = pair.Key; source = pair.Value })
            |> Seq.toArray
        postCandidateEvidence =
            postCandidateEvidence.Values
            |> Seq.sortBy (fun value -> value.stopId, value.candidateId, value.observationId)
            |> Seq.toArray
        routingDemands = routingDemands
        routeVersions =
            routesByLicNum.Values
            |> Seq.collect id
            |> Seq.map (fun route ->
                let key = route.id, route.idDistinction
                let validFrom, validTo = originalValidityByRoute.[key]
                { routeId = route.id; routeDistinction = route.idDistinction
                  originalValidFrom = validFrom; originalValidTo = validTo
                  sourceCreationDate = batchDateByRoute.[key] }: RouteVersion)
            |> Seq.sortBy (fun value -> value.routeId, value.routeDistinction)
            |> Seq.toArray
    }

    member this.batch =
        let tripStops =
            match tripStopSpool with
            | Some spool -> spool.ToArray()
            | None -> tripStopsByRoute.Values |> Seq.collect id |> Seq.toArray
        this.batchWithTripStops tripStops

    member this.write(path: string) =
        match tripStopSpool with
        | None -> Jdf.jdfBatchDirWriter () (Jdf.FsPath path) this.batch
        | Some spool ->
            let withoutTripStops = this.batchWithTripStops [||]
            Jdf.jdfBatchDirWriter () (Jdf.FsPath path) withoutTripStops
            use output = File.Open(Path.Combine(path, "Zasspoje.txt"), FileMode.Create)
            spool.WriteTo(output)

    member _.tripStopSpillBytes =
        tripStopSpool |> Option.map (fun spool -> spool.PeakLength) |> Option.defaultValue 0L

    member _.stopMergeStatistics = stopReconciler.Statistics

    member _.logStopMergeSummary() =
        let statistics = stopReconciler.Statistics
        Log.Information(
            "Stop reconciliation completed: {ExactCount} exact, {SuffixCount} suffix, "
            + "{FuzzyCount} fuzzy, {AmbiguousCount} ambiguous, "
            + "{CandidateComparisonCount} candidate comparisons "
            + "({FuzzyComparisonCount} fuzzy)",
            statistics.exact,
            statistics.suffix,
            statistics.fuzzy,
            statistics.ambiguous,
            statistics.candidateComparisons,
            statistics.fuzzyComparisons)

    member private this.deleteRoute(r: Route) =
        let routeId = r.id
        let routeDistinction = r.idDistinction
        routesByLicNum.[routeId].RemoveAll(fun r ->
            r.idDistinction = routeDistinction) |> ignore
        routeIntegrationsByRoute.Remove((routeId, routeDistinction)) |> ignore
        routeStopsByRoute.Remove((routeId, routeDistinction)) |> ignore
        tripsByRoute.Remove((routeId, routeDistinction)) |> ignore
        match tripStopSpool with
        | Some spool -> spool.Delete((routeId, routeDistinction))
        | None -> tripStopsByRoute.Remove((routeId, routeDistinction)) |> ignore
        routeInfoByRoute.Remove((routeId, routeDistinction)) |> ignore
        serviceNotesByRoute.Remove((routeId, routeDistinction)) |> ignore
        transfersByRoute.Remove((routeId, routeDistinction)) |> ignore
        agencyAlternationsByRoute.Remove((routeId, routeDistinction)) |> ignore
        alternateRouteNamesByRoute.Remove((routeId, routeDistinction)) |> ignore
        reservationOptionsByRoute.Remove((routeId, routeDistinction)) |> ignore
        batchDateByRoute.Remove((routeId, routeDistinction)) |> ignore
        originalValidityByRoute.Remove((routeId, routeDistinction)) |> ignore

    member private this.copyRoute(copy: Route) =
        let oldDist = copy.idDistinction
        let newDist =
            (routesByLicNum.[copy.id]
             |> Seq.map (fun r -> r.idDistinction)
             |> Seq.max) + 1
        routesByLicNum.[copy.id].Add(
            {copy with idDistinction = newDist })
        routeIntegrationsByRoute.Replace((copy.id, newDist),
            routeIntegrationsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        routeStopsByRoute.Replace((copy.id, newDist),
            routeStopsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        tripsByRoute.Replace((copy.id, newDist),
            tripsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        match tripStopSpool with
        | Some spool -> spool.Copy((copy.id, oldDist), (copy.id, newDist))
        | None ->
            tripStopsByRoute.Replace((copy.id, newDist),
                tripStopsByRoute.[(copy.id, oldDist)]
                |> Seq.map (fun x -> {
                    x with
                        routeId = copy.id
                        routeDistinction = newDist
                }))
        routeInfoByRoute.Replace((copy.id, newDist),
            routeInfoByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        serviceNotesByRoute.Replace((copy.id, newDist),
            serviceNotesByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        transfersByRoute.Replace((copy.id, newDist),
            transfersByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        agencyAlternationsByRoute.Replace((copy.id, newDist),
            agencyAlternationsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        alternateRouteNamesByRoute.Replace((copy.id, newDist),
            alternateRouteNamesByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        reservationOptionsByRoute.Replace((copy.id, newDist),
            reservationOptionsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            }))
        batchDateByRoute.[(copy.id, newDist)] <-
            batchDateByRoute.[(copy.id, oldDist)]
        originalValidityByRoute.[(copy.id, newDist)] <-
            originalValidityByRoute.[(copy.id, oldDist)]

    /// Day classes a route version's trips can run on: bits 0-6 are
    /// Monday-Sunday on ordinary days, bit 7 is a public holiday. Conservative:
    /// a trip without day codes, or with dated "runs also/only" notes, can run
    /// on any day, so two masks are disjoint only when the versions provably
    /// never run on the same date.
    member private _.dayClassMask(route: Route) =
        let key = route.id, route.idDistinction
        let attributeValues =
            attributeRefs |> Seq.map (fun (value: AttributeRef) -> value.attributeId, value.value) |> dict
        let dated =
            serviceNotesByRoute.[key]
            |> Seq.filter (fun (note: ServiceNote) ->
                note.noteType = Some ServiceAlso || note.noteType = Some ServiceOnly)
            |> Seq.map (fun note -> note.tripId)
            |> HashSet
        let everyDay = 0xFF
        tripsByRoute.[key]
        |> Seq.fold (fun mask (trip: Trip) ->
            let days =
                trip.attributes
                |> Array.choose id
                |> Array.choose (fun id ->
                    match attributeValues.TryGetValue(id) with
                    | true, WeekdayService -> Some 0b0001_1111
                    | true, HolidaySundayService -> Some 0b1100_0000
                    | true, DayOfWeekService day when day >= 1 && day <= 7 -> Some ((1 <<< (day - 1)) ||| 0b1000_0000)
                    | _ -> None)
            let tripMask =
                if dated.Contains(trip.id) || days.Length = 0 then everyDay
                else Array.fold (|||) 0 days
            mask ||| tripMask) 0

    /// Resolve overlapping versions of one licence date by date: on each date
    /// the version with priority wins (a newer batch, or a detour as below);
    /// otherwise the one starting later, then the one ending sooner, then the
    /// first added. Each version keeps the dates it wins, split into
    /// contiguous ranges. Deciding every date against the published input
    /// ranges keeps the result independent of the order of comparisons.
    ///
    /// Versions with the same validity, batch date and detour flag whose
    /// trips run on disjoint day classes (a weekday-only and a weekend-only
    /// timetable published side by side) are kept together instead of one
    /// deleting the other.
    member private this.resolveLicenceOverlaps(versions: Route array) =
        let batchDate (route: Route) = batchDateByRoute.[(route.id, route.idDistinction)]
        // CIS publishes many detour timetables open-ended, ending with the
        // regular versions. Such a detour only lasts until the next version
        // starts, so it has no priority over a variant that starts later and
        // was originally published with the same end.
        let detourPriority (route: Route) (other: Route) =
            let routeFrom, routeTo =
                originalValidityByRoute.[(route.id, route.idDistinction)]
            let otherFrom, otherTo =
                originalValidityByRoute.[(other.id, other.idDistinction)]
            route.detour && not (routeTo = otherTo && routeFrom < otherFrom)
        let hasPriority (route: Route) (other: Route) =
            let routeDetour = detourPriority route other
            let otherDetour = detourPriority other route
            (routeDetour && (not otherDetour || batchDate other < batchDate route))
            || (not otherDetour && batchDate other < batchDate route)
        let beats (route: Route) (other: Route) =
            if hasPriority route other then true
            elif hasPriority other route then false
            elif route.timetableValidFrom <> other.timetableValidFrom then
                route.timetableValidFrom > other.timetableValidFrom
            elif route.timetableValidTo <> other.timetableValidTo then
                route.timetableValidTo < other.timetableValidTo
            else route.idDistinction < other.idDistinction

        // Units: one version, or side-by-side versions on disjoint day classes.
        let units = ResizeArray<ResizeArray<Route> * int>()
        for route in versions do
            let mask = this.dayClassMask route
            let partner =
                units
                |> Seq.tryFindIndex (fun (members, unitMask) ->
                    let head = members.[0]
                    head.timetableValidFrom = route.timetableValidFrom
                    && head.timetableValidTo = route.timetableValidTo
                    && head.detour = route.detour
                    && batchDate head = batchDate route
                    && unitMask &&& mask = 0)
            match partner with
            | Some index ->
                let members, unitMask = units.[index]
                members.Add(route)
                units.[index] <- (members, unitMask ||| mask)
            | None -> units.Add((ResizeArray([ route ]), mask))
        let head index = (fst units.[index]).[0]

        let boundaries =
            units
            |> Seq.collect (fun (members, _) ->
                [ members.[0].timetableValidFrom; members.[0].timetableValidTo + Period.FromDays(1) ])
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray
        let won = Array.init units.Count (fun _ -> ResizeArray<LocalDate * LocalDate>())
        for i in 0 .. boundaries.Length - 2 do
            let first, last = boundaries.[i], boundaries.[i + 1] - Period.FromDays(1)
            let winner =
                seq { 0 .. units.Count - 1 }
                |> Seq.filter (fun index ->
                    let route = head index
                    route.timetableValidFrom <= first && route.timetableValidTo >= first)
                |> Seq.fold (fun best index ->
                    match best with
                    | Some current when not (beats (head index) (head current)) -> best
                    | _ -> Some index) None
            match winner with
            | Some index ->
                let ranges = won.[index]
                if ranges.Count > 0 && snd ranges.[ranges.Count - 1] + Period.FromDays(1) = first then
                    ranges.[ranges.Count - 1] <- (fst ranges.[ranges.Count - 1], last)
                else ranges.Add((first, last))
            | None -> ()

        let dateText (date: LocalDate) = NodaTime.Text.LocalDatePattern.Iso.Format(date)
        for index in 0 .. units.Count - 1 do
            let members, _ = units.[index]
            let ranges = won.[index]
            for route in members do
                if ranges.Count = 0 then
                    Log.Debug(
                        "Route {LicNum} version {Dist} is superseded on every date, removing it",
                        route.id, route.idDistinction)
                    this.deleteRoute(route)
                else
                    let first, last = ranges.[0]
                    if first <> route.timetableValidFrom || last <> route.timetableValidTo || ranges.Count > 1 then
                        Log.Debug(
                            "Route {LicNum} version {Dist} keeps {Ranges}",
                            route.id, route.idDistinction,
                            ranges |> Seq.map (fun (first, last) -> dateText first + ".." + dateText last) |> String.concat ", ")
                    routesByLicNum.[route.id].Remove(route) |> ignore
                    routesByLicNum.[route.id].Add(
                        { route with timetableValidFrom = first; timetableValidTo = last })
                    for first, last in ranges |> Seq.skip 1 do
                        this.copyRoute(
                            { route with timetableValidFrom = first; timetableValidTo = last })

    member this.resolveRouteOverlaps() =
        for id in routesByLicNum.Keys |> Seq.toArray do
            let versions =
                routesByLicNum.[id] |> Seq.sortBy (fun route -> route.idDistinction) |> Seq.toArray
            if versions.Length > 1 then this.resolveLicenceOverlaps(versions)

    /// Keep only versions valid on or after `reference` within the GVD, clamped to
    /// its bounds. The GVD change is a hard cutover, so validity past its end (often
    /// open-ended on international lines) is never real. Run after overlap
    /// resolution, which needs every version's published validity.
    member this.boundValidity(reference: LocalDate, gvdStart: LocalDate, gvdEnd: LocalDate) =
        referenceDate <- Some reference
        let mutable expired = 0
        let mutable nextGvd = 0
        let mutable clamped = 0
        for id in routesByLicNum.Keys |> Seq.toArray do
            for route in routesByLicNum.[id] |> Seq.toArray do
                if route.timetableValidTo < reference || route.timetableValidTo < gvdStart then
                    expired <- expired + 1
                    this.deleteRoute(route)
                else if route.timetableValidFrom > gvdEnd then
                    nextGvd <- nextGvd + 1
                    this.deleteRoute(route)
                else if route.timetableValidFrom < gvdStart || route.timetableValidTo > gvdEnd then
                    clamped <- clamped + 1
                    routesByLicNum.[id].Remove(route) |> ignore
                    routesByLicNum.[id].Add(
                        {route with
                            timetableValidFrom = max route.timetableValidFrom gvdStart
                            timetableValidTo = min route.timetableValidTo gvdEnd})
        Log.Information(
            "Bounded route validity to {GvdStart}..{GvdEnd} as of {Reference}: \
             dropped {Expired} expired and {NextGvd} next-GVD versions, clamped {Clamped}",
            NodaTime.Text.LocalDatePattern.Iso.Format(gvdStart), NodaTime.Text.LocalDatePattern.Iso.Format(gvdEnd),
            NodaTime.Text.LocalDatePattern.Iso.Format(reference), expired, nextGvd, clamped)

    /// Several registered stops share a name: take the one whose reference
    /// coordinates are nearest to a precise incoming location.
    member private _.nearestRegisteredCandidate(candidates: StopReconciliationMatch array,
                                                location: StopLocation option) =
        match location with
        | Some location when location.precision = StopPrecise
                             && candidates |> Array.forall (fun candidate ->
                                 registryReferences.ContainsKey candidate.stopId) ->
            let point = float location.lat, float location.lon
            candidates
            |> Array.sortBy (fun candidate ->
                StopRegistry.distanceMetres point registryReferences.[candidate.stopId], candidate.stopId)
            |> Array.tryHead
        | _ -> None

    member private _.recordStopIdentity(stopId: int64, stop: Stop) =
        if stopRegistry.IsSome then
            match provisionalSpellings.TryGetValue(stopId) with
            | true, spellings -> spellings.TryAdd(spellingKey stop, stop) |> ignore
            | false, _ when provisionalReasons.ContainsKey stopId ->
                provisionalSpellings.[stopId] <- Dictionary([KeyValuePair(spellingKey stop, stop)], StringComparer.Ordinal)
            | false, _ ->
                if registryIds.Contains stopId
                   && not (registryIdentities.Contains(struct (stopId, stopIdentityKey stop))) then
                    unregisteredAliases.TryAdd(spellingKey stop, (stopId, stop)) |> ignore

    /// Stops without a registered number, and spellings of registered stops
    /// that matched only by suffix, fuzzy name or locality, as review rows in
    /// `StopRegistry.stopCandidatesHeader` order.
    member _.stopRegistryCandidates =
        let text = Option.defaultValue ""
        let locationColumns stopId =
            match stopLocationsByStop.TryGetValue(stopId) with
            | true, location when location.precision = StopPrecise ->
                StopRegistry.formatCoordinate (float location.lat),
                StopRegistry.formatCoordinate (float location.lon)
            | _ -> "", ""
        let suggestions (stop: Stop) =
            match registryStopsByTown.TryGetValue(stop.town.Trim().ToLowerInvariant()) with
            | true, registered ->
                registered
                |> Seq.filter (fun candidate ->
                    compatibleLocality stop candidate && stopNameSimilarity stop candidate >= 0.85)
                |> Seq.map _.id
                |> Seq.distinct
                |> Seq.sort
                |> Seq.truncate 3
                |> Seq.map string
                |> String.concat ";"
            | false, _ -> ""
        let row (provisionalId: string) (stop: Stop) lat lon reason suggestion = [|
            provisionalId; stop.town; text stop.district; text stop.nearbyPlace
            text stop.regionId; text stop.country; lat; lon; reason; suggestion
        |]
        let provisional =
            provisionalSpellings
            |> Seq.sortBy _.Key
            |> Seq.collect (fun pair ->
                let lat, lon = locationColumns pair.Key
                pair.Value
                |> Seq.sortWith (fun left right -> StringComparer.Ordinal.Compare(left.Key, right.Key))
                |> Seq.map (fun spelling ->
                    row (string pair.Key) spelling.Value lat lon provisionalReasons.[pair.Key] (suggestions spelling.Value)))
        let aliases =
            unregisteredAliases
            |> Seq.sortWith (fun left right -> StringComparer.Ordinal.Compare(left.Key, right.Key))
            |> Seq.map (fun pair ->
                let stopId, stop = pair.Value
                let lat, lon = locationColumns stopId
                row "" stop lat lon "alias" (string stopId))
        Seq.append provisional aliases |> Seq.toArray

    member _.logStopRegistrySummary() =
        if stopRegistry.IsSome then
            let registered = stops |> Seq.filter (fun stop -> stop.id < StopRegistry.provisionalBase) |> Seq.length
            Log.Information(
                "Stop registry: {Registered} registered, {Provisional} provisional "
                + "({Ambiguous} ambiguous), {Aliases} unregistered spellings",
                registered,
                provisionalReasons.Count,
                provisionalReasons.Values |> Seq.filter ((=) "ambiguous") |> Seq.length,
                unregisteredAliases.Count)

    member this.beginAdd(batch: JdfBatch) =
        let attributeRefIdMap = Dictionary<int, int>()
        let existingAttributeRefsByValue =
            Dictionary<Attribute * string option, int>(attributeRefsByValue)
        for ar in batch.attributeRefs do
            match existingAttributeRefsByValue.TryGetValue((ar.value, ar.reserved1)) with
            | true, existingId -> attributeRefIdMap.[ar.attributeId] <- existingId
            | false, _ ->
                lastAttributeRefId <- lastAttributeRefId + 1
                let added = { ar with attributeId = lastAttributeRefId }
                attributeRefs.Add(added)
                attributeRefsByValue.[(added.value, added.reserved1)] <- added.attributeId
                attributeRefIdMap.[ar.attributeId] <- added.attributeId
        let mapAttributes attributes =
            let mapped =
                attributes
                |> Array.map (Option.map (fun id -> attributeRefIdMap.[id]))
            match attributeArrays.TryGetValue(mapped) with
            | true, interned -> interned
            | false, _ ->
                attributeArrays.Add(mapped, mapped)
                mapped

        let batchLocationsByStop = Dictionary<int64, StopLocation>()
        for location in batch.stopLocations do
            batchLocationsByStop.[location.stopId] <- location
        let stopIdMap = Dictionary<int64, int64>()
        let preferredIncomingLocationSources = HashSet<int64>()

        for sourceStop in batch.stops do
            let mappedStop = { sourceStop with attributes = mapAttributes sourceStop.attributes }
            let sourceLocation =
                match batchLocationsByStop.TryGetValue(sourceStop.id) with
                | true, location -> Some location
                | false, _ -> None
            match stopMergeStrategy with
            | MergeStopsById ->
                match stopsByIds.TryGetValue(sourceStop.id) with
                | true, existingId -> stopIdMap.[sourceStop.id] <- existingId
                | false, _ ->
                    stopsByIds.[sourceStop.id] <- sourceStop.id
                    stopIndexesById.[sourceStop.id] <- stops.Count
                    stops.Add(mappedStop)
                    stopIdMap.[sourceStop.id] <- sourceStop.id
            | MergeStopsByName ->
                let resolved =
                    match stopReconciler.FindMatch(sourceStop, sourceLocation) with
                    | Choice2Of2 candidates when candidates.Length > 1 ->
                        match this.nearestRegisteredCandidate(candidates, sourceLocation) with
                        | Some candidate -> Choice1Of2 candidate
                        | None -> Choice2Of2 candidates
                    | result -> result
                match resolved with
                | Choice1Of2 candidate ->
                    let stopIndex =
                        match stopIndexesById.TryGetValue(candidate.stopId) with
                        | true, stopIndex ->
                            let current = stops.[stopIndex]
                            let incomingPreferred = isCanonicalNamePreferred mappedStop current
                            let preferred = if incomingPreferred then mappedStop else current
                            let other = if incomingPreferred then current else mappedStop
                            stops.[stopIndex] <- {
                                preferred with
                                    id = candidate.stopId
                                    regionId = preferred.regionId |> Option.orElse other.regionId
                                    country = preferred.country |> Option.orElse other.country
                                    attributes = mergeAttributes candidate.stopId current.attributes mappedStop.attributes
                            }
                            if incomingPreferred then
                                preferredIncomingLocationSources.Add(sourceStop.id) |> ignore
                            stopIndex
                        | false, _ ->
                            // First use of a registered number in this merge.
                            let stopIndex = stops.Count
                            stopIndexesById.[candidate.stopId] <- stopIndex
                            stops.Add({ mappedStop with id = candidate.stopId })
                            stopIndex
                    stopIdMap.[sourceStop.id] <- candidate.stopId
                    stopReconciler.AddAlias(candidate.stopId, sourceStop, sourceLocation)
                    this.recordStopIdentity(candidate.stopId, sourceStop)
                    let aliasName = stopDisplayName sourceStop
                    let canonicalName = stopDisplayName stops.[stopIndex]
                    if aliasName <> canonicalName then
                        Log.Information(
                            "Merged stop {AliasName} into {CanonicalName} ({MatchKind}, "
                            + "Levenshtein {Levenshtein:F3}, Dice {Dice:F3}, distance {Distance})",
                            aliasName,
                            canonicalName,
                            candidate.kind,
                            candidate.levenshteinSimilarity,
                            candidate.tokenDiceSimilarity,
                            candidate.distance)
                | Choice2Of2 candidates ->
                    if candidates.Length > 0 then
                        let candidateDetails =
                            candidates
                            |> Array.map (fun candidate ->
                                let existing =
                                    match stopIndexesById.TryGetValue(candidate.stopId) with
                                    | true, stopIndex -> stopDisplayName stops.[stopIndex]
                                    | false, _ -> $"registered {candidate.stopId}"
                                $"{existing} "
                                + $"[{candidate.kind}; distance={candidate.distance}]" )
                        Log.Warning(
                            "Ambiguous stop reconciliation for {StopName}; candidates: {Candidates}",
                            stopDisplayName sourceStop,
                            candidateDetails)
                    let newId =
                        match stopRegistry with
                        | None ->
                            lastStopId <- lastStopId + 1L
                            lastStopId
                        | Some _ ->
                            let identity = stopIdentityKey sourceStop + "\u001f" + localityKey sourceStop
                            let id = StopRegistry.provisionalStopId identity stopIndexesById.ContainsKey
                            provisionalReasons.[id] <- if candidates.Length > 0 then "ambiguous" else "new"
                            id
                    let added = { mappedStop with id = newId }
                    stopIndexesById.[added.id] <- stops.Count
                    stops.Add(added)
                    stopIdMap.[sourceStop.id] <- added.id
                    stopReconciler.AddAlias(added.id, sourceStop, sourceLocation)
                    this.recordStopIdentity(added.id, sourceStop)
        let batchLocationSources = Dictionary<int64, string>()
        for value in batch.stopLocationSources do
            batchLocationSources.[value.stopId] <- value.source
        for (sl: StopLocation) in batch.stopLocations do
            let stopId = stopIdMap.[sl.stopId]
            let hasOldSl, oldSl = stopLocationsByStop.TryGetValue(stopId)

            // Check if old location isn't too far
            if hasOldSl
               && oldSl.precision = StopPrecise
               && sl.precision = StopPrecise
               && (oldSl.lat <> sl.lat || oldSl.lon <> sl.lon)
               && locationDistance sl oldSl > locationDistanceThresh then
                Log.Warning("Stop location for {StopId} in new batch is too \
                             far from existing location: {Lat}, {Lon}",
                            stopId, sl.lat, sl.lon)

            let precisionRank = function
                | StopPrecise -> 0
                | Estimated -> 1

            // Either this is a new location or a precision upgrade.
            if not hasOldSl
               || precisionRank sl.precision < precisionRank oldSl.precision
               || (precisionRank sl.precision = precisionRank oldSl.precision
                   && preferredIncomingLocationSources.Contains(sl.stopId))
            then
                stopLocationsByStop.[stopId] <- { sl with stopId = stopId }
                match batchLocationSources.TryGetValue(sl.stopId) with
                | true, source -> stopLocationSourcesByStop.[stopId] <- source
                | false, _ -> stopLocationSourcesByStop.Remove(stopId) |> ignore

        for evidence in batch.postCandidateEvidence do
            let mapped = { evidence with stopId = stopIdMap.[evidence.stopId] }
            let key = struct (mapped.stopId, mapped.observationId)
            match postCandidateEvidence.TryGetValue(key) with
            | true,existing when existing<>mapped ->
                invalidArg "batch"
                    $"Conflicting merged post observation {mapped.stopId}/{mapped.observationId}"
            | true,_ -> ()
            | false,_ -> postCandidateEvidence.[key] <- mapped
        let stopPostsToAdd =
            batch.stopPosts
            |> Seq.filter (fun sp ->
                not <| stopPostsSet.Contains((stopIdMap.[sp.stopId], sp.stopPostId)))
            |> Seq.map (fun sp -> { sp with stopId = stopIdMap.[sp.stopId] })
        stopPosts.AddRange(stopPostsToAdd)
        for sp in stopPostsToAdd do
            stopPostsSet.Add((sp.stopId, sp.stopPostId)) |> ignore

        let existingAgenciesMap =
            batch.agencies
            |> Seq.map (fun a ->
                a,
                agenciesByIco.[a.id]
                |> Seq.tryFind (fun a2 ->
                    a = {a2 with idDistinction = a.idDistinction}))
            |> Seq.cache
        let agenciesToAdd =
            existingAgenciesMap
            |> Seq.filter (fun (_, e) -> Option.isNone e)
            |> Seq.map fst
            |> Seq.cache
        let agenciesToAddNewId =
            agenciesToAdd
            |> Seq.map (fun a ->
                let other: Agency ResizeArray = agenciesByIco.[a.id]
                let subId =
                    if other |> Seq.isEmpty then 1
                    else (other
                          |> Seq.map (fun a -> a.idDistinction)
                          |> Seq.max) + 1
                let ani = { a with idDistinction = subId }
                // We need to add it right away, in case the input has multiple
                // agencies of the same ID
                agenciesByIco.[a.id].Add(ani)
                ani)
            |> Seq.toArray
        let agencyIdMap =
            Seq.concat [
                Seq.zip agenciesToAdd agenciesToAddNewId
                |> Seq.map (fun (a, ani) ->
                    (a.id, a.idDistinction), (ani.id, ani.idDistinction))

                existingAgenciesMap
                |> Seq.choose (fun (a, eo) ->
                    eo |> Option.map (fun e ->
                        (a.id, a.idDistinction), (e.id, e.idDistinction)))
            ]
            |> Map

        // For later merging steps
        let routesMap = Dictionary<string * int, Route>()
        for route in batch.routes do
            routesMap.[(route.id, route.idDistinction)] <- route
        // We don't resolve validity overlaps here and leave that for a
        // post-processing phase
        let newRoutes =
            batch.routes
            |> Array.map (fun r ->
                let other = routesByLicNum.[r.id]
                let aid, aidd = agencyIdMap.[(r.agencyId, r.agencyDistinction)]
                {r with
                    idDistinction = (Seq.length other) + 1
                    agencyId = aid
                    agencyDistinction = aidd})
        // Already merged input keeps the identity of its source versions
        let inputVersions =
            batch.routeVersions
            |> Array.map (fun value -> (value.routeId, value.routeDistinction), value)
            |> dict
        for source, r in Seq.zip batch.routes newRoutes do
            routesByLicNum.[r.id].Add(r)

            match inputVersions.TryGetValue((source.id, source.idDistinction)) with
            | true, version ->
                batchDateByRoute.[(r.id, r.idDistinction)] <- version.sourceCreationDate
                originalValidityByRoute.[(r.id, r.idDistinction)] <-
                    (version.originalValidFrom, version.originalValidTo)
            | _ ->
                batchDateByRoute.[(r.id, r.idDistinction)] <-
                    batch.version.creationDate
                originalValidityByRoute.[(r.id, r.idDistinction)] <-
                    (r.timetableValidFrom, r.timetableValidTo)
        let routeIdMap = Dictionary<string * int, string * int>()
        for source, mapped in Seq.zip batch.routes newRoutes do
            routeIdMap.[(source.id, source.idDistinction)] <-
                (mapped.id, mapped.idDistinction)

        batch.routeIntegrations
        |> Seq.map (fun ri ->
            let rid, ridd = routeIdMap.[(ri.routeId, ri.routeDistinction)]
            { ri with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ri -> ri.routeId, ri.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeIntegrationsByRoute.Replace(k, v))

        batch.routeStops
        |> Seq.map (fun (rs: RouteStop) ->
            let rid, ridd = routeIdMap.[(rs.routeId, rs.routeDistinction)]
            { rs with
                routeId = rid
                routeDistinction = ridd
                stopId = stopIdMap.[rs.stopId]
                attributes = mapAttributes rs.attributes
            })
        |> Seq.groupBy (fun rs -> rs.routeId, rs.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeStopsByRoute.Replace(k, v))

        let newTripGroups =
            batch.tripGroups
            |> Seq.map (fun tg ->
                lastTripGroupId <- lastTripGroupId + 1
                { tg with
                    id = lastTripGroupId
                })
        tripGroups.AddRange(newTripGroups)
        let tripGroupIdMap =
            Seq.zip batch.tripGroups newTripGroups
            |> Seq.map (fun (tg, tgni) -> tg.id, tgni.id)
            |> Map

        batch.trips
        |> Seq.map (fun t ->
            let rid, ridd = routeIdMap.[(t.routeId, t.routeDistinction)]
            let route = routesMap.[(t.routeId, t.routeDistinction)]
            { t with
                routeId = rid
                routeDistinction = ridd
                tripGroupId =
                    if route.grouped
                    then Some tripGroupIdMap.[Option.get t.tripGroupId]
                    else None
                attributes = mapAttributes t.attributes
            })
        |> Seq.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Seq.iter (fun (k, v) -> tripsByRoute.Replace(k, v))

        let tripStopAttributes =
            Dictionary<int option array, int option array>(HashIdentity.Structural)
        // Populate the structural interning dictionaries in source order
        // before workers perform read-only lookups. This keeps allocation and
        // dictionary insertion deterministic while allowing the dominant
        // relation's remapping to use the aggressive CLI worker ceiling.
        for source in batch.tripStops do
            if not (tripStopAttributes.ContainsKey(source.attributes)) then
                tripStopAttributes.Add(source.attributes, mapAttributes source.attributes)
        let mappedTripStop (ts: TripStop) =
            let rid, ridd = routeIdMap.[(ts.routeId, ts.routeDistinction)]
            { ts with
                routeId = rid
                routeDistinction = ridd
                stopId = stopIdMap.[ts.stopId]
                attributes = tripStopAttributes.[ts.attributes]
            }
        let tripStopRegistration =
            match tripStopSpool with
            | Some spool ->
                spool.BeginMappedBatch(batch.tripStops, tripStopTransformWorkers, mappedTripStop)
            | None ->
                batch.tripStops
                |> mapParallelOrderedBatches tripStopTransformWorkers mappedTripStop
                |> Seq.groupBy (fun ts -> ts.routeId, ts.routeDistinction)
                |> Seq.iter (fun (k, v) -> tripStopsByRoute.Replace(k, v))
                Threading.Tasks.Task.FromResult(fun () -> ())

        batch.routeInfo
        |> Seq.map (fun ri ->
            let rid, ridd = routeIdMap.[(ri.routeId, ri.routeDistinction)]
            { ri with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ri -> ri.routeId, ri.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeInfoByRoute.Replace(k, v))

        batch.serviceNotes
        |> Seq.map (fun sn ->
            let rid, ridd = routeIdMap.[(sn.routeId, sn.routeDistinction)]
            { sn with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun sn -> sn.routeId, sn.routeDistinction)
        |> Seq.iter (fun (k, v) -> serviceNotesByRoute.Replace(k, v))

        batch.transfers
        |> Seq.map (fun t ->
            let rid, ridd = routeIdMap.[(t.routeId, t.routeDistinction)]
            { t with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Seq.iter (fun (k, v) -> transfersByRoute.Replace(k, v))

        batch.agencyAlternations
        |> Seq.map (fun aa ->
            let rid, ridd = routeIdMap.[(aa.routeId, aa.routeDistinction)]
            let aid, aidd = agencyIdMap.[(aa.agencyId, aa.agencyDistinction)]
            { aa with
                routeId = rid
                routeDistinction = ridd
                agencyId = aid
                agencyDistinction = aidd
                attributes = mapAttributes aa.attributes
            })
        |> Seq.groupBy (fun aa -> aa.routeId, aa.routeDistinction)
        |> Seq.iter (fun (k, v) -> agencyAlternationsByRoute.Replace(k, v))

        batch.alternateRouteNames
        |> Seq.map (fun arn ->
            let rid, ridd = routeIdMap.[(arn.routeId, arn.routeDistinction)]
            { arn with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun arn -> arn.routeId, arn.routeDistinction)
        |> Seq.iter (fun (k, v) -> alternateRouteNamesByRoute.Replace(k, v))

        batch.reservationOptions
        |> Seq.map (fun ro ->
            let rid, ridd = routeIdMap.[(ro.routeId, ro.routeDistinction)]
            { ro with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ro -> ro.routeId, ro.routeDistinction)
        |> Seq.iter (fun (k, v) -> reservationOptionsByRoute.Replace(k, v))

        tripStopRegistration

    member this.add(batch: JdfBatch) =
        let register = this.beginAdd(batch).GetAwaiter().GetResult()
        register ()

    interface IDisposable with
        member _.Dispose() =
            tripStopSpool
            |> Option.iter (fun spool -> (spool :> IDisposable).Dispose())
