// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// This module provides utility functions that can help repair bad JDF data,
/// intentionally bad or not.
module JrUtil.JdfFixups

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Threading
open NetTopologySuite.Geometries
open Serilog
open Serilog.Events

open JrUtil
open JrUtil.Jdf
open JrUtil.JdfModel
open JrUtil.GeoData.Common
open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.EurTowns
open JrUtil.GeoData.StopMatcher

type JdfStopGeodata = {
    regionId: string option
    country: string option
    // The CRS is assumed to be ETRS89-Extended
    point: Point
    source: string option
}
type JdfStopToMatch = StopToMatch<JdfStopGeodata>

/// Matches that are too spread out (radius > ...) aren't considered for
/// matching
let maxRadiusMetres = 1000.0

let mutable private strictRegionMatchCount = 0L
let mutable private borderRegionMatchCount = 0L
let mutable private countryRejectCount = 0L
let mutable private nonAdjacentRegionRejectCount = 0L
let mutable private outsideBorderToleranceRejectCount = 0L

let resetMatchDiagnostics () =
    Interlocked.Exchange(&strictRegionMatchCount, 0L) |> ignore
    Interlocked.Exchange(&borderRegionMatchCount, 0L) |> ignore
    Interlocked.Exchange(&countryRejectCount, 0L) |> ignore
    Interlocked.Exchange(&nonAdjacentRegionRejectCount, 0L) |> ignore
    Interlocked.Exchange(&outsideBorderToleranceRejectCount, 0L) |> ignore

let logMatchDiagnostics () =
    Log.Information(
        "Stop matcher geography summary: strict region matches {StrictRegionMatches}, border-tolerant matches {BorderRegionMatches}, country rejects {CountryRejects}, non-adjacent region rejects {NonAdjacentRegionRejects}, outside-border-tolerance rejects {OutsideBorderToleranceRejects}",
        strictRegionMatchCount, borderRegionMatchCount, countryRejectCount,
        nonAdjacentRegionRejectCount, outsideBorderToleranceRejectCount)

let private czechRegionAdjacency =
    Utils.memoizeVoidFunc <| fun () ->
        let regions = czechRegionPolygons () |> Map.toArray
        regions
        |> Seq.collect (fun (leftCode, leftPolygon) ->
            regions
            |> Seq.choose (fun (rightCode, rightPolygon) ->
                if leftCode < rightCode && leftPolygon.Distance(rightPolygon) <= 100.0
                then Some (leftCode, rightCode)
                else None))
        |> Set

let private regionsAdjacent left right =
    let pair = if left < right then left, right else right, left
    czechRegionAdjacency().Contains pair

// OL occurs in current CIS JŘ exports, while the JDF/SPZ okres code and the
// checked-in boundary data use OC for Olomouc. Treat it as an identity alias
// during matching without rewriting the source JDF value.
let private canonicalRegionCode = function
    | "OL" -> "OC"
    | code -> code

let private borderToleranceCache =
    ConcurrentDictionary<string * float * float, bool>()

let private withinExpectedRegionBorder expected (point: Point) =
    borderToleranceCache.GetOrAdd(
        (expected, point.X, point.Y),
        fun _ ->
            match czechRegionPolygons() |> Map.tryFind expected with
            | Some polygon -> polygon.Boundary.Distance(point) <= maxRadiusMetres
            | None -> false)

let private regionMatches expectedRegion (candidate: JdfStopGeodata) =
    let expectedRegion = expectedRegion |> Option.map canonicalRegionCode
    let candidateRegion = candidate.regionId |> Option.map canonicalRegionCode
    match expectedRegion, candidateRegion with
    | None, _ -> true
    | Some expected, Some actual when expected = actual ->
        Interlocked.Increment(&strictRegionMatchCount) |> ignore
        true
    | Some expected, Some actual when regionsAdjacent expected actual ->
        if withinExpectedRegionBorder expected candidate.point then
            Interlocked.Increment(&borderRegionMatchCount) |> ignore
            true
        else
            Interlocked.Increment(&outsideBorderToleranceRejectCount) |> ignore
            false
    | Some _, _ ->
        Interlocked.Increment(&nonAdjacentRegionRejectCount) |> ignore
        false

/// Some JDF batches in CIS JŘ public exports have the nearby town two-letter id
/// appended to the town name like so: "Praha [AB]". This function will move it
/// to its rightful place three columns to the right.
let moveRegionFromName =
    let ntRegex = Regex(@"(.*) \[(..)\]$")
    fun (stop: Stop) ->
        let m = ntRegex.Match(stop.town)
        if m.Success
        then {
            stop with
                town = m.Groups.[1].Value
                country = Some "CZ"
                regionId = Some m.Groups.[2].Value
        }
        else stop

let doubleCommaNameRegex = Regex(@"([^,]+),([^,]*),([^,]*)")
let singleCommaNameRegex = Regex(@"([^,]+), *([^,]+)")

/// In the public CIS JŘ exports, stops don't have consistent naming.
/// Sometimes, the whole name, delimited by commas, is in "town".
/// When "districy" should be empty, it sometimes has the value of
/// "nearbyPlace".
/// This function will take a Stop and give it a normalised name that can
/// later be compared between batches, not necessarily so some yet-unpublished
/// official stop registry name.
let normaliseStopName (stop: Stop) =
    let emptyToNone s = if s = "" then None else Some s
    let newTown, newDistrict, newNearbyPlace =
        let dcMatch = doubleCommaNameRegex.Match(stop.town)
        if stop.district.IsNone && stop.nearbyPlace.IsNone
           && dcMatch.Success then
            dcMatch.Groups.[1].Value,
            emptyToNone dcMatch.Groups.[2].Value,
            emptyToNone dcMatch.Groups.[3].Value
        else if stop.district.IsNone then
            stop.town, stop.nearbyPlace, None
        else
            stop.town, stop.district, stop.nearbyPlace
    { stop with
        town = newTown
        district = newDistrict
        nearbyPlace = newNearbyPlace }

// When we know (guess, really) that a batch doesn't use MHD naming, we can
// split stop names with single commas
let normaliseNonMhdStopName (stop: Stop) =
    let scMatch = singleCommaNameRegex.Match(stop.town)
    if stop.district.IsNone && stop.nearbyPlace.IsNone
       && scMatch.Success then
        { stop with
            town = scMatch.Groups.[1].Value
            district = Some scMatch.Groups.[2].Value }
    else stop

let matchStopByName (matcher: StopMatcher<JdfStopGeodata>)
                    (stop: Stop) =
    matcher.matchStop(jdfStopNameString stop)

let exactMatches (stop: Stop) (matches: StopMatch<JdfStopGeodata> array) =
    let countryMatches =
        matches
        |> Array.filter (fun m ->
        // Only take perfect matches
        if m.score <> 1.0f then false else
        // If the stop has a country assigned, honour it.
        let countryMatches =
            match stop.country with
            | Some country -> Some country = m.stop.data.country
            | None -> true
        if not countryMatches then
            Interlocked.Increment(&countryRejectCount) |> ignore
            false
        else true)

    match stop.country, stop.regionId with
    | Some country, _ when country <> "CZ" -> countryMatches
    | _, None -> countryMatches
    | _, Some expectedRegion ->
        let expectedRegion = canonicalRegionCode expectedRegion
        let strictMatches =
            countryMatches
            |> Array.filter (fun m ->
                m.stop.data.regionId
                |> Option.map canonicalRegionCode
                |> Option.contains expectedRegion)
        if strictMatches.Length > 0 then
            Interlocked.Add(&strictRegionMatchCount, int64 strictMatches.Length) |> ignore
            strictMatches
        else
            // Only broaden to an adjacent-boundary candidate when no strict
            // okres candidate exists. Mixing both sets makes a known strict
            // match appear geographically ambiguous.
            countryMatches
            |> Array.filter (fun m -> regionMatches stop.regionId m.stop.data)

let addRegionFromMatch (stop: Stop) match_ =
    match stop.regionId, match_ with
    | None, Some m ->
        { stop with regionId = m.data.regionId;
                    country = m.data.country }
    | _ -> stop

let czTownNameMatcher =
    Utils.memoizeVoidFunc
    <| fun () ->
        Utils.logWrappedOp "Creating Czech town name matcher" <| fun () ->
            // Sure, it's a *Stop* matcher, but it works for parts of a stop's
            // name too
            new StopMatcher<_>(
                czechTownsPolygons ()
                |> Seq.map (fun (n, r, p) ->
                    { name = n
                      data = r, p
                    })
                |> Seq.toArray)

let eurTownNameMatcher =
    Utils.memoizeVoidFunc <| fun () ->
        Utils.logWrappedOp "Creating European town name matcher" <| fun () ->
            new StopMatcher<_>(
                eurTownCountries ()
                |> Seq.map (fun r ->
                    { name = r.Name
                      data = r.CountryCode.ToUpper(), r.Lat, r.Lon })
                |> Seq.toArray,
                Utils.persistentCachePath
                |> Option.map (fun d -> Path.Combine(d, "eur-town-matcher")))

let matchCzTownByNameRaw town =
    let town =
        townSynonyms
        |> Map.tryFind town
        |> Option.defaultValue town
    czTownNameMatcher().matchStop(town)

let matchesCzTownByName town =
    matchCzTownByNameRaw town
    |> Seq.exists (fun m -> m.score = 1.0f)

let topStopMatch (stop: Stop) (matches: StopMatch<JdfStopGeodata> array) =
    let preciseMatches =
        exactMatches stop matches
    let checkedMatches =
        preciseMatches
        |> Array.filter (fun m ->
            m.stop.data.source
            |> Option.map (fun source ->
                not (source.StartsWith("osm:", StringComparison.Ordinal)))
            |> Option.defaultValue true)
    let select candidates =
        if candidates |> Array.isEmpty then None
        // Sanity check - if stops are close together, they need to be in the
        // same region and country.
        else if candidates
                |> Array.map (fun m -> m.stop.data.regionId, m.stop.data.country)
                |> set
                |> Set.count > 1 then None
        else
            let points = candidates |> Array.map (fun m -> m.stop.data.point)
            let radius = pointsRadius points
            if radius < maxRadiusMetres
            then Some {
                name = candidates.[0].stop.name
                data = {
                    regionId = candidates.[0].stop.data.regionId
                    country = candidates.[0].stop.data.country
                    point =
                        etrs89ExFactory.CreateGeometryCollection(
                            points |> Array.map (fun p -> p :> _))
                            .Centroid
                    source =
                        candidates
                        |> Seq.choose (fun m -> m.stop.data.source)
                        |> Seq.distinct
                        |> Seq.sort
                        |> String.concat "+"
                        |> function "" -> None | value -> Some value
                }
            }
            else
                Log.Debug("Not considering match for stop {StopId} since \
                          radius {Radius:n1} is too high",
                          stop.id, radius)
                None

    // Checked catalogues get the first chance, but an ambiguous checked set
    // must not suppress an otherwise usable OSM fallback.
    select checkedMatches
    |> Option.orElseWith (fun () ->
        preciseMatches
        |> Array.filter (fun match_ -> not (checkedMatches |> Array.contains match_))
        |> select)

let matchConflictsWithEurCity (m: StopToMatch<JdfStopGeodata>) =
    // Some Czech towns share names with ones with other countries. Since we
    // only have coordinates for Czech stops (for now), we need to check if we
    // aren't possibly matching a foreign town to a Czech stop.
    // Correct matches that seemingly conflict can be added in secondary
    // matching, where distance is considered.
    let town = m.name.Split(",").[0]
    eurTownNameMatcher().matchStop(town)
    |> Seq.exists (fun tm -> tm.score = 1f)

let matchingCzTownRegions (stop: Stop) =
    matchCzTownByNameRaw stop.town
    |> Array.filter (fun m -> m.score = 1f)
    |> Array.map (fun m ->
        m.stop.name, fst m.stop.data)
    |> Array.distinct

let addGeographyFromTopTown (stop: Stop) mo =
    let czMatches () =
        matchingCzTownRegions stop

    match stop.country, stop.regionId with
    | Some "CZ", None ->
        match czMatches () with
        | [| (_, regionId) |] ->
            { stop with regionId = Some regionId }, mo
        | _ -> stop, mo
    | None, None ->
        let eurTown =
            eurTownNameMatcher().matchStop(stop.town)
            |> Array.tryFind (fun m -> m.score = 1f)
            |> Option.map (fun m -> m.stop.data)
        match czMatches (), eurTown with
        | [| (_, regionId) |], None ->
            { stop with regionId = Some regionId; country = Some "CZ" }, mo
        | [| |], Some (country, _, _) -> { stop with country = Some country }, mo
        | _ -> stop, mo
    | _ -> stop, mo

let distanceToCzRegionEdge (stop1Pos: Point) stop2RegionId =
    let region = (czechRegionPolygons ()).[stop2RegionId]
    stop1Pos.Distance(region.Shell)

// tripStops in a trip are assumed to be sorted by the order they appear on the
// line (one direction only!) and to be for one specific line
let fillStopRegionsFromPrevious
        (tripsMatrix: TripStop option array array)
        stopsWithMatches =
    let mutable lastRegion = None
    let mutable lastCountry = None
    // Tuple of country, region, km, position, distance to region edge
    let mutable lastPosKmPerTrip =
        tripsMatrix
        |> Array.tryHead
        |> Option.defaultValue [||]
        |> Seq.map (fun _ -> None)
        |> Seq.toArray

    let augmentedStops =
        Utils.innerJoinOn (fun (tss: TripStop option array) ->
                              (tss |> Seq.choose id |> Seq.head).stopId)
                          (fun (s: Stop, _) -> s.id)
                          tripsMatrix stopsWithMatches
        |> Seq.map (fun (tss, (s, mo)) ->
            let newStop =
                match s.regionId, s.country with
                // Stop has country and region, no need to change, update prev
                | Some r, Some c ->
                    lastRegion <- Some r
                    lastCountry <- Some c
                    s
                | None, Some c ->
                    if Some c = lastCountry then
                        // Stop has country only and previous country matches,
                        // add region
                        {s with regionId = lastRegion}
                    else
                        // Stop has country only and previous country doesn't
                        // match, don't modify
                        lastRegion <- None
                        lastCountry <- Some c
                        s
                // Stop has region only, add country
                | Some r, None ->
                    lastRegion <- Some r
                    {s with country = lastCountry}
                // Stop has nothing, add region and country
                | None, None ->
                    {s with regionId = lastRegion; country = lastCountry}

            let newStop =
                if newStop.regionId <> s.regionId
                   && Seq.zip tss lastPosKmPerTrip
                      |> Seq.forall (fun (ts, lpo) ->
                          let kmOpt =
                              ts |> Option.bind (fun ts -> ts.kilometer)
                          match lpo, kmOpt with
                          | Some (lc, lr, lkm, _, led), Some km
                            when Some lc = newStop.country
                              && Some lr = newStop.regionId ->
                              float (abs (km - lkm)) > led
                          | _ -> true)
                // If we changed the region but our change is uncertain,
                // revert it
                then
                    Log.Debug("Not setting region for {StopId} from previous \
                               stop since distance is too large \
                               (could cross region boundary)", s.id)
                    s
                else
                    newStop

            let newStop =
                if [ None; Some "CZ" ] |> Seq.contains newStop.country
                   && newStop.regionId.IsNone then
                    // Try to find region by looking at possible towns
                    let regions =
                        czTownNameMatcher().matchStop(s.town)
                        |> Array.filter (fun m -> m.score = 1f)
                        |> Array.filter (fun m ->
                            let region, poly = m.stop.data
                            Seq.zip tss lastPosKmPerTrip
                            |> Seq.exists (fun (tso, lpo) ->
                                let kmOpt =
                                    tso |> Option.bind (fun ts -> ts.kilometer)
                                match lpo, kmOpt with
                                | Some (_, _, lkm, lp, _), Some km ->
                                    poly.Distance(lp) / 1000.0
                                     < float (abs (km - lkm) + 1m)
                                | _ -> false))
                        |> Array.map (fun m -> m.stop.data |> fst)
                        |> set
                        |> Set.toArray
                    match regions with
                    | [| r |] ->
                        Log.Debug("Picked region by distance \
                                   for {StopId}: {Region}",
                                  newStop.id, r)
                        { newStop with country = Some "CZ"; regionId = Some r}
                    | rs ->
                        Log.Debug("Can't pick region by distance \
                                   for {StopId} from {Regions}",
                                  s.id, rs)
                        newStop
                else newStop

            for i, ts in Seq.indexed tss do
                let kmOpt = ts |> Option.bind (fun ts -> ts.kilometer)
                match mo, kmOpt, s.country, s.regionId with
                | Some m, Some km, Some c, Some r when c = "CZ" ->
                    let edgeDist =
                        distanceToCzRegionEdge m.data.point r / 1000.0
                    lastPosKmPerTrip.[i] <-
                        Some (c, r, km, m.data.point, edgeDist)
                | _ -> ()

            newStop, mo
        )
        // Stops can occur multiple times, pick the one with the most
        // information
        |> Seq.groupBy (fun (s: Stop, _) -> s.id)
        |> Seq.map (fun (_, xs) ->
            xs
            |> Seq.sortByDescending (fun (s, _) ->
                s.regionId.IsSome, s.country.IsSome)
            |> Seq.head)
        |> Seq.toArray

    let augmentedStopIds =
        augmentedStops |> Array.map (fun (s, _) -> s.id) |> set
    // Add back in the stops that weren't used in any of the trips
    Array.concat [
        augmentedStops

        stopsWithMatches
        |> Array.filter (fun (s, _) ->
            augmentedStopIds |> Set.contains s.id |> not)
    ]

/// Sort tripStops for one trip by call order
let sortOneTripStops =
    Array.sortBy (fun (ts: TripStop) ->
        ts.routeStopId * if ts.tripId % 2L = 1L then 1L else -1L)

// Takes trip stops and grous them twice: by direction and then by pattern,
// creating two matrices of tripStops (row - one stop)
let groupedTripsMatrices (routeStops: RouteStop array)
                         (tripStops: TripStop array) =
    let routeStopIds =
        routeStops
        |> Array.map (fun rs -> rs.routeStopId)
        |> Array.sort
    let trips = tripStops |> Array.groupBy (fun ts -> ts.tripId)
    let tripsByRem rem =
        trips
        |> Array.filter (fun (tid, ts) -> tid % 2L = rem)
        |> Array.map (fun (_, ts) ->
            Utils.leftJoinOn id (fun (ts: TripStop) -> ts.routeStopId)
                             routeStopIds ts
            |> Seq.map snd
            |> Seq.toArray)
        // Take one trip per physical route
        |> Array.groupBy (Array.map (Option.map (fun t -> t.kilometer)))
        |> Array.map (snd >> Array.head
                      >> (if rem = 0L then Array.rev else id))
        // Transpose to have stops as rows
        |> Array.transpose
        |> Array.filter (Array.exists Option.isSome)

    tripsByRem 1L, tripsByRem 0L

/// Will add matches and regions disambiguated by previous matches' positions
/// and km data from tripStops
let addSecondaryMatches
        tolerance
        (tripsMatrix: TripStop option array array)
        // Array of tuples of:
        // - JDF stop
        // - previously confirmed match
        // - all matches
        (stopsWithMatches: (Stop
                            * JdfStopToMatch option
                            * StopMatch<JdfStopGeodata> array) array) =
    // Tuple of position, km
    let mutable lastPosKmPerTrip =
        tripsMatrix
        |> Array.tryHead
        |> Option.defaultValue [||]
        |> Seq.map (fun _ -> None)
        |> Seq.toArray

    let augmentedStops =
        Utils.innerJoinOn (fun (tss: TripStop option array) ->
                              (tss |> Seq.choose id |> Seq.head).stopId)
                          (fun (s: Stop, _, _) -> s.id)
                          tripsMatrix stopsWithMatches
        |> Seq.map (fun (tss, (s, mo, ams)) ->
            let suggested =
                tss
                |> Seq.mapi (fun i ts ->
                    let kmOpt = ts |> Option.bind (fun ts -> ts.kilometer)
                    match mo, kmOpt, lastPosKmPerTrip.[i] with
                    | Some m, Some km, _ ->
                        lastPosKmPerTrip.[i] <- Some (m.data.point, km)
                        Some m
                    | None, Some km, Some (lp, lkm) ->
                        let kmDiff = abs (km - lkm)
                        ams
                        |> Seq.filter (fun m ->
                            m.stop.data.point.Distance(lp) / 1000.0
                             < float kmDiff + tolerance)
                        |> Seq.toArray
                        |> topStopMatch s
                    | _ -> mo)
                |> Seq.filter ((<>) mo)
            match Seq.tryHead suggested with
            // Check if all trips agree on this match
            | Some (Some pick as pickOpt) ->
                if Seq.forall ((=) pickOpt) suggested then
                    Log.Debug("Picked position for {StopId} by secondary match", s.id)
                    s, pickOpt, ams
                else
                    Log.Debug("Trips disagreed on secondary match \
                               for {StopId}", s.id)
                    s, mo, ams
            | _ -> s, mo, ams)
        |> Seq.groupBy (fun (s: Stop, _, _) -> s.id)
        |> Seq.map (fun (_, xs) ->
            xs
            |> Seq.sortByDescending (fun (_, mo, _) -> mo.IsSome)
            |> Seq.head)
        |> Seq.toArray
    let augmentedStopIds =
        augmentedStops |> Array.map (fun (s, _, _) -> s.id) |> set
    Array.concat [
        augmentedStops

        stopsWithMatches
        |> Array.filter (fun (s, _, _) ->
            augmentedStopIds |> Set.contains s.id |> not)
    ]

let townNoPerfectMatch town =
    not (matchesCzTownByName town)
    &&
    eurTownNameMatcher().matchStop(town)
    |> Seq.forall (fun m -> m.score < 1f)

// The CIS JŘ exports sometimes don't even have one consistent town for one
// timetable. Thankfully it seems rare, so we fix it manually.
let mhdTownsJoined = [
    ["Ústí nad Labem"; "Ústí n.L."; "Trmice"; "Telnice"
     "Petrovice,Krásný Les"]
]

/// Some JDF batches in the public CIS JŘ data don't use the full triple for
/// naming stops, but put everything in the town column. This is often used for
/// intra-town lines, and the stop names don't contain the town name directly.
/// This is a problem for matching these stops, so we need to detect these
/// batches and fix them.
///
/// This funtion looks at MHD-named stops and find the town that matches the
/// most stops
let mhdNamingTownCandidates
        (stopMatcher: StopMatcher<_>)
        (stopsWithMatches: (Stop * StopMatch<JdfStopGeodata> array) array) =
    stopsWithMatches
    |> Seq.filter (fun (s, _) ->
        s.district.IsNone
        && s.nearbyPlace.IsNone)
    |> Seq.collect (fun (s, ms) ->
        // Don't suggest any city if we already have a match with a town
        if (ms |> exactMatches s |> Seq.isEmpty |> not)
           &&
           (matchesCzTownByName (s.town.Split(",").[0].Trim()))
        then
            Log.Debug("Not considering stop {StopId} for MHD town \
                       due to existing match", s.id)
            Seq.empty
        else
            // Look at all the possible matches, group them by the town they
            // suggest, and take the best score for each town
            ms
            |> Seq.map (fun m ->
                let town = m.stop.name.Split(",").[0].Trim()
                town, m)
            // Only consider stops that match town + old name perfectly
            // but don't match the bare old name
            |> Seq.filter (fun (t, m) ->
                stopMatcher.checkExactMatch(
                    stopNameToTokens $"{t},{s.town}",
                    stopNameToTokens m.stop.name)
                &&
                not (stopMatcher.checkExactMatch(
                    stopNameToTokens s.town,
                    stopNameToTokens m.stop.name)))
            |> Seq.map (fun (t, m) ->
                t, m)
            |> Seq.groupBy (fun (t, m) -> t)
            |> Seq.map fst
            // Normalise town names
            |> Seq.map (fun town ->
                mhdTownsJoined
                |> List.tryFind (fun ts -> List.contains town ts)
                |> Option.map (fun ts -> List.head ts)
                |> Option.defaultValue town))
    // Get composite score for each town suggestion
    |> Seq.countBy id
    // Don't let just one stop change the whoel set
    |> Seq.filter (fun (t, c) -> c > 1)
    |> Seq.sortByDescending (fun (t, c) -> c)
    |> Seq.toArray

let fixMhdNaming (stopMatcher: StopMatcher<JdfStopGeodata>) towns (stop: Stop) =
    if stop.district.IsSome || stop.nearbyPlace.IsSome then stop
    else
        let nameSplit = stop.town.Split(",")
        if nameSplit.Length > 3 then
            Log.Warning("MHD-named stop with more than two commas: {StopName}",
                        stop.town)
        let nameSplit12 = String.Join(" ", nameSplit.[1..]).Trim()
        let mhdAdjustedStops =
            towns
            |> List.map (fun town ->
                { stop with
                    town = town
                    district = Some nameSplit.[0]
                    nearbyPlace =
                        if nameSplit12 <> ""
                        then Some nameSplit12
                        else None })
        // We need to check if this is actually an MHD name or if the first
        // element is a town
        let matchedMhdStop =
            mhdAdjustedStops
            |> List.filter (fun mas ->
                matchStopByName stopMatcher mas
                |> topStopMatch mas
                |> Option.isSome)
            |> List.tryHead
        if matchedMhdStop.IsSome then matchedMhdStop.Value
        else if matchesCzTownByName nameSplit.[0]
        then { stop with
                town = nameSplit.[0]
                district = if nameSplit.Length > 1
                           then Some nameSplit.[1]
                           else None
                nearbyPlace = if nameSplit.Length = 3
                              then Some nameSplit.[2]
                              else None }
        else mhdAdjustedStops |> List.head

let matchStops stopMatcher tripsMatrix1 tripsMatrix2 stops =
    stops
    |> Array.map (fun s -> s, matchStopByName stopMatcher s)
    // Try to assign the stop-accurate matches to a stop, if they are
    // unambiguous.
    |> Array.map (fun (s, ms) ->
        let tm = topStopMatch s ms
        let townOnly = s.district.IsNone && s.nearbyPlace.IsNone
        if townOnly && tm |> Option.map matchConflictsWithEurCity = Some true
        then s, None, ms
        else if townOnly
           && (matchesCzTownByName s.town) then
            Log.Debug("Not considering stop matches for stop {StopId} since \
                      its only name component matches a Czech town",
                      s.id)
            s, None, ms
        else s, tm, ms)
    |> addSecondaryMatches 1.0 tripsMatrix1
    |> addSecondaryMatches 1.0 tripsMatrix2
    // Town polygons may fill missing geography, but never coordinates.
    |> Array.map (fun (s, mo, ams) ->
        match mo with
        | None ->
            let s2, mo2 = addGeographyFromTopTown s mo
            s2, mo2, ams
        | Some _ -> s, mo, ams)

let routeDescTownNameRegex = Regex(@"MHD ([^:]*) linka|MHD ([^ :]*)|([^,:-]*) *[,:-]")

let dropDegenerateBatch (jdfBatch: JdfBatch) =
    let hasScheduledTime = function
        | Some (StopTime _) -> true
        | _ -> false
    let calledStopIds =
        jdfBatch.tripStops
        |> Seq.filter (fun call ->
            hasScheduledTime call.arrivalTime
            || hasScheduledTime call.departureTime)
        |> Seq.map (fun call -> call.stopId)
        |> Set
    if calledStopIds.Count >= 2 then None
    else
        Some {
            jdfBatch with
                stops = [||]; stopPosts = [||]; agencies = [||]
                routes = [||]; routeIntegrations = [||]; routeStops = [||]
                trips = [||]; tripGroups = [||]; tripStops = [||]
                routeInfo = [||]; attributeRefs = [||]; serviceNotes = [||]
                transfers = [||]; agencyAlternations = [||]
                alternateRouteNames = [||]; reservationOptions = [||]
                stopLocations = [||]; stopLocationSources = [||]
        }

let townNameFromRouteDesc (route: Route) =
    let townOpt =
        (routeDescTownNameRegex.Match(route.name).Groups
         |> Seq.tail
         |> Seq.tryFind (fun g -> g.Success)
         |> Option.map (fun g -> g.Value))
    townOpt
    |> Option.bind(fun town ->
        if matchesCzTownByName town then Some town else None)

/// Public JDF batches from CIS JŘ have a lot of problems, which this module
/// tries to fix. This function will run them all and return a (hopefully)
/// valid and usable JDF batch, with stop positions as a bonus.
let fixPublicCisJrBatch (stopMatcher: StopMatcher<JdfStopGeodata>)
                        (jdfBatch: JdfBatch) =
    if jdfBatch.routes.Length <> 1 then
        Log.Error("Expected just one route in batch, got {RouteCount}",
                  jdfBatch.routes.Length)

    let tripsMatrix1, tripsMatrix2 =
        groupedTripsMatrices jdfBatch.routeStops jdfBatch.tripStops

    let stops = jdfBatch.stops |> Array.map moveRegionFromName
    let swamNonMhd =
        stops
        |> Array.map (normaliseStopName >> normaliseNonMhdStopName)
        |> matchStops stopMatcher tripsMatrix1 tripsMatrix2
    let routeTownName = townNameFromRouteDesc jdfBatch.routes.[0]
    let mhdTownCandidates =
        stops
        |> Seq.map normaliseStopName
        |> Seq.map (fun s -> s, matchStopByName stopMatcher s)
        |> Seq.toArray
        |> mhdNamingTownCandidates stopMatcher
        |> Array.map (fun (t, s) ->
            // Boost town that matches name in routes file
            match routeTownName with
            | Some rtn ->
                if stopMatcher.checkExactMatch(
                    stopNameToTokens t,
                    stopNameToTokens rtn)
                then t, s + 2
                else t, s
            | None -> t, s)
        |> Array.sortByDescending (fun (t, s) -> s)
    Log.Debug("MHD town candidates: {Candidates}", mhdTownCandidates)
    let stopsWithAllMatches =
        match Array.tryHead mhdTownCandidates with
        | None ->
            Log.Debug("No town candidate for MHD stops, \
                       Proceeding with unmodified stops")
            swamNonMhd
        // They're sorted, so first is best
        | Some (town, _) ->
            Log.Debug("Trying candidate town for MHD naming: {Town}", town)
            let mhdTowns =
                mhdTownsJoined
                |> List.tryFind (fun ts -> List.contains town ts)
                |> Option.defaultValue [town]
            if mhdTowns <> [town] then
                Log.Debug("Expanded MHD town to list: {Towns}", mhdTowns)
            let swamMhd =
                jdfBatch.stops
                |> Array.map (fixMhdNaming stopMatcher mhdTowns)
                |> matchStops stopMatcher tripsMatrix1 tripsMatrix2
            let matchCount =
                Array.map (fun (_, mo, _) -> if Option.isSome mo then 1 else 0)
                >> Array.sum
            let mhdMatchCount = matchCount swamMhd
            let nonMhdMatchCount = matchCount swamNonMhd
            Log.Debug("Comparing MHD stops with {MhdMatches} matches vs. \
                       non-mhd with {NonMhdMatches} matches",
                      mhdMatchCount, nonMhdMatchCount)
            if mhdMatchCount > nonMhdMatchCount + 1
            then swamMhd
            else swamNonMhd

    let ifUnfinished f swm =
        if swm |> Seq.exists (fun (s: Stop, _) ->
            s.country.IsNone || (s.country = Some "CZ" && s.regionId.IsNone))
        then f swm
        else swm

    let augmentedStops =
        stopsWithAllMatches
        // Write data from whatever matches we have into the stops
        |> Array.map (fun (s, mo, _) -> addRegionFromMatch s mo, mo)
        // Fill in gaps, if possible
        |> ifUnfinished (fillStopRegionsFromPrevious tripsMatrix1)
        |> ifUnfinished (fillStopRegionsFromPrevious (Array.rev tripsMatrix1))
        |> ifUnfinished (fillStopRegionsFromPrevious tripsMatrix2)
        |> ifUnfinished (fillStopRegionsFromPrevious (Array.rev tripsMatrix2))
        |> Seq.toArray

    { jdfBatch with stops = augmentedStops |> Array.map fst },
    augmentedStops |> Array.map snd

let private stopLocationFromMatch (stop: Stop) (match_: JdfStopToMatch) =
    let wgs84Pt =
        transformPoint
            etrs89ExSrid wgs84Srid
            (wgs84ToEtrs89Ex.Inverse())
            match_.data.point
    {
        stopId = stop.id
        lat = decimal wgs84Pt.Y
        lon = decimal wgs84Pt.X
        precision = StopPrecise
    }, match_.data.source

let private addMatchedLocations jdfBatch matchedLocations =
    { jdfBatch with
        stopLocations =
            Array.append jdfBatch.stopLocations (matchedLocations |> Array.map fst)
        stopLocationSources =
            Array.append
                jdfBatch.stopLocationSources
                (matchedLocations
                 |> Array.choose (fun (location, source) ->
                     source |> Option.map (fun value -> {
                         stopId = location.stopId
                         source = value
                     })))
    }

let addStopLocations jdfBatch stopsWithMatches =
    let matchedLocations =
        stopsWithMatches
        |> Array.choose (fun (stop: Stop, match_) ->
            match_ |> Option.map (stopLocationFromMatch stop))
    // A fixed single-route batch does not carry pre-existing locations.
    { jdfBatch with stopLocations = [||]; stopLocationSources = [||] }
    |> fun batch -> addMatchedLocations batch matchedLocations

let private tripStopClockMinutes (tripStop: TripStop) =
    let getTime = function
        | Some (StopTime value) -> Some value
        | _ -> None
    getTime tripStop.arrivalTime
    |> Option.orElseWith (fun () -> getTime tripStop.departureTime)
    |> Option.map (fun value ->
        float (value.Hour * 60 + value.Minute) + float value.Second / 60.0)

let private orderedTimedTrips (tripStops: TripStop array) =
    tripStops
    |> Array.groupBy (fun call -> call.routeId, call.routeDistinction, call.tripId)
    |> Array.map (fun ((_, _, tripId), calls) ->
        let direction = if tripIsReverse tripId then -1L else 1L
        let mutable dayOffset = 0.0
        let mutable previous = None
        calls
        |> Array.sortBy (fun call -> call.routeStopId * direction)
        |> Array.map (fun call ->
            let normalizedTime =
                tripStopClockMinutes call
                |> Option.map (fun raw ->
                    let mutable value = raw + dayOffset
                    while previous |> Option.exists (fun old -> value < old - 720.0) do
                        dayOffset <- dayOffset + 1440.0
                        value <- raw + dayOffset
                    previous <- Some value
                    value)
            call, normalizedTime))

let rejectImplausibleMatches
        (tripStops: TripStop array)
        (stopsWithMatches: (Stop * JdfStopToMatch option) array) =
    let matchesByStop =
        stopsWithMatches
        |> Array.choose (fun (stop, match_) ->
            match_ |> Option.map (fun value -> stop.id, value))
        |> Map
    let conflicts = Dictionary<int64, HashSet<int64>>()
    let supports = Dictionary<int64, HashSet<int64>>()
    let addNeighbor (target: Dictionary<int64, HashSet<int64>>) left right =
        let found, neighbors = target.TryGetValue(left)
        let neighbors =
            if found then neighbors
            else
                let value = HashSet<int64>()
                target.[left] <- value
                value
        neighbors.Add(right) |> ignore
    let addEdge target left right =
        addNeighbor target left right
        addNeighbor target right left

    orderedTimedTrips tripStops
    |> Array.iter (fun calls ->
        calls
        |> Array.choose (fun (call, time) ->
            match time, matchesByStop |> Map.tryFind call.stopId with
            | Some value, Some match_ -> Some (call.stopId, value, match_)
            | _ -> None)
        |> Array.pairwise
        |> Array.iter (fun ((leftId, leftTime, left), (rightId, rightTime, right)) ->
            let elapsedMinutes = rightTime - leftTime
            if elapsedMinutes >= 0.0 then
                let distanceKm = left.data.point.Distance(right.data.point) / 1000.0
                // 2 km of local slack plus a deliberately generous 150 km/h.
                // This catches impossible matches without policing timetables.
                let maximumKm = 2.0 + elapsedMinutes * 2.5
                if distanceKm > maximumKm then addEdge conflicts leftId rightId
                else addEdge supports leftId rightId))

    let neighborCount (source: Dictionary<int64, HashSet<int64>>) stopId =
        match source.TryGetValue(stopId) with
        | true, values -> values.Count
        | _ -> 0
    let rejected = HashSet<int64>()
    for pair in conflicts do
        if pair.Value.Count >= 2 then rejected.Add(pair.Key) |> ignore
    for pair in conflicts do
        for neighbor in pair.Value do
            if not (rejected.Contains(pair.Key) || rejected.Contains(neighbor)) then
                let leftSupport = neighborCount supports pair.Key
                let rightSupport = neighborCount supports neighbor
                if leftSupport > rightSupport then rejected.Add(neighbor) |> ignore
                else if rightSupport > leftSupport then rejected.Add(pair.Key) |> ignore
                else
                    // With no contextual reason to trust either endpoint,
                    // keeping both would preserve a known-impossible edge.
                    rejected.Add(pair.Key) |> ignore
                    rejected.Add(neighbor) |> ignore

    if rejected.Count > 0 then
        Log.Warning(
            "Rejected {MatchCount} stop matches contradicted by scheduled travel time",
            rejected.Count)
    stopsWithMatches
    |> Array.map (fun (stop, match_) ->
        stop, if rejected.Contains(stop.id) then None else match_)

let estimateMissingStopLocations (jdfBatch: JdfBatch) =
    let positioned = jdfBatch.stopLocations |> Array.map (fun value -> value.stopId) |> Set
    let pointsByStop =
        jdfBatch.stopLocations
        |> Array.map (fun location ->
            location.stopId,
            (wgs84Factory.CreatePoint(
                Coordinate(float location.lon, float location.lat))
             |> pointWgs84ToEtrs89Ex))
        |> Map
    let estimates = Dictionary<int64, ResizeArray<Point * string>>()
    let addEstimate stopId point source =
        let found, values = estimates.TryGetValue(stopId)
        let values =
            if found then values
            else
                let value = ResizeArray<Point * string>()
                estimates.[stopId] <- value
                value
        values.Add(point, source)
    let previousIndex predicate index =
        seq { index - 1 .. -1 .. 0 } |> Seq.tryFind predicate
    let nextIndex length predicate index =
        seq { index + 1 .. length - 1 } |> Seq.tryFind predicate

    orderedTimedTrips jdfBatch.tripStops
    |> Array.iter (fun calls ->
        let knownPoint index =
            let call, _ = calls.[index]
            pointsByStop |> Map.tryFind call.stopId
        let knownTimedPoint index =
            let _, time = calls.[index]
            match knownPoint index, time with
            | Some point, Some value -> Some (point, value)
            | _ -> None
        for index = 0 to calls.Length - 1 do
            let call, callTime = calls.[index]
            if not (positioned.Contains call.stopId) && callTime.IsSome then
                // Untimed calls are passing/not-passing route points, not
                // usable anchors. Continue to the nearest called, timed stop.
                let previousKnown = previousIndex (knownTimedPoint >> Option.isSome) index
                let nextKnown = nextIndex calls.Length (knownTimedPoint >> Option.isSome) index
                match previousKnown, nextKnown with
                | Some leftIndex, Some rightIndex ->
                    let leftPoint, left = knownTimedPoint leftIndex |> Option.get
                    let rightPoint, right = knownTimedPoint rightIndex |> Option.get
                    match callTime with
                    | Some current when right > left ->
                        let ratio = Math.Clamp((current - left) / (right - left), 0.0, 1.0)
                        let point = etrs89ExFactory.CreatePoint(
                            Coordinate(
                                leftPoint.X + (rightPoint.X - leftPoint.X) * ratio,
                                leftPoint.Y + (rightPoint.Y - leftPoint.Y) * ratio))
                        addEstimate call.stopId point "estimated:route-time"
                    | _ -> ()
                | None, Some anchorIndex ->
                    let anchor, _ = knownTimedPoint anchorIndex |> Option.get
                    let offset = 300.0 * float (anchorIndex - index)
                    addEstimate call.stopId
                        (etrs89ExFactory.CreatePoint(Coordinate(anchor.X, anchor.Y + offset)))
                        "estimated:route-end-north"
                | Some anchorIndex, None ->
                    let anchor, _ = knownTimedPoint anchorIndex |> Option.get
                    let offset = 300.0 * float (index - anchorIndex)
                    addEstimate call.stopId
                        (etrs89ExFactory.CreatePoint(Coordinate(anchor.X, anchor.Y + offset)))
                        "estimated:route-end-north"
                | None, None -> ())

    let estimatedLocations =
        estimates
        |> Seq.map (fun pair ->
            let selectedEstimates =
                let timed =
                    pair.Value
                    |> Seq.filter (fun (_, source) -> source = "estimated:route-time")
                    |> Seq.toArray
                if timed.Length > 0 then timed else pair.Value |> Seq.toArray
            let points = selectedEstimates |> Array.map fst
            let source =
                selectedEstimates
                |> Array.map snd
                |> Array.distinct
                |> Array.sort
                |> String.concat "+"
            let point =
                etrs89ExFactory.CreateGeometryCollection(
                    points |> Array.map (fun value -> value :> Geometry))
                    .Centroid
                |> transformPoint etrs89ExSrid wgs84Srid (wgs84ToEtrs89Ex.Inverse())
            {
                stopId = pair.Key
                lat = decimal point.Y
                lon = decimal point.X
                precision = Estimated
            }, Some source)
        |> Seq.toArray
    if estimatedLocations.Length > 0 then
        Log.Information(
            "Estimated {LocationCount} missing stop locations from route timing",
            estimatedLocations.Length)
    addMatchedLocations jdfBatch estimatedLocations

/// WARN: Expects tripsStops to be for one trip only and sorted by call order
let checkMatchDistances
        (tripStops: TripStop array)
        (stopsWithMatches: (Stop * JdfStopToMatch option) array) =
    let international =
        stopsWithMatches
        |> Array.tryFind (fun (s, _) ->
            s.country
            |> Option.map (fun c -> c <> "CZ")
            |> Option.defaultValue false)
        |> Option.isSome
    let internationalNote =
        if international then " on international route" else ""

    // Some timetables have unrealistically low distance values,
    // I guess nobody checks them...
    let multTolerance = 2.0
    let preciseTolerance = 4m
    let mutable lastPos = None
    let mutable lastPosStopId = None
    let mutable lastPosKm = None
    Utils.innerJoinOn (fun (ts: TripStop) -> ts.stopId)
                      (fun (s: Stop, m) -> s.id)
                      tripStops stopsWithMatches
    |> Seq.choose (fun (tripStop, (stop, matchOpt)) ->
        let warning =
            match lastPos, lastPosStopId, lastPosKm, matchOpt,
                  tripStop.kilometer with
            | Some (lp: Point), Some lpsi, Some lkm, Some m, Some km ->
                let dist = lp.Distance(m.data.point) / 1000.0
                let kmDiff = abs (km - lkm)
                if dist > float (kmDiff + preciseTolerance) * multTolerance
                then Some <| Utils.logEvent
                      LogEventLevel.Warning
                      ("Distance between matches is too high"
                       + internationalNote
                       + " (got {MatchDist}, expected {ExpKm} between \
                          {StopId1} and {StopId2}, {Pos1} and {Pos2})")
                      [|box dist; kmDiff; lpsi; stop.id; lp; m.data.point|]
                else None
            | _ -> None

        match matchOpt, tripStop.kilometer with
        | Some m, Some km ->
            lastPos <- Some m.data.point
            lastPosStopId <- Some stop.id
            lastPosKm <- Some km
        | _ -> ()

        warning
    )

let checkMissingRegionsCountries (jdfBatch: JdfBatch) =
    let isUsed stopId =
        jdfBatch.tripStops
        |> Array.tryFind (fun ts ->
            ts.stopId = stopId
            && match ts.arrivalTime, ts.departureTime with
               | Some (StopTime _), _ -> true
               | _, Some (StopTime _) -> true
               | _ -> false)
        |> Option.isSome
    let level stopId =
        if isUsed stopId
        then LogEventLevel.Warning
        else LogEventLevel.Debug

    jdfBatch.stops
    |> Seq.choose (fun s ->
        match s.country, s.regionId with
        | None, None ->
            Some <| Utils.logEvent
                (level s.id)
                ((if isUsed s.id then "Stop" else "Unused stop")
                 + " without country: {StopId} ({StopName})")
                [|box s.id; jdfStopNameString s |]
        | Some "CZ", None ->
            Some <| Utils.logEvent
                (level s.id)
                ((if isUsed s.id then "Czech stop" else "Unused Czech stop")
                 + " without region: {StopId} ({StopName})")
                [|box s.id; jdfStopNameString s |]
        | _ -> None)
