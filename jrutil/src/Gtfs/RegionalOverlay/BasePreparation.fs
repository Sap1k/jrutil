// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BasePreparation

open System.Collections.Generic
open JrUtil.GtfsModel
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay.Runtime

/// Read-only national data. Source-local matching indexes are built separately.
type Result = {
    baseStopRows: CsvRow array
    baseRouteRows: CsvRow array
    baseTripValues: Trip array
    baseCzRouteRows: CsvRow array
    baseDates: Dictionary<string, DateSet.Dates>
    baseRoutes: IDictionary<string, CsvRow>
    baseTrips: IDictionary<string, Trip>
    baseCisByRoute: IDictionary<string, string>
    baseRoutesByCis: IDictionary<string, string array>
    basePlaceByStop: IDictionary<string, string>
    baseStopGroupById: IDictionary<string, StopGroup>
    baseStopGroups: StopGroup array
}

let private loadTrips baseGtfs =
    let text = Dictionary<string, string option>(System.StringComparer.Ordinal)
    let share value =
        match value with
        | None -> None
        | Some value ->
            match text.TryGetValue(value) with
            | true, existing -> existing
            | _ ->
                let stored = Some value
                text.Add(value, stored)
                stored
    let bicycle = Dictionary<BicycleCapacity, BicycleCapacity option>()
    let shareBicycle value =
        match value with
        | None -> None
        | Some key ->
            match bicycle.TryGetValue(key) with
            | true, existing -> existing
            | _ -> bicycle.Add(key, value); value
    csvRows baseGtfs "trips.txt"
    |> Seq.map (fun row ->
        let trip = tripRow row
        { trip with
            routeId = (share (Some trip.routeId)).Value
            serviceId = (share (Some trip.serviceId)).Value
            headsign = share trip.headsign
            shortName = share trip.shortName
            directionId = share trip.directionId
            wheelchairAccessible = share trip.wheelchairAccessible
            bikesAllowed = shareBicycle trip.bikesAllowed })
    |> Seq.toArray

let prepare (window: DateWindow) baseGtfs baseExtensions : Result =
    let baseStopRows = requireTable baseGtfs "stops.txt"
    let baseRouteRows = requireTable baseGtfs "routes.txt"
    let baseTripValues = loadTrips baseGtfs
    let baseCzRouteRows = requireTable baseExtensions "cz_routes.txt"
    let baseDates =
        parseCalendar window (csvRows baseGtfs "calendar.txt") (csvRows baseGtfs "calendar_dates.txt")
    let baseRoutes = baseRouteRows |> Array.map (fun row -> rowValue row "route_id", row) |> dict
    let baseTrips = baseTripValues |> Array.map (fun trip -> trip.id, trip) |> dict
    logProgress "load-base-indexes" (int64 baseTripValues.Length) (Some (int64 baseTripValues.Length))
    let baseCisByRoute =
        baseCzRouteRows
        |> Array.choose (fun row -> optionText (rowValue row "cis_line_id") |> Option.map (fun cis -> rowValue row "route_id", cis))
        |> dict
    let baseRoutesByCis =
        baseCzRouteRows
        |> Array.choose (fun row -> optionText (rowValue row "cis_line_id") |> Option.map (fun cis -> cis, rowValue row "route_id"))
        |> Array.groupBy fst
        |> Array.map (fun (cis, values) -> cis, values |> Array.map snd)
        |> dict
    let basePlaceByStop =
        csvRows baseExtensions "cz_stops.txt"
        |> Seq.map (fun row -> rowValue row "stop_id", rowValue row "stop_place_id")
        |> dict
    let baseStopGroups = groupBaseStops baseStopRows basePlaceByStop
    let baseStopGroupById = baseStopGroups |> Array.map (fun group -> group.groupId, group) |> dict
    {
        baseStopRows = baseStopRows
        baseRouteRows = baseRouteRows
        baseTripValues = baseTripValues
        baseCzRouteRows = baseCzRouteRows
        baseDates = baseDates
        baseRoutes = baseRoutes
        baseTrips = baseTrips
        baseCisByRoute = baseCisByRoute
        baseRoutesByCis = baseRoutesByCis
        basePlaceByStop = basePlaceByStop
        baseStopGroupById = baseStopGroupById
        baseStopGroups = baseStopGroups
    }
