// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// GTFS stop, trip, calendar and call rows used by the overlay.
module internal JrUtil.RegionalOverlay.GtfsRows

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Microsoft.VisualBasic.FileIO
open NodaTime
open NodaTime.Text
open JrUtil.GtfsModel
open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Values

let parseCalendar (window: DateWindow) (calendarRows: CsvRow seq) (exceptionRows: CsvRow seq) =
    let result = Dictionary<string, bool array>(StringComparer.Ordinal)
    let ensure serviceId =
        match result.TryGetValue(serviceId) with
        | true, values -> values
        | _ -> let values = emptyDates window in result.[serviceId] <- values; values
    for row in calendarRows do
        let serviceId = rowValue row "service_id"
        let startDate = parseDate (rowValue row "start_date")
        let endDate = parseDate (rowValue row "end_date")
        let weekdays = [|
            rowValue row "monday" = "1"; rowValue row "tuesday" = "1"
            rowValue row "wednesday" = "1"; rowValue row "thursday" = "1"
            rowValue row "friday" = "1"; rowValue row "saturday" = "1"
            rowValue row "sunday" = "1"
        |]
        let values = ensure serviceId
        window.dates |> Array.iteri (fun index date ->
            if date >= startDate && date <= endDate then
                values.[index] <- weekdays.[int date.DayOfWeek - 1])
    for row in exceptionRows do
        let serviceId = rowValue row "service_id"
        let date = parseDate (rowValue row "date")
        match window.index.TryGetValue(date) with
        | true, index -> (ensure serviceId).[index] <- rowValue row "exception_type" = "1"
        | _ -> ()
    let pool = DateSet.Pool()
    let packed = Dictionary<string, DateSet.Dates>(StringComparer.Ordinal)
    for KeyValue(service, dates) in result do packed.Add(service, pool.Intern dates)
    packed

let locationType value =
    match value with
    | "" -> None
    | "0" -> Some LocationType.Stop
    | "1" -> Some LocationType.Station
    | "2" -> Some LocationType.StationEntrance
    | _ -> None

let stopRow (row: CsvRow) : Stop = {
    id = rowValue row "stop_id"
    code = rowValue row "stop_code" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    name = rowValue row "stop_name"
    description = rowValue row "stop_desc" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    lat = parseDecimalOpt (rowValue row "stop_lat")
    lon = parseDecimalOpt (rowValue row "stop_lon")
    zoneId = rowValue row "zone_id" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    url = rowValue row "stop_url" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    locationType = locationType (rowValue row "location_type")
    parentStation = rowValue row "parent_station" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    timezone = rowValue row "stop_timezone" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    wheelchairBoarding = rowValue row "wheelchair_boarding" |> fun value -> if value = "" then None else Some (parseInt value)
    platformCode = rowValue row "platform_code" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
}

let bicycleCapacity value =
    match value with
    | "1" -> Some BicycleCapacity.OneOrMore
    | "2" -> Some BicycleCapacity.NoBicycles
    | "0" -> Some BicycleCapacity.NoInformation
    | _ -> None

let tripRow (row: CsvRow) : Trip = {
    routeId = rowValue row "route_id"
    serviceId = rowValue row "service_id"
    id = rowValue row "trip_id"
    headsign = rowValue row "trip_headsign" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    shortName = rowValue row "trip_short_name" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    directionId = rowValue row "direction_id" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    blockId = rowValue row "block_id" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    shapeId = rowValue row "shape_id" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    wheelchairAccessible = rowValue row "wheelchair_accessible" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    bikesAllowed = bicycleCapacity (rowValue row "bikes_allowed")
}

let tripToRow (trip: Trip) =
    let row = CsvRow(StringComparer.Ordinal)
    row.["route_id"] <- trip.routeId
    row.["service_id"] <- trip.serviceId
    row.["trip_id"] <- trip.id
    row.["trip_headsign"] <- trip.headsign |> Option.defaultValue ""
    row.["trip_short_name"] <- trip.shortName |> Option.defaultValue ""
    row.["direction_id"] <- trip.directionId |> Option.defaultValue ""
    row.["block_id"] <- trip.blockId |> Option.defaultValue ""
    row.["shape_id"] <- trip.shapeId |> Option.defaultValue ""
    row.["wheelchair_accessible"] <- trip.wheelchairAccessible |> Option.defaultValue ""
    row.["bikes_allowed"] <-
        match trip.bikesAllowed with
        | Some BicycleCapacity.NoInformation -> "0"
        | Some BicycleCapacity.OneOrMore -> "1"
        | Some BicycleCapacity.NoBicycles -> "2"
        | None -> ""
    row

let callColumns = [| "trip_id"; "stop_id"; "arrival_time"; "departure_time"; "stop_sequence"; "shape_dist_traveled"; "stop_headsign" |]

let callValues parentByStop (row: string array) =
    let stopId = row.[1]
    {
        stopId = stopId
        stopPlaceId = parentByStop |> Map.tryFind stopId |> Option.defaultValue stopId
        arrival = row.[2]
        departure = row.[3]
        sequence = parseInt row.[4]
        distance = parseDecimalOpt row.[5]
        stopHeadsign = row.[6] |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    }
