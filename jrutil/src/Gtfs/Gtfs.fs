// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2025 David Koňařík

module JrUtil.Gtfs

open System
open System.IO

open JrUtil.GtfsCsvSerializer
open JrUtil.GtfsModel
open JrUtil.GtfsParser

let internal obehyFeedInfo version startDate endDate = {
    publisherName = "Oběhy project (via JrUtil)"
    publisherUrl = "https://obehy.cz"
    lang = "cs"
    startDate = startDate
    endDate = endDate
    version = version
    contactEmail = Some "admin@obehy.cz"
}

let markApproximateStopName (name: string) =
    let suffix = " [?]"
    if name.EndsWith(suffix, StringComparison.Ordinal) then name
    else name + suffix

let gtfsStopTimesToFolder () =
    fun path stopTimes ->
        Directory.CreateDirectory(path) |> ignore
        let filePath = Path.Combine(path, "stop_times.txt")
        use file = File.Open(filePath, FileMode.Create)
        writeStandardStopTimes file stopTimes

/// The feed's standard and Czech tables as in-memory text tables
/// (file name, header, replayable rows). Czech tables are compiler-internal.
let feedTables (feed: GtfsFeed) =
    let table name (formatter: string array * ('r -> string array)) (rows: 'r seq) =
        let header, cells = formatter
        name, header, (fun () -> rows |> Seq.map cells)
    let optional name formatter rows = rows |> Option.map (table name formatter) |> Option.toList
    let standard = [
        yield table "agency.txt" getCellFormatter<Agency> feed.agencies
        yield table "stops.txt" getCellFormatter<Stop> feed.stops
        yield table "routes.txt" getCellFormatter<Route> feed.routes
        yield table "trips.txt" getCellFormatter<Trip> feed.trips
        yield! optional "shapes.txt" getCellFormatter<ShapePoint> feed.shapes
        yield! optional "calendar.txt" getCellFormatter<CalendarEntry> feed.calendar
        yield! optional "calendar_dates.txt" getCellFormatter<CalendarException> feed.calendarExceptions
        yield! optional "transfers.txt" getCellFormatter<Transfer> feed.transfers
        yield! feed.feedInfo |> Option.map (fun info -> table "feed_info.txt" getCellFormatter<FeedInfo> [ info ]) |> Option.toList
        yield "stop_times.txt", standardStopTimeHeader, (fun () -> feed.stopTimes |> Seq.map standardStopTimeCells)
    ]
    let czech = [
        yield! optional "cz_routes.txt" getCellFormatter<CzRoute> feed.czRoutes
        yield! optional "cz_trips.txt" getCellFormatter<CzTrip> feed.czTrips
        yield! optional "cz_stops.txt" getCellFormatter<CzStop> feed.czStops
        yield! optional "cz_stop_zones.txt" getCellFormatter<CzStopZone> feed.czStopZones
        yield! optional "cz_trip_stop_zones.txt" getCellFormatter<CzTripStopZone> feed.czTripStopZones
    ]
    standard, czech

let gtfsParseFolder () =
    // In Python, I'd make a dictionary of file name -> type
    // I think this is the best I can do without reflection.
    let fileParser name =
        let parser = getGtfsFileParser
        fun path -> parser (Path.Combine(path, name)) |> Seq.toArray
    let fileParserOpt name =
        let parser = getGtfsFileParser
        fun path ->
            let p = Path.Combine(path, name)
            if File.Exists(p) then Some <| (parser p |> Seq.toArray) else None

    let agenciesParser = fileParser "agency.txt"
    let stopsParser = fileParser "stops.txt"
    let routesParser = fileParser "routes.txt"
    let tripsParser = fileParser "trips.txt"
    let stopTimesParser = fileParser "stop_times.txt"
    let calendarParser = fileParserOpt "calendar.txt"
    let calendarExceptionsParser = fileParserOpt "calendar_dates.txt"
    let feedInfoParser = fileParserOpt "feed_info.txt"
    let transfersParser = fileParserOpt "transfers.txt"
    let shapesParser = fileParserOpt "shapes.txt"
    let czRoutesParser = fileParserOpt "cz_routes.txt"
    let czTripsParser = fileParserOpt "cz_trips.txt"
    let czStopsParser = fileParserOpt "cz_stops.txt"
    let czStopZonesParser = fileParserOpt "cz_stop_zones.txt"
    let czTripStopZonesParser = fileParserOpt "cz_trip_stop_zones.txt"

    fun path ->
        let feed: GtfsFeed = {
            agencies = agenciesParser path
            stops = stopsParser path
            routes = routesParser path
            trips = tripsParser path
            stopTimes = stopTimesParser path
            shapes = shapesParser path
            calendar = calendarParser path
            calendarExceptions = calendarExceptionsParser path
            feedInfo =
                feedInfoParser path
                |> Option.map (fun fi -> fi.[0])
            transfers = transfersParser path
            czRoutes = czRoutesParser path
            czTrips = czTripsParser path
            czStops = czStopsParser path
            czStopZones = czStopZonesParser path
            czTripStopZones = czTripStopZonesParser path
        }
        feed

/// The GTFS standard requires that some fields we have no way of filling from
/// source data be populated, and some GTFS-consuming software actually needs
/// them filled, even if the user doesn't. To make our exports GTFS-compliant,
/// fill them with nonsense.
let fillStandardRequiredFields (feed: GtfsFeed) =
    { feed with
        agencies =
            feed.agencies
            |> Array.map (fun a ->
                { a with
                    url = a.url
                        |> Option.defaultValue "jrutil://invalid"
                        |> Some
                })
        stops =
            feed.stops
            |> Array.map (fun s ->
                { s with
                    lat = s.lat |> Option.defaultValue 0m |> Some
                    lon = s.lon |> Option.defaultValue 0m |> Some
                })
    }

let deduplicateCalendar (feed: GtfsFeed) =
    let allCalEntries = feed.calendar |> Option.defaultValue [||]
    let allCalExcs = feed.calendarExceptions |> Option.defaultValue [||]
    let serviceIds =
        Set.union
            (allCalEntries |> Array.map (fun ce -> ce.id) |> Set)
            (allCalExcs |> Array.map (fun c -> c.id) |> Set)
    let calById = allCalEntries |> Array.map (fun ce -> ce.id, ce) |> Map
    let excsById = allCalExcs |> Array.groupBy (fun ce -> ce.id) |> Map
    let deduplicated =
        serviceIds
        |> Seq.map (fun si ->
            let calEntry = calById |> Map.tryFind si
            let calExcs = excsById |> Map.tryFind si |> Option.defaultValue [||]
            let dataWithoutId =
                calEntry |> Option.map (fun ce -> { ce with id = "" }),
                calExcs |> Array.map (fun ce -> { ce with id = "" })
                        |> Array.sort
            dataWithoutId, si
        )
        // A canonical text key: structural hashing of the exception arrays only
        // looks at their first elements, so equal-prefix calendars collide.
        |> Seq.groupBy (fun ((entry, exceptions), _) ->
            let key = System.Text.StringBuilder()
            let date (value: NodaTime.LocalDate) =
                key.Append(value.Year).Append('-').Append(value.Month).Append('-').Append(value.Day) |> ignore
            match entry with
            | Some entry ->
                key.Append('E') |> ignore
                for day in entry.weekdayService do key.Append(if day then '1' else '0') |> ignore
                date entry.startDate
                key.Append('/') |> ignore
                date entry.endDate
            | None -> key.Append('N') |> ignore
            for item in exceptions do
                key.Append(';') |> ignore
                date item.date
                key.Append(match item.exceptionType with
                           | ServiceAdded -> '+'
                           | ServiceRemoved -> '-') |> ignore
            key.ToString())
        |> Seq.mapi (fun i (_, items) ->
            let items = items |> Seq.toArray
            let oldIds = items |> Array.map (fun (_, si) -> si)
            let (reprCalEntry, reprCalExcs), _ = items |> Array.head
            // Put the bitmap string in the ID to make manual debugging easier
            let bitmapStr =
                reprCalEntry
                |> Option.map (fun ce ->
                    ce.weekdayService
                    |> Array.map (fun b -> if b then '1' else '0')
                    |> System.String)
                |> Option.defaultValue "exc"
            let newId = $"gtfs:service:{bitmapStr}:{i}"

            oldIds,
            newId,
            reprCalEntry |> Option.map (fun ce -> { ce with id = newId }),
            reprCalExcs |> Array.map (fun ce -> { ce with id = newId })
        )
        |> Seq.toArray
    let idMap =
        deduplicated
        |> Array.collect (fun (os, n, _, _) ->
            os |> Array.map (fun o -> o, n))
        |> Map
    { feed with
        trips =
            feed.trips
            |> Array.map (fun t -> { t with serviceId = idMap[t.serviceId] })
        calendar =
            Some <| (deduplicated |> Array.choose (fun (_, _, ce, _) -> ce))
        calendarExceptions =
            Some <| (deduplicated |> Array.collect (fun (_, _, _, ces) -> ces))
    }
