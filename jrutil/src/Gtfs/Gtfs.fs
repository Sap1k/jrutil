// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2025 David Koňařík

module JrUtil.Gtfs

open System.IO

open JrUtil.GtfsCsvSerializer
open JrUtil.GtfsModel
open JrUtil.GtfsParser

let gtfsStandardTablesToFolder () =
    // This is an attempt at speeding serialization up. In the end it didn't do
    // much, but this should be faster so I'm leaving it like this
    let agencySerializer = getRowsSerializerWriter<Agency>
    let stopSerializer = getRowsSerializerWriter<Stop>
    let routeSerializer = getRowsSerializerWriter<Route>
    let tripSerializer = getRowsSerializerWriter<Trip>
    let stopTimeSerializer = getRowsSerializerWriter<StandardStopTime>
    let calendarEntrySerializer = getRowsSerializerWriter<CalendarEntry>
    let calendarExceptionSerializer = getRowsSerializerWriter<CalendarException>
    let feedInfoSerializer = getRowsSerializerWriter<FeedInfo>
    fun path feed ->
        Directory.CreateDirectory(path) |> ignore
        let serializeTo name ser obj =
            let filePath = Path.Combine(path, name)
            use file = File.Open(filePath, FileMode.Create)
            ser file obj
        let serializeToOpt name ser obj =
            obj |> Option.iter (fun o -> serializeTo name ser o)
        serializeTo "agency.txt" agencySerializer feed.agencies
        serializeTo "stops.txt" stopSerializer feed.stops
        serializeTo "routes.txt" routeSerializer feed.routes
        serializeTo "trips.txt" tripSerializer feed.trips
        let standardStopTimes =
            feed.stopTimes
            |> Array.map (fun stopTime ->
                {
                    tripId = stopTime.tripId
                    arrivalTime = stopTime.arrivalTime
                    departureTime = stopTime.departureTime
                    stopId = stopTime.stopId
                    stopSequence = stopTime.stopSequence
                    headsign = stopTime.headsign
                    pickupType = stopTime.pickupType
                    dropoffType = stopTime.dropoffType
                    shapeDistTraveled = stopTime.shapeDistTraveled
                    timepoint = stopTime.timepoint
                }: StandardStopTime)
        serializeTo "stop_times.txt" stopTimeSerializer standardStopTimes
        serializeToOpt "calendar.txt" calendarEntrySerializer feed.calendar
        serializeToOpt "calendar_dates.txt"
                       calendarExceptionSerializer
                       feed.calendarExceptions
        match feed.feedInfo with
        | Some fi -> serializeTo "feed_info.txt" feedInfoSerializer [fi]
        | _ -> ()

let gtfsExtensionsToFolder () =
    let czRouteSerializer = getRowsSerializerWriter<CzRoute>
    let czTripSerializer = getRowsSerializerWriter<CzTrip>
    let czStopSerializer = getRowsSerializerWriter<CzStop>
    let czStopZoneSerializer = getRowsSerializerWriter<CzStopZone>
    fun path feed ->
        Directory.CreateDirectory(path) |> ignore
        let serializeToOpt name ser obj =
            obj |> Option.iter (fun rows ->
                let filePath = Path.Combine(path, name)
                use file = File.Open(filePath, FileMode.Create)
                ser file rows)
        serializeToOpt "cz_routes.txt" czRouteSerializer feed.czRoutes
        serializeToOpt "cz_trips.txt" czTripSerializer feed.czTrips
        serializeToOpt "cz_stops.txt" czStopSerializer feed.czStops
        serializeToOpt "cz_stop_zones.txt" czStopZoneSerializer feed.czStopZones

let gtfsFeedToFolder () =
    let standardSerializer = gtfsStandardTablesToFolder ()
    let extensionSerializer = gtfsExtensionsToFolder ()
    fun path feed ->
        standardSerializer path feed
        extensionSerializer path feed

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
    let czRoutesParser = fileParserOpt "cz_routes.txt"
    let czTripsParser = fileParserOpt "cz_trips.txt"
    let czStopsParser = fileParserOpt "cz_stops.txt"
    let czStopZonesParser = fileParserOpt "cz_stop_zones.txt"

    fun path ->
        let feed: GtfsFeed = {
            agencies = agenciesParser path
            stops = stopsParser path
            routes = routesParser path
            trips = tripsParser path
            stopTimes = stopTimesParser path
            calendar = calendarParser path
            calendarExceptions = calendarExceptionsParser path
            feedInfo =
                feedInfoParser path
                |> Option.map (fun fi -> fi.[0])
            czRoutes = czRoutesParser path
            czTrips = czTripsParser path
            czStops = czStopsParser path
            czStopZones = czStopZonesParser path
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
        |> Seq.groupBy fst
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
