// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// JDF service calendars as GTFS calendars.
module JrUtil.JdfCalendar

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
    DateUtils.dateRange route.timetableValidFrom route.timetableValidTo
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
    DateUtils.dateRange calendar.startDate calendar.endDate
    |> Seq.map (fun d ->
        let dow = LanguagePrimitives.EnumToValue d.DayOfWeek - 1
        calendar.weekdayService.[dow])
    |> Seq.toArray

type CalendarPreparation = {
    tripsToDelete: Set<string>
    // Equal schedules share one exception array. A national trip-date object
    // collection is never needed by streaming compilation.
    schedules: Map<string, GtfsModel.CalendarEntry option * GtfsModel.CalendarException array>
}

// Production sinks use the distinct schedules and never expand per trip.
let internal materializeUniqueCalendars (prepared: CalendarPreparation) =
    let unique = Dictionary<GtfsModel.CalendarEntry option * GtfsModel.CalendarException array, string>(HashIdentity.Structural)
    let services = Dictionary<string, string>(StringComparer.Ordinal)
    let calendar = ResizeArray<GtfsModel.CalendarEntry>()
    let exceptions = ResizeArray<GtfsModel.CalendarException>()
    // Map iteration matches v1's ordinal service-ID traversal, preserving IDs.
    for KeyValue(trip, schedule) in prepared.schedules do
        match unique.TryGetValue(schedule) with
        | true, service -> services.Add(trip, service)
        | _ ->
            let entry, changes = schedule
            let bitmap = entry |> Option.map (fun value -> value.weekdayService |> Array.map (fun active -> if active then '1' else '0') |> String) |> Option.defaultValue "exc"
            let service = $"jdf:service:{bitmap}:{unique.Count}"
            unique.Add(schedule, service)
            services.Add(trip, service)
            entry |> Option.iter (fun value -> calendar.Add({ value with id = service }))
            for change in changes do exceptions.Add({ change with id = service })
    services, calendar.ToArray(), exceptions.ToArray()

// Computes the expensive per-trip service bitmap exactly once. Bundle
// conversion carries this result through international filtering instead of
// rebuilding every bitmap for the retained batch.
let prepareGtfsCalendarWithWorkersAndProgress
        maximumWorkers
        (progress: int64 -> int64 option -> unit)
        (jdfBatch: JdfModel.JdfBatch) =
    if maximumWorkers <= 0 then invalidArg "maximumWorkers" "Calendar worker count must be positive"
    let notesByTrip =
        jdfBatch.serviceNotes
        |> Array.groupBy (fun sn -> sn.routeId, sn.routeDistinction, sn.tripId)
        |> Map

    let routeById =
        jdfBatch.routes
        |> Array.map (fun r -> (r.id, r.idDistinction), r)
        |> Map

    let calculate (jdfTrip: JdfModel.Trip) =
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
        let tripId = jdfTripId jdfBatch jdfTrip.routeId
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
            [|tripId|], [||], [||]
        elif bitmapDiffCount > bitmapTrueCount then
            [||], [||],
            bitmap
            |> Array.indexed
            |> Array.choose (fun (i, s) ->
                if s then Some ({
                    id = tripId
                    date = jdfRoute.timetableValidFrom.PlusDays(i)
                    exceptionType = GtfsModel.ServiceAdded
                }: GtfsModel.CalendarException)
                else None)
        else
            [||], [|calendarEntry|],
            Seq.zip bitmap calendarBitmap
            |> Seq.indexed
            |> Seq.choose (fun (i, (s, sc)) ->
                if s <> sc then Some ({
                    id = tripId
                    date = jdfRoute.timetableValidFrom.PlusDays(i)
                    exceptionType = if s then GtfsModel.ServiceAdded
                                    else GtfsModel.ServiceRemoved
                }: GtfsModel.CalendarException)
                else None)
            |> Seq.toArray
    let mutable calculatedTrips = 0L
    let calendarProgressLock = obj()
    let mutable lastCalendarProgress = 0L
    let calculateGroup (group: (int * JdfModel.Trip) array) =
        let value = calculate (snd group.[0])
        let count = Interlocked.Add(&calculatedTrips, int64 group.Length)
        if count - lastCalendarProgress >= 5_000L || count = int64 jdfBatch.trips.Length then
            lock calendarProgressLock (fun () ->
                if count > lastCalendarProgress then
                    lastCalendarProgress <- count
                    progress count (Some (int64 jdfBatch.trips.Length)))
        value
    let scheduleInput (trip: JdfModel.Trip) =
        let notes =
            notesByTrip |> Map.tryFind (trip.routeId, trip.routeDistinction, trip.id)
            |> Option.defaultValue [||]
            |> Array.choose (fun note -> note.noteType |> Option.map (fun kind -> kind, note.dateFrom, note.dateTo))
            |> Array.sort
        // These are exactly the inputs consumed by tripDateBitmap. Source IDs,
        // labels and free-text notices do not change service dates.
        trip.routeId, trip.routeDistinction, trip.attributes, notes
    let tripsToDelete = ResizeArray<string>()
    let unique = Dictionary<GtfsModel.CalendarEntry option * GtfsModel.CalendarException array,
                            GtfsModel.CalendarEntry option * GtfsModel.CalendarException array>(HashIdentity.Structural)
    let schedules = ResizeArray<string * (GtfsModel.CalendarEntry option * GtfsModel.CalendarException array)>()
    for chunk in jdfBatch.trips |> Seq.chunkBySize 4096 do
        let values = Array.zeroCreate chunk.Length
        let groups =
            chunk |> Array.mapi (fun index trip -> index, trip)
            |> Array.groupBy (snd >> scheduleInput)
            |> Array.map snd
        let calculateAt index =
            let group = groups.[index]
            let result = calculateGroup group
            for offset, _ in group do values.[offset] <- result
        if maximumWorkers = 1 then
            for index = 0 to groups.Length - 1 do calculateAt index
        else
            Parallel.For(0, groups.Length, ParallelOptions(MaxDegreeOfParallelism = maximumWorkers), calculateAt) |> ignore
        for index = 0 to values.Length - 1 do
            let deleted, entries, tripExceptions = values.[index]
            if deleted.Length > 0 then
                let trip = chunk.[index]
                tripsToDelete.Add(jdfTripId jdfBatch trip.routeId trip.routeDistinction trip.id)
            if deleted.Length = 0 then
                let entry = entries |> Array.tryHead |> Option.map (fun value -> { value with id = "" })
                let changes = tripExceptions |> Array.map (fun value -> { value with id = "" }) |> Array.sort
                let candidate = entry, changes
                let shared =
                    match unique.TryGetValue(candidate) with
                    | true, existing -> existing
                    | _ -> unique.Add(candidate, candidate); candidate
                let trip = chunk.[index]
                schedules.Add(jdfTripId jdfBatch trip.routeId trip.routeDistinction trip.id, shared)
    {
        tripsToDelete = set tripsToDelete
        schedules = Map schedules
    }

let prepareGtfsCalendarWithWorkers maximumWorkers (jdfBatch: JdfModel.JdfBatch) =
    prepareGtfsCalendarWithWorkersAndProgress maximumWorkers (fun _ _ -> ()) jdfBatch

let prepareGtfsCalendar (jdfBatch: JdfModel.JdfBatch) =
    prepareGtfsCalendarWithWorkers 1 jdfBatch

let filterCalendarPreparation (batch: JdfModel.JdfBatch)
                              (prepared: CalendarPreparation) =
    let retained =
        batch.trips
        |> Seq.map (fun trip -> jdfTripId batch trip.routeId trip.routeDistinction trip.id)
        |> HashSet
    {
        tripsToDelete = prepared.tripsToDelete |> Set.filter retained.Contains
        schedules = prepared.schedules |> Map.filter (fun key _ -> retained.Contains key)
    }
