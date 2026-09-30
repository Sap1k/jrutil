// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023-2026 David Koňařík and contributors

/// CZPTT GTFS entities: stops, routes, trips and stop times.
module JrUtil.CzPttEntities

open System
open System.Globalization
open System.Text.Json
open System.Text.RegularExpressions
open FSharp.Data
open NodaTime
open NodaTime.Text
open Serilog
open JrUtil.CzPtt
open JrUtil.CzPttMerge
open JrUtil.GtfsModel
open JrUtil.Utils
open JrUtil.CzPttModel
open JrUtil.CzPttNormalize

let internal pointIdentityText (countryCode, primaryCode) =
    $"{idComponent countryCode}:{idComponent primaryCode}"

let internal stationIdFor identity =
    $"czptt:stop:{pointIdentityText identity}"

let internal stationId (call: NormalizedCall) =
    stationIdFor (pointIdentity call)

let internal stopId (call: NormalizedCall) =
    let parent = stationId call
    match nullOpt call.location.Location.LocationSubsidiaryIdentification with
    | None -> $"{parent}:unspecified"
    | Some subsidiary ->
        $"{parent}:platform:{idComponent subsidiary.LocationSubsidiaryCode.Value}"

let internal busStopId (call: NormalizedCall) =
    $"{stationId call}:platform:BUS"

let internal journeyStopId (journey: Journey) (call: NormalizedCall) =
    if journey.alternativeTransport then busStopId call else stopId call

let internal busStopDescription =
    "Pro přesné informace k nástupišti náhradní dopravy sledujte informace dopravce."

let internal stopName (pointNames: PointNameIndex) (call: NormalizedCall) =
    pointNames
    |> Map.tryFind (pointIdentity call)
    |> Option.defaultWith (fun () ->
        call.location.Location.PrimaryLocationName
        |> nullOpt
        |> Option.defaultValue "")

let internal pointDisplayName pointNames (call: NormalizedCall) =
    stopName pointNames call
    |> Option.ofObj
    |> Option.filter (String.IsNullOrWhiteSpace >> not)
    |> Option.defaultValue (
        let country, code = pointIdentity call
        $"{country} {code}")

let internal publicEndpoints (calls: NormalizedCall array) =
    let passengerCalls = calls |> Array.filter (fun call -> call.passenger)
    passengerCalls.[0], passengerCalls.[passengerCalls.Length - 1]

let internal stops pointNames (journeyCalls: (Journey * NormalizedCall) seq) =
    journeyCalls
    |> Seq.collect (fun (journey, call) ->
        let create id locationType parent platform description = {
            id = id
            code = None
            name = stopName pointNames call
            description = description
            lat = None
            lon = None
            zoneId = None
            url = None
            locationType = Some locationType
            parentStation = parent
            timezone = if locationType = Station then Some "Europe/Prague" else None
            wheelchairBoarding = None
            platformCode = platform
        }
        let platform =
            nullOpt call.location.Location.LocationSubsidiaryIdentification
            |> Option.map (fun subsidiary ->
                nullOpt subsidiary.LocationSubsidiaryName
                |> Option.filter (String.IsNullOrWhiteSpace >> not)
                |> Option.defaultValue subsidiary.LocationSubsidiaryCode.Value)
        seq {
            create (stationId call) Station None None None
            if journey.alternativeTransport then
                create
                    (busStopId call) Stop (Some (stationId call))
                    (Some "BUS") (Some busStopDescription)
            else
                create
                    (stopId call) Stop (Some (stationId call)) platform None
        })
    |> Seq.sortByDescending (fun stop -> stop.description.IsSome)
    |> Seq.distinctBy (fun stop -> stop.id)
    |> Seq.sortBy (fun stop -> stop.id)
    |> Seq.toArray

let internal lineForDate (catalog: CatalogSnapshot) (date: LocalDate) (code: string) =
    catalog.lines
    |> Array.filter (fun line ->
        line.code = code
        && (line.validFrom |> Option.forall (fun startDate -> startDate <= date))
        && (line.validTo |> Option.forall (fun endDate -> date <= endDate)))
    |> Array.sortBy (fun line -> line.validFrom)
    |> Array.tryLast

type internal JourneyLabel = {
    identity: string
    display: string
    mappedLine: CatalogLine option
}

let internal trainNumberForCalls (message: CzPttXml.CzpttcisMessage)
                                (calls: NormalizedCall array) =
    calls
    |> Array.choose (fun call -> nullOpt call.location.OperationalTrainNumber)
    |> Array.tryHead
    |> Option.orElseWith (fun () -> nullOpt (trIdentifier message).Core)

let internal labelsForJourney catalog date message (journey: Journey) =
    journey.parts
    |> Seq.map (fun part ->
        let mapped = part.lineCode |> Option.bind (lineForDate catalog date)
        let withNadSuffix value =
            if journey.alternativeTransport then value + " (NAD)" else value
        match part.lineCode, mapped with
        | Some code, Some line -> {
            identity = $"line:{code}"
            display = withNadSuffix line.mark
            mappedLine = Some line
          }
        | _ ->
            let number = trainNumberForCalls message part.calls
            let numberKey = number |> Option.defaultValue ""
            let display =
                number
                |> Option.map (fun value -> $"{part.category} {value}")
                |> Option.defaultValue part.category
            {
                identity = $"train:{part.category}:{numberKey}"
                display = withNadSuffix display
                mappedLine = None
            })
    |> distinctInOrder

let internal routeId catalog date message (journey: Journey) =
    let segment = journey.parts |> Array.exactlyOne
    match segment.lineCode with
    | Some code when lineForDate catalog date code |> Option.isSome ->
        $"czptt:route:line:{idComponent code}:" +
        $"{idComponent segment.responsibleRu}:{idComponent segment.routeType}"
    | _ ->
        let number = trainNumberForCalls message segment.calls
        $"czptt:route:fallback:{idComponent segment.responsibleRu}:" +
        $"{idComponent segment.category}:{idComponent segment.routeType}:" +
        $"{idComponent (number |> Option.defaultValue segment.category)}"

let internal tripId (message: CzPttXml.CzpttcisMessage) (journey: Journey) =
    $"czptt:trip:{idComponent (paId message)}:{journey.index + 1}"

let internal serviceId (message: CzPttXml.CzpttcisMessage) =
    $"czptt:service:{idComponent (paId message)}:calendar:journey"
let internal blockId (message: CzPttXml.CzpttcisMessage) =
    $"czptt:block:{idComponent (paId message)}"
let internal agencyId (code: string) = $"czptt:agency:{idComponent code}"

let internal tripShortName (message: CzPttXml.CzpttcisMessage)
                          (journey: Journey) =
    let category = journey.calls.[0].category
    trainNumberForCalls message journey.calls
    |> Option.map (fun number -> $"{category} {number}")
    |> Option.orElse (Some category)

let internal calendarExceptions (message: CzPttXml.CzpttcisMessage) =
    let calendar = message.CzpttInformation.PlannedCalendar
    let startDate = LocalDate.FromDateTime(calendar.ValidityPeriod.StartDateTime)
    calendar.BitmapDays
    |> Seq.mapi (fun index active -> index, active)
    |> Seq.choose (fun (index, active) ->
        if active = '1' then Some {
            id = serviceId message
            date = startDate.PlusDays(index)
            exceptionType = ServiceAdded
        } else None)
    |> Seq.toArray

let internal gtfsTimeShift (calls: NormalizedCall array) =
    let times =
        calls
        |> Seq.collect (fun call ->
            Seq.choose id [ call.arrival; call.departure ])
        |> Seq.toArray
    let minimum = if times.Length = 0 then 0 else Array.min times
    if minimum >= 0 then 0
    else ((-minimum + 86399) / 86400) * 86400

let internal stopTimes message shift journey =
    journey.calls
    |> Array.map (fun call ->
        let request = hasActivity RequestStop call.location
        let regular =
            if request then CoordinationWithDriver else RegularlyScheduled
        {
            tripId = tripId message journey
            arrivalTime =
                call.arrival
                |> Option.map (fun value ->
                    Period.FromSeconds(int64 (value + shift)))
            departureTime =
                call.departure
                |> Option.map (fun value ->
                    Period.FromSeconds(int64 (value + shift)))
            stopId = journeyStopId journey call
            stopSequence = call.sourceIndex + 1
            headsign = None
            pickupType =
                if not call.passenger || hasActivity DisembarkOnly call.location
                then Some NoService else Some regular
            dropoffType =
                if not call.passenger || hasActivity EmbarkOnly call.location
                then Some NoService else Some regular
            shapeDistTraveled = None
            timepoint =
                if call.inferredTiming then Some Approximate
                elif call.arrival.IsSome || call.departure.IsSome
                then Some Exact
                else Some Approximate
            stopZoneIds = None
        })

let internal routeColor routeType =
    match routeType with
    | "106" -> "1c1745"
    | "102" -> "b91c1c"
    | "103" -> "b45309"
    | "105" -> "4c1d95"
    | _ -> "475569"

let internal journeyAgencyId journey = agencyId journey.responsibleRu

let internal route catalog date message journey =
    let labels = labelsForJourney catalog date message journey
    let mappedLongName =
        match labels with
        | [| label |] ->
            label.mappedLine |> Option.map (fun line -> line.name.TrimEnd())
        | _ -> None
    {
        id = routeId catalog date message journey
        agencyId = Some (journeyAgencyId journey)
        shortName =
            labels
            |> Array.map (fun label -> label.display)
            |> distinctInOrder
            |> String.concat "/"
            |> Some
        longName = mappedLongName
        description = None
        routeType = journey.routeType
        url = None
        color = Some (routeColor journey.routeType)
        textColor = Some "ffffff"
        sortOrder = None
    }

let internal trip pointNames catalog date message sourceCalls endpoints
                 separateModeBlocks journey =
    let finalCall = snd endpoints
    let wheelchairAccessible, bikesAllowed, _ =
        projectedTripFeatures message sourceCalls journey
    {
        routeId = routeId catalog date message journey
        serviceId = serviceId message
        id = tripId message journey
        headsign = Some (pointDisplayName pointNames finalCall)
        shortName = tripShortName message journey
        directionId = None
        blockId =
            if separateModeBlocks
            then Some ($"{blockId message}:mode:{journey.modeBlockIndex + 1}")
            else Some (blockId message)
        shapeId = None
        wheelchairAccessible = wheelchairAccessible
        bikesAllowed = bikesAllowed
    }
