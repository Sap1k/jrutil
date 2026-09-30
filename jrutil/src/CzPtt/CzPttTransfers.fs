// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023-2026 David Koňařík and contributors

/// CZPTT in-message and cross-PA transfers.
module JrUtil.CzPttTransfers

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
open JrUtil.CzPttModel
open JrUtil.CzPttNormalize
open JrUtil.CzPttEntities

let internal transfers message (journeys: Journey array) =
    let diagnostics = ResizeArray<string>()
    let values =
        journeys
        |> Array.pairwise
        |> Array.choose (fun (fromJourney, toJourney) ->
            let modeChange =
                fromJourney.alternativeTransport <> toJourney.alternativeTransport
            let fromBoundaryStop, toBoundaryStop =
                if modeChange then
                    fromJourney.calls
                    |> Array.tryLast
                    |> Option.map (journeyStopId fromJourney),
                    toJourney.calls
                    |> Array.tryHead
                    |> Option.map (journeyStopId toJourney)
                else None, None
            let sameStation =
                not modeChange
                || pointIdentity fromJourney.calls.[fromJourney.calls.Length - 1]
                    = pointIdentity toJourney.calls.[0]
            if not sameStation then
                let fromIdentity =
                    fromJourney.calls.[fromJourney.calls.Length - 1]
                    |> pointIdentity
                    |> pointIdentityText
                let toIdentity =
                    toJourney.calls.[0] |> pointIdentity |> pointIdentityText
                diagnostics.Add(
                    $"{paId message}: omitted cross-station NAD transfer " +
                    $"{tripId message fromJourney} ({fromIdentity}) -> " +
                    $"{tripId message toJourney} ({toIdentity})")
                None
            else
                Some {
                    fromStopId = fromBoundaryStop
                    toStopId = toBoundaryStop
                    fromRouteId = None
                    toRouteId = None
                    fromTripId = Some (tripId message fromJourney)
                    toTripId = Some (tripId message toJourney)
                    transferType = if modeChange then 1 else 4
                    minTransferTime = if modeChange then Some 0 else None
                    maxWaitingTime = None
                })
    values, diagnostics.ToArray()

type internal OrdinaryConnectionCall = {
    message: CzPttXml.CzpttcisMessage
    journey: Journey
    call: NormalizedCall
}

let internal calendarsOverlap (left: CzPttXml.CzpttcisMessage)
                             (right: CzPttXml.CzpttcisMessage) =
    let calendar (message: CzPttXml.CzpttcisMessage) =
        let value = message.CzpttInformation.PlannedCalendar
        LocalDate.FromDateTime(value.ValidityPeriod.StartDateTime), value.BitmapDays
    let leftStart, leftDays = calendar left
    let rightStart, rightDays = calendar right
    if String.IsNullOrEmpty(leftDays) || String.IsNullOrEmpty(rightDays) then false
    else
        let overlapStart = max leftStart rightStart
        let overlapEnd =
            min
                (leftStart.PlusDays(leftDays.Length - 1))
                (rightStart.PlusDays(rightDays.Length - 1))
        if overlapEnd < overlapStart then false
        else
            let leftOffset =
                Period.Between(leftStart, overlapStart, PeriodUnits.Days).Days
            let rightOffset =
                Period.Between(rightStart, overlapStart, PeriodUnits.Days).Days
            let length =
                Period.Between(overlapStart, overlapEnd, PeriodUnits.Days).Days + 1
            seq { 0 .. length - 1 }
            |> Seq.exists (fun offset ->
                leftDays.[leftOffset + offset] = '1'
                && rightDays.[rightOffset + offset] = '1')

let internal trainNumberValue message journey =
    trainNumberForCalls message journey.calls
    |> Option.bind (fun value ->
        match Int64.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | true, number -> Some number
        | _ -> None)

let internal crossPaTransfers
        (accepted:
            seq<CzPttXml.CzpttcisMessage * NormalizedCall array *
                NormalizedCall array * Journey array>) =
    let nadEndpointStations =
        accepted
        |> Seq.collect (fun (_, _, _, generatedJourneys) ->
            generatedJourneys
            |> Seq.mapi (fun index journey -> index, journey)
            |> Seq.filter (fun (_, journey) -> journey.alternativeTransport)
            |> Seq.collect (fun (index, journey) ->
                let passengerCalls =
                    journey.calls |> Array.filter (fun call -> call.passenger)
                if passengerCalls.Length = 0 then Seq.empty
                else
                    seq {
                        if index = 0 then yield stationId passengerCalls.[0]
                        if index = generatedJourneys.Length - 1 then
                            yield stationId passengerCalls.[passengerCalls.Length - 1]
                    }))
        |> Set
    let byLine =
        Collections.Generic.Dictionary<
            string * string, ResizeArray<OrdinaryConnectionCall>>()
    let byNumber =
        Collections.Generic.Dictionary<
            string * int64, ResizeArray<OrdinaryConnectionCall>>()
    let addCandidate (index: Collections.Generic.Dictionary<'key, ResizeArray<_>>)
                     key candidate =
        match index.TryGetValue(key) with
        | true, values -> values.Add(candidate)
        | false, _ ->
            let values = ResizeArray()
            values.Add(candidate)
            index.Add(key, values)
    for message, _, _, generatedJourneys in accepted do
        for journey in generatedJourneys do
            if not journey.alternativeTransport then
                let number = trainNumberValue message journey
                for call in journey.calls do
                    let station = stationId call
                    if call.passenger && Set.contains station nadEndpointStations then
                        let candidate = {
                            message = message
                            journey = journey
                            call = call
                        }
                        call.lineCode
                        |> Option.iter (fun line ->
                            addCandidate byLine (station, line) candidate)
                        number
                        |> Option.iter (fun value ->
                            addCandidate byNumber (station, value) candidate)

    let matchingOrdinaryCalls nadMessage nadJourney (nadCall: NormalizedCall) =
        let station = stationId nadCall
        let values lookup =
            match lookup with
            | true, (items: ResizeArray<OrdinaryConnectionCall>) -> items.ToArray()
            | false, _ -> [||]
        let numberMatches =
            trainNumberValue nadMessage nadJourney
            |> Option.filter (fun number -> number >= 300000L)
            |> Option.map (fun number ->
                values (byNumber.TryGetValue((station, number - 300000L))))
            |> Option.defaultValue [||]
        let lineMatches =
            nadCall.lineCode
            |> Option.map (fun line -> values (byLine.TryGetValue((station, line))))
            |> Option.defaultValue [||]
        numberMatches, lineMatches

    let nonOverlappingClosest candidates =
        let closestByTrip =
            candidates
            |> Seq.groupBy (fun (gap, candidate) ->
                tripId candidate.message candidate.journey)
            |> Seq.map (fun (_, values) -> Seq.minBy fst values)
            |> Seq.sortBy (fun (gap, candidate) ->
                gap, tripId candidate.message candidate.journey)
            |> Seq.toArray
        closestByTrip
        |> Array.filter (fun (gap, candidate) ->
            closestByTrip
            |> Array.exists (fun (otherGap, other) ->
                tripId other.message other.journey
                    <> tripId candidate.message candidate.journey
                && otherGap <= gap
                && calendarsOverlap candidate.message other.message)
            |> not)
        |> Array.map snd

    let transfer fromMessage fromJourney fromCall
                 toMessage toJourney toCall =
        {
            fromStopId = Some (journeyStopId fromJourney fromCall)
            toStopId = Some (journeyStopId toJourney toCall)
            fromRouteId = None
            toRouteId = None
            fromTripId = Some (tripId fromMessage fromJourney)
            toTripId = Some (tripId toMessage toJourney)
            transferType = 1
            minTransferTime = Some 0
            maxWaitingTime = None
        }

    accepted
    |> Seq.collect (fun (nadMessage, _, _, generatedJourneys) ->
        generatedJourneys
        |> Seq.mapi (fun index journey -> index, journey)
        |> Seq.filter (fun (_, journey) -> journey.alternativeTransport)
        |> Seq.collect (fun (journeyIndex, nadJourney) ->
            let passengerCalls =
                nadJourney.calls |> Array.filter (fun call -> call.passenger)
            if passengerCalls.Length = 0 then Seq.empty
            else
                let firstCall = passengerCalls.[0]
                let lastCall = passengerCalls.[passengerCalls.Length - 1]
                let matchOrdinary nadCall ordinaryTime gapFor allowed =
                    let eligible candidates =
                        candidates
                        |> Seq.filter (fun ordinary ->
                            paId ordinary.message <> paId nadMessage
                            && allowed ordinary)
                        |> Seq.choose (fun ordinary ->
                            ordinaryTime ordinary.call
                            |> Option.bind (fun time ->
                                let gap = gapFor time
                                if gap >= 0 && gap <= 600
                                then Some (gap, ordinary) else None))
                        |> Seq.filter (fun (_, ordinary) ->
                            calendarsOverlap nadMessage ordinary.message)
                        |> Seq.toArray
                    let numberMatches, lineMatches =
                        matchingOrdinaryCalls nadMessage nadJourney nadCall
                    let eligibleNumbers = eligible numberMatches
                    if eligibleNumbers.Length > 0 then
                        nonOverlappingClosest eligibleNumbers
                    else
                        lineMatches |> eligible |> nonOverlappingClosest
                let before =
                    if journeyIndex > 0
                       || hasActivity DisembarkOnly firstCall.location then [||]
                    else
                        firstCall.departure
                        |> Option.orElse firstCall.arrival
                        |> Option.map (fun departure ->
                            matchOrdinary
                                firstCall
                                (fun call -> call.arrival |> Option.orElse call.departure)
                                (fun arrival -> departure - arrival)
                                (fun ordinary ->
                                    ordinary.journey.calls.[0].sourceIndex
                                        < ordinary.call.sourceIndex
                                    && not (
                                        hasActivity EmbarkOnly ordinary.call.location))
                            |> Array.map (fun ordinary ->
                                transfer
                                    ordinary.message ordinary.journey ordinary.call
                                    nadMessage nadJourney firstCall))
                        |> Option.defaultValue [||]
                let after =
                    if journeyIndex < generatedJourneys.Length - 1
                       || hasActivity EmbarkOnly lastCall.location then [||]
                    else
                        lastCall.arrival
                        |> Option.orElse lastCall.departure
                        |> Option.map (fun arrival ->
                            matchOrdinary
                                lastCall
                                (fun call -> call.departure |> Option.orElse call.arrival)
                                (fun departure -> departure - arrival)
                                (fun ordinary ->
                                    ordinary.call.sourceIndex
                                        < ordinary.journey.calls.[ordinary.journey.calls.Length - 1].sourceIndex
                                    && not (
                                        hasActivity DisembarkOnly ordinary.call.location))
                            |> Array.map (fun ordinary ->
                                transfer
                                    nadMessage nadJourney lastCall
                                    ordinary.message ordinary.journey ordinary.call))
                        |> Option.defaultValue [||]
                Seq.append before after))
    |> Seq.distinct
    |> Seq.toArray
