// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023-2026 David Koňařík and contributors

module JrUtil.CzPttToGtfs

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
open JrUtil.CzPttEntities
open JrUtil.CzPttTransfers
open JrUtil.CzPttAgencies


let convert catalog options pointNames
                          (messages: CzPttXml.CzpttcisMessage seq) =
    let noteContexts =
        ResizeArray<CzPttXml.CzpttcisMessage * NormalizedCall array>()
    let accepted =
        ResizeArray<
            CzPttXml.CzpttcisMessage * NormalizedCall array *
            NormalizedCall array * Journey array>()
    let rejected = ResizeArray<RejectedJourney>()
    let approximations = ResizeArray<string>()
    let boundaryAdjustments = ResizeArray<BoundaryAdjustment>()
    for message in messages |> Seq.sortBy paId do
        let calls, timingDiagnostics =
            normalize catalog message |> forceMonotonicFlaggedTimes message
        approximations.AddRange(timingDiagnostics)
        noteContexts.Add(message, calls)
        if not (calls |> Array.exists (fun call -> call.passenger)) then
            rejected.Add {
                paId = paId message
                reason = "no activity 0001 passenger call"
                sequence = None
                previousSeconds = None
                currentSeconds = None
            }
        else
            match chronologyError message calls with
            | Some error -> rejected.Add error
            | None ->
                let projected, adjustments =
                    projectPassengerServiceBoundaries message calls
                let selected =
                    selectedCalls options.operationalPointMode projected
                if selected.Length < 2 then
                    rejected.Add {
                        paId = paId message
                        reason = "fewer than two selected GTFS calls"
                        sequence = None
                        previousSeconds = None
                        currentSeconds = None
                    }
                else
                    boundaryAdjustments.AddRange(adjustments)
                    let journeySegments = selected |> segments
                    let rawGeneratedJourneys =
                        journeys journeySegments
                        |> Array.choose (fun journey ->
                            if journey.alternativeTransport then
                                let passengerCalls =
                                    journey.calls
                                    |> Array.filter (fun call -> call.passenger)
                                if passengerCalls.Length >= 2 then
                                    Some { journey with calls = passengerCalls }
                                else None
                            else Some journey)
                    let generatedJourneys, edgeDiagnostics =
                        reconcileJourneyEdgeTimes message calls rawGeneratedJourneys
                    approximations.AddRange(edgeDiagnostics)
                    if options.operationalPointMode = Sidecar then
                        let exactChanges =
                            calls
                            |> Array.pairwise
                            |> Array.choose (fun (left, right) ->
                                if left.responsibleRu <> right.responsibleRu
                                   && not right.passenger then
                                    Some right.sourceIndex
                                else None)
                        if exactChanges.Length > 0 then
                            let sourceIndex = exactChanges.[0]
                            approximations.Add(
                                $"{paId message}: operator boundary moved from source sequence " +
                                $"{sourceIndex + 1} to the following passenger call")
                    accepted.Add(message, calls, selected, generatedJourneys)

    let acceptedMessages =
        accepted |> Seq.map (fun (message, _, _, _) -> message) |> Seq.toArray
    let routes =
        accepted
        |> Seq.collect (fun (message, _, _, generatedJourneys) ->
            let date =
                LocalDate.FromDateTime(
                    message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)
            generatedJourneys
            |> Seq.map (route catalog date message))
        |> Seq.distinctBy (fun value -> value.id)
        |> Seq.sortBy (fun value -> value.id)
        |> Seq.toArray
    let generatedTrips =
        accepted
        |> Seq.collect (fun (message, calls, _, generatedJourneys) ->
            let date =
                LocalDate.FromDateTime(
                    message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)
            let endpoints = publicEndpoints calls
            let separateModeBlocks =
                generatedJourneys
                |> Array.pairwise
                |> Array.exists (fun (left, right) ->
                    left.alternativeTransport <> right.alternativeTransport)
            generatedJourneys
            |> Seq.map (fun journey ->
                message, journey,
                trip
                    pointNames catalog date message calls endpoints
                    separateModeBlocks journey))
        |> Seq.toArray
    let trips = generatedTrips |> Array.map (fun (_, _, value) -> value)
    let allJourneyCalls =
        accepted
        |> Seq.collect (fun (_, _, _, generatedJourneys) ->
            generatedJourneys
            |> Seq.collect (fun journey ->
                journey.calls |> Seq.map (fun call -> journey, call)))
        |> Seq.toArray
    let allStopTimes =
        accepted
        |> Seq.collect (fun (message, calls, _, generatedJourneys) ->
            let shift = gtfsTimeShift calls
            generatedJourneys |> Seq.collect (stopTimes message shift))
        |> Seq.toArray
    let internalTransferResults =
        accepted
        |> Seq.map (fun (message, _, _, generatedJourneys) ->
            transfers message generatedJourneys)
        |> Seq.toArray
    let internalTransfers =
        internalTransferResults |> Array.collect fst
    internalTransferResults
    |> Array.collect snd
    |> approximations.AddRange
    let preciseTransfers =
        Array.append internalTransfers (crossPaTransfers accepted)
    // MOTIS applies its configured default transfer time unless transfers.txt
    // contains a stop-pair minimum. It also ignores trip specificity for ordinary
    // transfers. Emit NAD boundaries directly as zero-second stop-pair minimums;
    // the synthetic BUS boarding point makes that broader scope NAD-only.
    let allTransfers =
        preciseTransfers
        |> Array.map (fun transfer ->
            match transfer.transferType, transfer.fromStopId, transfer.toStopId with
            | 1, Some fromStop, Some toStop ->
                {
                    fromStopId = Some fromStop
                    toStopId = Some toStop
                    fromRouteId = None
                    toRouteId = None
                    fromTripId = None
                    toTripId = None
                    transferType = 2
                    minTransferTime = Some 0
                    maxWaitingTime = None
                }
            | _ -> transfer)
        |> Array.distinct
        |> Array.sortBy (fun value ->
            value.fromTripId, value.toTripId, value.fromStopId, value.toStopId)
    let allCalendarExceptions =
        acceptedMessages |> Array.collect calendarExceptions
    let agencyCodes =
        accepted
        |> Seq.collect (fun (_, _, _, generatedJourneys) ->
            generatedJourneys |> Seq.map (fun journey -> journey.responsibleRu))
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toArray
    let publicLineRouteIds =
        accepted
        |> Seq.collect (fun (message, _, _, generatedJourneys) ->
            let date =
                LocalDate.FromDateTime(
                    message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)
            generatedJourneys
            |> Seq.choose (fun journey ->
                let hasMappedLine =
                    labelsForJourney catalog date message journey
                    |> Array.exists (fun label -> label.mappedLine.IsSome)
                if hasMappedLine then
                    Some (routeId catalog date message journey)
                else None))
        |> Set
    let czRoutes =
        routes
        |> Array.map (fun value -> {
            routeId = value.id
            cisLineId = None
            publicLineNumber =
                if Set.contains value.id publicLineRouteIds
                then value.shortName
                else None
            sourceProvenance = "czptt"
        })
    let segmentByCall =
        accepted
        |> Seq.collect (fun (message, _, _, generatedJourneys) ->
            generatedJourneys
            |> Seq.collect (fun journey ->
                journey.calls
                |> Seq.map (fun call ->
                    (paId message, call.sourceIndex), tripId message journey)))
        |> Seq.groupBy fst
        |> Seq.map (fun (key, values) ->
            key, values |> Seq.map snd |> Seq.distinct |> Seq.toArray)
        |> Map
    let idsDiagnostics = ResizeArray<string>()
    let generatedJourneysByPa =
        accepted
        |> Seq.map (fun (message, _, _, journeys) -> paId message, journeys)
        |> Map
    let noteId pa kind index =
        $"czptt:note:{idComponent pa}:{kind}:{index + 1}"
    let noteBounds message calendarId =
        match calendarId |> Option.bind (resolvedCalendarDates message) with
        | Some dates when not dates.IsEmpty -> Some (Set.minElement dates), Some (Set.maxElement dates)
        | _ -> None, None
    let catalogNoteLabel code =
        catalog.centralNotes
        |> Array.tryFind (fun note -> note.code = code)
        |> Option.map (fun note -> note.name)
    let intersectingTrips message firstIndex lastIndex generatedJourneys =
        match firstIndex, lastIndex with
        | Some first, Some last when first <= last ->
            generatedJourneys
            |> Array.filter (fun journey ->
                journey.calls
                |> Array.exists (fun call ->
                    call.sourceIndex >= first && call.sourceIndex <= last))
            |> Array.map (tripId message)
        | _ -> [||]
    let centralNotes =
        noteContexts
        |> Seq.collect (fun (message, calls) ->
            let generatedJourneys =
                Map.tryFind (paId message) generatedJourneysByPa
                |> Option.defaultValue [||]
            centralNoteSpecs message calls
            |> Seq.map (fun note ->
                let id = noteId (paId message) "central" note.index
                let resolved =
                    match note.firstIndex, note.lastIndex with
                    | Some first, Some last -> first <= last
                    | _ -> false
                if not resolved then
                    idsDiagnostics.Add(
                        $"{paId message}: unresolved CZCentralPTTNote interval {note.rawValue}")
                if note.calendarId.IsSome && not note.coversAllDates
                   && (note.calendarId |> Option.bind (resolvedCalendarDates message)).IsNone then
                    idsDiagnostics.Add(
                        $"{paId message}: unresolved CZCentralPTTNote calendar {note.rawValue}")
                let validFrom, validTo = noteBounds message note.calendarId
                { id = id; paId = paId message; kind = "czptt_central_note"
                  code = Some note.code; label = catalogNoteLabel note.code
                  rawValue = note.rawValue; validFrom = validFrom; validTo = validTo
                  tripIds = intersectingTrips message note.firstIndex note.lastIndex generatedJourneys
                  firstSequence = (if resolved then note.firstIndex |> Option.map ((+) 1) else None)
                  lastSequence = (if resolved then note.lastIndex |> Option.map ((+) 1) else None)
                  resolved = resolved }))
        |> Seq.toArray
    let nonCentralNotes =
        noteContexts
        |> Seq.collect (fun (message, calls) ->
            let generatedJourneys =
                Map.tryFind (paId message) generatedJourneysByPa
                |> Option.defaultValue [||]
            parameterValues "CZNonCentralPTTNote" message.NetworkSpecificParameter
            |> Seq.mapi (fun index raw ->
                let fields = raw.Split('|')
                let firstIndex, lastIndex =
                    if fields.Length < 4 then None, None
                    else
                        resolveNoteEndpoint fields.[0] (noteOccurrence fields.[1]) calls,
                        resolveNoteEndpoint fields.[2] (noteOccurrence fields.[3]) calls
                let calendarId =
                    fields
                    |> Array.tryItem 9
                    |> Option.filter (String.IsNullOrWhiteSpace >> not)
                let resolved =
                    match firstIndex, lastIndex with
                    | Some first, Some last -> first <= last
                    | _ -> false
                if not resolved then
                    idsDiagnostics.Add(
                        $"{paId message}: unresolved CZNonCentralPTTNote interval {raw}")
                if calendarId.IsSome
                   && (calendarId |> Option.bind (resolvedCalendarDates message)).IsNone then
                    idsDiagnostics.Add(
                        $"{paId message}: unresolved CZNonCentralPTTNote calendar {raw}")
                let validFrom, validTo = noteBounds message calendarId
                { id = noteId (paId message) "noncentral" index
                  paId = paId message; kind = "czptt_local_note"; code = None
                  label = fields |> Array.tryItem 4 |> Option.filter (String.IsNullOrWhiteSpace >> not)
                  rawValue = raw; validFrom = validFrom; validTo = validTo
                  tripIds = intersectingTrips message firstIndex lastIndex generatedJourneys
                  firstSequence = (if resolved then firstIndex |> Option.map ((+) 1) else None)
                  lastSequence = (if resolved then lastIndex |> Option.map ((+) 1) else None)
                  resolved = resolved }))
        |> Seq.toArray
    let calendarNotes =
        noteContexts
        |> Seq.collect (fun (message, _) ->
            parameterValues "CZCalendarPTTNote" message.NetworkSpecificParameter
            |> Seq.mapi (fun index raw ->
                let fields = raw.Split('|')
                let validFrom = fields |> Array.tryItem 1 |> Option.bind tryCompactDate
                let validTo = fields |> Array.tryItem 2 |> Option.bind tryCompactDate
                let resolved = fields.Length >= 4 && validFrom.IsSome && validTo.IsSome
                if not resolved then
                    idsDiagnostics.Add(
                        $"{paId message}: malformed CZCalendarPTTNote {raw}")
                { id = noteId (paId message) "calendar" index
                  paId = paId message; kind = "czptt_calendar_note"
                  code = fields |> Array.tryItem 0; label = None; rawValue = raw
                  validFrom = validFrom; validTo = validTo; tripIds = [||]
                  firstSequence = None; lastSequence = None
                  resolved = resolved }))
        |> Seq.toArray
    let allNotes = Array.concat [| centralNotes; nonCentralNotes; calendarNotes |]
    let featureKinds code =
        match code with
        | "17" -> [| "wheelchair_accessible_vehicle"; "wheelchair_booking_recommended" |]
        | "34" -> [| "wheelchair_accessible_vehicle"; "wheelchair_booking_required" |]
        | "22" -> [| "bicycle_transport"; "bicycle_carry_on" |]
        | "26" -> [| "bicycle_transport"; "bicycle_storage"; "bicycle_reservation_available" |]
        | "27" -> [| "bicycle_transport"; "bicycle_storage"; "bicycle_reservation_required" |]
        | "28" -> [| "bicycle_transport"; "bicycle_carry_on"; "bicycle_reservation_available" |]
        | "29" -> [| "bicycle_transport"; "bicycle_carry_on"; "bicycle_reservation_required" |]
        | "36" -> [| "bicycle_transport_prohibited" |]
        | _ -> [||]
    let noteFeatures =
        accepted
        |> Seq.collect (fun (message, calls, _, generatedJourneys) ->
            centralNoteSpecs message calls
            |> Seq.collect (fun note ->
                let kinds = featureKinds note.code
                if kinds.Length = 0 then Seq.empty
                else
                    match note.firstIndex, note.lastIndex with
                    | Some first, Some last when first <= last ->
                        generatedJourneys
                        |> Seq.collect (fun journey ->
                            if journey.alternativeTransport then Seq.empty
                            else
                                let coveredCalls =
                                    journey.calls
                                    |> Array.map (fun call -> call.sourceIndex + 1, call)
                                    |> Array.filter (fun (_, call) ->
                                        call.sourceIndex >= first && call.sourceIndex <= last)
                                if coveredCalls.Length = 0 then Seq.empty
                                else
                                    let wholeTrip =
                                        first <= journey.calls.[0].sourceIndex
                                        && last >= journey.calls.[journey.calls.Length - 1].sourceIndex
                                    kinds
                                    |> Seq.collect (fun kind ->
                                        let sequences =
                                            if wholeTrip then [| None |]
                                            else coveredCalls |> Array.map (fun (sequence, _) -> Some sequence)
                                        sequences
                                        |> Seq.map (fun sequence ->
                                            let sourceObject = noteId (paId message) "central" note.index
                                            let scopeKey = sequence |> Option.map string |> Option.defaultValue "trip"
                                            let generatedTrip = tripId message journey
                                            { id = $"{sourceObject}:feature:{kind}:trip:{idComponent generatedTrip}:{scopeKey}"
                                              tripId = generatedTrip; callSequence = sequence
                                              sourceCode = note.code; kind = kind; noteId = Some sourceObject
                                              sourceObjectId = sourceObject })))
                    | _ -> Seq.empty))
        |> Seq.toArray
    let requestFeatures =
        accepted
        |> Seq.collect (fun (message, _, _, generatedJourneys) ->
            generatedJourneys
            |> Seq.collect (fun journey ->
                journey.calls
                |> Seq.map (fun call -> call.sourceIndex + 1, call)
                |> Seq.filter (fun (_, call) -> hasActivity RequestStop call.location)
                |> Seq.map (fun (sequence, call) ->
                    let sourceObject = $"{paId message}:sequence:{call.sourceIndex + 1}:activity:0030"
                    let generatedTrip = tripId message journey
                    { id = $"{sourceObject}:feature:on_request:trip:{idComponent generatedTrip}"
                      tripId = generatedTrip; callSequence = Some sequence
                      sourceCode = "0030"; kind = "on_request"; noteId = None
                      sourceObjectId = sourceObject })))
        |> Seq.toArray
    let allFeatures = Array.append noteFeatures requestFeatures
    accepted
    |> Seq.iter (fun (message, calls, _, generatedJourneys) ->
        generatedJourneys
        |> Seq.iter (fun journey ->
            let _, _, conflict = projectedTripFeatures message calls journey
            if conflict then
                idsDiagnostics.Add(
                    $"{paId message}: conflicting whole-trip bicycle notes for {tripId message journey}")))
    let tripStopZones =
        accepted
        |> Seq.collect (fun (message, calls, _, _) ->
            let date =
                LocalDate.FromDateTime(
                    message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)
            parameterValues "CZIPTS" message.NetworkSpecificParameter
            |> Seq.collect (fun sourceValue ->
                let fields = sourceValue.Split('|')
                if fields.Length < 5 then
                    idsDiagnostics.Add(
                        $"{paId message}: malformed CZIPTS value {sourceValue}")
                    Seq.empty
                else
                    let code = fields.[0]
                    let startIndex =
                        resolveIntervalEndpoint
                            fields.[1] (parseOrdinal fields.[2]) calls
                    let endIndex =
                        resolveIntervalEndpoint
                            fields.[3] (parseOrdinal fields.[4]) calls
                    match startIndex, endIndex, validIdsRecord catalog date code with
                    | Some firstIndex, Some lastIndex, Some idsRecord
                        when firstIndex <= lastIndex ->
                        match fareZone idsRecord with
                        | None -> Seq.empty
                        | Some zoneCode ->
                            let calendarId =
                                if fields.Length > 5
                                   && not (String.IsNullOrWhiteSpace(fields.[5]))
                                then fields.[5] else "journey"
                            calls
                            |> Seq.filter (fun call ->
                                call.sourceIndex >= firstIndex
                                && call.sourceIndex <= lastIndex)
                            |> Seq.collect (fun call ->
                                segmentByCall
                                |> Map.tryFind (paId message, call.sourceIndex)
                                |> Option.defaultValue [||]
                                |> Seq.map (fun generatedTripId ->
                                    let zone: CzTripStopZone = {
                                        tripId = generatedTripId
                                        stopSequence = call.sourceIndex + 1
                                        zoneId = $"czptt:ids:{code}"
                                        zoneCode = zoneCode
                                        idsSystemId = catalogIdsSystemId idsRecord
                                        sourceProvenance =
                                            $"czptt:CZIPTS:{code}:{firstIndex + 1}-" +
                                            $"{lastIndex + 1}:calendar={calendarId}"
                                    }
                                    zone))
                    | _ ->
                        idsDiagnostics.Add(
                            $"{paId message}: unresolved CZIPTS interval {sourceValue}")
                        Seq.empty))
        |> Seq.distinct
        |> Seq.sortBy (fun value ->
            value.tripId, value.stopSequence, value.zoneId)
        |> Seq.toArray
    let callsByStop =
        allStopTimes
        |> Seq.groupBy (fun value -> value.stopId)
        |> Seq.map (fun (stop, values) ->
            stop,
            values
            |> Seq.map (fun value -> value.tripId, value.stopSequence)
            |> Set)
        |> Map
    let zoneByCall =
        tripStopZones
        |> Seq.groupBy (fun zone -> zone.tripId, zone.stopSequence)
        |> Seq.map (fun (call, values) ->
            call, values |> Seq.map (fun value -> value.zoneId) |> Set)
        |> Map
    let unambiguousStopZones =
        callsByStop
        |> Map.toSeq
        |> Seq.choose (fun (stop, calls) ->
            let memberships =
                calls
                |> Seq.map (fun call ->
                    Map.tryFind call zoneByCall |> Option.defaultValue Set.empty)
                |> Seq.toArray
            if memberships.Length > 0
               && memberships |> Array.forall (fun zones -> zones.Count = 1)
               && memberships |> Array.distinct |> Array.length = 1 then
                Some (stop, memberships.[0] |> Seq.exactlyOne)
            else None)
        |> Map
    let operationalCalls =
        accepted
        |> Seq.collect (fun (message, calls, _, _) ->
            calls |> Seq.map (fun call ->
                let subsidiary =
                    nullOpt call.location.Location.LocationSubsidiaryIdentification
                {
                    paId = paId message
                    sourceSequence = call.sourceIndex + 1
                    countryCode = call.location.Location.CountryCodeIso
                    primaryCode = call.location.Location.LocationPrimaryCode
                    name = stopName pointNames call
                    passengerCall = call.passenger
                    arrivalSeconds = call.arrival
                    departureSeconds = call.departure
                    subsidiaryCode =
                        subsidiary |> Option.map (fun value ->
                            value.LocationSubsidiaryCode.Value)
                    subsidiaryName =
                        subsidiary |> Option.bind (fun value ->
                            nullOpt value.LocationSubsidiaryName)
                    activeLineCode = call.lineCode
                    generatedStationId = stationId call
                    generatedStopId = stopId call
                    generatedTripIds =
                        segmentByCall
                        |> Map.tryFind (paId message, call.sourceIndex)
                        |> Option.defaultValue [||]
                }))
        |> Seq.toArray
    let feedStops =
        stops pointNames allJourneyCalls
        |> Array.map (fun value ->
            { value with zoneId = Map.tryFind value.id unambiguousStopZones })
    let feed = {
        agencies = agencyCodes |> Array.map (agency catalog)
        stops = feedStops
        routes = routes
        trips = trips
        stopTimes = allStopTimes
        shapes = None
        calendar = None
        calendarExceptions = Some allCalendarExceptions
        feedInfo = feedInfo acceptedMessages
        transfers = Some allTransfers
        czRoutes = Some czRoutes
        czTrips =
            generatedTrips
            |> Array.map (fun (message, journey, value) -> {
                tripId = value.id
                cisLineId = None
                cisTripId = None
                trainNumber = trainNumberForCalls message journey.calls
                sourceTripIds =
                    Some (
                        $"PA={paId message}|" +
                        $"TR={identifierStr (trIdentifier message)}")
                coverageSources = Some "czptt"
            })
            |> Some
        czStops = None
        czStopZones = None
        czTripStopZones = Some tripStopZones
    }
    {
        feed = feed
        operationalCalls = operationalCalls
        rejectedJourneys = rejected.ToArray()
        cancelledPaIds = [||]
        acceptedPaIds = acceptedMessages |> Array.map paId
        sidecarBoundaryApproximations = approximations.ToArray()
        boundaryAdjustments = boundaryAdjustments.ToArray()
        idsDiagnostics = idsDiagnostics.ToArray()
        mergeDiagnostics = [||]
        coordinateDiagnostics = {
            resolutionMethod = "not-applied"
            resolvedPointCount = 0
            resolvedStopCount = 0
            unresolvedByCountry = [||]
            unresolvedPointIds = [||]
            unresolvedPassengerPointIds = [||]
            conflictingSr70Codes = [||]
            authoritativeSr70Resolutions = [||]
            osmGapFills = [||]
            estimatedResolutions = [||]
            osmSr70Disagreements = [||]
            invalidSr70Identities = [||]
            ambiguousOsmCandidates = [||]
            corridorRejectedOsmCandidates = [||]
        }
        notes = allNotes
        features = allFeatures
    }
