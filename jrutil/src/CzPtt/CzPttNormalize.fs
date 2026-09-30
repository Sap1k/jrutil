// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023-2026 David Koňařík and contributors

/// CZPTT message normalization: calls, notes, chronology, service boundaries and journeys.
module JrUtil.CzPttNormalize

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

let internal parameterPairs (parameters: CzPttXml.NetworkSpecificParameter array) =
    parameters
    |> nullOpt
    |> Option.defaultValue [||]
    |> Seq.collect (fun parameter ->
        let names = parameter.Name |> nullOpt |> Option.defaultValue [||]
        let values = parameter.Value |> nullOpt |> Option.defaultValue [||]
        Seq.zip (names |> Seq.truncate values.Length)
                (values |> Seq.truncate names.Length))

let internal parameterValues name parameters =
    parameterPairs parameters
    |> Seq.choose (fun (key, value) ->
        if String.Equals(key, name, StringComparison.Ordinal) then Some value
        else None)

let internal lastParameter name parameters =
    parameterValues name parameters |> Seq.tryLast

let internal paIdentifier (message: CzPttXml.CzpttcisMessage) =
    timetableIdentifier message CzPttXml.ObjectType.Pa

let internal paId (message: CzPttXml.CzpttcisMessage) =
    paIdentifier message |> identifierStr

let internal trIdentifier (message: CzPttXml.CzpttcisMessage) =
    timetableIdentifier message CzPttXml.ObjectType.Tr

let internal idComponent (value: string) = Uri.EscapeDataString(value)

let internal timingSeconds (timing: CzPttXml.Timing) =
    let local = timing.Time.Split('+').[0]
    let parsed =
        TimeSpan.Parse(local, CultureInfo.InvariantCulture)
    int parsed.TotalSeconds + int timing.Offset * 86400

let internal findRawTime (qualifier: CzPttXml.TimingTimingQualifierCode)
                        (location: CzPttXml.CzpttLocation) =
    location.TimingAtLocation
    |> nullOpt
    |> Option.bind (fun timingAtLocation ->
        timingAtLocation.Timing
        |> nullOpt
        |> Option.defaultValue [||]
        |> Array.tryFind (fun timing -> timing.TimingQualifierCode = qualifier))
    |> Option.map timingSeconds

let internal locationTimes (location: CzPttXml.CzpttLocation) =
    let arrival = findRawTime CzPttXml.TimingTimingQualifierCode.Ala location
    let departure = findRawTime CzPttXml.TimingTimingQualifierCode.Ald location
    arrival |> Option.orElse departure, departure |> Option.orElse arrival

let internal hasActivity (activity: TrainActivity)
                        (location: CzPttXml.CzpttLocation) =
    locationActivities location |> Seq.contains activity

let internal catalogAbbreviation (entries: CatalogCode array) code =
    entries
    |> Array.tryFind (fun entry -> entry.code = code)
    |> Option.map (fun entry -> entry.abbreviation)
    |> Option.defaultValue code

let internal rawCategory (catalog: CatalogSnapshot)
                        (location: CzPttXml.CzpttLocation) =
    nullOpt location.CommercialTrafficType
    |> Option.filter (String.IsNullOrWhiteSpace >> not)
    |> Option.map (catalogAbbreviation catalog.commercialTrainTypes)
    |> Option.orElseWith (fun () ->
        nullOpt location.TrafficType
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        |> Option.map (catalogAbbreviation catalog.trainTypes))

let internal interCityCategories =
    Set [ "IC"; "EC"; "RJ"; "rj"; "LE"; "SC"; "AEx" ]

let internal fastCategories = Set [ "R"; "Ex"; "Rx" ]

let internal regionalCategories =
    Set [ "Os"; "Sp"; "TL"; "TLX"; "LET" ]

let internal nightCategories = Set [ "NJ"; "EN"; "ES" ]

let internal routeTypeForCategory category =
    match category with
    | value when Set.contains value interCityCategories -> "102"
    | value when Set.contains value fastCategories -> "103"
    | value when Set.contains value regionalCategories -> "106"
    | value when Set.contains value nightCategories -> "105"
    | _ -> "100"

let internal normalize (catalog: CatalogSnapshot)
                      (message: CzPttXml.CzpttcisMessage) =
    let locations = message.CzpttInformation.CzpttLocation
    let cleanLine (value: string option) =
        value
        |> Option.map (fun line -> line.Trim())
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
    let hasLocationLines =
        locations
        |> Array.exists (fun location ->
            parameterValues
                "CZPassengerServiceNumber" location.NetworkSpecificParameter
            |> Seq.isEmpty
            |> not)
    let rootLine =
        parameterValues "CZPassengerServiceNumber" message.NetworkSpecificParameter
        |> Seq.tryLast
        |> cleanLine
    let isAlternativeTransport (location: CzPttXml.CzpttLocation) =
        match lastParameter
                "CZAlternativeTransport" location.NetworkSpecificParameter with
        | Some "1" -> true
        | _ -> false
    let mutable currentRu = (trIdentifier message).Company
    let mutable currentCategory = "Vlak"
    locations
    |> Array.mapi (fun index location ->
        let currentLine =
            if hasLocationLines then
                lastParameter
                    "CZPassengerServiceNumber" location.NetworkSpecificParameter
                |> cleanLine
            else rootLine
        nullOpt location.ResponsibleRu
        |> Option.iter (fun responsible -> currentRu <- responsible)
        rawCategory catalog location
        |> Option.iter (fun category -> currentCategory <- category)
        let arrival, departure = locationTimes location
        {
            sourceIndex = index
            location = location
            passenger = isPublicLocation location
            arrival = arrival
            departure = departure
            lineCode = currentLine
            responsibleRu = currentRu
            routeType = routeTypeForCategory currentCategory
            category = currentCategory
            alternativeTransport = isAlternativeTransport location
            inferredTiming = false
        })

type internal CentralNoteSpec = {
    index: int
    code: string
    rawValue: string
    firstIndex: int option
    lastIndex: int option
    calendarId: string option
    coversAllDates: bool
}

let internal compactLocationCode (value: string) =
    let compact = value.Trim().ToUpperInvariant()
    if compact.Length > 2 && Char.IsLetter(compact.[0]) && Char.IsLetter(compact.[1])
    then compact.Substring(2)
    else compact

let internal noteOccurrence (value: string) =
    if String.IsNullOrWhiteSpace(value) then Some 0
    else
        match Int32.TryParse(value, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, ordinal when ordinal >= 0 -> Some ordinal
        | _ -> None

let internal resolveNoteEndpoint code occurrence (calls: NormalizedCall array) =
    let matches =
        calls
        |> Array.filter (fun call ->
            compactLocationCode call.location.Location.LocationPrimaryCode =
                compactLocationCode code)
    occurrence |> Option.bind (fun ordinal ->
        matches |> Array.tryItem ordinal |> Option.map (fun call -> call.sourceIndex))

let internal tryCompactDate (value: string) =
    LocalDatePattern.CreateWithInvariantCulture("yyyyMMdd").Parse(value)
    |> fun parsed -> if parsed.Success then Some parsed.Value else None

let internal activeJourneyDates (message: CzPttXml.CzpttcisMessage) =
    let calendar = message.CzpttInformation.PlannedCalendar
    let startDate = LocalDate.FromDateTime(calendar.ValidityPeriod.StartDateTime)
    calendar.BitmapDays
    |> Seq.mapi (fun index active -> startDate.PlusDays(index), active)
    |> Seq.choose (fun (date, active) -> if active = '1' then Some date else None)
    |> Set

let internal noteCalendarDates (message: CzPttXml.CzpttcisMessage) =
    parameterValues "CZCalendarPTTNote" message.NetworkSpecificParameter
    |> Seq.choose (fun (raw: string) ->
        let fields = raw.Split('|')
        if fields.Length < 4 then None
        else
            match tryCompactDate fields.[1] with
            | None -> None
            | Some startDate ->
                let dates =
                    fields.[3]
                    |> Seq.mapi (fun index active -> startDate.PlusDays(index), active)
                    |> Seq.choose (fun (date, active) -> if active = '1' then Some date else None)
                    |> Set
                let nextId =
                    fields
                    |> Array.tryItem 4
                    |> Option.filter (String.IsNullOrWhiteSpace >> not)
                Some (fields.[0], dates, nextId))
    |> Seq.groupBy (fun (id, _, _) -> id)
    |> Seq.map (fun (id, rows) -> id, rows |> Seq.toArray)
    |> Map

let internal resolvedCalendarDates message calendarId =
    let calendars = noteCalendarDates message
    let rec collect visited id =
        if Set.contains id visited then None
        else
            match Map.tryFind id calendars with
            | None -> None
            | Some rows ->
                let own = rows |> Seq.collect (fun (_, dates, _) -> dates) |> Set
                let next =
                    rows
                    |> Seq.choose (fun (_, _, nextId) -> nextId)
                    |> Seq.distinct
                    |> Seq.toArray
                if next.Length > 1 then None
                elif next.Length = 0 then Some own
                else
                    collect (Set.add id visited) next.[0]
                    |> Option.map (Set.union own)
    collect Set.empty calendarId

let internal centralNoteSpecs message calls =
    let journeyDates = activeJourneyDates message
    parameterValues "CZCentralPTTNote" message.NetworkSpecificParameter
    |> Seq.mapi (fun index raw ->
        let fields = raw.Split('|')
        let code = fields |> Array.tryItem 0 |> Option.defaultValue ""
        let firstIndex, lastIndex =
            if fields.Length < 5 then None, None
            else
                resolveNoteEndpoint fields.[1] (noteOccurrence fields.[2]) calls,
                resolveNoteEndpoint fields.[3] (noteOccurrence fields.[4]) calls
        let calendarId =
            fields
            |> Array.tryItem 6
            |> Option.filter (String.IsNullOrWhiteSpace >> not)
        let coversAllDates =
            match calendarId with
            | None -> true
            | Some id ->
                resolvedCalendarDates message id
                |> Option.exists (fun dates -> Set.isSubset journeyDates dates)
        { index = index; code = code; rawValue = raw
          firstIndex = firstIndex; lastIndex = lastIndex
          calendarId = calendarId; coversAllDates = coversAllDates })
    |> Seq.toArray

let internal noteCoversJourney (journey: Journey) note =
    match note.firstIndex, note.lastIndex with
    | Some firstIndex, Some lastIndex when firstIndex <= lastIndex ->
        firstIndex <= journey.calls.[0].sourceIndex
        && lastIndex >= journey.calls.[journey.calls.Length - 1].sourceIndex
    | _ -> false

let internal projectedTripFeatures message calls journey =
    if journey.alternativeTransport then None, None, false
    else
        let complete code =
            centralNoteSpecs message calls
            |> Array.exists (fun note ->
                note.code = code && note.coversAllDates && noteCoversJourney journey note)
        let wheelchair = complete "17" || complete "34"
        let positiveBike = [| "22"; "26"; "27"; "28"; "29" |] |> Array.exists complete
        let prohibitedBike = complete "36"
        let bikes =
            match positiveBike, prohibitedBike with
            | true, false -> Some OneOrMore
            | false, true -> Some NoBicycles
            | _ -> None
        (if wheelchair then Some "1" else None), bikes, (positiveBike && prohibitedBike)

let internal inconsistentTime (call: NormalizedCall) =
    parameterValues "CZInconsistentTime" call.location.NetworkSpecificParameter
    |> Seq.exists ((=) "1")

let internal forceMonotonicFlaggedTimes message (calls: NormalizedCall array) =
    let adjusted = Array.copy calls
    let diagnostics = ResizeArray<string>()
    for index in 0 .. calls.Length - 1 do
        let call = adjusted.[index]
        if inconsistentTime call then
            let previous =
                adjusted.[..index - 1]
                |> Array.tryFindBack (fun value ->
                    value.departure.IsSome || value.arrival.IsSome)
            let following =
                calls.[index + 1..]
                |> Array.tryFind (fun value ->
                    not (inconsistentTime value)
                    && (value.arrival.IsSome || value.departure.IsSome))
            match previous, following with
            | Some left, Some right ->
                let leftTime = left.departure |> Option.orElse left.arrival |> Option.get
                let rightTime = right.arrival |> Option.orElse right.departure |> Option.get
                if rightTime >= leftTime then
                    let span = right.sourceIndex - left.sourceIndex
                    let inferred =
                        int64 leftTime
                        + int64 (rightTime - leftTime)
                            * int64 (call.sourceIndex - left.sourceIndex) / int64 span
                        |> int
                    let arrival =
                        match call.arrival |> Option.orElse call.departure with
                        | Some value when value >= leftTime && value <= rightTime -> value
                        | _ -> inferred
                    let departure =
                        match call.departure |> Option.orElse call.arrival with
                        | Some value when value >= arrival && value <= rightTime -> value
                        | _ -> arrival
                    if call.arrival <> Some arrival || call.departure <> Some departure then
                        adjusted.[index] <- {
                            call with
                                arrival = Some arrival
                                departure = Some departure
                        }
                        diagnostics.Add(
                            $"{paId message}: corrected CZInconsistentTime at source " +
                            $"sequence {call.sourceIndex + 1} from " +
                            $"arrival={call.arrival}, departure={call.departure} to " +
                            $"arrival={arrival}, departure={departure}")
            | _ -> ()
    adjusted, diagnostics.ToArray()

let internal chronologyError (message: CzPttXml.CzpttcisMessage)
                            (calls: NormalizedCall array) =
    let mutable previous: int option = None
    let mutable error: RejectedJourney option = None
    for call in calls do
        if error.IsNone then
            match call.arrival, call.departure with
            | Some arrival, Some departure when departure < arrival ->
                error <- Some {
                    paId = paId message
                    reason = "departure precedes arrival"
                    sequence = Some (call.sourceIndex + 1)
                    previousSeconds = Some arrival
                    currentSeconds = Some departure
                }
            | _ ->
                let first = call.arrival |> Option.orElse call.departure
                let last = call.departure |> Option.orElse call.arrival
                match previous, first with
                | Some prior, Some current when current < prior ->
                    error <- Some {
                        paId = paId message
                        reason = "event precedes previous event"
                        sequence = Some (call.sourceIndex + 1)
                        previousSeconds = Some prior
                        currentSeconds = Some current
                    }
                | _ -> previous <- last |> Option.orElse previous
    error

let internal distinctInOrder values =
    let seen = Collections.Generic.HashSet<_>()
    values |> Seq.filter seen.Add |> Seq.toArray

let internal projectPassengerServiceBoundaries
        (message: CzPttXml.CzpttcisMessage)
        (calls: NormalizedCall array) =
    if calls.Length < 2 then calls, [||]
    else
        let passengerIndexes =
            calls
            |> Array.filter (fun call -> call.passenger)
            |> Array.map (fun call -> call.sourceIndex)
        let ruTransitions =
            [| for index in 1 .. calls.Length - 1 do
                   if calls.[index - 1].responsibleRu <> calls.[index].responsibleRu then
                       yield index |]
        let serviceKey (call: NormalizedCall) =
            call.lineCode, call.routeType, call.category
        let events = ResizeArray<int * int * (string option * string * string)>()
        let adjustments = ResizeArray<BoundaryAdjustment>()
        for sourceIndex in 1 .. calls.Length - 1 do
            let previousKey = serviceKey calls.[sourceIndex - 1]
            let currentKey = serviceKey calls.[sourceIndex]
            if previousKey <> currentKey then
                let target =
                    if calls.[sourceIndex].passenger then Some sourceIndex
                    else
                        passengerIndexes
                        |> Array.tryFind (fun index -> index > sourceIndex)
                        |> Option.map (fun nextPassenger ->
                            let previousPassenger =
                                passengerIndexes
                                |> Array.filter (fun index -> index < sourceIndex)
                                |> Array.tryLast
                                |> Option.defaultValue -1
                            ruTransitions
                            |> Array.filter (fun index ->
                                index > previousPassenger && index <= nextPassenger)
                            |> Array.sortBy (fun index ->
                                abs (index - sourceIndex), index)
                            |> Array.tryHead
                            |> Option.defaultValue nextPassenger)
                match target with
                | Some appliedIndex ->
                    events.Add(appliedIndex, sourceIndex, currentKey)
                    if appliedIndex <> sourceIndex then
                        adjustments.Add {
                            paId = paId message
                            sourceSequence = sourceIndex + 1
                            appliedSequence = Some (appliedIndex + 1)
                            reason =
                                if ruTransitions |> Array.contains appliedIndex
                                then "coalesced-with-operator-boundary"
                                else "deferred-to-next-passenger-call"
                        }
                | None ->
                    adjustments.Add {
                        paId = paId message
                        sourceSequence = sourceIndex + 1
                        appliedSequence = None
                        reason = "ignored-after-final-passenger-call"
                    }
        let eventsByTarget =
            events
            |> Seq.groupBy (fun (target, _, _) -> target)
            |> Seq.map (fun (target, values) ->
                target,
                values
                |> Seq.maxBy (fun (_, source, _) -> source)
                |> fun (_, _, value) -> value)
            |> Map
        let initialLine, initialRouteType, initialCategory = serviceKey calls.[0]
        let mutable lineCode = initialLine
        let mutable routeType = initialRouteType
        let mutable category = initialCategory
        let projected =
            calls
            |> Array.mapi (fun index call ->
                match Map.tryFind index eventsByTarget with
                | Some (line, route, trainCategory) ->
                    lineCode <- line
                    routeType <- route
                    category <- trainCategory
                | None -> ()
                { call with
                    lineCode = lineCode
                    routeType = routeType
                    category = category })
        projected, adjustments.ToArray()

let internal selectedCalls (mode: OperationalPointMode)
                          (calls: NormalizedCall array) =
    let passengerIndexes =
        calls
        |> Array.filter (fun call -> call.passenger)
        |> Array.map (fun call -> call.sourceIndex)
    if passengerIndexes.Length = 0 then [||]
    else
        let firstPassenger = Array.min passengerIndexes
        let lastPassenger = Array.max passengerIndexes
        let selected =
            calls
            |> Array.filter (fun call ->
            call.passenger
            || (mode = Gtfs
                && call.sourceIndex >= firstPassenger
                && call.sourceIndex <= lastPassenger))
        let collapsed = ResizeArray<NormalizedCall>()
        for call in selected do
            let previous =
                if collapsed.Count = 0 then None
                else Some collapsed.[collapsed.Count - 1]
            let hasSubsidiary (value: NormalizedCall) =
                value.location.Location.LocationSubsidiaryIdentification <> null
            match previous with
            | Some prior when
                prior.sourceIndex + 1 = call.sourceIndex
                && hasSubsidiary prior
                && hasSubsidiary call
                && prior.location.Location.CountryCodeIso =
                    call.location.Location.CountryCodeIso
                && prior.location.Location.LocationPrimaryCode =
                    call.location.Location.LocationPrimaryCode
                && prior.lineCode = call.lineCode
                && prior.responsibleRu = call.responsibleRu
                && prior.routeType = call.routeType
                && prior.category = call.category
                && prior.alternativeTransport = call.alternativeTransport ->
                collapsed.[collapsed.Count - 1] <- {
                    prior with
                        passenger = prior.passenger || call.passenger
                        arrival = prior.arrival |> Option.orElse call.arrival
                        departure = call.departure |> Option.orElse prior.departure
                }
            | _ -> collapsed.Add(call)
        collapsed.ToArray()

let internal segments (calls: NormalizedCall array) =
    if calls.Length = 0 then [||]
    else
        let key (call: NormalizedCall) =
            call.lineCode, call.responsibleRu, call.routeType, call.category,
            call.alternativeTransport
        let mutable start = 0
        let mutable segmentKey = key calls.[0]
        let result = ResizeArray<Segment>()
        for index in 1 .. calls.Length - 1 do
            let currentKey = key calls.[index]
            if currentKey <> segmentKey && index < calls.Length - 1 then
                let boundary = index
                let segmentCalls = calls.[start..boundary]
                let line, ru, railRouteType, category, alternative = segmentKey
                if segmentCalls.Length > 1 then
                    result.Add {
                        index = result.Count
                        calls = segmentCalls
                        lineCode = line
                        responsibleRu = ru
                        routeType = if alternative then "714" else railRouteType
                        category = category
                        alternativeTransport = alternative
                    }
                start <- boundary
                segmentKey <- currentKey
        let finalCalls = calls.[start..]
        let line, ru, railRouteType, category, alternative = segmentKey
        result.Add {
            index = result.Count
            calls = finalCalls
            lineCode = line
            responsibleRu = ru
            routeType = if alternative then "714" else railRouteType
            category = category
            alternativeTransport = alternative
        }
        result.ToArray()

let internal journeys (journeySegments: Segment array) =
    let mutable modeBlockIndex = 0
    journeySegments
    |> Array.mapi (fun index segment ->
        if index > 0
           && journeySegments.[index - 1].alternativeTransport
                <> segment.alternativeTransport then
            modeBlockIndex <- modeBlockIndex + 1
        {
        index = segment.index
        modeBlockIndex = modeBlockIndex
        calls = segment.calls
        parts = [| segment |]
        responsibleRu = segment.responsibleRu
        routeType = segment.routeType
        alternativeTransport = segment.alternativeTransport
    })

let internal pointIdentity (call: NormalizedCall) =
    call.location.Location.CountryCodeIso,
    call.location.Location.LocationPrimaryCode

let internal hasCompleteTiming (call: NormalizedCall) =
    call.arrival.IsSome && call.departure.IsSome

let internal copyTiming (source: NormalizedCall) (target: NormalizedCall) =
    {
        target with
            arrival = source.arrival |> Option.orElse source.departure
            departure = source.departure |> Option.orElse source.arrival
            inferredTiming = true
    }

let internal interpolateTiming (sourceCalls: NormalizedCall array)
                              (target: NormalizedCall) =
    let previous =
        sourceCalls
        |> Array.tryFindBack (fun call ->
            call.sourceIndex < target.sourceIndex
            && (call.departure.IsSome || call.arrival.IsSome))
    let following =
        sourceCalls
        |> Array.tryFind (fun call ->
            call.sourceIndex > target.sourceIndex
            && (call.arrival.IsSome || call.departure.IsSome))
    match previous, following with
    | Some left, Some right ->
        let leftTime = left.departure |> Option.orElse left.arrival |> Option.get
        let rightTime = right.arrival |> Option.orElse right.departure |> Option.get
        let span = right.sourceIndex - left.sourceIndex
        if span > 0 && rightTime >= leftTime then
            let offset = target.sourceIndex - left.sourceIndex
            let inferred =
                int64 leftTime
                + (int64 (rightTime - leftTime) * int64 offset / int64 span)
                |> int
            Some {
                target with
                    arrival = Some inferred
                    departure = Some inferred
                    inferredTiming = true
            }
        else None
    | _ -> None

let internal reconcileJourneyEdgeTimes message
                                          (sourceCalls: NormalizedCall array)
                                          (journeys: Journey array) =
    let reconciled =
        journeys
        |> Array.map (fun journey ->
            { journey with calls = Array.copy journey.calls })
    for index in 0 .. reconciled.Length - 2 do
        let left = reconciled.[index]
        let right = reconciled.[index + 1]
        let leftIndex = left.calls.Length - 1
        let leftCall = left.calls.[leftIndex]
        let rightCall = right.calls.[0]
        if pointIdentity leftCall = pointIdentity rightCall then
            if hasCompleteTiming leftCall && not (hasCompleteTiming rightCall) then
                right.calls.[0] <- copyTiming leftCall rightCall
            elif hasCompleteTiming rightCall && not (hasCompleteTiming leftCall) then
                left.calls.[leftIndex] <- copyTiming rightCall leftCall

    let diagnostics = ResizeArray<string>()
    let retained =
        reconciled
        |> Array.choose (fun journey ->
            let calls = Array.copy journey.calls
            let lastIndex = calls.Length - 1
            for index in [| 0; lastIndex |] |> Array.distinct do
                if not (hasCompleteTiming calls.[index]) then
                    interpolateTiming sourceCalls calls.[index]
                    |> Option.iter (fun inferred -> calls.[index] <- inferred)
            if hasCompleteTiming calls.[0] && hasCompleteTiming calls.[lastIndex] then
                Some { journey with calls = calls }
            else
                let edges =
                    [|
                        if not (hasCompleteTiming calls.[0]) then yield calls.[0].sourceIndex + 1
                        if lastIndex > 0 && not (hasCompleteTiming calls.[lastIndex]) then
                            yield calls.[lastIndex].sourceIndex + 1
                    |]
                    |> Array.map string
                    |> String.concat ","
                diagnostics.Add(
                    $"{paId message}: omitted generated journey {journey.index + 1} " +
                    $"because GTFS edge timing could not be inferred at source sequence {edges}")
                None)
    retained, diagnostics.ToArray()
