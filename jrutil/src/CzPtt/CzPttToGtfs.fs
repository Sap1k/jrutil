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

type OperationalPointMode =
    | Gtfs
    | Sidecar

type CatalogLine = {
    code: string
    mark: string
    name: string
    validFrom: LocalDate option
    validTo: LocalDate option
}

type CatalogCompany = {
    code: string
    name: string
    url: string option
}

type CatalogIds = {
    code: string
    abbreviation: string
    name: string
    note: string option
    validFrom: LocalDate option
    validTo: LocalDate option
}

type CatalogCode = {
    code: string
    abbreviation: string
}

type CatalogSnapshot = {
    lines: CatalogLine array
    companies: CatalogCompany array
    ids: CatalogIds array
    trainTypes: CatalogCode array
    commercialTrainTypes: CatalogCode array
}

type PointNameIndex = Map<string * string, string>

type RejectedJourney = {
    paId: string
    reason: string
    sequence: int option
    previousSeconds: int option
    currentSeconds: int option
}

type OperationalCall = {
    paId: string
    sourceSequence: int
    countryCode: string
    primaryCode: string
    name: string
    passengerCall: bool
    arrivalSeconds: int option
    departureSeconds: int option
    subsidiaryCode: string option
    subsidiaryName: string option
    activeLineCode: string option
    generatedStationId: string
    generatedStopId: string
    generatedTripIds: string array
}

type CoordinateCountrySummary = {
    countryCode: string
    pointCount: int
    stopCount: int
}

type CoordinateResolutionDiagnostic = {
    sourceLocationId: string
    countryCode: string
    primaryCode: string
    coordinateSource: string
    coordinateSourceObjectId: string option
    coordinateMatchMethod: string
}

type CoordinateConflictDiagnostic = {
    sourceLocationId: string
    retainedSource: string
    candidateObjectId: string
    distanceMeters: double
}

type CoordinateDiagnostics = {
    resolutionMethod: string
    resolvedPointCount: int
    resolvedStopCount: int
    unresolvedByCountry: CoordinateCountrySummary array
    unresolvedPointIds: string array
    unresolvedPassengerPointIds: string array
    conflictingSr70Codes: string array
    authoritativeSr70Resolutions: CoordinateResolutionDiagnostic array
    osmGapFills: CoordinateResolutionDiagnostic array
    estimatedResolutions: CoordinateResolutionDiagnostic array
    osmSr70Disagreements: CoordinateConflictDiagnostic array
    invalidSr70Identities: string array
    ambiguousOsmCandidates: string array
    corridorRejectedOsmCandidates: string array
}

type ConversionResult = {
    feed: GtfsFeed
    operationalCalls: OperationalCall array
    rejectedJourneys: RejectedJourney array
    cancelledPaIds: string array
    acceptedPaIds: string array
    sidecarBoundaryApproximations: string array
    idsDiagnostics: string array
    mergeDiagnostics: string array
    coordinateDiagnostics: CoordinateDiagnostics
}

type private NormalizedCall = {
    sourceIndex: int
    location: CzPttXml.CzpttLocation
    passenger: bool
    arrival: int option
    departure: int option
    lineCode: string option
    responsibleRu: string
    routeType: string
    category: string
}

type private Segment = {
    index: int
    calls: NormalizedCall array
    lineCode: string option
    responsibleRu: string
    routeType: string
    category: string
}

type private FallbackDisplayCandidate = {
    routeId: string
    paId: string
    serviceDays: int
    originIdentity: string * string
    destinationIdentity: string * string
    originName: string
    destinationName: string
}

let emptyCatalog = {
    lines = [||]
    companies = [||]
    ids = [||]
    trainTypes = [||]
    commercialTrainTypes = [||]
}

let emptyPointNames: PointNameIndex = Map.empty

let private tryString (element: JsonElement) (name: string) =
    match element.TryGetProperty(name) with
    | true, value when value.ValueKind <> JsonValueKind.Null ->
        value.GetString() |> nullOpt
    | _ -> None

let private tryDate (element: JsonElement) (name: string) =
    tryString element name
    |> Option.bind (fun value ->
        match LocalDatePattern.Iso.Parse(value) with
        | result when result.Success -> Some result.Value
        | _ -> None)

let loadCatalogSnapshot (path: string) =
    use document = JsonDocument.Parse(IO.File.ReadAllText(path))
    let root = document.RootElement
    let array (name: string) =
        match root.TryGetProperty(name) with
        | true, value when value.ValueKind = JsonValueKind.Array ->
            value.EnumerateArray() |> Seq.toArray
        | _ -> [||]
    {
        lines =
            array "lines"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    mark = tryString value "mark" |> Option.defaultValue code
                    name = tryString value "name" |> Option.defaultValue ""
                    validFrom = tryDate value "valid_from"
                    validTo = tryDate value "valid_to"
                }))
        companies =
            array "companies"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    name = tryString value "name" |> Option.defaultValue $"Unknown {code}"
                    url = tryString value "url"
                }))
        ids =
            array "ids"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    abbreviation =
                        tryString value "abbreviation" |> Option.defaultValue code
                    name = tryString value "name" |> Option.defaultValue ""
                    note = tryString value "note"
                    validFrom = tryDate value "valid_from"
                    validTo = tryDate value "valid_to"
                }))
        trainTypes =
            array "train_types"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    abbreviation =
                        tryString value "abbreviation"
                        |> Option.defaultValue code
                }))
        commercialTrainTypes =
            array "commercial_train_types"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    abbreviation =
                        tryString value "abbreviation"
                        |> Option.defaultValue code
                }))
    }

let private normalizedSr70Name20 (value: string) =
    let trimmed = value.TrimEnd()
    let withoutStopSuffix =
        Regex.Replace(
            trimmed,
            @"\s+(?:n?z)\s*$",
            "",
            RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)
        |> fun result -> result.TrimEnd()
    if String.IsNullOrWhiteSpace(withoutStopSuffix) || withoutStopSuffix = "-"
    then None
    else Some withoutStopSuffix

let loadSr70Name20 (path: string) : PointNameIndex =
    CsvFile.Parse(IO.File.ReadAllText(path), hasHeaders = false).Rows
    |> Seq.mapi (fun index row ->
        let columns = row.Columns
        if columns.Length <> 4 then
            invalidArg "path"
                $"SR70 Název20 row must have four columns: {path} row {index + 1}"
        if columns.[0].Length < 5 then
            invalidArg "path"
                $"Invalid SR70 identifier in {path} row {index + 1}: '{columns.[0]}'"
        columns.[0].Substring(0, 5), normalizedSr70Name20 columns.[1])
    |> Seq.choose (fun (code, name) -> name |> Option.map (fun value -> code, value))
    |> Seq.groupBy fst
    |> Seq.choose (fun (code, values) ->
        let names = values |> Seq.map snd |> Seq.distinct |> Seq.toArray
        if names.Length = 1 then Some (("CZ", code), names.[0])
        else None)
    |> Map

let private parameterPairs (parameters: CzPttXml.NetworkSpecificParameter array) =
    parameters
    |> nullOpt
    |> Option.defaultValue [||]
    |> Seq.collect (fun parameter ->
        let names = parameter.Name |> nullOpt |> Option.defaultValue [||]
        let values = parameter.Value |> nullOpt |> Option.defaultValue [||]
        Seq.zip (names |> Seq.truncate values.Length)
                (values |> Seq.truncate names.Length))

let private parameterValues name parameters =
    parameterPairs parameters
    |> Seq.choose (fun (key, value) ->
        if String.Equals(key, name, StringComparison.Ordinal) then Some value
        else None)

let private lastParameter name parameters =
    parameterValues name parameters |> Seq.tryLast

let private paIdentifier (message: CzPttXml.CzpttcisMessage) =
    timetableIdentifier message CzPttXml.ObjectType.Pa

let private paId (message: CzPttXml.CzpttcisMessage) =
    paIdentifier message |> identifierStr

let private trIdentifier (message: CzPttXml.CzpttcisMessage) =
    timetableIdentifier message CzPttXml.ObjectType.Tr

let private idComponent (value: string) = Uri.EscapeDataString(value)

let private timingSeconds (timing: CzPttXml.Timing) =
    let local = timing.Time.Split('+').[0]
    let parsed =
        TimeSpan.Parse(local, CultureInfo.InvariantCulture)
    int parsed.TotalSeconds + int timing.Offset * 86400

let private findRawTime (qualifier: CzPttXml.TimingTimingQualifierCode)
                        (location: CzPttXml.CzpttLocation) =
    location.TimingAtLocation
    |> nullOpt
    |> Option.bind (fun timingAtLocation ->
        timingAtLocation.Timing
        |> nullOpt
        |> Option.defaultValue [||]
        |> Array.tryFind (fun timing -> timing.TimingQualifierCode = qualifier))
    |> Option.map timingSeconds

let private locationTimes (location: CzPttXml.CzpttLocation) =
    let arrival = findRawTime CzPttXml.TimingTimingQualifierCode.Ala location
    let departure = findRawTime CzPttXml.TimingTimingQualifierCode.Ald location
    arrival |> Option.orElse departure, departure |> Option.orElse arrival

let private hasActivity (activity: TrainActivity)
                        (location: CzPttXml.CzpttLocation) =
    locationActivities location |> Seq.contains activity

let private catalogAbbreviation entries code =
    entries
    |> Array.tryFind (fun entry -> entry.code = code)
    |> Option.map (fun entry -> entry.abbreviation)
    |> Option.defaultValue code

let private rawCategory (catalog: CatalogSnapshot)
                        (location: CzPttXml.CzpttLocation) =
    nullOpt location.CommercialTrafficType
    |> Option.filter (String.IsNullOrWhiteSpace >> not)
    |> Option.map (catalogAbbreviation catalog.commercialTrainTypes)
    |> Option.orElseWith (fun () ->
        nullOpt location.TrafficType
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        |> Option.map (catalogAbbreviation catalog.trainTypes))

let private interCityCategories =
    Set [ "IC"; "EC"; "RJ"; "rj"; "LE"; "SC"; "AEx" ]

let private fastCategories = Set [ "R"; "Ex"; "Rx" ]

let private regionalCategories =
    Set [ "Os"; "Sp"; "TL"; "TLX"; "LET" ]

let private nightCategories = Set [ "NJ"; "EN"; "ES" ]

let private routeTypeForCategory category =
    match category with
    | value when Set.contains value interCityCategories -> "102"
    | value when Set.contains value fastCategories -> "103"
    | value when Set.contains value regionalCategories -> "106"
    | value when Set.contains value nightCategories -> "105"
    | _ -> "100"

let private normalize (catalog: CatalogSnapshot)
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
        })

let private chronologyError (message: CzPttXml.CzpttcisMessage)
                            (calls: NormalizedCall array) =
    let rootInconsistent =
        parameterValues "CZInconsistentTime" message.NetworkSpecificParameter
        |> Seq.exists ((=) "1")
    let inconsistent =
        calls
        |> Array.tryFind (fun call ->
            parameterValues "CZInconsistentTime" call.location.NetworkSpecificParameter
            |> Seq.exists ((=) "1"))
    match rootInconsistent, inconsistent with
    | true, _ ->
        Some {
            paId = paId message
            reason = "CZInconsistentTime=1"
            sequence = None
            previousSeconds = None
            currentSeconds = None
        }
    | false, Some call ->
        Some {
            paId = paId message
            reason = "CZInconsistentTime=1"
            sequence = Some (call.sourceIndex + 1)
            previousSeconds = None
            currentSeconds = call.arrival |> Option.orElse call.departure
        }
    | false, None ->
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

let hasPublicLocations (message: CzPttXml.CzpttcisMessage) =
    message.CzpttInformation.CzpttLocation |> Array.exists isPublicLocation

let private selectedCalls (mode: OperationalPointMode)
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
                && prior.category = call.category ->
                collapsed.[collapsed.Count - 1] <- {
                    prior with
                        passenger = prior.passenger || call.passenger
                        arrival = prior.arrival |> Option.orElse call.arrival
                        departure = call.departure |> Option.orElse prior.departure
                }
            | _ -> collapsed.Add(call)
        collapsed.ToArray()

let private segments (calls: NormalizedCall array) =
    if calls.Length = 0 then [||]
    else
        let key (call: NormalizedCall) =
            call.lineCode, call.responsibleRu, call.routeType, call.category
        let mutable start = 0
        let mutable segmentKey = key calls.[0]
        let result = ResizeArray<Segment>()
        for index in 1 .. calls.Length - 1 do
            let currentKey = key calls.[index]
            let previousLine, _, _, _ = segmentKey
            let currentLine, _, _, _ = currentKey
            let lineExpires =
                match previousLine, currentLine with
                | Some _, None -> true
                | _ -> false
            if currentKey <> segmentKey
               && (index < calls.Length - 1 || lineExpires) then
                let boundary =
                    match previousLine, currentLine with
                    | Some _, None -> index - 1
                    | _ -> index
                let segmentCalls = calls.[start..boundary]
                let line, ru, routeType, category = segmentKey
                if segmentCalls.Length > 1 then
                    result.Add {
                        index = result.Count
                        calls = segmentCalls
                        lineCode = line
                        responsibleRu = ru
                        routeType = routeType
                        category = category
                    }
                start <- boundary
                segmentKey <- currentKey
        let finalCalls = calls.[start..]
        let line, ru, routeType, category = segmentKey
        result.Add {
            index = result.Count
            calls = finalCalls
            lineCode = line
            responsibleRu = ru
            routeType = routeType
            category = category
        }
        result.ToArray()

let private pointIdentity (call: NormalizedCall) =
    call.location.Location.CountryCodeIso,
    call.location.Location.LocationPrimaryCode

let private pointIdentityText (countryCode, primaryCode) =
    $"{idComponent countryCode}:{idComponent primaryCode}"

let private stationIdFor identity =
    $"czptt:stop:{pointIdentityText identity}"

let private stationId (call: NormalizedCall) =
    stationIdFor (pointIdentity call)

let private stopId (call: NormalizedCall) =
    let parent = stationId call
    match nullOpt call.location.Location.LocationSubsidiaryIdentification with
    | None -> $"{parent}:unspecified"
    | Some subsidiary ->
        $"{parent}:platform:{idComponent subsidiary.LocationSubsidiaryCode.Value}"

let private stopName (call: NormalizedCall) =
    call.location.Location.PrimaryLocationName
    |> nullOpt
    |> Option.defaultValue ""

let private pointDisplayName (call: NormalizedCall) =
    stopName call
    |> Option.ofObj
    |> Option.filter (String.IsNullOrWhiteSpace >> not)
    |> Option.defaultValue (
        let country, code = pointIdentity call
        $"{country} {code}")

let private normalizedSpaces (value: string) =
    Regex.Replace(value.Trim(), @"\s+", " ")

let private knownMunicipalityRoots =
    [
        "CZ", "PRAHA", "Praha"
        "CZ", "BRNO", "Brno"
        "CZ", "OSTRAVA", "Ostrava"
        "CZ", "PLZEŇ", "Plzeň"
        "CZ", "OLOMOUC", "Olomouc"
        "CZ", "LIBEREC", "Liberec"
        "CZ", "ÚSTÍ N.L.", "Ústí nad Labem"
        "CZ", "ÚSTÍ NAD LABEM", "Ústí nad Labem"
        "CZ", "HRADEC KRÁLOVÉ", "Hradec Králové"
        "CZ", "PARDUBICE", "Pardubice"
        "CZ", "Č.BUDĚJOVICE", "České Budějovice"
        "CZ", "ČESKÉ BUDĚJOVICE", "České Budějovice"
        "CZ", "KARLOVY VARY", "Karlovy Vary"
        "CZ", "MLADÁ BOLESLAV", "Mladá Boleslav"
        "CZ", "DĚČÍN", "Děčín"
        "CZ", "OPAVA", "Opava"
        "CZ", "HAVÍŘOV", "Havířov"
        "CZ", "KARVINÁ", "Karviná"
        "CZ", "NYMBURK", "Nymburk"
        "CZ", "PROSTĚJOV", "Prostějov"
        "CZ", "PŘEROV", "Přerov"
        "CZ", "KLADNO", "Kladno"
        "CZ", "ZLÍN", "Zlín"
        "CZ", "JIHLAVA", "Jihlava"
        "CZ", "MOST", "Most"
        "CZ", "TEPLICE", "Teplice"
        "CZ", "BOHUMÍN", "Bohumín"
        "AT", "WIEN", "Wien"
        "AT", "LINZ", "Linz"
        "AT", "SALZBURG", "Salzburg"
        "AT", "GRAZ", "Graz"
        "AT", "GMÜND", "Gmünd"
        "DE", "DRESDEN", "Dresden"
        "DE", "LEIPZIG", "Leipzig"
        "DE", "NÜRNBERG", "Nürnberg"
        "DE", "MÜNCHEN", "München"
        "DE", "ZITTAU", "Zittau"
        "DE", "BAD SCHANDAU", "Bad Schandau"
        "PL", "WROCŁAW", "Wrocław"
        "PL", "KATOWICE", "Katowice"
        "PL", "GŁUCHOŁAZY", "Głuchołazy"
        "PL", "CIESZYN", "Cieszyn"
        "SK", "BRATISLAVA", "Bratislava"
        "SK", "ŽILINA", "Žilina"
        "SK", "KOŠICE", "Košice"
        "SK", "ČADCA", "Čadca"
    ]

let private municipalityPointName (call: NormalizedCall) =
    let source = pointDisplayName call |> normalizedSpaces
    let country = call.location.Location.CountryCodeIso.ToUpperInvariant()
    let upper = source.ToUpperInvariant()
    let hasRootBoundary length =
        upper.Length = length
        || (upper.Length > length
            && (Char.IsWhiteSpace(upper.[length])
                || upper.[length] = '-'
                || upper.[length] = ','))
    knownMunicipalityRoots
    |> List.tryPick (fun (expectedCountry, prefix, display) ->
        if country = expectedCountry
           && upper.StartsWith(prefix, StringComparison.Ordinal)
           && hasRootBoundary prefix.Length
        then Some display
        else None)
    |> Option.defaultWith (fun () ->
        let replace pattern value =
            Regex.Replace(
                value,
                pattern,
                "",
                RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)
            |> normalizedSpaces
        source
        |> replace @"\s*\([^)]*\)\s*$"
        |> replace @"\s*/\s*(?:S\.?|SACHS\.?)\s*$"
        |> replace @",\s*.+$"
        |> replace (
            @"\s+(?:" +
            @"HLAVNÍ\s+NÁDRAŽÍ|HLAVNÁ\s+STANICA|HAUPTBAHNHOF|BAHNHOF|" +
            @"HL\.?\s*N\.?|OS\.?\s*N\.?|DOL\.?\s*N\.?|D\.?\s*N\.?|" +
            @"ZAST\.?|N?Z|HP|HBF|FBF" +
            @")\s*$"))

let private bestEffortShortPointName (value: string) =
    let replace (pattern: string) (replacement: string) (name: string) =
        Regex.Replace(
            name,
            pattern,
            replacement,
            RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)

    let compacted =
        [
            @"\bHauptbahnhof\b", "Hbf"
            @"\bBahnhof\b", "Bf"
            @"\bhlavná\s+stanica\b", "hl. st."
            @"\bželezničná\s+stanica\b", "žel. st."
            @"\ban\s+der\b", "a. d."
            @"\bam\b", "a."
            @"\bnad\b", "n."
            @"\bpod\b", "p."
            @"(\p{L}{6,})stadt\b", "$1st."
        ]
        |> List.fold (fun name (pattern, replacement) ->
            replace pattern replacement name) (normalizedSpaces value)
        |> normalizedSpaces

    let abbreviated =
        if compacted.Length <= 20 then compacted
        else
            let words =
                compacted.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            if words.Length <= 2 then compacted
            else
                words
                |> Array.mapi (fun index word ->
                    if index > 0
                        && index < words.Length - 1
                        && word.Length > 3
                    then $"{word.[0]}."
                    else word)
                |> String.concat " "

    if abbreviated.Length <= 20 then abbreviated
    else
        let prefix =
            abbreviated.Substring(0, 19).TrimEnd()
                .TrimEnd([| ' '; '.'; ','; '-'; '–' |])
        $"{prefix}."

let private fallbackPointDisplayName (call: NormalizedCall) =
    municipalityPointName call |> bestEffortShortPointName

let private publicEndpoints (calls: NormalizedCall array) =
    let passengerCalls = calls |> Array.filter (fun call -> call.passenger)
    passengerCalls.[0], passengerCalls.[passengerCalls.Length - 1]

let private stops (calls: NormalizedCall seq) =
    calls
    |> Seq.collect (fun call ->
        let create id locationType parent platform = {
            id = id
            code = None
            name = stopName call
            description = None
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
            create (stationId call) Station None None
            create (stopId call) Stop (Some (stationId call)) platform
        })
    |> Seq.distinctBy (fun stop -> stop.id)
    |> Seq.sortBy (fun stop -> stop.id)
    |> Seq.toArray

let private lineForDate (catalog: CatalogSnapshot) (date: LocalDate) (code: string) =
    catalog.lines
    |> Array.filter (fun line ->
        line.code = code
        && (line.validFrom |> Option.forall (fun startDate -> startDate <= date))
        && (line.validTo |> Option.forall (fun endDate -> date <= endDate)))
    |> Array.sortBy (fun line -> line.validFrom)
    |> Array.tryLast

let private routeId (catalog: CatalogSnapshot) (date: LocalDate)
                    (endpoints: NormalizedCall * NormalizedCall)
                    (segment: Segment) =
    match segment.lineCode with
    | Some code when lineForDate catalog date code |> Option.isSome ->
        $"czptt:route:line:{idComponent code}:{idComponent segment.responsibleRu}:" +
        $"{idComponent segment.routeType}"
    | _ ->
        let firstIdentity, lastIdentity =
            pointIdentity (fst endpoints), pointIdentity (snd endpoints)
        let endpointA, endpointB =
            if firstIdentity <= lastIdentity
            then firstIdentity, lastIdentity
            else lastIdentity, firstIdentity
        $"czptt:route:fallback:{idComponent segment.responsibleRu}:" +
        $"{idComponent segment.category}:{idComponent segment.routeType}:" +
        $"{pointIdentityText endpointA}:{pointIdentityText endpointB}"

let private tripId (message: CzPttXml.CzpttcisMessage) (segment: Segment) =
    $"czptt:trip:{idComponent (paId message)}:{segment.index + 1}"

let private serviceId (message: CzPttXml.CzpttcisMessage) =
    $"czptt:service:{idComponent (paId message)}:calendar:journey"
let private blockId (message: CzPttXml.CzpttcisMessage) =
    $"czptt:block:{idComponent (paId message)}"
let private agencyId (code: string) = $"czptt:agency:{idComponent code}"

let private trainNumber (message: CzPttXml.CzpttcisMessage)
                        (segment: Segment) =
    segment.calls
    |> Array.choose (fun call -> nullOpt call.location.OperationalTrainNumber)
    |> Array.tryHead
    |> Option.orElseWith (fun () -> nullOpt (trIdentifier message).Core)

let private tripShortName (message: CzPttXml.CzpttcisMessage)
                          (segment: Segment) =
    trainNumber message segment
    |> Option.map (fun number -> $"{segment.category} {number}")
    |> Option.orElse (Some segment.category)

let private calendarExceptions (message: CzPttXml.CzpttcisMessage) =
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

let private gtfsTimeShift (calls: NormalizedCall array) =
    let times =
        calls
        |> Seq.collect (fun call ->
            Seq.choose id [ call.arrival; call.departure ])
        |> Seq.toArray
    let minimum = if times.Length = 0 then 0 else Array.min times
    if minimum >= 0 then 0
    else ((-minimum + 86399) / 86400) * 86400

let private stopTimes message shift segment =
    segment.calls
    |> Array.map (fun call ->
        let request = hasActivity RequestStop call.location
        let regular =
            if request then CoordinationWithDriver else RegularlyScheduled
        {
            tripId = tripId message segment
            arrivalTime =
                call.arrival
                |> Option.map (fun value ->
                    Period.FromSeconds(int64 (value + shift)))
            departureTime =
                call.departure
                |> Option.map (fun value ->
                    Period.FromSeconds(int64 (value + shift)))
            stopId = stopId call
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
                if call.arrival.IsSome || call.departure.IsSome
                then Some Exact
                else Some Approximate
            stopZoneIds = None
        })

let private routeColor routeType =
    match routeType with
    | "106" -> "1c1745"
    | "102" -> "b91c1c"
    | "103" -> "b45309"
    | "105" -> "4c1d95"
    | _ -> "475569"

let private route catalog date endpoints fallbackDisplayNames segment =
    let mapped =
        segment.lineCode |> Option.bind (lineForDate catalog date)
    let id = routeId catalog date endpoints segment
    let fallbackShortName =
        Map.tryFind id fallbackDisplayNames
        |> Option.map (fun (origin, destination) ->
            $"{segment.category} {origin} – {destination}")
    {
        id = id
        agencyId = Some (agencyId segment.responsibleRu)
        shortName =
            mapped
            |> Option.map (fun line -> line.mark)
            |> Option.orElse fallbackShortName
        longName = mapped |> Option.map (fun line -> line.name)
        description = None
        routeType = segment.routeType
        url = None
        color = Some (routeColor segment.routeType)
        textColor = Some "ffffff"
        sortOrder = None
    }

let private trip catalog date message endpoints segment =
    let finalCall = snd endpoints
    {
        routeId = routeId catalog date endpoints segment
        serviceId = serviceId message
        id = tripId message segment
        headsign = Some (pointDisplayName finalCall)
        shortName = tripShortName message segment
        directionId = None
        blockId = Some (blockId message)
        shapeId = None
        wheelchairAccessible = None
        bikesAllowed = None
    }

let private transfers message (segments: Segment array) =
    segments
    |> Array.pairwise
    |> Array.map (fun (fromSegment, toSegment) -> {
        fromStopId = None
        toStopId = None
        fromRouteId = None
        toRouteId = None
        fromTripId = Some (tripId message fromSegment)
        toTripId = Some (tripId message toSegment)
        transferType = 4
        minTransferTime = None
    })

let private agency catalog code =
    let company = catalog.companies |> Array.tryFind (fun company -> company.code = code)
    let agencyUrl =
        company
        |> Option.bind (fun value -> value.url)
        |> Option.map (fun value -> value.Trim())
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        |> Option.map (fun value ->
            if Regex.IsMatch(value, @"^[a-z][a-z0-9+.-]*://", RegexOptions.IgnoreCase)
            then value
            else "https://" + value)
        |> Option.filter (fun value ->
            match Uri.TryCreate(value, UriKind.Absolute) with
            | true, uri -> uri.Scheme = Uri.UriSchemeHttp || uri.Scheme = Uri.UriSchemeHttps
            | _ -> false)
        |> Option.defaultValue "https://portal.cisjr.cz/"
    {
        id = Some (agencyId code)
        name = company |> Option.map (fun value -> value.name) |> Option.defaultValue $"Unknown {code}"
        url = Some agencyUrl
        timezone = "Europe/Prague"
        lang = Some "cs"
        phone = None
        fareUrl = None
        email = None
    }

let private validIdsRecord (catalog: CatalogSnapshot) date code =
    catalog.ids
    |> Array.filter (fun value ->
        value.code = code
        && (value.validFrom |> Option.forall (fun startDate -> startDate <= date))
        && (value.validTo |> Option.forall (fun endDate -> date <= endDate)))
    |> Array.sortBy (fun value -> value.validFrom)
    |> Array.tryLast

let private fareZone (record: CatalogIds) =
    record.note
    |> Option.bind (fun note ->
        let matched =
            Regex.Match(note, @"(?i)\bpásmo\s+(.+?)\s*$",
                        RegexOptions.CultureInvariant)
        if matched.Success then Some (matched.Groups.[1].Value.Trim())
        else None)

let private catalogIdsSystemId (record: CatalogIds) =
    match record.abbreviation.Split('_') with
    | parts when parts.Length > 1 -> parts.[0]
    | _ -> record.abbreviation

let private resolveIntervalEndpoint (code: string) (occurrence: int option)
                                    (calls: NormalizedCall array) =
    let normalizedCode =
        if code.Length > 2 && Char.IsLetter(code.[0]) && Char.IsLetter(code.[1])
        then code.Substring(2) else code
    let matches =
        calls
        |> Array.filter (fun call ->
            call.location.Location.LocationPrimaryCode = normalizedCode)
    match occurrence with
    | Some ordinal when ordinal > 0 && ordinal <= matches.Length ->
        Some matches.[ordinal - 1].sourceIndex
    | None when matches.Length = 1 -> Some matches.[0].sourceIndex
    | _ -> None

let private parseOrdinal value =
    if String.IsNullOrWhiteSpace(value) then None
    else
        match Int32.TryParse(value, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, ordinal -> Some ordinal
        | _ -> Some -1

let private feedInfo (messages: CzPttXml.CzpttcisMessage array) =
    if messages.Length = 0 then None
    else
        let starts =
            messages
            |> Array.map (fun message ->
                LocalDate.FromDateTime(
                    message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime))
        let ends =
            messages
            |> Array.map (fun message ->
                message.CzpttInformation.PlannedCalendar.ValidityPeriod.EndDateTime
                |> nullableOpt
                |> Option.map LocalDate.FromDateTime
                |> Option.defaultValue (
                    LocalDate.FromDateTime(
                        message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)))
        Some {
            publisherName = "Oběhy / JrUtil"
            publisherUrl = "https://github.com/dvdkon/jrutil"
            lang = "cs"
            startDate = Some (Array.min starts)
            endDate = Some (Array.max ends)
            version = None
        }

let private activeServiceDays (message: CzPttXml.CzpttcisMessage) =
    message.CzpttInformation.PlannedCalendar.BitmapDays
    |> Seq.filter ((=) '1')
    |> Seq.length

let private fallbackDisplayNames
        (catalog: CatalogSnapshot)
        (accepted:
            ResizeArray<
                CzPttXml.CzpttcisMessage * NormalizedCall array * Segment array>) =
    accepted
    |> Seq.collect (fun (message, calls, journeySegments) ->
        let date =
            LocalDate.FromDateTime(
                message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)
        let endpoints = publicEndpoints calls
        journeySegments
        |> Seq.choose (fun segment ->
            match segment.lineCode |> Option.bind (lineForDate catalog date) with
            | Some _ -> None
            | None ->
                Some {
                    routeId = routeId catalog date endpoints segment
                    paId = paId message
                    serviceDays = activeServiceDays message
                    originIdentity = pointIdentity (fst endpoints)
                    destinationIdentity = pointIdentity (snd endpoints)
                    originName = fallbackPointDisplayName (fst endpoints)
                    destinationName = fallbackPointDisplayName (snd endpoints)
                }))
    |> Seq.distinctBy (fun candidate -> candidate.routeId, candidate.paId)
    |> Seq.groupBy (fun candidate -> candidate.routeId)
    |> Seq.map (fun (id, candidates) ->
        let selected =
            candidates
            |> Seq.groupBy (fun candidate ->
                candidate.originIdentity, candidate.destinationIdentity)
            |> Seq.map (fun (_, direction) ->
                let values = direction |> Seq.toArray
                let representative = values |> Array.minBy (fun value -> value.paId)
                values |> Array.sumBy (fun value -> value.serviceDays),
                representative)
            |> Seq.sortBy (fun (serviceDays, representative) ->
                -serviceDays, representative.paId)
            |> Seq.head
            |> snd
        id, (selected.originName, selected.destinationName))
    |> Map

let convertWithPointNames catalog mode _pointNames
                          (messages: CzPttXml.CzpttcisMessage seq) =
    let accepted = ResizeArray<CzPttXml.CzpttcisMessage * NormalizedCall array * Segment array>()
    let rejected = ResizeArray<RejectedJourney>()
    let approximations = ResizeArray<string>()
    for message in messages |> Seq.sortBy paId do
        let calls = normalize catalog message
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
                let selected = selectedCalls mode calls
                let journeySegments = segments selected
                if mode = Sidecar then
                    let exactChanges =
                        calls
                        |> Array.pairwise
                        |> Array.choose (fun (left, right) ->
                            let leftKey =
                                left.lineCode, left.responsibleRu,
                                left.routeType, left.category
                            let rightKey =
                                right.lineCode, right.responsibleRu,
                                right.routeType, right.category
                            if leftKey <> rightKey && not right.passenger
                            then
                                let placement =
                                    match left.lineCode, right.lineCode with
                                    | Some _, None -> "preceding passenger call"
                                    | _ -> "following passenger call"
                                Some (right.sourceIndex, placement)
                            else None)
                    if exactChanges.Length > 0 then
                        let sourceIndex, placement = exactChanges.[0]
                        approximations.Add(
                            $"{paId message}: service boundary moved from source sequence " +
                            $"{sourceIndex + 1} to the {placement}")
                accepted.Add(message, calls, journeySegments)

    let acceptedMessages = accepted |> Seq.map (fun (message, _, _) -> message) |> Seq.toArray
    let fallbackDisplays = fallbackDisplayNames catalog accepted
    let routes =
        accepted
        |> Seq.collect (fun (message, calls, journeySegments) ->
            let date =
                LocalDate.FromDateTime(
                    message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)
            let endpoints = publicEndpoints calls
            journeySegments
            |> Seq.map (route catalog date endpoints fallbackDisplays))
        |> Seq.distinctBy (fun value -> value.id)
        |> Seq.sortBy (fun value -> value.id)
        |> Seq.toArray
    let generatedTrips =
        accepted
        |> Seq.collect (fun (message, calls, journeySegments) ->
            let date =
                LocalDate.FromDateTime(
                    message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)
            let endpoints = publicEndpoints calls
            journeySegments
            |> Seq.map (fun segment ->
                message, segment, trip catalog date message endpoints segment))
        |> Seq.toArray
    let trips = generatedTrips |> Array.map (fun (_, _, value) -> value)
    let allSelectedCalls =
        accepted
        |> Seq.collect (fun (_, calls, _) -> selectedCalls mode calls)
        |> Seq.toArray
    let allStopTimes =
        accepted
        |> Seq.collect (fun (message, calls, journeySegments) ->
            let shift = gtfsTimeShift calls
            journeySegments |> Seq.collect (stopTimes message shift))
        |> Seq.toArray
    let allTransfers =
        accepted
        |> Seq.collect (fun (message, _, journeySegments) ->
            transfers message journeySegments)
        |> Seq.toArray
    let allCalendarExceptions =
        acceptedMessages |> Array.collect calendarExceptions
    let agencyCodes =
        accepted
        |> Seq.collect (fun (_, _, journeySegments) ->
            journeySegments |> Seq.map (fun segment -> segment.responsibleRu))
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toArray
    let czRoutes =
        routes
        |> Array.map (fun value -> {
            routeId = value.id
            cisLineId = None
            publicLineNumber =
                if value.id.StartsWith(
                       "czptt:route:line:", StringComparison.Ordinal)
                then value.shortName
                else None
            sourceProvenance = "czptt"
        })
    let segmentByCall =
        accepted
        |> Seq.collect (fun (message, _, journeySegments) ->
            journeySegments
            |> Seq.collect (fun segment ->
                segment.calls
                |> Seq.map (fun call ->
                    (paId message, call.sourceIndex), tripId message segment)))
        |> Seq.groupBy fst
        |> Seq.map (fun (key, values) ->
            key, values |> Seq.map snd |> Seq.distinct |> Seq.toArray)
        |> Map
    let idsDiagnostics = ResizeArray<string>()
    let tripStopZones =
        accepted
        |> Seq.collect (fun (message, calls, _) ->
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
        |> Seq.collect (fun (message, calls, _) ->
            calls |> Seq.map (fun call ->
                let subsidiary =
                    nullOpt call.location.Location.LocationSubsidiaryIdentification
                {
                    paId = paId message
                    sourceSequence = call.sourceIndex + 1
                    countryCode = call.location.Location.CountryCodeIso
                    primaryCode = call.location.Location.LocationPrimaryCode
                    name = stopName call
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
        stops allSelectedCalls
        |> Array.map (fun value ->
            { value with zoneId = Map.tryFind value.id unambiguousStopZones })
    let feed = {
        agencies = agencyCodes |> Array.map (agency catalog)
        stops = feedStops
        routes = routes
        trips = trips
        stopTimes = allStopTimes
        calendar = None
        calendarExceptions = Some allCalendarExceptions
        feedInfo = feedInfo acceptedMessages
        transfers = Some allTransfers
        czRoutes = Some czRoutes
        czTrips =
            generatedTrips
            |> Array.map (fun (message, segment, value) -> {
                tripId = value.id
                cisLineId = None
                cisTripId = None
                trainNumber = trainNumber message segment
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
    }

let convert catalog mode messages =
    convertWithPointNames catalog mode emptyPointNames messages

let gtfsFeedWithOptions catalog mode messages =
    (convert catalog mode messages).feed

let gtfsFeed messages = gtfsFeedWithOptions emptyCatalog Gtfs messages

let gtfsFeedMergedWithOptions catalog mode (messages: (string * CzpttMessage) seq) =
    let merger = CzPttMerger()
    merger.ProcessAll(messages)
    gtfsFeedWithOptions catalog mode merger.Messages.Values

let gtfsFeedMergedWithOptionsAndPointNames
        catalog mode pointNames (messages: (string * CzpttMessage) seq) =
    let merger = CzPttMerger()
    merger.ProcessAll(messages)
    (convertWithPointNames catalog mode pointNames merger.Messages.Values).feed

let gtfsFeedMerged messages =
    gtfsFeedMergedWithOptions emptyCatalog Gtfs messages
