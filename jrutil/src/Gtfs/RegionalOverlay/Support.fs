// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.Support

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Diagnostics
open System.Reflection
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

let jsonOptions =
    JsonSerializerOptions(
        PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower,
        PropertyNameCaseInsensitive = true,
        WriteIndented = true)

let requiredText name (value: string) =
    if String.IsNullOrWhiteSpace(value) then
        invalidArg name $"{name} must be non-empty"
    value

let capabilityNames =
    set [
        "stop_coordinates"; "boarding_points"; "call_boarding_points"; "shapes"
        "route_short_name"; "route_long_name"; "route_color"; "route_text_color"
        "transfers"; "stop_zones"; "stop_names"; "trip_headsigns"; "trip_short_names"
        "schedules"; "calendars"; "agencies"
    ]

let sha256Bytes (bytes: byte array) =
    Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant()

let sha256Text (value: string) =
    value |> Encoding.UTF8.GetBytes |> sha256Bytes

let sha256File path =
    use stream = File.OpenRead(path)
    Convert.ToHexString(SHA256.HashData(stream)).ToLowerInvariant()

let sha256Tree root =
    let builder = StringBuilder()
    for path in Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories) |> Seq.sort do
        let relative = Path.GetRelativePath(root, path).Replace('\\', '/')
        let info = FileInfo(path)
        builder.Append(relative).Append('\u001f').Append(info.Length).Append('\u001f').Append(sha256File path).Append('\n') |> ignore
    sha256Text (builder.ToString())

let rowValue (row: CsvRow) name =
    match (if String.IsNullOrEmpty(name) then false, "" else row.TryGetValue(name)) with
    | true, value -> value
    | _ -> ""

let sourceIdentity fallback (row: CsvRow) =
    match rowValue row "overlay_source_id" with
    | value when String.IsNullOrWhiteSpace(value) -> fallback
    | value -> value

let originalIdentity column (row: CsvRow) =
    match rowValue row ("overlay_original_" + column) with
    | value when String.IsNullOrWhiteSpace(value) -> rowValue row column
    | value -> value

let parseInt (value: string) =
    match Int32.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | true, result -> result
    | _ -> invalidOp $"Invalid integer value: {value}"

let parseInt64Opt (value: string) =
    match Int64.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | true, result -> Some result
    | _ when String.IsNullOrWhiteSpace(value) -> None
    | _ -> invalidOp $"Invalid integer value: {value}"

let parseDecimalOpt (value: string) =
    match Decimal.TryParse(value, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, result -> Some result
    | _ when String.IsNullOrWhiteSpace(value) -> None
    | _ -> invalidOp $"Invalid decimal value: {value}"

let parseDate value =
    let result = LocalDatePattern.CreateWithInvariantCulture("yyyyMMdd").Parse(value)
    if not result.Success then invalidOp $"Invalid GTFS date: {value}"
    result.Value

let dateString (value: LocalDate) =
    value.ToString("yyyyMMdd", CultureInfo.InvariantCulture)

let secondSunday year =
    let first = LocalDate(year, 12, 1)
    let offset = (int IsoDayOfWeek.Sunday - int first.DayOfWeek + 7) % 7 + 7
    first.PlusDays(offset)

let gvdWindow year =
    let startDate = secondSunday (year - 1)
    let endDate = (secondSunday year).PlusDays(-1)
    let dates = [| for offset in 0 .. Period.Between(startDate, endDate, PeriodUnits.Days).Days -> startDate.PlusDays(offset) |]
    let index = Dictionary<LocalDate, int>()
    dates |> Array.iteri (fun i date -> index.[date] <- i)
    { startDate = startDate; endDate = endDate; dates = dates; index = index }

let emptyDates (window: DateWindow) = Array.zeroCreate<bool> window.dates.Length

let dateIntersection (left: DateSet.Dates) right = left.Intersect(right)

let anyDate (values: seq<bool>) =
    match values with | :? DateSet.Dates as dates -> dates.Any | _ -> values |> Seq.exists id

let dateKey (values: seq<bool>) =
    match values with
    | :? DateSet.Dates as dates -> dates.Key
    | _ -> (DateSet.Dates.Of values).Key

let normalizeName (value: string) =
    value.ToLowerInvariant()
    |> Seq.map (fun value -> if Char.IsLetterOrDigit(value) then value else ' ')
    |> Array.ofSeq
    |> String
    |> fun value -> value.Split(' ', StringSplitOptions.RemoveEmptyEntries)

let private canonicalStopNameTokens (value: string) =
    let tokens = normalizeName value
    let expanded = ResizeArray<string>()
    let mutable index = 0
    while index < tokens.Length do
        let token = tokens.[index]
        let next = if index + 1 < tokens.Length then tokens.[index + 1] else ""
        match token, next with
        | "žel", "st" ->
            expanded.Add("železniční")
            expanded.Add("stanice")
            index <- index + 2
        | "čerp", "st" ->
            expanded.Add("čerpací")
            expanded.Add("stanice")
            index <- index + 2
        | "obch", "stř" ->
            expanded.Add("obchodní")
            expanded.Add("středisko")
            index <- index + 2
        | "obú", _ ->
            expanded.Add("obecní")
            expanded.Add("úřad")
            index <- index + 1
        | "nám", _ -> expanded.Add("náměstí"); index <- index + 1
        | "rozc", _ -> expanded.Add("rozcestí"); index <- index + 1
        | "rest", _ -> expanded.Add("restaurace"); index <- index + 1
        | "zast", _ -> expanded.Add("zastávka"); index <- index + 1
        | "dol", _ -> expanded.Add("dolní"); index <- index + 1
        | "hor", _ -> expanded.Add("horní"); index <- index + 1
        | "nem", _ -> expanded.Add("nemocnice"); index <- index + 1
        | "kult", _ -> expanded.Add("kulturní"); index <- index + 1
        | "mech", _ -> expanded.Add("mechanizační"); index <- index + 1
        | "stř", _ -> expanded.Add("středisko"); index <- index + 1
        | "záv", _ -> expanded.Add("závod"); index <- index + 1
        | "n", _ -> expanded.Add("nad"); index <- index + 1
        | _ -> expanded.Add(token); index <- index + 1
    expanded.ToArray()

let stopNameMatchRank (leftRaw: string) (rightRaw: string) =
    let left = canonicalStopNameTokens leftRaw
    let right = canonicalStopNameTokens rightRaw
    let localitySuffix (shorter: string array) (longerRaw: string) =
        let comma = longerRaw.IndexOf(',')
        comma >= 0 && canonicalStopNameTokens longerRaw.[comma + 1..] = shorter
    let prefixEquivalent (left: string array) (right: string array) =
        left.Length = right.Length
        && Array.zip left right
           |> Array.forall (fun (a, b) ->
               a = b
               || (min a.Length b.Length >= 3 && (a.StartsWith(b, StringComparison.Ordinal) || b.StartsWith(a, StringComparison.Ordinal))))
    if left = right then Some 0
    elif localitySuffix left rightRaw || localitySuffix right leftRaw then Some (abs (left.Length - right.Length))
    elif prefixEquivalent left right then
        Some (Array.zip left right |> Array.sumBy (fun (a, b) -> if a = b then 0 else 1))
    else None

let compatibleModeClasses (left: string) (right: string) =
    left = right
    || (Set.ofList [ "bus"; "trolleybus" ] |> fun road -> road.Contains(left) && road.Contains(right))

let structurallyCompatibleRouteLabels (leftRaw: string) (rightRaw: string) =
    let normalize (value: string) =
        value.ToUpperInvariant()
        |> Seq.filter Char.IsLetterOrDigit
        |> Array.ofSeq
        |> String
    let left, right = normalize leftRaw, normalize rightRaw
    let prefixVariant (shorter: string) (longer: string) =
        shorter.Length = 1
        && Char.IsLetter(shorter.[0])
        && longer.StartsWith(shorter, StringComparison.Ordinal)
        && longer.[shorter.Length..] |> Seq.forall Char.IsDigit
    left = right || (left <> "" && right <> "" && (prefixVariant left right || prefixVariant right left))

let haversineMetres lat1 lon1 lat2 lon2 =
    let radians value = value * Math.PI / 180.0
    let dLat = radians (lat2 - lat1)
    let dLon = radians (lon2 - lon1)
    let a =
        Math.Sin(dLat / 2.0) ** 2.0
        + Math.Cos(radians lat1) * Math.Cos(radians lat2) * Math.Sin(dLon / 2.0) ** 2.0
    6371000.0 * 2.0 * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1.0 - a))

let csvFields archivePath fileName =
    seq {
        let openStream () =
            if Directory.Exists(archivePath) then
                let path = Path.Combine(archivePath, fileName)
                if not (File.Exists(path)) then None
                else Some (File.OpenRead(path) :> Stream, None)
            else
                let archive = ZipFile.OpenRead(archivePath)
                let entry =
                    archive.Entries
                    |> Seq.tryFind (fun value ->
                        value.FullName.Replace('\\', '/').TrimStart('/').Equals(
                            fileName.Replace('\\', '/').TrimStart('/'),
                            StringComparison.OrdinalIgnoreCase))
                match entry with
                | Some value -> Some (value.Open(), Some archive)
                | None -> archive.Dispose(); None
        match openStream () with
        | None -> ()
        | Some (stream, owner) ->
            use stream = stream
            use _owner = owner |> Option.map (fun value -> value :> IDisposable) |> Option.toObj
            use reader = new StreamReader(stream, Encoding.UTF8, true)
            use parser = new TextFieldParser(reader)
            parser.TextFieldType <- FieldType.Delimited
            parser.SetDelimiters(",")
            parser.HasFieldsEnclosedInQuotes <- true
            parser.TrimWhiteSpace <- false
            if not parser.EndOfData then
                let header = parser.ReadFields()
                if header |> Array.exists String.IsNullOrWhiteSpace then
                    invalidOp $"{fileName} has an empty column name"
                if header |> Array.distinct |> Array.length <> header.Length then
                    invalidOp $"{fileName} has duplicate column names"
                while not parser.EndOfData do
                    let fields = parser.ReadFields()
                    if not (isNull fields) && not (fields.Length = 1 && fields.[0] = "") then
                        yield header, fields
    }

let textLines archivePath fileName = seq {
    let openStream () =
        if Directory.Exists(archivePath) then
            let path = Path.Combine(archivePath, fileName)
            if File.Exists(path) then Some (File.OpenRead(path) :> Stream, None) else None
        else
            let archive = ZipFile.OpenRead(archivePath)
            match archive.Entries |> Seq.tryFind (fun entry -> entry.FullName.Replace('\\', '/').Equals(fileName, StringComparison.OrdinalIgnoreCase)) with
            | Some entry -> Some (entry.Open(), Some archive)
            | None -> archive.Dispose(); None
    match openStream () with
    | None -> ()
    | Some (stream, owner) ->
        use stream = stream
        use _owner = owner |> Option.map (fun value -> value :> IDisposable) |> Option.toObj
        use reader = new StreamReader(stream, Encoding.UTF8, true)
        while not reader.EndOfStream do yield reader.ReadLine()
}

let csvRows archivePath fileName =
    csvFields archivePath fileName
    |> Seq.map (fun (header, fields) ->
        let row = CsvRow(StringComparer.Ordinal)
        for index in 0 .. header.Length - 1 do
            row.[header.[index]] <- if index < fields.Length then fields.[index] else ""
        row)

/// Project a table into a fixed header once, without a dictionary per input row.
let csvValues archivePath fileName (columns: string array) = seq {
    let mutable indexes = [||]
    for header, fields in csvFields archivePath fileName do
        if indexes.Length = 0 then
            indexes <- columns |> Array.map (fun column -> header |> Array.tryFindIndex ((=) column) |> Option.defaultValue -1)
        yield indexes |> Array.map (fun index -> if index >= 0 && index < fields.Length then fields.[index] else "")
}

let requireTable archivePath fileName =
    let rows = csvRows archivePath fileName |> Seq.toArray
    if rows.Length = 0 then invalidOp $"Required GTFS table is empty or missing: {fileName}"
    rows

let optionalTable archivePath fileName =
    csvRows archivePath fileName |> Seq.toArray

let readDescriptor (path: string) =
    use document = JsonDocument.Parse(File.ReadAllText(path))
    let root = document.RootElement
    let property (names: string list) : JsonElement =
        names
        |> Seq.tryPick (fun name ->
            match root.TryGetProperty(name) with
            | true, value -> Some value
            | _ -> None)
        |> Option.defaultWith (fun () ->
            let joinedNames = String.concat "/" names
            invalidOp $"Descriptor is missing {joinedNames}")
    {
        retrievedAt = property ["retrieved_at"; "retrievedAt"] |> fun value -> value.GetString() |> DateTimeOffset.Parse
        payloadSha256 = property ["payload_sha256"; "payloadSha256"; "sha256"] |> fun value -> value.GetString().ToLowerInvariant()
    }

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

let periodOption value =
    if String.IsNullOrWhiteSpace(value) then None
    else
        let parts = value.Split(':') |> Array.map int64
        if parts.Length <> 3 then invalidOp $"Invalid GTFS time: {value}"
        Some (Period.FromSeconds(parts.[0] * 3600L + parts.[1] * 60L + parts.[2]))

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

let routeRow (row: CsvRow) : Route = {
    id = rowValue row "route_id"
    agencyId = rowValue row "agency_id" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    shortName = rowValue row "route_short_name" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    longName = rowValue row "route_long_name" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    description = rowValue row "route_desc" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    routeType = rowValue row "route_type"
    url = rowValue row "route_url" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    color = rowValue row "route_color" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    textColor = rowValue row "route_text_color" |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    sortOrder = rowValue row "route_sort_order" |> fun value -> if value = "" then None else Some (parseInt value)
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

let matchingTimeNormalizer (policy: TripMatchPolicy) =
    if policy.timeResolutionSeconds <= 0 then invalidOp "trip_match.time_resolution_seconds must be positive"
    if policy.timeRounding <> "floor" then invalidOp "trip_match.time_rounding must be 'floor'"
    fun (value: string) ->
        if String.IsNullOrWhiteSpace(value) then ""
        else
            let parts = value.Split(':') |> Array.map int64
            if parts.Length <> 3 then invalidOp $"Invalid GTFS time: {value}"
            let seconds = parts.[0] * 3600L + parts.[1] * 60L + parts.[2]
            let resolution = int64 policy.timeResolutionSeconds
            string ((seconds / resolution) * resolution)

let signature (normalizeTime: string -> string) (lineId: string) (calls: CallValue array) (includeAllTimes: bool) (includeLast: bool) =
    let builder = StringBuilder(lineId).Append('|')
    for call in calls do
        builder.Append(call.stopPlaceId).Append(';') |> ignore
        if includeAllTimes then
            builder.Append(normalizeTime call.arrival).Append('/').Append(normalizeTime call.departure).Append(';') |> ignore
    if not includeAllTimes && calls.Length > 0 then
        builder.Append('|').Append(normalizeTime calls.[0].departure) |> ignore
        if includeLast then builder.Append('|').Append(normalizeTime calls.[calls.Length - 1].arrival) |> ignore
    sha256Text (builder.ToString())

let stopPatternKey (pattern: string array) =
    pattern |> String.concat "\u001f" |> sha256Text

let timeSeconds (value: string) =
    if String.IsNullOrWhiteSpace(value) then -1
    else
        let parts = value.Split(':')
        if parts.Length <> 3 then invalidOp $"Invalid GTFS time: {value}"
        parseInt parts.[0] * 3600 + parseInt parts.[1] * 60 + parseInt parts.[2]

let overlayEquivalenceDigest (calls: CallValue array) =
    let builder = StringBuilder()
    for call in calls do
        builder
            .Append(call.stopPlaceId).Append('/')
            .Append(timeSeconds call.arrival).Append('/')
            .Append(timeSeconds call.departure).Append(';')
        |> ignore
    sha256Text (builder.ToString())

let normalizedTimeSeconds (normalizeTime: string -> string) value =
    let normalized = normalizeTime value
    if String.IsNullOrWhiteSpace(normalized) then -1 else parseInt normalized

let identityAlignment length = Array.init length Some

let patternEditAlignment (policy: PatternEditPolicy) (source: string array) (target: string array) =
    if not policy.enabled || source.Length = 0 || target.Length = 0 then None
    elif policy.requireSameEndpoints && (source.[0] <> target.[0] || source.[source.Length - 1] <> target.[target.Length - 1]) then None
    elif abs (source.Length - target.Length) > policy.maximumEdits then None
    else
        let positionalMatches =
            Array.zip source.[0 .. min source.Length target.Length - 1] target.[0 .. min source.Length target.Length - 1]
            |> Array.sumBy (fun (left, right) -> if left = right then 1 else 0)
        let mutable commonPrefix = 0
        while commonPrefix < min source.Length target.Length && source.[commonPrefix] = target.[commonPrefix] do
            commonPrefix <- commonPrefix + 1
        let mutable commonSuffix = 0
        while commonSuffix < min source.Length target.Length - commonPrefix
              && source.[source.Length - commonSuffix - 1] = target.[target.Length - commonSuffix - 1] do
            commonSuffix <- commonSuffix + 1
        let quickSupport = max positionalMatches (commonPrefix + commonSuffix)
        if quickSupport < min source.Length target.Length - policy.maximumEdits then None
        else
            let rows = source.Length + 1
            let columns = target.Length + 1
            let costs = Array2D.zeroCreate<int> rows columns
            for sourceIndex in 0 .. source.Length do costs.[sourceIndex, 0] <- sourceIndex
            for targetIndex in 0 .. target.Length do costs.[0, targetIndex] <- targetIndex
            for sourceIndex in 1 .. source.Length do
                for targetIndex in 1 .. target.Length do
                    let substitution = costs.[sourceIndex - 1, targetIndex - 1] + (if source.[sourceIndex - 1] = target.[targetIndex - 1] then 0 else 1)
                    costs.[sourceIndex, targetIndex] <- min substitution (min (costs.[sourceIndex - 1, targetIndex] + 1) (costs.[sourceIndex, targetIndex - 1] + 1))
            let edits = costs.[source.Length, target.Length]
            let agreement = 1.0 - float edits / float (max source.Length target.Length)
            if edits > policy.maximumEdits || agreement < policy.minimumAgreement then None
            else
                let alignment = Array.create target.Length None
                let mutable sourceIndex = source.Length
                let mutable targetIndex = target.Length
                while sourceIndex > 0 || targetIndex > 0 do
                    if sourceIndex > 0 && targetIndex > 0
                       && costs.[sourceIndex, targetIndex] = costs.[sourceIndex - 1, targetIndex - 1] + (if source.[sourceIndex - 1] = target.[targetIndex - 1] then 0 else 1) then
                        if source.[sourceIndex - 1] = target.[targetIndex - 1] then alignment.[targetIndex - 1] <- Some (sourceIndex - 1)
                        sourceIndex <- sourceIndex - 1
                        targetIndex <- targetIndex - 1
                    elif sourceIndex > 0 && costs.[sourceIndex, targetIndex] = costs.[sourceIndex - 1, targetIndex] + 1 then
                        sourceIndex <- sourceIndex - 1
                    else
                        targetIndex <- targetIndex - 1
                Some (edits, alignment)

let sourceTimeArrays (sourceCalls: CallValue array) =
    sourceCalls |> Array.map (fun call -> timeSeconds call.arrival),
    sourceCalls |> Array.map (fun call -> timeSeconds call.departure)

let candidateTimeScore (sourceArrivals: int array) (sourceDepartures: int array) (target: BaseSignature) (sourceOrdinalByTarget: int option array) =
    let firstSource = sourceDepartures |> Array.tryFind ((<=) 0) |> Option.defaultValue -1
    let firstTarget = target.departures |> Array.tryFind ((<=) 0) |> Option.defaultValue -1
    let lastSource = sourceArrivals |> Array.tryFindBack ((<=) 0) |> Option.defaultValue -1
    let lastTarget = target.arrivals |> Array.tryFindBack ((<=) 0) |> Option.defaultValue -1
    let firstDelta = if firstSource < 0 || firstTarget < 0 then Int32.MaxValue / 4 else abs (firstSource - firstTarget)
    let durationDelta =
        if firstSource < 0 || firstTarget < 0 || lastSource < 0 || lastTarget < 0 then Int32.MaxValue / 4
        else abs ((lastSource - firstSource) - (lastTarget - firstTarget))
    let mutable aggregate = 0L
    let mutable maximum = 0
    let mutable squared = 0L
    let mutable comparable = 0
    for targetIndex in 0 .. sourceOrdinalByTarget.Length - 1 do
        match sourceOrdinalByTarget.[targetIndex] with
        | Some sourceIndex when sourceIndex < sourceArrivals.Length && targetIndex < target.arrivals.Length ->
            if sourceArrivals.[sourceIndex] >= 0 && target.arrivals.[targetIndex] >= 0 then
                let delta = abs (sourceArrivals.[sourceIndex] - target.arrivals.[targetIndex])
                aggregate <- aggregate + int64 delta
                maximum <- max maximum delta
                squared <- squared + int64 delta * int64 delta
                comparable <- comparable + 1
            if sourceDepartures.[sourceIndex] >= 0 && target.departures.[targetIndex] >= 0 then
                let delta = abs (sourceDepartures.[sourceIndex] - target.departures.[targetIndex])
                aggregate <- aggregate + int64 delta
                maximum <- max maximum delta
                squared <- squared + int64 delta * int64 delta
                comparable <- comparable + 1
        | _ -> ()
    let aggregate = if comparable = 0 then Int64.MaxValue / 4L else aggregate
    let squared = if comparable = 0 then Int64.MaxValue / 4L else squared
    let alignedCalls = sourceOrdinalByTarget |> Array.sumBy (function Some _ -> 1 | None -> 0)
    firstDelta, aggregate, durationDelta, maximum, squared, alignedCalls

let candidateRank (candidate: CandidateMatch) =
    struct (
        candidate.editCount,
        candidate.firstDepartureDelta,
        candidate.aggregateTimeDelta,
        candidate.durationDelta,
        candidate.maximumTimeDelta,
        candidate.squaredTimeDelta,
        -candidate.alignedCallCount)

let candidateRankMargin (best: CandidateMatch) (runnerUp: CandidateMatch) =
    if best.editCount <> runnerUp.editCount then int64 (runnerUp.editCount - best.editCount)
    elif best.firstDepartureDelta <> runnerUp.firstDepartureDelta then int64 (runnerUp.firstDepartureDelta - best.firstDepartureDelta)
    elif best.aggregateTimeDelta <> runnerUp.aggregateTimeDelta then runnerUp.aggregateTimeDelta - best.aggregateTimeDelta
    elif best.durationDelta <> runnerUp.durationDelta then int64 (runnerUp.durationDelta - best.durationDelta)
    elif best.maximumTimeDelta <> runnerUp.maximumTimeDelta then int64 (runnerUp.maximumTimeDelta - best.maximumTimeDelta)
    elif best.squaredTimeDelta <> runnerUp.squaredTimeDelta then runnerUp.squaredTimeDelta - best.squaredTimeDelta
    else int64 (best.alignedCallCount - runnerUp.alignedCallCount)

let rec copyDirectory source target =
    Directory.CreateDirectory(target) |> ignore
    for file in Directory.EnumerateFiles(source) do
        File.Copy(file, Path.Combine(target, Path.GetFileName(file)), false)
    for directory in Directory.EnumerateDirectories(source) do
        copyDirectory directory (Path.Combine(target, Path.GetFileName(directory)))

let csvEscape (value: string) =
    "\"" + (if isNull value then "" else value.Replace("\"", "\"\"")) + "\""

let writeCsvRow (writer: TextWriter) (fields: seq<string>) =
    let mutable first = true
    for field in fields do
        if not first then writer.Write(',')
        first <- false
        writer.Write('"')
        if not (isNull field) then
            for character in field do
                if character = '"' then writer.Write('"')
                writer.Write(character)
        writer.Write('"')
    writer.WriteLine()

let writeValues (path: string) (columns: string array) (rows: string array seq) =
    Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
    use writer = new StreamWriter(path, false, new UTF8Encoding(false))
    writer.NewLine <- "\n"
    writeCsvRow writer columns
    for row in rows do writeCsvRow writer row

let writeRows (path: string) (columns: string array) (rows: CsvRow seq) =
    writeValues path columns (rows |> Seq.map (fun row -> columns |> Array.map (rowValue row)))

let columnsOf archivePath fileName =
    let row = csvRows archivePath fileName |> Seq.tryHead
    match row with
    | Some value -> value.Keys |> Seq.toArray
    | None -> [||]

let optionText (value: string) =
    if String.IsNullOrWhiteSpace(value) then None else Some value

let modeClass (routeType: string) =
    match Int32.TryParse(routeType, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | false, _ -> "other:" + routeType
    | true, value when value = 0 || (value >= 900 && value <= 906) -> "tram"
    | true, value when value = 1 || (value >= 400 && value <= 405) -> "metro"
    | true, value when value = 2 || (value >= 100 && value <= 117) -> "heavy-rail"
    | true, value when value = 3 || (value >= 700 && value <= 716) -> "bus"
    | true, value when value = 4 || (value >= 1000 && value <= 1021) -> "ferry"
    | true, value when value = 11 || value = 800 -> "trolleybus"
    | true, value when value = 12 || (value >= 1200 && value <= 1207) -> "other-guided"
    | true, value -> "other:" + string value

let datesFor (window: DateWindow) (values: seq<bool>) =
    values
    |> Seq.mapi (fun index active -> if active then Some window.dates.[index] else None)
    |> Seq.choose id
    |> Seq.toArray

let parseOverridePath policyPath configuredPath =
    if String.IsNullOrWhiteSpace(configuredPath) then None
    else
        let path =
            if Path.IsPathRooted(configuredPath) then configuredPath
            else Path.Combine(Path.GetDirectoryName(Path.GetFullPath(policyPath)), configuredPath)
        if not (File.Exists(path)) then invalidOp $"Configured override CSV does not exist: {path}"
        Some path

type OverrideBinding = {
    sourceNamespace: string
    sourceId: string
    targetNamespace: string
    targetId: string
    validFrom: LocalDate
    validTo: LocalDate
    reviewNote: string
}

let loadOverrides policyPath configuredPath =
    match parseOverridePath policyPath configuredPath with
    | None -> [||]
    | Some path ->
        csvRows (Path.GetDirectoryName(path)) (Path.GetFileName(path))
        |> Seq.map (fun row ->
            let sourceNamespace = rowValue row "source_namespace"
            let targetNamespace = rowValue row "target_namespace"
            let note = rowValue row "review_note"
            if String.IsNullOrWhiteSpace(sourceNamespace)
               || String.IsNullOrWhiteSpace(targetNamespace)
               || String.IsNullOrWhiteSpace(note) then
                invalidOp $"Override {path} must include source_namespace, target_namespace, and review_note"
            {
                sourceNamespace = sourceNamespace
                sourceId = rowValue row "source_id"
                targetNamespace = targetNamespace
                targetId = rowValue row "target_id"
                validFrom = parseDate (rowValue row "valid_from")
                validTo = parseDate (rowValue row "valid_to")
                reviewNote = note
            })
        |> Seq.toArray

let capability policy name =
    match policy.source.capabilities.TryGetValue(name) with
    | true, value -> value
    | _ -> { mode = "disabled"; priority = 0 }

let enabled policy name = (capability policy name).mode <> "disabled"

let currentCommit () =
    try
        let rec findRepository (directory: DirectoryInfo) =
            if isNull directory then None
            elif Directory.Exists(Path.Combine(directory.FullName, ".git")) || File.Exists(Path.Combine(directory.FullName, ".git")) then Some directory.FullName
            else findRepository directory.Parent
        let info = ProcessStartInfo("git", "rev-parse HEAD")
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        info.UseShellExecute <- false
        info.CreateNoWindow <- true
        findRepository (DirectoryInfo(AppContext.BaseDirectory))
        |> Option.iter (fun path -> info.WorkingDirectory <- path)
        use childProcess = Process.Start(info)
        let value = childProcess.StandardOutput.ReadToEnd().Trim()
        childProcess.WaitForExit()
        if childProcess.ExitCode = 0 && value.Length = 40 then value
        else
            Assembly.GetExecutingAssembly().GetName().Version.ToString()
    with _ -> Assembly.GetExecutingAssembly().GetName().Version.ToString()

type StopGroup = {
    groupId: string
    name: string
    lat: decimal option
    lon: decimal option
    members: CsvRow array
}

let averageCoordinate column (rows: CsvRow array) =
    let values = rows |> Array.choose (rowValue >> fun reader -> reader column |> parseDecimalOpt)
    if values.Length = 0 then None else Some (Array.average values)

let stopGroupPoints (group: StopGroup) =
    let memberPoints =
        group.members
        |> Array.choose (fun row ->
            match parseDecimalOpt (rowValue row "stop_lat"), parseDecimalOpt (rowValue row "stop_lon") with
            | Some lat, Some lon -> Some (lat, lon)
            | _ -> None)
        |> Array.distinct
    if memberPoints.Length > 0 then memberPoints
    else
        match group.lat, group.lon with
        | Some lat, Some lon -> [| lat, lon |]
        | _ -> [||]

let groupSourceStops (policy: OverlayPolicy) (scheduledStopIds: Set<string>) (rows: CsvRow array) =
    let byId = rows |> Array.map (fun row -> rowValue row "stop_id", row) |> dict
    rows
    |> Array.filter (fun row -> scheduledStopIds.Contains(rowValue row "stop_id"))
    |> Array.groupBy (fun row ->
        match optionText (rowValue row "parent_station") with
        | Some parent -> parent
        | None ->
            match optionText (rowValue row policy.source.stopMatch.groupColumn) with
            | Some group when policy.source.stopMatch.splitFlatGroupsByName ->
                "group:" + group + ":" + (normalizeName (rowValue row "stop_name") |> String.concat "-")
            | Some group -> "group:" + group
            | None -> "stop:" + rowValue row "stop_id")
    |> Array.map (fun (groupId, members) ->
        let representative =
            let explicitParent =
                members
                |> Array.tryPick (fun row -> optionText (rowValue row "parent_station"))
            match explicitParent with
            | Some parent when byId.ContainsKey(parent) -> byId.[parent]
            | _ -> members.[0]
        {
            groupId = groupId
            name = rowValue representative "stop_name"
            lat = averageCoordinate "stop_lat" members
            lon = averageCoordinate "stop_lon" members
            members = members
        })

let groupBaseStops (rows: CsvRow array) (placeByStop: IDictionary<string, string>) =
    let byId = rows |> Array.map (fun row -> rowValue row "stop_id", row) |> dict
    rows
    |> Array.groupBy (fun row ->
        let id = rowValue row "stop_id"
        match placeByStop.TryGetValue(id) with
        | true, place -> place
        | _ -> optionText (rowValue row "parent_station") |> Option.defaultValue id)
    |> Array.map (fun (groupId, members) ->
        let representative = if byId.ContainsKey(groupId) then byId.[groupId] else members.[0]
        {
            groupId = groupId
            name = rowValue representative "stop_name"
            lat = parseDecimalOpt (rowValue representative "stop_lat") |> Option.orElseWith (fun () -> averageCoordinate "stop_lat" members)
            lon = parseDecimalOpt (rowValue representative "stop_lon") |> Option.orElseWith (fun () -> averageCoordinate "stop_lon" members)
            members = members
        })

let datesOverlap (left: DateSet.Dates) right = left.Overlaps(right)

let addDiagnostic (diagnostics: DiagnosticLog) code objectId message =
    diagnostics.Add({ code = code; sourceObjectId = objectId; message = message })

let setRow (row: CsvRow) name value =
    row.[name] <- value
    row

let cloneRow (row: CsvRow) =
    let result = CsvRow(StringComparer.Ordinal)
    for KeyValue(key, value) in row do result.[key] <- value
    result

let serviceId dates = "overlay:service:" + (dateKey dates).Substring(0, 16)

let sourcePostId sourceId sourceStopId =
    "overlay:" + sourceId + ":post:" + (sha256Text sourceStopId).Substring(0, 16)

let sourceStopPlaceId sourceId sourceGroupId =
    "overlay:" + sourceId + ":stop-place:" + (sha256Text sourceGroupId).Substring(0, 16)

let sourceRouteOutputId sourceId sourceRouteId cisLineId =
    "overlay:" + sourceId + ":route:" + (sha256Text (sourceRouteId + "|" + cisLineId)).Substring(0, 16)

let sourceAgencyOutputId sourceId sourceAgencyId =
    "overlay:" + sourceId + ":agency:" + (sha256Text sourceAgencyId).Substring(0, 16)

let splitTripId baseTripId dates selectionFingerprint =
    let key = baseTripId + "|" + dateKey dates + "|" + selectionFingerprint
    baseTripId + ":overlay:" + (sha256Text key).Substring(0, 12)

let addedSourceTripId sourceId sourceTripId dates =
    let key = sourceId + "|" + sourceTripId + "|" + dateKey dates
    "overlay:" + sourceId + ":trip:" + (sha256Text key).Substring(0, 20)

/// Historical encoding used in split-trip IDs. Keep the exact formatting stable.
let selectionEncoding (value: OverlaySelection) =
    String.concat "|" [
        String.concat ";" value.outputStopIds
        value.outputShapeId |> Option.defaultValue ""
        value.distances
        |> Array.map (Option.map (fun item -> item.ToString(CultureInfo.InvariantCulture)) >> Option.defaultValue "")
        |> String.concat ";"
        value.arrivals |> Array.map (Option.defaultValue "") |> String.concat ";"
        value.departures |> Array.map (Option.defaultValue "") |> String.concat ";"
    ]

let selectionKey selection =
    match selection with | None -> "base" | Some value -> value.factKey

let descriptorRetrievedAtFromBase basePath =
    let manifestPath = Path.Combine(basePath, "manifest.json")
    if not (File.Exists(manifestPath)) then invalidOp "Base bundle is missing manifest.json"
    use document = JsonDocument.Parse(File.ReadAllText(manifestPath))
    let root = document.RootElement
    let mutable snapshot = Unchecked.defaultof<JsonElement>
    let mutable retrieved = Unchecked.defaultof<JsonElement>
    if root.TryGetProperty("source_snapshot", &snapshot) then ()
    else
        let mutable sources = Unchecked.defaultof<JsonElement>
        if root.TryGetProperty("sources", &sources) && sources.GetArrayLength() > 0 then
            snapshot <- sources.[0]
        else invalidOp "Base manifest does not pin source retrieval metadata"
    if not (snapshot.TryGetProperty("retrieved_at", &retrieved)) then
        invalidOp "Base manifest does not pin source retrieved_at"
    DateTimeOffset.Parse(retrieved.GetString(), CultureInfo.InvariantCulture)

let validateInputs policyPath basePath outputPath gvdYear (binding: SourceBinding) =
    if gvdYear < 2000 || gvdYear > 9999 then invalidArg "--gvd-year" "GVD year is invalid"
    if not (Directory.Exists(basePath)) then invalidArg "base-bundle" $"Base bundle does not exist: {basePath}"
    if not (File.Exists(Path.Combine(basePath, "gtfs.zip")))
       && not (Directory.Exists(Path.Combine(basePath, "gtfs-intermediate"))) then
        invalidArg "base-bundle" "Base bundle has neither a production GTFS ZIP nor compiler staging GTFS"
    if Directory.Exists(outputPath) || File.Exists(outputPath) then
        invalidArg "output-bundle" "Output already exists; overlay bundles are immutable"
    if not (File.Exists(policyPath)) then invalidArg "--policy" $"Policy does not exist: {policyPath}"
    if not (File.Exists(binding.payloadPath) || Directory.Exists(binding.payloadPath)) then
        invalidArg "--source" $"Source payload does not exist: {binding.payloadPath}"
    if not (File.Exists(binding.descriptorPath)) then
        invalidArg "--source-descriptor" $"Source descriptor does not exist: {binding.descriptorPath}"

/// Resolve display abbreviations only when they identify one downstream full name.
let resolveFullHeadsignDetailed (raw: string) (destinations: string array) =
    let tokens (value: string) =
        Regex.Matches(value.ToLowerInvariant(), @"[\p{L}\p{N}]+")
        |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.toArray
    let forms (value: string) =
        [| yield tokens value
           let comma = value.IndexOf(',')
           if comma >= 0 then yield tokens value.[comma + 1..] |]
        |> Array.filter (fun value -> value.Length > 0)
        |> Array.distinct
    let abbreviatedForms = forms raw
    let tokenMatches short long =
        short = long
        || match short with
           | "žel" -> long = "železniční"
           | "st" -> long = "stanice"
           | "aut" -> long = "autobusové" || long = "autobusová"
           | "nám" -> long = "náměstí"
           | "sídl" -> long = "sídliště"
           | "zast" -> long = "zastávka"
           | _ -> false
    let matches name =
        forms name
        |> Array.exists (fun full ->
            abbreviatedForms
            |> Array.exists (fun abbreviated ->
                full.Length = abbreviated.Length
                && Array.zip abbreviated full
                   |> Array.forall (fun (short, long) -> tokenMatches short long)))
    let unique values = values |> Array.distinct
    if Regex.IsMatch(raw, @"(?i)\b(via|přes)\b|[→;]") then None, "via_or_composite"
    else
        let exact = destinations |> unique |> Array.filter (fun name -> tokens name = tokens raw)
        let locality = destinations |> unique |> Array.filter (fun name -> stopNameMatchRank raw name |> Option.isSome)
        if exact.Length = 1 then Some exact.[0], "exact_full_name"
        elif locality.Length = 1 then Some locality.[0], "locality_prefix"
        elif destinations.Length > 0 && matches destinations.[destinations.Length - 1] then
            Some destinations.[destinations.Length - 1], "terminal_name"
        else
            match destinations |> unique |> Array.filter matches with
            | [| name |] -> Some name, "known_abbreviation"
            | [||] -> None, "no_destination_match"
            | _ -> None, "ambiguous_destination"

let resolveFullHeadsign raw destinations = resolveFullHeadsignDetailed raw destinations |> fst
