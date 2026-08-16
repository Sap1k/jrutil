// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfBundle

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open NodaTime
open Parquet
open Parquet.Schema
open Parquet.Serialization
open Serilog

open JrUtil

type PostInferenceExecutionMode = Disabled | CaptureOnly | Live | Replay

type PostInferenceCaptureMetrics = {
    estimatedEvidenceBytes: int64
    atomicOutputHeadroomBytes: int64
    currentSpillBytes: int64
    peakSpillBytes: int64
    maximumWorkers: int
}

type BundleExecutionResult =
    | BundleCompleted
    | CaptureCompleted of JdfPostInference.PostInferenceEvidenceManifest * PostInferenceCaptureMetrics

[<Literal>]
let BundleVersion = 1

[<Literal>]
let ParquetSchemaVersion = 7

[<Literal>]
let PostEvidenceOutputSafetyReserveBytes = 268435456L

type SnapshotDescriptor = {
    sourceId: string
    retrievedAt: string
    retrievalMethod: string
    sourceUri: string option
    licence: string
    payloadKind: string
    payloadSha256: string
    payloadBytes: int64
}

type Diagnostic = {
    severity: string
    code: string
    sourceObjectId: string
    message: string
}

let estimatePostEvidenceOutputBytes
        (bounds:JdfPostEvidence.PostEvidenceCaptureUpperBounds) =
    // These deliberately conservative encoded-row allowances are applied to
    // the canonical, deduplicated capture plan.  Timetable call count is not
    // evidence cardinality: identical trip patterns share one routed context.
    bounds.observationCount*384L
    + bounds.routePointCount*320L
    + bounds.contextCount*1024L
    + bounds.corridorVariantCount*512L
    + bounds.routePointEvidenceCount*384L

type private FileEntry = {
    path: string
    sha256: string
    bytes: int64
    rows: int option
}

type private ParquetTable = {
    fields: DataField array
    rows: IReadOnlyCollection<IDictionary<string, obj>>
}

type private DerivedPostAssignmentRow = {
    targetGtfsStopId: string
    assignmentKind: string
    derivedLocationId: string option
    mode: string
    lineId: string option
    direction: int option
    patternHash: string option
    patternPosition: int option
    movementFamilyId: string option
    contextPreviousStopId: string option
    contextNextStopId: string option
    sameStopBlockRole: string
    score: double option
    margin: double option
    selectedCandidates: string
    rejectedCandidates: string
    status: string
}

type BundleProgressEvent = {
    phase: string
    state: string
    completed: int64
    total: int64 option
    unit: string
    detail: string option
    elapsedMilliseconds: int64
    activeWorkers: int
    privateBytes: int64
    workingSetBytes: int64
}

type BundleExecutionOptions = {
    maximumWorkers: int
    memoryBudgetBytes: int64
    reviewStopsPath: string option
    capturePostInferenceEvidencePath: string option
    postInferenceEvidenceOnly: bool
    postInferenceEvidencePath: string option
    postInferencePolicyPath: string option
    includePostInferenceScores: bool
    progress: BundleProgressEvent -> unit
}

let defaultBundleExecutionOptions = {
    maximumWorkers = 1
    memoryBudgetBytes = Int64.MaxValue
    reviewStopsPath = None
    capturePostInferenceEvidencePath = None
    postInferenceEvidenceOnly = false
    postInferenceEvidencePath = None
    postInferencePolicyPath = None
    includePostInferenceScores = true
    progress = ignore
}

let private logPhaseResources phase (timer: Stopwatch) =
    use currentProcess = Process.GetCurrentProcess()
    currentProcess.Refresh()
    let memory = GC.GetGCMemoryInfo()
    Log.Information(
        "Bundle resource snapshot: phase={Phase}; elapsed_ms={ElapsedMs}; private_bytes={PrivateBytes}; working_set_bytes={WorkingSetBytes}; peak_working_set_bytes={PeakWorkingSetBytes}; managed_heap_bytes={ManagedHeapBytes}; fragmented_bytes={FragmentedBytes}",
        phase, timer.ElapsedMilliseconds, currentProcess.PrivateMemorySize64,
        currentProcess.WorkingSet64, currentProcess.PeakWorkingSet64, GC.GetTotalMemory(false),
        memory.FragmentedBytes)

let private reportProgress (options: BundleExecutionOptions) (timer: Stopwatch)
                           phase state completed total unit detail activeWorkers =
    use currentProcess = Process.GetCurrentProcess()
    currentProcess.Refresh()
    options.progress {
        phase = phase
        state = state
        completed = completed
        total = total
        unit = unit
        detail = detail
        elapsedMilliseconds = timer.ElapsedMilliseconds
        activeWorkers = activeWorkers
        privateBytes = currentProcess.PrivateMemorySize64
        workingSetBytes = currentProcess.WorkingSet64
    }

let private applyDiagnosticPostLabels stopIdsCis (batch: JdfModel.JdfBatch)
                                      (plan: JdfToGtfs.PostEstimationPlan)
                                      (feed: GtfsModel.GtfsFeed) =
    let candidateRanks =
        plan.physicalHypotheses
        |> Array.groupBy (fun value -> value.stopId)
        |> Array.collect (fun (stopId, values) ->
            values
            |> Array.sortBy (fun value -> value.hypothesisId)
            |> Array.mapi (fun index value -> (stopId, value.hypothesisId), index + 1))
        |> Map
    let labels =
        plan.locations
        |> Array.choose (fun selection ->
            let label =
                match selection.sideGroupId with
                | Some groupId ->
                    plan.sideGroups
                    |> Array.tryFind (fun value -> value.sideGroupId = groupId)
                    |> Option.map (fun value -> $"O-{value.sector}")
                | None ->
                    selection.representativeCandidateId
                    |> Option.bind (fun candidateId -> candidateRanks |> Map.tryFind (selection.stopId, candidateId))
                    |> Option.map (fun rank -> $"O{rank}")
            label
            |> Option.map (fun value ->
                JdfToGtfs.inferredPostId stopIdsCis plan selection, value))
        |> Map
    { feed with
        stops =
            feed.stops
            |> Array.map (fun stop ->
                if stop.platformCode.IsSome then stop else
                match labels |> Map.tryFind stop.id with
                | Some label -> { stop with platformCode = Some label }
                | None when stop.id.EndsWith(":unspecified", StringComparison.Ordinal) ->
                    { stop with platformCode = Some "?" }
                | None -> stop) }

let private requiredString (root: JsonElement) (name: string) =
    let mutable value = Unchecked.defaultof<JsonElement>
    if not (root.TryGetProperty(name, &value))
       || value.ValueKind <> JsonValueKind.String
       || String.IsNullOrWhiteSpace(value.GetString()) then
        invalidArg "snapshotDescriptor" $"Snapshot descriptor field '{name}' is required"
    value.GetString()

let private optionalString (root: JsonElement) (name: string) =
    let mutable value = Unchecked.defaultof<JsonElement>
    if not (root.TryGetProperty(name, &value)) || value.ValueKind = JsonValueKind.Null then None
    elif value.ValueKind = JsonValueKind.String then
        value.GetString()
        |> Option.ofObj
        |> Option.bind (fun text ->
            if String.IsNullOrWhiteSpace(text) then None else Some text)
    else invalidArg "snapshotDescriptor" $"Snapshot descriptor field '{name}' must be a string or null"

let loadSnapshotDescriptor path =
    use document = JsonDocument.Parse(File.ReadAllBytes(path))
    let root = document.RootElement
    let mutable schemaVersion = Unchecked.defaultof<JsonElement>
    if not (root.TryGetProperty("schema_version", &schemaVersion))
       || schemaVersion.ValueKind <> JsonValueKind.Number
       || schemaVersion.GetInt32() <> 1 then
        invalidArg "snapshotDescriptor" "Snapshot descriptor schema_version must be 1"
    let retrievedAt = requiredString root "retrieved_at"
    if not (Regex.IsMatch(retrievedAt, "(?:Z|[+-][0-9]{2}:[0-9]{2})$")) then
        invalidArg "snapshotDescriptor" "retrieved_at must include an explicit UTC offset"
    match DateTimeOffset.TryParse(retrievedAt, CultureInfo.InvariantCulture,
                                  DateTimeStyles.RoundtripKind) with
    | false, _ -> invalidArg "snapshotDescriptor" "retrieved_at is not a valid timestamp"
    | _ -> ()
    let payloadKind = requiredString root "payload_kind"
    if payloadKind <> "zip" && payloadKind <> "directory-tree" then
        invalidArg "snapshotDescriptor" "payload_kind must be 'zip' or 'directory-tree'"
    let payloadSha256 = (requiredString root "payload_sha256").ToLowerInvariant()
    if not (Regex.IsMatch(payloadSha256, "^[0-9a-f]{64}$")) then
        invalidArg "snapshotDescriptor" "payload_sha256 must contain 64 hexadecimal characters"
    let mutable payloadBytes = Unchecked.defaultof<JsonElement>
    if not (root.TryGetProperty("payload_bytes", &payloadBytes))
       || payloadBytes.ValueKind <> JsonValueKind.Number
       || payloadBytes.GetInt64() < 0L then
        invalidArg "snapshotDescriptor" "payload_bytes must be a non-negative integer"
    let sourceUri = optionalString root "source_uri"
    sourceUri |> Option.iter (fun value ->
        match Uri.TryCreate(value, UriKind.Absolute) with
        | true, _ -> ()
        | _ -> invalidArg "snapshotDescriptor" "source_uri must be an absolute URI or null")
    {
        sourceId = requiredString root "source_id"
        retrievedAt = retrievedAt
        retrievalMethod = requiredString root "retrieval_method"
        sourceUri = sourceUri
        licence = requiredString root "licence"
        payloadKind = payloadKind
        payloadSha256 = payloadSha256
        payloadBytes = payloadBytes.GetInt64()
    }

let private sha256Stream (stream: Stream) =
    SHA256.HashData(stream) |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()

let private fileSha256 path =
    use stream = File.OpenRead(path)
    sha256Stream stream

let directoryTreeIdentity path =
    let files =
        Directory.GetFiles(path, "*", SearchOption.AllDirectories)
        |> Array.map (fun file ->
            Path.GetRelativePath(path, file).Replace('\\', '/'), file)
        |> Array.sortBy fst
    use hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256)
    let mutable totalBytes = 0L
    for relativePath, filePath in files do
        let pathBytes = Encoding.UTF8.GetBytes(relativePath)
        hash.AppendData(pathBytes)
        hash.AppendData([| 0uy |])
        use stream = File.OpenRead(filePath)
        let fileHash = SHA256.HashData(stream)
        hash.AppendData(fileHash)
        totalBytes <- totalBytes + FileInfo(filePath).Length
    Convert.ToHexString(hash.GetHashAndReset()).ToLowerInvariant(), totalBytes

let private validateSnapshot descriptor inputPath =
    let actualKind, actualHash, actualBytes =
        if Directory.Exists(inputPath) then
            let hash, bytes = directoryTreeIdentity inputPath
            "directory-tree", hash, bytes
        elif File.Exists(inputPath) && Path.GetExtension(inputPath).Equals(".zip", StringComparison.OrdinalIgnoreCase) then
            "zip", fileSha256 inputPath, FileInfo(inputPath).Length
        else invalidArg "inputPath" "JDF input must be a directory or ZIP file"
    if descriptor.payloadKind <> actualKind then
        invalidArg "snapshotDescriptor" $"payload_kind is {descriptor.payloadKind}, input is {actualKind}"
    if descriptor.payloadSha256 <> actualHash then
        invalidArg "snapshotDescriptor" $"Payload SHA-256 mismatch: expected {descriptor.payloadSha256}, got {actualHash}"
    if descriptor.payloadBytes <> actualBytes then
        invalidArg "snapshotDescriptor" $"Payload byte-size mismatch: expected {descriptor.payloadBytes}, got {actualBytes}"

let private validateZip (archive: ZipArchive) =
    let files = archive.Entries |> Seq.filter (fun entry -> entry.Name <> "") |> Seq.toArray
    let normalized = HashSet<string>(StringComparer.OrdinalIgnoreCase)
    for entry in files do
        let path = entry.FullName.Replace('\\', '/')
        if Path.IsPathRooted(path)
           || path.Split('/') |> Array.exists (fun segment -> segment = ".." || segment = "") then
            invalidArg "inputPath" $"Unsafe ZIP entry: {entry.FullName}"
        if not (normalized.Add(path)) then
            invalidArg "inputPath" $"Duplicate case-insensitive ZIP entry: {entry.FullName}"
    let versionEntries =
        files
        |> Array.filter (fun entry -> entry.Name.Equals("VerzeJDF.txt", StringComparison.OrdinalIgnoreCase))
    if versionEntries.Length <> 1 then
        invalidArg "inputPath" "JDF ZIP must contain exactly one VerzeJDF.txt"
    let root =
        versionEntries.[0].FullName.Replace('\\', '/')
        |> fun path -> path.Substring(0, path.Length - versionEntries.[0].Name.Length)
    if files |> Array.exists (fun entry ->
        not (entry.FullName.Replace('\\', '/').StartsWith(root, StringComparison.OrdinalIgnoreCase))) then
        invalidArg "inputPath" "JDF ZIP contains files outside its single batch root"

let private jdfInputContainsRelation inputPath relationName =
    if Directory.Exists(inputPath) then
        Directory.EnumerateFiles(inputPath, "*", SearchOption.TopDirectoryOnly)
        |> Seq.exists (fun path ->
            Path.GetFileName(path).Equals(relationName, StringComparison.OrdinalIgnoreCase))
    else
        use archive = ZipFile.OpenRead(inputPath)
        archive.Entries
        |> Seq.exists (fun entry ->
            entry.Name.Equals(relationName, StringComparison.OrdinalIgnoreCase))

let private withJdfInput inputPath action =
    if Directory.Exists(inputPath) then action (Jdf.FsPath inputPath)
    else
        use archive = ZipFile.OpenRead(inputPath)
        validateZip archive
        action (Jdf.ZipArchive archive)

let private field<'T> name nullable = DataField<'T>(name, Nullable nullable) :> DataField

let private row values =
    let result = Dictionary<string, obj>()
    values |> Seq.iter (fun (name, value) -> result.Add(name, value))
    result :> IDictionary<string, obj>

let private nullableObj value =
    value |> Option.map box |> Option.defaultValue null

let private localDateString (value: LocalDate) =
    value.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)

let private nonEmptyText (value: string option) =
    value
    |> Option.bind (fun text ->
        if String.IsNullOrWhiteSpace(text) then None else Some text)

let private serviceNoteTypeName = function
    | JdfModel.Service -> "service"
    | JdfModel.ServiceAlso -> "service_also"
    | JdfModel.ServiceOnly -> "service_only"
    | JdfModel.NoService -> "no_service"
    | JdfModel.ServiceOddWeeks -> "odd_weeks"
    | JdfModel.ServiceEvenWeeks -> "even_weeks"
    | JdfModel.ServiceOddWeeksFromTo -> "odd_weeks_from_to"
    | JdfModel.ServiceEvenWeeksFromTo -> "even_weeks_from_to"

let private restrictionLookup (batch: JdfModel.JdfBatch) =
    let result = Dictionary<int, int>()
    for attribute in batch.attributeRefs do
        let bit =
            match attribute.value with
            | JdfModel.TravelExclusion0 -> 1
            | JdfModel.TravelExclusion1 -> 2
            | JdfModel.TravelExclusion2 -> 4
            | JdfModel.TravelExclusion3 -> 8
            | _ -> 0
        if bit <> 0 then result.[attribute.attributeId] <- bit
    result

let private restrictionMask (lookup: Dictionary<int, int>) (attributes: int option array) =
    let mutable mask = 0
    for attributeId in attributes do
        match attributeId with
        | Some id ->
            match lookup.TryGetValue(id) with
            | true, bit -> mask <- mask ||| bit
            | _ -> ()
        | None -> ()
    mask

let private restrictionGroupCodes mask = seq {
    if mask &&& 1 <> 0 then yield "§"
    if mask &&& 2 <> 0 then yield "A"
    if mask &&& 4 <> 0 then yield "B"
    if mask &&& 8 <> 0 then yield "C"
}

let private withOwnerOrdinals ownerKey values =
    values
    |> Seq.groupBy ownerKey
    |> Seq.collect (fun (_, ownedValues) ->
        ownedValues |> Seq.mapi (fun index value -> index + 1, value))

let private sourceSegment (value: string) = Uri.EscapeDataString(value)

let private routeNoticeId routeId distinction noticeId =
    $"jdf:notice:route:{sourceSegment routeId}:{distinction}:{noticeId}"

let private tripNoticeId routeId distinction tripId noticeId =
    $"jdf:notice:trip:{sourceSegment routeId}:{distinction}:{tripId}:{noticeId}"

let private reservationNoticeId routeId distinction tripId ordinal =
    $"jdf:notice:reservation:{sourceSegment routeId}:{distinction}:{tripId}:{ordinal}"

let private transferId routeId distinction tripId ordinal =
    $"jdf:transfer:{sourceSegment routeId}:{distinction}:{tripId}:{ordinal}"

let private restrictionId routeId distinction tripId routeStopId =
    $"jdf:restriction:{sourceSegment routeId}:{distinction}:{tripId}:{routeStopId}"

let private routeStopSourceId routeId distinction sourceRouteStopId =
    $"jdf:route-stop:{sourceSegment routeId}:{distinction}:{sourceRouteStopId}"

let private table fields rows =
    { fields = fields; rows = rows |> Array.map (fun value -> value :> IDictionary<string, obj>) }

let private callIsEmitted (call: JdfModel.TripStop) =
    match call.departureTime with
    | Some JdfModel.Passing | Some JdfModel.NotPassing -> false
    | None when call.arrivalTime = None -> false
    | _ -> true

type private CallDerivedFacts = {
    emittedCallCount: int64
    tripRestrictionAssignments: struct (string * int64 * string) array
    filteredRestrictionIds: string array
    unjoinableRestrictionIds: string array
    singletonRestrictionDiagnostics: Diagnostic array
    conflictingPostDiagnostics: Diagnostic array
    authoredModes: Dictionary<struct (int64 * string), HashSet<JdfModel.TransportMode>>
}

let private scanCallDerivedFacts (batch: JdfModel.JdfBatch) (retainedTrips: HashSet<string>)
                                 (candidateStopsWithMultiple: HashSet<int64>)
                                 progress =
    let restrictions = restrictionLookup batch
    let routeStops = Dictionary<struct(string*int*int64),JdfModel.RouteStop>()
    for stop in batch.routeStops do
        routeStops.[struct(stop.routeId,stop.routeDistinction,stop.routeStopId)] <- stop
    let modeByRoute = Dictionary<struct(string*int),JdfModel.TransportMode>()
    for route in batch.routes do
        modeByRoute.[struct(route.id,route.idDistinction)] <- route.transportMode
    let assignments = ResizeArray<struct(string*int64*string)>()
    let filtered = ResizeArray<string>()
    let unjoinable = ResizeArray<string>()
    let singletonDiagnostics = ResizeArray<Diagnostic>()
    let authoredModes = Dictionary<struct(int64*string),HashSet<JdfModel.TransportMode>>()
    let postNumbers = Dictionary<struct(int64*int64),HashSet<string>>()
    let mutable currentTripId: string option = None
    let mutable emittedCallCount = 0L
    let mutable restrictionMembers = Dictionary<string,HashSet<int64>>(StringComparer.Ordinal)
    let flushTrip () =
        match currentTripId with
        | None -> ()
        | Some tripId ->
            for pair in restrictionMembers do
                if pair.Value.Count = 1 then
                    singletonDiagnostics.Add({
                        severity = "warning"; code = "singleton_travel_restriction"
                        sourceObjectId = $"{tripId}:restriction-group:{sourceSegment pair.Key}"
                        message = "Effective travel-exclusion group has only one emitted call" })
        restrictionMembers <- Dictionary<string,HashSet<int64>>(StringComparer.Ordinal)
    for index=0 to batch.tripStops.Length-1 do
        let call=batch.tripStops.[index]
        let tripId=JdfToGtfs.jdfTripId call.routeId call.routeDistinction call.tripId
        if currentTripId<>Some tripId then
            flushTrip(); currentTripId <- Some tripId
        let retained=retainedTrips.Contains(tripId)
        let emitted=callIsEmitted call
        if retained && emitted then emittedCallCount <- emittedCallCount+1L
        let callRestrictionGroups =
            restrictionGroupCodes (restrictionMask restrictions call.attributes) |> Seq.toArray
        if callRestrictionGroups.Length>0 then
            let sourceId=restrictionId call.routeId call.routeDistinction call.tripId call.routeStopId
            if not retained then filtered.Add(sourceId)
            elif not emitted then unjoinable.Add(sourceId)
            else
                for groupCode in callRestrictionGroups do
                    assignments.Add(struct(tripId,call.routeStopId,groupCode))
        if retained && emitted then
            let routeStop=routeStops.[struct(call.routeId,call.routeDistinction,call.routeStopId)]
            Seq.append
                (restrictionGroupCodes (restrictionMask restrictions routeStop.attributes))
                callRestrictionGroups
            |> Seq.distinct
            |> Seq.iter (fun groupCode ->
                let members =
                    match restrictionMembers.TryGetValue(groupCode) with
                    | true,values -> values
                    | _ ->
                        let values=HashSet<int64>()
                        restrictionMembers.[groupCode] <- values
                        values
                members.Add(call.routeStopId) |> ignore)
        if candidateStopsWithMultiple.Contains(call.stopId) then
            let authoredKey =
                match call.stopPostId,nonEmptyText call.stopPostNum with
                | Some value,_ -> Some $"id:{value}"
                | None,Some value -> Some $"num:{value}"
                | _ -> None
            match authoredKey with
            | Some key ->
                let compound=struct(call.stopId,key)
                let modes =
                    match authoredModes.TryGetValue(compound) with
                    | true,values -> values
                    | _ ->
                        let values=HashSet<JdfModel.TransportMode>()
                        authoredModes.[compound] <- values
                        values
                modes.Add(modeByRoute.[struct(call.routeId,call.routeDistinction)]) |> ignore
            | None -> ()
        match call.stopPostId,call.stopPostNum |> Option.bind JdfToGtfs.nonEmptyTrimmed with
        | Some postId,Some postNumber ->
            let key=struct(call.stopId,postId)
            let numbers =
                match postNumbers.TryGetValue(key) with
                | true,values -> values
                | _ ->
                    let values=HashSet<string>(StringComparer.Ordinal)
                    postNumbers.[key] <- values
                    values
            numbers.Add(postNumber) |> ignore
        | _ -> ()
        if (index+1)%250_000=0 then progress (int64(index+1)) (Some(int64 batch.tripStops.Length))
    flushTrip()
    let conflictingPosts =
        postNumbers
        |> Seq.choose (fun pair ->
            if pair.Value.Count <= 1 then None else
            let struct(stopId,postId) = pair.Key
            let numbers = pair.Value |> Seq.sort |> String.concat ","
            Some { severity = "warning"; code = "conflicting_post_numbers"
                   sourceObjectId = $"jdf:stop:{stopId}:post:id:{postId}"
                   message = $"Authoritative post has conflicting display numbers: {numbers}" })
        |> Seq.toArray
    { emittedCallCount=emittedCallCount
      tripRestrictionAssignments=assignments |> Seq.distinct |> Seq.sort |> Seq.toArray
      filteredRestrictionIds=filtered |> Seq.distinct |> Seq.sort |> Seq.toArray
      unjoinableRestrictionIds=unjoinable |> Seq.distinct |> Seq.sort |> Seq.toArray
      singletonRestrictionDiagnostics=singletonDiagnostics |> Seq.sortBy (fun value -> value.sourceObjectId) |> Seq.toArray
      conflictingPostDiagnostics=conflictingPosts
      authoredModes=authoredModes }

// Parquet is deliberately limited to source facts that cannot be reconstructed
// from standard GTFS plus the Oběhy extension tables. Snapshot identity belongs
// in file metadata and manifest.json rather than being repeated on every row.
let private transportModeCode = function
    | JdfModel.Bus -> "A"
    | JdfModel.Tram -> "E"
    | JdfModel.Trolleybus -> "T"
    | JdfModel.CableCar -> "L"
    | JdfModel.Metro -> "M"
    | JdfModel.Ferry -> "P"

let private getTableProducers stopIdsCis (sourceTransportModes: Map<string * int, JdfModel.TransportMode>)
                      (batch: JdfModel.JdfBatch) (feed: GtfsModel.GtfsFeed)
                      (postPlan: JdfToGtfs.PostEstimationPlan)
                      (callFacts: CallDerivedFacts)
                      (progress: string -> int64 -> int64 option -> unit)
                      (emittedTransferCalls: HashSet<struct (string * int64)>) =
    let restrictionLookup = restrictionLookup batch
    let stopLocations =
        batch.stopLocations
        |> Seq.groupBy (fun location -> location.stopId)
        |> Seq.map (fun (stopId, locations) ->
            stopId,
            (locations
             |> Seq.sortBy (fun location ->
                 match location.precision with
                 | JdfModel.StopPrecise -> 0
                 | JdfModel.Estimated -> 1)
             |> Seq.head))
        |> Map
    let retainedTripIds = feed.trips |> Seq.map (fun trip -> trip.id) |> Set
    let retainedRouteIds = feed.routes |> Seq.map (fun route -> route.id) |> Set
    let retainedStopIds = feed.stops |> Seq.map (fun stop -> stop.id) |> Set
    let stopLocationSources =
        batch.stopLocationSources |> Seq.map (fun value -> value.stopId, value.source) |> Map

    let routes () =
        batch.routes
        |> Array.sortBy (fun route -> route.id, route.idDistinction)
        |> Array.map (fun route -> row [
            "gtfs_route_id", box (JdfToGtfs.jdfRouteId route.id route.idDistinction)
            "source_route_id", box (JdfToGtfs.jdfSourceRouteId route.id route.idDistinction)
            "route_distinction", box route.idDistinction
            "source_agency_id", box route.agencyId
            "source_agency_distinction", box route.agencyDistinction
            "source_transport_mode", box (sourceTransportModes.[route.id, route.idDistinction] |> transportModeCode)
            "effective_transport_mode", box (transportModeCode route.transportMode)
            "valid_from", box (localDateString route.timetableValidFrom)
            "valid_to", box (localDateString route.timetableValidTo) ])

    let stopPlaces () =
        batch.stops
        |> Array.filter (fun stop ->
            retainedStopIds.Contains(JdfToGtfs.jdfStopId stopIdsCis stop.id))
        |> Array.sortBy (fun stop -> stop.id)
        |> Array.map (fun stop ->
            let location = stopLocations |> Map.tryFind stop.id
            let coordinatesMissing =
                location
                |> Option.map (fun value -> value.lat = 0m && value.lon = 0m)
                |> Option.defaultValue true
            let coordinatePrecision =
                if coordinatesMissing then "missing"
                else
                    match location.Value.precision with
                    | JdfModel.StopPrecise -> "stop"
                    | JdfModel.Estimated -> "estimated"
            row [
                "gtfs_stop_id", box (JdfToGtfs.jdfStopId stopIdsCis stop.id)
                "town", box stop.town
                "district", nullableObj stop.district
                "nearby_place", nullableObj stop.nearbyPlace
                "country", nullableObj stop.country
                "coordinates_missing", box coordinatesMissing
                "coordinate_precision", box coordinatePrecision
                "coordinate_source", nullableObj (stopLocationSources |> Map.tryFind stop.id) ])

    let routeStopZones () =
        batch.routeStops
        |> Seq.collect (fun routeStop ->
            Jdf.normalizeZoneTokens [routeStop.zone]
            |> Seq.mapi (fun index zoneCode ->
                row [
                    "gtfs_route_id", box (JdfToGtfs.jdfRouteId routeStop.routeId routeStop.routeDistinction)
                    "source_route_stop_id", box routeStop.routeStopId
                    "zone_id", box (JdfToGtfs.jdfSourceZoneId routeStop.routeId routeStop.routeDistinction zoneCode)
                    "zone_order", box index ]))
        |> Seq.distinctBy (fun value ->
            value.["gtfs_route_id"], value.["source_route_stop_id"], value.["zone_id"])
        |> Seq.sortBy (fun value ->
            string value.["gtfs_route_id"], unbox<int64> value.["source_route_stop_id"],
            unbox<int> value.["zone_order"], string value.["zone_id"])
        |> Seq.toArray

    let notices () =
        let routeNotices =
            batch.routeInfo
            |> Array.choose (fun notice ->
                if String.IsNullOrWhiteSpace(notice.text) then None else
                let gtfsRouteId = JdfToGtfs.jdfRouteId notice.routeId notice.routeDistinction
                if not (retainedRouteIds.Contains gtfsRouteId) then None else
                Some (row [
                    "source_notice_id", box (routeNoticeId notice.routeId notice.routeDistinction notice.id)
                    "notice_kind", box "route_information"
                    "gtfs_route_id", box gtfsRouteId
                    "gtfs_trip_id", null
                    "label", null
                    "text", box notice.text
                    "valid_from", null
                    "valid_to", null
                    "service_note_type", null ]))
        let serviceNotices =
            batch.serviceNotes
            |> Array.choose (fun notice ->
                let text = nonEmptyText notice.note
                let label = nonEmptyText (Some notice.designation)
                // A typed, text-free time code is already represented exactly
                // by calendar.txt/calendar_dates.txt.
                if notice.noteType.IsSome && text.IsNone then None
                elif text.IsNone && label.IsNone then None
                else
                    let gtfsTripId =
                        JdfToGtfs.jdfTripId notice.routeId notice.routeDistinction notice.tripId
                    if not (retainedTripIds.Contains gtfsTripId) then None else
                    Some (row [
                        "source_notice_id", box (tripNoticeId notice.routeId notice.routeDistinction notice.tripId notice.id)
                        "notice_kind", box "service_note"
                        "gtfs_route_id", null
                        "gtfs_trip_id", box gtfsTripId
                        "label", nullableObj label
                        "text", nullableObj text
                        "valid_from", notice.dateFrom |> Option.map localDateString |> nullableObj
                        "valid_to", notice.dateTo |> Option.map localDateString |> nullableObj
                        "service_note_type", notice.noteType |> Option.map serviceNoteTypeName |> nullableObj ]))
        let reservationNotices =
            batch.reservationOptions
            |> withOwnerOrdinals (fun notice -> notice.routeId, notice.routeDistinction, notice.tripId)
            |> Seq.choose (fun (ordinal, notice) ->
                if String.IsNullOrWhiteSpace(notice.note) then None else
                let gtfsTripId =
                    JdfToGtfs.jdfTripId notice.routeId notice.routeDistinction notice.tripId
                if not (retainedTripIds.Contains gtfsTripId) then None else
                Some (row [
                    "source_notice_id", box (reservationNoticeId notice.routeId notice.routeDistinction notice.tripId ordinal)
                    "notice_kind", box "reservation"
                    "gtfs_route_id", null
                    "gtfs_trip_id", box gtfsTripId
                    "label", null
                    "text", box notice.note
                    "valid_from", null
                    "valid_to", null
                    "service_note_type", null ]))
            |> Seq.toArray
        Array.concat [routeNotices; serviceNotices; reservationNotices]
        |> Array.sortBy (fun value -> string value.["source_notice_id"])

    let transfers () =
        batch.transfers
        |> withOwnerOrdinals (fun transfer -> transfer.routeId, transfer.routeDistinction, transfer.tripId)
        |> Seq.choose (fun (ordinal, transfer) ->
            let gtfsTripId =
                JdfToGtfs.jdfTripId transfer.routeId transfer.routeDistinction transfer.tripId
            if not (retainedTripIds.Contains gtfsTripId)
               || not (emittedTransferCalls.Contains(struct (gtfsTripId, transfer.routeStopId))) then None
            else Some (row [
                "source_transfer_id", box (transferId transfer.routeId transfer.routeDistinction transfer.tripId ordinal)
                "gtfs_trip_id", box gtfsTripId
                "source_route_stop_id", box transfer.routeStopId
                "transfer_type", box transfer.transferType
                "transfer_route_id", nullableObj transfer.transferRouteId
                "transfer_stop_id", nullableObj transfer.transferStopId
                "transfer_stop_post_id", nullableObj transfer.transferStopPostId
                "transfer_end_stop_id", nullableObj transfer.transferEndStopId
                "transfer_end_stop_post_id", nullableObj transfer.transferEndStopPostId
                "wait_minutes", nullableObj transfer.waitMinutes
                "note", nonEmptyText transfer.note |> nullableObj ]))
        |> Seq.sortBy (fun value ->
            string value.["gtfs_trip_id"], unbox<int64> value.["source_route_stop_id"],
            string value.["source_transfer_id"])
        |> Seq.toArray

    let restrictions () =
        let routeStopAssignments =
            batch.routeStops
            |> Seq.collect (fun routeStop ->
                let gtfsRouteId =
                    JdfToGtfs.jdfRouteId routeStop.routeId routeStop.routeDistinction
                if not (retainedRouteIds.Contains gtfsRouteId) then Seq.empty else
                restrictionGroupCodes (restrictionMask restrictionLookup routeStop.attributes)
                |> Seq.map (fun groupCode -> row [
                    "assignment_scope", box "route_stop"
                    "gtfs_route_id", box gtfsRouteId
                    "gtfs_trip_id", null
                    "source_route_stop_id", box routeStop.routeStopId
                    "group_code", box groupCode ]))
        let tripCallAssignments =
            callFacts.tripRestrictionAssignments
            |> Seq.map (fun struct(gtfsTripId,routeStopId,groupCode) -> row [
                "assignment_scope", box "trip_call"
                "gtfs_route_id", null
                "gtfs_trip_id", box gtfsTripId
                "source_route_stop_id", box routeStopId
                "group_code", box groupCode ])
        Seq.append routeStopAssignments tripCallAssignments
        |> Seq.distinctBy (fun value ->
            value.["assignment_scope"], value.["gtfs_route_id"], value.["gtfs_trip_id"],
            value.["source_route_stop_id"], value.["group_code"])
        |> Seq.sortBy (fun value ->
            string value.["assignment_scope"], string value.["gtfs_route_id"],
            string value.["gtfs_trip_id"], unbox<int64> value.["source_route_stop_id"],
            string value.["group_code"])
        |> Seq.toArray

    let candidateLookup =
        postPlan.physicalHypotheses
        |> Seq.groupBy (fun candidate -> candidate.stopId)
        |> Seq.map (fun (stopId, values) -> stopId,values |> Seq.toArray)
        |> Map
    let derivedPostLocations () =
        postPlan.locations
        |> Array.map (fun location ->
            let candidates = candidateLookup |> Map.tryFind location.stopId |> Option.defaultValue [||]
            let provenance =
                candidates
                |> Seq.filter (fun candidate -> location.candidateIds |> Array.contains candidate.hypothesisId)
                |> Seq.collect _.sources
                |> Seq.distinct
                |> Seq.sort
                |> String.concat ";"
            let representative = location.representativeCandidateId
            let diagnosticLabel =
                match location.sideGroupId with
                | Some groupId ->
                    postPlan.sideGroups |> Array.tryFind (fun value -> value.sideGroupId = groupId)
                    |> Option.map (fun value -> $"O-{value.sector}")
                | None ->
                    representative
                    |> Option.bind (fun candidateId ->
                        candidates |> Array.sortBy (fun value -> value.hypothesisId)
                        |> Array.tryFindIndex (fun value -> value.hypothesisId = candidateId))
                    |> Option.map (fun index -> $"O{index + 1}")
            row [
                "derived_location_id", box location.locationId
                "gtfs_stop_place_id", box (JdfToGtfs.jdfStopId stopIdsCis location.stopId)
                "selection_kind", box location.selectionKind
                "physical_candidate_id", if location.selectionKind = "physical" then representative |> nullableObj else null
                "side_group_id", location.sideGroupId |> nullableObj
                "representative_candidate_id", representative |> nullableObj
                "diagnostic_label", diagnosticLabel |> nullableObj
                "latitude", box (float location.lat)
                "longitude", box (float location.lon)
                "candidate_ids", box (String.Join(";", location.candidateIds))
                "provenance", box provenance ])

    let authoredTargetId stopId (key: string) =
        if key.StartsWith("id:", StringComparison.Ordinal) then
            JdfToGtfs.jdfStopPostId stopIdsCis stopId (Int64.Parse(key.Substring(3), CultureInfo.InvariantCulture))
        else JdfToGtfs.jdfStopPostNumId stopIdsCis stopId (key.Substring(4))
    let rejectedCandidateIds stopId (selection: JdfToGtfs.DerivedPostSelection) =
        candidateLookup
        |> Map.tryFind stopId
        |> Option.defaultValue [||]
        |> Array.map (fun candidate -> candidate.hypothesisId)
        |> Array.filter (fun candidateId -> not (selection.candidateIds |> Array.contains candidateId))
        |> Array.sort
        |> fun ids -> String.Join(";", ids)
    let contextStopIds selector (contexts: JdfToGtfs.DerivedPostContext array) =
        contexts
        |> Seq.choose selector
        |> Seq.distinct
        |> Seq.sort
        |> Seq.map (JdfToGtfs.jdfStopId stopIdsCis)
        |> String.concat ";"
        |> function value when String.IsNullOrEmpty(value) -> None | value -> Some value
    let contextRoles (contexts: JdfToGtfs.DerivedPostContext array) =
        contexts
        |> Seq.map (fun context -> context.sameStopBlockRole)
        |> Seq.distinct |> Seq.sort |> String.concat ";"
    let movementFamilyId key =
        match postPlan.movementFamilyIds.TryGetValue(key) with
        | true,value -> Some value
        | _ -> None
    let modeByRoute = Dictionary<struct (string * int), string>()
    for route in batch.routes do
        modeByRoute.[struct (route.id, route.idDistinction)] <-
            transportModeCode route.transportMode
    let candidateStopsWithMultiple =
        candidateLookup
        |> Seq.choose (fun pair ->
            if pair.Value.Length >= 2 then Some pair.Key else None)
        |> HashSet
    let authoredKeys = HashSet<struct (int64 * string)>()
    for post in batch.stopPosts do
        if candidateStopsWithMultiple.Contains(post.stopId) then
            authoredKeys.Add(struct (post.stopId, $"id:{post.stopPostId}")) |> ignore
    let authoredModes = callFacts.authoredModes
    for compoundKey in authoredModes.Keys do authoredKeys.Add(compoundKey) |> ignore
    let modesFor stopId key =
        match authoredModes.TryGetValue(struct (stopId, key)) with
        | true, modes -> modes |> Seq.map transportModeCode |> Seq.sort |> String.concat ";"
        | _ -> ""
    let authoredAssignments () =
        postPlan.authored
        |> Seq.sortBy (fun pair -> pair.Key)
        |> Seq.map (fun pair ->
            let stopId, key = pair.Key
            let selection = pair.Value
            let contexts = postPlan.authoredContexts |> Map.tryFind (stopId, key) |> Option.defaultValue [||]
            let modes = modesFor stopId key
            { targetGtfsStopId=authoredTargetId stopId key; assignmentKind="authored"
              derivedLocationId=Some selection.locationId; mode=modes
              lineId=None; direction=None; patternHash=None; patternPosition=None
              movementFamilyId=None
              contextPreviousStopId=contextStopIds (fun context -> context.previousStopId) contexts
              contextNextStopId=contextStopIds (fun context -> context.nextStopId) contexts
              sameStopBlockRole=contextRoles contexts; score=Some selection.score; margin=selection.margin
              selectedCandidates=String.Join(";", selection.candidateIds)
              rejectedCandidates=rejectedCandidateIds stopId selection; status="positioned" })
    let authoredInsufficientAssignments () =
        authoredKeys
        |> Seq.sort
        |> Seq.choose (fun struct (stopId, key) ->
            if postPlan.authored.ContainsKey((stopId, key)) then None else
            let modes = modesFor stopId key
            let rejected =
                candidateLookup.[stopId]
                |> Array.map (fun candidate -> candidate.hypothesisId)
                |> Array.sort |> fun ids -> String.Join(";", ids)
            Some { targetGtfsStopId=authoredTargetId stopId key; assignmentKind="authored"
                   derivedLocationId=None; mode=modes; lineId=None; direction=None
                   patternHash=None; patternPosition=None; contextPreviousStopId=None
                   movementFamilyId=None
                   contextNextStopId=None; sameStopBlockRole="unresolved"; score=None; margin=None
                   selectedCandidates=""; rejectedCandidates=rejected
                   status="insufficient_parent_centroid" })
    let assignmentTotal =
        int64 (postPlan.authored.Count + authoredKeys.Count - postPlan.authored.Count
               + postPlan.calls.Count + postPlan.unresolvedPatternContexts.Length)
    let mutable assignmentProgress = 0L
    let reportAssignmentProgress () =
        assignmentProgress <- assignmentProgress + 1L
        if assignmentProgress % 100_000L = 0L then
            progress "prepare-derived-post-assignments" assignmentProgress (Some assignmentTotal)
    let internalAssignments () =
        postPlan.calls
        |> Seq.sortBy (fun pair ->
            let key=pair.Key
            key.stopId,key.mode,key.lineId,key.routeDistinction,key.direction,
            key.patternHash,key.position,key.sameStopBlockRole)
        |> Seq.map (fun pair ->
            reportAssignmentProgress ()
            let stopId, mode = pair.Key.stopId, pair.Key.mode
            let selection = pair.Value
            let context = postPlan.callContexts.[pair.Key]
            { targetGtfsStopId = JdfToGtfs.inferredPostId stopIdsCis postPlan selection
              assignmentKind="internal"; derivedLocationId=Some selection.locationId
              mode=transportModeCode mode; lineId=Some pair.Key.lineId
              direction=Some pair.Key.direction; patternHash=Some pair.Key.patternHash
              patternPosition=Some pair.Key.position
              movementFamilyId=movementFamilyId pair.Key
              contextPreviousStopId=context.previousStopId |> Option.map (JdfToGtfs.jdfStopId stopIdsCis)
              contextNextStopId=context.nextStopId |> Option.map (JdfToGtfs.jdfStopId stopIdsCis)
              sameStopBlockRole=context.sameStopBlockRole; score=Some selection.score; margin=selection.margin
              selectedCandidates=String.Join(";", selection.candidateIds)
              rejectedCandidates=rejectedCandidateIds stopId selection; status="positioned" })
    let internalCentroidAssignments () =
        postPlan.unresolvedPatternContexts
        |> Seq.map (fun contextKey ->
            reportAssignmentProgress ()
            let context = postPlan.callContexts.[contextKey]
            let rejected =
                candidateLookup |> Map.tryFind contextKey.stopId |> Option.defaultValue [||]
                |> Array.map (fun value -> value.hypothesisId) |> Array.sort
                |> fun values -> String.Join(";",values)
            { targetGtfsStopId = $"{JdfToGtfs.jdfStopId stopIdsCis contextKey.stopId}:unspecified"
              assignmentKind="internal"; derivedLocationId=None; mode=transportModeCode contextKey.mode
              lineId=Some contextKey.lineId; direction=Some contextKey.direction
              patternHash=Some contextKey.patternHash; patternPosition=Some contextKey.position
              movementFamilyId=movementFamilyId contextKey
              contextPreviousStopId=context.previousStopId |> Option.map (JdfToGtfs.jdfStopId stopIdsCis)
              contextNextStopId=context.nextStopId |> Option.map (JdfToGtfs.jdfStopId stopIdsCis)
              sameStopBlockRole=contextKey.sameStopBlockRole; score=None; margin=None
              selectedCandidates=""; rejectedCandidates=rejected; status="centroid_fallback" })
    let derivedPostAssignments () : seq<DerivedPostAssignmentRow> =
        assignmentProgress <- 0L
        let authored =
            Seq.append (authoredAssignments ()) (authoredInsufficientAssignments ())
            |> Seq.map (fun value ->
                reportAssignmentProgress ()
                value)
        Seq.concat [ authored; internalAssignments (); internalCentroidAssignments () ]
    let modalityByCandidate = postPlan.modalityEstimates |> Array.map (fun value -> value.candidateId, value) |> Map
    let hypothesisByCandidate =
        postPlan.physicalHypotheses
        |> Seq.collect (fun hypothesis ->
            hypothesis.memberCandidateIds |> Seq.map (fun candidateId -> candidateId,hypothesis.hypothesisId))
        |> Map
    let candidateEvidenceRows () =
        batch.postCandidateEvidence
        |> Array.sortBy (fun value -> value.stopId, value.candidateId, value.observationId)
        |> Array.map (fun evidence ->
            let hypothesisId=hypothesisByCandidate |> Map.tryFind evidence.candidateId
            let estimate=hypothesisId |> Option.bind (fun value -> modalityByCandidate |> Map.tryFind value)
            row [
                "gtfs_stop_place_id", box (JdfToGtfs.jdfStopId stopIdsCis evidence.stopId)
                "candidate_id", box evidence.candidateId; "observation_id", box evidence.observationId
                "hypothesis_id", hypothesisId |> nullableObj
                "source_kind", box evidence.sourceKind; "source_object_id", evidence.sourceObjectId |> nullableObj
                "observed_at", evidence.observedAt |> nullableObj
                "latitude", box (float evidence.lat); "longitude", box (float evidence.lon)
                "support_weight", box (float evidence.supportWeight); "raw_tags", box evidence.rawTags
                "explicit_modes", box evidence.explicitModes; "denied_modes", box evidence.deniedModes
                "lifecycle", box evidence.lifecycle
                "modality_status", estimate |> Option.map (fun value -> value.status) |> nullableObj
                "supported_modes", estimate |> Option.map (fun value -> String.Join(";", value.supportedModes)) |> nullableObj
                "distinct_supporting_patterns", estimate |> Option.map (fun value -> value.distinctSupportingPatterns) |> nullableObj
                "modality_confidence", estimate |> Option.map (fun value -> value.confidence) |> nullableObj ])
    let physicalHypothesisRows () =
        postPlan.physicalHypotheses
        |> Array.map (fun hypothesis -> row [
            "gtfs_stop_place_id",box(JdfToGtfs.jdfStopId stopIdsCis hypothesis.stopId)
            "hypothesis_id",box hypothesis.hypothesisId
            "member_observation_ids",box(String.Join(";",hypothesis.memberObservationIds))
            "member_legacy_candidate_ids",box(String.Join(";",hypothesis.memberCandidateIds))
            "representative_candidate_id",box hypothesis.representativeCandidateId
            "latitude",box(float hypothesis.lat); "longitude",box(float hypothesis.lon)
            "sources",box(String.Join(";",hypothesis.sources)) ])
    let sideGroupRows () =
        postPlan.sideGroups
        |> Array.map (fun group -> row [
            "gtfs_stop_place_id", box (JdfToGtfs.jdfStopId stopIdsCis group.stopId)
            "side_group_id", box group.sideGroupId; "mode", box(string group.mode)
            "corridor_face_id", box group.corridorFaceId
            "sector", box group.sector
            "representative_candidate_id", box group.representativeCandidateId
            "member_candidate_ids", box (String.Join(";", group.memberCandidateIds))
            "compactness_metres", box group.compactnessMetres
            "repeated_pattern_support", box group.repeatedPatternSupport ])
    let stringField name nullable = field<string> name nullable
    let intField name nullable = field<int> name nullable
    let int64Field name nullable = field<int64> name nullable
    let boolField name nullable = field<bool> name nullable
    let doubleField name nullable = field<double> name nullable
    let producer fields rows = fun () -> table fields (rows ())
    [|
        "source_route_metadata.parquet", producer [|
            stringField "gtfs_route_id" false; stringField "source_route_id" false
            intField "route_distinction" false; stringField "source_agency_id" false
            intField "source_agency_distinction" false; stringField "source_transport_mode" false
            stringField "effective_transport_mode" false; stringField "valid_from" false
            stringField "valid_to" false |] routes
        "source_stop_metadata.parquet", producer [|
            stringField "gtfs_stop_id" false; stringField "town" false
            stringField "district" true; stringField "nearby_place" true
            stringField "country" true; boolField "coordinates_missing" false
            stringField "coordinate_precision" false; stringField "coordinate_source" true |] stopPlaces
        "source_route_stop_zone_metadata.parquet", producer [|
            stringField "gtfs_route_id" false; int64Field "source_route_stop_id" false
            stringField "zone_id" false; intField "zone_order" false |] routeStopZones
        "source_notice_metadata.parquet", producer [|
            stringField "source_notice_id" false; stringField "notice_kind" false
            stringField "gtfs_route_id" true; stringField "gtfs_trip_id" true
            stringField "label" true; stringField "text" true
            stringField "valid_from" true; stringField "valid_to" true
            stringField "service_note_type" true |] notices
        "source_transfer_metadata.parquet", producer [|
            stringField "source_transfer_id" false; stringField "gtfs_trip_id" false
            int64Field "source_route_stop_id" false; stringField "transfer_type" false
            int64Field "transfer_route_id" true; int64Field "transfer_stop_id" true
            int64Field "transfer_stop_post_id" true; int64Field "transfer_end_stop_id" true
            int64Field "transfer_end_stop_post_id" true; intField "wait_minutes" true
            stringField "note" true |] transfers
        "source_travel_restriction_metadata.parquet", producer [|
            stringField "assignment_scope" false; stringField "gtfs_route_id" true
            stringField "gtfs_trip_id" true; int64Field "source_route_stop_id" false
            stringField "group_code" false |] restrictions
        "derived_post_locations.parquet", producer [|
            stringField "derived_location_id" false; stringField "gtfs_stop_place_id" false
            stringField "selection_kind" false; stringField "physical_candidate_id" true
            stringField "side_group_id" true; stringField "representative_candidate_id" true
            stringField "diagnostic_label" true; doubleField "latitude" false
            doubleField "longitude" false; stringField "candidate_ids" false
            stringField "provenance" false |] derivedPostLocations
        "post_candidate_evidence.parquet", producer [|
            stringField "gtfs_stop_place_id" false; stringField "candidate_id" false
            stringField "observation_id" false; stringField "source_kind" false
            stringField "hypothesis_id" true
            stringField "source_object_id" true; stringField "observed_at" true
            doubleField "latitude" false; doubleField "longitude" false
            doubleField "support_weight" false; stringField "raw_tags" false
            stringField "explicit_modes" false; stringField "denied_modes" false
            stringField "lifecycle" false; stringField "modality_status" true
            stringField "supported_modes" true; intField "distinct_supporting_patterns" true
            doubleField "modality_confidence" true |] candidateEvidenceRows
        "post_physical_hypotheses.parquet", producer [|
            stringField "gtfs_stop_place_id" false; stringField "hypothesis_id" false
            stringField "member_observation_ids" false; stringField "member_legacy_candidate_ids" false
            stringField "representative_candidate_id" false; doubleField "latitude" false
            doubleField "longitude" false; stringField "sources" false |] physicalHypothesisRows
        "post_side_groups.parquet", producer [|
            stringField "gtfs_stop_place_id" false; stringField "side_group_id" false
            stringField "mode" false; stringField "corridor_face_id" false; stringField "sector" false
            stringField "representative_candidate_id" false
            stringField "member_candidate_ids" false; doubleField "compactness_metres" false
            intField "repeated_pattern_support" false |] sideGroupRows
    |], assignmentTotal, derivedPostAssignments

let private writeParquet descriptor path table =
    task {
        let schema = ParquetSchema(table.fields |> Array.map (fun field -> field :> Field))
        let options = ParquetOptions(CompressionMethod = CompressionMethod.Snappy,
                                     RowGroupSize = Nullable 65536)
        let metadata = Dictionary<string, string>()
        metadata.Add("obehy.bundle_version", string BundleVersion)
        metadata.Add("obehy.schema_version", string ParquetSchemaVersion)
        metadata.Add("obehy.source_id", descriptor.sourceId)
        metadata.Add("obehy.snapshot_id", $"sha256:{descriptor.payloadSha256}")
        use stream = File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
        do! ParquetSerializer.SerializeUntypedAsync(table.rows, schema, stream, options,
                                                    metadata, CancellationToken.None)
    } |> fun operation -> operation.GetAwaiter().GetResult()

let private writeTypedEvidenceParquet descriptor captureToolVersion routingPbfSha256 packId relationName
                                      path (relationFields:DataField array) (rows:seq<'T>)
                                      (writeGroup:ParquetRowGroupWriter -> DataField array -> 'T array -> unit) =
    let schema=ParquetSchema(relationFields |> Array.map(fun value -> value :> Field))
    let options=ParquetOptions(CompressionMethod=CompressionMethod.Snappy)
    let metadata=Dictionary<string,string>()
    metadata.Add("obehy.evidence_format",JdfPostInference.EvidenceFormat)
    metadata.Add("obehy.evidence_schema_version",string JdfPostInference.EvidenceSchemaVersion)
    metadata.Add("obehy.router_evidence_version",JdfPostInference.RouterEvidenceVersion)
    metadata.Add("obehy.variant_enumeration_version",JdfPostInference.VariantEnumerationVersion)
    metadata.Add("obehy.capture_tool_version",captureToolVersion)
    metadata.Add("obehy.pack_id",packId)
    metadata.Add("obehy.relation",relationName)
    metadata.Add("obehy.source_id",descriptor.sourceId)
    metadata.Add("obehy.snapshot_id",$"sha256:{descriptor.payloadSha256}")
    metadata.Add("obehy.routing_pbf_sha256",routingPbfSha256)
    metadata.Add("obehy.capture_routed_excess_metres",
                 JdfPostInference.CaptureRoutedExcessHorizonMetres.ToString(CultureInfo.InvariantCulture))
    metadata.Add("obehy.capture_maximum_corridor_variants",string JdfPostInference.CaptureMaximumCorridorVariants)
    metadata.Add("obehy.maximum_search_states",string JdfPostInference.CaptureMaximumSearchStates)
    metadata.Add("obehy.maximum_search_distance_metres",
                 JdfPostInference.CaptureMaximumSearchDistanceMetres.ToString(CultureInfo.InvariantCulture))
    use stream=File.Open(path,FileMode.CreateNew,FileAccess.Write,FileShare.None)
    let writer=ParquetWriter.CreateAsync(schema,stream,options,false,CancellationToken.None)
               |> fun operation -> operation.GetAwaiter().GetResult()
    writer.CustomMetadata<-metadata
    try
        for chunk in rows |> Seq.chunkBySize 65536 do
            use rowGroup=writer.CreateRowGroup()
            writeGroup rowGroup relationFields chunk
    finally writer.DisposeAsync().AsTask().GetAwaiter().GetResult()

let private writeEvidenceStrings (rowGroup:ParquetRowGroupWriter) (field:DataField)
                                 (values:string array) =
    rowGroup.WriteAsync(field,values :> IReadOnlyCollection<string>,Nullable<ReadOnlyMemory<int>>())
    |> fun operation -> operation.GetAwaiter().GetResult()

let private writeEvidenceMappedStrings (group:ParquetRowGroupWriter) (fields:DataField array)
                                       index (rows:'Row array) (mapping:'Row->string) =
    rows |> Array.map mapping |> writeEvidenceStrings group fields.[index]

let private writeEvidenceMappedValues<'T,'Row when 'T:(new:unit->'T)
                                                   and 'T:struct and 'T :> ValueType>
                                      (group:ParquetRowGroupWriter) (fields:DataField array)
                                      index (rows:'Row array) (mapping:'Row->'T) =
    let values=rows |> Array.map mapping
    group.WriteAsync<'T>(fields.[index],ReadOnlyMemory<'T>(values),
                         Nullable<ReadOnlyMemory<int>>(),null,CancellationToken.None)
    |> fun operation -> operation.GetAwaiter().GetResult()

let private writeEvidenceMappedNullableValues<'T,'Row when 'T:(new:unit->'T)
                                                           and 'T:struct and 'T :> ValueType>
                                              (group:ParquetRowGroupWriter) (fields:DataField array)
                                              index (rows:'Row array) (mapping:'Row->'T option) =
    let values=rows |> Array.map(mapping >> Option.toNullable)
    group.WriteAsync<'T>(fields.[index],ReadOnlyMemory<Nullable<'T>>(values),
                         Nullable<ReadOnlyMemory<int>>(),null,CancellationToken.None)
    |> fun operation -> operation.GetAwaiter().GetResult()

let private parquetSchemaFingerprint path =
    use stream=File.OpenRead(path)
    let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
    try
        reader.Schema.DataFields
        |> Array.map(fun field -> $"{field.Name}:{field.ClrType.FullName}:{field.IsNullable}")
        |> String.concat "|"
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> fun value -> value.ToLowerInvariant()
    finally
        (reader :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()

let private evidenceFileEntry directory fileName rows : JdfPostInference.EvidenceFileManifest =
    let path=Path.Combine(directory,fileName)
    let info=FileInfo(path)
    { path=fileName
      sha256=fileSha256 path
      bytes=info.Length
      rows=rows
      schemaFingerprint=parquetSchemaFingerprint path }

let private writeParquetValues<'T when 'T : (new : unit -> 'T)
                                  and 'T : struct and 'T :> ValueType>
                               (rowGroup: ParquetRowGroupWriter) field (values: 'T array) =
    rowGroup.WriteAsync<'T>(field,ReadOnlyMemory<'T>(values),
                            Nullable<ReadOnlyMemory<int>>(),null,CancellationToken.None)
    |> fun operation -> operation.GetAwaiter().GetResult()

let private writeParquetNullableValues<'T when 'T : (new : unit -> 'T)
                                          and 'T : struct and 'T :> ValueType>
                                       (rowGroup: ParquetRowGroupWriter) field
                                       (values: Nullable<'T> array) =
    rowGroup.WriteAsync<'T>(field,ReadOnlyMemory<Nullable<'T>>(values),
                            Nullable<ReadOnlyMemory<int>>(),null,CancellationToken.None)
    |> fun operation -> operation.GetAwaiter().GetResult()

let private writeDerivedPostAssignmentsParquet descriptor path expectedCount
                                                  (rows: unit -> seq<DerivedPostAssignmentRow>)
                                                  (progress: int64 -> int64 option -> unit) =
    let fields: DataField array = [|
        field<string> "target_gtfs_stop_id" false; field<string> "assignment_kind" false
        field<string> "derived_location_id" true; field<string> "mode" false
        field<string> "line_id" true; field<int> "direction" true
        field<string> "pattern_hash" true; field<int> "pattern_position" true
        field<string> "movement_family_id" true
        field<string> "context_previous_stop_id" true; field<string> "context_next_stop_id" true
        field<string> "same_stop_block_role" false; field<double> "score" true
        field<double> "margin" true; field<string> "selected_candidates" false
        field<string> "rejected_candidates" false; field<string> "status" false
    |]
    let schema=ParquetSchema(fields |> Array.map (fun value -> value :> Field))
    let options=ParquetOptions(CompressionMethod=CompressionMethod.Snappy)
    let metadata=Dictionary<string,string>()
    metadata.Add("obehy.bundle_version",string BundleVersion)
    metadata.Add("obehy.schema_version",string ParquetSchemaVersion)
    metadata.Add("obehy.source_id",descriptor.sourceId)
    metadata.Add("obehy.snapshot_id",$"sha256:{descriptor.payloadSha256}")
    use stream=File.Open(path,FileMode.CreateNew,FileAccess.Write,FileShare.None)
    let writer=
        ParquetWriter.CreateAsync(schema,stream,options,false,CancellationToken.None)
        |> fun operation -> operation.GetAwaiter().GetResult()
    writer.CustomMetadata <- metadata
    let writeStrings (rowGroup: ParquetRowGroupWriter) fieldIndex
                     (mapping: DerivedPostAssignmentRow -> string)
                     (values: DerivedPostAssignmentRow array) =
        let column=values |> Array.map mapping
        rowGroup.WriteAsync(fields.[fieldIndex],column :> IReadOnlyCollection<string>,
                            Nullable<ReadOnlyMemory<int>>())
        |> fun operation -> operation.GetAwaiter().GetResult()
    let writeNullableInts (rowGroup: ParquetRowGroupWriter) fieldIndex
                          (mapping: DerivedPostAssignmentRow -> int option)
                          (values: DerivedPostAssignmentRow array) =
        values |> Array.map (mapping >> Option.toNullable)
        |> writeParquetNullableValues<int> rowGroup fields.[fieldIndex]
    let writeNullableDoubles (rowGroup: ParquetRowGroupWriter) fieldIndex
                             (mapping: DerivedPostAssignmentRow -> double option)
                             (values: DerivedPostAssignmentRow array) =
        values |> Array.map (mapping >> Option.toNullable)
        |> writeParquetNullableValues<double> rowGroup fields.[fieldIndex]
    try
        let mutable written=0L
        for chunk in rows() |> Seq.chunkBySize 65536 do
            use rowGroup=writer.CreateRowGroup()
            writeStrings rowGroup 0 (fun value -> value.targetGtfsStopId) chunk
            writeStrings rowGroup 1 (fun value -> value.assignmentKind) chunk
            writeStrings rowGroup 2 (fun value -> value.derivedLocationId |> Option.defaultValue null) chunk
            writeStrings rowGroup 3 (fun value -> value.mode) chunk
            writeStrings rowGroup 4 (fun value -> value.lineId |> Option.defaultValue null) chunk
            writeNullableInts rowGroup 5 (fun value -> value.direction) chunk
            writeStrings rowGroup 6 (fun value -> value.patternHash |> Option.defaultValue null) chunk
            writeNullableInts rowGroup 7 (fun value -> value.patternPosition) chunk
            writeStrings rowGroup 8 (fun value -> value.movementFamilyId |> Option.defaultValue null) chunk
            writeStrings rowGroup 9 (fun value -> value.contextPreviousStopId |> Option.defaultValue null) chunk
            writeStrings rowGroup 10 (fun value -> value.contextNextStopId |> Option.defaultValue null) chunk
            writeStrings rowGroup 11 (fun value -> value.sameStopBlockRole) chunk
            writeNullableDoubles rowGroup 12 (fun value -> value.score) chunk
            writeNullableDoubles rowGroup 13 (fun value -> value.margin) chunk
            writeStrings rowGroup 14 (fun value -> value.selectedCandidates) chunk
            writeStrings rowGroup 15 (fun value -> value.rejectedCandidates) chunk
            writeStrings rowGroup 16 (fun value -> value.status) chunk
            written <- written+int64 chunk.Length
            progress written (Some expectedCount)
        if written<>expectedCount then
            failwith $"Derived post assignment count mismatch: expected {expectedCount}, wrote {written}"
        int written
    finally
        (writer :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()

let private writeDerivedPostScoresParquet descriptor stopIdsCis path
                                         (plan: JdfToGtfs.PostEstimationPlan)
                                         (progress: int64 -> int64 option -> unit) =
    let fields: DataField array = [|
        field<string> "gtfs_stop_place_id" false; field<string> "candidate_id" false
        field<string> "mode" false; field<string> "line_id" false; field<int> "direction" false
        field<string> "pattern_hash" false; field<int> "pattern_position" false
        field<string> "same_stop_block_role" false; field<bool> "eligible" false
        field<double> "alignment" false; field<double> "side" false
        field<double> "proximity" false; field<double> "routed_fit" false
        field<string> "corridor_id" true; field<string> "ingress_thread_id" true
        field<string> "egress_thread_id" true; field<string> "corridor_face_id" true
        field<string> "routing_availability" false; field<int> "alternative_corridor_count" false
        field<double> "alternative_cost_gap" true; field<int> "snap_edge_id" true
        field<double> "snap_fraction" true; field<double> "corridor_distance" true
        field<double> "signed_lateral_offset" true; field<double> "corridor_heading" true
        field<double> "attachment_heading" true; field<double> "source_adjustment" false
        field<double> "modality_adjustment" false; field<double> "popularity_prior" false
        field<double> "total" false; field<string> "rejection_reason" true
        field<string> "movement_family_id" false
        field<double> "routed_excess_metres" true
        field<bool> "tied_corridors_agree" false
        field<string> "topology_failure_reason" true
        field<int> "route_distinction" false
    |]
    let schema = ParquetSchema(fields |> Array.map (fun value -> value :> Field))
    let options = ParquetOptions(CompressionMethod = CompressionMethod.Snappy)
    let metadata = Dictionary<string,string>()
    metadata.Add("obehy.bundle_version",string BundleVersion)
    metadata.Add("obehy.schema_version",string ParquetSchemaVersion)
    metadata.Add("obehy.source_id",descriptor.sourceId)
    metadata.Add("obehy.snapshot_id",$"sha256:{descriptor.payloadSha256}")
    use stream=File.Open(path,FileMode.CreateNew,FileAccess.Write,FileShare.None)
    let writer =
        ParquetWriter.CreateAsync(schema,stream,options,false,CancellationToken.None)
        |> fun operation -> operation.GetAwaiter().GetResult()
    writer.CustomMetadata <- metadata
    let writeStrings (rowGroup: ParquetRowGroupWriter) fieldIndex
                     (mapping: JdfToGtfs.DerivedPostScore -> string)
                     (rows: JdfToGtfs.DerivedPostScore array) =
        let values=rows |> Array.map mapping
        rowGroup.WriteAsync(fields.[fieldIndex],values :> IReadOnlyCollection<string>,
                            Nullable<ReadOnlyMemory<int>>())
        |> fun operation -> operation.GetAwaiter().GetResult()
    let writeInts (rowGroup: ParquetRowGroupWriter) fieldIndex
                  (mapping: JdfToGtfs.DerivedPostScore -> int)
                  (rows: JdfToGtfs.DerivedPostScore array) =
        rows |> Array.map mapping |> writeParquetValues<int> rowGroup fields.[fieldIndex]
    let writeBools (rowGroup: ParquetRowGroupWriter) fieldIndex
                   (mapping: JdfToGtfs.DerivedPostScore -> bool)
                   (rows: JdfToGtfs.DerivedPostScore array) =
        rows |> Array.map mapping |> writeParquetValues<bool> rowGroup fields.[fieldIndex]
    let writeDoubles (rowGroup: ParquetRowGroupWriter) fieldIndex
                     (mapping: JdfToGtfs.DerivedPostScore -> double)
                     (rows: JdfToGtfs.DerivedPostScore array) =
        rows |> Array.map mapping |> writeParquetValues<double> rowGroup fields.[fieldIndex]
    let writeNullableInts (rowGroup: ParquetRowGroupWriter) fieldIndex
                          (mapping: JdfToGtfs.DerivedPostScore -> int option)
                          (rows: JdfToGtfs.DerivedPostScore array) =
        rows |> Array.map (mapping >> Option.toNullable)
        |> writeParquetNullableValues<int> rowGroup fields.[fieldIndex]
    let writeNullableDoubles (rowGroup: ParquetRowGroupWriter) fieldIndex
                             (mapping: JdfToGtfs.DerivedPostScore -> double option)
                             (rows: JdfToGtfs.DerivedPostScore array) =
        rows |> Array.map (mapping >> Option.toNullable)
        |> writeParquetNullableValues<double> rowGroup fields.[fieldIndex]
    try
        let mutable written=0L
        for rows in plan.scoreRows() |> Seq.chunkBySize 65536 do
            use rowGroup=writer.CreateRowGroup()
            writeStrings rowGroup 0 (fun value -> JdfToGtfs.jdfStopId stopIdsCis value.context.stopId) rows
            writeStrings rowGroup 1 (fun value -> value.candidateId) rows
            writeStrings rowGroup 2 (fun value -> transportModeCode value.context.mode) rows
            writeStrings rowGroup 3 (fun value -> value.context.lineId) rows
            writeInts rowGroup 4 (fun value -> value.context.direction) rows
            writeStrings rowGroup 5 (fun value -> value.context.patternHash) rows
            writeInts rowGroup 6 (fun value -> value.context.position) rows
            writeStrings rowGroup 7 (fun value -> value.context.sameStopBlockRole) rows
            writeBools rowGroup 8 (fun value -> value.eligible) rows
            writeDoubles rowGroup 9 (fun value -> value.alignment) rows
            writeDoubles rowGroup 10 (fun value -> value.side) rows
            writeDoubles rowGroup 11 (fun value -> value.proximity) rows
            writeDoubles rowGroup 12 (fun value -> value.routedFit) rows
            writeStrings rowGroup 13 (fun value -> value.corridorId |> Option.defaultValue null) rows
            writeStrings rowGroup 14 (fun value -> value.ingressThreadId |> Option.defaultValue null) rows
            writeStrings rowGroup 15 (fun value -> value.egressThreadId |> Option.defaultValue null) rows
            writeStrings rowGroup 16 (fun value -> value.corridorFaceId |> Option.defaultValue null) rows
            writeStrings rowGroup 17 (fun value -> value.routingAvailability) rows
            writeInts rowGroup 18 (fun value -> value.alternativeCorridorCount) rows
            writeNullableDoubles rowGroup 19 (fun value -> value.alternativeCostGap) rows
            writeNullableInts rowGroup 20 (fun value -> value.snapEdgeId) rows
            writeNullableDoubles rowGroup 21 (fun value -> value.snapFraction) rows
            writeNullableDoubles rowGroup 22 (fun value -> value.corridorDistance) rows
            writeNullableDoubles rowGroup 23 (fun value -> value.signedLateralOffset) rows
            writeNullableDoubles rowGroup 24 (fun value -> value.corridorHeading) rows
            writeNullableDoubles rowGroup 25 (fun value -> value.attachmentHeading) rows
            writeDoubles rowGroup 26 (fun value -> value.sourceAdjustment) rows
            writeDoubles rowGroup 27 (fun value -> value.modalityAdjustment) rows
            writeDoubles rowGroup 28 (fun value -> value.popularityPrior) rows
            writeDoubles rowGroup 29 (fun value -> value.total) rows
            writeStrings rowGroup 30 (fun value -> value.rejectionReason |> Option.defaultValue null) rows
            writeStrings rowGroup 31 (fun value -> value.movementFamilyId) rows
            writeNullableDoubles rowGroup 32 (fun value -> value.routedExcessMetres) rows
            writeBools rowGroup 33 (fun value -> value.tiedCorridorsAgree) rows
            writeStrings rowGroup 34 (fun value -> value.topologyFailureReason |> Option.defaultValue null) rows
            writeInts rowGroup 35 (fun value -> value.routeDistinction) rows
            written <- written+int64 rows.Length
            progress written (Some plan.scoreCount)
        if written<>plan.scoreCount then
            failwith $"Derived post score count mismatch: expected {plan.scoreCount}, wrote {written}"
        int written
    finally
        (writer :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()

let writePostEvidenceStore descriptor captureToolVersion stopIdsCis evidencePath routingPbfPath
                           (store:JdfPostEvidence.CapturedPostEvidence) progress =
    let outputFull=Path.GetFullPath(evidencePath)
    if Directory.Exists(outputFull) || File.Exists(outputFull) then
        invalidArg "evidencePath" $"Post-inference evidence output already exists: {outputFull}"
    let parent=Path.GetDirectoryName(outputFull)
    if String.IsNullOrWhiteSpace(parent) then invalidArg "evidencePath" "Evidence output requires a parent directory"
    Directory.CreateDirectory(parent) |> ignore
    let temp=Path.Combine(parent,$".{Path.GetFileName(outputFull)}.tmp-{Guid.NewGuid():N}")
    Directory.CreateDirectory(temp) |> ignore
    let mutable activated=false
    try
        let routingHash =
            use stream=File.OpenRead(routingPbfPath)
            sha256Stream stream
        let packId=JdfPostInference.evidencePackId captureToolVersion descriptor.payloadSha256 routingHash
        let stopId stop = JdfToGtfs.jdfStopId stopIdsCis stop
        let write relationName fields rows writeGroup =
            writeTypedEvidenceParquet descriptor captureToolVersion routingHash packId relationName
                (Path.Combine(temp,relationName)) fields rows writeGroup

        let observationFields:DataField array=[|
            field<string> "gtfs_stop_place_id" false;field<string> "route_point_id" false
            field<string> "observation_id" false;field<string> "source_kind" false
            field<string> "source_object_id" true;field<string> "observed_at" true
            field<double> "latitude" false;field<double> "longitude" false
            field<double> "support_weight" false;field<string> "raw_tags" false
            field<string> "explicit_modes" false;field<string> "denied_modes" false
            field<string> "lifecycle" false |]
        write "observations.parquet" observationFields (store.observations.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows (fun value -> stopId value.stopId)
            writeEvidenceMappedStrings group fields 1 rows _.routePointId
            writeEvidenceMappedStrings group fields 2 rows _.observationId
            writeEvidenceMappedStrings group fields 3 rows _.sourceKind
            writeEvidenceMappedStrings group fields 4 rows (fun value -> value.sourceObjectId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 5 rows (fun value -> value.observedAt |> Option.defaultValue null)
            writeEvidenceMappedValues group fields 6 rows _.latitude
            writeEvidenceMappedValues group fields 7 rows _.longitude
            writeEvidenceMappedValues group fields 8 rows _.supportWeight
            writeEvidenceMappedStrings group fields 9 rows _.rawTags
            writeEvidenceMappedStrings group fields 10 rows _.explicitModes
            writeEvidenceMappedStrings group fields 11 rows _.deniedModes
            writeEvidenceMappedStrings group fields 12 rows _.lifecycle)
        progress "capture-evidence-observations" store.observations.Count (Some store.observations.Count)

        let routePointFields:DataField array=[|
            field<string> "gtfs_stop_place_id" false;field<string> "route_point_id" false
            field<string> "representative_observation_id" false;field<string> "observation_ids" false
            field<double> "latitude" false;field<double> "longitude" false
            field<bool> "has_current_lifecycle" false;field<bool> "has_obsolete_lifecycle" false
            field<double> "source_support_weight" false;field<string> "explicit_modes" false
            field<string> "denied_modes" false |]
        write "route_points.parquet" routePointFields (store.routePoints.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows (fun value -> stopId value.stopId)
            writeEvidenceMappedStrings group fields 1 rows _.routePointId
            writeEvidenceMappedStrings group fields 2 rows _.representativeObservationId
            writeEvidenceMappedStrings group fields 3 rows (fun value -> String.Join(";",value.observationIds))
            writeEvidenceMappedValues group fields 4 rows _.latitude
            writeEvidenceMappedValues group fields 5 rows _.longitude
            writeEvidenceMappedValues group fields 6 rows _.hasCurrentLifecycle
            writeEvidenceMappedValues group fields 7 rows _.hasObsoleteLifecycle
            writeEvidenceMappedValues group fields 8 rows _.sourceSupportWeight
            writeEvidenceMappedStrings group fields 9 rows (fun value -> String.Join(";",value.explicitModes))
            writeEvidenceMappedStrings group fields 10 rows (fun value -> String.Join(";",value.deniedModes)))
        progress "capture-evidence-route-points" store.routePoints.Count (Some store.routePoints.Count)

        let contextFields:DataField array=[|
            field<string> "context_id" false;field<string> "gtfs_stop_place_id" false
            field<string> "mode" false;field<string> "line_id" false
            field<int> "route_distinction" false;field<int> "direction" false
            field<string> "pattern_hash" false;field<int> "pattern_position" false
            field<string> "same_stop_block_role" false;field<string> "movement_family_id" false
            field<string> "context_previous_stop_id" true;field<string> "context_next_stop_id" true
            field<string> "assignment_kind" false;field<string> "authored_post_key" true
            field<string> "same_stop_block_id" true |]
        write "contexts.parquet" contextFields (store.contexts.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows _.contextId
            writeEvidenceMappedStrings group fields 1 rows (fun value -> stopId value.key.stopId)
            writeEvidenceMappedStrings group fields 2 rows (fun value -> value.key.mode)
            writeEvidenceMappedStrings group fields 3 rows (fun value -> value.key.lineId)
            writeEvidenceMappedValues group fields 4 rows (fun value -> value.key.routeDistinction)
            writeEvidenceMappedValues group fields 5 rows (fun value -> value.key.direction)
            writeEvidenceMappedStrings group fields 6 rows (fun value -> value.key.patternHash)
            writeEvidenceMappedValues group fields 7 rows (fun value -> value.key.patternPosition)
            writeEvidenceMappedStrings group fields 8 rows (fun value -> value.key.sameStopBlockRole)
            writeEvidenceMappedStrings group fields 9 rows _.movementFamilyId
            writeEvidenceMappedStrings group fields 10 rows (fun value -> value.previousStopId |> Option.map stopId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 11 rows (fun value -> value.nextStopId |> Option.map stopId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 12 rows _.assignmentKind
            writeEvidenceMappedStrings group fields 13 rows (fun value -> value.key.authoredPostKey |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 14 rows (fun value -> value.sameStopBlockId |> Option.defaultValue null))
        progress "capture-evidence-contexts" store.contexts.Count (Some store.contexts.Count)

        let corridorFields:DataField array=[|
            field<string> "context_id" false;field<string> "gtfs_stop_place_id" false
            field<int> "variant_rank" false;field<string> "corridor_id" true
            field<double> "absolute_cost_metres" true;field<double> "relative_cost_metres" true
            field<double> "relative_cost_fraction" true;field<double> "path_length_metres" true
            field<int> "directed_edge_count" false;field<int> "repeated_directed_edge_count" false
            field<int> "service_edge_count" false;field<int> "restricted_access_edge_count" false
            field<double> "access_penalty_metres" true;field<string> "ingress_thread_id" true
            field<string> "egress_thread_id" true;field<string> "routing_availability" false
            field<string> "invariant_failure_reason" true |]
        write "corridor_variants.parquet" corridorFields (store.corridorVariants.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows _.contextId
            writeEvidenceMappedStrings group fields 1 rows (fun value -> stopId value.stopId)
            writeEvidenceMappedValues group fields 2 rows (fun value -> value.variant.variantRank)
            writeEvidenceMappedStrings group fields 3 rows (fun value -> value.variant.corridorId |> Option.defaultValue null)
            writeEvidenceMappedNullableValues group fields 4 rows (fun value -> value.variant.absoluteCostMetres)
            writeEvidenceMappedNullableValues group fields 5 rows (fun value -> value.variant.relativeCostMetres)
            writeEvidenceMappedNullableValues group fields 6 rows (fun value -> value.variant.relativeCostFraction)
            writeEvidenceMappedNullableValues group fields 7 rows (fun value -> value.variant.pathLengthMetres)
            writeEvidenceMappedValues group fields 8 rows (fun value -> value.variant.directedEdgeCount)
            writeEvidenceMappedValues group fields 9 rows (fun value -> value.variant.repeatedDirectedEdgeCount)
            writeEvidenceMappedValues group fields 10 rows (fun value -> value.variant.serviceEdgeCount)
            writeEvidenceMappedValues group fields 11 rows (fun value -> value.variant.restrictedAccessEdgeCount)
            writeEvidenceMappedNullableValues group fields 12 rows (fun value -> value.variant.accessPenaltyMetres)
            writeEvidenceMappedStrings group fields 13 rows (fun value -> value.variant.ingressThreadId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 14 rows (fun value -> value.variant.egressThreadId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 15 rows (fun value -> value.variant.routingAvailability)
            writeEvidenceMappedStrings group fields 16 rows (fun value -> value.variant.invariantFailureReason |> Option.defaultValue null))
        progress "capture-evidence-corridors" store.corridorVariants.Count (Some store.corridorVariants.Count)

        let attachmentFields:DataField array=[|
            field<string> "context_id" false;field<string> "gtfs_stop_place_id" false
            field<int> "variant_rank" false;field<string> "corridor_id" true
            field<string> "route_point_id" false;field<double> "routed_excess_metres" true
            field<int> "snap_edge_id" true;field<int64> "snap_way_id" true
            field<double> "snap_fraction" true;field<double> "snap_projected_longitude" true
            field<double> "snap_projected_latitude" true;field<double> "snap_distance_metres" true
            field<string> "corridor_face_id" true;field<double> "corridor_distance_metres" true
            field<double> "signed_lateral_offset_metres" true;field<double> "corridor_heading_degrees" true
            field<double> "attachment_heading_degrees" true;field<double> "heading_difference_degrees" true
            field<double> "proximity_distance_metres" true;field<string> "routing_availability" false
            field<string> "invariant_failure_reason" true |]
        write "route_point_evidence.parquet" attachmentFields (store.routePointEvidence.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows _.contextId
            writeEvidenceMappedStrings group fields 1 rows (fun value -> stopId value.stopId)
            writeEvidenceMappedValues group fields 2 rows (fun value -> value.attachment.variantRank)
            writeEvidenceMappedStrings group fields 3 rows (fun value -> value.attachment.corridorId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 4 rows _.routePointId
            writeEvidenceMappedNullableValues group fields 5 rows (fun value -> value.attachment.routedExcessMetres)
            writeEvidenceMappedNullableValues group fields 6 rows (fun value -> value.attachment.snapEdgeId)
            writeEvidenceMappedNullableValues group fields 7 rows (fun value -> value.attachment.snapWayId)
            writeEvidenceMappedNullableValues group fields 8 rows (fun value -> value.attachment.snapFraction)
            writeEvidenceMappedNullableValues group fields 9 rows (fun value -> value.attachment.snapProjectedLongitude)
            writeEvidenceMappedNullableValues group fields 10 rows (fun value -> value.attachment.snapProjectedLatitude)
            writeEvidenceMappedNullableValues group fields 11 rows (fun value -> value.attachment.snapDistanceMetres)
            writeEvidenceMappedStrings group fields 12 rows (fun value -> value.attachment.corridorFaceId |> Option.defaultValue null)
            writeEvidenceMappedNullableValues group fields 13 rows (fun value -> value.attachment.corridorDistanceMetres)
            writeEvidenceMappedNullableValues group fields 14 rows (fun value -> value.attachment.signedLateralOffsetMetres)
            writeEvidenceMappedNullableValues group fields 15 rows (fun value -> value.attachment.corridorHeadingDegrees)
            writeEvidenceMappedNullableValues group fields 16 rows (fun value -> value.attachment.attachmentHeadingDegrees)
            writeEvidenceMappedNullableValues group fields 17 rows (fun value -> value.attachment.headingDifferenceDegrees)
            writeEvidenceMappedNullableValues group fields 18 rows (fun value -> value.attachment.proximityDistanceMetres)
            writeEvidenceMappedStrings group fields 19 rows (fun value -> value.attachment.routingAvailability)
            writeEvidenceMappedStrings group fields 20 rows (fun value -> value.attachment.invariantFailureReason |> Option.defaultValue null))
        progress "capture-evidence-routing" store.routePointEvidence.Count (Some store.routePointEvidence.Count)

        let manifest:JdfPostInference.PostInferenceEvidenceManifest = {
            evidenceFormat=JdfPostInference.EvidenceFormat;schemaVersion=JdfPostInference.EvidenceSchemaVersion
            packId=packId;captureToolVersion=captureToolVersion
            mergedJdfSha256=descriptor.payloadSha256;routingPbfSha256=routingHash
            osmSnapshot=None;routerEvidenceVersion=JdfPostInference.RouterEvidenceVersion
            variantEnumerationVersion=JdfPostInference.VariantEnumerationVersion
            captureCeilings={routedExcessMetres=JdfPostInference.CaptureRoutedExcessHorizonMetres
                             maximumCorridorVariants=JdfPostInference.CaptureMaximumCorridorVariants}
            maximumSearchStates=JdfPostInference.CaptureMaximumSearchStates
            maximumSearchDistanceMetres=JdfPostInference.CaptureMaximumSearchDistanceMetres
            contextCount=store.contexts.Count;routePointCount=store.routePoints.Count
            observationCount=store.observations.Count;corridorVariantCount=store.corridorVariants.Count
            routePointEvidenceCount=store.routePointEvidence.Count
            files=[|evidenceFileEntry temp "observations.parquet" store.observations.Count
                    evidenceFileEntry temp "route_points.parquet" store.routePoints.Count
                    evidenceFileEntry temp "contexts.parquet" store.contexts.Count
                    evidenceFileEntry temp "corridor_variants.parquet" store.corridorVariants.Count
                    evidenceFileEntry temp "route_point_evidence.parquet" store.routePointEvidence.Count|] }
        JdfPostInference.writeEvidenceManifest (Path.Combine(temp,"manifest.json")) manifest
        use validated=JdfPostEvidenceStore.openValidatedStore
                          { mergedJdfSha256=Some descriptor.payloadSha256
                            routingPbfSha256=Some routingHash
                            captureToolVersion=Some captureToolVersion }
                          temp
        Directory.Move(temp,outputFull)
        activated<-true
    finally
        if not activated && Directory.Exists(temp) then Directory.Delete(temp,true)

let replayPostInferenceEvidence evidencePath policyPath policyGridPath expectationsPath reviewStopsPath outputPath =
    let evidenceFull=Path.GetFullPath(evidencePath)
    if not(Directory.Exists evidenceFull) then
        invalidArg "evidencePath" $"Evidence directory does not exist: {evidenceFull}"
    use evidenceStore=JdfPostEvidenceStore.openValidatedStore
                          JdfPostEvidenceStore.noIdentityExpectation evidenceFull
    JdfPostInferenceEvaluator.writeReplayReport evidenceStore policyPath policyGridPath
        expectationsPath reviewStopsPath outputPath
type private CallMetadataParquetWriter(descriptor: SnapshotDescriptor, path: string) =
    let fields: DataField array = [|
            field<string> "gtfs_trip_id" false
            field<int> "stop_sequence" false
            field<int64> "source_route_stop_id" false
    |]
    let schema = ParquetSchema(fields |> Array.map (fun value -> value :> Field))
    let options = ParquetOptions(CompressionMethod = CompressionMethod.Snappy)
    let metadata = Dictionary<string, string>()
    let stream = File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
    let writer =
        ParquetWriter.CreateAsync(schema, stream, options, false, CancellationToken.None)
        |> fun operation -> operation.GetAwaiter().GetResult()
    let rowGroupSize = 65536
    let mutable tripIds = Array.zeroCreate<string> rowGroupSize
    let mutable stopSequences = Array.zeroCreate<int> rowGroupSize
    let mutable sourceRouteStopIds = Array.zeroCreate<int64> rowGroupSize
    let mutable buffered = 0
    let mutable rowCount = 0
    let mutable completed = false
    let mutable disposed = false
    let flush () =
        task {
            let writtenTripIds =
                if buffered = rowGroupSize then tripIds else tripIds |> Array.take buffered
            let writtenStopSequences =
                if buffered = rowGroupSize then stopSequences else stopSequences |> Array.take buffered
            let writtenSourceRouteStopIds =
                if buffered = rowGroupSize then sourceRouteStopIds
                else sourceRouteStopIds |> Array.take buffered
            use rowGroup = writer.CreateRowGroup()
            do! rowGroup.WriteAsync(
                    fields.[0], writtenTripIds :> IReadOnlyCollection<string>,
                    Nullable<ReadOnlyMemory<int>>())
            do! rowGroup.WriteAsync<int>(
                    fields.[1], ReadOnlyMemory<int>(writtenStopSequences),
                    Nullable<ReadOnlyMemory<int>>(), null, CancellationToken.None)
            do! rowGroup.WriteAsync<int64>(
                    fields.[2], ReadOnlyMemory<int64>(writtenSourceRouteStopIds),
                    Nullable<ReadOnlyMemory<int>>(), null, CancellationToken.None)
        }
        |> fun operation -> operation.GetAwaiter().GetResult()
        tripIds <- Array.zeroCreate rowGroupSize
        stopSequences <- Array.zeroCreate rowGroupSize
        sourceRouteStopIds <- Array.zeroCreate rowGroupSize
        buffered <- 0
    do
        metadata.Add("obehy.bundle_version", string BundleVersion)
        metadata.Add("obehy.schema_version", string ParquetSchemaVersion)
        metadata.Add("obehy.source_id", descriptor.sourceId)
        metadata.Add("obehy.snapshot_id", $"sha256:{descriptor.payloadSha256}")
        writer.CustomMetadata <- metadata
    member _.Append(tripId: string, stopSequence: int, sourceRouteStopId: int64) =
        if completed then invalidOp "Call metadata writer is already complete"
        tripIds.[buffered] <- tripId
        stopSequences.[buffered] <- stopSequence
        sourceRouteStopIds.[buffered] <- sourceRouteStopId
        buffered <- buffered + 1
        rowCount <- rowCount + 1
        if buffered = rowGroupSize then flush ()
    member _.Complete(expectedRows: int) =
        if not completed then
            if buffered > 0 then flush ()
            completed <- true
        if rowCount <> expectedRows then
            failwith $"Source/GTFS call count mismatch: expected {expectedRows}, got {rowCount}"
        if not disposed then
            disposed <- true
            (writer :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()
            stream.Dispose()
        rowCount
    interface IDisposable with
        member _.Dispose() =
            if not disposed then
                disposed <- true
                (writer :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()
                stream.Dispose()

let private diagnostics (batch: JdfModel.JdfBatch) (feed: GtfsModel.GtfsFeed)
                        (callFacts: CallDerivedFacts)
                        (emittedTransferCalls: HashSet<struct (string * int64)>) =
    let retainedTrips = HashSet<string>(feed.trips |> Seq.map (fun trip -> trip.id))
    let filteredTrips =
        batch.trips
        |> Seq.choose (fun trip ->
            let gtfsId = JdfToGtfs.jdfTripId trip.routeId trip.routeDistinction trip.id
            if retainedTrips.Contains gtfsId then None else Some {
                severity = "warning"; code = "filtered_trip"; sourceObjectId = gtfsId
                message = "Trip has no retained service dates and was omitted"
            })
    let filteredEnrichments =
        let serviceNotes =
            batch.serviceNotes
            |> Seq.choose (fun note ->
                let hasText = nonEmptyText note.note |> Option.isSome
                let isUnhandled = note.noteType.IsNone && not (String.IsNullOrWhiteSpace(note.designation))
                let gtfsTripId = JdfToGtfs.jdfTripId note.routeId note.routeDistinction note.tripId
                if (hasText || isUnhandled) && not (retainedTrips.Contains gtfsTripId) then
                    Some (tripNoticeId note.routeId note.routeDistinction note.tripId note.id)
                else None)
        let reservations =
            batch.reservationOptions
            |> withOwnerOrdinals (fun note -> note.routeId, note.routeDistinction, note.tripId)
            |> Seq.choose (fun (ordinal, note) ->
                let gtfsTripId = JdfToGtfs.jdfTripId note.routeId note.routeDistinction note.tripId
                if not (retainedTrips.Contains gtfsTripId) then
                    Some (reservationNoticeId note.routeId note.routeDistinction note.tripId ordinal)
                else None)
        let transfers =
            batch.transfers
            |> withOwnerOrdinals (fun transfer -> transfer.routeId, transfer.routeDistinction, transfer.tripId)
            |> Seq.choose (fun (ordinal, transfer) ->
                let gtfsTripId =
                    JdfToGtfs.jdfTripId transfer.routeId transfer.routeDistinction transfer.tripId
                if not (retainedTrips.Contains gtfsTripId) then
                    Some (transferId transfer.routeId transfer.routeDistinction transfer.tripId ordinal)
                else None)
        let restrictions = callFacts.filteredRestrictionIds
        Seq.concat [serviceNotes; reservations; transfers; restrictions]
        |> Seq.map (fun sourceObjectId -> {
            severity = "warning"; code = "filtered_enrichment"
            sourceObjectId = sourceObjectId
            message = "Enrichment belongs to a trip omitted from GTFS"
        })
    let unjoinableCallEnrichments =
        let transfers =
            batch.transfers
            |> withOwnerOrdinals (fun transfer -> transfer.routeId, transfer.routeDistinction, transfer.tripId)
            |> Seq.choose (fun (ordinal, transfer) ->
                let gtfsTripId =
                    JdfToGtfs.jdfTripId transfer.routeId transfer.routeDistinction transfer.tripId
                if retainedTrips.Contains gtfsTripId
                   && not (emittedTransferCalls.Contains(struct (gtfsTripId, transfer.routeStopId))) then
                    Some (transferId transfer.routeId transfer.routeDistinction transfer.tripId ordinal)
                else None)
        let restrictions = callFacts.unjoinableRestrictionIds
        Seq.append transfers restrictions
        |> Seq.map (fun sourceObjectId -> {
            severity = "warning"; code = "unjoinable_call_enrichment"
            sourceObjectId = sourceObjectId
            message = "Call-scoped enrichment cannot join an emitted GTFS call"
        })
    let blankNotices =
        let routeNotices =
            batch.routeInfo
            |> Seq.filter (fun note -> String.IsNullOrWhiteSpace(note.text))
            |> Seq.map (fun note -> routeNoticeId note.routeId note.routeDistinction note.id)
        let serviceNotices =
            batch.serviceNotes
            |> Seq.filter (fun note -> note.noteType.IsNone
                                      && String.IsNullOrWhiteSpace(note.designation)
                                      && nonEmptyText note.note |> Option.isNone)
            |> Seq.map (fun note -> tripNoticeId note.routeId note.routeDistinction note.tripId note.id)
        let reservations =
            batch.reservationOptions
            |> withOwnerOrdinals (fun note -> note.routeId, note.routeDistinction, note.tripId)
            |> Seq.filter (fun (_, note) -> String.IsNullOrWhiteSpace(note.note))
            |> Seq.map (fun (ordinal, note) ->
                reservationNoticeId note.routeId note.routeDistinction note.tripId ordinal)
        Seq.concat [routeNotices; serviceNotices; reservations]
        |> Seq.map (fun sourceObjectId -> {
            severity = "warning"; code = "blank_notice"
            sourceObjectId = sourceObjectId
            message = "Textual notice is blank and was omitted"
        })
    let singletonRestrictions = callFacts.singletonRestrictionDiagnostics
    let conflictingRouteStops =
        batch.routeStops
        |> Seq.groupBy (fun stop -> stop.routeId, stop.routeDistinction, stop.routeStopId)
        |> Seq.choose (fun ((routeId, distinction, routeStopId), rows) ->
            let stopIds = rows |> Seq.map (fun stop -> stop.stopId) |> Seq.distinct |> Seq.toArray
            if stopIds.Length <= 1 then None else Some {
                severity = "error"; code = "conflicting_route_stop_zone_mapping"
                sourceObjectId = routeStopSourceId routeId distinction routeStopId
                message = "One source route-stop ID refers to multiple stop places"
            })
    let publicLines = JdfToGtfs.getPublicLineNumbers batch
    let missingLines =
        batch.routes
        |> Seq.filter (fun route -> publicLines.[route.id, route.idDistinction].IsNone)
        |> Seq.map (fun route -> {
            severity = "warning"; code = "missing_public_line_number"
            sourceObjectId = JdfToGtfs.jdfRouteId route.id route.idDistinction
            message = "No unambiguous public line number could be selected"
        })
    let multiZones =
        let zones = feed.czStopZones |> Option.defaultValue [||] |> Seq.groupBy (fun zone -> zone.stopPlaceId)
        zones
        |> Seq.choose (fun (stopPlaceId, memberships) ->
            if memberships |> Seq.map (fun zone -> zone.zoneId) |> Seq.distinct |> Seq.length > 1 then
                Some {
                    severity = "warning"; code = "standard_zone_omitted"
                    sourceObjectId = stopPlaceId
                    message = "Standard GTFS zone_id is blank because this stop has multiple route-scoped zones"
                }
            else None)
    let conflictingPosts = callFacts.conflictingPostDiagnostics
    let missingStopCoordinates =
        feed.stops
        |> Seq.filter (fun stop -> stop.locationType = Some GtfsModel.Station)
        |> Seq.choose (fun stop ->
            match stop.lat, stop.lon with
            | Some 0m, Some 0m -> Some {
                severity = "warning"; code = "missing_stop_coordinates"
                sourceObjectId = stop.id
                message = "Referenced stop has no resolved coordinates and was serialized as 0,0"
              }
            | _ -> None)
    Seq.concat [filteredTrips; filteredEnrichments; unjoinableCallEnrichments; blankNotices
                singletonRestrictions; conflictingRouteStops; missingLines; multiZones
                conflictingPosts; missingStopCoordinates]
    |> Seq.sortBy (fun diagnostic -> diagnostic.code, diagnostic.sourceObjectId)
    |> Seq.toArray

let private writeDiagnostics path diagnostics =
    use stream = File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
    use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))
    writer.WriteStartObject()
    writer.WriteNumber("schema_version", 1)
    writer.WriteStartArray("diagnostics")
    for diagnostic in diagnostics do
        writer.WriteStartObject()
        writer.WriteString("severity", diagnostic.severity)
        writer.WriteString("code", diagnostic.code)
        writer.WriteString("source_object_id", diagnostic.sourceObjectId)
        writer.WriteString("message", diagnostic.message)
        writer.WriteEndObject()
    writer.WriteEndArray()
    writer.WriteEndObject()

let private countTextRows path =
    File.ReadLines(path) |> Seq.skip 1 |> Seq.filter (fun line -> line <> "") |> Seq.length

let private validateStopCoordinates (feed: GtfsModel.GtfsFeed) =
    feed.stops
    |> Seq.iter (fun stop ->
        match stop.lat, stop.lon with
        | Some lat, Some lon
            when lat >= -90m && lat <= 90m && lon >= -180m && lon <= 180m -> ()
        | _ -> invalidArg "feed" $"Stop {stop.id} has missing or out-of-range coordinates")

let private fileEntries maximumWorkers progress root parquetRows =
    let paths =
        Directory.GetFiles(root, "*", SearchOption.AllDirectories)
        |> Array.filter (fun path -> not (Path.GetFileName(path).Equals("manifest.json", StringComparison.Ordinal)))
        |> Array.sortBy (fun path -> Path.GetRelativePath(root,path))
    let results = Array.zeroCreate<FileEntry> paths.Length
    let mutable completed=0L
    let progressLock=obj()
    let calculate index =
        let path=paths.[index]
        let relative = Path.GetRelativePath(root, path).Replace('\\', '/')
        let rows =
            match parquetRows |> Map.tryFind relative with
            | Some count -> Some count
            | None when Path.GetExtension(path).Equals(".txt", StringComparison.OrdinalIgnoreCase) -> Some (countTextRows path)
            | _ -> None
        results.[index] <- { path = relative; sha256 = fileSha256 path; bytes = FileInfo(path).Length; rows = rows }
        let count=Interlocked.Increment(&completed)
        lock progressLock (fun () -> progress count (Some(int64 paths.Length)))
    if maximumWorkers<=1 || paths.Length<=1 then
        for index=0 to paths.Length-1 do calculate index
    else
        Parallel.For(0,paths.Length,ParallelOptions(MaxDegreeOfParallelism=min 2 maximumWorkers),calculate) |> ignore
    results |> Array.sortBy (fun entry -> entry.path)

let private writeManifest path descriptor (converterVersion: string) stopIdsCis
                          (internationalPolicy: JdfToGtfs.InternationalRoutePolicy)
                          (internationalDecisions: JdfToGtfs.InternationalRouteDecision array)
                          (transportModeRules: JdfToGtfs.TransportModeRuleSet)
                          (transportModeDecisions: JdfToGtfs.TransportModeDecision array)
                          (postPlan: JdfToGtfs.PostEstimationPlan)
                          (routingPbfPath: string option)
                          (postInferenceEvidencePath:string option)
                          (postInferencePolicyPath:string option)
                          diagnosticPostLabels
                          (batch: JdfModel.JdfBatch) (feed: GtfsModel.GtfsFeed) files =
    use stream = File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
    use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))
    writer.WriteStartObject()
    writer.WriteString("bundle_format", "obehy-jrutil-jdf")
    writer.WriteNumber("bundle_version", BundleVersion)
    writer.WriteStartObject("source_snapshot")
    writer.WriteNumber("schema_version", 1)
    writer.WriteString("source_id", descriptor.sourceId)
    writer.WriteString("retrieved_at", descriptor.retrievedAt)
    writer.WriteString("retrieval_method", descriptor.retrievalMethod)
    match descriptor.sourceUri with Some value -> writer.WriteString("source_uri", value) | None -> writer.WriteNull("source_uri")
    writer.WriteString("licence", descriptor.licence)
    writer.WriteString("payload_kind", descriptor.payloadKind)
    writer.WriteString("payload_sha256", descriptor.payloadSha256)
    writer.WriteNumber("payload_bytes", descriptor.payloadBytes)
    writer.WriteEndObject()
    writer.WriteStartObject("source_format")
    writer.WriteString("kind", "jdf")
    writer.WriteString("version", batch.version.version)
    match batch.version.duNum with Some value -> writer.WriteNumber("du_number", value) | None -> writer.WriteNull("du_number")
    match batch.version.region with Some value -> writer.WriteString("region", value) | None -> writer.WriteNull("region")
    match batch.version.batchId with Some value -> writer.WriteString("batch_id", value) | None -> writer.WriteNull("batch_id")
    match batch.version.creationDate with Some value -> writer.WriteString("creation_date", localDateString value) | None -> writer.WriteNull("creation_date")
    match batch.version.generator with Some value -> writer.WriteString("generator", value) | None -> writer.WriteNull("generator")
    writer.WriteEndObject()
    writer.WriteStartObject("conversion")
    writer.WriteString("tool", "jrutil")
    writer.WriteString("version", converterVersion)
    writer.WriteBoolean("stop_ids_cis", stopIdsCis)
    writer.WriteStartObject("international_route_filter")
    writer.WriteString("policy", JdfToGtfs.internationalRoutePolicyName internationalPolicy)
    writer.WriteNumber("non_integrated_maximum_trip_span_km", 120)
    writer.WriteNumber("non_integrated_maximum_foreign_depth_km", 60)
    writer.WriteNumber("integrated_maximum_trip_span_km", 200)
    writer.WriteNumber("integrated_maximum_foreign_depth_km", 80)
    writer.WriteNumber(
        "retained_cross_border_route_distinctions",
        internationalDecisions
        |> Seq.filter (fun decision ->
            decision.keep && decision.countries |> Array.exists ((<>) "CZ"))
        |> Seq.length)
    writer.WriteNumber(
        "dropped_route_distinctions",
        internationalDecisions |> Seq.filter (fun decision -> not decision.keep) |> Seq.length)
    writer.WriteNumber("retained_domestic_trips", internationalDecisions |> Seq.sumBy (fun d -> d.retainedDomesticTrips))
    writer.WriteNumber("qualifying_cross_border_trips", internationalDecisions |> Seq.sumBy (fun d -> d.qualifyingCrossBorderTrips))
    writer.WriteNumber("rejected_cross_border_trips", internationalDecisions |> Seq.sumBy (fun d -> d.rejectedCrossBorderTrips))
    writer.WriteNumber("foreign_only_trips_pruned", internationalDecisions |> Seq.sumBy (fun d -> d.foreignOnlyTrips))
    writer.WriteEndObject()
    writer.WriteStartObject("transport_mode_corrections")
    match transportModeRules.sha256 with Some value -> writer.WriteString("rules_sha256", value) | None -> writer.WriteNull("rules_sha256")
    writer.WriteNumber("corrected_routes", transportModeDecisions |> Seq.filter (fun d -> d.corrected) |> Seq.length)
    writer.WriteNumber("guard_mismatches", transportModeDecisions |> Seq.filter (fun d -> not d.corrected) |> Seq.length)
    writer.WriteStartObject("by_effective_mode")
    for mode, values in transportModeDecisions |> Seq.filter (fun d -> d.corrected) |> Seq.groupBy (fun d -> string d.effectiveMode) |> Seq.sortBy fst do
        writer.WriteNumber(mode, values |> Seq.length)
    writer.WriteEndObject()
    writer.WriteEndObject()
    writer.WriteStartObject("route_type_distribution")
    for routeType, values in feed.routes |> Seq.groupBy (fun route -> route.routeType) |> Seq.sortBy fst do
        writer.WriteNumber(routeType, values |> Seq.length)
    writer.WriteEndObject()
    writer.WriteStartObject("estimated_posts")
    let selectedPolicy =
        postInferencePolicyPath |> Option.map JdfPostInferencePolicy.loadPolicy
        |> Option.defaultValue JdfPostInferencePolicy.conservativeRoutedV4
    writer.WriteString("execution_mode",
        if postInferenceEvidencePath.IsSome && routingPbfPath.IsSome then "live"
        elif postInferenceEvidencePath.IsSome then "replay"
        else "disabled")
    writer.WriteString("evaluator_version",JdfPostInference.EvaluatorVersion)
    writer.WriteNumber("policy_schema_version",selectedPolicy.schemaVersion)
    writer.WriteString("policy_id",selectedPolicy.policyId)
    writer.WriteString("policy_sha256",JdfPostInferencePolicy.policySha256 selectedPolicy)
    writer.WriteBoolean("diagnostic_labels", diagnosticPostLabels)
    match routingPbfPath with
    | Some value ->
        writer.WriteString("routing_pbf_sha256", fileSha256 value)
        writer.WriteString("routing_manifest_sha256", fileSha256 (value + ".manifest.json"))
    | None ->
        writer.WriteNull("routing_pbf_sha256")
        writer.WriteNull("routing_manifest_sha256")
    match postInferenceEvidencePath with
    | Some value ->
        let full=Path.GetFullPath(value)
        let evidenceManifest=JdfPostInference.loadEvidenceManifest full
        writer.WriteString("evidence_format",evidenceManifest.evidenceFormat)
        writer.WriteString("evidence_manifest_sha256",fileSha256(Path.Combine(full,"manifest.json")))
        writer.WriteString("routing_evidence_sha256",
            fileSha256(Path.Combine(full,"route_point_evidence.parquet")))
    | None ->
        writer.WriteNull("evidence_format")
        writer.WriteNull("evidence_manifest_sha256")
        writer.WriteNull("routing_evidence_sha256")
    writer.WriteNumber("candidate_bearing_stops", postPlan.candidateStopCount)
    writer.WriteNumber("authored_posts_positioned", postPlan.authored.Count)
    writer.WriteNumber(
        "single_internal_posts",
        postPlan.calls
        |> Seq.map (fun pair -> pair.Value)
        |> Seq.distinctBy (fun selection -> selection.locationId)
        |> Seq.filter (fun selection -> selection.selectionKind = "physical")
        |> Seq.length)
    writer.WriteNumber(
        "composite_internal_posts",
        postPlan.calls
        |> Seq.map (fun pair -> pair.Value)
        |> Seq.distinctBy (fun selection -> selection.locationId)
        |> Seq.filter (fun selection -> selection.selectionKind = "centroid")
        |> Seq.length)
    writer.WriteNumber("single_candidate_skips", postPlan.singleCandidateSkips)
    writer.WriteNumber("side_internal_posts", postPlan.locations |> Seq.filter (fun value -> value.selectionKind = "side") |> Seq.length)
    writer.WriteNumber("physical_internal_posts", postPlan.locations |> Seq.filter (fun value -> value.selectionKind = "physical") |> Seq.length)
    writer.WriteNumber("side_groups", postPlan.sideGroups.Length)
    writer.WriteNumber("centroid_pattern_fallbacks", postPlan.unresolvedPatternContexts.Length)
    writer.WriteNumber("modality_explicit", postPlan.modalityEstimates |> Seq.filter (fun value -> value.status = "Explicit") |> Seq.length)
    writer.WriteNumber("modality_estimated", postPlan.modalityEstimates |> Seq.filter (fun value -> value.status = "Estimated") |> Seq.length)
    writer.WriteNumber("distinct_pattern_scores", postPlan.scoreCount)
    writer.WriteNumber("weak_or_unresolved_contexts", postPlan.unresolvedContexts)
    writer.WriteNumber("two_call_same_stop_blocks", postPlan.sameStopBlocks)
    writer.WriteNumber("distinct_pair_choices", postPlan.distinctPairChoices)
    writer.WriteNumber("unresolved_block_edges", postPlan.unresolvedBlockEdges)
    writer.WriteEndObject()
    writer.WriteEndObject()
    writer.WriteStartArray("files")
    for file in files do
        writer.WriteStartObject()
        writer.WriteString("path", file.path)
        writer.WriteString("sha256", file.sha256)
        writer.WriteNumber("bytes", file.bytes)
        match file.rows with Some rows -> writer.WriteNumber("rows", rows) | None -> writer.WriteNull("rows")
        writer.WriteEndObject()
    writer.WriteEndArray()
    writer.WriteEndObject()

[<Literal>]
let routingEnvelopePolicy = "jdf-routing-envelope-v2"

let validateRoutingPbfManifest path =
    let manifestPath = path + ".manifest.json"
    if not (File.Exists(manifestPath)) then
        invalidArg "routingPbfPath" $"Routing PBF manifest is missing: {manifestPath}"
    use manifest = JsonDocument.Parse(File.ReadAllBytes(manifestPath))
    let mutable schema = Unchecked.defaultof<JsonElement>
    if not (manifest.RootElement.TryGetProperty("filter_schema", &schema))
       || schema.GetString() <> routingEnvelopePolicy then
        invalidArg "routingPbfPath" "Routing PBF was not created by the supported Osmium demand-envelope policy"
    let mutable sourceKey = Unchecked.defaultof<JsonElement>
    if not (manifest.RootElement.TryGetProperty("source_key", &sourceKey))
       || sourceKey.ValueKind <> JsonValueKind.String
       || String.IsNullOrWhiteSpace(sourceKey.GetString()) then
        invalidArg "routingPbfPath" "Routing PBF manifest has no declared OSM source key"
    let mutable output = Unchecked.defaultof<JsonElement>
    let mutable bytes = Unchecked.defaultof<JsonElement>
    if not (manifest.RootElement.TryGetProperty("output", &output))
       || not (output.TryGetProperty("bytes", &bytes))
       || bytes.GetInt64() <> FileInfo(path).Length then
        invalidArg "routingPbfPath" "Routing PBF size does not match its Osmium manifest"

let private writePostReviewGeoJson path selectorsPath stopIdsCis
                                   (batch: JdfModel.JdfBatch)
                                   (plan: JdfToGtfs.PostEstimationPlan) =
    let selectors =
        File.ReadLines(selectorsPath)
        |> Seq.map (fun value -> value.Trim())
        |> Seq.filter (fun value -> value.Length > 0 && not (value.StartsWith("#")))
        |> Seq.toArray
    let stopLabel (stop: JdfModel.Stop) =
        [| Some stop.town; stop.district; stop.nearbyPlace |]
        |> Array.choose id
        |> String.concat ","
    let selectedStopIds = HashSet<int64>()
    for selector in selectors do
        match Int64.TryParse(selector, Globalization.NumberStyles.Integer,
                             Globalization.CultureInfo.InvariantCulture) with
        | true, stopId -> selectedStopIds.Add(stopId) |> ignore
        | _ ->
            let matches =
                batch.stops
                |> Array.filter (fun stop ->
                    String.Equals(stopLabel stop, selector, StringComparison.OrdinalIgnoreCase))
            if matches.Length = 0 then
                Log.Warning("Post review selector did not match a stop: {Selector}", selector)
            for stop in matches do selectedStopIds.Add(stop.id) |> ignore
    let candidates =
        plan.physicalHypotheses
        |> Array.filter (fun candidate -> selectedStopIds.Contains(candidate.stopId))
        |> Array.map (fun candidate -> (candidate.stopId, candidate.hypothesisId), candidate)
        |> Map
    let scores =
        plan.scoreRows ()
        |> Seq.filter (fun score -> selectedStopIds.Contains(score.context.stopId))
        |> Seq.toArray
    let writeOptionString (writer: Utf8JsonWriter) (name: string) (value: string option) =
        match value with Some text -> writer.WriteString(name, text) | None -> writer.WriteNull(name)
    let writeOptionNumber (writer: Utf8JsonWriter) (name: string) (value: float option) =
        match value with Some number -> writer.WriteNumber(name, number) | None -> writer.WriteNull(name)
    let writeContextProperties (writer: Utf8JsonWriter) (score: JdfToGtfs.DerivedPostScore) =
        writer.WriteNumber("stop_id", score.context.stopId)
        writer.WriteString("gtfs_stop_place_id", JdfToGtfs.jdfStopId stopIdsCis score.context.stopId)
        writer.WriteString("candidate_id", score.candidateId)
        writer.WriteString("mode", transportModeCode score.context.mode)
        writer.WriteString("line_id", score.context.lineId)
        writer.WriteNumber("direction", score.context.direction)
        writer.WriteString("pattern_hash", score.context.patternHash)
        writer.WriteString("movement_family_id",score.movementFamilyId)
        writer.WriteNumber("pattern_position", score.context.position)
        writer.WriteString("same_stop_block_role", score.context.sameStopBlockRole)
        writeOptionString writer "corridor_id" score.corridorId
        writeOptionString writer "ingress_thread_id" score.ingressThreadId
        writeOptionString writer "egress_thread_id" score.egressThreadId
        writeOptionString writer "corridor_face_id" score.corridorFaceId
        writer.WriteString("routing_availability", score.routingAvailability)
        writer.WriteNumber("alternative_corridor_count", score.alternativeCorridorCount)
        writeOptionNumber writer "alternative_cost_gap" score.alternativeCostGap
        writeOptionNumber writer "snap_fraction" score.snapFraction
        writeOptionNumber writer "corridor_distance" score.corridorDistance
        writeOptionNumber writer "signed_lateral_offset" score.signedLateralOffset
        writeOptionNumber writer "corridor_heading" score.corridorHeading
        writeOptionNumber writer "attachment_heading" score.attachmentHeading
        writer.WriteNumber("alignment", score.alignment)
        writer.WriteNumber("side", score.side)
        writer.WriteNumber("proximity", score.proximity)
        writer.WriteNumber("routed_fit", score.routedFit)
        writer.WriteNumber("source_adjustment", score.sourceAdjustment)
        writer.WriteNumber("modality_adjustment", score.modalityAdjustment)
        writer.WriteNumber("popularity_prior", score.popularityPrior)
        writer.WriteNumber("total", score.total)
        writeOptionString writer "rejection_reason" score.rejectionReason
        match plan.calls.TryGetValue(score.context) with
        | true, selection ->
            writer.WriteString("decision", selection.selectionKind)
            writer.WriteBoolean("selected", selection.candidateIds |> Array.contains score.candidateId)
        | _ ->
            writer.WriteString("decision", "centroid")
            writer.WriteBoolean("selected", false)
    let writeFeatureStart (writer: Utf8JsonWriter) (featureKind: string) =
        writer.WriteStartObject()
        writer.WriteString("type", "Feature")
        writer.WriteStartObject("properties")
        writer.WriteString("feature_kind", featureKind)
    let writePointGeometry (writer: Utf8JsonWriter) (lon: float) (lat: float) =
        writer.WriteStartObject("geometry")
        writer.WriteString("type", "Point")
        writer.WriteStartArray("coordinates")
        writer.WriteNumberValue(lon)
        writer.WriteNumberValue(lat)
        writer.WriteEndArray()
        writer.WriteEndObject()
        writer.WriteEndObject()
    use stream = File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
    use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))
    writer.WriteStartObject()
    writer.WriteString("type", "FeatureCollection")
    writer.WriteStartArray("features")
    for evidence in batch.postCandidateEvidence do
        if selectedStopIds.Contains(evidence.stopId) then
            let hypothesis =
                plan.physicalHypotheses
                |> Array.tryFind (fun value ->
                    value.stopId=evidence.stopId
                    && value.memberObservationIds |> Array.contains evidence.observationId)
            writeFeatureStart writer "raw_observation"
            writer.WriteNumber("stop_id",evidence.stopId)
            writer.WriteString("observation_id",evidence.observationId)
            writer.WriteString("legacy_candidate_id",evidence.candidateId)
            writer.WriteString("source_kind",evidence.sourceKind)
            writeOptionString writer "source_object_id" evidence.sourceObjectId
            writer.WriteString("lifecycle",evidence.lifecycle)
            writeOptionString writer "hypothesis_id" (hypothesis |> Option.map (fun value -> value.hypothesisId))
            writer.WriteEndObject()
            writePointGeometry writer (float evidence.lon) (float evidence.lat)
    for score in scores do
        match candidates |> Map.tryFind (score.context.stopId, score.candidateId) with
        | None -> ()
        | Some candidate ->
            writeFeatureStart writer "hypothesis"
            writeContextProperties writer score
            writer.WriteString("member_observation_ids",String.Join(";",candidate.memberObservationIds))
            writer.WriteString("member_legacy_candidate_ids",String.Join(";",candidate.memberCandidateIds))
            writer.WriteString("representative_candidate_id",candidate.representativeCandidateId)
            writer.WriteEndObject()
            writePointGeometry writer (float candidate.lon) (float candidate.lat)
    let tangents =
        scores
        |> Array.choose (fun score ->
            match score.corridorHeading, score.signedLateralOffset,
                  candidates |> Map.tryFind (score.context.stopId, score.candidateId) with
            | Some heading, Some signed, Some candidate -> Some(score, heading, signed, candidate)
            | _ -> None)
        |> Array.distinctBy (fun (score, _, _, _) ->
            score.context.stopId, score.context.patternHash,
            score.context.position, score.corridorFaceId)
    for score, heading, signed, candidate in tangents do
        let radians = heading * Math.PI / 180.0
        let east, north = Math.Sin(radians), Math.Cos(radians)
        let lat = float candidate.lat
        let metresPerLat = 111_320.0
        let metresPerLon = max 1.0 (metresPerLat * Math.Cos(lat * Math.PI / 180.0))
        let centreLon = float candidate.lon + north * signed / metresPerLon
        let centreLat = lat - east * signed / metresPerLat
        let extent = 30.0
        let lon0, lat0 = centreLon - east * extent / metresPerLon, centreLat - north * extent / metresPerLat
        let lon1, lat1 = centreLon + east * extent / metresPerLon, centreLat + north * extent / metresPerLat
        writeFeatureStart writer "corridor_tangent"
        writeContextProperties writer score
        writer.WriteEndObject()
        writer.WriteStartObject("geometry")
        writer.WriteString("type", "LineString")
        writer.WriteStartArray("coordinates")
        writer.WriteStartArray(); writer.WriteNumberValue(lon0); writer.WriteNumberValue(lat0); writer.WriteEndArray()
        writer.WriteStartArray(); writer.WriteNumberValue(lon1); writer.WriteNumberValue(lat1); writer.WriteEndArray()
        writer.WriteEndArray()
        writer.WriteEndObject()
        writer.WriteEndObject()
    for group in plan.sideGroups |> Array.filter (fun value -> selectedStopIds.Contains(value.stopId)) do
        match candidates |> Map.tryFind (group.stopId, group.representativeCandidateId) with
        | None -> ()
        | Some representative ->
            writeFeatureStart writer "face"
            writer.WriteNumber("stop_id", group.stopId)
            writer.WriteString("side_group_id", group.sideGroupId)
            writer.WriteString("mode", string group.mode)
            writer.WriteString("corridor_face_id", group.corridorFaceId)
            writer.WriteString("sector", group.sector)
            writer.WriteString("representative_candidate_id", group.representativeCandidateId)
            writer.WriteString("member_candidate_ids", String.Join(";", group.memberCandidateIds))
            writer.WriteNumber("repeated_pattern_support", group.repeatedPatternSupport)
            writer.WriteEndObject()
            writePointGeometry writer (float representative.lon) (float representative.lat)
    writer.WriteEndArray()
    writer.WriteEndObject()
    writer.Flush()

let private capturePostInferenceEvidenceOnly snapshotDescriptorPath converterVersion stopIdsCis
                                                   (internationalPolicy:JdfToGtfs.InternationalRoutePolicy)
                                                   internationalOverrides transportModeRules
                                                   routingPbfPath
                                                   (executionOptions:BundleExecutionOptions)
                                                   inputPath evidencePath =
    if String.IsNullOrWhiteSpace(converterVersion) then
        invalidArg "converterVersion" "Converter version is required"
    if not(jdfInputContainsRelation inputPath "JrutilPostCandidateEvidence.txt") then
        invalidArg "inputPath"
            "Post-inference evidence capture requires JrutilPostCandidateEvidence.txt"
    validateRoutingPbfManifest routingPbfPath
    let descriptor=loadSnapshotDescriptor snapshotDescriptorPath
    validateSnapshot descriptor inputPath
    let phaseTimer=Stopwatch.StartNew()
    let progress phase state completed total unit detail workers =
        reportProgress executionOptions phaseTimer phase state completed total unit detail workers
    progress "validate-snapshot" "completed" 1L (Some 1L) "items" None 0
    use graph =
        GeoData.Osm.PackedRoutingGraph.OpenWithProgress(
            routingPbfPath,
            (fun phase count total ->
                progress phase "running" count total "items" None 1),
            buildGlobalSnaps=false)
    graph.MaximumWorkers<-executionOptions.maximumWorkers
    withJdfInput inputPath (fun source ->
        progress "parse-jdf" "started" 0L None "bytes" None 0
        let sourceBatch=Jdf.jdfBatchDirParser () source
        progress "parse-jdf" "completed" (int64 sourceBatch.tripStops.Length) None "calls" None 0
        progress "prepare-calendar" "started" 0L (Some(int64 sourceBatch.trips.Length)) "trips" None 0
        let sourceCalendar =
            JdfToGtfs.prepareGtfsCalendarWithWorkersAndProgress
                executionOptions.maximumWorkers
                (fun count total ->
                    progress "calendar-trips" "running" count total "trips" None
                             executionOptions.maximumWorkers)
                sourceBatch
        progress "prepare-calendar" "completed" (int64 sourceBatch.trips.Length)
                 (Some(int64 sourceBatch.trips.Length)) "trips" None 0
        let sourceRouteKeys =
            sourceBatch.routes |> Seq.map(fun route -> route.id,route.idDistinction) |> Set
        JdfToGtfs.validateInternationalRouteOverrides sourceRouteKeys internationalOverrides
        let filterResult =
            JdfToGtfs.applyInternationalRoutePolicyWithCalendarWorkersAndProgress
                executionOptions.maximumWorkers
                (fun phase count total ->
                    progress phase "running" count total "items" None executionOptions.maximumWorkers)
                internationalPolicy internationalOverrides sourceCalendar sourceBatch
        JdfToGtfs.logInternationalRouteDecisions internationalPolicy filterResult.decisions
        let batch,_=JdfToGtfs.applyTransportModeRules transportModeRules filterResult.batch
        let evidenceFull=Path.GetFullPath(evidencePath)
        let evidenceParent=Path.GetDirectoryName(evidenceFull)
        if String.IsNullOrWhiteSpace evidenceParent then
            invalidArg "evidencePath" "Evidence output requires a parent directory"
        let drive=DriveInfo(Path.GetPathRoot(evidenceParent))
        let mutable estimatedBytes=0L
        let mutable requiredFreeBytes=0L
        let capturePreflight bounds =
            estimatedBytes<-estimatePostEvidenceOutputBytes bounds
            // Evidence is written to a sibling temporary directory and made
            // visible with Directory.Move.  A same-volume rename does not
            // require a second full copy of the pack.
            requiredFreeBytes<-estimatedBytes+PostEvidenceOutputSafetyReserveBytes
            let detail =
                $"estimated_evidence_bytes={estimatedBytes}; atomic_headroom_bytes={requiredFreeBytes}; observations={bounds.observationCount}; route_points={bounds.routePointCount}; source_contexts={bounds.sourceContextCount}; contexts={bounds.contextCount}; routing_evidence={bounds.routingEvidenceCount}; corridor_variants_upper_bound={bounds.corridorVariantCount}; route_point_evidence_upper_bound={bounds.routePointEvidenceCount}"
            progress "capture-disk-preflight" "running" drive.AvailableFreeSpace
                     (Some requiredFreeBytes) "bytes" (Some detail) 0
            if drive.AvailableFreeSpace<requiredFreeBytes then
                invalidArg "evidencePath"
                    $"Insufficient free disk for deterministic evidence capture: required={requiredFreeBytes}, available={drive.AvailableFreeSpace}"
            progress "capture-disk-preflight" "completed" drive.AvailableFreeSpace
                     (Some requiredFreeBytes) "bytes" (Some detail) 0
        let queryCoordinates = [|
            for location in batch.stopLocations do
                if location.precision=JdfModel.StopPrecise then
                    yield struct(float location.lon,float location.lat)
            for observation in batch.postCandidateEvidence do
                yield struct(float observation.lon,float observation.lat) |]
        progress "prepare-routing-snaps" "started" 0L (Some(int64 graph.EdgeCount)) "edges" None 0
        graph.PrepareKnownSnaps(
            queryCoordinates,
            fun count total ->
                progress "routing-snaps" "running" count total "edges" None
                         executionOptions.maximumWorkers)
        progress "prepare-routing-snaps" "completed" (int64 graph.EdgeCount)
                 (Some(int64 graph.EdgeCount)) "edges" None 0
        use captured =
            JdfPostEvidence.captureToStore
                { maximumWorkers=executionOptions.maximumWorkers
                  memoryBudgetBytes=executionOptions.memoryBudgetBytes
                  preflight=capturePreflight
                  progress=fun phase count total detail ->
                      progress phase "running" count total "items" detail
                               executionOptions.maximumWorkers }
                graph batch
        progress "capture-post-inference-evidence" "started" 0L None "rows" None 0
        writePostEvidenceStore descriptor converterVersion stopIdsCis evidencePath routingPbfPath captured
            (fun phase count total -> progress phase "running" count total "rows" None 1)
        let routingHash=fileSha256 routingPbfPath
        use store=JdfPostEvidenceStore.openValidatedStore
                      { mergedJdfSha256=Some descriptor.payloadSha256
                        routingPbfSha256=Some routingHash
                        captureToolVersion=Some converterVersion }
                      evidencePath
        progress "capture-post-inference-evidence" "completed"
                 store.Manifest.routePointEvidenceCount
                 (Some store.Manifest.routePointEvidenceCount) "rows" None 0
        CaptureCompleted(
            store.Manifest,
            { estimatedEvidenceBytes=estimatedBytes
              atomicOutputHeadroomBytes=requiredFreeBytes
              currentSpillBytes=captured.CurrentSpillBytes
              peakSpillBytes=captured.PeakSpillBytes
              maximumWorkers=executionOptions.maximumWorkers }))

let private writeBundleWithPolicyCore snapshotDescriptorPath converterVersion stopIdsCis
                                      (internationalPolicy: JdfToGtfs.InternationalRoutePolicy)
                                      (internationalOverrides: JdfToGtfs.InternationalRouteOverride array)
                                      (transportModeRules: JdfToGtfs.TransportModeRuleSet)
                                      estimatedPosts
                                      (routingPbfPath: string option)
                                      diagnosticPostLabels
                                      (executionOptions: BundleExecutionOptions)
                                      inputPath outputPath =
    if String.IsNullOrWhiteSpace(converterVersion) then invalidArg "converterVersion" "Converter version is required"
    let rawProgress=executionOptions.progress
    let progressGate=Dictionary<string,int64>(StringComparer.Ordinal)
    let progressLock=obj()
    let throttledProgress event =
        let emit =
            lock progressLock (fun () ->
                if event.state<>"running" then true else
                match progressGate.TryGetValue(event.phase) with
                | true,last when event.elapsedMilliseconds-last<1000L -> false
                | _ ->
                    progressGate.[event.phase] <- event.elapsedMilliseconds
                    true)
        if emit then rawProgress event
    let executionOptions={ executionOptions with progress=throttledProgress }
    let phaseTimer = Stopwatch.StartNew()
    let started phase total unit =
        JdfPostInferencePolicy.PostInferencePhaseProbe.record phase
        reportProgress executionOptions phaseTimer phase "started" 0L total unit None 0
    let progressCompleted phase count total unit =
        reportProgress executionOptions phaseTimer phase "completed" count total unit None 0
    started "validate-snapshot" None "items"
    Log.Information("Bundle phase: loading and validating snapshot descriptor")
    let descriptor = loadSnapshotDescriptor snapshotDescriptorPath
    validateSnapshot descriptor inputPath
    if executionOptions.capturePostInferenceEvidencePath.IsSome
       && executionOptions.postInferenceEvidencePath.IsSome then
        invalidArg "executionOptions"
            "--capture-post-inference-evidence and --post-inference-evidence are mutually exclusive"
    if executionOptions.postInferenceEvidenceOnly then
        invalidArg "executionOptions" "Capture-only execution must use the dedicated capture dispatcher"
    if executionOptions.capturePostInferenceEvidencePath.IsSome
       && not executionOptions.postInferenceEvidenceOnly then
        invalidArg "executionOptions"
            "Evidence capture currently requires --post-inference-evidence-only so policy evaluation cannot contaminate the captured route points"
    if executionOptions.capturePostInferenceEvidencePath.IsSome
       && (not estimatedPosts || routingPbfPath.IsNone) then
        invalidArg "executionOptions" "Post-inference evidence capture requires routed estimation and --routing-osm-pbf"
    if executionOptions.postInferenceEvidencePath.IsSome && not estimatedPosts then
        invalidArg "executionOptions" "--post-inference-evidence requires estimated-post inference"
    if executionOptions.postInferenceEvidencePath.IsSome && routingPbfPath.IsSome then
        invalidArg "executionOptions"
            "--post-inference-evidence reuses captured routes and cannot be combined with --routing-osm-pbf"
    if not executionOptions.includePostInferenceScores && executionOptions.reviewStopsPath.IsSome then
        invalidArg "executionOptions"
            "--no-post-inference-scores cannot be combined with --post-review-stops"
    let replayEvidence =
        executionOptions.postInferenceEvidencePath
        |> Option.map(fun path ->
            let full=Path.GetFullPath(path)
            let store=JdfPostEvidenceStore.openValidatedStore
                          { JdfPostEvidenceStore.noIdentityExpectation with
                              mergedJdfSha256=Some descriptor.payloadSha256 }
                          full
            let manifest=store.Manifest
            let policy =
                executionOptions.postInferencePolicyPath
                |> Option.map JdfPostInferencePolicy.loadPolicy
                |> Option.defaultValue JdfPostInferencePolicy.conservativeRoutedV4
                |> JdfPostInferencePolicy.validatePolicyForEvidence manifest.captureCeilings.routedExcessMetres manifest.captureCeilings.maximumCorridorVariants
            store,manifest,policy)
    let livePolicy =
        executionOptions.postInferencePolicyPath
        |> Option.map JdfPostInferencePolicy.loadPolicy
        |> Option.defaultValue JdfPostInferencePolicy.conservativeRoutedV4
        |> JdfPostInferencePolicy.validatePolicy JdfPostInference.CaptureRoutedExcessHorizonMetres
        |> Some
    if estimatedPosts && routingPbfPath.IsSome
       && not (jdfInputContainsRelation inputPath "JrutilPostCandidateEvidence.txt") then
        invalidArg "inputPath"
            "Routed post inference requires JrutilPostCandidateEvidence.txt; the supplied JDF input has no observation relation"
    let outputFull = Path.GetFullPath(outputPath)
    if Directory.Exists(outputFull) || File.Exists(outputFull) then
        invalidArg "outputPath" $"Bundle output already exists: {outputFull}"
    let parent = Path.GetDirectoryName(outputFull)
    if String.IsNullOrEmpty(parent) then invalidArg "outputPath" "Bundle output requires a parent directory"
    Directory.CreateDirectory(parent) |> ignore
    let temp = Path.Combine(parent, $".{Path.GetFileName(outputFull)}.tmp-{Guid.NewGuid():N}")
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "bundle-staging"
    Directory.CreateDirectory(temp) |> ignore
    let mutable completed = false
    let mutable liveEvidenceTemporaryDirectory:string option=None
    // PBF decoding and JDF parsing are independent and predominantly use
    // different resources. Starting the graph build here hides most of the
    // parse/calendar/filter latency without changing either result.
    let routingGraphTask =
        match estimatedPosts, routingPbfPath,replayEvidence with
        | true, Some path,None ->
            validateRoutingPbfManifest path
            Task.Run(fun () ->
                let graph =
                    GeoData.Osm.PackedRoutingGraph.OpenWithProgress(
                        path,
                        (fun phase count total ->
                            reportProgress executionOptions phaseTimer phase "running"
                                           count total "items" None 1),
                        buildGlobalSnaps=false)
                graph.MaximumWorkers <- executionOptions.maximumWorkers
                Some graph)
        | _ -> Task.FromResult(None)
    let mutable routingGraph: GeoData.Osm.PackedRoutingGraph option = None
    progressCompleted "validate-snapshot" 1L (Some 1L) "items"
    try
        withJdfInput inputPath (fun source ->
            started "parse-jdf" None "bytes"
            Log.Information("Bundle phase: parsing merged JDF")
            let sourceBatch = Jdf.jdfBatchDirParser () source
            logPhaseResources "parse-jdf" phaseTimer
            progressCompleted "parse-jdf" (int64 sourceBatch.tripStops.Length) None "calls"
            started "prepare-calendar" (Some (int64 sourceBatch.trips.Length)) "trips"
            let sourceCalendar =
                JdfToGtfs.prepareGtfsCalendarWithWorkersAndProgress
                    executionOptions.maximumWorkers
                    (fun count total ->
                        reportProgress executionOptions phaseTimer "calendar-trips" "running"
                                       count total "trips" None executionOptions.maximumWorkers)
                    sourceBatch
            progressCompleted "prepare-calendar" (int64 sourceBatch.trips.Length)
                              (Some (int64 sourceBatch.trips.Length)) "trips"
            started "filter-international" (Some (int64 sourceBatch.routes.Length)) "routes"
            Log.Information("Bundle phase: applying international trip policy")
            let sourceRouteKeys =
                sourceBatch.routes
                |> Seq.map (fun route -> route.id, route.idDistinction)
                |> Set
            let sourceTransportModes =
                sourceBatch.routes
                |> Seq.map (fun route -> (route.id, route.idDistinction), route.transportMode)
                |> Map
            JdfToGtfs.validateInternationalRouteOverrides
                sourceRouteKeys internationalOverrides
            let filterResult =
                JdfToGtfs.applyInternationalRoutePolicyWithCalendarWorkersAndProgress
                    executionOptions.maximumWorkers
                    (fun phase count total ->
                        reportProgress executionOptions phaseTimer phase "running"
                                       count total "items" None executionOptions.maximumWorkers)
                    internationalPolicy internationalOverrides sourceCalendar sourceBatch
            JdfToGtfs.logInternationalRouteDecisions internationalPolicy filterResult.decisions
            let correctedBatch, transportModeDecisions =
                JdfToGtfs.applyTransportModeRules transportModeRules filterResult.batch
            let batch =
                if estimatedPosts then correctedBatch
                else { correctedBatch with
                           postCandidateEvidence = [||] }
            let retainedCalendar =
                lazy(JdfToGtfs.filterCalendarPreparation batch sourceCalendar)
            logPhaseResources "filter-jdf" phaseTimer
            progressCompleted "filter-international" (int64 sourceBatch.routes.Length)
                      (Some (int64 sourceBatch.routes.Length)) "routes"
            started "await-routing-graph" None "items"
            routingGraph <- routingGraphTask.GetAwaiter().GetResult()
            progressCompleted "await-routing-graph"
                              (if routingGraph.IsSome then 1L else 0L)
                              (Some (if routingGraph.IsSome then 1L else 0L)) "items"
            routingGraph |> Option.iter (fun graph ->
                let queryCoordinates = [|
                    for location in batch.stopLocations do
                        if location.precision = JdfModel.StopPrecise then
                            yield struct (float location.lon,float location.lat)
                    for observation in batch.postCandidateEvidence do
                        yield struct (float observation.lon,float observation.lat)
                |]
                started "prepare-routing-snaps" (Some (int64 graph.EdgeCount)) "edges"
                graph.PrepareKnownSnaps(
                    queryCoordinates,
                    fun count total ->
                        reportProgress executionOptions phaseTimer "routing-snaps" "running"
                                       count total "edges" None executionOptions.maximumWorkers)
                progressCompleted "prepare-routing-snaps" (int64 graph.EdgeCount)
                                  (Some (int64 graph.EdgeCount)) "edges")
            started "prepare-inference" None "contexts"
            Log.Information("Bundle phase: preparing streaming JDF to GTFS conversion")
            let preparation =
                match replayEvidence,routingGraph with
                | Some(evidenceStore,manifest,policy),_ ->
                    started "replay-post-inference" (Some manifest.routePointEvidenceCount) "rows"
                    let postPlan =
                        JdfPostInferenceEvaluator.evaluateWithDiagnostics
                            executionOptions.includePostInferenceScores evidenceStore policy
                        |> JdfToGtfs.postEstimationPlanFromInferenceResult
                    progressCompleted "replay-post-inference" manifest.routePointEvidenceCount
                                      (Some manifest.routePointEvidenceCount) "rows"
                    JdfToGtfs.prepareGtfsFeedForStreamingBundleWithCalendarAndPostPlan
                        stopIdsCis retainedCalendar.Value postPlan batch
                | None,Some graph ->
                    let progress phase count total detail =
                        reportProgress executionOptions phaseTimer phase "running"
                                       count total "items" detail executionOptions.maximumWorkers
                    // Graph-backed and replay-backed conversion deliberately
                    // meet at the same persisted evidence contract. The
                    // routing pass emits raw route-point facts only; every
                    // consolidation and publication decision is made by the
                    // evaluator below.
                    use evidenceStore =
                        JdfPostEvidence.captureToStore
                            { maximumWorkers=executionOptions.maximumWorkers
                              memoryBudgetBytes=executionOptions.memoryBudgetBytes
                              preflight=ignore
                              progress=progress }
                            graph batch
                    let evidencePath =
                        match executionOptions.capturePostInferenceEvidencePath with
                        | Some path -> path
                        | None ->
                            let path=Path.Combine(Path.GetTempPath(),$"jrutil-post-evidence-{Guid.NewGuid():N}")
                            liveEvidenceTemporaryDirectory<-Some path
                            path
                    let routingPath=routingPbfPath.Value
                    let routingHash=fileSha256 routingPath
                    started "capture-post-inference-evidence" None "rows"
                    writePostEvidenceStore descriptor converterVersion stopIdsCis evidencePath routingPath evidenceStore
                        (fun phase count total ->
                            reportProgress executionOptions phaseTimer phase "running" count total "rows" None 1)
                    use evidenceStore=JdfPostEvidenceStore.openValidatedStore
                                          { mergedJdfSha256=Some descriptor.payloadSha256
                                            routingPbfSha256=Some routingHash
                                            captureToolVersion=Some converterVersion }
                                          evidencePath
                    let manifest=evidenceStore.Manifest
                    progressCompleted "capture-post-inference-evidence" manifest.routePointEvidenceCount
                                      (Some manifest.routePointEvidenceCount) "rows"
                    started "evaluate-post-inference" (Some manifest.routePointEvidenceCount) "rows"
                    let postPlan =
                        JdfPostInferenceEvaluator.evaluateWithDiagnostics
                            executionOptions.includePostInferenceScores evidenceStore livePolicy.Value
                        |> JdfToGtfs.postEstimationPlanFromInferenceResult
                    progressCompleted "evaluate-post-inference" manifest.routePointEvidenceCount
                                      (Some manifest.routePointEvidenceCount) "rows"
                    JdfToGtfs.prepareGtfsFeedForStreamingBundleWithCalendarAndPostPlan
                        stopIdsCis retainedCalendar.Value postPlan batch
                | None,None ->
                    JdfToGtfs.prepareGtfsFeedForStreamingBundleWithCalendar
                        stopIdsCis retainedCalendar.Value batch
            routingGraph |> Option.iter (fun graph ->
                Log.Information(
                    "Routing metrics: mapped_bytes={MappedBytes}; nodes={Nodes}; edges={Edges}; searches={Searches}; cache_entries={CacheEntries}; cache_hits={CacheHits}; cache_misses={CacheMisses}; restriction_lookups={RestrictionLookups}; restriction_rules_examined={RestrictionRulesExamined}",
                    graph.EstimatedMappedBytes, graph.NodeCount, graph.EdgeCount,
                    graph.Searches, graph.CacheEntries, graph.CacheHits, graph.CacheMisses,
                    graph.RestrictionLookups, graph.RestrictionRulesExamined))
            logPhaseResources "prepare-inference" phaseTimer
            progressCompleted "prepare-inference" preparation.postPlan.scoreCount None "scores"
            match executionOptions.reviewStopsPath with
            | Some reviewStopsPath when estimatedPosts ->
                if not (File.Exists(reviewStopsPath)) then
                    invalidArg "reviewStopsPath" $"Post review stop file does not exist: {reviewStopsPath}"
                started "write-post-review" (Some 1L) "files"
                writePostReviewGeoJson (Path.Combine(temp, "post-review.geojson"))
                                       reviewStopsPath stopIdsCis batch preparation.postPlan
                progressCompleted "write-post-review" 1L (Some 1L) "files"
            | Some _ -> Log.Warning("Ignoring --post-review-stops because estimated posts are disabled")
            | None -> ()
            // Routing is conversion-local and no later bundle phase consults
            // the graph. Release mappings and temporary files before the
            // 17-million-row output stream begins.
            routingGraph |> Option.iter (fun graph -> (graph :> IDisposable).Dispose())
            routingGraph <- None
            let candidateStopsWithMultiple =
                batch.postCandidateEvidence
                |> Seq.groupBy (fun observation -> observation.stopId)
                |> Seq.choose (fun (stopId,observations) ->
                    if observations |> Seq.distinctBy(fun value -> value.lat,value.lon) |> Seq.length >= 2
                    then Some stopId else None)
                |> HashSet
            let retainedTripIdsForCalls =
                batch.trips
                |> Seq.map (fun trip -> JdfToGtfs.jdfTripId trip.routeId trip.routeDistinction trip.id)
                |> Seq.filter (preparation.tripsToDelete.Contains >> not)
                |> HashSet
            started "prepare-call-diagnostics" (Some(int64 batch.tripStops.Length)) "calls"
            let callFacts =
                scanCallDerivedFacts batch retainedTripIdsForCalls candidateStopsWithMultiple (fun count total ->
                    reportProgress executionOptions phaseTimer "prepare-call-diagnostics" "running"
                                   count total "calls" None 1)
            progressCompleted "prepare-call-diagnostics" (int64 batch.tripStops.Length)
                              (Some(int64 batch.tripStops.Length)) "calls"
            let referencedStopIds = HashSet<string>(StringComparer.Ordinal)
            let mutable stopTimeCount = 0
            let callTableName = "source_call_metadata.parquet"
            let transferCallQueries = HashSet<struct (string * int64)>()
            for transfer in batch.transfers do
                transferCallQueries.Add(
                    struct (JdfToGtfs.jdfTripId transfer.routeId transfer.routeDistinction transfer.tripId,
                            transfer.routeStopId))
                |> ignore
            let emittedTransferCalls = HashSet<struct (string * int64)>()
            use callMetadataWriter =
                new CallMetadataParquetWriter(descriptor, Path.Combine(temp, callTableName))
            let stopTimes =
                JdfToGtfs.getStreamingBundleStopTimeRows preparation
                |> Seq.map (fun row ->
                    let stopTime = row.stopTime
                    referencedStopIds.Add(stopTime.stopId) |> ignore
                    stopTimeCount <- stopTimeCount + 1
                    callMetadataWriter.Append(
                        stopTime.tripId, stopTime.stopSequence, row.sourceRouteStopId)
                    let transferKey = struct (stopTime.tripId, row.sourceRouteStopId)
                    if transferCallQueries.Contains(transferKey) then
                        emittedTransferCalls.Add(transferKey) |> ignore
                    if stopTimeCount % 250_000 = 0 then
                        reportProgress executionOptions phaseTimer "stream-stop-times" "running"
                                       (int64 stopTimeCount) (Some callFacts.emittedCallCount)
                                       "rows" None 1
                    stopTime)
            let gtfsPath = Path.Combine(temp, "gtfs-intermediate")
            let extensionsPath = Path.Combine(temp, "extensions")
            Log.Information("Bundle phase: streaming GTFS stop times")
            started "stream-stop-times" (Some callFacts.emittedCallCount) "calls"
            Gtfs.gtfsStopTimesToFolder () gtfsPath stopTimes
            logPhaseResources "stream-stop-times" phaseTimer
            progressCompleted "stream-stop-times" (int64 stopTimeCount) (Some callFacts.emittedCallCount) "rows"
            Log.Information("Bundle phase: preparing remaining GTFS relations")
            started "prepare-remaining-gtfs" (Some 1L) "feeds"
            let feed =
                JdfToGtfs.finishStreamingBundleFeed preparation (referencedStopIds |> Set.ofSeq)
                |> Gtfs.deduplicateCalendar
                |> Gtfs.fillStandardRequiredFields
                |> fun value ->
                    if diagnosticPostLabels then
                        applyDiagnosticPostLabels stopIdsCis batch preparation.postPlan value
                    else value
            logPhaseResources "prepare-remaining-gtfs" phaseTimer
            progressCompleted "prepare-remaining-gtfs" 1L (Some 1L) "feeds"
            validateStopCoordinates feed
            Log.Information("Bundle phase: writing remaining GTFS tables")
            started "write-relations" None "tables"
            Gtfs.gtfsStandardTablesExceptStopTimesToFolder () gtfsPath feed
            Log.Information("Bundle phase: writing GTFS extension tables")
            Gtfs.gtfsExtensionsToFolder () extensionsPath feed
            let retainedTrips = HashSet<string>(feed.trips |> Seq.map (fun trip -> trip.id))
            let callRows = callMetadataWriter.Complete(stopTimeCount)
            Log.Information("Bundle phase: preparing Parquet relations")
            let tables, assignmentCount, assignmentRows =
                getTableProducers stopIdsCis sourceTransportModes batch feed preparation.postPlan callFacts
                    (fun phase count total ->
                        reportProgress executionOptions phaseTimer phase "running"
                                       count total "rows" None 1)
                    emittedTransferCalls
            let totalParquetTables=tables.Length+2
            let mutable parquetRows = Map [callTableName, callRows]
            for index, (name, produceTable) in tables |> Array.indexed do
                reportProgress executionOptions phaseTimer "write-parquet" "running"
                               (int64 index) (Some (int64 totalParquetTables)) "tables" (Some name) 1
                let parquetTable = produceTable ()
                Log.Information(
                    "Bundle phase: writing Parquet table {Index}/{Total}: {Table} ({Rows} rows)",
                    index + 1, totalParquetTables, name, parquetTable.rows.Count)
                writeParquet descriptor (Path.Combine(temp, name)) parquetTable
                parquetRows <- parquetRows |> Map.add name parquetTable.rows.Count
                reportProgress executionOptions phaseTimer "write-parquet" "running"
                               (int64 (index+1)) (Some (int64 totalParquetTables)) "tables" (Some name) 1
            let assignmentTableName="derived_post_assignments.parquet"
            reportProgress executionOptions phaseTimer "write-parquet" "running"
                           (int64 tables.Length) (Some (int64 totalParquetTables))
                           "tables" (Some assignmentTableName) 1
            Log.Information(
                "Bundle phase: streaming Parquet table {Index}/{Total}: {Table} ({Rows} rows)",
                tables.Length+1,totalParquetTables,assignmentTableName,assignmentCount)
            let assignmentRowsWritten =
                writeDerivedPostAssignmentsParquet descriptor (Path.Combine(temp,assignmentTableName))
                    assignmentCount assignmentRows
                    (fun count total ->
                        reportProgress executionOptions phaseTimer "stream-derived-post-assignments" "running"
                                       count total "rows" None 1)
            parquetRows <- parquetRows |> Map.add assignmentTableName assignmentRowsWritten
            let scoreTableName="derived_post_scores.parquet"
            reportProgress executionOptions phaseTimer "write-parquet" "running"
                           (int64 (tables.Length+1)) (Some (int64 totalParquetTables))
                           "tables" (Some scoreTableName) 1
            Log.Information(
                "Bundle phase: streaming Parquet table {Index}/{Total}: {Table} ({Rows} rows)",
                totalParquetTables,totalParquetTables,scoreTableName,preparation.postPlan.scoreCount)
            let scoreRows =
                try
                    writeDerivedPostScoresParquet descriptor stopIdsCis (Path.Combine(temp,scoreTableName))
                        preparation.postPlan
                        (fun count total ->
                            reportProgress executionOptions phaseTimer "stream-derived-post-scores" "running"
                                           count total "rows" None 1)
                finally
                    preparation.postPlan.cleanupScoreRows()
            parquetRows <- parquetRows |> Map.add scoreTableName scoreRows
            reportProgress executionOptions phaseTimer "write-parquet" "running"
                           (int64 totalParquetTables) (Some (int64 totalParquetTables))
                           "tables" (Some scoreTableName) 1
            logPhaseResources "stream-parquet" phaseTimer
            progressCompleted "write-relations" (int64 totalParquetTables)
                              (Some (int64 totalParquetTables)) "tables"
            Log.Information("Bundle phase: creating diagnostics")
            started "write-diagnostics" (Some 1L) "files"
            let internationalDiagnostics =
                filterResult.decisions
                |> Seq.filter (fun decision ->
                    not decision.keep || decision.rejectedCrossBorderTrips > 0 || decision.foreignOnlyTrips > 0)
                |> Seq.map (fun decision ->
                    let countries = String.Join(",", decision.countries)
                    let span = decision.maximumTripSpanKm |> Option.map string |> Option.defaultValue "missing"
                    let depth = decision.maximumForeignDepthKm |> Option.map string |> Option.defaultValue "missing"
                    let overrideValue =
                        match decision.overrideDecision with
                        | Some JdfToGtfs.KeepRoute -> "keep"
                        | Some JdfToGtfs.DropRoute -> "drop"
                        | None -> "none"
                    { severity = "warning"
                      code = if decision.keep then "filtered_international_trips" else "filtered_international_route"
                      sourceObjectId = JdfToGtfs.jdfRouteId decision.routeId decision.routeDistinction
                      message = $"{decision.reason}; countries={countries}; maximum_trip_span_km={span}; maximum_foreign_depth_km={depth}; integrated={decision.integrated}; override={overrideValue}; domestic={decision.retainedDomesticTrips}; qualifying_cross_border={decision.qualifyingCrossBorderTrips}; rejected_cross_border={decision.rejectedCrossBorderTrips}; foreign_only={decision.foreignOnlyTrips}" })
            let transportModeDiagnostics =
                transportModeDecisions
                |> Seq.map (fun decision ->
                    { severity = if decision.corrected then "info" else "warning"
                      code = if decision.corrected then "corrected_transport_mode" else "transport_mode_rule_mismatch"
                      sourceObjectId = JdfToGtfs.jdfRouteId decision.routeId decision.routeDistinction
                      message = $"{decision.message}; effective_mode={decision.effectiveMode}" })
            let bundleDiagnostics =
                Seq.concat [ diagnostics batch feed callFacts emittedTransferCalls :> seq<_>
                             internationalDiagnostics
                             transportModeDiagnostics ]
                |> Seq.sortBy (fun diagnostic -> diagnostic.code, diagnostic.sourceObjectId)
                |> Seq.toArray
            let missingCoordinateCount =
                bundleDiagnostics
                |> Seq.filter (fun diagnostic -> diagnostic.code = "missing_stop_coordinates")
                |> Seq.length
            if missingCoordinateCount > 0 then
                Log.Warning("{MissingCoordinateCount} referenced stop places have unresolved coordinates and were serialized as 0,0",
                            missingCoordinateCount)
            writeDiagnostics (Path.Combine(temp, "diagnostics.json")) bundleDiagnostics
            progressCompleted "write-diagnostics" 1L (Some 1L) "files"
            Log.Information("Bundle phase: hashing payloads and creating manifest")
            started "hash-payloads" None "files"
            let files =
                fileEntries executionOptions.maximumWorkers
                            (fun count total ->
                                reportProgress executionOptions phaseTimer "hash-payloads" "running"
                                               count total "files" None (min 2 executionOptions.maximumWorkers))
                            temp parquetRows
            progressCompleted "hash-payloads" (int64 files.Length) (Some(int64 files.Length)) "files"
            writeManifest (Path.Combine(temp, "manifest.json")) descriptor converterVersion stopIdsCis
                          internationalPolicy filterResult.decisions transportModeRules
                          transportModeDecisions preparation.postPlan routingPbfPath
                          (executionOptions.postInferenceEvidencePath |> Option.orElse liveEvidenceTemporaryDirectory)
                          executionOptions.postInferencePolicyPath
                          diagnosticPostLabels batch feed files)
        Log.Information("Bundle phase: activating completed bundle")
        JdfPostInferencePolicy.PostInferencePhaseProbe.record "activation"
        Directory.Move(temp, outputFull)
        completed <- true
    finally
        replayEvidence
        |> Option.iter(fun (store,_,_) -> (store :> IDisposable).Dispose())
        routingGraph |> Option.iter (fun graph -> (graph :> IDisposable).Dispose())
        // If an earlier conversion phase failed while the concurrent graph
        // builder was still running, join it and release its temporary maps.
        // This prevents abandoned multi-gigabyte conversion-local artifacts.
        if not routingGraphTask.IsFaulted && not routingGraphTask.IsCanceled then
            try
                routingGraphTask.GetAwaiter().GetResult()
                |> Option.iter (fun graph -> (graph :> IDisposable).Dispose())
            with _ -> ()
        if not completed && Directory.Exists(temp) then Directory.Delete(temp, true)
        liveEvidenceTemporaryDirectory
        |> Option.iter(fun path -> if Directory.Exists(path) then Directory.Delete(path,true))
    BundleCompleted

let writeBundleWithPolicyAndMemory releaseStopTimesAfterMaterialization
                                   snapshotDescriptorPath converterVersion stopIdsCis
                                   internationalPolicy internationalOverrides
                                   inputPath outputPath =
    // Stop times are always streamed directly to GTFS. Keep the public wrapper
    // for source compatibility, but the old materialization switch no longer
    // changes bundle behavior.
    ignore releaseStopTimesAfterMaterialization
    writeBundleWithPolicyCore snapshotDescriptorPath converterVersion stopIdsCis
                              internationalPolicy internationalOverrides JdfToGtfs.emptyTransportModeRules
                              true None false defaultBundleExecutionOptions
                              inputPath outputPath |> ignore

let writeBundleWithPolicy snapshotDescriptorPath converterVersion stopIdsCis
                          internationalPolicy internationalOverrides inputPath outputPath =
    writeBundleWithPolicyCore snapshotDescriptorPath converterVersion stopIdsCis
                              internationalPolicy internationalOverrides JdfToGtfs.emptyTransportModeRules
                              true None false defaultBundleExecutionOptions
                              inputPath outputPath |> ignore

let writeBundleWithPolicyAndRules snapshotDescriptorPath converterVersion stopIdsCis
                                  internationalPolicy internationalOverrides transportModeRules
                                  inputPath outputPath =
    writeBundleWithPolicyCore snapshotDescriptorPath converterVersion stopIdsCis
                              internationalPolicy internationalOverrides transportModeRules
                              true None false defaultBundleExecutionOptions
                              inputPath outputPath |> ignore

let writeBundleWithPolicyAndRulesEstimatedPosts snapshotDescriptorPath converterVersion stopIdsCis
                                                internationalPolicy internationalOverrides transportModeRules
                                                estimatedPosts inputPath outputPath =
    writeBundleWithPolicyCore snapshotDescriptorPath converterVersion stopIdsCis
                              internationalPolicy internationalOverrides transportModeRules
                              estimatedPosts None false defaultBundleExecutionOptions inputPath outputPath |> ignore

let writeBundleWithRoutedPostInference snapshotDescriptorPath converterVersion stopIdsCis
                                       internationalPolicy internationalOverrides transportModeRules
                                       estimatedPosts routingPbfPath diagnosticPostLabels inputPath outputPath =
    writeBundleWithPolicyCore snapshotDescriptorPath converterVersion stopIdsCis
                              internationalPolicy internationalOverrides transportModeRules
                              estimatedPosts routingPbfPath diagnosticPostLabels
                              defaultBundleExecutionOptions inputPath outputPath |> ignore

let executeBundleWithRoutedPostInferenceOptions snapshotDescriptorPath converterVersion stopIdsCis
                                              internationalPolicy internationalOverrides transportModeRules
                                              estimatedPosts routingPbfPath diagnosticPostLabels
                                              executionOptions inputPath outputPath =
    if executionOptions.maximumWorkers <= 0 then
        invalidArg "executionOptions" "Bundle maximum workers must be positive"
    if executionOptions.memoryBudgetBytes <= 0L then
        invalidArg "executionOptions" "Bundle memory budget must be positive"
    let executionMode =
        match executionOptions.postInferenceEvidenceOnly,
              executionOptions.postInferenceEvidencePath,routingPbfPath,estimatedPosts with
        | true,None,Some _,true -> CaptureOnly
        | false,Some _,None,true -> Replay
        | false,None,Some _,true -> Live
        | false,None,None,_ -> Disabled
        | _ -> invalidArg "executionOptions" "Invalid post-inference execution-mode combination"
    match executionMode with
    | CaptureOnly ->
        if executionOptions.postInferencePolicyPath.IsSome then
            invalidArg "executionOptions" "Capture-only execution cannot load a policy"
        let evidencePath =
            executionOptions.capturePostInferenceEvidencePath
            |> Option.defaultWith(fun () ->
                invalidArg "executionOptions"
                    "Capture-only execution requires --capture-post-inference-evidence=DIR")
        capturePostInferenceEvidenceOnly snapshotDescriptorPath converterVersion stopIdsCis
            internationalPolicy internationalOverrides transportModeRules routingPbfPath.Value
            executionOptions inputPath evidencePath
    | _ ->
        if executionOptions.capturePostInferenceEvidencePath.IsSome then
            invalidArg "executionOptions"
                "--capture-post-inference-evidence is valid only in capture-only execution"
        writeBundleWithPolicyCore snapshotDescriptorPath converterVersion stopIdsCis
                                  internationalPolicy internationalOverrides transportModeRules
                                  estimatedPosts routingPbfPath diagnosticPostLabels
                                  executionOptions inputPath outputPath

let writeBundleWithRoutedPostInferenceOptions snapshotDescriptorPath converterVersion stopIdsCis
                                              internationalPolicy internationalOverrides transportModeRules
                                              estimatedPosts routingPbfPath diagnosticPostLabels
                                              executionOptions inputPath outputPath =
    executeBundleWithRoutedPostInferenceOptions snapshotDescriptorPath converterVersion stopIdsCis
        internationalPolicy internationalOverrides transportModeRules estimatedPosts routingPbfPath
        diagnosticPostLabels executionOptions inputPath outputPath |> ignore

let writeBundle snapshotDescriptorPath converterVersion stopIdsCis inputPath outputPath =
    writeBundleWithPolicy snapshotDescriptorPath converterVersion stopIdsCis
                          JdfToGtfs.KeepAll [||] inputPath outputPath
