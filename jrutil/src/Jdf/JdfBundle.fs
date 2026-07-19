// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfBundle

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open NodaTime
open Parquet
open Parquet.Schema
open Parquet.Serialization

open JrUtil

[<Literal>]
let BundleVersion = 1

[<Literal>]
let ParquetSchemaVersion = 1

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

let private restrictionGroupCode = function
    | JdfModel.TravelExclusion0 -> Some "§"
    | JdfModel.TravelExclusion1 -> Some "A"
    | JdfModel.TravelExclusion2 -> Some "B"
    | JdfModel.TravelExclusion3 -> Some "C"
    | _ -> None

let private restrictionGroups batch attributes =
    Jdf.parseAttributes batch attributes
    |> Seq.choose restrictionGroupCode

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

// Parquet is deliberately limited to source facts that cannot be reconstructed
// from standard GTFS plus the Oběhy extension tables. Snapshot identity belongs
// in file metadata and manifest.json rather than being repeated on every row.
let private getTables stopIdsCis (batch: JdfModel.JdfBatch) (feed: GtfsModel.GtfsFeed) =
    let stopLocations =
        batch.stopLocations
        |> Seq.filter (fun location -> location.precision = JdfModel.StopPrecise)
        |> Seq.map (fun location -> location.stopId, location)
        |> Map
    let retainedTripIds = feed.trips |> Seq.map (fun trip -> trip.id) |> Set
    let retainedRouteIds = feed.routes |> Seq.map (fun route -> route.id) |> Set
    let gtfsStopTimes = feed.stopTimes |> Seq.groupBy (fun call -> call.tripId) |> Map

    let routes =
        batch.routes
        |> Array.sortBy (fun route -> route.id, route.idDistinction)
        |> Array.map (fun route -> row [
            "gtfs_route_id", box (JdfToGtfs.jdfRouteId route.id route.idDistinction)
            "source_route_id", box (JdfToGtfs.jdfSourceRouteId route.id route.idDistinction)
            "route_distinction", box route.idDistinction
            "source_agency_id", box route.agencyId
            "source_agency_distinction", box route.agencyDistinction
            "valid_from", box (localDateString route.timetableValidFrom)
            "valid_to", box (localDateString route.timetableValidTo) ])

    let stopPlaces =
        batch.stops
        |> Array.sortBy (fun stop -> stop.id)
        |> Array.map (fun stop ->
            let location = stopLocations |> Map.tryFind stop.id
            row [
                "gtfs_stop_id", box (JdfToGtfs.jdfStopId stopIdsCis stop.id)
                "town", box stop.town
                "district", nullableObj stop.district
                "nearby_place", nullableObj stop.nearbyPlace
                "country", nullableObj stop.country
                "coordinates_missing", box location.IsNone ])

    let calls =
        batch.tripStops
        |> Seq.groupBy (fun call -> call.routeId, call.routeDistinction, call.tripId)
        |> Seq.collect (fun ((routeId, distinction, tripId), sourceCalls) ->
            let gtfsId = JdfToGtfs.jdfTripId routeId distinction tripId
            if not (retainedTripIds.Contains gtfsId) then Seq.empty else
            let direction = if Jdf.tripIsReverse tripId then -1L else 1L
            let emittedSourceCalls =
                sourceCalls
                |> Seq.sortBy (fun call -> call.routeStopId * direction)
                |> Seq.filter (fun call ->
                    match call.departureTime with
                    | Some JdfModel.Passing | Some JdfModel.NotPassing -> false
                    | None when call.arrivalTime = None -> false
                    | _ -> true)
                |> Seq.toArray
            let emittedGtfsCalls = gtfsStopTimes.[gtfsId] |> Seq.sortBy (fun call -> call.stopSequence) |> Seq.toArray
            if emittedSourceCalls.Length <> emittedGtfsCalls.Length then
                failwith $"Source/GTFS call count mismatch for {gtfsId}"
            Seq.zip emittedSourceCalls emittedGtfsCalls
            |> Seq.map (fun (sourceCall, gtfsCall) ->
                row [
                    "gtfs_trip_id", box gtfsId
                    "stop_sequence", box gtfsCall.stopSequence
                    "source_route_stop_id", box sourceCall.routeStopId ]))
        |> Seq.sortBy (fun value -> string value.["gtfs_trip_id"], unbox<int> value.["stop_sequence"])
        |> Seq.toArray

    let routeStopZones =
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

    let emittedCallKeys =
        calls
        |> Seq.map (fun value ->
            (string value.["gtfs_trip_id"], unbox<int64> value.["source_route_stop_id"]), ())
        |> Map

    let notices =
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

    let transfers =
        batch.transfers
        |> withOwnerOrdinals (fun transfer -> transfer.routeId, transfer.routeDistinction, transfer.tripId)
        |> Seq.choose (fun (ordinal, transfer) ->
            let gtfsTripId =
                JdfToGtfs.jdfTripId transfer.routeId transfer.routeDistinction transfer.tripId
            if not (retainedTripIds.Contains gtfsTripId)
               || not (emittedCallKeys.ContainsKey (gtfsTripId, transfer.routeStopId)) then None
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

    let restrictions =
        let routeStopAssignments =
            batch.routeStops
            |> Seq.collect (fun routeStop ->
                let gtfsRouteId =
                    JdfToGtfs.jdfRouteId routeStop.routeId routeStop.routeDistinction
                if not (retainedRouteIds.Contains gtfsRouteId) then Seq.empty else
                restrictionGroups batch routeStop.attributes
                |> Seq.map (fun groupCode -> row [
                    "assignment_scope", box "route_stop"
                    "gtfs_route_id", box gtfsRouteId
                    "gtfs_trip_id", null
                    "source_route_stop_id", box routeStop.routeStopId
                    "group_code", box groupCode ]))
        let tripCallAssignments =
            batch.tripStops
            |> Seq.collect (fun tripStop ->
                let gtfsTripId =
                    JdfToGtfs.jdfTripId tripStop.routeId tripStop.routeDistinction tripStop.tripId
                if not (retainedTripIds.Contains gtfsTripId)
                   || not (emittedCallKeys.ContainsKey (gtfsTripId, tripStop.routeStopId)) then Seq.empty
                else restrictionGroups batch tripStop.attributes
                     |> Seq.map (fun groupCode -> row [
                        "assignment_scope", box "trip_call"
                        "gtfs_route_id", null
                        "gtfs_trip_id", box gtfsTripId
                        "source_route_stop_id", box tripStop.routeStopId
                        "group_code", box groupCode ]))
        Seq.append routeStopAssignments tripCallAssignments
        |> Seq.distinctBy (fun value ->
            value.["assignment_scope"], value.["gtfs_route_id"], value.["gtfs_trip_id"],
            value.["source_route_stop_id"], value.["group_code"])
        |> Seq.sortBy (fun value ->
            string value.["assignment_scope"], string value.["gtfs_route_id"],
            string value.["gtfs_trip_id"], unbox<int64> value.["source_route_stop_id"],
            string value.["group_code"])
        |> Seq.toArray

    let stringField name nullable = field<string> name nullable
    let intField name nullable = field<int> name nullable
    let int64Field name nullable = field<int64> name nullable
    let boolField name nullable = field<bool> name nullable
    [|
        "source_route_metadata.parquet", table [|
            stringField "gtfs_route_id" false; stringField "source_route_id" false
            intField "route_distinction" false; stringField "source_agency_id" false
            intField "source_agency_distinction" false; stringField "valid_from" false
            stringField "valid_to" false |] routes
        "source_stop_metadata.parquet", table [|
            stringField "gtfs_stop_id" false; stringField "town" false
            stringField "district" true; stringField "nearby_place" true
            stringField "country" true; boolField "coordinates_missing" false |] stopPlaces
        "source_call_metadata.parquet", table [|
            stringField "gtfs_trip_id" false; intField "stop_sequence" false
            int64Field "source_route_stop_id" false |] calls
        "source_route_stop_zone_metadata.parquet", table [|
            stringField "gtfs_route_id" false; int64Field "source_route_stop_id" false
            stringField "zone_id" false; intField "zone_order" false |] routeStopZones
        "source_notice_metadata.parquet", table [|
            stringField "source_notice_id" false; stringField "notice_kind" false
            stringField "gtfs_route_id" true; stringField "gtfs_trip_id" true
            stringField "label" true; stringField "text" true
            stringField "valid_from" true; stringField "valid_to" true
            stringField "service_note_type" true |] notices
        "source_transfer_metadata.parquet", table [|
            stringField "source_transfer_id" false; stringField "gtfs_trip_id" false
            int64Field "source_route_stop_id" false; stringField "transfer_type" false
            int64Field "transfer_route_id" true; int64Field "transfer_stop_id" true
            int64Field "transfer_stop_post_id" true; int64Field "transfer_end_stop_id" true
            int64Field "transfer_end_stop_post_id" true; intField "wait_minutes" true
            stringField "note" true |] transfers
        "source_travel_restriction_metadata.parquet", table [|
            stringField "assignment_scope" false; stringField "gtfs_route_id" true
            stringField "gtfs_trip_id" true; int64Field "source_route_stop_id" false
            stringField "group_code" false |] restrictions
    |]

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

let private diagnostics (batch: JdfModel.JdfBatch) (feed: GtfsModel.GtfsFeed) =
    let retainedTrips = feed.trips |> Seq.map (fun trip -> trip.id) |> Set
    let emittedCallKeys =
        batch.tripStops
        |> Seq.choose (fun call ->
            let gtfsTripId = JdfToGtfs.jdfTripId call.routeId call.routeDistinction call.tripId
            if not (retainedTrips.Contains gtfsTripId) then None else
            match call.departureTime with
            | Some JdfModel.Passing | Some JdfModel.NotPassing -> None
            | None when call.arrivalTime = None -> None
            | _ -> Some (gtfsTripId, call.routeStopId))
        |> Set
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
        let restrictions =
            batch.tripStops
            |> Seq.filter (fun call -> restrictionGroups batch call.attributes |> Seq.isEmpty |> not)
            |> Seq.choose (fun call ->
                let gtfsTripId = JdfToGtfs.jdfTripId call.routeId call.routeDistinction call.tripId
                if not (retainedTrips.Contains gtfsTripId) then
                    Some (restrictionId call.routeId call.routeDistinction call.tripId call.routeStopId)
                else None)
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
                   && not (emittedCallKeys.Contains (gtfsTripId, transfer.routeStopId)) then
                    Some (transferId transfer.routeId transfer.routeDistinction transfer.tripId ordinal)
                else None)
        let restrictions =
            batch.tripStops
            |> Seq.filter (fun call -> restrictionGroups batch call.attributes |> Seq.isEmpty |> not)
            |> Seq.choose (fun call ->
                let gtfsTripId = JdfToGtfs.jdfTripId call.routeId call.routeDistinction call.tripId
                if retainedTrips.Contains gtfsTripId
                   && not (emittedCallKeys.Contains (gtfsTripId, call.routeStopId)) then
                    Some (restrictionId call.routeId call.routeDistinction call.tripId call.routeStopId)
                else None)
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
    let singletonRestrictions =
        let routeStops =
            batch.routeStops
            |> Seq.map (fun stop -> (stop.routeId, stop.routeDistinction, stop.routeStopId), stop)
            |> Map
        batch.tripStops
        |> Seq.groupBy (fun call -> call.routeId, call.routeDistinction, call.tripId)
        |> Seq.collect (fun ((routeId, distinction, tripId), calls) ->
            let gtfsTripId = JdfToGtfs.jdfTripId routeId distinction tripId
            if not (retainedTrips.Contains gtfsTripId) then Seq.empty else
            calls
            |> Seq.filter (fun call -> emittedCallKeys.Contains (gtfsTripId, call.routeStopId))
            |> Seq.collect (fun call ->
                let routeStop = routeStops.[routeId, distinction, call.routeStopId]
                Seq.append (restrictionGroups batch routeStop.attributes)
                           (restrictionGroups batch call.attributes)
                |> Seq.distinct
                |> Seq.map (fun groupCode -> groupCode, call.routeStopId))
            |> Seq.groupBy fst
            |> Seq.choose (fun (groupCode, members) ->
                let count = members |> Seq.map snd |> Seq.distinct |> Seq.length
                if count = 1 then Some {
                    severity = "warning"; code = "singleton_travel_restriction"
                    sourceObjectId = $"{gtfsTripId}:restriction-group:{sourceSegment groupCode}"
                    message = "Effective travel-exclusion group has only one emitted call"
                } else None))
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
    let conflictingPosts =
        batch.tripStops
        |> Seq.choose (fun call ->
            match call.stopPostId,
                  call.stopPostNum |> Option.bind JdfToGtfs.nonEmptyTrimmed with
            | Some postId, Some postNum -> Some ((call.stopId, postId), postNum)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.choose (fun ((stopId, postId), values) ->
            let numbers = values |> Seq.map snd |> Seq.distinct |> Seq.toArray
            if numbers.Length <= 1 then None else
                let joinedNumbers = String.Join(",", numbers)
                Some {
                severity = "warning"; code = "conflicting_post_numbers"
                sourceObjectId = $"jdf:stop:{stopId}:post:id:{postId}"
                message = $"Authoritative post has conflicting display numbers: {joinedNumbers}"
                })
    Seq.concat [filteredTrips; filteredEnrichments; unjoinableCallEnrichments; blankNotices
                singletonRestrictions; conflictingRouteStops; missingLines; multiZones; conflictingPosts]
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

let private fileEntries root parquetRows =
    Directory.GetFiles(root, "*", SearchOption.AllDirectories)
    |> Array.filter (fun path -> not (Path.GetFileName(path).Equals("manifest.json", StringComparison.Ordinal)))
    |> Array.map (fun path ->
        let relative = Path.GetRelativePath(root, path).Replace('\\', '/')
        let rows =
            match parquetRows |> Map.tryFind relative with
            | Some count -> Some count
            | None when Path.GetExtension(path).Equals(".txt", StringComparison.OrdinalIgnoreCase) -> Some (countTextRows path)
            | _ -> None
        { path = relative; sha256 = fileSha256 path; bytes = FileInfo(path).Length; rows = rows })
    |> Array.sortBy (fun entry -> entry.path)

let private writeManifest path descriptor (converterVersion: string) stopIdsCis
                          (batch: JdfModel.JdfBatch) files =
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

let writeBundle snapshotDescriptorPath converterVersion stopIdsCis inputPath outputPath =
    if String.IsNullOrWhiteSpace(converterVersion) then invalidArg "converterVersion" "Converter version is required"
    let descriptor = loadSnapshotDescriptor snapshotDescriptorPath
    validateSnapshot descriptor inputPath
    let outputFull = Path.GetFullPath(outputPath)
    if Directory.Exists(outputFull) || File.Exists(outputFull) then
        invalidArg "outputPath" $"Bundle output already exists: {outputFull}"
    let parent = Path.GetDirectoryName(outputFull)
    if String.IsNullOrEmpty(parent) then invalidArg "outputPath" "Bundle output requires a parent directory"
    Directory.CreateDirectory(parent) |> ignore
    let temp = Path.Combine(parent, $".{Path.GetFileName(outputFull)}.tmp-{Guid.NewGuid():N}")
    Directory.CreateDirectory(temp) |> ignore
    let mutable completed = false
    try
        withJdfInput inputPath (fun source ->
            let batch = Jdf.jdfBatchDirParser () source
            let feed =
                JdfToGtfs.getGtfsFeedForBundle stopIdsCis batch
                |> Gtfs.deduplicateCalendar
                |> Gtfs.fillStandardRequiredFields
            let gtfsPath = Path.Combine(temp, "gtfs-intermediate")
            let extensionsPath = Path.Combine(temp, "extensions")
            Gtfs.gtfsStandardTablesToFolder () gtfsPath feed
            Gtfs.gtfsExtensionsToFolder () extensionsPath feed
            let tables = getTables stopIdsCis batch feed
            let mutable parquetRows = Map.empty
            for name, parquetTable in tables do
                writeParquet descriptor (Path.Combine(temp, name)) parquetTable
                parquetRows <- parquetRows |> Map.add name parquetTable.rows.Count
            let bundleDiagnostics = diagnostics batch feed
            writeDiagnostics (Path.Combine(temp, "diagnostics.json")) bundleDiagnostics
            let files = fileEntries temp parquetRows
            writeManifest (Path.Combine(temp, "manifest.json")) descriptor converterVersion stopIdsCis batch files)
        Directory.Move(temp, outputFull)
        completed <- true
    finally
        if not completed && Directory.Exists(temp) then Directory.Delete(temp, true)
