// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Reviewed, append-only stop identity registry kept in jrunify-ext-geodata
/// (`registry/`). It pins merged-JDF stop numbers, inferred post ordinals and
/// overlay-native stop places so their public IDs survive between exports.
module JrUtil.StopRegistry

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Security.Cryptography
open System.Text
open FSharp.Data

open JrUtil.Hashing

/// Stops the registry does not know are numbered from here; registered
/// numbers always stay below it.
let provisionalBase = 1_000_000_000L
let private provisionalSpan = 1_000_000_000L

/// Inferred posts within this distance of a registered post reuse its ordinal.
let inferredPostMatchMetres = 25.0

type RegistryStop = {
    id: int64
    town: string
    district: string option
    nearbyPlace: string option
    regionId: string option
    country: string option
    reference: (float * float) option
    retired: bool
    /// Target of a `merged_into:<id>` note; the row resolves to that number.
    mergedInto: int64 option
} with
    member this.resolvedId = this.mergedInto |> Option.defaultValue this.id

type RegistryPost = {
    stopId: int64
    /// Suffix after `jdf:stop:N:`, e.g. `post:3` or `est:2`.
    postKey: string
    reference: (float * float) option
    retired: bool
}

type RegistryOverlayPlace = {
    sourceId: string
    groupKey: string
    placeId: string
}

type StopRegistry = {
    stops: RegistryStop array
    posts: RegistryPost array
    overlayPlaces: RegistryOverlayPlace array
    /// SHA-256 over the per-file digests, for provenance logging.
    sha256: string
}

let stopsHeader = [| "id"; "town"; "district"; "nearby_place"; "okres"; "country"; "lat"; "lon"; "status"; "note" |]
let postsHeader = [| "stop_id"; "post_key"; "lat"; "lon"; "status"; "note" |]
let overlayPlacesHeader = [| "source_id"; "group_key"; "place_id"; "status"; "note" |]
let stopCandidatesHeader =
    [| "provisional_id"; "town"; "district"; "nearby_place"; "okres"; "country"; "lat"; "lon"; "reason"; "alias_of_suggestion" |]
let postCandidatesHeader = [| "stop_id"; "post_key"; "lat"; "lon"; "reason" |]
let overlayPlaceCandidatesHeader =
    [| "source_id"; "group_key"; "place_id"; "stop_name"; "lat"; "lon"; "reason"; "alias_of_suggestion" |]

let private optional (value: string) =
    let trimmed = value.Trim()
    if trimmed = "" then None else Some trimmed

let private invalid (file: string) (line: int) message =
    invalidArg "stopRegistry" $"{file}:{line}: {message}"

let private parseStatus file line (value: string) =
    match value.Trim() with
    | "active" -> false
    | "retired" -> true
    | other -> invalid file line $"status must be active or retired, not '{other}'"

let private parseReference file line (lat: string) (lon: string) =
    match optional lat, optional lon with
    | None, None -> None
    | Some lat, Some lon ->
        match Double.TryParse(lat, NumberStyles.Float, CultureInfo.InvariantCulture),
              Double.TryParse(lon, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | (true, lat), (true, lon) when abs lat <= 90.0 && abs lon <= 180.0 -> Some (lat, lon)
        | _ -> invalid file line "lat/lon must be valid coordinates"
    | _ -> invalid file line "lat and lon must both be set or both be empty"

let private readTable (path: string) (header: string array) =
    let bytes = File.ReadAllBytes(path)
    let text = Encoding.UTF8.GetString(bytes).TrimStart('﻿')
    let csv = CsvFile.Parse(text, hasHeaders = true, quote = '"', ignoreErrors = false)
    if csv.Headers <> Some header then
        let expected = String.Join(",", header)
        invalidArg "stopRegistry" $"{Path.GetFileName(path)} must have the header {expected}"
    let rows = csv.Rows |> Seq.map (fun row -> row.Columns) |> Seq.toArray
    rows, sha256Bytes bytes

let private parseStops (path: string) =
    let file = Path.GetFileName(path)
    let rows, digest = readTable path stopsHeader
    let stops =
        rows
        |> Array.mapi (fun index row ->
            let line = index + 2
            let id =
                match Int64.TryParse(row.[0].Trim()) with
                | true, value when value > 0L && value < provisionalBase -> value
                | _ -> invalid file line $"id must be an integer in 1..{provisionalBase - 1L}"
            if String.IsNullOrWhiteSpace(row.[1]) then invalid file line "town is required"
            let mergedInto =
                let note = row.[9]
                let marker = "merged_into:"
                match note.IndexOf(marker, StringComparison.Ordinal) with
                | -1 -> None
                | start ->
                    let digits =
                        note.Substring(start + marker.Length)
                        |> Seq.takeWhile Char.IsDigit
                        |> Seq.toArray
                        |> String
                    match Int64.TryParse(digits) with
                    | true, value when value <> id -> Some value
                    | _ -> invalid file line "merged_into must name another id"
            {
                id = id
                town = row.[1].Trim()
                district = optional row.[2]
                nearbyPlace = optional row.[3]
                regionId = optional row.[4] |> Option.map (fun value -> value.ToUpperInvariant())
                country = optional row.[5] |> Option.map (fun value -> value.ToUpperInvariant())
                reference = parseReference file line row.[6] row.[7]
                retired = parseStatus file line row.[8]
                mergedInto = mergedInto
            })
    let ids = stops |> Array.map _.id |> HashSet
    for stop in stops do
        stop.mergedInto
        |> Option.iter (fun target ->
            if not (ids.Contains target) then
                invalidArg "stopRegistry" $"{file}: merged_into:{target} is not a registered id")
    let byId = stops |> Array.groupBy _.id |> dict
    for stop in stops do
        // Chains are not followed: a merge must name the surviving number.
        stop.mergedInto
        |> Option.iter (fun target ->
            if byId.[target] |> Array.exists (fun row -> row.mergedInto.IsSome) then
                invalidArg "stopRegistry" $"{file}: merged_into:{target} is itself merged")
    stops, digest

let isPostKey (value: string) =
    if value.StartsWith("est:", StringComparison.Ordinal) then
        match Int32.TryParse(value.Substring(4), NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, ordinal -> ordinal > 0 && string ordinal = value.Substring(4)
        | _ -> false
    else
        value.StartsWith("post:", StringComparison.Ordinal)
        && value.Length > 5
        && not (value.Substring(5) |> Seq.exists (fun c -> c = ':' || Char.IsWhiteSpace c))

let private parsePosts (path: string) (stopIds: HashSet<int64>) =
    let file = Path.GetFileName(path)
    let rows, digest = readTable path postsHeader
    let seen = HashSet<struct (int64 * string)>()
    let posts =
        rows
        |> Array.mapi (fun index row ->
            let line = index + 2
            let stopId =
                match Int64.TryParse(row.[0].Trim()) with
                | true, value when stopIds.Contains value -> value
                | _ -> invalid file line $"stop_id {row.[0]} is not registered"
            let postKey = row.[1].Trim()
            if not (isPostKey postKey) then invalid file line "post_key must be post:<num> or est:<k>"
            if not (seen.Add(struct (stopId, postKey))) then invalid file line $"duplicate post {stopId} {postKey}"
            let reference = parseReference file line row.[2] row.[3]
            if postKey.StartsWith("est:", StringComparison.Ordinal) && reference.IsNone then
                invalid file line "inferred posts need reference coordinates"
            { stopId = stopId; postKey = postKey; reference = reference; retired = parseStatus file line row.[4] })
    posts, digest

let private parseOverlayPlaces (path: string) =
    let file = Path.GetFileName(path)
    let rows, digest = readTable path overlayPlacesHeader
    let seen = HashSet<struct (string * string)>()
    let places =
        rows
        |> Array.mapi (fun index row ->
            let line = index + 2
            let sourceId, groupKey, placeId = row.[0].Trim(), row.[1], row.[2].Trim()
            if sourceId = "" || groupKey = "" || placeId = "" then
                invalid file line "source_id, group_key and place_id are required"
            if not (seen.Add(struct (sourceId, groupKey))) then
                invalid file line $"duplicate group {sourceId} {groupKey}"
            parseStatus file line row.[3] |> ignore
            { sourceId = sourceId; groupKey = groupKey; placeId = placeId })
    places, digest

/// Load `stops.csv` and the optional `posts.csv` and `overlay_places.csv`
/// from a registry directory.
let load (directory: string) =
    let path name = Path.Combine(directory, name)
    if not (File.Exists(path "stops.csv")) then
        invalidArg "stopRegistry" $"Stop registry {directory} has no stops.csv"
    let stops, stopsDigest = parseStops (path "stops.csv")
    let stopIds = stops |> Array.map _.id |> HashSet
    let posts, postsDigest =
        if File.Exists(path "posts.csv") then parsePosts (path "posts.csv") stopIds else [||], ""
    let places, placesDigest =
        if File.Exists(path "overlay_places.csv") then parseOverlayPlaces (path "overlay_places.csv") else [||], ""
    {
        stops = stops
        posts = posts
        overlayPlaces = places
        sha256 = sha256Text $"stops={stopsDigest};posts={postsDigest};overlay_places={placesDigest}"
    }

/// Deterministic number for an unregistered stop. It depends only on the
/// identity, so it is the same whatever order batches are merged in; a
/// collision probes upwards in the provisional range.
let provisionalStopId (identity: string) (isTaken: int64 -> bool) =
    let digest = SHA256.HashData(Encoding.UTF8.GetBytes(identity))
    let value = BitConverter.ToUInt64(digest, 0) % uint64 provisionalSpan
    let rec probe offset =
        if offset >= provisionalSpan then invalidOp "Provisional stop ID range is exhausted"
        let candidate = provisionalBase + (int64 value + offset) % provisionalSpan
        if isTaken candidate then probe (offset + 1L) else candidate
    probe 0L

let distanceMetres (lat: float, lon: float) (otherLat: float, otherLon: float) =
    let middle = (lat + otherLat) / 2.0 * Math.PI / 180.0
    let north = (lat - otherLat) * 111_320.0
    let east = (lon - otherLon) * 111_320.0 * cos middle
    sqrt (north * north + east * east)

let private estOrdinal (postKey: string) =
    if postKey.StartsWith("est:", StringComparison.Ordinal) then Some (int (postKey.Substring(4))) else None

/// Assign `est:<k>` ordinals for one stop's inferred post locations
/// `(locationId, lat, lon)`. Each registered inferred post is reused by at
/// most one location within `inferredPostMatchMetres`, closest pairs first.
/// Remaining locations take the next ordinals above every registered one, in
/// location ID order, and are returned as new.
let assignInferredPostOrdinals (registered: RegistryPost array) (locations: (string * float * float) array) =
    let registeredEst =
        registered
        |> Array.choose (fun post ->
            match estOrdinal post.postKey, post.reference with
            | Some ordinal, Some point -> Some (ordinal, point)
            | _ -> None)
    let pairs =
        [| for (locationId, lat, lon) in locations do
             for (ordinal, point) in registeredEst do
                 let distance = distanceMetres (lat, lon) point
                 if distance <= inferredPostMatchMetres then
                     yield distance, ordinal, locationId |]
        |> Array.sortWith (fun (leftDistance, leftOrdinal, leftId) (rightDistance, rightOrdinal, rightId) ->
            match compare leftDistance rightDistance with
            | 0 ->
                match compare leftOrdinal rightOrdinal with
                | 0 -> StringComparer.Ordinal.Compare(leftId, rightId)
                | order -> order
            | order -> order)
    let assigned = Dictionary<string, int>(StringComparer.Ordinal)
    let usedOrdinals = HashSet<int>()
    for (_, ordinal, locationId) in pairs do
        if not (assigned.ContainsKey locationId) && usedOrdinals.Add(ordinal) then
            assigned.[locationId] <- ordinal
    let mutable next =
        registered
        |> Array.choose (fun post -> estOrdinal post.postKey)
        |> Array.fold max 0
    let fresh = ResizeArray()
    for (locationId, lat, lon) in locations |> Array.sortWith (fun (left, _, _) (right, _, _) -> StringComparer.Ordinal.Compare(left, right)) do
        if not (assigned.ContainsKey locationId) then
            next <- next + 1
            assigned.[locationId] <- next
            fresh.Add((locationId, next, lat, lon))
    assigned |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq, fresh.ToArray()

let private csvField (value: string) =
    if value.IndexOfAny([| ','; '"'; '\n'; '\r' |]) >= 0 then
        "\"" + value.Replace("\"", "\"\"") + "\""
    else value

/// Write a review CSV with LF line endings and RFC 4180 quoting.
let writeCsv (path: string) (header: string array) (rows: string array seq) =
    let directory = Path.GetDirectoryName(Path.GetFullPath(path))
    Directory.CreateDirectory(directory) |> ignore
    use writer = new StreamWriter(path, false, UTF8Encoding(false))
    writer.NewLine <- "\n"
    writer.WriteLine(String.Join(",", header |> Array.map csvField))
    for row in rows do
        writer.WriteLine(String.Join(",", row |> Array.map csvField))

let formatCoordinate (value: float) = value.ToString("0.000000", CultureInfo.InvariantCulture)
