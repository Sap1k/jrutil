// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Snapshot descriptors and JDF input (directory or ZIP) validation.
module JrUtil.JdfBundleInput

open System
open JrUtil.Hashing
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
open JrUtil.JdfBundleModel

let internal requiredString (root: JsonElement) (name: string) =
    let mutable value = Unchecked.defaultof<JsonElement>
    if not (root.TryGetProperty(name, &value))
       || value.ValueKind <> JsonValueKind.String
       || String.IsNullOrWhiteSpace(value.GetString()) then
        invalidArg "snapshotDescriptor" $"Snapshot descriptor field '{name}' is required"
    value.GetString()

let internal optionalString (root: JsonElement) (name: string) =
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

let internal validateSnapshot descriptor inputPath =
    let actualKind, actualHash, actualBytes =
        if Directory.Exists(inputPath) then
            let hash, bytes = directoryTreeIdentity inputPath
            "directory-tree", hash, bytes
        elif File.Exists(inputPath) && Path.GetExtension(inputPath).Equals(".zip", StringComparison.OrdinalIgnoreCase) then
            "zip", sha256File inputPath, FileInfo(inputPath).Length
        else invalidArg "inputPath" "JDF input must be a directory or ZIP file"
    if descriptor.payloadKind <> actualKind then
        invalidArg "snapshotDescriptor" $"payload_kind is {descriptor.payloadKind}, input is {actualKind}"
    if descriptor.payloadSha256 <> actualHash then
        invalidArg "snapshotDescriptor" $"Payload SHA-256 mismatch: expected {descriptor.payloadSha256}, got {actualHash}"
    if descriptor.payloadBytes <> actualBytes then
        invalidArg "snapshotDescriptor" $"Payload byte-size mismatch: expected {descriptor.payloadBytes}, got {actualBytes}"

let internal validateZip (archive: ZipArchive) =
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

let internal jdfInputContainsRelation inputPath relationName =
    if Directory.Exists(inputPath) then
        Directory.EnumerateFiles(inputPath, "*", SearchOption.TopDirectoryOnly)
        |> Seq.exists (fun path ->
            Path.GetFileName(path).Equals(relationName, StringComparison.OrdinalIgnoreCase))
    else
        use archive = ZipFile.OpenRead(inputPath)
        archive.Entries
        |> Seq.exists (fun entry ->
            entry.Name.Equals(relationName, StringComparison.OrdinalIgnoreCase))

let internal withJdfInput inputPath action =
    if Directory.Exists(inputPath) then action (Jdf.FsPath inputPath)
    else
        use archive = ZipFile.OpenRead(inputPath)
        validateZip archive
        action (Jdf.ZipArchive archive)
