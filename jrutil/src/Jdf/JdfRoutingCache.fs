// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Routed context evidence kept between runs. Each entry is keyed by the
/// routing key (stop, mode and every routed coordinate) and records the tiles
/// of the routing graph its computation read, with their digest. It is reused
/// only while the current graph has the same digest over those tiles, so a
/// hit returns exactly what routing would, even after the demand clip or the
/// OSM data changed elsewhere. Router code changes invalidate every entry.
/// The cache never affects output; an unreadable file is ignored.
module JrUtil.JdfRoutingCache

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Reflection
open System.Runtime.InteropServices
open System.Text
open System.Text.Json
open System.Threading
open Serilog

open JrUtil.GeoData.Osm

let private formatVersion = 2
let private filePrefix = "routing-contexts-"

/// Unused entries are kept this long, for services that do not run daily.
let DefaultRetentionDays = 45

/// The routing code and runtime. The build embeds a hash of the routing
/// sources, so unrelated JrUtil changes keep the cache.
let routerIdentity =
    let assembly = typeof<PackedRoutingGraph>.Assembly
    let sourceHash =
        assembly.GetCustomAttributes<AssemblyMetadataAttribute>()
        |> Seq.tryFind (fun attribute -> attribute.Key = "RoutingSourceSha256")
        |> Option.map _.Value
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        // Without the build hash, fall back to this exact build.
        |> Option.defaultWith (fun () -> assembly.ManifestModule.ModuleVersionId.ToString("N"))
    let version (value: Type) = string (value.Assembly.GetName().Version)
    String.Join("|", [|
        $"format-{formatVersion}"
        sourceHash
        version typeof<ProjNet.CoordinateSystems.Transformations.MathTransform>
        version typeof<OsmSharp.Node>
        version typeof<CompilationArgumentCountsAttribute>
        RuntimeInformation.FrameworkDescription
        string RuntimeInformation.ProcessArchitecture
    |])

/// A snap edge by OSM identity; edge indices differ between graphs.
type StableEdgeRef = { fromNode: int64; toNode: int64; way: int64; occurrence: int }

type CachedPayload = {
    evidence: RoutedContextEvidence
    /// Per variant and route point, the snap edge of the attachment.
    snapEdges: StableEdgeRef option array array
}

type private Entry = {
    minTileX: int; minTileY: int; maxTileX: int; maxTileY: int
    digest: string
    lastUsedDay: int
    payload: byte array
}

let private today () = int (DateTime.UtcNow.Date - DateTime.UnixEpoch).TotalDays

type RoutingContextCache(directory: string, graph: PackedRoutingGraph, retentionDays: int) =
    let identity = Hashing.sha256Text routerIdentity
    let path = Path.Combine(directory, filePrefix + identity + ".bin")
    let stored = Dictionary<string, Entry>(StringComparer.Ordinal)
    let used = ConcurrentDictionary<string, Entry>(StringComparer.Ordinal)
    let day = today ()
    let mutable hits = 0L
    let mutable misses = 0L
    let mutable invalidated = 0L
    do
        Directory.CreateDirectory(directory) |> ignore
        if File.Exists(path) then
            try
                use stream = File.OpenRead(path)
                use gzip = new GZipStream(stream, CompressionMode.Decompress)
                use reader = new BinaryReader(gzip, Encoding.UTF8)
                if reader.ReadInt32() <> formatVersion then raise (InvalidDataException("Unsupported routing cache format"))
                for _ in 1 .. reader.ReadInt32() do
                    let key = reader.ReadString()
                    let minTileX, minTileY = reader.ReadInt32(), reader.ReadInt32()
                    let maxTileX, maxTileY = reader.ReadInt32(), reader.ReadInt32()
                    let digest = reader.ReadString()
                    let lastUsedDay = reader.ReadInt32()
                    let payload = reader.ReadBytes(reader.ReadInt32())
                    stored.[key] <- {
                        minTileX = minTileX; minTileY = minTileY; maxTileX = maxTileX; maxTileY = maxTileY
                        digest = digest; lastUsedDay = lastUsedDay; payload = payload }
            with error ->
                Log.Warning(error, "Ignoring unreadable routing cache {RoutingCachePath}", path)
                stored.Clear()

    let tryRestore (entry: Entry) =
        if graph.RegionDigest(struct(entry.minTileX, entry.minTileY, entry.maxTileX, entry.maxTileY)) <> entry.digest then None
        else
            let cached = JsonSerializer.Deserialize<CachedPayload>(entry.payload)
            let mutable resolved = true
            let attachments =
                cached.evidence.attachments
                |> Array.mapi (fun variant values ->
                    values
                    |> Array.mapi (fun point attachment ->
                        match cached.snapEdges.[variant].[point] with
                        | Some edge ->
                            match graph.TryResolveEdge(struct(edge.fromNode, edge.toNode, edge.way, edge.occurrence)) with
                            | ValueSome index -> { attachment with snapEdgeId = Some index }
                            | ValueNone -> resolved <- false; attachment
                        | None -> attachment))
            if resolved then Some { cached.evidence with attachments = attachments } else None

    let store (evidence: RoutedContextEvidence) (box: RoutingReadBox) =
        let struct(minTileX, minTileY, maxTileX, maxTileY) as tiles = graph.RegionTiles(box)
        let snapEdges =
            evidence.attachments
            |> Array.map (Array.map (fun attachment ->
                attachment.snapEdgeId
                |> Option.map (fun index ->
                    let struct(fromNode, toNode, way, occurrence) = graph.StableEdge(index)
                    { fromNode = fromNode; toNode = toNode; way = way; occurrence = occurrence })))
        let stripped =
            { evidence with
                attachments = evidence.attachments |> Array.map (Array.map (fun value -> { value with snapEdgeId = None })) }
        { minTileX = minTileX; minTileY = minTileY; maxTileX = maxTileX; maxTileY = maxTileY
          digest = graph.RegionDigest(tiles); lastUsedDay = day
          payload = JsonSerializer.SerializeToUtf8Bytes({ evidence = stripped; snapEdges = snapEdges }) }

    new(directory, graph) = RoutingContextCache(directory, graph, DefaultRetentionDays)

    member _.Path = path
    member _.StoredEntries = stored.Count
    member _.Hits = Interlocked.Read(&hits)
    member _.Misses = Interlocked.Read(&misses)
    /// Entries found but no longer valid for the current graph.
    member _.Invalidated = Interlocked.Read(&invalidated)

    /// The cached evidence for a routing key if its graph region is
    /// unchanged; otherwise the routed evidence, which is remembered. Safe to
    /// call from several workers.
    member _.GetOrRoute(key: string, route: unit -> struct(RoutedContextEvidence * RoutingReadBox)) =
        let restored =
            match stored.TryGetValue(key) with
            | true, entry ->
                match tryRestore entry with
                | Some evidence ->
                    used.[key] <- { entry with lastUsedDay = day }
                    Some evidence
                | None ->
                    Interlocked.Increment(&invalidated) |> ignore
                    None
            | _ -> None
        match restored with
        | Some evidence ->
            Interlocked.Increment(&hits) |> ignore
            evidence
        | None ->
            Interlocked.Increment(&misses) |> ignore
            let struct(evidence, box) = route ()
            used.[key] <- store evidence box
            evidence

    /// Write the entries used this run and those unused for at most the
    /// retention period, and remove caches of other router builds.
    member _.Save() =
        let retained = Dictionary<string, Entry>(used, StringComparer.Ordinal)
        for KeyValue(key, entry) in stored do
            if not (retained.ContainsKey(key)) && day - entry.lastUsedDay <= retentionDays then
                retained.[key] <- entry
        let temporary = path + ".tmp"
        do
            use stream = File.Create(temporary)
            use gzip = new GZipStream(stream, CompressionLevel.Fastest)
            use writer = new BinaryWriter(gzip, Encoding.UTF8)
            let keys = retained.Keys |> Seq.toArray
            Array.Sort(keys, StringComparer.Ordinal)
            writer.Write(formatVersion)
            writer.Write(keys.Length)
            for key in keys do
                let entry = retained.[key]
                writer.Write(key)
                writer.Write(entry.minTileX); writer.Write(entry.minTileY)
                writer.Write(entry.maxTileX); writer.Write(entry.maxTileY)
                writer.Write(entry.digest)
                writer.Write(entry.lastUsedDay)
                writer.Write(entry.payload.Length)
                writer.Write(entry.payload)
        File.Move(temporary, path, true)
        for stale in Directory.EnumerateFiles(directory, filePrefix + "*.bin") do
            if not (String.Equals(Path.GetFullPath(stale), Path.GetFullPath(path), StringComparison.OrdinalIgnoreCase)) then
                try File.Delete(stale)
                with error -> Log.Warning(error, "Could not remove stale routing cache {RoutingCachePath}", stale)
        Log.Information(
            "Routing cache: {Hits} reused, {Invalidated} invalidated by graph changes, {Misses} routed, {Entries} entries saved to {RoutingCachePath}",
            Interlocked.Read(&hits), Interlocked.Read(&invalidated), Interlocked.Read(&misses), retained.Count, path)
