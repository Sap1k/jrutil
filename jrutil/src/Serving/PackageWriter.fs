// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open JrUtil
open JrUtil.Hashing
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Runtime
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Parquet
open Parquet.Schema

open JrUtil.RegionalOverlay.GtfsFiles
open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.Model

open JrUtil.Serving.PackageRows
open JrUtil.Serving.PackageBaseRelations
open JrUtil.Serving.PackageGtfsRelations
open JrUtil.Serving.PackageKeyRelations
open JrUtil.Serving.PackageSemanticRelations

module PackageWriter =
    /// Zone facts the route-stop post-pass projects onto final calls.
    let private zoneInputs (input: CompilerOutput.Output) : RouteStopWriter.ZoneInputs =
        let zone code system : RouteStopWriter.Zone =
            { code = code; system = if String.IsNullOrWhiteSpace(system) then null else system }
        let collect (rows: seq<'key * RouteStopWriter.Zone>) (comparer: IEqualityComparer<'key>) =
            let result = Dictionary<'key, ResizeArray<RouteStopWriter.Zone>>(comparer)
            for key, value in rows do
                match result.TryGetValue(key) with
                | true, values -> if not (values.Contains(value)) then values.Add(value)
                | _ -> result.Add(key, ResizeArray([ value ]))
            let final = Dictionary<'key, RouteStopWriter.Zone array>(comparer)
            for KeyValue(key, values) in result do final.Add(key, values.ToArray())
            final :> IDictionary<_, _>
        let calls =
            CompilerOutput.rows input.czech "cz_trip_stop_zones.txt"
            |> Seq.filter (fun row -> not (String.IsNullOrWhiteSpace(value "zone_code" row)))
            |> Seq.map (fun row ->
                struct(value "trip_id" row, integer (value "stop_sequence" row)),
                zone (value "zone_code" row) (value "ids_system_id" row))
        let routeStops =
            if CompilerOutput.has input.sidecars "source_route_stop_zone_metadata" then
                CompilerOutput.values input.sidecars "source_route_stop_zone_metadata"
                    [| "gtfs_route_id"; "source_route_version"; "source_route_stop_id"; "zone_code"; "zone_order" |]
                |> Seq.sortBy (fun row -> row.[0], row.[1], row.[2], integer row.[4])
                |> Seq.map (fun row -> Identity.routeStopKey row.[0] row.[1] row.[2], zone row.[3] null)
            else Seq.empty
        let places =
            CompilerOutput.rows input.czech "cz_stop_zones.txt"
            |> Seq.filter (fun row ->
                String.IsNullOrWhiteSpace(value "route_id" row) && not (String.IsNullOrWhiteSpace(value "zone_code" row)))
            |> Seq.map (fun row -> value "stop_place_id" row, zone (value "zone_code" row) (value "ids_system_id" row))
        { calls = collect calls HashIdentity.Structural
          routeStops = collect routeStops StringComparer.Ordinal
          places = collect places StringComparer.Ordinal }

    let private writeDiagnosticsSummary (input: CompilerOutput.Output) output (routeStops: RouteStopWriter.Summary) =
        let events = ResizeArray<string * string * string * string>()
        match input.diagnostics with
        | None -> ()
        | Some root ->
            match root.TryGetProperty("diagnostics") with
            | true, values when values.ValueKind = JsonValueKind.Array ->
                for item in values.EnumerateArray() do
                    let property (name: string) (fallback: string) =
                        match item.TryGetProperty(name) with
                        | true, value when value.ValueKind = JsonValueKind.String -> value.GetString()
                        | _ -> fallback
                    events.Add(property "severity" "warning", property "code" "unknown", property "source_object_id" "", property "message" "")
            | _ -> ()
        for row in CompilerOutput.rows input.reports "diagnostics.csv" do
            events.Add("warning", value "code" row, value "source_object_id" row, value "message" row)
        let counts =
            events |> Seq.countBy (fun (severity, code, _, _) -> severity, code)
            |> Seq.sortBy (fun ((severity, code), _) -> code, severity)
            |> Seq.map (fun ((severity, code), count) -> dict [ "severity", box severity; "code", box code; "count", box count ])
            |> Seq.toArray
        let examples =
            events |> Seq.groupBy (fun (_, code, _, _) -> code) |> Seq.sortBy fst
            |> Seq.map (fun (code, values) ->
                code, box (values |> Seq.sortBy (fun (_, _, sourceObject, message) -> sourceObject, message) |> Seq.truncate 10
                           |> Seq.map (fun (severity, _, sourceObject, message) -> dict [ "severity", box severity; "source_object_id", box sourceObject; "message", box message ]) |> Seq.toArray))
            |> dict
        let coverage = Dictionary<string,obj>(StringComparer.Ordinal)
        for fileName in [| "coverage.csv"; "coverage_by_mode.csv"; "coverage_by_tier.csv"; "trip_coverage_populations.csv"; "trip_coverage_populations_by_mode.csv"; "snapshot_day_coverage.csv"; "exclusions.csv" |] do
            let rows =
                CompilerOutput.rows input.reports fileName
                |> Seq.map (fun (row: CsvRow) ->
                    row |> Seq.map (fun pair -> pair.Key, box pair.Value) |> dict :> obj)
                |> Seq.toArray
            if rows.Length > 0 then coverage.[Path.GetFileNameWithoutExtension(fileName)] <- box rows
        let diagnostics = dict [
            "schema_version", box Schema.DiagnosticsSchemaVersion
            "counts_by_code", box counts
            "examples_by_code", box examples
            "coverage_populations", box coverage
            "route_stop_order", box (dict [
                "route_directions", box routeStops.routeDirections; "patterns", box routeStops.patterns
                "slots", box routeStops.slots; "maximum_patterns_per_route_direction", box routeStops.maximumPatterns
                "route_stop_zones", box routeStops.routeStopZones; "call_zones", box routeStops.callZones
                "unmatched_call_zone_calls", box routeStops.unmatchedCallZones ]) ]
        File.WriteAllText(Path.Combine(output, "diagnostics.json"), JsonSerializer.Serialize(diagnostics, JsonSerializerOptions(WriteIndented = true)) + "\n", new UTF8Encoding(false))

    let private writeManifest (input: CompilerOutput.Output) output (relationCounts: IDictionary<string,int>) =
        let source = input.manifest
        let files =
            Directory.EnumerateFiles(output, "*", SearchOption.AllDirectories)
            |> Seq.filter (fun path -> Path.GetFileName(path) <> "manifest.json")
            |> Seq.toArray
            |> Array.Parallel.map (fun path ->
                let info = FileInfo(path)
                dict [ "path", box (Path.GetRelativePath(output, path).Replace('\\', '/')); "size_bytes", box info.Length; "sha256", box (sha256File path) ] :> obj)
            |> Seq.sortBy (fun item -> (item :?> IDictionary<string,obj>).["path"] :?> string)
            |> Seq.toArray
        let fieldType value =
            match value with
            | Schema.Text -> "string" | Schema.Int16 -> "int16" | Schema.Int32 -> "int32" | Schema.Int64 -> "int64"
            | Schema.Float64 -> "double" | Schema.Boolean -> "bool" | Schema.Date -> "date32"
        let relations = Schema.relations |> Array.map (fun relation ->
            dict [
                "name", box relation.name; "path", box ("serving/" + relation.name + ".parquet")
                "schema", box (relation.fields |> Array.map (fun field ->
                    let entry = Dictionary<string, obj>()
                    entry.["name"] <- box field.name
                    entry.["type"] <- box (fieldType field.dataType)
                    entry.["nullable"] <- box field.nullable
                    if not (isNull field.enumeration) then entry.["enum"] <- box field.enumeration
                    entry))
                "primary_key", box relation.primaryKey
                "foreign_keys", box (relation.foreignKeys |> Array.map (fun key -> dict [ "fields", box key.fields; "relation", box key.relation; "target_fields", box key.targetFields ]))
                "row_count", box relationCounts.[relation.name]
            ] :> obj)
        let manifest = Dictionary<string,obj>()
        manifest.["bundle_format"] <- box Schema.BundleFormat
        manifest.["bundle_version"] <- box Schema.BundleVersion
        manifest.["serving_schema_version"] <- box Schema.ServingSchemaVersion
        manifest.["diagnostics_schema_version"] <- box Schema.DiagnosticsSchemaVersion
        manifest.["identity_contract"] <- box "jrutil-identity-v1"
        let payloadIdentity =
            files
            |> Seq.map (fun item ->
                let value = item :?> IDictionary<string,obj>
                (value.["path"] :?> string) + "=" + (value.["sha256"] :?> string))
            |> String.concat "\n" |> Identity.sha256
        manifest.["feed_version"] <- box ("sha256:" + payloadIdentity)
        manifest.["build_spec_sha256"] <- box (Identity.sha256(source.GetRawText()))
        if source.TryGetProperty("conversion") |> fst then
            manifest.["compiler"] <- box (JsonSerializer.Deserialize<obj>(source.GetProperty("conversion").GetRawText()))
        manifest.["contract_valid"] <- box true
        manifest.["publication_eligible"] <- box (source.TryGetProperty("publishable") |> function | true, value -> value.GetBoolean() | _ -> true)
        manifest.["calibration"] <- box (source.TryGetProperty("calibration") |> function | true, value -> value.GetBoolean() | _ -> false)
        if source.TryGetProperty("gvd") |> fst then manifest.["service_horizon"] <- box (JsonSerializer.Deserialize<obj>(source.GetProperty("gvd").GetRawText()))
        let combinedSources = ResizeArray<obj>()
        let sourceIds = HashSet<string>(StringComparer.Ordinal)
        let addSources (root: JsonElement) =
            let add (item: JsonElement) =
                let id = if item.TryGetProperty("source_id") |> fst then item.GetProperty("source_id").GetString() else null
                if not (isNull id) && sourceIds.Add(id) then
                    combinedSources.Add(JsonSerializer.Deserialize<obj>(item.GetRawText()))
            match root.TryGetProperty("sources") with
            | true, values -> for item in values.EnumerateArray() do add item
            | _ ->
                match root.TryGetProperty("source") with
                | true, item -> add item
                | _ -> match root.TryGetProperty("source_snapshot") with | true, item -> add item | _ -> ()
        match input.basePackage with
        | Some package ->
            let baseManifest = Path.Combine(package, "manifest.json")
            if File.Exists(baseManifest) then
                use baseDocument = JsonDocument.Parse(File.ReadAllText(baseManifest))
                addSources baseDocument.RootElement
        | None -> ()
        addSources source
        if combinedSources.Count > 0 then manifest.["sources"] <- box (combinedSources.ToArray())
        manifest.["namespaces"] <- box (Schema.namespaces |> Array.map (fun item -> dict [ "name", box item.name; "entity_kind", box item.entityKind ]))
        manifest.["relations"] <- box relations
        manifest.["files"] <- box files
        File.WriteAllText(Path.Combine(output, "manifest.json"), JsonSerializer.Serialize(manifest, JsonSerializerOptions(WriteIndented = true)) + "\n", new UTF8Encoding(false))

    /// Write the production package from a compiler's hand-off. `compiled`
    /// holds relations a compiler already wrote natively (path, row count);
    /// rows generated here for the same relation are appended to them.
    let writePackage (compiled: Map<string, string * int>) (native: JrUtil.Serving.Model.NativeCallArtifacts option) (progress: string -> int64 -> unit) (input: CompilerOutput.Output) output =
        if Directory.Exists(output) || File.Exists(output) then invalidArg "output" "Production package output already exists"
        if input.feed <> "jdf" && input.feed <> "czptt" then invalidArg "input" $"Unknown package feed {input.feed}"
        Directory.CreateDirectory(output) |> ignore
        let progressLock = obj()
        let mutable currentPhase = "production-package"
        let mutable completed = 0L
        let currentProcess = Process.GetCurrentProcess()
        let mutable phaseStarted = Stopwatch.GetTimestamp()
        let mutable phaseCpuStarted = currentProcess.TotalProcessorTime
        let mutable phasePeakPrivate = currentProcess.PrivateMemorySize64
        let emit () = lock progressLock (fun () ->
            currentProcess.Refresh()
            phasePeakPrivate <- max phasePeakPrivate currentProcess.PrivateMemorySize64
            progress currentPhase completed)
        let completePhase () =
            currentProcess.Refresh()
            phasePeakPrivate <- max phasePeakPrivate currentProcess.PrivateMemorySize64
            let elapsed = Stopwatch.GetElapsedTime(phaseStarted)
            let cpu = currentProcess.TotalProcessorTime - phaseCpuStarted
            Serilog.Log.Information(
                "Production package phase complete: {Phase}; rows={Rows}; elapsed_ms={ElapsedMs}; cpu_ms={CpuMs}; private_peak_bytes={PrivatePeakBytes}",
                currentPhase, completed, int64 elapsed.TotalMilliseconds, int64 cpu.TotalMilliseconds, phasePeakPrivate)
        let phase name =
            lock progressLock (fun () ->
                completePhase ()
                currentPhase <- name
                completed <- 0L
                phaseStarted <- Stopwatch.GetTimestamp()
                phaseCpuStarted <- currentProcess.TotalProcessorTime
                currentProcess.Refresh()
                phasePeakPrivate <- currentProcess.PrivateMemorySize64)
            Serilog.Log.Information("Production package phase: {Phase}", name)
            emit ()
        let advance count = lock progressLock (fun () -> completed <- count)
        use heartbeat = new Timer((fun _ -> emit ()), null, 1000, 1000)
        try
            // The overlay/compiler phase has completed, so release its dead
            // object graph before production-package projection starts.
            reclaimManagedPhaseMemory ()
            if input.gtfs.Count = 0 then invalidArg "input" "Compiler output has no GTFS tables"
            let serving = Path.Combine(output, "serving")
            Directory.CreateDirectory(serving) |> ignore
            let counts = Dictionary<string,int>()
            let suppliedState =
                phase "prepare-serving-core"
                let tripCallSummaries = Dictionary<string, TripCallSummary>(StringComparer.Ordinal)
                let nonContiguousTripSequences = Dictionary<string, HashSet<int>>(StringComparer.Ordinal)
                let mutable core = gtfsRelations input
                if native.IsNone then
                    phase "write-ordered-trip_call"
                    counts.["trip_call"] <-
                        writeOrderedTripCalls advance input (Path.Combine(serving, "trip_call.parquet"))
                            tripCallSummaries nonContiguousTripSequences (operationalPaths input)
                    phase "write-ordered-shapes"
                    let shapeCount, pointCount = writeOrderedShapes advance input serving
                    counts.["shape"] <- shapeCount
                    counts.["shape_point"] <- pointCount
                    core <- core |> Map.remove "shape" |> Map.remove "shape_point"
                    phase "write-ordered-trip"
                    counts.["trip"] <- writeOrderedTrips advance input (Path.Combine(serving, "trip.parquet"))
                phase "prepare-serving-keys"
                reclaimManagedPhaseMemory ()
                let keys = keyRelations input tripCallSummaries nonContiguousTripSequences
                phase "prepare-serving-semantics"
                let semantics =
                    if [ "source_notice_metadata"; "source_route_stop_zone_metadata"; "source_transfer_metadata"
                         "source_location_feature_metadata"; "source_travel_restriction_metadata"; "source_trip_feature_metadata" ]
                       |> List.exists (CompilerOutput.has input.sidecars) then
                        semanticRelations native input
                    else Map.empty
                let czptt =
                    if CompilerOutput.has input.sidecars "operational_calls" then
                        czpttRelations input tripCallSummaries nonContiguousTripSequences
                    else Map.empty
                let projectedBase = projectedBaseRelations input
                let merge (state: Map<string, seq<IDictionary<string, obj>>>) (rows: Map<string, seq<IDictionary<string, obj>>>) =
                    rows |> Map.fold (fun current name values ->
                        match current |> Map.tryFind name with
                        | Some existing -> current |> Map.add name (Seq.append existing values)
                        | None -> current |> Map.add name values) state
                let generated = [ keys; semantics; czptt ] |> List.fold merge core
                // Base package rows win over regenerated rows with the same key.
                let preferBase name baseRows currentRows = seq {
                    let relation = Schema.relations |> Array.find (fun relation -> relation.name = name)
                    let seen = HashSet<string>(StringComparer.Ordinal)
                    let key (row: IDictionary<string,obj>) =
                        relation.primaryKey |> Seq.map (fun field -> fieldText row.[field]) |> Identity.compositeKey
                    for row in baseRows do
                        if seen.Add(key row) then yield row
                    for row in currentRows do
                        if seen.Add(key row) then yield row
                }
                let supplied =
                    projectedBase
                    |> Map.fold (fun state name baseRows ->
                        let current = state |> Map.tryFind name |> Option.defaultValue Seq.empty
                        state |> Map.add name (preferBase name baseRows current)) generated
                ref supplied
            for relation in Schema.relations do
                let target = Path.Combine(serving, relation.name + ".parquet")
                // route_stop and the zone relations are derived from the finished calls below.
                if counts.ContainsKey(relation.name) || relation.name = "route_stop" || relation.name.EndsWith("_zone") then () else
                let rows = suppliedState.Value |> Map.tryFind relation.name
                match compiled |> Map.tryFind relation.name, rows with
                | Some (path, count), None ->
                    phase ("finalize-" + relation.name)
                    File.Move(path, target)
                    counts.[relation.name] <- count
                    advance (int64 count)
                | written, rows ->
                    phase ("dedup-" + relation.name)
                    let sourceRows = rows |> Option.defaultValue Seq.empty
                    let mutable read = 0L
                    let unique = deduplicated output relation (sourceRows |> Seq.map (fun row -> read <- read + 1L; advance read; row))
                    phase ("write-" + relation.name)
                    match written with
                    | None -> counts.[relation.name] <- writeParquet advance target relation unique
                    | Some (path, _) ->
                        let part = target + ".generated"
                        writeParquet advance part relation unique |> ignore
                        counts.[relation.name] <- RelationWriter.concat target relation [ path; part ] CancellationToken.None
                        File.Delete(path)
                        File.Delete(part)
                suppliedState.Value <- suppliedState.Value |> Map.remove relation.name
                // Relation writers allocate large, short-lived column and
                // sort buffers.  Reclaim them before the next relation when
                // the process footprint has grown beyond the soft package
                // target; this does not constrain the heap or fail a build.
                currentProcess.Refresh()
                if currentProcess.PrivateMemorySize64 >= 3_000_000_000L then
                    reclaimManagedPhaseMemory ()
            phase "route-stop-order"
            suppliedState.Value <- Map.empty
            let routeStops =
                RouteStopWriter.finalize serving (zoneInputs input) (fun name count ->
                    lock progressLock (fun () -> currentPhase <- name; completed <- count))
            for KeyValue(name, count) in routeStops.counts do counts.[name] <- count
            phase "project-gtfs"
            let feedInfo = match input.gtfs.TryGetValue("feed_info.txt") with | true, table -> Some table | _ -> None
            GtfsProjection.write serving feedInfo (Path.Combine(output, "gtfs.zip"))
            phase "write-diagnostics-summary"
            writeDiagnosticsSummary input output routeStops.summary
            phase "hash-production-payloads"
            writeManifest input output counts
            phase "validate-production-package"
            reclaimManagedPhaseMemory ()
            Validation.validatePackage output |> ignore
            lock progressLock completePhase
        with error ->
            Serilog.Log.Error(error, "Production package failed in {Phase}; partial output retained at {OutputPath}", currentPhase, output)
            reraise ()

    /// Write the optional, explicitly addressed diagnostic artifact.  This is
    /// deliberately separate from the production package inventory.
    let writeDiagnosticArtifact (input: CompilerOutput.Output) output includeTraces =
        if Directory.Exists(output) || File.Exists(output) then
            invalidArg "output" "Diagnostic output already exists"
        let temporary = output + ".tmp-" + Guid.NewGuid().ToString("N")
        Directory.CreateDirectory(temporary) |> ignore
        try
            // Coverage and matching summaries already live in diagnostics.json.
            let summarizedReports = set [
                "coverage.csv"; "coverage_by_mode.csv"; "coverage_by_tier.csv"; "trip_coverage_populations.csv"
                "trip_coverage_populations_by_mode.csv"; "snapshot_day_coverage.csv"; "exclusions.csv" ]
            let writeTables (tables: IReadOnlyDictionary<string, CompilerOutput.Table>) (directory: string) (keep: string -> bool) =
                for KeyValue(name, table) in tables do
                    if keep name then
                        let target = Path.Combine(temporary, directory, name)
                        Directory.CreateDirectory(Path.GetDirectoryName(target)) |> ignore
                        use writer = new StreamWriter(target, false, UTF8Encoding(false))
                        writer.NewLine <- "\n"
                        CompilerOutput.writeCsv writer table
            writeTables input.reports "events" (summarizedReports.Contains >> not)
            writeTables input.czech (Path.Combine("projection", "czech")) (fun _ -> true)
            if includeTraces then writeTables input.mappings "traces" (fun _ -> true)
            else writeTables input.mappings "traces" (fun name -> not (name.StartsWith("base_to_output_", StringComparison.Ordinal)))
            for KeyValue(relative, source) in input.diagnosticFiles do
                let target = Path.Combine(temporary, relative.Replace('/', Path.DirectorySeparatorChar))
                Directory.CreateDirectory(Path.GetDirectoryName(target)) |> ignore
                File.Copy(source, target)
            input.diagnostics |> Option.iter (fun diagnostics ->
                let target = Path.Combine(temporary, "events", "diagnostics.json")
                Directory.CreateDirectory(Path.GetDirectoryName(target)) |> ignore
                File.WriteAllText(target, diagnostics.GetRawText() + "\n", UTF8Encoding(false)))
            let evidenceReferences = ResizeArray<obj>()
            input.basePackage |> Option.iter (fun package ->
                let baseManifest = Path.Combine(package, "manifest.json")
                if File.Exists(baseManifest) then
                    evidenceReferences.Add(dict [
                        "kind", box "base-package"; "manifest_sha256", box (sha256File baseManifest) ] :> obj))
            let entries =
                Directory.EnumerateFiles(temporary, "*", SearchOption.AllDirectories)
                |> Seq.map (fun path -> dict [
                    "path", box (Path.GetRelativePath(temporary, path).Replace('\\', '/'))
                    "size_bytes", box (FileInfo(path).Length)
                    "sha256", box (sha256File path) ] :> obj)
                |> Seq.sortBy (fun item -> (item :?> IDictionary<string,obj>).["path"] :?> string)
                |> Seq.toArray
            let manifest = dict [
                "diagnostics_format", box "jrutil-diagnostics"
                "diagnostics_schema_version", box Schema.DiagnosticsSchemaVersion
                "traces_included", box includeTraces
                "evidence_references", box (evidenceReferences.ToArray())
                "files", box entries ]
            File.WriteAllText(Path.Combine(temporary, "manifest.json"), JsonSerializer.Serialize(manifest, JsonSerializerOptions(WriteIndented = true)) + "\n", new UTF8Encoding(false))
            Directory.Move(temporary, output)
        with _ ->
            if Directory.Exists(temporary) then Directory.Delete(temporary, true)
            reraise ()
