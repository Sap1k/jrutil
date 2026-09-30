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
open JrUtil.Serving.PackageBindingRelations
open JrUtil.Serving.PackageSemanticRelations

module PackageWriter =
    let private extensionRows (input: CompilerOutput.Output) =
        let stopZones = CompilerOutput.rows input.czech "cz_stop_zones.txt" |> Seq.toArray
        let callZones = CompilerOutput.rows input.czech "cz_trip_stop_zones.txt" |> Seq.toArray
        let zones =
            Seq.append
                (stopZones |> Seq.map (fun row -> value "zone_id" row, value "zone_code" row, value "ids_system_id" row, value "source_provenance" row, "route_stop"))
                (callZones |> Seq.map (fun row -> value "zone_id" row, value "zone_code" row, value "ids_system_id" row, value "source_provenance" row, "call"))
            |> Seq.filter (fun (id, _, _, _, _) -> not (String.IsNullOrEmpty id))
            |> Seq.distinctBy (fun (id, _, _, _, _) -> id)
            |> Seq.map (fun (id, code, system, source, scope) -> objectRow [
                "zone_id", box id; "fare_system_id", nullableString system; "zone_code", box code; "name", null
                "source_id", box (if String.IsNullOrEmpty source then "national-jdf-vld-drahy" else source); "source_scope", box scope ])
        let callZoneRows = callZones |> Seq.map (fun row -> objectRow [
            "trip_id", box (value "trip_id" row); "sequence", box (integer (value "stop_sequence" row)); "zone_id", box (value "zone_id" row); "source_order", box 0 ])
        zones, callZoneRows

    let private writeCsv (path: string) (columns: string array) (rows: seq<string array>) =
        Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
        use writer = new StreamWriter(path, false, new UTF8Encoding(false))
        writer.NewLine <- "\n"
        writeCsvRow writer columns
        for row: string array in rows do writeCsvRow writer row

    let private writeGtfsZip (input: CompilerOutput.Output) output =
        let zipPath = Path.Combine(output, "gtfs.zip")
        use stream = File.Open(zipPath, FileMode.CreateNew, FileAccess.Write, FileShare.None)
        use archive = new ZipArchive(stream, ZipArchiveMode.Create, false, Encoding.UTF8)
        let standardTransferColumns = [| "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id"; "transfer_type"; "min_transfer_time" |]
        for name in input.gtfs.Keys |> Seq.sortWith (fun left right -> String.CompareOrdinal(left, right)) do
            let table = input.gtfs.[name]
            // Fastest compression materially reduces package wall/CPU time;
            // contents and deterministic entry metadata are unaffected.
            let entry = archive.CreateEntry(name, CompressionLevel.Fastest)
            entry.LastWriteTime <- DateTimeOffset(1980, 1, 1, 0, 0, 0, TimeSpan.Zero)
            use target = entry.Open()
            match name, table.file with
            | "transfers.txt", _ ->
                // Waiting-time constraints are serving-only; GTFS keeps standard columns.
                use writer = new StreamWriter(target, UTF8Encoding(false))
                writer.NewLine <- "\n"
                CompilerOutput.writeCsv writer {
                    table with
                        columns = standardTransferColumns
                        rows = fun () -> CompilerOutput.values input.gtfs name standardTransferColumns }
            | _, Some path ->
                // Canonical text a compiler already spooled is copied verbatim.
                use source = File.OpenRead(path)
                source.CopyTo(target)
            | _ ->
                use writer = new StreamWriter(target, UTF8Encoding(false))
                writer.NewLine <- "\n"
                CompilerOutput.writeCsv writer table

    let private writeDiagnosticsSummary (input: CompilerOutput.Output) output =
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
            "coverage_populations", box coverage ]
        File.WriteAllText(Path.Combine(output, "diagnostics.json"), JsonSerializer.Serialize(diagnostics, JsonSerializerOptions(WriteIndented = true)) + "\n", new UTF8Encoding(false))

    let private writeManifest (input: CompilerOutput.Output) output (relationCounts: IDictionary<string,int>) =
        let source = input.manifest
        let files =
            Directory.EnumerateFiles(output, "*", SearchOption.AllDirectories)
            |> Seq.filter (fun path -> Path.GetFileName(path) <> "manifest.json")
            |> Seq.map (fun path ->
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
                "schema", box (relation.fields |> Array.map (fun field -> dict [ "name", box field.name; "type", box (fieldType field.dataType); "nullable", box field.nullable ]))
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
        manifest.["namespaces"] <- box [|
            dict [ "name", box "gtfs_trip_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "gtfs_route_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "gtfs_stop_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "gtfs_stop_sequence"; "normalization_version", box 1; "component_encoding", box "decimal-text" ]
            dict [ "name", box "operational_line_course"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8-components/slash" ]
            dict [ "name", box "czptt_pa_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "czptt_tr_id"; "normalization_version", box 1; "component_encoding", box "rfc3986-utf8" ]
            dict [ "name", box "czptt_pa_sequence"; "normalization_version", box 1; "component_encoding", box "decimal-text" ]
            dict [ "name", box "cis_line_id"; "normalization_version", box 1; "component_encoding", box "opaque-string" ]
            dict [ "name", box "cis_trip_id"; "normalization_version", box 1; "component_encoding", box "decimal-int64" ]
            dict [ "name", box "train_number"; "normalization_version", box 1; "component_encoding", box "opaque-string" ]
        |]
        manifest.["relations"] <- box relations
        manifest.["files"] <- box files
        File.WriteAllText(Path.Combine(output, "manifest.json"), JsonSerializer.Serialize(manifest, JsonSerializerOptions(WriteIndented = true)) + "\n", new UTF8Encoding(false))

    /// Write the production package from a compiler's hand-off. `compiled`
    /// holds relations a compiler already wrote natively (path, row count).
    let writePackage (compiled: Map<string, string * int>) (nativeSummaries: JrUtil.Serving.Model.NativeCallArtifacts option) (progress: string -> int64 -> unit) (input: CompilerOutput.Output) output =
        if Directory.Exists(output) || File.Exists(output) then invalidArg "output" "Production package output already exists"
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
            phase "start-independent-output-jobs"
            let startJob name action =
                Task.Run(fun () ->
                    let started = Stopwatch.StartNew()
                    action ()
                    Serilog.Log.Information(
                        "Production package job complete: {Job}; elapsed_ms={ElapsedMs}",
                        name, int64 started.Elapsed.TotalMilliseconds))
            let gtfsZipJob = startJob "zip-gtfs" (fun () -> writeGtfsZip input output)
            let counts = Dictionary<string,int>()
            let bindingsState, suppliedState =
                phase "prepare-serving-core"
                let tripCallSummaries = Dictionary<string, TripCallSummary>(StringComparer.Ordinal)
                let nonContiguousTripSequences = Dictionary<string, HashSet<int>>(StringComparer.Ordinal)
                let targetCallSchedules = Dictionary<string, TargetCallSchedule>(StringComparer.Ordinal)
                let wantedTargetTrips =
                    if nativeSummaries.IsSome || not (CompilerOutput.has input.mappings "source_to_output_trips.csv") then HashSet<string>(StringComparer.Ordinal) else
                    HashSet<string>(
                        CompilerOutput.values input.mappings "source_to_output_trips.csv" [| "output_trip_id" |]
                        |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
                let mutable core = gtfsRelations input tripCallSummaries nonContiguousTripSequences
                if nativeSummaries.IsNone then
                    phase "write-ordered-trip_call"
                    let relation = Schema.relations |> Array.find (fun value -> value.name = "trip_call")
                    let target = Path.Combine(serving, "trip_call.parquet")
                    counts.[relation.name] <- writeOrderedTripCalls advance input target tripCallSummaries nonContiguousTripSequences wantedTargetTrips targetCallSchedules
                    core <- core |> Map.remove relation.name
                    phase "write-ordered-shapes"
                    let shapeCount, pointCount = writeOrderedShapes advance input serving
                    counts.["shape"] <- shapeCount
                    counts.["shape_point"] <- pointCount
                    core <- core |> Map.remove "shape" |> Map.remove "shape_point"
                    phase "write-ordered-trip"
                    counts.["trip"] <- writeOrderedTrips advance input (Path.Combine(serving, "trip.parquet"))
                    core <- core |> Map.remove "trip"
                phase "prepare-source-bindings"
                let bindingsSequence, calls, baseCoverage =
                    match nativeSummaries with
                    | Some native -> BindingWriter.readNativeFacts native.tripFacts native.summaries, Seq.empty, Seq.empty
                    | None -> bindingRows None input input.manifest tripCallSummaries nonContiguousTripSequences targetCallSchedules
                let bindings = bindingsSequence |> Seq.toArray
                let czptt, czpttCalls =
                    if CompilerOutput.has input.sidecars "operational_calls" then
                        czpttRelations input bindings input.manifest
                    else Map.empty, Seq.empty
                if nativeSummaries.IsNone then
                    // Do not overlap the dominant nationwide source-call sort
                    // with every other serving relation.  On the national
                    // overlay this used to keep tens of millions of call rows
                    // active while feature/provenance relations were sorted.
                    phase "typed-source_call_map"
                    let target = Path.Combine(serving, "source_call_map.parquet")
                    let report operation count =
                        lock progressLock (fun () -> currentPhase <- "source-calls-" + operation; completed <- count)
                    let rows = Seq.append calls (SourceCallWriter.fromModelRows czpttCalls)
                    counts.["source_call_map"] <- SourceCallWriter.writeMappedTyped target rows CancellationToken.None report
                    targetCallSchedules.Clear()
                    // Drop the source-call projection closures and their
                    // lookup tables before constructing identity/semantic
                    // relations from the same nationwide bindings.
                    reclaimManagedPhaseMemory ()
                let zones, callZones = extensionRows input
                let projectedBase = projectedBaseRelations input
                phase "prepare-serving-identities"
                reclaimManagedPhaseMemory ()
                let identities = identityRelations input bindings input.manifest
                reclaimManagedPhaseMemory ()
                phase "prepare-native-semantics"
                let semantics =
                    if CompilerOutput.has input.sidecars "source_route_stop_zone_metadata"
                       || CompilerOutput.has input.sidecars "source_notice_metadata" then
                        semanticRelations nativeSummaries input input.manifest
                    else Map.empty
                let generated =
                    identities |> Map.fold (fun state name rows -> state |> Map.add name rows) core
                    |> fun state -> semantics |> Map.fold (fun current name rows -> current |> Map.add name rows) state
                    |> fun state -> czptt |> Map.fold (fun current name rows -> current |> Map.add name rows) state
                    |> Map.add "source_trip_map" (bindings |> Seq.map bindingRow)
                    |> fun state -> if nativeSummaries.IsSome then state |> Map.add "source_call_map" czpttCalls else state
                    |> Map.add "source_trip_coverage" (Seq.append (czptt |> Map.tryFind "source_trip_coverage" |> Option.defaultValue Seq.empty) baseCoverage)
                    |> Map.add "fare_zone" zones
                    |> Map.add "call_zone" callZones
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
                        let combined = preferBase name baseRows current
                        state |> Map.add name combined) generated
                gtfsZipJob.Wait()
                ref bindings, ref supplied
            for relation in Schema.relations do
                let target = Path.Combine(serving, relation.name + ".parquet")
                if counts.ContainsKey(relation.name) then () else
                match compiled |> Map.tryFind relation.name with
                | Some (path, count) ->
                    phase ("finalize-" + relation.name)
                    File.Move(path, target)
                    counts.[relation.name] <- count
                    advance (int64 count)
                | None ->
                    match nativeSummaries with
                    | _ when relation.name = "source_trip_map" ->
                        let report operation count =
                            lock progressLock (fun () -> currentPhase <- "source-trips-" + operation; completed <- count)
                        counts.[relation.name] <- BindingWriter.write target CancellationToken.None report bindingsState.Value
                        if nativeSummaries.IsNone then bindingsState.Value <- Array.empty
                    | None when relation.name = "source_call_map" ->
                        phase "typed-source_call_map"
                        let report operation count =
                            lock progressLock (fun () -> currentPhase <- "source-calls-" + operation; completed <- count)
                        let rows = suppliedState.Value |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        counts.[relation.name] <- SourceCallWriter.writeMapped target rows CancellationToken.None report
                    | Some native when relation.name = "source_call_map" ->
                        phase "sort-and-write-native-source-calls"
                        let byTrip = bindingsState.Value |> Seq.map (fun binding -> binding.trip_id, binding.binding_id) |> dict
                        let report operation count =
                            lock progressLock (fun () -> currentPhase <- "source-calls-" + operation; completed <- count)
                        counts.[relation.name] <- SourceCallWriter.write target native.sourceCalls byTrip CancellationToken.None report
                        bindingsState.Value <- Array.empty
                    | None when relation.name = "shape" || relation.name = "shape_point" ->
                        phase ("write-ordered-" + relation.name)
                        let rows = suppliedState.Value |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        counts.[relation.name] <- writeParquet advance target relation rows
                    | _ when relation.name = "object_origin"
                             || relation.name = "binding_evidence"
                             || relation.name = "route_stop" ->
                        phase ("typed-" + relation.name)
                        let rows = suppliedState.Value |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        counts.[relation.name] <- writeRequiredTextRelation (fun _ count -> advance count) target relation rows
                    | _ ->
                        phase ("dedup-" + relation.name)
                        let sourceRows = suppliedState.Value |> Map.tryFind relation.name |> Option.defaultValue Seq.empty
                        let mutable read = 0L
                        let rows = deduplicated output relation (sourceRows |> Seq.map (fun row -> read <- read + 1L; advance read; row))
                        phase ("write-" + relation.name)
                        counts.[relation.name] <- writeParquet advance target relation rows
                suppliedState.Value <- suppliedState.Value |> Map.remove relation.name
                // Relation writers allocate large, short-lived column and
                // sort buffers.  Reclaim them before the next relation when
                // the process footprint has grown beyond the soft package
                // target; this does not constrain the heap or fail a build.
                currentProcess.Refresh()
                if currentProcess.PrivateMemorySize64 >= 3_000_000_000L then
                    reclaimManagedPhaseMemory ()
            phase "write-diagnostics-summary"
            writeDiagnosticsSummary input output
            phase "hash-production-payloads"
            writeManifest input output counts
            phase "validate-production-package"
            suppliedState.Value <- Map.empty
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
