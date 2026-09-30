// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text.Json
open Serilog
open Serilog.Context

open JrUtil
open JrUtil.RegionalOverlay.Types
open JrUtil.GeoData
open JrUtil.Utils

let docstring = (fun (s: string) -> s.Trim()) """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil-multitool.exe jdf-to-bundle [options] [--gvd-year=YEAR] --snapshot-descriptor=FILE --converter-version=VALUE <JDF-input> <bundle-out-dir>
    jrutil-multitool.exe jdf-export-post-features [options] [--policy=FILE] --evidence=DIR --output=DIR
    jrutil-multitool.exe czptt-to-bundle [options] --catalog-snapshot=FILE <CzPtt-in-file> <bundle-out-dir>
    jrutil-multitool.exe regional-gtfs-overlay [options] --policy=FILE --gvd-year=YEAR --converter-version=VALUE (--source=BINDING --source-descriptor=BINDING)... <base-bundle> <overlay-bundle-out>
    jrutil-multitool.exe validate-package <package-dir>
    jrutil-multitool.exe compare-packages (--byte-identical | --semantic) [--expect=FILE] <left-package> <right-package>
    jrutil-multitool.exe fix-jdf [options] <JDF-in-dir> <JDF-out-dir>
    jrutil-multitool.exe merge-jdf [options] --gvd-year=YEAR --reference-date=DATE <JDF-out-dir> <JDF-in-dir>...
    jrutil-multitool.exe --help

Options:
    -g --ext-geodata=PATH        CSV file or directory with stop positions
    -o --cz-pbf=PATH             OSM data for Czech Republic
    -l --logfile=FILE            Logfile
    -s --strict                  Fail instead of skipping a malformed batch
    --snapshot-descriptor=FILE    Retrieval provenance and input checksum JSON
    --converter-version=VALUE     Exact JrUtil fork version or commit for provenance
    --international-route-policy=VALUE  keep-all (default) or regional-adjacent
    --transport-mode-rules=FILE  Reviewed JDF effective transport-mode rule CSV
    --no-estimated-posts         Disable candidate-based internal post inference
    --routing-osm-pbf=FILE       Osmium demand-clipped road/tram PBF for routed inference
    --diagnostic-post-labels     Emit inferred O*/O-direction labels in GTFS platform_code
    --post-review-stops=FILE     Stop IDs/names for routed-inference review GeoJSON
    --capture-post-inference-evidence=DIR  Persist reusable policy-neutral routed evidence
    --post-inference-evidence-only         Stop after writing the evidence directory
    --capture-stop-region=BOX              Capture only stops in MINLON,MINLAT,MAXLON,MAXLAT (review/training packs)
    --capture-exclude-source=PREFIXES      Drop observations whose source ID starts with a comma-separated prefix
    --export-post-context-calls=FILE       Also write per-call context IDs (Parquet) for label joins
    --post-inference-evidence=DIR          Reuse captured routed evidence without loading a graph
    --post-inference-policy=FILE           Versioned routed-inference policy JSON
    --no-post-inference-scores             Skip diagnostic score rows for publication-only bundles
    --evidence=DIR                         Evidence directory for policy replay
    --policy=FILE                          Policy JSON for policy replay
    --gvd-year=YEAR                       GVD year: overlay window, merge-jdf validity bound, jdf-to-bundle manifest
    --reference-date=DATE                 merge-jdf: drop timetables expired before YYYY-MM-DD
    --source=BINDING                      Overlay source binding SOURCE_ID=GTFS.zip
    --source-descriptor=BINDING           Source checksum binding SOURCE_ID=descriptor.json
    --diagnostics-out=DIR                 Optional separate detailed diagnostics artifact
    --diagnostic-traces                   Include large compiler trace relations in diagnostics
    --byte-identical                      Require every production byte to match
    --semantic                            Compare relations by key, GTFS as row multisets, JSON canonically
    --expect=FILE                         Allow-list of intended semantic differences (`<kind> <glob>` lines)
    --output=DIR                           Replay report output directory
    -j --jobs=VALUE             Worker count or auto (default: auto)
    --memory-budget=VALUE       RAM budget such as 10GiB or auto (default: auto)
    --batch-output=VALUE        fix-jdf output: directory or zip (default: directory)
    --catalog-snapshot=FILE     Offline KADR catalog snapshot JSON for CZPTT
    --operational-points=VALUE  CZPTT internal points: gtfs (default) or sidecar
    --sr70=FILE                 SR70 CSV snapshot for CZPTT point names and coordinates
    --osm-pbf=FILE              Shared regional OSM PBF for CZPTT coordinate gaps
    --osm-aliases=FILE          Reviewed CZPTT identity-to-OSM-object aliases
    --progress-events           Emit versioned JRUTIL_PROGRESS JSON lines
"""

let emitProgressEvent enabled eventName (fields: (string * obj) list) =
    if enabled then
        let payload = Dictionary<string, obj>()
        payload.["schema_version"] <- box 1
        payload.["event"] <- box eventName
        for name, value in fields do payload.[name] <- value
        let json = JsonSerializer.Serialize(payload)
        let line = "JRUTIL_PROGRESS " + json
        // The console sink recognizes this property and writes the raw event
        // under the same lock used for human-readable Serilog output.
        Log.ForContext("JrUtilProgressEvent", line)
           .Information("{ProgressEvent:l}", line)

let private parseOverlayBindingsArguments args =
    let parseBinding argumentName (value: string) =
        let separator = value.IndexOf('=')
        if separator <= 0 || separator = value.Length - 1 then
            invalidArg argumentName $"Expected SOURCE_ID=PATH, got {value}"
        value.Substring(0, separator), value.Substring(separator + 1)
    let sources = argValues args "--source" |> Seq.map (parseBinding "--source") |> Seq.toArray
    let descriptorBindings = argValues args "--source-descriptor" |> Seq.map (parseBinding "--source-descriptor") |> Seq.toArray
    if sources.Length < 1 then invalidArg "--source" "regional-gtfs-overlay requires at least one source"
    if sources |> Array.map fst |> Array.distinct |> Array.length <> sources.Length then invalidArg "--source" "Duplicate source ID"
    if descriptorBindings |> Array.map fst |> Array.distinct |> Array.length <> descriptorBindings.Length then invalidArg "--source-descriptor" "Duplicate source descriptor ID"
    let descriptors = descriptorBindings |> dict
    if descriptors.Count <> sources.Length then invalidArg "--source-descriptor" "Each source requires exactly one descriptor"
    let bindings =
        sources
        |> Array.map (fun (sourceId, payloadPath) ->
            match descriptors.TryGetValue(sourceId) with
            | true, descriptorPath -> { sourceId = sourceId; payloadPath = payloadPath; descriptorPath = descriptorPath }
            | _ -> invalidArg "--source-descriptor" $"Missing descriptor for source {sourceId}")
    let mutable gvdYear = 0
    if not (Int32.TryParse(argValue args "--gvd-year", &gvdYear)) then invalidArg "--gvd-year" "Expected a four-digit year"
    gvdYear, bindings

[<EntryPoint>]
let main (args: string array) =
    withProcessedArgs docstring args (fun args ->
        setupLogging (optArgValue args "--logfile") ()
        let progressEvents = argFlagSet args "--progress-events"

        let memoryRequest =
            optArgValue args "--memory-budget"
            |> Option.defaultValue "auto"
            |> Execution.parseMemoryBudget
        let processBudget =
            Execution.resolveMemoryBudget memoryRequest
            |> min Execution.ProductionProcessBudgetBytes
        let memoryRequest = Execution.FixedMemory processBudget
        Log.Information("Memory target: process target {ProcessBudget} bytes for worker admission and spill decisions; the GC heap is not hard limited",
                        processBudget)
        let jobRequest =
            optArgValue args "--jobs"
            |> Option.defaultValue "auto"
            |> Execution.parseJobRequest
        let admissionBytesByStage = Collections.Generic.Dictionary<string, int64>()
        let jobsFor stage workload reservedBytes =
            let memorySnapshot = Execution.detectMemorySnapshot ()
            let memoryBudget =
                Execution.resolveMemoryBudgetFromSnapshot memoryRequest memorySnapshot
            let plan =
                Execution.workerPlan
                    workload
                    (Execution.requestedJobCount jobRequest)
                    Environment.ProcessorCount
                    memoryBudget
                    reservedBytes
            let requested =
                match jobRequest with
                | Execution.AutoJobs -> "auto"
                | Execution.FixedJobs value -> string value
            let baselinePrivateBytes = memorySnapshot.processPrivateBytes
            let automaticReserveBytes =
                Execution.automaticMemoryReserveBytes memorySnapshot.effectiveTotalBytes
            let automaticEvictableBytes =
                Execution.automaticEvictableAllowanceBytes
                    memorySnapshot.effectiveTotalBytes
                    memorySnapshot.availableBytes
                    memorySnapshot.processPrivateBytes
            let admissionBytes =
                max (8L * Execution.MiB)
                    (plan.memoryBudgetBytes * 85L / 100L - baselinePrivateBytes)
            admissionBytesByStage.[stage] <- admissionBytes
            let weightFormula =
                match workload with
                | Execution.MergeParsing ->
                    "max(8 MiB, ZIP bytes * 12); directories use uncompressed bytes * 3"
                | _ -> "max(8 MiB, uncompressed input bytes * 3)"
            let minimumWorkers, minimumIo = Threading.ThreadPool.GetMinThreads()
            Threading.ThreadPool.SetMinThreads(
                max minimumWorkers plan.maximumWorkers, minimumIo)
            |> ignore
            Log.Information(
                "Execution plan for {Stage}: {InitialWorkers}-{MaximumWorkers} adaptive workers; requested {RequestedJobs}; " +
                "CPU {ProcessorCount}; memory cap {MemoryJobs}; budget {MemoryBudgetGiB:F1} GiB; " +
                "available {AvailableMemoryGiB:F1} GiB; evictable allowance {EvictableMemoryGiB:F1} GiB; " +
                "system reserve {SystemReserveGiB:F1} GiB",
                stage, plan.initialWorkers, plan.maximumWorkers, requested, plan.processorCount,
                plan.memoryLimitedJobs, float plan.memoryBudgetBytes / float Execution.GiB,
                float memorySnapshot.availableBytes / float Execution.GiB,
                float automaticEvictableBytes / float Execution.GiB,
                float automaticReserveBytes / float Execution.GiB)
            emitProgressEvent progressEvents "execution_plan" [
                "stage", box stage
                "requested_jobs", box requested
                "processor_count", box plan.processorCount
                "memory_budget_bytes", box plan.memoryBudgetBytes
                "reserved_bytes", box plan.reservedBytes
                "worker_allowance_bytes", box plan.workerAllowanceBytes
                "memory_limited_jobs", box plan.memoryLimitedJobs
                "resolved_workers", box plan.resolvedWorkers
                "initial_workers", box plan.initialWorkers
                "maximum_workers", box plan.maximumWorkers
                "phase_start_private_bytes", box baselinePrivateBytes
                "effective_total_memory_bytes", box memorySnapshot.effectiveTotalBytes
                "available_memory_bytes", box memorySnapshot.availableBytes
                "automatic_evictable_allowance_bytes", box automaticEvictableBytes
                "automatic_system_reserve_bytes", box automaticReserveBytes
                "admission_allowance_bytes", box admissionBytes
                "weight_formula", box weightFormula
                "steady_memory_percent", box 85
                "pause_memory_percent", box 95
                "resume_memory_percent", box 80
            ]
            plan
        let phase stage name state =
            emitProgressEvent progressEvents "phase" [
                "stage", box stage
                "name", box name
                "state", box state
            ]
        let batchEvent eventName stage batchPath (error: exn option) =
            emitProgressEvent progressEvents eventName (
                [
                    "stage", box stage
                    "batch", box batchPath
                ]
                @ (error
                   |> Option.map (fun value -> [
                       "exception_type", box (value.GetType().FullName)
                       "message", box value.Message
                   ])
                   |> Option.defaultValue []))
        let resourceUsage stage phase spillBytes =
            let currentProcess = Diagnostics.Process.GetCurrentProcess()
            currentProcess.Refresh()
            let gc = GC.GetGCMemoryInfo()
            emitProgressEvent progressEvents "resource_usage" [
                "stage", box stage
                "phase", box phase
                "working_set_bytes", box currentProcess.WorkingSet64
                "peak_working_set_bytes", box currentProcess.PeakWorkingSet64
                "private_bytes", box currentProcess.PrivateMemorySize64
                "managed_heap_bytes", box gc.HeapSizeBytes
                "fragmented_bytes", box gc.FragmentedBytes
                "spill_bytes", box spillBytes
            ]
        let schedulerSample stage (sample: Utils.AdaptiveSchedulerSample) =
            emitProgressEvent progressEvents "scheduler_sample" (
                [
                    "stage", box stage
                    "target_workers", box sample.targetWorkers
                    "active_workers", box sample.activeWorkers
                    "maximum_active_workers", box sample.maximumActiveWorkers
                    "completed_backlog", box sample.completedBacklog
                    "reorder_depth", box sample.completedBacklog
                    "retained_estimated_bytes", box sample.retainedEstimatedBytes
                    "reorder_bytes", box sample.retainedEstimatedBytes
                    "queued_parse_work", box sample.queuedWork
                    "queued_transform_work", box 0
                    "private_bytes", box sample.privateBytes
                    "working_set_bytes", box sample.workingSetBytes
                    "managed_heap_bytes", box sample.managedHeapBytes
                    "normalized_cpu_percent", box sample.normalizedCpuPercent
                    "throughput_per_second", box sample.throughputPerSecond
                    "admission_paused", box sample.admissionPaused
                ] @ (sample.pauseReason
                     |> Option.map (fun reason -> ["pause_reason", box reason])
                     |> Option.defaultValue []))
        let adaptiveAdmissionBytes stage = admissionBytesByStage.[stage]
        let inputBytes batchPath =
            if File.Exists(batchPath) then
                if Path.GetExtension(batchPath).Equals(".zip", StringComparison.OrdinalIgnoreCase) then
                    use archive = ZipFile.OpenRead(batchPath)
                    archive.Entries
                    |> Seq.filter (fun entry -> not (String.IsNullOrEmpty(entry.Name)))
                    |> Seq.sumBy (fun entry -> entry.Length)
                else FileInfo(batchPath).Length
            elif Directory.Exists(batchPath) then
                Directory.EnumerateFiles(batchPath, "*", SearchOption.AllDirectories)
                |> Seq.sumBy (fun path -> FileInfo(path).Length)
            else 0L
        let mergeInputWeight batchPath =
            if File.Exists(batchPath) then
                max (8L * Execution.MiB) (FileInfo(batchPath).Length * 12L)
            else
                max (8L * Execution.MiB) (inputBytes batchPath * 3L)

        let longestTripStops (tripStops: JdfModel.TripStop array) =
            if Array.isEmpty tripStops then
                invalidOp "Cannot select a representative trip from an empty TripStop relation"
            let counts = Dictionary<_, struct (int * int)>()
            tripStops
            |> Array.iteri (fun index tripStop ->
                let key = tripStop.routeId, tripStop.tripId
                match counts.TryGetValue(key) with
                | true, struct (count, first) -> counts.[key] <- struct (count + 1, first)
                | _ -> counts.[key] <- struct (1, index))
            let mutable selected = tripStops.[0].routeId, tripStops.[0].tripId
            let mutable selectedCount = -1
            let mutable selectedFirst = Int32.MaxValue
            for KeyValue(key, struct (count, first)) in counts do
                if count > selectedCount || (count = selectedCount && first < selectedFirst) then
                    selected <- key
                    selectedCount <- count
                    selectedFirst <- first
            tripStops
            |> Array.filter (fun tripStop ->
                (tripStop.routeId, tripStop.tripId) = selected)

        let sr70Path = optArgValue args "--sr70"
        let osmPath = optArgValue args "--osm-pbf"
        let osmAliasesPath = optArgValue args "--osm-aliases"
        sr70Path
        |> Option.iter (fun path ->
            if not (File.Exists(path)) then
                invalidArg "--sr70" $"SR70 snapshot does not exist: {path}")
        osmPath
        |> Option.iter (fun path ->
            if not (File.Exists(path)) then
                invalidArg "--osm-pbf" $"OSM snapshot does not exist: {path}")
        osmAliasesPath
        |> Option.iter (fun path ->
            if not (File.Exists(path)) then
                invalidArg "--osm-aliases" $"OSM alias file does not exist: {path}")
        let internationalRoutePolicy =
            optArgValue args "--international-route-policy"
            |> Option.defaultValue "keep-all"
            |> JdfToGtfs.parseInternationalRoutePolicy
        let transportModeRules =
            optArgValue args "--transport-mode-rules"
            |> Option.map JdfToGtfs.loadTransportModeRules
            |> Option.defaultValue JdfToGtfs.emptyTransportModeRules
        let routingPbf = optArgValue args "--routing-osm-pbf"
        let postInferenceEvidence = optArgValue args "--post-inference-evidence"
        // Candidate discovery has to happen during fix-jdf, before the merged
        // routing-demand relation (and therefore its clipped routing PBF) can
        // exist.  Keep that switch independent from routed inference, which is
        // only available to conversion commands once a routing PBF is supplied.
        let estimatedPostActivation =
            Execution.estimatedPostActivation
                (argFlagSet args "--no-estimated-posts")
                (routingPbf.IsSome || postInferenceEvidence.IsSome)
        let collectEstimatedPostEvidence = estimatedPostActivation.collectEvidence
        let routedPostInference = estimatedPostActivation.runRoutedInference
        let mutable exitCode = 0
        if argFlagSet args "regional-gtfs-overlay" then
            try
                let gvdYear, sourceBindings = parseOverlayBindingsArguments args
                let baseBundle = argValue args "<base-bundle>"
                JrUtil.Serving.Validation.validatePackage baseBundle |> ignore
                let result =
                    RegionalGtfsOverlay.compile {
                        auditDate = None
                        policyPath = argValue args "--policy"
                        gvdYear = gvdYear
                        bindings = sourceBindings
                        baseBundle = baseBundle
                        outputBundle = argValue args "<overlay-bundle-out>"
                        converterVersion = argValue args "--converter-version"
                        diagnosticsOutput = optArgValue args "--diagnostics-out"
                        diagnosticTraces = argFlagSet args "--diagnostic-traces" }
                Log.Information(
                    "Regional overlay complete: sources={Sources}; matched_trips={MatchedTrips}; unmatched_trips={UnmatchedTrips}; ambiguous_trips={AmbiguousTrips}",
                    String.concat "," result.sources, result.aggregate.matchedTrips,
                    result.aggregate.unmatchedTrips, result.aggregate.ambiguousTrips)
            with error ->
                exitCode <- 1
                Log.Error(error, "Regional GTFS overlay failed")
        else if argFlagSet args "validate-package" then
            try
                let result = JrUtil.Serving.Validation.validatePackage (argValue args "<package-dir>")
                Log.Information("Production package valid: files={Files}; relations={Relations}", result.fileCount, result.relationCount)
            with error ->
                exitCode <- 1
                Log.Error(error, "Production package validation failed")
        else if argFlagSet args "compare-packages" then
            try
                let left, right = argValue args "<left-package>", argValue args "<right-package>"
                if argFlagSet args "--byte-identical" then
                    JrUtil.Serving.Validation.compareByteIdentical left right
                    Log.Information("Production packages are byte-identical")
                else
                    let expectations =
                        optArgValue args "--expect"
                        |> Option.map (File.ReadAllLines >> JrUtil.Serving.Comparison.parseExpectations)
                        |> Option.defaultValue []
                    let report = JrUtil.Serving.Comparison.comparePackages expectations left right
                    for difference in report.differences do
                        let expected = not (List.contains difference report.unexpected)
                        let level = if expected then Events.LogEventLevel.Information else Events.LogEventLevel.Error
                        Log.Write(level, "{Expected}{Kind} {Subject}: {Summary}",
                                  (if expected then "expected " else ""), difference.kind, difference.subject, difference.summary)
                        for sample in difference.samples do Log.Write(level, "    {Sample}", sample)
                    Log.Information("Compared {Count} package parts; {Differences} differences, {Unexpected} unexpected",
                                    report.compared.Length, report.differences.Length, report.unexpected.Length)
                    if not report.isEquivalent then exitCode <- 1
            with error ->
                exitCode <- 1
                Log.Error(error, "Package comparison failed")
        else if argFlagSet args "jdf-to-bundle" then
            try
                let bundlePlan =
                    jobsFor "jdf-to-bundle" Execution.BundleWork
                            (max (512L * Execution.MiB) (processBudget - 256L * Execution.MiB))
                let bundleProgress (event: JdfBundle.BundleProgressEvent) =
                    emitProgressEvent progressEvents "work_progress" (
                        [ "stage", box "jdf-to-bundle"
                          "phase", box event.phase
                          "state", box event.state
                          "completed", box event.completed
                          "unit", box event.unit
                          "elapsed_ms", box event.elapsedMilliseconds
                          "active_workers", box event.activeWorkers
                          "private_bytes", box event.privateBytes
                          "working_set_bytes", box event.workingSetBytes ]
                        @ (event.total |> Option.map (fun value -> [ "total", box value ])
                           |> Option.defaultValue [])
                        @ (event.detail |> Option.map (fun value -> [ "detail", box value ])
                           |> Option.defaultValue []))
                let bundleOptions: JdfBundle.BundleOptions = {
                    snapshotDescriptorPath = argValue args "--snapshot-descriptor"
                    converterVersion = argValue args "--converter-version"
                    internationalPolicy = internationalRoutePolicy
                    transportModeRules = transportModeRules
                    estimatedPosts = routedPostInference
                    routingPbfPath = routingPbf
                    diagnosticPostLabels = argFlagSet args "--diagnostic-post-labels"
                    maximumWorkers = min 8 bundlePlan.resolvedWorkers
                    memoryBudgetBytes = bundlePlan.memoryBudgetBytes
                    reviewStopsPath = optArgValue args "--post-review-stops"
                    capturePostInferenceEvidencePath = optArgValue args "--capture-post-inference-evidence"
                    postInferenceEvidenceOnly = argFlagSet args "--post-inference-evidence-only"
                    captureRestriction =
                        { JdfPostEvidence.CaptureRestriction.stopRegion =
                            optArgValue args "--capture-stop-region"
                            |> Option.map JdfPostEvidence.parseCaptureStopRegion
                          excludedSourcePrefixes =
                            optArgValue args "--capture-exclude-source"
                            |> Option.map JdfPostEvidence.parseCaptureExcludedSources
                            |> Option.defaultValue [||] }
                    exportPostContextCallsPath = optArgValue args "--export-post-context-calls"
                    postInferenceEvidencePath = postInferenceEvidence
                    postInferencePolicyPath = optArgValue args "--post-inference-policy"
                    includePostInferenceScores = not(argFlagSet args "--no-post-inference-scores")
                    diagnosticsOutput = optArgValue args "--diagnostics-out"
                    diagnosticTraces = argFlagSet args "--diagnostic-traces"
                    progress = bundleProgress
                    gvdYear =
                        optArgValue args "--gvd-year"
                        |> Option.map (fun value ->
                            match Int32.TryParse(value) with
                            | true, year when year >= 2000 && year <= 9999 -> year
                            | _ -> invalidArg "--gvd-year" "Expected a four-digit year")
                }
                phase "jdf-to-bundle" "write-bundle" "started"
                let bundleResult =
                    JdfBundle.execute bundleOptions (argValue args "<JDF-input>") (argValue args "<bundle-out-dir>")
                match bundleResult with
                | JdfBundle.CaptureCompleted(manifest,metrics) ->
                    phase "jdf-to-bundle" "capture-post-inference-evidence" "completed"
                    emitProgressEvent progressEvents "capture_metrics" [
                        "stage", box "jdf-to-bundle"
                        "estimated_evidence_bytes", box metrics.estimatedEvidenceBytes
                        "atomic_output_headroom_bytes", box metrics.atomicOutputHeadroomBytes
                        "current_spill_bytes", box metrics.currentSpillBytes
                        "peak_spill_bytes", box metrics.peakSpillBytes
                        "maximum_workers", box metrics.maximumWorkers
                    ]
                    resourceUsage "jdf-to-bundle" "capture-post-inference-evidence"
                                  metrics.peakSpillBytes
                    Log.Information(
                        "Post-inference evidence capture completed: pack_id={PackId}; rows={Rows}",
                        manifest.packId,manifest.routePointEvidenceCount)
                | JdfBundle.BundleCompleted ->
                    phase "jdf-to-bundle" "write-bundle" "completed"
                    resourceUsage "jdf-to-bundle" "write-bundle" 0L
                Log.Information("Finished!")
            with e ->
                exitCode <- 1
                Log.Error(e, "JDF bundle conversion failed")
        else if argFlagSet args "jdf-export-post-features" then
            try
                JdfBundle.exportPostInferenceFeatures
                    (argValue args "--evidence")
                    (optArgValue args "--policy")
                    (argValue args "--output")
                Log.Information("Finished!")
            with e ->
                exitCode <- 1
                Log.Error(e,"JDF post-inference feature export failed")
        else if argFlagSet args "czptt-to-bundle" then
            try
                let operationalPointMode =
                    match optArgValue args "--operational-points" |> Option.defaultValue "gtfs" with
                    | "gtfs" -> CzPttToGtfs.Gtfs
                    | "sidecar" -> CzPttToGtfs.Sidecar
                    | value -> invalidArg "--operational-points" $"Expected gtfs or sidecar, got {value}"
                let options: CzPttPackage.Options = {
                    catalog = CzPttToGtfs.loadCatalogSnapshot (argValue args "--catalog-snapshot")
                    conversion = { operationalPointMode = operationalPointMode }
                    sr70Path = sr70Path
                    osmPath = osmPath
                    osmAliasesPath = osmAliasesPath
                    diagnosticsOutput = optArgValue args "--diagnostics-out"
                    diagnosticTraces = argFlagSet args "--diagnostic-traces"
                }
                let bundleProgress name state =
                    phase "convert" name state
                    if state = "completed" then
                        resourceUsage "convert" name (CzPttBundle.currentSpillBytes())
                CzPttPackage.write options (argValue args "<CzPtt-in-file>") (argValue args "<bundle-out-dir>") bundleProgress |> ignore
                Log.Information("Finished!")
            with e ->
                exitCode <- 1
                Log.Error(e, "CZPTT bundle conversion failed")
        else if argFlagSet args "fix-jdf" then
            let inDir = argValues args "<JDF-in-dir>" |> Seq.head
            let outDir = argValue args "<JDF-out-dir>"
            let geodataPath = optArgValue args "--ext-geodata"
            let czPbf = optArgValue args "--cz-pbf"
            let batchOutput =
                optArgValue args "--batch-output"
                |> Option.defaultValue "directory"
                |> fun value -> value.ToLowerInvariant()
            if batchOutput <> "directory" && batchOutput <> "zip" then
                invalidArg "--batch-output" "fix-jdf batch output must be 'directory' or 'zip'"
            Directory.CreateDirectory(outDir) |> ignore

            phase "fix-jdf" "read-external-stops" "started"
            let extStopsToMatch =
                geodataPath
                |> Option.map (fun gdp ->
                    Utils.logWrappedOp "Reading external stops" <| fun () ->
                        ExternalCsv.otherStopsFromPathForJdfMatch gdp)
                |> Option.defaultValue [||]
            phase "fix-jdf" "read-external-stops" "completed"
            resourceUsage "fix-jdf" "read-external-stops" 0L
            phase "fix-jdf" "read-osm-stops" "started"
            let osmStopsToMatch =
                czPbf
                |> Option.map (fun pbf ->
                    Utils.logWrappedOp "Reading OSM stops" <| fun () ->
                        Osm.getCzOtherStops pbf
                        |> Osm.czOtherStopsForJdfMatch)
                |> Option.defaultValue [||]
            phase "fix-jdf" "read-osm-stops" "completed"
            resourceUsage "fix-jdf" "read-osm-stops" 0L
            use stopMatcher = new StopMatcher.StopMatcher<_>(
                Array.concat [ extStopsToMatch; osmStopsToMatch ])

            // The transit-geometry index is a persistent part of this stage's
            // working set. Snapshot memory only after it (and the stop matcher)
            // exist so the adaptive budget cannot start below its true baseline.
            let fixPlan = jobsFor "fix-jdf" Execution.FixBatches (4L * Execution.GiB)
            let jdfPar = Jdf.jdfBatchDirParser ()
            let jdfWri = Jdf.jdfBatchDirWriter ()
            JdfFixups.resetMatchDiagnostics ()
            Log.Information(
                "Fixing JDF with {Jobs} workers and {BatchOutput} batch output",
                fixPlan.resolvedWorkers, batchOutput)
            phase "fix-jdf" "process-batches" "started"
            let fixAdmissionBytes =
                adaptiveAdmissionBytes "fix-jdf"
            let results =
                Jdf.findJdfBatchPaths inDir
                |> Seq.sort
                |> Utils.mapParallelOrderedAdaptive
                    fixPlan.maximumWorkers
                    fixPlan.initialWorkers
                    fixPlan.memoryBudgetBytes
                    fixAdmissionBytes
                    (fun batchPath -> max (8L * Execution.MiB) (inputBytes batchPath * 3L))
                    (fun batchPath -> batchEvent "batch_started" "fix-jdf" batchPath None)
                    (schedulerSample "fix-jdf")
                    (fun batchPath ->
                let batchName = Path.GetFileNameWithoutExtension(batchPath)
                use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
                Log.Information("Processing JDF batch {BatchPath}", batchPath)
                try
                    let batch = Jdf.parseJdfBatchPath jdfPar batchPath
                    let routeFilter =
                        JdfToGtfs.applyInternationalRoutePolicy
                            internationalRoutePolicy batch
                    let inferredBatch =
                        match JdfFixups.dropDegenerateBatch batch with
                        | Some emptyBatch ->
                            Log.Warning(
                                "Dropping degenerate JDF batch with fewer than two distinct called stops")
                            emptyBatch
                        | None ->
                            // Route classification can change after batches with
                            // the same distinction are merged. Always geocode a
                            // non-degenerate source batch.
                            let batchFixed, stopMatches =
                                JdfFixups.fixPublicCisJrBatch stopMatcher batch
                            let stopsWithMatches =
                                Array.zip batchFixed.stops stopMatches
                                |> JdfFixups.rejectImplausibleMatches (batchFixed.tripStops |> Seq.toArray)
                            let retainedCandidateStops =
                                stopsWithMatches
                                |> Seq.choose (fun (stop, match_) ->
                                    match_ |> Option.map (fun _ -> stop.id))
                                |> Set
                            let batchFixed = {
                                batchFixed with
                                    postCandidateEvidence =
                                        batchFixed.postCandidateEvidence
                                        |> Array.filter (fun observation ->
                                            retainedCandidateStops.Contains(observation.stopId))
                            }
                            JdfModel.validatePostCandidateEvidence batchFixed.postCandidateEvidence
                            Seq.concat [
                                // Take one trip most likely to contain all stops'
                                // km distances (testing all takes too much time).
                                JdfFixups.checkMatchDistances
                                    (longestTripStops (batchFixed.tripStops |> Seq.toArray)) stopsWithMatches

                                JdfFixups.checkMissingRegionsCountries batchFixed
                            ]
                            |> Seq.iter (fun msg -> Log.Write(msg))
                            JdfFixups.addStopLocations batchFixed stopsWithMatches
                            |> JdfFixups.estimateMissingStopLocations
                    let batchWithLocations =
                        if collectEstimatedPostEvidence then inferredBatch
                        else { inferredBatch with
                                   postCandidateEvidence = [||] }

                    if batchOutput = "zip" then
                        let fixedOutPath = Path.Combine(outDir, batchName + ".zip")
                        Utils.writeAtomicFile fixedOutPath (fun temporary ->
                            use archive = ZipFile.Open(temporary, ZipArchiveMode.Create)
                            jdfWri (Jdf.ZipArchive archive) batchWithLocations)
                    else
                        let fixedOutDir = Path.Combine(outDir, batchName)
                        Directory.CreateDirectory(fixedOutDir) |> ignore
                        jdfWri (Jdf.FsPath fixedOutDir) batchWithLocations
                    Log.Information("Completed JDF batch {BatchPath}", batchPath)
                    Ok (batchPath, routeFilter.decisions)
                with error ->
                    Error (batchPath, error))
            let internationalRouteDecisions = ResizeArray<_>()
            for result in results do
                match result with
                | Ok (batchPath, decisions) ->
                    batchEvent "batch_completed" "fix-jdf" batchPath None
                    internationalRouteDecisions.AddRange(decisions)
                | Error (batchPath, error) ->
                    batchEvent "batch_failed" "fix-jdf" batchPath (Some error)
                    raise error
            phase "fix-jdf" "process-batches" "completed"
            resourceUsage "fix-jdf" "process-batches" 0L
            JdfFixups.logMatchDiagnostics ()
            JdfToGtfs.logInternationalRouteDecisions
                internationalRoutePolicy (internationalRouteDecisions.ToArray())
            Log.Information("Finished!")
        else if argFlagSet args "merge-jdf" then
            let outDir = argValue args "<JDF-out-dir>"
            let strict = argFlagSet args "--strict"
            let mergePlan = jobsFor "merge-jdf" Execution.MergeParsing (4L * Execution.GiB)
            let mutable gvdYear = 0
            if not (Int32.TryParse(argValue args "--gvd-year", &gvdYear)) || gvdYear < 2000 || gvdYear > 9999 then
                invalidArg "--gvd-year" "Expected a four-digit year"
            let referenceDate =
                let parsed = NodaTime.Text.LocalDatePattern.Iso.Parse(argValue args "--reference-date")
                if not parsed.Success then invalidArg "--reference-date" "Expected YYYY-MM-DD"
                parsed.Value
            let gvdStart, gvdEnd = Utils.gvdBounds gvdYear
            if referenceDate < gvdStart || referenceDate > gvdEnd then
                invalidArg "--reference-date" $"The reference date is outside GVD {gvdYear} ({gvdStart}..{gvdEnd})"

            let outPath = Path.GetFullPath(outDir)
            let outParent = Path.GetDirectoryName(outPath)
            Directory.CreateDirectory(outParent) |> ignore
            let spillPath =
                Path.Combine(
                    outParent,
                    $".{Path.GetFileName(outPath)}.trip-stops.{Guid.NewGuid():N}.tmp")

            use merger =
                new JdfMerger.JdfMerger(
                    JdfMerger.MergeStopsByName,
                    spillPath,
                    mergePlan.maximumWorkers)
            let jdfPar = Jdf.jdfBatchDirParser ()
            let mergeParseBytes =
                adaptiveAdmissionBytes "merge-jdf"
            Log.Information("Parsing merge inputs with {Jobs} workers", mergePlan.resolvedWorkers)
            phase "merge-jdf" "parse-batches" "started"
            let availableSpillBytes = DriveInfo(Path.GetPathRoot(outParent)).AvailableFreeSpace
            let minimumSpillReserve = Execution.GiB
            emitProgressEvent progressEvents "spill_preflight" [
                "stage", box "merge-jdf"
                "mode", box "progressive"
                "minimum_free_bytes", box minimumSpillReserve
                "available_bytes", box availableSpillBytes
            ]
            if availableSpillBytes < minimumSpillReserve then
                invalidOp (
                    $"Insufficient temporary disk for merge-jdf: require at least "
                    + $"{minimumSpillReserve} free bytes, available {availableSpillBytes}")
            let mergeInputs = seq {
                for inDir in argValues args "<JDF-in-dir>" do
                    for path in Jdf.findJdfBatchPaths inDir |> Seq.sort do
                        yield path, mergeInputWeight path
            }
            let parsedInputs =
                mergeInputs
                |> Utils.mapParallelOrderedAdaptive
                    mergePlan.maximumWorkers
                    mergePlan.initialWorkers
                    mergePlan.memoryBudgetBytes
                    mergeParseBytes
                    snd
                    (fun (batchPath, _) ->
                        batchEvent "batch_started" "merge-jdf" batchPath None)
                    (schedulerSample "merge-jdf")
                    (fun (batchPath, _) ->
                    try Ok (batchPath, Jdf.parseJdfBatchPath jdfPar batchPath)
                    with error -> Error (batchPath, error))
            for parsed in parsedInputs do
                match parsed with
                | Ok (batchPath, batch) ->
                    let batchName = Path.GetFileNameWithoutExtension(batchPath)
                    use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
                    Log.Information("Merging JDF batch {BatchPath}", batchPath)
                    try
                        merger.add(batch)
                        Log.Information("Completed merge of JDF batch {BatchPath}", batchPath)
                        batchEvent "batch_completed" "merge-jdf" batchPath None
                    with error ->
                        batchEvent "batch_failed" "merge-jdf" batchPath (Some error)
                        reraise ()
                | Error (batchPath, error) ->
                    batchEvent "batch_failed" "merge-jdf" batchPath (Some error)
                    Log.Error(error, "Error while processing {Batch}", batchPath)
                    if strict then raise error
            phase "merge-jdf" "parse-batches" "completed"
            merger.logStopMergeSummary()
            resourceUsage "merge-jdf" "parse-batches" merger.tripStopSpillBytes

            phase "merge-jdf" "resolve-route-overlaps" "started"
            Log.Information("Resolving route overlaps")
            merger.resolveRouteOverlaps()
            merger.boundValidity(referenceDate, gvdStart, gvdEnd)
            phase "merge-jdf" "resolve-route-overlaps" "completed"
            resourceUsage "merge-jdf" "resolve-route-overlaps" merger.tripStopSpillBytes

            phase "merge-jdf" "write-merged-jdf" "started"
            Log.Information("Writing merged JDF")
            merger.write(outDir)
            phase "merge-jdf" "write-merged-jdf" "completed"
            resourceUsage "merge-jdf" "write-merged-jdf" merger.tripStopSpillBytes
            Log.Information("Finished!")
        else printfn "%s" docstring
        exitCode
    )
