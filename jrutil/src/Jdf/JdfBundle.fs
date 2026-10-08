// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// JDF bundle pipeline: input to production package or evidence pack.
module JrUtil.JdfBundle

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
open JrUtil.JdfBundleInput
open JrUtil.JdfBundleTables
open JrUtil.JdfBundleEvidence
open JrUtil.JdfBundleManifest

let private logPhaseResources phase (timer: Stopwatch) =
    use currentProcess = Process.GetCurrentProcess()
    currentProcess.Refresh()
    let memory = GC.GetGCMemoryInfo()
    Log.Information(
        "Bundle resource snapshot: phase={Phase}; elapsed_ms={ElapsedMs}; private_bytes={PrivateBytes}; working_set_bytes={WorkingSetBytes}; peak_working_set_bytes={PeakWorkingSetBytes}; managed_heap_bytes={ManagedHeapBytes}; fragmented_bytes={FragmentedBytes}",
        phase, timer.ElapsedMilliseconds, currentProcess.PrivateMemorySize64,
        currentProcess.WorkingSet64, currentProcess.PeakWorkingSet64, GC.GetTotalMemory(false),
        memory.FragmentedBytes)

let private reportProgress (options: BundleOptions) (timer: Stopwatch)
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

let private capturePostInferenceEvidenceOnly snapshotDescriptorPath converterVersion
                                                   (internationalPolicy:JdfGtfsRules.InternationalRoutePolicy)
                                                   transportModeRules
                                                   routingPbfPath
                                                   (executionOptions:BundleOptions)
                                                   inputPath evidencePath =
    if String.IsNullOrWhiteSpace(converterVersion) then
        invalidArg "converterVersion" "Converter version is required"
    if JdfPostEvidence.isRestrictedCaptureToolVersion converterVersion then
        invalidArg "converterVersion" "Converter version must not contain a capture restriction marker"
    // A restricted pack records its restriction in the tool version, and
    // therefore in the pack ID.
    let captureToolVersion =
        converterVersion
        + JdfPostEvidence.captureRestrictionToolVersionSuffix executionOptions.captureRestriction
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
    graph.MaximumWorkers<-executionOptions.routingWorkers
    withJdfInput inputPath (fun source ->
        progress "parse-jdf" "started" 0L None "bytes" None 0
        let sourceBatch=Jdf.jdfBatchDirParser () source
        progress "parse-jdf" "completed" (int64 sourceBatch.tripStops.Count) None "calls" None 0
        progress "prepare-calendar" "started" 0L (Some(int64 sourceBatch.trips.Length)) "trips" None 0
        let sourceCalendar =
            JdfCalendar.prepareGtfsCalendarWithWorkersAndProgress
                executionOptions.maximumWorkers
                (fun count total ->
                    progress "calendar-trips" "running" count total "trips" None
                             executionOptions.maximumWorkers)
                sourceBatch
        progress "prepare-calendar" "completed" (int64 sourceBatch.trips.Length)
                 (Some(int64 sourceBatch.trips.Length)) "trips" None 0
        let filterResult =
            JdfInternationalFilter.applyInternationalRoutePolicyWithCalendarWorkersAndProgress
                executionOptions.maximumWorkers
                (fun phase count total ->
                    progress phase "running" count total "items" None executionOptions.maximumWorkers)
                internationalPolicy sourceCalendar sourceBatch
        JdfInternationalFilter.logInternationalRouteDecisions internationalPolicy filterResult.decisions
        let batch,_=JdfGtfsRules.applyTransportModeRules transportModeRules filterResult.batch
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
                         executionOptions.routingWorkers)
        progress "prepare-routing-snaps" "completed" (int64 graph.EdgeCount)
                 (Some(int64 graph.EdgeCount)) "edges" None 0
        let routingHash=sha256File routingPbfPath
        let routingCache =
            executionOptions.routingCachePath
            |> Option.map (fun directory ->
                JdfRoutingCache.RoutingContextCache(directory, graph))
        use captured =
            JdfPostEvidence.captureToStoreRestricted
                { maximumWorkers=executionOptions.routingWorkers
                  memoryBudgetBytes=executionOptions.memoryBudgetBytes
                  preflight=capturePreflight
                  progress=fun phase count total detail ->
                      progress phase "running" count total "items" detail
                               executionOptions.routingWorkers
                  routingCache=routingCache }
                executionOptions.captureRestriction
                graph batch
        routingCache |> Option.iter _.Save()
        progress "capture-post-inference-evidence" "started" 0L None "rows" None 0
        writePostEvidenceStore descriptor captureToolVersion evidencePath routingPbfPath captured
            (fun phase count total -> progress phase "running" count total "rows" None 1)
        use store=JdfPostEvidenceStore.openValidatedStore
                      { mergedJdfSha256=Some descriptor.payloadSha256
                        routingPbfSha256=Some routingHash
                        captureToolVersion=Some captureToolVersion }
                      evidencePath
        progress "capture-post-inference-evidence" "completed"
                 store.Manifest.routePointEvidenceCount
                 (Some store.Manifest.routePointEvidenceCount) "rows" None 0
        executionOptions.exportPostContextCallsPath |> Option.iter(fun path ->
            progress "capture-evidence-context-calls" "started" 0L None "calls" None 0
            let full=Path.GetFullPath(path)
            if File.Exists(full) || Directory.Exists(full) then
                invalidArg "exportPostContextCallsPath" $"Context-call export already exists: {full}"
            let temporary=full + $".tmp-{Guid.NewGuid():N}"
            try
                writePostContextCalls descriptor captureToolVersion routingHash store.Manifest.packId
                    executionOptions.captureRestriction batch temporary
                File.Move(temporary,full)
            finally
                if File.Exists(temporary) then File.Delete(temporary)
            progress "capture-evidence-context-calls" "completed" 1L (Some 1L) "files" None 0)
        CaptureCompleted(
            store.Manifest,
            { estimatedEvidenceBytes=estimatedBytes
              atomicOutputHeadroomBytes=requiredFreeBytes
              currentSpillBytes=captured.CurrentSpillBytes
              peakSpillBytes=captured.PeakSpillBytes
              maximumWorkers=executionOptions.maximumWorkers }))

/// Pin inferred post ordinals to the stop registry when one is configured,
/// and write the newly numbered posts for review.
let private applyPostRegistry (options: BundleOptions) (plan: JdfPostPlan.PostEstimationPlan) =
    match options.stopRegistry with
    | None -> plan
    | Some registry ->
        let pinned, candidates = JdfPostPlan.applyStopRegistry registry plan
        Log.Information(
            "Stop registry {Sha256}: {Inferred} inferred post locations, {New} new ordinals",
            registry.sha256, plan.inferredLocations.Length, candidates.Length)
        options.stopRegistryCandidatesPath
        |> Option.iter (fun path ->
            StopRegistry.writeCsv path StopRegistry.postCandidatesHeader candidates)
        pinned

let private writeBundleCore (executionOptions: BundleOptions) inputPath outputPath =
    let snapshotDescriptorPath = executionOptions.snapshotDescriptorPath
    let converterVersion = executionOptions.converterVersion
    let internationalPolicy = executionOptions.internationalPolicy
    let transportModeRules = executionOptions.transportModeRules
    let estimatedPosts = executionOptions.estimatedPosts
    let routingPbfPath = executionOptions.routingPbfPath
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
    if executionOptions.postInferenceEvidenceOnly then
        invalidArg "executionOptions" "Capture-only execution must use the dedicated capture dispatcher"
    if executionOptions.capturePostInferenceEvidencePath.IsSome
       && not executionOptions.postInferenceEvidenceOnly then
        invalidArg "executionOptions"
            "Evidence capture currently requires --post-inference-evidence-only so policy evaluation cannot contaminate the captured route points"
    if executionOptions.capturePostInferenceEvidencePath.IsSome
       && (not estimatedPosts || routingPbfPath.IsNone) then
        invalidArg "executionOptions" "Post-inference evidence capture requires routed estimation and --routing-osm-pbf"
    let livePolicy =
        let policy,scorer = policyAndScorer executionOptions.postInferencePolicyPath
        Some(JdfPostInferencePolicy.validatePolicy JdfPostInference.CaptureRoutedExcessHorizonMetres policy,scorer)
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
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "bundle-output"
    Directory.CreateDirectory(temp) |> ignore
    // Tables and files only the optional diagnostics artifact needs.
    let diagnosticScratch = Path.Combine(temp, "diagnostics")
    let writeDiagnosticTables = executionOptions.diagnosticsOutput.IsSome
    let mutable packageInput: Serving.CompilerOutput.Output option = None
    let mutable completed = false
    let mutable liveEvidenceTemporaryDirectory:string option=None
    let nativeCallPath = temp + ".trip_call.parquet"
    let mutable nativeCallCount = 0L
    let mutable nativeRelations = Map.empty
    let nativeTransferSequences = Dictionary<struct(string * int64), int>()
    // PBF decoding and JDF parsing are independent and predominantly use
    // different resources. Starting the graph build here hides most of the
    // parse/calendar/filter latency without changing either result.
    let routingGraphTask =
        match estimatedPosts, routingPbfPath with
        | true, Some path ->
            validateRoutingPbfManifest path
            Task.Run(fun () ->
                let graph =
                    GeoData.Osm.PackedRoutingGraph.OpenWithProgress(
                        path,
                        (fun phase count total ->
                            reportProgress executionOptions phaseTimer phase "running"
                                           count total "items" None 1),
                        buildGlobalSnaps=false)
                graph.MaximumWorkers <- executionOptions.routingWorkers
                Some graph)
        | _ -> Task.FromResult(None)
    let mutable routingGraph: GeoData.Osm.PackedRoutingGraph option = None
    progressCompleted "validate-snapshot" 1L (Some 1L) "items"
    try
        withJdfInput inputPath (fun source ->
            started "parse-jdf" None "bytes"
            Log.Information("Bundle phase: parsing merged JDF")
            use calls = new JdfCallStore.Store(parent, Threading.CancellationToken.None)
            let sourceBatch =
                Jdf.jdfCompilationParser calls
                    (fun count -> reportProgress executionOptions phaseTimer "parse-jdf" "running" count None "calls" None 1)
                    source
            logPhaseResources "parse-jdf" phaseTimer
            progressCompleted "parse-jdf" (int64 sourceBatch.tripStops.Count) None "calls"
            started "prepare-calendar" (Some (int64 sourceBatch.trips.Length)) "trips"
            let sourceCalendar =
                JdfCalendar.prepareGtfsCalendarWithWorkersAndProgress
                    executionOptions.maximumWorkers
                    (fun count total ->
                        reportProgress executionOptions phaseTimer "calendar-trips" "running"
                                       count total "trips" None executionOptions.maximumWorkers)
                    sourceBatch
            progressCompleted "prepare-calendar" (int64 sourceBatch.trips.Length)
                              (Some (int64 sourceBatch.trips.Length)) "trips"
            started "filter-international" (Some (int64 sourceBatch.routes.Length)) "routes"
            Log.Information("Bundle phase: applying international trip policy")
            let sourceTransportModes =
                sourceBatch.routes
                |> Seq.map (fun route -> (route.id, route.idDistinction), route.transportMode)
                |> Map
            let filterResult =
                JdfInternationalFilter.applyInternationalRoutePolicyWithCalendarWorkersAndProgress
                    executionOptions.maximumWorkers
                    (fun phase count total ->
                        reportProgress executionOptions phaseTimer phase "running"
                                       count total "items" None executionOptions.maximumWorkers)
                    internationalPolicy sourceCalendar sourceBatch
            JdfInternationalFilter.logInternationalRouteDecisions internationalPolicy filterResult.decisions
            let correctedBatch, transportModeDecisions =
                JdfGtfsRules.applyTransportModeRules transportModeRules filterResult.batch
            let batch =
                if estimatedPosts then correctedBatch
                else { correctedBatch with
                           postCandidateEvidence = [||] }
            let retainedCalendar =
                lazy(JdfCalendar.filterCalendarPreparation batch sourceCalendar)
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
                                       count total "edges" None executionOptions.routingWorkers)
                progressCompleted "prepare-routing-snaps" (int64 graph.EdgeCount)
                                  (Some (int64 graph.EdgeCount)) "edges")
            started "prepare-inference" None "contexts"
            Log.Information("Bundle phase: preparing streaming JDF to GTFS conversion")
            let preparation =
                match routingGraph with
                | Some graph ->
                    let progress phase count total detail =
                        reportProgress executionOptions phaseTimer phase "running"
                                       count total "items" detail executionOptions.routingWorkers
                    // The routing pass emits raw route-point facts into a
                    // temporary evidence pack, the same contract capture
                    // writes; every consolidation and publication decision is
                    // made by the evaluator below.
                    let routingPath=routingPbfPath.Value
                    let routingHash=sha256File routingPath
                    let routingCache =
                        executionOptions.routingCachePath
                        |> Option.map (fun directory ->
                            JdfRoutingCache.RoutingContextCache(directory, graph))
                    use evidenceStore =
                        JdfPostEvidence.captureToStore
                            { maximumWorkers=executionOptions.routingWorkers
                              memoryBudgetBytes=executionOptions.memoryBudgetBytes
                              preflight=ignore
                              progress=progress
                              routingCache=routingCache }
                            graph batch
                    routingCache |> Option.iter _.Save()
                    let evidencePath =
                        match executionOptions.capturePostInferenceEvidencePath with
                        | Some path -> path
                        | None ->
                            let path=Path.Combine(Path.GetTempPath(),$"jrutil-post-evidence-{Guid.NewGuid():N}")
                            liveEvidenceTemporaryDirectory<-Some path
                            path
                    started "capture-post-inference-evidence" None "rows"
                    writePostEvidenceStore descriptor converterVersion evidencePath routingPath evidenceStore
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
                        let policy,scorer = livePolicy.Value
                        JdfPostInferenceEvaluator.evaluateWithScorer false evidenceStore policy scorer
                        |> JdfPostPlan.postEstimationPlanFromInferenceResult
                        |> applyPostRegistry executionOptions
                    progressCompleted "evaluate-post-inference" manifest.routePointEvidenceCount
                                      (Some manifest.routePointEvidenceCount) "rows"
                    JdfToGtfs.prepareGtfsFeedForStreamingBundleWithCalendarAndPostPlan
                        retainedCalendar.Value postPlan batch
                | None ->
                    JdfToGtfs.prepareGtfsFeedForStreamingBundleWithCalendar
                        retainedCalendar.Value batch
            routingGraph |> Option.iter (fun graph ->
                Log.Information(
                    "Routing metrics: mapped_bytes={MappedBytes}; nodes={Nodes}; edges={Edges}; searches={Searches}; cache_entries={CacheEntries}; cache_hits={CacheHits}; cache_misses={CacheMisses}; restriction_lookups={RestrictionLookups}; restriction_rules_examined={RestrictionRulesExamined}",
                    graph.EstimatedMappedBytes, graph.NodeCount, graph.EdgeCount,
                    graph.Searches, graph.CacheEntries, graph.CacheHits, graph.CacheMisses,
                    graph.RestrictionLookups, graph.RestrictionRulesExamined))
            logPhaseResources "prepare-inference" phaseTimer
            progressCompleted "prepare-inference" (int64 preparation.postPlan.calls.Count) None "contexts"
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
                |> Seq.map (fun trip -> JdfGtfsRules.jdfTripId batch trip.routeId trip.routeDistinction trip.id)
                |> Seq.filter (preparation.tripsToDelete.Contains >> not)
                |> HashSet
            started "prepare-call-diagnostics" (Some(int64 batch.tripStops.Count)) "calls"
            let callFacts =
                scanCallDerivedFacts batch retainedTripIdsForCalls candidateStopsWithMultiple (fun count total ->
                    reportProgress executionOptions phaseTimer "prepare-call-diagnostics" "running"
                                   count total "calls" None 1)
            progressCompleted "prepare-call-diagnostics" (int64 batch.tripStops.Count)
                              (Some(int64 batch.tripStops.Count)) "calls"
            let referencedStopIds = HashSet<string>(StringComparer.Ordinal)
            let mutable stopTimeCount = 0
            let transferCallQueries = HashSet<struct (string * int64)>()
            for transfer in batch.transfers do
                transferCallQueries.Add(
                    struct (JdfGtfsRules.jdfTripId batch transfer.routeId transfer.routeDistinction transfer.tripId,
                            transfer.routeStopId))
                |> ignore
            let emittedTransferCalls = HashSet<struct (string * int64)>()
            let bufferedCalls = executionOptions.maximumWorkers > 1 && executionOptions.memoryBudgetBytes >= 512L * 1024L * 1024L
            use nativeCalls = new JrUtil.Serving.TripCallWriter.Writer(nativeCallPath, Threading.CancellationToken.None, bufferedOutput = bufferedCalls)
            let parents =
                JdfToGtfs.getGtfsStopsWithPlan preparation.postPlan batch
                |> Seq.map (fun stop -> stop.id, stop.parentStation)
                |> dict
            let stopTimes =
                JdfToGtfs.getStreamingBundleStopTimeRows preparation
                |> Seq.map (fun row ->
                    let stopTime = row.stopTime
                    referencedStopIds.Add(stopTime.stopId) |> ignore
                    stopTimeCount <- stopTimeCount + 1
                    let parent = parents.[stopTime.stopId]
                    let location = parent |> Option.defaultValue stopTime.stopId
                    let boarding = if parent.IsSome then stopTime.stopId else null
                    // The version-scoped JDF route-stop key; the package post-pass maps it to the
                    // line's ordered route_stop slot and its zones.
                    let routeStop = JrUtil.Serving.Identity.routeStopKey row.routeId (string row.sourceRouteVersion) (string row.sourceRouteStopId)
                    nativeCalls.Append(JrUtil.Serving.TripCallWriter.fromGtfs stopTime location boarding routeStop)
                    let transferKey = struct (stopTime.tripId, row.sourceRouteStopId)
                    if transferCallQueries.Contains(transferKey) then
                        emittedTransferCalls.Add(transferKey) |> ignore
                        nativeTransferSequences.TryAdd(transferKey, stopTime.stopSequence) |> ignore
                    if stopTimeCount % 250_000 = 0 then
                        reportProgress executionOptions phaseTimer "stream-stop-times" "running"
                                       (int64 stopTimeCount) (Some callFacts.emittedCallCount)
                                       "rows" None (1 + nativeCalls.ActiveCompressionWorkers)
                    stopTime)
            Log.Information("Bundle phase: streaming GTFS stop times")
            started "stream-stop-times" (Some callFacts.emittedCallCount) "calls"
            // Calls stream once into the native trip_call; gtfs.zip projects it.
            stopTimes |> Seq.iter ignore
            nativeCallCount <- nativeCalls.Complete()
            logPhaseResources "stream-stop-times" phaseTimer
            progressCompleted "stream-stop-times" (int64 stopTimeCount) (Some callFacts.emittedCallCount) "rows"
            Log.Information("Bundle phase: preparing remaining GTFS relations")
            started "prepare-remaining-gtfs" (Some 1L) "feeds"
            let feed, presentationChanges =
                JdfToGtfs.finishStreamingFeedWithUniqueCalendars preparation (referencedStopIds |> Set.ofSeq)
                |> JdfToGtfs.applyRoutePresentationRules executionOptions.routePresentationRules
            let feed = Gtfs.fillStandardRequiredFields feed
            logPhaseResources "prepare-remaining-gtfs" phaseTimer
            progressCompleted "prepare-remaining-gtfs" 1L (Some 1L) "feeds"
            validateStopCoordinates feed
            let nativeTripPath = temp + ".trip.parquet"
            let nativeTripCount =
                JrUtil.Serving.TripWriter.write nativeTripPath Threading.CancellationToken.None
                    (fun phase count -> reportProgress executionOptions phaseTimer ("trips-" + phase) "running" count None "rows" None 1)
                    feed.trips
            nativeRelations <- nativeRelations |> Map.add "trip" (nativeTripPath, nativeTripCount)
            started "write-relations" None "tables"
            Log.Information("Bundle phase: preparing Parquet relations")
            let tables, assignmentCount, assignmentRows, nativeNotes, nativeFeatures =
                getTableProducers sourceTransportModes batch feed preparation.postPlan callFacts
                    (fun phase count total ->
                        reportProgress executionOptions phaseTimer phase "running"
                                       count total "rows" None 1)
                    emittedTransferCalls
            let notePath = temp + ".service_note.parquet"
            let noteCount =
                JrUtil.Serving.NoteWriter.write notePath Threading.CancellationToken.None
                    (fun phase count -> reportProgress executionOptions phaseTimer phase "running" count None "rows" None 1)
                    (nativeNotes ())
            nativeRelations <- nativeRelations |> Map.add "service_note" (notePath, noteCount)
            // Note links and trip features share `assignment`.
            let assignmentPath = temp + ".assignment.parquet"
            let servingAssignmentCount =
                JrUtil.Serving.AssignmentWriter.write assignmentPath Threading.CancellationToken.None
                    (fun phase count -> reportProgress executionOptions phaseTimer ("assignments-" + phase) "running" count None "rows" None 1)
                    (Seq.append (nativeNotes () |> Seq.map (JrUtil.Serving.NoteWriter.assignment "jdf")) (nativeFeatures ()))
            nativeRelations <- nativeRelations |> Map.add "assignment" (assignmentPath, servingAssignmentCount)
            // Source metadata tables feed the package writer; post-inference
            // tables are diagnostics and are only produced for the artifact.
            let tables =
                tables |> Array.filter (fun (name, _) ->
                    name.StartsWith("source_", StringComparison.Ordinal) || writeDiagnosticTables)
            if writeDiagnosticTables then Directory.CreateDirectory(diagnosticScratch) |> ignore
            let tablePath (name: string) =
                if name.StartsWith("source_", StringComparison.Ordinal) then Path.Combine(temp, name)
                else Path.Combine(diagnosticScratch, name)
            let totalParquetTables = tables.Length + (if writeDiagnosticTables then 1 else 0)
            for index, (name, produceTable) in tables |> Array.indexed do
                reportProgress executionOptions phaseTimer "write-parquet" "running"
                               (int64 index) (Some (int64 totalParquetTables)) "tables" (Some name) 1
                let parquetTable = produceTable ()
                Log.Information(
                    "Bundle phase: writing Parquet table {Index}/{Total}: {Table} ({Rows} rows)",
                    index + 1, totalParquetTables, name, parquetTable.rows.Count)
                writeParquet descriptor (tablePath name) parquetTable
                reportProgress executionOptions phaseTimer "write-parquet" "running"
                               (int64 (index+1)) (Some (int64 totalParquetTables)) "tables" (Some name) 1
            if writeDiagnosticTables then
                let assignmentTableName="derived_post_assignments.parquet"
                Log.Information(
                    "Bundle phase: streaming Parquet table {Table} ({Rows} rows)",
                    assignmentTableName,assignmentCount)
                writeDerivedPostAssignmentsParquet descriptor (Path.Combine(diagnosticScratch,assignmentTableName))
                    assignmentCount assignmentRows
                    (fun count total ->
                        reportProgress executionOptions phaseTimer "stream-derived-post-assignments" "running"
                                       count total "rows" None 1)
                |> ignore
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
                    { severity = "warning"
                      code = if decision.keep then "filtered_international_trips" else "filtered_international_route"
                      sourceObjectId = JdfGtfsRules.jdfSourceRouteId decision.routeId decision.routeDistinction
                      message = $"{decision.reason}; countries={countries}; maximum_trip_span_km={span}; maximum_foreign_depth_km={depth}; integrated={decision.integrated}; domestic={decision.retainedDomesticTrips}; qualifying_cross_border={decision.qualifyingCrossBorderTrips}; rejected_cross_border={decision.rejectedCrossBorderTrips}; foreign_only={decision.foreignOnlyTrips}" })
            let transportModeDiagnostics =
                transportModeDecisions
                |> Seq.map (fun decision ->
                    { severity = if decision.corrected then "info" else "warning"
                      code = if decision.corrected then "corrected_transport_mode" else "transport_mode_rule_mismatch"
                      sourceObjectId = JdfGtfsRules.jdfSourceRouteId decision.routeId decision.routeDistinction
                      message = $"{decision.message}; effective_mode={decision.effectiveMode}" })
            let presentationDiagnostics =
                presentationChanges
                |> Seq.map (fun change ->
                    let text value = defaultArg value ""
                    { severity = "info"; code = "route_presentation_override"
                      sourceObjectId = change.routeId
                      message = $"{change.field}: {text change.before} -> {text change.after}" })
            let bundleDiagnostics =
                Seq.concat [ diagnostics batch feed callFacts emittedTransferCalls :> seq<_>
                             internationalDiagnostics
                             transportModeDiagnostics
                             presentationDiagnostics ]
                |> Seq.sortBy (fun diagnostic -> diagnostic.code, diagnostic.sourceObjectId)
                |> Seq.toArray
            let missingCoordinateCount =
                bundleDiagnostics
                |> Seq.filter (fun diagnostic -> diagnostic.code = "missing_stop_coordinates")
                |> Seq.length
            if missingCoordinateCount > 0 then
                Log.Warning("{MissingCoordinateCount} referenced stop places have unresolved coordinates and were serialized as 0,0",
                            missingCoordinateCount)
            let diagnosticsText = serializeJson (fun stream -> writeDiagnostics stream bundleDiagnostics)
            progressCompleted "write-diagnostics" 1L (Some 1L) "files"
            Log.Information("Bundle phase: creating manifest")
            let manifestText =
                serializeJson (fun stream ->
                    writeManifest stream descriptor converterVersion
                                  internationalPolicy filterResult.decisions transportModeRules
                                  transportModeDecisions executionOptions.routePresentationRules
                                  presentationChanges preparation.postPlan routingPbfPath
                                  liveEvidenceTemporaryDirectory
                                  executionOptions.postInferencePolicyPath
                                  batch feed)
                |> fun text -> recordManifestGvd text executionOptions.gvdYear
            let memoryTables entries =
                entries
                |> Seq.map (fun (name, header, rows) -> name, Serving.CompilerOutput.memoryTable header rows)
            let standard, czech = Gtfs.feedTables feed
            let diagnosticFiles = Dictionary<string, string>(StringComparer.Ordinal)
            if Directory.Exists(diagnosticScratch) then
                for path in Directory.EnumerateFiles(diagnosticScratch) do
                    diagnosticFiles.["post-inference/" + Path.GetFileName(path)] <- path
            packageInput <- Some {
                feed = "jdf"
                gtfs =
                    standard |> Seq.filter (fun (name, _, _) -> name <> "stop_times.txt") |> memoryTables
                    |> Serving.CompilerOutput.tables
                czech = czech |> memoryTables |> Serving.CompilerOutput.tables
                mappings = Serving.CompilerOutput.noTables
                reports = Serving.CompilerOutput.noTables
                sidecars =
                    Directory.EnumerateFiles(temp, "*.parquet")
                    |> Seq.map (fun path -> Path.GetFileNameWithoutExtension(path), Serving.CompilerOutput.parquetFileTable path)
                    |> Serving.CompilerOutput.tables
                manifest = jsonElement manifestText
                diagnostics = Some (jsonElement diagnosticsText)
                basePackage = None
                diagnosticFiles = diagnosticFiles :> IReadOnlyDictionary<_, _> })
        Log.Information("Bundle phase: activating completed bundle")
        JdfPostInferencePolicy.PostInferencePhaseProbe.record "activation"
        let productionTemp = temp + ".production"
        let input = packageInput.Value
        JrUtil.Serving.PackageWriter.writePackage (nativeRelations |> Map.add "trip_call" (nativeCallPath, int nativeCallCount))
            (Some { transferSequences = nativeTransferSequences })
            (fun phase count -> reportProgress executionOptions phaseTimer phase "running" count None "items" None 1)
            input productionTemp
        executionOptions.diagnosticsOutput
        |> Option.iter (fun output ->
            JrUtil.Serving.PackageWriter.writeDiagnosticArtifact input output executionOptions.diagnosticTraces)
        Directory.Delete(temp, true)
        Directory.Move(productionTemp, outputFull)
        completed <- true
    finally
        routingGraph |> Option.iter (fun graph -> (graph :> IDisposable).Dispose())
        // If an earlier conversion phase failed while the concurrent graph
        // builder was still running, join it and release its temporary maps.
        // This prevents abandoned multi-gigabyte conversion-local artifacts.
        if not routingGraphTask.IsFaulted && not routingGraphTask.IsCanceled then
            try
                routingGraphTask.GetAwaiter().GetResult()
                |> Option.iter (fun graph -> (graph :> IDisposable).Dispose())
            with _ -> ()
        if not completed && Directory.Exists(temp) then
            Log.Error("JDF compilation failed; scratch retained at {ScratchPath}", temp)
        liveEvidenceTemporaryDirectory
        |> Option.iter(fun path ->
            if completed && Directory.Exists(path) then Directory.Delete(path,true)
            elif Directory.Exists(path) then Log.Error("Inference evidence retained at {EvidencePath}", path))
    BundleCompleted

/// Compile a JDF batch (directory or ZIP) into a production package, or, in
/// capture-only mode, into a post-inference evidence pack.
let execute (options: BundleOptions) inputPath outputPath =
    if options.maximumWorkers <= 0 then
        invalidArg "options" "Bundle maximum workers must be positive"
    if options.routingWorkers <= 0 then
        invalidArg "options" "Bundle routing workers must be positive"
    if options.memoryBudgetBytes <= 0L then
        invalidArg "options" "Bundle memory budget must be positive"
    let executionMode =
        match options.postInferenceEvidenceOnly,options.routingPbfPath,options.estimatedPosts with
        | true,Some _,true -> CaptureOnly
        | false,Some _,true -> Live
        | false,None,_ -> Disabled
        | _ -> invalidArg "options" "Invalid post-inference execution-mode combination"
    match executionMode with
    | CaptureOnly ->
        if options.postInferencePolicyPath.IsSome then
            invalidArg "options" "Capture-only execution cannot load a policy"
        let evidencePath =
            options.capturePostInferenceEvidencePath
            |> Option.defaultWith(fun () ->
                invalidArg "options"
                    "Capture-only execution requires --capture-post-inference-evidence=DIR")
        capturePostInferenceEvidenceOnly options.snapshotDescriptorPath options.converterVersion
            options.internationalPolicy options.transportModeRules options.routingPbfPath.Value
            options inputPath evidencePath
    | _ ->
        if options.capturePostInferenceEvidencePath.IsSome then
            invalidArg "options"
                "--capture-post-inference-evidence is valid only in capture-only execution"
        if options.captureRestriction<>JdfPostEvidence.noCaptureRestriction
           || options.exportPostContextCallsPath.IsSome then
            invalidArg "options"
                "--capture-stop-region, --capture-exclude-source and --export-post-context-calls are valid only in capture-only execution"
        writeBundleCore options inputPath outputPath
