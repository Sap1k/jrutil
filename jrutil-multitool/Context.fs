// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// Shared command state: parsed arguments, execution planning and the
/// JRUTIL_PROGRESS event stream.
module JrUtil.Multitool.Context

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text.Json
open DocoptNet
open Serilog

open JrUtil
open JrUtil.ParallelUtils
open JrUtil.CliArgs

type Args = IDictionary<string, ArgValue>

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

/// Uncompressed size of a batch (ZIP entries, a file or a directory tree).
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

let internationalRoutePolicy args =
    optArgValue args "--international-route-policy"
    |> Option.defaultValue "keep-all"
    |> JdfGtfsRules.parseInternationalRoutePolicy

// Candidate discovery has to happen during fix-jdf, before the merged
// routing-demand relation (and therefore its clipped routing PBF) can
// exist.  Keep that switch independent from routed inference, which is
// only available to conversion commands once a routing PBF is supplied.
let estimatedPostActivation args =
    Execution.estimatedPostActivation
        (argFlagSet args "--no-estimated-posts")
        ((optArgValue args "--routing-osm-pbf").IsSome
         || (optArgValue args "--post-inference-evidence").IsSome)

let existingFile args name description =
    let path = optArgValue args name
    path
    |> Option.iter (fun path ->
        if not (File.Exists(path)) then
            invalidArg name $"{description} does not exist: {path}")
    path

type CommandContext(args: Args) =
    let progressEvents = argFlagSet args "--progress-events"
    let memoryRequest =
        optArgValue args "--memory-budget"
        |> Option.defaultValue "auto"
        |> Execution.parseMemoryBudget
    let processBudget =
        Execution.resolveMemoryBudget memoryRequest
        |> min Execution.ProductionProcessBudgetBytes
    let memoryRequest = Execution.FixedMemory processBudget
    do Log.Information("Memory target: process target {ProcessBudget} bytes for worker admission and spill decisions; the GC heap is not hard limited",
                       processBudget)
    let jobRequest =
        optArgValue args "--jobs"
        |> Option.defaultValue "auto"
        |> Execution.parseJobRequest
    let admissionBytesByStage = Dictionary<string, int64>()
    let emit eventName fields = emitProgressEvent progressEvents eventName fields

    member _.Args = args
    member _.ProcessBudget = processBudget
    member _.Emit eventName fields = emit eventName fields

    member _.JobsFor stage workload reservedBytes =
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
        emit "execution_plan" [
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

    /// Admission allowance computed by the last JobsFor call for the stage.
    member _.AdmissionBytes stage = admissionBytesByStage.[stage]

    member _.Phase stage name state =
        emit "phase" [
            "stage", box stage
            "name", box name
            "state", box state
        ]

    member _.BatchEvent eventName stage batchPath (error: exn option) =
        emit eventName (
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

    member _.ResourceUsage stage phase spillBytes =
        let currentProcess = Diagnostics.Process.GetCurrentProcess()
        currentProcess.Refresh()
        let gc = GC.GetGCMemoryInfo()
        emit "resource_usage" [
            "stage", box stage
            "phase", box phase
            "working_set_bytes", box currentProcess.WorkingSet64
            "peak_working_set_bytes", box currentProcess.PeakWorkingSet64
            "private_bytes", box currentProcess.PrivateMemorySize64
            "managed_heap_bytes", box gc.HeapSizeBytes
            "fragmented_bytes", box gc.FragmentedBytes
            "spill_bytes", box spillBytes
        ]

    member _.SchedulerSample stage (sample: ParallelUtils.AdaptiveSchedulerSample) =
        emit "scheduler_sample" (
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
