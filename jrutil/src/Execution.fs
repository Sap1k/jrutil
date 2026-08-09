// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.Execution

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions

[<Literal>]
let KiB = 1024L
[<Literal>]
let MiB = 1024L * 1024L
[<Literal>]
let GiB = 1024L * 1024L * 1024L

type MemoryBudget =
    | AutoMemory
    | FixedMemory of bytes: int64

type StorageMode =
    | SpillFirst
    | AdaptiveMemory

type Workload =
    | FixBatches
    | MergeParsing
    | BundleWork

type JobRequest =
    | AutoJobs
    | FixedJobs of count: int

type WorkerPlan = {
    workload: Workload
    requestedJobs: int
    processorCount: int
    memoryBudgetBytes: int64
    reservedBytes: int64
    workerAllowanceBytes: int64
    memoryLimitedJobs: int
    resolvedWorkers: int
    initialWorkers: int
    maximumWorkers: int
}

type AdaptiveState = {
    targetWorkers: int
    admissionPaused: bool
}

type AdaptiveSample = {
    privateBytes: int64
    normalizedCpuPercent: float
    workQueued: bool
    backlogHealthy: bool
}

type MemorySnapshot = {
    effectiveTotalBytes: int64
    availableBytes: int64
    processPrivateBytes: int64
}

[<StructLayout(LayoutKind.Sequential)>]
type private MemoryStatusEx =
    struct
        val mutable length: uint32
        val mutable memoryLoad: uint32
        val mutable totalPhysical: uint64
        val mutable availablePhysical: uint64
        val mutable totalPageFile: uint64
        val mutable availablePageFile: uint64
        val mutable totalVirtual: uint64
        val mutable availableVirtual: uint64
        val mutable availableExtendedVirtual: uint64
    end

[<DllImport("kernel32.dll", SetLastError = true)>]
extern bool private GlobalMemoryStatusEx(MemoryStatusEx& status)

let private windowsMemory () =
    let mutable status = MemoryStatusEx()
    status.length <- uint32 (Marshal.SizeOf<MemoryStatusEx>())
    if GlobalMemoryStatusEx(&status) then
        Some (int64 status.totalPhysical, int64 status.availablePhysical)
    else None

let private linuxMemory () =
    if not (File.Exists("/proc/meminfo")) then None else
    let values =
        File.ReadLines("/proc/meminfo")
        |> Seq.choose (fun line ->
            let parts = line.Split(':', 2)
            if parts.Length <> 2 then None else
            let number = parts.[1].Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries).[0]
            match Int64.TryParse(number, NumberStyles.None, CultureInfo.InvariantCulture) with
            | true, kib -> Some (parts.[0], kib * KiB)
            | _ -> None)
        |> Map
    match values |> Map.tryFind "MemTotal", values |> Map.tryFind "MemAvailable" with
    | Some total, Some available -> Some (total, available)
    | _ -> None

let detectEffectiveMemoryBytes () =
    let physical =
        if OperatingSystem.IsWindows() then windowsMemory ()
        elif OperatingSystem.IsLinux() then linuxMemory ()
        else None
    let gcInfo = GC.GetGCMemoryInfo()
    let gcLimit = gcInfo.TotalAvailableMemoryBytes
    match physical with
    | Some (total, available) when gcLimit > 0L && gcLimit < total * 7L / 10L ->
        let gcAvailable = max 0L (gcLimit - gcInfo.MemoryLoadBytes)
        gcLimit, min available gcAvailable
    | Some values -> values
    | None when gcLimit > 0L ->
        gcLimit, max 0L (gcLimit - gcInfo.MemoryLoadBytes)
    | None -> 8L * GiB, 6L * GiB

let automaticMemoryReserveBytes effectiveTotalBytes =
    if effectiveTotalBytes <= 0L then
        invalidArg "effectiveTotalBytes" "Effective total memory must be positive"

    min (4L * GiB) (max GiB (effectiveTotalBytes / 4L))

let autoMemoryBudgetBytes effectiveTotalBytes availableBytes processPrivateBytes =
    if effectiveTotalBytes <= 0L then
        invalidArg "effectiveTotalBytes" "Effective total memory must be positive"
    if availableBytes < 0L then
        invalidArg "availableBytes" "Available memory must not be negative"
    if processPrivateBytes < 0L then
        invalidArg "processPrivateBytes" "Process memory must not be negative"

    // Keep the former capacity ceiling, but never assume that capacity is
    // currently free. Available memory excludes the process's existing private
    // bytes, so add only the portion left after reserving RAM for the OS and
    // other applications to the current process footprint.
    let capacityBudget =
        if effectiveTotalBytes <= 20L * GiB then
            min (10L * GiB) (effectiveTotalBytes - 6L * GiB)
        else
            effectiveTotalBytes - max (8L * GiB) (effectiveTotalBytes / 4L)
    let capacityBudget = max 1L capacityBudget
    let available = min effectiveTotalBytes availableBytes
    let allocatable = max 0L (available - automaticMemoryReserveBytes effectiveTotalBytes)
    let liveBudget = processPrivateBytes + allocatable

    min capacityBudget liveBudget
    |> max processPrivateBytes
    |> max 1L

let detectMemorySnapshot () =
    let total, available = detectEffectiveMemoryBytes ()
    use currentProcess = Process.GetCurrentProcess()
    currentProcess.Refresh()
    {
        effectiveTotalBytes = total
        availableBytes = max 0L (min total available)
        processPrivateBytes = max 0L currentProcess.PrivateMemorySize64
    }

let parseMemoryBudget (value: string) =
    if value.Equals("auto", StringComparison.OrdinalIgnoreCase) then AutoMemory
    else
        let parsed = Regex.Match(value.Trim(), "^([0-9]+(?:\\.[0-9]+)?)(KiB|MiB|GiB)$",
                                 RegexOptions.IgnoreCase)
        if not parsed.Success then
            invalidArg "value" "Memory budget must be 'auto' or a size such as 10GiB"
        let amount = Decimal.Parse(parsed.Groups.[1].Value, CultureInfo.InvariantCulture)
        let multiplier =
            match parsed.Groups.[2].Value.ToUpperInvariant() with
            | "KIB" -> decimal KiB
            | "MIB" -> decimal MiB
            | "GIB" -> decimal GiB
            | _ -> failwith "unreachable"
        let bytes = int64 (amount * multiplier)
        if bytes <= 0L then invalidArg "value" "Memory budget must be positive"
        FixedMemory bytes

let parseJobRequest (value: string) =
    if value.Equals("auto", StringComparison.OrdinalIgnoreCase) then AutoJobs
    else
        match Int32.TryParse(value, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, count when count > 0 -> FixedJobs count
        | _ -> invalidArg "value" "Jobs must be 'auto' or a positive integer"

let resolveMemoryBudgetFromSnapshot request snapshot =
    match request with
    | FixedMemory bytes -> bytes
    | AutoMemory ->
        autoMemoryBudgetBytes
            snapshot.effectiveTotalBytes
            snapshot.availableBytes
            snapshot.processPrivateBytes

let resolveMemoryBudget request =
    resolveMemoryBudgetFromSnapshot request (detectMemorySnapshot ())

let aggressiveAutoMaximum processorCount =
    min 256 (max 32 (processorCount * 8))

let requestedJobCount request =
    match request with
    | AutoJobs -> aggressiveAutoMaximum Environment.ProcessorCount
    | FixedJobs count -> count

let initialJobCount maximum processorCount =
    min maximum (max 4 (processorCount * 2))

let advanceAdaptiveState (memoryBudget: int64) (maximumWorkers: int)
                         (state: AdaptiveState) (sample: AdaptiveSample) =
    let memoryRatio = float sample.privateBytes / float memoryBudget
    if memoryRatio >= 0.95 then
        { targetWorkers = max 1 (state.targetWorkers / 2); admissionPaused = true }
    elif state.admissionPaused && memoryRatio >= 0.80 then
        state
    elif memoryRatio > 0.85 then
        { targetWorkers = max 1 (state.targetWorkers * 3 / 4); admissionPaused = false }
    elif memoryRatio >= 0.80 || sample.normalizedCpuPercent >= 90.0
         || not sample.workQueued || not sample.backlogHealthy then
        { state with admissionPaused = false }
    else
        { targetWorkers =
            min maximumWorkers
                (state.targetWorkers + max 2 (state.targetWorkers / 4))
          admissionPaused = false }

let storageModeForBudget budgetBytes =
    if budgetBytes <= 12L * GiB then SpillFirst else AdaptiveMemory

let private workerAllowance = function
    // Fix and merge use live process-memory and per-input byte admission.
    | FixBatches | MergeParsing -> 0L
    | BundleWork -> 128L * MiB

let workerPlan workload requestedJobs processorCount budgetBytes reservedBytes =
    if requestedJobs <= 0 then invalidArg "requestedJobs" "Requested jobs must be positive"
    if processorCount <= 0 then invalidArg "processorCount" "Processor count must be positive"
    let usable = max 0L (budgetBytes - reservedBytes)
    let allowance = workerAllowance workload
    let memoryJobs =
        if allowance = 0L then requestedJobs
        else max 1 (int (usable / allowance))
    {
        workload = workload
        requestedJobs = requestedJobs
        processorCount = processorCount
        memoryBudgetBytes = budgetBytes
        reservedBytes = reservedBytes
        workerAllowanceBytes = allowance
        memoryLimitedJobs = memoryJobs
        resolvedWorkers = min requestedJobs memoryJobs |> max 1
        initialWorkers = initialJobCount (min requestedJobs memoryJobs) processorCount
        maximumWorkers = min requestedJobs memoryJobs
    }

let effectiveJobCount workload requestedJobs processorCount budgetBytes reservedBytes =
    (workerPlan workload requestedJobs processorCount budgetBytes reservedBytes).resolvedWorkers
