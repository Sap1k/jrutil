// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.Execution

open System
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

let autoMemoryBudgetBytes effectiveTotalBytes _availableBytes =
    if effectiveTotalBytes <= 0L then
        invalidArg "effectiveTotalBytes" "Effective total memory must be positive"

    // Base automatic budgets on machine/process capacity, not the instantaneous
    // available-physical-memory counter. In particular, Windows can reclaim
    // standby pages and use its page file, so treating that counter as a hard
    // admission limit rejects work that the OS can run normally.
    let capacityBudget =
        if effectiveTotalBytes <= 20L * GiB then
            min (10L * GiB) (effectiveTotalBytes - 6L * GiB)
        else
            effectiveTotalBytes - max (8L * GiB) (effectiveTotalBytes / 4L)
    // Do not add another minimum-memory gate here. Bounded workers can still
    // make progress with a small budget, and an explicit OS/GC limit remains
    // represented by effectiveTotalBytes.
    max GiB capacityBudget

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

let resolveMemoryBudget request =
    match request with
    | FixedMemory bytes -> bytes
    | AutoMemory ->
        let total, available = detectEffectiveMemoryBytes ()
        autoMemoryBudgetBytes total available

let requestedJobCount request =
    match request with
    | AutoJobs -> Environment.ProcessorCount
    | FixedJobs count -> count

let storageModeForBudget budgetBytes =
    if budgetBytes <= 12L * GiB then SpillFirst else AdaptiveMemory

let private workerAllowance = function
    | FixBatches -> 512L * MiB
    | MergeParsing -> 256L * MiB
    | BundleWork -> 128L * MiB

let workerPlan workload requestedJobs processorCount budgetBytes reservedBytes =
    if requestedJobs <= 0 then invalidArg "requestedJobs" "Requested jobs must be positive"
    if processorCount <= 0 then invalidArg "processorCount" "Processor count must be positive"
    let usable = max 0L (budgetBytes - reservedBytes)
    let allowance = workerAllowance workload
    let memoryJobs = max 1 (int (usable / allowance))
    {
        workload = workload
        requestedJobs = requestedJobs
        processorCount = processorCount
        memoryBudgetBytes = budgetBytes
        reservedBytes = reservedBytes
        workerAllowanceBytes = allowance
        memoryLimitedJobs = memoryJobs
        resolvedWorkers =
            [requestedJobs; processorCount; 16; memoryJobs]
            |> List.min
            |> max 1
    }

let effectiveJobCount workload requestedJobs processorCount budgetBytes reservedBytes =
    (workerPlan workload requestedJobs processorCount budgetBytes reservedBytes).resolvedWorkers
