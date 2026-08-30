// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.Runtime

open System
open System.Diagnostics
open Serilog

let logProgress phase completed total =
    use currentProcess = Process.GetCurrentProcess()
    let privateMiB = currentProcess.PrivateMemorySize64 / 1024L / 1024L
    Log.Debug("Regional overlay memory: phase={Phase}; working_bytes={WorkingBytes}; private_bytes={PrivateBytes}; managed_bytes={ManagedBytes}; allocated_bytes={AllocatedBytes}; elapsed_ms={ElapsedMs}",
        phase, currentProcess.WorkingSet64, currentProcess.PrivateMemorySize64, GC.GetTotalMemory(false), GC.GetTotalAllocatedBytes(false), int64 (DateTime.Now - currentProcess.StartTime).TotalMilliseconds)
    match total with
    | Some totalRows ->
        Log.Information(
            "Regional overlay progress: phase={Phase}; rows={Completed}/{Total}; private_mib={PrivateMiB}",
            phase, completed, totalRows, privateMiB)
    | None ->
        Log.Information(
            "Regional overlay progress: phase={Phase}; rows={Completed}; private_mib={PrivateMiB}",
            phase, completed, privateMiB)

/// Matching creates a short-lived generation of call signatures and candidate indexes.
/// Kept as a single measured boundary; row processing never forces collections.
let releaseAnalysisMemory () =
    logProgress "before-release-analysis" 0L None
    GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced, true, true)
    logProgress "after-release-analysis" 1L None
