// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open System.Diagnostics
open System.Runtime

/// Growth-gated reclamation for long streaming phases. A forced compacting
/// collection releases dead buffers but does not return all committed memory,
/// so a plain "private bytes above threshold" check keeps firing at every
/// checkpoint after the first trigger, running blocking full collections of a
/// multi-gigabyte heap each time. This gate reclaims only when private memory
/// is above the threshold *and* has grown by at least `minimumGrowthBytes`
/// since the level measured after the previous reclamation. Reclamation never
/// limits the heap or rejects live data.
type MemoryReclaimGate(thresholdBytes: int64, minimumGrowthBytes: int64, reclaim: unit -> unit,
                       privateBytes: unit -> int64) =
    let mutable baseline = 0L
    let mutable reclaims = 0

    new(thresholdBytes, minimumGrowthBytes, reclaim) =
        let current () =
            use processInfo = Process.GetCurrentProcess()
            processInfo.PrivateMemorySize64
        MemoryReclaimGate(thresholdBytes, minimumGrowthBytes, reclaim, current)

    member _.Reclaims = reclaims

    /// Returns true when a reclamation ran.
    member _.Check() =
        let current = privateBytes ()
        if current >= thresholdBytes && current >= baseline + minimumGrowthBytes then
            reclaim ()
            reclaims <- reclaims + 1
            baseline <- privateBytes ()
            true
        else false

module MemoryReclaim =
    let DefaultMinimumGrowthBytes = 512L * 1024L * 1024L

    let compactingCollection () =
        GCSettings.LargeObjectHeapCompactionMode <- GCLargeObjectHeapCompactionMode.CompactOnce
        GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced, true, true)

    let gate thresholdBytes = MemoryReclaimGate(thresholdBytes, DefaultMinimumGrowthBytes, compactingCollection)
