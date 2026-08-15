// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting

open JrUtil.Execution
open JrUtil.Tests.Asserts

[<TestClass>]
type ExecutionTests() =
    [<TestMethod>]
    member _.``Candidate collection does not require a routing PBF``() =
        assertEqual
            { collectEvidence = true; runRoutedInference = false }
            (estimatedPostActivation false false)
        assertEqual
            { collectEvidence = true; runRoutedInference = true }
            (estimatedPostActivation false true)
        assertEqual
            { collectEvidence = false; runRoutedInference = false }
            (estimatedPostActivation true true)

    [<TestMethod>]
    member _.``Automatic memory budget is relaxed on low-memory hosts``() =
        let baseline = 512L * MiB
        assertEqual (53L * GiB / 4L)
            (autoMemoryBudgetBytes (16L * GiB) (14L * GiB) baseline)
        assertEqual (47L * GiB / 4L)
            (autoMemoryBudgetBytes (16L * GiB) (11L * GiB) baseline)
        assertEqual (24L * GiB)
            (autoMemoryBudgetBytes (32L * GiB) (30L * GiB) baseline)
        assertEqual (48L * GiB)
            (autoMemoryBudgetBytes (64L * GiB) (60L * GiB) baseline)

    [<TestMethod>]
    member _.``Automatic memory budget includes bounded evictable memory``() =
        assertEqual (33L * GiB / 4L)
            (autoMemoryBudgetBytes (16L * GiB) (4L * GiB) (512L * MiB))
        assertEqual (61L * GiB / 16L)
            (autoMemoryBudgetBytes (6L * GiB) 0L (256L * MiB))

    [<TestMethod>]
    member _.``Low-memory reserve and eviction allowance remain bounded``() =
        assertEqual (768L * MiB) (automaticMemoryReserveBytes (6L * GiB))
        assertEqual GiB (automaticMemoryReserveBytes (8L * GiB))
        assertEqual (2L * GiB) (automaticMemoryReserveBytes (16L * GiB))
        assertEqual (4L * GiB) (automaticMemoryReserveBytes (32L * GiB))
        assertEqual (69L * GiB / 16L)
            (automaticEvictableAllowanceBytes (6L * GiB) 0L (256L * MiB))
        assertEqual 0L
            (automaticEvictableAllowanceBytes (32L * GiB) (4L * GiB) GiB)

    [<TestMethod>]
    member _.``Sixteen GiB auto budget contracts as other applications consume RAM``() =
        let baseline = 512L * MiB
        let abundant = autoMemoryBudgetBytes (16L * GiB) (14L * GiB) baseline
        let constrained = autoMemoryBudgetBytes (16L * GiB) (8L * GiB) baseline
        let exhausted = autoMemoryBudgetBytes (16L * GiB) (3L * GiB) baseline
        assertEqual (53L * GiB / 4L) abundant
        assertEqual (41L * GiB / 4L) constrained
        assertEqual (31L * GiB / 4L) exhausted

    [<TestMethod>]
    member _.``Memory budget parser accepts binary units and auto``() =
        assertEqual AutoMemory (parseMemoryBudget "auto")
        assertEqual (FixedMemory (10L * GiB)) (parseMemoryBudget "10GiB")
        assertEqual (FixedMemory (1536L * MiB)) (parseMemoryBudget "1536MiB")
        Assert.ThrowsExactly<ArgumentException>(fun () -> parseMemoryBudget "0GiB" |> ignore)
        |> ignore

    [<TestMethod>]
    member _.``Storage mode changes only above low-memory ceiling``() =
        assertEqual SpillFirst (storageModeForBudget (10L * GiB))
        assertEqual SpillFirst (storageModeForBudget (12L * GiB))
        assertEqual AdaptiveMemory (storageModeForBudget (12L * GiB + 1L))

    [<TestMethod>]
    member _.``Explicit worker count is not capped by CPU count``() =
        assertEqual 1 (effectiveJobCount FixBatches 1 16 (32L * GiB) (4L * GiB))
        assertEqual 8 (effectiveJobCount FixBatches 8 4 (32L * GiB) (4L * GiB))
        assertEqual 16 (effectiveJobCount FixBatches 16 4 (6L * GiB) (4L * GiB))
        assertEqual 32 (effectiveJobCount BundleWork 32 4 (32L * GiB) (4L * GiB))

    [<TestMethod>]
    member _.``Automatic worker ceiling aggressively oversubscribes processors``() =
        assertEqual 32 (aggressiveAutoMaximum 1)
        assertEqual 96 (aggressiveAutoMaximum 12)
        assertEqual 256 (aggressiveAutoMaximum 64)
        assertEqual 24 (initialJobCount 96 12)
        assertEqual 12 (initialJobCount 12 12)

    [<TestMethod>]
    member _.``Worker plan exposes every limiting factor``() =
        let plan = workerPlan FixBatches 20 12 (9L * GiB) (4L * GiB)
        assertEqual 20 plan.requestedJobs
        assertEqual 12 plan.processorCount
        assertEqual (9L * GiB) plan.memoryBudgetBytes
        assertEqual (4L * GiB) plan.reservedBytes
        assertEqual 0L plan.workerAllowanceBytes
        assertEqual 20 plan.memoryLimitedJobs
        assertEqual 20 plan.resolvedWorkers
        assertEqual 20 plan.initialWorkers
        assertEqual 20 plan.maximumWorkers

    [<TestMethod>]
    member _.``Merge worker ceiling delegates memory admission to input weights``() =
        let plan = workerPlan MergeParsing 12 12 GiB (4L * GiB)
        assertEqual 0L plan.workerAllowanceBytes
        assertEqual 12 plan.memoryLimitedJobs
        assertEqual 12 plan.resolvedWorkers

    [<TestMethod>]
    member _.``Adaptive controller applies CPU and memory hysteresis``() =
        let state target paused = { targetWorkers = target; admissionPaused = paused }
        let sample ratio cpu queued healthy = {
            privateBytes = int64 (ratio * float GiB)
            normalizedCpuPercent = cpu
            workQueued = queued
            backlogHealthy = healthy
        }
        assertEqual (state 25 false)
            (advanceAdaptiveState GiB 96 (state 20 false) (sample 0.70 40.0 true true))
        assertEqual (state 20 false)
            (advanceAdaptiveState GiB 96 (state 20 false) (sample 0.70 90.0 true true))
        assertEqual (state 20 false)
            (advanceAdaptiveState GiB 96 (state 20 false) (sample 0.82 20.0 true true))
        assertEqual (state 15 false)
            (advanceAdaptiveState GiB 96 (state 20 false) (sample 0.90 20.0 true true))
        assertEqual (state 10 true)
            (advanceAdaptiveState GiB 96 (state 20 false) (sample 0.96 20.0 true true))
        assertEqual (state 10 true)
            (advanceAdaptiveState GiB 96 (state 10 true) (sample 0.85 20.0 true true))
        assertEqual (state 12 false)
            (advanceAdaptiveState GiB 96 (state 10 true) (sample 0.79 20.0 true true))

    [<TestMethod>]
    member _.``Bounded parallel map preserves input order``() =
        let results =
            [0..11]
            |> JrUtil.Utils.mapParallelOrderedBatches 4 (fun value ->
                Thread.Sleep((3 - value % 4) * 5)
                value * value)
            |> Seq.toArray
        assertEqual ([| for value in 0..11 -> value * value |]) results

    [<TestMethod>]
    member _.``Bounded parallel map never exceeds requested concurrency``() =
        let gate = obj()
        let mutable running = 0
        let mutable maximum = 0
        let work value =
            lock gate (fun () ->
                running <- running + 1
                maximum <- max maximum running)
            try
                Thread.Sleep(15)
                value
            finally
                lock gate (fun () -> running <- running - 1)
        [0..19]
        |> JrUtil.Utils.mapParallelOrderedBatches 3 work
        |> Seq.iter ignore
        Assert.IsTrue(maximum <= 3, $"Observed {maximum} concurrent workers")
        Assert.IsTrue(maximum > 1, "The test did not observe concurrent execution")

    [<TestMethod>]
    member _.``Bounded parallel map replenishes a rolling window``() =
        use releaseSecond = new ManualResetEventSlim(false)
        use thirdStarted = new ManualResetEventSlim(false)
        let processing =
            Task.Run(fun () ->
                [0..3]
                |> JrUtil.Utils.mapParallelOrderedBatches 2 (fun value ->
                    if value = 1 then releaseSecond.Wait()
                    if value = 2 then thirdStarted.Set()
                    value)
                |> Seq.toArray)
        try
            Assert.IsTrue(
                thirdStarted.Wait(TimeSpan.FromSeconds(2.0)),
                "The next input was not scheduled while the ordered head was blocked")
        finally
            releaseSecond.Set()
        assertEqual [|0; 1; 2; 3|] processing.Result

    [<TestMethod>]
    member _.``Bounded parallel map reports failures in input order``() =
        let values =
            [0..3]
            |> JrUtil.Utils.mapParallelOrderedBatches 3 (fun value ->
                if value = 1 then raise (InvalidOperationException("first"))
                if value = 2 then raise (InvalidOperationException("second"))
                value)
        use enumerator = values.GetEnumerator()
        assertEqual true (enumerator.MoveNext())
        assertEqual 0 enumerator.Current
        let error = Assert.ThrowsExactly<InvalidOperationException>(fun () -> enumerator.MoveNext() |> ignore)
        assertEqual "first" error.Message

    [<TestMethod>]
    member _.``Weighted parallel map preserves order and byte bound``() =
        let gate = obj()
        let mutable activeWeight = 0L
        let mutable maximumWeight = 0L
        let weights = [| 3L; 4L; 6L; 2L; 5L |]
        let work index =
            lock gate (fun () ->
                activeWeight <- activeWeight + weights.[index]
                maximumWeight <- max maximumWeight activeWeight)
            try
                Thread.Sleep((weights.Length - index) * 5)
                index * index
            finally
                lock gate (fun () -> activeWeight <- activeWeight - weights.[index])
        let result =
            [0 .. weights.Length - 1]
            |> JrUtil.Utils.mapParallelOrderedWeighted 4 10L (fun index -> weights.[index]) work
            |> Seq.toArray
        assertEqual [|0; 1; 4; 9; 16|] result
        Assert.IsTrue(maximumWeight <= 10L, $"Observed {maximumWeight} bytes in flight")

    [<TestMethod>]
    member _.``Weighted parallel map admits one oversized input``() =
        let result =
            [0; 1; 2]
            |> JrUtil.Utils.mapParallelOrderedWeighted 3 10L (fun index -> if index = 1 then 20L else 4L) id
            |> Seq.toArray
        assertEqual [|0; 1; 2|] result

    [<TestMethod>]
    member _.``Weighted parallel map reports failures in input order``() =
        let values =
            [0..3]
            |> JrUtil.Utils.mapParallelOrderedWeighted 3 10L (fun _ -> 3L) (fun value ->
                if value = 1 then raise (InvalidOperationException("first"))
                if value = 2 then raise (InvalidOperationException("second"))
                value)
        use enumerator = values.GetEnumerator()
        assertEqual true (enumerator.MoveNext())
        assertEqual 0 enumerator.Current
        let error = Assert.ThrowsExactly<InvalidOperationException>(fun () -> enumerator.MoveNext() |> ignore)
        assertEqual "first" error.Message

    [<TestMethod>]
    member _.``Adaptive map continuously replenishes independently of consumer``() =
        use thirdStarted = new ManualResetEventSlim(false)
        let values =
            [0..7]
            |> JrUtil.Utils.mapParallelOrderedAdaptive
                4 2 (1024L * GiB) 64L (fun _ -> 1L) ignore ignore (fun value ->
                    if value = 2 then thirdStarted.Set()
                    Thread.Sleep(20)
                    value)
        use enumerator = values.GetEnumerator()
        assertEqual true (enumerator.MoveNext())
        assertEqual 0 enumerator.Current
        Assert.IsTrue(
            thirdStarted.Wait(TimeSpan.FromSeconds(2.0)),
            "Workers were not replenished while ordered consumption was paused")
        let remaining = ResizeArray<int>()
        while enumerator.MoveNext() do remaining.Add(enumerator.Current)
        assertEqual [|1; 2; 3; 4; 5; 6; 7|] (remaining.ToArray())

    [<TestMethod>]
    member _.``Adaptive map does not force-admit behind a blocked consumer``() =
        let mutable started = 0
        let values =
            [0..99]
            |> JrUtil.Utils.mapParallelOrderedAdaptive
                2 2 (1024L * GiB) 1024L (fun _ -> 1L) ignore ignore (fun value ->
                    Interlocked.Increment(&started) |> ignore
                    Thread.Sleep(5)
                    value)
        use enumerator = values.GetEnumerator()
        assertEqual true (enumerator.MoveNext())
        Thread.Sleep(250)
        Assert.IsTrue(started <= 6, $"Blocked consumer admitted {started} inputs")

    [<TestMethod>]
    member _.``Adaptive map preserves order concurrency and deterministic failures``() =
        let gate = obj()
        let mutable active = 0
        let mutable maximum = 0
        let values =
            [0..7]
            |> JrUtil.Utils.mapParallelOrderedAdaptive
                6 4 (1024L * GiB) 64L (fun _ -> 1L) ignore ignore (fun value ->
                    lock gate (fun () ->
                        active <- active + 1
                        maximum <- max maximum active)
                    try
                        Thread.Sleep((8 - value) * 4)
                        if value = 2 then raise (InvalidOperationException("first"))
                        if value = 3 then raise (InvalidOperationException("second"))
                        value
                    finally
                        lock gate (fun () -> active <- active - 1))
        use enumerator = values.GetEnumerator()
        assertEqual true (enumerator.MoveNext())
        assertEqual 0 enumerator.Current
        assertEqual true (enumerator.MoveNext())
        assertEqual 1 enumerator.Current
        let error = Assert.ThrowsExactly<InvalidOperationException>(fun () -> enumerator.MoveNext() |> ignore)
        assertEqual "first" error.Message
        Assert.IsTrue(maximum > 1 && maximum <= 4, $"Observed {maximum} workers")

    [<TestMethod>]
    member _.``Adaptive map admits one oversized input``() =
        let result =
            [0; 1; 2]
            |> JrUtil.Utils.mapParallelOrderedAdaptive
                3 3 (1024L * GiB) 10L
                (fun value -> if value = 1 then 20L else 4L)
                ignore ignore id
            |> Seq.toArray
        assertEqual [|0; 1; 2|] result

    [<TestMethod>]
    member _.``Adaptive map charges the current result until the consumer advances``() =
        use secondStarted = new ManualResetEventSlim(false)
        let values =
            [0; 1]
            |> JrUtil.Utils.mapParallelOrderedAdaptive
                2 2 (1024L * GiB) 10L (fun _ -> 8L) ignore ignore (fun value ->
                    if value = 1 then secondStarted.Set()
                    value)
        use enumerator = values.GetEnumerator()
        assertEqual true (enumerator.MoveNext())
        assertEqual 0 enumerator.Current
        Assert.IsFalse(
            secondStarted.Wait(TimeSpan.FromMilliseconds(200.0)),
            "The yielded result was uncharged before the consumer advanced")
        assertEqual true (enumerator.MoveNext())
        assertEqual 1 enumerator.Current
        Assert.IsTrue(secondStarted.IsSet)

    [<TestMethod>]
    member _.``Atomic file output preserves destination and removes failed temporary``() =
        let root = Path.Combine(Path.GetTempPath(), $"jrutil-atomic-{Guid.NewGuid():N}")
        Directory.CreateDirectory(root) |> ignore
        let destination = Path.Combine(root, "result.txt")
        try
            File.WriteAllText(destination, "old")
            Assert.ThrowsExactly<InvalidOperationException>(fun () ->
                JrUtil.Utils.writeAtomicFile destination (fun temporary ->
                    File.WriteAllText(temporary, "partial")
                    raise (InvalidOperationException("stop"))))
            |> ignore
            assertEqual "old" (File.ReadAllText(destination))
            assertEqual 0 (Directory.EnumerateFiles(root, "*.part") |> Seq.length)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)
