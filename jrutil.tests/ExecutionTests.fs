// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting

open JrUtil.Execution
open JrUtil.Tests.Asserts

[<TestClass>]
type ExecutionTests() =
    [<TestMethod>]
    member _.``Automatic memory budget leaves capacity headroom``() =
        assertEqual (10L * GiB) (autoMemoryBudgetBytes (16L * GiB) (14L * GiB))
        assertEqual (10L * GiB) (autoMemoryBudgetBytes (16L * GiB) (11L * GiB))
        assertEqual (24L * GiB) (autoMemoryBudgetBytes (32L * GiB) (30L * GiB))
        assertEqual (48L * GiB) (autoMemoryBudgetBytes (64L * GiB) (60L * GiB))

    [<TestMethod>]
    member _.``Automatic memory budget does not reject low instantaneous availability``() =
        assertEqual (10L * GiB) (autoMemoryBudgetBytes (16L * GiB) 0L)
        assertEqual GiB (autoMemoryBudgetBytes (6L * GiB) 0L)

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
    member _.``Worker count respects CPU requested jobs and stage allowance``() =
        assertEqual 1 (effectiveJobCount FixBatches 1 16 (32L * GiB) (4L * GiB))
        assertEqual 4 (effectiveJobCount FixBatches 8 4 (32L * GiB) (4L * GiB))
        assertEqual 4 (effectiveJobCount FixBatches 16 16 (6L * GiB) (4L * GiB))
        assertEqual 16 (effectiveJobCount BundleWork 32 32 (32L * GiB) (4L * GiB))

    [<TestMethod>]
    member _.``Worker plan exposes every limiting factor``() =
        let plan = workerPlan FixBatches 20 12 (9L * GiB) (4L * GiB)
        assertEqual 20 plan.requestedJobs
        assertEqual 12 plan.processorCount
        assertEqual (9L * GiB) plan.memoryBudgetBytes
        assertEqual (4L * GiB) plan.reservedBytes
        assertEqual (512L * MiB) plan.workerAllowanceBytes
        assertEqual 10 plan.memoryLimitedJobs
        assertEqual 10 plan.resolvedWorkers

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
