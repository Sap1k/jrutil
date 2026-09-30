// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// Ordered parallel mapping with adaptive, memory-aware admission.
module JrUtil.ParallelUtils

open System
open System.IO
open System.Diagnostics
open System.Globalization
open System.Collections
open System.Collections.Concurrent
open System.Runtime.InteropServices
open System.Runtime.Serialization.Formatters.Binary
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open DocoptNet
open Serilog
open Serilog.Core
open Serilog.Events
open Serilog.Sinks.SystemConsole.Themes
open Serilog.Formatting.Compact
open NodaTime
open NodaTime.Text
#nowarn "0342"
open System

let internal mapParallelOrderedCore<'a, 'b>
    degreeOfParallelism
    (maximumInFlightBytes: int64 option)
    (weight: 'a -> int64)
    (func: 'a -> 'b)
    (inputs: 'a seq) = seq {
    if degreeOfParallelism <= 0 then
        invalidArg "degreeOfParallelism" "Parallelism must be positive"
    maximumInFlightBytes
    |> Option.iter (fun maximum ->
        if maximum <= 0L then
            invalidArg "maximumInFlightBytes" "The in-flight byte bound must be positive")
    if degreeOfParallelism = 1 then
        for input in inputs do yield func input
    else
        use enumerator = inputs.GetEnumerator()
        let tasks = Queue<Task<'b> * int64>()
        let mutable pending: ('a * int64) option = None
        let mutable inFlightBytes = 0L
        let nextInput () =
            match pending with
            | Some value ->
                pending <- None
                Some value
            | None when enumerator.MoveNext() ->
                let input = enumerator.Current
                Some (input, max 1L (weight input))
            | None -> None
        let enqueueNext () =
            match nextInput () with
            | None -> false
            | Some (input, inputWeight) ->
                let withinByteLimit =
                    match maximumInFlightBytes with
                    | None -> true
                    | Some maximum -> tasks.Count = 0 || inFlightBytes + inputWeight <= maximum
                if withinByteLimit then
                    tasks.Enqueue(Task.Run<'b>(fun () -> func input), inputWeight)
                    inFlightBytes <- inFlightBytes + inputWeight
                    true
                else
                    pending <- Some (input, inputWeight)
                    false
        let mutable filling = true
        while tasks.Count < degreeOfParallelism && filling do
            filling <- enqueueNext ()
        while tasks.Count > 0 do
            let mutable task, taskWeight = tasks.Dequeue()
            let mutable result = task.GetAwaiter().GetResult()
            inFlightBytes <- inFlightBytes - taskWeight
            yield result
            // Sequence state survives across the yield. Clear the completed task
            // and its potentially large parsed result before replenishing the
            // window so the bound applies to retained results as well as workers.
            result <- Unchecked.defaultof<'b>
            task <- null
            filling <- true
            while tasks.Count < degreeOfParallelism && filling do
                filling <- enqueueNext ()
}

/// Maintains a rolling bounded task window and yields results in input order.
/// A slow input can delay later results, but never allows more than the
/// requested number of parsed results to be retained.
let mapParallelOrderedBatches<'a, 'b> degreeOfParallelism (func: 'a -> 'b) (inputs: 'a seq) =
    mapParallelOrderedCore degreeOfParallelism None (fun _ -> 1L) func inputs

type AdaptiveSchedulerSample = {
    targetWorkers: int
    activeWorkers: int
    maximumActiveWorkers: int
    completedBacklog: int
    retainedEstimatedBytes: int64
    privateBytes: int64
    workingSetBytes: int64
    managedHeapBytes: int64
    normalizedCpuPercent: float
    admissionPaused: bool
    queuedWork: int
    throughputPerSecond: float
    pauseReason: string option
}

/// Runs an independently replenished adaptive task producer while exposing
/// results in deterministic input order. Estimated bytes remain charged until
/// the ordered consumer releases a result.
let mapParallelOrderedAdaptive<'a, 'b>
    maximumWorkers initialWorkers memoryBudgetBytes maximumRetainedBytes
    (weight: 'a -> int64)
    (onAdmit: 'a -> unit)
    (onSample: AdaptiveSchedulerSample -> unit)
    (func: 'a -> 'b)
    (inputs: 'a seq) = seq {
    if maximumWorkers <= 0 then invalidArg "maximumWorkers" "Maximum workers must be positive"
    if initialWorkers <= 0 || initialWorkers > maximumWorkers then
        invalidArg "initialWorkers" "Initial workers must be within the worker limit"
    if memoryBudgetBytes <= 0L || maximumRetainedBytes <= 0L then
        invalidArg "memoryBudgetBytes" "Memory limits must be positive"

    let gate = obj()
    let completed = Dictionary<int, struct (int64 * Choice<'b, exn>)>()
    let mutable active = 0
    let mutable maximumActive = 0
    let mutable retainedBytes = 0L
    let mutable admittedCount = 0
    let mutable exhausted = false
    let mutable cancelled = false
    let mutable targetWorkers = initialWorkers
    let mutable admissionPaused = false
    let mutable completedTotal = 0L
    let mutable nextToConsume = 0

    let producer =
        Thread(ThreadStart(fun () ->
            use enumerator = inputs.GetEnumerator()
            use currentProcess = Process.GetCurrentProcess()
            let mutable pending: (int * 'a * int64) option = None
            let mutable lastSample = Stopwatch.GetTimestamp()
            let mutable lastCpu = currentProcess.TotalProcessorTime
            let mutable lastCompleted = 0L
            let sampleIntervalTicks = Stopwatch.Frequency
            let sample now =
                currentProcess.Refresh()
                let elapsed = float (now - lastSample) / float Stopwatch.Frequency
                let cpuNow = currentProcess.TotalProcessorTime
                let cpu =
                    if elapsed <= 0.0 then 0.0 else
                    (cpuNow - lastCpu).TotalSeconds / elapsed
                    / float Environment.ProcessorCount * 100.0
                lastSample <- now
                lastCpu <- cpuNow
                let workQueued = pending.IsSome || not exhausted
                let memoryRatio =
                    float currentProcess.PrivateMemorySize64 / float memoryBudgetBytes
                let backlogHealthy =
                    retainedBytes <= maximumRetainedBytes / 2L
                    && completed.Count < max 1 targetWorkers
                let mutable pauseReason = None
                if memoryRatio >= 0.95 then
                    targetWorkers <- max 1 (targetWorkers / 2)
                    admissionPaused <- true
                    pauseReason <- Some "memory-pause"
                elif admissionPaused && memoryRatio >= 0.80 then
                    pauseReason <- Some "memory-hysteresis"
                elif memoryRatio > 0.85 then
                    targetWorkers <- max 1 (targetWorkers * 3 / 4)
                    admissionPaused <- false
                    pauseReason <- Some "memory-reduction"
                elif memoryRatio >= 0.80 || cpu >= 90.0
                     || not workQueued || not backlogHealthy then
                    admissionPaused <- false
                    pauseReason <-
                        if memoryRatio >= 0.80 then Some "memory-hold"
                        elif cpu >= 90.0 then Some "cpu-saturated"
                        elif not backlogHealthy then Some "ordered-backlog"
                        else Some "no-queued-work"
                else
                    targetWorkers <-
                        min maximumWorkers
                            (targetWorkers + max 2 (targetWorkers / 4))
                    admissionPaused <- false
                let throughput = float (completedTotal - lastCompleted) / max elapsed 0.001
                lastCompleted <- completedTotal
                onSample {
                    targetWorkers = targetWorkers
                    activeWorkers = active
                    maximumActiveWorkers = maximumActive
                    completedBacklog = completed.Count
                    retainedEstimatedBytes = retainedBytes
                    privateBytes = currentProcess.PrivateMemorySize64
                    workingSetBytes = currentProcess.WorkingSet64
                    managedHeapBytes = GC.GetGCMemoryInfo().HeapSizeBytes
                    normalizedCpuPercent = cpu
                    admissionPaused = admissionPaused
                    queuedWork = if workQueued then 1 else 0
                    throughputPerSecond = throughput
                    pauseReason = pauseReason
                }

            let nextInput () =
                match pending with
                | Some value -> Some value
                | None when enumerator.MoveNext() ->
                    let ordinal = admittedCount
                    let input = enumerator.Current
                    Some (ordinal, input, max 1L (weight input))
                | None ->
                    exhausted <- true
                    None

            let rec loop () =
                let mutable startWork: (int * 'a * int64) list = []
                lock gate (fun () ->
                    let now = Stopwatch.GetTimestamp()
                    if now - lastSample >= sampleIntervalTicks then sample now
                    let mutable filling = true
                    while not cancelled && filling && active < targetWorkers do
                        match nextInput () with
                        | None -> filling <- false
                        | Some (ordinal, input, inputWeight) ->
                            let forceProgress = active = 0 && ordinal = nextToConsume
                            let withinBytes = retainedBytes + inputWeight <= maximumRetainedBytes
                            let backlogHealthy =
                                retainedBytes <= maximumRetainedBytes / 2L
                                && completed.Count < max 1 targetWorkers
                            if forceProgress
                               || (not admissionPaused && withinBytes && backlogHealthy) then
                                pending <- None
                                admittedCount <- admittedCount + 1
                                retainedBytes <- retainedBytes + inputWeight
                                active <- active + 1
                                maximumActive <- max maximumActive active
                                startWork <- (ordinal, input, inputWeight) :: startWork
                            else
                                pending <- Some (ordinal, input, inputWeight)
                                filling <- false
                    if startWork.IsEmpty && not cancelled && (not exhausted || active > 0) then
                        Monitor.Wait(gate, 100) |> ignore)
                for ordinal, input, inputWeight in List.rev startWork do
                    onAdmit input
                    Task.Run(fun () ->
                        let result =
                            try Choice1Of2 (func input)
                            with error -> Choice2Of2 error
                        lock gate (fun () ->
                            completed.[ordinal] <- struct (inputWeight, result)
                            completedTotal <- completedTotal + 1L
                            active <- active - 1
                            Monitor.PulseAll(gate)))
                    |> ignore
                let shouldContinue =
                    lock gate (fun () -> not cancelled && (not exhausted || active > 0))
                if shouldContinue then loop () else lock gate (fun () -> Monitor.PulseAll(gate))
            loop ()))
    producer.IsBackground <- true
    producer.Name <- "jrutil-adaptive-producer"
    producer.Start()
    try
        let mutable ordinal = 0
        let mutable running = true
        while running do
            let result =
                lock gate (fun () ->
                    while not (completed.ContainsKey(ordinal))
                          && not (exhausted && active = 0 && ordinal >= admittedCount) do
                        Monitor.Wait(gate) |> ignore
                    match completed.TryGetValue(ordinal) with
                    | true, struct (inputWeight, value) ->
                        completed.Remove(ordinal) |> ignore
                        Some struct (inputWeight, value)
                    | _ -> None)
            match result with
            | Some struct (inputWeight, Choice1Of2 value) ->
                // Keep the handed-off result charged while the caller is
                // processing it. The producer may replenish other capacity,
                // but cannot treat the current parsed batch as already freed.
                yield value
                lock gate (fun () ->
                    retainedBytes <- retainedBytes - inputWeight
                    ordinal <- ordinal + 1
                    nextToConsume <- ordinal
                    Monitor.PulseAll(gate))
            | Some struct (inputWeight, Choice2Of2 error) ->
                lock gate (fun () ->
                    retainedBytes <- retainedBytes - inputWeight
                    ordinal <- ordinal + 1
                    nextToConsume <- ordinal
                    Monitor.PulseAll(gate))
                raise error
            | None -> running <- false
    finally
        lock gate (fun () ->
            cancelled <- true
            Monitor.PulseAll(gate))
        producer.Join()
}

// Used DateTime to parse and the converts the result to LocalDate
