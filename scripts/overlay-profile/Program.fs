// Compile this runner before profiling; do not run concurrent builds or audits.

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Text.Json
open Serilog
open Serilog.Core
open JrUtil

let run (args: string array) =
    if args.Length <> 8 then invalidArg "args" "Expected POLICY YEAR SOURCE ZIP DESCRIPTOR BASE OUTPUT YYYYMMDD"
    let policy, year, source, zip, descriptor, baseline, output, audit = args.[0], int args.[1], args.[2], args.[3], args.[4], args.[5], args.[6], args.[7]
    let auditDate = NodaTime.LocalDate(int (audit.Substring(0, 4)), int (audit.Substring(4, 2)), int (audit.Substring(6, 2)))
    use metrics = new StreamWriter(output + ".memory.csv", false)
    metrics.AutoFlush <- true
    metrics.WriteLine("elapsed_ms,phase,working_set,private_bytes,managed_bytes,heap_size,allocated_bytes,scratch_bytes")
    let watch = Stopwatch.StartNew()
    let mutable phase, scratch = "start", ""
    let mutable peakWorking, peakPrivate, peakManaged, peakScratch = 0L, 0L, 0L, 0L
    let record () = lock metrics (fun () ->
        use current = Process.GetCurrentProcess()
        let managed = GC.GetTotalMemory(false)
        let scratchBytes =
            try
                if Directory.Exists(scratch) then Directory.EnumerateFiles(scratch) |> Seq.sumBy (fun path -> FileInfo(path).Length)
                else 0L
            with :? IOException -> 0L // A spill can be removed between enumeration and stat.
        peakWorking <- max peakWorking current.PeakWorkingSet64
        peakPrivate <- max peakPrivate current.PrivateMemorySize64
        peakManaged <- max peakManaged managed
        peakScratch <- max peakScratch scratchBytes
        metrics.WriteLine($"{watch.ElapsedMilliseconds},{phase},{current.WorkingSet64},{current.PrivateMemorySize64},{managed},{GC.GetGCMemoryInfo().HeapSizeBytes},{GC.GetTotalAllocatedBytes(false)},{scratchBytes}"))
    let sink = { new ILogEventSink with
        member _.Emit(event) =
            match event.Properties.TryGetValue("ScratchDirectory") with
            | true, value -> scratch <- (value :?> Serilog.Events.ScalarValue).Value :?> string
            | _ -> ()
            match event.Properties.TryGetValue("Phase") with
            | true, value -> phase <- (value :?> Serilog.Events.ScalarValue).Value :?> string; record()
            | _ -> () }
    use logger = LoggerConfiguration().WriteTo.Sink(sink).CreateLogger()
    Log.Logger <- logger
    let timer = new Timer((fun _ -> record()), null, 0, 100)
    printfn "profile_pid=%d" Environment.ProcessId
    try
        let binding: RegionalOverlay.Types.SourceBinding = { sourceId = source; payloadPath = zip; descriptorPath = descriptor }
        RegionalGtfsOverlay.executeWithAuditDate (Some auditDate) policy year binding baseline output |> printfn "%A"
    finally
        timer.DisposeAsync().AsTask().GetAwaiter().GetResult()
        record()
        let summary = {| elapsed_seconds = watch.Elapsed.TotalSeconds; peak_working_set_bytes = peakWorking
                         peak_private_bytes = peakPrivate; peak_managed_bytes = peakManaged; peak_scratch_bytes = peakScratch
                         allocated_bytes = GC.GetTotalAllocatedBytes(true); scratch_removed = not (Directory.Exists(scratch)) |}
        let json = JsonSerializer.Serialize(summary, JsonSerializerOptions(WriteIndented = true))
        File.WriteAllText(output + ".summary.json", json)
        printfn "%s" json

[<EntryPoint>]
let main args =
    run args
    0