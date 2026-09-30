// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// fix-jdf and merge-jdf.
module JrUtil.Multitool.Commands.JdfCommands

open System
open System.IO
open System.IO.Compression
open Serilog
open Serilog.Context

open JrUtil
open JrUtil.GeoData
open JrUtil.DateUtils
open JrUtil.FileUtils
open JrUtil.ParallelUtils
open JrUtil.CliArgs
open JrUtil.Logging
open JrUtil.Multitool.Context

let fixJdf (ctx: CommandContext) =
    let args = ctx.Args
    let inDir = argValues args "<JDF-in-dir>" |> Seq.head
    let outDir = argValue args "<JDF-out-dir>"
    let geodataPath = optArgValue args "--ext-geodata"
    let czPbf = optArgValue args "--cz-pbf"
    let internationalRoutePolicy = internationalRoutePolicy args
    let collectEstimatedPostEvidence = (estimatedPostActivation args).collectEvidence
    let batchOutput =
        optArgValue args "--batch-output"
        |> Option.defaultValue "directory"
        |> fun value -> value.ToLowerInvariant()
    if batchOutput <> "directory" && batchOutput <> "zip" then
        invalidArg "--batch-output" "fix-jdf batch output must be 'directory' or 'zip'"
    Directory.CreateDirectory(outDir) |> ignore

    ctx.Phase "fix-jdf" "read-external-stops" "started"
    let extStopsToMatch =
        geodataPath
        |> Option.map (fun gdp ->
            Logging.logWrappedOp "Reading external stops" <| fun () ->
                ExternalCsv.otherStopsFromPathForJdfMatch gdp)
        |> Option.defaultValue [||]
    ctx.Phase "fix-jdf" "read-external-stops" "completed"
    ctx.ResourceUsage "fix-jdf" "read-external-stops" 0L
    ctx.Phase "fix-jdf" "read-osm-stops" "started"
    let osmStopsToMatch =
        czPbf
        |> Option.map (fun pbf ->
            Logging.logWrappedOp "Reading OSM stops" <| fun () ->
                OsmStops.getCzOtherStops pbf
                |> OsmStops.czOtherStopsForJdfMatch)
        |> Option.defaultValue [||]
    ctx.Phase "fix-jdf" "read-osm-stops" "completed"
    ctx.ResourceUsage "fix-jdf" "read-osm-stops" 0L
    use stopMatcher = new StopMatcher.StopMatcher<_>(
        Array.concat [ extStopsToMatch; osmStopsToMatch ])

    // The transit-geometry index is a persistent part of this stage's
    // working set. Snapshot memory only after it (and the stop matcher)
    // exist so the adaptive budget cannot start below its true baseline.
    let fixPlan = ctx.JobsFor "fix-jdf" Execution.FixBatches (4L * Execution.GiB)
    let jdfPar = Jdf.jdfBatchDirParser ()
    let jdfWri = Jdf.jdfBatchDirWriter ()
    JdfFixups.resetMatchDiagnostics ()
    Log.Information(
        "Fixing JDF with {Jobs} workers and {BatchOutput} batch output",
        fixPlan.resolvedWorkers, batchOutput)
    ctx.Phase "fix-jdf" "process-batches" "started"
    let results =
        Jdf.findJdfBatchPaths inDir
        |> Seq.sort
        |> ParallelUtils.mapParallelOrderedAdaptive
            fixPlan.maximumWorkers
            fixPlan.initialWorkers
            fixPlan.memoryBudgetBytes
            (ctx.AdmissionBytes "fix-jdf")
            (fun batchPath -> max (8L * Execution.MiB) (inputBytes batchPath * 3L))
            (fun batchPath -> ctx.BatchEvent "batch_started" "fix-jdf" batchPath None)
            (ctx.SchedulerSample "fix-jdf")
            (fun batchPath ->
        let batchName = Path.GetFileNameWithoutExtension(batchPath)
        use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
        Log.Information("Processing JDF batch {BatchPath}", batchPath)
        try
            let batchWithLocations, decisions =
                Jdf.parseJdfBatchPath jdfPar batchPath
                |> JdfFix.fixBatch stopMatcher internationalRoutePolicy collectEstimatedPostEvidence
            if batchOutput = "zip" then
                let fixedOutPath = Path.Combine(outDir, batchName + ".zip")
                FileUtils.writeAtomicFile fixedOutPath (fun temporary ->
                    use archive = ZipFile.Open(temporary, ZipArchiveMode.Create)
                    jdfWri (Jdf.ZipArchive archive) batchWithLocations)
            else
                let fixedOutDir = Path.Combine(outDir, batchName)
                Directory.CreateDirectory(fixedOutDir) |> ignore
                jdfWri (Jdf.FsPath fixedOutDir) batchWithLocations
            Log.Information("Completed JDF batch {BatchPath}", batchPath)
            Ok (batchPath, decisions)
        with error ->
            Error (batchPath, error))
    let internationalRouteDecisions = ResizeArray<_>()
    for result in results do
        match result with
        | Ok (batchPath, decisions) ->
            ctx.BatchEvent "batch_completed" "fix-jdf" batchPath None
            internationalRouteDecisions.AddRange(decisions)
        | Error (batchPath, error) ->
            ctx.BatchEvent "batch_failed" "fix-jdf" batchPath (Some error)
            raise error
    ctx.Phase "fix-jdf" "process-batches" "completed"
    ctx.ResourceUsage "fix-jdf" "process-batches" 0L
    JdfFixups.logMatchDiagnostics ()
    JdfInternationalFilter.logInternationalRouteDecisions
        internationalRoutePolicy (internationalRouteDecisions.ToArray())
    Log.Information("Finished!")
    0

let private mergeInputWeight batchPath =
    if File.Exists(batchPath) then
        max (8L * Execution.MiB) (FileInfo(batchPath).Length * 12L)
    else
        max (8L * Execution.MiB) (inputBytes batchPath * 3L)

let mergeJdf (ctx: CommandContext) =
    let args = ctx.Args
    let outDir = argValue args "<JDF-out-dir>"
    let strict = argFlagSet args "--strict"
    let mergePlan = ctx.JobsFor "merge-jdf" Execution.MergeParsing (4L * Execution.GiB)
    let mutable gvdYear = 0
    if not (Int32.TryParse(argValue args "--gvd-year", &gvdYear)) || gvdYear < 2000 || gvdYear > 9999 then
        invalidArg "--gvd-year" "Expected a four-digit year"
    let referenceDate =
        let parsed = NodaTime.Text.LocalDatePattern.Iso.Parse(argValue args "--reference-date")
        if not parsed.Success then invalidArg "--reference-date" "Expected YYYY-MM-DD"
        parsed.Value
    let gvdStart, gvdEnd = DateUtils.gvdBounds gvdYear
    if referenceDate < gvdStart || referenceDate > gvdEnd then
        invalidArg "--reference-date" $"The reference date is outside GVD {gvdYear} ({gvdStart}..{gvdEnd})"

    let outPath = Path.GetFullPath(outDir)
    let outParent = Path.GetDirectoryName(outPath)
    Directory.CreateDirectory(outParent) |> ignore
    let spillPath =
        Path.Combine(
            outParent,
            $".{Path.GetFileName(outPath)}.trip-stops.{Guid.NewGuid():N}.tmp")

    use merger =
        new JdfMerger.JdfMerger(
            JdfMerger.MergeStopsByName,
            spillPath,
            mergePlan.maximumWorkers)
    let jdfPar = Jdf.jdfBatchDirParser ()
    Log.Information("Parsing merge inputs with {Jobs} workers", mergePlan.resolvedWorkers)
    ctx.Phase "merge-jdf" "parse-batches" "started"
    let availableSpillBytes = DriveInfo(Path.GetPathRoot(outParent)).AvailableFreeSpace
    let minimumSpillReserve = Execution.GiB
    ctx.Emit "spill_preflight" [
        "stage", box "merge-jdf"
        "mode", box "progressive"
        "minimum_free_bytes", box minimumSpillReserve
        "available_bytes", box availableSpillBytes
    ]
    if availableSpillBytes < minimumSpillReserve then
        invalidOp (
            $"Insufficient temporary disk for merge-jdf: require at least "
            + $"{minimumSpillReserve} free bytes, available {availableSpillBytes}")
    let mergeInputs = seq {
        for inDir in argValues args "<JDF-in-dir>" do
            for path in Jdf.findJdfBatchPaths inDir |> Seq.sort do
                yield path, mergeInputWeight path
    }
    let parsedInputs =
        mergeInputs
        |> ParallelUtils.mapParallelOrderedAdaptive
            mergePlan.maximumWorkers
            mergePlan.initialWorkers
            mergePlan.memoryBudgetBytes
            (ctx.AdmissionBytes "merge-jdf")
            snd
            (fun (batchPath, _) ->
                ctx.BatchEvent "batch_started" "merge-jdf" batchPath None)
            (ctx.SchedulerSample "merge-jdf")
            (fun (batchPath, _) ->
            try Ok (batchPath, Jdf.parseJdfBatchPath jdfPar batchPath)
            with error -> Error (batchPath, error))
    for parsed in parsedInputs do
        match parsed with
        | Ok (batchPath, batch) ->
            let batchName = Path.GetFileNameWithoutExtension(batchPath)
            use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
            Log.Information("Merging JDF batch {BatchPath}", batchPath)
            try
                merger.add(batch)
                Log.Information("Completed merge of JDF batch {BatchPath}", batchPath)
                ctx.BatchEvent "batch_completed" "merge-jdf" batchPath None
            with error ->
                ctx.BatchEvent "batch_failed" "merge-jdf" batchPath (Some error)
                reraise ()
        | Error (batchPath, error) ->
            ctx.BatchEvent "batch_failed" "merge-jdf" batchPath (Some error)
            Log.Error(error, "Error while processing {Batch}", batchPath)
            if strict then raise error
    ctx.Phase "merge-jdf" "parse-batches" "completed"
    merger.logStopMergeSummary()
    ctx.ResourceUsage "merge-jdf" "parse-batches" merger.tripStopSpillBytes

    ctx.Phase "merge-jdf" "resolve-route-overlaps" "started"
    Log.Information("Resolving route overlaps")
    merger.resolveRouteOverlaps()
    merger.boundValidity(referenceDate, gvdStart, gvdEnd)
    ctx.Phase "merge-jdf" "resolve-route-overlaps" "completed"
    ctx.ResourceUsage "merge-jdf" "resolve-route-overlaps" merger.tripStopSpillBytes

    ctx.Phase "merge-jdf" "write-merged-jdf" "started"
    Log.Information("Writing merged JDF")
    merger.write(outDir)
    ctx.Phase "merge-jdf" "write-merged-jdf" "completed"
    ctx.ResourceUsage "merge-jdf" "write-merged-jdf" merger.tripStopSpillBytes
    Log.Information("Finished!")
    0
