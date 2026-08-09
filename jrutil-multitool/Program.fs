// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open System.Text.Json
open FSharp.Data
open Serilog
open Serilog.Context

open JrUtil
open JrUtil.GeoData
open JrUtil.Utils

let docstring = (fun (s: string) -> s.Trim()) """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil-multitool.exe jdf-to-gtfs [options] <JDF-in-dir> <GTFS-out-dir>
    jrutil-multitool.exe jdf-to-bundle [options] --snapshot-descriptor=FILE --converter-version=VALUE <JDF-input> <bundle-out-dir>
    jrutil-multitool.exe czptt-to-gtfs [options] <CzPtt-in-file> <GTFS-out-dir>
    jrutil-multitool.exe czptt-to-bundle [options] --catalog-snapshot=FILE <CzPtt-in-file> <bundle-out-dir>
    jrutil-multitool.exe fix-jdf [options] <JDF-in-dir> <JDF-out-dir>
    jrutil-multitool.exe merge-jdf [options] <JDF-out-dir> <JDF-in-dir>...
    jrutil-multitool.exe --help

Options:
    -C --stop-coords-by-id=FILE  CSV file assigning coordinates to stops by ID
    -g --ext-geodata=PATH        CSV file or directory with stop positions
    -o --cz-pbf=PATH             OSM data for Czech Republic
    -l --logfile=FILE            Logfile
    -c --cache=DIR               Persistent cache directory
    -i --by-id                   Merge stops by numeric ID
    -s --strict                  Fail instead of skipping a malformed batch
    --stop-ids-cis               Treat JDF stop numbers as authoritative CIS IDs
    --snapshot-descriptor=FILE    Retrieval provenance and input checksum JSON
    --converter-version=VALUE     Exact JrUtil fork version or commit for provenance
    --international-route-policy=VALUE  keep-all (default) or regional-adjacent
    --international-route-overrides=FILE  Optional route keep/drop override CSV
    --transport-mode-rules=FILE  Reviewed JDF effective transport-mode rule CSV
    -j --jobs=VALUE             Worker count or auto (default: auto)
    --memory-budget=VALUE       RAM budget such as 10GiB or auto (default: auto)
    --batch-output=VALUE        fix-jdf output: directory or zip (default: directory)
    --catalog-snapshot=FILE     Offline KADR catalog snapshot JSON for CZPTT
    --operational-points=VALUE  CZPTT internal points: gtfs (default) or sidecar
    --block-mode=VALUE         CZPTT trip grouping: blocks (default) or none
    --sr70=FILE                 SR70 CSV snapshot for CZPTT point names and coordinates
    --sr70-name20=FILE          Companion SR70 Název20 CSV for fallback route names
    --osm-pbf=FILE              Shared regional OSM PBF for CZPTT coordinate gaps
    --osm-aliases=FILE          Reviewed CZPTT identity-to-OSM-object aliases
    --progress-events           Emit versioned JRUTIL_PROGRESS JSON lines

Passing - to an input path parameter will make most jrutil commands read
input filenames from stdin. Each result will be output into a sequentially
numbered directory.
"""

type StopCoordsById = CsvProvider<
    HasHeaders = false,
    Schema = "id(string), lat(decimal), lon(decimal)">

let stdinLinesSeq () =
    Seq.initInfinite (fun _ -> stdin.ReadLine())
    |> Seq.takeWhile (fun l -> l <> null)

let inOutFiles inpath outpath =
    if inpath = "-" then
        stdinLinesSeq ()
        |> Seq.mapi (fun i l -> (l, Path.Combine(outpath, string i)))
    else
        seq [(inpath, outpath)]

let gtfsWithCoords stopCoordsByIdPath (gtfs: GtfsModel.GtfsFeed) =
    // Allow specific platforms to get fallback positions for stations
    let sr70sRegex = new Regex("^-SR70S-CZ-(\\d+)")
    let idsToMatch id =
        let m = sr70sRegex.Match(id)
        if m.Success then [id; sprintf "-SR70ST-CZ-" + m.Groups.[1].Value]
        else [id]

    match stopCoordsByIdPath with
    | Some p ->
        let coords =
            StopCoordsById.Load(Path.GetFullPath(p)).Rows
            |> Seq.map (fun r -> r.Id, (r.Lat, r.Lon))
            |> Map
        { gtfs with
            stops =
                gtfs.stops
                |> Array.map (fun s ->
                    match idsToMatch s.id
                          |> Seq.choose (fun id -> coords |> Map.tryFind id)
                          |> Seq.tryHead with
                    | Some (lat, lon) -> { s with
                                             lat = Some lat
                                             lon = Some lon }
                    | None -> s)
        }
    | _ -> gtfs

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

[<EntryPoint>]
let main (args: string array) =
    withProcessedArgs docstring args (fun args ->
        setupLogging (optArgValue args "--logfile") ()
        Utils.persistentCachePath <- optArgValue args "--cache"
        let progressEvents = argFlagSet args "--progress-events"

        let memoryRequest =
            optArgValue args "--memory-budget"
            |> Option.defaultValue "auto"
            |> Execution.parseMemoryBudget
        let jobRequest =
            optArgValue args "--jobs"
            |> Option.defaultValue "auto"
            |> Execution.parseJobRequest
        let jobsFor stage workload reservedBytes =
            let memoryBudget = Execution.resolveMemoryBudget memoryRequest
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
            Log.Information(
                "Execution plan for {Stage}: {Workers} workers; requested {RequestedJobs}; " +
                "CPU {ProcessorCount}; memory cap {MemoryJobs}; budget {MemoryBudgetGiB:F1} GiB",
                stage, plan.resolvedWorkers, requested, plan.processorCount,
                plan.memoryLimitedJobs, float plan.memoryBudgetBytes / float Execution.GiB)
            emitProgressEvent progressEvents "execution_plan" [
                "stage", box stage
                "requested_jobs", box requested
                "processor_count", box plan.processorCount
                "memory_budget_bytes", box plan.memoryBudgetBytes
                "reserved_bytes", box plan.reservedBytes
                "worker_allowance_bytes", box plan.workerAllowanceBytes
                "memory_limited_jobs", box plan.memoryLimitedJobs
                "resolved_workers", box plan.resolvedWorkers
            ]
            plan
        let phase stage name state =
            emitProgressEvent progressEvents "phase" [
                "stage", box stage
                "name", box name
                "state", box state
            ]
        let batchEvent eventName stage batchPath (error: exn option) =
            emitProgressEvent progressEvents eventName (
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

        let stopCoordsByIdPath = optArgValue args "--stop-coords-by-id"
        let sr70Path = optArgValue args "--sr70"
        let osmPath = optArgValue args "--osm-pbf"
        let osmAliasesPath = optArgValue args "--osm-aliases"
        let sr70Name20Path =
            optArgValue args "--sr70-name20"
            |> Option.orElseWith (fun () ->
                sr70Path
                |> Option.map (fun path ->
                    Path.Combine(
                        Path.GetDirectoryName(Path.GetFullPath(path)),
                        "SR70_Nazev20.csv")))
        sr70Path
        |> Option.iter (fun path ->
            if not (File.Exists(path)) then
                invalidArg "--sr70" $"SR70 snapshot does not exist: {path}")
        sr70Name20Path
        |> Option.iter (fun path ->
            if not (File.Exists(path)) then
                invalidArg "--sr70-name20"
                    $"SR70 Název20 companion snapshot does not exist: {path}")
        osmPath
        |> Option.iter (fun path ->
            if not (File.Exists(path)) then
                invalidArg "--osm-pbf" $"OSM snapshot does not exist: {path}")
        osmAliasesPath
        |> Option.iter (fun path ->
            if not (File.Exists(path)) then
                invalidArg "--osm-aliases" $"OSM alias file does not exist: {path}")
        let internationalRoutePolicy =
            optArgValue args "--international-route-policy"
            |> Option.defaultValue "keep-all"
            |> JdfToGtfs.parseInternationalRoutePolicy
        let internationalRouteOverrides =
            optArgValue args "--international-route-overrides"
            |> Option.map JdfToGtfs.loadInternationalRouteOverrides
            |> Option.defaultValue [||]
        let transportModeRules =
            optArgValue args "--transport-mode-rules"
            |> Option.map JdfToGtfs.loadTransportModeRules
            |> Option.defaultValue JdfToGtfs.emptyTransportModeRules
        if internationalRoutePolicy = JdfToGtfs.KeepAll
           && internationalRouteOverrides.Length > 0 then
            invalidArg "--international-route-overrides"
                "International route overrides require --international-route-policy=regional-adjacent"
        let mutable exitCode = 0
        if argFlagSet args "jdf-to-bundle" then
            try
                JdfBundle.writeBundleWithPolicyAndRules
                    (argValue args "--snapshot-descriptor")
                    (argValue args "--converter-version")
                    (argFlagSet args "--stop-ids-cis")
                    internationalRoutePolicy
                    internationalRouteOverrides
                    transportModeRules
                    (argValue args "<JDF-input>")
                    (argValue args "<bundle-out-dir>")
                Log.Information("Finished!")
            with e ->
                exitCode <- 1
                Log.Error(e, "JDF bundle conversion failed")
        else if argFlagSet args "jdf-to-gtfs" then
            let stopIdsCis = argFlagSet args "--stop-ids-cis"
            let jdfPar = Jdf.jdfBatchDirParser ()
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (argValues args "<JDF-in-dir>" |> Seq.head)
                       (argValue args "<GTFS-out-dir>")
            |> Seq.iter (fun (inpath, out) ->
                Log.Information("Processing {Batch}", inpath)
                try
                    Log.Information("Reading JDF")
                    let jdf = jdfPar (Jdf.FsPath inpath)
                    let routeKeys =
                        jdf.routes
                        |> Seq.map (fun route -> route.id, route.idDistinction)
                        |> Set
                    JdfToGtfs.validateInternationalRouteOverrides
                        routeKeys internationalRouteOverrides
                    let filterResult =
                        JdfToGtfs.applyInternationalRoutePolicy
                            internationalRoutePolicy internationalRouteOverrides jdf
                    JdfToGtfs.logInternationalRouteDecisions
                        internationalRoutePolicy filterResult.decisions
                    Log.Information("Converting to GTFS")
                    let effectiveBatch, transportModeDecisions =
                        JdfToGtfs.applyTransportModeRules transportModeRules filterResult.batch
                    transportModeDecisions
                    |> Array.iter (fun decision ->
                        if decision.corrected then
                            Log.Information("Corrected JDF route {Route}/{Distinction} transport mode: {Reason}",
                                            decision.routeId, decision.routeDistinction, decision.message)
                        else
                            Log.Warning("JDF route {Route}/{Distinction} transport-mode rule mismatch",
                                        decision.routeId, decision.routeDistinction))
                    let gtfs =
                        JdfToGtfs.getGtfsFeed stopIdsCis effectiveBatch
                        |> Gtfs.deduplicateCalendar
                        |> gtfsWithCoords stopCoordsByIdPath

                    Log.Information("Writing GTFS")
                    gtfs
                    |> Gtfs.fillStandardRequiredFields
                    |> gtfsSer out
                    Log.Information("Finished!")
                with e ->
                    Log.Error(e, "Error while processing {Batch}", inpath)
            )
        else if argFlagSet args "czptt-to-bundle" then
            try
                let catalog =
                    CzPttToGtfs.loadCatalogSnapshot(
                        argValue args "--catalog-snapshot")
                let operationalPointMode =
                    match optArgValue args "--operational-points"
                          |> Option.defaultValue "gtfs" with
                    | "gtfs" -> CzPttToGtfs.Gtfs
                    | "sidecar" -> CzPttToGtfs.Sidecar
                    | value ->
                        invalidArg "--operational-points"
                            $"Expected gtfs or sidecar, got {value}"
                let blockMode =
                    match optArgValue args "--block-mode"
                          |> Option.defaultValue "blocks" with
                    | "blocks" -> CzPttToGtfs.Blocks
                    | "none" -> CzPttToGtfs.NoBlocks
                    | value ->
                        invalidArg "--block-mode"
                            $"Expected blocks or none, got {value}"
                let conversionOptions: CzPttToGtfs.ConversionOptions = {
                    operationalPointMode = operationalPointMode
                    blockMode = blockMode
                }
                let inputPath = argValue args "<CzPtt-in-file>"
                let outputPath = argValue args "<bundle-out-dir>"
                if Directory.Exists(outputPath) then
                    invalidArg "<bundle-out-dir>" "Output path must not exist"
                Directory.CreateDirectory(outputPath) |> ignore
                let bundleProgress name state =
                    phase "convert" name state
                let result =
                    CzPttBundle.writeSidecarsWithProgressAndOptions
                        catalog conversionOptions inputPath outputPath
                        sr70Path sr70Name20Path osmPath osmAliasesPath bundleProgress
                phase "convert" "write-gtfs" "started"
                result.feed
                |> Gtfs.deduplicateCalendar
                |> Gtfs.fillStandardRequiredFields
                |> Gtfs.gtfsFeedToFolder ()
                    (Path.Combine(outputPath, "gtfs-intermediate"))
                let extensionsPath = Path.Combine(outputPath, "extensions")
                Directory.CreateDirectory(extensionsPath) |> ignore
                for fileName in
                    [| "cz_routes.txt"; "cz_trips.txt"; "cz_trip_stop_zones.txt" |] do
                    let source =
                        Path.Combine(outputPath, "gtfs-intermediate", fileName)
                    if File.Exists(source) then
                        File.Move(source, Path.Combine(extensionsPath, fileName))
                phase "convert" "write-gtfs" "completed"
                phase "convert" "write-diagnostics" "started"
                let diagnostics = Dictionary<string, obj>()
                diagnostics.["schema_version"] <- box 1
                diagnostics.["bundle_format"] <- box "czptt-v1"
                diagnostics.["operational_points"] <-
                    box (
                        match operationalPointMode with
                        | CzPttToGtfs.Gtfs -> "gtfs"
                        | CzPttToGtfs.Sidecar -> "sidecar")
                diagnostics.["block_mode"] <-
                    box (
                        match blockMode with
                        | CzPttToGtfs.Blocks -> "blocks"
                        | CzPttToGtfs.NoBlocks -> "none")
                diagnostics.["accepted_pa_count"] <- box result.acceptedPaIds.Length
                diagnostics.["rejected_journeys"] <- box result.rejectedJourneys
                diagnostics.["cancelled_pa_ids"] <- box result.cancelledPaIds
                diagnostics.["sidecar_boundary_approximations"] <-
                    box result.sidecarBoundaryApproximations
                diagnostics.["line_boundary_adjustments"] <-
                    box result.boundaryAdjustments
                diagnostics.["ids_diagnostics"] <- box result.idsDiagnostics
                diagnostics.["merge_diagnostics"] <- box result.mergeDiagnostics
                diagnostics.["coordinate_diagnostics"] <-
                    box result.coordinateDiagnostics
                File.WriteAllText(
                    Path.Combine(outputPath, "diagnostics.json"),
                    JsonSerializer.Serialize(
                        diagnostics,
                        JsonSerializerOptions(WriteIndented = true)) + "\n")
                phase "convert" "write-diagnostics" "completed"
                CzPttBundle.writeManifest outputPath
                Log.Information("Finished!")
            with e ->
                exitCode <- 1
                Log.Error(e, "CZPTT bundle conversion failed")
        else if argFlagSet args "czptt-to-gtfs" then
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            try
                let catalog =
                    optArgValue args "--catalog-snapshot"
                    |> Option.map CzPttToGtfs.loadCatalogSnapshot
                    |> Option.defaultValue CzPttToGtfs.emptyCatalog
                let operationalPointMode =
                    match optArgValue args "--operational-points"
                          |> Option.defaultValue "gtfs" with
                    | "gtfs" -> CzPttToGtfs.Gtfs
                    | "sidecar" -> CzPttToGtfs.Sidecar
                    | value ->
                        invalidArg "--operational-points"
                            $"Expected gtfs or sidecar, got {value}"
                let blockMode =
                    match optArgValue args "--block-mode"
                          |> Option.defaultValue "blocks" with
                    | "blocks" -> CzPttToGtfs.Blocks
                    | "none" -> CzPttToGtfs.NoBlocks
                    | value ->
                        invalidArg "--block-mode"
                            $"Expected blocks or none, got {value}"
                CzPtt.parseAll (argValue args "<CzPtt-in-file>")
                |> CzPttToGtfs.gtfsFeedMergedWithConversionOptions
                    catalog {
                        operationalPointMode = operationalPointMode
                        blockMode = blockMode
                    }
                |> Gtfs.deduplicateCalendar
                |> gtfsWithCoords stopCoordsByIdPath
                |> Gtfs.fillStandardRequiredFields
                |> gtfsSer (argValue args "<GTFS-out-dir>")
                Log.Information("Finished!")
            with e ->
                exitCode <- 1
                Log.Error(e, "Error while processing CzPtt")
        else if argFlagSet args "fix-jdf" then
            let inDir = argValues args "<JDF-in-dir>" |> Seq.head
            let outDir = argValue args "<JDF-out-dir>"
            let geodataPath = optArgValue args "--ext-geodata"
            let czPbf = optArgValue args "--cz-pbf"
            let batchOutput =
                optArgValue args "--batch-output"
                |> Option.defaultValue "directory"
                |> fun value -> value.ToLowerInvariant()
            if batchOutput <> "directory" && batchOutput <> "zip" then
                invalidArg "--batch-output" "fix-jdf batch output must be 'directory' or 'zip'"
            let fixPlan = jobsFor "fix-jdf" Execution.FixBatches (4L * Execution.GiB)
            Directory.CreateDirectory(outDir) |> ignore

            phase "fix-jdf" "read-external-stops" "started"
            let extStopsToMatch =
                geodataPath
                |> Option.map (fun gdp ->
                    Utils.logWrappedOp "Reading external stops" <| fun () ->
                        ExternalCsv.otherStopsFromPathForJdfMatch gdp)
                |> Option.defaultValue [||]
            phase "fix-jdf" "read-external-stops" "completed"
            phase "fix-jdf" "read-osm-stops" "started"
            let osmStopsToMatch =
                czPbf
                |> Option.map (fun pbf ->
                    Utils.logWrappedOp "Reading OSM stops" <| fun () ->
                        Osm.getCzOtherStops pbf ()
                        |> Osm.czOtherStopsForJdfMatch)
                |> Option.defaultValue [||]
            phase "fix-jdf" "read-osm-stops" "completed"
            use stopMatcher = new StopMatcher.StopMatcher<_>(
                Array.concat [ extStopsToMatch; osmStopsToMatch ],
                Utils.persistentCachePath
                |> Option.map (fun d -> Path.Combine(d, "cz-stop-matcher")))

            let jdfPar = Jdf.jdfBatchDirParser ()
            let jdfWri = Jdf.jdfBatchDirWriter ()
            JdfFixups.resetMatchDiagnostics ()
            Log.Information(
                "Fixing JDF with {Jobs} workers and {BatchOutput} batch output",
                fixPlan.resolvedWorkers, batchOutput)
            phase "fix-jdf" "process-batches" "started"
            let results =
                Jdf.findJdfBatchPaths inDir
                |> Seq.sort
                |> Utils.mapParallelOrderedBatches fixPlan.resolvedWorkers (fun batchPath ->
                let batchName = Path.GetFileNameWithoutExtension(batchPath)
                use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
                Log.Information("Processing JDF batch {BatchPath}", batchPath)
                batchEvent "batch_started" "fix-jdf" batchPath None

                try
                    let batch = Jdf.parseJdfBatchPath jdfPar batchPath
                    let routeKeys =
                        batch.routes
                        |> Seq.map (fun route -> route.id, route.idDistinction)
                        |> Set
                    let routeFilter =
                        JdfToGtfs.applyInternationalRoutePolicy
                            internationalRoutePolicy internationalRouteOverrides batch
                    let batchWithLocations =
                        match JdfFixups.dropDegenerateBatch batch with
                        | Some emptyBatch ->
                            Log.Warning(
                                "Dropping degenerate JDF batch with fewer than two distinct called stops")
                            emptyBatch
                        | None ->
                            // Route classification can change after batches with
                            // the same distinction are merged. Always geocode a
                            // non-degenerate source batch.
                            let batchFixed, stopMatches =
                                JdfFixups.fixPublicCisJrBatch stopMatcher batch
                            let stopsWithMatches =
                                Array.zip batchFixed.stops stopMatches
                                |> JdfFixups.rejectImplausibleMatches batchFixed.tripStops
                            Seq.concat [
                                batchFixed.tripStops
                                |> Seq.groupBy (fun ts -> ts.routeId, ts.tripId)
                                |> Seq.map snd
                                // Take one trip most likely to contain all stops'
                                // km distances (testing all takes too much time).
                                |> Seq.sortByDescending Seq.length
                                |> Seq.head
                                |> fun ts ->
                                    JdfFixups.checkMatchDistances
                                        (Seq.toArray ts) stopsWithMatches

                                JdfFixups.checkMissingRegionsCountries batchFixed
                            ]
                            |> Seq.iter (fun msg -> Log.Write(msg))
                            JdfFixups.addStopLocations batchFixed stopsWithMatches
                            |> JdfFixups.estimateMissingStopLocations

                    if batchOutput = "zip" then
                        let fixedOutPath = Path.Combine(outDir, batchName + ".zip")
                        let temporary = fixedOutPath + ".part"
                        use archive = ZipFile.Open(temporary, ZipArchiveMode.Create)
                        jdfWri (Jdf.ZipArchive archive) batchWithLocations
                        archive.Dispose()
                        File.Move(temporary, fixedOutPath)
                    else
                        let fixedOutDir = Path.Combine(outDir, batchName)
                        Directory.CreateDirectory(fixedOutDir) |> ignore
                        jdfWri (Jdf.FsPath fixedOutDir) batchWithLocations
                    Log.Information("Completed JDF batch {BatchPath}", batchPath)
                    batchEvent "batch_completed" "fix-jdf" batchPath None
                    routeKeys, routeFilter.decisions
                with error ->
                    batchEvent "batch_failed" "fix-jdf" batchPath (Some error)
                    reraise ())
                |> Seq.toArray
            phase "fix-jdf" "process-batches" "completed"
            let internationalRouteKeys =
                results |> Seq.collect (fst >> Set.toSeq) |> Set
            let internationalRouteDecisions =
                results |> Array.collect snd
            JdfToGtfs.validateInternationalRouteOverrides
                internationalRouteKeys internationalRouteOverrides
            JdfFixups.logMatchDiagnostics ()
            JdfToGtfs.logInternationalRouteDecisions
                internationalRoutePolicy internationalRouteDecisions
            Log.Information("Finished!")
        else if argFlagSet args "merge-jdf" then
            let outDir = argValue args "<JDF-out-dir>"
            let mergeById = argFlagSet args "--by-id"
            let strict = argFlagSet args "--strict"
            let mergePlan = jobsFor "merge-jdf" Execution.MergeParsing (4L * Execution.GiB)

            let merger = JdfMerger.JdfMerger(
                if mergeById then JdfMerger.MergeStopsById
                else JdfMerger.MergeStopsByName)
            let jdfPar = Jdf.jdfBatchDirParser ()
            let jdfWri = Jdf.jdfBatchDirWriter ()

            Log.Information("Parsing merge inputs with {Jobs} workers", mergePlan.resolvedWorkers)
            phase "merge-jdf" "parse-batches" "started"
            let mergeInputs = seq {
                for inDir in argValues args "<JDF-in-dir>" do
                    yield! Jdf.findJdfBatchPaths inDir |> Seq.sort
            }
            let parsedInputs =
                mergeInputs
                |> Utils.mapParallelOrderedBatches mergePlan.resolvedWorkers (fun batchPath ->
                    batchEvent "batch_started" "merge-jdf" batchPath None
                    try Ok (batchPath, Jdf.parseJdfBatchPath jdfPar batchPath)
                    with error ->
                        batchEvent "batch_failed" "merge-jdf" batchPath (Some error)
                        Error (batchPath, error))
            for parsed in parsedInputs do
                match parsed with
                | Ok (batchPath, batch) ->
                    let batchName = Path.GetFileNameWithoutExtension(batchPath)
                    use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
                    Log.Information("Merging JDF batch {BatchPath}", batchPath)
                    try
                        merger.add(batch)
                        Log.Information("Completed merge of JDF batch {BatchPath}", batchPath)
                        batchEvent "batch_completed" "merge-jdf" batchPath None
                    with error ->
                        batchEvent "batch_failed" "merge-jdf" batchPath (Some error)
                        reraise ()
                | Error (batchPath, error) ->
                    Log.Error(error, "Error while processing {Batch}", batchPath)
                    if strict then raise error
            phase "merge-jdf" "parse-batches" "completed"

            phase "merge-jdf" "resolve-route-overlaps" "started"
            Log.Information("Resolving route overlaps")
            merger.resolveRouteOverlaps()
            phase "merge-jdf" "resolve-route-overlaps" "completed"

            phase "merge-jdf" "write-merged-jdf" "started"
            Log.Information("Writing merged JDF")
            jdfWri (Jdf.FsPath outDir) merger.batch
            phase "merge-jdf" "write-merged-jdf" "completed"
            Log.Information("Finished!")
        else printfn "%s" docstring
        exitCode
    )
