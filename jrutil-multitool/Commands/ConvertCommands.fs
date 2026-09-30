// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// jdf-to-bundle, jdf-export-post-features and czptt-to-bundle.
module JrUtil.Multitool.Commands.ConvertCommands

open System
open Serilog

open JrUtil
open JrUtil.ParallelUtils
open JrUtil.CliArgs
open JrUtil.Multitool.Context

let jdfToBundle (ctx: CommandContext) =
    let args = ctx.Args
    let bundlePlan =
        ctx.JobsFor "jdf-to-bundle" Execution.BundleWork
                (max (512L * Execution.MiB) (ctx.ProcessBudget - 256L * Execution.MiB))
    let bundleProgress (event: JdfBundleModel.BundleProgressEvent) =
        ctx.Emit "work_progress" (
            [ "stage", box "jdf-to-bundle"
              "phase", box event.phase
              "state", box event.state
              "completed", box event.completed
              "unit", box event.unit
              "elapsed_ms", box event.elapsedMilliseconds
              "active_workers", box event.activeWorkers
              "private_bytes", box event.privateBytes
              "working_set_bytes", box event.workingSetBytes ]
            @ (event.total |> Option.map (fun value -> [ "total", box value ])
               |> Option.defaultValue [])
            @ (event.detail |> Option.map (fun value -> [ "detail", box value ])
               |> Option.defaultValue []))
    let bundleOptions: JdfBundleModel.BundleOptions = {
        snapshotDescriptorPath = argValue args "--snapshot-descriptor"
        converterVersion = argValue args "--converter-version"
        internationalPolicy = internationalRoutePolicy args
        transportModeRules =
            optArgValue args "--transport-mode-rules"
            |> Option.map JdfGtfsRules.loadTransportModeRules
            |> Option.defaultValue JdfGtfsRules.emptyTransportModeRules
        estimatedPosts = (estimatedPostActivation args).runRoutedInference
        routingPbfPath = optArgValue args "--routing-osm-pbf"
        diagnosticPostLabels = argFlagSet args "--diagnostic-post-labels"
        maximumWorkers = min 8 bundlePlan.resolvedWorkers
        memoryBudgetBytes = bundlePlan.memoryBudgetBytes
        reviewStopsPath = optArgValue args "--post-review-stops"
        capturePostInferenceEvidencePath = optArgValue args "--capture-post-inference-evidence"
        postInferenceEvidenceOnly = argFlagSet args "--post-inference-evidence-only"
        captureRestriction =
            { JdfPostEvidence.CaptureRestriction.stopRegion =
                optArgValue args "--capture-stop-region"
                |> Option.map JdfPostEvidence.parseCaptureStopRegion
              excludedSourcePrefixes =
                optArgValue args "--capture-exclude-source"
                |> Option.map JdfPostEvidence.parseCaptureExcludedSources
                |> Option.defaultValue [||] }
        exportPostContextCallsPath = optArgValue args "--export-post-context-calls"
        postInferenceEvidencePath = optArgValue args "--post-inference-evidence"
        postInferencePolicyPath = optArgValue args "--post-inference-policy"
        includePostInferenceScores = not(argFlagSet args "--no-post-inference-scores")
        diagnosticsOutput = optArgValue args "--diagnostics-out"
        diagnosticTraces = argFlagSet args "--diagnostic-traces"
        progress = bundleProgress
        gvdYear =
            optArgValue args "--gvd-year"
            |> Option.map (fun value ->
                match Int32.TryParse(value) with
                | true, year when year >= 2000 && year <= 9999 -> year
                | _ -> invalidArg "--gvd-year" "Expected a four-digit year")
    }
    ctx.Phase "jdf-to-bundle" "write-bundle" "started"
    match JdfBundle.execute bundleOptions (argValue args "<JDF-input>") (argValue args "<bundle-out-dir>") with
    | JdfBundleModel.CaptureCompleted(manifest,metrics) ->
        ctx.Phase "jdf-to-bundle" "capture-post-inference-evidence" "completed"
        ctx.Emit "capture_metrics" [
            "stage", box "jdf-to-bundle"
            "estimated_evidence_bytes", box metrics.estimatedEvidenceBytes
            "atomic_output_headroom_bytes", box metrics.atomicOutputHeadroomBytes
            "current_spill_bytes", box metrics.currentSpillBytes
            "peak_spill_bytes", box metrics.peakSpillBytes
            "maximum_workers", box metrics.maximumWorkers
        ]
        ctx.ResourceUsage "jdf-to-bundle" "capture-post-inference-evidence"
                          metrics.peakSpillBytes
        Log.Information(
            "Post-inference evidence capture completed: pack_id={PackId}; rows={Rows}",
            manifest.packId,manifest.routePointEvidenceCount)
    | JdfBundleModel.BundleCompleted ->
        ctx.Phase "jdf-to-bundle" "write-bundle" "completed"
        ctx.ResourceUsage "jdf-to-bundle" "write-bundle" 0L
    Log.Information("Finished!")
    0

let jdfExportPostFeatures (ctx: CommandContext) =
    let args = ctx.Args
    JdfBundleEvidence.exportPostInferenceFeatures
        (argValue args "--evidence")
        (optArgValue args "--policy")
        (argValue args "--output")
    Log.Information("Finished!")
    0

let czpttToBundle (ctx: CommandContext) =
    let args = ctx.Args
    let operationalPointMode =
        match optArgValue args "--operational-points" |> Option.defaultValue "gtfs" with
        | "gtfs" -> CzPttModel.Gtfs
        | "sidecar" -> CzPttModel.Sidecar
        | value -> invalidArg "--operational-points" $"Expected gtfs or sidecar, got {value}"
    let options: CzPttPackage.Options = {
        catalog = CzPttModel.loadCatalogSnapshot (argValue args "--catalog-snapshot")
        conversion = { operationalPointMode = operationalPointMode }
        sr70Path = existingFile args "--sr70" "SR70 snapshot"
        osmPath = existingFile args "--osm-pbf" "OSM snapshot"
        osmAliasesPath = existingFile args "--osm-aliases" "OSM alias file"
        diagnosticsOutput = optArgValue args "--diagnostics-out"
        diagnosticTraces = argFlagSet args "--diagnostic-traces"
    }
    let bundleProgress name state =
        ctx.Phase "convert" name state
        if state = "completed" then
            ctx.ResourceUsage "convert" name (CzPttBundle.currentSpillBytes())
    CzPttPackage.write options (argValue args "<CzPtt-in-file>") (argValue args "<bundle-out-dir>") bundleProgress |> ignore
    Log.Information("Finished!")
    0
