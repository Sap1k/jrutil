// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// validate-package, compare-packages and regional-gtfs-overlay.
module JrUtil.Multitool.Commands.PackageCommands

open System
open System.IO
open Serilog

open JrUtil
open JrUtil.RegionalOverlay.Types
open JrUtil.CliArgs
open JrUtil.Multitool.Context

let validate (ctx: CommandContext) =
    let result = JrUtil.Serving.Validation.validatePackage (argValue ctx.Args "<package-dir>")
    Log.Information("Production package valid: files={Files}; relations={Relations}", result.fileCount, result.relationCount)
    0

let compare (ctx: CommandContext) =
    let args = ctx.Args
    let left, right = argValue args "<left-package>", argValue args "<right-package>"
    if argFlagSet args "--byte-identical" then
        JrUtil.Serving.Validation.compareByteIdentical left right
        Log.Information("Production packages are byte-identical")
        0
    else
        let expectations =
            optArgValue args "--expect"
            |> Option.map (File.ReadAllLines >> JrUtil.Serving.Comparison.parseExpectations)
            |> Option.defaultValue []
        let report = JrUtil.Serving.Comparison.comparePackages expectations left right
        for difference in report.differences do
            let expected = not (List.contains difference report.unexpected)
            let level = if expected then Events.LogEventLevel.Information else Events.LogEventLevel.Error
            Log.Write(level, "{Expected}{Kind} {Subject}: {Summary}",
                      (if expected then "expected " else ""), difference.kind, difference.subject, difference.summary)
            for sample in difference.samples do Log.Write(level, "    {Sample}", sample)
        Log.Information("Compared {Count} package parts; {Differences} differences, {Unexpected} unexpected",
                        report.compared.Length, report.differences.Length, report.unexpected.Length)
        if report.isEquivalent then 0 else 1

let private parseOverlayBindings args =
    let parseBinding argumentName (value: string) =
        let separator = value.IndexOf('=')
        if separator <= 0 || separator = value.Length - 1 then
            invalidArg argumentName $"Expected SOURCE_ID=PATH, got {value}"
        value.Substring(0, separator), value.Substring(separator + 1)
    let sources = argValues args "--source" |> Seq.map (parseBinding "--source") |> Seq.toArray
    let descriptorBindings = argValues args "--source-descriptor" |> Seq.map (parseBinding "--source-descriptor") |> Seq.toArray
    if sources.Length < 1 then invalidArg "--source" "regional-gtfs-overlay requires at least one source"
    if sources |> Array.map fst |> Array.distinct |> Array.length <> sources.Length then invalidArg "--source" "Duplicate source ID"
    if descriptorBindings |> Array.map fst |> Array.distinct |> Array.length <> descriptorBindings.Length then invalidArg "--source-descriptor" "Duplicate source descriptor ID"
    let descriptors = descriptorBindings |> dict
    if descriptors.Count <> sources.Length then invalidArg "--source-descriptor" "Each source requires exactly one descriptor"
    let bindings =
        sources
        |> Array.map (fun (sourceId, payloadPath) ->
            match descriptors.TryGetValue(sourceId) with
            | true, descriptorPath -> { sourceId = sourceId; payloadPath = payloadPath; descriptorPath = descriptorPath }
            | _ -> invalidArg "--source-descriptor" $"Missing descriptor for source {sourceId}")
    let mutable gvdYear = 0
    if not (Int32.TryParse(argValue args "--gvd-year", &gvdYear)) then invalidArg "--gvd-year" "Expected a four-digit year"
    gvdYear, bindings

let overlay (ctx: CommandContext) =
    let args = ctx.Args
    let gvdYear, sourceBindings = parseOverlayBindings args
    let baseBundle = argValue args "<base-bundle>"
    JrUtil.Serving.Validation.validatePackage baseBundle |> ignore
    let result =
        RegionalGtfsOverlay.compile {
            auditDate = None
            policyPath = argValue args "--policy"
            gvdYear = gvdYear
            bindings = sourceBindings
            baseBundle = baseBundle
            outputBundle = argValue args "<overlay-bundle-out>"
            converterVersion = argValue args "--converter-version"
            diagnosticsOutput = optArgValue args "--diagnostics-out"
            diagnosticTraces = argFlagSet args "--diagnostic-traces"
            stopRegistry = optArgValue args "--stop-registry" |> Option.map StopRegistry.load
            stopRegistryCandidatesPath = optArgValue args "--stop-registry-candidates" }
    Log.Information(
        "Regional overlay complete: sources={Sources}; matched_trips={MatchedTrips}; unmatched_trips={UnmatchedTrips}; ambiguous_trips={AmbiguousTrips}",
        String.concat "," result.sources, result.aggregate.matchedTrips,
        result.aggregate.unmatchedTrips, result.aggregate.ambiguousTrips)
    0
