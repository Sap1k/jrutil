// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BundleWriter

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Runtime.CompilerServices
open System.Text
open System.Text.Json
open NodaTime

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Values
open JrUtil.RegionalOverlay.GtfsFiles
open JrUtil.RegionalOverlay.GtfsRows
open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.OutputIds
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay
open JrUtil.RegionalOverlay.BundleOutput

type Input = {
    prepared: InputPreparation.Result
    projection: Projection.Result
    source: SourceAnalysis.Result
    matches: TripMatching.Result
    gvdYear: int
    converterVersion: string
    diagnosticsOutput: string option
    diagnosticTraces: bool
}

/// Stream the resolved bundle, write reports and atomically activate the output.
[<MethodImpl(MethodImplOptions.NoInlining)>]
let private compile ({
    prepared = prepared
    projection = projection
    source = source
    matches = matches
    gvdYear = gvdYear
    converterVersion = converterVersion
    diagnosticsOutput = diagnosticsOutput
    diagnosticTraces = diagnosticTraces
}: Input) =
    let sibling = Path.GetDirectoryName(prepared.outputBundle)
    let temporary = Path.Combine(sibling, "." + Path.GetFileName(prepared.outputBundle) + ".tmp-" + Guid.NewGuid().ToString("N"))
    let gtfsOutput = Path.Combine(temporary, "gtfs-intermediate")
    let extensionsOutput = Path.Combine(temporary, "extensions")
    let usedStopIds = HashSet<string>(StringComparer.Ordinal)
    let usedRouteIds = HashSet<string>(StringComparer.Ordinal)
    let usedAgencyIds = HashSet<string>(StringComparer.Ordinal)
    try
        Directory.CreateDirectory(gtfsOutput) |> ignore
        Directory.CreateDirectory(extensionsOutput) |> ignore
        let context: Context = {
            prepared = prepared; projection = projection; source = source; matches = matches
            gtfsOutput = gtfsOutput; extensionsOutput = extensionsOutput
            usedStopIds = usedStopIds; usedRouteIds = usedRouteIds; usedAgencyIds = usedAgencyIds }
        BundleCalls.write context sibling
        BundleGtfs.writeTrips context
        let agencyColumns, sourceNativeAgencyRows = BundleGtfs.writeRoutes context
        let zones = BundleGtfs.writeStops context
        BundleGtfs.writeAgencies context agencyColumns sourceNativeAgencyRows
        BundleGtfs.writeCalendar context gvdYear
        let usedOutputShapeIds = BundleGtfs.writeShapes context
        let transfers = BundleTransfers.write context
        let transferRows = transfers.rows
        let transferProvenance = transfers.provenance
        let slicesForBinding = transfers.slicesForBinding
        BundleExtensions.write context zones

        logProgress "write-reports" 0L None
        let reports = Reports.write {
            prepared = prepared
            projection = projection
            source = source
            matches = matches
            temporary = temporary
            usedStopIds = usedStopIds
            usedOutputShapeIds = usedOutputShapeIds
            slicesForBinding = slicesForBinding
            transferProvenance = transferProvenance
        }
        let fullyCoveredSourceTrips = reports.fullyCoveredSourceTrips
        let matchedSourceTripSet = reports.matchedSourceTripSet
        let activeSourceTrips = reports.activeSourceTrips

        let finalResult = {
            outputPath = prepared.outputBundle
            matchedTrips = matchedSourceTripSet.Count
            unmatchedTrips = activeSourceTrips.Length - matchedSourceTripSet.Count
            ambiguousTrips =
                matches.unresolvedPending
                |> Seq.map (fun value -> value.sourceTripId)
                |> Seq.filter (fullyCoveredSourceTrips.Contains >> not)
                |> Seq.distinct
                |> Seq.length
            matchedStopGroups = source.stopGroupMatches.Count
            unmatchedStopGroups = source.stopGroups.Length - source.stopGroupMatches.Count
            selectedShapes = usedOutputShapeIds.Count
            selectedTransfers = transferRows.Count
        }
        projection.selectionByBinding.Clear()
        matches.bindings.Clear()
        source.stopInferenceEvidence.Clear()
        projection.slicesByBaseTrip.Clear()
        projection.serviceDates.Clear()
        transferRows.Clear()
        transferProvenance.Clear()
        logProgress "compact-before-evidence" 1L (Some 1L)

        // The production base is read in place until finalization; its serving
        // payloads are never duplicated into compiler scratch.
        let basePackage = prepared.baseBundle
        let policyOutput = Path.Combine(temporary, "policy")
        Directory.CreateDirectory(policyOutput) |> ignore
        copyDirectory (Path.Combine(prepared.binding.payloadPath, "_overlay", "policies")) policyOutput
        match parseOverridePath prepared.policy.source.overrides.stops with
        | Some path -> File.Copy(path, Path.Combine(policyOutput, Path.GetFileName(path)), false)
        | None -> ()
        let sourceMetadata = Path.Combine(temporary, "source-metadata")
        copyDirectory (Path.Combine(prepared.binding.payloadPath, "_overlay", "descriptors")) sourceMetadata

        let gvd = Dictionary<string, obj>()
        gvd.["year"] <- box gvdYear
        gvd.["start_date"] <- box (dateString prepared.window.startDate)
        gvd.["end_date"] <- box (dateString prepared.window.endDate)
        let manifest = Dictionary<string, obj>()
        manifest.["bundle_format"] <- box "obehy-jrutil-regional-gtfs-overlay"
        manifest.["bundle_version"] <- box OverlayBundleVersion
        manifest.["audit_date"] <- box (dateString prepared.auditDate)
        manifest.["base_snapshot_date"] <- box (dateString prepared.snapshotDate)
        manifest.["base_manifest_sha256"] <- box (sha256File (Path.Combine(prepared.baseBundle, "manifest.json")) )
        use metadataDocument = JsonDocument.Parse(File.ReadAllText(Path.Combine(prepared.binding.payloadPath, "overlay_sources.json")))
        let metadataRoot = metadataDocument.RootElement
        manifest.["sources"] <- box (JsonSerializer.Deserialize<obj>(metadataRoot.GetProperty("sources").GetRawText(), jsonOptions))
        manifest.["policy_sha256"] <- box (metadataRoot.GetProperty("policy_sha256").GetString())
        manifest.["gvd"] <- box gvd
        manifest.["conversion"] <- box (dict [ "tool", box "jrutil"; "version", box converterVersion ])

        let tablesIn directory pattern =
            let root = Path.Combine(temporary, directory)
            if Directory.Exists(root) then
                Directory.EnumerateFiles(root, pattern)
                |> Seq.map (fun path -> Path.GetFileName(path), JrUtil.Serving.CompilerOutput.csvFileTable path)
                |> JrUtil.Serving.CompilerOutput.tables
            else JrUtil.Serving.CompilerOutput.noTables
        let diagnosticFiles = Dictionary<string, string>(StringComparer.Ordinal)
        for directory, target in [ policyOutput, "inputs/policies"; sourceMetadata, "inputs/source-descriptors" ] do
            for path in Directory.EnumerateFiles(directory, "*", SearchOption.AllDirectories) do
                diagnosticFiles.[target + "/" + Path.GetRelativePath(directory, path).Replace('\\', '/')] <- path
        let json (text: string) =
            use document = JsonDocument.Parse(text)
            document.RootElement.Clone()
        let output: JrUtil.Serving.CompilerOutput.Output = {
            gtfs = tablesIn "gtfs-intermediate" "*.txt"
            czech = tablesIn "extensions" "*.txt"
            mappings = tablesIn "mappings" "*.csv"
            reports = tablesIn "reports" "*.csv"
            sidecars = JrUtil.Serving.CompilerOutput.noTables
            manifest = json (JsonSerializer.Serialize(manifest, jsonOptions))
            diagnostics = None
            basePackage = Some basePackage
            diagnosticFiles = diagnosticFiles :> IReadOnlyDictionary<_, _> }

        prepared.scratch.Flush()
        temporary, output, prepared.outputBundle, diagnosticsOutput, diagnosticTraces, finalResult
    with error ->
        if Directory.Exists(temporary) then Directory.Delete(temporary, true)
        reraise ()

/// Finalize only after compile has returned.  The method boundary makes
/// the compiler's national matching/projection graph unreachable before the
/// production writer allocates its own nationwide buffers.
let write input =
    let temporary, compiled, outputBundle, diagnosticsOutput, diagnosticTraces, finalResult =
        compile input
    let productionTemporary = temporary + ".production"
    try
        JrUtil.Serving.PackageWriter.writePackage
            Map.empty None (fun _ _ -> ()) compiled productionTemporary
        diagnosticsOutput
        |> Option.iter (fun path ->
            JrUtil.Serving.PackageWriter.writeDiagnosticArtifact compiled path diagnosticTraces)
        Directory.Delete(temporary, true)
        Directory.Move(productionTemporary, outputBundle)
        finalResult
    with error ->
        // The compiler scratch (GTFS, mappings, reports) is the only evidence
        // for a finalization failure. Keep it for diagnosis.
        if Directory.Exists(temporary) then
            Serilog.Log.Error("Regional overlay finalization failed; compiler scratch retained at {ScratchPath}", temporary)
        reraise ()
