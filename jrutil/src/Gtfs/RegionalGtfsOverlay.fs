// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module JrUtil.RegionalGtfsOverlay

open JrUtil.RegionalOverlay
open JrUtil.RegionalOverlay.Types
open System.Collections.Generic

let loadPolicy path = Policy.loadPolicy path

let resolveFullHeadsign raw destinations = Support.resolveFullHeadsign raw destinations

// A method boundary ends the lifetime of matching-only indexes before projection.
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private analyzeSource prepared =
    let source, indexes = SourceAnalysis.analyze { prepared = prepared }
    let matches = TripMatching.matchTrips { prepared = prepared; source = source; indexes = indexes }
    source, matches

let private executeSingle (options: CompilationOptions) (binding: SourceBinding) =
    use scratch = new Scratch.Storage(System.IO.Path.GetTempPath())
    Serilog.Log.Information("Regional overlay scratch: {ScratchDirectory}", scratch.Directory)
    let prepared = InputPreparation.prepare {
        scratch = scratch
        auditDate = options.auditDate
        policyPath = options.policyPath
        gvdYear = options.gvdYear
        binding = binding
        baseBundle = options.baseBundle
        outputBundle = options.outputBundle
    }

    let source, matches = analyzeSource prepared
    Runtime.releaseAnalysisMemory ()

    let projection = Projection.resolve {
        prepared = prepared
        source = source
        matches = matches
    }

    BundleWriter.write {
        prepared = prepared
        projection = projection
        source = source
        matches = matches
        gvdYear = options.gvdYear
        diagnosticsOutput = options.diagnosticsOutput
        diagnosticTraces = options.diagnosticTraces
    }

let private executeMultiple (options: CompilationOptions) =
    use scratch = new Scratch.Storage(System.IO.Path.GetTempPath())
    let combined = MultiSourcePreparation.prepare scratch.Directory options.policyPath options.baseBundle options.bindings
    let prepared = InputPreparation.prepare {
        scratch = scratch
        auditDate = options.auditDate
        policyPath = combined.policyPath
        gvdYear = options.gvdYear
        binding = combined.binding
        baseBundle = options.baseBundle
        outputBundle = options.outputBundle
    }
    let source, matches = analyzeSource prepared
    Runtime.releaseAnalysisMemory ()
    let projection = Projection.resolve { prepared = prepared; source = source; matches = matches }
    let result = BundleWriter.write {
        prepared = prepared; projection = projection; source = source; matches = matches
        gvdYear = options.gvdYear; diagnosticsOutput = options.diagnosticsOutput
        diagnosticTraces = options.diagnosticTraces }
    let perSource = Dictionary<string, OverlayResult>(System.StringComparer.Ordinal)
    for sourceId in combined.sourceIds do
        let sourceTrips = source.tripRows |> Array.filter (fun row -> Support.sourceIdentity prepared.binding.sourceId row = sourceId)
        let matched = matches.bindings |> Seq.filter (fun value -> value.sourceId = sourceId) |> Seq.map (fun value -> value.sourceTripId) |> Seq.distinct |> Seq.length
        let ambiguous = matches.unresolvedPending |> Array.filter (fun value -> Support.sourceIdentity prepared.binding.sourceId source.tripsById.[value.sourceTripId] = sourceId) |> Array.map (fun value -> value.sourceTripId) |> Array.distinct |> Array.length
        let sourceGroups = source.stopGroups |> Array.filter (fun group -> Support.sourceIdentity prepared.binding.sourceId group.members.[0] = sourceId)
        let matchedGroups = sourceGroups |> Array.filter (fun group -> source.stopGroupMatches.ContainsKey(group.groupId)) |> Array.length
        perSource.[sourceId] <- {
            outputPath = result.outputPath
            matchedTrips = matched
            unmatchedTrips = max 0 (sourceTrips.Length - matched - ambiguous)
            ambiguousTrips = ambiguous
            matchedStopGroups = matchedGroups
            unmatchedStopGroups = sourceGroups.Length - matchedGroups
            selectedShapes = projection.outputShapeBySource |> Seq.filter (fun pair -> pair.Key.StartsWith(sourceId + ":", System.StringComparison.Ordinal)) |> Seq.length
            selectedTransfers = 0
        }
    {
        outputPath = result.outputPath
        sources = combined.sourceIds
        aggregate = result
        perSource = perSource
    }

/// The single production compilation API. The source array may contain one
/// binding or the exact set declared by a combined-source policy.
let compile (options: CompilationOptions) =
    if isNull options.bindings || options.bindings.Length = 0 then
        invalidArg "options" "At least one source binding is required"
    elif options.bindings.Length = 1 then
        let result = executeSingle options options.bindings.[0]
        let perSource = Dictionary<string, OverlayResult>(System.StringComparer.Ordinal)
        perSource.[options.bindings.[0].sourceId] <- result
        { outputPath = result.outputPath; sources = [| options.bindings.[0].sourceId |]
          aggregate = result; perSource = perSource }
    else executeMultiple options
