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

let executeWithAuditDate auditDate policyPath gvdYear (binding: SourceBinding) baseBundle outputBundle =
    use scratch = new Scratch.Storage(System.IO.Path.GetTempPath())
    Serilog.Log.Information("Regional overlay scratch: {ScratchDirectory}", scratch.Directory)
    let prepared = InputPreparation.prepare {
        scratch = scratch
        auditDate = auditDate
        policyPath = policyPath
        gvdYear = gvdYear
        binding = binding
        baseBundle = baseBundle
        outputBundle = outputBundle
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
        gvdYear = gvdYear
    }

let execute policyPath gvdYear binding baseBundle outputBundle =
    executeWithAuditDate None policyPath gvdYear binding baseBundle outputBundle

let executeAllWithAuditDate auditDate policyPath gvdYear (bindings: SourceBinding array) baseBundle outputBundle =
    use scratch = new Scratch.Storage(System.IO.Path.GetTempPath())
    let combined = MultiSourcePreparation.prepare scratch.Directory policyPath baseBundle bindings
    let prepared = InputPreparation.prepare {
        scratch = scratch
        auditDate = auditDate
        policyPath = combined.policyPath
        gvdYear = gvdYear
        binding = combined.binding
        baseBundle = baseBundle
        outputBundle = outputBundle
    }
    let source, matches = analyzeSource prepared
    Runtime.releaseAnalysisMemory ()
    let projection = Projection.resolve { prepared = prepared; source = source; matches = matches }
    let result = BundleWriter.write { prepared = prepared; projection = projection; source = source; matches = matches; gvdYear = gvdYear }
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

let executeAll policyPath gvdYear bindings baseBundle outputBundle =
    executeAllWithAuditDate None policyPath gvdYear bindings baseBundle outputBundle
