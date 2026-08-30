// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module JrUtil.RegionalGtfsOverlay

open JrUtil.RegionalOverlay
open JrUtil.RegionalOverlay.Types

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
