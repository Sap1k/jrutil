// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module JrUtil.RegionalGtfsOverlay

open JrUtil.RegionalOverlay
open JrUtil.RegionalOverlay.Types

/// Load and validate one source's overlay profile (policy schema v4).
let loadPolicy path = Policy.loadPolicy path

let resolveFullHeadsign raw destinations = Support.resolveFullHeadsign raw destinations

// A method boundary ends the lifetime of matching-only indexes before projection.
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private analyzeSource prepared =
    let source, indexes = SourceAnalysis.analyze { prepared = prepared }
    let matches = TripMatching.matchTrips { prepared = prepared; source = source; indexes = indexes }
    source, matches

/// Overlay the sources declared by an overlay-all policy onto a national
/// package. The sources are merged into one namespaced input and resolved in a
/// single pass.
let compile (options: CompilationOptions) =
    if isNull options.bindings || options.bindings.Length = 0 then
        invalidArg "options" "At least one source binding is required"
    use scratch = new Scratch.Storage(System.IO.Path.GetTempPath())
    let combined = MultiSourcePreparation.prepare scratch.Directory options.policyPath options.baseBundle options.bindings
    let prepared = InputPreparation.prepare {
        scratch = scratch
        auditDate = options.auditDate
        policy = combined.policy
        gvdYear = options.gvdYear
        binding = combined.binding
        verifiedPayloadSha256 = Some combined.payloadSha256
        baseBundle = options.baseBundle
        outputBundle = options.outputBundle
    }
    let source, matches = analyzeSource prepared
    Runtime.releaseAnalysisMemory ()
    let projection = Projection.resolve { prepared = prepared; source = source; matches = matches }
    let result = BundleWriter.write {
        prepared = prepared; projection = projection; source = source; matches = matches
        gvdYear = options.gvdYear; converterVersion = options.converterVersion
        diagnosticsOutput = options.diagnosticsOutput
        diagnosticTraces = options.diagnosticTraces }
    { outputPath = result.outputPath; sources = combined.sourceIds; aggregate = result }
