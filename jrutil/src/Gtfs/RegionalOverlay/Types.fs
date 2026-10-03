// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module JrUtil.RegionalOverlay.Types

open System.Collections.Generic

[<Literal>]
let OverlayBundleVersion = 2

[<Literal>]
let OverlayPolicySchemaVersion = 4

[<Literal>]
let OverlayAllPolicySchemaVersion = 1

[<CLIMutable>]
type RouteJoinPolicy = {
    table: string
    sourceKeys: string array
    lookupKeys: string array
    valueColumn: string
    targetNamespace: string
}

[<CLIMutable>]
type StopMatchPolicy = {
    groupColumn: string
    postColumn: string
    maximumDistanceMetres: float
    coordinateIdentityMaximumMetres: float
    coordinateIdentityMinimumMarginMetres: float
    splitFlatGroupsByName: bool
    contextualInference: ContextualStopInferencePolicy
}

and [<CLIMutable>] ContextualStopInferencePolicy = {
    enabled: bool
    minimumMappedCalls: int
}

[<CLIMutable>]
type PatternEditPolicy = {
    enabled: bool
    maximumEdits: int
    minimumAgreement: float
    requireSameEndpoints: bool
}

[<CLIMutable>]
type SourceRevisionPolicy = {
    column: string
    regex: string
    dateFormat: string
}

[<CLIMutable>]
type TripSetAuthorityPolicy = {
    modes: string array
    sourceNativeModes: string array
}

[<CLIMutable>]
type TripMatchPolicy = {
    timeResolutionSeconds: int
    exactPatternProximity: bool
    patternEdit: PatternEditPolicy
    sourceRevision: SourceRevisionPolicy
    minimumCapabilityTier: Dictionary<string, string>
}

[<CLIMutable>]
type OverridePolicy = {
    stops: string
}

[<CLIMutable>]
type SourcePolicy = {
    sourceId: string
    excludedRouteTypes: string array
    maximumSnapshotSkewDays: int
    routeJoin: RouteJoinPolicy
    stopMatch: StopMatchPolicy
    tripMatch: TripMatchPolicy
    tripSetAuthority: TripSetAuthorityPolicy
    routeMatchTiers: string array
    tripMatchTiers: string array
    /// Enabled capabilities; everything else is inherited from the base.
    capabilities: string array
    overrides: OverridePolicy
}

[<CLIMutable>]
type OverlayPolicy = {
    schemaVersion: int
    source: SourcePolicy
}

[<CLIMutable>]
type OverlayAllSourcePolicy = {
    sourceId: string
    policy: string
    adapter: string
}

[<CLIMutable>]
type OverlayAllPolicy = {
    schemaVersion: int
    sources: OverlayAllSourcePolicy array
}

type SourceBinding = {
    sourceId: string
    payloadPath: string
    descriptorPath: string
}

type OverlayResult = {
    outputPath: string
    matchedTrips: int
    unmatchedTrips: int
    ambiguousTrips: int
    matchedStopGroups: int
    unmatchedStopGroups: int
    selectedShapes: int
    selectedTransfers: int
}

type CompilationOptions = {
    auditDate: NodaTime.LocalDate option
    policyPath: string
    gvdYear: int
    bindings: SourceBinding array
    baseBundle: string
    outputBundle: string
    converterVersion: string
    diagnosticsOutput: string option
    diagnosticTraces: bool
    /// Pins source-native stop-place IDs (`overlay_places.csv`) when set.
    stopRegistry: JrUtil.StopRegistry.StopRegistry option
    /// Review CSV for source-native stop places that are not pinned yet.
    stopRegistryCandidatesPath: string option
}

type MultiSourceOverlayResult = {
    outputPath: string
    sources: string array
    aggregate: OverlayResult
}
