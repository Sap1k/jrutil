// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module JrUtil.RegionalOverlay.Types

open System.Collections.Generic

[<Literal>]
let OverlayBundleVersion = 1

[<Literal>]
let MultiSourceOverlayBundleVersion = 2

[<Literal>]
let OverlayPolicySchemaVersion = 3

[<Literal>]
let OverlayAllPolicySchemaVersion = 1

[<CLIMutable>]
type CapabilityPolicy = {
    mode: string
    priority: int
}

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
    maximumUnresolvedGroupsPerTrip: int
    requireEqualCallCount: bool
    minimumMappedCalls: int
    conflictPolicy: string
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
    routeMatchTier: string
}

[<CLIMutable>]
type TripMatchPolicy = {
    timeResolutionSeconds: int
    timeRounding: string
    exactPatternProximity: bool
    requireUniqueBest: bool
    patternEdit: PatternEditPolicy
    sourceRevision: SourceRevisionPolicy
    minimumCapabilityTier: Dictionary<string, string>
}

[<CLIMutable>]
type OverridePolicy = {
    routes: string
    trips: string
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
    neverInherit: string array
    capabilities: Dictionary<string, CapabilityPolicy>
    overrides: OverridePolicy
}

[<CLIMutable>]
type OverlayPolicy = {
    schemaVersion: int
    calibration: bool
    publicationEnabled: bool
    minimumCoverage: Dictionary<string, float>
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
    calibration: bool
    publicationEnabled: bool
    conflictPolicy: string
    minimumCoverage: Dictionary<string, float>
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

type MultiSourceOverlayResult = {
    outputPath: string
    sources: string array
    aggregate: OverlayResult
    perSource: IReadOnlyDictionary<string, OverlayResult>
}
