// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.Model

open System
open System.Collections.Generic
open NodaTime

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types

type SourceDescriptor = {
    retrievedAt: DateTimeOffset
    payloadSha256: string
}

type DateWindow = {
    startDate: LocalDate
    endDate: LocalDate
    dates: LocalDate array
    index: Dictionary<LocalDate, int>
}

type CsvRow = Dictionary<string, string>

type CallValue = {
    stopId: string
    stopPlaceId: string
    arrival: string
    departure: string
    sequence: int
    distance: decimal option
    stopHeadsign: string option
}

/// How strongly a source trip's calls agree with a base trip's, strongest first.
type TripTier =
    | FullSignature
    | PatternEndpoints
    | PatternFirst
    | PatternNearest
    | PatternEditNearest
    /// Used only while contextual stop inference scores its candidates.
    | ContextStopInference

module TripTier =
    let name tier =
        match tier with
        | FullSignature -> "full_signature"
        | PatternEndpoints -> "pattern_endpoints"
        | PatternFirst -> "pattern_first"
        | PatternNearest -> "pattern_nearest"
        | PatternEditNearest -> "pattern_edit_nearest"
        | ContextStopInference -> "context_stop_inference"

    /// The tiers a policy may list in `trip_match_tiers`.
    let configurable = [| FullSignature; PatternEndpoints; PatternFirst; PatternNearest; PatternEditNearest |]

    let parse (value: string) =
        match configurable |> Array.tryFind (fun tier -> name tier = value) with
        | Some tier -> tier
        | None -> invalidArg "--policy" $"Unknown trip matching tier: {value}"

/// What ties a source route to its base routes.
type RouteMethod =
    | CompanionAssertion
    | StructuralTripEvidence

module RouteMethod =
    let name method =
        match method with
        | CompanionAssertion -> "companion_assertion"
        | StructuralTripEvidence -> "structural_trip_evidence"

    let all = [| CompanionAssertion; StructuralTripEvidence |]

type BaseSignature = {
    tripId: string
    full: string
    patternEndpoints: string
    patternFirst: string
    pattern: string array
    arrivals: int array
    departures: int array
    overlayEquivalenceDigest: string
}

type MatchBinding = {
    sourceId: string
    sourceTripId: string
    targetTripId: string
    dates: DateSet.Dates
    routeMethod: RouteMethod
    tier: TripTier
    sourceCalls: CallValue array
    sourceShapeId: string option
    sourceOrdinalByTarget: int option array
    editCount: int
    firstDepartureDelta: int
    aggregateTimeDelta: int64
    durationDelta: int
    runnerUpMargin: int64 option
}
    with
    /// The method name reports and mappings carry.
    member this.method = RouteMethod.name this.routeMethod + "+" + TripTier.name this.tier

type OverlaySelection = {
    factKey: string
    sourceId: string
    sourceIds: string array
    sourceTripId: string
    sourceStopIds: string array
    outputStopIds: string array
    sourceShapeId: string option
    outputShapeId: string option
    distances: decimal option array
    sourceOrdinalByTarget: int option array
    arrivals: string option array
    departures: string option array
}

type StopInferenceEvidence = {
    sourceGroupId: string
    sourceTripId: string
    targetStopPlaceId: string
    sourceCallOrdinal: int
    method: string
}

type CandidateScoreReport = {
    sourceTripId: string
    targetTripId: string
    date: LocalDate
    routeMethod: string
    tripMethod: string
    editCount: int
    alignedCallCount: int
    firstDepartureDelta: int
    aggregateTimeDelta: int64
    durationDelta: int
    maximumTimeDelta: int
    squaredTimeDelta: int64
    runnerUpMargin: int64 option
}

type CandidateMatch = {
    tripId: string
    tier: TripTier
    sourceOrdinalByTarget: int option array
    editCount: int
    alignedCallCount: int
    firstDepartureDelta: int
    aggregateTimeDelta: int64
    durationDelta: int
    maximumTimeDelta: int
    squaredTimeDelta: int64
}

type PendingAmbiguity = {
    sourceTripId: string
    dateIndex: int
    routeMethod: RouteMethod
    candidates: CandidateMatch array
}

type TripSlice = {
    trip: Trip
    dates: DateSet.Dates
    selection: OverlaySelection option
}

type SourceTripProjection = {
    sourceId: string
    sourceIds: string array
    sourceTripReferences: (string * string) array
    sourceTripId: string
    sourceRouteId: string
    targetRouteId: string
    cisLineId: string
    dates: DateSet.Dates
    sourceCalls: CallValue array
    sourceShapeId: string option
    sourceRow: CsvRow
}

type SourceTripAddition = {
    projection: SourceTripProjection
    trip: Trip
}

type Diagnostic = {
    code: string
    sourceObjectId: string
    message: string
}


/// Diagnostics remain readable after matching; only suppression keys stay in memory.
type DiagnosticLog(storage: Scratch.Storage) =
    let rows = new Scratch.RowLog(storage)
    member _.Add(value: Diagnostic) = rows.Add [| value.code; value.sourceObjectId; value.message |]
    member _.Rows = rows.Rows |> Seq.map (fun row -> { code = row.[0]; sourceObjectId = row.[1]; message = row.[2] })
