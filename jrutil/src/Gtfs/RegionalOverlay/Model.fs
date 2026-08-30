// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.Model

open System
open System.Collections.Generic
open System.IO
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
    method: string
    sourceCalls: CallValue array
    sourceShapeId: string option
    sourceOrdinalByTarget: int option array
    editCount: int
    firstDepartureDelta: int
    aggregateTimeDelta: int64
    durationDelta: int
    runnerUpMargin: int64 option
    completePattern: bool
}

type OverlaySelection = {
    factKey: string
    sourceId: string
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
    tier: string
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
    routeMethod: string
    candidates: CandidateMatch array
}

type TripSlice = {
    trip: Trip
    dates: DateSet.Dates
    selection: OverlaySelection option
}

type SourceTripProjection = {
    sourceId: string
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
