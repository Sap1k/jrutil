// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Bundle options, progress events, results and snapshot descriptors.
module JrUtil.JdfBundleModel

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open NodaTime
open Parquet
open Parquet.Schema
open Parquet.Serialization
open Serilog
open JrUtil

type PostInferenceExecutionMode = Disabled | CaptureOnly | Live

type PostInferenceCaptureMetrics = {
    estimatedEvidenceBytes: int64
    atomicOutputHeadroomBytes: int64
    currentSpillBytes: int64
    peakSpillBytes: int64
    maximumWorkers: int
}

type BundleExecutionResult =
    | BundleCompleted
    | CaptureCompleted of JdfPostInference.PostInferenceEvidenceManifest * PostInferenceCaptureMetrics

[<Literal>]
let BundleVersion = 1

[<Literal>]
let ParquetSchemaVersion = 7

[<Literal>]
let PostEvidenceOutputSafetyReserveBytes = 268435456L

type SnapshotDescriptor = {
    sourceId: string
    retrievedAt: string
    retrievalMethod: string
    sourceUri: string option
    licence: string
    payloadKind: string
    payloadSha256: string
    payloadBytes: int64
}

type Diagnostic = {
    severity: string
    code: string
    sourceObjectId: string
    message: string
}

let estimatePostEvidenceOutputBytes
        (bounds:JdfPostEvidence.PostEvidenceCaptureUpperBounds) =
    // These deliberately conservative encoded-row allowances are applied to
    // the canonical, deduplicated capture plan.  Timetable call count is not
    // evidence cardinality: identical trip patterns share one routed context.
    bounds.observationCount*384L
    + bounds.routePointCount*320L
    + bounds.contextCount*1024L
    + bounds.corridorVariantCount*512L
    + bounds.routePointEvidenceCount*384L

type internal ParquetTable = {
    fields: DataField array
    rows: IReadOnlyCollection<IDictionary<string, obj>>
}

type internal DerivedPostAssignmentRow = {
    targetGtfsStopId: string
    assignmentKind: string
    derivedLocationId: string option
    mode: string
    lineId: string option
    direction: int option
    patternHash: string option
    patternPosition: int option
    movementFamilyId: string option
    contextPreviousStopId: string option
    contextNextStopId: string option
    sameStopBlockRole: string
    score: double option
    margin: double option
    selectedCandidates: string
    rejectedCandidates: string
    status: string
}

type BundleProgressEvent = {
    phase: string
    state: string
    completed: int64
    total: int64 option
    unit: string
    detail: string option
    elapsedMilliseconds: int64
    activeWorkers: int
    privateBytes: int64
    workingSetBytes: int64
}

type BundleOptions = {
    /// Retrieval provenance and input checksum JSON
    snapshotDescriptorPath: string
    /// Exact JrUtil version or commit recorded as the package compiler
    converterVersion: string
    internationalPolicy: JdfGtfsRules.InternationalRoutePolicy
    transportModeRules: JdfGtfsRules.TransportModeRuleSet
    estimatedPosts: bool
    routingPbfPath: string option
    maximumWorkers: int
    /// Post-inference routing workers. The routing graph is memory-mapped and
    /// shared, and the capture stores divide the memory budget between workers,
    /// so routing is not bound by the per-worker allowance of the other phases.
    routingWorkers: int
    /// Directory keeping routed post-inference evidence between runs.
    routingCachePath: string option
    memoryBudgetBytes: int64
    capturePostInferenceEvidencePath: string option
    postInferenceEvidenceOnly: bool
    captureRestriction: JdfPostEvidence.CaptureRestriction
    exportPostContextCallsPath: string option
    postInferencePolicyPath: string option
    diagnosticsOutput: string option
    diagnosticTraces: bool
    progress: BundleProgressEvent -> unit
    /// GVD the merged JDF was bounded to (merge-jdf --gvd-year); recorded in the manifest
    gvdYear: int option
}

/// Callers must set snapshotDescriptorPath and converterVersion.
let defaultBundleOptions = {
    snapshotDescriptorPath = ""
    converterVersion = ""
    internationalPolicy = JdfGtfsRules.KeepAll
    transportModeRules = JdfGtfsRules.emptyTransportModeRules
    estimatedPosts = true
    routingPbfPath = None
    maximumWorkers = 1
    routingWorkers = 1
    routingCachePath = None
    memoryBudgetBytes = Int64.MaxValue
    capturePostInferenceEvidencePath = None
    postInferenceEvidenceOnly = false
    captureRestriction = JdfPostEvidence.noCaptureRestriction
    exportPostContextCallsPath = None
    postInferencePolicyPath = None
    diagnosticsOutput = None
    diagnosticTraces = false
    progress = ignore
    gvdYear = None
}
