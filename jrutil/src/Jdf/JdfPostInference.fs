// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfPostInference

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json

open JrUtil.JdfPostInferencePolicy

[<Literal>]
let EvidenceFormat = "post-inference-evidence-v2"

[<Literal>]
let EvidenceSchemaVersion = 2

[<Literal>]
let EvaluatorVersion = "post-estimator-v2"

[<Literal>]
let RouterEvidenceVersion = "packed-directed-v3"

[<Literal>]
let VariantEnumerationVersion = "directed-thread-top3-v1"

[<Literal>]
let CaptureMaximumSearchStates = 100000

[<Literal>]
let CaptureMaximumSearchDistanceMetres = 30000.0

[<Literal>]
let CaptureRoutedExcessHorizonMetres = 1000.0

[<Literal>]
let CaptureMaximumCorridorVariants = 3

[<Literal>]
let CaptureAlignmentRangeDegrees = 90.0

[<Literal>]
let CaptureProximityRangeMetres = 50.0

let RequiredEvidenceFiles = [|
    "observations.parquet"
    "route_points.parquet"
    "contexts.parquet"
    "corridor_variants.parquet"
    "route_point_evidence.parquet"
|]

type SameStopPairChoice = {
    candidateId:string
    ordinaryScore:float
    ordinaryGeometry:float
    individualMargin:float
    independentlyPublishable:bool
}

type SameStopBlockFacts = {
    contextCount:int
    leftAssignmentKind:string
    rightAssignmentKind:string
    leftMovementFamilyId:string
    rightMovementFamilyId:string
    leftResolution:string
    rightResolution:string
    leftCandidateId:string option
    rightCandidateId:string option
}

let sameStopDistinctnessEligible facts =
    facts.contextCount=2
    && facts.leftAssignmentKind="unlabelled"
    && facts.rightAssignmentKind="unlabelled"
    && facts.leftMovementFamilyId<>facts.rightMovementFamilyId
    && facts.leftResolution="Physical"
    && facts.rightResolution="Physical"
    && facts.leftCandidateId.IsSome
    && facts.leftCandidateId=facts.rightCandidateId

let selectDistinctSameStopPair (policy:SameStopPairPolicy)
                               (left:SameStopPairChoice array)
                               (right:SameStopPairChoice array) =
    let eligible values =
        let gated =
            values
            |> Array.filter(fun value ->
                value.independentlyPublishable
                && value.ordinaryScore>=policy.minimumIndividualScore
                && value.individualMargin>=policy.minimumIndividualMargin)
        if gated.Length=0 then [||] else
        let bestScore=gated |> Array.maxBy _.ordinaryScore |> _.ordinaryScore
        let scoreTies=gated |> Array.filter(fun value -> abs(value.ordinaryScore-bestScore)<=1e-12)
        let bestGeometry=scoreTies |> Array.maxBy _.ordinaryGeometry |> _.ordinaryGeometry
        scoreTies
        |> Array.filter(fun value -> abs(value.ordinaryGeometry-bestGeometry)<=1e-12)
        |> Array.sortBy _.candidateId
    if not policy.enabled then None else
    let left=eligible left
    let right=eligible right
    seq {
        for leftChoice in left do
            for rightChoice in right do
                if leftChoice.candidateId<>rightChoice.candidateId then
                    yield leftChoice.candidateId,rightChoice.candidateId
    }
    |> Seq.sortBy id |> Seq.tryHead

type EvidenceFileManifest = {
    path: string
    sha256: string
    bytes: int64
    rows: int64
    schemaFingerprint: string
}

type CaptureCeilings = {
    routedExcessMetres: float
    maximumCorridorVariants: int
}

type PostInferenceEvidenceManifest = {
    evidenceFormat: string
    schemaVersion: int
    packId: string
    captureToolVersion: string
    mergedJdfSha256: string
    routingPbfSha256: string
    osmSnapshot: string option
    routerEvidenceVersion: string
    variantEnumerationVersion: string
    captureCeilings: CaptureCeilings
    maximumSearchStates: int
    maximumSearchDistanceMetres: float
    contextCount: int64
    routePointCount: int64
    observationCount: int64
    corridorVariantCount: int64
    routePointEvidenceCount: int64
    files: EvidenceFileManifest array
}

type IReplayableRowStore<'T> =
    inherit IDisposable
    abstract ReadRows: unit -> seq<'T>
    abstract Count:int64
    abstract CurrentSpillBytes:int64
    abstract PeakSpillBytes:int64

type private ReplayableStorage<'T> =
    | MemoryChunks of 'T array array
    | BinarySpool of string

type private PeakAdjustedRowStore<'T>(inner:IReplayableRowStore<'T>,peakSpillBytes:int64) =
    interface IReplayableRowStore<'T> with
        member _.ReadRows()=inner.ReadRows()
        member _.Count=inner.Count
        member _.CurrentSpillBytes=inner.CurrentSpillBytes
        member _.PeakSpillBytes=max inner.PeakSpillBytes peakSpillBytes
        member _.Dispose()=inner.Dispose()

/// A typed, repeatable row sequence. Small results stay in bounded chunks;
/// larger results use a private length-prefixed spool that is removed by
/// Dispose even when a reader is abandoned.
type ReplayableRowStore<'T> private (storage:ReplayableStorage<'T>,count:int64,
                                     currentSpillBytesAtCreation:int64,
                                     peakSpillBytes:int64) =
    let mutable disposed=false
    let mutable currentSpillBytes=currentSpillBytesAtCreation

    let throwIfDisposed () =
        if disposed then raise(ObjectDisposedException(nameof ReplayableRowStore))

    member _.ReadRows() =
        throwIfDisposed()
        match storage with
        | MemoryChunks chunks ->
            chunks |> Seq.collect(fun chunk -> chunk :> seq<'T>)
        | BinarySpool path ->
            seq {
                use stream =
                    new FileStream(path,FileMode.Open,FileAccess.Read,FileShare.Read,
                                   64*1024,FileOptions.SequentialScan)
                use reader=new BinaryReader(stream,Encoding.UTF8,false)
                while stream.Position<stream.Length do
                    let length=reader.ReadInt32()
                    if length<0 || int64 length>stream.Length-stream.Position then
                        invalidOp "Replayable row spool is truncated or corrupt"
                    let payload=reader.ReadBytes(length)
                    if payload.Length<>length then
                        invalidOp "Replayable row spool ended inside a row"
                    let row=JsonSerializer.Deserialize<'T>(payload)
                    if isNull(box row) then invalidOp "Replayable row spool contains an empty row"
                    yield row
            }

    member _.Count=count
    member _.CurrentSpillBytes=currentSpillBytes
    member _.PeakSpillBytes=peakSpillBytes

    member _.Dispose() =
        if not disposed then
            disposed<-true
            match storage with
            | BinarySpool path ->
                try
                    if File.Exists(path) then File.Delete(path)
                finally
                    currentSpillBytes<-0L
            | MemoryChunks _ -> ()

    interface IReplayableRowStore<'T> with
        member this.ReadRows()=this.ReadRows()
        member this.Count=this.Count
        member this.CurrentSpillBytes=this.CurrentSpillBytes
        member this.PeakSpillBytes=this.PeakSpillBytes
        member this.Dispose()=this.Dispose()

    static member Create(memoryBudgetBytes:int64,temporaryDirectory:string,
                         rows:seq<'T>) : IReplayableRowStore<'T> =
        if memoryBudgetBytes<=0L then
            invalidArg "memoryBudgetBytes" "Replayable-row memory budget must be positive"
        if String.IsNullOrWhiteSpace temporaryDirectory then
            invalidArg "temporaryDirectory" "Replayable-row temporary directory is required"
        let chunks=ResizeArray<'T array>()
        let pending=ResizeArray<struct('T*byte array)>()
        let mutable pendingBytes=0L
        let mutable count=0L
        let mutable spoolPath:string option=None
        let mutable spoolStream:FileStream option=None
        let mutable spoolWriter:BinaryWriter option=None
        let writePayload (writer:BinaryWriter) (payload:byte array) =
            writer.Write(payload.Length)
            writer.Write(payload)
        let openSpool () =
            Directory.CreateDirectory(temporaryDirectory) |> ignore
            let requiredHeadroom=max (64L*1024L*1024L) (2L*max memoryBudgetBytes pendingBytes)
            let root=Path.GetPathRoot(Path.GetFullPath(temporaryDirectory))
            if not(String.IsNullOrEmpty root) then
                let drive=DriveInfo(root)
                if drive.IsReady && drive.AvailableFreeSpace<requiredHeadroom then
                    invalidOp
                        $"Insufficient temporary disk space for replayable rows: {drive.AvailableFreeSpace} bytes available, {requiredHeadroom} required"
            let path=Path.Combine(temporaryDirectory,$"post-inference-rows-{Guid.NewGuid():N}.bin")
            let stream =
                new FileStream(path,FileMode.CreateNew,FileAccess.Write,FileShare.None,
                               64*1024,FileOptions.SequentialScan)
            let writer=new BinaryWriter(stream,Encoding.UTF8,true)
            spoolPath<-Some path
            spoolStream<-Some stream
            spoolWriter<-Some writer
            for struct(_,payload) in pending do writePayload writer payload
            pending.Clear()
            pendingBytes<-0L
        try
            for row in rows do
                let payload=JsonSerializer.SerializeToUtf8Bytes(row)
                let admittedBytes=4L+int64 payload.Length
                match spoolWriter with
                | Some writer -> writePayload writer payload
                | None when pendingBytes+admittedBytes>memoryBudgetBytes ->
                    openSpool()
                    writePayload spoolWriter.Value payload
                | None ->
                    pending.Add(struct(row,payload))
                    pendingBytes<-pendingBytes+admittedBytes
                count<-count+1L
            match spoolWriter,spoolStream,spoolPath with
            | Some writer,Some stream,Some path ->
                writer.Flush()
                stream.Flush(true)
                let bytes=stream.Length
                writer.Dispose()
                stream.Dispose()
                spoolWriter<-None
                spoolStream<-None
                new ReplayableRowStore<'T>(BinarySpool path,count,bytes,bytes)
                :> IReplayableRowStore<'T>
            | _ ->
                pending
                |> Seq.map(fun struct(value,_) -> value)
                |> Seq.chunkBySize 4096
                |> Seq.iter chunks.Add
                new ReplayableRowStore<'T>(MemoryChunks(chunks.ToArray()),count,0L,0L)
                :> IReplayableRowStore<'T>
        with _ ->
            spoolWriter |> Option.iter _.Dispose()
            spoolStream |> Option.iter _.Dispose()
            spoolPath |> Option.iter(fun path -> if File.Exists(path) then File.Delete(path))
            reraise()

    /// External canonical sort using bounded in-memory runs, deterministic
    /// binary run spools and a stable k-way merge into one replayable store.
    static member CreateSorted<'Key when 'Key: comparison>
            (memoryBudgetBytes:int64,temporaryDirectory:string,
             keySelector:'T -> 'Key,rows:seq<'T>) : IReplayableRowStore<'T> =
        if memoryBudgetBytes<=0L then
            invalidArg "memoryBudgetBytes" "Replayable-row sort budget must be positive"
        let runs=ResizeArray<IReplayableRowStore<'T>>()
        let pending=ResizeArray<'T>()
        let mutable pendingBytes=0L
        let disposeRuns () =
            for run in runs do
                try run.Dispose() with _ -> ()
            runs.Clear()
        let flushRun () =
            if pending.Count>0 then
                let values=pending.ToArray()
                Array.sortInPlaceBy keySelector values
                let run=ReplayableRowStore<'T>.Create(1L,temporaryDirectory,values)
                runs.Add(run)
                pending.Clear()
                pendingBytes<-0L
        try
            for row in rows do
                let estimatedBytes=4L+int64(JsonSerializer.SerializeToUtf8Bytes(row).Length)
                if pending.Count>0 && pendingBytes+estimatedBytes>memoryBudgetBytes then
                    flushRun()
                pending.Add(row)
                pendingBytes<-pendingBytes+estimatedBytes
            if runs.Count=0 then
                let values=pending.ToArray()
                Array.sortInPlaceBy keySelector values
                ReplayableRowStore<'T>.Create(memoryBudgetBytes,temporaryDirectory,values)
            else
                flushRun()
                let merged=ReplayableRowStore<'T>.MergeSorted(
                    memoryBudgetBytes,temporaryDirectory,keySelector,runs.ToArray())
                disposeRuns()
                merged
        with _ ->
            disposeRuns()
            reraise()

    /// Stable k-way merge of already sorted replayable inputs. Inputs remain
    /// owned by the caller; the returned store owns only the merged output.
    static member MergeSorted<'Key when 'Key: comparison>
            (memoryBudgetBytes:int64,temporaryDirectory:string,
             keySelector:'T -> 'Key,
             stores:IReplayableRowStore<'T> array) : IReplayableRowStore<'T> =
        if memoryBudgetBytes<=0L then
            invalidArg "memoryBudgetBytes" "Replayable-row merge budget must be positive"
        let transientSpillBytes=stores |> Seq.sumBy _.CurrentSpillBytes
        let mergedRows = seq {
            let enumerators=stores |> Array.map(fun store -> store.ReadRows().GetEnumerator())
            try
                let active=Array.zeroCreate<bool> enumerators.Length
                let current=Array.zeroCreate<'T> enumerators.Length
                for index=0 to enumerators.Length-1 do
                    if enumerators.[index].MoveNext() then
                        active.[index]<-true
                        current.[index]<-enumerators.[index].Current
                while active |> Array.exists id do
                    let selected=
                        seq { for index=0 to active.Length-1 do
                                  if active.[index] then
                                      yield keySelector current.[index],index }
                        |> Seq.minBy id |> snd
                    yield current.[selected]
                    if enumerators.[selected].MoveNext() then
                        current.[selected]<-enumerators.[selected].Current
                    else active.[selected]<-false
            finally
                for enumerator in enumerators do enumerator.Dispose()
        }
        let merged=ReplayableRowStore<'T>.Create(memoryBudgetBytes,temporaryDirectory,mergedRows)
        let peak=transientSpillBytes+merged.PeakSpillBytes
        new PeakAdjustedRowStore<'T>(merged,peak) :> IReplayableRowStore<'T>

type ConsolidatedPostHypothesis = {
    hypothesisId:string;stopId:int64;representativeRoutePointId:string
    memberRoutePointIds:string array;memberObservationIds:string array
    latitude:float;longitude:float
}

type GlobalPostSideGroup = {
    sideGroupId:string;stopId:int64;mode:string;corridorFaceId:string
    memberHypothesisIds:string array;representativeHypothesisId:string
    sector:string;latitude:float;longitude:float
    compactnessMetres:float;support:int
}

type ContextPostAssignment = {
    contextId:string;stopId:int64;mode:string;lineId:string;routeDistinction:int
    direction:int;patternHash:string;patternPosition:int
    previousStopId:int64 option;nextStopId:int64 option
    assignmentKind:string;authoredPostKey:string option;sameStopBlockId:string option
    sameStopBlockRole:string;movementFamilyId:string;resolution:string
    selectedLocationId:string option;selectedHypothesisId:string option
    selectedSideGroupId:string option;score:float option;margin:float option
}

type AuthoredPostPosition = {
    stopId:int64;authoredPostKey:string;locationId:string
    latitude:float option;longitude:float option
}

type PostInferenceDiagnosticScore = {
    contextId:string;candidateId:string;variantRank:int;eligible:bool
    alignment:float;side:float;proximity:float;routedExcess:float
    routedExcessMetres:float option;corridorId:string option
    ingressThreadId:string option;egressThreadId:string option
    corridorFaceId:string option;routingAvailability:string
    alternativeCorridorCount:int;alternativeCostGap:float option
    selectedCorridorRanks:int array;tiedCorridorsAgree:bool
    corridorDistance:float option;signedLateralOffset:float option
    corridorHeading:float option;attachmentHeading:float option
    snapEdgeId:int option;snapFraction:float option
    topologyFailureReason:string option
    sourceAdjustment:float;modalityAdjustment:float;popularityAdjustment:float
    total:float;rejectionReason:string option
}

type PostInferenceCounters = {
    evidenceRows:int64;contextCount:int;candidateStopCount:int
    unresolvedContexts:int;authoredPositions:int;sameStopBlocks:int
    distinctPairChoices:int;unresolvedBlockEdges:int
    physicalResolutions:int;sideResolutions:int;centroidResolutions:int
}

type PostInferenceResult(hypotheses:ConsolidatedPostHypothesis array,
                         sideGroups:GlobalPostSideGroup array,
                         assignments:IReplayableRowStore<ContextPostAssignment>,
                         authoredPositions:AuthoredPostPosition array,
                         diagnosticScores:IReplayableRowStore<PostInferenceDiagnosticScore>,
                         counters:PostInferenceCounters) =
    member _.Hypotheses=hypotheses
    member _.SideGroups=sideGroups
    member _.Assignments=assignments
    member _.AuthoredPositions=authoredPositions
    member _.DiagnosticScores=diagnosticScores
    member _.Counters=counters
    interface IDisposable with
        member _.Dispose()=
            try assignments.Dispose()
            finally diagnosticScores.Dispose()

type AlternativeCorridorFact = {
    variantRank:int
    relativeCostMetres:float option
    relativeCostFraction:float option
}

type PhysicalResolutionFacts = {
    finalScore:float;margin:float;routedAdvantage:float;geometryMargin:float
    contradictory:bool;contextCount:int;winnerShare:float
    establishedStrong:bool;hasAlternativeCorridors:bool;alternativesAgree:bool
}

let withinConsolidationDiameter (policy:PostInferencePolicyV2) distanceMetres =
    distanceMetres<=policy.consolidation.maximumDiameterMetres

let consolidationSidesCompatible (policy:PostInferencePolicyV2)
                                 (left:float option) (right:float option) =
    match left,right with
    | Some leftSide,Some rightSide
        when abs leftSide>policy.consolidation.oppositeSideToleranceMetres
             && abs rightSide>policy.consolidation.oppositeSideToleranceMetres
             && Math.Sign(leftSide)<>Math.Sign(rightSide) -> false
    | _ -> true

let consolidationHeadingCompatible (policy:PostInferencePolicyV2) differenceDegrees =
    differenceDegrees<=policy.consolidation.maximumHeadingDifferenceDegrees

let consolidationChainageCompatible (policy:PostInferencePolicyV2) differenceMetres =
    differenceMetres<=policy.consolidation.maximumChainageDifferenceMetres

let consolidationContextDistinguishes (policy:PostInferencePolicyV2) leftScore rightScore =
    max leftScore rightScore>=policy.consolidation.distinguishedContextMinimumScore
    && abs(leftScore-rightScore)>=policy.consolidation.distinguishedContextMinimumMargin

let consolidationDistinguishedCountCompatible (policy:PostInferencePolicyV2) count =
    count<policy.consolidation.distinguishedContextMinimumCount

let consensusSatisfied (policy:PostInferencePolicyV2) contextCount winnerShare contradictory =
    contextCount>=policy.consensus.minimumContexts
    && winnerShare>=policy.consensus.minimumWinningShare
    && not contradictory

let physicalResolutionEligible (policy:PostInferencePolicyV2) facts =
    let materialRouted =
        facts.routedAdvantage>=policy.resolution.materialRoutedAdvantage
        && facts.geometryMargin>= -0.000001
    facts.finalScore>=policy.resolution.minimumPhysicalScore
    && (facts.margin>=policy.resolution.minimumPhysicalMargin || materialRouted)
    && not facts.contradictory
    && (facts.contextCount<policy.consensus.minimumContexts
        || consensusSatisfied policy facts.contextCount facts.winnerShare facts.contradictory
        || facts.establishedStrong)
    && (not facts.hasAlternativeCorridors || facts.alternativesAgree)

let plausibleCandidate (policy:PostInferencePolicyV2) topGeometry geometry finalScore =
    finalScore>=policy.resolution.minimumPlausibleScore
    && topGeometry-geometry<=policy.resolution.minimumPhysicalMargin

let spatialIsolationAdjustment (policy:PostInferencePolicyV2) distanceMetres =
    match distanceMetres with
    | None -> 0.0
    | Some _ when policy.spatialIsolation.minimumSeparationMetres=0.0 -> 0.0
    | Some distance ->
        policy.spatialIsolation.maximumAdjustment
        * min 1.0 (distance/policy.spatialIsolation.minimumSeparationMetres)

let modalitySupportAdjustments (policy:PostInferencePolicyV2)
                               mode supportWeight
                               (explicitModes:string array) (deniedModes:string array) =
    let compatible value =
        value="SHARED" || value=mode
        || (mode="A" && (value="BUS" || value="ROAD"))
        || (mode="E" && value="TRAM")
        || (mode="T" && (value="TROLLEYBUS" || value="BUS" || value="ROAD"))
    let source =
        min policy.modality.maximumSourceSupportAdjustment
            (max 0.0 (supportWeight-1.0)*policy.modality.sourceSupportPerWeight)
    let modality =
        if deniedModes |> Array.exists compatible then policy.modality.conflictAdjustment
        elif explicitModes |> Array.exists compatible then policy.modality.explicitSupportAdjustment
        elif explicitModes.Length=0 then policy.modality.estimatedSupportAdjustment
        else 0.0
    source,modality

let establishedPostStrong (policy:PostInferencePolicyV2)
                          supportingContexts runnerUpContexts geometryMargin
                          contradictory hasAlternatives alternativesAgree =
    supportingContexts>=policy.establishedPost.minimumSupportingContexts
    && float supportingContexts>=
       policy.establishedPost.minimumRunnerUpRatio*float(max 1 runnerUpContexts)
    && geometryMargin>= -policy.establishedPost.maximumGeometryDisadvantage
    && not contradictory
    && (not hasAlternatives || alternativesAgree)

let establishedPopularityAdjustment (policy:PostInferencePolicyV2) supportingContexts =
    min policy.establishedPost.maximumPopularityAdjustment
        (float supportingContexts*policy.establishedPost.perContextAdjustment)

let combinedSupportAdjustment (policy:PostInferencePolicyV2) sourceAdjustment modalityAdjustment =
    Math.Clamp(sourceAdjustment+modalityAdjustment,
               policy.modality.conflictAdjustment,
               policy.modality.maximumCombinedSupportingAdjustment)

let contradictoryContext (policy:PostInferencePolicyV2) score lead =
    score>=policy.consensus.minimumPerContextScore
    && lead>=policy.consensus.minimumPerContextLead

let sideResolutionEligible (policy:PostInferencePolicyV2)
                           plausibleCandidateCount distinctFaceCount
                           conflictingAlternative ordinaryGroupCount contextCount compactnessMetres =
    plausibleCandidateCount>0 && distinctFaceCount=1 && not conflictingAlternative
    && (ordinaryGroupCount<=policy.sideGroups.maximumOrdinaryGroups
        || contextCount>=policy.sideGroups.additionalGroupMinimumContexts)
    && compactnessMetres<=policy.sideGroups.maximumCompactnessMetres

let authoredResolutionEligible (policy:PostInferencePolicyV2)
                               relevantContextCount physicalContextCount
                               distinctCandidateCount (scoresAndMargins:(float*float option) array) =
    distinctCandidateCount=1
    && (not policy.authoredResolution.requireUnanimousContexts
        || (relevantContextCount>0 && physicalContextCount=relevantContextCount))
    && scoresAndMargins |> Array.forall(fun (score,margin) ->
        score>=policy.authoredResolution.minimumPhysicalScore
        && margin |> Option.defaultValue 0.0
                  |> fun value -> value>=policy.authoredResolution.minimumPhysicalMargin)

let selectedCorridorVariantRanks (policy:PostInferencePolicyV2) facts =
    facts
    |> Array.distinctBy _.variantRank
    |> Array.filter(fun fact ->
        fact.variantRank=0
        || (fact.variantRank<policy.alternativeCorridors.maximumVariants
            && ((fact.relativeCostMetres
                 |> Option.exists(fun gap -> gap<=policy.alternativeCorridors.absoluteTieMetres))
                || (fact.relativeCostFraction
                    |> Option.exists(fun gap -> gap<=policy.alternativeCorridors.relativeTieFraction)))))
    |> Array.map _.variantRank
    |> Array.sort

let private evidenceJsonOptions =
    JsonSerializerOptions(PropertyNamingPolicy=JsonNamingPolicy.SnakeCaseLower,
                          WriteIndented=true)

let private positive field value =
    if not(Double.IsFinite value) || value<=0.0 then
        invalidArg "evidenceDirectory" $"{field}: must be positive"

let private sha256File path =
    use stream=File.OpenRead(path)
    SHA256.HashData(stream) |> Convert.ToHexString |> _.ToLowerInvariant()

let evidencePackId (captureToolVersion:string) (mergedJdfSha256:string) (routingPbfSha256:string) =
    if String.IsNullOrWhiteSpace captureToolVersion then
        invalidArg "captureToolVersion" "Evidence capture tool version is required"
    String.Join("|",[|
        captureToolVersion
        mergedJdfSha256.ToLowerInvariant()
        routingPbfSha256.ToLowerInvariant()
        EvidenceFormat
        string EvidenceSchemaVersion
        RouterEvidenceVersion
        VariantEnumerationVersion
        CaptureRoutedExcessHorizonMetres.ToString(Globalization.CultureInfo.InvariantCulture)
        string CaptureMaximumCorridorVariants
        string CaptureMaximumSearchStates
        CaptureMaximumSearchDistanceMetres.ToString(Globalization.CultureInfo.InvariantCulture) |])
    |> Encoding.UTF8.GetBytes |> SHA256.HashData |> Convert.ToHexString
    |> fun value -> value.ToLowerInvariant()

let writeEvidenceManifest path (manifest:PostInferenceEvidenceManifest) =
    if manifest.evidenceFormat<>EvidenceFormat || manifest.schemaVersion<>EvidenceSchemaVersion then
        invalidArg "manifest" "Unsupported post-inference evidence manifest"
    File.WriteAllText(path,JsonSerializer.Serialize(manifest,evidenceJsonOptions)+"\n")

let loadEvidenceManifest evidenceDirectory =
    let path=Path.Combine(evidenceDirectory,"manifest.json")
    if not(File.Exists path) then invalidArg "evidenceDirectory" $"Post-inference evidence manifest does not exist: {path}"
    use document=JsonDocument.Parse(File.ReadAllText(path))
    let format =
        match document.RootElement.TryGetProperty("evidence_format") with
        | true,value -> value.GetString()
        | _ -> null
    if format<>EvidenceFormat then
        invalidArg "evidenceDirectory" $"Unsupported post-inference evidence format '{format}'. Evidence v1 is intentionally rejected; recapture v2."
    let propertyNames (element:JsonElement) =
        element.EnumerateObject() |> Seq.map _.Name |> Set.ofSeq
    let expectedManifestProperties=Set.ofArray [|
        "evidence_format";"schema_version";"pack_id";"capture_tool_version"
        "merged_jdf_sha256";"routing_pbf_sha256";"osm_snapshot"
        "router_evidence_version";"variant_enumeration_version";"capture_ceilings"
        "maximum_search_states";"maximum_search_distance_metres";"context_count"
        "route_point_count";"observation_count";"corridor_variant_count"
        "route_point_evidence_count";"files" |]
    if document.RootElement.ValueKind<>JsonValueKind.Object
       || propertyNames document.RootElement<>expectedManifestProperties then
        invalidArg "evidenceDirectory" "Evidence manifest shape is obsolete or incomplete; recapture evidence v2"
    let ceilings=document.RootElement.GetProperty("capture_ceilings")
    if ceilings.ValueKind<>JsonValueKind.Object
       || propertyNames ceilings<>Set.ofArray[|"routed_excess_metres";"maximum_corridor_variants"|] then
        invalidArg "evidenceDirectory" "Evidence capture-ceiling manifest shape is invalid"
    let expectedFileProperties=Set.ofArray[|"path";"sha256";"bytes";"rows";"schema_fingerprint"|]
    let filesElement=document.RootElement.GetProperty("files")
    if filesElement.ValueKind<>JsonValueKind.Array
       || filesElement.EnumerateArray() |> Seq.exists(fun entry ->
            entry.ValueKind<>JsonValueKind.Object || propertyNames entry<>expectedFileProperties) then
        invalidArg "evidenceDirectory" "Evidence file-entry manifest shape is invalid"
    let manifest=JsonSerializer.Deserialize<PostInferenceEvidenceManifest>(document.RootElement.GetRawText(),evidenceJsonOptions)
    if isNull(box manifest) || manifest.schemaVersion<>EvidenceSchemaVersion then
        invalidArg "evidenceDirectory" $"Unsupported post-inference evidence manifest: {path}"
    if manifest.routerEvidenceVersion<>RouterEvidenceVersion
       || manifest.variantEnumerationVersion<>VariantEnumerationVersion then
        invalidArg "evidenceDirectory" "Evidence router contract is obsolete or incomplete; recapture evidence v2"
    if String.IsNullOrWhiteSpace manifest.captureToolVersion then
        invalidArg "evidenceDirectory" "Evidence capture tool version is missing; recapture evidence v2"
    let validSha256 (value:string) =
        not(isNull value) && value.Length=64 && (value |> Seq.forall Uri.IsHexDigit)
    if not(validSha256 manifest.packId && validSha256 manifest.mergedJdfSha256
           && validSha256 manifest.routingPbfSha256) then
        invalidArg "evidenceDirectory" "Evidence manifest contains an invalid SHA-256 identity"
    let expectedPackId=evidencePackId manifest.captureToolVersion manifest.mergedJdfSha256 manifest.routingPbfSha256
    if not(String.Equals(manifest.packId,expectedPackId,StringComparison.OrdinalIgnoreCase)) then
        invalidArg "evidenceDirectory" "Evidence pack ID does not match its inputs and routing ceilings"
    positive "capture_ceilings.routed_excess_metres" manifest.captureCeilings.routedExcessMetres
    if manifest.captureCeilings.routedExcessMetres<>CaptureRoutedExcessHorizonMetres
       || manifest.captureCeilings.maximumCorridorVariants<>CaptureMaximumCorridorVariants
       || manifest.maximumSearchStates<>CaptureMaximumSearchStates
       || manifest.maximumSearchDistanceMetres<>CaptureMaximumSearchDistanceMetres then
        invalidArg "evidenceDirectory" "Evidence routing ceilings do not match the supported router contract; recapture evidence v2"
    if manifest.contextCount<0L || manifest.routePointCount<0L || manifest.observationCount<0L
       || manifest.corridorVariantCount<0L || manifest.routePointEvidenceCount<0L then
        invalidArg "evidenceDirectory" "Evidence manifest relation counts must be non-negative"
    manifest

let validateEvidencePack evidenceDirectory =
    let manifest=loadEvidenceManifest evidenceDirectory
    let entries=manifest.files |> Array.map(fun value -> value.path,value) |> Map.ofArray
    if entries.Count<>manifest.files.Length then invalidArg "evidenceDirectory" "Evidence manifest contains duplicate file entries"
    let declaredRows = Map [
        "observations.parquet",int64 manifest.observationCount
        "route_points.parquet",int64 manifest.routePointCount
        "contexts.parquet",manifest.contextCount
        "corridor_variants.parquet",manifest.corridorVariantCount
        "route_point_evidence.parquet",manifest.routePointEvidenceCount
    ]
    for fileName in RequiredEvidenceFiles do
        let entry=entries |> Map.tryFind fileName |> Option.defaultWith(fun () ->
            invalidArg "evidenceDirectory" $"Evidence manifest is missing {fileName}")
        let path=Path.Combine(evidenceDirectory,fileName)
        if not(File.Exists path) then invalidArg "evidenceDirectory" $"Post-inference evidence pack is incomplete; missing {fileName}"
        let info=FileInfo(path)
        if info.Length<>entry.bytes then invalidArg "evidenceDirectory" $"Evidence file size mismatch: {fileName}"
        let actual=sha256File path
        if not(String.Equals(actual,entry.sha256,StringComparison.OrdinalIgnoreCase)) then
            invalidArg "evidenceDirectory" $"Evidence file hash mismatch: {fileName}"
        if entry.rows<0L || String.IsNullOrWhiteSpace(entry.schemaFingerprint) then
            invalidArg "evidenceDirectory" $"Evidence file metadata is invalid: {fileName}"
        if entry.rows<>declaredRows.[fileName] then
            invalidArg "evidenceDirectory" $"Evidence row-count metadata is inconsistent: {fileName}"
    if entries.Count<>RequiredEvidenceFiles.Length then
        invalidArg "evidenceDirectory" "Evidence manifest must contain exactly the v2 evidence relations"
    let actualFiles=
        Directory.EnumerateFiles(evidenceDirectory)
        |> Seq.map Path.GetFileName
        |> Set.ofSeq
    let expectedFiles=Set.add "manifest.json" (Set.ofArray RequiredEvidenceFiles)
    if actualFiles<>expectedFiles then
        invalidArg "evidenceDirectory" "Evidence directory contains missing or extra evidence relations"
    manifest

let geometryScore (policy:PostInferencePolicyV2) alignment side proximity routedFit =
    let weights=policy.geometryWeights
    weights.alignment*alignment + weights.side*side + weights.proximity*proximity + weights.routedExcess*routedFit
    |> fun value -> Math.Clamp(value,0.0,1.0)

let geometryComponents (policy:PostInferencePolicyV2) corridorDistance signedLateralOffset
                       corridorHeading attachmentHeading =
    let angleDifference left right =
        let difference=abs(left-right)%360.0
        min difference (360.0-difference)
    let alignment =
        match corridorHeading,attachmentHeading with
        | Some corridor,Some attachment ->
            max 0.0 (1.0-angleDifference corridor attachment/CaptureAlignmentRangeDegrees)
        | _ -> 0.0
    let side =
        match signedLateralOffset with
        | Some offset when abs offset<=policy.hardGates.centrelineNeutralToleranceMetres -> 0.5
        | Some offset when offset<0.0 -> 1.0
        | Some _ -> 0.0
        | None -> 0.0
    let proximity =
        corridorDistance
        |> Option.map(fun distance -> max 0.0 (1.0-distance/CaptureProximityRangeMetres))
        |> Option.defaultValue 0.0
    alignment,side,proximity

let evidenceGeometryScore policy corridorDistance signedLateralOffset
                          corridorHeading attachmentHeading routedFit =
    let alignment,side,proximity =
        geometryComponents policy corridorDistance signedLateralOffset
                           corridorHeading attachmentHeading
    geometryScore policy alignment side proximity routedFit

let supportingScore geometry supporting =
    if supporting>=0.0 then geometry+supporting*(1.0-geometry) else geometry+supporting*geometry
    |> fun value -> Math.Clamp(value,0.0,1.0)

let routedFit (policy:PostInferencePolicyV2) routedExcessMetres =
    max 0.0 (1.0-routedExcessMetres/policy.hardGates.maximumRoutedExcessMetres)

let hardGateReason (policy:PostInferencePolicyV2) topologyFailure corridorDistance signedLateralOffset routedExcessMetres =
    match topologyFailure with
    | Some reason -> Some reason
    | None when corridorDistance |> Option.exists(fun value -> value>policy.hardGates.maximumCorridorDistanceMetres) -> Some "corridor-distance"
    | None when routedExcessMetres |> Option.exists(fun value -> value>policy.hardGates.maximumRoutedExcessMetres) -> Some "routed-excess"
    | None when signedLateralOffset |> Option.exists(fun value -> value>policy.hardGates.centrelineNeutralToleranceMetres) -> Some "confident-left-side"
    | None -> None
