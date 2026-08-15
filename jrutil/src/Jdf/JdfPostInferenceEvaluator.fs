// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfPostInferenceEvaluator

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading

open Parquet
open Parquet.Schema

let mutable private evaluatorEntryCountValue=0L

/// Diagnostic counter used by isolation and malformed-pack tests. Validation
/// happens before callers can obtain a PostEvidenceStore, so rejected packs
/// must leave this counter unchanged.
let evaluatorEntryCount () = Interlocked.Read(&evaluatorEntryCountValue)
let resetEvaluatorEntryCount () = Interlocked.Exchange(&evaluatorEntryCountValue,0L) |> ignore

type private ReplayScore = {
    contextId:string;variantRank:int;relativeCostMetres:float option;relativeCostFraction:float option
    stopId:int64; candidateId:string; mode:string; lineId:string; routeDistinction:int; direction:int
    patternHash:string; position:int; role:string; movementFamilyId:string
    previousStopId:int64 option;nextStopId:int64 option
    assignmentKind:string;authoredPostKey:string option;sameStopBlockId:string option
    alignment:float; side:float; proximity:float; routedExcess:float option
    ingressThreadId:string option; egressThreadId:string option
    corridorId:string option;corridorFaceId:string option; routingAvailability:string
    alternativeCorridorCount:int; alternativeCostGap:float option; tiedCorridorsAgree:bool
    corridorDistance:float option; signedLateralOffset:float option
    corridorHeading:float option; attachmentHeading:float option
    snapEdgeId:int option;snapFraction:float option;topologyFailureReason:string option
}

type private ReplayContext = {
    stopId:int64;mode:string;lineId:string;routeDistinction:int;direction:int
    patternHash:string;position:int;role:string;movementFamilyId:string
    previousStopId:int64 option;nextStopId:int64 option
    assignmentKind:string;authoredPostKey:string option;sameStopBlockId:string option
}

type private ReplayCorridor = {
    relativeCostMetres:float option;relativeCostFraction:float option
    corridorId:string option
    ingressThreadId:string option;egressThreadId:string option
    routingAvailability:string;invariantFailureReason:string option
}

type private ReplayRoutePoint = {
    stopId:int64; routePointId:string; latitude:float; longitude:float
    observationIds:string array
    hasCurrentLifecycle:bool; hasObsoleteLifecycle:bool
    supportWeight:float
    explicitModes:string array;deniedModes:string array
}

type private ReplayDecision = {
    stopId:int64; movementFamilyId:string; mode:string; lineId:string;role:string
    resolution:string; candidateId:string option; representativeCandidateId:string option
    corridorFaceId:string option
    score:float option; margin:float option; unsafePhysical:bool
    latitude:float option;longitude:float option
}

type private ReplayJointCandidate = {
    candidateId:string;corridorFaceId:string option;score:float;geometry:float;margin:float
    latitude:float option;longitude:float option
}

type private ReplayPolicyCounters = {
    sameStopBlocks:int;distinctPairChoices:int;unresolvedBlockEdges:int
}

type private ReplayEvidenceStopSummary = {
    rowCount:int; usableRowCount:int; routePointCount:int; distinctFaces:int
}

type private ReplayExpectation = {
    stopId:int64;stopName:string;description:string;minimumDistinctFaces:int
    minimumEvidenceFaces:int;allowPhysicalUnderTiedCorridors:bool
    allowCentroidFallback:bool;mode:string option;lineId:string option
    role:string option;expectedResolution:string option;targetObservationIds:string array
    targetLatitude:float option;targetLongitude:float option;coordinateToleranceMetres:float
}

type private ReplayContextColumns = {
    ids:string array;stops:string array;modes:string array;lines:string array
    distinctions:int array;directions:int array;patterns:string array;positions:int array
    roles:string array;families:string array;previousStops:string array;nextStops:string array
    assignmentKinds:string array;authoredKeys:string array;blockIds:string array
}

type private ReplayVariantColumns = {
    ids:string array;ranks:int array;relativeCostMetres:Nullable<float> array
    relativeCostFractions:Nullable<float> array;corridorIds:string array option
    ingress:string array option;egress:string array option;availability:string array;failures:string array
}

type private ReplayAttachmentColumns = {
    ids:string array;ranks:int array;candidates:string array
    routedExcess:Nullable<float> array;snapEdges:Nullable<int> array option
    snapFractions:Nullable<float> array option;faces:string array
    corridorDistances:Nullable<float> array;lateralOffsets:Nullable<float> array
    corridorHeadings:Nullable<float> array;attachmentHeadings:Nullable<float> array
    availability:string array;failures:string array
}

/// Read the three routing relations as one ordered join.  Publication does not
/// need diagnostic-only identifiers, so it can avoid decoding them entirely.
let private replayScoreRows includeDiagnostics evidencePath (selectedStops:Set<int64> option) =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "evidence-joined-traversal"
    let stopRegex=Regex("(?:jdf|cis):stop:(?<id>[0-9]+)",RegexOptions.CultureInvariant)
    let parseStop value =
        let matched=stopRegex.Match(value)
        if not matched.Success then invalidArg "evidencePath" $"Invalid evidence stop identity: {value}"
        Int64.Parse(matched.Groups.["id"].Value,CultureInfo.InvariantCulture)
    let strings count (group:ParquetRowGroupReader) (fields:Map<string,DataField>) name =
        let values=Array.zeroCreate<string> count
        group.ReadAsync(fields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let ints count (group:ParquetRowGroupReader) (fields:Map<string,DataField>) name =
        let values=Array.zeroCreate<int> count
        group.ReadAsync<int>(fields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let doubles count (group:ParquetRowGroupReader) (fields:Map<string,DataField>) name =
        let values=Array.zeroCreate<Nullable<float>> count
        group.ReadAsync<float>(fields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let nullableInts count (group:ParquetRowGroupReader) (fields:Map<string,DataField>) name =
        let values=Array.zeroCreate<Nullable<int>> count
        group.ReadAsync<int>(fields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let contextGroups = seq {
        use stream=File.OpenRead(Path.Combine(evidencePath,"contexts.parquet"))
        let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let fields=reader.Schema.DataFields |> Array.map(fun field -> field.Name,field) |> Map.ofArray
            for groupIndex=0 to reader.RowGroupCount-1 do
                use group=reader.OpenRowGroupReader(groupIndex)
                let count=int group.RowCount
                yield { ids=strings count group fields "context_id"
                        stops=strings count group fields "gtfs_stop_place_id"
                        modes=strings count group fields "mode";lines=strings count group fields "line_id"
                        distinctions=ints count group fields "route_distinction"
                        directions=ints count group fields "direction"
                        patterns=strings count group fields "pattern_hash"
                        positions=ints count group fields "pattern_position"
                        roles=strings count group fields "same_stop_block_role"
                        families=strings count group fields "movement_family_id"
                        previousStops=strings count group fields "context_previous_stop_id"
                        nextStops=strings count group fields "context_next_stop_id"
                        assignmentKinds=strings count group fields "assignment_kind"
                        authoredKeys=strings count group fields "authored_post_key"
                        blockIds=strings count group fields "same_stop_block_id" }
        finally reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
    }
    let variantGroups = seq {
        use stream=File.OpenRead(Path.Combine(evidencePath,"corridor_variants.parquet"))
        let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let fields=reader.Schema.DataFields |> Array.map(fun field -> field.Name,field) |> Map.ofArray
            for groupIndex=0 to reader.RowGroupCount-1 do
                use group=reader.OpenRowGroupReader(groupIndex)
                let count=int group.RowCount
                yield { ids=strings count group fields "context_id";ranks=ints count group fields "variant_rank"
                        relativeCostMetres=doubles count group fields "relative_cost_metres"
                        relativeCostFractions=doubles count group fields "relative_cost_fraction"
                        corridorIds=if includeDiagnostics then Some(strings count group fields "corridor_id") else None
                        ingress=if includeDiagnostics then Some(strings count group fields "ingress_thread_id") else None
                        egress=if includeDiagnostics then Some(strings count group fields "egress_thread_id") else None
                        availability=strings count group fields "routing_availability"
                        failures=strings count group fields "invariant_failure_reason" }
        finally reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
    }
    let attachmentGroups = seq {
        use stream=File.OpenRead(Path.Combine(evidencePath,"route_point_evidence.parquet"))
        let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let fields=reader.Schema.DataFields |> Array.map(fun field -> field.Name,field) |> Map.ofArray
            for groupIndex=0 to reader.RowGroupCount-1 do
                use group=reader.OpenRowGroupReader(groupIndex)
                let count=int group.RowCount
                yield { ids=strings count group fields "context_id";ranks=ints count group fields "variant_rank"
                        candidates=strings count group fields "route_point_id"
                        routedExcess=doubles count group fields "routed_excess_metres"
                        snapEdges=if includeDiagnostics then Some(nullableInts count group fields "snap_edge_id") else None
                        snapFractions=if includeDiagnostics then Some(doubles count group fields "snap_fraction") else None
                        faces=strings count group fields "corridor_face_id"
                        corridorDistances=doubles count group fields "corridor_distance_metres"
                        lateralOffsets=doubles count group fields "signed_lateral_offset_metres"
                        corridorHeadings=doubles count group fields "corridor_heading_degrees"
                        attachmentHeadings=doubles count group fields "attachment_heading_degrees"
                        availability=strings count group fields "routing_availability"
                        failures=strings count group fields "invariant_failure_reason" }
        finally reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
    }
    seq {
        use contextEnumerator=contextGroups.GetEnumerator()
        use variantEnumerator=variantGroups.GetEnumerator()
        use attachmentEnumerator=attachmentGroups.GetEnumerator()
        let mutable contextColumns:ReplayContextColumns option=None
        let mutable variantColumns:ReplayVariantColumns option=None
        let mutable attachmentColumns:ReplayAttachmentColumns option=None
        let mutable contextIndex=0
        let mutable variantIndex=0
        let mutable attachmentIndex=0
        let rec ensureContext () =
            match contextColumns with
            | Some value when contextIndex<value.ids.Length -> true
            | _ when contextEnumerator.MoveNext() ->
                contextColumns<-Some contextEnumerator.Current;contextIndex<-0;ensureContext()
            | _ -> false
        let rec ensureVariant () =
            match variantColumns with
            | Some value when variantIndex<value.ids.Length -> true
            | _ when variantEnumerator.MoveNext() ->
                variantColumns<-Some variantEnumerator.Current;variantIndex<-0;ensureVariant()
            | _ -> false
        let rec ensureAttachment () =
            match attachmentColumns with
            | Some value when attachmentIndex<value.ids.Length -> true
            | _ when attachmentEnumerator.MoveNext() ->
                attachmentColumns<-Some attachmentEnumerator.Current;attachmentIndex<-0;ensureAttachment()
            | _ -> false
        let optionalString (values:string array) index=Option.ofObj values.[index]
        let optionalDouble (values:Nullable<float> array) index =
            if values.[index].HasValue then Some values.[index].Value else None
        while ensureContext() do
            let contexts=contextColumns.Value
            let contextRow=contextIndex
            contextIndex<-contextIndex+1
            let contextId=contexts.ids.[contextRow]
            let stopId=parseStop contexts.stops.[contextRow]
            let selected=selectedStops |> Option.forall(fun values -> values.Contains stopId)
            let context = {
                stopId=stopId;mode=contexts.modes.[contextRow];lineId=contexts.lines.[contextRow]
                routeDistinction=contexts.distinctions.[contextRow];direction=contexts.directions.[contextRow]
                patternHash=contexts.patterns.[contextRow];position=contexts.positions.[contextRow]
                role=contexts.roles.[contextRow];movementFamilyId=contexts.families.[contextRow]
                previousStopId=optionalString contexts.previousStops contextRow |> Option.map parseStop
                nextStopId=optionalString contexts.nextStops contextRow |> Option.map parseStop
                assignmentKind=contexts.assignmentKinds.[contextRow]
                authoredPostKey=optionalString contexts.authoredKeys contextRow
                sameStopBlockId=optionalString contexts.blockIds contextRow }
            if not(ensureVariant()) || variantColumns.Value.ids.[variantIndex]<>contextId then
                invalidArg "evidencePath" "Evidence context is missing corridor variants"
            while ensureVariant() && variantColumns.Value.ids.[variantIndex]=contextId do
                let variants=variantColumns.Value
                let variantRow=variantIndex
                let rank=variants.ranks.[variantRow]
                let corridor = {
                    relativeCostMetres=optionalDouble variants.relativeCostMetres variantRow
                    relativeCostFraction=optionalDouble variants.relativeCostFractions variantRow
                    corridorId=variants.corridorIds |> Option.bind(fun values -> optionalString values variantRow)
                    ingressThreadId=variants.ingress |> Option.bind(fun values -> optionalString values variantRow)
                    egressThreadId=variants.egress |> Option.bind(fun values -> optionalString values variantRow)
                    routingAvailability=variants.availability.[variantRow]
                    invariantFailureReason=optionalString variants.failures variantRow }
                if not(ensureAttachment())
                   || attachmentColumns.Value.ids.[attachmentIndex]<>contextId
                   || attachmentColumns.Value.ranks.[attachmentIndex]<>rank then
                    invalidArg "evidencePath" "Evidence corridor variant is missing route-point evidence"
                while ensureAttachment()
                      && attachmentColumns.Value.ids.[attachmentIndex]=contextId
                      && attachmentColumns.Value.ranks.[attachmentIndex]=rank do
                    let attachments=attachmentColumns.Value
                    let attachmentRow=attachmentIndex
                    attachmentIndex<-attachmentIndex+1
                    if selected then
                        let failure=optionalString attachments.failures attachmentRow
                                    |> Option.orElse corridor.invariantFailureReason
                        yield {
                            contextId=contextId;variantRank=rank
                            relativeCostMetres=corridor.relativeCostMetres
                            relativeCostFraction=corridor.relativeCostFraction
                            stopId=context.stopId;candidateId=attachments.candidates.[attachmentRow]
                            mode=context.mode;lineId=context.lineId
                            routeDistinction=context.routeDistinction;direction=context.direction
                            patternHash=context.patternHash;position=context.position;role=context.role
                            movementFamilyId=context.movementFamilyId
                            previousStopId=context.previousStopId;nextStopId=context.nextStopId
                            assignmentKind=context.assignmentKind;authoredPostKey=context.authoredPostKey
                            sameStopBlockId=context.sameStopBlockId
                            alignment=0.0;side=0.0;proximity=0.0
                            routedExcess=optionalDouble attachments.routedExcess attachmentRow
                            ingressThreadId=corridor.ingressThreadId;egressThreadId=corridor.egressThreadId
                            corridorId=corridor.corridorId
                            corridorFaceId=optionalString attachments.faces attachmentRow
                            routingAvailability=if failure.IsSome then "unavailable" else attachments.availability.[attachmentRow]
                            alternativeCorridorCount=0;alternativeCostGap=None;tiedCorridorsAgree=true
                            corridorDistance=optionalDouble attachments.corridorDistances attachmentRow
                            signedLateralOffset=optionalDouble attachments.lateralOffsets attachmentRow
                            corridorHeading=optionalDouble attachments.corridorHeadings attachmentRow
                            attachmentHeading=optionalDouble attachments.attachmentHeadings attachmentRow
                            snapEdgeId=attachments.snapEdges |> Option.bind(fun values ->
                                if values.[attachmentRow].HasValue then Some values.[attachmentRow].Value else None)
                            snapFraction=attachments.snapFractions |> Option.bind(fun values -> optionalDouble values attachmentRow)
                            topologyFailureReason=failure }
                variantIndex<-variantIndex+1
        if ensureVariant() then
            invalidArg "evidencePath" "Corridor evidence references an unknown context"
        if ensureAttachment() then
            invalidArg "evidencePath" "Route-point evidence references an unknown context or corridor variant"
    }

let private readReplayRoutePoints evidencePath (selectedStops:Set<int64> option) =
    let path=Path.Combine(evidencePath,"route_points.parquet")
    begin
        use stream=File.OpenRead(path)
        let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let fields=reader.Schema.DataFields |> Array.map(fun field -> field.Name,field) |> Map.ofArray
            let field name = fields |> Map.tryFind name |> Option.defaultWith(fun () ->
                invalidArg "evidencePath" $"Route points are missing required field {name}")
            let stopField=field "gtfs_stop_place_id"
            let idField=field "route_point_id"
            let observationsField=field "observation_ids"
            let latitudeField=field "latitude"
            let longitudeField=field "longitude"
            let currentField=field "has_current_lifecycle"
            let obsoleteField=field "has_obsolete_lifecycle"
            let supportField=field "source_support_weight"
            let explicitModesField=field "explicit_modes"
            let deniedModesField=field "denied_modes"
            let stopRegex=Regex("(?:jdf|cis):stop:(?<id>[0-9]+)",RegexOptions.CultureInvariant)
            let values=ResizeArray<ReplayRoutePoint>()
            for groupIndex=0 to reader.RowGroupCount-1 do
                use group=reader.OpenRowGroupReader(groupIndex)
                let mightContainSelectedStop =
                    selectedStops |> Option.forall(fun selected ->
                        let statistics=group.GetStatistics(stopField)
                        match Option.ofObj statistics.MinValue,Option.ofObj statistics.MaxValue with
                        | Some minimum,Some maximum ->
                            let minimum=string minimum
                            let maximum=string maximum
                            selected |> Seq.exists(fun stopId ->
                                [|$"jdf:stop:{stopId}";$"cis:stop:{stopId}"|]
                                |> Array.exists(fun value ->
                                    StringComparer.Ordinal.Compare(value,minimum)>=0
                                    && StringComparer.Ordinal.Compare(value,maximum)<=0))
                        | _ -> true)
                let count=if mightContainSelectedStop then int group.RowCount else 0
                let readStrings field =
                    let result=Array.zeroCreate<string> count
                    if count>0 then
                        group.ReadAsync(field,result.AsMemory(),Nullable(),CancellationToken.None).AsTask().GetAwaiter().GetResult()
                    result
                let readDoubles field =
                    let result=Array.zeroCreate<float> count
                    if count>0 then
                        group.ReadAsync<float>(field,result.AsMemory(),Nullable(),CancellationToken.None).AsTask().GetAwaiter().GetResult()
                    result
                let readBools field =
                    let result=Array.zeroCreate<bool> count
                    if count>0 then
                        group.ReadAsync<bool>(field,result.AsMemory(),Nullable(),CancellationToken.None).AsTask().GetAwaiter().GetResult()
                    result
                let stops=readStrings stopField
                let ids=readStrings idField
                let observationIds=readStrings observationsField
                let latitudes=readDoubles latitudeField
                let longitudes=readDoubles longitudeField
                let current=readBools currentField
                let obsolete=readBools obsoleteField
                let support=readDoubles supportField
                let explicitModes=readStrings explicitModesField
                let deniedModes=readStrings deniedModesField
                for index=0 to count-1 do
                    let matched=stopRegex.Match(stops.[index])
                    if matched.Success then
                        let stopId=Int64.Parse(matched.Groups.["id"].Value,CultureInfo.InvariantCulture)
                        if selectedStops |> Option.forall(fun selected -> selected.Contains(stopId)) then
                            values.Add({ stopId=stopId;routePointId=ids.[index]
                                         latitude=latitudes.[index];longitude=longitudes.[index]
                                         observationIds=observationIds.[index].Split(';',StringSplitOptions.RemoveEmptyEntries)
                                         hasCurrentLifecycle=current.[index]
                                         hasObsoleteLifecycle=obsolete.[index]
                                         supportWeight=support.[index]
                                         explicitModes=explicitModes.[index].Split(';',StringSplitOptions.RemoveEmptyEntries)
                                         deniedModes=deniedModes.[index].Split(';',StringSplitOptions.RemoveEmptyEntries) })
            values |> Seq.groupBy(fun value -> value.stopId)
                   |> Seq.map(fun (stopId,points) -> stopId,points |> Seq.sortBy(fun value -> value.routePointId) |> Seq.toArray)
                   |> Map.ofSeq
        finally
            reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
    end

let private medianFloat values =
    let ordered=values |> Seq.sort |> Seq.toArray
    if ordered.Length=0 then 0.0
    elif ordered.Length%2=1 then ordered.[ordered.Length/2]
    else (ordered.[ordered.Length/2-1]+ordered.[ordered.Length/2])/2.0

let private validateReplayEvidenceSemantics evidencePath
                                                    (manifest:JdfPostInference.PostInferenceEvidenceManifest)
                                                    (routePointsByStop:Map<int64,ReplayRoutePoint array>) =
    let mutable rowCount=0L
    let mutable contextCount=0L
    let mutable variantCount=0L
    let mutable currentStop=None
    let rows=ResizeArray<ReplayScore>()
    let flush () =
        if rows.Count>0 then
            let values=rows.ToArray()
            let stopId=values.[0].stopId
            let expectedPoints =
                routePointsByStop |> Map.tryFind stopId |> Option.defaultValue [||]
                |> Array.map _.routePointId |> Set.ofArray
            for _,contextRows in values |> Array.groupBy _.contextId do
                contextCount<-contextCount+1L
                let ranks=contextRows |> Array.map _.variantRank |> Array.distinct |> Array.sort
                if ranks.Length<1 || ranks.Length>JdfPostInference.CaptureMaximumCorridorVariants
                   || ranks<>[|0..ranks.Length-1|] then
                    invalidArg "evidencePath" "Evidence corridor ranks must be contiguous from zero and respect the capture ceiling"
                variantCount<-variantCount+int64 ranks.Length
                for rank in ranks do
                    let rankRows=contextRows |> Array.filter(fun row -> row.variantRank=rank)
                    let actualPoints=rankRows |> Array.map _.candidateId |> Set.ofArray
                    if rankRows.Length<>actualPoints.Count || actualPoints<>expectedPoints then
                        invalidArg "evidencePath" "Evidence must contain exactly one attachment per context, variant, and route point"
                    for row in rankRows do
                        let finite value=value |> Option.forall Double.IsFinite
                        if not(finite row.relativeCostMetres && finite row.relativeCostFraction
                               && finite row.routedExcess && finite row.corridorDistance
                               && finite row.signedLateralOffset && finite row.corridorHeading
                               && finite row.attachmentHeading) then
                            invalidArg "evidencePath" "Evidence contains a non-finite routing fact"
            rows.Clear()
    for row in replayScoreRows true evidencePath None do
        match currentStop with
        | Some stopId when stopId<>row.stopId -> flush()
        | _ -> ()
        currentStop<-Some row.stopId
        rows.Add(row)
        rowCount<-rowCount+1L
    flush()
    if rowCount<>manifest.routePointEvidenceCount
       || contextCount<>manifest.contextCount
       || variantCount<>manifest.corridorVariantCount then
        invalidArg "evidencePath" "Evidence semantic counts do not match the manifest"

let private consolidateReplayRows (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                                  (points:ReplayRoutePoint array)
                                  (rows:ReplayScore array) =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "consolidation"
    if points.Length=0 then rows,Map.empty else
    let contextIdentity (row:ReplayScore) =
        row.contextId,row.variantRank
    let rowsByPoint =
        rows |> Array.groupBy(fun value -> value.candidateId)
             |> Array.map(fun (candidate,values) ->
                 candidate,values |> Array.map(fun value -> contextIdentity value,value) |> Map.ofArray)
             |> Map.ofArray
    let metresBetween (left:ReplayRoutePoint) (right:ReplayRoutePoint) =
        let latitude=(left.latitude+right.latitude)*0.5*Math.PI/180.0
        let dx=(left.longitude-right.longitude)*111320.0*Math.Cos(latitude)
        let dy=(left.latitude-right.latitude)*110540.0
        Math.Sqrt(dx*dx+dy*dy),dx,dy
    let headingDifference left right =
        let difference=abs(left-right)%360.0
        min difference (360.0-difference)
    let compatiblePoints (left:ReplayRoutePoint) (right:ReplayRoutePoint) =
        let distance,dx,dy=metresBetween left right
        if not(JdfPostInference.withinConsolidationDiameter policy distance) then false
        elif (left.hasCurrentLifecycle<>right.hasCurrentLifecycle
              || left.hasObsoleteLifecycle<>right.hasObsoleteLifecycle)
             && (left.hasObsoleteLifecycle || right.hasObsoleteLifecycle) then false
        else
            let leftRows=rowsByPoint |> Map.tryFind left.routePointId |> Option.defaultValue Map.empty
            let rightRows=rowsByPoint |> Map.tryFind right.routePointId |> Option.defaultValue Map.empty
            let mutable incompatible=false
            let mutable distinguished=0
            for KeyValue(context,leftRow) in leftRows do
                match rightRows |> Map.tryFind context with
                | None -> ()
                | Some rightRow ->
                    match leftRow.corridorFaceId,rightRow.corridorFaceId with
                    | Some leftFace,Some rightFace when leftFace<>rightFace -> incompatible<-true
                    | _ -> ()
                    if not(JdfPostInference.consolidationSidesCompatible policy
                               leftRow.signedLateralOffset rightRow.signedLateralOffset) then
                        incompatible<-true
                    match leftRow.corridorHeading,rightRow.corridorHeading with
                    | Some leftHeading,Some rightHeading ->
                        if not(JdfPostInference.consolidationHeadingCompatible policy
                                   (headingDifference leftHeading rightHeading)) then incompatible<-true
                        let radians=leftHeading*Math.PI/180.0
                        let chainage=abs(dx*Math.Sin(radians)+dy*Math.Cos(radians))
                        if not(JdfPostInference.consolidationChainageCompatible policy chainage) then
                            incompatible<-true
                    | _ -> ()
                    let score (row:ReplayScore) =
                        let routed=row.routedExcess |> Option.map(JdfPostInference.routedFit policy) |> Option.defaultValue 0.0
                        JdfPostInference.evidenceGeometryScore policy row.corridorDistance
                            row.signedLateralOffset row.corridorHeading row.attachmentHeading routed
                    let leftScore=score leftRow
                    let rightScore=score rightRow
                    if JdfPostInference.consolidationContextDistinguishes policy leftScore rightScore then
                        distinguished<-distinguished+1
            not incompatible
            && JdfPostInference.consolidationDistinguishedCountCompatible policy distinguished
    let clusters=ResizeArray<ReplayRoutePoint array>()
    for point in points |> Array.sortBy(fun value -> value.routePointId) do clusters.Add([|point|])
    let mutable changed=true
    while changed do
        changed<-false
        let selected =
            seq {
                for leftIndex=0 to clusters.Count-1 do
                    for rightIndex=leftIndex+1 to clusters.Count-1 do
                        let left=clusters.[leftIndex]
                        let right=clusters.[rightIndex]
                        if left |> Array.forall(fun leftPoint ->
                            right |> Array.forall(compatiblePoints leftPoint)) then
                            let diameter =
                                seq {
                                    for leftPoint in left do
                                        for rightPoint in right do
                                            let distance,_,_=metresBetween leftPoint rightPoint
                                            yield distance
                                }
                                |> Seq.max
                            let identity =
                                Array.append left right |> Array.map _.routePointId |> Array.sort
                            yield diameter,String.Join("|",identity),leftIndex,rightIndex
            }
            |> Seq.sortBy(fun (diameter,identity,_,_) -> diameter,identity)
            |> Seq.tryHead
        match selected with
        | Some(_,_,leftIndex,rightIndex) ->
            let merged=Array.append clusters.[leftIndex] clusters.[rightIndex]
                       |> Array.sortBy(fun value -> value.routePointId)
            clusters.[leftIndex]<-merged
            clusters.RemoveAt(rightIndex)
            changed<-true
        | None -> ()
    let pointToHypothesis =
        clusters
        |> Seq.collect(fun cluster ->
            let identities=cluster |> Array.collect(fun point -> point.observationIds)
                                  |> Array.distinct |> Array.sort
            let identities=if identities.Length=0 then cluster |> Array.map(fun point -> point.routePointId) else identities
            let joined=String.Join("|",identities)
            let payload = $"{cluster.[0].stopId}|{joined}"
            let hypothesis=SHA256.HashData(Encoding.UTF8.GetBytes(payload)) |> Convert.ToHexString
                           |> fun value -> value.ToLowerInvariant()
            cluster |> Seq.map(fun point -> point.routePointId,hypothesis))
        |> Map.ofSeq
    let hypothesisDetails =
        clusters
        |> Seq.map(fun cluster ->
            let hypothesis=pointToHypothesis.[cluster.[0].routePointId]
            let medoid=cluster |> Array.minBy(fun candidate ->
                cluster |> Array.sumBy(fun other -> let distance,_,_=metresBetween candidate other in distance),
                candidate.routePointId)
            hypothesis,(medoid,
                        cluster |> Array.exists _.hasCurrentLifecycle,
                        cluster |> Array.exists _.hasObsoleteLifecycle,
                        cluster |> Array.sumBy _.supportWeight,
                        cluster |> Array.collect _.explicitModes |> Array.distinct,
                        cluster |> Array.collect _.deniedModes |> Array.distinct,
                        cluster))
        |> Map.ofSeq
    let aggregated =
      rows
      |> Array.groupBy(fun row ->
        contextIdentity row,row.movementFamilyId,
        (pointToHypothesis |> Map.tryFind row.candidateId |> Option.defaultValue row.candidateId))
      |> Array.map(fun ((_,_,hypothesis),values) ->
        let first=values.[0]
        let medianOption selector=values |> Seq.choose selector |> medianFloat |> Some
        let uniqueOption selector=values |> Seq.choose selector |> Seq.distinct |> Seq.toArray
                                 |> function [|value|] -> Some value | _ -> None
        { first with candidateId=hypothesis
                     alignment=values |> Seq.map(fun value -> value.alignment) |> medianFloat
                     side=values |> Seq.map(fun value -> value.side) |> medianFloat
                     proximity=values |> Seq.map(fun value -> value.proximity) |> medianFloat
                     routedExcess=if values |> Array.exists(fun value -> value.routedExcess.IsSome)
                                  then medianOption(fun value -> value.routedExcess) else None
                     corridorFaceId=uniqueOption(fun value -> value.corridorFaceId)
                     corridorDistance=if values |> Array.exists(fun value -> value.corridorDistance.IsSome)
                                      then medianOption(fun value -> value.corridorDistance) else None
                     signedLateralOffset=if values |> Array.exists(fun value -> value.signedLateralOffset.IsSome)
                                         then medianOption(fun value -> value.signedLateralOffset) else None
                     corridorHeading=uniqueOption(fun value -> value.corridorHeading)
                     attachmentHeading=uniqueOption(fun value -> value.attachmentHeading)
                     alternativeCorridorCount=values |> Array.maxBy(fun value -> value.alternativeCorridorCount)
                                                     |> fun value -> value.alternativeCorridorCount
                     alternativeCostGap=values |> Seq.choose _.alternativeCostGap |> Seq.sort |> Seq.tryHead
                     tiedCorridorsAgree=values |> Array.forall(fun value -> value.tiedCorridorsAgree)
                     topologyFailureReason=
                        if values |> Array.exists(fun value -> value.topologyFailureReason.IsNone) then None
                        else values |> Seq.choose(fun value -> value.topologyFailureReason) |> Seq.sort |> Seq.tryHead
                     })
    aggregated,hypothesisDetails

let private replaySupportAdjustments (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                                     mode supportWeight explicitModes deniedModes =
    JdfPostInference.modalitySupportAdjustments policy mode supportWeight explicitModes deniedModes

let private evaluateReplayPolicy capturedHorizon
                                (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                                (points:ReplayRoutePoint array)
                                (rows:ReplayScore array) =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "scoring"
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "resolution"
    let policy=JdfPostInferencePolicy.validatePolicy capturedHorizon policy
    let rows,hypothesisDetails=consolidateReplayRows policy points rows
    let rows =
        rows
        |> Array.groupBy _.contextId
        |> Array.collect(fun (_,contextRows) ->
            let selectedRanks =
                contextRows
                |> Array.map(fun row ->
                    ({ variantRank=row.variantRank
                       relativeCostMetres=row.relativeCostMetres
                       relativeCostFraction=row.relativeCostFraction }
                     : JdfPostInference.AlternativeCorridorFact))
                |> JdfPostInference.selectedCorridorVariantRanks policy
                |> Set.ofArray
            contextRows
            |> Array.groupBy _.candidateId
            |> Array.choose(fun (_,candidateRows) ->
                candidateRows |> Array.tryFind(fun row -> row.variantRank=0)
                |> Option.map(fun baseline ->
                    let selected = candidateRows |> Array.filter(fun row -> selectedRanks.Contains row.variantRank)
                    let complete=selected.Length=selectedRanks.Count
                    let valid=complete && selected |> Array.forall(fun row ->
                        row.topologyFailureReason.IsNone && row.routingAvailability="available"
                        && row.corridorFaceId.IsSome)
                    let faces=selected |> Array.choose _.corridorFaceId |> Array.distinct
                    { baseline with
                        alternativeCorridorCount=selectedRanks.Count
                        alternativeCostGap=selected |> Seq.choose _.relativeCostMetres |> Seq.filter(fun value -> value>0.0) |> Seq.sort |> Seq.tryHead
                        tiedCorridorsAgree=valid && faces.Length=1
                        topologyFailureReason=
                            if baseline.topologyFailureReason.IsSome then baseline.topologyFailureReason
                            elif valid && faces.Length=1 then None
                            else Some "alternative-corridor-invariant" })))
    let coordinates candidateId =
        hypothesisDetails |> Map.tryFind candidateId
        |> Option.map(fun (value,_,_,_,_,_,_) -> Some value.latitude,Some value.longitude)
        |> Option.defaultValue(None,None)
    let isolationAdjustment candidateId =
        match hypothesisDetails |> Map.tryFind candidateId with
        | None -> 0.0
        | Some(point,_,_,_,_,_,_) ->
            let nearest =
                hypothesisDetails
                |> Seq.choose(fun pair ->
                    if pair.Key=candidateId then None else
                    let other,_,_,_,_,_,_=pair.Value
                    let dlat=(point.latitude-other.latitude)*111_320.0
                    let dlon=(point.longitude-other.longitude)*111_320.0
                             *Math.Cos((point.latitude+other.latitude)*Math.PI/360.0)
                    let distance=Math.Sqrt(dlat*dlat+dlon*dlon)
                    Some distance)
                |> Seq.sort |> Seq.tryHead
            JdfPostInference.spatialIsolationAdjustment policy nearest
    let alternativeApplies (row:ReplayScore) =
        row.alternativeCorridorCount>1
    let evaluated =
        rows
        |> Array.map(fun row ->
            let lifecycleFailure =
                hypothesisDetails |> Map.tryFind row.candidateId
                |> Option.bind(fun (_,hasCurrent,hasObsolete,_,_,_,_) ->
                    if hasObsolete && not hasCurrent then Some "obsolete-lifecycle" else None)
            let rejection=JdfPostInference.hardGateReason policy (lifecycleFailure |> Option.orElse row.topologyFailureReason)
                              row.corridorDistance row.signedLateralOffset row.routedExcess
            let routedFit=row.routedExcess |> Option.map(JdfPostInference.routedFit policy) |> Option.defaultValue 0.0
            let geometry=JdfPostInference.evidenceGeometryScore policy row.corridorDistance
                             row.signedLateralOffset row.corridorHeading row.attachmentHeading routedFit
            let sourceAdjustment,modalityAdjustment =
                match hypothesisDetails |> Map.tryFind row.candidateId with
                | Some(_,_,_,supportWeight,explicitModes,deniedModes,_) ->
                    replaySupportAdjustments policy row.mode supportWeight explicitModes deniedModes
                | _ -> 0.0,0.0
            let support=JdfPostInference.combinedSupportAdjustment policy sourceAdjustment modalityAdjustment
            row,rejection,geometry,JdfPostInference.supportingScore geometry support,routedFit)
    let contextKey (row:ReplayScore) =
        row.stopId,row.mode,row.lineId,row.routeDistinction,row.direction,row.patternHash,row.position,row.role,row.movementFamilyId
    let contextWinners =
        evaluated
        |> Array.groupBy(fun (row,_,_,_,_) -> contextKey row)
        |> Array.choose(fun (key,values) ->
            let ranked=values |> Array.filter(fun (_,rejection,_,_,_) -> rejection.IsNone)
                              |> Array.sortBy(fun (row,_,geometry,final,_) -> -final,-geometry,row.candidateId)
            if ranked.Length=0 then None else
            let row,_,geometry,final,routed=ranked.[0]
            let runner=ranked |> Array.tryItem 1
            let lead=runner |> Option.map(fun (_,_,_,value,_) -> final-value) |> Option.defaultValue final
            Some(key,(row.candidateId,geometry,final,routed,lead,row.corridorFaceId,
                      (if alternativeApplies row then row.alternativeCorridorCount else 1),
                      (not(alternativeApplies row) || row.tiedCorridorsAgree))))
        |> Map.ofSeq
    let jointCandidates=Dictionary<struct(int64*string),ReplayJointCandidate array>()
    let ordinaryDecisions =
        evaluated
        |> Array.groupBy(fun (row,_,_,_,_) -> row.stopId,row.movementFamilyId)
        |> Array.map(fun ((stopId,familyId),familyRows) ->
        let first,_,_,_,_=familyRows.[0]
        let contexts=familyRows |> Array.map(fun (row,_,_,_,_) -> contextKey row) |> Array.distinct
        let winners=contexts |> Array.choose(fun key -> contextWinners |> Map.tryFind key)
        let winnerCounts=winners |> Array.countBy(fun (candidate,_,_,_,_,_,_,_) -> candidate)
                                |> Array.sortBy(fun (candidate,count) -> -count,candidate)
        let externalSupport =
            contextWinners
            |> Seq.filter(fun pair -> let _,_,_,_,_,_,_,_,movementFamilyId=pair.Key in movementFamilyId<>familyId)
            |> Seq.groupBy(fun pair -> let candidate,_,_,_,_,_,_,_=pair.Value in candidate)
            |> Seq.map(fun (candidate,values) -> candidate,min 5 (Seq.length values))
            |> Map.ofSeq
        let candidateScores =
            familyRows
            |> Array.filter(fun (_,rejection,_,_,_) -> rejection.IsNone)
            |> Array.groupBy(fun (row,_,_,_,_) -> row.candidateId)
            |> Array.map(fun (candidate,values) ->
                let geometry=values |> Seq.map(fun (_,_,value,_,_) -> value) |> medianFloat
                let final=values |> Seq.map(fun (_,_,_,value,_) -> value) |> medianFloat
                let establishedAdjustment =
                    externalSupport |> Map.tryFind candidate |> Option.defaultValue 0
                    |> JdfPostInference.establishedPopularityAdjustment policy
                let final=JdfPostInference.supportingScore final establishedAdjustment
                let final=JdfPostInference.supportingScore final (isolationAdjustment candidate)
                let routed=values |> Seq.map(fun (_,_,_,_,value) -> value) |> medianFloat
                let faces=values |> Seq.choose(fun (row,_,_,_,_) -> row.corridorFaceId) |> Set
                let alternatives=values |> Seq.exists(fun (row,_,_,_,_) -> alternativeApplies row)
                let agree=values |> Seq.forall(fun (row,_,_,_,_) -> not(alternativeApplies row) || row.tiedCorridorsAgree)
                candidate,geometry,final,routed,faces,alternatives,agree)
            |> Array.sortBy(fun (candidate,geometry,final,_,_,_,_) -> -final,-geometry,candidate)
        if candidateScores.Length=0 then
            { stopId=stopId;movementFamilyId=familyId;mode=first.mode;lineId=first.lineId;role=first.role
              resolution="Centroid";candidateId=None;corridorFaceId=None;score=None;margin=None;unsafePhysical=false
              representativeCandidateId=None
              latitude=None;longitude=None }
        else
            let candidate,geometry,final,routed,faces,alternatives,alternativesAgree=candidateScores.[0]
            let runner=candidateScores |> Array.tryItem 1
            let margin=runner |> Option.map(fun (_,_,value,_,_,_,_) -> final-value) |> Option.defaultValue final
            let geometryMargin=runner |> Option.map(fun (_,value,_,_,_,_,_) -> geometry-value) |> Option.defaultValue geometry
            let routedAdvantage=runner |> Option.map(fun (_,_,_,value,_,_,_) -> routed-value) |> Option.defaultValue routed
            let winnerSupport=winnerCounts |> Array.tryFind(fun (value,_) -> value=candidate) |> Option.map snd |> Option.defaultValue 0
            let winnerShare=if contexts.Length=0 then 0.0 else float winnerSupport/float contexts.Length
            let contradictory =
                winners |> Array.exists(fun (other,_,score,_,lead,_,_,_) ->
                    other<>candidate && JdfPostInference.contradictoryContext policy score lead)
            let established=externalSupport |> Map.tryFind candidate |> Option.defaultValue 0
            let establishedRunner =
                externalSupport |> Seq.filter(fun pair -> pair.Key<>candidate)
                |> Seq.map(fun pair -> pair.Value) |> Seq.sortDescending |> Seq.tryHead |> Option.defaultValue 0
            let establishedStrong =
                JdfPostInference.establishedPostStrong policy established establishedRunner
                    geometryMargin contradictory alternatives alternativesAgree
            // Same-stop preference is a joint tie-break between candidates
            // that have already passed these ordinary gates; it must never
            // make an otherwise ineligible candidate publishable.
            let physical =
                JdfPostInference.physicalResolutionEligible policy {
                    finalScore=final;margin=margin;routedAdvantage=routedAdvantage
                    geometryMargin=geometryMargin;contradictory=contradictory
                    contextCount=contexts.Length;winnerShare=winnerShare
                    establishedStrong=establishedStrong
                    hasAlternativeCorridors=alternatives;alternativesAgree=alternativesAgree }
            if physical then
                let tiedCandidates =
                    candidateScores
                    |> Array.filter(fun (_,otherGeometry,otherFinal,_,_,_,_) ->
                        abs(otherFinal-final)<=1e-12 && abs(otherGeometry-geometry)<=1e-12)
                    |> Array.choose(fun (other,otherGeometry,otherFinal,otherRouted,otherFaces,
                                          otherAlternatives,otherAlternativesAgree) ->
                        let lower =
                            candidateScores
                            |> Array.tryFind(fun (_,candidateGeometry,candidateFinal,_,_,_,_) ->
                                candidateFinal<otherFinal-1e-12
                                || (abs(candidateFinal-otherFinal)<=1e-12
                                    && candidateGeometry<otherGeometry-1e-12))
                        let individualMargin =
                            lower
                            |> Option.map(fun (_,_,candidateFinal,_,_,_,_) -> otherFinal-candidateFinal)
                            |> Option.defaultValue otherFinal
                        let otherWinnerSupport =
                            winnerCounts |> Array.tryFind(fun (value,_) -> value=other)
                            |> Option.map snd |> Option.defaultValue 0
                        let otherWinnerShare =
                            if contexts.Length=0 then 0.0
                            else float otherWinnerSupport/float contexts.Length
                        let otherContradictory =
                            winners |> Array.exists(fun (candidateValue,_,score,_,lead,_,_,_) ->
                                candidateValue<>other && score>=policy.consensus.minimumPerContextScore
                                && lead>=policy.consensus.minimumPerContextLead)
                        let otherConsensus =
                            contexts.Length>=policy.consensus.minimumContexts
                            && otherWinnerShare>=policy.consensus.minimumWinningShare
                            && not otherContradictory
                        let otherEstablished = externalSupport |> Map.tryFind other |> Option.defaultValue 0
                        let otherEstablishedRunner =
                            externalSupport |> Seq.filter(fun pair -> pair.Key<>other)
                            |> Seq.map(fun pair -> pair.Value) |> Seq.sortDescending
                            |> Seq.tryHead |> Option.defaultValue 0
                        let otherEstablishedStrong =
                            JdfPostInference.establishedPostStrong policy otherEstablished
                                otherEstablishedRunner 0.0 otherContradictory
                                otherAlternatives otherAlternativesAgree
                        let otherMaterialRouted =
                            match lower with
                            | Some(_,lowerGeometry,_,lowerRouted,_,_,_) ->
                                otherRouted-lowerRouted>=policy.resolution.materialRoutedAdvantage
                                && otherGeometry-lowerGeometry>= -0.000001
                            | None -> otherRouted>=policy.resolution.materialRoutedAdvantage
                        let independentlyPublishable =
                            otherFinal>=policy.resolution.minimumPhysicalScore
                            && (individualMargin>=policy.resolution.minimumPhysicalMargin
                                || otherMaterialRouted)
                            && not otherContradictory
                            && (contexts.Length<policy.consensus.minimumContexts
                                || otherConsensus || otherEstablishedStrong)
                            && (not otherAlternatives || otherAlternativesAgree)
                        if independentlyPublishable
                           && otherFinal>=policy.sameStopPairs.minimumIndividualScore
                           && individualMargin>=policy.sameStopPairs.minimumIndividualMargin then
                            let latitude,longitude=coordinates other
                            Some { candidateId=other;corridorFaceId=otherFaces |> Seq.tryHead
                                   score=otherFinal;geometry=otherGeometry;margin=individualMargin
                                   latitude=latitude;longitude=longitude }
                        else None)
                jointCandidates.[struct(stopId,familyId)]<-tiedCandidates
                let latitude,longitude=coordinates candidate
                { stopId=stopId;movementFamilyId=familyId;mode=first.mode;lineId=first.lineId;role=first.role
                  resolution="Physical";candidateId=Some candidate;corridorFaceId=faces |> Seq.tryHead
                  representativeCandidateId=Some candidate
                  score=Some final;margin=Some margin;unsafePhysical=alternatives && not alternativesAgree
                  latitude=latitude;longitude=longitude }
            else
                let _,topGeometry,_,_,_,_,_=candidateScores.[0]
                let plausible=candidateScores |> Array.filter(fun (_,geometry,final,_,_,_,_) ->
                    JdfPostInference.plausibleCandidate policy topGeometry geometry final)
                let plausibleFaces=plausible |> Seq.collect(fun (_,_,_,_,values,_,_) -> values) |> Set
                let conflictingAlternative =
                    plausible |> Array.exists(fun (_,_,_,_,_,hasAlternatives,agrees) -> hasAlternatives && not agrees)
                let plausibleCoordinates =
                    plausible |> Array.choose(fun (id,_,_,_,_,_,_) ->
                        match coordinates id with Some lat,Some lon -> Some(lat,lon) | _ -> None)
                let compactness =
                    seq {
                        for left in plausibleCoordinates do
                            for right in plausibleCoordinates do
                                let leftLat,leftLon=left
                                let rightLat,rightLon=right
                                let dlat=(leftLat-rightLat)*111_320.0
                                let dlon=(leftLon-rightLon)*111_320.0*Math.Cos((leftLat+rightLat)*Math.PI/360.0)
                                yield Math.Sqrt(dlat*dlat+dlon*dlon)
                    } |> Seq.append [0.0] |> Seq.max
                if JdfPostInference.sideResolutionEligible policy plausible.Length plausibleFaces.Count
                       conflictingAlternative plausibleFaces.Count contexts.Length compactness then
                    let latitude,longitude=coordinates candidate
                    { stopId=stopId;movementFamilyId=familyId;mode=first.mode;lineId=first.lineId;role=first.role
                      resolution="Side";candidateId=None;corridorFaceId=plausibleFaces |> Seq.tryHead
                      representativeCandidateId=Some candidate
                      score=Some final;margin=Some margin;unsafePhysical=false
                      latitude=latitude;longitude=longitude }
                else
                    { stopId=stopId;movementFamilyId=familyId;mode=first.mode;lineId=first.lineId;role=first.role
                      resolution="Centroid";candidateId=None;corridorFaceId=None
                      representativeCandidateId=None
                      score=None;margin=None;unsafePhysical=false;latitude=None;longitude=None })
    let decisionsByFamily =
        ordinaryDecisions
        |> Array.mapi(fun index value -> struct(value.stopId,value.movementFamilyId),(index,value))
        |> Map.ofArray
    let mutable sameStopBlocks=0
    let mutable distinctPairChoices=0
    let mutable unresolvedBlockEdges=0
    if policy.sameStopPairs.enabled then
        rows
        |> Array.groupBy _.sameStopBlockId
        |> Array.choose(fun (blockId,blockRows) -> blockId |> Option.map(fun value -> value,blockRows))
        |> Array.sortBy fst
        |> Array.iter(fun (_,blockRows) ->
            sameStopBlocks<-sameStopBlocks+1
            let contexts = blockRows |> Array.distinctBy _.contextId
            if contexts.Length<>2 then unresolvedBlockEdges<-unresolvedBlockEdges+1 else
            let left,right=contexts.[0],contexts.[1]
            match decisionsByFamily |> Map.tryFind(struct(left.stopId,left.movementFamilyId)),
                  decisionsByFamily |> Map.tryFind(struct(right.stopId,right.movementFamilyId)) with
            | Some(leftIndex,leftDecision),Some(rightIndex,rightDecision)
                when JdfPostInference.sameStopDistinctnessEligible {
                         contextCount=contexts.Length
                         leftAssignmentKind=left.assignmentKind
                         rightAssignmentKind=right.assignmentKind
                         leftMovementFamilyId=left.movementFamilyId
                         rightMovementFamilyId=right.movementFamilyId
                         leftResolution=leftDecision.resolution
                         rightResolution=rightDecision.resolution
                         leftCandidateId=leftDecision.candidateId
                         rightCandidateId=rightDecision.candidateId } ->
                    let leftChoices =
                        match jointCandidates.TryGetValue(struct(left.stopId,left.movementFamilyId)) with
                        | true,values -> values | _ -> [||]
                    let rightChoices =
                        match jointCandidates.TryGetValue(struct(right.stopId,right.movementFamilyId)) with
                        | true,values -> values | _ -> [||]
                    let policyChoices values =
                        values |> Array.map(fun value ->
                            ({ candidateId=value.candidateId;ordinaryScore=value.score
                               ordinaryGeometry=value.geometry;individualMargin=value.margin
                               independentlyPublishable=true }
                             : JdfPostInference.SameStopPairChoice))
                    let pair =
                        JdfPostInference.selectDistinctSameStopPair policy.sameStopPairs
                            (policyChoices leftChoices) (policyChoices rightChoices)
                        |> Option.bind(fun (leftId,rightId) ->
                            match leftChoices |> Array.tryFind(fun value -> value.candidateId=leftId),
                                  rightChoices |> Array.tryFind(fun value -> value.candidateId=rightId) with
                            | Some leftChoice,Some rightChoice -> Some(leftChoice,rightChoice)
                            | _ -> None)
                    match pair with
                    | Some(leftChoice,rightChoice) ->
                        let replace decision choice =
                            { decision with candidateId=Some choice.candidateId
                                            representativeCandidateId=Some choice.candidateId
                                            corridorFaceId=choice.corridorFaceId
                                            score=Some choice.score;margin=Some choice.margin
                                            latitude=choice.latitude;longitude=choice.longitude }
                        ordinaryDecisions.[leftIndex]<-replace leftDecision leftChoice
                        ordinaryDecisions.[rightIndex]<-replace rightDecision rightChoice
                        distinctPairChoices<-distinctPairChoices+1
                    | None -> unresolvedBlockEdges<-unresolvedBlockEdges+1
            | _ -> () )
    ordinaryDecisions
    |> Array.sortBy(fun value -> value.stopId,value.mode,value.lineId,value.movementFamilyId),
    { sameStopBlocks=sameStopBlocks;distinctPairChoices=distinctPairChoices
      unresolvedBlockEdges=unresolvedBlockEdges },hypothesisDetails

let private evaluateReplayPolicies capturedHorizon
                                   (policies:JdfPostInferencePolicy.PostInferencePolicyV2 array)
                                   (routePointsByStop:Map<int64,ReplayRoutePoint array>)
                                   (rows:seq<ReplayScore>) =
    let decisions = policies |> Array.map (fun _ -> ResizeArray<ReplayDecision>())
    let counters = policies |> Array.map(fun _ -> ref {
        sameStopBlocks=0;distinctPairChoices=0;unresolvedBlockEdges=0 })
    let stopRows=ResizeArray<ReplayScore>()
    let summaries=Dictionary<int64,struct(int*int*HashSet<string>)>()
    let mutable currentStop=None
    let mutable evidenceRows=0L
    let flush () =
        if stopRows.Count>0 then
            let values=stopRows.ToArray()
            let points=routePointsByStop |> Map.tryFind values.[0].stopId |> Option.defaultValue [||]
            for index=0 to policies.Length-1 do
                let stopDecisions,stopCounters,_=
                    evaluateReplayPolicy capturedHorizon policies.[index] points values
                decisions.[index].AddRange(stopDecisions)
                let current=counters.[index].Value
                counters.[index].Value <- {
                    sameStopBlocks=current.sameStopBlocks+stopCounters.sameStopBlocks
                    distinctPairChoices=current.distinctPairChoices+stopCounters.distinctPairChoices
                    unresolvedBlockEdges=current.unresolvedBlockEdges+stopCounters.unresolvedBlockEdges }
            stopRows.Clear()
    for row in rows do
        match currentStop with
        | Some stopId when stopId<>row.stopId -> flush()
        | _ -> ()
        currentStop<-Some row.stopId
        stopRows.Add(row)
        let struct(rowCount,usableCount,faces) =
            match summaries.TryGetValue(row.stopId) with
            | true,value -> value
            | _ -> struct(0,0,HashSet<string>(StringComparer.Ordinal))
        row.corridorFaceId |> Option.iter(fun value -> faces.Add(value) |> ignore)
        summaries.[row.stopId] <-
            struct(rowCount+1,usableCount+(if row.topologyFailureReason.IsNone then 1 else 0),faces)
        evidenceRows<-evidenceRows+1L
    flush()
    let summaryMap =
        summaries
        |> Seq.map(fun pair ->
            let struct(rows,usable,faces)=pair.Value
            pair.Key,{ rowCount=rows;usableRowCount=usable
                       routePointCount=routePointsByStop |> Map.tryFind pair.Key |> Option.map Array.length |> Option.defaultValue 0
                       distinctFaces=faces.Count })
        |> Map.ofSeq
    decisions |> Array.map (fun values -> values.ToArray()),
    counters |> Array.map(fun value -> value.Value),evidenceRows,summaryMap

let private replayTransportMode = function
    | "A" -> JdfModel.Bus
    | "E" -> JdfModel.Tram
    | "T" -> JdfModel.Trolleybus
    | "L" -> JdfModel.CableCar
    | "M" -> JdfModel.Metro
    | "P" -> JdfModel.Ferry
    | value -> invalidArg "evidencePath" $"Unsupported routed-evidence mode: {value}"

let private stableHex (payload:string) =
    SHA256.HashData(Encoding.UTF8.GetBytes(payload))
    |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()

let private replayPhysicalLocationId stopId candidateId =
    let payload = $"{stopId}|{candidateId}"
    $"estimated:{stableHex payload}"

let private replaySideGroupId stopId (mode:string) face =
    stableHex $"{stopId}|{mode}|{face}"

let private replaySideLocationId stopId groupId =
    let payload = $"{stopId}|side|{groupId}"
    $"estimated-side:{stableHex payload}"

let private replaySector (points:ReplayRoutePoint array) latitude longitude =
    if points.Length=0 then "?" else
    let centreLatitude=points |> Array.averageBy _.latitude
    let centreLongitude=points |> Array.averageBy _.longitude
    let north=(latitude-centreLatitude)*111_320.0
    let east=(longitude-centreLongitude)*111_320.0*Math.Cos(centreLatitude*Math.PI/180.0)
    if Math.Sqrt(north*north+east*east)<2.0 then "?" else
    let index=int(Math.Round(((Math.Atan2(east,north)*180.0/Math.PI+360.0)%360.0)/45.0))%8
    [|"N";"NE";"E";"SE";"S";"SW";"W";"NW"|].[index]

let private sideGroupCatalog
        (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
        (routePointsByStop:Map<int64,ReplayRoutePoint array>)
        (decisions:ReplayDecision array)
        : JdfPostInference.GlobalPostSideGroup array =
    decisions
    |> Array.choose(fun value ->
        match value.resolution,value.corridorFaceId,value.representativeCandidateId,
              value.latitude,value.longitude with
        | "Side",Some face,Some hypothesisId,Some latitude,Some longitude ->
            Some(value.stopId,value.mode,face,value.movementFamilyId,
                 hypothesisId,latitude,longitude)
        | _ -> None)
    |> Array.groupBy(fun (stopId,mode,face,_,_,_,_) -> stopId,mode,face)
    |> Array.choose(fun ((stopId,mode,face),values) ->
        let members=values |> Array.distinctBy(fun (_,_,_,_,candidateId,_,_) -> candidateId)
        let distance (_,_,_,_,_,leftLat,leftLon) (_,_,_,_,_,rightLat,rightLon) =
            let dlat=(leftLat-rightLat)*111_320.0
            let dlon=(leftLon-rightLon)*111_320.0*Math.Cos((leftLat+rightLat)*Math.PI/360.0)
            Math.Sqrt(dlat*dlat+dlon*dlon)
        let compactness =
            seq { for left in members do for right in members do yield distance left right }
            |> Seq.append [0.0] |> Seq.max
        if compactness>policy.sideGroups.maximumCompactnessMetres then None else
        let medoid=members |> Array.minBy(fun candidate ->
            members |> Array.sumBy(distance candidate),
            (let _,_,_,_,candidateId,_,_=candidate in candidateId))
        let _,_,_,_,representative,latitude,longitude=medoid
        let groupId=replaySideGroupId stopId mode face
        let group:JdfPostInference.GlobalPostSideGroup = {
            sideGroupId=groupId
            stopId=stopId;mode=mode;corridorFaceId=face
            sector=replaySector
                (routePointsByStop |> Map.tryFind stopId |> Option.defaultValue [||])
                latitude longitude
            representativeHypothesisId=representative
            memberHypothesisIds=
                members |> Array.map(fun (_,_,_,_,candidateId,_,_) -> candidateId) |> Array.sort
            latitude=latitude;longitude=longitude;compactnessMetres=compactness
            support=values |> Array.distinctBy(fun (_,_,_,family,_,_,_) -> family) |> Array.length }
        Some group)
    |> Array.groupBy(fun value -> value.stopId,value.mode)
    |> Array.collect(fun (_,groups) ->
        groups
        |> Array.sortBy(fun value -> -value.support,value.corridorFaceId)
        |> Array.mapi(fun index value -> index,value)
        |> Array.choose(fun (index,value) ->
            if index<policy.sideGroups.maximumOrdinaryGroups
               || value.support>=policy.sideGroups.additionalGroupMinimumContexts
            then Some value else None))
    |> Array.sortBy(fun value -> value.stopId,value.mode,value.sideGroupId)

[<Literal>]
let private ResultRowMemoryBudgetBytes=256L*1024L*1024L

/// The sole production policy-evaluation boundary. The store has already
/// passed structural, hash, ordering, key, FK, sentinel, and coverage checks;
/// no JDF, graph, GTFS object, path, or unvalidated row collection can enter.
let evaluateWithDiagnostics includeDiagnostics
                            (store:JdfPostEvidenceStore.PostEvidenceStore)
                            (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
                            : JdfPostInference.PostInferenceResult =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "evaluator-entry"
    Interlocked.Increment(&evaluatorEntryCountValue) |> ignore
    let manifest=store.Manifest
    let policy =
        JdfPostInferencePolicy.validatePolicyForEvidence
            manifest.captureCeilings.routedExcessMetres
            manifest.captureCeilings.maximumCorridorVariants policy
    let evidencePath=store.Directory
    let routePointsByStop=readReplayRoutePoints evidencePath None
    let temporaryDirectory=Path.Combine(Path.GetTempPath(),"jrutil-post-inference-results")
    let decisions=ResizeArray<ReplayDecision>()
    let hypotheses=ResizeArray<JdfPostInference.ConsolidatedPostHypothesis>()
    let mutable policyCounters = {
        sameStopBlocks=0;distinctPairChoices=0;unresolvedBlockEdges=0 }
    let mutable evidenceRows=0L
    // The assignment store drives the one and only joined evidence traversal
    // in publication mode.  Per-stop evaluation completes before the next
    // stop is read, so consolidation, decisions, hypotheses, side groups and
    // assignments derive from the same bounded stop-major buffer.
    let sideGroups=ResizeArray<JdfPostInference.GlobalPostSideGroup>()
    let assignments:JdfPostInference.IReplayableRowStore<JdfPostInference.ContextPostAssignment> =
        JdfPostInference.ReplayableRowStore<JdfPostInference.ContextPostAssignment>.Create(
            ResultRowMemoryBudgetBytes,temporaryDirectory,
            seq {
                let stopRows=ResizeArray<ReplayScore>()
                let contextRows=ResizeArray<ReplayScore>()
                let mutable currentStop:int64 option=None
                let mutable previousContext:string option=None
                let flush () = seq {
                    if stopRows.Count>0 then
                        let values=stopRows.ToArray()
                        let stopId=values.[0].stopId
                        let points=routePointsByStop |> Map.tryFind stopId |> Option.defaultValue [||]
                        let stopDecisions,stopCounters,details =
                            evaluateReplayPolicy manifest.captureCeilings.routedExcessMetres policy points values
                        decisions.AddRange(stopDecisions)
                        policyCounters <- {
                            sameStopBlocks=policyCounters.sameStopBlocks+stopCounters.sameStopBlocks
                            distinctPairChoices=policyCounters.distinctPairChoices+stopCounters.distinctPairChoices
                            unresolvedBlockEdges=policyCounters.unresolvedBlockEdges+stopCounters.unresolvedBlockEdges }
                        for pair in details do
                            let medoid,_,_,_,_,_,members=pair.Value
                            hypotheses.Add({
                                hypothesisId=pair.Key
                                stopId=stopId
                                memberObservationIds=
                                    members |> Array.collect _.observationIds |> Array.distinct |> Array.sort
                                memberRoutePointIds=members |> Array.map _.routePointId |> Array.sort
                                representativeRoutePointId=medoid.routePointId
                                latitude=medoid.latitude
                                longitude=medoid.longitude })
                        let stopSideGroups=sideGroupCatalog policy routePointsByStop stopDecisions
                        sideGroups.AddRange(stopSideGroups)
                        let sideGroupsByFamily =
                            stopDecisions
                            |> Array.choose(fun decision ->
                                match decision.resolution,decision.corridorFaceId with
                                | "Side",Some face ->
                                    stopSideGroups
                                    |> Array.tryFind(fun group ->
                                        group.stopId=decision.stopId && group.mode=decision.mode
                                        && group.corridorFaceId=face)
                                    |> Option.map(fun group ->
                                        struct(decision.stopId,decision.movementFamilyId),group)
                                | _ -> None)
                            |> Map.ofArray
                        let decisionsByFamily =
                            stopDecisions
                            |> Array.map(fun value ->
                                struct(value.stopId,value.movementFamilyId),value)
                            |> Map.ofArray
                        for context in contextRows do
                            let decision=decisionsByFamily.[struct(context.stopId,context.movementFamilyId)]
                            let resolution,locationId,hypothesisId,sideGroupId =
                                if context.assignmentKind<>"unlabelled" then
                                    decision.resolution,None,None,None
                                elif decision.resolution="Physical" then
                                    match decision.candidateId with
                                    | Some candidate ->
                                        "Physical",Some(replayPhysicalLocationId context.stopId candidate),
                                        Some candidate,None
                                    | None -> "Centroid",None,None,None
                                elif decision.resolution="Side" then
                                    match sideGroupsByFamily |> Map.tryFind(struct(context.stopId,context.movementFamilyId)) with
                                    | Some group ->
                                        "Side",Some(replaySideLocationId context.stopId group.sideGroupId),
                                        Some group.representativeHypothesisId,Some group.sideGroupId
                                    | None -> "Centroid",None,None,None
                                else "Centroid",None,None,None
                            let assignment:JdfPostInference.ContextPostAssignment = {
                                contextId=context.contextId
                                stopId=context.stopId;mode=context.mode;lineId=context.lineId
                                routeDistinction=context.routeDistinction;direction=context.direction
                                patternHash=context.patternHash;patternPosition=context.position
                                previousStopId=context.previousStopId;nextStopId=context.nextStopId
                                assignmentKind=context.assignmentKind;authoredPostKey=context.authoredPostKey
                                sameStopBlockId=context.sameStopBlockId;sameStopBlockRole=context.role
                                movementFamilyId=context.movementFamilyId;resolution=resolution
                                selectedLocationId=locationId;selectedHypothesisId=hypothesisId
                                selectedSideGroupId=sideGroupId;score=decision.score;margin=decision.margin }
                            yield assignment
                        stopRows.Clear()
                        contextRows.Clear()
                    }
                for row in replayScoreRows includeDiagnostics evidencePath None do
                    match currentStop with
                    | Some stopId when stopId<>row.stopId -> yield! flush()
                    | _ -> ()
                    currentStop<-Some row.stopId
                    stopRows.Add(row)
                    evidenceRows<-evidenceRows+1L
                    if previousContext<>Some row.contextId then
                        previousContext<-Some row.contextId
                        contextRows.Add(row)
                yield! flush() })
    let hypotheses=hypotheses.ToArray() |> Array.sortBy(fun value -> value.stopId,value.hypothesisId)
    let hypothesisById=hypotheses |> Array.map(fun value -> value.hypothesisId,value) |> Map.ofArray
    let decisions=decisions.ToArray()
    let decisionsByFamily =
        decisions |> Array.map(fun value -> struct(value.stopId,value.movementFamilyId),value) |> Map.ofArray
    let sideGroups=sideGroups.ToArray() |> Array.sortBy(fun value -> value.stopId,value.mode,value.sideGroupId)
    try
        let publishedPhysical =
            assignments.ReadRows()
            |> Seq.choose(fun value ->
                match value.assignmentKind,value.selectedHypothesisId,value.selectedLocationId with
                | "unlabelled",Some hypothesis,Some location
                    when value.resolution="Physical" ->
                    Some(struct(value.stopId,hypothesis),location)
                | _ -> None)
            |> Map.ofSeq
        let authoredPositions:JdfPostInference.AuthoredPostPosition array =
            JdfPostInferencePolicy.PostInferencePhaseProbe.record "authored-selection"
            assignments.ReadRows()
            |> Seq.choose(fun value ->
                match value.assignmentKind,value.authoredPostKey with
                | "authored",Some key -> Some((value.stopId,key),value)
                | _ -> None)
            |> Seq.groupBy fst
            |> Seq.choose(fun (authoredKey,values) ->
                let contexts=values |> Seq.map snd |> Seq.toArray
                let relevant =
                    contexts
                    |> Array.choose(fun context ->
                        decisionsByFamily |> Map.tryFind(struct(context.stopId,context.movementFamilyId)))
                let physical =
                    relevant |> Array.choose(fun value ->
                        match value.resolution,value.candidateId,value.score,value.margin with
                        | "Physical",Some candidate,Some score,margin ->
                            Some(candidate,score,margin)
                        | _ -> None)
                let candidates=physical |> Array.map(fun (candidate,_,_) -> candidate) |> Array.distinct
                match candidates with
                | [|candidate|]
                    when JdfPostInference.authoredResolutionEligible policy relevant.Length
                             physical.Length candidates.Length
                             (physical |> Array.map(fun (_,score,margin) -> score,margin)) ->
                    match publishedPhysical |> Map.tryFind(struct(fst authoredKey,candidate)),
                          hypothesisById |> Map.tryFind candidate with
                    | Some location,Some hypothesis ->
                        let position:JdfPostInference.AuthoredPostPosition = {
                            stopId=fst authoredKey
                            authoredPostKey=snd authoredKey;locationId=location
                            latitude=Some hypothesis.latitude;longitude=Some hypothesis.longitude }
                        Some position
                    | _ -> None
                | _ -> None)
            |> Seq.sortBy(fun value -> value.stopId,value.authoredPostKey)
            |> Seq.toArray
        let diagnosticRows:seq<JdfPostInference.PostInferenceDiagnosticScore> = seq {
            let routePointsById =
                routePointsByStop
                |> Seq.collect(fun pair -> pair.Value)
                |> Seq.map(fun value -> value.routePointId,value)
                |> Map.ofSeq
            let stopRows=ResizeArray<ReplayScore>()
            let mutable currentStop:int64 option=None
            let emit () = seq {
                if stopRows.Count>0 then
                    let stopId=stopRows.[0].stopId
                    let points=routePointsByStop |> Map.tryFind stopId |> Option.defaultValue [||]
                    let rows,details=consolidateReplayRows policy points (stopRows.ToArray())
                    let selectedRanksByContext =
                        rows
                        |> Array.groupBy _.contextId
                        |> Array.map(fun (contextId,values) ->
                            let ranks =
                                values
                                |> Array.map(fun row ->
                                    let fact:JdfPostInference.AlternativeCorridorFact = {
                                        variantRank=row.variantRank
                                        relativeCostMetres=row.relativeCostMetres
                                        relativeCostFraction=row.relativeCostFraction }
                                    fact)
                                |> JdfPostInference.selectedCorridorVariantRanks policy
                            contextId,ranks)
                        |> Map.ofArray
                    for row in rows do
                        let lifecycleFailure =
                            details |> Map.tryFind row.candidateId
                            |> Option.bind(fun (_,hasCurrent,hasObsolete,_,_,_,_) ->
                                if hasObsolete && not hasCurrent then Some "obsolete-lifecycle" else None)
                        let rejection =
                            JdfPostInference.hardGateReason policy
                                (lifecycleFailure |> Option.orElse row.topologyFailureReason)
                                row.corridorDistance row.signedLateralOffset row.routedExcess
                        let routed =
                            row.routedExcess
                            |> Option.map(JdfPostInference.routedFit policy)
                            |> Option.defaultValue 0.0
                        let alignment,side,proximity =
                            JdfPostInference.geometryComponents policy row.corridorDistance
                                row.signedLateralOffset row.corridorHeading row.attachmentHeading
                        let geometry=JdfPostInference.geometryScore policy alignment side proximity routed
                        let sourceAdjustment,modalityAdjustment =
                            match details |> Map.tryFind row.candidateId with
                            | Some(_,_,_,supportWeight,explicitModes,deniedModes,_) ->
                                replaySupportAdjustments policy row.mode supportWeight
                                    explicitModes deniedModes
                            | _ ->
                                routePointsById |> Map.tryFind row.candidateId
                                |> Option.map(fun point ->
                                    replaySupportAdjustments policy row.mode point.supportWeight
                                        point.explicitModes point.deniedModes)
                                |> Option.defaultValue(0.0,0.0)
                        let support=JdfPostInference.combinedSupportAdjustment
                                        policy sourceAdjustment modalityAdjustment
                        let diagnostic:JdfPostInference.PostInferenceDiagnosticScore = {
                            contextId=row.contextId
                            candidateId=row.candidateId;variantRank=row.variantRank
                            eligible=rejection.IsNone;alignment=alignment;side=side
                            proximity=proximity;routedExcess=routed
                            routedExcessMetres=row.routedExcess;corridorId=row.corridorId
                            ingressThreadId=row.ingressThreadId;egressThreadId=row.egressThreadId
                            corridorFaceId=row.corridorFaceId;routingAvailability=row.routingAvailability
                            alternativeCorridorCount=selectedRanksByContext.[row.contextId].Length
                            alternativeCostGap=row.relativeCostMetres
                            selectedCorridorRanks=selectedRanksByContext.[row.contextId]
                            tiedCorridorsAgree=row.tiedCorridorsAgree
                            corridorDistance=row.corridorDistance
                            signedLateralOffset=row.signedLateralOffset
                            corridorHeading=row.corridorHeading
                            attachmentHeading=row.attachmentHeading
                            snapEdgeId=row.snapEdgeId;snapFraction=row.snapFraction
                            topologyFailureReason=row.topologyFailureReason
                            sourceAdjustment=sourceAdjustment
                            modalityAdjustment=modalityAdjustment;popularityAdjustment=0.0
                            total=JdfPostInference.supportingScore geometry support
                            rejectionReason=rejection }
                        yield diagnostic
                    stopRows.Clear()
            }
            for row in replayScoreRows true evidencePath None do
                match currentStop with
                | Some stopId when stopId<>row.stopId ->
                    yield! emit()
                | _ -> ()
                currentStop<-Some row.stopId
                stopRows.Add(row)
            yield! emit()
        }
        let diagnostics:JdfPostInference.IReplayableRowStore<JdfPostInference.PostInferenceDiagnosticScore> =
            JdfPostInference.ReplayableRowStore<JdfPostInference.PostInferenceDiagnosticScore>.Create(
                ResultRowMemoryBudgetBytes,temporaryDirectory,
                if includeDiagnostics then diagnosticRows else Seq.empty)
        let unresolved=assignments.ReadRows() |> Seq.filter(fun value -> value.assignmentKind="unlabelled" && value.selectedLocationId.IsNone) |> Seq.length
        let counters:JdfPostInference.PostInferenceCounters = {
            evidenceRows=evidenceRows
            contextCount=int assignments.Count;candidateStopCount=routePointsByStop.Count
            unresolvedContexts=unresolved;authoredPositions=authoredPositions.Length
            sameStopBlocks=policyCounters.sameStopBlocks
            distinctPairChoices=policyCounters.distinctPairChoices
            unresolvedBlockEdges=policyCounters.unresolvedBlockEdges
            physicalResolutions=decisions |> Array.filter(fun value -> value.resolution="Physical") |> Array.length
            sideResolutions=decisions |> Array.filter(fun value -> value.resolution="Side") |> Array.length
            centroidResolutions=decisions |> Array.filter(fun value -> value.resolution="Centroid") |> Array.length }
        new JdfPostInference.PostInferenceResult(
            hypotheses,sideGroups,assignments,authoredPositions,diagnostics,counters)
    with _ ->
        assignments.Dispose()
        reraise()

let evaluate (store:JdfPostEvidenceStore.PostEvidenceStore)
             (policy:JdfPostInferencePolicy.PostInferencePolicyV2) =
    evaluateWithDiagnostics true store policy

let writeReplayReport (evidenceStore:JdfPostEvidenceStore.PostEvidenceStore)
                      policyPath policyGridPath expectationsPath reviewStopsPath outputPath =
    let evidenceFull=evidenceStore.Directory
    let outputFull=Path.GetFullPath(outputPath)
    if Directory.Exists(outputFull) || File.Exists(outputFull) then invalidArg "outputPath" $"Replay output already exists: {outputFull}"
    let evidenceManifest=evidenceStore.Manifest
    let capturedHorizon=evidenceManifest.captureCeilings.routedExcessMetres
    let selectedStops =
        reviewStopsPath
        |> Option.map(fun path ->
            File.ReadAllLines(path) |> Seq.map _.Trim()
            |> Seq.filter(fun value -> value<>"" && not(value.StartsWith("#")))
            |> Seq.map(fun value -> Int64.Parse(value.Split([|'\t';','|]).[0],CultureInfo.InvariantCulture))
            |> Set)
    let policies =
        match policyGridPath,policyPath with
        | Some path,_ -> JdfPostInferencePolicy.loadPolicyGrid path
        | None,Some path -> [|JdfPostInferencePolicy.loadPolicy path|]
        | _ -> [|JdfPostInferencePolicy.conservativeRoutedV4|]
    let expectations =
        expectationsPath |> Option.map(fun path ->
            let lines=File.ReadAllLines(path)
            if lines.Length=0 then [||] else
            let headers=lines.[0].Split('\t') |> Array.mapi(fun index value -> value,index) |> Map
            let required name=headers |> Map.tryFind name |> Option.defaultWith(fun () ->
                invalidArg "expectationsPath" $"Expectation file is missing {name}")
            let optional name=Map.tryFind name headers
            let text (fields:string array) name =
                optional name |> Option.bind(fun index ->
                    if index<fields.Length && not(String.IsNullOrWhiteSpace(fields.[index]))
                    then Some(fields.[index].Trim()) else None)
            lines |> Array.skip 1 |> Array.filter(String.IsNullOrWhiteSpace>>not)
            |> Array.map(fun row ->
                let fields=row.Split('\t')
                { stopId=Int64.Parse(fields.[required "stop_id"],CultureInfo.InvariantCulture)
                  stopName=fields.[required "stop_name"]
                  description=fields.[required "expectation"]
                  minimumDistinctFaces=Int32.Parse(fields.[required "minimum_distinct_faces"],CultureInfo.InvariantCulture)
                  minimumEvidenceFaces=Int32.Parse(fields.[required "minimum_evidence_faces"],CultureInfo.InvariantCulture)
                  allowPhysicalUnderTiedCorridors=Boolean.Parse(fields.[required "allow_physical_under_tied_corridors"])
                  allowCentroidFallback=Boolean.Parse(fields.[required "allow_centroid_fallback"])
                  mode=text fields "mode";lineId=text fields "line_id";role=text fields "same_stop_role"
                  expectedResolution=text fields "expected_resolution"
                  targetObservationIds=text fields "target_observation_ids"
                                       |> Option.map(fun value -> value.Split(';',StringSplitOptions.RemoveEmptyEntries)
                                                                 |> Array.map _.Trim() |> Array.sort)
                                       |> Option.defaultValue [||]
                  targetLatitude=text fields "target_latitude" |> Option.map(fun value -> Double.Parse(value,CultureInfo.InvariantCulture))
                  targetLongitude=text fields "target_longitude" |> Option.map(fun value -> Double.Parse(value,CultureInfo.InvariantCulture))
                  coordinateToleranceMetres=text fields "coordinate_tolerance_metres"
                                            |> Option.map(fun value -> Double.Parse(value,CultureInfo.InvariantCulture))
                                            |> Option.defaultValue 25.0 }))
        |> Option.defaultValue [||]
    let routePointsByStop=readReplayRoutePoints evidenceFull selectedStops
    let routePointsById =
        routePointsByStop |> Seq.collect(fun pair -> pair.Value)
        |> Seq.map(fun value -> value.routePointId,value) |> Map.ofSeq
    // Corpus replay is deliberately tiny. Materialize the already row-group-
    // pruned rows once so review score output does not reopen and decode the
    // same national Parquet a second time. Full-national replay remains
    // streaming and memory bounded.
    let selectedReplayRows =
        selectedStops |> Option.map(fun _ -> replayScoreRows true evidenceFull selectedStops |> Seq.toArray)
    let replayRows () : seq<ReplayScore> =
        selectedReplayRows |> Option.map(fun values -> values :> seq<_>)
        |> Option.defaultWith(fun () -> replayScoreRows true evidenceFull selectedStops)
    let decisionsByPolicy,countersByPolicy,evidenceRowCount,evidenceSummaries =
        replayRows ()
        |> evaluateReplayPolicies capturedHorizon policies routePointsByStop
    let expectationMatches (expectation:ReplayExpectation) (value:ReplayDecision) =
        value.stopId=expectation.stopId
        && expectation.mode |> Option.forall((=) value.mode)
        && expectation.lineId |> Option.forall((=) value.lineId)
        && expectation.role |> Option.forall((=) value.role)
    let exactTargetPasses (expectation:ReplayExpectation) (values:ReplayDecision array) =
        let expectedCandidate =
            if expectation.targetObservationIds.Length=0 then None
            else
                let joined=String.Join("|",expectation.targetObservationIds)
                Some(stableHex($"{expectation.stopId}|{joined}"))
        values |> Array.exists(fun value ->
            expectation.expectedResolution |> Option.forall((=) value.resolution)
            && expectedCandidate |> Option.forall(fun candidate -> value.candidateId=Some candidate)
            && match expectation.targetLatitude,expectation.targetLongitude,value.latitude,value.longitude with
               | Some expectedLat,Some expectedLon,Some actualLat,Some actualLon ->
                   let dlat=(actualLat-expectedLat)*111_320.0
                   let dlon=(actualLon-expectedLon)*111_320.0*Math.Cos(expectedLat*Math.PI/180.0)
                   Math.Sqrt(dlat*dlat+dlon*dlon)<=expectation.coordinateToleranceMetres
               | None,None,_,_ -> true
               | _ -> false)
    let scorePolicy (policy:JdfPostInferencePolicy.PostInferencePolicyV2) decisions =
        let mutable unsafe=decisions |> Array.sumBy(fun value -> if value.unsafePhysical then 1 else 0)
        let mutable failed=0
        let mutable correct=0
        let mutable avoidableCentroids=0
        for expectation in expectations do
            let stop=decisions |> Array.filter(expectationMatches expectation)
            let faces=stop |> Seq.choose(fun value -> value.corridorFaceId |> Option.orElse value.candidateId) |> Set.ofSeq
            let stationUnsafe=not expectation.allowPhysicalUnderTiedCorridors
                              && (stop |> Array.exists(fun value -> value.resolution="Physical"))
            let evidencePasses =
                evidenceSummaries
                |> Map.tryFind expectation.stopId
                |> Option.exists(fun value ->
                    value.usableRowCount>0 && value.distinctFaces>=expectation.minimumEvidenceFaces)
            let centroidFallback =
                expectation.allowCentroidFallback && stop.Length>0
                && (stop |> Array.forall(fun value -> value.resolution="Centroid"))
            let decisionPasses=faces.Count>=expectation.minimumDistinctFaces || centroidFallback
            if stationUnsafe then unsafe<-unsafe+1
            if evidencePasses && decisionPasses && not stationUnsafe
               && exactTargetPasses expectation stop then correct<-correct+1 else failed<-failed+1
            avoidableCentroids<-avoidableCentroids+(stop |> Array.sumBy(fun value -> if value.resolution="Centroid" then 1 else 0))
        policy,decisions,unsafe,failed,correct,avoidableCentroids
    let results=Array.map2 scorePolicy policies decisionsByPolicy
    let policyDistance (policy:JdfPostInferencePolicy.PostInferencePolicyV2) =
        let baseline=JdfPostInferencePolicy.conservativeRoutedV4
        (abs(policy.resolution.minimumPhysicalScore-baseline.resolution.minimumPhysicalScore))
        + (abs(policy.resolution.minimumPhysicalMargin-baseline.resolution.minimumPhysicalMargin))
        + (abs(policy.hardGates.maximumRoutedExcessMetres-baseline.hardGates.maximumRoutedExcessMetres)/500.0)
    let winner = results |> Array.minBy(fun (policy:JdfPostInferencePolicy.PostInferencePolicyV2,_,unsafe,failed,correct,centroids) ->
        unsafe,failed,-correct,centroids,policyDistance policy,policy.policyId)
    Directory.CreateDirectory(outputFull) |> ignore
    let policy,decisions,unsafe,failed,correct,centroids=winner
    JdfPostInferencePolicy.writePolicy (Path.Combine(outputFull,"winning-policy.json")) policy
    use writer=new StreamWriter(Path.Combine(outputFull,"movement-family-decisions.tsv"),false,new UTF8Encoding(false))
    writer.WriteLine("stop_id\tmovement_family_id\tmode\tline_id\tresolution\tcandidate_id\tcorridor_face_id\tscore\tmargin")
    for value in decisions do
        let columns = [|
            string value.stopId
            value.movementFamilyId
            value.mode
            value.lineId
            value.resolution
            value.candidateId |> Option.defaultValue ""
            value.corridorFaceId |> Option.defaultValue ""
            value.score |> Option.map(fun x->x.ToString("R",CultureInfo.InvariantCulture)) |> Option.defaultValue ""
            value.margin |> Option.map(fun x->x.ToString("R",CultureInfo.InvariantCulture)) |> Option.defaultValue "" |]
        writer.WriteLine(String.Join('\t',columns))
    writer.Flush()
    use expectationWriter=new StreamWriter(Path.Combine(outputFull,"expectation-report.tsv"),false,new UTF8Encoding(false))
    expectationWriter.WriteLine("stop_id\tstop_name\texpectation\tclassification\tdistinct_decision_faces\tminimum_distinct_faces\tavailable_evidence_faces\tminimum_evidence_faces\tphysical\tside\tcentroid")
    for expectation in expectations do
        let stop=decisions |> Array.filter(expectationMatches expectation)
        let faces=stop |> Seq.choose(fun value -> value.corridorFaceId |> Option.orElse value.candidateId) |> Set.ofSeq
        let evidence=evidenceSummaries |> Map.tryFind expectation.stopId
        let unsafePhysical=not expectation.allowPhysicalUnderTiedCorridors
                           && (stop |> Array.exists(fun value -> value.resolution="Physical"))
        let centroidFallback =
            expectation.allowCentroidFallback && stop.Length>0
            && (stop |> Array.forall(fun value -> value.resolution="Centroid"))
        let decisionPasses=faces.Count>=expectation.minimumDistinctFaces || centroidFallback
        let classification =
            match evidence with
            | None -> "missing-label"
            | Some value when value.usableRowCount=0 -> "routing-evidence-failure"
            | Some value when value.distinctFaces < expectation.minimumEvidenceFaces -> "routing-evidence-failure"
            | _ when not decisionPasses -> "clustering-or-policy-failure"
            | _ when unsafePhysical
                     || not(exactTargetPasses expectation stop) -> "policy-failure"
            | _ -> "pass"
        let availableFaces=evidence |> Option.map(fun value -> value.distinctFaces) |> Option.defaultValue 0
        let count resolution=stop |> Array.sumBy(fun value -> if value.resolution=resolution then 1 else 0)
        let columns=[|string expectation.stopId;expectation.stopName;expectation.description;classification
                      string faces.Count;string expectation.minimumDistinctFaces;string availableFaces
                      string expectation.minimumEvidenceFaces;string (count "Physical")
                      string (count "Side");string (count "Centroid")|]
        expectationWriter.WriteLine(String.Join('\t',columns))
    expectationWriter.Flush()
    use policyWriter=new StreamWriter(Path.Combine(outputFull,"policy-results.tsv"),false,new UTF8Encoding(false))
    policyWriter.WriteLine("policy_id\tunsafe_physical\tfailed_expectations\tcorrect_expectations\tcentroid_decisions\tsame_stop_blocks\tdistinct_pair_choices\tunresolved_block_edges\tselected")
    for index=0 to results.Length-1 do
        let result=results.[index]
        let candidate,_,candidateUnsafe,candidateFailed,candidateCorrect,candidateCentroids=result
        let counters=countersByPolicy.[index]
        let columns=[|candidate.policyId;string candidateUnsafe;string candidateFailed;string candidateCorrect
                      string candidateCentroids;string counters.sameStopBlocks
                      string counters.distinctPairChoices;string counters.unresolvedBlockEdges
                      string (candidate.policyId=policy.policyId)|]
        policyWriter.WriteLine(String.Join('\t',columns))
    match selectedStops with
    | Some _ ->
        use scoreWriter=new StreamWriter(Path.Combine(outputFull,"candidate-scores.tsv"),false,new UTF8Encoding(false))
        scoreWriter.WriteLine("stop_id\tmode\tline_id\tdirection\tpattern_hash\tposition\trole\tmovement_family_id\troute_point_id\tface\talignment\tside\tproximity\trouted_excess_metres\tgeometry_score\tfinal_score\thard_gate\ttopology_failure")
        for row in replayRows () do
            let hardGate=JdfPostInference.hardGateReason policy row.topologyFailureReason
                             row.corridorDistance row.signedLateralOffset row.routedExcess
            let routed=row.routedExcess |> Option.map(JdfPostInference.routedFit policy) |> Option.defaultValue 0.0
            let alignment,side,proximity =
                JdfPostInference.geometryComponents policy row.corridorDistance
                    row.signedLateralOffset row.corridorHeading row.attachmentHeading
            let geometry=JdfPostInference.geometryScore policy alignment side proximity routed
            let sourceAdjustment,modalityAdjustment =
                routePointsById |> Map.tryFind row.candidateId
                |> Option.map(fun point -> replaySupportAdjustments policy row.mode point.supportWeight
                                                               point.explicitModes point.deniedModes)
                |> Option.defaultValue(0.0,0.0)
            let support=JdfPostInference.combinedSupportAdjustment
                            policy sourceAdjustment modalityAdjustment
            let final=JdfPostInference.supportingScore geometry support
            let number (value:float)=value.ToString("R",CultureInfo.InvariantCulture)
            let optionalNumber value=value |> Option.map number |> Option.defaultValue ""
            let columns=[|string row.stopId;row.mode;row.lineId;string row.direction;row.patternHash
                          string row.position;row.role;row.movementFamilyId;row.candidateId
                          row.corridorFaceId |> Option.defaultValue "";number alignment;number side
                          number proximity;optionalNumber row.routedExcess;number geometry;number final
                          hardGate |> Option.defaultValue "";row.topologyFailureReason |> Option.defaultValue ""|]
            scoreWriter.WriteLine(String.Join('\t',columns))
    | None -> ()
    let winningCounters=countersByPolicy.[policies |> Array.findIndex(fun value -> value.policyId=policy.policyId)]
    let summary = {| evidence_format=JdfPostInference.EvidenceFormat; policy_id=policy.policyId
                     evidence_rows=evidenceRowCount; movement_families=decisions.Length
                     unsafe_physical=unsafe; failed_expectations=failed; correct_expectations=correct
                     centroid_decisions=centroids; policies_evaluated=policies.Length
                     same_stop_blocks=winningCounters.sameStopBlocks
                     distinct_pair_choices=winningCounters.distinctPairChoices
                     unresolved_block_edges=winningCounters.unresolvedBlockEdges |}
    File.WriteAllText(Path.Combine(outputFull,"summary.json"),JsonSerializer.Serialize(summary,JsonSerializerOptions(WriteIndented=true))+"\n")
    let featureCollection =
        {| ``type``="FeatureCollection"
           features=
              Array.append
                (decisions |> Array.map(fun value ->
                    box {| ``type``="Feature"; geometry=(null:obj)
                           properties={| feature_kind="decision";stop_id=value.stopId
                                         movement_family_id=value.movementFamilyId;mode=value.mode
                                         line_id=value.lineId;resolution=value.resolution
                                         candidate_id=value.candidateId;corridor_face_id=value.corridorFaceId |} |}))
                (routePointsByStop |> Seq.collect(fun pair -> pair.Value)
                 |> Seq.map(fun value ->
                    box {| ``type``="Feature"
                           geometry={| ``type``="Point";coordinates=[|value.longitude;value.latitude|] |}
                           properties={| feature_kind="route_point";stop_id=value.stopId
                                         route_point_id=value.routePointId
                                         observation_ids=String.Join(";",value.observationIds) |} |})
                 |> Seq.toArray) |}
    File.WriteAllText(Path.Combine(outputFull,"review.geojson"),JsonSerializer.Serialize(featureCollection)+"\n")
