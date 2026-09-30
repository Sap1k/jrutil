// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Reading and validating captured post-inference evidence for replay.
module JrUtil.JdfPostEvidenceReader

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

type internal ReplayScore = {
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
    serviceEdgeCount:int
}

type internal ReplayContext = {
    stopId:int64;mode:string;lineId:string;routeDistinction:int;direction:int
    patternHash:string;position:int;role:string;movementFamilyId:string
    previousStopId:int64 option;nextStopId:int64 option
    assignmentKind:string;authoredPostKey:string option;sameStopBlockId:string option
}

type internal ReplayCorridor = {
    relativeCostMetres:float option;relativeCostFraction:float option
    corridorId:string option
    ingressThreadId:string option;egressThreadId:string option
    routingAvailability:string;invariantFailureReason:string option
    serviceEdgeCount:int
}

type internal ReplayRoutePoint = {
    stopId:int64; routePointId:string; latitude:float; longitude:float
    observationIds:string array
    hasCurrentLifecycle:bool; hasObsoleteLifecycle:bool
    supportWeight:float
    explicitModes:string array;deniedModes:string array
}

type internal ReplayDecision = {
    stopId:int64; movementFamilyId:string; mode:string; lineId:string;role:string
    resolution:string; candidateId:string option; representativeCandidateId:string option
    corridorFaceId:string option
    score:float option; margin:float option; unsafePhysical:bool
    latitude:float option;longitude:float option
}

type internal ReplayJointCandidate = {
    candidateId:string;corridorFaceId:string option;score:float;geometry:float;margin:float
    latitude:float option;longitude:float option
}

type internal ReplayPolicyCounters = {
    sameStopBlocks:int;distinctPairChoices:int;unresolvedBlockEdges:int
}

type internal ReplayEvidenceStopSummary = {
    rowCount:int; usableRowCount:int; routePointCount:int; distinctFaces:int
}

type internal ReplayExpectation = {
    stopId:int64;stopName:string;description:string;minimumDistinctFaces:int
    minimumEvidenceFaces:int;allowPhysicalUnderTiedCorridors:bool
    allowCentroidFallback:bool;mode:string option;lineId:string option
    role:string option;expectedResolution:string option;targetObservationIds:string array
    targetLatitude:float option;targetLongitude:float option;coordinateToleranceMetres:float
}

type internal ReplayContextColumns = {
    ids:string array;stops:string array;modes:string array;lines:string array
    distinctions:int array;directions:int array;patterns:string array;positions:int array
    roles:string array;families:string array;previousStops:string array;nextStops:string array
    assignmentKinds:string array;authoredKeys:string array;blockIds:string array
}

type internal ReplayVariantColumns = {
    ids:string array;ranks:int array;relativeCostMetres:Nullable<float> array
    relativeCostFractions:Nullable<float> array;corridorIds:string array option
    ingress:string array option;egress:string array option;availability:string array;failures:string array
    serviceEdges:int array
}

type internal ReplayAttachmentColumns = {
    ids:string array;ranks:int array;candidates:string array
    routedExcess:Nullable<float> array;snapEdges:Nullable<int> array option
    snapFractions:Nullable<float> array option;faces:string array
    corridorDistances:Nullable<float> array;lateralOffsets:Nullable<float> array
    corridorHeadings:Nullable<float> array;attachmentHeadings:Nullable<float> array
    availability:string array;failures:string array
}

/// Read the three routing relations as one ordered join.  Publication does not
/// need diagnostic-only identifiers, so it can avoid decoding them entirely.
let internal replayScoreRows includeDiagnostics evidencePath (selectedStops:Set<int64> option) =
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
                        failures=strings count group fields "invariant_failure_reason"
                        serviceEdges=ints count group fields "service_edge_count" }
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
                    invariantFailureReason=optionalString variants.failures variantRow
                    serviceEdgeCount=variants.serviceEdges.[variantRow] }
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
                            topologyFailureReason=failure
                            serviceEdgeCount=corridor.serviceEdgeCount }
                variantIndex<-variantIndex+1
        if ensureVariant() then
            invalidArg "evidencePath" "Corridor evidence references an unknown context"
        if ensureAttachment() then
            invalidArg "evidencePath" "Route-point evidence references an unknown context or corridor variant"
    }

let internal readReplayRoutePoints evidencePath (selectedStops:Set<int64> option) =
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

/// Source class of an observation: "osm", or "external:<catalogue file>".
/// Each source maps a physical post at most once (learned-scorer clustering).
let internal observationSource (observationId:string) =
    if isNull observationId then "?"
    elif observationId.StartsWith("osm:",StringComparison.Ordinal) then "osm"
    elif observationId.StartsWith("external:",StringComparison.Ordinal) then
        let parts=observationId.Split(':')
        if parts.Length>1 then "external:"+parts.[1] else "external:"
    else observationId.Split(':').[0]

type internal ObservationFacts = {
    tags:Dictionary<string,struct(string*bool)>   // observation id -> local_ref (or null), public_transport=platform
    stopLevelSources:HashSet<string>
}

[<Literal>]
let internal StopLevelMaximumShare = 0.5

[<Literal>]
let internal StopLevelMinimumStops = 20

/// Observation tags and stop-level source granularity for the learned scorer
/// (mirrors post_scorer.features.source_granularity and osm_facts.observation_tags).
/// A stop-level source publishes one point per stop: at stops where OSM maps two
/// or more posts it almost never has two points itself.
let internal readObservationFacts evidencePath =
    use stream=File.OpenRead(Path.Combine(evidencePath,"observations.parquet"))
    let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
    try
        let fields=reader.Schema.DataFields |> Array.map(fun field -> field.Name,field) |> Map.ofArray
        let tags=Dictionary<string,struct(string*bool)>(StringComparer.Ordinal)
        let pointsByStopSource=Dictionary<struct(string*string),HashSet<string>>()
        for groupIndex=0 to reader.RowGroupCount-1 do
            use group=reader.OpenRowGroupReader(groupIndex)
            let count=int group.RowCount
            let read name =
                let values=Array.zeroCreate<string> count
                group.ReadAsync(fields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
                     .AsTask().GetAwaiter().GetResult()
                values
            let stops=read "gtfs_stop_place_id"
            let points=read "route_point_id"
            let observations=read "observation_id"
            let rawTags=read "raw_tags"
            for index=0 to count-1 do
                let mutable localRef:string=null
                let mutable platform=false
                if not(String.IsNullOrEmpty rawTags.[index]) then
                    for pair in rawTags.[index].Split(';') do
                        let separator=pair.IndexOf('=')
                        if separator>0 then
                            let key=pair.Substring(0,separator)
                            let value=pair.Substring(separator+1)
                            if key="local_ref" && isNull localRef then localRef<-value
                            elif key="public_transport" && value="platform" then platform<-true
                tags.[observations.[index]]<-struct(localRef,platform)
                let key=struct(stops.[index],observationSource observations.[index])
                match pointsByStopSource.TryGetValue key with
                | true,set -> set.Add(points.[index]) |> ignore
                | _ -> pointsByStopSource.[key]<-HashSet<string>([points.[index]],StringComparer.Ordinal)
        let multiPostStops =
            pointsByStopSource
            |> Seq.choose(fun pair ->
                let struct(stop,source)=pair.Key
                if source="osm" && pair.Value.Count>=2 then Some stop else None)
            |> HashSet
        let stopLevel=HashSet<string>(StringComparer.Ordinal)
        pointsByStopSource
        |> Seq.filter(fun pair -> let struct(stop,_)=pair.Key in multiPostStops.Contains stop)
        |> Seq.groupBy(fun pair -> let struct(_,source)=pair.Key in source)
        |> Seq.iter(fun (source,values) ->
            let values=values |> Seq.toArray
            let share=(values |> Array.filter(fun pair -> pair.Value.Count>=2) |> Array.length |> float)/float values.Length
            if values.Length>=StopLevelMinimumStops && share<StopLevelMaximumShare then stopLevel.Add(source) |> ignore)
        { tags=tags;stopLevelSources=stopLevel }
    finally
        reader.DisposeAsync().AsTask().GetAwaiter().GetResult()

let internal medianFloat values =
    let ordered=values |> Seq.sort |> Seq.toArray
    if ordered.Length=0 then 0.0
    elif ordered.Length%2=1 then ordered.[ordered.Length/2]
    else (ordered.[ordered.Length/2-1]+ordered.[ordered.Length/2])/2.0

let internal validateReplayEvidenceSemantics evidencePath
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
