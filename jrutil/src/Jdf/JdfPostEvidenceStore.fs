// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfPostEvidenceStore

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open System.Threading
open Parquet
open Parquet.Schema

type EvidenceIdentityExpectation = {
    mergedJdfSha256:string option
    routingPbfSha256:string option
    captureToolVersion:string option
}

let noIdentityExpectation = {
    mergedJdfSha256=None;routingPbfSha256=None;captureToolVersion=None
}

type PostEvidenceStore private (directory:string,
                                manifest:JdfPostInference.PostInferenceEvidenceManifest) =
    member _.Directory = directory
    member _.Manifest = manifest
    member _.RelationPath(name:string) = Path.Combine(directory,name)
    member _.RelationMetrics =
        manifest.files |> Array.map(fun value -> value.path,value.rows,value.bytes) |> Array.copy
    member _.ReadStops() = seq {
        use stream=File.OpenRead(Path.Combine(directory,"route_points.parquet"))
        let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let field=reader.Schema.DataFields |> Array.find(fun value -> value.Name="gtfs_stop_place_id")
            let mutable previous:string=null
            for groupIndex=0 to reader.RowGroupCount-1 do
                use group=reader.OpenRowGroupReader(groupIndex)
                let values=Array.zeroCreate<string>(int group.RowCount)
                group.ReadAsync(field,values.AsMemory(),Nullable(),CancellationToken.None)
                     .AsTask().GetAwaiter().GetResult()
                for value in values do
                    if value<>previous then
                        previous<-value
                        yield Int64.Parse(value.Substring(value.LastIndexOf(':')+1),
                                          Globalization.CultureInfo.InvariantCulture)
        finally reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
    }
    interface IDisposable with member _.Dispose() = ()
    static member internal Create(directory,manifest) = new PostEvidenceStore(directory,manifest)

type private ExpectedField = { name:string;clrType:Type;nullable:bool }

type private ContextFact = {
    ordinal:int;stopId:int64;mode:string;lineId:string;routeDistinction:int
    patternHash:string;patternPosition:int;role:string;movementFamilyId:string
    assignmentKind:string;authoredPostKey:string option;sameStopBlockId:string option
}

type private VariantFact = {
    stopId:int64;rank:int;corridorId:string option;availability:string
    ingressThreadId:string option;egressThreadId:string option
}

type private ContextColumns = {
    ids:string array;stops:string array;modes:string array;lines:string array
    distinctions:int array;directions:int array;patterns:string array;positions:int array
    roles:string array;families:string array;previousStops:string array;nextStops:string array
    assignments:string array;authoredKeys:string array;blockIds:string array
}

type private VariantColumns = {
    ids:string array;stops:string array;ranks:int array;corridorIds:string array
    absolute:Nullable<float> array;relative:Nullable<float> array
    fractions:Nullable<float> array;lengths:Nullable<float> array
    directed:int array;repeated:int array;service:int array;restricted:int array
    penalties:Nullable<float> array;ingress:string array;egress:string array
    availability:string array;failures:string array
}

type private AttachmentColumns = {
    ids:string array;stops:string array;ranks:int array;corridorIds:string array
    pointIds:string array;numeric:Nullable<float> array array
    snapEdges:Nullable<int> array;snapWays:Nullable<int64> array;faces:string array
    availability:string array;failures:string array
}

let private expected name clr nullable = { name=name;clrType=clr;nullable=nullable }

let private schemas = Map [
    "observations.parquet", [|
        expected "gtfs_stop_place_id" typeof<string> false;expected "route_point_id" typeof<string> false
        expected "observation_id" typeof<string> false;expected "source_kind" typeof<string> false
        expected "source_object_id" typeof<string> true;expected "observed_at" typeof<string> true
        expected "latitude" typeof<double> false;expected "longitude" typeof<double> false
        expected "support_weight" typeof<double> false;expected "raw_tags" typeof<string> false
        expected "explicit_modes" typeof<string> false;expected "denied_modes" typeof<string> false
        expected "lifecycle" typeof<string> false |]
    "route_points.parquet", [|
        expected "gtfs_stop_place_id" typeof<string> false;expected "route_point_id" typeof<string> false
        expected "representative_observation_id" typeof<string> false;expected "observation_ids" typeof<string> false
        expected "latitude" typeof<double> false;expected "longitude" typeof<double> false
        expected "has_current_lifecycle" typeof<bool> false;expected "has_obsolete_lifecycle" typeof<bool> false
        expected "source_support_weight" typeof<double> false;expected "explicit_modes" typeof<string> false
        expected "denied_modes" typeof<string> false |]
    "contexts.parquet", [|
        expected "context_id" typeof<string> false;expected "gtfs_stop_place_id" typeof<string> false
        expected "mode" typeof<string> false;expected "line_id" typeof<string> false
        expected "route_distinction" typeof<int> false;expected "direction" typeof<int> false
        expected "pattern_hash" typeof<string> false;expected "pattern_position" typeof<int> false
        expected "same_stop_block_role" typeof<string> false;expected "movement_family_id" typeof<string> false
        expected "context_previous_stop_id" typeof<string> true;expected "context_next_stop_id" typeof<string> true
        expected "assignment_kind" typeof<string> false;expected "authored_post_key" typeof<string> true
        expected "same_stop_block_id" typeof<string> true |]
    "corridor_variants.parquet", [|
        expected "context_id" typeof<string> false;expected "gtfs_stop_place_id" typeof<string> false
        expected "variant_rank" typeof<int> false;expected "corridor_id" typeof<string> true
        expected "absolute_cost_metres" typeof<double> true;expected "relative_cost_metres" typeof<double> true
        expected "relative_cost_fraction" typeof<double> true;expected "path_length_metres" typeof<double> true
        expected "directed_edge_count" typeof<int> false;expected "repeated_directed_edge_count" typeof<int> false
        expected "service_edge_count" typeof<int> false;expected "restricted_access_edge_count" typeof<int> false
        expected "access_penalty_metres" typeof<double> true;expected "ingress_thread_id" typeof<string> true
        expected "egress_thread_id" typeof<string> true;expected "routing_availability" typeof<string> false
        expected "invariant_failure_reason" typeof<string> true |]
    "route_point_evidence.parquet", [|
        expected "context_id" typeof<string> false;expected "gtfs_stop_place_id" typeof<string> false
        expected "variant_rank" typeof<int> false;expected "corridor_id" typeof<string> true
        expected "route_point_id" typeof<string> false;expected "routed_excess_metres" typeof<double> true
        expected "snap_edge_id" typeof<int> true;expected "snap_way_id" typeof<int64> true
        expected "snap_fraction" typeof<double> true;expected "snap_projected_longitude" typeof<double> true
        expected "snap_projected_latitude" typeof<double> true;expected "snap_distance_metres" typeof<double> true
        expected "corridor_face_id" typeof<string> true;expected "corridor_distance_metres" typeof<double> true
        expected "signed_lateral_offset_metres" typeof<double> true;expected "corridor_heading_degrees" typeof<double> true
        expected "attachment_heading_degrees" typeof<double> true;expected "heading_difference_degrees" typeof<double> true
        expected "proximity_distance_metres" typeof<double> true;expected "routing_availability" typeof<string> false
        expected "invariant_failure_reason" typeof<string> true |]
]

let private sha256File path =
    use stream=File.OpenRead(path)
    SHA256.HashData(stream) |> Convert.ToHexString |> _.ToLowerInvariant()

let private schemaFingerprint (fields:DataField array) =
    fields |> Array.map(fun field -> $"{field.Name}:{field.ClrType.FullName}:{field.IsNullable}")
    |> String.concat "|" |> Encoding.UTF8.GetBytes |> SHA256.HashData
    |> Convert.ToHexString |> _.ToLowerInvariant()

let private metadataValue (metadata:IReadOnlyDictionary<string,string>) key =
    match metadata.TryGetValue key with true,value -> value | _ -> null

let private validateParquet directory
                            (manifest:JdfPostInference.PostInferenceEvidenceManifest) fileName =
    let entry=manifest.files |> Array.find(fun value -> value.path=fileName)
    let path=Path.Combine(directory,fileName)
    use stream=File.OpenRead(path)
    let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
    try
        let actual=reader.Schema.DataFields
        let expectedFields=schemas.[fileName]
        if actual.Length<>expectedFields.Length
           || Array.exists2(fun (left:DataField) right ->
                left.Name<>right.name || left.ClrType<>right.clrType || left.IsNullable<>right.nullable)
                actual expectedFields then
            invalidArg "evidenceDirectory" $"Evidence schema type/nullability mismatch: {fileName}"
        let metadata=reader.CustomMetadata
        if isNull metadata
           || metadataValue metadata "obehy.evidence_format"<>JdfPostInference.EvidenceFormat
           || metadataValue metadata "obehy.evidence_schema_version"<>string JdfPostInference.EvidenceSchemaVersion
           || metadataValue metadata "obehy.router_evidence_version"<>manifest.routerEvidenceVersion
           || metadataValue metadata "obehy.variant_enumeration_version"<>manifest.variantEnumerationVersion
           || metadataValue metadata "obehy.capture_tool_version"<>manifest.captureToolVersion
           || metadataValue metadata "obehy.pack_id"<>manifest.packId
           || metadataValue metadata "obehy.relation"<>fileName
           || String.IsNullOrWhiteSpace(metadataValue metadata "obehy.source_id")
           || metadataValue metadata "obehy.snapshot_id" <> $"sha256:{manifest.mergedJdfSha256}"
           || metadataValue metadata "obehy.routing_pbf_sha256"<>manifest.routingPbfSha256
           || metadataValue metadata "obehy.capture_routed_excess_metres"
              <>manifest.captureCeilings.routedExcessMetres.ToString(Globalization.CultureInfo.InvariantCulture)
           || metadataValue metadata "obehy.capture_maximum_corridor_variants"
              <>string manifest.captureCeilings.maximumCorridorVariants
           || metadataValue metadata "obehy.maximum_search_states"<>string manifest.maximumSearchStates
           || metadataValue metadata "obehy.maximum_search_distance_metres"
              <>manifest.maximumSearchDistanceMetres.ToString(Globalization.CultureInfo.InvariantCulture) then
            invalidArg "evidenceDirectory" $"Evidence Parquet metadata mismatch: {fileName}"
        if schemaFingerprint actual<>entry.schemaFingerprint then
            invalidArg "evidenceDirectory" $"Evidence schema fingerprint mismatch: {fileName}"
        let mutable rows=0L
        for index=0 to reader.RowGroupCount-1 do
            use group=reader.OpenRowGroupReader(index)
            rows<-rows+group.RowCount
        if rows<>entry.rows then
            invalidArg "evidenceDirectory" $"Evidence physical row count mismatch: {fileName}"
    finally reader.DisposeAsync().AsTask().GetAwaiter().GetResult()

let private stableId prefix (parts:obj seq) =
    parts |> Seq.map string |> String.concat "|" |> Encoding.UTF8.GetBytes
    |> SHA256.HashData |> Convert.ToHexString
    |> fun value -> prefix+value.ToLowerInvariant()

let private stopRegex=Regex("(?:jdf|cis):stop:(?<id>[0-9]+)",RegexOptions.CultureInvariant)

let private parseStop value =
    let matched=stopRegex.Match(value)
    if not matched.Success || matched.Length<>value.Length then
        invalidArg "evidenceDirectory" $"Invalid evidence stop identity: {value}"
    Int64.Parse(matched.Groups.["id"].Value,Globalization.CultureInfo.InvariantCulture)

let private validateSemantics directory
                              (manifest:JdfPostInference.PostInferenceEvidenceManifest) =
    let openReader name =
        let stream=File.OpenRead(Path.Combine(directory,name))
        let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        stream,reader
    let fields (reader:ParquetReader) =
        reader.Schema.DataFields |> Array.map(fun field -> field.Name,field) |> Map.ofArray
    let strings count (group:ParquetRowGroupReader) (relationFields:Map<string,DataField>) name =
        let values=Array.zeroCreate<string> count
        group.ReadAsync(relationFields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let ints count (group:ParquetRowGroupReader) (relationFields:Map<string,DataField>) name =
        let values=Array.zeroCreate<int> count
        group.ReadAsync<int>(relationFields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let doubles count (group:ParquetRowGroupReader) (relationFields:Map<string,DataField>) name =
        let values=Array.zeroCreate<float> count
        group.ReadAsync<float>(relationFields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let nullableDoubles count (group:ParquetRowGroupReader) (relationFields:Map<string,DataField>) name =
        let values=Array.zeroCreate<Nullable<float>> count
        group.ReadAsync<float>(relationFields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let nullableInts count (group:ParquetRowGroupReader) (relationFields:Map<string,DataField>) name =
        let values=Array.zeroCreate<Nullable<int>> count
        group.ReadAsync<int>(relationFields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let nullableInt64s count (group:ParquetRowGroupReader) (relationFields:Map<string,DataField>) name =
        let values=Array.zeroCreate<Nullable<int64>> count
        group.ReadAsync<int64>(relationFields.[name],values.AsMemory(),Nullable(),CancellationToken.None)
             .AsTask().GetAwaiter().GetResult()
        values
    let optionString (values:string array) index=Option.ofObj values.[index]
    let optionDouble (values:Nullable<float> array) index =
        if values.[index].HasValue then Some values.[index].Value else None
    let finite value=Double.IsFinite value

    let observations=Dictionary<struct(int64*string),string>()
    let observationMembership=Dictionary<struct(int64*string),HashSet<string>>()
    let mutable lastObservation:struct(int64*string) option=None
    let observationStream,observationReader=openReader "observations.parquet"
    try
        let relationFields=fields observationReader
        for groupIndex=0 to observationReader.RowGroupCount-1 do
            use group=observationReader.OpenRowGroupReader(groupIndex)
            let count=int group.RowCount
            let stops=strings count group relationFields "gtfs_stop_place_id"
            let pointIds=strings count group relationFields "route_point_id"
            let observationIds=strings count group relationFields "observation_id"
            let latitudes=doubles count group relationFields "latitude"
            let longitudes=doubles count group relationFields "longitude"
            let support=doubles count group relationFields "support_weight"
            for index=0 to count-1 do
                let stopId=parseStop stops.[index]
                let key=struct(stopId,observationIds.[index])
                if lastObservation |> Option.exists(fun value -> value>=key) then
                    invalidArg "evidenceDirectory" "Evidence observations are not uniquely stop-major ordered"
                lastObservation<-Some key
                if not(finite latitudes.[index] && latitudes.[index]>= -90.0 && latitudes.[index]<=90.0
                       && finite longitudes.[index] && longitudes.[index]>= -180.0 && longitudes.[index]<=180.0
                       && finite support.[index] && support.[index]>=0.0) then
                    invalidArg "evidenceDirectory" "Evidence observation contains invalid numeric facts"
                observations.Add(key,pointIds.[index])
                let membershipKey=struct(stopId,pointIds.[index])
                match observationMembership.TryGetValue membershipKey with
                | true,values -> values.Add(observationIds.[index]) |> ignore
                | _ -> observationMembership.Add(membershipKey,HashSet<string>([|observationIds.[index]|],StringComparer.Ordinal))
    finally
        observationReader.DisposeAsync().AsTask().GetAwaiter().GetResult()
        observationStream.Dispose()

    let pointsByStop=Dictionary<int64,ResizeArray<string>>()
    let pointIds=HashSet<struct(int64*string)>()
    let mutable lastPoint:struct(int64*string) option=None
    let pointStream,pointReader=openReader "route_points.parquet"
    try
        let relationFields=fields pointReader
        for groupIndex=0 to pointReader.RowGroupCount-1 do
            use group=pointReader.OpenRowGroupReader(groupIndex)
            let count=int group.RowCount
            let stops=strings count group relationFields "gtfs_stop_place_id"
            let ids=strings count group relationFields "route_point_id"
            let representatives=strings count group relationFields "representative_observation_id"
            let memberTexts=strings count group relationFields "observation_ids"
            let latitudes=doubles count group relationFields "latitude"
            let longitudes=doubles count group relationFields "longitude"
            let support=doubles count group relationFields "source_support_weight"
            for index=0 to count-1 do
                let stopId=parseStop stops.[index]
                let key=struct(stopId,ids.[index])
                if lastPoint |> Option.exists(fun value -> value>=key) then
                    invalidArg "evidenceDirectory" "Evidence route points are not uniquely stop-major ordered"
                lastPoint<-Some key
                let members=memberTexts.[index].Split(';',StringSplitOptions.RemoveEmptyEntries)
                let uniqueMembers=members |> Array.distinct |> Array.sort
                let expectedMembers =
                    match observationMembership.TryGetValue key with
                    | true,values -> values |> Seq.sort |> Seq.toArray | _ -> [||]
                if members.Length=0 || members<>uniqueMembers || members<>expectedMembers
                   || not(Array.contains representatives.[index] members)
                   || (members |> Array.exists(fun observationId ->
                        match observations.TryGetValue(struct(stopId,observationId)) with
                        | true,pointId -> pointId<>ids.[index] | _ -> true)) then
                    invalidArg "evidenceDirectory" "Evidence route-point observation membership is inconsistent"
                if not(finite latitudes.[index] && latitudes.[index]>= -90.0 && latitudes.[index]<=90.0
                       && finite longitudes.[index] && longitudes.[index]>= -180.0 && longitudes.[index]<=180.0
                       && finite support.[index] && support.[index]>=0.0) then
                    invalidArg "evidenceDirectory" "Evidence route point contains invalid numeric facts"
                pointIds.Add(key) |> ignore
                match pointsByStop.TryGetValue stopId with
                | true,values -> values.Add(ids.[index])
                | _ -> pointsByStop.Add(stopId,ResizeArray([|ids.[index]|]))
    finally
        pointReader.DisposeAsync().AsTask().GetAwaiter().GetResult()
        pointStream.Dispose()
    if observationMembership.Keys |> Seq.exists(pointIds.Contains >> not) then
        invalidArg "evidenceDirectory" "Evidence observation references an unknown route point"

    // These three relations are already written in the same context-major order.  Walk
    // them together so validation retains only one Parquet row group and one stop's
    // block facts, instead of millions of contexts, variants, and attachment sets.
    let contextGroups = seq {
        let stream,reader=openReader "contexts.parquet"
        try
            let relationFields=fields reader
            for groupIndex=0 to reader.RowGroupCount-1 do
                use group=reader.OpenRowGroupReader(groupIndex)
                let count=int group.RowCount
                yield { ids=strings count group relationFields "context_id"
                        stops=strings count group relationFields "gtfs_stop_place_id"
                        modes=strings count group relationFields "mode"
                        lines=strings count group relationFields "line_id"
                        distinctions=ints count group relationFields "route_distinction"
                        directions=ints count group relationFields "direction"
                        patterns=strings count group relationFields "pattern_hash"
                        positions=ints count group relationFields "pattern_position"
                        roles=strings count group relationFields "same_stop_block_role"
                        families=strings count group relationFields "movement_family_id"
                        previousStops=strings count group relationFields "context_previous_stop_id"
                        nextStops=strings count group relationFields "context_next_stop_id"
                        assignments=strings count group relationFields "assignment_kind"
                        authoredKeys=strings count group relationFields "authored_post_key"
                        blockIds=strings count group relationFields "same_stop_block_id" }
        finally
            reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
            stream.Dispose()
    }
    let variantGroups = seq {
        let stream,reader=openReader "corridor_variants.parquet"
        try
            let relationFields=fields reader
            for groupIndex=0 to reader.RowGroupCount-1 do
                use group=reader.OpenRowGroupReader(groupIndex)
                let count=int group.RowCount
                yield { ids=strings count group relationFields "context_id"
                        stops=strings count group relationFields "gtfs_stop_place_id"
                        ranks=ints count group relationFields "variant_rank"
                        corridorIds=strings count group relationFields "corridor_id"
                        absolute=nullableDoubles count group relationFields "absolute_cost_metres"
                        relative=nullableDoubles count group relationFields "relative_cost_metres"
                        fractions=nullableDoubles count group relationFields "relative_cost_fraction"
                        lengths=nullableDoubles count group relationFields "path_length_metres"
                        directed=ints count group relationFields "directed_edge_count"
                        repeated=ints count group relationFields "repeated_directed_edge_count"
                        service=ints count group relationFields "service_edge_count"
                        restricted=ints count group relationFields "restricted_access_edge_count"
                        penalties=nullableDoubles count group relationFields "access_penalty_metres"
                        ingress=strings count group relationFields "ingress_thread_id"
                        egress=strings count group relationFields "egress_thread_id"
                        availability=strings count group relationFields "routing_availability"
                        failures=strings count group relationFields "invariant_failure_reason" }
        finally
            reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
            stream.Dispose()
    }
    let attachmentGroups = seq {
        let stream,reader=openReader "route_point_evidence.parquet"
        try
            let relationFields=fields reader
            let numericNames=[|"routed_excess_metres";"snap_fraction";"snap_projected_longitude"
                               "snap_projected_latitude";"snap_distance_metres";"corridor_distance_metres"
                               "signed_lateral_offset_metres";"corridor_heading_degrees"
                               "attachment_heading_degrees";"heading_difference_degrees"
                               "proximity_distance_metres"|]
            for groupIndex=0 to reader.RowGroupCount-1 do
                use group=reader.OpenRowGroupReader(groupIndex)
                let count=int group.RowCount
                yield { ids=strings count group relationFields "context_id"
                        stops=strings count group relationFields "gtfs_stop_place_id"
                        ranks=ints count group relationFields "variant_rank"
                        corridorIds=strings count group relationFields "corridor_id"
                        pointIds=strings count group relationFields "route_point_id"
                        numeric=numericNames |> Array.map(fun name -> nullableDoubles count group relationFields name)
                        snapEdges=nullableInts count group relationFields "snap_edge_id"
                        snapWays=nullableInt64s count group relationFields "snap_way_id"
                        faces=strings count group relationFields "corridor_face_id"
                        availability=strings count group relationFields "routing_availability"
                        failures=strings count group relationFields "invariant_failure_reason" }
        finally
            reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
            stream.Dispose()
    }

    use contextEnumerator=contextGroups.GetEnumerator()
    use variantEnumerator=variantGroups.GetEnumerator()
    use attachmentEnumerator=attachmentGroups.GetEnumerator()
    let mutable contextColumns:ContextColumns option=None
    let mutable variantColumns:VariantColumns option=None
    let mutable attachmentColumns:AttachmentColumns option=None
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

    let blocks=Dictionary<string,ResizeArray<ContextFact>>(StringComparer.Ordinal)
    let seenBlockIds=HashSet<string>(StringComparer.Ordinal)
    let validateAndClearBlocks () =
        for pair in blocks do
            if not(seenBlockIds.Add pair.Key) then
                invalidArg "evidenceDirectory" "Evidence same-stop block identity is inconsistent"
            let members=pair.Value.ToArray()
            let first=members.[0]
            let expected=stableId "same-stop-block:" [|
                box first.lineId;box first.routeDistinction;box first.patternHash;
                box(members |> Array.minBy _.patternPosition |> _.patternPosition);
                box((members |> Array.maxBy _.patternPosition |> _.patternPosition)+1) |]
            if pair.Key<>expected || members |> Array.exists(fun value ->
                value.stopId<>first.stopId || value.lineId<>first.lineId
                || value.routeDistinction<>first.routeDistinction || value.patternHash<>first.patternHash) then
                invalidArg "evidenceDirectory" "Evidence same-stop block identity is inconsistent"
        blocks.Clear()

    let mutable lastContext:(int64*string*string*int*int*string*int*string*string*string) option=None
    let mutable currentStop:int64 option=None
    let mutable contextCount=0L
    let mutable variantCount=0L
    let mutable attachmentCount=0L
    while ensureContext() do
        let columns=contextColumns.Value
        let index=contextIndex
        contextIndex<-contextIndex+1
        let contextId=columns.ids.[index]
        let stopId=parseStop columns.stops.[index]
        if currentStop<>Some stopId then
            validateAndClearBlocks()
            currentStop<-Some stopId
        let authored=optionString columns.authoredKeys index
        let blockId=optionString columns.blockIds index
        let key=(stopId,columns.modes.[index],columns.lines.[index],columns.distinctions.[index],
                 columns.directions.[index],columns.patterns.[index],columns.positions.[index],
                 columns.roles.[index],authored |> Option.defaultValue "",contextId)
        if lastContext |> Option.exists(fun value -> value>=key) then
            invalidArg "evidenceDirectory" "Evidence contexts are not uniquely stop-major ordered"
        lastContext<-Some key
        if not(pointsByStop.ContainsKey stopId)
           || (columns.assignments.[index]<>"unlabelled" && columns.assignments.[index]<>"authored")
           || ((columns.assignments.[index]="authored")<>authored.IsSome)
           || not(Set.ofList["through";"incoming";"outgoing";"interior"] |> Set.contains columns.roles.[index])
           || ((columns.roles.[index]="through")<>blockId.IsNone) then
            invalidArg "evidenceDirectory" "Evidence context assignment or block facts are invalid"
        let previous=optionString columns.previousStops index
        let next=optionString columns.nextStops index
        previous |> Option.iter(parseStop >> ignore)
        next |> Option.iter(parseStop >> ignore)
        if (columns.roles.[index]="incoming" && next.IsSome)
           || (columns.roles.[index]="outgoing" && previous.IsSome)
           || (columns.roles.[index]="interior" && (previous.IsSome || next.IsSome)) then
            invalidArg "evidenceDirectory" "Evidence context neighbour facts conflict with its block role"
        let expectedContextId=stableId "context:" [|
            box stopId;box columns.modes.[index];box columns.lines.[index];box columns.distinctions.[index];
            box columns.directions.[index];box columns.patterns.[index];box columns.positions.[index];
            box columns.roles.[index];box(authored |> Option.defaultValue "") |]
        if contextId<>expectedContextId then
            invalidArg "evidenceDirectory" "Evidence context identity is inconsistent"
        let context={ ordinal=int contextCount;stopId=stopId;mode=columns.modes.[index]
                      lineId=columns.lines.[index];routeDistinction=columns.distinctions.[index]
                      patternHash=columns.patterns.[index];patternPosition=columns.positions.[index]
                      role=columns.roles.[index];movementFamilyId=columns.families.[index]
                      assignmentKind=columns.assignments.[index];authoredPostKey=authored
                      sameStopBlockId=blockId }
        blockId |> Option.iter(fun value ->
            match blocks.TryGetValue value with
            | true,members -> members.Add(context)
            | _ -> blocks.Add(value,ResizeArray([|context|])))

        if not(ensureVariant()) || variantColumns.Value.ids.[variantIndex]<>contextId then
            invalidArg "evidenceDirectory" "Evidence contexts are missing corridor variants"
        let mutable expectedRank=0
        let mutable baseline:VariantFact option=None
        let mutable lastCost:float option=None
        while ensureVariant() && variantColumns.Value.ids.[variantIndex]=contextId do
            let variants=variantColumns.Value
            let variantRow=variantIndex
            let rank=variants.ranks.[variantRow]
            if rank<>expectedRank || rank>=3 then
                invalidArg "evidenceDirectory" "Evidence corridor ranks are not contiguous from zero"
            let anyOptional = not(isNull variants.corridorIds.[variantRow])
                              || not(isNull variants.ingress.[variantRow])
                              || not(isNull variants.egress.[variantRow])
            let allOptional = not(isNull variants.corridorIds.[variantRow])
                              && not(isNull variants.ingress.[variantRow])
                              && not(isNull variants.egress.[variantRow])
            let numericFacts=[|variants.absolute.[variantRow];variants.relative.[variantRow]
                               variants.fractions.[variantRow];variants.lengths.[variantRow]
                               variants.penalties.[variantRow]|]
            let counts=[|variants.directed.[variantRow];variants.repeated.[variantRow]
                         variants.service.[variantRow];variants.restricted.[variantRow]|]
            if parseStop variants.stops.[variantRow]<>stopId || rank<0
               || counts |> Array.exists(fun value -> value<0)
               || counts |> Array.skip 1 |> Array.exists(fun value -> value>variants.directed.[variantRow]) then
                invalidArg "evidenceDirectory" "Evidence corridor stop, rank, or counts are invalid"
            if variants.availability.[variantRow]="unavailable" then
                if rank<>0 || anyOptional || numericFacts |> Array.exists _.HasValue
                   || counts |> Array.exists((<>)0) || isNull variants.failures.[variantRow] then
                    invalidArg "evidenceDirectory" "Evidence unavailable corridor sentinel is invalid"
            elif variants.availability.[variantRow]="available" then
                if not allOptional || numericFacts |> Array.exists(fun value -> not value.HasValue)
                   || not(isNull variants.failures.[variantRow])
                   || numericFacts |> Array.exists(fun value -> not(finite value.Value) || value.Value<0.0) then
                    invalidArg "evidenceDirectory" "Evidence available corridor facts are invalid"
                let cost=variants.absolute.[variantRow].Value
                if lastCost |> Option.exists(fun previousCost -> cost<previousCost) then
                    invalidArg "evidenceDirectory" "Evidence corridor costs are not monotonically ordered"
                lastCost<-Some cost
                if rank=0 && (variants.relative.[variantRow].Value<>0.0
                              || variants.fractions.[variantRow].Value<>0.0) then
                    invalidArg "evidenceDirectory" "Evidence baseline-relative rank-0 costs must be zero"
            else invalidArg "evidenceDirectory" "Evidence corridor availability is invalid"
            let variant={ stopId=stopId;rank=rank;corridorId=optionString variants.corridorIds variantRow
                          availability=variants.availability.[variantRow]
                          ingressThreadId=optionString variants.ingress variantRow
                          egressThreadId=optionString variants.egress variantRow }
            if rank=0 then baseline<-Some variant

            let expectedPoints=pointsByStop.[stopId]
            let mutable expectedPointIndex=0
            while ensureAttachment()
                  && attachmentColumns.Value.ids.[attachmentIndex]=contextId
                  && attachmentColumns.Value.ranks.[attachmentIndex]=rank do
                let attachments=attachmentColumns.Value
                let attachmentRow=attachmentIndex
                if expectedPointIndex>=expectedPoints.Count
                   || attachments.pointIds.[attachmentRow]<>expectedPoints.[expectedPointIndex] then
                    invalidArg "evidenceDirectory" "Evidence attachments are not uniquely context-major ordered"
                let sameCorridor =
                    match variant.corridorId with
                    | Some value -> attachments.corridorIds.[attachmentRow]=value
                    | None -> isNull attachments.corridorIds.[attachmentRow]
                if parseStop attachments.stops.[attachmentRow]<>stopId || not sameCorridor then
                    invalidArg "evidenceDirectory" "Evidence attachment foreign keys are inconsistent"
                if variant.availability="unavailable"
                   && attachments.availability.[attachmentRow]<>"unavailable" then
                    invalidArg "evidenceDirectory" "Evidence unavailable corridor has an available attachment"
                let numeric=attachments.numeric
                if attachments.availability.[attachmentRow]="unavailable" then
                    if numeric |> Array.exists(fun values -> values.[attachmentRow].HasValue)
                       || attachments.snapEdges.[attachmentRow].HasValue
                       || attachments.snapWays.[attachmentRow].HasValue
                       || not(isNull attachments.faces.[attachmentRow])
                       || isNull attachments.failures.[attachmentRow] then
                        invalidArg "evidenceDirectory" "Evidence unavailable attachment sentinel is invalid"
                elif attachments.availability.[attachmentRow]="available" then
                    if numeric |> Array.exists(fun values -> not values.[attachmentRow].HasValue)
                       || not attachments.snapEdges.[attachmentRow].HasValue
                       || not attachments.snapWays.[attachmentRow].HasValue
                       || attachments.snapEdges.[attachmentRow].Value<0
                       || attachments.snapWays.[attachmentRow].Value<0L
                       || isNull attachments.faces.[attachmentRow]
                       || not(isNull attachments.failures.[attachmentRow])
                       || numeric |> Array.exists(fun values -> not(finite values.[attachmentRow].Value)) then
                        invalidArg "evidenceDirectory" "Evidence available attachment facts are invalid"
                    if numeric.[0].[attachmentRow].Value<0.0
                       || numeric.[4].[attachmentRow].Value<0.0
                       || numeric.[5].[attachmentRow].Value<0.0
                       || numeric.[10].[attachmentRow].Value<0.0
                       || numeric.[1].[attachmentRow].Value<0.0
                       || numeric.[1].[attachmentRow].Value>1.0
                       || numeric.[2].[attachmentRow].Value< -180.0
                       || numeric.[2].[attachmentRow].Value>180.0
                       || numeric.[3].[attachmentRow].Value< -90.0
                       || numeric.[3].[attachmentRow].Value>90.0 then
                        invalidArg "evidenceDirectory" "Evidence attachment numeric ranges are invalid"
                else invalidArg "evidenceDirectory" "Evidence attachment availability is invalid"
                expectedPointIndex<-expectedPointIndex+1
                attachmentCount<-attachmentCount+1L
                attachmentIndex<-attachmentIndex+1
            if expectedPointIndex<>expectedPoints.Count then
                invalidArg "evidenceDirectory" "Evidence attachment coverage is incomplete"
            expectedRank<-expectedRank+1
            variantCount<-variantCount+1L
            variantIndex<-variantIndex+1
        match baseline with
        | None -> invalidArg "evidenceDirectory" "Evidence contexts are missing corridor variants"
        | Some value ->
            let expected=stableId "movement-family:" [|box context.stopId;box context.mode;
                box(value.ingressThreadId |> Option.defaultValue "");
                box(value.egressThreadId |> Option.defaultValue "");box context.role|]
            if expected<>context.movementFamilyId then
                invalidArg "evidenceDirectory" "Evidence movement-family identity is inconsistent"
        if ensureAttachment() && attachmentColumns.Value.ids.[attachmentIndex]=contextId then
            invalidArg "evidenceDirectory" "Evidence attachment references an unknown corridor"
        contextCount<-contextCount+1L
    validateAndClearBlocks()
    if ensureVariant() then
        invalidArg "evidenceDirectory" "Evidence corridor references an unknown context"
    if ensureAttachment() then
        invalidArg "evidenceDirectory" "Evidence attachment references an unknown corridor"
    if int64 observations.Count<>manifest.observationCount
       || int64 pointIds.Count<>manifest.routePointCount
       || contextCount<>manifest.contextCount
       || variantCount<>manifest.corridorVariantCount
       || attachmentCount<>manifest.routePointEvidenceCount then
        invalidArg "evidenceDirectory" "Evidence semantic counts do not match the manifest"

let openValidatedStore expectation evidenceDirectory =
    let directory=Path.GetFullPath(evidenceDirectory)
    let manifest=JdfPostInference.validateEvidencePack directory
    let requireIdentity message expected actual =
        match expected with
        | Some value when not(String.Equals(value,actual,StringComparison.OrdinalIgnoreCase)) ->
            invalidArg "evidenceDirectory" message
        | _ -> ()
    requireIdentity "Post-inference evidence was captured from a different merged JDF snapshot"
                    expectation.mergedJdfSha256 manifest.mergedJdfSha256
    requireIdentity "Post-inference evidence was captured from a different routing PBF snapshot"
                    expectation.routingPbfSha256 manifest.routingPbfSha256
    requireIdentity "Post-inference evidence was captured by a different capture tool version"
                    expectation.captureToolVersion manifest.captureToolVersion
    for fileName in JdfPostInference.RequiredEvidenceFiles do
        validateParquet directory manifest fileName
    validateSemantics directory manifest
    PostEvidenceStore.Create(directory,manifest)
