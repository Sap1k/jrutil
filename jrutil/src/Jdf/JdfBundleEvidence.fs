// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Post-inference evidence packs and feature export.
module JrUtil.JdfBundleEvidence

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
open JrUtil.JdfBundleModel
open JrUtil.JdfBundleTables

let internal writeTypedEvidenceParquet descriptor captureToolVersion routingPbfSha256 packId relationName
                                      path (relationFields:DataField array) (rows:seq<'T>)
                                      (writeGroup:ParquetRowGroupWriter -> DataField array -> 'T array -> unit) =
    let schema=ParquetSchema(relationFields |> Array.map(fun value -> value :> Field))
    let options=ParquetOptions(CompressionMethod=CompressionMethod.Snappy)
    let metadata=Dictionary<string,string>()
    metadata.Add("obehy.evidence_format",JdfPostInference.EvidenceFormat)
    metadata.Add("obehy.evidence_schema_version",string JdfPostInference.EvidenceSchemaVersion)
    metadata.Add("obehy.router_evidence_version",JdfPostInference.RouterEvidenceVersion)
    metadata.Add("obehy.variant_enumeration_version",JdfPostInference.VariantEnumerationVersion)
    metadata.Add("obehy.capture_tool_version",captureToolVersion)
    metadata.Add("obehy.pack_id",packId)
    metadata.Add("obehy.relation",relationName)
    metadata.Add("obehy.source_id",descriptor.sourceId)
    metadata.Add("obehy.snapshot_id",$"sha256:{descriptor.payloadSha256}")
    metadata.Add("obehy.routing_pbf_sha256",routingPbfSha256)
    metadata.Add("obehy.capture_routed_excess_metres",
                 JdfPostInference.CaptureRoutedExcessHorizonMetres.ToString(CultureInfo.InvariantCulture))
    metadata.Add("obehy.capture_maximum_corridor_variants",string JdfPostInference.CaptureMaximumCorridorVariants)
    metadata.Add("obehy.maximum_search_states",string JdfPostInference.CaptureMaximumSearchStates)
    metadata.Add("obehy.maximum_search_distance_metres",
                 JdfPostInference.CaptureMaximumSearchDistanceMetres.ToString(CultureInfo.InvariantCulture))
    use stream=File.Open(path,FileMode.CreateNew,FileAccess.Write,FileShare.None)
    let writer=ParquetWriter.CreateAsync(schema,stream,options,false,CancellationToken.None)
               |> fun operation -> operation.GetAwaiter().GetResult()
    writer.CustomMetadata<-metadata
    try
        for chunk in rows |> Seq.chunkBySize 65536 do
            use rowGroup=writer.CreateRowGroup()
            writeGroup rowGroup relationFields chunk
    finally writer.DisposeAsync().AsTask().GetAwaiter().GetResult()

let internal writeEvidenceStrings (rowGroup:ParquetRowGroupWriter) (field:DataField)
                                 (values:string array) =
    rowGroup.WriteAsync(field,values :> IReadOnlyCollection<string>,Nullable<ReadOnlyMemory<int>>())
    |> fun operation -> operation.GetAwaiter().GetResult()

let internal writeEvidenceMappedStrings (group:ParquetRowGroupWriter) (fields:DataField array)
                                       index (rows:'Row array) (mapping:'Row->string) =
    rows |> Array.map mapping |> writeEvidenceStrings group fields.[index]

let internal writeEvidenceMappedValues<'T,'Row when 'T:(new:unit->'T)
                                                   and 'T:struct and 'T :> ValueType>
                                      (group:ParquetRowGroupWriter) (fields:DataField array)
                                      index (rows:'Row array) (mapping:'Row->'T) =
    let values=rows |> Array.map mapping
    group.WriteAsync<'T>(fields.[index],ReadOnlyMemory<'T>(values),
                         Nullable<ReadOnlyMemory<int>>(),null,CancellationToken.None)
    |> fun operation -> operation.GetAwaiter().GetResult()

let internal writeEvidenceMappedNullableValues<'T,'Row when 'T:(new:unit->'T)
                                                           and 'T:struct and 'T :> ValueType>
                                              (group:ParquetRowGroupWriter) (fields:DataField array)
                                              index (rows:'Row array) (mapping:'Row->'T option) =
    let values=rows |> Array.map(mapping >> Option.toNullable)
    group.WriteAsync<'T>(fields.[index],ReadOnlyMemory<Nullable<'T>>(values),
                         Nullable<ReadOnlyMemory<int>>(),null,CancellationToken.None)
    |> fun operation -> operation.GetAwaiter().GetResult()

/// Training/review sidecar: every usable road/tram call in the capture region
/// with its evidence context ID and GTFS trip identity. `stop_occurrence`
/// numbers repeated calls at one stop within a trip from zero, so external
/// call mappings can be joined on (trip, stop, occurrence) without assuming
/// JDF and GTFS call ordinals agree.
let internal writePostContextCalls descriptor captureToolVersion routingPbfSha256 packId
                                  (restriction:JdfPostEvidence.CaptureRestriction)
                                  (batch:JdfModel.JdfBatch) path =
    let precise=
        batch.stopLocations
        |> Seq.filter(fun value -> value.precision=JdfModel.StopPrecise)
        |> Seq.groupBy _.stopId
        |> Seq.map(fun (stopId,values) ->
            let value=values |> Seq.sortBy(fun item -> item.lat,item.lon) |> Seq.head
            stopId,struct(float value.lon,float value.lat)) |> Map.ofSeq
    let inRegion stopId =
        match restriction.stopRegion with
        | None -> true
        | Some region ->
            precise |> Map.tryFind stopId
            |> Option.exists(JdfPostEvidence.captureStopRegionContains region)
    let rows = seq {
        let occurrences=Dictionary<int64,int>()
        let mutable currentTrip=None
        for call in JdfPostEvidence.contextCallsForBatch batch do
            let trip=Some(call.routeId,call.routeDistinction,call.tripId)
            if trip<>currentTrip then
                currentTrip<-trip
                occurrences.Clear()
            let occurrence=
                match occurrences.TryGetValue(call.key.stopId) with
                | true,value -> value
                | _ -> 0
            occurrences.[call.key.stopId]<-occurrence+1
            if inRegion call.key.stopId then yield call,occurrence }
    let fields:DataField array = [|
        DataField<string>("gtfs_trip_id",false); DataField<string>("route_id",false)
        DataField<int>("route_distinction",false); DataField<int64>("trip_id",false)
        DataField<int>("call_index",false); DataField<int64>("route_stop_id",false)
        DataField<int64>("stop_id",false); DataField<int>("stop_occurrence",false)
        DataField<string>("context_id",false); DataField<string>("same_stop_block_role",false)
        DataField<string>("mode",false); DataField<int>("direction",false)
        DataField<string>("authored_post_key",true) |]
    writeTypedEvidenceParquet descriptor captureToolVersion routingPbfSha256 packId
        "post_context_calls" path fields rows
        (fun group fields chunk ->
            let strings index mapping = writeEvidenceMappedStrings group fields index chunk mapping
            strings 0 (fun ((call:JdfPostEvidence.PostContextCall),_) ->
                JdfGtfsRules.jdfTripId call.routeId call.routeDistinction call.tripId)
            strings 1 (fun (call,_) -> call.routeId)
            writeEvidenceMappedValues group fields 2 chunk (fun (call,_) -> call.routeDistinction)
            writeEvidenceMappedValues group fields 3 chunk (fun (call,_) -> call.tripId)
            writeEvidenceMappedValues group fields 4 chunk (fun (call,_) -> call.callIndex)
            writeEvidenceMappedValues group fields 5 chunk (fun (call,_) -> call.call.routeStopId)
            writeEvidenceMappedValues group fields 6 chunk (fun (call,_) -> call.key.stopId)
            writeEvidenceMappedValues group fields 7 chunk (fun (_,occurrence) -> occurrence)
            strings 8 (fun (call,_) -> call.contextId)
            strings 9 (fun (call,_) -> call.key.sameStopBlockRole)
            strings 10 (fun (call,_) -> call.key.mode)
            writeEvidenceMappedValues group fields 11 chunk (fun (call,_) -> call.key.direction)
            strings 12 (fun (call,_) -> call.key.authoredPostKey |> Option.toObj))

let internal parquetSchemaFingerprint path =
    use stream=File.OpenRead(path)
    let reader=ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
    try
        reader.Schema.DataFields
        |> Array.map(fun field -> $"{field.Name}:{field.ClrType.FullName}:{field.IsNullable}")
        |> String.concat "|"
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> fun value -> value.ToLowerInvariant()
    finally
        (reader :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()

let internal evidenceFileEntry directory fileName rows : JdfPostInference.EvidenceFileManifest =
    let path=Path.Combine(directory,fileName)
    let info=FileInfo(path)
    { path=fileName
      sha256=sha256File path
      bytes=info.Length
      rows=rows
      schemaFingerprint=parquetSchemaFingerprint path }

let internal writeParquetValues<'T when 'T : (new : unit -> 'T)
                                  and 'T : struct and 'T :> ValueType>
                               (rowGroup: ParquetRowGroupWriter) field (values: 'T array) =
    rowGroup.WriteAsync<'T>(field,ReadOnlyMemory<'T>(values),
                            Nullable<ReadOnlyMemory<int>>(),null,CancellationToken.None)
    |> fun operation -> operation.GetAwaiter().GetResult()

let internal writeParquetNullableValues<'T when 'T : (new : unit -> 'T)
                                          and 'T : struct and 'T :> ValueType>
                                       (rowGroup: ParquetRowGroupWriter) field
                                       (values: Nullable<'T> array) =
    rowGroup.WriteAsync<'T>(field,ReadOnlyMemory<Nullable<'T>>(values),
                            Nullable<ReadOnlyMemory<int>>(),null,CancellationToken.None)
    |> fun operation -> operation.GetAwaiter().GetResult()

let internal writeDerivedPostAssignmentsParquet descriptor path expectedCount
                                                  (rows: unit -> seq<DerivedPostAssignmentRow>)
                                                  (progress: int64 -> int64 option -> unit) =
    let fields: DataField array = [|
        field<string> "target_gtfs_stop_id" false; field<string> "assignment_kind" false
        field<string> "derived_location_id" true; field<string> "mode" false
        field<string> "line_id" true; field<int> "direction" true
        field<string> "pattern_hash" true; field<int> "pattern_position" true
        field<string> "movement_family_id" true
        field<string> "context_previous_stop_id" true; field<string> "context_next_stop_id" true
        field<string> "same_stop_block_role" false; field<double> "score" true
        field<double> "margin" true; field<string> "selected_candidates" false
        field<string> "rejected_candidates" false; field<string> "status" false
    |]
    let schema=ParquetSchema(fields |> Array.map (fun value -> value :> Field))
    let options=ParquetOptions(CompressionMethod=CompressionMethod.Snappy)
    let metadata=Dictionary<string,string>()
    metadata.Add("obehy.bundle_version",string BundleVersion)
    metadata.Add("obehy.schema_version",string ParquetSchemaVersion)
    metadata.Add("obehy.source_id",descriptor.sourceId)
    metadata.Add("obehy.snapshot_id",$"sha256:{descriptor.payloadSha256}")
    use stream=File.Open(path,FileMode.CreateNew,FileAccess.Write,FileShare.None)
    let writer=
        ParquetWriter.CreateAsync(schema,stream,options,false,CancellationToken.None)
        |> fun operation -> operation.GetAwaiter().GetResult()
    writer.CustomMetadata <- metadata
    let writeStrings (rowGroup: ParquetRowGroupWriter) fieldIndex
                     (mapping: DerivedPostAssignmentRow -> string)
                     (values: DerivedPostAssignmentRow array) =
        let column=values |> Array.map mapping
        rowGroup.WriteAsync(fields.[fieldIndex],column :> IReadOnlyCollection<string>,
                            Nullable<ReadOnlyMemory<int>>())
        |> fun operation -> operation.GetAwaiter().GetResult()
    let writeNullableInts (rowGroup: ParquetRowGroupWriter) fieldIndex
                          (mapping: DerivedPostAssignmentRow -> int option)
                          (values: DerivedPostAssignmentRow array) =
        values |> Array.map (mapping >> Option.toNullable)
        |> writeParquetNullableValues<int> rowGroup fields.[fieldIndex]
    let writeNullableDoubles (rowGroup: ParquetRowGroupWriter) fieldIndex
                             (mapping: DerivedPostAssignmentRow -> double option)
                             (values: DerivedPostAssignmentRow array) =
        values |> Array.map (mapping >> Option.toNullable)
        |> writeParquetNullableValues<double> rowGroup fields.[fieldIndex]
    try
        let mutable written=0L
        for chunk in rows() |> Seq.chunkBySize 65536 do
            use rowGroup=writer.CreateRowGroup()
            writeStrings rowGroup 0 (fun value -> value.targetGtfsStopId) chunk
            writeStrings rowGroup 1 (fun value -> value.assignmentKind) chunk
            writeStrings rowGroup 2 (fun value -> value.derivedLocationId |> Option.defaultValue null) chunk
            writeStrings rowGroup 3 (fun value -> value.mode) chunk
            writeStrings rowGroup 4 (fun value -> value.lineId |> Option.defaultValue null) chunk
            writeNullableInts rowGroup 5 (fun value -> value.direction) chunk
            writeStrings rowGroup 6 (fun value -> value.patternHash |> Option.defaultValue null) chunk
            writeNullableInts rowGroup 7 (fun value -> value.patternPosition) chunk
            writeStrings rowGroup 8 (fun value -> value.movementFamilyId |> Option.defaultValue null) chunk
            writeStrings rowGroup 9 (fun value -> value.contextPreviousStopId |> Option.defaultValue null) chunk
            writeStrings rowGroup 10 (fun value -> value.contextNextStopId |> Option.defaultValue null) chunk
            writeStrings rowGroup 11 (fun value -> value.sameStopBlockRole) chunk
            writeNullableDoubles rowGroup 12 (fun value -> value.score) chunk
            writeNullableDoubles rowGroup 13 (fun value -> value.margin) chunk
            writeStrings rowGroup 14 (fun value -> value.selectedCandidates) chunk
            writeStrings rowGroup 15 (fun value -> value.rejectedCandidates) chunk
            writeStrings rowGroup 16 (fun value -> value.status) chunk
            written <- written+int64 chunk.Length
            progress written (Some expectedCount)
        if written<>expectedCount then
            failwith $"Derived post assignment count mismatch: expected {expectedCount}, wrote {written}"
        int written
    finally
        (writer :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()

let writePostEvidenceStore descriptor captureToolVersion evidencePath routingPbfPath
                           (store:JdfPostEvidence.CapturedPostEvidence) progress =
    let outputFull=Path.GetFullPath(evidencePath)
    if Directory.Exists(outputFull) || File.Exists(outputFull) then
        invalidArg "evidencePath" $"Post-inference evidence output already exists: {outputFull}"
    let parent=Path.GetDirectoryName(outputFull)
    if String.IsNullOrWhiteSpace(parent) then invalidArg "evidencePath" "Evidence output requires a parent directory"
    Directory.CreateDirectory(parent) |> ignore
    let temp=Path.Combine(parent,$".{Path.GetFileName(outputFull)}.tmp-{Guid.NewGuid():N}")
    Directory.CreateDirectory(temp) |> ignore
    let mutable activated=false
    try
        let routingHash =
            use stream=File.OpenRead(routingPbfPath)
            sha256Stream stream
        let packId=JdfPostInference.evidencePackId captureToolVersion descriptor.payloadSha256 routingHash
        let stopId stop = JdfGtfsRules.jdfStopId stop
        let write relationName fields rows writeGroup =
            writeTypedEvidenceParquet descriptor captureToolVersion routingHash packId relationName
                (Path.Combine(temp,relationName)) fields rows writeGroup

        let observationFields:DataField array=[|
            field<string> "gtfs_stop_place_id" false;field<string> "route_point_id" false
            field<string> "observation_id" false;field<string> "source_kind" false
            field<string> "source_object_id" true;field<string> "observed_at" true
            field<double> "latitude" false;field<double> "longitude" false
            field<double> "support_weight" false;field<string> "raw_tags" false
            field<string> "explicit_modes" false;field<string> "denied_modes" false
            field<string> "lifecycle" false |]
        write "observations.parquet" observationFields (store.observations.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows (fun value -> stopId value.stopId)
            writeEvidenceMappedStrings group fields 1 rows _.routePointId
            writeEvidenceMappedStrings group fields 2 rows _.observationId
            writeEvidenceMappedStrings group fields 3 rows _.sourceKind
            writeEvidenceMappedStrings group fields 4 rows (fun value -> value.sourceObjectId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 5 rows (fun value -> value.observedAt |> Option.defaultValue null)
            writeEvidenceMappedValues group fields 6 rows _.latitude
            writeEvidenceMappedValues group fields 7 rows _.longitude
            writeEvidenceMappedValues group fields 8 rows _.supportWeight
            writeEvidenceMappedStrings group fields 9 rows _.rawTags
            writeEvidenceMappedStrings group fields 10 rows _.explicitModes
            writeEvidenceMappedStrings group fields 11 rows _.deniedModes
            writeEvidenceMappedStrings group fields 12 rows _.lifecycle)
        progress "capture-evidence-observations" store.observations.Count (Some store.observations.Count)

        let routePointFields:DataField array=[|
            field<string> "gtfs_stop_place_id" false;field<string> "route_point_id" false
            field<string> "representative_observation_id" false;field<string> "observation_ids" false
            field<double> "latitude" false;field<double> "longitude" false
            field<bool> "has_current_lifecycle" false;field<bool> "has_obsolete_lifecycle" false
            field<double> "source_support_weight" false;field<string> "explicit_modes" false
            field<string> "denied_modes" false |]
        write "route_points.parquet" routePointFields (store.routePoints.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows (fun value -> stopId value.stopId)
            writeEvidenceMappedStrings group fields 1 rows _.routePointId
            writeEvidenceMappedStrings group fields 2 rows _.representativeObservationId
            writeEvidenceMappedStrings group fields 3 rows (fun value -> String.Join(";",value.observationIds))
            writeEvidenceMappedValues group fields 4 rows _.latitude
            writeEvidenceMappedValues group fields 5 rows _.longitude
            writeEvidenceMappedValues group fields 6 rows _.hasCurrentLifecycle
            writeEvidenceMappedValues group fields 7 rows _.hasObsoleteLifecycle
            writeEvidenceMappedValues group fields 8 rows _.sourceSupportWeight
            writeEvidenceMappedStrings group fields 9 rows (fun value -> String.Join(";",value.explicitModes))
            writeEvidenceMappedStrings group fields 10 rows (fun value -> String.Join(";",value.deniedModes)))
        progress "capture-evidence-route-points" store.routePoints.Count (Some store.routePoints.Count)

        let contextFields:DataField array=[|
            field<string> "context_id" false;field<string> "gtfs_stop_place_id" false
            field<string> "mode" false;field<string> "line_id" false
            field<int> "route_distinction" false;field<int> "direction" false
            field<string> "pattern_hash" false;field<int> "pattern_position" false
            field<string> "same_stop_block_role" false;field<string> "movement_family_id" false
            field<string> "context_previous_stop_id" true;field<string> "context_next_stop_id" true
            field<string> "assignment_kind" false;field<string> "authored_post_key" true
            field<string> "same_stop_block_id" true |]
        write "contexts.parquet" contextFields (store.contexts.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows _.contextId
            writeEvidenceMappedStrings group fields 1 rows (fun value -> stopId value.key.stopId)
            writeEvidenceMappedStrings group fields 2 rows (fun value -> value.key.mode)
            writeEvidenceMappedStrings group fields 3 rows (fun value -> value.key.lineId)
            writeEvidenceMappedValues group fields 4 rows (fun value -> value.key.routeDistinction)
            writeEvidenceMappedValues group fields 5 rows (fun value -> value.key.direction)
            writeEvidenceMappedStrings group fields 6 rows (fun value -> value.key.patternHash)
            writeEvidenceMappedValues group fields 7 rows (fun value -> value.key.patternPosition)
            writeEvidenceMappedStrings group fields 8 rows (fun value -> value.key.sameStopBlockRole)
            writeEvidenceMappedStrings group fields 9 rows _.movementFamilyId
            writeEvidenceMappedStrings group fields 10 rows (fun value -> value.previousStopId |> Option.map stopId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 11 rows (fun value -> value.nextStopId |> Option.map stopId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 12 rows _.assignmentKind
            writeEvidenceMappedStrings group fields 13 rows (fun value -> value.key.authoredPostKey |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 14 rows (fun value -> value.sameStopBlockId |> Option.defaultValue null))
        progress "capture-evidence-contexts" store.contexts.Count (Some store.contexts.Count)

        let corridorFields:DataField array=[|
            field<string> "context_id" false;field<string> "gtfs_stop_place_id" false
            field<int> "variant_rank" false;field<string> "corridor_id" true
            field<double> "absolute_cost_metres" true;field<double> "relative_cost_metres" true
            field<double> "relative_cost_fraction" true;field<double> "path_length_metres" true
            field<int> "directed_edge_count" false;field<int> "repeated_directed_edge_count" false
            field<int> "service_edge_count" false;field<int> "restricted_access_edge_count" false
            field<double> "access_penalty_metres" true;field<string> "ingress_thread_id" true
            field<string> "egress_thread_id" true;field<string> "routing_availability" false
            field<string> "invariant_failure_reason" true |]
        write "corridor_variants.parquet" corridorFields (store.corridorVariants.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows _.contextId
            writeEvidenceMappedStrings group fields 1 rows (fun value -> stopId value.stopId)
            writeEvidenceMappedValues group fields 2 rows (fun value -> value.variant.variantRank)
            writeEvidenceMappedStrings group fields 3 rows (fun value -> value.variant.corridorId |> Option.defaultValue null)
            writeEvidenceMappedNullableValues group fields 4 rows (fun value -> value.variant.absoluteCostMetres)
            writeEvidenceMappedNullableValues group fields 5 rows (fun value -> value.variant.relativeCostMetres)
            writeEvidenceMappedNullableValues group fields 6 rows (fun value -> value.variant.relativeCostFraction)
            writeEvidenceMappedNullableValues group fields 7 rows (fun value -> value.variant.pathLengthMetres)
            writeEvidenceMappedValues group fields 8 rows (fun value -> value.variant.directedEdgeCount)
            writeEvidenceMappedValues group fields 9 rows (fun value -> value.variant.repeatedDirectedEdgeCount)
            writeEvidenceMappedValues group fields 10 rows (fun value -> value.variant.serviceEdgeCount)
            writeEvidenceMappedValues group fields 11 rows (fun value -> value.variant.restrictedAccessEdgeCount)
            writeEvidenceMappedNullableValues group fields 12 rows (fun value -> value.variant.accessPenaltyMetres)
            writeEvidenceMappedStrings group fields 13 rows (fun value -> value.variant.ingressThreadId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 14 rows (fun value -> value.variant.egressThreadId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 15 rows (fun value -> value.variant.routingAvailability)
            writeEvidenceMappedStrings group fields 16 rows (fun value -> value.variant.invariantFailureReason |> Option.defaultValue null))
        progress "capture-evidence-corridors" store.corridorVariants.Count (Some store.corridorVariants.Count)

        let attachmentFields:DataField array=[|
            field<string> "context_id" false;field<string> "gtfs_stop_place_id" false
            field<int> "variant_rank" false;field<string> "corridor_id" true
            field<string> "route_point_id" false;field<double> "routed_excess_metres" true
            field<int> "snap_edge_id" true;field<int64> "snap_way_id" true
            field<double> "snap_fraction" true;field<double> "snap_projected_longitude" true
            field<double> "snap_projected_latitude" true;field<double> "snap_distance_metres" true
            field<string> "corridor_face_id" true;field<double> "corridor_distance_metres" true
            field<double> "signed_lateral_offset_metres" true;field<double> "corridor_heading_degrees" true
            field<double> "attachment_heading_degrees" true;field<double> "heading_difference_degrees" true
            field<double> "proximity_distance_metres" true;field<string> "routing_availability" false
            field<string> "invariant_failure_reason" true |]
        write "route_point_evidence.parquet" attachmentFields (store.routePointEvidence.ReadRows()) (fun group fields rows ->
            writeEvidenceMappedStrings group fields 0 rows _.contextId
            writeEvidenceMappedStrings group fields 1 rows (fun value -> stopId value.stopId)
            writeEvidenceMappedValues group fields 2 rows (fun value -> value.attachment.variantRank)
            writeEvidenceMappedStrings group fields 3 rows (fun value -> value.attachment.corridorId |> Option.defaultValue null)
            writeEvidenceMappedStrings group fields 4 rows _.routePointId
            writeEvidenceMappedNullableValues group fields 5 rows (fun value -> value.attachment.routedExcessMetres)
            writeEvidenceMappedNullableValues group fields 6 rows (fun value -> value.attachment.snapEdgeId)
            writeEvidenceMappedNullableValues group fields 7 rows (fun value -> value.attachment.snapWayId)
            writeEvidenceMappedNullableValues group fields 8 rows (fun value -> value.attachment.snapFraction)
            writeEvidenceMappedNullableValues group fields 9 rows (fun value -> value.attachment.snapProjectedLongitude)
            writeEvidenceMappedNullableValues group fields 10 rows (fun value -> value.attachment.snapProjectedLatitude)
            writeEvidenceMappedNullableValues group fields 11 rows (fun value -> value.attachment.snapDistanceMetres)
            writeEvidenceMappedStrings group fields 12 rows (fun value -> value.attachment.corridorFaceId |> Option.defaultValue null)
            writeEvidenceMappedNullableValues group fields 13 rows (fun value -> value.attachment.corridorDistanceMetres)
            writeEvidenceMappedNullableValues group fields 14 rows (fun value -> value.attachment.signedLateralOffsetMetres)
            writeEvidenceMappedNullableValues group fields 15 rows (fun value -> value.attachment.corridorHeadingDegrees)
            writeEvidenceMappedNullableValues group fields 16 rows (fun value -> value.attachment.attachmentHeadingDegrees)
            writeEvidenceMappedNullableValues group fields 17 rows (fun value -> value.attachment.headingDifferenceDegrees)
            writeEvidenceMappedNullableValues group fields 18 rows (fun value -> value.attachment.proximityDistanceMetres)
            writeEvidenceMappedStrings group fields 19 rows (fun value -> value.attachment.routingAvailability)
            writeEvidenceMappedStrings group fields 20 rows (fun value -> value.attachment.invariantFailureReason |> Option.defaultValue null))
        progress "capture-evidence-routing" store.routePointEvidence.Count (Some store.routePointEvidence.Count)

        let manifest:JdfPostInference.PostInferenceEvidenceManifest = {
            evidenceFormat=JdfPostInference.EvidenceFormat;schemaVersion=JdfPostInference.EvidenceSchemaVersion
            packId=packId;captureToolVersion=captureToolVersion
            mergedJdfSha256=descriptor.payloadSha256;routingPbfSha256=routingHash
            osmSnapshot=None;routerEvidenceVersion=JdfPostInference.RouterEvidenceVersion
            variantEnumerationVersion=JdfPostInference.VariantEnumerationVersion
            captureCeilings={routedExcessMetres=JdfPostInference.CaptureRoutedExcessHorizonMetres
                             maximumCorridorVariants=JdfPostInference.CaptureMaximumCorridorVariants}
            maximumSearchStates=JdfPostInference.CaptureMaximumSearchStates
            maximumSearchDistanceMetres=JdfPostInference.CaptureMaximumSearchDistanceMetres
            contextCount=store.contexts.Count;routePointCount=store.routePoints.Count
            observationCount=store.observations.Count;corridorVariantCount=store.corridorVariants.Count
            routePointEvidenceCount=store.routePointEvidence.Count
            files=[|evidenceFileEntry temp "observations.parquet" store.observations.Count
                    evidenceFileEntry temp "route_points.parquet" store.routePoints.Count
                    evidenceFileEntry temp "contexts.parquet" store.contexts.Count
                    evidenceFileEntry temp "corridor_variants.parquet" store.corridorVariants.Count
                    evidenceFileEntry temp "route_point_evidence.parquet" store.routePointEvidence.Count|] }
        JdfPostInference.writeEvidenceManifest (Path.Combine(temp,"manifest.json")) manifest
        use validated=JdfPostEvidenceStore.openValidatedStore
                          { mergedJdfSha256=Some descriptor.payloadSha256
                            routingPbfSha256=Some routingHash
                            captureToolVersion=Some captureToolVersion }
                          temp
        Directory.Move(temp,outputFull)
        activated<-true
    finally
        if not activated && Directory.Exists(temp) then Directory.Delete(temp,true)

let internal writeFeatureParquet (manifest:JdfPostInference.PostInferenceEvidenceManifest)
                                policyId relationName path
                                (fields:DataField array) (rows:seq<'T>)
                                (writeGroup:ParquetRowGroupWriter -> DataField array -> 'T array -> unit) =
    let schema=ParquetSchema(fields |> Array.map(fun value -> value :> Field))
    let options=ParquetOptions(CompressionMethod=CompressionMethod.Snappy)
    let metadata=Dictionary<string,string>()
    metadata.Add("obehy.relation",relationName)
    metadata.Add("obehy.pack_id",manifest.packId)
    metadata.Add("obehy.capture_tool_version",manifest.captureToolVersion)
    metadata.Add("obehy.evaluator_version",JdfPostInference.EvaluatorVersion)
    metadata.Add("obehy.policy_id",policyId)
    use stream=File.Open(path,FileMode.CreateNew,FileAccess.Write,FileShare.None)
    let writer=ParquetWriter.CreateAsync(schema,stream,options,false,CancellationToken.None)
               |> fun operation -> operation.GetAwaiter().GetResult()
    writer.CustomMetadata<-metadata
    try
        for chunk in rows |> Seq.chunkBySize 65536 do
            use rowGroup=writer.CreateRowGroup()
            writeGroup rowGroup fields chunk
    finally writer.DisposeAsync().AsTask().GetAwaiter().GetResult()

/// Training/analysis export: the evaluator's per-(context, candidate, variant)
/// diagnostic rows, consolidated hypotheses and one policy's decisions.
/// Raw context, variant and observation facts are read from the pack itself.
/// A v2 policy file (heuristic) or a v3 document with a learned scorer; the compiled
/// default policy with the heuristic scorer when no path is given.
let internal policyAndScorer (policyPath:string option) =
    match policyPath with
    | Some path ->
        let loaded=JdfPostInferencePolicy.loadPolicyWithScorer path
        loaded.policy,loaded.scorer
    | None -> JdfPostInferencePolicy.conservativeRoutedV4,JdfPostInferencePolicy.HeuristicScorer

let exportPostInferenceFeatures evidencePath policyPath outputPath =
    let evidenceFull=Path.GetFullPath(evidencePath)
    let outputFull=Path.GetFullPath(outputPath)
    if Directory.Exists(outputFull) || File.Exists(outputFull) then
        invalidArg "outputPath" $"Feature export output already exists: {outputFull}"
    use store=JdfPostEvidenceStore.openValidatedStore
                  JdfPostEvidenceStore.noIdentityExpectation evidenceFull
    let manifest=store.Manifest
    let policy,scorer=policyAndScorer policyPath
    use result=JdfPostInferenceEvaluator.evaluateWithScorer true store policy scorer
    let temporary=outputFull + $".tmp-{Guid.NewGuid():N}"
    Directory.CreateDirectory(temporary) |> ignore
    try
        let write name fields rows writeGroup =
            writeFeatureParquet manifest policy.policyId name (Path.Combine(temporary,name))
                fields rows writeGroup
        let text name = field<string> name true
        let number name = field<double> name true
        let integer name = field<int> name false
        let flag name = field<bool> name false
        let scoreFields = [|
            text "context_id";text "candidate_id";integer "variant_rank";flag "eligible"
            number "alignment";number "side";number "proximity";number "routed_fit"
            number "routed_excess_metres";text "corridor_id";text "ingress_thread_id"
            text "egress_thread_id";text "corridor_face_id";text "routing_availability"
            integer "alternative_corridor_count";number "alternative_cost_gap"
            flag "tied_corridors_agree";number "corridor_distance";number "signed_lateral_offset"
            number "corridor_heading";number "attachment_heading";number "snap_fraction"
            text "topology_failure_reason";number "source_adjustment";number "modality_adjustment"
            number "popularity_adjustment";number "total";text "rejection_reason" |]
        write "diagnostic_scores.parquet" scoreFields (result.DiagnosticScores.ReadRows())
            (fun group fields (rows:JdfPostInference.PostInferenceDiagnosticScore array) ->
                let strings index mapping = writeEvidenceMappedStrings group fields index rows mapping
                let optionalText index (mapping:JdfPostInference.PostInferenceDiagnosticScore -> string option) =
                    strings index (mapping >> Option.toObj)
                let values index mapping = writeEvidenceMappedNullableValues group fields index rows mapping
                strings 0 _.contextId; strings 1 _.candidateId
                writeEvidenceMappedValues group fields 2 rows _.variantRank
                writeEvidenceMappedValues group fields 3 rows _.eligible
                values 4 (fun value -> Some value.alignment); values 5 (fun value -> Some value.side)
                values 6 (fun value -> Some value.proximity); values 7 (fun value -> Some value.routedExcess)
                values 8 _.routedExcessMetres; optionalText 9 _.corridorId
                optionalText 10 _.ingressThreadId; optionalText 11 _.egressThreadId
                optionalText 12 _.corridorFaceId; strings 13 _.routingAvailability
                writeEvidenceMappedValues group fields 14 rows _.alternativeCorridorCount
                values 15 _.alternativeCostGap
                writeEvidenceMappedValues group fields 16 rows _.tiedCorridorsAgree
                values 17 _.corridorDistance; values 18 _.signedLateralOffset
                values 19 _.corridorHeading; values 20 _.attachmentHeading
                values 21 _.snapFraction; optionalText 22 _.topologyFailureReason
                values 23 (fun value -> Some value.sourceAdjustment)
                values 24 (fun value -> Some value.modalityAdjustment)
                values 25 (fun value -> Some value.popularityAdjustment)
                values 26 (fun value -> Some value.total); optionalText 27 _.rejectionReason)
        let hypothesisFields = [|
            text "hypothesis_id";field<int64> "stop_id" false
            text "representative_route_point_id";text "member_route_point_ids"
            text "member_observation_ids";number "latitude";number "longitude" |]
        write "hypotheses.parquet" hypothesisFields result.Hypotheses
            (fun group fields (rows:JdfPostInference.ConsolidatedPostHypothesis array) ->
                let strings index mapping = writeEvidenceMappedStrings group fields index rows mapping
                strings 0 _.hypothesisId
                writeEvidenceMappedValues group fields 1 rows _.stopId
                strings 2 _.representativeRoutePointId
                strings 3 (fun value -> String.Join(";",value.memberRoutePointIds))
                strings 4 (fun value -> String.Join(";",value.memberObservationIds))
                writeEvidenceMappedNullableValues group fields 5 rows (fun value -> Some value.latitude)
                writeEvidenceMappedNullableValues group fields 6 rows (fun value -> Some value.longitude))
        let assignmentFields = [|
            text "context_id";field<int64> "stop_id" false;text "mode";text "line_id"
            text "assignment_kind";text "authored_post_key";text "same_stop_block_role"
            text "movement_family_id";text "resolution";text "selected_location_id"
            text "selected_hypothesis_id";number "score";number "margin" |]
        write "assignments.parquet" assignmentFields (result.Assignments.ReadRows())
            (fun group fields (rows:JdfPostInference.ContextPostAssignment array) ->
                let strings index mapping = writeEvidenceMappedStrings group fields index rows mapping
                let optionalText index (mapping:JdfPostInference.ContextPostAssignment -> string option) =
                    strings index (mapping >> Option.toObj)
                strings 0 _.contextId
                writeEvidenceMappedValues group fields 1 rows _.stopId
                strings 2 _.mode; strings 3 _.lineId; strings 4 _.assignmentKind
                optionalText 5 _.authoredPostKey; strings 6 _.sameStopBlockRole
                strings 7 _.movementFamilyId; strings 8 _.resolution
                optionalText 9 _.selectedLocationId; optionalText 10 _.selectedHypothesisId
                writeEvidenceMappedNullableValues group fields 11 rows _.score
                writeEvidenceMappedNullableValues group fields 12 rows _.margin)
        JdfPostInferencePolicy.writePolicy (Path.Combine(temporary,"policy.json")) policy
        Directory.Move(temporary,outputFull)
    finally
        if Directory.Exists(temporary) then Directory.Delete(temporary,true)
