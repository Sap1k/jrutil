// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Bundle diagnostics and manifest.
module JrUtil.JdfBundleManifest

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

let internal diagnostics (batch: JdfModel.JdfBatch) (feed: GtfsModel.GtfsFeed)
                        (callFacts: CallDerivedFacts)
                        (emittedTransferCalls: HashSet<struct (string * int64)>) =
    let retainedTrips = HashSet<string>(feed.trips |> Seq.map (fun trip -> trip.id))
    let filteredTrips =
        batch.trips
        |> Seq.choose (fun trip ->
            let gtfsId = JdfGtfsRules.jdfTripId trip.routeId trip.routeDistinction trip.id
            if retainedTrips.Contains gtfsId then None else Some {
                severity = "warning"; code = "filtered_trip"; sourceObjectId = gtfsId
                message = "Trip has no retained service dates and was omitted"
            })
    let filteredEnrichments =
        let serviceNotes =
            batch.serviceNotes
            |> Seq.choose (fun note ->
                let hasText = nonEmptyText note.note |> Option.isSome
                let isUnhandled = note.noteType.IsNone && not (String.IsNullOrWhiteSpace(note.designation))
                let gtfsTripId = JdfGtfsRules.jdfTripId note.routeId note.routeDistinction note.tripId
                if (hasText || isUnhandled) && not (retainedTrips.Contains gtfsTripId) then
                    Some (tripNoticeId note.routeId note.routeDistinction note.tripId note.id)
                else None)
        let reservations =
            batch.reservationOptions
            |> withOwnerOrdinals (fun note -> note.routeId, note.routeDistinction, note.tripId)
            |> Seq.choose (fun (ordinal, note) ->
                let gtfsTripId = JdfGtfsRules.jdfTripId note.routeId note.routeDistinction note.tripId
                if not (retainedTrips.Contains gtfsTripId) then
                    Some (reservationNoticeId note.routeId note.routeDistinction note.tripId ordinal)
                else None)
        let transfers =
            batch.transfers
            |> withOwnerOrdinals (fun transfer -> transfer.routeId, transfer.routeDistinction, transfer.tripId)
            |> Seq.choose (fun (ordinal, transfer) ->
                let gtfsTripId =
                    JdfGtfsRules.jdfTripId transfer.routeId transfer.routeDistinction transfer.tripId
                if not (retainedTrips.Contains gtfsTripId) then
                    Some (transferId transfer.routeId transfer.routeDistinction transfer.tripId ordinal)
                else None)
        let restrictions = callFacts.filteredRestrictionIds
        Seq.concat [serviceNotes; reservations; transfers; restrictions]
        |> Seq.map (fun sourceObjectId -> {
            severity = "warning"; code = "filtered_enrichment"
            sourceObjectId = sourceObjectId
            message = "Enrichment belongs to a trip omitted from GTFS"
        })
    let unjoinableCallEnrichments =
        let transfers =
            batch.transfers
            |> withOwnerOrdinals (fun transfer -> transfer.routeId, transfer.routeDistinction, transfer.tripId)
            |> Seq.choose (fun (ordinal, transfer) ->
                let gtfsTripId =
                    JdfGtfsRules.jdfTripId transfer.routeId transfer.routeDistinction transfer.tripId
                if retainedTrips.Contains gtfsTripId
                   && not (emittedTransferCalls.Contains(struct (gtfsTripId, transfer.routeStopId))) then
                    Some (transferId transfer.routeId transfer.routeDistinction transfer.tripId ordinal)
                else None)
        let restrictions = callFacts.unjoinableRestrictionIds
        Seq.append transfers restrictions
        |> Seq.map (fun sourceObjectId -> {
            severity = "warning"; code = "unjoinable_call_enrichment"
            sourceObjectId = sourceObjectId
            message = "Call-scoped enrichment cannot join an emitted GTFS call"
        })
    let blankNotices =
        let routeNotices =
            batch.routeInfo
            |> Seq.filter (fun note -> String.IsNullOrWhiteSpace(note.text))
            |> Seq.map (fun note -> routeNoticeId note.routeId note.routeDistinction note.id)
        let serviceNotices =
            batch.serviceNotes
            |> Seq.filter (fun note -> note.noteType.IsNone
                                      && String.IsNullOrWhiteSpace(note.designation)
                                      && nonEmptyText note.note |> Option.isNone)
            |> Seq.map (fun note -> tripNoticeId note.routeId note.routeDistinction note.tripId note.id)
        let reservations =
            batch.reservationOptions
            |> withOwnerOrdinals (fun note -> note.routeId, note.routeDistinction, note.tripId)
            |> Seq.filter (fun (_, note) -> String.IsNullOrWhiteSpace(note.note))
            |> Seq.map (fun (ordinal, note) ->
                reservationNoticeId note.routeId note.routeDistinction note.tripId ordinal)
        Seq.concat [routeNotices; serviceNotices; reservations]
        |> Seq.map (fun sourceObjectId -> {
            severity = "warning"; code = "blank_notice"
            sourceObjectId = sourceObjectId
            message = "Textual notice is blank and was omitted"
        })
    let singletonRestrictions = callFacts.singletonRestrictionDiagnostics
    let conflictingRouteStops =
        batch.routeStops
        |> Seq.groupBy (fun stop -> stop.routeId, stop.routeDistinction, stop.routeStopId)
        |> Seq.choose (fun ((routeId, distinction, routeStopId), rows) ->
            let stopIds = rows |> Seq.map (fun stop -> stop.stopId) |> Seq.distinct |> Seq.toArray
            if stopIds.Length <= 1 then None else Some {
                severity = "error"; code = "conflicting_route_stop_zone_mapping"
                sourceObjectId = routeStopSourceId routeId distinction routeStopId
                message = "One source route-stop ID refers to multiple stop places"
            })
    let publicLines = JdfGtfsRules.getPublicLineNumbers batch
    let missingLines =
        batch.routes
        |> Seq.filter (fun route -> publicLines.[route.id, route.idDistinction].IsNone)
        |> Seq.map (fun route -> {
            severity = "warning"; code = "missing_public_line_number"
            sourceObjectId = JdfGtfsRules.jdfSourceRouteId route.id route.idDistinction
            message = "No unambiguous public line number could be selected"
        })
    let multiZones =
        let zones = feed.czStopZones |> Option.defaultValue [||] |> Seq.groupBy (fun zone -> zone.stopPlaceId)
        zones
        |> Seq.choose (fun (stopPlaceId, memberships) ->
            if memberships |> Seq.map (fun zone -> zone.zoneId) |> Seq.distinct |> Seq.length > 1 then
                Some {
                    severity = "warning"; code = "standard_zone_omitted"
                    sourceObjectId = stopPlaceId
                    message = "Standard GTFS zone_id is blank because this stop has multiple route-scoped zones"
                }
            else None)
    let conflictingPosts = callFacts.conflictingPostDiagnostics
    let missingStopCoordinates =
        feed.stops
        |> Seq.filter (fun stop -> stop.locationType = Some GtfsModel.Station)
        |> Seq.choose (fun stop ->
            match stop.lat, stop.lon with
            | Some 0m, Some 0m -> Some {
                severity = "warning"; code = "missing_stop_coordinates"
                sourceObjectId = stop.id
                message = "Referenced stop has no resolved coordinates and was serialized as 0,0"
              }
            | _ -> None)
    Seq.concat [filteredTrips; filteredEnrichments; unjoinableCallEnrichments; blankNotices
                singletonRestrictions; conflictingRouteStops; missingLines; multiZones
                conflictingPosts; missingStopCoordinates]
    |> Seq.sortBy (fun diagnostic -> diagnostic.code, diagnostic.sourceObjectId)
    |> Seq.toArray

let internal writeDiagnostics (stream: Stream) diagnostics =
    use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))
    writer.WriteStartObject()
    writer.WriteNumber("schema_version", 1)
    writer.WriteStartArray("diagnostics")
    for diagnostic in diagnostics do
        writer.WriteStartObject()
        writer.WriteString("severity", diagnostic.severity)
        writer.WriteString("code", diagnostic.code)
        writer.WriteString("source_object_id", diagnostic.sourceObjectId)
        writer.WriteString("message", diagnostic.message)
        writer.WriteEndObject()
    writer.WriteEndArray()
    writer.WriteEndObject()

let internal validateStopCoordinates (feed: GtfsModel.GtfsFeed) =
    feed.stops
    |> Seq.iter (fun stop ->
        match stop.lat, stop.lon with
        | Some lat, Some lon
            when lat >= -90m && lat <= 90m && lon >= -180m && lon <= 180m -> ()
        | _ -> invalidArg "feed" $"Stop {stop.id} has missing or out-of-range coordinates")

/// Record the bundle's GVD, which the serving manifest exposes as service_horizon.
let internal recordManifestGvd (text: string) (gvdYear: int option) =
    match gvdYear with
    | None -> text
    | Some year ->
        let startDate, endDate = DateUtils.gvdBounds year
        let manifest = System.Text.Json.Nodes.JsonNode.Parse(text).AsObject()
        let gvd = System.Text.Json.Nodes.JsonObject()
        gvd.["year"] <- System.Text.Json.Nodes.JsonValue.Create(year)
        gvd.["start_date"] <- System.Text.Json.Nodes.JsonValue.Create(startDate.ToString("yyyyMMdd", CultureInfo.InvariantCulture))
        gvd.["end_date"] <- System.Text.Json.Nodes.JsonValue.Create(endDate.ToString("yyyyMMdd", CultureInfo.InvariantCulture))
        manifest.["gvd"] <- gvd
        manifest.ToJsonString(JsonSerializerOptions(WriteIndented = true))

let internal jsonElement (text: string) =
    use document = JsonDocument.Parse(text)
    document.RootElement.Clone()

let internal serializeJson (write: Stream -> unit) =
    use stream = new MemoryStream()
    write stream
    Text.Encoding.UTF8.GetString(stream.ToArray())

let internal writeManifest (stream: Stream) descriptor (converterVersion: string) 
                          (internationalPolicy: JdfGtfsRules.InternationalRoutePolicy)
                          (internationalDecisions: JdfGtfsRules.InternationalRouteDecision array)
                          (transportModeRules: JdfGtfsRules.TransportModeRuleSet)
                          (transportModeDecisions: JdfGtfsRules.TransportModeDecision array)
                          (postPlan: JdfPostPlan.PostEstimationPlan)
                          (routingPbfPath: string option)
                          (postInferenceEvidencePath:string option)
                          (postInferencePolicyPath:string option)
                          diagnosticPostLabels
                          (batch: JdfModel.JdfBatch) (feed: GtfsModel.GtfsFeed) =
    use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))
    writer.WriteStartObject()
    writer.WriteString("bundle_format", "obehy-jrutil-jdf")
    writer.WriteNumber("bundle_version", BundleVersion)
    writer.WriteStartObject("source_snapshot")
    writer.WriteNumber("schema_version", 1)
    writer.WriteString("source_id", descriptor.sourceId)
    writer.WriteString("retrieved_at", descriptor.retrievedAt)
    writer.WriteString("retrieval_method", descriptor.retrievalMethod)
    match descriptor.sourceUri with Some value -> writer.WriteString("source_uri", value) | None -> writer.WriteNull("source_uri")
    writer.WriteString("licence", descriptor.licence)
    writer.WriteString("payload_kind", descriptor.payloadKind)
    writer.WriteString("payload_sha256", descriptor.payloadSha256)
    writer.WriteNumber("payload_bytes", descriptor.payloadBytes)
    writer.WriteEndObject()
    writer.WriteStartObject("source_format")
    writer.WriteString("kind", "jdf")
    writer.WriteString("version", batch.version.version)
    match batch.version.duNum with Some value -> writer.WriteNumber("du_number", value) | None -> writer.WriteNull("du_number")
    match batch.version.region with Some value -> writer.WriteString("region", value) | None -> writer.WriteNull("region")
    match batch.version.batchId with Some value -> writer.WriteString("batch_id", value) | None -> writer.WriteNull("batch_id")
    match batch.version.creationDate with Some value -> writer.WriteString("creation_date", localDateString value) | None -> writer.WriteNull("creation_date")
    match batch.version.generator with Some value -> writer.WriteString("generator", value) | None -> writer.WriteNull("generator")
    writer.WriteEndObject()
    writer.WriteStartObject("conversion")
    writer.WriteString("tool", "jrutil")
    writer.WriteString("version", converterVersion)
    writer.WriteStartObject("international_route_filter")
    writer.WriteString("policy", JdfGtfsRules.internationalRoutePolicyName internationalPolicy)
    writer.WriteNumber("non_integrated_maximum_trip_span_km", 120)
    writer.WriteNumber("non_integrated_maximum_foreign_depth_km", 60)
    writer.WriteNumber("integrated_maximum_trip_span_km", 200)
    writer.WriteNumber("integrated_maximum_foreign_depth_km", 80)
    writer.WriteNumber(
        "retained_cross_border_route_distinctions",
        internationalDecisions
        |> Seq.filter (fun decision ->
            decision.keep && decision.countries |> Array.exists ((<>) "CZ"))
        |> Seq.length)
    writer.WriteNumber(
        "dropped_route_distinctions",
        internationalDecisions |> Seq.filter (fun decision -> not decision.keep) |> Seq.length)
    writer.WriteNumber("retained_domestic_trips", internationalDecisions |> Seq.sumBy (fun d -> d.retainedDomesticTrips))
    writer.WriteNumber("qualifying_cross_border_trips", internationalDecisions |> Seq.sumBy (fun d -> d.qualifyingCrossBorderTrips))
    writer.WriteNumber("rejected_cross_border_trips", internationalDecisions |> Seq.sumBy (fun d -> d.rejectedCrossBorderTrips))
    writer.WriteNumber("foreign_only_trips_pruned", internationalDecisions |> Seq.sumBy (fun d -> d.foreignOnlyTrips))
    writer.WriteEndObject()
    writer.WriteStartObject("transport_mode_corrections")
    match transportModeRules.sha256 with Some value -> writer.WriteString("rules_sha256", value) | None -> writer.WriteNull("rules_sha256")
    writer.WriteNumber("corrected_routes", transportModeDecisions |> Seq.filter (fun d -> d.corrected) |> Seq.length)
    writer.WriteNumber("guard_mismatches", transportModeDecisions |> Seq.filter (fun d -> not d.corrected) |> Seq.length)
    writer.WriteStartObject("by_effective_mode")
    for mode, values in transportModeDecisions |> Seq.filter (fun d -> d.corrected) |> Seq.groupBy (fun d -> string d.effectiveMode) |> Seq.sortBy fst do
        writer.WriteNumber(mode, values |> Seq.length)
    writer.WriteEndObject()
    writer.WriteEndObject()
    writer.WriteStartObject("route_type_distribution")
    for routeType, values in feed.routes |> Seq.groupBy (fun route -> route.routeType) |> Seq.sortBy fst do
        writer.WriteNumber(routeType, values |> Seq.length)
    writer.WriteEndObject()
    writer.WriteStartObject("estimated_posts")
    let selectedDocument = postInferencePolicyPath |> Option.map JdfPostInferencePolicy.loadPolicyWithScorer
    let selectedPolicy =
        selectedDocument |> Option.map _.policy
        |> Option.defaultValue JdfPostInferencePolicy.conservativeRoutedV4
    writer.WriteString("execution_mode",
        if postInferenceEvidencePath.IsSome && routingPbfPath.IsSome then "live"
        elif postInferenceEvidencePath.IsSome then "replay"
        else "disabled")
    writer.WriteString("evaluator_version",JdfPostInference.EvaluatorVersion)
    writer.WriteNumber("policy_schema_version",selectedPolicy.schemaVersion)
    writer.WriteString("policy_id",selectedPolicy.policyId)
    writer.WriteString("policy_sha256",JdfPostInferencePolicy.policySha256 selectedPolicy)
    writer.WriteString("scorer",
        match selectedDocument |> Option.map _.scorer with
        | Some(JdfPostInferencePolicy.LearnedScorer _) -> "learned"
        | _ -> "heuristic")
    match selectedDocument with
    | Some document -> writer.WriteString("policy_document_sha256",document.documentSha256)
    | None -> writer.WriteNull("policy_document_sha256")
    writer.WriteBoolean("diagnostic_labels", diagnosticPostLabels)
    match routingPbfPath with
    | Some value ->
        writer.WriteString("routing_pbf_sha256", sha256File value)
        writer.WriteString("routing_manifest_sha256", sha256File (value + ".manifest.json"))
    | None ->
        writer.WriteNull("routing_pbf_sha256")
        writer.WriteNull("routing_manifest_sha256")
    match postInferenceEvidencePath with
    | Some value ->
        let full=Path.GetFullPath(value)
        let evidenceManifest=JdfPostInference.loadEvidenceManifest full
        writer.WriteString("evidence_format",evidenceManifest.evidenceFormat)
        writer.WriteString("evidence_manifest_sha256",sha256File(Path.Combine(full,"manifest.json")))
        writer.WriteString("routing_evidence_sha256",
            sha256File(Path.Combine(full,"route_point_evidence.parquet")))
    | None ->
        writer.WriteNull("evidence_format")
        writer.WriteNull("evidence_manifest_sha256")
        writer.WriteNull("routing_evidence_sha256")
    writer.WriteNumber("candidate_bearing_stops", postPlan.candidateStopCount)
    writer.WriteNumber("authored_posts_positioned", postPlan.authored.Count)
    writer.WriteNumber(
        "single_internal_posts",
        postPlan.calls
        |> Seq.map (fun pair -> pair.Value)
        |> Seq.distinctBy (fun selection -> selection.locationId)
        |> Seq.filter (fun selection -> selection.selectionKind = "physical")
        |> Seq.length)
    writer.WriteNumber(
        "composite_internal_posts",
        postPlan.calls
        |> Seq.map (fun pair -> pair.Value)
        |> Seq.distinctBy (fun selection -> selection.locationId)
        |> Seq.filter (fun selection -> selection.selectionKind = "centroid")
        |> Seq.length)
    writer.WriteNumber("single_candidate_skips", postPlan.singleCandidateSkips)
    writer.WriteNumber("side_internal_posts", postPlan.locations |> Seq.filter (fun value -> value.selectionKind = "side") |> Seq.length)
    writer.WriteNumber("physical_internal_posts", postPlan.locations |> Seq.filter (fun value -> value.selectionKind = "physical") |> Seq.length)
    writer.WriteNumber("side_groups", postPlan.sideGroups.Length)
    writer.WriteNumber("centroid_pattern_fallbacks", postPlan.unresolvedPatternContexts.Length)
    writer.WriteNumber("modality_explicit", postPlan.modalityEstimates |> Seq.filter (fun value -> value.status = "Explicit") |> Seq.length)
    writer.WriteNumber("modality_estimated", postPlan.modalityEstimates |> Seq.filter (fun value -> value.status = "Estimated") |> Seq.length)
    writer.WriteNumber("distinct_pattern_scores", postPlan.scoreCount)
    writer.WriteNumber("weak_or_unresolved_contexts", postPlan.unresolvedContexts)
    writer.WriteNumber("two_call_same_stop_blocks", postPlan.sameStopBlocks)
    writer.WriteNumber("distinct_pair_choices", postPlan.distinctPairChoices)
    writer.WriteNumber("unresolved_block_edges", postPlan.unresolvedBlockEdges)
    writer.WriteEndObject()
    writer.WriteEndObject()
    writer.WriteEndObject()
