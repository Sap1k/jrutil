// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Routing PBF manifests and the post-review GeoJSON.
module JrUtil.JdfBundleReview

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

[<Literal>]
let routingEnvelopePolicy = "jdf-routing-envelope-v2"

let validateRoutingPbfManifest path =
    let manifestPath = path + ".manifest.json"
    if not (File.Exists(manifestPath)) then
        invalidArg "routingPbfPath" $"Routing PBF manifest is missing: {manifestPath}"
    use manifest = JsonDocument.Parse(File.ReadAllBytes(manifestPath))
    let mutable schema = Unchecked.defaultof<JsonElement>
    if not (manifest.RootElement.TryGetProperty("filter_schema", &schema))
       || schema.GetString() <> routingEnvelopePolicy then
        invalidArg "routingPbfPath" "Routing PBF was not created by the supported Osmium demand-envelope policy"
    let mutable sourceKey = Unchecked.defaultof<JsonElement>
    if not (manifest.RootElement.TryGetProperty("source_key", &sourceKey))
       || sourceKey.ValueKind <> JsonValueKind.String
       || String.IsNullOrWhiteSpace(sourceKey.GetString()) then
        invalidArg "routingPbfPath" "Routing PBF manifest has no declared OSM source key"
    let mutable output = Unchecked.defaultof<JsonElement>
    let mutable bytes = Unchecked.defaultof<JsonElement>
    if not (manifest.RootElement.TryGetProperty("output", &output))
       || not (output.TryGetProperty("bytes", &bytes))
       || bytes.GetInt64() <> FileInfo(path).Length then
        invalidArg "routingPbfPath" "Routing PBF size does not match its Osmium manifest"

let internal writePostReviewGeoJson path selectorsPath 
                                   (batch: JdfModel.JdfBatch)
                                   (plan: JdfPostPlan.PostEstimationPlan) =
    let selectors =
        File.ReadLines(selectorsPath)
        |> Seq.map (fun value -> value.Trim())
        |> Seq.filter (fun value -> value.Length > 0 && not (value.StartsWith("#")))
        |> Seq.toArray
    let stopLabel (stop: JdfModel.Stop) =
        [| Some stop.town; stop.district; stop.nearbyPlace |]
        |> Array.choose id
        |> String.concat ","
    let selectedStopIds = HashSet<int64>()
    for selector in selectors do
        match Int64.TryParse(selector, Globalization.NumberStyles.Integer,
                             Globalization.CultureInfo.InvariantCulture) with
        | true, stopId -> selectedStopIds.Add(stopId) |> ignore
        | _ ->
            let matches =
                batch.stops
                |> Array.filter (fun stop ->
                    String.Equals(stopLabel stop, selector, StringComparison.OrdinalIgnoreCase))
            if matches.Length = 0 then
                Log.Warning("Post review selector did not match a stop: {Selector}", selector)
            for stop in matches do selectedStopIds.Add(stop.id) |> ignore
    let candidates =
        plan.physicalHypotheses
        |> Array.filter (fun candidate -> selectedStopIds.Contains(candidate.stopId))
        |> Array.map (fun candidate -> (candidate.stopId, candidate.hypothesisId), candidate)
        |> Map
    let scores =
        plan.scoreRows ()
        |> Seq.filter (fun score -> selectedStopIds.Contains(score.context.stopId))
        |> Seq.toArray
    let writeOptionString (writer: Utf8JsonWriter) (name: string) (value: string option) =
        match value with Some text -> writer.WriteString(name, text) | None -> writer.WriteNull(name)
    let writeOptionNumber (writer: Utf8JsonWriter) (name: string) (value: float option) =
        match value with Some number -> writer.WriteNumber(name, number) | None -> writer.WriteNull(name)
    let writeContextProperties (writer: Utf8JsonWriter) (score: JdfPostPlan.DerivedPostScore) =
        writer.WriteNumber("stop_id", score.context.stopId)
        writer.WriteString("gtfs_stop_place_id", JdfGtfsRules.jdfStopId score.context.stopId)
        writer.WriteString("candidate_id", score.candidateId)
        writer.WriteString("mode", transportModeCode score.context.mode)
        writer.WriteString("line_id", score.context.lineId)
        writer.WriteNumber("direction", score.context.direction)
        writer.WriteString("pattern_hash", score.context.patternHash)
        writer.WriteString("movement_family_id",score.movementFamilyId)
        writer.WriteNumber("pattern_position", score.context.position)
        writer.WriteString("same_stop_block_role", score.context.sameStopBlockRole)
        writeOptionString writer "corridor_id" score.corridorId
        writeOptionString writer "ingress_thread_id" score.ingressThreadId
        writeOptionString writer "egress_thread_id" score.egressThreadId
        writeOptionString writer "corridor_face_id" score.corridorFaceId
        writer.WriteString("routing_availability", score.routingAvailability)
        writer.WriteNumber("alternative_corridor_count", score.alternativeCorridorCount)
        writeOptionNumber writer "alternative_cost_gap" score.alternativeCostGap
        writeOptionNumber writer "snap_fraction" score.snapFraction
        writeOptionNumber writer "corridor_distance" score.corridorDistance
        writeOptionNumber writer "signed_lateral_offset" score.signedLateralOffset
        writeOptionNumber writer "corridor_heading" score.corridorHeading
        writeOptionNumber writer "attachment_heading" score.attachmentHeading
        writer.WriteNumber("alignment", score.alignment)
        writer.WriteNumber("side", score.side)
        writer.WriteNumber("proximity", score.proximity)
        writer.WriteNumber("routed_fit", score.routedFit)
        writer.WriteNumber("source_adjustment", score.sourceAdjustment)
        writer.WriteNumber("modality_adjustment", score.modalityAdjustment)
        writer.WriteNumber("popularity_prior", score.popularityPrior)
        writer.WriteNumber("total", score.total)
        writeOptionString writer "rejection_reason" score.rejectionReason
        match plan.calls.TryGetValue(score.context) with
        | true, selection ->
            writer.WriteString("decision", selection.selectionKind)
            writer.WriteBoolean("selected", selection.candidateIds |> Array.contains score.candidateId)
        | _ ->
            writer.WriteString("decision", "centroid")
            writer.WriteBoolean("selected", false)
    let writeFeatureStart (writer: Utf8JsonWriter) (featureKind: string) =
        writer.WriteStartObject()
        writer.WriteString("type", "Feature")
        writer.WriteStartObject("properties")
        writer.WriteString("feature_kind", featureKind)
    let writePointGeometry (writer: Utf8JsonWriter) (lon: float) (lat: float) =
        writer.WriteStartObject("geometry")
        writer.WriteString("type", "Point")
        writer.WriteStartArray("coordinates")
        writer.WriteNumberValue(lon)
        writer.WriteNumberValue(lat)
        writer.WriteEndArray()
        writer.WriteEndObject()
        writer.WriteEndObject()
    use stream = File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
    use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))
    writer.WriteStartObject()
    writer.WriteString("type", "FeatureCollection")
    writer.WriteStartArray("features")
    for evidence in batch.postCandidateEvidence do
        if selectedStopIds.Contains(evidence.stopId) then
            let hypothesis =
                plan.physicalHypotheses
                |> Array.tryFind (fun value ->
                    value.stopId=evidence.stopId
                    && value.memberObservationIds |> Array.contains evidence.observationId)
            writeFeatureStart writer "raw_observation"
            writer.WriteNumber("stop_id",evidence.stopId)
            writer.WriteString("observation_id",evidence.observationId)
            writer.WriteString("legacy_candidate_id",evidence.candidateId)
            writer.WriteString("source_kind",evidence.sourceKind)
            writeOptionString writer "source_object_id" evidence.sourceObjectId
            writer.WriteString("lifecycle",evidence.lifecycle)
            writeOptionString writer "hypothesis_id" (hypothesis |> Option.map (fun value -> value.hypothesisId))
            writer.WriteEndObject()
            writePointGeometry writer (float evidence.lon) (float evidence.lat)
    for score in scores do
        match candidates |> Map.tryFind (score.context.stopId, score.candidateId) with
        | None -> ()
        | Some candidate ->
            writeFeatureStart writer "hypothesis"
            writeContextProperties writer score
            writer.WriteString("member_observation_ids",String.Join(";",candidate.memberObservationIds))
            writer.WriteString("member_legacy_candidate_ids",String.Join(";",candidate.memberCandidateIds))
            writer.WriteString("representative_candidate_id",candidate.representativeCandidateId)
            writer.WriteEndObject()
            writePointGeometry writer (float candidate.lon) (float candidate.lat)
    let tangents =
        scores
        |> Array.choose (fun score ->
            match score.corridorHeading, score.signedLateralOffset,
                  candidates |> Map.tryFind (score.context.stopId, score.candidateId) with
            | Some heading, Some signed, Some candidate -> Some(score, heading, signed, candidate)
            | _ -> None)
        |> Array.distinctBy (fun (score, _, _, _) ->
            score.context.stopId, score.context.patternHash,
            score.context.position, score.corridorFaceId)
    for score, heading, signed, candidate in tangents do
        let radians = heading * Math.PI / 180.0
        let east, north = Math.Sin(radians), Math.Cos(radians)
        let lat = float candidate.lat
        let metresPerLat = 111_320.0
        let metresPerLon = max 1.0 (metresPerLat * Math.Cos(lat * Math.PI / 180.0))
        let centreLon = float candidate.lon + north * signed / metresPerLon
        let centreLat = lat - east * signed / metresPerLat
        let extent = 30.0
        let lon0, lat0 = centreLon - east * extent / metresPerLon, centreLat - north * extent / metresPerLat
        let lon1, lat1 = centreLon + east * extent / metresPerLon, centreLat + north * extent / metresPerLat
        writeFeatureStart writer "corridor_tangent"
        writeContextProperties writer score
        writer.WriteEndObject()
        writer.WriteStartObject("geometry")
        writer.WriteString("type", "LineString")
        writer.WriteStartArray("coordinates")
        writer.WriteStartArray(); writer.WriteNumberValue(lon0); writer.WriteNumberValue(lat0); writer.WriteEndArray()
        writer.WriteStartArray(); writer.WriteNumberValue(lon1); writer.WriteNumberValue(lat1); writer.WriteEndArray()
        writer.WriteEndArray()
        writer.WriteEndObject()
        writer.WriteEndObject()
    for group in plan.sideGroups |> Array.filter (fun value -> selectedStopIds.Contains(value.stopId)) do
        match candidates |> Map.tryFind (group.stopId, group.representativeCandidateId) with
        | None -> ()
        | Some representative ->
            writeFeatureStart writer "face"
            writer.WriteNumber("stop_id", group.stopId)
            writer.WriteString("side_group_id", group.sideGroupId)
            writer.WriteString("mode", string group.mode)
            writer.WriteString("corridor_face_id", group.corridorFaceId)
            writer.WriteString("sector", group.sector)
            writer.WriteString("representative_candidate_id", group.representativeCandidateId)
            writer.WriteString("member_candidate_ids", String.Join(";", group.memberCandidateIds))
            writer.WriteNumber("repeated_pattern_support", group.repeatedPatternSupport)
            writer.WriteEndObject()
            writePointGeometry writer (float representative.lon) (float representative.lat)
    writer.WriteEndArray()
    writer.WriteEndObject()
    writer.Flush()
