// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.Reports

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open NodaTime

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay

type Input = {
    prepared: InputPreparation.Result
    projection: Projection.Result
    source: SourceAnalysis.Result
    matches: TripMatching.Result
    temporary: string
    usedStopIds: HashSet<string>
    usedRouteIds: HashSet<string>
    usedOutputShapeIds: Set<string>
    slicesForBinding: MatchBinding -> (TripSlice * DateSet.Dates) array
    transferProvenance: ResizeArray<string array>
}

type Result = {
    fullyCoveredSourceTrips: HashSet<string>
    matchedSourceTripSet: Set<string>
    activeSourceTrips: CsvRow array
}

/// Write mappings, provenance and coverage from final output decisions.
let write ({
    prepared = prepared
    projection = projection
    source = source
    matches = matches
    temporary = temporary
    usedStopIds = usedStopIds
    usedRouteIds = usedRouteIds
    usedOutputShapeIds = usedOutputShapeIds
    slicesForBinding = slicesForBinding
    transferProvenance = transferProvenance
}: Input) : Result =
    let dateRanges (values: seq<bool>) =
        let dates = Seq.toArray values
        seq {
            let mutable start = -1
            for index in 0 .. dates.Length do
                let active = index < dates.Length && dates.[index]
                if active && start < 0 then start <- index
                elif not active && start >= 0 then
                    yield prepared.window.dates.[start], prepared.window.dates.[index - 1]
                    start <- -1
        }
    let baseTripMappings =
        seq {
            for baseTrip in prepared.baseTripValues do
                match projection.slicesByBaseTrip.TryGetValue(baseTrip.id) with
                | true, slices ->
                    for slice in slices do
                        for firstDate, lastDate in dateRanges slice.dates do
                            yield [| baseTrip.id; slice.trip.id; dateString firstDate; dateString lastDate |]
                | _ -> ()
        }
    writeValues (Path.Combine(temporary, "mappings", "base_to_output_trips.csv"))
        [| "base_trip_id"; "output_trip_id"; "valid_from"; "valid_to" |] baseTripMappings
    writeValues (Path.Combine(temporary, "mappings", "base_to_output_routes.csv"))
        [| "base_route_id"; "output_route_id" |]
        (usedRouteIds |> Seq.filter prepared.baseRoutes.ContainsKey |> Seq.sort |> Seq.map (fun id -> [| id; id |]))
    writeValues (Path.Combine(temporary, "mappings", "base_to_output_stops.csv"))
        [| "base_stop_id"; "output_stop_id" |]
        (prepared.baseStopRows |> Seq.map (fun row -> rowValue row "stop_id") |> Seq.filter usedStopIds.Contains |> Seq.sort |> Seq.map (fun id -> [| id; id |]))
    writeValues (Path.Combine(temporary, "mappings", "source_to_output_stops.csv"))
        [| "source_id"; "source_stop_id"; "output_stop_id"; "target_stop_place_id"; "method" |]
        (projection.acceptedSourceStopIds
         |> Seq.filter (fun sourceStopId -> usedStopIds.Contains(projection.outputStopForSource.[sourceStopId]))
         |> Seq.sort
         |> Seq.map (fun sourceStopId ->
             let groupId = source.groupByMember.[sourceStopId]
             let sourceRow = projection.sourceStops.[sourceStopId]
             [| sourceIdentity prepared.binding.sourceId sourceRow; originalIdentity "stop_id" sourceRow; projection.outputStopForSource.[sourceStopId]; source.mappedPlaceBySourceStop.[sourceStopId]; source.stopMatchMethods.[groupId] |]))
    writeValues (Path.Combine(temporary, "reports", "post_pruning.csv"))
        [| "source_stop_id"; "output_post_id"; "disposition" |]
        (projection.outputStopForSource
         |> Seq.choose (fun pair ->
             let parent = source.mappedPlaceBySourceStop.[pair.Key]
             if pair.Value = parent then None
             else Some [| pair.Key; pair.Value; if usedStopIds.Contains(pair.Value) then "retained_used" else "pruned_unused" |]))
    writeValues (Path.Combine(temporary, "reports", "stop_group_matches.csv"))
        [| "source_group_id"; "source_stop_id"; "source_stop_name"; "target_stop_place_id"; "target_stop_name"; "target_stable_digest"; "method"; "status"; "distance_metres"; "geodata_disposition" |]
        (source.stopGroups
         |> Seq.collect (fun group ->
             let target = match source.stopGroupMatches.TryGetValue(group.groupId) with | true, value -> value | _ -> ""
             let targetName, targetDigest =
                 if source.nativeStopPlaces.ContainsKey(target) then
                     let native = source.nativeStopPlaces.[target]
                     native.name, sha256Text (prepared.stableStopNameKey native.name)
                 elif String.IsNullOrEmpty(target) || not (prepared.baseStopGroupById.ContainsKey(target)) then "", ""
                 else
                     let targetGroup = prepared.baseStopGroupById.[target]
                     let coordinate =
                         match targetGroup.lat, targetGroup.lon with
                         | Some lat, Some lon -> lat.ToString("G29", CultureInfo.InvariantCulture) + "," + lon.ToString("G29", CultureInfo.InvariantCulture)
                         | _ -> ""
                     targetGroup.name, sha256Text (prepared.stableStopNameKey targetGroup.name + "|" + coordinate)
             let methodName = match source.stopMatchMethods.TryGetValue(group.groupId) with | true, value -> value | _ -> ""
             let status =
                 if String.IsNullOrEmpty(target) then "quarantined"
                 elif methodName.Contains("gtfs_authoritative", StringComparison.Ordinal) then "corrected"
                 elif methodName = "source_native" then "native"
                 else "matched"
             let distance =
                 match source.stopMatchDistances.TryGetValue(group.groupId) with
                 | true, value -> value.ToString("0.0", CultureInfo.InvariantCulture)
                 | _ -> ""
             let geodataDisposition =
                 if methodName.Contains("gtfs_authoritative", StringComparison.Ordinal) then "pid_name_and_coordinates"
                 elif methodName = "source_native" then "pid_native"
                 elif String.IsNullOrEmpty(target) then "none"
                 else "matched_identity"
             group.members
             |> Seq.map (fun stopMember ->
                 [| group.groupId; rowValue stopMember "stop_id"; rowValue stopMember "stop_name"; target; targetName; targetDigest; methodName; status; distance; geodataDisposition |])))
    writeValues (Path.Combine(temporary, "reports", "stop_context_inference.csv"))
        [| "source_group_id"; "source_trip_id"; "source_call_ordinal"; "target_stop_place_id"; "target_stop_name"; "method" |]
        (source.stopInferenceEvidence
         |> Seq.map (fun value ->
             let targetName = if prepared.baseStopGroupById.ContainsKey(value.targetStopPlaceId) then prepared.baseStopGroupById.[value.targetStopPlaceId].name else ""
             [| value.sourceGroupId; value.sourceTripId; string value.sourceCallOrdinal; value.targetStopPlaceId; targetName; value.method |])
         |> Seq.distinctBy (fun row -> String.concat "\u001f" row))
    let sourceTripMappings =
        seq {
            for matchBinding in matches.bindings do
                for slice, sharedDates in slicesForBinding matchBinding do
                    for firstDate, lastDate in dateRanges sharedDates do
                        yield [|
                            matchBinding.sourceId; originalIdentity "trip_id" source.tripsById.[matchBinding.sourceTripId]; matchBinding.targetTripId; slice.trip.id
                            dateString firstDate; dateString lastDate; matchBinding.method; string matchBinding.editCount
                            string matchBinding.firstDepartureDelta; string matchBinding.aggregateTimeDelta; string matchBinding.durationDelta
                            matchBinding.runnerUpMargin |> Option.map string |> Option.defaultValue ""
                        |]
            for addition in projection.sourceTripAdditions do
                for sourceId, sourceTripId in addition.projection.sourceTripReferences do
                    for firstDate, lastDate in dateRanges addition.projection.dates do
                        yield [|
                            sourceId; sourceTripId; ""; addition.trip.id
                            dateString firstDate; dateString lastDate; "authoritative_source_trip_set"; ""
                            ""; ""; ""; ""
                        |]
        }
        |> Seq.distinctBy (fun row -> String.concat "\u001f" row)
        |> Seq.toArray
    writeValues (Path.Combine(temporary, "mappings", "source_to_output_trips.csv"))
        [| "source_id"; "source_trip_id"; "base_trip_id"; "output_trip_id"; "valid_from"; "valid_to"; "method"; "pattern_edits"; "first_departure_delta_seconds"; "aggregate_time_delta_seconds"; "duration_delta_seconds"; "runner_up_margin" |]
        sourceTripMappings
    let outputRangesBySourceTrip =
        sourceTripMappings
        |> Array.groupBy (fun row -> struct (row.[0], row.[1]))
        |> dict
    let operationalCandidates =
        csvRows prepared.binding.payloadPath "operational_trip_candidates.txt"
        |> Seq.filter (fun row -> source.tripsById.ContainsKey(rowValue row "trip_id"))
        |> Seq.collect (fun row ->
            let key = struct (rowValue row "source_id", rowValue row "original_trip_id")
            match outputRangesBySourceTrip.TryGetValue(key) with
            | true, ranges ->
                ranges
                |> Seq.map (fun mapping -> [|
                    rowValue row "source_id"; rowValue row "operational_line_id"; rowValue row "operational_trip_id"
                    rowValue row "original_trip_id"; mapping.[4]; mapping.[5]
                |])
            | _ -> Seq.empty)
        |> Seq.distinctBy (fun row -> String.concat "\u001f" row)
    writeValues (Path.Combine(temporary, "mappings", "operational_to_source_trips.csv"))
        [| "source_id"; "operational_line_id"; "operational_trip_id"; "source_trip_id"; "valid_from"; "valid_to" |]
        operationalCandidates
    let sourceRouteMappings =
        Seq.append
            (matches.bindings
             |> Seq.map (fun value ->
                 let sourceRouteId = rowValue source.tripsById.[value.sourceTripId] "route_id"
                 let targetRouteId = prepared.baseTrips.[value.targetTripId].routeId
                 let sourceRow = source.routes.[sourceRouteId]
                 [| value.sourceId; originalIdentity "route_id" sourceRow; targetRouteId; value.method |]))
            (projection.sourceTripAdditions
             |> Seq.collect (fun value ->
                 value.projection.sourceIds
                 |> Seq.map (fun sourceId -> [| sourceId; originalIdentity "route_id" source.routes.[value.projection.sourceRouteId]; value.trip.routeId; "authoritative_source_trip_set" |])))
        |> Seq.distinctBy (fun row -> String.concat "\u001f" row)

    writeValues (Path.Combine(temporary, "mappings", "source_to_output_routes.csv"))
        [| "source_id"; "source_route_id"; "output_route_id"; "method" |] sourceRouteMappings
    logProgress "write-call-mappings" 0L None
    let callMappings = seq {
        let bindingsBySource = matches.bindings |> Seq.groupBy (fun binding -> binding.sourceTripId) |> dict
        let sourceIds =
            Seq.append bindingsBySource.Keys (projection.sourceTripAdditions |> Seq.map (fun addition -> addition.projection.sourceTripId))
            |> Seq.distinct |> Seq.sort
        for sourceTripId in sourceIds do
            // All columns except the fixed source identity are part of this structural key.
            let seen = HashSet<struct (int * string * string * int * string)>()
            let emit sourceOrdinal sourceStop outputTrip outputOrdinal outputStop =
                if seen.Add(struct (sourceOrdinal, sourceStop, outputTrip, outputOrdinal, outputStop)) then
                    let tripRow = source.tripsById.[sourceTripId]
                    let stopRow = projection.sourceStops.[sourceStop]
                    Some [| sourceIdentity prepared.binding.sourceId tripRow; originalIdentity "trip_id" tripRow; string sourceOrdinal; originalIdentity "stop_id" stopRow; outputTrip; string outputOrdinal; outputStop |]
                else None
            match bindingsBySource.TryGetValue(sourceTripId) with
            | true, bindings ->
                for binding in bindings do
                    for slice, sharedDates in slicesForBinding binding do
                        if anyDate sharedDates then
                            let selection = projection.selectionByBinding.[projection.bindingKey binding]
                            for targetIndex in 0 .. selection.sourceOrdinalByTarget.Length - 1 do
                                match selection.sourceOrdinalByTarget.[targetIndex] with
                                | Some sourceIndex ->
                                    match emit (sourceIndex + 1) selection.sourceStopIds.[sourceIndex] slice.trip.id (targetIndex + 1) selection.outputStopIds.[targetIndex] with
                                    | Some row -> yield row
                                    | None -> ()
                                | None -> ()
            | _ -> ()
            match projection.sourceTripAdditionsBySourceId.TryGetValue(sourceTripId) with
            | true, addition ->
                let sourceProjection = projection.projectionsBySource.[sourceTripId]
                for sourceIndex in 0 .. sourceProjection.sourceCalls.Length - 1 do
                    let call = sourceProjection.sourceCalls.[sourceIndex]
                    match emit (sourceIndex + 1) call.stopId addition.trip.id (sourceIndex + 1) projection.outputStopForSource.[call.stopId] with
                    | Some row -> yield row
                    | None -> ()
            | _ -> ()
    }
    writeValues (Path.Combine(temporary, "mappings", "source_to_output_calls.csv"))
        [| "source_id"; "source_trip_id"; "source_call_ordinal"; "source_stop_id"; "output_trip_id"; "output_call_ordinal"; "output_stop_id" |]
        callMappings
    logProgress "write-coverage-reports" 0L None
    let acceptedDateBuilders = Dictionary<string, System.Collections.BitArray>(StringComparer.Ordinal)
    let acceptDate sourceTripId dateIndex =
        let dates =
            match acceptedDateBuilders.TryGetValue(sourceTripId) with
            | true, dates -> dates
            | _ ->
                let dates = System.Collections.BitArray(prepared.window.dates.Length)
                acceptedDateBuilders.Add(sourceTripId, dates)
                dates
        dates.[dateIndex] <- true
    for matchBinding in matches.bindings do
        for _, sharedDates in slicesForBinding matchBinding do
            for dateIndex in 0 .. sharedDates.Length - 1 do
                if sharedDates.[dateIndex] then acceptDate matchBinding.sourceTripId dateIndex
    for addition in projection.sourceTripAdditions do
        for sourceId, originalTripId in addition.projection.sourceTripReferences do
            source.tripProjections
            |> Array.tryFind (fun value -> value.sourceId = sourceId && originalIdentity "trip_id" value.sourceRow = originalTripId)
            |> Option.iter (fun sourceProjection ->
                for dateIndex in 0 .. sourceProjection.dates.Length - 1 do
                    if sourceProjection.dates.[dateIndex] then acceptDate sourceProjection.sourceTripId dateIndex)
    let acceptedDates =
        acceptedDateBuilders
        |> Seq.map (fun pair ->
            let dates = seq { for i in 0 .. pair.Value.Length - 1 -> pair.Value.[i] } |> DateSet.Dates.Of
            KeyValuePair(pair.Key, dates))
        |> fun values -> Dictionary<string, DateSet.Dates>(values, StringComparer.Ordinal)
    let accepted sourceTripId dateIndex =
        match acceptedDates.TryGetValue(sourceTripId) with
        | true, dates -> dates.[dateIndex]
        | _ -> false
    let countDates (dates: DateSet.Dates) = dates.Count
    let fullyCoveredSourceTrips = HashSet<string>(StringComparer.Ordinal)
    for sourceTripRow in source.tripRows do
        let sourceTripId = rowValue sourceTripRow "trip_id"
        match source.dates.TryGetValue(rowValue sourceTripRow "service_id") with
        | true, dates when anyDate dates ->
            let mutable covered = true
            let mutable dateIndex = 0
            while covered && dateIndex < dates.Length do
                if dates.[dateIndex] && not (accepted sourceTripId dateIndex) then covered <- false
                dateIndex <- dateIndex + 1
            if covered then fullyCoveredSourceTrips.Add(sourceTripId) |> ignore
        | _ -> ()
    writeValues (Path.Combine(temporary, "provenance", "transfer_claims.csv"))
        [| "source_from_stop_id"; "source_to_stop_id"; "disposition"; "detail" |]
        transferProvenance
    writeValues (Path.Combine(temporary, "reports", "trip_candidate_scores.csv"))
        [| "source_trip_id"; "base_trip_id"; "first_accepted_date"; "route_method"; "trip_method"; "pattern_edits"; "aligned_target_calls"; "first_departure_delta_seconds"; "aggregate_time_delta_seconds"; "duration_delta_seconds"; "maximum_aligned_time_delta_seconds"; "squared_aligned_time_delta"; "runner_up_margin" |]
        matches.candidateScoreReports
    writeValues (Path.Combine(temporary, "reports", "ambiguous_trip_candidates.csv"))
        [| "source_trip_id"; "date"; "base_trip_id"; "base_route_id"; "stable_line_descriptor"; "route_method"; "trip_method"; "pattern_edits"; "aligned_target_calls"; "first_departure_delta_seconds"; "aggregate_time_delta_seconds"; "duration_delta_seconds"; "maximum_aligned_time_delta_seconds"; "squared_aligned_time_delta"; "semantic_equivalence_key" |]
        matches.ambiguousCandidateRows
    writeValues (Path.Combine(temporary, "reports", "pattern_edits.csv"))
        [| "source_trip_id"; "base_trip_id"; "trip_method"; "pattern_edits"; "aligned_target_calls"; "target_call_count" |]
        (matches.bindings
         |> Seq.filter (fun value -> value.editCount > 0)
         |> Seq.map (fun value -> [|
             value.sourceTripId; value.targetTripId; projection.bindingTier value; string value.editCount
             string (value.sourceOrdinalByTarget |> Array.choose id |> Array.length); string value.sourceOrdinalByTarget.Length
         |])
         |> Seq.distinctBy (fun row -> String.concat "\u001f" row))
    logProgress "write-diagnostics" 0L None
    let unmatchedTripCodes =
        Set.ofList [
            "trip_call_pattern_unavailable"; "trip_stop_unresolved"; "trip_route_candidate_unresolved"; "trip_signature_unresolved"
            "trip_validity_unresolved"; "trip_same_date_ambiguous"; "base_snapshot_gap"
            "base_snapshot_capacity_gap"
        ]
    let sourceTripRowsById = source.tripRows |> Array.map (fun row -> rowValue row "trip_id", row) |> dict
    let matchedTripIds = matches.bindings |> Seq.map (fun value -> value.sourceTripId) |> Set.ofSeq
    let projectedTripIds = source.tripProjections |> Seq.map (fun value -> value.sourceTripId) |> Set.ofSeq
    let sourceNativeTripIds =
        projection.sourceTripAdditions
        |> Seq.filter (fun value -> value.projection.cisLineId.StartsWith("source:", StringComparison.Ordinal))
        |> Seq.map (fun value -> value.projection.sourceTripId)
        |> Set.ofSeq
    let unmatchedDiagnostics =
        prepared.diagnostics.Rows
        |> Seq.filter (fun value -> unmatchedTripCodes.Contains(value.code) && sourceTripRowsById.ContainsKey(value.sourceObjectId))
        |> Seq.toArray
    let diagnosedTripIds = unmatchedDiagnostics |> Seq.map (fun value -> value.sourceObjectId) |> Set.ofSeq
    let activeUnrepresentedTrips =
        source.tripRows
        |> Seq.filter (fun row ->
            let sourceTripId = rowValue row "trip_id"
            match source.dates.TryGetValue(rowValue row "service_id") with
            | true, dates -> anyDate dates && not (acceptedDates.ContainsKey(sourceTripId)) && not (diagnosedTripIds.Contains(sourceTripId))
            | _ -> false)
    writeValues (Path.Combine(temporary, "reports", "unmatched_trip_reasons.csv"))
        [| "source_id"; "source_trip_id"; "source_route_id"; "mode"; "reason_code"; "disposition"; "message" |]
        (Seq.append
          (unmatchedDiagnostics
           |> Seq.map (fun value ->
             let row = sourceTripRowsById.[value.sourceObjectId]
             let sourceRouteId = rowValue row "route_id"
             let disposition =
                 if sourceNativeTripIds.Contains(value.sourceObjectId) then "source_native_addition"
                 elif projectedTripIds.Contains(value.sourceObjectId) then "authoritative_projection"
                 elif matchedTripIds.Contains(value.sourceObjectId) then "matched_on_other_dates"
                 else "withheld"
             [|
                 sourceIdentity prepared.binding.sourceId row; originalIdentity "trip_id" row
                 originalIdentity "route_id" source.routes.[sourceRouteId]
                 modeClass (rowValue source.routes.[sourceRouteId] "route_type")
                 value.code; disposition; value.message
             |]))
          (activeUnrepresentedTrips
           |> Seq.map (fun row ->
               let sourceTripId = rowValue row "trip_id"
               let sourceRouteId = rowValue row "route_id"
               let reason, message =
                   if matchedTripIds.Contains(sourceTripId) || projectedTripIds.Contains(sourceTripId) then
                       "trip_projection_unrepresented", "Matching or authority evidence existed, but no active source date survived resolution and projection"
                   else
                       "trip_unclassified_unmatched", "The active trip produced neither matching evidence nor a more specific matching diagnostic"
               [|
                   sourceIdentity prepared.binding.sourceId row; originalIdentity "trip_id" row
                   originalIdentity "route_id" source.routes.[sourceRouteId]
                   modeClass (rowValue source.routes.[sourceRouteId] "route_type")
                   reason; "withheld"; message
                |]))
         |> Seq.distinctBy (fun row -> String.concat "\u001f" row))
    let effectiveDiagnostics =
        prepared.diagnostics.Rows
        |> Seq.filter (fun value ->
            let resolvedStop =
                (value.code = "stop_match_unresolved" || value.code = "stop_match_ambiguous")
                && source.stopGroupMatches.ContainsKey(value.sourceObjectId)
            let resolvedValidity =
                value.code = "trip_validity_unresolved" && matches.matchedAfterAvailability.Contains(value.sourceObjectId)
            let resolvedSameDateAmbiguity =
                value.code = "trip_same_date_ambiguous"
                && value.message.Length >= 8
                && matches.resolvedAmbiguityKeys.Contains(value.sourceObjectId + "|" + value.message.Substring(0, 8))
            let obsoleteTripFailure =
                fullyCoveredSourceTrips.Contains(value.sourceObjectId)
                && (value.code = "trip_route_candidate_unresolved"
                    || value.code = "trip_signature_unresolved"
                    || value.code = "trip_validity_unresolved"
                    || value.code = "trip_same_date_ambiguous"
                    || value.code = "base_snapshot_gap"
                    || value.code = "base_snapshot_capacity_gap")
            not resolvedStop && not resolvedValidity && not resolvedSameDateAmbiguity && not obsoleteTripFailure)
    writeValues (Path.Combine(temporary, "reports", "diagnostics.csv"))
        [| "code"; "source_object_id"; "message" |]
        (effectiveDiagnostics |> Seq.map (fun value -> [| value.code; value.sourceObjectId; value.message |]))
    let reportDiagnostics name predicate =
        effectiveDiagnostics
        |> Seq.filter predicate
        |> Seq.map (fun value -> [| value.code; value.sourceObjectId; value.message |])
        |> writeValues (Path.Combine(temporary, "reports", name)) [| "code"; "source_object_id"; "message" |]
    reportDiagnostics "ambiguities.csv" (fun value -> value.code.Contains("ambiguous", StringComparison.Ordinal))
    reportDiagnostics "conflicts.csv" (fun value -> value.code.Contains("conflict", StringComparison.Ordinal))
    reportDiagnostics "equivalent_ties.csv" (fun value -> value.code = "trip_equivalent_tie_expanded")
    reportDiagnostics "quarantine.csv" (fun value ->
        value.code.Contains("unresolved", StringComparison.Ordinal)
        || value.code.Contains("invalid", StringComparison.Ordinal)
        || value.code.Contains("missing", StringComparison.Ordinal))
    writeValues (Path.Combine(temporary, "reports", "substitutions.csv"))
        [| "source_trip_id"; "base_trip_id"; "matching_tier" |]
        (Seq.append
            (matches.bindings |> Seq.map (fun value -> [| value.sourceTripId; value.targetTripId; value.method |]))
            (projection.sourceTripAdditions |> Seq.map (fun value -> [| value.projection.sourceTripId; ""; "authoritative_source_trip_set" |]))
         |> Seq.distinctBy (fun row -> String.concat "\u001f" row))
    writeValues (Path.Combine(temporary, "reports", "source_trip_additions.csv"))
        [| "source_trip_id"; "output_trip_id"; "cis_line_id"; "output_route_id"; "valid_from"; "valid_to"; "reason" |]
        (seq {
            for addition in projection.sourceTripAdditions do
                for firstDate, lastDate in dateRanges addition.projection.dates do
                    yield [|
                        addition.projection.sourceTripId; addition.trip.id; addition.projection.cisLineId; addition.trip.routeId
                        dateString firstDate; dateString lastDate; "authoritative_source_trip_set"
                    |]
         })
    logProgress "write-exclusions" 0L None
    let allSourceRoutes = requireTable prepared.binding.payloadPath "routes.txt"
    let excludedRouteIds =
        allSourceRoutes
        |> Array.filter (fun row -> source.excludedTypes.Contains(rowValue row "route_type") || modeClass (rowValue row "route_type") = "heavy-rail")
        |> Array.map (fun row -> rowValue row "route_id")
        |> Set.ofArray
    let excludedTripCount =
        csvValues prepared.binding.payloadPath "trips.txt" [| "route_id" |]
        |> Seq.filter (fun row -> excludedRouteIds.Contains(row.[0]))
        |> Seq.length
    let pathwayCount = csvRows prepared.binding.payloadPath "pathways.txt" |> Seq.length
    let levelCount = csvRows prepared.binding.payloadPath "levels.txt" |> Seq.length
    writeValues (Path.Combine(temporary, "reports", "exclusions.csv"))
        [| "object_type"; "count"; "reason" |]
        [
            [| "route"; string excludedRouteIds.Count; "heavy_rail_permanently_protected" |]
            [| "trip"; string excludedTripCount; "heavy_rail_permanently_protected" |]
            [| "pathway"; string pathwayCount; "hard_exclusion" |]
            [| "level"; string levelCount; "hard_exclusion" |]
        ]
    let matchedSourceTripSet =
        acceptedDates.Keys
        |> Set.ofSeq
    let matchedSourceRouteSet =
        matchedSourceTripSet
        |> Seq.map (fun id -> rowValue source.tripsById.[id] "route_id")
        |> Set.ofSeq
    let activeSourceTrips =
        source.tripRows
        |> Array.filter (fun row ->
            match source.dates.TryGetValue(rowValue row "service_id") with
            | true, dates -> anyDate dates
            | _ -> false)
    let totalSourceTripDates =
        activeSourceTrips
        |> Seq.sumBy (fun row -> source.dates.[rowValue row "service_id"].Count)
    let matchedSourceTripDates = acceptedDates.Values |> Seq.sumBy countDates
    let matchedSourceCallDates =
        acceptedDates
        |> Seq.sumBy (fun pair -> countDates pair.Value * matches.sourceCallCountByTrip.[pair.Key])
    let totalSourceShapeIds = activeSourceTrips |> Array.choose (fun row -> optionText (rowValue row "shape_id")) |> Set.ofArray
    let usedSourceShapeIds =
        Seq.append
            (matches.bindings
             |> Seq.choose (fun value ->
                 value.sourceShapeId
                 |> Option.filter (fun id -> projection.outputShapeBySource.ContainsKey(id) && usedOutputShapeIds.Contains(projection.outputShapeBySource.[id]))))
            (projection.sourceTripAdditions
             |> Seq.choose (fun value ->
                 value.projection.sourceShapeId
                 |> Option.filter (fun id -> projection.outputShapeBySource.ContainsKey(id) && usedOutputShapeIds.Contains(projection.outputShapeBySource.[id]))))
        |> Set.ofSeq
    let totalSourceTransfers = csvRows prepared.binding.payloadPath "transfers.txt" |> Seq.length
    let projectedTransferClaims = transferProvenance |> Seq.filter (fun row -> row.[2] = "trip_specific_projected") |> Seq.length
    let coverage = [|
        "route", matchedSourceRouteSet.Count, source.routeRows.Length
        "stop", source.stopGroupMatches.Count, source.stopGroups.Length
        "trip", matchedSourceTripSet.Count, activeSourceTrips.Length
        "trip_date", matchedSourceTripDates, totalSourceTripDates
        "call", matches.coverageMatchedSourceCalls, matches.coverageTotalSourceCalls
        "call_date", matchedSourceCallDates, matches.coverageTotalSourceCallDates
        "shape", usedSourceShapeIds.Count, totalSourceShapeIds.Count
        "transfer", projectedTransferClaims, totalSourceTransfers
    |]
    let ratio matched total = if total = 0 then 1.0 else float matched / float total
    writeValues (Path.Combine(temporary, "reports", "headsigns.csv"))
        [| "source_trip_id"; "stop_sequence"; "raw_headsign"; "output_headsign"; "status"; "reason"; "destination_stop_id"; "audit_day" |] projection.headsignProvenance.Rows
    // Identity mappings describe provenance, not unconditional notice validity.
    // Consumers may inherit snapshot-only semantics solely on the evidence date.
    logProgress "write-semantic-inheritance" 0L None
    writeValues (Path.Combine(temporary, "reports", "semantic_inheritance.csv"))
        [| "source_trip_id"; "output_trip_id"; "base_trip_id"; "date"; "status" |]
        (seq {
            for matchBinding in matches.bindings do
                for slice, dates in slicesForBinding matchBinding do
                    for index in 0 .. dates.Length - 1 do
                        if dates.[index] then
                            yield [| matchBinding.sourceTripId; slice.trip.id; matchBinding.targetTripId; dateString prepared.window.dates.[index];
                                     (if prepared.window.dates.[index] = prepared.snapshotDate && matchBinding.sourceOrdinalByTarget = identityAlignment matchBinding.sourceCalls.Length then "aligned_snapshot_evidence" else "requires_explicit_notice_validity") |]
            for addition in projection.sourceTripAdditions do
                for index in 0 .. addition.projection.dates.Length - 1 do
                    if addition.projection.dates.[index] then
                        yield [| addition.projection.sourceTripId; addition.trip.id; ""; dateString prepared.window.dates.[index]; "unassigned_jdf_trip_semantics" |]
        })
    let dailyCoverage = Dictionary<string, int array * int array>(StringComparer.Ordinal)
    for trip in activeSourceTrips do
        let tripId = rowValue trip "trip_id"
        let mode = matches.modeForSourceTrip tripId
        let represented, total =
            match dailyCoverage.TryGetValue(mode) with
            | true, counts -> counts
            | _ ->
                let counts = Array.zeroCreate prepared.window.dates.Length, Array.zeroCreate prepared.window.dates.Length
                dailyCoverage.Add(mode, counts)
                counts
        let dates = source.dates.[rowValue trip "service_id"]
        for index in 0 .. dates.Length - 1 do
            if dates.[index] then
                total.[index] <- total.[index] + 1
                if accepted tripId index then represented.[index] <- represented.[index] + 1
    writeValues (Path.Combine(temporary, "reports", "snapshot_day_coverage.csv"))
        [| "date"; "scope"; "mode"; "represented"; "total"; "coverage" |]
        (seq {
            for dateIndex in 0 .. prepared.window.dates.Length - 1 do
                let day = prepared.window.dates.[dateIndex]
                for KeyValue(mode, (represented, totals)) in dailyCoverage |> Seq.sortBy (fun pair -> pair.Key) do
                    let count, total = represented.[dateIndex], totals.[dateIndex]
                    if total > 0 then
                        yield [| dateString day; (if day = prepared.auditDate then "audit_day" elif day > prepared.snapshotDate then "future_unverified" else "other_date"); mode; string count; string total; (ratio count total).ToString("0.000000", CultureInfo.InvariantCulture) |]
        })
    writeValues (Path.Combine(temporary, "reports", "coverage.csv"))
        [| "object_type"; "matched"; "total"; "coverage" |]
        (coverage
         |> Seq.map (fun (name, matched, total) -> [| name; string matched; string total; (ratio matched total).ToString("0.000000", CultureInfo.InvariantCulture) |]))
    let populationRows =
        let availableTargetTotal = matches.sharedDateCandidateSourceTrips.Count - matches.capacityGapSourceTrips.Count
        let availableTargetMatched = matchedSourceTripSet |> Seq.filter matches.sharedDateCandidateSourceTrips.Contains |> Seq.length
        [|
            "raw_active", matchedSourceTripSet.Count, activeSourceTrips.Length
            "shared_date_candidate", (matchedSourceTripSet |> Seq.filter matches.sharedDateCandidateSourceTrips.Contains |> Seq.length), matches.sharedDateCandidateSourceTrips.Count
            "pattern_compatible", (matchedSourceTripSet |> Seq.filter matches.patternCompatibleSourceTrips.Contains |> Seq.length), matches.patternCompatibleSourceTrips.Count
            "available_target_candidate", availableTargetMatched, availableTargetTotal
            "base_snapshot_gap", 0, matches.snapshotGapSourceTrips.Count
            "base_snapshot_capacity_gap", 0, matches.capacityGapSourceTrips.Count
        |]
    writeValues (Path.Combine(temporary, "reports", "trip_coverage_populations.csv"))
        [| "population"; "matched"; "total"; "coverage" |]
        (populationRows
         |> Seq.map (fun (name, matched, total) -> [|
             name; string matched; string total; (ratio matched total).ToString("0.000000", CultureInfo.InvariantCulture)
         |]))
    let activeRoutesByMode =
        activeSourceTrips
        |> Seq.groupBy (fun trip -> modeClass (rowValue source.routes.[rowValue trip "route_id"] "route_type"))
        |> Seq.map (fun (mode, trips) -> mode, trips |> Seq.map (fun trip -> rowValue trip "route_id") |> Set.ofSeq)
        |> dict
    let matchedRoutesByMode =
        matchedSourceTripSet
        |> Seq.groupBy matches.modeForSourceTrip
        |> Seq.map (fun (mode, tripIds) -> mode, tripIds |> Seq.map (fun id -> rowValue source.tripsById.[id] "route_id") |> Set.ofSeq)
        |> dict
    let activeTripsByMode = activeSourceTrips |> Seq.groupBy (rowValue >> fun get -> modeClass (rowValue source.routes.[get "route_id"] "route_type")) |> dict
    let matchedTripsByMode = matchedSourceTripSet |> Seq.groupBy matches.modeForSourceTrip |> dict
    let activeTripDatesByMode = Dictionary<string, int>(StringComparer.Ordinal)
    for sourceTrip in activeSourceTrips do
        let sourceTripId = rowValue sourceTrip "trip_id"
        let mode = matches.modeForSourceTrip sourceTripId
        matches.addCount activeTripDatesByMode mode (source.dates.[rowValue sourceTrip "service_id"].Count)
    let matchedTripDatesByMode = Dictionary<string, int>(StringComparer.Ordinal)
    let matchedCallDatesByMode = Dictionary<string, int>(StringComparer.Ordinal)
    for KeyValue(sourceTripId, dates) in acceptedDates do
        let mode = matches.modeForSourceTrip sourceTripId
        matches.addCount matchedTripDatesByMode mode (countDates dates)
        matches.addCount matchedCallDatesByMode mode (countDates dates * matches.sourceCallCountByTrip.[sourceTripId])
    let totalCallDatesByMode = Dictionary<string, int>(StringComparer.Ordinal)
    for sourceTrip in activeSourceTrips do
        let sourceTripId = rowValue sourceTrip "trip_id"
        let dateCount = source.dates.[rowValue sourceTrip "service_id"].Count
        matches.addCount totalCallDatesByMode (matches.modeForSourceTrip sourceTripId) (dateCount * matches.sourceCallCountByTrip.[sourceTripId])
    let activeShapesByMode =
        activeSourceTrips
        |> Seq.groupBy (rowValue >> fun get -> modeClass (rowValue source.routes.[get "route_id"] "route_type"))
        |> Seq.map (fun (mode, trips) -> mode, trips |> Seq.choose (rowValue >> fun get -> optionText (get "shape_id")) |> Set.ofSeq)
        |> dict
    let usedShapesByMode =
        activeSourceTrips
        |> Seq.choose (fun row ->
            optionText (rowValue row "shape_id")
            |> Option.filter usedSourceShapeIds.Contains
            |> Option.map (fun shapeId -> matches.modeForSourceTrip (rowValue row "trip_id"), shapeId))
        |> Seq.groupBy fst
        |> Seq.map (fun (mode, values) -> mode, values |> Seq.map snd |> Set.ofSeq)
        |> dict
    let modes = activeRoutesByMode.Keys |> Seq.sort |> Seq.toArray
    let countSet (values: IDictionary<string, Set<string>>) mode = match values.TryGetValue(mode) with | true, items -> items.Count | _ -> 0
    let countSeq (values: IDictionary<string, seq<'a>>) mode = match values.TryGetValue(mode) with | true, items -> Seq.length items | _ -> 0
    let modeCoverageRows =
        seq {
            for mode in modes do
                yield mode, "route", countSet matchedRoutesByMode mode, countSet activeRoutesByMode mode
                yield mode, "trip", countSeq matchedTripsByMode mode, countSeq activeTripsByMode mode
                yield mode, "trip_date", (match matchedTripDatesByMode.TryGetValue(mode) with | true, value -> value | _ -> 0), activeTripDatesByMode.[mode]
                match matches.stopCoverageByMode |> Array.tryFind (fun (value, _, _) -> value = mode) with
                | Some (_, matched, total) -> yield mode, "stop", matched, total
                | None -> ()
                yield mode, "call_date", (match matchedCallDatesByMode.TryGetValue(mode) with | true, value -> value | _ -> 0), totalCallDatesByMode.[mode]
                match matches.callCoverageByMode |> Array.tryFind (fun (value, _, _) -> value = mode) with
                | Some (_, matched, total) -> yield mode, "call", matched, total
                | None -> ()
                yield mode, "shape", countSet usedShapesByMode mode, countSet activeShapesByMode mode
        }
    writeValues (Path.Combine(temporary, "reports", "coverage_by_mode.csv"))
        [| "mode"; "object_type"; "matched"; "total"; "coverage" |]
        (modeCoverageRows
         |> Seq.map (fun (mode, name, matched, total) ->
             [| mode; name; string matched; string total; (ratio matched total).ToString("0.000000", CultureInfo.InvariantCulture) |]))
    writeValues (Path.Combine(temporary, "reports", "coverage_by_tier.csv"))
        [| "mode"; "matching_tier"; "matched_trips" |]
        (matches.bindings
         |> Seq.map (fun value -> matches.modeForSourceTrip value.sourceTripId, value.method, value.sourceTripId)
         |> Seq.distinct
         |> Seq.groupBy (fun (mode, methodName, _) -> mode, methodName)
         |> Seq.map (fun ((mode, methodName), values) -> [| mode; methodName; string (Seq.length values) |]))
    writeValues (Path.Combine(temporary, "reports", "trip_coverage_populations_by_mode.csv"))
        [| "mode"; "population"; "matched"; "total"; "coverage" |]
        (seq {
            for mode in modes do
                let active = activeTripsByMode.[mode] |> Seq.map (rowValue >> fun get -> get "trip_id") |> Set.ofSeq
                let rows = [|
                    "raw_active", matchedSourceTripSet |> Set.intersect active |> Set.count, active.Count
                    "shared_date_candidate", matchedSourceTripSet |> Seq.filter (fun id -> active.Contains(id) && matches.sharedDateCandidateSourceTrips.Contains(id)) |> Seq.length, matches.sharedDateCandidateSourceTrips |> Seq.filter active.Contains |> Seq.length
                    "pattern_compatible", matchedSourceTripSet |> Seq.filter (fun id -> active.Contains(id) && matches.patternCompatibleSourceTrips.Contains(id)) |> Seq.length, matches.patternCompatibleSourceTrips |> Seq.filter active.Contains |> Seq.length
                    "available_target_candidate", matchedSourceTripSet |> Seq.filter (fun id -> active.Contains(id) && matches.sharedDateCandidateSourceTrips.Contains(id)) |> Seq.length, (matches.sharedDateCandidateSourceTrips |> Seq.filter active.Contains |> Seq.length) - (matches.capacityGapSourceTrips |> Seq.filter active.Contains |> Seq.length)
                    "base_snapshot_gap", 0, matches.snapshotGapSourceTrips |> Seq.filter active.Contains |> Seq.length
                    "base_snapshot_capacity_gap", 0, matches.capacityGapSourceTrips |> Seq.filter active.Contains |> Seq.length
                |]
                for population, matched, total in rows do
                    yield [| mode; population; string matched; string total; (ratio matched total).ToString("0.000000", CultureInfo.InvariantCulture) |]
         })
    if prepared.policy.publicationEnabled then
        for name, matched, total in coverage do
            match prepared.policy.minimumCoverage.TryGetValue(name) with
            | true, minimum when ratio matched total >= minimum -> ()
            | true, minimum -> invalidOp $"Coverage floor failed for {name}: {ratio matched total:F6} < {minimum:F6}"
            | _ -> invalidOp $"Publication policy is missing minimum coverage for {name}"
    {
        fullyCoveredSourceTrips = fullyCoveredSourceTrips
        matchedSourceTripSet = matchedSourceTripSet
        activeSourceTrips = activeSourceTrips
    }
