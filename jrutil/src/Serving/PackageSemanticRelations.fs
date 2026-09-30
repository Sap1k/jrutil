// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open JrUtil
open JrUtil.Hashing
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Runtime
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Parquet
open Parquet.Schema

open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay.Model

open JrUtil.Serving.PackageRows
open JrUtil.Serving.PackageBaseRelations
open JrUtil.Serving.PackageGtfsRelations
open JrUtil.Serving.PackageBindingRelations

/// Identity, semantic and CZPTT operational relations.
module PackageSemanticRelations =
    let internal identityRelations (input: CompilerOutput.Output) (bindings: TripBinding array) (manifest: JsonElement) =
        let firstDate, lastDate =
            if bindings.Length = 0 then DateOnly(1970, 1, 1), DateOnly(1970, 1, 1)
            else
                bindings |> Array.map (fun row -> row.valid_from) |> Array.min,
                bindings |> Array.map (fun row -> row.valid_to) |> Array.max
        let locationKind =
            CompilerOutput.rows input.gtfs "stops.txt"
            |> Seq.map (fun row ->
                let kind = if value "location_type" row = "1" || String.IsNullOrEmpty(value "parent_station" row) then "stop_place" else "boarding_point"
                value "stop_id" row, kind)
            |> dict
        let snapshots = combinedSourceManifest input manifest
        let defaultSource = defaultSourceId manifest snapshots
        let entityRows kind namespaceName mappingName sourceColumn targetColumn =
            let mapped =
                CompilerOutput.rows input.mappings mappingName
                |> Seq.map (fun row -> value "source_id" row, value sourceColumn row, value targetColumn row)
            let native =
                if input.basePackage |> Option.isSome then Seq.empty
                elif kind = "route" then
                    CompilerOutput.rows input.czech "cz_routes.txt"
                    |> Seq.map (fun row ->
                        let owner = value "source_provenance" row |> fun value -> if String.IsNullOrWhiteSpace(value) then defaultSource else value
                        owner, value "route_id" row, value "route_id" row)
                else
                    CompilerOutput.rows input.czech "cz_stops.txt"
                    |> Seq.map (fun row -> defaultSource, value "stop_id" row, value "stop_id" row)
            Seq.append mapped native |> Seq.distinct |> Seq.map (fun (sourceId, sourceObject, target) ->
                let effectiveKind = if kind = "location" then locationKind.[target] else kind
                let fields = [ "source_id", sourceId; "namespace", namespaceName; "kind", effectiveKind; "source_object_id", sourceObject; "public_id", target; "valid_from", firstDate.ToString("yyyyMMdd"); "valid_to", lastDate.ToString("yyyyMMdd") ]
                objectRow [
                    "entity_binding_id", box (Identity.bindingId "entity" fields); "source_id", box sourceId
                    "identifier_namespace", box namespaceName; "entity_kind", box effectiveKind
                    "source_object_id", box sourceObject; "public_id", box target
                    "valid_from", box firstDate; "valid_to", box lastDate ])
        let routeEntities = entityRows "route" "gtfs_route_id" "source_to_output_routes.csv" "source_route_id" "output_route_id"
        let stopEntities = entityRows "location" "gtfs_stop_id" "source_to_output_stops.csv" "source_stop_id" "output_stop_id"
        let entities = Seq.append routeEntities stopEntities |> Seq.toArray
        let routeBindingByTarget =
            entities |> Seq.filter (fun row -> row.["entity_kind"] :?> string = "route")
            |> Seq.groupBy (fun row -> row.["public_id"] :?> string)
            |> Seq.map (fun (key, values) -> key, values |> Seq.head) |> dict
        let routeKeys =
            CompilerOutput.rows input.czech "cz_routes.txt"
            |> Seq.choose (fun row ->
                let target, cis = value "route_id" row, value "cis_line_id" row
                match routeBindingByTarget.TryGetValue(target) with
                | true, binding when not (String.IsNullOrWhiteSpace(cis)) -> Some (objectRow [
                    "entity_binding_id", binding.["entity_binding_id"]; "cis_line_id", box cis; "route_id", box target
                    "valid_from", binding.["valid_from"]; "valid_to", binding.["valid_to"] ])
                | _ -> None)
        let tripKeys keyName keyType = seq {
            let tripBindingsByTarget = lazy (
                bindings |> Seq.groupBy (fun row -> row.trip_id)
                |> Seq.map (fun (key, values) -> key, values |> Seq.toArray) |> dict)
            yield! CompilerOutput.rows input.czech "cz_trips.txt"
            |> Seq.collect (fun row ->
                let target, key = value "trip_id" row, value keyName row
                if String.IsNullOrWhiteSpace(key) then Seq.empty else
                match tripBindingsByTarget.Value.TryGetValue(target) with
                | true, targetBindings when not (String.IsNullOrWhiteSpace(key)) ->
                    let preferredNamespace = if keyType = "rail" then "czptt_pa_id" else "gtfs_trip_id"
                    let preferred = targetBindings |> Array.filter (fun binding -> binding.trip_namespace = preferredNamespace)
                    (if preferred.Length > 0 then preferred else targetBindings) |> Seq.map (fun binding ->
                        if keyType = "road" then objectRow [
                            "binding_id", box binding.binding_id; "cis_line_id", box (value "cis_line_id" row)
                            "cis_trip_id", box (integer64 key); "trip_id", box target
                            "valid_from", box binding.valid_from; "valid_to", box binding.valid_to ]
                        else objectRow [
                            "binding_id", box binding.binding_id; "train_number", box key; "trip_id", box target
                            "valid_from", box binding.valid_from; "valid_to", box binding.valid_to ])
                | _ -> Seq.empty)
        }
        let evidence = bindings |> Seq.choose (fun binding ->
            let sourceId = binding.source_id
            match snapshots.TryGetValue(sourceId) with
            | true, digest -> Some (objectRow [
                "binding_kind", box "trip"; "binding_id", box binding.binding_id
                "evidence_source_id", box sourceId; "source_snapshot_sha256", box digest
                "identifier_namespace", box binding.trip_namespace; "source_object_id", box binding.source_trip_id
                "selection_rule", box (if binding.binding_status = "candidate" then "admissible_static_candidate" else "accepted_static_match") ])
            | _ -> None)
        let origins = seq {
            let rows kind namespaceName table idColumn =
                CompilerOutput.rows input.gtfs table |> Seq.map (fun row ->
                    let id = value idColumn row
                    objectRow [
                        "object_type", box kind; "object_key", box (Identity.compositeKey [ id ])
                        "source_id", box defaultSource; "source_snapshot_sha256", box (match snapshots.TryGetValue(defaultSource) with | true, value -> value | _ -> String.replicate 64 "0")
                        "identifier_namespace", box namespaceName; "source_object_id", box id; "selection_rule", box "source_or_compiler_origin" ])
            yield! rows "route" "gtfs_route_id" "routes.txt" "route_id"
            yield! rows "location" "gtfs_stop_id" "stops.txt" "stop_id"
            yield! rows "trip" "gtfs_trip_id" "trips.txt" "trip_id"
            yield! rows "shape" "gtfs_shape_id" "shapes.txt" "shape_id" |> Seq.distinctBy (fun row -> row.["object_key"])
            for transfer in CompilerOutput.rows input.gtfs "transfers.txt" do
                let selectors = [ "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" ]
                let key = selectors |> Seq.map (fun name -> value name transfer) |> Identity.compositeKey
                yield objectRow [
                    "object_type", box "transfer"; "object_key", box key; "source_id", box defaultSource
                    "source_snapshot_sha256", box (match snapshots.TryGetValue(defaultSource) with | true, value -> value | _ -> String.replicate 64 "0")
                    "identifier_namespace", box "gtfs_transfer_selectors"; "source_object_id", box key; "selection_rule", box "source_or_compiler_origin" ]
        }
        Map [
            "source_entity_map", entities :> seq<_>
            "road_route_key", routeKeys
            "road_trip_key", tripKeys "cis_trip_id" "road"
            "rail_trip_key", tripKeys "train_number" "rail"
            "binding_evidence", evidence
            "object_origin", origins
        ]

    let internal semanticRelations (nativeCalls: JrUtil.Serving.Model.NativeCallArtifacts option) (input: CompilerOutput.Output) (manifest: JsonElement) =
        let read name columns =
            CompilerOutput.values input.sidecars (Path.GetFileNameWithoutExtension(name: string)) columns
        let readOptional optional name columns =
            CompilerOutput.values input.sidecars (Path.GetFileNameWithoutExtension(name: string)) columns
        let snapshots = combinedSourceManifest input manifest
        let sourceId = defaultSourceId manifest snapshots
        let digest = match snapshots.TryGetValue(sourceId) with | true, value -> value | _ -> String.replicate 64 "0"
        let callFacts () =
            read "source_call_metadata.parquet"
                [| "gtfs_trip_id"; "stop_sequence"; "source_route_stop_id"; "gtfs_stop_id" |]
        let routeStops = seq {
            let routeByTrip = CompilerOutput.values input.gtfs "trips.txt" [| "trip_id"; "route_id" |] |> Seq.map (fun row -> row.[0], row.[1]) |> dict
            for row in callFacts () do
                match routeByTrip.TryGetValue(row.[0]) with
                | true, route ->
                    yield objectRow [
                        "route_id", box route; "route_stop_id", box (Identity.compositeKey [ route; row.[2] ]); "location_id", box row.[3] ]
                | _ -> ()
        }
        let routeStopZonesRaw =
            readOptional [ "source_route_version" ] "source_route_stop_zone_metadata.parquet" [| "gtfs_route_id"; "source_route_stop_id"; "zone_id"; "zone_order"; "source_route_version" |]
            |> Seq.toArray
        let routeStopZones = routeStopZonesRaw |> Seq.map (fun row -> objectRow [
            "route_id", box row.[0]; "route_stop_id", box (Identity.routeStopKey row.[0] row.[4] row.[1])
            "zone_id", box row.[2]; "source_order", box (integer row.[3]) ])
        let semanticZones = routeStopZonesRaw |> Seq.map (fun row -> objectRow [
            "zone_id", box row.[2]; "fare_system_id", null; "zone_code", box row.[2]; "name", null
            "source_id", box sourceId; "source_scope", box "route_stop" ])
        let notesRaw () =
            read "source_notice_metadata.parquet"
                [| "source_notice_id"; "notice_kind"; "gtfs_route_id"; "gtfs_trip_id"; "label"; "text"; "valid_from"; "valid_to"; "service_note_type" |]
        let notes = notesRaw () |> Seq.map (fun row -> objectRow [
            "note_id", box row.[0]; "kind", box row.[1]; "label", nullableString row.[4]; "text", nullableString row.[5]
            "valid_from", nullableParsed date row.[6]; "valid_to", nullableParsed date row.[7]; "service_note_type", nullableString row.[8]
            "source_id", box sourceId; "source_snapshot_sha256", box digest; "source_object_id", box row.[0] ])
        let noteAssignments = notesRaw () |> Seq.choose (fun row ->
            let scope, route, trip =
                if not (String.IsNullOrEmpty row.[3]) then "trip", null, box row.[3]
                elif not (String.IsNullOrEmpty row.[2]) then "route", box row.[2], null
                else "source", null, null
            Some (objectRow [
                "assignment_id", box (Identity.bindingId "note-assignment" [ "note", row.[0]; "scope", scope; "route", row.[2]; "trip", row.[3] ])
                "note_id", box row.[0]; "scope", box scope; "route_id", route; "trip_id", trip; "service_id", null ]))
        let features =
            read "source_trip_feature_metadata.parquet" [| "gtfs_trip_id"; "source_code"; "feature_kind"; "source_object_id" |]
            |> Seq.map (fun row -> objectRow [
                "feature_id", box (Identity.bindingId "service-feature" [ "trip", row.[0]; "code", row.[1]; "source", sourceId ])
                "scope", box "trip"; "kind", box row.[2]; "route_id", null; "trip_id", box row.[0]
                "call_sequence", null; "service_id", null; "source_code", box row.[1]; "note_id", null
                "source_id", box sourceId; "source_snapshot_sha256", box digest; "source_object_id", box row.[3] ])
        let locationFeatures =
            read "source_location_feature_metadata.parquet" [| "gtfs_stop_id"; "source_code"; "feature_kind"; "source_object_id" |]
            |> Seq.map (fun row -> objectRow [
                "feature_id", box (Identity.bindingId "location-feature" [ "location", row.[0]; "code", row.[1]; "source", sourceId ])
                "location_id", box row.[0]; "kind", box row.[2]; "source_code", box row.[1]
                "source_id", box sourceId; "source_snapshot_sha256", box digest; "source_object_id", box row.[3] ])
        let transferFacts =
            read "source_transfer_metadata.parquet"
                [| "source_transfer_id"; "gtfs_trip_id"; "source_route_stop_id"; "transfer_type"; "transfer_route_id"; "transfer_stop_id"; "transfer_stop_post_id"; "transfer_end_stop_id"; "transfer_end_stop_post_id"; "wait_minutes"; "note" |]
            |> Seq.toArray
        let transferCalls =
            let wanted =
                transferFacts
                |> Seq.map (fun row -> struct(row.[1], row.[2]))
                |> HashSet
            let result = Dictionary<struct(string * string), int>()
            match nativeCalls with
            | Some native ->
                for struct(trip, routeStop) as key in wanted do
                    match native.transferSequences.TryGetValue(struct(trip, integer64 routeStop)) with
                    | true, sequence -> result.Add(key, sequence)
                    | _ -> ()
            | None ->
                for call in (if wanted.Count = 0 then Seq.empty else callFacts ()) do
                    let key = struct(call.[0], call.[2])
                    if wanted.Contains(key) && not (result.ContainsKey(key)) then
                        result.Add(key, integer call.[1])
            result
        let transfers =
            transferFacts |> Seq.map (fun row ->
                let key = struct(row.[1], row.[2])
                let sequence = match transferCalls.TryGetValue(key) with | true, value -> value | _ -> 0
                objectRow [
                    "connection_id", box row.[0]; "direction", box row.[3]; "origin_trip_id", box row.[1]; "origin_sequence", box sequence
                    "service_id", null; "target_source_route_id", nullableString row.[4]; "target_source_trip_id", null
                    "target_source_stop_id", nullableString row.[5]; "target_source_post_id", nullableString row.[6]
                    "target_source_end_stop_id", nullableString row.[7]; "target_source_end_post_id", nullableString row.[8]
                    "wait_minutes", nullableParsed integer row.[9]; "note", nullableString row.[10]; "target_public_line", null; "target_destination_text", null
                    "target_derivation", box "jdf_structured"; "resolution_status", box "unresolved"; "target_route_id", null; "target_trip_id", null; "target_location_id", null
                    "source_id", box sourceId; "source_snapshot_sha256", box digest; "source_object_id", box row.[0] ])
        let restrictions =
            readOptional [ "source_route_version" ] "source_travel_restriction_metadata.parquet"
                [| "assignment_scope"; "gtfs_route_id"; "gtfs_trip_id"; "source_route_stop_id"; "group_code"; "source_route_version" |]
            |> Seq.map (fun row ->
                let routeStop = if String.IsNullOrEmpty row.[1] then null else box (Identity.routeStopKey row.[1] row.[5] row.[3])
                objectRow [
                    "assignment_id", box (Identity.bindingId "restriction" [ "scope", row.[0]; "route", row.[1]; "trip", row.[2]; "route_stop", row.[3]; "group", row.[4] ])
                    "scope", box row.[0]; "route_id", nullableString row.[1]; "trip_id", nullableString row.[2]
                    "source_route_stop_id", box row.[3]; "route_stop_id", routeStop; "call_sequence", null; "service_id", null
                    "group_code", box row.[4]; "source_id", box sourceId; "source_snapshot_sha256", box digest
                    "source_object_id", box (Identity.compositeKey [ row.[0]; row.[1]; row.[2]; row.[3]; row.[4] ]) ])
        Map [
            "route_stop", routeStops
            "route_stop_zone", routeStopZones
            "fare_zone", semanticZones
            "service_note", notes
            "service_note_assignment", noteAssignments
            "service_feature_assignment", features
            "location_feature", locationFeatures
            "connection_claim", transfers
            "travel_restriction_assignment", restrictions
        ]

    let internal czpttRelations (input: CompilerOutput.Output) (bindings: TripBinding array) (manifest: JsonElement) =
        let read name columns =
            CompilerOutput.values input.sidecars (Path.GetFileNameWithoutExtension(name: string)) columns
        let snapshots = combinedSourceManifest input manifest
        let sourceId = defaultSourceId manifest snapshots
        let digest = match snapshots.TryGetValue(sourceId) with | true, value -> value | _ -> String.replicate 64 "0"
        let operationalCallsRaw =
            read "operational_calls.parquet"
                [| "source_pa_id"; "source_sequence"; "source_location_id"; "passenger_call"; "arrival_seconds"; "departure_seconds"; "subsidiary_code"; "subsidiary_name"; "active_line_code" |]
            |> Seq.toArray
        let locations =
            read "operational_points.parquet"
                [| "source_location_id"; "country_code"; "primary_code"; "source_name"; "latitude"; "longitude"; "coordinate_source"; "coordinate_source_object_id"; "coordinate_match_method" |]
            |> Seq.map (fun row -> objectRow [
                "source_id", box sourceId; "source_location_id", box row.[0]; "source_snapshot_sha256", box digest
                "country_code", box row.[1]; "primary_code", box row.[2]; "name", box row.[3]
                "latitude", nullableParsed number row.[4]; "longitude", nullableParsed number row.[5]
                "coordinate_source", nullableString row.[6]; "coordinate_source_object_id", nullableString row.[7]; "coordinate_match_method", nullableString row.[8] ])
        let journeys =
            operationalCallsRaw |> Seq.map (fun row -> row.[0]) |> Seq.distinct |> Seq.map (fun pa -> objectRow [
                "source_id", box sourceId; "source_journey_id", box pa; "source_snapshot_sha256", box digest; "domain", box "czptt"; "mode", box "rail" ])
        let operationalCalls = operationalCallsRaw |> Seq.map (fun row -> objectRow [
            "source_id", box sourceId; "source_journey_id", box row.[0]; "sequence", box (integer row.[1]); "source_location_id", box row.[2]
            "passenger_service", box (Boolean.Parse row.[3]); "scheduled_arrival", nullableParsed integer row.[4]; "scheduled_departure", nullableParsed integer row.[5]
            "scheduled_passage", null; "subsidiary_code", nullableString row.[6]; "subsidiary_name", nullableString row.[7]; "active_line_code", nullableString row.[8] ])
        let paBindings =
            bindings |> Seq.filter (fun row -> row.trip_namespace = "czptt_pa_id")
            |> Seq.groupBy (fun row -> row.source_trip_id, row.trip_id)
            |> Seq.map (fun (key, values) -> key, values |> Seq.toArray) |> dict
        let operationalLocationByCall =
            operationalCallsRaw |> Seq.map (fun row -> struct(row.[0], row.[1]), row.[2]) |> dict
        let sourceCalls =
            read "source_call_metadata.parquet" [| "gtfs_trip_id"; "stop_sequence"; "source_pa_id"; "source_sequence" |]
            |> Seq.collect (fun row ->
                match paBindings.TryGetValue((row.[2], row.[0])) with
                | false, _ -> Seq.empty
                | true, candidates -> candidates |> Seq.map (fun binding -> objectRow [
                    "binding_id", box binding.binding_id; "call_namespace", box "czptt_pa_sequence"; "source_sequence", box row.[3]
                    "call_sequence", box (integer row.[1]); "source_stop_id", (match operationalLocationByCall.TryGetValue(struct(row.[2], row.[3])) with | true, value -> box value | _ -> null)
                    "scheduled_arrival", null; "scheduled_departure", null ]))
        let coverageFacts =
            read "source_ids_coverage_metadata.parquet"
                [| "source_coverage_id"; "source_pa_id"; "record_type"; "ids_system_id"; "coverage_role"; "from_location_code"; "from_occurrence"; "to_location_code"; "to_occurrence" |]
            |> Seq.map (fun row -> row.[0], row) |> dict
        let callsByPa =
            operationalCallsRaw |> Seq.groupBy (fun row -> row.[0])
            |> Seq.map (fun (pa, rows) -> pa, rows |> Seq.sortBy (fun row -> integer row.[1]) |> Seq.toArray) |> dict
        let endpoint (calls: string array array) (code: string) (occurrence: string) fallback =
            let matching = calls |> Array.filter (fun row ->
                let primary = row.[2].Split(':') |> Array.last
                String.Equals(primary, code, StringComparison.OrdinalIgnoreCase)
                || String.Equals(row.[2], code, StringComparison.OrdinalIgnoreCase))
            let index = match Int32.TryParse occurrence with | true, value when value > 0 -> value - 1 | _ -> fallback matching
            matching |> Array.tryItem index |> Option.map (fun row -> integer row.[1])
        let coverage =
            read "source_ids_coverage_trip_metadata.parquet" [| "source_coverage_id"; "gtfs_trip_id" |]
            |> Seq.collect (fun row ->
                match coverageFacts.TryGetValue(row.[0]) with
                | false, _ -> Seq.empty
                | true, fact ->
                    match paBindings.TryGetValue((fact.[1], row.[1])), callsByPa.TryGetValue(fact.[1]) with
                    | (true, candidates), (true, calls) when calls.Length > 0 ->
                        let first, last =
                            if fact.[2] = "CZIPTS" then
                                endpoint calls fact.[5] fact.[6] (fun _ -> 0), endpoint calls fact.[7] fact.[8] (fun values -> values.Length - 1)
                            else Some (integer calls.[0].[1]), Some (integer calls.[calls.Length - 1].[1])
                        match first, last with
                        | Some fromSequence, Some toSequence when fromSequence <= toSequence -> candidates |> Seq.map (fun binding -> objectRow [
                            "binding_id", box binding.binding_id; "coverage_id", box row.[0]; "service_id", box binding.service_id
                            "from_sequence", box fromSequence; "to_sequence", box toSequence; "coverage_type", box (if fact.[2] = "CZIPTS" then "segment" else "calendar")
                            "system_id", nullableString fact.[3]; "coverage_role", nullableString fact.[4] ])
                        | _ -> Seq.empty
                    | _ -> Seq.empty)
        let noteRows =
            read "source_note_metadata.parquet"
                [| "source_note_id"; "source_pa_id"; "note_kind"; "source_code"; "gtfs_trip_id"; "label"; "raw_value"; "valid_from"; "valid_to"; "resolved" |]
            |> Seq.toArray
        let notes =
            noteRows
            |> Seq.distinctBy (fun row -> row.[0])
            |> Seq.map (fun row -> objectRow [
                "note_id", box row.[0]; "kind", box row.[2]
                "label", nullableString row.[5]; "text", box row.[6]
                "valid_from", nullableParsed date row.[7]; "valid_to", nullableParsed date row.[8]
                "service_note_type", nullableString row.[3]
                "source_id", box sourceId; "source_snapshot_sha256", box digest
                "source_object_id", box row.[0] ])
        let noteAssignments =
            noteRows
            |> Seq.map (fun row ->
                let trip = row.[4]
                let scope = if String.IsNullOrWhiteSpace(trip) then "source" else "trip"
                objectRow [
                    "assignment_id", box (Identity.bindingId "note-assignment" [ "note", row.[0]; "scope", scope; "trip", trip ])
                    "note_id", box row.[0]; "scope", box scope; "route_id", null
                    "trip_id", nullableString trip; "service_id", null ])
        let features =
            read "source_feature_metadata.parquet"
                [| "source_feature_id"; "gtfs_trip_id"; "call_sequence"; "source_code"; "feature_kind"; "note_id"; "source_object_id" |]
            |> Seq.map (fun row -> objectRow [
                "feature_id", box row.[0]
                "scope", box (if String.IsNullOrWhiteSpace(row.[2]) then "trip" else "call")
                "kind", box row.[4]; "route_id", null; "trip_id", box row.[1]
                "call_sequence", nullableParsed integer row.[2]; "service_id", null
                "source_code", box row.[3]; "note_id", nullableString row.[5]
                "source_id", box sourceId; "source_snapshot_sha256", box digest
                "source_object_id", box row.[6] ])
        Map [
            "operational_location", locations
            "operational_journey", journeys
            "operational_call", operationalCalls
            "source_trip_coverage", coverage
            "service_note", notes
            "service_note_assignment", noteAssignments
            "service_feature_assignment", features
        ], sourceCalls
