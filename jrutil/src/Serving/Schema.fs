// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

/// The production package contract is intentionally owned by JrUtil.  Consumers
/// validate this version rather than importing JrUtil's implementation types.
module Schema =
    [<Literal>]
    let BundleFormat = "jrutil-production"

    [<Literal>]
    let BundleVersion = 1

    [<Literal>]
    let ServingSchemaVersion = 2

    [<Literal>]
    let ExtensionSchemaVersion = 2

    [<Literal>]
    let DiagnosticsSchemaVersion = 1

    type FieldType =
        | Text
        | Int16
        | Int32
        | Int64
        | Float64
        | Boolean
        | Date

    type Field = {
        name: string
        dataType: FieldType
        nullable: bool
    }

    type ForeignKey = {
        fields: string array
        relation: string
        targetFields: string array
    }

    type Relation = {
        name: string
        fields: Field array
        primaryKey: string array
        sortKey: string array
        foreignKeys: ForeignKey array
    }

    let private f name dataType nullable = { name = name; dataType = dataType; nullable = nullable }
    let private required name kind = f name kind false
    let private optional name kind = f name kind true
    let private fk fields relation targets = { fields = fields; relation = relation; targetFields = targets }
    let private relation name fields key foreignKeys = {
        name = name
        fields = fields
        primaryKey = key
        sortKey = key
        foreignKeys = foreignKeys
    }

    let relations = [|
        relation "agency" [| required "agency_id" Text; required "name" Text; optional "url" Text; required "timezone" Text; optional "language" Text; optional "phone" Text; optional "fare_url" Text; optional "email" Text |] [| "agency_id" |] [||]
        relation "location" [| required "location_id" Text; required "kind" Text; required "domain" Text; optional "parent_location_id" Text; required "name" Text; optional "public_code" Text; optional "description" Text; optional "municipality_name" Text; optional "district_name" Text; optional "district_code" Text; optional "nearby_place" Text; optional "country_code" Text; optional "coordinate_precision" Text; optional "longitude" Float64; optional "latitude" Float64; optional "url" Text; optional "timezone" Text; optional "wheelchair_boarding" Int16 |] [| "location_id" |] [| fk [|"parent_location_id"|] "location" [|"location_id"|] |]
        relation "route" [| required "route_id" Text; required "agency_id" Text; required "mode" Text; required "gtfs_route_type" Int32; optional "short_name" Text; optional "long_name" Text; optional "description" Text; optional "url" Text; optional "color" Text; optional "text_color" Text; optional "sort_order" Int32 |] [| "route_id" |] [| fk [|"agency_id"|] "agency" [|"agency_id"|] |]
        relation "service_calendar" [| required "service_id" Text; required "valid_from" Date; required "valid_to" Date; required "weekday_mask" Int16 |] [| "service_id" |] [||]
        relation "service_exception" [| required "service_id" Text; required "service_date" Date; required "added" Boolean |] [| "service_id"; "service_date" |] [| fk [|"service_id"|] "service_calendar" [|"service_id"|] |]
        relation "shape" [| required "shape_id" Text; required "generation_method" Text |] [| "shape_id" |] [||]
        relation "shape_point" [| required "shape_id" Text; required "sequence" Int32; required "longitude" Float64; required "latitude" Float64; optional "distance_traveled" Float64 |] [| "shape_id"; "sequence" |] [| fk [|"shape_id"|] "shape" [|"shape_id"|] |]
        relation "trip" [| required "trip_id" Text; required "route_id" Text; required "service_id" Text; optional "direction" Int16; optional "headsign" Text; optional "short_name" Text; optional "block_key" Text; optional "wheelchair_accessible" Int16; optional "bikes_allowed" Int16; optional "shape_id" Text |] [| "trip_id" |] [| fk [|"route_id"|] "route" [|"route_id"|]; fk [|"service_id"|] "service_calendar" [|"service_id"|]; fk [|"shape_id"|] "shape" [|"shape_id"|] |]
        relation "trip_call" [| required "trip_id" Text; required "sequence" Int32; required "location_id" Text; required "passenger_service" Boolean; optional "boarding_point_id" Text; optional "route_stop_id" Text; optional "scheduled_arrival" Int32; optional "scheduled_departure" Int32; optional "scheduled_passage" Int32; required "pickup_type" Int16; required "dropoff_type" Int16; required "timepoint" Boolean; optional "stop_headsign" Text; optional "shape_distance_traveled" Float64 |] [| "trip_id"; "sequence" |] [| fk [|"trip_id"|] "trip" [|"trip_id"|]; fk [|"location_id"|] "location" [|"location_id"|]; fk [|"boarding_point_id"|] "location" [|"location_id"|] |]
        relation "route_segment" [| required "trip_id" Text; required "from_sequence" Int32; required "to_sequence" Int32; required "route_id" Text |] [| "trip_id"; "from_sequence" |] [| fk [|"trip_id"|] "trip" [|"trip_id"|]; fk [|"route_id"|] "route" [|"route_id"|] |]
        relation "transfer" [| required "transfer_key" Text; required "from_location_id" Text; required "to_location_id" Text; optional "from_route_id" Text; optional "to_route_id" Text; optional "from_trip_id" Text; optional "to_trip_id" Text; required "transfer_type" Int16; optional "minimum_transfer_time" Int32; optional "maximum_waiting_time" Int32 |] [| "transfer_key" |] [||]
        relation "fare_system" [| required "fare_system_id" Text; required "name" Text |] [| "fare_system_id" |] [||]
        relation "fare_zone" [| required "zone_id" Text; optional "fare_system_id" Text; required "zone_code" Text; optional "name" Text; required "source_id" Text; required "source_scope" Text |] [| "zone_id" |] [| fk [|"fare_system_id"|] "fare_system" [|"fare_system_id"|] |]
        relation "location_zone" [| required "location_id" Text; required "zone_id" Text |] [| "location_id"; "zone_id" |] [| fk [|"location_id"|] "location" [|"location_id"|]; fk [|"zone_id"|] "fare_zone" [|"zone_id"|] |]
        relation "call_zone" [| required "trip_id" Text; required "sequence" Int32; required "zone_id" Text; required "source_order" Int32 |] [| "trip_id"; "sequence"; "zone_id" |] [| fk [|"trip_id";"sequence"|] "trip_call" [|"trip_id";"sequence"|]; fk [|"zone_id"|] "fare_zone" [|"zone_id"|] |]
        relation "service_note" [| required "note_id" Text; required "kind" Text; optional "label" Text; optional "text" Text; optional "valid_from" Date; optional "valid_to" Date; optional "service_note_type" Text; required "source_id" Text; required "source_snapshot_sha256" Text; required "source_object_id" Text |] [| "note_id" |] [||]
        relation "service_note_assignment" [| required "assignment_id" Text; required "note_id" Text; required "scope" Text; optional "route_id" Text; optional "trip_id" Text; optional "service_id" Text |] [| "assignment_id" |] [| fk [|"note_id"|] "service_note" [|"note_id"|] |]
        relation "service_feature_assignment" [| required "feature_id" Text; required "scope" Text; required "kind" Text; optional "route_id" Text; optional "trip_id" Text; optional "call_sequence" Int32; optional "service_id" Text; required "source_code" Text; optional "note_id" Text; required "source_id" Text; required "source_snapshot_sha256" Text; required "source_object_id" Text |] [| "feature_id" |] [||]
        relation "location_feature" [| required "feature_id" Text; required "location_id" Text; required "kind" Text; required "source_code" Text; required "source_id" Text; required "source_snapshot_sha256" Text; required "source_object_id" Text |] [| "feature_id" |] [| fk [|"location_id"|] "location" [|"location_id"|] |]
        relation "connection_claim" [| required "connection_id" Text; required "direction" Text; required "origin_trip_id" Text; required "origin_sequence" Int32; optional "service_id" Text; optional "target_source_route_id" Text; optional "target_source_trip_id" Text; optional "target_source_stop_id" Text; optional "target_source_post_id" Text; optional "target_source_end_stop_id" Text; optional "target_source_end_post_id" Text; optional "wait_minutes" Int32; optional "note" Text; optional "target_public_line" Text; optional "target_destination_text" Text; required "target_derivation" Text; required "resolution_status" Text; optional "target_route_id" Text; optional "target_trip_id" Text; optional "target_location_id" Text; required "source_id" Text; required "source_snapshot_sha256" Text; required "source_object_id" Text |] [| "connection_id" |] [||]
        relation "travel_restriction_assignment" [| required "assignment_id" Text; required "scope" Text; optional "route_id" Text; optional "trip_id" Text; required "source_route_stop_id" Text; optional "route_stop_id" Text; optional "call_sequence" Int32; optional "service_id" Text; required "group_code" Text; required "source_id" Text; required "source_snapshot_sha256" Text; required "source_object_id" Text |] [| "assignment_id" |] [||]
        relation "operational_location" [| required "source_id" Text; required "source_location_id" Text; required "source_snapshot_sha256" Text; required "country_code" Text; required "primary_code" Text; required "name" Text; optional "longitude" Float64; optional "latitude" Float64; optional "coordinate_source" Text; optional "coordinate_source_object_id" Text; optional "coordinate_match_method" Text |] [| "source_id"; "source_location_id" |] [||]
        relation "operational_journey" [| required "source_id" Text; required "source_journey_id" Text; required "source_snapshot_sha256" Text; required "domain" Text; required "mode" Text |] [| "source_id"; "source_journey_id" |] [||]
        relation "operational_call" [| required "source_id" Text; required "source_journey_id" Text; required "sequence" Int32; required "source_location_id" Text; required "passenger_service" Boolean; optional "scheduled_arrival" Int32; optional "scheduled_departure" Int32; optional "scheduled_passage" Int32; optional "subsidiary_code" Text; optional "subsidiary_name" Text; optional "active_line_code" Text |] [| "source_id"; "source_journey_id"; "sequence" |] [||]
        relation "source_entity_map" [| required "entity_binding_id" Text; required "source_id" Text; required "identifier_namespace" Text; required "entity_kind" Text; required "source_object_id" Text; required "public_id" Text; required "valid_from" Date; required "valid_to" Date |] [| "entity_binding_id" |] [||]
        relation "source_trip_map" [| required "binding_id" Text; required "source_id" Text; required "trip_namespace" Text; required "source_trip_id" Text; required "trip_id" Text; required "service_id" Text; required "valid_from" Date; required "valid_to" Date; required "binding_status" Text; optional "scheduled_start" Int32; optional "scheduled_end" Int32; optional "source_route_id" Text; optional "source_direction_id" Text; optional "source_start_location_id" Text; optional "source_end_location_id" Text; optional "source_block_id" Text; optional "source_run_id" Text; optional "source_duty_id" Text; optional "call_pattern_sha256" Text; optional "variant_key" Text |] [| "binding_id" |] [| fk [|"trip_id"|] "trip" [|"trip_id"|]; fk [|"service_id"|] "service_calendar" [|"service_id"|] |]
        relation "source_call_map" [| required "binding_id" Text; required "call_namespace" Text; required "source_sequence" Text; required "call_sequence" Int32; optional "source_stop_id" Text; optional "scheduled_arrival" Int32; optional "scheduled_departure" Int32 |] [| "binding_id"; "call_namespace"; "source_sequence"; "call_sequence" |] [| fk [|"binding_id"|] "source_trip_map" [|"binding_id"|] |]
        relation "source_trip_coverage" [| required "binding_id" Text; required "coverage_id" Text; required "service_id" Text; required "from_sequence" Int32; required "to_sequence" Int32; required "coverage_type" Text; optional "system_id" Text; optional "coverage_role" Text |] [| "binding_id"; "coverage_id"; "from_sequence"; "to_sequence" |] [| fk [|"binding_id"|] "source_trip_map" [|"binding_id"|] |]
        relation "identifier_alias" [| required "source_id" Text; required "namespace" Text; required "observed_id" Text; required "valid_from" Date; required "valid_to" Date; required "canonical_value" Text; required "reason" Text |] [| "source_id"; "namespace"; "observed_id"; "valid_from" |] [||]
        relation "road_route_key" [| required "entity_binding_id" Text; required "cis_line_id" Text; required "route_id" Text; required "valid_from" Date; required "valid_to" Date |] [| "entity_binding_id"; "cis_line_id" |] [||]
        relation "road_trip_key" [| required "binding_id" Text; required "cis_line_id" Text; required "cis_trip_id" Int64; required "trip_id" Text; required "valid_from" Date; required "valid_to" Date |] [| "binding_id"; "cis_line_id"; "cis_trip_id" |] [||]
        relation "rail_trip_key" [| required "binding_id" Text; required "train_number" Text; required "trip_id" Text; required "valid_from" Date; required "valid_to" Date |] [| "binding_id"; "train_number" |] [||]
        relation "selected_field_provenance" [| required "object_type" Text; required "object_key" Text; required "field_name" Text; required "source_id" Text; required "source_snapshot_sha256" Text; required "source_object_id" Text; required "selection_rule" Text |] [| "object_type"; "object_key"; "field_name"; "source_id"; "source_snapshot_sha256"; "source_object_id"; "selection_rule" |] [||]
        relation "object_origin" [| required "object_type" Text; required "object_key" Text; required "source_id" Text; required "source_snapshot_sha256" Text; required "identifier_namespace" Text; required "source_object_id" Text; required "selection_rule" Text |] [| "object_type"; "object_key" |] [||]
        relation "binding_evidence" [| required "binding_kind" Text; required "binding_id" Text; required "evidence_source_id" Text; required "source_snapshot_sha256" Text; required "identifier_namespace" Text; required "source_object_id" Text; required "selection_rule" Text |] [| "binding_kind"; "binding_id"; "evidence_source_id"; "source_snapshot_sha256"; "identifier_namespace"; "source_object_id"; "selection_rule" |] [||]
        relation "route_stop" [| required "route_id" Text; required "route_stop_id" Text; required "location_id" Text |] [| "route_id"; "route_stop_id" |] [| fk [|"route_id"|] "route" [|"route_id"|]; fk [|"location_id"|] "location" [|"location_id"|] |]
        relation "route_stop_zone" [| required "route_id" Text; required "route_stop_id" Text; required "zone_id" Text; required "source_order" Int32 |] [| "route_id"; "route_stop_id"; "zone_id" |] [| fk [|"route_id";"route_stop_id"|] "route_stop" [|"route_id";"route_stop_id"|]; fk [|"zone_id"|] "fare_zone" [|"zone_id"|] |]
    |]

    let relationNames = relations |> Array.map (fun value -> value.name)

    let extensions = [|
        "cz_zones.txt", [| "zone_id"; "zone_code"; "fare_system_id"; "source_id"; "source_scope" |], [| "zone_id" |]
        "cz_route_stop_zones.txt", [| "route_id"; "route_stop_id"; "stop_id"; "zone_id"; "source_order" |], [| "route_id"; "route_stop_id"; "zone_id" |]
        "cz_call_zones.txt", [| "trip_id"; "stop_sequence"; "zone_id"; "source_order" |], [| "trip_id"; "stop_sequence"; "zone_id" |]
        "cz_transfer_constraints.txt", [| "transfer_key"; "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id"; "max_waiting_time" |], [| "transfer_key" |]
    |]
