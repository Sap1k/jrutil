// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

/// The production package contract is intentionally owned by JrUtil.  Consumers
/// validate this version rather than importing JrUtil's implementation types.
module Schema =
    [<Literal>]
    let BundleFormat = "jrutil-production"

    [<Literal>]
    let BundleVersion = 3

    /// `major.minor`: a minor version only adds relations, trailing nullable
    /// fields, enumeration values or namespaces.
    [<Literal>]
    let ServingSchemaVersion = "5.0"

    [<Literal>]
    let ServingSchemaMajor = 5

    [<Literal>]
    let DiagnosticsSchemaVersion = 1

    /// The major of a `major.minor` version string, if it is one.
    let schemaMajor (version: string) =
        match (if isNull version then [||] else version.Split('.')) with
        | [| major; minor |] ->
            match System.Int32.TryParse(major), System.Int32.TryParse(minor) with
            | (true, value), (true, _) -> Some value
            | _ -> None
        | _ -> None

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
        /// Name of the closed value list in `enumerations`, or null.
        enumeration: string
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
        foreignKeys: ForeignKey array
    }

    type Namespace = { name: string; entityKind: string; pattern: string }

    let private f name dataType nullable = { name = name; dataType = dataType; nullable = nullable; enumeration = null }
    let private required name kind = f name kind false
    let private optional name kind = f name kind true
    let private coded name kind nullable enumeration = { f name kind nullable with enumeration = enumeration }
    let private fk fields relation targets = { fields = fields; relation = relation; targetFields = targets }
    let private relation name fields key foreignKeys = {
        name = name
        fields = fields
        primaryKey = key
        foreignKeys = foreignKeys
    }

    let relations = [|
        relation "agency" [| required "agency_id" Text; required "name" Text; optional "url" Text; required "timezone" Text; optional "language" Text; optional "phone" Text; optional "fare_url" Text; optional "email" Text |] [| "agency_id" |] [|  |]
        relation "location" [| required "location_id" Text; coded "kind" Text false "location_kind"; coded "domain" Text false "location_domain"; optional "parent_location_id" Text; required "name" Text; optional "public_code" Text; optional "description" Text; optional "municipality_name" Text; optional "district_name" Text; optional "district_code" Text; optional "nearby_place" Text; optional "country_code" Text; coded "coordinate_precision" Text false "coordinate_precision"; coded "coordinate_source" Text true "coordinate_source"; optional "longitude" Float64; optional "latitude" Float64; optional "url" Text; optional "timezone" Text; coded "wheelchair_boarding" Int16 true "gtfs_tristate" |] [| "location_id" |] [| fk [| "parent_location_id" |] "location" [| "location_id" |] |]
        relation "route" [| required "route_id" Text; required "agency_id" Text; coded "mode" Text false "route_mode"; required "gtfs_route_type" Int32; optional "short_name" Text; optional "long_name" Text; optional "description" Text; optional "url" Text; optional "color" Text; optional "text_color" Text; optional "sort_order" Int32; coded "timetable_kind" Text false "timetable_kind" |] [| "route_id" |] [| fk [| "agency_id" |] "agency" [| "agency_id" |] |]
        relation "service_calendar" [| required "service_id" Text; required "valid_from" Date; required "valid_to" Date; required "weekday_mask" Int16 |] [| "service_id" |] [|  |]
        relation "service_exception" [| required "service_id" Text; required "service_date" Date; required "added" Boolean |] [| "service_id"; "service_date" |] [| fk [| "service_id" |] "service_calendar" [| "service_id" |] |]
        relation "trip" [| required "trip_id" Text; required "route_id" Text; required "service_id" Text; optional "direction" Int16; optional "headsign" Text; optional "short_name" Text; optional "block_key" Text; optional "run_key" Text; optional "run_part" Int16; coded "wheelchair_accessible" Int16 true "gtfs_tristate"; coded "bikes_allowed" Int16 true "gtfs_tristate"; optional "shape_id" Text |] [| "trip_id" |] [| fk [| "route_id" |] "route" [| "route_id" |]; fk [| "service_id" |] "service_calendar" [| "service_id" |]; fk [| "shape_id" |] "shape" [| "shape_id" |] |]
        relation "trip_call" [| required "trip_id" Text; required "sequence" Int32; required "location_id" Text; required "passenger_service" Boolean; optional "boarding_point_id" Text; optional "route_stop_id" Text; optional "scheduled_arrival" Int32; optional "scheduled_departure" Int32; coded "pickup_type" Int16 false "gtfs_pickup_dropoff"; coded "dropoff_type" Int16 false "gtfs_pickup_dropoff"; required "timepoint" Boolean; optional "stop_headsign" Text; optional "shape_distance_traveled" Float64; optional "subsidiary_code" Text; optional "subsidiary_name" Text; optional "active_line_code" Text |] [| "trip_id"; "sequence" |] [| fk [| "trip_id" |] "trip" [| "trip_id" |]; fk [| "location_id" |] "location" [| "location_id" |]; fk [| "boarding_point_id" |] "location" [| "location_id" |]; fk [| "route_stop_id" |] "route_stop" [| "route_stop_id" |] |]
        relation "route_stop" [| required "route_stop_id" Text; required "route_id" Text; optional "direction" Int16; required "sequence" Int32; required "location_id" Text |] [| "route_stop_id" |] [| fk [| "route_id" |] "route" [| "route_id" |]; fk [| "location_id" |] "location" [| "location_id" |] |]
        relation "route_stop_zone" [| required "route_stop_id" Text; required "source_order" Int32; required "zone_code" Text; optional "zone_system" Text |] [| "route_stop_id"; "source_order" |] [| fk [| "route_stop_id" |] "route_stop" [| "route_stop_id" |] |]
        relation "call_zone" [| required "trip_id" Text; required "sequence" Int32; required "source_order" Int32; required "zone_code" Text; optional "zone_system" Text |] [| "trip_id"; "sequence"; "source_order" |] [| fk [| "trip_id"; "sequence" |] "trip_call" [| "trip_id"; "sequence" |] |]
        relation "shape" [| required "shape_id" Text; coded "generation_method" Text false "shape_generation" |] [| "shape_id" |] [|  |]
        relation "shape_point" [| required "shape_id" Text; required "sequence" Int32; required "longitude" Float64; required "latitude" Float64; optional "distance_traveled" Float64 |] [| "shape_id"; "sequence" |] [| fk [| "shape_id" |] "shape" [| "shape_id" |] |]
        relation "transfer" [| required "transfer_key" Text; required "from_location_id" Text; required "to_location_id" Text; optional "from_route_id" Text; optional "to_route_id" Text; optional "from_trip_id" Text; optional "to_trip_id" Text; coded "transfer_type" Int16 false "gtfs_transfer_type"; optional "minimum_transfer_time" Int32; optional "maximum_waiting_time" Int32 |] [| "transfer_key" |] [| fk [| "from_location_id" |] "location" [| "location_id" |]; fk [| "to_location_id" |] "location" [| "location_id" |]; fk [| "from_route_id" |] "route" [| "route_id" |]; fk [| "to_route_id" |] "route" [| "route_id" |]; fk [| "from_trip_id" |] "trip" [| "trip_id" |]; fk [| "to_trip_id" |] "trip" [| "trip_id" |] |]
        relation "service_note" [| required "note_id" Text; coded "kind" Text false "note_kind"; optional "label" Text; optional "text" Text; optional "valid_from" Date; optional "valid_to" Date; optional "service_note_type" Text; required "source_object_id" Text |] [| "note_id" |] [|  |]
        relation "assignment" [| required "assignment_id" Text; coded "scope" Text false "assignment_scope"; coded "kind" Text false "assignment_kind"; optional "route_id" Text; optional "trip_id" Text; optional "call_sequence" Int32; optional "call_sequence_to" Int32; optional "service_id" Text; optional "location_id" Text; optional "source_code" Text; optional "note_id" Text; optional "source_object_id" Text |] [| "assignment_id" |] [| fk [| "route_id" |] "route" [| "route_id" |]; fk [| "trip_id" |] "trip" [| "trip_id" |]; fk [| "trip_id"; "call_sequence" |] "trip_call" [| "trip_id"; "sequence" |]; fk [| "trip_id"; "call_sequence_to" |] "trip_call" [| "trip_id"; "sequence" |]; fk [| "service_id" |] "service_calendar" [| "service_id" |]; fk [| "location_id" |] "location" [| "location_id" |]; fk [| "note_id" |] "service_note" [| "note_id" |] |]
        relation "connection_claim" [| required "connection_id" Text; coded "direction" Text false "connection_direction"; required "origin_trip_id" Text; required "origin_sequence" Int32; optional "service_id" Text; optional "target_source_route_id" Text; optional "target_source_trip_id" Text; optional "target_source_stop_id" Text; optional "target_source_post_id" Text; optional "target_source_end_stop_id" Text; optional "target_source_end_post_id" Text; optional "wait_minutes" Int32; optional "note" Text; optional "target_public_line" Text; optional "target_destination_text" Text; coded "target_derivation" Text false "connection_derivation"; coded "resolution_status" Text false "connection_resolution"; optional "target_route_id" Text; optional "target_trip_id" Text; optional "target_location_id" Text; required "source_object_id" Text |] [| "connection_id" |] [| fk [| "origin_trip_id"; "origin_sequence" |] "trip_call" [| "trip_id"; "sequence" |]; fk [| "service_id" |] "service_calendar" [| "service_id" |]; fk [| "target_route_id" |] "route" [| "route_id" |]; fk [| "target_trip_id" |] "trip" [| "trip_id" |]; fk [| "target_location_id" |] "location" [| "location_id" |] |]
        relation "travel_restriction" [| required "restriction_id" Text; coded "scope" Text false "restriction_scope"; optional "route_id" Text; optional "trip_id" Text; required "source_route_stop_id" Text; optional "route_stop_id" Text; optional "call_sequence" Int32; optional "service_id" Text; coded "group_code" Text false "restriction_group"; required "source_object_id" Text |] [| "restriction_id" |] [| fk [| "route_id" |] "route" [| "route_id" |]; fk [| "trip_id" |] "trip" [| "trip_id" |]; fk [| "route_stop_id" |] "route_stop" [| "route_stop_id" |]; fk [| "trip_id"; "call_sequence" |] "trip_call" [| "trip_id"; "sequence" |]; fk [| "service_id" |] "service_calendar" [| "service_id" |] |]
        relation "source_key" [| coded "entity_kind" Text false "entity_kind"; coded "namespace" Text false "namespace"; required "identifier" Text; required "public_id" Text; required "valid_from" Date; required "valid_to" Date; coded "binding_method" Text false "binding_method" |] [| "namespace"; "identifier"; "public_id"; "valid_from"; "valid_to" |] [|  |]
        relation "call_key" [| coded "namespace" Text false "namespace"; required "identifier" Text; required "source_sequence" Text; required "trip_id" Text; required "sequence" Int32 |] [| "namespace"; "identifier"; "trip_id"; "source_sequence" |] [| fk [| "trip_id"; "sequence" |] "trip_call" [| "trip_id"; "sequence" |] |]
    |]

    /// Closed value lists; int16 enumerations use decimal text keys.
    let enumerations: Map<string, string array> = Map [
        "location_kind", [| "stop_place"; "boarding_point"; "operational_point" |]
        "location_domain", [| "surface"; "heavy_rail" |]
        "coordinate_precision", [| "exact"; "estimated"; "missing" |]
        "coordinate_source", [| "sr70"; "osm"; "gapfill"; "catalogue"; "overlay"; "route_time"; "route_end" |]
        "route_mode", [| "bus"; "trolleybus"; "tram"; "metro"; "rail"; "water"; "cable"; "funicular" |]
        "timetable_kind", [| "regular"; "detour" |]
        "shape_generation", [| "source"; "compiler"; "motis" |]
        "note_kind", [| "route_information"; "timetable_note"; "reservation_note"; "czptt_central_note"; "czptt_local_note"; "czptt_calendar_note" |]
        "assignment_scope", [| "route"; "trip"; "call"; "call_range"; "service"; "location" |]
        "assignment_kind", [| "note"; "calendar_designation"; "reservation_available"; "reservation_required"; "wheelchair_accessible_vehicle"; "partly_wheelchair_accessible_vehicle"; "wheelchair_booking_required"; "wheelchair_booking_recommended"; "refreshments_on_vehicle"; "luggage_transport"; "bicycle_transport"; "bicycle_carry_on"; "bicycle_storage"; "bicycle_reservation_available"; "bicycle_reservation_required"; "bicycle_transport_prohibited"; "on_request"; "conditional"; "self_service_ticketing"; "integrated_transport"; "not_stopping"; "diversion"; "request_stop"; "exit_only"; "boarding_only"; "wheelchair_accessible"; "refreshments"; "toilet"; "accessible_toilet"; "urban_transport_interchange"; "border_control_only"; "visually_impaired_accessible"; "accessibility_terminal"; "rail_interchange"; "line_interchange"; "metro_interchange"; "ship_terminal"; "airport_nearby"; "park_and_ride" |]
        "connection_direction", [| "waits_for"; "connects_to" |]
        "connection_derivation", [| "none"; "structured"; "spec_note" |]
        "connection_resolution", [| "unresolved"; "pattern"; "resolved" |]
        "restriction_scope", [| "route_stop"; "trip_call" |]
        "restriction_group", [| "§"; "A"; "B"; "C" |]
        "entity_kind", [| "trip"; "route"; "location" |]
        "binding_method", [| "identity"; "source_native"; "authoritative_source_trip_set"; "structural_match"; "companion_assertion"; "operator_crosswalk" |]
        "gtfs_pickup_dropoff", [| "0"; "1"; "2"; "3" |]
        "gtfs_tristate", [| "0"; "1"; "2" |]
        "gtfs_transfer_type", [| "0"; "1"; "2"; "3"; "4"; "5" |]
        "namespace", [| "cis:line"; "cis:line_trip"; "czptt:train_number"; "czptt:pa"; "czptt:tr"; "pid:gtfs_trip_id"; "pid:gtfs_route_id"; "pid:gtfs_stop_id"; "ids-jmk:gtfs_trip_id"; "ids-jmk:gtfs_route_id"; "ids-jmk:gtfs_stop_id"; "ids-jmk:line_course" |]
    ]

    /// Source-qualified key namespaces with their identifier encodings.
    let namespaces = [|
        { name = "cis:line"; entityKind = "route"; pattern = "^[0-9]{6}$" }
        { name = "cis:line_trip"; entityKind = "trip"; pattern = "^[0-9]{6}:[1-9][0-9]*$" }
        { name = "czptt:train_number"; entityKind = "trip"; pattern = "^[0-9]+$" }
        { name = "czptt:pa"; entityKind = "trip"; pattern = "^.+$" }
        { name = "czptt:tr"; entityKind = "trip"; pattern = "^.+$" }
        { name = "pid:gtfs_trip_id"; entityKind = "trip"; pattern = "^.+$" }
        { name = "pid:gtfs_route_id"; entityKind = "route"; pattern = "^.+$" }
        { name = "pid:gtfs_stop_id"; entityKind = "location"; pattern = "^.+$" }
        { name = "ids-jmk:gtfs_trip_id"; entityKind = "trip"; pattern = "^.+$" }
        { name = "ids-jmk:gtfs_route_id"; entityKind = "route"; pattern = "^.+$" }
        { name = "ids-jmk:gtfs_stop_id"; entityKind = "location"; pattern = "^.+$" }
        { name = "ids-jmk:line_course"; entityKind = "trip"; pattern = "^[^/]+/[^/]+$" }
    |]

    let relationNames = relations |> Array.map (fun value -> value.name)

