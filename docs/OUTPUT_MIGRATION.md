# Output migration

The production output contract is a clean break. There is no legacy-output flag and Oběhy serving v1 compatibility is not claimed.

`regional-gtfs-overlay` accepts one or more repeated `--source` and `--source-descriptor` bindings. `jdf-to-bundle` and `czptt-to-bundle` retain their command names but write `jrutil-production` packages.

Removed production families include `gtfs-intermediate/`, `cz_routes.txt`, `cz_trips.txt`, `cz_stops.txt`, all `base_to_output_*` and `source_to_output_*` CSV files, `operational_to_source_trips.csv`, copied policy/descriptor/base-evidence trees, and physical report CSVs. Their supported information is projected into standard GTFS or the serving relations. Matching explanations and compiler traces require an explicit diagnostic output.

Consumers must validate `bundle_format`, all schema versions, the closed file inventory, Parquet metadata, keys, hashes, and semantic applicability before activation. Legacy packages are rejected with an instruction to rebuild. `validate-package` performs production validation. `compare-packages --byte-identical` checks reproducibility. `compare-packages --semantic` checks meaning-level equality of two packages.

JDF output routes now group all versions of a CIS line (`jdf:route:<line>`, with a separate `jdf:route:<line>:detour` for výluka timetables) instead of emitting `jdf:route:<line>:<distinction>` per version. JDF route-stop keys include the version distinction. Serving `route` gains the nullable `timetable_kind` (`regular` | `detour`); it is null for non-JDF routes. JDF service is bounded to the GVD recorded in `service_horizon`.

JDF trip ids are `jdf:trip:<line>:<yymmdd>[:det][:<hash>][:pN]:<trip>`, keyed by the version's published validity start instead of the merge-assigned distinction, so the same schedule keeps its trip ids between runs. Detour routes keep the line's own colours (no amber text) and carry `route_desc` "Výlukový jízdní řád".

## Bundle version 2 (serving schema 3)

- `extensions/` is removed. The four former public extensions (`cz_zones`, `cz_route_stop_zones`, `cz_call_zones`, `cz_transfer_constraints`) are already fully represented by `fare_zone`, `location_zone`, `route_stop_zone`, `call_zone` and `transfer`. `extension_schema_version` is gone from the manifest.
- `selected_field_provenance` is removed. Provenance is kept at trip and route level through `object_origin`, `source_trip_map` and `source_entity_map`.
- Relations no longer have a declared sort key and are not sorted. Primary keys stay unique and are validated with a hash check.
- Compilers hand the package writer in-memory tables; there is no compiler staging directory and no `--migration-audit`.
- The overlay requires a production base package and records `--converter-version` as the manifest `compiler`, like `jdf-to-bundle`.
- Removed options: `--stop-ids-cis`, `--international-route-overrides`, `--sr70-name20`, `--cache`, `--by-id`, `--audit-date`, `--policy-grid`, `--block-mode`, `--stop-coords-by-id`. Removed commands: `jdf-to-gtfs`, `czptt-to-gtfs`, `regional-gtfs-overlay-all`, `jdf-validate-post-inference`, `jdf-replay-post-inference`.

## Bundle version 3 (serving schema 4)

- Zones are codes with an optional `zone_system`; `fare_system` and `fare_zone` are removed. `route_stop_zone` is keyed by the new route stop slots and holds a slot's zones when all its calls agree; `call_zone` holds only the calls at slots without zones; `location_zone` is the distinct union per location. JDF Zaslinky tokens, CZPTT per-call zones and IDS JMK overlay stop zones all go through this rule. GTFS `stops.zone_id` is unchanged.
- `route_stop` is the merged, ordered stop list per route direction (`route_id, route_stop_id, direction, sequence, location_id`), derived from the final calls of every producer. `trip_call.route_stop_id` is filled for JDF, overlay and CZPTT calls. The previous JDF version-scoped keys are replaced. Route-scoped travel restrictions get one row per reached slot.
- Removed relations: `route_segment` and `identifier_alias` (never written), `object_origin` and `binding_evidence` (provenance is in `source_trip_map` and `source_entity_map`).
- `source_trip_map` loses `source_run_id` and `source_duty_id`, which no source filled.
- `validate-package` checks every declared foreign key.

Oběhy accepts only bundle version 3 with serving schema version 4.

## Serving schema 5.0

- The serving version is `major.minor` (`"5.0"`); consumers accept any minor of their major.
- 30 relations become 19. `source_trip_map`, `source_entity_map`, `road_route_key`, `road_trip_key` and `rail_trip_key` become `source_key` with source-qualified namespaces and no identity rows; `source_call_map` becomes `call_key` with differing sequences only; `service_note_assignment`, `service_feature_assignment` and `location_feature` become `assignment`; `travel_restriction_assignment` is renamed `travel_restriction`; `operational_location`, `operational_journey` and `operational_call` become `trip_call` railway points and `operational_point` locations; `location_zone`, `source_trip_coverage` and binding/call-pattern hashes are removed.
- Generated ids are feed-prefixed (`jdf:service:…`, `czptt:service:…`, `jdf:transfer:…`, …). Trip, route and stop ids are unchanged.
- Coded fields use closed enumerations; JDF attribute codes are typed assignment kinds.
- `gtfs.zip` is projected from the relations; exception-only services gain a `calendar.txt` row with no weekdays, `platform_code` is filled and `stops.zone_id` is no longer written.
- The CZPTT manifest records the input digest.
