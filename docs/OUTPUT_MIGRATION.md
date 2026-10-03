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

- Zones are call-scoped only. `fare_system`, `fare_zone`, `location_zone` and `route_stop_zone` are removed. `call_zone` is `(trip_id, sequence, source_order, zone_code, zone_system)`. JDF writes its Zaslinky zone tokens onto every served call; the overlay writes IDS JMK stop zones onto the calls at those places. GTFS `stops.zone_id` is unchanged.
- `route_stop` is the merged, ordered stop list per route direction (`route_id, route_stop_id, direction, sequence, location_id`), derived from the final calls of every producer. `trip_call.route_stop_id` is filled for JDF, overlay and CZPTT calls. The previous JDF version-scoped keys are replaced. Route-scoped travel restrictions get one row per reached slot.
- Removed relations: `route_segment` and `identifier_alias` (never written), `object_origin` and `binding_evidence` (provenance is in `source_trip_map` and `source_entity_map`).
- `source_trip_map` loses `source_run_id` and `source_duty_id`, which no source filled.
- `validate-package` checks every declared foreign key.

Oběhy accepts only bundle version 3 with serving schema version 4.
