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

Oběhy accepts only bundle version 2 with serving schema version 3.
