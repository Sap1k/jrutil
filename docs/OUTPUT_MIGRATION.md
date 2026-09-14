# Output migration

The production output contract is a clean break. There is no legacy-output flag and Oběhy serving v1 compatibility is not claimed.

`regional-gtfs-overlay` accepts one or more repeated `--source` and `--source-descriptor` bindings. `regional-gtfs-overlay-all` exits nonzero and names that replacement. `jdf-to-bundle` and `czptt-to-bundle` retain their command names but write `jrutil-production` packages. Expanded output remains available only from the standalone `*-to-gtfs` tools.

Removed production families include `gtfs-intermediate/`, `cz_routes.txt`, `cz_trips.txt`, `cz_stops.txt`, all `base_to_output_*` and `source_to_output_*` CSV files, `operational_to_source_trips.csv`, copied policy/descriptor/base-evidence trees, and physical report CSVs. Their supported information is projected into standard GTFS, the four public extensions, or serving v2. Matching explanations and compiler traces require an explicit diagnostic output.

Consumers must validate `bundle_format`, all schema versions, the closed file inventory, Parquet metadata, keys, hashes, and semantic applicability before activation. Legacy packages are rejected with an instruction to rebuild. `validate-package` performs production validation. `compare-packages --byte-identical` checks reproducibility. `compare-packages --migration-audit` validates the target package and establishes the explicit legacy migration boundary.

Oběhy needs a coordinated serving-v2 importer, indexes, and central resolver before activation. Until then it must reject these packages. No Oběhy source is changed by this migration.
