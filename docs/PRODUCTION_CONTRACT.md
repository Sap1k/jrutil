# JrUtil production package contract

JrUtil production packages use `bundle_format: jrutil-production`, bundle version 3 and serving schema version 4. The checked-in JSON contracts, [production-v3.json](../contracts/production-v3.json) and [serving-v4.json](../contracts/serving-v4.json), are normative. The F# schema declarations and writer are tested against them.

The package inventory is closed:

```text
output/
├── gtfs.zip
├── serving/
│   └── <28 declared relations>.parquet
├── manifest.json
└── diagnostics.json
```

`gtfs.zip` is the only GTFS representation, and it contains only standard GTFS. Its entries are flat, ordered by ordinal file name, and have a fixed ZIP timestamp. `transfers.txt` contains only standard GTFS fields. Transfer waiting limits live in the serving relation `transfer`.

All serving Parquet relations exist even when empty. Their exact field order, types, nullability, primary keys and foreign keys are in [serving-v4.json](../contracts/serving-v4.json). Every declared foreign key resolves; null references are allowed. Primary keys are unique. Row order is not part of the contract: rows appear in generation order, which is deterministic for identical inputs, so consumers must not rely on sorted relations. Strings are opaque, case-sensitive UTF-8 values. Dates are inclusive service dates. Times are signed `int32` seconds from service-day midnight and may exceed 86,400. Repeated facts use repeated rows. Production cells never contain encoded lists.

Provenance is kept at trip and route level. `source_trip_map` and `source_entity_map` show which source produced each trip, route and stop. Field-level provenance is not recorded.

Zones are call-scoped only. `call_zone` holds each zone code of a call in source order, with the integrated-transport system (`zone_system`, for example `ids-jmk` or a CZPTT catalog code) where the source names one. JDF route-stop zones are written onto every served call of the route stop. There are no fare systems or zone entities.

`route_stop` is the ordered stop list of each route direction, as printed in a line timetable. It is derived from the final calls of every producer: the distinct call patterns of a route and `trip.direction` (null directions form their own group) are merged by longest-common-subsequence alignment, heaviest pattern first; stops that only some patterns serve are inserted between their aligned neighbours by scheduled time. Every visit of a location is its own slot, so loop lines keep both rows. `route_stop_id` is `<route>/<direction or ->/<location>/<visit>`, and `sequence` is the slot's position. Every `trip_call.route_stop_id` points at its slot. A route-scoped travel restriction gets one row per slot its source route stop reaches; a trip-scoped one points at its call. `diagnostics.json` summarizes the merge under `route_stop_order`.

Trip applicability requires three things to be active: the binding's validity envelope, its binding calendar, and the target trip calendar. `binding_id` is a SHA-256 digest over a versioned canonical serialization of owner, namespace, source key, target, applicability, status and context variant. It excludes build time and diagnostic scores. `source_call_map` always refers to a trip binding and keeps the source's actual sequence value.

The manifest inventories every payload except itself, with its byte size and SHA-256. It declares every relation. It records contract validity and publication eligibility independently, so a calibration package may be contract-valid while remaining ineligible for publication. `compiler` records the converter version passed with `--converter-version`.

Detailed compiler reports are a separate artifact, created only with `--diagnostics-out`. `--diagnostic-traces` also includes large trace mappings. Diagnostic selection cannot change production bytes.

Ownership is divided as follows:

- **Adapters** parse syntax and keep source facts, identifiers, calendars and sequences.
- **The national and overlay compilers** match identities, align calls, slice validity, preserve admissible ambiguity, and enforce the duplicate and rail rules. They hand the package writer in-memory tables, not a staging directory.
- **The package writer** validates the finalized model and serializes it atomically.
- **A static importer** validates the contract versions and bulk-loads the declared relations.
- **The central realtime resolver** applies operating date and context to candidate bindings.
- **Connectors** parse payloads, normalize documented key semantics, and emit claims.
- **Diagnostic tooling** explains compiler decisions outside the production package.

Consumers are responsible for operating-date inference, delay calculation, vehicle fusion and claim arbitration.
