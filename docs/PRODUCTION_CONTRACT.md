# JrUtil production package contract

JrUtil production packages use `bundle_format: jrutil-production`, bundle version 3 and serving schema version 4. The checked-in JSON contracts, [production-v3.json](../contracts/production-v3.json) and [serving-v4.json](../contracts/serving-v4.json), are normative. The F# schema declarations and writer are tested against them.

The package inventory is closed:

```text
output/
├── gtfs.zip
├── serving/
│   └── <30 declared relations>.parquet
├── manifest.json
└── diagnostics.json
```

`gtfs.zip` is the only GTFS representation, and it contains only standard GTFS. Its entries are flat, ordered by ordinal file name, and have a fixed ZIP timestamp. `transfers.txt` contains only standard GTFS fields. Transfer waiting limits live in the serving relation `transfer`.

All serving Parquet relations exist even when empty. Their exact field order, types, nullability, primary keys and foreign keys are in [serving-v4.json](../contracts/serving-v4.json). Every declared foreign key resolves; null references are allowed. Primary keys are unique. Row order is not part of the contract: rows appear in generation order, which is deterministic for identical inputs, so consumers must not rely on sorted relations. Strings are opaque, case-sensitive UTF-8 values. Dates are inclusive service dates. Times are signed `int32` seconds from service-day midnight and may exceed 86,400. Repeated facts use repeated rows. Production cells never contain encoded lists.

Provenance is kept at trip and route level. `source_trip_map` and `source_entity_map` show which source produced each trip, route and stop. Field-level provenance is not recorded.

Zones are codes, not entities: there are no fare systems or zone records. Each code carries the integrated-transport system (`zone_system`, for example `ids-jmk` or a CZPTT catalog code) where the source names one. A route stop slot (see below) holds its zones in `route_stop_zone` when every call at it has the same non-empty zone set. Otherwise the slot has none, and each zoned call has its own rows in `call_zone`. A call's zones are therefore its slot's `route_stop_zone` rows if there are any, else its `call_zone` rows. `location_zone` is the distinct union per location, for stop lookups; a missing system is stored there as an empty string because it is part of the key.

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

## Serving schema 5.0 (draft)

[serving-v5.json](../contracts/serving-v5.json) is a reviewed draft and not yet produced. It replaces the 30 v4 relations with 19 that are easier to query and keep every fact Oběhy, riders and the NeTEx export need. It was checked by running the realtime, departure-board, vehicle-detail and NeTEx-mapping queries against a v5 prototype derived from the 2026-10-03 release.

**Versioning.** The version is `major.minor` (`"5.0"`) in the manifest and the Parquet metadata. A minor version only adds: relations, nullable fields at the end of a relation, enumeration values, or namespaces. Consumers accept every minor of their major, ignore unknown relations and fields, and treat unknown enumeration values as unknown. Anything else (removing, renaming or retyping a field, changing a key or the meaning of a value) is a new major.

**Identifiers.** Every primary-key identifier starts with its feed prefix (`jdf:` or `czptt:`), so both packages share tables without collisions. This includes services, transfers, notes and assignments, which v4 left unprefixed or under a shared `gtfs:` prefix.

**Times.** Times are signed seconds after noon minus 12 hours, local time (Europe/Prague), of the service date, as in GTFS `stop_times`. This equals the wall-clock reading except on DST change days. Values from 86,400 up continue into the next day.

**Enumerations.** Every coded field names a closed list in `enumerations`. `weekday_mask` uses bit 0 for Monday through bit 6 for Sunday. The typed feature kinds of `JDF_SEMANTICS.md` replace v4's generic `jdf_trip_attribute` and `jdf_stop_attribute` rows. JDF calendar codes (`X`, `+`, `1`–`7`) become `calendar_designation` assignments.

**Calls and trip parts.** `trip_call` is the only call relation. Its passenger calls are exactly GTFS `stop_times`, with `stop_sequence = sequence`. JDF pass-through stops are not published, because the source gives them no time. A CZPTT path (PA) is split into trip parts, ordered by `trip.run_key`/`run_part` and keeping the PA's sequence numbers:

- Consecutive parts share exactly one boundary call. It is the last call of one part and the first of the next, with identical times.
- A rail part also carries every non-passenger railway point between its first and last call.
- The first rail part carries the points before the path's first passenger call; the last rail part carries the points after its last passenger call.
- A rail-replacement (NAD) bus part carries only its passenger calls. Railway points inside a bus part are not published (106,561 in the 2026-10-03 release), because the bus does not pass them.
- A non-passenger call has pickup and drop-off `1`, no boarding point and no route stop. For a pass, arrival equals departure; times are null when the source gives none.

The CZPTT subsidiary location and active line code are `trip_call` columns. `operational_location`, `operational_journey`, `operational_call` and `scheduled_passage` are removed; operational points are `location` rows of kind `operational_point`.

**Calendars.** `service_calendar` and `service_exception` stay as in v4. GTFS keeps proper calendars: `calendar.txt` holds the weekday pattern and `calendar_dates.txt` the exceptions.

**Keys.** `source_key(namespace, identifier) → public_id` replaces `source_trip_map`, `source_entity_map`, `road_route_key`, `road_trip_key` and `rail_trip_key`.

- It records validity and a `binding_method`.
- Namespaces name their source system (`cis:line_trip`, `czptt:tr`, `pid:gtfs_trip_id`, `ids-jmk:line_course`, …). The contract fixes each namespace's identifier encoding, for example `582492:143` for CIS line + trip.
- A public ID resolves to itself without a row. A trip key applies on its trip's service dates.
- `call_key` lists source call sequences only where they differ from `trip_call.sequence` (2,035 rows in the 2026-10-03 release instead of 16.6M).
- Binding hashes, call-pattern hashes and `source_trip_coverage` are removed.

**Locations.** `location` carries `domain` (`surface`/`heavy_rail`), `coordinate_precision` (`exact`/`estimated`/`missing`) and `coordinate_source`. A boarding point's `public_code` is its platform or post designation (GTFS `platform_code`); v4 leaves it null everywhere.

**Zones.** Zones stay on route stop slots (`route_stop_zone`, keyed by the now globally unique `route_stop_id`), with `call_zone` exceptions. `location_zone` is dropped because it can be derived.

**Typed semantics stay lossless.** The relations are `service_note`, `connection_claim`, `travel_restriction`, and one `assignment` relation for note links, features and calendar designations.

- `assignment` scope is route, trip, call, call range, service or location. CZPTT notes that apply between two locations become `call_range` assignments, not whole-trip ones.
- `service_note.label` is the display text, and `text` keeps the verbatim source.
- Rows keep `source_object_id`. Source ids and snapshot digests move to the manifest, which must record the CZPTT input digest (v4 writes zeros).

GTFS is a pure projection of these relations.
