# JrUtil production package contract

JrUtil production packages use `bundle_format: jrutil-production`, bundle version 3 and serving schema version 5.0. The checked-in JSON contracts, [production-v3.json](../contracts/production-v3.json) and [serving-v5.json](../contracts/serving-v5.json), are normative. The F# schema declarations (`Serving/Schema.fs`) are tested against them, and `validate-package` enforces them.

The package inventory is closed:

```text
output/
├── gtfs.zip
├── serving/
│   └── <19 declared relations>.parquet
├── manifest.json
└── diagnostics.json
```

All serving Parquet relations exist even when empty. Their exact field order, types, nullability, coded values, primary keys and foreign keys are in [serving-v5.json](../contracts/serving-v5.json). Every declared foreign key resolves; null references are allowed. Primary keys are unique. Row order is not part of the contract: rows appear in generation order, which is deterministic for identical inputs, so consumers must not rely on sorted relations. Strings are opaque, case-sensitive UTF-8 values. Dates are inclusive service dates. Repeated facts use repeated rows. Production cells never contain encoded lists.

**Versioning.** The version is `major.minor` (`"5.0"`) in the manifest and the Parquet metadata. A minor version only adds: relations, nullable fields at the end of a relation, enumeration values, or namespaces. Consumers accept every minor of their major, ignore unknown relations and fields, and treat unknown enumeration values as unknown. Anything else (removing, renaming or retyping a field, changing a key or the meaning of a value) is a new major.

**Identifiers.** Every primary-key identifier starts with its feed prefix (`jdf:` or `czptt:`; regional-overlay entities in the JDF package use `overlay:`), so both packages share tables without collisions. Trip, route and stop identifiers are the GTFS identifiers, so GTFS-RT matches them directly.

**Times.** Times are signed `int32` seconds after noon minus 12 hours, local time (Europe/Prague), of the service date, as in GTFS `stop_times`. This equals the wall-clock reading except on DST change days. Values from 86,400 up continue into the next day.

**Enumerations.** Every coded field names a closed list in `enumerations`. `weekday_mask` uses bit 0 for Monday through bit 6 for Sunday. The typed feature kinds of JDF attribute codes replace generic attribute rows; JDF calendar codes (`X`, `+`, `1`–`7`) are `calendar_designation` assignments. Travel exclusions (`§`, `A`–`C`) are `travel_restriction` rows.

**Calls and trip parts.** `trip_call` is the only call relation. Its passenger calls are exactly GTFS `stop_times`, with `stop_sequence = sequence`. JDF pass-through stops are not published, because the source gives them no time. A CZPTT path (PA) is split into trip parts, ordered by `trip.run_key`/`run_part` and keeping the PA's sequence numbers:

- Consecutive parts share exactly one boundary call. It is the last call of one part and the first of the next, with identical times.
- A rail part also carries every non-passenger railway point between its first and last call.
- The first part of a path carries the points before the path's first passenger call, and the last part those after its last passenger call, when that part is a rail part.
- A rail-replacement (NAD) bus part carries only its passenger calls; railway points inside a bus part are not published, because the bus does not pass them.
- A non-passenger call has pickup and drop-off `1`, no boarding point and no route stop. For a pass, arrival equals departure; times are null when the source gives none.

The CZPTT subsidiary location and active line code are `trip_call` columns. Railway points that are not stops are `location` rows of kind `operational_point`.

**Route stops and zones.** `route_stop` is the ordered stop list of each route direction, as printed in a line timetable. It is derived from the passenger calls of every producer: the distinct call patterns of a route and `trip.direction` (null directions form their own group) are merged by longest-common-subsequence alignment, heaviest pattern first; stops that only some patterns serve are inserted between their aligned neighbours by scheduled time. Every visit of a location is its own slot, so loop lines keep both rows. `route_stop_id` is `<route>/<direction or ->/<location>/<visit>`, and `sequence` is the slot's position. Every passenger `trip_call.route_stop_id` points at its slot. A route-scoped travel restriction gets one row per slot its source route stop reaches; a trip-scoped one points at its call. `diagnostics.json` summarizes the merge under `route_stop_order`.

Zones are codes, not entities. Each code carries the integrated-transport system (`zone_system`) where the source names one. A slot holds its zones in `route_stop_zone` when every call at it has the same non-empty zone set; otherwise each zoned call has its own rows in `call_zone`. A call's zones are therefore its slot's `route_stop_zone` rows if there are any, else its `call_zone` rows. Zones per location are derived, not stored.

**Calendars.** `service_calendar` holds a validity range and weekday mask; `service_exception` adds or removes single dates. A service known only from exceptions has a calendar row with mask `0`.

**Keys.** `source_key(namespace, identifier) → public_id` maps source identifiers to public ones, with validity and a `binding_method`. Namespaces name their source system (`cis:line_trip`, `czptt:tr`, `pid:gtfs_trip_id`, `ids-jmk:line_course`, …), and the contract fixes each namespace's identifier encoding, for example `582492:143` for CIS line + trip. A public ID resolves to itself without a row. A trip key applies on its trip's service dates within its validity. `call_key` lists source call sequences only where they differ from `trip_call.sequence`. A regional source without a declared namespace publishes no keys until a minor version declares one.

**Locations.** `location` carries `domain` (`surface` for the JDF package, `heavy_rail` for CZPTT), `coordinate_precision` (`exact`/`estimated`/`missing`) and `coordinate_source`. A boarding point's `public_code` is its platform or post designation (GTFS `platform_code`).

**Typed semantics.** The relations are `service_note`, `connection_claim`, `travel_restriction`, and one `assignment` relation for note links, features and calendar designations.

- `assignment` scope is route, trip, call, call range, service or location. CZPTT notes that apply between two locations are `call_range` assignments on the calls of each trip part inside the range; a note whose range cannot be resolved links to every part of its path.
- `service_note.label` is the display text, and `text` keeps the verbatim source.
- `connection_claim` keeps every JDF `Navaznosti` row: `m` is `waits_for`, `M` is `connects_to`.
- Rows keep `source_object_id`. Source ids and snapshot digests are in the manifest `sources`; every source records its `payload_sha256`.

**GTFS.** `gtfs.zip` is a pure projection of the relations and contains only standard GTFS: `stop_times` are the passenger calls, `calendar.txt` the calendars (`weekday_mask` as weekday columns), `calendar_dates.txt` the exceptions (`added` as exception type 1, else 2), `stops.txt` the stop places and boarding points with `public_code` as `platform_code`, and `transfers.txt` the standard transfer columns (waiting limits stay in the serving relation `transfer`). `feed_info.txt` is build metadata passed through from the compiler. Entries are flat, ordered by ordinal file name, and have a fixed ZIP timestamp. `validate-package` checks that `stop_times`, `trips`, `calendar` and `calendar_dates` equal the projection.

The manifest inventories every payload except itself, with its byte size and SHA-256. It declares every relation and the key namespaces. It records contract validity and publication eligibility independently, so a calibration package may be contract-valid while remaining ineligible for publication. `compiler` records the converter version passed with `--converter-version`.

Detailed compiler reports are a separate artifact, created only with `--diagnostics-out`. `--diagnostic-traces` also includes large trace mappings. Diagnostic selection cannot change production bytes.

Ownership is divided as follows:

- **Adapters** parse syntax and keep source facts, identifiers, calendars and sequences.
- **The national and overlay compilers** match identities, align calls, slice validity, preserve admissible ambiguity, and enforce the duplicate and rail rules. They hand the package writer in-memory tables, not a staging directory.
- **The package writer** validates the finalized model and serializes it atomically.
- **A static importer** validates the contract versions and bulk-loads the declared relations.
- **The central realtime resolver** applies operating date and context to candidate keys.
- **Connectors** parse payloads, normalize documented key semantics, and emit claims.
- **Diagnostic tooling** explains compiler decisions outside the production package.

Consumers are responsible for operating-date inference, delay calculation, vehicle fusion and claim arbitration.
