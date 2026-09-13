# [JrUtil](https://gitlab.com/dvdkon/jrutil)

JrUtil is a library and a set of utilities for working with Czech (and Slovak)
public transport data, such as:

- scheduled timetables (JDF to GTFS, CZPTTCIS to GTFS, more to come)
- real-time timetable changes (TODO)
- vehicle positions (scraping GRAPP, viewing historical data)

## Regional GTFS overlays

`regional-gtfs-overlay` enriches an immutable national JDF bundle with facts
from a checksum-pinned regional GTFS snapshot. It never downloads a feed and
never mutates either input. Output is first written to a sibling temporary
directory and atomically activated as a new, self-contained bundle.

The PID calibration profile is in
`config/regional-gtfs-overlay/pid-overlay-v1.json`. It excludes heavy rail and
hard-excludes `pathways.txt` and `levels.txt`; regional calendars and times are
used as matching evidence, and uniquely matched complete call patterns inherit
the regional arrival/departure values (including seconds) on shared dates. GVD
2026 is explicitly clipped to 2025-12-14 through 2026-12-12.

```text
jrutil-multitool regional-gtfs-overlay \
  --policy=config/regional-gtfs-overlay/pid-overlay-v1.json \
  --gvd-year=2026 \
  --source=pid-gtfs=PID_GTFS.zip \
  --source-descriptor=pid-gtfs=pid-snapshot.json \
  jdf-aug-final-v2/bundle output-bundle
```

The descriptor must contain `retrieved_at` and the source ZIP's lowercase
`payload_sha256`. Reviewed override CSVs are validity-bounded and require
source/target namespaces, IDs, validity dates, and a review note. An empty CSV
with only the supplied header is valid.

For the non-publishable PID + IDS JMK calibration, use the versioned
`pid-ids-jmk-overlay-v1.json` policy and bind both checksum-pinned archives in
any order. The base is prepared once and both feeds are resolved in one pass;
the command does not chain independently produced overlays.

```text
jrutil-multitool regional-gtfs-overlay-all \
  --policy=config/regional-gtfs-overlay/pid-ids-jmk-overlay-v1.json \
  --gvd-year=2026 \
  --source=pid-gtfs=PID_GTFS.zip \
  --source-descriptor=pid-gtfs=pid-snapshot.json \
  --source=ids-jmk-gtfs=IDSJMK_GTFS.zip \
  --source-descriptor=ids-jmk-gtfs=ids-jmk-snapshot.json \
  jdf-aug-final-v2/bundle output-bundle
```

JrUtil performs no download. Pin the official IDS JMK `gtfs.zip` separately,
record its hash in the descriptor, and keep `api.txt` in the archive. The
IDS-JMK adapter requires exactly one valid mapping per static trip; duplicated
line/course keys remain candidates and are emitted with disjoint validity
ranges. Standard parent/child stops become places and boarding points, route
type 800 is trolleybus, ferry is supported, and heavy rail is excluded. Missing
IDS JMK shapes do not disable PID shapes. IDS JMK zones are additive namespaced
claims; ordinary `stops.zone_id` is populated only for a unique effective zone.
IDS JMK trip-set authority covers bus, tram, trolleybus and ferry service when a
unique structural national route identity is proven. This lets the source fill
dates beyond the national snapshot in the same way as PID. Arbitrary
source-native routes/trips remain restricted to ferries. Unresolved non-ferry
trips are withheld with a reason in `reports/unmatched_trip_reasons.csv`, so a
failed identity match cannot create a parallel journey.
To reconcile verbose IDS JMK names with abbreviated JDF names, its profile also
allows a coordinate-only identity within 25 m when the next candidate is at
least 10 m farther away; nearby urban stops without that margin stay quarantined.

Policy schema v3 retains v2's iterative one-gap stop inference, nearest-schedule
ranking for exact stop patterns, bounded ordered-pattern edits, and
capability-specific minimum match tiers. Equal-score candidates are expanded
only when they share a stable CIS line identity and have an identical complete
stop/time digest. This safely covers parallel JDF validity variants while each
target's pickup/drop-off, timepoint, accessibility, and trip display fields stay
untouched. Otherwise-equal candidates are iteratively resolved when exactly one
target remains unclaimed by another source trip; competing proposals remain
quarantined. The PID profile declaratively extracts a revision date from each
source trip ID. If overlapping source revisions claim conflicting facts for the
same national trip/date, the uniquely newest revision wins; equal-revision
conflicts still remain quarantined. Ties across stable lines or different
stop/times, and conflicting stop-context claims, remain quarantined.
Ordinary stop matching uses compatible names and a configurable geographic
radius (300 m in the PID profile). Strong, unique route/call context can resolve
a compatible stop beyond that radius. When the JDF name ends in ` [?]`, an
accepted PID match supplies the authoritative full name, parent coordinates and
boarding points; source-created posts unused by final calls are pruned.
It also permits explicitly configured transport modes to use a complete regional
trip set on a route/date only when every active source trip has an exact companion
CIS line assertion and every call's stop place resolves. Those proven instances
retain their full regional call patterns; the corresponding unclaimed national
instances are removed instead of being duplicated or partially overlaid.
For separately configured source-native modes, a missing national route is not a
hard gap: the overlay imports namespaced regional agencies, routes, stop places,
posts, trips, calendars and shapes while retaining the asserted CIS line ID. The
PID profile enables this for all supported non-rail modes, including new stops
on existing routes; it never fabricates a CIS trip ID or
JDF-specific trip/call evidence for the imported objects.
Coverage reports distinguish all active source trips, shared-date and
pattern-compatible populations, target-available candidates, and national
snapshot/capacity gaps, so an older or less frequent national snapshot cannot
make the calibration percentage misleading.

`--audit-date=YYYY-MM-DD` defaults to the CIS snapshot retrieval date in
Europe/Prague. `reports/snapshot_day_coverage.csv` separates that day's service
coverage from future, unverified JDF comparisons. `reports/semantic_inheritance.csv`
separates aligned snapshot evidence from source-only trips and claims requiring
explicit notice validity; trip identity mappings alone do not authorize future
notice inheritance. Original sidecars remain unchanged in `base-evidence/`.
Ambiguous national route versions use a source-native route for new trips rather
than inheriting an arbitrary version. Exact CIS identity permits bus/trolleybus
compatibility, with a separate source-mode route preserving retained base service.

GTFS-derived destination displays which identify the final stop are expanded to
that stop's full canonical overlay name. Genuinely distinct source displays,
including bilingual, via and intermediate-destination text, remain unchanged.
`reports/headsigns.csv` preserves raw and output values, the decision reason and
snapshot-day scope. Retained national headsigns are not rewritten. Bounded pattern edits are not complete service coverage and
yield to full source trip projection on authoritative dates.

The bundle contains regenerated `gtfs-intermediate/` and `extensions/`, copied
national non-GTFS evidence under `base-evidence/`, complete mappings and field
provenance, quarantine/conflict/exclusion/coverage reports, and a deterministic
manifest. The checked-in PID v1 policy is deliberately a non-publishable
calibration policy. Publication requires a reviewed policy-only update which
sets coverage floors and enables publication.

### Overlay implementation and feed profiles

`RegionalGtfsOverlay` is the command/library entry point. Policy and binding
records live in `JrUtil.RegionalOverlay.Types`; callers import that module
directly. There are no compatibility type aliases. Implementation modules live
together under `src/Gtfs/RegionalOverlay/`.
The implementation is split into base/input preparation, stop and route matching,
source analysis, trip matching, projection, reports, and bundle writing. Each
stage receives explicit records and returns its evidence or output decisions;
large call tables remain streamed. Runtime logging and the single collection at
the end of analysis are isolated from the matching rules; no row-processing loop
forces garbage collection.

Shape points are sorted into a temporary spool before source-call analysis,
using a 64 MiB row budget and at most 16 merge inputs. Projection validates and
incrementally hashes selected shapes; the writer reads their geometry from disk.
Input order need not group shape IDs. Diagnostic, candidate-score and headsign
rows use append-only scratch files. Scratch lives under the OS temporary directory,
is excluded from the manifest, and is removed on success or failure. Allow space
for spill runs as well as the final output; write failures do not activate a bundle.

Calendars are immutable packed date sets. Identical output service calendars,
matching date sets and exact call alignments are shared within their owning stages.
Projection resolves one national trip at a time in stable ID order and carries only
final slices into writing. Partial matching evidence still yields to complete
source representation on authoritative dates. Contextual stop inference and target
availability resolve each round's proposals together before applying them.

Report and mapping row order is deterministic but is not a semantic contract.
To compare pinned runs, use `scripts/verify_regional_overlay.py BASELINE OUTPUT`:
it checks both manifests independently, requires unchanged transit/evidence bytes,
and compares changed report, mapping and provenance files as exact row multisets.
The offline verifier uses Python's built-in SQLite to bound its own memory; the
overlay has no database dependency.

The compiled runner in `scripts/overlay-profile/` samples
working set, private memory, managed heap, allocations and scratch use every 100 ms,
with named stages inside writing. Pass policy, GVD year, source ID, payload,
descriptor, base bundle, output bundle and an explicit `YYYYMMDD` audit date.
Build it with `dotnet build scripts/overlay-profile/overlay-profile.fsproj`, then
run its generated executable. It writes `.memory.csv` and `.summary.json` beside
the output. Run measurements
without concurrent builds or audits; heap collection itself perturbs the process.

Schema-v3 profiles may omit `source.route_join` and
`source.trip_match.source_revision`. An omitted join provides no companion CIS
assertions. Reviewed overrides and structural matching remain available; a
unique structural route/date proof can authorize a complete source trip set,
while configured road/local modes can import unmatched service under
deterministic source-namespaced IDs. A configured join must name `cis_line_id`
as its target namespace,
provide equally sized non-empty source/lookup key arrays, and supply its table.
An omitted revision extractor provides no recency ordering; conflicting claims
remain quarantined. Configured but unparseable revisions retain the existing
oldest-revision treatment and diagnostic.

Stop grouping first uses standard `parent_station`, then the configured group
column, then the individual stop ID. Group/post columns can be omitted; posts
fall back to their source stop IDs. These defaults allow ordinary GTFS snapshots
without PID-specific columns. IDS JMK has a supplied adapter/profile. IDZK is
intentionally out of scope because its feed lacks the required boarding-post
hierarchy.

In the combined profile, compatible equal-priority facts coalesce and retain all
source provenance. Differing facts are quarantined and the national value
survives. Missing facts are neutral, so a PID shape can coexist with an identical
JMK schedule. Equivalent source-native trips and strongly equivalent boarding
points (same resolved place and nonblank platform code) are structurally
deduplicated. Matching-tier arrays enable existing tiers;
route precedence remains companion assertion, reviewed override, then structural
evidence. `require_unique_best` and `require_equal_call_count` retain their
existing fixed behavior: uniqueness and equal-call-count contextual inference
are always enforced. `never_inherit` must include pathways and levels; this is
not a general-purpose table filter. Disabled name, agency, calendar and
trip-display inheritance applies to enrichment of retained national objects. Complete authoritative/source-native
projections have their existing construction rules, including source agencies,
calendars and displays; approximate national stops also retain their explicitly
supported correction behavior.

Single-source bundle version 1, IDs, report schemas and machine-readable values
are preserved. Combined bundles use version 2 with a sorted `sources` array,
per-source and aggregate counts, source payload/descriptor hashes, and the
combined policy hash.
In particular, `pid_name_and_coordinates` and `pid_native` in the stop-match
report are legacy classification labels even for another source. Consult the
source identity and provenance rather than interpreting these values as feed IDs.
Human-readable diagnostics use the configured source identity.

The future regional `overlay-all` command should prepare one immutable national
base, analyze each feed against that base, resolve source-qualified claims, and
write one bundle. It must not chain overlays of already-overlaid bundles. The
current internal preparation/analysis/projection/writer boundaries are the seams
for that work; cross-source conflict resolution, priority semantics and a
multi-source manifest remain future work.

# Project parts

JrUtil's main part is the central library, also called *JrUtil*. It allows the
user to read a variety of public transport data formats (currently JDF and
CZPTTCIS), convert them to GTFS and then perform various operations on the
result.

*jrutil-multitool* is a small tool whose purpose is to provide a command line
interface to commonly used functions of *JrUtil*. It can, for example, convert
a single JDF or CZPTT batch to a GTFS feed.

*jrutil.tests* contains unit and small integration tests of JrUtil.

*GeoReport* loads Czech stop position data and runs it through JrUtil to
determine how many stops have a matching location.

*RtCollect* uses JrUtil to scrape current vehicle delays and store them in a
PostgreSQL database.

*RtView* displays data collected by RtCollect as a web app.

*mkscriptenv.sh* creates an environment for writing .fsx scripts in *scripts*.

# Oběhy JDF schedule extensions

JDF-to-GTFS conversion also writes four optional Czech extension files used
by the Oběhy canonical importer:

| File | Purpose |
| --- | --- |
| `cz_routes.txt` | CIS line identity, passenger-facing line number and JDF provenance |
| `cz_trips.txt` | CIS line/trip identity and source-trip provenance |
| `cz_stops.txt` | Source stop-place, CIS stop and post identities |
| `cz_stop_zones.txt` | Normalized, route-scoped stop-to-zone memberships |

The selected public line number is written to both `cz_routes.txt` and
`routes.txt`'s `route_short_name`. A preferred JDF `LinExt` designation wins;
otherwise the last three digits of the six-digit CIS line ID are used.
Numeric designations have leading zeroes removed, while alphanumeric
designations retain their spelling and case.

JDF post references from `Oznacniky.txt` and text-only station numbers from
`Zasspoje.txt` are both projected as distinct child stops with standard GTFS
`parent_station` links. In `cz_stops.txt`, `stop_id` identifies the exact GTFS
place-or-post row while `stop_place_id` always identifies its containing stop
place; they are equal for a place and differ for a post. Post IDs share one
hierarchy while retaining source semantics: `:post:id:` is an authoritative
`Oznacniky` ID and bare `:post:<value>` is a textual `Zasspoje` designation.

Raw zone lists are split, trimmed and deduplicated without attempting to infer
their IDS system. Per-membership `ids_system_id` in `cz_stop_zones.txt`
therefore remains empty for this conversion. A route
distinction and raw token form the source zone identity. Standard GTFS
`stops.zone_id` is populated only when a stop has exactly one such identity;
plural memberships remain authoritative in `cz_stop_zones.txt`. The bundle's
zone Parquet contains only the JDF route-stop provenance and source order that
cannot be reconstructed from that extension. Standard `stop_times.txt` never contains the old
non-standard `stop_zone_ids` column.

Generated intermediate identifiers consistently use colon-separated
namespaces: `jdf:agency:…`, `jdf:route:…`, `jdf:trip:…`, `jdf:stop:…`,
`jdf:zone:…`, `jdf:notice:…`, `jdf:transfer:…` and `jdf:restriction:…`.
With `--stop-ids-cis`, stop and post IDs use `cis:stop:…`. Text components are
URI-escaped before being embedded in an identifier. Deduplicated GTFS service
patterns use the derived `gtfs:service:<weekday-bitmap>:<ordinal>` namespace.
Inferred boarding points use `jdf:stop:<stop>:est:<ordinal>`, or the equivalent
`cis:stop:` prefix. The 1-based ordinal is assigned per parent stop by sorting
the full internal derived-location IDs; those full IDs remain in diagnostic
relations for traceability.

JDF stop numbers are not always global CIS identifiers. Pass `--stop-ids-cis`
only for a batch known to use the national CIS stop registry:

```text
dotnet run --project jrutil-multitool -- \
  jdf-to-gtfs --stop-ids-cis JDF-input GTFS-output
```

The extension files are optional in the shared GTFS model. CZPTT conversion
populates route, trip and trip-stop-zone extensions when the source supplies
the corresponding data.

CZPTT conversion defaults to `--block-mode=blocks`: line, train-category and
operator changes produce linked GTFS trips with a shared `block_id`. Use
`--block-mode=none` to emit one GTFS trip per PA instead. In that mode route
labels combine first-seen unique line marks and train-designation fallbacks,
for example `U32/S32` or `U2/Os 7002`, and multi-operator routes reference a
composite agency such as `ČD / DB`. Both `czptt-to-gtfs` and
`czptt-to-bundle` accept the option.

Passenger line/category changes recorded at internal infrastructure points
are applied at the following passenger call. If an operator handover occurs
in the same inter-stop span, the passenger attributes are coalesced onto that
handover so conversion does not create a tiny intermediate block. Bundle
diagnostics record these adjustments.

For `czptt-to-bundle`, `--sr70=FILE` supplies canonical Czech operational-point
names as well as coordinates. A matching SR70 name takes precedence over the
CZPTT `PrimaryLocationName`; points missing from SR70 retain the CZPTT name.

For `fix-jdf`, `--ext-geodata` accepts either one headerless stop-position CSV
or a directory. Directory inputs recursively load `*.csv` files in stable
relative-path order and remove exact duplicate rows before constructing the
stop matcher. Pass `--strict` to `merge-jdf` when a malformed input batch must
fail the command instead of being logged and skipped.

## Parallelism and memory budgets

The multitool accepts `--jobs=<count|auto>` and
`--memory-budget=<KiB|MiB|GiB|auto>`. Automatic jobs start at twice the
logical processor count and may grow to eight times it (between 32 and 256
workers), subject to live process-memory, CPU, and ordered-backlog pressure.
An explicit numeric job count is a hard ceiling and is not reduced to the
processor count. Automatic memory mode snapshots currently available RAM and
the process's existing private bytes, keeps an OS reserve, and applies a
capacity ceiling. On systems with at most 20 GiB it normally uses a smaller
12.5% reserve and treats a bounded share of memory occupied outside JrUtil as evictable (75%
at 8 GiB or less, 50% above that). This avoids collapsing the budget on hosts
whose OS can trim caches and other working sets. Larger systems retain the
conservative available-memory policy. `fix-jdf` takes its worker-budget snapshot
after loading its persistent stop index, so that index is included in the
process baseline rather than mistaken for worker headroom. Legacy straight-line
transit geometry is not built.
`fix-jdf` and `merge-jdf` admit independent batch parsing by estimated
uncompressed bytes while committing results in stable input order. Admission
targets 85% of the process memory budget, keeps the batch currently being
consumed charged against that bound, pauses at 95%, and resumes below 80%.
Use `--jobs=1` for a serial run or an explicit memory budget for repeatable
capacity tests.

`fix-jdf --batch-output=zip` writes one deterministic uncompressed ZIP per
fixed input batch; the default remains the traditional directory layout.
`--progress-events` adds `JRUTIL_PROGRESS` JSON lines for resolved worker
plans, phase changes, batch start/completion/failure events, and one-second
`scheduler_sample` resource/controller snapshots. Human logs remain available
alongside the machine-readable stream.

`merge-jdf` keeps the large `Zasspoje.txt` relation in an uncompressed spool
beside the output while route overlap metadata remains in memory. Within each
ordered batch, trip-stop rows are remapped and serialized into bounded parallel
memory buffers, then appended to the spool in their original order. The spool
is removed on success or failure. Parsing uses the requested worker ceiling
with an additional byte-weighted in-flight bound.
Phase-boundary `resource_usage` progress events report working-set, private,
managed-heap, fragmentation, and spill byte counts.

`czptt-to-bundle` uses a sibling spill file for surviving timetable messages,
replays them in PA-ID order after cancellations, and removes the spill on all
exit paths. Library conversion helpers remain memory-backed by default. Its
Parquet writers enumerate count-known replayable rows instead of retaining an
additional array of row dictionaries.

National bundle conversion uses pooled source values and streams GTFS stop
times and row-grouped Parquet call metadata directly from compact source trip
descriptors. It never retains a second national-sized stop-time or call table;
the memory budget only bounds parallel batch work. Serialized output remains
deterministic and byte-identical across memory budgets.

## Oběhy JDF conversion bundles

`jdf-to-bundle` accepts either an extracted JDF directory or a ZIP and writes
an immutable bundle with standard GTFS tables, the four extension tables,
typed Parquet sidecars, structured diagnostics and a checksummed manifest:

```text
dotnet run --project jrutil-multitool -- \
  jdf-to-bundle \
  --snapshot-descriptor=snapshot.json \
  --converter-version=<fork-commit> \
  --routing-osm-pbf=jdf-transit-routing-demand.osm.pbf \
  [--diagnostic-post-labels] \
  JDF-input bundle-output
```

The output path must not exist. The command writes through a temporary sibling
directory and activates the result only after every file and checksum has been
produced. Conversion or packaging failures return a non-zero exit code.

Bundle format v1 deliberately treats standard GTFS plus the four extension
tables as the primary normalized representation. It does not mirror those
entities into Parquet:

```text
bundle/
├── gtfs-intermediate/
├── extensions/
├── source_route_metadata.parquet
├── source_stop_metadata.parquet
├── source_call_metadata.parquet
├── source_route_stop_zone_metadata.parquet
├── source_notice_metadata.parquet
├── source_transfer_metadata.parquet
├── source_travel_restriction_metadata.parquet
├── derived_post_locations.parquet
├── derived_post_assignments.parquet
├── post_candidate_evidence.parquet
├── post_side_groups.parquet
├── derived_post_scores.parquet
├── diagnostics.json
└── manifest.json
```

The source Parquet tables retain facts that would otherwise be
lost. The original route/stop/call metadata tables preserve JDF distinctions,
structured stop-name components, coordinate absence and route-stop IDs. The
additional relations preserve route-stop zone scope, textual notices,
connection context and the `§`/`A`/`B`/`C` travel-exclusion groups. Trip,
boarding-point, call and fare-zone entity mirrors are intentionally absent.

Estimated posts are disabled by default unless `--routing-osm-pbf` or
`--post-inference-evidence` is supplied.
The input must be the Osmium-prepared demand clip and have its matching
`.manifest.json` sidecar. `--no-estimated-posts` remains the complete rollback
path. Diagnostic `O1`/`O-N`/`?` platform codes are independently default-off.

Expensive directed routing can be captured once and replayed without opening
OSM or running A*:

```text
jdf-to-bundle --routing-osm-pbf=clip.osm.pbf \
  --capture-post-inference-evidence=evidence --post-inference-evidence-only ...

jdf-validate-post-inference --evidence=evidence

jdf-replay-post-inference --evidence=evidence --policy=policy.json \
  --expectations=expectations.tsv --review-stops=stops.txt --output=review

jdf-to-bundle --post-inference-evidence=evidence \
  --post-inference-policy=policy.json ...
```

Evidence v2 contains `observations.parquet`, `route_points.parquet`,
`contexts.parquet`, `corridor_variants.parquet`, and
`route_point_evidence.parquet`. Capture is a raw-fact phase: it does not load a
policy or enter consolidation, scoring, resolution, GTFS conversion, or bundle
writing. Each routed context contains one to three baseline-relative directed
corridor variants and one attachment or explicit failure row for every route
point on every variant. Its manifest is bound to the merged JDF and routing PBF
identities, routing ceilings, router/variant-enumeration versions, and a pack ID
repeated in each Parquet relation. The capture-tool version is part of that pack
identity, so two capture implementations cannot silently publish the same ID.
It records the hash, size, 64-bit row count,
and schema fingerprint of every relation. V1 and incomplete older v2 packs are
rejected and must be recaptured. Capture-only cannot be combined with a policy.
Capture-only is dispatched before policy and bundle preparation and returns a
typed `CaptureCompleted` result to the CLI.
Its disk preflight is derived from the canonical deduplicated route-pattern
plan, not from the number of timetable calls. The upper bound includes every
captured relation, and atomic publication requires one temporary pack plus a
fixed safety reserve because activation is a same-volume directory rename.
Evidence-backed bundle generation rejects a different input snapshot before
conversion and performs no graph construction or routing searches. Live and
replay conversion invoke the same v2 evaluator; the live path first creates a
temporary evidence store and reopens it through the same validated trust
boundary as replay. Validation covers exact Arrow types/nullability and
metadata, canonical ordering, key/foreign-key integrity, context/block/family
identity, corridor ranks and costs, unavailable sentinels, finite numeric facts,
and complete route-point attachment coverage. Policy v2 owns consolidation, hard gates, geometry,
support, consensus, alternatives, side groups, authored resolution, and
same-stop thresholds. A policy
may request at most the captured routed-excess horizon (currently 1,000 m;
the default publication gate remains 500 m).

The sole production decision boundary is a validated `PostEvidenceStore` plus a
`PostInferencePolicyV2`, returning a complete disposable
`PostInferenceResult`. The result owns replayable assignment and diagnostic-row
stores, while its hypotheses, side groups, authored positions, and counters are
complete policy outputs. `JdfToGtfs` only adapts that result to the conversion
plan; capture and bundle orchestration do not contain a second evaluator.

Capture canonicalizes observations, route points, and context work through one
bounded replayable-row abstraction. Rows remain in chunks below budget and use
private binary spools above it. Stop-major contexts receive ordinals before
routing; admitted workers consume bounded queues with graph-internal concurrency
set to one, write ordinal-sorted worker spools, and feed a stable k-way merge.
The same typed enumerators produce fixed 65,536-row Parquet groups, so relation
and manifest bytes are independent of worker count, input order, and spill
boundaries. Capture progress reports admission and current/peak spill bytes;
temporary stores are removed after success, validation failure, routing failure,
cancellation, or output collision.

The enrichment schemas are:

```text
source_route_stop_zone_metadata
    gtfs_route_id, source_route_stop_id, zone_id, zone_order

source_notice_metadata
    source_notice_id, notice_kind, gtfs_route_id?, gtfs_trip_id?,
    label?, text?, valid_from?, valid_to?, service_note_type?

source_transfer_metadata
    source_transfer_id, gtfs_trip_id, source_route_stop_id,
    transfer_type, transfer_route_id?, transfer_stop_id?,
    transfer_stop_post_id?, transfer_end_stop_id?,
    transfer_end_stop_post_id?, wait_minutes?, note?

source_travel_restriction_metadata
    assignment_scope, gtfs_route_id?, gtfs_trip_id?,
    source_route_stop_id, group_code
```

`source_notice_metadata` maps `Udaje` to route notices, text-bearing or
otherwise unhandled `Caskody` to trip notices, and `Mistenky` to reservation
notices. Calendar-only `Caskody` are not repeated because their complete effect
is already present in GTFS calendars. `Navaznosti` records retain their source
target IDs and waiting context without guessing a canonical target.

Travel exclusions retain their source scope: `Zaslinky` produces
`route_stop` assignments and `Zasspoje` produces `trip_call` assignments.
For one trip, calls sharing the same effective group code (`§`, `A`, `B` or
`C`) may not be used as both the boarding and alighting endpoints. Importers
union route-stop and trip-call assignments rather than expecting expanded rows.

The join path is deliberately relational: GTFS identifies routes, trips and
calls; `source_call_metadata` maps a GTFS call to its JDF route-stop;
route-stop zone, transfer and restriction relations add only facts missing
from GTFS. These Parquet files are an immutable import format. A runtime
application should import them into indexed database tables rather than query
bundle files per request.

Every row is a positive source claim. Absence from a later regional overlay is
not a deletion of a national notice, zone or restriction; precedence and
materialization belong to the downstream compiler.

Snapshot descriptor schema version 1 is:

```json
{
  "schema_version": 1,
  "source_id": "national-jdf",
  "retrieved_at": "2026-07-18T12:00:00+02:00",
  "retrieval_method": "https",
  "source_uri": "https://example.invalid/jdf.zip",
  "licence": "source-specific licence",
  "payload_kind": "zip",
  "payload_sha256": "64 lowercase hexadecimal characters",
  "payload_bytes": 123456
}
```

For `payload_kind=zip`, the hash and byte count cover the ZIP itself. For
`payload_kind=directory-tree`, files are ordered by normalized relative UTF-8
path; the tree digest appends each path, a NUL byte and that file's SHA-256.
`payload_bytes` is the sum of file sizes. ZIP input must contain exactly one
JDF batch root and is rejected if paths are unsafe or collide by case.

Bundle Parquet files use schema version 1, Snappy compression and fixed row
groups. `source_id` and `snapshot_id` are stored once in Parquet file metadata
and in `manifest.json`, not repeated on every row. The manifest also records
JDF-declared metadata, conversion options, row counts, byte sizes and SHA-256
for every other output. Identical input, descriptor, converter version and
options produce identical bytes. When JDF lacks coordinates, standard GTFS
uses JrUtil's existing `0,0` required-field fallback and
`source_stop_metadata.parquet.coordinates_missing` preserves that fact without
duplicating coordinate values.

# Installation:

This project uses [.NET Core](https://www.microsoft.com/net).

First, download and install the
[.NET Core SDK](https://www.microsoft.com/net/download).
Using the latest version is highly recommended.

Clone the repo and run `dotnet build`. Now you can use the various utilities by
going to their directory and running `dotnet run -- ARGS...`.

Some parts of JrUtil use a PostgreSQL database to store data and speed up
bulk operations. To use those, set up a PostgreSQL server and pass an
[Npgsql connection string](https://www.npgsql.org/doc/connection-string-parameters.html)
as a parameter.

# Community

To contribute to the project or to report issues go to the
[GitLab repository](https://gitlab.com/dvdkon/jrutil).

You may also visit a forum dedicated to Czech transport data (in Czech
language): https://dadof.ggu.cz
