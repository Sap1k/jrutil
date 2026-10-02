# [JrUtil](https://gitlab.com/dvdkon/jrutil)

JrUtil is a library and a set of utilities for working with Czech (and Slovak)
public transport data, such as:

- scheduled timetables (JDF and CZPTTCIS to production packages with GTFS)
- regional GTFS overlays (PID, IDS JMK) on top of the national JDF package

Production schedule writers emit the closed `jrutil-production` contract
(bundle version 2): a standard `gtfs.zip`, serving Parquet relations (schema
version 3), `manifest.json`, and bounded `diagnostics.json`. The normative
contract is [docs/PRODUCTION_CONTRACT.md](docs/PRODUCTION_CONTRACT.md), with
[migration instructions](docs/OUTPUT_MIGRATION.md) and the
[historical output audit](docs/OUTPUT_AUDIT.md). The source metadata tables
described below are compiler-internal, not consumer interfaces.

## Regional GTFS overlays

`regional-gtfs-overlay` enriches an immutable national JDF bundle with facts
from a checksum-pinned regional GTFS snapshot. It never downloads a feed and
never mutates either input. Output is first written to a sibling temporary
directory and atomically activated as a new, self-contained bundle.

The command takes an overlay-all policy (schema v1) that names one or more
sources, each with a source profile (policy schema v4) and an adapter
(`pid-v1` or `ids-jmk-v1`). Oběhy's production policy is
`src/obehy/data/regional-gtfs-overlay/pid-ids-jmk-production-v1.json`. Bind every
declared source with its checksum-pinned archive, in any order. The sources are
merged into one namespaced input and resolved in a single pass; the command
does not chain independently produced overlays. Heavy rail is excluded and
`pathways.txt` and `levels.txt` are never inherited; regional calendars and
times are used as matching evidence, and uniquely matched complete call
patterns inherit the regional arrival/departure values (including seconds) on
shared dates. The GVD year clips the window (GVD 2026: 2025-12-14 through
2026-12-12).

```text
jrutil-multitool regional-gtfs-overlay   --policy=pid-ids-jmk-production-v1.json   --gvd-year=2026   --converter-version=<fork-commit>   --source=pid-gtfs=PID_GTFS.zip   --source-descriptor=pid-gtfs=pid-snapshot.json   --source=ids-jmk-gtfs=IDSJMK_GTFS.zip   --source-descriptor=ids-jmk-gtfs=ids-jmk-snapshot.json   national-jdf-package output-bundle
```

The descriptor must contain `retrieved_at` and the source ZIP's lowercase
`payload_sha256`. Reviewed stop override CSVs are validity-bounded and require
source/target namespaces, IDs, validity dates, and a review note. An empty CSV
with only the supplied header is valid.

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

Source profiles keep iterative one-gap stop inference, nearest-schedule
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
make a coverage percentage misleading.

The coverage audit date is the CIS snapshot retrieval date in
Europe/Prague. `reports/snapshot_day_coverage.csv` separates that day's service
coverage from future, unverified JDF comparisons. Semantic applicability is
compiled into calendar-backed serving assignments; trip identity mappings alone
do not authorize future notice inheritance.
Ambiguous national route versions use a source-native route for new trips rather
than inheriting an arbitrary version. Exact CIS identity permits bus/trolleybus
compatibility, with a separate source-mode route preserving retained base service.

GTFS-derived destination displays which identify the final stop are expanded to
that stop's full canonical overlay name. Genuinely distinct source displays,
including bilingual, via and intermediate-destination text, remain unchanged.
Optional detailed diagnostics preserve raw and output headsign decisions.
Retained national headsigns are not rewritten. Bounded pattern edits are not complete service coverage and
yield to full source trip projection on authoritative dates.

The production package contains `gtfs.zip`, the declared serving Parquet
relations, `manifest.json`, and bounded `diagnostics.json`. Detailed matching evidence is written only to an explicitly
requested diagnostics artifact. Coverage is reported, never gated.

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

Profiles may omit `source.route_join`. A configured join (PID:
`route_sub_agencies.txt`) supplies companion CIS line assertions and requires
its table; an omitted join provides none. Revision dates come from the adapter:
`pid-v1` reads a `_YYMMDD` suffix of the trip ID (unparseable IDs date
700101), other adapters leave revisions equal. Reviewed stop overrides and
structural matching remain available; a
unique structural route/date proof can authorize a complete source trip set,
while configured road/local modes can import unmatched service under
deterministic source-namespaced IDs. A configured join must name `cis_line_id`
as its target namespace,
provide equally sized non-empty source/lookup key arrays, and supply its table.
Equal revisions provide no recency ordering; conflicting claims remain
quarantined.

Stop grouping comes from the adapter: `pid-v1` groups by `asw_node_id`, then
`parent_station`, then the stop ID, with `asw_stop_id` as the post; `ids-jmk-v1`
groups by `parent_station`, then the stop ID. These defaults allow ordinary GTFS snapshots
without PID-specific columns. IDS JMK has a supplied adapter/profile. IDZK is
intentionally out of scope because its feed lacks the required boarding-post
hierarchy.

In the combined profile, compatible equal-priority facts coalesce and retain all
source provenance. Differing facts are quarantined and the national value
survives. Missing facts are neutral, so a PID shape can coexist with an identical
JMK schedule. Equivalent source-native trips and strongly equivalent boarding
points (same resolved place and nonblank platform code) are structurally
deduplicated. Matching-tier arrays enable existing tiers;
route precedence is companion assertion, then structural evidence. Uniqueness
and equal-call-count contextual inference are always enforced. `capabilities`
lists what a source may supply; stop names, agencies, calendars and trip
displays are never inherited into retained national objects. Complete authoritative/source-native
projections have their existing construction rules, including source agencies,
calendars and displays; approximate national stops also retain their explicitly
supported correction behavior.

IDs, report schemas and machine-readable values are stable. The overlay
records every source in the manifest `sources` array with its payload/descriptor
hashes, and the combined policy hash.
In particular, `pid_name_and_coordinates` and `pid_native` in the stop-match
report are legacy classification labels even for another source. Consult the
source identity and provenance rather than interpreting these values as feed IDs.
Human-readable diagnostics use the configured source identity.

The regional overlay command prepares one national model from every declared
source binding, resolves source-qualified claims, and writes one bundle. It
does not chain overlays of already-overlaid bundles.

# Project parts

JrUtil's main part is the central library, also called *JrUtil*. It allows the
user to read a variety of public transport data formats (currently JDF and
CZPTTCIS), convert them to GTFS and then perform various operations on the
result.

*jrutil-multitool* is a small tool whose purpose is to provide a command line
interface to commonly used functions of *JrUtil*: the JDF fix/merge/bundle
pipeline, CZPTT and regional-overlay packages, and package validation.

*jrutil.tests* contains unit and small integration tests of JrUtil.

*scripts/golden* is the bounded real-data regression and performance gate
used for refactors; see its README.

# Czech schedule facts

`gtfs.zip` is standard GTFS only. Facts that standard GTFS cannot represent,
along with identity and provenance, are available exclusively through the
typed serving relations documented in `docs/PRODUCTION_CONTRACT.md`:

| Relation | Purpose |
| --- | --- |
| `fare_zone` | Zone identity and optional fare-system ownership |
| `route_stop_zone` | Ordered route-stop-specific zone memberships |
| `call_zone` | Ordered call-specific zone memberships |
| `transfer` | Standard transfers plus waiting limits |

The selected public line number is written to `routes.txt`'s
`route_short_name`. A preferred JDF `LinExt` designation wins;
otherwise the last three digits of the six-digit CIS line ID are used.
Numeric designations have leading zeroes removed, while alphanumeric
designations retain their spelling and case.

JDF post references from `Oznacniky.txt` and text-only station numbers from
`Zasspoje.txt` are both projected as distinct child stops with standard GTFS
`parent_station` links. Typed `location` and `source_entity_map` rows preserve
the place/boarding-point hierarchy and source identity. Post IDs share one
hierarchy while retaining source semantics: `:post:id:` is an authoritative
`Oznacniky` ID and bare `:post:<value>` is a textual `Zasspoje` designation.

Raw zone lists are split, trimmed and deduplicated without attempting to infer
their IDS system. `fare_system_id` therefore remains null when ownership is
unknown. A route
distinction and raw token form the source zone identity. Standard GTFS
route-stop occurrences and source order are retained in `route_stop_zone`.
Standard `stop_times.txt` never contains
the old non-standard `stop_zone_ids` column.

Output routes group the merged versions of one CIS line. Versions that share
agency, public line number, name, route type and colours form one route, and
detour (výluka) timetables form a separate route. The group containing the
line's earliest-starting version is `jdf:route:<line>` (detours:
`jdf:route:<line>:detour`). Any other group appends eight hex characters hashed
from its semantics. Detour routes keep the route colour and use amber text
(`ffd23f`), or dark orange (`7a3500`) where amber would contrast below 3:1.
Serving `route.timetable_kind` is `regular` or `detour` for JDF routes. Trips,
zones and diagnostics keep the per-version `(line, distinction)` identity, and
route-stop keys include the version (`<route>/<distinction>/<route stop>`)
because route stop numbers are unique only within one version.

Generated intermediate identifiers consistently use colon-separated
namespaces: `jdf:agency:…`, `jdf:route:…`, `jdf:trip:…`, `jdf:stop:…`,
`jdf:zone:…`, `jdf:notice:…`, `jdf:transfer:…` and `jdf:restriction:…`.
Text components are
URI-escaped before being embedded in an identifier. Deduplicated GTFS service
patterns use the derived `gtfs:service:<weekday-bitmap>:<ordinal>` namespace.
Inferred boarding points use `jdf:stop:<stop>:est:<ordinal>`. The 1-based ordinal is assigned per parent stop by sorting
the full internal derived-location IDs; those full IDs remain in diagnostic
relations for traceability.

In CZPTT conversion, line, train-category and operator changes produce linked
GTFS trips with a shared `block_id`.

`CZAlternativeTransport=1` marks the segment departing that location as rail
replacement transport. Such segments are emitted with extended GTFS route
type `714` and ` (NAD)` appended to the route short name. A mapped passenger
line therefore becomes, for example, `U1 (NAD)`; a line-less fallback retains
the train designation, for example `Os 363784 (NAD)`. NAD trips contain only
passenger calls and serve a synthetic `BUS` platform instead of railway track
numbers; its stop description directs passengers to the operator's boarding
information. A mixed PA is split at each rail/NAD boundary; the resulting trips use separate block IDs and a
trip-specific timed transfer (`transfer_type=1`) between the exact stops served
by both trips, with `min_transfer_time=0`. Separate NAD PAs are connected to a train within ten minutes by
the `+300000` operational-number convention first, falling back to the
passenger line only when no numbered candidate applies. Tied candidates with
overlapping calendars remain unmatched.

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

`merge-jdf` requires `--gvd-year` and `--reference-date`. After resolving
version overlaps it drops timetables that expired before the reference date or
start after the GVD, and clamps the rest to the GVD bounds. The December GVD
change is a hard cutover, even for international lines that claim validity
indefinitely. Pass the same `--gvd-year` to `jdf-to-bundle` to record the GVD as
the package `service_horizon`. `regional-gtfs-overlay` rejects a base package
whose horizon belongs to a different GVD.

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
an immutable production package with `gtfs.zip`, serving Parquet relations,
bounded diagnostics and a checksummed manifest:

```text
dotnet run --project jrutil-multitool -- \
  jdf-to-bundle \
  --snapshot-descriptor=snapshot.json \
  --converter-version=<fork-commit> \
  --routing-osm-pbf=jdf-transit-routing-demand.osm.pbf \
  [--post-inference-policy=learned-v1.json] \
  JDF-input bundle-output
```

The output path must not exist. The command writes through a temporary sibling
directory and activates the result only after every file and checksum has been
produced. Conversion or packaging failures return a non-zero exit code.

Production packages use the versioned contract documented in
`docs/PRODUCTION_CONTRACT.md`:

```text
output/
├── gtfs.zip
├── serving/
├── manifest.json
└── diagnostics.json
```

Serving relations retain normalized identities, exact route-stop and call
scope, zones, notices, connection claims, restrictions, and source
provenance. The private source metadata used to build them is not a published
consumer interface.

Estimated posts are disabled by default unless `--routing-osm-pbf` is supplied.
The input must be the Osmium-prepared demand clip and have its matching
`.manifest.json` sidecar. `--no-estimated-posts` remains the complete rollback
path.

`--routing-cache=DIR` reuses routed context evidence across runs. Each entry
records the 1 km tiles of the routing graph its computation read, and it is
reused only while their fingerprints are unchanged, so a changed demand clip or
OSM update recomputes only nearby contexts and output stays byte-identical to
routing everything. A change to the routing sources invalidates the cache;
entries unused for 45 days are dropped.

For retraining, directed routing can be captured once into a policy-neutral
evidence pack without writing a bundle:

```text
jdf-to-bundle --routing-osm-pbf=clip.osm.pbf \
  --capture-post-inference-evidence=evidence --post-inference-evidence-only ...
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
`--capture-stop-region=MINLON,MINLAT,MAXLON,MAXLAT` (capture-only) routes only
contexts at stops whose precise location lies in the box. Observations, route
points and neighbour anchors stay complete, so in-region rows equal those of a
full capture. `--capture-exclude-source=PREFIX[,PREFIX…]` drops observations
whose source object ID starts with a prefix (e.g. `external:PID.csv:`), so a
training pack cannot contain the catalogue its labels come from. Restrictions
are appended to the capture-tool version (`<version>+region:…+exclude-source:…`)
and therefore to the pack ID. Such packs feed `jdf-export-post-features` for review and training.
`--export-post-context-calls=FILE` additionally writes every usable
road/tram call in the region with its evidence `context_id`, GTFS trip ID and
per-stop occurrence, for joining external call mappings.

`jdf-export-post-features --evidence=DIR [--policy=FILE] --output=DIR` writes
the evaluator's per-candidate diagnostic rows, consolidated hypotheses and one
policy's context decisions as Parquet for offline analysis and training
(`scripts/post-scorer`).
Its disk preflight is derived from the canonical deduplicated route-pattern
plan, not from the number of timetable calls. The upper bound includes every
captured relation, and atomic publication requires one temporary pack plus a
fixed safety reserve because activation is a same-volume directory rename.
Live conversion writes a temporary evidence store and reopens it through the
same validated trust boundary that feature export uses, so one evaluator serves
both. Validation covers exact Arrow types/nullability and
metadata, canonical ordering, key/foreign-key integrity, context/block/family
identity, corridor ranks and costs, unavailable sentinels, finite numeric facts,
and complete route-point attachment coverage. Policy v2 owns consolidation, hard gates, geometry,
support, consensus, alternatives and authored resolution. The heuristic
publishes one physical candidate or the parent centroid. A policy
may request at most the captured routed-excess horizon (currently 1,000 m;
the default publication gate remains 500 m).

The sole production decision boundary is a validated `PostEvidenceStore` plus a
`PostInferencePolicyV2`, returning a complete disposable
`PostInferenceResult`. The result owns replayable assignment and diagnostic-row
stores, while its hypotheses, authored positions, and counters are
complete policy outputs. `JdfToGtfs` only adapts that result to the conversion
plan; capture and bundle orchestration do not contain a second evaluator.

A policy file is either a v2 policy (heuristic scorer) or a v3 document
`{"schema_version":3,"policy":{…v2…},"scorer":{"kind":"heuristic"|"learned","model":{…}}}`.
A learned scorer (`JdfPostScorer`) is the two-stage conditional logit trained
by `scripts/post-scorer` (`model.json`, format `post-scorer-two-stage-v1`).
It replaces the per-family decision for unlabelled contexts:
- `Physical` publishes the post;
- `Area` publishes the most probable post of a confident 40 m area;
- abstention publishes the centroid.

Consolidation, hard gates and authored resolution still come from
the embedded v2 policy. The bundle manifest records the scorer and the
document's SHA-256. `JdfPostScorerTests` pins F#/Python parity on a golden
fixture (`TestData/post-scorer-golden.json`, from `python -m
post_scorer.golden`).

A `route_stop` is published at the calls' stop place. The post each trip uses
(which differs by direction) is on `trip_call.boarding_point_id`.

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

The internal JDF extraction maps `Udaje` to route notices, all typed `Caskody`
including text-free calendar designations to trip facts, and `Mistenky` to
reservation facts. `Navaznosti` records retain their source
target IDs and waiting context without guessing a canonical target.

Travel exclusions retain their source scope: `Zaslinky` produces
`route_stop` assignments and `Zasspoje` produces `trip_call` assignments.
For one trip, calls sharing the same effective group code (`§`, `A`, `B` or
`C`) may not be used as both the boarding and alighting endpoints. Importers
union route-stop and trip-call assignments rather than expecting expanded rows.

The join path is deliberately relational: GTFS identifies routes, trips and
calls; `source_call_metadata` maps a GTFS call to its JDF route-stop;
route-stop zone, transfer and restriction relations add only facts missing
from GTFS. These tables are compiler inputs; the serving relations are the
immutable import format. A runtime
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

Production Parquet relations use serving schema v3, Snappy compression and
row groups of at most 65,536 rows. Rows are in deterministic generation order;
relations are not sorted, and primary keys are validated for uniqueness. The manifest records schemas, keys,
source snapshots, row counts, byte sizes and SHA-256 for every payload.
Identical inputs, converter version and options produce identical bytes.

# Installation:

This project uses [.NET Core](https://www.microsoft.com/net).

First, download and install the
[.NET Core SDK](https://www.microsoft.com/net/download).
Using the latest version is highly recommended.

Clone the repo and run `dotnet build`. Now you can use the various utilities by
going to their directory and running `dotnet run -- ARGS...`.

# Community

To contribute to the project or to report issues go to the
[GitLab repository](https://gitlab.com/dvdkon/jrutil).

You may also visit a forum dedicated to Czech transport data (in Czech
language): https://dadof.ggu.cz
