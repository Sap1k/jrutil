# [JrUtil](https://gitlab.com/dvdkon/jrutil)

JrUtil is a library and a set of utilities for working with Czech (and Slovak)
public transport data, such as:

- scheduled timetables (JDF to GTFS, CZPTTCIS to GTFS, more to come)
- real-time timetable changes (TODO)
- vehicle positions (scraping GRAPP, viewing historical data)

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
the process's existing private bytes, reserves 25% of effective system memory
(between 1 and 4 GiB) for the OS and other applications, and also applies a
capacity ceiling. On a busy 16 GiB machine the budget therefore contracts with
live availability instead of assuming that 10 GiB can always be allocated.
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
├── diagnostics.json
└── manifest.json
```

The seven slim Parquet tables retain only source facts that would otherwise be
lost. The original route/stop/call metadata tables preserve JDF distinctions,
structured stop-name components, coordinate absence and route-stop IDs. The
additional relations preserve route-stop zone scope, textual notices,
connection context and the `§`/`A`/`B`/`C` travel-exclusion groups. Trip,
boarding-point, call and fare-zone entity mirrors are intentionally absent.

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
