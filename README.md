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
| `cz_routes.txt` | CIS line identity, passenger-facing line number, raw fare zones and JDF provenance |
| `cz_trips.txt` | CIS line/trip identity and source-trip provenance |
| `cz_stops.txt` | Source stop-place, CIS stop and post identities |
| `cz_stop_zones.txt` | Normalized, route-scoped stop-to-zone memberships |

The selected public line number is written to both `cz_routes.txt` and
`routes.txt`'s `route_short_name`. A preferred JDF `LinExt` designation wins;
otherwise the last three digits of the six-digit CIS line ID are used.
Numeric designations have leading zeroes removed, while alphanumeric
designations retain their spelling and case.

JDF post references from `Oznacniky.txt` and text-only station numbers from
`Zasspoje.txt` are both projected as distinct child stops. Raw zone lists are
split, trimmed and deduplicated without attempting to infer their IDS system.
`ids_system_id` therefore remains empty for this conversion. A route
distinction and raw token form the source zone identity. Standard GTFS
`stops.zone_id` is populated only when a stop has exactly one such identity;
plural memberships remain authoritative in `cz_stop_zones.txt`. The bundle's
zone Parquet contains only the JDF route-stop provenance and source order that
cannot be reconstructed from that extension. Standard `stop_times.txt` never contains the old
non-standard `stop_zone_ids` column.

JDF stop numbers are not always global CIS identifiers. Pass `--stop-ids-cis`
only for a batch known to use the national CIS stop registry:

```text
dotnet run --project jrutil-multitool -- \
  jdf-to-gtfs --stop-ids-cis JDF-input GTFS-output
```

The extension files are optional in the shared GTFS model. CZPTT conversion
does not populate them yet.

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
├── source_stop_zone_metadata.parquet
├── diagnostics.json
└── manifest.json
```

The four slim Parquet tables retain only source facts that would otherwise be
lost: route distinction/source agency/validity, structured JDF stop-name
components and original coordinate absence, JDF route-stop IDs behind emitted
GTFS calls, and route-stop provenance/order behind normalized zone membership.
Trip, boarding-point and fare-zone entity mirrors are intentionally absent.

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
