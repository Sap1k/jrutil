# JrUtil production package contract

JrUtil production packages use `bundle_format: jrutil-production`, bundle version 1, serving schema version 2, and extension schema version 2. The checked-in JSON contracts are normative. The F# schema declarations and writer are tested against them.

The package inventory is closed:

```text
output/
├── gtfs.zip
├── extensions/
│   ├── cz_zones.txt
│   ├── cz_route_stop_zones.txt
│   ├── cz_call_zones.txt
│   └── cz_transfer_constraints.txt
├── serving/
│   └── <37 declared relations>.parquet
├── manifest.json
└── diagnostics.json
```

`gtfs.zip` is the only standard GTFS representation. Its entries are flat, ordered by ordinal file name, and have a fixed ZIP timestamp. `transfers.txt` contains only standard GTFS fields. Waiting constraints are projected to `cz_transfer_constraints.txt` and `serving/transfer.parquet`.

The four public extensions carry portable schedule enrichment. They never carry provider trip, route, or stop identities. Identity is normalized into serving relations.

All serving Parquet relations exist even when empty. Their exact field order, types, nullability, primary keys, and sort keys are in [serving-v2.json](../contracts/serving-v2.json). Strings are opaque, case-sensitive UTF-8 values. Dates are inclusive service dates. Times are signed `int32` seconds from service-day midnight and may exceed 86,400. Repeated facts use repeated rows; production cells do not contain encoded lists.

Trip applicability requires its validity envelope, its binding calendar, and the target trip calendar to be active. `binding_id` is a SHA-256 digest over a versioned canonical serialization of owner, namespace, source key, target, applicability, status, and context variant. It excludes build time and diagnostic scores. `source_call_map` always refers to a trip binding and retains the source's actual sequence value.

The manifest inventories every payload except itself with byte size and SHA-256, declares every relation, and records contract-valid and publication-eligible independently. A calibration package may be contract-valid while remaining ineligible for publication.

Detailed compiler reports are a separate artifact created only with `--diagnostics-out`. `--diagnostic-traces` additionally includes large trace mappings. Diagnostic selection cannot change production bytes.

Ownership is divided as follows:

- adapters parse syntax and retain source facts, identifiers, calendars, and sequences;
- the national/overlay compiler matches identities, aligns calls, slices validity, preserves admissible ambiguity, and enforces duplicate and rail rules;
- the package writer validates and serializes the finalized model atomically;
- a static importer validates version 2 and bulk-loads the declared relations;
- the central realtime resolver applies operating date and context to candidate bindings;
- connectors parse payloads, normalize documented key semantics, and emit claims;
- diagnostic tooling explains compiler decisions outside the production package.

Operating-date inference, delay calculation, vehicle fusion, and claim arbitration are consumer responsibilities.
