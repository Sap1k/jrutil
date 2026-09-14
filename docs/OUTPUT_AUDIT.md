# Historical output audit

The audited PID and IDS JMK overlay bundle contained 70 physical files and 4,389,830,163 bytes. Its 68 manifest payloads matched their recorded sizes and SHA-256 values. The package contained about 1.81 GB of GTFS, 1.15 GB of mapping CSVs, 1.06 GB of selected-field and transfer provenance, 246 MB of reports, 84 MB of Czech extensions, and 48 MB of copied base evidence.

The largest files were compiler traces: `base_to_output_trips.csv` expanded active calendars into about 9.44 million ranges; `source_to_output_calls.csv` repeated structural alignments; `selected_fields.csv` repeated selected values and packed contributors. Oběhy did not import these CSV families. `operational_to_source_trips.csv` also required a provider-specific second join. The compiler now creates direct generic bindings instead.

The former `cz_routes`, `cz_trips`, and `cz_stops` files mixed portable enrichment, identity, and provenance. Packed identity syntax differed among JDF, overlay, and CZPTT producers. These files are removed from production. Zones and transfer waiting constraints remain public extensions; identities and attribution move to typed serving relations.

Reports such as `ambiguities`, `conflicts`, `equivalent_ties`, and `quarantine` were filtered copies of the same diagnostic event stream. Coverage reports also duplicated structural manifest counts. Production retains one bounded `diagnostics.json`; detailed events, candidate scores, edits, and traces are written only to an explicitly requested diagnostic artifact.

Base evidence remains an input to compilation, but blanket copying is removed. Required route-stop zones, notices, connection claims, restrictions, source calls, and operational facts must be projected before staging evidence is discarded. Empty post-estimation skeletons are not production relations.

The prior README statements that combined overlay preparation was future work and that metadata Parquet sidecars were the immutable consumer interface contradicted implemented behavior and this contract. The durable consumer interface is the finalized production package.
