# Regional overlay memory check

Build once, then measure without concurrent builds or audits:

```powershell
dotnet build scripts/overlay-profile/overlay-profile.fsproj
dotnet scripts/overlay-profile/bin/Debug/net10.0/overlay-profile.dll POLICY YEAR SOURCE_ID PAYLOAD DESCRIPTOR BASE OUTPUT YYYYMMDD
```

The runner calls the same single-source library entry point as the multitool. It
does not change GC settings or impose a heap limit. It samples process working
set, private bytes, managed memory, allocation totals and scratch-file sizes every
100 ms and at stage boundaries. Process peak working set also captures shorter
resident-memory peaks. Private-memory peaks and scratch sizes are sampled.

Outputs are `OUTPUT.memory.csv` and `OUTPUT.summary.json`, outside the bundle.
An explicit audit date makes reruns independent of the current date. A failed run
still writes its measurement summary and exits with an error.

## Verified PID fixture, 2026-09-11

The two sequential compiled-runner executions used GVD 2026 and audit date
20260910. Both met the 2 GiB process-memory limit:

| Run | Peak working set | Peak private bytes | Runtime | Peak scratch |
| --- | ---: | ---: | ---: | ---: |
| 1 | 1.733 GiB | 1.789 GiB | 272.2 s | 296.7 MiB |
| 2 | 1.770 GiB | 1.875 GiB | 292.6 s | 296.0 MiB |

Both removed their scratch directories. All 63 manifest-listed files were
byte-identical between these runs, and their checksums were independently checked.
The full test project passed 230 tests; the multitool built and its overlay CLI
smoke test passed.

Comparison with the frozen working-tree baseline verified all 63 manifest-listed
files: GTFS and extensions are byte-identical; 16 mapping, provenance and report
files differ only in row order, with exact row multiplicities preserved. The
independent snapshot audit verified 47,403 of 47,419 instances, preserving the 16
known omissions and reporting zero duplicate output instances. The independent
reference audit checked 12,834,091 calls and reported zero invalid references.

Pinned identities:

- PID payload SHA-256: `117881a201cb82db5918791bf9d0cb775c82dfc40eafc46a899b33bec52f5833`
- Base manifest SHA-256: `15a9d3f9a680e4251b949b1eceff077af94f66c6c0b4d3068f4b7f4fc920342d`
- Source retrieval: `2026-09-10T12:31:51.5127266+00:00`

The pre-change working-tree DLL and complete baseline output were frozen before
editing. Its diagnostic run reached 6.44 GiB working set; the earlier unprofiled
measurement was 6.52 GiB and 327 s. That baseline used F# Interactive, whereas the
acceptance runs above use this compiled runner. Do not interpret the timing or
memory difference as a controlled comparison of hosts. Heap collection also
perturbed the diagnostic baseline's runtime. Intermediate F# Interactive runs
were diagnostic only and did not establish acceptance.

Heap samples and phase measurements identified CSV dictionaries/strings, retained
shape geometry, per-trip calendars, and mapping/report materialization. Shapes
now spill before source-call loading; selections are resolved per target trip;
date sets and exact alignments are shared; evidence and call mappings stream.
Repeated call/trip vocabulary is pooled locally rather than globally interned.

One collection remains after analysis, beyond the method boundary that releases
matching indexes. A measured diagnostic run reduced managed memory there from
about 1.26 GiB to 0.81 GiB. Routine collections inside projection/writing were
removed. The retained checkpoint supports these lifetime changes; it is not a
per-row memory-control mechanism.

Compare a baseline and a new output with:

```powershell
python scripts/verify_regional_overlay.py BASELINE OUTPUT
```

This requires exact transit/evidence bytes, unchanged manifest semantics, and
exact report/mapping/provenance row multiplicities. Only their ordering may vary.
The verifier uses a temporary SQLite file to keep its own memory bounded and
checks both manifests independently. It should run after memory measurement.
