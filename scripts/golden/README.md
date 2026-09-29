# Golden regression gate

These scripts guard refactors against output and performance regressions. They only use bounded
inputs; never point them at the full national feed.

1. `prepare.py` freezes a deterministic subset of real snapshots under `<root>/inputs`:
   - about 1,000 JDF batches: the Prague `100…` and South Moravian `72…` lines (every third
     batch), every 40th VLD batch and every 10th dráhy batch;
   - about 4% of the CZPTT trains, chosen by a hash of the TR identifier;
   - the PID and IDS JMK snapshots, OSM extracts, geodata and policy files.

   The demand-clipped routing extract (`inputs/osm/jdf-transit-routing-demand.osm.pbf` and its
   manifest) is created once from the baseline `merge-posts` output with the Oběhy Osmium
   helpers.
2. `run.ps1` publishes the multitool (ServerGC, TieredPGO) to `<root>/bin/<label>`, runs the
   stages and records the median wall time and peak private bytes in `<root>/perf/<label>.json`.
   The stages are two JDF chains plus CZPTT:
   - `fix → merge → bundle → overlay`
   - `fix-posts → merge-posts → bundle-posts`
   - `czptt`

   Use `-Repeat 3` when the numbers will gate a change.
3. `gate.ps1` compares a candidate with the baseline:
   - `compare-packages --semantic` (or `--byte-identical`) on the `bundle`, `bundle-posts`,
     `overlay` and `czptt` packages;
   - a performance budget of +5% wall time and +5% peak private bytes per stage;
   - a failure if any stage logs errors.

Intended output changes go into an expectation file (`expect/*.txt`) with lines like
`file extensions/*` or `relation location`, so every other difference still fails.

```powershell
pwsh scripts/golden/run.ps1 -Root E:/Git/obehy/work/refactor-golden -Label baseline -Repeat 3
pwsh scripts/golden/run.ps1 -Root E:/Git/obehy/work/refactor-golden -Repeat 3
pwsh scripts/golden/gate.ps1 -Root E:/Git/obehy/work/refactor-golden -Baseline baseline -Candidate <label>
```
