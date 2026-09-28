# post-scorer

Offline labels, features and training for a learned stop-post scorer. The current
routed estimator often picks a post whose routing penalty is lower than the station
bays, for example at `Most, nádraží` and `Litvínov, nádraží`. The scorer is trained
where posts are known (PID, IDS JMK) and evaluated where they are not (Ústecký kraj).
JrUtil runs the same model when the post-inference policy is a v3 document with a
learned scorer (`repo/src/obehy/data/post-inference/learned-v1.json`).

## Data flow

| Step | Tool | Output |
|---|---|---|
| Region capture, training catalogues excluded | `jdf-to-bundle --capture-post-inference-evidence --post-inference-evidence-only --capture-stop-region=… --capture-exclude-source=… --export-post-context-calls=…` | evidence-v2 pack plus per-call context IDs |
| Evaluator facts | `jdf-export-post-features --evidence=… --output=…` | `diagnostic_scores`, `hypotheses`, baseline `assignments` |
| JDF↔regional call mappings | `regional-gtfs-overlay` (one labelled source) on a post-free base bundle | `traces/source_to_output_{trips,calls}.csv` |
| Traffic weights | `python -m post_scorer.weights` | departures per context over the GVD year |
| Labels | `python -m post_scorer.labels` | `labels.parquet`, `label-report.json` |
| Candidate table | `python -m post_scorer.features` (`--labels` or `--authored-labels`) | one row per (context, candidate) |
| Transfer check | `python -m post_scorer.model crossval --region NAME=CANDIDATES[,WEIGHTS]…` | train-on-one/test-on-other matrix, plus pooled |
| Train / evaluate | `python -m post_scorer.model train --region …` (pooled) or `--candidates` | `model.json`, validation metrics |
| JrUtil parity fixture | `python -m post_scorer.golden` | `jrutil.tests/TestData/post-scorer-golden.json` |

`run-regions.ps1` runs the whole chain and skips steps whose outputs already exist.

## Labels
- **PID and IDS JMK (training):** the overlay's call mapping (edit-free bindings only), then the base
  `stop_times` (JDF stop and its occurrence within the trip), then the context-call
  export. Each context takes its majority regional post; contexts where posts disagree (share
  under 0.8) are dropped.
  - Every candidate within 20 m of the PID post is correct, because one physical post is
    often mapped more than once (OSM platform, stop position, catalogue points).
  - If no candidate lies within 35 m, the label is `no-correct-candidate`.
  - Contexts whose nearest candidate falls between 20 and 35 m are dropped as ambiguous.
  - Posts more than 300 m from every candidate are treated as alignment errors and dropped.
  - The mappings come from the overlay's diagnostics artifact
    (`--diagnostics-out --diagnostic-traces`, directory `traces/`). The production package
    does not carry them.
- **Authored (evaluation anywhere):** a JDF stanoviště number (`num:N`) matched to the one
  candidate at the stop whose OSM `local_ref` is `N`.

## Leakage guards
- The regional catalogues (`external:PID.csv:`, `external:MPVNet_PID.csv:`,
  `external:IDSJMK.csv:`) are excluded at capture. They contain every bay of that region.
- Features never include source identity (catalogue, `sourceKind`, source-support weight)
  or network-specific OSM references such as `ref:PID`.
- The train/validation split is by stop (deterministic hash). A stop captured by two
  regions is pooled from the first one only. Ústecký kraj is used only for evaluation.

## Physical posts and sources
- **Stop-level sources:** some catalogues publish one point per stop, not per post
  (LibereckyKraj, UsteckyKraj, MapaIREDO, MPVNet_JIKORD, PlzenskyKraj, gapfill and
  others). They are measured per evidence pack: at stops where OSM maps two or more
  posts, a stop-level source almost never has two points. Candidates backed only by
  such sources are not posts and leave the choice set.
- **Post clusters:** each stop's remaining candidates are grouped by closest-first
  complete linkage.
  - A post never contains the same source twice (a source maps a post once).
  - Its diameter stays within 25 m.
  - Differing `local_ref` values never merge.
  - Two OSM bays 4 m apart stay separate, while four sources spread over 17 m merge.
- **Source support:** the number of independent sources in a post is a feature. A point
  only one source knows about is usually misplaced.
- **Mode gate:** tram-only posts (and modeless points within 15 m of them) are rejected
  for bus lines by the JrUtil evaluator.

## Model
A conditional logit: a softmax over each context's candidates plus a "no correct
candidate" alternative with a learned constant utility. Training maximises the
probability mass on the correct set, which can contain several candidates. Features are standardised, and
the coefficients and scaling are exported in `model.json` for a deterministic port to F#.
**Two stages.**
- Stage 1 scores each context alone.
- Stage 2 adds what related contexts at the same stop believe, from stage-1
  probabilities (`collective.py`):
  - `reverse_mass`: how strongly the reverse movement favours this post (learned
    negative, since opposite directions rarely share a post);
  - `same_movement_mass`: agreement among other lines making the same movement.
- Stage 2 is fitted on out-of-fold stage-1 scores (folds by stop).

**Decisions.**
- A post is published when its probability mass (summed over its duplicates) reaches
  the threshold for its stratum.
- There are two strata, simple stops (up to 2 posts) and complex stops (3 or more
  posts). Each stratum's threshold is calibrated on validation to the target precision
  (`--target-precision`, default 0.9, traffic-weighted when weights are given), so ambiguous
  terminals abstain instead of guessing.
- Otherwise, when the area (posts within 40 m, never mixing tram and road posts) holding
  most of the mass reaches the area threshold, the area's most probable post is published
  (`Area`). The area threshold is calibrated on "the true post lies in the area".
- Otherwise the context abstains (the stop centroid in GTFS).

**Frozen features** (the JrUtil port reproduces exactly these): `side`, `alignment`,
`proximity`, `corridor_distance`, `routed_fit`, `routed_excess_metres_capped`,
`excess_minus_context_min`, `eligible`, `has_local_ref`, `is_bay`, `post_support`,
`post_support_deficit`, `is_tram`, `is_trolleybus`, `anchor_previous_missing`,
`anchor_next_missing`, `service_edge_count`; stage 2 adds the six collective columns.
Changing them needs a new model format and a JrUtil change.

The **street-over-bay** metric needs no labels. It counts publications of a non-bay
candidate at a stop that has bays. A bay is a candidate inside an `amenity=bus_station`
area, or a numbered platform at a stop with at least three numbered platforms. The
second rule matters because Litvínov and Most have no station polygon.

## Checks
```bash
uv run --group dev pytest -q
```
