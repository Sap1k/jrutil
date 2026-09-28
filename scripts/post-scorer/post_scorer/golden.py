"""Golden scorer fixture for the JrUtil port (JdfPostScorer.scoreStop).

Usage:
    uv run python -m post_scorer.golden --candidates CANDIDATES.parquet --model MODEL.json \
        --stops 40 --output golden.json

Picks a deterministic sample of stops (all complex stops first, then simple ones, by
stable hash) and writes, per stop, the context movement keys, every candidate's
stage-1 features, the stage-1/stage-2 probabilities and the decisions. The JrUtil test
feeds the same contexts and candidates to `scoreStop` and compares.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import polars as pl

from post_scorer.model import TwoStageModel, decisions, post_mass

GOLDEN_FORMAT = "post-scorer-golden-v1"


def _stop_order(stop_id) -> str:
    return hashlib.sha256(str(stop_id).encode()).hexdigest()


def sample_stops(table: pl.DataFrame, count: int) -> list:
    """Half complex (>= 3 posts), half simple stops of moderate size, by stable hash."""
    # Stops with at most 150 candidate rows keep the fixture small.
    stops = (table.group_by("stop_id").agg(pl.col("stop_post_count").first(), pl.len().alias("rows"))
             .filter(pl.col("rows") <= 150))
    def ordered(part: pl.DataFrame) -> list:
        return sorted(part["stop_id"].to_list(), key=_stop_order)
    complex_stops = ordered(stops.filter(pl.col("stop_post_count") >= 3))[: count - count // 2]
    return complex_stops + ordered(stops.filter(pl.col("stop_post_count") < 3))[: count - len(complex_stops)]


def golden(table: pl.DataFrame, model: TwoStageModel, stops: list) -> dict:
    table = table.filter(pl.col("stop_id").is_in(stops))
    first = model.stage1.probabilities(table).select("context_id", "hypothesis_id",
                                                     pl.col("probability").alias("stage1_probability"))
    scored = post_mass(model.probabilities(table))
    result = decisions(scored, model.thresholds)
    scored = scored.join(first, on=["context_id", "hypothesis_id"])
    features = model.stage1.columns
    output = []
    for stop in sorted(stops, key=str):
        rows = scored.filter(pl.col("stop_id") == stop).sort("context_id", "hypothesis_id")
        contexts = rows.select("context_id", "previous_stop", "next_stop", "stop_post_count").unique(
            "context_id").sort("context_id")
        output.append({
            "stop_id": str(stop),
            "contexts": [{"context_id": str(r["context_id"]),
                          "previous_stop": None if r["previous_stop"] is None else str(r["previous_stop"]),
                          "next_stop": None if r["next_stop"] is None else str(r["next_stop"]),
                          "stop_post_count": int(r["stop_post_count"])} for r in contexts.iter_rows(named=True)],
            "candidates": [{"context_id": str(r["context_id"]), "hypothesis_id": str(r["hypothesis_id"]),
                            "post_id": str(r["post_id"]), "area_id": str(r["area_id"]),
                            "is_platform": bool(r["is_platform"]),
                            "features": [float(r[c]) for c in features],
                            "stage1_probability": float(r["stage1_probability"]),
                            "stage2_probability": float(r["probability"])} for r in rows.iter_rows(named=True)],
            "decisions": [{"context_id": str(r["context_id"]), "resolution": r["model_resolution"],
                           "hypothesis_id": None if r["model_resolution"] == "Abstain" else str(r["hypothesis_id"]),
                           "post_probability": float(r["post_probability"]),
                           "area_probability": float(r["area_probability"])}
                          for r in result.filter(pl.col("stop_id") == stop).sort("context_id").iter_rows(named=True)],
        })
    return {"format": GOLDEN_FORMAT, "model": model.to_json(), "stops": output}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--candidates", type=Path, required=True)
    parser.add_argument("--model", type=Path, required=True)
    parser.add_argument("--stops", type=int, default=40)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    model = TwoStageModel.from_json(json.loads(args.model.read_text(encoding="utf8")))
    table = pl.read_parquet(args.candidates)
    document = golden(table, model, sample_stops(table, args.stops))
    args.output.write_text(json.dumps(document, separators=(",", ":")) + "\n", encoding="utf8")
    print(f"{len(document['stops'])} stops, {sum(len(s['candidates']) for s in document['stops'])} candidates")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
