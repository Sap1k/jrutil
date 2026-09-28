"""Turn review-map corrections into labels for the candidate table.

Usage:
    uv run python -m post_scorer.corrections --corrections corrections.json \
        --evidence EVIDENCE_DIR --output LABELS_DIR

A correction names a movement (stop, line, direction, previous and next stop)
and either the correct post's coordinates or "none". Every evidence context of
that movement gets the label. Positives carry the post's coordinates as the
source post, so `features --labels LABELS_DIR` marks the nearest candidate and
its same-post duplicates, exactly as for GTFS-derived labels.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import polars as pl

from post_scorer.labels import NO_CORRECT_CANDIDATE, jdf_stop_number

REVIEWED = "reviewed"


def corrections_to_labels(corrections: list[dict], contexts: pl.DataFrame) -> pl.DataFrame:
    if not corrections:
        return pl.DataFrame(schema={"context_id": pl.Utf8, "label": pl.Utf8,
                                    "source_lat": pl.Float64, "source_lon": pl.Float64})
    reviewed = pl.DataFrame(
        [{"stop_id": int(c["stop_id"]), "line_id": str(c["line_id"]), "direction": int(c["direction"]),
          "previous_key": c.get("previous_stop_id") or "", "next_key": c.get("next_stop_id") or "",
          "verdict": c["verdict"], "source_lat": c.get("latitude"), "source_lon": c.get("longitude"),
          "recorded_at": c.get("recorded_at") or ""} for c in corrections],
        schema_overrides={"source_lat": pl.Float64, "source_lon": pl.Float64},
    ).sort("recorded_at").unique(["stop_id", "line_id", "direction", "previous_key", "next_key"], keep="last")
    keyed = contexts.select(
        "context_id", jdf_stop_number("gtfs_stop_place_id").alias("stop_id"), pl.col("line_id").cast(pl.Utf8),
        pl.col("direction").cast(pl.Int64),
        pl.col("context_previous_stop_id").fill_null("").alias("previous_key"),
        pl.col("context_next_stop_id").fill_null("").alias("next_key"),
    )
    return (
        keyed.join(reviewed.with_columns(pl.col("direction").cast(pl.Int64)),
                   on=["stop_id", "line_id", "direction", "previous_key", "next_key"])
        .select(
            "context_id",
            pl.when(pl.col("verdict") == "none").then(pl.lit(NO_CORRECT_CANDIDATE)).otherwise(pl.lit(REVIEWED))
            .alias("label"),
            "source_lat", "source_lon",
        )
        .sort("context_id")
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--corrections", type=Path, required=True)
    parser.add_argument("--evidence", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.output.exists():
        parser.error(f"output already exists: {args.output}")
    corrections = json.loads(args.corrections.read_text(encoding="utf8"))
    labels = corrections_to_labels(corrections, pl.read_parquet(args.evidence / "contexts.parquet"))
    args.output.mkdir(parents=True)
    labels.write_parquet(args.output / "labels.parquet")
    summary = {"corrections": len(corrections), "labelled_contexts": labels.height,
               "no_correct_candidate": int((labels["label"] == NO_CORRECT_CANDIDATE).sum())}
    (args.output / "label-report.json").write_text(json.dumps(summary, indent=2) + "\n", encoding="utf8")
    print(json.dumps(summary))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
