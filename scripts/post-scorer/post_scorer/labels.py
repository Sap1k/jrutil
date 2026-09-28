"""Derive stop-post labels for JDF evidence contexts from a regional GTFS overlay.

Usage:
    uv run python -m post_scorer.labels \
        --evidence EVIDENCE_DIR --context-calls CONTEXT_CALLS.parquet \
        --mappings OVERLAY_MAPPINGS_DIR --base-gtfs BASE_BUNDLE/gtfs.zip \
        --source-gtfs PID_GTFS.zip --source-id pid-gtfs --output LABELS_DIR

Join chain, all on exact identifiers:
    overlay source_to_output_calls (PID trip call -> output trip call)
    -> source_to_output_trips (output trip -> base JDF trip; edit-free bindings only)
    -> base GTFS stop_times (base call ordinal -> JDF stop and its occurrence in the trip)
    -> JrUtil context-call export (trip, stop, occurrence -> evidence context_id)
Each context takes the majority source post. A context is labelled with the
route point nearest that post within the tolerance, or `no-correct-candidate`.
"""

from __future__ import annotations

import argparse
import json
import tempfile
import zipfile
from dataclasses import asdict, dataclass
from pathlib import Path

import polars as pl

STOP_ID_PATTERN = r"(?:jdf|cis):stop:(\d+)"
NO_CORRECT_CANDIDATE = "no-correct-candidate"


@dataclass(frozen=True)
class LabelSettings:
    # Candidates within `tolerance_metres` of the source post are the same
    # physical post (OSM platform, stop position and catalogue points often sit
    # a few metres apart). A context whose nearest candidate is farther than
    # `no_candidate_metres` has no correct candidate; the band in between is
    # too ambiguous to label and is dropped.
    tolerance_metres: float = 20.0
    no_candidate_metres: float = 35.0
    minimum_share: float = 0.8
    minimum_calls: int = 1
    # A source post this far from every candidate of its JDF stop indicates a
    # call-alignment error rather than a missing candidate.
    alignment_guard_metres: float = 300.0


def haversine_metres(lat1: pl.Expr, lon1: pl.Expr, lat2: pl.Expr, lon2: pl.Expr) -> pl.Expr:
    radius = 6_371_008.8
    phi1 = lat1.radians()
    phi2 = lat2.radians()
    dphi = (lat2 - lat1).radians()
    dlambda = (lon2 - lon1).radians()
    a = (dphi / 2).sin().pow(2) + phi1.cos() * phi2.cos() * (dlambda / 2).sin().pow(2)
    return 2 * radius * a.sqrt().arcsin()


def jdf_stop_number(column: str) -> pl.Expr:
    return pl.col(column).str.extract(STOP_ID_PATTERN, 1).cast(pl.Int64)


def base_call_occurrences(stop_times: pl.DataFrame) -> pl.DataFrame:
    """Base GTFS calls with 1-based call ordinal and per-stop occurrence."""
    return (
        stop_times.select(
            pl.col("trip_id").cast(pl.Utf8),
            pl.col("stop_sequence").cast(pl.Int64),
            pl.col("stop_id").cast(pl.Utf8),
        )
        .sort("trip_id", "stop_sequence")
        .with_columns(
            (pl.int_range(pl.len()).over("trip_id") + 1).alias("call_ordinal"),
            jdf_stop_number("stop_id").alias("jdf_stop_id"),
        )
        .with_columns(pl.int_range(pl.len()).over("trip_id", "jdf_stop_id").alias("stop_occurrence"))
        .select("trip_id", "call_ordinal", "jdf_stop_id", "stop_occurrence")
    )


def source_calls_on_base(
    call_mappings: pl.DataFrame,
    trip_mappings: pl.DataFrame,
    source_stops: pl.DataFrame,
    source_id: str,
) -> pl.DataFrame:
    """Source (e.g. PID) calls placed on edit-free base JDF trips."""
    trips = (
        trip_mappings.filter(
            (pl.col("source_id") == source_id)
            & (pl.col("base_trip_id").fill_null("") != "")
            & (pl.col("pattern_edits").cast(pl.Utf8) == "0")
        )
        .select("source_trip_id", "output_trip_id", "base_trip_id")
        .unique()
    )
    stops = source_stops.select(
        pl.col("stop_id").cast(pl.Utf8).alias("source_stop_id"),
        pl.col("stop_lat").cast(pl.Float64).alias("source_lat"),
        pl.col("stop_lon").cast(pl.Float64).alias("source_lon"),
    )
    return (
        call_mappings.filter(pl.col("source_id") == source_id)
        .select(
            pl.col("source_trip_id").cast(pl.Utf8),
            pl.col("source_stop_id").cast(pl.Utf8),
            pl.col("output_trip_id").cast(pl.Utf8),
            pl.col("output_call_ordinal").cast(pl.Int64),
        )
        .join(trips, on=["source_trip_id", "output_trip_id"], how="inner")
        .join(stops, on="source_stop_id", how="inner")
        .unique(["base_trip_id", "output_call_ordinal", "source_stop_id"])
    )


def context_source_posts(
    source_calls: pl.DataFrame, base_calls: pl.DataFrame, context_calls: pl.DataFrame
) -> pl.DataFrame:
    """One row per (context call, source post) observation."""
    placed = source_calls.join(
        base_calls,
        left_on=["base_trip_id", "output_call_ordinal"],
        right_on=["trip_id", "call_ordinal"],
        how="inner",
    )
    calls = context_calls.select(
        pl.col("gtfs_trip_id").cast(pl.Utf8),
        pl.col("stop_id").cast(pl.Int64).alias("jdf_stop_id"),
        pl.col("stop_occurrence").cast(pl.Int64),
        pl.col("context_id").cast(pl.Utf8),
    )
    return placed.join(
        calls,
        left_on=["base_trip_id", "jdf_stop_id", "stop_occurrence"],
        right_on=["gtfs_trip_id", "jdf_stop_id", "stop_occurrence"],
        how="inner",
    )


def majority_posts(posts: pl.DataFrame, settings: LabelSettings) -> pl.DataFrame:
    counts = posts.group_by("context_id", "jdf_stop_id", "source_stop_id").agg(
        pl.len().alias("calls"),
        pl.col("source_lat").first(),
        pl.col("source_lon").first(),
    )
    totals = counts.group_by("context_id").agg(
        pl.col("calls").sum().alias("total_calls"),
        pl.col("source_stop_id").n_unique().alias("distinct_source_posts"),
    )
    return (
        counts.sort(["context_id", "calls", "source_stop_id"], descending=[False, True, False])
        .group_by("context_id", maintain_order=True)
        .first()
        .join(totals, on="context_id")
        .with_columns((pl.col("calls") / pl.col("total_calls")).alias("majority_share"))
    )


def attach_labels(
    majorities: pl.DataFrame, route_points: pl.DataFrame, settings: LabelSettings
) -> pl.DataFrame:
    points = route_points.select(
        jdf_stop_number("gtfs_stop_place_id").alias("jdf_stop_id"),
        pl.col("route_point_id"),
        pl.col("latitude").alias("point_lat"),
        pl.col("longitude").alias("point_lon"),
    )
    nearest = (
        majorities.select("context_id", "jdf_stop_id", "source_lat", "source_lon")
        .join(points, on="jdf_stop_id", how="inner")
        .with_columns(
            haversine_metres(
                pl.col("source_lat"), pl.col("source_lon"), pl.col("point_lat"), pl.col("point_lon")
            ).alias("distance_metres")
        )
        .sort(["context_id", "distance_metres", "route_point_id"])
        .group_by("context_id", maintain_order=True)
        .first()
        .select("context_id", "route_point_id", "distance_metres")
    )
    return (
        majorities.join(nearest, on="context_id", how="left")
        .with_columns(
            pl.when(pl.col("distance_metres") <= settings.tolerance_metres)
            .then(pl.col("route_point_id"))
            .when(pl.col("distance_metres") > settings.no_candidate_metres)
            .then(pl.lit(NO_CORRECT_CANDIDATE))
            .otherwise(None)
            .alias("label"),
            (
                pl.col("distance_metres").is_null()
                | (pl.col("distance_metres") > settings.alignment_guard_metres)
            ).alias("alignment_suspect"),
        )
        .rename({"route_point_id": "nearest_route_point_id", "distance_metres": "nearest_distance_metres"})
    )


@dataclass
class LabelReport:
    source_calls_on_edit_free_base_trips: int = 0
    context_call_observations: int = 0
    contexts_with_source_posts: int = 0
    contexts_in_evidence: int = 0
    evidence_contexts_with_posts: int = 0
    dropped_disagreeing: int = 0
    dropped_alignment_suspect: int = 0
    dropped_ambiguous_distance: int = 0
    labelled_candidate: int = 0
    labelled_no_correct_candidate: int = 0


def build_labels(
    *,
    evidence_contexts: pl.DataFrame,
    route_points: pl.DataFrame,
    context_calls: pl.DataFrame,
    call_mappings: pl.DataFrame,
    trip_mappings: pl.DataFrame,
    base_stop_times: pl.DataFrame,
    source_stops: pl.DataFrame,
    source_id: str,
    settings: LabelSettings = LabelSettings(),
) -> tuple[pl.DataFrame, LabelReport]:
    report = LabelReport()
    source_calls = source_calls_on_base(call_mappings, trip_mappings, source_stops, source_id)
    report.source_calls_on_edit_free_base_trips = source_calls.height
    posts = context_source_posts(source_calls, base_call_occurrences(base_stop_times), context_calls)
    report.context_call_observations = posts.height
    majorities = majority_posts(posts, settings)
    report.contexts_with_source_posts = majorities.height
    evidence_ids = evidence_contexts.select(pl.col("context_id").cast(pl.Utf8)).unique()
    report.contexts_in_evidence = evidence_ids.height
    majorities = majorities.join(evidence_ids, on="context_id", how="semi")
    report.evidence_contexts_with_posts = majorities.height
    agreeing = majorities.filter(
        (pl.col("majority_share") >= settings.minimum_share)
        & (pl.col("total_calls") >= settings.minimum_calls)
    )
    report.dropped_disagreeing = majorities.height - agreeing.height
    labelled = attach_labels(agreeing, route_points, settings)
    report.dropped_alignment_suspect = labelled.filter(pl.col("alignment_suspect")).height
    labelled = labelled.filter(~pl.col("alignment_suspect")).drop("alignment_suspect")
    report.dropped_ambiguous_distance = labelled.filter(pl.col("label").is_null()).height
    labelled = labelled.filter(pl.col("label").is_not_null())
    report.labelled_no_correct_candidate = labelled.filter(pl.col("label") == NO_CORRECT_CANDIDATE).height
    report.labelled_candidate = labelled.height - report.labelled_no_correct_candidate
    return labelled.sort("context_id"), report


def authored_local_ref_labels(
    contexts: pl.DataFrame, hypotheses: pl.DataFrame, osm_tags: pl.DataFrame
) -> pl.DataFrame:
    """Exact labels where JDF authors a post number and one candidate carries it as OSM local_ref.

    Needs no regional GTFS, so it also serves regions such as Ústecký kraj.
    Ambiguous numbers (several candidates with the same local_ref) are dropped.
    """
    authored = contexts.filter(pl.col("authored_post_key").fill_null("").str.starts_with("num:")).select(
        "context_id",
        jdf_stop_number("gtfs_stop_place_id").alias("stop_id"),
        pl.col("authored_post_key").str.strip_prefix("num:").str.strip_chars().alias("post_number"),
    )
    numbered = (
        hypotheses.select(
            "hypothesis_id",
            "stop_id",
            "representative_route_point_id",
            pl.col("member_observation_ids").str.split(";").alias("observation_id"),
        )
        .explode("observation_id", empty_as_null=True)
        .join(osm_tags.select("observation_id", "local_ref"), on="observation_id")
        .filter(pl.col("local_ref").is_not_null())
        .select("hypothesis_id", "stop_id", "representative_route_point_id",
                pl.col("local_ref").str.strip_chars().alias("post_number"))
        .unique()
    )
    matches = authored.join(numbered, on=["stop_id", "post_number"])
    unique = matches.group_by("context_id").agg(
        pl.col("hypothesis_id").n_unique().alias("matches"), pl.col("representative_route_point_id").first()
    )
    return unique.filter(pl.col("matches") == 1).select(
        "context_id", pl.col("representative_route_point_id").alias("label")
    ).sort("context_id")


def _read_zip_csv(archive: Path, name: str, columns: list[str], scratch: Path) -> pl.DataFrame:
    with zipfile.ZipFile(archive) as zipped:
        target = zipped.extract(name, scratch)
    return pl.read_csv(target, columns=columns, infer_schema=False)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--evidence", type=Path, required=True)
    parser.add_argument("--context-calls", type=Path, required=True)
    parser.add_argument("--mappings", type=Path, required=True, help="overlay mappings directory (diagnostics traces/ or compiled staging mappings/)")
    parser.add_argument("--base-gtfs", type=Path, required=True, help="gtfs.zip of the base JDF bundle")
    parser.add_argument("--source-gtfs", type=Path, required=True, help="regional GTFS zip with posts")
    parser.add_argument("--source-id", default="pid-gtfs")
    parser.add_argument("--tolerance-metres", type=float, default=LabelSettings.tolerance_metres)
    parser.add_argument("--no-candidate-metres", type=float, default=LabelSettings.no_candidate_metres)
    parser.add_argument("--minimum-share", type=float, default=LabelSettings.minimum_share)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.output.exists():
        parser.error(f"output already exists: {args.output}")
    settings = LabelSettings(
        tolerance_metres=args.tolerance_metres,
        no_candidate_metres=args.no_candidate_metres,
        minimum_share=args.minimum_share,
    )
    mappings = args.mappings
    with tempfile.TemporaryDirectory(prefix="post-labels-") as scratch_name:
        scratch = Path(scratch_name)
        labels, report = build_labels(
            evidence_contexts=pl.read_parquet(args.evidence / "contexts.parquet", columns=["context_id"]),
            route_points=pl.read_parquet(args.evidence / "route_points.parquet"),
            context_calls=pl.read_parquet(args.context_calls),
            call_mappings=pl.read_csv(mappings / "source_to_output_calls.csv", infer_schema=False),
            trip_mappings=pl.read_csv(mappings / "source_to_output_trips.csv", infer_schema=False),
            base_stop_times=_read_zip_csv(
                args.base_gtfs, "stop_times.txt", ["trip_id", "stop_sequence", "stop_id"], scratch
            ),
            source_stops=_read_zip_csv(args.source_gtfs, "stops.txt", ["stop_id", "stop_lat", "stop_lon"], scratch),
            source_id=args.source_id,
            settings=settings,
        )
    args.output.mkdir(parents=True)
    labels.write_parquet(args.output / "labels.parquet")
    summary = {"settings": asdict(settings), "report": asdict(report)}
    distances = labels["nearest_distance_metres"].drop_nulls()
    if distances.len() > 0:
        summary["nearest_distance_metres_quantiles"] = {
            str(q): float(distances.quantile(q)) for q in (0.5, 0.9, 0.99)
        }
    (args.output / "label-report.json").write_text(json.dumps(summary, indent=2) + "\n", encoding="utf8")
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
