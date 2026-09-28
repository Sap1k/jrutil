"""Build the per-(context, candidate) feature table for the learned post scorer.

Usage:
    uv run python -m post_scorer.features --evidence EVIDENCE_DIR \
        --exported FEATURE_EXPORT_DIR --osm-candidates post-candidates.geojsonseq \
        [--bus-stations bus-stations.geojsonseq] [--labels LABELS_DIR] --output candidates.parquet

FEATURE_EXPORT_DIR is `jrutil-multitool jdf-export-post-features` output for the
same evidence pack. Candidate identity is the evaluator's consolidated
hypothesis; a candidate is positive when it contains the labelled route point.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import numpy as np
import polars as pl

from post_scorer.labels import (
    NO_CORRECT_CANDIDATE,
    LabelSettings,
    authored_local_ref_labels,
    haversine_metres,
    jdf_stop_number,
)
from post_scorer.osm_facts import StationIndex, candidate_node_tags, observation_tags

# Model inputs, in a fixed order. Source identity (sourceKind, catalogue name,
# source-support weight and network-specific OSM refs) is excluded on purpose.
FEATURE_COLUMNS = [
    # Frozen 2026-09-28 by leave-one-out ablation on PID validation; the 17-feature set beat the
    # full 34 (log-loss 0.699 vs 0.704, same coverage at 0.9 precision). JrUtil's port computes
    # exactly these, in this order.
    "side",
    "alignment",
    "proximity",
    "corridor_distance",
    "routed_fit",
    "routed_excess_metres_capped",
    "excess_minus_context_min",
    "eligible",
    "has_local_ref",
    "is_bay",
    "post_support",
    "post_support_deficit",
    "is_tram",
    "is_trolleybus",
    "anchor_previous_missing",
    "anchor_next_missing",
    "service_edge_count",
]

EXCESS_CAP_METRES = 1000.0
MINIMUM_NUMBERED_BAYS = 3


def _angle_difference(left: pl.Expr, right: pl.Expr) -> pl.Expr:
    difference = (left - right).abs() % 360.0
    return pl.min_horizontal(difference, 360.0 - difference)


def _metric_distance(lat1: pl.Expr, lon1: pl.Expr, lat2: pl.Expr, lon2: pl.Expr) -> pl.Expr:
    dy = (lat1 - lat2) * 110_540.0
    dx = (lon1 - lon2) * 111_320.0 * ((lat1 + lat2) / 2.0).radians().cos()
    return (dx.pow(2) + dy.pow(2)).sqrt()


MAXIMUM_POST_DIAMETER_METRES = 25.0


def _cluster_stop(latitude, longitude, sources, local_refs, maximum_diameter=MAXIMUM_POST_DIAMETER_METRES):
    """Source-constrained complete-linkage clustering of one stop's candidates.

    Every source maps a physical post at most once, so a post may contain each
    source only once: two OSM platforms 4 m apart stay separate posts, while
    four catalogue/OSM points of one post spread over 17 m merge. Merges run
    closest-first, the merged diameter stays within `maximum_diameter`, and
    differing local_ref values never merge. Returns a cluster index per input.
    """
    count = len(latitude)
    lat = np.asarray(latitude, dtype=float)
    lon = np.asarray(longitude, dtype=float)
    y = lat * 110_540.0
    x = lon * 111_320.0 * np.cos(np.radians(lat.mean() if count else 50.0))
    distance = np.hypot(x[:, None] - x[None, :], y[:, None] - y[None, :])
    clusters = [{index} for index in range(count)]
    cluster_sources = [set(sources[index]) for index in range(count)]
    cluster_refs = [{local_refs[index]} - {None} for index in range(count)]
    while True:
        best = None
        for left in range(len(clusters)):
            for right in range(left + 1, len(clusters)):
                if cluster_sources[left] & cluster_sources[right]:
                    continue
                refs = cluster_refs[left] | cluster_refs[right]
                if len(refs) > 1:
                    continue
                diameter = max(distance[i, j] for i in clusters[left] for j in clusters[right])
                if diameter > maximum_diameter:
                    continue
                key = (diameter, min(clusters[left] | clusters[right]))
                if best is None or key < best[0]:
                    best = (key, left, right)
        if best is None:
            break
        _, left, right = best
        clusters[left] |= clusters.pop(right)
        cluster_sources[left] |= cluster_sources.pop(right)
        cluster_refs[left] |= cluster_refs.pop(right)
    assignment = np.zeros(count, dtype=int)
    for index, members in enumerate(clusters):
        for member in members:
            assignment[member] = index
    return assignment


def post_clusters(facts: pl.DataFrame) -> pl.DataFrame:
    """Physical-post id, source support and diameter for every candidate, per stop."""
    rows = []
    for (stop_id,), group in facts.group_by(["stop_id"], maintain_order=True):
        group = group.sort("hypothesis_id")
        sources = [(value or "?").split(";") for value in group["sources"].to_list()]
        assignment = _cluster_stop(group["latitude"].to_list(), group["longitude"].to_list(),
                                   sources, group["local_ref"].to_list())
        for hypothesis_id, cluster, candidate_sources in zip(group["hypothesis_id"].to_list(), assignment, sources):
            rows.append((hypothesis_id, f"{stop_id}:{cluster}", candidate_sources))
    table = pl.DataFrame(rows, schema=["hypothesis_id", "post_id", "candidate_sources"], orient="row")
    support = table.explode("candidate_sources", empty_as_null=True).group_by("post_id").agg(
        pl.col("candidate_sources").n_unique().alias("post_support")
    )
    return table.drop("candidate_sources").join(support, on="post_id")


STOP_LEVEL_MAXIMUM_SHARE = 0.5
STOP_LEVEL_MINIMUM_STOPS = 20


def _source_expression(column: str = "observation_id") -> pl.Expr:
    return (
        pl.when(pl.col(column).str.starts_with("osm:")).then(pl.lit("osm"))
        .when(pl.col(column).str.starts_with("external:"))
        .then(pl.lit("external:") + pl.col(column).str.split(":").list.get(1, null_on_oob=True))
        .otherwise(pl.col(column).fill_null("?").str.split(":").list.first())
    )


def source_granularity(observations: pl.DataFrame) -> pl.DataFrame:
    """Measure whether each source lists posts or only one point per stop.

    At stops where OSM maps two or more posts, a post-level source usually has
    two or more points too; a stop-level source (one point per stop, typically
    between the directions) almost never does. Sources seen at fewer than
    `STOP_LEVEL_MINIMUM_STOPS` such stops are treated as post-level.
    """
    points = (
        observations.select("gtfs_stop_place_id", "route_point_id", _source_expression().alias("source"))
        .group_by("gtfs_stop_place_id", "source").agg(pl.col("route_point_id").n_unique().alias("points"))
    )
    multi_post = points.filter((pl.col("source") == "osm") & (pl.col("points") >= 2)).select("gtfs_stop_place_id")
    return (
        points.join(multi_post, on="gtfs_stop_place_id")
        .group_by("source")
        .agg(pl.len().alias("multi_post_stops"), (pl.col("points") >= 2).mean().alias("multi_point_share"))
        .with_columns(
            ((pl.col("multi_post_stops") >= STOP_LEVEL_MINIMUM_STOPS)
             & (pl.col("multi_point_share") < STOP_LEVEL_MAXIMUM_SHARE)).alias("stop_level")
        )
        .sort("source")
    )


def source_class(observation_id: str) -> str:
    """`osm` or `external:<catalogue>`: each source maps a physical post at most once."""
    if observation_id is None:
        return "?"
    if observation_id.startswith("osm:"):
        return "osm"
    if observation_id.startswith("external:"):
        return "external:" + observation_id.split(":")[1]
    return observation_id.split(":")[0]


def same_post_pairs(candidates: pl.DataFrame, key: str = "context_id") -> pl.DataFrame:
    """Pairs (including each candidate with itself) that belong to the same physical post.

    Uses the table's `post_id` (from `post_clusters`) when present; otherwise
    clusters the candidates of each `key` group directly.
    """
    if "post_id" in candidates.columns:
        posts = candidates.select(key, "hypothesis_id", "post_id")
    else:
        facts = candidates.select(
            pl.col(key).alias("stop_id"), "hypothesis_id", "latitude", "longitude", "local_ref", "sources"
        ).unique(["stop_id", "hypothesis_id"])
        posts = (
            post_clusters(facts).join(facts.select(pl.col("stop_id").alias(key), "hypothesis_id"), on="hypothesis_id")
            .select(key, "hypothesis_id", "post_id")
        )
    return posts.join(posts.rename({"hypothesis_id": "same_post_id"}), on=[key, "post_id"]).select(
        key, "hypothesis_id", "same_post_id"
    )

def mode_compatible_expression(mode_column: str, modes_column: str) -> pl.Expr:
    """True when the known modes are empty or include one the context mode can use (see JrUtil modeCompatible)."""
    mode = pl.col(mode_column)
    modes = pl.col(modes_column).fill_null([])

    def serves(values):
        return modes.list.eval(pl.element().is_in(values)).list.any()

    compatible = (
        pl.when(mode == "A").then(serves(["BUS", "ROAD", "SHARED"]))
        .when(mode == "E").then(serves(["TRAM", "SHARED"]))
        .when(mode == "T").then(serves(["TROLLEYBUS", "BUS", "ROAD", "SHARED"]))
        .otherwise(True)
    )
    return (modes.list.len() == 0) | compatible


def post_modes(hypotheses: pl.DataFrame, route_point_modes: pl.DataFrame, facts: pl.DataFrame) -> pl.DataFrame:
    """Union of explicit modes over every member of each candidate's physical post."""
    members = hypotheses.select(
        "hypothesis_id", pl.col("member_route_point_ids").str.split(";").alias("route_point_id")
    ).explode("route_point_id", empty_as_null=True)
    candidate_modes = (
        members.join(route_point_modes.select("route_point_id", pl.col("explicit_modes").str.split(";").alias("modes")),
                     on="route_point_id", how="left")
        .explode("modes", empty_as_null=True)
        .filter(pl.col("modes").is_not_null() & (pl.col("modes") != ""))
        .select("hypothesis_id", "modes")
    )
    posts = facts.select("hypothesis_id", "post_id")
    per_post = (
        posts.join(candidate_modes, on="hypothesis_id")
        .group_by("post_id").agg(pl.col("modes").unique().sort().alias("post_modes"))
    )
    return posts.join(per_post, on="post_id", how="left").select("hypothesis_id", "post_modes")


MAXIMUM_AREA_DIAMETER_METRES = 40.0


def _cluster_areas(latitude, longitude, classes, maximum_diameter=MAXIMUM_AREA_DIAMETER_METRES):
    """Complete-linkage clustering of post centres; clusters never mix tram and road posts."""
    count = len(latitude)
    lat = np.asarray(latitude, dtype=float)
    y = lat * 110_540.0
    x = np.asarray(longitude, dtype=float) * 111_320.0 * np.cos(np.radians(lat.mean() if count else 50.0))
    distance = np.hypot(x[:, None] - x[None, :], y[:, None] - y[None, :])
    clusters = [{index} for index in range(count)]
    cluster_classes = [set(value) for value in classes]
    while True:
        best = None
        for left in range(len(clusters)):
            for right in range(left + 1, len(clusters)):
                if len(cluster_classes[left] | cluster_classes[right]) > 1:
                    continue
                diameter = max(distance[i, j] for i in clusters[left] for j in clusters[right])
                if diameter <= maximum_diameter and (best is None or diameter < best[0]):
                    best = (diameter, left, right)
        if best is None:
            break
        _, left, right = best
        clusters[left] |= clusters.pop(right)
        cluster_classes[left] |= cluster_classes.pop(right)
    assignment = np.zeros(count, dtype=int)
    for index, members in enumerate(clusters):
        for member in members:
            assignment[member] = index
    return assignment


def post_areas(facts: pl.DataFrame, modes: pl.DataFrame | None) -> pl.DataFrame:
    """Group a stop's physical posts into areas: posts within a 40 m diameter sharing a mode class.

    A bus station's bays form one area and the tram platforms another, even when
    the model cannot tell which bay a line uses. Returns hypothesis_id -> area_id.
    """
    posts = facts.select("hypothesis_id", "post_id", "latitude", "longitude")
    if modes is not None:
        posts = posts.join(modes, on="hypothesis_id", how="left")
    else:
        posts = posts.with_columns(pl.lit(None, dtype=pl.List(pl.Utf8)).alias("post_modes"))
    posts = posts.with_columns(
        pl.when(pl.col("post_modes").fill_null([]).list.len() == 0).then(pl.lit("any"))
        .when(pl.col("post_modes").list.eval(pl.element().is_in(["ROAD", "BUS", "TROLLEYBUS", "SHARED"])).list.any())
        .then(pl.lit("road")).otherwise(pl.lit("tram")).alias("mode_class"),
        pl.col("post_id").str.split(":").list.first().alias("stop_key"),
    )
    centres = posts.group_by("stop_key", "post_id").agg(
        pl.col("latitude").mean(), pl.col("longitude").mean(), pl.col("mode_class").first()
    ).sort("stop_key", "post_id")
    area_of = {}
    for (stop_key,), group in centres.group_by(["stop_key"], maintain_order=True):
        ids = group["post_id"].to_list()
        # Modeless posts join a mode class freely; only tram and road posts never share an area.
        classes = [{c} - {"any"} for c in group["mode_class"].to_list()]
        assignment = _cluster_areas(group["latitude"].to_list(), group["longitude"].to_list(), classes)
        for post_id, area in zip(ids, assignment):
            area_of[post_id] = f"{stop_key}:a{area}"
    return posts.select("hypothesis_id", pl.col("post_id").replace_strict(area_of, default=None).alias("area_id"))


def candidate_facts(
    hypotheses: pl.DataFrame, osm_tags: pl.DataFrame, stations: StationIndex | None,
    stop_level_sources: frozenset[str] = frozenset(),
) -> pl.DataFrame:
    members = hypotheses.select(
        "hypothesis_id",
        pl.col("member_observation_ids").str.split(";").alias("observation_id"),
    ).explode("observation_id", empty_as_null=True)
    tagged = members.join(osm_tags, on="observation_id", how="left").with_columns(
        _source_expression().alias("source")
    )
    # A hypothesis only stop-level sources know about is a stop point, not a post.
    post_hypotheses = (
        tagged.group_by("hypothesis_id")
        .agg((~pl.col("source").is_in(list(stop_level_sources))).any().alias("has_post_source"))
        .filter(pl.col("has_post_source")).select("hypothesis_id")
    )
    hypotheses = hypotheses.join(post_hypotheses, on="hypothesis_id", how="semi")
    tagged = tagged.join(post_hypotheses, on="hypothesis_id", how="semi").filter(
        ~pl.col("source").is_in(list(stop_level_sources))
    )
    tags = tagged.group_by("hypothesis_id").agg(
        pl.col("local_ref").drop_nulls().first().alias("local_ref"),
        pl.col("route_ref").drop_nulls().first().alias("route_ref"),
        (pl.col("public_transport") == "platform").any().alias("is_platform"),
        pl.col("observation_id").n_unique().alias("member_count"),
        pl.col("source").unique().sort().str.join(";").alias("sources"),
    )
    facts = hypotheses.select("hypothesis_id", "stop_id", "latitude", "longitude").join(
        tags, on="hypothesis_id", how="left"
    )
    if stations is not None and facts.height:
        membership = stations.membership(facts["latitude"].to_numpy(), facts["longitude"].to_numpy())
        facts = facts.with_columns(pl.Series("in_bus_station", membership))
    else:
        facts = facts.with_columns(pl.lit(False).alias("in_bus_station"))
    medoids = (
        facts.join(facts.select("stop_id", pl.col("latitude").alias("olat"), pl.col("longitude").alias("olon")), on="stop_id")
        .with_columns(_metric_distance(pl.col("latitude"), pl.col("longitude"), pl.col("olat"), pl.col("olon")).alias("d"))
        .group_by("hypothesis_id", "stop_id", "latitude", "longitude")
        .agg(pl.col("d").sum().alias("total_distance"))
        .sort(["stop_id", "total_distance", "hypothesis_id"])
        .group_by("stop_id", maintain_order=True)
        .first()
        .select("stop_id", pl.col("latitude").alias("medoid_lat"), pl.col("longitude").alias("medoid_lon"))
    )
    numbered = pl.col("local_ref").is_not_null() & pl.col("is_platform")
    # Physical posts and how many independent sources agree on each. A point only
    # one source knows about, next to posts that three or four sources confirm,
    # is usually misplaced or no longer exists.
    posts = post_clusters(facts.select("hypothesis_id", "stop_id", "latitude", "longitude", "local_ref",
                                       pl.col("sources").fill_null("?")))
    facts = facts.join(posts, on="hypothesis_id", how="left").with_columns(
        pl.col("post_support").max().over("stop_id").alias("stop_max_post_support"),
        pl.len().over("post_id").alias("post_size"),
    ).with_columns((pl.col("stop_max_post_support") - pl.col("post_support")).alias("post_support_deficit"))
    return (
        facts.join(medoids, on="stop_id", how="left")
        .with_columns(
            _metric_distance(pl.col("latitude"), pl.col("longitude"), pl.col("medoid_lat"), pl.col("medoid_lon")).alias(
                "distance_to_stop_medoid"
            ),
            pl.col("is_platform").fill_null(False),
            pl.col("member_count").fill_null(1),
        )
        .with_columns(numbered.fill_null(False).cast(pl.Int32).sum().over("stop_id").alias("stop_numbered_platforms"))
        .with_columns(
            # Many terminals (e.g. Litvínov, nádraží) have numbered bays but no
            # amenity=bus_station area, so either signal marks a bay.
            (numbered.fill_null(False) & (pl.col("stop_numbered_platforms") >= MINIMUM_NUMBERED_BAYS)).alias(
                "is_numbered_bay"
            ),
        )
        .with_columns((pl.col("in_bus_station") | pl.col("is_numbered_bay")).alias("point_is_bay"))
        # Bays are physical posts: a catalogue point of an OSM-mapped bay is the bay too.
        .with_columns(pl.col("point_is_bay").any().over("post_id").alias("is_bay"))
        .with_columns(
            pl.col("in_bus_station").cast(pl.Int32).sum().over("stop_id").alias("stop_bus_station_candidates"),
            pl.col("post_id").filter(pl.col("is_bay")).n_unique().over("stop_id").alias("stop_bay_candidates"),
            pl.col("post_id").n_unique().over("stop_id").alias("stop_post_count"),
        )
        .drop("medoid_lat", "medoid_lon")
    )


def build_candidate_table(
    *,
    scores: pl.DataFrame,
    hypotheses: pl.DataFrame,
    assignments: pl.DataFrame,
    contexts: pl.DataFrame,
    variants: pl.DataFrame,
    osm_tags: pl.DataFrame,
    stations: StationIndex | None,
    labels: pl.DataFrame | None,
    positive_tolerance_metres: float = LabelSettings.tolerance_metres,
    stop_level_sources: frozenset[str] = frozenset(),
    route_point_modes: pl.DataFrame | None = None,
) -> pl.DataFrame:
    # A candidate whose known modes exclude the context mode (a tram-only stop for a bus line)
    # is not a choice at all, for the model as for the policy.
    base = scores.filter(
        (pl.col("variant_rank") == 0) & (pl.col("rejection_reason").fill_null("") != "mode-incompatible")
    ).rename({"candidate_id": "hypothesis_id"})
    # Stop-level points (one point per stop from a stop-level source) are not posts either.
    facts = candidate_facts(hypotheses, osm_tags, stations, stop_level_sources).drop("stop_id")
    base = base.join(facts.select("hypothesis_id"), on="hypothesis_id", how="semi")
    modes = post_modes(hypotheses, route_point_modes, facts) if route_point_modes is not None else None
    facts = facts.join(post_areas(facts, modes), on="hypothesis_id", how="left")
    if modes is not None:
        # Modes propagate through the physical post: a catalogue point clustered with a
        # tram-only OSM stop is part of a tram post and is not a bus choice.
        base = (
            base.join(contexts.select("context_id", pl.col("mode").alias("_mode")), on="context_id")
            .join(modes, on="hypothesis_id", how="left")
            .filter(mode_compatible_expression("_mode", "post_modes"))
            .drop("_mode", "post_modes")
        )
    table = base.select(
        "context_id",
        "hypothesis_id",
        "alignment",
        "side",
        "proximity",
        pl.col("routed_fit"),
        pl.col("routed_excess_metres"),
        pl.col("corridor_distance"),
        pl.col("signed_lateral_offset").abs().alias("abs_lateral_offset"),
        _angle_difference(pl.col("corridor_heading"), pl.col("attachment_heading")).alias("heading_difference"),
        pl.col("eligible").cast(pl.Int32),
        pl.col("tied_corridors_agree").cast(pl.Int32),
        "alternative_corridor_count",
        "modality_adjustment",
        "popularity_adjustment",
        pl.col("total").alias("baseline_total"),
        "rejection_reason",
    ).with_columns(
        pl.col("routed_excess_metres").is_null().cast(pl.Int32).alias("excess_missing"),
        pl.col("routed_excess_metres").clip(0.0, EXCESS_CAP_METRES).fill_null(EXCESS_CAP_METRES).alias(
            "routed_excess_metres_capped"
        ),
    )
    table = table.with_columns(
        (
            pl.col("routed_excess_metres_capped") - pl.col("routed_excess_metres_capped").min().over("context_id")
        ).alias("excess_minus_context_min"),
        pl.col("routed_excess_metres_capped").rank("min").over("context_id").cast(pl.Float64).alias("excess_rank"),
        pl.len().over("context_id").cast(pl.Float64).alias("candidate_count"),
    )
    context_facts = contexts.select(
        "context_id",
        jdf_stop_number("gtfs_stop_place_id").alias("stop_id"),
        "line_id",
        "mode",
        "same_stop_block_role",
        pl.col("context_previous_stop_id").is_null().cast(pl.Int32).alias("anchor_previous_missing"),
        pl.col("context_next_stop_id").is_null().cast(pl.Int32).alias("anchor_next_missing"),
        (pl.col("same_stop_block_role") == "through").cast(pl.Int32).alias("role_through"),
        pl.col("context_previous_stop_id").alias("previous_stop"),
        pl.col("context_next_stop_id").alias("next_stop"),
        (pl.col("mode") == "E").cast(pl.Int32).alias("is_tram"),
        (pl.col("mode") == "T").cast(pl.Int32).alias("is_trolleybus"),
    )
    variant_facts = variants.filter(pl.col("variant_rank") == 0).select(
        "context_id",
        pl.col("service_edge_count").cast(pl.Float64),
        pl.col("restricted_access_edge_count").cast(pl.Float64),
        pl.col("access_penalty_metres").clip(0.0, EXCESS_CAP_METRES).fill_null(0.0).alias(
            "access_penalty_metres_capped"
        ),
    )
    table = (
        table.join(context_facts, on="context_id", how="left")
        .join(variant_facts, on="context_id", how="left")
        .join(facts, on="hypothesis_id", how="inner")
        .with_columns(
            pl.col("local_ref").is_not_null().cast(pl.Int32).alias("has_local_ref"),
            pl.col("is_platform").cast(pl.Int32),
            pl.col("in_bus_station").fill_null(False).cast(pl.Int32),
            pl.col("is_bay").fill_null(False).cast(pl.Int32),
            pl.col("stop_bay_candidates").fill_null(0),
        )
    )
    baseline = assignments.select(
        "context_id",
        pl.col("resolution").alias("baseline_resolution"),
        pl.col("selected_hypothesis_id").alias("baseline_hypothesis_id"),
    )
    table = table.join(baseline, on="context_id", how="left").with_columns(
        (pl.col("hypothesis_id") == pl.col("baseline_hypothesis_id")).fill_null(False).alias("baseline_selected")
    )
    if labels is not None:
        answerable = labels.filter(pl.col("label") != NO_CORRECT_CANDIDATE)
        if "source_lat" in labels.columns:
            # The candidate nearest the true post (within the tolerance) and its
            # same-post duplicates are correct; a neighbouring bay is not.
            nearest = (
                table.select("context_id", "hypothesis_id", "latitude", "longitude")
                .join(answerable.select("context_id", "source_lat", "source_lon"), on="context_id")
                .with_columns(
                    haversine_metres(pl.col("latitude"), pl.col("longitude"), pl.col("source_lat"), pl.col("source_lon"))
                    .alias("distance")
                )
                .filter(pl.col("distance") <= positive_tolerance_metres)
                .sort("context_id", "distance", "hypothesis_id")
                .group_by("context_id", maintain_order=True)
                .first()
                .select("context_id", "hypothesis_id")
            )
            positives = (
                nearest.join(same_post_pairs(table), on=["context_id", "hypothesis_id"])
                .select("context_id", pl.col("same_post_id").alias("hypothesis_id"))
                .unique()
                .with_columns(pl.lit(1).alias("y"))
            )
        else:
            members = hypotheses.select(
                "hypothesis_id", pl.col("member_route_point_ids").str.split(";").alias("route_point_id")
            ).explode("route_point_id", empty_as_null=True)
            positives = (
                answerable.select("context_id", pl.col("label").alias("route_point_id"))
                .join(members, on="route_point_id")
                .select("context_id", "hypothesis_id")
                .unique()
                .with_columns(pl.lit(1).alias("y"))
            )
        label_state = labels.select(
            "context_id",
            (pl.col("label") == NO_CORRECT_CANDIDATE).alias("no_correct_candidate"),
            pl.lit(True).alias("labelled"),
        )
        table = (
            table.join(positives, on=["context_id", "hypothesis_id"], how="left")
            .join(label_state, on="context_id", how="left")
            .with_columns(
                pl.col("y").fill_null(0),
                pl.col("labelled").fill_null(False),
                pl.col("no_correct_candidate").fill_null(False),
            )
        )
    for column in FEATURE_COLUMNS:
        table = table.with_columns(pl.col(column).cast(pl.Float64).fill_null(0.0))
    return table.sort("context_id", "hypothesis_id")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--evidence", type=Path, required=True)
    parser.add_argument("--exported", type=Path, required=True)
    parser.add_argument("--osm-candidates", type=Path,
                        help="osmium geojsonseq export; default is the pack's observation raw_tags (as in production)")
    parser.add_argument("--bus-stations", type=Path)
    parser.add_argument("--labels", type=Path, help="labels directory from post_scorer.labels")
    parser.add_argument(
        "--authored-labels", action="store_true", help="label authored JDF post numbers via unique OSM local_ref"
    )
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.output.exists():
        parser.error(f"output already exists: {args.output}")
    if args.labels and args.authored_labels:
        parser.error("--labels and --authored-labels are mutually exclusive")
    hypotheses = pl.read_parquet(args.exported / "hypotheses.parquet")
    contexts = pl.read_parquet(args.evidence / "contexts.parquet")
    osm_tags = (candidate_node_tags(args.osm_candidates) if args.osm_candidates else
                observation_tags(pl.read_parquet(args.evidence / "observations.parquet",
                                                 columns=["observation_id", "raw_tags"])))
    labels = None
    if args.labels:
        labels = pl.read_parquet(args.labels / "labels.parquet")
    elif args.authored_labels:
        labels = authored_local_ref_labels(contexts, hypotheses, osm_tags)
    granularity = source_granularity(
        pl.read_parquet(args.evidence / "observations.parquet",
                        columns=["gtfs_stop_place_id", "route_point_id", "observation_id"])
    )
    stop_level = frozenset(granularity.filter(pl.col("stop_level"))["source"].to_list())
    table = build_candidate_table(
        stop_level_sources=stop_level,
        route_point_modes=pl.read_parquet(args.evidence / "route_points.parquet",
                                          columns=["route_point_id", "explicit_modes"]),
        scores=pl.read_parquet(args.exported / "diagnostic_scores.parquet"),
        hypotheses=hypotheses,
        assignments=pl.read_parquet(args.exported / "assignments.parquet"),
        contexts=contexts,
        variants=pl.read_parquet(args.evidence / "corridor_variants.parquet"),
        osm_tags=osm_tags,
        stations=StationIndex.load(args.bus_stations) if args.bus_stations else None,
        labels=labels,
    )
    table.write_parquet(args.output)
    summary = {
        "stop_level_sources": sorted(stop_level),
        "rows": table.height,
        "contexts": table["context_id"].n_unique(),
        "stops": table["stop_id"].n_unique(),
    }
    if "labelled" in table.columns:
        labelled = table.filter(pl.col("labelled"))
        summary["labelled_contexts"] = labelled["context_id"].n_unique()
        summary["positive_rows"] = int(labelled["y"].sum())
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
