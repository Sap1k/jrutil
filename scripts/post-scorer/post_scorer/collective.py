"""Second-stage features: what related contexts at the same stop believe.

A context alone often cannot tell which of two posts it uses (a junction
where both approaches share a street, a terminus). Related contexts can:

* the reverse movement (arriving from where this one departs to) usually uses
  the other post, so the post it favours is less likely for this context;
* other lines making the same movement usually share a post.

Features are computed per movement key (stop, previous stop, next stop) from
first-stage probabilities, so joins stay small even at large stations. A
missing previous/next stop (terminus) is its own key value, which pairs an
arrival at a terminus with the departure towards the same neighbour.
"""

from __future__ import annotations

import polars as pl

COLLECTIVE_COLUMNS = ["reverse_mass", "has_reverse", "same_movement_mass", "has_same_movement",
                      "same_next_mass", "same_previous_mass"]
_NONE = "∅"


def collective_features(scored: pl.DataFrame) -> pl.DataFrame:
    """Add COLLECTIVE_COLUMNS to a scored candidate table (needs probability and post_id)."""
    movement = [pl.col("previous_stop").fill_null(_NONE).alias("previous_key"),
                pl.col("next_stop").fill_null(_NONE).alias("next_key")]
    post = (
        scored.with_columns(movement)
        .group_by("context_id", "stop_id", "previous_key", "next_key", "post_id")
        .agg(pl.col("probability").sum().alias("mass"))
    )
    contexts = post.select("context_id", "stop_id", "previous_key", "next_key").unique("context_id")
    sizes = contexts.group_by("stop_id", "previous_key", "next_key").agg(pl.len().alias("movement_contexts"))
    totals = post.group_by("stop_id", "previous_key", "next_key", "post_id").agg(pl.col("mass").sum().alias("total"))

    same = (
        post.join(totals, on=["stop_id", "previous_key", "next_key", "post_id"])
        .join(sizes, on=["stop_id", "previous_key", "next_key"])
        .select(
            "context_id", "post_id",
            pl.when(pl.col("movement_contexts") > 1)
            .then((pl.col("total") - pl.col("mass")) / (pl.col("movement_contexts") - 1))
            .otherwise(0.0).alias("same_movement_mass"),
            (pl.col("movement_contexts") > 1).cast(pl.Float64).alias("has_same_movement"),
        )
    )
    reverse_totals = totals.join(sizes, on=["stop_id", "previous_key", "next_key"]).select(
        "stop_id",
        pl.col("next_key").alias("previous_key"),
        pl.col("previous_key").alias("next_key"),
        "post_id",
        (pl.col("total") / pl.col("movement_contexts")).alias("reverse_mass"),
    )
    reverse_present = sizes.select(
        "stop_id", pl.col("next_key").alias("previous_key"), pl.col("previous_key").alias("next_key"),
        pl.lit(1.0).alias("has_reverse"),
    )
    reverse = (
        post.select("context_id", "stop_id", "previous_key", "next_key", "post_id")
        .join(reverse_totals, on=["stop_id", "previous_key", "next_key", "post_id"], how="left")
        .join(reverse_present, on=["stop_id", "previous_key", "next_key"], how="left")
        # A movement whose reverse is itself (both neighbours missing) has no reverse.
        .with_columns(
            pl.when(pl.col("previous_key") == pl.col("next_key")).then(None).otherwise(pl.col("has_reverse"))
            .alias("has_reverse")
        )
        .with_columns(
            pl.when(pl.col("has_reverse").is_null()).then(0.0).otherwise(pl.col("reverse_mass").fill_null(0.0))
            .alias("reverse_mass"),
            pl.col("has_reverse").fill_null(0.0),
        )
        .select("context_id", "post_id", "reverse_mass", "has_reverse")
    )
    # Contexts leaving towards the same neighbour (or arriving from the same one) usually
    # stand at the same post; this is what a terminus departure has to go on.
    def shared(key: str, alias: str) -> pl.DataFrame:
        known = post.filter(pl.col(key) != _NONE)
        counts = known.select("context_id", "stop_id", key).unique("context_id").group_by("stop_id", key).agg(
            pl.len().alias("n"))
        sums = known.group_by("stop_id", key, "post_id").agg(pl.col("mass").sum().alias("sum"))
        return (known.join(sums, on=["stop_id", key, "post_id"]).join(counts, on=["stop_id", key])
                .select("context_id", "post_id",
                        pl.when(pl.col("n") > 1).then((pl.col("sum") - pl.col("mass")) / (pl.col("n") - 1))
                        .otherwise(0.0).alias(alias)))

    return (
        scored.drop([c for c in COLLECTIVE_COLUMNS if c in scored.columns])
        .join(same, on=["context_id", "post_id"], how="left")
        .join(shared("next_key", "same_next_mass"), on=["context_id", "post_id"], how="left")
        .join(shared("previous_key", "same_previous_mass"), on=["context_id", "post_id"], how="left")
        .join(reverse, on=["context_id", "post_id"], how="left")
        .with_columns([pl.col(c).fill_null(0.0) for c in COLLECTIVE_COLUMNS])
    )
