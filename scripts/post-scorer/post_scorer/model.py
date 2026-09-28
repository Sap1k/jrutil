"""Conditional-logit post scorer with an explicit "no correct candidate" option.

Usage:
    uv run python -m post_scorer.model train --candidates TRAIN.parquet --output MODEL_DIR
    uv run python -m post_scorer.model evaluate --candidates EVAL.parquet --model MODEL_DIR/model.json

Each context is a choice among its candidates plus a null alternative whose
utility is a learned constant. Softmax probabilities therefore express both
"which post" and "none of these"; a candidate is published when its
probability reaches the calibrated threshold. Training/validation split is by
stop (deterministic hash), so contexts of one terminal never straddle it.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import polars as pl
from scipy.optimize import minimize

from post_scorer.collective import COLLECTIVE_COLUMNS, collective_features
from post_scorer.features import FEATURE_COLUMNS, same_post_pairs

MODEL_FORMAT = "post-scorer-conditional-logit-v1"


@dataclass
class ContextBatch:
    """Candidates sorted by context; `starts` delimits each context's rows."""

    context_ids: np.ndarray
    features: np.ndarray
    y: np.ndarray
    starts: np.ndarray
    null_target: np.ndarray
    context_weight: np.ndarray | None = None

    @classmethod
    def from_table(cls, table: pl.DataFrame, columns: list[str]) -> ContextBatch:
        table = table.sort("context_id", "hypothesis_id")
        context_ids = table["context_id"].to_numpy()
        starts = np.flatnonzero(np.r_[True, context_ids[1:] != context_ids[:-1]])
        y = table["y"].to_numpy().astype(float) if "y" in table.columns else np.zeros(table.height)
        positives = np.add.reduceat(y, starts) if len(starts) else np.zeros(0)
        return cls(
            context_ids=context_ids[starts],
            features=table.select(columns).to_numpy().astype(float),
            y=y,
            starts=starts,
            null_target=(positives == 0).astype(float),
            context_weight=(table["weight"].to_numpy().astype(float)[starts]
                            if "weight" in table.columns and len(starts) else None),
        )


def stable_fold(stop_id: int, folds: int = 5) -> int:
    return int(hashlib.sha256(str(stop_id).encode()).hexdigest()[:8], 16) % folds


def _segment_softmax(utilities: np.ndarray, null_utility: float, starts: np.ndarray):
    """Per-context softmax over candidates plus the null option."""
    maxima = np.maximum(np.maximum.reduceat(utilities, starts), null_utility)
    counts = np.diff(np.r_[starts, len(utilities)])
    shifted = np.exp(utilities - np.repeat(maxima, counts))
    null_exp = np.exp(null_utility - maxima)
    denominators = np.add.reduceat(shifted, starts) + null_exp
    return shifted / np.repeat(denominators, counts), null_exp / denominators


def fit(batch: ContextBatch, l2: float = 1.0) -> tuple[np.ndarray, float]:
    columns = batch.features.shape[1]

    def objective(parameters: np.ndarray):
        weights, null_utility = parameters[:columns], parameters[columns]
        probabilities, null_probability = _segment_softmax(batch.features @ weights, null_utility, batch.starts)
        # A context may have several correct candidates (one physical post
        # mapped more than once); the target is the total mass on them.
        chosen = np.add.reduceat(batch.y * probabilities, batch.starts) + batch.null_target * null_probability
        chosen = np.maximum(chosen, 1e-300)
        # Traffic weights scale each context's term (and its gradient); unweighted = 1.
        context_weight = batch.context_weight if batch.context_weight is not None else np.ones(len(chosen))
        loss = -(context_weight * np.log(chosen)).sum() + 0.5 * l2 * weights @ weights
        # d(-log sum_pos p)/dw = sum_j p_j x_j - sum_pos (p_k / sum_pos p) x_k
        counts = np.diff(np.r_[batch.starts, len(batch.y)])
        row_weight = np.repeat(context_weight, counts)
        responsibility = batch.y * probabilities / np.repeat(chosen, counts)
        expected = ((row_weight * probabilities)[:, None] * batch.features).sum(axis=0)
        observed = ((row_weight * responsibility)[:, None] * batch.features).sum(axis=0)
        gradient = expected - observed + l2 * weights
        # The null option's own gradient: sum w p_null - sum over null targets of w.
        null_gradient = (context_weight * null_probability).sum() - (context_weight * batch.null_target).sum()
        return loss, np.r_[gradient, null_gradient]

    result = minimize(objective, np.zeros(columns + 1), jac=True, method="L-BFGS-B")
    if not result.success:
        raise RuntimeError(f"Optimisation failed: {result.message}")
    return result.x[:columns], float(result.x[columns])


@dataclass
class Model:
    columns: list[str]
    mean: np.ndarray
    scale: np.ndarray
    weights: np.ndarray
    null_utility: float
    threshold: float = 0.5

    def probabilities(self, table: pl.DataFrame) -> pl.DataFrame:
        batch = ContextBatch.from_table(table, self.columns)
        standardized = (batch.features - self.mean) / self.scale
        probabilities, null_probability = _segment_softmax(standardized @ self.weights, self.null_utility, batch.starts)
        counts = np.diff(np.r_[batch.starts, len(batch.y)])
        return table.sort("context_id", "hypothesis_id").with_columns(
            pl.Series("probability", probabilities),
            pl.Series("null_probability", np.repeat(null_probability, counts)),
        )

    def to_json(self) -> dict:
        return {
            "format": MODEL_FORMAT,
            "features": self.columns,
            "mean": self.mean.tolist(),
            "scale": self.scale.tolist(),
            "weights": self.weights.tolist(),
            "null_utility": self.null_utility,
            "threshold": self.threshold,
        }

    @classmethod
    def from_json(cls, data: dict) -> Model:
        if data.get("format") != MODEL_FORMAT:
            raise ValueError(f"Unsupported model format: {data.get('format')}")
        return cls(
            columns=list(data["features"]),
            mean=np.asarray(data["mean"]),
            scale=np.asarray(data["scale"]),
            weights=np.asarray(data["weights"]),
            null_utility=float(data["null_utility"]),
            threshold=float(data["threshold"]),
        )


def train(table: pl.DataFrame, columns: list[str] = FEATURE_COLUMNS, l2: float = 1.0) -> Model:
    table = table.filter(pl.col("labelled"))
    features = table.select(columns).to_numpy().astype(float)
    mean = features.mean(axis=0)
    scale = features.std(axis=0)
    scale[scale < 1e-9] = 1.0
    standardized = table.with_columns(
        [((pl.col(column) - mean[index]) / scale[index]).alias(column) for index, column in enumerate(columns)]
    )
    weights, null_utility = fit(ContextBatch.from_table(standardized, columns), l2)
    return Model(columns, mean, scale, weights, null_utility)


def post_mass(scored: pl.DataFrame) -> pl.DataFrame:
    """Per candidate: probability summed over its same-post duplicates.

    One physical post is often mapped by several sources a few metres apart and
    the softmax splits its probability across those points, so the decision
    uses the post's mass. Duplicates follow `features.same_post_pairs`, which
    keeps distinct bays apart even when they are close together.
    """
    if "post_id" not in scored.columns and not {"latitude", "longitude", "sources", "local_ref"} <= set(scored.columns):
        return scored.with_columns(pl.col("probability").alias("post_probability"))
    probabilities = scored.select(
        "context_id", pl.col("hypothesis_id").alias("same_post_id"), pl.col("probability").alias("n_probability")
    )
    mass = (
        same_post_pairs(scored)
        .join(probabilities, on=["context_id", "same_post_id"])
        .group_by("context_id", "hypothesis_id")
        .agg(pl.col("n_probability").sum().clip(upper_bound=1.0).alias("post_probability"))
    )
    return scored.join(mass, on=["context_id", "hypothesis_id"], how="left")


COMPLEX_STOP_POSTS = 3


def threshold_expression(threshold, columns) -> pl.Expr:
    """A float, or {"simple": x, "complex": y} split by the stop's number of physical posts."""
    if isinstance(threshold, dict):
        if "stop_post_count" not in columns:
            return pl.lit(threshold["complex"])
        return (pl.when(pl.col("stop_post_count") >= COMPLEX_STOP_POSTS)
                .then(pl.lit(threshold["complex"])).otherwise(pl.lit(threshold["simple"])))
    return pl.lit(float(threshold))


def decisions(scored: pl.DataFrame, threshold) -> pl.DataFrame:
    """One row per context: the model's post and publication, baseline choice, truth.

    1. Physical: the heaviest physical post reaches the stratum threshold.
    2. Area: otherwise, when `threshold["area"]` is set and the heaviest area
       (posts within 40 m sharing a mode class, e.g. a bus station's bays)
       reaches it, the area's most probable post is published.
    3. Otherwise abstain. Within the chosen post, its OSM platform point is
       published when there is one (best-mapped geometry).
    """
    scored = scored if "post_probability" in scored.columns else post_mass(scored)
    platform = pl.col("is_platform").fill_null(0) if "is_platform" in scored.columns else pl.lit(0)
    has_area = "area_id" in scored.columns
    scored = scored.with_columns(
        platform.alias("_platform"),
        (pl.col("probability").sum().over("context_id", "area_id") if has_area
         else pl.col("post_probability")).alias("area_probability"),
    )
    top = (
        scored.sort(["context_id", "post_probability", "_platform", "probability", "hypothesis_id"],
                    descending=[False, True, True, True, False])
        .group_by("context_id", maintain_order=True).first()
    )
    area_top = (
        scored.sort(["context_id", "area_probability", "post_probability", "_platform", "probability", "hypothesis_id"],
                    descending=[False, True, True, True, True, False])
        .group_by("context_id", maintain_order=True).first()
    )
    second = (
        scored.sort(["context_id", "probability"], descending=[False, True])
        .group_by("context_id", maintain_order=True)
        .agg(pl.col("probability").slice(1, 1).first().fill_null(0.0).alias("second_probability"))
    )
    columns = ["context_id", "stop_id", "hypothesis_id", "probability", "post_probability", "area_probability",
               "null_probability", "is_bay", "stop_bay_candidates", "stop_post_count", "baseline_resolution", "y",
               "area_id", "weight"]
    physical = top.select([c for c in columns if c in top.columns]).with_columns(
        (pl.col("post_probability") >= threshold_expression(threshold, top.columns)).alias("_physical")
    )
    area_threshold = threshold.get("area") if isinstance(threshold, dict) else None
    area = area_top.select([c for c in columns if c in area_top.columns]).with_columns(
        (pl.col("area_probability") >= (area_threshold if area_threshold is not None else 9.0)).alias("_area")
    )
    result = (
        physical.join(area.select("context_id", "_area"), on="context_id")
        .with_columns(pl.when(pl.col("_physical")).then(pl.lit("Physical"))
                      .when(pl.col("_area")).then(pl.lit("Area")).otherwise(pl.lit("Abstain")).alias("model_resolution"))
    )
    # Area decisions publish the area's best post instead of the overall heaviest post.
    area_choice = area.select(
        "context_id", *[pl.col(c).alias(f"area_{c}") for c in ("hypothesis_id", "y", "is_bay", "stop_post_count",
                                                                "post_probability", "area_probability")
                        if c in area.columns]
    )
    is_area = pl.col("model_resolution") == "Area"
    result = result.join(area_choice, on="context_id").with_columns(
        pl.when(is_area).then(pl.col("area_hypothesis_id")).otherwise(pl.col("hypothesis_id")).alias("hypothesis_id"),
        # Probabilities describe the published post (as JrUtil reports them).
        pl.when(is_area).then(pl.col("area_post_probability")).otherwise(pl.col("post_probability"))
        .alias("post_probability"),
        pl.when(is_area).then(pl.col("area_area_probability")).otherwise(pl.col("area_probability"))
        .alias("area_probability"),
        *([pl.when(pl.col("model_resolution") == "Area").then(pl.col("area_is_bay")).otherwise(pl.col("is_bay"))
           .alias("is_bay")] if "is_bay" in result.columns else []),
        (pl.col("model_resolution") != "Abstain").alias("model_published"),
    ).join(second, on="context_id")
    baseline_row = scored.filter(pl.col("baseline_selected")).select(
        "context_id", pl.col("is_bay").alias("baseline_is_bay"),
        *([pl.col("y").alias("baseline_y")] if "y" in scored.columns else []),
    )
    result = result.join(baseline_row, on="context_id", how="left").with_columns(
        (pl.col("baseline_resolution") == "Physical").fill_null(False).alias("baseline_published")
    )
    if "y" in scored.columns:
        truth = scored.group_by("context_id").agg(
            pl.col("labelled").first(), pl.col("no_correct_candidate").first(), (pl.col("y").max() > 0).alias("has_positive")
        )
        area_truth = (scored.group_by("context_id", "area_id").agg((pl.col("y").max() > 0).alias("area_has_truth"))
                      if has_area else None)
        result = result.join(truth, on="context_id").with_columns(
            pl.when(pl.col("model_resolution") == "Area").then(pl.col("area_y")).otherwise(pl.col("y")).alias("model_y")
        )
        if area_truth is not None:
            chosen_area = scored.select("context_id", "hypothesis_id", "area_id").unique(["context_id", "hypothesis_id"])
            result = (result.drop("area_id", strict=False)
                      .join(chosen_area, on=["context_id", "hypothesis_id"], how="left")
                      .join(area_truth, on=["context_id", "area_id"], how="left")
                      .with_columns(pl.col("area_has_truth").fill_null(False)))
    return result.drop([c for c in ("_physical", "_area", "y", "area_y", "area_post_probability",
                                    "area_area_probability") if c in result.columns])

def metrics(result: pl.DataFrame) -> dict:
    """Label-based metrics (when labels exist) plus the label-free street-over-bay count."""
    report: dict = {"contexts": result.height}
    for system, published, in_station in (
        ("model", "model_published", "is_bay"),
        ("baseline", "baseline_published", "baseline_is_bay"),
    ):
        stats: dict = {"published": int(result[published].sum())}
        # Street-over-bay: publishing a non-station post where station bays exist.
        stats["street_over_bay"] = int(
            result.filter(
                pl.col(published) & (pl.col("stop_bay_candidates") > 0) & (pl.col(in_station).fill_null(0) == 0)
            ).height
        )
        if "labelled" in result.columns:
            labelled = result.filter(pl.col("labelled"))
            y_column = "model_y" if system == "model" else "baseline_y"
            chosen = labelled.filter(pl.col(published))
            correct = int(chosen.filter(pl.col(y_column).fill_null(0) == 1).height)
            answerable = labelled.filter(pl.col("has_positive"))
            unanswerable = labelled.filter(~pl.col("has_positive"))
            if "weight" in labelled.columns:
                chosen_weight = chosen["weight"].sum()
                correct_weight = chosen.filter(pl.col(y_column).fill_null(0) == 1)["weight"].sum()
                answerable_weight = labelled.filter(pl.col("has_positive"))["weight"].sum()
                stats["weighted_precision"] = correct_weight / chosen_weight if chosen_weight else None
                stats["weighted_coverage"] = correct_weight / answerable_weight if answerable_weight else None
            stats.update(
                labelled=labelled.height,
                correct=correct,
                wrong=chosen.height - correct,
                precision=correct / chosen.height if chosen.height else None,
                coverage=correct / answerable.height if answerable.height else None,
                abstain_on_no_candidate=(
                    1.0 - unanswerable.filter(pl.col(published)).height / unanswerable.height
                    if unanswerable.height
                    else None
                ),
            )
            if system == "model" and "model_resolution" in result.columns:
                area = labelled.filter(pl.col("model_resolution") == "Area")
                stats["area_decisions"] = area.height
                stats["area_post_correct"] = int(area.filter(pl.col("model_y").fill_null(0) == 1).height)
                if "area_has_truth" in area.columns:
                    stats["area_contains_truth"] = int(area.filter(pl.col("area_has_truth")).height)
            if system == "model":
                stats["top1_accuracy"] = (
                    answerable.filter(pl.col("model_y") == 1).height / answerable.height if answerable.height else None
                )
        report[system] = stats
    return report


def calibrate_threshold(result_for: callable, target_precision: float | None) -> float:
    """Lowest threshold whose precision reaches the target; never publish when none does."""
    for threshold in np.round(np.arange(0.30, 0.995, 0.01), 2):
        stats = metrics(result_for(float(threshold)))["model"]
        # Calibrate on traffic-weighted precision when weights are present.
        precision = stats.get("weighted_precision", stats.get("precision"))
        if precision is not None and (target_precision is None or precision >= target_precision):
            return float(threshold)
    return 1.01


def calibrate_stratified(scored: pl.DataFrame, target_precision: float) -> dict:
    """Post thresholds per stratum, then an area threshold for the contexts they leave unpublished.

    Area decisions count as correct when the true post lies in the chosen area.
    """
    complex_stop = pl.col("stop_post_count") >= COMPLEX_STOP_POSTS
    thresholds = {
        "simple": calibrate_threshold(lambda t: decisions(scored.filter(~complex_stop), t), target_precision),
        "complex": calibrate_threshold(lambda t: decisions(scored.filter(complex_stop), t), target_precision),
    }
    if "area_id" not in scored.columns:
        return thresholds
    for area_threshold in np.round(np.arange(0.50, 0.995, 0.01), 2):
        result = decisions(scored, {**thresholds, "area": float(area_threshold)})
        area = result.filter(pl.col("labelled") & (pl.col("model_resolution") == "Area"))
        if "weight" in area.columns:
            share = (area.filter(pl.col("area_has_truth"))["weight"].sum() / area["weight"].sum()) if area.height else 0.0
        else:
            share = area.filter(pl.col("area_has_truth")).height / area.height if area.height else 0.0
        if area.height and share >= target_precision:
            thresholds["area"] = float(area_threshold)
            return thresholds
    thresholds["area"] = 1.01
    return thresholds


TWO_STAGE_FORMAT = "post-scorer-two-stage-v1"


@dataclass
class TwoStageModel:
    """First stage scores each context alone; the second adds related contexts' beliefs."""

    stage1: Model
    stage2: Model
    thresholds: dict

    def probabilities(self, table: pl.DataFrame) -> pl.DataFrame:
        first = self.stage1.probabilities(table).drop("null_probability")
        return self.stage2.probabilities(collective_features(first).drop("probability"))

    def to_json(self) -> dict:
        return {"format": TWO_STAGE_FORMAT, "stage1": self.stage1.to_json(), "stage2": self.stage2.to_json(),
                "thresholds": self.thresholds}

    @classmethod
    def from_json(cls, data: dict) -> TwoStageModel:
        if data.get("format") != TWO_STAGE_FORMAT:
            raise ValueError(f"Unsupported model format: {data.get('format')}")
        return cls(Model.from_json(data["stage1"]), Model.from_json(data["stage2"]), dict(data["thresholds"]))


def _with_folds(table: pl.DataFrame, folds: int = 5) -> pl.DataFrame:
    return table.join(
        table.select(pl.col("stop_id").unique()).with_columns(
            pl.col("stop_id").map_elements(lambda stop: stable_fold(stop, folds), return_dtype=pl.Int64).alias("fold")
        ),
        on="stop_id",
    )


def train_two_stage(table: pl.DataFrame, l2: float = 1.0) -> TwoStageModel:
    """Stage 2 is fitted on out-of-fold stage-1 scores (folds by stop), never on in-sample ones."""
    table = _with_folds(table.filter(pl.col("labelled")), folds=5)
    out_of_fold = []
    for fold in sorted(table["fold"].unique().to_list()):
        held_out = table.filter(pl.col("fold") == fold).drop("fold")
        out_of_fold.append(train(table.filter(pl.col("fold") != fold).drop("fold"), l2=l2)
                           .probabilities(held_out).drop("null_probability"))
    second_stage = collective_features(pl.concat(out_of_fold)).drop("probability")
    stage2 = train(second_stage, columns=FEATURE_COLUMNS + COLLECTIVE_COLUMNS, l2=l2)
    stage1 = train(table.drop("fold"), l2=l2)
    return TwoStageModel(stage1, stage2, {"simple": 0.5, "complex": 0.5})

def attach_weights(table: pl.DataFrame, weights_path: Path | None) -> pl.DataFrame:
    """Join traffic weights: departures clipped to [1, p99] and normalised to mean 1 over labelled contexts."""
    if weights_path is None:
        return table
    departures = pl.read_parquet(weights_path).select("context_id", pl.col("departures").cast(pl.Float64))
    table = table.drop("weight", strict=False).join(departures, on="context_id", how="left").with_columns(
        pl.col("departures").fill_null(1.0))
    keep = ["context_id", "departures"] + (["labelled"] if "labelled" in table.columns else [])
    per_context = table.select(keep).unique("context_id")
    cap = float(per_context["departures"].quantile(0.99) or 1.0)
    reference = per_context.filter(pl.col("labelled")) if "labelled" in per_context.columns else per_context
    mean = float(reference["departures"].clip(1.0, cap).mean() or 1.0)
    return table.with_columns((pl.col("departures").clip(1.0, cap) / mean).alias("weight")).drop("departures")


def strata(scored: pl.DataFrame):
    complex_stop = pl.col("stop_post_count") >= COMPLEX_STOP_POSTS
    return (("all", scored), ("simple_stops", scored.filter(~complex_stop)),
            ("complex_stops", scored.filter(complex_stop)),
            ("stops_with_bays", scored.filter(pl.col("stop_bay_candidates") > 0)),
            ("trams", scored.filter(pl.col("mode") == "E")))


def evaluate_strata(model, thresholds, table: pl.DataFrame) -> dict:
    scored = post_mass(model.probabilities(table.filter(pl.col("labelled"))))
    return {population: metrics(decisions(part, thresholds)) for population, part in strata(scored)}


def fit_region(table: pl.DataFrame, target_precision: float, l2: float = 1.0):
    """Train on a region's training stops and calibrate thresholds on its held-out stops."""
    training, validation = _split(table)
    model = train_two_stage(training, l2=l2)
    thresholds = calibrate_stratified(post_mass(model.probabilities(validation.filter(pl.col("labelled")))),
                                      target_precision)
    model.thresholds = thresholds
    return model, validation


def cross_validation(regions: dict[str, pl.DataFrame], target_precision: float, l2: float = 1.0) -> list[dict]:
    """Transfer between regions, plus a pooled model tested on each region's held-out stops."""
    rows = []

    def record(trained_on, tested_on, model, table):
        for population, report in evaluate_strata(model, model.thresholds, table).items():
            for system in ("model", "baseline"):
                stats = report[system]
                rows.append({"trained_on": trained_on, "tested_on": tested_on, "population": population,
                             "system": system, **{key: stats.get(key) for key in (
                                 "correct", "wrong", "precision", "coverage", "weighted_precision",
                                 "weighted_coverage", "area_decisions", "area_contains_truth")}})

    for name, table in regions.items():
        model, validation = fit_region(table, target_precision, l2)
        record(name, f"{name} (held-out stops)", model, validation)
        for other, other_table in regions.items():
            if other != name:
                record(name, other, model, other_table)
    if len(regions) > 1:
        pooled = pool(regions)
        model, validation = fit_region(pooled, target_precision, l2)
        for name in regions:
            record("+".join(regions), f"{name} (held-out stops)", model, validation.filter(pl.col("region") == name))
    return rows


def stage1_quality(model: Model, validation: pl.DataFrame) -> dict:
    """Validation log-loss of the true choice, top-1 accuracy and coverage at 0.9 precision."""
    scored = post_mass(model.probabilities(validation))
    chosen = scored.group_by("context_id").agg(
        (pl.col("probability") * pl.col("y")).sum().alias("p_true"),
        pl.col("null_probability").first(), (pl.col("y").max() > 0).alias("answerable"),
    ).with_columns(
        pl.when(pl.col("answerable")).then(pl.col("p_true")).otherwise(pl.col("null_probability"))
        .clip(lower_bound=1e-12).log().neg().alias("loss")
    )
    threshold = calibrate_threshold(lambda t: decisions(scored, t), 0.9)
    report = metrics(decisions(scored, threshold))["model"]
    return {"log_loss": float(chosen["loss"].mean()), "top1": report["top1_accuracy"],
            "threshold_0_9": threshold, "coverage_at_0_9": report["coverage"] or 0.0}


def read_regions(values: list[str]) -> dict[str, pl.DataFrame]:
    """NAME=CANDIDATES.parquet[,WEIGHTS.parquet] -> weighted candidate table per region."""
    regions = {}
    for value in values:
        name, paths = value.split("=", 1)
        candidates, _, weights = paths.partition(",")
        regions[name] = attach_weights(pl.read_parquet(candidates), Path(weights) if weights else None)
    return regions


def pool(regions: dict[str, pl.DataFrame]) -> pl.DataFrame:
    """Regions stacked on their shared columns; weights stay normalised per region.

    A stop captured by two regions (a box border) is kept from the first one only, so
    no context is counted twice and the stop split stays disjoint.
    """
    shared = sorted(set.intersection(*(set(t.columns) for t in regions.values())))
    parts, seen = [], pl.DataFrame({"stop_id": []}, schema={"stop_id": pl.Int64})
    for name, table in regions.items():
        part = table.join(seen, on="stop_id", how="anti")
        parts.append(part.select(shared).with_columns(pl.lit(name).alias("region")))
        seen = pl.concat([seen, part.select(pl.col("stop_id").cast(pl.Int64)).unique()])
    return pl.concat(parts)


def pooled_regions(values: list[str]) -> pl.DataFrame:
    return pool(read_regions(values))


def _split(table: pl.DataFrame) -> tuple[pl.DataFrame, pl.DataFrame]:
    folds = table.select(pl.col("stop_id").unique()).with_columns(
        pl.col("stop_id").map_elements(stable_fold, return_dtype=pl.Int64).alias("fold")
    )
    table = table.join(folds, on="stop_id")
    return table.filter(pl.col("fold") != 0).drop("fold"), table.filter(pl.col("fold") == 0).drop("fold")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    commands = parser.add_subparsers(dest="command", required=True)
    train_parser = commands.add_parser("train")
    train_parser.add_argument("--candidates", type=Path)
    train_parser.add_argument("--weights", type=Path, help="context_weights.parquet from post_scorer.weights")
    train_parser.add_argument("--region", action="append",
                              help="pooled training: NAME=CANDIDATES.parquet[,WEIGHTS.parquet] (repeatable)")
    train_parser.add_argument("--l2", type=float, default=1.0)
    train_parser.add_argument("--target-precision", type=float, default=0.9)
    train_parser.add_argument("--output", type=Path, required=True)
    evaluate_parser = commands.add_parser("evaluate")
    evaluate_parser.add_argument("--candidates", type=Path, required=True)
    evaluate_parser.add_argument("--weights", type=Path, help="context_weights.parquet from post_scorer.weights")
    evaluate_parser.add_argument("--model", type=Path, required=True)
    evaluate_parser.add_argument("--decisions-out", type=Path)
    cross_parser = commands.add_parser("crossval", help="transfer between labelled regions")
    cross_parser.add_argument("--region", action="append", required=True,
                              help="NAME=CANDIDATES.parquet[,WEIGHTS.parquet]")
    cross_parser.add_argument("--target-precision", type=float, default=0.9)
    cross_parser.add_argument("--output", type=Path)
    review_parser = commands.add_parser("review-list", help="stops ranked by traffic-weighted uncertainty")
    review_parser.add_argument("--candidates", type=Path, required=True)
    review_parser.add_argument("--weights", type=Path, help="context_weights.parquet from post_scorer.weights")
    review_parser.add_argument("--model", type=Path, required=True)
    review_parser.add_argument("--merged-jdf", type=Path, help="for stop names")
    review_parser.add_argument("--top", type=int, default=100)
    review_parser.add_argument("--output", type=Path)
    ablate_parser = commands.add_parser("ablate", help="leave-one-feature-out on stage 1 (validation split)")
    ablate_parser.add_argument("--candidates", type=Path, required=True)
    ablate_parser.add_argument("--weights", type=Path, help="context_weights.parquet from post_scorer.weights")
    ablate_parser.add_argument("--l2", type=float, default=1.0)
    ablate_parser.add_argument("--output", type=Path)
    curve_parser = commands.add_parser("curve", help="precision/coverage by threshold on the stop-held-out validation split")
    curve_parser.add_argument("--candidates", type=Path, required=True)
    curve_parser.add_argument("--weights", type=Path, help="context_weights.parquet from post_scorer.weights")
    curve_parser.add_argument("--l2", type=float, default=1.0)
    args = parser.parse_args(argv)


    if args.command == "review-list":
        model = TwoStageModel.from_json(json.loads(args.model.read_text(encoding="utf8")))
        table = attach_weights(pl.read_parquet(args.candidates), args.weights)
        result = decisions(post_mass(model.probabilities(table)), model.thresholds)
        weight = pl.col("weight") if "weight" in result.columns else pl.lit(1.0)
        best = pl.max_horizontal("post_probability", "area_probability")
        ranked = (result.group_by("stop_id").agg(
            (weight * (1 - best)).sum().alias("uncertain_traffic"), pl.len().alias("contexts"),
            (pl.col("model_resolution") == "Abstain").sum().alias("abstained"),
            (pl.col("model_resolution") == "Area").sum().alias("area"))
            .sort("uncertain_traffic", descending=True).head(args.top))
        if args.merged_jdf:
            from post_scorer.review_map import stop_names
            names = stop_names(args.merged_jdf)
            ranked = ranked.with_columns(pl.col("stop_id").map_elements(lambda s: names.get(s, ""), return_dtype=pl.Utf8).alias("name"))
        if args.output:
            ranked.write_csv(args.output)
        with pl.Config(tbl_rows=args.top, tbl_width_chars=160, fmt_str_lengths=40):
            print(ranked)
        return 0

    if args.command == "crossval":
        regions = read_regions(args.region)
        table = pl.DataFrame(cross_validation(regions, args.target_precision))
        if args.output:
            table.write_csv(args.output)
        with pl.Config(tbl_rows=200, tbl_width_chars=220):
            print(table.filter(pl.col("system") == "model").drop("system"))
            print(table.filter((pl.col("system") == "baseline") & (pl.col("population") == "all")).drop("system"))
        return 0

    if args.command == "ablate":
        training, validation = _split(attach_weights(pl.read_parquet(args.candidates), args.weights))
        validation = validation.filter(pl.col("labelled"))
        rows = []
        for dropped in [None, *FEATURE_COLUMNS]:
            columns = [column for column in FEATURE_COLUMNS if column != dropped]
            rows.append({"dropped": dropped or "(none)", **stage1_quality(train(training, columns, args.l2), validation)})
            print(rows[-1], flush=True)
        table = pl.DataFrame(rows)
        base = table.row(0, named=True)
        table = table.with_columns(
            (pl.col("log_loss") - base["log_loss"]).alias("d_log_loss"),
            (pl.col("coverage_at_0_9") - base["coverage_at_0_9"]).alias("d_coverage"),
        ).sort("d_log_loss")
        if args.output:
            table.write_csv(args.output)
        with pl.Config(tbl_rows=60, tbl_width_chars=160):
            print(table)
        return 0

    if args.command == "curve":
        training, validation = _split(attach_weights(pl.read_parquet(args.candidates), args.weights))
        scored = post_mass(train_two_stage(training, l2=args.l2).probabilities(validation.filter(pl.col("labelled"))))
        rows = []
        for population, table in strata(scored):
            baseline = metrics(decisions(table, 1.1))["baseline"]
            rows.append({"population": population, "system": "baseline", "threshold": None,
                         **{k: baseline.get(k) for k in ("precision", "coverage", "correct", "wrong")}})
            for threshold in (0.5, 0.6, 0.7, 0.8, 0.9):
                stats = metrics(decisions(table, threshold))["model"]
                rows.append({"population": population, "system": "model", "threshold": threshold,
                             **{k: stats.get(k) for k in ("precision", "coverage", "correct", "wrong")}})
        with pl.Config(tbl_rows=60, tbl_width_chars=160):
            print(pl.DataFrame(rows))
        return 0

    if args.command == "train":
        if args.output.exists():
            parser.error(f"output already exists: {args.output}")
        if bool(args.candidates) == bool(args.region):
            parser.error("train needs either --candidates or --region")
        table = (attach_weights(pl.read_parquet(args.candidates), args.weights) if args.candidates
                 else pooled_regions(args.region))
        training, validation = _split(table)
        model = train_two_stage(training, l2=args.l2)
        scored = post_mass(model.probabilities(validation.filter(pl.col("labelled"))))
        thresholds = calibrate_stratified(scored, args.target_precision)
        validation_report = {population: metrics(decisions(part, thresholds)) for population, part in strata(scored)}
        final = train_two_stage(table, l2=args.l2)
        final.thresholds = thresholds
        args.output.mkdir(parents=True)
        (args.output / "model.json").write_text(json.dumps(final.to_json(), indent=2) + "\n", encoding="utf8")
        report = {
            "target_precision": args.target_precision,
            "thresholds": thresholds,
            "validation": validation_report,
            "stage2_collective_coefficients": dict(zip(
                final.stage2.columns[-len(COLLECTIVE_COLUMNS):],
                np.round(final.stage2.weights[-len(COLLECTIVE_COLUMNS):], 4).tolist())),
        }
        (args.output / "training-report.json").write_text(json.dumps(report, indent=2) + "\n", encoding="utf8")
        print(json.dumps(report, indent=2))
    else:
        data = json.loads(args.model.read_text(encoding="utf8"))
        model = TwoStageModel.from_json(data) if data.get("format") == TWO_STAGE_FORMAT else Model.from_json(data)
        threshold = model.thresholds if isinstance(model, TwoStageModel) else model.threshold
        result = decisions(post_mass(model.probabilities(attach_weights(pl.read_parquet(args.candidates), args.weights))), threshold)
        if args.decisions_out:
            result.write_parquet(args.decisions_out)
        print(json.dumps(metrics(result), indent=2))
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
