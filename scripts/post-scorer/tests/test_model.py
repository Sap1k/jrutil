import numpy as np
import polars as pl
from scipy.optimize import approx_fprime

from post_scorer.model import ContextBatch, Model, _segment_softmax, decisions, fit, metrics, post_mass, train


def synthetic(contexts=400, seed=7, no_candidate_share=0.2):
    """Bays (in_bus_station) are correct; a street post has lower routed excess."""
    rng = np.random.default_rng(seed)
    rows = []
    for index in range(contexts):
        stop = index // 4
        no_candidate = rng.random() < no_candidate_share
        correct = int(rng.integers(0, 3))
        for candidate in range(4):
            bay = candidate < 3
            positive = (not no_candidate) and candidate == correct
            rows.append(
                {
                    "context_id": f"c{index:04d}",
                    "hypothesis_id": f"h{index:04d}-{candidate}",
                    "stop_id": stop,
                    "signal": (3.0 if positive else 0.0) + rng.normal(0, 0.5) - (2.0 if no_candidate else 0.0),
                    "street": 0.0 if bay else 1.0,
                    "is_bay": int(bay),
                    "stop_bay_candidates": 3,
                    "y": int(positive),
                    "labelled": True,
                    "no_correct_candidate": no_candidate,
                    # The baseline always picks the street post.
                    "baseline_selected": not bay,
                    "baseline_resolution": "Physical",
                }
            )
    return pl.DataFrame(rows)


def test_gradient_matches_finite_differences():
    table = synthetic(contexts=30)
    batch = ContextBatch.from_table(table, ["signal", "street"])

    def loss(parameters):
        probabilities, null_probability = _segment_softmax(batch.features @ parameters[:2], parameters[2], batch.starts)
        chosen = np.add.reduceat(batch.y * probabilities, batch.starts) + batch.null_target * null_probability
        return -np.log(chosen).sum() + 0.5 * parameters[:2] @ parameters[:2]

    parameters = np.array([0.3, -0.2, 0.1])
    numeric = approx_fprime(parameters, loss, 1e-6)

    # fit's analytic gradient is exercised through its objective via L-BFGS; check it directly too.
    captured = {}

    def capture(fun, x0, jac, method):
        captured["objective"] = fun
        class Result:
            success = True
            x = x0
        return Result()

    import post_scorer.model as model_module
    original = model_module.minimize
    model_module.minimize = capture
    try:
        fit(batch)
    finally:
        model_module.minimize = original
    _, analytic = captured["objective"](parameters)
    np.testing.assert_allclose(analytic, numeric, rtol=1e-4, atol=1e-3)
    weights, null_utility = fit(batch)
    assert weights[0] > 1.0


def test_weighted_gradient_matches_finite_differences():
    table = synthetic(contexts=30, seed=11)
    weights = pl.DataFrame({"context_id": table["context_id"].unique().sort()}).with_row_index("i").with_columns(
        (1.0 + (pl.col("i") % 5).cast(pl.Float64)).alias("weight")).drop("i")
    batch = ContextBatch.from_table(table.join(weights, on="context_id"), ["signal", "street"])
    assert batch.context_weight is not None and batch.context_weight.max() == 5.0

    def loss(parameters):
        probabilities, null_probability = _segment_softmax(batch.features @ parameters[:2], parameters[2], batch.starts)
        chosen = np.add.reduceat(batch.y * probabilities, batch.starts) + batch.null_target * null_probability
        return -(batch.context_weight * np.log(chosen)).sum() + 0.5 * parameters[:2] @ parameters[:2]

    captured = {}

    def capture(fun, x0, jac, method):
        captured["objective"] = fun
        class Result:
            success = True
            x = x0
        return Result()

    import post_scorer.model as model_module
    original = model_module.minimize
    model_module.minimize = capture
    try:
        fit(batch)
    finally:
        model_module.minimize = original
    parameters = np.array([0.2, -0.4, 0.3])
    value, analytic = captured["objective"](parameters)
    np.testing.assert_allclose(value, loss(parameters), rtol=1e-9)
    np.testing.assert_allclose(analytic, approx_fprime(parameters, loss, 1e-6), rtol=1e-4, atol=1e-3)


def test_gradient_with_multiple_positive_candidates():
    table = synthetic(contexts=30).with_columns(
        # Mark the first bay positive too wherever a context has a positive: duplicate mapping of one post.
        pl.when(pl.col("hypothesis_id").str.ends_with("-0") & (pl.col("y").max().over("context_id") == 1))
        .then(1)
        .otherwise(pl.col("y"))
        .alias("y")
    )
    batch = ContextBatch.from_table(table, ["signal", "street"])
    assert (np.add.reduceat(batch.y, batch.starts) > 1).any()

    def loss(parameters):
        probabilities, null_probability = _segment_softmax(batch.features @ parameters[:2], parameters[2], batch.starts)
        chosen = np.add.reduceat(batch.y * probabilities, batch.starts) + batch.null_target * null_probability
        return -np.log(chosen).sum() + 0.5 * parameters[:2] @ parameters[:2]

    captured = {}

    def capture(fun, x0, jac, method):
        captured["objective"] = fun
        class Result:
            success = True
            x = x0
        return Result()

    import post_scorer.model as model_module
    original = model_module.minimize
    model_module.minimize = capture
    try:
        fit(batch)
    finally:
        model_module.minimize = original
    parameters = np.array([0.4, 0.1, -0.3])
    value, analytic = captured["objective"](parameters)
    np.testing.assert_allclose(value, loss(parameters), rtol=1e-9)
    np.testing.assert_allclose(analytic, approx_fprime(parameters, loss, 1e-6), rtol=1e-4, atol=1e-3)


def test_model_learns_signal_and_abstains_and_beats_street_baseline():
    table = synthetic()
    model = train(table, columns=["signal", "street"])
    model.threshold = 0.6
    result = decisions(model.probabilities(table), model.threshold)
    report = metrics(result)
    assert report["model"]["top1_accuracy"] > 0.95
    assert report["model"]["precision"] > 0.95
    assert report["model"]["abstain_on_no_candidate"] > 0.8
    assert report["model"]["street_over_bay"] == 0
    assert report["baseline"]["street_over_bay"] == report["baseline"]["published"]
    assert report["baseline"]["correct"] == 0


def test_decision_uses_physical_post_mass():
    # Three mappings of one bay (within 20 m) share 0.75; a street post has 0.25.
    scored = pl.DataFrame(
        {
            "context_id": ["c"] * 4,
            "hypothesis_id": ["bay-osm", "bay-cat1", "bay-cat2", "street"],
            "latitude": [50.5105, 50.51052, 50.51055, 50.5090],
            "longitude": [13.6579, 13.65792, 13.65785, 13.6583],
            "probability": [0.25, 0.26, 0.24, 0.25],
            "sources": ["osm", "external:Most", "external:MapaDUK", "osm"],
            "local_ref": [None, None, None, None],
            "null_probability": [0.0] * 4,
            "is_bay": [1, 1, 1, 0],
            "stop_bay_candidates": [3] * 4,
            "stop_id": [1] * 4,
            "baseline_selected": [False, False, False, True],
            "baseline_resolution": ["Physical"] * 4,
        }
    )
    result = decisions(scored, 0.7)
    row = result.row(0, named=True)
    assert row["hypothesis_id"] == "bay-cat1"  # highest point probability within the heaviest post
    assert abs(row["post_probability"] - 0.75) < 1e-9
    assert row["model_published"]  # no single point reaches 0.7, the post does
    assert metrics(result)["model"]["street_over_bay"] == 0


def test_adjacent_bays_from_one_source_are_not_merged():
    # Two OSM bays 6 m apart and numbered differently are distinct posts; a catalogue point
    # on bay 1 is its duplicate. Only bay 1's two points share mass.
    scored = pl.DataFrame(
        {
            "context_id": ["c"] * 3,
            "hypothesis_id": ["bay1-osm", "bay2-osm", "bay1-cat"],
            "latitude": [50.59370, 50.59375, 50.59371],
            "longitude": [13.61000, 13.61000, 13.61001],
            "probability": [0.4, 0.35, 0.25],
            "sources": ["osm", "osm", "external:MapaDUK"],
            "local_ref": ["1", "2", None],
        }
    )
    mass = dict(zip(*post_mass(scored).select("hypothesis_id", "post_probability").to_dict(as_series=False).values()))
    assert abs(mass["bay1-osm"] - 0.65) < 1e-9
    assert abs(mass["bay2-osm"] - 0.35) < 1e-9
    assert abs(mass["bay1-cat"] - 0.65) < 1e-9


def test_area_fallback_publishes_the_best_bay_when_no_single_bay_is_confident():
    # Three bus bays share 0.9 (0.4/0.3/0.2); a tram platform elsewhere 0.1.
    scored = pl.DataFrame({
        "context_id": ["c"] * 4, "hypothesis_id": ["bay1", "bay2", "bay3", "tram"],
        "post_id": ["p1", "p2", "p3", "p4"], "area_id": ["bus", "bus", "bus", "tram"],
        "probability": [0.4, 0.3, 0.2, 0.1], "post_probability": [0.4, 0.3, 0.2, 0.1],
        "null_probability": [0.0] * 4, "stop_post_count": [4] * 4, "stop_bay_candidates": [3] * 4,
        "is_bay": [1, 1, 1, 0], "baseline_selected": [False, False, False, True],
        "baseline_resolution": ["Physical"] * 4, "stop_id": [1] * 4,
        "y": [1, 0, 0, 0], "labelled": [True] * 4, "no_correct_candidate": [False] * 4,
    })
    thresholds = {"simple": 0.75, "complex": 0.75}
    assert decisions(scored, thresholds)["model_resolution"].item() == "Abstain"
    row = decisions(scored, {**thresholds, "area": 0.8}).row(0, named=True)
    assert row["model_resolution"] == "Area" and row["hypothesis_id"] == "bay1"
    assert row["model_y"] == 1 and row["area_has_truth"]
    stats = metrics(decisions(scored, {**thresholds, "area": 0.8}))["model"]
    assert (stats["area_decisions"], stats["area_post_correct"], stats["area_contains_truth"]) == (1, 1, 1)


def test_model_json_round_trip():
    model = train(synthetic(contexts=40), columns=["signal", "street"])
    restored = Model.from_json(model.to_json())
    table = synthetic(contexts=10, seed=3)
    left = model.probabilities(table)["probability"].to_numpy()
    right = restored.probabilities(table)["probability"].to_numpy()
    np.testing.assert_allclose(left, right)
