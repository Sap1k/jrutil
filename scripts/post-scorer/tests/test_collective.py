import polars as pl

from post_scorer.collective import collective_features


def test_reverse_and_same_movement_mass():
    # Stop 5, posts P0 (north side) and P1 (south side).
    # Two lines A->B favour P0; the reverse movement B->A favours P1; a terminus
    # departure towards B has no reverse other than the arrival from B.
    rows = []
    for context, previous, next_, p0 in (
        ("ab-1", "A", "B", 0.9), ("ab-2", "A", "B", 0.7), ("ba-1", "B", "A", 0.2), ("start", None, "B", 0.5),
        ("end", "B", None, 0.3),
    ):
        rows += [(context, 5, "5:0", f"{context}-h0", previous, next_, p0),
                 (context, 5, "5:1", f"{context}-h1", previous, next_, 1 - p0)]
    scored = pl.DataFrame(rows, schema=["context_id", "stop_id", "post_id", "hypothesis_id", "previous_stop",
                                        "next_stop", "probability"], orient="row")
    result = collective_features(scored)
    value = {(r["context_id"], r["post_id"]): r for r in result.iter_rows(named=True)}
    # Reverse of A->B is B->A (one context): its mass on P0 is 0.2.
    assert abs(value[("ab-1", "5:0")]["reverse_mass"] - 0.2) < 1e-9
    assert value[("ab-1", "5:0")]["has_reverse"] == 1.0
    # Same movement excludes the context itself: ab-1 sees ab-2's 0.7 on P0.
    assert abs(value[("ab-1", "5:0")]["same_movement_mass"] - 0.7) < 1e-9
    assert value[("ba-1", "5:0")]["has_same_movement"] == 0.0
    assert abs(value[("ba-1", "5:1")]["reverse_mass"] - (0.1 + 0.3) / 2) < 1e-9
    # Terminus departure (∅ -> B) pairs with the arrival from B (B -> ∅).
    assert abs(value[("start", "5:0")]["reverse_mass"] - 0.3) < 1e-9
    assert abs(value[("end", "5:0")]["reverse_mass"] - 0.5) < 1e-9
    assert result.height == scored.height
