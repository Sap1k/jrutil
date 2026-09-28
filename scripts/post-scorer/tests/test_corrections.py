import polars as pl

from post_scorer.corrections import REVIEWED, corrections_to_labels
from post_scorer.labels import NO_CORRECT_CANDIDATE


def test_corrections_label_every_context_of_the_movement_and_latest_wins():
    contexts = pl.DataFrame({
        "context_id": ["c1", "c2", "c3", "c4"],
        "gtfs_stop_place_id": ["jdf:stop:25057"] * 3 + ["jdf:stop:252"],
        "line_id": ["590400", "590400", "590400", "590400"],
        "direction": [0, 0, 1, 0],
        "context_previous_stop_id": ["jdf:stop:1", "jdf:stop:1", "jdf:stop:2", None],
        "context_next_stop_id": ["jdf:stop:2", "jdf:stop:2", "jdf:stop:1", "jdf:stop:9"],
    }, schema_overrides={"context_previous_stop_id": pl.Utf8})
    corrections = [
        {"stop_id": 25057, "line_id": "590400", "direction": 0, "previous_stop_id": "jdf:stop:1",
         "next_stop_id": "jdf:stop:2", "verdict": "post", "latitude": 50.62, "longitude": 13.69,
         "recorded_at": "2026-09-28T10:00:00Z"},
        # A later correction of the same movement replaces the first.
        {"stop_id": 25057, "line_id": "590400", "direction": 0, "previous_stop_id": "jdf:stop:1",
         "next_stop_id": "jdf:stop:2", "verdict": "post", "latitude": 50.63, "longitude": 13.70,
         "recorded_at": "2026-09-28T11:00:00Z"},
        {"stop_id": 252, "line_id": "590400", "direction": 0, "previous_stop_id": None,
         "next_stop_id": "jdf:stop:9", "verdict": "none", "latitude": None, "longitude": None,
         "recorded_at": "2026-09-28T12:00:00Z"},
    ]
    labels = corrections_to_labels(corrections, contexts)
    rows = {r["context_id"]: r for r in labels.iter_rows(named=True)}
    assert set(rows) == {"c1", "c2", "c4"}
    assert rows["c1"]["label"] == REVIEWED and rows["c1"]["source_lat"] == 50.63
    assert rows["c4"]["label"] == NO_CORRECT_CANDIDATE
