import polars as pl

from post_scorer.labels import (
    NO_CORRECT_CANDIDATE,
    LabelSettings,
    authored_local_ref_labels,
    base_call_occurrences,
    build_labels,
)


def test_authored_post_numbers_match_unique_osm_local_refs():
    contexts = pl.DataFrame(
        {
            "context_id": ["a", "b", "c", "d"],
            "gtfs_stop_place_id": ["jdf:stop:5", "jdf:stop:5", "jdf:stop:5", "jdf:stop:6"],
            "authored_post_key": ["num:2", "num: 9", "num:3", None],
        }
    )
    hypotheses = pl.DataFrame(
        {
            "hypothesis_id": ["h1", "h2", "h3", "h4"],
            "stop_id": [5, 5, 5, 6],
            "representative_route_point_id": ["rp1", "rp2", "rp3", "rp4"],
            "member_observation_ids": ["osm:node:1", "osm:node:2;external:x", "osm:node:3", "osm:node:4"],
        }
    )
    tags = pl.DataFrame(
        {
            "observation_id": ["osm:node:1", "osm:node:2", "osm:node:3", "osm:node:4"],
            # Two candidates claim "3": ambiguous, dropped. "9" does not exist.
            "local_ref": ["3", "2", "3", "1"],
        }
    )
    labels = authored_local_ref_labels(contexts, hypotheses, tags)
    assert labels.to_dicts() == [{"context_id": "a", "label": "rp2"}]

TRIP = "jdf:trip:100001:1:1"
TRIP_TWO = "jdf:trip:100001:1:3"


def fixture(**overrides):
    # Stop 10 is a terminal with bays A (50.0000,14.0000) and B (50.0003,14.0000).
    # Stop 20 is visited twice by TRIP (a loop), at different posts.
    tables = dict(
        evidence_contexts=pl.DataFrame({"context_id": ["ctx-10", "ctx-20a", "ctx-20b", "ctx-30"]}),
        route_points=pl.DataFrame(
            {
                "gtfs_stop_place_id": ["jdf:stop:10", "jdf:stop:10", "jdf:stop:20", "jdf:stop:20", "jdf:stop:30"],
                "route_point_id": ["rp-10a", "rp-10b", "rp-20x", "rp-20y", "rp-30"],
                "latitude": [50.0, 50.0003, 50.01, 50.0103, 50.02],
                "longitude": [14.0, 14.0, 14.0, 14.0, 14.0],
            }
        ),
        context_calls=pl.DataFrame(
            {
                "gtfs_trip_id": [TRIP, TRIP, TRIP, TRIP, TRIP_TWO],
                "stop_id": [10, 20, 30, 20, 10],
                "stop_occurrence": [0, 0, 0, 1, 0],
                "context_id": ["ctx-10", "ctx-20a", "ctx-30", "ctx-20b", "ctx-10"],
            }
        ),
        base_stop_times=pl.DataFrame(
            {
                "trip_id": [TRIP, TRIP, TRIP, TRIP, TRIP_TWO],
                # Ordinals come from stop_sequence order, not file order.
                "stop_sequence": ["1", "2", "3", "4", "1"],
                "stop_id": ["jdf:stop:10", "jdf:stop:20:unspecified", "jdf:stop:30", "jdf:stop:20", "jdf:stop:10"],
            }
        ),
        trip_mappings=pl.DataFrame(
            {
                "source_id": ["pid-gtfs", "pid-gtfs", "pid-gtfs"],
                "source_trip_id": ["p1", "p2", "p3"],
                "base_trip_id": [TRIP, TRIP_TWO, "jdf:trip:edited"],
                "output_trip_id": ["o1", "o2", "o3"],
                "pattern_edits": ["0", "0", "2"],
            }
        ),
        call_mappings=pl.DataFrame(
            {
                "source_id": ["pid-gtfs"] * 6,
                "source_trip_id": ["p1", "p1", "p1", "p1", "p2", "p3"],
                "source_stop_id": ["U10Z2", "U20Z1", "U30Z9", "U20Z2", "U10Z2", "U10Z1"],
                "output_trip_id": ["o1", "o1", "o1", "o1", "o2", "o3"],
                "output_call_ordinal": ["1", "2", "3", "4", "1", "1"],
            }
        ),
        source_stops=pl.DataFrame(
            {
                "stop_id": ["U10Z1", "U10Z2", "U20Z1", "U20Z2", "U30Z9"],
                "stop_lat": ["50.0", "50.00031", "50.01", "50.0103", "50.03"],
                "stop_lon": ["14.0", "14.0", "14.0", "14.0", "14.0"],
            }
        ),
    )
    tables.update(overrides)
    return tables


def labels_by_context(labels):
    return dict(zip(labels["context_id"].to_list(), labels["label"].to_list()))


def test_base_call_occurrences_number_repeated_stops():
    calls = base_call_occurrences(fixture()["base_stop_times"]).filter(pl.col("trip_id") == TRIP)
    assert calls.sort("call_ordinal")["stop_occurrence"].to_list() == [0, 0, 0, 1]
    assert calls.sort("call_ordinal")["jdf_stop_id"].to_list() == [10, 20, 30, 20]


def test_labels_follow_exact_call_chain_and_loop_occurrences():
    labels, report = build_labels(source_id="pid-gtfs", **fixture())
    by_context = labels_by_context(labels)
    # Both trips use bay B; the edited binding (bay A) is ignored.
    assert by_context["ctx-10"] == "rp-10b"
    assert labels.filter(pl.col("context_id") == "ctx-10")["total_calls"].item() == 2
    assert by_context["ctx-20a"] == "rp-20x"
    assert by_context["ctx-20b"] == "rp-20y"
    # PID post for stop 30 is 1.1 km from every candidate: alignment-suspect, dropped.
    assert "ctx-30" not in by_context
    assert report.dropped_alignment_suspect == 1
    assert report.labelled_candidate == 3


def test_distant_post_becomes_no_correct_candidate():
    stops = fixture()["source_stops"].with_columns(
        pl.when(pl.col("stop_id") == "U20Z1").then(pl.lit("50.0101")).otherwise(pl.col("stop_lat")).alias("stop_lat")
    )
    # 50.0101 is ~11 m from rp-20x: labelled.
    labels, _ = build_labels(source_id="pid-gtfs", **fixture(source_stops=stops))
    assert labels_by_context(labels)["ctx-20a"] == "rp-20x"

    def moved(latitude):
        return fixture()["source_stops"].with_columns(
            pl.when(pl.col("stop_id") == "U20Z1").then(pl.lit(latitude)).otherwise(pl.col("stop_lat")).alias("stop_lat")
        )

    # ~33 m from rp-20y lies in the 20-35 m ambiguity band: dropped, not labelled.
    labels, report = build_labels(source_id="pid-gtfs", **fixture(source_stops=moved("50.0106")))
    assert "ctx-20a" not in labels_by_context(labels)
    assert report.dropped_ambiguous_distance == 1
    # ~78 m from every candidate: no correct candidate.
    labels, report = build_labels(source_id="pid-gtfs", **fixture(source_stops=moved("50.0110")))
    assert labels_by_context(labels)["ctx-20a"] == NO_CORRECT_CANDIDATE
    assert report.labelled_no_correct_candidate == 1


def test_disagreeing_contexts_are_dropped():
    tables = fixture()
    calls = tables["call_mappings"].with_columns(
        pl.when(pl.col("source_trip_id") == "p2").then(pl.lit("U10Z1")).otherwise(pl.col("source_stop_id")).alias(
            "source_stop_id"
        )
    )
    labels, report = build_labels(source_id="pid-gtfs", **fixture(call_mappings=calls))
    assert "ctx-10" not in labels_by_context(labels)
    assert report.dropped_disagreeing == 1
    relaxed, _ = build_labels(
        source_id="pid-gtfs", settings=LabelSettings(minimum_share=0.5), **fixture(call_mappings=calls)
    )
    assert labels_by_context(relaxed)["ctx-10"] in {"rp-10a", "rp-10b"}


def test_contexts_absent_from_evidence_are_not_labelled():
    evidence = pl.DataFrame({"context_id": ["ctx-20a"]})
    labels, report = build_labels(source_id="pid-gtfs", **fixture(evidence_contexts=evidence))
    assert labels["context_id"].to_list() == ["ctx-20a"]
    assert report.contexts_in_evidence == 1
