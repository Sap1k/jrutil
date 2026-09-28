import polars as pl

from post_scorer.features import (
    FEATURE_COLUMNS,
    build_candidate_table,
    mode_compatible_expression,
    post_areas,
    post_clusters,
    post_modes,
    source_granularity,
)
from post_scorer.labels import NO_CORRECT_CANDIDATE

STOP = "jdf:stop:7"


def tables():
    hypotheses = pl.DataFrame(
        {
            "hypothesis_id": ["bay1", "bay2", "bay3", "street"],
            "stop_id": [7, 7, 7, 7],
            "representative_route_point_id": ["rp1", "rp2", "rp3", "rp4"],
            "member_route_point_ids": ["rp1", "rp2", "rp3", "rp4"],
            "member_observation_ids": ["osm:node:1", "osm:node:2", "osm:node:3", "osm:node:4"],
            "latitude": [50.5937, 50.5936, 50.5935, 50.5940],
            "longitude": [13.6100, 13.6102, 13.6104, 13.6090],
        }
    )
    osm = pl.DataFrame(
        {
            "observation_id": ["osm:node:1", "osm:node:2", "osm:node:3", "osm:node:4"],
            "local_ref": ["1", "2", "3", None],
            "ref": [None] * 4,
            "route_ref": ["412;415", None, None, None],
            "public_transport": ["platform"] * 4,
            "highway": ["bus_stop"] * 4,
            "amenity": [None] * 4,
        },
        schema_overrides={"ref": pl.Utf8},
    )
    scores = pl.DataFrame(
        {
            "context_id": ["c1"] * 4 + ["c1"],
            "candidate_id": ["bay1", "bay2", "bay3", "street", "bay1"],
            "variant_rank": [0, 0, 0, 0, 1],
            "alignment": [0.9, 0.9, 0.9, 1.0, 0.8],
            "side": [1.0] * 5,
            "proximity": [0.5, 0.5, 0.5, 0.9, 0.5],
            "routed_fit": [0.2, 0.2, 0.3, 1.0, 0.2],
            "routed_excess_metres": [400.0, 410.0, None, 0.0, 380.0],
            "corridor_distance": [20.0, 21.0, 22.0, 3.0, 20.0],
            "signed_lateral_offset": [-5.0, -5.0, -5.0, -3.0, -5.0],
            "corridor_heading": [10.0, 10.0, 10.0, 350.0, 10.0],
            "attachment_heading": [30.0, 10.0, 10.0, 10.0, 30.0],
            "eligible": [True, True, False, True, True],
            "tied_corridors_agree": [True] * 5,
            "alternative_corridor_count": [1] * 5,
            "modality_adjustment": [0.0] * 5,
            "popularity_adjustment": [0.0] * 5,
            "total": [0.6, 0.6, 0.5, 0.99, 0.6],
            "rejection_reason": [None] * 5,
        },
        schema_overrides={"rejection_reason": pl.Utf8},
    )
    assignments = pl.DataFrame(
        {"context_id": ["c1"], "resolution": ["Physical"], "selected_hypothesis_id": ["street"]}
    )
    contexts = pl.DataFrame(
        {
            "context_id": ["c1"],
            "gtfs_stop_place_id": [STOP],
            "line_id": ["590412"],
            "mode": ["A"],
            "same_stop_block_role": ["through"],
            "context_previous_stop_id": [None],
            "context_next_stop_id": ["jdf:stop:8"],
        },
        schema_overrides={"context_previous_stop_id": pl.Utf8},
    )
    variants = pl.DataFrame(
        {
            "context_id": ["c1"],
            "variant_rank": [0],
            "service_edge_count": [2],
            "restricted_access_edge_count": [0],
            "access_penalty_metres": [None],
        },
        schema_overrides={"access_penalty_metres": pl.Float64},
    )
    return dict(
        scores=scores, hypotheses=hypotheses, assignments=assignments, contexts=contexts,
        variants=variants, osm_tags=osm, stations=None,
    )


def test_candidate_table_features_and_labels():
    labels = pl.DataFrame({"context_id": ["c1"], "label": ["rp2"]})
    table = build_candidate_table(labels=labels, **tables())
    rows = {row["hypothesis_id"]: row for row in table.iter_rows(named=True)}
    assert table.height == 4  # variant rank 1 rows are not separate candidates
    assert set(FEATURE_COLUMNS) <= set(table.columns)
    # Numbered platforms form bays even without an amenity=bus_station area.
    assert [rows[h]["is_bay"] for h in ("bay1", "bay2", "bay3", "street")] == [1, 1, 1, 0]
    assert rows["street"]["stop_bay_candidates"] == 3
    # Relative excess: the street post is the context minimum; a missing excess is capped.
    assert rows["street"]["excess_minus_context_min"] == 0.0
    assert rows["bay1"]["excess_minus_context_min"] == 400.0
    assert rows["bay3"]["excess_missing"] == 1.0
    assert rows["bay3"]["routed_excess_metres_capped"] == 1000.0
    assert rows["bay1"]["heading_difference"] == 20.0
    assert rows["street"]["heading_difference"] == 20.0
    assert rows["bay1"]["anchor_previous_missing"] == 1.0
    assert rows["bay1"]["anchor_next_missing"] == 0.0
    assert rows["bay1"]["service_edge_count"] == 2.0
    assert [rows[h]["y"] for h in ("bay1", "bay2", "bay3", "street")] == [0, 1, 0, 0]
    assert rows["street"]["baseline_selected"] and not rows["bay2"]["baseline_selected"]
    for row in rows.values():
        assert all(row[column] is not None for column in FEATURE_COLUMNS)


def test_source_coordinates_mark_the_nearest_post_and_its_duplicates():
    # bay2 is ~18 m from bay1 but a distinct OSM platform, so only the nearest post is correct.
    labels = pl.DataFrame(
        {"context_id": ["c1"], "label": ["rp1"], "source_lat": [50.5937], "source_lon": [13.6100]}
    )
    table = build_candidate_table(labels=labels, **tables())
    rows = {row["hypothesis_id"]: row["y"] for row in table.iter_rows(named=True)}
    assert rows == {"bay1": 1, "bay2": 0, "bay3": 0, "street": 0}
    # A catalogue point 3 m from bay1 is the same physical post and also correct.
    base = tables()
    hypotheses = pl.concat([base["hypotheses"], pl.DataFrame({
        "hypothesis_id": ["bay1-cat"], "stop_id": [7], "representative_route_point_id": ["rp5"],
        "member_route_point_ids": ["rp5"], "member_observation_ids": ["external:MapaDUK.csv:sha256:x"],
        "latitude": [50.59372], "longitude": [13.61002]})])
    scores = pl.concat([base["scores"], base["scores"].filter(pl.col("candidate_id") == "bay1")
                        .filter(pl.col("variant_rank") == 0).with_columns(pl.lit("bay1-cat").alias("candidate_id"))])
    base.update(hypotheses=hypotheses, scores=scores)
    table = build_candidate_table(labels=labels, **base)
    rows = {row["hypothesis_id"]: row["y"] for row in table.iter_rows(named=True)}
    assert rows["bay1"] == 1 and rows["bay1-cat"] == 1 and rows["bay2"] == 0


def test_decin_myslbekova_clusters_into_two_posts_and_a_lone_point():
    # Real candidates of Děčín, Myslbekova: 0-3 are one post (four sources over 17.5 m),
    # 4-6 another, 7 (one source, ~30 m from anything) does not exist.
    facts = pl.DataFrame({
        "hypothesis_id": [f"h{i}" for i in range(8)],
        "stop_id": [1936] * 8,
        "latitude": [50.77993, 50.779947, 50.77993, 50.779923, 50.779793, 50.779839, 50.779793, 50.780202],
        "longitude": [14.215686, 14.215765, 14.21552, 14.215519, 14.216226, 14.21616, 14.216142, 14.215538],
        "local_ref": [None] * 8,
        "sources": ["external:MapaDUK.csv", "external:LibereckyKraj.csv", "external:QRideDUK.csv", "osm",
                    "osm", "external:QRideDUK.csv", "external:MapaDUK.csv", "external:UsteckyKraj.csv"],
    }, schema_overrides={"local_ref": pl.Utf8})
    posts = post_clusters(facts).sort("hypothesis_id")
    ids = dict(zip(posts["hypothesis_id"], posts["post_id"]))
    support = dict(zip(posts["hypothesis_id"], posts["post_support"]))
    assert len({ids[h] for h in ("h0", "h1", "h2", "h3")}) == 1
    assert len({ids[h] for h in ("h4", "h5", "h6")}) == 1
    assert ids["h0"] != ids["h4"] and ids["h7"] not in {ids["h0"], ids["h4"]}
    assert (support["h0"], support["h4"], support["h7"]) == (4, 3, 1)


def test_one_source_never_maps_a_post_twice():
    # Two OSM bays 4 m apart stay separate; differing local_ref never merge.
    facts = pl.DataFrame({
        "hypothesis_id": ["a", "b", "c", "d"], "stop_id": [1] * 4,
        "latitude": [50.0, 50.000036, 50.001, 50.001018], "longitude": [14.0, 14.0, 14.0, 14.0],
        "local_ref": [None, None, "1", "2"],
        "sources": ["osm", "osm", "osm", "external:X.csv"],
    }, schema_overrides={"local_ref": pl.Utf8})
    ids = dict(zip(*post_clusters(facts).select("hypothesis_id", "post_id").to_dict(as_series=False).values()))
    assert ids["a"] != ids["b"]
    assert ids["c"] != ids["d"]


def test_stop_level_sources_are_detected_and_are_not_posts():
    # At stops where OSM maps two posts, a post-level catalogue has two points and a
    # stop-level one (LibereckyKraj-like) has one point in the middle.
    rows = []
    for stop in range(25):
        rows += [(f"jdf:stop:{stop}", f"rp{stop}a", f"osm:node:{stop}1"),
                 (f"jdf:stop:{stop}", f"rp{stop}b", f"osm:node:{stop}2"),
                 (f"jdf:stop:{stop}", f"rp{stop}c", f"external:MapaDUK.csv:sha256:{stop}a"),
                 (f"jdf:stop:{stop}", f"rp{stop}d", f"external:MapaDUK.csv:sha256:{stop}b"),
                 (f"jdf:stop:{stop}", f"rp{stop}e", f"external:LibereckyKraj.csv:sha256:{stop}")]
    observations = pl.DataFrame(rows, schema=["gtfs_stop_place_id", "route_point_id", "observation_id"], orient="row")
    granularity = {row["source"]: row["stop_level"] for row in source_granularity(observations).iter_rows(named=True)}
    assert granularity == {"external:LibereckyKraj.csv": True, "external:MapaDUK.csv": False, "osm": False}

    # A candidate backed only by a stop-level source leaves the choice set.
    base = tables()
    hypotheses = pl.concat([base["hypotheses"], pl.DataFrame({
        "hypothesis_id": ["centroid"], "stop_id": [7], "representative_route_point_id": ["rp9"],
        "member_route_point_ids": ["rp9"], "member_observation_ids": ["external:LibereckyKraj.csv:sha256:z"],
        "latitude": [50.59365], "longitude": [13.61010]})])
    scores = pl.concat([base["scores"], base["scores"].filter((pl.col("candidate_id") == "bay1") & (pl.col("variant_rank") == 0))
                        .with_columns(pl.lit("centroid").alias("candidate_id"))])
    base.update(hypotheses=hypotheses, scores=scores)
    kept = build_candidate_table(labels=None, stop_level_sources=frozenset({"external:LibereckyKraj.csv"}), **base)
    assert "centroid" not in kept["hypothesis_id"].to_list()
    assert kept["candidate_count"].max() == 4.0
    assert "centroid" in build_candidate_table(labels=None, **base)["hypothesis_id"].to_list()


def test_modes_propagate_through_the_post_and_areas_split_by_mode():
    # Most, nádraží in miniature: a bus bay (OSM ROAD + catalogue) and, 170 m away, a tram
    # platform (OSM TRAM + a catalogue point 18 m away, outside the 15 m point inheritance).
    facts = pl.DataFrame({
        "hypothesis_id": ["bus-osm", "bus-cat", "tram-osm", "tram-cat"],
        "stop_id": [252] * 4,
        "latitude": [50.51052, 50.51054, 50.50904, 50.50890],
        "longitude": [13.65798, 13.65793, 13.65826, 13.65835],
        "local_ref": [None] * 4,
        "sources": ["osm", "external:MapaDUK.csv", "osm", "external:Most.csv"],
    }, schema_overrides={"local_ref": pl.Utf8})
    facts = facts.join(post_clusters(facts), on="hypothesis_id")
    hypotheses = pl.DataFrame({"hypothesis_id": facts["hypothesis_id"],
                               "member_route_point_ids": ["rp1", "rp2", "rp3", "rp4"]})
    route_points = pl.DataFrame({"route_point_id": ["rp1", "rp2", "rp3", "rp4"],
                                 "explicit_modes": ["ROAD", "", "TRAM", ""]})
    modes = post_modes(hypotheses, route_points, facts)
    by_id = dict(zip(modes["hypothesis_id"], modes["post_modes"].to_list()))
    assert by_id["tram-cat"] == ["TRAM"] and by_id["bus-cat"] == ["ROAD"]
    check = pl.DataFrame({"mode": ["A", "A", "E"], "post_modes": [["TRAM"], ["ROAD"], ["TRAM"]]})
    ok = check.select(mode_compatible_expression("mode", "post_modes").alias("ok"))["ok"].to_list()
    assert ok == [False, True, True]
    areas = dict(zip(*post_areas(facts, modes).select("hypothesis_id", "area_id").to_dict(as_series=False).values()))
    assert areas["bus-osm"] == areas["bus-cat"] and areas["tram-osm"] == areas["tram-cat"]
    assert areas["bus-osm"] != areas["tram-osm"]


def test_no_correct_candidate_contexts_have_no_positive():
    labels = pl.DataFrame({"context_id": ["c1"], "label": [NO_CORRECT_CANDIDATE]})
    table = build_candidate_table(labels=labels, **tables())
    assert table["y"].sum() == 0
    assert table["no_correct_candidate"].all() and table["labelled"].all()


def test_observation_raw_tags_parse_into_generic_tags():
    from post_scorer.osm_facts import observation_tags
    observations = pl.DataFrame({
        "observation_id": ["osm:node:1", "osm:node:2", "external:X.csv:sha256:a"],
        "raw_tags": ["highway=bus_stop;public_transport=platform;local_ref=3", "railway=tram_stop", ""],
    })
    tags = {r["observation_id"]: r for r in observation_tags(observations).iter_rows(named=True)}
    assert tags["osm:node:1"]["local_ref"] == "3" and tags["osm:node:1"]["public_transport"] == "platform"
    assert tags["osm:node:2"]["local_ref"] is None
    assert tags["external:X.csv:sha256:a"]["highway"] is None
