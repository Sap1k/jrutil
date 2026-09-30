"""Write a local Leaflet review map comparing model and current-policy post picks.

Usage:
    uv run python -m post_scorer.review_map --candidates ustecky-candidates.parquet \
        --decisions ustecky-decisions.parquet --hypotheses EXPORT/hypotheses.parquet \
        --contexts EVIDENCE/contexts.parquet --merged-jdf merged-jdf.zip --output review-map.html

Open the HTML file in a browser; it loads Leaflet from cdnjs and OpenStreetMap tiles.
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import re
import zipfile
from pathlib import Path

import polars as pl

from post_scorer.labels import jdf_stop_number

TERMINAL_NAME = r"nádr|aut\.?\s*st|\bAN\b|terminál"


def stop_names(merged_jdf: Path) -> dict[int, str]:
    with zipfile.ZipFile(merged_jdf) as archive:
        text = archive.read("Zastavky.txt").decode("cp1250")
    names = {}
    for row in csv.reader(io.StringIO(text.replace('";\r\n', '"\r\n').replace('";\n', '"\n'))):
        if len(row) >= 4 and row[0].isdigit():
            names[int(row[0])] = ",".join(part for part in row[1:4] if part) or row[1]
    return names


def source_label(observation_ids: str) -> str:
    kinds = []
    for identity in observation_ids.split(";"):
        if identity.startswith("osm:"):
            kinds.append("OSM")
        elif identity.startswith("external:"):
            kinds.append(identity.split(":")[1].removesuffix(".csv"))
    return ", ".join(sorted(set(kinds))) or "?"


def policy_picks(assignments):
    """Current-policy post per context: the selected post, if any."""
    return assignments.select(
        "context_id", pl.col("resolution").alias("baseline_resolution"),
        pl.col("selected_hypothesis_id").alias("baseline_pick"),
    )


def build(candidates, decisions, hypotheses, contexts, assignments, names, disagreement_stops=150,
          include_stops: str = ""):
    include = re.compile("|".join(p for p in (TERMINAL_NAME, include_stops) if p), re.IGNORECASE)
    per_stop = candidates.group_by("stop_id").agg(
        pl.col("stop_bay_candidates").first(), pl.col("hypothesis_id").n_unique().alias("candidate_count")
    )
    joined = decisions.select(
        "context_id", pl.col("hypothesis_id").alias("model_pick"),
        pl.when(pl.col("model_resolution") == "Area").then(pl.col("area_probability")).otherwise(pl.col("post_probability")).alias("probability"),
        "model_published", "model_resolution"
    ).join(
        policy_picks(assignments), on="context_id", how="left",
    ).join(
        contexts.select(
            "context_id", jdf_stop_number("gtfs_stop_place_id").alias("stop_id"), "line_id", "direction",
            "same_stop_block_role", "context_previous_stop_id", "context_next_stop_id", "mode",
        ),
        on="context_id",
    )

    disagreement = (
        joined.filter(pl.col("model_published") & (pl.col("model_pick") != pl.col("baseline_pick").fill_null("")))
        .group_by("stop_id").agg(pl.len().alias("disagreements"))
        .join(per_stop, on="stop_id").filter(pl.col("candidate_count") >= 4)
        .sort("disagreements", descending=True).head(disagreement_stops)
    )
    selected = set(per_stop.filter(pl.col("stop_bay_candidates") >= 2)["stop_id"].to_list())
    selected |= {stop for stop in per_stop["stop_id"].to_list() if include.search(names.get(stop, ""))}
    selected |= set(disagreement["stop_id"].to_list())

    hypothesis_facts = candidates.select(
        "hypothesis_id", "stop_id", "latitude", "longitude", "local_ref", "route_ref", "is_bay", "in_bus_station", "post_id", "post_support"
    ).unique("hypothesis_id").join(hypotheses.select("hypothesis_id", "member_observation_ids"), on="hypothesis_id")

    def neighbour(value):
        match = re.search(r":stop:(\d+)", value or "")
        return names.get(int(match.group(1)), match.group(1)) if match else None

    stops = []
    for stop_id in sorted(selected):
        facts = hypothesis_facts.filter(pl.col("stop_id") == stop_id)
        if facts.height == 0:
            continue
        index = {row["hypothesis_id"]: position for position, row in enumerate(facts.iter_rows(named=True))}
        movements = (
            joined.filter(pl.col("stop_id") == stop_id)
            .group_by("line_id", "direction", "same_stop_block_role", "context_previous_stop_id",
                      "context_next_stop_id", "model_pick", "baseline_pick", "baseline_resolution", "model_published", "model_resolution")
            .agg(pl.len().alias("contexts"), pl.col("probability").mean())
            .sort("line_id", "direction", "context_previous_stop_id")
        )
        stops.append({
            "id": stop_id,
            "name": names.get(stop_id, str(stop_id)),
            "candidates": [
                {"lat": round(row["latitude"], 6), "lon": round(row["longitude"], 6), "ref": row["local_ref"],
                 "routes": row["route_ref"], "bay": bool(row["is_bay"]), "station": bool(row["in_bus_station"]),
                 "source": source_label(row["member_observation_ids"]),
                 "post": row["post_id"].split(":")[-1], "support": int(row["post_support"])}
                for row in facts.iter_rows(named=True)
            ],
            "movements": [
                {"line": row["line_id"], "dir": row["direction"], "role": row["same_stop_block_role"],
                 "from": neighbour(row["context_previous_stop_id"]), "to": neighbour(row["context_next_stop_id"]),
                 "fromId": row["context_previous_stop_id"], "toId": row["context_next_stop_id"],
                 "model": index.get(row["model_pick"]), "p": round(row["probability"], 3),
                 "published": bool(row["model_published"]), "model_resolution": row["model_resolution"], "baseline": index.get(row["baseline_pick"]),
                 "resolution": row["baseline_resolution"], "n": row["contexts"]}
                for row in movements.iter_rows(named=True)
            ],
        })
    return stops


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    for name in ("candidates", "decisions", "hypotheses", "contexts", "assignments", "merged-jdf", "output"):
        parser.add_argument(f"--{name}", type=Path, required=True)
    parser.add_argument("--title", default="Ústecký post review")
    parser.add_argument("--include-stops", default="", help="extra regex of stop names to always include")
    args = parser.parse_args(argv)
    stops = build(
        pl.read_parquet(args.candidates), pl.read_parquet(args.decisions), pl.read_parquet(args.hypotheses),
        pl.read_parquet(args.contexts), pl.read_parquet(args.assignments),
        stop_names(args.merged_jdf), include_stops=args.include_stops,
    )
    template = Path(__file__).with_name("review_map.html").read_text(encoding="utf8")
    payload = json.dumps(stops, ensure_ascii=False, separators=(",", ":")).replace("</", "<\\/")
    html = template.replace("__TITLE__", args.title).replace("__DATA__", payload)
    args.output.write_text(html, encoding="utf8")
    print(json.dumps({"stops": len(stops), "movements": sum(len(s["movements"]) for s in stops),
                      "bytes": len(html.encode("utf8"))}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
