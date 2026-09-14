"""Find source-added journeys that duplicate another output journey on one date."""

import csv
import math
import sys
import tempfile
import zipfile
from collections import defaultdict
from pathlib import Path

import pyarrow.parquet as parquet


def rows(path):
    with path.open(encoding="utf-8-sig", newline="") as stream:
        yield from csv.DictReader(stream)


def seconds(value):
    if not value:
        return -1
    hour, minute, second = map(int, value.split(":"))
    return hour * 3600 + minute * 60 + second


def distance(left, right):
    lat1, lon1 = map(math.radians, left)
    lat2, lon2 = map(math.radians, right)
    dlat, dlon = lat2 - lat1, lon2 - lon1
    value = math.sin(dlat / 2) ** 2 + math.cos(lat1) * math.cos(lat2) * math.sin(dlon / 2) ** 2
    return 6371000 * 2 * math.atan2(math.sqrt(value), math.sqrt(1 - value))


def main():
    root = Path(sys.argv[1])
    audit_date = sys.argv[2].replace("-", "")
    if len(sys.argv) < 4:
        raise SystemExit("usage: audit_duplicate_journeys.py PACKAGE YYYYMMDD OUTPUT.csv")
    output = Path(sys.argv[3])
    temporary = tempfile.TemporaryDirectory(prefix="duplicate-journey-audit-")
    gtfs = Path(temporary.name) / "gtfs"
    gtfs.mkdir()
    with zipfile.ZipFile(root / "gtfs.zip") as archive:
        archive.extractall(gtfs)

    active_services = {
        row["service_id"]
        for row in rows(gtfs / "calendar_dates.txt")
        if row["date"] == audit_date and row["exception_type"] == "1"
    }
    routes = {row["route_id"]: row for row in rows(gtfs / "routes.txt")}
    active_trips = {
        row["trip_id"]: (row["route_id"], routes[row["route_id"]]["route_type"])
        for row in rows(gtfs / "trips.txt")
        if row["service_id"] in active_services
    }

    stop_rows = {row["stop_id"]: row for row in rows(gtfs / "stops.txt")}
    stop_points = {}
    for stop_id, row in stop_rows.items():
        parent = row.get("parent_station", "")
        place = stop_rows.get(parent, row) if parent else row
        if place.get("stop_lat") and place.get("stop_lon"):
            stop_points[stop_id] = float(place["stop_lat"]), float(place["stop_lon"])

    calls = defaultdict(list)
    for row in rows(gtfs / "stop_times.txt"):
        trip_id = row["trip_id"]
        if trip_id in active_trips and row["stop_id"] in stop_points:
            calls[trip_id].append(
                (int(row["stop_sequence"]), seconds(row["arrival_time"]), seconds(row["departure_time"]), stop_points[row["stop_id"]])
            )
    for values in calls.values():
        values.sort()

    index = defaultdict(list)
    for trip_id, values in calls.items():
        if values:
            route_id, mode = active_trips[trip_id]
            key = mode, values[0][2] // 60
            index[key].append(trip_id)

    jmk_sources_by_output = defaultdict(set)
    binding_rows = parquet.read_table(
        root / "serving" / "source_trip_map.parquet",
        columns=["source_id", "trip_namespace", "source_trip_id", "trip_id", "valid_from", "valid_to"],
    ).to_pylist()
    for row in binding_rows:
        if (row["source_id"] == "ids-jmk-gtfs"
                and row["valid_from"].strftime("%Y%m%d") <= audit_date <= row["valid_to"].strftime("%Y%m%d")
                and row["trip_id"] in calls):
            jmk_sources_by_output[row["trip_id"]].add(row["source_trip_id"])
    jmk_outputs = set(jmk_sources_by_output)
    operational_by_source = defaultdict(set)
    for row in binding_rows:
        if row["source_id"] == "ids-jmk-gtfs" and row["trip_namespace"] == "operational_line_course":
            operational_by_source[row["trip_id"]].add(row["source_trip_id"])
    findings = []
    source_confirmed_parallel = 0
    seen_pairs = set()
    for source_trip in sorted(jmk_outputs):
        source_calls = calls[source_trip]
        route_id, mode = active_trips[source_trip]
        first_minute, last_minute = source_calls[0][2] // 60, source_calls[-1][1] // 60
        candidates = set()
        for first_delta in range(-3, 4):
            candidates.update(index.get((mode, first_minute + first_delta), ()))
        for target_trip in sorted(candidates):
            pair = tuple(sorted((source_trip, target_trip)))
            if target_trip == source_trip or pair in seen_pairs:
                continue
            target_calls = calls[target_trip]
            if abs(last_minute - target_calls[-1][1] // 60) > 3:
                continue
            if distance(source_calls[0][3], target_calls[0][3]) > 200 or distance(source_calls[-1][3], target_calls[-1][3]) > 200:
                continue
            source_index = target_index = matched = 0
            maximum_time_delta = maximum_distance = 0
            while source_index < len(source_calls) and target_index < len(target_calls):
                source, target = source_calls[source_index], target_calls[target_index]
                time_delta = max(abs(source[1] - target[1]), abs(source[2] - target[2]))
                stop_distance = distance(source[3], target[3])
                if time_delta <= 180 and stop_distance <= 150:
                    matched += 1
                    maximum_time_delta = max(maximum_time_delta, time_delta)
                    maximum_distance = max(maximum_distance, stop_distance)
                    source_index += 1
                    target_index += 1
                elif source[2] < target[2]:
                    source_index += 1
                else:
                    target_index += 1
            coverage = min(matched / len(source_calls), matched / len(target_calls))
            if matched >= 2 and coverage >= 0.75 and maximum_time_delta <= 120:
                source_operational = operational_by_source[source_trip]
                target_operational = operational_by_source[target_trip]
                # Close headways and short-turn variants are valid distinct journeys.
                # When IDS JMK itself gives both outputs disjoint operational
                # line/course identities, the pair is confirmed by the source and
                # is not evidence that overlay identity matching created a copy.
                if source_operational and target_operational and source_operational.isdisjoint(target_operational):
                    seen_pairs.add(pair)
                    source_confirmed_parallel += 1
                    continue
                seen_pairs.add(pair)
                target_route = active_trips[target_trip][0]
                findings.append((
                    source_trip, route_id, routes[route_id].get("route_short_name", ""), target_trip, target_route,
                    routes[target_route].get("route_short_name", ""), len(source_calls), len(target_calls), matched,
                    f"{coverage:.3f}", maximum_time_delta, f"{maximum_distance:.1f}"
                ))

    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(("jmk_output_trip_id", "jmk_route_id", "jmk_route_short_name", "candidate_output_trip_id", "candidate_route_id", "candidate_route_short_name", "jmk_call_count", "candidate_call_count", "matched_calls", "coverage", "maximum_time_delta_seconds", "maximum_stop_distance_metres"))
        writer.writerows(findings)
    print(f"active_jmk_output_trips={len(jmk_outputs)}; source_confirmed_parallel={source_confirmed_parallel}; duplicate_candidates={len(findings)}; report={output}")
    if findings:
        raise SystemExit(2)


if __name__ == "__main__":
    main()
