"""Audit referential integrity of a generated regional overlay bundle."""

import csv
import sqlite3
import sys
import tempfile
import zipfile
from pathlib import Path

import pyarrow.parquet as parquet


def rows(path):
    with path.open(encoding="utf-8-sig", newline="") as stream:
        yield from csv.DictReader(stream)


def main():
    root = Path(sys.argv[1])
    extensions = root / "extensions"
    failures = []
    checked = 0
    with tempfile.TemporaryDirectory(prefix="overlay-reference-audit-") as scratch:
        gtfs = Path(scratch) / "gtfs"
        gtfs.mkdir()
        with zipfile.ZipFile(root / "gtfs.zip") as archive:
            archive.extractall(gtfs)
        db = sqlite3.connect(Path(scratch) / "references.db")
        db.execute("PRAGMA cache_size=-32768")
        db.execute("CREATE TABLE ids(kind TEXT, id TEXT, PRIMARY KEY(kind,id)) WITHOUT ROWID")

        def load(kind, path, column):
            if path.exists():
                db.executemany("INSERT OR IGNORE INTO ids VALUES (?,?)", ((kind, row[column]) for row in rows(path) if row.get(column)))
                db.commit()

        load("agency", gtfs / "agency.txt", "agency_id")
        load("route", gtfs / "routes.txt", "route_id")
        load("trip", gtfs / "trips.txt", "trip_id")
        load("stop", gtfs / "stops.txt", "stop_id")
        load("service", gtfs / "calendar.txt", "service_id")
        load("service", gtfs / "calendar_dates.txt", "service_id")

        def audit(path, references):
            nonlocal checked
            if not path.exists():
                return
            cursors = {kind: db.cursor() for _, kind in references}
            # GTFS call and mapping tables repeat the same trip/stop IDs millions
            # of times. Keep only the boolean lookup result, while the complete ID
            # index itself remains in bounded SQLite scratch storage.
            validity = {kind: {} for _, kind in references}
            for number, row in enumerate(rows(path), 2):
                for column, kind in references:
                    value = row.get(column, "")
                    if value:
                        checked += 1
                        cache = validity[kind]
                        valid = cache.get(value)
                        if valid is None:
                            valid = cursors[kind].execute("SELECT 1 FROM ids WHERE kind=? AND id=?", (kind, value)).fetchone() is not None
                            cache[value] = valid
                        if not valid:
                            failures.append(f"{path.relative_to(root)}:{number}:{column}={value}")
                            if len(failures) >= 20:
                                raise AssertionError("Invalid references:\n" + "\n".join(failures))

        audit(gtfs / "routes.txt", [("agency_id", "agency")])
        audit(gtfs / "trips.txt", [("route_id", "route"), ("service_id", "service")])
        audit(gtfs / "stops.txt", [("parent_station", "stop")])
        audit(gtfs / "stop_times.txt", [("trip_id", "trip"), ("stop_id", "stop")])
        audit(gtfs / "transfers.txt", [("from_stop_id", "stop"), ("to_stop_id", "stop"), ("from_route_id", "route"), ("to_route_id", "route"), ("from_trip_id", "trip"), ("to_trip_id", "trip")])
        audit(extensions / "cz_route_stop_zones.txt", [("route_id", "route"), ("stop_id", "stop")])
        audit(extensions / "cz_call_zones.txt", [("trip_id", "trip")])
        audit(extensions / "cz_transfer_constraints.txt", [("from_stop_id", "stop"), ("to_stop_id", "stop"), ("from_route_id", "route"), ("to_route_id", "route"), ("from_trip_id", "trip"), ("to_trip_id", "trip")])

        bindings = parquet.read_table(root / "serving" / "source_trip_map.parquet", columns=["binding_id", "trip_id"]).to_pylist()
        db.execute("CREATE TABLE bindings(binding_id TEXT PRIMARY KEY, trip_id TEXT NOT NULL) WITHOUT ROWID")
        db.executemany("INSERT INTO bindings VALUES (?,?)", ((row["binding_id"], row["trip_id"]) for row in bindings))
        db.commit()
        cursor = db.cursor()
        for relation in ("source_call_map", "source_trip_coverage"):
            for row in parquet.read_table(root / "serving" / f"{relation}.parquet", columns=["binding_id"]).to_pylist():
                checked += 1
                if cursor.execute("SELECT 1 FROM bindings WHERE binding_id=?", (row["binding_id"],)).fetchone() is None:
                    failures.append(f"serving/{relation}.parquet:binding_id={row['binding_id']}")
        db.close()
    if failures:
        raise AssertionError("Invalid references:\n" + "\n".join(failures[:20]))
    print(f"reference_checks={checked}; invalid_references=0")


if __name__ == "__main__":
    main()
