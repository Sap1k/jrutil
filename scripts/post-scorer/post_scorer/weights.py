"""Traffic weights: how many vehicle departures each evidence context stands for.

Usage:
    uv run python -m post_scorer.weights --context-calls CONTEXT_CALLS.parquet \
        --base-gtfs BASE_BUNDLE/gtfs.zip [--start 2025-12-14 --days 364] --output context_weights.parquet

weight(context) = sum over trips using the context of their active days in the
reference window (default: the whole GVD year). A wrong post
on a line with 100 trips a day matters far more than one with two.
"""

from __future__ import annotations

import argparse
import io
import json
import zipfile
from datetime import date, timedelta
from pathlib import Path

import polars as pl

WEEKDAYS = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]


def active_days(calendar: pl.DataFrame, calendar_dates: pl.DataFrame, start: date, days: int) -> pl.DataFrame:
    """Active service days per service_id within [start, start + days)."""
    window = pl.DataFrame({"date": [start + timedelta(days=offset) for offset in range(days)]}).with_columns(
        pl.col("date").dt.weekday().alias("weekday"),  # 1 = Monday
        pl.col("date").dt.strftime("%Y%m%d").alias("key"),
    )
    regular = (
        calendar.with_columns([pl.col(day).cast(pl.Int32) for day in WEEKDAYS])
        .join(window, how="cross")
        .filter(
            (pl.col("key") >= pl.col("start_date")) & (pl.col("key") <= pl.col("end_date"))
            & pl.concat_list([pl.col(day) for day in WEEKDAYS]).list.get(pl.col("weekday") - 1).eq(1)
        )
        .select("service_id", "key")
    )
    exceptions = calendar_dates.join(window.select("key"), left_on="date", right_on="key", how="semi").select(
        "service_id", pl.col("date").alias("key"), pl.col("exception_type").cast(pl.Int32)
    )
    added = exceptions.filter(pl.col("exception_type") == 1).select("service_id", "key")
    removed = exceptions.filter(pl.col("exception_type") == 2).select("service_id", "key")
    active = pl.concat([regular, added]).unique().join(removed, on=["service_id", "key"], how="anti")
    return active.group_by("service_id").agg(pl.len().alias("active_days"))


def context_weights(context_calls: pl.DataFrame, trips: pl.DataFrame, service_days: pl.DataFrame) -> pl.DataFrame:
    trip_days = trips.select("trip_id", "service_id").join(service_days, on="service_id", how="left").select(
        pl.col("trip_id").alias("gtfs_trip_id"), pl.col("active_days").fill_null(0)
    )
    return (
        context_calls.select("context_id", "gtfs_trip_id").unique()
        .join(trip_days, on="gtfs_trip_id", how="left")
        .group_by("context_id")
        .agg(pl.col("gtfs_trip_id").n_unique().alias("trips"), pl.col("active_days").fill_null(0).sum().alias("departures"))
        .sort("context_id")
    )


def _read(archive: zipfile.ZipFile, name: str, columns: list[str]) -> pl.DataFrame:
    if name not in archive.namelist():
        return pl.DataFrame({column: [] for column in columns}, schema={column: pl.Utf8 for column in columns})
    return pl.read_csv(io.BytesIO(archive.read(name)), columns=columns, infer_schema=False)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--context-calls", type=Path, required=True)
    parser.add_argument("--base-gtfs", type=Path, required=True)
    # JDF keeps every timetable version of the year, each running only within its validity,
    # so the window is the whole GVD year (2025-12-14 to 2026-12-12) rather than a few weeks.
    parser.add_argument("--start", type=date.fromisoformat, default=date(2025, 12, 14))
    parser.add_argument("--days", type=int, default=364)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    with zipfile.ZipFile(args.base_gtfs) as archive:
        calendar = _read(archive, "calendar.txt", ["service_id", *WEEKDAYS, "start_date", "end_date"])
        calendar_dates = _read(archive, "calendar_dates.txt", ["service_id", "date", "exception_type"])
        trips = _read(archive, "trips.txt", ["trip_id", "service_id"])
    weights = context_weights(pl.read_parquet(args.context_calls), trips,
                              active_days(calendar, calendar_dates, args.start, args.days))
    weights.write_parquet(args.output)
    print(json.dumps({"contexts": weights.height, "departures_total": int(weights["departures"].sum()),
                      "zero_departure_contexts": int((weights["departures"] == 0).sum()),
                      "departures_p50_p99": [float(weights["departures"].quantile(0.5)),
                                             float(weights["departures"].quantile(0.99))]}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
