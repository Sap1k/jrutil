from datetime import date

import polars as pl

from post_scorer.weights import active_days, context_weights


def test_active_days_follow_weekdays_validity_and_exceptions():
    calendar = pl.DataFrame({
        "service_id": ["weekday", "weekend"],
        "monday": ["1", "0"], "tuesday": ["1", "0"], "wednesday": ["1", "0"], "thursday": ["1", "0"],
        "friday": ["1", "0"], "saturday": ["0", "1"], "sunday": ["0", "1"],
        "start_date": ["20261001", "20261001"], "end_date": ["20261231", "20261010"],
    })
    # 2026-10-05 is a Monday. Remove Wednesday 7th for weekday, add Monday 12th for weekend.
    calendar_dates = pl.DataFrame({"service_id": ["weekday", "weekend"], "date": ["20261007", "20261012"],
                                   "exception_type": ["2", "1"]})
    days = dict(zip(*active_days(calendar, calendar_dates, date(2026, 10, 5), 14).to_dict(as_series=False).values()))
    assert days["weekday"] == 10 - 1
    # Weekend runs 10-10 only (validity ends) plus the added 12th.
    assert days["weekend"] == 2


def test_context_weights_sum_departures_over_distinct_trips():
    calls = pl.DataFrame({"context_id": ["a", "a", "a", "b"], "gtfs_trip_id": ["t1", "t1", "t2", "t3"]})
    trips = pl.DataFrame({"trip_id": ["t1", "t2", "t3"], "service_id": ["s5", "s2", "none"]})
    days = pl.DataFrame({"service_id": ["s5", "s2"], "active_days": [5, 2]})
    weights = {r["context_id"]: r for r in context_weights(calls, trips, days).iter_rows(named=True)}
    assert (weights["a"]["trips"], weights["a"]["departures"]) == (2, 7)
    assert (weights["b"]["trips"], weights["b"]["departures"]) == (1, 0)
