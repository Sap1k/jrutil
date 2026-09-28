"""OSM facts for candidate posts: bay tags and bus-station membership.

Inputs are `osmium export -f geojsonseq --add-unique-id=type_id` outputs of
the JDF post-candidate extract (nodes with tags) and of `amenity=bus_station`
objects filtered from the source snapshot. Only generic tags are kept; network
specific references such as `ref:PID` are deliberately dropped so a model
trained in one network cannot key on them.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import polars as pl

GENERIC_TAGS = ("local_ref", "ref", "route_ref", "public_transport", "highway", "amenity")
STATION_POINT_RADIUS_METRES = 60.0
STATION_POLYGON_BUFFER_METRES = 15.0


def _features(path: Path):
    with path.open(encoding="utf8") as stream:
        for line in stream:
            line = line.strip().lstrip("\x1e")
            if line:
                yield json.loads(line)


def candidate_node_tags(path: Path) -> pl.DataFrame:
    rows = []
    for feature in _features(path):
        identity = feature.get("id", "")
        if not identity.startswith("n"):
            continue
        properties = feature.get("properties", {})
        rows.append({"observation_id": f"osm:node:{identity[1:]}", **{tag: properties.get(tag) for tag in GENERIC_TAGS}})
    return pl.DataFrame(rows, schema={"observation_id": pl.Utf8, **{tag: pl.Utf8 for tag in GENERIC_TAGS}})


def observation_tags(observations: pl.DataFrame) -> pl.DataFrame:
    """Generic tags from evidence `raw_tags` ("key=value;key=value"), the source production uses.

    Packs captured before `local_ref` joined JrUtil's audit keys have no stand
    numbers here; use `candidate_node_tags` on an osmium export for those.
    """
    pairs = (
        observations.select("observation_id", pl.col("raw_tags").fill_null("").str.split(";").alias("pair"))
        .explode("pair", empty_as_null=True)
        .filter(pl.col("pair").str.contains("="))
        .with_columns(pl.col("pair").str.splitn("=", 2).struct.rename_fields(["key", "value"]).alias("kv"))
        .unnest("kv")
        .filter(pl.col("key").is_in(list(GENERIC_TAGS)))
        .unique(["observation_id", "key"], keep="first")
    )
    wide = pairs.pivot(on="key", index="observation_id", values="value") if pairs.height else pairs.select("observation_id")
    for tag in GENERIC_TAGS:
        if tag not in wide.columns:
            wide = wide.with_columns(pl.lit(None, dtype=pl.Utf8).alias(tag))
    ids = observations.select("observation_id").unique()
    return ids.join(wide.select("observation_id", *GENERIC_TAGS), on="observation_id", how="left")


@dataclass
class StationIndex:
    """Bus-station polygons (outer rings) and station points, in a local metric frame."""

    rings: list[np.ndarray]
    ring_boxes: np.ndarray
    points: np.ndarray

    @staticmethod
    def project(latitude: np.ndarray, longitude: np.ndarray) -> np.ndarray:
        # Equirectangular around 50 N is accurate to well under a metre at station scale.
        y = latitude * 110_540.0
        x = longitude * 111_320.0 * np.cos(np.radians(50.0))
        return np.column_stack([x, y])

    @classmethod
    def load(cls, path: Path) -> StationIndex:
        rings: list[np.ndarray] = []
        points: list[tuple[float, float]] = []
        for feature in _features(path):
            geometry = feature.get("geometry") or {}
            kind = geometry.get("type")
            coordinates = geometry.get("coordinates")
            if kind == "Point":
                points.append((coordinates[1], coordinates[0]))
            elif kind == "Polygon":
                polygons = [coordinates]
            elif kind == "MultiPolygon":
                polygons = coordinates
            else:
                continue
            if kind in ("Polygon", "MultiPolygon"):
                for polygon in polygons:
                    outer = np.asarray(polygon[0], dtype=float)
                    rings.append(cls.project(outer[:, 1], outer[:, 0]))
        boxes = (
            np.array([[ring[:, 0].min(), ring[:, 1].min(), ring[:, 0].max(), ring[:, 1].max()] for ring in rings])
            if rings
            else np.zeros((0, 4))
        )
        projected_points = (
            cls.project(np.array([p[0] for p in points]), np.array([p[1] for p in points]))
            if points
            else np.zeros((0, 2))
        )
        return cls(rings, boxes, projected_points)

    def membership(self, latitude: np.ndarray, longitude: np.ndarray) -> np.ndarray:
        """True where a point lies within (buffered) a station polygon or near a station node."""
        query = self.project(np.asarray(latitude, dtype=float), np.asarray(longitude, dtype=float))
        result = np.zeros(len(query), dtype=bool)
        buffer = STATION_POLYGON_BUFFER_METRES
        for ring, box in zip(self.rings, self.ring_boxes):
            near = (
                (query[:, 0] >= box[0] - buffer)
                & (query[:, 0] <= box[2] + buffer)
                & (query[:, 1] >= box[1] - buffer)
                & (query[:, 1] <= box[3] + buffer)
                & ~result
            )
            if not near.any():
                continue
            indices = np.nonzero(near)[0]
            candidates = query[indices]
            inside = _points_in_ring(candidates, ring) | (_distance_to_ring(candidates, ring) <= buffer)
            result[indices[inside]] = True
        if len(self.points):
            for index in np.nonzero(~result)[0]:
                distances = np.hypot(*(self.points - query[index]).T)
                if distances.min() <= STATION_POINT_RADIUS_METRES:
                    result[index] = True
        return result


def _points_in_ring(points: np.ndarray, ring: np.ndarray) -> np.ndarray:
    x, y = points[:, 0][:, None], points[:, 1][:, None]
    x1, y1 = ring[:-1, 0][None, :], ring[:-1, 1][None, :]
    x2, y2 = ring[1:, 0][None, :], ring[1:, 1][None, :]
    crosses = ((y1 > y) != (y2 > y)) & (x < (x2 - x1) * (y - y1) / np.where(y2 == y1, 1e-12, y2 - y1) + x1)
    return (crosses.sum(axis=1) % 2) == 1


def _distance_to_ring(points: np.ndarray, ring: np.ndarray) -> np.ndarray:
    start, end = ring[:-1], ring[1:]
    segment = end - start
    length = np.maximum((segment**2).sum(axis=1), 1e-12)
    relative = points[:, None, :] - start[None, :, :]
    t = np.clip((relative * segment[None, :, :]).sum(axis=2) / length[None, :], 0.0, 1.0)
    closest = start[None, :, :] + t[:, :, None] * segment[None, :, :]
    return np.hypot(*(points[:, None, :] - closest).transpose(2, 0, 1)).min(axis=1)
