"""Freeze bounded golden inputs for refactor regression and performance gates.

The selection is deterministic: the same source snapshots always yield the same
subset. Never point the gate at the full national feed.

Usage: python prepare.py <golden-root> --jdf-sources <dir> --czptt-run <dir>
           --pid <gtfs.zip> --pid-descriptor <json> --jmk <gtfs.zip> --jmk-descriptor <json>
"""

from __future__ import annotations

import argparse
import hashlib
import io
import json
import shutil
import sys
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path

# Prague (PID overlap) and South Moravia (IDS JMK overlap) line-number prefixes,
# each thinned to every third batch, plus a spread sample of the whole country.
JDF_REGION_PREFIXES = ("100", "72")
JDF_REGION_STRIDE = 3
JDF_SPREAD_STRIDE = 40
JDF_DRAHY_STRIDE = 10
CZPTT_TRAIN_MODULUS = 20


def _stable_bucket(value: str, modulus: int) -> int:
    return int.from_bytes(hashlib.sha256(value.encode("utf-8")).digest()[:8], "big") % modulus


def _first_line_number(batch: bytes) -> str:
    with zipfile.ZipFile(io.BytesIO(batch)) as archive:
        names = [name for name in archive.namelist() if name.lower().endswith("linky.txt")]
        if not names:
            return ""
        text = archive.read(names[0]).decode("cp1250", errors="replace").splitlines()
        return text[0].split(",")[0].strip('"') if text else ""


def select_jdf(sources: Path, destination: Path) -> dict[str, int]:
    destination.mkdir(parents=True, exist_ok=False)
    counts = {"vld": 0, "drahy": 0}
    for source_name, archive_name in (("vld", "JDF_VLD.zip"), ("drahy", "JDF_drahy.zip")):
        with zipfile.ZipFile(sources / archive_name) as archive:
            entries = sorted(
                (info for info in archive.infolist() if info.filename.lower().endswith(".zip")),
                key=lambda info: info.filename,
            )
            region_seen = 0
            for index, info in enumerate(entries):
                payload = archive.read(info)
                if source_name == "drahy":
                    keep = index % JDF_DRAHY_STRIDE == 0
                else:
                    line = _first_line_number(payload)
                    regional = line.startswith(JDF_REGION_PREFIXES)
                    keep = index % JDF_SPREAD_STRIDE == 0
                    if regional:
                        keep = keep or region_seen % JDF_REGION_STRIDE == 0
                        region_seen += 1
                if keep:
                    stem = Path(info.filename).stem
                    (destination / f"{source_name}-{stem}.zip").write_bytes(payload)
                    counts[source_name] += 1
    return counts


def _train_key(message: bytes) -> str | None:
    root = ET.fromstring(message)
    for identifiers in root.iter("PlannedTransportIdentifiers"):
        if identifiers.findtext("ObjectType") == "TR":
            return "|".join(
                identifiers.findtext(field) or ""
                for field in ("Company", "Core", "Variant", "TimetableYear")
            )
    return None


def select_czptt(run: Path, destination: Path) -> dict[str, int]:
    destination.mkdir(parents=True, exist_ok=False)
    kept = 0
    total = 0
    output = destination / "derived" / "messages.zip"
    output.parent.mkdir()
    with (
        zipfile.ZipFile(run / "derived" / "messages.zip") as source,
        zipfile.ZipFile(output, "w", zipfile.ZIP_DEFLATED) as target,
    ):
        for info in sorted(source.infolist(), key=lambda value: value.filename):
            total += 1
            payload = source.read(info)
            key = _train_key(payload)
            if key is not None and _stable_bucket(key, CZPTT_TRAIN_MODULUS) == 0:
                # Keep the original ordinal name: message order is semantic.
                target.writestr(info.filename, payload)
                kept += 1
    shutil.copytree(run / "sources" / "sr70", destination / "sources" / "sr70")
    shutil.copytree(run / "sources" / "kadr", destination / "sources" / "kadr")
    return {"messages_total": total, "messages_kept": kept}


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("root", type=Path)
    parser.add_argument("--jdf-sources", type=Path, required=True)
    parser.add_argument("--czptt-run", type=Path, required=True)
    parser.add_argument("--pid", type=Path, required=True)
    parser.add_argument("--pid-descriptor", type=Path, required=True)
    parser.add_argument("--jmk", type=Path, required=True)
    parser.add_argument("--jmk-descriptor", type=Path, required=True)
    args = parser.parse_args()
    inputs = args.root / "inputs"
    if inputs.exists():
        print(f"{inputs} already exists; golden inputs are immutable", file=sys.stderr)
        return 2
    summary: dict[str, object] = {"schema_version": 1}
    summary["jdf"] = select_jdf(args.jdf_sources, inputs / "jdf" / "batches")
    summary["czptt"] = select_czptt(args.czptt_run, inputs / "czptt")
    overlay = inputs / "overlay"
    overlay.mkdir(parents=True)
    for name, payload, descriptor in (
        ("pid-gtfs", args.pid, args.pid_descriptor),
        ("ids-jmk-gtfs", args.jmk, args.jmk_descriptor),
    ):
        shutil.copyfile(payload, overlay / f"{name}.zip")
        shutil.copyfile(descriptor, overlay / f"{name}-descriptor.json")
    (inputs / "inputs.json").write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
