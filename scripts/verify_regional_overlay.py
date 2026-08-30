"""Compare a pinned overlay baseline, including exact report-row multiplicities.

Usage: python scripts/verify_regional_overlay.py BASELINE OUTPUT
Only report, mapping and provenance row ordering and commit metadata may differ.
Both manifests' checksums are independently verified. SQLite is used only by
this offline verifier to keep comparison memory bounded, never by the overlay.
"""
from contextlib import closing
import csv
import hashlib
import json
import sqlite3
import sys
import tempfile
from pathlib import Path


def manifest(root):
    data = json.loads((root / "manifest.json").read_text(encoding="utf8"))
    for entry in data["files"]:
        path = root / entry["path"]
        with path.open("rb") as stream:
            digest = hashlib.file_digest(stream, "sha256").hexdigest()
        assert digest == entry["sha256"], f"Checksum mismatch: {path}"
        assert path.stat().st_size == entry["bytes"], f"Size mismatch: {path}"
    return data


def compare_rows(left, right):
    with tempfile.TemporaryDirectory(prefix="overlay-compare-") as scratch:
        with closing(sqlite3.connect(str(Path(scratch) / "rows.db"))) as db:
            db.execute("PRAGMA cache_size=-16384")
            db.execute("CREATE TABLE rows (value TEXT PRIMARY KEY, count INTEGER) WITHOUT ROWID")
            headers = []
            for path, sign in [(left, 1), (right, -1)]:
                with path.open(encoding="utf-8-sig", newline="") as stream:
                    reader = csv.reader(stream)
                    headers.append(next(reader))
                    db.executemany(
                        "INSERT INTO rows VALUES (?, ?) ON CONFLICT(value) DO UPDATE SET count=count+excluded.count",
                        ((json.dumps(row, ensure_ascii=False), sign) for row in reader),
                    )
                db.commit()
            assert headers[0] == headers[1], f"Columns changed: {right}"
            difference = db.execute("SELECT value, count FROM rows WHERE count<>0 LIMIT 5").fetchall()
            assert not difference, f"Row content changed: {right}: {difference}"


def main():
    left, right = map(Path, sys.argv[1:])
    old, new = manifest(left), manifest(right)
    old_files = {entry["path"]: entry for entry in old.pop("files")}
    new_files = {entry["path"]: entry for entry in new.pop("files")}
    old.pop("jrutil_commit", None)
    new.pop("jrutil_commit", None)
    assert old == new, "Manifest semantics changed"
    assert old_files.keys() == new_files.keys(), "Bundle file set changed"
    reordered = []
    for path, entry in old_files.items():
        if entry == new_files[path]:
            continue
        assert path.startswith(("reports/", "mappings/", "provenance/")), f"Transit/evidence bytes changed: {path}"
        compare_rows(left / path, right / path)
        reordered.append(path)
        print("Row order only:", path, flush=True)
    print(json.dumps({"verified_files": len(old_files), "reordered": reordered, "content_changes": 0}))


if __name__ == "__main__":
    main()
