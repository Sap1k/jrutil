// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

open Serilog

open JrUtil.CliArgs
open JrUtil.Logging
open JrUtil.Multitool.Context
open JrUtil.Multitool.Commands

let docstring = (fun (s: string) -> s.Trim()) """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil-multitool.exe jdf-to-bundle [options] [--gvd-year=YEAR] --snapshot-descriptor=FILE --converter-version=VALUE <JDF-input> <bundle-out-dir>
    jrutil-multitool.exe jdf-export-post-features [options] [--policy=FILE] --evidence=DIR --output=DIR
    jrutil-multitool.exe czptt-to-bundle [options] --catalog-snapshot=FILE <CzPtt-in-file> <bundle-out-dir>
    jrutil-multitool.exe regional-gtfs-overlay [options] --policy=FILE --gvd-year=YEAR --converter-version=VALUE (--source=BINDING --source-descriptor=BINDING)... <base-bundle> <overlay-bundle-out>
    jrutil-multitool.exe validate-package <package-dir>
    jrutil-multitool.exe compare-packages (--byte-identical | --semantic) [--expect=FILE] <left-package> <right-package>
    jrutil-multitool.exe fix-jdf [options] <JDF-in-dir> <JDF-out-dir>
    jrutil-multitool.exe merge-jdf [options] --gvd-year=YEAR --reference-date=DATE <JDF-out-dir> <JDF-in-dir>...
    jrutil-multitool.exe --help

Options:
    -g --ext-geodata=PATH        CSV file or directory with stop positions
    -o --cz-pbf=PATH             OSM data for Czech Republic
    -l --logfile=FILE            Logfile
    -s --strict                  Fail instead of skipping a malformed batch
    --snapshot-descriptor=FILE    Retrieval provenance and input checksum JSON
    --converter-version=VALUE     Exact JrUtil fork version or commit for provenance
    --international-route-policy=VALUE  keep-all (default) or regional-adjacent
    --transport-mode-rules=FILE  Reviewed JDF effective transport-mode rule CSV
    --no-estimated-posts         Disable candidate-based internal post inference
    --routing-osm-pbf=FILE       Osmium demand-clipped road/tram PBF for routed inference
    --diagnostic-post-labels     Emit inferred O*/O-direction labels in GTFS platform_code
    --post-review-stops=FILE     Stop IDs/names for routed-inference review GeoJSON
    --capture-post-inference-evidence=DIR  Persist reusable policy-neutral routed evidence
    --post-inference-evidence-only         Stop after writing the evidence directory
    --capture-stop-region=BOX              Capture only stops in MINLON,MINLAT,MAXLON,MAXLAT (review/training packs)
    --capture-exclude-source=PREFIXES      Drop observations whose source ID starts with a comma-separated prefix
    --export-post-context-calls=FILE       Also write per-call context IDs (Parquet) for label joins
    --post-inference-evidence=DIR          Reuse captured routed evidence without loading a graph
    --post-inference-policy=FILE           Versioned routed-inference policy JSON
    --no-post-inference-scores             Skip diagnostic score rows for publication-only bundles
    --evidence=DIR                         Evidence directory for policy replay
    --policy=FILE                          Policy JSON for policy replay
    --gvd-year=YEAR                       GVD year: overlay window, merge-jdf validity bound, jdf-to-bundle manifest
    --reference-date=DATE                 merge-jdf: drop timetables expired before YYYY-MM-DD
    --source=BINDING                      Overlay source binding SOURCE_ID=GTFS.zip
    --source-descriptor=BINDING           Source checksum binding SOURCE_ID=descriptor.json
    --diagnostics-out=DIR                 Optional separate detailed diagnostics artifact
    --diagnostic-traces                   Include large compiler trace relations in diagnostics
    --byte-identical                      Require every production byte to match
    --semantic                            Compare relations by key, GTFS as row multisets, JSON canonically
    --expect=FILE                         Allow-list of intended semantic differences (`<kind> <glob>` lines)
    --output=DIR                           Replay report output directory
    -j --jobs=VALUE             Worker count or auto (default: auto)
    --memory-budget=VALUE       RAM budget such as 10GiB or auto (default: auto)
    --batch-output=VALUE        fix-jdf output: directory or zip (default: directory)
    --catalog-snapshot=FILE     Offline KADR catalog snapshot JSON for CZPTT
    --operational-points=VALUE  CZPTT internal points: gtfs (default) or sidecar
    --sr70=FILE                 SR70 CSV snapshot for CZPTT point names and coordinates
    --osm-pbf=FILE              Shared regional OSM PBF for CZPTT coordinate gaps
    --osm-aliases=FILE          Reviewed CZPTT identity-to-OSM-object aliases
    --progress-events           Emit versioned JRUTIL_PROGRESS JSON lines
"""

/// Command name, the error logged when it throws (None lets the exception
/// escape), and its implementation returning the process exit code.
let commands: (string * string option * (CommandContext -> int)) list = [
    "regional-gtfs-overlay", Some "Regional GTFS overlay failed", PackageCommands.overlay
    "validate-package", Some "Production package validation failed", PackageCommands.validate
    "compare-packages", Some "Package comparison failed", PackageCommands.compare
    "jdf-to-bundle", Some "JDF bundle conversion failed", ConvertCommands.jdfToBundle
    "jdf-export-post-features", Some "JDF post-inference feature export failed", ConvertCommands.jdfExportPostFeatures
    "czptt-to-bundle", Some "CZPTT bundle conversion failed", ConvertCommands.czpttToBundle
    "fix-jdf", None, JdfCommands.fixJdf
    "merge-jdf", None, JdfCommands.mergeJdf
]

[<EntryPoint>]
let main (args: string array) =
    withProcessedArgs docstring args (fun args ->
        setupLogging (optArgValue args "--logfile") ()
        let ctx = CommandContext(args)
        match commands |> List.tryFind (fun (name, _, _) -> argFlagSet args name) with
        | None ->
            printfn "%s" docstring
            0
        | Some (_, None, run) -> run ctx
        | Some (_, Some failure, run) ->
            try run ctx
            with error ->
                Log.Error(error, failure)
                1)
