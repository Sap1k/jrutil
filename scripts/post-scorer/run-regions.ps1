<#
Reproducible data pipeline for the learned stop-post scorer.

Per region: region capture (with context-call export) -> evaluator feature export ->
traffic weights -> [regional GTFS overlay + labels] -> candidate table.
Then: cross-validation across labelled regions, a pooled model, and evaluation of
unlabelled regions.

Prerequisites
  * A national JDF build with post candidates, e.g.
      uv run obehy-national-jdf build --capture-post-inference-evidence --keep-work --output <Build>
  * A fresh regional GTFS (+ descriptor) for each labelled region; the overlay rejects
    snapshots more than 7 days apart from the JDF build.

OSM tags come from the evidence observations' raw_tags (as in production). Packs captured
before `local_ref` joined JrUtil's audit keys can pass -OsmCandidates (osmium geojsonseq export).
Every step skips when its output already exists, so the script can be re-run after a failure.
#>
param(
    [Parameter(Mandatory)] [string] $Build,
    [Parameter(Mandatory)] [string] $Work,
    # name -> @{ Box; Exclude; SourceId; Gtfs; Descriptor; Policy }. Regions without SourceId are evaluation-only.
    [Parameter(Mandatory)] [hashtable] $Regions,
    [string] $OsmCandidates = "",
    [string] $Tool = (Join-Path $PSScriptRoot "..\..\jrutil-multitool\bin\Release\net10.0\jrutil-multitool.exe"),
    [string] $TransportModeRules = (Join-Path $PSScriptRoot "..\..\..\repo\src\obehy\data\jdf_transport_mode_rules.csv"),
    [int] $GvdYear = 2026,
    [double] $TargetPrecision = 0.9
)
$ErrorActionPreference = "Stop"

$merged = Join-Path $Build "derived\merged-jdf.zip"
$descriptor = Join-Path $Build "derived\snapshot-descriptor.json"
$manifest = Get-Content (Join-Path $Build "run-manifest.json") -Raw | ConvertFrom-Json
$routing = $manifest.osm_jdf_routing_extract.path
if (-not $routing) { throw "The build has no routing extract; rebuild with --estimated-posts or --capture-post-inference-evidence" }
$version = "post-scorer+" + $manifest.jrutil.commit
New-Item -ItemType Directory -Force $Work | Out-Null

function Invoke-Tool([string] $Name, [string[]] $Arguments) {
    Write-Host "== $Name $(Get-Date -Format T)"
    & $Tool @Arguments "--logfile=$(Join-Path $Work "$Name.log")"
    if ($LASTEXITCODE -ne 0) { throw "$Name failed with exit code $LASTEXITCODE" }
}

function Invoke-Python([string] $Name, [string[]] $Arguments) {
    Write-Host "== $Name $(Get-Date -Format T)"
    Push-Location $PSScriptRoot
    try {
        uv run python @Arguments
        if ($LASTEXITCODE -ne 0) { throw "$Name failed with exit code $LASTEXITCODE" }
    } finally { Pop-Location }
}

$common = @(
    "--international-route-policy=regional-adjacent",
    "--transport-mode-rules=$TransportModeRules",
    "--snapshot-descriptor=$descriptor",
    "--converter-version=$version"
)

# Base bundle (no posts): overlay reference package, trip identities and calendars for weights.
$base = Join-Path $Work "base-bundle"
if (-not (Test-Path $base)) {
    Invoke-Tool "base-bundle" (@("jdf-to-bundle") + $common + @("--no-estimated-posts", $merged, $base))
}

$labelled = @()
foreach ($name in $Regions.Keys) {
    $region = $Regions[$name]
    $evidence = Join-Path $Work "$name-evidence"
    $calls = Join-Path $Work "$name-context-calls.parquet"
    if (-not (Test-Path $evidence)) {
        $arguments = @("jdf-to-bundle") + $common + @(
            "--routing-osm-pbf=$routing", "--capture-post-inference-evidence=$evidence",
            "--post-inference-evidence-only", "--capture-stop-region=$($region.Box)",
            "--export-post-context-calls=$calls")
        # Training regions exclude their own catalogue so labels cannot leak into candidates.
        if ($region.Exclude) { $arguments += "--capture-exclude-source=$($region.Exclude)" }
        Invoke-Tool "$name-capture" ($arguments + @($merged, (Join-Path $Work "$name-unused")))
    }
    $exported = Join-Path $Work "$name-features-export"
    if (-not (Test-Path $exported)) {
        Invoke-Tool "$name-export" @("jdf-export-post-features", "--evidence=$evidence", "--output=$exported")
    }
    $weights = Join-Path $Work "$name-context-weights.parquet"
    if (-not (Test-Path $weights)) {
        Invoke-Python "$name-weights" @("-m", "post_scorer.weights", "--context-calls", $calls,
            "--base-gtfs", (Join-Path $base "gtfs.zip"), "--output", $weights)
    }
    $labelArguments = @()
    if ($region.SourceId) {
        $diagnostics = Join-Path $Work "$name-overlay-diagnostics"
        if (-not (Test-Path (Join-Path $diagnostics "traces\source_to_output_calls.csv"))) {
            # The production package has no call mappings; the diagnostics traces do.
            Invoke-Tool "$name-overlay" @(
                "regional-gtfs-overlay", "--policy=$($region.Policy)", "--gvd-year=$GvdYear",
                "--source=$($region.SourceId)=$($region.Gtfs)", "--source-descriptor=$($region.SourceId)=$($region.Descriptor)",
                "--diagnostics-out=$diagnostics", "--diagnostic-traces", $base, (Join-Path $Work "$name-overlay"))
        }
        $labels = Join-Path $Work "$name-labels"
        if (-not (Test-Path $labels)) {
            Invoke-Python "$name-labels" @("-m", "post_scorer.labels", "--evidence", $evidence,
                "--context-calls", $calls, "--mappings", (Join-Path $diagnostics "traces"),
                "--base-gtfs", (Join-Path $base "gtfs.zip"), "--source-gtfs", $region.Gtfs,
                "--source-id", $region.SourceId, "--output", $labels)
        }
        $labelArguments = @("--labels", $labels)
    }
    $candidates = Join-Path $Work "$name-candidates.parquet"
    if (-not (Test-Path $candidates)) {
        $arguments = @("-m", "post_scorer.features", "--evidence", $evidence, "--exported", $exported,
                       "--output", $candidates) + $labelArguments
        if ($OsmCandidates) { $arguments += @("--osm-candidates", $OsmCandidates) }
        Invoke-Python "$name-candidates" $arguments
    }
    if ($region.SourceId) { $labelled += "$name=$candidates,$weights" }
}

$regionArguments = @(); foreach ($value in $labelled) { $regionArguments += @("--region", $value) }
if ($labelled.Count -gt 1 -and -not (Test-Path (Join-Path $Work "crossval.csv"))) {
    Invoke-Python "crossval" (@("-m", "post_scorer.model", "crossval", "--target-precision", "$TargetPrecision",
        "--output", (Join-Path $Work "crossval.csv")) + $regionArguments)
}

# Pooled production model over every labelled region (weights normalised per region; a
# stop captured by two regions is kept once, so stops never straddle the split).
$model = Join-Path $Work "model"
if ($labelled.Count -gt 0 -and -not (Test-Path $model)) {
    Invoke-Python "train" (@("-m", "post_scorer.model", "train", "--target-precision", "$TargetPrecision",
        "--output", $model) + $regionArguments)
}
foreach ($name in $Regions.Keys) {
    if ($Regions[$name].SourceId) { continue }
    Invoke-Python "evaluate-$name" @("-m", "post_scorer.model", "evaluate",
        "--candidates", (Join-Path $Work "$name-candidates.parquet"),
        "--weights", (Join-Path $Work "$name-context-weights.parquet"),
        "--model", (Join-Path $model "model.json"),
        "--decisions-out", (Join-Path $Work "$name-decisions.parquet"))
}
