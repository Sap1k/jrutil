# Run the bounded golden pipeline with the published (ServerGC) multitool and
# record wall time and peak private memory per stage.
#
#   pwsh scripts/golden/run.ps1 -Root E:/Git/obehy/work/refactor-golden [-Label NAME] [-Repeat 3] [-Stages fix,merge,...]
#
# Outputs of the first repetition go to <Root>/out/<Label>; later repetitions are
# measured in scratch and discarded. Metrics go to <Root>/perf/<Label>.json.
# Never point this at the full national feed.

param(
    [Parameter(Mandatory)] [string] $Root,
    [string] $Label,
    [int] $Repeat = 1,
    [string[]] $Stages = @("fix", "merge", "bundle", "overlay", "fix-posts", "merge-posts", "bundle-posts", "czptt"),
    [string] $Jobs = "8",
    [string] $MemoryBudget = "4GiB",
    [string] $OsmPbf = "E:/Git/obehy/work/regional-merged.osm.pbf",
    [switch] $NoPublish
)

$ErrorActionPreference = "Stop"
# pwsh -File passes "a,b" as one string.
$Stages = @($Stages | ForEach-Object { $_ -split "," } | Where-Object { $_ })
$repo = (Resolve-Path "$PSScriptRoot/../..").Path
$Root = (Resolve-Path $Root).Path
$inputs = Join-Path $Root "inputs"

if (-not $Label) {
    $commit = (git -C $repo rev-parse --short=12 HEAD).Trim()
    $dirty = git -C $repo status --porcelain --untracked-files=no
    $Label = if ($dirty) { "$commit-dirty" } else { $commit }
}

$bin = Join-Path $Root "bin/$Label"
$exe = Join-Path $bin "jrutil-multitool.exe"
if (-not $NoPublish -or -not (Test-Path $exe)) {
    if (Test-Path $bin) { Remove-Item -Recurse -Force $bin }
    dotnet publish (Join-Path $repo "jrutil-multitool/jrutil-multitool.fsproj") -c Release -o $bin --nologo -v quiet
    if ($LASTEXITCODE -ne 0) { throw "dotnet publish failed" }
}

$out = Join-Path $Root "out/$Label"
$scratch = Join-Path $Root "scratch"
$perfDir = Join-Path $Root "perf"
New-Item -ItemType Directory -Force $out, $perfDir | Out-Null

function Invoke-Measured([string] $Name, [string[]] $Arguments, [string] $LogDir) {
    New-Item -ItemType Directory -Force $LogDir | Out-Null
    $stdout = Join-Path $LogDir "$Name.stdout.log"
    $stderr = Join-Path $LogDir "$Name.stderr.log"
    $quoted = $Arguments | ForEach-Object { if ($_ -match '[\s"]') { '"' + ($_ -replace '"', '\"') + '"' } else { $_ } }
    $watch = [Diagnostics.Stopwatch]::StartNew()
    $process = Start-Process -FilePath $exe -ArgumentList $quoted -NoNewWindow -PassThru `
        -RedirectStandardOutput $stdout -RedirectStandardError $stderr
    $null = $process.Handle  # keep the handle so ExitCode survives process exit
    $peakPrivate = 0L
    while (-not $process.HasExited) {
        try {
            $process.Refresh()
            if ($process.PrivateMemorySize64 -gt $peakPrivate) { $peakPrivate = $process.PrivateMemorySize64 }
        } catch { }
        Start-Sleep -Milliseconds 100
    }
    $process.WaitForExit()
    $watch.Stop()
    $peakWorkingSet = try { $process.PeakWorkingSet64 } catch { 0L }
    $errors = @(Select-String -Path $stdout, $stderr -Pattern '\[\d\d:\d\d:\d\d (ERR|FTL)\]' -ErrorAction SilentlyContinue).Count
    $warnings = @(Select-String -Path $stdout, $stderr -Pattern '\[\d\d:\d\d:\d\d WRN\]' -ErrorAction SilentlyContinue).Count
    if ($process.ExitCode -ne 0) { throw "$Name failed with exit code $($process.ExitCode); see $stdout" }
    [ordered]@{
        wall_seconds = [math]::Round($watch.Elapsed.TotalSeconds, 2)
        peak_private_bytes = $peakPrivate
        peak_working_set_bytes = $peakWorkingSet
        error_log_lines = $errors
        warning_log_lines = $warnings
    }
}

# Deterministic ZIP of a directory (sorted entries, fixed timestamp), matching
# what the orchestrator hands to jdf-to-bundle.
function New-DeterministicZip([string] $Source, [string] $Destination) {
    Add-Type -AssemblyName System.IO.Compression
    if (Test-Path $Destination) { Remove-Item -Force $Destination }
    $stream = [IO.File]::Open($Destination, [IO.FileMode]::CreateNew)
    $archive = [IO.Compression.ZipArchive]::new($stream, [IO.Compression.ZipArchiveMode]::Create)
    try {
        $files = Get-ChildItem -Recurse -File $Source | ForEach-Object {
            [pscustomobject]@{ Path = $_.FullName; Name = [IO.Path]::GetRelativePath($Source, $_.FullName).Replace('\', '/') }
        } | Sort-Object { $_.Name } -Culture ([Globalization.CultureInfo]::InvariantCulture)
        foreach ($file in $files) {
            $entry = $archive.CreateEntry($file.Name, [IO.Compression.CompressionLevel]::Optimal)
            $entry.LastWriteTime = [DateTimeOffset]::new(1980, 1, 1, 0, 0, 0, [TimeSpan]::Zero)
            $target = $entry.Open()
            try { $bytes = [IO.File]::ReadAllBytes($file.Path); $target.Write($bytes, 0, $bytes.Length) } finally { $target.Dispose() }
        }
    } finally { $archive.Dispose(); $stream.Dispose() }
}

function Write-Descriptor([string] $Payload, [string] $Destination) {
    $hash = (Get-FileHash -Algorithm SHA256 $Payload).Hash.ToLowerInvariant()
    [ordered]@{
        schema_version = 1
        source_id = "national-jdf-vld-drahy"
        retrieved_at = "2026-09-28T12:00:00+00:00"
        retrieval_method = "derived-from-https-and-configured-snapshots"
        source_uri = "obehy:derived:national-jdf-vld-drahy"
        licence = "CIS JR public data; OSM ODbL; external geodata source-specific"
        payload_kind = "zip"
        payload_sha256 = $hash
        payload_bytes = (Get-Item $Payload).Length
    } | ConvertTo-Json | Set-Content -Encoding utf8NoBOM $Destination
}

$common = @("--jobs=$Jobs", "--memory-budget=$MemoryBudget")
$config = Join-Path $inputs "config"
$czptt = Join-Path $inputs "czptt"
$overlayInputs = Join-Path $inputs "overlay"
$routingPbf = Join-Path $inputs "osm/jdf-transit-routing-demand.osm.pbf"

function Get-FixArguments([string] $Target, [bool] $Posts) {
    @("fix-jdf", "--strict", "--batch-output=zip", "--international-route-policy=regional-adjacent") +
    $(if ($Posts) { @() } else { @("--no-estimated-posts") }) +
    @("--ext-geodata=$inputs/geodata/other", "--cz-pbf=$inputs/osm/jdf-post-candidates.osm.pbf") + $common + @("$inputs/jdf/batches", $Target)
}

function Get-BundleArguments([string] $Target, [string] $Merged, [string[]] $PostOptions) {
    @("jdf-to-bundle", "--international-route-policy=regional-adjacent", "--transport-mode-rules=$config/jdf_transport_mode_rules.csv",
      "--snapshot-descriptor=$out/$Merged-descriptor.json", "--converter-version=golden", "--gvd-year=2026") +
    $PostOptions + @("--diagnostics-out=$Target-diagnostics") + $common + @("$out/$Merged.zip", $Target)
}

# Stages form two JDF chains (fix -> merge -> bundle -> overlay, and
# fix-posts -> merge-posts -> bundle-posts) plus the independent czptt stage.
function Get-StageArguments([string] $Stage, [string] $Target) {
    switch ($Stage) {
        "fix" { Get-FixArguments $Target $false }
        "fix-posts" { Get-FixArguments $Target $true }
        "merge" { @("merge-jdf", "--strict", "--gvd-year=2026", "--reference-date=2026-09-16") + $common + @($Target, "$out/fix") }
        "merge-posts" { @("merge-jdf", "--strict", "--gvd-year=2026", "--reference-date=2026-09-16") + $common + @($Target, "$out/fix-posts") }
        "bundle" { Get-BundleArguments $Target "merge" @("--no-estimated-posts") }
        "bundle-posts" { Get-BundleArguments $Target "merge-posts" @("--routing-osm-pbf=$routingPbf") }
        "overlay" { @("regional-gtfs-overlay", "--policy=$config/pid-ids-jmk-production-v1.json", "--gvd-year=2026", "--converter-version=golden",
                      "--source=pid-gtfs=$overlayInputs/pid-gtfs.zip", "--source-descriptor=pid-gtfs=$overlayInputs/pid-gtfs-descriptor.json",
                      "--source=ids-jmk-gtfs=$overlayInputs/ids-jmk-gtfs.zip", "--source-descriptor=ids-jmk-gtfs=$overlayInputs/ids-jmk-gtfs-descriptor.json",
                      "--diagnostics-out=$Target-diagnostics") + $common + @("$out/bundle", $Target) }
        "czptt" { @("czptt-to-bundle", "--catalog-snapshot=$czptt/sources/kadr/catalog.json", "--operational-points=sidecar",
                    "--sr70=$czptt/sources/sr70/SR70.csv", "--sr70-name20=$czptt/sources/sr70/SR70_Nazev20.csv",
                    "--osm-pbf=$inputs/osm/railway-locations.osm.pbf", "--osm-aliases=$config/czptt_osm_aliases.json",
                    "--diagnostics-out=$Target-diagnostics") + $common + @("$czptt/derived/messages.zip", $Target) }
        default { throw "Unknown stage $Stage" }
    }
}

$perfPath = Join-Path $perfDir "$Label.json"
$perf = [ordered]@{ label = $Label; jobs = $Jobs; memory_budget = $MemoryBudget; stages = [ordered]@{} }
if (Test-Path $perfPath) {
    # Keep stages measured by earlier invocations for the same label.
    $previous = Get-Content -Raw $perfPath | ConvertFrom-Json -AsHashtable
    foreach ($key in $previous.stages.Keys) { $perf.stages[$key] = $previous.stages[$key] }
}

foreach ($stage in $Stages) {
    if ($stage -eq "bundle-posts" -and -not (Test-Path $routingPbf)) {
        Write-Warning "Skipping bundle-posts: $routingPbf is missing (run prepare-routing first)"
        continue
    }
    $runs = @()
    for ($run = 1; $run -le $Repeat; $run++) {
        $target = if ($run -eq 1) { Join-Path $out $stage } else { Join-Path $scratch "$stage-$run" }
        foreach ($path in @($target, "$target-diagnostics")) { if (Test-Path $path) { Remove-Item -Recurse -Force $path } }
        Write-Host "[$Label] $stage run $run/$Repeat"
        $runs += Invoke-Measured $stage (Get-StageArguments $stage $target) (Join-Path $out "logs/run$run")
        if ($run -gt 1) { foreach ($path in @($target, "$target-diagnostics")) { if (Test-Path $path) { Remove-Item -Recurse -Force $path } } }
    }
    if ($stage -like "merge*") {
        New-DeterministicZip (Join-Path $out $stage) (Join-Path $out "$stage.zip")
        Write-Descriptor (Join-Path $out "$stage.zip") (Join-Path $out "$stage-descriptor.json")
    }
    $sortedWall = @($runs | ForEach-Object { $_.wall_seconds } | Sort-Object)
    $sortedPrivate = @($runs | ForEach-Object { $_.peak_private_bytes } | Sort-Object)
    $perf.stages[$stage] = [ordered]@{
        repeat = $Repeat
        median_wall_seconds = $sortedWall[[int][math]::Floor(($sortedWall.Count - 1) / 2)]
        median_peak_private_bytes = $sortedPrivate[[int][math]::Floor(($sortedPrivate.Count - 1) / 2)]
        runs = $runs
    }
    $perf | ConvertTo-Json -Depth 6 | Set-Content -Encoding utf8NoBOM $perfPath
}

if (Test-Path $scratch) { Remove-Item -Recurse -Force $scratch }
Write-Host "Metrics: $perfPath"
Write-Host "Outputs: $out"
