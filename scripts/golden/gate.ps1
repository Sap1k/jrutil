# Regression gate: semantic package equality plus the performance budget.
#
#   pwsh scripts/golden/gate.ps1 -Root E:/Git/obehy/work/refactor-golden -Baseline baseline -Candidate <label>
#       [-Expect scripts/golden/expect/<phase>.txt] [-ByteIdentical] [-TimeTolerance 0.05] [-MemoryTolerance 0.05]
#
# Both labels must have been produced by run.ps1. Performance is compared on
# median wall time and median peak private bytes per stage; use -Repeat 3 in
# run.ps1 for both labels before trusting the verdict.

param(
    [Parameter(Mandatory)] [string] $Root,
    [Parameter(Mandatory)] [string] $Baseline,
    [Parameter(Mandatory)] [string] $Candidate,
    [string] $Expect,
    [switch] $ByteIdentical,
    [switch] $SkipPerf,
    [double] $TimeTolerance = 0.05,
    [double] $MemoryTolerance = 0.05
)

$ErrorActionPreference = "Stop"
$Root = (Resolve-Path $Root).Path
$exe = Join-Path $Root "bin/$Candidate/jrutil-multitool.exe"
$failed = $false

foreach ($package in @("bundle", "bundle-posts", "overlay", "czptt")) {
    $left = Join-Path $Root "out/$Baseline/$package"
    $right = Join-Path $Root "out/$Candidate/$package"
    if (-not (Test-Path $left) -or -not (Test-Path $right)) {
        Write-Warning "Skipping ${package}: missing in baseline or candidate"
        continue
    }
    $mode = if ($ByteIdentical) { @("--byte-identical") } else { @("--semantic") + $(if ($Expect) { @("--expect=$((Resolve-Path $Expect).Path)") } else { @() }) }
    Write-Host "== $package ($($mode[0]))"
    & $exe compare-packages @mode $left $right 2>&1 | ForEach-Object { "   $_" }
    if ($LASTEXITCODE -ne 0) { $failed = $true; Write-Host "   FAIL: $package differs" -ForegroundColor Red }
}

if (-not $SkipPerf) {
    $base = (Get-Content -Raw (Join-Path $Root "perf/$Baseline.json") | ConvertFrom-Json -AsHashtable).stages
    $cand = (Get-Content -Raw (Join-Path $Root "perf/$Candidate.json") | ConvertFrom-Json -AsHashtable).stages
    Write-Host ""
    Write-Host ("{0,-14} {1,10} {2,10} {3,8}   {4,10} {5,10} {6,8}" -f "stage", "base s", "cand s", "delta", "base MiB", "cand MiB", "delta")
    foreach ($stage in $cand.Keys) {
        if (-not $base.ContainsKey($stage)) { continue }
        $b, $c = $base[$stage], $cand[$stage]
        $timeDelta = ($c.median_wall_seconds - $b.median_wall_seconds) / $b.median_wall_seconds
        $memoryDelta = ($c.median_peak_private_bytes - $b.median_peak_private_bytes) / $b.median_peak_private_bytes
        $verdict = ""
        if ($timeDelta -gt $TimeTolerance) { $verdict += " TIME" }
        if ($memoryDelta -gt $MemoryTolerance) { $verdict += " MEMORY" }
        if ($c.runs | Where-Object { $_.error_log_lines -gt 0 }) { $verdict += " ERRORS-LOGGED" }
        if ($verdict) { $failed = $true }
        Write-Host ("{0,-14} {1,10:N1} {2,10:N1} {3,7:P1}   {4,10:N0} {5,10:N0} {6,7:P1} {7}" -f $stage,
            $b.median_wall_seconds, $c.median_wall_seconds, $timeDelta,
            ($b.median_peak_private_bytes / 1MB), ($c.median_peak_private_bytes / 1MB), $memoryDelta, $verdict)
    }
}

if ($failed) { Write-Host "GATE FAILED" -ForegroundColor Red; exit 1 }
Write-Host "GATE PASSED" -ForegroundColor Green
