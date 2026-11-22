[CmdletBinding()]
param([Parameter(Mandatory)][string]$RunDirectory)
$ErrorActionPreference = 'Stop'
$run = (Resolve-Path -LiteralPath $RunDirectory).Path
$drive = Join-Path $run 'drive'
$plan = Get-Content -LiteralPath (Join-Path $run 'plan.json') -Raw | ConvertFrom-Json
if ($plan.formatVersion -ne 2) { throw 'External timing plan required.' }
foreach ($case in $plan.benchmarks) {
    foreach ($suffix in @('s','t')) {
        if (-not (Test-Path -LiteralPath (Join-Path $drive ($case.tag+$suffix)))) { throw "Missing $($case.tag)$suffix" }
    }
    $report = Get-Content -LiteralPath (Join-Path $drive ($case.tag+'_report')) -Raw
    if ($report -notmatch '0 errors' -or $report -notmatch 'completed successfully') { throw "Turbo failed: $($case.name)" }
}
foreach ($file in @('sb-verification.json','native-verification.json')) {
    $rows = @(Get-Content -LiteralPath (Join-Path $run $file) -Raw | ConvertFrom-Json)
    if ($rows.Count -ne $plan.benchmarks.Count) { throw "Incomplete verification: $file" }
    foreach ($row in $rows) {
        if (($file -eq 'sb-verification.json' -and (-not $row.OrdinaryExecutableIdentical -or -not $row.OriginalSourceUnchanged)) -or
            ($file -eq 'native-verification.json' -and (-not $row.CompatibilityExecutableIdentical -or $row.NativeIntegerOperations -le 0))) { throw "Failed verification: $file" }
    }
}
if ((Get-Content -LiteralPath (Join-Path $run 'benchmark.QCF')) -notcontains 'Speed=QL') { throw 'Timing requires Speed=QL.' }
Copy-Item -LiteralPath (Join-Path $drive 'run_boot') -Destination (Join-Path $drive 'boot')
[IO.File]::WriteAllText((Join-Path $drive 'build_done'),"Verified ordinary SB and Turbo FAST executables.`n")
Write-Output "Open $run\benchmark.QCF in Q-emuLator to run the external timing trials."
