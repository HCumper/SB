$ErrorActionPreference='Stop'
$taskRoot='C:\Source\codex-workspace\.scratch\turbo-native-template-loops'
$run=Join-Path $taskRoot 'qemulator'
$baseline='C:\Source\codex-workspace\.scratch\turbo-invariant-arrays\qemulator'


$plan=Get-Content -LiteralPath (Join-Path $run 'plan.json') -Raw | ConvertFrom-Json
$old=Get-Content -LiteralPath (Join-Path $baseline 'plan.json') -Raw | ConvertFrom-Json
$comparisons=@(foreach ($case in $plan.benchmarks) {
    $previous=@($old.benchmarks | Where-Object name -eq $case.name)
    if ($previous.Count -ne 1 -or $previous[0].sourceSha256 -ne $case.sourceSha256) { throw 'Baseline source mismatch' }
    $tag=$case.tag
    if ((Get-FileHash -LiteralPath (Join-Path $run "drive\${tag}_bas")).Hash -ne (Get-FileHash -LiteralPath (Join-Path $baseline "drive\${tag}_bas")).Hash) { throw 'Turbo input changed' }
    foreach ($suffix in @('t','_ir','_report')) { Copy-Item -LiteralPath (Join-Path $baseline "drive\$tag$suffix") -Destination (Join-Path $run "drive\$tag$suffix") -Force }
    $before=Join-Path $baseline "drive\${tag}s"
    $after=Join-Path $run "drive\${tag}s"
    $beforeHash=(Get-FileHash -LiteralPath $before).Hash
    $afterHash=(Get-FileHash -LiteralPath $after).Hash
    [pscustomobject]@{Benchmark=$case.name;Tag=$tag;SourceSha256=$case.sourceSha256;BeforeSha256=$beforeHash;AfterSha256=$afterHash;BeforeBytes=(Get-Item -LiteralPath $before).Length;AfterBytes=(Get-Item -LiteralPath $after).Length;Identical=$beforeHash -eq $afterHash}
})
$comparisons | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $run 'binary-comparison.json')
[ordered]@{Baseline=$baseline;BaselineCompilerSha256=(Get-FileHash -LiteralPath (Join-Path $taskRoot 'baseline-SB.dll')).Hash;SameCanonicalSources=$true;SameTurboInputs=$true;TurboReusedFromVerifiedBuild=$true} | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $run 'baseline-provenance.json')
dotnet fsi --exec Benchmarks\QemuLator\Inspect-Turbo.fsx $run
if ($LASTEXITCODE -ne 0) { throw 'Turbo native verification failed' }
$subset=Join-Path $taskRoot 'qemulator-timings'
if (Test-Path -LiteralPath (Join-Path $subset 'drive\done')) { throw 'Refusing to overwrite completed timings' }
New-Item -ItemType Directory -Path $subset -Force | Out-Null
Copy-Item -Path (Join-Path $run '*') -Destination $subset -Recurse -Force
$changedTags=@($comparisons | Where-Object { -not $_.Identical } | ForEach-Object Tag)
$plan.benchmarks=@($plan.benchmarks | Where-Object { $_.tag -in $changedTags })
$plan.expectedResults=$plan.benchmarks.Count*2*($plan.trials+$plan.warmups)
$plan | ConvertTo-Json -Depth 10 | Set-Content -LiteralPath (Join-Path $subset 'plan.json')
$runner=Get-Content -LiteralPath (Join-Path $run 'drive\runner_bas') | Where-Object { $_ -notmatch '^\d+ onecase ' -or ([regex]::Match($_,'^\d+ onecase "(b\d\d)[st]"').Groups[1].Value -in $changedTags) }
[IO.File]::WriteAllText((Join-Path $subset 'drive\runner_bas'),($runner -join "`n")+"`n",[Text.Encoding]::ASCII)
foreach ($name in @('benchmark.QCF','compile.QCF')) {
    $text=(Get-Content -LiteralPath (Join-Path $subset $name) -Raw).Replace($run,$subset)
    [IO.File]::WriteAllText((Join-Path $subset $name),$text,[Text.Encoding]::ASCII)
}
foreach ($name in @('sb-verification.json','native-verification.json')) {
    $rows=@(Get-Content -LiteralPath (Join-Path $subset $name) -Raw | ConvertFrom-Json | Where-Object { $_.Tag -in $changedTags })
    ConvertTo-Json -InputObject $rows -Depth 10 | Set-Content -LiteralPath (Join-Path $subset $name)
}
.\Benchmarks\QemuLator\Use-TimingBoot.ps1 -RunDirectory $subset
.\Benchmarks\QemuLator\Save-Metadata.ps1 -RunDirectory $subset
Copy-Item -LiteralPath (Join-Path $subset 'configuration.json') -Destination (Join-Path $subset 'configuration-before-run.json') -Force
$comparisons | Format-Table Benchmark,BeforeBytes,AfterBytes,Identical

