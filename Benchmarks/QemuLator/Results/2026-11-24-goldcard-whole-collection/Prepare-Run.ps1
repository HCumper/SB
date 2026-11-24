$ErrorActionPreference='Stop'
Set-Location C:\Source\SB
# A fresh whole-collection GoldCard run: prepared inputs and verified Turbo FAST captures
# from the earlier run directory, SB jobs rebuilt by the current compiler.
$source='C:\Source\codex-workspace\.scratch\sb-steps-goldcard\qemulator'
$run='C:\Source\codex-workspace\.scratch\sb-direct-rom-maths-goldcard\qemulator-unthrottled'
$compiler='C:\Source\SB\SB\bin\Debug\net10.0\SB.dll'
if (Test-Path -LiteralPath $run) { throw "Refusing to reuse $run" }
New-Item -ItemType Directory -Path (Join-Path $run 'drive') | Out-Null
foreach ($file in @('plan.json','benchmark.QCF','compile.QCF','compile-commands.json','turbo-provenance.json')) {
    Copy-Item -LiteralPath (Join-Path $source $file) -Destination $run
}
Get-ChildItem -LiteralPath $source -File | Where-Object { $_.Name -match '^b\d\d_(sb|common)\.bas$' } | Copy-Item -Destination $run
# Turbo inputs, captures and reports, the tools and the runner. No SB jobs, samples or markers.
Get-ChildItem -LiteralPath (Join-Path $source 'drive') -File |
    Where-Object { $_.Name -match '^b\d\d(_bas|_ir|_report|t)$' -or $_.Name -in @('parser_task','codegen_task','pollclock','ttk','run_boot','runner_bas') } |
    Copy-Item -Destination (Join-Path $run 'drive')
foreach ($file in @('benchmark.QCF','compile.QCF')) {
    $text=(Get-Content -LiteralPath (Join-Path $run $file) -Raw).Replace($source,$run)
    [IO.File]::WriteAllText((Join-Path $run $file),$text,[Text.Encoding]::ASCII)
}
$plan=Get-Content -LiteralPath (Join-Path $run 'plan.json') -Raw | ConvertFrom-Json
foreach ($case in $plan.benchmarks) {
    foreach ($suffix in @('_bas','t','_ir','_report')) {
        $name="drive\$($case.tag)$suffix"
        if ((Get-FileHash -LiteralPath (Join-Path $run $name)).Hash -ne (Get-FileHash -LiteralPath (Join-Path $source $name)).Hash) { throw "Copy mismatch: $name" }
    }
}
Write-Output "Copied inputs and $($plan.benchmarks.Count) verified Turbo FAST captures to $run"
dotnet fsi --exec Benchmarks\QemuLator\Inspect-Turbo.fsx $run
if ($LASTEXITCODE -ne 0) { throw 'Turbo native verification failed.' }
.\Benchmarks\QemuLator\Build-SB.ps1 -RunDirectory $run -CompilerDll $compiler
.\Benchmarks\QemuLator\Use-TimingBoot.ps1 -RunDirectory $run
.\Benchmarks\QemuLator\Save-Metadata.ps1 -RunDirectory $run -CompilerDll $compiler
Copy-Item -LiteralPath (Join-Path $run 'configuration.json') -Destination (Join-Path $run 'configuration-before-run.json') -Force
Write-Output "Prepared $run"
