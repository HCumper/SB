<#
.SYNOPSIS
Builds and compares the SuperBASIC Turbo benchmarks with HIR optimization off and on.
.DESCRIPTION
Writes reusable executables, compiler logs, Turbo IR listings, HIR dumps and CSV/JSON
reports under artifacts\turbo-benchmarks. Instruction counts come from a functional
68000 emulator and do not measure Sinclair QL elapsed time. ROM workloads compile
in both HIR modes but require a real QL or full QL emulator for execution.
Use -CompareNative to compare threaded and audited native output in both HIR modes.
Use -CompareRegisters to include native register allocation on/off (six modes).
#>
[CmdletBinding()]
param(
    [switch]$Quick,
    [switch]$NoBuild,
    [switch]$CompareNative,
    [switch]$CompareRegisters,
    [string[]]$Name = @(),
    [ValidateRange(1, [long]::MaxValue)]
    [long]$MaxSteps = 50000000,
    [string]$OutputDirectory
)

$ErrorActionPreference = 'Stop'
$repository = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..\..'))
if (-not $OutputDirectory) { $OutputDirectory = Join-Path $repository 'artifacts\turbo-benchmarks' }
$OutputDirectory = [IO.Path]::GetFullPath($OutputDirectory)
New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null
$benchmarkTemp = Join-Path $OutputDirectory 'temp'
New-Item -ItemType Directory -Path $benchmarkTemp -Force | Out-Null
$previousTemp, $previousTmp = $env:TEMP, $env:TMP
try {
    $env:TEMP, $env:TMP = $benchmarkTemp, $benchmarkTemp
    & (Join-Path $PSScriptRoot 'Check-ReferenceChecksums.ps1')
    if (-not $NoBuild) {
        $buildLog = Join-Path $OutputDirectory 'build.log'
        & dotnet build (Join-Path $repository 'SBTests\SBTests.fsproj') --configuration Debug --nologo *> $buildLog
        if ($LASTEXITCODE -ne 0) { throw "Build failed; see $buildLog" }
    }
    # Use the selected SDK's FSI directly, avoiding a separate global tool install.
    $sdkVersion = (& dotnet --version).Trim()
    if ($LASTEXITCODE -ne 0) { throw 'Unable to find the active .NET SDK.' }
    $dotnetRoot = Split-Path -Parent (Get-Command dotnet).Source
    $fsi = Join-Path $dotnetRoot "sdk\$sdkVersion\FSharp\fsi.dll"
    if (-not (Test-Path -LiteralPath $fsi)) { throw "F# Interactive not found: $fsi" }
    $arguments = @($fsi, '--exec', (Join-Path $PSScriptRoot 'RunBenchmarks.fsx'), '--output', $OutputDirectory, '--max-steps', [string]$MaxSteps)
    if ($Quick) { $arguments += '--quick' }
    if ($CompareNative) { $arguments += '--compare-native' }
    if ($CompareRegisters) { $arguments += '--compare-registers' }
    foreach ($benchmarkName in $Name) { $arguments += @('--name', $benchmarkName) }
    & dotnet @arguments
    if ($LASTEXITCODE -ne 0) { throw 'One or more benchmark comparisons failed.' }
}
finally {
    $env:TEMP, $env:TMP = $previousTemp, $previousTmp
}
