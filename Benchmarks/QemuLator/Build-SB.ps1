[CmdletBinding()]
param(
    [Parameter(Mandatory)][string]$RunDirectory,
    [string]$CompilerDll = (Join-Path $PSScriptRoot '..\..\SB\bin\Debug\net10.0\SB.dll')
)
$ErrorActionPreference = 'Stop'
$run = (Resolve-Path -LiteralPath $RunDirectory).Path
$compiler = (Resolve-Path -LiteralPath $CompilerDll).Path
$plan = Get-Content -LiteralPath (Join-Path $run 'plan.json') -Raw | ConvertFrom-Json
if ($plan.formatVersion -ne 2 -or $plan.timingMethod -ne 'external-qdos-job') { throw 'External timing plan required.' }
$compilerHash = (Get-FileHash -LiteralPath $compiler).Hash
function Compile([string]$InputPath,[string]$OutputPath,[string]$Dump) {
    & dotnet $compiler $InputPath $OutputPath false turbo --hir-opt --native-regions --regalloc ('--dump-hir=' + $Dump) *> ($Dump + '_compile.log')
    if ($LASTEXITCODE -ne 0) { throw "SB compilation failed; see ${Dump}_compile.log" }
}
function Coverage([string]$Dump) {
    $native = Get-Content -LiteralPath ($Dump + '.native-ir.txt') -Raw
    $analysis = Get-Content -LiteralPath ($Dump + '.analysis.txt') -Raw
    $loop = [regex]::Match($native,'Native integer FOR loops: selected=(\d+); rejected=(\d+)')
    $proof = [regex]::Match($analysis,'Unchecked arithmetic proofs enabled: (True|False); proven expressions: (\d+)')
    if (-not $loop.Success -or -not $proof.Success) { throw "Missing coverage diagnostics: $Dump" }
    [ordered]@{ NativeForSelected=[int]$loop.Groups[1].Value; NativeForRejected=[int]$loop.Groups[2].Value
        ArithmeticProofsEnabled=$proof.Groups[1].Value -eq 'True'; ProvenExpressions=[int]$proof.Groups[2].Value
        ProvenArrayAccesses=[int]([regex]::Match($analysis,'Proven numeric array accesses: (\d+)').Groups[1].Value) }
}
$verification = @(
foreach ($case in $plan.benchmarks) {
    $original = [IO.File]::ReadAllText($case.sourceFile).Replace("`r`n","`n")
    $sourceHash = [Convert]::ToHexString([Security.Cryptography.SHA256]::HashData([Text.Encoding]::ASCII.GetBytes($original)))
    if ($sourceHash -ne $case.sourceSha256) { throw "Canonical source changed: $($case.name)" }
    $directives = for ($i=0; $i -lt $case.compilerDirectives.Count; $i++) { "$($i+1) $($case.compilerDirectives[$i])`n" }
    $expectedSource = ($directives -join '') + $original
    $inputPath = Join-Path $run ($case.tag + '_sb.bas')
    foreach ($path in @($inputPath,(Join-Path $run ('drive\'+$case.tag+'_bas')))) {
        if ([IO.File]::ReadAllText($path) -cne $expectedSource) { throw "Compiled input modifies the original program: $path" }
    }
    $outputPath = Join-Path $run ('drive\' + $case.tag + 's')
    $dump = Join-Path $run ($case.tag + '_sb')
    Compile $inputPath $outputPath $dump
    $referenceDirectory = Join-Path $run ('reference\'+$case.tag)
    New-Item -ItemType Directory -Path $referenceDirectory -Force | Out-Null
    $referenceInput = Join-Path $referenceDirectory 'ordinary.bas'
    [IO.File]::WriteAllText($referenceInput,$expectedSource,[Text.Encoding]::ASCII)
    $referenceOutput = Join-Path $referenceDirectory ($case.tag+'s')
    $referenceDump = Join-Path $referenceDirectory 'ordinary'
    Compile $referenceInput $referenceOutput $referenceDump
    $actualCoverage = Coverage $dump
    $referenceCoverage = Coverage $referenceDump
    $actualHash = (Get-FileHash -LiteralPath $outputPath).Hash
    $referenceHash = (Get-FileHash -LiteralPath $referenceOutput).Hash
    if ($actualHash -ne $referenceHash -or ($actualCoverage|ConvertTo-Json -Compress) -cne ($referenceCoverage|ConvertTo-Json -Compress)) {
        throw "Ordinary-build verification failed: $($case.name)"
    }
    [pscustomobject]@{ Benchmark=$case.name; Tag=$case.tag; OriginalSourceUnchanged=$true
        OrdinaryExecutableIdentical=$true; ExecutableSha256=$actualHash; ReferenceSha256=$referenceHash
        Coverage=$actualCoverage; ReferenceCoverage=$referenceCoverage }
    Write-Host "Built and verified ordinary optimized output: $($case.name)"
})
$verification | ConvertTo-Json -Depth 8 -AsArray | Set-Content -LiteralPath (Join-Path $run 'sb-verification.json')
if ((Get-FileHash -LiteralPath $compiler).Hash -ne $compilerHash) { throw 'Compiler changed during build.' }
[pscustomobject]@{ CompilerDll=$compiler; CompilerSha256=$compilerHash; Flags='--hir-opt --native-regions --regalloc'
    RecordedUtc=[DateTime]::UtcNow.ToString('o') } | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $run 'sb-build.json')
