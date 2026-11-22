[CmdletBinding()]
param(
    [Parameter(Mandatory)][string]$RunDirectory,
    [string]$CompilerDll = (Join-Path $PSScriptRoot '..\..\SB\bin\Debug\net10.0\SB.dll'),
    [string]$RepositoryDirectory = (Join-Path $PSScriptRoot '..\..'),
    [string]$Emulator = 'C:\Program Files (x86)\Q-emuLator\QemuLator.exe'
)
$ErrorActionPreference='Stop'
$run=(Resolve-Path -LiteralPath $RunDirectory).Path
$drive=Join-Path $run 'drive'
$plan=Get-Content -LiteralPath (Join-Path $run 'plan.json') -Raw | ConvertFrom-Json
if ($plan.formatVersion -ne 2) { throw 'External timing plan required.' }
function Hash([string]$Path) { (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash }
$build=Get-Content -LiteralPath (Join-Path $run 'sb-build.json') -Raw | ConvertFrom-Json
if ((Hash $CompilerDll) -ne $build.CompilerSha256) { throw 'SB compiler changed since the benchmark build.' }
$sbVerification=@(Get-Content -LiteralPath (Join-Path $run 'sb-verification.json') -Raw | ConvertFrom-Json)
$fingerprints=[ordered]@{}
foreach ($file in @('parser_task','codegen_task','ttk','pollclock')) { $fingerprints[$file]=Hash (Join-Path $drive $file) }
$fingerprints['SB.dll']=Hash $CompilerDll
$fingerprints['lib537.bin']=Hash (Join-Path $RepositoryDirectory 'SB\TurboRuntime\lib537.bin')
$fingerprints['emulator']=Hash $Emulator
$config=Get-Content -LiteralPath (Join-Path $run 'benchmark.QCF')
foreach ($key in @('MainRom','BackRom')) {
    $line=@($config | Where-Object { $_.StartsWith($key+'=') })
    if ($line.Count -ne 1) { throw "Missing $key setting." }
    $fingerprints[$key]=Hash $line[0].Substring($key.Length+1)
}
foreach ($file in @('Prepare-Benchmarks.py','Build-SB.ps1','Collect-Results.py','Use-TimingBoot.ps1','Inspect-Turbo.fsx','Save-Metadata.ps1','Test-Harness.py','Check-References.py')) {
    $fingerprints[$file]=Hash (Join-Path $PSScriptRoot $file)
}
$fingerprints['benchmark.QCF']=Hash (Join-Path $run 'benchmark.QCF')
$fingerprints['runner_bas']=Hash (Join-Path $drive 'runner_bas')
$programs=@(foreach ($case in $plan.benchmarks) {
    $original=[IO.File]::ReadAllText($case.sourceFile).Replace("`r`n","`n")
    $sourceHash=[Convert]::ToHexString([Security.Cryptography.SHA256]::HashData([Text.Encoding]::ASCII.GetBytes($original)))
    if ($sourceHash -ne $case.sourceSha256) { throw "Canonical source changed: $($case.name)" }
    $directives=for ($i=0;$i -lt $case.compilerDirectives.Count;$i++) { "$($i+1) $($case.compilerDirectives[$i])`n" }
    $expected=($directives -join '')+$original
    foreach ($path in @((Join-Path $run ($case.tag+'_sb.bas')),(Join-Path $drive ($case.tag+'_bas')))) {
        if ([IO.File]::ReadAllText($path) -cne $expected) { throw "Compiled source changed: $path" }
    }
    $verified=@($sbVerification | Where-Object Benchmark -eq $case.name)
    if ($verified.Count -ne 1 -or (Hash (Join-Path $drive ($case.tag+'s'))) -ne $verified[0].ExecutableSha256) { throw "SB executable changed: $($case.name)" }
    [pscustomobject]@{ Benchmark=$case.name; Passes=$case.passes; ExpectedChecksum=$case.expectedChecksum
        OriginalProgramUnchanged=$true; SourceSha256=$case.sourceSha256
        SbExecutableSha256=Hash (Join-Path $drive ($case.tag+'s'))
        TurboExecutableSha256=Hash (Join-Path $drive ($case.tag+'t'))
        CapturedTurboIrSha256=Hash (Join-Path $run ($case.tag+'.turbo.ir'))
        SbInputSha256=Hash (Join-Path $run ($case.tag+'_sb.bas'))
        TurboInputSha256=Hash (Join-Path $drive ($case.tag+'_bas'))
        OrdinarySbExecutableIdentical=$verified[0].OrdinaryExecutableIdentical
        SbOptimizationCoverage=$verified[0].Coverage }
})
$previousPath=Join-Path $run 'configuration.json'
if (Test-Path -LiteralPath $previousPath) {
    $previous=Get-Content -LiteralPath $previousPath -Raw | ConvertFrom-Json
    foreach ($key in @('parser_task','codegen_task','ttk','pollclock','SB.dll','lib537.bin','emulator','MainRom','BackRom','benchmark.QCF','runner_bas')) {
        if ($previous.Fingerprints.$key -ne $fingerprints[$key]) { throw "Runtime/build fingerprint changed during timing: $key" }
    }
    foreach ($program in $programs) {
        $before=@($previous.Programs | Where-Object Benchmark -eq $program.Benchmark)
        if ($before.Count -ne 1 -or $before[0].SbExecutableSha256 -ne $program.SbExecutableSha256 -or
            $before[0].TurboExecutableSha256 -ne $program.TurboExecutableSha256 -or
            $before[0].SourceSha256 -ne $program.SourceSha256) { throw "Benchmark executable or source changed during timing: $($program.Benchmark)" }
    }
}
$revision=(& git -C $RepositoryDirectory rev-parse HEAD).Trim()
if ($LASTEXITCODE -ne 0) { throw 'Cannot identify SB revision.' }
[pscustomobject]@{ FormatVersion=2; TimingMethod=$plan.timingMethod; RecordedUtc=[DateTime]::UtcNow.ToString('o')
    Emulator='Q-emuLator'; EmulatorVersion=(Get-Item -LiteralPath $Emulator).VersionInfo.FileVersion
    TimingSpeed='Original QL (Speed=QL)'; CompilationSpeed='Full; compilation excluded'; RamKiB=640; Rom='JS'
    TurboParserVersion='5.09'; TurboCodeGeneratorVersion='3.27 (generator 4.10, library 5.37)'; TurboToolkitVersion='3.44'
    TurboOptions='FAST (TURBO_optim 2), large addressing, no copied windows, no line numbers, structured, 20 KiB dataspace, base stack 350 bytes'
    SbRevision=$revision; SbOptions=$build.Flags; HostOs=[Environment]::OSVersion.VersionString
    ClockHz=50; ClockResolutionSeconds=0.02; WarmupsPerPair=$plan.warmups; TimedTrialsPerPair=$plan.trials
    Scope=$plan.scope; OutputCapture='Unchanged PRINT #1; 31 inherited references to the same output-file channel (lib537 channel table is filled backwards from #31).'
    Interpretation='Elapsed time in the recorded emulator configuration, including whole-job overhead; not original MC68008 hardware timing or directly comparable to the superseded instrumented kernel-only timings.'
    Fingerprints=$fingerprints; Programs=$programs } | ConvertTo-Json -Depth 10 | Set-Content -LiteralPath (Join-Path $run 'configuration.json')
Write-Output "Recorded external timing configuration and $($programs.Count) unchanged programs."
