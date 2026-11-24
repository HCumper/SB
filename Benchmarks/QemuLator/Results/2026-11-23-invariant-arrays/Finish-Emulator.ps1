$ErrorActionPreference='Stop'
$taskRoot='C:\Source\codex-workspace\.scratch\turbo-invariant-arrays'
$run=Join-Path $taskRoot 'qemulator-arrays'
if (-not (Test-Path -LiteralPath (Join-Path $run 'drive\done'))) { throw 'Timing run has not finished.' }
python .\Benchmarks\QemuLator\Collect-Results.py $run --complete
if ($LASTEXITCODE -ne 0) { throw 'Timing collection failed.' }
.\Benchmarks\QemuLator\Save-Metadata.ps1 -RunDirectory $run
$record=Get-Content -LiteralPath (Join-Path $taskRoot 'emulator-launch.json') -Raw | ConvertFrom-Json
$observed=Get-CimInstance Win32_Process -Filter ('ProcessId='+$record.ProcessId)
if ($observed) {
    if ($observed.ExecutablePath -ne $record.Executable -or -not $observed.CommandLine.Contains($record.Config) -or $observed.CreationDate.ToUniversalTime() -ne ([datetime]$record.CreationDateUtc).ToUniversalTime()) { throw 'Refusing to stop a process that does not match the recorded owned benchmark process.' }
    Stop-Process -Id $record.ProcessId
    Write-Output 'Stopped the completed, owned hidden benchmark emulator.'
}
