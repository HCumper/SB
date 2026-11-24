$ErrorActionPreference='Stop'
$taskRoot='C:\Source\codex-workspace\.scratch\turbo-invariant-arrays'
$run=Join-Path $taskRoot 'qemulator-arrays'
$config=Join-Path $run 'benchmark.QCF'
$exe='C:\Program Files (x86)\Q-emuLator\QemuLator.exe'
$log=Get-Content -LiteralPath (Join-Path $taskRoot 'full-tests.log') -Raw
if ([regex]::Matches($log,'Passed!\s*- Failed:\s*0, Passed:').Count -ne 2) { throw 'Wait for both full-suite assemblies before elapsed timing.' }
if (Test-Path -LiteralPath (Join-Path $run 'drive\done')) { throw 'Refusing to overwrite completed timings.' }
if ((Get-Content -LiteralPath $config) -notcontains 'Speed=QL') { throw 'Expected Original QL speed.' }
$emulatorProcess=Start-Process -FilePath $exe -ArgumentList ('"'+$config+'"') -WindowStyle Hidden -PassThru
$observed=Get-CimInstance Win32_Process -Filter ('ProcessId='+$emulatorProcess.Id)
if (-not $observed -or $observed.ExecutablePath -ne $exe -or -not $observed.CommandLine.Contains($config)) { throw 'Emulator configuration/process ownership could not be verified.' }
$record=[ordered]@{ProcessId=$emulatorProcess.Id;Executable=$observed.ExecutablePath;CommandLine=$observed.CommandLine;CreationDateUtc=$observed.CreationDate.ToUniversalTime().ToString('O');WindowStyle='Hidden';Config=$config;ConfigSha256=(Get-FileHash -LiteralPath $config).Hash;Speed='QL';FullSuiteCompletedBeforeLaunch=$true}
$record | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $taskRoot 'emulator-launch.json')
$record | ConvertTo-Json
