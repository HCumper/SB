$ErrorActionPreference='Stop'
$taskRoot='C:\Source\codex-workspace\.scratch\turbo-native-recursive-calls'
$compiler='C:\Source\SB\SB\bin\Debug\net10.0\SB.dll'
$exe='C:\Program Files (x86)\Q-emuLator\QemuLator.exe'
$groups=@('other','loops','arrays','remaining')
while ($true) {
    $complete=$true
    foreach ($group in $groups) {
        $log=Get-Content -LiteralPath (Join-Path $taskRoot ('full-'+$group+'.log')) -Raw
        if ($log -match 'Failed!\s*- Failed:') { throw ('Test partition failed: '+$group) }
        $expected=if ($group -eq 'other') { 2 } else { 1 }
        if ([regex]::Matches($log,'Passed!\s*- Failed:\s*0, Passed:').Count -ne $expected) {
            $complete=$false
            $running=Get-CimInstance Win32_Process | Where-Object { $_.Name -eq 'dotnet.exe' -and $_.CommandLine -and $_.CommandLine.Contains((Join-Path $taskRoot ('full-tests\'+$group))) }
            if (-not $running) { throw ('Test partition ended without success: '+$group) }
        }
    }
    if ($complete) { break }
    Start-Sleep -Seconds 20
}
python (Join-Path $taskRoot 'Audit-FullSuite.py')
if ($LASTEXITCODE -ne 0) { throw 'Full-suite coverage audit failed.' }
function Run-One([string]$Name,[string]$RunDirectory,[string]$CompilerDll) {
    $config=Join-Path $RunDirectory 'benchmark.QCF'
    if ((Get-Content -LiteralPath $config) -notcontains 'Speed=GoldCard') { throw 'Expected GoldCard speed.' }
    if (Test-Path -LiteralPath (Join-Path $RunDirectory 'drive\done')) { throw 'Refusing to overwrite completed timings.' }
    $process=Start-Process -FilePath $exe -ArgumentList ('"'+$config+'"') -WindowStyle Hidden -PassThru
    $observed=Get-CimInstance Win32_Process -Filter ('ProcessId='+$process.Id)
    if (-not $observed -or $observed.ExecutablePath -ne $exe -or -not $observed.CommandLine.Contains($config)) { throw 'Emulator launch ownership not verified.' }
    $record=[ordered]@{ProcessId=$process.Id;Executable=$exe;CommandLine=$observed.CommandLine;CreationDateUtc=$observed.CreationDate.ToUniversalTime().ToString('O');WindowStyle='Hidden';Config=$config;ConfigSha256=(Get-FileHash -LiteralPath $config).Hash;Speed='GoldCard';FullSuiteCompletedBeforeLaunch=$true;CompilerSha256=(Get-FileHash -LiteralPath $CompilerDll).Hash}
    $record | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $RunDirectory 'emulator-launch.json')
    Write-Output ("Started GoldCard timing: "+$Name)
    $last=-1
    while (-not (Test-Path -LiteralPath (Join-Path $RunDirectory 'drive\done'))) {
        $count=@(Get-ChildItem -LiteralPath (Join-Path $RunDirectory 'drive') -File | Where-Object Name -match '^r\d{3}$').Count
        if ($count -ne $last) { Write-Output ($Name+': '+$count+' / 8 external jobs completed');$last=$count }
        $observed=Get-CimInstance Win32_Process -Filter ('ProcessId='+$record.ProcessId)
        if (-not $observed -or $observed.ExecutablePath -ne $exe -or -not $observed.CommandLine.Contains($config)) { throw 'Owned benchmark emulator exited unexpectedly.' }
        Start-Sleep -Seconds 10
    }
    python Benchmarks/QemuLator/Collect-Results.py $RunDirectory --complete
    if ($LASTEXITCODE -ne 0) { throw 'Timing validation failed.' }
    .\Benchmarks\QemuLator\Save-Metadata.ps1 -RunDirectory $RunDirectory -CompilerDll $CompilerDll
    $observed=Get-CimInstance Win32_Process -Filter ('ProcessId='+$record.ProcessId)
    if ($observed) {
        if ($observed.ExecutablePath -ne $exe -or -not $observed.CommandLine.Contains($config) -or $observed.CreationDate.ToUniversalTime() -ne ([datetime]$record.CreationDateUtc).ToUniversalTime()) { throw 'Refusing to stop a process whose ownership changed.' }
        Stop-Process -Id $record.ProcessId
    }
    Write-Output ('Validated and stopped owned emulator: '+$Name)
}
Run-One 'baseline SB versus Turbo FAST' (Join-Path $taskRoot 'qemulator-baseline-goldcard') (Join-Path $taskRoot 'baseline-SB.dll')
Run-One 'native recursive boundary versus Turbo FAST' (Join-Path $taskRoot 'qemulator-goldcard') $compiler
