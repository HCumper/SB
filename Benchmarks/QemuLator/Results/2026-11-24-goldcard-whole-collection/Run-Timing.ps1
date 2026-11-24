$ErrorActionPreference='Stop'
Set-Location C:\Source\SB
$run='C:\Source\codex-workspace\.scratch\sb-direct-rom-maths-goldcard\qemulator-unthrottled'
$compiler='C:\Source\SB\SB\bin\Debug\net10.0\SB.dll'
$exe='C:\Program Files (x86)\Q-emuLator\QemuLator.exe'
if (Get-Process -Name QemuLator -ErrorAction SilentlyContinue) { throw 'Another Q-emuLator is running.' }
$plan=Get-Content -LiteralPath (Join-Path $run 'plan.json') -Raw | ConvertFrom-Json
$config=Join-Path $run 'benchmark.QCF'
if ((Get-Content -LiteralPath $config) -notcontains 'Speed=GoldCard') { throw 'Expected GoldCard speed.' }
if (Test-Path -LiteralPath (Join-Path $run 'drive\done')) { throw 'Refusing to overwrite completed timings.' }
$load=Join-Path $run 'host-load.csv'
'utc,totalCpuPercent' | Set-Content -LiteralPath $load
$process=Start-Process -FilePath $exe -ArgumentList ('"'+$config+'"') -WindowStyle Hidden -PassThru
$observed=Get-CimInstance Win32_Process -Filter ('ProcessId='+$process.Id)
if (-not $observed -or $observed.ExecutablePath -ne $exe -or -not $observed.CommandLine.Contains($config)) { throw 'Emulator launch ownership not verified.' }
# Keep the emulator on performance cores (logical 0-15) at high priority, so a busy host cannot
# push it onto an efficiency core and below real time.
$process.ProcessorAffinity=[IntPtr]0xFFFF
$process.PriorityClass='High'
# Opt the emulator out of Windows power throttling: no EcoQoS execution-speed throttling and
# no ignored timer-resolution requests while its window is hidden.
Add-Type -TypeDefinition @'
using System; using System.Runtime.InteropServices;
public static class EmulatorThrottling {
  [StructLayout(LayoutKind.Sequential)] public struct State { public uint Version; public uint ControlMask; public uint StateMask; }
  [DllImport("kernel32.dll", SetLastError=true)] static extern bool SetProcessInformation(IntPtr process, int informationClass, ref State information, uint size);
  public static void Disable(IntPtr process) {
    var state = new State { Version = 1, ControlMask = 0x1 | 0x4, StateMask = 0 };
    if (!SetProcessInformation(process, 4, ref state, (uint)Marshal.SizeOf(state))) throw new System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error());
  }
}
'@
[EmulatorThrottling]::Disable($process.Handle)
$record=[ordered]@{PowerThrottling='execution speed and timer resolution disabled';ProcessorAffinity='0xFFFF';PriorityClass='High';ProcessId=$process.Id;Executable=$exe;CommandLine=$observed.CommandLine;CreationDateUtc=$observed.CreationDate.ToUniversalTime().ToString('O');WindowStyle='Hidden';Config=$config;ConfigSha256=(Get-FileHash -LiteralPath $config).Hash;Speed='GoldCard';FullSuiteCompletedBeforeLaunch=$true;CompilerSha256=(Get-FileHash -LiteralPath $compiler).Hash}
$record | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $run 'emulator-launch.json')
Write-Output 'Started GoldCard timing: whole suite'
$started=Get-Date
$last=-1
while (-not (Test-Path -LiteralPath (Join-Path $run 'drive\done'))) {
    $count=@(Get-ChildItem -LiteralPath (Join-Path $run 'drive') -File | Where-Object Name -match '^r\d{3}$').Count
    if ($count -ne $last -and $count % 8 -eq 0) { Write-Output ("{0} / {1} external jobs completed" -f $count,$plan.expectedResults) }
    $last=$count
    $observed=Get-CimInstance Win32_Process -Filter ('ProcessId='+$record.ProcessId)
    if (-not $observed -or $observed.ExecutablePath -ne $exe -or -not $observed.CommandLine.Contains($config)) { throw 'Owned benchmark emulator exited unexpectedly.' }
    if (((Get-Date)-$started).TotalMinutes -gt 40) { throw 'Timing run exceeded 40 minutes.' }
    $cpu=[math]::Round((Get-Counter '\Processor(_Total)\% Processor Time' -SampleInterval 1 -MaxSamples 1).CounterSamples[0].CookedValue,1)
    ('{0},{1}' -f [DateTime]::UtcNow.ToString('o'),$cpu) | Add-Content -LiteralPath $load
    Start-Sleep -Seconds 4
}
python Benchmarks/QemuLator/Collect-Results.py $run --complete
if ($LASTEXITCODE -ne 0) { throw 'Timing validation failed.' }
.\Benchmarks\QemuLator\Save-Metadata.ps1 -RunDirectory $run -CompilerDll $compiler
$observed=Get-CimInstance Win32_Process -Filter ('ProcessId='+$record.ProcessId)
if ($observed) {
    if ($observed.ExecutablePath -ne $exe -or -not $observed.CommandLine.Contains($config) -or $observed.CreationDate.ToUniversalTime() -ne ([datetime]$record.CreationDateUtc).ToUniversalTime()) { throw 'Refusing to stop a process whose ownership changed.' }
    Stop-Process -Id $record.ProcessId
}
Write-Output 'Validated and stopped owned emulator: whole suite'
