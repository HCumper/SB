param([Parameter(Mandatory)][string]$Run, [int]$TimeoutMinutes = 40)
$ErrorActionPreference = 'Stop'
$exe = 'C:\Program Files (x86)\Q-emuLator\QemuLator.exe'
$config = Join-Path $Run 'turbo.QCF'
$drive = Join-Path $Run 'drive'
if (Get-Process -Name QemuLator -ErrorAction SilentlyContinue) { throw 'Another Q-emuLator is running.' }
if (Test-Path -LiteralPath (Join-Path $drive 'e')) { throw 'Refusing to overwrite a completed run.' }
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
$process = Start-Process -FilePath $exe -ArgumentList ('"' + $config + '"') -WindowStyle Hidden -PassThru
$process.ProcessorAffinity = [IntPtr]0xFFFF
$process.PriorityClass = 'High'
[EmulatorThrottling]::Disable($process.Handle)
$observed = Get-CimInstance Win32_Process -Filter ('ProcessId=' + $process.Id)
$launch = [ordered]@{ ProcessId = $process.Id; CreationDateUtc = $observed.CreationDate.ToUniversalTime().ToString('O'); Config = $config
    Speed = 'GoldCard'; WindowStyle = 'Hidden'; PowerThrottling = 'execution speed and timer resolution disabled'; ProcessorAffinity = '0xFFFF'; PriorityClass = 'High' }
$launch | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $Run 'emulator-launch.json')
$load = Join-Path $Run 'host-load.csv'
'utc,totalCpuPercent' | Set-Content -LiteralPath $load
$started = Get-Date
try {
    while (-not (Test-Path -LiteralPath (Join-Path $drive 'e'))) {
        if (-not (Get-Process -Id $process.Id -ErrorAction SilentlyContinue)) { throw 'The emulator exited.' }
        if (((Get-Date) - $started).TotalMinutes -gt $TimeoutMinutes) { throw "No end marker after $TimeoutMinutes minutes." }
        $cpu = [math]::Round((Get-Counter '\Processor(_Total)\% Processor Time' -SampleInterval 1 -MaxSamples 1).CounterSamples[0].CookedValue, 1)
        ('{0},{1}' -f [DateTime]::UtcNow.ToString('o'), $cpu) | Add-Content -LiteralPath $load
        Start-Sleep -Seconds 4
    }
    Start-Sleep -Seconds 2
    $s = (Get-Item -LiteralPath (Join-Path $drive 's')).CreationTimeUtc
    $e = (Get-Item -LiteralPath (Join-Path $drive 'e')).CreationTimeUtc
    $result = [ordered]@{ StartUtc = $s.ToString('O'); EndUtc = $e.ToString('O'); Seconds = [math]::Round(($e - $s).TotalSeconds, 3)
        ObjectBytes = (Get-Item -LiteralPath (Join-Path $drive 'p509t') -ErrorAction SilentlyContinue).Length }
    $result | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $Run 'result.json')
    $result | ConvertTo-Json
}
finally {
    $now = Get-CimInstance Win32_Process -Filter ('ProcessId=' + $process.Id)
    if ($now -and $now.ExecutablePath -eq $exe -and $now.CreationDate.ToUniversalTime() -eq $observed.CreationDate.ToUniversalTime()) { Stop-Process -Id $process.Id }
}
