[CmdletBinding()]
param(
    [Parameter(Mandatory)][string]$RunDirectory,
    [Parameter(Mandatory)][string]$OutputDirectory
)
$ErrorActionPreference='Stop'
$repository=Split-Path (Split-Path $PSScriptRoot -Parent) -Parent
$run=(Resolve-Path -LiteralPath $RunDirectory).Path
$output=[IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath (Join-Path $output 'profiles.json')) { throw 'Refusing to overwrite completed profiles.' }
$generated=Join-Path $output 'generated'
New-Item -ItemType Directory -Force -Path $output | Out-Null
python (Join-Path $PSScriptRoot 'Prepare-Profiler.py') --repository $repository --output $generated
if ($LASTEXITCODE -ne 0) { throw 'Diagnostic source preparation failed.' }
dotnet fsi --exec (Join-Path $generated 'Profile-Workloads.fsx') $run $output 2>&1 | Tee-Object -FilePath (Join-Path $output 'profiles.log')
if ($LASTEXITCODE -ne 0) { throw 'Workload profiling failed.' }
python (Join-Path $PSScriptRoot 'Verify-Profiles.py') $output --negative-checks --output (Join-Path $output 'verification.json')
if ($LASTEXITCODE -ne 0) { throw 'Profile verification failed.' }
