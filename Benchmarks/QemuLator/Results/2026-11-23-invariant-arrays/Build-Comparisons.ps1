$ErrorActionPreference='Stop'
$taskRoot='C:\Source\codex-workspace\.scratch\turbo-invariant-arrays'
$cases=@(
    @{Name='route_planner';File='Benchmarks\QemuLator\Programs\route_planner.bas';Task='route_exe';Output="route_planner:4701`n"},
    @{Name='eight_queens';File='Benchmarks\QemuLator\Programs\eight_queens.bas';Task='queens_exe';Output="eight_queens:4962`n"},
    @{Name='local_integer_for';File='Benchmarks\Turbo\local_integer_for.bas';Task='local_exe';Output="local_integer_for:270`n"}
)
$cases += @(
    @{Name='matrix_multiply';File='Benchmarks\QemuLator\Programs\matrix_multiply.bas';Task='matrix_exe';Output="matrix_multiply:1094`n"},
    @{Name='native_truth_for';File='Benchmarks\Turbo\native_truth_for.bas';Task='truth_exe';Output="native_truth_for:4970`n"},
    @{Name='scoped_integer_for';File='Benchmarks\Turbo\scoped_integer_for.bas';Task='scope_exe';Output="scoped_integer_for:270`n"},
    @{Name='native_conditional_for';File='Benchmarks\Turbo\native_conditional_for.bas';Task='cond_exe';Output="native_conditional_for:6870`n"},
    @{Name='sieve';File='Benchmarks\QemuLator\Programs\sieve.bas';Task='sieve_exe';Output="sieve:840`n"},
    @{Name='bubble_sort';File='Benchmarks\Turbo\bubble_sort.bas';Task='bubble_exe';Output="bubble_sort:121`n"}
)
foreach ($case in $cases) {
    $before=Join-Path $taskRoot ('comparison\before\'+$case.Task)
    $after=Join-Path $taskRoot ('comparison\after\'+$case.Task)
    & dotnet fsi --exec (Join-Path $taskRoot 'Invoke-Baseline.fsx') $case.File $before false turbo --hir-opt --native-regions --regalloc ('--dump-hir='+$before)
    if ($LASTEXITCODE -ne 0) { throw "Baseline compile failed: $($case.Name)" }
    & dotnet SB\bin\Debug\net10.0\SB.dll $case.File $after false turbo --hir-opt --native-regions --regalloc ('--dump-hir='+$after)
    if ($LASTEXITCODE -ne 0) { throw "Current compile failed: $($case.Name)" }
}
$cases | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath (Join-Path $taskRoot 'comparison\plan.json')

