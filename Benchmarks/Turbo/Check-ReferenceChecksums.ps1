# Independent host calculations. These never invoke SB, its optimizer or its runtime.
[CmdletBinding()]
param()
$ErrorActionPreference = 'Stop'

function Get-Checksum([string]$Name, [int]$Passes) {
    if ($Passes -lt 1 -or $Passes -gt 1000) { throw 'Workload passes must be between 1 and 1000.' }
    [long]$check = 0
    switch ($Name) {
        'regalloc_copy_chain' {
            # Each pass ends at i=1000; use the closed-form sum of pass numbers.
            $check = (1000 * $Passes + $Passes * ($Passes + 1) / 2) % 10000
        }
        'regalloc_arithmetic' {
            # Final -d = 3*1000+7-2 = 3005, independently of the copy chain.
            $check = (3005 * $Passes + $Passes * ($Passes + 1) / 2) % 10000
        }
        'integer_arithmetic' {
            for ($pass = 1; $pass -le $Passes; $pass++) {
                [long]$value = $pass
                for ($i = 1; $i -le 1000; $i++) { $value = ($value + ($i % 97) * 3 + 1) % 8192 }
                $check = ($check + $value) % 10000
            }
        }
        'constant_expressions' {
            for ($pass = 1; $pass -le $Passes; $pass++) {
                # Sum 1..1000 directly rather than repeating the SuperBASIC loop.
                $check = ($check + (($pass + 20 * 1000 + 1000 * 1001 / 2) % 8192)) % 10000
            }
        }
        'sieve' {
            $count = 0
            for ($candidate = 2; $candidate -le 1000; $candidate++) {
                $prime = $true
                for ($divisor = 2; $divisor * $divisor -le $candidate; $divisor++) {
                    if ($candidate % $divisor -eq 0) { $prime = $false; break }
                }
                if ($prime) { $count++ }
            }
            $check = ($Passes * $count) % 10000
        }
        'matrix_multiply' {
            for ($pass = 1; $pass -le $Passes; $pass++) {
                for ($row = 0; $row -lt 8; $row++) {
                    for ($col = 0; $col -lt 8; $col++) {
                        [long]$dot = 0
                        for ($k = 0; $k -lt 8; $k++) { $dot += (($row + $k + $pass) % 7) * (($k + 2 * $col + $pass) % 5) }
                        $check = ($check + $dot * (1 + (($row + $col) % 7))) % 10000
                    }
                }
            }
        }
        'bubble_sort' {
            for ($pass = 1; $pass -le $Passes; $pass++) {
                $values = @(0..63 | ForEach-Object { ($_ * 73 + $pass * 19) % 997 } | Sort-Object)
                for ($i = 0; $i -lt 64; $i++) { $check = ($check + $values[$i] * (1 + ($i % 7))) % 10000 }
            }
        }
        'procedure_calls' {
            # 1000 MOD 7 = 6: 142 full cycles contribute 142*21, then 1..6 adds 21.
            for ($pass = 1; $pass -le $Passes; $pass++) { $check = ($check + (($pass + 3003) % 8192)) % 10000 }
        }
        'recursion' {
            for ($pass = 1; $pass -le $Passes; $pass++) {
                [long]$a, [long]$b = 0, 1
                for ($i = 0; $i -lt 12 + ($pass % 2); $i++) { $a, $b = $b, ($a + $b) }
                $check = ($check + $a) % 10000
            }
        }
        'string_operations' {
            # 64 bytes plus the first and last ASCII codes after 32 rotations.
            $odd = [int][Math]::Ceiling($Passes / 2.0)
            $even = $Passes - $odd
            $check = ($odd * (64 + 97 + 98) + $even * (64 + 99 + 100)) % 10000
        }
        'float_dyadic' { $check = ($Passes * 251 + $Passes * ($Passes + 1)) % 10000 }
        'float_polynomial' {
            [decimal]$n = 1000
            $sum1 = $n * ($n + 1) / 2 / 1000
            $sum2 = $n * ($n + 1) * (2 * $n + 1) / 6 / 1000000
            $sum3 = ($n * ($n + 1) / 2) * ($n * ($n + 1) / 2) / 1000000000
            $total = [decimal]1.25 * $sum3 - [decimal]0.75 * $sum2 + [decimal]0.5 * $sum1 + 2 * $n
            if ($total -ne [decimal]2313.0001875) { throw 'Polynomial reference calculation is inconsistent.' }
            $check = ($Passes * [int][Math]::Round($total, 0, [MidpointRounding]::AwayFromZero)) % 10000
        }
        'float_transcendentals' { $check = ($Passes * 256) % 10000 }
        default { throw "Unknown reference workload: $Name" }
    }
    return [int]$check
}

$suite = Get-Content -LiteralPath (Join-Path $PSScriptRoot 'suite.json') -Raw | ConvertFrom-Json
foreach ($benchmark in $suite.benchmarks) {
    $default = Get-Checksum $benchmark.name $benchmark.passes
    $quick = Get-Checksum $benchmark.name $benchmark.quickPasses
    if ($default -ne $benchmark.expectedChecksum -or $quick -ne $benchmark.quickChecksum) {
        throw "Reference checksum mismatch for $($benchmark.name): default $default, quick $quick."
    }
}
Write-Output "Verified independent reference checksums for $($suite.benchmarks.Count) benchmarks."
