# SuperBASIC Turbo benchmarks

Use `-CompareRegisters` to compare all six combinations of HIR rewriting and
threaded/native-template/register-allocated output. Native comparison alone
(`-CompareNative`) explicitly disables register allocation to preserve its
template baseline. Register-aware reports and source mappings are described in
[the allocator documentation](../../SB/Optimization/TURBO-REGISTER-ALLOCATION.md).

These original SuperBASIC workloads test SB's QL output with HIR optimization disabled and enabled. The runner checks independently calculated results and records executable bytes and functional 68000 instruction counts. The programs print once after the workload; sort and floating cases also contain correctness checks.

## Workloads

| Source | Default passes | Default checksum | Quick checksum | Execution |
| --- | ---: | ---: | ---: | --- |
| `regalloc_copy_chain.bas` | 20 | 210 | 1001 | Allocation microbenchmark |
| `regalloc_arithmetic.bas` | 20 | 310 | 3006 | Allocation microbenchmark |
| `native_constant_chain.bas` | 20 | 490 | 1015 | Native-value microbenchmark |
| `native_repeated_expression.bas` | 20 | 9430 | 962 | Native-value microbenchmark |
| `integer_arithmetic.bas` | 20 | 6430 | 2812 | Functional harness |
| `constant_expressions.bas` | 20 | 8290 | 4405 | Functional harness |
| `sieve.bas` | 5 | 840 | 168 | Functional harness |
| `matrix_multiply.bas` | 10 | 1094 | 1774 | Functional harness |
| `bubble_sort.bas` | 5 | 121 | 3846 | Functional harness |
| `procedure_calls.bas` | 20 | 270 | 3004 | Functional harness |
| `recursion.bas` | 10 | 1885 | 233 | Functional harness |
| `string_operations.bas` | 20 | 5220 | 259 | Functional harness |
| `float_dyadic.bas` | 20 | 5440 | 253 | Functional harness |
| `float_polynomial.bas` | 4 | 9252 | 2313 | Full QL ROM |
| `float_transcendentals.bas` | 5 | 1280 | 256 | Full QL ROM |

- Integer arithmetic exercises additions, multiplication and MOD with bounded values.
- Constant expressions is a deliberate optimizer microbenchmark. Keep its results separate from the representative kernels.
- Sieve counts the 168 primes through 1000. The reference checker uses trial division.
- Matrix multiplication computes eight by eight integer matrices and a weighted checksum.
- Bubble sort orders 64 deterministic values, checks ordering and uses a weighted checksum that depends on the sorted order. The reference uses the host sort.
- Procedure calls exercise reference scalar mutation and value arguments.
- Recursion evaluates Fibonacci at 12 and 13. The reference computes Fibonacci iteratively.
- String operations build and rotate a 64-character string, then check length and endpoint character codes.
- Dyadic floating work uses exactly representable increments and scaling by two.
- Polynomial floating work uses general multiplication and division. Its exact mathematical sum is 2313.0001875; the program checks a tolerance of 0.02 and an integer result of 2313.
- Transcendental work checks that SQRT(SIN(x)^2 + COS(x)^2) is within 0.00001 of one.

All integer intermediates fit checked 16-bit arithmetic for the supplied defaults and quick workloads. Thirteen workloads execute in the console test harness. The two full-ROM cases compile in both modes and have status `compiled-only`; their execution and numerical tolerances must be verified on the target.

The `regalloc_` cases deliberately isolate copy-chain and bounded-arithmetic
register reuse. Report them separately from the original general workloads;
they demonstrate capability rather than predicting application speedups. Their
checksums use independent closed-form calculations in the reference checker.

The two `native_` microbenchmarks isolate local constant propagation and arithmetic
result reuse. One builds constants through scalar assignments; the other repeats
the same DIV/MOD and addition before multiplying the results. Report their gains
separately from the general workloads: they exercise specific compiler capabilities
and do not predict application speedups. Their reference checksums use closed-form
values at the last iteration, independently of the compiler.

## Run the comparisons

From the repository root, run the quick check first:

```powershell
.\Benchmarks\Turbo\Run-Benchmarks.ps1 -Quick
```

Then run the default workloads, reusing the build:

```powershell
.\Benchmarks\Turbo\Run-Benchmarks.ps1 -NoBuild
```

Select workloads when iterating:

```powershell
.\Benchmarks\Turbo\Run-Benchmarks.ps1 -NoBuild -Name sieve,bubble_sort
```

The wrapper builds the existing Debug SBTests project unless `-NoBuild` is supplied. It reuses the installed .NET SDK and invokes its F# Interactive directly. `-MaxSteps` defaults to 50 million instructions per execution; `-OutputDirectory` overrides the artifact destination. Unknown names, compile errors, wrong output, execution faults and instruction-limit failures cause a nonzero exit.

`Check-ReferenceChecksums.ps1` independently verifies the manifest checksums before each run without invoking SB or its runtime. Its formulas and alternative algorithms can also be run separately.

## Artifacts

The default destination is `artifacts/turbo-benchmarks`. Quick and default workloads have separate directories. Repeating the same run replaces its generated files rather than creating numbered copies.

Each workload and mode keeps:

- The source copy with its effective pass count.
- A QDOS executable named `bench_exe`, including its QDOS file header.
- A Turbo IR listing at `bench_exe.lst`.
- Original and optimized HIR dumps.
- Compiler diagnostics in `compiler.log`.

Each run exports `results.csv` and `results.json`, including both HIR modes, checksums, executable sizes, instruction counts and status. Output and zero job exit are checked before an execution is marked passed. Executable sizes include the 30-byte QDOS header. Instruction counts include startup, the final output, shutdown and host-handled ROM utility calls.

`InstructionWordsFetched` counts the 16-bit opcode and extension words consumed
by the functional CPU. Long immediates and address extensions count as two words.
Data reads and host-handled utility calls contribute no instruction fetches.
This distinguishes shorter encodings from fewer executed instructions: a byte
branch saves one fetched extension word each time it executes. It excludes
speculative prefetch and is not a measurement of hardware bus traffic or cycles.

`configuration.json` records the workload, host runtime, instruction limit, template library, code mode and SHA256 fingerprints of the compiler, harness, manifest and compiler configuration used for the run.

## Measure QL performance

The functional harness has no MC68008 cycle model. Its instruction-count reductions are diagnostic evidence, not QL acceleration measurements. Host wall-clock time is not reported as QL timing.

Use the saved executables on a real QL or a full QL emulator to obtain elapsed times. Record hardware or emulator version, clock or acceleration setting, ROM, runtime library, arithmetic policy, pass count and output channel. Compare the same workload and settings in both HIR modes, repeat runs, and keep console/file I/O outside the repeated kernel. Use `-CompareNative` for the independent native-region comparison described below; register allocation remains future work.

For longer target runs, adjust the manifest pass count, recalculate its expected checksum and compile again. Keep passes between 1 and 1000 to retain the documented integer bounds, and raise the functional instruction limit when needed. The quick mode changes only source line 20 to one pass; original source fixtures remain unchanged.
## Comparing audited native regions

Add `-CompareNative` to `Run-Benchmarks.ps1` to execute all four combinations of
HIR optimization and native-region selection. For example:

```powershell
.\Benchmarks\Turbo\Run-Benchmarks.ps1 -NoBuild -CompareNative -OutputDirectory C:\Source\codex-workspace\.scratch\turbo-native-regions\benchmarks
```

The normal two-mode benchmark explicitly disables native regions to retain the
threaded baseline. Reports include a `NativeRegions` column and record both
switches in the configuration. These remain functional instruction counts;
elapsed QL timings require the full emulator. See
[the native-region audit](../../SB/Optimization/TURBO-NATIVE-REGIONS.md) for eligibility and
checked-operation boundaries.
