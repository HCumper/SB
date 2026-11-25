# Turbo FAST versus SB in Q-emuLator

[Compiling Turbo's own parser: Turbo 94.9 s at GoldCard speed, SB 15.8 s on a PC, 24 November 2026](Results/2026-11-24-turbo-parser-compile/README.md)

[Whole collection at GoldCard speed: SB faster than Turbo FAST on all nine workloads, and keeping the emulator in real time, 24 November 2026](Results/2026-11-24-goldcard-whole-collection/README.md)

[Direct RI.EXEC calls for ROM maths functions: float transcendentals 12.1% ahead of Turbo FAST, 24 November 2026](Results/2026-11-24-direct-rom-maths/README.md)

[In-place string stores, CODE and LEN: concordance 32.1% ahead of Turbo FAST, 24 November 2026](Results/2026-11-24-in-place-string-stores/README.md)

[In-place string comparison operands: concordance 26.8% fewer instructions, 28.6% ahead of Turbo FAST, 24 November 2026](Results/2026-11-24-in-place-string-comparisons/README.md)

[Terminal STOP branches inside native loops: all nine workloads now ahead of Turbo FAST on instruction counts, 24 November 2026](Results/2026-11-24-terminal-stop-branches/README.md)

[Inline FOR stepping in memory-counter native loops: instruction counts for all nine workloads and full verification, 24 November 2026](Results/2026-11-24-inline-for-stepping/README.md)

[Native string operations, ROM maths calls and loop EXIT: instruction counts for all nine workloads and full verification, 24 November 2026](Results/2026-11-24-native-strings-rom-maths/README.md)

[Fused conditions, native integer ABS and recursive parameter ranges: N-queens at GoldCard speed, profiles and full verification, 24 November 2026](Results/2026-11-24-fused-conditions-abs-ranges/README.md)

[Memory-based native recursive calls: N-queens at GoldCard speed and full verification, 23 November 2026](Results/2026-11-23-native-recursion/README.md)

[Native control flow for string and floating-point loop bodies: timings, instruction reductions and full verification, 23 November 2026](Results/2026-11-23-native-template-loops/README.md)

[Profiles of the five remaining slower workloads: instruction attribution, fresh timings and next priorities, 23 November 2026](Results/2026-11-23-workload-profiles/README.md)

[Invariant native array rows: matrix and route timings and verification, 23 November 2026](Results/2026-11-23-invariant-arrays/README.md)

[Direct comparisons, integer truth and eager AND/OR branches: timings and verification, 23 November 2026](Results/2026-11-23-direct-conditions/README.md)

[Procedure and region eligibility: eight-queens timing and verification, 23 November 2026](Results/2026-11-23-scoped-eligibility/README.md)

[Native integer comparisons and conditional FOR loops: sieve and route timing, 23 November 2026](Results/2026-11-23-native-branches/README.md)

[Certified procedure-local native FOR loops: route timing and verification, 23 November 2026](Results/2026-11-23-local-for/README.md)

[Procedure-entry array shapes: route-planner timing and verification, 22 November 2026](Results/2026-11-22-call-entry-shapes/README.md)

[Rank-two native arrays: matrix timing and full verification, 22 November 2026](Results/2026-11-22-rank-two/README.md)

[Full collection timings with direct string append, 22 November 2026](Results/2026-11-22-string-append/README.md)

[External-timing baseline before string append](Results/2026-11-22-external/README.md)

This collection times unchanged executable jobs built by genuine Turbo 5.09
FAST and fully optimized SB, running under QDOS in Q-emuLator. It includes six
small kernels and three larger original programs: an exhaustive eight-queens
solver, a road-network route planner and a text concordance.

The [superseded instrumented results](Results/2026-11-22/README.md) remain for
provenance. Their source-level POKE timing writes suppressed SB's native FOR
selection throughout each program and reduced arithmetic-proof coverage. The
external harness removes those writes and all other instrumentation from the
compiled programs. Whole-job timings have a different scope from the old
kernel-only measurements.

The programs are deterministic and need no input files. The larger programs
check queen placements, predecessor-chain distances, ordering and word-count
conservation. `Check-References.py` independently checks all nine using methods
including bitboards, Floyd-Warshall and Python tokenization/counting/sorting.

| Program | Passes | Source lines | Checksum |
| --- | ---: | ---: | ---: |
| Integer arithmetic | 20 | 16 | 6430 |
| Prime sieve | 5 | 28 | 840 |
| Matrix multiplication | 10 | 33 | 1094 |
| String operations | 80 | 22 | 880 |
| Floating polynomial | 4 | 20 | 9252 |
| Floating transcendental functions | 5 | 18 | 1280 |
| Eight queens | 1 | 72 | 4962 |
| Route planner | 1 | 97 | 4701 |
| Text concordance | 4 | 104 | 7936 |

The kernels derive from `../Turbo`, with unsuffixed integer names declared by
`IMPLICIT%` so integer FOR loops pass through QDOS's BASIC editor. Floating
divisions explicitly read floating variables to avoid Turbo selecting integer
division for integer-valued operands. The string test declares its temporary
string and repeats enough work to time. The three larger programs are original
algorithmic workloads rather than interactive productivity applications.

## Reproduce

Build SB from the desired revision first. Use Python 3, .NET, an installed
Q-emuLator, Turbo 5.09 with Q-emuLator-headered `parser_task` and `codegen_task`,
and raw Turbo Toolkit 3.44 code. ROM defaults are the installed JS and Toolkit II
ROMs; preparation arguments allow alternative locations.

```powershell
$run = 'C:\Source\codex-workspace\.scratch\turbo-sb-qemulator-reproduce'
python .\Prepare-Benchmarks.py --output $run --turbo-directory I:\QL\Turbo --toolkit 'C:\Source\codex-workspace\ql-records-demo\turbo_tk'
.\Build-SB.ps1 -RunDirectory $run
python .\Check-References.py --output "$run\reference-checks.json"
python .\Test-Harness.py
```

1. Open the generated `compile.QCF` in Q-emuLator. The boot file installs the
   toolkit and loads the first unchanged program. Compilation uses Full speed.
2. Enter the following as **immediate BASIC commands**, without line numbers.
   They are never inserted into the program or executable:

   ```text
   EXECUTE_A mdv1_parser_task;"*"
   SBYTES mdv1_b00_ir,PEEK_L(PEEK_L(164192)+36),65536:EW mdv1_codegen_task
   ```

   Wait for parsing to finish before issuing the second command. For each next
   case, `LOAD mdv1_b01_bas` (then b02 through b08) and repeat those two commands,
   changing the capture name to `mdv1_b01_ir`, etc. `compile-commands.json`
   contains the exact commands and names for the selected collection. Each
   `drive\bNN_report` must report zero errors and successful code generation.
   There is no self-build launcher in the executable.
3. Run `dotnet fsi --exec .\Inspect-Turbo.fsx $run`. This verifies native FAST
   integer code and byte-for-byte agreement of each genuine Turbo code payload
   with the compatibility generator. Run `.\Use-TimingBoot.ps1 -RunDirectory
   $run`; it checks all builds and both compilers' verification evidence before
   installing the timing boot file.
4. Open `benchmark.QCF` in Q-emuLator. This starts a fresh session at **GoldCard**
   speed (`Speed=GoldCard`), with 640 KiB RAM, JS ROM, Toolkit II, unaccelerated
   graphics and sound disabled. Leave that setting unchanged until completion.
5. Wait for `drive\done`, then run:

   ```powershell
   python .\Collect-Results.py $run --complete
   .\Save-Metadata.ps1 -RunDirectory $run
   ```

Preparation refuses directories containing compiled jobs or timing samples. Generated QDOS
files use LF endings. No files in the installed Turbo distribution are changed.

Future timing runs default to GoldCard speed, as requested on 23 November 2026.
Use `Prepare-Benchmarks.py --speed QL` only when reproducing historical Original QL
runs. The plan and metadata record the chosen speed; compare timings collected at
the same speed. Compilation continues to use Full speed and is excluded from timing.

## Measurement and validation

An interpreted parent BASIC runner reads a common resident 50 Hz polling clock
immediately before and after `EW`, which waits for the executable job to end.
The callback counts deferred polls as well. No compiled source contains timing
reads, writes, completion markers or build-launcher code. Apart from compile-time
Turbo directives, each compiler's input is the exact original numbered program,
including its normal checksum `PRINT` and `STOP`.

The interval includes executable loading, runtime startup, initialization,
computation, internal checks, checksum printing and shutdown. Opening and closing
the output file and writing harness reports are outside it. These whole-job
times must not be directly equated with the old kernel-only results. The same
parent EW launch/wait overhead is included for both compilers; no estimated
startup or wrapper cost is subtracted.

Library 5.37 fills inherited channels backwards from BASIC channel #31. Passing
31 references to a single output-file channel makes the unchanged program's
normal `PRINT #1` reach that file. Both compilers use the same channel setup.
The collector requires an exact `program:checksum` output, the expected workload,
return from `EW`, positive ticks and an enclosing host timestamp. Runtime errors,
missing or duplicate results, extra output and incorrect checksums fail validation.
Old instrumented-format results are rejected.

A sample also fails when its host job time exceeds its emulated time by more
than 2% plus 0.05 s (`behindRealTime`). The emulator has then fallen behind
real time, and its emulated times are inflated as well. Windows 11 can throttle
a hidden or background emulator this way: see the
[whole-collection GoldCard result](Results/2026-11-24-goldcard-whole-collection/README.md),
whose run script opts the emulator out of power throttling.

One warm-up and three measured trials run per compiler/program pair, alternating
order by trial and program: 72 jobs, of which 54 are measured. A 100-tick PAUSE
checks clock calibration, and host file-creation timestamps enclose every job.
Resolution is 0.02 seconds. Samples, medians and ranges are retained. These are
results for the recorded Q-emuLator configuration, not calibrated MC68008 hardware
timings.

`Build-SB.ps1` uses `--hir-opt --native-regions --regalloc`. It independently builds
each original program with the same compile-time metadata, requires an identical
complete executable, and compares native FOR and arithmetic/array proof coverage.
The ordinary arithmetic build selects one native FOR loop; sieve selects three,
matrix multiplication selects three, route planner selects five and eight queens
selects four. The other four currently select none.
Compiler checks and threaded fallbacks remain where required. Turbo FAST can
omit overflow checks; supplied successful
workloads keep integer arithmetic within signed 16-bit limits.

`Test-Harness.py` checks untouched inputs, failure detection (including samples
behind real time), old-format rejection and refusal to overwrite completed samples. `pollclock.asm` and `pollclock.hex`
provide the external timer. Captured IR, native-mode evidence, ordinary-build
verification, sources, reports, executable hashes, raw output and configuration
fingerprints are retained in the run directory.

Turbo's FAST mode is described in the
[updated Turbo manual](https://sinclairql.net/djw/docs/ebooks/QL%20Turbo%20Manual%20-%20Simon%20N.%20Goodwin.pdf).
The compiler distribution is available from the
[Turbo archive](https://sinclairql.net/djw/turbo/index.html).
