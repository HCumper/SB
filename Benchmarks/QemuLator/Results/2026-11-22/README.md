# Turbo FAST versus SB: Q-emuLator timings

Measured 22 November 2026.

> **Superseded:** [Corrected external timings](../2026-11-22-external/README.md) now measure unchanged executable jobs with normal SB optimization coverage. The historical measurements below remain unchanged.
> **Benchmark review correction:** The timer's in-source `POKE_W`/`POKE_L` result writes trigger SB's program-wide native-loop safety guard. Every timed SB build selected zero retained-counter native FOR loops. Compiling without instrumentation selects one in integer arithmetic and two in the sieve; the other seven workloads still select zero. Timer reads also reduce proven arithmetic expressions from four to one in integer arithmetic and seven to five in matrix multiplication. These are valid timings of the instrumented binaries, but the comparison is provisional for normal optimized SB output. Rerun with timing outside the compiled source before drawing definitive compiler-performance conclusions. See the [coverage audit](optimization-coverage-review.json). The measurements below have been preserved unchanged.

Nine deterministic SuperBASIC programs, including an exhaustive eight-queens solver, a 32-town route planner and a text concordance.

**All 72 executions passed:** 18 warm-ups and 54 timed executions. Each compiler/program pair has three measured samples. The table reports median elapsed seconds; lower is faster.

| Program | Passes | Turbo FAST (s) | SB (s) | Faster build |
| --- | ---: | ---: | ---: | --- |
| Integer arithmetic | 20 | 6.46 | 4.58 | SB 1.41x |
| Prime sieve | 5 | 3.60 | 3.34 | SB 1.08x |
| Matrix multiplication | 10 | 3.40 | 3.70 | Turbo 1.09x |
| String operations | 80 | 3.80 | 5.36 | Turbo 1.41x |
| Floating polynomial | 4 | 12.58 | 13.78 | Turbo 1.10x |
| SIN / COS / SQRT | 5 | 17.92 | 18.40 | Turbo 1.03x |
| Eight queens | 1 | 19.06 | 22.56 | Turbo 1.18x |
| Route planner | 1 | 35.74 | 44.04 | Turbo 1.23x |
| Text concordance | 4 | 49.26 | 56.54 | Turbo 1.15x |

The factors compare throughput for the identical workload: 1.40x means the faster build completes that workload in 1/1.40 of the time. No aggregate score is assigned to this small, deliberately varied collection.

## Configuration

- Q-emuLator 3.5.1; **QL speed**, 640 KiB RAM, JS ROM and Toolkit II ROM; accelerated graphics and sound disabled.
- Genuine Turbo parser 5.09, code generator 3.27 (generator 4.10 / lib537), Turbo Toolkit 3.44. `TURBO_optim 2` selects **FAST native/in-line code**; large addressing, no copied windows and no line-number instrumentation.
- SB revision `48755e0`, using `--hir-opt --native-regions --regalloc` and lib537. Existing threaded fallbacks and checked error handling are retained.
- Both have 20 KiB dataspace and a 350-byte base CPU stack. SB reserves any additional native spill/recovery space it needs.

The Turbo builds were produced by Turbo running inside Q-emuLator. Their captured IR has native integer arithmetic, with zero threaded integer arithmetic records in all nine builds. SB's compatibility generator reproduces all nine original Turbo code payloads byte for byte. This provides an additional check on the recorded library and code-generation settings; the benchmark uses the original Turbo executables.

## Method and limits

A common 50 Hz resident polling callback times initialization, computation, internal result checks and checksum generation. It excludes executable loading, runtime startup, result-file writes and shutdown. Native Turbo and SB receive identical instrumented kernels. Run order alternates by trial and by program.

The calibration PAUSE measured 101 guest ticks (2.02 seconds) in 2.019 seconds of host time. Guest kernel times were also checked against host timestamps enclosing each job. Clock resolution is 0.02 seconds.

These are elapsed times in this Q-emuLator configuration. They are not estimates from SB's functional test CPU and are not calibrated original MC68008 hardware timings. The three larger programs are algorithmic workloads, not full interactive applications. Console and disk throughput are outside the timed interval.

Turbo FAST can omit arithmetic overflow checks; the benchmark inputs keep integer operations within signed 16-bit limits. SB retains its normal checked semantics. The Turbo self-build launcher is outside the timed interval. Because that launcher adds untimed code, recorded binary sizes should not be used as a fair application-size comparison.

The pilot identified Turbo integer-expression division in the original floating kernels. Final sources materialize a floating variable before division; both corrected polynomial builds pass the numerical tolerance, and the trigonometric builds execute a floating divide. The failed pilot is excluded from this report.

## Sample ranges

| Program | Turbo min-max (s) | SB min-max (s) |
| --- | ---: | ---: |
| Integer arithmetic | 6.46-6.46 | 4.58-4.58 |
| Prime sieve | 3.60-3.60 | 3.34-3.34 |
| Matrix multiplication | 3.40-3.42 | 3.68-3.70 |
| String operations | 3.80-3.82 | 5.34-5.36 |
| Floating polynomial | 12.58-12.58 | 13.78-13.78 |
| SIN / COS / SQRT | 17.90-17.92 | 18.40-18.40 |
| Eight queens | 19.04-19.06 | 22.56-22.56 |
| Route planner | 35.72-35.74 | 44.02-44.04 |
| Text concordance | 49.26-49.28 | 56.54-56.54 |

## Evidence

- [Raw samples](samples.csv), [summary](summary.csv), and [validation state](result-state.json).
- [Configuration and SHA256 fingerprints](configuration.json), [native IR verification](native-verification.json), and [independent reference checks](reference-results.json).
- [Programs and reproduction scripts](../../README.md).

Full generated sources, Turbo compilation reports, executable files, captured IR and HIR/native allocation diagnostics are retained under `C:\Source\codex-workspace\.scratch\turbo-sb-qemulator\final`.
