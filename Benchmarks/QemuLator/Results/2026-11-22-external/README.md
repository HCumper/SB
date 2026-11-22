# Turbo FAST versus optimized SB: corrected external timings

Measured 22 November 2026 under Q-emuLator.

**All 72 unchanged executable jobs passed:** 18 warm-ups and 54 measured jobs. Each compiler/program pair has three measured samples. Values below are median whole-job seconds; lower is faster.

| Program | Turbo FAST (s) | Optimized SB (s) | Faster build |
| --- | ---: | ---: | --- |
| Integer arithmetic | 6.74 | 2.32 | SB 2.91x |
| Prime sieve | 3.88 | 2.26 | SB 1.72x |
| Matrix multiplication | 3.68 | 3.94 | Turbo 1.07x |
| String operations | 4.08 | 5.62 | Turbo 1.38x |
| Floating polynomial | 12.86 | 14.06 | Turbo 1.09x |
| SIN / COS / SQRT | 18.20 | 18.68 | Turbo 1.03x |
| Eight queens | 19.38 | 22.86 | Turbo 1.18x |
| Route planner | 36.10 | 44.38 | Turbo 1.23x |
| Text concordance | 49.66 | 56.92 | Turbo 1.15x |

The factors compare identical workloads. No aggregate score is assigned to this small, varied collection.

## What was repaired

Timing now runs entirely in the interpreted parent BASIC runner, immediately around `EW`. The compiler inputs preserve each original numbered program, including its normal checksum `PRINT` and `STOP`; only compile-time Turbo directives are prepended. There are no timing reads, POKE writes, completion-marker writes or self-build procedures in either executable. Turbo was built by genuine parser 5.09 and code generator 3.27 inside Q-emuLator, using immediate commands outside the program.

The normal `PRINT #1` output is captured through inherited QDOS channels. The collector validates the exact program/checksum output, workload, completed `EW`, positive ticks and host interval. All nine independent host reference checks and all 11 harness regression checks passed.

The interval includes executable loading, runtime startup, initialization, computation, internal checks, checksum output and shutdown. Parent output-file opening/closing and result reporting occur outside it. The identical parent EW launch/wait overhead is included; no estimated startup or wrapper cost is subtracted. The [older instrumented kernel-only results](../2026-11-22/README.md) are preserved for provenance; their scope differs and their instrumentation suppressed optimization. Differences between those numbers and this table cannot all be attributed to compiler optimization.

## Configuration and checks

- Q-emuLator 3.5.1, Original QL (`Speed=QL`), 640 KiB RAM, JS ROM and Toolkit II; graphics acceleration and sound disabled. The [Speed menu](qemulator-speed.png) was checked after completion.
- Turbo FAST (`TURBO_optim 2`), large addressing, no copied windows or line numbers, structured mode, 20 KiB dataspace and 350-byte base stack; Turbo Toolkit 3.44 and lib537.
- SB revision `48755e0`, `--hir-opt --native-regions --regalloc`, lib537; ordinary checked semantics and existing threaded fallbacks remain. SB reserves any additional native spill/recovery stack space.
- One warm-up and three measured trials per pair; order alternates by trial and program. Compilation at Full speed is excluded. The measured jobs use Original QL speed.
- External polling clock: 50 Hz, 0.02-second resolution. Calibration: 100 ticks, 2.020 host seconds for a 100-tick PAUSE. No guest interval exceeds its enclosing host interval beyond the 0.05-second allowance.
- Captured Turbo IR proves native FAST integer operations in every case. The compatibility generator reproduces all nine genuine Turbo executable payloads byte for byte.
- Every SB executable matches an independently compiled ordinary reference build byte for byte. Sources, executable hashes and runtime/configuration fingerprints were checked before and after timing.

These are elapsed times for this emulator configuration, not calibrated original MC68008 hardware timings. Turbo FAST can omit overflow checks; these successful workloads keep integer arithmetic within signed 16-bit limits.

## Repeated samples and executable sizes

| Program | Turbo range (s) | SB range (s) | Turbo bytes | SB bytes |
| --- | ---: | ---: | ---: | ---: |
| Integer arithmetic | 6.74–6.74 | 2.32–2.32 | 6,862 | 6,448 |
| Prime sieve | 3.86–3.88 | 2.26–2.26 | 8,012 | 7,398 |
| Matrix multiplication | 3.68–3.68 | 3.94–3.94 | 9,672 | 7,666 |
| String operations | 4.08–4.08 | 5.62–5.62 | 9,184 | 7,588 |
| Floating polynomial | 12.86–12.88 | 14.06–14.06 | 7,856 | 7,096 |
| SIN / COS / SQRT | 18.20–18.20 | 18.68–18.70 | 7,788 | 7,140 |
| Eight queens | 19.38–19.38 | 22.86–22.86 | 12,718 | 8,944 |
| Route planner | 36.10–36.10 | 44.36–44.38 | 13,192 | 9,180 |
| Text concordance | 49.66–49.68 | 56.92–56.92 | 17,920 | 12,012 |

Sizes include the 30-byte Q-emuLator file header. Both are ordinary executable programs, with no benchmark launcher or timing routine linked in.

## Ordinary SB optimization coverage

Coverage matches the independent ordinary builds in every row. Enabling the full pipeline does not force optimizations where the current compiler cannot prove eligibility.

| Program | Native FOR loops | Arithmetic proofs enabled | Proven arithmetic expressions | Proven array accesses |
| --- | ---: | --- | ---: | ---: |
| Integer arithmetic | 1 | yes | 4 | 0 |
| Prime sieve | 2 | yes | 1 | 6 |
| Matrix multiplication | 0 | yes | 7 | 6 |
| String operations | 0 | no | 0 | 0 |
| Floating polynomial | 0 | yes | 0 | 0 |
| SIN / COS / SQRT | 0 | no | 0 | 0 |
| Eight queens | 0 | no | 0 | 0 |
| Route planner | 0 | no | 0 | 0 |
| Text concordance | 0 | no | 0 | 0 |

The arithmetic and sieve programs now retain one and two native FOR loops respectively. The old timed builds selected none. Arithmetic proof counts are restored to four for integer arithmetic and seven for matrix multiplication.

## Why SB is still slower on seven programs

SB is 2.91x faster on integer arithmetic and 1.72x faster on sieve. The other
seven take about 3% to 38% longer than Turbo FAST. Enabling the full optimization
pipeline does not make every operation or loop native: its current eligibility
rules still exclude much of these workloads' repeated work.

- All seven slower programs select zero retained-counter native FOR loops.
  Native regions cover selected straight-line operations; control-flow targets,
  calls, floating arithmetic and strings return to threaded execution. Local
  register allocation ends at those boundaries, requiring values to be
  materialized and subsequently reloaded. Branches, nesting, local/reference
  bindings and calls prevent the current whole-loop selector from retaining
  counters through these more complex loops.
- Range-based arithmetic proofs are disabled in five programs: strings,
  transcendentals, queens, routes and concordance. Their analysis reports
  uncertain control, and the current certification rule withholds proofs
  program-wide. Unclassified function calls have conservative unknown effects;
  local/reference bindings and routine calls add further restrictions. Checked
  arithmetic and generic array paths consequently remain where the compiler
  cannot certify a cheaper operation. This does not disable every optimization.
- There is a specific string-lowering gap. In the repeated `text$ = text$ &
  piece$` operation, captured Turbo IR fetches `piece$` and uses opcode 374,
  the runtime's direct append operation. SB fetches both strings, concatenates
  them with opcode 84 and stores the result with opcode 264. The runtime append
  copies only the added text. SB's sequence creates an intermediate result and
  copies the existing text as well. The string benchmark takes 37.7% longer;
  concordance also repeatedly appends to tokens.
- Matrix multiplication uses two-dimensional arrays. The register-aware native
  array IR specializes one-dimensional integers; multidimensional accesses use
  generic helpers and interrupt register allocation. Matrix retains seven
  arithmetic proofs and six proven array accesses, so its 7.1% gap is not simply
  a failure to run range analysis.
- Floating arithmetic remains threaded in SB. Both compilers use QL floating
  operations; the differences here concern surrounding dispatch, loads and
  expression lowering. Transcendentals spend substantial work in shared math
  routines, consistent with their smaller 2.6% gap. Turbo FAST may also omit
  integer overflow checks that SB preserves unless a proof permits removal.

These are generated-code findings and explanations consistent with the timings,
not a measured breakdown of time by cause. In particular, the results do not
establish how much each remaining check, mode bridge or helper call costs.
Reproducing all nine genuine Turbo payloads from their captured IR shows that
the compatibility generator can produce the same machine code when given the
same IR; the main opportunities are in SB's IR lowering and native coverage.

The next focused changes should be a checked direct string-append lowering,
more precise effect/control certification for known operations and safe scopes,
and native loops with conditional bodies. Multidimensional register-aware array
accesses and audited native floating arithmetic follow. These changes should
preserve bounds, alias, overflow and error behavior rather than globally dropping
checks to imitate FAST mode.

## Evidence and reproduction

[Harness and reproduction steps](../../README.md), [raw samples](samples.csv), [summary](summary.csv), [configuration and hashes](configuration.json), [run validation](result-state.json), [SB ordinary-build verification](sb-verification.json), [Turbo native verification](native-verification.json), [independent reference checks](reference-checks.json), [harness tests](harness-tests.txt), [workload plan](plan.json).

Complete local guest artifacts, ordinary reference builds, captured IR, compiler reports and stdout files: `C:\Source\codex-workspace\.scratch\turbo-sb-qemulator-external\final`.
