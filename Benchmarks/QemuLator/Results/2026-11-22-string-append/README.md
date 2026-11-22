# Direct string append: Q-emuLator comparison

Measured 22 November 2026. All 72 unchanged executable jobs passed: one warm-up and three measured trials per compiler/program pair. Values are median whole-job seconds at Original QL speed; lower is faster.

| Program | Turbo FAST (s) | Previous SB (s) | SB with append (s) | SB time change |
| --- | ---: | ---: | ---: | ---: |
| Integer arithmetic | 6.74 | 2.32 | 2.32 | +0.0% |
| Prime sieve | 3.88 | 2.26 | 2.26 | +0.0% |
| Matrix multiplication | 3.68 | 3.94 | 3.94 | +0.0% |
| String operations | 4.08 | 5.62 | 4.50 | -19.9% |
| Floating polynomial | 12.86 | 14.06 | 14.06 | +0.0% |
| SIN / COS / SQRT | 18.22 | 18.68 | 18.70 | +0.1% |
| Eight queens | 19.38 | 22.86 | 22.86 | +0.0% |
| Route planner | 36.08 | 44.38 | 44.36 | 0.0% |
| Text concordance | 49.68 | 56.92 | 56.50 | -0.7% |

The previous SB values come from the [external-timing baseline](../2026-11-22-external/README.md). Both runs time whole jobs, including loading, startup, checksum printing and shutdown, with the same external runner. No startup or wrapper cost is subtracted. This is separate from the older instrumented kernel-only measurements.

## Change and semantics

Simple `text$ = text$ & suffix` assignments now evaluate a stable suffix and emit Turbo instruction 374. This copies only the text being appended and truncates to the current descriptor capacity. The checked threaded runtime retains missing-string errors. The suffix is evaluated even when the destination is full, and failures leave the destination unchanged. No combined-length overflow check was added: Turbo append has none.

Compiled or replaceable resident function calls in the suffix retain the old prefix snapshot and general concatenation. Aliases, local/reference bindings, bounds/division errors and repeated RETRY are covered by execution tests. See [the lowering design](../../../../SB/Optimization/TURBO-STRING-APPEND.md).

Only the SB executables for string operations and concordance changed. The other seven medians differ from the baseline by at most one 0.02-second clock tick. The other seven SB binaries are byte-for-byte identical to the baseline. All nine genuine Turbo FAST binaries are also identical to that baseline; they were reused after source/hash verification, rechecked against their captured native IR, and executed again in this run.

## Verification

- SBTests.dll: 2841 passed, 0 failed, 0 skipped (2841 total).
- SBRuntimeTests.dll: 248 passed, 0 failed, 0 skipped (248 total).
- All nine independent reference checks and all 11 benchmark harness regression checks passed.
- Every SB executable matches an independent ordinary build, including native-loop and range-proof coverage.
- All nine Turbo FAST payloads are reproduced byte for byte by the compatibility generator from their captured IR.
- The external 50 Hz clock calibrated at 100 ticks over 2.020 host seconds. Every job has validated checksum output and enclosing host timestamps.

## Configuration

Q-emuLator 3.5.1, Original QL (`Speed=QL`, [verified in the Speed menu](qemulator-speed.png)), 640 KiB RAM, JS and Toolkit II ROMs, graphics acceleration and sound disabled. Turbo parser 5.09 / codegen 3.27 / library 5.37 with FAST (`TURBO_optim 2`), large addressing, no copied windows or line numbers, structured mode, 20 KiB dataspace and a 350-byte base stack. SB uses `--hir-opt --native-regions --regalloc`, retaining existing checked behavior.

The measurements used commit `20ffa01` plus the direct-append implementation recorded with these results. The exact compiler binary, source patch and executable hashes are recorded below. Results describe this emulator configuration, not calibrated original MC68008 hardware timings.

## Evidence

[Samples](samples.csv), [summary](summary.csv), [configuration](configuration.json), [pre-run configuration](configuration-before-run.json), [run validation](result-state.json), [suite summary](suite-summary.json), [suite log](full-suite.log), [source patch](compiler-change.patch), [SB ordinary-build verification](sb-verification.json), [Turbo native verification](native-verification.json), [Turbo provenance](turbo-baseline.json), [reference checks](reference-checks.json), [harness tests](harness-tests.txt), [plan](plan.json).

Full executables, compiler dumps, captured IR, outputs and verification files: `C:\Source\codex-workspace\.scratch\turbo-string-append\qemulator`.
