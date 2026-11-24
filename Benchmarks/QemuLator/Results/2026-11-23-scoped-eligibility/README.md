# Procedure and region eligibility for native FOR loops

Implemented 23 November 2026. Local uncertainty no longer disables native-loop
proofs throughout the program. Arithmetic/array proofs are certified per scope;
binding certificates additionally validate each individual FOR region.

## Implementation and safeguards

The CFG records each range FOR's initializer, check, advance and body members.
Its own initializer must dominate and own the captured control, and outside
edges may only enter through that initializer. Counter ownership, aliases,
escapes, body effects, checked arithmetic, stack recovery and machine-level
entry/branch validation retain their existing rules. A safe loop beside a local
GOSUB can qualify even when its scope is ineligible for range proofs.

Local GOSUB contexts, enumerated ranges and overwritten continuations withhold
arithmetic/array proofs in the affected scope. Independent procedures remain
eligible. Computed/missing or cross-scope line transfers, error handlers and
resumption, raw memory and opaque residents retain program-wide barriers.
This does not enable calls, recursion or nested loops inside a native body.

Global-array entry facts still require every call site. An untrusted caller
contributes UNKNOWN shapes, including at sites whose reachability it cannot
certify. It is never dropped from a callee's MUST facts. Trusted independent
callers/callees can propagate shapes; bounded fixed-point failure still discards
the imported facts. All occurrences of each expression/access key must be safe.

The existing ROM ABS helper receives a narrow nonmutating effect summary.
Arguments, coercion/allocation failure and its call boundary remain modeled;
user-defined ABS routines retain their actual summaries. Codegen independently
requires the exact ABS resident name, result/argument descriptors and unique
linkage before allowing that outside call beside a certified loop. Other ROM
calls and byte-only generation retain their exclusions. ABS inside a native
body remains unsupported.

HIR diagnostics now list eligible scopes, global barriers and rejected loop
bindings. Native diagnostics give each rejected loop's IR offset, counter vector
and failed machine checks. The serialized IR format, legacy peephole pass, runtime
library and optimization switches are unchanged. See
[the analysis design](../../../../SB/Optimization/HIR-ANALYSIS.md) and
[native-loop rules](../../../../SB/Optimization/TURBO-NATIVE-LOOPS.md).

## External eight-queens timing

The original recursive eight-queens program and complete executables ran
unmodified under QDOS at Original QL speed. Each compiler had one warm-up and
three measured whole-job trials, alternating order. All eight jobs returned
checksum 4962, with polling-clock calibration and enclosing host times validated.
Its normal ROM ABS calls execute in this full-emulator run.

| Build | Median seconds | Range | Executable bytes |
| --- | ---: | ---: | ---: |
| Turbo FAST | 19.34 | 19.34–19.36 | 12,718 |
| Current SB | 22.78 | 22.78–22.80 | 9,030 |
| Previous SB, earlier run | 22.86 | 22.86–22.88 | 8,944 |

Current SB takes 17.79% more time than Turbo FAST. The previous SB value
comes from the [earlier full collection](../2026-11-22-string-append/README.md)
and was not freshly retimed. Its executable remained byte-identical through the
preceding local-loop and branch changes. This elapsed comparison combines the
new scope/region eligibility and precise ABS effects; it does not isolate their
individual contributions.
The newly selected loops cover initialization and result checking. The hot
recursive search remains threaded; selecting more loops alone does not imply
a substantial whole-program acceleration.

Eight queens now selects three native FOR loops, previously zero: its result
checksum loop and the two occupancy-map initialization loops. It gains three
arithmetic proofs and eight proven rank-one array accesses. Its main graph falls
from 202 to 115 edges after removing ABS's spurious arbitrary mutation effects.
Four loops remain threaded, including the recursive search and nested board
validation. The ordinary executable grows by 86 bytes (0.96%).

All nine ordinary SB builds and genuine Turbo FAST payloads were reverified.
Only eight queens changes; the other eight SB binaries are byte-identical to the
immediate previous compiler and were not retimed. Turbo builds were reused only
after canonical source and compiler-input hashes matched. Elapsed intervals
include loading, startup, computation, checksum PRINT and shutdown; parent file
operations/reporting are excluded and no overhead is subtracted. The filtered
runner retains the original b06 calls and nonconsecutive result IDs.

## Isolated functional comparison

Both compilers built identical sources with all optimizations enabled, the same
task names and console startup. Complete executables ran unmodified and returned
independently calculated checksums.

| Workload | Previous instructions | Current instructions | Reduction | Bytes before → after |
| --- | ---: | ---: | ---: | ---: |
| Scoped local-loop microbenchmark | 748,819 | 389,175 | 48.03% | 6,808 → 6,856 |
| Straight-line local-loop microbenchmark | 389,115 | 389,115 | 0.00% | 6,846 → 6,846 |
| Conditional local-loop microbenchmark | 515,925 | 515,925 | 0.00% | 6,918 → 6,918 |
| Route planner | 3,864,403 | 3,864,403 | 0.00% | 9,650 → 9,650 |
| Sieve | 348,439 | 348,439 | 0.00% | 7,558 → 7,558 |
| Bubble sort | 430,242 | 430,242 | 0.00% | 7,712 → 7,712 |

The new `scoped_integer_for` fixture repeats the existing LOCAL-counter MOD
calculation beside a separate GOSUB with a normal return. Its checksum is 270
(quick workload 3004), independently derived from MOD-7 cycles. It isolates
eligibility localization without ABS. Fetch words fall from 1,031,274 to 491,774
(52.31%). This is a microbenchmark, not an application-wide prediction. The
functional harness cannot link the eight-queens ROM calls; elapsed Q-emuLator
validation covers that program. Functional counts do not model MC68008 cycles.

## Full SB test suite

`dotnet test SB.sln --no-build --no-restore` passed 3,664 tests: 3,416
compiler tests and 248 runtime tests, with no failures or skips. The final
focused analysis/effects/native-loop/CLI run passed 322 overlapping checks.

There are 62 additional regression cases. Coverage includes independent
procedures, a safe region beside local GOSUB uncertainty, enumerated ranges,
outside entries, cross-scope/computed targets, handlers, opaque memory/functions,
counter effects, untrusted caller shape joins, ABS/user-routine distinctions,
exact resident descriptors, rejection diagnostics and six-mode CLI execution.
One ABS integration case is compile-only because the minimal CPU cannot link
that ROM resident; the original eight-queens job exercises it under Q-emuLator.

## Comparative benchmark suite

All 96 functional executions passed across six HIR/native/register modes;
12 ROM-dependent builds compiled successfully. Independent reference calculations
passed for all 18 functional fixtures and all nine Q-emuLator programs. The
ordinary Turbo peephole optimization remains enabled in every mode.

## Provenance and evidence

Base revision `0b2cb4093f2cf0366815131731e26ed93c76cc9b` plus the archived combined
compiler/test patch. It includes the pending procedure-local loop and conditional
branch changes; the separate baseline patch records the immediate pre-eligibility
source. Baseline compiler SHA256:
`506311D79644B464281830F0176154A93FFB24CC8D336D59506676B3BFA30915`.
Current compiler SHA256: `846E82B26AE3692E091975525588AADAC401AD8F105D687CE9B2866361C26A47`.

Q-emuLator 3.5.1, [Original QL configuration](benchmark.QCF), 640 KiB RAM, JS and
Toolkit II ROMs, graphics acceleration and sound off. Turbo parser 5.09 / codegen
3.27 / lib537, FAST, large addressing, no copied windows or line numbers,
structured, 20 KiB dataspace and 350-byte base stack. SB uses
`--hir-opt --native-regions --regalloc`. These are emulator-configuration results,
not calibrated original-hardware measurements.
The installed emulator loaded this configuration through its command line;
the process command line and configuration fingerprint are archived in the
[launch record](emulator-launch.json).

[Samples](samples.csv), [timing summary](summary.csv), [configuration](configuration.json),
[pre-run configuration](configuration-before-run.json), [validation](result-state.json),
[plan](plan.json), [runner](runner_bas), [suite summary](suite-summary.json),
[full-suite log](full-suite.log), [focused log](focused.log),
[benchmark run log](benchmarks.log),
[combined source patch](compiler-change.patch), [baseline source patch](baseline-source.patch),
[binary comparison](binary-comparison.json), [ordinary SB verification](sb-verification-all.json),
[Turbo native verification](native-verification-all.json), [baseline provenance](baseline-provenance.json),
[functional comparison](execution-comparison.json), [queens analysis](queens-analysis.txt),
[queens native IR](queens-native-ir.txt), [six-mode results](six-mode-results.csv),
[six-mode configuration](six-mode-configuration.json), [functional manifest](suite.json),
[reference checker](Check-ReferenceChecksums.ps1), [independent QL references](reference-checks.json),
[scoped benchmark source](scoped_integer_for.bas),
[baseline invocation](Invoke-Baseline.fsx), [comparison build script](Build-Comparisons.ps1),
[comparison execution script](Count-Comparisons.fsx).

Reusable current binaries, the preserved compiler and raw diagnostics remain in
`C:\Source\codex-workspace\.scratch\turbo-local-eligibility`; the elapsed run
is its `qemulator-scopes` subdirectory. Changes remain uncommitted.
