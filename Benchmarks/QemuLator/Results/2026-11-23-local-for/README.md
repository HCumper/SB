# Certified procedure-local native FOR loops

Implemented 23 November 2026. Native integer FOR selection now accepts certified
procedure/function LOCAL counters and independent global counters in programs
with other routine bindings. The body remains straight-line and call-free.

## Implementation

HIR certificates require an owned integer LOCAL or uniquely named global counter,
resolved body effects, no counter stores, no other read/write root that may alias
the counter, and no escaped counter reference. Formal parameters may supply start,
END and STEP values; formal body accesses remain conservative. The existing global
CFG/range certification gate remains in place, as do opaque-memory and error
continuation barriers. This does not yet enable branching or recursive-call bodies.

The emitter maps each proof to its body label, counter vector and allowed scalar
vectors, including separately emitted flexible-argument ABI variants. Proofs remain
in memory, outside the serialized Turbo IR and its listing. Byte-only codegen and
unannotated programmatic generation keep the original conservative eligibility.
The machine-code planner independently checks operand coverage, supported body
instructions, references, stack and branch budgets.

The runtime initializer, word-wrap NEXT semantics, captured END/STEP, D2-D4 save
protocol, counter writeback and error cleanup are unchanged. No values remain in
registers across calls. See [native loops](../../../../SB/Optimization/TURBO-NATIVE-LOOPS.md)
and [the analysis design](../../../../SB/Optimization/HIR-ANALYSIS.md).

## External Q-emuLator timing

The original route planner still returns checksum 4701. One warm-up and three
measured whole-job trials ran for each compiler at Original QL speed, alternating
order. All eight unchanged executable jobs passed, including timer calibration
and enclosing host-time validation.

| Build | Median seconds | Range | Executable bytes |
| --- | ---: | ---: | ---: |
| Turbo FAST | 36.06 | 36.06–36.06 | 13,192 |
| SB with local-loop certificates | 32.18 | 32.18–32.18 | 9,542 |
| Previous SB, from the earlier run | 32.58 | See earlier report | 9,446 |

Current SB takes 10.8% less time than Turbo FAST. Its time is
1.2% below the [previous SB median](../2026-11-22-call-entry-shapes/README.md).
That previous value was not freshly retimed. The deterministic comparison below
uses the preserved immediate baseline compiler and isolates this implementation.

The interval includes executable loading, startup, initialization, computation,
checksum PRINT and shutdown. Parent file operations/reporting are excluded, and
no overhead is subtracted. Neither source nor executable contains instrumentation.
The original eight b07 calls and their nonconsecutive result IDs were retained
from the full runner without altering the timing procedure or call arguments.

All nine ordinary SB builds were independently verified. Only route planner's
binary changes: the other eight, including eight queens, remain byte-identical.
They were not timed again. Genuine Turbo FAST jobs were reused only after exact
canonical/compiler-input hash checks; all nine native payloads were reverified
against their captured IR.

## Functional comparisons against the previous compiler

Both compilers built unchanged sources with all optimizations enabled and the same
task names/default console startup. Complete executables ran unmodified in the
functional 68000 harness and returned independently calculated checksums.

| Workload | Previous instructions | Current instructions | Reduction | Bytes before → after |
| --- | ---: | ---: | ---: | ---: |
| Route planner | 6,019,267 | 5,954,147 | 1.08% | 9,548 → 9,644 |
| Procedure-local loop microbenchmark | 748,759 | 389,115 | 48.03% | 6,798 → 6,846 |

Route instruction words fetched fall from 7,997,184 to 7,897,840 (1.24%).
The microbenchmark falls from 1,031,199 to 491,699 words (52.32%). These are
functional counts, not calibrated MC68008 cycle or elapsed-time estimates.
Default console startup explains the 102-byte difference from ordinary route jobs.
Eight queens needs QDOS console services beyond this minimal harness; its ordinary
binary remains unchanged and its existing full-emulator result is retained.

Route planner selects three native loops, previously zero: its checksum loop,
network-row initialization and shortest-route array initialization. Eight loops
remain rejected. Its six arithmetic proofs, 15 proven array accesses and five
rank-two access facts are unchanged. Ordinary route code grows by 96 bytes (1.0%).
The important conditional search/relaxation loops remain future work.

The new `local_integer_for` microbenchmark executes 20 procedure calls, each with
a LOCAL counter ranging to a parameter-supplied 1000. It accumulates `i MOD 7`
using variable-dependent integer arithmetic. An independent cycle-sum calculation
gives checksum 270 (quick workload 3004). This isolates the new capability and
must not be presented as an expected application-wide speedup. Very short loops
can pay more register-save/setup overhead than they recover.

## Full SB test suite

`dotnet test SB.sln --no-build --no-restore` passed all 3,446 tests: 3,198
compiler tests and 248 runtime tests, with no failures or skips. The compiler and
test assemblies were freshly built before this final run.

There are 94 new regression cases. Coverage includes private/global shadows,
repeated and recursive invocations, empty and descending loops, captured mutable
bounds/steps, formal-supplied bounds, both flexible-argument ABI variants, integer
and rank-two arrays, parameter aliases, indirect counter writes, certificate
mismatches, opaque-memory/error barriers, frame cleanup and unchanged serialized
IR. Execution cases span all six HIR/native/register configurations.

The analysis/native-loop/procedure focused run passed 393 checks; the final ABI,
execution and serialization run passed 65. These sets overlap and are not added
together. Full-suite results include every final case.

## Comparative benchmark suite

All 84 functional executions passed across six modes; all 12 ROM-dependent cases
compiled successfully. The collection now includes the dedicated local-loop case
alongside the existing workloads. All nine independent Q-emuLator reference
calculations passed. The ordinary Turbo peephole pass remains enabled in every
configuration. Functional counts and full-emulator elapsed times are reported
separately.

## Provenance and evidence

Base revision `0b2cb4093f2cf0366815131731e26ed93c76cc9b`, plus the included compiler/test
patch and new benchmark source. Baseline compiler SHA256:
`26E83A25BBB9A46AB00ACFC9070FFF2CC55217EA4C027D07C04A49F39D1486C2`.
Current compiler SHA256: `3E5303705A4965A6D51C3D8F10305321606F2238C3D82FA7FD510CB055777507`.

Q-emuLator 3.5.1, [Original QL speed](qemulator-speed.png), 640 KiB RAM, JS and
Toolkit II ROMs, sound/graphics acceleration off. Turbo parser 5.09 / codegen
3.27 / lib537, FAST, large addressing, no copied windows or line numbers,
structured, 20 KiB dataspace and 350-byte base stack. SB uses
`--hir-opt --native-regions --regalloc`. These are emulator-configuration results,
not calibrated original-hardware measurements.

[Samples](samples.csv), [timing summary](summary.csv), [configuration](configuration.json),
[pre-run configuration](configuration-before-run.json), [validation](result-state.json),
[plan](plan.json), [runner](runner_bas), [suite summary](suite-summary.json),
[full-suite log](full-suite.log), [focused analysis log](focused-analysis.log),
[focused ABI log](focused-abi.log), [compiler patch](compiler-change.patch),
[binary comparison](binary-comparison.json), [ordinary SB verification](sb-verification-all.json),
[Turbo native verification](native-verification-all.json), [baseline provenance](baseline-provenance.json),
[functional comparison](execution-comparison.json), [route analysis](route-analysis.txt),
[route native IR](route-native-ir.txt), [six-mode results](six-mode-results.csv),
[six-mode configuration](six-mode-configuration.json), [functional manifest](functional-suite.json),
[independent references](reference-checks.json), [local-loop source](local_integer_for.bas),
[baseline invocation](Invoke-Baseline.fsx), [comparison build script](Build-Comparisons.ps1),
[comparison execution script](Count-Comparisons.fsx).

Reusable raw executables, the preserved compiler, source dumps and diagnostics
remain in `C:\Source\codex-workspace\.scratch\turbo-local-for`. The exact elapsed
run is its `qemulator-loops` subdirectory. No commit or push was performed.
