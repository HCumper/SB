# Verified global-array shapes at procedure entry

This extends the [precise procedure/array effects](../../../Turbo/Results/2026-11-22-procedure-effects/README.md)
with certified shape propagation into known procedures and functions. Implemented
and measured 22 November 2026. Existing native array operations are selected more
often; no new machine-code operations or register-pinning scheme were introduced.

## Analysis and safety

Incoming shapes are joined across every known call site for uniquely named global
arrays that cannot be shadowed by any formal or LOCAL binding. Common ranks and
minimum guaranteed bounds survive joins; an exact stride survives only when all
callers agree on the shape. Parameter/local shapes and scalar values remain private
to their scope. Array-element writes preserve descriptors; DIM, aliases, unknown
memory effects, argument mutations and sibling function calls invalidate affected
shape facts before entry. Existing error handling and retained checks are unchanged.

The outer fixed point begins without imported assumptions, so recursive calls
cannot bootstrap unproved facts. Each scope and the final call-entry solution must
be certified. Nonconvergence, uncertain control flow or the 64-pass limit discards
all imported facts and keeps the original conservative analysis. Existing HIR,
native-region and register-allocation switches continue to control this path.

See [the analysis design](../../../../SB/Optimization/HIR-ANALYSIS.md).

## External route-planner timings

The unchanged route planner returns checksum 4701. One warm-up and three measured
whole-job trials ran per compiler at Original QL speed, alternating order. All eight
jobs passed. The sources and executables contain no timing instrumentation.

| Build | Median seconds | Measured range | Executable bytes |
| --- | ---: | ---: | ---: |
| Turbo FAST | 36.06 | 36.06–36.06 | 13,192 |
| SB with call-entry shapes | 32.58 | 32.58–32.58 | 9,446 |
| Earlier SB, from the previous timed collection | 44.36 | See earlier report | 9,180 |

SB takes 9.7% less time than Turbo FAST in this run (1.11 times the
throughput). Its time is 26.6% below the earlier SB median, which comes
from the [direct-string-append collection](../2026-11-22-string-append/README.md),
not a fresh matched pre-change trial. That earlier Turbo median was 36.08 seconds.
The immediate procedure-effects baseline was used for the binary and functional
comparisons below, rather than elapsed timing.

Only route planner changed among all nine ordinary SB benchmark executables; the
other eight remain byte-for-byte identical to the immediate procedure-effects
baseline. All nine were rebuilt and independently verified. The genuine Turbo FAST
jobs were reused after exact canonical/compiler-input hash checks and all nine
native payloads were reverified against their captured IR. The other eight programs
were not timed again.

The runner retains the eight original b07 calls from the full collection with
their original nonconsecutive result identifiers. Loading, runtime startup,
initialization, computation, checksum PRINT and shutdown are timed. Parent file
opening/closing and reporting are outside the timer; no overhead was subtracted.
Every checksum, job return, calibration and enclosing host timestamp validated.

## Code generation and functional execution

| Route metric | Procedure-effects baseline | With entry shapes |
| --- | ---: | ---: |
| Proven arithmetic expressions | 6 | 6 |
| Proven numeric array accesses | 1 | 15 |
| Rank-one native array facts | 1 | 15 |
| Rank-two native array facts | 0 | 5 |
| Selected native FOR loops | 0 | 0 |
| Ordinary zero-window executable bytes | 9,136 | 9,446 |
| Call-entry fixed point | Not imported | 3 passes, certified |

The ordinary executable grows by 310 bytes (3.4%). Global descriptors/data pointers
are still reloaded for each access. Local and branching native FOR loops, pointer
hoisting and cross-block register allocation remain future work.

Both preserved compilers also built the original source with default console
startup and the same task name, then ran their full, unmodified executables in the
functional 68000 harness. Both returned checksum 4701 and exited normally:

| Default-console metric | Before | After | Change |
| --- | ---: | ---: | ---: |
| Executed instructions | 7,945,035 | 6,019,267 | −24.2% |
| Instruction words fetched | 10,194,445 | 7,997,184 | −21.6% |
| Executable bytes | 9,238 | 9,548 | +310 bytes |

These deterministic counts are separate from elapsed timings and do not estimate
calibrated MC68008 cycles. Default console startup explains the size difference
from the ordinary zero-window jobs.

## Full SB test suite

`dotnet test SB.sln --no-build --no-restore` passed all 3,352 tests: 3,104
compiler tests and 248 runtime tests, with no failures or skips. The compiler and
test assemblies were freshly built by the preceding focused run.

All 231 focused checks passed, including 104 new analysis/execution cases covering
call chains, joins with differing ranks/strides, calls before allocation, argument
and sibling effects, read-before-resize ordering, array aliases, dynamic/private
LOCAL bindings, recursion, certification-limit fallback and both load/store errors.
Execution cases run across six HIR/native/register settings; checked negative and
high subscripts retain their error behavior.

## Comparative benchmarks

The six-mode functional collection passed 78 executions and all 12 ROM-dependent
compile-only cases. All nine independent reference calculations passed. Ordinary
SB output/proof coverage and all genuine Turbo FAST native payloads were verified
for all nine programs. These compilation checks do not stand for new elapsed
measurements of the eight byte-identical programs.

## Configuration and evidence

Q-emuLator 3.5.1, [Original QL speed](qemulator-speed.png), 640 KiB RAM, JS and
Toolkit II ROMs, sound and graphics acceleration off. Turbo parser 5.09 / codegen
3.27 / lib537, FAST, large addressing, no copied windows or line numbers,
structured, 20 KiB dataspace and 350-byte base stack. SB uses
`--hir-opt --native-regions --regalloc`. These measurements describe this emulator
configuration, not calibrated MC68008 hardware timing.

Base revision: `24eb6f58457c4b8cae1d7d5bf04ee752557a805d` plus the included combined
source patch, including earlier uncommitted rank-two and procedure-effect changes.
Current compiler SHA256:
`26E83A25BBB9A46AB00ACFC9070FFF2CC55217EA4C027D07C04A49F39D1486C2`.
Immediate baseline compiler SHA256:
`966F8917AF889C47A6DE3800151A62403B17A7245326F82F7335E4C37CF7D773`.
No commit or push was performed.

[Timing samples](samples.csv), [summary](summary.csv), [configuration](configuration.json),
[pre-run configuration](configuration-before-run.json), [validation](result-state.json),
[plan](plan.json), [actual runner](runner_bas), [suite summary](suite-summary.json),
[full-suite log](full-suite.log), [focused log](targeted.log),
[compiler patch](compiler-change.patch), [baseline source patch](baseline-source.patch),
[all-nine binary comparisons](binary-comparison.json), [SB verification](sb-verification-all.json),
[Turbo verification](native-verification-all.json), [baseline provenance](baseline-provenance.json),
[route execution counts](route-execution.json), [route analysis](route-analysis.txt),
[route native IR](route-native-ir.txt), [six-mode results](six-mode-results.csv),
[six-mode configuration](six-mode-configuration.json), [references](reference-checks.json),
[baseline invocation](Invoke-Baseline.fsx), [route execution script](Count-Route.fsx).

Full raw executables, compiler dumps, captured IR, the baseline compiler and useful
diagnostics remain in `C:\Source\codex-workspace\.scratch\turbo-procedure-shapes`.
The exact elapsed-time run is its `qemulator-route` subdirectory.
