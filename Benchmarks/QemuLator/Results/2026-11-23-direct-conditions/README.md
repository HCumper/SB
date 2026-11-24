# Direct comparisons and integer truth branches

Implemented 23 November 2026. Certified native FOR conditions now branch
directly from signed CMP flags or integer truth tests, with trailing NOT folded
into branch polarity. Eligible eager AND/OR tails also avoid materializing their
right-hand Boolean. The existing scope, binding and control-flow gates remain.

## Implementation and QL semantics

Post-allocation tail fusion replaces final CMP/Scc/ANDI plus Boolean testing with
an allocated `CompareFlags` and Bcc. All seven integer comparisons retain their
signed-word rules; integer almost-equal remains exact equality. Operands are
unchanged, and LEA releases any spill frame without destroying N/V/Z.

An immediately consumed integer-to-float conversion becomes TST.W of the result
register, or TST.W (A1)+ when the word must stay materialized. Final scratch
copies can be forwarded. Repeated integer/float logical NOT operations invert
the branch condition. Fractional float conditions continue to test the complete
mantissa; they are never narrowed to integer truth.

For eager AND/OR, the left float remains on the original maths stack throughout
the complete RHS evaluation. Its original six-byte representation therefore
remains available on division, arithmetic or array failures. Only after RHS
success does the fused tail consume it and branch on the RHS flags/register and
left mantissa. Both operands execute even for false-left AND and true-left OR.
Generated internal skip labels use the existing short-branch relaxation and
word fallback. No new value persists across CFG joins.

Value-producing comparisons, stored Booleans and pending operands retain their
canonical QL representation. XOR and unsupported compound tails keep the
existing library path. Required integer-to-float calls keep their D2/D3 saves;
only remaining calls are included in the helper stack budget. A simple pinned
counter condition now fits the 12-byte loop save area rather than requiring an
additional 12-byte conversion save/return area.

The serialized IR format, HIR/range/effect/alias analysis, legacy peephole pass,
runtime library and optimization switches are unchanged. Either
`--no-native-regions` or `--no-regalloc` disables native FOR lowering. See
[native loops](../../../../SB/Optimization/TURBO-NATIVE-LOOPS.md) and
[register allocation](../../../../SB/Optimization/TURBO-REGISTER-ALLOCATION.md).

## Isolated functional comparison

The preserved immediate baseline and current compiler built identical sources
with all optimizations enabled and identical task names/default console startup.
Complete executables ran unmodified and returned independently checked results.

| Workload | Previous instructions | Current instructions | Reduction | Bytes before → after |
| --- | ---: | ---: | ---: | ---: |
| Route planner | 3,864,403 | 3,089,535 | 20.05% | 9,650 → 9,544 |
| Sieve | 348,439 | 239,026 | 31.40% | 7,558 → 7,534 |
| Bubble sort | 430,242 | 399,986 | 7.03% | 7,712 → 7,704 |
| Conditional-loop microbenchmark | 515,925 | 455,905 | 11.63% | 6,918 → 6,908 |
| Truth/AND microbenchmark | 2,766,076 | 1,672,252 | 39.54% | 7,064 → 6,992 |

The straight-line local-loop and scoped-eligibility controls remain at 389,115
and 389,175 instructions respectively, with identical bytes and fetch counts.
Across all 16 pre-existing functional workloads, only sieve, bubble sort and
the conditional-loop fixture change; the other 13 retain identical instruction
counts and executable sizes in the fully optimized mode.
Route instruction words fetched fall from 5,145,743 to 4,043,310 (21.42%);
sieve from 406,859 to 275,774 (32.22%); bubble sort from 511,817 to 471,477
(7.88%); conditional microbenchmark from 678,525 to 578,520 (14.74%); truth/AND
from 3,340,251 to 2,013,071 (39.73%). These are functional counts, not calibrated
MC68008 cycles. Default console startup adds 102 bytes compared with the ordinary
zero-window application jobs below.

The new `native_truth_for` fixture uses `(i MOD 3)-1` as a variable truth value,
then eager AND with `i MOD 7<>0`, in a procedure-local loop. Across 1000 indexes,
334 have zero truth. Of 142 multiples of seven, 48 are in that zero class.
The first IF adds 1666, the second 572: 2238 per call before its seed. Independent
residue counts give checksum 4970 (quick workload 2239). This isolates the
capability and is not an application-wide speedup prediction.

## External Q-emuLator timings

Unmodified sieve, eight-queens and route-planner executables ran under QDOS at
Original QL speed. Each compiler/program pair had one warm-up and three measured
whole-job trials, alternating order. All 24 jobs passed exact checksums,
polling-clock calibration and enclosing host-time validation.

| Program | Turbo FAST median (range), seconds | SB median (range), seconds | SB speedup (Turbo/SB) |
| --- | ---: | ---: | ---: |
| Sieve | 3.84 (3.84–3.84) | 1.16 (1.14–1.16) | 3.31× |
| Eight queens | 19.34 (19.34–19.36) | 22.80 (22.80–22.80) | 0.85× |
| Route planner | 36.06 (36.06–36.08) | 15.26 (15.24–15.26) | 2.36× |

Ratios above one favor SB; ratios below one favor Turbo. Previous SB medians
were sieve 1.68s and route planner 19.82s in the
[conditional-loop run](../2026-11-23-native-branches/README.md), and eight queens
22.78s in the [scope-eligibility run](../2026-11-23-scoped-eligibility/README.md).
Those baseline times were not freshly retimed. The deterministic before/after
comparison above isolates this implementation without combining historical
elapsed samples.

Ordinary sieve code shrinks 7,456 → 7,432 bytes; route planner 9,548 → 9,442;
eight queens 9,030 → 9,022. Their native FOR counts remain three, five and three
respectively, and their arithmetic/array proof coverage is unchanged. Only these
three complete binaries change. The other six application jobs are byte-identical
to the previous compiler and were not retimed. Eight queens' recursive search
remains threaded; its one directly fused comparison is in result checking.

All nine ordinary builds were independently rebuilt from the exact canonical
sources with matching complete executables and proof coverage. Genuine Turbo
FAST payloads were reused only after source/compiler-input hash checks, then
all nine payloads were reverified against captured native IR. Parent file/report
operations are outside the timer. Executable loading, startup, initialization,
computation, checksum PRINT and shutdown are included; no overhead is subtracted.
The filtered runner retains original b01/b06/b07 calls and nonconsecutive IDs.

## Full SB test suite

`dotnet test SB.sln --no-build --no-restore` passed 4,086 tests: 3,838
compiler tests and 248 runtime tests, with no failures or skips. The final focused
native-IR/native-loop/CLI run passed 1,949 overlapping checks.

There are 422 additional regression cases. Coverage includes all seven signed
comparisons at word limits with both branch polarities, register/spill/immediate
CMP sources, N/V/Z across frame release, integer truth and NOT parity, eager
AND/OR truth tables and errors, long branch fallback, fractional float truth,
canonical stored scalar/array Booleans, tight stack budgets and exact overflow
recovery operands. CLI executions compare all six HIR/native/register modes.
False-left AND and true-left OR still evaluate failing RHS expressions.

## Comparative benchmark suite

All 102 functional executions passed across six modes; 12 ROM-dependent builds
compiled successfully. Independent references passed for all 19 functional
fixtures and all nine Q-emuLator programs. The ordinary Turbo peephole pass stays
enabled in every configuration.

## Provenance and evidence

Base revision `0b2cb4093f2cf0366815131731e26ed93c76cc9b` plus the combined compiler/test
patch, including preceding pending loop/branch/eligibility work. The separate
baseline patch records the immediate pre-fusion source. Baseline compiler SHA256:
`846E82B26AE3692E091975525588AADAC401AD8F105D687CE9B2866361C26A47`.
Current compiler SHA256: `EE66E4120B02077ADF4E9B971ED055CAC1002504F855AB46695A0FC04F1D5AD2`.

Q-emuLator 3.5.1, [Original QL configuration](benchmark.QCF), 640 KiB RAM, JS and
Toolkit II ROMs, graphics acceleration and sound off. The installed emulator
loaded the configuration through its command line; the
[launch record](emulator-launch.json) retains that command and its fingerprint.
Turbo parser 5.09 / codegen 3.27 / lib537, FAST, large addressing, no copied
windows or line numbers, structured, 20 KiB dataspace and 350-byte base stack.
SB uses `--hir-opt --native-regions --regalloc`. These are emulator-configuration
results, not calibrated original-hardware measurements.

[Samples](samples.csv), [timing summary](summary.csv), [configuration](configuration.json),
[pre-run configuration](configuration-before-run.json), [validation](result-state.json),
[plan](plan.json), [runner](runner_bas), [suite summary](suite-summary.json),
[full-suite log](full-suite.log), [focused log](focused.log), [benchmark log](benchmarks.log),
[combined source patch](compiler-change.patch), [baseline patch](baseline-source.patch),
[binary comparison](binary-comparison.json), [ordinary SB verification](sb-verification-all.json),
[Turbo verification](native-verification-all.json), [baseline provenance](baseline-provenance.json),
[functional comparison](execution-comparison.json), [route native IR](route-native-ir.txt),
[sieve native IR](sieve-native-ir.txt), [queens native IR](queens-native-ir.txt),
[six-mode results](six-mode-results.csv), [six-mode configuration](six-mode-configuration.json),
[existing-workload comparison](suite-before-after.csv),
[manifest](suite.json), [reference checker](Check-ReferenceChecksums.ps1),
[independent QL references](reference-checks.json), [truth benchmark](native_truth_for.bas),
[baseline invocation](Invoke-Baseline.fsx), [comparison build script](Build-Comparisons.ps1),
[comparison execution script](Count-Comparisons.fsx).

Reusable current binaries, preserved compiler and raw diagnostics remain in
`C:\Source\codex-workspace\.scratch\turbo-direct-conditions`; the elapsed run
is its `qemulator-conditions` subdirectory. Changes remain uncommitted.
