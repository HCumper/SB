# Retaining proven invariant native array rows

Implemented 23 November 2026. Certified native integer FOR loops retain up to
three rank-two row addresses across iterations. This reuses descriptor and data
pointer reads, the row-times-stride multiplication and the row address additions.
Initialization stays at the first executed original access, after its checks.

## Implementation and QL semantics

HIR binding certificates now carry separate invariant scalar and array sets.
A scalar must be unaffected by the counter, any body write, both IF arms and
all possible aliases of those writes. Array descriptors must be unaffected by
descriptor writes; element content writes are allowed. Escaped storage is
excluded. The emitted certificates cover each routine ABI variant and are kept
outside the serialized Turbo IR. Existing scope, effect and control gates remain.

`TurboNativeInvariants.fs` matches immutable virtual array operations to allocated
accesses. A retained row is a literal or a certified invariant scalar; the key
also contains the array, check mask and exact/dynamic stride. Selection favors
repeated accesses and uses at most A3–A5, with D5 initialization bits. All extra
register saves count against the existing stack budget. Registers and the entire
spill/save frame are restored before every runtime error exit and on normal exit.

The first executed access performs its column check, row check and complete
long address calculation in their original order. Only after success is the
address marked valid. Later accesses still evaluate their index expressions,
perform any column check in place, and reuse the successful invariant row check
and row base. Column failures retain the pending row; row failures consume it.
Empty loops and untaken IF arms do not initialize an address. Checks and accesses
are never moved ahead of earlier arithmetic or division errors.

For unknown exact stride, initialization reads the actual column count from the
descriptor. Minimum bounds are never substituted for the stride. Internal
address arithmetic retains Turbo's full modulo-32-bit behavior, including row
offsets beyond 65535 bytes. No new BASIC arithmetic or overflow checks are added.

Caches reset on each loop invocation, allowing row values, DIM sizes and bindings
to change between calls. Row changes, possible row aliases, missing descriptor
proofs, tight stack budgets and remaining conversion helpers retain ordinary
addressing. Rank-one arrays, varying rows, calls and unsupported loops retain
their existing lowering. Stores remain immediate. The library, serialized IR
format and legacy Turbo peephole pass are unchanged. Either `--no-native-regions`
or `--no-regalloc` disables this whole-loop optimization; `--no-hir-opt` controls
HIR rewriting independently and still permits native analysis and allocation.

See [native loops](../../../../SB/Optimization/TURBO-NATIVE-LOOPS.md) and
[HIR analysis](../../../../SB/Optimization/HIR-ANALYSIS.md).

## Isolated functional comparison

The preserved immediate baseline and current compiler built the same unmodified
sources, fully optimized, with identical task names and default console startup.
Complete jobs returned independent expected checksums under the functional CPU.

| Workload | Previous instructions | Current instructions | Reduction | Fetch words before → after | Complete bytes before → after |
| --- | ---: | ---: | ---: | ---: | ---: |
| Matrix multiplication | 383,513 | 373,821 | 2.53% | 547,224 → 539,109 | 7,962 → 8,076 |
| Route planner | 3,089,535 | 3,001,999 | 2.83% | 4,043,310 → 3,958,970 | 9,544 → 9,600 |

Fetch reductions are 1.48% and 2.09% respectively. These counts are not calibrated
MC68008 cycles; removing a row MULU can matter more than its single instruction
count suggests. Initialization and saved registers add code and work, especially
for short loops. This is a measured incremental gain, not a prediction for all
array programs.

Matrix multiplication retains two row addresses in initialization, one in its
dot-product loop and one in checksum traversal. The varying row of B(k,col)
continues to be calculated. Route planning retains the road row in network
initialization and in edge relaxation, including reuse across conditional leaves.
Both retain their existing native FOR counts (three and five) and arithmetic/
array proof coverage. The cache uses successful invariant checks; it does not
expand the existing range proofs.

The matched sieve, bubble-sort, local-loop, scoped-loop and truth/conditional-loop
controls retain identical sizes and instruction/fetch counts. Eight queens was
compiled but requires ROM ABS and is validated by its unchanged ordinary binary,
not executed in the minimal functional CPU. Of all 114 six-mode benchmark rows,
only the two register-allocated matrix rows change. HIR rewriting on/off gives
the same new matrix counts. The other 112 rows retain sizes and counts.

## External Q-emuLator timings

Unmodified matrix and route-planner jobs ran under QDOS at Original QL speed.
Each compiler/program pair had one warm-up and three measured whole-job trials,
alternating order. All 16 jobs passed exact checksums, polling-clock calibration
and enclosing host-time validation.

| Program | Turbo FAST median (range), seconds | SB median (range), seconds | SB speedup (Turbo/SB) |
| --- | ---: | ---: | ---: |
| Matrix multiplication | 3.66 (3.66–3.66) | 2.10 (2.08–2.10) | 1.74× |
| Route planner | 36.06 (36.06–36.06) | 14.60 (14.60–14.60) | 2.47× |

Ratios above one favor SB. Previous SB medians were matrix multiplication 2.18s
in the [rank-two run](../2026-11-22-rank-two/README.md) and route planner 15.26s
in the [direct-condition run](../2026-11-23-direct-conditions/README.md). Those
historical samples were not freshly retimed. The matched functional comparison
above isolates this implementation; the table compares Turbo and SB in the new
run. Whole-job loading, startup, initialization, computation, checksum PRINT and
shutdown are included, with no overhead subtraction. File/report operations
are outside the timer. Clock resolution is 0.02s.

Ordinary zero-window matrix code grows 7,860 → 7,974 bytes and route planner
9,442 → 9,498 bytes. Default console jobs above include another 102 startup bytes.
All other seven complete application binaries are identical to the immediate
baseline and were not retimed. All nine SB ordinary builds were independently
rebuilt from the exact canonical sources, requiring matching complete binaries
and proof coverage. Genuine Turbo FAST jobs were reused only after source/input
hash checks; all nine native payloads were then reverified against captured IR.
The filtered runner preserves original b02/b07 calls and nonconsecutive IDs.

## Full SB test suite

`dotnet test SB.sln --no-build --no-restore` passed 4,173 tests: 3,925
compiler tests and 248 runtime tests, with no failures or skips. The final focused
native-IR/native-loop/analysis/end-to-end run passed 1,996 overlapping checks.

There are 87 new regression cases. They cover all four check masks, literal and
scalar rows, exact/dynamic/32768-column strides, one-time initialization across
loads/stores and iterations, full long offsets, column-before-row failures,
errors after initialization, saved-register/frame recovery, absent certificates,
counter/changing/aliased rows, helper and stack fallback, multiple retained rows
and distinct check identities. Six-mode CLI cases cover repeated calls, DIM
between calls, joined dynamic strides, delayed first use, untaken/empty loops,
changing-row fallback and first-row/later-column runtime errors. Existing tests
continue to cover handlers, RETRY/CONTINUE, arithmetic and division errors.
An additional standalone execution check initializes all three retained rows,
reuses them, then triggers arithmetic overflow. It confirms prior array/scalar
stores, the current counter, original operand, CPU stack and D2–D7/A0/A3–A6
survive the error exit. The matrix and route analysis dumps are byte-identical
to their immediate baseline dumps.

## Comparative benchmark suite

All 102 functional executions passed across six modes; 12 ROM-dependent builds
compiled successfully. Independent references passed for all 19 functional
fixtures and all nine Q-emuLator programs. The ordinary Turbo peephole pass stays
enabled in every configuration. No new microbenchmark is needed: existing
matrix multiplication and route planning exercise the optimized work directly.

## Provenance and evidence

Base revision `0b2cb4093f2cf0366815131731e26ed93c76cc9b` plus the combined compiler/test
patch, including preceding pending loop/branch/eligibility work. The baseline
patch records the immediate pre-hoisting source. Baseline compiler SHA256:
`EE66E4120B02077ADF4E9B971ED055CAC1002504F855AB46695A0FC04F1D5AD2`.
Current compiler SHA256: `12F7395A57F72A44887603AD7F47553F5537505F19CA3B6E895BD59EE32CAAF8`.

Q-emuLator 3.5.1, [Original QL configuration](benchmark.QCF), 640 KiB RAM, JS and
Toolkit II ROMs, graphics acceleration and sound off. The installed emulator
loads that configuration through its command line; the [launch record](emulator-launch.json)
retains the command and its fingerprint. Turbo parser 5.09 / codegen 3.27 /
lib537, FAST, large addressing, no copied windows or line numbers, structured,
20 KiB dataspace and 350-byte base stack. SB uses
`--hir-opt --native-regions --regalloc`. These are emulator-configuration results,
not calibrated original-hardware measurements.

[Samples](samples.csv), [timing summary](summary.csv), [configuration](configuration.json),
[pre-run configuration](configuration-before-run.json), [validation](result-state.json),
[plan](plan.json), [runner](runner_bas), [suite summary](suite-summary.json),
[full-suite log](full-suite.log), [focused log](focused.log), [benchmark log](benchmarks.log),
[combined source patch](compiler-change.patch), [baseline patch](baseline-source.patch),
[binary comparison](binary-comparison.json), [ordinary SB verification](sb-verification-all.json),
[Turbo verification](native-verification-all.json), [baseline provenance](baseline-provenance.json),
[functional comparison](execution-comparison.json), [route native IR](route-native-ir.txt),
[matrix native IR](matrix-native-ir.txt), [six-mode results](six-mode-results.csv),
[six-mode configuration](six-mode-configuration.json), [all-configuration comparison](suite-before-after.csv),
[manifest](suite.json), [reference checker](Check-ReferenceChecksums.ps1),
[independent QL references](reference-checks.json), [baseline invocation](Invoke-Baseline.fsx),
[comparison build script](Build-Comparisons.ps1), [comparison execution script](Count-Comparisons.fsx).
[Three-cache overflow check](Check-Retained-Unwind.fsx), [its output](retained-unwind.log),
[analysis comparison](analysis-comparison.json).

Reusable binaries, preserved compiler and raw diagnostics remain in
`C:\Source\codex-workspace\.scratch\turbo-invariant-arrays`; the elapsed run
is its `qemulator-arrays` subdirectory. Changes remain uncommitted.
