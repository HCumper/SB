# Native integer comparisons and conditional FOR bodies

Implemented 23 November 2026. Certified native integer FOR loops now accept
signed integer comparisons and simple nested IF/ELSE bodies. This builds on the
pending procedure-local loop certificates from the preceding implementation.

## Implementation

Integer comparisons use signed MC68000 CMP and conditional set instructions.
An immediately consumed comparison branches directly on its integer Boolean
result. Boolean values stored or combined use the exact QL six-byte 0/1 format.
Integer truth conversion and NOT/AND/OR/XOR retain the bundled lib537 semantics,
including eager evaluation and its error behavior.

The control-flow planner validates every forward edge and requires canonical,
empty maths stacks at branch joins. Stores reach memory before edges and leaf
register caches restart at boundaries. The certified counter, captured END and
STEP remain pinned in D2-D4. The exact QL word-to-float helper preserves D2/D3
around its call; its saves and return address are included in the stack budget.
Checked arithmetic and array failures retain the current counter, original
maths-stack operands and any pending Boolean values before entering the existing
runtime error handler.

Internal branches share the existing short-branch relaxation with word fallback.
Backward body edges, external entries, incompatible join stacks, unsupported
float operations, calls and unsafe aliases retain the threaded fallback. The
existing global CFG/range certification gate remains in place. There is no
general register allocation across branches. Serialized Turbo IR is unchanged.
`--no-native-regions` or `--no-regalloc` disables this lowering; HIR rewriting
remains independently controlled. The ordinary Turbo peephole pass is unchanged.

See [native loops](../../../../SB/Optimization/TURBO-NATIVE-LOOPS.md) and
[the analysis design](../../../../SB/Optimization/HIR-ANALYSIS.md).

## External Q-emuLator timings

The original sieve and route-planner sources and complete executables ran
unmodified. Each compiler/program pair had one warm-up and three measured
whole-job trials, with alternating order at Original QL speed. All 16 jobs passed
their exact checksums, polling-clock calibration and enclosing host-time checks.

| Program | Turbo FAST median (range), seconds | SB median (range), seconds | SB time reduction vs Turbo |
| --- | ---: | ---: | ---: |
| Sieve | 3.84 (3.82–3.84) | 1.68 (1.68–1.70) | 56.25% |
| Route planner | 36.06 (36.06–36.06) | 19.82 (19.82–19.82) | 45.04% |

The preceding route-planner SB median was 32.18 seconds in the
[procedure-local-loop run](../2026-11-23-local-for/README.md). That baseline was
not freshly retimed. The isolated deterministic comparisons below use the
preserved immediate baseline compiler with identical build options.

Elapsed intervals include loading, startup, initialization, computation, checksum
PRINT and shutdown. Parent file/report operations are excluded; no overhead is
subtracted. Sources and executables contain no timing instrumentation. The
filtered runner retains the original b01/b07 calls and their nonconsecutive
result IDs from the full collection.

All nine ordinary SB builds were independently verified. Only sieve and route
planner changed; the other seven complete binaries are byte-identical to the
previous compiler and were not retimed. Genuine Turbo FAST payloads were reused
after exact canonical/compiler-input hash checks and all nine native payloads
were reverified against their captured IR.

## Isolated functional comparisons

Both compilers built the same unchanged sources with all optimizations enabled,
identical task names and default console startup. Unmodified complete executables
returned independently calculated checksums in the functional 68000 harness.

| Workload | Previous instructions | Current instructions | Reduction | Bytes before → after |
| --- | ---: | ---: | ---: | ---: |
| Route planner | 5,954,147 | 3,864,403 | 35.10% | 9,644 → 9,650 |
| Sieve | 447,318 | 348,439 | 22.10% | 7,500 → 7,558 |
| Bubble sort | 754,019 | 430,242 | 42.94% | 7,720 → 7,712 |
| Conditional local-loop microbenchmark | 1,255,613 | 515,925 | 58.91% | 6,892 → 6,918 |
| Straight-line local-loop microbenchmark | 389,115 | 389,115 | 0.00% | 6,846 → 6,846 |

Route instruction words fetched fall from 7,897,840 to 5,145,743 (34.85%);
sieve from 544,799 to 406,859 (25.32%); bubble sort from 970,044 to 511,817
(47.24%); the conditional microbenchmark from 1,638,080 to 678,525 (58.58%).
The straight-line microbenchmark remains at 491,699. These are functional counts,
not calibrated MC68008 cycles or elapsed times. Default console startup adds
102 bytes compared with the ordinary zero-window Q-emuLator jobs.

The ordinary sieve executable grows by 58 bytes (7,398 → 7,456) and selects
three native FOR loops, previously two. Ordinary route planner grows by six
bytes (9,542 → 9,548) and selects five, previously three. Its newly native bodies
perform nearest-town selection and road relaxation. Six route loops remain
threaded. Its six arithmetic proofs, 15 proven array accesses and five rank-two
access facts are unchanged.

The new `native_conditional_for` microbenchmark makes 20 calls with a LOCAL
counter ranging to parameter-supplied 1000. It adds two when `i MOD 3=0`, one
otherwise, and reduces the sum modulo 8192. An independent count gives checksum
6870 (quick workload 1334). It isolates this capability and does not predict
application-wide acceleration.

## Full SB test suite

`dotnet test SB.sln --no-build --no-restore` passed all 3,602 tests:
3,354 compiler tests and 248 runtime tests, with no failures or skips.
The final focused run passed 412 checks. Its cases overlap the full suite and
are not added to that total.

The change adds 156 net regression cases beyond the preceding 3,446-test suite.
Coverage includes all seven integer comparison operators across signed limits,
nested IF/ELSE and NOT/AND/OR/XOR, scalar truth, Boolean scalar/array assignments,
join reloads, captured END/STEP, alias/float fallbacks, external entries, backward
edges, invalid joins, helper stack budgets and long conditional branch fallback.
Execution cases span all six HIR/native/register configurations. Eager false-left
AND cases still raise division, arithmetic and bounds errors, with exact pending
maths-stack values checked.

## Comparative benchmark suite

All 90 functional executions passed across six modes; all 12 ROM-dependent cases
compiled successfully. All 17 independent functional references and all nine
Q-emuLator reference calculations passed. The ordinary Turbo peephole pass
remains enabled in every mode. Functional instruction counts and full-emulator
elapsed times are reported separately.

## Provenance and evidence

Base revision `0b2cb4093f2cf0366815131731e26ed93c76cc9b`, plus the included combined
compiler/test patch and benchmark files. The patch includes the preceding
procedure-local native FOR implementation and the new control-flow module.
The separately archived baseline patch records the immediate pre-branch source.
Baseline compiler SHA256:
`3E5303705A4965A6D51C3D8F10305321606F2238C3D82FA7FD510CB055777507`.
Current compiler SHA256: `506311D79644B464281830F0176154A93FFB24CC8D336D59506676B3BFA30915`.

Q-emuLator 3.5.1, [Original QL speed](qemulator-speed.png), 640 KiB RAM, JS and
Toolkit II ROMs, graphics acceleration and sound off. Turbo parser 5.09 / codegen
3.27 / lib537, FAST, large addressing, no copied windows or line numbers,
structured, 20 KiB dataspace and 350-byte base stack. SB uses
`--hir-opt --native-regions --regalloc`. These are emulator-configuration results,
not calibrated original-hardware measurements.

[Samples](samples.csv), [timing summary](summary.csv), [configuration](configuration.json),
[pre-run configuration](configuration-before-run.json), [validation](result-state.json),
[plan](plan.json), [runner](runner_bas), [suite summary](suite-summary.json),
[full-suite log](full-suite.log), [focused log](focused.log),
[combined compiler patch](compiler-change.patch), [baseline source patch](baseline-source.patch),
[binary comparison](binary-comparison.json), [ordinary SB verification](sb-verification-all.json),
[Turbo native verification](native-verification-all.json), [baseline provenance](baseline-provenance.json),
[functional comparison](execution-comparison.json), [route analysis](route-analysis.txt),
[route native IR](route-native-ir.txt), [six-mode results](six-mode-results.csv),
[six-mode configuration](six-mode-configuration.json), [functional manifest](suite.json),
[functional reference checker](Check-ReferenceChecksums.ps1),
[independent references](reference-checks.json), [conditional source](native_conditional_for.bas),
[straight-line local-loop source](local_integer_for.bas),
[baseline invocation](Invoke-Baseline.fsx), [comparison build script](Build-Comparisons.ps1),
[comparison execution script](Count-Comparisons.fsx).

Reusable raw executables, the preserved compiler and diagnostics remain in
`C:\Source\codex-workspace\.scratch\turbo-native-branches`. The elapsed run is
its `qemulator-branches` subdirectory. No commit or push was performed.
