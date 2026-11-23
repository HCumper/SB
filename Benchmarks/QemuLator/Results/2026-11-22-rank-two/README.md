# Rank-two native integer arrays

Implemented the first three multidimensional-array improvements: fixed-rank
two-dimensional integer accesses in native IR, exact-shape facts distinct from
minimum bounds guarantees, and rank-two access support in native integer FOR
loops. The existing optimization switches control the new path. Pointer
increments and row-pointer hoisting remain future work.

## External Q-emuLator timings

Measured 22 November 2026 at Original QL speed. The unchanged matrix program
runs ten passes and prints checksum 1094. Each compiler had one warm-up and
three measured whole-job trials, alternating execution order. All eight jobs
passed; no executable or source instrumentation was added.

| Build | Median seconds | Measured range | Executable bytes |
| --- | ---: | ---: | ---: |
| Turbo FAST | 3.64 | 3.64–3.66 | 9,672 |
| SB with rank-two native accesses | 2.18 | 2.18–2.18 | 7,860 |
| Previous SB, from the earlier run | 3.94 | See prior report | 7,666 |

SB takes 40.1% less time than Turbo FAST in this run (1.67 times the throughput).
Its time is 44.7% below the previous SB median. The previous value comes from
the [direct-string-append run](../2026-11-22-string-append/README.md), rather than
a fresh baseline trial. That run's Turbo median was 3.68 seconds. The executable
grows by 194 bytes, or 2.5%.

All nine programs were rebuilt and independently verified against ordinary SB
builds. The other eight SB executables are byte-for-byte identical to the prior
revision, so only matrix multiplication was timed again. Their earlier timings
are not presented as new measurements. All nine genuine Turbo FAST executables
were reused after exact source/input hash checks and reverified against their
captured native IR using the compatibility generator.

The timing runner was filtered from the full prepared collection to retain only
the eight `b02` calls, without changing those calls, the timing procedure or either
executable. Nonconsecutive result identifiers therefore reflect their original
positions in the full runner. The clock calibrated at 101 ticks over 2.040 host
seconds; every checksum, job return and enclosing host timestamp validated.

## Code generation and functional execution counts

The matrix program now selects three native FOR loops, previously zero. Six
rank-two access facts retain the existing bounds proofs. The seven arithmetic
proofs are unchanged; remaining arithmetic overflow checks remain in place.

| Matrix metric | Previous SB | Current SB | Change |
| --- | ---: | ---: | ---: |
| Executed instructions | 652,186 | 383,294 | −41.2% |
| Instruction words fetched | 875,081 | 546,926 | −37.5% |
| Whole executable bytes | 7,666 | 7,860 | +2.5% |
| Selected native FOR loops | 0 | 3 | +3 |

These are deterministic functional 68000 counts, separate from elapsed timing.
Both unchanged executables used the same scratch console harness, derived from
`TurboExecutionHarness.fs` with 31 inherited output references supplied at job
startup, as required by the zero-window benchmark builds. No executable bytes
were patched. Other ordinary kernels supported by this shim produced identical
counts. The larger programs were verified by binary identity and the prior
full Q-emuLator results, without extending the minimal console shim.

The standard `Benchmarks/Turbo/Run-Benchmarks.ps1 -CompareRegisters` collection
also passed across all six HIR/native/register settings: 78 executions and 12
ROM-dependent compile-only cases, with no failures. Its matrix variant includes
default console startup and has different executable sizes and startup counts;
its results must not be substituted for the ordinary zero-window jobs above.

## Semantics and remaining scope

Each access reloads its descriptor/data pointer. The compiler emits independent
row/column bounds checks unless proven unnecessary, checks the column first,
and uses the current column length unless the exact reaching shape is known.
Address calculation uses unsigned word multiplication and long additions,
matching Turbo's wrap behavior without introducing address-overflow traps.
Scalar aliases, failed stores, consumed subscript prefixes, RETRY, spill recovery
and the current FOR counter retain their existing observable behavior.

Unknown ranks, higher dimensions, floating/string arrays and unsafe continuation
contexts retain their generic paths. Disabling register allocation uses existing
checked helpers; disabling native regions keeps threaded output. The ordinary
Turbo peephole optimizer remains enabled. No pointer caching or hoisting was
introduced. See [the design](../../../../SB/Optimization/TURBO-NATIVE-ARRAYS.md).

## Full SB test suite

`dotnet test SB.sln --no-restore` passed all 3,178 tests: 2,930 compiler tests and
248 runtime tests, with no failures or skips. This includes 89 new regression
cases covering exact/minimum shape joins, independent checks, long offsets and
address wrap, register pressure, aliases, native loop cleanup, stack fallback,
local rebinding and error ordering. Nine independent benchmark references and
all 11 external-harness regression tests also passed.

## Configuration and evidence

Q-emuLator 3.5.1, [Original QL speed](qemulator-speed.png), 640 KiB RAM, JS and
Toolkit II ROMs, graphics acceleration and sound off. Turbo parser 5.09 / codegen
3.27 / lib537, FAST, large addressing, no copied windows or line numbers,
structured mode, 20 KiB dataspace and 350-byte base stack. SB uses
`--hir-opt --native-regions --regalloc`. Measurements include loading, runtime
startup, initialization, computation, checksum printing and shutdown. They
describe this emulator configuration, not calibrated MC68008 hardware timing.

The base revision is `24eb6f58457c4b8cae1d7d5bf04ee752557a805d` plus the included
compiler patch. Compiler SHA256:
`336299334BDE47C889756DE97494A82B0CE796CDDB384DA9DC6261A3581D958C`.
No commit or push was performed for this implementation.

[Timing samples](samples.csv), [summary](summary.csv), [configuration](configuration.json),
[pre-run configuration](configuration-before-run.json), [validation](result-state.json),
[plan](plan.json), [actual runner](runner_bas), [full-suite summary](suite-summary.json),
[suite log](full-suite.log), [compiler patch](compiler-change.patch),
[all-nine SB verification](sb-verification-all.json), [all-nine Turbo verification](native-verification-all.json),
[baseline provenance](baseline-provenance.json), [executable hashes](executable-fingerprints.json),
[functional comparison](execution-comparison.json), [six-mode results](six-mode-results.csv),
[six-mode configuration](six-mode-configuration.json), [reference checks](reference-checks.json),
[harness tests](harness-tests.txt), [matrix native IR plan](matrix-native-ir.txt).

Full executables, compiler dumps, captured IR and generated diagnostics are in
`C:\Source\codex-workspace\.scratch\turbo-rank-two`; the exact timing run is in
its `qemulator-matrix` subdirectory.
