# Memory-based native call boundaries for recursion

Implemented audited procedure-call boundaries inside memory-counter native FOR
loops. The unchanged eight-queens search now runs its loop natively, retaining
counter, END/STEP and continuation state in its activation's original runtime
cell. A short threaded section executes each recursive call using the existing
GOSUB/LOCAL/return protocol. No register, array value or address cache survives the
call. The return continuation reloads restored bindings and shared values.

## N-queens at GoldCard speed

Fresh externally timed, **unmodified executables**, under Q-emuLator 3.5.1 at
**GoldCard speed (`Speed=GoldCard`)**, JS ROM and Toolkit II, 640 KiB RAM, no
accelerated graphics/sound. Compilation at Full speed is outside the timer.
Each row represents one warmup plus three measured trials per compiler; there
are 16 whole-job executions across the two separate control/current runs.
Times are seconds, median (minimum–maximum).

| Build | Turbo FAST | SB |
|---|---:|---:|
| Previous SB compiler control | 2.54 (2.54–2.54) | 2.98 (2.98–3.00) |
| Native recursive boundary | 2.56 (2.54–2.56) | 2.72 (2.72–2.74) |

The new SB median is **8.72% lower** than
the freshly rebuilt previous SB executable at the same GoldCard speed. Relative
to the current Turbo FAST median, SB is **+6.25%**
(positive means slower). The two runs use the same genuine Turbo executable as a
control. These results must not be compared directly with historical Original QL
speed medians. The runs are sequential controls, not an interleaved three-build
trial; the raw samples and ranges expose their scope and variability.

All jobs produced `eight_queens:4962`, returned normally, and passed 50 Hz clock
calibration and host-enclosure checks. The original program validates all 92
solutions and its board checksums. See [before samples](timings/before/samples.csv),
[after samples](timings/after/samples.csv),
[before validation](timings/before/result-state.json),
[after validation](timings/after/result-state.json),
[before configuration](timings/before/configuration.json), and
[after configuration](timings/after/configuration.json).

The baseline executable was rebuilt from the preserved previous compiler and
matched its prior independently verified executable byte for byte. Turbo FAST
captures were reused after unchanged input hashes and genuine IR code payloads
were verified. Each current SB build independently matches an ordinary build of
the unmodified source. [Baseline rebuild log](baseline-build.log),
[all-nine SB verification](all-nine-sb-verification.json),
[all-nine Turbo verification](all-nine-turbo-verification.json).

## Isolated instruction and code-size changes

| Metric | Previous SB | New SB | Change |
|---|---:|---:|---:|
| Executed 68000 instructions | 4,292,463 | 3,918,739 | 8.71% fewer |
| Consumed opcode/extension words | 5,554,032 | 5,093,436 | 8.29% fewer |
| Executable bytes | 9,022 | 9,108 | +86 (0.95%) |
| Dispatch/mode instructions | 519,226 | 161,050 | 358,176 fewer |

Turbo FAST remains at 3,654,790 instructions, 4,625,374 fetched words and 12,718
bytes in the same functional setup. Both SB versions and Turbo execute the same
230,828 ROM instructions for board validation. Counts use the actual JS ROM and
a zero-window job with 31 inherited console references, reproducing the unchanged
job's setup. They are instruction/fetch counts, not cycle or elapsed-time estimates.
See [matched counts](profile-comparison.csv), [raw profiles](profiles/profiles.json),
and [verification](profiles/verification.json). Ten complete SB/Turbo profiles
passed independent PC/fetch/ROM/routine accounting, checksum and executable/code-map
identity checks; eight deliberate corruptions were rejected.

The general call-boundary pass also selects eligible loops in route planner and
concordance. Concordance's matched instruction count falls another 1.08%; it was
not timed in this task. The other six application executables are byte-identical
to the immediate baseline. [All-nine binary comparison](all-nine-binary-comparison.json).

## QL semantics and fallback

HIR supplies a separate memory-loop certificate, kept out of the pinned-register
planner. It requires an owned integer LOCAL or unique global counter, entry
through the original FOR initializer, stable FOR metadata across calls, resolved
storage effects, and supported acyclic control. Scalar aliases and ordinary
counter-value writes are permitted because FOR/NEXT reads the live memory cell.
Callee LOCAL/formal effects are projected out at normal returns by the existing
interprocedural analysis, including recursive call graphs.

The emitter attests each exact compiled procedure entry and integer value-argument
count outside serialized IR. This distinguishes value/reference ABI variants.
The typed stack audit requires arguments to consume the entire maths stack at the
call boundary. SLOW/GOSUB/FAST preserves argument order, binding, heap frames,
return-mode bits and original out-of-memory checks. Register allocation flushes
before the call and begins a fresh leaf afterward. Arrays are re-read after
callee writes and descriptor changes invalidate bounds assumptions.

Handlers/resumption, unknown effects, possible caller FOR-metadata replacement,
unsupported transfers, nested loops, functions and reference/array/string/float
parameter calls retain their existing lowering. No new arithmetic overflow policy
is added. `--no-native-regions` disables the new path; `--no-regalloc` retains
memory-counter loops while disabling register allocation/pinned loops.
`--no-hir-opt` independently disables HIR rewriting. The library, machine-code
encodings and legacy Turbo peephole optimization are unchanged.

[New native diagnostics](timings/after/queens-native-ir.txt) show one selected
memory-counter loop and one procedure-call boundary in the recursive search;
[baseline diagnostics](timings/before/queens-native-ir.txt) retain the threaded
search. Original register-retaining loops elsewhere remain in place.

## Full SB test suite

**4,328 unique tests passed: 4,080 compiler and 248 runtime; zero failures/skips.**
The full suite ran in isolated processes with explicit disjoint selection.
[Exact partition commands](full-suite-partitions.json),
[coverage and uniqueness audit](suite-summary.json),
[discovery counts](discovery-counts.json), and
[audit implementation](Audit-FullSuite.py). Final TRX results are retained under
[full-tests](full-tests). The audit checks both test IDs and canonical case names,
fixture membership, complete discovery counts and the tested assembly/source hashes.
An initial exclusion-only NUnit selection was replaced after it executed already
selected cases; that interrupted run is not counted in the final results.

The 207 focused checks also pass: [focused log](focused-tests.log).
The 75 added cases cover self/mutual recursion, repeated activations, saved LOCAL
scalars/strings and global shadows, multiple arguments and value/reference ABI
variants, shared-array visibility, captured END/STEP, empty and descending loops,
ordinary counter writes, separate call-proof authorization and stack mismatches,
reference/handler fallback, and matching QL error numbers for arithmetic overflow,
division by zero, array bounds after resizing, recursive bounds failures and
out-of-memory. The test harness now reads lib537's recorded ERNUM on the fatal
error path without altering guest state; the reviewed profiling recipe was rebased
for that diagnostic change. Existing handler, retry and parser-guard cases remain
part of the full suite.

## Provenance and reproduction

Previous compiler SHA-256: `3CF85C2AE537BCE03B74A7831B7CD3041F30AFBA60D29CD795C6987577EEDB56`.
Tested compiler SHA-256: `504851652E8EDE12985EE83A39E22C35E80759C6C5026A6131D35D57467A6228`.
[Source verification](source-verification.json) and
[tested source snapshot](compiled-source-verification.json) identify the exact
binary and source. The [complete pending compiler patch](compiler-change.patch)
includes earlier uncommitted work; [baseline source patch](baseline-source.patch)
records this task's starting state. [Benchmark tools patch](benchmark-tools.patch)
includes the GoldCard defaults, while [profiling tools](profiling-tools) include
the reviewed source recipe. These snapshots describe the measurement state before
commit and push. Installed ROMs are not redistributed.

Rebuild with the archived source state, run the recorded full-suite partitions,
and audit them. Prepare ordinary benchmarks with
`Benchmarks/QemuLator/Prepare-Benchmarks.py` (GoldCard is the default), build SB,
verify genuine Turbo FAST IR and use the external timing boot. The archived
[preparation script](Prepare-Timings.ps1) records unchanged input checks and the
N-queens subset. [Timing launcher](Run-GoldCard-When-Verified.ps1) waits for the
complete coverage audit, validates both eight-job controls, and stops only its
owned hidden emulator processes. [Comparison data](comparison.json) records the
reported medians and instruction/fetch deltas.
