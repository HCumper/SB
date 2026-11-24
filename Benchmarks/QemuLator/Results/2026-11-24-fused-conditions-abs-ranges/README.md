# Fused conditions, native integer ABS and recursive parameter ranges

Three general compiler changes aimed at the remaining eight-queens gap. Each
preserves QL evaluation order and error behaviour, and none special-cases the
program.

1. **Recursive array shapes and fused IF conditions.** Call-entry array shapes
   are now a least fixed point, so recursive procedures inherit them: every
   search and validation array access uses the register-aware rank-one path.
   Integer IF conditions inside native code become one leaf and one native
   branch instead of Turbo's float Boolean templates. See
   [fused native conditions](../../../../SB/Optimization/TURBO-NATIVE-CONDITIONS.md).
2. **Native integer ABS.** `x%=ABS(word)` (int-to-float, ROM ABS, float-to-int)
   becomes the native record 1334, with ABS(-32768) still raising overflow. A
   validation loop containing it can now be certified and kept in registers.
   See [native regions](../../../../SB/Optimization/TURBO-NATIVE-REGIONS.md).
3. **Ranges through recursion.** Integer value parameters receive their
   argument ranges at call entry, widened to the callee's literals across
   recursion, and `<>` now excludes an interval endpoint. A formal that every
   call passes a scalar value no longer aliases visible storage. Together these
   prove every search subscript and its arithmetic, and a fused AND/OR chain
   skips terms that can neither fail nor write once it is decided. See
   [HIR analysis](../../../../SB/Optimization/HIR-ANALYSIS.md).

## N-queens at GoldCard speed

Unmodified executables under Q-emuLator 3.5.1 at **GoldCard speed
(`Speed=GoldCard`)**, JS ROM and Toolkit II, 640 KiB RAM, no accelerated
graphics or sound. Each run is one warm-up and three measured trials per
compiler, externally timed. Times are seconds, median (minimum–maximum).

| Build | Turbo FAST | SB |
|---|---:|---:|
| Previous SB compiler control (HEAD `2fb7b2a`) | 2.56 (2.54–2.56) | 2.72 (2.72–2.74) |
| This change | 2.56 (2.54–2.56) | **1.00 (1.00–1.02)** |

SB now takes **60.9% less time** than Turbo FAST (2.56× faster), and 63.2% less
than the previous SB compiler. The control's N-queens executable is byte
identical to the one timed in the
[previous result](../2026-11-23-native-recursion/README.md) and reproduces its
2.72 s exactly. Both runs used the same genuine Turbo FAST executable. All 16
jobs produced `eight_queens:4962`, returned normally and passed 50 Hz clock
calibration and host-enclosure checks; host job time matched emulated time in
every sample. See [control](timings/nqueens-control/samples.csv) and
[current](timings/nqueens-current/samples.csv) samples, with their
configuration, launch and validation records alongside.

## Instructions and code size

Matched real-ROM profiles of the same functional setup as the previous result.
Counts are executed instructions and consumed opcode/extension words, not cycle
or elapsed-time estimates.

| N-queens | Instructions | Fetch words | Bytes | ROM instructions |
|---|---:|---:|---:|---:|
| Turbo FAST | 3,654,790 | 4,625,374 | 12,718 | 230,828 |
| Previous SB | 3,918,739 | 5,093,436 | 9,108 | 230,828 |
| **This change** | **1,432,402** | **1,947,083** | **8,986** | **0** |

SB now executes 63.4% fewer instructions than before and 60.8% fewer than
Turbo FAST. `SEARCH_BOARD` falls from 2,879,482 to 1,190,500 instructions;
`RECORD_BOARD` from 1,029,020 to 231,932, with no ROM call left. The fused
search condition is 457,632 instructions in total, about 29 per loop iteration.
The largest remaining costs are Turbo's FOR initializer template (186,675) and
the procedure-call protocol (return, LOCAL binding and GOSUB, about 250,000).
[Profile comparison](profile-comparison.csv), [raw profiles](profiles/profiles.json),
[verification](profiles/verification.json).

Concordance also falls 1.2% (12,274,411 to 12,123,279 instructions). String
operations and both floating-point workloads are unchanged, with identical
executables. Across the collection, six SB executables are byte identical to
the previous compiler's; eight queens, route planner and concordance change and
each becomes smaller. [All-nine binary comparison](all-nine-binary-comparison.json).

Eight queens now selects four native FOR loops (was three), seven proven
arithmetic expressions (was three) and 23 proven array accesses (was eight).
Route planner's proven array accesses rise from 15 to 18.
[N-queens native diagnostics](queens-native-ir.txt), [analysis](queens-analysis.txt).

## Whole collection: correctness only

A whole-collection GoldCard run of all nine programs (72 jobs) produced the
exact checksum for every job and passed the harness validation. Its times are
**not used**: an unrelated ICS regression run started on the host part-way
through. In two stretches (jobs r011–r045 and r063–r071) host job time was about
1.5 times the emulated time for both compilers. Q-emuLator's 50 Hz clock follows
wall time, so starving the emulator lengthens measured jobs. The clean stretches
agree with the N-queens runs above. The [raw samples](timings/suite-contended/samples.csv)
are kept for provenance; a whole-collection timing needs a rerun on a quiet host.

## QL semantics and fallback

- Fused conditions evaluate every operand that could fail, in source order:
  QL AND/OR do not short-circuit. A failure snapshot rebuilds each pending
  Boolean as Turbo's six-byte float, so the maths stack matches the original
  templates. Only terms that cannot fail or write are skipped once decided.
- Native ABS gives the exact result for every word and raises the ROM
  conversion's overflow (ERNUM -18) for -32768, with the operand consumed as
  `fast_float_to_int` does. A program's own ABS function and any non-integer
  use keep their calls. This assumes the QL ROM's ABS, as the effect analysis
  already did; a later-loaded extension that redefined ABS would not be called
  for these integer assignments.
- Imported parameter ranges and private formals apply only to scalar value
  arguments at every static call site. Bare-variable, array, missing and
  reference arguments, and routines with no call site, keep the conservative
  rules. The outer solution must be a post-fixpoint, or all imported facts are
  discarded.
- `--no-native-regions`, `--no-regalloc` and `--no-hir-opt` behave as before.
  The library, machine-code encodings for existing records and the legacy
  peephole pass are unchanged.

## Full SB test suite

**4,181 SB compiler tests and 248 runtime tests passed, with zero failures or
skips** ([log](full-suite.log)). New coverage executes the AND/OR flags, integer
truth tests, Boolean failure snapshots and integer ABS on the 68000 CPU, and
compares every HIR/native/register mode against threaded output. It also checks
that a later failing operand keeps its QL error, ABS(-32768) gives -18, a
far IF label forces the long-jump retry, and the analysis proves array shapes
through recursion and mutual recursion and guarded recursive parameter ranges. The runtime getenv test needs
`HOME` unset; Git Bash sets it, and that test fails the same way on HEAD.

## Provenance and reproduction

Turbo FAST captures were reused from the verified 23 November build after
confirming identical canonical sources and Turbo inputs, and native Turbo IR
was re-verified ([provenance](turbo-provenance.json),
[verification](turbo-verification-all-nine.json)). Each SB executable was
independently rebuilt from the unmodified source with the normal CLI and
required to be identical ([SB verification](sb-verification-all-nine.json)).
The control used a separate build of HEAD `2fb7b2a`. The profiling recipe was
rebased onto the changed `TurboCodegen.fs`; its code maps reproduce every
executable byte for byte, and ten profiles passed independent accounting while
eight deliberate corruptions were rejected.

Prepare and build as in the [collection README](../../README.md), reuse
verified Turbo captures, subset the plan to `b06` for the N-queens runs, run
`Use-TimingBoot.ps1`, open `benchmark.QCF` at GoldCard speed, then run
`Collect-Results.py --complete` and `Save-Metadata.ps1`. Profiles come from
`Benchmarks/Profiling/Run-Profiles.ps1` on the full run directory.
