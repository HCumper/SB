# Profiles of the five remaining slower workloads

Measured 23 November 2026. This is item 4 from the optimization recommendations:
profile eight queens, concordance, string operations and the two floating-point
workloads before choosing further compiler changes. The production compiler and
test harness are unchanged. Reusable diagnostic tools were added under
`Benchmarks/Profiling`.

## Fresh external elapsed timings

These are unmodified, fully optimized SB jobs versus genuine Turbo FAST jobs.
Each pair has one warm-up and three measured trials at Original QL speed under
the installed Q-emuLator 3.5.1. All 40 jobs passed exact checksums, return-status,
external-clock calibration and enclosing host-time checks. The 30 measured
samples include executable loading, startup, initialization, computation, PRINT
and shutdown. Parent file/report operations are outside the timer; no overhead
is subtracted. Resolution is 0.02 seconds.

| Workload | Turbo FAST median (range), s | SB median (range), s | SB extra time |
| --- | ---: | ---: | ---: |
| String operations | 4.06 (4.06–4.06) | 4.48 (4.46–4.48) | 10.34% |
| Polynomial | 12.84 (12.84–12.86) | 14.04 (14.02–14.04) | 9.35% |
| Transcendentals | 18.20 (18.18–18.20) | 18.68 (18.68–18.68) | 2.64% |
| Eight queens | 19.38 (19.36–19.38) | 22.82 (22.80–22.82) | 17.75% |
| Concordance | 49.66 (49.66–49.66) | 56.46 (56.46–56.48) | 13.69% |

The timed files are exactly those profiled below. All five current SB payloads
and all five mapped genuine Turbo payloads were reproduced byte for byte before
profiling. Their expected results are string operations 880, polynomial 9252,
transcendentals 1280, eight queens 4962 and concordance 7936.

## Instruction-count comparison

The diagnostic runner executes the installed JS ROM itself: QL floating
arithmetic, ABS/SIN/COS/SQRT and QL string collation. Console and job services are
small host hooks, excluded from the instruction totals below. No BASIC source,
IR or executable instrumentation was inserted.

| Workload | Turbo instructions | SB instructions | SB extra instructions | SB interpreter/mode share | Mode overhead's share of the extra instruction count |
| --- | ---: | ---: | ---: | ---: | ---: |
| String operations | 859,067 | 946,785 | 10.21% | 8.19% | 88.34% |
| Polynomial | 2,977,799 | 3,224,499 | 8.28% | 6.71% | 87.65% |
| Transcendentals | 4,478,927 | 4,587,878 | 2.43% | 1.96% | 82.35% |
| Eight queens | 3,654,790 | 4,292,463 | 17.45% | 12.10% | 81.41% |
| Concordance | 11,260,160 | 12,729,163 | 13.05% | 9.05% | 78.41% |

The mode bucket includes shared-template interpreter tails, the runtime
interpreter, N/P transition entries and the emitted SLOW/FAST transition
templates. Transitions inside an already compiled native-loop body remain
charged to that body. These counts explain most of the difference from Turbo FAST. They
are functional work counts, not measured elapsed-time shares or calibrated
68008 cycles. Removing that entire bucket is an accounting upper bound, not
a promised speedup. Native emission still needs correct helper, branch, binding,
stack and error protocols.

## Where the work goes

**Eight queens:** about 76% of SB's instructions execute on behalf of
`SEARCH_BOARD`, and about 24% on behalf of `RECORD_BOARD`. The search's recursive
loop remains outside the native FOR planner. Its scalar leaves already receive
native allocation, and its array operations use compact helpers; those helpers
alone account for 27.2% of all SB instructions. Predicate materialization,
threading and repeated N/P transitions remain in the hot recursive search.
Interpreter/mode work is 519,226 instructions versus Turbo's 68. ABS is called
2,576 times in both compilers and executes the same 230,828 ROM instructions.
Optimizing board clearing or changing ABS would miss the main cost.

**Concordance:** `ADD_WORD` accounts for about 52% and `SORT_INDEX` about 38%
of SB instructions. Generic string extraction executes 49,636 times and costs
2,682,324 instructions (21.1%). QL `UT.CSTR` performs 33,208 comparisons and
3,030,380 ROM instructions in both compilers. String movement and materialization
add substantial common work: `get_up` and `copy_upstr` together account for about
7.1% of SB instructions. There are 5,544 CODE calls, versus 49,636 generic
extractions. A direct CODE/character-read path is useful, but dictionary lookup,
sorting and their string operands deserve higher priority. Interpreter/mode work
is 1,151,883 instructions versus Turbo's 72.

**String operations:** no ROM arithmetic is involved. Generic extraction is
23.9% of SB instructions; the unrolled `byte_table` copy loop is another 18.4%.
That label names executable copy code, not a data table. The major byte-copy
counts match Turbo; much of the deficit comes from retaining P templates and
dispatch around the same work. Interpreter/mode work is 77,533 versus 40.
The existing direct append path remains active; repeated slicing/concatenation
and string assignment dominate the rotation loop.

**Polynomial floating work:** both compilers execute exactly 1,687,584 ROM
instructions, including 3,980 RI.EXEC divisions and 11,936 RI.EXEC multiplications.
Runtime special-value paths handle the remaining source operations without a ROM
call. ROM work is 52.3% of SB instructions. The extra work is in the surrounding
threaded program, not an additional mathematical workload: interpreter/mode work
is 216,269 versus 42. Native loop/control emission can help without reassociating
floating expressions or replacing division with reciprocal multiplication.

**Transcendentals:** both compilers call SIN, COS and SQRT 1,280 times each,
plus 2,560 RI.EXEC multiplications, and execute exactly 3,861,857 ROM instructions.
This is 84.2% of SB instructions. The absolute compiler deficit is much smaller
here; interpreter/mode work is 89,765 versus 42. Replacing the QL mathematical
functions would create a much larger semantic burden than the current gap
justifies.

| Workload | ROM instructions, identical in both compilers | ROM share of SB instructions |
| --- | ---: | ---: |
| String operations | 0 | 0.00% |
| Polynomial | 1,687,584 | 52.34% |
| Transcendentals | 3,861,857 | 84.18% |
| Eight queens | 230,828 | 5.38% |
| Concordance | 3,030,380 | 23.81% |

## Recommended next implementation

1. **Extend audited native control flow to simple string and floating-point
   loops.** Start with call-free, single-entry loops using the existing runtime
   templates and counters stored in memory. This avoids assuming that D2â€“D4
   survive arbitrary helpers. Audit N/P template differences, joins, branch
   encoding, register clobbers, maths-stack snapshots and errors. Keep the
   existing native-disable switches and conservative fallback. This directly
   addresses most of the measured extra instruction count across the collection.
2. **Add checked direct operands for comparisons of stored strings and string
   arrays.** Reuse the existing QL `UT.CSTR` collation semantics while avoiding
   generic extraction and temporary copies where effect/alias proofs permit.
   Preserve left/right evaluation and bounds-error order; snapshot a left value
   before an RHS that may mutate it. Add direct CODE reads as a smaller follow-up.
3. **Extend native regions through carefully modeled procedure-call boundaries.**
   Eight queens is the main test. Prove LOCAL/formal ownership, preserve dynamic
   bindings and captured FOR state, and materialize/restore required values at
   recursive calls. Then apply direct integer predicates and branch fusion in
   those newly eligible regions. Existing whole-loop pinning must not simply be
   enabled across calls.

Keep QL ROM mathematics and floating expression order intact. Further constant
folding or register allocation inside already optimized integer leaves addresses
less of this remaining gap than wider native control flow and string operands.
No optimization implementation was added as part of this profiling task.

## Verification and reproducibility

Ten complete profiles pass independent checksums and zero exit status. The
diagnostic generator produces an identical complete code payload for every SB
and genuine Turbo job; SB is also independently rebuilt with the normal compiler.
Every PC histogram sums to the independently counted instruction and fetch
totals. Routine, ROM-service and host-hook totals reconcile. Eight deliberately
corrupted profiles and a changed generator source are rejected. The reusable
tools were separately rerun after elapsed timing; their instruction/fetch totals,
code-group counts, ROM work and binary fingerprints reproduce the first run.

SB routine attribution follows mapped IR records and P dispatch. Bootstrap ranges
are charged to main/startup; return overhead remains with the callee until the
next caller record executes. Captured Turbo IR has no routine names, so its
routine table is deliberately not split into named source routines. ROM call
tables contain overlapping entry and named-service observations; those counters
must not be added together.

The QDOS shim has no scheduler, loading or console implementation instruction
cost. It supplies only the four required resident names, uses a fixed job base
and gives the unchanged zero-window job the same 31 inherited output-channel
references as the external runner. Unsupported instructions/services fail;
there is no approximate host replacement for ROM floating or string work.
The external timings use the real QDOS environment and the same installed ROM.

Compiler SHA256: `12F7395A57F72A44887603AD7F47553F5537505F19CA3B6E895BD59EE32CAAF8`.
JS ROM SHA256: `FC6A683E44570D7E4A144580729E25FF4B3BD293A9EFF5313CFDAE11520D5EFC`.
Base revision remains `0b2cb4093f2cf0366815131731e26ed93c76cc9b`, with the preceding
uncommitted optimization changes. The production source fingerprints match the
[last compiler verification](../2026-11-23-invariant-arrays/README.md), which passed
4,173 tests. That compiler suite was not rerun for this profiling-only task.

Q-emuLator 3.5.1, Original QL speed, 640 KiB RAM, JS/Toolkit II ROMs, sound and
accelerated graphics off. Turbo parser 5.09/codegen 3.27/lib537, FAST, large model,
no copied windows/line numbers, 20 KiB dataspace and base stack 350. SB flags are
`--hir-opt --native-regions --regalloc`; the legacy Turbo peephole remains enabled.
These elapsed results apply to this emulator configuration and are not calibrated
original-hardware measurements.

[Reusable tools and instructions](../../../Profiling/README.md),
[profile summaries](profiles/profiles.json), [profile configuration](profiles/configuration.json),
[profile validation](profiles/verification.json), [reproduction comparison](reproduction.json),
[instruction comparison](instruction-comparison.csv), [mode accounting](mode-accounting.json),
[timing samples](samples.csv), [timing summary](summary.csv), [clock validation](result-state.json),
[timing configuration](configuration.json), [pre-run configuration](configuration-before-run.json),
[launch record](emulator-launch.json), [plan](plan.json), [runner](runner_bas),
[SB verification](sb-verification.json), [Turbo verification](native-verification.json),
[build fingerprint](sb-build.json), [source verification](source-verification.json).

Per-PC histograms, code maps and IR listings for all ten jobs are retained in
the `profiles` subfolder. Original executables, raw QDOS output and initial
diagnostics remain in `C:\Source\codex-workspace\.scratch\turbo-workload-profiles`.
New tools and evidence remain uncommitted.
