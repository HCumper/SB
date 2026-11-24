# Native string operations, ROM maths calls and loop EXIT

Three general changes aimed at the three workloads the previous result left
behind Turbo FAST: string operations, floating-point transcendentals and
concordance. None special-cases a program, and each keeps QL evaluation order
and error behaviour.

1. **ROM maths calls in native code.** The one-argument QL ROM maths functions
   (ABS, ACOS, ACOT, ASIN, ATAN, COS, COT, DEG, EXP, INT, LN, LOG10, RAD, SIN,
   SQRT, TAN) are treated as nonmutating. Native regions and memory-counter
   native loops call them with the library's native `rom_fn`, and floating
   arithmetic, comparisons and AND/OR join native regions. RND and any
   program-defined function of the same name keep their threaded calls. See
   [native regions](../../../../SB/Optimization/TURBO-NATIVE-REGIONS.md).
2. **String work in native regions, and EXIT in native loops.** Straight
   regions and loop bodies now take string literals, concatenation,
   comparisons, conversions, CODE/CHR$ and fetches and stores of whole DIMmed
   strings and string-array elements. A native FOR loop may contain an EXIT of
   that same loop: it jumps to the loop's own end and leaves the counter cell
   as threaded EXIT does. See
   [native loops](../../../../SB/Optimization/TURBO-NATIVE-LOOPS.md).
3. **Proven-allocated append and LEN.** Turbo's native append (`s$=s$&x`, 374)
   and simple LEN (375) omit the "string not found" check that the threaded
   templates make. HIR analysis now proves, per site, that a DIMmed string's
   DIM reaches every occurrence; only those sites run natively. Unproven sites
   stay threaded, and inside a native loop they drop to threaded code for that
   one operation. See
   [HIR analysis](../../../../SB/Optimization/HIR-ANALYSIS.md) and
   [string append](../../../../SB/Optimization/TURBO-STRING-APPEND.md).

## Instruction counts

Functional counts of executed 68000 instructions under the test harness's
68000 emulator with the real JS ROM, on the prepared inputs of the GoldCard
run (`b00`–`b08`). Host hook steps are excluded. These are **not** cycle
counts or elapsed times. "Before" is a separate build of HEAD `8e3217f`. For
the five workloads the
[previous result profiled](../2026-11-24-fused-conditions-abs-ranges/profile-comparison.csv),
its counts, fetch words and sizes, and the Turbo FAST ones, match that table
exactly. The Turbo FAST executables are the same genuine captures.

| Benchmark | Before | After | Change | Turbo FAST | SB vs Turbo |
|---|---:|---:|---:|---:|---:|
| integer_arithmetic | 405,132 | 405,114 | ≈0 | 1,144,744 | −64.6% |
| sieve | 238,806 | 238,788 | ≈0 | 729,202 | −67.3% |
| matrix_multiply | 373,601 | 373,583 | ≈0 | 635,881 | −41.2% |
| string_operations | 890,149 | 862,415 | −3.1% | 859,067 | **+0.39%** |
| float_polynomial | 2,976,987 | 2,976,829 | ≈0 | 2,977,799 | −0.03% |
| float_transcendentals | 4,587,878 | 4,485,468 | −2.2% | 4,478,927 | +0.15% |
| eight_queens | 1,432,402 | 1,432,402 | 0 | 3,654,790 | −60.8% |
| route_planner | 3,000,135 | 2,994,291 | −0.2% | 6,627,612 | −54.8% |
| concordance | 12,123,279 | 11,185,645 | −7.7% | 11,260,160 | **−0.66%** |

Every job produced its exact checksum and exited normally.
[Counts with fetch words and sizes](instruction-counts.csv),
[raw counter output](instruction-counts.txt).

- **Concordance** goes from 7.7% behind Turbo FAST to 0.7% ahead. A fourth
  loop becomes native (three before), now that native loops accept EXIT and
  string work. Three append/LEN sites are proven allocated; two of them sit in
  native code. The executable grows from 12,936 to 15,476 bytes, still smaller than
  Turbo's 17,920.
- **String operations** goes from 3.6% behind Turbo FAST to 0.4% behind. Both
  of its append/LEN sites are proven and native. The remaining gap, about
  3,300 instructions, is the outer `FOR pass_i` loop, which stays threaded
  because the native loop selector does not take nested loops.
- **Float transcendentals** goes from 2.4% behind to 0.15% behind: its three
  ROM maths calls now run inside native regions.
- The ≈0 changes come from end-of-run checksum tests, whose float comparisons
  and AND now join a native region. Eight queens is byte identical.

## QL semantics and fallback

- ROM maths functions may still fail coercion, allocation or their own domain
  checks, and report the same errors. They are assumed not to rebind storage
  or install continuations, as ABS already was. A later-loaded extension that
  redefined one of these names would not be seen by the analysis.
- A DIMmed string keeps its bound through ordinary assignment, because QL
  assignment copies into the existing buffer. An append or LEN site counts as
  allocated only if:
  - its scope is trusted, as for the other native proofs;
  - the DIM reaches it on every path, after any effects earlier in the same
    statement;
  - the name belongs to exactly one variable in the program, so no LOCAL or
    parameter of that name can stand in for it through dynamic binding.
- String facts do not cross procedure calls, so append and LEN inside
  procedures keep their checks. A `GO TO` around the DIM still raises the QL
  "not found" error, natively and threaded.
- `--no-native-regions`, `--no-regalloc` and `--no-hir-opt` behave as before.

## Tests

**4,236 SB compiler tests and 248 runtime tests passed, with zero failures or
skips** ([log](full-suite.txt)). New coverage runs proven append/LEN programs
(loops, nested loops, EXIT, a procedure reading a global) on the 68000 CPU and
compares every mode against threaded output. It checks that unproven sites
still raise the QL error natively and threaded, and that conditional DIMs, LEN
before DIM and a LOCAL of the same name leave sites checked. The loop bridge
for unproven sites and the one-variable-per-name rule were each disabled in
turn, and the tests failed both times. The runtime getenv test needs `HOME`
unset, as before.

## Not measured

GoldCard elapsed times were not rerun for this change; the counts above are
the only performance evidence. A quiet-host GoldCard run of the whole
collection is still needed.
