# Inline FOR stepping in memory-counter native loops

One general change to native FOR loops whose counter stays in its memory cell
(the loops that hold string, floating-point or procedure-call work). It aims at
the per-iteration cost the profiles showed in every such loop.

lib537's `next_for_int` jumps through the counter cell to the iterator left by
`for_range_int`, which adds STEP, tests STEP's sign, compares with END, stores
the counter and jumps back through the stored loop-top address: 14 instructions
per repeated iteration. When the STEP is a literal, the loop's own NEXT now
becomes the SB record 1530, which does the same work inline in seven: reload the
binding, add the literal step with no range check, compare with the cell's END,
store and branch to the loop top. See
[native loops](../../../../SB/Optimization/TURBO-NATIVE-LOOPS.md#inline-next).

## Instruction counts

Functional counts of executed 68000 instructions under the test harness's
68000 emulator with the real JS ROM, on the prepared inputs of the GoldCard run.
Host hook steps are excluded. These are **not** cycle counts or elapsed times.
"Before" is HEAD `f5709dc`, whose counts are in the
[previous result](../2026-11-24-native-strings-rom-maths/instruction-counts.csv);
the Turbo FAST executables are the same genuine captures.

| Benchmark | Before | After | Change | Turbo FAST | SB vs Turbo |
|---|---:|---:|---:|---:|---:|
| integer_arithmetic | 405,114 | 405,114 | 0 | 1,144,744 | −64.6% |
| sieve | 238,788 | 238,788 | 0 | 729,202 | −67.3% |
| matrix_multiply | 373,583 | 373,583 | 0 | 635,881 | −41.2% |
| string_operations | 862,415 | 826,599 | −4.2% | 859,067 | **−3.8%** |
| float_polynomial | 2,976,829 | 2,948,841 | −0.9% | 2,977,799 | **−1.0%** |
| float_transcendentals | 4,485,468 | 4,485,468 | 0 | 4,478,927 | +0.15% |
| eight_queens | 1,432,402 | 1,322,374 | −7.7% | 3,654,790 | −63.8% |
| route_planner | 2,994,291 | 2,994,079 | ≈0 | 6,627,612 | −54.8% |
| concordance | 11,185,645 | 10,986,165 | −1.8% | 11,260,160 | **−2.4%** |

Every job produced its exact checksum and exited normally.
[Counts with fetch words and sizes](instruction-counts.csv),
[raw counter output](instruction-counts.txt).

- Every selected memory-counter loop in the collection has a literal STEP, so
  all nine take the inline NEXT: two in string operations, four in concordance
  and one each in polynomial, eight queens and route planner.
- **String operations** moves from 0.4% behind Turbo FAST to 3.8% ahead.
  Polynomial's lead grows from 0.03% to 1.0%, and concordance's from 0.7% to
  2.4%.
- **Eight queens** falls 7.7%: its recursive search loop is a memory-counter
  loop with procedure calls, and each NEXT now saves seven instructions.
- **Float transcendentals** is unchanged and still 0.15% behind: its loop is
  not selected as a native loop, so it keeps the threaded NEXT.
- Each inline NEXT is a few bytes longer than the template call; executables
  grow by 6 to 24 bytes.

## QL semantics and fallback

- Both loop certificates already exclude any FOR on the counter in the body or
  in its callees, so the cell keeps this loop's STEP, END and loop top from the
  initializer to NEXT. The record uses the cell's END and the literal that was
  pushed as STEP.
- The counter is read from and stored to the live cell, after reloading its
  binding, so writes by the body or a callee, and LOCAL rebinding in callees,
  behave as before. The word add has no range check, so a counter passing 32767
  wraps to −32768 exactly as the template does. A finished loop leaves the
  counter at its last value.
- A computed STEP, the initializer, EXIT and loops outside native code keep their
  templates. Validation rejects 1530 in threaded code, and code generation
  refuses it outside native mode.

## Tests

**4,288 SB compiler tests and 248 runtime tests passed, with zero failures or
skips** ([log](full-suite.txt)). New execution tests run eight
programs in every HIR/native/register mode against threaded output: each step
encoding (ADDQ/SUBQ at −2 and ±8, ADDI at 500 and −900), negative steps, wraparound past
32767 with EXIT, a procedure that writes the counter, and an END changed inside
the body. Report tests confirm one inline record per fixture and none for a
computed STEP, and validation rejects the record in threaded code.

Three guards were each disabled in turn and the tests failed every time:
accepting a non-literal STEP (the variable-STEP fixture failed in all native
modes), always using the positive-step branch (all three negative-step fixtures
failed), and storing the counter before the exit test (60 loop tests failed).

## Not measured

GoldCard elapsed times were not rerun; the counts above are the only
performance evidence. A quiet-host GoldCard run of the whole collection is
still needed.
