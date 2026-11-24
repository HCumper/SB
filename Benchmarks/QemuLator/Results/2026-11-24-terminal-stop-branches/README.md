# Terminal STOP branches inside native loops

float_transcendentals was the last workload behind Turbo FAST. Its inner loop
was rejected for native code for one reason only: the never-taken check
`IF root<0.99999 OR root>1.00001 THEN PRINT "FAIL ..." : STOP`. A branch that
ends in STOP never returns to the loop, so it can run threaded without
affecting the loop around it.

- **HIR certificate.** An IF arm of assignments and PRINTs ending in STOP no
  longer rejects a native FOR binding certificate. Its effects still count, so
  a call in the PRINT or a write to the counter still rejects the loop. See
  [HIR analysis](../../../../SB/Optimization/HIR-ANALYSIS.md#native-for-binding-certificates).
- **Loop selection.** After a conditional branch, a run of straight records
  with no label operand or other transfer, ending in STOP and followed by the
  branch's own label, is a terminal tail. A selected memory-counter loop runs
  it between SLOW and an unreachable FAST, so the branch's label stays native.
  See [native loops](../../../../SB/Optimization/TURBO-NATIVE-LOOPS.md#call-free-string-and-floating-point-bodies).

## Instruction counts

Functional counts of executed 68000 instructions under the test harness's
68000 emulator with the real JS ROM, on the prepared inputs of the GoldCard run.
Host hook steps are excluded. These are **not** cycle counts or elapsed times.
"Before" is HEAD `03c8f58`
([previous result](../2026-11-24-inline-for-stepping/instruction-counts.csv));
the Turbo FAST executables are the same genuine captures.

| Benchmark | Before | After | Change | Turbo FAST | SB vs Turbo |
|---|---:|---:|---:|---:|---:|
| integer_arithmetic | 405,114 | 405,114 | 0 | 1,144,744 | −64.6% |
| sieve | 238,788 | 238,788 | 0 | 729,202 | −67.3% |
| matrix_multiply | 373,583 | 373,583 | 0 | 635,881 | −41.2% |
| string_operations | 826,599 | 826,599 | 0 | 859,067 | −3.8% |
| float_polynomial | 2,948,841 | 2,948,841 | 0 | 2,977,799 | −1.0% |
| float_transcendentals | 4,485,468 | 4,453,600 | −0.71% | 4,478,927 | **−0.57%** |
| eight_queens | 1,322,374 | 1,322,374 | 0 | 3,654,790 | −63.8% |
| route_planner | 2,994,079 | 2,994,079 | 0 | 6,627,612 | −54.8% |
| concordance | 10,986,165 | 10,986,165 | 0 | 11,260,160 | −2.4% |

SB now executes fewer instructions than Turbo FAST on all nine workloads.
Every job produced its exact checksum and exited normally.
[Counts with fetch words and sizes](instruction-counts.csv),
[raw counter output](instruction-counts.txt).

- float_transcendentals' inner loop is now a native memory-counter loop with
  an inline NEXT and one terminal tail. Its threaded FOR stepping, IF test and
  mode switches are gone; the inline NEXT costs 8,950 instructions in total and
  the native IF test 6,400. The executable grows from 7,136 to 7,222 bytes.
- The other eight executables are byte-identical to the previous compiler's:
  their failure checks all sit outside the innermost loops.

## QL semantics and fallback

- The tail's records are the original threaded ones, so its output, its
  errors and STOP behave exactly as before. SLOW flushes leaf register
  allocation, so the tail reads current values.
- Only the fall-through from the branch enters the tail, and STOP never reaches
  the label after it. A tail containing a label, a jump, a call, RETurn, NEXT,
  CONTINUE or a mode switch is not terminal.
- An IF/ELSE whose THEN arm stops is not handled: that arm ends in a jump over
  the ELSE, so its loop stays threaded.
- Global control barriers such as WHEN ERROR still reject every certificate, so
  no handler can resume into a loop from its tail.

## Tests

**The full run passed 4,316 of 4,319 SB compiler tests and 247 of 248 runtime
tests; all four failures passed when re-run on their own** ([log](full-suite.txt)).
A chess engine was using most of the host's CPU during the run, which took
28 minutes instead of the usual 16. Two failures were the SB CLI's 30-second
time limit (CallEntry fixtures, one of them fully threaded and so untouched by
this change), one was a locked `program.exe` in a C-backend test, and one was a
timing-sensitive runtime input test. Re-running every CallEntry and C-backend
`unsupported` test (123) and every runtime flush test (5) passed. New execution tests run four
programs in every HIR/native/register mode against threaded output: a tail that
is taken and prints the loop's live string, float and counter values; one that
is never taken; a block IF that assigns, prints and stops; and an IF/ELSE that
stays threaded. Report tests check the tail and loop counts. An error raised
inside a tail gives the same ERNUM natively and threaded. An analysis test
shows that a PRINT without STOP still rejects the certificate. IR tests show a
tail without its STOP, or with a jump added, keeps its loop threaded.

Four guards were each disabled in turn and the tests failed every time:
accepting a branch without STOP in the certificate, accepting a tail that does
not end in STOP, accepting a tail with a jump, and leaving out the FAST that
keeps the branch's label native (17 tests failed).

## Not measured

GoldCard elapsed times were not rerun; the counts above are the only
performance evidence. A quiet-host GoldCard run of the whole collection is
still needed, especially now that several workloads lead Turbo FAST by under 1%.
