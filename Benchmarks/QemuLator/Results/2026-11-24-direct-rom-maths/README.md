# Direct RI.EXEC calls for ROM maths functions

float_transcendentals was SB's narrowest lead over Turbo FAST: 0.57%. Its
`SIN`, `COS` and `SQRT` calls went through lib537's `rom_fn`, which calls a
BASIC function the way the interpreter would. It builds a one-entry parameter
name table on the heap, saves the interpreter's state, and afterwards
reconciles the result type. The ROM's BASIC function then fetches the argument
back out of that table. Turbo FAST compiles every ROM function this way.

In the JS ROM, ABS, ACOS, ACOT, ASIN, ATAN, COS, COT, EXP, LN, LOG10, SIN, SQRT
and TAN do no more than fetch their one argument as a float (`CA.GTFP`),
reserve 48 bytes of maths stack and call the routine that `RI.EXEC` dispatches
that operation to. A new pass, `TurboNativeRomMaths.fs`, rewrites native calls
of these functions to the SB-only record **1539**, which calls `RI.EXEC` with
the operation directly. It keeps `rom_fn`'s out-of-memory test and error exit,
and converts an integer argument with `RI.FLOAT`.

See [native regions](../../../../SB/Optimization/TURBO-NATIVE-REGIONS.md#direct-rom-maths-calls).

## Instruction counts

Functional counts of executed 68000 instructions under the test harness's
68000 emulator with the real JS ROM, on the prepared inputs of the GoldCard run.
Host hook steps are excluded. These are **not** cycle counts or elapsed times.
"Before" is HEAD `a1526bc`
([previous result](../2026-11-24-in-place-string-stores/instruction-counts.csv));
the Turbo FAST executables are the same genuine captures.

| Benchmark | Before | After | Change | Turbo FAST | SB vs Turbo |
|---|---:|---:|---:|---:|---:|
| integer_arithmetic | 405,114 | 405,114 | 0 | 1,144,744 | −64.6% |
| sieve | 238,788 | 238,788 | 0 | 729,202 | −67.3% |
| matrix_multiply | 373,583 | 373,583 | 0 | 635,881 | −41.2% |
| string_operations | 825,575 | 825,575 | 0 | 859,067 | −3.9% |
| float_polynomial | 2,948,841 | 2,948,841 | 0 | 2,977,799 | −1.0% |
| float_transcendentals | 4,453,600 | 3,935,145 | **−11.6%** | 4,478,927 | **−12.1%** |
| eight_queens | 1,322,374 | 1,322,374 | 0 | 3,654,790 | −63.8% |
| route_planner | 2,994,079 | 2,994,079 | 0 | 6,627,612 | −54.8% |
| concordance | 7,643,493 | 7,643,493 | 0 | 11,260,160 | −32.1% |

Every job produced its exact checksum and exited normally.
[Counts with fetch words and sizes](instruction-counts.csv),
[raw counter output](instruction-counts.txt).

- float_transcendentals' 3,840 calls each save 135 instructions, 518,455 in
  all, net of the new record's own work. The executable runs 257,334 fewer:
  `rom_fn`'s parameter table and type reconciliation. The ROM runs 261,121
  fewer: the BASIC function's argument fetch and stack reservation, less
  `RI.EXEC`'s dispatch. The ROM now executes 3,600,737 instructions, 91.5% of
  the run. Fetch words fall by the same 11.7%, to 12.1% fewer than
  Turbo FAST's ([split](ri-exec-fuzz.txt)).
- Its executable grows from 7,222 to 7,274 bytes: each call is 34 bytes instead
  of 8, and the three link records for SIN, COS and SQRT are dropped.
- The other eight executables are byte-identical. float_polynomial calls no
  maths function; its divisions and multiplications already call `RI.EXEC`.

## QL semantics and fallback

- The routine receives the same float at the same address that the BASIC
  function would have passed it, so the result is the same. Under the real
  ROM, each operation was called with every other register set at random,
  twice per argument, on 2,006 arguments: results, errors and stack positions
  never differed, and `RI.EXEC` preserved every register except D0 and A1
  ([fuzz](ri-exec-fuzz.txt)).
- `rom_fn`'s out-of-memory test comes first: `no_memory` unless
  `A1 - 128 >= even(A4) + 8`, which for the always-even A1 is
  `A1 >= A4 + 136`. An error the routine returns, such as -18 for `SQRT(-1)`,
  goes to `qdos_error` as before.
- Only ATAN's integer arguments reach the call: every other function
  declares a float parameter. `RI.FLOAT` converts them, as `CA.GTFP` did.
- DEG and RAD scale by a constant and INT rounds, so they keep `rom_fn`, as do
  calls in threaded code and names linked more than once. A function no
  longer called through `rom_fn` loses its link record, as ABS already did.
- This is verified against the JS ROM only. On another ROM the record calls
  that system's `RI.EXEC` operation rather than its BASIC function. As for
  ABS, a resident extension that later redefines one of these names is not
  called. `--no-native-regions` disables the pass.

## Tests

**4,384 SB compiler tests passed with zero failures or skips. The runtime
suite passed 247 of 248 in the Bash shell and 248 of 248 with `HOME` unset**
([log](full-suite.txt)). The one failure is
`interpreter getenv maps ql environment aliases to host values`: it sets
`USERPROFILE` and expects `GETENV$("HOME")` to return it, but `GETENV$` reads
`HOME` first, and the shell running the suite sets `HOME`. This change does
not touch the runtime.

- The test harness can now run a program under an installed QL ROM image
  (`runWithRom`). The ROM's own `RI.EXEC`, `UT.CSTR` and maths functions
  execute, and the maths functions are linkable residents. Real-ROM tests
  are skipped when no ROM is installed (none were skipped here).
- Thirteen real-ROM tests, one per function, run 41 float and 41 integer
  arguments through threaded code (`rom_fn_p` and the BASIC function) and
  through the direct record. They print every result as its sign, binary
  exponent and all 31 mantissa bits, and require identical output. A larger
  run of 802 results per function was also identical
  ([differential](rom-differential.txt)).
- Six real-ROM failure programs give the same ERNUM and output natively and
  threaded: `SQRT(-1)`, `LN(0)`, `ASIN(2)`, `EXP(2000)`, `LOG10` of an
  integer 0, and a loop that grows the heap 64 bytes an iteration until the
  call's out-of-memory test fails (-3).
- IR tests check the selected operations and argument types, dropped and
  kept links, threaded calls, DEG/RAD/INT, a name linked twice, and
  validation of the record in threaded code and with an invalid operation.

Six guards were each disabled in turn, and a test failed every time:
- the out-of-memory test (the heap-growth program);
- the error exit (all five domain errors);
- `RI.FLOAT` for integer arguments (ATAN's bit-for-bit test);
- the native-mode requirement;
- keeping the link of a function still called through `rom_fn`;
- requiring a single link record.

While writing the bit-exact test programs, a separate front-end bug turned
up. Unary minus on an untyped procedure parameter (`PRINT -v`), or on an
untyped `LOCal` in a procedure that has parameters, fails semantic analysis
with `InvalidOperandTypes`. Binary minus works, so the fixtures use `0-t`.

## Not measured

GoldCard elapsed times were not rerun; the counts above are the only
performance evidence.
