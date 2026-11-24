# In-place string comparison operands

Concordance spent most of its compiler-controlled time copying strings to compare
them. For `IF dictionary$(scan_i)=token$`, `extr_string` copied the element to
the string heap, `fch_string_1d` copied `token$` there too, and
`compare_string` copied both texts again to the maths stack before calling the
ROM's `UT.CSTR`. The ROM comparison itself is unavoidable; the copies are not.

A new pass, `TurboNativeStringOperands.fs`, runs after native-region selection.
Where both operands of a native string comparison are a DIMmed string or a
whole string-array element, it substitutes three SB-only records:

- **1532** and **1531** stand where `fch_string_1d` and `extr_string` stood
  and push the same descriptor, pointing at the stored text instead of a copy.
  The element record keeps `extr_string`'s index checks.
- **1533** keeps the comparison's type and true/false result. It gives
  `compare_string`'s results for empty strings and otherwise calls `UT.CSTR`
  on the stored strings directly.

See [native regions](../../../../SB/Optimization/TURBO-NATIVE-REGIONS.md#in-place-comparison-operands).

## Instruction counts

Functional counts of executed 68000 instructions under the test harness's
68000 emulator with the real JS ROM, on the prepared inputs of the GoldCard run.
Host hook steps are excluded. These are **not** cycle counts or elapsed times.
"Before" is HEAD `1251ad5`
([previous result](../2026-11-24-terminal-stop-branches/instruction-counts.csv));
the Turbo FAST executables are the same genuine captures.

| Benchmark | Before | After | Change | Turbo FAST | SB vs Turbo |
|---|---:|---:|---:|---:|---:|
| integer_arithmetic | 405,114 | 405,114 | 0 | 1,144,744 | −64.6% |
| sieve | 238,788 | 238,788 | 0 | 729,202 | −67.3% |
| matrix_multiply | 373,583 | 373,583 | 0 | 635,881 | −41.2% |
| string_operations | 826,599 | 826,599 | 0 | 859,067 | −3.8% |
| float_polynomial | 2,948,841 | 2,948,841 | 0 | 2,977,799 | −1.0% |
| float_transcendentals | 4,453,600 | 4,453,600 | 0 | 4,478,927 | −0.57% |
| eight_queens | 1,322,374 | 1,322,374 | 0 | 3,654,790 | −63.8% |
| route_planner | 2,994,079 | 2,994,079 | 0 | 6,627,612 | −54.8% |
| concordance | 10,986,165 | 8,040,909 | **−26.8%** | 11,260,160 | **−28.6%** |

Every job produced its exact checksum and exited normally.
[Counts with fetch words and sizes](instruction-counts.csv),
[raw counter output](instruction-counts.txt).

- All three of concordance's comparison sites are rewritten: the dictionary
  scan (`dictionary$(scan_i)=token$`), the insertion sort's test
  (`dictionary$(place_i)<=sort_word$`) and the ordering check
  (`dictionary$(scan_i-1)>=dictionary$(scan_i)`). Fetch words fall 26.4%, and
  the executable shrinks from 15,500 to 15,428 bytes.
- The ROM still executes exactly 3,030,381 instructions: `UT.CSTR` compares
  the same strings as before. `get_up`, `copy_upstr`, `similar`,
  `compare_string` and `qdos_compare` no longer appear in the profile, and
  `extr_string` falls from 2,483,780 to 758,940 instructions. The new records
  cost 831,448 (comparison), 739,024 (element) and 492,360 (string), including
  the replayed checks. [Profile](concordance-profile.txt).
- What remains of `extr_string` is mostly the sort's element-to-element copy
  `dictionary$(place_i+1)=dictionary$(place_i)` and single-character `CODE`
  extractions, which this change does not touch.
- The other eight executables are byte-identical.

## QL semantics and fallback

- Each operand record stands where its fetch stood, so every check happens in
  the original order: the element's index checks (negative or beyond a bound
  give the same range error), then each copy's out-of-memory test.
- The out-of-memory tests are those the copies made: `mega_check`'s
  `maths - 128 - length < data` for every non-empty `fch_string_1d`, and for
  `extr_string` only above 14 bytes. The right operand's test counts the heap
  the left copy would have taken. After each operand record the maths stack is
  as deep as after the original fetch.
- Only integer constants, fetches and arithmetic may stand between the two
  operands, so nothing can change the left string before it is compared.
  Calls end a native region anyway.
- Empty strings get `compare_string`'s own results without a ROM call; other
  strings get `UT.CSTR` with the same comparison type and operand order. The
  heap pointer is left where `compare_string` would have reset it.
- Slices, other string expressions and comparisons in threaded code keep the
  copying templates. `--no-native-regions` disables the pass.

## Tests

**4,341 SB compiler tests and 248 runtime tests passed, with zero failures or
skips** ([log](full-suite.txt)).

- The unit-test harness had no `UT.CSTR`, so no test could compare two
  non-empty strings in either mode. It now has one: by comparison type it
  ignores case and compares runs of digits by value. It approximates the ROM's
  collation, so the tests are differential.
- Three programs compare every operator (`= < > <= >= <> ==`) over equal,
  case-different, empty and numeric-suffix strings, with elements and DIMmed
  strings on both sides. Native output must equal threaded output in every
  HIR and register mode. The same three programs were also run under the
  **real JS ROM**, native against threaded, with identical output
  ([ROM differential](rom-differential.txt)).
- Six failure programs must give the same ERNUM and output natively and
  threaded: a negative left index, an out-of-range right index, a left range
  error that must win over an overflowing right index, a recursion that runs
  out of memory, and two loops that grow the heap 64 bytes an iteration until
  a comparison's copies no longer fit. One loop has a plain string on the
  right, the other an element.
- IR tests show a whole-operand comparison is rewritten, and a store inside
  the right operand's index expression prevents it.

Four guards were each disabled in turn and the tests failed every time:
accepting a store between the operands, ignoring the left copy's heap in the
right operand's test (native ran two iterations further before failing),
skipping the out-of-memory test, and skipping the negative-index check.

## Not measured

GoldCard elapsed times were not rerun; the counts above are the only
performance evidence. A quiet-host GoldCard run of the whole collection is
still needed.
