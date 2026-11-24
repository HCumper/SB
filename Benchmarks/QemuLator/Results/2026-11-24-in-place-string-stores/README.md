# In-place string stores, CODE and LEN

This finishes the in-place string work for concordance. The
[previous step](../2026-11-24-in-place-string-comparisons/README.md) read
comparison operands where they are stored. What remained of `extr_string` was
the insertion sort's element copy `dictionary$(place_i+1)=dictionary$(place_i)`,
the other string assignments, and `CODE` and `LEN` of elements and
single-character slices. Each of these copied its operand to the string heap
only for the consumer to release it straight away.

Where a store, `CODE` or `LEN` directly follows its operand in native code, the
operand is now read in place and the consumer is an SB-only record that
releases nothing:

- **1534** and **1538** are the element and DIMmed-string store templates
  (`st_string_var`, `st_str_var_1d`) without the `SUBA.L` that released the
  copy. Truncation, the string-not-DIMmed exit and the index checks are
  unchanged.
- **1535** and **1536** give `CODE`'s and `LEN`'s results.
- **1537** reads any `extr_string` slice in place: the same bound and slice
  checks, and the copy's out-of-memory test above 14 bytes. Whole elements and
  DIMmed strings use the earlier 1531 and 1532.

See [native regions](../../../../SB/Optimization/TURBO-NATIVE-REGIONS.md#in-place-stores-code-and-len).

## Instruction counts

Functional counts of executed 68000 instructions under the test harness's
68000 emulator with the real JS ROM, on the prepared inputs of the GoldCard run.
Host hook steps are excluded. These are **not** cycle counts or elapsed times.
"Before" is HEAD `432fb8f`
([previous result](../2026-11-24-in-place-string-comparisons/instruction-counts.csv));
the Turbo FAST executables are the same genuine captures.

| Benchmark | Before | After | Change | Turbo FAST | SB vs Turbo |
|---|---:|---:|---:|---:|---:|
| integer_arithmetic | 405,114 | 405,114 | 0 | 1,144,744 | −64.6% |
| sieve | 238,788 | 238,788 | 0 | 729,202 | −67.3% |
| matrix_multiply | 373,583 | 373,583 | 0 | 635,881 | −41.2% |
| string_operations | 826,599 | 825,575 | −0.12% | 859,067 | −3.9% |
| float_polynomial | 2,948,841 | 2,948,841 | 0 | 2,977,799 | −1.0% |
| float_transcendentals | 4,453,600 | 4,453,600 | 0 | 4,478,927 | −0.57% |
| eight_queens | 1,322,374 | 1,322,374 | 0 | 3,654,790 | −63.8% |
| route_planner | 2,994,079 | 2,994,079 | 0 | 6,627,612 | −54.8% |
| concordance | 8,040,909 | 7,643,493 | **−4.9%** | 11,260,160 | **−32.1%** |

Every job produced its exact checksum and exited normally.
[Counts with fetch words and sizes](instruction-counts.csv),
[raw counter output](instruction-counts.txt).

- Concordance rewrites four stores and three `CODE`/`LEN` sites. `extr_string`
  and the heap-copy runtime no longer appear in its profile; the ROM's work is
  unchanged at 3,030,381 instructions. Its executable shrinks from 15,428 to
  15,180 bytes. [Profile](concordance-profile.txt).
- Across the in-place work, concordance has gone from 10,986,165 to 7,643,493
  instructions (−30.4%), and from 2.4% to 32.1% ahead of Turbo FAST.
- string_operations' `CODE(text$(1))` and `CODE(text$(64))` read in place
  (−1,024 instructions). The rotation `text$=text$(2 TO) & text$(1)` is a
  concatenation and keeps its copies.
- The other seven executables are byte-identical.

### What is left in concordance

The largest remaining compiler costs are the in-place records themselves
(element access 968,968, comparison 831,448, string 500,852, element store
423,408). They replay what the originals checked: the element records walk
the array descriptor with two `MULU`s, and each operand repeats its copy's
out-of-memory test. About 410k instructions go on mode switches and threaded
IF tests in code that stays threaded, including the insertion sort's `REPeat`
loop, which has no native loop form. Branching directly on a string comparison
instead of building a floating-point Boolean would also save the result push
and IF test, roughly 10 instructions per comparison.

## QL semantics and fallback

- A consumer is rewritten only when its operand is the record directly before
  it, so nothing runs between the read and the use.
- A store's source may be a slice of its own destination. Its text always
  starts at or after the destination's text and both copy loops run forward,
  so the overlap is safe; the length word written first lies before it.
- Slices lowered to `MID$` calls instead of `extr_string`, such as some with
  constant bounds, keep that path. Concatenation still copies its operands.

## Tests

**4,361 SB compiler tests and 248 runtime tests passed, with zero failures or
skips** ([log](full-suite.txt)).

- Two programs run in every HIR, native and register mode against threaded
  output:
  - element and DIMmed-string stores in both directions, self-assignment,
    truncation into a short DIM, slices (including a long overlapping
    self-slice on the `mega_move` path) and empty sources;
  - `CODE` and `LEN` of whole strings, elements and slices, including index 0
    (the whole string), positions past the current length and empty slices.
- Report tests check the rewritten store and `CODE`/`LEN` counts with HIR
  optimization off and on.
- Six failure programs give the same ERNUM and output natively and threaded:
  - a slice end beyond the DIM, a negative slice, a negative start, and a
    slice ending before its start;
  - a store to an element out of range;
  - a 100-byte slice whose copy's out-of-memory test fails as the heap grows.

Four guards in the slice record were each disabled in turn and a test failed
every time: its out-of-memory test, the negative-start check, the
end-beyond-DIM check and the end-before-start check.

## Not measured

GoldCard elapsed times were not rerun; the counts above are the only
performance evidence.
