# Compact checked native numeric arrays

Audited native regions now include integer and six-byte QL floating-point array
loads and stores. Each access reloads the current vector and calls a shared
native helper. There is no threaded dispatch inside the helper, and bounds
checks are always retained. Enable this with `--native-regions` (the CLI default);
`--no-native-regions` retains the previous threaded body. The helpers work with
both `--regalloc` and `--no-regalloc`.

## Representation and size

The selected IR uses explicit IDs 1111/1112 for integer load/store and 1143/1144
for float load/store, each taking one vector operand. These are SB extensions
in native mode; SB's generator is required. The original 111/112/143/144 native
templates and ordinary threaded streams keep their existing implementation.

Code generation emits one indexing routine and only the operation wrappers used
by that executable. All four wrappers plus indexing total 128 bytes; integer
load/store together need 82 bytes. Every call site is eight bytes:

```asm
MOVEA.L vector(A6),A2
JSR     operation(A6)
```

The original per-access native templates occupy 44/46/54/56 bytes respectively.
Helpers live in the addressable runtime prefix and participate in both codegen
layout passes. Calls use A6-relative addresses even when the body exceeds 64 KiB.
The runtime binary `lib537.bin` is unchanged.

## Indexing, effects and errors

The descriptor supplies the current data pointer, rank-minus-one and reversed
dimension lengths. Subscripting consumes the last source subscript first and
checks negative values before checking the upper bound, matching lib537. The
first subscript initializes the linear offset directly, removing the multiply
by one and corresponding addition. Remaining dimensions retain lib537's unsigned
word-stride multiplications and long offset accumulation. Integer offsets are
scaled by two; float offsets by six. Six-byte float values are copied exactly,
without arithmetic or coercion. Index zero and a dimension length of 32768 are
handled with the existing runtime representation.

Stores consume their value before checking subscripts, as the P templates do,
and change no array element until every dimension passes. On failure, the maths
stack has consumed exactly the same value and subscript prefix as threaded
code. The indexer releases both return addresses before jumping to the unchanged
range-error handler at A6+$8AE0 (ERNUM -4). WHEN ERROR, RETRY, CONTINUE and routine
binding unwinding continue through the runtime's normal paths.

Numeric accesses are register-allocation boundaries. All pending words are
materialized and private spill frames released before the helper call; scalar
load caches start afresh afterward. Neither element addresses nor descriptors
are cached across accesses. Aliases, local array rebinding and global resizing
therefore always use the current vector contents.

Scratch registers are D0-D5, A0 and A2. D6 (the runtime line number), D7, A3-A6
and the caller's CPU stack pointer are preserved. A1 follows the maths-stack
protocol. The helper's maximum CPU-stack depth is eight bytes. Codegen reserves
the maximum of this depth and a local allocation frame in both MAX_STACK and
QDOS dataspace; the frames cannot overlap. If the configured stack has less than
eight bytes of headroom, or helpers would exceed the prefix's addressable space,
accesses use the original checked inline N templates.

## Boundaries and validation

DIM, allocation, string arrays and slicing, routine calls and control transfers
retain their existing paths. Every label remains in its original mode. This
work adds numeric access helpers and does not eliminate array bounds checks.

Tests execute emitted helpers directly on the 68000 CPU with synthetic
descriptors, then exercise real CLI executables against threaded output. They
cover multidimensional addresses, signed and upper bounds, large offsets, exact
float bits, partial failure stack consumption, unchanged failed-store targets,
aliases, resizing, local rebinding, retries and register-spill boundaries.

Run the full comparison with `Benchmarks/Turbo/Run-Benchmarks.ps1 -CompareRegisters`.
This compares HIR off/on across threaded, native-template and allocated modes.
The reported instruction counts are functional 68000 counts, not calibrated
MC68008 elapsed timings. Two ROM-dependent workloads compile only.
