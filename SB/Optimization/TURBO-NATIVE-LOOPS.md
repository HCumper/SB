# Native integer FOR loops

Turbo executable output enables simple native integer FOR loops when both
`--native-regions` and `--regalloc` are enabled (the CLI defaults). Either
`--no-native-regions` or `--no-regalloc` retains the existing loop path. HIR
rewriting remains independently controlled by `--hir-opt` / `--no-hir-opt`.
Programmatic codegen defaults remain byte compatible; explicitly set both
`LocalRegisterAllocation` and `NativeIntegerForLoops` to opt in.

## Selection and execution

`TurboNativeLoops` plans whole loops after the legacy peephole improver, before
both machine code layout passes. It recognizes the ordinary integer range FOR,
done-for marker, single body label, balanced straight-line integer body, matching
NEXT and possible-end marker. The original IR and its listing are unchanged.

The genuine runtime FOR initializer still evaluates start, end and step in order,
captures END and STEP, installs the hidden NEXT/body/continuation addresses,
stores the initial counter and checks whether the first iteration is valid.
Thus empty loops, runtime bounds/steps and expressions referring to the old
counter value keep their existing behavior. Zero STEP retains its runtime
behavior; the compiler does not assume termination.

The original body label remains threaded. On its first entry, FAST enters a
native loop that saves D2-D4, loads the counter into D2 and the captured END/STEP
into D3/D4. Leaf integer operations use the existing register-aware IR and
D0/D1 allocator; counter loads read D2 directly. Every native backedge advances
a candidate with unchecked word wrap, tests it using STEP's captured direction,
and updates D2 only if the next iteration is valid. The independent global
counter's cell is written once on normal exit, or before any runtime error.
Every body counter read uses D2, and no permitted body operation can observe its
cell indirectly. A failed candidate leaves the last counter intact. SLOW resumes
threading once after the loop finishes.

When the last STEP value pushed immediately before the initializer is an integer
literal, NEXT uses that captured literal and its known direction. Small signed
steps use ADDQ/SUBQ; other words use ADDI. Word wrap, empty-loop initialization,
and zero STEP retain the runtime rules. Expressions, variable loads, intervening
labels or mode bridges keep the generic captured-D4 direction test.

The native NEXT wrapper uses byte branches when the final displacement fits
the 68000 signed-byte range and is nonzero. It starts with word branches and
relaxes them to a fixed point, accounting for each shortened forward branch's
own extension word. Larger backedges retain word branches; displacements beyond
the signed-word range retain the ordinary loop path. The embedded leaf body and
its error-recovery branches are unchanged. Both codegen passes use the same
completed byte layout.

Assignments to the variables from which END and STEP were computed cannot change
their captured register values. Other scalar stores still commit immediately;
their existing conservative load-cache invalidation is retained. No accumulator
store is deferred and no array address is cached.

## Eligibility and fallback

This first implementation requires independent global integer storage. Programs
with routine/reference/local binding, error handlers or RETRY/CONTINUE, external
calls, opaque code/memory mutation, or computed transfers retain their existing
loops. Bodies containing branches, labels, calls, generic arrays, floating operations,
counter assignments or nested loops are rejected. An inner loop may
still qualify when its enclosing loop does not. A body must have a balanced
integer maths stack, and the original body label must have only its FOR reference.

At most 256 leaf IR records are lowered per loop. Register-spill limits, stack
headroom and 68000 branch displacement limits cause fallback, rather than a
change in arithmetic policy or an inability to compile an otherwise valid loop.
The selector retains checked arithmetic from threaded body segments; range-proven
unchecked operations remain unchecked only where the preceding audit selected
them. This is a loop-specific extension, not general allocation across CFG blocks.

Checked integer DIV/MOD are supported in the body through the register-aware IR.
They retain QL signed rounding and remainder rules, divisor-zero and quotient
overflow checks, including -32768 MOD -1. Both operand registers are scratch on
success; live accumulators and reused operands use the ordinary spill slots.
The pinned counter, END and STEP remain untouched. See
[register allocation](TURBO-REGISTER-ALLOCATION.md) for recovery and clobber details.

Certified one-dimensional global integer array loads/stores are also eligible.
Their explicit native IR records retain checked or proven bounds semantics and
use only D0/D1 and A2, preserving D2-D4 across each access. Scalar caches are
invalidated after stores; current descriptors are reloaded and no pointer is
hoisted. A bounds failure reconstructs the consumed-prefix maths state and
releases the complete spill plus loop-save depth before the existing runtime
range handler. Unknown-rank, multidimensional and floating accesses remain on
their previous paths. The native loop dump includes its allocated operations.

## Registers, stack and errors

D2-D4 are saved and restored as full long registers. Their 12-byte save area
remains live throughout the native loop; a leaf spill frame may coexist with it.
The complete maximum live depth (12 plus spill bytes) is included in MAX_STACK
and QDOS dataspace. Other leaf/array frames outside the loop do not overlap it.
Normal exit releases both frames and preserves the previous register ABI.

On arithmetic overflow, the leaf recovery snapshot first reconstructs the exact
maths-stack and store state required by the original operation. It then releases
the leaf frame, writes the current D2 counter to its cell, restores D2-D4, and
jumps to the unchanged library error handler. This writeback also occurs for
range errors, divisor zero and quotient overflow.
Checked increments retain their store-before-trap behavior. Error continuation
programs are excluded because their resumption points need a wider CFG model.

## Inspection and verification

`--dump-hir=<prefix>` appends a native FOR summary to `<prefix>.native-ir.txt`,
including selected/rejected counts, source IR byte spans, counter vector,
registers, spill slots, generated size and maximum additional stack. Both codegen
passes consume the same immutable plans. The bundled runtime is unchanged.

Tests execute native loops directly on the 68000 CPU, including signed steps,
word wrap, last-counter stores, overflow cleanup and ABI preservation. CLI tests
compare all six HIR/native/register configurations, captured values, empty loops,
nested loops and unsupported control flow/bindings. Codegen tests cover tight
stack fallback and loop entry beyond 64 KiB. Comparative benchmarks use
`Benchmarks/Turbo/Run-Benchmarks.ps1 -CompareRegisters`; functional instruction
counts are reported separately from QL elapsed timings.
Boundary tests execute backedges at -128 and -130 bytes, including layouts that
only fit after forward branches shrink. Benchmark reports also count consumed
opcode and extension words, exposing fetch reductions when instruction counts
are unchanged. These counts exclude data reads, host hooks and speculative
prefetch; they do not model hardware bus cycles or elapsed QL time.
