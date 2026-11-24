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
done-for marker, single external body entry, balanced integer/conditional body, matching
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
and updates D2 only if the next iteration is valid. The independent
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

The analyzed CLI pipeline can certify each loop's storage bindings. Counters may
be fresh integer LOCals owned by the current procedure/function, or uniquely named
independent globals. Body accesses must resolve without unknown memory effects;
no other referenced root may alias or indirectly observe the counter, and its
reference must not escape to an unknown callee. Formal parameters are excluded
as counters and conservatively excluded from body reads/writes that could alias
the counter. They may still supply captured start, END and STEP values.

Certificates identify the emitted body label, counter vector and allowed scalar
vectors, including each generated value/reference ABI variant. They travel as
in-memory metadata from HIR lowering through native selection to codegen. The
serialized IR and listing are unchanged. Byte-only codegen and programmatic
generation without certificates retain the original program-wide binding
exclusions. The selector still verifies the complete body and every scalar
operand, stack budget and original body-label references.

Binding certificates use the individual loop's CFG region, rather than the
whole-program or whole-scope range gate. Its initializer must own the captured
control at the check and advance, and every edge entering its body/check/NEXT
must come from that initializer or inside the region. A local GOSUB elsewhere
can therefore coexist with an independent native loop. Calls in the body,
counter aliases and externally entered bodies still fail certification.

Error handlers, RETRY/CONTINUE, opaque residents/memory mutation and possible
nonlocal entries retain their program-wide barriers. Recursive calls outside a selected loop retain
the normal runtime binding protocol. Each native invocation commits/restores its
own currently bound counter cell before normal return or error unwinding.
The nonmutating ROM ABS helper is permitted outside a certified loop. Codegen
independently checks its exact resident linkage, result and argument descriptor;
other ROM calls retain their exclusion. Byte-only generation retains all legacy
program-wide restrictions. ABS itself is still a call boundary inside a body.
Bodies may contain nested integer IF/ELSE with acyclic forward edges. Calls,
generic arrays, floating comparisons/arithmetic, counter assignments and nested
loops are rejected. An inner loop may
still qualify when its enclosing loop does not. A body must have a balanced
maths stack, and the original body label must have only its FOR reference.

## Comparisons and conditional branches

`TurboNativeControlFlow` validates stack types and forward edges before allocating
any conditional body. Internal labels accept no external references and must have
an empty maths stack on every incoming edge. Unequal joins, backward edges,
unsupported types or an allocation failure retain the complete ordinary loop.
Each integer leaf has its own local allocation and releases its spill frame before
an edge. Scalar stores commit immediately and caches restart at joins; only D2's
certified counter and captured D3/D4 control values persist across branches.

All seven integer comparisons use signed word comparisons. Integer almost-equal
is exact equality, as in lib537. An immediately consumed comparison/IF pair emits
CMP followed by Bcc, including inverted branch polarity and trailing NOT. It
does not construct a register Boolean with Scc/ANDI and then test it. Spill frames
are released with LEA, preserving CMP's N/V/Z flags through to the branch.
Results needed as values still use CMP/Scc and QL's exact six-byte float 0/1.

A final integer truth test uses TST.W in its result register, or TST.W (A1)+
when a prior operation must leave the word materialized. Its integer-to-float
conversion and result push/pop are removed. Trailing integer/float logical NOT
folds into branch polarity; a redundant final scratch copy can also be forwarded.
These rules run only after the original typed CFG has certified stack joins.

For eligible AND/OR branches, the left operand remains in its original six-byte
form while the complete right operand executes. Only then is the right comparison
or integer truth result used directly, the left float consumed, and the final
branches emitted. This is eager evaluation, with no source short circuit. A
failing right operand still fails even when the left determines the truth.
Previously evaluated float booleans remain on the real maths stack while checked
integer/array leaves reconstruct only their own consumed word prefix on failure.
Value-producing AND/OR and XOR retain their original library templates. Unsupported
tails retain canonical materialization and the existing branch path.

An integer value that must remain a float, including a pending left operand,
uses the exact QL integer-to-float helper with D2/D3 saved and restored. Its save
area plus return address is charged only when such a call remains after fusion.
Canonical boolean-to-integer conversion is direct and cannot fail. General float
coercion/comparison stays on the existing path. Word branches are relaxed to byte
encodings where nonzero displacements fit; larger conditional edges retain words.
No value numbering or register allocation crosses a conditional edge.

At most 256 body IR records are lowered per loop. Register-spill limits, stack
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

Certified one- and two-dimensional global integer array loads/stores are also eligible.
Their explicit native IR records retain checked or proven bounds semantics and
use only D0/D1 and A2, preserving D2-D4 across each access. Scalar caches are
invalidated after stores; current descriptors are reloaded and no pointer is
hoisted. A bounds failure reconstructs the consumed-prefix maths state and
releases the complete spill plus loop-save depth before the existing runtime
range handler. Rank-two accesses check columns before rows and use either an
exact proven column length or the current descriptor length for long offset
arithmetic. Minimum bounds never stand in for exact strides. Unknown-rank,
higher-dimensional and floating accesses remain on
their previous paths. The native loop dump includes its allocated operations.

## Registers, stack and errors

D2-D4 are saved and restored as full long registers. Their 12-byte save area
remains live throughout the native loop; a leaf spill frame may coexist with it.
The complete maximum live depth is 12 plus the larger of the leaf spill frame
and any truth-conversion helper's saves and return address. It is included in
MAX_STACK and QDOS dataspace. Leaf frames are released before helper calls;
other leaf/array frames outside the loop do not overlap it. Normal exit releases
the applicable frames and preserves the previous register ABI.

On arithmetic overflow, the leaf recovery snapshot first reconstructs the exact
maths-stack and store state required by the original operation. It then releases
the leaf frame, writes the current D2 counter to its cell, restores D2-D4, and
jumps to the unchanged library error handler. This writeback also occurs for
range errors, divisor zero and quotient overflow.
Checked increments retain their store-before-trap behavior. Error continuation
programs are excluded because their resumption points need a wider CFG model.

## Inspection and verification

Certified call-free loops retain up to three invariant rank-two row addresses
in A3–A5. HIR effects prove that no body operation replaces the array descriptor;
the row must be a literal or a scalar unchanged by the counter, either IF arm,
or an aliased content/scalar write. D5 holds initialization bits. Full registers
are saved once per loop invocation and restored on normal and error exits;
all saves count against the existing stack budget. Tight budgets, byte-only
generation and bodies with remaining conversion helpers retain ordinary access
code. Rank-one accesses and varying rows keep their existing lowering.

Initialization happens at the first executed original access, after its column
check and row check have succeeded. It retains the complete long data-plus-row
address, including the descriptor's actual stride when the exact stride is
unknown. Later accesses still evaluate their subscript expressions and perform
their column checks in place; the successful invariant row check and row
address calculation are reused. Separate cache identities include the array,
row, check mask and stride, so unchecked accesses cannot suppress another
access's checks. No descriptor, data pointer, check or multiplication is moved
into an empty loop or untaken IF. Stores remain immediate and error recovery
retains the original column-first maths-stack snapshots. Caches reset on every
invocation, allowing DIM, bindings and row values to change between invocations.

`--dump-hir=<prefix>` appends a native FOR summary to `<prefix>.native-ir.txt`,
including selected/rejected counts, source IR byte spans, counter vector,
registers, spill slots, generated size and maximum additional stack. Both codegen
passes consume the same immutable plans. The bundled runtime is unchanged.
The HIR dump lists binding rejections by routine/loop identity, including global
barriers, outside entries, counter ownership/alias/escape and body effects.
The native dump lists each rejected loop's IR byte offset, counter vector and
failed machine checks: unsupported opcodes, stack or spill budgets, unbalanced
values, missing NEXT/end markers and branch displacement limits.
Conditional plans also report direct comparisons, integer truth tests, folded
NOT operations and fused eager AND/OR branches. Branch counts in the original
conditional-body summary refer to source IR edges; generated eager tails can
contain an additional native edge and an internal skip label.
Each loop also reports retained array rows, their proof identities and extra
register saves. `TurboNativeInvariants.fs` plans reuse across allocated leaves;
the two layout passes use the same immutable plan.

Tests execute native loops directly on the 68000 CPU, including signed steps,
word wrap, last-counter stores, overflow cleanup and ABI preservation. CLI tests
compare all six HIR/native/register configurations, captured values, empty loops,
nested loops and unsupported control flow/bindings. Codegen tests cover tight
stack fallback and loop entry beyond 64 KiB. Comparative benchmarks use
`Benchmarks/Turbo/Run-Benchmarks.ps1 -CompareRegisters`; functional instruction
counts are reported separately from QL elapsed timings.
Additional tests cover global/LOCAL shadows, recursion, repeated calls, empty
loops, parameter-supplied bounds, both flexible-argument ABI variants, scalar
reference aliases, certificate mismatches, unchanged IR bytes and local error
cleanup. `local_integer_for.bas` is a dedicated microbenchmark; report it
separately from the application workloads.
Boundary tests execute backedges at -128 and -130 bytes, including layouts that
only fit after forward branches shrink. Benchmark reports also count consumed
opcode and extension words, exposing fetch reductions when instruction counts
are unchanged. These counts exclude data reads, host hooks and speculative
prefetch; they do not model hardware bus cycles or elapsed QL time.

## Call-free string and floating-point bodies

`TurboNativeTemplateLoops.fs` selects small certified integer range FORs whose
string or floating-point operations prevent register-retaining allocation. It
uses lib537's native FOR/NEXT templates: counter, captured END/STEP and iterator
continuations remain in the vector cell. Nothing is cached in registers across
library or ROM arithmetic helpers. Initializers are evaluated in their original
threaded order; the FOR initializer, body, NEXT and empty-loop exit then execute
in a native region. SLOW follows the end marker so both exits restore P mode.

Selection requires the same HIR binding certificate as integer loops, a matching
NEXT, at most 256 body records, known descriptor ranks for indexed operands, no
external entries, and forward local IF edges with empty, equal typed stacks at
every join. Nested loops, procedure/resident calls, handlers, explicit transfers
and unknown operations fall back. Word-only loops retain the existing allocator.
LEN, CODE and CHR$ are classified as nonmutating inline intrinsics; only the exact
MID$/RIGHT$ slice shapes lowered inline by the backend share this classification.
User overrides and general MID$/RIGHT$ calls remain ordinary calls. Argument
effects, allocation and failures are retained.

Integer arithmetic and numeric indexing use the same checked/proven lowering as
straight native regions. Generic string indexing keeps its original checks.
Native append and simple LEN omit missing-string checks present in P mode, so
those two operations use short SLOW/FAST sections with the original P templates.
Native/threaded library IF truth tests agree, including their exponent-word test.
This pass does not change the pinned allocator's existing predicate lowering.

`--no-native-regions` disables selection. `--no-regalloc` disables pinned loops
and local allocation but still permits these memory-counter native loops.
`--no-hir-opt` independently disables HIR rewriting. `.native-ir.txt` reports the
number selected. The library, machine-code generator and legacy peephole pass
are unchanged.

## Memory-based procedure-call boundaries

Known compiled procedures with integer value arguments can execute inside a
native memory-counter FOR. This includes self and mutual recursion. A separate
HIR certificate proves an owned integer counter, entry through the original FOR
initializer, stable captured FOR metadata, resolved effects, and a call-free or
forward-IF body around supported calls. It is never supplied to the register
pinning planner. Scalar aliases and ordinary counter writes are allowed because
the original library FOR/NEXT reads the live cell on every iteration.

Each call uses SLOW, the original threaded GOSUB, and FAST on normal return.
Argument order, LOCAL/formal binding, value/reference ABI variants, return mode
flags, frame allocation and out-of-memory checks remain the runtime's own
protocol. Integer arguments must consume the complete audited maths stack.
The emitter attests the exact procedure entry and value-argument count outside
serialized IR. Functions, reference/array/string/float parameter calls and
unattested targets retain their existing path.

No scalar, counter register, array value or address cache survives a call. Leaf
register allocation materializes values and releases its frame before SLOW;
the continuation starts a fresh leaf and fetches the restored vector cells.
Each recursive activation retains its own LOCAL counter and captured END/STEP.
Shared writes by callees remain visible. Calls that may replace caller FOR
metadata, unknown effects, handlers/resumption and unsupported control retain
threaded loops. The existing native-disable flags still apply; `--no-regalloc`
continues to permit the memory-counter path.

Diagnostics report `Memory-counter native procedure call boundaries` separately
from register-retaining loops. Regression tests cover self/mutual recursion,
LOCAL strings and scalar shadows, shared arrays, ordinary counter writes,
captured bounds, empty/descending loops, multiple arguments, ABI variants,
reference/handler fallback and matching arithmetic/bounds/out-of-memory errors.
