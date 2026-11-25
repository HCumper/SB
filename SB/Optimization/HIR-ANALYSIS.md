# HIR analyses for QL output

`HirAnalysis.analyze target program` returns immutable analysis results used by
Turbo native-region selection. The pass runs after the optional HIR rewrite and
before Turbo IR emission. Disabling HIR rewriting does not disable analysis for
native output. `--no-native-regions` retains the threaded path; the legacy
programmatic generator helpers retain their existing behavior.

`--dump-hir=<prefix>` writes `<prefix>.analysis.txt` beside the before/after HIR
dumps. The report includes node/edge identities, reachability, dominators,
natural loops, liveness, entry/exit intervals, expression ranges, effects,
escaping references, widening thresholds, narrowing/certification status,
captured FOR metadata, array shapes and native check-removal decisions. Analysis can also
be requested directly through the F# API without generating code.

## Compile cost

The solver revisits each node many times (worklist, certification, narrowing),
and call-entry solving re-solves whole scopes. Everything it would recompute
that does not depend on flow state is therefore computed once per analysis:

- each node's evaluation and full effects, per scope and node;
- each scope's declared array bounds, and the arrays re-DIMmed anywhere;
- alias expansions and name resolutions, which `HirEffects` keeps per alias or
  storage table (an empty set expands to nothing without a scan);
- effect summaries: each round of the interprocedural fixed point re-evaluates
  only the callers of a summary that changed in the round before.

Liveness is computed only when the analysis dump reads it. None of this changes
a result: Turbo's parser and the benchmark programs produce byte-identical
executables and analysis dumps. On Turbo's parser (4,369 lines, 258 scopes) the
analysis takes 1.7 s instead of 11.3 s.

## Control flow

Each elementary operation or source anchor has its own block. IF diamonds,
counted loops, REPeat, named NEXT/EXIT, literal/computed jumps, ON transfers,
GOSUB/RETURN and exceptional transfers have explicit edges. Numbered anchors
inside structured bodies remain independently reachable: jumping into an IF
does not acquire that IF's guard. STOP/QUIT have no normal fallthrough.

Unknown targets connect conservatively to numbered anchors, entry and exit.
GOSUB returns include conservative continuation edges. Possible exceptions
connect to handlers with unknown storage facts. These graphs support
reachability, dominators, natural-loop discovery and backward liveness.

Arithmetic and array proofs are eligible per scope, after its fixed point,
control flow and captured FOR ownership have been certified. Local GOSUB
contexts, enumerated FOR sequences and overwritten FOR continuations withhold
these proofs in the affected scope. They do not disable independent procedures.
Computed/missing or cross-scope line targets, error handlers/resumption, raw
memory and opaque residents can affect other scopes and retain program-wide
barriers. A normal RETURN and a literal transfer to an anchor in the same scope
do not introduce a foreign entry. Uncertain CFG edges remain conservative.

## Ranges and arithmetic proofs

Target-typed integer scalar values, captured loop metadata and guaranteed array
dimensions are tracked. A QL `%` suffix and IMPLICIT
rules take precedence over advisory HIR types. The abstract domain uses signed
16-bit intervals, a full-word unknown value and a distinct unreachable state.
Branches refine integer comparisons and suitable logical conditions. Joins
include every incoming path. After three changes, cycle cut points widen to
finite thresholds drawn from integer literals and their adjacent values, plus
the QL word endpoints. Large literal sets are sampled deterministically to bound
compile time; dropping a threshold affects precision only. Guards retain their
bounds inside loops, including in graphs with unstructured back edges.

Up to eight narrowing passes recompute each block from all predecessor states
in reverse postorder, rather than retaining the widened joins. This recovers
invariants whose endpoints were computed at runtime and were not thresholds.
The solver certifies that its result covers the entry state and every abstract
transfer/edge equation before proofs can be used. Incomplete narrowing remains
sound; a failed certification withholds check-removal proofs. The F# API
`analyzeWithOptions` exposes threshold and narrowing settings for diagnostics.

FOR end and step intervals are captured when the initializer evaluates them,
including variable bounds. Assigning those source variables in the body does
not change the captured facts. Guards use the captured step's known direction;
mixed signs retain unknown bounds. The matching initializer must dominate FOR
checks and advances: jumping between bodies cannot borrow another loop's hidden
NEXT address. Captures also carry their initializer identity: another FOR using
the same vector invalidates ownership even if the earlier initializer still
dominates. Ordinary integer counter stores preserve ownership. Invalid
ownership withholds arithmetic/array proofs in that scope.
FOR advancement models the library's unchecked
word wrap. The synthetic candidate counter is invalidated on loop exit because
Turbo only stores it when the next iteration is valid. Overflowing arithmetic,
possible zero division, quotient overflow and uncertain coercions produce
unknown ranges rather than mathematical results that the QL cannot represent.
DIV rounds down and MOD follows the divisor's sign.

A proof permits native add/subtract/multiply/negate only when the mathematical
result fits a word for all inputs and all occurrences of that exact expression.
Proof identities include the expression and its CFG context, not just the line
number. Effects during sibling-expression evaluation invalidate relevant facts
before a proof is attempted. Effectful evaluations do not receive unchecked
arithmetic proofs. The analysis annotates HIR-to-Turbo lowering without rewriting
HIR, propagating values or moving operations.

## Array bounds proofs

An executed DIM establishes a rank and guaranteed minimum upper bound for each
dimension. Dynamic bounds may be used when their complete integer intervals
are nonnegative. Joins retain the common rank and minimum upper bounds from
every path. Storage declarations alone do not establish reaching facts when a
DIM exists; implicit entry allocations follow the backend's explicit rules.

Exact shape is tracked separately in `EntryExactBounds`. A DIM supplies exact
bounds only when each evaluated dimension has a singleton interval. Joins keep
an exact shape only when every incoming path agrees; an elementwise minimum is
only a bounds guarantee. The same effect/alias invalidations kill both maps.
This prevents a branch selecting different row strides, or a later DIM, from
turning a safe minimum bound into an incorrect address calculation.

Element stores preserve descriptor facts. Calls, unknown memory writes, scalar
aliases and DIM/rebinding invalidate affected descriptor facts before reuse.
Routine parameters have no assumed entry shape. Effects in sibling expressions
prevent proofs, and INPUT/READ targets cannot inherit another occurrence's proof.
All occurrences of an access key must have the same safe conclusion, including
distinct scopes sharing a dynamically bound name.

Numeric accesses lose bounds checks only if every index is an integer interval
inside the current guaranteed bounds, the rank matches, and control and solver
certification are trusted. The native selector emits 1611/1612/1643/1644 for
proven integer/float loads/stores. Other accesses retain checked helpers. Calls
still reload current vectors; this does not cache addresses or hoist accesses.

Rank-one integer accesses are also recorded independently of index-range safety
in `OneDimensionalIntegerAccesses`. Every occurrence of the access key must have
a certified reaching one-dimensional descriptor and ordinary numeric access
semantics. Unknown indexes can therefore use checked register-aware operations;
fully proven indexes use their unchecked variants. Unknown ranks, ambiguous
dynamic bindings, sibling writes and uncertain continuation contexts retain the
generic helpers. Neither source subscript count nor declaration rank alone is
treated as a reaching shape proof.

`TwoDimensionalIntegerAccesses` records reaching rank-two facts, separate row
and column safety, and optional exact lengths. Every occurrence must have a
trusted shape; range proofs are intersected and exact shapes must agree across
all occurrences. Native IR uses the live descriptor column length otherwise.
Unknown ranks, sibling mutation, ambiguous bindings and uncertain continuation
contexts retain the generic path. This enables fixed-rank native FOR accesses
without assuming dimensions are constants or caching descriptor pointers.

## Allocated strings

A DIMmed string variable (`DIM s$(n)`, `String` storage) gets a one-element
bound from its DIM, like an array. Unlike other scalar stores, an assignment to
it keeps that bound: `st_str_var_1d` copies the value into the existing buffer,
truncated to its capacity, and never replaces the descriptor. Other descriptor
writes (calls whose summaries write it, unknown writes, INPUT/READ) still kill it.

`AllocatedStringSites` holds the HIR expressions of append values (the Concat in
`s$=s$&x`) and `LEN(s$)` calls whose string is allocated at every occurrence: the
node's entry state, after effects evaluated earlier in that node, still holds
the string's bound, and the scope is proof-eligible. The name must belong to
exactly one storage in the program, so no LOCAL or formal can dynamically rebind
it; a dynamic reference in a routine then names the DIMmed string. String bounds
do not join the call-entry shape propagation: sites inside routines stay
checked. A path that skips the DIM, such as a GO TO around it, leaves the site
checked, as does any program-wide barrier. The backend tags proven 374/375
records so native code may omit P mode's missing-string checks.

## Effects and aliases

`HirEffects.analyze` computes interprocedural MAY summaries to a fixed point,
including recursion. Summaries distinguish scalar/array roots read and written,
descriptor writes, captured FOR metadata writes, unknown memory, possible
exceptions, I/O, allocation, control barriers and calls.
Unknown residents remain full memory/effect barriers. Known calls map formal
effects to actual references. Scalar `ValueArg` temporaries do not write their
caller's variable; array references and element writes retain their owner root.
The one-argument ROM maths functions (ABS, ACOS, ACOT, ASIN, ATAN, COS, COT,
DEG, EXP, INT, LN, LOG10, RAD, SIN, SQRT, TAN) have a narrow nonmutating effect
summary: their argument effects, possible coercion/allocation/domain failure and
call boundary remain. They cannot replace descriptors or FOR continuations, and
they raise no program-wide barrier. RND updates generator state and stays
opaque. A user-defined routine of the same name uses its actual summary;
unknown functions retain their full effects.

Array-element stores invalidate affected values but preserve descriptor and
captured FOR metadata. Scalar stores conservatively invalidate affected
descriptor facts, while ordinary counter-word assignments leave END/STEP/NEXT
intact. DIM and FOR initialization invalidate both metadata categories. Unknown
residents and memory operations retain all barriers. Calls project each category
independently: an element-only array formal cannot resize its actual array, and
a scalar reference to one element changes its owner's contents rather than its
descriptor. Calls in subscripts/RHS expressions retain their own metadata effects.

Named accesses resolve to the routine's own formal/LOCAL binding when present.
A free name resolves to a unique global only if no declared formal or LOCAL in
the program can shadow it. Otherwise its memory effects remain unknown. This
closed-program check avoids assuming lexical binding for QL's dynamic names.
Private formal/LOCAL effects are removed at a normal return, with reference
effects mapped onto the actual arguments. Same-named private vectors do not
automatically become writes to a caller's vector; bindings are restored by the
existing runtime. Recursive summaries and reference escape tracking use the
same conservative resolution rules.

Alias queries are scope-aware. Same-named storage is conservative because Turbo
uses dynamically rebound vectors. A parameter may alias another parameter or
visible storage, unless every static call site passes it a scalar value: such a
formal is bound to a private temporary and aliases nothing. A bare variable or
array argument, a missing argument, a reference formal, or a routine with no
call site at all keeps the conservative rule. Independent global variables are not made aliases merely
because one parameter could point to either: MAY alias is not transitively
closed. Array elements share their owner region. Address-taken roots and
references escaping through known forwarding calls to unknown callees are
recorded separately.

Writes invalidate every possible alias before range facts are reused. A strong
update adds a fact only for the assigned scalar, leaving possible aliases
unknown. A call's MAY writes never become definite kills in liveness analysis.
Unknown calls, unresolved dynamic writes and unmodeled mutations discard
affected value facts. Descriptor and loop facts are killed independently by
their corresponding metadata effects.
String/coercion/allocation effects are classified conservatively, so some safe
operations intentionally remain barriers.

STOP/QUIT retain their evaluated argument effects and terminate control flow;
they do not represent arbitrary writes to loop metadata.

## Global-array shapes at call entry

Known procedures and functions can inherit guaranteed bounds and exact shapes
from their call sites. Only uniquely named global arrays qualify: any declared
formal or LOCAL with the same name prevents propagation for that array. An
integer formal bound to an integer value argument also starts with that
argument's interval, joined over every call site. A bare-variable argument may
alias the actual and passes nothing, as do missing arguments, reference
formals and any call whose argument evaluation can write storage. Other scalar
values, formal/LOCAL array shapes and captured FOR metadata are not imported.
All statically known reachable call sites contribute, including calls in other
routine bodies. Uncalled routine bodies are also considered conservatively.

Joins retain a common rank and the minimum guaranteed bounds. Exact dimensions
survive only when every caller agrees; differing lengths use the live descriptor
stride. A call before allocation, a possible descriptor mutation or a rank
mismatch removes the corresponding entry fact. Argument/channel evaluation and
sibling expression effects are applied before passing facts. For calls inside
expressions this conservatively includes the callee's own MAY effects, so a
mutating function may receive fewer facts than a standalone procedure.

The outer solver computes a least fixed point. Main and routines with no static
call site start from the unknown entry; every other routine starts unreachable
and gains entry states only from reachable call sites, so entries only grow.
A recursive call therefore contributes exactly the shapes that hold on the paths
from an entry back to that call: the shape is an inductive invariant of every
activation, never an assumption used to prove itself. A DIM or other descriptor
change on the recursive path joins its smaller or different shape into the
entry. Routines whose callers all stay unreachable, such as a mutually
recursive pair never called from main, are then analyzed from the unknown entry.
Only scopes whose entry facts change are reanalyzed. Each pass joins the new
call-site states into the entries. A recursive parameter can grow one step per
pass (`walk (d%+1)`), so an entry that has changed twice widens to the callee's
literal thresholds, as loop heads do. A guard such as `IF d%=9 THEN RETurn`
then excludes the endpoint: `<>` narrows an interval whose bound it equals.
Each scope still requires its normal range/CFG certification, and the outer
solution must be a post-fixpoint: every call site's state is covered by its
callee's entry. An untrusted caller
contributes unknown shapes at every static call site, including sites whose
reachability it cannot certify. It is never silently omitted from a callee's
MUST facts. Independent trusted callers/callees can still propagate shapes.
Program-wide barriers withhold propagation. The pass is bounded to 64 rounds; nonconvergence or a
failed certificate discards all imported facts and retains the original scope
analysis. `--dump-hir` reports call-entry rounds and certification status.

Procedure entry bindings cannot shadow propagated globals, while body aliases
and DIM continue to invalidate shapes through the precise effect categories.
Standalone native array operations reload descriptors/data pointers, preserve
checked accesses for unknown indexes and retain generic fallback. Certified
native loops can retain invariant row addresses as described below. This
analysis does not allocate registers across calls.

## Native FOR binding certificates

Certified loop regions can identify independent integer LOCAL counters owned by the
current routine and uniquely named global counters. A call-free
body must have resolved effects, no counter writes, no escaped counter reference
and no other read/write root that may alias the counter. Formal body accesses
remain conservative; formal values may still supply the captured loop bounds.
Only simple range FOR statements qualify, with no imported counter values or
register state across calls. Nested IF arms are included in the effect proof;
calls, counter/alias writes and other unsupported statements still reject it.
An IF arm of assignments and PRINTs ending in STOP is terminal: it never
returns to the loop, so it does not reject the certificate, though its effects
still count (a PRINT of a call, or a write to the counter, still rejects it).
Only memory-counter native loops can use such a body; the register-pinning
planner refuses STOP and PRINT records.
Inline intrinsics and the nonmutating resident ABS are not rejected as calls:
their argument effects are part of the proof, and the machine planner refuses
any body in which an ABS ROM call remains rather than the native integer form.
The downstream typed native CFG independently validates branch targets, join
stacks and supported integer/boolean operations.
The source CFG records each range FOR's initializer, check, advance and members.
Its own initializer must own the captured control, and all outside edges must
enter through it. This binding proof can succeed even when local uncertainty
elsewhere withholds range proofs for the surrounding scope. Global control
barriers still reject every binding certificate. Diagnostics list both the
proof-eligible scopes and each rejected binding's reasons.
`ArithmeticProofsEnabled` now means that at least one scope is eligible; the
`ProofEligibleScopes` set identifies exactly which ones. Proofs for an expression
or array key still require every occurrence, across all scopes, to be safe.

Certificates additionally identify unchanged scalar bindings and array descriptors.
Scalar invariance excludes the counter, every body write and any possible alias
of those writes, including content writes through reference parameters and both
IF arms. Array descriptor invariance uses the separate descriptor-write effects;
ordinary element stores do not invalidate it. Escaped storage is excluded.
The emitter translates these certificates into body-label/counter/scalar/array-vector
metadata for every routine ABI variant. The metadata is kept outside the IR byte
stream and consumed by the existing whole-loop planner, which independently
checks supported body instructions, operand coverage, references and budgets.
Legacy byte-only generation remains conservative. The ordinary initializer,
counter writeback and spill recovery remain unchanged. The native loop planner
may retain a literal or certified invariant scalar row's complete long address,
initializing it only at the first executed, successful access. Extra saved
registers are included in its stack budget and restored on errors. Unknown
descriptors, changing/aliased rows and remaining helper calls retain normal
address calculation. This does not infer an exact stride from minimum bounds.
