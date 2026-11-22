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

GOSUB contexts, unknown/missing targets, error-handler RETRY/CONTINUE protocols,
enumerated FOR sequences and possibly overwritten FOR continuations withhold arithmetic and array check-removal proofs
for the whole program. Their CFG edges remain available; the existing audited
native scalar/comparison/DIV/MOD regions still work. This is a deliberate limit
until their continuation state is modeled precisely.

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
ownership withholds proofs program-wide.
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

## Effects and aliases

`HirEffects.analyze` computes interprocedural MAY summaries to a fixed point,
including recursion. Summaries distinguish scalar/array roots read and written,
unknown memory, possible exceptions, I/O, allocation, control barriers and calls.
Unknown residents remain full memory/effect barriers. Known calls map formal
effects to actual references. Scalar `ValueArg` temporaries do not write their
caller's variable; array references and element writes retain their owner root.

Alias queries are scope-aware. Same-named storage is conservative because Turbo
uses dynamically rebound vectors. A parameter may alias another parameter or
visible storage. Independent global variables are not made aliases merely
because one parameter could point to either: MAY alias is not transitively
closed. Array elements share their owner region. Address-taken roots and
references escaping through known forwarding calls to unknown callees are
recorded separately.

Writes invalidate every possible alias before range facts are reused. A strong
update adds a fact only for the assigned scalar, leaving possible aliases
unknown. A call's MAY writes never become definite kills in liveness analysis.
Unknown calls, dynamic writes and unmodeled mutations discard affected facts.
String/coercion/allocation effects are classified conservatively, so some safe
operations intentionally remain barriers.

STOP/QUIT retain their evaluated argument effects and terminate control flow;
they do not represent arbitrary writes to loop metadata.
