# HIR analyses for QL output

`HirAnalysis.analyze target program` returns immutable analysis results used by
Turbo native-region selection. The pass runs after the optional HIR rewrite and
before Turbo IR emission. Disabling HIR rewriting does not disable analysis for
native output. `--no-native-regions` retains the threaded path; the legacy
programmatic generator helpers retain their existing behavior.

`--dump-hir=<prefix>` writes `<prefix>.analysis.txt` beside the before/after HIR
dumps. The report includes node/edge identities, reachability, dominators,
natural loops, liveness, entry/exit intervals, expression ranges, effects,
escaping references and native arithmetic proof decisions. Analysis can also
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
enumerated FOR sequences and possibly overwritten FOR continuations withhold unchecked-arithmetic proofs
for the whole program. Their CFG edges remain available; the existing audited
native scalar/comparison/DIV/MOD regions still work. This is a deliberate limit
until their continuation state is modeled precisely.

## Ranges and arithmetic proofs

Only target-typed integer scalar values are tracked. A QL `%` suffix and IMPLICIT
rules take precedence over advisory HIR types. The abstract domain uses signed
16-bit intervals, a full-word unknown value and a distinct unreachable state.
Branches refine integer comparisons and suitable logical conditions. Joins
include every incoming path. Cycle cut points use widening; guards then retain
their bounds inside loops, including in graphs with unstructured back edges.

FOR end/step constraints are used only when constant; runtime-evaluated values
are not reread as if unchanged. FOR advancement models the library's unchecked
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
arithmetic proofs. No HIR propagation, reassociation, loop motion, bounds-check
removal or register allocation is performed by this change.

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
