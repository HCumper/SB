# Fused native integer conditions

Turbo compiles `IF a%=0 AND b%(i%)=0 THEN` as maths-stack templates: each
comparison pushes Turbo's six-byte float Boolean, `AND`/`OR` combine floats, and
IF TRUE/IF FALSE pops and tests the result. With `--regalloc` (the CLI default),
SB's code generator replaces such a condition with one register-aware leaf and a
single native branch to the original label. `--no-regalloc` or
`--no-native-regions` retains the templates. The IR stream and listing are
unchanged; the plan exists only inside code generation.

## Selection

`TurboNativeConditions.plan` runs after whole-loop planning and before ordinary
leaf allocation. A candidate is an IF TRUE/FALSE in native mode whose label is
defined later in native mode. Today such branches occur in memory-counter
native FOR bodies, including the recursive call regions. Working backwards from
the branch, it takes the shortest suffix that produces the tested value from an
untouched maths stack, using only:

- leaf integer operations accepted by the register-aware IR, including checked
  and range-proven rank-one/rank-two integer array loads;
- the seven integer comparisons, with `==` as exact integer equality;
- Boolean `AND`/`OR` of comparison results;
- a final integer truth test (`IF x%`), and trailing float `NOT`s, which only
  change the branch polarity.

Preceding complete straight-line statements on the same native run join the
leaf, so values just stored stay in registers. Labels, line records, mode
bridges, calls, floats, strings, `XOR`, an inner `NOT` and everything else end
the candidate; it then keeps the original templates. Whole-loop plans own
their bodies and are excluded. A leaf is limited to 256 IR records.

## Semantics

QL `AND` and `OR` do not short-circuit. Every operand is still evaluated in
source order, so a checked array subscript or arithmetic overflow in a later
operand fails even when an earlier comparison already decides the result. The
comparisons become `CMP`/`Scc`/`ANDI.W #1` words; `AND.W`/`OR.W` combine them.
The final comparison is consumed directly by the branch (`CMP` then `Bcc`), and a
final AND/OR's own zero flag is used without a separate test.

A left-associative chain `t1 AND t2 AND ...` (or `OR`) of comparison terms may
skip terms that can neither fail nor write: loads, literals, range-proven
arithmetic and array reads. Terms up to the last one that can fail are
evaluated eagerly in one leaf; each later term is its own leaf, and a deciding
value (false for AND, true for OR) branches at once. Depending on the IF's
polarity that branch goes to the original label or to the code after the
condition. Skipping a term that cannot fail or write is unobservable.

A failure snapshot reconstructs the exact P-template maths stack. Each pending
Boolean is rebuilt as the six-byte QL float 0 or 1 (`RecoveryBoolean`), below
the failing operand's consumed prefix, exactly where the original comparison
template would have left it. The spill frame is released before the unchanged
runtime handlers, as for every other native leaf.

## Branch encoding

Each branch follows its leaf, after the leaf's spill frame has been released
with `LEA`, which preserves the flags. A branch to the label is a 4-byte
`Bcc.W`. If the label is out of word range, code generation is repeated with
that condition's label branches given 12-byte slots: an inverted `Bcc.S` over
lib537's own 10-byte native long jump (`MOVE.L #label,D0; JMP 0(A6,D0.L)`), or
its native-to-P jump for a P label. Each retry fixes one more condition, so
generation terminates. An early exit to the end of the condition is always a
`Bcc.W`; conditions are limited so that it is always in range.

## Inspection and validation

`--dump-hir=<prefix>` appends `Fused native conditions: selected=...` to
`<prefix>.native-ir.txt`, with each condition's IR byte range, branch condition,
comparison and AND/OR counts, body size, spill slots and allocated instructions.

Unit tests execute AND/OR flags, integer truth tests and every Boolean snapshot
location on the 68000 CPU. CLI tests compare all HIR/native/register modes,
check that later failing operands keep their QL error numbers, and confirm the
report counts. A codegen test pads an IF body beyond 32 KiB to force the long
jump retry.
