# Direct string append

Turbo lowering recognizes `text$ = text$ & suffix` for a whole simple string
variable, including local, implicit-string and reference bindings. It evaluates
the suffix and emits the existing Turbo append instruction 374 instead of
fetching the old string, concatenating and assigning the complete result.

The operation stays threaded, using lib537's checked P-mode implementation.
It checks that the destination exists, consumes the suffix, and copies only the
characters that fit the current descriptor. A full destination leaves its value
unchanged; it still evaluates the suffix, including any failing arithmetic or
slice operation. Capacity and odd-length rounding follow Turbo's descriptor.
There is no extra combined-length overflow check: Turbo's append truncates to
capacity without one. No runtime library or machine-code template was changed.

The RHS must not mutate or rebind the destination while it is evaluated. Literal
and variable expressions, array/character reads and their arithmetic qualify.
Fixed-runtime CHR$, CODE, LEN, FILL$, INSTR, PI, ERNUM and ERLIN operations qualify
when their arguments do. The exact MID$/RIGHT$ forms emitted for string slices
also qualify. Compiled functions, replaceable resident functions and reference
arguments retain the original fetch/concatenate/store sequence, preserving the
prefix snapshot, evaluation order and alias behavior. A user-defined function
with a built-in name is not assumed pure.

String array elements, destination slices, reversed concatenations and nested
left-prefix concatenations retain their existing lowering. In particular, the
pass does not split an expression into multiple destination writes. Suffix
errors therefore leave the destination unchanged, and WHEN ERROR/RETRY continue
through the existing runtime checkpoints.

This is a Turbo-compatible lowering choice, like specialized LEN and slice
lowering, and applies with the HIR pass enabled or disabled. It does not extend
native-region or register-allocation eligibility to strings.

Regression tests cover empty/full/truncated strings, self and slice aliases,
local and reference bindings, mutating function fallbacks, missing destinations,
RHS bounds/division errors, repeated RETRY, and a combined length above 32767.
The execution cases run with HIR optimization on/off and with threaded, native
template and allocated-native modes.
