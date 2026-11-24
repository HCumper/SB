# Audited native regions

Turbo output enables audited native regions by default in the CLI. Use
`--no-native-regions` for the previous threaded executable body, or
`--native-regions` to enable selection explicitly. `AppSettings:TurboNativeRegions`
provides the default. These switches are independent of `--hir-opt` and
`--no-hir-opt`, and apply to both `turbo` and `turboir` output.

```powershell
dotnet run --project .\SB\SB.fsproj -- program.bas program_exe false turbo --native-regions
dotnet run --project .\SB\SB.fsproj -- program.bas threaded_exe false turbo --no-native-regions
```

The pass runs after HIR optimization and HIR-to-Turbo-IR lowering, before machine
code generation. It inserts Turbo FAST (254) and SLOW (255) records and explicit
checked arithmetic records described below. The `.lst` output marks each selected
run. The runtime library binary is unchanged.

The first audit covers numeric scalar loads and stores, numeric literals,
integer logical NOT, integer comparisons, integer DIV/MOD, numeric duplicates,
and integer-to-float conversion. Scalar accesses reload the vector entry each
time, preserving flexible parameter bindings. Comparisons retain Turbo's float
Boolean representation. DIV/MOD use the same checked template in both modes,
including signed rounding, zero divisors and quotient overflow.

Integer add, subtract, multiply and negate run natively with overflow checks.
The HIR control-flow, range, effect and alias analyses may remove checks only
when every occurrence provably fits a signed QL word. Unproven operations become
explicit checked records 1001/1005/1006/1007; these select the existing checked
library bodies even in native mode. The improver preserves them as checked
increments 1339/1340, whose native expansion commits the wrapped destination
before trapping, matching the original threaded increment's observable behavior.
Numeric array loads and stores use shared checked native helpers (1111/1112
and 1143/1144), with eight-byte call sites. See
[compact numeric arrays](TURBO-NATIVE-ARRAYS.md) for bounds, alias, stack and error
semantics. String operations, floating arithmetic, allocation, ROM routines,
calls, returns, loops, jumps,
handlers and retry markers also remain threaded pending separate audits.
Every label stays in its original mode, and every selected run returns to
threaded execution before a control-flow or unsupported instruction. Startup,
DATA, linkage and resident-name records are unchanged.

## Integer ABS

`x%=ABS(word)` compiles to int-to-float (320), the one-float-argument ROM ABS
resident (224) and float-to-int (334). Inside a selected native run, that exact
triple becomes the SB-only record 1334, a checked integer ABS. It is chosen only
when the 224 vector is bound solely to the resident named ABS, so a program's
own ABS function keeps its call. Any other ABS use, such as a floating result,
keeps the ROM call, and a triple left in threaded code keeps its three records.

The result is exact: every word except -32768 has an exact float magnitude and
converts back unchanged. For -32768 the ROM returns 32768.0, which
`fast_float_to_int` pops and then rejects with overflow (ERNUM -18). The native
form raises the same error through the same library handler, with the operand
consumed and the pending words below it intact. It can no longer fail for lack
of memory inside the ROM call, which needed none for a valid argument.
Without register allocation, 1334 expands inline to `TST.W (A1)`, `BPL.S`,
`NEG.W (A1)`, `BVC.S`, then the overflow exit. With allocation it is a
register `TST.W/BPL.S/NEG.W` followed by the usual checked-arithmetic `BVC.W`.
Validation rejects 1334 in threaded code.

This assumes compiled programs use the QL ROM's ABS, as the effect analysis
already does: a resident extension that later redefines ABS would no longer
be called for these integer assignments.

[Direct string append](TURBO-STRING-APPEND.md) uses Turbo's existing checked
threaded append operation for safe self-concatenating assignments. It avoids
copying the old prefix without extending native-region eligibility to strings.

Selection requires estimated instruction savings to exceed the six instructions
in the two mode bridges. This is a conservative dispatch instruction count, not
a cycle model or a guarantee of elapsed-time improvement on a physical MC68008.
The estimate accounts for the improver's existing checked increments, retaining
threaded output for isolated increments whose native bridges would cost more.
Trailing operand loads remain threaded to avoid incomplete regions. Checked
increment peepholes remain together within a selected region. Large native
array/string templates are excluded; numeric accesses use compact helpers.

The explicit checked records use existing library IDs with an SB-defined native
meaning. Streams containing them require SB's code generator: the original Turbo
generator normalizes away the +1000 distinction. Use `--no-native-regions` when
exporting IR to that original generator. Existing raw native opcodes 1/5/6/7 keep
their unchecked semantics; checks are never silently added or removed there.

Analysis runs on the optimized HIR, before IR emission. `--dump-hir=<prefix>`
also writes `<prefix>.analysis.txt` with CFG edges, dominators, natural loops,
liveness, ranges, effects and proof decisions. See [HIR analysis](HIR-ANALYSIS.md)
for the assumptions and conservative continuation/alias rules.

Executable generation can then lower leaf integer sequences into a
register-aware native IR and allocate D0/D1 locally. This defaults on in the CLI;
`--no-regalloc` retains native templates. See
[local register allocation](TURBO-REGISTER-ALLOCATION.md) for spill, alias and
runtime-boundary guarantees. Native integer IF conditions additionally become
one leaf and one branch; see [fused native conditions](TURBO-NATIVE-CONDITIONS.md).

Checked integer DIV/MOD now participate in register allocation rather than
splitting leaf blocks. Both operand clobbers and the original failure maths-stack
state are explicit in the native IR; QL signed corrections and existing runtime
errors are preserved. They are also eligible in simple native integer FOR bodies.

With register allocation enabled, a later machine-code plan also selects
[simple native integer FOR loops](TURBO-NATIVE-LOOPS.md). It keeps the runtime
initializer and threaded entry/exit labels, retains counter/END/STEP in registers,
and replaces per-iteration bridges and NEXT dispatch with a native backedge.
Unsupported bodies keep the straight-line region behavior described above.

Existing programmatic `generateTurbo*WithOptions` helpers keep their threaded
behavior for compatibility. Call `generateTurbo*WithNativeRegions` with the HIR
options and an independent Boolean to select native runs. The low-level pass is
`TurboNativeRegions.select`; disabling it returns the original IR unchanged.

Validation includes all four combinations of HIR optimization and native
selection, checked failures and signed DIV/MOD, routine bindings and recursion,
arrays and branches, plus byte comparisons with the original Turbo generator.
