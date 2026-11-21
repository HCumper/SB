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
code generation. It inserts ordinary Turbo FAST (254) and SLOW (255) records.
The `.lst` output marks each selected run. It uses the existing library and
machine code generator; the runtime binary is unchanged.

The first audit covers numeric scalar loads and stores, numeric literals,
integer logical NOT, integer comparisons, integer DIV/MOD, numeric duplicates,
and integer-to-float conversion. Scalar accesses reload the vector entry each
time, preserving flexible parameter bindings. Comparisons retain Turbo's float
Boolean representation. DIV/MOD use the same checked template in both modes,
including signed rounding, zero divisors and quotient overflow.

Integer add, subtract, multiply and negate can now run natively when the HIR
control-flow, range, effect and alias analyses prove that every occurrence fits
a signed QL word. Otherwise they and their increment peepholes retain the
threaded overflow checks. Array and string operations,
floating arithmetic, allocation, ROM routines, calls, returns, loops, jumps,
handlers and retry markers also remain threaded pending separate audits.
Every label stays in its original mode, and every selected run returns to
threaded execution before a control-flow or unsupported instruction. Startup,
DATA, linkage and resident-name records are unchanged.

Selection requires estimated instruction savings to exceed the six instructions
in the two mode bridges. This is a conservative dispatch instruction count, not
a cycle model or a guarantee of elapsed-time improvement on a physical MC68008.
Trailing operand loads remain threaded so selection preserves the generator's
checked increment peepholes. Large native array/string templates are excluded.

Analysis runs on the optimized HIR, before IR emission. `--dump-hir=<prefix>`
also writes `<prefix>.analysis.txt` with CFG edges, dominators, natural loops,
liveness, ranges, effects and proof decisions. See [HIR analysis](HIR-ANALYSIS.md)
for the assumptions and conservative continuation/alias rules.

Executable generation can then lower leaf integer sequences into a
register-aware native IR and allocate D0/D1 locally. This defaults on in the CLI;
`--no-regalloc` retains native templates. See
[local register allocation](TURBO-REGISTER-ALLOCATION.md) for spill, alias and
runtime-boundary guarantees.

Existing programmatic `generateTurbo*WithOptions` helpers keep their threaded
behavior for compatibility. Call `generateTurbo*WithNativeRegions` with the HIR
options and an independent Boolean to select native runs. The low-level pass is
`TurboNativeRegions.select`; disabling it returns the original IR unchanged.

Validation includes all four combinations of HIR optimization and native
selection, checked failures and signed DIV/MOD, routine bindings and recursion,
arrays and branches, plus byte comparisons with the original Turbo generator.
