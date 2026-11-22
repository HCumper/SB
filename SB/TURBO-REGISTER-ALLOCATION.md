# Local allocation for Turbo native regions

Turbo executable output enables local register allocation by default when native
regions are enabled. `--no-regalloc` selects the existing native templates;
`--regalloc` enables allocation explicitly. The default configuration key is
`AppSettings:TurboLocalRegisterAllocation`. HIR optimization, native-region
selection and register allocation have independent switches. Allocation has no
effect when native regions are disabled.

```powershell
dotnet run --project .\SB\SB.fsproj -- program.bas program_exe false turbo --native-regions --regalloc
dotnet run --project .\SB\SB.fsproj -- program.bas templates_exe false turbo --native-regions --no-regalloc
```

## Pipeline and representation

The register-aware stage operates inside SB's machine code generator, after Turbo's fixed-width
peephole improver and before either layout/emission pass:

1. Split native code at labels, mode switches and unsupported operations.
2. Lower leaf integer sequences into immutable virtual word values.
3. Allocate D0/D1 using next-use information and two-address destination reuse.
4. Record overflow recovery locations without inserting flag-clobbering moves.
5. Emit 68000 instructions from the allocated representation.

The virtual IR represents constants, scalar vector loads/stores, maths-stack
inputs/outputs, negation, integer add/subtract/multiply and checked DIV/MOD. Duplicates share a
virtual value. Increment operations introduced by the old improver are lowered
too. Explicit checked operations carry a recovery snapshot of the maths stack.
Range-proven operations retain their unchecked form; other selected arithmetic
keeps its checks. The local positive-literal division proof described below
can remove DIV/MOD error exits because every signed-word dividend is safe.

Eligible integer FOR bodies can also use [native loops](TURBO-NATIVE-LOOPS.md),
keeping their counters, captured bounds and steps in D2-D4 across backedges.
This loop-specific plan saves those registers and restores them on normal and
overflow exits; ordinary leaf allocation still uses D0/D1.

Calls, branches, array/string operations and floating operations
remain allocation boundaries. Numeric arrays use
[compact checked native helpers](TURBO-NATIVE-ARRAYS.md); all pending maths words
are materialized and private frames released before calling them. Other
unsupported operations use their existing templates. No allocated value or
private frame crosses these boundaries.
Both codegen passes use the same plan, byte lengths and original IR offsets, so
label addresses and mixed-mode jumps account for the generated instructions.

Certified one-dimensional integer arrays are the exception to the generic
array boundary: explicit load/store IR operations retain index/value operands
in registers and use A2 to reload each current descriptor and data pointer.
Addressing preserves the index with two ADDA.W instructions, forming a long
byte offset without a destructive shift. Loads have a distinct result operand
and use a free scratch register to retain a live dynamic index when possible.
Pinned D2 indexes need no scratch copy. Stores can read registers, literal words
or existing spill slots directly, including a shared index/value. Stores invalidate scalar
load caches to preserve element/scalar aliases. Checked accesses carry range
failure snapshots; proven ones omit bounds checks. Both work inside eligible
native FOR loops without borrowing D2-D4. See
[one-dimensional arrays](TURBO-NATIVE-ARRAYS.md) for shape and recovery rules.

## Registers, memory and spills

D0/D1 and A2 are scratch. A1 retains its maths-stack role; A4, A5 and A6 retain
their heap, threaded-PC and base roles. All other registers are preserved.
Condition codes are scratch except between arithmetic and its overflow check.
No reload or spill can be inserted there. General cross-block register
allocation and floating-point allocation are not performed. Native integer FOR
plans retain only their explicitly pinned counter/END/STEP registers.

Stores commit immediately and invalidate all cached variable loads. Only the
value just stored to that same vector can subsequently be reused. This preserves
flexible/reference aliases, including different vectors addressing one cell.
Vector addresses are reloaded for each actual memory access. Cached values on
the virtual expression stack remain immutable even when an aliased cell changes.
Scalar stores accept registers, literal words and existing spill slots directly;
they still reload the current target vector and commit at the original point.

Register pressure spills word values into reusable CPU-stack slots. A leaf block
reserves its frame with LEA and releases it before every exit. Maths-stack inputs
are consumed in order and remaining values are materialized before any boundary.
The maximum extra leaf frame is reserved in both the runtime stack setting and
the QDOS task dataspace, preserving the previous usable stack/heap capacity.
Frames are capped at 128 bytes and at the configured stack-limit headroom. A
block that cannot fit falls back to its existing templates.

Literal words are materialized only when required, and rematerialized instead of
spilled. An evicted constant is
recreated with MOVEQ or MOVE.W when needed, including after DIV/MOD and array
index clobbers. Error snapshots can retain an immediate word and write it
directly to the maths stack on the cold failure path. No materialization is
inserted between arithmetic and its overflow check. Dynamic values still use
the ordinary bounded spill frame; this does not introduce memory reloads or
change alias assumptions.

Eligible native FOR bodies additionally rematerialize their read-only pinned
counter from D2. Read-only arithmetic operands and error snapshots use its word directly,
without spilling the counter across DIV/MOD or array indexing. This context is
used only after whole-loop selection proves independent storage, prohibits
counter writes and excludes calls and other observers; ordinary leaf allocation
never treats mutable vector memory as rematerializable.

Add/subtract/multiply with a literal operand use immediate 68000 instructions
without allocating a second operand register. Add/subtract by signed 1 through 8
use ADDQ/SUBQ; other words use ADDI/SUBI, and multiply uses MULS.W #word.
Commuting is limited to add/multiply. Checked forms still immediately test V or
the full long product, with their original left-word recovery snapshots.

## Overflow and error recovery

Checked add, subtract and negate test the 68000 V flag. Checked multiply compares
the full signed long product against -32768 and 32767; MULS alone cannot detect
overflow of a word result. Each check has a non-returning failure path which
materializes the exact P-template maths-stack state, releases the entire private
CPU-stack frame, then jumps to the unchanged library overflow handler at
A6+$8AD8. No native allocation state survives into WHEN ERROR, RETRY or CONTINUE.
The runtime restores the saved retry registers and unwinds routine bindings.

DIV/MOD are atomic checked native IR operations with explicit failure snapshots.
They test the divisor before DIVS and test V immediately afterward, preserving
zero-divisor and -32768/-1 errors through the same lib537 overflow handler.
The original left maths word and all pending words are reconstructed on failure;
an assignment following a failed operation does not commit. Normal leaf error
handling retains WHEN ERROR, RETRY, CONTINUE and routine-binding unwinding.

For the general checked path, both operand registers are clobbered on success. A shared
dividend/divisor gets a separate scratch copy; values still live after DIV/MOD
are spilled before the operation and reloaded as needed. No extra data register
is borrowed from a native loop's pinned counter/END/STEP. The word dividend is
sign extended for DIVS. A nonzero remainder with a different sign from the
divisor adjusts the quotient down or adds the divisor to the remainder, retaining
QL floor division and divisor-signed MOD for every signed-word operand pair.
MOD retains the quotient-overflow check even when its mathematical remainder
would fit. A positive literal divisor cannot fail for any signed-word dividend.
It uses DIVS.W #word with a correction only for a negative remainder; no second
scratch register or error snapshot is required. Positive powers of two use
ASR.W for floor DIV and ANDI.W with divisor-minus-one for MOD instead, including
negative dividends. These specializations preserve the dividend if it remains
live. Zero, negative and dynamic divisors retain the full checked path; in
particular, -32768 MOD -1 still reports quotient overflow.

Inside a native FOR, division failure releases the leaf spill frame and restores
the loop's saved D2-D4 before entering the runtime handler. Recovery storage is
charged to the existing spill budget, stack reserve and QDOS dataspace; a block
that cannot fit retains the original checked templates. Selection charges the
success-path checks, corrections, copies and spills while excluding cold recovery
payloads, just as for checked add/subtract/multiply.

The original threaded increment peephole updates its variable before checking
overflow. Checked native increments preserve that wrapped store on failure,
including reference aliases. Ordinary binary assignments commit only after the
check succeeds. Pending stores before the failed operation remain visible.

Recovery snapshots retain immutable values in registers, spill slots or literal words, even
when a destructive arithmetic instruction overwrites its left operand. Blocks
are limited to 256 source IR records so all inline recovery branches fit the
68000 word displacement. Spill-budget or cost rejection falls back to checked
native templates; disabling allocation never disables overflow checking.

Selection compares estimated success-path instruction-fetch and data-access
words with the original templates, charging every check and spill but excluding
non-returning failure payloads. Candidates that do not improve this estimate also
fall back. Recovery snapshots can increase executable size even when the success
path improves.
This is an allocation cost estimate, not a calibrated MC68008 timing model.

## Inspection and comparison

`--dump-hir=<prefix>` on the `turbo` backend also writes
`<prefix>.native-ir.txt`, containing virtual and allocated instructions, spill
slots, source IR byte ranges and generated byte counts. The ordinary `.lst`
retains the selected Turbo IR, including explicit checked records. These have
an SB-defined native meaning and require SB's code generator; use
`--no-native-regions` to export to the original Turbo generator. The `turboir`
backend does not perform final machine code allocation.

The low-level `TurboCodegen.defaultCodegenOptions` keeps allocation disabled for
reference-generator compatibility. Set `LocalRegisterAllocation=true` or use
`generateTurboExecutableFromLoweredHirWithRegisterAllocation` explicitly from
F#. Results expose the plan in `TurboCodegenOutput.NativeAllocation`.

Run comparisons with:

```powershell
.\Benchmarks\Turbo\Run-Benchmarks.ps1 -NoBuild -CompareRegisters -OutputDirectory C:\Source\codex-workspace\.scratch\turbo-regalloc\benchmarks
```

This compares threaded output, native templates and allocated native code, each
with HIR rewriting disabled/enabled: six configurations. Reports include an
independent `RegisterAllocation` field. Counts are functional 68000 instructions;
two ROM-dependent workloads compile but require a full QL environment to execute.
