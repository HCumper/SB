# Compiling Turbo's own parser: Turbo and SB

Turbo 5.09 compiles its own parser (`parser509`, 4,369 lines) in **94.9 s** at GoldCard speed
under Q-emuLator. SB compiles the identical file in **5.5 s** with full optimization (Release
build) on a Windows PC, or **2.9 s** without optimization.

| Compiler | Where it runs | Optimization | Compile time, s | Output, bytes |
|---|---|---|---:|---:|
| Turbo 5.09 (parse + codegen) | Q-emuLator, GoldCard speed | `TURBO_optim 2`, `TURBO_model ">"` | 94.87 (94.873, 94.872) | 354,960 |
| SB, Release build | i7-13700 PC | `--hir-opt --native-regions --regalloc` | 5.51 (5.4–5.6) | 160,060 |
| SB, Debug build | i7-13700 PC | the same | 5.59 (5.5–5.8) | 160,060 |
| SB, Release build | i7-13700 PC | `--hir-opt --native-regions` | 5.35 (5.3–5.4) | 159,950 |
| SB, Release build | i7-13700 PC | HIR optimization only | 3.64 (3.6–3.7) | 88,814 |
| SB, Release build | i7-13700 PC | none | 2.91 (2.9–3.0) | 88,828 |

The two sides run on different machines, so these are not like-for-like. Turbo compiles inside
the emulated QL; SB is a cross-compiler running on .NET, and its times include .NET startup
and JIT. SB times are the median (range) of five runs, or three for the last three rows
([SB timings](sb-timings.txt)). Another CPU-heavy program ran on the PC throughout, using about
a third of its threads, so SB's absolute times are approximate; times measured back to back
compare fairly.

- Native regions, mostly the HIR analysis behind them, take about 1.7 of SB's 5.5 seconds. HIR
  optimization adds about 0.7 s and register allocation about 0.2 s; the rest is parsing,
  semantic analysis and code generation.
- Turbo's two runs agree to the millisecond and produced byte-identical object files. Its report
  gives 0 errors, 11 warnings ("only exists as a LOCAL") and a parse rate of 10,384
  statements per minute ([report](turbo-run3/p509_report.txt)).
- Neither compiled parser was run. Both need the Turbo Toolkit, and SB's needs nothing else: its
  29 resident names are all Turbo Toolkit routines from Turbo's own name table.

### Earlier SB figures

The first figures given here, 27.06 s (Release) and 36.32 s (Debug) at commit 404837e, were
wrong. Those five Release runs did take 27 s each, but rebuilt from 404837e the next day the
same compiler takes 15.20 s (14.9–15.3) and makes the same executable; the host was more
heavily loaded the first time. They were followed by 15.75 s for commit 5400ccb, and the
difference was credited to 5400ccb. It changes the executable but not the compile time:
measured back to back, 404837e takes 15.20 s and 5400ccb 15.45 s (14.9–15.5).

5400ccb fixed five constant slices, such as `PROG$(1 TO 31)` (P 12706). HIR optimization had
folded their lengths, so code generation no longer recognised them as slices and called a
resident `MID$` instead, which neither the QL ROM nor these toolkits provide.

At both commits the HIR analysis behind native regions took 11.3 of the 15.2 s. It now takes
1.7 s and produces the same results, as described under
[compile cost](../../../../SB/Optimization/HIR-ANALYSIS.md#compile-cost).

## The source both compilers compiled

Both compiled the same file (SHA-256 `bc46254655d4adc253dc3fee46071eb1fec091d3d5123128b7fad14f369e2e57`):

1. `I:\QL\Turbo\turbo source\parser509.bas` is a text copy of the 5.09 parser in which 12 QL
   characters (arrows and ©, in 5 lines) had become U+FFFD. [`restore-ql-bytes.py`](restore-ql-bytes.py)
   puts back the original bytes, taken from the same text in the QL-native `parser502_bas`.
2. Six lines use `$hex` literals, which Minerva and SMSQ/E accept but the JS ROM's SuperBASIC
   loads as mistakes. The first Turbo run stopped with 7 errors on those lines.
   [`decimalise-hex.py`](decimalise-hex.py) rewrites each in decimal, reading 8-digit values with
   the top bit set as signed 32-bit, as SMSQ/E does: `$FFFF92BC` becomes -27972.
3. Lines 1–11 are the compile-time directives the benchmark collection uses, naming this
   program's output, with a 300 KB `TURBO_buffersz` and 100 KB `TURBO_objdat`.

## How Turbo was timed

Q-emuLator 3.5.1 at GoldCard speed, with the JS ROM, Toolkit II and 640 KiB RAM; Turbo
Toolkit 3.44 loaded; the genuine `parser_task` and `codegen_task` of Turbo 5.09
([configuration](turbo-run3/turbo.QCF)). The boot program loads the Toolkit, queues one
command line with `TYPE_IN`, then `LOAD`s the program. SuperBASIC runs the queued line when
`LOAD` returns:

```
OPEN_NEW #3,mdv1_s:CLOSE #3:EXECUTE_W mdv1_parser_task;"*":EW mdv1_codegen_task:OPEN_NEW #3,mdv1_e:CLOSE #3
```

`"*"` runs the parser without its dialogue. The host creation times of files `s` and `e`
bracket parse plus code generation; `LOAD` (about 2.5 minutes) is outside. The emulator was
launched hidden, opted out of Windows power throttling, pinned to the performance cores at high
priority, as in the [valid GoldCard runs](../2026-11-24-goldcard-whole-collection/README.md),
which kept real time in all 144 jobs. Host CPU stayed at a median of 4.9% during run 3. No
emulated tick count was taken here, so real time is inferred from those settings and from the
two runs agreeing to the millisecond.

[`prepare-turbo-compile.py`](prepare-turbo-compile.py) builds a run directory and
[`Run-Turbo-Compile.ps1`](Run-Turbo-Compile.ps1) runs it. Both hold this machine's paths.

## What SB needed

SB could not compile this program before. Its parser recovered from 144 syntax errors and
semantic analysis reported 82; once those were fixed, code generation reported 193 more. The
changes, each tested and each confirmed by disabling it, are described in
[Turbo's SuperBASIC dialect](../../../../docs/turbo-dialect.md):

- `&&`, `||` and `^^` were mapped to the logical `AND`, `OR` and `XOR`, so masks such as
  `(type% && last_mask%) <> 0` gave wrong answers. They are now bitwise, with `~~`.
- `ELSE` anywhere in a block IF's lines, a short IF closed by its own `END IF` with statements
  after it, and `END REPeat` after statements on a line.
- `DEBUG`/`DEBUG_LEVEL` conditional compilation, `RETRY_HERE`, `COMPILED`, the Turbo Toolkit's
  procedures and functions, and `PRINT \`.
- A routine indexing its caller's `LOCal` array, `LEN` of a number, `RETurn` converting to the
  function's type, and `MANIFEST` names in DIM bounds, which had been dropping a dimension.
- Turbo's argument passing: a formal not named by `REFERENCE` takes a value when the argument
  cannot be bound by reference (a different type, an array element or an expression).
- Constant slice bounds, such as `PROG$(1 TO 31)`, stay slices under HIR optimization (see
  [earlier SB figures](#earlier-sb-figures)).

Each of the 18 changes made at least one test fail when disabled. Each new Turbo-dialect
program runs in six modes (HIR optimization, native regions and register allocation on and
off). All 4,455 SBTests and 248 SBRuntimeTests pass. One existing test expected `LEN(1)` to
be rejected; it now checks `CODE(1)`. The nine
benchmark programs still compile to the executables timed in the
[GoldCard whole-collection run](../2026-11-24-goldcard-whole-collection/README.md), byte for
byte.
