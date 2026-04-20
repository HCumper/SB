# Built-In Status

Current status of recognized SuperBASIC and project-specific built-ins.

Primary references:

- [SB/BuiltIns.fs](/C:/Source/SB/SB/BuiltIns.fs)
- [SB/Interpreter.fs](/C:/Source/SB/SB/Interpreter.fs)
- [SB/CSharpRuntime/GeneratedRuntime.cs](/C:/Source/SB/SB/CSharpRuntime/GeneratedRuntime.cs)
- [SB/CRuntime/sbruntime_c.c](/C:/Source/SB/SB/CRuntime/sbruntime_c.c)
- [SBRuntime/BuiltInFunctions.fs](/C:/Source/SB/SBRuntime/BuiltInFunctions.fs)
- [SBRuntime/BuiltInStatements.fs](/C:/Source/SB/SBRuntime/BuiltInStatements.fs)
- [SBRuntime/ManagedBuiltInBridge.fs](/C:/Source/SB/SBRuntime/ManagedBuiltInBridge.fs)

Status markers:

- `[x]` implemented for the named runtime/backend.
- `[~]` implemented with simplified or host-specific behavior.
- `[ ]` recognized but unsupported.
- `n/a` handled by syntax/lowering rather than as a callable runtime built-in.

## Summary

The interpreter has the broadest built-in coverage and is the reference backend for new behavior.

Generated C# has meaningful runtime support and delegates a growing subset to `SBRuntime.ManagedBuiltInBridge`, but it still uses its own generated runtime for storage, channels, screen state, graphics projection, and error control.

Generated C has a smaller runtime surface. It supports core output, some file/channel operations, and a subset of screen/graphics calls, but many higher-level or host-specific operations remain unsupported.

`SBRuntime` currently owns shared expression built-ins, PRINT/INPUT formatting/parsing helpers, and bridge helpers for generated C# screen, graphics, and file/channel statements. It is not yet the single runtime for all interpreter and generated backend behavior.

## Shared SBRuntime Functions

Implemented in `SBRuntime/BuiltInFunctions.fs` and usable by the interpreter and generated C# bridge:

- `[x] ABS`
- `[x] ACOS`
- `[x] ACOT`
- `[x] ADATE`
- `[x] ASC`
- `[x] ASIN`
- `[x] ATAN`
- `[x] CHR$`
- `[x] CODE`
- `[x] COS`
- `[x] COT`
- `[x] DATE`
- `[x] DATE$`
- `[x] DAY$`
- `[x] DEG`
- `[x] EOF`
- `[x] EXP`
- `[x] FILL$`
- `[x] GETENV$`
- `[x] INKEY`
- `[x] INKEY$`
- `[x] INSTR`
- `[x] INT`
- `[x] KEYROW`
- `[x] LEFT$`
- `[x] LEN`
- `[x] LN`
- `[x] LOG`
- `[x] LOG10`
- `[x] MID$`
- `[x] PI`
- `[x] RAD`
- `[x] REPL$`
- `[x] RIGHT$`
- `[x] RND`
- `[x] ROUND`
- `[x] SGN`
- `[x] SIN`
- `[x] SQRT`
- `[x] STR$`
- `[x] TAN`
- `[x] TIME`
- `[x] VAL`

Shared statement helpers in `SBRuntime/BuiltInStatements.fs`:

- `[x] PRINT` formatting, including comma zones and trailing semicolon continuation markers.
- `[x] INPUT` line splitting and typed value parsing.

## Interpreter

Interpreter-only or interpreter-reference support in `SB/Interpreter.fs`:

- `[x] PRINT`
- `[x] INPUT`
- `[x] REFERENCE`
- `[x] RANDOMISE`
- `[x] STOP`
- `[x] GOTO`
- `[x] GOSUB`
- `[x] RETURN`
- `[x] ON-GOTO`
- `[x] ON-GOSUB`
- `[x] WHEN ERROR` runtime state, including `REPORT`, `CONTINUE`, `RETRY`, `ERLIN`, `ERNUM`, and `ERR_*` predicates.
- `[x] DATA`
- `[x] READ`
- `[x] RESTORE`

Interpreter file, channel, and program-management built-ins:

- `[~] OPEN`
- `[~] OPEN_IN`
- `[~] OPEN_NEW`
- `[~] APPEND`
- `[~] CLOSE`
- `[~] EOF`
- `[~] DIR`
- `[~] DELETE`
- `[~] COPY`
- `[~] COPY_N`
- `[~] MOVE`
- `[~] LOAD`
- `[~] LRUN`
- `[~] MERGE`
- `[~] MRUN`
- `[~] NEW`
- `[~] RUN`

Interpreter screen/window/control built-ins:

- `[~] AT`
- `[~] BORDER`
- `[~] CHAR_USE`
- `[~] CLEAR`
- `[~] CLS`
- `[~] CSIZE`
- `[~] CURSOR`
- `[~] FLASH`
- `[~] INK`
- `[~] MODE`
- `[~] OVER`
- `[~] PALETTE`
- `[~] PAN`
- `[~] PAPER`
- `[~] RECOL`
- `[~] S_FONT`
- `[~] SCROLL`
- `[~] STRIP`
- `[~] UNDER`
- `[~] WIDTH`
- `[~] WINDOW`

Interpreter graphics built-ins:

- `[~] ARC`
- `[~] ARC_R`
- `[~] BLOCK`
- `[~] CIRCLE`
- `[~] CIRCLE_R`
- `[~] DLINE`
- `[~] DRAW`
- `[~] ELLIPSE`
- `[~] ELLIPSE_R`
- `[~] FILL`
- `[~] LINE`
- `[~] LINE_R`
- `[~] PENDOWN`
- `[~] PENUP`
- `[~] PLOT`
- `[~] POINT`
- `[~] POINT_R`
- `[~] SCALE`
- `[~] TURN`
- `[~] TURNTO`

Interpreter host/time/memory built-ins:

- `[~] BEEP`
- `[~] BEEPING`
- `[~] FLUSH`
- `[~] PAUSE`
- `[~] PEEK`
- `[~] PEEK_W`
- `[~] PEEK_L`
- `[~] POKE`
- `[~] POKE_W`
- `[~] POKE_L`
- `[~] SDATE`
- `[~] SLUG`
- `[~] WAIT`

Interpreter no-op compatibility placeholders:

- `[~] TURBO_BUFFERSZ`
- `[~] TURBO_DIAGS`
- `[~] TURBO_LOCSTR`
- `[~] TURBO_OBJDAT`
- `[~] TURBO_OBJFIL`
- `[~] TURBO_OPTIM`
- `[~] TURBO_REPFIL`
- `[~] TURBO_STRUCT`
- `[~] TURBO_TASKN`
- `[~] TURBO_WINDO`

## Generated C# Backend

Generated C# has direct runtime support for:

- `[x] PRINT`
- `[x] INPUT`
- `[x] REFERENCE`
- `[x] RANDOMISE`
- `[x] STOP`
- `[x] RETRY`
- `[x] CONTINUE`
- `[x] REPORT`
- `[x] ERLIN`
- `[x] ERNUM`
- `[x] ERR_*`
- `[x] DATE`
- `[x] EOF`
- `[x] DIMN`
- `[x] BEEP`
- `[x] BEEPING`
- `[x] FLUSH`
- `[x] MODE`
- `[x] PAUSE`
- `[x] WAIT`
- `[x] SDATE`
- `[x] SLUG`
- `[x] PEEK`
- `[x] PEEK_W`
- `[x] PEEK_L`
- `[x] POKE`
- `[x] POKE_W`
- `[x] POKE_L`

Generated C# delegates these statement families through `ManagedBuiltInBridge` or generated runtime state:

- `[~] OPEN`
- `[~] OPEN_IN`
- `[~] OPEN_NEW`
- `[~] APPEND`
- `[~] CLOSE`
- `[~] DELETE`
- `[~] COPY`
- `[~] COPY_N`
- `[~] MOVE`
- `[~] DIR`
- `[~] CLS`
- `[~] WINDOW`
- `[~] AT`
- `[~] CURSOR`
- `[~] CSIZE`
- `[~] CHAR_USE`
- `[~] S_FONT`
- `[~] INK`
- `[~] PAPER`
- `[~] STRIP`
- `[~] BORDER`
- `[~] CLEAR`
- `[~] SCROLL`
- `[~] WIDTH`
- `[~] PAN`
- `[~] RECOL`
- `[~] PALETTE`
- `[~] PLOT`
- `[~] DRAW`
- `[~] DLINE`
- `[~] LINE`
- `[~] LINE_R`
- `[~] CIRCLE`
- `[~] CIRCLE_R`
- `[~] ELLIPSE`
- `[~] ELLIPSE_R`
- `[~] ARC`
- `[~] ARC_R`
- `[~] BLOCK`
- `[~] FILL`
- `[~] SCALE`
- `[~] OVER`
- `[~] UNDER`
- `[~] FLASH`
- `[~] PENDOWN`
- `[~] PENUP`
- `[~] TURN`
- `[~] TURNTO`

Generated C# explicitly rejects these host/program-management built-ins:

- `[ ] RUN`
- `[ ] LOAD`
- `[ ] LRUN`
- `[ ] MERGE`
- `[ ] MRUN`
- `[ ] SAVE`
- `[ ] NEW`
- `[ ] SEXEC`
- `[ ] SBYTES`
- `[ ] CALL`
- `[ ] RESPR`
- `[ ] LOADMEM`
- `[ ] SAVEMEM`
- `[ ] SYSVAR`

## Generated C Backend

Generated C supports core value/runtime operations, but the runtime surface is narrower than C#.

Generated C function support includes:

- `[x] ABS`
- `[x] ACOS`
- `[x] ACOT`
- `[x] ADATE`
- `[x] ASC`
- `[x] ASIN`
- `[x] ATAN`
- `[x] BEEPING`
- `[x] CHR$`
- `[x] CODE`
- `[x] COS`
- `[x] COT`
- `[x] DATE`
- `[x] DATE$`
- `[x] DAY$`
- `[x] DEG`
- `[x] DIMN`
- `[x] EOF`
- `[x] ERLIN`
- `[x] ERNUM`
- `[x] ERR_*`
- `[x] EXP`
- `[x] FILL$`
- `[x] GETENV$`
- `[x] INKEY`
- `[x] INKEY$`
- `[x] INT`
- `[x] KEYROW`
- `[x] LEFT$`
- `[x] LEN`
- `[x] LN`
- `[x] LOG`
- `[x] LOG10`
- `[x] MID$`
- `[x] PEEK`
- `[x] PEEK_W`
- `[x] PEEK_L`
- `[x] PI`
- `[x] RAD`
- `[x] REPL$`
- `[x] RIGHT$`
- `[x] RND`
- `[x] ROUND`
- `[x] SGN`
- `[x] SIN`
- `[x] SQRT`
- `[x] STR$`
- `[x] TAN`
- `[x] TIME`
- `[x] VAL`

Generated C statement support includes:

- `[x] PRINT`
- `[x] INPUT`
- `[x] REFERENCE`
- `[x] RANDOMISE`
- `[x] STOP`
- `[x] RETRY`
- `[x] CONTINUE`
- `[x] REPORT`
- `[x] BEEP`
- `[x] FLUSH`
- `[x] MODE`
- `[x] PAUSE`
- `[x] POKE`
- `[x] POKE_W`
- `[x] POKE_L`
- `[~] OPEN`
- `[~] OPEN_IN`
- `[~] OPEN_NEW`
- `[~] APPEND`
- `[~] CLOSE`
- `[~] DIR`
- `[~] DELETE`
- `[~] COPY`
- `[~] COPY_N`
- `[~] CLS`
- `[~] WINDOW`
- `[~] AT`
- `[~] CURSOR`
- `[~] CSIZE`
- `[~] CHAR_USE`
- `[~] S_FONT`
- `[~] INK`
- `[~] PAPER`
- `[~] STRIP`
- `[~] BORDER`
- `[~] LINE`
- `[~] CIRCLE`
- `[~] BLOCK`
- `[~] OVER`
- `[~] UNDER`
- `[~] FLASH`
- `[~] SCROLL`
- `[~] RECOL`
- `[~] SCALE`
- `[~] ARC`
- `[~] ELLIPSE`
- `[~] TRUNCATE`
- `[~] SET_POSITION`
- `[~] SET_CHANNEL`

Generated C explicitly rejects:

- `[ ] RUN`
- `[ ] LOAD`
- `[ ] LRUN`
- `[ ] MERGE`
- `[ ] MRUN`
- `[ ] SAVE`
- `[ ] NEW`
- `[ ] SEXEC`
- `[ ] SBYTES`
- `[ ] CALL`
- `[ ] MOVE`
- `[ ] RESPR`
- `[ ] LOADMEM`
- `[ ] SAVEMEM`
- `[ ] SYSVAR`

Generated C still reports many graphics/environment names as unsupported, including relative graphics variants and host-interactive functions that do not have a C runtime implementation.

## Recognized But Unsupported Or Out Of Scope

These names are recognized by the semantic/built-in registry but are not meaningfully implemented in the current runtime model:

- `[ ] ALLOCATE`
- `[ ] AUTO`
- `[ ] BACKUP`
- `[ ] BAUD`
- `[ ] CALL`
- `[ ] DEALLOCATE`
- `[ ] EDIT`
- `[ ] EXEC`
- `[ ] EXEC_W`
- `[ ] FORMAT`
- `[ ] LBYTES`
- `[ ] LIST`
- `[ ] LOADMEM`
- `[ ] LPRINT`
- `[ ] NET`
- `[ ] RENUM`
- `[ ] RESPR`
- `[ ] SAVE`
- `[ ] SAVEMEM`
- `[ ] SBYTES`
- `[ ] SEXEC`
- `[ ] SHELL`
- `[ ] SYSVAR`
- `[ ] TRON`
- `[ ] TROFF`

Language keywords handled by parser/lowering rather than callable built-ins:

- `DATA`
- `DEFine FuNction`
- `DEFine PROCedure`
- `DIM`
- `ELSE`
- `END DEFine`
- `END FOR`
- `END IF`
- `END REPeat`
- `END SELect`
- `EXIT`
- `FOR`
- `IF`
- `LET`
- `LOCAL`
- `MOD`
- `NEXT`
- `READ`
- `REMark`
- `REPEAT`
- `RESTORE`
- `RETurn`
- `SELect`
- `THEN`

## Important Gaps

- Host/channel behavior is pragmatic and file-system based, not full QL device semantics.
- Screen/window behavior is partial and host-specific; console, Avalonia, generated C#, and generated C do not yet share one complete virtual screen implementation.
- Graphics operations are modeled well enough for many tests, but clipping, transforms, backing buffers, and display projection are still incomplete.
- Generated C# has better parity than generated C, but neither generated backend should be treated as fully equivalent to the interpreter.
- Low-level memory, machine-code, shell, editor, serial, network, and QL firmware features are intentionally unsupported unless a host-neutral abstraction is added later.
