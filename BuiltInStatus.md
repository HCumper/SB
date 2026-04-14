# Built-In Status

Current status of recognized SuperBASIC and project-specific built-ins, based on:

- [SB/BuiltIns.fs](/C:/Source/SB/SB/BuiltIns.fs)
- [SBRuntime/BuiltInFunctions.fs](/C:/Source/SB/SBRuntime/BuiltInFunctions.fs)
- [SB/Interpreter.fs](/C:/Source/SB/SB/Interpreter.fs)

Status markers:

- `[x]` implemented
- `[~]` implemented but only partially faithful
- `[ ]` recognized but unsupported

## Implemented

- `[x] ABS` absolute value of a number.
- `[x] ACOS` inverse cosine.
- `[x] ACOT` inverse cotangent.
- `[x] ADATE` add a number of seconds to the current date/time value.
- `[x] APPEND` open an existing file or resource for append on a specified channel id.
- `[x] ARC` draw an arc with absolute coordinates.
- `[x] ARC_R` draw an arc with relative coordinates.
- `[x] ASC` return the character code of the first character in a string.
- `[x] ASIN` inverse sine.
- `[x] AT` move the text cursor to a row and column.
- `[x] ATAN` inverse tangent.
- `[x] BEEP` play a beep or tone through the host sound device.
- `[x] BEEPING` report whether the host sound device is currently beeping.
- `[x] BLOCK` draw a filled rectangular block.
- `[x] BORDER` set border color or border-related state.
- `[x] CHR$` convert a numeric character code to a one-character string.
- `[x] CIRCLE` draw a circle with absolute coordinates.
- `[x] CIRCLE_R` draw a circle with relative coordinates.
- `[x] CLEAR` clear graphics state or graphics surface.
- `[x] CLOSE` close an open channel.
- `[x] CLS` clear a screen or screen window.
- `[x] CODE` return the character code of the first character in a string.
- `[x] COPY` copy a file or directory-backed device-backed file.
- `[x] COPY_N` copy a file using the current simplified host copy semantics.
- `[x] COS` cosine.
- `[x] COT` cotangent.
- `[x] CSIZE` set text character size.
- `[x] CURSOR` set or move the text cursor.
- `[x] DATE` return the current date/time as a numeric value.
- `[x] DATE$` format a numeric date/time value as a string.
- `[x] DAY$` return the weekday name for a date/time value.
- `[x] DELETE` delete a file or directory-backed device-backed file.
- `[x] DEG` convert radians to degrees.
- `[x] DIMN` query array dimensionality or bounds.
- `[x] DIR` list a directory or device-backed directory contents.
- `[x] DLINE` draw a polyline from explicit coordinate pairs.
- `[x] DRAW` draw from the current graphics position to a new point.
- `[x] ELLIPSE` draw an ellipse with absolute coordinates.
- `[x] ELLIPSE_R` draw an ellipse with relative coordinates.
- `[x] EOF` report whether a channel is at end-of-file.
- `[x] EXP` exponential function, `e^x`.
- `[x] FILL` set fill mode for graphics operations.
- `[x] FILL$` repeat a string a specified number of times.
- `[x] FLASH` set flashing text or graphics mode.
- `[x] GETENV$` get an environment variable by name.
- `[x] GOSUB` jump to a numbered line and push a return point.
- `[x] GOTO` jump to a numbered line.
- `[x] INK` set foreground color or ink state.
- `[x] INKEY` read a key code as an integer.
- `[x] INKEY$` read a key as a string.
- `[x] INPUT` read values from the default input or a channel.
- `[x] INSTR` find one string inside another and return a 1-based position.
- `[x] INT` truncate toward negative infinity.
- `[x] KEYROW` query keyboard-row state.
- `[x] LEFT$` take the leftmost part of a string.
- `[x] LEN` return the length of a string.
- `[x] LINE` draw one or more straight line segments with absolute coordinates.
- `[x] LINE_R` draw one or more relative line segments.
- `[x] LN` natural logarithm.
- `[x] LOAD` load a program through the configured runtime loader.
- `[x] LOG` common logarithm, base 10.
- `[x] LOG10` common logarithm, base 10.
- `[x] LRUN` load and immediately run a program through the configured runtime loader.
- `[x] MERGE` merge another program into the current source/HIR state when source is available.
- `[x] MID$` take a substring starting at a 1-based position.
- `[x] MODE` change the current screen mode.
- `[x] MOVE` move or rename a file or directory-backed device-backed file.
- `[x] MRUN` merge another program and immediately restart execution when source is available.
- `[x] NEW` clear the current in-memory program.
- `[x] ON-GOSUB` select a target from a list and perform a `GOSUB`.
- `[x] ON-GOTO` select a target from a list and perform a `GOTO`.
- `[x] OPEN` open a named device or channel on a specified channel id.
- `[x] OPEN_IN` open a file for input on a specified channel id.
- `[x] OPEN_NEW` open a new output file on a specified channel id.
- `[x] OVER` set overprint drawing mode.
- `[x] PALETTE` set a color palette or palette entries.
- `[x] PAN` set screen panning or horizontal offset state.
- `[x] PAPER` set background color or paper state.
- `[x] PAUSE` delay execution for a period.
- `[x] PEEK` read a byte from the host memory model.
- `[x] PEEK_L` read a longword from the host memory model.
- `[x] PEEK_W` read a word from the host memory model.
- `[x] PENDOWN` enable drawing while moving.
- `[x] PENUP` disable drawing while moving.
- `[x] PI` return the mathematical constant pi.
- `[x] PLOT` move or plot the graphics cursor at a point.
- `[x] POINT` plot or mark a point with absolute coordinates.
- `[x] POINT_R` plot or mark a point with relative coordinates.
- `[x] POKE` write a byte into the host memory model.
- `[x] POKE_L` write a longword into the host memory model.
- `[x] POKE_W` write a word into the host memory model.
- `[x] PRINT` write values to the default output or a channel.
- `[x] RAD` convert degrees to radians.
- `[x] RANDOMISE` reseed the random number generator.
- `[x] READ` read values from `DATA`.
- `[x] RECOL` change recolor or palette remapping state.
- `[x] REFERENCE` carry explicit by-reference intent metadata.
- `[x] REPL$` replace part of a string starting at a 1-based position.
- `[x] RESTORE` reset the `DATA` read pointer.
- `[x] RIGHT$` take the rightmost part of a string.
- `[x] RND` produce a random number or random integer in a range.
- `[x] ROUND` round to the nearest integer-like value.
- `[x] RUN` restart the current loaded program, optionally from a line number.
- `[x] SCALE` set graphics scaling state.
- `[x] SCROLL` set scroll state for a screen or window.
- `[x] SDATE` adjust the interpreter's runtime date/time offset.
- `[x] SGN` return the sign of a number.
- `[x] SIN` sine.
- `[x] SQRT` square root.
- `[x] STOP` stop program execution.
- `[x] STR$` convert a numeric value to text.
- `[x] STRIP` set strip/background state for screen output.
- `[x] TAN` tangent.
- `[x] TIME` return the current time-of-day value with an offset.
- `[x] TURN` turn the graphics heading by a relative angle.
- `[x] TURNTO` set the graphics heading to an absolute angle.
- `[x] UNDER` set underprint drawing mode.
- `[x] VAL` parse numeric text into a number.
- `[x] WAIT` delay execution using the current pause timing behavior.
- `[x] WIDTH` set text width or columns for a screen/window.
- `[x] WINDOW` set a screen window’s size and position.

## Partial

- `[x] ADATE` implemented with simple Unix-epoch-based arithmetic, not QL-specific date semantics.
- `[x] DATE` implemented as Unix seconds, not Sinclair QL native date representation.
- `[x] DATE$` implemented with a pragmatic string format, not verified against QL formatting rules.
- `[x] DAY$` implemented with host weekday names.
- `[x] DELETE` resolves normal paths and directory-backed device paths, not full QL environment object deletion.
- `[x] COPY` and `[x] COPY_N` use host file copy semantics, not full QL channel/resource copy behavior.
- `[x] MOVE` uses host file move semantics, not full QL channel/resource move behavior.
- `[x] DIR` uses the host filesystem and directory-backed device roots, not a full QL directory environment.
- `[x] APPEND` supports basic file append, not full QL file semantics.
- `[x] BEEP` and `[x] BEEPING` use host sound state, not full QL sound semantics.
- `[x] DIMN` reports declared array dimensions from the current runtime model.
- `[x] EOF` implemented against current host/channel behavior, not full QL file semantics.
- `[x] GETENV$` uses host environment variables directly.
- `[x] INKEY` uses host key events, not full QL keyboard semantics.
- `[x] INKEY$` uses host key events, not full QL keyboard semantics.
- `[x] KEYROW` currently uses a host callback and defaults to `0` in the interpreter.
- `[x] MODE` uses host-defined modes and extensions.
- `[x] OPEN` supports only a limited set of named devices in `DefaultHost`.
- `[x] OPEN_IN` supports basic file reading, not full QL file semantics.
- `[x] OPEN_NEW` supports basic file writing, not full QL file semantics.
- `[x] LOAD`, `[x] LRUN`, `[x] MERGE`, `[x] MRUN`, `[x] NEW`, and `[x] RUN` operate on the interpreter's configured loader and in-memory program state, not a full interactive SuperBASIC environment.
- `[x] PEEK`/`PEEK_W`/`PEEK_L` and `POKE`/`POKE_W`/`POKE_L` operate on the host memory model and the emulated mode 8 screen buffer, not real QL memory.
- `[x] SDATE` changes the interpreter's effective clock offset rather than the host system clock.
- `[x] CLOSE` closes channels, but channel/device lifecycle is still simplified.
- `[x] screen/window/graphics operations` are broadly implemented, but the host model is still simplified compared with a full QL environment.
- `[x] STRIP` currently models screen strip/background state rather than a standalone string-processing built-in.
- `[x] TIME` uses host wall-clock time-of-day seconds, not verified QL semantics.
- `[x] WAIT` currently shares `PAUSE` timing behavior.

## Unsupported

- `[ ] ALLOCATE` allocate memory or resources through toolkit/runtime support.
- `[ ] AUTO` enable automatic line numbering in an interactive environment.
- `[ ] BACKUP` perform a file or device backup operation.
- `[ ] BAUD` configure serial baud rate.
- `[ ] CALL` call machine code at an address.
- `[ ] CONTINUE` resume after a stop or break.
- `[ ] DEALLOCATE` free allocated memory or resources.
- `[ ] EDIT` open an editor or edit a program line.
- `[ ] EXEC` execute machine code or an external program.
- `[ ] EXEC_W` execute machine code with workspace or variant semantics.
- `[ ] FORMAT` format a device or storage medium.
- `[ ] IF` keyword handled as syntax, not a callable built-in.
- `[ ] LBYTES` load bytes from a file or channel into memory.
- `[ ] LIST` list program lines.
- `[ ] LOADMEM` load a memory image.
- `[ ] LPRINT` print to a printer channel.
- `[ ] NET` perform networking-related setup or operations.
- `[ ] RENUM` renumber program lines.
- `[ ] RESPR` reserve memory for resident procedure or machine code support.
- `[ ] RETRY` retry a failed operation or interactive command.
- `[ ] SAVE` save a program or file.
- `[ ] SAVEMEM` save a memory image.
- `[ ] SBYTES` save bytes from memory to a file or channel.
- `[ ] SEXEC` save and execute or execute saved code depending on environment semantics.
- `[ ] SHELL` invoke a shell or command processor.
- `[ ] SYSVAR` access system variables.
- `[ ] TRON` enable execution tracing.
- `[ ] TROFF` disable execution tracing.

Turbo/project-specific placeholders:

- `[ ] TURBO_BUFFERSZ` Turbo compiler/runtime configuration flag.
- `[ ] TURBO_DIAGS` Turbo diagnostics configuration flag.
- `[ ] TURBO_LOCSTR` Turbo local-string configuration flag.
- `[ ] TURBO_OBJDAT` Turbo object-data configuration flag.
- `[ ] TURBO_OBJFIL` Turbo object-file configuration flag.
- `[ ] TURBO_OPTIM` Turbo optimization configuration flag.
- `[ ] TURBO_REPFIL` Turbo report-file configuration flag.
- `[ ] TURBO_STRUCT` Turbo structured mode configuration flag.
- `[ ] TURBO_TASKN` Turbo task-name configuration flag.
- `[ ] TURBO_WINDO` Turbo window configuration flag.

## Out Of Scope Or Low-Level

- `[ ] CALL` direct machine-code call.
- `[ ] EXEC` machine-code or external execution.
- `[ ] EXEC_W` machine-code or external execution variant.
- `[ ] SEXEC` saved/external execution support.
- `[ ] RESPR` low-level memory reservation.
- `[ ] LBYTES` low-level memory/file byte transfer.
- `[ ] SBYTES` low-level memory/file byte transfer.
- `[ ] LOADMEM` raw memory-image load.
- `[ ] SAVEMEM` raw memory-image save.
- `[ ] BAUD` serial communications configuration.
- `[ ] NET` networking or system communications support.
- `[ ] AUTO` interactive shell/editor command.
- `[ ] EDIT` interactive editor command.
- `[ ] LIST` interactive program listing command.
- `[ ] SAVE` interactive environment save command.
- `[ ] SHELL` shell invocation command.
- `[ ] TRON` tracing command.
- `[ ] TROFF` tracing command.


The highest-value next step is scr_ / con_ parameter parsing.
You now have:
•
more device support
•
independent screen windows
•
MODE
•
many graphics ops
•
richer open paths
But device strings still do almost nothing beyond prefix matching. A lot of QL-style environment behavior depends on opening screen devices with geometry and mode parameters.
So I would do:
1.
parse scr_ / con_ device strings
2.
create window state from those parameters
3.
bind opened channels to that configured window
4.
add tests for realistic device-open cases
After that:
•
add clipping and coordinate transforms so WINDOW, PAN, and SCALE affect drawing consistently
•
then consider a real backing buffer for text/graphics
•
then later an Avalonia host
So the next step is: make opened screen devices carry real window configuration, not just “this is a screen-like channel.”



INPUT is still effectively blocked or broken in the windowed host.
So the best next step is:
1.
add an input line box in the Avalonia UI
2.
queue submitted lines into the runtime host
3.
make ReadLine consume that queue
4.
optionally show prompt state in the UI
After that:
•
runtime-driven repaint instead of timer polling
•
better focus handling for keyboard input
•
maybe channel-specific input/output panes
•
then host tests for Avalonia session behavior
So the next milestone is: line-input support for interpreted programs, not more display polish.

It is the wrong way around. I don't want an avalonia shell for running programs using sb, I want to run sb programs from dos command line e.g. sb golfer. The program run may just write to the default channels 0-2, it may open new console channels it may move any channels around and resize them. Both graphics and text may be performed in any console or screen channel. How to make it work like that.
