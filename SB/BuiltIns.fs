module BuiltIns

open Types

// Built-ins are resolved outside the user symbol table so source declarations stay distinct.
let private zeroPosition =
    { BasicLineNo = None
      EditorLineNo = 0
      Column = 0 }

let private createBuiltInSymbol name =
    BuiltInSym {
        Common = {
            Name = name
            EvaluatedType = SBType.Unknown
            Position = zeroPosition
        }
    }

let private builtInNames =
    [ "ABS"; "ACOS"; "ALLOCATE"; "APPEND"; "ASC"; "ASIN"; "AT"; "ATAN"
      "BACKUP"; "BEEP"; "BLOCK"; "BORDER"; "CIRCLE"; "CLS"; "CLOSE"; "CODE"; "COPY"; "COS"; "CSIZE"; "CURSOR"
      "CURSOR OFF"; "CURSOR ON"; "DATE"; "DEALLOCATE"; "DELETE"; "DIR"; "DRAW"
      "ELLIPSE"; "EXP"; "FILL"; "FORMAT"; "GETENV$"; "INK"; "INKEY"; "INKEY$"; "INPUT"; "INSTR"; "INT"; "LEFT$"; "LEN"; "LINE"; "LIST"
      "LOAD"; "LOADMEM"; "LOG"; "LPRINT"; "LRUN"; "MID$"; "MODE"; "MOVE"; "NEW"
      "GOTO"; "GOSUB"; "ON-GOTO"; "ON-GOSUB"; "OPEN"; "OPEN_IN"; "OPEN_NEW";
      "OVER"; "PALETTE"; "PAPER"; "PAUSE"; "PEEK"; "PEEK_W"; "PI"; "POINT"; "POKE"; "POKE_W"; "PRINT"; "RAD";
      "RANDOMISE"; "REPL$"; "RIGHT$"; "RND"; "ROUND"; "RUN"; "SAVE"; "SAVEMEM"; "SCALE"; "SHELL"; "SQRT"
      "SGN"; "SIN"; "STOP"; "STR$"; "SYSVAR"; "TAN"; "TIME"; "TRON"; "TROFF"
      "TURBO_BUFFERSZ"; "TURBO_DIAGS"; "TURBO_LOCSTR"; "TURBO_OBJDAT"; "TURBO_OBJFIL"; "TURBO_OPTIM"; "TURBO_REPFIL"; "TURBO_STRUCT"; "TURBO_TASKN"; "TURBO_WINDO"
      "VAL"; "WAIT"; "WINDOW" ]

let private builtIns =
    builtInNames
    |> List.map (fun name -> normalizeIdentifier name, createBuiltInSymbol name)
    |> Map.ofList

// Lookup is normalized at the boundary to preserve case-insensitive language semantics.
let tryFind name =
    Map.tryFind (normalizeIdentifier name) builtIns
