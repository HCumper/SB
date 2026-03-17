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
      "BACKUP"; "BEEP"; "BORDER"; "CIRCLE"; "CLS"; "CLOSE"; "COPY"; "COS"
      "CURSOR OFF"; "CURSOR ON"; "DATE"; "DEALLOCATE"; "DELETE"; "DIR"; "DRAW"
      "EXP"; "FORMAT"; "INK"; "INKEY"; "INKEY$"; "INPUT"; "INSTR"; "INT"; "LEFT$"; "LEN"; "LIST"
      "LOAD"; "LOADMEM"; "LOG"; "LPRINT"; "LRUN"; "MID$"; "MOVE"; "NEW"
      "GOTO"; "GOSUB"; "ON-GOTO"; "ON-GOSUB"
      "PALETTE"; "PAPER"; "PAUSE"; "PEEK"; "PI"; "POINT"; "POKE"; "PRINT"
      "REPL$"; "RIGHT$"; "RND"; "ROUND"; "RUN"; "SAVE"; "SAVEMEM"; "SHELL"
      "SGN"; "SIN"; "STOP"; "STR$"; "SYSVAR"; "TAN"; "TIME"; "TRON"; "TROFF"
      "VAL"; "WAIT"; "WINDOW" ]

let private builtIns =
    builtInNames
    |> List.map (fun name -> normalizeIdentifier name, createBuiltInSymbol name)
    |> Map.ofList

// Lookup is normalized at the boundary to preserve case-insensitive language semantics.
let tryFind name =
    Map.tryFind (normalizeIdentifier name) builtIns
