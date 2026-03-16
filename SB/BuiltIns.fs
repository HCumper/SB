module BuiltIns

open Types

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
      "EXP"; "FORMAT"; "INK"; "INPUT"; "INSTR"; "INT"; "LEFT$"; "LEN"; "LIST"
      "LOAD"; "LOADMEM"; "LOG"; "LPRINT"; "LRUN"; "MID$"; "MOVE"; "NEW"
      "PALETTE"; "PAPER"; "PAUSE"; "PEEK"; "PI"; "POINT"; "POKE"; "PRINT"
      "REPL$"; "RIGHT$"; "RND"; "ROUND"; "RUN"; "SAVE"; "SAVEMEM"; "SHELL"
      "SGN"; "SIN"; "STOP"; "STR$"; "SYSVAR"; "TAN"; "TIME"; "TRON"; "TROFF"
      "VAL"; "WAIT"; "WINDOW" ]

let private builtIns =
    builtInNames
    |> List.map (fun name -> normalizeIdentifier name, createBuiltInSymbol name)
    |> Map.ofList

let tryFind name =
    Map.tryFind (normalizeIdentifier name) builtIns
