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

let private normalizeNames names =
    names |> List.map normalizeIdentifier |> Set.ofList

// Published Sinclair QL SuperBASIC keyword index. This is the language keyword surface,
// not necessarily the subset that should resolve as callable built-ins.
let coreSuperBasicKeywords =
    [ "ABS"; "ACOS"; "ACOT"; "ADATE"; "ARC"; "ARC_R"; "ASIN"; "AT"; "ATAN"; "AUTO"
      "BAUD"; "BEEP"; "BEEPING"; "BLOCK"; "BORDER"
      "CALL"; "CHR$"; "CIRCLE"; "CIRCLE_R"; "CLEAR"; "CLOSE"; "CLS"; "CODE"; "CONTINUE"; "COPY"; "COPY_N"; "COS"; "COT"; "CSIZE"; "CURSOR"
      "DATA"; "DATE"; "DATE$"; "DAY$"; "DEFine FuNction"; "DEFine PROCedure"; "DEG"; "DELETE"; "DIM"; "DIMN"; "DIR"; "DIV"; "DLINE"
      "EDIT"; "ELLIPSE"; "ELLIPSE_R"; "ELSE"; "END DEFine"; "END FOR"; "END IF"; "END REPeat"; "END SELect"; "EOF"; "EXEC"; "EXEC_W"; "EXIT"; "EXP"
      "FILL"; "FILL$"; "FLASH"; "FOR"; "FORMAT"
      "GOSUB"; "GOTO"
      "IF"; "INK"; "INKEY$"; "INPUT"; "INSTR"; "INT"
      "KEYROW"
      "LBYTES"; "LEN"; "LET"; "LINE"; "LINE_R"; "LIST"; "LN"; "LOAD"; "LOCAL"; "LOG10"; "LRUN"
      "MERGE"; "MOD"; "MOVE"; "MRUN"
      "NET"; "NEW"; "NEXT"
      "ON...GOSUB"; "ON...GOTO"; "OPEN"; "OPEN_IN"; "OPEN_NEW"; "OVER"
      "PAN"; "PAPER"; "PAUSE"; "PEEK"; "PEEK_W"; "PEEK_L"; "PENDOWN"; "PENUP"; "PI"; "POINT"; "POINT_R"; "POKE"; "POKE_W"; "POKE_L"; "PRINT"
      "RAD"; "RANDOMISE"; "READ"; "RECOL"; "REMark"; "RENUM"; "REPEAT"; "RESPR"; "RESTORE"; "RETRY"; "RETurn"; "RND"; "RUN"
      "SAVE"; "SBYTES"; "SCALE"; "SCROLL"; "SDATE"; "SELect"; "SEXEC"; "SIN"; "SQRT"; "STOP"; "STRIP"
      "TAN"; "THEN"; "TURN"; "TURNTO"
      "UNDER"
      "WIDTH"; "WINDOW" ]
    |> normalizeNames

// Published core callable built-ins plus a small set of pseudo-built-ins used by
// the current lowering/runtime model for control-transfer statements.
let private coreCallableBuiltInNames =
    [ "ABS"; "ACOS"; "ACOT"; "ADATE"; "ARC"; "ARC_R"; "ASIN"; "AT"; "ATAN"
      "AUTO"; "BAUD"; "BEEP"; "BEEPING"; "BLOCK"; "BORDER"
      "CALL"; "CHR$"; "CIRCLE"; "CIRCLE_R"; "CLEAR"; "CLOSE"; "CLS"; "CODE"; "CONTINUE"; "COPY"; "COPY_N"; "COS"; "COT"; "CSIZE"; "CURSOR"
      "DATE"; "DATE$"; "DAY$"; "DEG"; "DELETE"; "DIMN"; "DIR"; "DLINE"
      "ELLIPSE"; "ELLIPSE_R"; "EOF"; "EXEC"; "EXEC_W"; "EXP"; "FILL"; "FILL$"; "FLASH"; "FORMAT"
      "INK"; "INKEY$"; "INPUT"; "INSTR"; "INT"
      "KEYROW"; "LBYTES"; "LEN"; "LINE"; "LINE_R"; "LIST"; "LN"; "LOAD"; "LOG10"; "LRUN"
      "MERGE"; "MOVE"; "MRUN"; "NEW"
      "OPEN"; "OPEN_IN"; "OPEN_NEW"; "OVER"
      "PAN"; "PAPER"; "PAUSE"; "PEEK"; "PEEK_W"; "PEEK_L"; "PENDOWN"; "PENUP"; "PI"; "POINT"; "POINT_R"; "POKE"; "POKE_W"; "POKE_L"; "PRINT"
      "RAD"; "RANDOMISE"; "RECOL"; "RENUM"; "RESPR"; "RETRY"; "RND"; "RUN"
      "SAVE"; "SBYTES"; "SCALE"; "SCROLL"; "SDATE"; "SEXEC"; "SIN"; "SQRT"; "STOP"; "STRIP"
      "TAN"; "TURN"; "TURNTO"; "UNDER"; "WIDTH"; "WINDOW"
      "GOSUB"; "GOTO"; "ON-GOSUB"; "ON-GOTO" ]

// Toolkit / environment / project-specific names that are intentionally treated as built-ins.
let private extensionBuiltInNames =
    [ "ALLOCATE"; "APPEND"; "ASC"; "BACKUP"; "DEALLOCATE"; "DRAW"; "GETENV$"; "INKEY"; "LEFT$"; "LOADMEM"; "LOG"; "LPRINT"; "MID$"; "MODE"; "PALETTE"; "RAD"; "REPL$"; "RIGHT$"; "ROUND"; "SAVEMEM"; "SGN"; "SHELL"; "STR$"; "SYSVAR"; "TIME"; "TRON"; "TROFF"; "VAL"; "WAIT"
      "TURBO_BUFFERSZ"; "TURBO_DIAGS"; "TURBO_LOCSTR"; "TURBO_OBJDAT"; "TURBO_OBJFIL"; "TURBO_OPTIM"; "TURBO_REPFIL"; "TURBO_STRUCT"; "TURBO_TASKN"; "TURBO_WINDO" ]

let callableBuiltInNames =
    coreCallableBuiltInNames @ extensionBuiltInNames
    |> normalizeNames

let private builtIns =
    callableBuiltInNames
    |> Seq.map (fun name -> name, createBuiltInSymbol name)
    |> Map.ofSeq

let isCoreKeyword name =
    coreSuperBasicKeywords.Contains(normalizeIdentifier name)

let isCallableBuiltIn name =
    callableBuiltInNames.Contains(normalizeIdentifier name)

// Lookup is normalized at the boundary to preserve case-insensitive language semantics.
let tryFind name =
    Map.tryFind (normalizeIdentifier name) builtIns
