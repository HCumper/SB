module BuiltIns

open Types

// BuiltIns centralizes the language/runtime view of built-in names.
//
// It provides:
// - the broad keyword surface
// - the subset of names that should resolve as callable built-ins
// - the small signature model used by semantic analysis
//
// The current signature modeling is intentionally conservative and only captures
// the argument facts the analyzer can check cheaply and reliably.

type BuiltInArgumentKind =
    | Any
    | Numeric
    | String
    | Writable

type BuiltInSignature = {
    FixedArity: int option
    ArgumentKinds: BuiltInArgumentKind list option
}

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
      "CALL"; "CHAR_USE"; "CHR$"; "CIRCLE"; "CIRCLE_R"; "CLEAR"; "CLOSE"; "CLS"; "CODE"; "CONTINUE"; "COPY"; "COPY_N"; "COS"; "COT"; "CSIZE"; "CURSOR"
      "DATA"; "DATE"; "DATE$"; "DAY$"; "DEFine FuNction"; "DEFine PROCedure"; "DEG"; "DELETE"; "DIM"; "DIMN"; "DIR"; "DIV"; "DLINE"
      "EDIT"; "ELLIPSE"; "ELLIPSE_R"; "ELSE"; "END DEFine"; "END FOR"; "END IF"; "END REPeat"; "END SELect"; "EOF"; "EXEC"; "EXEC_W"; "EXIT"; "EXP"
      "ERLIN"; "ERNUM"; "ERR_NC"; "ERR_NJ"; "ERR_OM"; "ERR_OR"; "ERR_BO"; "ERR_NO"; "ERR_NF"; "ERR_EX"; "ERR_IU"; "ERR_EF"; "ERR_DF"; "ERR_BN"; "ERR_TE"; "ERR_FF"; "ERR_BP"; "ERR_FE"; "ERR_XP"; "ERR_OV"; "ERR_NI"; "ERR_RO"; "ERR_BL"
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
      "CALL"; "CHAR_USE"; "CHR$"; "CIRCLE"; "CIRCLE_R"; "CLEAR"; "CLOSE"; "CLS"; "CODE"; "CONTINUE"; "COPY"; "COPY_N"; "COS"; "COT"; "CSIZE"; "CURSOR"
      "DATE"; "DATE$"; "DAY$"; "DEG"; "DELETE"; "DIMN"; "DIR"; "DLINE"
      "ELLIPSE"; "ELLIPSE_R"; "EOF"; "EXEC"; "EXEC_W"; "EXP"; "FILL"; "FILL$"; "FLASH"; "FORMAT"
      "ERLIN"; "ERNUM"; "ERR_NC"; "ERR_NJ"; "ERR_OM"; "ERR_OR"; "ERR_BO"; "ERR_NO"; "ERR_NF"; "ERR_EX"; "ERR_IU"; "ERR_EF"; "ERR_DF"; "ERR_BN"; "ERR_TE"; "ERR_FF"; "ERR_BP"; "ERR_FE"; "ERR_XP"; "ERR_OV"; "ERR_NI"; "ERR_RO"; "ERR_BL"; "REPORT"
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
    [ "ALLOCATE"; "APPEND"; "ASC"; "BACKUP"; "DEALLOCATE"; "DRAW"; "GETENV$"; "INKEY"; "LEFT$"; "LOADMEM"; "LOG"; "LPRINT"; "MID$"; "MODE"; "PALETTE"; "RAD"; "REPL$"; "RIGHT$"; "ROUND"; "S_FONT"; "SAVEMEM"; "SGN"; "SHELL"; "STR$"; "SYSVAR"; "TIME"; "TRON"; "TROFF"; "VAL"; "WAIT"
      "TURBO_BUFFERSZ"; "TURBO_DIAGS"; "TURBO_LOCSTR"; "TURBO_OBJDAT"; "TURBO_OBJFIL"; "TURBO_OPTIM"; "TURBO_REPFIL"; "TURBO_STRUCT"; "TURBO_TASKN"; "TURBO_WINDO" ]

let callableBuiltInNames =
    coreCallableBuiltInNames @ extensionBuiltInNames
    |> normalizeNames

let private builtIns =
    callableBuiltInNames
    |> Seq.map (fun name -> name, createBuiltInSymbol name)
    |> Map.ofSeq

// Keyword membership and callable-built-in membership are separate because many
// language keywords are not valid callable symbols.
let isCoreKeyword name =
    coreSuperBasicKeywords.Contains(normalizeIdentifier name)

let isCallableBuiltIn name =
    callableBuiltInNames.Contains(normalizeIdentifier name)

let private fixedArityBuiltIns =
    [ "ABS", 1
      "ACOS", 1
      "ACOT", 1
      "ADATE", 1
      "ASC", 1
      "ASIN", 1
      "ATAN", 1
      "CHR$", 1
      "CODE", 1
      "COS", 1
      "COT", 1
      "DEG", 1
      "DIMN", 2
      "EOF", 1
      "ERLIN", 0
      "ERNUM", 0
      "ERR_NC", 0
      "ERR_NJ", 0
      "ERR_OM", 0
      "ERR_OR", 0
      "ERR_BO", 0
      "ERR_NO", 0
      "ERR_NF", 0
      "ERR_EX", 0
      "ERR_IU", 0
      "ERR_EF", 0
      "ERR_DF", 0
      "ERR_BN", 0
      "ERR_TE", 0
      "ERR_FF", 0
      "ERR_BP", 0
      "ERR_FE", 0
      "ERR_XP", 0
      "ERR_OV", 0
      "ERR_NI", 0
      "ERR_RO", 0
      "ERR_BL", 0
      "EXP", 1
      "FILL$", 2
      "GETENV$", 1
      "BEEPING", 0
      "INT", 1
      "KEYROW", 1
      "LEN", 1
      "LN", 1
      "LOG", 1
      "LOG10", 1
      "LEFT$", 2
      "PEEK", 1
      "PEEK_W", 1
      "PEEK_L", 1
      "RIGHT$", 2
      "RND", 1
      "ROUND", 1
      "SGN", 1
      "SIN", 1
      "SQRT", 1
      "STR$", 1
      "TAN", 1
      "VAL", 1 ]
    |> List.map (fun (name, arity) -> normalizeIdentifier name, arity)
    |> Map.ofList

let tryGetFixedArity name =
    Map.tryFind (normalizeIdentifier name) fixedArityBuiltIns

let private signature name fixedArity argumentKinds =
    normalizeIdentifier name,
    { FixedArity = fixedArity
      ArgumentKinds = argumentKinds }

let private builtInSignatures =
    // Signature entries only encode stable facts such as fixed arity and basic
    // argument kinds. More syntax-rich built-ins are left intentionally partial.
    [ signature "ABS" (Some 1) (Some [ Numeric ])
      signature "ACOS" (Some 1) (Some [ Numeric ])
      signature "ACOT" (Some 1) (Some [ Numeric ])
      signature "ADATE" (Some 1) (Some [ Numeric ])
      signature "ASC" (Some 1) (Some [ String ])
      signature "ASIN" (Some 1) (Some [ Numeric ])
      signature "ATAN" (Some 1) (Some [ Numeric ])
      signature "CHR$" (Some 1) (Some [ Numeric ])
      signature "CHAR_USE" (Some 2) (Some [ Numeric; Numeric ])
      signature "CODE" (Some 1) (Some [ String ])
      signature "COS" (Some 1) (Some [ Numeric ])
      signature "COT" (Some 1) (Some [ Numeric ])
      signature "DATE$" None None
      signature "DAY$" None None
      signature "DEG" (Some 1) (Some [ Numeric ])
      signature "DIMN" (Some 2) None
      signature "EOF" (Some 1) (Some [ Numeric ])
      signature "ERLIN" (Some 0) (Some [])
      signature "ERNUM" (Some 0) (Some [])
      signature "ERR_NC" (Some 0) (Some [])
      signature "ERR_NJ" (Some 0) (Some [])
      signature "ERR_OM" (Some 0) (Some [])
      signature "ERR_OR" (Some 0) (Some [])
      signature "ERR_BO" (Some 0) (Some [])
      signature "ERR_NO" (Some 0) (Some [])
      signature "ERR_NF" (Some 0) (Some [])
      signature "ERR_EX" (Some 0) (Some [])
      signature "ERR_IU" (Some 0) (Some [])
      signature "ERR_EF" (Some 0) (Some [])
      signature "ERR_DF" (Some 0) (Some [])
      signature "ERR_BN" (Some 0) (Some [])
      signature "ERR_TE" (Some 0) (Some [])
      signature "ERR_FF" (Some 0) (Some [])
      signature "ERR_BP" (Some 0) (Some [])
      signature "ERR_FE" (Some 0) (Some [])
      signature "ERR_XP" (Some 0) (Some [])
      signature "ERR_OV" (Some 0) (Some [])
      signature "ERR_NI" (Some 0) (Some [])
      signature "ERR_RO" (Some 0) (Some [])
      signature "ERR_BL" (Some 0) (Some [])
      signature "EXP" (Some 1) (Some [ Numeric ])
      signature "FILL$" (Some 2) (Some [ String; Numeric ])
      signature "GETENV$" (Some 1) (Some [ String ])
      signature "BEEPING" (Some 0) (Some [])
      signature "INKEY" None None
      signature "INKEY$" None None
      signature "INPUT" None None
      signature "INT" (Some 1) (Some [ Numeric ])
      signature "KEYROW" (Some 1) (Some [ Numeric ])
      signature "LEN" (Some 1) (Some [ String ])
      signature "LEFT$" (Some 2) (Some [ String; Numeric ])
      signature "LN" (Some 1) (Some [ Numeric ])
      signature "LOG" (Some 1) (Some [ Numeric ])
      signature "LOG10" (Some 1) (Some [ Numeric ])
      signature "PEEK" (Some 1) (Some [ Numeric ])
      signature "PEEK_W" (Some 1) (Some [ Numeric ])
      signature "PEEK_L" (Some 1) (Some [ Numeric ])
      signature "POKE" (Some 2) (Some [ Numeric; Numeric ])
      signature "POKE_W" (Some 2) (Some [ Numeric; Numeric ])
      signature "POKE_L" (Some 2) (Some [ Numeric; Numeric ])
      signature "RND" (Some 1) (Some [ Numeric ])
      signature "RIGHT$" (Some 2) (Some [ String; Numeric ])
      signature "ROUND" (Some 1) (Some [ Numeric ])
      signature "S_FONT" (Some 2) (Some [ Numeric; Numeric ])
      signature "SGN" (Some 1) (Some [ Numeric ])
      signature "SIN" (Some 1) (Some [ Numeric ])
      signature "SQRT" (Some 1) (Some [ Numeric ])
      signature "STR$" (Some 1) (Some [ Numeric ])
      signature "TAN" (Some 1) (Some [ Numeric ])
      signature "TIME" None None
      signature "VAL" (Some 1) (Some [ String ]) ]
    |> Map.ofList

let tryGetSignature name =
    Map.tryFind (normalizeIdentifier name) builtInSignatures

// Lookup is normalized at the boundary to preserve case-insensitive language semantics.
let tryFind name =
    Map.tryFind (normalizeIdentifier name) builtIns
