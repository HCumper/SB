module HIR

open Types

// HIR is a semantically-resolved, interpreter-friendly representation.
//
// It sits between the source-shaped AST and execution/lowering so later stages
// can work with explicit symbols, types, targets, and control-flow constructs
// instead of re-querying semantic state.

type SymbolId = SymbolId of int

type LabelId = LabelId of int

type LoopId = LoopId of int

type StorageSlotId = StorageSlotId of int

type DataSlotId = DataSlotId of int

type HirType =
    | Int
    | Float
    | String
    | Void
    | Array of HirType

type HirConst =
    | ConstInt of int
    | ConstFloat of double
    | ConstString of string

type HirUnaryOp =
    | Identity
    | Negate
    | BitwiseNot
    /// A '#' on a call argument after the first, as in CLOSE #3, #4. The value is unchanged;
    /// Turbo passes the mark to the resident routine.
    | Hash
    | UnaryUnknown of string

type HirBinaryOp =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Power
    | Concat
    | IntegerDivide
    | Modulo
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | Equal
    /// SuperBASIC '==': strings equal ignoring case, numbers equal to about 1 part in 10^7.
    | AlmostEqual
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Instr
    | SliceRange
    | BinaryUnknown of string

type BuiltInKind =
    | Reference
    | Input
    | Print
    | GotoBuiltIn
    | GosubBuiltIn
    | OnGotoBuiltIn
    | OnGosubBuiltIn
    | NamedBuiltIn of string

type LoopName =
    | AnonymousLoop
    | NamedLoop of string

type StorageClass =
    | GlobalStorage
    | RoutineLocalStorage of string
    | RoutineParameterStorage of string

type HirStorage = {
    Symbol: SymbolId
    Slot: StorageSlotId
    Name: string
    Type: HirType
    Dimensions: int list option
    Class: StorageClass
    Position: SourcePosition
}

type HirParameterBinding =
    | FlexibleBinding
    | ReferenceBinding

type HirParameter = {
    Storage: HirStorage
    Binding: HirParameterBinding
}

type HirRestorePoint = {
    LineNumber: int
    Slot: DataSlotId
}

type HirExpr =
    | Literal of HirConst * HirType * SourcePosition
    | ReadVar of SymbolId * HirType * SourcePosition
    | ReadArrayElem of SymbolId * HirExpr list * HirType * SourcePosition
    | ReadStringChar of SymbolId * HirExpr * HirType * SourcePosition
    | DynamicReadVar of string * HirType * SourcePosition
    | DynamicReadArrayElem of string * HirExpr list * HirType * SourcePosition
    | DynamicReadStringChar of string * HirExpr * HirType * SourcePosition
    | Unary of HirUnaryOp * HirExpr * HirType * SourcePosition
    | Binary of HirBinaryOp * HirExpr * HirExpr * HirType * SourcePosition
    | CallFunc of SymbolId * HirCallArg list * HirType * SourcePosition
    /// An omitted call argument: the empty one before a leading TO (LINE TO x,y), or between
    /// two separators (PROC a,,b).
    | Missing of SourcePosition

and HirChannel =
    | ExplicitChannel of HirExpr
    | ImplicitChannel of HirExpr

and HirTarget =
    | WriteVar of SymbolId * HirType * SourcePosition
    | WriteArrayElem of SymbolId * HirExpr list * HirType * SourcePosition
    | WriteStringChar of SymbolId * HirExpr * HirType * SourcePosition
    | DynamicWriteVar of string * HirType * SourcePosition
    | DynamicWriteArrayElem of string * HirExpr list * HirType * SourcePosition
    | DynamicWriteStringChar of string * HirExpr * HirType * SourcePosition

and HirCallArg =
    | ValueArg of HirExpr
    | RefArg of HirTarget

type HirDataEntry = {
    Slot: DataSlotId
    Value: HirExpr
    Position: SourcePosition
    LineNumber: int option
}

type HirBlock = HirStmt list

and HirStmt =
    | Assign of HirTarget * HirExpr * SourcePosition
    | ProcCall of SymbolId * HirChannel option * HirCallArg list * SourcePosition
    | BuiltInCall of BuiltInKind * HirChannel option * HirExpr list * SourcePosition
    | Input of HirChannel option * HirExpr list * HirTarget list * SourcePosition
    | WhenError of HirBlock * SourcePosition
    | If of HirExpr * HirBlock * HirBlock option * SourcePosition
    | For of LoopId * SymbolId * HirExpr * HirExpr * HirExpr * HirBlock * SourcePosition
    | ForSequence of LoopId * SymbolId * HirExpr list * HirExpr * HirExpr * HirExpr list * HirExpr * HirBlock * SourcePosition
    | Repeat of LoopId * LoopName * HirBlock * SourcePosition
    | Exit of LoopId * SourcePosition
    | Next of LoopId * SourcePosition
    | Goto of HirExpr * SourcePosition
    | OnGoto of HirExpr * HirExpr list * SourcePosition
    | Gosub of HirExpr * SourcePosition
    | OnGosub of HirExpr * HirExpr list * SourcePosition
    | Return of HirExpr option * SourcePosition
    | LineNumber of int * SourcePosition
    | Restore of HirExpr option * SourcePosition
    /// A DIM, or a LOCal with bounds, where it stands: the declared upper bounds, the last of a
    /// string array's (or of DIM a$(n)) being the maximum length. Storage already carries the
    /// bounds that fold to constants, so only the Turbo IR backend acts on it.
    | Dim of SymbolId * HirExpr list * SourcePosition
    | Read of HirTarget list * SourcePosition
    | Remark of string * SourcePosition

type HirRoutine = {
    Name: string
    Symbol: SymbolId
    Parameters: HirParameter list
    Locals: HirStorage list
    Body: HirBlock
    ReturnType: HirType option
    EndLineNumber: int option
    Position: SourcePosition
}

type HirProgram = {
    SymbolNames: Map<SymbolId, string>
    Globals: HirStorage list
    Routines: HirRoutine list
    DataEntries: HirDataEntry list
    RestorePoints: HirRestorePoint list
    Main: HirBlock
}

/// Built-ins whose TO separators SB's own backends read as plain coordinates: LINE x1,y1 TO x2,y2
/// joins the points (x1,y1) and (x2,y2), and ARC x1,y1 TO x2,y2,a draws between them.
let flattensRangeArguments name =
    match name with
    | "LINE" | "LINE_R" | "ARC" | "ARC_R" -> true
    | _ -> false

/// Replaces each 'a TO b' argument by a and b, and a leading 'TO b' (an omitted argument
/// followed by TO) by b alone. The Turbo IR backend keeps the TO instead (separator 5).
let flattenRangeArguments (args: HirExpr list) =
    args
    |> List.collect (function
        | Binary(SliceRange, Missing _, rhs, _, _) -> [ rhs ]
        | Binary(SliceRange, lhs, rhs, _, _) -> [ lhs; rhs ]
        | arg -> [ arg ])
