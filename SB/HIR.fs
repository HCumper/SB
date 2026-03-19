module HIR

open Types

// HIR is a semantically-resolved, interpreter-friendly representation.
//
// It sits between the source-shaped AST and execution/lowering so later stages
// can work with explicit symbols, types, targets, and control-flow constructs
// instead of re-querying semantic state.

type SymbolId = SymbolId of int

type LabelId = LabelId of int

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

type HirExpr =
    | Literal of HirConst * HirType * SourcePosition
    | ReadVar of SymbolId * HirType * SourcePosition
    | ReadArrayElem of SymbolId * HirExpr list * HirType * SourcePosition
    | Unary of HirUnaryOp * HirExpr * HirType * SourcePosition
    | Binary of HirBinaryOp * HirExpr * HirExpr * HirType * SourcePosition
    | CallFunc of SymbolId * HirExpr list * HirType * SourcePosition

type HirTarget =
    | WriteVar of SymbolId * HirType * SourcePosition
    | WriteArrayElem of SymbolId * HirExpr list * HirType * SourcePosition

type HirBlock = HirStmt list

and HirStmt =
    | Assign of HirTarget * HirExpr * SourcePosition
    | ProcCall of SymbolId * HirExpr option * HirExpr list * SourcePosition
    | BuiltInCall of BuiltInKind * HirExpr option * HirExpr list * SourcePosition
    | Input of HirExpr option * HirTarget list * SourcePosition
    | If of HirExpr * HirBlock * HirBlock option * SourcePosition
    | For of SymbolId * HirExpr * HirExpr * HirExpr * HirBlock * SourcePosition
    | Repeat of LoopName * HirBlock * SourcePosition
    | Goto of HirExpr * SourcePosition
    | OnGoto of HirExpr * HirExpr list * SourcePosition
    | Gosub of HirExpr * SourcePosition
    | OnGosub of HirExpr * HirExpr list * SourcePosition
    | Return of HirExpr option * SourcePosition
    | LineNumber of int * SourcePosition
    | Restore of HirExpr option * SourcePosition
    | Read of HirTarget list * SourcePosition
    | Remark of string * SourcePosition

type HirRoutine = {
    Name: string
    Symbol: SymbolId
    Parameters: SymbolId list
    Body: HirBlock
    ReturnType: HirType option
    Position: SourcePosition
}

type HirProgram = {
    Globals: SymbolId list
    Routines: HirRoutine list
    DataItems: HirExpr list
    Main: HirBlock
}
