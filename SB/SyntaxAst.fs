module SyntaxAst

open Types

// SyntaxAst defines the normalized syntax tree consumed after parsing.
//
// The parse-tree visitor lowers ANTLR contexts into this smaller DU-based model
// so downstream stages can work with language structure instead of grammar
// mechanics. The AST preserves source positions and important syntactic
// distinctions while avoiding parser-specific wrappers.
//
let mutable private nextNodeIdValue = 1

let freshNodeId () =
    let nodeId = NodeId nextNodeIdValue
    nextNodeIdValue <- nextNodeIdValue + 1
    nodeId

let resetNodeIds () =
    nextNodeIdValue <- 1

// The normalized AST shape produced by the parser visitor and consumed by later stages.
type Ast =
    | Program of SourcePosition * Line list

and Line =
    | Line of SourcePosition * int option * Stmt list

and Block =
    | StatementBlock of Stmt list
    | LineBlock of Line list

and SelectClause =
    | SelectClause of SourcePosition * Expr * Expr * Block option

and Stmt =
    | ProcedureDef of SourcePosition * string * string list * Line list
    | FunctionDef of SourcePosition * string * string list * Line list
    | DimStmt of SourcePosition * (string * Expr list) list
    | LocalStmt of SourcePosition * (string * Expr list option) list
    | ImplicitStmt of SourcePosition * string * string list
    | ReferenceStmt of SourcePosition * Expr list
    | Assignment of SourcePosition * Expr * Expr
    | GotoStmt of SourcePosition * Expr
    | GosubStmt of SourcePosition * Expr
    | OnGotoStmt of SourcePosition * Expr * Expr list
    | OnGosubStmt of SourcePosition * Expr * Expr list
    | ProcedureCall of SourcePosition * string * Expr list
    | ChannelProcedureCall of SourcePosition * string * Expr * Expr list
    | ForStmt of SourcePosition * string * Expr * Expr * Expr option * Block
    | RepeatStmt of SourcePosition * string * Block
    | IfStmt of SourcePosition * Expr * Block * Block option
    | SelectStmt of SourcePosition * Expr * SelectClause list
    | WhenStmt of SourcePosition * Expr option * Line list
    | ReturnStmt of SourcePosition * Expr option
    | DataStmt of SourcePosition * Expr list
    | ReadStmt of SourcePosition * Expr list
    | RestoreStmt of SourcePosition * Expr option
    | ExitStmt of SourcePosition * string
    | NextStmt of SourcePosition * string
    | Remark of SourcePosition * string

and Expr =
    | PostfixName of NodeId * SourcePosition * string * Expr list option
    | SliceRange of NodeId * SourcePosition * Expr * Expr
    | BinaryExpr of NodeId * SourcePosition * string * Expr * Expr
    | UnaryExpr of NodeId * SourcePosition * string * Expr
    | NumberLiteral of NodeId * SourcePosition * string
    | StringLiteral of NodeId * SourcePosition * string
    | Identifier of NodeId * SourcePosition * string

let nodeIdOfExpr expr =
    match expr with
    | PostfixName(nodeId, _, _, _)
    | SliceRange(nodeId, _, _, _)
    | BinaryExpr(nodeId, _, _, _, _)
    | UnaryExpr(nodeId, _, _, _)
    | NumberLiteral(nodeId, _, _)
    | StringLiteral(nodeId, _, _)
    | Identifier(nodeId, _, _) -> nodeId

let mkPostfixName pos name args = PostfixName(freshNodeId(), pos, name, args)
let mkSliceRange pos lhs rhs = SliceRange(freshNodeId(), pos, lhs, rhs)
let mkBinaryExpr pos op lhs rhs = BinaryExpr(freshNodeId(), pos, op, lhs, rhs)
let mkUnaryExpr pos op expr = UnaryExpr(freshNodeId(), pos, op, expr)
let mkNumberLiteral pos value = NumberLiteral(freshNodeId(), pos, value)
let mkStringLiteral pos value = StringLiteral(freshNodeId(), pos, value)
let mkIdentifier pos name = Identifier(freshNodeId(), pos, name)
