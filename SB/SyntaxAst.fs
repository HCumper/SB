module SyntaxAst

open Types

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
    | PostfixName of SourcePosition * string * Expr list option
    | SliceRange of SourcePosition * Expr * Expr
    | BinaryExpr of SourcePosition * string * Expr * Expr
    | UnaryExpr of SourcePosition * string * Expr
    | NumberLiteral of SourcePosition * string
    | StringLiteral of SourcePosition * string
    | Identifier of SourcePosition * string
