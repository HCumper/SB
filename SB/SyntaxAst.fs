module SyntaxAst

type Position = int * int

type Ast =
    | Program of Position * Ast list
    | Line of Position * int option * Ast list

    | ProcedureDef of Position * string * string list * Ast list
    | FunctionDef of Position * string * string list * Ast list

    | DimStmt of Position * (string * Ast list) list
    | LocalStmt of Position * (string * Ast list option) list
    | ImplicitStmt of Position * string * string list
    | ReferenceStmt of Position * Ast list

    | Assignment of Position * Ast * Ast
    | ProcedureCall of Position * string * Ast list
    | ChannelProcedureCall of Position * string * Ast * Ast list

    | ForStmt of Position * string * Ast * Ast * Ast option * Ast list
    | RepeatStmt of Position * string * Ast list
    | IfStmt of Position * Ast * Ast list * Ast list option
    | SelectStmt of Position * Ast * Ast list
    | SelectClause of Position * Ast * Ast * Ast list option
    | WhenStmt of Position * Ast option * Ast list

    | ReturnStmt of Position * Ast option
    | DataStmt of Position * Ast list
    | ReadStmt of Position * Ast list
    | RestoreStmt of Position * Ast option
    | ExitStmt of Position * string
    | NextStmt of Position * string
    | Remark of Position * string

    | PostfixName of Position * string * Ast list option // handles A, A(1), A(1,2), A(1 TO 5) uniformly.
    | SliceRange of Position * Ast * Ast // represents x TO y inside postfix args.

    | BinaryExpr of Position * string * Ast * Ast // carry operator text directly.
    | UnaryExpr of Position * string * Ast

    | NumberLiteral of Position * string
    | StringLiteral of Position * string
    | Identifier of Position * string
    
