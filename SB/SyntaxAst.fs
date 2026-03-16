module SyntaxAst

type Position = int * int

type Ast =
    | Program of Position * Line list

and Line =
    | Line of Position * int option * Stmt list

and Block =
    | StatementBlock of Stmt list
    | LineBlock of Line list

and SelectClause =
    | SelectClause of Position * Expr * Expr * Block option

and Stmt =
    | ProcedureDef of Position * string * string list * Line list
    | FunctionDef of Position * string * string list * Line list
    | DimStmt of Position * (string * Expr list) list
    | LocalStmt of Position * (string * Expr list option) list
    | ImplicitStmt of Position * string * string list
    | ReferenceStmt of Position * Expr list
    | Assignment of Position * Expr * Expr
    | ProcedureCall of Position * string * Expr list
    | ChannelProcedureCall of Position * string * Expr * Expr list
    | ForStmt of Position * string * Expr * Expr * Expr option * Block
    | RepeatStmt of Position * string * Block
    | IfStmt of Position * Expr * Block * Block option
    | SelectStmt of Position * Expr * SelectClause list
    | WhenStmt of Position * Expr option * Line list
    | ReturnStmt of Position * Expr option
    | DataStmt of Position * Expr list
    | ReadStmt of Position * Expr list
    | RestoreStmt of Position * Expr option
    | ExitStmt of Position * string
    | NextStmt of Position * string
    | Remark of Position * string

and Expr =
    | PostfixName of Position * string * Expr list option
    | SliceRange of Position * Expr * Expr
    | BinaryExpr of Position * string * Expr * Expr
    | UnaryExpr of Position * string * Expr
    | NumberLiteral of Position * string
    | StringLiteral of Position * string
    | Identifier of Position * string
