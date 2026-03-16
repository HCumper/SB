module Utility

open System
open System.Collections.Generic
open Antlr4.Runtime
open Antlr4.Runtime.Tree

open Types
open SyntaxAst

let globalScope = "~Global"

let getSymbolName (symbol: Symbol) : string =
    match symbol with
    | VariableSym s -> s.Common.Name
    | ConstantSym s -> s.Common.Name
    | ParameterSym s -> s.Common.Name
    | ArraySym s -> s.Common.Name
    | FunctionSym s -> s.Common.Name
    | ProcedureSym s -> s.Common.Name
    | BuiltInSym s -> s.Common.Name

let (|StringType|IntegerType|RealType|VoidType|) code =
    match code with
    | SBParser.String -> StringType
    | SBParser.Integer -> IntegerType
    | SBParser.Real -> RealType
    | _ -> VoidType

let identifyType code =
    match code with
    | StringType -> "string"
    | IntegerType -> "int"
    | RealType -> "float"
    | VoidType -> "void"

let getTypeFromAnnotation (name: string) : string * _ =
    let len = name.Length - 1
    match name[len] with
    | '%' -> name.Substring(0, len), SBParser.Integer
    | '$' -> name.Substring(0, len), SBParser.String
    | _ -> name, SBParser.Real

let gatherChildren (context: IParseTree) : IParseTree list =
    [ for index in 0 .. context.ChildCount - 1 do yield context.GetChild(index) ]

let mapAntlrList (mappingFunction: IParseTree -> 'b) (inputList: IList<IParseTree>) : 'b list =
    [ for item in inputList do yield mappingFunction item ]

let copyAntlrList (parentNode: IList<IParseTree>) : IParseTree list =
    mapAntlrList id parentNode

let sourcePositionFromTuple (line, column) : SourcePosition =
    { BasicLineNo = None; EditorLineNo = line; Column = column }

let sourcePositionToTuple (position: SourcePosition) : int * int =
    position.EditorLineNo, position.Column

let rec private exprChildren (node: Expr) =
    match node with
    | PostfixName(_, _, args) -> Option.defaultValue [] args
    | SliceRange(_, lhs, rhs) -> [ lhs; rhs ]
    | BinaryExpr(_, _, lhs, rhs) -> [ lhs; rhs ]
    | UnaryExpr(_, _, expr) -> [ expr ]
    | NumberLiteral _
    | StringLiteral _
    | Identifier _ -> []

and private exprLabel (node: Expr) =
    match node with
    | PostfixName(_, name, _) -> $"PostfixName ({name})"
    | SliceRange _ -> "SliceRange"
    | BinaryExpr(_, op, _, _) -> $"BinaryExpr ({op})"
    | UnaryExpr(_, op, _) -> $"UnaryExpr ({op})"
    | NumberLiteral(_, value) -> $"NumberLiteral ({value})"
    | StringLiteral(_, value) -> $"StringLiteral ({value})"
    | Identifier(_, value) -> $"Identifier ({value})"

let rec private prettyPrintExpr (node: Expr) (indent: int) : string =
    let padding = String.replicate indent " "
    let currentLine = sprintf "%s- %s\n" padding (exprLabel node)
    let childLines = exprChildren node |> List.map (fun child -> prettyPrintExpr child (indent + 2)) |> String.concat ""
    currentLine + childLines

let private stmtLabel (node: Stmt) =
    match node with
    | ProcedureDef(_, name, parms, _) ->
        let parmText = String.concat "," parms
        $"ProcedureDef ({name}, parms={parmText})"
    | FunctionDef(_, name, parms, _) ->
        let parmText = String.concat "," parms
        $"FunctionDef ({name}, parms={parmText})"
    | DimStmt(_, items) -> $"DimStmt ({items.Length})"
    | LocalStmt(_, items) -> $"LocalStmt ({items.Length})"
    | ImplicitStmt(_, suffix, names) ->
        let nameText = String.concat "," names
        $"ImplicitStmt ({suffix}: {nameText})"
    | ReferenceStmt _ -> "ReferenceStmt"
    | Assignment _ -> "Assignment"
    | ProcedureCall(_, name, _) -> $"ProcedureCall ({name})"
    | ChannelProcedureCall(_, name, _, _) -> $"ChannelProcedureCall ({name})"
    | ForStmt(_, name, _, _, _, _) -> $"ForStmt ({name})"
    | RepeatStmt(_, name, _) -> $"RepeatStmt ({name})"
    | IfStmt _ -> "IfStmt"
    | SelectStmt _ -> "SelectStmt"
    | WhenStmt _ -> "WhenStmt"
    | ReturnStmt _ -> "ReturnStmt"
    | DataStmt _ -> "DataStmt"
    | ReadStmt _ -> "ReadStmt"
    | RestoreStmt _ -> "RestoreStmt"
    | ExitStmt(_, name) -> $"ExitStmt ({name})"
    | NextStmt(_, name) -> $"NextStmt ({name})"
    | Remark(_, text) -> $"Remark ({text})"

let rec private prettyPrintBlock (block: Block) (indent: int) : string =
    match block with
    | StatementBlock stmts -> stmts |> List.map (fun stmt -> prettyPrintStmt stmt indent) |> String.concat ""
    | LineBlock lines -> lines |> List.map (fun line -> prettyPrintLine line indent) |> String.concat ""

and private prettyPrintSelectClause (node: SelectClause) (indent: int) : string =
    let padding = String.replicate indent " "
    let (SelectClause(_, selector, rangeExpr, body)) = node
    let currentLine = sprintf "%s- SelectClause\n" padding
    let childLines =
        [ yield prettyPrintExpr selector (indent + 2)
          yield prettyPrintExpr rangeExpr (indent + 2)
          match body with
          | Some block -> yield prettyPrintBlock block (indent + 2)
          | None -> () ]
        |> String.concat ""
    currentLine + childLines

and private prettyPrintStmt (node: Stmt) (indent: int) : string =
    let padding = String.replicate indent " "
    let currentLine = sprintf "%s- %s\n" padding (stmtLabel node)
    let childLines =
        match node with
        | ProcedureDef(_, _, _, body)
        | FunctionDef(_, _, _, body) -> body |> List.map (fun line -> prettyPrintLine line (indent + 2))
        | DimStmt(_, items) -> items |> List.collect snd |> List.map (fun expr -> prettyPrintExpr expr (indent + 2))
        | LocalStmt(_, items) -> items |> List.collect (snd >> Option.defaultValue []) |> List.map (fun expr -> prettyPrintExpr expr (indent + 2))
        | ImplicitStmt _ -> []
        | ReferenceStmt(_, children)
        | DataStmt(_, children)
        | ReadStmt(_, children) -> children |> List.map (fun expr -> prettyPrintExpr expr (indent + 2))
        | Assignment(_, lhs, rhs) -> [ prettyPrintExpr lhs (indent + 2); prettyPrintExpr rhs (indent + 2) ]
        | ProcedureCall(_, _, args) -> args |> List.map (fun expr -> prettyPrintExpr expr (indent + 2))
        | ChannelProcedureCall(_, _, channel, args) ->
            (channel :: args) |> List.map (fun expr -> prettyPrintExpr expr (indent + 2))
        | ForStmt(_, _, startExpr, endExpr, stepExpr, body) ->
            [ yield prettyPrintExpr startExpr (indent + 2)
              yield prettyPrintExpr endExpr (indent + 2)
              match stepExpr with
              | Some expr -> yield prettyPrintExpr expr (indent + 2)
              | None -> ()
              yield prettyPrintBlock body (indent + 2) ]
        | RepeatStmt(_, _, body) -> [ prettyPrintBlock body (indent + 2) ]
        | IfStmt(_, cond, thenBody, elseBody) ->
            [ yield prettyPrintExpr cond (indent + 2)
              yield prettyPrintBlock thenBody (indent + 2)
              match elseBody with
              | Some block -> yield prettyPrintBlock block (indent + 2)
              | None -> () ]
        | SelectStmt(_, selector, clauses) ->
            prettyPrintExpr selector (indent + 2)
            :: (clauses |> List.map (fun clause -> prettyPrintSelectClause clause (indent + 2)))
        | WhenStmt(_, condition, body) ->
            [ yield!
                condition |> Option.toList |> List.map (fun expr -> prettyPrintExpr expr (indent + 2))
              yield! body |> List.map (fun line -> prettyPrintLine line (indent + 2)) ]
        | ReturnStmt(_, value)
        | RestoreStmt(_, value) ->
            value |> Option.toList |> List.map (fun expr -> prettyPrintExpr expr (indent + 2))
        | ExitStmt _
        | NextStmt _
        | Remark _ -> []
        |> String.concat ""
    currentLine + childLines

and private prettyPrintLine (node: Line) (indent: int) : string =
    let padding = String.replicate indent " "
    let (Line(_, n, children)) = node
    let lineText = Option.map string n |> Option.defaultValue ""
    let currentLine = sprintf "%s- Line (%s)\n" padding lineText
    let childLines = children |> List.map (fun child -> prettyPrintStmt child (indent + 2)) |> String.concat ""
    currentLine + childLines

let prettyPrintAst (node: Ast) (indent: int) : string =
    let padding = String.replicate indent " "
    match node with
    | Program(_, children) ->
        let currentLine = sprintf "%s- Program\n" padding
        let childLines = children |> List.map (fun child -> prettyPrintLine child (indent + 2)) |> String.concat ""
        currentLine + childLines

let printAST (indent: string) (node: Ast) =
    match node with
    | Program(_, children) ->
        printfn "%sProgram" indent
        children
        |> List.iter (fun line ->
            let (Line(_, n, stmts)) = line
            let lineText = Option.map string n |> Option.defaultValue ""
            printfn "%s  Line (%s)" indent lineText
            stmts |> List.iter (fun stmt -> printf "%s" (prettyPrintStmt stmt 4)))

type ResultBuilder() =
    member _.Return(x) : Result<'T, 'E> = Ok x
    member _.ReturnFrom(m: Result<'T, 'E>) = m
    member _.Bind(m: Result<'T, 'E>, f: 'T -> Result<'U, 'E>) : Result<'U, 'E> =
        Result.bind f m
    member _.Zero() : Result<unit, 'E> = Ok ()
    member _.Combine(m: Result<'T, 'E>, f: unit -> Result<'U, 'E>) : Result<'U, 'E> =
        match m with
        | Ok _ -> f ()
        | Error e -> Error e
    member _.Delay(f: unit -> Result<'T, 'E>) = f
    member _.Run(f: unit -> Result<'T, 'E>) = f ()
    member _.TryWith(m: unit -> Result<'T, 'E>, handler: exn -> Result<'T, 'E>) =
        try m () with e -> handler e
    member _.TryFinally(m: unit -> Result<'T, 'E>, compensation: unit -> unit) =
        try m () finally compensation ()
    member _.Using(resource: #IDisposable, body: #IDisposable -> Result<'T, 'E>) =
        try body resource finally resource.Dispose()
    member _.While(condition: unit -> bool, body: unit -> Result<unit, 'E>) =
        let rec loop () =
            if condition () then
                match body () with
                | Ok () -> loop ()
                | Error e -> Error e
            else Ok ()
        loop ()
    member _.For(sequence: seq<'T>, body: 'T -> Result<unit, 'E>) =
        let rec loop seq =
            match Seq.tryHead seq with
            | None -> Ok ()
            | Some x ->
                match body x with
                | Ok () -> loop (Seq.tail seq)
                | Error e -> Error e
        loop sequence

let result = ResultBuilder()
