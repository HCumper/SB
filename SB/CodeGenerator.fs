module CodeGenerator

open System

open Types
open ProcessingTypes
open SyntaxAst

// Emit a minimal C# projection of the current AST; unsupported constructs are left as comments.
let private sbTypeToCSharp (t: SBType) =
    match t with
    | SBType.String -> "string"
    | SBType.Integer -> "int"
    | SBType.Real -> "double"
    | SBType.Void -> "void"
    | SBType.Unknown -> "object"

let private indent level (text: string) =
    let padding = String.replicate (level * 4) " "
    text.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
    |> Array.map (fun line -> if line = "" then "" else padding + line)
    |> String.concat Environment.NewLine

let rec private exprToCode (node: Expr) : string =
    match node with
    | NumberLiteral(_, value)
    | StringLiteral(_, value)
    | Identifier(_, value) -> value
    | PostfixName(_, name, None) -> name
    | PostfixName(_, name, Some args) ->
        let renderedArgs = args |> List.map exprToCode |> String.concat ", "
        $"{name}({renderedArgs})"
    | SliceRange(_, lhs, rhs) -> $"{exprToCode lhs} TO {exprToCode rhs}"
    | BinaryExpr(_, op, lhs, rhs) -> $"({exprToCode lhs} {op} {exprToCode rhs})"
    | UnaryExpr(_, op, expr) -> $"({op} {exprToCode expr})"

let rec private blockStatements block =
    match block with
    | StatementBlock stmts -> stmts
    | LineBlock lines -> lines |> List.collect (fun (Line(_, _, stmts)) -> stmts)

let rec private stmtToCode (node: Stmt) : string =
    match node with
    | Assignment(_, target, value) ->
        $"{exprToCode target} = {exprToCode value};"
    | GotoStmt(_, target) ->
        $"// GOTO {exprToCode target}"
    | GosubStmt(_, target) ->
        $"// GOSUB {exprToCode target}"
    | OnGotoStmt(_, selector, targets) ->
        let renderedTargets = targets |> List.map exprToCode |> String.concat ", "
        $"// ON {exprToCode selector} GOTO {renderedTargets}"
    | OnGosubStmt(_, selector, targets) ->
        let renderedTargets = targets |> List.map exprToCode |> String.concat ", "
        $"// ON {exprToCode selector} GOSUB {renderedTargets}"
    | ExitStmt _ -> "break;"
    | ReturnStmt(_, Some exprNode) -> $"return {exprToCode exprNode};"
    | ReturnStmt _ -> "return;"
    | Remark(_, text) -> $"// {text}"
    | LocalStmt(_, items) ->
        let names = items |> List.map fst |> List.filter (String.IsNullOrWhiteSpace >> not) |> String.concat ", "
        if String.IsNullOrWhiteSpace names then "// LOCAL" else $"object {names};"
    | ProcedureCall(_, name, args) ->
        let renderedArgs = args |> List.map exprToCode |> String.concat ", "
        $"{name}({renderedArgs});"
    | ChannelProcedureCall(_, name, channel, args) ->
        let renderedArgs = (channel :: args) |> List.map exprToCode |> String.concat ", "
        $"{name}({renderedArgs});"
    | IfStmt(_, cond, thenBody, elseBody) ->
        let thenLines = blockStatements thenBody |> List.map stmtToCode |> List.map (indent 1) |> String.concat Environment.NewLine
        let elseLines =
            elseBody
            |> Option.map (blockStatements >> List.map stmtToCode >> List.map (indent 1) >> String.concat Environment.NewLine)

        match elseLines with
        | Some elseText when not (String.IsNullOrWhiteSpace elseText) ->
            String.concat Environment.NewLine
                [ $"if {exprToCode cond}"
                  "{"
                  thenLines
                  "}"
                  "else"
                  "{"
                  elseText
                  "}" ]
        | _ ->
            String.concat Environment.NewLine
                [ $"if {exprToCode cond}"
                  "{"
                  thenLines
                  "}" ]
    | ForStmt(_, name, startExpr, endExpr, stepExpr, body) ->
        let stepText = stepExpr |> Option.map exprToCode |> Option.defaultValue "1"
        let bodyText = blockStatements body |> List.map stmtToCode |> List.map (indent 1) |> String.concat Environment.NewLine
        String.concat Environment.NewLine
            [ $"for (var {name} = {exprToCode startExpr}; {name} <= {exprToCode endExpr}; {name} += {stepText})"
              "{"
              bodyText
              "}" ]
    | RepeatStmt(_, _, body) ->
        let bodyText = blockStatements body |> List.map stmtToCode |> List.map (indent 1) |> String.concat Environment.NewLine
        String.concat Environment.NewLine
            [ "while (true)"
              "{"
              bodyText
              "}" ]
    | DimStmt(_, items) ->
        items
        |> List.map (fun (name, dims) ->
            let dimText = dims |> List.map exprToCode |> String.concat ", "
            $"// DIM {name}({dimText})")
        |> String.concat Environment.NewLine
    | ProcedureDef _
    | FunctionDef _ -> ""
    | SelectStmt _ -> "// SELECT not yet implemented"
    | WhenStmt _ -> "// WHEN not yet implemented"
    | DataStmt(_, exprs) ->
        let renderedExprs = exprs |> List.map exprToCode |> String.concat ", "
        $"// DATA {renderedExprs}"
    | ReadStmt(_, exprs) ->
        let renderedExprs = exprs |> List.map exprToCode |> String.concat ", "
        $"// READ {renderedExprs}"
    | RestoreStmt(_, value) ->
        match value with
        | Some expr -> $"// RESTORE {exprToCode expr}"
        | None -> "// RESTORE"
    | ImplicitStmt _ -> "// IMPLICIT"
    | ReferenceStmt(_, exprs) ->
        let renderedExprs = exprs |> List.map exprToCode |> String.concat ", "
        $"// REFERENCE {renderedExprs}"
    | NextStmt(_, name) -> $"// NEXT {name}"

let private methodParameters parameters =
    parameters
    |> List.map (fun paramName -> $"object {paramName}")
    |> String.concat ", "

let private generateMethodSyntax (_state: ProcessingState) (definitionNode: Stmt) : string =
    let returnType, methodName, parameters, body =
        match definitionNode with
        | FunctionDef(_, name, parms, body) -> "object", name, parms, body
        | ProcedureDef(_, name, parms, body) -> sbTypeToCSharp SBType.Void, name, parms, body
        | _ -> sbTypeToCSharp SBType.Void, "UnnamedMethod", [], []

    let bodyText =
        body
        |> List.collect (fun (Line(_, _, stmts)) -> stmts)
        |> List.map stmtToCode
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> List.map (indent 2)
        |> String.concat Environment.NewLine

    let renderedBody =
        if String.IsNullOrWhiteSpace bodyText then indent 2 "// no body"
        else bodyText

    String.concat Environment.NewLine
        [ $"    public static {returnType} {methodName}({methodParameters parameters})"
          "    {"
          renderedBody
          "    }" ]

let private topLevelDefinitions root =
    match root with
    | Program(_, children) ->
        children
        |> List.collect (fun (Line(_, _, stmts)) -> stmts)
        |> List.filter (function ProcedureDef _ | FunctionDef _ -> true | _ -> false)

let buildProgramFromAst (state: ProcessingState) (root: Ast) (className: string) : string =
    let safeClassName = if String.IsNullOrWhiteSpace className then "GeneratedProgram" else className
    let methodStrings = topLevelDefinitions root |> List.map (generateMethodSyntax state)

    let members =
        if List.isEmpty methodStrings then
            [ "    public static void Main()"
              "    {"
              "        // No procedures or functions were generated."
              "    }" ]
        else
            methodStrings

    String.concat Environment.NewLine
        ([ "using System;"
           ""
           $"public static class {safeClassName}"
           "{" ]
         @ members
         @ [ "}" ])

let generateCSharp (state: ProcessingState) (className: string) : string =
    // Global lines become the program body source; top-level routines are emitted as sibling methods.
    buildProgramFromAst state state.Ast className
