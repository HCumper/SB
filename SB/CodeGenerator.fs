module CodeGenerator

open System
open Types

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

let rec astNodeToExpression (state: ProcessingState) (node: ASTNode) : string =
    match node.Kind with
    | NodeKind.NumberLiteral
    | NodeKind.StringLiteral
    | NodeKind.Identifier
    | NodeKind.ID -> node.Value
    | NodeKind.BinaryExpr ->
        match node.Children with
        | [ left; right ] ->
            let lhs = astNodeToExpression state left
            let rhs = astNodeToExpression state right
            $"({lhs} {node.Value} {rhs})"
        | _ -> node.Value
    | NodeKind.CallExpr
    | NodeKind.ProcedureCall
    | NodeKind.FunctionCall
    | NodeKind.ArrayOrFunctionCall ->
        let args = node.Children |> List.map (astNodeToExpression state) |> String.concat ", "
        $"{node.Value}({args})"
    | _ when node.Children.IsEmpty -> node.Value
    | _ -> node.Children |> List.map (astNodeToExpression state) |> String.concat ", "

let rec astNodeToStatement (state: ProcessingState) (node: ASTNode) : string =
    match node.Kind with
    | NodeKind.Assignment ->
        match node.Children with
        | [ target; value ] ->
            $"{astNodeToExpression state target} = {astNodeToExpression state value};"
        | _ -> $"// Invalid assignment: {node.Value}"
    | NodeKind.ExitStmt -> "break;"
    | NodeKind.Return ->
        match node.Children with
        | [ exprNode ] -> $"return {astNodeToExpression state exprNode};"
        | _ -> "return;"
    | NodeKind.Remark -> $"// {node.Value}"
    | NodeKind.Local ->
        let names =
            node.Children
            |> List.map (fun child -> child.Value)
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat ", "
        if String.IsNullOrWhiteSpace names then
            "// LOCAL"
        else
            $"object {names};"
    | NodeKind.ProcedureCall
    | NodeKind.FunctionCall
    | NodeKind.CallExpr
    | NodeKind.ArrayOrFunctionCall -> $"{astNodeToExpression state node};"
    | NodeKind.If
    | NodeKind.For
    | NodeKind.Repeat
    | NodeKind.Select
    | NodeKind.When ->
        $"// Unsupported statement kind: {node.Kind} ({node.Value})"
    | _ ->
        let expr = astNodeToExpression state node
        if String.IsNullOrWhiteSpace expr then $"// Unsupported statement kind: {node.Kind}"
        else $"{expr};"

let private parameters (definitionNode: ASTNode) =
    definitionNode.Children
    |> List.filter (fun child -> child.Kind = NodeKind.Parameters)
    |> List.collect (fun paramGroup -> paramGroup.Children)
    |> List.map (fun param ->
        let paramName = if String.IsNullOrWhiteSpace param.Value then "arg" else param.Value
        $"object {paramName}")
    |> String.concat ", "

let private methodBodyLines (state: ProcessingState) (definitionNode: ASTNode) =
    definitionNode.Children
    |> List.collect (fun child ->
        match child.Kind with
        | NodeKind.Local -> [ astNodeToStatement state child ]
        | NodeKind.Body
        | NodeKind.StmtList -> child.Children |> List.map (astNodeToStatement state)
        | _ -> [])

let generateMethodSyntax (state: ProcessingState) (definitionNode: ASTNode) : string =
    let returnType =
        match definitionNode.Kind with
        | NodeKind.FunctionDefinition -> "object"
        | _ -> sbTypeToCSharp SBType.Void

    let body =
        methodBodyLines state definitionNode
        |> List.map (indent 2)
        |> String.concat Environment.NewLine

    let methodName =
        if String.IsNullOrWhiteSpace definitionNode.Value then "UnnamedMethod" else definitionNode.Value

    let bodyText =
        if String.IsNullOrWhiteSpace body then indent 2 "// no body"
        else body

    String.concat Environment.NewLine
        [ $"    public static {returnType} {methodName}({parameters definitionNode})"
          "    {"
          bodyText
          "    }" ]

let buildProgramFromAst (state: ProcessingState) (root: ASTNode) (className: string) : string =
    let safeClassName =
        if String.IsNullOrWhiteSpace className then "GeneratedProgram"
        else className

    let methodStrings =
        root.Children
        |> List.filter (fun c ->
            c.Kind = NodeKind.ProcedureDefinition || c.Kind = NodeKind.FunctionDefinition)
        |> List.map (generateMethodSyntax state)

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
    buildProgramFromAst state state.Ast className
