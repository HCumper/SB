module CodeGenerator

open Utility
open Antlr4.StringTemplate


// ---------------------------------------------------------
// 2. Utility function: get or guess C# type from SBTypes
//    (If you need typed variables, use this. We won't use
//     it for function return types, since they're always void now.)
// ---------------------------------------------------------
let sbTypeToCSharp (t: SBTypes) =
    match t with
    | SBTypes.String  -> "string"
    | SBTypes.Integer -> "int"
    | SBTypes.Real    -> "double"
    | SBTypes.Unknown -> "object"
    | SBTypes.Void    -> "void"  // fallback, though for local variables maybe?

// ---------------------------------------------------------
// 3. DRY Helper: Render a given template with named attributes
// ---------------------------------------------------------
let renderTemplate (tg: TemplateGroup) (templateName: string) (pairs: (string * obj) list) =
    let tmpl = tg.GetInstanceOf(templateName)
    for (k, v) in pairs do
        tmpl.Add(k, v) |> ignore
    tmpl.Render()

// ---------------------------------------------------------
// 4. Expressions: Convert ASTNode -> string (via templates)
// ---------------------------------------------------------
let rec astNodeToExpression (state: ProcessingState) (node: ASTNode) : string =
    let tg = state.Templates
    match node.TokenType with
    | NumberLiteral ->
        // node.Value = e.g. "123"
        renderTemplate tg "exprNumber" [ "num", node.Value ]

    | StringLiteral ->
        renderTemplate tg "exprString" [ "txt", node.Value ]

    | Identifier ->
        renderTemplate tg "exprIdentifier" [ "id", node.Value ]

    | BinaryExpr ->
        // We might store the operator in node.Value or sub-nodes (Plus, Minus, etc.)
        // Suppose node.Value is something like "+", "-", "*", "/"
        // node.Children = [ leftExpr; rightExpr ]
        if node.Children.Length <> 2 then
            renderTemplate tg "exprUnrecognized" [ "kind", "BinaryExpr (invalid structure)" ]
        else
            let left = astNodeToExpression state node.Children.[0]
            let right = astNodeToExpression state node.Children.[1]
            
            // If node.Value is empty, maybe the operator is in node.Children, 
            // or you might have NodeKind = Plus, Minus, Multiply, etc.
            let op =
                match node.TokenType with
                | BinaryExpr -> node.Value  // or check subnodes
                | Plus -> "+"
                | Minus -> "-"
                | Multiply -> "*"
                | Divide -> "/"
                | Mod -> "%"
                | Div -> "/"  // or whatever your logic for integer division is
                | _ -> node.Value // fallback

            renderTemplate tg "exprBinary"
                [ "left", left
                  "op", op
                  "right", right ]

    | CallExpr
    | ProcedureCall
    | FunctionCall
    | ArrayOrFunctionCall ->
        // node.Value = function name, node.Children = arguments
        let argExprs =
            node.Children
            |> List.map (astNodeToExpression state)

        renderTemplate tg "exprCall"
            [ "name", node.Value
              "args", argExprs ]

    | _ ->
        // Fallback for unhandled expression nodes
        renderTemplate tg "exprUnrecognized"
            [ "kind", node.TokenType.ToString() ]


// ---------------------------------------------------------
// 5. Statements: Convert ASTNode -> string (via templates)
// ---------------------------------------------------------
let rec astNodeToStatement (state: ProcessingState) (node: ASTNode) : string =
    let tg = state.Templates
    match node.TokenType with
    | Assignment ->
        // Typically node.Children = [ target, value ]
        if node.Children.Length <> 2 then
            "// Invalid assignment structure"
        else
            let targetStr = astNodeToExpression state node.Children.[0]
            let valueStr  = astNodeToExpression state node.Children.[1]
            renderTemplate tg "stmtAssignment"
                [ "target", targetStr
                  "value", valueStr ]

    | Exitstmt ->
        renderTemplate tg "stmtBreak" []

    | Return ->
        // If single child => return <expr>;
        match node.Children with
        | [ exprNode ] ->
            let exprStr = astNodeToExpression state exprNode
            renderTemplate tg "stmtReturn" [ "expr", exprStr ]
        | [] ->
            // Just "return;"
            renderTemplate tg "stmtReturn" []
        | _ ->
            "// Invalid return structure"

    | Remark ->
        renderTemplate tg "stmtComment" [ "text", node.Value ]

    // Possibly other statements like "If", "For", "Repeat", etc. 
    // Here we just show a basic approach for calls or default
    | ProcedureCall
    | FunctionCall
    | CallExpr
    | ArrayOrFunctionCall ->
        let callStr = astNodeToExpression state node
        renderTemplate tg "stmtExpression" [ "expr", callStr ]

    | _ ->
        // fallback
        renderTemplate tg "stmtUnrecognized"
            [ "kind", node.TokenType.ToString() ]


// ---------------------------------------------------------
// 6. Generate methods (procedures or functions) from the AST
//    In your new definition, "functions and procedures do not have return types" => always void
// ---------------------------------------------------------
let generateMethodSyntax (state: ProcessingState) (definitionNode: ASTNode) : string =
    // We'll unify function vs. procedure => always "void"
    let tg = state.Templates
    let methodName = definitionNode.Value

    // For the body, we look for child nodes of type Body, StmtList, etc.
    let bodyStmts =
        definitionNode.Children
        |> List.collect (fun child ->
            match child.TokenType with
            | Body
            | StmtList ->
                // Each child is a statement
                child.Children
                |> List.map (astNodeToStatement state)
            | _ -> []
        )

    // If you want parameters, you could parse them from the AST or symbol table
    // For this example, we keep it empty
    let parameters = ""

    // Render the method template
    renderTemplate tg "methodDef"
        [ "modifiers", "public"
          "name", methodName
          "parameters", parameters
          "bodyLines", bodyStmts ]


// ---------------------------------------------------------
// 7. Build a class from all procedure/function definitions
// ---------------------------------------------------------
let buildClassFromAst (state: ProcessingState) (root: ASTNode) (className: string) : string =
    let tg = state.Templates

    // Collect all definitions
    let methodNodes =
        root.Children
        |> List.filter (fun c ->
            c.TokenType = ProcedureDefinition
            || c.TokenType = FunctionDefinition
        )

    // Generate strings for each method
    let methodStrings =
        methodNodes
        |> List.map (generateMethodSyntax state)

    // Fill class template
    renderTemplate tg "classDef"
        [ "modifiers", "public"
          "className", className
          "members", methodStrings ]


// ---------------------------------------------------------
// 8. Top-level function to produce final C# code as string
// ---------------------------------------------------------
let generateCSharp (state: ProcessingState) (className: string) : string =
    let tg = state.Templates
    let classString = buildClassFromAst state state.Ast className

    // Wrap in a namespace
    renderTemplate tg "namespaceDef"
        [ "nsName",  "MyNamespace"
          "content", classString ]



