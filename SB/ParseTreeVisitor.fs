module ParseTreeVisitor

open Antlr4.Runtime.Tree
open System
open System.Collections.Generic
open Utility

(* The `ParseTreeVisitor` module is designed to traverse an ANTLR-generated parse tree and convert it into an Abstract Syntax Tree (AST). It defines various `NodeBehavior` types to specify how different parse tree nodes should be handled. The module includes dictionaries mapping parser rule contexts and token types to these behaviors. Helper functions are provided to extract positions and handle specific node types. The `ASTBuildingVisitor` class extends the base visitor to implement the traversal logic, applying the defined behaviors to produce the AST. The `convertTreeToAst` function initiates the conversion process using this visitor. *)

// ----------------------------------------------------------------
// 1. Types for Behavior
// ----------------------------------------------------------------

/// Defines how a parse tree node should be handled when building the AST.
type NodeBehavior =
    /// Create an AST node with the specified kind and use the node's text as content.
    | Produce of NodeKind

    /// Create an AST node with the specified kind but omit the node's text (i.e., store "").
    | ProduceNameOnly of NodeKind

    /// Create an AST node for an operator expression and store the operator text.
    | ProduceNameOnlyWithOperator of NodeKind

    /// Do not produce any AST node for this rule node (discard it entirely).
    | Discard

    /// Do not produce a node here; instead, return the child results as-is.
    | BubbleUp

// ----------------------------------------------------------------
// 2. Known Behaviors for Parser Rules and Tokens
// ----------------------------------------------------------------

/// Maps an SBParser context type to a NodeBehavior.
let typedBehaviors: IDictionary<Type, NodeBehavior> =
    dict [
        (typeof<SBParser.AssignContext>, ProduceNameOnly NodeKind.Assignment)
        (typeof<SBParser.AssigntoContext>, Discard)
        (typeof<SBParser.BinaryContext>, ProduceNameOnlyWithOperator NodeKind.BinaryExpr)
        (typeof<SBParser.ComparisonContext>, ProduceNameOnly NodeKind.BinaryExpr)
        (typeof<SBParser.DimContext>, Produce NodeKind.Dim)
        (typeof<SBParser.EndDefContext>, Discard)
        (typeof<SBParser.EndIfContext>, Discard)
        (typeof<SBParser.EndForContext>, Discard)
        (typeof<SBParser.EndRepeatContext>, Discard)
        (typeof<SBParser.ExitstmtContext>, Produce NodeKind.Exitstmt)
        (typeof<SBParser.ExprContext>, BubbleUp)
        (typeof<SBParser.ForloopContext>, ProduceNameOnly NodeKind.For)
        (typeof<SBParser.FunctionCallContext>, ProduceNameOnly NodeKind.FunctionCall)
        (typeof<SBParser.FunctionDefinitionContext>, ProduceNameOnly NodeKind.Function)
        (typeof<SBParser.FunchdrContext>, BubbleUp)
        (typeof<SBParser.IdentifierContext>, ProduceNameOnly NodeKind.Identifier)
        (typeof<SBParser.IfContext>, ProduceNameOnly NodeKind.If)
        (typeof<SBParser.ImplicitContext>, ProduceNameOnly NodeKind.Implicit)
        (typeof<SBParser.LineContext>, BubbleUp)
        (typeof<SBParser.LineNumberContext>, Discard)
        (typeof<SBParser.LocalContext>, ProduceNameOnly NodeKind.Local)
        (typeof<SBParser.ParametersContext>, ProduceNameOnly NodeKind.Parameters)
        (typeof<SBParser.ParenlistContext>, BubbleUp)
        (typeof<SBParser.ParenthesizedlistContext>, BubbleUp)
        (typeof<SBParser.PrimaryContext>, BubbleUp)
        (typeof<SBParser.ProcedureDefinitionContext>, ProduceNameOnly NodeKind.Procedure)
        (typeof<SBParser.ProcedureCallContext>, ProduceNameOnly NodeKind.ProcedureCall)
        (typeof<SBParser.ProchdrContext>, BubbleUp)
        (typeof<SBParser.ProgramContext>, ProduceNameOnly NodeKind.Program)
        (typeof<SBParser.RemarkContext>, Produce NodeKind.Remark)
        (typeof<SBParser.RepeatContext>, ProduceNameOnly NodeKind.Repeat)
        (typeof<SBParser.SeparatorContext>, Discard)
        (typeof<SBParser.StmtlistContext>, BubbleUp)
        (typeof<SBParser.UnaryContext>, BubbleUp)
        (typeof<SBParser.UnparenthesizedlistContext>, BubbleUp)
    ]

/// Maps a token name (string) to a NodeBehavior.
let terminalBehaviors: IDictionary<string, NodeBehavior> =
    dict [
        "COMMA", Discard
        "COMMENT", Discard
        "DEFFUNC", Discard
        "DEFPROC", Discard
        "DIMENSION", Discard
        "ENDDEF", Discard
        "ENDREPEAT", Discard
        "EOF", Discard
        "EQUAL", Discard
        "FOR", Discard
        "ID", Discard
        "IF", Discard
        "INTEGER", Produce NodeKind.NumberLiteral
        "LEFTPAREN", Discard
        "LOCAL", Discard
        "NEWLINE", Discard
        "REPEAT", Discard
        "RIGHTPAREN", Discard
        "TO", Discard
        "EXIT", Discard
        "STRING", Produce NodeKind.StringLiteral
        "COLON", Discard
        "SEMI", Discard
        "PLUS", Produce NodeKind.Plus
        "MINUS", Produce NodeKind.Minus
        "MULTIPLY", Produce NodeKind.Multiply
        "DIVIDE", Produce NodeKind.Divide
        "MOD", Produce NodeKind.Mod
        "DIV", Produce NodeKind.Div
        "AND", Discard
        "OR", Discard
        "XOR", Discard
        "CARET", Discard
        "NOT", Discard
        "TILDE", Discard
        "INSTR", Discard
        "AMP", Discard
        "QUESTION", Discard
        "POINT", Discard
        "BANG", Discard
        "GREATER", Discard
        "GREATEREQUAL", Discard
        "LESS", Discard
        "LESSEQUAL", Discard
        "THEN", Discard
        "ELSE", Discard
        "ENDFOR", Discard
        "ENDIF", Discard
    ]

// ----------------------------------------------------------------
// 3. Helper Functions
// ----------------------------------------------------------------

/// Extracts (line, column) position from the first token in the parse tree node.
let getPosition (ruleNode: IRuleNode) : int * int =
    let rec findFirstToken (node: IParseTree) =
        match node with
        | :? ITerminalNode as t -> Some t.Symbol
        | _ when node.ChildCount > 0 -> findFirstToken (node.GetChild(0))
        | _ -> None

    let rec findLastToken (node: IParseTree) =
        match node with
        | :? ITerminalNode as t -> Some t.Symbol
        | _ when node.ChildCount > 0 -> findLastToken (node.GetChild(node.ChildCount - 1))
        | _ -> None

    match findFirstToken ruleNode, findLastToken ruleNode with
    | Some first, Some _ -> (first.Line, first.Column)
    | _ -> (0, 0)

/// Handles special logic for a ProcedureDefinition node
let handleProcedureBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    match children with
    | [] ->
        // Fallback if no child nodes
        [ createAstNode NodeKind.ProcedureDefinition (ruleNode.GetChild(0).GetChild(1).GetText()) pos [] ]
    | _ ->
        [ createAstNode NodeKind.ProcedureDefinition (ruleNode.GetChild(0).GetChild(1).GetText()) pos children ]

/// Handles special logic for a FunctionDefinition node
let handleFunctionBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    match children with
    | [] ->
        [ createAstNode NodeKind.FunctionDefinition (ruleNode.GetChild(0).GetChild(1).GetText()) pos [] ]
    | _ ->
        [ createAstNode NodeKind.FunctionDefinition (ruleNode.GetChild(0).GetChild(1).GetText()) pos children ]

/// Handles special logic for a ProcedureCall
let handleProcedureCallBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    match children with
    | [] ->
        [ createAstNode NodeKind.ProcedureCall (ruleNode.GetChild(0).GetText()) pos [] ]
    | first :: args ->
        [ createAstNode NodeKind.ProcedureCall (ruleNode.GetChild(0).GetText()) pos args ]

/// Handles special logic for a FunctionCall
let handleFunctionCallBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    match children with
    | [] ->
        [ createAstNode NodeKind.FunctionCall (ruleNode.GetText()) pos [] ]
    | first :: args ->
        [ createAstNode NodeKind.FunctionCall first.Value pos args ]

/// Optionally handle an Implicit node, e.g. IMPLICIT% or IMPLICIT$
let handleImplicitBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    // The last char in the first child's text might be '%' or '$'
    let decorator =
        let txt = ruleNode.GetChild(0).GetText()
        if String.IsNullOrEmpty(txt) then ""
        else string (txt.[txt.Length - 1])
    let _::tl = children
    [ createAstNode NodeKind.Implicit decorator pos tl ]

/// Optionally handle a parameters node
let handleParametersBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    [ createAstNode NodeKind.Parameters "" pos children ]
// set mode so if it looks like an ID make it a parameter
/// Basic identifier handling (could be array indexing or function usage)
let handleIdentifierBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    let idText = ruleNode.GetText()
    match children with
    | [] ->
        [ createAstNode NodeKind.Identifier idText pos [] ]
    | [single] ->
        [ createAstNode NodeKind.ArrayOrFunctionCall idText pos [single] ]
    | multiple ->
        [ createAstNode NodeKind.Identifier idText pos multiple ]

// ----------------------------------------------------------------
// 4. The ASTBuildingVisitor Implementation
// ----------------------------------------------------------------

type ASTBuildingVisitor() =
    inherit SBBaseVisitor<ASTNode list>()

    override this.VisitChildren(ruleNode: IRuleNode) =
        // 1) Collect AST nodes from each child
        let childResults =
            [ for i in 0 .. ruleNode.ChildCount - 1 do
                yield! ruleNode.GetChild(i).Accept(this) ]

        // 2) Identify context type & text
        let ctxType = ruleNode.RuleContext.GetType()
        let content = ruleNode.GetText()
        let position = getPosition ruleNode

        // 3) Determine behavior
        let behavior =
            match typedBehaviors.TryGetValue(ctxType) with
            | true, b -> b
            | false, _ -> Produce NodeKind.Unknown

        // 4) Apply the behavior
        match behavior with
        | Produce kind ->
            [ createAstNode kind content position childResults ]

        | ProduceNameOnly kind ->
            match ctxType with
            | _ when ctxType = typeof<SBParser.IdentifierContext> ->
                handleIdentifierBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.ProcedureCallContext> ->
                handleProcedureCallBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.FunctionCallContext> ->
                handleFunctionCallBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.ProcedureDefinitionContext> ->
                handleProcedureBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.FunctionDefinitionContext> ->
                handleFunctionBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.ImplicitContext> ->
                handleImplicitBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.ParametersContext> ->
                handleParametersBehavior childResults position ruleNode
            | _ ->
                [ createAstNode kind "" position childResults ]

        | ProduceNameOnlyWithOperator kind ->
            let operatorNodeOpt =
                childResults
                |> List.tryFind (fun c ->
                    match c.TokenType with
                    | NodeKind.Plus
                    | NodeKind.Minus
                    | NodeKind.Multiply
                    | NodeKind.Divide
                    | NodeKind.Mod
                    | NodeKind.Div -> true
                    | _ -> false)
            let operatorText =
                operatorNodeOpt
                |> Option.map (fun op -> op.Value)
                |> Option.defaultValue "unknown-op"
            let filteredChildren =
                match operatorNodeOpt with
                | Some op -> childResults |> List.filter (fun x -> x <> op)
                | None -> childResults

            [ createAstNode kind operatorText position filteredChildren ]

        | Discard ->
            []

        | BubbleUp ->
            childResults

    override this.VisitTerminal(terminalNode: ITerminalNode) =
        let symbol = terminalNode.Symbol
        let tokenName = SBLexer.DefaultVocabulary.GetSymbolicName(symbol.Type).ToUpper()

        match terminalBehaviors.TryGetValue(tokenName) with
        | true, Produce kind ->
            [ createAstNode kind symbol.Text (symbol.Line, symbol.Column) [] ]
        | true, ProduceNameOnly kind ->
            [ createAstNode kind "" (symbol.Line, symbol.Column) [] ]
        | true, Discard ->
            []
        | _ ->
            [ createAstNode NodeKind.Unknown symbol.Text (symbol.Line, symbol.Column) [] ]

    override this.VisitErrorNode(errorNode: IErrorNode) =
        [ createAstNode NodeKind.Unknown (errorNode.ToString()) (0, 0) [] ]

// ----------------------------------------------------------------
// 5. Converting a Parse Tree to an AST
// ----------------------------------------------------------------

let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)
