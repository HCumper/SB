module ParseTreeVisitor

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open System.Collections.Generic
open Utility

// ----------------------------------------------------------------
// 1. Types for Behavior
// ----------------------------------------------------------------

type NodeBehavior =
    | Produce of NodeKind
    | ProduceNameOnly of NodeKind
    | ProduceNameOnlyWithOperator of NodeKind
    | Discard
    | BubbleUp

// ----------------------------------------------------------------
// 2. Known Behaviors for Parser Rules and Tokens
// ----------------------------------------------------------------

/// A dictionary keyed by the rule context type, mapping to a NodeBehavior
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
        (typeof<SBParser.FuncContext>, ProduceNameOnly NodeKind.Function)
        (typeof<SBParser.FunchdrContext>, BubbleUp)
        (typeof<SBParser.IdentifierContext>, ProduceNameOnly NodeKind.Identifier)
        (typeof<SBParser.IfContext>, ProduceNameOnly NodeKind.If)
        (typeof<SBParser.ImplicitContext>, Produce NodeKind.Implicit)
        (typeof<SBParser.LineContext>, BubbleUp)
        (typeof<SBParser.LineNumberContext>, Discard)
        (typeof<SBParser.LocalContext>, Produce NodeKind.Local)
        (typeof<SBParser.ParametersContext>, ProduceNameOnly NodeKind.Parameters)
        (typeof<SBParser.ParenlistContext>, BubbleUp)
        (typeof<SBParser.ParenthesizedlistContext>, BubbleUp)
        (typeof<SBParser.PrimaryContext>, BubbleUp)
        (typeof<SBParser.ProcContext>, ProduceNameOnly NodeKind.Procedure)
        (typeof<SBParser.ProcFnCallContext>, ProduceNameOnly NodeKind.ProcFnCall)
        (typeof<SBParser.ProchdrContext>, BubbleUp)
        (typeof<SBParser.ProgramContext>, ProduceNameOnly NodeKind.Program)
        (typeof<SBParser.RemarkContext>, Produce NodeKind.Remark)
        (typeof<SBParser.RepeatContext>, ProduceNameOnly NodeKind.Repeat)
        (typeof<SBParser.SeparatorContext>, Discard)
        (typeof<SBParser.StmtlistContext>, BubbleUp)
        (typeof<SBParser.UnaryContext>, BubbleUp)
        (typeof<SBParser.UnparenthesizedlistContext>, BubbleUp)
    ]

/// Dictionary of terminal behaviors keyed by the token type name (string)
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
        "IMPLIC", Discard
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

/// Retrieves the (line, column) for the first token in a rule node
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

/// Handle the creation of a ProcFnCall node specifically
let handleProcFnCallBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    // Suppose the first child is the procedure name, the rest are arguments
    match children with
    | [] ->
        // No children => just a fallback
        [ createAstNode NodeKind.ProcFnCall (ruleNode.GetText()) pos [] ]
    | first :: args ->
        [ createAstNode NodeKind.ProcFnCall first.Value pos args ]

/// Basic identifier handling
let handleIdentifierBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    // If we want to see if there's something like array indexing or function call
    let idText = ruleNode.GetText()
    match children with
    | [] ->
        // Just "identifier" with no sub-expressions
        [ createAstNode NodeKind.Identifier idText pos [] ]
    | [single] ->
        // Possibly an array or function reference
        [ createAstNode NodeKind.ArrayOrFunctionCall idText pos [single] ]
    | multiple ->
        // fallback scenario
        [ createAstNode NodeKind.Identifier idText pos multiple ]

// ----------------------------------------------------------------
// 4. The ASTBuildingVisitor Implementation
// ----------------------------------------------------------------

type ASTBuildingVisitor() =
    inherit SBBaseVisitor<ASTNode list>()

    override this.VisitChildren(ruleNode: IRuleNode) =
        // 1) Collect child results
        let childResults =
            [ for i in 0 .. ruleNode.ChildCount - 1 do
                yield! ruleNode.GetChild(i).Accept(this) ]

        // 2) Determine context info
        let ruleIndex = ruleNode.RuleContext.RuleIndex
        let ctxType = ruleNode.RuleContext.GetType()
        let content = ruleNode.GetText()
        let position = getPosition ruleNode

        // 3) Look up typed behavior
        let behavior =
            match typedBehaviors.TryGetValue(ctxType) with
            | true, b -> b
            | false, _ -> Produce NodeKind.Unknown

        // 4) Apply the behavior
        match behavior with
        | Produce kind ->
            [ createAstNode kind content position childResults ]

        | ProduceNameOnly kind ->
            // Special-case handling for some rules:
            if ctxType = typeof<SBParser.IdentifierContext> then
                handleIdentifierBehavior childResults position ruleNode
            elif ctxType = typeof<SBParser.ProcFnCallContext> then
                handleProcFnCallBehavior childResults position ruleNode
            else
                [ createAstNode kind "" position childResults ]

        | ProduceNameOnlyWithOperator kind ->
            // e.g. for BinaryContext with an operator
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
                | Some op -> childResults |> List.filter ((<>) op)
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
            // Fallback for unhandled tokens
            [ createAstNode NodeKind.Unknown symbol.Text (symbol.Line, symbol.Column) [] ]

    override this.VisitErrorNode(errorNode: IErrorNode) =
        [ createAstNode NodeKind.Unknown (errorNode.ToString()) (0, 0) [] ]

// ----------------------------------------------------------------
// 5. Converting a Parse Tree to an AST
// ----------------------------------------------------------------

let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)
