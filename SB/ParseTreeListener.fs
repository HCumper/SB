module ParseTreeListener

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System.Collections.Generic
open Utility

/// Helper function to get the first and last token positions
let getPosition (ruleNode: IRuleNode) : int * int =
    let rec findFirstToken (node: IParseTree) =
        match node with
        | :? ITerminalNode as terminal -> Some terminal.Symbol
        | _ when node.ChildCount > 0 -> findFirstToken (node.GetChild(0))
        | _ -> None

    let rec findLastToken (node: IParseTree) =
        match node with
        | :? ITerminalNode as terminal -> Some terminal.Symbol
        | _ when node.ChildCount > 0 -> findLastToken (node.GetChild(node.ChildCount - 1))
        | _ -> None

    match findFirstToken ruleNode, findLastToken ruleNode with
    | Some first, Some _ -> (first.Line, first.Column)
    | _ -> (0, 0)  // Default fallback

// ----------------------------------------------------------------
// 1. Types for your AST and behaviors
// ----------------------------------------------------------------
type NodeBehavior =
    | Produce of NodeKind
    | ProduceNameOnly of NodeKind
    | Discard
    | BubbleUp

/// Known rule behaviors keyed by rule name (from `SBParser.ruleNames`)
let knownBehaviors: IDictionary<string, NodeBehavior> =
    dict [
        "PROGRAM", Produce Program
        "FUNCHDR", ProduceNameOnly Function
        "TERMINALNODE", Produce TerminalNodeImpl
        "LINE",   BubbleUp
        "STMTLIST", BubbleUp
        "STMT",   BubbleUp
        "LINENUMBER",     Discard
    ]
    
let terminalBehaviors = 
    dict [
        "IDENTIFIER", Produce Identifier
        "NUMBER", Produce NumberLiteral
        "STRING", Produce StringLiteral
        "ENDDEF", Produce StringLiteral
        "FUNCHDR", ProduceNameOnly StringLiteral
        "FUNCTION", ProduceNameOnly StringLiteral
        "ID", Produce ID
        "COMMENT", Discard
        "WS", Discard
        // Add other terminal types as needed
    ]

// ----------------------------------------------------------------
// 2. The Visitor with purely functional child results aggregation
// ----------------------------------------------------------------

type ASTBuildingVisitor() =
    inherit SBBaseVisitor<ASTNode list>()

    /// Collect AST nodes from each child, then apply the known behavior
    override this.VisitChildren(ruleNode: IRuleNode) =
        // 1) Collect child results in a functional style, no mutables:
        let childResults =
            [ for i in 0 .. ruleNode.ChildCount - 1 do
                let child = ruleNode.GetChild(i)
                // Each child.Accept(this) returns ASTNode list
                yield! child.Accept(this)
            ]
            // The yield! splices each child's ASTNode list into the final collection

        // 2) Identify the rule name
        let ruleIndex = ruleNode.RuleContext.RuleIndex
        let content = ruleNode.GetText()
        let ruleName = SBParser.ruleNames.[ruleIndex]

        // 3) Position info (optional)
        let pos = getPosition ruleNode
        // 4) Look up the behavior
        match knownBehaviors.TryGetValue(ruleName.ToUpper()) with
        | (true, Produce kind) ->
            // Produce a new AST node containing all child results
            [ createAstNode kind content pos childResults ]

        | (true, ProduceNameOnly kind) ->
            // Produce a new AST node containing all child results but no content
            [ createAstNode kind "" pos childResults ]

        | (true, Discard) ->
            // Discard node and children
            []

        | (true, BubbleUp) ->
            // Return the child results as-is
            childResults

        | (false, _) ->
            // Fallback: treat as unknown
            [ createAstNode Unknown content pos childResults ]

    /// Terminals: discard, or produce a leaf node if needed
    // Modified VisitTerminal method
    override this.VisitTerminal(terminalNode: ITerminalNode) =
        let symbol = terminalNode.Symbol
        let tokenType = SBLexer.DefaultVocabulary.GetSymbolicName(symbol.Type).ToUpper()
        
        match terminalBehaviors.TryGetValue(tokenType) with
        | (true, Produce kind) ->
            [ createAstNode kind symbol.Text (symbol.Line, symbol.Column) [] ]
        | (true, ProduceNameOnly kind) ->
            [ createAstNode kind "" (symbol.Line, symbol.Column) [] ]
        | (true, Discard) -> []
        | _ -> 
            // Fallback for unhandled terminals
            [ createAstNode Unknown symbol.Text (symbol.Line, symbol.Column) [] ]

    /// Errors: typically return empty or produce an error node
    override this.VisitErrorNode(errorNode: IErrorNode) =
        []

// ----------------------------------------------------------------
// 3. Converting a parse tree (root) to an AST
// ----------------------------------------------------------------
let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)
