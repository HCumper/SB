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
        // Standard rules
        "TERMINALNODE", Produce NodeKind.TerminalNodeImpl

        "ASSIGNMENT", ProduceNameOnly NodeKind.Assignment
        "ASSIGNTO", Produce NodeKind.Assignment
        "PARAMETERS", ProduceNameOnly NodeKind.Parameters
        "PARENTHESIZEDLIST", ProduceNameOnly NodeKind.ParenthesizedList
        "PROGRAM", ProduceNameOnly NodeKind.Program

        "EXPR", BubbleUp
        "FUNCHDR", ProduceNameOnly NodeKind.Funchdr
        "IDENTIFIER", BubbleUp
        "LINE", BubbleUp
        "PRIMARY", BubbleUp
        "STMT", BubbleUp
        "STMTLIST", BubbleUp

        "LINENUMBER", Discard

        // SB-specific constructs
        "DIM", Produce NodeKind.Dim
        "LOC", Produce NodeKind.Local
        "IMPLICIT", Produce NodeKind.Implicit
        "REFERENCE155", Produce NodeKind.Reference
        "PROC", Produce NodeKind.Procedure
        "FORLOOP", Produce NodeKind.For
        "REPEAT", Produce NodeKind.Repeat
        "IF", Produce NodeKind.If
        "LONGSELECT", Produce NodeKind.Unknown   // No corresponding DU value; using Unknown
        "ONSELECT", Produce NodeKind.Unknown      // No corresponding DU value; using Unknown
        "NEXTSTMT", Produce NodeKind.Unknown        // No corresponding DU value; using Unknown
        "EXITSTMT", Produce NodeKind.Exitstmt
        "PROCCALL", Produce NodeKind.ProcFnCall
        "IDENTIFIERONLY", Produce NodeKind.IdentifierOnly

        "FUNC", BubbleUp

        "SEPARATOR", Discard
    ]
    
/// Known terminal behaviors keyed by token type
let terminalBehaviors: IDictionary<string, NodeBehavior> =
    dict [
        "ENDDEF", Produce NodeKind.EndDef
        "ID", Produce NodeKind.ID
        "IDENTIFIER", Produce NodeKind.Identifier
        "INTEGER", Produce NodeKind.NumberLiteral
        "NUMBER", Produce NodeKind.NumberLiteral
        "STRING", Produce NodeKind.StringLiteral
        
        "FUNCTION", ProduceNameOnly NodeKind.Function
        "COMMENT", Discard
        "DEFFUNC", Produce NodeKind.DefFunc
        "WS", Discard
        
        // Additional keywords and punctuation
        "REFERENCE", ProduceNameOnly NodeKind.Reference
        "IMPLICIT", ProduceNameOnly NodeKind.Implicit
        "LOCAL", ProduceNameOnly NodeKind.Local
        "DIM", ProduceNameOnly NodeKind.Dim
        "DEFPROC", Discard
        "DEFFUNC", Discard
        "IF", ProduceNameOnly NodeKind.If
        "ELSE", ProduceNameOnly NodeKind.Unknown  // No DU case for Else; using Unknown
        "THEN", Discard
        "ENDIF", Produce NodeKind.EndIf
        "SELECT", ProduceNameOnly NodeKind.Unknown  // No DU case for Select; using Unknown
        "ENDSELECT", Discard
        "ON", ProduceNameOnly NodeKind.Reference     // Approximate mapping
        "FOR", ProduceNameOnly NodeKind.For
        "NEXT", Discard                              // Could also be mapped to Unknown if needed
        "TO", Discard
        "STEP", Discard
        "REPEAT", ProduceNameOnly NodeKind.Repeat
        "EXIT", ProduceNameOnly NodeKind.Exitstmt
        "UNTIL", Discard
        "ENDREPEAT", Discard

        // Punctuation and operators (all discarded)
        "EQUALS", Discard
        "LEFTPAREN", Discard
        "RIGHTPAREN", Discard
        "COLON", Discard
        "SEMI", Discard
        "PLUS", Discard
        "MINUS", Discard
        "MULTIPLY", Discard
        "DIVIDE", Discard
        "MOD", Discard
        "DIV", Discard
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
        "COMMA", Discard
        "BANG", Discard
        "NEWLINE", Discard
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
            [ createAstNode kind ("would be " + content) pos childResults ]

        | (true, Discard) ->
            // Discard node and children
            []

        | (true, BubbleUp) ->
            // Return the child results as-is
            childResults

        | (false, _) ->
            // Fallback: treat as unknown
            [ createAstNode Unknown (ruleName.ToUpper() + content) pos childResults ]

    /// Terminals: discard, or produce a leaf node if needed
    // Modified VisitTerminal method
    override this.VisitTerminal(terminalNode: ITerminalNode) =
        let symbol = terminalNode.Symbol
        let tokenType = SBLexer.DefaultVocabulary.GetSymbolicName(symbol.Type).ToUpper()
        
        match terminalBehaviors.TryGetValue(tokenType) with
        | (true, Produce kind) ->
            [ createAstNode kind symbol.Text (symbol.Line, symbol.Column) [] ]
        | (true, ProduceNameOnly kind) ->
            [ createAstNode kind ("would be " + symbol.Text) (symbol.Line, symbol.Column) [] ]
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
