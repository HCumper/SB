module ParseTreeListener

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open System.Collections.Generic
open Utility

// ----------------------------------------------------------------
// 1. Types for your AST and behaviors
// ----------------------------------------------------------------
type NodeBehavior =
    | Produce of NodeKind
    | ProduceNameOnly of NodeKind
    | Discard
    | BubbleUp

/// Known rule behaviors keyed by rule name (from `SBParser.ruleNames`)
let typedBehaviors: IDictionary<Type, NodeBehavior> =
    dict [
        (typeof<SBParser.AssignContext>, ProduceNameOnly NodeKind.Assignment)
        (typeof<SBParser.AssigntoContext>, Discard)
        (typeof<SBParser.BinaryContext>, ProduceNameOnly NodeKind.BinaryExpr)
        (typeof<SBParser.ComparisonContext>, ProduceNameOnly NodeKind.Comparison)
        (typeof<SBParser.DimContext>, Produce NodeKind.Dim)
        (typeof<SBParser.EndDefContext>, Discard)
        (typeof<SBParser.EndIfContext>, Discard)
        (typeof<SBParser.EndForContext>, Discard)
        (typeof<SBParser.EndRepeatContext>, Discard)
        (typeof<SBParser.ExitstmtContext>, Produce NodeKind.Exitstmt)
        (typeof<SBParser.ExprContext>, BubbleUp)
        (typeof<SBParser.ForloopContext>, ProduceNameOnly NodeKind.Forloop)
        (typeof<SBParser.FuncContext>, ProduceNameOnly NodeKind.Function)
        (typeof<SBParser.FunchdrContext>, BubbleUp)
        (typeof<SBParser.IdentifierContext>, BubbleUp)
        (typeof<SBParser.IfContext>, ProduceNameOnly NodeKind.If)
        (typeof<SBParser.ImplicitContext>, Produce NodeKind.Implicit)
        (typeof<SBParser.LineContext>, BubbleUp)
        (typeof<SBParser.LineNumberContext>, Discard)
        (typeof<SBParser.LocalContext>, Produce NodeKind.Local)
        (typeof<SBParser.ParametersContext>, ProduceNameOnly NodeKind.Parameters)
        (typeof<SBParser.ParenlistContext>, BubbleUp)
        (typeof<SBParser.ParenthesizedlistContext>, BubbleUp)
        (typeof<SBParser.PrimaryContext>, BubbleUp)
        (typeof<SBParser.ProcContext>, ProduceNameOnly NodeKind.ProcedureDefinition)
        (typeof<SBParser.ProcFnCallContext>, ProduceNameOnly NodeKind.ProcFnCall)
        (typeof<SBParser.ProchdrContext>, BubbleUp)
        (typeof<SBParser.ProgramContext>, ProduceNameOnly Program)
        (typeof<SBParser.RemarkContext>, Produce NodeKind.Remark)
        (typeof<SBParser.RepeatContext>, Discard)
        (typeof<SBParser.SeparatorContext>, Discard)
        (typeof<SBParser.StmtlistContext>, BubbleUp)
        (typeof<SBParser.UnaryContext>, BubbleUp)
        (typeof<SBParser.UnparenthesizedlistContext>, BubbleUp)
        // "ASSIGNTO", Discard
        // "ENDDEF", Discard
        // "EXPR", BubbleUp
        // "FUNC", BubbleUp
        // "FUNCDEF", ProduceNameOnly NodeKind.Function
        // "FUNCHDR", ProduceNameOnly NodeKind.Funchdr
        // "IDENTIFIER", Produce NodeKind.Identifier
        // "LINE", BubbleUp
        // "LINENUMBER", Discard
        // "PARENTHESIZEDLIST", ProduceNameOnly NodeKind.ParenthesizedList
        // "PROGRAM", ProduceNameOnly NodeKind.Program
        // "SEPARATOR", Discard
        // "STMTLIST", BubbleUp
        // "STMT", BubbleUp
        // Standard rules
        // "TERMINALNODE", Produce NodeKind.TerminalNodeImpl
        //
        // "PARAMETERS", ProduceNameOnly NodeKind.Parameters
        // // SB-specific constructs
        // "DIM", Produce NodeKind.Dim
        // "IMPLICIT", Produce NodeKind.Implicit
        // "REFERENCE155", Produce NodeKind.Reference
        // "PROC", Produce NodeKind.Procedure
        // "FORLOOP", Produce NodeKind.For
        // "IF", Produce NodeKind.If
        // "LONGSELECT", Produce NodeKind.Unknown   // No corresponding DU value; using Unknown
        // "ONSELECT", Produce NodeKind.Unknown      // No corresponding DU value; using Unknown
        // "NEXTSTMT", Produce NodeKind.Unknown        // No corresponding DU value; using Unknown
        // "EXITSTMT", Produce NodeKind.Exitstmt
        // "PROCCALL", Produce NodeKind.ProcFnCall
        // "IDENTIFIERONLY", Produce NodeKind.IdentifierOnly
        //
        // "FUNC", BubbleUp
     ]
    
/// Known terminal behaviors keyed by token type
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
        "ID", Produce NodeKind.ID
        "IF", Discard
        "IMPLIC", Discard
        "INTEGER", Produce NodeKind.NumberLiteral
        "LEFTPAREN", Discard
        "LOCAL", Discard
        "NEWLINE", Discard
//        "PARENTHESIZEDLIST", Produce NodeKind.ParenthesizedList
        "REPEAT", ProduceNameOnly NodeKind.Repeat
        "RIGHTPAREN", Discard
        "TO", Discard
        "EXIT", Discard
        // "IDENTIFIER", Produce NodeKind.Identifier
        // "NUMBER", Produce NodeKind.NumberLiteral
        "STRING", Produce NodeKind.StringLiteral
        //
        // "FUNCTION", ProduceNameOnly NodeKind.Function
        // "WS", Discard
        //
        // // Additional keywords and punctuation
        // "REFERENCE", ProduceNameOnly NodeKind.Reference
        // "IMPLICIT", ProduceNameOnly NodeKind.Implicit
        // "LOCAL", ProduceNameOnly NodeKind.Local
        // "DIM", ProduceNameOnly NodeKind.Dim
        // "DEFPROC", Discard
        // "DEFFUNC", Discard
        // "IF", ProduceNameOnly NodeKind.If
        // "ELSE", ProduceNameOnly NodeKind.Unknown  // No DU case for Else; using Unknown
        // "THEN", Discard
        // "ENDIF", Produce NodeKind.EndIf
        // "SELECT", ProduceNameOnly NodeKind.Unknown  // No DU case for Select; using Unknown
        // "ENDSELECT", Discard
        // "ON", ProduceNameOnly NodeKind.Reference     // Approximate mapping
        // "NEXT", Discard                              // Could also be mapped to Unknown if needed
        // "STEP", Discard
        // "EXIT", ProduceNameOnly NodeKind.Exitstmt
        // "UNTIL", Discard
        //
        // Punctuation and operators (all discarded)
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
// 2. The Visitor with purely functional child results aggregation
// ----------------------------------------------------------------

type ASTBuildingVisitor() =
    inherit SBBaseVisitor<ASTNode list>()

    /// Collect AST nodes from each child, then apply the known behavior
    override this.VisitChildren(ruleNode: IRuleNode) =
        // 1) Collect child results in a functional style, no mutables:
        let childResults =
            [0 .. ruleNode.ChildCount - 1]
            |> List.collect (fun i ->
                 let child = ruleNode.GetChild(i)
                 child.Accept(this)
               )

        // 2) Identify the rule name
        let ruleIndex = ruleNode.RuleContext.RuleIndex
        let content = ruleNode.GetText()
        let ruleName = SBParser.ruleNames.[ruleIndex]
        let ctxType = ruleNode.RuleContext.GetType()
        
        // 3) Check typedBehaviors
        let behavior =
            match typedBehaviors.TryGetValue(ctxType) with
            | true, b -> b
            | false, _ -> Produce NodeKind.Unknown
            
        // 3) Position info (optional)
        let pos = getPosition ruleNode
        // 4) Look up the behavior
        match behavior with
        | Produce kind ->
            // Produce a new AST node containing all child results
            [ createAstNode kind content pos childResults ]

        | ProduceNameOnly kind ->
            // Produce a new AST node containing all child results but no content
            [ createAstNode kind ("") pos childResults ]  // "would be " + content

        | Discard ->
            // Discard node and children
            []

        | BubbleUp ->
            // Return the child results as-is
            childResults

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
        [createAstNode ErrorNode (errorNode.ToString()) (0, 0) [] ]

// ----------------------------------------------------------------
// 3. Converting a parse tree (root) to an AST
// ----------------------------------------------------------------
let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)
