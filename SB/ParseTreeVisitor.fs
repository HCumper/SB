module ParseTreeVisitor

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open System.Collections.Generic
open Utility


type NodeBehavior =
    | Produce of NodeKind
    | ProduceNameOnly of NodeKind
    | ProduceNameOnlyWithOperator of NodeKind
    | Discard
    | BubbleUp

// ----------------------------------------------------------------
// 2. Rationalized Known Behaviors
// ----------------------------------------------------------------

/// Dictionary keyed by the context type (using SBParser context types)
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
        // Retained commented-out alternatives:
        // ("ASSIGNTO", Discard)
        // ("ENDDEF", Discard)
        // ("EXPR", BubbleUp)
        // ("FUNC", BubbleUp)
        // ("FUNCDEF", ProduceNameOnly NodeKind.Function)
        // ("FUNCHDR", ProduceNameOnly NodeKind.Funchdr)
        // ("IDENTIFIER", Produce NodeKind.Identifier)
        // ("LINE", BubbleUp)
        // ("LINENUMBER", Discard)
        // ("PARENTHESIZEDLIST", ProduceNameOnly NodeKind.ParenthesizedList)
        // ("PROGRAM", ProduceNameOnly NodeKind.Program)
        // ("SEPARATOR", Discard)
        // ("STMTLIST", BubbleUp)
        // ("STMT", BubbleUp)
        // Standard rules:
        // ("TERMINALNODE", Produce NodeKind.TerminalNodeImpl)
        // ("PARAMETERS", ProduceNameOnly NodeKind.Parameters)
        // SB-specific constructs:
        // ("DIM", Produce NodeKind.Dim)
        // ("IMPLICIT", Produce NodeKind.Implicit)
        // ("REFERENCE155", Produce NodeKind.Reference)
        // ("PROC", Produce NodeKind.Procedure)
        // ("FORLOOP", Produce NodeKind.For)
        // ("IF", Produce NodeKind.If)
        // ("LONGSELECT", Produce NodeKind.Unknown)
        // ("ONSELECT", Produce NodeKind.Unknown)
        // ("NEXTSTMT", Produce NodeKind.Unknown)
        // ("EXITSTMT", Produce NodeKind.Exitstmt)
        // ("PROCCALL", Produce NodeKind.ProcFnCall)
        // ("IDENTIFIERONLY", Produce NodeKind.IdentifierOnly)
        // ("FUNC", BubbleUp)
    ]

/// Dictionary of terminal behaviors keyed by token type string.
/// Retained commented-out alternatives are left as comments.
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
        // "PARENTHESIZEDLIST", Produce NodeKind.ParenthesizedList
        "REPEAT", Discard
        "RIGHTPAREN", Discard
        "TO", Discard
        "EXIT", Discard
        // "IDENTIFIER", Produce NodeKind.Identifier
        // "NUMBER", Produce NodeKind.NumberLiteral
        "STRING", Produce NodeKind.StringLiteral
        // "FUNCTION", ProduceNameOnly NodeKind.Function
        // "WS", Discard
        // Additional keywords and punctuation:
        // "REFERENCE", ProduceNameOnly NodeKind.Reference
        // "IMPLICIT", ProduceNameOnly NodeKind.Implicit
        // "LOCAL", ProduceNameOnly NodeKind.Local
        // "DIM", ProduceNameOnly NodeKind.Dim
        // "DEFPROC", Discard
        // "DEFFUNC", Discard
        // "IF", ProduceNameOnly NodeKind.If
        // "ELSE", ProduceNameOnly NodeKind.Unknown
        // "THEN", Discard
        // "ENDIF", Produce NodeKind.EndIf
        // "SELECT", ProduceNameOnly NodeKind.Unknown
        // "ENDSELECT", Discard
        // "ON", ProduceNameOnly NodeKind.Reference
        // "NEXT", Discard
        // "STEP", Discard
        // "EXIT", ProduceNameOnly NodeKind.Exitstmt
        // "UNTIL", Discard
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

/// Retrieves the first and last token positions (line, column) for a rule node.
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
    | _ -> (0, 0) // Fallback if tokens not found

/// Helper for handling identifier-specific behavior.
let handleIdentifierBehavior (childResults: ASTNode list) (pos: int * int) : ASTNode list =
    let idNodeOpt = childResults |> List.tryFind (fun c -> c.TokenType = NodeKind.ID)
    let otherChildren = childResults |> List.filter (fun c -> c.TokenType <> NodeKind.ID)
    match idNodeOpt with
    | Some idNode ->
        match otherChildren with
        | [] ->
            [ createAstNode NodeKind.Identifier "" pos [ idNode ] ]
        | [ singleExpr ] ->
            [ createAstNode NodeKind.ArrayOrFunctionCall "" pos [ idNode; singleExpr ] ]
        | multiple ->
            [ createAstNode NodeKind.ProcFnCall "" pos (idNode :: multiple) ]
    | None ->
        [ createAstNode NodeKind.Identifier "" pos childResults ]

// ----------------------------------------------------------------
// 4. ASTBuildingVisitor Implementation
// ----------------------------------------------------------------

type ASTBuildingVisitor() =
    inherit SBBaseVisitor<ASTNode list>()

    override this.VisitChildren(ruleNode: IRuleNode) =
        // Collect children using list comprehension
        let childResults =
            [0 .. ruleNode.ChildCount - 1]
            |> List.collect (fun i -> ruleNode.GetChild(i).Accept(this))
        // Retrieve rule information
        let ruleIndex = ruleNode.RuleContext.RuleIndex
        let content = ruleNode.GetText()
        let ruleName = SBParser.ruleNames.[ruleIndex]
        let ctxType = ruleNode.RuleContext.GetType()
        let behavior =
            match typedBehaviors.TryGetValue(ctxType) with
            | true, b -> b
            | false, _ -> Produce NodeKind.Unknown
        let pos = getPosition ruleNode
        // Apply behavior
        match behavior with
        | Produce kind ->
            [ createAstNode kind content pos childResults ]
        | ProduceNameOnly kind ->
            if ctxType = typeof<SBParser.IdentifierContext> then
                handleIdentifierBehavior childResults pos
            else
                [ createAstNode kind "" pos childResults ]
        | ProduceNameOnlyWithOperator kind ->
            let operatorNodeOpt =
                childResults
                |> List.tryFind (fun c ->
                    match c.TokenType with
                    | NodeKind.Plus | NodeKind.Minus | NodeKind.Multiply
                    | NodeKind.Divide | NodeKind.Mod | NodeKind.Div -> true
                    | _ -> false)
            let operatorText =
                operatorNodeOpt |> Option.map (fun op -> op.Value) |> Option.defaultValue "unknown-op"
            let filteredChildren =
                match operatorNodeOpt with
                | Some op -> childResults |> List.filter (fun c -> c <> op)
                | None -> childResults
            [ createAstNode kind operatorText pos filteredChildren ]
        | Discard ->
            []
        | BubbleUp ->
            childResults

    override this.VisitTerminal(terminalNode: ITerminalNode) =
        let symbol = terminalNode.Symbol
        let tokenType = SBLexer.DefaultVocabulary.GetSymbolicName(symbol.Type).ToUpper()
        match terminalBehaviors.TryGetValue(tokenType) with
        | (true, Produce kind) ->
            [ createAstNode kind symbol.Text (symbol.Line, symbol.Column) [] ]
        | (true, ProduceNameOnly kind) ->
            [ createAstNode kind "" (symbol.Line, symbol.Column) [] ]
        | (true, Discard) ->
            []
        | _ ->
            [ createAstNode NodeKind.Unknown symbol.Text (symbol.Line, symbol.Column) [] ]

    override this.VisitErrorNode(errorNode: IErrorNode) =
        [ createAstNode NodeKind.Unknown (errorNode.ToString()) (0, 0) [] ]

// ----------------------------------------------------------------
// 5. Convert a Parse Tree (Root) to an AST
// ----------------------------------------------------------------

let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)