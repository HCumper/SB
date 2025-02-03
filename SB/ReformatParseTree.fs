module ReformatParseTree

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open Antlr4.Runtime.Misc
open Utility

/// Convert Antlr4 parse tree to FSParseTree

// ---------------------------------------------------
// Helper: convert class name to NodeKind
// ---------------------------------------------------
let toNodeKind (antlrClassName: string) : NodeKind =
    match antlrClassName with
    | "AssignmentContext"       -> Assignment
    | "AssignmentTargetContext" -> AssignmentTarget
    | "BinaryContext"           -> BinaryExpr
    | "BinaryExprContext"       -> BinaryExpr
    | "DimContext"              -> Dim
    | "ExitstmtContext"         -> Exitstmt
    | "EndDefContext"           -> EndDef
    | "EndIfContext"            -> EndIf
    | "EndForContext"           -> EndFor
    | "EndRepeatContext"        -> EndRepeat
    | "ExpressionContext"       -> Expression
    | "ForloopContext"          -> For
    | "FunchdrContext"          -> Funchdr
    | "FuncContext"             -> Function
    | "IfContext"               -> If
    | "IdentifierContext"       -> Identifier
    | "IdentifierOnlyContext"   -> IdentifierOnly
    | "ImplicitContext"         -> Implicit
    | "LineContext"             -> Line
    | "LineNumberContext"       -> LineNumber
    | "LocContext"              -> Loc
    | "LocalContext"            -> Local
    | "LongForContext"          -> For
    | "NothingContext"          -> Nothing
    | "ParameterContext"        -> Parameters
    | "ParenthesizedlistContext"-> ParenthesizedList
    | "PrimaryContext"          -> Primary
    | "ProcContext"             -> Proc
    | "ProchdrContext"          -> Prochdr
    | "ProcedureContext"        -> Procedure
    | "ProcCallContext"         -> ProcFnCall
    | "ProgramContext"          -> Program
    | "ReferenceContext"        -> Reference
    | "RemarkContext"           -> Remark
    | "RepeatContext"           -> Repeat
    | "SeparatorContext"        -> Nothing
    | "StmtContext"             -> Stmt
    | "StmtlistContext"         -> StmtList
    | "TermContext"             -> TerminalNodeImpl
    | "TerminalNodeImpl"        -> TerminalNodeImpl
    | "UnparenthesizedlistContext"  -> UnparenthesizedList
    | "ValueContext"            -> Value
    | _                         -> Unknown

// ---------------------------------------------------
// Helper: extract text from node
// ---------------------------------------------------
let getTextForNode (node: ParserRuleContext) (input: ICharStream) : string =
    match node.Start, node.Stop with
    | null, _ | _, null -> "" // fallback
    | startToken, stopToken -> 
        input.GetText(Interval(startToken.StartIndex, stopToken.StopIndex))

/// Extracts line and column information from an IParseTree node
let extractPosition (tree: IParseTree) =
    match tree with
    | :? ITerminalNode as terminalNode -> 
        terminalNode.Symbol.Line, terminalNode.Symbol.Column
    | :? ParserRuleContext as context ->
        context.Start.Line, context.Start.Column

// ---------------------------------------------------
// Helper: Create an FSParseTree node with empty children
// ---------------------------------------------------
let visitNode (node: IParseTree) (inputStream: ICharStream) : FSParseTree =
    {
        Kind = toNodeKind (node.GetType().Name); 
        Exception  = None
        SourceText = node.GetText()
        Position   = extractPosition node   
        Children   = []
    }

/// Generalized function for visiting any node
let visitGenericNode (node: IParseTree) (inputStream: ICharStream) : FSParseTree =
    match node with
    | :? TerminalNodeImpl as terminalNode -> 
        {
            Kind = toNodeKind (terminalNode.GetType().Name)
            Exception  = None
            SourceText = terminalNode.Payload.Text
            Position   = (terminalNode.Payload.StartIndex, terminalNode.Payload.StopIndex)
            Children   = []
        }
    | _ -> visitNode node inputStream

// ---------------------------------------------------
// Recursively populate children and create the FSParseTree
// ---------------------------------------------------
let rec traverseTree (node: IParseTree) (inputStream: ICharStream): FSParseTree =
    let current = visitGenericNode node inputStream
    if node.ChildCount = 0 then 
        current // No children, return the current node
    else 
        // Recursively traverse children
        let children = 
            [ 0 .. node.ChildCount - 1 ]
            |> List.map (fun i -> node.GetChild(i))
            |> List.map (fun child -> traverseTree child inputStream)
        { current with Children = children }

let processParseTree (tree: IParseTree) (inputStream: ICharStream) : FSParseTree =
    traverseTree tree inputStream
