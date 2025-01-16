module ReformatParseTree

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open Antlr4.Runtime.Misc
open Utility

// ---------------------------------------------------
// 2. A record for node data
// ---------------------------------------------------
type FSNode = {
    RuleIndex  : int
    Exception  : RecognitionException
    SourceText : string
    Position   : int * int
    Children   : FSParseTree list
}

// ---------------------------------------------------
// 3. Single record type to hold the node
// ---------------------------------------------------
and FSParseTree = {
    Kind : NodeKind
    Data : FSNode
}

// ---------------------------------------------------
// 4. Helper: convert class name to NodeKind
// ---------------------------------------------------
let toNodeKind (antlrClassName: string) : NodeKind =
    match antlrClassName with
    | "AssignmentContext"       -> Assignment
    | "AssignmentTargetContext" -> AssignmentTarget
    | "BinaryExprContext"       -> BinaryExpr
    | "DimContext"              -> Dim
    | "ExpressionContext"       -> Expression
    | "IfContext"               -> If
    | "FunctionContext"         -> Function
    | "IdentifierContext"       -> Identifier
    | "ImplicitContext"         -> Implicit
    | "LineContext"             -> Line
    | "LineNumberContext"       -> LineNumber
    | "LocalContext"            -> Local
    | "LongForContext"          -> For
    | "NothingContext"          -> Nothing
    | "ParameterContext"        -> Parameter
    | "ProcedureContext"        -> Procedure
    | "ProcFnCallContext"       -> ProcFnCall
    | "ProgramContext"          -> Program
    | "ReferenceContext"        -> Reference
    | "RemarkContext"           -> Remark
    | "RepeatContext"           -> Repeat
    | "StmtContext"             -> Stmt
    | "StmtlistContext"         -> StmtList
    | "ValueContext"            -> Value
    | _                         -> Unknown

// ---------------------------------------------------
// 5. Extract text from node
// ---------------------------------------------------
let getTextForNode (node: ParserRuleContext) (input: ICharStream) : string =
    match node.Start, node.Stop with
    | null, _ | _, null -> "" // fallback
    | startToken, stopToken ->
        input.GetText(Interval(startToken.StartIndex, stopToken.StopIndex))

// ---------------------------------------------------
// 6. Create an FSParseTree node with empty children
// ---------------------------------------------------
let visitNode (node: ParserRuleContext) (inputStream: ICharStream) : FSParseTree =
    let nodeData = {
        RuleIndex  = node.RuleIndex
        Exception  = RecognitionException("Default exception placeholder", null, null, null)
        SourceText = getTextForNode node inputStream
        Position   = (node.Start.Line, node.Stop.Line)
        Children   = []
    }
    {
        Kind = toNodeKind (node.GetType().Name)
        Data = nodeData
    }

// ---------------------------------------------------
// 7. Recursively populate children
// ---------------------------------------------------
let rec traverseTree (node: ParserRuleContext) (inputStream: ICharStream) : FSParseTree =
    let current = visitNode node inputStream
    let childTrees =
        [ for i in 0 .. node.ChildCount - 1 do
            match node.children[i] with
            | :? ParserRuleContext as childCtx ->
                yield traverseTree childCtx inputStream
            | _ -> () ]

    { current with Data.Children = childTrees }

// ---------------------------------------------------
// 8. Entry point
// ---------------------------------------------------
let processParseTree (tree: IParseTree) (inputStream: ICharStream) : FSParseTree =
    traverseTree (tree :?> ParserRuleContext) inputStream
