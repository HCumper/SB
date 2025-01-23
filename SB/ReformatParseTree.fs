module ReformatParseTree

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open Antlr4.Runtime.Misc
open Utility

// ---------------------------------------------------
// 4. Helper: convert class name to NodeKind
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
    | "ParameterContext"        -> Parameter
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
    | "TermContext"             -> Term
    | "UnparenthesizedlistContext"  -> UnparenthesizedList
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
