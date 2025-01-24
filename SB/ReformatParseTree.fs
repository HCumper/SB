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
    | "TermContext"             -> TerminalNodeImpl
    | "TerminalNodeImpl"        -> TerminalNodeImpl
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
let visitNode (node: IParseTree) (inputStream: ICharStream) : FSParseTree =
    let nodeData = {
//        RuleIndex  = node.RuleIndex
        Exception  = RecognitionException("Default exception placeholder", null, null, null)
        SourceText = node.GetText()
        Position   = (node.SourceInterval.a, node.SourceInterval.b)
        Children   = []
    }
    {
        Kind = toNodeKind (node.GetType().Name)
        Data = nodeData
    }

let visitTerminal (node: TerminalNodeImpl) (inputStream: ICharStream) : FSParseTree =
    let nodeData = {
//        RuleIndex  = 0
        Exception  = RecognitionException("Default exception placeholder", null, null, null)
        SourceText = node.Payload.Text
        Position   = (node.Payload.StartIndex, node.Payload.StopIndex)
        Children   = []
    }
    {
        Kind = toNodeKind (node.GetType().Name)
        Data = nodeData
    }

// ---------------------------------------------------
// 7. Recursively populate children
// ---------------------------------------------------
let rec traverseTree (node: IParseTree) (inputStream: ICharStream): FSParseTree =
    let current = visitNode node inputStream
    if node.ChildCount = 0 then
        // No children, return current node
        current
    else
        // Recursively traverse children
        let children = 
            [0..node.ChildCount-1]
            |> List.map (fun i -> node.GetChild(i))
            |> List.map (fun child -> traverseTree child inputStream)
        // Concatenate existing children with new ones
        { current with Data.Children = children }

// let rec traverseTree (node: ParserRuleContext) (inputStream: ICharStream) : FSParseTree =
//     let current = visitNode node inputStream
//     let childTrees =
//         node.children
//         |> List.ofSeq
//         |> List.choose (function
//             | :? ParserRuleContext as childCtx -> Some (traverseTree childCtx inputStream)
//             | _ -> None)
//
//     { current with Data.Children = childTrees }

// ---------------------------------------------------
// 8. Entry point
// ---------------------------------------------------
let processParseTree (tree: IParseTree) (inputStream: ICharStream) : FSParseTree =
    traverseTree tree inputStream 
