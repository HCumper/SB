module ASTWalker

open Antlr4.Runtime.Tree
open SymbolTable
open Utility

// Helper to cast to FSParseTree list and filter out terminal/separator nodes
let private meaningfulChildren (children: System.Collections.IEnumerable) =
    children
    |> Seq.cast<FSParseTree>
    |> List.ofSeq
    |> List.filter (fun x -> not (x.Kind = TerminalNodeImpl || x.Kind = SBParser.SeparatorContext))

// Helper to gather all children from a context and immediately filter them
let private gatherMeaningfulChildren (context: FSParseTree) =
    gatherChildren context |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))

/// Walks a DIM declaration node
let WalkDim (context: FSParseTree) =
    if context.ChildCount < 3 then failwith "Invalid DIM declaration"
    let varName = context.GetChild(1)Data.Children[0].SourceText
    let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
    let termList = meaningfulChildren paramList.children
    (varName, termList)

/// Walks a LOCAL declaration node
let WalkLocal (context: FSParseTree) =
    if context.Data.Children.Length < 2 then failwith "Invalid LOCAL declaration"
    let localKeyword = context.Data.Children[0].Data.SourceText
    //let paramList = context.Data.children[1].Payload :?> SBParser.UnparenthesizedlistContext
    //let termList = meaningfulChildren paramList.children
    (localKeyword, [])

/// Walks an assignment node
let WalkAssignment (context: FSParseTree) =
    if context.ChildCount < 3 then failwith "Invalid assignment"
    let lvalue = context.Data.Children[0].Data.Children[0].Data.SourceText

    let dimensions =
        if context.Data.Children[0].ChildCount > 1 then
            let paramList = context.Data.Children[0].GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
            meaningfulChildren paramList.children
        else []

    let rvalue = context.GetChild(2).Data.Children[0].Data.Children[0]Data.Children[0].SourceText

    let targetDimensions =
        if context.GetChild(2).Data.Children[0].Data.Children[0].ChildCount > 1 then
            let paramList =
                context.GetChild(2).Data.Children[0].Data.Children[0].GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
            meaningfulChildren paramList.children
        else []

    (lvalue, dimensions, rvalue, targetDimensions)

/// Walks the END DEF statement
let WalkEndDef (context: FSParseTree) action state =
    let endDefAction = action SBParser.EndDef
    let newState = { state with currentScope = globalScope }
    endDefAction "" [] newState

/// Walks a procedure or function definition
let WalkProcFunc (context: FSParseTree) =
    if context.ChildCount < 2 then failwith "Invalid procedure or function definition"
    let nameNode = context.GetChild(1)
    let procName = nameNode.Data.Children[0]Data.Children[0].SourceText
    let paramList =
        if nameNode.ChildCount > 1 then gatherMeaningfulChildren (nameNode.GetChild(1)) else []
    (procName, paramList)

/// Walks an IMPLICIT declaration
let WalkImplicit (context: FSParseTree) =
    if context.ChildCount < 2 then failwith "Invalid IMPLICIT declaration"
    let implic = context.Data.Children[0]Data.Children[0].SourceText
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let termList = meaningfulChildren paramList.children
    let stringValues = termList |> List.map (fun x -> xData.Children[0].SourceText)
    (implic, stringValues)

/// Walks a FOR loop node
let WalkFor (context: FSParseTree) =
    if context.ChildCount < 6 then failwith "Invalid FOR loop syntax"
    let loopVar = context.GetChild(1)Data.Children[0].SourceText
    let initialValue = context.GetChild(3)
    let finalValue = context.GetChild(5)
    let step =
        if context.ChildCount > 6 then
            let s = context.Data.Children[0].Data.SourceText.ToUpper()
            if s = "STEP" && context.ChildCount > 7 then context.GetChild(7)Data.Children[0].SourceText else "1"
        else "1"
    (loopVar, initialValue, finalValue, step)

/// Walks an IF statement node (placeholder)
let WalkIf (context: FSParseTree) =
    // Example: gather all children, excluding certain node types
    "bif"

/// Walks a REPEAT loop node
let WalkRepeat (context: FSParseTree) =
    let loopVar = context.GetChild(1)Data.Children[0].SourceText
    loopVar

/// Walks a binary expression node (placeholder)
let WalkBinaryExpr (context: FSParseTree) action state =
    let binaryAction = action 1000
    binaryAction "" [] state

/// Walks a terminal node (placeholder)
let WalkTerminal (context: FSParseTree) action state =
    let _text = contextData.Children[0].SourceText
    state
