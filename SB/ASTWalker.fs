module ASTWalker

open Antlr4.Runtime.Tree
open SymbolTable
open Utility

// Helper to cast to FSParseTree list and filter out terminal/separator nodes
let private meaningfulChildren (children: System.Collections.IEnumerable) =
    children
    |> Seq.cast<FSParseTree>
    |> List.ofSeq
    |> List.filter (fun x -> not (x.Kind = Terminal || x.Kind = Separator))

//  Helper to find a child by its index
let private getChild (context: FSParseTree) subscript =
    context.Data.Children[subscript]

// Helper to gather all children from a context and immediately filter them
let private gatherMeaningfulChildren (context: FSParseTree) =
    gatherFSChildren context.Data |> List.filter (fun x -> not (x.Kind = Terminal || x.Kind = Separator))

// Helper function counting the number of children in a context
let countChildren (context: FSParseTree) =  
    context.Data.Children.Length

/// Walks a DIM declaration node
let WalkDim (context: FSParseTree) =
    match context.Data.Children with
    | children when children.Length >= 3 ->
        let varName = (getChild context 0).Data.SourceText
        let termList = meaningfulChildren (getChild context 2).Data.Children
        (varName, termList)
    | _ -> failwith "Invalid DIM declaration"

/// Walks a LOCAL declaration node
let WalkLocal (context: FSParseTree) =
    if countChildren(context) < 2 then failwith "Invalid LOCAL declaration"
    let localKeyword = context.Data.Children[0].Data.SourceText
    //let paramList = context.Data.children[1].Payload :?> SBParser.UnparenthesizedlistContext
    //let termList = meaningfulChildren paramList.children
    (localKeyword, [])

/// Walks an assignment node
let WalkAssignment (context: FSParseTree) =
    if context.Data.Children.Length < 3 then failwith "Invalid assignment"
    let lvalue = context.Data.Children[0].Data.Children[0].Data.SourceText

    let dimensions =
        if countChildren context > 1 then
//            let paramList = context.Data.Children[0].GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
            []
        else []

    let rvalue = (getChild context 2).Data.Children[0].Data.Children[0].Data.Children[0].Data.SourceText

    // let targetDimensions =
    //     if ((getChild context 2).Data.Children[0].Data.Children[0].Data |> countChildren) > 1 then
    //         let paramList =
    //             context.GetChild(2).Data.Children[0].Data.Children[0].GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
    //         meaningfulChildren paramList.children
    //     else []

    let targetDimensions = []
    (lvalue, dimensions, rvalue, targetDimensions)

/// Walks the END DEF statement
let WalkEndDef (context: FSParseTree) action state =
    let endDefAction = action SBParser.EndDef
    let newState = { state with currentScope = globalScope }
    endDefAction "" [] newState

/// Walks a procedure or function definition
let WalkProcFunc (context: FSParseTree) =
    if countChildren context < 2 then failwith "Invalid procedure or function definition"
    let nameNode = getChild context 1
    let procName = (getChild (getChild nameNode 0) 0).Data.SourceText
    let paramList =
        if countChildren nameNode > 1 then gatherMeaningfulChildren (getChild nameNode 1) else []
    (procName, paramList)

/// Walks an IMPLICIT declaration
let WalkImplicit (context: FSParseTree) =
    // if countChildren context < 2 then failwith "Invalid IMPLICIT declaration"
    // let implic = context.Data.Children[0]Data.Children[0].SourceText
    // let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    // let termList = meaningfulChildren paramList.children
    // let stringValues = termList |> List.map (fun x -> xData.Children[0].SourceText)
    //(implic, stringValues)
    ([], "")

/// Walks a FOR loop node
let WalkFor (context: FSParseTree) =
    if countChildren context < 6 then failwith "Invalid FOR loop syntax"
    let loopVar = context.Data.Children[1].Data.Children[0].Data.SourceText
    let initialValue = context.Data.Children[3].Data.SourceText
    let finalValue = getChild context 5
    let step =
        if countChildren context > 6 then
            let s = context.Data.Children[0].Data.SourceText.ToUpper()
            if s = "STEP" && countChildren context > 7 then (getChild context 7).Data.Children[0].Data.SourceText else "1"
        else "1"
    (loopVar, initialValue, finalValue, step)

/// Walks an IF statement node (placeholder)
let WalkIf (context: FSParseTree) =
    // Example: gather all children, excluding certain node types
    "bif"

/// Walks a REPEAT loop node
let WalkRepeat (context: FSParseTree) =
    let loopVar = context.Data.Children[1].Data.Children[0].Data.SourceText
    loopVar

/// Walks a binary expression node (placeholder)
let WalkBinaryExpr (context: FSParseTree) action state =
    let binaryAction = action 1000
    binaryAction "" [] state

/// Walks a terminal node (placeholder)
let WalkTerminal (context: FSParseTree) action state =
    let _text = context.Data.Children[0].Data.SourceText
    state
