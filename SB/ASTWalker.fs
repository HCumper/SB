module ASTWalker

open Antlr4.Runtime.Tree
open SymbolTable
open Utility

// Helper to cast to FSParseTree list and filter out terminal/separator nodes
let private meaningfulChildren (children: System.Collections.IEnumerable) =
    children
    |> Seq.cast<FSParseTree>
    |> List.ofSeq
    |> List.filter (fun x -> not (x.Kind = TerminalNodeImpl || x.Kind = Separator))

//  Helper to find a child by its index
let private getChild (context: FSParseTree) subscript =
    context.Children[subscript]

// Helper to gather all children from a context and immediately filter them
let private gatherMeaningfulChildren (context: FSParseTree) =
    gatherFSChildren context |> List.filter (fun x -> not (x.Kind = TerminalNodeImpl || x.Kind = Separator))

// Helper function counting the number of children in a context
let countChildren (context: FSParseTree) =  
    context.Children.Length

/// Walks a DIM declaration node
let WalkDim (context: FSParseTree) =
    match context.Children with
    | children when children.Length >= 3 ->
        let varName = (getChild context 0).SourceText
        let termList = meaningfulChildren (getChild context 2).Children
        (varName, termList)
    | _ -> failwith "Invalid DIM declaration"

/// Walks a LOCAL declaration node
let WalkLocal (context: FSParseTree) =
    if countChildren(context) < 2 then failwith "Invalid LOCAL declaration"
    let localKeyword = context.Children[0].SourceText
    //let paramList = context.children[1].Payload :?> SBParser.UnparenthesizedlistContext
    //let termList = meaningfulChildren paramList.children
    (localKeyword, [])

/// Walks an assignment node
let WalkAssignment (context: FSParseTree) =
    if context.Children.Length < 3 then failwith "Invalid assignment"
    let lvalue = context.Children[0].Children[0].SourceText

    let dimensions =
        if countChildren context > 1 then
//            let paramList = context.Children[0].GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
            []
        else []

    let rvalue = (getChild context 2).Children[0].Children[0].Children[0].SourceText

    // let targetDimensions =
    //     if ((getChild context 2).Children[0].Children[0].Data |> countChildren) > 1 then
    //         let paramList =
    //             context.GetChild(2).Children[0].Children[0].GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
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
    let procName = (getChild (getChild nameNode 0) 0).SourceText
    let paramList =
        if countChildren nameNode > 1 then gatherMeaningfulChildren (getChild nameNode 1) else []
    (procName, paramList)

/// Walks an IMPLICIT declaration
let WalkImplicit (context: FSParseTree) =
    // if countChildren context < 2 then failwith "Invalid IMPLICIT declaration"
    // let implic = context.Children[0]Children[0].SourceText
    // let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    // let termList = meaningfulChildren paramList.children
    // let stringValues = termList |> List.map (fun x -> xChildren[0].SourceText)
    //(implic, stringValues)
    ([], "")

/// Walks a FOR loop node
let WalkFor (context: FSParseTree) =
    if countChildren context < 6 then failwith "Invalid FOR loop syntax"
    let loopVar = context.Children[1].Children[0].SourceText
    let initialValue = context.Children[3].SourceText
    let finalValue = getChild context 5
    let step =
        if countChildren context > 6 then
            let s = context.Children[0].SourceText.ToUpper()
            if s = "STEP" && countChildren context > 7 then (getChild context 7).Children[0].SourceText else "1"
        else "1"
    (loopVar, initialValue, finalValue, step)

/// Walks an IF statement node (placeholder)
let WalkIf (context: FSParseTree) =
    // Example: gather all children, excluding certain node types
    "bif"

/// Walks a REPEAT loop node
let WalkRepeat (context: FSParseTree) =
    let loopVar = context.Children[1].Children[0].SourceText
    loopVar

/// Walks a binary expression node (placeholder)
let WalkBinaryExpr (context: FSParseTree) action state =
    let binaryAction = action 1000
    binaryAction "" [] state

/// Walks a terminal node (placeholder)
let WalkTerminal (context: FSParseTree) action state =
    let _text = context.Children[0].SourceText
    state
