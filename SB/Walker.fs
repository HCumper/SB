module Walker

open Antlr4.Runtime.Tree
open SymbolTable
open Utility

// Helper to cast to IParseTree list and filter out terminal/separator nodes
let private meaningfulChildren (children: System.Collections.IEnumerable) =
    children
    |> Seq.cast<IParseTree>
    |> List.ofSeq
    |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))

// Helper to gather all children from a context and immediately filter them
let private gatherMeaningfulChildren (context: IParseTree) =
    gatherChildren context |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))

/// Walks a DIM declaration node
let WalkDim (context: IParseTree) =
    if context.ChildCount < 3 then failwith "Invalid DIM declaration"
    let varName = context.GetChild(1).GetText()
    let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
    let termList = meaningfulChildren paramList.children
    (varName, termList)

/// Walks a LOCAL declaration node
let WalkLocal (context: IParseTree) =
    if context.ChildCount < 2 then failwith "Invalid LOCAL declaration"
    let localKeyword = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let termList = meaningfulChildren paramList.children
    (localKeyword, termList)

/// Walks an assignment node
let WalkAssignment (context: IParseTree) =
    if context.ChildCount < 3 then failwith "Invalid assignment"
    let lvalue = context.GetChild(0).GetChild(0).GetText()

    let dimensions =
        if context.GetChild(0).ChildCount > 1 then
            let paramList = context.GetChild(0).GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
            meaningfulChildren paramList.children
        else []

    let rvalue = context.GetChild(2).GetChild(0).GetChild(0).GetText()

    let targetDimensions =
        if context.GetChild(2).GetChild(0).GetChild(0).ChildCount > 1 then
            let paramList =
                context.GetChild(2).GetChild(0).GetChild(0).GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
            meaningfulChildren paramList.children
        else []

    (lvalue, dimensions, rvalue, targetDimensions)

/// Walks the END DEF statement
let WalkEndDef (context: IParseTree) action state =
    let endDefAction = action SBParser.EndDef
    let newState = { state with currentScope = globalScope }
    endDefAction "" [] newState

/// Walks a procedure or function definition
let WalkProcFunc (context: IParseTree) =
    if context.ChildCount < 2 then failwith "Invalid procedure or function definition"
    let nameNode = context.GetChild(1)
    let procName = nameNode.GetChild(0).GetText()
    let paramList =
        if nameNode.ChildCount > 1 then gatherMeaningfulChildren (nameNode.GetChild(1)) else []
    (procName, paramList)

/// Walks an IMPLICIT declaration
let WalkImplicit (context: IParseTree) =
    if context.ChildCount < 2 then failwith "Invalid IMPLICIT declaration"
    let implic = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let termList = meaningfulChildren paramList.children
    let stringValues = termList |> List.map (fun x -> x.GetText())
    (implic, stringValues)

/// Walks a FOR loop node
let WalkFor (context: IParseTree) =
    if context.ChildCount < 6 then failwith "Invalid FOR loop syntax"
    let loopVar = context.GetChild(1).GetText()
    let initialValue = context.GetChild(3)
    let finalValue = context.GetChild(5)
    let step =
        if context.ChildCount > 6 then
            let s = context.GetChild(6).GetText().ToUpper()
            if s = "STEP" && context.ChildCount > 7 then context.GetChild(7).GetText() else "1"
        else "1"
    (loopVar, initialValue, finalValue, step)

/// Walks an IF statement node (placeholder)
let WalkIf (context: IParseTree) =
    // Example: gather all children, excluding certain node types
    "bif"

/// Walks a REPEAT loop node
let WalkRepeat (context: IParseTree) =
    let loopVar = context.GetChild(1).GetText()
    loopVar

/// Walks a binary expression node (placeholder)
let WalkBinaryExpr (context: IParseTree) action state =
    let binaryAction = action 1000
    binaryAction "" [] state

/// Walks a terminal node (placeholder)
let WalkTerminal (context: IParseTree) action state =
    let _text = context.GetText()
    state
