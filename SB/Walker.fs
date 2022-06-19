module Walker

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable
open Utility
// responsible for understanding SBasic constructs and navigating them for an unspecified purpose

// turn context into F# list of Antlr nodes representing its children with syntactic garbage discarded
let private parseParenthesizedParamList (context: IParseTree) =
    gatherChildren context |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))            

let WalkDim (context : IParseTree) =
    let varName = context.GetChild(1).GetText()
    let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    (varName, termList)

let WalkLocal (context : IParseTree) =
    let localVar = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    (localVar, termList)

let WalkAssignment (context : IParseTree) =
    let lvalue = context.GetChild(0).GetChild(0).GetText()
    let dimensions =
        match context.GetChild(0).ChildCount with 
        | 1 -> []
        | _ -> 
            let paramList = context.GetChild(0).GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
            let (fList:IParseTree list) = copyAntlrList paramList.children
            fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    (lvalue, dimensions, context.GetChild(2))

let WalkEndDef (context: IParseTree) action state =
    let endDefAction = action SBParser.EndDef
    let newState = {state with currentScope = globalScope}
    let newState = endDefAction "" [] state
    {newState with currentScope = globalScope}

let WalkProcFunc (context : IParseTree) =
    let procName = context.GetChild(1).GetChild(0).GetText()
    let paramList =
        match context.GetChild(1).ChildCount with
        | 1 -> []
        | _ -> parseParenthesizedParamList (context.GetChild(1).GetChild(1))
    (procName, paramList)

let WalkImplicit (context : IParseTree) =
    let implic = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let stringValues = List.map (fun (x: IParseTree) -> x.GetText()) termList
    (implic, stringValues)
    
let WalkFor (context : IParseTree) =
    let loopVar = context.GetChild(1).GetText()
    let initialValue = context.GetChild(3) // must return whole node as it may be an expression
    let finalValue = context.GetChild(5)
    let step = 
        match context.GetChild(6) with
        | :? TerminalNodeImpl -> "1"
        | _ -> context.GetChild(7).GetText()
    (loopVar, initialValue, finalValue, step)

let WalkIf (context : IParseTree) =
    let termList = context |> gatherChildren |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.EndIfContext || x :? SBParser.LineNumberContext))
    "bif"

let WalkRepeat (context : IParseTree) =
    let loopVar = context.GetChild(1).GetText()
    loopVar

let WalkBinaryExpr (context : IParseTree) action state =
    let binaryAction = action 1000
    binaryAction "" [] state
    
let WalkTerminal (context : IParseTree) action state =
    let text = context.GetText()
    state

