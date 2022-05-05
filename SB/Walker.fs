module Walker

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable

// copy generic list to F# list non destructively
let rec private copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) i newList = 
    if i = parentNode.Count then newList else
        newList @ [parentNode.Item(i)] 
        |> copyAntlrList parentNode (i+1)  

let private parseParamList (context: IParseTree) =
    let paramList = context.GetChild(1).GetChild(1) :?> SBParser.ParenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children 0 []
    fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))            

let WalkDim (context : IParseTree) =
    let varName = context.GetChild(1).GetText()
    let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children 0 []
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    (varName, termList)

let WalkLocal (context : IParseTree) state =
    let localVar = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children 0 [] 
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let stringValues = List.map (fun (x: IParseTree) -> x.GetText()) termList
    (localVar, stringValues)

let WalkAssignment (context : IParseTree) state =
    let lvalue = context.GetChild(0).GetChild(0).GetText()
    let dimensions =
        match context.GetChild(0).ChildCount with 
        | 1 -> []
        | _ -> 
            let paramList = context.GetChild(0).GetChild(1).Payload :?> SBParser.ParenthesizedlistContext
            let (fList:IParseTree list) = copyAntlrList paramList.children 0 []
            fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    (lvalue, dimensions)

let WalkEndDef (context: IParseTree) action state =
    let endDefAction = action SBParser.EndDef
    let newState = {state with currentScope = "~Global"}
    let newState = endDefAction "" [] state
    {newState with currentScope = "~global"}

let WalkProcedure (context : IParseTree) state =
    let procName = context.GetChild(1).GetChild(0).GetText()
    let paramList =
        match context.GetChild(1).ChildCount with
        | 1 -> []
        | _ -> parseParamList context
    let termList = paramList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let stringValues = List.map (fun (x: IParseTree) -> x.GetText()) termList
    (procName, stringValues)

let WalkFunction (context : IParseTree) state =
    let funcName = context.GetChild(1).GetChild(0).GetText()
    let paramList =
        match context.GetChild(1).ChildCount with
        | 1 -> []
        | _ -> parseParamList context
    let termList = paramList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let stringValues = List.map (fun (x: IParseTree) -> x.GetText()) termList
    (funcName, stringValues)

let WalkImplicit (context : IParseTree) state =
    let implic = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children 0 []
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let stringValues = List.map (fun (x: IParseTree) -> x.GetText()) termList
    (implic, stringValues)
    
let walkFor =
    0

let WalkBinaryExpr (context : IParseTree) action state =
    let binaryAction = action 1000
    binaryAction "" [] state
    
let WalkTerminal (context : IParseTree) action state =
    let text = context.GetText()
    state

