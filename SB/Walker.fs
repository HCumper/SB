module Walker

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable

// copy generic list to F# list non destructively
let rec copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) i newList = 
    if i = parentNode.Count then newList else
        parentNode.Item(i)::newList 
        |> copyAntlrList parentNode (i+1)  

let WalkDim (context : IParseTree) state =
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
    (localVar, termList)

let WalkAssignment (context : IParseTree) state =
    let lvalue = context.GetChild(0).GetText()
    let dimensions =
        match context.GetChild(0).ChildCount with 
        | 1 -> 
            let ctx = context.GetChild(0).Payload :?> SBParser.IdentifierContext
            ctx.GetText()
        | _ -> 
            let ctx = context.GetChild(0).GetChild(0)
            ctx.GetText()
    (lvalue, dimensions)

let parseParamList (context: IParseTree) =
    let paramList = context.GetChild(1).GetChild(1) :?> SBParser.ParenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children 0 []
    fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))            

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
    (procName, paramList)

let WalkFunction (context : IParseTree) state =
    let funcName = context.GetChild(1).GetChild(0).GetText()
    let paramList =
        match context.GetChild(1).ChildCount with
        | 1 -> []
        | _ -> parseParamList context
    (funcName, paramList)

let WalkImplicit (context : IParseTree) action state _ =
    let implic = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children 0 []
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let implicitAction = action SBParser.Implic
    implicitAction implic termList state
    state

let WalkBinaryExpr (context : IParseTree) action state =
    let binaryAction = action 1000
    binaryAction "" [] state
    
let WalkTerminal (context : IParseTree) action state =
    let text = context.GetText()
    state

//let rec WalkAcross (context : IParseTree) index action state =
//    let result = 
//        let count = context.ChildCount
//        match index with
//        | n when n < count ->
//            let downValue = WalkDown (context.GetChild(index) : IParseTree) action state
//            WalkAcross (((fst downValue): IParseTree).Parent : IParseTree) (index+1) action (snd downValue) 
//        | _ -> state
//    result
//and 
//    WalkDown (context : IParseTree) action (state: State) =
//        let newState = 
//            match context with
//            | :? SBParser.TermContext -> WalkTerminal context action state 
//            | :? SBParser.DimContext -> WalkDim context action state 
//            | :? SBParser.AssignmentContext -> WalkAssignment context action state 
////            | :? SBParser.ImplicitContext -> WalkImplicit context action state 8
//            | :? SBParser.LocContext -> WalkLocal context action state
//            | :? SBParser.ProchdrContext -> WalkProcedure context action state
//            | :? SBParser.FunchdrContext -> WalkFunction context action state
//            | :? SBParser.EndDefContext -> WalkEndDef context action state
//            | :? SBParser.BinaryContext -> WalkBinaryExpr context action state
//            | _ -> state
//        let (thing: State) =
//            match context with :? SBParser.ImplicitContext -> WalkImplicit context action state 8

//        (context, WalkAcross (context:IParseTree) 0 action newState )

//// top level only
//let WalkTreeRoot (context: ParserRuleContext) (action: int->String->IParseTree list->State->State) (state: State) =
//    WalkDown context action state 
