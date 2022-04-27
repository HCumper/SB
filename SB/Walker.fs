module Walker

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable

// map with generic list to F# list
let rec private mapAntlr f (parentNode: Collections.Generic.IList<'a>) =
    match parentNode.Count with
    | 0 -> []
    | _ -> 
        // new head + new tail
        let head = f parentNode[0]
        let _ = parentNode.RemoveAt(0)  // destructive, can't be helped
        head :: mapAntlr f parentNode

// copy generic list to F# list
let private copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) = mapAntlr (fun x -> x) parentNode

let private WalkDim (context : IParseTree) action state =
    let varName = context.GetChild(1).GetText()
    let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children  
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let dimAction = action TokenType.Dimension
    dimAction varName termList state

let private WalkAssignment (context : IParseTree) action state =
    let lvalue =
        match context.GetChild(0).ChildCount with 
        | 1 -> 
            let ctx = context.GetChild(0).Payload :?> SBParser.IdentifierContext
            ctx.GetText()
        | _ -> 
            let ctx = context.GetChild(0).GetChild(0)
            ctx.GetText()
    let assignmentAction = action TokenType.ID
    assignmentAction lvalue [] state

let private parseParamList (context: IParseTree) =
    let paramList = context.GetChild(1).GetChild(1) :?> SBParser.ParenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children  
    fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))            

let private WalkProcedure (context : IParseTree) action state =
    let procName = context.GetChild(1).GetChild(0).GetText()
    let paramList =
        match context.GetChild(1).ChildCount with
        | 1 -> []
        | _ -> parseParamList context 

    let procedureAction = action TokenType.DefProc
    procedureAction procName paramList state

let private WalkFunction (context : IParseTree) action state =
    let funcName = context.GetChild(1).GetChild(0).GetText()
    let paramList =
        match context.GetChild(1).ChildCount with
        | 1 -> []
        | _ -> parseParamList context

    let functionAction = action TokenType.DefFunc
    functionAction funcName paramList state

let private WalkImplicit (context : IParseTree) action state =
    let implic = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children  
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let implicitAction = action TokenType.Implic
    implicitAction implic termList state
    
let private WalkLocal (context : IParseTree) action state =
    let locals = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children  
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let localAction = action TokenType.Local
    localAction locals termList state
    
let rec private WalkAcross (context : IParseTree) index action state =
    let result = 
        let count = context.ChildCount
        match index with
        | n when n < count ->
            let downValue = WalkDown (context.GetChild(index) : IParseTree) action state
            WalkAcross (((fst downValue): IParseTree).Parent : IParseTree) (index+1) action (snd downValue) 
        | _ -> state
    result
and 
    private WalkDown (context : IParseTree) action (state: State) =
        let newState = 
            match context with
            | :? SBParser.DimContext -> WalkDim context action state 
            | :? SBParser.AssignmentContext -> WalkAssignment context action state 
            | :? SBParser.ImplicitContext -> WalkImplicit context action state
            | :? SBParser.LocContext -> WalkLocal context action state
            | :? SBParser.ProchdrContext -> WalkProcedure context action state
            | :? SBParser.FunchdrContext -> WalkFunction context action  state
            | :? SBParser.EnddefContext -> { state with currentScope = "~Global" }
            | _ -> state
        (context, WalkAcross (context:IParseTree) 0 action newState )

// top level only
let WalkTreeRoot context action =
    let state = { implicitInts = Set.empty; implicitStrings = Set.empty; references = Set.empty; symTab = Map.empty; errorList = []; currentScope = "~Global" }
    WalkDown (context : ParserRuleContext) action state 
 