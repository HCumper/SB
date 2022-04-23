module Walker

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable

// map with generic list to F# list
let rec mapAntlr f (parentNode: Collections.Generic.IList<'a>) =
    match parentNode.Count with
    | 0 -> []
    | _ -> 
        // new head + new tail
        let head = f parentNode[0]
        let _ = parentNode.RemoveAt(0)  // destructive, can't be helped
        head :: mapAntlr f parentNode

// copy generic list to F# list
let copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) = mapAntlr (fun x -> x) parentNode

let dimSymbol state construct =
    state

let addItem inputList x =
    x

let walkParenthesizedList itemList resultList =
    itemList |> mapAntlr     
    addItem

let WalkDim (context : IParseTree) state action =
    let varName = context.GetChild(1).GetText()
    let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children  
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let dimAction = action TokenType.Dimension
    dimAction varName termList state

let WalkAssignment (context : IParseTree) state action =
    let lvalue =
        match context.GetChild(0).ChildCount with 
        | 1 -> 
            let ctx =context.GetChild(0).Payload :?> SBParser.IdentifierContext
            ctx.GetText()
        | _ -> 
            let ctx = context.GetChild(0).GetChild(0)
            ctx.GetText()
    let assignmentAction = action TokenType.ID
    assignmentAction lvalue [] state

let WalkProcedure (context : IParseTree) state action =
    let procName = context.GetChild(1).GetChild(0).GetText()
    let paramList =
        match context.GetChild(1).ChildCount with
        | 1 -> []
        | _ -> 
            let paramList = context.GetChild(1).GetChild(1) :?> SBParser.ParenthesizedlistContext
            let (fList:IParseTree list) = copyAntlrList paramList.children  
            fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))            

    let procedureAction = action TokenType.DefProc
    procedureAction procName paramList state

let WalkFunction (context : IParseTree) state action =
    let funcName = context.GetChild(1).GetChild(0).GetText()
    let paramList =
        match context.GetChild(1).ChildCount with
        | 1 -> []
        | _ -> 
            let paramList = context.GetChild(1).GetChild(1) :?> SBParser.ParenthesizedlistContext
            let (fList:IParseTree list) = copyAntlrList paramList.children  
            fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))            

    let functionAction = action TokenType.DefFunc
    functionAction funcName paramList state

let WalkImplicit (context : IParseTree) state action =
    let implic = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children  
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let implicitAction = action TokenType.Implic
    implicitAction implic termList state
    
let WalkLocal (context : IParseTree) state action =
    let locals = context.GetChild(0).GetText()
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children  
    let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let localAction = action TokenType.Local
    localAction locals termList state
    
let WalkLocals ((context : IParseTree), state) index =
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedContext
    // let paramList2 = ParseUnparenthesizedContext paramList
    let (key:Key) = {Name = "varName"; Scope = "function"}
    let (symbol:Symbol) = {Name = "d"; Scope = "function"; Category=Variable; Type=TokenType.GreaterEqual;  ParameterMechanism = Inapplicable}
    let newSymbolTable = state.symTab.Add(key, symbol)
    let newState = {state with symTab = newSymbolTable}
    newState

let rec WalkAcross (context : IParseTree) state index action=
    let result = 
        let count = context.ChildCount
        match index with
        | n when n < count ->
            let downValue = WalkDown (context.GetChild(index) : IParseTree) state action
            WalkAcross (((fst downValue): IParseTree).Parent : IParseTree) (snd downValue) (index+1) action
        | _ -> state
    result
and 
    WalkDown (context : IParseTree) (state: State) action =
        let newState = 
            match context with
                | :? SBParser.DimContext -> WalkDim context state action
                | :? SBParser.AssignmentContext -> WalkAssignment context state action
                | :? SBParser.ImplicitContext -> WalkImplicit context state action
                | :? SBParser.LocContext -> WalkLocal context state action
                | :? SBParser.ProchdrContext -> WalkProcedure context state action
                | :? SBParser.EnddefContext -> { state with currentScope = "~Global" }
                | :? SBParser.FunchdrContext -> WalkFunction context state action
                | _ -> state
        (context, WalkAcross (context:IParseTree) newState 0 action)

// top level only
let WalkTreeRoot context action =
    let state = { implicitInts = Set.empty; implicitStrings = Set.empty; references = Set.empty; symTab = Map.empty; errorList = []; currentScope = "~Global" }
    let h =  WalkDown (context : ParserRuleContext) state action
    Console.WriteLine(h.ToString())
 