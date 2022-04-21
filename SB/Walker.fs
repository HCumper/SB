module Walker

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable

// map with generic list to F# list
let rec mapAntlr f (parentNode: Collections.Generic.IList<IParseTree>) =
    match parentNode.Count with
    | 0 -> []
    | _ -> 
        // new head + new tail
        let head = f parentNode[0]
        let _ = parentNode.RemoveAt(0)
        head :: mapAntlr f parentNode

// copy generic list to F# list
let rec copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) =
    match parentNode.Count with
    | 0 -> []
    | _ -> 
        // new head + new tail
        let head = parentNode[0]
        let _ = parentNode.RemoveAt(0)
        head :: copyAntlrList parentNode

let dimSymbol state construct =
    state

let addItem inputList x =
    x

let walkParenthesizedList itemList resultList =
    itemList |> mapAntlr     
    addItem

let extractText (term : SBParser.TermContext) =
    let termCont = term.children[0].Payload
    match termCont with
    | :? SBParser.IdentifierContext -> 
        let identifier = termCont :?> SBParser.IdentifierContext
        let token = identifier.children[0].Payload :?> SBToken
        token.Text
    | :? SBToken -> 
        let token = termCont :?> SBToken
        token.Text
    | _ -> "error"

let WalkDim (context : IParseTree) state action =
    let varName = context.GetChild(1).Payload :?> SB.SBToken
    let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
    let (fList:IParseTree list) = copyAntlrList paramList.children  
 //   let termList = fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))
    let dimAction = action TokenType.Dimension
    dimAction varName.Text state

let WalkLocals ((context : IParseTree), state) index =
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedContext
    // let paramList2 = ParseUnparenthesizedContext paramList
    let (key:Key) = {Name = "varName"; Scope = "function"}
    let (symbol:Symbol) = {Name = "d"; Scope = "function"; Category=Variable; Type=TokenType.GreaterEqual;  ParameterMechanism = Inapplicable}
    let newSymbolTable = state.symTab.Add(key, symbol)
    let newState = {state with symTab = newSymbolTable}
    newState

//let rec WalkAcross ((context : IParseTree), state) index =
//    let result = 
//        if index < context.ChildCount then
//            let downValue = WalkDown ((context.GetChild(index) : IParseTree), state)
//            WalkAcross ((((fst downValue): IParseTree).Parent : IParseTree), (snd downValue)) (index+1)
//        else
//            (context, state)
//    result
//and 
//    WalkDown ((context : IParseTree), state) =
//        Console.WriteLine(context.ToString())
//        let newState = 
//            match context with
//                | :? SBParser.DimContext -> WalkDim (context, state) 0
//                | :? SBParser.LocContext -> WalkLocals (context, state) 0
//                | _ -> state
//        WalkAcross (context, newState) 0

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
        Console.WriteLine(context.ToString())
        let newState = 
            match context with
                | :? SBParser.DimContext -> WalkDim context state action
//                | :? SBParser.LocContext -> WalkLocals context state
                | _ -> state
        (context, WalkAcross (context:IParseTree) newState 0 action)

// top level only
let WalkTreeRoot context action =
    let state = { implicitInts = Set.empty; implicitStrings = Set.empty; references = Set.empty; symTab = Map.empty; errorList = []; currentScope = "~Global" }
    let h =  WalkDown (context : ParserRuleContext) state action
    Console.WriteLine(h.ToString())
 