module Walker

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB

type Key = { name : string; scope : string }

type Param = string * int
type Symbol = { 
    name : string;
    varType : int;
    scope : string;
    paramList : List<Param> Option
}

type State = {
    implicitInts : Set<string>;
    implicitStrings : Set<string>;
    references : Set<string>;
    symTab : Map<Key, Symbol>
}

type dim = {id: int; size:int}

let rec mapAntlr f children =
    let x =children[1]
    //match children with
    //| [] ->
    //    []
    //| head::tail ->
    //    // new head + new tail
    //    (f head) :: (mapAntlr f tail)
    children

let dimSymbol state construct =
    state

let addItem inputList x =
    x

let walkParenthesizedList itemList resultList =
    itemList |> mapAntlr addItem

let WalkDim (context : IParseTree) state =
    let varName = context.GetChild(1).Payload :?> SB.SBToken
    let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
    let jj = paramList.children
    let j = paramList.children[1] :?> SBParser.LiteralContext
    let ewr = j.children[0].Payload :?> SBToken
    let fsf = ewr.Text
//    let y = (paramList.children[1] :?> SBParser.LiteralContext).(children[0].Payload :?> SBToken).Text
 
    let key = {name = varName.Text; scope = "~Global"}
    let symbol = {name = key.name; scope = "~Global"; varType=60; paramList=None}
    let newSymbolTable = state.symTab.Add(key, symbol)
    let newState = {state with symTab = newSymbolTable}
    newState

let WalkLocals ((context : IParseTree), state) index =
    let paramList = context.GetChild(1).Payload :?> SBParser.UnparenthesizedContext
    // let paramList2 = ParseUnparenthesizedContext paramList
    let (key:Key) = {name = "varName"; scope = "function"}
    let (symbol:Symbol) = {name = "d"; scope = "function"; varType=60; paramList=None}
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

let rec WalkAcross (context : IParseTree) state index =
    let result = 
        let count = context.ChildCount
        match index with
        | n when n < count ->
            let downValue = WalkDown (context.GetChild(index) : IParseTree) state
            WalkAcross (((fst downValue): IParseTree).Parent : IParseTree) (snd downValue) (index+1)
        | _ -> state
    result
and 
    WalkDown (context : IParseTree) state =
        Console.WriteLine(context.ToString())
        let newState = 
            match context with
                | :? SBParser.DimContext -> WalkDim context state
//                | :? SBParser.LocContext -> WalkLocals context state
                | _ -> state
        (context, WalkAcross (context:IParseTree) newState 0)


// top level only
let WalkTreeRoot context =
    let state = { implicitInts = Set.empty; implicitStrings = Set.empty; references = Set.empty; symTab = Map.empty }

 
    let h =  WalkDown (context : ParserRuleContext) state
    Console.WriteLine(h.ToString())
  //  3