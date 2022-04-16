module TreeRewriter

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib

type baseClass(tt) =
    member this.tokenType = tt

type tree<'a> = Empty | Tree of tree<baseClass> * baseClass

type dimClass(tt, parName) =
    inherit baseClass(tt)
    member this.name = parName

let dimSymbol state construct =
    state

let rec WalkAcross (context : IParseTree) rewrittenTree index =
    match index with
    | n when n < context.ChildCount ->
        let downValue = WalkDown (context.GetChild(index) : IParseTree) rewrittenTree
        WalkAcross (((fst downValue): IParseTree).Parent : IParseTree) rewrittenTree (index+1)
    | _ -> rewrittenTree
and 
    WalkDown (context : IParseTree) rewrittenTree =
        Console.WriteLine(context.ToString())
        let newTree = 
            match context with
                | :? SBParser.DimContext -> WalkDim context rewrittenTree
//                | :? SBParser.LocContext -> WalkLocals context rewrittenTree
                | _ -> rewrittenTree
        (context, WalkAcross (context:IParseTree) newTree 0)
and
    WalkDim (context : IParseTree) rewrittenTree =
        let varName = context.GetChild(1).Payload :?> SB.SBToken
        let paramList = context.GetChild(2).Payload :?> SBParser.ParenthesizedlistContext
        let parsedList = walkParenthesizedList paramList [] 0
        let baseNode = baseClass(SymbolTable.TokenType.Local)
        let newNode = dimClass(SymbolTable.TokenType.Caret, "x")
    
        //let key = {name = varName.Text; scope = "~Global"}
        //let symbol = {name = key.name; scope = "~Global"; varType=60; paramList=None}
        //let newSymbolTable = state.symTab.Add(key, symbol)
        //let newState = {state with symTab = newSymbolTable}
        rewrittenTree
and
    walkParenthesizedList (parameters :SBParser.ParenthesizedlistContext) rewrittenParams index=
        match index with
        | n when n < parameters.ChildCount ->
            let downValue = WalkDown (parameters.GetChild(index) : IParseTree) rewrittenParams
            WalkAcross (((fst downValue): IParseTree).Parent : IParseTree) rewrittenParams (index+1)
        | _ -> rewrittenParams
    



// top level only/let rewriteTree context =     
let rewriteTree context = 
    let result = WalkDown (context : ParserRuleContext) Empty
    3
//    Console.WriteLine(rewrittenTree.ToString())
    