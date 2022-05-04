module SymbolTableBuilder

open SymbolTable
open Utility
open SB
open SBLib
open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System

//There are 6 ways to create a new name
//    Dim
//    Local
//    Implicit
//    assignment to it
//    Define Procedure
//    Define Function
//    All but assignment take variable length parameter lists

// map while propagating state forward between operations on each element
let rec mapIter (paramList:IParseTree list) (state: State) dataType category =
    match paramList with
    | [] -> state
    | head::tail ->
        let term = (head :?> SBParser.TermContext).children[0].GetText()
        let (symbol: Symbol) = {Name = term; Scope = state.currentScope; Category=category; Type=dataType; ParameterMechanism = Inapplicable}
        let newState = set symbol state;
        mapIter tail newState dataType category

let private default_ _ state = state

let private addDimSymbol context state =
    let (varName, _) = Walker.WalkDim context state
    let dataType = Utility.getTypeFromAnnotation varName
    let symbol = {Name = varName; Scope = state.currentScope; Category=CategoryType.Dim; Type=dataType; ParameterMechanism = Inapplicable}
    set symbol state
    
let private addAssignmentSymbol context state =
    let (varName, _) = Walker.WalkAssignment context state
    let dataType = Utility.getTypeFromAnnotation varName
    let symbol = {Name = varName; Scope = state.currentScope; Category=CategoryType.Variable; Type=dataType;  ParameterMechanism = Inapplicable}
    trySet symbol state

let private addImplicitSymbol context state =
    let (varName, _) = Walker.WalkDim context state
    let dataType = getTypeFromAnnotation varName
    let symbol = {Name = varName; Scope = state.currentScope; Category=CategoryType.Implicit; Type=dataType; ParameterMechanism = Inapplicable}
    set symbol state

let private addLocalSymbol context state =
    let (_, paramList) = Walker.WalkLocal context state
    mapIter paramList state SBParser.Unknowntype CategoryType.Local

let private addProcedureSymbol context state =
    let (procName, paramList) = Walker.WalkProcedure context state
    let symbol = {Name = procName; Scope = state.currentScope; Category=CategoryType.Procedure; Type=SBParser.Void; ParameterMechanism = Inapplicable}
    let newContext = { state with currentScope = procName }
    let newState = set symbol newContext
    mapIter paramList newState SBParser.Unknowntype CategoryType.Parameter

let private addFunctionSymbol context state =
    let (funcName, paramList) = Walker.WalkFunction context state
    let symbol = {Name = funcName; Scope = state.currentScope; Category=CategoryType.Function; Type=SBParser.Unknowntype; ParameterMechanism = Inapplicable}
    let newContext = { state with currentScope = funcName }
    let newState = set symbol newContext
    mapIter paramList newState SBParser.Unknowntype CategoryType.Parameter

let private addEndDefSymbol state =
    {state with currentScope = "~Global"}

let rec private WalkAcross (context : IParseTree) index state =
    let result = 
        let count = context.ChildCount
        match index with
        | n when n < count ->
            let downValue = WalkDown (context.GetChild(index) : IParseTree) state
            WalkAcross (((fst downValue): IParseTree).Parent : IParseTree) (index+1) (snd downValue) 
        | _ -> state
    result
and 
    private WalkDown (context : IParseTree) (state: State) =
        let newState = 
            match context with
            | :? SBParser.DimContext -> addDimSymbol context state 
            | :? SBParser.AssignmentContext -> addAssignmentSymbol context state 
            | :? SBParser.ImplicitContext -> addImplicitSymbol context state
            | :? SBParser.LocContext -> addLocalSymbol context state
            | :? SBParser.ProchdrContext -> addProcedureSymbol context state
            | :? SBParser.FunchdrContext -> addFunctionSymbol context state
            | :? SBParser.EndDefContext -> addEndDefSymbol state
            | _ -> state

        (context, WalkAcross (context:IParseTree) 0 newState )

// top level only
let WalkTreeRoot (context: ParserRuleContext) (state: State) =
    WalkDown context state 
