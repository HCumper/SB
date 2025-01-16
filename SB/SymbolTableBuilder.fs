module SymbolTableBuilder

open SymbolTable
open Utility
open SB
open Antlr4.Runtime
open Antlr4.Runtime.Tree

//There are 7 ways to create a new name
//    Dim
//    Local
//    Implicit
//    assignment to it
//    use as control variable in for loop
//    Define Procedure
//    Define Function
//    All but assignment take variable length parameter lists

let rec mapStringIterFromAnnotation (paramList: string List) state category =
    match paramList with
    | [] -> state
    | head :: tail ->
        let (headText, headType) =
            getTypeFromAnnotation head

        let (symbol: Symbol) =
            { Name = headText
              Scope = state.currentScope
              Category = category
              Type = headType
              ParameterMechanism = Inapplicable }

        let newState = set symbol state
        mapStringIterFromAnnotation tail newState category

let private default_ _ state = state

let private addDimSymbol context state =
    let (varName, _) = Walker.WalkDim context

    let (name, dataType) =
        Utility.getTypeFromAnnotation varName

    let symbol =
        { Name = name
          Scope = state.currentScope
          Category = CategoryType.Dim
          Type = dataType
          ParameterMechanism = Inapplicable }

    set symbol state

let private addAssignmentSymbol context state =
    let (varName, _, _, _) =
        Walker.WalkAssignment context

    let (truncatedName, dataType) =
        Utility.getTypeFromAnnotation varName

    let symbol =
        { Name = truncatedName
          Scope = state.currentScope
          Category = CategoryType.Variable
          Type = dataType
          ParameterMechanism = Inapplicable }

    trySet symbol state

let private addImplicitSymbol context state =
    let (implicDecl, names) =
        Walker.WalkImplicit context

    let (_, dataType) =
        getTypeFromAnnotation implicDecl

    Utility.mapStringIter names state dataType CategoryType.Implicit

let private addLocalSymbol context state =
    let (_, paramList) =
        Walker.WalkLocal context

    mapStringIterFromAnnotation (*paramList*) [ "dummy param list" ] state CategoryType.Local

let private addProcFuncSymbol context state =
    let (procName, paramList) =
        Walker.WalkProcFunc context

    let symbol =
        { Name = procName
          Scope = state.currentScope
          Category = CategoryType.Procedure
          Type = SBParser.Void
          ParameterMechanism = Inapplicable }

    let newContext =
        { state with currentScope = procName }

    let newState = set symbol newContext
    state

let private addEndDefSymbol state =
    { state with currentScope = globalScope }

let private addLongForSymbol context state =
    let (loopVar, _, _, _) =
        Walker.WalkFor context

    let symbol =
        { Name = loopVar
          Scope = state.currentScope
          Category = CategoryType.Variable
          Type = SBParser.Real
          ParameterMechanism = Inapplicable }

    trySet symbol state

let private addShortForSymbol context state =
    let (loopVar, _, _, _) =
        Walker.WalkFor context

    let symbol =
        { Name = loopVar
          Scope = state.currentScope
          Category = CategoryType.Variable
          Type = SBParser.Real
          ParameterMechanism = Inapplicable }

    trySet symbol state

///////////////////////////////// Tree traversal stuff ////////////////////////////////////////
let rec private WalkAcross (context: IParseTree) index state =
    let result =
        let count = context.ChildCount

        match index with
        | n when n < count ->
            let (context, state) =
                WalkDown (context.GetChild (index): IParseTree) state

            WalkAcross ((context: IParseTree).Parent: IParseTree) (index + 1) state
        | _ -> state

    result

and private WalkDown (context: IParseTree) (state: State) =
    let newState =
        match context with
        | :? SBParser.DimContext -> addDimSymbol context state
        | :? SBParser.AssignmentContext -> addAssignmentSymbol context state
        | :? SBParser.ImplicitContext -> addImplicitSymbol context state
        | :? SBParser.LocContext -> addLocalSymbol context state
        | :? SBParser.ProchdrContext
        | :? SBParser.FunchdrContext -> addProcFuncSymbol context state
        | :? SBParser.EndDefContext -> addEndDefSymbol state
        | :? SBParser.ForloopContext -> addLongForSymbol context state
        | _ -> state

    (context, WalkAcross (context: IParseTree) 0 newState)

// top level only
let WalkTreeRoot (context: ParserRuleContext) (state: State) = WalkDown context state
