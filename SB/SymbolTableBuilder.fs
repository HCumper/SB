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
    let (varName, _) = ASTWalker.WalkDim context

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
        ASTWalker.WalkAssignment context

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
        ASTWalker.WalkImplicit context

    // let (_, dataType) =
    //     getTypeFromAnnotation implicDecl

    Utility.mapStringIter [names] state 0 CategoryType.Implicit

let private addLocalSymbol context state =
    let (_, paramList) =
        ASTWalker.WalkLocal context

    mapStringIterFromAnnotation (*paramList*) [ "dummy param list" ] state CategoryType.Local

let private addProcFuncSymbol context state =
    let (procName, paramList) =
        ASTWalker.WalkProcFunc context

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
        ASTWalker.WalkFor context

    let symbol =
        { Name = loopVar
          Scope = state.currentScope
          Category = CategoryType.Variable
          Type = SBParser.Real
          ParameterMechanism = Inapplicable }

    trySet symbol state

let private addShortForSymbol context state =
    let (loopVar, _, _, _) =
        ASTWalker.WalkFor context

    let symbol =
        { Name = loopVar
          Scope = state.currentScope
          Category = CategoryType.Variable
          Type = SBParser.Real
          ParameterMechanism = Inapplicable }

    trySet symbol state

///////////////////////////////// Tree traversal stuff ////////////////////////////////////////
let rec private WalkAcross (context: FSParseTree) index state =
    let result =
        let count = context.Data.Children.Length

        match index with
        | n when n < count ->
            let (context, state) =
                WalkDown (context.Data.Children[index]: FSParseTree) state

            WalkAcross (context: FSParseTree) (index + 1) state
        | _ -> state

    result

and private WalkDown (context: FSParseTree) (state: State) =
    let newState =
        match context.Kind with
        | DimContext -> addDimSymbol context state
        | AssignmentContext -> addAssignmentSymbol context state
        | ImplicitContext -> addImplicitSymbol context state
        | LocContext -> addLocalSymbol context state
        | ProchdrContext -> addProcFuncSymbol context state
        | FunchdrContext -> addProcFuncSymbol context state
        | EndDefContext -> addEndDefSymbol state
        | ForloopContext -> addLongForSymbol context state
        | _ -> state

    (context, WalkAcross (context: FSParseTree) 0 newState)

// top level only
let WalkTreeRoot (context: FSParseTree) (state: State) = WalkDown context state
