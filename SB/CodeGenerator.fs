module CodeGenerator

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable
open Utility

let addToCSharp item (state: State) =
    match state.currentScope with
    | "~Global" -> {state with outputGlobal = state.outputGlobal + item}
    | _ -> {state with outputProcFn = state.outputProcFn + item}

let rec private foldStateful f (initialValue: 'a) (name: string) (li: 'b list) (state: State) : 'a =
    match li with
        | [] -> initialValue
        | x::xs -> foldStateful f (f initialValue name x state) name xs state

// extract text from a parameter list
let private getParameterText accum (scope: string) (parameters: IParseTree) state =
    let dataType = 
        match SymbolTable.get (parameters.GetText()) scope state with
        | None -> SBParser.Void.ToString()        //TokenType.Void.ToString()
        | Some n when n.Type = SBParser.Unknowntype -> "float"
        | Some n when n.Type = SBParser.Integer -> "int"
        | Some n -> n.Type.ToString()
    $@"{dataType} {parameters.GetText()}, {accum}"

// if there is not output for this construct just preserve the state
let private defaultAction _ _ state = state

// output code for procedures an functions
let private genDim context (state: State) =
    let (varName, dimensions) = Walker.WalkDim context
    let (name, _) = Utility.getTypeFromAnnotation varName
    let typeString =
        match SymbolTable.get name state.currentScope state with
            | None -> "void"
            | Some sym -> identifyType sym.Type
    let dimensionString = String.replicate dimensions.Tail.Length ","
    let dimStmt = newLine + typeString + " " + name + "[" + dimensionString + "];"
    addToCSharp dimStmt state

// output code for procedures an functions
let private genProcFunc (routineName:string) parameters state =
    let funcType = 
        match SymbolTable.get routineName "~Global" state with
        | Some t -> t.Type.ToString()
        | None -> SBParser.Void.ToString().ToLower()
    let paramList =
        match parameters with
        | [] -> ""
        | _ -> foldStateful getParameterText "" routineName parameters state

    let cSharp = 
        match paramList.Length with
        | 0 -> ""
        | _ -> "(" + paramList.Remove(paramList.Length - 2) + ")"
    let text = Templates.procFunc (funcType.ToLower()) routineName cSharp
    let x = state.outputProcFn + text
    {state with outputProcFn = x}

let private genEndDefine _ _ state =
    let x = state.outputProcFn + "}\r\n"
    {state with outputProcFn = x}

let private genAssign (varName:string) parameters (state: State) =
    let operatingScope =
        match state.currentScope with
        | "~Global" -> state.outputGlobal + $@"{varName} = "
        | _ -> state.outputProcFn + $@"{varName} = "
    {state with outputProcFn = operatingScope}

let private genBinary varName parameters (state: State) =
    let x = state.outputProcFn + "xxx"
    {state with outputProcFn = x}

let private genTerminal _ _ (state: State) =
    let operatingScope =
        match state.currentScope with
        | "~Global" -> state.outputGlobal + "}\r\n"
        | _ -> state.outputProcFn + "}\r\n"
    {state with outputProcFn = operatingScope}

let private noAction _ _ (state: State) =
    state

// returns function for handling given type
let Generate (actionType:int) =
    match actionType with
    | SBParser.DefProc -> genProcFunc
    | SBParser.DefFunc -> genProcFunc
    | SBParser.EndDef -> genEndDefine
    | SBParser.ID -> genAssign
    | SBParser.Integer | SBParser.Real | SBParser.String | SBParser.ID -> genTerminal
    | 1000 -> genBinary
    | _ -> noAction

///////////////////////////////// Tree traversal stuff ////////////////////////////////////////
let rec private WalkAcross (context : IParseTree) index state =
    let result = 
        let count = context.ChildCount
        match index with
        | n when n < count ->
            let (context, state) = WalkDown (context.GetChild(index) : IParseTree) state
            WalkAcross ((context: IParseTree).Parent : IParseTree) (index+1) state 
        | _ -> state
    result
and 
    private WalkDown (context : IParseTree) (state: State) =
        let newState = 
            match context with
            | :? SBParser.DimContext -> genDim context state 
            //| :? SBParser.AssignmentContext -> addAssignmentSymbol context state 
            //| :? SBParser.ImplicitContext -> addImplicitSymbol context state
            //| :? SBParser.LocContext -> addLocalSymbol context state
            //| :? SBParser.ProchdrContext -> addProcedureSymbol context state
            //| :? SBParser.FunchdrContext -> addFunctionSymbol context state
            //| :? SBParser.EndDefContext -> addEndDefSymbol state
            //| :? SBParser.LongforContext -> addLongForSymbol context state
            //| :? SBParser.ShortforContext -> addShortForSymbol context state
            | _ -> state

        (context, WalkAcross (context:IParseTree) 0 newState )

// top level only
let WalkTreeRoot (context: ParserRuleContext) (state: State) =
    WalkDown context state 
