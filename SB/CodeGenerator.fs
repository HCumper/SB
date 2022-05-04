module CodeGenerator

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable

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

    