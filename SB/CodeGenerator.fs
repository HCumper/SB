module CodeGenerator

open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable
open Utility

let rec foldStateful f (initialValue: 'a)  (name: string) (li: 'b list) (state: State) : 'a =
    match li with
        | [] -> initialValue
        | x::xs -> foldStateful f (f initialValue name x state) name xs state

let private defaultAction _ _ state = state

let private getParameterText accum (scope: string) (parameters: IParseTree) state =
    let dataType = 
        match SymbolTable.get (parameters.GetText()) scope state with
        | None -> TokenType.Void.ToString()
        | Some n when n.Type = TokenType.Unknowntype -> "float"
        | Some n when n.Type = TokenType.Integer -> "int"
        | Some n -> n.Type.ToString()
    $@"{dataType} {parameters.GetText()}, {accum}"

let private genProcFunc (routineName:string) parameters state =
    let funcType = 
        match SymbolTable.get routineName "~Global" state with
        | Some t -> t.Type.ToString()
        | None -> TokenType.Void.ToString().ToLower()
    let paramList =
        match parameters with
        | [] -> ""
        | _ -> foldStateful getParameterText "" routineName parameters state

    let cSharp = 
        match paramList.Length with
        | 0 -> ""
        | _ -> "(" + paramList.Remove(paramList.Length - 2) + ")"
    let text = Templates.procFunc (funcType.ToLower()) routineName cSharp
    let x = state.outputProg @ [text]
    {state with outputProg = x}

let private genEndDefine _ _ state =
    let x = state.outputProg @ ["}\r\n"]
    {state with outputProg = x}

// returns function for handling given type
let Generate (actionType:TokenType) =
    match actionType with
    | TokenType.DefProc -> genProcFunc
    | TokenType.EndDef -> genEndDefine
    | _ -> defaultAction

