module CodeGenerator

open Antlr4.Runtime.Tree
open SymbolTable
open Utility

let rec private foldStateful f (initialValue: 'a) (name: string) (li: 'b list) (state: State) : 'a =
    match li with
        | [] -> initialValue
        | x::xs -> foldStateful f (f initialValue name x state) name xs state

// if there is not output for this construct just preserve the state
let private defaultAction _ _ state = state

// extract text from a parameter list
let private getParameterText accum (scope: string) (parameters: IParseTree) state =
    let dataType = 
        match SymbolTable.get (parameters.GetText()) scope state with
        | None -> TokenType.Void.ToString()
        | Some n when n.Type = TokenType.Unknowntype -> "float"
        | Some n when n.Type = TokenType.Integer -> "int"
        | Some n -> n.Type.ToString()
    $@"{dataType} {parameters.GetText()}, {accum}"

// output code for procedures an functions
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
    let x = state.outputProcFn @ [text]
    {state with outputProcFn = x}

let private genEndDefine _ _ state =
    let x = state.outputProcFn @ ["}\r\n"]
    {state with outputProcFn = x}

let private genAssign _ _ state =
    outputCs "}\r\n" state

// returns function for handling given type
let Generate (actionType:TokenType) =
    match actionType with
    | TokenType.DefProc -> genProcFunc
    | TokenType.DefFunc -> genProcFunc
    | TokenType.EndDef -> genEndDefine
    | TokenType.ID -> genAssign
    | _ -> defaultAction

