module Utility

open Antlr4.Runtime.Tree
open SBLib
open SymbolTable

let identifyType code =
    match code with
    | SBParser.String -> "string"
    | SBParser.Integer -> "int"
    | SBParser.Real -> "float"
    | _ -> "void"

let outputCs translation (state: State) =
    match state.currentScope with
    | globalScope -> { state with outputProcFn = state.outputProcFn + translation}
    | _ -> { state with outputProcFn = state.outputGlobal + translation}

let getTypeFromAnnotation (name:string) =
    let len = name.Length - 1
    match name.Substring(len, 1) with
    | "%" -> 
        let truncatedName = name.Substring(0, len) 
        (truncatedName, SBParser.Integer)
    | "$" -> 
        let truncatedName = name.Substring(0, len) 
        (truncatedName, SBParser.String)
    | _ -> 
        (name, SBParser.Real)
    