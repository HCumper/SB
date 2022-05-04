﻿module Utility

open Antlr4.Runtime.Tree
open SBLib
open SymbolTable

let outputCs translation (state: State) =
    match state.currentScope with
    | "~Global" -> { state with outputProcFn = state.outputProcFn + translation}
    | _ -> { state with outputProcFn = state.outputGlobal + translation}

let getTypeFromAnnotation (name:string) =
    let len = name.Length - 1
    match name.Substring(len, 1) with
    | "%" -> SBParser.Integer
    | "$" -> SBParser.String
    | _ -> SBParser.Real
    