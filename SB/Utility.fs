module Utility

open Antlr4.Runtime.Tree
open SBLib
open SymbolTable

// map while propagating state forward between operations on each element
let rec mapIter (paramList:IParseTree list) (state: State) dataType category =
    match paramList with
    | [] -> state
    | head::tail ->
        let term = (head :?> SBParser.TermContext).children[0].GetText()
        let (symbol: Symbol) = {Name = term; Scope = state.currentScope; Category=category; Type=dataType; ParameterMechanism = Inapplicable}
        let newState = set symbol state;
        mapIter tail newState dataType category

let outputCs translation (state: State) =
    match state.currentScope with
    | "~Global" -> { state with outputProcFn = state.outputProcFn @ [translation]}
    | _ -> { state with outputProcFn = state.outputGlobal @ [translation]}
