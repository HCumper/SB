module Utility

open Antlr4.Runtime.Tree
open SBLib
open SymbolTable

let rec mapIter (paramList:IParseTree list) (state: State) dataType category =
    match paramList with
    | [] -> state
    | head::tail ->
        let term = (head :?> SBParser.TermContext).children[0].GetText()
        let (symbol: Symbol) = {Name = term; Scope = state.currentScope; Category=category; Type=dataType; ParameterMechanism = Inapplicable}
        let newState = set symbol state;
        mapIter tail newState dataType category
