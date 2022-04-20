module SymbolTableBuilder

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable

//There are 6 ways to create a new name
//    Dim
//    Local
//    Implicit
//    assignment to it
//    Define Procedure
//    Define Function
//    All but assignment take variable length parameter lists

let addDimSymbol varName paramList state =
    let key = {Name = varName; Scope = "~Global"}
    let symbol = {Name = key.Name; Scope = "~Global"; Category=CategoryType.Dim; Type=TokenType.Dimension; TypeString="Dim"; Extra=Empty}
    let newSymbolTable = state.symTab.Add(key, symbol)
    {state with symTab = newSymbolTable}
    
// returns function for handling given type
let buildSymbolTable (actionType:TokenType) =
    match actionType with
    | TokenType.Dimension -> addDimSymbol
    | _ -> addDimSymbol    

