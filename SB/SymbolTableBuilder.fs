module SymbolTableBuilder

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

let defaultAction _ _ state = state

let getTypeFromAnnotation (name:string) =
    let len = name.Length - 1
    match name.Substring(len, 1) with
    | "%" -> TokenType.Integer
    | "$" -> TokenType.String
    | _ -> TokenType.Real

let addDimSymbol varName _ state =
    let dataType = getTypeFromAnnotation varName
    let symbol = {Name = varName; Scope = state.currentScope; Category=CategoryType.Dim; Type=dataType; ParameterMechanism = Inapplicable}
    set symbol state
    
let rec mapIter (paramList:IParseTree list) state dataType=
    match paramList with
    | [] -> state
    | head::tail ->
        let term = (head :?> SBParser.TermContext).children[0].GetText()
        let symbol = {Name = term; Scope = state.currentScope; Category=CategoryType.Implicit; Type=dataType; ParameterMechanism = Inapplicable}
        let newState = set symbol state;
        mapIter tail newState dataType

let addImplicitSymbol implictDecl paramList state =
    let dataType = getTypeFromAnnotation implictDecl
    mapIter paramList state dataType

let addAssignmentSymbol (varName:string) _ state =
    let dataType = getTypeFromAnnotation varName
    let symbol = {Name = varName; Scope = state.currentScope; Category=CategoryType.Variable; Type=dataType;  ParameterMechanism = Inapplicable}
    trySet symbol state

// returns function for handling given type
let buildSymbolTable (actionType:TokenType) =
    match actionType with
    | TokenType.Dimension -> addDimSymbol
    | TokenType.ID -> addAssignmentSymbol
    | TokenType.Implic -> addImplicitSymbol
    | _ -> defaultAction

