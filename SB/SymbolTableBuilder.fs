module SymbolTableBuilder

open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable
open Utility
//There are 6 ways to create a new name
//    Dim
//    Local
//    Implicit
//    assignment to it
//    Define Procedure
//    Define Function
//    All but assignment take variable length parameter lists

let private defaultAction _ _ state = state

let private getTypeFromAnnotation (name:string) =
    let len = name.Length - 1
    match name.Substring(len, 1) with
    | "%" -> TokenType.Integer
    | "$" -> TokenType.String
    | _ -> TokenType.Real
    
let private addDimSymbol varName _ state =
    let dataType = getTypeFromAnnotation varName
    let symbol = {Name = varName; Scope = state.currentScope; Category=CategoryType.Dim; Type=dataType; ParameterMechanism = Inapplicable}
    set symbol state
    
let private addImplicitSymbol implicitDecl paramList state =
    let dataType = getTypeFromAnnotation implicitDecl
    mapIter paramList state dataType CategoryType.Implicit

let private addLocalSymbol localDecl paramList state =
    mapIter paramList state TokenType.Unknowntype CategoryType.Local

let private addProcedureSymbol procName paramList state =
    let symbol = {Name = procName; Scope = state.currentScope; Category=CategoryType.Procedure; Type=TokenType.Void; ParameterMechanism = Inapplicable}
    let newContext = { state with currentScope = procName }
    let newState = set symbol newContext
    mapIter paramList newState TokenType.Unknowntype CategoryType.Parameter

let private addFunctionSymbol funcName paramList state =
    let symbol = {Name = funcName; Scope = state.currentScope; Category=CategoryType.Function; Type=TokenType.Unknowntype; ParameterMechanism = Inapplicable}
    let newContext = { state with currentScope = funcName }
    let newState = set symbol newContext
    mapIter paramList newState TokenType.Unknowntype CategoryType.Parameter

let private addAssignmentSymbol (varName:string) _ state =
    let dataType = getTypeFromAnnotation varName
    let symbol = {Name = varName; Scope = state.currentScope; Category=CategoryType.Variable; Type=dataType;  ParameterMechanism = Inapplicable}
    trySet symbol state

// No action here - maybe in another pass
let private nullAction _ _ state = state

// returns function for handling given type
let BuildSymbolTable (actionType:TokenType) =
    match actionType with
    | TokenType.Dimension -> addDimSymbol
    | TokenType.ID -> addAssignmentSymbol
    | TokenType.Implic -> addImplicitSymbol
    | TokenType.Local -> addLocalSymbol
    | TokenType.DefProc -> addProcedureSymbol
    | TokenType.DefFunc -> addFunctionSymbol
    | TokenType.EndDef -> nullAction
    | _ -> defaultAction

