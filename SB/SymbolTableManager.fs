module SymbolTableManager

open System
open Types
open Utility

/// The mode used when adding a symbol.
type SymbolAddMode =
    | Overwrite
    | Skip

/// Get the name from a symbol.
let getNameFromSymbol (symbol: Symbol) : string =
    getSymbolName symbol

/// Create an empty symbol table.
let emptySymbolTable : SymbolTable = Map.empty

/// Push a new scope (with the given name) onto the symbol table.
let pushScopeToTable (scopeName: string) (symbolTable: SymbolTable) : SymbolTable =
    symbolTable |> Map.add scopeName { Name = scopeName; Parent = None; Symbols = Map.empty }

/// Non-destructively fetch a scope from the symbol table.
/// Returns None if no scope exists.
let fetchScopeFromTable (name: string) (table: SymbolTable) : Scope option =
    Map.tryFind name table
   
/// Fetch a named symbol from the given scope.
let fetchSymbolFromScope (name: string) (scope: Scope) : Symbol option =
    Map.tryFind name scope.Symbols

/// Update a given scope with a new symbol (or update an existing symbol)
/// and then rebuild the symbol table.
let addSymbolToNamedScope 
    (mode: SymbolAddMode) 
    (symbol: Symbol) 
    (scopeName: string) 
    (symbolTable: SymbolTable) : SymbolTable =
    
    let symbolName = getSymbolName symbol
    let scopeOption = Map.tryFind scopeName symbolTable
        
    match scopeOption with
    | Some scope ->
        let updatedSymbols =
            match mode with
            | Overwrite -> Map.add symbolName symbol scope.Symbols
            | Skip when Map.containsKey symbolName scope.Symbols -> scope.Symbols
            | Skip -> Map.add symbolName symbol scope.Symbols
        let newScope = { scope with Symbols = updatedSymbols }
        Map.add scopeName newScope symbolTable
    | None ->  
        // No scope found, return unchanged table.
        symbolTable
    
/// Pretty print the contents of the entire symbol table.
let printSymbolTable (symbolTable: SymbolTable) =
    symbolTable |> Map.iter (fun scopeName scope ->
        printfn $"Scope: %s{scopeName}"
        scope.Symbols |> Map.iter (fun symbolName symbol ->
            printfn $"  Symbol: %s{symbolName} -> %A{symbol}"
        )
    )
