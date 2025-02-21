module SymbolTableManager

open System
open Utility

/// The mode used when adding a symbol.
type SymbolAddMode =
    | Overwrite
    | Skip

/// A symbol table is a map of scopes, i.e. a map of maps.
/// Since SB does not have blocks, all functions and procedures are global,
/// so the symbol table has only a root and single-level branches.
/// The global scope is identified by the name stored in a constant.
type SymbolTable = Map<string, Scope<Symbol>>

/// Global scope constant.
let globalScope = "~Global"

/// Get the name from a symbol.
let getNameFromSymbol (symbol: Symbol) : string =
    match symbol with
    | Common s -> s.Name
    | Parameter s -> s.Common.Name
    | Array s -> s.Common.Name

/// Create an empty symbol table.
let emptySymbolTable : SymbolTable = Map.empty

/// Push a new scope (with the given name) onto the symbol table.
let pushScopeToTable (scopeName: string) (symbolTable: SymbolTable) : SymbolTable =
    symbolTable |> Map.add scopeName { Name = scopeName; Symbols = Map.empty }

/// Non-destructively fetch a scope from the symbol table.
/// Returns None if no scope exists.
let fetchScopeFromTable (name: string) (table: SymbolTable) : Scope<Symbol> option =
    Map.tryFind name table
   
/// Fetch a named symbol from the given scope.
let fetchSymbolFromScope (name: string) (scope: Scope<Symbol>) : Symbol option =
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
        printfn "Scope: %s" scopeName
        scope.Symbols |> Map.iter (fun symbolName symbol ->
            printfn "  Symbol: %s -> %A" symbolName symbol
        )
    )
