module SymbolTableManager

open System
open Types
open SymbolHelpers

// SymbolTableManager owns the low-level immutable symbol-table operations.
//
// It handles scope creation, symbol insertion, lookup, and debug printing.
// Higher-level semantic policies such as name resolution rules and declaration
// behavior live in the semantic-analysis modules.
//
// Basic scope and symbol-table mutation helpers used by semantic analysis.
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
    symbolTable |> Map.add scopeName { Id = scopeName; Parent = None; Symbols = Map.empty }

/// Non-destructively fetch a scope from the symbol table.
/// Returns None if no scope exists.
let fetchScopeFromTable (name: string) (table: SymbolTable) : Scope option =
    Map.tryFind name table
   
/// Fetch a named symbol from the given scope.
let fetchSymbolFromScope (name: string) (scope: Scope) : Symbol option =
    Map.tryFind (normalizeIdentifier name) scope.Symbols

/// Update a given scope with a new symbol (or update an existing symbol)
/// and then rebuild the symbol table.
let addSymbolToNamedScope 
    (mode: SymbolAddMode) 
    (symbol: Symbol) 
    (scopeName: string) 
    (symbolTable: SymbolTable) : SymbolTable =
    // Overwrite vs Skip is centralized here so callers can choose declaration
    // semantics without repeating the map-update logic.
    let symbolName = getNormalizedSymbolName symbol
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
    // This is intentionally simple and debug-oriented rather than a stable format.
    symbolTable |> Map.iter (fun scopeName scope ->
        printfn $"Scope: %s{scopeName}"
        scope.Symbols |> Map.iter (fun symbolName symbol ->
            printfn $"  Symbol: %s{symbolName} -> %A{symbol}"
        )
    )
