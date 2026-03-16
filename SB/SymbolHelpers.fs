module SymbolHelpers

open Types

// Small wrappers so table-management code does not depend on Symbol union internals.
let getSymbolName (symbol: Symbol) : string =
    Symbol.name symbol

let getNormalizedSymbolName (symbol: Symbol) : string =
    Symbol.normalizedName symbol
