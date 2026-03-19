module SymbolHelpers

open Types

// SymbolHelpers provides a tiny abstraction layer over the Symbol DU.
//
// It exists so lower-level symbol-table code can ask for common symbol metadata
// without depending directly on the union layout.
//
// Small wrappers so table-management code does not depend on Symbol union internals.
let getSymbolName (symbol: Symbol) : string =
    Symbol.name symbol

let getNormalizedSymbolName (symbol: Symbol) : string =
    Symbol.normalizedName symbol
