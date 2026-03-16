module SymbolHelpers

open Types

let getSymbolName (symbol: Symbol) : string =
    Symbol.name symbol

let getNormalizedSymbolName (symbol: Symbol) : string =
    Symbol.normalizedName symbol
