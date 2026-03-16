module SymbolHelpers

open Types

let getSymbolName (symbol: Symbol) : string =
    match symbol with
    | VariableSym s -> s.Common.Name
    | ConstantSym s -> s.Common.Name
    | ParameterSym s -> s.Common.Name
    | ArraySym s -> s.Common.Name
    | FunctionSym s -> s.Common.Name
    | ProcedureSym s -> s.Common.Name
    | BuiltInSym s -> s.Common.Name
