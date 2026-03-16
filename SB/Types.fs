module Types

open SyntaxAst

type SourcePosition = {
    BasicLineNo: int option
    EditorLineNo: int
    Column: int
}

type SBType =
    | String
    | Integer
    | Real
    | Void
    | Unknown

type SymbolCategory =
    | Variable
    | Constant
    | Parameter
    | Array
    | Function
    | Procedure
    | TypeName
    | Keyword
    | BuiltIn

type ParameterPassing =
    | ByValue
    | ByReference

type CommonSymbol = {
    Name: string
    Category: SymbolCategory
    EvaluatedType: SBType
    Position: SourcePosition
}

type VariableSymbol = {
    Common: CommonSymbol
}

type ConstantSymbol = {
    Common: CommonSymbol
    ValueText: string option
}

type ParameterSymbol = {
    Common: CommonSymbol
    Passing: ParameterPassing option
}

type ArraySymbol = {
    Common: CommonSymbol
    Dimensions: int list
}

type FunctionSymbol = {
    Common: CommonSymbol
    Parameters: ParameterSymbol list
    ReturnType: SBType
}

type ProcedureSymbol = {
    Common: CommonSymbol
    Parameters: ParameterSymbol list
}

type BuiltInSymbol = {
    Common: CommonSymbol
}

type Symbol =
    | VariableSym of VariableSymbol
    | ConstantSym of ConstantSymbol
    | ParameterSym of ParameterSymbol
    | ArraySym of ArraySymbol
    | FunctionSym of FunctionSymbol
    | ProcedureSym of ProcedureSymbol
    | BuiltInSym of BuiltInSymbol

module Symbol =
    let common sym =
        match sym with
        | VariableSym s -> s.Common
        | ConstantSym s -> s.Common
        | ParameterSym s -> s.Common
        | ArraySym s -> s.Common
        | FunctionSym s -> s.Common
        | ProcedureSym s -> s.Common
        | BuiltInSym s -> s.Common

    let name sym = (common sym).Name
    let category sym = (common sym).Category
    let typ sym = (common sym).EvaluatedType
    let position sym = (common sym).Position

type Scope = {
    Name: string
    Parent: string option
    Symbols: Map<string, Symbol>
}

type SymbolTable = Map<string, Scope>

type ImplicitTypingRule = {
    Integers: Set<string>
    Strings: Set<string>
}

type ProcessingState = {
    Ast: Ast
    SymTab: SymbolTable
    CurrentScope: string
    InParameterList: bool
    ImplicitTyping: Map<string, ImplicitTypingRule>
    Errors: string list
}
