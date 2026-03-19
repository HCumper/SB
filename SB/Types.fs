module Types

open System

// Types is the shared domain contract for the compiler.
//
// It defines the cross-stage data structures used by parsing, semantic analysis,
// diagnostics, and later lowering/codegen work. Centralizing those shapes here
// keeps the rest of the codebase from duplicating basic compiler concepts.
//
// Shared compiler-domain types used across parsing, semantic analysis, and code generation.
type SourcePosition = {
    BasicLineNo: int option
    EditorLineNo: int
    Column: int
}

type NodeId = NodeId of int

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

let normalizeIdentifier (name: string) =
    name.Trim().ToUpperInvariant()

// Common symbol payload plus per-kind extensions for the symbol table.
type CommonSymbol = {
    Name: string
    EvaluatedType: SBType
    Position: SourcePosition
}

type VariableSymbol = {
    Common: CommonSymbol
    ValueText: string option
}

type ConstantSymbol = {
    Common: CommonSymbol
    ValueText: string option
}

type ParameterSymbol = {
    Common: CommonSymbol
    Passing: ParameterPassing option
    ValueText: string option
}

type ArraySymbol = {
    Common: CommonSymbol
    ElementType: SBType
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
    // These accessors keep the rest of the compiler from pattern-matching on the
    // Symbol DU just to retrieve common metadata.
    let common sym =
        match sym with
        | VariableSym s -> s.Common
        | ConstantSym s -> s.Common
        | ParameterSym s -> s.Common
        | ArraySym s -> s.Common
        | FunctionSym s -> s.Common
        | ProcedureSym s -> s.Common
        | BuiltInSym s -> s.Common

    let category sym =
        match sym with
        | VariableSym _ -> SymbolCategory.Variable
        | ConstantSym _ -> SymbolCategory.Constant
        | ParameterSym _ -> SymbolCategory.Parameter
        | ArraySym _ -> SymbolCategory.Array
        | FunctionSym _ -> SymbolCategory.Function
        | ProcedureSym _ -> SymbolCategory.Procedure
        | BuiltInSym _ -> SymbolCategory.BuiltIn

    let name sym = (common sym).Name
    let normalizedName sym = name sym |> normalizeIdentifier
    let typ sym = (common sym).EvaluatedType
    let position sym = (common sym).Position
    let valueText sym =
        match sym with
        | VariableSym s -> s.ValueText
        | ConstantSym s -> s.ValueText
        | ParameterSym s -> s.ValueText
        | _ -> None

type Scope = {
    Id: string
    Parent: string option
    Symbols: Map<string, Symbol>
}

type SymbolTable = Map<string, Scope>
// Symbol tables map scope ids to scope records; each scope stores normalized symbol names.

type ImplicitTypingRule = {
    Integers: Set<string>
    Strings: Set<string>
}

type SemanticFactKind =
    | DeclarationSite
    | ReferenceSite
    | CallSite
    | ExpressionResult

type DiagnosticSeverity =
    | Error
    | Warning

type SemanticDiagnosticCode =
    | Generic
    | CategoryMismatch
    | UnresolvedReference
    | UnresolvedCall
    | NotCallable
    | InvalidIndexing
    | NonWritableTarget
    | InvalidCallArity
    | InvalidBuiltInArgument
    | InvalidOperandCoercion
    | InvalidOperandTypes
    | InvalidExpressionContext
    | InvalidStatementCall
    | InvalidAssignment
    | InvalidControlFlowTarget
    | InvalidCondition
    | InvalidSelector
    | InvalidSliceBounds
    | InvalidForBounds
    | InvalidSelectClause
    | InvalidReturn
    | InvalidRestoreTarget
    | InvalidChannel
    | InvalidLoopControl

type SemanticFact = {
    Name: string
    Scope: string
    Position: SourcePosition
    Category: SymbolCategory option
    Kind: SemanticFactKind
    EvaluatedType: SBType option
    ValueText: string option
}

type SemanticDiagnostic = {
    Code: SemanticDiagnosticCode
    Severity: DiagnosticSeverity
    Message: string
    Position: SourcePosition option
    Scope: string
    SymbolName: string option
}
