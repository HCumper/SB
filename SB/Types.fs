module Types

/// <summary>
/// Source position in the original program.
/// </summary>
type SourcePosition = {
    BasicLineNo: int option
    EditorLineNo: int
    Column: int
}

/// <summary>
/// Semantic types in Structured SuperBASIC.
/// These are language types, not symbol categories.
/// </summary>
type SBType =
    | String
    | Integer
    | Real
    | Void       // Used for procedures / statements with no value
    | Unknown    // Used during analysis when type is not yet resolved

/// <summary>
/// Semantic category of a symbol.
/// This describes what kind of program entity the name denotes.
/// </summary>
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

/// <summary>
/// Passing mode for parameters.
/// If the language does not always make this knowable at compile time,
/// the value can be optional in parameter symbols.
/// </summary>
type ParameterPassing =
    | ByValue
    | ByReference

/// <summary>
/// Types for AST nodes.
/// This remains a syntactic classification and should not be used
/// as the semantic kind of a symbol table entry.
/// </summary>
type NodeKind =
    | Any   // For pattern matching only
    | ArrayOrFunctionCall
    | Assignment
    | AssignmentTarget
    | BinaryExpr
    | Body
    | CallExpr
    | Data
    | Dim
    | Div
    | Divide
    | EndDef
    | EndFor
    | EndIf
    | EndRepeat
    | ExitStmt
    | Expression
    | For
    | Funchdr
    | Function
    | FunctionCall
    | FunctionDefinition
    | Gosub
    | Goto
    | ID
    | Identifier
    | IdentifierOnly
    | If
    | Implicit
    | Line
    | LineNumber
    | Local
    | Minus
    | Mod
    | Multiply
    | NextStmt
    | Nothing
    | NumberLiteral
    | OnClause
    | OnGosub
    | OnGoto
    | Operator
    | Parameters
    | ParenthesizedList
    | Plus
    | Primary
    | Procedure
    | ProcedureCall
    | ProcedureDefinition
    | Prochdr
    | ProcName
    | Program
    | Read
    | Reference
    | Remark
    | Repeat
    | Restore
    | Return
    | Select
    | Separator
    | Stmt
    | StmtList
    | StringLiteral
    | Term      // should not need all 3
    | TerminalNodeImpl
    | Terminator
    | TypedIdentifier
    | UnaryExpr
    | Unknown
    | UnparenthesizedList
    | Value
    | When
    | WhenError

/// <summary>
/// Represents a node in the abstract syntax tree.
/// This is a generic tree representation. Later passes may translate
/// it into a more strongly typed AST if desired.
/// </summary>
type ASTNode = {
    Kind: NodeKind
    Value: string
    Position: SourcePosition
    Children: ASTNode list
}

/// <summary>
/// Common fields shared by all symbols.
/// </summary>
type CommonSymbol = {
    Name: string
    Category: SymbolCategory
    EvaluatedType: SBType
    Position: SourcePosition
}

/// <summary>
/// Variable symbol.
/// </summary>
type VariableSymbol = {
    Common: CommonSymbol
}

/// <summary>
/// Constant symbol.
/// ValueText stores the textual constant form if known at compile time.
/// </summary>
type ConstantSymbol = {
    Common: CommonSymbol
    ValueText: string option
}

/// <summary>
/// Parameter symbol.
/// Passing mode may be unknown at compile time in some cases.
/// </summary>
type ParameterSymbol = {
    Common: CommonSymbol
    Passing: ParameterPassing option
}

/// <summary>
/// Array symbol.
/// Dimensions are compile-time extents if known.
/// </summary>
type ArraySymbol = {
    Common: CommonSymbol
    Dimensions: int list
}

/// <summary>
/// Function symbol.
/// </summary>
type FunctionSymbol = {
    Common: CommonSymbol
    Parameters: ParameterSymbol list
    ReturnType: SBType
}

/// <summary>
/// Procedure symbol.
/// Procedures do not return a value.
/// </summary>
type ProcedureSymbol = {
    Common: CommonSymbol
    Parameters: ParameterSymbol list
}

/// <summary>
/// Keyword or built-in symbol.
/// This can be useful when pre-populating the global scope.
/// </summary>
type BuiltInSymbol = {
    Common: CommonSymbol
}

/// <summary>
/// Discriminated union covering all symbol kinds.
/// </summary>
type Symbol =
    | VariableSym of VariableSymbol
    | ConstantSym of ConstantSymbol
    | ParameterSym of ParameterSymbol
    | ArraySym of ArraySymbol
    | FunctionSym of FunctionSymbol
    | ProcedureSym of ProcedureSymbol
    | BuiltInSym of BuiltInSymbol

/// <summary>
/// Helper functions for working with symbols.
/// </summary>
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

/// <summary>
/// A scope contains a name, an optional parent scope, and an immutable map of symbols.
/// Even if Structured SuperBASIC currently uses only a root plus one level of procedure/function
/// scopes, the parent link makes the design safer and easier to extend.
/// </summary>
type Scope = {
    Name: string
    Parent: string option  // The Global scope only has no parent
    Symbols: Map<string, Symbol>
}

/// <summary>
/// A symbol table is a map from scope name to scope.
/// The global scope is normally stored under a fixed name such as "global".
/// </summary>
type SymbolTable = Map<string, Scope>

/// <summary>
/// Records an implicit typing rule introduced by IMPLICIT declarations.
/// </summary>
type ImplicitTypingRule = {
    Integers: Set<string>
    Strings: Set<string>
}

/// <summary>
/// Semantic processing state threaded through the state monad.
/// This state is focused on AST and symbol analysis.
/// </summary>
type ProcessingState = {
    Ast: ASTNode
    SymTab: SymbolTable
    CurrentScope: string
    InParameterList: bool
    ImplicitTyping: Map<string, ImplicitTypingRule>   // keyed by scope name
    Errors: string list
    }

