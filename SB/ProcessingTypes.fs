module ProcessingTypes

open SyntaxAst
open Types

// ProcessingState is the shared compiler context threaded through semantic passes.
type ProcessingState = {
    Ast: Ast
    SymTab: SymbolTable
    CurrentScope: string
    InParameterList: bool
    ImplicitTyping: Map<string, ImplicitTypingRule>
    Facts: SemanticFact list
    ExpressionFacts: SemanticFact list
    Diagnostics: SemanticDiagnostic list
    Errors: string list
}
