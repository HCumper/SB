module ProcessingTypes

open SyntaxAst
open Types

// ProcessingTypes defines the threaded semantic-analysis state.
//
// Rather than keeping symbols, diagnostics, facts, and implicit typing in
// separate accumulators, the compiler carries a single ProcessingState through
// the state monad. That makes pass ordering explicit and keeps test inspection
// straightforward.
//
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
