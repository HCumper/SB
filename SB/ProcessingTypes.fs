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
type ResolvedSymbolRef = {
    Scope: string
    Name: string
}

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
    ExprTypes: Map<NodeId, SBType>
    TargetTypes: Map<NodeId, SBType>
    ResolvedSymbols: Map<NodeId, ResolvedSymbolRef>
    RoutineSymbols: Map<string, ResolvedSymbolRef>
    ParameterSymbols: Map<string * string, ResolvedSymbolRef>
    ActiveLoops: string list
}

let exprPosition expr =
    match expr with
    | PostfixName(_, pos, _, _)
    | SliceRange(_, pos, _, _)
    | BinaryExpr(_, pos, _, _, _)
    | UnaryExpr(_, pos, _, _)
    | NumberLiteral(_, pos, _)
    | StringLiteral(_, pos, _)
    | Identifier(_, pos, _) -> pos
