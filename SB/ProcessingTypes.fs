module ProcessingTypes

open SyntaxAst
open Types

type ProcessingState = {
    Ast: Ast
    SymTab: SymbolTable
    CurrentScope: string
    InParameterList: bool
    ImplicitTyping: Map<string, ImplicitTypingRule>
    Facts: SemanticFact list
    Errors: string list
}
