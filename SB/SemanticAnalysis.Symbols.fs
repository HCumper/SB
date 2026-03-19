module SemanticAnalysisSymbols

open Types
open ProcessingTypes
open ScopeNames
open BuiltIns
open SymbolTableManager
open SemanticAnalysisFacts

// SemanticAnalysisSymbols owns the scope and symbol-table side of semantic analysis.
//
// This module answers questions like:
// - what symbol should a name resolve to in the current scope chain?
// - where should a new declaration be inserted?
// - what type is implied by suffixes or IMPLICIT rules?
// - how should reference/call facts be recorded once a symbol is resolved?
//
// The orchestration layer walks the AST; this module provides the symbol-centric
// operations that make those walks semantically meaningful.
let withDefaultRule (state: ProcessingState) : ImplicitTypingRule =
    match Map.tryFind globalScope state.ImplicitTyping with
    | Some rule -> rule
    | None -> { Integers = Set.empty; Strings = Set.empty }

let updateImplicitTyping (decorator: string) (names: Set<string>) (state: ProcessingState) =
    // IMPLICIT rules are normalized up front so later lookups stay case-insensitive
    // and can work against the same normalized symbol keys as the symbol table.
    let normalizedNames = names |> Set.map normalizeIdentifier
    let currentRule = withDefaultRule state
    let updatedRule =
        if decorator.Contains "%" then
            { currentRule with Integers = Set.union currentRule.Integers normalizedNames }
        elif decorator.Contains "$" then
            { currentRule with Strings = Set.union currentRule.Strings normalizedNames }
        else
            currentRule

    { state with ImplicitTyping = Map.add globalScope updatedRule state.ImplicitTyping }

let createCommonSymbol name position evaluatedType =
    { Name = name
      EvaluatedType = evaluatedType
      Position = position }

let createVariableSymbol name position evaluatedType =
    VariableSym { Common = createCommonSymbol name position evaluatedType; ValueText = None }

let createParameterSymbol name position evaluatedType =
    ParameterSym { Common = createCommonSymbol name position evaluatedType; Passing = None; ValueText = None }

let createArraySymbol name position elementType dimensions =
    ArraySym {
        Common = createCommonSymbol name position elementType
        ElementType = elementType
        Dimensions = dimensions
    }

let createFunctionSymbol name position returnType =
    FunctionSym {
        Common = createCommonSymbol name position returnType
        Parameters = []
        ReturnType = returnType
    }

let createProcedureSymbol name position =
    ProcedureSym {
        Common = createCommonSymbol name position SBType.Void
        Parameters = []
    }

let ensureScope scopeName parentScope table =
    if Map.containsKey scopeName table then table
    else table |> Map.add scopeName { Id = scopeName; Parent = parentScope; Symbols = Map.empty }

let updateRoutineParameters (scopeName: string) (parameters: ParameterSymbol list) (table: SymbolTable) =
    // Routine symbols live in the global scope, but their parameter lists are only
    // known after the routine body header has been processed. This helper patches
    // the existing routine symbol once parameter symbols have been created.
    match Map.tryFind globalScope table with
    | None -> table
    | Some globalScopeSymbols ->
        let key = normalizeIdentifier scopeName
        match Map.tryFind key globalScopeSymbols.Symbols with
        | None -> table
        | Some symbol ->
            let updatedSymbol =
                match symbol with
                | FunctionSym symbol -> FunctionSym { symbol with Parameters = parameters }
                | ProcedureSym symbol -> ProcedureSym { symbol with Parameters = parameters }
                | symbol -> symbol
            let updatedScope =
                { globalScopeSymbols with Symbols = Map.add key updatedSymbol globalScopeSymbols.Symbols }
            Map.add globalScope updatedScope table

let prePopulateSymbolTable (oldState: ProcessingState) : ProcessingState =
    { oldState with SymTab = ensureScope globalScope None oldState.SymTab }

let suffixDeclaredType (name: string) =
    if name.Contains "$" then SBType.String
    elif name.Contains "%" then SBType.Integer
    else SBType.Unknown

let inferredVariableType (state: ProcessingState) scopeName name =
    let currentRule = withDefaultRule state
    let normalizedName = normalizeIdentifier name
    // SuperBASIC typing is a blend of explicit suffixes and implicit rules. Suffixes
    // win first, then IMPLICIT declarations, otherwise the type remains unknown until
    // later inference refines it.
    match suffixDeclaredType name with
    | SBType.Unknown ->
        if currentRule.Integers.Contains normalizedName then SBType.Integer
        elif currentRule.Strings.Contains normalizedName then SBType.String
        else SBType.Unknown
    | declaredType -> declaredType

let rec tryResolveSymbol scopeName symbolName (table: SymbolTable) =
    // Resolution walks lexical scopes outward and only falls back to built-ins once
    // user-defined scopes are exhausted. That preserves the language rule that user
    // symbols shadow built-in names when the source allows it.
    match Map.tryFind scopeName table with
    | None -> None
    | Some scope ->
        match Map.tryFind (normalizeIdentifier symbolName) scope.Symbols with
        | Some symbol -> Some(scopeName, symbol)
        | None ->
            match scope.Parent with
            | Some parent -> tryResolveSymbol parent symbolName table
            | None ->
                match BuiltIns.tryFind symbolName with
                | Some symbol -> Some(globalScope, symbol)
                | None -> None

let tryResolveLocalSymbol scopeName symbolName (table: SymbolTable) =
    match Map.tryFind scopeName table with
    | None -> None
    | Some scope -> Map.tryFind (normalizeIdentifier symbolName) scope.Symbols

let isRoutineScope scopeName (table: SymbolTable) =
    match Map.tryFind scopeName table with
    | Some scope when scopeName <> globalScope && scope.Parent = Some globalScope -> true
    | _ -> false

let declarationScopeForDim currentScope name (table: SymbolTable) =
    // DIM and writable declarations inside routines are slightly subtle: they stay
    // local only when a compatible local symbol already exists, otherwise they are
    // treated as affecting the global scope, matching the current project semantics.
    if currentScope = globalScope then
        globalScope
    elif isRoutineScope currentScope table then
        match tryResolveLocalSymbol currentScope name table with
        | Some(ParameterSym _)
        | Some(VariableSym _)
        | Some(ArraySym _) -> currentScope
        | _ -> globalScope
    else
        currentScope

let declarationScopeForWritable currentScope name (table: SymbolTable) =
    if currentScope = globalScope then
        globalScope
    elif isRoutineScope currentScope table then
        match tryResolveLocalSymbol currentScope name table with
        | Some(ParameterSym _)
        | Some(VariableSym _)
        | Some(ArraySym _) -> currentScope
        | _ -> globalScope
    else
        currentScope

let declareSymbol mode scopeName symbol state =
    let symTab = addSymbolToNamedScope mode symbol scopeName state.SymTab
    { state with SymTab = symTab }

let declareVariable mode scopeName name position state =
    // Declaration helpers centralize both symbol creation and declaration-site fact
    // recording so callers do not need to remember to do both.
    let inferredType = inferredVariableType state scopeName name
    let symbol = createVariableSymbol name position inferredType
    state
    |> declareSymbol mode scopeName symbol
    |> recordFact DeclarationSite (Some SymbolCategory.Variable) name scopeName position (Some inferredType) None

let declareArray mode scopeName name position dimensions state =
    let inferredType = inferredVariableType state scopeName name
    let symbol = createArraySymbol name position inferredType dimensions
    state
    |> declareSymbol mode scopeName symbol
    |> recordFact DeclarationSite (Some SymbolCategory.Array) name scopeName position (Some inferredType) None

let declareProcedure mode name position state =
    state
    |> declareSymbol mode globalScope (createProcedureSymbol name position)
    |> recordFact DeclarationSite (Some SymbolCategory.Procedure) name globalScope position (Some SBType.Void) None

let declareFunction mode name position state =
    state
    |> declareSymbol mode globalScope (createFunctionSymbol name position (inferredVariableType state globalScope name))
    |> recordFact DeclarationSite (Some SymbolCategory.Function) name globalScope position (Some (inferredVariableType state globalScope name)) None

let declareParameter mode scopeName name position state =
    state
    |> declareSymbol mode scopeName (createParameterSymbol name position (inferredVariableType state scopeName name))
    |> recordFact DeclarationSite (Some SymbolCategory.Parameter) name scopeName position (Some (inferredVariableType state scopeName name)) None

let ensureVariableDeclaredInScope mode scopeName name position state =
    match tryResolveSymbol scopeName name state.SymTab with
    | Some _ -> state
    | None -> declareVariable mode scopeName name position state

let ensureWritableDeclared mode scopeName name position state =
    match tryResolveSymbol scopeName name state.SymTab with
    | Some _ -> state
    | None ->
        let targetScope = declarationScopeForWritable scopeName name state.SymTab
        declareVariable mode targetScope name position state

let referenceSymbol expectedCategory name position (state: ProcessingState) =
    // Reference recording always emits a fact, even for unresolved names, so tooling
    // and tests can observe attempted uses as well as successful resolutions.
    match tryResolveSymbol state.CurrentScope name state.SymTab with
    | Some(resolvedScope, symbol) ->
        let category = Symbol.category symbol
        let stateWithFact = recordFact ReferenceSite (Some category) name resolvedScope position (Some (Symbol.typ symbol)) None state
        match expectedCategory with
        | Some expected when expected <> category ->
            appendDiagnostic SemanticDiagnosticCode.CategoryMismatch (Some name) (Some position) $"Expected {expected} for '{name}' but found {category} at {position.EditorLineNo}:{position.Column}" stateWithFact
        | _ -> stateWithFact
    | None ->
        state
        |> recordFact ReferenceSite expectedCategory name state.CurrentScope position None None
        |> appendDiagnostic SemanticDiagnosticCode.UnresolvedReference (Some name) (Some position) $"Unresolved reference '{name}' at {position.EditorLineNo}:{position.Column}"

let recordCall name position (state: ProcessingState) =
    // Calls are resolved similarly to references, but they also validate that the
    // resolved symbol is actually callable in the language model.
    match tryResolveSymbol state.CurrentScope name state.SymTab with
    | Some(resolvedScope, symbol) ->
        let category = Symbol.category symbol
        let stateWithFact = recordFact CallSite (Some category) name resolvedScope position (Some (Symbol.typ symbol)) None state
        match category with
        | SymbolCategory.Procedure
        | SymbolCategory.Function
        | SymbolCategory.BuiltIn -> stateWithFact
        | _ -> appendDiagnostic SemanticDiagnosticCode.NotCallable (Some name) (Some position) $"Symbol '{name}' is not callable at {position.EditorLineNo}:{position.Column}" stateWithFact
    | None ->
        state
        |> recordFact CallSite None name state.CurrentScope position None None
        |> appendDiagnostic SemanticDiagnosticCode.UnresolvedCall (Some name) (Some position) $"Unresolved call '{name}' at {position.EditorLineNo}:{position.Column}"
