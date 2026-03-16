module SemanticAnalyzer

open Types
open ProcessingTypes
open ScopeNames
open BuiltIns
open SymbolTableManager
open Monads.State
open FSharpPlus.Data
open SyntaxAst

let private zeroPosition =
    { BasicLineNo = None
      EditorLineNo = 0
      Column = 0 }

let private withDefaultRule scopeName (state: ProcessingState) : ImplicitTypingRule =
    match Map.tryFind scopeName state.ImplicitTyping with
    | Some rule -> rule
    | None -> { Integers = Set.empty; Strings = Set.empty }

let private updateImplicitTyping scopeName (decorator: string) (names: Set<string>) (state: ProcessingState) =
    let normalizedNames = names |> Set.map normalizeIdentifier
    let currentRule = withDefaultRule scopeName state
    let updatedRule =
        if decorator.Contains "%" then
            { currentRule with Integers = Set.union currentRule.Integers normalizedNames }
        elif decorator.Contains "$" then
            { currentRule with Strings = Set.union currentRule.Strings normalizedNames }
        else
            currentRule

    { state with ImplicitTyping = Map.add scopeName updatedRule state.ImplicitTyping }

let private createCommonSymbol name position evaluatedType =
    { Name = name
      EvaluatedType = evaluatedType
      Position = position }

let private createVariableSymbol name position evaluatedType =
    VariableSym { Common = createCommonSymbol name position evaluatedType }

let private createParameterSymbol name position evaluatedType =
    ParameterSym { Common = createCommonSymbol name position evaluatedType; Passing = None }

let private createArraySymbol name position elementType dimensions =
    ArraySym {
        Common = createCommonSymbol name position elementType
        ElementType = elementType
        Dimensions = dimensions
    }

let private createFunctionSymbol name position =
    FunctionSym {
        Common = createCommonSymbol name position SBType.Unknown
        Parameters = []
        ReturnType = SBType.Unknown
    }

let private createProcedureSymbol name position =
    ProcedureSym {
        Common = createCommonSymbol name position SBType.Void
        Parameters = []
    }

let private addOrUpdateSymbol mode scopeName symbol table =
    addSymbolToNamedScope mode symbol scopeName table

let private ensureScope scopeName parentScope table =
    if Map.containsKey scopeName table then table
    else table |> Map.add scopeName { Id = scopeName; Parent = parentScope; Symbols = Map.empty }

let private updateRoutineParameters (scopeName: string) (parameters: ParameterSymbol list) (table: SymbolTable) =
    match Map.tryFind globalScope table with
    | None -> table
    | Some globalScopeSymbols ->
        let key = normalizeIdentifier scopeName
        match Map.tryFind key globalScopeSymbols.Symbols with
        | None -> table
        | Some (symbol: Symbol) ->
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

let private posOfExpr expr =
    match expr with
    | PostfixName(pos, _, _)
    | SliceRange(pos, _, _)
    | BinaryExpr(pos, _, _, _)
    | UnaryExpr(pos, _, _)
    | NumberLiteral(pos, _)
    | StringLiteral(pos, _)
    | Identifier(pos, _) -> pos

let private inferredVariableType (state: ProcessingState) scopeName name =
    let currentRule = withDefaultRule scopeName state
    let normalizedName = normalizeIdentifier name
    if currentRule.Integers.Contains normalizedName then SBType.Integer
    elif currentRule.Strings.Contains normalizedName then SBType.String
    else SBType.Unknown

let private recordFact kind category name scope position (state: ProcessingState) =
    let fact =
        { Name = name
          Scope = scope
          Position = position
          Category = category
          Kind = kind }
    { state with Facts = fact :: state.Facts }

let private appendError message (state: ProcessingState) =
    { state with Errors = state.Errors @ [ message ] }

let rec private tryResolveSymbol scopeName symbolName (table: SymbolTable) =
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

let private declareSymbol mode scopeName symbol state =
    let symTab = addOrUpdateSymbol mode scopeName symbol state.SymTab
    { state with SymTab = symTab }

let private declareVariable mode scopeName name position state =
    let inferredType = inferredVariableType state scopeName name
    let symbol = createVariableSymbol name position inferredType
    state
    |> declareSymbol mode scopeName symbol
    |> recordFact DeclarationSite (Some SymbolCategory.Variable) name scopeName position

let private declareArray mode scopeName name position dimensions state =
    let inferredType = inferredVariableType state scopeName name
    let symbol = createArraySymbol name position inferredType dimensions
    state
    |> declareSymbol mode scopeName symbol
    |> recordFact DeclarationSite (Some SymbolCategory.Array) name scopeName position

let private declareProcedure mode name position state =
    state
    |> declareSymbol mode globalScope (createProcedureSymbol name position)
    |> recordFact DeclarationSite (Some SymbolCategory.Procedure) name globalScope position

let private declareFunction mode name position state =
    state
    |> declareSymbol mode globalScope (createFunctionSymbol name position)
    |> recordFact DeclarationSite (Some SymbolCategory.Function) name globalScope position

let private declareParameter mode scopeName name position state =
    state
    |> declareSymbol mode scopeName (createParameterSymbol name position SBType.Unknown)
    |> recordFact DeclarationSite (Some SymbolCategory.Parameter) name scopeName position

let private ensureVariableDeclared mode scopeName name position state =
    match tryResolveSymbol scopeName name state.SymTab with
    | Some _ -> state
    | None -> declareVariable mode scopeName name position state

let private referenceSymbol expectedCategory name position (state: ProcessingState) =
    match tryResolveSymbol state.CurrentScope name state.SymTab with
    | Some(resolvedScope, symbol) ->
        let category = Symbol.category symbol
        let stateWithFact = recordFact ReferenceSite (Some category) name resolvedScope position state
        match expectedCategory with
        | Some expected when expected <> category ->
            appendError $"Expected {expected} for '{name}' but found {category} at {position.EditorLineNo}:{position.Column}" stateWithFact
        | _ -> stateWithFact
    | None ->
        state
        |> recordFact ReferenceSite expectedCategory name state.CurrentScope position
        |> appendError $"Unresolved reference '{name}' at {position.EditorLineNo}:{position.Column}"

let private recordCall name position (state: ProcessingState) =
    match tryResolveSymbol state.CurrentScope name state.SymTab with
    | Some(resolvedScope, symbol) ->
        let category = Symbol.category symbol
        let stateWithFact = recordFact CallSite (Some category) name resolvedScope position state
        match category with
        | SymbolCategory.Procedure
        | SymbolCategory.Function
        | SymbolCategory.BuiltIn -> stateWithFact
        | _ -> appendError $"Symbol '{name}' is not callable at {position.EditorLineNo}:{position.Column}" stateWithFact
    | None ->
        state
        |> recordFact CallSite None name state.CurrentScope position
        |> appendError $"Unresolved call '{name}' at {position.EditorLineNo}:{position.Column}"

let private resolveWritableTarget expr (state: ProcessingState) =
    match expr with
    | Identifier(pos, name)
    | PostfixName(pos, name, None) ->
        referenceSymbol None name pos state, []
    | PostfixName(pos, name, Some args) ->
        referenceSymbol None name pos state, args
    | _ -> state, []

let rec private collectDeclarationStmtList mode stmts =
    state {
        match stmts with
        | [] -> return ()
        | head :: tail ->
            do! collectDeclarations mode head
            do! collectDeclarationStmtList mode tail
    }

and private collectDeclarationLineList mode lines =
    state {
        match lines with
        | [] -> return ()
        | head :: tail ->
            do! collectDeclarationLine mode head
            do! collectDeclarationLineList mode tail
    }

and private collectDeclarationBlock mode block =
    state {
        match block with
        | StatementBlock stmts -> do! collectDeclarationStmtList mode stmts
        | LineBlock lines -> do! collectDeclarationLineList mode lines
    }

and private collectWritableDeclarations mode expr =
    state {
        let! currentState = getState
        match expr with
        | Identifier(pos, name)
        | PostfixName(pos, name, None) ->
            do! putState (ensureVariableDeclared mode currentState.CurrentScope name pos currentState)
        | PostfixName(pos, name, Some _) ->
            do! putState (ensureVariableDeclared mode currentState.CurrentScope name pos currentState)
        | _ -> ()
    }

and private collectDeclarationWritableList mode exprs =
    state {
        match exprs with
        | [] -> return ()
        | head :: tail ->
            do! collectWritableDeclarations mode head
            do! collectDeclarationWritableList mode tail
    }

and private collectDeclarationLine mode (line: Line) : State<ProcessingState, unit> =
    state {
        let (Line(_, _, children)) = line
        do! collectDeclarationStmtList mode children
    }

and collectDeclarations (mode: SymbolAddMode) (node: Stmt) : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        match node with
        | ProcedureDef(pos, name, parameters, body) ->
            let stateWithProc =
                currentState
                |> declareProcedure mode name pos
                |> fun s -> { s with SymTab = ensureScope name (Some globalScope) s.SymTab; CurrentScope = name }
            do! putState stateWithProc
            let parameterSymbols : ParameterSymbol list =
                parameters
                |> List.map (fun parameter ->
                    match createParameterSymbol parameter pos SBType.Unknown with
                    | ParameterSym symbol -> symbol
                    | _ -> failwith "unreachable")
            let stateWithParameters =
                parameterSymbols
                |> List.fold (fun state parameter -> declareSymbol mode name (ParameterSym parameter) state) stateWithProc
            let scopedState =
                List.foldBack
                    (fun (parameter: ParameterSymbol) state ->
                        recordFact DeclarationSite (Some SymbolCategory.Parameter) parameter.Common.Name name pos state)
                    parameterSymbols
                    { stateWithParameters with SymTab = updateRoutineParameters name parameterSymbols stateWithParameters.SymTab }
            do! putState scopedState
            do! collectDeclarationLineList mode body
            let! stateAfterBody = getState
            do! putState { stateAfterBody with CurrentScope = currentState.CurrentScope }

        | FunctionDef(pos, name, parameters, body) ->
            let stateWithFunc =
                currentState
                |> declareFunction mode name pos
                |> fun s -> { s with SymTab = ensureScope name (Some globalScope) s.SymTab; CurrentScope = name }
            do! putState stateWithFunc
            let parameterSymbols : ParameterSymbol list =
                parameters
                |> List.map (fun parameter ->
                    match createParameterSymbol parameter pos SBType.Unknown with
                    | ParameterSym symbol -> symbol
                    | _ -> failwith "unreachable")
            let stateWithParameters =
                parameterSymbols
                |> List.fold (fun state parameter -> declareSymbol mode name (ParameterSym parameter) state) stateWithFunc
            let scopedState =
                List.foldBack
                    (fun (parameter: ParameterSymbol) state ->
                        recordFact DeclarationSite (Some SymbolCategory.Parameter) parameter.Common.Name name pos state)
                    parameterSymbols
                    { stateWithParameters with SymTab = updateRoutineParameters name parameterSymbols stateWithParameters.SymTab }
            do! putState scopedState
            do! collectDeclarationLineList mode body
            let! stateAfterBody = getState
            do! putState { stateAfterBody with CurrentScope = currentState.CurrentScope }

        | DimStmt(pos, items) ->
            let nextState =
                items
                |> List.fold (fun state (name, dims) ->
                    let sizes =
                        dims
                        |> List.choose (function
                            | NumberLiteral(_, value) ->
                                match System.Int32.TryParse value with
                                | true, n -> Some n
                                | _ -> None
                            | _ -> None)
                    declareArray mode state.CurrentScope name pos sizes state) currentState
            do! putState nextState

        | LocalStmt(pos, items) ->
            let nextState =
                items
                |> List.fold (fun state (name, _) -> declareVariable mode state.CurrentScope name pos state) currentState
            do! putState nextState

        | ImplicitStmt(_, decorator, names) ->
            let stateWithImplicit = updateImplicitTyping currentState.CurrentScope decorator (Set.ofList names) currentState
            let nextState =
                names
                |> List.fold (fun state name -> declareVariable mode state.CurrentScope name zeroPosition state) stateWithImplicit
            do! putState nextState

        | ReferenceStmt _
        | DataStmt _ -> ()

        | ReadStmt(_, children) ->
            do! collectDeclarationWritableList mode children

        | Assignment(_, lhs, rhs) ->
            do! collectWritableDeclarations mode lhs

        | ProcedureCall _
        | ChannelProcedureCall _ -> ()

        | ForStmt(pos, name, _, _, _, body) ->
            let nextState = ensureVariableDeclared mode currentState.CurrentScope name pos currentState
            do! putState nextState
            do! collectDeclarationBlock mode body

        | RepeatStmt(_, _, body) ->
            do! collectDeclarationBlock mode body

        | IfStmt(_, _, thenBody, elseBody) ->
            do! collectDeclarationBlock mode thenBody
            match elseBody with
            | Some block -> do! collectDeclarationBlock mode block
            | None -> ()

        | SelectStmt(_, _, clauses) ->
            let rec collectClauses remaining =
                state {
                    match remaining with
                    | [] -> return ()
                    | SelectClause(_, _, _, body) :: tail ->
                        match body with
                        | Some block -> do! collectDeclarationBlock mode block
                        | None -> ()
                        do! collectClauses tail
                }
            do! collectClauses clauses

        | WhenStmt(_, _, body) ->
            do! collectDeclarationLineList mode body

        | ReturnStmt _
        | RestoreStmt _
        | ExitStmt _
        | NextStmt _
        | Remark _ -> ()
    }

let rec private resolveExpr mode expr : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        match expr with
        | PostfixName(pos, name, None) ->
            do! putState (referenceSymbol None name pos currentState)
        | PostfixName(pos, name, Some args) ->
            do! putState (referenceSymbol None name pos currentState)
            do! resolveExprList mode args
        | SliceRange(_, lhs, rhs)
        | BinaryExpr(_, _, lhs, rhs) ->
            do! resolveExpr mode lhs
            do! resolveExpr mode rhs
        | UnaryExpr(_, _, inner) ->
            do! resolveExpr mode inner
        | Identifier(pos, name) ->
            do! putState (referenceSymbol None name pos currentState)
        | NumberLiteral _
        | StringLiteral _ -> ()
    }

and private resolveExprList mode exprs =
    state {
        match exprs with
        | [] -> return ()
        | head :: tail ->
            do! resolveExpr mode head
            do! resolveExprList mode tail
    }

and private resolveLineList mode lines =
    state {
        match lines with
        | [] -> return ()
        | head :: tail ->
            do! resolveLine mode head
            do! resolveLineList mode tail
    }

and private resolveStmtList mode stmts =
    state {
        match stmts with
        | [] -> return ()
        | head :: tail ->
            do! resolveStmt mode head
            do! resolveStmtList mode tail
    }

and private resolveLine mode (line: Line) : State<ProcessingState, unit> =
    state {
        let (Line(_, _, children)) = line
        do! resolveStmtList mode children
    }

and private resolveBlock mode block : State<ProcessingState, unit> =
    state {
        match block with
        | StatementBlock stmts -> do! resolveStmtList mode stmts
        | LineBlock lines -> do! resolveLineList mode lines
    }

and private resolveWritableExpr mode expr : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        let nextState, args = resolveWritableTarget expr currentState
        do! putState nextState
        do! resolveExprList mode args
    }

and private resolveStmt (mode: SymbolAddMode) (node: Stmt) : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        match node with
        | ProcedureDef(_, name, _, body)
        | FunctionDef(_, name, _, body) ->
            do! putState { currentState with CurrentScope = name }
            do! resolveLineList mode body
            let! stateAfterBody = getState
            do! putState { stateAfterBody with CurrentScope = currentState.CurrentScope }

        | DimStmt(_, items) ->
            do! resolveExprList mode (items |> List.collect snd)

        | LocalStmt(_, items) ->
            do! resolveExprList mode (items |> List.collect (snd >> Option.defaultValue []))

        | ImplicitStmt _ -> ()

        | ReferenceStmt(_, children)
        | DataStmt(_, children) ->
            do! resolveExprList mode children

        | ReadStmt(_, children) ->
            let rec loop remaining =
                state {
                    match remaining with
                    | [] -> return ()
                    | head :: tail ->
                        do! resolveWritableExpr mode head
                        do! loop tail
                }
            do! loop children

        | Assignment(_, lhs, rhs) ->
            do! resolveWritableExpr mode lhs
            do! resolveExpr mode rhs

        | ProcedureCall(pos, name, args) ->
            do! putState (recordCall name pos currentState)
            do! resolveExprList mode args

        | ChannelProcedureCall(pos, name, channel, args) ->
            do! putState (recordCall name pos currentState)
            do! resolveExpr mode channel
            do! resolveExprList mode args

        | ForStmt(pos, name, startExpr, endExpr, stepExpr, body) ->
            do! putState (referenceSymbol None name pos currentState)
            do! resolveExpr mode startExpr
            do! resolveExpr mode endExpr
            match stepExpr with
            | Some expr -> do! resolveExpr mode expr
            | None -> ()
            do! resolveBlock mode body

        | RepeatStmt(_, _, body) ->
            do! resolveBlock mode body

        | IfStmt(_, cond, thenBody, elseBody) ->
            do! resolveExpr mode cond
            do! resolveBlock mode thenBody
            match elseBody with
            | Some block -> do! resolveBlock mode block
            | None -> ()

        | SelectStmt(_, selector, clauses) ->
            do! resolveExpr mode selector
            let rec resolveClauses remaining =
                state {
                    match remaining with
                    | [] -> return ()
                    | SelectClause(_, clauseSelector, rangeExpr, body) :: tail ->
                        do! resolveExpr mode clauseSelector
                        do! resolveExpr mode rangeExpr
                        match body with
                        | Some block -> do! resolveBlock mode block
                        | None -> ()
                        do! resolveClauses tail
                }
            do! resolveClauses clauses

        | WhenStmt(_, condition, body) ->
            match condition with
            | Some expr -> do! resolveExpr mode expr
            | None -> ()
            do! resolveLineList mode body

        | ReturnStmt(_, value)
        | RestoreStmt(_, value) ->
            match value with
            | Some expr -> do! resolveExpr mode expr
            | None -> ()

        | ExitStmt _
        | NextStmt _
        | Remark _ -> ()
    }

let addToTable (mode: SymbolAddMode) (node: Ast) (_incomingState: ProcessingState) : State<ProcessingState, unit> =
    state {
        let (Program(_, children)) = node
        do! collectDeclarationLineList mode children
        do! resolveLineList mode children
    }
