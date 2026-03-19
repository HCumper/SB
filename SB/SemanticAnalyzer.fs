module SemanticAnalyzer

open Types
open ProcessingTypes
open ScopeNames
open SymbolTableManager
open Monads.State
open FSharpPlus.Data
open SyntaxAst
open SemanticAnalysisFacts
open SemanticAnalysisSymbols
open SemanticAnalysisExpressions

// SemanticAnalyzer is the top-level semantic pass coordinator.
//
// It does not own the detailed rules for symbol resolution, expression typing,
// constant folding, or fact/diagnostic recording. Those live in the
// SemanticAnalysis.* helper modules. This file is responsible for walking the
// AST in semantic order and sequencing the major phases:
// 1. collect declarations so scopes and symbols exist before use sites
// 2. resolve references, calls, and writable targets
// 3. apply statement-level semantic checks and update inferred symbol facts
//
// In practice, this module is the bridge between the parsed AST and the richer
// semantic model that later stages such as IR lowering will consume.
let prePopulateSymbolTable = SemanticAnalysisSymbols.prePopulateSymbolTable

let private isInputStatement name =
    normalizeIdentifier name = "INPUT"

let private validateStatementCallShape name pos (state: ProcessingState) =
    match tryResolveSymbol state.CurrentScope name state.SymTab with
    | Some(_, FunctionSym _) ->
        appendDiagnostic SemanticDiagnosticCode.InvalidStatementCall (Some name) (Some pos) $"Function '{name}' cannot be used as a statement call at {pos.EditorLineNo}:{pos.Column}" state
    | _ -> state

let private normalizeLoopName name =
    if System.String.IsNullOrWhiteSpace name then ""
    else normalizeIdentifier name

let private pushLoop name (state: ProcessingState) =
    { state with ActiveLoops = normalizeLoopName name :: state.ActiveLoops }

let private popLoop (state: ProcessingState) =
    match state.ActiveLoops with
    | _ :: rest -> { state with ActiveLoops = rest }
    | [] -> state

let private validateLoopControl name pos (state: ProcessingState) =
    let normalized = normalizeLoopName name
    let loopExists =
        if normalized = "" then not state.ActiveLoops.IsEmpty
        else state.ActiveLoops |> List.exists ((=) normalized)

    if loopExists then
        state
    else
        appendDiagnostic SemanticDiagnosticCode.InvalidLoopControl (if normalized = "" then None else Some name) (Some pos) $"Loop control target '{name}' is not active at {pos.EditorLineNo}:{pos.Column}" state

let private recordExprType scope expr exprType (state: ProcessingState) =
    { state with ExprTypes = Map.add (nodeIdOfExpr expr) exprType state.ExprTypes }

let private recordTargetType scope expr targetType (state: ProcessingState) =
    { state with TargetTypes = Map.add (nodeIdOfExpr expr) targetType state.TargetTypes }

let private recordResolvedSymbol scope expr resolvedScope resolvedName (state: ProcessingState) =
    let resolved =
        { Scope = resolvedScope
          Name = normalizeIdentifier resolvedName }
    { state with ResolvedSymbols = Map.add (nodeIdOfExpr expr) resolved state.ResolvedSymbols }

let private recordRoutineSymbol name resolvedName (state: ProcessingState) =
    let resolved =
        { Scope = globalScope
          Name = normalizeIdentifier resolvedName }
    { state with RoutineSymbols = Map.add (normalizeIdentifier name) resolved state.RoutineSymbols }

let private recordParameterSymbol routineName parameterName resolvedName (state: ProcessingState) =
    let resolved =
        { Scope = routineName
          Name = normalizeIdentifier resolvedName }
    { state with ParameterSymbols = Map.add (normalizeIdentifier routineName, normalizeIdentifier parameterName) resolved state.ParameterSymbols }

let private inferArgumentTypes args state =
    (([], state), args)
    ||> List.fold (fun (collectedTypes, currentState) arg ->
        let argType, nextState = inferExprType currentState arg
        collectedTypes @ [ argType ], nextState)

let private inferExprList exprs state =
    inferArgumentTypes exprs state |> snd

let rec private stateIter action items =
    state {
        match items with
        | [] -> return ()
        | head :: tail ->
            do! action head
            do! stateIter action tail
    }

let rec private collectRoutineDeclaration mode declareRoutine pos name parameters body =
    state {
        let! currentState = getState
        let stateWithRoutine =
            currentState
            |> declareRoutine mode name pos
            |> fun s -> { s with SymTab = ensureScope name (Some globalScope) s.SymTab; CurrentScope = name }
        do! putState stateWithRoutine
        let parameterSymbols : ParameterSymbol list =
            parameters
            |> List.map (fun parameter ->
                match createParameterSymbol parameter pos (inferredVariableType currentState name parameter) with
                | ParameterSym symbol -> symbol
                | _ -> failwith "unreachable")
        let stateWithParameters =
            parameterSymbols
            |> List.fold (fun state parameter -> declareSymbol mode name (ParameterSym parameter) state) stateWithRoutine
        let scopedState =
            List.foldBack
                (fun (parameter: ParameterSymbol) state ->
                    recordFact DeclarationSite (Some SymbolCategory.Parameter) parameter.Common.Name name pos (Some parameter.Common.EvaluatedType) None state)
                parameterSymbols
                { stateWithParameters with SymTab = updateRoutineParameters name parameterSymbols stateWithParameters.SymTab }
        let recordedState =
            parameterSymbols
            |> List.fold (fun state parameter -> recordParameterSymbol name parameter.Common.Name parameter.Common.Name state) scopedState
            |> recordRoutineSymbol name name
        do! putState recordedState
        do! collectDeclarationLineList mode body
        let! stateAfterBody = getState
        do! putState { stateAfterBody with CurrentScope = currentState.CurrentScope }
    }

and private collectDeclarationStmtList mode stmts =
    stateIter (collectDeclarations mode) stmts

and private collectDeclarationLineList mode lines =
    stateIter (collectDeclarationLine mode) lines

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
        | Identifier(_, pos, name)
        | PostfixName(_, pos, name, None) ->
            do! putState (ensureWritableDeclared mode currentState.CurrentScope name pos currentState)
        | PostfixName(_, pos, name, Some _) ->
            do! putState (ensureWritableDeclared mode currentState.CurrentScope name pos currentState)
        | _ -> ()
    }

and private collectDeclarationWritableList mode exprs =
    stateIter (collectWritableDeclarations mode) exprs

and private collectDeclarationLine mode (line: Line) : State<ProcessingState, unit> =
    state {
        let (Line(_, _, children)) = line
        do! collectDeclarationStmtList mode children
    }

and collectDeclarations (mode: SymbolAddMode) (node: Stmt) : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        match node with
        // Routine declarations are handled in declaration-pass order:
        // create the routine symbol in the global scope, create or enter the
        // routine scope, seed parameter symbols, and only then walk the body.
        //
        // That ordering matters because later statements in the same routine may
        // refer to parameters or locals introduced earlier in the body, and the
        // resolution pass expects the enclosing scope structure to already exist.
        | ProcedureDef(pos, name, parameters, body) ->
            do! collectRoutineDeclaration mode declareProcedure pos name parameters body

        | FunctionDef(pos, name, parameters, body) ->
            do! collectRoutineDeclaration mode declareFunction pos name parameters body

        // Declaration-like statements only shape the symbol table in this pass.
        // They do not validate expression semantics yet; that is deferred until
        // the resolution pass so all declarations are available first.
        //
        // Array bounds are folded here where possible because dimensions belong
        // to the declaration itself and later passes benefit from concrete sizes.
        | DimStmt(pos, items) ->
            let nextState =
                items
                |> List.fold (fun state (name, dims) ->
                    let sizes =
                        dims
                        |> List.choose (tryEvaluateConstantIntegerExpr state)
                    let targetScope = declarationScopeForDim state.CurrentScope name state.SymTab
                    declareArray mode targetScope name pos sizes state) currentState
            do! putState nextState

        | LocalStmt(pos, items) ->
            let nextState =
                items
                |> List.fold (fun state (name, dims) ->
                    match dims with
                    | Some dimExprs ->
                        let sizes =
                            dimExprs
                            |> List.choose (tryEvaluateConstantIntegerExpr state)
                        declareArray mode state.CurrentScope name pos sizes state
                    | None -> declareVariable mode state.CurrentScope name pos state) currentState
            do! putState nextState

        | ImplicitStmt(_, decorator, names) ->
            do! putState (updateImplicitTyping decorator (Set.ofList names) currentState)

        | ReferenceStmt _
        | DataStmt _ -> ()

        | ReadStmt(_, children) ->
            do! collectDeclarationWritableList mode children

        | Assignment(_, lhs, _) ->
            do! collectWritableDeclarations mode lhs

        | GotoStmt _
        | GosubStmt _
        | OnGotoStmt _
        | OnGosubStmt _ -> ()

        | ProcedureCall(_, name, args)
        | ChannelProcedureCall(_, name, _, args) ->
            if isInputStatement name then
                do! collectDeclarationWritableList mode args

        | ForStmt(pos, name, _, _, _, body) ->
            let nextState = ensureVariableDeclaredInScope mode currentState.CurrentScope name pos currentState
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

let rec private resolveRoutineBody mode name body =
    state {
        let! currentState = getState
        do! putState { currentState with CurrentScope = name }
        do! resolveLineList mode body
        let! stateAfterBody = getState
        do! putState { stateAfterBody with CurrentScope = currentState.CurrentScope }
    }

and private resolveExpr mode expr : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        match expr with
        // Expression resolution is intentionally lighter-weight than expression
        // typing. This pass records reference and call facts, checks basic shape
        // constraints such as scalar-vs-array usage, and ensures nested
        // expressions are traversed in source order.
        //
        // Type inference, coercion, constant folding, and most operator rules are
        // deferred to inferExprType so those concerns stay centralized.
        | PostfixName(_, pos, name, None) ->
            let nextState = referenceSymbol None name pos currentState
            let validatedState =
                match tryResolveSymbol nextState.CurrentScope name nextState.SymTab with
                | Some(_, symbol) -> validateScalarVsArrayUsage name pos false symbol nextState
                | None -> nextState
            let recordedState =
                match tryResolveSymbol validatedState.CurrentScope name validatedState.SymTab with
                | Some(resolvedScope, symbol) ->
                    recordResolvedSymbol currentState.CurrentScope expr resolvedScope (Symbol.normalizedName symbol) validatedState
                | None -> validatedState
            do! putState recordedState
        | PostfixName(_, pos, name, Some args) ->
            let nextState = referenceSymbol None name pos currentState
            let validatedState =
                match tryResolveSymbol nextState.CurrentScope name nextState.SymTab with
                | Some(_, symbol) ->
                    match symbol with
                    | ArraySym _
                    | VariableSym _
                    | ConstantSym _
                    | ParameterSym _ -> validateScalarVsArrayUsage name pos true symbol nextState
                    | _ -> nextState
                | None -> nextState
            let recordedState =
                match tryResolveSymbol validatedState.CurrentScope name validatedState.SymTab with
                | Some(resolvedScope, symbol) ->
                    recordResolvedSymbol currentState.CurrentScope expr resolvedScope (Symbol.normalizedName symbol) validatedState
                | None -> validatedState
            do! putState recordedState
            do! resolveExprList mode args
        | SliceRange(_, _, lhs, rhs)
        | BinaryExpr(_, _, _, lhs, rhs) ->
            do! resolveExpr mode lhs
            do! resolveExpr mode rhs
        | UnaryExpr(_, _, _, inner) ->
            do! resolveExpr mode inner
        | Identifier(_, pos, name) ->
            let nextState = referenceSymbol None name pos currentState
            let validatedState =
                match tryResolveSymbol nextState.CurrentScope name nextState.SymTab with
                | Some(_, symbol) -> validateScalarVsArrayUsage name pos false symbol nextState
                | None -> nextState
            let recordedState =
                match tryResolveSymbol validatedState.CurrentScope name validatedState.SymTab with
                | Some(resolvedScope, symbol) ->
                    recordResolvedSymbol currentState.CurrentScope expr resolvedScope (Symbol.normalizedName symbol) validatedState
                | None -> validatedState
            do! putState recordedState
        | NumberLiteral _
        | StringLiteral _ -> ()
    }

and private resolveExprList mode exprs =
    stateIter (resolveExpr mode) exprs

and private resolveLineList mode lines =
    stateIter (resolveLine mode) lines

and private resolveStmtList mode stmts =
    stateIter (resolveStmt mode) stmts

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
        let targetType = inferWritableTargetType nextState expr
        let recordedState =
            let withTargetType = recordTargetType currentState.CurrentScope expr targetType nextState
            match expr with
            | Identifier(_, _, name)
            | PostfixName(_, _, name, _) ->
                match tryResolveSymbol withTargetType.CurrentScope name withTargetType.SymTab with
                | Some(resolvedScope, symbol) ->
                    recordResolvedSymbol currentState.CurrentScope expr resolvedScope (Symbol.normalizedName symbol) withTargetType
                | None -> withTargetType
            | _ -> withTargetType
        let typedArgsState = inferExprList args recordedState
        do! putState typedArgsState
        do! resolveExprList mode args
    }

and private resolveStmt (mode: SymbolAddMode) (node: Stmt) : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        match node with
        | ProcedureDef(_, name, _, body)
        | FunctionDef(_, name, _, body) ->
            do! resolveRoutineBody mode name body

        | DimStmt(_, items) ->
            do! resolveExprList mode (items |> List.collect snd)

        | LocalStmt(_, items) ->
            do! resolveExprList mode (items |> List.collect (snd >> Option.defaultValue []))

        | ImplicitStmt _ -> ()

        | ReferenceStmt(_, children)
        | DataStmt(_, children) ->
            do! resolveExprList mode children
            let! stateAfterChildren = getState
            do! putState (inferExprList children stateAfterChildren)

        | ReadStmt(_, children) ->
            do! stateIter (resolveWritableExpr mode) children

        // Assignment is the densest statement-level semantic path:
        // resolve the writable target, resolve the RHS, infer the RHS type,
        // propagate any folded constant value onto the target symbol, then
        // finally validate assignment compatibility using the target's pre- and
        // post-update types.
        //
        // The pre/post comparison lets the analyzer support inference-driven
        // declarations without losing mismatch diagnostics for already-typed
        // targets.
        | Assignment(_, lhs, rhs) ->
            do! resolveWritableExpr mode lhs
            do! resolveExpr mode rhs
            let! stateAfterResolution = getState
            let rhsType, stateAfterRhs = inferExprType stateAfterResolution rhs
            let rhsValue = tryEvaluateConstantExpr stateAfterRhs rhs
            let lhsType = inferWritableTargetType stateAfterRhs lhs
            let stateAfterUpdate = updateWritableTargetType lhs rhsType rhsValue stateAfterRhs
            let stateWithRecordedTypes =
                stateAfterUpdate
                |> recordExprType stateAfterResolution.CurrentScope rhs rhsType
                |> recordTargetType stateAfterResolution.CurrentScope lhs lhsType
            let updatedLhsType = inferWritableTargetType stateAfterUpdate lhs
            let finalState =
                let compatibilityCheckedState =
                    if areTypesCompatible lhsType rhsType || areTypesCompatible updatedLhsType rhsType then
                        stateWithRecordedTypes
                    else
                        let pos = posOfExpr lhs
                        appendDiagnostic SemanticDiagnosticCode.InvalidAssignment None (Some pos) $"Cannot assign {rhsType} expression to {lhsType} target at {pos.EditorLineNo}:{pos.Column}" stateWithRecordedTypes
                compatibilityCheckedState
            do! putState finalState

        | GotoStmt(_, target)
        | GosubStmt(_, target) ->
            do! resolveExpr mode target
            let! stateAfterTarget = getState
            let targetType, nextState = inferExprType stateAfterTarget target
            let typedState = recordExprType stateAfterTarget.CurrentScope target targetType nextState
            let finalState =
                if targetType = SBType.String then
                    let pos = posOfExpr target
                    appendDiagnostic SemanticDiagnosticCode.InvalidControlFlowTarget None (Some pos) $"Control-flow target must be numeric at {pos.EditorLineNo}:{pos.Column}" typedState
                else
                    typedState
            do! putState finalState

        | OnGotoStmt(_, selector, targets)
        | OnGosubStmt(_, selector, targets) ->
            do! resolveExpr mode selector
            do! resolveExprList mode targets
            let! stateAfterSelector = getState
            let selectorType, stateAfterTypeCheck = inferExprType stateAfterSelector selector
            let selectorTypedState = recordExprType stateAfterSelector.CurrentScope selector selectorType stateAfterTypeCheck
            let stateAfterTargetTypes =
                (selectorTypedState, targets)
                ||> List.fold (fun acc target ->
                    let targetType, nextState = inferExprType acc target
                    recordExprType stateAfterSelector.CurrentScope target targetType nextState)
            let finalState =
                if selectorType = SBType.String then
                    let pos = posOfExpr selector
                    appendDiagnostic SemanticDiagnosticCode.InvalidSelector None (Some pos) $"Selector expression must be numeric at {pos.EditorLineNo}:{pos.Column}" stateAfterTargetTypes
                else
                    stateAfterTargetTypes
            do! putState finalState

        // Statement-call handling is separate from expression-call handling so
        // the analyzer can distinguish:
        // - procedures used correctly as statements
        // - functions incorrectly used as statements
        // - built-ins that have statement-specific argument rules such as INPUT
        //
        // This keeps call-shape diagnostics aligned with the surface syntax the
        // user actually wrote.
        | ProcedureCall(pos, name, args) ->
            let stateAfterCall = recordCall name pos currentState
            let stateAfterArity =
                match tryResolveSymbol stateAfterCall.CurrentScope name stateAfterCall.SymTab with
                | Some(_, symbol) -> validateCallArity name pos args.Length symbol stateAfterCall
                | None -> stateAfterCall
            do! putState (validateStatementCallShape name pos stateAfterArity)
            if isInputStatement name then
                let resolveInputArg expr =
                    match expr with
                    | Identifier _
                    | PostfixName _ -> resolveWritableExpr mode expr
                    | _ -> resolveExpr mode expr
                do! stateIter resolveInputArg args
            else
                do! resolveExprList mode args
            let! stateAfterArgs = getState
            let stateAfterBuiltInValidation =
                match tryResolveSymbol stateAfterArgs.CurrentScope name stateAfterArgs.SymTab with
                | Some(_, BuiltInSym _) ->
                    let argTypes, typedState = inferArgumentTypes args stateAfterArgs
                    validateBuiltInArguments name pos (List.zip args argTypes) typedState
                | _ -> inferExprList args stateAfterArgs
            do! putState stateAfterBuiltInValidation

        | ChannelProcedureCall(pos, name, channel, args) ->
            let stateAfterCall = recordCall name pos currentState
            let stateAfterArity =
                match tryResolveSymbol stateAfterCall.CurrentScope name stateAfterCall.SymTab with
                | Some(_, symbol) -> validateCallArity name pos args.Length symbol stateAfterCall
                | None -> stateAfterCall
            do! putState (validateStatementCallShape name pos stateAfterArity)
            do! resolveExpr mode channel
            let! stateAfterChannel = getState
            let channelType, stateAfterChannelType = inferExprType stateAfterChannel channel
            let typedChannelState = recordExprType stateAfterChannel.CurrentScope channel channelType stateAfterChannelType
            let stateAfterValidatedChannel =
                if channelType = SBType.String then
                    appendDiagnostic SemanticDiagnosticCode.InvalidChannel None (Some pos) $"Channel expression must be numeric at {pos.EditorLineNo}:{pos.Column}" typedChannelState
                else
                    typedChannelState
            do! putState stateAfterValidatedChannel
            if isInputStatement name then
                let resolveInputArg expr =
                    match expr with
                    | Identifier _
                    | PostfixName _ -> resolveWritableExpr mode expr
                    | _ -> resolveExpr mode expr
                do! stateIter resolveInputArg args
            else
                do! resolveExprList mode args
            let! stateAfterArgs = getState
            let stateAfterBuiltInValidation =
                match tryResolveSymbol stateAfterArgs.CurrentScope name stateAfterArgs.SymTab with
                | Some(_, BuiltInSym _) ->
                    let argTypes, typedState = inferArgumentTypes args stateAfterArgs
                    validateBuiltInArguments name pos (List.zip args argTypes) typedState
                | _ -> inferExprList args stateAfterArgs
            do! putState stateAfterBuiltInValidation

        | ForStmt(pos, name, startExpr, endExpr, stepExpr, body) ->
            do! putState (referenceSymbol None name pos currentState)
            do! resolveExpr mode startExpr
            do! resolveExpr mode endExpr
            match stepExpr with
            | Some expr -> do! resolveExpr mode expr
            | None -> ()
            let! stateAfterBounds = getState
            let startType, afterStart = inferExprType stateAfterBounds startExpr
            let endType, afterEnd = inferExprType afterStart endExpr
            let afterStep, stepType =
                match stepExpr with
                | Some expr ->
                    let inferred, updatedState = inferExprType afterEnd expr
                    recordExprType stateAfterBounds.CurrentScope expr inferred updatedState, inferred
                | None -> afterEnd, SBType.Integer
            let typedBoundsState =
                afterStep
                |> recordExprType stateAfterBounds.CurrentScope startExpr startType
                |> recordExprType stateAfterBounds.CurrentScope endExpr endType
            let counterExpr = mkIdentifier pos name
            let stateAfterCounter = updateWritableTargetType counterExpr (classifyNumericResultType startType endType) None typedBoundsState
            let finalState =
                if [ startType; endType; stepType ] |> List.exists (fun t -> t = SBType.String) then
                    appendDiagnostic SemanticDiagnosticCode.InvalidForBounds None (Some pos) $"FOR bounds and step must be numeric at {pos.EditorLineNo}:{pos.Column}" (recordTargetType stateAfterBounds.CurrentScope counterExpr (classifyNumericResultType startType endType) stateAfterCounter)
                else
                    recordTargetType stateAfterBounds.CurrentScope counterExpr (classifyNumericResultType startType endType) stateAfterCounter
            do! putState finalState
            do! putState (pushLoop name finalState)
            do! resolveBlock mode body
            let! stateAfterBody = getState
            do! putState (popLoop stateAfterBody)

        | RepeatStmt(_, name, body) ->
            do! putState (pushLoop name currentState)
            do! resolveBlock mode body
            let! stateAfterBody = getState
            do! putState (popLoop stateAfterBody)

        | IfStmt(_, cond, thenBody, elseBody) ->
            do! resolveExpr mode cond
            let! stateAfterCondition = getState
            let condType, stateAfterTypeCheck = inferExprType stateAfterCondition cond
            let typedConditionState = recordExprType stateAfterCondition.CurrentScope cond condType stateAfterTypeCheck
            let stateWithCondition =
                if condType = SBType.String || condType = SBType.Void then
                    let pos = posOfExpr cond
                    appendDiagnostic SemanticDiagnosticCode.InvalidCondition None (Some pos) $"Condition expression must be numeric at {pos.EditorLineNo}:{pos.Column}" typedConditionState
                else
                    typedConditionState
            do! putState stateWithCondition
            do! resolveBlock mode thenBody
            match elseBody with
            | Some block -> do! resolveBlock mode block
            | None -> ()

        | SelectStmt(_, selector, clauses) ->
            do! resolveExpr mode selector
            let! stateAfterSelector = getState
            let selectorType, stateAfterSelectorType = inferExprType stateAfterSelector selector
            let selectorTypedState = recordExprType stateAfterSelector.CurrentScope selector selectorType stateAfterSelectorType
            let stateWithSelector =
                if selectorType = SBType.Void then
                    let pos = posOfExpr selector
                    appendDiagnostic SemanticDiagnosticCode.InvalidSelector None (Some pos) $"Selector expression cannot be void at {pos.EditorLineNo}:{pos.Column}" selectorTypedState
                else
                    selectorTypedState
            do! putState stateWithSelector
            let rec resolveClauses remaining =
                state {
                    match remaining with
                    | [] -> return ()
                    | SelectClause(_, clauseSelector, rangeExpr, body) :: tail ->
                        do! resolveExpr mode clauseSelector
                        do! resolveExpr mode rangeExpr
                        let! stateAfterClauseExprs = getState
                        let clauseType, afterClauseType = inferExprType stateAfterClauseExprs clauseSelector
                        let rangeType, afterRangeType = inferExprType afterClauseType rangeExpr
                        let typedClauseState =
                            afterRangeType
                            |> recordExprType stateAfterClauseExprs.CurrentScope clauseSelector clauseType
                            |> recordExprType stateAfterClauseExprs.CurrentScope rangeExpr rangeType
                        let validatedState =
                            if selectorType <> SBType.Unknown && clauseType <> SBType.Unknown && not (areTypesCompatible selectorType clauseType) then
                                let pos = posOfExpr clauseSelector
                                appendDiagnostic SemanticDiagnosticCode.InvalidSelectClause None (Some pos) $"SELECT clause type does not match selector at {pos.EditorLineNo}:{pos.Column}" typedClauseState
                            elif rangeType = SBType.Void then
                                let pos = posOfExpr rangeExpr
                                appendDiagnostic SemanticDiagnosticCode.InvalidSelectClause None (Some pos) $"SELECT range expression cannot be void at {pos.EditorLineNo}:{pos.Column}" typedClauseState
                            else
                                typedClauseState
                        do! putState validatedState
                        match body with
                        | Some block -> do! resolveBlock mode block
                        | None -> ()
                        do! resolveClauses tail
                }
            do! resolveClauses clauses

        | WhenStmt(_, condition, body) ->
            match condition with
            | Some expr ->
                do! resolveExpr mode expr
                let! stateAfterCondition = getState
                let conditionType, nextState = inferExprType stateAfterCondition expr
                let typedConditionState = recordExprType stateAfterCondition.CurrentScope expr conditionType nextState
                let finalState =
                    if conditionType = SBType.String || conditionType = SBType.Void then
                        let pos = posOfExpr expr
                        appendDiagnostic SemanticDiagnosticCode.InvalidCondition None (Some pos) $"WHEN condition must be numeric at {pos.EditorLineNo}:{pos.Column}" typedConditionState
                    else
                        typedConditionState
                do! putState finalState
            | None -> ()
            do! resolveLineList mode body

        | ReturnStmt(_, value)
        | RestoreStmt(_, value) ->
            match value with
            | Some expr ->
                do! resolveExpr mode expr
                let! stateAfterValue = getState
                let valueType, nextState = inferExprType stateAfterValue expr
                let typedValueState = recordExprType stateAfterValue.CurrentScope expr valueType nextState
                let finalState =
                    match node with
                    | ReturnStmt(pos, _) ->
                        match tryResolveSymbol typedValueState.CurrentScope typedValueState.CurrentScope typedValueState.SymTab with
                        | Some(_, FunctionSym symbol) ->
                            let withReturnType =
                                { typedValueState with SymTab = tryUpdateResolvedSymbolTypeAndValue valueType None typedValueState.CurrentScope typedValueState.CurrentScope typedValueState.SymTab }
                            if areTypesCompatible symbol.ReturnType valueType then withReturnType
                            else appendDiagnostic SemanticDiagnosticCode.InvalidReturn (Some typedValueState.CurrentScope) (Some pos) $"Return expression type does not match function '{typedValueState.CurrentScope}' at {pos.EditorLineNo}:{pos.Column}" withReturnType
                        | Some(_, ProcedureSym _) ->
                            appendDiagnostic SemanticDiagnosticCode.InvalidReturn (Some typedValueState.CurrentScope) (Some pos) $"Procedure '{typedValueState.CurrentScope}' cannot return a value at {pos.EditorLineNo}:{pos.Column}" typedValueState
                        | _ -> typedValueState
                    | RestoreStmt _ ->
                        if valueType = SBType.String then
                            let pos = posOfExpr expr
                            appendDiagnostic SemanticDiagnosticCode.InvalidRestoreTarget None (Some pos) $"RESTORE target must be numeric at {pos.EditorLineNo}:{pos.Column}" typedValueState
                        else
                            typedValueState
                    | _ -> typedValueState
                do! putState finalState
            | None ->
                match node with
                | ReturnStmt(pos, _) ->
                    match tryResolveSymbol currentState.CurrentScope currentState.CurrentScope currentState.SymTab with
                    | Some(_, FunctionSym _) ->
                        do! putState (appendDiagnostic SemanticDiagnosticCode.InvalidReturn (Some currentState.CurrentScope) (Some pos) $"Function '{currentState.CurrentScope}' must return a value at {pos.EditorLineNo}:{pos.Column}" currentState)
                    | _ -> ()
                | _ -> ()

        | ExitStmt(pos, name)
        | NextStmt(pos, name) ->
            do! putState (validateLoopControl name pos currentState)

        | Remark _ -> ()
    }

let addToTable (mode: SymbolAddMode) (node: Ast) (_incomingState: ProcessingState) : State<ProcessingState, unit> =
    state {
        let (Program(_, children)) = node
        // The semantic walk is intentionally split into two full-tree passes over
        // the same AST:
        // 1. declaration collection builds scopes and symbols conservatively
        // 2. resolution/type validation consumes that symbol table to record
        //    semantic facts, diagnostics, inferred types, and constant values
        //
        // Keeping the passes explicit avoids many order-dependent bugs that come
        // from trying to resolve uses while declarations are still incomplete.
        do! collectDeclarationLineList mode children
        do! resolveLineList mode children
    }
