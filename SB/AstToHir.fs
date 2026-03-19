module AstToHir

open System.Globalization
open Types
open HIR
open SyntaxAst
open ProcessingTypes
open ScopeNames
open BuiltIns
open SemanticAnalysisSymbols

type LoweringError = {
    Message: string
    Position: SourcePosition option
    Scope: string
}

type private LoweringContext = {
    State: ProcessingState
    SymbolIds: Map<string * string, SymbolId>
    CurrentScope: string
    LoopStack: (LoopName * LoopId) list
    NextLoopId: int ref
}

let private ok value = Result.Ok value

let private fail scope position message =
    Result.Error [ { Message = message; Position = position; Scope = scope } ]

let private bind binder result =
    Result.bind binder result

let private map mapper result =
    Result.map mapper result

let private collectResults (results: Result<'a, LoweringError list> list) : Result<'a list, LoweringError list> =
    let folder state next =
        match state, next with
        | Result.Ok items, Result.Ok item -> Result.Ok(items @ [ item ])
        | Result.Error errs, Result.Ok _ -> Result.Error errs
        | Result.Ok _, Result.Error errs -> Result.Error errs
        | Result.Error left, Result.Error right -> Result.Error(left @ right)

    List.fold folder (Result.Ok []) results

let private zip left right =
    match left, right with
    | Result.Ok l, Result.Ok r -> Result.Ok(l, r)
    | Result.Error l, Result.Ok _ -> Result.Error l
    | Result.Ok _, Result.Error r -> Result.Error r
    | Result.Error l, Result.Error r -> Result.Error(l @ r)

let private lowerSBType = function
    | SBType.Integer -> HirType.Int
    | SBType.Real -> HirType.Float
    | SBType.String -> HirType.String
    | SBType.Void -> HirType.Void
    | SBType.Unknown -> HirType.Void

let private lowerUnaryOp op =
    match normalizeIdentifier op with
    | "+" -> HirUnaryOp.Identity
    | "-" -> HirUnaryOp.Negate
    | "NOT" -> HirUnaryOp.BitwiseNot
    | _ -> HirUnaryOp.UnaryUnknown op

let private lowerBinaryOp op =
    match normalizeIdentifier op with
    | "+" -> HirBinaryOp.Add
    | "-" -> HirBinaryOp.Subtract
    | "*" -> HirBinaryOp.Multiply
    | "/" -> HirBinaryOp.Divide
    | "^" -> HirBinaryOp.Power
    | "&" -> HirBinaryOp.Concat
    | "DIV" -> HirBinaryOp.IntegerDivide
    | "MOD" -> HirBinaryOp.Modulo
    | "AND" -> HirBinaryOp.BitwiseAnd
    | "OR" -> HirBinaryOp.BitwiseOr
    | "XOR" -> HirBinaryOp.BitwiseXor
    | "="
    | "==" -> HirBinaryOp.Equal
    | "<>" -> HirBinaryOp.NotEqual
    | "<" -> HirBinaryOp.LessThan
    | "<=" -> HirBinaryOp.LessThanOrEqual
    | ">" -> HirBinaryOp.GreaterThan
    | ">=" -> HirBinaryOp.GreaterThanOrEqual
    | "INSTR" -> HirBinaryOp.Instr
    | "TO" -> HirBinaryOp.SliceRange
    | _ -> HirBinaryOp.BinaryUnknown op

let private lowerBuiltInKind name =
    match normalizeIdentifier name with
    | "REFERENCE" -> BuiltInKind.Reference
    | "INPUT" -> BuiltInKind.Input
    | "PRINT" -> BuiltInKind.Print
    | "GOTO" -> BuiltInKind.GotoBuiltIn
    | "GOSUB" -> BuiltInKind.GosubBuiltIn
    | "ON-GOTO" -> BuiltInKind.OnGotoBuiltIn
    | "ON-GOSUB" -> BuiltInKind.OnGosubBuiltIn
    | normalized -> BuiltInKind.NamedBuiltIn normalized

let private lowerLoopName = function
    | null
    | "" -> LoopName.AnonymousLoop
    | name -> LoopName.NamedLoop name

let private nextLoopId ctx =
    let current = !(ctx.NextLoopId)
    ctx.NextLoopId := current + 1
    LoopId current

let private withLoop ctx loopName loopId =
    { ctx with LoopStack = (loopName, loopId) :: ctx.LoopStack }

let private normalizeKey scopeName symbolName =
    scopeName, normalizeIdentifier symbolName

let private exprKey ctx expr =
    nodeIdOfExpr expr

let private getRecordedExprType ctx expr =
    match Map.tryFind (exprKey ctx expr) ctx.State.ExprTypes with
    | Some exprType -> ok (lowerSBType exprType)
    | None -> fail ctx.CurrentScope (Some(exprPosition expr)) "Missing recorded expression type."

let private getRecordedTargetType ctx expr =
    match Map.tryFind (exprKey ctx expr) ctx.State.TargetTypes with
    | Some exprType -> ok (lowerSBType exprType)
    | None -> fail ctx.CurrentScope (Some(exprPosition expr)) "Missing recorded target type."

let private getRecordedResolvedSymbol ctx expr =
    Map.tryFind (exprKey ctx expr) ctx.State.ResolvedSymbols

let private buildSymbolIdMap (symTab: SymbolTable) =
    let scopedSymbols =
        symTab
        |> Map.toList
        |> List.collect (fun (scopeName, scope) ->
            scope.Symbols
            |> Map.toList
            |> List.map (fun (symbolName, _symbol) -> normalizeKey scopeName symbolName))

    let builtInSymbols =
        callableBuiltInNames
        |> Set.toList
        |> List.map (fun name -> normalizeKey globalScope name)

    scopedSymbols @ builtInSymbols
    |> List.distinct
    |> List.mapi (fun index key -> key, SymbolId index)
    |> Map.ofList

let private buildSymbolNameMap (symbolIds: Map<string * string, SymbolId>) =
    symbolIds
    |> Map.toList
    |> List.map (fun ((_, symbolName), symbolId) -> symbolId, symbolName)
    |> Map.ofList

let private requireSymbolIdForExpr ctx expr name pos =
    match getRecordedResolvedSymbol ctx expr with
    | Some resolved ->
        match Map.tryFind (normalizeKey resolved.Scope resolved.Name) ctx.SymbolIds with
        | Some symbolId -> Result.Ok symbolId
        | None -> fail ctx.CurrentScope (Some pos) $"Unable to resolve symbol id for '{name}'."
    | None when isCallableBuiltIn name ->
        match Map.tryFind (normalizeKey globalScope name) ctx.SymbolIds with
        | Some symbolId -> Result.Ok symbolId
        | None -> fail ctx.CurrentScope (Some pos) $"Unable to resolve built-in symbol id for '{name}'."
    | None -> fail ctx.CurrentScope (Some pos) $"Unable to resolve symbol id for '{name}'."

let private requireSymbolIdForName ctx scopeHint name pos =
    match tryResolveSymbol scopeHint name ctx.State.SymTab with
    | Some(resolvedScope, symbol) ->
        match Map.tryFind (normalizeKey resolvedScope (Symbol.normalizedName symbol)) ctx.SymbolIds with
        | Some symbolId -> Result.Ok symbolId
        | None -> fail ctx.CurrentScope (Some pos) $"Unable to resolve symbol id for '{name}'."
    | None when isCallableBuiltIn name ->
        match Map.tryFind (normalizeKey globalScope name) ctx.SymbolIds with
        | Some symbolId -> Result.Ok symbolId
        | None -> fail ctx.CurrentScope (Some pos) $"Unable to resolve built-in symbol id for '{name}'."
    | None -> fail ctx.CurrentScope (Some pos) $"Unable to resolve symbol id for '{name}'."

let private resolveLoopTarget ctx label pos =
    let desired =
        match lowerLoopName label with
        | AnonymousLoop -> None
        | NamedLoop name -> Some(normalizeIdentifier name)

    let matchesLoop (loopName, _) =
        match desired, loopName with
        | None, _ -> true
        | Some expected, NamedLoop actual -> normalizeIdentifier actual = expected
        | Some _, AnonymousLoop -> false

    match ctx.LoopStack |> List.tryFind matchesLoop with
    | Some(_, loopId) -> ok loopId
    | None -> fail ctx.CurrentScope (Some pos) $"Unable to resolve loop target '{label}'."

let private lowerStorageClass scopeName (symbol: Symbol) =
    match scopeName, symbol with
    | scope, ParameterSym _ -> RoutineParameterStorage scope
    | scope, _ when scope <> globalScope -> RoutineLocalStorage scope
    | _ -> GlobalStorage

let private lowerStorageType (symbol: Symbol) =
    match symbol with
    | ArraySym arr -> Array(lowerSBType arr.ElementType)
    | _ -> lowerSBType (Symbol.typ symbol)

let private lowerStorageSlotMap (symTab: SymbolTable) (symbolIds: Map<string * string, SymbolId>) =
    symTab
    |> Map.toList
    |> List.collect (fun (scopeName, scope) ->
        scope.Symbols
        |> Map.toList
        |> List.choose (fun (symbolName, symbol) ->
            match symbol with
            | VariableSym _
            | ParameterSym _
            | ArraySym _
            | ConstantSym _ ->
                match Map.tryFind (normalizeKey scopeName symbolName) symbolIds with
                | Some symbolId -> Some(scopeName, symbolName, symbolId, symbol)
                | None -> None
            | _ -> None))
    |> List.mapi (fun index (scopeName, symbolName, symbolId, symbol) ->
        symbolId,
        { Symbol = symbolId
          Slot = StorageSlotId index
          Name = symbolName
          Type = lowerStorageType symbol
          Class = lowerStorageClass scopeName symbol
          Position = Symbol.position symbol })
    |> Map.ofList

let private lowerSelectClauseCondition pos selector clauseSelector rangeExpr =
    let lowerBound = Binary(HirBinaryOp.GreaterThanOrEqual, selector, clauseSelector, HirType.Int, pos)
    let upperBound = Binary(HirBinaryOp.LessThanOrEqual, selector, rangeExpr, HirType.Int, pos)
    Binary(HirBinaryOp.BitwiseAnd, lowerBound, upperBound, HirType.Int, pos)

let rec private lowerSelectClauses selector (clauses: HirSelectClause list) =
    match clauses with
    | [] -> ok []
    | clause :: tail ->
        let condition = lowerSelectClauseCondition clause.Position selector clause.Selector clause.Range
        match clause.Body with
        | Some body ->
            lowerSelectClauses selector tail
            |> map (fun elseBranch -> [ If(condition, body, (if List.isEmpty elseBranch then None else Some elseBranch), clause.Position) ])
        | None ->
            lowerSelectClauses selector tail

let private tryParseDouble (value: string) =
    System.Double.TryParse(value, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture)

let private unquoteString (value: string) =
    if value.Length >= 2 && value.StartsWith("\"") && value.EndsWith("\"") then
        value.Substring(1, value.Length - 2).Replace("\"\"", "\"")
    else
        value

let private lowerLiteral (pos: SourcePosition) (value: string) =
    match System.Int32.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | true, n -> Literal(HirConst.ConstInt n, HirType.Int, pos)
    | _ when value.StartsWith("\"") && value.EndsWith("\"") ->
        Literal(HirConst.ConstString (unquoteString value), HirType.String, pos)
    | _ ->
        match tryParseDouble value with
        | true, n -> Literal(HirConst.ConstFloat n, HirType.Float, pos)
        | _ -> Literal(HirConst.ConstString value, HirType.String, pos)

let private unsupported ctx pos construct =
    fail ctx.CurrentScope (Some pos) $"HIR lowering does not support {construct} yet."

let rec private lowerExpr ctx expr =
    match expr with
    | NumberLiteral(_, pos, value) -> ok (lowerLiteral pos value)
    | StringLiteral(_, pos, value) -> ok (lowerLiteral pos value)
    | Identifier(_, pos, name) ->
        zip (getRecordedExprType ctx expr) (requireSymbolIdForExpr ctx expr name pos)
        |> map (fun (hirType, symbolId) -> ReadVar(symbolId, hirType, pos))
    | PostfixName(_, pos, name, None) ->
        zip (getRecordedExprType ctx expr) (requireSymbolIdForExpr ctx expr name pos)
        |> map (fun (hirType, symbolId) ->
            match getRecordedResolvedSymbol ctx expr with
            | Some resolved when isCallableBuiltIn resolved.Name -> CallFunc(symbolId, [], hirType, pos)
            | Some resolved ->
                match Map.tryFind resolved.Scope ctx.State.SymTab with
                | Some scope ->
                    match Map.tryFind resolved.Name scope.Symbols with
                    | Some(FunctionSym _)
                    | Some(BuiltInSym _) -> CallFunc(symbolId, [], hirType, pos)
                    | _ -> ReadVar(symbolId, hirType, pos)
                | None -> ReadVar(symbolId, hirType, pos)
            | _ -> ReadVar(symbolId, hirType, pos))
    | PostfixName(_, pos, name, Some args) ->
        zip (getRecordedExprType ctx expr) (zip (requireSymbolIdForExpr ctx expr name pos) (args |> List.map (lowerExpr ctx) |> collectResults))
        |> map (fun (hirType, (symbolId, loweredArgs)) ->
            match getRecordedResolvedSymbol ctx expr with
            | Some resolved when isCallableBuiltIn resolved.Name -> CallFunc(symbolId, loweredArgs, hirType, pos)
            | Some resolved ->
                match Map.tryFind resolved.Scope ctx.State.SymTab with
                | Some scope ->
                    match Map.tryFind resolved.Name scope.Symbols with
                    | Some(ArraySym _) -> ReadArrayElem(symbolId, loweredArgs, hirType, pos)
                    | Some(FunctionSym _)
                    | Some(BuiltInSym _) -> CallFunc(symbolId, loweredArgs, hirType, pos)
                    | _ -> ReadArrayElem(symbolId, loweredArgs, hirType, pos)
                | None -> ReadArrayElem(symbolId, loweredArgs, hirType, pos)
            | _ -> ReadArrayElem(symbolId, loweredArgs, hirType, pos))
    | SliceRange(_, pos, lhs, rhs) ->
        zip (getRecordedExprType ctx expr) (zip (lowerExpr ctx lhs) (lowerExpr ctx rhs))
        |> map (fun (hirType, (left, right)) -> Binary(HirBinaryOp.SliceRange, left, right, hirType, pos))
    | BinaryExpr(_, pos, op, lhs, rhs) ->
        zip (getRecordedExprType ctx expr) (zip (lowerExpr ctx lhs) (lowerExpr ctx rhs))
        |> map (fun (hirType, (left, right)) -> Binary(lowerBinaryOp op, left, right, hirType, pos))
    | UnaryExpr(_, pos, op, inner) ->
        zip (getRecordedExprType ctx expr) (lowerExpr ctx inner)
        |> map (fun (hirType, lowered) -> Unary(lowerUnaryOp op, lowered, hirType, pos))

let private lowerTarget ctx expr =
    match expr with
    | Identifier(_, pos, name)
    | PostfixName(_, pos, name, None) ->
        zip (getRecordedTargetType ctx expr) (requireSymbolIdForExpr ctx expr name pos)
        |> map (fun (hirType, symbolId) -> WriteVar(symbolId, hirType, pos))
    | PostfixName(_, pos, name, Some args) ->
        zip (getRecordedTargetType ctx expr) (zip (requireSymbolIdForExpr ctx expr name pos) (args |> List.map (lowerExpr ctx) |> collectResults))
        |> map (fun (hirType, (symbolId, loweredArgs)) -> WriteArrayElem(symbolId, loweredArgs, hirType, pos))
    | _ ->
        fail ctx.CurrentScope (Some(exprPosition expr)) "Expression is not a writable target."

let private lowerExprList ctx exprs =
    exprs |> List.map (lowerExpr ctx) |> collectResults

let private lowerTargetList ctx exprs =
    exprs |> List.map (lowerTarget ctx) |> collectResults

let private canLowerAsTarget ctx expr =
    Map.containsKey (nodeIdOfExpr expr) ctx.State.TargetTypes

let private lowerInputArgs ctx pos args =
    let rec loop prompts targets inTargetPhase remaining =
        match remaining with
        | [] -> ok (List.rev prompts, List.rev targets)
        | expr :: tail when canLowerAsTarget ctx expr ->
            lowerTarget ctx expr
            |> bind (fun loweredTarget -> loop prompts (loweredTarget :: targets) true tail)
        | expr :: tail when not inTargetPhase ->
            lowerExpr ctx expr
            |> bind (fun loweredPrompt -> loop (loweredPrompt :: prompts) targets false tail)
        | expr :: _ ->
            fail ctx.CurrentScope (Some(exprPosition expr)) "INPUT prompt expressions must appear before writable targets."

    loop [] [] false args

let rec private lowerStmt ctx stmt =
    match stmt with
    | ProcedureDef _
    | FunctionDef _
    | DimStmt _
    | LocalStmt _
    | ImplicitStmt _ -> ok []
    | ReferenceStmt(pos, exprs) ->
        lowerExprList ctx exprs
        |> map (fun lowered -> [ BuiltInCall(BuiltInKind.Reference, None, lowered, pos) ])
    | Assignment(pos, lhs, rhs) ->
        zip (lowerTarget ctx lhs) (lowerExpr ctx rhs)
        |> map (fun (target, value) -> [ Assign(target, value, pos) ])
    | GotoStmt(pos, target) ->
        lowerExpr ctx target
        |> map (fun lowered -> [ Goto(lowered, pos) ])
    | GosubStmt(pos, target) ->
        lowerExpr ctx target
        |> map (fun lowered -> [ Gosub(lowered, pos) ])
    | OnGotoStmt(pos, selector, targets) ->
        zip (lowerExpr ctx selector) (lowerExprList ctx targets)
        |> map (fun (loweredSelector, loweredTargets) -> [ OnGoto(loweredSelector, loweredTargets, pos) ])
    | OnGosubStmt(pos, selector, targets) ->
        zip (lowerExpr ctx selector) (lowerExprList ctx targets)
        |> map (fun (loweredSelector, loweredTargets) -> [ OnGosub(loweredSelector, loweredTargets, pos) ])
    | ProcedureCall(pos, name, args) ->
        if normalizeIdentifier name = "INPUT" then
            lowerInputArgs ctx pos args
            |> map (fun (prompts, loweredTargets) -> [ Input(None, prompts, loweredTargets, pos) ])
        else
            lowerExprList ctx args
            |> bind (fun loweredArgs ->
                if isCallableBuiltIn name then
                    ok [ BuiltInCall(lowerBuiltInKind name, None, loweredArgs, pos) ]
                else
                    requireSymbolIdForName ctx ctx.CurrentScope name pos
                    |> map (fun symbolId -> [ ProcCall(symbolId, None, loweredArgs, pos) ]))
    | ChannelProcedureCall(pos, name, channel, args) ->
        zip (lowerExpr ctx channel) (lowerExprList ctx args)
        |> bind (fun (loweredChannel, loweredArgs) ->
            if normalizeIdentifier name = "INPUT" then
                lowerInputArgs ctx pos args
                |> map (fun (prompts, loweredTargets) -> [ Input(Some loweredChannel, prompts, loweredTargets, pos) ])
            elif isCallableBuiltIn name then
                ok [ BuiltInCall(lowerBuiltInKind name, Some loweredChannel, loweredArgs, pos) ]
            else
                requireSymbolIdForName ctx ctx.CurrentScope name pos
                |> map (fun symbolId -> [ ProcCall(symbolId, Some loweredChannel, loweredArgs, pos) ]))
    | ForStmt(pos, name, startExpr, endExpr, stepExpr, body) ->
        let loopId = nextLoopId ctx
        let loopCtx = withLoop ctx (NamedLoop name) loopId
        let loweredStep =
            match stepExpr with
            | Some expr -> lowerExpr ctx expr
            | None -> ok (Literal(HirConst.ConstInt 1, HirType.Int, pos))

        zip (zip (lowerExpr ctx startExpr) (lowerExpr ctx endExpr)) loweredStep
        |> bind (fun ((loweredStart, loweredEnd), loweredStepExpr) ->
            lowerBlock loopCtx body
            |> bind (fun loweredBody ->
                requireSymbolIdForName ctx ctx.CurrentScope name pos
                    |> map (fun symbolId -> [ For(loopId, symbolId, loweredStart, loweredEnd, loweredStepExpr, loweredBody, pos) ])))
    | RepeatStmt(pos, label, body) ->
        let loopName = lowerLoopName label
        let loopId = nextLoopId ctx
        let loopCtx = withLoop ctx loopName loopId
        lowerBlock loopCtx body
        |> map (fun loweredBody -> [ Repeat(loopId, loopName, loweredBody, pos) ])
    | IfStmt(pos, cond, thenBody, elseBody) ->
        let loweredElse =
            match elseBody with
            | Some block -> lowerBlock ctx block |> map Some
            | None -> ok None

        zip (lowerExpr ctx cond) loweredElse
        |> bind (fun (loweredCond, loweredElseBlock) ->
            lowerBlock ctx thenBody
            |> map (fun loweredThen -> [ If(loweredCond, loweredThen, loweredElseBlock, pos) ]))
    | SelectStmt(pos, selector, clauses) ->
        let lowerClause (SelectClause(clausePos, clauseSelector, rangeExpr, body)) =
            zip (zip (lowerExpr ctx clauseSelector) (lowerExpr ctx rangeExpr)) (lowerOptionalBlock ctx body)
            |> map (fun ((loweredSelector, loweredRange), loweredBody) ->
                { Selector = loweredSelector
                  Range = loweredRange
                  Body = loweredBody
                  Position = clausePos })

        zip (lowerExpr ctx selector) (clauses |> List.map lowerClause |> collectResults)
        |> bind (fun (loweredSelector, loweredClauses) -> lowerSelectClauses loweredSelector loweredClauses)
    | WhenStmt(pos, condition, body) ->
        let loweredCondition =
            match condition with
            | Some expr -> lowerExpr ctx expr |> map Some
            | None -> ok None

        zip loweredCondition (lowerLineList ctx body)
        |> map (fun (loweredConditionExpr, loweredBody) ->
            match loweredConditionExpr with
            | Some cond -> [ If(cond, loweredBody, None, pos) ]
            | None -> loweredBody)
    | ReturnStmt(pos, value) ->
        match value with
        | Some expr -> lowerExpr ctx expr |> map (fun lowered -> [ Return(Some lowered, pos) ])
        | None -> ok [ Return(None, pos) ]
    | DataStmt(pos, exprs) ->
        ok []
    | ReadStmt(pos, exprs) ->
        lowerTargetList ctx exprs
        |> map (fun lowered -> [ Read(lowered, pos) ])
    | RestoreStmt(pos, value) ->
        match value with
        | Some expr -> lowerExpr ctx expr |> map (fun lowered -> [ Restore(Some lowered, pos) ])
        | None -> ok [ Restore(None, pos) ]
    | ExitStmt(pos, label) -> resolveLoopTarget ctx label pos |> map (fun loopId -> [ Exit(loopId, pos) ])
    | NextStmt(pos, label) -> resolveLoopTarget ctx label pos |> map (fun loopId -> [ Next(loopId, pos) ])
    | SyntaxAst.Remark(pos, text) -> ok [ HIR.Remark(text, pos) ]

and private lowerStmtList ctx stmts =
    stmts
    |> List.map (lowerStmt ctx)
    |> collectResults
    |> map List.concat

and private lowerLine ctx (Line(pos, lineNumber, stmts)) =
    lowerStmtList ctx stmts
    |> map (fun lowered ->
        match lineNumber with
        | Some n -> LineNumber(n, pos) :: lowered
        | None -> lowered)

and private lowerLineList ctx lines =
    lines
    |> List.map (lowerLine ctx)
    |> collectResults
    |> map List.concat

and private lowerBlock ctx block =
    match block with
    | StatementBlock stmts -> lowerStmtList ctx stmts
    | LineBlock lines -> lowerLineList ctx lines

and private lowerOptionalBlock ctx block =
    match block with
    | Some body -> lowerBlock ctx body |> map Some
    | None -> ok None

let private lowerRoutine symbolIds state routine =
    let slotMap = lowerStorageSlotMap state.SymTab symbolIds
    let buildRoutine pos name parameters body returnType =
        let ctx =
            { State = state
              SymbolIds = symbolIds
              CurrentScope = name
              LoopStack = []
              NextLoopId = ref 0 }

        let lowerParameter parameter =
            match Map.tryFind (normalizeIdentifier name, normalizeIdentifier parameter) state.ParameterSymbols with
            | Some resolved ->
                match Map.tryFind (normalizeKey resolved.Scope resolved.Name) symbolIds with
                | Some symbolId ->
                    match Map.tryFind symbolId slotMap with
                    | Some storage -> Result.Ok storage
                    | None -> fail name (Some pos) $"Missing storage slot for parameter '{parameter}' in routine '{name}'."
                | None -> fail name (Some pos) $"Unable to resolve parameter '{parameter}' in routine '{name}'."
            | None -> fail name (Some pos) $"Unable to resolve parameter '{parameter}' in routine '{name}'."

        match Map.tryFind (normalizeIdentifier name) state.RoutineSymbols with
        | Some resolved ->
            match Map.tryFind (normalizeKey resolved.Scope resolved.Name) symbolIds with
            | Some routineId ->
                zip (ok routineId) (parameters |> List.map lowerParameter |> collectResults)
                |> bind (fun (symbolId, parameterIds) ->
                    lowerLineList ctx body
                    |> map (fun loweredBody ->
                        let locals =
                            match Map.tryFind name state.SymTab with
                            | Some scope ->
                                scope.Symbols
                                |> Map.toList
                                |> List.choose (fun (_, symbol) ->
                                    match symbol with
                                    | VariableSym _
                                    | ArraySym _ ->
                                        Map.tryFind (normalizeKey name (Symbol.normalizedName symbol)) symbolIds
                                        |> Option.bind (fun symbolId -> Map.tryFind symbolId slotMap)
                                    | _ -> None)
                            | None -> []
                        { Name = name
                          Symbol = symbolId
                          Parameters = parameterIds
                          Locals = locals
                          Body = loweredBody
                          ReturnType = returnType
                          Position = pos }))
            | None -> fail name (Some pos) $"Unable to resolve routine symbol id for '{name}'."
        | None -> fail name (Some pos) $"Unable to resolve routine '{name}'."

    match routine with
    | ProcedureDef(pos, name, parameters, body) ->
        buildRoutine pos name parameters body None
    | FunctionDef(pos, name, parameters, body) ->
        let returnType =
            match tryResolveSymbol globalScope name state.SymTab with
            | Some(_, FunctionSym symbol) -> Some(lowerSBType symbol.ReturnType)
            | _ -> Some HirType.Void

        buildRoutine pos name parameters body returnType
    | _ ->
        fail globalScope None "Expected a routine definition."

let private lowerMainLines ctx lines =
    let stripRoutineDecls (Line(pos, lineNumber, stmts)) =
        let filtered =
            stmts
            |> List.filter (function
                | ProcedureDef _
                | FunctionDef _ -> false
                | _ -> true)

        Line(pos, lineNumber, filtered)

    lines
    |> List.map stripRoutineDecls
    |> List.filter (fun (Line(_, lineNumber, stmts)) -> lineNumber.IsSome || not stmts.IsEmpty)
    |> lowerLineList ctx

let private collectDataItems ctx lines =
    let folder (slotIndex, entries, restorePoints) (Line(_, lineNumber, stmts)) =
        let lineRestorePoints =
            match lineNumber with
            | Some n when stmts |> List.exists (function DataStmt _ -> true | _ -> false) ->
                restorePoints @ [ { LineNumber = n; Slot = DataSlotId slotIndex } ]
            | _ -> restorePoints

        let rec lowerStmts currentIndex currentEntries remaining =
            match remaining with
            | [] -> ok (currentIndex, currentEntries)
            | DataStmt(pos, exprs) :: tail ->
                lowerExprList ctx exprs
                |> bind (fun lowered ->
                    let newEntries =
                        lowered
                        |> List.mapi (fun offset value ->
                            { Slot = DataSlotId(currentIndex + offset)
                              Value = value
                              Position = pos
                              LineNumber = lineNumber })
                    lowerStmts (currentIndex + lowered.Length) (currentEntries @ newEntries) tail)
            | _ :: tail -> lowerStmts currentIndex currentEntries tail

        lowerStmts slotIndex entries stmts
        |> map (fun (nextIndex, nextEntries) -> nextIndex, nextEntries, lineRestorePoints)

    lines
    |> List.fold (fun acc line ->
        acc |> bind (fun state -> folder state line))
        (ok (0, [], []))
    |> map (fun (_, entries, restorePoints) -> entries, restorePoints)

let lowerToHir (state: ProcessingState) : Result<HirProgram, LoweringError list> =
    let symbolIds = buildSymbolIdMap state.SymTab
    let rootCtx =
        { State = state
          SymbolIds = symbolIds
          CurrentScope = globalScope
          LoopStack = []
          NextLoopId = ref 0 }

    let (Program(_, lines)) = state.Ast

    let topLevelStatements =
        lines
        |> List.collect (fun (Line(_, _, stmts)) -> stmts)

    let routines =
        topLevelStatements
        |> List.choose (function
            | ProcedureDef _ as stmt -> Some stmt
            | FunctionDef _ as stmt -> Some stmt
            | _ -> None)
        |> List.map (lowerRoutine symbolIds state)
        |> collectResults

    let slotMap = lowerStorageSlotMap state.SymTab symbolIds

    let globals =
        match Map.tryFind globalScope state.SymTab with
        | Some scope ->
            scope.Symbols
            |> Map.toList
            |> List.choose (fun (symbolName, symbol) ->
                match symbol with
                | VariableSym _
                | ConstantSym _
                | ArraySym _ ->
                    Map.tryFind (normalizeKey globalScope symbolName) symbolIds
                    |> Option.bind (fun symbolId -> Map.tryFind symbolId slotMap)
                | _ -> None)
        | None -> []

    zip (zip routines (collectDataItems rootCtx lines)) (lowerMainLines rootCtx lines)
    |> map (fun ((loweredRoutines, (dataEntries, restorePoints)), loweredMain) ->
        { SymbolNames = buildSymbolNameMap symbolIds
          Globals = globals
          Routines = loweredRoutines
          DataEntries = dataEntries
          RestorePoints = restorePoints
          Main = loweredMain })
