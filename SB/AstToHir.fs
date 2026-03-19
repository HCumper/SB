module AstToHir

open System.Globalization
open Types
open HIR
open SyntaxAst
open ProcessingTypes
open ScopeNames
open BuiltIns
open SemanticAnalysisSymbols
open SemanticAnalysisExpressions

type LoweringError = {
    Message: string
    Position: SourcePosition option
    Scope: string
}

type private LoweringContext = {
    State: ProcessingState
    SymbolIds: Map<string * string, SymbolId>
    CurrentScope: string
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

let private normalizeKey scopeName symbolName =
    scopeName, normalizeIdentifier symbolName

let private stateForScope ctx =
    { ctx.State with CurrentScope = ctx.CurrentScope }

let private inferExprTypeForScope ctx expr =
    inferExprType (stateForScope ctx) expr |> fst |> lowerSBType

let private inferTargetTypeForScope ctx expr =
    inferWritableTargetType (stateForScope ctx) expr |> lowerSBType

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

let private tryResolveSymbolId ctx name =
    match tryResolveSymbol ctx.CurrentScope name ctx.State.SymTab with
    | Some(resolvedScope, symbol) ->
        Map.tryFind (normalizeKey resolvedScope (Symbol.normalizedName symbol)) ctx.SymbolIds
    | None when isCallableBuiltIn name ->
        Map.tryFind (normalizeKey globalScope name) ctx.SymbolIds
    | None -> None

let private requireSymbolId ctx name pos =
    match tryResolveSymbolId ctx name with
    | Some symbolId -> Result.Ok symbolId
    | None -> fail ctx.CurrentScope (Some pos) $"Unable to resolve symbol id for '{name}'."

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
    let hirType = inferExprTypeForScope ctx expr

    match expr with
    | NumberLiteral(pos, value) -> ok (lowerLiteral pos value)
    | StringLiteral(pos, value) -> ok (lowerLiteral pos value)
    | Identifier(pos, name) ->
        requireSymbolId ctx name pos
        |> map (fun symbolId -> ReadVar(symbolId, hirType, pos))
    | PostfixName(pos, name, None) ->
        requireSymbolId ctx name pos
        |> map (fun symbolId ->
            match tryResolveSymbol ctx.CurrentScope name ctx.State.SymTab with
            | Some(_, FunctionSym _)
            | Some(_, BuiltInSym _) -> CallFunc(symbolId, [], hirType, pos)
            | _ -> ReadVar(symbolId, hirType, pos))
    | PostfixName(pos, name, Some args) ->
        zip (requireSymbolId ctx name pos) (args |> List.map (lowerExpr ctx) |> collectResults)
        |> map (fun (symbolId, loweredArgs) ->
            match tryResolveSymbol ctx.CurrentScope name ctx.State.SymTab with
            | Some(_, ArraySym _) -> ReadArrayElem(symbolId, loweredArgs, hirType, pos)
            | Some(_, FunctionSym _)
            | Some(_, BuiltInSym _) -> CallFunc(symbolId, loweredArgs, hirType, pos)
            | _ -> ReadArrayElem(symbolId, loweredArgs, hirType, pos))
    | SliceRange(pos, lhs, rhs) ->
        zip (lowerExpr ctx lhs) (lowerExpr ctx rhs)
        |> map (fun (left, right) -> Binary(HirBinaryOp.SliceRange, left, right, hirType, pos))
    | BinaryExpr(pos, op, lhs, rhs) ->
        zip (lowerExpr ctx lhs) (lowerExpr ctx rhs)
        |> map (fun (left, right) -> Binary(lowerBinaryOp op, left, right, hirType, pos))
    | UnaryExpr(pos, op, inner) ->
        lowerExpr ctx inner
        |> map (fun lowered -> Unary(lowerUnaryOp op, lowered, hirType, pos))

let private lowerTarget ctx expr =
    let hirType = inferTargetTypeForScope ctx expr

    match expr with
    | Identifier(pos, name)
    | PostfixName(pos, name, None) ->
        requireSymbolId ctx name pos
        |> map (fun symbolId -> WriteVar(symbolId, hirType, pos))
    | PostfixName(pos, name, Some args) ->
        zip (requireSymbolId ctx name pos) (args |> List.map (lowerExpr ctx) |> collectResults)
        |> map (fun (symbolId, loweredArgs) -> WriteArrayElem(symbolId, loweredArgs, hirType, pos))
    | _ ->
        fail ctx.CurrentScope (Some(posOfExpr expr)) "Expression is not a writable target."

let private lowerExprList ctx exprs =
    exprs |> List.map (lowerExpr ctx) |> collectResults

let private lowerTargetList ctx exprs =
    exprs |> List.map (lowerTarget ctx) |> collectResults

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
            lowerTargetList ctx args
            |> map (fun loweredTargets -> [ Input(None, loweredTargets, pos) ])
        else
            lowerExprList ctx args
            |> bind (fun loweredArgs ->
                if isCallableBuiltIn name then
                    ok [ BuiltInCall(lowerBuiltInKind name, None, loweredArgs, pos) ]
                else
                    requireSymbolId ctx name pos
                    |> map (fun symbolId -> [ ProcCall(symbolId, None, loweredArgs, pos) ]))
    | ChannelProcedureCall(pos, name, channel, args) ->
        zip (lowerExpr ctx channel) (lowerExprList ctx args)
        |> bind (fun (loweredChannel, loweredArgs) ->
            if normalizeIdentifier name = "INPUT" then
                lowerTargetList ctx args
                |> map (fun loweredTargets -> [ Input(Some loweredChannel, loweredTargets, pos) ])
            elif isCallableBuiltIn name then
                ok [ BuiltInCall(lowerBuiltInKind name, Some loweredChannel, loweredArgs, pos) ]
            else
                requireSymbolId ctx name pos
                |> map (fun symbolId -> [ ProcCall(symbolId, Some loweredChannel, loweredArgs, pos) ]))
    | ForStmt(pos, name, startExpr, endExpr, stepExpr, body) ->
        let loweredStep =
            match stepExpr with
            | Some expr -> lowerExpr ctx expr
            | None -> ok (Literal(HirConst.ConstInt 1, HirType.Int, pos))

        zip (zip (lowerExpr ctx startExpr) (lowerExpr ctx endExpr)) loweredStep
        |> bind (fun ((loweredStart, loweredEnd), loweredStepExpr) ->
            lowerBlock ctx body
            |> bind (fun loweredBody ->
                requireSymbolId ctx name pos
                    |> map (fun symbolId -> [ For(symbolId, loweredStart, loweredEnd, loweredStepExpr, loweredBody, pos) ])))
    | RepeatStmt(pos, label, body) ->
        lowerBlock ctx body
        |> map (fun loweredBody -> [ Repeat(lowerLoopName label, loweredBody, pos) ])
    | IfStmt(pos, cond, thenBody, elseBody) ->
        let loweredElse =
            match elseBody with
            | Some block -> lowerBlock ctx block |> map Some
            | None -> ok None

        zip (lowerExpr ctx cond) loweredElse
        |> bind (fun (loweredCond, loweredElseBlock) ->
            lowerBlock ctx thenBody
            |> map (fun loweredThen -> [ If(loweredCond, loweredThen, loweredElseBlock, pos) ]))
    | SelectStmt(pos, _, _) -> unsupported ctx pos "SELECT statements"
    | WhenStmt(pos, _, _) -> unsupported ctx pos "WHEN clauses"
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
    | ExitStmt(pos, label) -> ok [ HIR.Remark($"EXIT {label}", pos) ]
    | NextStmt(pos, label) -> ok [ HIR.Remark($"NEXT {label}", pos) ]
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

let private lowerRoutine symbolIds state routine =
    let buildRoutine pos name parameters body returnType =
        let ctx =
            { State = state
              SymbolIds = symbolIds
              CurrentScope = name }

        let lowerParameter parameter =
            match tryResolveSymbolId ctx parameter with
            | Some symbolId -> Result.Ok symbolId
            | None -> fail name (Some pos) $"Unable to resolve parameter '{parameter}' in routine '{name}'."

        zip (requireSymbolId ctx name pos) (parameters |> List.map lowerParameter |> collectResults)
        |> bind (fun (symbolId, parameterIds) ->
            lowerLineList ctx body
            |> map (fun loweredBody ->
                { Name = name
                  Symbol = symbolId
                  Parameters = parameterIds
                  Body = loweredBody
                  ReturnType = returnType
                  Position = pos }))

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
    let stmtData stmt =
        match stmt with
        | DataStmt(_, exprs) -> lowerExprList ctx exprs
        | _ -> ok []

    lines
    |> List.collect (fun (Line(_, _, stmts)) -> stmts)
    |> List.map stmtData
    |> collectResults
    |> map List.concat

let lowerToHir (state: ProcessingState) : Result<HirProgram, LoweringError list> =
    let symbolIds = buildSymbolIdMap state.SymTab
    let rootCtx =
        { State = state
          SymbolIds = symbolIds
          CurrentScope = globalScope }

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
                | _ -> None)
        | None -> []

    zip (zip routines (collectDataItems rootCtx lines)) (lowerMainLines rootCtx lines)
    |> map (fun ((loweredRoutines, dataItems), loweredMain) ->
        { Globals = globals
          Routines = loweredRoutines
          DataItems = dataItems
          Main = loweredMain })
