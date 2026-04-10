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

let private normalizeBuiltInArgs name args =
    match normalizeIdentifier name with
    | "LINE"
    | "ARC"
    | "ARC_R" ->
        args
        |> List.collect (function
            | SliceRange(_, _, lhs, rhs) -> [ lhs; rhs ]
            | expr -> [ expr ])
    | _ -> args

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

let rec private bindLoopCounterExpr (counterName: string) (symbolId: SymbolId) expr =
    let sameName name = normalizeIdentifier name = normalizeIdentifier counterName
    match expr with
    | DynamicReadVar(name, hirType, pos) when sameName name ->
        ReadVar(symbolId, hirType, pos)
    | DynamicReadArrayElem(name, args, hirType, pos) ->
        DynamicReadArrayElem(name, args |> List.map (bindLoopCounterExpr counterName symbolId), hirType, pos)
    | DynamicReadStringChar(name, index, hirType, pos) ->
        DynamicReadStringChar(name, bindLoopCounterExpr counterName symbolId index, hirType, pos)
    | Unary(op, inner, hirType, pos) ->
        Unary(op, bindLoopCounterExpr counterName symbolId inner, hirType, pos)
    | Binary(op, lhs, rhs, hirType, pos) ->
        Binary(op, bindLoopCounterExpr counterName symbolId lhs, bindLoopCounterExpr counterName symbolId rhs, hirType, pos)
    | CallFunc(callee, args, hirType, pos) ->
        let loweredArgs =
            args
            |> List.map (function
                | ValueArg valueExpr -> ValueArg(bindLoopCounterExpr counterName symbolId valueExpr)
                | RefArg target -> RefArg(bindLoopCounterTarget counterName symbolId target))
        CallFunc(callee, loweredArgs, hirType, pos)
    | ReadArrayElem(arraySymbol, args, hirType, pos) ->
        ReadArrayElem(arraySymbol, args |> List.map (bindLoopCounterExpr counterName symbolId), hirType, pos)
    | ReadStringChar(arraySymbol, index, hirType, pos) ->
        ReadStringChar(arraySymbol, bindLoopCounterExpr counterName symbolId index, hirType, pos)
    | other -> other

and private bindLoopCounterTarget (counterName: string) (symbolId: SymbolId) target =
    let sameName name = normalizeIdentifier name = normalizeIdentifier counterName
    match target with
    | DynamicWriteVar(name, hirType, pos) when sameName name ->
        WriteVar(symbolId, hirType, pos)
    | WriteArrayElem(arraySymbol, args, hirType, pos) ->
        WriteArrayElem(arraySymbol, args |> List.map (bindLoopCounterExpr counterName symbolId), hirType, pos)
    | WriteStringChar(arraySymbol, index, hirType, pos) ->
        WriteStringChar(arraySymbol, bindLoopCounterExpr counterName symbolId index, hirType, pos)
    | DynamicWriteArrayElem(name, args, hirType, pos) ->
        DynamicWriteArrayElem(name, args |> List.map (bindLoopCounterExpr counterName symbolId), hirType, pos)
    | DynamicWriteStringChar(name, index, hirType, pos) ->
        DynamicWriteStringChar(name, bindLoopCounterExpr counterName symbolId index, hirType, pos)
    | other -> other

and private bindLoopCounterStmt (counterName: string) (symbolId: SymbolId) stmt =
    match stmt with
    | Assign(target, expr, pos) ->
        Assign(bindLoopCounterTarget counterName symbolId target, bindLoopCounterExpr counterName symbolId expr, pos)
    | ProcCall(callee, channel, args, pos) ->
        let loweredChannel =
            channel
            |> Option.map (function
                | ExplicitChannel channelExpr -> ExplicitChannel(bindLoopCounterExpr counterName symbolId channelExpr)
                | ImplicitChannel channelExpr -> ImplicitChannel(bindLoopCounterExpr counterName symbolId channelExpr))
        let loweredArgs =
            args
            |> List.map (function
                | ValueArg valueExpr -> ValueArg(bindLoopCounterExpr counterName symbolId valueExpr)
                | RefArg target -> RefArg(bindLoopCounterTarget counterName symbolId target))
        ProcCall(callee, loweredChannel, loweredArgs, pos)
    | BuiltInCall(kind, channel, args, pos) ->
        let loweredChannel =
            channel
            |> Option.map (function
                | ExplicitChannel channelExpr -> ExplicitChannel(bindLoopCounterExpr counterName symbolId channelExpr)
                | ImplicitChannel channelExpr -> ImplicitChannel(bindLoopCounterExpr counterName symbolId channelExpr))
        BuiltInCall(kind, loweredChannel, args |> List.map (bindLoopCounterExpr counterName symbolId), pos)
    | Input(channel, prompts, targets, pos) ->
        let loweredChannel =
            channel
            |> Option.map (function
                | ExplicitChannel channelExpr -> ExplicitChannel(bindLoopCounterExpr counterName symbolId channelExpr)
                | ImplicitChannel channelExpr -> ImplicitChannel(bindLoopCounterExpr counterName symbolId channelExpr))
        Input(loweredChannel, prompts |> List.map (bindLoopCounterExpr counterName symbolId), targets |> List.map (bindLoopCounterTarget counterName symbolId), pos)
    | If(condition, thenBlock, elseBlock, pos) ->
        If(bindLoopCounterExpr counterName symbolId condition, bindLoopCounterBlock counterName symbolId thenBlock, elseBlock |> Option.map (bindLoopCounterBlock counterName symbolId), pos)
    | For(loopId, innerSymbolId, startExpr, endExpr, stepExpr, body, pos) ->
        For(loopId, innerSymbolId, bindLoopCounterExpr counterName symbolId startExpr, bindLoopCounterExpr counterName symbolId endExpr, bindLoopCounterExpr counterName symbolId stepExpr, body, pos)
    | ForSequence(loopId, innerSymbolId, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, body, pos) ->
        ForSequence(loopId, innerSymbolId, prefixExprs |> List.map (bindLoopCounterExpr counterName symbolId), bindLoopCounterExpr counterName symbolId startExpr, bindLoopCounterExpr counterName symbolId endExpr, suffixExprs |> List.map (bindLoopCounterExpr counterName symbolId), bindLoopCounterExpr counterName symbolId stepExpr, body, pos)
    | Repeat(loopId, loopName, body, pos) ->
        Repeat(loopId, loopName, bindLoopCounterBlock counterName symbolId body, pos)
    | WhenError(body, pos) ->
        WhenError(bindLoopCounterBlock counterName symbolId body, pos)
    | Goto(target, pos) ->
        Goto(bindLoopCounterExpr counterName symbolId target, pos)
    | OnGoto(selector, targets, pos) ->
        OnGoto(bindLoopCounterExpr counterName symbolId selector, targets |> List.map (bindLoopCounterExpr counterName symbolId), pos)
    | Gosub(target, pos) ->
        Gosub(bindLoopCounterExpr counterName symbolId target, pos)
    | OnGosub(selector, targets, pos) ->
        OnGosub(bindLoopCounterExpr counterName symbolId selector, targets |> List.map (bindLoopCounterExpr counterName symbolId), pos)
    | Return(Some valueExpr, pos) ->
        Return(Some(bindLoopCounterExpr counterName symbolId valueExpr), pos)
    | Restore(Some valueExpr, pos) ->
        Restore(Some(bindLoopCounterExpr counterName symbolId valueExpr), pos)
    | Read(targets, pos) ->
        Read(targets |> List.map (bindLoopCounterTarget counterName symbolId), pos)
    | other -> other

and private bindLoopCounterBlock (counterName: string) (symbolId: SymbolId) block =
    block |> List.map (bindLoopCounterStmt counterName symbolId)

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

let private lowerParameterBinding = function
    | ByReference -> ReferenceBinding
    | Flexible -> FlexibleBinding

let private canLowerDynamically ctx name =
    ctx.CurrentScope <> globalScope
    && not (isCallableBuiltIn name)

let private shouldLowerDynamicStorage ctx expr name =
    match getRecordedResolvedSymbol ctx expr with
    | None -> canLowerDynamically ctx name
    | Some resolved when canLowerDynamically ctx name && resolved.Scope = globalScope ->
        match Map.tryFind resolved.Scope ctx.State.SymTab with
        | Some scope ->
            match Map.tryFind resolved.Name scope.Symbols with
            | Some(VariableSym _)
            | Some(ArraySym _)
            | Some(ConstantSym _) -> true
            | _ -> false
        | None -> false
    | _ -> false

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
          Dimensions =
              match symbol with
              | ArraySym arr -> Some arr.Dimensions
              | _ -> None
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

let private tryParseHexInt (value: string) =
    if value.StartsWith("$") && value.Length > 1 then
        match System.UInt32.TryParse(value[1..], NumberStyles.AllowHexSpecifier, CultureInfo.InvariantCulture) with
        | true, parsed -> Some(int (int32 parsed))
        | _ -> None
    else
        None

let private tryParseDouble (value: string) =
    match tryParseHexInt value with
    | Some integer -> true, float integer
    | None -> System.Double.TryParse(value, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture)

let private unquoteString (value: string) =
    if value.Length >= 2 && value.StartsWith("\"") && value.EndsWith("\"") then
        value.Substring(1, value.Length - 2).Replace("\"\"", "\"")
    elif value.Length >= 2 && value.StartsWith("'") && value.EndsWith("'") then
        value.Substring(1, value.Length - 2).Replace("''", "'")
    else
        value

let private lowerLiteral (pos: SourcePosition) (value: string) =
    match tryParseHexInt value with
    | Some n -> Literal(HirConst.ConstInt n, HirType.Int, pos)
    | None ->
        match System.Int32.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | true, n -> Literal(HirConst.ConstInt n, HirType.Int, pos)
        | _ when (value.StartsWith("\"") && value.EndsWith("\"")) || (value.StartsWith("'") && value.EndsWith("'")) ->
            Literal(HirConst.ConstString (unquoteString value), HirType.String, pos)
        | _ ->
            match tryParseDouble value with
            | true, n -> Literal(HirConst.ConstFloat n, HirType.Float, pos)
            | _ -> Literal(HirConst.ConstString value, HirType.String, pos)

let private unsupported ctx pos construct =
    fail ctx.CurrentScope (Some pos) $"HIR lowering does not support {construct} yet."

let private tryLookupResolvedSymbol ctx expr =
    match getRecordedResolvedSymbol ctx expr with
    | Some resolved ->
        Map.tryFind resolved.Scope ctx.State.SymTab
        |> Option.bind (fun scope -> Map.tryFind resolved.Name scope.Symbols)
    | None -> None

let private isSingleStringIndexAccess ctx expr (args: Expr list) =
    args.Length = 1
    &&
    match tryLookupResolvedSymbol ctx expr with
    | Some(VariableSym _ | ConstantSym _ | ParameterSym _) ->
        match getRecordedExprType ctx expr with
        | Result.Ok HirType.String -> true
        | _ -> false
    | _ -> false

let rec private lowerExpr ctx expr =
    match expr with
    | NumberLiteral(_, pos, value) -> ok (lowerLiteral pos value)
    | StringLiteral(_, pos, value) -> ok (lowerLiteral pos value)
    | Identifier(_, pos, name) ->
        match getRecordedResolvedSymbol ctx expr with
        | _ when shouldLowerDynamicStorage ctx expr name ->
            getRecordedExprType ctx expr |> map (fun hirType -> DynamicReadVar(normalizeIdentifier name, hirType, pos))
        | _ ->
            zip (getRecordedExprType ctx expr) (requireSymbolIdForExpr ctx expr name pos)
            |> bind (fun (hirType, symbolId) ->
                match getRecordedResolvedSymbol ctx expr with
                | Some resolved when isCallableBuiltIn resolved.Name -> ok (CallFunc(symbolId, [], hirType, pos))
                | Some resolved ->
                    match Map.tryFind resolved.Scope ctx.State.SymTab with
                    | Some scope ->
                        match Map.tryFind resolved.Name scope.Symbols with
                        | Some(FunctionSym _)
                        | Some(BuiltInSym _) -> ok (CallFunc(symbolId, [], hirType, pos))
                        | _ -> ok (ReadVar(symbolId, hirType, pos))
                    | None -> ok (ReadVar(symbolId, hirType, pos))
                | _ when normalizeIdentifier name = "DATE" ->
                    fail ctx.CurrentScope (Some pos) "DATE is about to lower as ReadVar."
                | _ -> ok (ReadVar(symbolId, hirType, pos)))
    | PostfixName(_, pos, name, None) ->
        match getRecordedResolvedSymbol ctx expr with
        | _ when shouldLowerDynamicStorage ctx expr name ->
            getRecordedExprType ctx expr |> map (fun hirType -> DynamicReadVar(normalizeIdentifier name, hirType, pos))
        | _ ->
            zip (getRecordedExprType ctx expr) (requireSymbolIdForExpr ctx expr name pos)
            |> bind (fun (hirType, symbolId) ->
                match getRecordedResolvedSymbol ctx expr with
                | Some resolved when isCallableBuiltIn resolved.Name -> ok (CallFunc(symbolId, [], hirType, pos))
                | Some resolved ->
                    match Map.tryFind resolved.Scope ctx.State.SymTab with
                    | Some scope ->
                        match Map.tryFind resolved.Name scope.Symbols with
                        | Some(FunctionSym _)
                        | Some(BuiltInSym _) -> ok (CallFunc(symbolId, [], hirType, pos))
                        | _ -> ok (ReadVar(symbolId, hirType, pos))
                    | None -> ok (ReadVar(symbolId, hirType, pos))
                | _ when normalizeIdentifier name = "DATE" ->
                    fail ctx.CurrentScope (Some pos) "DATE is about to lower as ReadVar."
                | _ -> ok (ReadVar(symbolId, hirType, pos)))
    | PostfixName(_, pos, name, Some args) ->
        match getRecordedResolvedSymbol ctx expr with
        | _ when shouldLowerDynamicStorage ctx expr name ->
            if args.Length = 1 then
                zip (getRecordedExprType ctx expr) (lowerExpr ctx args.Head)
                |> map (fun (hirType, loweredIndex) ->
                    if hirType = HirType.String then
                        DynamicReadStringChar(normalizeIdentifier name, loweredIndex, hirType, pos)
                    else
                        DynamicReadArrayElem(normalizeIdentifier name, [ loweredIndex ], hirType, pos))
            else
                zip (getRecordedExprType ctx expr) (args |> List.map (lowerExpr ctx) |> collectResults)
                |> map (fun (hirType, loweredIndexes) -> DynamicReadArrayElem(normalizeIdentifier name, loweredIndexes, hirType, pos))
        | _ ->
            zip (getRecordedExprType ctx expr) (zip (requireSymbolIdForExpr ctx expr name pos) (args |> List.map (fun arg -> lowerExpr ctx arg |> map ValueArg) |> collectResults))
            |> map (fun (hirType, (symbolId, loweredArgs)) ->
                match getRecordedResolvedSymbol ctx expr with
                | Some resolved when isCallableBuiltIn resolved.Name -> CallFunc(symbolId, loweredArgs, hirType, pos)
                | Some resolved ->
                    match Map.tryFind resolved.Scope ctx.State.SymTab with
                    | Some scope ->
                        match Map.tryFind resolved.Name scope.Symbols with
                        | Some(ArraySym _) ->
                            let loweredIndexes =
                                loweredArgs
                                |> List.choose (function | ValueArg arg -> Some arg | RefArg _ -> None)
                            ReadArrayElem(symbolId, loweredIndexes, hirType, pos)
                        | Some(VariableSym _)
                        | Some(ConstantSym _)
                        | Some(ParameterSym _) when isSingleStringIndexAccess ctx expr args ->
                            let loweredIndex =
                                loweredArgs
                                |> List.choose (function | ValueArg arg -> Some arg | RefArg _ -> None)
                                |> List.head
                            ReadStringChar(symbolId, loweredIndex, hirType, pos)
                        | Some(FunctionSym _)
                        | Some(BuiltInSym _) -> CallFunc(symbolId, loweredArgs, hirType, pos)
                        | _ ->
                            let loweredIndexes =
                                loweredArgs
                                |> List.choose (function | ValueArg arg -> Some arg | RefArg _ -> None)
                            ReadArrayElem(symbolId, loweredIndexes, hirType, pos)
                    | None ->
                        let loweredIndexes =
                            loweredArgs
                            |> List.choose (function | ValueArg arg -> Some arg | RefArg _ -> None)
                        ReadArrayElem(symbolId, loweredIndexes, hirType, pos)
                | _ ->
                    let loweredIndexes =
                        loweredArgs
                        |> List.choose (function | ValueArg arg -> Some arg | RefArg _ -> None)
                    ReadArrayElem(symbolId, loweredIndexes, hirType, pos))
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
        match getRecordedResolvedSymbol ctx expr with
        | _ when shouldLowerDynamicStorage ctx expr name ->
            getRecordedTargetType ctx expr |> map (fun hirType -> DynamicWriteVar(normalizeIdentifier name, hirType, pos))
        | _ ->
            zip (getRecordedTargetType ctx expr) (requireSymbolIdForExpr ctx expr name pos)
            |> map (fun (hirType, symbolId) -> WriteVar(symbolId, hirType, pos))
    | PostfixName(_, pos, name, Some args) ->
        match getRecordedResolvedSymbol ctx expr with
        | _ when shouldLowerDynamicStorage ctx expr name ->
            if args.Length = 1 then
                zip (getRecordedTargetType ctx expr) (lowerExpr ctx args.Head)
                |> map (fun (hirType, loweredIndex) ->
                    if hirType = HirType.String then
                        DynamicWriteStringChar(normalizeIdentifier name, loweredIndex, hirType, pos)
                    else
                        DynamicWriteArrayElem(normalizeIdentifier name, [ loweredIndex ], hirType, pos))
            else
                zip (getRecordedTargetType ctx expr) (args |> List.map (lowerExpr ctx) |> collectResults)
                |> map (fun (hirType, loweredArgs) -> DynamicWriteArrayElem(normalizeIdentifier name, loweredArgs, hirType, pos))
        | _ ->
            if isSingleStringIndexAccess ctx expr args then
                zip (getRecordedTargetType ctx expr) (zip (requireSymbolIdForExpr ctx expr name pos) (lowerExpr ctx args.Head))
                |> map (fun (hirType, (symbolId, loweredIndex)) -> WriteStringChar(symbolId, loweredIndex, hirType, pos))
            else
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

let private lowerCallArg ctx expr =
    match expr, tryLookupResolvedSymbol ctx expr with
    | (Identifier(_, pos, name) | PostfixName(_, pos, name, None)), Some(VariableSym _ | ParameterSym _) ->
        zip (getRecordedExprType ctx expr) (requireSymbolIdForExpr ctx expr name pos)
        |> map (fun (hirType, symbolId) -> RefArg(WriteVar(symbolId, hirType, pos)))
    | PostfixName(_, pos, name, Some args), Some(VariableSym _ | ParameterSym _) when isSingleStringIndexAccess ctx expr args ->
        zip (getRecordedExprType ctx expr) (zip (requireSymbolIdForExpr ctx expr name pos) (lowerExpr ctx args.Head))
        |> map (fun (hirType, (symbolId, loweredIndex)) -> RefArg(WriteStringChar(symbolId, loweredIndex, hirType, pos)))
    | PostfixName(_, pos, name, Some args), Some(ArraySym _) ->
        zip (getRecordedExprType ctx expr) (zip (requireSymbolIdForExpr ctx expr name pos) (args |> List.map (lowerExpr ctx) |> collectResults))
        |> map (fun (hirType, (symbolId, loweredArgs)) -> RefArg(WriteArrayElem(symbolId, loweredArgs, hirType, pos)))
    | _ ->
        lowerExpr ctx expr |> map ValueArg

let private lowerCallArgList ctx exprs =
    exprs |> List.map (lowerCallArg ctx) |> collectResults

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
    | ImplicitStmt _
    | ManifestStmt _ -> ok []
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
            if isCallableBuiltIn name then
                lowerExprList ctx (normalizeBuiltInArgs name args)
                |> map (fun loweredArgs -> [ BuiltInCall(lowerBuiltInKind name, None, loweredArgs, pos) ])
            else
                lowerCallArgList ctx args
                |> bind (fun loweredArgs ->
                    requireSymbolIdForName ctx ctx.CurrentScope name pos
                    |> map (fun symbolId -> [ ProcCall(symbolId, None, loweredArgs, pos) ]))
    | ChannelProcedureCall(pos, name, channel, args) ->
        lowerExpr ctx channel
        |> bind (fun loweredChannel ->
            if normalizeIdentifier name = "INPUT" then
                lowerInputArgs ctx pos args
                |> map (fun (prompts, loweredTargets) -> [ Input(Some(ExplicitChannel loweredChannel), prompts, loweredTargets, pos) ])
            elif isCallableBuiltIn name then
                lowerExprList ctx (normalizeBuiltInArgs name args)
                |> map (fun loweredArgs -> [ BuiltInCall(lowerBuiltInKind name, Some(ExplicitChannel loweredChannel), loweredArgs, pos) ])
            else
                lowerCallArgList ctx args
                |> bind (fun loweredArgs ->
                    requireSymbolIdForName ctx ctx.CurrentScope name pos
                    |> map (fun symbolId -> [ ProcCall(symbolId, Some(ExplicitChannel loweredChannel), loweredArgs, pos) ])))
    | ImplicitChannelProcedureCall(pos, name, channel, args) ->
        lowerExpr ctx channel
        |> bind (fun loweredChannel ->
            if normalizeIdentifier name = "INPUT" then
                lowerInputArgs ctx pos args
                |> map (fun (prompts, loweredTargets) -> [ Input(Some(ImplicitChannel loweredChannel), prompts, loweredTargets, pos) ])
            elif isCallableBuiltIn name then
                lowerExprList ctx (normalizeBuiltInArgs name args)
                |> map (fun loweredArgs -> [ BuiltInCall(lowerBuiltInKind name, Some(ImplicitChannel loweredChannel), loweredArgs, pos) ])
            else
                lowerCallArgList ctx args
                |> bind (fun loweredArgs ->
                    requireSymbolIdForName ctx ctx.CurrentScope name pos
                    |> map (fun symbolId -> [ ProcCall(symbolId, Some(ImplicitChannel loweredChannel), loweredArgs, pos) ])))
    | ForStmt(pos, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, body, _) ->
        let loopId = nextLoopId ctx
        let loopCtx = withLoop ctx (NamedLoop name) loopId
        let loweredStep =
            match stepExpr with
            | Some expr -> lowerExpr ctx expr
            | None -> ok (Literal(HirConst.ConstInt 1, HirType.Int, pos))

        zip (zip (zip (lowerExprList ctx prefixExprs) (lowerExpr ctx startExpr)) (zip (lowerExpr ctx endExpr) (lowerExprList ctx suffixExprs))) loweredStep
        |> bind (fun ((((loweredPrefix, loweredStart), (loweredEnd, loweredSuffix))), loweredStepExpr) ->
            lowerBlock loopCtx body
            |> bind (fun loweredBody ->
                requireSymbolIdForName ctx ctx.CurrentScope name pos
                    |> map (fun symbolId ->
                        let boundPrefix = loweredPrefix |> List.map (bindLoopCounterExpr name symbolId)
                        let boundStart = bindLoopCounterExpr name symbolId loweredStart
                        let boundEnd = bindLoopCounterExpr name symbolId loweredEnd
                        let boundSuffix = loweredSuffix |> List.map (bindLoopCounterExpr name symbolId)
                        let boundStep = bindLoopCounterExpr name symbolId loweredStepExpr
                        let boundBody = bindLoopCounterBlock name symbolId loweredBody

                        if List.isEmpty boundPrefix && List.isEmpty boundSuffix then
                            [ For(loopId, symbolId, boundStart, boundEnd, boundStep, boundBody, pos) ]
                        else
                            [ ForSequence(loopId, symbolId, boundPrefix, boundStart, boundEnd, boundSuffix, boundStep, boundBody, pos) ])))
    | RepeatStmt(pos, label, body, _) ->
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
            | None -> [ WhenError(loweredBody, pos) ])
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
    let buildRoutine pos name parameters body returnType endLineNumber =
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
                    | Some storage ->
                        match Map.tryFind resolved.Scope state.SymTab with
                        | Some scope ->
                            match Map.tryFind resolved.Name scope.Symbols with
                            | Some(ParameterSym symbol) ->
                                Result.Ok { Storage = storage; Binding = lowerParameterBinding symbol.Passing }
                            | Some _ -> fail name (Some pos) $"Resolved parameter '{parameter}' was not a parameter symbol in routine '{name}'."
                            | None -> fail name (Some pos) $"Missing parameter symbol '{parameter}' in routine '{name}'."
                        | None -> fail name (Some pos) $"Missing parameter scope '{resolved.Scope}' while lowering routine '{name}'."
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
                          EndLineNumber = endLineNumber
                          Position = pos }))
            | None -> fail name (Some pos) $"Unable to resolve routine symbol id for '{name}'."
        | None -> fail name (Some pos) $"Unable to resolve routine '{name}'."

    match routine with
    | ProcedureDef(pos, name, parameters, body, _, endLineNumber) ->
        buildRoutine pos name parameters body None endLineNumber
    | FunctionDef(pos, name, parameters, body, _, endLineNumber) ->
        let returnType =
            match tryResolveSymbol globalScope name state.SymTab with
            | Some(_, FunctionSym symbol) -> Some(lowerSBType symbol.ReturnType)
            | _ -> Some HirType.Void

        buildRoutine pos name parameters body returnType endLineNumber
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
    let rec collectLines ((slotIndex, entries, restorePoints): int * HirDataEntry list * HirRestorePoint list) currentLines =
        currentLines
        |> List.fold (fun acc line ->
            acc |> bind (fun state -> collectLine state line))
            (ok (slotIndex, entries, restorePoints))

    and collectBlock (state: int * HirDataEntry list * HirRestorePoint list) block =
        match block with
        | StatementBlock stmts -> collectStmts state (None: int option) stmts
        | LineBlock blockLines -> collectLines state blockLines

    and collectStmt ((slotIndex, entries, restorePoints): int * HirDataEntry list * HirRestorePoint list) (currentLineNumber: int option) stmt =
        match stmt with
        | DataStmt(pos, exprs) ->
            let restorePoints =
                match currentLineNumber with
                | Some n when not (restorePoints |> List.exists (fun point -> point.LineNumber = n)) ->
                    restorePoints @ [ { LineNumber = n; Slot = DataSlotId slotIndex } ]
                | _ -> restorePoints

            lowerExprList ctx exprs
            |> map (fun lowered ->
                let newEntries =
                    lowered
                    |> List.mapi (fun offset value ->
                        { Slot = DataSlotId(slotIndex + offset)
                          Value = value
                          Position = pos
                          LineNumber = currentLineNumber })

                slotIndex + lowered.Length, entries @ newEntries, restorePoints)
        | ProcedureDef(_, _, _, body, _, _)
        | FunctionDef(_, _, _, body, _, _) ->
            collectLines (slotIndex, entries, restorePoints) body
        | ForStmt(_, _, _, _, _, _, _, body, _)
        | RepeatStmt(_, _, body, _) ->
            collectBlock (slotIndex, entries, restorePoints) body
        | IfStmt(_, _, thenBlock, elseBlock) ->
            collectBlock (slotIndex, entries, restorePoints) thenBlock
            |> bind (fun nextState ->
                match elseBlock with
                | Some block -> collectBlock nextState block
                | None -> ok nextState)
        | SelectStmt(_, _, clauses) ->
            clauses
            |> List.fold (fun acc (SelectClause(_, _, _, body)) ->
                acc
                |> bind (fun nextState ->
                    match body with
                    | Some block -> collectBlock nextState block
                    | None -> ok nextState))
                (ok (slotIndex, entries, restorePoints))
        | WhenStmt(_, _, body) ->
            collectLines (slotIndex, entries, restorePoints) body
        | _ -> ok (slotIndex, entries, restorePoints)

    and collectStmts (state: int * HirDataEntry list * HirRestorePoint list) (currentLineNumber: int option) stmts =
        stmts
        |> List.fold (fun acc stmt ->
            acc |> bind (fun nextState -> collectStmt nextState currentLineNumber stmt))
            (ok state)

    and collectLine (state: int * HirDataEntry list * HirRestorePoint list) (Line(_, lineNumber, stmts)) =
        collectStmts state lineNumber stmts

    collectLines (0, [], []) lines
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
