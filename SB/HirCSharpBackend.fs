module HirCSharpBackend

open System
open System.Globalization
open System.Text

open HIR

type private EmitterContext = {
    ClassName: string
    SymbolNames: Map<SymbolId, string>
    StorageNames: Map<SymbolId, string>
    Storages: Map<SymbolId, HirStorage>
    RoutineNames: Map<SymbolId, string>
    Routines: Map<SymbolId, HirRoutine>
    RoutineSymbols: Set<SymbolId>
    NextStatementId: int ref
    NextGosubReturnId: int ref
    NextWhenErrorId: int ref
    ProgramHasLineControlFlow: bool
    ProgramHasWhenError: bool
    FlatLoopIds: Set<LoopId>
    FlatLoopLineOwners: Map<int, LoopId>
    DispatchLineName: string
    DispatchTargetName: string
    DispatchLabelName: string
}

let cSharpRuntimeFileName = "GeneratedRuntime.cs"

let private indent level = String.replicate (level * 4) " "

let private appendLine (builder: StringBuilder) level (text: string) =
    builder.Append(indent level).Append(text).AppendLine() |> ignore

let private sanitizeIdentifier (value: string) =
    let normalized =
        value
        |> Seq.map (fun ch -> if Char.IsLetterOrDigit ch || ch = '_' then ch else '_')
        |> Seq.toArray
        |> System.String

    if String.IsNullOrWhiteSpace normalized then
        "_"
    elif Char.IsDigit normalized[0] then
        "_" + normalized
    else
        normalized

let private symbolBaseName (symbolNames: Map<SymbolId, string>) symbolId =
    Map.tryFind symbolId symbolNames
    |> Option.defaultValue "symbol"
    |> sanitizeIdentifier
    |> fun value -> if String.IsNullOrWhiteSpace value then "symbol" else value

let private storageFieldName (symbolNames: Map<SymbolId, string>) (storage: HirStorage) =
    let (SymbolId id) = storage.Symbol
    $"v{id}_{symbolBaseName symbolNames storage.Symbol}"

let private routineMethodName (symbolNames: Map<SymbolId, string>) (routine: HirRoutine) =
    let (SymbolId id) = routine.Symbol
    let safeName =
        routine.Name
        |> sanitizeIdentifier
        |> fun value -> if String.IsNullOrWhiteSpace value then symbolBaseName symbolNames routine.Symbol else value
    $"r{id}_{safeName}"

let private tryGetConstInt = function
    | Literal(ConstInt value, _, _) -> Some value
    | _ -> None

let private hirTypeToken = function
    | HirType.Int -> "\"int\""
    | HirType.Float -> "\"float\""
    | HirType.String -> "\"string\""
    | HirType.Void -> "\"void\""
    | HirType.Array _ -> "\"array\""

let rec private blockHasLineControlFlow (block: HirBlock) =
    block
    |> List.exists (function
        | Goto _
        | Gosub _
        | OnGoto _
        | OnGosub _ -> true
        | If(_, thenBlock, elseBlock, _) ->
            blockHasLineControlFlow thenBlock
            || (elseBlock |> Option.exists blockHasLineControlFlow)
        | WhenError(body, _) -> blockHasLineControlFlow body
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> blockHasLineControlFlow body
        | _ -> false)

let rec private blockHasWhenError (block: HirBlock) =
    block
    |> List.exists (function
        | WhenError _ -> true
        | If(_, thenBlock, elseBlock, _) ->
            blockHasWhenError thenBlock
            || (elseBlock |> Option.exists blockHasWhenError)
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> blockHasWhenError body
        | _ -> false)

let rec private blockContainsLineNumbers (block: HirBlock) =
    block
    |> List.exists (function
        | LineNumber _ -> true
        | If(_, thenBlock, elseBlock, _) ->
            blockContainsLineNumbers thenBlock
            || (elseBlock |> Option.exists blockContainsLineNumbers)
        | WhenError(body, _) -> blockContainsLineNumbers body
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> blockContainsLineNumbers body
        | _ -> false)

let rec private collectLineNumbersInBlockIncludingHandlers (block: HirBlock) =
    block
    |> List.collect (function
        | LineNumber(value, _) -> [ value ]
        | If(_, thenBlock, elseBlock, _) ->
            collectLineNumbersInBlockIncludingHandlers thenBlock
            @ (elseBlock |> Option.map collectLineNumbersInBlockIncludingHandlers |> Option.defaultValue [])
        | WhenError(handlerBlock, _) -> collectLineNumbersInBlockIncludingHandlers handlerBlock
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> collectLineNumbersInBlockIncludingHandlers body
        | _ -> [])
    |> List.distinct

let private buildContext className (program: HirProgram) =
    let programHasWhenError =
        blockHasWhenError program.Main
        || (program.Routines |> List.exists (fun routine -> blockHasWhenError routine.Body))
    let programHasLineControlFlow =
        blockHasLineControlFlow program.Main
        || (program.Routines |> List.exists (fun routine -> blockHasLineControlFlow routine.Body))
        || programHasWhenError

    let globals =
        program.Globals
        |> List.map (fun storage -> storage.Symbol, storageFieldName program.SymbolNames storage)

    let routineStorage =
        program.Routines
        |> List.collect (fun routine ->
            ((routine.Parameters |> List.map _.Storage) @ routine.Locals)
            |> List.map (fun storage -> storage.Symbol, storageFieldName program.SymbolNames storage))

    let routines =
        program.Routines
        |> List.map (fun routine -> routine.Symbol, routineMethodName program.SymbolNames routine)

    let storages =
        (program.Globals @ (program.Routines |> List.collect (fun routine -> (routine.Parameters |> List.map _.Storage) @ routine.Locals)))
        |> List.map (fun storage -> storage.Symbol, storage)
        |> Map.ofList

    { ClassName = if String.IsNullOrWhiteSpace className then "GeneratedProgram" else sanitizeIdentifier className
      SymbolNames = program.SymbolNames
      StorageNames = Map.ofList (globals @ routineStorage)
      Storages = storages
      RoutineNames = Map.ofList routines
      Routines = program.Routines |> List.map (fun routine -> routine.Symbol, routine) |> Map.ofList
      RoutineSymbols = routines |> List.map fst |> Set.ofList
      NextStatementId = ref 0
      NextGosubReturnId = ref 0
      NextWhenErrorId = ref 0
      ProgramHasLineControlFlow = programHasLineControlFlow
      ProgramHasWhenError = programHasWhenError
      FlatLoopIds = Set.empty
      FlatLoopLineOwners = Map.empty
      DispatchLineName = "__dispatchLine"
      DispatchTargetName = "__targetLine"
      DispatchLabelName = "__dispatch" }

let private storageName ctx symbolId =
    Map.tryFind symbolId ctx.StorageNames
    |> Option.defaultValue (let (SymbolId id) = symbolId in $"v{id}_{symbolBaseName ctx.SymbolNames symbolId}")

let private routineName ctx symbolId =
    Map.tryFind symbolId ctx.RoutineNames
    |> Option.defaultValue (let (SymbolId id) = symbolId in $"r{id}_{symbolBaseName ctx.SymbolNames symbolId}")

let private builtInName ctx symbolId =
    Map.tryFind symbolId ctx.SymbolNames
    |> Option.defaultValue "BUILTIN"

let private storageInfo ctx symbolId =
    Map.find symbolId ctx.Storages

let private storageSourceName ctx symbolId =
    (storageInfo ctx symbolId).Name

let private escapeStringLiteral (value: string) =
    value.Replace("\\", "\\\\").Replace("\"", "\\\"")

let private emitInvocation targetName args =
    match args with
    | [] -> $"{targetName}()"
    | _ ->
        let argsText = String.concat ", " args
        $"{targetName}({argsText})"

let rec private emitTargetRead ctx = function
    | WriteVar(symbolId, _, _) -> storageName ctx symbolId
    | WriteArrayElem(symbolId, indexes, _, _) ->
        emitInvocation "GetArrayCell" (storageName ctx symbolId :: (indexes |> List.map (emitExpr ctx)))
    | WriteStringChar(_, _, _, _) ->
        "InvalidReferenceActualCell(\"String character targets cannot be used as by-reference storage locations.\")"
    | DynamicWriteVar(name, _, _) -> emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
    | DynamicWriteArrayElem(name, indexes, _, _) ->
        emitInvocation "GetArrayCell" (emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ] :: (indexes |> List.map (emitExpr ctx)))
    | DynamicWriteStringChar(_, _, _, _) ->
        "InvalidReferenceActualCell(\"String character targets cannot be used as by-reference storage locations.\")"

and private emitRoutineCallArg ctx = function
    | ValueArg expr -> $"new Cell({emitExpr ctx expr})"
    | RefArg target -> emitTargetRead ctx target

and private emitBuiltInCallArg ctx = function
    | ValueArg expr -> emitExpr ctx expr
    | RefArg(WriteStringChar(_, _, _, _))
    | RefArg(DynamicWriteStringChar(_, _, _, _)) ->
        "InvalidReferenceActualValue(\"String character targets cannot be used as by-reference storage locations.\")"
    | RefArg target -> $"{emitTargetRead ctx target}.Value"

and private emitBuiltInFunctionArgs ctx symbolId args =
    let normalizedName = (builtInName ctx symbolId).Trim().ToUpperInvariant()
    match normalizedName, args with
    | "DIMN", [ ValueArg(ReadVar(arraySymbol, HirType.Array _, _)); ValueArg dimensionExpr ] ->
        [ storageName ctx arraySymbol; emitExpr ctx dimensionExpr ]
    | _ ->
        args |> List.map (emitBuiltInCallArg ctx)

and private emitExpr ctx = function
    | Literal(ConstInt value, _, _) -> string value
    | Literal(ConstFloat value, _, _) -> value.ToString("G17", CultureInfo.InvariantCulture)
    | Literal(ConstString value, _, _) -> $"\"{escapeStringLiteral value}\""
    | ReadVar(symbolId, _, _) -> $"{storageName ctx symbolId}.Value"
    | ReadArrayElem(symbolId, indexes, _, _) ->
        emitInvocation "GetArrayValue" (storageName ctx symbolId :: (indexes |> List.map (emitExpr ctx)))
    | ReadStringChar(symbolId, index, _, _) ->
        emitInvocation "GetStringCharValue" [ $"{storageName ctx symbolId}.Value"; emitExpr ctx index ]
    | DynamicReadVar(name, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        $"{cellExpr}.Value"
    | DynamicReadArrayElem(name, indexes, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        emitInvocation "GetArrayValue" (cellExpr :: (indexes |> List.map (emitExpr ctx)))
    | DynamicReadStringChar(name, index, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        emitInvocation "GetStringCharValue" [ $"{cellExpr}.Value"; emitExpr ctx index ]
    | Unary(op, inner, _, _) ->
        match op with
        | Identity -> $"Identity({emitExpr ctx inner})"
        | Negate -> $"Negate({emitExpr ctx inner})"
        | BitwiseNot -> $"BitwiseNot({emitExpr ctx inner})"
        | UnaryUnknown name -> $"ApplyUnary(\"{name}\", {emitExpr ctx inner})"
    | Binary(op, lhs, rhs, _, _) ->
        let left = emitExpr ctx lhs
        let right = emitExpr ctx rhs
        match op with
        | Add -> $"Add({left}, {right})"
        | Subtract -> $"Subtract({left}, {right})"
        | Multiply -> $"Multiply({left}, {right})"
        | Divide -> $"Divide({left}, {right})"
        | Power -> $"Power({left}, {right})"
        | Concat -> $"Concat({left}, {right})"
        | IntegerDivide -> $"IntegerDivide({left}, {right})"
        | Modulo -> $"Modulo({left}, {right})"
        | BitwiseAnd -> $"BitwiseAnd({left}, {right})"
        | BitwiseOr -> $"BitwiseOr({left}, {right})"
        | BitwiseXor -> $"BitwiseXor({left}, {right})"
        | Equal -> $"CompareEqual({left}, {right})"
        | NotEqual -> $"CompareNotEqual({left}, {right})"
        | LessThan -> $"CompareLessThan({left}, {right})"
        | LessThanOrEqual -> $"CompareLessThanOrEqual({left}, {right})"
        | GreaterThan -> $"CompareGreaterThan({left}, {right})"
        | GreaterThanOrEqual -> $"CompareGreaterThanOrEqual({left}, {right})"
        | Instr -> $"Instr({left}, {right})"
        | SliceRange -> $"SliceRange({left}, {right})"
        | BinaryUnknown name -> $"ApplyBinary(\"{name}\", {left}, {right})"
    | CallFunc(symbolId, args, _, _) ->
        let argsText =
            if Set.contains symbolId ctx.RoutineSymbols then
                args |> List.map (emitRoutineCallArg ctx)
            else
                emitBuiltInFunctionArgs ctx symbolId args
        if Set.contains symbolId ctx.RoutineSymbols then
            emitInvocation (routineName ctx symbolId) argsText
        else
            emitInvocation "InvokeBuiltInFunction" ($"\"{builtInName ctx symbolId}\"" :: argsText)

let private emitTargetWrite ctx target valueExpr =
    match target with
    | WriteVar(symbolId, _, _) -> $"{storageName ctx symbolId}.Value = {valueExpr};"
    | WriteArrayElem(symbolId, indexes, _, _) ->
        let invocation = emitInvocation "SetArrayValue" ([ storageName ctx symbolId; valueExpr ] @ (indexes |> List.map (emitExpr ctx)))
        invocation + ";"
    | WriteStringChar(symbolId, index, _, _) ->
        emitInvocation "SetStringCharValue" [ storageName ctx symbolId; emitExpr ctx index; valueExpr ] + ";"
    | DynamicWriteVar(name, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        $"{cellExpr}.Value = {valueExpr};"
    | DynamicWriteArrayElem(name, indexes, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        emitInvocation "SetArrayValue" ([ cellExpr; valueExpr ] @ (indexes |> List.map (emitExpr ctx))) + ";"
    | DynamicWriteStringChar(name, index, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        emitInvocation "SetStringCharValue" [ cellExpr; emitExpr ctx index; valueExpr ] + ";"

let private emitLoopTransfer loopId isNext =
    let (LoopId id) = loopId
    if isNext then
        $"throw new LoopControlException({id}, true);"
    else
        $"throw new LoopControlException({id}, false);"

let private nextStatementLabel ctx =
    let id = !ctx.NextStatementId
    ctx.NextStatementId := id + 1
    $"__stmt{id}"

let private nextGosubReturnId ctx =
    let id = !ctx.NextGosubReturnId
    ctx.NextGosubReturnId := id + 1
    id

let private nextWhenErrorId ctx =
    let id = !ctx.NextWhenErrorId
    ctx.NextWhenErrorId := id + 1
    id

let private nextSyntheticLabel ctx prefix =
    let id = !ctx.NextStatementId
    ctx.NextStatementId := id + 1
    $"{prefix}{id}"

let rec private countGosubSitesInBlock (block: HirBlock) =
    block
    |> List.sumBy (function
        | Gosub _ -> 1
        | OnGosub _ -> 1
        | If(_, thenBlock, elseBlock, _) ->
            countGosubSitesInBlock thenBlock
            + (elseBlock |> Option.map countGosubSitesInBlock |> Option.defaultValue 0)
        | WhenError(handlerBlock, _) -> countGosubSitesInBlock handlerBlock
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> countGosubSitesInBlock body
        | _ -> 0)

let rec private collectLineNumbersInBlock (block: HirBlock) =
    block
    |> List.collect (function
        | LineNumber(value, _) -> [ value ]
        | If(_, thenBlock, elseBlock, _) ->
            collectLineNumbersInBlock thenBlock
            @ (elseBlock |> Option.map collectLineNumbersInBlock |> Option.defaultValue [])
        | WhenError _ -> []
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> collectLineNumbersInBlock body
        | _ -> [])
    |> List.distinct

let rec private collectNestedLineNumbersInBlock depth (block: HirBlock) =
    block
    |> List.collect (function
        | LineNumber(value, _) when depth > 0 -> [ value ]
        | LineNumber _ -> []
        | If(_, thenBlock, elseBlock, _) ->
            collectNestedLineNumbersInBlock (depth + 1) thenBlock
            @ (elseBlock |> Option.map (collectNestedLineNumbersInBlock (depth + 1)) |> Option.defaultValue [])
        | WhenError(handlerBlock, _) -> collectNestedLineNumbersInBlock (depth + 1) handlerBlock
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> collectNestedLineNumbersInBlock (depth + 1) body
        | _ -> [])
    |> List.distinct

let rec private collectUnsupportedStructuredLineTargets (block: HirBlock) =
    block
    |> List.collect (function
        | If(_, thenBlock, elseBlock, _) ->
            collectUnsupportedStructuredLineTargets thenBlock
            @ (elseBlock |> Option.map collectUnsupportedStructuredLineTargets |> Option.defaultValue [])
        | For(_, _, _, _, _, body, _)
        | Repeat(_, _, body, _) ->
            collectUnsupportedStructuredLineTargets body
        | _ -> [])
    |> List.distinct

let private validateNestedLineNumberTargets scopeName hasLineControlFlow block =
    if hasLineControlFlow then
        let unsupportedTargets = collectUnsupportedStructuredLineTargets block
        if not unsupportedTargets.IsEmpty then
            let targetList = unsupportedTargets |> String.concat ", "
            invalidOp $"The generated C# backend cannot emit numbered line targets inside {targetList} in {scopeName}. Move these line numbers out of those constructs or use the interpreter backend instead."

let private findNextLineNumber block pc inheritedNextLine =
    block
    |> List.skip (pc + 1)
    |> List.tryPick (function
        | LineNumber(value, _) -> Some value
        | _ -> None)
    |> Option.orElse inheritedNextLine

let private emitLineLiteral = function
    | Some value -> string value
    | None -> "null"

let private emitTransferToLabel builder level continueLabelOpt returnText =
    match continueLabelOpt with
    | Some label -> appendLine builder level $"goto {label};"
    | None -> appendLine builder level returnText

let private emitDispatchToLine ctx builder level lineExpr =
    appendLine builder level $"{ctx.DispatchLineName} = {lineExpr};"
    appendLine builder level $"goto {ctx.DispatchLabelName};"

let private emitDispatcher ctx builder level lineNumbers exitLineNumber =
    appendLine builder level $"int? {ctx.DispatchLineName} = null;"
    appendLine builder level $"{ctx.DispatchLabelName}: ;"
    appendLine builder level $"if ({ctx.DispatchLineName}.HasValue)"
    appendLine builder level "{"
    appendLine builder (level + 1) $"var {ctx.DispatchTargetName} = {ctx.DispatchLineName}.Value;"
    appendLine builder (level + 1) $"{ctx.DispatchLineName} = null;"
    appendLine builder (level + 1) $"switch ({ctx.DispatchTargetName})"
    appendLine builder (level + 1) "{"
    lineNumbers
    |> List.iter (fun lineNumber ->
        match Map.tryFind lineNumber ctx.FlatLoopLineOwners with
        | Some(LoopId loopId) ->
            appendLine builder (level + 2) $"case {lineNumber}: __jumpedLoopId = {loopId}; goto line_{lineNumber};"
        | None when not ctx.FlatLoopIds.IsEmpty ->
            appendLine builder (level + 2) $"case {lineNumber}: __jumpedLoopId = null; goto line_{lineNumber};"
        | None ->
            appendLine builder (level + 2) $"case {lineNumber}: goto line_{lineNumber};")
    match exitLineNumber with
    | Some lineNumber when not (List.contains lineNumber lineNumbers) ->
        appendLine builder (level + 2) $"case {lineNumber}: return null;"
    | _ -> ()
    appendLine builder (level + 2) $"default: throw new InvalidOperationException($\"Target line {{{ctx.DispatchTargetName}}} does not exist.\");"
    appendLine builder (level + 1) "}"
    appendLine builder level "}"

let private emitBareReturnDispatch builder level returnText gosubReturnCount =
    if gosubReturnCount > 0 then
        appendLine builder level "if (__gosubStack.Count > 0)"
        appendLine builder level "{"
        appendLine builder (level + 1) "switch (__gosubStack.Pop())"
        appendLine builder (level + 1) "{"
        [ 0 .. gosubReturnCount - 1 ]
        |> List.iter (fun id ->
            appendLine builder (level + 2) $"case {id}: goto __gosub_return_{id};")
        appendLine builder (level + 2) "default: throw new InvalidOperationException(\"RETURN without an active GOSUB target.\");"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"
    appendLine builder level returnText

let rec private emitBlock ctx builder level currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling (block: HirBlock) =
    let stmtLabels = block |> List.map (fun _ -> nextStatementLabel ctx)

    let rec emitStatements index runningCurrentLine =
        if index < block.Length then
            let stmt = block[index]
            let stmtCurrentLine =
                match stmt with
                | LineNumber(value, _) -> Some value
                | _ -> runningCurrentLine
            let stmtNextLine = findNextLineNumber block index inheritedNextLine
            let nextLabelOpt =
                if index + 1 < stmtLabels.Length then Some stmtLabels[index + 1] else continueLabelOpt
            emitStmt ctx builder level stmtCurrentLine stmtNextLine nextLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling stmtLabels[index] stmt
            emitStatements (index + 1) stmtCurrentLine

    emitStatements 0 currentLine

and private emitIf ctx builder level currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling condition thenBlock elseBlock =
    let thenHasLineTargets = blockContainsLineNumbers thenBlock
    let elseHasLineTargets = elseBlock |> Option.exists blockContainsLineNumbers
    if thenHasLineTargets || elseHasLineTargets then
        let elseLabel = nextSyntheticLabel ctx "__if_else_"
        let endLabel = nextSyntheticLabel ctx "__if_end_"
        appendLine builder level $"if (!IsTrue({emitExpr ctx condition})) goto {(match elseBlock with | Some _ -> elseLabel | None -> endLabel)};"
        emitBlock ctx builder level currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling thenBlock
        match elseBlock with
        | Some branch ->
            appendLine builder level $"goto {endLabel};"
            appendLine builder level $"{elseLabel}: ;"
            emitBlock ctx builder level currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling branch
            appendLine builder level $"{endLabel}: ;"
        | None ->
            appendLine builder level $"{endLabel}: ;"
    else
        appendLine builder level $"if (IsTrue({emitExpr ctx condition}))"
        appendLine builder level "{"
        emitBlock ctx builder (level + 1) currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling thenBlock
        appendLine builder level "}"
        match elseBlock with
        | Some branch ->
            appendLine builder level "else"
            appendLine builder level "{"
            emitBlock ctx builder (level + 1) currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling branch
            appendLine builder level "}"
        | None -> ()

and private emitFor ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId symbolId startExpr endExpr stepExpr body =
    let (LoopId id) = loopId
    let counterName = storageName ctx symbolId
    let loopIndexName = $"__loop{id}Index"
    let endName = $"__loop{id}End"
    let stepName = $"__loop{id}Step"
    if Set.contains loopId ctx.FlatLoopIds then
        appendLine builder level $"__loop{id}Test: ;"
        appendLine builder level $"if (!__loop{id}Initialized)"
        appendLine builder level "{"
        appendLine builder (level + 1) $"{stepName} = AsInt({emitExpr ctx stepExpr});"
        appendLine builder (level + 1) $"{endName} = AsInt({emitExpr ctx endExpr});"
        appendLine builder (level + 1) $"{loopIndexName} = AsInt({emitExpr ctx startExpr});"
        appendLine builder (level + 1) $"__loop{id}Initialized = true;"
        appendLine builder level "}"
        appendLine builder level $"if (!({stepName} >= 0 ? {loopIndexName} <= {endName} : {loopIndexName} >= {endName})) goto __loop{id}Exit;"
        appendLine builder level $"__loop{id}Body: ;"
        appendLine builder level $"{counterName}.Value = {loopIndexName};"
        emitBlock ctx builder level currentLine inheritedNextLine None returnText gosubReturnCount lineNumbers enableErrorHandling body
        appendLine builder level $"if (__jumpedLoopId == {id})"
        appendLine builder level "{"
        appendLine builder (level + 1) "__jumpedLoopId = null;"
        appendLine builder (level + 1) $"__loop{id}Initialized = false;"
        appendLine builder (level + 1) returnText
        appendLine builder level "}"
        appendLine builder level $"__loop{id}Next: ;"
        appendLine builder level $"if (__jumpedLoopId == {id}) throw new InvalidOperationException(\"Loop control escaped the current scope after a numbered line jump.\");"
        appendLine builder level $"{loopIndexName} += {stepName};"
        appendLine builder level $"goto __loop{id}Test;"
        appendLine builder level $"__loop{id}Exit: ;"
        appendLine builder level $"if (__jumpedLoopId == {id}) throw new InvalidOperationException(\"Loop control escaped the current scope after a numbered line jump.\");"
        appendLine builder level $"__loop{id}Initialized = false;"
    else
        let exitLabel = $"__loop{id}Exit"
        appendLine builder level "{"
        appendLine builder (level + 1) $"var {stepName} = AsInt({emitExpr ctx stepExpr});"
        appendLine builder (level + 1) $"var {endName} = AsInt({emitExpr ctx endExpr});"
        appendLine builder (level + 1) $"for (var {loopIndexName} = AsInt({emitExpr ctx startExpr}); {stepName} >= 0 ? {loopIndexName} <= {endName} : {loopIndexName} >= {endName}; {loopIndexName} += {stepName})"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"{counterName}.Value = {loopIndexName};"
        appendLine builder (level + 2) "try"
        appendLine builder (level + 2) "{"
        emitBlock ctx builder (level + 3) currentLine inheritedNextLine None returnText gosubReturnCount lineNumbers enableErrorHandling body
        appendLine builder (level + 2) "}"
        appendLine builder (level + 2) "catch (LoopControlException ex)"
        appendLine builder (level + 2) "{"
        appendLine builder (level + 3) $"if (ex.LoopId != {id})"
        appendLine builder (level + 3) "{"
        appendLine builder (level + 4) "throw;"
        appendLine builder (level + 3) "}"
        appendLine builder (level + 3) "if (!ex.IsNext)"
        appendLine builder (level + 3) "{"
        appendLine builder (level + 4) "break;"
        appendLine builder (level + 3) "}"
        appendLine builder (level + 2) "}"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"

and private emitForSequence ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId symbolId prefixExprs startExpr endExpr suffixExprs stepExpr body =
    let (LoopId id) = loopId
    let counterName = storageName ctx symbolId
    let exitLabel = $"__loop{id}Exit"
    if Set.contains loopId ctx.FlatLoopIds then
        let loopIndexName = $"__loop{id}Index"
        let endName = $"__loop{id}End"
        let stepName = $"__loop{id}Step"
        let prefixValues = prefixExprs |> List.map (emitExpr ctx) |> String.concat ", "
        let suffixValues = suffixExprs |> List.map (emitExpr ctx) |> String.concat ", "
        appendLine builder level $"__loop{id}Test: ;"
        appendLine builder level $"if (!__loop{id}Initialized)"
        appendLine builder level "{"
        appendLine builder (level + 1) $"__loop{id}PrefixValues = new object?[] {{ {prefixValues} }};"
        appendLine builder (level + 1) $"__loop{id}PrefixIndex = 0;"
        appendLine builder (level + 1) $"__loop{id}SuffixIndex = 0;"
        appendLine builder (level + 1) $"__loop{id}Phase = 0;"
        appendLine builder (level + 1) $"__loop{id}RangeInitialized = false;"
        appendLine builder (level + 1) $"__loop{id}SuffixInitialized = false;"
        appendLine builder (level + 1) $"__loop{id}Initialized = true;"
        appendLine builder level "}"
        appendLine builder level $"__loop{id}HasValue = false;"
        appendLine builder level $"if (__loop{id}Phase == 0)"
        appendLine builder level "{"
        appendLine builder (level + 1) $"if (__loop{id}PrefixIndex < __loop{id}PrefixValues.Length)"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"__loop{id}Value = __loop{id}PrefixValues[__loop{id}PrefixIndex++];"
        appendLine builder (level + 2) $"__loop{id}HasValue = true;"
        appendLine builder (level + 1) "}"
        appendLine builder (level + 1) "else"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"__loop{id}Phase = 1;"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"
        appendLine builder level $"if (!__loop{id}HasValue && __loop{id}Phase == 1)"
        appendLine builder level "{"
        appendLine builder (level + 1) $"if (!__loop{id}RangeInitialized)"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"{stepName} = AsInt({emitExpr ctx stepExpr});"
        appendLine builder (level + 2) $"{endName} = AsInt({emitExpr ctx endExpr});"
        appendLine builder (level + 2) $"{loopIndexName} = AsInt({emitExpr ctx startExpr});"
        appendLine builder (level + 2) $"__loop{id}RangeInitialized = true;"
        appendLine builder (level + 1) "}"
        appendLine builder (level + 1) $"if ({stepName} >= 0 ? {loopIndexName} <= {endName} : {loopIndexName} >= {endName})"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"__loop{id}Value = {loopIndexName};"
        appendLine builder (level + 2) $"{loopIndexName} += {stepName};"
        appendLine builder (level + 2) $"__loop{id}HasValue = true;"
        appendLine builder (level + 1) "}"
        appendLine builder (level + 1) "else"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"__loop{id}Phase = 2;"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"
        appendLine builder level $"if (!__loop{id}HasValue && __loop{id}Phase == 2)"
        appendLine builder level "{"
        appendLine builder (level + 1) $"if (!__loop{id}SuffixInitialized)"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"__loop{id}SuffixValues = new object?[] {{ {suffixValues} }};"
        appendLine builder (level + 2) $"__loop{id}SuffixInitialized = true;"
        appendLine builder (level + 1) "}"
        appendLine builder (level + 1) $"if (__loop{id}SuffixIndex < __loop{id}SuffixValues.Length)"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"__loop{id}Value = __loop{id}SuffixValues[__loop{id}SuffixIndex++];"
        appendLine builder (level + 2) $"__loop{id}HasValue = true;"
        appendLine builder (level + 1) "}"
        appendLine builder (level + 1) "else"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"goto {exitLabel};"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"
        appendLine builder level $"{counterName}.Value = __loop{id}Value;"
        appendLine builder level $"__loop{id}Body: ;"
        emitBlock ctx builder level currentLine inheritedNextLine None returnText gosubReturnCount lineNumbers enableErrorHandling body
        appendLine builder level $"if (__jumpedLoopId == {id})"
        appendLine builder level "{"
        appendLine builder (level + 1) "__jumpedLoopId = null;"
        appendLine builder (level + 1) $"__loop{id}Initialized = false;"
        appendLine builder (level + 1) returnText
        appendLine builder level "}"
        appendLine builder level $"__loop{id}Next: ;"
        appendLine builder level $"if (__jumpedLoopId == {id}) throw new InvalidOperationException(\"Loop control escaped the current scope after a numbered line jump.\");"
        appendLine builder level $"goto __loop{id}Test;"
        appendLine builder level $"{exitLabel}: ;"
        appendLine builder level $"if (__jumpedLoopId == {id}) throw new InvalidOperationException(\"Loop control escaped the current scope after a numbered line jump.\");"
        appendLine builder level $"__loop{id}Initialized = false;"
    else
        appendLine builder level "{"
        appendLine builder (level + 1) $"bool __loop{id}Run(object? value)"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"{counterName}.Value = value;"
        appendLine builder (level + 2) "try"
        appendLine builder (level + 2) "{"
        emitBlock ctx builder (level + 3) currentLine inheritedNextLine None returnText gosubReturnCount lineNumbers enableErrorHandling body
        appendLine builder (level + 2) "}"
        appendLine builder (level + 2) "catch (LoopControlException ex)"
        appendLine builder (level + 2) "{"
        appendLine builder (level + 3) $"if (ex.LoopId != {id})"
        appendLine builder (level + 3) "{"
        appendLine builder (level + 4) "throw;"
        appendLine builder (level + 3) "}"
        appendLine builder (level + 3) "if (!ex.IsNext)"
        appendLine builder (level + 3) "{"
        appendLine builder (level + 4) "return false;"
        appendLine builder (level + 3) "}"
        appendLine builder (level + 2) "}"
        appendLine builder (level + 2) "return true;"
        appendLine builder (level + 1) "}"
        if not (List.isEmpty prefixExprs) then
            let prefixValues = prefixExprs |> List.map (emitExpr ctx) |> String.concat ", "
            appendLine builder (level + 1) $"foreach (var __loop{id}Prefix in new object?[] {{ {prefixValues} }})"
            appendLine builder (level + 1) "{"
            appendLine builder (level + 2) $"if (!__loop{id}Run(__loop{id}Prefix)) goto {exitLabel};"
            appendLine builder (level + 1) "}"
        let loopIndexName = $"__loop{id}Index"
        let endName = $"__loop{id}End"
        let stepName = $"__loop{id}Step"
        appendLine builder (level + 1) $"var {stepName} = AsInt({emitExpr ctx stepExpr});"
        appendLine builder (level + 1) $"var {endName} = AsInt({emitExpr ctx endExpr});"
        appendLine builder (level + 1) $"for (var {loopIndexName} = AsInt({emitExpr ctx startExpr}); {stepName} >= 0 ? {loopIndexName} <= {endName} : {loopIndexName} >= {endName}; {loopIndexName} += {stepName})"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"if (!__loop{id}Run({loopIndexName})) goto {exitLabel};"
        appendLine builder (level + 1) "}"
        if not (List.isEmpty suffixExprs) then
            let suffixValues = suffixExprs |> List.map (emitExpr ctx) |> String.concat ", "
            appendLine builder (level + 1) $"foreach (var __loop{id}Suffix in new object?[] {{ {suffixValues} }})"
            appendLine builder (level + 1) "{"
            appendLine builder (level + 2) $"if (!__loop{id}Run(__loop{id}Suffix)) goto {exitLabel};"
            appendLine builder (level + 1) "}"
        appendLine builder (level + 1) $"{exitLabel}: ;"
        appendLine builder level "}"

and private emitRepeat ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId body =
    let (LoopId id) = loopId
    if Set.contains loopId ctx.FlatLoopIds then
        appendLine builder level $"__loop{id}Body: ;"
        emitBlock ctx builder level currentLine inheritedNextLine None returnText gosubReturnCount lineNumbers enableErrorHandling body
        appendLine builder level $"if (__jumpedLoopId == {id})"
        appendLine builder level "{"
        appendLine builder (level + 1) "__jumpedLoopId = null;"
        appendLine builder (level + 1) returnText
        appendLine builder level "}"
        appendLine builder level $"__loop{id}Next: ;"
        appendLine builder level $"if (__jumpedLoopId == {id}) throw new InvalidOperationException(\"Loop control escaped the current scope after a numbered line jump.\");"
        appendLine builder level $"goto __loop{id}Body;"
        appendLine builder level $"__loop{id}Exit: ;"
        appendLine builder level $"if (__jumpedLoopId == {id}) throw new InvalidOperationException(\"Loop control escaped the current scope after a numbered line jump.\");"
    else
        appendLine builder level "while (true)"
        appendLine builder level "{"
        appendLine builder (level + 1) "try"
        appendLine builder (level + 1) "{"
        emitBlock ctx builder (level + 2) currentLine inheritedNextLine None returnText gosubReturnCount lineNumbers enableErrorHandling body
        appendLine builder (level + 1) "}"
        appendLine builder (level + 1) "catch (LoopControlException ex)"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) $"if (ex.LoopId != {id})"
        appendLine builder (level + 2) "{"
        appendLine builder (level + 3) "throw;"
        appendLine builder (level + 2) "}"
        appendLine builder (level + 2) "if (!ex.IsNext)"
        appendLine builder (level + 2) "{"
        appendLine builder (level + 3) "break;"
        appendLine builder (level + 2) "}"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"

and private emitOnGotoLike keyword ctx builder level selector targets =
    appendLine builder level $"switch (AsInt({emitExpr ctx selector}))"
    appendLine builder level "{"
    targets
    |> List.iteri (fun index target ->
        match tryGetConstInt target with
        | Some line when keyword = "GOTO" ->
            appendLine builder (level + 1) $"case {index + 1}:"
            emitDispatchToLine ctx builder (level + 2) (string line)
        | Some line ->
            appendLine builder (level + 1) $"case {index + 1}:"
            emitDispatchToLine ctx builder (level + 2) (string line)
        | None ->
            appendLine builder (level + 1) $"case {index + 1}:"
            emitDispatchToLine ctx builder (level + 2) $"AsInt({emitExpr ctx target})")
    appendLine builder (level + 1) "default: break;"
    appendLine builder level "}"

and private emitOnGosub ctx builder level selector targets =
    let returnId = nextGosubReturnId ctx
    appendLine builder level $"switch (AsInt({emitExpr ctx selector}))"
    appendLine builder level "{"
    targets
    |> List.iteri (fun index target ->
        match tryGetConstInt target with
        | Some line ->
            appendLine builder (level + 1) $"case {index + 1}:"
            appendLine builder (level + 2) $"__gosubStack.Push({returnId});"
            emitDispatchToLine ctx builder (level + 2) (string line)
        | None ->
            appendLine builder (level + 1) $"case {index + 1}:"
            appendLine builder (level + 2) $"__gosubStack.Push({returnId});"
            emitDispatchToLine ctx builder (level + 2) $"AsInt({emitExpr ctx target})")
    appendLine builder (level + 1) "default: break;"
    appendLine builder level "}"
    appendLine builder level $"__gosub_return_{returnId}: ;"

and private emitWhenError ctx builder level currentLine nextLine returnText gosubReturnCount lineNumbers handlerBody =
    let handlerId = nextWhenErrorId ctx
    let handlerLineNumbers = collectLineNumbersInBlockIncludingHandlers handlerBody
    let handlerHasLineControlFlow = blockHasLineControlFlow handlerBody
    let handlerCtx =
        { ctx with
            DispatchLineName = $"__whenError{handlerId}DispatchLine"
            DispatchTargetName = $"__whenError{handlerId}TargetLine"
            DispatchLabelName = $"__whenError{handlerId}Dispatch" }
    appendLine builder level $"ErrorAction __whenError{handlerId}(Exception __sbError)"
    appendLine builder level "{"
    appendLine builder (level + 1) $"RecordLastError(__sbError, {emitLineLiteral currentLine}, {emitLineLiteral currentLine}, {emitLineLiteral nextLine});"
    appendLine builder (level + 1) "__inErrorHandler = true;"
    appendLine builder (level + 1) "try"
    appendLine builder (level + 1) "{"
    if handlerHasLineControlFlow then
        emitDispatcher handlerCtx builder (level + 2) handlerLineNumbers None
    emitBlock handlerCtx builder (level + 2) currentLine nextLine None returnText gosubReturnCount handlerLineNumbers false handlerBody
    appendLine builder (level + 2) "throw new InvalidOperationException(\"WHEN ERROR handler must exit with RETRY, CONTINUE, or STOP.\");"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) "catch (RetryControlException ex)"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) "return ErrorAction.Retry(ex.LineNumber);"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) "catch (ContinueControlException ex)"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) "return ErrorAction.Continue(ex.LineNumber);"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) "catch (OperationCanceledException)"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) "return ErrorAction.Stop();"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) "finally"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) "__inErrorHandler = false;"
    appendLine builder (level + 1) "}"
    appendLine builder level "}"
    appendLine builder level $"__activeErrorHandler = __whenError{handlerId};"

and private emitStmtCore ctx builder level currentLine nextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling stmt =
    match stmt with
    | Assign(target, value, _) ->
        appendLine builder level (emitTargetWrite ctx target (emitExpr ctx value))
    | ProcCall(symbolId, _, args, _) ->
        let argsText = args |> List.map (emitRoutineCallArg ctx)
        appendLine builder level $"{emitInvocation (routineName ctx symbolId) argsText};"
    | BuiltInCall(kind, channel, args, _) ->
        let channelText =
            match channel with
            | Some(ExplicitChannel(expr: HirExpr))
            | Some(ImplicitChannel(expr: HirExpr)) -> emitExpr ctx expr
            | None -> "null"
        let argsText = args |> List.map (emitExpr ctx)
        let kindName =
            match kind with
            | Reference -> "REFERENCE"
            | BuiltInKind.Input -> "INPUT"
            | Print -> "PRINT"
            | GotoBuiltIn -> "GOTO"
            | GosubBuiltIn -> "GOSUB"
            | OnGotoBuiltIn -> "ON-GOTO"
            | OnGosubBuiltIn -> "ON-GOSUB"
            | NamedBuiltIn name -> name
        let invocation = emitInvocation "ExecuteBuiltInStatement" ([ $"\"{kindName}\""; channelText ] @ argsText)
        appendLine builder level (invocation + ";")
    | Input(channel, prompts, targets, _) ->
        let channelText =
            match channel with
            | Some(ExplicitChannel(expr: HirExpr))
            | Some(ImplicitChannel(expr: HirExpr)) -> emitExpr ctx expr
            | None -> "null"
        let promptText = prompts |> List.map (emitExpr ctx) |> String.concat ", "
        appendLine builder level $"ExecuteInput({channelText}, new object?[] {{ {promptText} }});"
        targets
        |> List.iteri (fun index target ->
            let reader =
                $"ReadInputValue({index}, {hirTypeToken (match target with | WriteVar(_, t, _) | WriteArrayElem(_, _, t, _) | WriteStringChar(_, _, t, _) | DynamicWriteVar(_, t, _) | DynamicWriteArrayElem(_, _, t, _) | DynamicWriteStringChar(_, _, t, _) -> t)})"
            appendLine builder level (emitTargetWrite ctx target reader))
    | WhenError(body, _) ->
        emitWhenError ctx builder level currentLine nextLine returnText gosubReturnCount lineNumbers body
    | If(condition, thenBlock, elseBlock, _) ->
        emitIf ctx builder level currentLine nextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling condition thenBlock elseBlock
    | For(loopId, symbolId, startExpr, endExpr, stepExpr, body, _) ->
        emitFor ctx builder level currentLine nextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId symbolId startExpr endExpr stepExpr body
    | ForSequence(loopId, symbolId, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, body, _) ->
        emitForSequence ctx builder level currentLine nextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId symbolId prefixExprs startExpr endExpr suffixExprs stepExpr body
    | Repeat(loopId, _, body, _) ->
        emitRepeat ctx builder level currentLine nextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId body
    | Exit(loopId, _) ->
        if Set.contains loopId ctx.FlatLoopIds then
            let (LoopId id) = loopId
            appendLine builder level $"goto __loop{id}Exit;"
        else
            appendLine builder level (emitLoopTransfer loopId false)
    | Next(loopId, _) ->
        if Set.contains loopId ctx.FlatLoopIds then
            let (LoopId id) = loopId
            appendLine builder level $"goto __loop{id}Next;"
        else
            appendLine builder level (emitLoopTransfer loopId true)
    | Goto(target, _) ->
        match tryGetConstInt target with
        | Some line -> emitDispatchToLine ctx builder level (string line)
        | None -> emitDispatchToLine ctx builder level $"AsInt({emitExpr ctx target})"
    | OnGoto(selector, targets, _) ->
        emitOnGotoLike "GOTO" ctx builder level selector targets
    | Gosub(target, _) ->
        match tryGetConstInt target with
        | Some line ->
            let returnId = nextGosubReturnId ctx
            appendLine builder level $"__gosubStack.Push({returnId});"
            emitDispatchToLine ctx builder level (string line)
            appendLine builder level $"__gosub_return_{returnId}: ;"
        | None ->
            let returnId = nextGosubReturnId ctx
            appendLine builder level $"__gosubStack.Push({returnId});"
            emitDispatchToLine ctx builder level $"AsInt({emitExpr ctx target})"
            appendLine builder level $"__gosub_return_{returnId}: ;"
    | OnGosub(selector, targets, _) ->
        emitOnGosub ctx builder level selector targets
    | Return(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"return {emitExpr ctx expr};"
        | None -> emitBareReturnDispatch builder level returnText gosubReturnCount
    | LineNumber(_, _) -> ()
    | Restore(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"RestoreToLine(AsInt({emitExpr ctx expr}));"
        | None -> appendLine builder level "__dataPointer = 0;"
    | Read(targets, _) ->
        targets
        |> List.iter (fun target ->
            let valueExpr =
                match target with
                | WriteVar(_, targetType, _)
                | WriteArrayElem(_, _, targetType, _)
                | WriteStringChar(_, _, targetType, _)
                | DynamicWriteVar(_, targetType, _)
                | DynamicWriteArrayElem(_, _, targetType, _)
                | DynamicWriteStringChar(_, _, targetType, _) -> $"ReadDataValue({hirTypeToken targetType})"
            appendLine builder level (emitTargetWrite ctx target valueExpr))
    | Remark(text, _) ->
        appendLine builder level $"// {text}"

and private emitStmt ctx builder level currentLine nextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling stmtLabel stmt =
    let stmtHasCrossScopeTargets =
        match stmt with
        | If(_, thenBlock, elseBlock, _) ->
            blockContainsLineNumbers thenBlock
            || (elseBlock |> Option.exists blockContainsLineNumbers)
        | For(_, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> blockContainsLineNumbers body
        | _ -> false
    appendLine builder level $"{stmtLabel}: ;"
    match stmt with
    | LineNumber(value, _) ->
        if ctx.ProgramHasLineControlFlow then
            appendLine builder level $"line_{value}: ;"
    | _ when enableErrorHandling && not stmtHasCrossScopeTargets ->
        appendLine builder level "try"
        appendLine builder level "{"
        emitStmtCore ctx builder (level + 1) currentLine nextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling stmt
        appendLine builder level "}"
        appendLine builder level "catch (Exception __sbError)"
        appendLine builder level "{"
        appendLine builder (level + 1) "if (__inErrorHandler || __activeErrorHandler is null || IsControlFlowException(__sbError))"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) "throw;"
        appendLine builder (level + 1) "}"
        appendLine builder (level + 1) "var __errorAction = __activeErrorHandler(__sbError);"
        appendLine builder (level + 1) "switch (__errorAction.Kind)"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) "case ErrorActionKind.Retry:"
        appendLine builder (level + 2) "{"
        appendLine builder (level + 3) "if (__errorAction.LineNumber.HasValue)"
        appendLine builder (level + 3) "{"
        emitDispatchToLine ctx builder (level + 4) "__errorAction.LineNumber.Value"
        appendLine builder (level + 3) "}"
        appendLine builder (level + 3) $"goto {stmtLabel};"
        appendLine builder (level + 2) "}"
        appendLine builder (level + 2) "case ErrorActionKind.Continue:"
        appendLine builder (level + 2) "{"
        appendLine builder (level + 3) "if (__errorAction.LineNumber.HasValue)"
        appendLine builder (level + 3) "{"
        emitDispatchToLine ctx builder (level + 4) "__errorAction.LineNumber.Value"
        appendLine builder (level + 3) "}"
        emitTransferToLabel builder (level + 3) continueLabelOpt returnText
        appendLine builder (level + 2) "}"
        appendLine builder (level + 2) "case ErrorActionKind.Stop:"
        appendLine builder (level + 3) "throw new OperationCanceledException(\"STOP encountered.\");"
        appendLine builder (level + 2) "default:"
        appendLine builder (level + 3) "throw;"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"
    | _ ->
        emitStmtCore ctx builder level currentLine nextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling stmt

let private emitStorageDeclarations (ctx: EmitterContext) (builder: StringBuilder) level (storages: HirStorage list) =
    storages
    |> List.iter (fun storage ->
        appendLine builder level $"private static readonly Cell {storageName ctx storage.Symbol} = new Cell();")

let private emitGlobalRegistration (ctx: EmitterContext) (builder: StringBuilder) level (storages: HirStorage list) =
    storages
    |> List.iter (fun storage ->
        appendLine builder level $"RegisterGlobal(\"{escapeStringLiteral (storageSourceName ctx storage.Symbol)}\", {storageName ctx storage.Symbol});")

let private emitArrayDimensionRegistration (ctx: EmitterContext) (builder: StringBuilder) level (storage: HirStorage) =
    match storage.Type, storage.Dimensions with
    | Array _, Some dimensions when not dimensions.IsEmpty ->
        let args = dimensions |> List.map string |> String.concat ", "
        appendLine builder level $"RegisterArrayDimensions({storageName ctx storage.Symbol}, {args});"
    | _ -> ()

let rec private collectFlatLoopIds (block: HirBlock) =
    let rec collect block =
        block
        |> List.collect (function
            | If(_, thenBlock, elseBlock, _) ->
                collect thenBlock
                @ (elseBlock |> Option.map collect |> Option.defaultValue [])
            | For(loopId, _, _, _, _, body, _) ->
                let nested = collect body
                if blockContainsLineNumbers body then loopId :: nested else nested
            | ForSequence(loopId, _, _, _, _, _, _, body, _) ->
                let nested = collect body
                if blockContainsLineNumbers body then loopId :: nested else nested
            | Repeat(loopId, _, body, _) ->
                let nested = collect body
                if blockContainsLineNumbers body then loopId :: nested else nested
            | WhenError(body, _) -> collect body
            | _ -> [])

    collect block |> Set.ofList

let private collectFlatLoopLineOwners (block: HirBlock) =
    let rec collect activeFlatLoop block =
        block
        |> List.collect (function
            | LineNumber(value, _) ->
                match activeFlatLoop with
                | Some loopId -> [ value, loopId ]
                | None -> []
            | If(_, thenBlock, elseBlock, _) ->
                collect activeFlatLoop thenBlock
                @ (elseBlock |> Option.map (collect activeFlatLoop) |> Option.defaultValue [])
            | For(loopId, _, _, _, _, body, _) ->
                let nestedLoop = if blockContainsLineNumbers body then Some loopId else activeFlatLoop
                collect nestedLoop body
            | ForSequence(loopId, _, _, _, _, _, _, body, _) ->
                let nestedLoop = if blockContainsLineNumbers body then Some loopId else activeFlatLoop
                collect nestedLoop body
            | Repeat(loopId, _, body, _) ->
                let nestedLoop = if blockContainsLineNumbers body then Some loopId else activeFlatLoop
                collect nestedLoop body
            | WhenError(body, _) -> collect activeFlatLoop body
            | _ -> [])

    collect None block |> Map.ofList

let private emitFlatLoopDeclarations ctx builder level =
    appendLine builder level "int? __jumpedLoopId = null;"
    ctx.FlatLoopIds
    |> Set.iter (fun (LoopId id) ->
        appendLine builder level $"var __loop{id}Initialized = false;"
        appendLine builder level $"var __loop{id}Index = 0;"
        appendLine builder level $"var __loop{id}End = 0;"
        appendLine builder level $"var __loop{id}Step = 0;"
        appendLine builder level $"var __loop{id}Phase = 0;"
        appendLine builder level $"var __loop{id}PrefixIndex = 0;"
        appendLine builder level $"var __loop{id}SuffixIndex = 0;"
        appendLine builder level $"var __loop{id}RangeInitialized = false;"
        appendLine builder level $"var __loop{id}SuffixInitialized = false;"
        appendLine builder level $"object?[] __loop{id}PrefixValues = Array.Empty<object?>();"
        appendLine builder level $"object?[] __loop{id}SuffixValues = Array.Empty<object?>();"
        appendLine builder level $"object? __loop{id}Value = null;"
        appendLine builder level $"var __loop{id}HasValue = false;")

let private emitRoutine ctx builder (routine: HirRoutine) =
    let gosubReturnCount = countGosubSitesInBlock routine.Body
    let lineNumbers = collectLineNumbersInBlock routine.Body
    let hasWhenError = blockHasWhenError routine.Body
    let hasLineControlFlow = blockHasLineControlFlow routine.Body || hasWhenError
    validateNestedLineNumberTargets $"routine '{routine.Name}'" hasLineControlFlow routine.Body
    let ctx =
        { ctx with
            FlatLoopIds = collectFlatLoopIds routine.Body
            FlatLoopLineOwners = collectFlatLoopLineOwners routine.Body }
    let parameterText =
        routine.Parameters
        |> List.map (fun parameter -> $"Cell {storageName ctx parameter.Storage.Symbol}")
        |> String.concat ", "

    appendLine builder 1 $"private static object? {routineName ctx routine.Symbol}({parameterText})"
    appendLine builder 1 "{"
    routine.Locals
    |> List.iter (fun storage ->
        appendLine builder 2 $"var {storageName ctx storage.Symbol} = new Cell();")
    let allBindings =
        (routine.Parameters |> List.map _.Storage) @ routine.Locals
    appendLine builder 2 "using var __frame = PushDynamicFrame(new Dictionary<string, Cell>(StringComparer.OrdinalIgnoreCase)"
    appendLine builder 2 "{"
    allBindings
    |> List.iter (fun storage ->
        appendLine builder 3 $"[\"{escapeStringLiteral storage.Name}\"] = {storageName ctx storage.Symbol},")
    appendLine builder 2 "});"
    if gosubReturnCount > 0 then
        appendLine builder 2 "var __gosubStack = new Stack<int>();"
    if hasWhenError then
        appendLine builder 2 "Func<Exception, ErrorAction>? __activeErrorHandler = null;"
        appendLine builder 2 "var __inErrorHandler = false;"
    if not ctx.FlatLoopIds.IsEmpty then
        emitFlatLoopDeclarations ctx builder 2
    if hasLineControlFlow then
        emitDispatcher ctx builder 2 lineNumbers routine.EndLineNumber
    routine.Locals
    |> List.iter (fun storage ->
        match storage.Type with
        | Array _ -> appendLine builder 2 $"{storageName ctx storage.Symbol}.Value = new Dictionary<string, Cell>(StringComparer.OrdinalIgnoreCase);"
        | _ -> appendLine builder 2 $"{storageName ctx storage.Symbol}.Value = null;")
    routine.Parameters
    |> List.iter (fun parameter -> emitArrayDimensionRegistration ctx builder 2 parameter.Storage)
    routine.Locals
    |> List.iter (emitArrayDimensionRegistration ctx builder 2)
    emitBlock ctx builder 2 None None None "return null;" gosubReturnCount lineNumbers hasWhenError routine.Body
    appendLine builder 2 "return null;"
    appendLine builder 1 "}"

let private emitDataLayout builder (program: HirProgram) =
    let renderDataEntry entry =
        match entry.Value with
        | Literal(ConstInt value, _, _) -> string value
        | Literal(ConstFloat value, _, _) -> value.ToString("G17", CultureInfo.InvariantCulture)
        | Literal(ConstString value, _, _) -> $"\"{escapeStringLiteral value}\""
        | _ -> "null"

    let dataItems = program.DataEntries |> List.map renderDataEntry |> String.concat ", "
    let restorePoints =
        program.RestorePoints
        |> List.map (fun point ->
            let (DataSlotId slot) = point.Slot
            $"{{ {point.LineNumber}, {slot} }}")
        |> String.concat ", "

    appendLine builder 1 $"private static readonly object?[] __data = new object?[] {{ {dataItems} }};"
    appendLine builder 1 $"private static readonly Dictionary<int, int> __restorePoints = new Dictionary<int, int> {{ {restorePoints} }};"

let generateCSharpFromHir className (program: HirProgram) =
    let ctx = buildContext className program
    let mainGosubReturnCount = countGosubSitesInBlock program.Main
    let mainLineNumbers = collectLineNumbersInBlock program.Main
    let mainHasWhenError = blockHasWhenError program.Main
    let mainHasLineControlFlow = blockHasLineControlFlow program.Main || mainHasWhenError
    validateNestedLineNumberTargets "the main program body" mainHasLineControlFlow program.Main
    let mainCtx =
        { ctx with
            FlatLoopIds = collectFlatLoopIds program.Main
            FlatLoopLineOwners = collectFlatLoopLineOwners program.Main }
    let builder = StringBuilder()

    appendLine builder 0 "using System;"
    appendLine builder 0 "using System.Collections.Generic;"
    appendLine builder 0 "using SBGeneratedRuntime;"
    appendLine builder 0 "using static SBGeneratedRuntime.GeneratedRuntime;"
    appendLine builder 0 ""
    appendLine builder 0 $"public static class {ctx.ClassName}"
    appendLine builder 0 "{"
    emitStorageDeclarations ctx builder 1 program.Globals
    if not program.Globals.IsEmpty then
        appendLine builder 0 ""
    emitDataLayout builder program
    appendLine builder 0 ""
    program.Routines |> List.iter (emitRoutine ctx builder)
    if not program.Routines.IsEmpty then
        appendLine builder 0 ""
    appendLine builder 1 "public static void Main()"
    appendLine builder 1 "{"
    appendLine builder 2 "InitializeProgramState(__data, __restorePoints);"
    emitGlobalRegistration ctx builder 2 program.Globals
    program.Globals
    |> List.iter (fun storage ->
        match storage.Type with
        | Array _ -> appendLine builder 2 $"{storageName ctx storage.Symbol}.Value = new Dictionary<string, Cell>(StringComparer.OrdinalIgnoreCase);"
        | _ -> appendLine builder 2 $"{storageName ctx storage.Symbol}.Value = null;")
    program.Globals
    |> List.iter (emitArrayDimensionRegistration ctx builder 2)
    appendLine builder 2 "void __run()"
    appendLine builder 2 "{"
    if mainGosubReturnCount > 0 then
        appendLine builder 3 "var __gosubStack = new Stack<int>();"
    if mainHasWhenError then
        appendLine builder 3 "Func<Exception, ErrorAction>? __activeErrorHandler = null;"
        appendLine builder 3 "var __inErrorHandler = false;"
    if not mainCtx.FlatLoopIds.IsEmpty then
        emitFlatLoopDeclarations mainCtx builder 3
    if mainHasLineControlFlow then
        emitDispatcher mainCtx builder 3 mainLineNumbers None
    emitBlock mainCtx builder 3 None None None "return;" mainGosubReturnCount mainLineNumbers mainHasWhenError program.Main
    appendLine builder 2 "}"
    appendLine builder 2 "try"
    appendLine builder 2 "{"
    appendLine builder 3 "__run();"
    appendLine builder 2 "}"
    appendLine builder 2 "catch (OperationCanceledException)"
    appendLine builder 2 "{"
    appendLine builder 2 "}"
    appendLine builder 1 "}"
    appendLine builder 0 "}"

    builder.ToString()
