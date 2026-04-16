module HirCBackend

open System
open System.Globalization
open System.Text

open HIR

type private EmitterContext = {
    ProgramName: string
    SymbolNames: Map<SymbolId, string>
    StorageNames: Map<SymbolId, string>
    Storages: Map<SymbolId, HirStorage>
    RoutineNames: Map<SymbolId, string>
    RoutineSymbols: Set<SymbolId>
    ReferencedLineNumbers: Set<int>
    ProgramHasLineControlFlow: bool
    NextGosubReturnId: int ref
    NextStatementId: int ref
    NextWhenErrorId: int ref
}

let private indent level = String.replicate (level * 4) " "

let private appendLine (builder: StringBuilder) level (text: string) =
    builder.Append(indent level).Append(text).AppendLine() |> ignore

let private appendBlankLine (builder: StringBuilder) =
    builder.AppendLine() |> ignore

let private sanitizeIdentifier (value: string) =
    let normalized =
        value
        |> Seq.map (fun ch -> if Char.IsLetterOrDigit ch || ch = '_' then ch else '_')
        |> Seq.toArray
        |> System.String

    if String.IsNullOrWhiteSpace normalized then "_"
    elif Char.IsDigit normalized[0] then "_" + normalized
    else normalized

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

let private escapeCString (value: string) =
    value.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\r", "\\r").Replace("\n", "\\n")

let private tryGetConstInt = function
    | Literal(ConstInt value, _, _) -> Some value
    | _ -> None

let rec private collectReferencedLineNumbers (block: HirBlock) =
    let collectFromStmt = function
        | If(_, thenBlock, elseBlock, _) ->
            let elseLines =
                elseBlock
                |> Option.map collectReferencedLineNumbers
                |> Option.defaultValue Set.empty
            Set.union (collectReferencedLineNumbers thenBlock) elseLines
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) ->
            collectReferencedLineNumbers body
        | Goto(target, _) ->
            tryGetConstInt target |> Option.map Set.singleton |> Option.defaultValue Set.empty
        | Gosub(target, _) ->
            tryGetConstInt target |> Option.map Set.singleton |> Option.defaultValue Set.empty
        | OnGoto(_, targets, _)
        | OnGosub(_, targets, _) ->
            targets
            |> List.choose tryGetConstInt
            |> Set.ofList
        | Restore(Some target, _) ->
            tryGetConstInt target |> Option.map Set.singleton |> Option.defaultValue Set.empty
        | _ ->
            Set.empty

    block |> List.fold (fun acc stmt -> Set.union acc (collectFromStmt stmt)) Set.empty

let rec private collectDeclaredLineNumbers (block: HirBlock) =
    block
    |> List.collect (function
        | LineNumber(value, _) -> [ value ]
        | If(_, thenBlock, elseBlock, _) ->
            (collectDeclaredLineNumbers thenBlock |> Set.toList)
            @ (elseBlock |> Option.map (collectDeclaredLineNumbers >> Set.toList) |> Option.defaultValue [])
        | WhenError(body, _) -> collectDeclaredLineNumbers body |> Set.toList
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> collectDeclaredLineNumbers body |> Set.toList
        | _ -> [])
    |> Set.ofList

let rec private countGosubSitesInBlock (block: HirBlock) =
    block
    |> List.sumBy (function
        | Gosub _ -> 1
        | OnGosub _ -> 1
        | If(_, thenBlock, elseBlock, _) ->
            countGosubSitesInBlock thenBlock
            + (elseBlock |> Option.map countGosubSitesInBlock |> Option.defaultValue 0)
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> countGosubSitesInBlock body
        | _ -> 0)

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
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> blockHasLineControlFlow body
        | _ -> false)

let rec private blockHasDynamicLineControlFlow (block: HirBlock) =
    block
    |> List.exists (function
        | Goto(target, _) -> tryGetConstInt target |> Option.isNone
        | Gosub(target, _) -> tryGetConstInt target |> Option.isNone
        | OnGoto(_, targets, _)
        | OnGosub(_, targets, _) -> targets |> List.exists (tryGetConstInt >> Option.isNone)
        | If(_, thenBlock, elseBlock, _) ->
            blockHasDynamicLineControlFlow thenBlock
            || (elseBlock |> Option.exists blockHasDynamicLineControlFlow)
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> blockHasDynamicLineControlFlow body
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

let private buildContext programName (program: HirProgram) =
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

    let referencedLineNumbers =
        let restoreLines =
            program.RestorePoints
            |> List.map _.LineNumber
            |> Set.ofList

        let routineLines =
            program.Routines
            |> List.map (fun routine -> collectReferencedLineNumbers routine.Body)
            |> List.fold Set.union Set.empty

        let declaredLines =
            collectDeclaredLineNumbers program.Main
            |> Set.union (program.Routines |> List.map (fun routine -> collectDeclaredLineNumbers routine.Body) |> List.fold Set.union Set.empty)

        let dynamicLineTargetsPresent =
            blockHasDynamicLineControlFlow program.Main
            || (program.Routines |> List.exists (fun routine -> blockHasDynamicLineControlFlow routine.Body))

        let whenErrorLines =
            if programHasWhenError then
                declaredLines
            else
                Set.empty

        let dynamicDispatchLines =
            if dynamicLineTargetsPresent then declaredLines
            else Set.empty

        restoreLines
        |> Set.union (collectReferencedLineNumbers program.Main)
        |> Set.union routineLines
        |> Set.union whenErrorLines
        |> Set.union dynamicDispatchLines

    { ProgramName = if String.IsNullOrWhiteSpace programName then "generated_program" else sanitizeIdentifier programName
      SymbolNames = program.SymbolNames
      StorageNames = Map.ofList (globals @ routineStorage)
      Storages =
        (program.Globals @ (program.Routines |> List.collect (fun routine -> (routine.Parameters |> List.map _.Storage) @ routine.Locals)))
        |> List.map (fun storage -> storage.Symbol, storage)
        |> Map.ofList
      RoutineNames = Map.ofList routines
      RoutineSymbols = routines |> List.map fst |> Set.ofList
      ReferencedLineNumbers = referencedLineNumbers
      ProgramHasLineControlFlow = programHasLineControlFlow
      NextGosubReturnId = ref 0
      NextStatementId = ref 0
      NextWhenErrorId = ref 0 }

let private nextGosubReturnId ctx =
    let id = !ctx.NextGosubReturnId
    ctx.NextGosubReturnId := id + 1
    id

let private nextStatementId ctx =
    let id = !ctx.NextStatementId
    ctx.NextStatementId := id + 1
    id

let private nextWhenErrorId ctx =
    let id = !ctx.NextWhenErrorId
    ctx.NextWhenErrorId := id + 1
    id

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

let private storageCellRef ctx symbolId =
    match (storageInfo ctx symbolId).Class with
    | RoutineParameterStorage _ -> storageName ctx symbolId
    | _ -> "&" + storageName ctx symbolId

let private cellValueExpr cellExpr =
    $"({cellExpr})->value"

let private typeTag = function
    | HirType.Int -> "TYPE_INT"
    | HirType.Float -> "TYPE_FLOAT"
    | HirType.String -> "TYPE_STRING"
    | HirType.Void -> "TYPE_NULL"
    | HirType.Array _ -> "TYPE_ARRAY"

let private emitCall targetName args =
    match args with
    | [] -> $"{targetName}()"
    | _ ->
        let argsText = String.concat ", " args
        $"{targetName}({argsText})"

let rec private emitTargetCellRef ctx = function
    | WriteVar(symbolId, _, _) -> storageCellRef ctx symbolId
    | WriteArrayElem(symbolId, indexes, _, _) ->
        emitCall "get_array_cell" ([ storageCellRef ctx symbolId; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx)))
    | WriteStringChar(_, _, _, _) ->
        failwith "String character targets cannot be lowered to Cell* references in the C backend."
    | DynamicWriteVar(name, _, _) -> emitCall "lookup_dynamic_cell" [ $"\"{escapeCString name}\"" ]
    | DynamicWriteArrayElem(name, indexes, _, _) ->
        emitCall "get_array_cell" ([ emitCall "lookup_dynamic_cell" [ $"\"{escapeCString name}\"" ]; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx)))
    | DynamicWriteStringChar(_, _, _, _) ->
        failwith "Dynamic string character targets cannot be lowered to Cell* references in the C backend."

and private emitRoutineCallArg ctx = function
    | ValueArg expr -> emitCall "make_value_arg" [ emitExpr ctx expr ]
    | RefArg(WriteStringChar(_, _, _, _))
    | RefArg(DynamicWriteStringChar(_, _, _, _)) ->
        "make_invalid_ref_arg(\"String character targets cannot be used as by-reference storage locations.\")"
    | RefArg target -> emitCall "make_ref_arg" [ emitTargetCellRef ctx target ]

and private emitBuiltInCallArg ctx = function
    | ValueArg expr -> emitExpr ctx expr
    | RefArg(WriteStringChar(_, _, _, _))
    | RefArg(DynamicWriteStringChar(_, _, _, _)) ->
        "invalid_reference_actual_value(\"String character targets cannot be used as by-reference storage locations.\")"
    | RefArg target -> cellValueExpr (emitTargetCellRef ctx target)

and private emitBuiltInFunctionArgs ctx symbolId args =
    let normalizedName = (builtInName ctx symbolId).Trim().ToUpperInvariant()
    match normalizedName, args with
    | "RND", [ ValueArg(Binary(SliceRange, lhs, rhs, _, _)) ] ->
        [ emitExpr ctx lhs; emitExpr ctx rhs ]
    | _ ->
        args |> List.map (emitBuiltInCallArg ctx)

and private emitExpr ctx = function
    | Literal(ConstInt value, _, _) -> $"make_int({value})"
    | Literal(ConstFloat value, _, _) ->
        let rendered = value.ToString("G17", CultureInfo.InvariantCulture)
        $"make_float({rendered})"
    | Literal(ConstString value, _, _) -> $"make_string(\"{escapeCString value}\")"
    | ReadVar(symbolId, _, _) -> cellValueExpr (storageCellRef ctx symbolId)
    | ReadArrayElem(symbolId, indexes, _, _) ->
        emitCall "get_array_value" ([ storageCellRef ctx symbolId; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx)))
    | ReadStringChar(symbolId, index, _, _) ->
        emitCall "get_string_char_value" [ storageCellRef ctx symbolId; $"as_int({emitExpr ctx index})" ]
    | DynamicReadVar(name, _, _) ->
        let cellExpr = emitCall "lookup_dynamic_cell" [ $"\"{escapeCString name}\"" ]
        cellValueExpr cellExpr
    | DynamicReadArrayElem(name, indexes, _, _) ->
        emitCall "get_array_value" ([ emitCall "lookup_dynamic_cell" [ $"\"{escapeCString name}\"" ]; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx)))
    | DynamicReadStringChar(name, index, _, _) ->
        emitCall "get_string_char_value" [ emitCall "lookup_dynamic_cell" [ $"\"{escapeCString name}\"" ]; $"as_int({emitExpr ctx index})" ]
    | Unary(op, inner, _, _) ->
        let value = emitExpr ctx inner
        match op with
        | Identity -> value
        | Negate -> $"negate_value({value})"
        | BitwiseNot -> $"bitwise_not_value({value})"
        | UnaryUnknown name -> $"unsupported_unary(\"{escapeCString name}\", {value})"
    | Binary(op, lhs, rhs, _, _) ->
        let left = emitExpr ctx lhs
        let right = emitExpr ctx rhs
        match op with
        | Add -> $"add_value({left}, {right})"
        | Subtract -> $"subtract_value({left}, {right})"
        | Multiply -> $"multiply_value({left}, {right})"
        | Divide -> $"divide_value({left}, {right})"
        | Power -> $"power_value({left}, {right})"
        | Concat -> $"concat_value({left}, {right})"
        | IntegerDivide -> $"integer_divide_value({left}, {right})"
        | Modulo -> $"modulo_value({left}, {right})"
        | BitwiseAnd -> $"bitwise_and_value({left}, {right})"
        | BitwiseOr -> $"bitwise_or_value({left}, {right})"
        | BitwiseXor -> $"bitwise_xor_value({left}, {right})"
        | Equal -> $"compare_equal({left}, {right})"
        | NotEqual -> $"compare_not_equal({left}, {right})"
        | LessThan -> $"compare_less_than({left}, {right})"
        | LessThanOrEqual -> $"compare_less_than_or_equal({left}, {right})"
        | GreaterThan -> $"compare_greater_than({left}, {right})"
        | GreaterThanOrEqual -> $"compare_greater_than_or_equal({left}, {right})"
        | Instr -> $"instr_value({left}, {right})"
        | SliceRange -> $"slice_range_value({left}, {right})"
        | BinaryUnknown name -> $"unsupported_binary(\"{escapeCString name}\", {left}, {right})"
    | CallFunc(symbolId, args, _, _) ->
        let renderedArgs =
            if Set.contains symbolId ctx.RoutineSymbols then
                args |> List.map (emitRoutineCallArg ctx)
            else
                emitBuiltInFunctionArgs ctx symbolId args
        if Set.contains symbolId ctx.RoutineSymbols then
            emitCall (routineName ctx symbolId) renderedArgs
        else
            emitCall "invoke_builtin_function" ([ $"\"{escapeCString (builtInName ctx symbolId)}\""; string renderedArgs.Length ] @ renderedArgs)

let private emitTargetWrite ctx target valueExpr =
    match target with
    | WriteVar(symbolId, _, _) -> $"{cellValueExpr (storageCellRef ctx symbolId)} = {valueExpr};"
    | WriteArrayElem(symbolId, indexes, _, _) ->
        emitCall "set_array_value" ([ storageCellRef ctx symbolId; valueExpr; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx))) + ";"
    | WriteStringChar(symbolId, index, _, _) ->
        emitCall "set_string_char_value" [ storageCellRef ctx symbolId; $"as_int({emitExpr ctx index})"; valueExpr ] + ";"
    | DynamicWriteVar(name, _, _) ->
        let cellExpr = emitCall "lookup_dynamic_cell" [ $"\"{escapeCString name}\"" ]
        $"{cellValueExpr cellExpr} = {valueExpr};"
    | DynamicWriteArrayElem(name, indexes, _, _) ->
        emitCall "set_array_value" ([ emitCall "lookup_dynamic_cell" [ $"\"{escapeCString name}\"" ]; valueExpr; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx))) + ";"
    | DynamicWriteStringChar(name, index, _, _) ->
        emitCall "set_string_char_value" [ emitCall "lookup_dynamic_cell" [ $"\"{escapeCString name}\"" ]; $"as_int({emitExpr ctx index})"; valueExpr ] + ";"

let rec private findNextLineNumber block index inheritedNextLine =
    block
    |> List.skip (index + 1)
    |> List.tryPick (function
        | LineNumber(value, _) -> Some value
        | _ -> None)
    |> Option.orElse inheritedNextLine

let rec private emitBlock ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine block =
    let stmtIds = block |> List.map (fun _ -> nextStatementId ctx)

    let rec loop index runningCurrentLine =
        if index < block.Length then
            let stmt = block[index]
            let stmtCurrentLine =
                match stmt with
                | LineNumber(value, _) -> Some value
                | _ -> runningCurrentLine
            let stmtNextLine = findNextLineNumber block index inheritedNextLine
            let nextStmtIdOpt =
                if index + 1 < stmtIds.Length then Some stmtIds[index + 1] else None
            emitStmt ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError stmtIds[index] stmtCurrentLine stmtNextLine nextStmtIdOpt stmt
            loop (index + 1) stmtCurrentLine

    loop 0 currentLine

and private emitIf ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine condition thenBlock elseBlock =
    appendLine builder level $"if (is_true({emitExpr ctx condition}))"
    appendLine builder level "{"
    emitBlock ctx builder (level + 1) gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine thenBlock
    appendLine builder level "}"
    match elseBlock with
    | Some branch ->
        appendLine builder level "else"
        appendLine builder level "{"
        emitBlock ctx builder (level + 1) gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine branch
        appendLine builder level "}"
    | None -> ()

and private emitFor ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine loopId symbolId startExpr endExpr stepExpr body =
    let (LoopId id) = loopId
    let counter = storageName ctx symbolId
    let indexName = $"loop_{id}_index"
    let endName = $"loop_{id}_end"
    let stepName = $"loop_{id}_step"
    appendLine builder level "{"
    appendLine builder (level + 1) $"int {stepName} = as_int({emitExpr ctx stepExpr});"
    appendLine builder (level + 1) $"int {endName} = as_int({emitExpr ctx endExpr});"
    appendLine builder (level + 1) $"int {indexName} = as_int({emitExpr ctx startExpr});"
    appendLine builder (level + 1) $"for (; ({stepName} >= 0) ? ({indexName} <= {endName}) : ({indexName} >= {endName}); {indexName} += {stepName})"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) $"{cellValueExpr (storageCellRef ctx symbolId)} = make_int({indexName});"
    emitBlock ctx builder (level + 2) gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine body
    appendLine builder (level + 2) $"loop_{id}_next: ;"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) $"loop_{id}_exit: ;"
    appendLine builder level "}"

and private emitForSequence ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine loopId symbolId prefixExprs startExpr endExpr suffixExprs stepExpr body =
    let (LoopId id) = loopId
    let prefixValues = prefixExprs |> List.map (emitExpr ctx) |> String.concat ", "
    let suffixValues = suffixExprs |> List.map (emitExpr ctx) |> String.concat ", "
    let counter = storageName ctx symbolId
    let prefixValuesName = $"loop_{id}_prefix_values"
    let prefixCountName = $"loop_{id}_prefix_count"
    let prefixIndexName = $"loop_{id}_prefix_index"
    let rangeIndexName = $"loop_{id}_range_index"
    let rangeEndName = $"loop_{id}_range_end"
    let rangeStepName = $"loop_{id}_range_step"
    let suffixValuesName = $"loop_{id}_suffix_values"
    let suffixCountName = $"loop_{id}_suffix_count"
    let suffixIndexName = $"loop_{id}_suffix_index"
    let phaseName = $"loop_{id}_phase"
    appendLine builder level "{"
    if not prefixExprs.IsEmpty then
        appendLine builder (level + 1) $"Value {prefixValuesName}[] = {{ {prefixValues} }};"
    else
        appendLine builder (level + 1) $"Value {prefixValuesName}[1];"
    appendLine builder (level + 1) $"int {prefixCountName} = {prefixExprs.Length};"
    appendLine builder (level + 1) $"int {prefixIndexName} = 0;"
    appendLine builder (level + 1) $"int {rangeStepName} = as_int({emitExpr ctx stepExpr});"
    appendLine builder (level + 1) $"int {rangeEndName} = as_int({emitExpr ctx endExpr});"
    appendLine builder (level + 1) $"int {rangeIndexName} = as_int({emitExpr ctx startExpr});"
    if not suffixExprs.IsEmpty then
        appendLine builder (level + 1) $"Value {suffixValuesName}[] = {{ {suffixValues} }};"
    else
        appendLine builder (level + 1) $"Value {suffixValuesName}[1];"
    appendLine builder (level + 1) $"int {suffixCountName} = {suffixExprs.Length};"
    appendLine builder (level + 1) $"int {suffixIndexName} = 0;"
    appendLine builder (level + 1) $"int {phaseName} = 0;"
    appendLine builder (level + 1) $"loop_{id}_dispatch: ;"
    appendLine builder (level + 1) $"if ({phaseName} == 0)"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) $"if ({prefixIndexName} < {prefixCountName})"
    appendLine builder (level + 2) "{"
    appendLine builder (level + 3) $"{cellValueExpr (storageCellRef ctx symbolId)} = {prefixValuesName}[{prefixIndexName}++];"
    appendLine builder (level + 3) $"goto loop_{id}_body;"
    appendLine builder (level + 2) "}"
    appendLine builder (level + 2) $"{phaseName} = 1;"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) $"if ({phaseName} == 1)"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) $"if (({rangeStepName} >= 0) ? ({rangeIndexName} <= {rangeEndName}) : ({rangeIndexName} >= {rangeEndName}))"
    appendLine builder (level + 2) "{"
    appendLine builder (level + 3) $"{cellValueExpr (storageCellRef ctx symbolId)} = make_int({rangeIndexName});"
    appendLine builder (level + 3) $"{rangeIndexName} += {rangeStepName};"
    appendLine builder (level + 3) $"goto loop_{id}_body;"
    appendLine builder (level + 2) "}"
    appendLine builder (level + 2) $"{phaseName} = 2;"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) $"if ({phaseName} == 2)"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) $"if ({suffixIndexName} < {suffixCountName})"
    appendLine builder (level + 2) "{"
    appendLine builder (level + 3) $"{cellValueExpr (storageCellRef ctx symbolId)} = {suffixValuesName}[{suffixIndexName}++];"
    appendLine builder (level + 3) $"goto loop_{id}_body;"
    appendLine builder (level + 2) "}"
    appendLine builder (level + 2) $"goto loop_{id}_exit;"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) $"goto loop_{id}_exit;"
    appendLine builder (level + 1) $"loop_{id}_body: ;"
    emitBlock ctx builder (level + 1) gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine body
    appendLine builder (level + 1) $"loop_{id}_next: ;"
    appendLine builder (level + 1) $"goto loop_{id}_dispatch;"
    appendLine builder (level + 1) $"loop_{id}_exit: ;"
    appendLine builder level "}"

and private emitRepeat ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine loopId body =
    let (LoopId id) = loopId
    appendLine builder level "while (1)"
    appendLine builder level "{"
    emitBlock ctx builder (level + 1) gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine inheritedNextLine body
    appendLine builder (level + 1) $"loop_{id}_next: ;"
    appendLine builder level "}"
    appendLine builder level $"loop_{id}_exit: ;"

and private emitLineJumpDispatch ctx builder level targetExpr =
    appendLine builder level $"switch (as_int({targetExpr}))"
    appendLine builder level "{"
    ctx.ReferencedLineNumbers
    |> Set.iter (fun line -> appendLine builder (level + 1) $"case {line}: goto line_{line};")
    appendLine builder (level + 1) "default: sb_raise_error(-21, \"ERR_BL\", \"Bad line of Basic\");"
    appendLine builder level "}"

and private emitOnGotoLike keyword ctx builder level selector targets =
    appendLine builder level $"switch (as_int({emitExpr ctx selector}))"
    appendLine builder level "{"
    targets
    |> List.iteri (fun index target ->
        match tryGetConstInt target with
        | Some line ->
            appendLine builder (level + 1) $"case {index + 1}: goto line_{line};"
        | None ->
            appendLine builder (level + 1) $"case {index + 1}:"
            emitLineJumpDispatch ctx builder (level + 2) (emitExpr ctx target)
            appendLine builder (level + 2) "break;")
    appendLine builder (level + 1) "default: break;"
    appendLine builder level "}"

and private emitOnGosub ctx builder level selector targets =
    let returnId = nextGosubReturnId ctx
    appendLine builder level $"switch (as_int({emitExpr ctx selector}))"
    appendLine builder level "{"
    targets
    |> List.iteri (fun index target ->
        match tryGetConstInt target with
        | Some line ->
            appendLine builder (level + 1) $"case {index + 1}:"
            appendLine builder (level + 2) $"__gosub_stack[__gosub_top++] = {returnId};"
            appendLine builder (level + 2) $"goto line_{line};"
        | None ->
            appendLine builder (level + 1) $"case {index + 1}:"
            appendLine builder (level + 2) $"__gosub_stack[__gosub_top++] = {returnId};"
            emitLineJumpDispatch ctx builder (level + 2) (emitExpr ctx target)
            appendLine builder (level + 2) "break;")
    appendLine builder (level + 1) "default: break;"
    appendLine builder level "}"
    appendLine builder level $"__gosub_return_{returnId}: ;"

and private emitBareReturnDispatch builder level gosubReturnStart gosubReturnCount =
    if gosubReturnCount > 0 then
        appendLine builder level "if (__gosub_top > 0)"
        appendLine builder level "{"
        appendLine builder (level + 1) "switch (__gosub_stack[--__gosub_top])"
        appendLine builder (level + 1) "{"
        [ gosubReturnStart .. gosubReturnStart + gosubReturnCount - 1 ]
        |> List.iter (fun id ->
            appendLine builder (level + 2) $"case {id}: goto __gosub_return_{id};")
        appendLine builder (level + 2) "default: runtime_not_supported(\"RETURN without an active GOSUB target.\"); break;"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"
    appendLine builder level "goto sb_exit;"

and private emitStmtCore ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError stmtId currentLine nextLine nextStmtIdOpt stmt =
    match stmt with
    | Assign(target, value, _) ->
        appendLine builder level (emitTargetWrite ctx target (emitExpr ctx value))
    | ProcCall(symbolId, _, args, _) ->
        appendLine builder level (emitCall (routineName ctx symbolId) (args |> List.map (emitRoutineCallArg ctx)) + ";")
    | BuiltInCall(NamedBuiltIn "RETRY", _, _, _) when not hasWhenError ->
        appendLine builder level "runtime_not_supported(\"RETRY requires WHEN ERROR in generated C backend.\");"
    | BuiltInCall(NamedBuiltIn "RETRY", _, args, _) ->
        appendLine builder level "__error_action = 1;"
        match args with
        | head :: _ -> appendLine builder level $"__error_target_line = as_int({emitExpr ctx head});"
        | [] -> appendLine builder level "__error_target_line = 0;"
        appendLine builder level "goto __when_error_resume;"
    | BuiltInCall(NamedBuiltIn "CONTINUE", _, _, _) when not hasWhenError ->
        appendLine builder level "runtime_not_supported(\"CONTINUE requires WHEN ERROR in generated C backend.\");"
    | BuiltInCall(NamedBuiltIn "CONTINUE", _, args, _) ->
        appendLine builder level "__error_action = 2;"
        match args with
        | head :: _ -> appendLine builder level $"__error_target_line = as_int({emitExpr ctx head});"
        | [] -> appendLine builder level "__error_target_line = 0;"
        appendLine builder level "goto __when_error_resume;"
    | BuiltInCall(NamedBuiltIn "STOP", _, _, _) ->
        appendLine builder level "goto sb_exit;"
    | BuiltInCall(kind, channel, args, _) ->
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
        let channelExpr =
            match channel with
            | Some(ExplicitChannel(expr: HirExpr))
            | Some(ImplicitChannel(expr: HirExpr)) -> emitExpr ctx expr
            | None -> "make_null()"
        appendLine builder level (emitCall "execute_builtin_statement" ([ $"\"{escapeCString kindName}\""; channelExpr; string args.Length ] @ (args |> List.map (emitExpr ctx))) + ";")
    | Input(channel, prompts, targets, _) ->
        let channelExpr =
            match channel with
            | Some(ExplicitChannel(expr: HirExpr))
            | Some(ImplicitChannel(expr: HirExpr)) -> emitExpr ctx expr
            | None -> "make_null()"
        appendLine builder level (emitCall "execute_input" ([ channelExpr; string prompts.Length ] @ (prompts |> List.map (emitExpr ctx))) + ";")
        targets
        |> List.iteri (fun index target ->
            let valueExpr =
                match target with
                | WriteVar(_, typ, _)
                | WriteArrayElem(_, _, typ, _)
                | WriteStringChar(_, _, typ, _)
                | DynamicWriteVar(_, typ, _)
                | DynamicWriteArrayElem(_, _, typ, _)
                | DynamicWriteStringChar(_, _, typ, _) -> $"read_input_value({index}, {typeTag typ})"
            appendLine builder level (emitTargetWrite ctx target valueExpr))
    | WhenError(body, _) ->
        let handlerId = nextWhenErrorId ctx
        appendLine builder level $"{activeHandlerVar} = {handlerId};"
        appendLine builder level $"goto __when_error_after_{handlerId};"
        appendLine builder level $"__when_error_{handlerId}: ;"
        appendLine builder level "__handling_error = 1;"
        appendLine builder level "__error_action = 0;"
        appendLine builder level "__error_target_line = 0;"
        emitBlock ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar true currentLine nextLine body
        appendLine builder level "runtime_not_supported(\"WHEN ERROR handler must exit with RETRY, CONTINUE, or STOP.\");"
        appendLine builder level $"__when_error_after_{handlerId}: ;"
    | If(condition, thenBlock, elseBlock, _) ->
        emitIf ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine nextLine condition thenBlock elseBlock
    | For(loopId, symbolId, startExpr, endExpr, stepExpr, body, _) ->
        emitFor ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine nextLine loopId symbolId startExpr endExpr stepExpr body
    | ForSequence(loopId, symbolId, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, body, _) ->
        emitForSequence ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine nextLine loopId symbolId prefixExprs startExpr endExpr suffixExprs stepExpr body
    | Repeat(loopId, _, body, _) ->
        emitRepeat ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError currentLine nextLine loopId body
    | Exit(loopId, _) ->
        let (LoopId id) = loopId
        appendLine builder level $"goto loop_{id}_exit;"
    | Next(loopId, _) ->
        let (LoopId id) = loopId
        appendLine builder level $"goto loop_{id}_next;"
    | Goto(target, _) ->
        match tryGetConstInt target with
        | Some line -> appendLine builder level $"goto line_{line};"
        | None -> emitLineJumpDispatch ctx builder level (emitExpr ctx target)
    | OnGoto(selector, targets, _) ->
        emitOnGotoLike "GOTO" ctx builder level selector targets
    | Gosub(target, _) ->
        match tryGetConstInt target with
        | Some line ->
            let returnId = nextGosubReturnId ctx
            appendLine builder level $"__gosub_stack[__gosub_top++] = {returnId};"
            appendLine builder level $"goto line_{line};"
            appendLine builder level $"__gosub_return_{returnId}: ;"
        | None ->
            let returnId = nextGosubReturnId ctx
            appendLine builder level $"__gosub_stack[__gosub_top++] = {returnId};"
            emitLineJumpDispatch ctx builder level (emitExpr ctx target)
            appendLine builder level $"__gosub_return_{returnId}: ;"
    | OnGosub(selector, targets, _) ->
        emitOnGosub ctx builder level selector targets
    | Return(value, _) ->
        match value with
        | Some expr ->
            appendLine builder level $"__result = {emitExpr ctx expr};"
            appendLine builder level "goto sb_exit;"
        | None ->
            emitBareReturnDispatch builder level gosubReturnStart gosubReturnCount
    | LineNumber(value, _) ->
        if ctx.ProgramHasLineControlFlow && Set.contains value ctx.ReferencedLineNumbers then
            appendLine builder level $"line_{value}: ;"
    | Restore(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"restore_to_line(as_int({emitExpr ctx expr}));"
        | None -> appendLine builder level "sb_data_pointer = 0;"
    | Read(targets, _) ->
        targets
        |> List.iter (fun target ->
            let valueExpr =
                match target with
                | WriteVar(_, typ, _)
                | WriteArrayElem(_, _, typ, _)
                | WriteStringChar(_, _, typ, _)
                | DynamicWriteVar(_, typ, _)
                | DynamicWriteArrayElem(_, _, typ, _)
                | DynamicWriteStringChar(_, _, typ, _) -> $"read_data_value({typeTag typ})"
            appendLine builder level (emitTargetWrite ctx target valueExpr))
    | Remark(text, _) ->
        appendLine builder level $"/* {escapeCString text} */"

and private emitStmt ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError stmtId currentLine nextLine nextStmtIdOpt stmt =
    if hasWhenError then
        appendLine builder level $"stmt_{stmtId}: ;"
    match stmt with
    | LineNumber _
    | WhenError _ ->
        emitStmtCore ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError stmtId currentLine nextLine nextStmtIdOpt stmt
    | _ when hasWhenError ->
        appendLine builder level "{"
        appendLine builder (level + 1) "jmp_buf __err_jmp;"
        appendLine builder (level + 1) $"sb_record_error_context({currentLine |> Option.defaultValue 0}, {currentLine |> Option.defaultValue 0}, {nextLine |> Option.defaultValue 0});"
        appendLine builder (level + 1) "if (setjmp(__err_jmp) == 0)"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) "sb_push_error_frame(&__err_jmp);"
        emitStmtCore ctx builder (level + 2) gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError stmtId currentLine nextLine nextStmtIdOpt stmt
        appendLine builder (level + 2) "sb_pop_error_frame();"
        appendLine builder (level + 1) "}"
        appendLine builder (level + 1) "else"
        appendLine builder (level + 1) "{"
        appendLine builder (level + 2) "sb_pop_error_frame();"
        appendLine builder (level + 2) $"if (__handling_error || {activeHandlerVar} < 0) sb_fail_last_error();"
        appendLine builder (level + 2) $"__error_retry_stmt = {stmtId};"
        appendLine builder (level + 2) $"__error_continue_stmt = {nextStmtIdOpt |> Option.defaultValue -1};"
        appendLine builder (level + 2) "goto __when_error_dispatch;"
        appendLine builder (level + 1) "}"
        appendLine builder level "}"
    | _ ->
        emitStmtCore ctx builder level gosubReturnStart gosubReturnCount activeHandlerVar hasWhenError stmtId currentLine nextLine nextStmtIdOpt stmt

let private emitStorageDeclarations (ctx: EmitterContext) (builder: StringBuilder) level (storages: HirStorage list) =
    storages |> List.iter (fun storage -> appendLine builder level $"static Cell {storageName ctx storage.Symbol};")

let private emitArrayDimensionRegistration ctx builder level (storage: HirStorage) =
    match storage.Type, storage.Dimensions with
    | Array _, Some dimensions when not dimensions.IsEmpty ->
        let args =
            dimensions
            |> List.map string
            |> String.concat ", "
        appendLine builder level $"register_array_dimensions({storageCellRef ctx storage.Symbol}, {dimensions.Length}, {args});"
    | _ -> ()

let private emitStatementResumeDispatch builder level startStmtId endStmtId targetVar =
    appendLine builder level $"switch ({targetVar})"
    appendLine builder level "{"
    [ startStmtId .. endStmtId ]
    |> List.iter (fun id -> appendLine builder (level + 1) $"case {id}: goto stmt_{id};")
    appendLine builder (level + 1) "default: goto sb_exit;"
    appendLine builder level "}"

let private emitLineResumeDispatch ctx builder level targetVar =
    appendLine builder level $"switch ({targetVar})"
    appendLine builder level "{"
    ctx.ReferencedLineNumbers
    |> Set.iter (fun line -> appendLine builder (level + 1) $"case {line}: goto line_{line};")
    appendLine builder (level + 1) "default: sb_fail_last_error();"
    appendLine builder level "}"

let private emitRoutinePrototype (ctx: EmitterContext) (builder: StringBuilder) level (routine: HirRoutine) =
    let parameters =
        match routine.Parameters with
        | [] -> "void"
        | items -> items |> List.map (fun parameter -> $"ParamBinding {storageName ctx parameter.Storage.Symbol}_arg") |> String.concat ", "
    appendLine builder level $"static Value {routineName ctx routine.Symbol}({parameters});"

let private emitRoutine (ctx: EmitterContext) (builder: StringBuilder) (routine: HirRoutine) =
    let parameters =
        match routine.Parameters with
        | [] -> "void"
        | items -> items |> List.map (fun parameter -> $"ParamBinding {storageName ctx parameter.Storage.Symbol}_arg") |> String.concat ", "
    appendLine builder 0 $"/* Routine: {routine.Name} */"
    appendLine builder 0 $"static Value {routineName ctx routine.Symbol}({parameters})"
    appendLine builder 0 "{"
    appendLine builder 1 "Value __result = make_null();"
    routine.Parameters
    |> List.iter (fun parameter ->
        appendLine builder 1 $"Cell {storageName ctx parameter.Storage.Symbol}_local = make_cell(make_null());"
        appendLine builder 1 $"Cell* {storageName ctx parameter.Storage.Symbol} = bind_parameter(&{storageName ctx parameter.Storage.Symbol}_arg, &{storageName ctx parameter.Storage.Symbol}_local);")
    routine.Locals
    |> List.iter (fun storage ->
        let initial =
            match storage.Type with
            | Array _ -> "make_cell(make_array())"
            | _ -> "make_cell(make_null())"
        appendLine builder 1 $"Cell {storageName ctx storage.Symbol} = {initial};")
    routine.Parameters
    |> List.iter (fun parameter -> emitArrayDimensionRegistration ctx builder 1 parameter.Storage)
    routine.Locals
    |> List.iter (emitArrayDimensionRegistration ctx builder 1)
    let allBindings =
        (routine.Parameters |> List.map _.Storage) @ routine.Locals
    if not allBindings.IsEmpty then
        appendLine builder 1 $"DynamicBinding __frame_bindings[{allBindings.Length}];"
    let gosubReturnCount = countGosubSitesInBlock routine.Body
    let gosubReturnStart = !ctx.NextGosubReturnId
    let hasWhenError = blockHasWhenError routine.Body
    let stmtStartId = !ctx.NextStatementId
    let whenErrorStartId = !ctx.NextWhenErrorId
    if gosubReturnCount > 0 then
        appendLine builder 1 $"int __gosub_stack[{gosubReturnCount}];"
        appendLine builder 1 "int __gosub_top = 0;"
    if hasWhenError then
        appendLine builder 1 "int __active_when_error = -1;"
        appendLine builder 1 "int __handling_error = 0;"
        appendLine builder 1 "int __error_action = 0;"
        appendLine builder 1 "int __error_target_line = 0;"
        appendLine builder 1 "int __error_retry_stmt = -1;"
        appendLine builder 1 "int __error_continue_stmt = -1;"
    if not allBindings.IsEmpty then
        appendBlankLine builder
        allBindings
        |> List.iteri (fun index storage ->
            appendLine builder 1 $"__frame_bindings[{index}].name = \"{escapeCString storage.Name}\";"
            appendLine builder 1 $"__frame_bindings[{index}].cell = {storageCellRef ctx storage.Symbol};")
        appendLine builder 1 $"push_dynamic_frame(__frame_bindings, {allBindings.Length});"
    if not routine.Body.IsEmpty then
        appendBlankLine builder
    emitBlock ctx builder 1 gosubReturnStart gosubReturnCount "__active_when_error" hasWhenError None None routine.Body
    let stmtEndId = !ctx.NextStatementId - 1
    let whenErrorEndId = !ctx.NextWhenErrorId - 1
    if hasWhenError then
        appendBlankLine builder
        appendLine builder 1 "/* WHEN ERROR resume dispatch */"
        appendLine builder 1 "__when_error_dispatch: ;"
        appendLine builder 1 "switch (__active_when_error)"
        appendLine builder 1 "{"
        [ whenErrorStartId .. whenErrorEndId ]
        |> List.iter (fun id -> appendLine builder 2 $"case {id}: goto __when_error_{id};")
        appendLine builder 2 "default: sb_fail_last_error();"
        appendLine builder 1 "}"
        appendLine builder 1 "__when_error_resume: ;"
        appendLine builder 1 "__handling_error = 0;"
        appendLine builder 1 "switch (__error_action)"
        appendLine builder 1 "{"
        appendLine builder 2 "case 1:"
        appendLine builder 2 "if (__error_target_line != 0)"
        appendLine builder 2 "{"
        emitLineResumeDispatch ctx builder 3 "__error_target_line"
        appendLine builder 2 "}"
        emitStatementResumeDispatch builder 2 stmtStartId stmtEndId "__error_retry_stmt"
        appendLine builder 2 "case 2:"
        appendLine builder 2 "if (__error_target_line != 0)"
        appendLine builder 2 "{"
        emitLineResumeDispatch ctx builder 3 "__error_target_line"
        appendLine builder 2 "}"
        emitStatementResumeDispatch builder 2 stmtStartId stmtEndId "__error_continue_stmt"
        appendLine builder 2 "case 3: goto sb_exit;"
        appendLine builder 2 "default: runtime_not_supported(\"WHEN ERROR handler must exit with RETRY, CONTINUE, or STOP.\");"
        appendLine builder 1 "}"
    appendBlankLine builder
    appendLine builder 0 "sb_exit:"
    if not allBindings.IsEmpty then
        appendLine builder 1 "pop_dynamic_frame();"
    appendLine builder 1 "return __result;"
    appendLine builder 0 "}"
    appendBlankLine builder

let private emitDataLayout builder (program: HirProgram) =
    let renderEntry entry =
        match entry.Value with
        | Literal(ConstInt value, _, _) ->
            let rendered = (float value).ToString("G17", CultureInfo.InvariantCulture)
            $"{{ TYPE_INT, {value}, {rendered}, NULL, NULL }}"
        | Literal(ConstFloat value, _, _) ->
            let rendered = value.ToString("G17", CultureInfo.InvariantCulture)
            $"{{ TYPE_FLOAT, 0, {rendered}, NULL, NULL }}"
        | Literal(ConstString value, _, _) -> $"{{ TYPE_STRING, 0, 0.0, \"{escapeCString value}\", NULL }}"
        | _ -> "{ TYPE_NULL, 0, 0.0, NULL, NULL }"

    let restorePoints =
        program.RestorePoints
        |> List.map (fun point ->
            let (DataSlotId slot) = point.Slot
            $"{{ {point.LineNumber}, {slot} }}")
        |> String.concat ", "

    let dataItems = String.concat ", " (program.DataEntries |> List.map renderEntry)
    appendLine builder 0 "/* DATA and INPUT runtime state */"
    appendLine builder 0 $"DataValue sb_data[] = {{ {dataItems} }};"
    appendLine builder 0 $"int sb_data_count = {program.DataEntries.Length};"
    appendLine builder 0 $"RestorePoint sb_restore_points[] = {{ {restorePoints} }};"
    appendLine builder 0 $"int sb_restore_point_count = {program.RestorePoints.Length};"
    appendLine builder 0 "int sb_data_pointer = 0;"
    appendLine builder 0 "char sb_input_buffer[4096];"
    appendLine builder 0 "char* sb_input_parts[256];"
    appendLine builder 0 "int sb_input_part_count = 0;"
    appendBlankLine builder

let cRuntimeHeaderFileName = "sbruntime_c.h"
let cRuntimeSourceFileName = "sbruntime_c.c"

let generateCFromHir programName (program: HirProgram) =
    let ctx = buildContext programName program
    let builder = StringBuilder()

    builder.AppendLine($"#include \"{cRuntimeHeaderFileName}\"") |> ignore
    appendBlankLine builder
    emitDataLayout builder program
    if not (List.isEmpty program.Globals) then
        appendLine builder 0 "/* Global storage */"
        emitStorageDeclarations ctx builder 0 program.Globals
        appendBlankLine builder
    if not (List.isEmpty program.Routines) then
        appendLine builder 0 "/* Routine prototypes */"
        program.Routines |> List.iter (emitRoutinePrototype ctx builder 0)
        appendBlankLine builder
    appendLine builder 0 "int main(void);"
    appendBlankLine builder
    program.Routines |> List.iter (emitRoutine ctx builder)
    appendLine builder 0 "/* Program entry point */"
    appendLine builder 0 "int main(void)"
    appendLine builder 0 "{"
    let mainGosubReturnCount = countGosubSitesInBlock program.Main
    let mainGosubReturnStart = !ctx.NextGosubReturnId
    let mainHasWhenError = blockHasWhenError program.Main
    let mainStmtStartId = !ctx.NextStatementId
    let mainWhenErrorStartId = !ctx.NextWhenErrorId
    if mainGosubReturnCount > 0 then
        appendLine builder 1 $"int __gosub_stack[{mainGosubReturnCount}];"
        appendLine builder 1 "int __gosub_top = 0;"
    if mainHasWhenError then
        appendLine builder 1 "int __active_when_error = -1;"
        appendLine builder 1 "int __handling_error = 0;"
        appendLine builder 1 "int __error_action = 0;"
        appendLine builder 1 "int __error_target_line = 0;"
        appendLine builder 1 "int __error_retry_stmt = -1;"
        appendLine builder 1 "int __error_continue_stmt = -1;"
    appendBlankLine builder
    appendLine builder 1 "sb_runtime_init();"
    program.Globals
    |> List.iter (fun storage ->
        let initial =
            match storage.Type with
            | Array _ -> "make_cell(make_array())"
            | _ -> "make_cell(make_null())"
        appendLine builder 1 $"{storageName ctx storage.Symbol} = {initial};"
        emitArrayDimensionRegistration ctx builder 1 storage
        appendLine builder 1 $"register_global(\"{escapeCString storage.Name}\", &{storageName ctx storage.Symbol});")
    if not program.Globals.IsEmpty then
        appendBlankLine builder
    emitBlock ctx builder 1 mainGosubReturnStart mainGosubReturnCount "__active_when_error" mainHasWhenError None None program.Main
    let mainStmtEndId = !ctx.NextStatementId - 1
    let mainWhenErrorEndId = !ctx.NextWhenErrorId - 1
    if mainHasWhenError then
        appendBlankLine builder
        appendLine builder 1 "/* WHEN ERROR resume dispatch */"
        appendLine builder 1 "__when_error_dispatch: ;"
        appendLine builder 1 "switch (__active_when_error)"
        appendLine builder 1 "{"
        [ mainWhenErrorStartId .. mainWhenErrorEndId ]
        |> List.iter (fun id -> appendLine builder 2 $"case {id}: goto __when_error_{id};")
        appendLine builder 2 "default: sb_fail_last_error();"
        appendLine builder 1 "}"
        appendLine builder 1 "__when_error_resume: ;"
        appendLine builder 1 "__handling_error = 0;"
        appendLine builder 1 "switch (__error_action)"
        appendLine builder 1 "{"
        appendLine builder 2 "case 1:"
        appendLine builder 2 "if (__error_target_line != 0)"
        appendLine builder 2 "{"
        emitLineResumeDispatch ctx builder 3 "__error_target_line"
        appendLine builder 2 "}"
        emitStatementResumeDispatch builder 2 mainStmtStartId mainStmtEndId "__error_retry_stmt"
        appendLine builder 2 "case 2:"
        appendLine builder 2 "if (__error_target_line != 0)"
        appendLine builder 2 "{"
        emitLineResumeDispatch ctx builder 3 "__error_target_line"
        appendLine builder 2 "}"
        emitStatementResumeDispatch builder 2 mainStmtStartId mainStmtEndId "__error_continue_stmt"
        appendLine builder 2 "case 3: goto sb_exit;"
        appendLine builder 2 "default: runtime_not_supported(\"WHEN ERROR handler must exit with RETRY, CONTINUE, or STOP.\");"
        appendLine builder 1 "}"
    appendBlankLine builder
    appendLine builder 0 "sb_exit:"
    appendLine builder 1 "return 0;"
    appendLine builder 0 "}"

    builder.ToString()
