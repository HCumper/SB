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
    // GOSUB return ids allocated in the current scope (a routine, the main program or a WHEN ERROR
    // handler), and the label of the switch that RETURN jumps to in order to reach them.
    GosubReturnIds: ResizeArray<int>
    GosubReturnDispatchLabel: string
}

let cSharpRuntimeFileName = "GeneratedRuntime.cs"

let private indent level = String.replicate (level * 4) " "

let private appendLine (builder: StringBuilder) level (text: string) =
    builder.Append(indent level).Append(text).AppendLine() |> ignore

let private appendLines builder level (lines: string list) =
    lines |> List.iter (appendLine builder level)

// Writes "{", the body one level deeper, then "}".
let private appendBraced builder level emitBody =
    appendLine builder level "{"
    emitBody (level + 1)
    appendLine builder level "}"

// Writes a header such as "if (...)", "try" or a method signature, then a braced body.
let private appendBlock builder level (header: string) emitBody =
    appendLine builder level header
    appendBraced builder level emitBody

// Writes a fixed C# fragment, usually a $$"""...""" string so that single braces are literal C# and
// {{...}} interpolates. The fragment's common indentation is removed, so it can sit at the F# code's
// indentation, and its own nesting is kept. Leading and trailing blank lines are dropped, so the
// fragment can open and close on lines of its own.
let private appendTemplate builder level (template: string) =
    let lines =
        template.Replace("\r", "").Split('\n')
        |> Array.toList
        |> List.skipWhile String.IsNullOrWhiteSpace
        |> List.rev
        |> List.skipWhile String.IsNullOrWhiteSpace
        |> List.rev
    let margin =
        lines
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> List.map (fun line -> line.Length - line.TrimStart(' ').Length)
        |> List.fold min Int32.MaxValue
    lines
    |> List.iter (fun line -> appendLine builder level (if String.IsNullOrWhiteSpace line then "" else line.Substring margin))

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

let private targetType = function
    | WriteVar(_, hirType, _)
    | WriteArrayElem(_, _, hirType, _)
    | WriteStringChar(_, _, hirType, _)
    | DynamicWriteVar(_, hirType, _)
    | DynamicWriteArrayElem(_, _, hirType, _)
    | DynamicWriteStringChar(_, _, hirType, _) -> hirType

let private exprType = function
    | Literal(_, hirType, _)
    | ReadVar(_, hirType, _)
    | ReadArrayElem(_, _, hirType, _)
    | ReadStringChar(_, _, hirType, _)
    | DynamicReadVar(_, hirType, _)
    | DynamicReadArrayElem(_, _, hirType, _)
    | DynamicReadStringChar(_, _, hirType, _)
    | Unary(_, _, hirType, _)
    | Binary(_, _, _, hirType, _)
    | CallFunc(_, _, hirType, _) -> hirType

// ---- Block traversal ----
//
// Every question the emitter asks about a block is answered by walking it through childBlocks, so
// the list of statements that own nested blocks lives in one place.

// The blocks a statement owns, in source order.
let private childBlocks = function
    | If(_, thenBlock, elseBlock, _) -> thenBlock :: Option.toList elseBlock
    | WhenError(body, _)
    | For(_, _, _, _, _, body, _)
    | ForSequence(_, _, _, _, _, _, _, body, _)
    | Repeat(_, _, body, _) -> [ body ]
    | _ -> []

// Every statement in the block and in the blocks nested inside it, in source order. A WHEN ERROR
// handler is emitted as a local function with its own dispatcher, so its statements are left out
// unless includeHandlers is set.
let rec private statementsIn includeHandlers (block: HirBlock) : HirStmt seq =
    seq {
        for stmt in block do
            yield stmt
            let children =
                match stmt with
                | WhenError _ when not includeHandlers -> []
                | _ -> childBlocks stmt
            for child in children do
                yield! statementsIn includeHandlers child
    }

let private lineNumberOf = function
    | LineNumber(value, _) -> Some value
    | _ -> None

let private isGosubSite = function
    | Gosub _
    | OnGosub _ -> true
    | _ -> false

let private blockHasLineControlFlow block =
    statementsIn true block
    |> Seq.exists (function
        | Goto _
        | Gosub _
        | OnGoto _
        | OnGosub _ -> true
        | _ -> false)

let private blockHasWhenError block =
    statementsIn true block
    |> Seq.exists (function
        | WhenError _ -> true
        | _ -> false)

// A jump target is a label that code outside the block jumps to: a numbered line (reached from the
// line dispatcher) or a GOSUB return point (reached from RETURN). C# cannot jump into a nested block
// or a try block, so a construct whose body holds one is emitted flat and is not wrapped in a try.
let private blockContainsJumpTargets block =
    statementsIn true block
    |> Seq.exists (fun stmt -> isGosubSite stmt || (lineNumberOf stmt).IsSome)

// Counts the GOSUBs in WHEN ERROR handlers too: a handler shares its method's GOSUB stack.
let private countGosubSitesInBlock block =
    statementsIn true block |> Seq.filter isGosubSite |> Seq.length

// The numbered lines reachable from the block's own dispatcher.
let private collectLineNumbersInBlock block =
    statementsIn false block |> Seq.choose lineNumberOf |> Seq.distinct |> List.ofSeq

// The numbered lines for a WHEN ERROR handler's dispatcher, including those of handlers nested in it.
let private collectLineNumbersInBlockIncludingHandlers block =
    statementsIn true block |> Seq.choose lineNumberOf |> Seq.distinct |> List.ofSeq

// A loop whose body holds a jump target is emitted flat: labels and gotos in the enclosing method.
let private flatLoopId stmt =
    match stmt with
    | For(loopId, _, _, _, _, body, _)
    | ForSequence(loopId, _, _, _, _, _, _, body, _)
    | Repeat(loopId, _, body, _) when blockContainsJumpTargets body -> Some loopId
    | _ -> None

let private collectFlatLoopIds block =
    statementsIn true block |> Seq.choose flatLoopId |> Set.ofSeq

// Maps each numbered line inside a flat loop to the innermost flat loop that holds it.
let private collectFlatLoopLineOwners (block: HirBlock) =
    let rec collect owner (block: HirBlock) =
        block
        |> List.collect (fun stmt ->
            match lineNumberOf stmt with
            | Some value -> owner |> Option.map (fun loopId -> value, loopId) |> Option.toList
            | None ->
                let childOwner = flatLoopId stmt |> Option.orElse owner
                childBlocks stmt |> List.collect (collect childOwner))

    collect None block |> Map.ofList

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
      DispatchLabelName = "__dispatch"
      GosubReturnIds = ResizeArray()
      GosubReturnDispatchLabel = "__gosubReturnDispatch" }

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

// A float must stay a double in C#. "G17" prints whole numbers without a decimal point, and C# reads
// those as int or long literals: 1E10 would become a long, which the runtime treats as 0.
let private emitFloatLiteral (value: float) =
    if Double.IsNaN value then "double.NaN"
    elif Double.IsPositiveInfinity value then "double.PositiveInfinity"
    elif Double.IsNegativeInfinity value then "double.NegativeInfinity"
    else
        let text = value.ToString("G17", CultureInfo.InvariantCulture)
        if text.IndexOfAny([| '.'; 'E' |]) >= 0 then text else text + ".0"

// The runtime computes + - * and unary minus in doubles. Like the interpreter, the result is rounded
// to an integer unless the expression is typed Float.
let private emitNumericResult hirType expr =
    match hirType with
    | HirType.Float -> expr
    | _ -> $"AsInt({expr})"

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
    | Literal(ConstFloat value, _, _) -> emitFloatLiteral value
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
    | Unary(op, inner, hirType, _) ->
        match op with
        | Identity -> $"Identity({emitExpr ctx inner})"
        | Negate -> emitNumericResult hirType $"Negate({emitExpr ctx inner})"
        | BitwiseNot -> $"BitwiseNot({emitExpr ctx inner})"
        | UnaryUnknown name -> $"ApplyUnary(\"{name}\", {emitExpr ctx inner})"
    | Binary(op, lhs, rhs, hirType, _) ->
        let left = emitExpr ctx lhs
        let right = emitExpr ctx rhs
        match op with
        | Add -> emitNumericResult hirType $"Add({left}, {right})"
        | Subtract -> emitNumericResult hirType $"Subtract({left}, {right})"
        | Multiply -> emitNumericResult hirType $"Multiply({left}, {right})"
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

let private emitTargetWriteValue ctx target sourceType valueExpr =
    let coercedValueExpr =
        if targetType target = sourceType then
            valueExpr
        else
            emitInvocation "CoerceAssignmentValue" [ hirTypeToken (targetType target); valueExpr ]
    match target with
    | WriteVar(symbolId, _, _) -> $"{storageName ctx symbolId}.Value = {coercedValueExpr};"
    | WriteArrayElem(symbolId, indexes, _, _) ->
        let invocation = emitInvocation "SetArrayValue" ([ storageName ctx symbolId; coercedValueExpr ] @ (indexes |> List.map (emitExpr ctx)))
        invocation + ";"
    | WriteStringChar(symbolId, index, _, _) ->
        emitInvocation "SetStringCharValue" [ storageName ctx symbolId; emitExpr ctx index; coercedValueExpr ] + ";"
    | DynamicWriteVar(name, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        $"{cellExpr}.Value = {coercedValueExpr};"
    | DynamicWriteArrayElem(name, indexes, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        emitInvocation "SetArrayValue" ([ cellExpr; coercedValueExpr ] @ (indexes |> List.map (emitExpr ctx))) + ";"
    | DynamicWriteStringChar(name, index, _, _) ->
        let cellExpr = emitInvocation "LookupDynamicCell" [ $"\"{name}\"" ]
        emitInvocation "SetStringCharValue" [ cellExpr; emitExpr ctx index; coercedValueExpr ] + ";"

let private emitTargetWrite ctx target value =
    emitTargetWriteValue ctx target (exprType value) (emitExpr ctx value)

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
    ctx.GosubReturnIds.Add id
    id

let private nextWhenErrorId ctx =
    let id = !ctx.NextWhenErrorId
    ctx.NextWhenErrorId := id + 1
    id

let private nextSyntheticLabel ctx prefix =
    let id = !ctx.NextStatementId
    ctx.NextStatementId := id + 1
    $"{prefix}{id}"

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

let private emitDispatchToLine ctx builder level lineExpr =
    appendLine builder level $"{ctx.DispatchLineName} = {lineExpr};"
    appendLine builder level $"goto {ctx.DispatchLabelName};"

let private emitDispatcher ctx builder level lineNumbers exitLineNumber =
    appendLine builder level $"int? {ctx.DispatchLineName} = null;"
    appendLine builder level $"{ctx.DispatchLabelName}: ;"
    appendBlock builder level $"if ({ctx.DispatchLineName}.HasValue)" (fun level ->
        appendLine builder level $"var {ctx.DispatchTargetName} = {ctx.DispatchLineName}.Value;"
        appendLine builder level $"{ctx.DispatchLineName} = null;"
        appendBlock builder level $"switch ({ctx.DispatchTargetName})" (fun level ->
            for lineNumber in lineNumbers do
                let jumpedLoop =
                    match Map.tryFind lineNumber ctx.FlatLoopLineOwners with
                    | Some(LoopId loopId) -> $"__jumpedLoopId = {loopId}; "
                    | None when not ctx.FlatLoopIds.IsEmpty -> "__jumpedLoopId = null; "
                    | None -> ""
                appendLine builder level $"case {lineNumber}: {jumpedLoop}goto line_{lineNumber};"
            match exitLineNumber with
            | Some lineNumber when not (List.contains lineNumber lineNumbers) ->
                appendLine builder level $"case {lineNumber}: return null;"
            | _ -> ()
            appendLine builder level $"default: throw new InvalidOperationException($\"Target line {{{ctx.DispatchTargetName}}} does not exist.\");"))

let private emitBareReturnDispatch ctx builder level returnText gosubReturnCount =
    if gosubReturnCount > 0 then
        appendLine builder level $"if (__gosubStack.Count > 0) goto {ctx.GosubReturnDispatchLabel};"
    appendLine builder level returnText

// Emitted once at the end of a scope, after its body, when every GOSUB in it has its return id.
// Each RETURN jumps here rather than carrying its own switch, because a RETURN can come before
// the GOSUBs it serves.
let private emitGosubReturnDispatch ctx builder level gosubReturnCount =
    if gosubReturnCount > 0 then
        appendLine builder level $"{ctx.GosubReturnDispatchLabel}: ;"
        appendBlock builder level "switch (__gosubStack.Pop())" (fun level ->
            for id in ctx.GosubReturnIds do
                appendLine builder level $"case {id}: goto __gosub_return_{id};"
            appendLine builder level "default: throw new InvalidOperationException(\"RETURN without an active GOSUB target.\");")

let private emitLineTarget ctx target =
    match tryGetConstInt target with
    | Some line -> string line
    | None -> $"AsInt({emitExpr ctx target})"

// Pushes the return id and jumps to the subroutine. The return label is emitted separately by
// emitGosubReturnLabel, because it must stay outside any try block that guards the call.
let private emitGosubCall ctx builder level returnId stmt =
    match stmt with
    | Gosub(target, _) ->
        appendLine builder level $"__gosubStack.Push({returnId});"
        emitDispatchToLine ctx builder level (emitLineTarget ctx target)
    | OnGosub(selector, targets, _) ->
        appendBlock builder level $"switch (AsInt({emitExpr ctx selector}))" (fun level ->
            targets
            |> List.iteri (fun index target ->
                appendLine builder level $"case {index + 1}:"
                appendLine builder (level + 1) $"__gosubStack.Push({returnId});"
                emitDispatchToLine ctx builder (level + 1) (emitLineTarget ctx target))
            appendLine builder level "default: break;")
    | _ -> invalidArg (nameof stmt) "Expected a GOSUB or ON GOSUB statement."

let private emitGosubReturnLabel builder level returnId =
    appendLine builder level $"__gosub_return_{returnId}: ;"

let private emitOnGoto ctx builder level selector targets =
    appendBlock builder level $"switch (AsInt({emitExpr ctx selector}))" (fun level ->
        targets
        |> List.iteri (fun index target ->
            appendLine builder level $"case {index + 1}:"
            emitDispatchToLine ctx builder (level + 1) (emitLineTarget ctx target))
        appendLine builder level "default: break;")

// Wraps one statement so that an error raised by it is passed to the active WHEN ERROR handler.
let private emitWithErrorHandler ctx builder level continueLabelOpt returnText stmtLabel emitBody =
    let continueText =
        match continueLabelOpt with
        | Some label -> $"goto {label};"
        | None -> returnText
    appendBlock builder level "try" emitBody
    appendTemplate builder level $$"""
        catch (Exception __sbError)
        {
            if (__inErrorHandler || __activeErrorHandler is null || IsControlFlowException(__sbError))
            {
                throw;
            }
            var __errorAction = __activeErrorHandler(__sbError);
            switch (__errorAction.Kind)
            {
                case ErrorActionKind.Retry:
                {
                    if (__errorAction.LineNumber.HasValue)
                    {
                        {{ctx.DispatchLineName}} = __errorAction.LineNumber.Value;
                        goto {{ctx.DispatchLabelName}};
                    }
                    goto {{stmtLabel}};
                }
                case ErrorActionKind.Continue:
                {
                    if (__errorAction.LineNumber.HasValue)
                    {
                        {{ctx.DispatchLineName}} = __errorAction.LineNumber.Value;
                        goto {{ctx.DispatchLabelName}};
                    }
                    {{continueText}}
                }
                case ErrorActionKind.Stop:
                    throw new OperationCanceledException("STOP encountered.");
                default:
                    throw;
            }
        }
        """

// The test that keeps a FOR range going, shared by every form of FOR loop.
let private forContinueTest id =
    $"__loop{id}Step >= 0 ? __loop{id}Index <= __loop{id}End : __loop{id}Index >= __loop{id}End"

// EXIT and NEXT in a structured loop throw LoopControlException. The loop whose id it carries stops,
// or goes on to its next iteration; any other loop passes it on.
let private emitLoopControlCatch builder level id =
    appendTemplate builder level $$"""
        catch (LoopControlException ex)
        {
            if (ex.LoopId != {{id}})
            {
                throw;
            }
            if (!ex.IsNext)
            {
                break;
            }
        }
        """

let private emitJumpedLoopEscapeCheck builder level id =
    appendLine builder level $"if (__jumpedLoopId == {id}) throw new InvalidOperationException(\"Loop control escaped the current scope after a numbered line jump.\");"

// After a flat loop's body. A loop entered by a numbered-line jump into its body leaves the current
// scope when the body ends, and its NEXT and EXIT are errors.
let private emitFlatLoopBodyEnd builder level id returnText =
    appendBlock builder level $"if (__jumpedLoopId == {id})" (fun level ->
        appendLine builder level "__jumpedLoopId = null;"
        appendLine builder level returnText)
    appendLine builder level $"__loop{id}Next: ;"
    emitJumpedLoopEscapeCheck builder level id

let private emitFlatLoopExit builder level id =
    appendLine builder level $"__loop{id}Exit: ;"
    emitJumpedLoopEscapeCheck builder level id

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

// CONTINUE after an error in the last statement of a loop body resumes at the end of the body, so
// the loop runs its next iteration instead of leaving the routine.
and private emitLoopBody ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling (LoopId id) body =
    let continueLabelOpt = if enableErrorHandling then Some $"__loop{id}Continue" else None
    emitBlock ctx builder level currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling body
    continueLabelOpt |> Option.iter (fun label -> appendLine builder level $"{label}: ;")

and private emitIf ctx builder level currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling condition thenBlock elseBlock =
    let hasJumpTargets =
        blockContainsJumpTargets thenBlock
        || (elseBlock |> Option.exists blockContainsJumpTargets)
    if hasJumpTargets then
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
        appendBlock builder level $"if (IsTrue({emitExpr ctx condition}))" (fun level ->
            emitBlock ctx builder level currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling thenBlock)
        elseBlock
        |> Option.iter (fun branch ->
            appendBlock builder level "else" (fun level ->
                emitBlock ctx builder level currentLine inheritedNextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling branch))

// The bounds are evaluated once, before the first iteration, in the interpreter's order: start, end, step.
// As in the interpreter, the counter is assigned before each test, so it holds end + step after the loop.
and private emitFor ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId symbolId startExpr endExpr stepExpr body =
    let (LoopId id) = loopId
    let counterName = storageName ctx symbolId
    let loopIndexName = $"__loop{id}Index"
    let endName = $"__loop{id}End"
    let stepName = $"__loop{id}Step"
    let emitBody level =
        emitLoopBody ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId body
    if Set.contains loopId ctx.FlatLoopIds then
        // Reaching the FOR statement always starts the loop afresh, even when a GOTO or an outer loop's
        // NEXT brings control back before the previous run finished. NEXT re-enters at the test instead.
        appendLines builder level [
            $"{loopIndexName} = AsInt({emitExpr ctx startExpr});"
            $"{endName} = AsInt({emitExpr ctx endExpr});"
            $"{stepName} = AsInt({emitExpr ctx stepExpr});"
            $"__loop{id}Test: ;"
            $"{counterName}.Value = {loopIndexName};"
            $"if (!({forContinueTest id})) goto __loop{id}Exit;"
            $"__loop{id}Body: ;" ]
        emitBody level
        emitFlatLoopBodyEnd builder level id returnText
        appendLines builder level [
            $"{loopIndexName} += {stepName};"
            $"goto __loop{id}Test;" ]
        emitFlatLoopExit builder level id
    else
        appendBraced builder level (fun level ->
            appendLines builder level [
                $"var {loopIndexName} = AsInt({emitExpr ctx startExpr});"
                $"var {endName} = AsInt({emitExpr ctx endExpr});"
                $"var {stepName} = AsInt({emitExpr ctx stepExpr});" ]
            appendBlock builder level "while (true)" (fun level ->
                appendLines builder level [
                    $"{counterName}.Value = {loopIndexName};"
                    $"if (!({forContinueTest id})) break;" ]
                appendBlock builder level "try" emitBody
                emitLoopControlCatch builder level id
                appendLine builder level $"{loopIndexName} += {stepName};"))

// Every value is evaluated once, before the first iteration, in the interpreter's order: the prefix
// values, start, end, the suffix values, then step.
and private emitForSequence ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId symbolId prefixExprs startExpr endExpr suffixExprs stepExpr body =
    let (LoopId id) = loopId
    let counterName = storageName ctx symbolId
    let loopIndexName = $"__loop{id}Index"
    let endName = $"__loop{id}End"
    let stepName = $"__loop{id}Step"
    let prefixName = $"__loop{id}PrefixValues"
    let suffixName = $"__loop{id}SuffixValues"
    let emitBody level =
        emitLoopBody ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId body
    let valueList exprs = exprs |> List.map (emitExpr ctx) |> String.concat ", "
    if Set.contains loopId ctx.FlatLoopIds then
        // Reaching the FOR statement always starts the loop afresh, even when a GOTO or an outer loop's
        // NEXT brings control back before the previous run finished. NEXT re-enters at the test instead.
        appendLines builder level [
            $"{prefixName} = new object?[] {{ {valueList prefixExprs} }};"
            $"{loopIndexName} = AsInt({emitExpr ctx startExpr});"
            $"{endName} = AsInt({emitExpr ctx endExpr});"
            $"{suffixName} = new object?[] {{ {valueList suffixExprs} }};"
            $"{stepName} = AsInt({emitExpr ctx stepExpr});"
            $"__loop{id}PrefixIndex = 0;"
            $"__loop{id}SuffixIndex = 0;"
            $"__loop{id}Phase = 0;" ]
        // Each pass takes the next value: the prefix values (phase 0), then the range (phase 1), then
        // the suffix values (phase 2), and leaves the loop when the suffix values run out.
        appendTemplate builder level $$"""
            __loop{{id}}Test: ;
            __loop{{id}}HasValue = false;
            if (__loop{{id}}Phase == 0)
            {
                if (__loop{{id}}PrefixIndex < {{prefixName}}.Length)
                {
                    __loop{{id}}Value = {{prefixName}}[__loop{{id}}PrefixIndex++];
                    __loop{{id}}HasValue = true;
                }
                else
                {
                    __loop{{id}}Phase = 1;
                }
            }
            if (!__loop{{id}}HasValue && __loop{{id}}Phase == 1)
            {
                if ({{forContinueTest id}})
                {
                    __loop{{id}}Value = {{loopIndexName}};
                    {{loopIndexName}} += {{stepName}};
                    __loop{{id}}HasValue = true;
                }
                else
                {
                    __loop{{id}}Phase = 2;
                }
            }
            if (!__loop{{id}}HasValue && __loop{{id}}Phase == 2)
            {
                if (__loop{{id}}SuffixIndex < {{suffixName}}.Length)
                {
                    __loop{{id}}Value = {{suffixName}}[__loop{{id}}SuffixIndex++];
                    __loop{{id}}HasValue = true;
                }
                else
                {
                    goto __loop{{id}}Exit;
                }
            }
            {{counterName}}.Value = __loop{{id}}Value;
            __loop{{id}}Body: ;
            """
        emitBody level
        emitFlatLoopBodyEnd builder level id returnText
        appendLine builder level $"goto __loop{id}Test;"
        emitFlatLoopExit builder level id
    else
        // The values come from a local iterator so that the body stays inline in the enclosing method:
        // GOTO, GOSUB and RETURN in the body must be able to leave the loop, and they cannot do that
        // from inside a local function. The iterator only reads values evaluated before it starts.
        let startName = $"__loop{id}Start"
        let valuesName = $"__loop{id}Values"
        let emitValueArray level name exprs =
            if not (List.isEmpty exprs) then
                appendLine builder level $"var {name} = new object?[] {{ {valueList exprs} }};"
        let emitYieldEach level name exprs itemName =
            if not (List.isEmpty exprs) then
                appendBlock builder level $"foreach (var {itemName} in {name})" (fun level ->
                    appendLine builder level $"yield return {itemName};")
        appendBraced builder level (fun level ->
            emitValueArray level prefixName prefixExprs
            appendLines builder level [
                $"var {startName} = AsInt({emitExpr ctx startExpr});"
                $"var {endName} = AsInt({emitExpr ctx endExpr});" ]
            emitValueArray level suffixName suffixExprs
            appendLine builder level $"var {stepName} = AsInt({emitExpr ctx stepExpr});"
            appendBlock builder level $"IEnumerable<object?> {valuesName}()" (fun level ->
                emitYieldEach level prefixName prefixExprs $"__loop{id}Prefix"
                appendBlock builder level $"for (var {loopIndexName} = {startName}; {forContinueTest id}; {loopIndexName} += {stepName})" (fun level ->
                    appendLine builder level $"yield return {loopIndexName};")
                emitYieldEach level suffixName suffixExprs $"__loop{id}Suffix")
            appendBlock builder level $"foreach (var __loop{id}Value in {valuesName}())" (fun level ->
                appendLine builder level $"{counterName}.Value = __loop{id}Value;"
                appendBlock builder level "try" emitBody
                emitLoopControlCatch builder level id))

and private emitRepeat ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId body =
    let (LoopId id) = loopId
    let emitBody level =
        emitLoopBody ctx builder level currentLine inheritedNextLine returnText gosubReturnCount lineNumbers enableErrorHandling loopId body
    if Set.contains loopId ctx.FlatLoopIds then
        appendLine builder level $"__loop{id}Body: ;"
        emitBody level
        emitFlatLoopBodyEnd builder level id returnText
        appendLine builder level $"goto __loop{id}Body;"
        emitFlatLoopExit builder level id
    else
        appendBlock builder level "while (true)" (fun level ->
            appendBlock builder level "try" emitBody
            emitLoopControlCatch builder level id)

and private emitWhenError ctx builder level currentLine nextLine returnText gosubReturnCount lineNumbers handlerBody =
    let handlerId = nextWhenErrorId ctx
    let handlerLineNumbers = collectLineNumbersInBlockIncludingHandlers handlerBody
    let handlerHasLineControlFlow = blockHasLineControlFlow handlerBody
    let handlerCtx =
        { ctx with
            DispatchLineName = $"__whenError{handlerId}DispatchLine"
            DispatchTargetName = $"__whenError{handlerId}TargetLine"
            DispatchLabelName = $"__whenError{handlerId}Dispatch"
            GosubReturnIds = ResizeArray()
            GosubReturnDispatchLabel = $"__whenError{handlerId}GosubReturnDispatch" }
    appendBlock builder level $"ErrorAction __whenError{handlerId}(Exception __sbError)" (fun level ->
        appendLines builder level [
            $"RecordLastError(__sbError, {emitLineLiteral currentLine}, {emitLineLiteral currentLine}, {emitLineLiteral nextLine});"
            "__inErrorHandler = true;" ]
        appendBlock builder level "try" (fun level ->
            if handlerHasLineControlFlow then
                emitDispatcher handlerCtx builder level handlerLineNumbers None
            emitBlock handlerCtx builder level currentLine nextLine None returnText gosubReturnCount handlerLineNumbers false handlerBody
            appendLine builder level "throw new InvalidOperationException(\"WHEN ERROR handler must exit with RETRY, CONTINUE, or STOP.\");"
            emitGosubReturnDispatch handlerCtx builder level gosubReturnCount)
        appendTemplate builder level """
            catch (RetryControlException ex)
            {
                return ErrorAction.Retry(ex.LineNumber);
            }
            catch (ContinueControlException ex)
            {
                return ErrorAction.Continue(ex.LineNumber);
            }
            catch (OperationCanceledException)
            {
                return ErrorAction.Stop();
            }
            finally
            {
                __inErrorHandler = false;
            }
            """)
    appendLine builder level $"__activeErrorHandler = __whenError{handlerId};"

and private emitStmtCore ctx builder level currentLine nextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling stmt =
    match stmt with
    | Assign(target, value, _) ->
        appendLine builder level (emitTargetWrite ctx target value)
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
            appendLine builder level (emitTargetWriteValue ctx target (targetType target) reader))
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
        emitDispatchToLine ctx builder level (emitLineTarget ctx target)
    | OnGoto(selector, targets, _) ->
        emitOnGoto ctx builder level selector targets
    | Gosub _
    | OnGosub _ ->
        let returnId = nextGosubReturnId ctx
        emitGosubCall ctx builder level returnId stmt
        emitGosubReturnLabel builder level returnId
    | Return(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"return {emitExpr ctx expr};"
        | None -> emitBareReturnDispatch ctx builder level returnText gosubReturnCount
    | LineNumber(_, _) -> ()
    | Restore(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"RestoreToLine(AsInt({emitExpr ctx expr}));"
        | None -> appendLine builder level "RestoreData();"
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
            appendLine builder level (emitTargetWriteValue ctx target (targetType target) valueExpr))
    | Remark(text, _) ->
        appendLine builder level $"// {text}"

and private emitStmt ctx builder level currentLine nextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling stmtLabel stmt =
    // A structured statement whose body holds jump targets is emitted flat, and C# cannot jump into a
    // try block, so it is not wrapped. A WHEN ERROR handler is a separate local function: safe to wrap.
    let stmtHasCrossScopeTargets =
        match stmt with
        | WhenError _ -> false
        | _ -> childBlocks stmt |> List.exists blockContainsJumpTargets
    appendLine builder level $"{stmtLabel}: ;"
    match stmt with
    | LineNumber(value, _) ->
        if ctx.ProgramHasLineControlFlow then
            appendLine builder level $"line_{value}: ;"
    | Gosub _
    | OnGosub _ when enableErrorHandling ->
        // Guard the call, but keep the return label outside the try so RETURN can reach it.
        let returnId = nextGosubReturnId ctx
        emitWithErrorHandler ctx builder level continueLabelOpt returnText stmtLabel (fun bodyLevel ->
            emitGosubCall ctx builder bodyLevel returnId stmt)
        emitGosubReturnLabel builder level returnId
    | _ when enableErrorHandling && not stmtHasCrossScopeTargets ->
        emitWithErrorHandler ctx builder level continueLabelOpt returnText stmtLabel (fun bodyLevel ->
            emitStmtCore ctx builder bodyLevel currentLine nextLine continueLabelOpt returnText gosubReturnCount lineNumbers enableErrorHandling stmt)
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

// A flat loop keeps its state in locals of the enclosing method. A FOR loop needs its range; a FOR
// sequence also tracks where it is in its list of values; a REPeat loop needs no state.
let private emitFlatLoopDeclarations builder level (block: HirBlock) =
    appendLine builder level "int? __jumpedLoopId = null;"
    for stmt in statementsIn true block do
        let rangeState id =
            [ $"var __loop{id}Index = 0;"
              $"var __loop{id}End = 0;"
              $"var __loop{id}Step = 0;" ]
        match stmt, flatLoopId stmt with
        | For _, Some(LoopId id) -> appendLines builder level (rangeState id)
        | ForSequence _, Some(LoopId id) ->
            appendLines builder level (
                rangeState id
                @ [ $"var __loop{id}Phase = 0;"
                    $"var __loop{id}PrefixIndex = 0;"
                    $"var __loop{id}SuffixIndex = 0;"
                    $"object?[] __loop{id}PrefixValues = Array.Empty<object?>();"
                    $"object?[] __loop{id}SuffixValues = Array.Empty<object?>();"
                    $"object? __loop{id}Value = null;"
                    $"var __loop{id}HasValue = false;" ])
        | _ -> ()

// The locals a routine's body, or the main program's __run, declares before its first statement.
let private emitScopePrologue ctx builder level gosubReturnCount hasWhenError hasLineControlFlow lineNumbers exitLineNumber block =
    if gosubReturnCount > 0 then
        appendLine builder level "var __gosubStack = new Stack<int>();"
    if hasWhenError then
        appendLines builder level [
            "Func<Exception, ErrorAction>? __activeErrorHandler = null;"
            "var __inErrorHandler = false;" ]
    if not ctx.FlatLoopIds.IsEmpty then
        emitFlatLoopDeclarations builder level block
    if hasLineControlFlow then
        emitDispatcher ctx builder level lineNumbers exitLineNumber

let private emitStorageReset ctx builder level (storage: HirStorage) =
    match storage.Type with
    | Array _ -> appendLine builder level $"{storageName ctx storage.Symbol}.Value = new Dictionary<string, Cell>(StringComparer.OrdinalIgnoreCase);"
    | _ -> appendLine builder level $"{storageName ctx storage.Symbol}.Value = null;"

let private emitRoutine ctx builder level (routine: HirRoutine) =
    let gosubReturnCount = countGosubSitesInBlock routine.Body
    let lineNumbers = collectLineNumbersInBlock routine.Body
    let hasWhenError = blockHasWhenError routine.Body
    let hasLineControlFlow = blockHasLineControlFlow routine.Body || hasWhenError
    let ctx =
        { ctx with
            FlatLoopIds = collectFlatLoopIds routine.Body
            FlatLoopLineOwners = collectFlatLoopLineOwners routine.Body
            GosubReturnIds = ResizeArray() }
    let parameterText =
        routine.Parameters
        |> List.map (fun parameter -> $"Cell {storageName ctx parameter.Storage.Symbol}")
        |> String.concat ", "

    appendBlock builder level $"private static object? {routineName ctx routine.Symbol}({parameterText})" (fun level ->
        for storage in routine.Locals do
            appendLine builder level $"var {storageName ctx storage.Symbol} = new Cell();"
        appendLines builder level [
            "using var __frame = PushDynamicFrame(new Dictionary<string, Cell>(StringComparer.OrdinalIgnoreCase)"
            "{" ]
        for storage in (routine.Parameters |> List.map _.Storage) @ routine.Locals do
            appendLine builder (level + 1) $"[\"{escapeStringLiteral storage.Name}\"] = {storageName ctx storage.Symbol},"
        appendLine builder level "});"
        emitScopePrologue ctx builder level gosubReturnCount hasWhenError hasLineControlFlow lineNumbers routine.EndLineNumber routine.Body
        for storage in routine.Locals do
            emitStorageReset ctx builder level storage
        for parameter in routine.Parameters do
            emitArrayDimensionRegistration ctx builder level parameter.Storage
        for storage in routine.Locals do
            emitArrayDimensionRegistration ctx builder level storage
        emitBlock ctx builder level None None None "return null;" gosubReturnCount lineNumbers hasWhenError routine.Body
        appendLine builder level "return null;"
        emitGosubReturnDispatch ctx builder level gosubReturnCount)

let private emitDataLayout builder level (program: HirProgram) =
    let renderDataEntry entry =
        match entry.Value with
        | Literal(ConstInt value, _, _) -> string value
        | Literal(ConstFloat value, _, _) -> emitFloatLiteral value
        | Literal(ConstString value, _, _) -> $"\"{escapeStringLiteral value}\""
        | _ -> "null"

    let dataItems = program.DataEntries |> List.map renderDataEntry |> String.concat ", "
    let restorePoints =
        program.RestorePoints
        |> List.map (fun point ->
            let (DataSlotId slot) = point.Slot
            $"{{ {point.LineNumber}, {slot} }}")
        |> String.concat ", "

    appendLines builder level [
        $"private static readonly object?[] __data = new object?[] {{ {dataItems} }};"
        $"private static readonly Dictionary<int, int> __restorePoints = new Dictionary<int, int> {{ {restorePoints} }};" ]

let generateCSharpFromHir className (program: HirProgram) =
    let ctx = buildContext className program
    let mainGosubReturnCount = countGosubSitesInBlock program.Main
    let mainLineNumbers = collectLineNumbersInBlock program.Main
    let mainHasWhenError = blockHasWhenError program.Main
    let mainHasLineControlFlow = blockHasLineControlFlow program.Main || mainHasWhenError
    let mainCtx =
        { ctx with
            FlatLoopIds = collectFlatLoopIds program.Main
            FlatLoopLineOwners = collectFlatLoopLineOwners program.Main }
    let builder = StringBuilder()

    appendLines builder 0 [
        "using System;"
        "using System.Collections.Generic;"
        "using SBGeneratedRuntime;"
        "using static SBGeneratedRuntime.GeneratedRuntime;"
        "" ]
    appendBlock builder 0 $"public static class {ctx.ClassName}" (fun level ->
        emitStorageDeclarations ctx builder level program.Globals
        if not program.Globals.IsEmpty then
            appendLine builder 0 ""
        emitDataLayout builder level program
        appendLine builder 0 ""
        for routine in program.Routines do
            emitRoutine ctx builder level routine
        if not program.Routines.IsEmpty then
            appendLine builder 0 ""
        appendBlock builder level "public static void Main()" (fun level ->
            appendLine builder level "InitializeProgramState(__data, __restorePoints);"
            emitGlobalRegistration ctx builder level program.Globals
            for storage in program.Globals do
                emitStorageReset ctx builder level storage
            for storage in program.Globals do
                emitArrayDimensionRegistration ctx builder level storage
            appendBlock builder level "void __run()" (fun level ->
                emitScopePrologue mainCtx builder level mainGosubReturnCount mainHasWhenError mainHasLineControlFlow mainLineNumbers None program.Main
                emitBlock mainCtx builder level None None None "return;" mainGosubReturnCount mainLineNumbers mainHasWhenError program.Main
                if mainGosubReturnCount > 0 then
                    appendLine builder level "return;"
                    emitGosubReturnDispatch mainCtx builder level mainGosubReturnCount)
            appendBlock builder level "try" (fun level -> appendLine builder level "__run();")
            appendBlock builder level "catch (OperationCanceledException)" ignore))

    builder.ToString()
