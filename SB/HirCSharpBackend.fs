module HirCSharpBackend

open System
open System.Globalization
open System.Text

open HIR

type private EmitterContext = {
    ClassName: string
    SymbolNames: Map<SymbolId, string>
    StorageNames: Map<SymbolId, string>
    RoutineNames: Map<SymbolId, string>
    RoutineSymbols: Set<SymbolId>
}

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

let private buildContext className (program: HirProgram) =
    let globals =
        program.Globals
        |> List.map (fun storage -> storage.Symbol, storageFieldName program.SymbolNames storage)

    let routineStorage =
        program.Routines
        |> List.collect (fun routine ->
            (routine.Parameters @ routine.Locals)
            |> List.map (fun storage -> storage.Symbol, storageFieldName program.SymbolNames storage))

    let routines =
        program.Routines
        |> List.map (fun routine -> routine.Symbol, routineMethodName program.SymbolNames routine)

    { ClassName = if String.IsNullOrWhiteSpace className then "GeneratedProgram" else sanitizeIdentifier className
      SymbolNames = program.SymbolNames
      StorageNames = Map.ofList (globals @ routineStorage)
      RoutineNames = Map.ofList routines
      RoutineSymbols = routines |> List.map fst |> Set.ofList }

let private storageName ctx symbolId =
    Map.tryFind symbolId ctx.StorageNames
    |> Option.defaultValue (let (SymbolId id) = symbolId in $"v{id}_{symbolBaseName ctx.SymbolNames symbolId}")

let private routineName ctx symbolId =
    Map.tryFind symbolId ctx.RoutineNames
    |> Option.defaultValue (let (SymbolId id) = symbolId in $"r{id}_{symbolBaseName ctx.SymbolNames symbolId}")

let private builtInName ctx symbolId =
    Map.tryFind symbolId ctx.SymbolNames
    |> Option.defaultValue "BUILTIN"

let private escapeStringLiteral (value: string) =
    value.Replace("\\", "\\\\").Replace("\"", "\\\"")

let private emitInvocation targetName args =
    match args with
    | [] -> $"{targetName}()"
    | _ ->
        let argsText = String.concat ", " args
        $"{targetName}({argsText})"

let rec private emitExpr ctx = function
    | Literal(ConstInt value, _, _) -> string value
    | Literal(ConstFloat value, _, _) -> value.ToString("G17", CultureInfo.InvariantCulture)
    | Literal(ConstString value, _, _) -> $"\"{escapeStringLiteral value}\""
    | ReadVar(symbolId, _, _) -> storageName ctx symbolId
    | ReadArrayElem(symbolId, indexes, _, _) ->
        emitInvocation "GetArrayValue" (storageName ctx symbolId :: (indexes |> List.map (emitExpr ctx)))
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
        let argsText = args |> List.map (emitExpr ctx)
        if Set.contains symbolId ctx.RoutineSymbols then
            emitInvocation (routineName ctx symbolId) argsText
        else
            emitInvocation "InvokeBuiltInFunction" ($"\"{builtInName ctx symbolId}\"" :: argsText)

let private emitTargetWrite ctx target valueExpr =
    match target with
    | WriteVar(symbolId, _, _) -> $"{storageName ctx symbolId} = {valueExpr};"
    | WriteArrayElem(symbolId, indexes, _, _) ->
        let invocation = emitInvocation "SetArrayValue" ([ storageName ctx symbolId; valueExpr ] @ (indexes |> List.map (emitExpr ctx)))
        invocation + ";"

let private emitLoopTransfer loopId isNext =
    let (LoopId id) = loopId
    if isNext then
        $"throw new LoopControlException({id}, true);"
    else
        $"throw new LoopControlException({id}, false);"

let rec private emitBlock ctx builder level (block: HirBlock) =
    block |> List.iter (emitStmt ctx builder level)

and private emitIf ctx builder level condition thenBlock elseBlock =
    appendLine builder level $"if (IsTrue({emitExpr ctx condition}))"
    appendLine builder level "{"
    emitBlock ctx builder (level + 1) thenBlock
    appendLine builder level "}"
    match elseBlock with
    | Some branch ->
        appendLine builder level "else"
        appendLine builder level "{"
        emitBlock ctx builder (level + 1) branch
        appendLine builder level "}"
    | None -> ()

and private emitFor ctx builder level loopId symbolId startExpr endExpr stepExpr body =
    let (LoopId id) = loopId
    let counterName = storageName ctx symbolId
    let loopIndexName = $"__loop{id}Index"
    let endName = $"__loop{id}End"
    let stepName = $"__loop{id}Step"
    appendLine builder level "{"
    appendLine builder (level + 1) $"var {stepName} = AsInt({emitExpr ctx stepExpr});"
    appendLine builder (level + 1) $"var {endName} = AsInt({emitExpr ctx endExpr});"
    appendLine builder (level + 1) $"for (var {loopIndexName} = AsInt({emitExpr ctx startExpr}); {stepName} >= 0 ? {loopIndexName} <= {endName} : {loopIndexName} >= {endName}; {loopIndexName} += {stepName})"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) $"{counterName} = {loopIndexName};"
    appendLine builder (level + 2) "try"
    appendLine builder (level + 2) "{"
    emitBlock ctx builder (level + 3) body
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

and private emitRepeat ctx builder level loopId body =
    let (LoopId id) = loopId
    appendLine builder level "while (true)"
    appendLine builder level "{"
    appendLine builder (level + 1) "try"
    appendLine builder (level + 1) "{"
    emitBlock ctx builder (level + 2) body
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
            appendLine builder (level + 1) $"case {index + 1}: goto line_{line};"
        | Some _ ->
            appendLine builder (level + 1) $"case {index + 1}: throw new NotSupportedException(\"{keyword} is not supported by the generated backend yet.\");"
        | None ->
            appendLine builder (level + 1) $"case {index + 1}: throw new NotSupportedException(\"Dynamic {keyword} is not supported by the generated backend yet.\");")
    appendLine builder (level + 1) "default: break;"
    appendLine builder level "}"

and private emitStmt ctx builder level stmt =
    match stmt with
    | Assign(target, value, _) ->
        appendLine builder level (emitTargetWrite ctx target (emitExpr ctx value))
    | ProcCall(symbolId, _, args, _) ->
        let argsText = args |> List.map (emitExpr ctx)
        appendLine builder level $"{emitInvocation (routineName ctx symbolId) argsText};"
    | BuiltInCall(kind, channel, args, _) ->
        let channelText =
            match channel with
            | Some expr -> emitExpr ctx expr
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
            | Some expr -> emitExpr ctx expr
            | None -> "null"
        let promptText = prompts |> List.map (emitExpr ctx) |> String.concat ", "
        appendLine builder level $"ExecuteInput({channelText}, new object?[] {{ {promptText} }});"
        targets
        |> List.iteri (fun index target ->
            let reader = $"ReadInputValue({index}, {hirTypeToken (match target with | WriteVar(_, t, _) | WriteArrayElem(_, _, t, _) -> t)})"
            appendLine builder level (emitTargetWrite ctx target reader))
    | If(condition, thenBlock, elseBlock, _) ->
        emitIf ctx builder level condition thenBlock elseBlock
    | For(loopId, symbolId, startExpr, endExpr, stepExpr, body, _) ->
        emitFor ctx builder level loopId symbolId startExpr endExpr stepExpr body
    | Repeat(loopId, _, body, _) ->
        emitRepeat ctx builder level loopId body
    | Exit(loopId, _) ->
        appendLine builder level (emitLoopTransfer loopId false)
    | Next(loopId, _) ->
        appendLine builder level (emitLoopTransfer loopId true)
    | Goto(target, _) ->
        match tryGetConstInt target with
        | Some line -> appendLine builder level $"goto line_{line};"
        | None -> appendLine builder level "throw new NotSupportedException(\"Dynamic GOTO is not supported by the generated backend yet.\");"
    | OnGoto(selector, targets, _) ->
        emitOnGotoLike "GOTO" ctx builder level selector targets
    | Gosub(_, _) ->
        appendLine builder level "throw new NotSupportedException(\"GOSUB is not supported by the generated backend yet.\");"
    | OnGosub(selector, targets, _) ->
        emitOnGotoLike "GOSUB" ctx builder level selector targets
    | Return(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"return {emitExpr ctx expr};"
        | None -> appendLine builder level "return null;"
    | LineNumber(value, _) ->
        appendLine builder level $"line_{value}:"
    | Restore(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"RestoreToLine(AsInt({emitExpr ctx expr}));"
        | None -> appendLine builder level "__dataPointer = 0;"
    | Read(targets, _) ->
        targets
        |> List.iter (fun target ->
            let valueExpr =
                match target with
                | WriteVar(_, targetType, _) -> $"ReadDataValue({hirTypeToken targetType})"
                | WriteArrayElem(_, _, targetType, _) -> $"ReadDataValue({hirTypeToken targetType})"
            appendLine builder level (emitTargetWrite ctx target valueExpr))
    | Remark(text, _) ->
        appendLine builder level $"// {text}"

let private emitStorageDeclarations ctx builder level storages =
    storages
    |> List.iter (fun storage ->
        match storage.Type with
        | Array _ -> appendLine builder level $"private static readonly Dictionary<string, object?> {storageName ctx storage.Symbol} = new();"
        | _ -> appendLine builder level $"private static object? {storageName ctx storage.Symbol};")

let private emitRoutine ctx builder (routine: HirRoutine) =
    let parameterText =
        routine.Parameters
        |> List.map (fun storage -> $"object? {storageName ctx storage.Symbol}")
        |> String.concat ", "

    appendLine builder 1 $"private static object? {routineName ctx routine.Symbol}({parameterText})"
    appendLine builder 1 "{"
    routine.Locals
    |> List.iter (fun storage ->
        match storage.Type with
        | Array _ -> appendLine builder 2 $"var {storageName ctx storage.Symbol} = new Dictionary<string, object?>();"
        | _ -> appendLine builder 2 $"object? {storageName ctx storage.Symbol} = null;")
    emitBlock ctx builder 2 routine.Body
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
    appendLine builder 1 "private static int __dataPointer;"
    appendLine builder 1 "private static string[] __inputBuffer = Array.Empty<string>();"

let private emitRuntimeHelpers builder =
    let helpers =
        [ "private sealed class LoopControlException : Exception"
          "{"
          "    public LoopControlException(int loopId, bool isNext)"
          "    {"
          "        LoopId = loopId;"
          "        IsNext = isNext;"
          "    }"
          ""
          "    public int LoopId { get; }"
          "    public bool IsNext { get; }"
          "}"
          ""
          "private static int AsInt(object? value)"
          "{"
          "    return value switch"
          "    {"
          "        null => 0,"
          "        int i => i,"
          "        double d => (int)Math.Round(d),"
          "        float f => (int)Math.Round(f),"
          "        string s when int.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture, out var i) => i,"
          "        string s when double.TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out var d) => (int)Math.Round(d),"
          "        _ => 0"
          "    };"
          "}"
          ""
          "private static double AsDouble(object? value)"
          "{"
          "    return value switch"
          "    {"
          "        null => 0.0,"
          "        int i => i,"
          "        double d => d,"
          "        float f => f,"
          "        string s when double.TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out var d) => d,"
          "        _ => 0.0"
          "    };"
          "}"
          ""
          "private static string AsString(object? value) => value switch"
          "{"
          "    null => string.Empty,"
          "    string s => s,"
          "    double d => d.ToString(\"G17\", CultureInfo.InvariantCulture),"
          "    float f => f.ToString(\"G9\", CultureInfo.InvariantCulture),"
          "    _ => Convert.ToString(value, CultureInfo.InvariantCulture) ?? string.Empty"
          "};"
          ""
          "private static bool IsTrue(object? value)"
          "{"
          "    return value switch"
          "    {"
          "        null => false,"
          "        string s => !string.IsNullOrEmpty(s),"
          "        _ => Math.Abs(AsDouble(value)) > double.Epsilon"
          "    };"
          "}"
          ""
          "private static string ArrayKey(params object?[] indexes) => string.Join(\",\", indexes.Select(AsInt));"
          ""
          "private static object? GetArrayValue(Dictionary<string, object?> array, params object?[] indexes)"
          "{"
          "    return array.TryGetValue(ArrayKey(indexes), out var value) ? value : null;"
          "}"
          ""
          "private static void SetArrayValue(Dictionary<string, object?> array, object? value, params object?[] indexes)"
          "{"
          "    array[ArrayKey(indexes)] = value;"
          "}"
          ""
          "private static object? Identity(object? value) => value;"
          "private static object? Negate(object? value) => -AsDouble(value);"
          "private static object? BitwiseNot(object? value) => ~AsInt(value);"
          "private static object? Add(object? left, object? right) => left is string || right is string ? AsString(left) + AsString(right) : AsDouble(left) + AsDouble(right);"
          "private static object? Subtract(object? left, object? right) => AsDouble(left) - AsDouble(right);"
          "private static object? Multiply(object? left, object? right) => AsDouble(left) * AsDouble(right);"
          "private static object? Divide(object? left, object? right) => AsDouble(left) / AsDouble(right);"
          "private static object? Power(object? left, object? right) => Math.Pow(AsDouble(left), AsDouble(right));"
          "private static object? Concat(object? left, object? right) => AsString(left) + AsString(right);"
          "private static object? IntegerDivide(object? left, object? right) => AsInt(left) / AsInt(right);"
          "private static object? Modulo(object? left, object? right) => AsInt(left) % AsInt(right);"
          "private static object? BitwiseAnd(object? left, object? right) => AsInt(left) & AsInt(right);"
          "private static object? BitwiseOr(object? left, object? right) => AsInt(left) | AsInt(right);"
          "private static object? BitwiseXor(object? left, object? right) => AsInt(left) ^ AsInt(right);"
          "private static int CompareEqual(object? left, object? right) => Equals(left is string || right is string ? AsString(left) : AsDouble(left), left is string || right is string ? AsString(right) : AsDouble(right)) ? 1 : 0;"
          "private static int CompareNotEqual(object? left, object? right) => CompareEqual(left, right) == 0 ? 1 : 0;"
          "private static int CompareLessThan(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) < 0 : AsDouble(left) < AsDouble(right)) ? 1 : 0;"
          "private static int CompareLessThanOrEqual(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) <= 0 : AsDouble(left) <= AsDouble(right)) ? 1 : 0;"
          "private static int CompareGreaterThan(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) > 0 : AsDouble(left) > AsDouble(right)) ? 1 : 0;"
          "private static int CompareGreaterThanOrEqual(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) >= 0 : AsDouble(left) >= AsDouble(right)) ? 1 : 0;"
          "private static int Instr(object? left, object? right) => AsString(left).IndexOf(AsString(right), StringComparison.Ordinal) + 1;"
          "private static object? SliceRange(object? left, object? right) => (AsInt(left), AsInt(right));"
          "private static object? ApplyUnary(string name, object? value) => throw new NotSupportedException($\"Unary operator '{name}' is not supported by the generated backend yet.\");"
          "private static object? ApplyBinary(string name, object? left, object? right) => throw new NotSupportedException($\"Binary operator '{name}' is not supported by the generated backend yet.\");"
          ""
          "private static object? InvokeBuiltInFunction(string name, params object?[] args)"
          "{"
          "    switch (name.ToUpperInvariant())"
          "    {"
          "        case \"ABS\": return Math.Abs(AsDouble(args.ElementAtOrDefault(0)));"
          "        case \"INT\": return (int)Math.Floor(AsDouble(args.ElementAtOrDefault(0)));"
          "        case \"ROUND\": return (int)Math.Round(AsDouble(args.ElementAtOrDefault(0)));"
          "        case \"STR$\": return AsString(args.ElementAtOrDefault(0));"
          "        case \"VAL\": return AsDouble(args.ElementAtOrDefault(0));"
          "        case \"LEFT$\":"
          "        {"
          "            var source = AsString(args.ElementAtOrDefault(0));"
          "            var length = Math.Max(0, Math.Min(source.Length, AsInt(args.ElementAtOrDefault(1))));"
          "            return source.Substring(0, length);"
          "        }"
          "        case \"RIGHT$\":"
          "        {"
          "            var source = AsString(args.ElementAtOrDefault(0));"
          "            var length = Math.Max(0, Math.Min(source.Length, AsInt(args.ElementAtOrDefault(1))));"
          "            return source.Substring(source.Length - length, length);"
          "        }"
          "        case \"DATE\": return (int)DateTimeOffset.UtcNow.ToUnixTimeSeconds();"
          "        default: throw new NotSupportedException($\"Built-in function '{name}' is not supported by the generated backend yet.\");"
          "    }"
          "}"
          ""
          "private static void ExecuteBuiltInStatement(string name, object? channel, params object?[] args)"
          "{"
          "    switch (name.ToUpperInvariant())"
          "    {"
          "        case \"PRINT\":"
          "            Console.WriteLine(string.Join(\" \", args.Select(AsString)));"
          "            break;"
          "        case \"REFERENCE\":"
          "            break;"
          "        case \"STOP\":"
          "            throw new OperationCanceledException(\"STOP encountered.\");"
          "        default:"
          "            if (name.StartsWith(\"TURBO\", StringComparison.OrdinalIgnoreCase))"
          "            {"
          "                return;"
          "            }"
          "            throw new NotSupportedException($\"Built-in statement '{name}' is not supported by the generated backend yet.\");"
          "    }"
          "}"
          ""
          "private static void ExecuteInput(object? channel, object?[] prompts)"
          "{"
          "    if (prompts.Length > 0)"
          "    {"
          "        Console.Write(string.Join(\" \", prompts.Select(AsString)));"
          "    }"
          "    var line = Console.ReadLine() ?? string.Empty;"
          "    __inputBuffer = line.Split(',').Select(part => part.Trim()).ToArray();"
          "}"
          ""
          "private static object? ReadInputValue(int index, string targetType)"
          "{"
          "    var raw = index < __inputBuffer.Length ? __inputBuffer[index] : string.Empty;"
          "    return targetType switch"
          "    {"
          "        \"string\" => raw,"
          "        \"float\" when double.TryParse(raw, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out var d) => d,"
          "        \"float\" => 0.0,"
          "        _ when int.TryParse(raw, NumberStyles.Integer, CultureInfo.InvariantCulture, out var i) => i,"
          "        _ => 0"
          "    };"
          "}"
          ""
          "private static object? ReadDataValue(string targetType)"
          "{"
          "    if (__dataPointer >= __data.Length)"
          "    {"
          "        throw new InvalidOperationException(\"READ moved past the end of DATA.\");"
          "    }"
          "    var value = __data[__dataPointer++];"
          "    return targetType switch"
          "    {"
          "        \"string\" => AsString(value),"
          "        \"float\" => AsDouble(value),"
          "        _ => AsInt(value)"
          "    };"
          "}"
          ""
          "private static void RestoreToLine(int line)"
          "{"
          "    if (!__restorePoints.TryGetValue(line, out var slot))"
          "    {"
          "        throw new InvalidOperationException($\"RESTORE line {line} does not exist.\");"
          "    }"
          "    __dataPointer = slot;"
          "}" ]

    helpers |> List.iter (appendLine builder 1)

let generateCSharpFromHir className (program: HirProgram) =
    let ctx = buildContext className program
    let builder = StringBuilder()

    appendLine builder 0 "using System;"
    appendLine builder 0 "using System.Collections.Generic;"
    appendLine builder 0 "using System.Globalization;"
    appendLine builder 0 "using System.Linq;"
    appendLine builder 0 ""
    appendLine builder 0 $"public static class {ctx.ClassName}"
    appendLine builder 0 "{"
    emitStorageDeclarations ctx builder 1 program.Globals
    if not program.Globals.IsEmpty then
        appendLine builder 0 ""
    emitDataLayout builder program
    appendLine builder 0 ""
    emitRuntimeHelpers builder
    appendLine builder 0 ""
    program.Routines |> List.iter (emitRoutine ctx builder)
    if not program.Routines.IsEmpty then
        appendLine builder 0 ""
    appendLine builder 1 "public static void Main()"
    appendLine builder 1 "{"
    emitBlock ctx builder 2 program.Main
    appendLine builder 1 "}"
    appendLine builder 0 "}"

    builder.ToString()
