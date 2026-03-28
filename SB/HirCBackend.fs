module HirCBackend

open System
open System.Globalization
open System.Text

open HIR

type private EmitterContext = {
    ProgramName: string
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

let private buildContext programName (program: HirProgram) =
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

    { ProgramName = if String.IsNullOrWhiteSpace programName then "generated_program" else sanitizeIdentifier programName
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

let rec private emitExpr ctx = function
    | Literal(ConstInt value, _, _) -> $"make_int({value})"
    | Literal(ConstFloat value, _, _) ->
        let rendered = value.ToString("G17", CultureInfo.InvariantCulture)
        $"make_float({rendered})"
    | Literal(ConstString value, _, _) -> $"make_string(\"{escapeCString value}\")"
    | ReadVar(symbolId, _, _) -> storageName ctx symbolId
    | ReadArrayElem(symbolId, indexes, _, _) ->
        emitCall "get_array_value" ([ "&" + storageName ctx symbolId; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx)))
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
        let renderedArgs = args |> List.map (emitExpr ctx)
        if Set.contains symbolId ctx.RoutineSymbols then
            emitCall (routineName ctx symbolId) renderedArgs
        else
            emitCall "invoke_builtin_function" ($"\"{escapeCString (builtInName ctx symbolId)}\"" :: renderedArgs)

let private emitTargetWrite ctx target valueExpr =
    match target with
    | WriteVar(symbolId, _, _) -> $"{storageName ctx symbolId} = {valueExpr};"
    | WriteArrayElem(symbolId, indexes, _, _) ->
        emitCall "set_array_value" ([ "&" + storageName ctx symbolId; valueExpr; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx))) + ";"

let rec private emitBlock ctx builder level block =
    block |> List.iter (emitStmt ctx builder level)

and private emitIf ctx builder level condition thenBlock elseBlock =
    appendLine builder level $"if (is_true({emitExpr ctx condition}))"
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
    let counter = storageName ctx symbolId
    let indexName = $"loop_{id}_index"
    let endName = $"loop_{id}_end"
    let stepName = $"loop_{id}_step"
    appendLine builder level "{"
    appendLine builder (level + 1) $"int {stepName} = as_int({emitExpr ctx stepExpr});"
    appendLine builder (level + 1) $"int {endName} = as_int({emitExpr ctx endExpr});"
    appendLine builder (level + 1) $"for (int {indexName} = as_int({emitExpr ctx startExpr}); ({stepName} >= 0) ? ({indexName} <= {endName}) : ({indexName} >= {endName}); {indexName} += {stepName})"
    appendLine builder (level + 1) "{"
    appendLine builder (level + 2) $"{counter} = make_int({indexName});"
    emitBlock ctx builder (level + 2) body
    appendLine builder (level + 2) $"loop_{id}_next: ;"
    appendLine builder (level + 1) "}"
    appendLine builder (level + 1) $"loop_{id}_exit: ;"
    appendLine builder level "}"

and private emitRepeat ctx builder level loopId body =
    let (LoopId id) = loopId
    appendLine builder level "while (1)"
    appendLine builder level "{"
    emitBlock ctx builder (level + 1) body
    appendLine builder (level + 1) $"loop_{id}_next: ;"
    appendLine builder level "}"
    appendLine builder level $"loop_{id}_exit: ;"

and private emitOnGotoLike keyword ctx builder level selector targets =
    appendLine builder level $"switch (as_int({emitExpr ctx selector}))"
    appendLine builder level "{"
    targets
    |> List.iteri (fun index target ->
        match tryGetConstInt target with
        | Some line when keyword = "GOTO" ->
            appendLine builder (level + 1) $"case {index + 1}: goto line_{line};"
        | Some _ ->
            appendLine builder (level + 1) $"case {index + 1}: runtime_not_supported(\"{keyword} is not supported by the generated C backend yet.\"); break;"
        | None ->
            appendLine builder (level + 1) $"case {index + 1}: runtime_not_supported(\"Dynamic {keyword} is not supported by the generated C backend yet.\"); break;")
    appendLine builder (level + 1) "default: break;"
    appendLine builder level "}"

and private emitStmt ctx builder level stmt =
    match stmt with
    | Assign(target, value, _) ->
        appendLine builder level (emitTargetWrite ctx target (emitExpr ctx value))
    | ProcCall(symbolId, _, args, _) ->
        appendLine builder level (emitCall (routineName ctx symbolId) (args |> List.map (emitExpr ctx)) + ";")
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
            | Some expr -> emitExpr ctx expr
            | None -> "make_null()"
        appendLine builder level (emitCall "execute_builtin_statement" ([ $"\"{escapeCString kindName}\""; channelExpr; string args.Length ] @ (args |> List.map (emitExpr ctx))) + ";")
    | Input(channel, prompts, targets, _) ->
        let channelExpr =
            match channel with
            | Some expr -> emitExpr ctx expr
            | None -> "make_null()"
        appendLine builder level (emitCall "execute_input" ([ channelExpr; string prompts.Length ] @ (prompts |> List.map (emitExpr ctx))) + ";")
        targets
        |> List.iteri (fun index target ->
            let valueExpr =
                match target with
                | WriteVar(_, typ, _)
                | WriteArrayElem(_, _, typ, _) -> $"read_input_value({index}, {typeTag typ})"
            appendLine builder level (emitTargetWrite ctx target valueExpr))
    | If(condition, thenBlock, elseBlock, _) ->
        emitIf ctx builder level condition thenBlock elseBlock
    | For(loopId, symbolId, startExpr, endExpr, stepExpr, body, _) ->
        emitFor ctx builder level loopId symbolId startExpr endExpr stepExpr body
    | Repeat(loopId, _, body, _) ->
        emitRepeat ctx builder level loopId body
    | Exit(loopId, _) ->
        let (LoopId id) = loopId
        appendLine builder level $"goto loop_{id}_exit;"
    | Next(loopId, _) ->
        let (LoopId id) = loopId
        appendLine builder level $"goto loop_{id}_next;"
    | Goto(target, _) ->
        match tryGetConstInt target with
        | Some line -> appendLine builder level $"goto line_{line};"
        | None -> appendLine builder level "runtime_not_supported(\"Dynamic GOTO is not supported by the generated C backend yet.\");"
    | OnGoto(selector, targets, _) ->
        emitOnGotoLike "GOTO" ctx builder level selector targets
    | Gosub(_, _) ->
        appendLine builder level "runtime_not_supported(\"GOSUB is not supported by the generated C backend yet.\");"
    | OnGosub(selector, targets, _) ->
        emitOnGotoLike "GOSUB" ctx builder level selector targets
    | Return(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"return {emitExpr ctx expr};"
        | None -> appendLine builder level "return make_null();"
    | LineNumber(value, _) ->
        appendLine builder level $"line_{value}: ;"
    | Restore(value, _) ->
        match value with
        | Some expr -> appendLine builder level $"restore_to_line(as_int({emitExpr ctx expr}));"
        | None -> appendLine builder level "__data_pointer = 0;"
    | Read(targets, _) ->
        targets
        |> List.iter (fun target ->
            let valueExpr =
                match target with
                | WriteVar(_, typ, _)
                | WriteArrayElem(_, _, typ, _) -> $"read_data_value({typeTag typ})"
            appendLine builder level (emitTargetWrite ctx target valueExpr))
    | Remark(text, _) ->
        appendLine builder level $"/* {escapeCString text} */"

let private emitStorageDeclarations (ctx: EmitterContext) (builder: StringBuilder) level (storages: HirStorage list) =
    storages |> List.iter (fun storage -> appendLine builder level $"static Value {storageName ctx storage.Symbol};")

let private emitRoutinePrototype (ctx: EmitterContext) (builder: StringBuilder) level (routine: HirRoutine) =
    let parameters =
        match routine.Parameters with
        | [] -> "void"
        | items -> items |> List.map (fun storage -> $"Value {storageName ctx storage.Symbol}") |> String.concat ", "
    appendLine builder level $"static Value {routineName ctx routine.Symbol}({parameters});"

let private emitRoutine (ctx: EmitterContext) (builder: StringBuilder) (routine: HirRoutine) =
    let parameters =
        match routine.Parameters with
        | [] -> "void"
        | items -> items |> List.map (fun storage -> $"Value {storageName ctx storage.Symbol}") |> String.concat ", "
    appendLine builder 0 $"static Value {routineName ctx routine.Symbol}({parameters})"
    appendLine builder 0 "{"
    routine.Locals
    |> List.iter (fun storage ->
        let initial =
            match storage.Type with
            | Array _ -> "make_array()"
            | _ -> "make_null()"
        appendLine builder 1 $"Value {storageName ctx storage.Symbol} = {initial};")
    emitBlock ctx builder 1 routine.Body
    appendLine builder 1 "return make_null();"
    appendLine builder 0 "}"
    appendLine builder 0 ""

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
    appendLine builder 0 $"static DataValue __data[] = {{ {dataItems} }};"
    appendLine builder 0 $"static int __data_count = {program.DataEntries.Length};"
    appendLine builder 0 $"static RestorePoint __restore_points[] = {{ {restorePoints} }};"
    appendLine builder 0 $"static int __restore_point_count = {program.RestorePoints.Length};"
    appendLine builder 0 "static int __data_pointer = 0;"
    appendLine builder 0 "static char __input_buffer[4096];"
    appendLine builder 0 "static char* __input_parts[256];"
    appendLine builder 0 "static int __input_part_count = 0;"
    appendLine builder 0 ""

let private emitRuntime builder =
    let lines =
        [ "#include <ctype.h>"
          "#include <math.h>"
          "#include <stdarg.h>"
          "#include <stdio.h>"
          "#include <stdlib.h>"
          "#include <string.h>"
          ""
          "typedef struct ArrayEntry ArrayEntry;"
          "typedef enum ValueType { TYPE_NULL, TYPE_INT, TYPE_FLOAT, TYPE_STRING, TYPE_ARRAY } ValueType;"
          "typedef struct Value { ValueType type; int int_value; double float_value; const char* string_value; ArrayEntry* array_value; } Value;"
          "struct ArrayEntry { char* key; Value value; ArrayEntry* next; };"
          "typedef Value DataValue;"
          "typedef struct RestorePoint { int line; int slot; } RestorePoint;"
          ""
          "static char* duplicate_string(const char* source)"
          "{"
          "    size_t length = strlen(source) + 1;"
          "    char* copy = (char*)malloc(length);"
          "    memcpy(copy, source, length);"
          "    return copy;"
          "}"
          "static int ci_compare(const char* left, const char* right)"
          "{"
          "    while (*left && *right)"
          "    {"
          "        int diff = toupper((unsigned char)*left) - toupper((unsigned char)*right);"
          "        if (diff != 0) return diff;"
          "        left++;"
          "        right++;"
          "    }"
          "    return toupper((unsigned char)*left) - toupper((unsigned char)*right);"
          "}"
          "static int ci_starts_with(const char* value, const char* prefix)"
          "{"
          "    while (*prefix)"
          "    {"
          "        if (toupper((unsigned char)*value) != toupper((unsigned char)*prefix)) return 0;"
          "        value++;"
          "        prefix++;"
          "    }"
          "    return 1;"
          "}"
          "static Value make_null(void) { Value v = { TYPE_NULL, 0, 0.0, NULL, NULL }; return v; }"
          "static Value make_int(int value) { Value v = { TYPE_INT, value, (double)value, NULL, NULL }; return v; }"
          "static Value make_float(double value) { Value v = { TYPE_FLOAT, 0, value, NULL, NULL }; return v; }"
          "static Value make_string(const char* value) { Value v = { TYPE_STRING, 0, 0.0, value, NULL }; return v; }"
          "static Value make_array(void) { Value v = { TYPE_ARRAY, 0, 0.0, NULL, NULL }; return v; }"
          "static void runtime_not_supported(const char* message) { fprintf(stderr, \"%s\\n\", message); exit(1); }"
          "static int as_int(Value value)"
          "{"
          "    switch (value.type)"
          "    {"
          "        case TYPE_INT: return value.int_value;"
          "        case TYPE_FLOAT: return (int)llround(value.float_value);"
          "        case TYPE_STRING: return value.string_value ? atoi(value.string_value) : 0;"
          "        default: return 0;"
          "    }"
          "}"
          "static double as_double(Value value)"
          "{"
          "    switch (value.type)"
          "    {"
          "        case TYPE_INT: return (double)value.int_value;"
          "        case TYPE_FLOAT: return value.float_value;"
          "        case TYPE_STRING: return value.string_value ? atof(value.string_value) : 0.0;"
          "        default: return 0.0;"
          "    }"
          "}"
          "static const char* as_string(Value value)"
          "{"
          "    static char buffers[8][128];"
          "    static int next_buffer = 0;"
          "    char* buffer = buffers[next_buffer++ % 8];"
          "    switch (value.type)"
          "    {"
          "        case TYPE_STRING: return value.string_value ? value.string_value : \"\";"
          "        case TYPE_INT: snprintf(buffer, 128, \"%d\", value.int_value); return buffer;"
          "        case TYPE_FLOAT: snprintf(buffer, 128, \"%.17g\", value.float_value); return buffer;"
          "        case TYPE_NULL: return \"\";"
          "        default: return \"<array>\";"
          "    }"
          "}"
          "static int is_true(Value value)"
          "{"
          "    if (value.type == TYPE_STRING) return value.string_value && value.string_value[0] != '\\0';"
          "    if (value.type == TYPE_ARRAY) return 1;"
          "    return fabs(as_double(value)) > 0.0000001;"
          "}"
          "static Value negate_value(Value value) { return make_float(-as_double(value)); }"
          "static Value bitwise_not_value(Value value) { return make_int(~as_int(value)); }"
          "static Value concat_value(Value left, Value right)"
          "{"
          "    static char buffers[8][512];"
          "    static int next_buffer = 0;"
          "    char* buffer = buffers[next_buffer++ % 8];"
          "    snprintf(buffer, 512, \"%s%s\", as_string(left), as_string(right));"
          "    return make_string(buffer);"
          "}"
          "static Value add_value(Value left, Value right) { return (left.type == TYPE_STRING || right.type == TYPE_STRING) ? concat_value(left, right) : make_float(as_double(left) + as_double(right)); }"
          "static Value subtract_value(Value left, Value right) { return make_float(as_double(left) - as_double(right)); }"
          "static Value multiply_value(Value left, Value right) { return make_float(as_double(left) * as_double(right)); }"
          "static Value divide_value(Value left, Value right) { return make_float(as_double(left) / as_double(right)); }"
          "static Value power_value(Value left, Value right) { return make_float(pow(as_double(left), as_double(right))); }"
          "static Value integer_divide_value(Value left, Value right) { return make_int(as_int(left) / as_int(right)); }"
          "static Value modulo_value(Value left, Value right) { return make_int(as_int(left) % as_int(right)); }"
          "static Value bitwise_and_value(Value left, Value right) { return make_int(as_int(left) & as_int(right)); }"
          "static Value bitwise_or_value(Value left, Value right) { return make_int(as_int(left) | as_int(right)); }"
          "static Value bitwise_xor_value(Value left, Value right) { return make_int(as_int(left) ^ as_int(right)); }"
          "static Value compare_equal(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) == 0 : fabs(as_double(left) - as_double(right)) < 0.0000001); }"
          "static Value compare_not_equal(Value left, Value right) { return make_int(as_int(compare_equal(left, right)) == 0); }"
          "static Value compare_less_than(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) < 0 : as_double(left) < as_double(right)); }"
          "static Value compare_less_than_or_equal(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) <= 0 : as_double(left) <= as_double(right)); }"
          "static Value compare_greater_than(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) > 0 : as_double(left) > as_double(right)); }"
          "static Value compare_greater_than_or_equal(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) >= 0 : as_double(left) >= as_double(right)); }"
          "static Value instr_value(Value left, Value right) { const char* source = as_string(left); const char* found = strstr(source, as_string(right)); return make_int(found ? (int)(found - source) + 1 : 0); }"
          "static Value slice_range_value(Value left, Value right) { (void)left; return right; }"
          "static Value unsupported_unary(const char* name, Value value) { runtime_not_supported(name); return value; }"
          "static Value unsupported_binary(const char* name, Value left, Value right) { runtime_not_supported(name); return left; }"
          "static char* build_array_key(int count, va_list args)"
          "{"
          "    static char buffer[256];"
          "    buffer[0] = '\\0';"
          "    for (int i = 0; i < count; i++)"
          "    {"
          "        Value value = va_arg(args, Value);"
          "        char part[32];"
          "        snprintf(part, sizeof(part), i == 0 ? \"%d\" : \",%d\", as_int(value));"
          "        strncat(buffer, part, sizeof(buffer) - strlen(buffer) - 1);"
          "    }"
          "    return buffer;"
          "}"
          "static Value get_array_value(Value* array_value, int count, ...)"
          "{"
          "    if (array_value->type != TYPE_ARRAY) *array_value = make_array();"
          "    va_list args;"
          "    va_start(args, count);"
          "    char* key = build_array_key(count, args);"
          "    va_end(args);"
          "    for (ArrayEntry* entry = array_value->array_value; entry; entry = entry->next)"
          "        if (strcmp(entry->key, key) == 0) return entry->value;"
          "    return make_null();"
          "}"
          "static void set_array_value(Value* array_value, Value value, int count, ...)"
          "{"
          "    if (array_value->type != TYPE_ARRAY) *array_value = make_array();"
          "    va_list args;"
          "    va_start(args, count);"
          "    char* key = build_array_key(count, args);"
          "    va_end(args);"
          "    for (ArrayEntry* entry = array_value->array_value; entry; entry = entry->next)"
          "    {"
          "        if (strcmp(entry->key, key) == 0) { entry->value = value; return; }"
          "    }"
          "    ArrayEntry* entry = (ArrayEntry*)calloc(1, sizeof(ArrayEntry));"
          "    entry->key = duplicate_string(key);"
          "    entry->value = value;"
          "    entry->next = array_value->array_value;"
          "    array_value->array_value = entry;"
          "}"
          "static Value invoke_builtin_function(const char* name, ...)"
          "{"
          "    va_list args;"
          "    va_start(args, name);"
          "    if (ci_compare(name, \"ABS\") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(fabs(as_double(v))); }"
          "    if (ci_compare(name, \"INT\") == 0) { Value v = va_arg(args, Value); va_end(args); return make_int((int)floor(as_double(v))); }"
          "    if (ci_compare(name, \"ROUND\") == 0) { Value v = va_arg(args, Value); va_end(args); return make_int((int)llround(as_double(v))); }"
          "    if (ci_compare(name, \"STR$\") == 0) { Value v = va_arg(args, Value); va_end(args); return make_string(as_string(v)); }"
          "    if (ci_compare(name, \"VAL\") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(atof(as_string(v))); }"
          "    va_end(args);"
          "    runtime_not_supported(\"Built-in function is not supported by the generated C backend yet.\");"
          "    return make_null();"
          "}"
          "static void execute_builtin_statement(const char* name, Value channel, int arg_count, ...)"
          "{"
          "    (void)channel;"
          "    (void)arg_count;"
          "    if (ci_compare(name, \"REFERENCE\") == 0) return;"
          "    if (ci_starts_with(name, \"TURBO\")) return;"
          "    runtime_not_supported(\"Built-in statement is not supported by the generated C backend yet.\");"
          "}"
          "static void execute_input(Value channel, int prompt_count, ...)"
          "{"
          "    (void)channel;"
          "    va_list args;"
          "    va_start(args, prompt_count);"
          "    for (int i = 0; i < prompt_count; i++)"
          "    {"
          "        Value prompt = va_arg(args, Value);"
          "        fputs(as_string(prompt), stdout);"
          "        if (i + 1 < prompt_count) fputc(' ', stdout);"
          "    }"
          "    va_end(args);"
          "    if (fgets(__input_buffer, sizeof(__input_buffer), stdin) == NULL) __input_buffer[0] = '\\0';"
          "    __input_part_count = 0;"
          "    char* token = strtok(__input_buffer, \",\\r\\n\");"
          "    while (token && __input_part_count < 256)"
          "    {"
          "        while (*token == ' ') token++;"
          "        __input_parts[__input_part_count++] = token;"
          "        token = strtok(NULL, \",\\r\\n\");"
          "    }"
          "}"
          "static Value read_input_value(int index, ValueType target_type)"
          "{"
          "    const char* raw = index < __input_part_count ? __input_parts[index] : \"\";"
          "    switch (target_type)"
          "    {"
          "        case TYPE_STRING: return make_string(raw);"
          "        case TYPE_FLOAT: return make_float(atof(raw));"
          "        default: return make_int(atoi(raw));"
          "    }"
          "}"
          "static Value read_data_value(ValueType target_type)"
          "{"
          "    if (__data_pointer >= __data_count) runtime_not_supported(\"READ moved past the end of DATA.\");"
          "    Value value = __data[__data_pointer++];"
          "    switch (target_type)"
          "    {"
          "        case TYPE_STRING: return make_string(as_string(value));"
          "        case TYPE_FLOAT: return make_float(as_double(value));"
          "        default: return make_int(as_int(value));"
          "    }"
          "}"
          "static void restore_to_line(int line)"
          "{"
          "    for (int i = 0; i < __restore_point_count; i++)"
          "    {"
          "        if (__restore_points[i].line == line) { __data_pointer = __restore_points[i].slot; return; }"
          "    }"
          "    runtime_not_supported(\"RESTORE target does not exist in generated C backend.\");"
          "}" ]
    lines |> List.iter (appendLine builder 0)
    appendLine builder 0 ""

let generateCFromHir programName (program: HirProgram) =
    let ctx = buildContext programName program
    let builder = StringBuilder()

    emitRuntime builder
    emitDataLayout builder program
    emitStorageDeclarations ctx builder 0 program.Globals
    if not (List.isEmpty program.Globals) then
        appendLine builder 0 ""
    program.Routines |> List.iter (emitRoutinePrototype ctx builder 0)
    if not (List.isEmpty program.Routines) then
        appendLine builder 0 ""
    program.Routines |> List.iter (emitRoutine ctx builder)
    appendLine builder 0 "int main(void)"
    appendLine builder 0 "{"
    program.Globals
    |> List.iter (fun storage ->
        let initial =
            match storage.Type with
            | Array _ -> "make_array()"
            | _ -> "make_null()"
        appendLine builder 1 $"{storageName ctx storage.Symbol} = {initial};")
    emitBlock ctx builder 1 program.Main
    appendLine builder 1 "return 0;"
    appendLine builder 0 "}"

    builder.ToString()
