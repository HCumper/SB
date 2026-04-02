module HirCBackend

open System
open System.Globalization
open System.IO
open System.Text

open HIR

type private EmitterContext = {
    ProgramName: string
    SymbolNames: Map<SymbolId, string>
    StorageNames: Map<SymbolId, string>
    RoutineNames: Map<SymbolId, string>
    RoutineSymbols: Set<SymbolId>
    ReferencedLineNumbers: Set<int>
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
        | OnGoto(_, targets, _) ->
            targets
            |> List.choose tryGetConstInt
            |> Set.ofList
        | Restore(Some target, _) ->
            tryGetConstInt target |> Option.map Set.singleton |> Option.defaultValue Set.empty
        | _ ->
            Set.empty

    block |> List.fold (fun acc stmt -> Set.union acc (collectFromStmt stmt)) Set.empty

let private buildContext programName (program: HirProgram) =
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

        restoreLines
        |> Set.union (collectReferencedLineNumbers program.Main)
        |> Set.union routineLines

    { ProgramName = if String.IsNullOrWhiteSpace programName then "generated_program" else sanitizeIdentifier programName
      SymbolNames = program.SymbolNames
      StorageNames = Map.ofList (globals @ routineStorage)
      RoutineNames = Map.ofList routines
      RoutineSymbols = routines |> List.map fst |> Set.ofList
      ReferencedLineNumbers = referencedLineNumbers }

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

let rec private emitTargetRead ctx = function
    | WriteVar(symbolId, _, _) -> storageName ctx symbolId
    | WriteArrayElem(symbolId, indexes, _, _) ->
        emitCall "get_array_value" ([ "&" + storageName ctx symbolId; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx)))
    | WriteStringChar(symbolId, index, _, _) ->
        emitCall "get_string_char_value" [ storageName ctx symbolId; $"as_int({emitExpr ctx index})" ]
    | DynamicWriteVar(name, _, _) -> $"unsupported_dynamic_write(\"{escapeCString name}\")"
    | DynamicWriteArrayElem(name, _, _, _) -> $"unsupported_dynamic_write(\"{escapeCString name}\")"
    | DynamicWriteStringChar(name, _, _, _) -> $"unsupported_dynamic_write(\"{escapeCString name}\")"

and private emitCallArg ctx = function
    | ValueArg expr -> emitExpr ctx expr
    | RefArg target -> emitTargetRead ctx target

and private emitExpr ctx = function
    | Literal(ConstInt value, _, _) -> $"make_int({value})"
    | Literal(ConstFloat value, _, _) ->
        let rendered = value.ToString("G17", CultureInfo.InvariantCulture)
        $"make_float({rendered})"
    | Literal(ConstString value, _, _) -> $"make_string(\"{escapeCString value}\")"
    | ReadVar(symbolId, _, _) -> storageName ctx symbolId
    | ReadArrayElem(symbolId, indexes, _, _) ->
        emitCall "get_array_value" ([ "&" + storageName ctx symbolId; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx)))
    | ReadStringChar(symbolId, index, _, _) ->
        emitCall "get_string_char_value" [ storageName ctx symbolId; $"as_int({emitExpr ctx index})" ]
    | DynamicReadVar(name, _, _) -> $"unsupported_dynamic_read(\"{escapeCString name}\")"
    | DynamicReadArrayElem(name, _, _, _) -> $"unsupported_dynamic_read(\"{escapeCString name}\")"
    | DynamicReadStringChar(name, _, _, _) -> $"unsupported_dynamic_read(\"{escapeCString name}\")"
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
        let renderedArgs = args |> List.map (emitCallArg ctx)
        if Set.contains symbolId ctx.RoutineSymbols then
            emitCall (routineName ctx symbolId) renderedArgs
        else
            emitCall "invoke_builtin_function" ([ $"\"{escapeCString (builtInName ctx symbolId)}\""; string args.Length ] @ renderedArgs)

let private emitTargetWrite ctx target valueExpr =
    match target with
    | WriteVar(symbolId, _, _) -> $"{storageName ctx symbolId} = {valueExpr};"
    | WriteArrayElem(symbolId, indexes, _, _) ->
        emitCall "set_array_value" ([ "&" + storageName ctx symbolId; valueExpr; string indexes.Length ] @ (indexes |> List.map (emitExpr ctx))) + ";"
    | WriteStringChar(symbolId, index, _, _) ->
        emitCall "set_string_char_value" [ "&" + storageName ctx symbolId; $"as_int({emitExpr ctx index})"; valueExpr ] + ";"
    | DynamicWriteVar(name, _, _) -> $"runtime_not_supported(\"Dynamic scoped write '{escapeCString name}' is not supported by the generated C backend yet.\");"
    | DynamicWriteArrayElem(name, _, _, _) -> $"runtime_not_supported(\"Dynamic scoped array write '{escapeCString name}' is not supported by the generated C backend yet.\");"
    | DynamicWriteStringChar(name, _, _, _) -> $"runtime_not_supported(\"Dynamic scoped string write '{escapeCString name}' is not supported by the generated C backend yet.\");"

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
        appendLine builder level (emitCall (routineName ctx symbolId) (args |> List.map (emitCallArg ctx)) + ";")
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
    | WhenError(_, _) ->
        appendLine builder level "runtime_not_supported(\"WHEN ERROR is only supported by the interpreter.\");"
    | If(condition, thenBlock, elseBlock, _) ->
        emitIf ctx builder level condition thenBlock elseBlock
    | For(loopId, symbolId, startExpr, endExpr, stepExpr, body, _) ->
        emitFor ctx builder level loopId symbolId startExpr endExpr stepExpr body
    | ForSequence(_, _, _, _, _, _, _, _, _) ->
        appendLine builder level "runtime_not_supported(\"Sequence FOR loops are not supported by the generated C backend yet.\");"
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
        if Set.contains value ctx.ReferencedLineNumbers then
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

let private emitStorageDeclarations (ctx: EmitterContext) (builder: StringBuilder) level (storages: HirStorage list) =
    storages |> List.iter (fun storage -> appendLine builder level $"static Value {storageName ctx storage.Symbol};")

let private emitRoutinePrototype (ctx: EmitterContext) (builder: StringBuilder) level (routine: HirRoutine) =
    let parameters =
        match routine.Parameters with
        | [] -> "void"
        | items -> items |> List.map (fun parameter -> $"Value {storageName ctx parameter.Storage.Symbol}") |> String.concat ", "
    appendLine builder level $"static Value {routineName ctx routine.Symbol}({parameters});"

let private emitRoutine (ctx: EmitterContext) (builder: StringBuilder) (routine: HirRoutine) =
    let parameters =
        match routine.Parameters with
        | [] -> "void"
        | items -> items |> List.map (fun parameter -> $"Value {storageName ctx parameter.Storage.Symbol}") |> String.concat ", "
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
    appendLine builder 0 $"static DataValue sb_data[] = {{ {dataItems} }};"
    appendLine builder 0 $"static int sb_data_count = {program.DataEntries.Length};"
    appendLine builder 0 $"static RestorePoint sb_restore_points[] = {{ {restorePoints} }};"
    appendLine builder 0 $"static int sb_restore_point_count = {program.RestorePoints.Length};"
    appendLine builder 0 "static int sb_data_pointer = 0;"
    appendLine builder 0 "static char sb_input_buffer[4096];"
    appendLine builder 0 "static char* sb_input_parts[256];"
    appendLine builder 0 "static int sb_input_part_count = 0;"
    appendLine builder 0 ""

let private getTemplateLines templateName =
    let templatePath = Path.Combine(__SOURCE_DIRECTORY__, "CTemplates.stg")
    let lines = File.ReadAllLines(templatePath)
    let marker = $"{templateName}() ::= <<"
    let startIndex =
        lines
        |> Array.tryFindIndex (fun line -> line.Trim() = marker)
        |> Option.defaultWith (fun () -> failwith $"Failed to find template '{templateName}' in CTemplates.stg.")

    let content =
        lines
        |> Array.skip (startIndex + 1)
        |> Array.takeWhile (fun line -> line.Trim() <> ">>")

    content

let private emitTemplate (builder: StringBuilder) templateName =
    getTemplateLines templateName
    |> Array.iter (fun line -> builder.AppendLine(line) |> ignore)
    builder.AppendLine() |> ignore

let cRuntimeHeaderFileName = "sbruntime_c.h"
let cRuntimeSourceFileName = "sbruntime_c.c"

let generateCRuntimeHeader () =
    let builder = StringBuilder()
    builder.AppendLine("#ifndef SBRUNTIME_C_H") |> ignore
    builder.AppendLine("#define SBRUNTIME_C_H") |> ignore
    builder.AppendLine() |> ignore
    getTemplateLines "runtimePrelude"
    |> Array.iter (fun line -> builder.AppendLine(line) |> ignore)
    builder.AppendLine() |> ignore
    builder.AppendLine("extern DataValue sb_data[];") |> ignore
    builder.AppendLine("extern int sb_data_count;") |> ignore
    builder.AppendLine("extern RestorePoint sb_restore_points[];") |> ignore
    builder.AppendLine("extern int sb_restore_point_count;") |> ignore
    builder.AppendLine("extern int sb_data_pointer;") |> ignore
    builder.AppendLine("extern char sb_input_buffer[4096];") |> ignore
    builder.AppendLine("extern char* sb_input_parts[256];") |> ignore
    builder.AppendLine("extern int sb_input_part_count;") |> ignore
    builder.AppendLine() |> ignore
    [ "Value make_null(void);"
      "Value make_int(int value);"
      "Value make_float(double value);"
      "Value make_string(const char* value);"
      "Value make_array(void);"
      "void runtime_not_supported(const char* message);"
      "Value unsupported_dynamic_read(const char* name);"
      "Value unsupported_dynamic_write(const char* name);"
      "int as_int(Value value);"
      "double as_double(Value value);"
      "const char* as_string(Value value);"
      "int is_true(Value value);"
      "Value negate_value(Value value);"
      "Value bitwise_not_value(Value value);"
      "Value concat_value(Value left, Value right);"
      "Value add_value(Value left, Value right);"
      "Value subtract_value(Value left, Value right);"
      "Value multiply_value(Value left, Value right);"
      "Value divide_value(Value left, Value right);"
      "Value power_value(Value left, Value right);"
      "Value integer_divide_value(Value left, Value right);"
      "Value modulo_value(Value left, Value right);"
      "Value bitwise_and_value(Value left, Value right);"
      "Value bitwise_or_value(Value left, Value right);"
      "Value bitwise_xor_value(Value left, Value right);"
      "Value compare_equal(Value left, Value right);"
      "Value compare_not_equal(Value left, Value right);"
      "Value compare_less_than(Value left, Value right);"
      "Value compare_less_than_or_equal(Value left, Value right);"
      "Value compare_greater_than(Value left, Value right);"
      "Value compare_greater_than_or_equal(Value left, Value right);"
      "Value instr_value(Value left, Value right);"
      "Value slice_range_value(Value left, Value right);"
      "Value unsupported_unary(const char* name, Value value);"
      "Value unsupported_binary(const char* name, Value left, Value right);"
      "Value get_array_value(Value* array_value, int count, ...);"
      "void set_array_value(Value* array_value, Value value, int count, ...);"
      "Value get_string_char_value(Value source, int one_based_index);"
      "void set_string_char_value(Value* target, int one_based_index, Value replacement);"
      "Value invoke_builtin_function(const char* name, int arg_count, ...);"
      "void execute_builtin_statement(const char* name, Value channel, int arg_count, ...);"
      "void execute_input(Value channel, int prompt_count, ...);"
      "Value read_input_value(int index, ValueType target_type);"
      "Value read_data_value(ValueType target_type);"
      "void restore_to_line(int line);" ]
    |> List.iter (fun line -> builder.AppendLine(line) |> ignore)
    builder.AppendLine() |> ignore
    builder.AppendLine("#endif") |> ignore
    builder.ToString()

let generateCRuntimeSource () =
    let builder = StringBuilder()
    builder.AppendLine($"#include \"{cRuntimeHeaderFileName}\"") |> ignore
    builder.AppendLine() |> ignore
    emitTemplate builder "runtimeSupport"
    builder.ToString()

let generateCFromHir programName (program: HirProgram) =
    let ctx = buildContext programName program
    let builder = StringBuilder()

    builder.AppendLine($"#include \"{cRuntimeHeaderFileName}\"") |> ignore
    builder.AppendLine() |> ignore
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
