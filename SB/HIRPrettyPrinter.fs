module HIRPrettyPrinter

open System
open System.Text

open Types
open HIR

let private indent level = String.replicate (level * 2) " "

let private appendLine (builder: StringBuilder) level (text: string) =
    builder.Append(indent level).Append(text).Append('\n') |> ignore

let private formatPosition (position: SourcePosition) =
    match position.BasicLineNo with
    | Some basicLine -> $"{basicLine}:{position.EditorLineNo}:{position.Column}"
    | None -> $"{position.EditorLineNo}:{position.Column}"

let private formatSymbolId (SymbolId id) = $"s{id}"
let private formatLoopId (LoopId id) = $"loop{id}"
let private formatSlotId (StorageSlotId id) = $"slot{id}"
let private formatDataSlotId (DataSlotId id) = $"data{id}"

let rec private formatType hirType =
    match hirType with
    | Int -> "Int"
    | Float -> "Float"
    | String -> "String"
    | Void -> "Void"
    | Array inner -> $"Array<{formatType inner}>"

let private formatStorageClass = function
    | GlobalStorage -> "global"
    | RoutineLocalStorage scope -> $"local({scope})"
    | RoutineParameterStorage scope -> $"param({scope})"

let private formatConst = function
    | ConstInt value -> string value
    | ConstFloat value -> value.ToString("G17", Globalization.CultureInfo.InvariantCulture)
    | ConstString value -> $"\"{value}\""

let private formatUnaryOp = function
    | Identity -> "+"
    | Negate -> "-"
    | BitwiseNot -> "NOT"
    | UnaryUnknown value -> value

let private formatBinaryOp = function
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Power -> "^"
    | Concat -> "&"
    | IntegerDivide -> "DIV"
    | Modulo -> "MOD"
    | BitwiseAnd -> "AND"
    | BitwiseOr -> "OR"
    | BitwiseXor -> "XOR"
    | Equal -> "="
    | NotEqual -> "<>"
    | LessThan -> "<"
    | LessThanOrEqual -> "<="
    | GreaterThan -> ">"
    | GreaterThanOrEqual -> ">="
    | Instr -> "INSTR"
    | SliceRange -> "TO"
    | BinaryUnknown value -> value

let private formatBuiltInKind = function
    | Reference -> "REFERENCE"
    | BuiltInKind.Input -> "INPUT"
    | Print -> "PRINT"
    | GotoBuiltIn -> "GOTO"
    | GosubBuiltIn -> "GOSUB"
    | OnGotoBuiltIn -> "ON-GOTO"
    | OnGosubBuiltIn -> "ON-GOSUB"
    | NamedBuiltIn name -> name

let private formatLoopName = function
    | AnonymousLoop -> "<anonymous>"
    | NamedLoop name -> name

let rec private formatExpr expr =
    match expr with
    | Literal(value, _, _) -> formatConst value
    | ReadVar(symbolId, hirType, _) -> $"{formatSymbolId symbolId}:{formatType hirType}"
    | ReadArrayElem(symbolId, args, hirType, _) ->
        let argsText = args |> List.map formatExpr |> String.concat ", "
        $"{formatSymbolId symbolId}[{argsText}]:{formatType hirType}"
    | Unary(op, inner, hirType, _) ->
        $"({formatUnaryOp op} {formatExpr inner}):{formatType hirType}"
    | Binary(op, lhs, rhs, hirType, _) ->
        $"({formatExpr lhs} {formatBinaryOp op} {formatExpr rhs}):{formatType hirType}"
    | CallFunc(symbolId, args, hirType, _) ->
        let argsText = args |> List.map formatExpr |> String.concat ", "
        $"{formatSymbolId symbolId}({argsText}):{formatType hirType}"

let private formatTarget = function
    | WriteVar(symbolId, hirType, _) -> $"{formatSymbolId symbolId}:{formatType hirType}"
    | WriteArrayElem(symbolId, args, hirType, _) ->
        let argsText = args |> List.map formatExpr |> String.concat ", "
        $"{formatSymbolId symbolId}[{argsText}]:{formatType hirType}"

let private formatChannel = function
    | Some channel -> $" channel={formatExpr channel}"
    | None -> ""

let private formatPromptList prompts =
    if List.isEmpty prompts then ""
    else
        let promptsText = prompts |> List.map formatExpr |> String.concat ", "
        $" prompts=[{promptsText}]"

let private formatTargetList targets =
    targets |> List.map formatTarget |> String.concat ", "

let private formatExprList exprs =
    exprs |> List.map formatExpr |> String.concat ", "

let rec private appendBlock (builder: StringBuilder) level (block: HirBlock) =
    block |> List.iter (appendStmt builder level)

and private appendStmt (builder: StringBuilder) level stmt =
    match stmt with
    | Assign(target, value, pos) ->
        appendLine builder level $"Assign {formatTarget target} <- {formatExpr value} @{formatPosition pos}"
    | ProcCall(symbolId, channel, args, pos) ->
        appendLine builder level $"ProcCall {formatSymbolId symbolId}{formatChannel channel} ({formatExprList args}) @{formatPosition pos}"
    | BuiltInCall(kind, channel, args, pos) ->
        appendLine builder level $"BuiltIn {formatBuiltInKind kind}{formatChannel channel} ({formatExprList args}) @{formatPosition pos}"
    | Input(channel, prompts, targets, pos) ->
        appendLine builder level $"Input{formatChannel channel}{formatPromptList prompts} targets=[{formatTargetList targets}] @{formatPosition pos}"
    | If(condition, thenBlock, elseBlock, pos) ->
        appendLine builder level $"If {formatExpr condition} @{formatPosition pos}"
        appendLine builder (level + 1) "Then"
        appendBlock builder (level + 2) thenBlock
        match elseBlock with
        | Some elseBranch ->
            appendLine builder (level + 1) "Else"
            appendBlock builder (level + 2) elseBranch
        | None -> ()
    | For(loopId, symbolId, startExpr, endExpr, stepExpr, body, pos) ->
        appendLine builder level $"For {formatLoopId loopId} counter={formatSymbolId symbolId} start={formatExpr startExpr} end={formatExpr endExpr} step={formatExpr stepExpr} @{formatPosition pos}"
        appendBlock builder (level + 1) body
    | Repeat(loopId, loopName, body, pos) ->
        appendLine builder level $"Repeat {formatLoopId loopId} name={formatLoopName loopName} @{formatPosition pos}"
        appendBlock builder (level + 1) body
    | Exit(loopId, pos) ->
        appendLine builder level $"Exit {formatLoopId loopId} @{formatPosition pos}"
    | Next(loopId, pos) ->
        appendLine builder level $"Next {formatLoopId loopId} @{formatPosition pos}"
    | Goto(target, pos) ->
        appendLine builder level $"Goto {formatExpr target} @{formatPosition pos}"
    | OnGoto(selector, targets, pos) ->
        appendLine builder level $"OnGoto selector={formatExpr selector} targets=[{formatExprList targets}] @{formatPosition pos}"
    | Gosub(target, pos) ->
        appendLine builder level $"Gosub {formatExpr target} @{formatPosition pos}"
    | OnGosub(selector, targets, pos) ->
        appendLine builder level $"OnGosub selector={formatExpr selector} targets=[{formatExprList targets}] @{formatPosition pos}"
    | Return(value, pos) ->
        let suffix =
            match value with
            | Some expr -> $" {formatExpr expr}"
            | None -> ""
        appendLine builder level $"Return{suffix} @{formatPosition pos}"
    | LineNumber(value, pos) ->
        appendLine builder level $"LineNumber {value} @{formatPosition pos}"
    | Restore(value, pos) ->
        let suffix =
            match value with
            | Some expr -> $" {formatExpr expr}"
            | None -> ""
        appendLine builder level $"Restore{suffix} @{formatPosition pos}"
    | Read(targets, pos) ->
        appendLine builder level $"Read [{formatTargetList targets}] @{formatPosition pos}"
    | Remark(text, pos) ->
        appendLine builder level $"Remark {text} @{formatPosition pos}"

let private appendStorage (builder: StringBuilder) level (storage: HirStorage) =
    appendLine builder level $"{formatSymbolId storage.Symbol} {storage.Name} {formatType storage.Type} {formatStorageClass storage.Class} {formatSlotId storage.Slot} @{formatPosition storage.Position}"

let private appendRoutine (builder: StringBuilder) level (routine: HirRoutine) =
    let returnText =
        match routine.ReturnType with
        | Some returnType -> formatType returnType
        | None -> "Void"

    appendLine builder level $"Routine {routine.Name} {formatSymbolId routine.Symbol} returns={returnText} @{formatPosition routine.Position}"
    appendLine builder (level + 1) "Parameters"
    routine.Parameters |> List.iter (appendStorage builder (level + 2))
    appendLine builder (level + 1) "Locals"
    routine.Locals |> List.iter (appendStorage builder (level + 2))
    appendLine builder (level + 1) "Body"
    appendBlock builder (level + 2) routine.Body

let prettyPrintHir hir =
    let builder = StringBuilder()

    appendLine builder 0 "HirProgram"
    appendLine builder 1 "Globals"
    hir.Globals |> List.iter (appendStorage builder 2)
    appendLine builder 1 "DataEntries"
    hir.DataEntries
    |> List.iter (fun entry ->
        let lineText =
            match entry.LineNumber with
            | Some lineNumber -> $" line={lineNumber}"
            | None -> ""
        appendLine builder 2 $"{formatDataSlotId entry.Slot} = {formatExpr entry.Value}{lineText} @{formatPosition entry.Position}")
    appendLine builder 1 "RestorePoints"
    hir.RestorePoints
    |> List.iter (fun point -> appendLine builder 2 $"line={point.LineNumber} -> {formatDataSlotId point.Slot}")
    appendLine builder 1 "Routines"
    hir.Routines |> List.iter (appendRoutine builder 2)
    appendLine builder 1 "Main"
    appendBlock builder 2 hir.Main

    builder.ToString()
