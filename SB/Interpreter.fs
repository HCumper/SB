module Interpreter

open System
open System.Collections.Generic
open System.Globalization

open HIR
open Types

type RuntimeValue =
    | IntValue of int
    | FloatValue of double
    | StringValue of string
    | ArrayValue of HirType * Dictionary<string, Cell>

and Cell = { mutable Value: RuntimeValue }

type RuntimeErrorCode =
    | MissingStorageCell
    | MissingDynamicStorageCell
    | InvalidArrayTarget
    | BuiltInArityMismatch
    | BuiltInUnsupportedArguments
    | BuiltInFunctionNotImplemented
    | InvalidReferenceActual
    | EscapedStop
    | MissingGotoTarget
    | MissingGosubTarget
    | EscapedLoopControl
    | UnsupportedChannelExecution
    | BuiltInStatementNotImplemented
    | ProcedureNotImplemented
    | InvalidForBounds
    | InvalidRestoreTarget
    | ReadPastData
    | EscapedReturn

type RuntimeError = {
    Code: RuntimeErrorCode
    Message: string
    Position: SourcePosition option
}

type RuntimeOptions = {
    InputProvider: unit -> string option
    OutputWriter: string -> unit
    Random: Random
    Clock: unit -> DateTime
}

type ExecutionResult = {
    Output: string list
}

let defaultRuntimeOptions =
    { InputProvider = fun () -> Console.ReadLine() |> Option.ofObj
      OutputWriter = Console.WriteLine
      Random = Random()
      Clock = fun () -> DateTime.UtcNow }

type private Frame = {
    Cells: Map<SymbolId, Cell>
    ReturnType: HirType option
}

type private RuntimeState = {
    Program: HirProgram
    Globals: Map<SymbolId, Cell>
    Frames: Frame list
    DataPointer: int
    Output: string list
    Options: RuntimeOptions
}

type private ControlFlow =
    | Continue
    | ReturnFromRoutine of RuntimeValue option
    | BareReturn
    | ExitLoop of LoopId
    | NextLoop of LoopId
    | JumpToLine of int
    | GosubToLine of int
    | StopExecution

let private formatRuntimeErrorCode = function
    | MissingStorageCell -> "MissingStorageCell"
    | MissingDynamicStorageCell -> "MissingDynamicStorageCell"
    | InvalidArrayTarget -> "InvalidArrayTarget"
    | BuiltInArityMismatch -> "BuiltInArityMismatch"
    | BuiltInUnsupportedArguments -> "BuiltInUnsupportedArguments"
    | BuiltInFunctionNotImplemented -> "BuiltInFunctionNotImplemented"
    | InvalidReferenceActual -> "InvalidReferenceActual"
    | EscapedStop -> "EscapedStop"
    | MissingGotoTarget -> "MissingGotoTarget"
    | MissingGosubTarget -> "MissingGosubTarget"
    | EscapedLoopControl -> "EscapedLoopControl"
    | UnsupportedChannelExecution -> "UnsupportedChannelExecution"
    | BuiltInStatementNotImplemented -> "BuiltInStatementNotImplemented"
    | ProcedureNotImplemented -> "ProcedureNotImplemented"
    | InvalidForBounds -> "InvalidForBounds"
    | InvalidRestoreTarget -> "InvalidRestoreTarget"
    | ReadPastData -> "ReadPastData"
    | EscapedReturn -> "EscapedReturn"

let private formatRuntimeErrorLocation = function
    | Some { BasicLineNo = Some basicLineNo } -> $" at BASIC line {basicLineNo}"
    | Some { EditorLineNo = editorLineNo; Column = column } -> $" at editor line {editorLineNo}, column {column}"
    | None -> ""

let private runtimeError code position detail =
    let message =
        $"Runtime error [{formatRuntimeErrorCode code}]{formatRuntimeErrorLocation position}: {detail}"

    Result.Error { Code = code; Message = message; Position = position }

let private defaultValue hirType =
    match hirType with
    | HIR.HirType.Int -> IntValue 0
    | HIR.HirType.Float -> FloatValue 0.0
    | HIR.HirType.String -> StringValue ""
    | HIR.HirType.Void -> IntValue 0
    | HIR.HirType.Array inner -> ArrayValue(inner, Dictionary<string, Cell>())

let private normalizeNumber value =
    match value with
    | IntValue n -> double n
    | FloatValue f -> f
    | StringValue s ->
        match Double.TryParse(s, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
        | true, n -> n
        | _ -> 0.0
    | ArrayValue _ -> 0.0

let private asInt value =
    match value with
    | IntValue n -> n
    | FloatValue f -> int (Math.Round(f))
    | StringValue s ->
        match Int32.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | true, n -> n
        | _ -> 0
    | ArrayValue _ -> 0

let private asString value =
    match value with
    | IntValue n -> string n
    | FloatValue f -> f.ToString("G17", CultureInfo.InvariantCulture)
    | StringValue s -> s
    | ArrayValue _ -> "<array>"

let private truthy value =
    match value with
    | StringValue s -> not (String.IsNullOrEmpty s)
    | ArrayValue _ -> true
    | _ -> normalizeNumber value <> 0.0

let private makeNumericValue hirType number =
    match hirType with
    | HIR.HirType.Float -> FloatValue number
    | _ -> IntValue (int (Math.Round(number)))

let private formatOutputValue value = asString value

let private lookupCell state symbolId =
    let rec find frames =
        match frames with
        | frame :: tail ->
            match Map.tryFind symbolId frame.Cells with
            | Some cell -> Some cell
            | None -> find tail
        | [] -> Map.tryFind symbolId state.Globals

    find state.Frames

let private symbolName state symbolId =
    Map.tryFind symbolId state.Program.SymbolNames |> Option.defaultValue (string symbolId)

let private requireCell state symbolId pos =
    match lookupCell state symbolId with
    | Some cell -> Result.Ok cell
    | None -> runtimeError MissingStorageCell (Some pos) $"No storage cell exists for symbol {symbolId} ({symbolName state symbolId})."

let private lookupDynamicCell state name =
    let normalized = normalizeIdentifier name
    let tryFindByName cells =
        cells
        |> Map.toList
        |> List.tryPick (fun (symbolId, cell) ->
            if normalizeIdentifier (symbolName state symbolId) = normalized then Some cell else None)

    let rec findInFrames frames =
        match frames with
        | frame :: tail ->
            match tryFindByName frame.Cells with
            | Some cell -> Some cell
            | None -> findInFrames tail
        | [] -> tryFindByName state.Globals

    findInFrames state.Frames

let private requireDynamicCell state name pos =
    match lookupDynamicCell state name with
    | Some cell -> Result.Ok cell
    | None -> runtimeError MissingDynamicStorageCell (Some pos) $"No dynamic storage cell exists for '{name}'."

let private arrayKey indexes =
    indexes |> List.map string |> String.concat ","

let private tryGetArrayEntries (value: RuntimeValue) =
    match value with
    | ArrayValue(_, cells) -> Result.Ok cells
    | _ -> runtimeError InvalidArrayTarget None "Target is not an array."

let private getArrayElementType (value: RuntimeValue) =
    match value with
    | ArrayValue(innerType, _) -> innerType
    | _ -> HIR.HirType.Int

let private getOrCreateArrayEntry (entries: Dictionary<string, Cell>) innerType key =
    match entries.TryGetValue key with
    | true, existing -> existing
    | _ ->
        let created = { Value = defaultValue innerType }
        entries[key] <- created
        created

let private allocateCells (storages: HirStorage list) =
    storages
    |> List.map (fun storage -> storage.Symbol, { Value = defaultValue storage.Type })
    |> Map.ofList

let private updateOutput line state =
    state.Options.OutputWriter line
    { state with Output = state.Output @ [ line ] }

let private withFrame frame state =
    { state with Frames = frame :: state.Frames }

let private popFrame state =
    match state.Frames with
    | _ :: rest -> { state with Frames = rest }
    | [] -> state

let private buildLineIndex (block: HirBlock) =
    block
    |> List.mapi (fun index stmt ->
        match stmt with
        | LineNumber(value, _) -> Some(value, index)
        | _ -> None)
    |> List.choose id
    |> Map.ofList

let rec private evalExpr state expr =
    match expr with
    | Literal(ConstInt n, _, _) -> Result.Ok(IntValue n, state)
    | Literal(ConstFloat n, _, _) -> Result.Ok(FloatValue n, state)
    | Literal(ConstString s, _, _) -> Result.Ok(StringValue s, state)
    | ReadVar(symbolId, _, pos) ->
        requireCell state symbolId pos
        |> Result.map (fun cell -> cell.Value, state)
    | ReadArrayElem(symbolId, indexes, _, pos) ->
        requireCell state symbolId pos
        |> Result.bind (fun cell ->
            tryGetArrayEntries cell.Value
            |> Result.bind (fun entries ->
                evalExprList state indexes
                |> Result.map (fun (indexValues, nextState) ->
                    let key = indexValues |> List.map asInt |> arrayKey
                    let entry = getOrCreateArrayEntry entries (getArrayElementType cell.Value) key
                    entry.Value, nextState)))
    | DynamicReadVar(name, _, pos) ->
        requireDynamicCell state name pos
        |> Result.map (fun cell -> cell.Value, state)
    | DynamicReadArrayElem(name, indexes, _, pos) ->
        requireDynamicCell state name pos
        |> Result.bind (fun cell ->
            tryGetArrayEntries cell.Value
            |> Result.bind (fun entries ->
                evalExprList state indexes
                |> Result.map (fun (indexValues, nextState) ->
                    let key = indexValues |> List.map asInt |> arrayKey
                    let entry = getOrCreateArrayEntry entries (getArrayElementType cell.Value) key
                    entry.Value, nextState)))
    | Unary(op, inner, hirType, _) ->
        evalExpr state inner
        |> Result.map (fun (value, nextState) ->
            let result =
                match op with
                | Identity -> value
                | Negate -> makeNumericValue hirType (-normalizeNumber value)
                | BitwiseNot -> IntValue(~~~(asInt value))
                | UnaryUnknown _ -> value
            result, nextState)
    | Binary(op, lhs, rhs, hirType, _) ->
        evalExpr state lhs
        |> Result.bind (fun (leftValue, leftState) ->
            evalExpr leftState rhs
            |> Result.map (fun (rightValue, rightState) ->
                let compareNumbers cmp = if cmp (normalizeNumber leftValue) (normalizeNumber rightValue) then IntValue 1 else IntValue 0
                let compareStrings cmp = if cmp (asString leftValue) (asString rightValue) then IntValue 1 else IntValue 0
                let result =
                    match op with
                    | Add -> makeNumericValue hirType (normalizeNumber leftValue + normalizeNumber rightValue)
                    | Subtract -> makeNumericValue hirType (normalizeNumber leftValue - normalizeNumber rightValue)
                    | Multiply -> makeNumericValue hirType (normalizeNumber leftValue * normalizeNumber rightValue)
                    | Divide -> FloatValue (normalizeNumber leftValue / normalizeNumber rightValue)
                    | Power -> FloatValue (Math.Pow(normalizeNumber leftValue, normalizeNumber rightValue))
                    | Concat -> StringValue (asString leftValue + asString rightValue)
                    | IntegerDivide -> IntValue (asInt leftValue / asInt rightValue)
                    | Modulo -> IntValue (asInt leftValue % asInt rightValue)
                    | BitwiseAnd -> IntValue (asInt leftValue &&& asInt rightValue)
                    | BitwiseOr -> IntValue (asInt leftValue ||| asInt rightValue)
                    | BitwiseXor -> IntValue (asInt leftValue ^^^ asInt rightValue)
                    | Equal ->
                        match leftValue, rightValue with
                        | StringValue _, _
                        | _, StringValue _ -> compareStrings (=)
                        | _ -> compareNumbers (=)
                    | NotEqual ->
                        match leftValue, rightValue with
                        | StringValue _, _
                        | _, StringValue _ -> compareStrings (<>)
                        | _ -> compareNumbers (<>)
                    | LessThan ->
                        match leftValue, rightValue with
                        | StringValue _, _
                        | _, StringValue _ -> compareStrings (<)
                        | _ -> compareNumbers (<)
                    | LessThanOrEqual ->
                        match leftValue, rightValue with
                        | StringValue _, _
                        | _, StringValue _ -> compareStrings (<=)
                        | _ -> compareNumbers (<=)
                    | GreaterThan ->
                        match leftValue, rightValue with
                        | StringValue _, _
                        | _, StringValue _ -> compareStrings (>)
                        | _ -> compareNumbers (>)
                    | GreaterThanOrEqual ->
                        match leftValue, rightValue with
                        | StringValue _, _
                        | _, StringValue _ -> compareStrings (>=)
                        | _ -> compareNumbers (>=)
                    | SliceRange -> rightValue
                    | Instr
                    | BinaryUnknown _ -> IntValue 0
                result, rightState))
    | CallFunc(symbolId, args, _, pos) ->
        match state.Program.Routines |> List.tryFind (fun (routine: HirRoutine) -> routine.Symbol = symbolId) with
        | Some routine ->
            callRoutine state routine args pos
        | None ->
            let valueArgs = args |> List.choose (function | ValueArg expr -> Some expr | RefArg _ -> None)
            evalBuiltInFunction state symbolId valueArgs pos

and private evalExprList state exprs =
    let folder acc expr =
        acc
        |> Result.bind (fun (values, currentState) ->
            evalExpr currentState expr
            |> Result.map (fun (value, nextState) -> values @ [ value ], nextState))

    List.fold folder (Result.Ok([], state)) exprs

and private evalLineNumber state expr pos =
    evalExpr state expr
    |> Result.map (fun (value, nextState) -> asInt value, nextState)

and private evalBuiltInFunction state symbolId argExprs pos =
    let name = symbolName state symbolId
    let normalized = normalizeIdentifier name

    let evalArgs () = evalExprList state argExprs
    let expectOneNumeric f =
        match argExprs with
        | [ _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) -> f values[0], nextState)
        | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in function '{name}' expects one argument."

    match normalized with
    | "ABS" -> expectOneNumeric (fun value -> makeNumericValue HIR.HirType.Int (abs (normalizeNumber value)))
    | "INT" -> expectOneNumeric (fun value -> IntValue (int (Math.Floor(normalizeNumber value))))
    | "ROUND" -> expectOneNumeric (fun value -> IntValue (int (Math.Round(normalizeNumber value))))
    | "STR$" ->
        match argExprs with
        | [ _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) -> StringValue (asString values[0]), nextState)
        | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in function '{name}' expects one argument."
    | "VAL" ->
        match argExprs with
        | [ _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) ->
                match Double.TryParse(asString values[0], NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
                | true, number -> FloatValue number, nextState
                | _ -> IntValue 0, nextState)
        | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in function '{name}' expects one argument."
    | "LEFT$" ->
        match argExprs with
        | [ _; _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) ->
                let source = asString values[0]
                let length = max 0 (min source.Length (asInt values[1]))
                StringValue(source.Substring(0, length)), nextState)
        | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in function '{name}' expects two arguments."
    | "RIGHT$" ->
        match argExprs with
        | [ _; _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) ->
                let source = asString values[0]
                let length = max 0 (min source.Length (asInt values[1]))
                StringValue(source.Substring(source.Length - length, length)), nextState)
        | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in function '{name}' expects two arguments."
    | "DATE" ->
        let seconds = int (state.Options.Clock().Subtract(DateTime.UnixEpoch).TotalSeconds)
        Result.Ok(IntValue seconds, state)
    | "RND" ->
        match argExprs with
        | [ Binary(SliceRange, lhs, rhs, _, _) ] ->
            evalExpr state lhs
            |> Result.bind (fun (low, lowState) ->
                evalExpr lowState rhs
                |> Result.map (fun (high, highState) ->
                    let minValue = asInt low
                    let maxValue = asInt high
                    let value = highState.Options.Random.Next(minValue, maxValue + 1)
                    IntValue value, highState))
        | [ _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) ->
                let upper = max 1 (asInt values[0])
                IntValue (nextState.Options.Random.Next(1, upper + 1)), nextState)
        | [] -> Result.Ok(FloatValue (state.Options.Random.NextDouble()), state)
        | _ -> runtimeError BuiltInUnsupportedArguments (Some pos) $"Built-in function '{name}' received unsupported arguments."
    | _ -> runtimeError BuiltInFunctionNotImplemented (Some pos) $"Built-in function '{name}' is not implemented."

and private resolveTargetCell state target =
    match target with
    | WriteVar(symbolId, _, pos) ->
        requireCell state symbolId pos
        |> Result.map (fun cell -> cell, state)
    | WriteArrayElem(symbolId, indexes, _, pos) ->
        requireCell state symbolId pos
        |> Result.bind (fun cell ->
            tryGetArrayEntries cell.Value
            |> Result.bind (fun entries ->
                evalExprList state indexes
                |> Result.map (fun (indexValues, nextState) ->
                    let key = indexValues |> List.map asInt |> arrayKey
                    let entry = getOrCreateArrayEntry entries (getArrayElementType cell.Value) key
                    entry, nextState)))
    | DynamicWriteVar(name, _, pos) ->
        requireDynamicCell state name pos
        |> Result.map (fun cell -> cell, state)
    | DynamicWriteArrayElem(name, indexes, _, pos) ->
        requireDynamicCell state name pos
        |> Result.bind (fun cell ->
            tryGetArrayEntries cell.Value
            |> Result.bind (fun entries ->
                evalExprList state indexes
                |> Result.map (fun (indexValues, nextState) ->
                    let key = indexValues |> List.map asInt |> arrayKey
                    let entry = getOrCreateArrayEntry entries (getArrayElementType cell.Value) key
                    entry, nextState)))

and private writeTarget state target value =
    resolveTargetCell state target
    |> Result.map (fun (cell, nextState) ->
        cell.Value <- value
        nextState)

and private callRoutine state (routine: HirRoutine) args pos =
    let bindCallArg currentState parameter arg =
        match parameter.Binding, arg with
        | ReferenceBinding, RefArg target
        | FlexibleBinding, RefArg target ->
            resolveTargetCell currentState target
            |> Result.map (fun (cell, nextState) -> (parameter.Storage.Symbol, cell), nextState)
        | ReferenceBinding, ValueArg _ ->
            runtimeError InvalidReferenceActual (Some parameter.Storage.Position) $"Parameter '{parameter.Storage.Name}' requires a writable argument."
        | FlexibleBinding, ValueArg expr ->
            evalExpr currentState expr
            |> Result.map (fun (value, nextState) -> (parameter.Storage.Symbol, { Value = value }), nextState)

    let parameterCellsResult =
        ((Result.Ok([], state), List.zip routine.Parameters args)
         ||> List.fold (fun acc (parameter, arg) ->
             acc
             |> Result.bind (fun (pairs, currentState) ->
                 bindCallArg currentState parameter arg
                 |> Result.map (fun (pair, nextState) -> pairs @ [ pair ], nextState))))
        |> Result.map (fun (pairs, nextState) -> Map.ofList pairs, nextState)

    parameterCellsResult
    |> Result.bind (fun (parameterCells, stateAfterArgs) ->
        let localCells =
            allocateCells routine.Locals

        let frame =
            { Cells = Map.fold (fun acc key value -> Map.add key value acc) localCells parameterCells
              ReturnType = routine.ReturnType }

        let stateWithFrame = withFrame frame stateAfterArgs

        executeBlock stateWithFrame routine.Body
        |> Result.bind (fun (flow, afterBody) ->
            let finalState = popFrame afterBody
            match flow, routine.ReturnType with
            | ReturnFromRoutine value, Some _ -> Result.Ok(value |> Option.defaultValue (IntValue 0), finalState)
            | ReturnFromRoutine _, None -> Result.Ok(IntValue 0, finalState)
            | BareReturn, _ -> Result.Ok(IntValue 0, finalState)
            | Continue, Some _ -> Result.Ok(IntValue 0, finalState)
            | Continue, None -> Result.Ok(IntValue 0, finalState)
            | StopExecution, _ -> runtimeError EscapedStop (Some pos) $"STOP escaped from routine '{routine.Name}'."
            | JumpToLine lineNumber, _ -> runtimeError MissingGotoTarget (Some pos) $"GOTO target line {lineNumber} does not exist in routine '{routine.Name}'."
            | GosubToLine lineNumber, _ -> runtimeError MissingGosubTarget (Some pos) $"GOSUB target line {lineNumber} does not exist in routine '{routine.Name}'."
            | ExitLoop _, _
            | NextLoop _, _ -> runtimeError EscapedLoopControl (Some pos) $"Loop control escaped from routine '{routine.Name}'."))

and private executeBuiltInCall state kind channel args targets pos =
    let unsupportedChannel name =
        match channel with
        | Some _ -> runtimeError UnsupportedChannelExecution (Some pos) $"Built-in '{name}' does not support channel execution in the interpreter yet."
        | None -> Result.Ok(Continue, state)

    match kind with
    | Print ->
        unsupportedChannel "PRINT"
        |> Result.bind (fun _ ->
            evalExprList state args
            |> Result.map (fun (values, nextState) ->
                let line = values |> List.map formatOutputValue |> String.concat " "
                Continue, updateOutput line nextState))
    | BuiltInKind.Input ->
        let promptState =
            if List.isEmpty args then state
            else
                let promptText =
                    args
                    |> List.map (fun expr ->
                        match evalExpr state expr with
                        | Result.Ok(value, _) -> formatOutputValue value
                        | Result.Error _ -> "")
                    |> String.concat " "
                updateOutput promptText state

        let readValues (line: string) =
            line.Split([| ',' |], StringSplitOptions.None)
            |> Array.toList
            |> List.map (fun segment -> segment.Trim())

        let sourceValues =
            match promptState.Options.InputProvider() with
            | Some line -> readValues line
            | None -> []

        let assignInput currentState target index =
            let raw = if index < sourceValues.Length then sourceValues[index] else ""
            let parsed =
                match target with
                | WriteVar(_, HIR.HirType.String, _)
                | WriteArrayElem(_, _, HIR.HirType.String, _) -> StringValue raw
                | WriteVar(_, HIR.HirType.Float, _)
                | WriteArrayElem(_, _, HIR.HirType.Float, _) ->
                    match Double.TryParse(raw, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
                    | true, number -> FloatValue number
                    | _ -> FloatValue 0.0
                | _ ->
                    match Int32.TryParse(raw, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                    | true, number -> IntValue number
                    | _ -> IntValue 0
            writeTarget currentState target parsed

        ((Result.Ok promptState, targets |> List.indexed)
         ||> List.fold (fun acc (index, target) ->
             acc |> Result.bind (fun currentState -> assignInput currentState target index)))
        |> Result.map (fun nextState -> Continue, nextState)
    | Reference -> Result.Ok(Continue, state)
    | NamedBuiltIn name ->
        let normalized = normalizeIdentifier name
        if normalized = "STOP" then Result.Ok(StopExecution, state)
        elif normalized.StartsWith("TURBO") then Result.Ok(Continue, state)
        else runtimeError BuiltInStatementNotImplemented (Some pos) $"Built-in statement '{kind}' is not implemented."
    | GotoBuiltIn
    | GosubBuiltIn
    | OnGotoBuiltIn
    | OnGosubBuiltIn -> runtimeError BuiltInStatementNotImplemented (Some pos) $"Built-in statement '{kind}' is not implemented."

and private executeStmt state stmt =
    match stmt with
    | Assign(target, expr, _) ->
        evalExpr state expr
        |> Result.bind (fun (value, nextState) ->
            writeTarget nextState target value
            |> Result.map (fun finalState -> Continue, finalState))
    | ProcCall(symbolId, _, args, pos) ->
        match state.Program.Routines |> List.tryFind (fun routine -> routine.Symbol = symbolId) with
        | Some routine ->
            callRoutine state routine args pos
            |> Result.map (fun (_, finalState) -> Continue, finalState)
        | None -> runtimeError ProcedureNotImplemented (Some pos) $"Procedure '{symbolName state symbolId}' is not implemented."
    | BuiltInCall(kind, channel, args, pos) ->
        executeBuiltInCall state kind channel args [] pos
    | Input(channel, prompts, targets, pos) ->
        executeBuiltInCall state BuiltInKind.Input channel prompts targets pos
    | If(condition, thenBlock, elseBlock, _) ->
        evalExpr state condition
        |> Result.bind (fun (value, nextState) ->
            if truthy value then executeBlock nextState thenBlock
            else
                match elseBlock with
                | Some block -> executeBlock nextState block
                | None -> Result.Ok(Continue, nextState))
    | For(loopId, symbolId, startExpr, endExpr, stepExpr, body, pos) ->
        evalExprList state [ startExpr; endExpr; stepExpr ]
        |> Result.bind (fun (values, nextState) ->
            match values with
            | [ startValue; endValue; stepValue ] ->
                requireCell nextState symbolId pos
                |> Result.bind (fun cell ->
                    let rec iterate currentState currentValue =
                        cell.Value <- IntValue currentValue
                        let step = asInt stepValue
                        let shouldContinue =
                            if step >= 0 then currentValue <= asInt endValue
                            else currentValue >= asInt endValue
                        if not shouldContinue then
                            Result.Ok(Continue, currentState)
                        else
                            executeBlock currentState body
                            |> Result.bind (fun (flow, bodyState) ->
                                match flow with
                                | Continue -> iterate bodyState (currentValue + step)
                                | NextLoop flowId when flowId = loopId -> iterate bodyState (currentValue + step)
                                | ExitLoop flowId when flowId = loopId -> Result.Ok(Continue, bodyState)
                                | _ -> Result.Ok(flow, bodyState))
                    iterate nextState (asInt startValue))
            | _ -> runtimeError InvalidForBounds (Some pos) "FOR requires start, end, and step values.")
    | Repeat(loopId, _, body, _) ->
        let rec iterate currentState =
            executeBlock currentState body
            |> Result.bind (fun (flow, nextState) ->
                match flow with
                | Continue -> iterate nextState
                | NextLoop flowId when flowId = loopId -> iterate nextState
                | ExitLoop flowId when flowId = loopId -> Result.Ok(Continue, nextState)
                | _ -> Result.Ok(flow, nextState))
        iterate state
    | Exit(loopId, _) -> Result.Ok(ExitLoop loopId, state)
    | Next(loopId, _) -> Result.Ok(NextLoop loopId, state)
    | Goto(target, pos) ->
        evalLineNumber state target pos
        |> Result.map (fun (lineNumber, nextState) -> JumpToLine lineNumber, nextState)
    | OnGoto(selector, targets, pos) ->
        evalExpr state selector
        |> Result.bind (fun (selectorValue, nextState) ->
            let index = asInt selectorValue
            if index < 1 || index > targets.Length then
                Result.Ok(Continue, nextState)
            else
                evalLineNumber nextState targets[index - 1] pos
                |> Result.map (fun (lineNumber, finalState) -> JumpToLine lineNumber, finalState))
    | Gosub(target, pos) ->
        evalLineNumber state target pos
        |> Result.map (fun (lineNumber, nextState) -> GosubToLine lineNumber, nextState)
    | OnGosub(selector, targets, pos) ->
        evalExpr state selector
        |> Result.bind (fun (selectorValue, nextState) ->
            let index = asInt selectorValue
            if index < 1 || index > targets.Length then
                Result.Ok(Continue, nextState)
            else
                evalLineNumber nextState targets[index - 1] pos
                |> Result.map (fun (lineNumber, finalState) -> GosubToLine lineNumber, finalState))
    | Return(expr, _) ->
        match expr with
        | Some valueExpr ->
            evalExpr state valueExpr |> Result.map (fun (value, nextState) -> ReturnFromRoutine(Some value), nextState)
        | None -> Result.Ok(BareReturn, state)
    | LineNumber _ -> Result.Ok(Continue, state)
    | Restore(lineNumberExpr, pos) ->
        match lineNumberExpr with
        | None -> Result.Ok(Continue, { state with DataPointer = 0 })
        | Some expr ->
            evalExpr state expr
            |> Result.bind (fun (value, nextState) ->
                let lineNumber = asInt value
                match nextState.Program.RestorePoints |> List.tryFind (fun point -> point.LineNumber = lineNumber) with
                | Some point ->
                    let (DataSlotId slot) = point.Slot
                    Result.Ok(Continue, { nextState with DataPointer = slot })
                | None -> runtimeError InvalidRestoreTarget (Some pos) $"RESTORE line {lineNumber} does not exist.")
    | Read(targets, pos) ->
        let rec assign currentState remaining =
            match remaining with
            | [] -> Result.Ok(Continue, currentState)
            | target :: tail ->
                match currentState.Program.DataEntries |> List.tryItem currentState.DataPointer with
                | Some entry ->
                    evalExpr currentState entry.Value
                    |> Result.bind (fun (value, nextState) ->
                        writeTarget { nextState with DataPointer = nextState.DataPointer + 1 } target value
                        |> Result.bind (fun updated -> assign updated tail))
                | None -> runtimeError ReadPastData (Some pos) "READ moved past the end of DATA."
        assign state targets
    | Remark _ -> Result.Ok(Continue, state)

and private executeBlock state block =
    let lineIndex = buildLineIndex block

    let rec loop currentState pc gosubStack =
        if pc >= block.Length then
            Result.Ok(Continue, currentState)
        else
            executeStmt currentState block[pc]
            |> Result.bind (fun (flow, nextState) ->
                match flow with
                | Continue -> loop nextState (pc + 1) gosubStack
                | JumpToLine lineNumber ->
                    match Map.tryFind lineNumber lineIndex with
                    | Some targetPc -> loop nextState targetPc gosubStack
                    | None -> Result.Ok(JumpToLine lineNumber, nextState)
                | GosubToLine lineNumber ->
                    match Map.tryFind lineNumber lineIndex with
                    | Some targetPc -> loop nextState targetPc ((pc + 1) :: gosubStack)
                    | None -> Result.Ok(GosubToLine lineNumber, nextState)
                | BareReturn ->
                    match gosubStack with
                    | returnPc :: rest -> loop nextState returnPc rest
                    | [] -> Result.Ok(BareReturn, nextState)
                | _ -> Result.Ok(flow, nextState))

    loop state 0 []

let interpretProgramWithOptions options (program: HirProgram) =
    let initialState =
        { Program = program
          Globals = allocateCells program.Globals
          Frames = []
          DataPointer = 0
          Output = []
          Options = options }

    executeBlock initialState program.Main
    |> Result.bind (fun (flow, finalState) ->
        match flow with
        | Continue
        | StopExecution -> Result.Ok { Output = finalState.Output }
        | ReturnFromRoutine _
        | BareReturn -> runtimeError EscapedReturn None "Return escaped the top-level program."
        | JumpToLine lineNumber -> runtimeError MissingGotoTarget None $"GOTO target line {lineNumber} does not exist."
        | GosubToLine lineNumber -> runtimeError MissingGosubTarget None $"GOSUB target line {lineNumber} does not exist."
        | ExitLoop _
        | NextLoop _ -> runtimeError EscapedLoopControl None "Loop control escaped the top-level program.")

let interpretProgram program =
    interpretProgramWithOptions defaultRuntimeOptions program
