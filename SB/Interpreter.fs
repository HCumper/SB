module Interpreter

open System
open System.Collections.Generic
open System.Globalization

open HIR
open SBRuntime
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
    Host: IRuntimeHost
    Random: Random
    Clock: unit -> DateTime
    Sleeper: int -> unit
}

type ExecutionResult = {
    Output: string list
}

let defaultRuntimeOptions =
    { Host =
        DefaultHost.create {
            ReadLine = fun () -> Console.ReadLine() |> Option.ofObj
            ReadKey =
                fun () ->
                    try
                        if Console.KeyAvailable then
                            let key = Console.ReadKey(true)
                            Some {
                                KeyCode = int key.KeyChar
                                Character = Some key.KeyChar
                                Shift = key.Modifiers.HasFlag(ConsoleModifiers.Shift)
                                Control = key.Modifiers.HasFlag(ConsoleModifiers.Control)
                            }
                        else
                            None
                    with
                    | _ -> None
            KeyAvailable =
                fun () ->
                    try Console.KeyAvailable with _ -> false
            KeyRowState = fun _ -> 0
            WriteLine = Console.WriteLine
        }
      Random = Random()
      Clock = fun () -> DateTime.UtcNow
      Sleeper = fun milliseconds -> System.Threading.Thread.Sleep(milliseconds) }

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
    RandomSource: Random
    Options: RuntimeOptions
}

type private ControlFlow =
    | Continue
    | TransferredContinue
    | ReturnFromRoutine of RuntimeValue option
    | BareReturn
    | ExitLoop of LoopId
    | NextLoop of LoopId
    | JumpToLine of int
    | GosubToLine of int
    | StopExecution

type private GosubReturn = RuntimeState -> Result<ControlFlow * RuntimeState, RuntimeError>
type private LineTarget = RuntimeState -> GosubReturn list -> Result<ControlFlow * RuntimeState, RuntimeError>

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

let private toRuntimeBuiltInValue value =
    match value with
    | IntValue n -> RuntimeValues.ofInt n
    | FloatValue f -> RuntimeValues.ofFloat f
    | StringValue s -> RuntimeValues.ofString s
    | ArrayValue _ -> Uninitialized

let private fromRuntimeBuiltInValue value =
    match value with
    | Numeric(IntNumber n) -> IntValue n
    | Numeric(FloatNumber f) -> FloatValue f
    | Text s -> StringValue s
    | Uninitialized -> IntValue 0

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
    state.Options.Host.Screen.WriteText line
    { state with Output = state.Output @ [ line ] }

let private hostErrorText = function
    | ChannelNotFound(ChannelId channelId) -> $"Channel #{channelId} does not exist."
    | DeviceOpenFailed detail -> detail
    | UnsupportedHostOperation detail -> detail
    | InvalidHostArgument detail -> detail

let private screenModeFromNumber = function
    | 4 -> QlMode4
    | 8 -> QlMode8
    | value -> ExtendedMode value

let private writeOutputToChannel channelId line state pos =
    match channelId with
    | None ->
        state.Options.Host.Screen.WriteText line
        Result.Ok { state with Output = state.Output @ [ line ] }
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok channel ->
            channel.WriteText line
            let captureOutput =
                match channel.Kind with
                | NamedChannel "NUL" -> false
                | _ -> true
            if captureOutput then
                Result.Ok { state with Output = state.Output @ [ line ] }
            else
                Result.Ok state
        | Result.Error hostError ->
            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)

let private readInputFromChannel channelId state pos =
    match channelId with
    | None -> Result.Ok(state.Options.Host.Input.ReadLine())
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok channel -> Result.Ok(channel.ReadText())
        | Result.Error hostError ->
            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)

let private validateScreenChannel channelId state pos =
    match channelId with
    | None -> Result.Ok()
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok channel ->
            match channel with
            | :? IScreenChannel -> Result.Ok()
            | _ ->
                runtimeError
                    UnsupportedChannelExecution
                    (Some pos)
                    $"Channel is not a screen channel for screen operation '{resolvedChannelId}'."
        | Result.Error hostError ->
            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)

let private executeChannelScreenOp channelId state pos (channelAction: IScreenChannel -> unit) (defaultAction: IScreenDevice -> unit) =
    match channelId with
    | None ->
        defaultAction state.Options.Host.Screen
        Result.Ok state
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok (:? IScreenChannel as screenChannel) ->
            channelAction screenChannel
            Result.Ok state
        | Result.Ok _ ->
            let (ChannelId id) = resolvedChannelId
            runtimeError UnsupportedChannelExecution (Some pos) $"Channel #{id} is not a screen channel."
        | Result.Error hostError ->
            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)

let private executeGraphicsOp channelId state pos (action: IGraphicsDevice -> unit) =
    let run nextState =
        action nextState.Options.Host.Graphics
        Result.Ok nextState

    match channelId with
    | None -> run state
    | Some _ ->
        validateScreenChannel channelId state pos
        |> Result.bind (fun () -> run state)

let private withFrame frame state =
    { state with Frames = frame :: state.Frames }

let private popFrame state =
    match state.Frames with
    | _ :: rest -> { state with Frames = rest }
    | [] -> state

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
    | CallFunc(symbolId, args, hirType, pos) ->
        match state.Program.Routines |> List.tryFind (fun (routine: HirRoutine) -> routine.Symbol = symbolId) with
        | Some routine ->
            callRoutine state routine args pos
        | None ->
            let valueArgs = args |> List.choose (function | ValueArg expr -> Some expr | RefArg _ -> None)
            evalBuiltInFunction state symbolId valueArgs hirType pos

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

and private evalBuiltInFunction state symbolId argExprs hirType pos =
    let name = symbolName state symbolId
    let allowRangePair =
        match normalizeIdentifier name, argExprs with
        | "RND", [ Binary(SliceRange, _, _, _, _) ] -> true
        | _ -> false
    let runtimeArgs =
        argExprs
        |> List.collect (function
            | Binary(SliceRange, lhs, rhs, _, _) -> [ lhs; rhs ]
            | expr -> [ expr ])

    evalExprList state runtimeArgs
    |> Result.bind (fun (values, nextState) ->
        BuiltInFunctions.evaluate
            name
            (hirType = HIR.HirType.Float)
            nextState.Options.Clock
            (fun () -> nextState.RandomSource)
            nextState.Options.Host.Environment.GetVariable
            nextState.Options.Host.Input.ReadKey
            nextState.Options.Sleeper
            nextState.Options.Host.Input.GetKeyRow
            (fun channelNumber ->
                match nextState.Options.Host.Channels.Get(ChannelId channelNumber) with
                | Result.Ok channel -> channel.IsEndOfFile()
                | Result.Error _ -> true)
            allowRangePair
            (values |> List.map toRuntimeBuiltInValue)
        |> function
            | Result.Ok value -> Result.Ok(fromRuntimeBuiltInValue value, nextState)
            | Result.Error(ArityMismatch message) -> runtimeError BuiltInArityMismatch (Some pos) message
            | Result.Error(UnsupportedArguments message) -> runtimeError BuiltInUnsupportedArguments (Some pos) message
            | Result.Error(NotImplemented message) -> runtimeError BuiltInFunctionNotImplemented (Some pos) message)

and private resolveChannelId state channelExpr =
    match channelExpr with
    | None -> Result.Ok(None, state)
    | Some expr ->
        evalExpr state expr
        |> Result.map (fun (value, nextState) -> Some(ChannelId(asInt value)), nextState)

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

        let resolveLine = buildLineTargets routine.Body

        executeBlock resolveLine stateWithFrame routine.Body 0 [] (fun state _ -> Result.Ok(Continue, state))
        |> Result.bind (fun (flow, afterBody) ->
            let finalState = popFrame afterBody
            match flow, routine.ReturnType with
            | ReturnFromRoutine value, Some _ -> Result.Ok(value |> Option.defaultValue (IntValue 0), finalState)
            | ReturnFromRoutine _, None -> Result.Ok(IntValue 0, finalState)
            | BareReturn, _ -> Result.Ok(IntValue 0, finalState)
            | Continue, Some _ -> Result.Ok(IntValue 0, finalState)
            | Continue, None -> Result.Ok(IntValue 0, finalState)
            | TransferredContinue, Some _ -> Result.Ok(IntValue 0, finalState)
            | TransferredContinue, None -> Result.Ok(IntValue 0, finalState)
            | JumpToLine lineNumber, _ when routine.EndLineNumber = Some lineNumber -> Result.Ok(IntValue 0, finalState)
            | StopExecution, _ -> runtimeError EscapedStop (Some pos) $"STOP escaped from routine '{routine.Name}'."
            | JumpToLine lineNumber, _ -> runtimeError MissingGotoTarget (Some pos) $"GOTO target line {lineNumber} does not exist in routine '{routine.Name}'."
            | GosubToLine lineNumber, _ -> runtimeError MissingGosubTarget (Some pos) $"GOSUB target line {lineNumber} does not exist in routine '{routine.Name}'."
            | ExitLoop _, _
            | NextLoop _, _ -> runtimeError EscapedLoopControl (Some pos) $"Loop control escaped from routine '{routine.Name}'."))

and private executeBuiltInCall state kind channel args targets pos =
    match kind with
    | Print ->
        resolveChannelId state channel
        |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
            evalExprList stateAfterChannel args
            |> Result.bind (fun (values, nextState) ->
                let line =
                    values
                    |> List.map toRuntimeBuiltInValue
                    |> BuiltInStatements.formatPrintLine
                writeOutputToChannel resolvedChannel line nextState pos
                |> Result.map (fun finalState -> Continue, finalState)))
    | BuiltInKind.Input ->
        resolveChannelId state channel
        |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
            let promptStateResult =
                if List.isEmpty args then
                    Result.Ok stateAfterChannel
                else
                    let promptText =
                        args
                        |> List.map (fun expr ->
                            match evalExpr stateAfterChannel expr with
                            | Result.Ok(value, _) -> formatOutputValue value
                            | Result.Error _ -> "")
                        |> String.concat " "
                    writeOutputToChannel resolvedChannel promptText stateAfterChannel pos

            promptStateResult
            |> Result.bind (fun promptState ->
                readInputFromChannel resolvedChannel promptState pos
                |> Result.map BuiltInStatements.splitInputLine
                |> Result.bind (fun sourceValues ->
                    let assignInput currentState target index =
                        let raw = if index < sourceValues.Length then sourceValues[index] else ""
                        let expectedType =
                            match target with
                            | WriteVar(_, HIR.HirType.String, _)
                            | WriteArrayElem(_, _, HIR.HirType.String, _) -> BuiltInInputType.InputString
                            | WriteVar(_, HIR.HirType.Float, _)
                            | WriteArrayElem(_, _, HIR.HirType.Float, _) -> BuiltInInputType.InputFloat
                            | _ -> BuiltInInputType.InputInt
                        writeTarget currentState target (BuiltInStatements.parseInputValue expectedType raw |> fromRuntimeBuiltInValue)

                    ((Result.Ok promptState, targets |> List.indexed)
                     ||> List.fold (fun acc (index, target) ->
                         acc |> Result.bind (fun currentState -> assignInput currentState target index)))
                    |> Result.map (fun nextState -> Continue, nextState))))
    | Reference -> Result.Ok(Continue, state)
    | NamedBuiltIn name ->
        let normalized = normalizeIdentifier name
        let unsupportedChannel () =
            match channel with
            | Some _ -> runtimeError UnsupportedChannelExecution (Some pos) $"Built-in '{name}' does not support channel execution in the interpreter yet."
            | None -> Result.Ok(Continue, state)

        let requireArity expected actual =
            if actual = expected then Result.Ok()
            else runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects {expected} argument(s)."

        let requireAtLeast minimum actual =
            if actual >= minimum then Result.Ok()
            else runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects at least {minimum} argument(s)."

        let requireChannelId resolvedChannel =
            match resolvedChannel with
            | Some channelId -> Result.Ok channelId
            | None -> runtimeError UnsupportedChannelExecution (Some pos) $"Built-in '{name}' requires a channel id."

        let executeScreenOp expectedArgCount atLeast (action: ChannelId option -> RuntimeState -> int list -> Result<RuntimeState, RuntimeError>) =
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        let arityCheck =
                            if atLeast then requireAtLeast expectedArgCount values.Length
                            else requireArity expectedArgCount values.Length
                        arityCheck
                        |> Result.bind (fun () ->
                            let numericArgs = values |> List.map asInt
                            action resolvedChannel nextState numericArgs
                            |> Result.map (fun finalState -> Continue, finalState)))))

        match normalized with
        | "STOP" -> Result.Ok(StopExecution, state)
        | "MODE" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 1 values.Length
                    |> Result.bind (fun () ->
                        let requestedMode =
                            values
                            |> List.head
                            |> asInt
                            |> screenModeFromNumber

                        match nextState.Options.Host.Screen.SetMode requestedMode with
                        | Result.Ok() -> Result.Ok(Continue, nextState)
                        | Result.Error hostError ->
                            runtimeError BuiltInStatementNotImplemented (Some pos) (hostErrorText hostError))))
        | "RANDOMISE" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    match values with
                    | [] ->
                        let seed = int (nextState.Options.Clock().Subtract(DateTime.UnixEpoch).TotalSeconds)
                        Result.Ok(Continue, { nextState with RandomSource = Random(seed) })
                    | [ seedValue ] ->
                        Result.Ok(Continue, { nextState with RandomSource = Random(asInt seedValue) })
                    | _ ->
                        runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero or one argument."))
        | "OPEN" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                requireChannelId resolvedChannel
                |> Result.bind (fun channelId ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ pathValue ] ->
                                match nextState.Options.Host.Channels.OpenAs(channelId, asString pathValue) with
                                | Result.Ok() -> Result.Ok(Continue, nextState)
                                | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "OPEN_IN" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                requireChannelId resolvedChannel
                |> Result.bind (fun channelId ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ pathValue ] ->
                                match nextState.Options.Host.Files.OpenFileAs(channelId, asString pathValue, OpenForInput) with
                                | Result.Ok() -> Result.Ok(Continue, nextState)
                                | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "OPEN_NEW" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                requireChannelId resolvedChannel
                |> Result.bind (fun channelId ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ pathValue ] ->
                                match nextState.Options.Host.Files.OpenFileAs(channelId, asString pathValue, OpenForOutput) with
                                | Result.Ok() -> Result.Ok(Continue, nextState)
                                | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "CLOSE" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                requireChannelId resolvedChannel
                |> Result.bind (fun channelId ->
                    requireArity 0 args.Length
                    |> Result.bind (fun () ->
                        match stateAfterChannel.Options.Host.Channels.Close(channelId) with
                        | Result.Ok() -> Result.Ok(Continue, stateAfterChannel)
                        | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError))))
        | "PAUSE" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 1 values.Length
                    |> Result.map (fun () ->
                        let duration =
                            values
                            |> List.head
                            |> asInt
                            |> max 0
                        nextState.Options.Sleeper duration
                        Continue, nextState)))
        | "CLS" ->
            executeScreenOp 0 false (fun resolvedChannel nextState _ ->
                executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.Clear()) (fun screen -> screen.Clear()))
        | "WINDOW" ->
            executeScreenOp 4 false (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [ width; height; x; y ] ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetWindow(width, height, x, y)) (fun screen -> screen.SetWindow(width, height, x, y))
                | _ -> Result.Ok nextState)
        | "AT" ->
            executeScreenOp 2 false (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [ row; column ] ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetCursor(column, row)) (fun screen -> screen.SetCursor(column, row))
                | _ -> Result.Ok nextState)
        | "CURSOR" ->
            executeScreenOp 2 false (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [ x; y ] ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetCursor(x, y)) (fun screen -> screen.SetCursor(x, y))
                | _ -> Result.Ok nextState)
        | "CSIZE" ->
            executeScreenOp 2 false (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [ width; height ] ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetCharacterSize(width, height)) (fun screen -> screen.SetCharacterSize(width, height))
                | _ -> Result.Ok nextState)
        | "INK" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [] -> Result.Ok nextState
                | _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetInk(numericArgs)) (fun screen -> screen.SetInk(numericArgs))
                    |> Result.bind (fun stateAfterScreen ->
                        executeGraphicsOp resolvedChannel stateAfterScreen pos (fun graphics -> graphics.SetInk(numericArgs))))
        | "PAPER" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | first :: _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetPaper(first)) (fun screen -> screen.SetPaper(first))
                | _ -> Result.Ok nextState)
        | "BORDER" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | first :: _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetBorder(first)) (fun screen -> screen.SetBorder(first))
                | _ -> Result.Ok nextState)
        | "CLEAR" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    requireArity 0 args.Length
                    |> Result.bind (fun () ->
                        executeGraphicsOp resolvedChannel stateAfterChannel pos (fun graphics -> graphics.Clear())
                        |> Result.map (fun finalState -> Continue, finalState))))
        | "SCROLL" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | first :: _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetScroll(first)) (fun screen -> screen.SetScroll(first))
                | _ -> Result.Ok nextState)
        | "WIDTH" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | first :: _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetWidth(first)) (fun screen -> screen.SetWidth(first))
                | _ -> Result.Ok nextState)
        | "PAN" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | first :: _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetPan(first)) (fun screen -> screen.SetPan(first))
                | _ -> Result.Ok nextState)
        | "RECOL" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | first :: _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetRecolor(first)) (fun screen -> screen.SetRecolor(first))
                | _ -> Result.Ok nextState)
        | "PALETTE" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [] -> Result.Ok nextState
                | _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetPalette(numericArgs)) (fun screen -> screen.SetPalette(numericArgs)))
        | "PLOT" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 2 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Plot(normalizeNumber x, normalizeNumber y))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "POINT" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 2 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Point(normalizeNumber x, normalizeNumber y))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "POINT_R" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 2 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ dx; dy ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.PointRelative(normalizeNumber dx, normalizeNumber dy))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "DRAW" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 2 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Draw(normalizeNumber x, normalizeNumber y))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "DLINE" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 4 values.Length
                        |> Result.bind (fun () ->
                            if values.Length % 2 <> 0 then
                                runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects an even number of coordinate arguments."
                            else
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.DLine(values |> List.map normalizeNumber))
                                |> Result.map (fun finalState -> Continue, finalState)))))
        | "LINE" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 4 values.Length
                        |> Result.bind (fun () ->
                            if values.Length % 2 <> 0 then
                                runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects an even number of coordinate arguments."
                            else
                                let coordinates =
                                    values
                                    |> List.map normalizeNumber
                                    |> List.chunkBySize 2
                                    |> List.choose (function
                                        | [ x; y ] -> Some(x, y)
                                        | _ -> None)

                                coordinates
                                |> List.pairwise
                                |> List.fold
                                    (fun acc ((x1, y1), (x2, y2)) ->
                                        acc
                                        |> Result.bind (fun currentState ->
                                            executeGraphicsOp resolvedChannel currentState pos (fun graphics -> graphics.Line(x1, y1, x2, y2))))
                                    (Result.Ok nextState)
                                |> Result.map (fun finalState -> Continue, finalState)))))
        | "LINE_R" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 2 values.Length
                        |> Result.bind (fun () ->
                            if values.Length % 2 <> 0 then
                                runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects an even number of coordinate arguments."
                            else
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.LineRelative(values |> List.map normalizeNumber))
                                |> Result.map (fun finalState -> Continue, finalState)))))
        | "CIRCLE" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 3 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y; radius ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Circle(normalizeNumber x, normalizeNumber y, normalizeNumber radius))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "CIRCLE_R" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 3 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ dx; dy; radius ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.CircleRelative(normalizeNumber dx, normalizeNumber dy, normalizeNumber radius))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "ELLIPSE" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 5 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y; radius; ratio; angle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Ellipse(normalizeNumber x, normalizeNumber y, normalizeNumber radius, normalizeNumber ratio, normalizeNumber angle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "ELLIPSE_R" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 5 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ dx; dy; radius; ratio; angle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.EllipseRelative(normalizeNumber dx, normalizeNumber dy, normalizeNumber radius, normalizeNumber ratio, normalizeNumber angle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "ARC" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 5 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y; radius; startAngle; endAngle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Arc(normalizeNumber x, normalizeNumber y, normalizeNumber radius, normalizeNumber startAngle, normalizeNumber endAngle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "ARC_R" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 5 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ dx; dy; radius; startAngle; endAngle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.ArcRelative(normalizeNumber dx, normalizeNumber dy, normalizeNumber radius, normalizeNumber startAngle, normalizeNumber endAngle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "BLOCK" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 5 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ width; height; x; y; color ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Block(normalizeNumber width, normalizeNumber height, normalizeNumber x, normalizeNumber y, asInt color))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "FILL" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ fillValue ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetFill(asInt fillValue))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "SCALE" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 3 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y; z ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetScale(normalizeNumber x, normalizeNumber y, normalizeNumber z))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "OVER" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | mode :: _ ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetOver(asInt mode))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "UNDER" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | mode :: _ ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetUnder(asInt mode))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "FLASH" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | mode :: _ ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetFlash(asInt mode))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "PENDOWN" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    requireArity 0 args.Length
                    |> Result.bind (fun () ->
                        executeGraphicsOp resolvedChannel stateAfterChannel pos (fun graphics -> graphics.SetPenDown(true))
                        |> Result.map (fun finalState -> Continue, finalState))))
        | "PENUP" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    requireArity 0 args.Length
                    |> Result.bind (fun () ->
                        executeGraphicsOp resolvedChannel stateAfterChannel pos (fun graphics -> graphics.SetPenDown(false))
                        |> Result.map (fun finalState -> Continue, finalState))))
        | "TURN" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ angle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Turn(normalizeNumber angle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | "TURNTO" ->
            resolveChannelId state channel
            |> Result.bind (fun (resolvedChannel, stateAfterChannel) ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ angle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.TurnTo(normalizeNumber angle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState)))))
        | _ when normalized.StartsWith("TURBO") -> Result.Ok(Continue, state)
        | _ ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                runtimeError BuiltInStatementNotImplemented (Some pos) $"Built-in statement '{kind}' is not implemented.")
    | GotoBuiltIn
    | GosubBuiltIn
    | OnGotoBuiltIn
    | OnGosubBuiltIn -> runtimeError BuiltInStatementNotImplemented (Some pos) $"Built-in statement '{kind}' is not implemented."

and private executeStmt resolveLine gosubStack state stmt =
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
            if truthy value then executeBlock resolveLine nextState thenBlock 0 gosubStack (fun state _ -> Result.Ok(Continue, state))
            else
                match elseBlock with
                | Some block -> executeBlock resolveLine nextState block 0 gosubStack (fun state _ -> Result.Ok(Continue, state))
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
                            executeBlock resolveLine currentState body 0 gosubStack (fun state _ -> Result.Ok(Continue, state))
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
            executeBlock resolveLine currentState body 0 gosubStack (fun state _ -> Result.Ok(Continue, state))
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

and private executeBlock resolveLine state block pc gosubStack kContinue =
    if pc >= block.Length then
        kContinue state gosubStack
    else
        executeStmt resolveLine gosubStack state block[pc]
        |> Result.bind (fun (flow, nextState) ->
            match flow with
            | Continue -> executeBlock resolveLine nextState block (pc + 1) gosubStack kContinue
            | JumpToLine lineNumber ->
                match resolveLine lineNumber with
                | Some target ->
                    target nextState gosubStack
                    |> Result.map (fun (flow, state) ->
                        match flow with
                        | Continue -> TransferredContinue, state
                        | _ -> flow, state)
                | None -> Result.Ok(JumpToLine lineNumber, nextState)
            | GosubToLine lineNumber ->
                let returnCont returnState =
                    executeBlock resolveLine returnState block (pc + 1) gosubStack kContinue

                match resolveLine lineNumber with
                | Some target -> target nextState (returnCont :: gosubStack)
                | None -> Result.Ok(GosubToLine lineNumber, nextState)
            | BareReturn ->
                match gosubStack with
                | returnCont :: _ -> returnCont nextState
                | [] -> Result.Ok(BareReturn, nextState)
            | _ -> Result.Ok(flow, nextState))

and private buildLineTargets rootBlock =
    let rec build block kContinue =
        block
        |> List.mapi (fun index stmt ->
            let directTargets =
                match stmt with
                | LineNumber(value, _) ->
                    [ value,
                      (fun state gosubStack ->
                          executeBlock resolveLine state block index gosubStack kContinue) ]
                | _ -> []

            let continueAfter state gosubStack =
                executeBlock resolveLine state block (index + 1) gosubStack kContinue

            let nestedTargets =
                match stmt with
                | If(_, thenBlock, elseBlock, _) ->
                    let elseTargets =
                        match elseBlock with
                        | Some block -> build block continueAfter
                        | None -> []

                    build thenBlock continueAfter @ elseTargets
                | For(_, _, _, _, _, body, _)
                | Repeat(_, _, body, _) -> build body continueAfter
                | _ -> []

            directTargets @ nestedTargets)
        |> List.concat

    and resolveLine lineNumber =
        targets.Value |> Map.tryFind lineNumber

    and targets : Lazy<Map<int, LineTarget>> =
        lazy (build rootBlock (fun state _ -> Result.Ok(Continue, state)) |> Map.ofList)

    resolveLine

let interpretProgramWithOptions options (program: HirProgram) =
    let initialState =
        { Program = program
          Globals = allocateCells program.Globals
          Frames = []
          DataPointer = 0
          Output = []
          RandomSource = options.Random
          Options = options }

    let resolveLine = buildLineTargets program.Main

    executeBlock resolveLine initialState program.Main 0 [] (fun state _ -> Result.Ok(Continue, state))
    |> Result.bind (fun (flow, finalState) ->
        match flow with
        | Continue
        | TransferredContinue
        | StopExecution -> Result.Ok { Output = finalState.Output }
        | ReturnFromRoutine _
        | BareReturn -> runtimeError EscapedReturn None "Return escaped the top-level program."
        | JumpToLine lineNumber -> runtimeError MissingGotoTarget None $"GOTO target line {lineNumber} does not exist."
        | GosubToLine lineNumber -> runtimeError MissingGosubTarget None $"GOSUB target line {lineNumber} does not exist."
        | ExitLoop _
        | NextLoop _ -> runtimeError EscapedLoopControl None "Loop control escaped the top-level program.")

let interpretProgram program =
    interpretProgramWithOptions defaultRuntimeOptions program
