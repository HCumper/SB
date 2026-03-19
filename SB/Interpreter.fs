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
    | ArrayValue of HirType * Dictionary<string, RuntimeValue>

type RuntimeError = {
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

type private Cell = { mutable Value: RuntimeValue }

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
    | ExitLoop of LoopId
    | NextLoop of LoopId
    | StopExecution

let private runtimeError position message =
    Result.Error { Message = message; Position = position }

let private defaultValue hirType =
    match hirType with
    | HIR.HirType.Int -> IntValue 0
    | HIR.HirType.Float -> FloatValue 0.0
    | HIR.HirType.String -> StringValue ""
    | HIR.HirType.Void -> IntValue 0
    | HIR.HirType.Array inner -> ArrayValue(inner, Dictionary<string, RuntimeValue>())

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

let private requireCell state symbolId pos =
    match lookupCell state symbolId with
    | Some cell -> Result.Ok cell
    | None -> runtimeError (Some pos) $"No storage cell exists for symbol {symbolId}."

let private arrayKey indexes =
    indexes |> List.map string |> String.concat ","

let private tryGetArrayCell (value: RuntimeValue) =
    match value with
    | ArrayValue(_, cells) -> Result.Ok cells
    | _ -> runtimeError None "Target is not an array."

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

let private symbolName state symbolId =
    Map.tryFind symbolId state.Program.SymbolNames |> Option.defaultValue (string symbolId)

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
            tryGetArrayCell cell.Value
            |> Result.bind (fun entries ->
                evalExprList state indexes
                |> Result.map (fun (indexValues, nextState) ->
                    let key = indexValues |> List.map asInt |> arrayKey
                    let value =
                        match entries.TryGetValue key with
                        | true, existing -> existing
                        | _ ->
                            match cell.Value with
                            | ArrayValue(innerType, _) -> defaultValue innerType
                            | _ -> IntValue 0
                    value, nextState)))
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
            evalExprList state args
            |> Result.bind (fun (argValues, nextState) -> callRoutine nextState routine argValues pos)
        | None -> evalBuiltInFunction state symbolId args pos

and private evalExprList state exprs =
    let folder acc expr =
        acc
        |> Result.bind (fun (values, currentState) ->
            evalExpr currentState expr
            |> Result.map (fun (value, nextState) -> values @ [ value ], nextState))

    List.fold folder (Result.Ok([], state)) exprs

and private evalBuiltInFunction state symbolId argExprs pos =
    let name = symbolName state symbolId
    let normalized = normalizeIdentifier name

    let evalArgs () = evalExprList state argExprs
    let expectOneNumeric f =
        match argExprs with
        | [ _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) -> f values[0], nextState)
        | _ -> runtimeError (Some pos) $"Built-in function '{name}' expects one argument."

    match normalized with
    | "ABS" -> expectOneNumeric (fun value -> makeNumericValue HIR.HirType.Int (abs (normalizeNumber value)))
    | "INT" -> expectOneNumeric (fun value -> IntValue (int (Math.Floor(normalizeNumber value))))
    | "ROUND" -> expectOneNumeric (fun value -> IntValue (int (Math.Round(normalizeNumber value))))
    | "STR$" ->
        match argExprs with
        | [ _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) -> StringValue (asString values[0]), nextState)
        | _ -> runtimeError (Some pos) $"Built-in function '{name}' expects one argument."
    | "VAL" ->
        match argExprs with
        | [ _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) ->
                match Double.TryParse(asString values[0], NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
                | true, number -> FloatValue number, nextState
                | _ -> IntValue 0, nextState)
        | _ -> runtimeError (Some pos) $"Built-in function '{name}' expects one argument."
    | "LEFT$" ->
        match argExprs with
        | [ _; _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) ->
                let source = asString values[0]
                let length = max 0 (min source.Length (asInt values[1]))
                StringValue(source.Substring(0, length)), nextState)
        | _ -> runtimeError (Some pos) $"Built-in function '{name}' expects two arguments."
    | "RIGHT$" ->
        match argExprs with
        | [ _; _ ] ->
            evalArgs ()
            |> Result.map (fun (values, nextState) ->
                let source = asString values[0]
                let length = max 0 (min source.Length (asInt values[1]))
                StringValue(source.Substring(source.Length - length, length)), nextState)
        | _ -> runtimeError (Some pos) $"Built-in function '{name}' expects two arguments."
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
        | _ -> runtimeError (Some pos) $"Built-in function '{name}' received unsupported arguments."
    | _ -> runtimeError (Some pos) $"Built-in function '{name}' is not implemented."

and private writeTarget state target value =
    match target with
    | WriteVar(symbolId, _, pos) ->
        requireCell state symbolId pos
        |> Result.map (fun cell ->
            cell.Value <- value
            state)
    | WriteArrayElem(symbolId, indexes, _, pos) ->
        requireCell state symbolId pos
        |> Result.bind (fun cell ->
            tryGetArrayCell cell.Value
            |> Result.bind (fun entries ->
                evalExprList state indexes
                |> Result.map (fun (indexValues, nextState) ->
                    let key = indexValues |> List.map asInt |> arrayKey
                    entries[key] <- value
                    nextState)))

and private callRoutine state (routine: HirRoutine) argValues pos =
    let parameterCells =
        (routine.Parameters, argValues)
        ||> List.map2 (fun storage value -> storage.Symbol, { Value = value })
        |> Map.ofList

    let localCells =
        allocateCells routine.Locals

    let frame =
        { Cells = Map.fold (fun acc key value -> Map.add key value acc) localCells parameterCells
          ReturnType = routine.ReturnType }

    let stateWithFrame = withFrame frame state

    executeBlock stateWithFrame routine.Body
    |> Result.bind (fun (flow, afterBody) ->
        let finalState = popFrame afterBody
        match flow, routine.ReturnType with
        | ReturnFromRoutine value, Some _ -> Result.Ok(value |> Option.defaultValue (IntValue 0), finalState)
        | ReturnFromRoutine _, None -> Result.Ok(IntValue 0, finalState)
        | Continue, Some _ -> Result.Ok(IntValue 0, finalState)
        | Continue, None -> Result.Ok(IntValue 0, finalState)
        | StopExecution, _ -> runtimeError (Some pos) $"STOP escaped from routine '{routine.Name}'."
        | ExitLoop _, _
        | NextLoop _, _ -> runtimeError (Some pos) $"Loop control escaped from routine '{routine.Name}'.")

and private executeBuiltInCall state kind channel args targets pos =
    let unsupportedChannel name =
        match channel with
        | Some _ -> runtimeError (Some pos) $"Built-in '{name}' does not support channel execution in the interpreter yet."
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
    | NamedBuiltIn name when normalizeIdentifier name = "STOP" -> Result.Ok(StopExecution, state)
    | GotoBuiltIn
    | GosubBuiltIn
    | OnGotoBuiltIn
    | OnGosubBuiltIn
    | NamedBuiltIn _ -> runtimeError (Some pos) $"Built-in statement '{kind}' is not implemented."

and private executeStmt state stmt =
    match stmt with
    | Assign(target, expr, _) ->
        evalExpr state expr
        |> Result.bind (fun (value, nextState) ->
            writeTarget nextState target value
            |> Result.map (fun finalState -> Continue, finalState))
    | ProcCall(symbolId, _, args, pos) ->
        evalExprList state args
        |> Result.bind (fun (values, nextState) ->
            match nextState.Program.Routines |> List.tryFind (fun routine -> routine.Symbol = symbolId) with
            | Some routine ->
                callRoutine nextState routine values pos
                |> Result.map (fun (_, finalState) -> Continue, finalState)
            | None -> runtimeError (Some pos) $"Procedure '{symbolName nextState symbolId}' is not implemented.")
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
            | _ -> runtimeError (Some pos) "FOR requires start, end, and step values.")
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
    | Goto(_, pos) -> runtimeError (Some pos) "GOTO is not implemented in the interpreter yet."
    | OnGoto(_, _, pos) -> runtimeError (Some pos) "ON GOTO is not implemented in the interpreter yet."
    | Gosub(_, pos) -> runtimeError (Some pos) "GOSUB is not implemented in the interpreter yet."
    | OnGosub(_, _, pos) -> runtimeError (Some pos) "ON GOSUB is not implemented in the interpreter yet."
    | Return(expr, _) ->
        match expr with
        | Some valueExpr ->
            evalExpr state valueExpr |> Result.map (fun (value, nextState) -> ReturnFromRoutine(Some value), nextState)
        | None -> Result.Ok(ReturnFromRoutine None, state)
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
                | None -> runtimeError (Some pos) $"RESTORE line {lineNumber} does not exist.")
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
                | None -> runtimeError (Some pos) "READ moved past the end of DATA."
        assign state targets
    | Remark _ -> Result.Ok(Continue, state)

and private executeBlock state block =
    let rec loop currentState remaining =
        match remaining with
        | [] -> Result.Ok(Continue, currentState)
        | stmt :: tail ->
            executeStmt currentState stmt
            |> Result.bind (fun (flow, nextState) ->
                match flow with
                | Continue -> loop nextState tail
                | _ -> Result.Ok(flow, nextState))

    loop state block

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
        | ReturnFromRoutine _ -> runtimeError None "Return escaped the top-level program."
        | ExitLoop _
        | NextLoop _ -> runtimeError None "Loop control escaped the top-level program.")

let interpretProgram program =
    interpretProgramWithOptions defaultRuntimeOptions program
