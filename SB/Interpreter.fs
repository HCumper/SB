module Interpreter

open System
open System.Collections.Generic
open System.Globalization
open System.Diagnostics
open System.Threading

open HIR
open SBRuntime
open Types

type RuntimeValue =
    | IntValue of int
    | FloatValue of double
    | StringValue of string
    | ArrayValue of HirType * int list * Dictionary<string, Cell>

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

type private ErrorInfo = {
    Number: int
    Name: string
    Description: string
    LineNumber: int option
    RetryLineNumber: int option
    ContinueLineNumber: int option
    RuntimeError: RuntimeError
}

type RuntimeLoadedProgram = {
    Ast: SyntaxAst.Ast
    Hir: HirProgram
}

type RuntimeOptions = {
    Host: IRuntimeHost
    Random: Random
    Clock: unit -> DateTime
    Sleeper: int -> unit
    ExecutionThrottle: ExecutionThrottleSettings option
    InitialSourceProgram: SyntaxAst.Ast option
    LoadProgram: string -> Result<RuntimeLoadedProgram, string>
    MergeProgram: SyntaxAst.Ast * string -> Result<RuntimeLoadedProgram, string>
}

and ExecutionThrottleSettings = {
    TargetStatementsPerSecond: int
    MaxRunAheadMilliseconds: int
}

type ExecutionResult = {
    Output: string list
}

let defaultRuntimeOptions =
    { Host =
        DefaultHost.create {
            ReadLine = fun () -> Console.ReadLine() |> Option.ofObj
            ReadScreenLine = fun _ -> Console.ReadLine() |> Option.ofObj
            FlushInput = fun () -> ()
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
      Random = QlRandom()
      Clock = fun () -> DateTime.UtcNow
      Sleeper = fun milliseconds -> System.Threading.Thread.Sleep(milliseconds)
      ExecutionThrottle = None
      InitialSourceProgram = None
      LoadProgram = fun path -> Result.Error $"Runtime program loading is not configured for '{path}'."
      MergeProgram = fun (_, path) -> Result.Error $"Runtime program merge is not configured for '{path}'." }

type private Frame = {
    Cells: Map<SymbolId, Cell>
    ReturnType: HirType option
    ScreenChannel: ChannelId
}

type private RuntimeState = {
    Program: HirProgram
    SourceProgram: SyntaxAst.Ast option
    Globals: Map<SymbolId, Cell>
    Frames: Frame list
    DataPointer: int
    Output: string list
    CurrentScreenChannel: ChannelId
    DefaultGraphicsScale: double * double * double
    ChannelGraphicsScales: Map<ChannelId, double * double * double>
    CurrentLineNumber: int option
    NextLineNumber: int option
    ActiveErrorHandler: HirBlock option
    LastError: ErrorInfo option
    InErrorHandler: bool
    RandomSource: Random
    ExecutionGovernor: (unit -> unit) option
    Options: RuntimeOptions
}

type private ControlFlow =
    | Continue
    | TransferredContinue
    | ReturnFromRoutine of RuntimeValue option
    | BareReturn
    | RetryError of int option
    | ContinueError of int option
    | ExitLoop of LoopId
    | NextLoop of LoopId
    | JumpToLine of int
    | GosubToLine of int
    | ReplaceProgram of HirProgram * SyntaxAst.Ast option
    | RestartProgram of HirProgram * SyntaxAst.Ast option * int option
    | StopExecution

type private BlockContinuation = RuntimeState -> GosubReturn list -> Result<ControlFlow * RuntimeState, RuntimeError>

and private PendingBlock =
    { State: RuntimeState
      Block: HirBlock
      Pc: int
      GosubStack: GosubReturn list
      KContinue: BlockContinuation }

and private ExecutionOutcome =
    | Completed of Result<ControlFlow * RuntimeState, RuntimeError>
    | Transfer of PendingBlock

and private GosubReturn = RuntimeState -> PendingBlock
and private LineTarget = RuntimeState -> GosubReturn list -> PendingBlock

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

let private qlErrorTable =
    dict [
        MissingStorageCell, (-15, "ERR_BP", "Bad parameter")
        MissingDynamicStorageCell, (-15, "ERR_BP", "Bad parameter")
        InvalidArrayTarget, (-15, "ERR_BP", "Bad parameter")
        BuiltInArityMismatch, (-15, "ERR_BP", "Bad parameter")
        BuiltInUnsupportedArguments, (-15, "ERR_BP", "Bad parameter")
        BuiltInFunctionNotImplemented, (-19, "ERR_NI", "Not implemented")
        InvalidReferenceActual, (-15, "ERR_BP", "Bad parameter")
        EscapedStop, (-19, "ERR_NI", "Not implemented")
        MissingGotoTarget, (-21, "ERR_BL", "Bad line of Basic")
        MissingGosubTarget, (-21, "ERR_BL", "Bad line of Basic")
        EscapedLoopControl, (-21, "ERR_BL", "Bad line of Basic")
        UnsupportedChannelExecution, (-16, "ERR_FE", "File error")
        BuiltInStatementNotImplemented, (-19, "ERR_NI", "Not implemented")
        ProcedureNotImplemented, (-19, "ERR_NI", "Not implemented")
        InvalidForBounds, (-15, "ERR_BP", "Bad parameter")
        InvalidRestoreTarget, (-21, "ERR_BL", "Bad line of Basic")
        ReadPastData, (-21, "ERR_BL", "Bad line of Basic")
        EscapedReturn, (-21, "ERR_BL", "Bad line of Basic")
    ]

let private errorInfoFromRuntimeError state (error: RuntimeError) =
    let number, name, description =
        match qlErrorTable.TryGetValue error.Code with
        | true, value -> value
        | _ -> -19, "ERR_NI", "Not implemented"

    { Number = number
      Name = name
      Description = description
      LineNumber = state.CurrentLineNumber
      RetryLineNumber = state.CurrentLineNumber
      ContinueLineNumber = state.NextLineNumber
      RuntimeError = error }

let private formatReportedError (errorInfo: ErrorInfo) =
    match errorInfo.LineNumber with
    | Some lineNumber -> $"{errorInfo.Description} ({errorInfo.Number}) at line {lineNumber}"
    | None -> $"{errorInfo.Description} ({errorInfo.Number})"

let private resetErrorProcessing state =
    { state with
        ActiveErrorHandler = None
        LastError = None
        InErrorHandler = false }

let private mergeProgramAst currentAst incomingAst =
    let mergeLines currentLines incomingLines =
        let incomingByLine =
            incomingLines
            |> List.choose (fun (SyntaxAst.Line(_, lineNumber, _) as line) -> lineNumber |> Option.map (fun value -> value, line))
            |> Map.ofList

        let mergedExisting =
            currentLines
            |> List.choose (fun (SyntaxAst.Line(_, lineNumber, _) as line) ->
                match lineNumber with
                | Some value ->
                    match incomingByLine.TryFind value with
                    | Some replacement -> Some replacement
                    | None -> Some line
                | None -> Some line)

        let existingNumbered =
            currentLines
            |> List.choose (fun (SyntaxAst.Line(_, lineNumber, _)) -> lineNumber)
            |> Set.ofList

        let appendedIncoming =
            incomingLines
            |> List.choose (fun (SyntaxAst.Line(_, lineNumber, _) as line) ->
                match lineNumber with
                | Some value when not (existingNumbered.Contains value) -> Some(value, line)
                | _ -> None)
            |> List.sortBy fst
            |> List.map snd

        mergedExisting @ appendedIncoming

    match currentAst, incomingAst with
    | SyntaxAst.Program(pos, currentLines), SyntaxAst.Program(_, incomingLines) -> SyntaxAst.Program(pos, mergeLines currentLines incomingLines)

let private defaultValue hirType dimensions =
    match hirType with
    | HIR.HirType.Int -> IntValue 0
    | HIR.HirType.Float -> FloatValue 0.0
    | HIR.HirType.String -> StringValue ""
    | HIR.HirType.Void -> IntValue 0
    | HIR.HirType.Array inner -> ArrayValue(inner, dimensions |> Option.defaultValue [], Dictionary<string, Cell>())

let private tryParseHexInt (value: string) =
    if value.StartsWith("$") && value.Length > 1 then
        match UInt32.TryParse(value[1..], NumberStyles.AllowHexSpecifier, CultureInfo.InvariantCulture) with
        | true, parsed -> Some(int (int32 parsed))
        | _ -> None
    else
        None

let private normalizeNumber value =
    match value with
    | IntValue n -> double n
    | FloatValue f -> f
    | StringValue s ->
        match tryParseHexInt s with
        | Some n -> float n
        | None ->
            match Double.TryParse(s, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
            | true, n -> n
            | _ -> 0.0
    | ArrayValue _ -> 0.0

let private asInt value =
    match value with
    | IntValue n -> n
    | FloatValue f -> int (Math.Round(f))
    | StringValue s ->
        match tryParseHexInt s with
        | Some n -> n
        | None ->
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

let private encodeCompositeColorSpec values =
    match values with
    | [] -> None
    | [ mainColor ] -> Some(abs mainColor % 8)
    | [ mainColor; contrastColor ] ->
        let main = abs mainColor % 8
        let contrast = abs contrastColor % 8
        Some(main ||| ((main ^^^ contrast) <<< 3) ||| (3 <<< 6))
    | mainColor :: contrastColor :: stipple :: _ ->
        let main = abs mainColor % 8
        let contrast = abs contrastColor % 8
        let pattern = abs stipple % 4
        Some(main ||| ((main ^^^ contrast) <<< 3) ||| (pattern <<< 6))

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

let private printContinueSentinel = "\uE000PRINT_CONTINUE\uE000"
let private printCommaSentinel = "\uE000PRINT_COMMA\uE000"

let private formatOutputValue value = asString value

let private formatScreenOutputValue value =
    match value with
    | FloatValue f when not (Double.IsNaN f || Double.IsInfinity f) ->
        let tryFixed decimals =
            let rounded = Math.Round(f, decimals, MidpointRounding.AwayFromZero)
            if abs (rounded - f) < 1e-9 then
                rounded
                    .ToString($"F{decimals}", CultureInfo.InvariantCulture)
                    .TrimEnd('0')
                    .TrimEnd('.')
                |> Some
            else
                None

        [ 0 .. 6 ]
        |> List.tryPick tryFixed
        |> Option.defaultValue (f.ToString("G15", CultureInfo.InvariantCulture))
    | _ -> asString value

let private formatScreenPrintValues values =
    let buffer = Text.StringBuilder()
    let mutable column = 0

    let appendText (text: string) =
        buffer.Append(text) |> ignore
        column <- column + text.Length

    for value in values do
        match value with
        | StringValue sentinel when sentinel = printCommaSentinel ->
            let nextTabStop = ((column / 8) + 1) * 8
            let spaces = max 1 (nextTabStop - column)
            appendText (System.String(' ', spaces))
        | _ ->
            appendText (formatScreenOutputValue value)

    buffer.ToString()

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
    | ArrayValue(_, _, cells) -> Result.Ok cells
    | _ -> runtimeError InvalidArrayTarget None "Target is not an array."

let private getArrayElementType (value: RuntimeValue) =
    match value with
    | ArrayValue(innerType, _, _) -> innerType
    | _ -> HIR.HirType.Int

let private getArrayDimensions (value: RuntimeValue) =
    match value with
    | ArrayValue(_, dimensions, _) -> dimensions
    | _ -> []

let private getOrCreateArrayEntry (entries: Dictionary<string, Cell>) innerType key =
    match entries.TryGetValue key with
    | true, existing -> existing
    | _ ->
        let created = { Value = defaultValue innerType None }
        entries[key] <- created
        created

let private readStringCharValue sourceValue indexValue =
    let oneBasedIndex = asInt indexValue
    let text = asString sourceValue

    if oneBasedIndex < 1 || oneBasedIndex > text.Length then
        StringValue ""
    else
        StringValue(string text[oneBasedIndex - 1])

let private writeStringCharValue sourceValue indexValue replacementValue =
    let oneBasedIndex = asInt indexValue

    if oneBasedIndex < 1 then
        sourceValue
    else
        let text = asString sourceValue
        let replacementText = asString replacementValue
        let replacementChar = if String.IsNullOrEmpty replacementText then ' ' else replacementText[0]
        let zeroBasedIndex = oneBasedIndex - 1
        let bufferLength = max text.Length (zeroBasedIndex + 1)
        let buffer = Array.create bufferLength ' '
        text.ToCharArray() |> Array.iteri (fun index ch -> buffer[index] <- ch)
        buffer[zeroBasedIndex] <- replacementChar
        StringValue(System.String(buffer))

let private allocateCells (storages: HirStorage list) =
    storages
    |> List.map (fun storage -> storage.Symbol, { Value = defaultValue storage.Type storage.Dimensions })
    |> Map.ofList

let private updateOutput line state =
    state.Options.Host.Screen.WriteText line
    { state with Output = state.Output @ [ line ] }

let private hostErrorText = function
    | ChannelNotFound(ChannelId channelId) -> $"Channel #{channelId} does not exist."
    | DeviceOpenFailed detail -> detail
    | UnsupportedHostOperation detail -> detail
    | InvalidHostArgument detail -> detail

let private mapMemoryFunctionError pos = function
    | InvalidHostArgument detail -> runtimeError BuiltInUnsupportedArguments (Some pos) detail
    | hostError -> runtimeError BuiltInFunctionNotImplemented (Some pos) (hostErrorText hostError)

let private mapMemoryStatementError pos = function
    | InvalidHostArgument detail -> runtimeError BuiltInUnsupportedArguments (Some pos) detail
    | hostError -> runtimeError BuiltInStatementNotImplemented (Some pos) (hostErrorText hostError)

let private peekMemory state pos name address =
    let operation =
        match normalizeIdentifier name with
        | "PEEK" -> state.Options.Host.Memory.Peek8
        | "PEEK_W" -> state.Options.Host.Memory.Peek16
        | "PEEK_L" -> state.Options.Host.Memory.Peek32
        | _ -> failwith $"Unsupported memory function '{name}'."

    match operation address with
    | Result.Ok value -> Result.Ok(IntValue value, state)
    | Result.Error hostError -> mapMemoryFunctionError pos hostError

let private pokeMemory state pos name address value =
    let operation =
        match normalizeIdentifier name with
        | "POKE" -> state.Options.Host.Memory.Poke8
        | "POKE_W" -> state.Options.Host.Memory.Poke16
        | "POKE_L" -> state.Options.Host.Memory.Poke32
        | _ -> failwith $"Unsupported memory statement '{name}'."

    match operation (address, value) with
    | Result.Ok() -> Result.Ok(Continue, state)
    | Result.Error hostError -> mapMemoryStatementError pos hostError

let private screenModeFromNumber = function
    | 4 -> QlMode4
    | 8 -> QlMode8
    | value -> ExtendedMode value

let private createExecutionGovernor settings sleeper =
    let targetStatementsPerSecond = max 1 settings.TargetStatementsPerSecond
    let maxRunAheadMilliseconds = max 0 settings.MaxRunAheadMilliseconds
    let stopwatch = Stopwatch.StartNew()
    let mutable executedStatements = 0L

    fun () ->
        executedStatements <- executedStatements + 1L
        let expectedElapsedMilliseconds =
            (float executedStatements * 1000.0) / float targetStatementsPerSecond
        let actualElapsedMilliseconds = stopwatch.Elapsed.TotalMilliseconds
        let runAheadMilliseconds = expectedElapsedMilliseconds - actualElapsedMilliseconds

        if runAheadMilliseconds > float maxRunAheadMilliseconds then
            let delay = int (Math.Ceiling(runAheadMilliseconds - float maxRunAheadMilliseconds))
            if delay > 0 then
                sleeper delay

let private reinitializeProgramState state program sourceProgram =
    { state with
        Program = program
        SourceProgram = sourceProgram
        Globals = allocateCells program.Globals
        Frames = []
        DataPointer = 0
        CurrentScreenChannel = ChannelId 1
        CurrentLineNumber = None
        NextLineNumber = None }
    |> resetErrorProcessing

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

let private writeRenderedOutputToChannel channelId renderedLine capturedLine state pos =
    match channelId with
    | None ->
        state.Options.Host.Screen.WriteText renderedLine
        Result.Ok { state with Output = state.Output @ [ capturedLine ] }
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok channel ->
            channel.WriteText renderedLine
            let captureOutput =
                match channel.Kind with
                | NamedChannel "NUL" -> false
                | _ -> true
            if captureOutput then
                Result.Ok { state with Output = state.Output @ [ capturedLine ] }
            else
                Result.Ok state
        | Result.Error hostError ->
            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)

let private isScreenLikeOutputChannel channelId state =
    match channelId with
    | None -> true
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok (:? IScreenChannel) -> true
        | Result.Ok _ -> false
        | Result.Error _ -> false

let private advanceScreenLine channelId state pos =
    match channelId with
    | None ->
        state.Options.Host.Screen.NewLine()
        Result.Ok state
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok (:? IScreenChannel as screenChannel) ->
            screenChannel.NewLine()
            Result.Ok state
        | Result.Ok _ ->
            Result.Ok state
        | Result.Error hostError ->
            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)

let private readInputFromChannel channelId state pos =
    let rec waitForHostLine () =
        match state.Options.Host.Input.ReadLine() with
        | Some line -> Some line
        | None ->
            state.Options.Sleeper 50
            waitForHostLine ()

    let rec waitForChannelLine (reader: unit -> string option) =
        match reader () with
        | Some line -> Some line
        | None ->
            state.Options.Sleeper 50
            waitForChannelLine reader

    match channelId with
    | None ->
        match state.Options.Host.Channels.Get(ChannelId 1) with
        | Result.Ok channel -> Result.Ok(waitForChannelLine channel.ReadText)
        | Result.Error hostError ->
            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok (:? SBRuntime.IScreenChannel as screenChannel) -> Result.Ok(waitForChannelLine screenChannel.ReadText)
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

let private currentGraphicsScale channelId state =
    match channelId with
    | Some resolvedChannelId ->
        state.ChannelGraphicsScales
        |> Map.tryFind resolvedChannelId
        |> Option.defaultValue state.DefaultGraphicsScale
    | None -> state.DefaultGraphicsScale

let private currentDrawingWindow channelId state pos =
    match channelId with
    | None -> Result.Ok(state.Options.Host.Screen.GetWindow(), state.Options.Host.Screen.GetPan())
    | Some resolvedChannelId ->
        match state.Options.Host.Channels.Get resolvedChannelId with
        | Result.Ok (:? IScreenChannel as screenChannel) ->
            Result.Ok(screenChannel.GetWindow(), screenChannel.GetPan())
        | Result.Ok _ ->
            let (ChannelId id) = resolvedChannelId
            runtimeError UnsupportedChannelExecution (Some pos) $"Channel #{id} is not a screen channel."
        | Result.Error hostError ->
            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)

let private executeGraphicsOp channelId state pos (action: IGraphicsDevice -> unit) =
    let run nextState =
        currentDrawingWindow channelId nextState pos
        |> Result.bind (fun (window, pan) ->
            let scale = currentGraphicsScale channelId nextState
            nextState.Options.Host.Graphics.SetDrawingContext(window, pan, scale)
            action nextState.Options.Host.Graphics
            Result.Ok nextState)

    match channelId with
    | None -> run state
    | Some _ ->
        validateScreenChannel channelId state pos
        |> Result.bind (fun () -> run state)

let private withFrame frame state =
    { state with Frames = frame :: state.Frames }

let private popFrame state =
    match state.Frames with
    | frame :: rest -> { state with Frames = rest; CurrentScreenChannel = frame.ScreenChannel }
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
    | ReadStringChar(symbolId, index, _, pos) ->
        requireCell state symbolId pos
        |> Result.bind (fun cell ->
            evalExpr state index
            |> Result.map (fun (indexValue, nextState) ->
                readStringCharValue cell.Value indexValue, nextState))
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
    | DynamicReadStringChar(name, index, _, pos) ->
        requireDynamicCell state name pos
        |> Result.bind (fun cell ->
            evalExpr state index
            |> Result.map (fun (indexValue, nextState) ->
                readStringCharValue cell.Value indexValue, nextState))
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
                    | Instr ->
                        IntValue((asString rightValue).IndexOf(asString leftValue, StringComparison.Ordinal) + 1)
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
    let normalizedName = normalizeIdentifier name
    let allowRangePair =
        match normalizedName, argExprs with
        | "RND", [ Binary(SliceRange, _, _, _, _) ] -> true
        | _ -> false
    let runtimeArgs =
        argExprs
        |> List.collect (function
            | Binary(SliceRange, lhs, rhs, _, _) -> [ lhs; rhs ]
            | expr -> [ expr ])

    evalExprList state runtimeArgs
    |> Result.bind (fun (values, nextState) ->
        match normalizedName, values with
        | "ERLIN", [] ->
            let value =
                nextState.LastError
                |> Option.bind _.LineNumber
                |> Option.defaultValue 0
            Result.Ok(IntValue value, nextState)
        | "ERNUM", [] ->
            let value =
                nextState.LastError
                |> Option.map _.Number
                |> Option.defaultValue 0
            Result.Ok(IntValue value, nextState)
        | ("ERR_NC" | "ERR_NJ" | "ERR_OM" | "ERR_OR" | "ERR_BO" | "ERR_NO" | "ERR_NF" | "ERR_EX" | "ERR_IU" | "ERR_EF" | "ERR_DF" | "ERR_BN" | "ERR_TE" | "ERR_FF" | "ERR_BP" | "ERR_FE" | "ERR_XP" | "ERR_OV" | "ERR_NI" | "ERR_RO" | "ERR_BL" as errorName), [] ->
            let value =
                match nextState.LastError with
                | Some errorInfo when errorInfo.Name = errorName -> 1
                | _ -> 0
            Result.Ok(IntValue value, nextState)
        | "BEEPING", [] ->
            Result.Ok(IntValue(if nextState.Options.Host.Sound.IsBeeping() then 1 else 0), nextState)
        | "BEEPING", _ ->
            runtimeError BuiltInArityMismatch (Some pos) $"Built-in function '{name}' expects zero arguments."
        | "DIMN", [ arrayValue; dimensionValue ] ->
            match arrayValue with
            | ArrayValue _ ->
                let requestedDimension = asInt dimensionValue
                let dimensions = getArrayDimensions arrayValue
                let value =
                    if requestedDimension >= 1 && requestedDimension <= dimensions.Length then
                        dimensions[requestedDimension - 1]
                    else
                        0
                Result.Ok(IntValue value, nextState)
            | _ ->
                runtimeError BuiltInUnsupportedArguments (Some pos) $"Built-in function '{name}' requires an array as its first argument."
        | "DIMN", _ ->
            runtimeError BuiltInArityMismatch (Some pos) $"Built-in function '{name}' expects two arguments."
        | ("PEEK" | "PEEK_W" | "PEEK_L"), [ addressValue ] ->
            peekMemory nextState pos name (asInt addressValue)
        | ("PEEK" | "PEEK_W" | "PEEK_L"), _ ->
            runtimeError BuiltInArityMismatch (Some pos) $"Built-in function '{name}' expects one argument."
        | _ ->
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
    | Some(ExplicitChannel(expr: HirExpr)) ->
        evalExpr state expr
        |> Result.map (fun (value, nextState) -> Some(ChannelId(asInt value)), nextState)
    | Some(ImplicitChannel _) ->
        runtimeError UnsupportedChannelExecution None "Implicit channels are only supported for selected built-ins."

and private withResolvedChannel state channelExpr pos action =
    match channelExpr with
    | None -> action None state
    | Some(ExplicitChannel(expr: HirExpr)) ->
        evalExpr state expr
        |> Result.bind (fun (value, nextState) -> action (Some(ChannelId(asInt value))) nextState)
    | Some(ImplicitChannel(expr: HirExpr)) ->
        evalExpr state expr
        |> Result.bind (fun (value, nextState) ->
            match nextState.Options.Host.Channels.Open(asString value) with
            | Result.Ok channelId ->
                action (Some channelId) nextState
                |> Result.bind (fun (flow, finalState) ->
                    match finalState.Options.Host.Channels.Close(channelId) with
                    | Result.Ok() -> Result.Ok(flow, finalState)
                    | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError))
            | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError))

and private withEffectiveChannel state channelExpr defaultChannel pos action =
    match channelExpr with
    | Some _ -> withResolvedChannel state channelExpr pos action
    | None -> action defaultChannel state

and private defaultScreenChannelId state =
    Some(ChannelId 1)

and private resolveRequiredChannelId state channelExpr defaultChannel pos name =
    match channelExpr with
    | None -> Result.Ok(defaultChannel, state)
    | Some(ExplicitChannel(expr: HirExpr)) ->
        evalExpr state expr
        |> Result.map (fun (value, nextState) -> ChannelId(asInt value), nextState)
    | Some(ImplicitChannel _) ->
        runtimeError UnsupportedChannelExecution (Some pos) $"Built-in '{name}' does not support implicit channel execution."

and private reportErrorToChannel resolvedChannel errorInfo state pos =
    writeOutputToChannel resolvedChannel (formatReportedError errorInfo) state pos
    |> Result.bind (fun finalState -> advanceScreenLine resolvedChannel finalState pos)

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
    | WriteStringChar(_, _, _, pos)
    | DynamicWriteStringChar(_, _, _, pos) ->
        runtimeError InvalidReferenceActual (Some pos) "String character targets cannot be used as by-reference storage locations."

and private writeTarget state target value =
    match target with
    | WriteStringChar(symbolId, index, _, pos) ->
        requireCell state symbolId pos
        |> Result.bind (fun cell ->
            evalExpr state index
            |> Result.map (fun (indexValue, nextState) ->
                cell.Value <- writeStringCharValue cell.Value indexValue value
                nextState))
    | DynamicWriteStringChar(name, index, _, pos) ->
        requireDynamicCell state name pos
        |> Result.bind (fun cell ->
            evalExpr state index
            |> Result.map (fun (indexValue, nextState) ->
                cell.Value <- writeStringCharValue cell.Value indexValue value
                nextState))
    | _ ->
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
              ReturnType = routine.ReturnType
              ScreenChannel = stateAfterArgs.CurrentScreenChannel }

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
            | RetryError _, _
            | ContinueError _, _
            | ReplaceProgram _, _
            | RestartProgram _, _
            | ExitLoop _, _
            | NextLoop _, _ -> runtimeError EscapedLoopControl (Some pos) $"Loop control escaped from routine '{routine.Name}'."))

and private executeBuiltInCall state kind channel args targets pos =
    match kind with
    | Print ->
        withEffectiveChannel state channel (defaultScreenChannelId state) pos (fun resolvedChannel stateAfterChannel ->
            evalExprList stateAfterChannel args
            |> Result.bind (fun (values, nextState) ->
                let suppressNewline, printableValues =
                    match List.rev values with
                    | StringValue sentinel :: rest when sentinel = printContinueSentinel -> true, List.rev rest
                    | _ -> false, values
                let capturedLine =
                    printableValues
                    |> List.map toRuntimeBuiltInValue
                    |> BuiltInStatements.formatPrintLine
                let renderedLine =
                    if isScreenLikeOutputChannel resolvedChannel nextState then
                        formatScreenPrintValues printableValues
                    else
                        capturedLine
                writeRenderedOutputToChannel resolvedChannel renderedLine capturedLine nextState pos
                |> Result.bind (fun finalState ->
                    if suppressNewline then Result.Ok finalState
                    else advanceScreenLine resolvedChannel finalState pos)
                |> Result.map (fun finalState -> Continue, finalState)))
    | BuiltInKind.Input ->
        withEffectiveChannel state channel (defaultScreenChannelId state) pos (fun resolvedChannel stateAfterChannel ->
            let promptStateResult =
                if List.isEmpty args then
                    Result.Ok stateAfterChannel
                else
                    let promptText =
                        let promptFormatter =
                            if isScreenLikeOutputChannel resolvedChannel stateAfterChannel then
                                formatScreenOutputValue
                            else
                                formatOutputValue

                        args
                        |> List.map (fun expr ->
                            match evalExpr stateAfterChannel expr with
                            | Result.Ok(value, _) -> promptFormatter value
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
                            | WriteArrayElem(_, _, HIR.HirType.String, _)
                            | DynamicWriteVar(_, HIR.HirType.String, _)
                            | DynamicWriteArrayElem(_, _, HIR.HirType.String, _) -> BuiltInInputType.InputString
                            | WriteVar(_, HIR.HirType.Float, _)
                            | WriteArrayElem(_, _, HIR.HirType.Float, _)
                            | DynamicWriteVar(_, HIR.HirType.Float, _)
                            | DynamicWriteArrayElem(_, _, HIR.HirType.Float, _) -> BuiltInInputType.InputFloat
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
            withEffectiveChannel state channel (defaultScreenChannelId state) pos (fun resolvedChannel stateAfterChannel ->
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

        let executeGraphicsStatement (body: ChannelId option -> RuntimeState -> Result<ControlFlow * RuntimeState, RuntimeError>) =
            withEffectiveChannel state channel (defaultScreenChannelId state) pos (fun resolvedChannel stateAfterChannel ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () -> body resolvedChannel stateAfterChannel))

        let executeOptionalGraphicsStatement (body: ChannelId option -> RuntimeState -> Result<ControlFlow * RuntimeState, RuntimeError>) =
            withEffectiveChannel state channel (defaultScreenChannelId state) pos (fun resolvedChannel stateAfterChannel ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () -> body resolvedChannel stateAfterChannel))

        let trySplitRange expr =
            match expr with
            | Binary(SliceRange, lhs, rhs, _, _) -> Some(lhs, rhs)
            | _ -> None

        let parseCircleGroups values =
            let rec loop remaining =
                match remaining with
                | [] -> Some []
                | [ x; y; radius ] ->
                    Some [ Choice1Of2(x, y, radius) ]
                | [ x; y; radius; ratio; ecc ] ->
                    Some [ Choice2Of2(x, y, radius, ratio, ecc) ]
                | x :: y :: radius :: ratio :: ecc :: rest ->
                    match loop rest with
                    | Some groups -> Some(Choice2Of2(x, y, radius, ratio, ecc) :: groups)
                    | None ->
                        match loop (ratio :: ecc :: rest) with
                        | Some groups -> Some(Choice1Of2(x, y, radius) :: groups)
                        | None -> None
                | x :: y :: radius :: rest ->
                    match loop rest with
                    | Some groups -> Some(Choice1Of2(x, y, radius) :: groups)
                    | None -> None
                | _ -> None

            loop values

        let executeArcStatement relative =
            let invalidArcArity () =
                runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects arguments in groups of x1,y1 TO x2,y2,angle or TO x2,y2,angle."

            let validateArcAngle value =
                if abs value >= (2.0 * Math.PI) then
                    runtimeError BuiltInUnsupportedArguments (Some pos) $"Built-in statement '{name}' requires ABS(angle) to be less than 2*PI."
                else
                    Result.Ok()

            let executeArcGroup resolvedChannel currentState useCurrentPoint x1Expr y1Expr x2Expr y2Expr angleExpr =
                let startResult =
                    match x1Expr, y1Expr with
                    | Some sx, Some sy ->
                        evalExpr currentState sx
                        |> Result.bind (fun (x1Value, stateAfterX1) ->
                            evalExpr stateAfterX1 sy
                            |> Result.map (fun (y1Value, stateAfterY1) -> Some(normalizeNumber x1Value, normalizeNumber y1Value), stateAfterY1))
                    | _ -> Result.Ok(None, currentState)

                startResult
                |> Result.bind (fun (startPoint, stateAfterStart) ->
                    evalExpr stateAfterStart x2Expr
                    |> Result.bind (fun (x2Value, stateAfterX2) ->
                        evalExpr stateAfterX2 y2Expr
                        |> Result.bind (fun (y2Value, stateAfterY2) ->
                            evalExpr stateAfterY2 angleExpr
                            |> Result.bind (fun (angleValue, stateAfterAngle) ->
                                let x2 = normalizeNumber x2Value
                                let y2 = normalizeNumber y2Value
                                let angle = normalizeNumber angleValue

                                validateArcAngle angle
                                |> Result.bind (fun () ->
                                    let startX, startY =
                                        match startPoint with
                                        | Some(x1, y1) -> x1, y1
                                        | None when useCurrentPoint -> Double.NaN, Double.NaN
                                        | None -> Double.NaN, Double.NaN

                                    executeGraphicsOp resolvedChannel stateAfterAngle pos (fun graphics ->
                                        if relative then
                                            graphics.ArcRelative(startX, startY, x2, y2, angle)
                                        else
                                            graphics.Arc(startX, startY, x2, y2, angle)))))))

            let rec executeArcGroups resolvedChannel currentState pendingEndX remaining =
                match pendingEndX, remaining with
                | None, [] -> Result.Ok currentState
                | Some _, [] -> invalidArcArity ()
                | Some endXExpr, y2Expr :: angleOrContinuation :: rest ->
                    let angleExpr, nextEndX =
                        match trySplitRange angleOrContinuation with
                        | Some(angleExpr, chainedEndXExpr) -> angleExpr, Some chainedEndXExpr
                        | None -> angleOrContinuation, None

                    executeArcGroup resolvedChannel currentState true None None endXExpr y2Expr angleExpr
                    |> Result.bind (fun stateAfterGroup -> executeArcGroups resolvedChannel stateAfterGroup nextEndX rest)
                | None, x1Expr :: Binary(SliceRange, y1Expr, x2Expr, _, _) :: y2Expr :: angleOrContinuation :: rest ->
                    let angleExpr, nextEndX =
                        match trySplitRange angleOrContinuation with
                        | Some(angleExpr, chainedEndXExpr) -> angleExpr, Some chainedEndXExpr
                        | None -> angleOrContinuation, None

                    executeArcGroup resolvedChannel currentState false (Some x1Expr) (Some y1Expr) x2Expr y2Expr angleExpr
                    |> Result.bind (fun stateAfterGroup -> executeArcGroups resolvedChannel stateAfterGroup nextEndX rest)
                | None, x1Expr :: y1Expr :: x2Expr :: y2Expr :: angleExpr :: rest ->
                    executeArcGroup resolvedChannel currentState false (Some x1Expr) (Some y1Expr) x2Expr y2Expr angleExpr
                    |> Result.bind (fun stateAfterGroup -> executeArcGroups resolvedChannel stateAfterGroup None rest)
                | None, x2Expr :: y2Expr :: angleExpr :: rest ->
                    executeArcGroup resolvedChannel currentState true None None x2Expr y2Expr angleExpr
                    |> Result.bind (fun stateAfterGroup -> executeArcGroups resolvedChannel stateAfterGroup None rest)
                | _ -> invalidArcArity ()

            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    executeArcGroups resolvedChannel stateAfterChannel None args
                    |> Result.map (fun finalState -> Continue, finalState))

        let executeCircleStatement relative =
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 3 values.Length
                        |> Result.bind (fun () ->
                            match parseCircleGroups values with
                            | None ->
                                runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects arguments in groups of 3 or 5."
                            | Some groups ->
                                groups
                                |> List.fold
                                    (fun acc group ->
                                        acc
                                        |> Result.bind (fun currentState ->
                                            match group with
                                            | Choice1Of2(x, y, radius) ->
                                                if relative then
                                                    executeGraphicsOp resolvedChannel currentState pos (fun graphics -> graphics.CircleRelative(normalizeNumber x, normalizeNumber y, normalizeNumber radius))
                                                else
                                                    executeGraphicsOp resolvedChannel currentState pos (fun graphics -> graphics.Circle(normalizeNumber x, normalizeNumber y, normalizeNumber radius))
                                            | Choice2Of2(x, y, radius, ratio, ecc) ->
                                                if relative then
                                                    executeGraphicsOp resolvedChannel currentState pos (fun graphics -> graphics.EllipseRelative(normalizeNumber x, normalizeNumber y, normalizeNumber radius, normalizeNumber ratio, normalizeNumber ecc))
                                                else
                                                    executeGraphicsOp resolvedChannel currentState pos (fun graphics -> graphics.Ellipse(normalizeNumber x, normalizeNumber y, normalizeNumber radius, normalizeNumber ratio, normalizeNumber ecc))))
                                    (Result.Ok nextState)
                                |> Result.map (fun finalState -> Continue, finalState))))

        match normalized with
        | "STOP" -> Result.Ok(StopExecution, state)
        | ("POKE" | "POKE_W" | "POKE_L") ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    match values with
                    | [ addressValue; dataValue ] ->
                        pokeMemory nextState pos name (asInt addressValue) (asInt dataValue)
                    | _ ->
                        runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects two arguments."))
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
                        Result.Ok(Continue, { nextState with RandomSource = QlRandom(seed) :> Random })
                    | [ seedValue ] ->
                        Result.Ok(Continue, { nextState with RandomSource = QlRandom(asInt seedValue) :> Random })
                    | _ ->
                        runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero or one argument."))
        | "RUN" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    match values with
                    | [] -> Result.Ok(RestartProgram(nextState.Program, nextState.SourceProgram, None), resetErrorProcessing nextState)
                    | [ lineValue ] -> Result.Ok(RestartProgram(nextState.Program, nextState.SourceProgram, Some(asInt lineValue)), resetErrorProcessing nextState)
                    | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero or one argument."))
        | "NEW" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                requireArity 0 args.Length
                |> Result.map (fun () ->
                    let emptyProgram =
                        { state.Program with
                            Globals = []
                            Routines = []
                            DataEntries = []
                            RestorePoints = []
                            Main = [] }
                    ReplaceProgram(emptyProgram, Some(SyntaxAst.Program(pos, []))), resetErrorProcessing state))
        | "LOAD" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    match values with
                    | [ pathValue ] ->
                        match nextState.Options.LoadProgram(asString pathValue) with
                        | Result.Ok loaded ->
                            Result.Ok(ReplaceProgram(loaded.Hir, Some loaded.Ast), resetErrorProcessing nextState)
                        | Result.Error message ->
                            runtimeError UnsupportedChannelExecution (Some pos) message
                    | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects one argument."))
        | "LRUN" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    match values with
                    | [ pathValue ] ->
                        match nextState.Options.LoadProgram(asString pathValue) with
                        | Result.Ok loaded ->
                            Result.Ok(RestartProgram(loaded.Hir, Some loaded.Ast, None), resetErrorProcessing nextState)
                        | Result.Error message ->
                            runtimeError UnsupportedChannelExecution (Some pos) message
                    | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects one argument."))
        | "MERGE" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    match values, nextState.SourceProgram with
                    | [ pathValue ], Some currentAst ->
                        match nextState.Options.MergeProgram(currentAst, asString pathValue) with
                        | Result.Ok loaded ->
                            Result.Ok(ReplaceProgram(loaded.Hir, Some loaded.Ast), resetErrorProcessing nextState)
                        | Result.Error message ->
                            runtimeError UnsupportedChannelExecution (Some pos) message
                    | [ _ ], None ->
                        runtimeError BuiltInStatementNotImplemented (Some pos) "MERGE requires source program support."
                    | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects one argument."))
        | "MRUN" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    match values, nextState.SourceProgram with
                    | [ pathValue ], Some currentAst ->
                        match nextState.Options.MergeProgram(currentAst, asString pathValue) with
                        | Result.Ok loaded ->
                            Result.Ok(RestartProgram(loaded.Hir, Some loaded.Ast, None), resetErrorProcessing nextState)
                        | Result.Error message ->
                            runtimeError UnsupportedChannelExecution (Some pos) message
                    | [ _ ], None ->
                        runtimeError BuiltInStatementNotImplemented (Some pos) "MRUN requires source program support."
                    | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects one argument."))
        | "REPORT" ->
            withEffectiveChannel state channel (Some(ChannelId 0)) pos (fun resolvedChannel stateAfterChannel ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    let reportedError =
                        match values, nextState.LastError with
                        | [], Some errorInfo -> Some errorInfo
                        | [], None -> None
                        | [ errorNumber ], _ ->
                            let number = asInt errorNumber
                            let name, description =
                                qlErrorTable.Values
                                |> Seq.tryFind (fun (candidateNumber, _, _) -> candidateNumber = number)
                                |> Option.map (fun (_, candidateName, candidateDescription) -> candidateName, candidateDescription)
                                |> Option.defaultValue("ERR_NI", "Not implemented")
                            Some {
                                Number = number
                                Name = name
                                Description = description
                                LineNumber = None
                                RetryLineNumber = None
                                ContinueLineNumber = None
                                RuntimeError = { Code = BuiltInStatementNotImplemented; Message = description; Position = Some pos }
                            }
                        | _ -> None

                    match reportedError with
                    | None when values.Length <= 1 -> Result.Ok(Continue, nextState)
                    | None -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero or one argument."
                    | Some errorInfo when values.Length <= 1 ->
                        reportErrorToChannel resolvedChannel errorInfo nextState pos
                        |> Result.map (fun finalState -> Continue, finalState)
                    | Some _ ->
                        runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero or one argument."))
        | "CONTINUE" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                if not state.InErrorHandler then
                    runtimeError BuiltInStatementNotImplemented (Some pos) "CONTINUE is only valid inside WHEN ERROR."
                else
                    evalExprList state args
                    |> Result.bind (fun (values, nextState) ->
                        match values with
                        | [] -> Result.Ok(ContinueError None, nextState)
                        | [ lineValue ] -> Result.Ok(ContinueError(Some(asInt lineValue)), nextState)
                        | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero or one argument."))
        | "RETRY" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                if not state.InErrorHandler then
                    runtimeError BuiltInStatementNotImplemented (Some pos) "RETRY is only valid inside WHEN ERROR."
                else
                    evalExprList state args
                    |> Result.bind (fun (values, nextState) ->
                        match values with
                        | [] -> Result.Ok(RetryError None, nextState)
                        | [ lineValue ] -> Result.Ok(RetryError(Some(asInt lineValue)), nextState)
                        | _ -> runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero or one argument."))
        | "OPEN" ->
            resolveRequiredChannelId state channel (ChannelId 3) pos name
            |> Result.bind (fun (channelId, stateAfterChannel) ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 1 values.Length
                    |> Result.bind (fun () ->
                        match values with
                        | [ pathValue ] ->
                            match nextState.Options.Host.Channels.OpenAs(channelId, asString pathValue) with
                            | Result.Ok() -> Result.Ok(Continue, nextState)
                            | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
                        | _ -> Result.Ok(Continue, nextState))))
        | "OPEN_IN" ->
            resolveRequiredChannelId state channel (ChannelId 3) pos name
            |> Result.bind (fun (channelId, stateAfterChannel) ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 1 values.Length
                    |> Result.bind (fun () ->
                        match values with
                        | [ pathValue ] ->
                            match nextState.Options.Host.Files.OpenFileAs(channelId, asString pathValue, OpenForInput) with
                            | Result.Ok() -> Result.Ok(Continue, nextState)
                            | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
                        | _ -> Result.Ok(Continue, nextState))))
        | "OPEN_NEW" ->
            resolveRequiredChannelId state channel (ChannelId 3) pos name
            |> Result.bind (fun (channelId, stateAfterChannel) ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 1 values.Length
                    |> Result.bind (fun () ->
                        match values with
                        | [ pathValue ] ->
                            match nextState.Options.Host.Files.OpenFileAs(channelId, asString pathValue, OpenForOutput) with
                            | Result.Ok() -> Result.Ok(Continue, nextState)
                            | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
                        | _ -> Result.Ok(Continue, nextState))))
        | "APPEND" ->
            resolveRequiredChannelId state channel (ChannelId 3) pos name
            |> Result.bind (fun (channelId, stateAfterChannel) ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 1 values.Length
                    |> Result.bind (fun () ->
                        match values with
                        | [ pathValue ] ->
                            match nextState.Options.Host.Files.OpenFileAs(channelId, asString pathValue, OpenForAppend) with
                            | Result.Ok() -> Result.Ok(Continue, nextState)
                            | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
                        | _ -> Result.Ok(Continue, nextState))))
        | "CLOSE" ->
            resolveRequiredChannelId state channel (ChannelId 3) pos name
            |> Result.bind (fun (channelId, stateAfterChannel) ->
                requireArity 0 args.Length
                |> Result.bind (fun () ->
                    match stateAfterChannel.Options.Host.Channels.Close(channelId) with
                    | Result.Ok() -> Result.Ok(Continue, stateAfterChannel)
                    | Result.Error hostError -> runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)))
        | "DIR" ->
            withEffectiveChannel state channel (defaultScreenChannelId state) pos (fun resolvedChannel stateAfterChannel ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    if values.Length > 1 then
                        runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero or one argument."
                    else
                        let pathSpec =
                            match values with
                            | [] -> None
                            | [ pathValue ] -> Some(asString pathValue)
                            | _ -> None
                        match nextState.Options.Host.Files.ListDirectory(pathSpec) with
                        | Result.Error hostError ->
                            runtimeError UnsupportedChannelExecution (Some pos) (hostErrorText hostError)
                        | Result.Ok entries ->
                            ((Result.Ok nextState, entries)
                             ||> List.fold (fun acc entry ->
                                 acc
                                 |> Result.bind (fun currentState ->
                                     writeOutputToChannel resolvedChannel entry currentState pos
                                     |> Result.bind (fun stateAfterWrite -> advanceScreenLine resolvedChannel stateAfterWrite pos))))
                            |> Result.map (fun finalState -> Continue, finalState)))
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

                        nextState.Options.Host.Input.Flush()

                        let rec pause remaining =
                            if remaining > 0 then
                                if nextState.Options.Host.Input.KeyAvailable() then
                                    nextState.Options.Host.Input.ReadKey() |> ignore
                                else
                                    let slice = min 20 remaining
                                    nextState.Options.Sleeper slice
                                    pause (remaining - slice)

                        pause duration
                        Continue, nextState)))
        | "SLUG" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 1 values.Length
                    |> Result.map (fun () ->
                        let count =
                            values
                            |> List.head
                            |> asInt
                            |> max 0

                        for _ = 1 to count do
                            Thread.SpinWait(1000)

                        Continue, nextState)))
        | "FLUSH" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 0 values.Length
                    |> Result.map (fun () ->
                        nextState.Options.Host.Input.Flush()
                        Continue, nextState)))
        | "BEEP" ->
            unsupportedChannel ()
            |> Result.bind (fun _ ->
                evalExprList state args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 2 values.Length
                    |> Result.map (fun () ->
                        match values with
                        | [ pitch; duration ] ->
                            nextState.Options.Host.Sound.Beep(asInt pitch, asInt duration)
                            Continue, nextState
                        | _ -> Continue, nextState)))
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
        | "CHAR_USE"
        | "S_FONT" ->
            executeScreenOp 2 false (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [ font1; font2 ] ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetCharacterFonts(font1, font2)) (fun screen -> screen.SetCharacterFonts(font1, font2))
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
                    |> Result.bind (fun stateAfterPaper ->
                        if numericArgs.Length > 1 then
                            executeChannelScreenOp resolvedChannel stateAfterPaper pos (fun screenChannel -> screenChannel.SetStrip(numericArgs)) (fun screen -> screen.SetStrip(numericArgs))
                        else
                            Result.Ok stateAfterPaper)
                | _ -> Result.Ok nextState)
        | "STRIP" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [] -> Result.Ok nextState
                | _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetStrip(numericArgs)) (fun screen -> screen.SetStrip(numericArgs)))
        | "BORDER" ->
            withEffectiveChannel state channel (defaultScreenChannelId state) pos (fun resolvedChannel stateAfterChannel ->
                validateScreenChannel resolvedChannel stateAfterChannel pos
                |> Result.bind (fun () ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        let numericArgs = values |> List.map asInt
                        match numericArgs with
                        | [] ->
                            executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetBorder(0, None)) (fun screen -> screen.SetBorder(0, None))
                            |> Result.map (fun finalState -> Continue, finalState)
                        | size :: colorParts when colorParts.Length <= 3 ->
                            let color = encodeCompositeColorSpec colorParts
                            executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetBorder(size, color)) (fun screen -> screen.SetBorder(size, color))
                            |> Result.map (fun finalState -> Continue, finalState)
                        | _ ->
                            runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects zero to four arguments.")))
        | "CLEAR" ->
            executeGraphicsStatement (fun resolvedChannel stateAfterChannel ->
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
                | [] -> Result.Ok nextState
                | _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetRecolor(numericArgs)) (fun screen -> screen.SetRecolor(numericArgs)))
        | "PALETTE" ->
            executeScreenOp 1 true (fun resolvedChannel nextState numericArgs ->
                match numericArgs with
                | [] -> Result.Ok nextState
                | _ ->
                    executeChannelScreenOp resolvedChannel nextState pos (fun screenChannel -> screenChannel.SetPalette(numericArgs)) (fun screen -> screen.SetPalette(numericArgs)))
        | "PLOT" ->
            executeGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 2 values.Length
                    |> Result.bind (fun () ->
                        match values with
                        | [ x; y ] ->
                            executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Plot(normalizeNumber x, normalizeNumber y))
                            |> Result.map (fun finalState -> Continue, finalState)
                        | _ -> Result.Ok(Continue, nextState))))
        | "POINT" ->
            executeGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 2 values.Length
                    |> Result.bind (fun () ->
                        match values with
                        | [ x; y ] ->
                            executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Point(normalizeNumber x, normalizeNumber y))
                            |> Result.map (fun finalState -> Continue, finalState)
                        | _ -> Result.Ok(Continue, nextState))))
        | "POINT_R" ->
            executeGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 2 values.Length
                    |> Result.bind (fun () ->
                        match values with
                        | [ dx; dy ] ->
                            executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.PointRelative(normalizeNumber dx, normalizeNumber dy))
                            |> Result.map (fun finalState -> Continue, finalState)
                        | _ -> Result.Ok(Continue, nextState))))
        | "DRAW" ->
            executeGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                evalExprList stateAfterChannel args
                |> Result.bind (fun (values, nextState) ->
                    requireArity 2 values.Length
                    |> Result.bind (fun () ->
                        match values with
                        | [ x; y ] ->
                            executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Draw(normalizeNumber x, normalizeNumber y))
                            |> Result.map (fun finalState -> Continue, finalState)
                        | _ -> Result.Ok(Continue, nextState))))
        | "DLINE" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 4 values.Length
                        |> Result.bind (fun () ->
                            if values.Length % 2 <> 0 then
                                runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects an even number of coordinate arguments."
                            else
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.DLine(values |> List.map normalizeNumber))
                                |> Result.map (fun finalState -> Continue, finalState))))
        | "LINE" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
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
                                |> Result.map (fun finalState -> Continue, finalState))))
        | "LINE_R" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 2 values.Length
                        |> Result.bind (fun () ->
                            if values.Length % 2 <> 0 then
                                runtimeError BuiltInArityMismatch (Some pos) $"Built-in statement '{name}' expects an even number of coordinate arguments."
                            else
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.LineRelative(values |> List.map normalizeNumber))
                                |> Result.map (fun finalState -> Continue, finalState))))
        | "CIRCLE" ->
            executeCircleStatement false
        | "CIRCLE_R" ->
            executeCircleStatement true
        | "ELLIPSE" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 5 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y; radius; ratio; angle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Ellipse(normalizeNumber x, normalizeNumber y, normalizeNumber radius, normalizeNumber ratio, normalizeNumber angle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "ELLIPSE_R" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 5 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ dx; dy; radius; ratio; angle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.EllipseRelative(normalizeNumber dx, normalizeNumber dy, normalizeNumber radius, normalizeNumber ratio, normalizeNumber angle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "ARC" ->
            executeArcStatement false
        | "ARC_R" ->
            executeArcStatement true
        | "BLOCK" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 5 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ width; height; x; y; color ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Block(normalizeNumber width, normalizeNumber height, normalizeNumber x, normalizeNumber y, asInt color))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "FILL" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ fillValue ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetFill(asInt fillValue))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "SCALE" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 3 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ x; y; z ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetScale(normalizeNumber x, normalizeNumber y, normalizeNumber z))
                                |> Result.map (fun finalState ->
                                    let scale = normalizeNumber x, normalizeNumber y, normalizeNumber z
                                    let updatedState =
                                        match resolvedChannel with
                                        | Some channelId ->
                                            { finalState with ChannelGraphicsScales = Map.add channelId scale finalState.ChannelGraphicsScales }
                                        | None ->
                                            { finalState with DefaultGraphicsScale = scale }
                                    Continue, updatedState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "OVER" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | mode :: _ ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetOver(asInt mode))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "UNDER" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | mode :: _ ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetUnder(asInt mode))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "FLASH" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireAtLeast 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | mode :: _ ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.SetFlash(asInt mode))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "PENDOWN" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    requireArity 0 args.Length
                    |> Result.bind (fun () ->
                        executeGraphicsOp resolvedChannel stateAfterChannel pos (fun graphics -> graphics.SetPenDown(true))
                        |> Result.map (fun finalState -> Continue, finalState)))
        | "PENUP" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    requireArity 0 args.Length
                    |> Result.bind (fun () ->
                        executeGraphicsOp resolvedChannel stateAfterChannel pos (fun graphics -> graphics.SetPenDown(false))
                        |> Result.map (fun finalState -> Continue, finalState)))
        | "TURN" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ angle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.Turn(normalizeNumber angle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
        | "TURNTO" ->
            executeOptionalGraphicsStatement (fun resolvedChannel stateAfterChannel ->
                    evalExprList stateAfterChannel args
                    |> Result.bind (fun (values, nextState) ->
                        requireArity 1 values.Length
                        |> Result.bind (fun () ->
                            match values with
                            | [ angle ] ->
                                executeGraphicsOp resolvedChannel nextState pos (fun graphics -> graphics.TurnTo(normalizeNumber angle))
                                |> Result.map (fun finalState -> Continue, finalState)
                            | _ -> Result.Ok(Continue, nextState))))
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
    let rec blockHasLineJump block =
        let rec stmtHasLineJump stmt =
            match stmt with
            | Goto _
            | OnGoto _ -> true
            | If(_, thenBlock, elseBlock, _) ->
                blockHasLineJump thenBlock
                || (elseBlock |> Option.exists blockHasLineJump)
            | For(_, _, _, _, _, body, _)
            | ForSequence(_, _, _, _, _, _, _, body, _)
            | Repeat(_, _, body, _)
            | WhenError(body, _) -> blockHasLineJump body
            | _ -> false

        block |> List.exists stmtHasLineJump

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
    | WhenError(body, _) ->
        Result.Ok(Continue, { state with ActiveErrorHandler = Some body })
    | If(condition, thenBlock, elseBlock, _) ->
        evalExpr state condition
        |> Result.bind (fun (value, nextState) ->
            let runBranch block =
                if blockHasLineJump block then
                    executeInlineBlock resolveLine nextState block 0 gosubStack (fun state _ -> Result.Ok(Continue, state)) |> fst
                else
                    executeBlock resolveLine nextState block 0 gosubStack (fun state _ -> Result.Ok(Continue, state))

            if truthy value then runBranch thenBlock
            else
                match elseBlock with
                | Some block -> runBranch block
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
    | ForSequence(loopId, symbolId, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, body, pos) ->
        let allExprs = prefixExprs @ [ startExpr; endExpr ] @ suffixExprs @ [ stepExpr ]
        evalExprList state allExprs
        |> Result.bind (fun (values, nextState) ->
            let prefixCount = prefixExprs.Length
            let suffixCount = suffixExprs.Length
            let expectedCount = prefixCount + suffixCount + 3

            if values.Length <> expectedCount then
                runtimeError InvalidForBounds (Some pos) "FOR sequence requires discrete values, range bounds, and step values."
            else
                let prefixValues = values |> List.take prefixCount
                let startValue = values |> List.item prefixCount
                let endValue = values |> List.item (prefixCount + 1)
                let suffixValues = values |> List.skip (prefixCount + 2) |> List.take suffixCount
                let stepValue = values |> List.last

                requireCell nextState symbolId pos
                |> Result.bind (fun cell ->
                    let executeCurrent currentState currentValue =
                        cell.Value <- IntValue currentValue
                        executeBlock resolveLine currentState body 0 gosubStack (fun state _ -> Result.Ok(Continue, state))

                    let rec iterateDiscrete remaining currentState =
                        match remaining with
                        | [] -> Result.Ok(Continue, currentState)
                        | value :: tail ->
                            executeCurrent currentState (asInt value)
                            |> Result.bind (fun (flow, bodyState) ->
                                match flow with
                                | Continue -> iterateDiscrete tail bodyState
                                | NextLoop flowId when flowId = loopId -> iterateDiscrete tail bodyState
                                | ExitLoop flowId when flowId = loopId -> Result.Ok(Continue, bodyState)
                                | _ -> Result.Ok(flow, bodyState))

                    let rec iterateRange currentState currentValue =
                        let step = asInt stepValue
                        let shouldContinue =
                            if step >= 0 then currentValue <= asInt endValue
                            else currentValue >= asInt endValue

                        if not shouldContinue then
                            Result.Ok(Continue, currentState)
                        else
                            executeCurrent currentState currentValue
                            |> Result.bind (fun (flow, bodyState) ->
                                match flow with
                                | Continue -> iterateRange bodyState (currentValue + step)
                                | NextLoop flowId when flowId = loopId -> iterateRange bodyState (currentValue + step)
                                | ExitLoop flowId when flowId = loopId -> Result.Ok(Continue, bodyState)
                                | _ -> Result.Ok(flow, bodyState))

                    iterateDiscrete prefixValues nextState
                    |> Result.bind (fun (flowAfterPrefix, prefixState) ->
                        match flowAfterPrefix with
                        | Continue ->
                            iterateRange prefixState (asInt startValue)
                            |> Result.bind (fun (flowAfterRange, rangeState) ->
                                match flowAfterRange with
                                | Continue -> iterateDiscrete suffixValues rangeState
                                | _ -> Result.Ok(flowAfterRange, rangeState))
                        | _ -> Result.Ok(flowAfterPrefix, prefixState))))
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

and private findNextLineNumber block pc inheritedNextLine =
    block
    |> List.skip (pc + 1)
    |> List.tryPick (function
        | LineNumber(value, _) -> Some value
        | _ -> None)
    |> Option.orElse inheritedNextLine

and private handleRuntimeError resolveLine block pc gosubStack state (error: RuntimeError) =
    match state.ActiveErrorHandler, state.InErrorHandler with
    | Some handler, false ->
        let errorInfo = errorInfoFromRuntimeError state error
        let handlerState =
            { state with
                LastError = Some errorInfo
                InErrorHandler = true }

        executeBlock resolveLine handlerState handler 0 gosubStack (fun currentState _ -> Result.Ok(Continue, currentState))
        |> Result.bind (fun (flow, finalState) ->
            let resumedState = { finalState with InErrorHandler = false }
            match flow with
            | RetryError lineOpt ->
                match lineOpt with
                | Some lineNumber -> Result.Ok(JumpToLine lineNumber, resumedState)
                | None ->
                    executeBlock resolveLine resumedState block pc gosubStack (fun currentState _ -> Result.Ok(Continue, currentState))
                    |> Result.map (fun (nestedFlow, nestedState) ->
                        match nestedFlow with
                        | Continue -> TransferredContinue, nestedState
                        | _ -> nestedFlow, nestedState)
            | ContinueError lineOpt ->
                match lineOpt with
                | Some lineNumber -> Result.Ok(JumpToLine lineNumber, resumedState)
                | None ->
                    executeBlock resolveLine resumedState block (pc + 1) gosubStack (fun currentState _ -> Result.Ok(Continue, currentState))
                    |> Result.map (fun (nestedFlow, nestedState) ->
                        match nestedFlow with
                        | Continue -> TransferredContinue, nestedState
                        | _ -> nestedFlow, nestedState)
            | StopExecution -> Result.Ok(StopExecution, resumedState)
            | JumpToLine _
            | GosubToLine _
            | ReturnFromRoutine _
            | BareReturn
            | ReplaceProgram _
            | RestartProgram _
            | ExitLoop _
            | NextLoop _
            | Continue
            | TransferredContinue ->
                runtimeError EscapedReturn error.Position "WHEN ERROR handler must exit with RETRY, CONTINUE, or STOP.")
    | _ ->
        let finalState = { state with LastError = Some(errorInfoFromRuntimeError state error) }
        Result.Error error

and private executeBlock resolveLine state block pc gosubStack kContinue =
    let mutable pending =
        { State = state
          Block = block
          Pc = pc
          GosubStack = gosubStack
          KContinue = kContinue }

    let mutable finished = false
    let mutable result = Result.Ok(Continue, state)

    while not finished do
        let inlineResult, context =
            executeInlineBlock resolveLine pending.State pending.Block pending.Pc pending.GosubStack pending.KContinue

        match inlineResult with
        | Result.Ok(JumpToLine lineNumber, nextState) ->
            match resolveLine lineNumber with
            | Some target ->
                let targetPending = target nextState context.GosubStack
                if LanguagePrimitives.PhysicalEquality targetPending.Block context.Block then
                    pending <- targetPending
                else
                    let wrappedContinue state gosubStack =
                        targetPending.KContinue state gosubStack
                        |> Result.map (fun (flow, state) ->
                            match flow with
                            | Continue -> TransferredContinue, state
                            | _ -> flow, state)

                    pending <- { targetPending with KContinue = wrappedContinue }
            | None ->
                result <- Result.Ok(JumpToLine lineNumber, nextState)
                finished <- true
        | Result.Ok(GosubToLine lineNumber, nextState) ->
            let returnCont returnState =
                { State = returnState
                  Block = context.Block
                  Pc = context.Pc + 1
                  GosubStack = context.GosubStack
                  KContinue = context.KContinue }

            match resolveLine lineNumber with
            | Some target ->
                pending <- target nextState (returnCont :: context.GosubStack)
            | None ->
                result <- Result.Ok(GosubToLine lineNumber, nextState)
                finished <- true
        | Result.Ok(BareReturn, nextState) ->
            match context.GosubStack with
            | returnCont :: _ ->
                pending <- returnCont nextState
            | [] ->
                result <- Result.Ok(BareReturn, nextState)
                finished <- true
        | completedResult ->
            result <- completedResult
            finished <- true

    result

and private executeInlineBlock resolveLine state block pc gosubStack kContinue =
    let mutable pending =
        { State = state
          Block = block
          Pc = pc
          GosubStack = gosubStack
          KContinue = kContinue }

    let mutable finished = false
    let mutable result = Result.Ok(Continue, state)

    while not finished do
        match executeBlockCore resolveLine pending.State pending.Block pending.Pc pending.GosubStack pending.KContinue with
        | Completed completedResult ->
            result <- completedResult
            finished <- true
        | Transfer next ->
            pending <- next

    result, pending

and private executeBlockCore resolveLine state block pc gosubStack kContinue =
    if pc >= block.Length then
        Completed(kContinue state gosubStack)
    else
        let currentStmt = block[pc]
        let currentLineNumber =
            match currentStmt with
            | LineNumber(value, _) -> Some value
            | _ -> state.CurrentLineNumber
        let stateAtStmt =
            { state with
                CurrentLineNumber = currentLineNumber
                NextLineNumber = findNextLineNumber block pc state.NextLineNumber }

        let continueWithFlow (flow, nextState) =
            match currentStmt, nextState.ExecutionGovernor with
            | LineNumber _, _
            | _, None -> ()
            | _, Some governor -> governor ()

            match flow with
            | Continue ->
                Transfer
                    { State = nextState
                      Block = block
                      Pc = pc + 1
                      GosubStack = gosubStack
                      KContinue = kContinue }
            | _ -> Completed(Result.Ok(flow, nextState))

        let stepResult =
            match executeStmt resolveLine gosubStack stateAtStmt currentStmt with
            | Result.Ok result -> Result.Ok result
            | Result.Error error -> handleRuntimeError resolveLine block pc gosubStack stateAtStmt error

        match stepResult with
        | Result.Ok result -> continueWithFlow result
        | Result.Error error -> Completed(Result.Error error)

and private buildLineTargets rootBlock =
    let rec build block kContinue =
        block
        |> List.mapi (fun index stmt ->
            let directTargets =
                match stmt with
                | LineNumber(value, _) ->
                    [ value,
                      (fun state gosubStack ->
                          { State = state
                            Block = block
                            Pc = index
                            GosubStack = gosubStack
                            KContinue = kContinue }) ]
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
                | WhenError(handlerBlock, _) -> build handlerBlock continueAfter
                | For(_, _, _, _, _, body, _)
                | ForSequence(_, _, _, _, _, _, _, body, _)
                | Repeat(_, _, body, _) ->
                    // A GOTO into a loop body must still complete through the loop's
                    // own control flow so falling off the end reiterates correctly.
                    build body (fun state _ -> Result.Ok(Continue, state))
                | _ -> []

            directTargets @ nestedTargets)
        |> List.concat

    and resolveLine lineNumber =
        targets.Value |> Map.tryFind lineNumber

    and targets : Lazy<Map<int, LineTarget>> =
        lazy (build rootBlock (fun state _ -> Result.Ok(Continue, state)) |> Map.ofList)

    resolveLine

let rec private executeTopLevelProgram state startLine =
    let resolveLine = buildLineTargets state.Program.Main

    let startResult =
        match startLine with
        | Some lineNumber ->
            match resolveLine lineNumber with
            | Some target ->
                let pending = target state []
                executeBlock resolveLine pending.State pending.Block pending.Pc pending.GosubStack pending.KContinue
            | None -> runtimeError MissingGotoTarget None $"RUN target line {lineNumber} does not exist."
        | None ->
            executeBlock resolveLine state state.Program.Main 0 [] (fun currentState _ -> Result.Ok(Continue, currentState))

    startResult
    |> Result.bind (fun (flow, finalState) ->
        match flow with
        | Continue
        | TransferredContinue
        | StopExecution -> Result.Ok { Output = finalState.Output }
        | ReplaceProgram(program, sourceProgram) ->
            let _ = reinitializeProgramState finalState program sourceProgram
            Result.Ok { Output = finalState.Output }
        | RestartProgram(program, sourceProgram, startLineNumber) ->
            let restartedState = reinitializeProgramState finalState program sourceProgram
            executeTopLevelProgram restartedState startLineNumber
        | ReturnFromRoutine _
        | BareReturn -> runtimeError EscapedReturn None "Return escaped the top-level program."
        | RetryError _
        | ContinueError _ -> runtimeError EscapedLoopControl None "Error control escaped the top-level program."
        | JumpToLine lineNumber -> runtimeError MissingGotoTarget None $"GOTO target line {lineNumber} does not exist."
        | GosubToLine lineNumber -> runtimeError MissingGosubTarget None $"GOSUB target line {lineNumber} does not exist."
        | ExitLoop _
        | NextLoop _ -> runtimeError EscapedLoopControl None "Loop control escaped the top-level program.")

let interpretProgramWithOptions options (program: HirProgram) =
    let executionGovernor =
        options.ExecutionThrottle
        |> Option.map (fun settings -> createExecutionGovernor settings options.Sleeper)

    let initialState =
        { Program = program
          SourceProgram = options.InitialSourceProgram
          Globals = allocateCells program.Globals
          Frames = []
          DataPointer = 0
          Output = []
          CurrentScreenChannel = ChannelId 1
          DefaultGraphicsScale = (100.0, 0.0, 0.0)
          ChannelGraphicsScales = Map.empty
          CurrentLineNumber = None
          NextLineNumber = None
          ActiveErrorHandler = None
          LastError = None
          InErrorHandler = false
          RandomSource = options.Random
          ExecutionGovernor = executionGovernor
          Options = options }
        |> resetErrorProcessing

    executeTopLevelProgram initialState None

let interpretProgram program =
    interpretProgramWithOptions defaultRuntimeOptions program
