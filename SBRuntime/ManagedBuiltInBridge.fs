namespace SBRuntime

open System

[<AllowNullLiteral>]
type ManagedKeyInfo(keyCode: int, character: string) =
    member _.KeyCode = keyCode
    member _.Character = character

type ManagedBuiltInBridge =
    static member private EncodeCompositeColorSpec(values: int array) =
        match values |> Array.toList with
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

    static member private ToRuntimeValue(value: obj) =
        match value with
        | null -> Uninitialized
        | :? int as intValue -> RuntimeValues.ofInt intValue
        | :? double as doubleValue -> RuntimeValues.ofFloat doubleValue
        | :? single as floatValue -> RuntimeValues.ofFloat (double floatValue)
        | :? string as stringValue -> RuntimeValues.ofString stringValue
        | _ -> RuntimeValues.ofString (string value)

    static member private FromRuntimeValue(value: SBValue) : obj =
        match value with
        | Uninitialized -> null
        | Numeric(IntNumber intValue) -> box intValue
        | Numeric(FloatNumber floatValue) -> box floatValue
        | Text text -> box text

    static member private IsFloatGeneratedFunction(name: string) =
        match name.Trim().ToUpperInvariant() with
        | "ABS"
        | "ACOS"
        | "ACOT"
        | "ASIN"
        | "ATAN"
        | "COS"
        | "COT"
        | "DEG"
        | "EXP"
        | "LN"
        | "LOG"
        | "LOG10"
        | "PI"
        | "RAD"
        | "SIN"
        | "SQRT"
        | "VAL" -> true
        | _ -> false

    static member InvokeGeneratedFunction
        (
            name: string,
            args: obj array,
            clock: Func<DateTime>,
            random: Random,
            getEnvironmentVariable: Func<string, string>,
            tryReadKey: Func<ManagedKeyInfo>,
            sleepMilliseconds: Action<int>,
            isEndOfFile: Func<int, bool>
        ) : obj =
        let normalized = name.Trim().ToUpperInvariant()
        let runtimeArgs =
            match normalized, args with
            | "RND", [| :? (int * int) as range |] ->
                [ RuntimeValues.ofInt (fst range); RuntimeValues.ofInt (snd range) ]
            | "INSTR", [| haystack; needle |] ->
                [ ManagedBuiltInBridge.ToRuntimeValue needle; ManagedBuiltInBridge.ToRuntimeValue haystack ]
            | _ ->
                args
                |> Array.toList
                |> List.map ManagedBuiltInBridge.ToRuntimeValue

        let readKey () =
            let keyInfo = tryReadKey.Invoke()
            if isNull keyInfo then
                None
            else
                let character =
                    if String.IsNullOrEmpty keyInfo.Character then None
                    else Some keyInfo.Character[0]

                Some {
                    KeyCode = keyInfo.KeyCode
                    Character = character
                    Shift = false
                    Control = false
                }

        let getEnvironment name =
            let value = getEnvironmentVariable.Invoke(name)
            if isNull value then None else Some value

        let result =
            BuiltInFunctions.evaluate
                normalized
                (ManagedBuiltInBridge.IsFloatGeneratedFunction normalized)
                clock.Invoke
                (fun () -> random)
                getEnvironment
                readKey
                sleepMilliseconds.Invoke
                (fun _ -> 0)
                isEndOfFile.Invoke
                true
                runtimeArgs

        match result with
        | Result.Ok value -> ManagedBuiltInBridge.FromRuntimeValue value
        | Result.Error(ArityMismatch message)
        | Result.Error(UnsupportedArguments message)
        | Result.Error(NotImplemented message) ->
            raise (NotSupportedException message)

    static member FormatPrintLine(values: obj array) =
        values
        |> Array.toList
        |> List.map ManagedBuiltInBridge.ToRuntimeValue
        |> BuiltInStatements.formatPrintLine

    static member SplitInputLine(line: string) =
        BuiltInStatements.splitInputLine (Some line) |> List.toArray

    static member ParseInputValue(targetType: string, raw: string) : obj =
        let inputType =
            match targetType with
            | null
            | ""
            | "int" -> InputInt
            | "float" -> InputFloat
            | "string" -> InputString
            | _ -> InputInt

        BuiltInStatements.parseInputValue inputType raw
        |> ManagedBuiltInBridge.FromRuntimeValue

    static member private ToIntArray(values: obj array) =
        values
        |> Array.map (ManagedBuiltInBridge.ToRuntimeValue >> RuntimeValues.toInt)

    static member private ToDoubleArray(values: obj array) =
        values
        |> Array.map (ManagedBuiltInBridge.ToRuntimeValue >> RuntimeValues.toFloat)

    static member ExecuteGeneratedCls
        (
            channelId: int,
            clearGraphics: Action<int>,
            setScreenCursor: Action<int, int, int>,
            clearConsole: Action
        ) =
        clearGraphics.Invoke(channelId)
        setScreenCursor.Invoke(channelId, 0, 0)
        clearConsole.Invoke()

    static member ExecuteGeneratedWindow
        (
            channelId: int,
            args: obj array,
            setWindowOrigin: Action<int, int, int>,
            positionConsoleCursor: Action<int>
        ) =
        if args.Length >= 4 then
            let x = ManagedBuiltInBridge.ToRuntimeValue(args[2]) |> RuntimeValues.toInt
            let y = ManagedBuiltInBridge.ToRuntimeValue(args[3]) |> RuntimeValues.toInt
            setWindowOrigin.Invoke(channelId, max 0 x / 8, max 0 y / 10)
            positionConsoleCursor.Invoke(channelId)

    static member ExecuteGeneratedAt
        (
            channelId: int,
            args: obj array,
            setScreenCursor: Action<int, int, int>,
            positionConsoleCursor: Action<int>
        ) =
        if args.Length >= 2 then
            let row = ManagedBuiltInBridge.ToRuntimeValue(args[0]) |> RuntimeValues.toInt
            let column = ManagedBuiltInBridge.ToRuntimeValue(args[1]) |> RuntimeValues.toInt
            setScreenCursor.Invoke(channelId, column, row)
            positionConsoleCursor.Invoke(channelId)

    static member ExecuteGeneratedInk
        (
            channelId: int,
            args: obj array,
            setInk: Action<int, int array>
        ) =
        let colors = ManagedBuiltInBridge.ToIntArray args
        if colors.Length > 0 then
            setInk.Invoke(channelId, colors)

    static member ExecuteGeneratedPaper
        (
            channelId: int,
            args: obj array,
            setPaper: Action<int, int>,
            setStrip: Action<int, int array>
        ) =
        let colors = ManagedBuiltInBridge.ToIntArray args
        if colors.Length > 0 then
            setPaper.Invoke(channelId, colors[0])
            if colors.Length > 1 then
                setStrip.Invoke(channelId, colors)

    static member ExecuteGeneratedStrip
        (
            channelId: int,
            args: obj array,
            setStrip: Action<int, int array>
        ) =
        let colors = ManagedBuiltInBridge.ToIntArray args
        if colors.Length > 0 then
            setStrip.Invoke(channelId, colors)

    static member ExecuteGeneratedBorder
        (
            channelId: int,
            args: obj array,
            setBorder: Action<int, int, Nullable<int>>
        ) =
        let colors = ManagedBuiltInBridge.ToIntArray args
        match colors |> Array.toList with
        | [] -> setBorder.Invoke(channelId, 0, Nullable())
        | size :: colorParts ->
            let encoded = ManagedBuiltInBridge.EncodeCompositeColorSpec(colorParts |> List.toArray)
            match encoded with
            | Some color -> setBorder.Invoke(channelId, size, Nullable color)
            | None -> setBorder.Invoke(channelId, size, Nullable())

    static member ExecuteGeneratedClear
        (
            channelId: int,
            clearGraphics: Action<int>
        ) =
        clearGraphics.Invoke(channelId)

    static member ExecuteGeneratedCharacterSize
        (
            channelId: int,
            args: obj array,
            setCharacterSize: Action<int, int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length >= 2 then
            setCharacterSize.Invoke(channelId, values[0], values[1])

    static member ExecuteGeneratedCharacterFonts
        (
            channelId: int,
            args: obj array,
            setCharacterFonts: Action<int, int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length >= 2 then
            setCharacterFonts.Invoke(channelId, values[0], values[1])

    static member ExecuteGeneratedScroll
        (
            channelId: int,
            args: obj array,
            setScroll: Action<int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length > 0 then
            setScroll.Invoke(channelId, values[0])

    static member ExecuteGeneratedWidth
        (
            channelId: int,
            args: obj array,
            setWidth: Action<int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length > 0 then
            setWidth.Invoke(channelId, values[0])

    static member ExecuteGeneratedPan
        (
            channelId: int,
            args: obj array,
            setPan: Action<int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length > 0 then
            setPan.Invoke(channelId, values[0])

    static member ExecuteGeneratedRecolor
        (
            channelId: int,
            args: obj array,
            setRecolor: Action<int, int array>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length > 0 then
            setRecolor.Invoke(channelId, values)

    static member ExecuteGeneratedPalette
        (
            channelId: int,
            args: obj array,
            setPalette: Action<int, int array>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length > 0 then
            setPalette.Invoke(channelId, values)

    static member ExecuteGeneratedPlot
        (
            channelId: int,
            args: obj array,
            plotPoint: Action<int, int, int>,
            setCursor: Action<int, int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length >= 2 then
            plotPoint.Invoke(channelId, values[0], values[1])
            setCursor.Invoke(channelId, values[0], values[1])

    static member ExecuteGeneratedDrawRelative
        (
            channelId: int,
            args: obj array,
            getCursor: Func<int, ValueTuple<int, int>>,
            drawLine: Action<int, int, int, int, int>,
            setCursor: Action<int, int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length >= 2 then
            let struct (startX, startY) = getCursor.Invoke(channelId)
            let endX = startX + values[0]
            let endY = startY + values[1]
            drawLine.Invoke(channelId, startX, startY, endX, endY)
            setCursor.Invoke(channelId, endX, endY)

    static member ExecuteGeneratedLine
        (
            channelId: int,
            args: obj array,
            drawLine: Action<int, int, int, int, int>,
            setCursor: Action<int, int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length >= 4 then
            drawLine.Invoke(channelId, values[0], values[1], values[2], values[3])
            setCursor.Invoke(channelId, values[2], values[3])

    static member ExecuteGeneratedLineRelative
        (
            channelId: int,
            args: obj array,
            getCursor: Func<int, ValueTuple<int, int>>,
            drawLine: Action<int, int, int, int, int>,
            setCursor: Action<int, int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length >= 4 then
            let struct (originX, originY) = getCursor.Invoke(channelId)
            let startX = originX + values[0]
            let startY = originY + values[1]
            let endX = originX + values[2]
            let endY = originY + values[3]
            drawLine.Invoke(channelId, startX, startY, endX, endY)
            setCursor.Invoke(channelId, endX, endY)

    static member ExecuteGeneratedFill
        (
            channelId: int,
            args: obj array,
            setFill: Action<int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length > 0 then
            setFill.Invoke(channelId, values[0])

    static member ExecuteGeneratedScale
        (
            channelId: int,
            args: obj array,
            setScale: Action<int, double, double, double>
        ) =
        let values = ManagedBuiltInBridge.ToDoubleArray args
        if values.Length >= 3 then
            setScale.Invoke(channelId, values[0], values[1], values[2])

    static member ExecuteGeneratedGraphicsMode
        (
            channelId: int,
            args: obj array,
            setMode: Action<int, int>
        ) =
        let values = ManagedBuiltInBridge.ToIntArray args
        if values.Length > 0 then
            setMode.Invoke(channelId, values[0])

    static member ExecuteGeneratedPenDown(channelId: int, setPenDown: Action<int, bool>) =
        setPenDown.Invoke(channelId, true)

    static member ExecuteGeneratedPenUp(channelId: int, setPenDown: Action<int, bool>) =
        setPenDown.Invoke(channelId, false)

    static member ExecuteGeneratedTurn
        (
            channelId: int,
            args: obj array,
            getHeading: Func<int, double>,
            setHeading: Action<int, double>
        ) =
        let values = ManagedBuiltInBridge.ToDoubleArray args
        if values.Length > 0 then
            setHeading.Invoke(channelId, getHeading.Invoke(channelId) + values[0])

    static member ExecuteGeneratedTurnTo
        (
            channelId: int,
            args: obj array,
            setHeading: Action<int, double>
        ) =
        let values = ManagedBuiltInBridge.ToDoubleArray args
        if values.Length > 0 then
            setHeading.Invoke(channelId, values[0])

    static member ExecuteGeneratedOpen
        (
            name: string,
            channelId: int,
            args: obj array,
            closeChannel: Action<int>,
            openReader: Action<int, string>,
            openAppender: Action<int, string>,
            openWriter: Action<int, string>
        ) =
        if args.Length > 0 then
            let path = ManagedBuiltInBridge.ToRuntimeValue(args[0]) |> RuntimeValues.toText
            closeChannel.Invoke(channelId)
            match name.Trim().ToUpperInvariant() with
            | "OPEN_IN" -> openReader.Invoke(channelId, path)
            | "APPEND" -> openAppender.Invoke(channelId, path)
            | _ -> openWriter.Invoke(channelId, path)

    static member ExecuteGeneratedClose(channelId: int, closeChannel: Action<int>) =
        closeChannel.Invoke(channelId)

    static member ExecuteGeneratedDelete(args: obj array, deleteFile: Action<string>) =
        if args.Length > 0 then
            let path = ManagedBuiltInBridge.ToRuntimeValue(args[0]) |> RuntimeValues.toText
            deleteFile.Invoke(path)

    static member ExecuteGeneratedCopy(args: obj array, copyFile: Action<string, string>) =
        if args.Length >= 2 then
            let sourcePath = ManagedBuiltInBridge.ToRuntimeValue(args[0]) |> RuntimeValues.toText
            let targetPath = ManagedBuiltInBridge.ToRuntimeValue(args[1]) |> RuntimeValues.toText
            copyFile.Invoke(sourcePath, targetPath)

    static member ExecuteGeneratedRename(args: obj array, moveFile: Action<string, string>) =
        if args.Length >= 2 then
            let sourcePath = ManagedBuiltInBridge.ToRuntimeValue(args[0]) |> RuntimeValues.toText
            let targetPath = ManagedBuiltInBridge.ToRuntimeValue(args[1]) |> RuntimeValues.toText
            moveFile.Invoke(sourcePath, targetPath)

    static member ExecuteGeneratedTruncate(truncateChannel: Action) =
        truncateChannel.Invoke()

    static member ExecuteGeneratedSetPosition
        (
            args: obj array,
            setPosition: Action<int>,
            discardBufferedData: Action
        ) =
        if args.Length > 0 then
            let position = ManagedBuiltInBridge.ToRuntimeValue(args[0]) |> RuntimeValues.toInt
            setPosition.Invoke(position)
            discardBufferedData.Invoke()

    static member ExecuteGeneratedSetChannel
        (
            sourceChannelValue: obj,
            setChannelTarget: Action<obj>
        ) =
        setChannelTarget.Invoke(sourceChannelValue)
