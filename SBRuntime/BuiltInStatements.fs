namespace SBRuntime

open System
open System.Globalization

// Implementations of statement built-ins such as PRINT, INPUT, BEEP, and CLS.

type BuiltInInputType =
    | InputInt
    | InputFloat
    | InputString

module BuiltInStatements =
    let private printContinueSentinel = "\uE000PRINT_CONTINUE\uE000"
    let private printCommaSentinel = "\uE000PRINT_COMMA\uE000"
    let private printZoneWidth = 8

    let formatPrintLine values =
        let builder = Text.StringBuilder()
        let mutable suppressSeparator = false

        let appendText (text: string) =
            if builder.Length > 0 && not suppressSeparator then
                builder.Append(' ') |> ignore
            builder.Append(text) |> ignore
            suppressSeparator <- false

        let advanceToNextPrintZone () =
            let nextColumn =
                ((builder.Length / printZoneWidth) + 1) * printZoneWidth
            if nextColumn > builder.Length then
                builder.Append(String(' ', nextColumn - builder.Length)) |> ignore
            suppressSeparator <- true

        values
        |> List.iter (fun value ->
            match value with
            | Text sentinel when sentinel = printContinueSentinel -> suppressSeparator <- true
            | Text sentinel when sentinel = printCommaSentinel -> advanceToNextPrintZone ()
            | _ -> appendText (RuntimeValues.formatOutputValue value))

        builder.ToString()

    let splitInputLine (line: string option) =
        match line with
        | Some text ->
            text.Split([| ',' |], StringSplitOptions.None)
            |> Array.toList
            |> List.map (fun segment -> segment.Trim())
        | None -> []

    let parseInputValue expectedType raw =
        match expectedType with
        | InputString -> Text raw
        | InputFloat ->
            match Double.TryParse(raw, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
            | true, number -> RuntimeValues.ofFloat number
            | false, _ -> RuntimeValues.ofFloat 0.0
        | InputInt ->
            match Int32.TryParse(raw, NumberStyles.Integer, CultureInfo.InvariantCulture) with
            | true, number -> RuntimeValues.ofInt number
            | false, _ -> RuntimeValues.ofInt 0
