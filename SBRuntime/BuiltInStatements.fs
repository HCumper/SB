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
    let private printPlingSentinel = "\uE000PRINT_PLING\uE000"
    let private printToSentinel = "\uE000PRINT_TO\uE000"
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

        let rec loop remaining =
            match remaining with
            | [] -> ()
            | Text sentinel :: rest when sentinel = printContinueSentinel ->
                suppressSeparator <- true
                loop rest
            | Text sentinel :: rest when sentinel = printCommaSentinel ->
                advanceToNextPrintZone ()
                loop rest
            | Text sentinel :: rest when sentinel = printPlingSentinel ->
                // '!': one space between items, none at the start of the line.
                if builder.Length > 0 && builder[builder.Length - 1] <> ' ' then builder.Append(' ') |> ignore
                suppressSeparator <- true
                loop rest
            | Text sentinel :: target :: rest when sentinel = printToSentinel ->
                // 'TO n': pad to column n (0-based).
                let column = RuntimeValues.toInt target
                if column > builder.Length then builder.Append(String(' ', column - builder.Length)) |> ignore
                suppressSeparator <- true
                loop rest
            | value :: rest ->
                appendText (RuntimeValues.formatOutputValue value)
                loop rest

        loop values
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
