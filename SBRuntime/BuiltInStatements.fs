namespace SBRuntime

open System
open System.Globalization

// Implementations of statement built-ins such as PRINT, INPUT, BEEP, and CLS.

type BuiltInInputType =
    | InputInt
    | InputFloat
    | InputString

module BuiltInStatements =
    let formatPrintLine values =
        values
        |> List.map RuntimeValues.formatOutputValue
        |> String.concat " "

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
