namespace SBRuntime

open System
open System.Globalization

// Shared runtime value representation and coercion helpers for SuperBASIC values.

type SBNumber =
    | IntNumber of int
    | FloatNumber of double

type SBValue =
    | Uninitialized
    | Numeric of SBNumber
    | Text of string

module RuntimeValues =
    let zero = Numeric(IntNumber 0)

    let ofInt value =
        Numeric(IntNumber value)

    let ofFloat value =
        Numeric(FloatNumber value)

    let ofString value =
        Text value

    let toFloat value =
        match value with
        | Numeric(IntNumber n) -> float n
        | Numeric(FloatNumber n) -> n
        | Text text ->
            match Double.TryParse(text, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
            | true, parsed -> parsed
            | false, _ -> 0.0
        | Uninitialized -> 0.0

    let toInt value =
        match value with
        | Numeric(IntNumber n) -> n
        | Numeric(FloatNumber n) -> int n
        | Text text ->
            match Int32.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
            | true, parsed -> parsed
            | false, _ -> 0
        | Uninitialized -> 0

    let toText value =
        match value with
        | Numeric(IntNumber n) -> string n
        | Numeric(FloatNumber n) -> n.ToString("G17", CultureInfo.InvariantCulture)
        | Text text -> text
        | Uninitialized -> String.Empty

    let normalizeNumber = toFloat

    let formatOutputValue = toText

    let makeNumeric isFloat number =
        if isFloat then
            ofFloat number
        else
            ofInt (int (Math.Round(number)))
