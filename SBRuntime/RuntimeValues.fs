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

    /// SuperBASIC '==' on floats, as Turbo's mosteq_float (lib537): equal to about 1 part in
    /// 10^7, that is within 127 units of a QL float's 31-bit mantissa at the larger operand's
    /// exponent. Integers compare exactly (mosteq_int is eq_int).
    let almostEqualFloats (left: double) (right: double) =
        left = right
        || abs (left - right) <= Math.ScaleB(127.0, Math.ILogB(max (abs left) (abs right)) - 30)

    /// SuperBASIC '==' on strings: QDOS comparison type 3, which ignores case. Type 3 (and the
    /// type 2 used by '=') also compares embedded numbers by value; SB compares them as text.
    let almostEqualStrings (left: string) (right: string) =
        String.Equals(left, right, StringComparison.OrdinalIgnoreCase)

    let formatOutputValue = toText

    let makeNumeric isFloat number =
        if isFloat then
            ofFloat number
        else
            ofInt (int (Math.Round(number)))
