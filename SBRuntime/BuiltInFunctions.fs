namespace SBRuntime

open System
open System.Globalization

// Implementations of expression built-ins such as ABS, VAL, LEFT$, RIGHT$, DATE, and RND.

type BuiltInFunctionError =
    | ArityMismatch of string
    | UnsupportedArguments of string
    | NotImplemented of string

module BuiltInFunctions =
    let evaluate
        (name: string)
        (isFloat: bool)
        (clock: unit -> DateTime)
        (random: unit -> Random)
        (readKey: unit -> KeyInfo option)
        (allowRangePair: bool)
        (args: SBValue list)
        =
        let normalized = name.Trim().ToUpperInvariant()
        let oneArg (f: SBValue -> SBValue) =
            match args with
            | [ value ] -> Result.Ok(f value)
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects one argument.")
        let twoArgs (f: SBValue -> SBValue -> SBValue) =
            match args with
            | [ left; right ] -> Result.Ok(f left right)
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects two arguments.")

        match normalized with
        | "ABS" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (abs (RuntimeValues.normalizeNumber value)))
        | "INT" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Floor(RuntimeValues.normalizeNumber value)))
        | "ROUND" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Round(RuntimeValues.normalizeNumber value)))
        | "STR$" -> oneArg (fun value -> Text(RuntimeValues.toText value))
        | "VAL" ->
            oneArg (fun value ->
                match Double.TryParse(RuntimeValues.toText value, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
                | true, number -> RuntimeValues.ofFloat number
                | false, _ -> RuntimeValues.ofInt 0)
        | "LEFT$" ->
            twoArgs (fun source length ->
                let text = RuntimeValues.toText source
                let count = max 0 (min text.Length (RuntimeValues.toInt length))
                Text(text.Substring(0, count)))
        | "RIGHT$" ->
            twoArgs (fun source length ->
                let text = RuntimeValues.toText source
                let count = max 0 (min text.Length (RuntimeValues.toInt length))
                Text(text.Substring(text.Length - count, count)))
        | "DATE" ->
            match args with
            | [] ->
                let seconds = int (clock().Subtract(DateTime.UnixEpoch).TotalSeconds)
                Result.Ok(RuntimeValues.ofInt seconds)
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects no arguments.")
        | "INKEY$" ->
            match args with
            | []
            | [ _ ]
            | [ _; _ ] ->
                let text =
                    match readKey() with
                    | Some { Character = Some ch } -> string ch
                    | Some { Character = None; KeyCode = keyCode } when keyCode >= 0 && keyCode <= 255 -> string (char keyCode)
                    | Some _ -> ""
                    | None -> ""
                Result.Ok(Text text)
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects zero, one, or two arguments.")
        | "RND" ->
            match args with
            | [] -> Result.Ok(RuntimeValues.ofFloat (random().NextDouble()))
            | [ Numeric(IntNumber lower); Numeric(IntNumber upper) ] when allowRangePair ->
                Result.Ok(RuntimeValues.makeNumeric isFloat (double (random().Next(lower, upper + 1))))
            | [ Numeric(FloatNumber lower); Numeric(FloatNumber upper) ] when allowRangePair ->
                Result.Ok(RuntimeValues.makeNumeric isFloat (double (random().Next(int lower, int upper + 1))))
            | [ upper ] ->
                let maxValue = max 1 (RuntimeValues.toInt upper)
                Result.Ok(RuntimeValues.makeNumeric isFloat (double (random().Next(1, maxValue + 1))))
            | _ -> Result.Error(UnsupportedArguments $"Built-in function '{name}' received unsupported arguments.")
        | _ -> Result.Error(NotImplemented $"Built-in function '{name}' is not implemented.")
