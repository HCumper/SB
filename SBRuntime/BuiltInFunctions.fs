namespace SBRuntime

open System
open System.Globalization

// Implementations of expression built-ins such as ABS, VAL, LEFT$, RIGHT$, DATE, and RND.

type BuiltInFunctionError =
    | ArityMismatch of string
    | UnsupportedArguments of string
    | NotImplemented of string

module BuiltInFunctions =
    let private secondsSinceUnixEpoch (clock: unit -> DateTime) =
        int64 (clock().Subtract(DateTime.UnixEpoch).TotalSeconds)

    let private fromUnixSeconds (seconds: int64) =
        DateTime.UnixEpoch.AddSeconds(float seconds)

    let evaluate
        (name: string)
        (isFloat: bool)
        (clock: unit -> DateTime)
        (random: unit -> Random)
        (getEnvironmentVariable: string -> string option)
        (readKey: unit -> KeyInfo option)
        (keyRowState: int -> int)
        (isEndOfFile: int -> bool)
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
        let clampStartIndex start length =
            max 0 (min length (start - 1))

        match normalized with
        | "ABS" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (abs (RuntimeValues.normalizeNumber value)))
        | "ACOS" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Acos(RuntimeValues.normalizeNumber value)))
        | "ACOT" ->
            oneArg (fun value ->
                let number = RuntimeValues.normalizeNumber value
                let angle =
                    if number = 0.0 then Math.PI / 2.0
                    else Math.Atan(1.0 / number)
                RuntimeValues.makeNumeric isFloat angle)
        | "ADATE" ->
            oneArg (fun value ->
                let currentSeconds = secondsSinceUnixEpoch clock
                let adjustedSeconds = currentSeconds + int64 (RuntimeValues.toInt value)
                RuntimeValues.makeNumeric isFloat (float adjustedSeconds))
        | "ASC"
        | "CODE" ->
            oneArg (fun value ->
                let text = RuntimeValues.toText value
                if String.IsNullOrEmpty text then RuntimeValues.ofInt 0
                else RuntimeValues.ofInt (int text[0]))
        | "ASIN" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Asin(RuntimeValues.normalizeNumber value)))
        | "ATAN" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Atan(RuntimeValues.normalizeNumber value)))
        | "CHR$" ->
            oneArg (fun value ->
                let codePoint = RuntimeValues.toInt value &&& 255
                Text(string (char codePoint)))
        | "COS" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Cos(RuntimeValues.normalizeNumber value)))
        | "COT" ->
            oneArg (fun value ->
                let tangent = Math.Tan(RuntimeValues.normalizeNumber value)
                RuntimeValues.makeNumeric isFloat (1.0 / tangent))
        | "DATE$" ->
            match args with
            | [] ->
                let text = clock().ToString("yyyy MMM dd HH:mm:ss", CultureInfo.InvariantCulture)
                Result.Ok(Text text)
            | [ value ] ->
                let text =
                    (fromUnixSeconds (int64 (RuntimeValues.toInt value)))
                        .ToString("yyyy MMM dd HH:mm:ss", CultureInfo.InvariantCulture)
                Result.Ok(Text text)
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects zero or one argument.")
        | "DAY$" ->
            match args with
            | [] ->
                let text = clock().ToString("dddd", CultureInfo.InvariantCulture)
                Result.Ok(Text text)
            | [ value ] ->
                let text =
                    (fromUnixSeconds (int64 (RuntimeValues.toInt value)))
                        .ToString("dddd", CultureInfo.InvariantCulture)
                Result.Ok(Text text)
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects zero or one argument.")
        | "DEG" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (RuntimeValues.normalizeNumber value * 180.0 / Math.PI))
        | "EOF" ->
            oneArg (fun value ->
                if isEndOfFile (RuntimeValues.toInt value) then RuntimeValues.ofInt 1
                else RuntimeValues.ofInt 0)
        | "EXP" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Exp(RuntimeValues.normalizeNumber value)))
        | "FILL$" ->
            twoArgs (fun source count ->
                let text = RuntimeValues.toText source
                let repeats = max 0 (RuntimeValues.toInt count)
                Text(String.replicate repeats text))
        | "GETENV$" ->
            oneArg (fun value ->
                let name = RuntimeValues.toText value
                Text(getEnvironmentVariable name |> Option.defaultValue String.Empty))
        | "INKEY" ->
            oneArg (fun _ ->
                let value =
                    match readKey() with
                    | Some { KeyCode = keyCode } -> keyCode
                    | None -> 0
                RuntimeValues.ofInt value)
        | "INT" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Floor(RuntimeValues.normalizeNumber value)))
        | "KEYROW" ->
            oneArg (fun value ->
                let row = RuntimeValues.toInt value
                RuntimeValues.ofInt (keyRowState row))
        | "LEN" ->
            oneArg (fun value ->
                RuntimeValues.ofInt (RuntimeValues.toText value).Length)
        | "MID$" ->
            match args with
            | [ source; start ] ->
                let text = RuntimeValues.toText source
                let startIndex = clampStartIndex (RuntimeValues.toInt start) text.Length
                Text(text.Substring(startIndex))
                |> Result.Ok
            | [ source; start; length ] ->
                let text = RuntimeValues.toText source
                let startIndex = clampStartIndex (RuntimeValues.toInt start) text.Length
                let count = max 0 (min (RuntimeValues.toInt length) (text.Length - startIndex))
                Text(text.Substring(startIndex, count))
                |> Result.Ok
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects two or three arguments.")
        | "LN" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Log(RuntimeValues.normalizeNumber value)))
        | "LOG"
        | "LOG10" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Log10(RuntimeValues.normalizeNumber value)))
        | "PI" ->
            match args with
            | [] -> Result.Ok(RuntimeValues.makeNumeric isFloat Math.PI)
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects no arguments.")
        | "RAD" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (RuntimeValues.normalizeNumber value * Math.PI / 180.0))
        | "ROUND" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Round(RuntimeValues.normalizeNumber value)))
        | "SGN" ->
            oneArg (fun value ->
                let number = RuntimeValues.normalizeNumber value
                let sign =
                    if number > 0.0 then 1.0
                    elif number < 0.0 then -1.0
                    else 0.0
                RuntimeValues.makeNumeric isFloat sign)
        | "SIN" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Sin(RuntimeValues.normalizeNumber value)))
        | "STR$" -> oneArg (fun value -> Text(RuntimeValues.toText value))
        | "SQRT" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Sqrt(RuntimeValues.normalizeNumber value)))
        | "TAN" -> oneArg (fun value -> RuntimeValues.makeNumeric isFloat (Math.Tan(RuntimeValues.normalizeNumber value)))
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
                let seconds = int (secondsSinceUnixEpoch clock)
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
            | [ Numeric lower; Numeric upper ] when allowRangePair ->
                let lowerBound = RuntimeValues.toInt (Numeric lower)
                let upperBound = RuntimeValues.toInt (Numeric upper)
                let minimum = min lowerBound upperBound
                let maximum = max lowerBound upperBound
                Result.Ok(RuntimeValues.makeNumeric isFloat (double (random().Next(minimum, maximum + 1))))
            | [ upper ] ->
                let maxValue = max 1 (RuntimeValues.toInt upper)
                Result.Ok(RuntimeValues.makeNumeric isFloat (double (random().Next(1, maxValue + 1))))
            | _ -> Result.Error(UnsupportedArguments $"Built-in function '{name}' received unsupported arguments.")
        | "REPL$" ->
            match args with
            | [ source; replacement; start ] ->
                let text = RuntimeValues.toText source
                let repl = RuntimeValues.toText replacement
                let startIndex = clampStartIndex (RuntimeValues.toInt start) text.Length
                let replaceCount = min repl.Length (text.Length - startIndex)
                let prefix = text.Substring(0, startIndex)
                let suffix = text.Substring(startIndex + replaceCount)
                Result.Ok(Text(prefix + repl + suffix))
            | [ source; replacement; start; length ] ->
                let text = RuntimeValues.toText source
                let repl = RuntimeValues.toText replacement
                let startIndex = clampStartIndex (RuntimeValues.toInt start) text.Length
                let replaceCount = max 0 (min (RuntimeValues.toInt length) (text.Length - startIndex))
                let prefix = text.Substring(0, startIndex)
                let suffix = text.Substring(startIndex + replaceCount)
                Result.Ok(Text(prefix + repl + suffix))
            | _ -> Result.Error(ArityMismatch $"Built-in function '{name}' expects three or four arguments.")
        | "TIME" ->
            oneArg (fun value ->
                let current = clock().TimeOfDay.TotalSeconds
                RuntimeValues.makeNumeric isFloat (current + float (RuntimeValues.toInt value)))
        | _ -> Result.Error(NotImplemented $"Built-in function '{name}' is not implemented.")
