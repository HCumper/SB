module SSB

// SSB contains the compatibility preprocessor for Structured SuperBasic input.
//
// The current compiler still supports the historical SSB-to-numbered-SuperBasic
// preprocessing flow, so this module preserves that textual transformation
// rather than trying to absorb it into the parser itself.

// This is kludge to support the SSB preprocessor. Tim Swenson originally implemented it as a syntax driven preprocessor so that is preserved instead of integrating it into the main pipeline.

open System
open System.IO
open System.Collections.Generic
open System.Text

module Ssb =

    type SourceKind =
        | Ssb
        | SuperBasic

    type Config =
        { MaxLabels: int
          MaxNameLength: int
          InputExt: string
          OutputExt: string
          StartLine: int
          LineDelta: int
          WorkingDir1: string
          WorkingDir2: string
          Overwrite: bool
          Language: string
          IgnoreBlankLines: bool }

    type FatalSsbError(message: string) =
        inherit Exception(message)

    type Pass1State =
        { mutable LineNum: int
          Labels: Dictionary<string, int>
          Defines: HashSet<string> }

    // The legacy preprocessor is configured almost entirely through environment variables.
    let env name =
        match Environment.GetEnvironmentVariable(name) with
        | null -> ""
        | s -> s

    let parseBoolOverwrite (s: string) =
        match s.Trim().ToUpperInvariant() with
        | "Y" | "S" -> true
        | _ -> false

    let parseIgnoreBlank (s: string) =
        match s.Trim().ToUpperInvariant() with
        | "Y" -> true
        | _ -> false

    let tryParseInt (s: string) =
        let mutable n = 0
        if Int32.TryParse(s, &n) then Some n else None

    // Mirror the original tool's defaults, then selectively override them from the environment.
    let defaultConfig () =
        let d =
            { MaxLabels = 30
              MaxNameLength = 16
              InputExt = "_ssb"
              OutputExt = "_bas"
              StartLine = 1000
              LineDelta = 10
              WorkingDir1 = ""
              WorkingDir2 = ""
              Overwrite = false
              Language = "E"
              IgnoreBlankLines = true }

        let inputExt =
            match env "SSBINPUT" with
            | "" -> d.InputExt
            | x -> x

        let outputExt =
            match env "SSBOUTPUT" with
            | "" -> d.OutputExt
            | x -> x

        let startLine =
            match env "SSBSTART" |> tryParseInt with
            | Some n -> n
            | None -> d.StartLine

        let lineDelta =
            match env "SSBINC" |> tryParseInt with
            | Some n -> n
            | None -> d.LineDelta

        let work1 =
            match env "SSBWORK" with
            | "" -> d.WorkingDir1
            | x -> x

        let work2 =
            match env "SSBWORK2" with
            | "" -> d.WorkingDir2
            | x -> x

        let overwrite =
            match env "SSBOVER" with
            | "" -> d.Overwrite
            | x -> parseBoolOverwrite x

        let lang =
            match env "SSBLANG" with
            | "" -> d.Language
            | x -> x

        let ignoreBlanks =
            match env "SSBIGNORE_BLANKS" with
            | "" -> d.IgnoreBlankLines
            | x -> parseIgnoreBlank x

        let work2Final = if String.IsNullOrEmpty(work2) then work1 else work2

        { d with
            InputExt = inputExt
            OutputExt = outputExt
            StartLine = startLine
            LineDelta = lineDelta
            WorkingDir1 = work1
            WorkingDir2 = work2Final
            Overwrite = overwrite
            Language = lang
            IgnoreBlankLines = ignoreBlanks }

    let firstChar (s: string) =
        let mutable i = 0
        while i < s.Length && s.[i] = ' ' do
            i <- i + 1
        if i >= s.Length then 0 else i + 1

    let firstNonSpaceIs (ch: char) (s: string) =
        let fc = firstChar s
        fc > 0 && s.[fc - 1] = ch

    let upperAscii (s: string) =
        let chars = s.ToCharArray()
        for i = 0 to chars.Length - 1 do
            let c = chars.[i]
            if c >= 'a' && c <= 'z' then
                chars.[i] <- char (int c - 32)
        String(chars)

    let ltrimSpaces (s: string) =
        let mutable i = 0
        while i < s.Length && s.[i] = ' ' do
            i <- i + 1
        if i >= s.Length then "" else s.Substring(i)

    let rtrimSpaces (s: string) =
        let mutable i = s.Length - 1
        while i >= 0 && s.[i] = ' ' do
            i <- i - 1
        if i < 0 then "" else s.Substring(0, i + 1)

    let trimSpaces (s: string) =
        s |> ltrimSpaces |> rtrimSpaces

    let isNum (s: string) =
        not (String.IsNullOrEmpty(s)) && s |> Seq.forall Char.IsAsciiDigit

    let endsWithContinuation (s: string) =
        s.Length >= 2 && s.EndsWith(@"\+")

    let startsAtCol1 (prefix: string) (s: string) =
        s.StartsWith(prefix, StringComparison.Ordinal)

    let startsWithFirstNonSpace (prefix: string) (s: string) =
        let p = firstChar s
        if p = 0 then false
        else
            let idx = p - 1
            s.Length >= idx + prefix.Length &&
            s.Substring(idx, prefix.Length) = prefix

    let resolveIncludePath (cfg: Config) (directiveLine: string) =
        // Includes remain a textual preprocessor concern rather than a parser-level
        // construct.
        if directiveLine.Length < 10 then
            raise (FatalSsbError(sprintf "Malformed #INCLUDE: %s" directiveLine))
        cfg.WorkingDir1 + directiveLine.Substring(9)

    // Track the active delimiter: the other quote character is ordinary text.
    let private commentMarker (marker: string) (s: string) =
        let mutable quote = '\000'
        let mutable found = -1
        let mutable i = 0
        while i < s.Length && found < 0 do
            let ch = s[i]
            if quote <> '\000' then
                if ch = quote then quote <- '\000'
            elif ch = '\'' || ch = '"' then quote <- ch
            elif i + marker.Length <= s.Length && s.Substring(i, marker.Length) = marker then found <- i
            i <- i + 1
        found

    let stripTabs (s: string) = s.Replace('\t', ' ')

    let stripComment (s: string) =
        let position = commentMarker "##" s
        if position < 0 then s else s.Substring(0, position)

    let doRemark (s: string) =
        let position = commentMarker "**" s
        if position < 0 then s else s.Substring(0, position) + "REMark" + s.Substring(position + 2)

    let joinLineFromReader (reader: StreamReader) (initial: string) =
        let mutable outLine = initial
        while endsWithContinuation outLine do
            if reader.EndOfStream then
                raise (FatalSsbError("End of file found while looking for continuation line"))
            let nextLine = reader.ReadLine()
            let fc = firstChar nextLine
            let suffix =
                if fc = 0 then ""
                else nextLine.Substring(fc - 1)
            outLine <- rtrimSpaces (outLine.Substring(0, outLine.Length - 2)) + " " + suffix
        outLine

    let emitLine (writer: TextWriter) (lineNum: int) (text: string) =
        writer.WriteLine($"{lineNum} {text}")

    // Freeze active input once. Both expansion and numbering consume this stream,
    // so nested conditionals, includes and preserved blanks cannot skew labels.
    let private activeSource (cfg: Config) (inputFile: string) =
        let symbols = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let activeFiles = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let output = ResizeArray<SsbRecords.SourceLine>()
        let rec readFile depth input =
            let path = Path.GetFullPath(input)
            if depth >= 19 then raise (FatalSsbError($"{path}: Include nesting exceeds 19 input files"))
            if not (activeFiles.Add path) then raise (FatalSsbError($"{path}: Cyclic #INCLUDE"))
            if not (File.Exists path) then raise (FatalSsbError($"File not found: {path}"))
            let conditions = ResizeArray<bool>()
            use reader = new StringReader(QlCharacterSet.readSourceText path)
            let mutable number = 0
            let fail message = raise (FatalSsbError($"{path}:{number}: {message}"))
            let enabled () = conditions |> Seq.forall id
            let read () =
                number <- number + 1
                reader.ReadLine() |> stripTabs
            while reader.Peek() >= 0 do
                let firstNumber = number + 1
                let mutable line = read ()
                let initial = line.TrimStart()
                let directive = upperAscii initial
                if directive.StartsWith("#IFDEF ", StringComparison.Ordinal) then
                    if conditions.Count >= 32 then fail "Conditional nesting exceeds 32"
                    conditions.Add(symbols.Contains(initial.Substring(7).Trim()))
                elif directive = "#ENDIF" then
                    if conditions.Count = 0 then fail "Unmatched #ENDIF"
                    conditions.RemoveAt(conditions.Count - 1)
                elif enabled () then
                    if directive.StartsWith("#DEFINE ", StringComparison.Ordinal) then
                        let name = initial.Substring(8).Trim()
                        if name.Length = 0 || name.Length > cfg.MaxNameLength then fail "Invalid #DEFINE name"
                        symbols.Add name |> ignore
                    elif directive.StartsWith("#INCLUDE ", StringComparison.Ordinal) then
                        let name = initial.Substring(9).Trim().Trim('"')
                        let configured = cfg.WorkingDir1 + name
                        let relative = Path.Combine(Path.GetDirectoryName(path), name)
                        let includePath = if File.Exists configured then configured else relative
                        readFile (depth + 1) includePath
                    elif initial.StartsWith("##", StringComparison.Ordinal) || initial.StartsWith(".", StringComparison.Ordinal) then ()
                    else
                        while endsWithContinuation line do
                            if reader.Peek() < 0 then fail "End of file found while looking for continuation line"
                            let next = read () |> ltrimSpaces
                            line <- rtrimSpaces (line.Substring(0, line.Length - 2)) + " " + next
                        if line.Length > 1000 then fail "Source logical line exceeds 1000 bytes"
                        line <- line |> stripComment |> doRemark
                        if String.IsNullOrWhiteSpace line then
                            if not cfg.IgnoreBlankLines then output.Add { Text = ":"; File = path; Number = firstNumber }
                        else output.Add { Text = line; File = path; Number = firstNumber }
            if conditions.Count <> 0 then fail "End of file found before #ENDIF"
            activeFiles.Remove path |> ignore
        readFile 0 inputFile
        List.ofSeq output

    let resolveLabelReference (labels: Dictionary<string, int>) (line: string) =
        // Only @identifiers outside literals, REMarks and DATA are references.
        let builder = StringBuilder()
        let mutable i = 0
        let mutable quote = '\000'
        let mutable changed = false
        let mutable statementStart = true
        while i < line.Length do
            let ch = line[i]
            if quote <> '\000' then
                builder.Append(ch) |> ignore
                if ch = quote then quote <- '\000'
                i <- i + 1
            elif ch = '"' || ch = '\'' then
                quote <- ch
                statementStart <- false
                builder.Append(ch) |> ignore
                i <- i + 1
            elif statementStart && Char.IsLetter ch then
                let first = i
                while i < line.Length && Char.IsLetter line[i] do i <- i + 1
                let word = line.Substring(first, i-first)
                if word.Equals("DATA", StringComparison.OrdinalIgnoreCase) || word.StartsWith("REM", StringComparison.OrdinalIgnoreCase) then
                    builder.Append(line.Substring(first)) |> ignore
                    i <- line.Length
                else builder.Append(word) |> ignore
                statementStart <- false
            elif ch = '@' then
                let first = i
                i <- i + 1
                while i < line.Length && (Char.IsLetterOrDigit line[i] || line[i] = '_') do i <- i + 1
                let name = line.Substring(first, i-first)
                match labels.TryGetValue name with
                | true, n -> builder.Append(n) |> ignore; changed <- true
                | _ -> raise (FatalSsbError($"Label not found: {name}"))
            else
                builder.Append(ch) |> ignore
                if ch = ':' then statementStart <- true
                elif not (Char.IsWhiteSpace ch) then statementStart <- false
                i <- i + 1
        if changed then Some(builder.ToString()) else None

    let derivePaths (cfg: Config) (args: string[]) =
        match args with
        | [||] ->
            raise (FatalSsbError("Usage: ssbfs <input-base-name> [start-line]"))

        | [| baseName |] ->
            let inFile = cfg.WorkingDir1 + baseName + cfg.InputExt
            let outFile = cfg.WorkingDir2 + baseName + cfg.OutputExt
            inFile, outFile, cfg.StartLine

        | [| baseName; startLineText |] ->
            match tryParseInt startLineText with
            | Some n ->
                let inFile = cfg.WorkingDir1 + baseName + cfg.InputExt
                let outFile = cfg.WorkingDir2 + baseName + cfg.OutputExt
                inFile, outFile, n
            | None ->
                raise (FatalSsbError(sprintf "Second argument is not a valid number: %s" startLineText))

        | _ ->
            raise (FatalSsbError("Usage: ssbfs <input-base-name> [start-line]"))

    let firstNonSpaceIndex (s: string) =
        s |> Seq.tryFindIndex (fun c -> c <> ' ')

    let startsWithAtFirstNonSpace (prefix: string) (s: string) =
        match firstNonSpaceIndex s with
        | None -> false
        | Some i ->
            s.Length >= i + prefix.Length &&
            s.Substring(i, prefix.Length) = prefix

    // A numbered line looks like the post-preprocessor SuperBASIC form: digits followed by a space.
    let isNumberedLine (s: string) =
        let t = s.TrimStart()
        if String.IsNullOrWhiteSpace t then false
        else
            let digits = t |> Seq.takeWhile Char.IsAsciiDigit |> Seq.length
            digits > 0 &&
            t.Length > digits &&
            t.[digits] = ' '

    // Decide whether the input is already numbered SuperBASIC or still needs SSB preprocessing.
    let classifySource (lines: string list) : SourceKind =
        let hasSsbMarker =
            lines
            |> List.exists (fun line ->
                let t = line.TrimStart()
                startsWithAtFirstNonSpace "#INCLUDE" line ||
                startsWithAtFirstNonSpace "#DEFINE" line ||
                startsWithAtFirstNonSpace "#IFDEF" line ||
                startsWithAtFirstNonSpace "#ENDIF" line ||
                line.StartsWith("@", StringComparison.Ordinal) ||
                t.StartsWith("##", StringComparison.Ordinal) ||
                line.TrimEnd().EndsWith(@"\+", StringComparison.Ordinal) ||
                line.Contains("GOTO @", StringComparison.Ordinal) ||
                line.Contains("GO SUB @", StringComparison.Ordinal))

        if hasSsbMarker then
            SourceKind.Ssb
        else
            let codeLines =
                lines
                |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s))

            let numberedCount =
                codeLines |> List.filter isNumberedLine |> List.length

            if numberedCount > codeLines.Length / 2 then
                SourceKind.SuperBasic
            else
                SourceKind.Ssb

    // Expansion must complete before labels acquire their final line numbers.
    let private transformToWriter (cfg: Config) (inputFile: string) (startLine: int) (writer: TextWriter) =
        if startLine < 1 || startLine > 32767 then raise (FatalSsbError("SSBSTART must be from 1 to 32767"))
        if cfg.LineDelta <= 0 then raise (FatalSsbError("SSBINC must be positive"))
        let lines =
            try activeSource cfg inputFile |> SsbRecords.expand
            with :? InvalidOperationException as error -> raise (FatalSsbError(error.Message))
        let labels = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
        let mutable lineNumber = int64 startLine
        for line in lines do
            if line.Text.StartsWith("@", StringComparison.Ordinal) then
                let name = line.Text.TrimEnd()
                if name.Length > cfg.MaxNameLength + 1 then raise (FatalSsbError($"Label too long: {name}"))
                if labels.ContainsKey name then raise (FatalSsbError($"Duplicate label: {name}"))
                if labels.Count >= cfg.MaxLabels then raise (FatalSsbError("Too many labels"))
                labels.Add(name, int lineNumber)
            else
                if lineNumber > 32767L then raise (FatalSsbError($"{line.File}:{line.Number}: Generated line number exceeds 32767"))
                lineNumber <- lineNumber + int64 cfg.LineDelta
        lineNumber <- int64 startLine
        // Build all output before publishing so unresolved labels leave no partial file.
        let numbered = ResizeArray<int * string>()
        for line in lines do
            if not (line.Text.StartsWith("@", StringComparison.Ordinal)) then
                let resolved =
                    try resolveLabelReference labels line.Text |> Option.defaultValue line.Text
                    with :? FatalSsbError as error -> raise (FatalSsbError($"{line.File}:{line.Number}: {error.Message}"))
                numbered.Add(int lineNumber, resolved)
                lineNumber <- lineNumber + int64 cfg.LineDelta
        for n, line in numbered do emitLine writer n line

    // Transform an SSB file into numbered source text without writing an intermediate file.
    let transformFile (inputFile: string) =
        let cfg = defaultConfig()
        if not (File.Exists(inputFile)) then
            raise (FatalSsbError(sprintf "Input file does not exist: %s" inputFile))

        let builder = StringBuilder()
        use writer = new StringWriter(builder)
        transformToWriter cfg inputFile cfg.StartLine writer
        builder.ToString()

    // Preserve the original command-line entry point for standalone preprocessor usage.
    let run (args: string[]) =
        let cfg = defaultConfig()
        let inputFile, outputFile, startLine = derivePaths cfg args

        if not (File.Exists(inputFile)) then
            raise (FatalSsbError(sprintf "Input file does not exist: %s" inputFile))

        if File.Exists(outputFile) && not cfg.Overwrite then
            raise (FatalSsbError(sprintf "Output file exists: %s" outputFile))
        let builder = StringBuilder()
        use writer = new StringWriter(builder)
        transformToWriter cfg inputFile startLine writer
        let destination = Path.GetFullPath outputFile
        let temporary = destination + "." + Guid.NewGuid().ToString("N") + ".tmp"
        try
            File.WriteAllText(temporary, builder.ToString())
            if File.Exists destination then
                let mutable index = 1
                while File.Exists(destination + $"_bak{index}") do index <- index + 1
                File.Move(destination, destination + $"_bak{index}")
            File.Move(temporary, destination)
        finally
            if File.Exists temporary then File.Delete temporary
