module SSB

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
        if directiveLine.Length < 10 then
            raise (FatalSsbError(sprintf "Malformed #INCLUDE: %s" directiveLine))
        cfg.WorkingDir1 + directiveLine.Substring(9)

    let inStringEndPosition (s: string) (stopHere1Based: int) =
        let mutable doubleQuoted = false
        let mutable singleQuoted = false

        for i = 0 to stopHere1Based - 1 do
            let ch = s.[i]
            if ch = '"' then
                doubleQuoted <- not doubleQuoted
            elif ch = '\'' then
                singleQuoted <- not singleQuoted

        if not doubleQuoted && not singleQuoted then
            0
        else
            let quote = if doubleQuoted then '"' else '\''
            let mutable found = 0
            let mutable i = stopHere1Based - 1
            while found = 0 && i < s.Length do
                if s.[i] = quote then
                    found <- i + 1
                i <- i + 1
            found

    let stripTabs (s: string) =
        s.Replace('\t', ' ')

    let stripComment (s: string) =
        let rec loop (remaining: string) (acc: string) =
            let pos = remaining.IndexOf("##", StringComparison.Ordinal)
            if pos < 0 then
                acc + remaining
            elif pos = 0 then
                acc
            else
                let marker1 = pos + 1
                let quoteEnd = inStringEndPosition remaining marker1
                if quoteEnd = 0 then
                    acc + remaining.Substring(0, pos)
                else
                    let keep = remaining.Substring(0, quoteEnd)
                    let tail =
                        if quoteEnd < remaining.Length then
                            remaining.Substring(quoteEnd)
                        else
                            ""
                    loop tail (acc + keep)
        loop s ""

    let doRemark (s: string) =
        let rec loop (remaining: string) (acc: string) =
            let pos = remaining.IndexOf("**", StringComparison.Ordinal)
            if pos < 0 then
                acc + remaining
            elif pos = 0 then
                acc + "REMark" + remaining.Substring(2)
            else
                let marker1 = pos + 1
                let quoteEnd = inStringEndPosition remaining marker1
                if quoteEnd = 0 then
                    acc + remaining.Substring(0, pos) + "REMark" + remaining.Substring(pos + 2)
                else
                    let keep = remaining.Substring(0, quoteEnd)
                    let tail =
                        if quoteEnd < remaining.Length then
                            remaining.Substring(quoteEnd)
                        else
                            ""
                    loop tail (acc + keep)
        loop s ""

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

    let rec passOne (cfg: Config) (state: Pass1State) (inputFile: string) =
        if not (File.Exists(inputFile)) then
            raise (FatalSsbError(sprintf "File not found: %s" inputFile))

        use reader = new StreamReader(inputFile)

        while not reader.EndOfStream do
            let mutable line = reader.ReadLine()
            let fc = firstChar line

            if fc = 0 then
                ()
            elif startsWithFirstNonSpace "##" line then
                ()
            elif firstNonSpaceIs '.' line then
                ()
            elif line.Length >= 6 && startsAtCol1 "#ENDIF" (upperAscii line) then
                ()
            else
                line <- joinLineFromReader reader line

                if line.Length > 0 && line.[0] = '@' then
                    if line.Length > cfg.MaxNameLength + 1 then
                        raise (FatalSsbError(sprintf "Label too long: %s" line))
                    if state.Labels.Count >= cfg.MaxLabels - 1 then
                        raise (FatalSsbError("Too many labels"))
                    state.Labels.[line] <- state.LineNum

                elif startsAtCol1 "#INCLUDE" (upperAscii line) then
                    let includeFile = resolveIncludePath cfg line
                    passOne cfg state includeFile

                elif startsAtCol1 "#DEFINE" (upperAscii line) then
                    if line.Length < 9 then
                        raise (FatalSsbError(sprintf "Malformed #DEFINE: %s" line))
                    let name = upperAscii (line.Substring(8))
                    if name.Length > cfg.MaxNameLength then
                        raise (FatalSsbError(sprintf "DEFINE too long: %s" name))
                    if state.Defines.Count >= cfg.MaxLabels - 1 then
                        raise (FatalSsbError("Too many DEFINEs"))
                    state.Defines.Add(name) |> ignore

                elif startsAtCol1 "#IFDEF" (upperAscii line) then
                    if line.Length < 8 then
                        raise (FatalSsbError(sprintf "Malformed #IFDEF: %s" line))
                    let symbol = upperAscii (line.Substring(7))
                    if not (state.Defines.Contains(symbol)) then
                        let mutable foundEndif = false
                        while not foundEndif do
                            if reader.EndOfStream then
                                raise (FatalSsbError("End of file found before #ENDIF"))
                            let skipLine = reader.ReadLine()
                            if skipLine.Length >= 6 && startsAtCol1 "#ENDIF" (upperAscii skipLine) then
                                foundEndif <- true
                else
                    state.LineNum <- state.LineNum + cfg.LineDelta

    let resolveLabelReference (labels: Dictionary<string, int>) (line: string) =
        let pos = line.IndexOf('@')
        if pos < 0 then
            None
        else
            let suffix = line.Substring(pos)
            match labels.TryGetValue suffix with
            | true, n ->
                let prefix = line.Substring(0, pos)
                Some(prefix + string n)
            | _ ->
                raise (FatalSsbError(sprintf "Label not found: %s" suffix))

    let rec passTwo
        (cfg: Config)
        (labels: Dictionary<string, int>)
        (defines: HashSet<string>)
        (writer: TextWriter)
        (lineNumRef: int ref)
        (inputFile: string) =

        if not (File.Exists(inputFile)) then
            raise (FatalSsbError(sprintf "File not found: %s" inputFile))

        use reader = new StreamReader(inputFile)

        while not reader.EndOfStream do
            let mutable line = reader.ReadLine()
            line <- stripTabs line

            let fc = firstChar line

            if fc = 0 then
                if cfg.IgnoreBlankLines then
                    ()
                else
                    emitLine writer !lineNumRef ":"
                    lineNumRef := !lineNumRef + cfg.LineDelta

            elif line.Length > 0 && line.[0] = '@' then
                ()

            elif startsWithFirstNonSpace "##" line then
                ()

            elif firstNonSpaceIs '.' line then
                ()

            else
                if endsWithContinuation line then
                    line <- joinLineFromReader reader line

                if line.Contains("##", StringComparison.Ordinal) then
                    line <- stripComment line

                if line.Contains("**", StringComparison.Ordinal) then
                    line <- doRemark line

                if startsAtCol1 "#DEFINE" (upperAscii line) then
                    ()

                elif line.Length >= 6 && startsAtCol1 "#ENDIF" (upperAscii line) then
                    ()

                else
                    match resolveLabelReference labels line with
                    | Some replaced ->
                        emitLine writer !lineNumRef replaced
                        lineNumRef := !lineNumRef + cfg.LineDelta

                    | None when startsAtCol1 "#INCLUDE" (upperAscii line) ->
                        let includeFile = resolveIncludePath cfg line
                        passTwo cfg labels defines writer lineNumRef includeFile

                    | None when startsAtCol1 "#IFDEF" (upperAscii line) ->
                        if line.Length < 8 then
                            raise (FatalSsbError(sprintf "Malformed #IFDEF: %s" line))
                        let symbol = upperAscii (line.Substring(7))
                        if not (defines.Contains(symbol)) then
                            let mutable foundEndif = false
                            while not foundEndif do
                                if reader.EndOfStream then
                                    raise (FatalSsbError("End of file found before #ENDIF"))
                                let skipLine = reader.ReadLine()
                                if skipLine.Length >= 6 && startsAtCol1 "#ENDIF" (upperAscii skipLine) then
                                    foundEndif <- true

                    | None ->
                        emitLine writer !lineNumRef line
                        lineNumRef := !lineNumRef + cfg.LineDelta

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

    let isNumberedLine (s: string) =
        let t = s.TrimStart()
        if String.IsNullOrWhiteSpace t then false
        else
            let digits = t |> Seq.takeWhile Char.IsAsciiDigit |> Seq.length
            digits > 0 &&
            t.Length > digits &&
            t.[digits] = ' '

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

    let private transformToWriter (cfg: Config) (inputFile: string) (startLine: int) (writer: TextWriter) =
        let pass1 =
            { LineNum = startLine
              Labels = Dictionary<string, int>(StringComparer.Ordinal)
              Defines = HashSet<string>(StringComparer.Ordinal) }

        passOne cfg pass1 inputFile

        let lineNumRef = ref startLine
        passTwo cfg pass1.Labels pass1.Defines writer lineNumRef inputFile

    let transformFile (inputFile: string) =
        let cfg = defaultConfig()
        if not (File.Exists(inputFile)) then
            raise (FatalSsbError(sprintf "Input file does not exist: %s" inputFile))

        let builder = StringBuilder()
        use writer = new StringWriter(builder)
        transformToWriter cfg inputFile cfg.StartLine writer
        builder.ToString()

    let run (args: string[]) =
        let cfg = defaultConfig()
        let inputFile, outputFile, startLine = derivePaths cfg args

        if not (File.Exists(inputFile)) then
            raise (FatalSsbError(sprintf "Input file does not exist: %s" inputFile))

        if File.Exists(outputFile) then
            if cfg.Overwrite then
                File.Delete(outputFile)
            else
                raise (FatalSsbError(sprintf "Output file exists: %s" outputFile))

        use writer = new StreamWriter(outputFile, false)
        transformToWriter cfg inputFile startLine writer
