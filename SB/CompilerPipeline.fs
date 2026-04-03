module CompilerPipeline

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.Configuration
open Serilog
open Antlr4.Runtime
open Antlr4.Runtime.Tree
open FSharpPlus.Data

open Types
open ProcessingTypes
open ScopeNames
open AstDiagnostics
open AstToHir
open HirCBackend
open HirCSharpBackend
open Interpreter
open ParseTreeVisitor
open SymbolTableManager
open SemanticAnalysisFacts
open SemanticAnalyzer
open TypeAnalyzer
open SSB
open Monads.State
open SyntaxAst

type SyntaxCheckingMode =
    | Relaxed
    | Rigorous

// CompilerPipeline sequences the major compiler stages.
//
// It is responsible for moving source text through preprocessing, parsing, AST
// construction, semantic analysis, and the current code-generation/debug entry
// points. The substantive logic for each stage lives elsewhere; this module owns
// the boundaries between those stages.
//
// The pipeline wires together preprocessing, parsing, AST construction, semantic analysis, and codegen.
type Configuration = {
    InputFile: string
    OutputFile: string
    Verbose: bool
}

type ProcessingError =
    | FileNotFound of string
    | ParseError of string
    | ASTConstructionError of string
    | SymbolTablePopulationError of string
    | IOError of string
    | InvalidArguments of string

type ParseError =
    | FileNotFound of string
    | ParseError of string

type HirLoweringError = {
    Message: string
    Scope: string
    Position: SourcePosition option
}

type PreparedSource = {
    OriginalPath: string
    EffectiveText: string
    Kind: Ssb.SourceKind
}

type RuntimeSettings = {
    InputFileName: string
    OutputFileName: string
    Verbose: bool
    Backend: string
    RuntimeHost: string
    SyntaxChecking: SyntaxCheckingMode
    AppName: string
    Logger: Core.Logger
}

let private buildConfig () =
    let basePath =
        let appBase = AppContext.BaseDirectory
        if Directory.Exists(appBase) then appBase else Directory.GetCurrentDirectory()

    ConfigurationBuilder()
        .SetBasePath(basePath)
        .AddJsonFile("appsettings.json", optional = false, reloadOnChange = true)
        .Build()

let private configureLogger (config: IConfiguration) =
    LoggerConfiguration()
        .ReadFrom.Configuration(config)
        .CreateLogger()

let private parseSyntaxCheckingMode (value: string) =
    match (if isNull value then "" else value.Trim().ToLowerInvariant()) with
    | "rigorous" -> Rigorous
    | "relaxed"
    | "" -> Relaxed
    | _ -> Relaxed

let getSettings argv : RuntimeSettings =
    let configSettings = buildConfig ()
    let appName = configSettings.GetValue<string>("ApplicationName")
    let inputFileName = configSettings.GetValue<string>("AppSettings:InputFile")
    let outputFileName = configSettings.GetValue<string>("AppSettings:OutputFile")
    let verbosityLevel = configSettings.GetValue<bool>("AppSettings:Verbose")
    let backend = configSettings.GetValue<string>("AppSettings:Backend")
    let runtimeHost = configSettings.GetValue<string>("AppSettings:RuntimeHost")
    let syntaxChecking = parseSyntaxCheckingMode (configSettings.GetValue<string>("AppSettings:SyntaxChecking"))
    let logger = configureLogger configSettings
    let defaultBackend = if String.IsNullOrWhiteSpace backend then "interpret" else backend

    let defaultRuntimeHost = if String.IsNullOrWhiteSpace runtimeHost then "console" else runtimeHost

    match argv with
    | [| input; output; verbose; backendName; runtimeHostName |] ->
        { InputFileName = input
          OutputFileName = output
          Verbose = Boolean.Parse(verbose)
          Backend = backendName
          RuntimeHost = runtimeHostName
          SyntaxChecking = syntaxChecking
          AppName = appName
          Logger = logger }
    | [| input; output; verbose; backendName |] ->
        { InputFileName = input
          OutputFileName = output
          Verbose = Boolean.Parse(verbose)
          Backend = backendName
          RuntimeHost = defaultRuntimeHost
          SyntaxChecking = syntaxChecking
          AppName = appName
          Logger = logger }
    | [| input; output; verbose |] ->
        { InputFileName = input
          OutputFileName = output
          Verbose = Boolean.Parse(verbose)
          Backend = defaultBackend
          RuntimeHost = defaultRuntimeHost
          SyntaxChecking = syntaxChecking
          AppName = appName
          Logger = logger }
    | [| input |] ->
        { InputFileName = input
          OutputFileName = outputFileName
          Verbose = verbosityLevel
          Backend = defaultBackend
          RuntimeHost = defaultRuntimeHost
          SyntaxChecking = syntaxChecking
          AppName = appName
          Logger = logger }
    | _ ->
        { InputFileName = inputFileName
          OutputFileName = outputFileName
          Verbose = verbosityLevel
          Backend = defaultBackend
          RuntimeHost = defaultRuntimeHost
          SyntaxChecking = syntaxChecking
          AppName = appName
          Logger = logger }

let createLexer (input: AntlrInputStream) =
    let factory = CommonTokenFactory() :> ITokenFactory
    SBLexer(input, TokenFactory = factory)

let private splitInlineElseBodies (sourceText: string) =
    let normalizeNewlines (text: string) = text.Replace("\r\n", "\n")

    let tryExpandLine (line: string) =
        let trimmed = line.TrimStart()
        let digitCount = trimmed |> Seq.takeWhile Char.IsDigit |> Seq.length

        if digitCount = 0 then
            None
        else
            let lineNumber = trimmed.Substring(0, digitCount)
            let remainder = trimmed.Substring(digitCount).TrimStart()

            if remainder.StartsWith("ELSE", StringComparison.OrdinalIgnoreCase) then
                let afterElse = remainder.Substring(4).TrimStart()

                if afterElse.StartsWith(":") then
                    let body = afterElse.Substring(1).TrimStart()

                    Some [
                        $"{lineNumber} ELSE"
                        $"{lineNumber} {body}"
                    ]
                else
                    None
            else
                None

    normalizeNewlines sourceText
    |> fun text -> text.Split('\n')
    |> Array.toList
    |> List.collect (fun line ->
        match tryExpandLine line with
        | Some expanded -> expanded
        | None -> [ line ])
    |> String.concat "\n"

let private splitInlineEndIfClosers (sourceText: string) =
    let normalizeNewlines (text: string) = text.Replace("\r\n", "\n")

    let tryExpandLine (line: string) =
        let trimmed = line.TrimStart()
        let digitCount = trimmed |> Seq.takeWhile Char.IsDigit |> Seq.length

        if digitCount = 0 then
            None
        else
            let lineNumber = trimmed.Substring(0, digitCount)
            let remainder = trimmed.Substring(digitCount).TrimStart()

            if String.IsNullOrWhiteSpace remainder then
                None
            else
                let segments = remainder.Split(':') |> Array.toList
                let trimmedSegments = segments |> List.map (fun segment -> segment.Trim())

                let rec trailingEndIfCount count remaining =
                    match remaining with
                    | [] -> count
                    | head :: tail when String.Equals(head, "END IF", StringComparison.OrdinalIgnoreCase) ->
                        trailingEndIfCount (count + 1) tail
                    | _ -> count

                let endIfCount =
                    trimmedSegments
                    |> List.rev
                    |> trailingEndIfCount 0

                if endIfCount < 2 then
                    None
                else
                    let prefixCount = segments.Length - endIfCount
                    let prefixSegments = segments |> List.take prefixCount
                    let firstLineBody =
                        if prefixSegments.IsEmpty then
                            "END IF"
                        else
                            String.concat ":" (prefixSegments @ [ " END IF" ])

                    Some(
                        [ yield $"{lineNumber} {firstLineBody}"
                          for _ in 2 .. endIfCount do
                              yield $"{lineNumber} END IF" ])

    normalizeNewlines sourceText
    |> fun text -> text.Split('\n')
    |> Array.toList
    |> List.collect (fun line ->
        match tryExpandLine line with
        | Some expanded -> expanded
        | None -> [ line ])
    |> String.concat "\n"

// Preprocess SSB inputs into numbered source text before handing them to ANTLR.
let private prepareSource (inputFileName: string) : Result<PreparedSource, ParseError> =
    try
        if not (File.Exists(inputFileName)) then
            Result.Error(FileNotFound inputFileName)
        else
            let sourceText = File.ReadAllText(inputFileName)
            let lines = sourceText.Replace("\r\n", "\n").Split('\n') |> Array.toList
            let kind = Ssb.classifySource lines
            let effectiveText =
                (match kind with
                 | Ssb.SourceKind.SuperBasic -> sourceText
                 | Ssb.SourceKind.Ssb -> Ssb.transformFile inputFileName)
                |> splitInlineElseBodies
                |> splitInlineEndIfClosers

            Ok {
                OriginalPath = inputFileName
                EffectiveText = effectiveText
                Kind = kind
            }
    with
    | ex -> Result.Error(ParseError ex.Message)

type private CollectingErrorListener() =
    inherit BaseErrorListener()

    let mutable errors : string list = []
    
    let isIgnorableEofError (msg: string) =
        let normalized = if isNull msg then String.Empty else msg.Trim()
        normalized.Contains("'<EOF>'")

    member _.Messages = List.rev errors

    override _.SyntaxError(_output, _recognizer, _offendingSymbol, line, charPositionInLine, msg, _e) =
        if not (isIgnorableEofError msg) then
            errors <- $"line {line}:{charPositionInLine} {msg}" :: errors

let parsePreparedSource syntaxChecking (preparedSource: PreparedSource) : Result<IParseTree * AntlrInputStream * string list, ParseError> =
    try
        let inputStream = AntlrInputStream(preparedSource.EffectiveText)
        let lexer = createLexer inputStream
        let tokenStream = CommonTokenStream(lexer)
        let parser = SBParser(tokenStream)
        let errorListener = CollectingErrorListener()

        parser.RemoveErrorListeners()
        parser.AddErrorListener(errorListener)

        let parseTree = parser.program()
        let parseMessages = errorListener.Messages

        if syntaxChecking = Rigorous then
            match parseMessages with
            | [] -> Ok(parseTree, inputStream, [])
            | errors -> Result.Error(ParseError(String.concat Environment.NewLine errors))
        else
            Ok(parseTree, inputStream, parseMessages)
    with
    | ex -> Result.Error(ParseError ex.Message)

let normalizeAst ast =
    let rec normalizeStmt stmt =
        let normalizeBlock = function
            | StatementBlock stmts -> StatementBlock(List.map normalizeStmt stmts)
            | LineBlock lines -> LineBlock(normalizeLines lines)

        match stmt with
        | ProcedureDef(pos, name, parameters, body, closingName, endLine) ->
            ProcedureDef(pos, name, parameters, normalizeLines body, closingName, endLine)
        | FunctionDef(pos, name, parameters, body, closingName, endLine) ->
            FunctionDef(pos, name, parameters, normalizeLines body, closingName, endLine)
        | ForStmt(pos, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, body, closingName) ->
            ForStmt(pos, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, normalizeBlock body, closingName)
        | RepeatStmt(pos, name, body, closingName) ->
            RepeatStmt(pos, name, normalizeBlock body, closingName)
        | IfStmt(pos, condition, thenBlock, elseBlock) ->
            IfStmt(pos, condition, normalizeBlock thenBlock, elseBlock |> Option.map normalizeBlock)
        | SelectStmt(pos, selector, clauses) ->
            let normalizedClauses =
                clauses
                |> List.map (fun (SelectClause(clausePos, clauseSelector, rangeExpr, body)) ->
                    SelectClause(clausePos, clauseSelector, rangeExpr, body |> Option.map normalizeBlock))
            SelectStmt(pos, selector, normalizedClauses)
        | WhenStmt(pos, condition, body) ->
            WhenStmt(pos, condition, normalizeLines body)
        | other -> other

    and normalizeLine (Line(pos, lineNumber, stmts)) =
        Line(pos, lineNumber, List.map normalizeStmt stmts)

    and tryExtractFlatFor stmts =
        match stmts with
        | [ ForStmt(pos, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, StatementBlock inlineStmts, closingName) ] ->
            Some(pos, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, inlineStmts, closingName)
        | _ -> None

    and tryMatchClosingNext loopName stmts =
        let normalizedLoopName = normalizeIdentifier loopName
        match stmts with
        | NextStmt(_, nextName) :: trailing ->
            let normalizedNext =
                if String.IsNullOrWhiteSpace nextName then ""
                else normalizeIdentifier nextName
            if normalizedNext = "" || normalizedNext = normalizedLoopName then
                Some trailing
            else
                None
        | _ -> None

    and normalizeLines lines =
        let rec consumeFlatFor loopName depth collected remaining =
            match remaining with
            | [] -> None
            | (Line(_, _, stmts) as line) :: tail ->
                match tryMatchClosingNext loopName stmts with
                | Some trailingStmts when depth = 0 ->
                    let remainingLines =
                        match trailingStmts with
                        | [] -> tail
                        | _ ->
                            match line with
                            | Line(pos, lineNumber, _) -> Line(pos, lineNumber, trailingStmts) :: tail
                    Some(List.rev collected, remainingLines)
                | _ ->
                    let nextDepth =
                        match tryExtractFlatFor stmts, stmts with
                        | Some _, _ -> depth + 1
                        | None, NextStmt _ :: _ when depth > 0 -> depth - 1
                        | _ -> depth
                    consumeFlatFor loopName nextDepth (line :: collected) tail

        let rec loop acc remaining =
            match remaining with
            | [] -> List.rev acc
            | (Line(linePos, lineNumber, stmts) as line) :: tail ->
                match tryExtractFlatFor stmts with
                | Some(forPos, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, inlineStmts, closingName) ->
                    match consumeFlatFor name 0 [] tail with
                    | Some(bodyLines, rest) ->
                        let normalizedBodyLines = normalizeLines bodyLines
                        let bodyBlock =
                            match inlineStmts, normalizedBodyLines with
                            | [], bodyOnly -> LineBlock(bodyOnly)
                            | inlineOnly, [] -> StatementBlock(List.map normalizeStmt inlineOnly)
                            | inlineOnly, bodyOnly ->
                                LineBlock(Line(forPos, forPos.BasicLineNo, List.map normalizeStmt inlineOnly) :: bodyOnly)
                        let normalizedFor =
                            ForStmt(forPos, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, bodyBlock, closingName)
                        loop (Line(linePos, lineNumber, [ normalizedFor ]) :: acc) rest
                    | None ->
                        loop (normalizeLine line :: acc) tail
                | None ->
                    loop (normalizeLine line :: acc) tail

        loop [] lines

    match ast with
    | Program(pos, lines) -> Program(pos, normalizeLines lines)

let processToAST ((tree: IParseTree), _inputStream) verbose : Ast =
    // The parser visitor returns a list, but the grammar is rooted at a single
    // Program node, so the pipeline extracts the first AST root.
    let astNodes = convertTreeToAst tree
    let astRoot = normalizeAst (List.head astNodes)
    if verbose then Console.WriteLine(prettyPrintAst astRoot)
    astRoot

let private validateLineSequence ast =
    let rec collectNestedLines contextName stmts =
        stmts
        |> List.collect (function
            | ProcedureDef(_, name, _, body, _, _)
            | FunctionDef(_, name, _, body, _, _) ->
                collectLines $"routine '{name}'" body
            | _ -> [])

    and collectLines contextName lines =
        lines
        |> List.collect (fun (Line(pos, lineNumber, stmts)) ->
            let currentEntry =
                match lineNumber with
                | Some value -> [ value, pos, contextName ]
                | None -> []

            currentEntry @ collectNestedLines contextName stmts)

    match ast with
    | Program(_, lines) ->
        let numberedLines = collectLines "program" lines

        let rec loop previous errors remaining =
            match remaining with
            | [] -> List.rev errors
            | (current, pos, contextName) :: tail ->
                let nextErrors =
                    match previous with
                    | Some(prevNumber, _, _) when current <= prevNumber ->
                        $"Out-of-sequence line number {current} after {prevNumber} in {contextName} at {pos.EditorLineNo}:{pos.Column}" :: errors
                    | _ -> errors
                loop (Some(current, pos, contextName)) nextErrors tail

        loop None [] numberedLines

// Semantic analysis is modeled as a state pipeline so symbol-table mutations stay explicit.
let semanticAnalysisState : State<ProcessingState, ProcessingState> =
    state {
        let! currentState = getState
        let prePopulatedState = prePopulateSymbolTable currentState
        do! putState prePopulatedState
        do! addToTable Overwrite currentState.Ast currentState
        let! stateWithSymbols = getState
        let finalState = fillImplicitTypesAndModifyNamesInState stateWithSymbols
        do! putState finalState
        return stateWithSymbols
    }

let runSemanticAnalysis (astRoot: Ast) =
    let normalizedAst = normalizeAst astRoot
    let initialState =
        { Ast = normalizedAst
          SymTab = emptySymbolTable
          CurrentScope = globalScope
          InParameterList = false
          ImplicitTyping = Map.empty
          Facts = []
          ExpressionFacts = []
          Diagnostics = []
          Errors = []
          ExprTypes = Map.empty
          TargetTypes = Map.empty
          ResolvedSymbols = Map.empty
          RoutineSymbols = Map.empty
          ParameterSymbols = Map.empty
          ActiveLoops = [] }

    let (finalState: ProcessingState, _) = run semanticAnalysisState initialState
    finalState

let runHirLowering (state: ProcessingState) =
    lowerToHir state
    |> Result.mapError (List.map (fun error ->
        { Message = error.Message
          Scope = error.Scope
          Position = error.Position }))

let generateCSharpFromHir state className =
    runHirLowering state
    |> Result.map (HirCSharpBackend.generateCSharpFromHir className)

let generateCSharpFromLoweredHir className hirProgram =
    HirCSharpBackend.generateCSharpFromHir className hirProgram

let generateCFromLoweredHir programName hirProgram =
    HirCBackend.generateCFromHir programName hirProgram

let private formatHirLoweringErrorForRuntime (error: HirLoweringError) =
    let location =
        match error.Position with
        | Some pos -> $" at %d{pos.EditorLineNo}:%d{pos.Column}"
        | None -> String.Empty

    $"[{error.Scope}] {error.Message}{location}"

let private sanitizeManagedAssemblyName (value: string) =
    let chars =
        value
        |> Seq.map (fun ch -> if Char.IsLetterOrDigit ch || ch = '_' then ch else '_')
        |> Seq.toArray
        |> System.String

    if String.IsNullOrWhiteSpace chars then "GeneratedProgram"
    elif Char.IsDigit chars[0] then "_" + chars
    else chars

let private runProcess (fileName: string) (arguments: string list) (workingDirectory: string) =
    let startInfo = ProcessStartInfo()
    startInfo.FileName <- fileName
    startInfo.WorkingDirectory <- workingDirectory
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.UseShellExecute <- false

    arguments |> List.iter startInfo.ArgumentList.Add

    use childProcess = new Process()
    childProcess.StartInfo <- startInfo

    let output = Text.StringBuilder()
    let errors = Text.StringBuilder()

    childProcess.OutputDataReceived.Add(fun args ->
        if not (isNull args.Data) then
            output.AppendLine(args.Data) |> ignore)

    childProcess.ErrorDataReceived.Add(fun args ->
        if not (isNull args.Data) then
            errors.AppendLine(args.Data) |> ignore)

    if not (childProcess.Start()) then
        Result.Error $"Failed to start process '{fileName}'."
    else
        childProcess.BeginOutputReadLine()
        childProcess.BeginErrorReadLine()
        childProcess.WaitForExit()

        let stdout = output.ToString().Trim()
        let stderr = errors.ToString().Trim()

        if childProcess.ExitCode = 0 then
            Result.Ok stdout
        else
            let details =
                [ if not (String.IsNullOrWhiteSpace stdout) then yield stdout
                  if not (String.IsNullOrWhiteSpace stderr) then yield stderr ]
                |> String.concat Environment.NewLine

            Result.Error $"Process '{fileName}' failed with exit code {childProcess.ExitCode}.{if String.IsNullOrWhiteSpace details then String.Empty else Environment.NewLine + details}"

let generateDotNetExeFromLoweredHir appName outputPath hirProgram =
    let outputFullPath = Path.GetFullPath(outputPath)
    let outputDirectory =
        match Path.GetDirectoryName(outputFullPath) with
        | null
        | "" -> Directory.GetCurrentDirectory()
        | directory -> directory

    let requestedBaseName = Path.GetFileNameWithoutExtension(outputFullPath)
    let assemblyName = sanitizeManagedAssemblyName requestedBaseName
    let className =
        if String.IsNullOrWhiteSpace appName then assemblyName else sanitizeManagedAssemblyName appName

    let generatedSource = HirCSharpBackend.generateCSharpFromHir className hirProgram
    let tempDirectory = Path.Combine(Path.GetTempPath(), "sb-dotnetexe", Guid.NewGuid().ToString("N"))
    let publishDirectory = Path.Combine(tempDirectory, "publish")
    let projectPath = Path.Combine(tempDirectory, $"{assemblyName}.csproj")
    let sourcePath = Path.Combine(tempDirectory, "Program.cs")
    let runtimeIdentifier = RuntimeInformation.RuntimeIdentifier

    let projectText =
        $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>disable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <AssemblyName>{assemblyName}</AssemblyName>
    <RootNamespace>{assemblyName}</RootNamespace>
    <PublishSingleFile>true</PublishSingleFile>
    <SelfContained>true</SelfContained>
    <DebugType>none</DebugType>
    <DebugSymbols>false</DebugSymbols>
    <EnableCompressionInSingleFile>true</EnableCompressionInSingleFile>
  </PropertyGroup>
</Project>
"""

    try
        Directory.CreateDirectory(tempDirectory) |> ignore
        Directory.CreateDirectory(outputDirectory) |> ignore
        File.WriteAllText(projectPath, projectText)
        File.WriteAllText(sourcePath, generatedSource)

        match runProcess
                  "dotnet"
                  [ "publish"
                    projectPath
                    "-c"
                    "Release"
                    "-r"
                    runtimeIdentifier
                    "-o"
                    publishDirectory
                    "--nologo" ]
                  tempDirectory with
        | Result.Error message ->
            Result.Error message
        | Result.Ok _ ->
            let publishedExe = Path.Combine(publishDirectory, $"{assemblyName}.exe")

            if not (File.Exists(publishedExe)) then
                Result.Error $"dotnet publish completed but did not produce '{publishedExe}'."
            else
                File.Copy(publishedExe, outputFullPath, true)
                Result.Ok outputFullPath
    finally
        if Directory.Exists(tempDirectory) then
            Directory.Delete(tempDirectory, true)

let generateAstOutput (config: Configuration) (ast: Ast) =
    File.Delete config.OutputFile
    use writer = new StreamWriter(config.OutputFile)
    writer.WriteLine(prettyPrintAst ast)

let logDiagnostics (config: Configuration) (parseTree, _) (ast: Ast) =
    if config.Verbose then
        Console.WriteLine("ANTLR Parse Tree:")
        Console.WriteLine(parseTree.ToString())
        Console.WriteLine("\nInitial AST:")
        prettyPrintAst ast
        |> fun text -> text.Replace("\n", Environment.NewLine + "  ")
        |> printf "%s%s" "  "
        Console.WriteLine("\nTransformed AST:")

let loadAstFromInput settings =
    // This is the shared parse entry point used by both tests and the CLI.
    match prepareSource settings.InputFileName with
    | Result.Error parseError -> Result.Error parseError
    | Ok preparedSource ->
        match parsePreparedSource settings.SyntaxChecking preparedSource with
        | Result.Error parseError -> Result.Error parseError
        | Ok(parseTree, inputStream, parseWarnings) ->
            if parseWarnings |> List.isEmpty |> not then
                for warning in parseWarnings do
                    settings.Logger.Warning("Parser recovery warning: {Warning}", warning)
            if settings.Verbose then
                if preparedSource.Kind = Ssb.SourceKind.Ssb then
                    Console.WriteLine("Preprocessed SuperBASIC:")
                    Console.WriteLine(preparedSource.EffectiveText)
                printfn $"Parsing succeeded. Source kind: %A{preparedSource.Kind}"
            let ast = processToAST (parseTree, inputStream) settings.Verbose
            match validateLineSequence ast with
            | [] -> Ok(parseTree, inputStream, ast)
            | errors -> Result.Error(ParseError(String.concat Environment.NewLine errors))

let lowerAstForRuntime (ast: Ast) =
    let state = runSemanticAnalysis ast

    if not state.Errors.IsEmpty then
        let messages =
            if state.Diagnostics.IsEmpty then
                state.Errors
            else
                state.Diagnostics |> List.map formatDiagnostic

        Result.Error(String.concat Environment.NewLine messages)
    else
        runHirLowering state
        |> Result.mapError (fun errors ->
            errors
            |> List.map formatHirLoweringErrorForRuntime
            |> String.concat Environment.NewLine)

let mergeRuntimeAst currentAst incomingAst =
    let mergeLines currentLines incomingLines =
        let incomingByLine =
            incomingLines
            |> List.choose (fun (Line(_, lineNumber, _) as line) -> lineNumber |> Option.map (fun value -> value, line))
            |> Map.ofList

        let mergedExisting =
            currentLines
            |> List.choose (fun (Line(_, lineNumber, _) as line) ->
                match lineNumber with
                | Some value ->
                    match incomingByLine.TryFind value with
                    | Some replacement -> Some replacement
                    | None -> Some line
                | None -> Some line)

        let existingNumbered =
            currentLines
            |> List.choose (fun (Line(_, lineNumber, _)) -> lineNumber)
            |> Set.ofList

        let appendedIncoming =
            incomingLines
            |> List.choose (fun (Line(_, lineNumber, _) as line) ->
                match lineNumber with
                | Some value when not (existingNumbered.Contains value) -> Some(value, line)
                | _ -> None)
            |> List.sortBy fst
            |> List.map snd

        mergedExisting @ appendedIncoming

    match currentAst, incomingAst with
    | Program(pos, currentLines), Program(_, incomingLines) -> Program(pos, mergeLines currentLines incomingLines)

let loadRuntimeProgram syntaxChecking verbose logger inputFileName : Result<RuntimeLoadedProgram, string> =
    let settings =
        { InputFileName = inputFileName
          OutputFileName = String.Empty
          Verbose = verbose
          Backend = "interpret"
          RuntimeHost = "console"
          SyntaxChecking = syntaxChecking
          AppName = "SB"
          Logger = logger }

    match loadAstFromInput settings with
    | Result.Error(FileNotFound path) -> Result.Error $"File '{path}' not found."
    | Result.Error(ParseError message) -> Result.Error message
    | Result.Ok(_, _, ast) ->
        lowerAstForRuntime ast
        |> Result.map (fun hir -> { Ast = ast; Hir = hir })
