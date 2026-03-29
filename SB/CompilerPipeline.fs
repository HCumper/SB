module CompilerPipeline

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.Configuration
open Serilog
open Antlr4.Runtime
open Antlr4.Runtime.Tree
open Antlr4.StringTemplate
open FSharpPlus.Data

open Types
open ProcessingTypes
open ScopeNames
open AstDiagnostics
open AstToHir
open HirCBackend
open HirCSharpBackend
open ParseTreeVisitor
open SymbolTableManager
open SemanticAnalyzer
open TypeAnalyzer
open SSB
open Monads.State
open SyntaxAst

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
    TemplateFileName: string
    Verbose: bool
    Backend: string
    AppName: string
    Logger: Core.Logger
}

let private buildConfig () =
    ConfigurationBuilder()
        .SetBasePath(Directory.GetCurrentDirectory())
        .AddJsonFile("appsettings.json", optional = false, reloadOnChange = true)
        .Build()

let private configureLogger (config: IConfiguration) =
    LoggerConfiguration()
        .ReadFrom.Configuration(config)
        .CreateLogger()

let getSettings argv : RuntimeSettings =
    let configSettings = buildConfig ()
    let appName = configSettings.GetValue<string>("ApplicationName")
    let inputFileName = configSettings.GetValue<string>("AppSettings:InputFile")
    let templatesFileName = configSettings.GetValue<string>("AppSettings:TemplatesFile")
    let outputFileName = configSettings.GetValue<string>("AppSettings:OutputFile")
    let verbosityLevel = configSettings.GetValue<bool>("AppSettings:Verbose")
    let backend = configSettings.GetValue<string>("AppSettings:Backend")
    let logger = configureLogger configSettings
    let defaultBackend = if String.IsNullOrWhiteSpace backend then "interpret" else backend

    match argv with
    | [| input; output; verbose; backendName |] ->
        { InputFileName = input
          OutputFileName = output
          TemplateFileName = templatesFileName
          Verbose = Boolean.Parse(verbose)
          Backend = backendName
          AppName = appName
          Logger = logger }
    | [| input; output; verbose |] ->
        { InputFileName = input
          OutputFileName = output
          TemplateFileName = templatesFileName
          Verbose = Boolean.Parse(verbose)
          Backend = defaultBackend
          AppName = appName
          Logger = logger }
    | _ ->
        { InputFileName = inputFileName
          OutputFileName = outputFileName
          TemplateFileName = templatesFileName
          Verbose = verbosityLevel
          Backend = defaultBackend
          AppName = appName
          Logger = logger }

let createLexer (input: AntlrInputStream) =
    let factory = CommonTokenFactory() :> ITokenFactory
    SBLexer(input, TokenFactory = factory)

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
                match kind with
                | Ssb.SourceKind.SuperBasic -> sourceText
                | Ssb.SourceKind.Ssb -> Ssb.transformFile inputFileName

            Ok {
                OriginalPath = inputFileName
                EffectiveText = effectiveText
                Kind = kind
            }
    with
    | ex -> Result.Error(ParseError ex.Message)

let parsePreparedSource (preparedSource: PreparedSource) : Result<IParseTree * AntlrInputStream, ParseError> =
    try
        let inputStream = AntlrInputStream(preparedSource.EffectiveText)
        let lexer = createLexer inputStream
        let tokenStream = CommonTokenStream(lexer)
        let parser = SBParser(tokenStream)
        Ok(parser.program(), inputStream)
    with
    | ex -> Result.Error(ParseError ex.Message)

let processToAST ((tree: IParseTree), _inputStream) verbose : Ast =
    // The parser visitor returns a list, but the grammar is rooted at a single
    // Program node, so the pipeline extracts the first AST root.
    let astNodes = convertTreeToAst tree
    let astRoot = List.head astNodes
    if verbose then Console.WriteLine(prettyPrintAst astRoot)
    astRoot

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
    let initialState =
        { Ast = astRoot
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
        match parsePreparedSource preparedSource with
        | Result.Error parseError -> Result.Error parseError
        | Ok(parseTree, inputStream) ->
            if settings.Verbose then
                printfn $"Parsing succeeded. Source kind: %A{preparedSource.Kind}"
            let ast = processToAST (parseTree, inputStream) settings.Verbose
            Ok(parseTree, inputStream, ast)

let createTemplateGroup settings =
    new TemplateGroupFile(settings.TemplateFileName)
