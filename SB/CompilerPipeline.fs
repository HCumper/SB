module CompilerPipeline

open System
open System.IO
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
    let logger = configureLogger configSettings

    match argv with
    | [| input; output; verbose |] ->
        { InputFileName = input
          OutputFileName = output
          TemplateFileName = templatesFileName
          Verbose = Boolean.Parse(verbose)
          AppName = appName
          Logger = logger }
    | _ ->
        { InputFileName = inputFileName
          OutputFileName = outputFileName
          TemplateFileName = templatesFileName
          Verbose = verbosityLevel
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
          Errors = [] }

    let (finalState: ProcessingState, _) = run semanticAnalysisState initialState
    finalState

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
