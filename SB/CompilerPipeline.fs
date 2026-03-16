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
open Monads.State
open SyntaxAst

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
    let configSettings = buildConfig()
    let appName = configSettings.GetValue<string>("ApplicationName")
    let inputFileName = configSettings.GetValue<string>("Appsettings:InputFile")
    let templatesFileName = configSettings.GetValue<string>("Appsettings:TemplatesFile")
    let outputFileName = configSettings.GetValue<string>("Appsettings:OutputFile")
    let verbosityLevel = configSettings.GetValue<bool>("Appsettings:Verbose")
    let logger = configureLogger configSettings

    match argv with
    | [| input; output; verbose |] ->
        { InputFileName = input
          OutputFileName = output
          TemplateFileName = templatesFileName
          Verbose = Boolean.Parse(verbose: string)
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

let parseFile (inputFileName: string) : Result<IParseTree * AntlrInputStream, ParseError> =
    try
        use reader = File.OpenText(inputFileName)
        let inputStream = AntlrInputStream(reader)
        let lexer = createLexer inputStream
        let tokenStream = CommonTokenStream(lexer)
        let parser = SBParser(tokenStream)
        Ok(parser.program(), inputStream)
    with
    | ex -> Error(ParseError ex.Message)

let processToAST ((tree: IParseTree), _inputStream) verbose : Ast =
    let astNodes = convertTreeToAst tree
    let astRoot = List.head astNodes
    if verbose then Console.WriteLine(prettyPrintAst astRoot)
    astRoot

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
    match parseFile settings.InputFileName with
    | Ok(parseTree, inputStream) ->
        if settings.Verbose then printfn "Parsing succeeded."
        let ast = processToAST (parseTree, inputStream) settings.Verbose
        Ok(parseTree, inputStream, ast)
    | Error parseError -> Error parseError

let createTemplateGroup settings =
    new TemplateGroupFile(settings.TemplateFileName)
