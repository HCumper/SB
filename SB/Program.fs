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
open Utility
open ParseTreeVisitor
open SymbolTableManager
open SemanticAnalyzer
open TypeAnalyzer
open Monads.State
open CodeGenerator
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

let private buildConfig () =
    ConfigurationBuilder()
        .SetBasePath(Directory.GetCurrentDirectory())
        .AddJsonFile("appsettings.json", optional = false, reloadOnChange = true)
        .Build()

let private configureLogger (config: IConfiguration) =
    LoggerConfiguration()
        .ReadFrom.Configuration(config)
        .CreateLogger()

let private getSettings argv =
    let configSettings = buildConfig()
    let appName = configSettings.GetValue<string>("ApplicationName")
    let inputFileName = configSettings.GetValue<string>("Appsettings:InputFile")
    let templatesFileName = configSettings.GetValue<string>("Appsettings:TemplatesFile")
    let outputFileName = configSettings.GetValue<string>("Appsettings:OutputFile")
    let verbosityLevel = configSettings.GetValue<bool>("Appsettings:Verbose")
    let (log: Core.Logger) = configureLogger configSettings

    match argv with
    | [| input; output; verbose |] ->
        {| inputFileName = input
           outputFileName = output
           templateFileName = templatesFileName
           verbose = Boolean.Parse(verbose: string)
           appName = appName
           logger = log |}
    | _ ->
        {| inputFileName = inputFileName
           outputFileName = outputFileName
           templateFileName = templatesFileName
           verbose = verbosityLevel
           appName = appName
           logger = log |}

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

let processToAST ((tree: IParseTree), _inputStream) _log verbose : Ast =
    let astNodes = convertTreeToAst tree
    let astRoot = List.head astNodes
    if verbose then Console.WriteLine(prettyPrintAst astRoot 4)
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

let runSemanticAnalysis (astRoot: Ast) (_logger: Core.Logger) (_group: TemplateGroup) =
    let initialState =
        { Ast = astRoot
          SymTab = emptySymbolTable
          CurrentScope = globalScope
          InParameterList = false
          ImplicitTyping = Map.empty
          Errors = [] }

    let (finalState: ProcessingState, _) = run semanticAnalysisState initialState
    finalState

let generateOutput (config: Configuration) (ast: Ast) =
    File.Delete config.OutputFile
    use writer = new StreamWriter(config.OutputFile)
    writer.WriteLine(prettyPrintAst ast 0)

let logDiagnostics (config: Configuration) (parseTree, _) (ast: Ast) =
    if config.Verbose then
        Console.WriteLine("ANTLR Parse Tree:")
        Console.WriteLine(parseTree.ToString())
        Console.WriteLine("\nInitial AST:")
        printAST "  " ast
        Console.WriteLine("\nTransformed AST:")

[<EntryPoint>]
let main argv =
    let settings = getSettings argv
    let parseTree, stream =
        match parseFile settings.inputFileName with
        | Ok(parseTree, inputStream) ->
            if settings.verbose then printfn "Parsing succeeded."
            parseTree, inputStream
        | Error(FileNotFound fileName) ->
            printfn $"Error: File '%s{fileName}' not found."
            exit 1
        | Error(ParseError errorMsg) ->
            printfn $"Parsing failed with error: %s{errorMsg}"
            exit 1

    let ast = processToAST (parseTree, stream) log settings.verbose
    let group = new TemplateGroupFile(settings.templateFileName)
    let newState = runSemanticAnalysis ast settings.logger group
    let z = generateCSharp newState settings.templateFileName
    Console.WriteLine(z)
    1
