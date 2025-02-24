module CompilerPipeline

open System
open System.IO
open Microsoft.Extensions.Configuration
open Serilog
open Antlr4.Runtime
open Antlr4.Runtime.Tree

open FSharpPlus
open FSharpPlus.Data

open Utility
open ParseTreeVisitor
open SymbolTableManager
open SemanticAnalyzer
open Monads.State

/// ----------------------------------
/// 1. Compiler Configuration
/// ----------------------------------
type Configuration = {
    InputFile  : string
    OutputFile : string
    Verbose    : bool
}

type ProcessingError =
    | FileNotFound               of string
    | ParseError                 of string
    | ASTConstructionError       of string
    | SymbolTablePopulationError of string
    | IOError                    of string
    | InvalidArguments           of string

type ParseError =
    | FileNotFound of string
    | ParseError of string
    
/// 1. Parse the command-line arguments into a Configuration
let parseArguments = function
    | [| input; output; verbose |] ->
        { InputFile = input; OutputFile = output; Verbose = (Boolean.Parse verbose) }
    | _ ->
        // Fallback if missing args
        { InputFile = "input"
          OutputFile = "output"
          Verbose = false }

/// ----------------------------------
/// 2. Parsing
/// ----------------------------------
let private createLexer (input: AntlrInputStream) =
    let factory = CommonTokenFactory() :> ITokenFactory
    SBLexer(input, TokenFactory = factory)

/// Parse file into (IParseTree, AntlrInputStream)

let parseFile (config: Configuration) : Result<IParseTree * AntlrInputStream, ParseError> =
    if not (File.Exists(config.InputFile)) then
        Error (FileNotFound config.InputFile)
    else
        try
            use reader = File.OpenText(config.InputFile)
            let inputStream = AntlrInputStream(reader)
            let lexer = createLexer inputStream
            let tokenStream = CommonTokenStream(lexer)
            let parser = SBParser(tokenStream)
            Ok (parser.program(), inputStream)
        with
        | ex -> Error (ParseError ex.Message)
        
/// ----------------------------------
/// 3. AST Construction
/// ----------------------------------
let processToAST ((tree: IParseTree), _inputStream) log (config: Configuration) : ASTNode =
    let visitor = ASTBuildingVisitor()
    let astNodes = tree.Accept(visitor)   // returns ASTNode list
    let astRoot  = List.head astNodes

    // Print the AST in a simple way
    if config.Verbose then Console.WriteLine(prettyPrintAst astRoot 4)
    astRoot

/// ----------------------------------
/// 4. Semantic Analysis (State+Result)
/// ----------------------------------

let semanticAnalysisState : State<ProcessingState, ProcessingState> =
    state {
        let! currentState = getState
        let prePopulatedState = prePopulateSymbolTable currentState
        do! putState prePopulatedState
        let! updatedState = addToTable Overwrite currentState.Ast currentState.CurrentScope
        let! finalState = getState
        // do! putState updatedState
        return finalState
    }


/// Run the semantic analysis, returning (Result<SymbolTable,string>, SymbolTable)
let runSemanticAnalysis (astRoot: ASTNode) (logger: Core.Logger) =
    // Create the initial ProcessingState
    let initialState = {
        Ast = astRoot
        SymTab = emptySymbolTable
        CurrentScope = globalScope
        InParameterList = false
        ImplicitInts = Set.empty<string>
        ImplicitStrings = Set.empty<string>
        Errors = []
        Logger = logger
    }

    // Run the semantic analysis state computation
    let (finalState: ProcessingState, _) = run semanticAnalysisState initialState

    // Return the result along with the final ProcessingState
    result, finalState

/// ----------------------------------
/// 5. Output Generation & Logging
/// ----------------------------------
let generateOutput (config: Configuration) (ast: ASTNode) =
    File.Delete config.OutputFile
    use writer = new StreamWriter(config.OutputFile)
    writer.WriteLine(prettyPrintAst ast 0)

let logDiagnostics (config: Configuration) (parseTree, _) (ast: ASTNode) =
    if config.Verbose then
        Console.WriteLine("ANTLR Parse Tree:")
        Console.WriteLine(parseTree.ToString())
        Console.WriteLine("\nInitial AST:")
        printAST "  " ast |> Console.WriteLine
        Console.WriteLine("\nTransformed AST:") // if you had transformations

let buildConfig () =
    ConfigurationBuilder()
        .SetBasePath(System.IO.Directory.GetCurrentDirectory())  // Ensure the working directory is correct
        .AddJsonFile("appsettings.json", optional = false, reloadOnChange = true)  // Load JSON config
        .Build()

let configureLogger (config: IConfiguration) =
    LoggerConfiguration()
        .ReadFrom.Configuration(config)  // Load Serilog settings from appsettings.json
        .CreateLogger()

/// ----------------------------------
/// 6. Main Entry
/// ----------------------------------
[<EntryPoint>]
let main argv =
    try
        let configSettings = (buildConfig ())
  // Load appsettings.json
        let (log: Core.Logger) = configureLogger configSettings  // Configure Serilog
        let appName = configSettings.GetValue<string>("ApplicationName")
        log.Information("Starting application: {ApplicationName}", appName)
        
        // The transpiler pipeline
        let config = parseArguments argv
        match parseFile config with
        | Ok (parseTree, inputStream) ->
            if config.Verbose then printfn "Parsing succeeded."
        | Error (FileNotFound fileName) ->
            printfn $"Error: File '%s{fileName}' not found."
            exit 1
        | Error (ParseError errorMsg) ->
            printfn $"Parsing failed with error: %s{errorMsg}"
            exit 1
        
        let parseTree, stream =
            match parseFile config with
            | Ok (tree, stream) -> tree, stream
            | Error err -> 
                log.Fatal("Parsing failed: {errorMsg}", err)
                Console.Error.WriteLine $"Parsing failed: %A{err}"
                exit 1
        let ast = processToAST (parseTree, stream) log (configSettings.Get<Configuration>())
        let newState =
            match runSemanticAnalysis ast log with
            | (_, returnedState) -> returnedState

        // If success, generate code; if error, report
        // match newState with
        // | Ok finalTable ->
        //     // For demonstration, finalTable is generate code
        //     logDiagnostics config parseTree ast
        //     generateOutput config ast
        //     ()
        // | Error errMsg ->
        //     log.Fatal("Semantic analysis failed: {errorMsg}", errMsg)
        //     Console.Error.WriteLine(sprintf "Semantic analysis failed: %A" errMsg)
        //     ()
        log.Information("Application shutting down.")
        0
    with ex ->
        Console.Error.WriteLine(ex.Message)
        1
