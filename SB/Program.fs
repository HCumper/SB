module CompilerPipeline

open System
open System.IO
open Antlr4.Runtime
open Antlr4.Runtime.Tree
open FSharpPlus
open FSharpPlus.Data
open Utility
open ParseTreeVisitor
open SymbolTableManager
open SemanticAnalyzer
open StateResult

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

/// Parse the command-line arguments into a Configuration
let parseArguments = function
    | [| input; output |] ->
        { InputFile = input; OutputFile = output; Verbose = false }
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
let parseFile (config: Configuration) : IParseTree * AntlrInputStream =
    try
        use reader = File.OpenText(config.InputFile)
        let inputStream = AntlrInputStream(reader)
        let lexer = createLexer inputStream
        let tokenStream = CommonTokenStream(lexer)
        let parser = SBParser(tokenStream)
        parser.program(), inputStream
    with
    | :? FileNotFoundException ->
        failwithf "FileNotFound: %s" config.InputFile
    | ex ->
        failwithf "ParseError: %s" ex.Message

/// ----------------------------------
/// 3. AST Construction
/// ----------------------------------
let processToAST ((tree: IParseTree), _inputStream) : ASTNode =
    let visitor = ASTBuildingVisitor()
    let astNodes = tree.Accept(visitor)   // returns ASTNode list
    let astRoot  = List.head astNodes

    // Print the AST in a simple way
    Console.WriteLine(prettyPrintAst astRoot 4)
    astRoot

/// ----------------------------------
/// 4. Semantic Analysis (State+Result)
/// ----------------------------------

/// Populate the symbol table with built-ins and AST symbols
let semanticAnalysisState (astRoot: ASTNode)
    : StateResult<SymbolTable, SymbolTable, string> =
    stateResult {
        let! emptyTable = stateResult.GetState()
        let tableWithKeywords = prePopulateSymbolTable emptyTable
        let fullyPopulateTable =
            addToTable Overwrite tableWithKeywords astRoot globalScope

        do! stateResult.PutState fullyPopulateTable

        // Return the final table on success
        return fullyPopulateTable
        // or report error with return! Error "Woops"
    }

/// Run the semantic analysis, returning (Result<SymbolTable,string>, SymbolTable)
let runSemanticAnalysis (astRoot: ASTNode) (initialTable: SymbolTable) =
    let (newState, result) =
        run (semanticAnalysisState astRoot) initialTable
    result, newState

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

/// ----------------------------------
/// 6. Main Entry
/// ----------------------------------
[<EntryPoint>]
let main argv =
    try
        let config = parseArguments argv
        let parseTree = parseFile config
        let ast = processToAST parseTree

        // Start with an empty symbol table
        let (result, _) = runSemanticAnalysis ast emptySymbolTable

        // If success, generate code; if error, report
        match result with
        | Ok finalTable ->
            // For demonstration, finalTable is a SymbolTable
            // We still have 'ast' if we want to generate code
            logDiagnostics config parseTree ast
            generateOutput config ast
            0
        | Error errMsg ->
            Console.Error.WriteLine(sprintf "Semantic analysis failed: %A" errMsg)
            1

    with ex ->
        Console.Error.WriteLine(ex.Message)
        1
