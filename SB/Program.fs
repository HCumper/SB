module CompilerPipeline

open System
open System.IO
open Antlr4.Runtime.Tree
open Antlr4.Runtime
open FSharpPlus
open FSharpPlus.Data
open Utility
open ParseTreeVisitor
open SymbolTableManager
open SemanticAnalyzer
open MyState

/// Configuration for the compiler.
type Configuration = {
    InputFile: string
    OutputFile: string
    Verbose: bool
}

// Not currently used
type ProcessingError =
    | FileNotFound of string
    | ParseError of string
    | ASTConstructionError of string
    | SymbolTablePopulationError of string
    | IOError of string
    | InvalidArguments of string

let parseArguments = function
    | [| input; output |] -> Ok { InputFile = input; OutputFile = output; Verbose = false }
    | args -> Error (InvalidArguments $"Expected 2 arguments but got %d{args.Length}")

let private createLexer (input: AntlrInputStream) =
    let factory = CommonTokenFactory() :> ITokenFactory
    SBLexer(input, TokenFactory = factory)

/// Parses the file specified in the configuration.
/// Returns a tuple of (parse tree, input stream).
let parseFile (config: Configuration) : (IParseTree * AntlrInputStream) =
    try
        use reader = File.OpenText(config.InputFile)
        let inputStream = AntlrInputStream(reader)
        let lexer = createLexer inputStream
        let tokenStream = CommonTokenStream(lexer)
        let parser = SBParser(tokenStream)
        (parser.program(), inputStream)
    with
    | :? FileNotFoundException -> failwith (sprintf "FileNotFound: %s" config.InputFile)
    | ex -> failwith (sprintf "ParseError: %s" ex.Message)

/// Processes the parse tree to build an AST.
/// Prints the initial AST and returns its root.
let processToAST (parseTree, inputStream) : ASTNode =
    let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
        let visitor = ASTBuildingVisitor()
        parseTree.Accept(visitor)  // Start visiting
    let ast = convertTreeToAst parseTree
    Console.WriteLine(prettyPrintAst (List.head ast) 4)
    List.head ast

/// A monadic semantic analysis using the state monad.
/// It pre-populates the symbol table and then walks the AST to add user-defined symbols.
let semanticAnalysisState (astTree: ASTNode) : MyState<SymbolTable, ASTNode> = state {
    // Retrieve current symbol table.
    let! oldTable = getState
    // Pre-populate with keywords.
    let table = prePopulateSymbolTable astTree oldTable
    do! putState table
    // Retrieve updated table.
    let! table = getState
    // Walk the AST and add user symbols.
    let table = addToTable Overwrite table astTree globalScope
    do! putState table
    // Return the AST (unchanged).
    return astTree
}

/// Generates output (e.g. writing the AST to a file).
let generateOutput (config: Configuration) (ast: ASTNode) =
    File.Delete config.OutputFile
    use writer = new StreamWriter(config.OutputFile)
    // For example, write the pretty-printed AST.
    writer.WriteLine(prettyPrintAst ast 0)
    ()

/// Logs diagnostic information if verbose output is enabled.
let logDiagnostics (config: Configuration) (parseTree, _) (ast: ASTNode) =
    if config.Verbose then
        Console.WriteLine("ANTLR Parse Tree:")
        Console.WriteLine(parseTree.ToString())
        Console.WriteLine("\nInitial AST:")
        printAST "  " ast |> Console.WriteLine
        Console.WriteLine("\nTransformed AST:")

/// Runs semantic analysis using the state monad, starting from an initial symbol table.
let runSemanticAnalysis (ast: ASTNode) (initialTable: SymbolTable) : ASTNode * SymbolTable =
    run (semanticAnalysisState ast) initialTable

[<EntryPoint>]
let main argv =
    try
        let config = parseArguments argv
        let parseTree = parseFile config
        let ast = processToAST parseTree
        let (finalAst, finalTable) = runSemanticAnalysis ast Map.empty
        logDiagnostics config parseTree ast
        generateOutput config ast
        0
    with ex ->
        Console.Error.WriteLine(ex.Message)
        1
