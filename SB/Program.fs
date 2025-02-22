module CompilerPipeline

open System
open System.IO
open Antlr4.Runtime.Tree
open Antlr4.Runtime
open FSharpPlus
open FSharpPlus.Data
//open Monads
open Utility
open ParseTreeVisitor
open SymbolTableManager
open SemanticAnalyzer
//open Monads.MyState
//open StateResult






/// A function: oldState -> (newState, Result<Value, Error>)
//type StateResult<'S,'T,'E> = 'S -> (Result<'T,'E> * 'S)
type StateResult<'S, 'T, 'E> = 'S -> 'S * Result<'T, 'E>

// let run (st: StateResult<'S, 'T, 'Error>) (init: 'S) : (Result<'T, 'Error> * 'S) =
//         st init
let run (comp: StateResult<'S,'T,'E>) (init: 'S) : ('S * Result<'T,'E>) =
    comp init

/// Our builder to support "monadic" syntax
type StateResultBuilder<'S, 'E>() =

    member _.Return(x : 'T) : StateResult<'S,'T,'E> =
        fun state -> (state, Ok x)

    member _.ReturnFrom(m : StateResult<'S,'T,'E>) : StateResult<'S,'T,'E> =
        m

    /// Bind for StateResult<'S,'T1,'E>
    member _.Bind
        (m : StateResult<'S,'T1,'E>,
         f : 'T1 -> StateResult<'S,'T2,'E>) : StateResult<'S,'T2,'E> =
        fun oldState ->
            let (newState, result) = m oldState
            match result with
            | Error err -> (newState, Error err)    // short-circuits
            | Ok value  -> f value newState

    /// Overload for binding a pure Result<'T,'E> into StateResult
    member this.Bind
        (r : Result<'T,'E>,
         f : 'T -> StateResult<'S,'U,'E>) : StateResult<'S,'U,'E> =
        fun st ->
            match r with
            | Error e -> (st, Error e)
            | Ok v    -> (this.Bind (this.Return v, f)) st

    member _.Zero() : StateResult<'S, unit, 'E> =
        fun st -> (st, Ok ())

    /// Utility: get current state
    member _.GetState() : StateResult<'S,'S,'E> =
        fun st -> (st, Ok st)

    /// Utility: set current state
    member _.PutState(newSt : 'S) : StateResult<'S, unit, 'E> =
        fun _ -> (newSt, Ok ())

/// Create a single instance of the builder
let stateResult<'S,'E> = StateResultBuilder<'S,'E>()
















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
    | [| input; output |] -> {InputFile = input; OutputFile = output; Verbose = false}
    | args -> {InputFile = "input"; OutputFile = "output"; Verbose = false}

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

/// A monadic semantic analysis using the stateResult monad.
/// It pre-populates the symbol table and then walks the AST to add user-defined symbols.
/// We want to produce 'SymbolTable' on success and short-circuit on error:
let semanticAnalysisState 
    (astTree: ASTNode) 
    : StateResult<SymbolTable, SymbolTable, string> // for instance
    = stateResult {

    let! emptyTable = stateResult.GetState()      // emptyTable : SymbolTable
    let tableWithKeywords = prePopulateSymbolTable emptyTable
    let fullyPopulateTable = 
        addToTable Overwrite tableWithKeywords astTree globalScope
    do! stateResult.PutState fullyPopulateTable
    // Now 'return' expects a 'SymbolTable', which the monad wraps as 'Ok SymbolTable'
    return fullyPopulateTable
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
// let runSemanticAnalysis (ast: ASTNode) (initialTable: SymbolTable) : ASTNode * SymbolTable =
//     run (semanticAnalysisState ast) initialTable
    
let runSemanticAnalysis (ast: ASTNode) (initialTable: SymbolTable)  =
    let (finalState, result) = run (semanticAnalysisState ast) initialTable
    (result, finalState)

[<EntryPoint>]
let main argv =
    try
        let config = parseArguments argv
        let parseTree = parseFile config
        let ast = processToAST parseTree
        let emptyTable = emptySymbolTable
        let (result, finalTable) = runSemanticAnalysis ast emptyTable

        // match result with
        // | Ok finalAst ->
        //     logDiagnostics config parseTree ast
        //     generateOutput config finalAst
        //     0
        // | Error err ->
        //     Console.Error.WriteLine(sprintf "Semantic analysis failed: %A" err)
        //     1
        // logDiagnostics config parseTree ast
        // generateOutput config ast
        0
    with ex ->
        Console.Error.WriteLine(ex.Message)
        1
