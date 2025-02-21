module CompilerPipeline

open System
open System.IO
open Antlr4.Runtime
open FSharpPlus
open FSharpPlus.Control
open Utility
open ParseTreeVisitor
open Antlr4.Runtime.Tree
open System.IO
open Antlr4.Runtime
open Utility
open SymbolTableManager
open SemanticAnalyzer

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

    let parseArguments = function
        | [| input; output |] -> Ok { InputFile = input; OutputFile = output; Verbose = false }
        | args -> Error (InvalidArguments $"Expected 2 arguments but got %d{args.Length}")

    let private createLexer (input: AntlrInputStream) =
        let factory = CommonTokenFactory() :> ITokenFactory
        SBLexer(input, TokenFactory = factory)

    let parseFile config = result {
        try
            use reader = File.OpenText(config.InputFile)
            let inputStream = AntlrInputStream(reader)
            let lexer = createLexer inputStream
            let tokenStream = CommonTokenStream(lexer)
            let parser = SBParser(tokenStream)
            return (parser.program(), inputStream)
        with
        | :? FileNotFoundException -> return! Error (FileNotFound config.InputFile)
        | ex -> return! Error (ParseError ex.Message) }

    let processToAST (parseTree, inputStream) : Result<ASTNode, ProcessingError> = 
        result {
            try
                let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
                    let visitor = ASTBuildingVisitor()
                    parseTree.Accept(visitor)  // Start the visiting process
                let ast = convertTreeToAst parseTree
                prettyPrintAst (List.head ast) 4 |> Console.WriteLine
                return! Ok (head ast)
            with ex ->
                return! Error (ASTConstructionError ex.Message) 
        }

    let semanticAnalysis astTree : Result<(ASTNode * SymbolTable), ProcessingError> = 
        result {
            try
                let primedSymbolTable = prePopulateSymbolTable astTree  // Populate the symbol table with keywords
                let finalSymbolTable = addToTable Overwrite primedSymbolTable  astTree globalScope  // Walk the AST and add symbols 
                return! Ok (astTree, finalSymbolTable)
            with ex ->
                return! Error (ASTConstructionError ex.Message) 
        }

    let generateOutput config ast = result {
        try
            File.Delete config.OutputFile
            use writer = new StreamWriter(config.OutputFile)
            // Actual generation would go here
          //  printAST "  " ast |> writer.WriteLine
            return ()
        with ex ->
            return! Error (IOError ex.Message) }

    let logDiagnostics config (parseTree, _) ast =
        if config.Verbose then
            Console.WriteLine("ANTLR Parse Tree:")
            Console.WriteLine(parseTree.ToString())
            Console.WriteLine("\nInitial AST:")
            printAST "  " ast |> Console.WriteLine
            Console.WriteLine("\nTransformed AST:")
//            transformedTree ast |> printAST "  " |> Console.WriteLine

[<EntryPoint>]
let main argv =
    let handleError = function
        | InvalidArguments msg ->
            Console.Error.WriteLine $"Argument error: %s{msg}"
            1
        | FileNotFound path ->
            Console.Error.WriteLine $"File not found: %s{path}"
            2
        | ParseError msg ->
            Console.Error.WriteLine $"Parse error: %s{msg}"
            3
        | ASTConstructionError msg ->
            Console.Error.WriteLine $"AST construction failed: %s{msg}"
            4
        | IOError msg ->
            Console.Error.WriteLine $"I/O error: %s{msg}"
            5

    result {
        let! config = parseArguments argv
        let! parseTree = parseFile config
        let! ast = processToAST parseTree
        let! semanticAnalysisResult = semanticAnalysis ast
        logDiagnostics config parseTree ast
        do! generateOutput config ast
        return 0
    }
    |> Result.either id handleError     