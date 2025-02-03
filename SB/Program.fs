module Program


open System
open System.IO
open Antlr4.Runtime
open SymbolTable
open ReformatParseTree
open CreateAST
open CleanParseTree
open Utility

[<EntryPoint>]
let main argv =
    let filename = @"C:\source\SB\q3.sb"

    let reader = File.OpenText(filename)

    let outputFile = @"C:\source\SB\q3.cs"

    File.Delete outputFile
    let cs = AntlrInputStream(reader)
    reader.Close()
    let factory = CommonTokenFactory()

    let (TokenFac: ITokenFactory) = factory :> ITokenFactory

    let lexer = SBLexer(input = cs, TokenFactory = TokenFac)

    /// This is the combined lex and parse using Antlr4
    let tokens = CommonTokenStream(lexer)
    let parser = SBParser(tokens)
    let parseTree = parser.program ()
    let x = parseTree.ToStringTree(parser)
    Console.WriteLine(x)
    Console.WriteLine("")
    
    /// Convert the OO parse tree to more idiomatic FSParseTree
    /// This is a dummb step with no knowlege of the parse tree structure
    let fsTree = processParseTree parseTree cs
    
    /// Clean the parse tree by removing nodes that match a given predicate.
    /// TODO remove this step
    let cleanedFsTree = cleanParseTree fsTree true
    
    /// Convert the parse tree to an AST
    /// requires detailed knowledge of SB grammar
    let ast = walkDown (cleanedFsTree |> Option.get)    
    Console.WriteLine("initial AST")
    subTreeToASTNode ast |>
    printAST "  "
    Console.WriteLine("cleaned AST")
    
    /// Clean the AST by removing nodes that match a given predicate.
    /// TODO remove this step
    let cleanedAST = transformedTree (subTreeToASTNode ast)
    cleanedAST |>
    printAST "  "

    4
    // let initialState =
    //     { references = Set.empty
    //       symTab = Map.empty
    //       errorList = []
    //       currentScope = globalScope
    //       outputProcFn = ""
    //       outputGlobal = "" }
    //
    // let (_, state) = SymbolTableBuilder.WalkTreeRoot parseTree initialState
    //
    // let typedState = TypeResolver.TypeImplicits state
    //
    // let resetState =
    //     { typedState with
    //         currentScope = globalScope }

//    let state = CodeGenerator.walkTreeRoot parseTree resetState
    //    let strProg = state.outputProcFn.ToString()
    //    List.map (fun x -> File.AppendAllText(outputFile, x)) (snd state).outputProcFn |> ignore

    // let listing =
    //     (snd state).outputProcFn
    //     + "\n************************************************\n"
    //     + (snd state).outputGlobal

//    File.WriteAllText(outputFile, listing)
