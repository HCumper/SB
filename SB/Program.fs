module Program

open System
open System.IO
open Antlr4.Runtime
open SB
open SBLib

[<EntryPoint>]
let main argv =
//    SymbolTable.testTable
    let filename = @"H:\source\Home Repos\SB\q3.sb"
    let reader = File.OpenText(filename)
    let cs = new AntlrInputStream(reader)
    let factory = new SBTokenFactory()
    let (TokenFac : ITokenFactory) = factory :> ITokenFactory
    let lexer = new SBLexer(input = cs, TokenFactory = TokenFac)   
    let tokens = new CommonTokenStream(lexer)
    let parser = new SBParser(tokens)
    let parseTree = parser.program()
    let x = parseTree.ToStringTree(parser)
    Console.WriteLine(x)
    Console.WriteLine(x)

    let (_, s) = Walker.WalkTreeRoot parseTree SymbolTableBuilder.BuildSymbolTable
    let typedSymbolTable = TypeResolver.TypeImplicits s
    3
