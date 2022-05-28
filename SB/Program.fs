﻿module Program

open System
open System.IO
open Antlr4.Runtime
open SB
open SBLib
open SymbolTable
open TreeRewriter

[<EntryPoint>]
let main argv =
//    SymbolTable.testTable
    let filename = @"H:\source\Home Repos\SB\q3.sb"
    let reader = File.OpenText(filename)
    let outputFile = @"H:\source\Home Repos\SB\q3.cs"
    File.Delete outputFile
    let cs = new AntlrInputStream(reader)
    let factory = new SBTokenFactory()
    let (TokenFac : ITokenFactory) = factory :> ITokenFactory
    let lexer = new SBLexer(input = cs, TokenFactory = TokenFac)   
    let tokens = new CommonTokenStream(lexer)
    let parser = new SBParser(tokens)
    let parseTree = parser.program()
    let x = parseTree.ToStringTree(parser)
    Console.WriteLine(x)
    Console.WriteLine("")

    let ast = TreeRewriter.RewriteTree parseTree
    
    let initialState = { references = Set.empty; symTab = Map.empty; errorList = []; currentScope = globalScope; outputProcFn = ""; outputGlobal = ""}
    let (_, state) = SymbolTableBuilder.WalkTreeRoot parseTree initialState
    let typedState = TypeResolver.TypeImplicits state
    let resetState = { typedState with currentScope = globalScope}
    let state = CodeGenerator.walkTreeRoot parseTree resetState
//    let strProg = state.outputProcFn.ToString()
//    List.map (fun x -> File.AppendAllText(outputFile, x)) (snd state).outputProcFn |> ignore

    let listing = (snd state).outputProcFn + "\n************************************************\n" + (snd state).outputGlobal 
    File.WriteAllText (outputFile, listing)
    3
