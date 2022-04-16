module Program

open System
open System.IO
open Antlr4.Runtime
open SB
open SBLib

[<EntryPoint>]
let main argv =
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

    let t = Walker.WalkTreeRoot parseTree

    let x=3
    //create symbol table
//    open System

//let children = [|3;5;7;2;1;7;5;|]
//type state= {x:int;y:int}
//let mystate={x=3;y=4}

//let rec Walk (input: Array) state output subscript =
//    let newstate = { state with x=7+state.x }
//    match subscript with
//    | t when t = input.Length -> output
//    | _ -> newstate.x::Walk input newstate output (subscript+1)
    //let WalkArray input state = Walk input state [] 0

//let myArray = WalkArray children  mystate




    0 // return an integer exit code
