module TestBed.SemanticAnalyzerTests

open Antlr4.Runtime
open NUnit.Framework

open Types
open ProcessingTypes
open SyntaxAst
open ScopeNames
open ParseTreeVisitor
open SemanticAnalyzer
open SymbolTableManager
open Monads.State

let private parseProgram (input: string) =
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)
    parser.program()

let private analyzeProgram (input: string) =
    let ast =
        parseProgram input
        |> convertTreeToAst
        |> List.head

    let initialState =
        { Ast = ast
          SymTab = emptySymbolTable
          CurrentScope = globalScope
          InParameterList = false
          ImplicitTyping = Map.empty
          Facts = []
          Errors = [] }

    let seededState = prePopulateSymbolTable initialState
    let (_, analyzedState) = run (addToTable Overwrite ast seededState) seededState
    analyzedState

[<Test>]
let ``assignment declares writable target but unresolved read stays an error`` () =
    let analyzed = analyzeProgram "10 y = x + 1\n"

    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey("y"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey("x"), Is.False)
    Assert.That(analyzed.Errors, Has.Some.Contains("Unresolved reference 'x'"))

    let declarationFacts =
        analyzed.Facts
        |> List.filter (fun fact -> fact.Name = "y" && fact.Kind = DeclarationSite)
    Assert.That(declarationFacts, Has.Length.EqualTo(1))

[<Test>]
let ``unknown procedure calls are tracked as call sites instead of declarations`` () =
    let analyzed = analyzeProgram "10 DoThing 1\n"

    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey("DoThing"), Is.False)
    Assert.That(analyzed.Errors, Has.Some.Contains("Unresolved call 'DoThing'"))

    let callFacts =
        analyzed.Facts
        |> List.filter (fun fact -> fact.Name = "DoThing" && fact.Kind = CallSite)
    Assert.That(callFacts, Has.Length.EqualTo(1))

[<Test>]
let ``parameter references resolve within procedure scope`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure main(paramtype)\n20 PRINT paramtype\n30 END DEFine\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.SymTab["main"].Symbols.ContainsKey("paramtype"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey("paramtype"), Is.False)

    let paramReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "paramtype"
            && fact.Kind = ReferenceSite
            && fact.Scope = "main"
            && fact.Category = Some SymbolCategory.Parameter)

    let printCall =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "PRINT"
            && fact.Kind = CallSite
            && fact.Category = Some SymbolCategory.Keyword)

    Assert.That(paramReference.IsSome, Is.True)
    Assert.That(printCall.IsSome, Is.True)
