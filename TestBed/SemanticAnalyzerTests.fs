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

    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "y"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "x"), Is.False)
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
    Assert.That(analyzed.SymTab["main"].Symbols.ContainsKey(normalizeIdentifier "paramtype"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "paramtype"), Is.False)

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
            && fact.Category = Some SymbolCategory.BuiltIn)

    Assert.That(paramReference.IsSome, Is.True)
    Assert.That(printCall.IsSome, Is.True)

[<Test>]
let ``dim inside procedure without local remains globally resolvable`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure initialise\n20 DIM score(100)\n30 END DEFine\n40 PRINT score(1)\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "score"), Is.True)
    Assert.That(analyzed.SymTab["initialise"].Symbols.ContainsKey(normalizeIdentifier "score"), Is.False)

    let scoreReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "score"
            && fact.Kind = ReferenceSite
            && fact.Scope = globalScope
            && fact.Category = Some SymbolCategory.Array)

    Assert.That(scoreReference.IsSome, Is.True)

[<Test>]
let ``local statement with dimensions declares local array`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure main\n20 LOCal work(10)\n30 PRINT work(1)\n40 END DEFine\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.SymTab["main"].Symbols.ContainsKey(normalizeIdentifier "work"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "work"), Is.False)

    let workReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "work"
            && fact.Kind = ReferenceSite
            && fact.Scope = "main"
            && fact.Category = Some SymbolCategory.Array)

    Assert.That(workReference.IsSome, Is.True)

[<Test>]
let ``assignment inside procedure without local declares global variable`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure initialise\n20 score = 1\n30 END DEFine\n40 PRINT score\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "score"), Is.True)
    Assert.That(analyzed.SymTab["initialise"].Symbols.ContainsKey(normalizeIdentifier "score"), Is.False)

    let scoreReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "score"
            && fact.Kind = ReferenceSite
            && fact.Scope = globalScope
            && fact.Category = Some SymbolCategory.Variable)

    Assert.That(scoreReference.IsSome, Is.True)

[<Test>]
let ``assignment inside procedure respects explicit local declaration`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure main\n20 LOCal score\n30 score = 1\n40 PRINT score\n50 END DEFine\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.SymTab["main"].Symbols.ContainsKey(normalizeIdentifier "score"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "score"), Is.False)

    let scoreReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "score"
            && fact.Kind = ReferenceSite
            && fact.Scope = "main"
            && fact.Category = Some SymbolCategory.Variable)

    Assert.That(scoreReference.IsSome, Is.True)

[<Test>]
let ``input inside procedure without local declares global variable`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure main\n20 INPUT score\n30 PRINT score\n40 END DEFine\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "score"), Is.True)
    Assert.That(analyzed.SymTab["main"].Symbols.ContainsKey(normalizeIdentifier "score"), Is.False)

    let scoreReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "score"
            && fact.Kind = ReferenceSite
            && fact.Scope = globalScope
            && fact.Category = Some SymbolCategory.Variable)

    Assert.That(scoreReference.IsSome, Is.True)

[<Test>]
let ``channel input respects explicit local declaration`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure main\n20 LOCal score\n30 INPUT#0,score\n40 PRINT score\n50 END DEFine\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.SymTab["main"].Symbols.ContainsKey(normalizeIdentifier "score"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "score"), Is.False)

    let scoreReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "score"
            && fact.Kind = ReferenceSite
            && fact.Scope = "main"
            && fact.Category = Some SymbolCategory.Variable)

    Assert.That(scoreReference.IsSome, Is.True)

[<Test>]
let ``inkey resolves as built in expression source`` () =
    let analyzed =
        analyzeProgram "10 x$=INKEY$(-1)\n20 PRINT x$\n"

    Assert.That(analyzed.Errors, Is.Empty)

    let inkeyReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "INKEY$"
            && fact.Kind = ReferenceSite
            && fact.Scope = globalScope
            && fact.Category = Some SymbolCategory.BuiltIn)

    Assert.That(inkeyReference.IsSome, Is.True)

[<Test>]
let ``goto is handled as control flow not a built in call`` () =
    let analyzed =
        analyzeProgram "10 GO TO 100\n100 PRINT \"done\"\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.Facts |> List.exists (fun fact -> fact.Name = "GOTO" && fact.Kind = CallSite), Is.False)

[<Test>]
let ``symbol positions preserve basic line numbers while keeping editor line numbers`` () =
    let analyzed =
        analyzeProgram "100 score = 1\n200 DEFine PROCedure main\n210 LOCal work\n220 END DEFine\n"

    let scoreSymbol = analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "score"]
    let mainSymbol = analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "main"]
    let workSymbol = analyzed.SymTab["main"].Symbols[normalizeIdentifier "work"]

    Assert.That((Symbol.position scoreSymbol).BasicLineNo, Is.EqualTo(Some 100))
    Assert.That((Symbol.position scoreSymbol).EditorLineNo, Is.EqualTo(1))
    Assert.That((Symbol.position mainSymbol).BasicLineNo, Is.EqualTo(Some 200))
    Assert.That((Symbol.position mainSymbol).EditorLineNo, Is.EqualTo(2))
    Assert.That((Symbol.position workSymbol).BasicLineNo, Is.EqualTo(Some 210))
    Assert.That((Symbol.position workSymbol).EditorLineNo, Is.EqualTo(3))

[<Test>]
let ``implicit updates typing rules without declaring symbols`` () =
    let analyzed =
        analyzeProgram "10 IMPLICIT% a,b\n20 PRINT a\n"

    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "a"), Is.False)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "b"), Is.False)
    Assert.That(analyzed.Errors, Has.Some.Contains("Unresolved reference 'a'"))
    Assert.That(analyzed.ImplicitTyping[globalScope].Integers.Contains(normalizeIdentifier "a"), Is.True)
    Assert.That(analyzed.ImplicitTyping[globalScope].Integers.Contains(normalizeIdentifier "b"), Is.True)

[<Test>]
let ``procedure implicit typing applies globally to later declarations`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure main\n20 IMPLICIT% score\n30 END DEFine\n40 score = 1\n"

    Assert.That(analyzed.Errors, Is.Empty)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "score"), Is.True)

    match analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "score"] with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.Integer))
    | other -> Assert.Fail($"Expected variable symbol for score, got %A{other}")
