module TestBed.ProgramFixtureTests

open System.IO
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

let private fixturePath fileName =
    let candidates =
        [ Path.Combine(__SOURCE_DIRECTORY__, "..", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", Path.GetFileName(fileName)) ]
        |> List.map Path.GetFullPath

    match candidates |> List.tryFind File.Exists with
    | Some path -> path
    | None -> failwith $"Fixture file not found: {fileName}"

let private parseProgramFile fileName =
    let input = File.ReadAllText(fixturePath fileName)
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)
    parser.program()

let private parseAstFromFile fileName =
    parseProgramFile fileName
    |> convertTreeToAst
    |> List.head

let private analyzeAst ast =
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

let private topLevelStatements ast =
    match ast with
    | Program(_, lines) ->
        lines
        |> List.collect (fun (Line(_, _, stmts)) -> stmts)

let private tryFindProcedure name ast =
    topLevelStatements ast
    |> List.tryPick (function
        | ProcedureDef(_, procName, parameters, body) when procName = name -> Some(parameters, body)
        | _ -> None)

let private tryFindLine lineNumber lines =
    lines
    |> List.tryPick (fun line ->
        match line with
        | Line(_, Some n, stmts) when n = lineNumber -> Some stmts
        | _ -> None)

[<Test>]
[<Category("ProgramFixture")>]
let ``q3 fixture preserves selected AST subtrees`` () =
    let ast = parseAstFromFile "q3.SB"

    match tryFindProcedure "quicksort" ast with
    | Some(parameters, body) ->
        Assert.That(parameters, Has.Length.EqualTo(2))
        Assert.That(parameters[0], Is.EqualTo("l"))
        Assert.That(parameters[1], Is.EqualTo("r"))

        match tryFindLine 630 body with
        | Some [ Assignment(_, Identifier(_, "i"), Identifier(_, "l")); Assignment(_, Identifier(_, "j"), Identifier(_, "r")) ] -> ()
        | other -> Assert.Fail($"Unexpected quicksort line 630: %A{other}")
    | None -> Assert.Fail("Expected top-level procedure 'quicksort'")

    match tryFindProcedure "QUICKSORT1" ast with
    | Some(_, body) ->
        match tryFindLine 1230 body with
        | Some [ Assignment(_, Identifier(_, "low"), Identifier(_, "bottom")); Assignment(_, Identifier(_, "high"), Identifier(_, "top")); Assignment(_, Identifier(_, "ptr"), Identifier(_, "bottom")) ] -> ()
        | other -> Assert.Fail($"Unexpected QUICKSORT1 line 1230: %A{other}")
    | None -> Assert.Fail("Expected top-level procedure 'QUICKSORT1'")

[<Test>]
[<Category("ProgramFixture")>]
let ``q3 fixture semantic analysis records declarations references and calls`` () =
    let ast = parseAstFromFile "q3.SB"
    let analyzed = analyzeAst ast

    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "check"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "quicksort"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "QS2_PARTITION"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "QUICKSORT2"), Is.True)

    Assert.That(analyzed.SymTab["quicksort"].Symbols.ContainsKey(normalizeIdentifier "l"), Is.True)
    Assert.That(analyzed.SymTab["quicksort"].Symbols.ContainsKey(normalizeIdentifier "r"), Is.True)
    Assert.That(analyzed.SymTab["quicksort"].Symbols.ContainsKey(normalizeIdentifier "i"), Is.True)
    Assert.That(analyzed.SymTab["quicksort"].Symbols.ContainsKey(normalizeIdentifier "j"), Is.True)

    let quicksortParameterDeclaration =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "l"
            && fact.Scope = "quicksort"
            && fact.Kind = DeclarationSite
            && fact.Category = Some SymbolCategory.Parameter)

    let printCall =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "PRINT"
            && fact.Kind = CallSite
            && fact.Category = Some SymbolCategory.BuiltIn)

    let quicksortDecl =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "quicksort"
            && fact.Scope = globalScope
            && fact.Kind = DeclarationSite
            && fact.Category = Some SymbolCategory.Procedure)

    let partitionCall =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "QS2_PARTITION"
            && fact.Scope = globalScope
            && fact.Kind = CallSite
            && fact.Category = Some SymbolCategory.Procedure)

    let checkCall =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "check"
            && fact.Scope = globalScope
            && fact.Kind = CallSite
            && fact.Category = Some SymbolCategory.Procedure)

    Assert.That(quicksortParameterDeclaration.IsSome, Is.True)
    Assert.That(printCall.IsSome, Is.True)
    Assert.That(quicksortDecl.IsSome, Is.True)
    Assert.That(partitionCall.IsSome, Is.True)
    Assert.That(checkCall.IsSome, Is.True)
    Assert.That(analyzed.Errors, Is.Empty)

[<Test>]
[<Category("ProgramFixture")>]
let ``golfer fixture preserves selected AST subtrees`` () =
    let ast = parseAstFromFile "Golfer.sb"

    match tryFindProcedure "pow" ast with
    | Some(_, body) ->
        match tryFindLine 860 body with
        | Some [ ForStmt(_, "p", NumberLiteral(_, "0"), NumberLiteral(_, "200"), None, LineBlock _) ] -> ()
        | other -> Assert.Fail($"Unexpected pow line 860: %A{other}")
    | None -> Assert.Fail("Expected top-level procedure 'pow'")

    match tryFindProcedure "ang" ast with
    | Some(_, body) ->
        match tryFindLine 980 body with
        | Some stmts
            when List.length stmts = 3
                 && (match stmts[0] with ChannelProcedureCall(_, "AT", _, _) -> true | _ -> false)
                 && (match stmts[1] with ChannelProcedureCall(_, "INPUT", _, _) -> true | _ -> false)
                 && (match stmts[2] with Assignment(_, Identifier(_, "angle"), _) -> true | _ -> false) -> ()
        | other -> Assert.Fail($"Unexpected ang line 980: %A{other}")
    | None -> Assert.Fail("Expected top-level procedure 'ang'")

[<Test>]
[<Category("ProgramFixture")>]
let ``golfer fixture semantic analysis records declarations references and calls`` () =
    let ast = parseAstFromFile "Golfer.sb"
    let analyzed = analyzeAst ast

    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "pow"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "ang"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "score"), Is.True)

    Assert.That(analyzed.SymTab["pow"].Symbols.ContainsKey(normalizeIdentifier "p"), Is.True)

    let atCall =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "AT"
            && fact.Kind = CallSite)

    let inputCall =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "INPUT"
            && fact.Kind = CallSite)

    let pDeclaration =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "p"
            && fact.Scope = "pow"
            && fact.Kind = DeclarationSite
            && fact.Category = Some SymbolCategory.Variable)

    let powCall =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "pow"
            && fact.Scope = globalScope
            && fact.Kind = CallSite
            && fact.Category = Some SymbolCategory.Procedure)

    let scoreReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "score"
            && fact.Scope = globalScope
            && fact.Kind = ReferenceSite
            && fact.Category = Some SymbolCategory.Array)

    Assert.That(atCall.IsSome, Is.True)
    Assert.That(inputCall.IsSome, Is.True)
    Assert.That(pDeclaration.IsSome, Is.True)
    Assert.That(powCall.IsSome, Is.True)
    Assert.That(scoreReference.IsSome, Is.True)
    Assert.That(analyzed.Errors |> List.exists (fun error -> error.Contains("score")), Is.False)

[<Test>]
[<Category("ProgramFixture")>]
let ``project planner fixture semantic analysis resolves mixed-case globals and channel references`` () =
    let ast = parseAstFromFile "Project Planner.sb"
    let analyzed = analyzeAst ast

    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "fieldWidth3"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "fieldwidth3"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "fieldWidth4"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "fieldWidth9"), Is.True)
    Assert.That(analyzed.SymTab[globalScope].Symbols.ContainsKey(normalizeIdentifier "fieldWidth10"), Is.True)

    let fieldWidth3Declaration =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "fieldWidth3"
            && fact.Scope = globalScope
            && fact.Kind = DeclarationSite
            && fact.Category = Some SymbolCategory.Variable)

    let drawMenuCaseReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "fieldWidth3"
            && fact.Scope = globalScope
            && fact.Position.EditorLineNo = 306)

    let inkeyChannelReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "fieldWidth3"
            && fact.Scope = globalScope
            && fact.Position.EditorLineNo = 323
            && fact.Kind = ReferenceSite)

    let createWindowReference =
        analyzed.Facts
        |> List.tryFind (fun fact ->
            fact.Name = "fieldWidth4"
            && fact.Scope = globalScope
            && fact.Position.EditorLineNo = 210
            && fact.Kind = ReferenceSite)

    Assert.That(fieldWidth3Declaration.IsSome, Is.True)
    Assert.That(drawMenuCaseReference.IsSome, Is.True)
    Assert.That(inkeyChannelReference.IsSome, Is.True)
    Assert.That(createWindowReference.IsSome, Is.True)
    Assert.That(analyzed.Errors |> List.exists (fun error -> error.Contains("Unresolved reference 'fieldWidth3'") || error.Contains("Unresolved reference 'fieldwidth3'")), Is.False)
    Assert.That(analyzed.Errors |> List.exists (fun error -> error.Contains("Unresolved call 'fieldWidth3'") || error.Contains("Unresolved call 'fieldwidth3'")), Is.False)
    Assert.That(analyzed.Errors |> List.exists (fun error -> error.Contains("Unresolved reference 'fieldWidth4'") || error.Contains("Unresolved reference 'fieldWidth9'") || error.Contains("Unresolved reference 'fieldWidth10'")), Is.False)
