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
          ExpressionFacts = []
          Diagnostics = []
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
let ``semantic diagnostics expose structured code scope and position`` () =
    let analyzed = analyzeProgram "10 PRINT missing\n"

    let unresolvedReference =
        analyzed.Diagnostics
        |> List.tryFind (fun diagnostic -> diagnostic.Code = SemanticDiagnosticCode.UnresolvedReference)

    match unresolvedReference with
    | Some diagnostic ->
        Assert.That(diagnostic.Message, Does.Contain("Unresolved reference 'missing'"))
        Assert.That(diagnostic.Scope, Is.EqualTo(globalScope))
        Assert.That(diagnostic.SymbolName, Is.EqualTo(Some "missing"))
        Assert.That(diagnostic.Position |> Option.map (fun p -> p.EditorLineNo), Is.EqualTo(Some 1))
    | None -> Assert.Fail("Expected unresolved reference diagnostic")

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

[<Test>]
let ``assignment infers symbol types from expression results`` () =
    let analyzed =
        analyzeProgram "10 total = 1 + 2.5\n20 name$ = \"a\" & \"b\"\n"

    Assert.That(analyzed.Errors, Is.Empty)

    match analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "total"] with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.Real))
    | other -> Assert.Fail($"Expected variable symbol for total, got %A{other}")

    match analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "name$"] with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.String))
    | other -> Assert.Fail($"Expected variable symbol for name$, got %A{other}")

    let additionResult =
        analyzed.ExpressionFacts
        |> List.tryFind (fun fact ->
            fact.Kind = ExpressionResult
            && fact.Name = "+"
            && fact.Position.EditorLineNo = 1
            && fact.EvaluatedType = Some SBType.Real
            && fact.ValueText = Some "3.5")

    let concatResult =
        analyzed.ExpressionFacts
        |> List.tryFind (fun fact ->
            fact.Kind = ExpressionResult
            && fact.Name = "&"
            && fact.Position.EditorLineNo = 2
            && fact.EvaluatedType = Some SBType.String
            && fact.ValueText = Some "\"ab\"")

    Assert.That(additionResult.IsSome, Is.True)
    Assert.That(concatResult.IsSome, Is.True)

[<Test>]
let ``single assignment to foldable expression records constant valued symbol`` () =
    let analyzed =
        analyzeProgram "10 base = 1 + 2\n20 total = base + 4\n"

    Assert.That(analyzed.Errors, Is.Empty)

    match analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "base"] with
    | VariableSym sym ->
        Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.Integer))
        Assert.That(sym.ValueText, Is.EqualTo(Some "3"))
    | other -> Assert.Fail($"Expected variable symbol for base, got %A{other}")

    let foldedUse =
        analyzed.ExpressionFacts
        |> List.tryFind (fun fact ->
            fact.Kind = ExpressionResult
            && fact.Name = "+"
            && fact.Position.EditorLineNo = 2
            && fact.ValueText = Some "7")

    Assert.That(foldedUse.IsSome, Is.True)

[<Test>]
let ``reassignment clears constant valued symbol state`` () =
    let analyzed =
        analyzeProgram "10 base = 3\n20 base = RND(1)\n30 total = base + 4\n"

    match analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "base"] with
    | VariableSym sym -> Assert.That(sym.ValueText, Is.EqualTo(None))
    | other -> Assert.Fail($"Expected variable symbol for base, got %A{other}")

    let laterUse =
        analyzed.ExpressionFacts
        |> List.tryFind (fun fact ->
            fact.Kind = ExpressionResult
            && fact.Name = "+"
            && fact.Position.EditorLineNo = 3)

    match laterUse with
    | Some fact -> Assert.That(fact.ValueText, Is.EqualTo(None))
    | None -> Assert.Fail("Expected expression result fact for later non-folded addition")

[<Test>]
let ``dim uses folded constant expressions for array bounds`` () =
    let analyzed =
        analyzeProgram "10 DIM score(1+2)\n20 PRINT score(3)\n"

    Assert.That(analyzed.Errors, Is.Empty)

    match analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "score"] with
    | ArraySym sym ->
        Assert.That(sym.Dimensions.Length, Is.EqualTo(1))
        Assert.That(sym.Dimensions[0], Is.EqualTo(3))
    | other -> Assert.Fail($"Expected array symbol for score, got %A{other}")

[<Test>]
let ``assignment rejects function call result as non writable target`` () =
    let analyzed =
        analyzeProgram "10 DEFine FuNction twice(x)\n20 RETurn x*2\n30 END DEFine\n40 twice(2)=10\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Function 'twice' is not a writable assignment target"))

[<Test>]
let ``assignment rejects built in call result as non writable target`` () =
    let analyzed =
        analyzeProgram "10 LEN(\"abc\")=1\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Built-in 'LEN' is not a writable assignment target"))

[<Test>]
let ``assignment rejects built in constant like target`` () =
    let analyzed =
        analyzeProgram "10 PI=3\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Built-in 'PI' is not a writable assignment target"))

[<Test>]
let ``procedure cannot be used in expression context`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure show(x)\n20 END DEFine\n30 total = show(1)\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Procedure 'show' cannot be used in expression context"))

[<Test>]
let ``function cannot be used as a statement call`` () =
    let analyzed =
        analyzeProgram "10 DEFine FuNction add(a,b)\n20 RETurn a+b\n30 END DEFine\n40 add 1,2\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Function 'add' cannot be used as a statement call"))

[<Test>]
let ``assignment reports incompatible string and numeric types`` () =
    let analyzed =
        analyzeProgram "10 name$ = 1 + 2\n20 total = \"abc\"\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Cannot assign Integer expression to String target"))

[<Test>]
let ``superbasic arithmetic coerces numeric strings for plus`` () =
    let analyzed =
        analyzeProgram "10 total = \"3\" + \"4\"\n"

    Assert.That(analyzed.Errors, Is.Empty)

    let additionResult =
        analyzed.ExpressionFacts
        |> List.tryFind (fun fact ->
            fact.Kind = ExpressionResult
            && fact.Name = "+"
            && fact.Position.EditorLineNo = 1)

    match additionResult with
    | Some fact ->
        Assert.That(fact.EvaluatedType, Is.EqualTo(Some SBType.Real))
        Assert.That(fact.ValueText, Is.EqualTo(Some "7"))
    | None -> Assert.Fail("Expected expression result fact for numeric string addition")

[<Test>]
let ``superbasic arithmetic reports non coercible string operands`` () =
    let analyzed =
        analyzeProgram "10 total = \"A\" + 1\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Operator '+' cannot coerce operand"))

[<Test>]
let ``if condition rejects string expressions`` () =
    let analyzed =
        analyzeProgram "10 IF \"abc\" THEN PRINT 1\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Condition expression must be numeric"))

[<Test>]
let ``scalar indexed use is rejected`` () =
    let analyzed =
        analyzeProgram "10 score = 1\n20 PRINT score(1)\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Scalar 'score' cannot be indexed"))

[<Test>]
let ``array scalar use is rejected`` () =
    let analyzed =
        analyzeProgram "10 DIM score(10)\n20 PRINT score\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Array 'score' used without index"))

[<Test>]
let ``procedure call arity is validated from declared parameters`` () =
    let analyzed =
        analyzeProgram "10 DEFine PROCedure show(x,y)\n20 END DEFine\n30 show 1\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Call to 'show' expects 2 argument(s) but received 1"))

[<Test>]
let ``function call arity is validated in expression context`` () =
    let analyzed =
        analyzeProgram "10 DEFine FuNction add(a,b)\n20 RETURN a+b\n30 END DEFine\n40 total = add(1)\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Call to 'add' expects 2 argument(s) but received 1"))

[<Test>]
let ``fixed arity built in calls are validated where modeled`` () =
    let analyzed =
        analyzeProgram "10 value = LEN(\"abc\",1)\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Call to 'LEN' expects 1 argument(s) but received 2"))

[<Test>]
let ``built in string arguments are validated where modeled`` () =
    let analyzed =
        analyzeProgram "10 value = LEN(1)\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Built-in 'LEN' argument 1 must be string-compatible"))

[<Test>]
let ``built in numeric arguments reject non coercible constant strings`` () =
    let analyzed =
        analyzeProgram "10 value = ABS(\"A\")\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Built-in 'ABS' argument 1 must be numeric-compatible"))

[<Test>]
let ``input arguments must be writable`` () =
    let analyzed =
        analyzeProgram "10 INPUT 1\n"

    Assert.That(analyzed.Errors, Has.Some.Contains("Built-in 'INPUT' argument 1 must be writable"))

[<Test>]
let ``function return and call expressions propagate inferred string type`` () =
    let analyzed =
        analyzeProgram "10 DEFine FuNction twice$(value$)\n20 RETURN value$ & value$\n30 END DEFine\n40 result$ = twice$(\"a\")\n"

    Assert.That(analyzed.Errors, Is.Empty)

    match analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "twice$"] with
    | FunctionSym sym -> Assert.That(sym.ReturnType, Is.EqualTo(SBType.String))
    | other -> Assert.Fail($"Expected function symbol for twice$, got %A{other}")

    match analyzed.SymTab[globalScope].Symbols[normalizeIdentifier "result$"] with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.String))
    | other -> Assert.Fail($"Expected variable symbol for result$, got %A{other}")

    let returnConcat =
        analyzed.ExpressionFacts
        |> List.tryFind (fun fact ->
            fact.Kind = ExpressionResult
            && fact.Name = "&"
            && fact.Position.EditorLineNo = 2
            && fact.EvaluatedType = Some SBType.String)

    let functionCallResult =
        analyzed.ExpressionFacts
        |> List.tryFind (fun fact ->
            fact.Kind = ExpressionResult
            && fact.Name = "twice$(...)"
            && fact.Position.EditorLineNo = 4
            && fact.EvaluatedType = Some SBType.String)

    Assert.That(returnConcat.IsSome, Is.True)
    Assert.That(functionCallResult.IsSome, Is.True)
