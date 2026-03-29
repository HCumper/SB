module TestBed.InterpreterTests

open System
open System.IO
open Antlr4.Runtime
open NUnit.Framework

open Types
open SyntaxAst
open CompilerPipeline
open AstToHir
open Interpreter
open ParseTreeVisitor

let private pos =
    { BasicLineNo = None
      EditorLineNo = 1
      Column = 0 }

let private num value = mkNumberLiteral pos value
let private str value = mkStringLiteral pos value
let private id name = mkIdentifier pos name
let private call name args = mkPostfixName pos name (Some args)
let private binary op lhs rhs = mkBinaryExpr pos op lhs rhs

let private fixturePath fileName =
    let candidates =
        [ Path.Combine(__SOURCE_DIRECTORY__, "..", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", Path.GetFileName(fileName)) ]
        |> List.map Path.GetFullPath

    match candidates |> List.tryFind File.Exists with
    | Some path -> path
    | None -> failwith $"Fixture file not found: {fileName}"

let private parseAstFromFile fileName =
    let input = File.ReadAllText(fixturePath fileName)
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)

    parser.program()
    |> convertTreeToAst
    |> List.head

let private lowerProgram ast =
    let analyzed = runSemanticAnalysis ast
    match lowerToHir analyzed with
    | Result.Ok hir -> hir
    | Result.Error errs -> Assert.Fail($"Expected HIR lowering to succeed, got %A{errs}"); Unchecked.defaultof<_>

let private runProgramWithInput input ast =
    let hir = lowerProgram ast
    let outputs = ResizeArray<string>()
    let inputs = Collections.Generic.Queue<string>(input : string list)
    let options =
        { defaultRuntimeOptions with
            InputProvider = fun () -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
            OutputWriter = fun line -> outputs.Add(line)
            Random = Random(1234)
            Clock = fun () -> DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc) }

    match interpretProgramWithOptions options hir with
    | Result.Ok result -> result.Output
    | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}"); []

let private runProgram ast =
    runProgramWithInput [] ast

[<Test>]
let ``interpreter evaluates arithmetic and print`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "x", binary "+" (num "1") (num "2")) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ id "x" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("3"))

[<Test>]
let ``interpreter calls user functions`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ FunctionDef(
                    pos,
                    "add1",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReturnStmt(pos, Some(binary "+" (id "a") (num "1"))) ]) ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", call "add1" [ num "3" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "x" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("4"))

[<Test>]
let ``interpreter aliases bare variable procedure arguments without reference`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", num "3") ])
              Line(pos, Some 30, [ ProcedureCall(pos, "bump", [ id "x" ]) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "PRINT", [ id "x" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("4"))

[<Test>]
let ``interpreter aliases bare array element procedure arguments without reference`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ]) ])
              Line(pos, Some 20, [ DimStmt(pos, [ "scores", [ num "5" ] ]) ])
              Line(pos, Some 30, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "1" ]), num "3") ])
              Line(pos, Some 40, [ ProcedureCall(pos, "bump", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("4"))

[<Test>]
let ``interpreter local is dynamically visible to called procedure`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ]) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "7") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "main", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("7"))

[<Test>]
let ``interpreter local shadows global in called procedure`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "3") ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ]) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "9") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ]) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "main", []) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("9|3"))

[<Test>]
let ``interpreter dynamic local lookup walks multiple caller levels`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "leaf",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ]) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ ProcedureCall(pos, "leaf", []) ]) ]) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "11") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ]) ]) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "root", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("11"))

[<Test>]
let ``interpreter parameter shadows caller local which shadows global`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "1") ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "show",
                    [ "score" ],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ]) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "2") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", [ num "3" ]) ]) ]) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "main", []) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("3|1"))

[<Test>]
let ``interpreter local array is dynamically visible to called procedure`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ]) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "scores", Some [ num "5" ] ]) ])
                      Line(pos, Some 210, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "1" ]), num "8") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "main", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("8"))

[<Test>]
let ``interpreter locals are isolated across repeated procedure calls`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 100, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "score", binary "+" (id "score") (num "1")) ])
                      Line(pos, Some 120, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "main", []) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "main", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1|1"))

[<Test>]
let ``interpreter rejects literal actual for reference parameter`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "bump", [ num "7" ]) ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected literal actual for REFERENCE parameter to fail")
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(InvalidReferenceActual))
        Assert.That(err.Message, Does.StartWith("Runtime error [InvalidReferenceActual]"))

[<Test>]
let ``interpreter rejects expression actual for reference parameter`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", num "7") ])
              Line(pos, Some 30, [ ProcedureCall(pos, "bump", [ binary "+" (id "x") (num "0") ]) ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected expression actual for REFERENCE parameter to fail")
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(InvalidReferenceActual))
        Assert.That(err.Message, Does.StartWith("Runtime error [InvalidReferenceActual]"))

[<Test>]
let ``interpreter executes for loops`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "sum", num "0") ])
              Line(
                pos,
                Some 20,
                [ ForStmt(
                    pos,
                    "i",
                    num "1",
                    num "3",
                    None,
                    StatementBlock [ Assignment(pos, id "sum", binary "+" (id "sum") (id "i")) ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "sum" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("6"))

[<Test>]
let ``interpreter goto jumps to numbered line target`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GotoStmt(pos, num "30") ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ str "\"right\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("right"))

[<Test>]
let ``interpreter gosub returns to following statement`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "100") ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ])
              Line(pos, Some 30, [ GotoStmt(pos, num "200") ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"sub\"" ]) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("sub|after"))

[<Test>]
let ``interpreter nested gosub returns in stack order`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "100") ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"done\"" ]) ])
              Line(pos, Some 30, [ GotoStmt(pos, num "300") ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"outer\"" ]) ])
              Line(pos, Some 110, [ GosubStmt(pos, num "200") ])
              Line(pos, Some 120, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ ProcedureCall(pos, "PRINT", [ str "\"inner\"" ]) ])
              Line(pos, Some 210, [ ReturnStmt(pos, None) ])
              Line(pos, Some 300, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("outer|inner|done"))

[<Test>]
let ``interpreter return without active gosub reports runtime error`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ReturnStmt(pos, None) ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected RETURN without active GOSUB to fail")
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(EscapedReturn))
        Assert.That(err.Message, Does.StartWith("Runtime error [EscapedReturn]"))

[<Test>]
let ``interpreter goto missing target reports runtime error`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GotoStmt(pos, num "999") ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected missing GOTO target to fail")
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(MissingGotoTarget))
        Assert.That(err.Message, Does.StartWith("Runtime error [MissingGotoTarget]"))

[<Test>]
let ``interpreter gosub missing target reports runtime error`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "999") ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected missing GOSUB target to fail")
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(MissingGosubTarget))
        Assert.That(err.Message, Does.StartWith("Runtime error [MissingGosubTarget]"))

[<Test>]
let ``interpreter on goto selects target by one based index`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGotoStmt(pos, num "2", [ num "100"; num "200"; num "300" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"one\"" ]) ])
              Line(pos, Some 110, [ GotoStmt(pos, num "400") ])
              Line(pos, Some 200, [ ProcedureCall(pos, "PRINT", [ str "\"two\"" ]) ])
              Line(pos, Some 210, [ GotoStmt(pos, num "400") ])
              Line(pos, Some 300, [ ProcedureCall(pos, "PRINT", [ str "\"three\"" ]) ])
              Line(pos, Some 400, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("two"))

[<Test>]
let ``interpreter on goto selector zero falls through`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGotoStmt(pos, num "0", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"fallthrough\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 200, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("fallthrough|wrong|wrong"))

[<Test>]
let ``interpreter on goto selector out of range falls through`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGotoStmt(pos, num "3", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"fallthrough\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("fallthrough"))

[<Test>]
let ``interpreter on gosub selects target and returns to caller flow`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGosubStmt(pos, num "2", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ])
              Line(pos, Some 30, [ GotoStmt(pos, num "300") ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"one\"" ]) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ ProcedureCall(pos, "PRINT", [ str "\"two\"" ]) ])
              Line(pos, Some 210, [ ReturnStmt(pos, None) ])
              Line(pos, Some 300, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("two|after"))

[<Test>]
let ``interpreter on gosub selector zero falls through`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGosubStmt(pos, num "0", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("after"))

[<Test>]
let ``interpreter on gosub selector out of range falls through`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGosubStmt(pos, num "3", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("after"))

[<Test>]
let ``interpreter restore missing target reports runtime error code`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ DataStmt(pos, [ num "1" ]) ])
              Line(pos, Some 20, [ RestoreStmt(pos, Some(num "999")) ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected missing RESTORE target to fail")
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(InvalidRestoreTarget))
        Assert.That(err.Message, Does.StartWith("Runtime error [InvalidRestoreTarget]"))

[<Test>]
let ``interpreter missing dynamic local reports runtime error code`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "show", []) ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected missing dynamic local to fail")
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(MissingDynamicStorageCell))
        Assert.That(err.Message, Does.StartWith("Runtime error [MissingDynamicStorageCell]"))

[<Test>]
let ``interpreter runtime errors prefer BASIC line numbers in messages`` () =
    let posWithBasicLine =
        { pos with
            BasicLineNo = Some 120
            EditorLineNo = 9
            Column = 3 }

    let ast =
        Program(
            posWithBasicLine,
            [ Line(
                posWithBasicLine,
                Some 120,
                [ ProcedureDef(
                    posWithBasicLine,
                    "show",
                    [],
                    [ Line(posWithBasicLine, Some 130, [ ProcedureCall(posWithBasicLine, "PRINT", [ mkIdentifier posWithBasicLine "score" ]) ]) ]) ])
              Line(posWithBasicLine, Some 140, [ ProcedureCall(posWithBasicLine, "show", []) ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected missing dynamic local to fail")
    | Result.Error err ->
        Assert.That(err.Message, Does.Contain("at BASIC line 120"))

[<Test>]
let ``interpreter runtime errors fall back to editor line and column in messages`` () =
    let posWithoutBasicLine =
        { pos with
            EditorLineNo = 9
            Column = 3 }

    let ast =
        Program(
            posWithoutBasicLine,
            [ Line(
                posWithoutBasicLine,
                Some 10,
                [ ProcedureDef(
                    posWithoutBasicLine,
                    "bump",
                    [ "a" ],
                    [ Line(posWithoutBasicLine, Some 20, [ ReferenceStmt(posWithoutBasicLine, [ mkIdentifier posWithoutBasicLine "a" ]) ])
                      Line(posWithoutBasicLine, Some 30, [ Assignment(posWithoutBasicLine, mkIdentifier posWithoutBasicLine "a", binary "+" (mkIdentifier posWithoutBasicLine "a") (num "1")) ]) ]) ])
              Line(posWithoutBasicLine, Some 40, [ ProcedureCall(posWithoutBasicLine, "bump", [ num "7" ]) ]) ])

    let hir = lowerProgram ast

    match interpretProgramWithOptions defaultRuntimeOptions hir with
    | Result.Ok _ -> Assert.Fail("Expected invalid REFERENCE actual to fail")
    | Result.Error err ->
        Assert.That(err.Message, Does.Contain("at editor line 9, column 3"))

[<Test>]
let ``interpreter reads data and restores by line`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 20, [ DataStmt(pos, [ num "1"; num "2" ]) ])
              Line(pos, Some 30, [ ReadStmt(pos, [ id "a" ]) ])
              Line(pos, Some 40, [ ReadStmt(pos, [ id "b" ]) ])
              Line(pos, Some 50, [ RestoreStmt(pos, Some(num "20")) ])
              Line(pos, Some 60, [ ReadStmt(pos, [ id "c" ]) ])
              Line(pos, Some 70, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b"; id "c" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1 2 1"))

[<Test>]
let ``interpreter handles input prompts and targets`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "INPUT", [ str "\"Enter\""; id "n" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ id "n" ]) ]) ])

    let output = runProgramWithInput [ "42" ] ast

    Assert.That(String.concat "|" output, Is.EqualTo("Enter|42"))

[<Test>]
let ``q3 fixture runtime completes sort check without inversion output`` () =
    let ast = parseAstFromFile "q3.SB"
    let hir = lowerProgram ast
    let outputs = ResizeArray<string>()
    let inputs = Collections.Generic.Queue<string>([ "25"; "" ])
    let options =
        { defaultRuntimeOptions with
            InputProvider = fun () -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
            OutputWriter = fun line -> outputs.Add(line)
            Random = Random(1234)
            Clock = fun () -> DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc) }

    match interpretProgramWithOptions options hir with
    | Result.Ok result ->
        Assert.That(result.Output |> List.exists (fun line -> line.StartsWith("Error at")), Is.False)
    | Result.Error err ->
        Assert.Fail($"Expected q3 fixture interpretation to succeed, got %A{err}")
