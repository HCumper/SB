module SBRuntimeTests.InterpreterScopeTests

open NUnit.Framework

open SyntaxAst
open Interpreter
open SBRuntimeTests.TestSupport

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
                    [ Line(pos, Some 100, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
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
                    [ Line(pos, Some 100, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "7") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ],
                    None) ])
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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "9") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ],
                    None) ])
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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ ProcedureCall(pos, "leaf", []) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "11") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ]) ],
                    None) ])
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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "2") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", [ num "3" ]) ]) ],
                    None) ])
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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "scores", Some [ num "5" ] ]) ])
                      Line(pos, Some 210, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "1" ]), num "8") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ],
                    None) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "main", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("8"))

[<Test>]
let ``interpreter callee can write caller local and grandchild reads updated value`` () =
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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ Assignment(pos, id "score", num "17") ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "leaf", []) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "11") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ])
                      Line(pos, Some 330, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "root", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("17|17"))

[<Test>]
let ``interpreter callee can write caller local array element and grandchild reads updated value`` () =
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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "2" ]) ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "2" ]), num "23") ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "leaf", []) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "scores", Some [ num "5" ] ]) ])
                      Line(pos, Some 310, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "2" ]), num "7") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ])
                      Line(pos, Some 330, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "2" ]) ]) ]) ],
                    None) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "root", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("23|23"))

[<Test>]
let ``interpreter dynamic writes target nearest caller local before outer local or global`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "1") ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "leaf",
                    [],
                    [ Line(pos, Some 100, [ Assignment(pos, id "score", num "99") ])
                      Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "5") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "leaf", []) ])
                      Line(pos, Some 230, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 40,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "7") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ])
                      Line(pos, Some 330, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "root", []) ])
              Line(pos, Some 60, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("99|99|7|1"))

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
                      Line(pos, Some 120, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
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
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "bump", [ num "7" ]) ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError InvalidReferenceActual
    |> ignore

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
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", num "7") ])
              Line(pos, Some 30, [ ProcedureCall(pos, "bump", [ binary "+" (id "x") (num "0") ]) ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError InvalidReferenceActual
    |> ignore

[<Test>]
let ``interpreter accepts array element actual for reference parameter`` () =
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
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(pos, Some 20, [ DimStmt(pos, [ "scores", [ num "5" ] ]) ])
              Line(pos, Some 30, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "2" ]), num "9") ])
              Line(pos, Some 40, [ ProcedureCall(pos, "bump", [ mkPostfixName pos "scores" (Some [ num "2" ]) ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "2" ]) ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("10"))

[<Test>]
let ``interpreter supports mixed flexible and reference parameters in one routine`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "mix",
                    [ "value"; "total" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "total" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "value", binary "+" (id "value") (num "10")) ])
                      Line(pos, Some 120, [ Assignment(pos, id "total", binary "+" (id "total") (id "value")) ]) ],
                    None) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", num "2") ])
              Line(pos, Some 30, [ Assignment(pos, id "sum", num "5") ])
              Line(pos, Some 40, [ ProcedureCall(pos, "mix", [ binary "+" (id "x") (num "0"); id "sum" ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "x"; id "sum" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("2 17"))

[<Test>]
let ``interpreter repeated nested reference aliasing updates caller storage`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "inc",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "inc2",
                    [ "a" ],
                    [ Line(pos, Some 200, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "inc", [ id "a" ]) ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "inc", [ id "a" ]) ]) ],
                    None) ])
              Line(pos, Some 30, [ Assignment(pos, id "x", num "4") ])
              Line(pos, Some 40, [ ProcedureCall(pos, "inc2", [ id "x" ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "x" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("6"))

[<Test>]
let ``interpreter reference parameter shadows globals and caller locals`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "1") ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "leaf",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "show",
                    [ "score" ],
                    [ Line(pos, Some 200, [ ReferenceStmt(pos, [ id "score" ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "99") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "leaf", []) ])
                      Line(pos, Some 230, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 40,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "7") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "show", [ id "score" ]) ])
                      Line(pos, Some 330, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "root", []) ])
              Line(pos, Some 60, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("99|99|99|1"))
