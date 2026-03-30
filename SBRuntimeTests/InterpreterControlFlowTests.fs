module SBRuntimeTests.InterpreterControlFlowTests

open NUnit.Framework

open SyntaxAst
open Interpreter
open SBRuntimeTests.TestSupport

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
                    StatementBlock [ Assignment(pos, id "sum", binary "+" (id "sum") (id "i")) ],
                    None) ])
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

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError EscapedReturn
    |> ignore

[<Test>]
let ``interpreter goto missing target reports runtime error`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GotoStmt(pos, num "999") ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError MissingGotoTarget
    |> ignore

[<Test>]
let ``interpreter gosub missing target reports runtime error`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "999") ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError MissingGosubTarget
    |> ignore

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
let ``interpreter goto can jump out of for body`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ ForStmt(
                    pos,
                    "i",
                    num "1",
                    num "3",
                    None,
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "100") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter goto can jump into for body line block`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(pos, Some 15, [ GotoStmt(pos, num "30") ])
              Line(
                pos,
                Some 20,
                [ ForStmt(
                    pos,
                    "i",
                    num "1",
                    num "1",
                    None,
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "50") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter gosub from for body returns and loop continues`` () =
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
                    num "2",
                    None,
                    LineBlock
                        [ Line(pos, Some 30, [ GosubStmt(pos, num "100") ])
                          Line(pos, Some 40, [ Assignment(pos, id "sum", binary "+" (id "sum") (num "10")) ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "sum" ]) ])
              Line(pos, Some 60, [ GotoStmt(pos, num "200") ])
              Line(pos, Some 100, [ Assignment(pos, id "sum", binary "+" (id "sum") (id "i")) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("23"))

[<Test>]
let ``interpreter goto can jump out of repeat body`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ RepeatStmt(
                    pos,
                    "loop",
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "100") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter goto can jump into repeat body line block`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(pos, Some 15, [ GotoStmt(pos, num "30") ])
              Line(
                pos,
                Some 20,
                [ RepeatStmt(
                    pos,
                    "loop",
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "50") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter gosub from repeat body returns and loop can exit`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ RepeatStmt(
                    pos,
                    "loop",
                    LineBlock
                        [ Line(pos, Some 30, [ GosubStmt(pos, num "100") ])
                          Line(pos, Some 40, [ ExitStmt(pos, "loop") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ])
              Line(pos, Some 60, [ GotoStmt(pos, num "200") ])
              Line(pos, Some 100, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter goto can jump into if line block`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(pos, Some 15, [ GotoStmt(pos, num "30") ])
              Line(
                pos,
                Some 20,
                [ IfStmt(
                    pos,
                    num "1",
                    LineBlock [ Line(pos, Some 30, [ Assignment(pos, id "count", num "7") ]) ],
                    Some(LineBlock [ Line(pos, Some 40, [ Assignment(pos, id "count", num "9") ]) ])) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("7"))

[<Test>]
let ``interpreter goto can jump out of if line block`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ IfStmt(
                    pos,
                    num "1",
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", num "1") ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "100") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter gosub from if body returns to branch flow`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ IfStmt(
                    pos,
                    num "1",
                    LineBlock
                        [ Line(pos, Some 30, [ GosubStmt(pos, num "100") ])
                          Line(pos, Some 40, [ Assignment(pos, id "count", binary "+" (id "count") (num "10")) ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ])
              Line(pos, Some 60, [ GotoStmt(pos, num "200") ])
              Line(pos, Some 100, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("11"))

[<Test>]
let ``interpreter return after goto inside gosub still returns to caller`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "100") ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ])
              Line(pos, Some 30, [ GotoStmt(pos, num "300") ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"sub\"" ]) ])
              Line(pos, Some 110, [ GotoStmt(pos, num "120") ])
              Line(pos, Some 120, [ ReturnStmt(pos, None) ])
              Line(pos, Some 300, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("sub|after"))
