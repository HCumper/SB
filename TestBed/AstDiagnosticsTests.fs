module TestBed.AstDiagnosticsTests

open NUnit.Framework

open Types
open SyntaxAst
open AstDiagnostics

let private pos line column =
    { BasicLineNo = None
      EditorLineNo = line
      Column = column }

let private id line column name = mkIdentifier (pos line column) name
let private num line column value = mkNumberLiteral (pos line column) value
let private bin line column op lhs rhs = mkBinaryExpr (pos line column) op lhs rhs

let private sampleAst =
    Program(
        pos 1 0,
        [ Line(
            pos 1 0,
            Some 10,
            [ ProcedureDef(
                pos 1 0,
                "main",
                [ "paramtype" ],
                [ Line(
                    pos 2 0,
                    Some 20,
                    [ IfStmt(
                        pos 2 2,
                        bin 2 5 "=" (id 2 5 "x") (num 2 9 "3"),
                        StatementBlock [ ProcedureCall(pos 2 12, "PRINT", [ id 2 18 "paramtype" ]) ],
                        Some (StatementBlock [ Remark(pos 2 28, "done") ])) ]) ]) ]) ])

[<Test>]
let ``prettyPrintAst renders canonical structural output`` () =
    let rendered = prettyPrintAst sampleAst

    Assert.That(rendered, Does.StartWith("Program @1:0"))
    Assert.That(rendered, Does.Contain("ProcedureDef main(paramtype) @1:0"))
    Assert.That(rendered, Does.Contain("IfStmt @2:2"))
    Assert.That(rendered, Does.Contain("StatementBlock"))
    Assert.That(rendered, Does.Contain("ProcedureCall PRINT @2:12"))

[<Test>]
let ``serializeAst emits stable json structure`` () =
    let serialized = serializeAst sampleAst

    Assert.That(serialized, Does.Contain("\"kind\": \"Program\""))
    Assert.That(serialized, Does.Contain("\"kind\": \"ProcedureDef\""))
    Assert.That(serialized, Does.Contain("\"name\": \"main\""))
    Assert.That(serialized, Does.Contain("\"parameters\": ["))
    Assert.That(serialized, Does.Contain("\"kind\": \"IfStmt\""))
    Assert.That(serialized, Does.Contain("\"kind\": \"StatementBlock\""))
