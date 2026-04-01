module SBTests.AstVerificationTests

open Antlr4.Runtime
open NUnit.Framework

open ParseTreeVisitor
open SyntaxAst

let private parseProgram (input: string) =
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)
    parser.program()

[<Test>]
let ``procedure definitions preserve parameters and body structure`` () =
    let testTree =
        parseProgram "10 DEFine PROCedure main(paramtype)\n20 PRINT paramtype\n30 END DEFine\n"

    let ast = convertTreeToAst testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | Program(_, [ Line(_, Some 10, [ ProcedureDef(_, "main", [ "paramtype" ], [ Line(_, Some 20, [ ProcedureCall(_, "PRINT", [ Identifier(_, _, "paramtype") ]) ]) ], _, _) ]) ]) ->
        Assert.Pass()
    | other -> Assert.Fail($"Unexpected AST: %A{other}")

[<Test>]
let ``if statements preserve then and else branches separately`` () =
    let testTree = parseProgram "10 IF ptr = low : high = high-1 : ELSE : low = low+1\n"

    let ast = convertTreeToAst testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | Program(_, [ Line(_, Some 10, [ IfStmt(_, BinaryExpr(_, _, "=", Identifier(_, _, "ptr"), Identifier(_, _, "low")), StatementBlock [ Assignment(_, Identifier(_, _, "high"), BinaryExpr(_, _, "-", Identifier(_, _, "high"), NumberLiteral(_, _, "1"))) ], Some (StatementBlock [ Assignment(_, Identifier(_, _, "low"), BinaryExpr(_, _, "+", Identifier(_, _, "low"), NumberLiteral(_, _, "1"))) ])) ]) ]) ->
        Assert.Pass()
    | other -> Assert.Fail($"Unexpected AST: %A{other}")

[<Test>]
let ``compact if accepts else without colon after else and empty print statements`` () =
    let testTree = parseProgram "10 IF w1<13:sysmess (7):PRINT:ELSE sysmess (8):PRINT\n"

    let ast = convertTreeToAst testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ IfStmt(
                _,
                BinaryExpr(_, _, "<", Identifier(_, _, "w1"), NumberLiteral(_, _, "13")),
                StatementBlock
                    [ ProcedureCall(_, "sysmess", [ NumberLiteral(_, _, "7") ])
                      ProcedureCall(_, "PRINT", []) ],
                Some(
                    StatementBlock
                        [ ProcedureCall(_, "sysmess", [ NumberLiteral(_, _, "8") ])
                          ProcedureCall(_, "PRINT", []) ])) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected compact IF/ELSE AST: %A{other}")

[<Test>]
let ``compact select on clause accepts same line statement body`` () =
    let testTree =
        parseProgram
            "24020 SELect ON dummy\n24031 =1:obw=11\n24032 =2:obw=12\n24470 END SELect\n"

    let ast = convertTreeToAst testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 24020,
            [ SelectStmt(
                _,
                Identifier(_, _, "dummy"),
                [ SelectClause(_, Identifier(_, _, "dummy"), NumberLiteral(_, _, "1"), Some(StatementBlock [ Assignment(_, Identifier(_, _, "obw"), NumberLiteral(_, _, "11")) ]))
                  SelectClause(_, Identifier(_, _, "dummy"), NumberLiteral(_, _, "2"), Some(StatementBlock [ Assignment(_, Identifier(_, _, "obw"), NumberLiteral(_, _, "12")) ])) ]) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected compact SELECT inline-body AST: %A{other}")

[<Test>]
let ``implicit channel syntax is preserved distinctly from ordinary arguments`` () =
    let testTree = parseProgram "10 DIR \\files\n20 DIR files\n"

    let ast = convertTreeToAst testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | Program(
        _,
        [ Line(_, Some 10, [ ImplicitChannelProcedureCall(_, "DIR", StringLiteral(_, _, "\"files\""), []) ])
          Line(_, Some 20, [ ProcedureCall(_, "DIR", [ Identifier(_, _, "files") ]) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected implicit channel AST: %A{other}")


