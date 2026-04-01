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

[<Test>]
let ``when_error alias parses as when error`` () =
    let testTree =
        parseProgram
            "10 WHEN_ERROR\n20 PRINT \"oops\"\n30 END WHEN\n"

    let ast = convertTreeToAst testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ WhenStmt(_, None, [ Line(_, Some 20, [ ProcedureCall(_, "PRINT", [ StringLiteral(_, _, "\"oops\"") ]) ]) ]) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected WHEN_ERROR AST: %A{other}")

[<Test>]
let ``double caret parses as xor`` () =
    let testTree =
        parseProgram
            "10 IF a ^^ b\n20 PRINT \"x\"\n30 END IF\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ IfStmt(_, BinaryExpr(_, _, "XOR", Identifier(_, _, "a"), Identifier(_, _, "b")), _, _) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected XOR AST: %A{other}")

[<Test>]
let ``print to channel parses as channel procedure call`` () =
    let testTree =
        parseProgram
            "10 PRINT TO 4;\"hello\"\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ ChannelProcedureCall(_, "PRINT", NumberLiteral(_, _, "4"), [ StringLiteral(_, _, "\"hello\"") ]) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected PRINT TO AST: %A{other}")

[<Test>]
let ``empty multiline if parses`` () =
    let testTree =
        parseProgram
            "10 IF ready\n20 END IF\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(_, [ Line(_, Some 10, [ IfStmt(_, Identifier(_, _, "ready"), LineBlock [], None) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected empty IF AST: %A{other}")

[<Test>]
let ``multiline if allows numbered final body line with end if suffix`` () =
    let testTree =
        parseProgram
            "10 IF Invalid_Address(link_ptr)\n12518 para_base = ALLOCATION(comms_area_sz,0,0)\n12546 EXIT lp : END IF\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ IfStmt(
                _,
                PostfixName(_, _, "Invalid_Address", Some [ Identifier(_, _, "link_ptr") ]),
                LineBlock
                    [ Line(_, Some 12518, [ Assignment(_, Identifier(_, _, "para_base"), PostfixName(_, _, "ALLOCATION", Some _)) ])
                      Line(_, Some 12546, [ ExitStmt(_, "lp") ]) ],
                None) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected multiline IF terminator AST: %A{other}")

[<Test>]
let ``turbo directives are ignored as remarks`` () =
    let testTree =
        parseProgram
            "10 TURBO_model '>'\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(_, [ Line(_, Some 10, [ Remark(_, text) ]) ]) ->
        Assert.That(text, Does.Contain("TURBO_model"))
    | other -> Assert.Fail($"Unexpected TURBO directive AST: %A{other}")

[<Test>]
let ``implicitb implicitd implicitl parse as implicit statements`` () =
    let testTree =
        parseProgram
            "10 IMPLICITB flag\n20 IMPLICITD ratio\n30 IMPLICITL count\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(
        _,
        [ Line(_, Some 10, [ ImplicitStmt(_, "B", [ "flag" ]) ])
          Line(_, Some 20, [ ImplicitStmt(_, "D", [ "ratio" ]) ])
          Line(_, Some 30, [ ImplicitStmt(_, "L", [ "count" ]) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected IMPLICITBDL AST: %A{other}")

[<Test>]
let ``identifiers may start with underscore`` () =
    let testTree =
        parseProgram
            "10 _name = 1\n20 PRINT _name\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(
        _,
        [ Line(_, Some 10, [ Assignment(_, Identifier(_, _, "_name"), NumberLiteral(_, _, "1")) ])
          Line(_, Some 20, [ ProcedureCall(_, "PRINT", [ Identifier(_, _, "_name") ]) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected underscore identifier AST: %A{other}")

[<Test>]
let ``double pipe parses as or`` () =
    let testTree =
        parseProgram
            "10 IF a || b\n20 PRINT \"x\"\n30 END IF\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ IfStmt(_, BinaryExpr(_, _, "OR", Identifier(_, _, "a"), Identifier(_, _, "b")), _, _) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected double-pipe AST: %A{other}")

[<Test>]
let ``manifest parses as constant declaration list`` () =
    let testTree =
        parseProgram
            "10 MANIFEST : TRUE = 1 : FALSE = 0\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ ManifestStmt(_, [ ("TRUE", NumberLiteral(_, _, "1")); ("FALSE", NumberLiteral(_, _, "0")) ]) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected MANIFEST AST: %A{other}")

[<Test>]
let ``hex literals parse as number literals`` () =
    let testTree =
        parseProgram
            "10 DATA $5387,$FFFFB287\n"

    let ast = convertTreeToAst testTree

    match ast[0] with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ DataStmt(_, [ NumberLiteral(_, _, "$5387"); NumberLiteral(_, _, "$FFFFB287") ]) ]) ]) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected hex DATA AST: %A{other}")


