module TestBed.AstVerificationTests

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
    | Program(_, [ Line(_, Some 10, [ ProcedureDef(_, "main", [ "paramtype" ], [ Line(_, Some 20, [ ProcedureCall(_, "PRINT", [ Identifier(_, "paramtype") ]) ]) ]) ]) ]) ->
        Assert.Pass()
    | other -> Assert.Fail($"Unexpected AST: %A{other}")

[<Test>]
let ``if statements preserve then and else branches separately`` () =
    let testTree = parseProgram "10 IF ptr = low : high = high-1 : ELSE : low = low+1\n"

    let ast = convertTreeToAst testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | Program(_, [ Line(_, Some 10, [ IfStmt(_, BinaryExpr(_, "=", Identifier(_, "ptr"), Identifier(_, "low")), StatementBlock [ Assignment(_, Identifier(_, "high"), BinaryExpr(_, "-", Identifier(_, "high"), NumberLiteral(_, "1"))) ], Some (StatementBlock [ Assignment(_, Identifier(_, "low"), BinaryExpr(_, "+", Identifier(_, "low"), NumberLiteral(_, "1"))) ])) ]) ]) ->
        Assert.Pass()
    | other -> Assert.Fail($"Unexpected AST: %A{other}")
