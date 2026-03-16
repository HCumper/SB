module TestBed.ParseTreeVisitorTests

open Antlr4.Runtime
open NUnit.Framework

open ParseTreeVisitor
open SyntaxAst

let private parseInput (input: string) =
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)
    parser.expr()

[<Test>]
let ``convertTreeToAst handles nested expressions correctly`` () =
    let testTree = parseInput "x+3"
    let ast = convertTreeToExpr testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | BinaryExpr(_, "+", Identifier _, NumberLiteral _) -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected AST: %A{other}")

[<Test>]
let ``convertAssignmentToAst handles equality expressions correctly`` () =
    let testTree = parseInput "y=x+3"
    let ast = convertTreeToExpr testTree
    Assert.That(ast, Has.Length.EqualTo(1))

    match ast[0] with
    | BinaryExpr(_, "=", Identifier(_, "y"), BinaryExpr(_, "+", Identifier(_, "x"), NumberLiteral(_, "3"))) ->
        Assert.Pass()
    | other -> Assert.Fail($"Unexpected AST: %A{other}")
