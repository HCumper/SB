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

let private parseProgram (input: string) =
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)
    parser.program()

[<Test>]
let ``goto statements lower to dedicated ast nodes`` () =
    let ast =
        parseProgram "10 GO TO 100\n20 GOSUB 200\n30 ON x GOTO 100,200\n40 ON y GOSUB 300,400\n"
        |> convertTreeToAst
        |> List.head

    let (Program(_, lines)) = ast
    let stmts = lines |> List.collect (fun (Line(_, _, children)) -> children)
    match stmts with
    | [ GotoStmt(_, NumberLiteral(_, "100"))
        GosubStmt(_, NumberLiteral(_, "200"))
        OnGotoStmt(_, Identifier(_, "x"), [ NumberLiteral(_, "100"); NumberLiteral(_, "200") ])
        OnGosubStmt(_, Identifier(_, "y"), [ NumberLiteral(_, "300"); NumberLiteral(_, "400") ]) ] -> Assert.Pass()
    | other -> Assert.Fail($"Unexpected statements: %A{other}")
