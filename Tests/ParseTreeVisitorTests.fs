module Tests.ParseTreeVisitor

open Xunit
open Types
open ParseTreeVisitor
open Antlr4.Runtime

let private parseInput (input: string) =
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)
    parser.expr()

[<Fact>]
let ``convertTreeToAst handles nested expressions correctly`` () =
    let testTree = parseInput "x+3"
    let ast = convertTreeToAst testTree
    Assert.Equal(1, ast.Length)
    Assert.Equal(NodeKind.BinaryExpr, ast[0].Kind)
    Assert.Equal(NodeKind.Identifier, ast[0].Children[0].Kind)
    Assert.Equal(NodeKind.NumberLiteral, ast[0].Children[1].Kind)

[<Fact>]
let ``convertAssignmentToAst handles assignment expressions correctly`` () =
    let testTree = parseInput "y=x+3"
    let ast = convertTreeToAst testTree
    Assert.Equal(1, ast.Length)
    Assert.Equal(2, ast[0].Children.Length)
    Assert.Equal(NodeKind.Identifier, ast[0].Children[0].Kind)
    Assert.Equal("y", ast[0].Children[0].Value)
    Assert.Equal("3", ast[0].Children[1].Children[1].Value)
