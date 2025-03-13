module Tests.ParseTreeVisitor

open Xunit
open Utility
open ParseTreeVisitor
open Antlr4.Runtime

// Import the generated parser and lexer (ensure the compiled DLL is referenced)
//open Generated // Change this based on the actual generated namespace

/// Function to parse a hardcoded input string
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
    Assert.Equal(NodeKind.BinaryExpr, ast[0].TokenType)
    Assert.Equal(NodeKind.Identifier, ast[0].Children[0].TokenType)
    Assert.Equal(NodeKind.NumberLiteral, ast[0].Children[1].TokenType)

[<Fact>]
let ``convertAssignmentToAst handles assignment expressions correctly`` () =
    let testTree = parseInput "y=x+3" 
    let ast = convertTreeToAst testTree
    Assert.Equal(1, ast.Length)
    Assert.Equal(2, ast[0].Children.Length)
    Assert.Equal(NodeKind.Identifier, ast[0].Children[0].TokenType)
    Assert.Equal("y", ast[0].Children[0].Value)
    Assert.Equal("3", ast[0].Children[1].Children[1].Value)

// [<Fact>]
// let ``convertTreeToAst discards nodes with Discard behavior`` () =
//     let parseTree = parseInput "ENDDEF"// create a mock parse tree with nodes that should be discarded
//     let ast = convertTreeToAst parseTree
//     Assert.Empty(ast)

// [<Fact>]
// let ``convertTreeToAst bubbles up child nodes correctly`` () =
//     let parseTree = // create a mock parse tree for an expression that should bubble up
//     let ast = convertTreeToAst parseTree
//     Assert.Equal(1, ast.Length)
//     Assert.Equal(NodeKind.BinaryExpr, ast.[0].TokenType)
//
// [<Fact>]
// let ``convertTreeToAst handles terminal nodes correctly`` () =
//     let parseTree = // create a mock parse tree with terminal nodes
//     let ast = convertTreeToAst parseTree
//     Assert.Equal(2, ast.Length)
//     Assert.Equal(NodeKind.NumberLiteral, ast.[0].TokenType)
//     Assert.Equal(NodeKind.StringLiteral, ast.[1].TokenType)
//
// [<Fact>]
// let ``convertTreeToAst handles error nodes correctly`` () =
//     let parseTree = // create a mock parse tree with error nodes
//     let ast = convertTreeToAst parseTree
//     Assert.Equal(1, ast.Length)
//     Assert.Equal(NodeKind.Unknown, ast.[0].TokenType)
