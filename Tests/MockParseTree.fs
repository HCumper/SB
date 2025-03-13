module Tests.MockParseTree

open Moq
open Antlr4.Runtime
open Antlr4.Runtime.Tree

// Helper function to mock IParseTree nodes
let mockParseTree (text: string) (children: IParseTree list) =
    let mock = Mock<IParseTree>()
    mock.Setup(fun t -> t.GetText()).Returns(text) |> ignore
    mock.Setup(fun t -> t.ChildCount).Returns(children.Length) |> ignore
    mock.Setup(fun t -> t.GetChild(It.IsAny<int>()))
        .Returns<int>(fun i -> children.[i])
        |> ignore
    mock.Object

// Mock leaf nodes (tokens)
let xToken = mockParseTree "x" []
let yToken = mockParseTree "y" []
let num2Token = mockParseTree "2" []
let num3Token = mockParseTree "3" []
let plusToken = mockParseTree "+" []
let starToken = mockParseTree "*" []
let equalsToken = mockParseTree "=" []
let lParenToken = mockParseTree "(" []
let rParenToken = mockParseTree ")" []

// Mock inner nodes (expressions)
let additionExpr =
    mockParseTree "y+2" [
        yToken
        plusToken
        num2Token
    ]

let parenExpr =
    mockParseTree "(y+2)" [
        lParenToken
        additionExpr
        rParenToken
    ]

let multiplicationExpr =
    mockParseTree "(y+2)*3" [
        parenExpr
        starToken
        num3Token
    ]

// Mock root node (assignment statement)
let assignmentRoot =
    mockParseTree "x=(y+2)*3" [
        xToken
        equalsToken
        multiplicationExpr
    ]

// Example usage in tests
printfn "Parse tree text: %s" (assignmentRoot.GetText())
printfn "Child count: %d" assignmentRoot.ChildCount