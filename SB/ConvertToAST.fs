//Create normalized homogenous AST
module ConvertToAST

open ReformatParseTree
open Utility
open Antlr4.Runtime.Tree
open Antlr4.Runtime

type ASTNode =
    // string only
    | Identifier of string
    | Literal of string
    | Operator of string
    | Keyword of string
    | Delimiter of string
    
    | Program of ASTNode list
    | FunctionDef of string * ASTNode list * ASTNode list
    | Assignment of string * ASTNode
    | UnaryExpression of ASTNode *string
    | BinaryExpression of ASTNode * string * ASTNode

let rec convertToAST (node: IParseTree) : ASTNode option =
    let text = node.GetText()
    match node with
    | :? TerminalNodeImpl as termNode ->       
        match termNode.Symbol.Type with
        // Identifier
        | SBParser.ID -> Some (Identifier text)
        // Literals
        | SBParser.Integer
        | SBParser.String
        | SBParser.Real
            -> Some (Literal text)
        // Operators
        // Arithmetic operators
        | SBParser.Plus
        | SBParser.Minus
        | SBParser.Multiply
        | SBParser.Divide
        | SBParser.Mod
        | SBParser.Div
        | SBParser.Caret
        // Comparison operators
        | SBParser.Equal
        | SBParser.NotEqual
        | SBParser.Less
        | SBParser.LessEqual
        | SBParser.Greater
        | SBParser.GreaterEqual
        // Logical operators
        | SBParser.And
        | SBParser.Or
        | SBParser.Xor
        | SBParser.Not
        // String operators
        | SBParser.Amp
        | SBParser.Instr
        // Miscellaneous operators
        | SBParser.Tilde
        | SBParser.Question
        | SBParser.Bang
        // Range and Assignment operators
        | SBParser.To
        | SBParser.Equal
            -> Some (Operator text)
        
        // Keywords
        // Control Flow
        | SBParser.If
        | SBParser.Else
        | SBParser.Then
        | SBParser.EndIf
        // Looping
        | SBParser.For
        | SBParser.Next
        | SBParser.To
        | SBParser.EndFor
        | SBParser.Step
        | SBParser.Repeat
        | SBParser.Until
        | SBParser.EndRepeat
        | SBParser.Exit  // EXIT
        // Function and Procedure
        | SBParser.DefProc  // DEFine PROCedure
        | SBParser.DefFunc  // DEFine FuNction
        | SBParser.EndDef  // END DEFine
        // Variable declaration
        | SBParser.Local  // LOCal
        | SBParser.Dimension  // DIM
        | SBParser.Refer  // REFERENCE
        | SBParser.Implic  // IMPLICIT$ / IMPLICIT%
        // Selection and case
        | SBParser.Select  // SELect ON
        | SBParser.EndSelect  // END SELect
        | SBParser.On  // ON
        // Comment and formatting
        | SBParser.Comment  // REMark
        | SBParser.Newline  // Newline character
        // Miscellaneous
        | SBParser.Let  // LET (Skipped in grammar)
        | SBParser.Unknowntype  // "program use only"
        | SBParser.Void  // "program use only"
            -> Some (Keyword text)
        //  Delimiters
        // Parentheses and brackets
        | SBParser.LeftParen    // (
        | SBParser.RightParen   // )
        | SBParser.LeftBracket  // [
        | SBParser.RightBracket // ]
        // Statement and expression separators
        | SBParser.Colon  // :
        | SBParser.Semi   // ;
        | SBParser.Comma  // ,
        | SBParser.Point  // .
        | SBParser.Bang   // !
        // Miscellaneous
        | SBParser.Question  // ?
            -> Some (Delimiter text)
        | _ -> None  // Catch-all for other tokens
        
        
        
    | :? SBParser.AssignmentContext as assign ->
        let identifier = "" //assign.ID().GetText()
        let expr = convertToAST (assign.expr())
        match expr with
        | Some e -> Some (Assignment (identifier, e))
        | None -> None    | _ -> failwith "Expected a terminal node"
    // | :? SBParser.ProgramContext as assign ->
    //     let identifier = "" //assign.ID().GetText()
    //     let expr = convertToAST (node)
    //     match expr with
    //     | Some e -> Some (Assignment (identifier, e))
    //     | None -> None    | _ -> failwith "Expected a terminal node"
    | _ -> None
    
// let rec convertToAST (node: IParseTree) : ASTNode option =
//     match node with
//     | :? TerminalNodeImpl -> Some (convertTerminalNode node)
//
//     // Function Definitions
//     | :? SBParser.FunctionContext as func ->
//         let name = func.ID().GetText()
//         let parameters = func.parameters().ID() |> Seq.map (fun p -> Identifier (p.GetText())) |> Seq.toList
//         let body = func.body().statement() |> Seq.choose convertToAST |> Seq.toList
//         Some (FunctionDef (name, parameters, body))
//
//     // Procedure Definitions
//     | :? SBParser.ProcedureContext as proc ->
//         let name = proc.ID().GetText()
//         let body = proc.body().statement() |> Seq.choose convertToAST |> Seq.toList
//         Some (ProcedureDef (name, body))
//
//     // Assignments (e.g., `x = 5`)
//     | :? SBParser.AssignmentContext as assign ->
//         let identifier = assign.ID().GetText()
//         let expr = assign.expr() |> convertToAST
//         match expr with
//         | Some e -> Some (Assignment (identifier, e))
//         | None -> None
//
//     // Binary Expressions (e.g., `5 + 3`)
//     | :? SBParser.ExprContext as expr when expr.GetChildCount() = 3 ->
//         let left = convertToAST (expr.GetChild 0)
//         let op = expr.GetChild(1).GetText()
//         let right = convertToAST (expr.GetChild 2)
//         match left, right with
//         | Some l, Some r -> Some (BinaryExpression (l, op, r))
//         | _ -> None
//
//     // Print Statement
//     | :? SBParser.PrintStatementContext as printStmt ->
//         let args = printStmt.expr() |> Seq.choose convertToAST |> Seq.toList
//         Some (Print args)
//
//     // Loop (FOR statement)
//     | :? SBParser.ForStatementContext as forStmt ->
//         let iterator = forStmt.ID().GetText()
//         let startExpr = convertToAST (forStmt.expr(0))
//         let endExpr = convertToAST (forStmt.expr(1))
//         let body = forStmt.statement() |> Seq.choose convertToAST |> Seq.toList
//         match startExpr, endExpr with
//         | Some s, Some e -> Some (Loop (iterator, s, e, body))
//         | _ -> None
//
//     // Ignore other nodes for now
//     | _ -> None
