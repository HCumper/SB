﻿module CreateAST

open Antlr4.Runtime.Tree
open Utility

// ----------------------------------------------------
// Common helper functions
// ----------------------------------------------------

/// Get a child of IParseTree by index.
let private getChild (context: IParseTree) index =
    context.GetChild(index)

/// Get first child of IParseTree.
let private getFirstChild (context: IParseTree) =
    getChild context 0

/// Helper function to create an ASTNode.
let private createAstNode (tokenType: NodeKind) (content: string) (position: int * int) (children: ASTNode list)=
    {
        TokenType = tokenType
        Content = content
        Position = position
        Children = children
    }

/// Creates a 'Value' node from a parse-tree context with no children.
// let private createValueSubTree (context: IParseTree) : SubTree =
//     createAstNode Value context.SourceText context.Position []

/// Recursively filters nodes based on a predicate.
let rec treeFilter predicate node =
    if predicate node then
        Some {
            node with Children = node.Children |> List.choose (treeFilter predicate)
        }
    else
        None

// ----------------------------------------------------
// Walk parse-tree nodes to create AST nodes
// ----------------------------------------------------

/// Walks down a parse-tree node to produce a `SubTree`.
/// This is the main dispatcher that routes parse-tree contexts to the appropriate AST construction function.
let rec walkDown (context: IParseTree)=
//    printfn $"Processing Kind: %A{context.Kind}"

    match context.Kind with
    | NodeKind.Assignment            -> translateAssignment context
    | NodeKind.BinaryExpr            -> translateExpr context
    | NodeKind.CallExpr              -> translateCallExpr context
    | NodeKind.Exitstmt              -> translateAsChildren context
    | NodeKind.Function              -> translateFunction context
    | NodeKind.Identifier
    | NodeKind.IdentifierOnly        -> translateIdentifier context
    | NodeKind.If                    -> translateIf context
    | NodeKind.Implicit              -> translateValueIfNoChildren context
    | NodeKind.Line                  -> translateAsChildren context
    | NodeKind.Loc                   -> translateValueIfNoChildren context
    | NodeKind.Nothing               -> translateValueIfNoChildren context
    | NodeKind.ParenthesizedList     -> translateAsChildren context
    | NodeKind.Primary               -> walkDown (getFirstChild context)
    | NodeKind.Proc                  -> translateProcedure context
    | NodeKind.Program               -> translateProgram context
    | NodeKind.Repeat                -> translateRepeat context
    | NodeKind.StmtList              -> translateValueIfNoChildren context
    | NodeKind.StringLiteral         -> translateStringLiteral context
    | NodeKind.Term                  -> translateValueIfNoChildren context
    | NodeKind.TerminalNodeImpl      -> translateTerminalNodeImpl context
    | NodeKind.TypedIdentifier       -> translateTypedIdentifier context
    | NodeKind.UnparenthesizedList   -> translateAsChildren context
    | _ -> createAstNode Unknown $"mismatch on {context.Kind}" context.Position []

/// Helper to walk across a list of parse-tree nodes, flattening any `Children` values.
and private walkAcross (contexts: IParseTree list) : ASTNode list =
    contexts
    |> List.map walkDown
    |> List.collect (function
        | AstNode n      -> [n]
        | Children nList -> nList
        | Empty          -> [])

/// Helper to walk all children of a single context as a list of `ASTNode`.
and private walkAllChildren (context: IParseTree) : ASTNode list =
    context.Children
    |> walkAcross

// ----------------------------------------------------
// Wrappers for returning `Children` vs. creating a single `AstNode`
// ----------------------------------------------------

/// Many translations simply return `Children` after walking all children.
and private translateAsChildren (context: IParseTree) =
    Children (walkAllChildren context)

/// Returns a 'Value' node if there are no children; otherwise walk down the first child.
and private translateValueIfNoChildren (context: IParseTree) =
    if context.Children.Length = 0 then
        createValueSubTree context
    else
        walkDown (getFirstChild context)

// ----------------------------------------------------
// Individual "translate*" functions
// ----------------------------------------------------

and translateAssignment (context: IParseTree) =
    walkAllChildren context
    |> createAstNode Assignment "Assignment" context.Position

and translateProgram (context: IParseTree) =
    walkAllChildren context
    |> createAstNode Program "The whole program" context.Position

and translateIdentifier (context: IParseTree) =
    let processedChildren = walkAllChildren context
    let punctuation = set [ "."; ","; ":"; ")" ]
    let filteredChildren = List.filter (fun x -> not (punctuation.Contains x.Content)) processedChildren
    let tokType =
        if processedChildren.Length = filteredChildren.Length then Identifier
        else ArrayOrFunctionCall
    createAstNode tokType context.SourceText context.Position filteredChildren

and translateTerminalNodeImpl (context: IParseTree) =
    if context.SourceText = "<EOF>" then
        Empty
    else
        if context.Children.Length = 0 then
            createValueSubTree context
        else
            walkDown (getFirstChild context)

and translateRepeat (context: IParseTree) =
    let loopVarStr = ASTWalker.WalkRepeat context
    createAstNode Repeat loopVarStr context.Position []

and translateIf (context: IParseTree) =
    [ getChild context 1; getChild context 2 ]
    |> walkAcross
    |> createAstNode If context.SourceText context.Position

and translateProcFunc kind (context: IParseTree) =
    // Extract the header node that contains the identifier and parameters.
    let headerNode = context.Children[0].Children[1]

    // Extract the name and parameters (if available).
    let name = headerNode.Children[0].SourceText
    let parameters = 
        if headerNode.Children.Length > 1 then 
            walkAllChildren headerNode.Children[1]
        else
            []

    // Create the parameters node.
    let parametersNode = createAstNode Parameters "Parameters" context.Position parameters |> function AstNode n -> n

    // Process the body of the function/procedure.
    // remove hdr and end def
    let bodyLines = List.take (List.length context.Children - 1) context.Children |> List.tail 
    let body = walkAcross (bodyLines)
    let procFNBody = createAstNode Body "Proc/FN Body" context.Position body |> function AstNode n -> n

    // Return the AST node with the provided kind, name, position, and the processed parameters and body.
    createAstNode kind name context.Position [parametersNode; procFNBody]

// Define the procedure translator using the common helper.
and translateProcedure (context: IParseTree)= translateProcFunc Procedure context

// Define the function translator using the common helper.
and translateFunction (context: IParseTree) = translateProcFunc Function context
    
// ----------------------------------------------------
// Expression translations
// ----------------------------------------------------

and translateExpr (context: IParseTree) =
    match context.Kind with
    | BinaryContext -> translateBinaryExpr context
    | UnaryContext  -> translateUnaryExpr context
    | _ -> createAstNode Unknown "expression mismatch" context.Position []

and translateBinaryExpr (context: IParseTree) =
    let lhsSubTree = walkDown (getFirstChild context)
    let lhsNode =
        match lhsSubTree with
        | AstNode n -> n
        | _ -> failwith "Expected Node on left side of binary expression"

    let operatorText = (getChild context 1).SourceText

    let rhsSubTree = walkDown (getChild context 2)
    let rhsNode =
        match rhsSubTree with
        | AstNode n -> n
        | _ -> failwith "Expected Node on right side of binary expression"

    createAstNode BinaryExpr operatorText context.Position [lhsNode; rhsNode]

and translateUnaryExpr (context: IParseTree) =
    let operatorText = (getChild context 0).SourceText
    let operandSubTree = walkDown (getChild context 1)
    let operandNode =
        match operandSubTree with
        | AstNode n -> n
        | _ -> failwith "Expected Node for unary operand"

    createAstNode UnaryExpr operatorText context.Position [operandNode]

and translateCallExpr (context: IParseTree) =
    let funcName = (getChild context 0).SourceText
    let args = walkAllChildren (getChild context 1)
    createAstNode CallExpr funcName context.Position args

and translateStringLiteral (context: IParseTree) =
    createAstNode StringLiteral context.SourceText context.Position []

and translateTypedIdentifier (context: IParseTree) =
    let identifier = (getChild context 0).SourceText
    let typeInfo = (getChild context 1).SourceText
    createAstNode TypedIdentifier identifier context.Position [
        { TokenType = TypedIdentifier; Content = typeInfo; Position = context.Position; Children = [] }
    ]