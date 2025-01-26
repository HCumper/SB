module CreateAST

open ReformatParseTree
open Utility
open Antlr4.Runtime.Tree

/// Intermediate union type to represent partial results when translating parse-tree nodes.
type SubTree =
    | AstNode of ASTNode
    | Children of ASTNode list
    | Empty

// --------------------------------------------------------------------
// Common helper functions
// --------------------------------------------------------------------

/// Get a child of FsParseTree by index.
let private getChild (context: FSParseTree) index =
    context.Data.Children[index]
    
/// Get first child of FsParseTree.
let private getFirstChild (context: FSParseTree) =
    getChild context 0

/// Creates a single 'Value' node from a parse-tree context that contains no children.
let private createValueSubTree (context: FSParseTree) =
    AstNode
        { tokenType = Value
          content = context.Data.SourceText
          position = context.Data.Position 
          children = [] }

/// A visitor to Walk down a single FSParseTree node to decide which translation function to call.
/// This is the core dispatcher that routes parse-tree contexts to their corresponding
/// AST-construction function.
let rec walkDown (context: FSParseTree) : SubTree =
    printfn $"Processing Kind: %A{context.Kind}"

    match context.Kind with
    // | NodeKind.Assignment           -> translateAssignment context
    | NodeKind.BinaryExpr              -> translateExpr context
    // | NodeKind.DimContext           -> translateDim context
    // | NodeKind.EndDefContext        -> Empty
    // | NodeKind.ForloopContext       -> translateFor context
    // | NodeKind.FuncContext          -> translateFunc context
    // | NodeKind.FunchdrContext       -> translateProcFuncHdr context
    | NodeKind.Identifier -> translateIdentifier context
    | NodeKind.If -> translateIf context
    // | NodeKind.IdentifierOnlyContext-> translateProcFnCall context
    | NodeKind.Implicit -> translateImplicit context
    | NodeKind.Line -> translateLine context
    | NodeKind.LineNumber -> Empty
    // | NodeKind.LocContext           -> translateLocal context
    | NodeKind.Nothing -> translateNothing context
    // | NodeKind.ParenthesizedContext -> translateParenthesized context
    // | NodeKind.ProchdrContext       -> translateProcFuncHdr context
    // | NodeKind.ProcContext          -> translateProc context
    | NodeKind.Primary -> translatePrimary context
    | NodeKind.Program -> translateProgram context
    // | NodeKind.ReferenceContext     -> translateReference context
    // | NodeKind.RemarkContext        -> translateRemark context
    | NodeKind.Repeat -> translateRepeat context
    | NodeKind.StmtList -> translateStmtList context
    // | NodeKind.TerminatorContext    -> translateTerminator context
    | NodeKind.TerminalNodeImpl        -> translateTerminalNodeImpl context
    | NodeKind.Term -> translateTerm context
    | NodeKind.UnparenthesizedList -> translateUnparenthesizeList context
    | _ ->
        // Fallback for unrecognized context
        AstNode
            { tokenType = Nothing
              content = $"mismatch on {context.Kind}"
              position = context.Data.Position 
              children = [] }

/// <summary>
/// Walk across a list of IParseTree nodes, converting each to an AST sub-tree.
/// </summary>
and private walkAcross (contexts: FSParseTree list) : ASTNode list =
    contexts
    |> List.map walkDown
    |> List.collect (function
        | AstNode n -> [ n ]
        | Children nLst -> nLst
        | Empty -> [])

/// If the given context has no children because it is terminal, create a Value node; otherwise descend into its first child.
and private translateValueIfNoChildren (context: FSParseTree) =
    match context.Data.Children.Length with
    | 0 -> createValueSubTree context
    | _ -> walkDown (getFirstChild context)

/// General helper to unify code for local/reference/implicit statements:
/// (1) Extracts keyword + child items using Walker.
/// (2) Walks across child items to produce AST nodes.
/// (3) Re-tags child nodes as Value to ensure consistent AST structure.
and private createNodeFromLocal (desiredTokenType: NodeKind) (context: FSParseTree) =
    let keyword, items = ASTWalker.WalkLocal context
    let childNodes = walkAcross items

    // Re-tag all child nodes as Value (these are typically variable names/identifiers)
    let localChildren =
        childNodes |> List.map (fun child -> { child with tokenType = Value })

    AstNode
        { tokenType = desiredTokenType
          content = keyword
          position = context.Data.Position 
          children = localChildren }

// --------------------------------------------------------------------
// Individual "translate*" functions
// --------------------------------------------------------------------

/// Translates the Program context into a Program node whose children
/// are all the walked results of its sub-nodes.
and translateProgram (context: FSParseTree) =
    let childrenNodes = walkAcross (context.Data |> gatherFSChildren)

    AstNode
        { tokenType = Program
          content = "The whole program"
          position = context.Data.Position 
          children = childrenNodes }

and translateIdentifier (context: FSParseTree) =
    let sourceText = context.Data.SourceText
    AstNode
        { tokenType = Identifier
          content = sourceText
          position = context.Data.Position 
          children = [] }

and translateNothing (context: FSParseTree) = translateValueIfNoChildren context

/// Translates a single line context into a collection of child nodes.
/// Typically lines themselves are aggregated, so we return Children.
and translateLine (context: FSParseTree) =
    let childNodes = walkAcross (context.Data |> gatherFSChildren)
    Children childNodes

and translatePrimary (context: FSParseTree) =
    walkDown (getFirstChild context)

/// Translates a single line context into a collection of child nodes.
/// Typically lines themselves are aggregated, so we return Children.
and translateUnparenthesizeList (context: FSParseTree) =
    let childNodes = walkAcross (context.Data |> gatherFSChildren)
    Children childNodes

and translateTerm (context: FSParseTree) = translateValueIfNoChildren context

/// Translates a statement-list context by aggregating all child statements.
and translateStmtList (context: FSParseTree) =
    let childNodes = walkAcross (context.Data |> gatherFSChildren)
    Children childNodes

// /// Translates a function definition.
// and translateFunc (context: FSParseTree) : SubTree =
//     let (routineName, _) = ASTWalker.WalkProcFunc(context.GetChild(0))
//     let funcChildren = walkAcross (context |> gatherFSChildren)
//     Node {
//         tokenType = Function
//         content = routineName
//         children = funcChildren
//     }
//
// /// Translates a DIM statement (dimension array or variable).
// and translateDim (context: FSParseTree) : SubTree =
//     let (varName, parameters) = ASTWalker.WalkDim context
//     let paramNodes = walkAcross parameters
//     Node {
//         tokenType = Dim
//         content = varName
//         children = paramNodes
//     }
//
// /// Translates procedure/function header contexts,
// /// converting parameters into 'Parameter' nodes for the AST.
// and translateProcFuncHdr (context: FSParseTree) : SubTree =
//     let (_, parameters) = ASTWalker.WalkProcFunc context
//     let paramNodes = walkAcross parameters
//
//     // Re-tag all child param nodes as Parameter
//     let paramChildren = paramNodes |> List.map (fun c -> { c with tokenType = Parameter })
//     Children paramChildren
//
// /// Translates a complete procedure definition.
// and translateProc (context: FSParseTree) : SubTree =
//     let (routineName, _) = ASTWalker.WalkProcFunc(context.GetChild(0))
//     let procChildren = walkAcross (context |> gatherFSChildren)
//     Node {
//         tokenType = Procedure
//         content = routineName
//         children = procChildren
//     }
//
// // Three statements for local/reference/implicit nodes
// and translateLocal context    = createNodeFromLocal Local context
// and translateReference context= createNodeFromLocal Reference context
and translateImplicit context = createNodeFromLocal Implicit context
//
// /// Produces a Value node if no children; otherwise walks its first child.
// and translateTerm (context: FSParseTree) =
//     translateValueIfNoChildren context

and translateTerminalNodeImpl (context: FSParseTree) =
    walkDown (getFirstChild context)

// and translateTerminator (context: FSParseTree) =
//     translateValueIfNoChildren context
//
// /// Translates an assignment statement: left-hand side (LHS) as a ProcFnCall node,
// /// right-hand side (RHS) as a standard expression node.
// and translateAssignment (context: FSParseTree) : SubTree =
//     // Child(0) is usually the assignment target
//     let lhsToken = context.GetChild(0).GetChild(0).GetText()
//
//     // Child(2) is typically the right-hand side expression
//     let rhsSubTree = walkDown (context.GetChild(2))
//     let rhsNode =
//         match rhsSubTree with
//         | Node node -> node
//         | _         -> failwith "Expected node on RHS of assignment"
//
//     let lhsNode =
//         { tokenType = ProcFnCall
//           content = lhsToken
//           children = [] }
//
//     Node {
//         tokenType = Assignment
//         content = "="
//         children = [ lhsNode; rhsNode ]
//     }

// /// Translates a procedure/function call context,
// /// possibly with parameters inside parentheses.
// and translateProcFnCall (context: FSParseTree) : SubTree =
//     let routineName = context.GetChild(0).GetText()
//
//     let childList =
//         if context.ChildCount > 1 then
//             gatherFSChildren (context.GetChild(1))
//             |> List.filter (fun c ->
//                 not (c :? TerminalNodeImpl || c :? SBParser.SeparatorContext))
//             |> List.map walkDown
//             |> List.choose (function Node n -> Some n | _ -> None)
//         else
//             []
//
//     Node {
//         tokenType = ProcFnCall
//         content = routineName
//         children = childList
//     }
//
// /// Translates a long FOR statement (or short FOR).
// /// Example logic retrieves loop variable, init/final expressions, and step.
// and translateLongFor (context: FSParseTree) : SubTree =
//     let (loopVarStr, initValue, finalValue, stepStr) = ASTWalker.WalkFor context
//
//     let loopVarNode =
//         { tokenType = Value
//           content = loopVarStr
//           children = [] }
//
//     let initNode =
//         match translateExpr initValue with
//         | Node n -> n
//         | _      -> failwith "Expected Node for initial value"
//
//     let finalNode =
//         match translateExpr finalValue with
//         | Node n -> n
//         | _      -> failwith "Expected Node for final value"
//
//     let stepNode =
//         { tokenType = Value
//           content = stepStr
//           children = [] }
//
//     Node {
//         tokenType = LongFor
//         content = loopVarStr
//         children = [ loopVarNode; initNode; finalNode; stepNode ]
//     }
//

/// Translates a REPEAT statement.
and translateRepeat (context: FSParseTree) : SubTree =
    let loopVarStr = ASTWalker.WalkRepeat context

    AstNode
        { tokenType = Repeat
          content = loopVarStr
          position = context.Data.Position 
          children = [] }

and translateIf (context: FSParseTree) : SubTree =
    let ifChildren = walkAcross [getChild context 1; getChild context 3]
    AstNode {
        tokenType = If
        content = context.Data.SourceText
        position = context.Data.Position 
        children = ifChildren 
    }

// --------------------------------------------------------------------
// Expression translations
// --------------------------------------------------------------------

/// Main entry point for translating expressions.
/// Dispatches to specialized translation functions.
and translateExpr (context: FSParseTree) : SubTree =
    match context.Kind with
 //   | ParenthesizedList -> translateParenthesizedExpr context
    | BinaryContext        -> translateBinaryExpr context
 //   | InstrContext
//    | NotContext           -> translateUnaryExpr context
//    | TermContext          -> translateTermExpr context
    | _ ->
        AstNode {
            tokenType = Unknown
            content = "expression mismatch"
            position =  context.Data.Position
            children = []
        }

/// Translates a binary expression (expr op expr).
/// Left and right child expressions are walked separately.
and translateBinaryExpr (context: FSParseTree) : SubTree =
    // Left expression
    let lhsSubTree = walkDown (getFirstChild context)
    let lhsNode =
        match lhsSubTree with
        | AstNode n -> n
        | _      -> failwith "Expected Node on left side of binary expression"

    // Operator
    let operatorText = (getChild context 1).Data.Children[1].Data.SourceText 
    // Right expression
    let rhsSubTree = walkDown (getChild context 2)
    let rhsNode =
        match rhsSubTree with
        | AstNode n -> n
        | _      -> failwith "Expected Node on right side of binary expression"
    AstNode {
        tokenType = BinaryExpr
        content = operatorText
        position =  context.Data.Position
        children = [lhsNode; rhsNode]
    }
