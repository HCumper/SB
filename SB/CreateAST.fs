module CreateAST

open ReformatParseTree
open Utility
open Antlr4.Runtime.Tree

/// Intermediate union type to represent partial results when translating parse-tree nodes.
type SubTree =
    | Node of ASTNode
    | Children of ASTNode list
    | Empty

// --------------------------------------------------------------------
// Common helper functions
// --------------------------------------------------------------------

/// Creates a single 'Value' node from a parse-tree context that contains no children.
let private createValueSubTree (context: FSParseTree) =
    Node
        { tokenType = Value
          content = context.Data.SourceText
          children = [] }

/// A visitor to Walk down a single FSParseTree node to decide which translation function to call.
/// This is the core dispatcher that routes parse-tree contexts to their corresponding
/// AST-construction function.
let rec walkDown (context: FSParseTree) : SubTree =
    match context.Kind with
    // | Assignment           -> translateAssignment context
    // | BinaryContext        -> translateExpr context
    // | EndDefContext        -> Empty
    // | FuncContext          -> translateFunc context
    // | FunchdrContext       -> translateProcFuncHdr context 
    // | ProchdrContext       -> translateProcFuncHdr context
    | LineContext             -> translateLine context
    | LineNumberContext       -> translateLineNumber context
    // | LocContext           -> translateLocal context
    // | ForloopContext       -> translateFor context
    | Nothing                 -> translateNothing context
    // | RepeatContext        -> translateRepeat context
    // | IfContext            -> translateIf context
    // | TerminalNodeImpl     -> translateTerminalNodeImpl context
    | Term                    -> translateTerm context
    // | TerminatorContext    -> translateTerminator context
    // | ProcContext          -> translateProc context
     | ProgramContext         -> translateProgram context
    // | StmtlistContext      -> translateStmtList context
    // | IdentifierOnlyContext-> translateProcFnCall context
    | Identifier              -> translateIdentifier context
       | UnparenthesizedList  -> translateUnparenthesizeList context 
    // | ParenthesizedContext -> translateParenthesized context
    // | DimContext           -> translateDim context
    | Primary                 -> translatePrimary context        
    | ImplicitContext         -> translateImplicit context
    // | ReferenceContext     -> translateReference context
//    | RemarkContext        -> translateRemark context
     | _ ->
        // Fallback for unrecognized context
        Node
            { tokenType = Nothing
              content = "mismatch"
              children = [] }
            
/// <summary>
/// Walk across a list of IParseTree nodes, converting each to an AST sub-tree.
/// </summary>
and private walkAcross (contexts: FSParseTree list) : ASTNode list =
    contexts
    |> List.map walkDown
    |> List.collect (function
        | Node n        -> [ n ]
        | Children nLst -> nLst
        | Empty         -> [])

/// If the given context has no children because it is terminal, create a Value node; otherwise descend into its first child.
and private translateValueIfNoChildren (context: FSParseTree) =
    match context.Data.Children.Length with
    | 0 -> createValueSubTree context
    | _ -> walkDown context.Data.Children[0]

/// General helper to unify code for local/reference/implicit statements:
/// (1) Extracts keyword + child items using Walker.
/// (2) Walks across child items to produce AST nodes.
/// (3) Re-tags child nodes as Value to ensure consistent AST structure.
and private createNodeFromLocal (desiredTokenType: NodeKind) (context: FSParseTree) =
    let keyword, items = ASTWalker.WalkLocal context
    let childNodes = walkAcross items

    // Re-tag all child nodes as Value (these are typically variable names/identifiers)
    let localChildren =
        childNodes
        |> List.map (fun child -> { child with tokenType = Value })

    Node
        { tokenType = desiredTokenType
          content = keyword
          children = localChildren }

// --------------------------------------------------------------------
// Individual "translate*" functions
// --------------------------------------------------------------------

/// Translates the Program context into a Program node whose children
/// are all the walked results of its sub-nodes.
and translateProgram (context: FSParseTree) =
    let childrenNodes = walkAcross (context.Data |> gatherFSChildren)
    Node {
        tokenType = Program
        content = "The whole program"
        children = childrenNodes
    }
    
and translateIdentifier (context: FSParseTree) =
    translateValueIfNoChildren context

and translateNothing (context: FSParseTree) =
    translateValueIfNoChildren context

/// Translates a single line context into a collection of child nodes.
/// Typically lines themselves are aggregated, so we return Children.
and translateLine (context: FSParseTree) =
    let childNodes = walkAcross (context.Data |> gatherFSChildren)
    Children childNodes

and translatePrimary (context: FSParseTree) =
    let childNodes = walkAcross (context.Data |> gatherFSChildren)
    Children childNodes

/// Translates a single line context into a collection of child nodes.
/// Typically lines themselves are aggregated, so we return Children.
and translateUnparenthesizeList (context: FSParseTree) =
    let childNodes = walkAcross (context.Data |> gatherFSChildren)
    Children childNodes

/// A line number is often carried separately or is not necessary to store in the AST.
/// Return Empty here, or you can store it if needed.
and translateLineNumber (_context: FSParseTree) = Empty

and translateTerm (context: FSParseTree) =
    translateValueIfNoChildren context

// /// Translates a statement-list context by aggregating all child statements.
// and translateStmtList (context: FSParseTree) =
//     let childNodes = walkAcross (context |> gatherFSChildren)
//     Children childNodes
//
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
//
// and translateTerminalNodeImpl (context: FSParseTree) =
//     translateValueIfNoChildren context
//
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
// /// Translates a long REPEAT statement.
// and translateLongRepeat (context: FSParseTree) : SubTree =
//     let loopVarStr = ASTWalker.WalkRepeat context
//     Node {
//         tokenType = Repeat
//         content = loopVarStr
//         children = []
//     }
//
// /// Translates an IF statement context.
// /// The actual string returned from Walker.WalkIf might hold some additional info (condition, etc.).
// and translateIf (context: FSParseTree) : SubTree =
//     let ifString = ASTWalker.WalkIf context
//     Node {
//         tokenType = If
//         content = ifString
//         children = []
//     }
//
// --------------------------------------------------------------------
// Expression translations
// --------------------------------------------------------------------

/// Main entry point for translating expressions.
/// Dispatches to specialized translation functions.
// and translateExpr (context: FSParseTree) : SubTree =
//     match context.Kind with
//     | ParenthesizedList -> translateParenthesizedExpr context
//     | BinaryContext        -> translateBinaryExpr context
//     | InstrContext
// //    | NotContext           -> translateUnaryExpr context
//     | TermContext          -> translateTermExpr context
//     | _ ->
//         Node {
//             tokenType = None
//             content = "expression mismatch"
//             children = []
//         }
//
// /// Translates a binary expression (expr op expr).
// /// Left and right child expressions are walked separately.
// and translateBinaryExpr (context: FSParseTree) : SubTree =
//     // Left expression
//     let lhsSubTree = walkDown (context.GetChild(0))
//     let lhsNode =
//         match lhsSubTree with
//         | Node n -> n
//         | _      -> failwith "Expected Node on left side of binary expression"
//
//     // Operator
//     let operatorText = context.GetChild(1).GetText()
//
//     // Right expression
//     let rhsSubTree = walkDown (context.GetChild(2))
//     let rhsNode =
//         match rhsSubTree with
//         | Node n -> n
//         | _      -> failwith "Expected Node on right side of binary expression"

    // Decide whether child nodes are "Expression" or "Value" based on child presence
 
