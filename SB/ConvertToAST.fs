//Create normalized homogenous AST
module ConvertToAST

open ReformatParseTree
open Utility
open Antlr4.Runtime.Tree
open Antlr4.Runtime

//type NewNode (?TokenType0 : TokenType, ?Content0 : string, ?sourceReference0 : int, ?Children0 : NewNode list) =
//    let TokenType = defaultArg TokenType0 None
//    let Content = defaultArg Content0 ""
//    let sourceReference = defaultArg sourceReference0 0
//    let Children = defaultArg Children0 []

// walk the Children of the current node
let rec private walkAcross (contextList: FSParseTree List) =
    let rec WalkAcrossInner (contextList: FSParseTree List) index (astChildren: FSParseTree List) =
        let count = contextList.Length

        match index with
        | n when n < count ->
            let ast =
                walkDown (contextList[index]: FSParseTree)

            let appendedAstChildren =
                match ast with
                | Node ast -> astChildren @ [ ast ]
                | Children (astList) -> astChildren @ astList
                | Empty -> astChildren

            WalkAcrossInner (contextList: FSParseTree List) (index + 1) appendedAstChildren
        | _ -> astChildren

    WalkAcrossInner (contextList: FSParseTree List) 0 []
// generate output for a node depending on type and which will call walk across to traverse its Children
and walkDown (context: FSParseTree) : SubTree =
    match context.TokenType with
    | ProgramContext -> translateProgram context
   (* | TokenType.Program -> translateAssignment context  
    | :? SBParser.BinaryContext -> translateExpr context
    | :? SBParser.EndDefContext -> Empty
    | :? SBParser.FuncContext -> translateFunc context
    | :? SBParser.FunchdrContext
    | :? SBParser.ProchdrContext -> translateProcFuncHdr context
    | :? SBParser.LineContext -> translateLine context
    | :? SBParser.LineNumberContext -> translateLineNumber context
    | :? SBParser.LocContext -> translateLocal context
    | :? SBParser.LongforContext -> translateLongFor context
    | :? SBParser.ShortforContext -> translateLongFor context
    | :? SBParser.LongrepeatContext -> translateLongRepeat context
    | :? SBParser.LongifContext -> translateIf context
    | :? TerminalNodeImpl -> translateTerminalNodeImpl context
    | :? SBParser.TermContext -> translateTerm context
    | :? SBParser.TerminatorContext -> translateTerminator context
    | :? SBParser.ProcContext -> translateProc context
    //        | :? SBParser.ProccallContext | :? SBParser.ProccallContext -> translateProcFnCall context
    | :? SBParser.StmtlistContext -> translateStmtList context
    | :? SBParser.IdentifierOnlyContext
    | :? SBParser.IdentifierContext -> translateProcFnCall context
    | :? SBParser.ParenthesizedContext -> translateParenthesized context
    //        | :? SBParser.UnaryAdditiveContext -> translateExpr context
    | :? SBParser.DimContext -> translateDim context
    | :? SBParser.ImplicitContext -> translateImplicit context
    | :? SBParser.ReferenceContext -> translateReference context
    | :? SBParser.RemarkContext -> translateRemark context
    *)| _ ->
        let f : FSParseTree = {
            RuleIndex = 0
            Position = (0, 0)
            Children = []
            Exception = RecognitionException("Default exception placeholder", null, null, null)
            InvokingState = 0
            ContextType = Unknown
            Start = context.Start
            Stop  IToken stop
            // SourceText: string
            //         TokenType = Unknown
            // Content = context.SourceText
            // Children = []
        }
        f

and translateProgram context =
    let astProgram =
        { TokenType = Program
          Content = "The whole program"
          Children = [] }

    Node { astProgram with Children = walkAcross (astProgram.Children) }

// and translateLine context =
//     Children (walkAcross (context |> gatherChildren))
//
// and translateLineNumber context = Empty
//
// and translateStmtList (context: IParseTree) =
//     Children (walkAcross (context |> gatherChildren))
//
// and translateFunc (context: IParseTree) : SubTree =
//     let (routineName, parameters) =
//         Walker.WalkProcFunc (context.GetChild (0))
//
//     Node (
//         { TokenType = Function
//           Content = routineName
//           Children = walkAcross (context |> gatherChildren) }
//     )
//
// and translateDim (context: IParseTree) : SubTree =
//     let (routineName, parameters) =
//         Walker.WalkDim (context)
//
//     Node (
//         { TokenType = Dim
//           Content = routineName
//           Children = walkAcross parameters }
//     )
//
// and translateProcFuncHdr (context: IParseTree) : SubTree =
//     let (_, parameters) =
//         Walker.WalkProcFunc context
//
//     let Children = walkAcross parameters
//
//     let paramChildren =
//         List.map (fun child -> { child with TokenType = Parameter }) Children
//
//     Children paramChildren
//
// and translateProc (context: IParseTree) : SubTree =
//     let (routineName, _) =
//         Walker.WalkProcFunc (context.GetChild (0))
//
//     Node (
//         { TokenType = Procedure
//           Content = routineName
//           Children = walkAcross (context |> Utility.gatherChildren) }
//     )
//
// and translateTerm (context: IParseTree) : SubTree =
//     match context.ChildCount with
//     | 0 ->
//         Node (
//             { TokenType = Value
//               Content = context.GetText ()
//               Children = [] }
//         )
//     | _ -> walkDown (context.GetChild (0))
//
// and translateTerminalNodeImpl (context: IParseTree) : SubTree =
//     match context.ChildCount with
//     | 0 ->
//         Node (
//             { TokenType = Value
//               Content = context.GetText ()
//               Children = [] }
//         )
//     | _ -> walkDown (context.GetChild (0))
//
// and translateTerminator (context: IParseTree) : SubTree =
//     match context.ChildCount with
//     | 0 ->
//         Node (
//             { TokenType = Value
//               Content = context.GetText ()
//               Children = [] }
//         )
//     | _ -> walkDown (context.GetChild (0))
//
// and translateLocal (context: IParseTree) : SubTree =
//     let (local, locals) =
//         Walker.WalkLocal context
//
//     let Children = walkAcross locals
//
//     let localChildren =
//         List.map (fun child -> { child with TokenType = Value }) Children
//
//     Node (
//         { TokenType = Local
//           Content = local
//           Children = localChildren }
//     )
//
// and translateReference (context: IParseTree) : SubTree =
//     let (ref, refVars) =
//         Walker.WalkLocal context
//
//     let Children = walkAcross refVars
//
//     let localChildren =
//         List.map (fun child -> { child with TokenType = Value }) Children
//
//     Node (
//         { TokenType = Reference
//           Content = ref
//           Children = localChildren }
//     )
//
// and translateImplicit (context: IParseTree) : SubTree =
//     let (local, locals) =
//         Walker.WalkLocal context
//
//     let Children = walkAcross locals
//
//     let localChildren =
//         List.map (fun child -> { child with TokenType = Value }) Children
//
//     Node (
//         { TokenType = Implicit
//           Content = local
//           Children = localChildren }
//     )
//
// and translateAssignment (context: FSParseTree) : SubTree =
//     //        let (lvalue, dimensions, rvalue, targetDimensions) = Walker.WalkAssignment context
//     let lvalue =
//         context.Children[0]
//
//     let target = walkDown (context.Children[2])
//
//     let targetNode =
//         (fun x ->
//             match x with
//             | Node x -> x)
//             target
//     //        let dims = WalkAcross targetNode
//     let sourceNode =
//         { TokenType = ProcFnCall
//           Content = lvalue
//           Children = [] }
//
//     Node (
//         { TokenType = Assignment
//           Content = "="
//           Children = sourceNode :: [ targetNode ] }
//     )
//
// and translateProcFnCall (context: IParseTree) : SubTree =
//     let routineName =
//         context.GetChild(0).GetText ()
//
//     let nodeChildren =
//         match context.ChildCount with
//         | 1 -> []
//         | _ ->
//             let Children =
//                 gatherChildren (context.GetChild (1))
//
//             let fChildren =
//                 Children
//                 |> List.filter (fun x ->
//                     not (
//                         x :? TerminalNodeImpl
//                         || x :? SBParser.SeparatorContext
//                     ))
//
//             List.map (fun x -> walkDown x) fChildren
//             |> List.map (fun x ->
//                 match x with
//                 | Node (x) -> x)
//
//     Node (
//         { TokenType = ProcFnCall
//           Content = routineName
//           Children = nodeChildren }
//     )
//
// and translateLongFor (context: IParseTree) : SubTree =
//     let (loopVarString, initialValue, finalValue, stepString) =
//         Walker.WalkFor context
//
//     let loopVar =
//         { TokenType = Value
//           Content = loopVarString
//           Children = [] }
//
//     let initialValue =
//         translateExpr initialValue
//
//     let finalValue = translateExpr finalValue
//
//     let nodeInitialValue =
//         match initialValue with
//         | Node (initialValue) -> initialValue
//         | _ -> failwith "todo"
//
//     let nodeFinalValue =
//         match finalValue with
//         | Node (finalValue) -> finalValue
//         | _ -> failwith "todo"
//
//     let step =
//         { TokenType = Value
//           Content = stepString
//           Children = [] }
//
//     Node
//         { TokenType = TokenType.LongFor
//           Content = loopVarString
//           Children =
//             [ loopVar
//               nodeInitialValue
//               nodeFinalValue
//               step ] }
//
// and translateLongRepeat (context: IParseTree) : SubTree =
//     let loopVarString =
//         Walker.WalkRepeat context
//
//     Node
//         { TokenType = TokenType.Repeat
//           Content = loopVarString
//           Children = [] }
//
// and translateIf (context: IParseTree) : SubTree =
//     let loopVarString = Walker.WalkIf context
//
//     Node
//         { TokenType = TokenType.If
//           Content = loopVarString
//           Children = [] }
//
// and translateExpr (context: IParseTree) : SubTree =
//     match context with
//     | :? SBParser.ParenthesizedContext -> translateParenthesizedExpr context
//     | :? SBParser.BinaryContext -> translateBinaryExpr context
//     | :? SBParser.InstrContext
//     | :? SBParser.NotContext -> translateUnaryExpr context
//     | :? SBParser.TermContext -> translateTermExpr context
//     | _ ->
//         Node
//             { TokenType = None
//               Content = "expression mismatch"
//               Children = [] }
//
// and translateBinaryExpr (context: IParseTree) : SubTree =
//     let st = walkDown (context.GetChild (0))
//
//     let nodeContents =
//         match st with
//         | Node (subTree) -> subTree
//
//     let tokType =
//         match nodeContents.Children with
//         | [] -> Value
//         | _ -> Expression
//
//     let expr1 =
//         { TokenType = tokType
//           Content = nodeContents.Content
//           Children = nodeContents.Children }
//
//     let operator =
//         context.GetChild(1).GetText ()
//
//     let subTree2 =
//         walkDown (context.GetChild (2))
//
//     let nodeContent2 =
//         match subTree2 with
//         | Node (subTree2) -> subTree2
//
//     let tokType2 =
//         match nodeContent2.Children with
//         | [] -> Value
//         | _ -> Expression
//
//     let expr2 =
//         { TokenType = tokType2
//           Content = nodeContent2.Content
//           Children = nodeContent2.Children }
//
//     Node (
//         { TokenType = BinaryExpr
//           Content = operator
//           Children = [ expr1; expr2 ] }
//     )
//
// and translateTermExpr (context: IParseTree) : SubTree =
//     let text = context.GetText ()
//
//     Node
//         { TokenType = Value
//           Content = text
//           Children = [] }
//
// and translateParenthesizedExpr (context: IParseTree) : SubTree = walkDown context
//
// and translateUnaryExpr (context: IParseTree) : SubTree =
//     let st = walkDown (context.GetChild (1))
//
//     let nodeContents =
//         match st with
//         | Node (subTree) -> subTree
//
//     let expr1 =
//         { TokenType = Value
//           Content = nodeContents.Content
//           Children = [] }
//
//     Node (
//         { TokenType = Expression
//           Content = context.GetChild(0).GetText ()
//           Children = [ expr1 ] }
//     )
//
// and translateParenthesized (context: IParseTree) : SubTree =
//     let Children =
//         gatherChildren (context.GetChild (1))
//
//     let op = Children[ 1 ].GetText ()
//
//     Node (
//         { TokenType = Expression
//           Content = op
//           Children = walkAcross Children }
//     )
//
// and translateRemark (context: IParseTree) : SubTree =
//     let Content =
//         context.GetChild(0).GetText().[7..]
//
//     Node (
//         { TokenType = Remark
//           Content = Content
//           Children = [] }
//     )

// top level only
//let RewriteTree (parseTree: FSParseTree) : ASTNode = WalkDown parseTree

//// debugging purposes
//let rec dumpTree (ast: subtree) =
//    let nodeContents = match ast with Node(subTree) -> subTree
//    printf("%s", $@"Type:{nodeContents} ")
