//Create normalized homogenous AST
module TreeRewriter

open Utility
open Antlr4.Runtime.Tree
open SBLib
open System

type TokenType =
    Program | Line | LineNumber | StmtList | Stmt | None | Function | Procedure | Parameter | Local | Assignment | Value | BinaryExpr | ProcFnCall | LongFor | AssignmentTarget | Expression | Dim | Reference | Repeat | If | Remark | Implicit

type EvaluatedType = Integer | Real | String

// The one and only
type Node = {
    tokenType: TokenType;
    content: string;
    //evaluatedType: EvaluatedType
    //sourceReference: int
    children: Node list;
}

type SubTree = Node of Node | Children of Node List | Empty

//type NewNode (?tokenType0 : TokenType, ?content0 : string, ?sourceReference0 : int, ?children0 : NewNode list) =
//    let tokenType = defaultArg tokenType0 None
//    let content = defaultArg content0 ""
//    let sourceReference = defaultArg sourceReference0 0
//    let children = defaultArg children0 []
 
//let doIt tokenType content children =
//    Node({tokenType=tokenType; content=content; children=children})

// walk the children of the current node
let rec private WalkAcross (contextList : IParseTree List) =
    let rec WalkAcrossInner (contextList : IParseTree List) index (astChildren : Node List) =
        let count = contextList.Length
        match index with
        | n when n < count ->
            let ast = WalkDown (contextList[index] : IParseTree)
            let appendedAstChildren =
                match ast with
                | Node(ast) -> astChildren @ [ast]
                | Children(astList) -> astChildren @ astList
                | Empty -> astChildren
            WalkAcrossInner (contextList : IParseTree List) (index+1) appendedAstChildren 
        | _ -> astChildren
    WalkAcrossInner (contextList : IParseTree List) 0 []
and 
    // generate output for a node depending on type and which will call walk across to traverse its children
    private WalkDown context : SubTree =
        match context with
        | :? SBParser.AssignmentContext -> translateAssignment context
        | :? SBParser.BinaryContext -> translateExpr context
        | :? SBParser.EndDefContext -> Empty
        | :? SBParser.FuncContext -> translateFunc context
        | :? SBParser.FunchdrContext | :? SBParser.ProchdrContext -> translateProcFuncHdr context
        | :? SBParser.LineContext -> translateLine context
        | :? SBParser.LineNumberContext -> translateLineNumber context
        | :? SBParser.LocContext -> translateLocal context
        | :? SBParser.LongforContext -> translateLongFor context
        | :? SBParser.ShortforContext -> translateLongFor context
        | :? SBParser.LongrepeatContext -> translateLongRepeat context
        | :? SBParser.LongifContext -> translateIf context
        | :? TerminalNodeImpl -> Empty
        | :? SBParser.ProcContext -> translateProc context
//        | :? SBParser.ProccallContext | :? SBParser.ProccallContext -> translateProcFnCall context
        | :? SBParser.ProgramContext -> translateProgram context
        | :? SBParser.StmtlistContext -> translateStmtList context
        | :? SBParser.TermContext -> translateTerminal context
        | :? SBParser.TerminatorContext -> translateTerm context
        | :? SBParser.IdentifierOnlyContext | :? SBParser.IdentifierContext -> translateProcFnCall context
        | :? SBParser.ParenthesizedContext -> translateParenthesized context
        | :? SBParser.UnaryAdditiveContext -> translateExpr context
        | :? SBParser.DimContext -> translateDim context
        | :? SBParser.ImplicitContext -> translateImplicit context
        | :? SBParser.ReferenceContext -> translateReference context
        | :? SBParser.RemarkContext -> translateRemark context
        | _ -> Node{tokenType=None; content="mismatch"; children=[]}
 and
    // translate the root program node
    translateProgram context =
        let astProgram = {tokenType= Program; content="The whole program"; children = []}
        Node({astProgram with children=WalkAcross (context |> gatherChildren)})
and
    translateLine context =
        Children(WalkAcross (context |> gatherChildren))
and
   translateLineNumber context =
        Empty
        // only a terminal node underneath, no need to look at it
and
    translateStmtList (context: IParseTree) =
       Children(WalkAcross (context |> gatherChildren))
and
    translateFunc (context: IParseTree): SubTree =
        let (routineName, parameters) = Walker.WalkProcFunc (context.GetChild(0))
        Node({tokenType=Function; content=routineName; children=WalkAcross (context |> gatherChildren)})
and
    translateDim (context: IParseTree): SubTree =
        let (routineName, parameters) = Walker.WalkDim (context)
        Node({tokenType=Dim; content=routineName; children=WalkAcross parameters})
and
    translateProcFuncHdr (context: IParseTree): SubTree =
        let (_, parameters) = Walker.WalkProcFunc context
        let children = WalkAcross parameters
        let paramChildren = List.map (fun child -> {child with tokenType=Parameter}) children
        Children paramChildren
and
    translateProc (context: IParseTree): SubTree =
        let (routineName, _) = Walker.WalkProcFunc (context.GetChild(0))
        Node({tokenType=Procedure; content=routineName; children=WalkAcross (context |> Utility.gatherChildren)})
and
    translateTerminal (context: IParseTree): SubTree =
    match context.ChildCount with
        | 0 -> Node({tokenType=Value; content=context.GetText(); children=[]})
        | _ -> WalkDown (context.GetChild(0))
and
    translateLocal (context: IParseTree): SubTree =
        let (local, locals) = Walker.WalkLocal context
        let children = WalkAcross locals
        let localChildren = List.map (fun child -> {child with tokenType=Value}) children
        Node({tokenType=Local; content=local; children=localChildren})
and
    translateReference (context: IParseTree): SubTree =
        let (ref, refVars) = Walker.WalkLocal context
        let children = WalkAcross refVars
        let localChildren = List.map (fun child -> {child with tokenType=Value}) children
        Node({tokenType=Reference; content=ref; children=localChildren})
and
    translateImplicit (context: IParseTree): SubTree =
        let (local, locals) = Walker.WalkLocal context
        let children = WalkAcross locals
        let localChildren = List.map (fun child -> {child with tokenType=Value}) children
        Node({tokenType=Implicit; content=local; children=localChildren})
and
    translateAssignment (context: IParseTree): SubTree =
        let (lvalue, dimensions, rvalue, targetDimensions) = Walker.WalkAssignment context
        let target = WalkDown (context.GetChild(2))
        let targetNode = (fun x -> match x with Node(x) -> x) target
        let dims = WalkAcross dimensions
        let sourceNode = {tokenType=ProcFnCall; content=lvalue; children=dims}
        Node({tokenType=Assignment; content="="; children=sourceNode::[targetNode]})
and
    translateTerm (context: IParseTree): SubTree =
    match context.ChildCount with
        | 0 -> Node({tokenType=Value; content=context.GetText(); children=[]})
        | _ -> WalkDown (context.GetChild(0))
and
    translateProcFnCall (context: IParseTree): SubTree =
        let routineName = context.GetChild(0).GetText()
        let nodeChildren =
            match context.ChildCount with
            | 1 -> []
            | _ ->
                let children = gatherChildren (context.GetChild(1))
                let fChildren = children |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))            
                List.map (fun x -> WalkDown x ) fChildren |> List.map (fun x -> match x with Node(x) -> x)
        Node ({tokenType=ProcFnCall; content=routineName; children=nodeChildren})
and
    translateLongFor (context: IParseTree): SubTree =
        let (loopVarString, initialValue, finalValue, stepString) = Walker.WalkFor context
        let loopVar = {tokenType=Value; content=loopVarString; children=[]}
        let initialValue = translateExpr initialValue
        let finalValue = translateExpr finalValue
        let nodeInitialValue = match initialValue with Node(initialValue) -> initialValue
        let nodeFinalValue = match finalValue with Node(finalValue) -> finalValue
        let step = {tokenType=Value; content=stepString; children=[]}
        Node {tokenType=TokenType.LongFor; content=loopVarString; children=[loopVar; nodeInitialValue; nodeFinalValue; step]}
and
    translateLongRepeat (context: IParseTree): SubTree =
        let loopVarString = Walker.WalkRepeat context
        Node {tokenType=TokenType.Repeat; content=loopVarString; children=[]}
and
    translateIf (context: IParseTree): SubTree =
        let loopVarString = Walker.WalkIf context
        Node {tokenType=TokenType.If; content=loopVarString; children=[]}
and
    translateExpr  (context: IParseTree): SubTree =
        match context with
        | :? SBParser.ParenthesizedContext -> translateParenthesizedExpr context
        | :? SBParser.BinaryContext -> translateBinaryExpr context
        | :? SBParser.InstrContext | :? SBParser.UnaryAdditiveContext | :? SBParser.NotContext -> translateUnaryExpr context
        | :? SBParser.TermContext -> translateTermExpr context
        | _ -> Node{tokenType=None; content="expression mismatch"; children=[]}
and
    translateBinaryExpr (context: IParseTree): SubTree =
        let st = WalkDown (context.GetChild(0))
        let nodeContents = match st with Node(subTree) -> subTree
        let tokType = match nodeContents.children with [] -> Value | _ -> Expression
        let expr1 = {tokenType=tokType; content=nodeContents.content; children=nodeContents.children}
        let operator = context.GetChild(1).GetText()
        let subTree2 = WalkDown (context.GetChild(2))
        let nodeContent2 = match subTree2 with Node(subTree2) -> subTree2
        let tokType2 = match nodeContent2.children with [] -> Value | _ -> Expression
        let expr2 = {tokenType=tokType2; content=nodeContent2.content; children=nodeContent2.children}
        Node({tokenType=BinaryExpr; content=operator; children=[expr1; expr2]})
and
    translateTermExpr (context: IParseTree): SubTree =
        let text = context.GetText()
        Node{tokenType=Value; content=text; children=[]}
and
    translateParenthesizedExpr (context: IParseTree): SubTree =
        WalkDown context
and
    translateUnaryExpr (context: IParseTree): SubTree =
        let st = WalkDown (context.GetChild(1))
        let nodeContents = match st with Node(subTree) -> subTree
        let expr1 = {tokenType=Value; content=nodeContents.content; children=[]}
        Node({tokenType=Expression; content=context.GetChild(0).GetText(); children=[expr1]})
and
    translateParenthesized (context: IParseTree): SubTree =
        let children = gatherChildren (context.GetChild(1))
        let op = children[1].GetText()
        Node({tokenType=Expression; content=op; children=WalkAcross children})
and
    translateRemark (context: IParseTree): SubTree =
        let content = context.GetChild(0).GetText().[7..]
        Node({tokenType=Remark; content=content; children=[]})

// top level only
let RewriteTree (parseTree: IParseTree) : SubTree =
    WalkDown parseTree  

//// debugging purposes
//let rec dumpTree (ast: subtree) =
//    let nodeContents = match ast with Node(subTree) -> subTree
//    printf("%s", $@"Type:{nodeContents} ")
