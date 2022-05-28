//Create normalized homogenous AST
module TreeRewriter

open Utility
open Antlr4.Runtime.Tree
open SBLib

type TokenType =
    Program | Line | LineNumber | StmtList | Stmt | None | Function | Procedure | Parameter | Terminator | Local | Assignment | Value | BinaryExpr | ProcFnCall | LongFor

// The one and only
type Node = {
    tokenType: TokenType;
    content: string;
    // evaluated type
    // source code reference
    children: Node list;
}

type SubTree = Node of Node | Children of Node List | Empty

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
        | :? SBParser.BinaryContext -> translateBinaryExpr context
        | :? SBParser.EndDefContext -> Empty
        | :? SBParser.FuncContext -> translateFunc context
        | :? SBParser.FunchdrContext | :? SBParser.ProchdrContext -> translateProcFuncHdr context
        | :? SBParser.LineContext -> translateLine context
        | :? SBParser.LineNumberContext -> translateLineNumber context
        | :? SBParser.LocContext -> translateLocal context
        | :? SBParser.LongforContext -> translateLongFor context
        | :? TerminalNodeImpl -> Empty
        | :? SBParser.ProcContext -> translateProc context
//        | :? SBParser.ProccallContext | :? SBParser.ProccallContext -> translateProcFnCall context
        | :? SBParser.ProgramContext -> translateProgram context
        | :? SBParser.StmtlistContext -> translateStmtList context
        | :? SBParser.TermContext -> translateTerminal context
        | :? SBParser.TerminatorContext -> translateTerm context
        | :? SBParser.IdentifierOnlyContext -> translateProcFnCall context
        | :? SBParser.ParenthesizedContext -> translateParenthesized context
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
        Node({tokenType=Terminator; content=context.GetText(); children=[]})
and
    translateLocal (context: IParseTree): SubTree =
        let (local, locals) = Walker.WalkLocal context
        let children = WalkAcross locals
        let localChildren = List.map (fun child -> {child with tokenType=Value}) children
        Node({tokenType=Local; content=local; children=localChildren})
and
    translateAssignment (context: IParseTree): SubTree =
        let (lvalue, dimensions, rvalue) = Walker.WalkAssignment context
        let target = WalkDown rvalue
        let nodeContents = match target with Node(target) -> target
        Node({tokenType=Assignment; content=lvalue; children=[nodeContents]})
and
    translateTerm (context: IParseTree): SubTree =
        Empty
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
        let loopVar = {tokenType=Terminator; content=loopVarString; children=[]}
        let initialValue = translateExpr initialValue
        let finalValue = translateExpr finalValue
        let nodeInitialValue = match initialValue with Node(initialValue) -> initialValue
        let nodeFinalValue = match finalValue with Node(finalValue) -> finalValue
        let step = {tokenType=Terminator; content=stepString; children=[]}
        Node {tokenType=TokenType.LongFor; content=context.GetText(); children=[loopVar; nodeInitialValue; nodeFinalValue; step]}
and
    translateExpr  (context: IParseTree): SubTree =
        match context with
        | :? SBParser.ParenthesizedContext -> translateParenthesizedExpr context
        | :? SBParser.BinaryContext -> translateBinaryExpr context
        | :? SBParser.InstrContext | :? SBParser.UnaryAdditiveContext | :? SBParser.NotContext -> translateUnaryExpr context
        //| :? SBParser.TermContext -> translateTermExpr context
        | _ -> Node{tokenType=None; content="expression mismatch"; children=[]}
and
    translateBinaryExpr (context: IParseTree): SubTree =
        let st = WalkDown (context.GetChild(0))
        let nodeContents = match st with Node(subTree) -> subTree
        let expr1 = {tokenType=Value; content=nodeContents.content; children=[]}
        let operator = context.GetChild(1).GetText()
        let subTree2 = WalkDown (context.GetChild(2))
        let nodeContent2 = match subTree2 with Node(subTree2) -> subTree2
        let expr2 = {tokenType=Value; content=nodeContent2.content; children=[]}
        Node({tokenType=BinaryExpr; content=operator; children=[expr1; expr2]})
and
    translateParenthesizedExpr (context: IParseTree): SubTree =
        WalkDown context
and
    translateUnaryExpr (context: IParseTree): SubTree =
        let st = WalkDown (context.GetChild(0))
        let nodeContents = match st with Node(subTree) -> subTree
        let expr1 = {tokenType=Value; content=nodeContents.content; children=[]}
        let operator = context.GetChild(1).GetText()
        Node({tokenType=BinaryExpr; content=operator; children=[expr1]})
and
    translateParenthesized (context: IParseTree): SubTree =
        let children = gatherChildren (context.GetChild(1))
        Children(WalkAcross children)


// top level only
let RewriteTree (parseTree: IParseTree) : SubTree =
    WalkDown parseTree  
