module Utility

open Antlr4.Runtime.Tree
open SymbolTable
open System
open Antlr4.Runtime


// Types of tokens used in parse tree
type NodeKind =
    | Assignment
    | AssignmentTarget
    | BinaryExpr
    | Dim
    | EndDef
    | EndIf
    | EndFor
    | Exitstmt
    | EndRepeat
    | Expression
    | For
    | If
    | Funchdr
    | Function
    | Identifier
    | IdentifierOnly
    | Implicit
    | Line
    | LineNumber
    | Loc
    | Local
    | Nothing
    | Operator
    | Parameter
    | ParenthesizedList
    | Primary
    | Procedure
    | ProcFnCall
    | Prochdr
    | Proc
    | Program
    | Reference
    | Remark
    | Repeat
    | Separator
    | Stmt
    | StmtList
    | Term
    | TerminalNodeImpl
    | Terminator
    | Value
    | Unknown
    | UnparenthesizedList

// A record for node data
type FSNode = {
//    RuleIndex  : int
    Exception  : RecognitionException
    SourceText : string
    Position   : int * int
    Children   : FSParseTree list
}

// Single record type to hold the node
and FSParseTree = {
    Kind : NodeKind
    Data : FSNode
}
    
/// <summary>
/// Represents a node in the final abstract syntax tree (AST).
/// </summary>
type ASTNode =
    { tokenType: NodeKind
      content: stringworking
      position: int * int
      children: ASTNode list }

// // Convert Antlr data types for parse nodes to Token Type field entries for FSParse
// let extractTokenType (node: ParserRuleContext) =
//     match node with
//     | :? SBParser.ProgramContext as programCtx ->
//         Program
//     | :? SBParser.LineContext as lineCtx ->
//         Line
//     | :? SBParser.ForloopContext as forCtx ->
//         For
//     
//     | :? SBParser.AssignmentContext as assignCtx ->
//         let variable = assignCtx.identifier().GetText()
//         let valueExpr = ParseTreeToAst (assignCtx.expr())
//         AssignmentNode(variable, valueExpr)
//
//     | :? SBParser.IfContext as ifCtx ->
//         let condition = parseTreeToAst (ifCtx.expr())
//         let thenBlock =
//             [ for i in 0 .. ifCtx.line().Length - 1 do
//                 yield parseTreeToAst (ifCtx.line(i)) ]
//         let elseBlock =
//             if ifCtx.Else() <> null then
//                 Some (
//                     [ for i in 0 .. ifCtx.lineNumber().Length - 1 do
//                         yield parseTreeToAst (ifCtx.lineNumber(i)) ]
//                 )
//             else None
//         IfNode(condition, thenBlock, elseBlock)
//
//     | :? SBParser.RepeatContext as repeatCtx ->
//         let variable = repeatCtx.ID().GetText()
//         let body =
//             [ for i in 0 .. repeatCtx.line().Length - 1 do
//                 yield parseTreeToAst (repeatCtx.line(i)) ]
//         RepeatNode(variable, body)
//
//     | :? SBParser.RemarkContext as remarkCtx ->
//         let commentText = remarkCtx.GetText()
//         RemarkNode(commentText)
//
//     | :? SBParser.BinaryContext as binaryCtx ->
//         let leftOperand = parseTreeToAst (binaryCtx.GetChild(0))
//         let operator = binaryCtx.GetChild(1).GetText()
//         let rightOperand = parseTreeToAst (binaryCtx.GetChild(2))
//         BinaryOperationNode(operator, leftOperand, rightOperand)
//
//     | :? SBParser.IdentifierContext as identifierCtx ->
//         let identifierText = identifierCtx.GetText()
//         IdentifierNode(identifierText)
//
//     | :? SBParser.LiteralContext as literalCtx ->
//         let literalValue = literalCtx.GetText()
//         LiteralNode(literalValue)
//
//     | _ ->
//         UnknownNode(node.GetText())
        

// Node in the AST
// type ASTNode = {
//     TokenType: TokenType
//     SourceText: string
//     Children: ASTNode list
//     //evaluatedType: EvaluatedType
//     //sourceReference: int
// }

// type EvaluatedType =
//     | Integer
//     | Real
//     | String

// Functions implemented
// convert SBParser type to string
// returns variable name and type from annotated name
// get the children of a context as an F# list of Antlr nodes without assuming children is visible
// implementation of map, input Antlr list output F# list
// copy generic list to F# list non destructively
// map while propagating state forward between operations on each element using an Antlr list of nodes
// map while propagating state forward between operations on each element using an F# list of values

// convert SBParser type to string
let identifyType code =
    match code with
    | SBParser.String -> "string"
    | SBParser.Integer -> "int"
    | SBParser.Real -> "float"
    | _ -> "void"

// returns variable name and type from annotated name
let getTypeFromAnnotation (name: string) =
    let len = name.Length - 1

    match name.Substring (len, 1) with
    | "%" ->
        let truncatedName = name.Substring (0, len)
        (truncatedName, SBParser.Integer)
    | "$" ->
        let truncatedName = name.Substring (0, len)
        (truncatedName, SBParser.String)
    | _ -> (name, SBParser.Real)

// get the children of a context as an F# list of Antlr nodes without assuming children is visible
let gatherChildren (context: IParseTree) =
    let rec gatherChildrenInner (context: IParseTree) index antlrList =
        let count = context.ChildCount

        match index with
        | n when n < count -> context.GetChild (index) :: gatherChildrenInner context (index + 1) antlrList
        | _ -> antlrList

    gatherChildrenInner context 0 []

// get the children of a context as an F# list of Antlr nodes without assuming children is visible
let gatherFSChildren (context: FSNode) =
    let rec gatherChildrenInner (context: FSNode) index antlrList =
        let count = context.Children.Length

        match index with
        | n when n < count -> context.Children[index] :: gatherChildrenInner context (index + 1) antlrList
        | _ -> antlrList

    gatherChildrenInner context 0 []

// implementation of map, input Antlr list output F# list
let mapAntlrList (mappingFunction: IParseTree -> 'b) (inputList: Collections.Generic.IList<IParseTree>) =
    let rec mapAntlrListWithIndex
        (mappingFunction: IParseTree -> 'b)
        (inputList: Collections.Generic.IList<IParseTree>)
        outputList
        index
        =
        match index with
        | len when len = inputList.Count -> outputList
        | _ ->
            let newElement = mappingFunction inputList[index]

            mapAntlrListWithIndex mappingFunction inputList (outputList @ [ newElement ]) (index + 1)

    mapAntlrListWithIndex mappingFunction inputList [] 0

// copy generic list to F# list non destructively
let copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) =
    mapAntlrList id (parentNode: Collections.Generic.IList<IParseTree>)

// map while propagating state forward between operations on each element using an Antlr list of nodes
let rec mapIter (paramList: IParseTree list) (state: State) dataType category =
    match paramList with
    | [] -> state
    | head :: tail ->
        let term =
            (head :?> SBParser.TermContext)
                .children[ 0 ]
                .GetText ()

        let symbol =
            { Name = term
              Scope = state.currentScope
              Category = category
              Type = dataType
              ParameterMechanism = Inapplicable }

        let newState = set symbol state
        mapIter tail newState dataType category

// map while propagating state forward between operations on each element using an F# list of values
let rec mapStringIter (paramList: string List) (state: State) dataType category =
    match paramList with
    | [] -> state
    | head :: tail ->
        let (symbol: Symbol) =
            { Name = head
              Scope = state.currentScope
              Category = category
              Type = dataType
              ParameterMechanism = Inapplicable }

        let newState = set symbol state
        mapStringIter tail newState dataType category

//let private parseUnparenthesizedParams (context: IParseTree) =
//    let paramList = context.GetChild(1) :?> SBParser.UnparenthesizedlistContext
//    let (fList:IParseTree list) = copyAntlrList paramList.children
//    fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))


//let private parseParamList (context: Collections.Generic.IList<IParseTree>) =
//    let paramList = context.GetChild(1).GetChild(1) :?> SBParser.ParenthesizedlistContext
//    let (fList:IParseTree list) = copyAntlrList paramList.children
//    fList |> List.filter (fun x -> not (x :? TerminalNodeImpl || x :? SBParser.SeparatorContext))


//let rec parseTreeMap fn (parseTree: ParseTree) =
//    match parseTree with
//    | Empty -> Empty
//    | Node(n) ->
//        let (newNode: Node) = fn n
//        let kids = newNode.children
//        let newKids = List.map (fun x -> parseTreeMap fn x) kids
//        Node {newNode with children=newKids}


// converts an antlr list of antlr nodes to an F# list of antlr nodes
//let private buildChildList (antlrTree: IParseTree) =
//    let rec buildChildListInternal (antlrTree: IParseTree) index childList =
//        match index with
//        | numberOfChildren when numberOfChildren = antlrTree.ChildCount -> childList
//        | _ -> buildChildListInternal antlrTree (index+1) (childList @ [antlrTree.GetChild(index)])
//    buildChildListInternal antlrTree 0 []
