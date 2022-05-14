module TreeRewriter

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open Walker

type TokenType =
    Program | Line | StmtList | Stmt

// The one and only
type Node = {
    tokenType: TokenType;
    content: string;
    // evaluated type
    // source code reference
    children: Node list;
}

// converts an antlr list of antlr nodes to an F# list of antlr nodes
let buildChildList (antlrTree: IParseTree) =
    let rec buildChildListInternal (antlrTree: IParseTree) index childList =
        match index with
        | numberOfChildren when numberOfChildren = antlrTree.ChildCount -> childList
        | _ -> buildChildListInternal antlrTree (index+1) (childList @ [antlrTree.GetChild(index)])
    buildChildListInternal antlrTree 0 []

let rec private WalkDown (context : IParseTree) (parseTree: Node) =
    let childList = buildChildList context
    let node = {tokenType=Program; content=context.GetText(); children=[]}
    (context, WalkList childList 0 parseTree )
and
    private WalkList (contextList: IParseTree List) index parseTree =
        let count = contextList.Length
        match index with
        | n when n < count ->
            let (contextList) = WalkDown (contextList[index] : IParseTree) parseTree
            WalkList ((context: IParseTree).Parent : IParseTree) (index+1) parseTree 
        | _ -> parseTree
        


// converts an antlr tree of antlr nodes to an F# tree of F# nodes
let TranslateParseTree (context: IParseTree) (parseTree: Node) =
    let node = {tokenType=Program; content=context.GetText(); children=[]}
    WalkDown context parseTree




//and
//    ParseTree = Empty | Node of Node

//let rec parseTreeMap fn (parseTree: ParseTree) =
//    match parseTree with
//    | Empty -> Empty
//    | Node(n) -> 
//        let (newNode: Node) = fn n
//        let kids = newNode.children
//        let newKids = List.map (fun x -> parseTreeMap fn x) kids
//        Node {newNode with children=newKids}

//let x (node: Node) = 
//    let myNode = Node {tokenType=Program; content="CCC"; children=[]}
//    let pt = ParseTree(node)
//    4
//let rec ParseTreeMap fn ParseTree =
//    match ParseTree with
//    | Empty -> Empty
//    | ParseTree(l, n, r) -> 
//        ParseTree(ParseTreeMap fn l, fn n, ParseTreeMap fn r)


//let rec isMember element (tree: ParseTree<int>) =
//    match ParseTree with
//    | Empty -> false
//    | ParseTree(l, n, r) ->
//        match n with
//        | i when i = element -> true
//        | _ -> isMember element l || isMember element r

//let rec insert newValue existingParseTree =
//  match existingParseTree with
//  | Empty -> ParseTree (Empty, newValue, Empty)
//  | ParseTree(left, root, right) ->
//    match root with
//    | i when i > newValue -> ParseTree(insert newValue left, root, right)
//    | j when j < newValue -> ParseTree(left, root, insert newValue right)
//    | _ -> existingParseTree

//let rec insertList valueList ParseTree =
//  match valueList with
//  | [] -> ParseTree
//  | hd::tl -> 
//    insert hd ParseTree
//    |> insertList tl

//let rec update oldValue newValue ParseTree =
//  match ParseTree with
//  | Empty -> ParseTree (Empty, oldValue, Empty)
//  | ParseTree(l,n,r) ->
//    match n with
//    | i when oldValue < i -> ParseTree(update oldValue newValue l, n, r)
//    | j when oldValue > j -> ParseTree(l, n, update oldValue newValue r)
//    | _ -> ParseTree(l, newValue, r)

//let rec delete element ParseTree =
//    match element with
//    | Empty -> Empty
//    | ParseTree (l, n, r) ->
//    match n with
//        | i when i > ParseTree -> ParseTree(delete l ParseTree, n, r)
//        | j when j < ParseTree -> ParseTree(l, n, delete r ParseTree)
//        | _ -> Empty

//let rec ParseTreeMap fn ParseTree =
//    match ParseTree with
//    | Empty -> Empty
//    | ParseTree(l, n, r) -> 
//        ParseTree(ParseTreeMap fn l, fn n, ParseTreeMap fn r)

//let rec ParseTreeFold fn accumulator ParseTree =
//    match ParseTree with
//    | Empty -> accumulator
//    | ParseTree (l, n, r) ->
//        let accumulator2 = ParseTreeFold fn accumulator l
//        let x = fn n
//        let accumulator3  = ParseTreeFold fn accumulator r
//        accumulator2 + x + accumulator3
  
//let display x = printfn "%d" x
//let double x = 2 * x

//let ParseTree = insert 7 Empty
//let ParseTree1 = insert 3 ParseTree
//let ParseTree2 = insert 8 ParseTree1
//let ParseTree3 = insert 4 ParseTree2
//let ParseTree6 = insert 22 ParseTree3
//let ParseTree7 = insert 0 ParseTree6

//let doubled = ParseTreeMap double ParseTree7 
//let addUp x  = x*x
//ParseTreeMap display doubled |> ignore
//printfn ""
//let final = ParseTreeFold  (fun x->x*x) 2 doubled

//let a = isMember 44 doubled
//let b = isMember 109 doubled

//let ParseTree4 = update 3 99 ParseTree7
//let ParseTree5 = delete ParseTree4 3

//let sorted = insertList [15; 4; 73; 25; 4; 1; 81; 14] Empty |> ParseTreeMap (fun x->printfn "%d" x) 
////let sorted2 = List.map insert [15; 4; 73; 25; 4; 1; 81; 14] Empty //|> ParseTreeMap (fun x->printfn "%d" x) 

let c=3