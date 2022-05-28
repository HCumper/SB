module Utility

open Antlr4.Runtime.Tree
open SBLib
open SymbolTable
open Antlr4.Runtime.Tree
open System

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
let getTypeFromAnnotation (name:string) =
    let len = name.Length - 1
    match name.Substring(len, 1) with
    | "%" -> 
        let truncatedName = name.Substring(0, len) 
        (truncatedName, SBParser.Integer)
    | "$" -> 
        let truncatedName = name.Substring(0, len) 
        (truncatedName, SBParser.String)
    | _ -> 
        (name, SBParser.Real)
    
//get the children of a context as an F# list of Antlr nodes without assuming children is visible
let gatherChildren (context: IParseTree) = 
    let rec innerGatherChildren (context: IParseTree) index antlrList = 
        let count = context.ChildCount 
        match index with
        | n when n < count ->
            context.GetChild(index)::innerGatherChildren context (index+1) antlrList 
        | _ -> antlrList
    innerGatherChildren context 0 []

// implementation of map, input Antlr list output F# list
let mapAntlrList (mappingFunction: IParseTree -> 'b) (inputList: Collections.Generic.IList<IParseTree>) =
    let rec mapAntlrListWithIndex (mappingFunction: 'a -> 'b) (inputList: Collections.Generic.IList<IParseTree>) outputList index =
        match index with
        | len when len = inputList.Count -> outputList
        | _ -> 
            let newElement = mappingFunction inputList[index]
            mapAntlrListWithIndex mappingFunction inputList (outputList @ [newElement]) (index+1)
    mapAntlrListWithIndex mappingFunction inputList [] 0

// copy generic list to F# list non destructively
let copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) = mapAntlrList (fun x -> x) (parentNode: Collections.Generic.IList<IParseTree>)

// map while propagating state forward between operations on each element using an Antlr list of nodes
let rec mapIter (paramList:IParseTree list) (state: State) dataType category =
    match paramList with
    | [] -> state
    | head::tail ->
        let term = (head :?> SBParser.TermContext).children[0].GetText()
        let (symbol: Symbol) = {Name = term; Scope = state.currentScope; Category=category; Type=dataType; ParameterMechanism = Inapplicable}
        let newState = set symbol state;
        mapIter tail newState dataType category

// map while propagating state forward between operations on each element using an F# list of values
let rec mapStringIter (paramList:string List) (state: State) dataType category =
    match paramList with
    | [] -> state
    | head::tail ->
        let (symbol: Symbol) = {Name = head; Scope = state.currentScope; Category=category; Type=dataType; ParameterMechanism = Inapplicable}
        let newState = set symbol state;
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


