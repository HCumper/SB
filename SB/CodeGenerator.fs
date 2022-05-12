module CodeGenerator

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable
open Utility

let addToCSharp item (state: State) =
    match state.currentScope with
    | "~Global" -> {state with outputGlobal = state.outputGlobal + item}
    | _ -> {state with outputProcFn = state.outputProcFn + item}

let rec private foldStateful f (initialValue: 'a) (name: string) (li: 'b list) (state: State) : 'a =
    match li with
        | [] -> initialValue
        | x::xs -> foldStateful f (f initialValue name x state) name xs state

// extract text from a parameter list
let private getParameterText accum (scope: string) (parameters: IParseTree) state =
    let dataType = 
        match SymbolTable.get (parameters.GetText()) scope state with
        | None -> SBParser.Void.ToString()  
        | Some n when n.Type = SBParser.Unknowntype -> "float"
        | Some n when n.Type = SBParser.Integer -> "int"
        | Some n -> n.Type.ToString()
    $@"{dataType} {parameters.GetText()}, {accum}"

// if there is not output for this construct just preserve the state
let private defaultAction _ _ state = state

// output code for procedures an functions
let private genDim context (state: State) =
    let (varName, dimensions) = Walker.WalkDim context
    let (name, _) = Utility.getTypeFromAnnotation varName
    let typeString =
        match SymbolTable.get name state.currentScope state with
            | None -> "void"
            | Some sym -> identifyType sym.Type
    let dimensionString = String.replicate dimensions.Tail.Length ","
    let dimStmt = SymbolTable.newLine + typeString + " " + name + "[" + dimensionString + "];"
    addToCSharp dimStmt state

// Operator precedence tightest binding lowest
type Precedence = Term=0 | Paren=1 | Primary=2 | Unary=3 | Range=4 | Switch=5 | Multiplicative=6 | Additive=7 | Shift=8 | Relational=9 | Equality=10 | And=11 | XOr=12 | Or=13 | ConditionalAnd=14 | ConditionalOr=15 | Coalescing=16 | Conditional=17 | Assignment=18

let private getPrecedence operator =
    match operator with
    | "NOT" -> Precedence.Unary
    | "SELect" -> Precedence.Switch
    | "*" | "/" | "MOD" | "DIV" -> Precedence.Multiplicative
    | "+" | "-" -> Precedence.Additive
    | "<" | ">" | "<=" | ">=" -> Precedence.Relational
    | "=" -> Precedence.Equality
    | "AND" -> Precedence.And
    | "XOR" -> Precedence.XOr
    | "OR" -> Precedence.Or
    | _ -> Precedence.Term

let rec private genExpression (context: IParseTree) =
    match context with
        | :? SBParser.TermContext -> 
            (context.GetText(), Precedence.Term)
        | :? SBParser.BinaryContext ->
            let binaryContext = context :?> SBParser.BinaryContext
            let (leftChild, leftPrecedence) = genExpression binaryContext.children[0]
            let (rightChild, rightPrecedence) = genExpression binaryContext.children[2]
            let operator = binaryContext.children[1].GetText()
            let expressionPrecedence = getPrecedence operator
            let leftChildString =
                match expressionPrecedence with
                | ep when ep < leftPrecedence -> "(" + leftChild + ")"
                | _ -> leftChild
            let rightChildString =
                match expressionPrecedence with
                | ep when ep < rightPrecedence -> "(" + rightChild + ")"
                | _ -> rightChild
            (leftChildString + " " + operator + " " + rightChildString, expressionPrecedence)
        | :? SBParser.ParenthesizedContext -> 
            genExpression (context :?> SBParser.ParenthesizedContext).children[1]  // discard parentheses
        | :? SBParser.UnaryAdditiveContext -> 
            (context.GetText(), Precedence.Unary)
        | _ -> raise (MyError("Expression generation error"))

let private genAssignment context (state: State) =
    let (lvalue, dimensionsk, rvalue) = Walker.WalkAssignment context
    let (name, _) = Utility.getTypeFromAnnotation lvalue
    //let operatingScope =
    //    match state.currentScope with
    //    | globalScope -> state.outputGlobal + $@"{varName} = "
    //    | _ -> state.outputProcFn + $@"{varName} = "
    //{state with outputProcFn = operatingScope}
//    let expressionString = genExpression rvalue
    let expr = genExpression rvalue
    let assignStmt = $@"{SymbolTable.newLine} {name} = { fst expr};"
    addToCSharp assignStmt state

// output code for procedures an functions
let private genProcFunc (routineName:string) parameters state =
    let funcType = 
        match SymbolTable.get routineName globalScope state with
        | Some t -> t.Type.ToString()
        | None -> SBParser.Void.ToString().ToLower()
    let paramList =
        match parameters with
        | [] -> ""
        | _ -> foldStateful getParameterText "" routineName parameters state

    let cSharp = 
        match paramList.Length with
        | 0 -> ""
        | _ -> "(" + paramList.Remove(paramList.Length - 2) + ")"
    let text = Templates.procFunc (funcType.ToLower()) routineName cSharp
    let x = state.outputProcFn + text
    {state with outputProcFn = x}

let private genEndDefine _ _ state =
    let x = state.outputProcFn + "}" + newLine
    {state with outputProcFn = x}

let private genBinary varName parameters (state: State) =
    let x = state.outputProcFn + "xxx"
    {state with outputProcFn = x}

let private genTerminal _ _ (state: State) =
    let operatingScope =
        match state.currentScope with
        | globalScope -> state.outputGlobal + "}" + newLine
        | _ -> state.outputProcFn + "}" + newLine
    {state with outputProcFn = operatingScope}

let private noAction _ _ (state: State) =
    state

///////////////////////////////// Tree traversal stuff ////////////////////////////////////////
let rec private WalkAcross (context : IParseTree) index state =
    let result = 
        let count = context.ChildCount
        match index with
        | n when n < count ->
            let (context, state) = WalkDown (context.GetChild(index) : IParseTree) state
            WalkAcross ((context: IParseTree).Parent : IParseTree) (index+1) state 
        | _ -> state
    result
and 
    private WalkDown (context : IParseTree) (state: State) =
        let newState = 
            match context with
            | :? SBParser.DimContext -> genDim context state 
            | :? SBParser.AssignmentContext -> genAssignment context state 
            //| :? SBParser.ImplicitContext -> addImplicitSymbol context state
            //| :? SBParser.LocContext -> addLocalSymbol context state
            //| :? SBParser.ProchdrContext -> addProcedureSymbol context state
            //| :? SBParser.FunchdrContext -> addFunctionSymbol context state
            //| :? SBParser.EndDefContext -> addEndDefSymbol state
            | :? SBParser.LongforContext -> genLongFor context state
            //| :? SBParser.ShortforContext -> addShortForSymbol context state
            | _ -> state
        (context, WalkAcross (context:IParseTree) 0 newState )
and private genLongFor (context: IParseTree) (state:State) =
    let (loopVar, initialValue, finalValue, step) = Walker.WalkFor context state
    //call walk across for line contexts only
    state

// top level only
let WalkTreeRoot (context: ParserRuleContext) (state: State) =
    WalkDown context state 
