module CodeGenerator

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open SB
open SymbolTable
open Utility

let outputCs translation (state: State) =
    match state.currentScope with
    | globalScope -> { state with outputProcFn = state.outputProcFn + translation }
    | _ -> { state with outputProcFn = state.outputGlobal + translation }

let addToCSharp (item: string) (state: State) =
    match state.currentScope with
    | "~Global" -> { state with outputGlobal = state.outputGlobal + item }
    | _ -> { state with outputProcFn = state.outputProcFn + item }

let addStateToCSharp (item: State) (state: State) =
    match state.currentScope with
    | "~Global" -> { state with outputGlobal = state.outputGlobal + item.outputGlobal }
    | _ -> { state with outputProcFn = state.outputProcFn + item.outputProcFn }

let rec private foldStateful f (initialValue: 'a) (name: string) (li: 'b list) (state: State) : 'a =
    match li with
    | [] -> initialValue
    | hd :: tl -> foldStateful f (f initialValue name hd state) name tl state

// translate SBParser numeric type to string
let translateType typeCode =
    match typeCode with
    | SBParser.Unknowntype -> "float"
    | SBParser.Real -> "float"
    | SBParser.Integer -> "int"
    | SBParser.String -> "string"
    | SBParser.Void -> "void"
    | _ -> "void"

// extract text from a parameter list
let private getParameterText accum (scope: string) (parameter: string) state =
    let dataType =
        match SymbolTable.get parameter scope state with
        | None -> "void"
        | Some n -> translateType n.Type

    $@"{accum} {dataType} {parameter},"

// if there is not output for this construct just preserve the state
let private defaultAction _ _ state = state

// output code for dims
let private genDim context (state: State) =
    let (varName, dimensions) =
        Walker.WalkDim context

    let (name, _) =
        Utility.getTypeFromAnnotation varName

    let typeString =
        match SymbolTable.get name state.currentScope state with
        | None -> "void"
        | Some sym -> identifyType sym.Type

    let dimensionString =
        String.replicate dimensions.Tail.Length ","

    let dimStmt =
        SymbolTable.newLine
        + typeString
        + " "
        + name
        + "["
        + dimensionString
        + "];"

    addToCSharp dimStmt state

// Operator precedence tightest binding lowest
type Precedence =
    | Term = 0
    | Paren = 1
    | Primary = 2
    | Unary = 3
    | Range = 4
    | Switch = 5
    | Multiplicative = 6
    | Additive = 7
    | Shift = 8
    | Relational = 9
    | Equality = 10
    | And = 11
    | XOr = 12
    | Or = 13
    | ConditionalAnd = 14
    | ConditionalOr = 15
    | Coalescing = 16
    | Conditional = 17
    | Assignment = 18

let private getPrecedence operator =
    match operator with
    | "NOT" -> Precedence.Unary
    | "SELect" -> Precedence.Switch
    | "*"
    | "/"
    | "MOD"
    | "DIV" -> Precedence.Multiplicative
    | "+"
    | "-" -> Precedence.Additive
    | "<"
    | ">"
    | "<="
    | ">=" -> Precedence.Relational
    | "=" -> Precedence.Equality
    | "AND" -> Precedence.And
    | "XOR" -> Precedence.XOr
    | "OR" -> Precedence.Or
    | _ -> Precedence.Term

let rec private genExpression (context: IParseTree) =
    match context with
    | :? SBParser.TermContext -> (context.GetText (), Precedence.Term)
    | :? SBParser.BinaryContext ->
        let binaryContext =
            context :?> SBParser.BinaryContext

        let (leftChild, leftPrecedence) =
            genExpression binaryContext.children[0]

        let (rightChild, rightPrecedence) =
            genExpression binaryContext.children[2]

        let operator =
            binaryContext.children[ 1 ].GetText ()

        let expressionPrecedence =
            getPrecedence operator

        let leftChildString =
            match expressionPrecedence with
            | ep when ep < leftPrecedence -> "(" + leftChild + ")"
            | _ -> leftChild

        let rightChildString =
            match expressionPrecedence with
            | ep when ep < rightPrecedence -> "(" + rightChild + ")"
            | _ -> rightChild

        (leftChildString
         + " "
         + operator
         + " "
         + rightChildString,
         expressionPrecedence)
    | :? SBParser.ParenthesizedContext ->
        genExpression
            (context :?> SBParser.ParenthesizedContext)
                .children[1]
    //| :? SBParser.UnaryAdditiveContext ->
    //    (context.GetText(), Precedence.Unary)
    | _ -> raise (MyError ("Expression generation error"))

let private genAssignment context (state: State) =
    let (lvalue, dimensions, rvalue, targetDimensions) =
        Walker.WalkAssignment context

    let (name, _) =
        Utility.getTypeFromAnnotation lvalue
    //let operatingScope =
    //    match state.currentScope with
    //    | globalScope -> state.outputGlobal + $@"{varName} = "
    //    | _ -> state.outputProcFn + $@"{varName} = "
    //{state with outputProcFn = operatingScope}
//    let expressionString = genExpression rvalue
    //let expr = genExpression rvalue
    //let assignStmt = $@"{SymbolTable.newLine} {name} = { fst expr};"
    //addToCSharp assignStmt state
    state

let private genEndDefine _ _ state =
    let x = state.outputProcFn + "}" + newLine
    { state with outputProcFn = x }

let private genBinary varName parameters (state: State) =
    let x = state.outputProcFn + "xxx"
    { state with outputProcFn = x }

let private genTerminal _ _ (state: State) =
    let operatingScope =
        match state.currentScope with
        | "~Global" -> state.outputGlobal + "}" + newLine
        | _ -> state.outputProcFn + "}" + newLine

    { state with outputProcFn = operatingScope }

let private noAction _ _ (state: State) = state

let private genNext (_: IParseTree) (state: State) =
    addToCSharp $@"{SymbolTable.newLine}continue;{SymbolTable.newLine}" state

let private varString (tokenType: int) (symbols: Symbol List) =
    let prefix =
        match tokenType with
        | SBParser.Integer -> "int "
        | SBParser.Real -> "float "
        | _ -> "string "

    let vars =
        symbols
        |> List.filter (fun sym -> sym.Type = tokenType)

    match vars.Length with
    | 0 -> ""
    | _ ->
        let commaList =
            (prefix, vars)
            ||> List.fold (fun accum element -> accum + element.Name + ", ")

        commaList.Remove (commaList.Length - 2) + ";"

// walk a child list
let rec private WalkAcross (context: IParseTree) index state =
    let count = context.ChildCount

    match index with
    | n when n < count ->
        let (context, newState) =
            WalkDown (context.GetChild (index): IParseTree) state

        WalkAcross ((context: IParseTree).Parent: IParseTree) (index + 1) newState
    | _ -> state
// generate output for a node depending on type and then call walk across to traverse it children
and private WalkDown (context: IParseTree) (state: State) =
    let newState =
        match context with
        | :? SBParser.DimContext -> genDim context state
        | :? SBParser.AssignmentContext -> genAssignment context state
        | :? SBParser.ForloopContext -> genFor context state
        | :? SBParser.NextstmtContext -> genNext context state
        | :? SBParser.ProcContext
        | :? SBParser.FuncContext -> genProcFunc context state
        | _ -> WalkAcross (context: IParseTree) 0 state

    (context, newState)

and private genFor (context: IParseTree) (state: State) =
    let (loopVar, initialValue, finalValue, step) =
        Walker.WalkFor context

    let forStmt =
        $@"{SymbolTable.newLine} for ({loopVar} = {initialValue}; {loopVar} <= {finalValue}; {loopVar} += {step} {{"

    let state = addToCSharp forStmt state
    let state = WalkAcross context 0 state
    addToCSharp $@"{SymbolTable.newLine}}}" state

and private genProcFunc context state =
    let (routineName, parameters) =
        Walker.WalkProcFunc (context.GetChild (0))

    let routineType =
        match SymbolTable.get routineName globalScope state with
        | Some t -> t.Type.ToString ()
        | None -> translateType SBParser.Void

    let routineInt =
        translateType (routineType |> int)

    let paramList =
        match parameters with
        | [] -> ""
        | _ -> foldStateful getParameterText "" routineName [] state

    let cSharp =
        match paramList.Length with
        | 0 -> ""
        | _ ->
            "("
            + paramList.Remove (paramList.Length - 1)
            + ")"

    let text =
        Templates.procFunc routineInt routineName cSharp

    let newText = state.outputProcFn + text

    let declaration =
        { { state with outputProcFn = newText } with currentScope = routineName }

    //Demo of annotating a node
    //let (values:ParseTreeProperty<int>) = new ParseTreeProperty<int>()
    //let y = values.Put(context, 36);
    //let x = values.Get(context);
    //values.removeFrom(context);

    // Add local variable declarations
    let locals =
        declaration.symTab
        |> Map.filter (fun { Key.Name = _; Scope = scope } symbol ->
            scope = routineName
            && symbol.Category = CategoryType.Local)

    let symbols =
        ([], locals)
        ||> Map.fold (fun accum _ symbol -> accum @ [ symbol ])

    let integersString =
        symbols |> varString SBParser.Integer

    let stringsString =
        symbols |> varString SBParser.String

    let floatsString =
        symbols |> varString SBParser.Real

    let y =
        $@"{declaration.outputProcFn} {integersString}{SymbolTable.newLine}{stringsString}{SymbolTable.newLine} {floatsString}{SymbolTable.newLine}"

    let readyForBody =
        { declaration with outputProcFn = y }
    //TODO declarations for local arrays
    //TODO function return types
    let withBody =
        WalkAcross context 0 readyForBody

    let bodyAdded =
        addToCSharp $@"{SymbolTable.newLine} " withBody

    addToCSharp $@"{SymbolTable.newLine}}}{SymbolTable.newLine}{SymbolTable.newLine}" bodyAdded

// top level only
let walkTreeRoot (context: ParserRuleContext) (state: State) = WalkDown context state
