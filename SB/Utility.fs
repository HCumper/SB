module Utility

open System
open System.IO
open System.Text
open Antlr4.Runtime
open Antlr4.Runtime.Tree
open FSharpPlus
open FSharpPlus.Data

/// <summary>
/// Represents the category of a symbol in the symbol table.
/// Broader than NodeKind.
/// </summary>
type CategoryType =
    | Variable    // A standard variable.
    | Dim         // A DIM declaration (often for arrays or fixed storage).
    | Procedure   // A procedure declaration.
    | Function    // A function declaration.
    | Keyword     // A reserved keyword.
    | Array       // Specifically for array declarations.
    | Constant    // A constant value.
    | Unknown     // Fallback for symbols that don't match any known category.

/// <summary>
/// Represents possible symbol types.
/// </summary>
type SymbolType =
    | Variable of string
    | Function of string * string list  // Function with a return type and argument types
    | Constant of string
    | TypeDefinition of string
    | Keyword of string

/// <summary>
/// Types for AST nodes and corresponding grammar rule behaviors.
/// </summary>
type NodeKind =
    | Any   // For pattern matching only
    | ArrayOrFunctionCall
    | Assignment
    | AssignmentTarget
    | BinaryExpr
    | Body
    | CallExpr
    | Dim
    | EndDef
    | EndIf
    | EndFor
    | EndRepeat
    | Exitstmt
    | Expression
    | For
    | ID
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
    | NumberLiteral
    | Operator
    | Parameters
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
    | StringLiteral
    | Term
    | TerminalNodeImpl
    | Terminator
    | TypedIdentifier
    | Value
    | UnaryExpr    
    | Unknown
    | UnparenthesizedList
    | Plus
    | Minus
    | Multiply
    | Divide
    | Mod
    | Div

/// <summary>
/// Represents a node in the abstract syntax tree (AST).
/// </summary>
type ASTNode = {
    TokenType: NodeKind
    Value: string
    Position: int * int
    Children: ASTNode list
}

/// <summary>
/// A scope holds a name and an immutable map of symbols.
/// </summary>
type Scope<'T> = {
    Name: string
    Symbols: Map<string, 'T>  // symbol name to symbol
}

/// <summary>
/// Represents how a parameter is passed to a function or procedure.
/// </summary>
type ParameterMechanismType =
    | Value
    | Reference

type SBTypes =
    | String
    | Integer
    | Real
    | Unknown
    
/// Fields common to all symbols
type CommonSymbol = {
    Name: string
    SymbolKind: NodeKind
    EvaluatedType: SBTypes
    Category: CategoryType
    Position: int * int
}

/// Define a record type for function parameters
/// Does not contain parameters
type ParameterSymbol = {
    Common: CommonSymbol
    ParameterMechanism: ParameterMechanismType
}

/// Define a record type for arrays
/// Containes dimensions
type ArraySymbol = {
    Common: CommonSymbol
    Dimensions: int list
}

/// Use the record types inside the DU
type Symbol =
    | Common of CommonSymbol
    | Parameter of ParameterSymbol
    | Array of ArraySymbol
//  | value?    

/// Functions

let globalScope = "~Global"

/// Function to extract the symbol name from a symbol
let getSymbolName (symbol: Symbol) : string =
    match symbol with
    | Common sym -> sym.Name
    | Parameter param -> param.Common.Name
    | Array arr -> arr.Common.Name

/// <summary>
/// Active pattern to simplify type identification based on SBParser types.
/// </summary>
let (|StringType|IntegerType|RealType|VoidType|) code =
    match code with
    | SBParser.String -> StringType
    | SBParser.Integer -> IntegerType
    | SBParser.Real -> RealType
    | _ -> VoidType

/// <summary>
/// Converts an SBParser type to its corresponding string representation.
/// </summary>
let identifyType code =
    match code with
    | StringType -> "string"
    | IntegerType -> "int"
    | RealType -> "float"
    | VoidType -> "void"

/// <summary>
/// Extracts the variable name and its type from an annotated name.
/// </summary>
let getTypeFromAnnotation (name: string) : string * _ =
    let len = name.Length - 1
    match name[len] with
    | '%' -> name.Substring(0, len), SBParser.Integer
    | '$' -> name.Substring(0, len), SBParser.String
    | _ -> name, SBParser.Real

/// <summary>
/// Gets the children of an IParseTree context as an F# list.
/// </summary>
let gatherChildren (context: IParseTree) : IParseTree list =
    [ for index in 0 .. context.ChildCount - 1 do yield context.GetChild(index) ]

/// <summary>
/// Maps over an Antlr list with a provided mapping function.
/// </summary>
let mapAntlrList (mappingFunction: IParseTree -> 'b) (inputList: Collections.Generic.IList<IParseTree>) : 'b list =
    [ for item in inputList do yield mappingFunction item ]

/// <summary>
/// Copies an Antlr list to an F# list non-destructively.
/// </summary>
let copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) : IParseTree list =
    mapAntlrList id parentNode

/// <summary>
/// Helper function to create an ASTNode.
/// </summary>
let createAstNode (tokenType: NodeKind) (value: string) (position: int * int) (children: ASTNode list) : ASTNode =
    { TokenType = tokenType; Value = value; Position = position; Children = children }

/// <summary>
/// Recursively pretty-prints an AST node and its descendants with indentation.
/// </summary>
let rec prettyPrintAst (node: ASTNode) (indent: int) : string =
    let padding = String.replicate indent " "
    let currentLine = sprintf "%s- %A (\"%s\", pos=%A)\n" padding node.TokenType node.Value node.Position
    let childLines = node.Children |> List.map (fun child -> prettyPrintAst child (indent + 2)) |> String.concat ""
    currentLine + childLines

let rec printAST (indent: string) (node: ASTNode) =
    printfn $"%s{indent}{node.TokenType}: \"%s{node.Value}\""
    node.Children |> List.iter (printAST (indent + "  "))
        
///////////////////////////////////////////////////////
// Monads
///////////////////////////////////////////////////////

/// <summary>
/// Represents the State monad: a function from state 's to a result 'a and a new state 's.
/// </summary>
type State<'s, 'a> = 's -> 'a * 's

/// <summary>
/// Computation expression builder for the State monad.
/// </summary>
type StateBuilder() =
    member _.Bind(x: State<'s, 'a>, f: 'a -> State<'s, 'b>) : State<'s, 'b> =
        fun state ->
            let (result, newState) = x state
            f result newState
    member _.Return(x: 'a) : State<'s, 'a> =
        fun state -> (x, state)
    member _.ReturnFrom(x: State<'s, 'a>) : State<'s, 'a> = x
    member _.Zero() : State<'s, unit> =
        fun state -> ((), state)
    member _.Delay(f: unit -> State<'s, 'a>) : State<'s, 'a> = f()
    member _.For(sequence: seq<'T>, body: 'T -> State<'s, unit>) : State<'s, unit> =
        fun s ->
            let mutable s' = s
            for x in sequence do
                let (_, newState) = body x s'
                s' <- newState
            ((), s')

/// Instantiate the State computation builder.
let state = StateBuilder()

/// Helper function to get the current state.
let get : State<'s, 's> = fun s -> (s, s)

/// Helper function to update the state.
let put newState : State<'s, unit> = fun _ -> ((), newState)
/// Symbol Table builder

// let updateStateWithSymbol (symbol: Symbol) (state: State) : State =
//     { state with symTab =  state.symTab.Add({ Name = symbol.Name; Scope = symbol.Scope }, symbol) }
//     
/// A stateful computation that processes one IParseTree node.
/// It extracts the term from the node, creates a Symbol using the current state's scope,
/// updates the state with that Symbol, and finally returns the Symbol.
// let processNode (node: IParseTree) dataType category : State<State, Symbol> =
//     state {
//         // Use pattern matching to safely cast the node to the expected type.
//         let term =
//             match node with
//             | :? SBParser.TermContext as termCtx ->
//                 // Assuming the term is in the first child.
//                 termCtx.children[0].GetText()
//             | _ ->
//                 failwith "Unexpected node type; expected SBParser.TermContext."
//                 
//         // Retrieve the current state.
//         let! currentState = get
//         
//         // Create a new Symbol using the current state's scope.
//         let symbol = {
//             Name = term
//             Scope = currentState.currentScope
//             Category = category
//             Type = dataType
//             ParameterMechanism = Inapplicable
//         }
//         
//         // Update the state with the new symbol.
//         do! put (updateStateWithSymbol symbol currentState)
//         
//         // Return the symbol as the result of this computation.
//         return symbol
//     }

// no longer used
/// Processes a list of IParseTree nodes, threading state through each operation,
/// and returns the final state after all nodes have been processed.
// let mapIter (nodes: IParseTree list) (initialState: State) dataType category : State =
//     // Traverse the list, applying the stateful processNode function to each node.
//     let computation : State<State, Symbol list> =
//         traverse (fun node -> processNode node dataType category) nodes
//         
//     // Run the computation with the provided initial state.
//     let (_symbols, finalState) = State.run computation initialState
//     
//     // Return the final state.
//     finalState

/// Map while propagating state forward between operations on each element using an Antlr list of nodes
// let rec mapIter (paramList: IParseTree list) (state: State) dataType category =
//     paramList |> List.fold (fun state head ->
//         let term = (head :?> SBParser.TermContext).children.[0].GetText()
//         let symbol = {
//             Name = term
//             Scope = state.currentScope
//             Category = category
//             Type = dataType
//             ParameterMechanism = Inapplicable
//         }
//         set symbol state
//     ) state

/// Map while propagating state forward between operations on each element using an F# list of values
// let rec mapStringIter (paramList: string list) (state: State) dataType category =
//     paramList |> List.fold (fun state head ->
//         let symbol = {
//             Name = head
//             Scope = state.currentScope
//             Category = category
//             Type = dataType
//             ParameterMechanism = Inapplicable
//         }
//         set symbol state
//     ) state

/// Replace subtree in AST
/// Tree pattern matching
type ASTPattern = {
    PatternTokenType: NodeKind option
    PatternContent: string option // `None` means ignore, `"@parent"` means match parent content
    PatternPosition: (int * int) option
    PatternChildren: ASTPattern list option
}

let rec private matchPattern (parentContent: string option) (pattern: ASTPattern) (node: ASTNode) =
    let matchTokenType = 
        match pattern.PatternTokenType with
        | Some pType -> pType = node.TokenType
        | None -> true

    let matchContent = 
        match pattern.PatternContent with
        | Some "@parent" -> parentContent = Some node.Value  // Match inherited parent content
        | Some pContent -> pContent = node.Value
        | None -> true

    let matchPosition = 
        match pattern.PatternPosition with
        | Some pPos -> pPos = node.Position
        | None -> true

    let matchChildren =
        match pattern.PatternChildren with
        | Some pChildren -> 
            List.length pChildren = List.length node.Children &&
            List.forall2 (matchPattern (Some node.Value)) pChildren node.Children
        | None -> true

    matchTokenType && matchContent && matchPosition && matchChildren

/// Replace a pattern in the AST with a new subtree
let rec replacePattern (pattern: ASTPattern) (replacementTemplate: ASTNode) (node: ASTNode) =
    if matchPattern None pattern node then
        if replacementTemplate.Value = "@parent" then
            { replacementTemplate with Value = node.Value }  // Inherit content
        else
            replacementTemplate
    else
        { node with Children = List.map (replacePattern pattern replacementTemplate) node.Children }

///////////////////////////////////////////////////////
// Result Monad (Computation Expression)
///////////////////////////////////////////////////////

/// <summary>
/// Computation expression builder for Result<'T, 'E>.
/// </summary>
type ResultBuilder() =
    member _.Return(x) : Result<'T, 'E> = Ok x
    member _.ReturnFrom(m: Result<'T, 'E>) = m
    member _.Bind(m: Result<'T, 'E>, f: 'T -> Result<'U, 'E>) : Result<'U, 'E> =
        Result.bind f m
    member _.Zero() : Result<unit, 'E> = Ok ()
    member _.Combine(m: Result<'T, 'E>, f: unit -> Result<'U, 'E>) : Result<'U, 'E> =
        match m with
        | Ok _ -> f ()
        | Error e -> Error e
    member _.Delay(f: unit -> Result<'T, 'E>) = f
    member _.Run(f: unit -> Result<'T, 'E>) = f ()
    member _.TryWith(m: unit -> Result<'T, 'E>, handler: exn -> Result<'T, 'E>) =
        try m () with e -> handler e
    member _.TryFinally(m: unit -> Result<'T, 'E>, compensation: unit -> unit) =
        try m () finally compensation ()
    member _.Using(resource: #IDisposable, body: #IDisposable -> Result<'T, 'E>) =
        try body resource finally resource.Dispose()
    member _.While(condition: unit -> bool, body: unit -> Result<unit, 'E>) =
        let rec loop () =
            if condition () then
                match body () with
                | Ok () -> loop ()
                | Error e -> Error e
            else Ok ()
        loop ()
    member _.For(sequence: seq<'T>, body: 'T -> Result<unit, 'E>) =
        let rec loop seq =
            match Seq.tryHead seq with
            | None -> Ok ()
            | Some x ->
                match body x with
                | Ok () -> loop (Seq.tail seq)
                | Error e -> Error e
        loop sequence

/// Instantiate the Result computation builder.
let result = ResultBuilder()

