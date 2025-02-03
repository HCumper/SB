module Utility

open Antlr4.Runtime.Tree
open SymbolTable
open System
open Antlr4.Runtime
open FSharpPlus
open FSharpPlus.Data

/// Types of tokens used in the FS parse tree
type NodeKind =
    | Any   // Never assigned to a node, for pattern matching only
    | ArrayOrFunctionCall
    | Assignment
    | AssignmentTarget
    | BinaryExpr
    | Body
    | CallExpr
    | Dim
    | EndDef // TODO create node for
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

/// Represents a node in the parse tree
and FSParseTree = {
    Kind: NodeKind
    Exception: RecognitionException option
    SourceText: string
    Position: int * int
    Children: FSParseTree list
}

/// Represents a node in the abstract syntax tree (AST)
type ASTNode = {
    TokenType: NodeKind
    Content: string
    Position: int * int
    Children: ASTNode list
}

/// Active pattern to simplify type identification
let (|StringType|IntegerType|RealType|VoidType|) code =
    match code with
    | SBParser.String -> StringType
    | SBParser.Integer -> IntegerType
    | SBParser.Real -> RealType
    | _ -> VoidType

/// Convert SBParser type to string
let identifyType code =
    match code with
    | StringType -> "string"
    | IntegerType -> "int"
    | RealType -> "float"
    | VoidType -> "void"

/// Returns variable name and type from annotated name
let getTypeFromAnnotation (name: string) =
    let len = name.Length - 1
    match name[len] with
    | '%' -> name.Substring(0, len), SBParser.Integer
    | '$' -> name.Substring(0, len), SBParser.String
    | _ -> name, SBParser.Real

/// Get the children of a context as an F# list of Antlr nodes
let gatherChildren (context: IParseTree) =
    [ for index in 0 .. context.ChildCount - 1 do yield context.GetChild(index) ]

/// Get the children of a context as an F# list of FS nodes
let gatherFSChildren (context: FSParseTree) =
    context.Children

/// Map over an Antlr list with a mapping function
let mapAntlrList (mappingFunction: IParseTree -> 'b) (inputList: Collections.Generic.IList<IParseTree>) =
    [ for item in inputList do yield mappingFunction item ]

/// Copy an Antlr list to an F# list non-destructively
let copyAntlrList (parentNode: Collections.Generic.IList<IParseTree>) =
    mapAntlrList id parentNode




// Define the State monad type: a function from state 's to a result 'a and a new state 's.
type State<'s, 'a> = 's -> 'a * 's

// Define a computation expression builder for the State monad.
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

// Instantiate the builder.
let state = StateBuilder()

// Helper function to get the current state.
let get : State<'s, 's> = fun s -> (s, s)

// Helper function to update (or "put") a new state.
let put newState : State<'s, unit> = fun _ -> ((), newState)
/// Symbol Table builder

let updateStateWithSymbol (symbol: Symbol) (state: State) : State =
    { state with symTab =  state.symTab.Add({ Name = symbol.Name; Scope = symbol.Scope }, symbol) }
    
/// A stateful computation that processes one IParseTree node.
/// It extracts the term from the node, creates a Symbol using the current state's scope,
/// updates the state with that Symbol, and finally returns the Symbol.
let processNode (node: IParseTree) dataType category : State<State, Symbol> =
    state {
        // Use pattern matching to safely cast the node to the expected type.
        let term =
            match node with
            | :? SBParser.TermContext as termCtx ->
                // Assuming the term is in the first child.
                termCtx.children[0].GetText()
            | _ ->
                failwith "Unexpected node type; expected SBParser.TermContext."
                
        // Retrieve the current state.
        let! currentState = get
        
        // Create a new Symbol using the current state's scope.
        let symbol = {
            Name = term
            Scope = currentState.currentScope
            Category = category
            Type = dataType
            ParameterMechanism = Inapplicable
        }
        
        // Update the state with the new symbol.
        do! put (updateStateWithSymbol symbol currentState)
        
        // Return the symbol as the result of this computation.
        return symbol
    }

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
let rec mapStringIter (paramList: string list) (state: State) dataType category =
    paramList |> List.fold (fun state head ->
        let symbol = {
            Name = head
            Scope = state.currentScope
            Category = category
            Type = dataType
            ParameterMechanism = Inapplicable
        }
        set symbol state
    ) state

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
        | Some "@parent" -> parentContent = Some node.Content  // Match inherited parent content
        | Some pContent -> pContent = node.Content
        | None -> true

    let matchPosition = 
        match pattern.PatternPosition with
        | Some pPos -> pPos = node.Position
        | None -> true

    let matchChildren =
        match pattern.PatternChildren with
        | Some pChildren -> 
            List.length pChildren = List.length node.Children &&
            List.forall2 (matchPattern (Some node.Content)) pChildren node.Children
        | None -> true

    matchTokenType && matchContent && matchPosition && matchChildren

/// Replace a pattern in the AST with a new subtree
let rec replacePattern (pattern: ASTPattern) (replacementTemplate: ASTNode) (node: ASTNode) =
    if matchPattern None pattern node then
        if replacementTemplate.Content = "@parent" then
            { replacementTemplate with Content = node.Content }  // Inherit content
        else
            replacementTemplate
    else
        { node with Children = List.map (replacePattern pattern replacementTemplate) node.Children }

let rec printAST (indent: string) (node: ASTNode) =
    printfn $"%s{indent}{node.TokenType}: \"%s{node.Content}\""
    node.Children |> List.iter (printAST (indent + "  "))
    
/// Intermediate union type to represent partial results when translating parse-tree nodes.
type SubTree =
    | AstNode of ASTNode
    | Children of ASTNode list
    | Empty
    
  let subTreeToASTNode (subTree: SubTree) : ASTNode =
    match subTree with
    | AstNode ast -> ast  // Directly return ASTNode
    | Children children -> 
        { TokenType = Unknown; Content = ""; Position = (0, 0); Children = children }  // Create a new ASTNode with children
    | Empty -> 
        { TokenType = Unknown; Content = ""; Position = (0, 0); Children = [] }  // Default empty ASTNode

// Computation Expressions omitted by FSharPlus

open FSharpPlus
open FSharpPlus.Data

    /// Computation expression builder for Result<'T, 'E>
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

    /// Instantiate the builder
    let result = ResultBuilder()
