module Utility

open System
open System.IO
open System.Text
open Antlr4.Runtime
open Antlr4.Runtime.Tree
open FSharpPlus
open FSharpPlus.Data
open Serilog

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
    | ProcName
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

type SBTypes =
    | String
    | Integer
    | Real
    | Unknown
    | Void  // For procedures
    
/// Fields common to all symbols
type CommonSymbol = {
    Name: string
    SymbolKind: NodeKind
    EvaluatedType: SBTypes
    Category: CategoryType
    Position: int * int
}

/// Define a record type for arrays
/// Contains dimensions
type ArraySymbol = {
    Common: CommonSymbol
    Dimensions: int list
}

/// Use the record types inside the DU
/// There is no ParameterSymbol, but we can use CommonSymbol as the parameter passing mechanism in unknowable at compile time
type Symbol =
    | Common of CommonSymbol
    | Array of ArraySymbol

/// A symbol table is a map of scopes, i.e. a map of maps.
/// Since SB does not have blocks, all functions and procedures are global,
/// so the symbol table has only a root and single-level branches.
/// The global scope is identified by the name stored in a constant.
type SymbolTable = Map<string, Scope<Symbol>>

/// The state hidden in the state monad
type ProcessingState = {
    Ast: ASTNode //The latest AST
    SymTab: SymbolTable //The latest symbol table
    CurrentScope: string //Cope currently being processed
    InParameterList: bool //True if currently processing a parameter list so IDs become ParameterSymbols
    ImplicitInts: Set<string> // names declared as IMPLICIT% which will be needed for type analysis and applies retroactively
    ImplicitStrings: Set<string>
    Errors: string list //Collection of error message so far
    Logger: Core.Logger //Read only Serilog logger
}

/// Functions

let globalScope = "~Global"

/// Function to extract the symbol name from a symbol
let getSymbolName (symbol: Symbol) : string =
    match symbol with
    | Common sym -> sym.Name
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

