module Utility
open Types
open System
open System.Collections.Generic
open Antlr4.Runtime
open Antlr4.Runtime.Tree

/// Functions

let globalScope = "~Global"

/// Function to extract the symbol name from a symbol
let getSymbolName (symbol: Symbol) : string =
    match symbol with
    | VariableSym s -> s.Common.Name
    | ConstantSym s -> s.Common.Name
    | ParameterSym s -> s.Common.Name
    | ArraySym s -> s.Common.Name
    | FunctionSym s -> s.Common.Name
    | ProcedureSym s -> s.Common.Name
    | BuiltInSym s -> s.Common.Name

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
let mapAntlrList (mappingFunction: IParseTree -> 'b) (inputList: IList<IParseTree>) : 'b list =
    [ for item in inputList do yield mappingFunction item ]

/// <summary>
/// Copies an Antlr list to an F# list non-destructively.
/// </summary>
let copyAntlrList (parentNode: IList<IParseTree>) : IParseTree list =
    mapAntlrList id parentNode

/// <summary>
/// Helper function to create an ASTNode.
/// </summary>
let private sourcePositionFromTuple (line, column) : SourcePosition =
    { BasicLineNo = None; EditorLineNo = line; Column = column }

let private sourcePositionToTuple (position: SourcePosition) : int * int =
    position.EditorLineNo, position.Column

let createAstNode (tokenType: NodeKind) (value: string) (position: int * int) (children: ASTNode list) : ASTNode =
    { Kind = tokenType; Value = value; Position = sourcePositionFromTuple position; Children = children }

/// <summary>
/// Recursively pretty-prints an AST node and its descendants with indentation.
/// </summary>
let rec prettyPrintAst (node: ASTNode) (indent: int) : string =
    let padding = String.replicate indent " "
    let currentLine = sprintf "%s- %A (\"%s\", pos=%A)\n" padding node.Kind node.Value node.Position
    let childLines = node.Children |> List.map (fun child -> prettyPrintAst child (indent + 2)) |> String.concat ""
    currentLine + childLines

let rec printAST (indent: string) (node: ASTNode) =
    printfn $"%s{indent}{node.Kind}: \"%s{node.Value}\""
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
        | Some pType -> pType = node.Kind
        | None -> true

    let matchContent = 
        match pattern.PatternContent with
        | Some "@parent" -> parentContent = Some node.Value  // Match inherited parent content
        | Some pContent -> pContent = node.Value
        | None -> true

    let matchPosition = 
        match pattern.PatternPosition with
        | Some pPos -> pPos = sourcePositionToTuple node.Position
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

