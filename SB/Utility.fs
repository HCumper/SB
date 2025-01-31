module Utility

open Antlr4.Runtime.Tree
open SymbolTable
open System
open Antlr4.Runtime

/// Types of tokens used in the parse tree
type NodeKind =
    | Any   // Never assigned to a node, for pattern matching only
    | ArrayOrFunctionCall
    | Assignment
    | AssignmentTarget
    | BinaryExpr
    | CallExpr
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
    match name.[len] with
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

/// Map while propagating state forward between operations on each element using an Antlr list of nodes
let rec mapIter (paramList: IParseTree list) (state: State) dataType category =
    paramList |> List.fold (fun state head ->
        let term = (head :?> SBParser.TermContext).children.[0].GetText()
        let symbol = {
            Name = term
            Scope = state.currentScope
            Category = category
            Type = dataType
            ParameterMechanism = Inapplicable
        }
        set symbol state
    ) state

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

/// Tree pattern matching
type ASTPattern = {
    PatternTokenType: NodeKind option
    PatternContent: string option // `None` means ignore, `"@parent"` means match parent content
    PatternPosition: (int * int) option
    PatternChildren: ASTPattern list option
}

let rec matchPattern (parentContent: string option) (pattern: ASTPattern) (node: ASTNode) =
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

let rec replacePattern (pattern: ASTPattern) (replacementTemplate: ASTNode) (node: ASTNode) =
    if matchPattern None pattern node then
        if replacementTemplate.Content = "@parent" then
            { replacementTemplate with Content = node.Content }  // Inherit content
        else
            replacementTemplate
    else
        { node with Children = List.map (replacePattern pattern replacementTemplate) node.Children }

let rec printAST (indent: string) (node: ASTNode) =
    printfn "%s%O: \"%s\"" indent node.TokenType node.Content
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
