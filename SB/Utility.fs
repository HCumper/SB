module Utility

open Antlr4.Runtime.Tree
open SymbolTable
open System
open Antlr4.Runtime

/// Types of tokens used in the parse tree
type NodeKind =
    | ArrayOrFunctionCall
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
