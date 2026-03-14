module AstHelpers

open SyntaxAst
open Antlr4.Runtime
open Antlr4.Runtime.Tree

let posOfToken (t: IToken) : Position =
    (t.Line, t.Column)

let rec firstTokenOf (tree: IParseTree) =
    match tree with
    | :? ITerminalNode as t -> Some t.Symbol
    | _ when tree.ChildCount > 0 -> firstTokenOf (tree.GetChild 0)
    | _ -> None

let posOfTree (tree: IParseTree) : Position =
    match firstTokenOf tree with
    | Some t -> posOfToken t
    | None -> (0, 0)