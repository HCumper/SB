module TypeResolver

open Antlr4.Runtime.Tree
open System
open SBLib
open SB
open SymbolTable

let rec private mapIter implicitList name (dataType: TokenType) (state: State) =
    match implicitList with
    | [] -> state
    | head::tail ->
        let oldSymbol = SymbolTable.get name (snd head).Scope state
        match oldSymbol with
        | None -> state
        | Some n ->
            let newSymbol = { n with Type = dataType }
            SymbolTable.set newSymbol state
            |> mapIter tail name dataType

let rec private listIter  listImplicits (state: State) =
    match listImplicits with
    | [] -> state
    | head::tail ->
        let name = (snd head).Name
        let symList = Map.toList state.symTab
        let filteredList = List.filter (fun x -> (snd x).Name = name && (snd x).Category <> CategoryType.Implicit) symList
        mapIter filteredList name (snd head).Type state 
        |> listIter tail

// change variables in symbol table to type given by implicit declaration
let TypeImplicits state =
    let implicits = Map.filter (fun _ s -> s.Category = CategoryType.Implicit) state.symTab
    let listImplicits = Map.toList implicits
    listIter  listImplicits state
