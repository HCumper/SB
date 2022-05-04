module SymbolTable

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open System
open SBLib
open SB

type CategoryType = Dim | Function | Procedure | Parameter | Variable | Implicit | Local
type ParameterPassingMethod = Value | Reference | Unknown | Inapplicable

type Key = { Name : string; Scope : string }
type Symbol = {
    Name : string
    Scope : string 
    Category: CategoryType
    Type : int
    ParameterMechanism : ParameterPassingMethod
    }

type State = {
    symTab : Map<Key, Symbol>
    references : string Set;
    errorList : string list
    currentScope : string
    outputProcFn : string
    outputGlobal : string
}
   
let get name scope state =
    // try local scope first
    let symbolFound = state.symTab |> Map.tryFind {Name=name; Scope=scope}
    match symbolFound with
    | Some _ -> symbolFound
    | None ->     // not found so try global
        let newKey = {Name=name; Scope="~Global"}
        state.symTab |> Map.tryFind newKey

// Overwrite if key exists
let set (entry:Symbol) state =
    let key = {Name=entry.Name; Scope=entry.Scope}
    let newTable:Map<Key, Symbol> = state.symTab |> Map.add key entry
    { state with symTab = newTable }
    
// Do nothing if key exists
let trySet (entry:Symbol) state =
    let key = {Name=entry.Name; Scope=entry.Scope}
    match get entry.Name entry.Scope state with
    | None ->
        set entry state
    | Some _ ->
        state

// create list of eveything in the scope
let listScope currentScope state = state.symTab |> Map.filter (fun n _ -> n.Scope = currentScope)

let testTable =
    let state = { references = Set.empty; symTab = Map.empty; errorList = []; currentScope = "~Global"; outputProcFn = ""; outputGlobal = ""}
    let entry = { Name="a"; Scope = "func1"; Category=CategoryType.Dim; Type=SBParser.EndDef; ParameterMechanism = Inapplicable }
    let table1 = set entry state
    let entry = { Name="b"; Scope = "~Global"; Category=CategoryType.Procedure; Type=SBParser.Integer;  ParameterMechanism = Inapplicable; }
    let table2 = set entry table1
    let entry = { Name="c"; Scope = "func1"; Category=CategoryType.Function; Type=SBParser.Implic;  ParameterMechanism = Inapplicable; }
    let table3 = set entry table2
    
    let x = get  "c" "func2" table3
    let key2 = { Name="b"; Scope = "~Global";}
    let y = get  "b" "~Global" table3
    let key3 = { Name="b"; Scope = "func1";}
    let z = get  "b" "func1" table3
    let key4 = { Name="notthere"; Scope = "func1";}
    let a = get  "notthere" "func1" table3
    let b = listScope "func1" table3
    ""

