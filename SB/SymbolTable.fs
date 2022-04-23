module SymbolTable

open Antlr4.Runtime
open Antlr4.Runtime.Tree
open SB
open SBLib

type TokenType =
   | Refer = 1 | Implic = 2 | Local=3 | Dimension=4 | DefProc=5 | DefFunc=6 | EndDef=7 | If=8 | Else=9 | Then=10 | EndIf=11 | Select=12 | EndSelect=13 | On=14 | For=15 | 
      Next=16 | To=17 | EndFor=18 | Step=19 | Repeat=20 | Exit=21 | Until=22 | EndRepeat=23 | LeftParen=24 | RightParen=25 | LeftBracket=26 | RightBracket=27 | Equal=28 | 
      NotEqual=29 | Less=30 | LessEqual=31 | Greater=32 | GreaterEqual=33 | Plus=34 | Minus=35 | Multiply=36 | Divide=37 | Mod=38 | Div=39 | And=40 | Or=41 | Xor=42 | 
      Caret=43 | Not=44 | Tilde=45 | Instr=46 | Amp=47 | Question=48 | Colon=49 | Semi=50 | Comma=51 | Point=52 | Bang=53 | Whitespace=54 | Let=55 | Newline=56 | String=57 | 
      Comment=58 | ID=59 | Integer=60 | Real=61 | Unknowntype=62 | Void=63 | Scalar=64 | LineNumber=65

type CategoryType = Dim | Function | Procedure | Parameter | Variable | Implicit
type ParameterPassingMethod = Value | Reference | Unknown | Inapplicable
//type ExtraFields = Array of int list| Function of ParameterList | Procedure of ParameterList | IsParameter | Empty | Params

type Key = { Name : string; Scope : string }
type Symbol = {
    Name : string
    Scope : string 
    Category: CategoryType
    Type : TokenType
    ParameterMechanism : ParameterPassingMethod
    }

type State = {
    symTab : Map<Key, Symbol>
    implicitInts : Set<string>;
    implicitStrings : string Set;
    references : string Set;
    errorList : string list
    currentScope : string
}
   
let get name scope state  =
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

let listScope currentScope state = state.symTab |> Map.filter (fun n _ -> n.Scope = currentScope)
    
let testTable =
    let state = { implicitInts = Set.empty; implicitStrings = Set.empty; references = Set.empty; symTab = Map.empty; errorList = []; currentScope = "~Global" }
    let entry = { Name="a"; Scope = "func1"; Category=CategoryType.Dim; Type=TokenType.EndDef; ParameterMechanism = Inapplicable }
    let table1 = set entry state
    let entry = { Name="b"; Scope = "~Global"; Category=CategoryType.Procedure; Type=TokenType.Integer;  ParameterMechanism = Inapplicable; }
    let table2 = set entry table1
    let entry = { Name="c"; Scope = "func1"; Category=CategoryType.Function; Type=TokenType.Implic;  ParameterMechanism = Inapplicable; }
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

