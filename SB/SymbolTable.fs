module SymbolTable

open System.Collections.Generic
open System

type Key = {
    Name : string
    Scope : string // scope to lookup in
    }

type Content = {
    Type : int
    TypeString : string
    Scope : string  // actual scope
}

type Symbol = {
    Name : string
    Scope : string 
    Type : int
    TypeString : string
    }

type TokenType =
   | Refer = 1 | Implic = 2 | Local=3 | Dimension=4 | DefProc=5 | DefFunc=6 | EndDef=7 | If=8 | Else=9 | Then=10 | EndIf=11 | Select=12 | EndSelect=13 | On=14 | For=15 | 
      Next=16 | To=17 | EndFor=18 | Step=19 | Repeat=20 | Exit=21 | Until=22 | EndRepeat=23 | LeftParen=24 | RightParen=25 | LeftBracket=26 | RightBracket=27 | Equal=28 | 
      NotEqual=29 | Less=30 | LessEqual=31 | Greater=32 | GreaterEqual=33 | Plus=34 | Minus=35 | Multiply=36 | Divide=37 | Mod=38 | Div=39 | And=40 | Or=41 | Xor=42 | 
      Caret=43 | Not=44 | Tilde=45 | Instr=46 | Amp=47 | Question=48 | Colon=49 | Semi=50 | Comma=51 | Point=52 | Bang=53 | Whitespace=54 | Let=55 | Newline=56 | String=57 | 
      Comment=58 | ID=59 | Integer=60 | Real=61 | Unknowntype=62 | Void=63 | Scalar=64 | LineNumber=65
   
// Overwrite if key exists
let Insert entry table =
    let key = {Name=entry.Name; Scope=entry.Scope}
    let content = {Type=entry.Type; TypeString="type"; Scope=key.Scope}
    let newTable:Map<Key, Content> = table |> Map.add key content
    newTable

let lookUp key (table:Map<Key, Content>)  =
    // try local scope first
    let symbolFound = table |> Map.tryFind key
    match symbolFound with
    | Some x -> symbolFound
    | None ->     // not found so try global
        let newKey = { key with Scope="~Global" }
        table |> Map.tryFind newKey

let testTable =
    let entry = { Name="a"; Scope = "func1"; Type=7; TypeString="ËndDef" }
    let table1:Map<'Key, 'Content> = Insert entry Map.empty
    let entry = { Name="b"; Scope = "~Global"; Type=3; TypeString="If" }
    let table2:Map<'Key, 'Content> = Insert entry table1
    let entry = { Name="c"; Scope = "func2"; Type=7; TypeString="something" }
    let table3:Map<'Key, 'Content> = Insert entry table2
    
    let key1 = { Name="c"; Scope = "func2";}
    let x = lookUp  key1 table3
    let key2 = { Name="b"; Scope = "~Global";}
    let y = lookUp  key2 table3
    let key3 = { Name="b"; Scope = "func1";}
    let z = lookUp  key3 table3
    let key4 = { Name="notthere"; Scope = "func1";}
    let a = lookUp  key4 table3

    ""

