module SemanticAnalyzer

open Utility
open SymbolTableManager

let keywords = ["DEFine PROCedure"; "DEFine FuNction"; "END DEFine"; "RETurn"; "LOCAL"; "GLOBAL"; "EXTERNAL"; "REMark"; "REM"; "IF"; "THEN"; "ELSE"; "SELect ON"; "WHEN"; "END SELect"; "REPeat"
                "UNTIL"; "LOOP"; "EXIT"; "FOR"; "TO"; "STEP"; "NEXT"; "WHILE"; "END WHILE"; "GO TO"; "GO SUB"; "RETURN"; "ON ERROR"; "ON"; "TRON"; "TROFF"; "INPUT"; "PRINT"; "LIST"; "LLIST"; "LPRINT"
                "CLS"; "INK"; "PAPER"; "BORDER"; "AT"; "WINDOW"; "FORMAT"; "CURSOR ON"; "CURSOR OFF"; "OPEN"; "CLOSE"; "DELETE"; "RENAME"; "DIR"; "COPY"; "MOVE"; "BACKUP"; "MERGE"; "SAVE"; "LOAD"; "LRUN"
                "RUN"; "NEW"; "APPEND"; "PRINT"; "INPUT"; "DIM"; "DATA"; "READ"; "RESTORE"; "LET"; "STR$"; "VAL"; "CHR$"; "ASC"; "LEN"; "LEFT$"; "RIGHT$"; "MID$"; "INSTR"; "REPL$"; "ABS"; "SGN"; "SQR"
                "EXP"; "LOG"; "ASIN"; "ACOS"; "ATAN"; "SIN"; "COS"; "TAN"; "INT"; "RND"; "ROUND"; "MOD"; "AND"; "OR"; "NOT"; "XOR"; "ALLOCATE"; "DEALLOCATE"; "PEEK"; "POKE"; "SYSVAR"; "SAVEMEM"; "LOADMEM"; "DRAW"; "POINT"; "CIRCLE"
                "PALETTE"; "BEEP"; "STOP"; "PAUSE"; "WAIT"; "TIME"; "DATE"; "SHELL"
            ]
            
let prePopulateSymbolTable (astTree: ASTNode) : SymbolTable<Symbol> =
    let emptyTable: SymbolTable<Symbol> = SymbolTable.Empty
    let tableWithGlobalScope: SymbolTable<Symbol> = pushScope "~Global" emptyTable

    // Fold over the list of keywords (which is a string list)
    let tableWithKeyWords: SymbolTable<Symbol> =
        List.fold 
            (fun (table: SymbolTable<Symbol>) (keyWord: string) ->
                // Call addSymbol with the key (string) and then the symbol record.
                match addSymbol Overwrite keyWord
                    { Name = keyWord
                      SymbolKind = Proc
                      Value = Some keyWord
                      Category = CategoryType.Procedure
                      Position = 0, 0
                      ParameterMechanism = None
                       }
                    table 
                with
                | Some newTable -> newTable
                | None -> table)
            tableWithGlobalScope
            keywords

//    prettyPrintSymbolTable tableWithKeyWords |> ignore
    tableWithKeyWords

(* There are 7 ways to create a new symbol
    Dim
    Local
    Implicit
    assignment to it
    use as control variable in for loop
    
    all but assignment take variable length parameter lists
    
    and 2 ways to create a new scope
    Define Procedure
    Define Function
*)

let rec addToTable (mode: SymbolAddMode) (table: SymbolTable<Symbol>) (node: ASTNode) : SymbolTable<Symbol> =
    
    // Update the symbol table for the current node.
    let updatedTable : SymbolTable<Symbol> =
        match node.TokenType with
        | Procedure | Function ->
            // push a new scope using the first child's value (assumed to be the name).
            pushScope node.Children.Head.Value table
        | Dim ->
            // addSymbol mode node.Children.Head.Value
            //     { Name = node.Children.Head.Value
            //       Scope = table.Head.Name
            //       SymbolKind = Dim
            //       LineNumber = Some node.LineNumber
            //       Value = None }
                table
        | _ ->

            table

    // Process all child nodes recursively, folding the table.
    node.Children
    |> List.fold (fun (acc: SymbolTable<Symbol>) (child: ASTNode) ->
            // Here we choose Overwrite mode for child symbols.
            addToTable Overwrite acc child)
       updatedTable

    
// One off only called at the root
// let rec populateSymbolTable astTree primedSymbolTable  =
//     foldTree addToTable primedSymbolTable primedSymbolTable

