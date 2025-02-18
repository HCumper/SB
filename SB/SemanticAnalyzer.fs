module SemanticAnalyzer

open Utility
open SymbolTableManager

let keywords = ["DEFine PROCedure"; "DEFine FuNction"; "END DEFine"; "RETurn"; "LOCAL"; "GLOBAL"; "EXTERNAL"; "REMark"; "REM"; "IF"; "THEN"; "ELSE"; "SELect ON"; "WHEN"; "END SELect"; "REPeat"
                "UNTIL"; "LOOP"; "EXIT"; "FOR"; "TO"; "STEP"; "NEXT"; "WHILE"; "END WHILE"; "GO TO"; "GO SUB"; "RETURN"; "ON ERROR"; "ON"; "TRON"; "TROFF"; "INPUT"; "PRINT"; "LIST"; "LLIST"; "LPRINT"
                "CLS"; "INK"; "PAPER"; "BORDER"; "AT"; "WINDOW"; "FORMAT"; "CURSOR ON"; "CURSOR OFF"; "OPEN"; "CLOSE"; "DELETE"; "RENAME"; "DIR"; "COPY"; "MOVE"; "BACKUP"; "MERGE"; "SAVE"; "LOAD"; "LRUN"
                "RUN"; "NEW"; "APPEND"; "PRINT"; "INPUT"; "DIM"; "DATA"; "READ"; "RESTORE"; "LET"; "STR$"; "VAL"; "CHR$"; "ASC"; "LEN"; "LEFT$"; "RIGHT$"; "MID$"; "INSTR"; "REPL$"; "ABS"; "SGN"; "SQR"
                "EXP"; "LOG"; "SIN"; "COS"; "TAN"; "INT"; "RND"; "ROUND"; "MOD"; "AND"; "OR"; "NOT"; "XOR"; "ALLOCATE"; "DEALLOCATE"; "PEEK"; "POKE"; "SYSVAR"; "SAVEMEM"; "LOADMEM"; "DRAW"; "POINT"; "CIRCLE"
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
                match addSymbol Overwrite 
                        keyWord 
                        { Name = keyWord
                          Scope = "~Global"
                          SymbolKind = Keyword keyWord
                          LineNumber = None
                          Value = Some keyWord } 
                        table with
                | Some newTable -> newTable
                | None -> table)
            tableWithGlobalScope
            keywords

    prettyPrintSymbolTable tableWithKeyWords |> ignore
    tableWithKeyWords

        
let rec populateSymbolTable astTree primedSymbolTable =
    let newSymbolTable = prePopulateSymbolTable Skip symbolTable
    List.fold (populateSymbolTable ) newSymbolTable tree.Children

    // let addToTable (mode: SymbolAddMode) (table: SymbolTable<Symbol>) (node: ASTNode) : SymbolTable<Symbol> =
    //     match node with
    //     | DimNode (name, dataType) ->
    //         let symbol =
    //             { Name = name
    //               Scope = table.Head.Name
    //               Category = CategoryType.Dim
    //               Type = dataType
    //               ParameterMechanism = Inapplicable }
    //         addSymbol mode name symbol table
    //     | AssignmentNode (name, dataType, _, _) ->
    //         let symbol =
    //             { Name = name
    //               Scope = table.Head.Name
    //               Category = CategoryType.Assignment
    //               Type = dataType
    //               ParameterMechanism = Inapplicable }
    //         addSymbol mode name symbol table
    //     | _ -> table
    // walkAst addToTable empty ast