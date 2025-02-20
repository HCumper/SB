module SemanticAnalyzer

open Utility
open SymbolTableManager

let keywords =
    [ "DEFine PROCedure"
      "DEFine FuNction"
      "END DEFine"
      "RETurn"
      "LOCAL"
      "GLOBAL"
      "EXTERNAL"
      "REMark"
      "REM"
      "IF"
      "THEN"
      "ELSE"
      "SELect ON"
      "WHEN"
      "END SELect"
      "REPeat"
      "UNTIL"
      "LOOP"
      "EXIT"
      "FOR"
      "TO"
      "STEP"
      "NEXT"
      "WHILE"
      "END WHILE"
      "GO TO"
      "GO SUB"
      "RETURN"
      "ON ERROR"
      "ON"
      "TRON"
      "TROFF"
      "INPUT"
      "PRINT"
      "LIST"
      "LLIST"
      "LPRINT"
      "CLS"
      "INK"
      "PAPER"
      "BORDER"
      "AT"
      "WINDOW"
      "FORMAT"
      "CURSOR ON"
      "CURSOR OFF"
      "OPEN"
      "CLOSE"
      "DELETE"
      "RENAME"
      "DIR"
      "COPY"
      "MOVE"
      "BACKUP"
      "MERGE"
      "SAVE"
      "LOAD"
      "LRUN"
      "RUN"
      "NEW"
      "APPEND"
      "PRINT"
      "INPUT"
      "DIM"
      "DATA"
      "READ"
      "RESTORE"
      "LET"
      "STR$"
      "VAL"
      "CHR$"
      "ASC"
      "LEN"
      "LEFT$"
      "RIGHT$"
      "MID$"
      "INSTR"
      "REPL$"
      "ABS"
      "SGN"
      "SQR"
      "EXP"
      "LOG"
      "ASIN"
      "ACOS"
      "ATAN"
      "SIN"
      "COS"
      "TAN"
      "INT"
      "RND"
      "ROUND"
      "MOD"
      "AND"
      "OR"
      "NOT"
      "XOR"
      "ALLOCATE"
      "DEALLOCATE"
      "PEEK"
      "POKE"
      "SYSVAR"
      "SAVEMEM"
      "LOADMEM"
      "DRAW"
      "POINT"
      "CIRCLE"
      "PALETTE"
      "BEEP"
      "STOP"
      "PAUSE"
      "WAIT"
      "TIME"
      "DATE"
      "SHELL" ]

/// Helper to create the shared CommonSymbol record.
let private baseSymbol
    (name: string)
    (symbolKind: NodeKind)
    (category: CategoryType)
    (position: int * int)
    : CommonSymbol =
    { Name = name
      SymbolKind = symbolKind
      Category = category
      Position = position }

/// Create each kind of symbol.
/// Creates a simple (base) symbol.
let createCommonSymbol (name: string) (symbolKind: NodeKind) (category: CategoryType) (position: int * int) : Symbol =
    Common(baseSymbol name symbolKind category position)

/// Creates a parameter symbol with a specified parameter passing mechanism.
let createParameterSymbol
    (name: string)
    (symbolKind: NodeKind)
    (category: CategoryType)
    (position: int * int)
    (parameterMechanism: ParameterMechanismType)
    : Symbol =
    Parameter
        { Common = baseSymbol name symbolKind category position
          ParameterMechanism = parameterMechanism }

/// Creates an array symbol with a list of dimensions.
let createArraySymbol
    (name: string)
    (symbolKind: NodeKind)
    (category: CategoryType)
    (position: int * int)
    (dimensions: int list)
    : Symbol =
    Array
        { Common = baseSymbol name symbolKind category position
          Dimensions = dimensions }

let prePopulateSymbolTable (astTree: ASTNode) : SymbolTable<Symbol> =
    let emptyTable: SymbolTable<Symbol> = SymbolTable.Empty
    let tableWithGlobalScope: SymbolTable<Symbol> = pushScope "~Global" emptyTable

    // Fold over the list of keywords (which is a string list)
    List.fold
        (fun (table: SymbolTable<Symbol>) (keyWord: string) ->
            // Call createSymbol with the key (string) and then the symbol record.
            let newSymbol = createCommonSymbol keyWord Proc CategoryType.Procedure (0, 0)
            addSymbolToGlobalScope Overwrite keyWord newSymbol table
        )
        tableWithGlobalScope
        keywords


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

let rec addToTable 
    (mode: SymbolAddMode) 
    (table: SymbolTable<Symbol>) // all scopes
    (node: ASTNode) // to track position in AST
    : SymbolTable<Symbol> =
    
    // Update the symbol table for the current node.
    let updatedTable: SymbolTable<Symbol> =
        match node.TokenType with
        | Procedure
        | Function ->
            // Push a new scope using the first child's value which is the name
            pushScope node.Children.Head.Value table

        | Dim ->
            // Ensure node.Children has elements before calling List.tail.
            if List.isEmpty node.Children then table
            else
                // Convert array sizes from AST nodes to integer values.
                let arraySizes: int list = 
                    node.Children.Tail
                    |> List.choose (fun n ->
                        try Some (int n.Value)
                        with _ -> None)
                
                // Create an array symbol (assuming createArraySymbol returns an updated table).
                // The function 'createArraySymbol' is assumed to take:
                // (name: string) (tokenType: NodeKind) (category: CategoryType) (position: int * int)
                // (dimensions: int list) (table: SymbolTable<Symbol>) : SymbolTable<Symbol>
                let newSymbol = createArraySymbol node.Children.Head.Value Dim CategoryType.Variable (0, 0) arraySizes
                addSymbolToCurrentScope Overwrite node.Value newSymbol table 

        | _ -> table  // Return unchanged table for other cases.
        
    // Process all child nodes recursively, folding the table.
    node.Children
    |> List.fold (fun (acc: SymbolTable<Symbol>) (child: ASTNode) ->
            // Here we choose Overwrite mode for child symbols.
            addToTable Overwrite acc child)
       updatedTable
       

