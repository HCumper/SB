module SemanticAnalyzer

open Utility
open SymbolTableManager

/// List of keywords used to pre-populate the symbol table.
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
    
/// Create each kind of symbol.
/// Creates a simple (base) symbol.

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
        
/// Pre-populate the symbol table with the list of keywords.
/// The global scope is used (via the constant 'globalScope').
let prePopulateSymbolTable (astTree: ASTNode) : SymbolTable =
    let emptyTable : SymbolTable = Map.empty
    let tableWithGlobalScope : SymbolTable = pushScopeToTable globalScope emptyTable
    List.fold
        (fun (table: SymbolTable) (keyWord: string) ->
            // Create a symbol for the keyword and add it to the global scope.
            let newSymbol = createCommonSymbol keyWord Proc CategoryType.Procedure (0, 0)
            addSymbolToNamedScope Overwrite newSymbol globalScope table
        )
        tableWithGlobalScope
        keywords

/// Recursively update the symbol table based on an AST.
/// The function takes the current table, an AST node, and the current scope name,
/// then updates the table according to the node's TokenType.
let rec addToTable 
    (mode: SymbolAddMode) 
    (table: SymbolTable)   // all scopes
    (node: ASTNode)        // current AST node
    (incomingScopeName: string)    // current scope name
    : SymbolTable =
    
    // Update the symbol table for the current node.
    let (updatedTable, outgoingScopeName) : (SymbolTable * string) =
        match node.TokenType with
        | Procedure
        | Function ->
            // If there is at least one child, push a new scope using the first child's value.
            if node.Children <> [] then
                (pushScopeToTable node.Children.Head.Value table, node.Children.Head.Value)
            else
                (table, incomingScopeName)
        | Dim ->
            if List.isEmpty node.Children then 
                (table, incomingScopeName)
            else
                // Convert array sizes from AST nodes (children after the first) to integer values.
                let arraySizes : int list = 
                    node.Children.Tail
                    |> List.choose (fun n ->
                        try Some (int n.Value)
                        with _ -> None)
                // Create an array symbol.
                let newSymbol : Symbol =
                    createArraySymbol node.Children.Head.Value Dim CategoryType.Variable (0, 0) arraySizes
                // Update the scope (identified by incomingScopeName) with the new symbol.
                let newTable : SymbolTable =
                    addSymbolToNamedScope Overwrite newSymbol incomingScopeName table
                (newTable, incomingScopeName)
        | _ ->
            (table, incomingScopeName)
    
    // Recursively process all child nodes, folding the updated table.
    node.Children
    |> List.fold (fun (acc: SymbolTable) (child: ASTNode) ->
            addToTable Overwrite acc child outgoingScopeName)
       updatedTable
