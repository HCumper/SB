module SemanticAnalyzer

open Utility
open SymbolTableManager
open Monads.State
open FSharpPlus.Data

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

/// Helper to create the shared CommonSymbol record.
let private baseSymbol
    (name: string)
    (symbolKind: NodeKind)
    (category: CategoryType)
    (position: int * int)
    (evaluatedType: SBTypes)
    : CommonSymbol =
    { Name = name
      SymbolKind = symbolKind
      EvaluatedType = evaluatedType
      Category = category
      Position = position }

/// Create a simple (base) symbol.
let createCommonSymbol (name: string) (symbolKind: NodeKind) (category: CategoryType) (position: int * int) : Symbol =
    Common (baseSymbol name symbolKind category position Unknown)

/// Creates a parameter symbol with a specified parameter passing mechanism.
let createParameterSymbol
    (name: string)
    (symbolKind: NodeKind)
    (category: CategoryType)
    (position: int * int)
    (parameterMechanism: ParameterMechanismType)
    : Symbol =
    Parameter { Common = baseSymbol name symbolKind category position Unknown
                ParameterMechanism = parameterMechanism }

/// Creates an array symbol with a list of dimensions.
let createArraySymbol
    (name: string)
    (symbolKind: NodeKind)
    (category: CategoryType)
    (position: int * int)
    (dimensions: int list)
    : Symbol =
    Array { Common = baseSymbol name symbolKind category position Unknown
            Dimensions = dimensions }

/// Pre-populate the symbol table with keywords, updating the ProcessingState.
let prePopulateSymbolTable (oldState: ProcessingState) : ProcessingState =
    let tableWithGlobalScope: SymbolTable = pushScopeToTable globalScope oldState.SymTab
    let newSymTab =
        List.fold
            (fun (table: SymbolTable) (keyWord: string) ->
                let newSymbol = createCommonSymbol keyWord Proc CategoryType.Procedure (0, 0)
                addSymbolToNamedScope Overwrite newSymbol globalScope table)
            tableWithGlobalScope
            keywords
    { oldState with SymTab = newSymTab }

/// Recursively update the symbol table based on an AST.
/// The function now takes an AST node as parameter and returns a stateful computation
/// that produces unit.
let rec addToTable 
    (mode: SymbolAddMode) 
    (node: ASTNode) 
    (incomingScopeName: string)
    : State<ProcessingState, unit> =
    state {
        let! initialState = getState
        let currentTable = initialState.SymTab
        let currentNode = node
        let incomingScopeName = incomingScopeName

        let (updatedTable, outgoingScopeName) =
            match currentNode.TokenType with
            | Procedure
            | Function ->
                if currentNode.Children <> [] then
                    let newScope = currentNode.Children.Head.Value
                    (pushScopeToTable newScope currentTable, newScope)
                else
                    (currentTable, incomingScopeName)
            | Dim ->
                if List.isEmpty currentNode.Children then
                    (currentTable, incomingScopeName)
                else
                    let arraySizes =
                        currentNode.Children.Tail
                        |> List.choose (fun n -> try Some (int n.Value) with _ -> None)
                    let newSymbol =
                        createArraySymbol currentNode.Children.Head.Value Dim CategoryType.Variable currentNode.Position arraySizes
                    let newTable = addSymbolToNamedScope Overwrite newSymbol globalScope currentTable
                    (newTable, incomingScopeName)
            | ID ->
                let newSymbol = createCommonSymbol currentNode.Value ID CategoryType.Variable currentNode.Position
                let newTable = addSymbolToNamedScope Overwrite newSymbol incomingScopeName currentTable
                (newTable, incomingScopeName)
            | Parameters ->
                let newState = { initialState with InParameterList = true }
                let newSymbol = createParameterSymbol currentNode.Value Parameters CategoryType.Variable currentNode.Position ParameterMechanismType.Value
                let newTable = addSymbolToNamedScope Overwrite newSymbol incomingScopeName currentTable
                (newTable, incomingScopeName)
            | _ -> (currentTable, incomingScopeName)

        let updatedState = { initialState with SymTab = updatedTable }
        do! putState updatedState

        // Process children: we define a helper that folds over the children.
        let processChildren (children: ASTNode list) (scopeName: string) : State<ProcessingState, unit> =
            // Fold over children; for each child, process it using addToTable.
            List.foldBack (fun child acc -> 
                state {
                    do! addToTable Overwrite child scopeName
                    do! acc
                }) children (state { return () })
        do! processChildren currentNode.Children outgoingScopeName

        // If finishing a Parameters block, reset the InParameterList flag.
        if currentNode.TokenType = Parameters then
            do! putState { updatedState with InParameterList = false }

        return ()
    }
