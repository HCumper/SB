module SemanticAnalyzer

open Utility
open SymbolTableManager
open Monads.State
open FSharpPlus.Data

/// Alphabetically sorted list of keywords for pre-populating the symbol table.
let keywords = 
  [ "ABS"; "ACOS"; "ALLOCATE"; "AND"; "APPEND"; "ASC"; "ASIN"; "AT"; "ATAN"; 
    "BEEP"; "BACKUP"; "BORDER"; "CIRCLE"; "CLS"; "CLOSE"; "COPY"; "COS"; 
    "CURSOR OFF"; "CURSOR ON"; "DATA"; "DATE"; "DEALLOCATE"; "DEFine FuNction"; 
    "DEFine PROCedure"; "DELETE"; "DIM"; "DIR"; "DRAW"; "END DEFine"; 
    "END SELect"; "END WHILE"; "EXP"; "EXTERNAL"; "FORMAT"; "FOR"; "GLOBAL"; 
    "GO SUB"; "GO TO"; "IF"; "INK"; "INPUT"; "INSTR"; "INT"; "LEFT$"; "LET"; 
    "LEN"; "LIST"; "LOAD"; "LOADMEM"; "LOCAL"; "LOG"; "LOOP"; "LPRINT"; "LRUN"; 
    "MOD"; "MID$"; "MOVE"; "NEW"; "NEXT"; "NOT"; "ON"; "ON ERROR"; "OR"; 
    "PALETTE"; "PAPER"; "PAUSE"; "PEEK"; "PI"; "POINT"; "POKE"; "PRINT"; 
    "REMark"; "REM"; "REPEAT"; "RESTORE"; "RETURN"; "RETurn"; "REPL$"; "RIGHT$"; 
    "RND"; "ROUND"; "RUN"; "SAVE"; "SAVEMEM"; "SELect ON"; "SHELL"; "SGN"; 
    "SIN"; "STEP"; "STOP"; "STR$"; "SYSVAR"; "TAN"; "THEN"; "TIME"; "TO"; 
    "TRON"; "TROFF"; "UNTIL"; "VAL"; "WAIT"; "WHEN"; "WHILE"; "WINDOW"; "XOR" ]

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
                let newSymbol = createCommonSymbol keyWord ProcedureDefinition CategoryType.Keyword (0, 0)
                addSymbolToNamedScope Overwrite newSymbol globalScope table)
            tableWithGlobalScope
            keywords
    { oldState with SymTab = newSymTab }

/// Helper: update the state's implicit sets (integers or strings) based on a list of newly created symbols.
let private updateImplicitSets
    (implicitCat: CategoryType)
    (symbols: Symbol list)
    (currentState: ProcessingState)
    : ProcessingState =

    // Convert each symbol to its name, build a set
    let names =
        symbols
        |> List.map getSymbolName
        |> Set.ofList

    match implicitCat with
    | CategoryType.Integer ->
        let newInts = Set.union names currentState.ImplicitInts
        { currentState with ImplicitInts = newInts }

    | CategoryType.String ->
        let newStrs = Set.union names currentState.ImplicitStrings
        { currentState with ImplicitStrings = newStrs }

    | _ ->
        currentState
        
/// Recursively update the symbol table based on an AST node,
/// returning a 'State<ProcessingState, unit>' computation.
let rec addToTable
    (mode: SymbolAddMode)
    (node: ASTNode)
    (incomingState: ProcessingState)
    : State<ProcessingState, unit> =

    state {
        // 1) Grab the current state from the State monad
        let! currentState = getState
        let currentTable  = currentState.SymTab
        let scopeName     = incomingState.CurrentScope

        // 2) Decide how to handle this AST node
        let (updatedTable, outgoingScope, possiblyUpdatedState) =
            match node.TokenType with
            | ProcedureCall ->
                // e.g. 'quicksort 1,n'
                let sym =
                    createCommonSymbol
                        node.Value
                        ProcedureCall
                        CategoryType.Procedure
                        node.Position
                let tbl =
                    addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, currentState)

            | FunctionCall ->
                // e.g. 'someFunc(...)'
                let sym =
                    createCommonSymbol
                        node.Value
                        FunctionCall
                        CategoryType.Function
                        node.Position
                let tbl =
                    addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, currentState)

            | Implicit ->
                // Deduce int vs. string from node.Value
                let implicitCat =
                    if node.Value.Contains("%") then CategoryType.Integer
                    elif node.Value.Contains("$") then CategoryType.String
                    else CategoryType.Variable

                // Create a symbol for each child
                let implicitSyms =
                    node.Children
                    |> List.map (fun c ->
                        createCommonSymbol c.Value Implicit implicitCat c.Position
                    )

                // Add them all to the table
                let tbl =
                    implicitSyms
                    |> List.fold (fun acc sym -> addSymbolToNamedScope Overwrite sym scopeName acc) currentTable

                // Possibly update state sets
                let newState = updateImplicitSets implicitCat implicitSyms currentState

                (tbl, scopeName, newState)

            | ProcedureDefinition
            | FunctionDefinition ->
                // If there's a name, push a scope
                if node.Children.Length > 0 then
                    let nameOfScope = node.Value
                    let tbl = pushScopeToTable nameOfScope currentTable
                    (tbl, nameOfScope, currentState)
                else
                    (currentTable, scopeName, currentState)

            | Dim ->
                // e.g. 'DIM a(100,200)'
                match node.Children with
                | [] ->
                    (currentTable, scopeName, currentState)
                | head :: tail ->
                    let arraySizes =
                        tail
                        |> List.choose (fun x ->
                            try Some (int x.Value) with _ -> None
                        )
                    let arrSym =
                        createArraySymbol head.Value Dim CategoryType.Variable node.Position arraySizes
                    // Usually array is in global scope
                    let tbl = addSymbolToNamedScope Overwrite arrSym globalScope currentTable
                    (tbl, scopeName, currentState)

            | Identifier ->
                // e.g. 'someVar'
                let sym =
                    createCommonSymbol
                        node.Value
                        ID
                        CategoryType.Variable
                        node.Position
                let tbl = addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, currentState)

            | Parameters ->
                // Mark that we are in a parameter list
                let newSt = { currentState with InParameterList = true }
                let sym =
                    createCommonSymbol
                        node.Value
                        Parameters
                        CategoryType.Parameter
                        node.Position
                let tbl = addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, newSt)

            | Local ->
                // e.g. 'LOCal something'
                let newSt = { currentState with InParameterList = true }
                let localName = node.Children.[0].Value
                let sym =
                    createCommonSymbol
                        localName
                        Local
                        CategoryType.Local
                        node.Position
                let tbl = addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, newSt)

            | _ ->
                // No special logic
                (currentTable, scopeName, currentState)

        // 3) Merge changes back into the state
        let updatedState = { possiblyUpdatedState with SymTab = updatedTable }
        do! putState updatedState

        // 4) Recurse into the node's children
        let rec processKids (kids: ASTNode list) : State<ProcessingState, unit> =
            match kids with
            | [] -> state { return () }
            | head :: tail ->
                state {
                    do! addToTable Overwrite head updatedState
                    do! processKids tail
                }

        do! processKids node.Children

        // 5) If we just finished 'Parameters', revert InParameterList
        if node.TokenType = Parameters then
            do! putState { updatedState with InParameterList = false }

        return ()
    }