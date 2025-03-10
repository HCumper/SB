module SemanticAnalyzer

open Utility
open SymbolTableManager
open Monads.State
open FSharpPlus.Data

/// List of keywords used to pre-populate the symbol table.
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

/// Creates the base CommonSymbol record.
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

/// Create a simple (common) symbol.
let createCommonSymbol (name: string) (symbolKind: NodeKind) (category: CategoryType) (position: int * int) : Symbol =
    Common (baseSymbol name symbolKind category position Unknown)

/// Creates an array symbol with the specified dimensions.
let createArraySymbol
    (name: string)
    (symbolKind: NodeKind)
    (category: CategoryType)
    (position: int * int)
    (dimensions: int list)
    : Symbol =
    Array { Common = baseSymbol name symbolKind category position Unknown
            Dimensions = dimensions }

/// Pre-populate the symbol table with keywords.
/// This pushes a new (global) scope and adds each keyword as a symbol.
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

/// Update the state's implicit integer or string sets based on new symbols.
let private updateImplicitSets
    (implicitCat: CategoryType)
    (symbols: Symbol list)
    (currentState: ProcessingState)
    : ProcessingState =
    let names = symbols |> List.map getSymbolName |> Set.ofList
    match implicitCat with
    | CategoryType.Integer ->
        { currentState with ImplicitInts = Set.union names currentState.ImplicitInts }
    | CategoryType.String ->
        { currentState with ImplicitStrings = Set.union names currentState.ImplicitStrings }
    | _ ->
        currentState

/// Recursively update the symbol table based on an AST node.
/// Uses the State monad to thread ProcessingState.
let rec addToTable
    (mode: SymbolAddMode)
    (node: ASTNode)
    (incomingState: ProcessingState)
    : State<ProcessingState, unit> =
    state {
        // 1. Get the current state.
        let! currentState = getState
        let currentTable  = currentState.SymTab
        let scopeName     = incomingState.CurrentScope

        // 2. Process the node based on its token type.
        let (updatedTable, outgoingScope, possiblyUpdatedState) =
            match node.TokenType with
            | ProcedureCall ->
                let sym =
                    createCommonSymbol node.Value ProcedureCall CategoryType.Procedure node.Position
                let tbl = addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, currentState)

            | FunctionCall ->
                let sym =
                    createCommonSymbol node.Value FunctionCall CategoryType.Function node.Position
                let tbl = addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, currentState)

            | Implicit ->
                let implicitCat =
                    if node.Value.Contains("%") then CategoryType.Integer
                    elif node.Value.Contains("$") then CategoryType.String
                    else CategoryType.Variable
                let implicitSyms =
                    node.Children |> List.map (fun c ->
                        createCommonSymbol c.Value Implicit implicitCat c.Position)
                let tbl =
                    implicitSyms |> List.fold (fun acc sym -> addSymbolToNamedScope Overwrite sym scopeName acc) currentTable
                let newState = updateImplicitSets implicitCat implicitSyms currentState
                (tbl, scopeName, newState)

            | ProcedureDefinition
            | FunctionDefinition ->
                if node.Children.Length > 0 then
                    let nameOfScope = node.Value
                    let tbl = pushScopeToTable nameOfScope currentTable
                    (tbl, nameOfScope, currentState)
                else
                    (currentTable, scopeName, currentState)

            | Dim ->
                match node.Children with
                | [] -> (currentTable, scopeName, currentState)
                | head :: tail ->
                    let arraySizes =
                        tail |> List.choose (fun x -> try Some (int x.Value) with _ -> None)
                    let arrSym =
                        createArraySymbol head.Value Dim CategoryType.Variable node.Position arraySizes
                    let tbl = addSymbolToNamedScope Overwrite arrSym globalScope currentTable
                    (tbl, scopeName, currentState)

            | Identifier ->
                let sym =
                    createCommonSymbol node.Value ID CategoryType.Variable node.Position
                let tbl = addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, currentState)

            | Parameters ->
                // Mark the state as inside a parameter list.
                let newSt = { currentState with InParameterList = true }
                let sym =
                    createCommonSymbol node.Value Parameters CategoryType.Parameter node.Position
                let tbl = addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, newSt)

            | Local ->
                let newSt = { currentState with InParameterList = true }
                let localName = node.Children.[0].Value
                let sym =
                    createCommonSymbol localName Local CategoryType.Local node.Position
                let tbl = addSymbolToNamedScope Overwrite sym scopeName currentTable
                (tbl, scopeName, newSt)

            | _ ->
                (currentTable, scopeName, currentState)

        // 3. Update the state with the new symbol table.
        let updatedState = { possiblyUpdatedState with SymTab = updatedTable }
        do! putState updatedState

        // 4. Recurse over children.
        let rec processKids kids =
            state {
                match kids with
                | [] -> return ()
                | head :: tail ->
                    do! addToTable Overwrite head updatedState
                    do! processKids tail
            }
        do! processKids node.Children

        // 5. If finishing a Parameters node, revert the parameter flag.
        if node.TokenType = Parameters then
            do! putState { updatedState with InParameterList = false }

        return ()
    }
