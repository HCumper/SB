module SemanticAnalyzer

open Types
open Utility
open SymbolTableManager
open Monads.State
open FSharpPlus.Data

/// List of keywords used to pre-populate the symbol table.
let keywords =
    [ "ABS"; "ACOS"; "ALLOCATE"; "AND"; "APPEND"; "ASC"; "ASIN"; "AT"; "ATAN"
      "BEEP"; "BACKUP"; "BORDER"; "CIRCLE"; "CLS"; "CLOSE"; "COPY"; "COS"
      "CURSOR OFF"; "CURSOR ON"; "DATA"; "DATE"; "DEALLOCATE"; "DEFine FuNction"
      "DEFine PROCedure"; "DELETE"; "DIM"; "DIR"; "DRAW"; "END DEFine"
      "END SELect"; "END WHILE"; "EXP"; "EXTERNAL"; "FORMAT"; "FOR"; "GLOBAL"
      "GO SUB"; "GO TO"; "IF"; "INK"; "INPUT"; "INSTR"; "INT"; "LEFT$"; "LET"
      "LEN"; "LIST"; "LOAD"; "LOADMEM"; "LOCAL"; "LOG"; "LOOP"; "LPRINT"; "LRUN"
      "MOD"; "MID$"; "MOVE"; "NEW"; "NEXT"; "NOT"; "ON"; "ON ERROR"; "OR"
      "PALETTE"; "PAPER"; "PAUSE"; "PEEK"; "PI"; "POINT"; "POKE"; "PRINT"
      "REMark"; "REM"; "REPEAT"; "RESTORE"; "RETURN"; "RETurn"; "REPL$"; "RIGHT$"
      "RND"; "ROUND"; "RUN"; "SAVE"; "SAVEMEM"; "SELect ON"; "SHELL"; "SGN"
      "SIN"; "STEP"; "STOP"; "STR$"; "SYSVAR"; "TAN"; "THEN"; "TIME"; "TO"
      "TRON"; "TROFF"; "UNTIL"; "VAL"; "WAIT"; "WHEN"; "WHILE"; "WINDOW"; "XOR" ]

let private withDefaultRule scopeName (state: ProcessingState) : ImplicitTypingRule =
    match Map.tryFind scopeName state.ImplicitTyping with
    | Some rule -> rule
    | None -> { Integers = Set.empty; Strings = Set.empty }

let private updateImplicitTyping scopeName (decorator: string) (names: Set<string>) (state: ProcessingState) =
    let currentRule = withDefaultRule scopeName state
    let updatedRule =
        if decorator.Contains "%" then
            { currentRule with Integers = Set.union currentRule.Integers names }
        elif decorator.Contains "$" then
            { currentRule with Strings = Set.union currentRule.Strings names }
        else
            currentRule

    { state with ImplicitTyping = Map.add scopeName updatedRule state.ImplicitTyping }

let private createCommonSymbol name category position evaluatedType =
    { Name = name
      Category = category
      EvaluatedType = evaluatedType
      Position = position }

let private createVariableSymbol name position evaluatedType =
    VariableSym { Common = createCommonSymbol name SymbolCategory.Variable position evaluatedType }

let private createParameterSymbol name position evaluatedType =
    ParameterSym { Common = createCommonSymbol name SymbolCategory.Parameter position evaluatedType; Passing = None }

let private createArraySymbol name position dimensions =
    ArraySym {
        Common = createCommonSymbol name SymbolCategory.Array position SBType.Unknown
        Dimensions = dimensions
    }

let private createFunctionSymbol name position =
    FunctionSym {
        Common = createCommonSymbol name SymbolCategory.Function position SBType.Unknown
        Parameters = []
        ReturnType = SBType.Unknown
    }

let private createProcedureSymbol name position =
    ProcedureSym {
        Common = createCommonSymbol name SymbolCategory.Procedure position SBType.Void
        Parameters = []
    }

let private createBuiltInSymbol name =
    BuiltInSym {
        Common =
            createCommonSymbol
                name
                SymbolCategory.Keyword
                { BasicLineNo = None; EditorLineNo = 0; Column = 0 }
                SBType.Unknown
    }

let private addOrUpdateSymbol mode scopeName symbol table =
    addSymbolToNamedScope mode symbol scopeName table

let private ensureScope scopeName parentScope table =
    if Map.containsKey scopeName table then
        table
    else
        table |> Map.add scopeName { Name = scopeName; Parent = parentScope; Symbols = Map.empty }

/// Pre-populate the symbol table with language keywords.
let prePopulateSymbolTable (oldState: ProcessingState) : ProcessingState =
    let tableWithGlobalScope = ensureScope globalScope None oldState.SymTab
    let newSymTab =
        keywords
        |> List.fold (fun table keyword ->
            addOrUpdateSymbol Overwrite globalScope (createBuiltInSymbol keyword) table) tableWithGlobalScope

    { oldState with SymTab = newSymTab }

let rec private addChildrenToTable mode (children: ASTNode list) (stateForChildren: ProcessingState) : State<ProcessingState, unit> =
    state {
        match children with
        | [] -> return ()
        | head :: tail ->
            do! addToTable mode head stateForChildren
            let! nextState = getState
            do! addChildrenToTable mode tail nextState
    }

and addToTable
    (mode: SymbolAddMode)
    (node: ASTNode)
    (incomingState: ProcessingState)
    : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        let scopeName = currentState.CurrentScope

        let currentRule = withDefaultRule scopeName currentState

        let symbolFromIdentifier name position =
            if currentState.InParameterList then
                createParameterSymbol name position SBType.Unknown
            else
                let inferredType =
                    if currentRule.Integers.Contains name then SBType.Integer
                    elif currentRule.Strings.Contains name then SBType.String
                    else SBType.Unknown
                createVariableSymbol name position inferredType

        let mutable updatedState = currentState
        let mutable childState = currentState

        match node.Kind with
        | NodeKind.ProcedureCall ->
            let sym = createProcedureSymbol node.Value node.Position
            updatedState <- { updatedState with SymTab = addOrUpdateSymbol mode scopeName sym updatedState.SymTab }

        | NodeKind.FunctionCall ->
            let sym = createFunctionSymbol node.Value node.Position
            updatedState <- { updatedState with SymTab = addOrUpdateSymbol mode scopeName sym updatedState.SymTab }

        | NodeKind.Implicit ->
            let names = node.Children |> List.map (fun child -> child.Value) |> Set.ofList
            let stateWithImplicit = updateImplicitTyping scopeName node.Value names updatedState
            let symTabWithImplicit =
                node.Children
                |> List.fold (fun table child ->
                    addOrUpdateSymbol mode scopeName (createVariableSymbol child.Value child.Position SBType.Unknown) table) stateWithImplicit.SymTab
            updatedState <- { stateWithImplicit with SymTab = symTabWithImplicit }

        | NodeKind.ProcedureDefinition ->
            let sym = createProcedureSymbol node.Value node.Position
            let symTab =
                updatedState.SymTab
                |> addOrUpdateSymbol mode globalScope sym
                |> ensureScope node.Value (Some globalScope)
            updatedState <- { updatedState with SymTab = symTab; CurrentScope = node.Value }
            childState <- updatedState

        | NodeKind.FunctionDefinition ->
            let sym = createFunctionSymbol node.Value node.Position
            let symTab =
                updatedState.SymTab
                |> addOrUpdateSymbol mode globalScope sym
                |> ensureScope node.Value (Some globalScope)
            updatedState <- { updatedState with SymTab = symTab; CurrentScope = node.Value }
            childState <- updatedState

        | NodeKind.Dim ->
            match node.Children with
            | [] -> ()
            | head :: tail ->
                let arraySizes =
                    tail |> List.choose (fun child -> match System.Int32.TryParse child.Value with | true, value -> Some value | _ -> None)
                let arrSym = createArraySymbol head.Value head.Position arraySizes
                updatedState <- { updatedState with SymTab = addOrUpdateSymbol mode scopeName arrSym updatedState.SymTab }

        | NodeKind.Identifier
        | NodeKind.ID ->
            let sym = symbolFromIdentifier node.Value node.Position
            updatedState <- { updatedState with SymTab = addOrUpdateSymbol mode scopeName sym updatedState.SymTab }

        | NodeKind.Parameters ->
            childState <- { updatedState with InParameterList = true }

        | NodeKind.Local ->
            match node.Children with
            | first :: _ ->
                let sym = createVariableSymbol first.Value first.Position SBType.Unknown
                updatedState <- { updatedState with SymTab = addOrUpdateSymbol mode scopeName sym updatedState.SymTab }
                childState <- updatedState
            | [] -> ()

        | _ -> ()

        do! putState updatedState
        do! addChildrenToTable mode node.Children childState

        match node.Kind with
        | NodeKind.Parameters ->
            let! stateAfterChildren = getState
            do! putState { stateAfterChildren with InParameterList = currentState.InParameterList }
        | NodeKind.ProcedureDefinition
        | NodeKind.FunctionDefinition ->
            let! stateAfterChildren = getState
            do! putState { stateAfterChildren with CurrentScope = currentState.CurrentScope }
        | _ -> ()

        return ()
    }
