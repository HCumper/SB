module SemanticAnalyzer

open Types
open Utility
open SymbolTableManager
open Monads.State
open FSharpPlus.Data
open SyntaxAst

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
    if Map.containsKey scopeName table then table
    else table |> Map.add scopeName { Name = scopeName; Parent = parentScope; Symbols = Map.empty }

let prePopulateSymbolTable (oldState: ProcessingState) : ProcessingState =
    let tableWithGlobalScope = ensureScope globalScope None oldState.SymTab
    let newSymTab =
        keywords
        |> List.fold (fun table keyword ->
            addOrUpdateSymbol Overwrite globalScope (createBuiltInSymbol keyword) table) tableWithGlobalScope

    { oldState with SymTab = newSymTab }

let private sourcePositionOf pos = sourcePositionFromTuple pos

let private posOfExpr expr =
    match expr with
    | PostfixName(pos, _, _)
    | SliceRange(pos, _, _)
    | BinaryExpr(pos, _, _, _)
    | UnaryExpr(pos, _, _)
    | NumberLiteral(pos, _)
    | StringLiteral(pos, _)
    | Identifier(pos, _) -> pos

let rec private addExprList mode exprs =
    state {
        match exprs with
        | [] -> return ()
        | head :: tail ->
            do! addExpr mode head
            do! addExprList mode tail
    }

and private addLineList mode lines =
    state {
        match lines with
        | [] -> return ()
        | head :: tail ->
            do! addLine mode head
            do! addLineList mode tail
    }

and private addStmtList mode stmts =
    state {
        match stmts with
        | [] -> return ()
        | head :: tail ->
            do! addStmt mode head
            do! addStmtList mode tail
    }

and private addExpr mode (expr: Expr) : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        let scopeName = currentState.CurrentScope
        let currentRule = withDefaultRule scopeName currentState

        let symbolFromIdentifier name position =
            let inferredType =
                if currentRule.Integers.Contains name then SBType.Integer
                elif currentRule.Strings.Contains name then SBType.String
                else SBType.Unknown
            createVariableSymbol name position inferredType

        match expr with
        | PostfixName(pos, name, None) ->
            let sym = symbolFromIdentifier name (sourcePositionOf pos)
            let symTab = addOrUpdateSymbol mode scopeName sym currentState.SymTab
            do! putState { currentState with SymTab = symTab }
        | PostfixName(_, _, Some args) ->
            do! addExprList mode args
        | SliceRange(_, lhs, rhs)
        | BinaryExpr(_, _, lhs, rhs) ->
            do! addExpr mode lhs
            do! addExpr mode rhs
        | UnaryExpr(_, _, inner) ->
            do! addExpr mode inner
        | Identifier(pos, name) ->
            let sym = symbolFromIdentifier name (sourcePositionOf pos)
            let symTab = addOrUpdateSymbol mode scopeName sym currentState.SymTab
            do! putState { currentState with SymTab = symTab }
        | NumberLiteral _
        | StringLiteral _ -> ()
    }

and private addLine mode (line: Line) : State<ProcessingState, unit> =
    state {
        let (Line(_, _, children)) = line
        do! addStmtList mode children
    }

and private addBlock mode block : State<ProcessingState, unit> =
    state {
        match block with
        | StatementBlock stmts -> do! addStmtList mode stmts
        | LineBlock lines -> do! addLineList mode lines
    }

and addStmt (mode: SymbolAddMode) (node: Stmt) : State<ProcessingState, unit> =
    state {
        let! currentState = getState
        let scopeName = currentState.CurrentScope
        let currentRule = withDefaultRule scopeName currentState

        let symbolFromIdentifier name position =
            let inferredType =
                if currentRule.Integers.Contains name then SBType.Integer
                elif currentRule.Strings.Contains name then SBType.String
                else SBType.Unknown
            createVariableSymbol name position inferredType

        match node with
        | ProcedureDef(pos, name, parameters, body) ->
            let sp = sourcePositionOf pos
            let sym = createProcedureSymbol name sp
            let symTab =
                currentState.SymTab
                |> addOrUpdateSymbol mode globalScope sym
                |> ensureScope name (Some globalScope)
            let scopedState = { currentState with SymTab = symTab; CurrentScope = name }
            do! putState scopedState
            let paramSymbols =
                parameters
                |> List.fold (fun table param ->
                    addOrUpdateSymbol mode name (createParameterSymbol param sp SBType.Unknown) table) scopedState.SymTab
            do! putState { scopedState with SymTab = paramSymbols }
            do! addLineList mode body
            let! stateAfter = getState
            do! putState { stateAfter with CurrentScope = currentState.CurrentScope }

        | FunctionDef(pos, name, parameters, body) ->
            let sp = sourcePositionOf pos
            let sym = createFunctionSymbol name sp
            let symTab =
                currentState.SymTab
                |> addOrUpdateSymbol mode globalScope sym
                |> ensureScope name (Some globalScope)
            let scopedState = { currentState with SymTab = symTab; CurrentScope = name }
            do! putState scopedState
            let paramSymbols =
                parameters
                |> List.fold (fun table param ->
                    addOrUpdateSymbol mode name (createParameterSymbol param sp SBType.Unknown) table) scopedState.SymTab
            do! putState { scopedState with SymTab = paramSymbols }
            do! addLineList mode body
            let! stateAfter = getState
            do! putState { stateAfter with CurrentScope = currentState.CurrentScope }

        | DimStmt(_, items) ->
            let symTab =
                items
                |> List.fold (fun table (name, dims) ->
                    let sizes =
                        dims
                        |> List.choose (function
                            | NumberLiteral(_, value) ->
                                match System.Int32.TryParse value with
                                | true, n -> Some n
                                | _ -> None
                            | _ -> None)
                    let pos =
                        dims
                        |> List.tryHead
                        |> Option.map (posOfExpr >> sourcePositionOf)
                        |> Option.defaultValue { BasicLineNo = None; EditorLineNo = 0; Column = 0 }
                    addOrUpdateSymbol mode scopeName (createArraySymbol name pos sizes) table) currentState.SymTab
            do! putState { currentState with SymTab = symTab }
            do! addExprList mode (items |> List.collect snd)

        | LocalStmt(pos, items) ->
            let sp = sourcePositionOf pos
            let symTab =
                items
                |> List.fold (fun table (name, _) ->
                    addOrUpdateSymbol mode scopeName (createVariableSymbol name sp SBType.Unknown) table) currentState.SymTab
            do! putState { currentState with SymTab = symTab }
            do! addExprList mode (items |> List.collect (snd >> Option.defaultValue []))

        | ImplicitStmt(_, decorator, names) ->
            let stateWithImplicit = updateImplicitTyping scopeName decorator (Set.ofList names) currentState
            let symTab =
                names
                |> List.fold (fun table name ->
                    addOrUpdateSymbol mode scopeName (createVariableSymbol name { BasicLineNo = None; EditorLineNo = 0; Column = 0 } SBType.Unknown) table) stateWithImplicit.SymTab
            do! putState { stateWithImplicit with SymTab = symTab }

        | ReferenceStmt(_, children)
        | DataStmt(_, children)
        | ReadStmt(_, children) ->
            do! addExprList mode children

        | Assignment(_, lhs, rhs) ->
            do! addExpr mode lhs
            do! addExpr mode rhs

        | ProcedureCall(pos, name, args) ->
            let sym = createProcedureSymbol name (sourcePositionOf pos)
            let symTab = addOrUpdateSymbol mode scopeName sym currentState.SymTab
            do! putState { currentState with SymTab = symTab }
            do! addExprList mode args

        | ChannelProcedureCall(pos, name, channel, args) ->
            let sym = createProcedureSymbol name (sourcePositionOf pos)
            let symTab = addOrUpdateSymbol mode scopeName sym currentState.SymTab
            do! putState { currentState with SymTab = symTab }
            do! addExpr mode channel
            do! addExprList mode args

        | ForStmt(pos, name, startExpr, endExpr, stepExpr, body) ->
            let sym = createVariableSymbol name (sourcePositionOf pos) SBType.Unknown
            let symTab = addOrUpdateSymbol mode scopeName sym currentState.SymTab
            do! putState { currentState with SymTab = symTab }
            do! addExpr mode startExpr
            do! addExpr mode endExpr
            match stepExpr with
            | Some expr -> do! addExpr mode expr
            | None -> ()
            do! addBlock mode body

        | RepeatStmt(_, _, body) ->
            do! addBlock mode body

        | IfStmt(_, cond, thenBody, elseBody) ->
            do! addExpr mode cond
            do! addBlock mode thenBody
            match elseBody with
            | Some block -> do! addBlock mode block
            | None -> ()

        | SelectStmt(_, selector, clauses) ->
            do! addExpr mode selector
            let rec addClauses remaining =
                state {
                    match remaining with
                    | [] -> return ()
                    | SelectClause(_, clauseSelector, rangeExpr, body) :: tail ->
                        do! addExpr mode clauseSelector
                        do! addExpr mode rangeExpr
                        match body with
                        | Some block -> do! addBlock mode block
                        | None -> ()
                        do! addClauses tail
                }
            do! addClauses clauses

        | WhenStmt(_, condition, body) ->
            match condition with
            | Some expr -> do! addExpr mode expr
            | None -> ()
            do! addLineList mode body

        | ReturnStmt(_, value)
        | RestoreStmt(_, value) ->
            match value with
            | Some expr -> do! addExpr mode expr
            | None -> ()

        | ExitStmt _
        | NextStmt _
        | Remark _ -> ()
    }

let addToTable (mode: SymbolAddMode) (node: Ast) (_incomingState: ProcessingState) : State<ProcessingState, unit> =
    state {
        let (Program(_, children)) = node
        do! addLineList mode children
    }
