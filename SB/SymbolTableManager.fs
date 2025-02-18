module SymbolTableManager

    /// The mode used when adding a symbol.
    type SymbolAddMode =
        | Overwrite
        | Skip

    /// A scope holds a name and an immutable map of symbols.
    type Scope<'T> = {
        Name: string
        Symbols: Map<string, 'T>
    }

    /// A symbol table is a stack (list) of scopes; the head is the innermost/current scope.
    type SymbolTable<'T> = Scope<'T> list

    /// Create an empty symbol table.
    let empty : SymbolTable<'T> = []

    /// Push a new scope (with the given name) onto the symbol table.
    let pushScope (name: string) (table: SymbolTable<'T>) : SymbolTable<'T> =
        { Name = name; Symbols = Map.empty } :: table

    /// Pop the current (innermost) scope from the symbol table.
    /// Returns None if no scope exists.
    let popScope (table: SymbolTable<'T>) : SymbolTable<'T> option =
        match table with
        | [] -> None
        | _::rest -> Some rest

    /// Add a symbol to the current scope.
    /// - If the symbol already exists and mode is Overwrite, its value is updated.
    /// - If it exists and mode is Skip, the table is left unchanged.
    /// Returns None if there is no current scope.
    let addSymbol (mode: SymbolAddMode) (key: string) (value: 'T) (table: SymbolTable<'T>) : SymbolTable<'T> option =
        match table with
        | [] -> None  // No current scope available.
        | currentScope :: rest ->
            let exists = Map.containsKey key currentScope.Symbols
            match exists, mode with
            | true, Overwrite ->
                let newSymbols = Map.add key value currentScope.Symbols
                let newScope = { currentScope with Symbols = newSymbols }
                Some (newScope :: rest)
            | true, Skip ->
                // Do nothing; return the original table.
                Some table
            | false, _ ->
                let newSymbols = Map.add key value currentScope.Symbols
                let newScope = { currentScope with Symbols = newSymbols }
                Some (newScope :: rest)

    /// Lookup a symbol by key in the stacked scopes.
    /// The search starts from the innermost scope and proceeds outward.
    /// If found, returns Some (scopeName, value); otherwise, returns None.
    let lookupSymbol (table: SymbolTable<'T>) (key: string) : (string * 'T) option =
        let rec search scopes =
            match scopes with
            | [] -> None
            | scope :: rest ->
                match Map.tryFind key scope.Symbols with
                | Some value -> Some (scope.Name, value)
                | None -> search rest
        search table

    /// Pretty print the contents of the entire symbol table.
    /// Each scope is printed with its name and the key/value pairs of its symbols.
    let prettyPrintSymbolTable (table: SymbolTable<'T>) : unit =
        table
        |> List.iter (fun scope ->
            printfn "Scope: %s" scope.Name
            scope.Symbols
            |> Map.iter (fun key value -> printfn "  %s: %A" key value)
            printfn "")
        ()


// ---------------------
// Example Usage:
//
// open SymbolTableManager
//
// // Start with an empty symbol table and push a global scope.
// let table0 = empty |> pushScope "Global"
//
// // Add a symbol "x" with value 42 in the current scope.
// let table1 =
//     match addSymbol Overwrite "x" 42 table0 with
//     | Some t -> t
//     | None -> table0
//
// // Push a new inner scope.
// let table2 = pushScope "Local" table1
//
// // Add a symbol "y" with value "hello" in the inner scope.
// let table3 =
//     match addSymbol Overwrite "y" "hello" table2 with
//     | Some t -> t
//     | None -> table2
//
// // Lookup "x" -- should find it in the Global scope.
// let result = lookupSymbol table3 "x"
// match result with
// | Some (scopeName, value) -> printfn "Found 'x' in %s with value %A" scopeName value
// | None -> printfn "'x' not found"
//
// // Pretty print the entire symbol table.
// printfn "\nSymbol Table Contents:\n%s" (prettyPrintSymbolTable table3)
// ---------------------
