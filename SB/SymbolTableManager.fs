module SymbolTableManager

    open Utility
    
    /// The mode used when adding a symbol.
    type SymbolAddMode =
        | Overwrite
        | Skip


    /// A symbol table is a stack (list) of scopes; the head is the innermost/current scope.
    type SymbolTable<'T> = Scope<'T> list

    /// Create an empty symbol table.
    let empty : SymbolTable<'T> = []

    /// Push a new scope (with the given name) onto the symbol table.
    let pushScope (name: string) (table: SymbolTable<Symbol>) : SymbolTable<Symbol> =
        { Name = name; Symbols = Map.empty } :: table

    /// Pop the current (innermost) scope from the symbol table.
    /// Returns None if no scope exists.
    let popScope (table: SymbolTable<'T>) : SymbolTable<'T> option =
        match table with
        | [] -> None
        | _::rest -> Some rest

    /// Helper: update a given scope with a new symbol (or update an existing symbol)
    /// and then rebuild the symbol table by prepending the updated scope to the rest.
    let private addSymbolToScopeHelper (mode: SymbolAddMode) (key: string) (value: 'T) (scope: Scope<'T>) : Scope<'T> =
        if Map.containsKey key scope.Symbols then
            match mode with
            | Overwrite -> { scope with Symbols = Map.add key value scope.Symbols }
            | Skip -> scope
        else
            { scope with Symbols = Map.add key value scope.Symbols }

    /// Add a symbol to the current (innermost) scope.
    let addSymbolToCurrentScope (mode: SymbolAddMode) (key: string) (value: 'T) (table: SymbolTable<'T>) : SymbolTable<'T> =
        match table with
        | currentScope :: rest -> (addSymbolToScopeHelper mode key value currentScope) :: rest

    let addSymbolToGlobalScope (mode: SymbolAddMode) (key: string) (value: 'T) (table: SymbolTable<'T>) : SymbolTable<'T> =
        match (List.rev table) with
        | globalScope :: rest ->
        addSymbolToScopeHelper mode key value globalScope :: rest
        |> List.rev

    

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
