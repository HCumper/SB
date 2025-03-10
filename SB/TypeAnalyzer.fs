module TypeAnalyzer

open Utility
open SemanticAnalyzer

/// Update a symbol’s evaluated type and modify its name based on both its suffix
/// and its membership in the implicit sets. Specifically:
/// - If the symbol’s name ends with "%" or is in the ImplicitInts set, then mark it as IntegerType.
/// - If it ends with "$" or is in the ImplicitStrings set, then mark it as StringType.
/// Also, "%" is replaced with "i" and "$" with "s" in the symbol’s name.
/// Updates a single Symbol's type/name based on implicit sets.
let updateSymbolTypeAndName
    (implicitInts: Set<string>)
    (implicitStrings: Set<string>)
    (sym: Symbol)
    : Symbol =
    
    let updateCommon (commonSym: CommonSymbol) =
        let baseType =
            if commonSym.Name.Contains "%" then Integer
            elif commonSym.Name.Contains "$" then String
            else commonSym.EvaluatedType

        let finalType =
            if implicitInts.Contains commonSym.Name then Integer
            elif implicitStrings.Contains commonSym.Name then String
            else baseType

        let newName =
            commonSym.Name.Replace("%", "i").Replace("$", "s")

        { commonSym with Name = newName; EvaluatedType = finalType }

    match sym with
    | Common commonSym ->
        Common (updateCommon commonSym)
    | Array arrSym ->
        let updatedCommon = updateCommon arrSym.Common
        Array { arrSym with Common = updatedCommon }

/// Traverse the symbol table (a Map of scopes),
/// and update each symbol’s evaluated type and name.
let fillImplicitTypesAndModifyNames
    (implicitInts: Set<string>)
    (implicitStrings: Set<string>)
    (symTab: SymbolTable)
    : SymbolTable =
    symTab
    |> Map.map (fun scopeKey scope ->
        // Map over each symbol in this scope’s Symbols map.
        let updatedSymbols =
            scope.Symbols
            |> Map.map (fun symKey sym ->
                updateSymbolTypeAndName implicitInts implicitStrings sym
            )

        // Return a new scope with the updated Symbols.
        { scope with Symbols = updatedSymbols }
    )

/// Update the ProcessingState by applying the above transformation to its symbol table.
/// This function uses the ImplicitInts and ImplicitStrings sets from the state.
let fillImplicitTypesAndModifyNamesInState (state: ProcessingState) : ProcessingState =
    let newSymTab = fillImplicitTypesAndModifyNames state.ImplicitInts state.ImplicitStrings state.SymTab
    { state with SymTab = newSymTab }

