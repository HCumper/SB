module TypeAnalyzer

open Types
open ProcessingTypes

/// Update a symbol's evaluated type and normalize its name based on suffixes
/// and implicit typing rules.
let updateSymbolTypeAndName
    (implicitInts: Set<string>)
    (implicitStrings: Set<string>)
    (sym: Symbol)
    : Symbol =

    let updateCommon (commonSym: CommonSymbol) =
        let baseType =
            if commonSym.Name.Contains "%" then SBType.Integer
            elif commonSym.Name.Contains "$" then SBType.String
            else commonSym.EvaluatedType

        let finalType =
            if implicitInts.Contains commonSym.Name then SBType.Integer
            elif implicitStrings.Contains commonSym.Name then SBType.String
            else baseType

        let newName =
            commonSym.Name.Replace("%", "i").Replace("$", "s")

        { commonSym with Name = newName; EvaluatedType = finalType }

    match sym with
    | VariableSym varSym ->
        VariableSym { varSym with Common = updateCommon varSym.Common }
    | ConstantSym constSym ->
        ConstantSym { constSym with Common = updateCommon constSym.Common }
    | ParameterSym paramSym ->
        ParameterSym { paramSym with Common = updateCommon paramSym.Common }
    | ArraySym arrSym ->
        ArraySym { arrSym with Common = updateCommon arrSym.Common }
    | FunctionSym funcSym ->
        FunctionSym {
            funcSym with
                Common = updateCommon funcSym.Common
                ReturnType =
                    match (updateCommon funcSym.Common).EvaluatedType with
                    | SBType.Unknown -> funcSym.ReturnType
                    | inferred -> inferred
        }
    | ProcedureSym procSym ->
        ProcedureSym { procSym with Common = updateCommon procSym.Common }
    | BuiltInSym builtInSym ->
        BuiltInSym { builtInSym with Common = updateCommon builtInSym.Common }

/// Traverse the symbol table and update each symbol's evaluated type and name.
let fillImplicitTypesAndModifyNames
    (implicitInts: Set<string>)
    (implicitStrings: Set<string>)
    (symTab: SymbolTable)
    : SymbolTable =
    symTab
    |> Map.map (fun _ scope ->
        let updatedSymbols =
            scope.Symbols
            |> Map.map (fun _ sym -> updateSymbolTypeAndName implicitInts implicitStrings sym)

        { scope with Symbols = updatedSymbols }
    )

/// Update the ProcessingState by applying the above transformation to its symbol table.
let fillImplicitTypesAndModifyNamesInState (state: ProcessingState) : ProcessingState =
    let implicitInts =
        state.ImplicitTyping
        |> Map.values
        |> Seq.collect (fun rule -> rule.Integers)
        |> Set.ofSeq

    let implicitStrings =
        state.ImplicitTyping
        |> Map.values
        |> Seq.collect (fun rule -> rule.Strings)
        |> Set.ofSeq

    let newSymTab = fillImplicitTypesAndModifyNames implicitInts implicitStrings state.SymTab
    { state with SymTab = newSymTab }
