module TypeAnalyzer

open Types
open ProcessingTypes

// TypeAnalyzer is a post-pass over the completed symbol table.
//
// Semantic analysis initially records declarations, references, inferred types,
// and implicit typing facts. This module applies the accumulated IMPLICIT rules
// and suffix-based naming conventions across the finished symbol table in one
// place so the rest of the pipeline can consume a normalized view.
//
// It is intentionally separate from the main semantic walk because IMPLICIT rules
// can affect symbols declared earlier or later in the source.
/// Update a symbol's evaluated type and normalize its name based on suffixes
/// and implicit typing rules.
let updateSymbolTypeAndName
    (implicitInts: Set<string>)
    (implicitStrings: Set<string>)
    (sym: Symbol)
    : Symbol =

    let implicitInts = implicitInts |> Set.map normalizeIdentifier
    let implicitStrings = implicitStrings |> Set.map normalizeIdentifier

    let updateCommon (commonSym: CommonSymbol) =
        let normalizedName = normalizeIdentifier commonSym.Name
        // Suffixes are treated as an explicit declaration of the final symbol type,
        // then IMPLICIT rules are applied for otherwise-unsuffixed names.
        let baseType =
            if commonSym.Name.Contains "%" then SBType.Integer
            elif commonSym.Name.Contains "$" then SBType.String
            else commonSym.EvaluatedType

        let finalType =
            if implicitInts.Contains normalizedName then SBType.Integer
            elif implicitStrings.Contains normalizedName then SBType.String
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
        let updatedCommon = updateCommon arrSym.Common
        ArraySym {
            arrSym with
                Common = updatedCommon
                ElementType = updatedCommon.EvaluatedType
        }
    | FunctionSym funcSym ->
        let updatedCommon = updateCommon funcSym.Common
        FunctionSym {
            funcSym with
                Common = updatedCommon
                Parameters = funcSym.Parameters |> List.map (fun parameter -> { parameter with Common = updateCommon parameter.Common })
                ReturnType =
                    match updatedCommon.EvaluatedType with
                    | SBType.Unknown -> funcSym.ReturnType
                    | inferred -> inferred
        }
    | ProcedureSym procSym ->
        ProcedureSym {
            procSym with
                Common = updateCommon procSym.Common
                Parameters = procSym.Parameters |> List.map (fun parameter -> { parameter with Common = updateCommon parameter.Common })
        }
    | BuiltInSym builtInSym ->
        BuiltInSym { builtInSym with Common = updateCommon builtInSym.Common }

/// Traverse the symbol table and update each symbol's evaluated type and name.
let fillImplicitTypesAndModifyNames
    (implicitInts: Set<string>)
    (implicitStrings: Set<string>)
    (symTab: SymbolTable)
    : SymbolTable =
    // The whole-table rewrite keeps symbol keys and symbol payload names aligned
    // after normalization, which is important because later lookups use the
    // normalized key form.
    symTab
    |> Map.map (fun _ scope ->
        let updatedSymbols =
            scope.Symbols
            |> Map.toList
            |> List.map (fun (_, sym) ->
                let updated = updateSymbolTypeAndName implicitInts implicitStrings sym
                Symbol.normalizedName updated, updated)
            |> Map.ofList

        { scope with Symbols = updatedSymbols }
    )

/// Update the ProcessingState by applying the above transformation to its symbol table.
let fillImplicitTypesAndModifyNamesInState (state: ProcessingState) : ProcessingState =
    // Aggregate all collected IMPLICIT rules first, then rewrite the symbol table
    // once so the normalization step is not order-sensitive.
    let implicitInts =
        state.ImplicitTyping
        |> Map.values
        |> Seq.collect (fun rule -> rule.Integers)
        |> Seq.map normalizeIdentifier
        |> Set.ofSeq

    let implicitStrings =
        state.ImplicitTyping
        |> Map.values
        |> Seq.collect (fun rule -> rule.Strings)
        |> Seq.map normalizeIdentifier
        |> Set.ofSeq

    let newSymTab = fillImplicitTypesAndModifyNames implicitInts implicitStrings state.SymTab
    { state with SymTab = newSymTab }
