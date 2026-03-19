module TestBed.UtilityTests

open NUnit.Framework

open Types
open ProcessingTypes
open TypeAnalyzer
open SyntaxAst

let private pos =
    { BasicLineNo = None
      EditorLineNo = 0
      Column = 0 }

let private variableSymbol name evaluatedType =
    VariableSym {
        Common = {
            Name = name
            EvaluatedType = evaluatedType
            Position = pos
        }
        ValueText = None
    }

let private anyAst = Program(pos, [])

[<Test>]
let ``updateSymbolTypeAndName updates symbol type to Integer when name is in implicitInts set`` () =
    let symbol = variableSymbol "var" SBType.Unknown
    let updatedSymbol = updateSymbolTypeAndName (Set.ofList [ "var" ]) Set.empty symbol
    match updatedSymbol with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.Integer))
    | _ -> Assert.Fail("Expected VariableSym")

[<Test>]
let ``updateSymbolTypeAndName updates symbol type to String when name is in implicitStrings set`` () =
    let symbol = variableSymbol "var" SBType.Unknown
    let updatedSymbol = updateSymbolTypeAndName Set.empty (Set.ofList [ "var" ]) symbol
    match updatedSymbol with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.String))
    | _ -> Assert.Fail("Expected VariableSym")

[<Test>]
let ``fillImplicitTypesAndModifyNames updates symbol type to Integer when name is in implicitInts set`` () =
    let symTab =
        Map.ofList [
            ("global",
             { Id = "global"
               Parent = None
               Symbols = Map.ofList [ ("var", variableSymbol "var" SBType.Unknown) ] })
        ]
    let updatedSymTab = fillImplicitTypesAndModifyNames (Set.ofList [ "var" ]) Set.empty symTab
    let updatedSymbol = updatedSymTab["global"].Symbols[normalizeIdentifier "var"]
    match updatedSymbol with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.Integer))
    | _ -> Assert.Fail("Expected VariableSym")

[<Test>]
let ``fillImplicitTypesAndModifyNames updates symbol type to String when name is in implicitStrings set`` () =
    let symTab =
        Map.ofList [
            ("global",
             { Id = "global"
               Parent = None
               Symbols = Map.ofList [ ("var", variableSymbol "var" SBType.Unknown) ] })
        ]
    let updatedSymTab = fillImplicitTypesAndModifyNames Set.empty (Set.ofList [ "var" ]) symTab
    let updatedSymbol = updatedSymTab["global"].Symbols[normalizeIdentifier "var"]
    match updatedSymbol with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.String))
    | _ -> Assert.Fail("Expected VariableSym")

[<Test>]
let ``fillImplicitTypesAndModifyNamesInState updates symbol type to Integer when rule contains name`` () =
    let state =
        { Ast = anyAst
          SymTab =
            Map.ofList [
                ("global",
                 { Id = "global"
                   Parent = None
                   Symbols = Map.ofList [ ("var", variableSymbol "var" SBType.Unknown) ] })
            ]
          CurrentScope = "global"
          InParameterList = false
          ImplicitTyping = Map.ofList [ ("global", { Integers = Set.ofList [ "var" ]; Strings = Set.empty }) ]
          Facts = []
          ExpressionFacts = []
          Diagnostics = []
          Errors = []
          ExprTypes = Map.empty
          TargetTypes = Map.empty
          ResolvedSymbols = Map.empty
          RoutineSymbols = Map.empty
          ParameterSymbols = Map.empty
          ActiveLoops = [] }
    let updatedState = fillImplicitTypesAndModifyNamesInState state
    let updatedSymbol = updatedState.SymTab["global"].Symbols[normalizeIdentifier "var"]
    match updatedSymbol with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.Integer))
    | _ -> Assert.Fail("Expected VariableSym")

[<Test>]
let ``fillImplicitTypesAndModifyNamesInState updates symbol type to String when rule contains name`` () =
    let state =
        { Ast = anyAst
          SymTab =
            Map.ofList [
                ("global",
                 { Id = "global"
                   Parent = None
                   Symbols = Map.ofList [ ("var", variableSymbol "var" SBType.Unknown) ] })
            ]
          CurrentScope = "global"
          InParameterList = false
          ImplicitTyping = Map.ofList [ ("global", { Integers = Set.empty; Strings = Set.ofList [ "var" ] }) ]
          Facts = []
          ExpressionFacts = []
          Diagnostics = []
          Errors = []
          ExprTypes = Map.empty
          TargetTypes = Map.empty
          ResolvedSymbols = Map.empty
          RoutineSymbols = Map.empty
          ParameterSymbols = Map.empty
          ActiveLoops = [] }
    let updatedState = fillImplicitTypesAndModifyNamesInState state
    let updatedSymbol = updatedState.SymTab["global"].Symbols[normalizeIdentifier "var"]
    match updatedSymbol with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.String))
    | _ -> Assert.Fail("Expected VariableSym")

[<Test>]
let ``fillImplicitTypesAndModifyNamesInState applies implicit typing case-insensitively`` () =
    let state =
        { Ast = anyAst
          SymTab =
            Map.ofList [
                ("global",
                 { Id = "global"
                   Parent = None
                   Symbols = Map.ofList [ ("VAR", variableSymbol "VAR" SBType.Unknown) ] })
            ]
          CurrentScope = "global"
          InParameterList = false
          ImplicitTyping = Map.ofList [ ("global", { Integers = Set.ofList [ "var" ]; Strings = Set.empty }) ]
          Facts = []
          ExpressionFacts = []
          Diagnostics = []
          Errors = []
          ExprTypes = Map.empty
          TargetTypes = Map.empty
          ResolvedSymbols = Map.empty
          RoutineSymbols = Map.empty
          ParameterSymbols = Map.empty
          ActiveLoops = [] }
    let updatedState = fillImplicitTypesAndModifyNamesInState state
    let updatedSymbol = updatedState.SymTab["global"].Symbols[normalizeIdentifier "VAR"]
    match updatedSymbol with
    | VariableSym sym -> Assert.That(sym.Common.EvaluatedType, Is.EqualTo(SBType.Integer))
    | _ -> Assert.Fail("Expected VariableSym")
