module SBTests.HirCSharpBackendTests

open NUnit.Framework

open Types
open HIR
open HirCSharpBackend

let private pos =
    { BasicLineNo = None
      EditorLineNo = 1
      Column = 0 }

let private literalInt value = Literal(ConstInt value, HirType.Int, pos)
let private literalString value = Literal(ConstString value, HirType.String, pos)

let private storage symbol name hirType storageClass =
    { Symbol = symbol
      Slot = StorageSlotId 0
      Name = name
      Type = hirType
      Dimensions = None
      Class = storageClass
      Position = pos }

let private parameter symbol name hirType storageClass binding =
    { Storage = storage symbol name hirType storageClass
      Binding = binding }

[<Test>]
let ``generateCSharpFromHir emits globals routines data and structured control flow`` () =
    let globalSymbol = SymbolId 0
    let routineSymbol = SymbolId 1
    let parameterSymbol = SymbolId 2

    let program =
        { SymbolNames =
            [ globalSymbol, "X"
              routineSymbol, "PROC"
              parameterSymbol, "P" ]
            |> Map.ofList
          Globals = [ storage globalSymbol "X" HirType.Int GlobalStorage ]
          Routines =
            [ { Name = "PROC"
                Symbol = routineSymbol
                Parameters = [ parameter parameterSymbol "P" HirType.Int (RoutineParameterStorage "PROC") FlexibleBinding ]
                Locals = []
                Body = [ Return(Some(ReadVar(parameterSymbol, HirType.Int, pos)), pos) ]
                ReturnType = Some HirType.Int
                EndLineNumber = None
                Position = pos } ]
          DataEntries =
            [ { Slot = DataSlotId 0
                Value = literalInt 2
                Position = pos
                LineNumber = Some 20 }
              { Slot = DataSlotId 1
                Value = literalString "HELLO"
                Position = pos
                LineNumber = Some 20 } ]
          RestorePoints = [ { LineNumber = 20; Slot = DataSlotId 0 } ]
          Main =
            [ LineNumber(10, pos)
              Assign(WriteVar(globalSymbol, HirType.Int, pos), literalInt 1, pos)
              For(LoopId 0, globalSymbol, literalInt 1, literalInt 3, literalInt 1, [ Next(LoopId 0, pos) ], pos)
              Repeat(LoopId 1, AnonymousLoop, [ Exit(LoopId 1, pos) ], pos)
              Input(None, [ literalString "How many?" ], [ WriteVar(globalSymbol, HirType.Int, pos) ], pos)
              Read([ WriteVar(globalSymbol, HirType.Int, pos) ], pos)
              Restore(Some(literalInt 20), pos)
              Goto(literalInt 10, pos)
              OnGoto(literalInt 2, [ literalInt 10; literalInt 20 ], pos)
              Gosub(literalInt 10, pos) ] }

    let generated = generateCSharpFromHir "SampleProgram" program

    Assert.That(generated, Does.Contain("public static class SampleProgram"))
    Assert.That(generated, Does.Contain("using SBGeneratedRuntime;"))
    Assert.That(generated, Does.Contain("using static SBGeneratedRuntime.GeneratedRuntime;"))
    Assert.That(generated, Does.Contain("private static readonly Cell v0_X = new Cell();"))
    Assert.That(generated, Does.Contain("private static readonly object?[] __data = new object?[] { 2, \"HELLO\" };"))
    Assert.That(generated, Does.Contain("private static readonly Dictionary<int, int> __restorePoints = new Dictionary<int, int> { { 20, 0 } };"))
    Assert.That(generated, Does.Contain("private static object? r1_PROC(Cell v2_P)"))
    Assert.That(generated, Does.Contain("return v2_P.Value;"))
    Assert.That(generated, Does.Contain("line_10: ;"))
    Assert.That(generated, Does.Contain("throw new LoopControlException(0, true);"))
    Assert.That(generated, Does.Contain("throw new LoopControlException(1, false);"))
    Assert.That(generated, Does.Contain("ExecuteInput(null, new object?[] { \"How many?\" });"))
    Assert.That(generated, Does.Contain("v0_X.Value = ReadDataValue(\"int\");"))
    Assert.That(generated, Does.Contain("RestoreToLine(AsInt(20));"))
    Assert.That(generated, Does.Contain("__dispatchLine = 10;"))
    Assert.That(generated, Does.Contain("goto __dispatch;"))
    Assert.That(generated, Does.Contain("case 2:"))
    Assert.That(generated, Does.Contain("__dispatchLine = 20;"))
    Assert.That(generated, Does.Contain("var __gosubStack = new Stack<int>();"))
    Assert.That(generated, Does.Contain("__gosubStack.Push(0);"))
    Assert.That(generated, Does.Contain("__gosub_return_0: ;"))
    Assert.That(generated, Does.Contain("InitializeProgramState(__data, __restorePoints);"))
    Assert.That(generated, Does.Contain("RegisterGlobal(\"X\", v0_X);"))

[<Test>]
let ``generateCSharpFromHir emits array access routine calls and builtin function calls from hir expressions`` () =
    let globalSymbol = SymbolId 0
    let arraySymbol = SymbolId 1
    let routineSymbol = SymbolId 2
    let builtInSymbol = SymbolId 3

    let program =
        { SymbolNames =
            [ globalSymbol, "TOTAL"
              arraySymbol, "A"
              routineSymbol, "DOUBLEIT"
              builtInSymbol, "ABS" ]
            |> Map.ofList
          Globals =
            [ storage globalSymbol "TOTAL" HirType.Int GlobalStorage
              storage arraySymbol "A" (HirType.Array HirType.Int) GlobalStorage ]
          Routines =
            [ { Name = "DOUBLEIT"
                Symbol = routineSymbol
                Parameters = []
                Locals = []
                Body = [ Return(Some(Binary(Add, literalInt 1, literalInt 1, HirType.Int, pos)), pos) ]
                ReturnType = Some HirType.Int
                EndLineNumber = None
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteArrayElem(arraySymbol, [ literalInt 1 ], HirType.Int, pos), CallFunc(routineSymbol, [], HirType.Int, pos), pos)
              Assign(WriteVar(globalSymbol, HirType.Int, pos), CallFunc(builtInSymbol, [ ValueArg(Unary(Negate, literalInt 4, HirType.Int, pos)) ], HirType.Int, pos), pos)
              BuiltInCall(Print, None, [ ReadArrayElem(arraySymbol, [ literalInt 1 ], HirType.Int, pos); ReadVar(globalSymbol, HirType.Int, pos) ], pos) ] }

    let generated = generateCSharpFromHir "ExprProgram" program

    Assert.That(generated, Does.Contain("private static readonly Cell v1_A = new Cell();"))
    Assert.That(generated, Does.Contain("v1_A.Value = new Dictionary<string, Cell>(StringComparer.OrdinalIgnoreCase);"))
    Assert.That(generated, Does.Contain("SetArrayValue(v1_A, r2_DOUBLEIT(), 1);"))
    Assert.That(generated, Does.Contain("v0_TOTAL.Value = InvokeBuiltInFunction(\"ABS\", Negate(4));"))
    Assert.That(generated, Does.Contain("ExecuteBuiltInStatement(\"PRINT\", null, GetArrayValue(v1_A, 1), v0_TOTAL.Value);"))
    Assert.That(generated, Does.Contain("GetArrayValue(v1_A, 1)"))
    Assert.That(generated, Does.Not.Contain("__dispatchLine"))
    Assert.That(generated, Does.Not.Contain("__dispatch:"))
    Assert.That(generated, Does.Not.Contain("__gosubStack"))

[<Test>]
let ``generateCSharpFromHir includes expanded runtime support for memory channels and built-ins`` () =
    let memorySymbol = SymbolId 0

    let program =
        { SymbolNames = [ memorySymbol, "PEEK_W" ] |> Map.ofList
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "POKE_W", None, [ literalInt 200; literalInt 4660 ], pos)
              BuiltInCall(NamedBuiltIn "RANDOMISE", None, [ literalInt 1234 ], pos)
              BuiltInCall(NamedBuiltIn "OPEN_NEW", Some(ExplicitChannel(literalInt 9)), [ literalString "out.txt" ], pos)
              BuiltInCall(NamedBuiltIn "LINE", None, [ literalInt 0; literalInt 0; literalInt 10; literalInt 10 ], pos)
              Assign(WriteVar(memorySymbol, HirType.Int, pos), CallFunc(memorySymbol, [ ValueArg(literalInt 200) ], HirType.Int, pos), pos) ] }

    let generated = generateCSharpFromHir "RuntimeSupportProgram" program

    Assert.That(generated, Does.Contain("using static SBGeneratedRuntime.GeneratedRuntime;"))
    Assert.That(generated, Does.Contain("ExecuteBuiltInStatement(\"POKE_W\", null, 200, 4660);"))
    Assert.That(generated, Does.Contain("ExecuteBuiltInStatement(\"RANDOMISE\", null, 1234);"))
    Assert.That(generated, Does.Contain("ExecuteBuiltInStatement(\"OPEN_NEW\", 9, \"out.txt\");"))
    Assert.That(generated, Does.Contain("ExecuteBuiltInStatement(\"LINE\", null, 0, 0, 10, 10);"))
    Assert.That(generated, Does.Contain("v0_PEEK_W.Value = InvokeBuiltInFunction(\"PEEK_W\", 200);"))

[<Test>]
let ``generateCSharpFromHir emits string character reads and writes`` () =
    let textSymbol = SymbolId 0
    let charSymbol = SymbolId 1

    let program =
        { SymbolNames = [ textSymbol, "TEXT$"; charSymbol, "CH$" ] |> Map.ofList
          Globals =
            [ storage textSymbol "TEXT$" HirType.String GlobalStorage
              storage charSymbol "CH$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(textSymbol, HirType.String, pos), literalString "AB", pos)
              Assign(WriteStringChar(textSymbol, literalInt 2, HirType.String, pos), literalString "Z", pos)
              Assign(WriteVar(charSymbol, HirType.String, pos), ReadStringChar(textSymbol, literalInt 2, HirType.String, pos), pos)
              Input(None, [], [ WriteStringChar(textSymbol, literalInt 1, HirType.String, pos) ], pos)
              Read([ WriteStringChar(textSymbol, literalInt 1, HirType.String, pos) ], pos) ] }

    let generated = generateCSharpFromHir "StringCharProgram" program

    Assert.That(generated, Does.Contain("SetStringCharValue(v0_TEXT_, 2, \"Z\");"))
    Assert.That(generated, Does.Contain("v1_CH_.Value = GetStringCharValue(v0_TEXT_.Value, 2);"))
    Assert.That(generated, Does.Contain("SetStringCharValue(v0_TEXT_, 1, ReadInputValue(0, \"string\"));"))
    Assert.That(generated, Does.Contain("SetStringCharValue(v0_TEXT_, 1, ReadDataValue(\"string\"));"))

[<Test>]
let ``generateCSharpFromHir emits dynamic scope lookups and sequence for support`` () =
    let counterSymbol = SymbolId 0

    let program =
        { SymbolNames = [ counterSymbol, "COUNTER" ] |> Map.ofList
          Globals = [ storage counterSymbol "COUNTER" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(DynamicWriteVar("SCORE", HirType.Int, pos), literalInt 1, pos)
              Assign(WriteVar(counterSymbol, HirType.Int, pos), DynamicReadVar("SCORE", HirType.Int, pos), pos)
              ForSequence(LoopId 0, counterSymbol, [ literalInt 1; literalInt 2 ], literalInt 5, literalInt 6, [ literalInt 9 ], literalInt 1, [ Next(LoopId 0, pos) ], pos) ] }

    let generated = generateCSharpFromHir "DynamicProgram" program

    Assert.That(generated, Does.Contain("LookupDynamicCell(\"SCORE\").Value = 1;"))
    Assert.That(generated, Does.Contain("v0_COUNTER.Value = LookupDynamicCell(\"SCORE\").Value;"))
    Assert.That(generated, Does.Contain("bool __loop0Run(object? value)"))
    Assert.That(generated, Does.Contain("foreach (var __loop0Prefix in new object?[] { 1, 2 })"))
    Assert.That(generated, Does.Contain("foreach (var __loop0Suffix in new object?[] { 9 })"))

[<Test>]
let ``generateCSharpFromHir emits gosub dispatch and when error runtime hooks`` () =
    let valueSymbol = SymbolId 0

    let program =
        { SymbolNames = [ valueSymbol, "VALUE" ] |> Map.ofList
          Globals = [ storage valueSymbol "VALUE" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ WhenError([ BuiltInCall(NamedBuiltIn "CONTINUE", None, [], pos) ], pos)
              LineNumber(100, pos)
              BuiltInCall(NamedBuiltIn "POKE_W", None, [ literalInt -1; literalInt 1 ], pos)
              OnGosub(literalInt 1, [ literalInt 100 ], pos)
              Return(None, pos) ] }

    let generated = generateCSharpFromHir "ErrorProgram" program

    Assert.That(generated, Does.Contain("Func<Exception, ErrorAction>? __activeErrorHandler = null;"))
    Assert.That(generated, Does.Contain("ErrorAction __whenError0(Exception __sbError)"))
    Assert.That(generated, Does.Contain("RecordLastError(__sbError, null, null, 100);"))
    Assert.That(generated, Does.Contain("__activeErrorHandler = __whenError0;"))
    Assert.That(generated, Does.Contain("catch (Exception __sbError)"))
    Assert.That(generated, Does.Contain("var __errorAction = __activeErrorHandler(__sbError);"))
    Assert.That(generated, Does.Contain("ExecuteBuiltInStatement(\"CONTINUE\", null);"))
    Assert.That(generated, Does.Contain("catch (RetryControlException ex)"))
    Assert.That(generated, Does.Contain("catch (ContinueControlException ex)"))
    Assert.That(generated, Does.Contain("switch (AsInt(1))"))
    Assert.That(generated, Does.Contain("case 1:"))
    Assert.That(generated, Does.Contain("__dispatchLine = 100;"))
    Assert.That(generated, Does.Contain("switch (__gosubStack.Pop())"))
