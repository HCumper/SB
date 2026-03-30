module SBTests.HirCBackendTests

open NUnit.Framework

open Types
open HIR
open HirCBackend

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
      Class = storageClass
      Position = pos }

let private parameter symbol name hirType storageClass binding =
    { Storage = storage symbol name hirType storageClass
      Binding = binding }

[<Test>]
let ``generateCFromHir emits globals routines labels and loop jumps`` () =
    let globalSymbol = SymbolId 0
    let routineSymbol = SymbolId 1
    let parameterSymbol = SymbolId 2

    let program =
        { SymbolNames = [ globalSymbol, "X"; routineSymbol, "PROC"; parameterSymbol, "P" ] |> Map.ofList
          Globals = [ storage globalSymbol "X" HirType.Int GlobalStorage ]
          Routines =
            [ { Name = "PROC"
                Symbol = routineSymbol
                Parameters = [ parameter parameterSymbol "P" HirType.Int (RoutineParameterStorage "PROC") FlexibleBinding ]
                Locals = []
                Body = [ Return(Some(ReadVar(parameterSymbol, HirType.Int, pos)), pos) ]
                ReturnType = Some HirType.Int
                Position = pos } ]
          DataEntries =
            [ { Slot = DataSlotId 0; Value = literalInt 2; Position = pos; LineNumber = Some 20 }
              { Slot = DataSlotId 1; Value = literalString "HELLO"; Position = pos; LineNumber = Some 20 } ]
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

    let generated = generateCFromHir "sample_program" program

    Assert.That(generated, Does.Contain("static Value v0_X;"))
    Assert.That(generated, Does.Contain("static Value r1_PROC(Value v2_P);"))
    Assert.That(generated, Does.Contain("static DataValue sb_data[] = { { TYPE_INT, 2, 2, NULL, NULL }, { TYPE_STRING, 0, 0.0, \"HELLO\", NULL } };"))
    Assert.That(generated, Does.Contain("line_10: ;"))
    Assert.That(generated, Does.Contain("goto loop_0_next;"))
    Assert.That(generated, Does.Contain("loop_1_exit: ;"))
    Assert.That(generated, Does.Contain("execute_input(make_null(), 1, make_string(\"How many?\"));"))
    Assert.That(generated, Does.Contain("v0_X = read_data_value(TYPE_INT);"))
    Assert.That(generated, Does.Contain("restore_to_line(as_int(make_int(20)));"))
    Assert.That(generated, Does.Contain("case 2: goto line_20;"))
    Assert.That(generated, Does.Contain("runtime_not_supported(\"GOSUB is not supported by the generated C backend yet.\");"))

[<Test>]
let ``generateCFromHir emits arrays routine calls and builtin function calls`` () =
    let globalSymbol = SymbolId 0
    let arraySymbol = SymbolId 1
    let routineSymbol = SymbolId 2
    let builtInSymbol = SymbolId 3

    let program =
        { SymbolNames = [ globalSymbol, "TOTAL"; arraySymbol, "A"; routineSymbol, "DOUBLEIT"; builtInSymbol, "ABS" ] |> Map.ofList
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
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteArrayElem(arraySymbol, [ literalInt 1 ], HirType.Int, pos), CallFunc(routineSymbol, [], HirType.Int, pos), pos)
              Assign(WriteVar(globalSymbol, HirType.Int, pos), CallFunc(builtInSymbol, [ ValueArg(Unary(Negate, literalInt 4, HirType.Int, pos)) ], HirType.Int, pos), pos)
              Assign(WriteVar(globalSymbol, HirType.Int, pos), ReadArrayElem(arraySymbol, [ literalInt 1 ], HirType.Int, pos), pos) ] }

    let generated = generateCFromHir "expr_program" program

    Assert.That(generated, Does.Contain("v1_A = make_array();"))
    Assert.That(generated, Does.Contain("set_array_value(&v1_A, r2_DOUBLEIT(), 1, make_int(1));"))
    Assert.That(generated, Does.Contain("v0_TOTAL = invoke_builtin_function(\"ABS\", negate_value(make_int(4)));"))
    Assert.That(generated, Does.Contain("get_array_value(&v1_A, 1, make_int(1))"))
