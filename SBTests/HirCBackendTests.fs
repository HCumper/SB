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
      Dimensions = None
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
                EndLineNumber = None
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

    Assert.That(generated, Does.Contain("#include \"sbruntime_c.h\""))
    Assert.That(generated, Does.Contain("static Cell v0_X;"))
    Assert.That(generated, Does.Contain("static Value r1_PROC(ParamBinding v2_P_arg);"))
    Assert.That(generated, Does.Contain("DataValue sb_data[] = { { TYPE_INT, 2, 2, NULL, NULL }, { TYPE_STRING, 0, 0.0, \"HELLO\", NULL } };"))
    Assert.That(generated, Does.Contain("int main(void);"))
    Assert.That(generated, Does.Contain("line_10: ;"))
    Assert.That(generated, Does.Contain("goto loop_0_next;"))
    Assert.That(generated, Does.Contain("loop_1_exit: ;"))
    Assert.That(generated, Does.Contain("execute_input(make_null(), 1, make_string(\"How many?\"));"))
    Assert.That(generated, Does.Contain("v0_X = make_cell(make_null());"))
    Assert.That(generated, Does.Contain("register_global(\"X\", &v0_X);"))
    Assert.That(generated, Does.Contain("(v2_P)->value"))
    Assert.That(generated, Does.Contain("(&v0_X)->value = read_data_value(TYPE_INT);"))
    Assert.That(generated, Does.Contain("restore_to_line(as_int(make_int(20)));"))
    Assert.That(generated, Does.Contain("case 2: goto line_20;"))
    Assert.That(generated, Does.Contain("sb_runtime_init();"))
    Assert.That(generated, Does.Contain("int __gosub_stack[1];"))
    Assert.That(generated, Does.Contain("__gosub_stack[__gosub_top++] = 0;"))
    Assert.That(generated, Does.Contain("__gosub_return_0: ;"))
    Assert.That(generated, Does.Not.Contain("_P_(("))
    Assert.That(generated, Does.Not.Contain("for (int "))

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
                EndLineNumber = None
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteArrayElem(arraySymbol, [ literalInt 1 ], HirType.Int, pos), CallFunc(routineSymbol, [], HirType.Int, pos), pos)
              Assign(WriteVar(globalSymbol, HirType.Int, pos), CallFunc(builtInSymbol, [ ValueArg(Unary(Negate, literalInt 4, HirType.Int, pos)) ], HirType.Int, pos), pos)
              Assign(WriteVar(globalSymbol, HirType.Int, pos), ReadArrayElem(arraySymbol, [ literalInt 1 ], HirType.Int, pos), pos) ] }

    let generated = generateCFromHir "expr_program" program

    Assert.That(generated, Does.Contain("v1_A = make_cell(make_array());"))
    Assert.That(generated, Does.Contain("set_array_value(&v1_A, r2_DOUBLEIT(), 1, make_int(1));"))
    Assert.That(generated, Does.Contain("(&v0_TOTAL)->value = invoke_builtin_function(\"ABS\", 1, negate_value(make_int(4)));"))
    Assert.That(generated, Does.Contain("get_array_value(&v1_A, 1, make_int(1))"))
    Assert.That(generated, Does.Not.Contain("line_"))
    Assert.That(generated, Does.Not.Contain("goto line_"))
    Assert.That(generated, Does.Not.Contain("_P_(("))

[<Test>]
let ``generateCFromHir expands ranged rnd into two builtin arguments`` () =
    let rndSymbol = SymbolId 0
    let valueSymbol = SymbolId 1

    let program =
        { SymbolNames = [ rndSymbol, "RND"; valueSymbol, "VALUE" ] |> Map.ofList
          Globals = [ storage valueSymbol "VALUE" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(
                WriteVar(valueSymbol, HirType.Int, pos),
                CallFunc(
                    rndSymbol,
                    [ ValueArg(Binary(SliceRange, literalInt 2, literalInt 4, HirType.Int, pos)) ],
                    HirType.Int,
                    pos),
                pos) ] }

    let generated = generateCFromHir "rnd_range_program" program

    Assert.That(generated, Does.Contain("invoke_builtin_function(\"RND\", 2, make_int(2), make_int(4))"))

[<Test>]
let ``generateCFromHir registers declared array dimensions for dimn support`` () =
    let arraySymbol = SymbolId 0
    let dimnSymbol = SymbolId 1
    let resultSymbol = SymbolId 2

    let program =
        { SymbolNames = [ arraySymbol, "A"; dimnSymbol, "DIMN"; resultSymbol, "RESULT" ] |> Map.ofList
          Globals =
            [ { storage arraySymbol "A" (HirType.Array HirType.Int) GlobalStorage with Dimensions = Some [ 10; 17 ] }
              storage resultSymbol "RESULT" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(
                WriteVar(resultSymbol, HirType.Int, pos),
                CallFunc(
                    dimnSymbol,
                    [ ValueArg(ReadVar(arraySymbol, HirType.Array HirType.Int, pos))
                      ValueArg(literalInt 2) ],
                    HirType.Int,
                    pos),
                pos) ] }

    let generated = generateCFromHir "dimn_program" program

    Assert.That(generated, Does.Contain("register_array_dimensions(&v0_A, 2, 10, 17);"))
    Assert.That(generated, Does.Contain("invoke_builtin_function(\"DIMN\", 2, (&v0_A)->value, make_int(2))"))

[<Test>]
let ``generateCFromHir includes expanded runtime support for memory channels and built-ins`` () =
    let memorySymbol = SymbolId 0

    let program =
        { SymbolNames = [ memorySymbol, "PEEK_W" ] |> Map.ofList
          Globals = [ storage memorySymbol "PEEK_W" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "POKE_W", None, [ literalInt 200; literalInt 4660 ], pos)
              BuiltInCall(NamedBuiltIn "RANDOMISE", None, [ literalInt 1234 ], pos)
              BuiltInCall(NamedBuiltIn "OPEN_NEW", Some(ExplicitChannel(literalInt 9)), [ literalString "out.dat" ], pos)
              BuiltInCall(NamedBuiltIn "LINE", None, [ literalInt 0; literalInt 0; literalInt 10; literalInt 10 ], pos)
              Assign(WriteVar(memorySymbol, HirType.Int, pos), CallFunc(memorySymbol, [ ValueArg(literalInt 200) ], HirType.Int, pos), pos) ] }

    let generated = generateCFromHir "runtime_support_program" program

    Assert.That(generated, Does.Contain("#include \"sbruntime_c.h\""))
    Assert.That(generated, Does.Contain("execute_builtin_statement(\"LINE\", make_null(), 4, make_int(0), make_int(0), make_int(10), make_int(10));"))

[<Test>]
let ``generateCFromHir emits string character reads and writes`` () =
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

    let generated = generateCFromHir "string_char_program" program

    Assert.That(generated, Does.Contain("set_string_char_value(&v0_TEXT_, as_int(make_int(2)), make_string(\"Z\"));"))
    Assert.That(generated, Does.Contain("(&v1_CH_)->value = get_string_char_value(&v0_TEXT_, as_int(make_int(2)));"))
    Assert.That(generated, Does.Contain("set_string_char_value(&v0_TEXT_, as_int(make_int(1)), read_input_value(0, TYPE_STRING));"))
    Assert.That(generated, Does.Contain("set_string_char_value(&v0_TEXT_, as_int(make_int(1)), read_data_value(TYPE_STRING));"))

[<Test>]
let ``generateCFromHir emits dynamic scope lookups and by reference parameter bindings`` () =
    let globalSymbol = SymbolId 0
    let routineSymbol = SymbolId 1
    let parameterSymbol = SymbolId 2

    let program =
        { SymbolNames = [ globalSymbol, "TOTAL"; routineSymbol, "BUMP"; parameterSymbol, "P" ] |> Map.ofList
          Globals = [ storage globalSymbol "TOTAL" HirType.Int GlobalStorage ]
          Routines =
            [ { Name = "BUMP"
                Symbol = routineSymbol
                Parameters = [ parameter parameterSymbol "P" HirType.Int (RoutineParameterStorage "BUMP") FlexibleBinding ]
                Locals = []
                Body =
                  [ Assign(DynamicWriteVar("TOTAL", HirType.Int, pos), Binary(Add, DynamicReadVar("TOTAL", HirType.Int, pos), literalInt 1, HirType.Int, pos), pos)
                    Assign(WriteVar(parameterSymbol, HirType.Int, pos), Binary(Add, ReadVar(parameterSymbol, HirType.Int, pos), literalInt 2, HirType.Int, pos), pos)
                    Return(Some(ReadVar(parameterSymbol, HirType.Int, pos)), pos) ]
                ReturnType = Some HirType.Int
                EndLineNumber = None
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(globalSymbol, HirType.Int, pos), literalInt 1, pos)
              ProcCall(routineSymbol, None, [ RefArg(WriteVar(globalSymbol, HirType.Int, pos)) ], pos)
              Assign(WriteVar(globalSymbol, HirType.Int, pos), CallFunc(routineSymbol, [ ValueArg(literalInt 5) ], HirType.Int, pos), pos) ] }

    let generated = generateCFromHir "dynamic_param_program" program

    Assert.That(generated, Does.Contain("register_global(\"TOTAL\", &v0_TOTAL);"))
    Assert.That(generated, Does.Contain("push_dynamic_frame(__frame_bindings, 1);"))
    Assert.That(generated, Does.Contain("__frame_bindings[0].cell = v2_P;"))
    Assert.That(generated, Does.Contain("(lookup_dynamic_cell(\"TOTAL\"))->value = add_value((lookup_dynamic_cell(\"TOTAL\"))->value, make_int(1));"))
    Assert.That(generated, Does.Contain("r1_BUMP(make_ref_arg(&v0_TOTAL));"))
    Assert.That(generated, Does.Contain("r1_BUMP(make_value_arg(make_int(5)))"))

[<Test>]
let ``generateCFromHir emits sequence for dispatch`` () =
    let counterSymbol = SymbolId 0

    let program =
        { SymbolNames = [ counterSymbol, "COUNTER" ] |> Map.ofList
          Globals = [ storage counterSymbol "COUNTER" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ ForSequence(LoopId 0, counterSymbol, [ literalInt 1; literalInt 2 ], literalInt 5, literalInt 6, [ literalInt 9 ], literalInt 1, [ Next(LoopId 0, pos) ], pos) ] }

    let generated = generateCFromHir "sequence_loop_program" program

    Assert.That(generated, Does.Contain("Value loop_0_prefix_values[] = { make_int(1), make_int(2) };"))
    Assert.That(generated, Does.Contain("int loop_0_phase = 0;"))
    Assert.That(generated, Does.Contain("loop_0_dispatch: ;"))
    Assert.That(generated, Does.Contain("(&v0_COUNTER)->value = loop_0_prefix_values[loop_0_prefix_index++];"))
    Assert.That(generated, Does.Contain("(&v0_COUNTER)->value = make_int(loop_0_range_index);"))
    Assert.That(generated, Does.Contain("Value loop_0_suffix_values[] = { make_int(9) };"))
    Assert.That(generated, Does.Contain("goto loop_0_body;"))
    Assert.That(generated, Does.Not.Contain("Sequence FOR loops are not supported by the generated C backend yet."))

[<Test>]
let ``generateCFromHir emits when error handler dispatch when needed`` () =
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
              Return(None, pos) ] }

    let generated = generateCFromHir "when_error_program" program

    Assert.That(generated, Does.Contain("int __active_when_error = -1;"))
    Assert.That(generated, Does.Contain("jmp_buf __err_jmp;"))
    Assert.That(generated, Does.Contain("sb_push_error_frame(&__err_jmp);"))
    Assert.That(generated, Does.Contain("goto __when_error_dispatch;"))
    Assert.That(generated, Does.Contain("__when_error_0: ;"))
    Assert.That(generated, Does.Contain("__error_action = 2;"))
    Assert.That(generated, Does.Contain("goto __when_error_resume;"))
    Assert.That(generated, Does.Contain("switch (__active_when_error)"))

[<Test>]
let ``generateCFromHir emits dynamic goto and gosub line dispatch`` () =
    let targetSymbol = SymbolId 0

    let program =
        { SymbolNames = [ targetSymbol, "TARGET" ] |> Map.ofList
          Globals = [ storage targetSymbol "TARGET" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ LineNumber(10, pos)
              Assign(WriteVar(targetSymbol, HirType.Int, pos), literalInt 20, pos)
              Goto(ReadVar(targetSymbol, HirType.Int, pos), pos)
              LineNumber(20, pos)
              Gosub(ReadVar(targetSymbol, HirType.Int, pos), pos)
              Return(None, pos) ] }

    let generated = generateCFromHir "dynamic_line_program" program

    Assert.That(generated, Does.Contain("switch (as_int((&v0_TARGET)->value))"))
    Assert.That(generated, Does.Contain("case 10: goto line_10;"))
    Assert.That(generated, Does.Contain("case 20: goto line_20;"))
    Assert.That(generated, Does.Contain("sb_raise_error(-21, \"ERR_BL\", \"Bad line of Basic\");"))
    Assert.That(generated, Does.Not.Contain("Dynamic GOTO is not supported by the generated C backend yet."))
    Assert.That(generated, Does.Not.Contain("Dynamic GOSUB is not supported by the generated C backend yet."))

[<Test>]
let ``generateCFromHir emits dynamic on goto and on gosub target dispatch`` () =
    let selectorSymbol = SymbolId 0
    let targetSymbol = SymbolId 1

    let program =
        { SymbolNames = [ selectorSymbol, "SEL"; targetSymbol, "TARGET" ] |> Map.ofList
          Globals =
            [ storage selectorSymbol "SEL" HirType.Int GlobalStorage
              storage targetSymbol "TARGET" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ LineNumber(10, pos)
              OnGoto(ReadVar(selectorSymbol, HirType.Int, pos), [ ReadVar(targetSymbol, HirType.Int, pos); literalInt 10 ], pos)
              OnGosub(ReadVar(selectorSymbol, HirType.Int, pos), [ ReadVar(targetSymbol, HirType.Int, pos); literalInt 10 ], pos)
              Return(None, pos) ] }

    let generated = generateCFromHir "dynamic_on_line_program" program

    Assert.That(generated, Does.Contain("case 1:"))
    Assert.That(generated, Does.Contain("switch (as_int((&v1_TARGET)->value))"))
    Assert.That(generated, Does.Not.Contain("Dynamic GOSUB is not supported by the generated C backend yet."))
