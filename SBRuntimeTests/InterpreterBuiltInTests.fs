module SBRuntimeTests.InterpreterBuiltInTests

open System
open NUnit.Framework

open SyntaxAst
open Interpreter
open SBRuntime
open SBRuntimeTests.TestSupport

module H = HIR

[<Test>]
let ``interpreter evaluates arithmetic and print`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "x", binary "+" (num "1") (num "2")) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ id "x" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("3"))

[<Test>]
let ``interpreter calls user functions`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ FunctionDef(
                    pos,
                    "add1",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReturnStmt(pos, Some(binary "+" (id "a") (num "1"))) ]) ],
                    None) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", call "add1" [ num "3" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "x" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("4"))

[<Test>]
let ``interpreter built in numeric functions preserve real results`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a", call "ABS" [ num "-1.5" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b", call "INT" [ num "3.9" ]) ])
              Line(pos, Some 30, [ Assignment(pos, id "c", call "ROUND" [ num "3.4" ]) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b"; id "c" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1.5 3 3"))

[<Test>]
let ``interpreter val parses numeric text and returns zero for non numeric text`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a", call "VAL" [ str "\"12.5\"" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b", call "VAL" [ str "\"nope\"" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("12.5 0"))

[<Test>]
let ``interpreter left and right clamp lengths to string bounds`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a$", call "LEFT$" [ str "\"abc\""; num "5" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b$", call "RIGHT$" [ str "\"abc\""; num "-1" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "a$"; id "b$" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("abc "))

[<Test>]
let ``interpreter date uses runtime clock`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "d", call "DATE" []) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ id "d" ]) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    WriteLine = fun line -> outputs.Add(line)
                }
            Clock = fun () -> DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc) }

    let hir = lowerProgram ast
    let output = runHirProgramWithOptions options hir

    Assert.That(String.concat "|" output, Is.EqualTo("1704164645"))

[<Test>]
let ``interpreter rnd supports deterministic float integer and range forms`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a", call "RND" [ num "5" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b", call "RND" [ mkSliceRange pos (num "2") (num "4") ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b" ]) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    WriteLine = fun line -> outputs.Add(line)
                }
            Random = Random(1234) }

    let hir = lowerProgram ast
    let output = runHirProgramWithOptions options hir

    Assert.That(String.concat "|" output, Is.EqualTo("2 4"))

[<Test>]
let ``interpreter rnd without arguments returns deterministic float`` () =
    let rndId = H.SymbolId 0
    let xId = H.SymbolId 1
    let xStorage = makeStorage xId 0 "X" H.HirType.Float H.GlobalStorage
    let hir =
        makeProgram
            (Map.ofList [ rndId, "RND"; xId, "X" ])
            [ xStorage ]
            [ H.Assign(H.WriteVar(xId, H.HirType.Float, pos), H.CallFunc(rndId, [], H.HirType.Float, pos), pos)
              H.BuiltInCall(H.Print, None, [ H.ReadVar(xId, H.HirType.Float, pos) ], pos) ]

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    WriteLine = fun line -> outputs.Add(line)
                }
            Random = Random(1234) }

    let output = runHirProgramWithOptions options hir

    Assert.That(String.concat "|" output, Is.EqualTo("0.39908097935797693"))

[<Test>]
let ``interpreter built in function arity mismatch reports coded runtime error`` () =
    let absId = H.SymbolId 0
    let xId = H.SymbolId 1
    let xStorage = makeStorage xId 0 "X" H.HirType.Float H.GlobalStorage
    let hir =
        makeProgram
            (Map.ofList [ absId, "ABS"; xId, "X" ])
            [ xStorage ]
            [ H.Assign(
                H.WriteVar(xId, H.HirType.Float, pos),
                H.CallFunc(absId, [ H.ValueArg(H.Literal(H.ConstInt 1, H.HirType.Int, pos)); H.ValueArg(H.Literal(H.ConstInt 2, H.HirType.Int, pos)) ], H.HirType.Float, pos),
                pos) ]

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError BuiltInArityMismatch
    |> ignore

[<Test>]
let ``interpreter rnd unsupported arguments reports coded runtime error`` () =
    let rndId = H.SymbolId 0
    let xId = H.SymbolId 1
    let xStorage = makeStorage xId 0 "X" H.HirType.Float H.GlobalStorage
    let hir =
        makeProgram
            (Map.ofList [ rndId, "RND"; xId, "X" ])
            [ xStorage ]
            [ H.Assign(
                H.WriteVar(xId, H.HirType.Float, pos),
                H.CallFunc(
                    rndId,
                    [ H.ValueArg(H.Literal(H.ConstInt 1, H.HirType.Int, pos))
                      H.ValueArg(H.Literal(H.ConstInt 2, H.HirType.Int, pos)) ],
                    H.HirType.Float,
                    pos),
                pos) ]

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError BuiltInUnsupportedArguments
    |> ignore

[<Test>]
let ``interpreter inkey$ returns queued key and then empty string`` () =
    let inkeyId = H.SymbolId 0
    let aId = H.SymbolId 1
    let bId = H.SymbolId 2
    let aStorage = makeStorage aId 0 "A$" H.HirType.String H.GlobalStorage
    let bStorage = makeStorage bId 1 "B$" H.HirType.String H.GlobalStorage
    let keys =
        Collections.Generic.Queue<KeyInfo>(
            [ { KeyCode = int 'A'; Character = Some 'A'; Shift = false; Control = false } ])
    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadKey = fun () -> if keys.Count > 0 then Some(keys.Dequeue()) else None
                    KeyAvailable = fun () -> keys.Count > 0
                    WriteLine = fun line -> outputs.Add(line)
                } }
    let hir =
        makeProgram
            (Map.ofList [ inkeyId, "INKEY$"; aId, "A$"; bId, "B$" ])
            [ aStorage; bStorage ]
            [ H.Assign(H.WriteVar(aId, H.HirType.String, pos), H.CallFunc(inkeyId, [], H.HirType.String, pos), pos)
              H.Assign(H.WriteVar(bId, H.HirType.String, pos), H.CallFunc(inkeyId, [], H.HirType.String, pos), pos)
              H.BuiltInCall(H.Print, None, [ H.ReadVar(aId, H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.Print, None, [ H.ReadVar(bId, H.HirType.String, pos) ], pos) ]

    let output = runHirProgramWithOptions options hir

    Assert.That(String.concat "|" output, Is.EqualTo("A|"))

[<Test>]
let ``interpreter inkey$ accepts timeout and channel arguments`` () =
    let inkeyId = H.SymbolId 0
    let aId = H.SymbolId 1
    let bId = H.SymbolId 2
    let aStorage = makeStorage aId 0 "A$" H.HirType.String H.GlobalStorage
    let bStorage = makeStorage bId 1 "B$" H.HirType.String H.GlobalStorage
    let keys =
        Collections.Generic.Queue<KeyInfo>(
            [ { KeyCode = int 'X'; Character = Some 'X'; Shift = false; Control = false }
              { KeyCode = int 'Y'; Character = Some 'Y'; Shift = false; Control = false } ])
    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadKey = fun () -> if keys.Count > 0 then Some(keys.Dequeue()) else None
                    KeyAvailable = fun () -> keys.Count > 0
                    WriteLine = fun line -> outputs.Add(line)
                } }
    let hir =
        makeProgram
            (Map.ofList [ inkeyId, "INKEY$"; aId, "A$"; bId, "B$" ])
            [ aStorage; bStorage ]
            [ H.Assign(
                H.WriteVar(aId, H.HirType.String, pos),
                H.CallFunc(inkeyId, [ H.ValueArg(H.Literal(H.ConstInt -1, H.HirType.Int, pos)) ], H.HirType.String, pos),
                pos)
              H.Assign(
                H.WriteVar(bId, H.HirType.String, pos),
                H.CallFunc(
                    inkeyId,
                    [ H.ValueArg(H.Literal(H.ConstInt 3, H.HirType.Int, pos))
                      H.ValueArg(H.Literal(H.ConstInt -1, H.HirType.Int, pos)) ],
                    H.HirType.String,
                    pos),
                pos)
              H.BuiltInCall(H.Print, None, [ H.ReadVar(aId, H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.Print, None, [ H.ReadVar(bId, H.HirType.String, pos) ], pos) ]

    let output = runHirProgramWithOptions options hir

    Assert.That(String.concat "|" output, Is.EqualTo("X|Y"))

[<Test>]
let ``interpreter randomise reseeds rnd deterministically`` () =
    let rndId = H.SymbolId 0
    let aId = H.SymbolId 1
    let bId = H.SymbolId 2
    let aStorage = makeStorage aId 0 "A" H.HirType.Int H.GlobalStorage
    let bStorage = makeStorage bId 1 "B" H.HirType.Int H.GlobalStorage
    let hir =
        makeProgram
            (Map.ofList [ rndId, "RND"; aId, "A"; bId, "B" ])
            [ aStorage; bStorage ]
            [ H.BuiltInCall(H.NamedBuiltIn "RANDOMISE", None, [ H.Literal(H.ConstInt 123, H.HirType.Int, pos) ], pos)
              H.Assign(H.WriteVar(aId, H.HirType.Int, pos), H.CallFunc(rndId, [ H.ValueArg(H.Literal(H.ConstInt 100, H.HirType.Int, pos)) ], H.HirType.Int, pos), pos)
              H.BuiltInCall(H.NamedBuiltIn "RANDOMISE", None, [ H.Literal(H.ConstInt 123, H.HirType.Int, pos) ], pos)
              H.Assign(H.WriteVar(bId, H.HirType.Int, pos), H.CallFunc(rndId, [ H.ValueArg(H.Literal(H.ConstInt 100, H.HirType.Int, pos)) ], H.HirType.Int, pos), pos)
              H.BuiltInCall(H.Print, None, [ H.ReadVar(aId, H.HirType.Int, pos); H.ReadVar(bId, H.HirType.Int, pos) ], pos) ]

    let output = runHirProgramWithOptions defaultRuntimeOptions hir

    Assert.That(String.concat "|" output, Is.EqualTo("99 99"))
