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
                    None, None) ])
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
let ``interpreter rnd range accepts mixed numeric bound types`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "b", num "2.0") ])
              Line(pos, Some 20, [ Assignment(pos, id "a", call "RND" [ mkSliceRange pos (num "0") (id "b") ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "a" ]) ]) ])

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

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

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
let ``interpreter trig functions handle standard angles`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a", call "SIN" [ num "0" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b", call "COS" [ num "0" ]) ])
              Line(pos, Some 30, [ Assignment(pos, id "c", call "TAN" [ num "0" ]) ])
              Line(pos, Some 40, [ Assignment(pos, id "d", call "DEG" [ num "3.141592653589793" ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b"; id "c"; id "d" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("0 1 0 180"))

[<Test>]
let ``runtime trig inverse and angle conversion functions return expected values`` () =
    let evaluateOne name value =
        match BuiltInFunctions.evaluate name true (fun () -> DateTime.UnixEpoch) (fun () -> Random(1234)) (fun _ -> None) (fun () -> None) (fun _ -> 0) (fun _ -> false) false [ RuntimeValues.ofFloat value ] with
        | Result.Ok(Numeric(FloatNumber number)) -> number
        | Result.Ok other -> Assert.Fail($"Expected numeric float result for {name}, got %A{other}"); 0.0
        | Result.Error err -> Assert.Fail($"Expected {name} to succeed, got %A{err}"); 0.0

    let asinHalf = evaluateOne "ASIN" 0.5
    let acosHalf = evaluateOne "ACOS" 0.5
    let atanOne = evaluateOne "ATAN" 1.0
    let acotOne = evaluateOne "ACOT" 1.0
    let cotQuarterPi = evaluateOne "COT" (Math.PI / 4.0)
    let rad180 = evaluateOne "RAD" 180.0

    Assert.That(asinHalf, Is.EqualTo(Math.PI / 6.0).Within(1e-12))
    Assert.That(acosHalf, Is.EqualTo(Math.PI / 3.0).Within(1e-12))
    Assert.That(atanOne, Is.EqualTo(Math.PI / 4.0).Within(1e-12))
    Assert.That(acotOne, Is.EqualTo(Math.PI / 4.0).Within(1e-12))
    Assert.That(cotQuarterPi, Is.EqualTo(1.0).Within(1e-12))
    Assert.That(rad180, Is.EqualTo(Math.PI).Within(1e-12))

[<Test>]
let ``interpreter remaining math functions handle simple numeric cases`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a", call "SGN" [ num "-4" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b", call "SQRT" [ num "9" ]) ])
              Line(pos, Some 30, [ Assignment(pos, id "c", call "EXP" [ num "0" ]) ])
              Line(pos, Some 40, [ Assignment(pos, id "d", call "LOG10" [ num "1000" ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b"; id "c"; id "d" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("-1 3 1 3"))

[<Test>]
let ``runtime remaining math functions return expected values`` () =
    let evaluateNoArg name =
        match BuiltInFunctions.evaluate name true (fun () -> DateTime.UnixEpoch) (fun () -> Random(1234)) (fun _ -> None) (fun () -> None) (fun _ -> 0) (fun _ -> false) false [] with
        | Result.Ok(Numeric(FloatNumber number)) -> number
        | Result.Ok other -> Assert.Fail($"Expected numeric float result for {name}, got %A{other}"); 0.0
        | Result.Error err -> Assert.Fail($"Expected {name} to succeed, got %A{err}"); 0.0

    let evaluateOne name value =
        match BuiltInFunctions.evaluate name true (fun () -> DateTime.UnixEpoch) (fun () -> Random(1234)) (fun _ -> None) (fun () -> None) (fun _ -> 0) (fun _ -> false) false [ RuntimeValues.ofFloat value ] with
        | Result.Ok(Numeric(FloatNumber number)) -> number
        | Result.Ok other -> Assert.Fail($"Expected numeric float result for {name}, got %A{other}"); 0.0
        | Result.Error err -> Assert.Fail($"Expected {name} to succeed, got %A{err}"); 0.0

    let expZero = evaluateOne "EXP" 0.0
    let lnE = evaluateOne "LN" Math.E
    let logHundred = evaluateOne "LOG" 100.0
    let log10Thousand = evaluateOne "LOG10" 1000.0
    let sqrtNine = evaluateOne "SQRT" 9.0
    let sgnNegative = evaluateOne "SGN" -5.0
    let sgnZero = evaluateOne "SGN" 0.0
    let sgnPositive = evaluateOne "SGN" 7.0
    let piValue = evaluateNoArg "PI"

    Assert.That(expZero, Is.EqualTo(1.0).Within(1e-12))
    Assert.That(lnE, Is.EqualTo(1.0).Within(1e-12))
    Assert.That(logHundred, Is.EqualTo(2.0).Within(1e-12))
    Assert.That(log10Thousand, Is.EqualTo(3.0).Within(1e-12))
    Assert.That(sqrtNine, Is.EqualTo(3.0).Within(1e-12))
    Assert.That(sgnNegative, Is.EqualTo(-1.0).Within(1e-12))
    Assert.That(sgnZero, Is.EqualTo(0.0).Within(1e-12))
    Assert.That(sgnPositive, Is.EqualTo(1.0).Within(1e-12))
    Assert.That(piValue, Is.EqualTo(Math.PI).Within(1e-12))

[<Test>]
let ``interpreter date string family and character functions work`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a", call "ADATE" [ num "10" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b$", call "CHR$" [ num "65" ]) ])
              Line(pos, Some 30, [ Assignment(pos, id "c", call "ASC" [ str "\"AZ\"" ]) ])
              Line(pos, Some 40, [ Assignment(pos, id "d", call "CODE" [ str "\"BC\"" ]) ])
              Line(pos, Some 50, [ Assignment(pos, id "e$", call "DATE$" [ num "1704164645" ]) ])
              Line(pos, Some 60, [ Assignment(pos, id "f$", call "DAY$" [ num "1704164645" ]) ])
              Line(pos, Some 70, [ Assignment(pos, id "g", call "EOF" [ num "1" ]) ])
              Line(pos, Some 80, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b$"; id "c"; id "d"; id "e$"; id "f$"; id "g" ]) ]) ])

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

    Assert.That(String.concat "|" output, Is.EqualTo("1704164655 A 65 66 2024 Jan 02 03:04:05 Tuesday 0"))

[<Test>]
let ``runtime date and eof built ins return expected values`` () =
    let clock () = DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc)

    let evaluate name args eof =
        BuiltInFunctions.evaluate name true clock (fun () -> Random(1234)) (fun _ -> None) (fun () -> None) (fun _ -> 0) eof false args

    let expectInt name args eof (expected: int) =
        match evaluate name args eof with
        | Result.Ok(Numeric(IntNumber number)) -> Assert.That(number, Is.EqualTo(expected))
        | Result.Ok(Numeric(FloatNumber number)) -> Assert.That(number, Is.EqualTo(float expected))
        | Result.Ok other -> Assert.Fail($"Expected numeric result for {name}, got %A{other}")
        | Result.Error err -> Assert.Fail($"Expected {name} to succeed, got %A{err}")

    let expectText name args eof (expected: string) =
        match evaluate name args eof with
        | Result.Ok(Text text) -> Assert.That(text, Is.EqualTo(expected))
        | Result.Ok other -> Assert.Fail($"Expected text result for {name}, got %A{other}")
        | Result.Error err -> Assert.Fail($"Expected {name} to succeed, got %A{err}")

    expectInt "ADATE" [ RuntimeValues.ofInt 10 ] (fun _ -> false) 1704164655
    expectInt "ASC" [ RuntimeValues.ofString "AZ" ] (fun _ -> false) 65
    expectInt "CODE" [ RuntimeValues.ofString "BC" ] (fun _ -> false) 66
    expectText "CHR$" [ RuntimeValues.ofInt 65 ] (fun _ -> false) "A"
    expectText "DATE$" [ RuntimeValues.ofInt 1704164645 ] (fun _ -> false) "2024 Jan 02 03:04:05"
    expectText "DAY$" [ RuntimeValues.ofInt 1704164645 ] (fun _ -> false) "Tuesday"
    expectInt "EOF" [ RuntimeValues.ofInt 5 ] (fun channel -> channel = 5) 1
    expectInt "EOF" [ RuntimeValues.ofInt 6 ] (fun channel -> channel = 5) 0

[<Test>]
let ``interpreter getenv len inkey and keyrow functions work`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a$", call "GETENV$" [ str "\"SB_TEST_ENV\"" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b", call "LEN" [ str "\"abcd\"" ]) ])
              Line(pos, Some 30, [ Assignment(pos, id "c", call "INKEY" [ num "-1" ]) ])
              Line(pos, Some 40, [ Assignment(pos, id "d", call "KEYROW" [ num "3" ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "a$"; id "b"; id "c"; id "d" ]) ]) ])

    let outputs = ResizeArray<string>()
    let keyQueue =
        Collections.Generic.Queue<KeyInfo>(
            [ { KeyCode = 65; Character = Some 'A'; Shift = false; Control = false } ])

    let previous = Environment.GetEnvironmentVariable("SB_TEST_ENV")

    try
        Environment.SetEnvironmentVariable("SB_TEST_ENV", "hello")

        let options =
            { defaultRuntimeOptions with
                Host =
                    DefaultHost.create {
                        ReadLine = fun () -> None
                        ReadKey = fun () -> if keyQueue.Count > 0 then Some(keyQueue.Dequeue()) else None
                        KeyAvailable = fun () -> keyQueue.Count > 0
                        WriteLine = fun line -> outputs.Add(line)
                    } }

        let hir = lowerProgram ast
        let output = runHirProgramWithOptions options hir

        Assert.That(String.concat "|" output, Is.EqualTo("hello 4 65 0"))
    finally
        Environment.SetEnvironmentVariable("SB_TEST_ENV", previous)

[<Test>]
let ``runtime getenv len inkey and keyrow built ins return expected values`` () =
    let evaluate name args =
        BuiltInFunctions.evaluate
            name
            true
            (fun () -> DateTime.UnixEpoch)
            (fun () -> Random(1234))
            (fun variable -> if variable = "SB_TEST_ENV" then Some "hello" else None)
            (fun () -> Some { KeyCode = 65; Character = Some 'A'; Shift = false; Control = false })
            (fun row -> row * 10)
            (fun _ -> false)
            false
            args

    match evaluate "GETENV$" [ RuntimeValues.ofString "SB_TEST_ENV" ] with
    | Result.Ok(Text text) -> Assert.That(text, Is.EqualTo("hello"))
    | other -> Assert.Fail($"Expected GETENV$ to return text, got %A{other}")

    match evaluate "LEN" [ RuntimeValues.ofString "abcd" ] with
    | Result.Ok(Numeric(IntNumber number)) -> Assert.That(number, Is.EqualTo(4))
    | other -> Assert.Fail($"Expected LEN to return int, got %A{other}")

    match evaluate "INKEY" [ RuntimeValues.ofInt -1 ] with
    | Result.Ok(Numeric(IntNumber number)) -> Assert.That(number, Is.EqualTo(65))
    | other -> Assert.Fail($"Expected INKEY to return int, got %A{other}")

    match evaluate "KEYROW" [ RuntimeValues.ofInt 3 ] with
    | Result.Ok(Numeric(IntNumber number)) -> Assert.That(number, Is.EqualTo(30))
    | other -> Assert.Fail($"Expected KEYROW to return int, got %A{other}")

[<Test>]
let ``interpreter time and string helper functions work`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "a", call "TIME" [ num "0" ]) ])
              Line(pos, Some 20, [ Assignment(pos, id "b$", call "FILL$" [ str "\"*\""; num "4" ]) ])
              Line(pos, Some 30, [ Assignment(pos, id "c$", call "MID$" [ str "\"abcdef\""; num "2"; num "3" ]) ])
              Line(pos, Some 40, [ Assignment(pos, id "d$", call "REPL$" [ str "\"abcdef\""; str "\"ZZ\""; num "3"; num "2" ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b$"; id "c$"; id "d$" ]) ]) ])

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

    Assert.That(String.concat "|" output, Is.EqualTo("11045 **** bcd abZZef"))

[<Test>]
let ``runtime time and string helper built ins return expected values`` () =
    let evaluate name args =
        BuiltInFunctions.evaluate
            name
            true
            (fun () -> DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc))
            (fun () -> Random(1234))
            (fun _ -> None)
            (fun () -> None)
            (fun _ -> 0)
            (fun _ -> false)
            false
            args

    match evaluate "TIME" [ RuntimeValues.ofInt 0 ] with
    | Result.Ok(Numeric(FloatNumber number)) -> Assert.That(number, Is.EqualTo(11045.0).Within(1e-12))
    | other -> Assert.Fail($"Expected TIME to return float, got %A{other}")

    match evaluate "FILL$" [ RuntimeValues.ofString "*"; RuntimeValues.ofInt 4 ] with
    | Result.Ok(Text text) -> Assert.That(text, Is.EqualTo("****"))
    | other -> Assert.Fail($"Expected FILL$ to return text, got %A{other}")

    match evaluate "MID$" [ RuntimeValues.ofString "abcdef"; RuntimeValues.ofInt 2; RuntimeValues.ofInt 3 ] with
    | Result.Ok(Text text) -> Assert.That(text, Is.EqualTo("bcd"))
    | other -> Assert.Fail($"Expected MID$ to return text, got %A{other}")

    match evaluate "REPL$" [ RuntimeValues.ofString "abcdef"; RuntimeValues.ofString "ZZ"; RuntimeValues.ofInt 3; RuntimeValues.ofInt 2 ] with
    | Result.Ok(Text text) -> Assert.That(text, Is.EqualTo("abZZef"))
    | other -> Assert.Fail($"Expected REPL$ to return text, got %A{other}")

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


