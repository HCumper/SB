module SBRuntimeTests.InterpreterRuntimeHostTests

open System
open System.Collections.Generic
open System.IO
open NUnit.Framework

open SyntaxAst
open Interpreter
open SBRuntime
open SBRuntimeTests.TestSupport

module H = HIR

let private explicitChannel expr = Some(H.ExplicitChannel expr)

[<Test>]
let ``interpreter restore missing target reports runtime error code`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ DataStmt(pos, [ num "1" ]) ])
              Line(pos, Some 20, [ RestoreStmt(pos, Some(num "999")) ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError InvalidRestoreTarget
    |> ignore

[<Test>]
let ``interpreter missing dynamic local reports runtime error code`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None, None) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "show", []) ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError MissingDynamicStorageCell
    |> ignore

[<Test>]
let ``interpreter peek and poke built-ins use little endian virtual memory`` () =
    let host, screenState = createScreenHost []
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "POKE", [ num "100"; num "18" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "POKE_W", [ num "200"; num "13398" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "POKE_L", [ num "300"; num "305419896" ]) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "PRINT", [ call "PEEK" [ num "100" ]
                                                                 call "PEEK" [ num "200" ]
                                                                 call "PEEK" [ num "201" ]
                                                                 call "PEEK_W" [ num "200" ]
                                                                 call "PEEK_L" [ num "300" ] ]) ]) ])

    let hir = lowerProgram ast
    let output =
        interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir
        |> function
            | Result.Ok result -> result.Output
            | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}"); []

    Assert.That(String.concat "|" output, Is.EqualTo("18 86 52 13398 305419896"))
    Assert.That(screenState.Memory[100], Is.EqualTo(18uy))
    Assert.That(screenState.Memory[200], Is.EqualTo(0x56uy))
    Assert.That(screenState.Memory[201], Is.EqualTo(0x34uy))
    Assert.That(screenState.Memory[300], Is.EqualTo(0x78uy))
    Assert.That(screenState.Memory[301], Is.EqualTo(0x56uy))
    Assert.That(screenState.Memory[302], Is.EqualTo(0x34uy))
    Assert.That(screenState.Memory[303], Is.EqualTo(0x12uy))

[<Test>]
let ``interpreter peek invalid address reports bad parameter runtime error`` () =
    let host, _ = createScreenHost []
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "PRINT", [ call "PEEK" [ num "-1" ] ]) ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir
    |> assertRuntimeError BuiltInUnsupportedArguments
    |> ignore

[<Test>]
let ``default host supports peek and poke built-ins`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "POKE_W", [ num "200"; num "4660" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ call "PEEK" [ num "200" ]
                                                                 call "PEEK" [ num "201" ]
                                                                 call "PEEK_W" [ num "200" ] ]) ]) ])

    let output = runProgram ast
    Assert.That(String.concat "|" output, Is.EqualTo("52 18 4660"))

[<Test>]
let ``interpreter runtime errors prefer BASIC line numbers in messages`` () =
    let posWithBasicLine =
        { pos with
            BasicLineNo = Some 120
            EditorLineNo = 9
            Column = 3 }

    let ast =
        Program(
            posWithBasicLine,
            [ Line(
                posWithBasicLine,
                Some 120,
                [ ProcedureDef(
                    posWithBasicLine,
                    "show",
                    [],
                    [ Line(posWithBasicLine, Some 130, [ ProcedureCall(posWithBasicLine, "PRINT", [ mkIdentifier posWithBasicLine "score" ]) ]) ],
                    None, None) ])
              Line(posWithBasicLine, Some 140, [ ProcedureCall(posWithBasicLine, "show", []) ]) ])

    let hir = lowerProgram ast

    let err =
        interpretProgramWithOptions defaultRuntimeOptions hir
        |> assertRuntimeError MissingDynamicStorageCell

    Assert.That(err.Message, Does.Contain("at BASIC line 120"))

[<Test>]
let ``interpreter when error exposes ernum and erlin and can continue`` () =
    let handlerBody =
        [ Line(pos, Some 11, [ ProcedureCall(pos, "PRINT", [ id "ERNUM"; id "ERLIN" ]) ])
          Line(pos, Some 12, [ ProcedureCall(pos, "CONTINUE", [ num "40" ]) ]) ]
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ WhenStmt(pos, None, handlerBody) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "CLOSE", num "1", []) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "PRINT", [ str "\"ok\"" ]) ]) ])

    let outputs = runProgram ast
    Assert.That(String.concat "|" outputs, Is.EqualTo("-16 20|ok"))

[<Test>]
let ``interpreter when error report writes last error to channel zero`` () =
    let handlerBody =
        [ Line(pos, Some 11, [ ProcedureCall(pos, "REPORT", []) ])
          Line(pos, Some 12, [ ProcedureCall(pos, "CONTINUE", [ num "40" ]) ]) ]
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ WhenStmt(pos, None, handlerBody) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "CLOSE", num "1", []) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "PRINT", [ str "\"done\"" ]) ]) ])

    let outputs = runProgram ast
    Assert.That(outputs[0], Does.Contain("File error (-16) at line 20"))
    Assert.That(outputs[1], Is.EqualTo("done"))

[<Test>]
let ``interpreter report supports explicit channel and explicit error number`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ChannelProcedureCall(pos, "REPORT", num "1", [ num "-16" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"done\"" ]) ]) ])

    let outputs = runProgram ast
    Assert.That(outputs[0], Is.EqualTo("File error (-16)"))
    Assert.That(outputs[1], Is.EqualTo("done"))

[<Test>]
let ``interpreter bare continue resumes at next statement on same line`` () =
    let handlerBody =
        [ Line(pos, Some 11, [ ProcedureCall(pos, "CONTINUE", []) ]) ]
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ WhenStmt(pos, None, handlerBody) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "CLOSE", num "1", [])
                                   ProcedureCall(pos, "PRINT", [ str "\"same-line\"" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ str "\"done\"" ]) ]) ])

    let outputs = runProgram ast
    Assert.That(String.concat "|" outputs, Is.EqualTo("same-line|done"))

[<Test>]
let ``interpreter continue preserves when error for subsequent errors`` () =
    let handlerBody =
        [ Line(pos, Some 11, [ ProcedureCall(pos, "PRINT", [ id "ERR_FE"; id "ERNUM"; id "ERLIN" ]) ])
          Line(pos, Some 12, [ ProcedureCall(pos, "CONTINUE", []) ]) ]
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ WhenStmt(pos, None, handlerBody) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "CLOSE", num "1", [])
                                   ProcedureCall(pos, "PRINT", [ str "\"after-first\"" ]) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "1", [])
                                   ProcedureCall(pos, "PRINT", [ str "\"after-second\"" ]) ]) ])

    let outputs = runProgram ast
    Assert.That(String.concat "|" outputs, Is.EqualTo("1 -16 20|after-first|1 -16 30|after-second"))

[<Test>]
let ``interpreter later when error replaces previous handler`` () =
    let firstHandler =
        [ Line(pos, Some 11, [ ProcedureCall(pos, "PRINT", [ str "\"first\"" ]) ])
          Line(pos, Some 12, [ ProcedureCall(pos, "CONTINUE", [ num "60" ]) ]) ]
    let secondHandler =
        [ Line(pos, Some 21, [ ProcedureCall(pos, "PRINT", [ str "\"second\"" ]) ])
          Line(pos, Some 22, [ ProcedureCall(pos, "CONTINUE", [ num "60" ]) ]) ]
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ WhenStmt(pos, None, firstHandler) ])
              Line(pos, Some 20, [ WhenStmt(pos, None, secondHandler) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "1", []) ])
              Line(pos, Some 60, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

    let outputs = runProgram ast
    Assert.That(String.concat "|" outputs, Is.EqualTo("second|after"))

[<Test>]
let ``interpreter run restarts current program from specified line`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "PRINT", [ str "\"before\"" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "RUN", [ num "40" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ str "\"skipped\"" ]) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

    let outputs = runProgram ast
    Assert.That(String.concat "|" outputs, Is.EqualTo("before|after"))

[<Test>]
let ``interpreter new stops execution after resetting program`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "PRINT", [ str "\"before\"" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "NEW", []) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

    let outputs = runProgram ast
    Assert.That(String.concat "|" outputs, Is.EqualTo("before"))

[<Test>]
let ``interpreter load replaces program without running it`` () =
    let fileId = Guid.NewGuid().ToString("N")
    let tempPath = Path.Combine(Path.GetTempPath(), $"sb-load-{fileId}.bas")

    try
        File.WriteAllText(tempPath, "10 PRINT \"loaded\"" + Environment.NewLine)

        let ast =
            Program(
                pos,
                [ Line(pos, Some 10, [ ProcedureCall(pos, "PRINT", [ str "\"before\"" ]) ])
                  Line(pos, Some 20, [ ProcedureCall(pos, "LOAD", [ str $"\"{tempPath}\"" ]) ])
                  Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

        let outputs = runProgram ast
        Assert.That(String.concat "|" outputs, Is.EqualTo("before"))
    finally
        if File.Exists(tempPath) then
            File.Delete(tempPath)

[<Test>]
let ``interpreter lrun loads and runs replacement program`` () =
    let fileId = Guid.NewGuid().ToString("N")
    let tempPath = Path.Combine(Path.GetTempPath(), $"sb-lrun-{fileId}.bas")

    try
        File.WriteAllText(tempPath, "10 PRINT \"loaded\"" + Environment.NewLine)

        let ast =
            Program(
                pos,
                [ Line(pos, Some 10, [ ProcedureCall(pos, "PRINT", [ str "\"before\"" ]) ])
                  Line(pos, Some 20, [ ProcedureCall(pos, "LRUN", [ str $"\"{tempPath}\"" ]) ])
                  Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

        let outputs = runProgram ast
        Assert.That(String.concat "|" outputs, Is.EqualTo("before|loaded"))
    finally
        if File.Exists(tempPath) then
            File.Delete(tempPath)

[<Test>]
let ``interpreter mrun merges numbered lines and runs merged program`` () =
    let fileId = Guid.NewGuid().ToString("N")
    let tempPath = Path.Combine(Path.GetTempPath(), $"sb-mrun-{fileId}.bas")

    try
        File.WriteAllText(tempPath, "10 PRINT \"new10\"" + Environment.NewLine + "20 PRINT \"new20\"" + Environment.NewLine + "40 PRINT \"new40\"" + Environment.NewLine)

        let ast =
            Program(
                pos,
                [ Line(pos, Some 10, [ ProcedureCall(pos, "PRINT", [ str "\"old10\"" ]) ])
                  Line(pos, Some 20, [ ProcedureCall(pos, "MRUN", [ str $"\"{tempPath}\"" ]) ])
                  Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ str "\"old30\"" ]) ]) ])

        let outputs = runProgram ast
        Assert.That(String.concat "|" outputs, Is.EqualTo("old10|new10|new20|old30|new40"))
    finally
        if File.Exists(tempPath) then
            File.Delete(tempPath)

[<Test>]
let ``interpreter runtime errors fall back to editor line and column in messages`` () =
    let posWithoutBasicLine =
        { pos with
            EditorLineNo = 9
            Column = 3 }

    let ast =
        Program(
            posWithoutBasicLine,
            [ Line(
                posWithoutBasicLine,
                Some 10,
                [ ProcedureDef(
                    posWithoutBasicLine,
                    "bump",
                    [ "a" ],
                    [ Line(posWithoutBasicLine, Some 20, [ ReferenceStmt(posWithoutBasicLine, [ mkIdentifier posWithoutBasicLine "a" ]) ])
                      Line(posWithoutBasicLine, Some 30, [ Assignment(posWithoutBasicLine, mkIdentifier posWithoutBasicLine "a", binary "+" (mkIdentifier posWithoutBasicLine "a") (num "1")) ]) ],
                    None, None) ])
              Line(posWithoutBasicLine, Some 40, [ ProcedureCall(posWithoutBasicLine, "bump", [ num "7" ]) ]) ])

    let hir = lowerProgram ast

    let err =
        interpretProgramWithOptions defaultRuntimeOptions hir
        |> assertRuntimeError InvalidReferenceActual

    Assert.That(err.Message, Does.Contain("at editor line 9, column 3"))

[<Test>]
let ``interpreter reads data and restores by line`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 20, [ DataStmt(pos, [ num "1"; num "2" ]) ])
              Line(pos, Some 30, [ ReadStmt(pos, [ id "a" ]) ])
              Line(pos, Some 40, [ ReadStmt(pos, [ id "b" ]) ])
              Line(pos, Some 50, [ RestoreStmt(pos, Some(num "20")) ])
              Line(pos, Some 60, [ ReadStmt(pos, [ id "c" ]) ])
              Line(pos, Some 70, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b"; id "c" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1 2 1"))

[<Test>]
let ``interpreter reads hex data literals as signed 32 bit integers`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ DataStmt(pos, [ num "$5387"; num "$670A"; num "$3E3C"; num "$270F"; num "$FFFFB287"; num "$6F02" ]) ])
              Line(pos, Some 20, [ ReadStmt(pos, [ id "a"; id "b"; id "c"; id "d"; id "e"; id "f" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "a"; id "b"; id "c"; id "d"; id "e"; id "f" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("21383 26378 15932 9999 -19833 28418"))

[<Test>]
let ``interpreter handles input prompts and targets`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "INPUT", [ str "\"Enter\""; id "n" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ id "n" ]) ]) ])

    let output = runProgramWithInput [ "42" ] ast

    Assert.That(String.concat "|" output, Is.EqualTo("Enter|42"))

[<Test>]
let ``interpreter default input prompt follows default screen cursor state`` () =
    let host, screenState = createScreenHost [ "2" ]
    let levelId = H.SymbolId 0
    let levelStorage = makeStorage levelId 0 "LEVEL" H.HirType.Int H.GlobalStorage
    let hir =
        makeProgram
            (Map.ofList [ levelId, "LEVEL" ])
            [ levelStorage ]
            [ H.BuiltInCall(H.NamedBuiltIn "AT", None, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.HirStmt.Input(None, [ H.Literal(H.ConstString "WHICH LEVEL?", H.HirType.String, pos) ], [ H.WriteVar(levelId, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok execution ->
        Assert.That(String.concat "|" execution.Output, Is.EqualTo("WHICH LEVEL?"))
        Assert.That(screenState.Windows[1].Writes |> Seq.toList |> String.concat "|", Is.EqualTo("WHICH LEVEL?"))
        Assert.That(screenState.Windows[0].Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
        Assert.That(screenState.Windows[1].Cursor, Is.EqualTo((12, 10)))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``default host exposes default channels zero through two`` () =
    let host =
        DefaultHost.create {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let channel0 =
        match host.Channels.Get(ChannelId 0) with
        | Result.Ok channel -> channel
        | Result.Error err -> Assert.Fail($"Expected channel #0, got %A{err}"); Unchecked.defaultof<_>

    let channel1 =
        match host.Channels.Get(ChannelId 1) with
        | Result.Ok channel -> channel
        | Result.Error err -> Assert.Fail($"Expected channel #1, got %A{err}"); Unchecked.defaultof<_>

    let channel2 =
        match host.Channels.Get(ChannelId 2) with
        | Result.Ok channel -> channel
        | Result.Error err -> Assert.Fail($"Expected channel #2, got %A{err}"); Unchecked.defaultof<_>

    Assert.That(channel0.Kind, Is.EqualTo(ChannelKind.ConsoleChannel))
    Assert.That(channel1.Kind, Is.EqualTo(ChannelKind.ScreenChannel))
    Assert.That(channel2.Kind, Is.EqualTo(ChannelKind.ScreenChannel))

[<Test>]
let ``default host exposes ql and extended screen modes`` () =
    let host =
        DefaultHost.create {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let modes = host.Screen.GetSupportedModes()

    Assert.That(modes |> List.exists (fun mode -> mode.Mode = QlMode4), Is.True)
    Assert.That(modes |> List.exists (fun mode -> mode.Mode = QlMode8), Is.True)
    Assert.That(modes |> List.exists (fun mode -> mode.Mode = ExtendedMode 256), Is.True)
    Assert.That(modes |> List.exists (fun mode -> mode.Mode = ExtendedMode 512), Is.True)
    let qlMode4 = modes |> List.find (fun mode -> mode.Mode = QlMode4)
    let extended256 = modes |> List.find (fun mode -> mode.Mode = ExtendedMode 256)
    Assert.That(qlMode4.BaseTextCellWidth, Is.EqualTo(8))
    Assert.That(qlMode4.BaseTextCellHeight, Is.EqualTo(10))
    Assert.That(qlMode4.DefaultCharacterSize, Is.EqualTo((0, 0)))
    Assert.That(extended256.BaseTextCellWidth, Is.EqualTo(8))
    Assert.That(extended256.BaseTextCellHeight, Is.EqualTo(8))
    Assert.That(extended256.DefaultCharacterSize, Is.EqualTo((0, 0)))

[<Test>]
let ``interpreter print to screen channels does not mirror into console output`` () =
    let outputs = ResizeArray<string>()
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.Print, explicitChannel(H.Literal(H.ConstInt 1, H.HirType.Int, pos)), [ H.Literal(H.ConstString "hello", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.Print, explicitChannel(H.Literal(H.ConstInt 2, H.HirType.Int, pos)), [ H.Literal(H.ConstString "world", H.HirType.String, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host =
                    DefaultHost.create {
                        ReadLine = fun () -> None
                        ReadScreenLine = fun _ -> None
                        FlushInput = fun () -> ()
                        ReadKey = fun () -> None
                        KeyAvailable = fun () -> false
                        KeyRowState = fun _ -> 0
                        WriteLine = fun line -> outputs.Add(line)
                    } }
            hir

    match result with
    | Result.Ok execution ->
        Assert.That(String.concat "|" execution.Output, Is.EqualTo("hello|world"))
        Assert.That(String.concat "|" outputs, Is.EqualTo(""))
    | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter input from default channel uses host channel manager`` () =
    let xId = H.SymbolId 0
    let xStorage = makeStorage xId 0 "X" H.HirType.Int H.GlobalStorage
    let outputs = ResizeArray<string>()
    let inputs = Queue<string>([ "42" ])
    let channelExpr = H.Literal(H.ConstInt 2, H.HirType.Int, pos)
    let promptExpr = H.Literal(H.ConstString "Enter", H.HirType.String, pos)
    let target = H.WriteVar(xId, H.HirType.Int, pos)
    let hir =
        makeProgram
            (Map.ofList [ xId, "X" ])
            [ xStorage ]
            [ H.HirStmt.Input(explicitChannel channelExpr, [ promptExpr ], [ target ], pos)
              H.BuiltInCall(H.Print, None, [ H.ReadVar(xId, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host =
                    DefaultHost.create {
                        ReadLine = fun () -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                        ReadScreenLine = fun _ -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                        FlushInput = fun () -> ()
                        ReadKey = fun () -> None
                        KeyAvailable = fun () -> false
                        KeyRowState = fun _ -> 0
                        WriteLine = fun line -> outputs.Add(line)
                    } }
            hir

    match result with
    | Result.Ok execution -> Assert.That(String.concat "|" execution.Output, Is.EqualTo("Enter|42"))
    | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter input preserves dynamic string targets`` () =
    let outputs = ResizeArray<string>()
    let inputs = Queue<string>([ "ADD" ])
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "INPUT", [ str "\"Enter\""; id "a$" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ id "a$" ]) ]) ])
    let hir = lowerProgram ast

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host =
                    DefaultHost.create {
                        ReadLine = fun () -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                        ReadScreenLine = fun _ -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                        FlushInput = fun () -> ()
                        ReadKey = fun () -> None
                        KeyAvailable = fun () -> false
                        KeyRowState = fun _ -> 0
                        WriteLine = fun line -> outputs.Add(line)
                    } }
            hir

    match result with
    | Result.Ok execution -> Assert.That(String.concat "|" execution.Output, Is.EqualTo("Enter|ADD"))
    | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter screen print formats floats compactly`` () =
    let host, screenState = createScreenHost []
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.Print, explicitChannel channel1, [ H.Literal(H.ConstFloat 23.82, H.HirType.Float, pos) ], pos) ]

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Windows[1].Writes |> Seq.toList |> String.concat "", Is.EqualTo("23.82"))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter screen channel operations keep default window state independent`` () =
    let host, screenState = createScreenHost [ "99" ]
    let xId = H.SymbolId 0
    let xStorage = makeStorage xId 0 "X" H.HirType.Int H.GlobalStorage
    let channel0 = H.Literal(H.ConstInt 0, H.HirType.Int, pos)
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let channel2 = H.Literal(H.ConstInt 2, H.HirType.Int, pos)
    let hir =
        makeProgram
            (Map.ofList [ xId, "X" ])
            [ xStorage ]
            [ H.BuiltInCall(H.NamedBuiltIn "CLS", explicitChannel channel0, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "WINDOW", explicitChannel channel0, [ H.Literal(H.ConstInt 512, H.HirType.Int, pos); H.Literal(H.ConstInt 256, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "AT", explicitChannel channel0, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WINDOW", explicitChannel channel1, [ H.Literal(H.ConstInt 448, H.HirType.Int, pos); H.Literal(H.ConstInt 40, H.HirType.Int, pos); H.Literal(H.ConstInt 32, H.HirType.Int, pos); H.Literal(H.ConstInt 216, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CURSOR", explicitChannel channel1, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CSIZE", explicitChannel channel1, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "INK", explicitChannel channel1, [ H.Literal(H.ConstInt 7, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WINDOW", explicitChannel channel2, [ H.Literal(H.ConstInt 300, H.HirType.Int, pos); H.Literal(H.ConstInt 120, H.HirType.Int, pos); H.Literal(H.ConstInt 100, H.HirType.Int, pos); H.Literal(H.ConstInt 80, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CSIZE", explicitChannel channel2, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", explicitChannel channel2, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "BORDER", explicitChannel channel2, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.HirStmt.Input(explicitChannel channel2, [ H.Literal(H.ConstString "Enter", H.HirType.String, pos) ], [ H.WriteVar(xId, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok execution ->
        Assert.That(String.concat "|" execution.Output, Is.EqualTo("Enter"))
        let window0 = screenState.Windows[0]
        let window1 = screenState.Windows[1]
        let window2 = screenState.Windows[2]
        Assert.That(window0.ClearCount, Is.EqualTo(1))
        Assert.That(window0.Window, Is.EqualTo((512, 256, 0, 0)))
        Assert.That(window0.Cursor, Is.EqualTo((4, 3)))
        Assert.That(window0.CharacterSize, Is.EqualTo((0, 0)))
        Assert.That(window0.Ink, Is.EqualTo(Some [ 7 ]))
        Assert.That(window1.Window, Is.EqualTo((448, 40, 32, 216)))
        Assert.That(window1.Cursor, Is.EqualTo((10, 20)))
        Assert.That(window1.CharacterSize, Is.EqualTo((2, 1)))
        Assert.That(window1.Ink, Is.EqualTo(Some [ 7 ]))
        Assert.That(window1.Paper, Is.EqualTo(Some 0))
        Assert.That(window2.Window, Is.EqualTo((296, 118, 102, 81)))
        Assert.That(window2.CharacterSize, Is.EqualTo((4, 3)))
        Assert.That(window2.Paper, Is.EqualTo(Some 2))
        Assert.That(window2.BorderSize, Is.EqualTo(Some 1))
        Assert.That(window2.Writes |> Seq.toList |> String.concat "|", Is.EqualTo("Enter"))
        Assert.That(window0.Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
        Assert.That(window1.Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter screen text writes and cls update backing text buffer`` () =
    let host, screenState = createScreenHost []
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "WINDOW", explicitChannel channel1, [ H.Literal(H.ConstInt 20, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos); H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "AT", explicitChannel channel1, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.Print, explicitChannel channel1, [ H.Literal(H.ConstString "HELLO", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLS", explicitChannel channel1, [], pos) ]

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.TextBuffer[4, 12] = ' ', Is.True)
        Assert.That(screenState.Windows[1].ClearCount, Is.EqualTo(1))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter display controls update per-window state`` () =
    let host, screenState = createScreenHost []
    let channel2 = H.Literal(H.ConstInt 2, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "SCROLL", explicitChannel channel2, [ H.Literal(H.ConstInt 12, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WIDTH", explicitChannel channel2, [ H.Literal(H.ConstInt 80, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAN", explicitChannel channel2, [ H.Literal(H.ConstInt -150, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "RECOL", explicitChannel channel2, [ H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PALETTE", explicitChannel channel2, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "SCROLL", None, [ H.Literal(H.ConstInt -7, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WIDTH", None, [ H.Literal(H.ConstInt 64, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAN", None, [ H.Literal(H.ConstInt 150, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "RECOL", None, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PALETTE", None, [ H.Literal(H.ConstInt 8, H.HirType.Int, pos); H.Literal(H.ConstInt 6, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        let window2 = screenState.Windows[2]
        let window1 = screenState.Windows[1]
        let window0 = screenState.Windows[0]
        Assert.That(window2.Scroll, Is.EqualTo(12))
        Assert.That(window2.Width, Is.EqualTo(Some 80))
        Assert.That(window2.Pan, Is.EqualTo(-150))
        Assert.That(window2.Recolor, Is.EqualTo(Some [ 5 ]))
        Assert.That(window2.Palette, Is.EqualTo(Some [ 1; 3; 5 ]))
        Assert.That(window1.Scroll, Is.EqualTo(-7))
        Assert.That(window1.Width, Is.EqualTo(Some 64))
        Assert.That(window1.Pan, Is.EqualTo(150))
        Assert.That(window1.Recolor, Is.EqualTo(Some [ 2 ]))
        Assert.That(window1.Palette, Is.EqualTo(Some [ 8; 6 ]))
        Assert.That(window0.Scroll, Is.EqualTo(0))
        Assert.That(window0.Width, Is.EqualTo(None))
        Assert.That(window0.Pan, Is.EqualTo(0))
        Assert.That(window0.Recolor, Is.EqualTo(None))
        Assert.That(window0.Palette, Is.EqualTo(None))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter char use updates per-channel and default screen fonts`` () =
    let host, screenState = createScreenHost []
    let channel2 = H.Literal(H.ConstInt 2, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "CHAR_USE", explicitChannel channel2, [ H.Literal(H.ConstInt 1234, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CHAR_USE", explicitChannel channel2, [ H.Literal(H.ConstInt -1, H.HirType.Int, pos); H.Literal(H.ConstInt 5678, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "S_FONT", None, [ H.Literal(H.ConstInt 111, H.HirType.Int, pos); H.Literal(H.ConstInt 222, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Windows[2].CharacterFonts, Is.EqualTo((1234, 5678)))
        Assert.That(screenState.Windows[1].CharacterFonts, Is.EqualTo((111, 222)))
        Assert.That(screenState.Windows[0].CharacterFonts, Is.EqualTo((0, 0)))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter mode changes host screen mode`` () =
    let host, screenState = createScreenHost []
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "MODE", None, [ H.Literal(H.ConstInt 8, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "MODE", None, [ H.Literal(H.ConstInt 512, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Mode.Mode, Is.EqualTo(ExtendedMode 512))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter mode resets default screen geometry and default windows`` () =
    let host, screenState = createScreenHost []
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(
                  H.NamedBuiltIn "WINDOW",
                  None,
                  [ H.Literal(H.ConstInt 480, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 200, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 20, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 10, H.HirType.Int, pos) ],
                  pos)
              H.BuiltInCall(H.NamedBuiltIn "CSIZE", explicitChannel channel1, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CURSOR", explicitChannel channel1, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "MODE", None, [ H.Literal(H.ConstInt 8, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Mode.Mode, Is.EqualTo(QlMode8))
        Assert.That(screenState.Windows[1].Window, Is.EqualTo((256, 256, 0, 0)))
        Assert.That(screenState.Windows[1].CharacterSize, Is.EqualTo((0, 0)))
        Assert.That(screenState.Windows[1].Cursor, Is.EqualTo((0, 0)))
        Assert.That(screenState.Windows[1].Width, Is.EqualTo(None))
        Assert.That(screenState.Windows[1].Pan, Is.EqualTo(0))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter window updates default screen geometry`` () =
    let host, screenState = createScreenHost []
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(
                  H.NamedBuiltIn "WINDOW",
                  None,
                  [ H.Literal(H.ConstInt 480, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 255, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 20, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 0, H.HirType.Int, pos) ],
                  pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Windows[1].Window, Is.EqualTo((480, 255, 20, 0)))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter graphics primitives dispatch to host graphics device`` () =
    let host, screenState = createScreenHost []
    let channel0 = H.Literal(H.ConstInt 0, H.HirType.Int, pos)
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let channel2 = H.Literal(H.ConstInt 2, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "INK", None, [ H.Literal(H.ConstInt 6, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "SCALE", None, [ H.Literal(H.ConstInt 100, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PLOT", None, [ H.Literal(H.ConstFloat 1.5, H.HirType.Float, pos); H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "DRAW", explicitChannel channel1, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstFloat 4.5, H.HirType.Float, pos) ], pos)
              H.BuiltInCall(
                  H.NamedBuiltIn "LINE",
                  explicitChannel channel2,
                  [ H.Literal(H.ConstInt 0, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 0, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 10, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 10, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 20, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 5, H.HirType.Int, pos) ],
                  pos)
              H.BuiltInCall(H.NamedBuiltIn "CIRCLE", None, [ H.Literal(H.ConstInt 8, H.HirType.Int, pos); H.Literal(H.ConstInt 9, H.HirType.Int, pos); H.Literal(H.ConstFloat 2.5, H.HirType.Float, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CIRCLE", None, [ H.Literal(H.ConstInt 100, H.HirType.Int, pos); H.Literal(H.ConstInt 100, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos); H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 50, H.HirType.Int, pos); H.Literal(H.ConstInt 50, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "ELLIPSE", explicitChannel channel2, [ H.Literal(H.ConstInt 11, H.HirType.Int, pos); H.Literal(H.ConstInt 12, H.HirType.Int, pos); H.Literal(H.ConstInt 13, H.HirType.Int, pos); H.Literal(H.ConstFloat 0.7, H.HirType.Float, pos); H.Literal(H.ConstInt 6, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(
                  H.NamedBuiltIn "ARC",
                  None,
                  [ H.Literal(H.ConstInt 14, H.HirType.Int, pos)
                    H.Binary(H.SliceRange, H.Literal(H.ConstInt 15, H.HirType.Int, pos), H.Literal(H.ConstInt 16, H.HirType.Int, pos), H.HirType.Int, pos)
                    H.Literal(H.ConstInt 17, H.HirType.Int, pos)
                    H.Binary(H.SliceRange, H.Literal(H.ConstFloat 3.0, H.HirType.Float, pos), H.Literal(H.ConstInt 20, H.HirType.Int, pos), H.HirType.Float, pos)
                    H.Literal(H.ConstInt 21, H.HirType.Int, pos)
                    H.Literal(H.ConstFloat 1.5, H.HirType.Float, pos) ],
                  pos)
              H.BuiltInCall(H.NamedBuiltIn "FILL", explicitChannel channel0, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        let expectedOps : string list =
            [ "INK 6,4,3"
              "SCALE 100,0,0"
              "PLOT 1.5,2"
              "DRAW 3,4.5"
              "LINE 0,0 TO 10,10"
              "LINE 10,10 TO 20,5"
              "CIRCLE 8,9,2.5"
              "ELLIPSE 100,100,20,1,0"
              "CIRCLE 50,50,20"
              "ELLIPSE 11,12,13,0.69999999999999996,6"
              "ARC 14,15 TO 16,17,3"
              "ARC TO 20,21,1.5"
              "FILL 1" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
        Assert.That(screenState.GraphicsInk = [ 6; 4; 3 ], Is.True)
        Assert.That(screenState.Scale, Is.EqualTo((100.0, 0.0, 0.0)))
        Assert.That(screenState.FillMode, Is.EqualTo(1))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter channeled ink preserves all supplied values`` () =
    let host, screenState = createScreenHost []
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "INK", explicitChannel channel1, [ H.Literal(H.ConstInt 6, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Windows[1].Ink, Is.EqualTo(Some [ 6; 4; 3 ]))
        Assert.That(screenState.GraphicsInk = [ 6; 4; 3 ], Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter drawing mode controls update graphics state`` () =
    let host, screenState = createScreenHost []
    let channel0 = H.Literal(H.ConstInt 0, H.HirType.Int, pos)
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "OVER", explicitChannel channel0, [ H.Literal(H.ConstInt -1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "UNDER", None, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "FLASH", explicitChannel channel1, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PENUP", None, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "PENDOWN", explicitChannel channel1, [], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        let expectedOps : string list =
            [ "OVER -1"
              "UNDER 1"
              "FLASH 3"
              "PENUP"
              "PENDOWN" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
        Assert.That(screenState.OverMode, Is.EqualTo(-1))
        Assert.That(screenState.UnderMode, Is.EqualTo(1))
        Assert.That(screenState.FlashMode, Is.EqualTo(3))
        Assert.That(screenState.PenDown, Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter remaining graphics primitives dispatch to host graphics device`` () =
    let host, screenState = createScreenHost []
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "CLEAR", None, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "POINT", None, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "POINT_R", explicitChannel channel1, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt -2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "DLINE", None, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "LINE_R", explicitChannel channel1, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 6, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CIRCLE_R", None, [ H.Literal(H.ConstInt 7, H.HirType.Int, pos); H.Literal(H.ConstInt 8, H.HirType.Int, pos); H.Literal(H.ConstInt 9, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CIRCLE_R", None, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos); H.Literal(H.ConstInt 6, H.HirType.Int, pos); H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "ELLIPSE_R", explicitChannel channel1, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 11, H.HirType.Int, pos); H.Literal(H.ConstInt 12, H.HirType.Int, pos); H.Literal(H.ConstFloat 0.5, H.HirType.Float, pos); H.Literal(H.ConstInt 13, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(
                  H.NamedBuiltIn "ARC",
                  None,
                  [ H.Literal(H.ConstInt 14, H.HirType.Int, pos)
                    H.Binary(H.SliceRange, H.Literal(H.ConstInt 15, H.HirType.Int, pos), H.Literal(H.ConstInt 16, H.HirType.Int, pos), H.HirType.Int, pos)
                    H.Literal(H.ConstInt 17, H.HirType.Int, pos)
                    H.Literal(H.ConstFloat 3.0, H.HirType.Float, pos) ],
                  pos)
              H.BuiltInCall(
                  H.NamedBuiltIn "ARC_R",
                  explicitChannel channel1,
                  [ H.Literal(H.ConstInt 19, H.HirType.Int, pos)
                    H.Binary(H.SliceRange, H.Literal(H.ConstInt 20, H.HirType.Int, pos), H.Literal(H.ConstInt 21, H.HirType.Int, pos), H.HirType.Int, pos)
                    H.Literal(H.ConstInt 22, H.HirType.Int, pos)
                    H.Binary(H.SliceRange, H.Literal(H.ConstFloat 2.5, H.HirType.Float, pos), H.Literal(H.ConstInt 24, H.HirType.Int, pos), H.HirType.Float, pos)
                    H.Literal(H.ConstInt 25, H.HirType.Int, pos)
                    H.Literal(H.ConstFloat 1.25, H.HirType.Float, pos) ],
                  pos)
              H.BuiltInCall(H.NamedBuiltIn "BLOCK", None, [ H.Literal(H.ConstInt 24, H.HirType.Int, pos); H.Literal(H.ConstInt 25, H.HirType.Int, pos); H.Literal(H.ConstInt 26, H.HirType.Int, pos); H.Literal(H.ConstInt 27, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "TURN", None, [ H.Literal(H.ConstInt 30, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "TURNTO", explicitChannel channel1, [ H.Literal(H.ConstInt 45, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        let expectedOps : string list =
            [ "CLEAR"
              "POINT 4,5"
              "POINT_R 1,-2"
              "DLINE 0,0,5,5"
              "LINE_R 2,3,4,6"
              "CIRCLE_R 7,8,9"
              "ELLIPSE_R 4,5,6,1,0"
              "CIRCLE_R 2,3,4"
              "ELLIPSE_R 10,11,12,0.5,13"
              "ARC 14,15 TO 16,17,3"
              "ARC_R 19,20 TO 21,22,2.5"
              "ARC_R TO 24,25,1.25"
              "BLOCK 24,25,26,27,3"
              "TURN 30"
              "TURNTO 45" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
        Assert.That(screenState.Heading, Is.EqualTo(45.0))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``parser preserves leading TO arc syntax as arc arguments`` () =
    let host, screenState = createScreenHost []
    let ast = parseAstFromSource "100 ARC TO 10,20,1.5\n110 ARC_R TO 5,6,0.5\n"
    let hir = lowerProgram ast

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        let expectedOps : string list =
            [ "ARC TO 10,20,1.5"
              "ARC_R TO 5,6,0.5" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``parser lowers explicit TO arc syntax into endpoint arc arguments`` () =
    let host, screenState = createScreenHost []
    let ast = parseAstFromSource "100 ARC 50,35 TO 50,25,-1.5\n110 ARC_R 10,5 TO 12,2,-1\n"
    let hir = lowerProgram ast

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        let expectedOps : string list =
            [ "ARC 50,35 TO 50,25,-1.5"
              "ARC_R 10,5 TO 12,2,-1" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter minimum arity graphics and display primitives dispatch correctly`` () =
    let host, screenState = createScreenHost []
    let channel0 = H.Literal(H.ConstInt 0, H.HirType.Int, pos)
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "INK", explicitChannel channel0, [ H.Literal(H.ConstInt 7, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", explicitChannel channel0, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "BORDER", explicitChannel channel0, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "SCROLL", explicitChannel channel1, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WIDTH", explicitChannel channel1, [ H.Literal(H.ConstInt 40, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAN", explicitChannel channel1, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "RECOL", explicitChannel channel1, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PALETTE", explicitChannel channel1, [ H.Literal(H.ConstInt 9, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "LINE", None, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "LINE_R", None, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "DLINE", None, [ H.Literal(H.ConstInt 6, H.HirType.Int, pos); H.Literal(H.ConstInt 7, H.HirType.Int, pos); H.Literal(H.ConstInt 8, H.HirType.Int, pos); H.Literal(H.ConstInt 9, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Windows[0].Ink, Is.EqualTo(Some [ 7 ]))
        Assert.That(screenState.Windows[0].Paper, Is.EqualTo(Some 2))
        Assert.That(screenState.Windows[0].BorderSize, Is.EqualTo(Some 1))
        Assert.That(screenState.Windows[1].Scroll, Is.EqualTo(3))
        Assert.That(screenState.Windows[1].Width, Is.EqualTo(Some 40))
        Assert.That(screenState.Windows[1].Pan, Is.EqualTo(10))
        Assert.That(screenState.Windows[1].Recolor, Is.EqualTo(Some [ 4 ]))
        Assert.That(screenState.Windows[1].Palette, Is.EqualTo(Some [ 9 ]))
        let expectedOps =
            [ "INK 7"
              "LINE 10,1 TO 12,3"
              "LINE_R 4,5"
              "DLINE 16,7,18,9" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter strip dispatches separately from paper and paper resets strip`` () =
    let host, screenState = createScreenHost []
    let channel0 = H.Literal(H.ConstInt 0, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "PAPER", explicitChannel channel0, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "STRIP", explicitChannel channel0, [ H.Literal(H.ConstInt 7, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", explicitChannel channel0, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with Host = host }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Windows[0].Paper, Is.EqualTo(Some 4))
        Assert.That(screenState.Windows[0].Strip, Is.EqualTo(Some [ 4 ]))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter extended arity graphics and display primitives dispatch correctly`` () =
    let host, screenState = createScreenHost []
    let channel0 = H.Literal(H.ConstInt 0, H.HirType.Int, pos)
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "BORDER", explicitChannel channel0, [ H.Literal(H.ConstInt 5, H.HirType.Int, pos); H.Literal(H.ConstInt 8, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PALETTE", explicitChannel channel1, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "LINE", None, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "LINE_R", None, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 6, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "DLINE", None, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos); H.Literal(H.ConstInt 9, H.HirType.Int, pos); H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host = host }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Windows[0].BorderSize, Is.EqualTo(Some 5))
        Assert.That(screenState.Windows[1].Palette, Is.EqualTo(Some [ 1; 2; 3 ]))
        let expectedOps =
            [ "LINE 0,0 TO 10,10"
              "LINE 10,10 TO 20,5"
              "LINE_R 2,3,4,6"
              "DLINE 0,0,5,5,9,1" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter unsupported mode reports runtime error`` () =
    let host, _ = createScreenHost []
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "MODE", None, [ H.Literal(H.ConstInt 1024, H.HirType.Int, pos) ], pos) ]

    interpretProgramWithOptions
        { defaultRuntimeOptions with
            Host = host }
        hir
    |> assertRuntimeError BuiltInStatementNotImplemented
    |> ignore

[<Test>]
let ``interpreter pause uses runtime sleeper`` () =
    let pauses = ResizeArray<int>()
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "PAUSE", None, [ H.Literal(H.ConstInt 50, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAUSE", None, [ H.Literal(H.ConstInt -3, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Sleeper = fun milliseconds -> pauses.Add(milliseconds) }
            hir

    match result with
    | Result.Ok _ ->
        Assert.That(pauses |> Seq.toList = [ 50; 0 ], Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter open_new open_in and close handle file channels`` () =
    let tempPath = Path.Combine(Path.GetTempPath(), $"sb-runtime-{Guid.NewGuid():N}.txt")
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ChannelProcedureCall(pos, "OPEN_NEW", num "9", [ str $"\"{tempPath}\"" ]) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"hello\"" ]) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 40, [ ChannelProcedureCall(pos, "OPEN_IN", num "9", [ str $"\"{tempPath}\"" ]) ])
              Line(pos, Some 50, [ ChannelProcedureCall(pos, "INPUT", num "9", [ id "text$" ]) ])
              Line(pos, Some 60, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 70, [ ProcedureCall(pos, "PRINT", [ id "text$" ]) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    try
        let hir = lowerProgram ast
        let result = interpretProgramWithOptions options hir

        match result with
        | Result.Ok execution ->
            Assert.That(String.concat "|" execution.Output, Is.EqualTo("hello|hello"))
            Assert.That(File.ReadAllText(tempPath), Does.Contain("hello"))
        | Result.Error err ->
            Assert.Fail($"Expected interpretation to succeed, got %A{err}")
    finally
        if File.Exists(tempPath) then
            File.Delete(tempPath)

[<Test>]
let ``interpreter print supports implicit channel to file`` () =
    let tempPath = Path.Combine(Path.GetTempPath(), $"sb-runtime-implicit-{Guid.NewGuid():N}.txt")
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ImplicitChannelProcedureCall(pos, "PRINT", str $"\"{tempPath}\"", [ str "\"hello\"" ]) ]) ])

    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = ignore
                } }

    try
        let hir = lowerProgram ast
        let result = interpretProgramWithOptions options hir

        match result with
        | Result.Ok _ ->
            Assert.That(File.Exists(tempPath), Is.True)
            Assert.That(File.ReadAllText(tempPath), Does.Contain("hello"))
        | Result.Error err ->
            Assert.Fail($"Expected interpretation to succeed, got %A{err}")
    finally
        if File.Exists(tempPath) then
            File.Delete(tempPath)

[<Test>]
let ``interpreter append adds to existing file channels`` () =
    let tempPath = Path.Combine(Path.GetTempPath(), $"sb-runtime-append-{Guid.NewGuid():N}.txt")
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ChannelProcedureCall(pos, "OPEN_NEW", num "9", [ str $"\"{tempPath}\"" ]) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"hello\"" ]) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 40, [ ChannelProcedureCall(pos, "APPEND", num "9", [ str $"\"{tempPath}\"" ]) ])
              Line(pos, Some 50, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"again\"" ]) ])
              Line(pos, Some 60, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 70, [ ChannelProcedureCall(pos, "OPEN_IN", num "9", [ str $"\"{tempPath}\"" ]) ])
              Line(pos, Some 80, [ ChannelProcedureCall(pos, "INPUT", num "9", [ id "first$" ]) ])
              Line(pos, Some 90, [ ChannelProcedureCall(pos, "INPUT", num "9", [ id "second$" ]) ])
              Line(pos, Some 100, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ id "first$"; id "second$" ]) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    try
        let hir = lowerProgram ast
        let result = interpretProgramWithOptions options hir

        match result with
        | Result.Ok execution ->
            Assert.That(execution.Output |> Seq.last, Is.EqualTo("hello again"))
            let lines = File.ReadAllLines(tempPath)
            Assert.That(lines.Length, Is.EqualTo(2))
            Assert.That(lines[0], Is.EqualTo("hello"))
            Assert.That(lines[1], Is.EqualTo("again"))
        | Result.Error err ->
            Assert.Fail($"Expected interpretation to succeed, got %A{err}")
    finally
        if File.Exists(tempPath) then
            File.Delete(tempPath)

[<Test>]
let ``interpreter open_new and open_in support directory backed devices`` () =
    let deviceRoot = Path.Combine(Environment.CurrentDirectory, "RuntimeDevices", "ram1")
    let devicePath = Path.Combine(deviceRoot, "demo_io")
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ChannelProcedureCall(pos, "OPEN_NEW", num "9", [ str "\"ram1_demo_io\"" ]) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"hello\"" ]) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 40, [ ChannelProcedureCall(pos, "OPEN_IN", num "9", [ str "\"ram1_demo_io\"" ]) ])
              Line(pos, Some 50, [ ChannelProcedureCall(pos, "INPUT", num "9", [ id "text$" ]) ])
              Line(pos, Some 60, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 70, [ ProcedureCall(pos, "PRINT", [ id "text$" ]) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    try
        if File.Exists(devicePath) then
            File.Delete(devicePath)

        let hir = lowerProgram ast
        let result = interpretProgramWithOptions options hir

        match result with
        | Result.Ok execution ->
            Assert.That(execution.Output |> Seq.last, Is.EqualTo("hello"))
            Assert.That(File.Exists(devicePath), Is.True)
        | Result.Error err ->
            Assert.Fail($"Expected interpretation to succeed, got %A{err}")
    finally
        if File.Exists(devicePath) then
            File.Delete(devicePath)

[<Test>]
let ``interpreter append supports directory backed devices`` () =
    let deviceRoot = Path.Combine(Environment.CurrentDirectory, "RuntimeDevices", "ram1")
    let devicePath = Path.Combine(deviceRoot, "demo_append")
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ChannelProcedureCall(pos, "OPEN_NEW", num "9", [ str "\"ram1_demo_append\"" ]) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"hello\"" ]) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 40, [ ChannelProcedureCall(pos, "APPEND", num "9", [ str "\"ram1_demo_append\"" ]) ])
              Line(pos, Some 50, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"again\"" ]) ])
              Line(pos, Some 60, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 70, [ ChannelProcedureCall(pos, "OPEN_IN", num "9", [ str "\"ram1_demo_append\"" ]) ])
              Line(pos, Some 80, [ ChannelProcedureCall(pos, "INPUT", num "9", [ id "first$" ]) ])
              Line(pos, Some 90, [ ChannelProcedureCall(pos, "INPUT", num "9", [ id "second$" ]) ])
              Line(pos, Some 100, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ id "first$"; id "second$" ]) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    try
        if File.Exists(devicePath) then
            File.Delete(devicePath)

        let hir = lowerProgram ast
        let result = interpretProgramWithOptions options hir

        match result with
        | Result.Ok execution ->
            Assert.That(execution.Output |> Seq.last, Is.EqualTo("hello again"))
            let lines = File.ReadAllLines(devicePath)
            Assert.That(lines.Length, Is.EqualTo(2))
            Assert.That(lines[0], Is.EqualTo("hello"))
            Assert.That(lines[1], Is.EqualTo("again"))
        | Result.Error err ->
            Assert.Fail($"Expected interpretation to succeed, got %A{err}")
    finally
        if File.Exists(devicePath) then
            File.Delete(devicePath)

[<Test>]
let ``interpreter open_new supports printer device`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ChannelProcedureCall(pos, "OPEN_NEW", num "9", [ str "\"prt_\"" ]) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"printer\"" ]) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    let hir = lowerProgram ast
    let result = interpretProgramWithOptions options hir

    match result with
    | Result.Ok execution ->
        Assert.That(String.concat "|" execution.Output, Is.EqualTo("printer"))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter eof reflects file channel read position`` () =
    let tempPath = Path.Combine(Path.GetTempPath(), $"sb-runtime-eof-{Guid.NewGuid():N}.txt")
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ChannelProcedureCall(pos, "OPEN_NEW", num "9", [ str $"\"{tempPath}\"" ]) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"hello\"" ]) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 40, [ ChannelProcedureCall(pos, "OPEN_IN", num "9", [ str $"\"{tempPath}\"" ]) ])
              Line(pos, Some 50, [ Assignment(pos, id "before", call "EOF" [ num "9" ]) ])
              Line(pos, Some 60, [ ChannelProcedureCall(pos, "INPUT", num "9", [ id "text$" ]) ])
              Line(pos, Some 70, [ Assignment(pos, id "after", call "EOF" [ num "9" ]) ])
              Line(pos, Some 80, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ])
              Line(pos, Some 90, [ ProcedureCall(pos, "PRINT", [ id "before"; id "text$"; id "after" ]) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    try
        let hir = lowerProgram ast
        let result = interpretProgramWithOptions options hir

        match result with
        | Result.Ok execution ->
            Assert.That(execution.Output |> Seq.last, Is.EqualTo("0 hello 1"))
        | Result.Error err ->
            Assert.Fail($"Expected interpretation to succeed, got %A{err}")
    finally
        if File.Exists(tempPath) then
            File.Delete(tempPath)

[<Test>]
let ``interpreter open and close support dynamic screen channels`` () =
    let channel9 = H.Literal(H.ConstInt 9, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel channel9, [ H.Literal(H.ConstString "scr_", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.Print, explicitChannel channel9, [ H.Literal(H.ConstString "hello", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLOSE", explicitChannel channel9, [], pos) ]

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    let result = interpretProgramWithOptions options hir

    match result with
    | Result.Ok execution ->
        Assert.That(String.concat "|" execution.Output, Is.EqualTo("hello"))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter open parses screen device geometry strings`` () =
    let channel9 = H.Literal(H.ConstInt 9, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel channel9, [ H.Literal(H.ConstString "scr_448x40a32x216", H.HirType.String, pos) ], pos) ]

    let host =
        DefaultHost.create {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ ->
        match host.Channels.Get(ChannelId 9) with
        | Result.Ok (:? IScreenChannel as screenChannel) ->
            Assert.That(screenChannel.Kind, Is.EqualTo(ChannelKind.ScreenChannel))
            Assert.That(screenChannel.GetWindow(), Is.EqualTo((448, 40, 32, 216)))
        | Result.Ok _ -> Assert.Fail("Expected channel #9 to be a screen channel.")
        | Result.Error err -> Assert.Fail($"Expected channel #9 to exist, got %A{err}")
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``default host display includes dynamically opened screen panes`` () =
    let channel5 = H.Literal(H.ConstInt 5, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel channel5, [ H.Literal(H.ConstString "scr_512x180a0x75", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", explicitChannel channel5, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLS", explicitChannel channel5, [], pos) ]

    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ ->
        let pane = display.GetSnapshot().Panes |> List.tryFind (fun item -> item.ChannelId = Some 5)
        match pane with
        | Some pane ->
            Assert.That(pane.Window, Is.EqualTo((512, 180, 0, 75)))
            Assert.That(pane.Paper, Is.EqualTo(4))
        | None ->
            Assert.Fail("Expected dynamically opened screen channel #5 to appear in the display snapshot.")
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``default host overlapping pane surface shows shared screen text`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as channel1) ->
        channel1.SetWindow(16, 20, 0, 0)
        channel1.SetCharacterSize(0, 0)
        channel1.SetPaper(4)
        channel1.SetStrip([ 4 ])
        channel1.SetInk([ 7 ])

        match host.Channels.OpenAs(ChannelId 5, "scr_16x20a0x0") with
        | Result.Ok () ->
            match host.Channels.Get(ChannelId 5) with
            | Result.Ok (:? IScreenChannel as channel5) ->
                channel5.SetPaper(4)
                channel5.Clear()
                channel1.WriteText("A")

                let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 5)
                Assert.That(pane.Surface[0, 0] &&& 0x00FFFFFF, Is.EqualTo(4))
                let hasInk =
                    seq {
                        for row in 0 .. Array2D.length1 pane.Surface - 1 do
                            for col in 0 .. Array2D.length2 pane.Surface - 1 do
                                yield pane.Surface[row, col] &&& 0x00FFFFFF
                    }
                    |> Seq.exists (fun color -> color = 7)
                Assert.That(hasInk, Is.True)
            | Result.Ok channel ->
                Assert.Fail($"Expected channel #5 to be a screen channel, got {channel.Kind}")
            | Result.Error err ->
                Assert.Fail($"Expected channel #5 to exist, got %A{err}")
        | Result.Error err ->
            Assert.Fail($"Expected channel #5 to open, got %A{err}")
    | Result.Ok channel ->
        Assert.Fail($"Expected channel #1 to be a screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel #1 to exist, got %A{err}")

[<Test>]
let ``default host pent prompt sequence keeps second prompt visible after cls on overlay pane`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as channel1) ->
        channel1.SetWindow(512, 256, 0, 0)
        channel1.SetPaper(4)
        channel1.SetStrip([ 4 ])
        channel1.SetInk([ 6 ])
        channel1.SetCharacterSize(2, 1)
        channel1.Clear()
        channel1.SetInk([ 0 ])
        channel1.SetCharacterSize(2, 0)
        channel1.SetStrip([ 2 ])
        channel1.SetCursor(0, 10)
        channel1.WriteText("WHICH LEVEL DO YOU WANT TO PLAY ON?")

        match host.Channels.OpenAs(ChannelId 5, "scr_512x180a0x75") with
        | Result.Ok () ->
            match host.Channels.Get(ChannelId 5) with
            | Result.Ok (:? IScreenChannel as channel5) ->
                channel5.SetPaper(4)
                channel5.SetBorder(4, None)
                channel5.Clear()

                channel1.SetInk([ 1 ])
                channel1.SetCursor(8, 12)
                channel1.WriteText("(HAS TO BE 3 CHARACTERS)")
                channel1.SetCursor(2, 15)
                channel1.WriteText("eg. INITIALS: ADD")
                channel1.SetInk([ 0 ])
                channel1.SetCursor(7, 10)
                channel1.WriteText("ENTER YOUR IDENTIFYING CODE:")

                let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 5)
                let promptBandHasInk =
                    seq {
                        for row in 25 .. 35 do
                            for col in 56 .. 260 do
                                yield pane.Surface[row, col] &&& 0x00FFFFFF
                    }
                    |> Seq.exists (fun color -> color <> 4)

                Assert.That(promptBandHasInk, Is.True)
            | Result.Ok channel ->
                Assert.Fail($"Expected channel #5 to be a screen channel, got {channel.Kind}")
            | Result.Error err ->
                Assert.Fail($"Expected channel #5 to exist, got %A{err}")
        | Result.Error err ->
            Assert.Fail($"Expected channel #5 to open, got %A{err}")
    | Result.Ok channel ->
        Assert.Fail($"Expected channel #1 to be a screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel #1 to exist, got %A{err}")

[<Test>]
let ``default host mode 8 overlay pane preserves full prompt width`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Screen.SetMode(QlMode8) with
    | Result.Ok () ->
        match host.Channels.Get(ChannelId 1) with
        | Result.Ok (:? IScreenChannel as channel1) ->
            channel1.SetWindow(512, 256, 0, 0)
            channel1.SetPaper(4)
            channel1.SetStrip([ 4 ])
            channel1.SetInk([ 0 ])
            channel1.SetCharacterSize(2, 0)
            channel1.Clear()

            match host.Channels.OpenAs(ChannelId 5, "scr_512x180a0x75") with
            | Result.Ok () ->
                match host.Channels.Get(ChannelId 5) with
                | Result.Ok (:? IScreenChannel as channel5) ->
                    channel5.SetPaper(4)
                    channel5.SetBorder(4, None)
                    channel5.Clear()
                    channel1.SetCursor(7, 10)
                    channel1.WriteText("ENTER YOUR IDENTIFYING CODE:")

                    let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 5)
                    let spans =
                        [ for row in 24 .. 34 do
                              let colored =
                                  [ for col in 0 .. Array2D.length2 pane.Surface - 1 do
                                        if (pane.Surface[row, col] &&& 0x00FFFFFF) <> 4 then
                                            yield col ]
                              if not colored.IsEmpty then
                                  yield List.head colored, List.last colored ]

                    match spans with
                    | [] -> Assert.Fail("Expected overlay pane to contain prompt pixels.")
                    | _ ->
                        let _, maxRight = spans |> List.maxBy snd
                        Assert.That(maxRight, Is.GreaterThanOrEqualTo(269))
                | Result.Ok channel ->
                    Assert.Fail($"Expected channel #5 to be a screen channel, got {channel.Kind}")
                | Result.Error err ->
                    Assert.Fail($"Expected channel #5 to exist, got %A{err}")
            | Result.Error err ->
                Assert.Fail($"Expected channel #5 to open, got %A{err}")
        | Result.Ok channel ->
            Assert.Fail($"Expected channel #1 to be a screen channel, got {channel.Kind}")
        | Result.Error err ->
            Assert.Fail($"Expected channel #1 to exist, got %A{err}")
    | Result.Error err ->
        Assert.Fail($"Expected mode 8 to succeed, got %A{err}")

[<Test>]
let ``default host bordered pane keeps enough text rows to avoid scrolling footer labels`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let rowText (pane: ScreenPaneSnapshot) row =
        let cols = Array2D.length2 pane.Text
        [| for col in 0 .. cols - 1 -> pane.Text[row, col].Character |] |> String

    match host.Channels.OpenAs(ChannelId 4, "scr_492x78a10x5") with
    | Result.Ok () ->
        match host.Channels.Get(ChannelId 4) with
        | Result.Ok (:? IScreenChannel as channel4) ->
            channel4.SetPaper(0)
            channel4.SetBorder(1, Some 7)
            channel4.Clear()
            channel4.SetInk([ 2 ])
            channel4.SetCharacterSize(2, 0)
            channel4.SetCursor(0, 0)
            channel4.WriteText("SCORE: 00000    EVENT 01    LEVEL 01")
            channel4.NewLine()
            channel4.SetCursor(24, 1)
            channel4.WriteText("WORLD RECORDS")
            channel4.NewLine()
            channel4.SetCursor(2, 2)
            channel4.WriteText("FIRST")
            channel4.NewLine()
            channel4.SetCursor(1, 3)
            channel4.WriteText("SECOND:")
            channel4.NewLine()
            channel4.SetCursor(2, 4)
            channel4.WriteText("THIRD:")
            channel4.NewLine()
            channel4.SetCursor(0, 6)
            channel4.WriteText("  EVENT: SHOT PUTT   WORLD RECORDS: 00")
            channel4.NewLine()
            channel4.SetCursor(38, 6)
            channel4.WriteText("0")
            channel4.NewLine()

            let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 4)
            Assert.That(Array2D.length1 pane.Text, Is.GreaterThanOrEqualTo(8))
            Assert.That(rowText pane 0, Does.Contain("SCORE: 00000"))
            Assert.That(rowText pane 1, Does.Contain("WORLD RECORDS"))
            Assert.That(rowText pane 2, Does.Contain("FIRST"))
            Assert.That(rowText pane 6, Does.Contain("EVENT: SHOT PUTT"))
        | Result.Ok channel ->
            Assert.Fail($"Expected channel #4 to be a screen channel, got {channel.Kind}")
        | Result.Error err ->
            Assert.Fail($"Expected channel #4 to exist, got %A{err}")
    | Result.Error err ->
        Assert.Fail($"Expected channel #4 to open, got %A{err}")

[<Test>]
let ``default host shared raster text honors bordered pane pixel origin`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as channel1) ->
        channel1.SetWindow(512, 256, 0, 0)
        channel1.SetPaper(4)
        channel1.SetBorder(5, Some 1)
        channel1.SetCharacterSize(2, 0)
        channel1.SetStrip([ 4 ])
        channel1.SetInk([ 7 ])
        channel1.SetCursor(1, 8)
        channel1.WriteText("Q")

        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        let glyphPixels =
            [ for row in 0 .. Array2D.length1 pane.Surface - 1 do
                  for col in 0 .. Array2D.length2 pane.Surface - 1 do
                      if (pane.Surface[row, col] &&& 0x00FFFFFF) = 7 then
                          yield col, row ]

        match glyphPixels with
        | [] ->
            Assert.Fail("Expected bordered pane surface to contain rasterized text pixels.")
        | _ ->
            let minX = glyphPixels |> List.map fst |> List.min
            let minY = glyphPixels |> List.map snd |> List.min
            Assert.That(minX, Is.EqualTo(8))
            Assert.That(minY, Is.EqualTo(81))
    | Result.Ok channel ->
        Assert.Fail($"Expected channel #1 to be a screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel #1 to exist, got %A{err}")

[<Test>]
let ``default host dynamic text pane stays opaque over shared graphics`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    host.Graphics.SetInk([ 7 ])
    host.Graphics.Line(0.0, 45.0, 511.0, 45.0)

    match host.Channels.OpenAs(ChannelId 4, "scr_492x78a10x5") with
    | Result.Ok () ->
        match host.Channels.Get(ChannelId 4) with
        | Result.Ok (:? IScreenChannel as channel4) ->
            channel4.SetPaper(0)
            channel4.SetBorder(1, Some 7)
            channel4.Clear()
            channel4.SetCharacterSize(2, 0)
            channel4.SetCursor(24, 1)
            channel4.WriteText("WORLD RECORDS")

            let pane4 = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 4)
            Assert.That(pane4.Surface[39, 200], Is.EqualTo(0))
        | Result.Ok channel ->
            Assert.Fail($"Expected channel #4 to be a screen channel, got {channel.Kind}")
        | Result.Error err ->
            Assert.Fail($"Expected channel #4 to exist, got %A{err}")
    | Result.Error err ->
        Assert.Fail($"Expected channel #4 to open, got %A{err}")

[<Test>]
let ``default host borderless text overlay pane keeps shared graphics visible`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    host.Screen.SetMode(QlMode8) |> ignore

    match host.Channels.OpenAs(ChannelId 7, "scr_492x142a10x84") with
    | Result.Ok () ->
        match host.Channels.Get(ChannelId 7) with
        | Result.Ok (:? IScreenChannel as channel7) ->
            channel7.SetPaper(4)
            channel7.Clear()
            host.Graphics.SetDrawingContext(channel7.GetWindow(), channel7.GetPan(), (100.0, 0.0, 0.0))
            host.Graphics.SetInk([ 6 ])
            host.Graphics.Arc(50.0, 35.0, 50.0, 25.0, -1.5)
            channel7.SetInk([ 7 ])
            channel7.SetCharacterSize(2, 0)
            channel7.SetCursor(13, 21)
            channel7.WriteText("7m")

            let pane7 = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 7)
            let colors =
                seq {
                    for row in 0 .. Array2D.length1 pane7.Surface - 1 do
                        for col in 0 .. Array2D.length2 pane7.Surface - 1 do
                            let color = pane7.Surface[row, col] &&& 0x00FFFFFF
                            if color <> pane7.Paper then
                                yield color
                }
                |> Seq.distinct
                |> Seq.sort
                |> Seq.toList

            Assert.That(colors, Does.Contain(6))
            Assert.That(colors, Does.Contain(7))
        | Result.Ok channel ->
            Assert.Fail($"Expected channel #7 to be a screen channel, got {channel.Kind}")
        | Result.Error err ->
            Assert.Fail($"Expected channel #7 to exist, got %A{err}")
    | Result.Error err ->
        Assert.Fail($"Expected channel #7 to open, got %A{err}")

[<Test>]
let ``default host borderless pane keeps shared raster after paper and csize churn`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    host.Screen.SetMode(QlMode8) |> ignore

    match host.Channels.OpenAs(ChannelId 7, "scr_492x142a10x84") with
    | Result.Ok () ->
        match host.Channels.Get(ChannelId 7) with
        | Result.Ok (:? IScreenChannel as channel7) ->
            channel7.SetPaper(4)
            channel7.Clear()
            host.Graphics.SetDrawingContext(channel7.GetWindow(), channel7.GetPan(), (100.0, 0.0, 0.0))
            host.Graphics.SetInk([ 6 ])
            host.Graphics.Arc(50.0, 35.0, 50.0, 25.0, -1.5)
            channel7.SetInk([ 7 ])
            channel7.SetCharacterSize(2, 0)
            channel7.SetCursor(13, 21)
            channel7.WriteText("7m")

            // This is the sequence that previously rebuilt the local pane backing
            // buffer in black and caused the live playfield to disappear.
            channel7.SetPaper(0)
            channel7.SetCharacterSize(2, 1)
            channel7.SetPaper(4)

            let pane7 = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 7)
            let sampleX = 40
            let sampleY = 20
            Assert.That(pane7.Surface[sampleY, sampleX] &&& 0x00FFFFFF, Is.EqualTo(4))

            let colors =
                seq {
                    for row in 0 .. Array2D.length1 pane7.Surface - 1 do
                        for col in 0 .. Array2D.length2 pane7.Surface - 1 do
                            let color = pane7.Surface[row, col] &&& 0x00FFFFFF
                            if color <> 4 then
                                yield color
                }
                |> Seq.distinct
                |> Seq.sort
                |> Seq.toList

            Assert.That(colors, Does.Contain(6))
            Assert.That(colors, Does.Contain(7))
        | Result.Ok channel ->
            Assert.Fail($"Expected channel #7 to be a screen channel, got {channel.Kind}")
        | Result.Error err ->
            Assert.Fail($"Expected channel #7 to exist, got %A{err}")
    | Result.Error err ->
        Assert.Fail($"Expected channel #7 to open, got %A{err}")

[<Test>]
let ``interpreter pent prompt keeps unchanneled screen io on pane one while overlay shows shared raster`` () =
    let levelId = H.SymbolId 500
    let codeId = H.SymbolId 501
    let storages =
        [ makeStorage levelId 500 "LEVEL" H.HirType.Int H.GlobalStorage
          makeStorage codeId 501 "A$" H.HirType.String H.GlobalStorage ]
    let hir =
        makeProgram
            (Map.ofList [ levelId, "LEVEL"; codeId, "A$" ])
            storages
            [ H.BuiltInCall(H.NamedBuiltIn "WINDOW", None, [ H.Literal(H.ConstInt 512, H.HirType.Int, pos); H.Literal(H.ConstInt 256, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", None, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "STRIP", None, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "INK", None, [ H.Literal(H.ConstInt 6, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CSIZE", None, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLS", None, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "INK", None, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CSIZE", None, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "STRIP", None, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "AT", None, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.HirStmt.Input(None, [ H.Literal(H.ConstString "WHICH LEVEL DO YOU WANT TO PLAY ON?", H.HirType.String, pos) ], [ H.WriteVar(levelId, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel(H.Literal(H.ConstInt 5, H.HirType.Int, pos)), [ H.Literal(H.ConstString "scr_512x180a0x75", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", explicitChannel(H.Literal(H.ConstInt 5, H.HirType.Int, pos)), [ H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "BORDER", explicitChannel(H.Literal(H.ConstInt 5, H.HirType.Int, pos)), [ H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLS", explicitChannel(H.Literal(H.ConstInt 5, H.HirType.Int, pos)), [], pos)
              H.BuiltInCall(H.NamedBuiltIn "INK", None, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "AT", None, [ H.Literal(H.ConstInt 12, H.HirType.Int, pos); H.Literal(H.ConstInt 8, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.Print, None, [ H.Literal(H.ConstString "(HAS TO BE 3 CHARACTERS)", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "AT", None, [ H.Literal(H.ConstInt 15, H.HirType.Int, pos); H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.Print, None, [ H.Literal(H.ConstString "eg. INITIALS: ADD", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "INK", None, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "AT", None, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 7, H.HirType.Int, pos) ], pos)
              H.HirStmt.Input(None, [ H.Literal(H.ConstString "ENTER YOUR IDENTIFYING CODE:", H.HirType.String, pos) ], [ H.WriteVar(codeId, H.HirType.String, pos) ], pos) ]

    let inputs = System.Collections.Generic.Queue<string>([ "3"; "ADD" ])

    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine =
                fun _ ->
                    if inputs.Count > 0 then Some(inputs.Dequeue()) else Some "ADD"
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    let rowText (pane: ScreenPaneSnapshot) row =
        let cols = Array2D.length2 pane.Text
        [| for col in 0 .. cols - 1 -> pane.Text[row, col].Character |] |> String

    match result with
    | Result.Ok _ ->
        let pane1 = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        let pane5 = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 5)

        let pane5HasInkZero =
            seq {
                for row in 0 .. Array2D.length1 pane5.Surface - 1 do
                    for col in 0 .. Array2D.length2 pane5.Surface - 1 do
                        yield pane5.Surface[row, col] &&& 0x00FFFFFF
            }
            |> Seq.exists (fun color -> color = 0)

        Assert.That(rowText pane1 10, Does.Contain("ENTER YOUR IDENTIFYING CODE:"))
        Assert.That(pane5HasInkZero, Is.True)

    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``dynamic screen pane shows later unchanneled arc through shared raster`` () =
    let channel7 = H.Literal(H.ConstInt 7, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel channel7, [ H.Literal(H.ConstString "scr_492x142a10x84", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", explicitChannel channel7, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLS", explicitChannel channel7, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "INK", None, [ H.Literal(H.ConstInt 6, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "ARC", None, [ H.Literal(H.ConstInt 50, H.HirType.Int, pos); H.Literal(H.ConstInt 35, H.HirType.Int, pos); H.Literal(H.ConstInt 50, H.HirType.Int, pos); H.Literal(H.ConstInt 25, H.HirType.Int, pos); H.Literal(H.ConstFloat -1.5, H.HirType.Float, pos) ], pos) ]

    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ ->
        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 7)
        let hasArcInk =
            seq {
                for row in 0 .. Array2D.length1 pane.Surface - 1 do
                    for col in 0 .. Array2D.length2 pane.Surface - 1 do
                        yield pane.Surface[row, col] &&& 0x00FFFFFF
            }
            |> Seq.exists (fun color -> color = 6)
        Assert.That(hasArcInk, Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter graphics builtins support implicit channels`` () =
    let implicitScreen = Some(H.ImplicitChannel(H.Literal(H.ConstString "scr_", H.HirType.String, pos)))
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "LINE", implicitScreen, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 8, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "TURN", implicitScreen, [ H.Literal(H.ConstInt 30, H.HirType.Int, pos) ], pos) ]

    let host, _ =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ -> Assert.Pass()
    | Result.Error err -> Assert.Fail($"Expected implicit graphics channels to succeed, got %A{err}")

[<Test>]
let ``interpreter open parses console device strings and preserves configured window across mode changes`` () =
    let channel9 = H.Literal(H.ConstInt 9, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel channel9, [ H.Literal(H.ConstString "con_300x160a75x10_32", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "MODE", None, [ H.Literal(H.ConstInt 8, H.HirType.Int, pos) ], pos) ]

    let host =
        DefaultHost.create {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ ->
        match host.Channels.Get(ChannelId 9) with
        | Result.Ok (:? IScreenChannel as screenChannel) ->
            Assert.That(screenChannel.Kind, Is.EqualTo(ChannelKind.ConsoleChannel))
            Assert.That(screenChannel.GetWindow(), Is.EqualTo((234, 96, 108, 42)))
        | Result.Ok _ -> Assert.Fail("Expected channel #9 to be a console screen channel.")
        | Result.Error err -> Assert.Fail($"Expected channel #9 to exist, got %A{err}")
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter graphics operations respect window pan scale and clipping`` () =
    let host, screenState = createScreenHost []
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "WINDOW", explicitChannel channel1, [ H.Literal(H.ConstInt 100, H.HirType.Int, pos); H.Literal(H.ConstInt 50, H.HirType.Int, pos); H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAN", explicitChannel channel1, [ H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "SCALE", explicitChannel channel1, [ H.Literal(H.ConstInt 200, H.HirType.Int, pos); H.Literal(H.ConstInt 100, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PLOT", explicitChannel channel1, [ H.Literal(H.ConstInt 30, H.HirType.Int, pos); H.Literal(H.ConstInt 40, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "LINE", explicitChannel channel1, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 60, H.HirType.Int, pos); H.Literal(H.ConstInt 60, H.HirType.Int, pos) ], pos) ]

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ ->
        let expectedOps =
            [ "SCALE 200,100,0"
              "PLOT 75,60"
              "LINE 15,20 TO 109,69" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter graphics operations update backing pixel buffer`` () =
    let host, screenState = createScreenHost []
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "PLOT", None, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "LINE", None, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos) ]

    let result = interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.PixelBuffer[5, 4], Is.EqualTo(7))
        Assert.That(screenState.PixelBuffer[0, 0], Is.EqualTo(7))
        Assert.That(screenState.PixelBuffer[0, 1], Is.EqualTo(7))
        Assert.That(screenState.PixelBuffer[0, 2], Is.EqualTo(7))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter close rejects default channels`` () =
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "CLOSE", explicitChannel channel1, [], pos) ]

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError UnsupportedChannelExecution
    |> ignore

[<Test>]
let ``interpreter open supports console null and printer devices`` () =
    let channel9 = H.Literal(H.ConstInt 9, H.HirType.Int, pos)
    let channel10 = H.Literal(H.ConstInt 10, H.HirType.Int, pos)
    let channel11 = H.Literal(H.ConstInt 11, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel channel9, [ H.Literal(H.ConstString "con_", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel channel10, [ H.Literal(H.ConstString "nul_", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "OPEN", explicitChannel channel11, [ H.Literal(H.ConstString "prt_", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.Print, explicitChannel channel9, [ H.Literal(H.ConstString "console", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.Print, explicitChannel channel10, [ H.Literal(H.ConstString "discarded", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.Print, explicitChannel channel11, [ H.Literal(H.ConstString "printer", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLOSE", explicitChannel channel9, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLOSE", explicitChannel channel10, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "CLOSE", explicitChannel channel11, [], pos) ]

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    let result = interpretProgramWithOptions options hir

    match result with
    | Result.Ok execution ->
        Assert.That(String.concat "|" execution.Output, Is.EqualTo("console|printer"))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter open supports directory backed devices`` () =
    let deviceRoot = Path.Combine(Environment.CurrentDirectory, "RuntimeDevices", "ram1")
    let devicePath = Path.Combine(deviceRoot, "demo_txt")
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ChannelProcedureCall(pos, "OPEN", num "9", [ str "\"ram1_demo_txt\"" ]) ])
              Line(pos, Some 20, [ ChannelProcedureCall(pos, "PRINT", num "9", [ str "\"hello\"" ]) ])
              Line(pos, Some 30, [ ChannelProcedureCall(pos, "CLOSE", num "9", []) ]) ])

    let outputs = ResizeArray<string>()
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> None
                    ReadScreenLine = fun _ -> None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                } }

    try
        if File.Exists(devicePath) then
            File.Delete(devicePath)

        let hir = lowerProgram ast
        let result = interpretProgramWithOptions options hir

        match result with
        | Result.Ok _ ->
            Assert.That(File.Exists(devicePath), Is.True)
            Assert.That(File.ReadAllText(devicePath), Does.Contain("hello"))
        | Result.Error err ->
            Assert.Fail($"Expected interpretation to succeed, got %A{err}")
    finally
        if File.Exists(devicePath) then
            File.Delete(devicePath)

[<Test>]
let ``default host exposes tv style default output pane and listing pane`` () =
    let _, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let snapshot = display.GetSnapshot()
    let panes =
        snapshot.Panes
        |> List.choose (fun pane -> pane.ChannelId |> Option.map (fun channelId -> channelId, pane))
        |> Map.ofList

    Assert.That(panes.ContainsKey 0, Is.True)
    Assert.That(panes.ContainsKey 1, Is.True)
    Assert.That(panes.ContainsKey 2, Is.True)
    Assert.That(panes[0].Window, Is.EqualTo((512, 40, 0, 216)))
    Assert.That(panes[1].Window, Is.EqualTo((512, 216, 0, 0)))
    Assert.That(panes[2].Window, Is.EqualTo((256, 216, 0, 0)))
    Assert.That(panes[1].Text[0, 0].Paper, Is.EqualTo(2))
    Assert.That(panes[0].Text[0, 0].Paper, Is.EqualTo(0))

[<Test>]
let ``default host uses distinct tv style windows for output and listings`` () =
    let _, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    let snapshot = display.GetSnapshot()
    let pane1 = snapshot.Panes |> List.find (fun pane -> pane.ChannelId = Some 1)
    let pane2 = snapshot.Panes |> List.find (fun pane -> pane.ChannelId = Some 2)
    let _, _, pane1X, _ = pane1.Window
    let _, _, pane2X, _ = pane2.Window

    Assert.That(pane1.Window, Is.Not.EqualTo(pane2.Window))
    Assert.That(pane1X, Is.EqualTo(0))
    Assert.That(pane2X, Is.EqualTo(0))

[<Test>]
let ``default host pane snapshots stay independent per channel`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1), host.Channels.Get(ChannelId 2) with
    | Result.Ok channel1, Result.Ok channel2 ->
        channel1.WriteText("ONE")
        channel2.WriteText("TWO")
    | _ ->
        Assert.Fail("Expected default channels #1 and #2 to exist.")

    let snapshot = display.GetSnapshot()
    let pane1 = snapshot.Panes |> List.find (fun pane -> pane.ChannelId = Some 1)
    let pane2 = snapshot.Panes |> List.find (fun pane -> pane.ChannelId = Some 2)

    let paneText (pane: ScreenPaneSnapshot) =
        [ for row in 0 .. Array2D.length1 pane.Text - 1 do
              for col in 0 .. Array2D.length2 pane.Text - 1 do
                  let ch = pane.Text[row, col].Character
                  if ch <> ' ' then yield ch ]
        |> Array.ofList
        |> System.String

    Assert.That(paneText pane1, Does.Contain("ONE"))
    Assert.That(paneText pane1, Does.Not.Contain("TWO"))
    Assert.That(paneText pane2, Does.Contain("TWO"))
    Assert.That(paneText pane2, Does.Not.Contain("ONE"))

[<Test>]
let ``default host snapshot text cells use strip as text background`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetPaper(2)
        screenChannel.SetStrip([ 7; 0 ])
        screenChannel.WriteText("A")
        let snapshot = display.GetSnapshot()
        let pane = snapshot.Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(pane.Text[0, 0].Paper, Is.EqualTo(2))
        Assert.That(pane.Text[0, 0].Strip, Is.EqualTo(7))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host pane snapshots preserve paper strip and border state`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetPaper(4)
        screenChannel.SetStrip([ 6; 0 ])
        screenChannel.SetBorder(2, None)
        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(pane.Paper, Is.EqualTo(4))
        Assert.That(pane.Strip, Is.EqualTo(6))
        Assert.That(pane.BorderSize, Is.EqualTo(2))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host pane snapshots preserve recolor and palette state`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetRecolor([ 2; 3; 4; 5; 6; 7; 0; 1 ])
        screenChannel.SetPalette([ 7; 6; 5; 4; 3; 2; 1; 0 ])
        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(pane.Recolor, Is.EqualTo(Some [ 2; 3; 4; 5; 6; 7; 0; 1 ]))
        Assert.That(pane.Palette, Is.EqualTo(Some [ 7; 6; 5; 4; 3; 2; 1; 0 ]))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host preserves strip on explicitly written blank cells`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetPaper(2)
        screenChannel.SetStrip([ 6; 0 ])
        screenChannel.WriteText(" ")
        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(string pane.Text[0, 0].Character, Is.EqualTo(" "))
        Assert.That(pane.Text[0, 0].Paper, Is.EqualTo(2))
        Assert.That(pane.Text[0, 0].Strip, Is.EqualTo(6))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host snapshot text cells preserve raw character codes`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok channel ->
        channel.WriteText(string (char 128))
        let snapshot = display.GetSnapshot()
        let pane = snapshot.Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(pane.Text[0, 0].CodePoint, Is.EqualTo(128))
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host pane text grid follows window size and csize`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetWindow(24, 20, 0, 0)
        screenChannel.SetCharacterSize(0, 0)
        screenChannel.WriteText("ABCD")
        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(Array2D.length2 pane.Text, Is.EqualTo(8))
        Assert.That(Array2D.length1 pane.Text, Is.EqualTo(2))
        Assert.That(string pane.Text[0, 0].Character, Is.EqualTo("A"))
        Assert.That(string pane.Text[0, 1].Character, Is.EqualTo("B"))
        Assert.That(string pane.Text[0, 2].Character, Is.EqualTo("C"))
        Assert.That(string pane.Text[0, 3].Character, Is.EqualTo("D"))
        Assert.That(pane.Cursor, Is.EqualTo((4, 0)))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host newline scrolls by visible text rows`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetWindow(16, 20, 0, 0)
        screenChannel.SetCharacterSize(0, 0)
        screenChannel.WriteText("AB")
        screenChannel.NewLine()
        screenChannel.WriteText("CD")
        screenChannel.NewLine()
        screenChannel.WriteText("EF")
        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(string pane.Text[0, 0].Character + string pane.Text[0, 1].Character, Is.EqualTo("CD"))
        Assert.That(string pane.Text[1, 0].Character + string pane.Text[1, 1].Character, Is.EqualTo("EF"))
        Assert.That(pane.Cursor, Is.EqualTo((2, 1)))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host extended modes use denser text rows than classic ql modes`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Screen.SetMode(ExtendedMode 256), host.Channels.Get(ChannelId 1) with
    | Result.Ok (), Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetWindow(16, 16, 0, 0)
        screenChannel.SetCharacterSize(0, 0)
        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(Array2D.length2 pane.Text, Is.EqualTo(6))
        Assert.That(Array2D.length1 pane.Text, Is.EqualTo(2))
    | Result.Error err, _ ->
        Assert.Fail($"Expected mode change to succeed, got %A{err}")
    | _, Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | _, Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host rasterizes dline circle ellipse and arc into pane pixels`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetPaper(0)
        screenChannel.Clear()
        host.Graphics.SetDrawingContext(screenChannel.GetWindow(), screenChannel.GetPan(), (100.0, 0.0, 0.0))
        host.Graphics.SetInk([ 7 ])
        host.Graphics.DLine([ 0.0; 0.0; 20.0; 0.0; 20.0; 20.0 ])
        host.Graphics.Circle(40.0, 40.0, 8.0)
        host.Graphics.Ellipse(80.0, 40.0, 10.0, 0.5, 0.0)
        host.Graphics.Arc(120.0, 40.0, 10.0, 0.0, 180.0)

        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        let litPixels =
            seq {
                for row = 0 to Array2D.length1 pane.Pixels - 1 do
                    for col = 0 to Array2D.length2 pane.Pixels - 1 do
                        if (pane.Pixels[row, col] &&& 0x00FFFFFF) <> 0 then
                            yield row, col
            }
            |> Seq.toArray

        Assert.That(pane.Pixels[215, 0] &&& 0x00FFFFFF, Is.EqualTo(7))
        Assert.That(pane.Pixels[215, 43] &&& 0x00FFFFFF, Is.EqualTo(7))
        Assert.That(pane.Pixels[172, 43] &&& 0x00FFFFFF, Is.EqualTo(7))
        Assert.That(litPixels.Length, Is.GreaterThan(90))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host graphics modes apply xor under and flash to pixels`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetPaper(0)
        screenChannel.Clear()
        host.Graphics.SetDrawingContext(screenChannel.GetWindow(), screenChannel.GetPan(), (100.0, 0.0, 0.0))
        host.Graphics.SetInk([ 6 ])
        host.Graphics.Plot(10.0, 10.0)
        host.Graphics.SetOver(-1)
        host.Graphics.Plot(10.0, 10.0)
        host.Graphics.SetOver(0)
        host.Graphics.SetInk([ 3 ])
        host.Graphics.Plot(15.0, 15.0)
        host.Graphics.SetUnder(1)
        host.Graphics.SetInk([ 5 ])
        host.Graphics.Plot(15.0, 15.0)
        host.Graphics.Plot(16.0, 15.0)
        host.Graphics.SetUnder(0)
        host.Graphics.SetFlash(1)
        host.Graphics.Plot(20.0, 20.0)

        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(pane.Pixels[193, 22] &&& 0x00FFFFFF, Is.EqualTo(0))
        Assert.That(pane.Pixels[183, 32] &&& 0x00FFFFFF, Is.EqualTo(3))
        Assert.That(pane.Pixels[183, 35] &&& 0x00FFFFFF, Is.EqualTo(5))
        Assert.That((pane.Pixels[172, 43] &&& 0x01000000) <> 0, Is.True)
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host text cells stay transparent until text writes a background`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetPaper(0)
        screenChannel.Clear()
        host.Graphics.SetDrawingContext(screenChannel.GetWindow(), screenChannel.GetPan(), (100.0, 0.0, 0.0))
        host.Graphics.SetInk([ 4 ])
        host.Graphics.Plot(0.0, 0.0)

        let beforeWrite = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(beforeWrite.Text[0, 0].HasBackground, Is.False)
        Assert.That(beforeWrite.Pixels[215, 0] &&& 0x00FFFFFF, Is.EqualTo(4))

        screenChannel.SetStrip([ 6; 0 ])
        screenChannel.WriteText(" ")

        let afterWrite = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(afterWrite.Text[0, 0].HasBackground, Is.True)
        Assert.That(afterWrite.Text[0, 0].Strip, Is.EqualTo(6))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host pane surface composes graphics and text in one raster`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetWindow(16, 20, 0, 0)
        screenChannel.SetCharacterSize(0, 0)
        screenChannel.SetPaper(0)
        screenChannel.SetStrip([ 6; 0 ])
        screenChannel.Clear()
        host.Graphics.SetDrawingContext(screenChannel.GetWindow(), screenChannel.GetPan(), (100.0, 0.0, 0.0))
        host.Graphics.SetInk([ 4 ])
        host.Graphics.Plot(15.0, 19.0)
        screenChannel.WriteText("A")

        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(pane.Surface[15, 3] &&& 0x00FFFFFF, Is.EqualTo(4))
        Assert.That(pane.Surface[0, 0] &&& 0x00FFFFFF, Is.EqualTo(6))
        let hasInk =
            seq {
                for row in 0 .. Array2D.length1 pane.Surface - 1 do
                    for col in 0 .. Array2D.length2 pane.Surface - 1 do
                        yield pane.Surface[row, col] &&& 0x00FFFFFF
            }
            |> Seq.exists (fun color -> color = 7)
        Assert.That(hasInk, Is.True)
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host pane surface preserves graphics under transparent text cells`` () =
    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine = fun () -> None
            ReadScreenLine = fun _ -> None
            FlushInput = fun () -> ()
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    match host.Channels.Get(ChannelId 1) with
    | Result.Ok (:? IScreenChannel as screenChannel) ->
        screenChannel.SetWindow(16, 20, 0, 0)
        screenChannel.SetCharacterSize(0, 0)
        screenChannel.SetPaper(0)
        screenChannel.Clear()
        host.Graphics.SetDrawingContext(screenChannel.GetWindow(), screenChannel.GetPan(), (100.0, 0.0, 0.0))
        host.Graphics.SetInk([ 5 ])
        host.Graphics.Plot(8.0, 10.0)

        let pane = display.GetSnapshot().Panes |> List.find (fun item -> item.ChannelId = Some 1)
        Assert.That(pane.Text[0, 1].HasBackground, Is.False)
        Assert.That(pane.Surface[17, 2] &&& 0x00FFFFFF, Is.EqualTo(5))
    | Result.Ok channel ->
        Assert.Fail($"Expected screen channel, got {channel.Kind}")
    | Result.Error err ->
        Assert.Fail($"Expected channel lookup to succeed, got %A{err}")

[<Test>]
let ``default host maps unchanneled print and input prompt to output window`` () =
    let host, screenState = createScreenHost [ "42" ]
    let options = { defaultRuntimeOptions with Host = host }

    let xId = H.SymbolId 0
    let xStorage = makeStorage xId 0 "X" H.HirType.Int H.GlobalStorage
    let hir =
        makeProgram
            (Map.ofList [ xId, "X" ])
            [ xStorage ]
            [ H.BuiltInCall(H.Print, None, [ H.Literal(H.ConstString "HELLO", H.HirType.String, pos) ], pos)
              H.HirStmt.Input(None, [ H.Literal(H.ConstString "Enter", H.HirType.String, pos) ], [ H.WriteVar(xId, H.HirType.Int, pos) ], pos) ]

    let result = interpretProgramWithOptions options hir

    match result with
    | Result.Ok _ ->
        Assert.That(screenState.Windows[1].Writes |> Seq.toList |> String.concat "|", Is.EqualTo("HELLO|Enter"))
        Assert.That(screenState.Windows[0].Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
        Assert.That(screenState.Windows[2].Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")



