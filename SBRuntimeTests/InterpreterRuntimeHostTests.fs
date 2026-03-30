module SBRuntimeTests.InterpreterRuntimeHostTests

open System.Collections.Generic
open NUnit.Framework

open SyntaxAst
open Interpreter
open SBRuntime
open SBRuntimeTests.TestSupport

module H = HIR

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
let ``interpreter handles input prompts and targets`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "INPUT", [ str "\"Enter\""; id "n" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ id "n" ]) ]) ])

    let output = runProgramWithInput [ "42" ] ast

    Assert.That(String.concat "|" output, Is.EqualTo("Enter|42"))

[<Test>]
let ``default host exposes default channels zero through two`` () =
    let host =
        DefaultHost.create {
            ReadLine = fun () -> None
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
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
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            WriteLine = ignore
        }

    let modes = host.Screen.GetSupportedModes()

    Assert.That(modes |> List.exists (fun mode -> mode.Mode = QlMode4), Is.True)
    Assert.That(modes |> List.exists (fun mode -> mode.Mode = QlMode8), Is.True)
    Assert.That(modes |> List.exists (fun mode -> mode.Mode = ExtendedMode 256), Is.True)
    Assert.That(modes |> List.exists (fun mode -> mode.Mode = ExtendedMode 512), Is.True)

[<Test>]
let ``interpreter print to default channels uses host channel manager`` () =
    let outputs = ResizeArray<string>()
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.Print, Some(H.Literal(H.ConstInt 1, H.HirType.Int, pos)), [ H.Literal(H.ConstString "hello", H.HirType.String, pos) ], pos)
              H.BuiltInCall(H.Print, Some(H.Literal(H.ConstInt 2, H.HirType.Int, pos)), [ H.Literal(H.ConstString "world", H.HirType.String, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host =
                    DefaultHost.create {
                        ReadLine = fun () -> None
                        ReadKey = fun () -> None
                        KeyAvailable = fun () -> false
                        WriteLine = fun line -> outputs.Add(line)
                    } }
            hir

    match result with
    | Result.Ok execution -> Assert.That(String.concat "|" execution.Output, Is.EqualTo("hello|world"))
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
            [ H.HirStmt.Input(Some(channelExpr), [ promptExpr ], [ target ], pos)
              H.BuiltInCall(H.Print, None, [ H.ReadVar(xId, H.HirType.Int, pos) ], pos) ]

    let result =
        interpretProgramWithOptions
            { defaultRuntimeOptions with
                Host =
                    DefaultHost.create {
                        ReadLine = fun () -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                        ReadKey = fun () -> None
                        KeyAvailable = fun () -> false
                        WriteLine = fun line -> outputs.Add(line)
                    } }
            hir

    match result with
    | Result.Ok execution -> Assert.That(String.concat "|" execution.Output, Is.EqualTo("Enter|42"))
    | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}")

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
            [ H.BuiltInCall(H.NamedBuiltIn "CLS", Some channel0, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "WINDOW", Some channel0, [ H.Literal(H.ConstInt 512, H.HirType.Int, pos); H.Literal(H.ConstInt 256, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "AT", Some channel0, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WINDOW", Some channel1, [ H.Literal(H.ConstInt 448, H.HirType.Int, pos); H.Literal(H.ConstInt 40, H.HirType.Int, pos); H.Literal(H.ConstInt 32, H.HirType.Int, pos); H.Literal(H.ConstInt 216, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CURSOR", Some channel1, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CSIZE", Some channel1, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "INK", Some channel1, [ H.Literal(H.ConstInt 7, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WINDOW", Some channel2, [ H.Literal(H.ConstInt 300, H.HirType.Int, pos); H.Literal(H.ConstInt 120, H.HirType.Int, pos); H.Literal(H.ConstInt 100, H.HirType.Int, pos); H.Literal(H.ConstInt 80, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CSIZE", Some channel2, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", Some channel2, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "BORDER", Some channel2, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.HirStmt.Input(Some channel2, [ H.Literal(H.ConstString "Enter", H.HirType.String, pos) ], [ H.WriteVar(xId, H.HirType.Int, pos) ], pos) ]

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
        Assert.That(window0.Ink, Is.EqualTo(None))
        Assert.That(window1.Window, Is.EqualTo((448, 40, 32, 216)))
        Assert.That(window1.Cursor, Is.EqualTo((10, 20)))
        Assert.That(window1.CharacterSize, Is.EqualTo((2, 1)))
        Assert.That(window1.Ink, Is.EqualTo(Some [ 7 ]))
        Assert.That(window1.Paper, Is.EqualTo(None))
        Assert.That(window2.Window, Is.EqualTo((300, 120, 100, 80)))
        Assert.That(window2.CharacterSize, Is.EqualTo((4, 3)))
        Assert.That(window2.Paper, Is.EqualTo(Some 2))
        Assert.That(window2.Border, Is.EqualTo(Some 1))
        Assert.That(window2.Writes |> Seq.toList |> String.concat "|", Is.EqualTo("Enter"))
        Assert.That(window0.Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
        Assert.That(window1.Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
    | Result.Error err ->
        Assert.Fail($"Expected interpretation to succeed, got %A{err}")

[<Test>]
let ``interpreter display controls update per-window state`` () =
    let host, screenState = createScreenHost []
    let channel1 = H.Literal(H.ConstInt 1, H.HirType.Int, pos)
    let hir =
        makeProgram
            Map.empty
            []
            [ H.BuiltInCall(H.NamedBuiltIn "SCROLL", Some channel1, [ H.Literal(H.ConstInt 12, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WIDTH", Some channel1, [ H.Literal(H.ConstInt 80, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAN", Some channel1, [ H.Literal(H.ConstInt -150, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "RECOL", Some channel1, [ H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PALETTE", Some channel1, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
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
        let window0 = screenState.Windows[0]
        let window1 = screenState.Windows[1]
        Assert.That(window1.Scroll, Is.EqualTo(12))
        Assert.That(window1.Width, Is.EqualTo(Some 80))
        Assert.That(window1.Pan, Is.EqualTo(-150))
        Assert.That(window1.Recolor, Is.EqualTo(Some 5))
        Assert.That(window1.Palette, Is.EqualTo(Some [ 1; 3; 5 ]))
        Assert.That(window0.Scroll, Is.EqualTo(-7))
        Assert.That(window0.Width, Is.EqualTo(Some 64))
        Assert.That(window0.Pan, Is.EqualTo(150))
        Assert.That(window0.Recolor, Is.EqualTo(Some 2))
        Assert.That(window0.Palette, Is.EqualTo(Some [ 8; 6 ]))
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
        Assert.That(screenState.Windows[0].Window, Is.EqualTo((480, 255, 20, 0)))
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
              H.BuiltInCall(H.NamedBuiltIn "DRAW", Some channel1, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstFloat 4.5, H.HirType.Float, pos) ], pos)
              H.BuiltInCall(
                  H.NamedBuiltIn "LINE",
                  Some channel2,
                  [ H.Literal(H.ConstInt 0, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 0, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 10, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 10, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 20, H.HirType.Int, pos)
                    H.Literal(H.ConstInt 5, H.HirType.Int, pos) ],
                  pos)
              H.BuiltInCall(H.NamedBuiltIn "CIRCLE", None, [ H.Literal(H.ConstInt 8, H.HirType.Int, pos); H.Literal(H.ConstInt 9, H.HirType.Int, pos); H.Literal(H.ConstFloat 2.5, H.HirType.Float, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "ELLIPSE", Some channel2, [ H.Literal(H.ConstInt 11, H.HirType.Int, pos); H.Literal(H.ConstInt 12, H.HirType.Int, pos); H.Literal(H.ConstInt 13, H.HirType.Int, pos); H.Literal(H.ConstFloat 0.7, H.HirType.Float, pos); H.Literal(H.ConstInt 6, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "FILL", Some channel0, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos) ]

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
              "ELLIPSE 11,12,13,0.69999999999999996,6"
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
            [ H.BuiltInCall(H.NamedBuiltIn "INK", Some channel1, [ H.Literal(H.ConstInt 6, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos) ]

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
            [ H.BuiltInCall(H.NamedBuiltIn "OVER", Some channel0, [ H.Literal(H.ConstInt -1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "UNDER", None, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "FLASH", Some channel1, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PENUP", None, [], pos)
              H.BuiltInCall(H.NamedBuiltIn "PENDOWN", Some channel1, [], pos) ]

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
              H.BuiltInCall(H.NamedBuiltIn "POINT_R", Some channel1, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt -2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "DLINE", None, [ H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 0, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos); H.Literal(H.ConstInt 5, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "LINE_R", Some channel1, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos); H.Literal(H.ConstInt 6, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CIRCLE_R", None, [ H.Literal(H.ConstInt 7, H.HirType.Int, pos); H.Literal(H.ConstInt 8, H.HirType.Int, pos); H.Literal(H.ConstInt 9, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "ELLIPSE_R", Some channel1, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 11, H.HirType.Int, pos); H.Literal(H.ConstInt 12, H.HirType.Int, pos); H.Literal(H.ConstFloat 0.5, H.HirType.Float, pos); H.Literal(H.ConstInt 13, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "ARC", None, [ H.Literal(H.ConstInt 14, H.HirType.Int, pos); H.Literal(H.ConstInt 15, H.HirType.Int, pos); H.Literal(H.ConstInt 16, H.HirType.Int, pos); H.Literal(H.ConstInt 17, H.HirType.Int, pos); H.Literal(H.ConstInt 18, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "ARC_R", Some channel1, [ H.Literal(H.ConstInt 19, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos); H.Literal(H.ConstInt 21, H.HirType.Int, pos); H.Literal(H.ConstInt 22, H.HirType.Int, pos); H.Literal(H.ConstInt 23, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "BLOCK", None, [ H.Literal(H.ConstInt 24, H.HirType.Int, pos); H.Literal(H.ConstInt 25, H.HirType.Int, pos); H.Literal(H.ConstInt 26, H.HirType.Int, pos); H.Literal(H.ConstInt 27, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "TURN", None, [ H.Literal(H.ConstInt 30, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "TURNTO", Some channel1, [ H.Literal(H.ConstInt 45, H.HirType.Int, pos) ], pos) ]

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
              "ELLIPSE_R 10,11,12,0.5,13"
              "ARC 14,15,16,17,18"
              "ARC_R 19,20,21,22,23"
              "BLOCK 24,25,26,27,3"
              "TURN 30"
              "TURNTO 45" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
        Assert.That(screenState.Heading, Is.EqualTo(45.0))
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
            [ H.BuiltInCall(H.NamedBuiltIn "INK", Some channel0, [ H.Literal(H.ConstInt 7, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAPER", Some channel0, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "BORDER", Some channel0, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "SCROLL", Some channel1, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "WIDTH", Some channel1, [ H.Literal(H.ConstInt 40, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PAN", Some channel1, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "RECOL", Some channel1, [ H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PALETTE", Some channel1, [ H.Literal(H.ConstInt 9, H.HirType.Int, pos) ], pos)
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
        Assert.That(screenState.Windows[0].Border, Is.EqualTo(Some 1))
        Assert.That(screenState.Windows[1].Scroll, Is.EqualTo(3))
        Assert.That(screenState.Windows[1].Width, Is.EqualTo(Some 40))
        Assert.That(screenState.Windows[1].Pan, Is.EqualTo(10))
        Assert.That(screenState.Windows[1].Recolor, Is.EqualTo(Some 4))
        Assert.That(screenState.Windows[1].Palette, Is.EqualTo(Some [ 9 ]))
        let expectedOps =
            [ "INK 7"
              "LINE 0,1 TO 2,3"
              "LINE_R 4,5"
              "DLINE 6,7,8,9" ]
        Assert.That(screenState.GraphicsOps |> Seq.toList = expectedOps, Is.True)
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
            [ H.BuiltInCall(H.NamedBuiltIn "BORDER", Some channel0, [ H.Literal(H.ConstInt 5, H.HirType.Int, pos); H.Literal(H.ConstInt 8, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "PALETTE", Some channel1, [ H.Literal(H.ConstInt 1, H.HirType.Int, pos); H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 3, H.HirType.Int, pos) ], pos)
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
        Assert.That(screenState.Windows[0].Border, Is.EqualTo(Some 5))
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


