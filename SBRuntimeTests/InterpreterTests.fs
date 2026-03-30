module SBRuntimeTests.InterpreterTests

open System
open System.IO
open Antlr4.Runtime
open NUnit.Framework

open Types
open SyntaxAst
open CompilerPipeline
open AstToHir
open Interpreter
open ParseTreeVisitor
open SBRuntime

module H = HIR

let private pos =
    { BasicLineNo = None
      EditorLineNo = 1
      Column = 0 }

let private num value = mkNumberLiteral pos value
let private str value = mkStringLiteral pos value
let private id name = mkIdentifier pos name
let private call name args = mkPostfixName pos name (Some args)
let private binary op lhs rhs = mkBinaryExpr pos op lhs rhs

let private fixturePath fileName =
    let candidates =
        [ Path.Combine(__SOURCE_DIRECTORY__, "..", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", Path.GetFileName(fileName)) ]
        |> List.map Path.GetFullPath

    match candidates |> List.tryFind File.Exists with
    | Some path -> path
    | None -> failwith $"Fixture file not found: {fileName}"

let private parseAstFromFile fileName =
    let input = File.ReadAllText(fixturePath fileName)
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)

    parser.program()
    |> convertTreeToAst
    |> List.head

let private lowerProgram ast =
    let analyzed = runSemanticAnalysis ast
    match lowerToHir analyzed with
    | Result.Ok hir -> hir
    | Result.Error errs -> Assert.Fail($"Expected HIR lowering to succeed, got %A{errs}"); Unchecked.defaultof<_>

let private runProgramWithInput input ast =
    let hir = lowerProgram ast
    let outputs = ResizeArray<string>()
    let inputs = Collections.Generic.Queue<string>(input : string list)
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                    WriteLine = fun line -> outputs.Add(line)
                }
            Random = Random(1234)
            Clock = fun () -> DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc) }

    match interpretProgramWithOptions options hir with
    | Result.Ok result -> result.Output
    | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}"); []

let private runProgram ast =
    runProgramWithInput [] ast

type private ScreenWindowState = {
    mutable Cursor: int * int
    mutable CharacterSize: int * int
    mutable ClearCount: int
    mutable Ink: int option
    mutable Paper: int option
    mutable Border: int option
    Writes: ResizeArray<string>
}

type private ScreenState = {
    mutable Mode: ScreenModeInfo
    Inputs: Collections.Generic.Queue<string>
    Windows: Map<int, ScreenWindowState>
}

let private createScreenHost (inputs: string list) =
    let supportedModes =
        [ { Mode = QlMode4; Width = 512; Height = 256; Colors = Some 4; Name = "QL Mode 4"; IsQlCompatible = true }
          { Mode = QlMode8; Width = 256; Height = 256; Colors = Some 8; Name = "QL Mode 8"; IsQlCompatible = true }
          { Mode = ExtendedMode 256; Width = 256; Height = 256; Colors = Some 8; Name = "Extended Mode 256"; IsQlCompatible = false }
          { Mode = ExtendedMode 512; Width = 512; Height = 256; Colors = Some 4; Name = "Extended Mode 512"; IsQlCompatible = false } ]

    let makeWindowState () =
        { Cursor = 0, 0
          CharacterSize = 0, 0
          ClearCount = 0
          Ink = None
          Paper = None
          Border = None
          Writes = ResizeArray<string>() }

    let windows =
        [ 0, makeWindowState ()
          1, makeWindowState ()
          2, makeWindowState () ]
        |> Map.ofList

    let state =
        { Mode = supportedModes[0]
          Inputs = Collections.Generic.Queue<string>(inputs)
          Windows = windows }

    let reader () =
        if state.Inputs.Count > 0 then Some(state.Inputs.Dequeue()) else None

    let writeWindow channelId line =
        match Map.tryFind channelId state.Windows with
        | Some window -> window.Writes.Add(line)
        | None -> ()

    let makeConsoleChannel channelId =
        { new IScreenChannel with
            member _.Id = ChannelId channelId
            member _.Kind = ConsoleChannel
            member _.WriteText text = writeWindow channelId text
            member _.ReadText() = reader ()
            member _.Flush() = ()
            member _.Close() = ()
            member _.Clear() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.ClearCount <- window.ClearCount + 1
                | None -> ()
            member _.SetCursor(x, y) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Cursor <- x, y
                | None -> ()
            member _.GetCursor() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Cursor
                | None -> 0, 0
            member _.SetCharacterSize(width, height) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.CharacterSize <- width, height
                | None -> ()
            member _.GetCharacterSize() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.CharacterSize
                | None -> 0, 0
            member _.SetInk(value: int) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Ink <- Some value
                | None -> ()
            member _.SetPaper(value: int) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Paper <- Some value
                | None -> ()
            member _.SetBorder(value: int) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Border <- Some value
                | None -> () }

    let makeScreenChannel channelId =
        { new IScreenChannel with
            member _.Id = ChannelId channelId
            member _.Kind = ScreenChannel
            member _.WriteText text = writeWindow channelId text
            member _.ReadText() = reader ()
            member _.Flush() = ()
            member _.Close() = ()
            member _.Clear() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.ClearCount <- window.ClearCount + 1
                | None -> ()
            member _.SetCursor(x, y) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Cursor <- x, y
                | None -> ()
            member _.GetCursor() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Cursor
                | None -> 0, 0
            member _.SetCharacterSize(width, height) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.CharacterSize <- width, height
                | None -> ()
            member _.GetCharacterSize() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.CharacterSize
                | None -> 0, 0
            member _.SetInk(value: int) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Ink <- Some value
                | None -> ()
            member _.SetPaper(value: int) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Paper <- Some value
                | None -> ()
            member _.SetBorder(value: int) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Border <- Some value
                | None -> () }

    let channels =
        [ makeConsoleChannel 0 :> IChannel
          makeScreenChannel 1 :> IChannel
          makeScreenChannel 2 :> IChannel ]
        |> List.map (fun channel -> channel.Id, channel)
        |> Map.ofList

    let host =
        { new IRuntimeHost with
            member _.Channels =
                { new IChannelManager with
                    member _.Open(_name: string) = Result.Error(UnsupportedHostOperation "Open not implemented in test host.")
                    member _.Get(channelId: ChannelId) =
                        match Map.tryFind channelId channels with
                        | Some channel -> Result.Ok channel
                        | None -> Result.Error(ChannelNotFound channelId)
                    member _.Close(channelId: ChannelId) =
                        match Map.tryFind channelId channels with
                        | Some channel ->
                            channel.Close()
                            Result.Ok()
                        | None -> Result.Error(ChannelNotFound channelId) }
            member _.Screen =
                { new IScreenDevice with
                    member _.Clear() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.ClearCount <- window.ClearCount + 1
                        | None -> ()
                    member _.SetCursor(x, y) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Cursor <- x, y
                        | None -> ()
                    member _.GetCursor() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Cursor
                        | None -> 0, 0
                    member _.SetCharacterSize(width, height) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.CharacterSize <- width, height
                        | None -> ()
                    member _.GetCharacterSize() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.CharacterSize
                        | None -> 0, 0
                    member _.WriteText text = writeWindow 0 text
                    member _.SetInk(value: int) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Ink <- Some value
                        | None -> ()
                    member _.SetPaper(value: int) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Paper <- Some value
                        | None -> ()
                    member _.SetBorder(value: int) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Border <- Some value
                        | None -> ()
                    member _.GetSupportedModes() = supportedModes
                    member _.GetMode() = state.Mode
                    member _.SetMode(mode: ScreenMode) =
                        match supportedModes |> List.tryFind (fun candidate -> candidate.Mode = mode) with
                        | Some selected ->
                            state.Mode <- selected
                            Result.Ok()
                        | None ->
                            Result.Error(UnsupportedHostOperation $"Mode '{mode}' is not supported in the test host.") }
            member _.Graphics =
                { new IGraphicsDevice with
                    member _.Plot(_x, _y) = ()
                    member _.Draw(_x, _y) = ()
                    member _.Line(_x1, _y1, _x2, _y2) = ()
                    member _.Circle(_x, _y, _radius) = ()
                    member _.Fill(_x, _y) = ()
                    member _.Clear() = () }
            member _.Input =
                { new IInputDevice with
                    member _.ReadLine() = reader ()
                    member _.ReadKey() = None
                    member _.KeyAvailable() = false }
            member _.Sound =
                { new ISoundDevice with
                    member _.Beep(_pitch, _duration) = () }
            member _.Files =
                { new IDeviceFileSystem with
                    member _.OpenFile(_path, _mode) = Result.Error(UnsupportedHostOperation "Files not implemented in test host.")
                    member _.Exists(_path) = false
                    member _.Delete(_path) = Result.Error(UnsupportedHostOperation "Files not implemented in test host.") } }

    host, state

let private makeStorage symbolId slotId name hirType storageClass : H.HirStorage =
    { Symbol = symbolId
      Slot = H.StorageSlotId slotId
      Name = name
      Type = hirType
      Class = storageClass
      Position = pos }

let private makeProgram symbolNames globals main : H.HirProgram =
    { SymbolNames = symbolNames
      Globals = globals
      Routines = []
      DataEntries = []
      RestorePoints = []
      Main = main }

let private runHirProgramWithOptions options hir =
    match interpretProgramWithOptions options hir with
    | Result.Ok result -> result.Output
    | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}"); []

let private assertRuntimeError (expectedCode: RuntimeErrorCode) result =
    match result with
    | Result.Ok _ ->
        Assert.Fail($"Expected runtime error {expectedCode} but interpretation succeeded")
        Unchecked.defaultof<RuntimeError>
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(expectedCode))
        Assert.That(err.Message, Does.StartWith($"Runtime error [{expectedCode}]"))
        err

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
let ``interpreter aliases bare variable procedure arguments without reference`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", num "3") ])
              Line(pos, Some 30, [ ProcedureCall(pos, "bump", [ id "x" ]) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "PRINT", [ id "x" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("4"))

[<Test>]
let ``interpreter aliases bare array element procedure arguments without reference`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(pos, Some 20, [ DimStmt(pos, [ "scores", [ num "5" ] ]) ])
              Line(pos, Some 30, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "1" ]), num "3") ])
              Line(pos, Some 40, [ ProcedureCall(pos, "bump", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("4"))

[<Test>]
let ``interpreter local is dynamically visible to called procedure`` () =
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
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "7") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ],
                    None) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "main", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("7"))

[<Test>]
let ``interpreter local shadows global in called procedure`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "3") ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "9") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ],
                    None) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "main", []) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("9|3"))

[<Test>]
let ``interpreter dynamic local lookup walks multiple caller levels`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "leaf",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ ProcedureCall(pos, "leaf", []) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "11") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ]) ],
                    None) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "root", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("11"))

[<Test>]
let ``interpreter parameter shadows caller local which shadows global`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "1") ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "show",
                    [ "score" ],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "2") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", [ num "3" ]) ]) ],
                    None) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "main", []) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("3|1"))

[<Test>]
let ``interpreter local array is dynamically visible to called procedure`` () =
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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "scores", Some [ num "5" ] ]) ])
                      Line(pos, Some 210, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "1" ]), num "8") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "show", []) ]) ],
                    None) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "main", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("8"))

[<Test>]
let ``interpreter callee can write caller local and grandchild reads updated value`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "leaf",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ Assignment(pos, id "score", num "17") ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "leaf", []) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "11") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ])
                      Line(pos, Some 330, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "root", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("17|17"))

[<Test>]
let ``interpreter callee can write caller local array element and grandchild reads updated value`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "leaf",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "2" ]) ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "2" ]), num "23") ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "leaf", []) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "scores", Some [ num "5" ] ]) ])
                      Line(pos, Some 310, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "2" ]), num "7") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ])
                      Line(pos, Some 330, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "2" ]) ]) ]) ],
                    None) ])
              Line(pos, Some 40, [ ProcedureCall(pos, "root", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("23|23"))

[<Test>]
let ``interpreter dynamic writes target nearest caller local before outer local or global`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "1") ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "leaf",
                    [],
                    [ Line(pos, Some 100, [ Assignment(pos, id "score", num "99") ])
                      Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "middle",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "5") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "leaf", []) ])
                      Line(pos, Some 230, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 40,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "7") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "middle", []) ])
                      Line(pos, Some 330, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "root", []) ])
              Line(pos, Some 60, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("99|99|7|1"))

[<Test>]
let ``interpreter locals are isolated across repeated procedure calls`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 100, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "score", binary "+" (id "score") (num "1")) ])
                      Line(pos, Some 120, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "main", []) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "main", []) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1|1"))

[<Test>]
let ``interpreter rejects literal actual for reference parameter`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "bump", [ num "7" ]) ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError InvalidReferenceActual
    |> ignore

[<Test>]
let ``interpreter rejects expression actual for reference parameter`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", num "7") ])
              Line(pos, Some 30, [ ProcedureCall(pos, "bump", [ binary "+" (id "x") (num "0") ]) ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError InvalidReferenceActual
    |> ignore

[<Test>]
let ``interpreter accepts array element actual for reference parameter`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(pos, Some 20, [ DimStmt(pos, [ "scores", [ num "5" ] ]) ])
              Line(pos, Some 30, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "2" ]), num "9") ])
              Line(pos, Some 40, [ ProcedureCall(pos, "bump", [ mkPostfixName pos "scores" (Some [ num "2" ]) ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "2" ]) ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("10"))

[<Test>]
let ``interpreter supports mixed flexible and reference parameters in one routine`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "mix",
                    [ "value"; "total" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "total" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "value", binary "+" (id "value") (num "10")) ])
                      Line(pos, Some 120, [ Assignment(pos, id "total", binary "+" (id "total") (id "value")) ]) ],
                    None) ])
              Line(pos, Some 20, [ Assignment(pos, id "x", num "2") ])
              Line(pos, Some 30, [ Assignment(pos, id "sum", num "5") ])
              Line(pos, Some 40, [ ProcedureCall(pos, "mix", [ binary "+" (id "x") (num "0"); id "sum" ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "x"; id "sum" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("2 17"))

[<Test>]
let ``interpreter repeated nested reference aliasing updates caller storage`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "inc",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 110, [ Assignment(pos, id "a", binary "+" (id "a") (num "1")) ]) ],
                    None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "inc2",
                    [ "a" ],
                    [ Line(pos, Some 200, [ ReferenceStmt(pos, [ id "a" ]) ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "inc", [ id "a" ]) ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "inc", [ id "a" ]) ]) ],
                    None) ])
              Line(pos, Some 30, [ Assignment(pos, id "x", num "4") ])
              Line(pos, Some 40, [ ProcedureCall(pos, "inc2", [ id "x" ]) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "x" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("6"))

[<Test>]
let ``interpreter reference parameter shadows globals and caller locals`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "1") ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "leaf",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 30,
                [ ProcedureDef(
                    pos,
                    "show",
                    [ "score" ],
                    [ Line(pos, Some 200, [ ReferenceStmt(pos, [ id "score" ]) ])
                      Line(pos, Some 210, [ Assignment(pos, id "score", num "99") ])
                      Line(pos, Some 220, [ ProcedureCall(pos, "leaf", []) ])
                      Line(pos, Some 230, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(
                pos,
                Some 40,
                [ ProcedureDef(
                    pos,
                    "root",
                    [],
                    [ Line(pos, Some 300, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 310, [ Assignment(pos, id "score", num "7") ])
                      Line(pos, Some 320, [ ProcedureCall(pos, "show", [ id "score" ]) ])
                      Line(pos, Some 330, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "root", []) ])
              Line(pos, Some 60, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("99|99|99|1"))

[<Test>]
let ``interpreter executes for loops`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "sum", num "0") ])
              Line(
                pos,
                Some 20,
                [ ForStmt(
                    pos,
                    "i",
                    num "1",
                    num "3",
                    None,
                    StatementBlock [ Assignment(pos, id "sum", binary "+" (id "sum") (id "i")) ],
                    None) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "sum" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("6"))

[<Test>]
let ``interpreter goto jumps to numbered line target`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GotoStmt(pos, num "30") ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ str "\"right\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("right"))

[<Test>]
let ``interpreter gosub returns to following statement`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "100") ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ])
              Line(pos, Some 30, [ GotoStmt(pos, num "200") ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"sub\"" ]) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("sub|after"))

[<Test>]
let ``interpreter nested gosub returns in stack order`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "100") ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"done\"" ]) ])
              Line(pos, Some 30, [ GotoStmt(pos, num "300") ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"outer\"" ]) ])
              Line(pos, Some 110, [ GosubStmt(pos, num "200") ])
              Line(pos, Some 120, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ ProcedureCall(pos, "PRINT", [ str "\"inner\"" ]) ])
              Line(pos, Some 210, [ ReturnStmt(pos, None) ])
              Line(pos, Some 300, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("outer|inner|done"))

[<Test>]
let ``interpreter return without active gosub reports runtime error`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ReturnStmt(pos, None) ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError EscapedReturn
    |> ignore

[<Test>]
let ``interpreter goto missing target reports runtime error`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GotoStmt(pos, num "999") ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError MissingGotoTarget
    |> ignore

[<Test>]
let ``interpreter gosub missing target reports runtime error`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "999") ]) ])

    let hir = lowerProgram ast

    interpretProgramWithOptions defaultRuntimeOptions hir
    |> assertRuntimeError MissingGosubTarget
    |> ignore

[<Test>]
let ``interpreter on goto selects target by one based index`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGotoStmt(pos, num "2", [ num "100"; num "200"; num "300" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"one\"" ]) ])
              Line(pos, Some 110, [ GotoStmt(pos, num "400") ])
              Line(pos, Some 200, [ ProcedureCall(pos, "PRINT", [ str "\"two\"" ]) ])
              Line(pos, Some 210, [ GotoStmt(pos, num "400") ])
              Line(pos, Some 300, [ ProcedureCall(pos, "PRINT", [ str "\"three\"" ]) ])
              Line(pos, Some 400, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("two"))

[<Test>]
let ``interpreter on goto selector zero falls through`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGotoStmt(pos, num "0", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"fallthrough\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 200, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("fallthrough|wrong|wrong"))

[<Test>]
let ``interpreter on goto selector out of range falls through`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGotoStmt(pos, num "3", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"fallthrough\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("fallthrough"))

[<Test>]
let ``interpreter on gosub selects target and returns to caller flow`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGosubStmt(pos, num "2", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ])
              Line(pos, Some 30, [ GotoStmt(pos, num "300") ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"one\"" ]) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ ProcedureCall(pos, "PRINT", [ str "\"two\"" ]) ])
              Line(pos, Some 210, [ ReturnStmt(pos, None) ])
              Line(pos, Some 300, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("two|after"))

[<Test>]
let ``interpreter on gosub selector zero falls through`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGosubStmt(pos, num "0", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("after"))

[<Test>]
let ``interpreter on gosub selector out of range falls through`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ OnGosubStmt(pos, num "3", [ num "100"; num "200" ]) ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("after"))

[<Test>]
let ``interpreter goto can jump out of for body`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ ForStmt(
                    pos,
                    "i",
                    num "1",
                    num "3",
                    None,
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "100") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter goto can jump into for body line block`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(pos, Some 15, [ GotoStmt(pos, num "30") ])
              Line(
                pos,
                Some 20,
                [ ForStmt(
                    pos,
                    "i",
                    num "1",
                    num "1",
                    None,
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "50") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter gosub from for body returns and loop continues`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "sum", num "0") ])
              Line(
                pos,
                Some 20,
                [ ForStmt(
                    pos,
                    "i",
                    num "1",
                    num "2",
                    None,
                    LineBlock
                        [ Line(pos, Some 30, [ GosubStmt(pos, num "100") ])
                          Line(pos, Some 40, [ Assignment(pos, id "sum", binary "+" (id "sum") (num "10")) ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "sum" ]) ])
              Line(pos, Some 60, [ GotoStmt(pos, num "200") ])
              Line(pos, Some 100, [ Assignment(pos, id "sum", binary "+" (id "sum") (id "i")) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("23"))

[<Test>]
let ``interpreter goto can jump out of repeat body`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ RepeatStmt(
                    pos,
                    "loop",
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "100") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter goto can jump into repeat body line block`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(pos, Some 15, [ GotoStmt(pos, num "30") ])
              Line(
                pos,
                Some 20,
                [ RepeatStmt(
                    pos,
                    "loop",
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "50") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter gosub from repeat body returns and loop can exit`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ RepeatStmt(
                    pos,
                    "loop",
                    LineBlock
                        [ Line(pos, Some 30, [ GosubStmt(pos, num "100") ])
                          Line(pos, Some 40, [ ExitStmt(pos, "loop") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ])
              Line(pos, Some 60, [ GotoStmt(pos, num "200") ])
              Line(pos, Some 100, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter goto can jump into if line block`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(pos, Some 15, [ GotoStmt(pos, num "30") ])
              Line(
                pos,
                Some 20,
                [ IfStmt(
                    pos,
                    num "1",
                    LineBlock [ Line(pos, Some 30, [ Assignment(pos, id "count", num "7") ]) ],
                    Some(LineBlock [ Line(pos, Some 40, [ Assignment(pos, id "count", num "9") ]) ])) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("7"))

[<Test>]
let ``interpreter goto can jump out of if line block`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ IfStmt(
                    pos,
                    num "1",
                    LineBlock
                        [ Line(pos, Some 30, [ Assignment(pos, id "count", num "1") ])
                          Line(pos, Some 40, [ GotoStmt(pos, num "100") ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ str "\"wrong\"" ]) ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("1"))

[<Test>]
let ``interpreter gosub from if body returns to branch flow`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "count", num "0") ])
              Line(
                pos,
                Some 20,
                [ IfStmt(
                    pos,
                    num "1",
                    LineBlock
                        [ Line(pos, Some 30, [ GosubStmt(pos, num "100") ])
                          Line(pos, Some 40, [ Assignment(pos, id "count", binary "+" (id "count") (num "10")) ]) ],
                    None) ])
              Line(pos, Some 50, [ ProcedureCall(pos, "PRINT", [ id "count" ]) ])
              Line(pos, Some 60, [ GotoStmt(pos, num "200") ])
              Line(pos, Some 100, [ Assignment(pos, id "count", binary "+" (id "count") (num "1")) ])
              Line(pos, Some 110, [ ReturnStmt(pos, None) ])
              Line(pos, Some 200, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("11"))

[<Test>]
let ``interpreter return after goto inside gosub still returns to caller`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ GosubStmt(pos, num "100") ])
              Line(pos, Some 20, [ ProcedureCall(pos, "PRINT", [ str "\"after\"" ]) ])
              Line(pos, Some 30, [ GotoStmt(pos, num "300") ])
              Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ str "\"sub\"" ]) ])
              Line(pos, Some 110, [ GotoStmt(pos, num "120") ])
              Line(pos, Some 120, [ ReturnStmt(pos, None) ])
              Line(pos, Some 300, [ Remark(pos, "done") ]) ])

    let output = runProgram ast

    Assert.That(String.concat "|" output, Is.EqualTo("sub|after"))

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
                    None) ])
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
                    None) ])
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
                    None) ])
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
    let inputs = Collections.Generic.Queue<string>([ "42" ])
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
              H.BuiltInCall(H.NamedBuiltIn "AT", Some channel0, [ H.Literal(H.ConstInt 3, H.HirType.Int, pos); H.Literal(H.ConstInt 4, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CURSOR", Some channel1, [ H.Literal(H.ConstInt 10, H.HirType.Int, pos); H.Literal(H.ConstInt 20, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "CSIZE", Some channel1, [ H.Literal(H.ConstInt 2, H.HirType.Int, pos); H.Literal(H.ConstInt 1, H.HirType.Int, pos) ], pos)
              H.BuiltInCall(H.NamedBuiltIn "INK", Some channel1, [ H.Literal(H.ConstInt 7, H.HirType.Int, pos) ], pos)
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
        Assert.That(window0.Cursor, Is.EqualTo((4, 3)))
        Assert.That(window0.CharacterSize, Is.EqualTo((0, 0)))
        Assert.That(window0.Ink, Is.EqualTo(None))
        Assert.That(window1.Cursor, Is.EqualTo((10, 20)))
        Assert.That(window1.CharacterSize, Is.EqualTo((2, 1)))
        Assert.That(window1.Ink, Is.EqualTo(Some 7))
        Assert.That(window1.Paper, Is.EqualTo(None))
        Assert.That(window2.CharacterSize, Is.EqualTo((4, 3)))
        Assert.That(window2.Paper, Is.EqualTo(Some 2))
        Assert.That(window2.Border, Is.EqualTo(Some 1))
        Assert.That(window2.Writes |> Seq.toList |> String.concat "|", Is.EqualTo("Enter"))
        Assert.That(window0.Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
        Assert.That(window1.Writes |> Seq.toList |> String.concat "|", Is.EqualTo(""))
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
let ``q3 fixture runtime completes sort check without inversion output`` () =
    let ast = parseAstFromFile "q3.SB"
    let hir = lowerProgram ast
    let outputs = ResizeArray<string>()
    let inputs = Collections.Generic.Queue<string>([ "25"; "" ])
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                    WriteLine = fun line -> outputs.Add(line)
                }
            Random = Random(1234)
            Clock = fun () -> DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc) }

    match interpretProgramWithOptions options hir with
    | Result.Ok result ->
        Assert.That(result.Output |> List.exists (fun line -> line.StartsWith("Error at")), Is.False)
    | Result.Error err ->
        Assert.Fail($"Expected q3 fixture interpretation to succeed, got %A{err}")
