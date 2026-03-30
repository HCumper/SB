module SBRuntimeTests.TestSupport

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

let pos =
    { BasicLineNo = None
      EditorLineNo = 1
      Column = 0 }

let num value = mkNumberLiteral pos value
let str value = mkStringLiteral pos value
let id name = mkIdentifier pos name
let call name args = mkPostfixName pos name (Some args)
let binary op lhs rhs = mkBinaryExpr pos op lhs rhs

let fixturePath fileName =
    let candidates =
        [ Path.Combine(__SOURCE_DIRECTORY__, "..", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", Path.GetFileName(fileName)) ]
        |> List.map Path.GetFullPath

    match candidates |> List.tryFind File.Exists with
    | Some path -> path
    | None -> failwith $"Fixture file not found: {fileName}"

let parseAstFromFile fileName =
    let input = File.ReadAllText(fixturePath fileName)
    let inputStream = AntlrInputStream(input)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)

    parser.program()
    |> convertTreeToAst
    |> List.head

let lowerProgram ast =
    let analyzed = runSemanticAnalysis ast
    match lowerToHir analyzed with
    | Result.Ok hir -> hir
    | Result.Error errs -> Assert.Fail($"Expected HIR lowering to succeed, got %A{errs}"); Unchecked.defaultof<_>

let runProgramWithInput input ast =
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

let runProgram ast =
    runProgramWithInput [] ast

type ScreenWindowState = {
    mutable Cursor: int * int
    mutable CharacterSize: int * int
    mutable ClearCount: int
    mutable Ink: int option
    mutable Paper: int option
    mutable Border: int option
    Writes: ResizeArray<string>
}

type ScreenState = {
    mutable Mode: ScreenModeInfo
    Inputs: Collections.Generic.Queue<string>
    Windows: Map<int, ScreenWindowState>
}

let createScreenHost (inputs: string list) =
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

let makeStorage symbolId slotId name hirType storageClass : H.HirStorage =
    { Symbol = symbolId
      Slot = H.StorageSlotId slotId
      Name = name
      Type = hirType
      Class = storageClass
      Position = pos }

let makeProgram symbolNames globals main : H.HirProgram =
    { SymbolNames = symbolNames
      Globals = globals
      Routines = []
      DataEntries = []
      RestorePoints = []
      Main = main }

let runHirProgramWithOptions options hir =
    match interpretProgramWithOptions options hir with
    | Result.Ok result -> result.Output
    | Result.Error err -> Assert.Fail($"Expected interpretation to succeed, got %A{err}"); []

let assertRuntimeError (expectedCode: RuntimeErrorCode) result =
    match result with
    | Result.Ok _ ->
        Assert.Fail($"Expected runtime error {expectedCode} but interpretation succeeded")
        Unchecked.defaultof<RuntimeError>
    | Result.Error err ->
        Assert.That(err.Code, Is.EqualTo(expectedCode))
        Assert.That(err.Message, Does.StartWith($"Runtime error [{expectedCode}]"))
        err
