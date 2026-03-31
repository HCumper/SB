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
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
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
    mutable Window: int * int * int * int
    mutable Scroll: int
    mutable Width: int option
    mutable Pan: int
    mutable Recolor: int option
    mutable Palette: int list option
    mutable Cursor: int * int
    mutable CharacterSize: int * int
    mutable ClearCount: int
    mutable Ink: int list option
    mutable Paper: int option
    mutable Border: int option
    Writes: ResizeArray<string>
}

type ScreenState = {
    mutable Mode: ScreenModeInfo
    Inputs: Collections.Generic.Queue<string>
    Windows: Map<int, ScreenWindowState>
    GraphicsOps: ResizeArray<string>
    mutable GraphicsInk: int list
    mutable FillMode: int
    mutable Scale: double * double * double
    mutable OverMode: int
    mutable UnderMode: int
    mutable FlashMode: int
    mutable PenDown: bool
    mutable Heading: double
}

let createScreenHost (inputs: string list) =
    let supportedModes =
        [ { Mode = QlMode4; Width = 512; Height = 256; Colors = Some 4; Name = "QL Mode 4"; IsQlCompatible = true }
          { Mode = QlMode8; Width = 256; Height = 256; Colors = Some 8; Name = "QL Mode 8"; IsQlCompatible = true }
          { Mode = ExtendedMode 256; Width = 256; Height = 256; Colors = Some 8; Name = "Extended Mode 256"; IsQlCompatible = false }
          { Mode = ExtendedMode 512; Width = 512; Height = 256; Colors = Some 4; Name = "Extended Mode 512"; IsQlCompatible = false } ]

    let applyModeToWindow (mode: ScreenModeInfo) (window: ScreenWindowState) =
        window.Cursor <- 0, 0
        window.Window <- mode.Width, mode.Height, 0, 0
        window.Scroll <- 0
        window.Width <- None
        window.Pan <- 0
        window.Recolor <- None
        window.Palette <- None
        window.CharacterSize <- 1, 1
        window.Ink <- Some [ 7 ]
        window.Paper <- Some 0
        window.Border <- Some 0

    let makeWindowState () =
        { Cursor = 0, 0
          Window = 0, 0, 0, 0
          Scroll = 0
          Width = None
          Pan = 0
          Recolor = None
          Palette = None
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
          Windows = windows
          GraphicsOps = ResizeArray<string>()
          GraphicsInk = []
          FillMode = 0
          Scale = 0.0, 0.0, 0.0
          OverMode = 0
          UnderMode = 0
          FlashMode = 0
          PenDown = true
          Heading = 0.0 }

    do
        for window in state.Windows.Values do
            applyModeToWindow state.Mode window

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
            member _.IsEndOfFile() = false
            member _.Flush() = ()
            member _.Close() = ()
            member _.Clear() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.ClearCount <- window.ClearCount + 1
                | None -> ()
            member _.SetWindow(width, height, x, y) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Window <- width, height, x, y
                | None -> ()
            member _.GetWindow() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Window
                | None -> 0, 0, 0, 0
            member _.SetScroll(value) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Scroll <- value
                | None -> ()
            member _.GetScroll() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Scroll
                | None -> 0
            member _.SetWidth(value) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Width <- Some value
                | None -> ()
            member _.GetWidth() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Width
                | None -> None
            member _.SetPan(value) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Pan <- value
                | None -> ()
            member _.GetPan() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Pan
                | None -> 0
            member _.SetRecolor(value) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Recolor <- Some value
                | None -> ()
            member _.GetRecolor() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Recolor
                | None -> None
            member _.SetPalette(values) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Palette <- Some values
                | None -> ()
            member _.GetPalette() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Palette
                | None -> None
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
            member _.SetInk(values: int list) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Ink <- Some values
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
            member _.IsEndOfFile() = false
            member _.Flush() = ()
            member _.Close() = ()
            member _.Clear() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.ClearCount <- window.ClearCount + 1
                | None -> ()
            member _.SetWindow(width, height, x, y) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Window <- width, height, x, y
                | None -> ()
            member _.GetWindow() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Window
                | None -> 0, 0, 0, 0
            member _.SetScroll(value) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Scroll <- value
                | None -> ()
            member _.GetScroll() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Scroll
                | None -> 0
            member _.SetWidth(value) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Width <- Some value
                | None -> ()
            member _.GetWidth() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Width
                | None -> None
            member _.SetPan(value) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Pan <- value
                | None -> ()
            member _.GetPan() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Pan
                | None -> 0
            member _.SetRecolor(value) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Recolor <- Some value
                | None -> ()
            member _.GetRecolor() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Recolor
                | None -> None
            member _.SetPalette(values) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Palette <- Some values
                | None -> ()
            member _.GetPalette() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Palette
                | None -> None
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
            member _.SetInk(values: int list) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Ink <- Some values
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
                    member _.OpenAs(_channelId: ChannelId, _name: string) = Result.Error(UnsupportedHostOperation "OpenAs not implemented in test host.")
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
                    member _.SetWindow(width, height, x, y) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Window <- width, height, x, y
                        | None -> ()
                    member _.GetWindow() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Window
                        | None -> 0, 0, 0, 0
                    member _.SetScroll(value) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Scroll <- value
                        | None -> ()
                    member _.GetScroll() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Scroll
                        | None -> 0
                    member _.SetWidth(value) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Width <- Some value
                        | None -> ()
                    member _.GetWidth() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Width
                        | None -> None
                    member _.SetPan(value) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Pan <- value
                        | None -> ()
                    member _.GetPan() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Pan
                        | None -> 0
                    member _.SetRecolor(value) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Recolor <- Some value
                        | None -> ()
                    member _.GetRecolor() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Recolor
                        | None -> None
                    member _.SetPalette(values) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Palette <- Some values
                        | None -> ()
                    member _.GetPalette() =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Palette
                        | None -> None
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
                    member _.SetInk(values: int list) =
                        match Map.tryFind 0 state.Windows with
                        | Some window -> window.Ink <- Some values
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
                            for window in state.Windows.Values do
                                applyModeToWindow selected window
                            Result.Ok()
                        | None ->
                            Result.Error(UnsupportedHostOperation $"Mode '{mode}' is not supported in the test host.") }
            member _.Graphics =
                { new IGraphicsDevice with
                    member _.Plot(x, y) = state.GraphicsOps.Add($"PLOT {x:G17},{y:G17}")
                    member _.Point(x, y) = state.GraphicsOps.Add($"POINT {x:G17},{y:G17}")
                    member _.PointRelative(dx, dy) = state.GraphicsOps.Add($"POINT_R {dx:G17},{dy:G17}")
                    member _.Draw(x, y) = state.GraphicsOps.Add($"DRAW {x:G17},{y:G17}")
                    member _.LineRelative(values) =
                        let valueText = values |> List.map (fun value -> value.ToString("G17")) |> String.concat ","
                        state.GraphicsOps.Add($"LINE_R {valueText}")
                    member _.DLine(values) =
                        let valueText = values |> List.map (fun value -> value.ToString("G17")) |> String.concat ","
                        state.GraphicsOps.Add($"DLINE {valueText}")
                    member _.Line(x1, y1, x2, y2) = state.GraphicsOps.Add($"LINE {x1:G17},{y1:G17} TO {x2:G17},{y2:G17}")
                    member _.Circle(x, y, radius) = state.GraphicsOps.Add($"CIRCLE {x:G17},{y:G17},{radius:G17}")
                    member _.CircleRelative(dx, dy, radius) = state.GraphicsOps.Add($"CIRCLE_R {dx:G17},{dy:G17},{radius:G17}")
                    member _.Ellipse(x, y, radius, ratio, angle) = state.GraphicsOps.Add($"ELLIPSE {x:G17},{y:G17},{radius:G17},{ratio:G17},{angle:G17}")
                    member _.EllipseRelative(dx, dy, radius, ratio, angle) = state.GraphicsOps.Add($"ELLIPSE_R {dx:G17},{dy:G17},{radius:G17},{ratio:G17},{angle:G17}")
                    member _.Arc(x, y, radius, startAngle, endAngle) = state.GraphicsOps.Add($"ARC {x:G17},{y:G17},{radius:G17},{startAngle:G17},{endAngle:G17}")
                    member _.ArcRelative(dx, dy, radius, startAngle, endAngle) = state.GraphicsOps.Add($"ARC_R {dx:G17},{dy:G17},{radius:G17},{startAngle:G17},{endAngle:G17}")
                    member _.Block(width, height, x, y, color) = state.GraphicsOps.Add($"BLOCK {width:G17},{height:G17},{x:G17},{y:G17},{color}")
                    member _.SetInk(values: int list) =
                        let valueText = values |> List.map string |> String.concat ","
                        state.GraphicsInk <- values
                        state.GraphicsOps.Add($"INK {valueText}")
                    member _.SetFill(value: int) =
                        state.FillMode <- value
                        state.GraphicsOps.Add($"FILL {value}")
                    member _.SetScale(x, y, z) =
                        state.Scale <- x, y, z
                        state.GraphicsOps.Add($"SCALE {x:G17},{y:G17},{z:G17}")
                    member _.SetOver(value: int) =
                        state.OverMode <- value
                        state.GraphicsOps.Add($"OVER {value}")
                    member _.SetUnder(value: int) =
                        state.UnderMode <- value
                        state.GraphicsOps.Add($"UNDER {value}")
                    member _.SetFlash(value: int) =
                        state.FlashMode <- value
                        state.GraphicsOps.Add($"FLASH {value}")
                    member _.SetPenDown(value: bool) =
                        state.PenDown <- value
                        state.GraphicsOps.Add(if value then "PENDOWN" else "PENUP")
                    member _.Turn(angle) =
                        state.Heading <- state.Heading + angle
                        state.GraphicsOps.Add($"TURN {angle:G17}")
                    member _.TurnTo(angle) =
                        state.Heading <- angle
                        state.GraphicsOps.Add($"TURNTO {angle:G17}")
                    member _.Clear() = state.GraphicsOps.Add("CLEAR") }
            member _.Input =
                { new IInputDevice with
                    member _.ReadLine() = reader ()
                    member _.ReadKey() = None
                    member _.KeyAvailable() = false
                    member _.GetKeyRow(_row) = 0 }
            member _.Sound =
                { new ISoundDevice with
                    member _.Beep(_pitch, _duration) = () }
            member _.Files =
                { new IDeviceFileSystem with
                    member _.OpenFile(_path, _mode) = Result.Error(UnsupportedHostOperation "Files not implemented in test host.")
                    member _.OpenFileAs(_channelId, _path, _mode) = Result.Error(UnsupportedHostOperation "Files not implemented in test host.")
                    member _.Exists(_path) = false
                    member _.Delete(_path) = Result.Error(UnsupportedHostOperation "Files not implemented in test host.") }
            member _.Environment =
                { new IEnvironmentProvider with
                    member _.GetVariable(_name) = None } }

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
