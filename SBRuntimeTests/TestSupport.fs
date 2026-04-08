module SBRuntimeTests.TestSupport

open System
open System.IO
open Antlr4.Runtime
open NUnit.Framework
open Serilog

open Types
open SyntaxAst
open CompilerPipeline
open AstToHir
open Interpreter
open ParseTreeVisitor
open SBRuntime

module H = HIR

let private testLogger =
    LoggerConfiguration().MinimumLevel.Fatal().CreateLogger()

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
                    ReadScreenLine = fun _ -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                }
            Random = Random(1234)
            Clock = fun () -> DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc)
            InitialSourceProgram = Some ast
            LoadProgram = loadRuntimeProgram Relaxed false testLogger
            MergeProgram =
                fun (currentAst, path) ->
                    loadRuntimeProgram Relaxed false testLogger path
                    |> Result.bind (fun loaded ->
                        let mergedAst = mergeRuntimeAst currentAst loaded.Ast
                        lowerAstForRuntime mergedAst
                        |> Result.map (fun lowered -> { Ast = mergedAst; Hir = lowered })) }

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
    mutable Recolor: int list option
    mutable Palette: int list option
    mutable Cursor: int * int
    mutable CharacterSize: int * int
    mutable CharacterFonts: int * int
    mutable ClearCount: int
    mutable Ink: int list option
    mutable Paper: int option
    mutable Strip: int list option
    mutable Border: int option
    Writes: ResizeArray<string>
}

type ScreenState = {
    mutable Mode: ScreenModeInfo
    Inputs: Collections.Generic.Queue<string>
    Memory: Collections.Generic.Dictionary<int, byte>
    Windows: Map<int, ScreenWindowState>
    mutable TextBuffer: char[,]
    mutable PixelBuffer: int[,]
    GraphicsOps: ResizeArray<string>
    Beeps: ResizeArray<int * int>
    mutable GraphicsInk: int list
    mutable FillMode: int
    mutable Scale: double * double * double
    mutable OverMode: int
    mutable UnderMode: int
    mutable FlashMode: int
    mutable PenDown: bool
    mutable Heading: double
    mutable Beeping: bool
    mutable DrawingWindow: int * int * int * int
    mutable DrawingPan: int
    mutable DrawingScale: double * double * double
}

let createScreenHost (inputs: string list) =
    let supportedModes =
        [ { Mode = QlMode4; Width = 512; Height = 256; Colors = Some 4; Name = "QL Mode 4"; IsQlCompatible = true; BaseTextCellWidth = 8; BaseTextCellHeight = 10; DefaultCharacterSize = 0, 0 }
          { Mode = QlMode8; Width = 256; Height = 256; Colors = Some 8; Name = "QL Mode 8"; IsQlCompatible = true; BaseTextCellWidth = 8; BaseTextCellHeight = 10; DefaultCharacterSize = 0, 0 }
          { Mode = ExtendedMode 256; Width = 256; Height = 256; Colors = Some 8; Name = "Extended Mode 256"; IsQlCompatible = false; BaseTextCellWidth = 8; BaseTextCellHeight = 8; DefaultCharacterSize = 0, 0 }
          { Mode = ExtendedMode 512; Width = 512; Height = 256; Colors = Some 4; Name = "Extended Mode 512"; IsQlCompatible = false; BaseTextCellWidth = 8; BaseTextCellHeight = 8; DefaultCharacterSize = 0, 0 } ]

    let applyModeToWindow (_channelId: int) (mode: ScreenModeInfo) (window: ScreenWindowState) =
        window.Cursor <- 0, 0
        window.Window <- mode.Width, mode.Height, 0, 0
        window.Scroll <- 0
        window.Width <- None
        window.Pan <- 0
        window.Recolor <- None
        window.Palette <- None
        window.CharacterSize <- mode.DefaultCharacterSize
        window.CharacterFonts <- 0, 0
        window.Ink <- Some [ 7 ]
        window.Paper <- Some 0
        window.Strip <- Some [ 0 ]
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
          CharacterFonts = 0, 0
          ClearCount = 0
          Ink = None
          Paper = None
          Strip = None
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
          Memory = Collections.Generic.Dictionary<int, byte>()
          Windows = windows
          TextBuffer = Array2D.create supportedModes[0].Height supportedModes[0].Width ' '
          PixelBuffer = Array2D.create supportedModes[0].Height supportedModes[0].Width 0
          GraphicsOps = ResizeArray<string>()
          Beeps = ResizeArray<int * int>()
          GraphicsInk = []
          FillMode = 0
          Scale = 0.0, 0.0, 0.0
          OverMode = 0
          UnderMode = 0
          FlashMode = 0
          PenDown = true
          Heading = 0.0
          Beeping = false
          DrawingWindow = supportedModes[0].Width, supportedModes[0].Height, 0, 0
          DrawingPan = 0
          DrawingScale = (100.0, 0.0, 0.0) }

    do
        for KeyValue(channelId, window) in state.Windows do
            applyModeToWindow channelId state.Mode window

    let resizeBuffers mode =
        state.TextBuffer <- Array2D.create mode.Height mode.Width ' '
        state.PixelBuffer <- Array2D.create mode.Height mode.Width 0

    let reader () =
        if state.Inputs.Count > 0 then Some(state.Inputs.Dequeue()) else None

    let tryReadByte address =
        if address < 0 then
            Result.Error(InvalidHostArgument $"Memory address {address} is invalid.")
        else
            match state.Memory.TryGetValue address with
            | true, value -> Result.Ok value
            | false, _ -> Result.Ok 0uy

    let writeByte address value =
        if address < 0 then
            Result.Error(InvalidHostArgument $"Memory address {address} is invalid.")
        else
            state.Memory[address] <- value
            Result.Ok()

    let peekWord address =
        tryReadByte address
        |> Result.bind (fun b0 ->
            tryReadByte (address + 1)
            |> Result.map (fun b1 -> int b0 ||| (int b1 <<< 8)))

    let peekLong address =
        tryReadByte address
        |> Result.bind (fun b0 ->
            tryReadByte (address + 1)
            |> Result.bind (fun b1 ->
                tryReadByte (address + 2)
                |> Result.bind (fun b2 ->
                    tryReadByte (address + 3)
                    |> Result.map (fun b3 -> int b0 ||| (int b1 <<< 8) ||| (int b2 <<< 16) ||| (int b3 <<< 24)))))

    let pokeWord address value =
        writeByte address (byte (value &&& 0xFF))
        |> Result.bind (fun () -> writeByte (address + 1) (byte ((value >>> 8) &&& 0xFF)))

    let pokeLong address value =
        writeByte address (byte (value &&& 0xFF))
        |> Result.bind (fun () -> writeByte (address + 1) (byte ((value >>> 8) &&& 0xFF)))
        |> Result.bind (fun () -> writeByte (address + 2) (byte ((value >>> 16) &&& 0xFF)))
        |> Result.bind (fun () -> writeByte (address + 3) (byte ((value >>> 24) &&& 0xFF)))

    let writeWindow channelId line =
        match Map.tryFind channelId state.Windows with
        | Some window ->
            window.Writes.Add(line)
            let width, _, x, y = window.Window
            let cursorX, cursorY = window.Cursor
            let row = y + cursorY
            if row >= 0 && row < Array2D.length1 state.TextBuffer then
                for index = 0 to line.Length - 1 do
                    let col = x + cursorX + index
                    if col >= x && col < min (Array2D.length2 state.TextBuffer) (x + width) then
                        state.TextBuffer[row, col] <- line[index]
            let nextCursorX = min (max 0 (width - 1)) (cursorX + line.Length)
            window.Cursor <- nextCursorX, cursorY
        | None -> ()

    let scaleFactors () =
        let sx, sy, _ = state.DrawingScale
        let factorX = if sx = 0.0 then 1.0 else sx / 100.0
        let factorY = if sy = 0.0 then factorX else sy / 100.0
        factorX, factorY

    let clampToWindow x y =
        let width, height, originX, originY = state.DrawingWindow
        let maxX = originX + max 0 (width - 1)
        let maxY = originY + max 0 (height - 1)
        let clampedX = min (float maxX) (max (float originX) x)
        let clampedY = min (float maxY) (max (float originY) y)
        clampedX, clampedY

    let setPixel x y color =
        if y >= 0 && y < Array2D.length1 state.PixelBuffer && x >= 0 && x < Array2D.length2 state.PixelBuffer then
            state.PixelBuffer[y, x] <- color

    let drawLine x1 y1 x2 y2 color =
        let dx = abs (x2 - x1)
        let sx = if x1 < x2 then 1 else -1
        let dy = -abs (y2 - y1)
        let sy = if y1 < y2 then 1 else -1
        let rec loop x y err =
            setPixel x y color
            if x <> x2 || y <> y2 then
                let e2 = 2 * err
                let nextX, nextErr =
                    if e2 >= dy then x + sx, err + dy else x, err
                let nextY, finalErr =
                    if e2 <= dx then y + sy, nextErr + dx else y, nextErr
                loop nextX nextY finalErr
        loop x1 y1 (dx + dy)

    let clearWindow width height x y =
        let maxX = min (Array2D.length2 state.TextBuffer) (x + max 0 width)
        let maxY = min (Array2D.length1 state.TextBuffer) (y + max 0 height)
        for row in max 0 y .. max 0 (maxY - 1) do
            for col in max 0 x .. max 0 (maxX - 1) do
                state.TextBuffer[row, col] <- ' '
                state.PixelBuffer[row, col] <- 0

    let scrollWindow width height x y =
        let maxX = min (Array2D.length2 state.TextBuffer) (x + max 0 width)
        let maxY = min (Array2D.length1 state.TextBuffer) (y + max 0 height)
        let startX = max 0 x
        let startY = max 0 y

        if maxX > startX && maxY > startY then
            for row in startY .. maxY - 2 do
                for col in startX .. maxX - 1 do
                    state.TextBuffer[row, col] <- state.TextBuffer[row + 1, col]
                    state.PixelBuffer[row, col] <- state.PixelBuffer[row + 1, col]

            for col in startX .. maxX - 1 do
                state.TextBuffer[maxY - 1, col] <- ' '
                state.PixelBuffer[maxY - 1, col] <- 0

    let newLineWindow channelId =
        match Map.tryFind channelId state.Windows with
        | Some window ->
            let width, height, x, y = window.Window
            let _, cursorY = window.Cursor
            if cursorY + 1 >= max 1 height then
                scrollWindow width height x y
                window.Cursor <- 0, max 0 (height - 1)
            else
                window.Cursor <- 0, cursorY + 1
        | None -> ()

    let transformAbsolute x y =
        let factorX, factorY = scaleFactors ()
        let _, _, originX, originY = state.DrawingWindow
        let transformedX = float originX + float state.DrawingPan + (x * factorX)
        let transformedY = float originY + (y * factorY)
        clampToWindow transformedX transformedY

    let transformDelta dx dy =
        let factorX, factorY = scaleFactors ()
        dx * factorX, dy * factorY

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
                | Some window ->
                    window.ClearCount <- window.ClearCount + 1
                    let width, height, x, y = window.Window
                    clearWindow width height x y
                    window.Cursor <- 0, 0
                | None -> ()
            member _.NewLine() = newLineWindow channelId
            member _.SetWindow(width, height, x, y) =
                match Map.tryFind channelId state.Windows with
                | Some window ->
                    window.Window <- width, height, x, y
                    window.Cursor <- 0, 0
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
            member _.SetRecolor(values) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Recolor <- Some values
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
            member _.SetCharacterFonts(font1, font2) =
                match Map.tryFind channelId state.Windows with
                | Some window ->
                    let currentFont1, currentFont2 = window.CharacterFonts
                    window.CharacterFonts <- (if font1 = -1 then currentFont1 else font1), (if font2 = -1 then currentFont2 else font2)
                | None -> ()
            member _.GetCharacterFonts() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.CharacterFonts
                | None -> 0, 0
            member _.SetInk(values: int list) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Ink <- Some values
                | None -> ()
            member _.SetPaper(value: int) =
                match Map.tryFind channelId state.Windows with
                | Some window ->
                    window.Paper <- Some value
                    window.Strip <- Some [ value ]
                | None -> ()
            member _.SetStrip(values: int list) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Strip <- Some values
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
                | Some window ->
                    window.ClearCount <- window.ClearCount + 1
                    let width, height, x, y = window.Window
                    clearWindow width height x y
                    window.Cursor <- 0, 0
                | None -> ()
            member _.NewLine() = newLineWindow channelId
            member _.SetWindow(width, height, x, y) =
                match Map.tryFind channelId state.Windows with
                | Some window ->
                    window.Window <- width, height, x, y
                    window.Cursor <- 0, 0
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
            member _.SetRecolor(values) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Recolor <- Some values
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
            member _.SetCharacterFonts(font1, font2) =
                match Map.tryFind channelId state.Windows with
                | Some window ->
                    let currentFont1, currentFont2 = window.CharacterFonts
                    window.CharacterFonts <- (if font1 = -1 then currentFont1 else font1), (if font2 = -1 then currentFont2 else font2)
                | None -> ()
            member _.GetCharacterFonts() =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.CharacterFonts
                | None -> 0, 0
            member _.SetInk(values: int list) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Ink <- Some values
                | None -> ()
            member _.SetPaper(value: int) =
                match Map.tryFind channelId state.Windows with
                | Some window ->
                    window.Paper <- Some value
                    window.Strip <- Some [ value ]
                | None -> ()
            member _.SetStrip(values: int list) =
                match Map.tryFind channelId state.Windows with
                | Some window -> window.Strip <- Some values
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
                        match Map.tryFind 1 state.Windows with
                        | Some window ->
                            window.ClearCount <- window.ClearCount + 1
                            let width, height, x, y = window.Window
                            clearWindow width height x y
                            window.Cursor <- 0, 0
                        | None -> ()
                    member _.NewLine() = newLineWindow 1
                    member _.SetWindow(width, height, x, y) =
                        match Map.tryFind 1 state.Windows with
                        | Some window ->
                            window.Window <- width, height, x, y
                            window.Cursor <- 0, 0
                        | None -> ()
                    member _.GetWindow() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Window
                        | None -> 0, 0, 0, 0
                    member _.SetScroll(value) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Scroll <- value
                        | None -> ()
                    member _.GetScroll() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Scroll
                        | None -> 0
                    member _.SetWidth(value) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Width <- Some value
                        | None -> ()
                    member _.GetWidth() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Width
                        | None -> None
                    member _.SetPan(value) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Pan <- value
                        | None -> ()
                    member _.GetPan() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Pan
                        | None -> 0
                    member _.SetRecolor(values) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Recolor <- Some values
                        | None -> ()
                    member _.GetRecolor() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Recolor
                        | None -> None
                    member _.SetPalette(values) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Palette <- Some values
                        | None -> ()
                    member _.GetPalette() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Palette
                        | None -> None
                    member _.SetCursor(x, y) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Cursor <- x, y
                        | None -> ()
                    member _.GetCursor() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Cursor
                        | None -> 0, 0
                    member _.SetCharacterSize(width, height) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.CharacterSize <- width, height
                        | None -> ()
                    member _.GetCharacterSize() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.CharacterSize
                        | None -> 0, 0
                    member _.SetCharacterFonts(font1, font2) =
                        match Map.tryFind 1 state.Windows with
                        | Some window ->
                            let currentFont1, currentFont2 = window.CharacterFonts
                            window.CharacterFonts <- (if font1 = -1 then currentFont1 else font1), (if font2 = -1 then currentFont2 else font2)
                        | None -> ()
                    member _.GetCharacterFonts() =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.CharacterFonts
                        | None -> 0, 0
                    member _.WriteText text = writeWindow 1 text
                    member _.SetInk(values: int list) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Ink <- Some values
                        | None -> ()
                    member _.SetPaper(value: int) =
                        match Map.tryFind 1 state.Windows with
                        | Some window ->
                            window.Paper <- Some value
                            window.Strip <- Some [ value ]
                        | None -> ()
                    member _.SetStrip(values: int list) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Strip <- Some values
                        | None -> ()
                    member _.SetBorder(value: int) =
                        match Map.tryFind 1 state.Windows with
                        | Some window -> window.Border <- Some value
                        | None -> ()
                    member _.GetSupportedModes() = supportedModes
                    member _.GetMode() = state.Mode
                    member _.SetMode(mode: ScreenMode) =
                        match supportedModes |> List.tryFind (fun candidate -> candidate.Mode = mode) with
                        | Some selected ->
                            state.Mode <- selected
                            resizeBuffers selected
                            for KeyValue(channelId, window) in state.Windows do
                                applyModeToWindow channelId selected window
                            Result.Ok()
                        | None ->
                            Result.Error(UnsupportedHostOperation $"Mode '{mode}' is not supported in the test host.") }
            member _.Graphics =
                { new IGraphicsDevice with
                    member _.SetDrawingContext(window, pan, scale) =
                        state.DrawingWindow <- window
                        state.DrawingPan <- pan
                        state.DrawingScale <- scale
                    member _.Plot(x, y) =
                        let tx, ty = transformAbsolute x y
                        state.GraphicsOps.Add($"PLOT {tx:G17},{ty:G17}")
                        setPixel (int (Math.Round tx)) (int (Math.Round ty)) (state.GraphicsInk |> List.tryHead |> Option.defaultValue 7)
                    member _.Point(x, y) =
                        let tx, ty = transformAbsolute x y
                        state.GraphicsOps.Add($"POINT {tx:G17},{ty:G17}")
                        setPixel (int (Math.Round tx)) (int (Math.Round ty)) (state.GraphicsInk |> List.tryHead |> Option.defaultValue 7)
                    member _.PointRelative(dx, dy) =
                        let tx, ty = transformDelta dx dy
                        state.GraphicsOps.Add($"POINT_R {tx:G17},{ty:G17}")
                        setPixel (int (Math.Round tx)) (int (Math.Round ty)) (state.GraphicsInk |> List.tryHead |> Option.defaultValue 7)
                    member _.Draw(x, y) =
                        let tx, ty = transformAbsolute x y
                        state.GraphicsOps.Add($"DRAW {tx:G17},{ty:G17}")
                        drawLine 0 0 (int (Math.Round tx)) (int (Math.Round ty)) (state.GraphicsInk |> List.tryHead |> Option.defaultValue 7)
                    member _.LineRelative(values) =
                        let transformed =
                            values
                            |> List.chunkBySize 2
                            |> List.collect (function
                                | [ dx; dy ] ->
                                    let tx, ty = transformDelta dx dy
                                    [ tx; ty ]
                                | leftovers -> leftovers)
                        let valueText = transformed |> List.map (fun value -> value.ToString("G17")) |> String.concat ","
                        state.GraphicsOps.Add($"LINE_R {valueText}")
                    member _.DLine(values) =
                        let transformed =
                            values
                            |> List.chunkBySize 2
                            |> List.collect (function
                                | [ x; y ] ->
                                    let tx, ty = transformAbsolute x y
                                    [ tx; ty ]
                                | leftovers -> leftovers)
                        let valueText = transformed |> List.map (fun value -> value.ToString("G17")) |> String.concat ","
                        state.GraphicsOps.Add($"DLINE {valueText}")
                    member _.Line(x1, y1, x2, y2) =
                        let tx1, ty1 = transformAbsolute x1 y1
                        let tx2, ty2 = transformAbsolute x2 y2
                        state.GraphicsOps.Add($"LINE {tx1:G17},{ty1:G17} TO {tx2:G17},{ty2:G17}")
                        drawLine (int (Math.Round tx1)) (int (Math.Round ty1)) (int (Math.Round tx2)) (int (Math.Round ty2)) (state.GraphicsInk |> List.tryHead |> Option.defaultValue 7)
                    member _.Circle(x, y, radius) =
                        let tx, ty = transformAbsolute x y
                        state.GraphicsOps.Add($"CIRCLE {tx:G17},{ty:G17},{radius:G17}")
                        setPixel (int (Math.Round tx)) (int (Math.Round ty)) (state.GraphicsInk |> List.tryHead |> Option.defaultValue 7)
                    member _.CircleRelative(dx, dy, radius) =
                        let tx, ty = transformDelta dx dy
                        state.GraphicsOps.Add($"CIRCLE_R {tx:G17},{ty:G17},{radius:G17}")
                    member _.Ellipse(x, y, radius, ratio, angle) =
                        let tx, ty = transformAbsolute x y
                        state.GraphicsOps.Add($"ELLIPSE {tx:G17},{ty:G17},{radius:G17},{ratio:G17},{angle:G17}")
                        setPixel (int (Math.Round tx)) (int (Math.Round ty)) (state.GraphicsInk |> List.tryHead |> Option.defaultValue 7)
                    member _.EllipseRelative(dx, dy, radius, ratio, angle) =
                        let tx, ty = transformDelta dx dy
                        state.GraphicsOps.Add($"ELLIPSE_R {tx:G17},{ty:G17},{radius:G17},{ratio:G17},{angle:G17}")
                    member _.Arc(x, y, radius, startAngle, endAngle) =
                        let tx, ty = transformAbsolute x y
                        state.GraphicsOps.Add($"ARC {tx:G17},{ty:G17},{radius:G17},{startAngle:G17},{endAngle:G17}")
                        setPixel (int (Math.Round tx)) (int (Math.Round ty)) (state.GraphicsInk |> List.tryHead |> Option.defaultValue 7)
                    member _.ArcRelative(dx, dy, radius, startAngle, endAngle) =
                        let tx, ty = transformDelta dx dy
                        state.GraphicsOps.Add($"ARC_R {tx:G17},{ty:G17},{radius:G17},{startAngle:G17},{endAngle:G17}")
                    member _.Block(width, height, x, y, color) =
                        let tx, ty = transformAbsolute x y
                        state.GraphicsOps.Add($"BLOCK {width:G17},{height:G17},{tx:G17},{ty:G17},{color}")
                        for row = int (Math.Round ty) to int (Math.Round ty) + max 0 (int (Math.Round height)) - 1 do
                            for col = int (Math.Round tx) to int (Math.Round tx) + max 0 (int (Math.Round width)) - 1 do
                                setPixel col row color
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
                    member _.Clear() =
                        state.GraphicsOps.Add("CLEAR")
                        let width, height, x, y = state.DrawingWindow
                        clearWindow width height x y }
            member _.Input =
                { new IInputDevice with
                    member _.ReadLine() = reader ()
                    member _.ReadKey() = None
                    member _.KeyAvailable() = false
                    member _.GetKeyRow(_row) = 0
                    member _.Flush() =
                        while state.Inputs.Count > 0 do
                            state.Inputs.Dequeue() |> ignore }
            member _.Sound =
                { new ISoundDevice with
                    member _.Beep(pitch, duration) =
                        state.Beeps.Add(pitch, duration)
                        state.Beeping <- duration > 0
                    member _.IsBeeping() = state.Beeping }
            member _.Files =
                { new IDeviceFileSystem with
                    member _.OpenFile(_path, _mode) = Result.Error(UnsupportedHostOperation "Files not implemented in test host.")
                    member _.OpenFileAs(_channelId, _path, _mode) = Result.Error(UnsupportedHostOperation "Files not implemented in test host.")
                    member _.ListDirectory(_pathSpec) = Result.Error(UnsupportedHostOperation "Directory listing not implemented in test host.")
                    member _.Exists(_path) = false
                    member _.Delete(_path) = Result.Error(UnsupportedHostOperation "Files not implemented in test host.") }
            member _.Environment =
                { new IEnvironmentProvider with
                    member _.GetVariable(_name) = None }
            member _.Memory =
                { new IMemoryDevice with
                    member _.Peek8(address) =
                        tryReadByte address |> Result.map int
                    member _.Peek16(address) =
                        peekWord address
                    member _.Peek32(address) =
                        peekLong address
                    member _.Poke8(address, value) =
                        writeByte address (byte (value &&& 0xFF))
                    member _.Poke16(address, value) =
                        pokeWord address value
                    member _.Poke32(address, value) =
                        pokeLong address value } }

    host, state

let makeStorage symbolId slotId name hirType storageClass : H.HirStorage =
    { Symbol = symbolId
      Slot = H.StorageSlotId slotId
      Name = name
      Type = hirType
      Dimensions = None
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
