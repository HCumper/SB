namespace SBRuntime

open System
open System.IO
open System.Collections.Generic

// Minimal default host implementation for console-style execution and tests.

type DefaultHostOptions = {
    ReadLine: unit -> string option
    ReadKey: unit -> KeyInfo option
    KeyAvailable: unit -> bool
    KeyRowState: int -> int
    WriteLine: string -> unit
}

module private ScreenDefaults =
    let defaultPaperForChannel channelNumber =
        match channelNumber with
        | 1 -> 2
        | _ -> 0

    let defaultWindowForChannel (mode: ScreenModeInfo) channelNumber =
        let safeWidth =
            if mode.Width >= 512 then 448 else mode.Width

        let safeX =
            if mode.Width >= 512 then (mode.Width - safeWidth) / 2 else 0

        let bottomHeight =
            if mode.Height >= 256 then 40
            else max 24 (mode.Height / 6)

        let topHeight = max 1 (mode.Height - bottomHeight)

        match channelNumber with
        | 0 -> safeWidth, bottomHeight, safeX, mode.Height - bottomHeight
        | 1
        | 2 -> safeWidth, topHeight, safeX, 0
        | _ -> mode.Width, mode.Height, 0, 0

    let defaultWindowForMode (mode: ScreenModeInfo) =
        defaultWindowForChannel mode 0

    let defaultCharacterSizeForMode (_mode: ScreenModeInfo) =
        1, 1

    let supportedModes =
        [ { Mode = QlMode4; Width = 512; Height = 256; Colors = Some 4; Name = "QL Mode 4"; IsQlCompatible = true }
          { Mode = QlMode8; Width = 256; Height = 256; Colors = Some 8; Name = "QL Mode 8"; IsQlCompatible = true }
          { Mode = ExtendedMode 256; Width = 256; Height = 256; Colors = Some 8; Name = "Extended Mode 256"; IsQlCompatible = false }
          { Mode = ExtendedMode 512; Width = 512; Height = 256; Colors = Some 4; Name = "Extended Mode 512"; IsQlCompatible = false } ]

    let paneMode (mode: ScreenModeInfo) width height =
        { mode with
            Width = max 1 width
            Height = max 1 height }

type private TextCell = {
    mutable Character: char
    mutable Ink: int
    mutable Paper: int
}

type private ScreenBuffer(mode: ScreenModeInfo) =
    let createText width height =
        Array2D.init height width (fun _ _ -> { Character = ' '; Ink = 7; Paper = 0 } : TextCell)

    let mutable currentMode = mode
    let mutable text = createText mode.Width mode.Height
    let mutable pixels = Array2D.create mode.Height mode.Width 0

    member _.Mode = currentMode
    member _.Text = text
    member _.Pixels = pixels
    member _.Snapshot() =
        let textSnapshot: ScreenTextCell[,] =
            Array2D.init currentMode.Height currentMode.Width (fun row col ->
                let cell = text[row, col]
                { Character = cell.Character
                  Ink = cell.Ink
                  Paper = cell.Paper })

        let pixelSnapshot =
            Array2D.init currentMode.Height currentMode.Width (fun row col -> pixels[row, col])

        { Mode = currentMode
          Panes =
            [ { ChannelId = None
                Title = "screen"
                Kind = ScreenChannel
                Window = 0, 0, 0, 0
                Cursor = 0, 0
                Text = textSnapshot
                Pixels = pixelSnapshot } ] }
    member _.Resize(newMode: ScreenModeInfo) =
        currentMode <- newMode
        text <- createText newMode.Width newMode.Height
        pixels <- Array2D.create newMode.Height newMode.Width 0
    member _.ClearWindow(width: int, height: int, x: int, y: int, paper: int) =
        let maxX = min currentMode.Width (x + max 0 width)
        let maxY = min currentMode.Height (y + max 0 height)
        for row in max 0 y .. max 0 (maxY - 1) do
            for col in max 0 x .. max 0 (maxX - 1) do
                text[row, col] <- { Character = ' '; Ink = 7; Paper = paper }
                pixels[row, col] <- paper
    member _.ScrollTextWindow(width: int, height: int, x: int, y: int, paper: int) =
        let maxX = min currentMode.Width (x + max 0 width)
        let maxY = min currentMode.Height (y + max 0 height)
        let startX = max 0 x
        let startY = max 0 y

        if maxX > startX && maxY > startY then
            for row in startY .. maxY - 2 do
                for col in startX .. maxX - 1 do
                    let nextCell = text[row + 1, col]
                    text[row, col] <- { Character = nextCell.Character; Ink = nextCell.Ink; Paper = nextCell.Paper }
                    pixels[row, col] <- pixels[row + 1, col]

            for col in startX .. maxX - 1 do
                text[maxY - 1, col] <- { Character = ' '; Ink = 7; Paper = paper }
                pixels[maxY - 1, col] <- paper
    member _.WriteText(windowWidth: int, windowHeight: int, windowX: int, windowY: int, cursorX: int, cursorY: int, ink: int, paper: int, content: string) =
        let row = windowY + cursorY
        if row >= 0 && row < currentMode.Height then
            for index = 0 to content.Length - 1 do
                let col = windowX + cursorX + index
                if col >= windowX && col < min currentMode.Width (windowX + windowWidth) then
                    text[row, col] <- { Character = content[index]; Ink = ink; Paper = paper }
    member _.SetPixel(x: int, y: int, color: int) =
        if x >= 0 && x < currentMode.Width && y >= 0 && y < currentMode.Height then
            pixels[y, x] <- color
    member this.DrawLine(x1: int, y1: int, x2: int, y2: int, color: int) =
        let dx = abs (x2 - x1)
        let sx = if x1 < x2 then 1 else -1
        let dy = -abs (y2 - y1)
        let sy = if y1 < y2 then 1 else -1
        let rec loop x y err =
            this.SetPixel(x, y, color)
            if x <> x2 || y <> y2 then
                let e2 = 2 * err
                let nextX, nextErr =
                    if e2 >= dy then x + sx, err + dy else x, err
                let nextY, finalErr =
                    if e2 <= dx then y + sy, nextErr + dx else y, nextErr
                loop nextX nextY finalErr
        loop x1 y1 (dx + dy)

type private ScreenChannelConfig = {
    Window: (int * int * int * int) option
    Border: int option
}

module private ScreenDeviceStrings =
    let private tryParsePositiveInt (text: string) =
        match Int32.TryParse(text) with
        | true, value when value >= 0 -> Some value
        | _ -> None

    let private tryParsePair (text: string) =
        let pieces = text.Split('X', StringSplitOptions.RemoveEmptyEntries)
        if pieces.Length = 2 then
            match tryParsePositiveInt pieces[0], tryParsePositiveInt pieces[1] with
            | Some left, Some right -> Some(left, right)
            | _ -> None
        else
            None

    let tryParseScreenChannelConfig (normalizedName: string) =
        let trimmed =
            if normalizedName.StartsWith("CON_") then
                normalizedName.Substring(4)
            elif normalizedName.StartsWith("SCR_") then
                normalizedName.Substring(4)
            else
                String.Empty

        if String.IsNullOrEmpty trimmed then
            Some { Window = None; Border = None }
        else
            let segments = trimmed.Split('_')

            let geometryAndOrigin, borderText =
                match segments with
                | [| geometry |] -> geometry, None
                | [| geometry; border |] -> geometry, if String.IsNullOrWhiteSpace border then None else Some border
                | _ -> String.Empty, None

            let geometryText, originText =
                if geometryAndOrigin.Contains('A') then
                    let parts = geometryAndOrigin.Split('A')
                    if parts.Length = 2 then parts[0], Some parts[1] else geometryAndOrigin, None
                else
                    geometryAndOrigin, None

            let window =
                match String.IsNullOrWhiteSpace geometryText, originText with
                | true, None -> Some None
                | _ ->
                    match
                        (if String.IsNullOrWhiteSpace geometryText then Some(0, 0) else tryParsePair geometryText),
                        (match originText with
                         | None -> Some(0, 0)
                         | Some value -> tryParsePair value)
                    with
                    | Some(width, height), Some(x, y) -> Some(Some(width, height, x, y))
                    | _ -> None

            let border =
                match borderText with
                | None -> Some None
                | Some value -> tryParsePositiveInt value |> Option.map Some

            match window, border with
            | Some parsedWindow, Some parsedBorder ->
                Some { Window = parsedWindow; Border = parsedBorder }
            | _ -> None

type private DefaultChannel(id: ChannelId, kind: ChannelKind, reader: unit -> string option, writer: string -> unit) =
    interface IChannel with
        member _.Id = id
        member _.Kind = kind
        member _.WriteText text = writer text
        member _.ReadText() = reader ()
        member _.IsEndOfFile() = false
        member _.Flush() = ()
        member _.Close() = ()

type private NullChannel(id: ChannelId) =
    interface IChannel with
        member _.Id = id
        member _.Kind = NamedChannel "NUL"
        member _.WriteText(_text) = ()
        member _.ReadText() = None
        member _.IsEndOfFile() = true
        member _.Flush() = ()
        member _.Close() = ()

type private DefaultPrinterChannel(id: ChannelId, writer: string -> unit) =
    interface IChannel with
        member _.Id = id
        member _.Kind = PrinterChannel
        member _.WriteText text = writer text
        member _.ReadText() = None
        member _.IsEndOfFile() = true
        member _.Flush() = ()
        member _.Close() = ()

type private DefaultScreenChannel(id: ChannelId, kind: ChannelKind, reader: unit -> string option, writer: string -> unit, mirrorToWriter: bool, initialMode: ScreenModeInfo, config: ScreenChannelConfig option, buffer: ScreenBuffer) =
    let (ChannelId channelNumber) = id
    let mutable window = 0, 0, 0, 0
    let mutable scroll = 0
    let mutable width = None
    let mutable pan = 0
    let mutable recolor = None
    let mutable palette = None
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable ink = [ 7 ]
    let mutable paper = 0
    let mutable border = 0
    let mutable mode = initialMode
    let mutable paneBuffer = ScreenBuffer(ScreenDefaults.paneMode initialMode 1 1)

    let resetForMode (selectedMode: ScreenModeInfo) =
        mode <- selectedMode
        window <- ScreenDefaults.defaultWindowForChannel selectedMode channelNumber
        scroll <- 0
        width <- None
        pan <- 0
        recolor <- None
        palette <- None
        cursor <- 0, 0
        characterSize <- ScreenDefaults.defaultCharacterSizeForMode selectedMode
        ink <- [ 7 ]
        paper <- ScreenDefaults.defaultPaperForChannel channelNumber
        border <- 0
        match config with
        | Some channelConfig ->
            match channelConfig.Window with
            | Some(width, height, x, y) -> window <- width, height, x, y
            | None -> ()
            match channelConfig.Border with
            | Some configuredBorder -> border <- configuredBorder
            | None -> ()
        | None -> ()
        let width, height, _, _ = window
        paneBuffer <- ScreenBuffer(ScreenDefaults.paneMode selectedMode width height)
        paneBuffer.ClearWindow(width, height, 0, 0, paper)

    do
        resetForMode initialMode

    interface IScreenChannel with
        member _.Id = id
        member _.Kind = kind
        member _.WriteText text =
            let width, height, x, y = window
            let cursorX, cursorY = cursor
            let foreground = ink |> List.tryHead |> Option.defaultValue 7
            buffer.WriteText(width, height, x, y, cursorX, cursorY, foreground, paper, text)
            paneBuffer.WriteText(width, height, 0, 0, cursorX, cursorY, foreground, paper, text)
            let nextCursorX = min (max 0 (width - 1)) (cursorX + text.Length)
            cursor <- nextCursorX, cursorY
            if mirrorToWriter then
                writer text
        member _.ReadText() = reader ()
        member _.IsEndOfFile() = false
        member _.Flush() = ()
        member _.Close() = ()
        member _.Clear() =
            let width, height, x, y = window
            buffer.ClearWindow(width, height, x, y, paper)
            paneBuffer.ClearWindow(width, height, 0, 0, paper)
            cursor <- 0, 0
        member _.NewLine() =
            let width, height, x, y = window
            let _, cursorY = cursor
            if cursorY + 1 >= max 1 height then
                buffer.ScrollTextWindow(width, height, x, y, paper)
                paneBuffer.ScrollTextWindow(width, height, 0, 0, paper)
                cursor <- 0, max 0 (height - 1)
            else
                cursor <- 0, cursorY + 1
        member _.SetWindow(width, height, x, y) =
            window <- width, height, x, y
            paneBuffer <- ScreenBuffer(ScreenDefaults.paneMode mode width height)
            cursor <- 0, 0
        member _.GetWindow() = window
        member _.SetScroll(value) = scroll <- value
        member _.GetScroll() = scroll
        member _.SetWidth(value) = width <- Some value
        member _.GetWidth() = width
        member _.SetPan(value) = pan <- value
        member _.GetPan() = pan
        member _.SetRecolor(value) = recolor <- Some value
        member _.GetRecolor() = recolor
        member _.SetPalette(values) = palette <- Some values
        member _.GetPalette() = palette
        member _.SetCursor(x, y) = cursor <- x, y
        member _.GetCursor() = cursor
        member _.SetCharacterSize(width, height) = characterSize <- width, height
        member _.GetCharacterSize() = characterSize
        member _.SetInk(values: int list) = ink <- values
        member _.SetPaper(value: int) = paper <- value
        member _.SetBorder(value: int) = border <- value
    member this.ApplyMode(mode: ScreenModeInfo) =
        resetForMode mode
    member _.Snapshot() =
        let paneSnapshot = paneBuffer.Snapshot().Panes.Head
        { paneSnapshot with
            ChannelId = Some channelNumber
            Title = $"#{channelNumber}"
            Kind = kind
            Window = window
            Cursor = cursor }

type private DefaultFileChannel(id: ChannelId, path: string, mode: FileOpenMode) =
    let sharedStream, reader, writer =
        match mode with
        | OpenForInput ->
            None, Some(new StreamReader(File.Open(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))), None
        | OpenForOutput ->
            None, None, Some(new StreamWriter(File.Open(path, FileMode.Create, FileAccess.Write, FileShare.Read)))
        | OpenForAppend ->
            None, None, Some(new StreamWriter(File.Open(path, FileMode.Append, FileAccess.Write, FileShare.Read)))
        | OpenForUpdate ->
            let stream = File.Open(path, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)
            Some stream, Some(new StreamReader(stream)), Some(new StreamWriter(stream))

    interface IChannel with
        member _.Id = id
        member _.Kind = FileChannel
        member _.WriteText text =
            match writer with
            | Some streamWriter ->
                streamWriter.WriteLine(text)
                streamWriter.Flush()
            | None -> ()
        member _.ReadText() =
            match reader with
            | Some streamReader when not streamReader.EndOfStream -> Some(streamReader.ReadLine())
            | _ -> None
        member _.IsEndOfFile() =
            match reader with
            | Some streamReader -> streamReader.EndOfStream
            | None -> false
        member _.Flush() =
            match writer with
            | Some streamWriter -> streamWriter.Flush()
            | None -> ()
        member _.Close() =
            match sharedStream with
            | Some stream ->
                stream.Dispose()
            | None ->
                match reader with
                | Some streamReader -> streamReader.Dispose()
                | None -> ()

                match writer with
                | Some streamWriter -> streamWriter.Dispose()
                | None -> ()

type private DefaultChannelManager(defaultChannels: IChannel list, reader: unit -> string option, writer: string -> unit, currentMode: unit -> ScreenModeInfo, buffer: ScreenBuffer) as this =
    let channels = Dictionary<ChannelId, IChannel>()
    let fixedChannels = HashSet<ChannelId>()
    let screenChannels = Dictionary<ChannelId, DefaultScreenChannel>()

    do
        for channel in defaultChannels do
            channels[channel.Id] <- channel
            fixedChannels.Add(channel.Id) |> ignore
            match channel with
            | :? DefaultScreenChannel as screenChannel -> screenChannels[channel.Id] <- screenChannel
            | _ -> ()

    let tryResolveDirectoryBackedPath (normalized: string) =
        let knownPrefixes = [ "RAM"; "FLP"; "WIN"; "MDV" ]
        let prefixMatch =
            knownPrefixes
            |> List.tryPick (fun prefix ->
                if normalized.StartsWith(prefix) then
                    let suffix = normalized.Substring(prefix.Length)
                    let digitCount = suffix |> Seq.takeWhile Char.IsDigit |> Seq.length
                    if digitCount > 0 && suffix.Length > digitCount && suffix[digitCount] = '_' then
                        let device = normalized.Substring(0, prefix.Length + digitCount)
                        let rest = normalized.Substring(prefix.Length + digitCount + 1)
                        if String.IsNullOrWhiteSpace rest then None else Some(device, rest)
                    else
                        None
                else
                    None)

        prefixMatch
        |> Option.map (fun (device, leafName) ->
            let root = Path.Combine(Directory.GetCurrentDirectory(), "RuntimeDevices", device.ToLowerInvariant())
            Directory.CreateDirectory(root) |> ignore
            Path.Combine(root, leafName))

    let createNamedChannel (requestedId: ChannelId) (name: string) (fileModeOverride: FileOpenMode option) =
        let normalized = name.Trim().ToUpperInvariant()

        if normalized.StartsWith("CON") then
            match ScreenDeviceStrings.tryParseScreenChannelConfig normalized with
            | Some config ->
                let channel = DefaultScreenChannel(requestedId, ConsoleChannel, reader, writer, false, currentMode(), Some config, buffer)
                Result.Ok(channel :> IChannel)
            | None ->
                Result.Error(InvalidHostArgument $"Console device string '{name}' is invalid.")
        elif normalized.StartsWith("SCR") then
            match ScreenDeviceStrings.tryParseScreenChannelConfig normalized with
            | Some config ->
                let channel = DefaultScreenChannel(requestedId, ScreenChannel, reader, writer, false, currentMode(), Some config, buffer)
                Result.Ok(channel :> IChannel)
            | None ->
                Result.Error(InvalidHostArgument $"Screen device string '{name}' is invalid.")
        elif normalized.StartsWith("NUL") then
            Result.Ok(NullChannel(requestedId) :> IChannel)
        elif normalized.StartsWith("PRT") then
            Result.Ok(DefaultPrinterChannel(requestedId, writer) :> IChannel)
        else
            match tryResolveDirectoryBackedPath normalized with
            | Some fullPath ->
                try
                    let mode = fileModeOverride |> Option.defaultValue OpenForUpdate
                    let channel = DefaultFileChannel(requestedId, fullPath, mode) :> IChannel
                    Result.Ok channel
                with
                | ex -> Result.Error(DeviceOpenFailed ex.Message)
            | None ->
                Result.Error(UnsupportedHostOperation $"Channel '{name}' is not supported by DefaultHost.")

    member _.TryGet(channelId: ChannelId) =
        match channels.TryGetValue channelId with
        | true, channel -> Some channel
        | false, _ -> None

    member _.Register(channel: IChannel) =
        channels[channel.Id] <- channel
        match channel with
        | :? DefaultScreenChannel as screenChannel -> screenChannels[channel.Id] <- screenChannel
        | _ -> ()
        Result.Ok()

    member _.ApplyMode(mode: ScreenModeInfo) =
        for pair in screenChannels do
            pair.Value.ApplyMode(mode)

    member _.ScreenPanes() =
        screenChannels.Values
        |> Seq.map (fun channel -> channel.Snapshot())
        |> Seq.filter (fun pane -> pane.ChannelId <> Some 0)
        |> Seq.sortBy (fun pane -> pane.ChannelId |> Option.defaultValue Int32.MaxValue)
        |> Seq.toList

    member _.OpenResolved(requestedId: ChannelId, name: string, fileModeOverride: FileOpenMode option) =
        match channels.TryGetValue requestedId with
        | true, _ -> Result.Error(InvalidHostArgument $"Channel #{requestedId} already exists.")
        | false, _ ->
            createNamedChannel requestedId name fileModeOverride
            |> Result.map (fun channel -> channels[requestedId] <- channel)

    interface IChannelManager with
        member _.Open(_name: string) =
            Result.Error(UnsupportedHostOperation "Dynamic channel opening is not implemented in DefaultHost.")

        member _.OpenAs(requestedId: ChannelId, name: string) =
            this.OpenResolved(requestedId, name, None)

        member _.Get(channelId: ChannelId) =
            match channels.TryGetValue channelId with
            | true, channel -> Result.Ok channel
            | false, _ -> Result.Error(ChannelNotFound channelId)

        member _.Close(channelId: ChannelId) =
            match channels.TryGetValue channelId with
            | true, channel ->
                if fixedChannels.Contains(channelId) then
                    Result.Error(InvalidHostArgument $"Channel #{channelId} is a default channel and cannot be closed.")
                else
                    channel.Close()
                    channels.Remove(channelId) |> ignore
                    screenChannels.Remove(channelId) |> ignore
                    Result.Ok()
            | false, _ -> Result.Error(ChannelNotFound channelId)

type private DefaultScreenDevice(writer: string -> unit, buffer: ScreenBuffer) =
    let mutable window = 0, 0, 0, 0
    let mutable scroll = 0
    let mutable width = None
    let mutable pan = 0
    let mutable recolor = None
    let mutable palette = None
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable ink = [ 7 ]
    let mutable paper = 0
    let mutable mode = ScreenDefaults.supportedModes[0]
    let mutable paneBuffer = ScreenBuffer(ScreenDefaults.paneMode ScreenDefaults.supportedModes[0] 1 1)

    let resetForMode selectedMode =
        window <- ScreenDefaults.defaultWindowForMode selectedMode
        scroll <- 0
        width <- None
        pan <- 0
        recolor <- None
        palette <- None
        cursor <- 0, 0
        characterSize <- ScreenDefaults.defaultCharacterSizeForMode selectedMode
        ink <- [ 7 ]
        paper <- 0
        let width, height, _, _ = window
        paneBuffer <- ScreenBuffer(ScreenDefaults.paneMode selectedMode width height)

    do
        resetForMode mode

    interface IScreenDevice with
        member _.Clear() =
            let width, height, x, y = window
            buffer.ClearWindow(width, height, x, y, paper)
            paneBuffer.ClearWindow(width, height, 0, 0, paper)
            cursor <- 0, 0
        member _.NewLine() =
            let width, height, x, y = window
            let _, cursorY = cursor
            if cursorY + 1 >= max 1 height then
                buffer.ScrollTextWindow(width, height, x, y, paper)
                paneBuffer.ScrollTextWindow(width, height, 0, 0, paper)
                cursor <- 0, max 0 (height - 1)
            else
                cursor <- 0, cursorY + 1
        member _.SetWindow(width, height, x, y) =
            window <- width, height, x, y
            paneBuffer <- ScreenBuffer(ScreenDefaults.paneMode mode width height)
            cursor <- 0, 0
        member _.GetWindow() = window
        member _.SetScroll(value) = scroll <- value
        member _.GetScroll() = scroll
        member _.SetWidth(value) = width <- Some value
        member _.GetWidth() = width
        member _.SetPan(value) = pan <- value
        member _.GetPan() = pan
        member _.SetRecolor(value) = recolor <- Some value
        member _.GetRecolor() = recolor
        member _.SetPalette(values) = palette <- Some values
        member _.GetPalette() = palette
        member _.SetCursor(x, y) = cursor <- x, y
        member _.GetCursor() = cursor
        member _.SetCharacterSize(width, height) = characterSize <- width, height
        member _.GetCharacterSize() = characterSize
        member _.WriteText text =
            let width, height, x, y = window
            let cursorX, cursorY = cursor
            let foreground = ink |> List.tryHead |> Option.defaultValue 7
            buffer.WriteText(width, height, x, y, cursorX, cursorY, foreground, paper, text)
            paneBuffer.WriteText(width, height, 0, 0, cursorX, cursorY, foreground, paper, text)
            let nextCursorX = min (max 0 (width - 1)) (cursorX + text.Length)
            cursor <- nextCursorX, cursorY
            writer text
        member _.SetInk(values: int list) = ink <- values
        member _.SetPaper(value: int) = paper <- value
        member _.SetBorder(_value: int) = ()
        member _.GetSupportedModes() = ScreenDefaults.supportedModes
        member _.GetMode() = mode
        member _.SetMode requestedMode =
            match ScreenDefaults.supportedModes |> List.tryFind (fun candidate -> candidate.Mode = requestedMode) with
            | Some selected ->
                mode <- selected
                resetForMode selected
                buffer.Resize(selected)
                Result.Ok()
            | None ->
                Result.Error(UnsupportedHostOperation $"Screen mode '{requestedMode}' is not supported by DefaultHost.")
    member _.Snapshot() =
        let paneSnapshot = paneBuffer.Snapshot().Panes.Head
        { paneSnapshot with
            ChannelId = Some 0
            Title = "#0"
            Kind = ConsoleChannel
            Window = window
            Cursor = cursor }

type private DefaultGraphicsDevice(buffer: ScreenBuffer) =
    let mutable cursor = 0.0, 0.0
    let mutable ink = [ 7 ]
    let mutable fillMode = 0
    let mutable scale = 0.0, 0.0, 0.0
    let mutable drawingWindow = 512, 256, 0, 0
    let mutable drawingPan = 0
    let mutable drawingScale = 100.0, 0.0, 0.0
    let mutable overMode = 0
    let mutable underMode = 0
    let mutable flashMode = 0
    let mutable penDown = true
    let mutable heading = 0.0

    let scaleFactors () =
        let sx, sy, _ = drawingScale
        let factorX = if sx = 0.0 then 1.0 else sx / 100.0
        let factorY = if sy = 0.0 then factorX else sy / 100.0
        factorX, factorY

    let clampToWindow x y =
        let width, height, originX, originY = drawingWindow
        let maxX = originX + max 0 (width - 1)
        let maxY = originY + max 0 (height - 1)
        let clampedX = Math.Min(float maxX, Math.Max(float originX, x))
        let clampedY = Math.Min(float maxY, Math.Max(float originY, y))
        clampedX, clampedY

    let transformAbsolute x y =
        let factorX, factorY = scaleFactors ()
        let _, _, originX, originY = drawingWindow
        let transformedX = float originX + float drawingPan + (x * factorX)
        let transformedY = float originY + (y * factorY)
        clampToWindow transformedX transformedY

    let transformDelta dx dy =
        let factorX, factorY = scaleFactors ()
        dx * factorX, dy * factorY

    interface IGraphicsDevice with
        member _.SetDrawingContext(window, pan, scaleContext) =
            drawingWindow <- window
            drawingPan <- pan
            drawingScale <- scaleContext
        member _.Plot(x, y) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            buffer.SetPixel(int (Math.Round tx), int (Math.Round ty), ink |> List.tryHead |> Option.defaultValue 7)
        member _.Point(x, y) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            buffer.SetPixel(int (Math.Round tx), int (Math.Round ty), ink |> List.tryHead |> Option.defaultValue 7)
        member _.PointRelative(dx, dy) =
            let x, y = cursor
            let tx, ty = transformDelta dx dy
            cursor <- clampToWindow (x + tx) (y + ty)
            let cx, cy = cursor
            buffer.SetPixel(int (Math.Round cx), int (Math.Round cy), ink |> List.tryHead |> Option.defaultValue 7)
        member _.Draw(x, y) =
            let startX, startY = cursor
            let tx, ty = transformAbsolute x y
            buffer.DrawLine(int (Math.Round startX), int (Math.Round startY), int (Math.Round tx), int (Math.Round ty), ink |> List.tryHead |> Option.defaultValue 7)
            cursor <- tx, ty
        member _.LineRelative(values) =
            let pairs =
                values
                |> List.chunkBySize 2
                |> List.choose (function
                    | [ x; y ] -> Some(x, y)
                    | _ -> None)

            match List.tryLast pairs with
            | Some(dx, dy) ->
                let x, y = cursor
                let tx, ty = transformDelta dx dy
                let endX, endY = clampToWindow (x + tx) (y + ty)
                buffer.DrawLine(int (Math.Round x), int (Math.Round y), int (Math.Round endX), int (Math.Round endY), ink |> List.tryHead |> Option.defaultValue 7)
                cursor <- endX, endY
            | None -> ()
        member _.DLine(values) =
            let coordinates =
                values
                |> List.chunkBySize 2
                |> List.choose (function
                    | [ x; y ] -> Some(x, y)
                    | _ -> None)

            match List.tryLast coordinates with
            | Some(x, y) ->
                cursor <- transformAbsolute x y
                let cx, cy = cursor
                buffer.SetPixel(int (Math.Round cx), int (Math.Round cy), ink |> List.tryHead |> Option.defaultValue 7)
            | None -> ()
        member _.Line(x1, y1, x2, y2) =
            let tx1, ty1 = transformAbsolute x1 y1
            let tx2, ty2 = transformAbsolute x2 y2
            buffer.DrawLine(int (Math.Round tx1), int (Math.Round ty1), int (Math.Round tx2), int (Math.Round ty2), ink |> List.tryHead |> Option.defaultValue 7)
            cursor <- tx2, ty2
        member _.Circle(x, y, _radius) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            buffer.SetPixel(int (Math.Round tx), int (Math.Round ty), ink |> List.tryHead |> Option.defaultValue 7)
        member _.CircleRelative(dx, dy, _radius) =
            let x, y = cursor
            let tx, ty = transformDelta dx dy
            let endX, endY = clampToWindow (x + tx) (y + ty)
            cursor <- endX, endY
            buffer.SetPixel(int (Math.Round endX), int (Math.Round endY), ink |> List.tryHead |> Option.defaultValue 7)
        member _.Ellipse(x, y, _radius, _ratio, _angle) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            buffer.SetPixel(int (Math.Round tx), int (Math.Round ty), ink |> List.tryHead |> Option.defaultValue 7)
        member _.EllipseRelative(dx, dy, _radius, _ratio, _angle) =
            let x, y = cursor
            let tx, ty = transformDelta dx dy
            let endX, endY = clampToWindow (x + tx) (y + ty)
            cursor <- endX, endY
            buffer.SetPixel(int (Math.Round endX), int (Math.Round endY), ink |> List.tryHead |> Option.defaultValue 7)
        member _.Arc(x, y, _radius, _startAngle, _endAngle) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            buffer.SetPixel(int (Math.Round tx), int (Math.Round ty), ink |> List.tryHead |> Option.defaultValue 7)
        member _.ArcRelative(dx, dy, _radius, _startAngle, _endAngle) =
            let x, y = cursor
            let tx, ty = transformDelta dx dy
            let endX, endY = clampToWindow (x + tx) (y + ty)
            cursor <- endX, endY
            buffer.SetPixel(int (Math.Round endX), int (Math.Round endY), ink |> List.tryHead |> Option.defaultValue 7)
        member _.Block(width, height, x, y, color) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            let startX = int (Math.Round tx)
            let startY = int (Math.Round ty)
            for row = startY to startY + max 0 (int (Math.Round height)) - 1 do
                for col = startX to startX + max 0 (int (Math.Round width)) - 1 do
                    buffer.SetPixel(col, row, color)
        member _.SetInk(values: int list) = ink <- values
        member _.SetFill(value: int) = fillMode <- value
        member _.SetScale(x, y, z) = scale <- x, y, z
        member _.SetOver(value: int) = overMode <- value
        member _.SetUnder(value: int) = underMode <- value
        member _.SetFlash(value: int) = flashMode <- value
        member _.SetPenDown(value: bool) = penDown <- value
        member _.Turn(angle) = heading <- heading + angle
        member _.TurnTo(angle) = heading <- angle
        member _.Clear() =
            let width, height, x, y = drawingWindow
            buffer.ClearWindow(width, height, x, y, 0)

type private DefaultInputDevice(reader: unit -> string option, readKey: unit -> KeyInfo option, keyAvailable: unit -> bool, keyRowState: int -> int) =
    interface IInputDevice with
        member _.ReadLine() = reader ()
        member _.ReadKey() = readKey ()
        member _.KeyAvailable() = keyAvailable ()
        member _.GetKeyRow(row) = keyRowState row

type private NullSoundDevice() =
    let mutable beepingUntil = DateTime.MinValue

    interface ISoundDevice with
        member _.Beep(_pitch, duration) =
            if duration > 0 then
                beepingUntil <- DateTime.UtcNow.AddMilliseconds(float duration)
            else
                beepingUntil <- DateTime.MinValue
        member _.IsBeeping() =
            DateTime.UtcNow < beepingUntil

type private NullFileSystem() =
    interface IDeviceFileSystem with
        member _.OpenFile(_path, _mode) =
            Result.Error(UnsupportedHostOperation "File channels are not implemented in DefaultHost.")
        member _.OpenFileAs(_channelId, _path, _mode) =
            Result.Error(UnsupportedHostOperation "File channels are not implemented in DefaultHost.")
        member _.Exists(_path) = false
        member _.Delete(_path) =
            Result.Error(UnsupportedHostOperation "File deletion is not implemented in DefaultHost.")

type private DefaultRuntimeHost(options: DefaultHostOptions) =
    let mutable currentMode = ScreenDefaults.supportedModes[0]
    let screenBuffer = ScreenBuffer(currentMode)
    let channel0 = DefaultScreenChannel(ChannelId 0, ChannelKind.ConsoleChannel, options.ReadLine, options.WriteLine, true, currentMode, None, screenBuffer)
    let channel1 = DefaultScreenChannel(ChannelId 1, ChannelKind.ScreenChannel, options.ReadLine, options.WriteLine, false, currentMode, None, screenBuffer)
    let channel2 = DefaultScreenChannel(ChannelId 2, ChannelKind.ScreenChannel, options.ReadLine, options.WriteLine, false, currentMode, None, screenBuffer)
    let channelManager =
        [ channel0 :> IChannel
          channel1 :> IChannel
          channel2 :> IChannel ]
        |> fun defaults -> DefaultChannelManager(defaults, options.ReadLine, options.WriteLine, (fun () -> currentMode), screenBuffer)
    let graphics = DefaultGraphicsDevice(screenBuffer) :> IGraphicsDevice
    let input = DefaultInputDevice(options.ReadLine, options.ReadKey, options.KeyAvailable, options.KeyRowState) :> IInputDevice
    let sound = NullSoundDevice() :> ISoundDevice
    let files =
        { new IDeviceFileSystem with
            member _.OpenFile(_path, _mode) =
                Result.Error(UnsupportedHostOperation "DefaultHost requires an explicit channel id for file open.")
            member _.OpenFileAs(channelId, path, mode) =
                let trimmed = path.Trim()
                let looksLikeDevice =
                    trimmed.Contains("_") && not (trimmed.Contains(Path.DirectorySeparatorChar) || trimmed.Contains(Path.AltDirectorySeparatorChar) || Path.IsPathRooted(trimmed))

                if looksLikeDevice then
                    channelManager.OpenResolved(channelId, trimmed, Some mode)
                else
                    match channelManager.TryGet channelId with
                    | Some _ -> Result.Error(InvalidHostArgument $"Channel #{channelId} already exists.")
                    | None ->
                        try
                            let fullPath = Path.GetFullPath(path)
                            let channel = DefaultFileChannel(channelId, fullPath, mode) :> IChannel
                            channelManager.Register(channel)
                        with
                        | ex -> Result.Error(DeviceOpenFailed ex.Message)
            member _.Exists(path) = File.Exists(path)
            member _.Delete(path) =
                try
                    File.Delete(path)
                    Result.Ok()
                with
                | ex -> Result.Error(DeviceOpenFailed ex.Message) }
    let environment =
        { new IEnvironmentProvider with
            member _.GetVariable(name) =
                let normalized = name.Trim().ToUpperInvariant()
                let tryNames names =
                    names
                    |> List.tryPick (fun candidate -> System.Environment.GetEnvironmentVariable(candidate) |> Option.ofObj)

                match normalized with
                | "PROG_USE" -> Some(Directory.GetCurrentDirectory())
                | "HOME" -> tryNames [ "HOME"; "USERPROFILE" ]
                | "TMPDIR"
                | "TEMP_USE" -> tryNames [ normalized; "TEMP"; "TMP" ]
                | "USER" -> tryNames [ "USER"; "USERNAME" ]
                | _ ->
                    match System.Environment.GetEnvironmentVariable(name) |> Option.ofObj with
                    | Some value -> Some value
                    | None -> tryNames [ normalized ] }

    member _.DisplaySurface =
        { new IDisplaySurface with
            member _.GetSnapshot() =
                { Mode = currentMode
                  Panes = channel0.Snapshot() :: channelManager.ScreenPanes() } }

    interface IRuntimeHost with
        member _.Channels = channelManager :> IChannelManager
        member _.Screen =
            { new IScreenDevice with
                member _.Clear() = (channel1 :> IScreenChannel).Clear()
                member _.NewLine() = (channel1 :> IScreenChannel).NewLine()
                member _.SetWindow(width, height, x, y) = (channel1 :> IScreenChannel).SetWindow(width, height, x, y)
                member _.GetWindow() = (channel1 :> IScreenChannel).GetWindow()
                member _.SetScroll(value) = (channel1 :> IScreenChannel).SetScroll(value)
                member _.GetScroll() = (channel1 :> IScreenChannel).GetScroll()
                member _.SetWidth(value) = (channel1 :> IScreenChannel).SetWidth(value)
                member _.GetWidth() = (channel1 :> IScreenChannel).GetWidth()
                member _.SetPan(value) = (channel1 :> IScreenChannel).SetPan(value)
                member _.GetPan() = (channel1 :> IScreenChannel).GetPan()
                member _.SetRecolor(value) = (channel1 :> IScreenChannel).SetRecolor(value)
                member _.GetRecolor() = (channel1 :> IScreenChannel).GetRecolor()
                member _.SetPalette(values) = (channel1 :> IScreenChannel).SetPalette(values)
                member _.GetPalette() = (channel1 :> IScreenChannel).GetPalette()
                member _.SetCursor(x, y) = (channel1 :> IScreenChannel).SetCursor(x, y)
                member _.GetCursor() = (channel1 :> IScreenChannel).GetCursor()
                member _.SetCharacterSize(width, height) = (channel1 :> IScreenChannel).SetCharacterSize(width, height)
                member _.GetCharacterSize() = (channel1 :> IScreenChannel).GetCharacterSize()
                member _.WriteText(text) = (channel1 :> IScreenChannel).WriteText(text)
                member _.SetInk(values) = (channel1 :> IScreenChannel).SetInk(values)
                member _.SetPaper(value) = (channel1 :> IScreenChannel).SetPaper(value)
                member _.SetBorder(value) = (channel1 :> IScreenChannel).SetBorder(value)
                member _.GetSupportedModes() = ScreenDefaults.supportedModes
                member _.GetMode() = currentMode
                member _.SetMode(requestedMode) =
                    match ScreenDefaults.supportedModes |> List.tryFind (fun candidate -> candidate.Mode = requestedMode) with
                    | Some selected ->
                        currentMode <- selected
                        screenBuffer.Resize(selected)
                        channelManager.ApplyMode(currentMode)
                        Result.Ok()
                    | None ->
                        Result.Error(UnsupportedHostOperation $"Screen mode '{requestedMode}' is not supported by DefaultHost.") }
        member _.Graphics = graphics
        member _.Input = input
        member _.Sound = sound
        member _.Files = files
        member _.Environment = environment

module DefaultHost =
    let create options : IRuntimeHost =
        DefaultRuntimeHost(options) :> IRuntimeHost

    let createWithDisplay options : IRuntimeHost * IDisplaySurface =
        let host = DefaultRuntimeHost(options)
        (host :> IRuntimeHost), host.DisplaySurface
