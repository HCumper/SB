namespace SBRuntime

open System
open System.IO
open System.Collections.Generic
open SBDisplay

// Minimal default host implementation for console-style execution and tests.

type DefaultHostOptions = {
    ReadLine: unit -> string option
    ReadScreenLine: ChannelId -> string option
    FlushInput: unit -> unit
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
        let bottomHeight =
            if mode.Height >= 256 then 40
            else max 24 (mode.Height / 6)

        let topHeight = max 1 (mode.Height - bottomHeight)
        let bottomY = min (mode.Height - bottomHeight) topHeight
        let leftWidth = max 1 (mode.Width / 2)
        let rightWidth = max 1 (mode.Width - leftWidth)

        match channelNumber with
        | 0 -> mode.Width, bottomHeight, 0, bottomY
        | 1 -> rightWidth, topHeight, mode.Width - rightWidth, 0
        | 2 -> leftWidth, topHeight, 0, 0
        | _ -> mode.Width, mode.Height, 0, 0

    let defaultWindowForMode (mode: ScreenModeInfo) =
        defaultWindowForChannel mode 0

    let defaultCharacterSizeForMode (mode: ScreenModeInfo) =
        mode.DefaultCharacterSize

    let supportedModes =
        [ { Mode = QlMode4
            Width = 512
            Height = 256
            Colors = Some 4
            Name = "QL Mode 4"
            IsQlCompatible = true
            BaseTextCellWidth = 8
            BaseTextCellHeight = 10
            DefaultCharacterSize = 0, 0 }
          { Mode = QlMode8
            Width = 256
            Height = 256
            Colors = Some 8
            Name = "QL Mode 8"
            IsQlCompatible = true
            BaseTextCellWidth = 8
            BaseTextCellHeight = 10
            DefaultCharacterSize = 0, 0 }
          { Mode = ExtendedMode 256
            Width = 256
            Height = 256
            Colors = Some 8
            Name = "Extended Mode 256"
            IsQlCompatible = false
            BaseTextCellWidth = 8
            BaseTextCellHeight = 8
            DefaultCharacterSize = 0, 0 }
          { Mode = ExtendedMode 512
            Width = 512
            Height = 256
            Colors = Some 4
            Name = "Extended Mode 512"
            IsQlCompatible = false
            BaseTextCellWidth = 8
            BaseTextCellHeight = 8
            DefaultCharacterSize = 0, 0 } ]

    let paneMode (mode: ScreenModeInfo) width height =
        { mode with
            Width = max 1 width
            Height = max 1 height }

module private TextLayout =
    let cellSize (mode: ScreenModeInfo) (characterSize: int * int) =
        let widthScale, heightScale = characterSize
        mode.BaseTextCellWidth * max 1 (widthScale + 1),
        mode.BaseTextCellHeight * max 1 (heightScale + 1)

    let visualAdvance cellWidth =
        max 1 (cellWidth - max 4 ((2 * cellWidth) / 3))

    let visibleCellSize (mode: ScreenModeInfo) width height characterSize =
        let cellWidth, cellHeight = cellSize mode characterSize
        max 1 (width / visualAdvance cellWidth),
        max 1 (height / cellHeight)

    let textOrigin (mode: ScreenModeInfo) x y characterSize =
        let cellWidth, cellHeight = cellSize mode characterSize
        max 0 (x / visualAdvance cellWidth),
        max 0 (y / cellHeight)

module private DevicePaths =
    let tryParseDirectoryBackedSpec (normalized: string) =
        let knownPrefixes = [ "RAM"; "FLP"; "WIN"; "MDV" ]
        knownPrefixes
        |> List.tryPick (fun prefix ->
            if normalized.StartsWith(prefix) then
                let suffix = normalized.Substring(prefix.Length)
                let digitCount = suffix |> Seq.takeWhile Char.IsDigit |> Seq.length
                if digitCount > 0 && suffix.Length > digitCount && suffix[digitCount] = '_' then
                    let device = normalized.Substring(0, prefix.Length + digitCount)
                    let rest = normalized.Substring(prefix.Length + digitCount + 1)
                    Some(device, if String.IsNullOrWhiteSpace rest then None else Some rest)
                else
                    None
            else
                None)

    let tryResolveDirectoryBackedPath (normalized: string) =
        tryParseDirectoryBackedSpec normalized
        |> Option.bind (fun (device, leafNameOpt) ->
            leafNameOpt
            |> Option.map (fun leafName ->
                let root = Path.Combine(Directory.GetCurrentDirectory(), "RuntimeDevices", device.ToLowerInvariant())
                Directory.CreateDirectory(root) |> ignore
                Path.Combine(root, leafName)))

    let private listEntries (directoryPath: string) =
        Directory.EnumerateFileSystemEntries(directoryPath)
        |> Seq.map Path.GetFileName
        |> Seq.sort
        |> Seq.toList

    let private listEntriesWithPrefix (directoryPath: string) (prefix: string) =
        listEntries directoryPath
        |> List.filter (fun name -> name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))

    let listDirectoryEntries (pathSpec: string option) =
        let cwd = Directory.GetCurrentDirectory()

        let listCurrentDirectory prefixOpt =
            let entries = listEntries cwd
            match prefixOpt with
            | Some prefix when not (String.IsNullOrWhiteSpace prefix) ->
                entries |> List.filter (fun name -> name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
            | _ -> entries

        let specText =
            pathSpec
            |> Option.map _.Trim()
            |> Option.filter (fun text -> not (String.IsNullOrWhiteSpace text))

        match specText with
        | None -> Result.Ok(listCurrentDirectory None)
        | Some spec when Path.IsPathRooted(spec) ->
            try
                if Directory.Exists(spec) then
                    Result.Ok(listEntries spec)
                else
                    let parent =
                        match Path.GetDirectoryName(spec) with
                        | null
                        | "" -> cwd
                        | value -> value
                    if Directory.Exists(parent) then
                        Result.Ok(listEntriesWithPrefix parent (Path.GetFileName(spec)))
                    else
                        Result.Error(InvalidHostArgument $"Directory '{parent}' does not exist.")
            with
            | ex -> Result.Error(DeviceOpenFailed ex.Message)
        | Some spec ->
            let normalized = spec.ToUpperInvariant()
            match tryParseDirectoryBackedSpec normalized with
            | Some(device, leafPrefix) ->
                try
                    let root = Path.Combine(cwd, "RuntimeDevices", device.ToLowerInvariant())
                    Directory.CreateDirectory(root) |> ignore
                    let entries =
                        match leafPrefix with
                        | Some prefix -> listEntriesWithPrefix root prefix
                        | None -> listEntries root
                    Result.Ok(entries |> List.map (fun name -> $"{device.ToLowerInvariant()}_{name}"))
                with
                | ex -> Result.Error(DeviceOpenFailed ex.Message)
            | None ->
                try
                    Result.Ok(listCurrentDirectory (Some spec))
                with
                | ex -> Result.Error(DeviceOpenFailed ex.Message)

type private TextCell = {
    mutable Character: char
    mutable CodePoint: int
    mutable Ink: int
    mutable Paper: int
    mutable Strip: int
    mutable HasBackground: bool
}

type private ScreenBuffer(mode: ScreenModeInfo, ?textWidth: int, ?textHeight: int) =
    let textWidth = defaultArg textWidth mode.Width
    let textHeight = defaultArg textHeight mode.Height

    let colorMask = 0x00FFFFFF
    let flashBit = 0x01000000

    let createText width height =
        Array2D.init height width (fun _ _ -> { Character = ' '; CodePoint = int ' '; Ink = 7; Paper = 0; Strip = 0; HasBackground = false } : TextCell)

    let mutable currentMode = mode
    let mutable currentTextWidth = max 1 textWidth
    let mutable currentTextHeight = max 1 textHeight
    let mutable text = createText currentTextWidth currentTextHeight
    let mutable pixels = Array2D.create mode.Height mode.Width 0

    let composeSurface (targetMode: ScreenModeInfo) (characterSize: int * int) (textSnapshot: ScreenTextCell[,]) (pixelSnapshot: int[,]) =
        let surfaceHeight = Array2D.length1 pixelSnapshot
        let surfaceWidth = Array2D.length2 pixelSnapshot
        let surface = Array2D.init surfaceHeight surfaceWidth (fun row col -> pixelSnapshot[row, col])
        let cellWidth, cellHeight = TextLayout.cellSize targetMode characterSize
        let cellAdvance = TextLayout.visualAdvance cellWidth
        let textRows = Array2D.length1 textSnapshot
        let textCols = Array2D.length2 textSnapshot

        let paintPixel row col color =
            if row >= 0 && row < surfaceHeight && col >= 0 && col < surfaceWidth then
                surface[row, col] <- color

        for textRow = 0 to textRows - 1 do
            for textCol = 0 to textCols - 1 do
                let cell = textSnapshot[textRow, textCol]
                let glyphOriginX = textCol * cellAdvance
                let originY = textRow * cellHeight

                if cell.HasBackground then
                    for row = originY to min (surfaceHeight - 1) (originY + cellHeight - 1) do
                        for col = glyphOriginX to min (surfaceWidth - 1) (glyphOriginX + cellAdvance - 1) do
                            paintPixel row col cell.Strip

                if cell.Character <> ' ' then
                    let glyph = QlBitmapFont.glyphForCharacter cell.CodePoint cell.Character
                    let glyphRows = min 8 glyph.Length
                    let renderWidth = min cellWidth 8
                    let renderHeight = min cellHeight glyphRows
                    let verticalInset = max 0 ((cellHeight - renderHeight) / 2)

                    for glyphRow = 0 to glyphRows - 1 do
                        let pattern = int glyph[glyphRow]
                        for targetCol = 0 to renderWidth - 1 do
                            let glyphCol =
                                min 7 (int (float targetCol * 8.0 / float renderWidth))
                            if (pattern >>> glyphCol) &&& 1 = 1 then
                                let startX = glyphOriginX + targetCol
                                let startY = originY + verticalInset + glyphRow
                                let endX = min (surfaceWidth - 1) startX
                                let endY = min (surfaceHeight - 1) startY

                                for row = startY to endY do
                                    for col = startX to endX do
                                        paintPixel row col cell.Ink

        surface

    member _.Mode = currentMode
    member _.Text = text
    member _.Pixels = pixels
    member _.TextWidth = currentTextWidth
    member _.TextHeight = currentTextHeight
    member _.Snapshot() =
        let textSnapshot: ScreenTextCell[,] =
            Array2D.init currentTextHeight currentTextWidth (fun row col ->
                let cell = text[row, col]
                { Character = cell.Character
                  CodePoint = cell.CodePoint
                  Ink = cell.Ink
                  Paper = cell.Paper
                  Strip = cell.Strip
                  HasBackground = cell.HasBackground })

        let pixelSnapshot =
            Array2D.init currentMode.Height currentMode.Width (fun row col -> pixels[row, col])
        let surfaceSnapshot = composeSurface currentMode (0, 0) textSnapshot pixelSnapshot

        { Mode = currentMode
          Panes =
            [ { ChannelId = None
                Title = "screen"
                Kind = ScreenChannel
                Window = 0, 0, 0, 0
                Cursor = 0, 0
                CharacterSize = 0, 0
                Ink = 7
                Paper = 0
                Strip = 0
                Border = 0
                Recolor = None
                Palette = None
                Text = textSnapshot
                Pixels = pixelSnapshot
                Surface = surfaceSnapshot } ] }
    member _.Resize(newMode: ScreenModeInfo, ?newTextWidth: int, ?newTextHeight: int) =
        currentMode <- newMode
        currentTextWidth <- max 1 (defaultArg newTextWidth currentTextWidth)
        currentTextHeight <- max 1 (defaultArg newTextHeight currentTextHeight)
        text <- createText currentTextWidth currentTextHeight
        pixels <- Array2D.create newMode.Height newMode.Width 0
    member _.SetTextCell(col: int, row: int, ink: int, paper: int, strip: int, hasBackground: bool, ch: char) =
        if row >= 0 && row < currentTextHeight && col >= 0 && col < currentTextWidth then
            text[row, col] <- { Character = ch; CodePoint = int ch; Ink = ink; Paper = paper; Strip = strip; HasBackground = hasBackground }
    member _.CopyTextFrom(snapshot: ScreenTextCell[,]) =
        let sourceHeight = Array2D.length1 snapshot
        let sourceWidth = Array2D.length2 snapshot
        for row in 0 .. min (currentTextHeight - 1) (sourceHeight - 1) do
            for col in 0 .. min (currentTextWidth - 1) (sourceWidth - 1) do
                let cell = snapshot[row, col]
                text[row, col] <- { Character = cell.Character; CodePoint = cell.CodePoint; Ink = cell.Ink; Paper = cell.Paper; Strip = cell.Strip; HasBackground = cell.HasBackground }
    member _.ClearWindow(width: int, height: int, x: int, y: int, paper: int) =
        let maxTextX = min currentTextWidth (x + max 0 width)
        let maxTextY = min currentTextHeight (y + max 0 height)
        for row in max 0 y .. max 0 (maxTextY - 1) do
            for col in max 0 x .. max 0 (maxTextX - 1) do
                if row >= 0 && row < currentTextHeight && col >= 0 && col < currentTextWidth then
                    text[row, col] <- { Character = ' '; CodePoint = int ' '; Ink = 7; Paper = paper; Strip = paper; HasBackground = false }
        let maxX = min currentMode.Width (x + max 0 width)
        let maxY = min currentMode.Height (y + max 0 height)
        for row in max 0 y .. max 0 (maxY - 1) do
            for col in max 0 x .. max 0 (maxX - 1) do
                pixels[row, col] <- paper
    member _.ClearTextWindow(width: int, height: int, x: int, y: int, paper: int) =
        let maxX = min currentTextWidth (x + max 0 width)
        let maxY = min currentTextHeight (y + max 0 height)
        for row in max 0 y .. max 0 (maxY - 1) do
            for col in max 0 x .. max 0 (maxX - 1) do
                if row >= 0 && row < currentTextHeight && col >= 0 && col < currentTextWidth then
                    text[row, col] <- { Character = ' '; CodePoint = int ' '; Ink = 7; Paper = paper; Strip = paper; HasBackground = false }
    member _.ScrollTextWindow(width: int, height: int, x: int, y: int, paper: int) =
        let maxX = min currentTextWidth (x + max 0 width)
        let maxY = min currentTextHeight (y + max 0 height)
        let startX = max 0 x
        let startY = max 0 y

        if maxX > startX && maxY > startY then
            for row in startY .. maxY - 2 do
                for col in startX .. maxX - 1 do
                    if row + 1 < currentTextHeight && col < currentTextWidth then
                        let nextCell = text[row + 1, col]
                        text[row, col] <- { Character = nextCell.Character; CodePoint = nextCell.CodePoint; Ink = nextCell.Ink; Paper = nextCell.Paper; Strip = nextCell.Strip; HasBackground = nextCell.HasBackground }
                    if row + 1 < currentMode.Height && col < currentMode.Width then
                        pixels[row, col] <- pixels[row + 1, col]

            for col in startX .. maxX - 1 do
                if maxY - 1 >= 0 && maxY - 1 < currentTextHeight && col < currentTextWidth then
                    text[maxY - 1, col] <- { Character = ' '; CodePoint = int ' '; Ink = 7; Paper = paper; Strip = paper; HasBackground = false }
                if maxY - 1 >= 0 && maxY - 1 < currentMode.Height && col < currentMode.Width then
                    pixels[maxY - 1, col] <- paper
    member _.SetPixel(x: int, y: int, color: int) =
        if x >= 0 && x < currentMode.Width && y >= 0 && y < currentMode.Height then
            pixels[y, x] <- color
    member _.GetPixel(x: int, y: int) =
        if x >= 0 && x < currentMode.Width && y >= 0 && y < currentMode.Height then
            pixels[y, x]
        else
            0
    member _.ApplyPixel(x: int, y: int, color: int, overMode: int, underMode: int, flashMode: int, backgroundColor: int) =
        if x >= 0 && x < currentMode.Width && y >= 0 && y < currentMode.Height then
            let existing = pixels[y, x]
            let existingColor = existing &&& colorMask
            let canDraw =
                if underMode <> 0 then
                    existingColor = backgroundColor
                else
                    true

            if canDraw then
                let encodedColor = color &&& colorMask
                let finalColor =
                    if overMode = -1 then
                        existingColor ^^^ encodedColor
                    else
                        encodedColor

                let flash =
                    ((existing &&& flashBit) <> 0 && overMode = -1) || flashMode <> 0

                pixels[y, x] <- finalColor ||| (if flash then flashBit else 0)
    member this.DrawLine(x1: int, y1: int, x2: int, y2: int, color: int) =
        this.DrawLineStyled(x1, y1, x2, y2, color, 0, 0, 0, 0)
    member this.DrawLineStyled(x1: int, y1: int, x2: int, y2: int, color: int, overMode: int, underMode: int, flashMode: int, backgroundColor: int) =
        let dx = abs (x2 - x1)
        let sx = if x1 < x2 then 1 else -1
        let dy = -abs (y2 - y1)
        let sy = if y1 < y2 then 1 else -1
        let rec loop x y err =
            this.ApplyPixel(x, y, color, overMode, underMode, flashMode, backgroundColor)
            if x <> x2 || y <> y2 then
                let e2 = 2 * err
                let nextX, nextErr =
                    if e2 >= dy then x + sx, err + dy else x, err
                let nextY, finalErr =
                    if e2 <= dx then y + sy, nextErr + dx else y, nextErr
                loop nextX nextY finalErr
        loop x1 y1 (dx + dy)
    member this.FillPolygon(points: (int * int) list, color: int, overMode: int, underMode: int, flashMode: int, backgroundColor: int) =
        match points with
        | []
        | [ _ ]
        | [ _; _ ] -> ()
        | _ ->
            let minY = points |> List.minBy snd |> snd
            let maxY = points |> List.maxBy snd |> snd

            for scanY = minY to maxY do
                let intersections =
                    points
                    |> List.pairwise
                    |> List.append [ List.last points, List.head points ]
                    |> List.choose (fun ((x1, y1), (x2, y2)) ->
                        if y1 = y2 then
                            None
                        else
                            let minEdgeY = min y1 y2
                            let maxEdgeY = max y1 y2
                            if scanY < minEdgeY || scanY >= maxEdgeY then
                                None
                            else
                                let t = float (scanY - y1) / float (y2 - y1)
                                Some(int (Math.Round(float x1 + (float (x2 - x1) * t)))))
                    |> List.sort

                intersections
                |> List.chunkBySize 2
                |> List.iter (function
                    | [ startX; endX ] ->
                        for x = min startX endX to max startX endX do
                            this.ApplyPixel(x, scanY, color, overMode, underMode, flashMode, backgroundColor)
                    | _ -> ())
    member _.ComposeSurface(mode: ScreenModeInfo, characterSize: int * int, textSnapshot: ScreenTextCell[,], pixelSnapshot: int[,]) =
        composeSurface mode characterSize textSnapshot pixelSnapshot

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

type private DefaultScreenChannel(id: ChannelId, kind: ChannelKind, screenReader: ChannelId -> string option, writer: string -> unit, mirrorToWriter: bool, initialMode: ScreenModeInfo, config: ScreenChannelConfig option, buffer: ScreenBuffer) =
    let (ChannelId channelNumber) = id
    let mutable window = 0, 0, 0, 0
    let mutable scroll = 0
    let mutable width = None
    let mutable pan = 0
    let mutable recolor: int list option = None
    let mutable palette = None
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable characterFonts = 0, 0
    let mutable ink = [ 7 ]
    let mutable paper = 0
    let mutable strip = [ 0 ]
    let mutable border = 0
    let mutable mode = initialMode
    let mutable paneBuffer = ScreenBuffer(ScreenDefaults.paneMode initialMode 1 1)

    let textMetrics() =
        let width, height, _, _ = window
        TextLayout.visibleCellSize mode width height characterSize

    let textOrigin() =
        let _, _, x, y = window
        TextLayout.textOrigin mode x y characterSize

    let clampCursorPosition (x: int, y: int) =
        let cols, rows = textMetrics()
        max 0 (min (cols - 1) x), max 0 (min (rows - 1) y)

    let rebuildPaneBuffer preserveContents =
        let width, height, _, _ = window
        let cols, rows = textMetrics()
        let existingText =
            if preserveContents then Some(paneBuffer.Snapshot().Panes.Head.Text) else None
        let nextBuffer = ScreenBuffer(ScreenDefaults.paneMode mode width height, textWidth = cols, textHeight = rows)
        nextBuffer.ClearWindow(width, height, 0, 0, paper)
        existingText |> Option.iter nextBuffer.CopyTextFrom
        paneBuffer <- nextBuffer
        cursor <- clampCursorPosition cursor

    let clearTextBuffers() =
        let width, height, x, y = window
        let cols, rows = textMetrics()
        let textX, textY = textOrigin()
        buffer.ClearWindow(width, height, x, y, paper)
        buffer.ClearTextWindow(cols, rows, textX, textY, paper)
        paneBuffer.ClearWindow(width, height, 0, 0, paper)
        paneBuffer.ClearTextWindow(cols, rows, 0, 0, paper)

    let newline() =
        let cols, rows = textMetrics()
        let textX, textY = textOrigin()
        let _, cursorY = cursor
        if cursorY + 1 >= rows then
            buffer.ScrollTextWindow(cols, rows, textX, textY, paper)
            paneBuffer.ScrollTextWindow(cols, rows, 0, 0, paper)
            cursor <- 0, max 0 (rows - 1)
        else
            cursor <- 0, cursorY + 1

    let writeText content =
        let cols, rows = textMetrics()
        let textX, textY = textOrigin()
        let foreground = ink |> List.tryHead |> Option.defaultValue 7
        let background = strip |> List.tryHead |> Option.defaultValue paper
        let mutable cursorX, cursorY = clampCursorPosition cursor

        let writeChar ch =
            buffer.SetTextCell(textX + cursorX, textY + cursorY, foreground, paper, background, true, ch)
            paneBuffer.SetTextCell(cursorX, cursorY, foreground, paper, background, true, ch)
            cursorX <- cursorX + 1

        for ch in content do
            match ch with
            | '\r' -> ()
            | '\n' ->
                cursor <- cursorX, cursorY
                newline()
                let nextX, nextY = cursor
                cursorX <- nextX
                cursorY <- nextY
            | _ ->
                if cursorX >= cols then
                    cursor <- cursorX, cursorY
                    newline()
                    let nextX, nextY = cursor
                    cursorX <- nextX
                    cursorY <- nextY
                if rows > 0 && cols > 0 then
                    writeChar ch

        cursor <- clampCursorPosition (cursorX, cursorY)

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
        characterFonts <- 0, 0
        ink <- [ 7 ]
        paper <- ScreenDefaults.defaultPaperForChannel channelNumber
        strip <- [ paper ]
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
        rebuildPaneBuffer false

    do
        resetForMode initialMode

    interface IScreenChannel with
        member _.Id = id
        member _.Kind = kind
        member _.WriteText text =
            writeText text
            if mirrorToWriter then
                writer text
        member _.ReadText() = screenReader id
        member _.IsEndOfFile() = false
        member _.Flush() = ()
        member _.Close() = ()
        member _.Clear() =
            clearTextBuffers()
            cursor <- 0, 0
        member _.NewLine() = newline()
        member _.SetWindow(width, height, x, y) =
            window <- width, height, x, y
            cursor <- 0, 0
            rebuildPaneBuffer false
        member _.GetWindow() = window
        member _.SetScroll(value) = scroll <- value
        member _.GetScroll() = scroll
        member _.SetWidth(value) = width <- Some value
        member _.GetWidth() = width
        member _.SetPan(value) = pan <- value
        member _.GetPan() = pan
        member _.SetRecolor(values) = recolor <- Some values
        member _.GetRecolor() = recolor
        member _.SetPalette(values) = palette <- Some values
        member _.GetPalette() = palette
        member _.SetCursor(x, y) = cursor <- clampCursorPosition (x, y)
        member _.GetCursor() = cursor
        member _.SetCharacterSize(width, height) =
            characterSize <- width, height
            rebuildPaneBuffer true
        member _.GetCharacterSize() = characterSize
        member _.SetCharacterFonts(font1, font2) =
            let currentFont1, currentFont2 = characterFonts
            characterFonts <-
                (if font1 = -1 then currentFont1 else font1),
                (if font2 = -1 then currentFont2 else font2)
        member _.GetCharacterFonts() = characterFonts
        member _.SetInk(values: int list) = ink <- values
        member _.SetPaper(value: int) =
            paper <- value
            strip <- [ value ]
        member _.SetStrip(values: int list) =
            strip <- if List.isEmpty values then [ paper ] else values
        member _.SetBorder(value: int) = border <- value
    member this.ApplyMode(mode: ScreenModeInfo) =
        resetForMode mode
    member _.Snapshot() =
        let width, height, x, y = window
        let safeWidth = max 1 width
        let safeHeight = max 1 height
        let textSnapshot = paneBuffer.Snapshot().Panes.Head.Text
        let pixelSnapshot =
            Array2D.init safeHeight safeWidth (fun row col ->
                let sourceY = y + row
                let sourceX = x + col
                if sourceY >= 0 && sourceY < buffer.Mode.Height && sourceX >= 0 && sourceX < buffer.Mode.Width then
                    buffer.Pixels[sourceY, sourceX]
                else
                    paper)
        let surfaceSnapshot = buffer.ComposeSurface(mode, characterSize, textSnapshot, pixelSnapshot)

        { ChannelId = Some channelNumber
          Title = $"#{channelNumber}"
          Kind = kind
          Window = window
          Cursor = cursor
          CharacterSize = characterSize
          Ink = ink |> List.tryHead |> Option.defaultValue 7
          Paper = paper
          Strip = strip |> List.tryHead |> Option.defaultValue paper
          Border = border
          Recolor = recolor
          Palette = palette
          Text = textSnapshot
          Pixels = pixelSnapshot
          Surface = surfaceSnapshot }

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

type private DefaultChannelManager(defaultChannels: IChannel list, screenReader: ChannelId -> string option, writer: string -> unit, currentMode: unit -> ScreenModeInfo, buffer: ScreenBuffer) as this =
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

    let createNamedChannel (requestedId: ChannelId) (name: string) (fileModeOverride: FileOpenMode option) =
        let trimmed = name.Trim()
        let normalized = trimmed.ToUpperInvariant()

        if normalized.StartsWith("CON") then
            match ScreenDeviceStrings.tryParseScreenChannelConfig normalized with
            | Some config ->
                let channel = DefaultScreenChannel(requestedId, ConsoleChannel, screenReader, writer, false, currentMode(), Some config, buffer)
                Result.Ok(channel :> IChannel)
            | None ->
                Result.Error(InvalidHostArgument $"Console device string '{name}' is invalid.")
        elif normalized.StartsWith("SCR") then
            match ScreenDeviceStrings.tryParseScreenChannelConfig normalized with
            | Some config ->
                let channel = DefaultScreenChannel(requestedId, ScreenChannel, screenReader, writer, false, currentMode(), Some config, buffer)
                Result.Ok(channel :> IChannel)
            | None ->
                Result.Error(InvalidHostArgument $"Screen device string '{name}' is invalid.")
        elif normalized.StartsWith("NUL") then
            Result.Ok(NullChannel(requestedId) :> IChannel)
        elif normalized.StartsWith("PRT") then
            Result.Ok(DefaultPrinterChannel(requestedId, writer) :> IChannel)
        elif Path.IsPathRooted(trimmed) then
            try
                let mode = fileModeOverride |> Option.defaultValue OpenForUpdate
                let channel = DefaultFileChannel(requestedId, trimmed, mode) :> IChannel
                Result.Ok channel
            with
            | ex -> Result.Error(DeviceOpenFailed ex.Message)
        else
            match DevicePaths.tryResolveDirectoryBackedPath normalized with
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
        member _.Open(name: string) =
            let mutable candidate = 3
            while channels.ContainsKey(ChannelId candidate) do
                candidate <- candidate + 1
            let channelId = ChannelId candidate
            this.OpenResolved(channelId, name, None)
            |> Result.map (fun () -> channelId)

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
    let mutable recolor: int list option = None
    let mutable palette = None
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable characterFonts = 0, 0
    let mutable ink = [ 7 ]
    let mutable paper = 0
    let mutable strip = [ 0 ]
    let mutable mode = ScreenDefaults.supportedModes[0]
    let mutable paneBuffer = ScreenBuffer(ScreenDefaults.paneMode ScreenDefaults.supportedModes[0] 1 1)

    let textMetrics() =
        let width, height, _, _ = window
        TextLayout.visibleCellSize mode width height characterSize

    let textOrigin() =
        let _, _, x, y = window
        TextLayout.textOrigin mode x y characterSize

    let clampCursorPosition (x: int, y: int) =
        let cols, rows = textMetrics()
        max 0 (min (cols - 1) x), max 0 (min (rows - 1) y)

    let rebuildPaneBuffer preserveContents =
        let width, height, _, _ = window
        let cols, rows = textMetrics()
        let existingText =
            if preserveContents then Some(paneBuffer.Snapshot().Panes.Head.Text) else None
        let nextBuffer = ScreenBuffer(ScreenDefaults.paneMode mode width height, textWidth = cols, textHeight = rows)
        nextBuffer.ClearWindow(width, height, 0, 0, paper)
        existingText |> Option.iter nextBuffer.CopyTextFrom
        paneBuffer <- nextBuffer
        cursor <- clampCursorPosition cursor

    let clearTextBuffers() =
        let width, height, x, y = window
        let cols, rows = textMetrics()
        let textX, textY = textOrigin()
        buffer.ClearWindow(width, height, x, y, paper)
        buffer.ClearTextWindow(cols, rows, textX, textY, paper)
        paneBuffer.ClearWindow(width, height, 0, 0, paper)
        paneBuffer.ClearTextWindow(cols, rows, 0, 0, paper)

    let newline() =
        let cols, rows = textMetrics()
        let textX, textY = textOrigin()
        let _, cursorY = cursor
        if cursorY + 1 >= rows then
            buffer.ScrollTextWindow(cols, rows, textX, textY, paper)
            paneBuffer.ScrollTextWindow(cols, rows, 0, 0, paper)
            cursor <- 0, max 0 (rows - 1)
        else
            cursor <- 0, cursorY + 1

    let writeText content =
        let cols, rows = textMetrics()
        let textX, textY = textOrigin()
        let foreground = ink |> List.tryHead |> Option.defaultValue 7
        let background = strip |> List.tryHead |> Option.defaultValue paper
        let mutable cursorX, cursorY = clampCursorPosition cursor

        let writeChar ch =
            buffer.SetTextCell(textX + cursorX, textY + cursorY, foreground, paper, background, true, ch)
            paneBuffer.SetTextCell(cursorX, cursorY, foreground, paper, background, true, ch)
            cursorX <- cursorX + 1

        for ch in content do
            match ch with
            | '\r' -> ()
            | '\n' ->
                cursor <- cursorX, cursorY
                newline()
                let nextX, nextY = cursor
                cursorX <- nextX
                cursorY <- nextY
            | _ ->
                if cursorX >= cols then
                    cursor <- cursorX, cursorY
                    newline()
                    let nextX, nextY = cursor
                    cursorX <- nextX
                    cursorY <- nextY
                if rows > 0 && cols > 0 then
                    writeChar ch

        cursor <- clampCursorPosition (cursorX, cursorY)

    let resetForMode selectedMode =
        window <- ScreenDefaults.defaultWindowForMode selectedMode
        scroll <- 0
        width <- None
        pan <- 0
        recolor <- None
        palette <- None
        cursor <- 0, 0
        characterSize <- ScreenDefaults.defaultCharacterSizeForMode selectedMode
        characterFonts <- 0, 0
        ink <- [ 7 ]
        paper <- 0
        strip <- [ paper ]
        rebuildPaneBuffer false

    do
        resetForMode mode

    interface IScreenDevice with
        member _.Clear() =
            clearTextBuffers()
            cursor <- 0, 0
        member _.NewLine() = newline()
        member _.SetWindow(width, height, x, y) =
            window <- width, height, x, y
            cursor <- 0, 0
            rebuildPaneBuffer false
        member _.GetWindow() = window
        member _.SetScroll(value) = scroll <- value
        member _.GetScroll() = scroll
        member _.SetWidth(value) = width <- Some value
        member _.GetWidth() = width
        member _.SetPan(value) = pan <- value
        member _.GetPan() = pan
        member _.SetRecolor(values) = recolor <- Some values
        member _.GetRecolor() = recolor
        member _.SetPalette(values) = palette <- Some values
        member _.GetPalette() = palette
        member _.SetCursor(x, y) = cursor <- clampCursorPosition (x, y)
        member _.GetCursor() = cursor
        member _.SetCharacterSize(width, height) =
            characterSize <- width, height
            rebuildPaneBuffer true
        member _.GetCharacterSize() = characterSize
        member _.SetCharacterFonts(font1, font2) =
            let currentFont1, currentFont2 = characterFonts
            characterFonts <-
                (if font1 = -1 then currentFont1 else font1),
                (if font2 = -1 then currentFont2 else font2)
        member _.GetCharacterFonts() = characterFonts
        member _.WriteText text =
            writeText text
            writer text
        member _.SetInk(values: int list) = ink <- values
        member _.SetPaper(value: int) =
            paper <- value
            strip <- [ value ]
        member _.SetStrip(values: int list) =
            strip <- if List.isEmpty values then [ paper ] else values
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
        let surfaceSnapshot = buffer.ComposeSurface(mode, characterSize, paneSnapshot.Text, paneSnapshot.Pixels)
        { paneSnapshot with
            ChannelId = Some 0
            Title = "#0"
            Kind = ConsoleChannel
            Window = window
            Cursor = cursor
            CharacterSize = characterSize
            Ink = ink |> List.tryHead |> Option.defaultValue 7
            Paper = paper
            Strip = strip |> List.tryHead |> Option.defaultValue paper
            Border = 0
            Recolor = recolor
            Palette = palette
            Surface = surfaceSnapshot }

type private DefaultGraphicsDevice(buffer: ScreenBuffer) =
    let twoPi = Math.PI * 2.0
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
    let mutable drawingBackgroundColor = 0

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

    let currentInk() =
        ink |> List.tryHead |> Option.defaultValue 7

    let drawPixel (x: float) (y: float) =
        buffer.ApplyPixel(int (Math.Round x), int (Math.Round y), currentInk (), overMode, underMode, flashMode, drawingBackgroundColor)

    let drawLine (x1: float) (y1: float) (x2: float) (y2: float) =
        buffer.DrawLineStyled(int (Math.Round x1), int (Math.Round y1), int (Math.Round x2), int (Math.Round y2), currentInk (), overMode, underMode, flashMode, drawingBackgroundColor)

    let scaledRadius radius =
        let factorX, factorY = scaleFactors ()
        max 1.0 (abs radius * factorX), max 1.0 (abs radius * factorY)

    let sampledCurve closeShape shouldFill points =
        match points with
        | [] -> ()
        | [ point ] ->
            let x, y = point
            drawPixel x y
        | _ ->
            points
            |> List.pairwise
            |> List.iter (fun ((x1, y1), (x2, y2)) -> drawLine x1 y1 x2 y2)

            if closeShape then
                let startX, startY = List.head points
                let endX, endY = List.last points
                drawLine endX endY startX startY

            if shouldFill && fillMode <> 0 then
                let polygon =
                    if closeShape then points else points @ [ List.head points ]
                    |> List.map (fun (x: float, y: float) -> int (Math.Round x), int (Math.Round y))
                buffer.FillPolygon(polygon, currentInk (), overMode, underMode, flashMode, drawingBackgroundColor)

    let sampleEllipse centerX centerY radiusX radiusY angleDegrees startAngle endAngle closeShape shouldFill =
        let normalizedRadiusX = max 1.0 (abs radiusX)
        let normalizedRadiusY = max 1.0 (abs radiusY)
        let rotation = angleDegrees * Math.PI / 180.0
        let startRadians = startAngle * Math.PI / 180.0
        let endRadians = endAngle * Math.PI / 180.0
        let delta =
            if closeShape then
                twoPi
            elif endRadians >= startRadians then
                endRadians - startRadians
            else
                (twoPi - startRadians) + endRadians
        let span = max 0.001 delta
        let steps = max 24 (int (Math.Ceiling(max normalizedRadiusX normalizedRadiusY * span / 3.0)))
        [ for step = 0 to steps do
              let t = startRadians + (span * float step / float steps)
              let localX = normalizedRadiusX * Math.Cos(t)
              let localY = normalizedRadiusY * Math.Sin(t)
              let rotatedX = (localX * Math.Cos(rotation)) - (localY * Math.Sin(rotation))
              let rotatedY = (localX * Math.Sin(rotation)) + (localY * Math.Cos(rotation))
              yield clampToWindow (centerX + rotatedX) (centerY + rotatedY) ]
        |> sampledCurve closeShape shouldFill

    let sampleCircle centerX centerY radiusX radiusY shouldFill =
        sampleEllipse centerX centerY radiusX radiusY 0.0 0.0 360.0 true shouldFill

    let arcEndPoint centerX centerY radiusX radiusY angleDegrees =
        let radians = angleDegrees * Math.PI / 180.0
        clampToWindow (centerX + (radiusX * Math.Cos(radians))) (centerY + (radiusY * Math.Sin(radians)))

    interface IGraphicsDevice with
        member _.SetDrawingContext(window, pan, scaleContext) =
            drawingWindow <- window
            drawingPan <- pan
            drawingScale <- scaleContext
            let _, _, originX, originY = window
            drawingBackgroundColor <- buffer.GetPixel(originX, originY) &&& 0x00FFFFFF
        member _.Plot(x, y) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            drawPixel tx ty
        member _.Point(x, y) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            drawPixel tx ty
        member _.PointRelative(dx, dy) =
            let x, y = cursor
            let tx, ty = transformDelta dx dy
            cursor <- clampToWindow (x + tx) (y + ty)
            let cx, cy = cursor
            drawPixel cx cy
        member _.Draw(x, y) =
            let startX, startY = cursor
            let tx, ty = transformAbsolute x y
            drawLine startX startY tx ty
            cursor <- tx, ty
        member _.LineRelative(values) =
            let pairs =
                values
                |> List.chunkBySize 2
                |> List.choose (function
                    | [ x; y ] -> Some(x, y)
                    | _ -> None)

            let mutable currentX, currentY = cursor
            for dx, dy in pairs do
                let tx, ty = transformDelta dx dy
                let endX, endY = clampToWindow (currentX + tx) (currentY + ty)
                drawLine currentX currentY endX endY
                currentX <- endX
                currentY <- endY
            cursor <- currentX, currentY
        member _.DLine(values) =
            let coordinates =
                values
                |> List.chunkBySize 2
                |> List.choose (function
                    | [ x; y ] -> Some(x, y)
                    | _ -> None)

            let transformed = coordinates |> List.map (fun (x, y) -> transformAbsolute x y)
            match transformed with
            | [] -> ()
            | [ x, y ] ->
                cursor <- x, y
                drawPixel x y
            | _ ->
                transformed
                |> List.pairwise
                |> List.iter (fun ((x1, y1), (x2, y2)) -> drawLine x1 y1 x2 y2)
                cursor <- List.last transformed
        member _.Line(x1, y1, x2, y2) =
            let tx1, ty1 = transformAbsolute x1 y1
            let tx2, ty2 = transformAbsolute x2 y2
            drawLine tx1 ty1 tx2 ty2
            cursor <- tx2, ty2
        member _.Circle(x, y, radius) =
            let tx, ty = transformAbsolute x y
            let radiusX, radiusY = scaledRadius radius
            cursor <- tx, ty
            sampleCircle tx ty radiusX radiusY true
        member _.CircleRelative(dx, dy, radius) =
            let x, y = cursor
            let tx, ty = transformDelta dx dy
            let endX, endY = clampToWindow (x + tx) (y + ty)
            cursor <- endX, endY
            let radiusX, radiusY = scaledRadius radius
            sampleCircle endX endY radiusX radiusY true
        member _.Ellipse(x, y, radius, ratio, angle) =
            let tx, ty = transformAbsolute x y
            let radiusX, radiusY = scaledRadius radius
            let ellipseRadiusY = max 1.0 (radiusY * max 0.05 (abs ratio))
            cursor <- tx, ty
            sampleEllipse tx ty radiusX ellipseRadiusY angle 0.0 360.0 true true
        member _.EllipseRelative(dx, dy, radius, ratio, angle) =
            let x, y = cursor
            let tx, ty = transformDelta dx dy
            let endX, endY = clampToWindow (x + tx) (y + ty)
            cursor <- endX, endY
            let radiusX, radiusY = scaledRadius radius
            let ellipseRadiusY = max 1.0 (radiusY * max 0.05 (abs ratio))
            sampleEllipse endX endY radiusX ellipseRadiusY angle 0.0 360.0 true true
        member _.Arc(x, y, radius, startAngle, endAngle) =
            let tx, ty = transformAbsolute x y
            let radiusX, radiusY = scaledRadius radius
            sampleEllipse tx ty radiusX radiusY 0.0 startAngle endAngle false false
            cursor <- arcEndPoint tx ty radiusX radiusY endAngle
        member _.ArcRelative(dx, dy, radius, startAngle, endAngle) =
            let x, y = cursor
            let tx, ty = transformDelta dx dy
            let endX, endY = clampToWindow (x + tx) (y + ty)
            let radiusX, radiusY = scaledRadius radius
            sampleEllipse endX endY radiusX radiusY 0.0 startAngle endAngle false false
            cursor <- arcEndPoint endX endY radiusX radiusY endAngle
        member _.Block(width, height, x, y, color) =
            let tx, ty = transformAbsolute x y
            cursor <- tx, ty
            let startX = int (Math.Round tx)
            let startY = int (Math.Round ty)
            for row = startY to startY + max 0 (int (Math.Round height)) - 1 do
                for col = startX to startX + max 0 (int (Math.Round width)) - 1 do
                    buffer.ApplyPixel(col, row, color, overMode, underMode, flashMode, drawingBackgroundColor)
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

type private DefaultInputDevice(reader: unit -> string option, readKey: unit -> KeyInfo option, keyAvailable: unit -> bool, keyRowState: int -> int, flushInput: unit -> unit) =
    interface IInputDevice with
        member _.ReadLine() = reader ()
        member _.ReadKey() = readKey ()
        member _.KeyAvailable() = keyAvailable ()
        member _.GetKeyRow(row) = keyRowState row
        member _.Flush() =
            flushInput ()
            while keyAvailable () do
                readKey () |> ignore

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
        member _.ListDirectory(_pathSpec) =
            Result.Error(UnsupportedHostOperation "Directory listing is not implemented in DefaultHost.")
        member _.Exists(_path) = false
        member _.Delete(_path) =
            Result.Error(UnsupportedHostOperation "File deletion is not implemented in DefaultHost.")

type private DefaultRuntimeHost(options: DefaultHostOptions) =
    let mutable currentMode = ScreenDefaults.supportedModes[0]
    let screenBuffer = ScreenBuffer(currentMode)
    let memory = Dictionary<int, byte>()
    let screenReader channelId = options.ReadScreenLine channelId
    let channel0 = DefaultScreenChannel(ChannelId 0, ChannelKind.ConsoleChannel, screenReader, options.WriteLine, true, currentMode, None, screenBuffer)
    let channel1 = DefaultScreenChannel(ChannelId 1, ChannelKind.ScreenChannel, screenReader, options.WriteLine, false, currentMode, None, screenBuffer)
    let channel2 = DefaultScreenChannel(ChannelId 2, ChannelKind.ScreenChannel, screenReader, options.WriteLine, false, currentMode, None, screenBuffer)
    let channelManager =
        [ channel0 :> IChannel
          channel1 :> IChannel
          channel2 :> IChannel ]
        |> fun defaults -> DefaultChannelManager(defaults, screenReader, options.WriteLine, (fun () -> currentMode), screenBuffer)
    let graphics = DefaultGraphicsDevice(screenBuffer) :> IGraphicsDevice
    let input = DefaultInputDevice(options.ReadLine, options.ReadKey, options.KeyAvailable, options.KeyRowState, options.FlushInput) :> IInputDevice
    let sound = NullSoundDevice() :> ISoundDevice
    let tryReadByte address =
        if address < 0 then
            Result.Error(InvalidHostArgument $"Memory address {address} is invalid.")
        else
            match memory.TryGetValue address with
            | true, value -> Result.Ok value
            | false, _ -> Result.Ok 0uy
    let writeByte address value =
        if address < 0 then
            Result.Error(InvalidHostArgument $"Memory address {address} is invalid.")
        else
            memory[address] <- value
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
            member _.ListDirectory(pathSpec) =
                DevicePaths.listDirectoryEntries pathSpec
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
    let memoryDevice =
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
                pokeLong address value }

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
                member _.SetRecolor(values) = (channel1 :> IScreenChannel).SetRecolor(values)
                member _.GetRecolor() = (channel1 :> IScreenChannel).GetRecolor()
                member _.SetPalette(values) = (channel1 :> IScreenChannel).SetPalette(values)
                member _.GetPalette() = (channel1 :> IScreenChannel).GetPalette()
                member _.SetCursor(x, y) = (channel1 :> IScreenChannel).SetCursor(x, y)
                member _.GetCursor() = (channel1 :> IScreenChannel).GetCursor()
                member _.SetCharacterSize(width, height) = (channel1 :> IScreenChannel).SetCharacterSize(width, height)
                member _.GetCharacterSize() = (channel1 :> IScreenChannel).GetCharacterSize()
                member _.SetCharacterFonts(font1, font2) = (channel1 :> IScreenChannel).SetCharacterFonts(font1, font2)
                member _.GetCharacterFonts() = (channel1 :> IScreenChannel).GetCharacterFonts()
                member _.WriteText(text) = (channel1 :> IScreenChannel).WriteText(text)
                member _.SetInk(values) = (channel1 :> IScreenChannel).SetInk(values)
                member _.SetPaper(value) = (channel1 :> IScreenChannel).SetPaper(value)
                member _.SetStrip(values) = (channel1 :> IScreenChannel).SetStrip(values)
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
        member _.Memory = memoryDevice

module DefaultHost =
    let create options : IRuntimeHost =
        DefaultRuntimeHost(options) :> IRuntimeHost

    let createWithDisplay options : IRuntimeHost * IDisplaySurface =
        let host = DefaultRuntimeHost(options)
        (host :> IRuntimeHost), host.DisplaySurface
