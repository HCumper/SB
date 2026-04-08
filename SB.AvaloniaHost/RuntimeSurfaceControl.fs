namespace SBAvaloniaHost

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open SBDisplay
open SBRuntime

type RuntimeSurfaceControl() =
    inherit Control()

    static let flashBit = 0x01000000
    static let colorMask = 0x00FFFFFF

    static let qlPalette =
        [| Color.Parse("#000000")
           Color.Parse("#0000FF")
           Color.Parse("#FF0000")
           Color.Parse("#FF00FF")
           Color.Parse("#00FF00")
           Color.Parse("#00FFFF")
           Color.Parse("#FFFF00")
           Color.Parse("#FFFFFF") |]

    let legalQlColor (mode: ScreenMode) color =
        let normalized = abs color % 8
        match mode with
        | QlMode4
        | ExtendedMode 512 ->
            match normalized with
            | 0
            | 1 -> 0
            | 2
            | 3 -> 2
            | 4
            | 5 -> 4
            | _ -> 7
        | _ ->
            normalized

    let paletteEntry entries index fallback =
        entries
        |> Option.bind (fun values -> List.tryItem index values)
        |> Option.defaultValue fallback

    let recoloredColor mode recolor color =
        let logicalColor = abs color % 8
        let remapped = paletteEntry recolor logicalColor logicalColor
        legalQlColor mode remapped

    let paletteMappedColor palette color =
        paletteEntry palette color color

    let colorFor mode recolor palette color =
        let index =
            color
            |> recoloredColor mode recolor
            |> paletteMappedColor palette
            |> legalQlColor mode

        qlPalette[index]

    let decodeCompositeColor value =
        let composite = abs value % 256
        let mainColor = composite &&& 0b111
        let xorBits = (composite >>> 3) &&& 0b111
        let stipple = (composite >>> 6) &&& 0b11
        let contrastColor = mainColor ^^^ xorBits
        mainColor, contrastColor, stipple

    let isFlashVisible =
        let phase = DateTime.UtcNow.Ticks / TimeSpan.TicksPerMillisecond / 300L
        phase % 2L = 0L

    let fillRectangleWithColorSpec (context: DrawingContext) mode recolor palette screenX screenY (rect: Rect) colorSpec =
        let flash = (colorSpec &&& flashBit) <> 0
        let baseColorSpec = colorSpec &&& colorMask

        if flash && not isFlashVisible then
            ()
        elif baseColorSpec >= 0 && baseColorSpec <= 7 then
            context.FillRectangle(SolidColorBrush(colorFor mode recolor palette baseColorSpec), rect)
        else
            let mainColor, contrastColor, stipple = decodeCompositeColor baseColorSpec
            let mainBrush = SolidColorBrush(colorFor mode recolor palette mainColor)
            let contrastBrush = SolidColorBrush(colorFor mode recolor palette contrastColor)
            let halfWidth = rect.Width / 2.0
            let halfHeight = rect.Height / 2.0

            let fillAt left top useContrast =
                let target =
                    Rect(
                        rect.X + (if left then halfWidth else 0.0),
                        rect.Y + (if top then halfHeight else 0.0),
                        halfWidth,
                        halfHeight)
                context.FillRectangle((if useContrast then contrastBrush else mainBrush), target)

            match stipple with
            | 0 ->
                fillAt false false false
                fillAt true false false
                fillAt false true false
                fillAt true true true
            | 1 ->
                fillAt false false false
                fillAt true false true
                fillAt false true false
                fillAt true true true
            | 2 ->
                fillAt false false false
                fillAt true false false
                fillAt false true true
                fillAt true true true
            | _ ->
                let invert = (screenX + screenY) % 2 <> 0
                fillAt false false invert
                fillAt true false (not invert)
                fillAt false true (not invert)
                fillAt true true invert

    let drawGlyph (context: DrawingContext) (brush: IBrush) (cellRect: Rect) (glyph: byte array) =
        let columns = 8
        let rows = min 8 glyph.Length
        let pixelWidth = max 1.0 (cellRect.Width / float columns)
        let pixelHeight = max 1.0 (cellRect.Height / float rows)
        for row = 0 to rows - 1 do
            let pattern = int glyph[row]
            for col = 0 to columns - 1 do
                if (pattern >>> col) &&& 1 = 1 then
                    let pixelRect =
                        Rect(
                            cellRect.X + (float col * pixelWidth),
                            cellRect.Y + (float row * pixelHeight),
                            pixelWidth,
                            pixelHeight)
                    context.FillRectangle(brush, pixelRect)

    member val DisplaySurface: IDisplaySurface option = None with get, set

    member this.AttachDisplaySurface(surface: IDisplaySurface) =
        this.DisplaySurface <- Some surface
        this.InvalidateVisual()

    override this.Render(context: DrawingContext) =
        base.Render(context)

        let bounds = Rect(0.0, 0.0, this.Bounds.Width, this.Bounds.Height)
        let background = SolidColorBrush(Color.Parse("#10151B"))

        context.FillRectangle(background, bounds)

        match this.DisplaySurface with
        | None ->
            ()
        | Some surface ->
            let snapshot = surface.GetSnapshot()
            let drawArea = bounds
            let modeWidth = max 1 snapshot.Mode.Width
            let modeHeight = max 1 snapshot.Mode.Height
            let scale = min (drawArea.Width / float modeWidth) (drawArea.Height / float modeHeight)
            let screenOriginX = drawArea.X
            let screenOriginY = drawArea.Y
            let paneZOrder pane =
                match pane.ChannelId with
                | Some 2 -> 0
                | Some 1 -> 1
                | Some 0 -> 1000
                | Some id -> 100 + id
                | None -> 500

            snapshot.Panes
            |> List.filter (fun pane -> pane.ChannelId <> Some 0)
            |> List.sortBy paneZOrder
            |> List.iteri (fun index pane ->
                let width, height, x, y = pane.Window
                let borderThickness =
                    if pane.Border = 0 then 0.0
                    else max 1.0 scale
                let paneRect =
                    Rect(
                        screenOriginX + (float x * scale),
                        screenOriginY + (float y * scale),
                        max 24.0 (float width * scale),
                        max 24.0 (float height * scale))
                let viewportRect =
                    if borderThickness > 0.0 then paneRect.Deflate(borderThickness)
                    else paneRect

                fillRectangleWithColorSpec context snapshot.Mode.Mode pane.Recolor pane.Palette x y paneRect pane.Border
                if borderThickness > 0.0 then
                    fillRectangleWithColorSpec context snapshot.Mode.Mode pane.Recolor pane.Palette x y viewportRect pane.Paper

                let paneSurfaceHeight = Array2D.length1 pane.Surface
                let paneSurfaceWidth = Array2D.length2 pane.Surface
                let charWidthScale, charHeightScale =
                    let widthScale, heightScale = pane.CharacterSize
                    max 1 (widthScale + 1), max 1 (heightScale + 1)
                let logicalCellWidth = snapshot.Mode.BaseTextCellWidth * charWidthScale
                let logicalCellHeight = snapshot.Mode.BaseTextCellHeight * charHeightScale
                let logicalAdvance = max 1 (logicalCellWidth - max 4 ((2 * logicalCellWidth) / 3))
                let textRows = max 1 (height / logicalCellHeight)
                let textCols = max 1 (width / logicalCellWidth)

                use _clip = context.PushClip(viewportRect)

                if paneSurfaceHeight > 0 && paneSurfaceWidth > 0 then
                    let pixelWidth = max 1.0 (viewportRect.Width / float paneSurfaceWidth)
                    let pixelHeight = max 1.0 (viewportRect.Height / float paneSurfaceHeight)

                    for py = 0 to paneSurfaceHeight - 1 do
                        for px = 0 to paneSurfaceWidth - 1 do
                            let pixel = pane.Surface[py, px]
                            let pixelRect =
                                Rect(
                                    viewportRect.X + (float px * pixelWidth),
                                    viewportRect.Y + (float py * pixelHeight),
                                    pixelWidth,
                                    pixelHeight)
                            fillRectangleWithColorSpec context snapshot.Mode.Mode pane.Recolor pane.Palette (x + px) (y + py) pixelRect pixel

                let cellWidth = float logicalCellWidth * scale
                let cellHeight = float logicalCellHeight * scale
                let cellAdvance = float logicalAdvance * scale
                let cursorX, cursorY = pane.Cursor
                if cursorX >= 0 && cursorX < textCols && cursorY >= 0 && cursorY < textRows then
                    let cursorRect =
                        Rect(
                            viewportRect.X + (float cursorX * cellAdvance),
                            viewportRect.Y + (float cursorY * cellHeight),
                            cellAdvance,
                            cellHeight)
                    let caretHeight = max 2.0 (scale * 2.0)
                    let caretRect =
                        Rect(
                            cursorRect.X,
                            cursorRect.Bottom - caretHeight,
                            cursorRect.Width,
                            caretHeight)
                    context.FillRectangle(SolidColorBrush(Color.Parse("#FFFFFF")), caretRect))
