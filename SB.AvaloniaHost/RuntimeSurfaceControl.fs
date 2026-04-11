namespace SBAvaloniaHost

open System
open System.Runtime.InteropServices
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform
open SBDisplay
open SBRuntime

type RuntimeSurfaceControl() as this =
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

    let pixelColorFor mode recolor palette screenX screenY colorSpec =
        let flash = (colorSpec &&& flashBit) <> 0
        let baseColorSpec = colorSpec &&& colorMask

        if flash && not isFlashVisible then
            Color.Parse("#000000")
        elif baseColorSpec >= 0 && baseColorSpec <= 7 then
            colorFor mode recolor palette baseColorSpec
        else
            let mainColor, contrastColor, stipple = decodeCompositeColor baseColorSpec
            let useContrast =
                match stipple with
                | 0 -> (screenX % 2 <> 0) && (screenY % 2 <> 0)
                | 1 -> screenX % 2 <> 0
                | 2 -> screenY % 2 <> 0
                | _ -> (screenX + screenY) % 2 <> 0

            colorFor mode recolor palette (if useContrast then contrastColor else mainColor)

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

    let paneHasLocalText (pane: ScreenPaneSnapshot) =
        seq {
            for row = 0 to Array2D.length1 pane.Text - 1 do
                for col = 0 to Array2D.length2 pane.Text - 1 do
                    let cell = pane.Text[row, col]
                    yield cell.Character <> ' ' || cell.HasBackground
        }
        |> Seq.exists id

    let drawPaneTextDirectly (context: DrawingContext) (modeInfo: ScreenModeInfo) recolor palette (pane: ScreenPaneSnapshot) (viewportRect: Rect) (scale: float) =
        let x = let _, _, px, _ = pane.Window in px
        let y = let _, _, _, py = pane.Window in py
        let charWidthScale, charHeightScale =
            let widthScale, heightScale = pane.CharacterSize
            max 1 (widthScale + 1), max 1 (heightScale + 1)
        let logicalCellWidth = modeInfo.BaseTextCellWidth * charWidthScale
        let logicalCellHeight = modeInfo.BaseTextCellHeight * charHeightScale
        let logicalAdvance =
            if modeInfo.IsQlCompatible && pane.CharacterSize = (0, 0) then
                logicalCellWidth
            else
                max 1 (logicalCellWidth - max 4 ((2 * logicalCellWidth) / 3))

        fillRectangleWithColorSpec context modeInfo.Mode recolor palette x y viewportRect pane.Paper

        for row = 0 to Array2D.length1 pane.Text - 1 do
            for col = 0 to Array2D.length2 pane.Text - 1 do
                let cell = pane.Text[row, col]
                let originLogicalX = col * logicalAdvance
                let originLogicalY = row * logicalCellHeight

                if cell.HasBackground then
                    let cellRect =
                        Rect(
                            viewportRect.X + (float originLogicalX * scale),
                            viewportRect.Y + (float originLogicalY * scale),
                            float logicalAdvance * scale,
                            float logicalCellHeight * scale)
                    fillRectangleWithColorSpec context modeInfo.Mode recolor palette (x + originLogicalX) (y + originLogicalY) cellRect cell.Strip

                if cell.Character <> ' ' then
                    let glyph = QlBitmapFont.glyphForCharacter cell.CodePoint cell.Character
                    let glyphRows = min 8 glyph.Length
                    let renderWidth = min logicalAdvance 8
                    let renderHeight = min logicalCellHeight glyphRows
                    let verticalInset = max 0 ((logicalCellHeight - renderHeight) / 2)

                    for glyphRow = 0 to glyphRows - 1 do
                        let pattern = int glyph[glyphRow]
                        for targetCol = 0 to renderWidth - 1 do
                            let glyphCol = min 7 (int (float targetCol * 8.0 / float renderWidth))
                            if (pattern >>> glyphCol) &&& 1 = 1 then
                                let screenPixelX = x + originLogicalX + targetCol
                                let screenPixelY = y + originLogicalY + verticalInset + glyphRow
                                let brush =
                                    SolidColorBrush(pixelColorFor modeInfo.Mode recolor palette screenPixelX screenPixelY cell.Ink)
                                let pixelRect =
                                    Rect(
                                        viewportRect.X + (float (originLogicalX + targetCol) * scale),
                                        viewportRect.Y + (float (originLogicalY + verticalInset + glyphRow) * scale),
                                        scale,
                                        scale)
                                context.FillRectangle(brush, pixelRect)

    let paneHasVisibleContent (pane: ScreenPaneSnapshot) =
        let hasText = paneHasLocalText pane

        let hasNonPaperPixels =
            seq {
                for row = 0 to Array2D.length1 pane.Surface - 1 do
                    for col = 0 to Array2D.length2 pane.Surface - 1 do
                        yield (pane.Surface[row, col] &&& colorMask) <> (pane.Paper &&& colorMask)
            }
            |> Seq.exists id

        hasText
        || hasNonPaperPixels
        || pane.BorderSize > 0
        || pane.Recolor.IsSome
        || pane.Palette.IsSome

    do
        RenderOptions.SetBitmapInterpolationMode(this, BitmapInterpolationMode.None)

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
            let rawScale = min (drawArea.Width / float modeWidth) (drawArea.Height / float modeHeight)
            let scale = max 1.0 (Math.Floor rawScale)
            let screenWidth = float modeWidth * scale
            let screenHeight = float modeHeight * scale
            let screenOriginX = Math.Floor(drawArea.X + ((drawArea.Width - screenWidth) / 2.0))
            let screenOriginY = Math.Floor(drawArea.Y + ((drawArea.Height - screenHeight) / 2.0))
            let paneZOrder pane =
                match pane.ChannelId with
                | Some 2 -> 0
                | Some 1 -> 1
                | Some 0 -> 1000
                | Some id -> 100 + id
                | None -> 500

            let visiblePanes =
                snapshot.Panes
                |> List.filter (fun pane -> pane.ChannelId <> Some 0)
                |> List.filter (fun pane ->
                    match pane.ChannelId with
                    | Some 2 -> paneHasVisibleContent pane
                    | _ -> true)
                |> List.sortBy paneZOrder

            visiblePanes
            |> List.iteri (fun index pane ->
                let outerWidth, outerHeight, outerX, outerY = pane.OuterWindow
                let width, height, x, y = pane.Window
                let paneRect =
                    Rect(
                        screenOriginX + (float outerX * scale),
                        screenOriginY + (float outerY * scale),
                        max 24.0 (float outerWidth * scale),
                        max 24.0 (float outerHeight * scale))
                let viewportRect =
                    Rect(
                        screenOriginX + (float x * scale),
                        screenOriginY + (float y * scale),
                        max 1.0 (float width * scale),
                        max 1.0 (float height * scale))

                let charWidthScale, charHeightScale =
                    let widthScale, heightScale = pane.CharacterSize
                    max 1 (widthScale + 1), max 1 (heightScale + 1)
                let logicalCellWidth = snapshot.Mode.BaseTextCellWidth * charWidthScale
                let logicalCellHeight = snapshot.Mode.BaseTextCellHeight * charHeightScale
                let logicalAdvance =
                    if snapshot.Mode.IsQlCompatible && pane.CharacterSize = (0, 0) then
                        logicalCellWidth
                    else
                        max 1 (logicalCellWidth - max 4 ((2 * logicalCellWidth) / 3))
                let textRows = max 1 (height / logicalCellHeight)
                let textCols = max 1 (width / logicalAdvance)

                match pane.BorderColor with
                | Some borderColor when pane.BorderSize > 0 ->
                    if viewportRect.Y > paneRect.Y then
                        let topRect = Rect(paneRect.X, paneRect.Y, paneRect.Width, viewportRect.Y - paneRect.Y)
                        fillRectangleWithColorSpec context snapshot.Mode.Mode pane.Recolor pane.Palette outerX outerY topRect borderColor
                    if viewportRect.Bottom < paneRect.Bottom then
                        let bottomRect = Rect(paneRect.X, viewportRect.Bottom, paneRect.Width, paneRect.Bottom - viewportRect.Bottom)
                        fillRectangleWithColorSpec context snapshot.Mode.Mode pane.Recolor pane.Palette outerX outerY bottomRect borderColor
                    if viewportRect.X > paneRect.X then
                        let leftRect = Rect(paneRect.X, viewportRect.Y, viewportRect.X - paneRect.X, viewportRect.Height)
                        fillRectangleWithColorSpec context snapshot.Mode.Mode pane.Recolor pane.Palette outerX outerY leftRect borderColor
                    if viewportRect.Right < paneRect.Right then
                        let rightRect = Rect(viewportRect.Right, viewportRect.Y, paneRect.Right - viewportRect.Right, viewportRect.Height)
                        fillRectangleWithColorSpec context snapshot.Mode.Mode pane.Recolor pane.Palette outerX outerY rightRect borderColor
                | _ -> ()

                use _clip = context.PushClip(viewportRect)

                let paneSurfaceHeight = Array2D.length1 pane.Surface
                let paneSurfaceWidth = Array2D.length2 pane.Surface

                let drawPaneSurface = true

                if drawPaneSurface && paneSurfaceHeight > 0 && paneSurfaceWidth > 0 then
                    use bitmap =
                        new WriteableBitmap(
                            PixelSize(paneSurfaceWidth, paneSurfaceHeight),
                            Vector(96.0, 96.0),
                            PixelFormat.Bgra8888,
                            AlphaFormat.Opaque)

                    use framebuffer = bitmap.Lock()
                    let bytes = Array.zeroCreate<byte> (framebuffer.RowBytes * paneSurfaceHeight)

                    for py = 0 to paneSurfaceHeight - 1 do
                        let rowOffset = py * framebuffer.RowBytes
                        for px = 0 to paneSurfaceWidth - 1 do
                            let color = pixelColorFor snapshot.Mode.Mode pane.Recolor pane.Palette (x + px) (y + py) pane.Surface[py, px]
                            let pixelOffset = rowOffset + (px * 4)
                            bytes[pixelOffset] <- color.B
                            bytes[pixelOffset + 1] <- color.G
                            bytes[pixelOffset + 2] <- color.R
                            bytes[pixelOffset + 3] <- color.A

                    Marshal.Copy(bytes, 0, framebuffer.Address, bytes.Length)
                    context.DrawImage(bitmap, Rect(0.0, 0.0, float paneSurfaceWidth, float paneSurfaceHeight), viewportRect)
                elif paneHasLocalText pane then
                    drawPaneTextDirectly context snapshot.Mode pane.Recolor pane.Palette pane viewportRect scale

                let cellWidth = float logicalCellWidth * scale
                let cellHeight = float logicalCellHeight * scale
                let cellAdvance = float logicalAdvance * scale

                let cursorX, cursorY = pane.Cursor
                if pane.CursorVisible && cursorX >= 0 && cursorX < textCols && cursorY >= 0 && cursorY < textRows then
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
