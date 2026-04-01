namespace SBAvaloniaHost

open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open SBRuntime

type RuntimeSurfaceControl() =
    inherit Control()

    static let textCellPixelWidth = 8
    static let textCellPixelHeight = 10
    static let consoleTypeface = Typeface(FontFamily("Consolas, Courier New, Monospace"))

    static let palette =
        [| Color.Parse("#000000")
           Color.Parse("#F5F0E6")
           Color.Parse("#D96248")
           Color.Parse("#6EA8A1")
           Color.Parse("#D1A454")
           Color.Parse("#5A8F6B")
           Color.Parse("#89A7D8")
           Color.Parse("#F9E4B7") |]

    let colorFor value =
        let index = abs value % palette.Length
        palette[index]

    member val DisplaySurface: IDisplaySurface option = None with get, set

    member this.AttachDisplaySurface(surface: IDisplaySurface) =
        this.DisplaySurface <- Some surface
        this.InvalidateVisual()

    override this.Render(context: DrawingContext) =
        base.Render(context)

        let bounds = Rect(0.0, 0.0, this.Bounds.Width, this.Bounds.Height)
        let background = SolidColorBrush(Color.Parse("#10151B"))
        let frame = Pen(SolidColorBrush(Color.Parse("#C78352")), 2.0)
        let paneFrame = Pen(SolidColorBrush(Color.Parse("#6EA8A1")), 1.0)

        context.FillRectangle(background, bounds)
        context.DrawRectangle(frame, bounds.Deflate(12.0))

        match this.DisplaySurface with
        | None ->
            ()
        | Some surface ->
            let snapshot = surface.GetSnapshot()
            let margin = 24.0
            let drawArea = bounds.Deflate(margin)
            let modeWidth = max 1 snapshot.Mode.Width
            let modeHeight = max 1 snapshot.Mode.Height
            let scaleX = drawArea.Width / float modeWidth
            let scaleY = drawArea.Height / float modeHeight
            let paneZOrder pane =
                match pane.ChannelId with
                | Some 2 -> 0
                | Some 1 -> 1
                | Some 0 -> 1000
                | Some id -> 100 + id
                | None -> 500

            snapshot.Panes
            |> List.sortBy paneZOrder
            |> List.iteri (fun index pane ->
                let width, height, x, y = pane.Window
                let paneRect =
                    Rect(
                        drawArea.X + (float x * scaleX),
                        drawArea.Y + (float y * scaleY),
                        max 24.0 (float width * scaleX),
                        max 24.0 (float height * scaleY))
                let innerRect = paneRect.Deflate(10.0)
                let viewportRect = innerRect
                let paneBackground =
                    let alpha = if index = 0 then 255uy else 205uy
                    SolidColorBrush(Color.FromArgb(alpha, 22uy, 32uy, 40uy))

                context.FillRectangle(paneBackground, paneRect)
                context.DrawRectangle(paneFrame, paneRect)

                let paneTextHeight = Array2D.length1 pane.Text
                let paneTextWidth = Array2D.length2 pane.Text
                let textRows = max 1 (height / textCellPixelHeight)
                let textCols = max 1 (width / textCellPixelWidth)

                if paneTextHeight > 0 && paneTextWidth > 0 then
                    let cellWidth = max 1.0 (viewportRect.Width / float textCols)
                    let cellHeight = max 1.0 (viewportRect.Height / float textRows)
                    let defaultPaper = pane.Text[0, 0].Paper
                    let fontSize = max 12.0 (min (cellHeight * 0.78) (cellWidth * 1.4))
                    let _, cursorY = pane.Cursor
                    let rowOffset =
                        let lastVisibleRow = max 0 cursorY
                        max 0 (min (paneTextHeight - textRows) (lastVisibleRow - textRows + 1))

                    context.FillRectangle(SolidColorBrush(colorFor defaultPaper), viewportRect)

                    use _clip = context.PushClip(viewportRect)

                    for textRow = 0 to min (textRows - 1) (paneTextHeight - 1) do
                        for textCol = 0 to min (textCols - 1) (paneTextWidth - 1) do
                            let cell = pane.Text[rowOffset + textRow, textCol]
                            let cellRect =
                                Rect(
                                    viewportRect.X + (float textCol * cellWidth),
                                    viewportRect.Y + (float textRow * cellHeight),
                                    cellWidth,
                                    cellHeight)
                            context.FillRectangle(SolidColorBrush(colorFor cell.Paper), cellRect)

                            if cell.Character <> ' ' then
                                let text =
                                    new FormattedText(
                                        string cell.Character,
                                        CultureInfo.InvariantCulture,
                                        FlowDirection.LeftToRight,
                                        consoleTypeface,
                                        fontSize,
                                        SolidColorBrush(colorFor cell.Ink))
                                let textLocation =
                                    Point(
                                        cellRect.X + max 0.0 ((cellRect.Width - text.Width) / 2.0),
                                        cellRect.Y + max 0.0 ((cellRect.Height - text.Height) / 2.0))
                                context.DrawText(text, textLocation))
