namespace SBAvaloniaHost

open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open SBRuntime

type RuntimeSurfaceControl() =
    inherit Control()

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
            let headerHeight = 22.0
            let drawArea = bounds.Deflate(margin)
            let modeWidth = max 1 snapshot.Mode.Width
            let modeHeight = max 1 snapshot.Mode.Height
            let scaleX = drawArea.Width / float modeWidth
            let scaleY = drawArea.Height / float modeHeight

            snapshot.Panes
            |> List.iteri (fun index pane ->
                let width, height, x, y = pane.Window
                let paneRect =
                    Rect(
                        drawArea.X + (float x * scaleX),
                        drawArea.Y + (float y * scaleY),
                        max 24.0 (float width * scaleX),
                        max 24.0 (float height * scaleY))
                let innerRect = paneRect.Deflate(10.0)
                let viewportRect = Rect(innerRect.X, innerRect.Y + headerHeight, innerRect.Width, innerRect.Height - headerHeight)
                let paneBackground =
                    let alpha = if index = 0 then 255uy else 205uy
                    SolidColorBrush(Color.FromArgb(alpha, 22uy, 32uy, 40uy))
                let titleText =
                    new FormattedText(
                        pane.Title,
                        CultureInfo.InvariantCulture,
                        FlowDirection.LeftToRight,
                        Typeface.Default,
                        14.0,
                        SolidColorBrush(Color.Parse("#F9E4B7")))

                context.FillRectangle(paneBackground, paneRect)
                context.DrawRectangle(paneFrame, paneRect)
                context.DrawText(titleText, Point(innerRect.X + 4.0, innerRect.Y))

                let paneTextHeight = Array2D.length1 pane.Text
                let paneTextWidth = Array2D.length2 pane.Text

                if paneTextHeight > 0 && paneTextWidth > 0 then
                    let cellWidth = max 1.0 (viewportRect.Width / float paneTextWidth)
                    let cellHeight = max 1.0 (viewportRect.Height / float paneTextHeight)
                    let fontSize = max 10.0 (min 18.0 (cellHeight * 0.85))

                    for textRow = 0 to paneTextHeight - 1 do
                        for textCol = 0 to paneTextWidth - 1 do
                            let cell = pane.Text[textRow, textCol]
                            if cell.Character <> ' ' then
                                let location =
                                    Point(
                                        viewportRect.X + (float textCol * cellWidth),
                                        viewportRect.Y + (float textRow * cellHeight))
                                let text =
                                    new FormattedText(
                                        string cell.Character,
                                        CultureInfo.InvariantCulture,
                                        FlowDirection.LeftToRight,
                                        Typeface.Default,
                                        fontSize,
                                        SolidColorBrush(colorFor cell.Ink))
                                context.DrawText(text, location))
