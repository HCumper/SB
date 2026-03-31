namespace SBAvaloniaHost

open System
open System.Text
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open SBRuntime

type MainWindow(_runtimeHost: IRuntimeHost, display: IDisplaySurface, enqueueKey: KeyInfo -> unit, enqueueLine: string -> unit) as this =
    inherit Window()

    let surface = RuntimeSurfaceControl()
    let repaintTimer = DispatcherTimer()
    let lineBuffer = StringBuilder()

    let enqueuePrintableText (text: string) =
        if not (String.IsNullOrEmpty text) then
            for ch in text do
                enqueueKey {
                    KeyCode = int ch
                    Character = Some ch
                    Shift = false
                    Control = false
                }

    let tryMapSpecialKey (args: KeyEventArgs) =
        let keyCode =
            match args.Key with
            | Key.Enter -> Some 13
            | Key.Tab -> Some 9
            | Key.Back -> Some 8
            | Key.Escape -> Some 27
            | Key.Space -> Some 32
            | Key.Left -> Some 28
            | Key.Right -> Some 29
            | Key.Up -> Some 30
            | Key.Down -> Some 31
            | _ -> None

        keyCode
        |> Option.map (fun code ->
            { KeyCode = code
              Character = if code >= 32 && code <= 126 then Some(char code) else None
              Shift = args.KeyModifiers.HasFlag(KeyModifiers.Shift)
              Control = args.KeyModifiers.HasFlag(KeyModifiers.Control) })

    let submitInputLine () =
        let text = lineBuffer.ToString()
        enqueueLine text
        lineBuffer.Clear() |> ignore

    do
        this.Title <- "SB Runtime Host"
        this.Width <- 980.0
        this.Height <- 720.0
        this.MinWidth <- 720.0
        this.MinHeight <- 540.0
        this.WindowState <- WindowState.Maximized
        this.Background <- Brushes.Black
        surface.AttachDisplaySurface(display)
        this.Focusable <- true
        surface.Focusable <- false
        this.TextInput.Add(fun args ->
            let text = args.Text |> Option.ofObj |> Option.defaultValue String.Empty
            if not (String.IsNullOrEmpty text) then
                enqueuePrintableText text
                lineBuffer.Append(text) |> ignore)
        this.KeyDown.Add(fun args ->
            match tryMapSpecialKey args with
            | Some keyInfo ->
                match args.Key with
                | Key.Enter ->
                    enqueueKey keyInfo
                    submitInputLine()
                    args.Handled <- true
                | Key.Back ->
                    enqueueKey keyInfo
                    if lineBuffer.Length > 0 then
                        lineBuffer.Remove(lineBuffer.Length - 1, 1) |> ignore
                    args.Handled <- true
                | _ ->
                    enqueueKey keyInfo
            | None -> ())
        repaintTimer.Interval <- TimeSpan.FromMilliseconds(33.0)
        repaintTimer.Tick.Add(fun _ -> surface.InvalidateVisual())
        repaintTimer.Start()
        this.Content <- surface
        this.Opened.Add(fun _ -> this.Focus() |> ignore)

    member _.AppendOutput(text: string) =
        ()
