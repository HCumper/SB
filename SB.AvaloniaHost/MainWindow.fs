namespace SBAvaloniaHost

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Threading
open SBRuntime

type MainWindow(display: IDisplaySurface, handleTextInput: string -> unit, handleSpecialKey: KeyInfo -> bool, pulseGameplayKey: unit -> unit, releaseGameplayKey: unit -> unit, releaseSpecialKey: KeyInfo -> unit) as this =
    inherit Window()

    let surface = RuntimeSurfaceControl()
    let repaintTimer = DispatcherTimer()

    let tryMapSpecialKey (args: KeyEventArgs) =
        let keyCode =
            match args.Key with
            | Key.Enter -> Some 13
            | Key.Tab -> Some 9
            | Key.Back -> Some 8
            | Key.Delete -> Some 127
            | Key.Escape -> Some 27
            | Key.Space -> Some 32
            | Key.Left -> Some 28
            | Key.Right -> Some 29
            | Key.Up -> Some 30
            | Key.Down -> Some 31
            | Key.Home -> Some 1
            | Key.End -> Some 5
            | _ -> None

        keyCode
        |> Option.map (fun code ->
            { KeyCode = code
              Character = if code >= 32 && code <= 126 then Some(char code) else None
              Shift = args.KeyModifiers.HasFlag(KeyModifiers.Shift)
              Control = args.KeyModifiers.HasFlag(KeyModifiers.Control) })

    let handlePrintableKey (args: KeyEventArgs) =
        let text = args.KeySymbol |> Option.ofObj |> Option.defaultValue String.Empty
        if not (String.IsNullOrEmpty text) && not (args.KeyModifiers.HasFlag(KeyModifiers.Control)) then
            handleTextInput text
            surface.InvalidateVisual()
            args.Handled <- true
            true
        else
            false

    let handleKeyDown (args: KeyEventArgs) =
        match tryMapSpecialKey args with
        | Some keyInfo ->
            if handleSpecialKey keyInfo then
                surface.InvalidateVisual()
                args.Handled <- true
        | None ->
            if not (handlePrintableKey args) then
                pulseGameplayKey ()
                surface.InvalidateVisual()

    let handleKeyUp (args: KeyEventArgs) =
        match tryMapSpecialKey args with
        | Some keyInfo ->
            releaseSpecialKey keyInfo
            surface.InvalidateVisual()
            args.Handled <- true
        | None ->
            releaseGameplayKey ()
            surface.InvalidateVisual()
            args.Handled <- true

    let handleTextInputEvent (args: TextInputEventArgs) =
        let text = args.Text |> Option.ofObj |> Option.defaultValue String.Empty
        if not (String.IsNullOrEmpty text) then
            handleTextInput text
            surface.InvalidateVisual()
            args.Handled <- true

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
        surface.Focusable <- true
        this.TextInput.Add(handleTextInputEvent)
        surface.TextInput.Add(handleTextInputEvent)
        this.KeyDown.Add(handleKeyDown)
        surface.KeyDown.Add(handleKeyDown)
        this.KeyUp.Add(handleKeyUp)
        surface.KeyUp.Add(handleKeyUp)
        repaintTimer.Interval <- TimeSpan.FromMilliseconds(16.0)
        repaintTimer.Tick.Add(fun _ -> surface.InvalidateVisual())
        repaintTimer.Start()
        this.Content <- surface
        this.Opened.Add(fun _ ->
            this.Focus() |> ignore
            surface.Focus() |> ignore)
        this.Activated.Add(fun _ ->
            this.Focus() |> ignore
            surface.Focus() |> ignore)
        this.PointerPressed.Add(fun _ ->
            this.Focus() |> ignore
            surface.Focus() |> ignore)
        surface.PointerPressed.Add(fun _ ->
            this.Focus() |> ignore
            surface.Focus() |> ignore)
        this.Closed.Add(fun _ ->
            match Application.Current with
            | null -> ()
            | app ->
                match app.ApplicationLifetime with
                | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.Shutdown()
                | _ -> ())

    member _.AppendOutput(text: string) =
        ()
