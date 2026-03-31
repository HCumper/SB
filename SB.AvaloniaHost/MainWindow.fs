namespace SBAvaloniaHost

open System
open System.IO
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Controls.Primitives
open Avalonia.Threading
open CompilerPipeline
open Interpreter
open SemanticAnalysisFacts
open Serilog
open SBRuntime

type MainWindow() as this =
    inherit Window()

    let statusText = TextBlock()
    let outputText = TextBlock()
    let outputScroll = ScrollViewer()
    let surface = RuntimeSurfaceControl()
    let repaintTimer = DispatcherTimer()
    let filePathBox = TextBox()
    let runButton = Button()
    let mutable runtime = AvaloniaRuntimeHost().CreateSession(fun line -> this.AppendOutput(line))

    let enqueuePrintableText (text: string) =
        if not (String.IsNullOrEmpty text) then
            for ch in text do
                runtime.EnqueueKey {
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

    let seedDisplay() =
        runtime.Host.Screen.SetWindow(64, 28, 12, 12)
        runtime.Host.Screen.SetCursor(1, 1)
        runtime.Host.Screen.SetInk([ 7 ])
        runtime.Host.Screen.WriteText("SB Avalonia Host")
        runtime.Host.Screen.SetCursor(1, 3)
        runtime.Host.Screen.SetInk([ 3 ])
        runtime.Host.Screen.WriteText("Runtime display snapshot")
        runtime.Host.Graphics.SetDrawingContext(runtime.Host.Screen.GetWindow(), runtime.Host.Screen.GetPan(), (100.0, 0.0, 0.0))
        runtime.Host.Graphics.SetInk([ 2 ])
        runtime.Host.Graphics.Line(2.0, 8.0, 58.0, 8.0)
        runtime.Host.Graphics.SetInk([ 6 ])
        runtime.Host.Graphics.Block(18.0, 10.0, 4.0, 12.0, 6)
        runtime.Host.Graphics.SetInk([ 4 ])
        runtime.Host.Graphics.Plot(40.0, 18.0)

    let createRuntimeSession() =
        AvaloniaRuntimeHost().CreateSession(fun line -> this.AppendOutput(line))

    let formatHirLoweringError (error: HirLoweringError) =
        let location =
            match error.Position with
            | Some pos -> $" at {pos.EditorLineNo}:{pos.Column}"
            | None -> String.Empty

        $"[{error.Scope}] {error.Message}{location}"

    let compileProgram filePath =
        let logger = LoggerConfiguration().CreateLogger()
        let settings =
            { InputFileName = filePath
              OutputFileName = String.Empty
              Verbose = false
              Backend = "interpret"
              SyntaxChecking = Relaxed
              AppName = "SB"
              Logger = logger }

        match loadAstFromInput settings with
        | Error(FileNotFound path) -> Result.Error($"File not found: {path}")
        | Error(ParseError msg) -> Result.Error($"Parse failed:{Environment.NewLine}{msg}")
        | Ok(parseTree, inputStream, ast) ->
            logDiagnostics { InputFile = filePath; OutputFile = String.Empty; Verbose = false } (parseTree, inputStream) ast
            let state = runSemanticAnalysis ast

            if not state.Errors.IsEmpty then
                let details =
                    if state.Diagnostics.IsEmpty then
                        state.Errors
                    else
                        state.Diagnostics |> List.map formatDiagnostic

                Result.Error($"Semantic analysis failed:{Environment.NewLine}{String.concat Environment.NewLine details}")
            else
                runHirLowering state
                |> Result.mapError (fun errors ->
                    $"HIR lowering failed:{Environment.NewLine}{errors |> List.map formatHirLoweringError |> String.concat Environment.NewLine}")

    let runSelectedProgram() =
        let filePath = filePathBox.Text |> Option.ofObj |> Option.defaultValue String.Empty

        if String.IsNullOrWhiteSpace filePath then
            this.SetStatus("Status: enter a source file path first.")
        elif not (File.Exists(filePath)) then
            this.SetStatus("Status: selected source file does not exist.")
            this.AppendOutput($"Missing file: {filePath}")
        else
            outputText.Text <- String.Empty
            runtime <- createRuntimeSession()
            surface.AttachDisplaySurface(runtime.Display)
            this.SetStatus($"Status: running {Path.GetFileName(filePath)}")

            Task.Run(fun () ->
                match compileProgram filePath with
                | Error message ->
                    Dispatcher.UIThread.Post(fun () ->
                        this.SetStatus("Status: compilation failed.")
                        this.AppendOutput(message))
                | Ok hirProgram ->
                    let result =
                        interpretProgramWithOptions
                            { defaultRuntimeOptions with
                                Host = runtime.Host
                                Random = Random()
                                Clock = fun () -> DateTime.UtcNow
                                Sleeper = fun milliseconds -> System.Threading.Thread.Sleep(milliseconds) }
                            hirProgram

                    Dispatcher.UIThread.Post(fun () ->
                        match result with
                        | Ok _ -> this.SetStatus($"Status: finished {Path.GetFileName(filePath)}")
                        | Error runtimeError ->
                            this.SetStatus("Status: runtime failed.")
                            this.AppendOutput(runtimeError.Message))
            )
            |> ignore

    let buildContent() =
        let root = Grid()
        root.RowDefinitions <- RowDefinitions("Auto,Auto,*,Auto")

        let header = StackPanel()
        header.Margin <- Thickness(24.0, 20.0, 24.0, 12.0)
        header.Spacing <- 4.0

        let title = TextBlock()
        title.Text <- "SB Avalonia Host"
        title.FontSize <- 28.0
        title.FontWeight <- FontWeight.SemiBold

        let subtitle = TextBlock()
        subtitle.Text <- "Prototype desktop host for the SuperBASIC runtime"
        subtitle.Foreground <- SolidColorBrush(Color.Parse("#6E7D8A"))

        header.Children.Add(title) |> ignore
        header.Children.Add(subtitle) |> ignore

        let launcher = Grid()
        launcher.Margin <- Thickness(24.0, 0.0, 24.0, 12.0)
        launcher.ColumnDefinitions <- ColumnDefinitions("*,12,Auto")

        filePathBox.Watermark <- "Path to .sb or .ssb file"
        filePathBox.Text <- Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "SB", "golfer.sb") |> Path.GetFullPath

        runButton.Content <- "Run"
        runButton.MinWidth <- 96.0
        runButton.Click.Add(fun _ -> runSelectedProgram())

        Grid.SetColumn(filePathBox, 0)
        Grid.SetColumn(runButton, 2)
        launcher.Children.Add(filePathBox) |> ignore
        launcher.Children.Add(runButton) |> ignore

        let surfaceBorder = Border()
        surfaceBorder.Margin <- Thickness(24.0, 0.0, 24.0, 12.0)
        surfaceBorder.Padding <- Thickness(12.0)
        surfaceBorder.Background <- SolidColorBrush(Color.Parse("#162028"))
        surfaceBorder.CornerRadius <- CornerRadius(12.0)
        surfaceBorder.Child <- surface

        let footer = StackPanel()
        footer.Margin <- Thickness(24.0, 0.0, 24.0, 24.0)
        footer.Spacing <- 8.0

        statusText.Text <- "Status: runtime display connected to Avalonia surface."
        statusText.FontWeight <- FontWeight.Medium
        statusText.FontSize <- 24.0

        outputText.Text <- "Next step: run interpreted programs against the Avalonia-backed session and repaint from runtime state."
        outputText.TextWrapping <- TextWrapping.Wrap
        outputText.Foreground <- SolidColorBrush(Color.Parse("#6E7D8A"))
        outputText.FontSize <- 16.0

        outputScroll.MaxHeight <- 180.0
        outputScroll.VerticalScrollBarVisibility <- ScrollBarVisibility.Auto
        outputScroll.HorizontalScrollBarVisibility <- ScrollBarVisibility.Disabled
        outputScroll.Content <- outputText

        footer.Children.Add(statusText) |> ignore
        footer.Children.Add(outputScroll) |> ignore

        Grid.SetRow(header, 0)
        Grid.SetRow(launcher, 1)
        Grid.SetRow(surfaceBorder, 2)
        Grid.SetRow(footer, 3)

        root.Children.Add(header) |> ignore
        root.Children.Add(launcher) |> ignore
        root.Children.Add(surfaceBorder) |> ignore
        root.Children.Add(footer) |> ignore
        root

    do
        this.Title <- "SB Avalonia Host"
        this.Width <- 980.0
        this.Height <- 720.0
        this.MinWidth <- 720.0
        this.MinHeight <- 540.0
        this.Background <- SolidColorBrush(Color.Parse("#F3EBDD"))
        surface.AttachDisplaySurface(runtime.Display)
        seedDisplay()
        this.TextInput.Add(fun args ->
            let text = args.Text |> Option.ofObj |> Option.defaultValue String.Empty
            enqueuePrintableText text)
        this.KeyDown.Add(fun args ->
            match tryMapSpecialKey args with
            | Some keyInfo -> runtime.EnqueueKey keyInfo
            | None -> ())
        repaintTimer.Interval <- TimeSpan.FromMilliseconds(33.0)
        repaintTimer.Tick.Add(fun _ -> surface.InvalidateVisual())
        repaintTimer.Start()
        this.Content <- buildContent()

    member _.SetStatus(text: string) =
        statusText.Text <- text

    member _.AppendOutput(text: string) =
        let current =
            outputText.Text
            |> Option.ofObj
            |> Option.defaultValue String.Empty
        outputText.Text <- current + Environment.NewLine + text
        outputScroll.ScrollToEnd()
