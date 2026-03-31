namespace SBAvaloniaHost

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls
open Avalonia.Markup.Xaml
open Avalonia.Themes.Fluent

module HostAppState =
    let mutable CreateMainWindow : (unit -> Window) option = None
    let mutable OnWindowReady : (Window -> unit) option = None

type App() =
    inherit Application()

    override _.Initialize() =
        ()

    override _.OnFrameworkInitializationCompleted() =
        base.Styles.Add(FluentTheme())

        match base.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            let window =
                match HostAppState.CreateMainWindow with
                | Some factory -> factory()
                | None -> Window(Width = 800.0, Height = 600.0, Title = "SB Runtime Host")
            desktop.MainWindow <- window
            HostAppState.OnWindowReady |> Option.iter (fun callback -> callback window)
        | _ ->
            ()

        base.OnFrameworkInitializationCompleted()
