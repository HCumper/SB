namespace SBAvaloniaHost

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Markup.Xaml
open Avalonia.Themes.Fluent

type App() =
    inherit Application()

    override _.Initialize() =
        ()

    override _.OnFrameworkInitializationCompleted() =
        base.Styles.Add(FluentTheme())

        match base.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            desktop.MainWindow <- MainWindow()
        | _ ->
            ()

        base.OnFrameworkInitializationCompleted()
