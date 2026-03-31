namespace SBAvaloniaHost

open System
open System.Collections.Concurrent
open System.Threading
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Threading
open SBRuntime

type AvaloniaRuntimeSession(host: IRuntimeHost, display: IDisplaySurface, enqueueKey: KeyInfo -> unit, enqueueLine: string -> unit) =
    member _.Host = host
    member _.Display = display
    member _.EnqueueKey(keyInfo: KeyInfo) = enqueueKey keyInfo
    member _.EnqueueLine(line: string) = enqueueLine line

type AvaloniaRuntimeHost() =
    member _.CreateSession(writeLine: string -> unit) =
        let keyQueue = ConcurrentQueue<KeyInfo>()
        let lineQueue = ConcurrentQueue<string>()

        let readKey () =
            match keyQueue.TryDequeue() with
            | true, keyInfo -> Some keyInfo
            | _ -> None

        let readLine () =
            match lineQueue.TryDequeue() with
            | true, line -> Some line
            | _ -> None

        let host, display =
            DefaultHost.createWithDisplay {
                ReadLine = readLine
                ReadKey = readKey
                KeyAvailable = fun () -> not keyQueue.IsEmpty
                KeyRowState = fun _ -> 0
                WriteLine =
                    fun line ->
                        if Dispatcher.UIThread.CheckAccess() then
                            writeLine line
                        else
                            Dispatcher.UIThread.Post(fun () -> writeLine line)
            }

        AvaloniaRuntimeSession(host, display, keyQueue.Enqueue, lineQueue.Enqueue)

type AvaloniaHostHandle internal (session: AvaloniaRuntimeSession, shutdown: unit -> unit, waitForExit: unit -> unit) =
    member _.Session = session
    member _.Host = session.Host
    member _.Display = session.Display
    member _.WaitForExit() = waitForExit()
    interface IDisposable with
        member _.Dispose() = shutdown()

type AvaloniaHostService() =
    member _.Start() =
        let ready = new ManualResetEventSlim(false)
        let closed = new ManualResetEventSlim(false)
        let mutable sessionOpt : AvaloniaRuntimeSession option = None
        let mutable threadOpt : Thread option = None
        let mutable windowOpt : MainWindow option = None

        let shutdown () =
            Dispatcher.UIThread.Post(fun () ->
                match Application.Current with
                | null -> ()
                | app ->
                    match app.ApplicationLifetime with
                    | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.Shutdown()
                    | _ -> ()
                )

            match threadOpt with
            | Some thread when thread.IsAlive -> thread.Join(5000) |> ignore
            | _ -> ()

        let thread =
            Thread(ThreadStart(fun () ->
                try
                    let session =
                        (new AvaloniaRuntimeHost()).CreateSession(fun line ->
                            match windowOpt with
                            | Some window -> window.AppendOutput(line)
                            | None -> ())

                    sessionOpt <- Some session
                    HostAppState.CreateMainWindow <-
                        Some(fun () ->
                            let window = MainWindow(session.Host, session.Display, session.EnqueueKey, session.EnqueueLine)
                            windowOpt <- Some window
                            window :> Window)
                    HostAppState.OnWindowReady <- Some(fun _ -> ready.Set())

                    AppBuilder
                        .Configure<App>()
                        .UsePlatformDetect()
                        .StartWithClassicDesktopLifetime([||], ShutdownMode.OnExplicitShutdown)
                        |> ignore
                finally
                    HostAppState.CreateMainWindow <- None
                    HostAppState.OnWindowReady <- None
                    closed.Set()))

        thread.SetApartmentState(ApartmentState.STA)
        thread.IsBackground <- true
        threadOpt <- Some thread
        thread.Start()
        ready.Wait()

        let session =
            match sessionOpt with
            | Some value -> value
            | None -> failwith "Avalonia runtime session failed to start."

        new AvaloniaHostHandle(session, shutdown, fun () ->
            closed.Wait()
            match threadOpt with
            | Some thread when thread.IsAlive -> thread.Join(5000) |> ignore
            | _ -> ())
