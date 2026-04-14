namespace SBAvaloniaHost

open System
open System.Collections.Concurrent
open System.Text
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

module internal InteractiveInput =
    let private tryGetScreenChannel (host: IRuntimeHost) (channelId: ChannelId) =
        match host.Channels.Get(channelId) with
        | Result.Ok (:? IScreenChannel as screenChannel) -> Some screenChannel
        | _ -> None

    let private visualAdvance cellWidth =
        max 1 (cellWidth - max 4 ((2 * cellWidth) / 3))

    let private cellAdvance (mode: ScreenModeInfo) (characterSize: int * int) =
        let widthScale, _ = characterSize
        let cellWidth = mode.BaseTextCellWidth * max 1 (widthScale + 1)
        if mode.IsQlCompatible && characterSize = (0, 0) then
            cellWidth
        else
            visualAdvance cellWidth

    let private textColumns (host: IRuntimeHost) (screenChannel: IScreenChannel) =
        let width, _, _, _ = screenChannel.GetWindow()
        let characterSize = screenChannel.GetCharacterSize()
        max 1 (width / cellAdvance (host.Screen.GetMode()) characterSize)

    type Controller(host: IRuntimeHost, enqueueKey: KeyInfo -> unit, enqueueLine: string -> unit) as this =
        let gate = obj ()
        let buffer = StringBuilder()
        let submitted = ResizeArray<ChannelId * string>()
        let pressedRowBits = System.Collections.Generic.Dictionary<int, int>()
        let transientRowBits = System.Collections.Generic.Dictionary<int, int>()
        let mutable activeChannelId: ChannelId option = None
        let mutable origin = 0, 0
        let mutable caretIndex = 0
        let mutable renderedLength = 0
        let mutable gameplayPulseToggle = false
        let mutable activeGameplayKeyCodes: int list = []
        let textInputActivationPollMilliseconds = 10
        let textInputActivationPollAttempts = 10

        let tryGetKeyRowBit keyCode =
            match keyCode with
            | 28 -> Some(1, 0x01)   // cursor left
            | 29 -> Some(1, 0x02)   // cursor right
            | 30 -> Some(1, 0x04)   // cursor up
            | 31 -> Some(1, 0x08)   // cursor down
            | 27 -> Some(1, 0x10)   // escape
            | 13 -> Some(1, 0x20)   // enter
            | 32 -> Some(1, 0x40)   // space / fire
            | _ -> None

        let updateKeyRowState keyCode isPressed =
            match tryGetKeyRowBit keyCode with
            | Some(row, bit) ->
                let existing =
                    match pressedRowBits.TryGetValue row with
                    | true, value -> value
                    | false, _ -> 0
                let nextValue =
                    if isPressed then
                        existing ||| bit
                    else
                        existing &&& (~~~bit)
                if nextValue = 0 then
                    pressedRowBits.Remove(row) |> ignore
                else
                    pressedRowBits[row] <- nextValue

                let isTransient = keyCode = 13
                if isTransient then
                    let transientExisting =
                        match transientRowBits.TryGetValue row with
                        | true, value -> value
                        | false, _ -> 0
                    let transientNext =
                        if isPressed then
                            transientExisting ||| bit
                        else
                            transientExisting &&& (~~~bit)
                    if transientNext = 0 then
                        transientRowBits.Remove(row) |> ignore
                    else
                        transientRowBits[row] <- transientNext
            | None -> ()

        let beginGameplayKeyHold () =
            match activeGameplayKeyCodes with
            | _ :: _ -> ()
            | [] ->
                // Many games poll KEYROW and expect alternating row values
                // rather than the same bit every frame.
                let directionKeyCode =
                    if gameplayPulseToggle then 28 else 29
                gameplayPulseToggle <- not gameplayPulseToggle
                activeGameplayKeyCodes <- [ directionKeyCode ]
                updateKeyRowState directionKeyCode true

        let endGameplayKeyHold () =
            match activeGameplayKeyCodes with
            | [] -> ()
            | keyCodes ->
                for keyCode in keyCodes do
                    updateKeyRowState keyCode false
                activeGameplayKeyCodes <- []

        let setCursorFromIndex (screenChannel: IScreenChannel) index =
            let originX, originY = origin
            let cols = textColumns host screenChannel
            let absolute = originX + max 0 index
            let cursorX = absolute % cols
            let cursorY = originY + (absolute / cols)
            screenChannel.SetCursor(cursorX, cursorY)

        let redrawBuffer (screenChannel: IScreenChannel) =
            screenChannel.SetCursor(fst origin, snd origin)
            if renderedLength > 0 then
                screenChannel.WriteText(String(' ', renderedLength))
            screenChannel.SetCursor(fst origin, snd origin)
            let text = buffer.ToString()
            if text.Length > 0 then
                screenChannel.WriteText(text)
            renderedLength <- text.Length
            setCursorFromIndex screenChannel caretIndex

        let finishInput () =
            activeChannelId
            |> Option.bind (tryGetScreenChannel host)
            |> Option.iter (fun screenChannel -> screenChannel.SetCursorVisible(false))
            activeChannelId <- None
            buffer.Clear() |> ignore
            caretIndex <- 0
            renderedLength <- 0

        let ensureInputStarted channelId =
            if activeChannelId <> Some channelId then
                activeChannelId <- Some channelId
                let screenChannel =
                    tryGetScreenChannel host channelId
                    |> Option.defaultWith (fun () -> failwith $"Screen channel {channelId} is not available.")
                origin <- screenChannel.GetCursor()
                screenChannel.SetCursorVisible(true)
                buffer.Clear() |> ignore
                caretIndex <- 0
                renderedLength <- 0
                Monitor.PulseAll(gate)
                screenChannel
            else
                tryGetScreenChannel host channelId
                |> Option.defaultWith (fun () -> failwith $"Screen channel {channelId} is not available.")

        let queueKeyForRuntime (keyInfo: KeyInfo) =
            enqueueKey keyInfo

        member _.PulseGameplayKey() =
            lock gate (fun () -> beginGameplayKeyHold ())

        member _.ReleaseGameplayKey() =
            lock gate (fun () -> endGameplayKeyHold ())

        member _.TryReadDefaultLine() =
            this.TryReadLine(ChannelId 1)

        member _.TryReadLine(channelId: ChannelId) =
            lock gate (fun () ->
                ensureInputStarted channelId |> ignore

                let rec waitForSubmitted () =
                    match submitted |> Seq.tryFindIndex (fun (submittedChannelId, _) -> submittedChannelId = channelId) with
                    | Some index ->
                        let _, line = submitted[index]
                        submitted.RemoveAt(index)
                        Some line
                    | None ->
                        Monitor.Wait(gate) |> ignore
                        waitForSubmitted ()

                waitForSubmitted ())

        member _.HandleTextInput(text: string) =
            if not (String.IsNullOrEmpty text) then
                lock gate (fun () ->
                    let rec tryHandleTextInput attemptsRemaining =
                        match activeChannelId |> Option.bind (tryGetScreenChannel host) with
                        | Some screenChannel ->
                            buffer.Insert(caretIndex, text) |> ignore
                            caretIndex <- caretIndex + text.Length
                            redrawBuffer screenChannel
                        | None when attemptsRemaining > 0 ->
                            Monitor.Wait(gate, textInputActivationPollMilliseconds) |> ignore
                            tryHandleTextInput (attemptsRemaining - 1)
                        | None ->
                            beginGameplayKeyHold ()
                            for ch in text do
                                queueKeyForRuntime {
                                    KeyCode = int ch
                                    Character = Some ch
                                    Shift = false
                                    Control = false
                                }

                    tryHandleTextInput textInputActivationPollAttempts)

        member _.HandleSpecialKey(keyInfo: KeyInfo) =
            lock gate (fun () ->
                updateKeyRowState keyInfo.KeyCode true
                match activeChannelId |> Option.bind (tryGetScreenChannel host) with
                | Some screenChannel ->
                    match keyInfo.KeyCode with
                    | 13 ->
                        let channelId = activeChannelId |> Option.defaultValue (ChannelId 1)
                        let line = buffer.ToString()
                        submitted.Add(channelId, line)
                        Monitor.PulseAll(gate)
                        enqueueLine line
                        finishInput ()
                        screenChannel.NewLine()
                        true
                    | 8 ->
                        if caretIndex > 0 then
                            buffer.Remove(caretIndex - 1, 1) |> ignore
                            caretIndex <- caretIndex - 1
                            redrawBuffer screenChannel
                        true
                    | 127 ->
                        if caretIndex < buffer.Length then
                            buffer.Remove(caretIndex, 1) |> ignore
                            redrawBuffer screenChannel
                        true
                    | 28 ->
                        if caretIndex > 0 then
                            caretIndex <- caretIndex - 1
                            setCursorFromIndex screenChannel caretIndex
                        true
                    | 29 ->
                        if caretIndex < buffer.Length then
                            caretIndex <- caretIndex + 1
                            setCursorFromIndex screenChannel caretIndex
                        true
                    | 1 ->
                        caretIndex <- 0
                        setCursorFromIndex screenChannel caretIndex
                        true
                    | 5 ->
                        caretIndex <- buffer.Length
                        setCursorFromIndex screenChannel caretIndex
                        true
                    | 27 ->
                        buffer.Clear() |> ignore
                        caretIndex <- 0
                        redrawBuffer screenChannel
                        true
                    | _ ->
                        queueKeyForRuntime keyInfo
                        true
                | None ->
                    queueKeyForRuntime keyInfo
                    true)

        member _.ReleaseSpecialKey(keyInfo: KeyInfo) =
            lock gate (fun () ->
                updateKeyRowState keyInfo.KeyCode false)

        member _.GetKeyRow(row: int) =
            lock gate (fun () ->
                let value =
                    match pressedRowBits.TryGetValue row with
                    | true, current -> current
                    | false, _ -> 0

                match transientRowBits.TryGetValue row with
                | true, transient when transient <> 0 ->
                    let remaining = value &&& (~~~transient)
                    if remaining = 0 then
                        pressedRowBits.Remove(row) |> ignore
                    else
                        pressedRowBits[row] <- remaining
                    transientRowBits.Remove(row) |> ignore
                | _ -> ()

                value)

        member _.FlushInput() =
            lock gate (fun () ->
                submitted.Clear()
                pressedRowBits.Clear()
                transientRowBits.Clear()
                activeGameplayKeyCodes <- []
                Monitor.PulseAll(gate)
                match activeChannelId |> Option.bind (tryGetScreenChannel host) with
                | Some screenChannel ->
                    buffer.Clear() |> ignore
                    caretIndex <- 0
                    redrawBuffer screenChannel
                    finishInput ()
                | None ->
                    finishInput ())

type AvaloniaRuntimeHost() =
    member _.CreateSession(writeLine: string -> unit) =
        let keyQueue = ConcurrentQueue<KeyInfo>()
        let lineQueue = ConcurrentQueue<string>()
        let mutable controllerOpt : InteractiveInput.Controller option = None

        let readKey () =
            match keyQueue.TryDequeue() with
            | true, keyInfo -> Some keyInfo
            | _ -> None

        let readLine () =
            match controllerOpt with
            | Some controller -> controller.TryReadDefaultLine()
            | None -> None

        let host, display =
            DefaultHost.createWithDisplay {
                ReadLine = readLine
                ReadScreenLine =
                    fun channelId ->
                        match controllerOpt with
                        | Some controller -> controller.TryReadLine(channelId)
                        | None -> None
                FlushInput =
                    fun () ->
                        match controllerOpt with
                        | Some controller -> controller.FlushInput()
                        | None -> ()
                ReadKey = readKey
                KeyAvailable = fun () -> not keyQueue.IsEmpty
                KeyRowState =
                    fun row ->
                        match controllerOpt with
                        | Some controller -> controller.GetKeyRow(row)
                        | None -> 0
                WriteLine =
                    fun line ->
                        if Dispatcher.UIThread.CheckAccess() then
                            writeLine line
                        else
                            Dispatcher.UIThread.Post(fun () -> writeLine line)
            }

        let controller = InteractiveInput.Controller(host, keyQueue.Enqueue, lineQueue.Enqueue)
        controllerOpt <- Some controller

        AvaloniaRuntimeSession(
            host,
            display,
            controller.HandleSpecialKey >> ignore,
            lineQueue.Enqueue)

    member internal _.CreateInteractiveSession(writeLine: string -> unit) =
        let keyQueue = ConcurrentQueue<KeyInfo>()
        let lineQueue = ConcurrentQueue<string>()
        let mutable controllerOpt : InteractiveInput.Controller option = None

        let readKey () =
            match keyQueue.TryDequeue() with
            | true, keyInfo -> Some keyInfo
            | _ -> None

        let readLine () =
            match controllerOpt with
            | Some controller -> controller.TryReadDefaultLine()
            | None -> None

        let host, display =
            DefaultHost.createWithDisplay {
                ReadLine = readLine
                ReadScreenLine =
                    fun channelId ->
                        match controllerOpt with
                        | Some controller -> controller.TryReadLine(channelId)
                        | None -> None
                FlushInput =
                    fun () ->
                        match controllerOpt with
                        | Some controller -> controller.FlushInput()
                        | None -> ()
                ReadKey = readKey
                KeyAvailable = fun () -> not keyQueue.IsEmpty
                KeyRowState =
                    fun row ->
                        match controllerOpt with
                        | Some controller -> controller.GetKeyRow(row)
                        | None -> 0
                WriteLine =
                    fun line ->
                        if Dispatcher.UIThread.CheckAccess() then
                            writeLine line
                        else
                            Dispatcher.UIThread.Post(fun () -> writeLine line)
            }

        let controller = InteractiveInput.Controller(host, keyQueue.Enqueue, lineQueue.Enqueue)
        controllerOpt <- Some controller

        host, display, controller

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
                    let host, display, controller =
                        (new AvaloniaRuntimeHost()).CreateInteractiveSession(fun line ->
                            match windowOpt with
                            | Some window -> window.AppendOutput(line)
                            | None -> ())

                    let session =
                        AvaloniaRuntimeSession(host, display, controller.HandleSpecialKey >> ignore, ignore)

                    sessionOpt <- Some session
                    HostAppState.CreateMainWindow <-
                        Some(fun () ->
                            let window = MainWindow(session.Display, controller.HandleTextInput, controller.HandleSpecialKey, controller.PulseGameplayKey, controller.ReleaseGameplayKey, controller.ReleaseSpecialKey)
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
