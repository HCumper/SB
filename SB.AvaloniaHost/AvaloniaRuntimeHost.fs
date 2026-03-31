namespace SBAvaloniaHost

open System.Collections.Concurrent
open Avalonia.Threading
open SBRuntime

type AvaloniaRuntimeSession(host: IRuntimeHost, display: IDisplaySurface, enqueue: KeyInfo -> unit) =
    member _.Host = host
    member _.Display = display
    member _.EnqueueKey(keyInfo: KeyInfo) = enqueue keyInfo

type AvaloniaRuntimeHost() =
    member _.CreateSession(writeLine: string -> unit) =
        let keyQueue = ConcurrentQueue<KeyInfo>()
        let readKey () =
            match keyQueue.TryDequeue() with
            | true, keyInfo -> Some keyInfo
            | _ -> None

        let host, display =
            DefaultHost.createWithDisplay {
                ReadLine = fun () -> None
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

        AvaloniaRuntimeSession(host, display, keyQueue.Enqueue)
