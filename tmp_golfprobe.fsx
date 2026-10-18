#r @"C:\Source\SB\SBRuntime\bin\Debug\net10.0\SBRuntime.dll"
#r @"C:\Source\SB\SB\bin\Debug\net10.0\SB.dll"
#r @"C:\Source\SB\SBLib\bin\Debug\net10.0\SBLib.dll"
#r @"C:\Source\SB\SBTests\bin\Debug\net10.0\SBTests.dll"
#r @"C:\Source\SB\SBRuntimeTests\bin\Debug\net10.0\SBRuntimeTests.dll"
open System
open System.Collections.Generic
open SBRuntimeTests.TestSupport
open Interpreter
open SBRuntime
let ast = parseAstFromFile "Golfer.sb"
let hir = lowerProgram ast
let lines = Queue<string>()
["n"; "1"; "12"] |> List.iter lines.Enqueue
let keys = Queue<KeyInfo>()
[{ KeyCode = int ' '; Character = Some ' '; Shift = false; Control = false }
 { KeyCode = int ' '; Character = Some ' '; Shift = false; Control = false }
 { KeyCode = int ' '; Character = Some ' '; Shift = false; Control = false }]
|> List.iter keys.Enqueue
let host =
    DefaultHost.create {
        ReadLine = fun () -> if lines.Count > 0 then Some(lines.Dequeue()) else None
        ReadScreenLine = fun _ -> if lines.Count > 0 then Some(lines.Dequeue()) else None
        FlushInput = fun () -> ()
        ReadKey = fun () -> if keys.Count > 0 then Some(keys.Dequeue()) else None
        KeyAvailable = fun () -> keys.Count > 0
        KeyRowState = fun _ -> 0
        WriteLine = fun line -> printfn "OUT: %s" line
    }
let options =
    { defaultRuntimeOptions with
        Host = host
        InitialSourceProgram = Some ast
        ExecutionThrottle = Some { TargetStatementsPerSecond = 5000; MaxRunAheadMilliseconds = 1 } }
match interpretProgramWithOptions options hir with
| Result.Ok _ -> printfn "OK"
| Result.Error err -> printfn "ERR %A" err
