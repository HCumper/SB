module SBRuntimeTests.InterpreterFixtureTests

open System
open System.Collections.Generic
open NUnit.Framework

open Interpreter
open SBRuntime
open SBRuntimeTests.TestSupport

[<Test>]
let ``q3 fixture runtime completes sort check without inversion output`` () =
    let ast = parseAstFromFile "q3.SB"
    let hir = lowerProgram ast
    let outputs = ResizeArray<string>()
    let inputs = Queue<string>([ "25"; "" ])
    let options =
        { defaultRuntimeOptions with
            Host =
                DefaultHost.create {
                    ReadLine = fun () -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                    ReadScreenLine = fun _ -> if inputs.Count > 0 then Some(inputs.Dequeue()) else None
                    FlushInput = fun () -> ()
                    ReadKey = fun () -> None
                    KeyAvailable = fun () -> false
                    KeyRowState = fun _ -> 0
                    WriteLine = fun line -> outputs.Add(line)
                }
            Random = Random(1234)
            Clock = fun () -> DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc) }

    match interpretProgramWithOptions options hir with
    | Result.Ok result ->
        Assert.That(result.Output |> List.exists (fun line -> line.StartsWith("Error at")), Is.False)
    | Result.Error err ->
        Assert.Fail($"Expected q3 fixture interpretation to succeed, got %A{err}")
