module SBRuntimeTests.HostInputContract

open System
open System.Collections.Generic
open System.Reflection
open System.Threading
open NUnit.Framework

open SBAvaloniaHost
open SBRuntime
open Interpreter
open SBRuntimeTests.TestSupport

type AvaloniaHarness =
    { Host: IRuntimeHost
      Display: IDisplaySurface
      Controller: obj
      HandleTextInput: MethodInfo
      HandleSpecialKey: MethodInfo
      ReleaseSpecialKey: MethodInfo
      PulseGameplayKey: MethodInfo
      ReleaseGameplayKey: MethodInfo
      GetKeyRow: MethodInfo
      TryReadLine: MethodInfo
      TryReadDefaultLine: MethodInfo
      FlushInput: MethodInfo }

type ScriptedDisplayHarness =
    { Host: IRuntimeHost
      Display: IDisplaySurface
      EnqueueScreenLine: int -> string -> unit
      FlushPendingInput: unit -> unit }

let createAvaloniaHarness () =
    let runtimeHost = AvaloniaRuntimeHost()
    let flags = BindingFlags.Instance ||| BindingFlags.NonPublic
    let controllerFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic
    let createSession = typeof<AvaloniaRuntimeHost>.GetMethod("CreateInteractiveSession", flags)
    let sessionTuple = createSession.Invoke(runtimeHost, [| box ignore<string> |])
    let tupleType = sessionTuple.GetType()
    let controller = tupleType.GetProperty("Item3").GetValue(sessionTuple)
    let controllerType = controller.GetType()

    { Host = tupleType.GetProperty("Item1").GetValue(sessionTuple) :?> IRuntimeHost
      Display = tupleType.GetProperty("Item2").GetValue(sessionTuple) :?> IDisplaySurface
      Controller = controller
      HandleTextInput = controllerType.GetMethod("HandleTextInput", controllerFlags)
      HandleSpecialKey = controllerType.GetMethod("HandleSpecialKey", controllerFlags)
      ReleaseSpecialKey = controllerType.GetMethod("ReleaseSpecialKey", controllerFlags)
      PulseGameplayKey = controllerType.GetMethod("PulseGameplayKey", controllerFlags)
      ReleaseGameplayKey = controllerType.GetMethod("ReleaseGameplayKey", controllerFlags)
      GetKeyRow = controllerType.GetMethod("GetKeyRow", controllerFlags)
      TryReadLine = controllerType.GetMethod("TryReadLine", controllerFlags)
      TryReadDefaultLine = controllerType.GetMethod("TryReadDefaultLine", controllerFlags)
      FlushInput = controllerType.GetMethod("FlushInput", controllerFlags) }

let createScriptedDisplayHarness () =
    let gate = obj ()
    let screenInputs = Dictionary<int, Queue<string>>()

    let getQueue channelId =
        match screenInputs.TryGetValue channelId with
        | true, queue -> queue
        | false, _ ->
            let queue = Queue<string>()
            screenInputs[channelId] <- queue
            queue

    let host, display =
        DefaultHost.createWithDisplay {
            ReadLine =
                fun () ->
                    lock gate (fun () ->
                        let queue = getQueue 1
                        if queue.Count > 0 then Some(queue.Dequeue()) else None)
            ReadScreenLine =
                fun (ChannelId channelId) ->
                    lock gate (fun () ->
                        let queue = getQueue channelId
                        if queue.Count > 0 then Some(queue.Dequeue()) else None)
            FlushInput =
                fun () ->
                    lock gate (fun () ->
                        for pair in screenInputs.Values do
                            pair.Clear())
            ReadKey = fun () -> None
            KeyAvailable = fun () -> false
            KeyRowState = fun _ -> 0
            WriteLine = ignore
        }

    { Host = host
      Display = display
      EnqueueScreenLine =
        fun channelId text ->
            lock gate (fun () ->
                let queue = getQueue channelId
                queue.Enqueue(text))
      FlushPendingInput =
        fun () ->
            lock gate (fun () ->
                for pair in screenInputs.Values do
                    pair.Clear()) }

let sendText harness (text: string) =
    harness.HandleTextInput.Invoke(harness.Controller, [| box text |]) |> ignore

let keyInfo keyCode =
    { KeyCode = keyCode
      Character = if keyCode >= 32 && keyCode <= 126 then Some(char keyCode) else None
      Shift = false
      Control = false }

let tapSpecialKey harness keyCode =
    harness.HandleSpecialKey.Invoke(harness.Controller, [| box (keyInfo keyCode) |]) |> ignore
    harness.ReleaseSpecialKey.Invoke(harness.Controller, [| box (keyInfo keyCode) |]) |> ignore

let rowText (pane: ScreenPaneSnapshot) row =
    [| for col in 0 .. Array2D.length2 pane.Text - 1 -> pane.Text[row, col].Character |] |> String

let paneText (pane: ScreenPaneSnapshot) =
    [ for row in 0 .. Array2D.length1 pane.Text - 1 -> rowText pane row ]
    |> String.concat "\n"

let waitUntil timeoutMs predicate =
    let deadline = DateTime.UtcNow.AddMilliseconds(float timeoutMs)
    let rec loop () =
        if predicate() then
            true
        elif DateTime.UtcNow >= deadline then
            false
        else
            Thread.Sleep(20)
            loop ()
    loop ()

let tryFindPaneInDisplay (display: IDisplaySurface) channelId =
    display.GetSnapshot().Panes
    |> List.tryFind (fun pane -> pane.ChannelId = Some channelId)

let waitForPaneTextInDisplay (display: IDisplaySurface) channelId expectedText timeoutMs =
    waitUntil timeoutMs (fun () ->
        tryFindPaneInDisplay display channelId
        |> Option.exists (fun pane ->
            let text = paneText pane
            text.Contains(expectedText: string)))

let tryFindPane (harness: AvaloniaHarness) channelId =
    tryFindPaneInDisplay harness.Display channelId

let waitForPaneText (harness: AvaloniaHarness) channelId expectedText timeoutMs =
    waitForPaneTextInDisplay harness.Display channelId expectedText timeoutMs

let assertPaneContains (harness: AvaloniaHarness) channelId expectedText message =
    let pane =
        tryFindPane harness channelId
        |> Option.defaultWith (fun () -> Assert.Fail($"Channel #{channelId} pane was not present."); Unchecked.defaultof<_>)

    let assertionMessage: string = message
    Assert.That(paneText pane, Does.Contain(expectedText), assertionMessage)
    pane

let waitForCursorVisible (harness: AvaloniaHarness) channelId timeoutMs =
    waitUntil timeoutMs (fun () ->
        tryFindPane harness channelId
        |> Option.exists _.CursorVisible)

let runProgramAsync host ast =
    let hir = lowerProgram ast
    let mutable result: Result<ExecutionResult, RuntimeError> option = None
    let worker = Thread(ThreadStart(fun () -> result <- Some(interpretProgramWithOptions { defaultRuntimeOptions with Host = host } hir)))
    worker.IsBackground <- true
    worker.Start()
    worker, (fun () -> result)

let assertCompleted (worker: Thread) getResult (expectedOutput: string) (failureContext: string) =
    Assert.That(worker.Join(3000), Is.True, $"Interpreter did not complete {failureContext}.")

    match getResult() with
    | Some(Result.Ok execution) -> Assert.That(String.concat "|" execution.Output, Is.EqualTo(expectedOutput))
    | Some(Result.Error err) -> Assert.Fail($"Expected interpretation to succeed {failureContext}, got %A{err}")
    | None -> Assert.Fail($"Interpreter result was not captured {failureContext}.")

let runEarlyPromptCapture (channelId: int) (promptText: string) (submittedText: string) =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource $"10 INPUT#{channelId},\"{promptText}\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    let initialText = submittedText.Substring(0, 1)
    let remainingText = submittedText.Substring(1)
    sendText harness initialText

    Assert.That(waitForPaneText harness channelId promptText 3000, Is.True, $"Timed out waiting for channel #{channelId} prompt.")
    Assert.That(waitForPaneText harness channelId (promptText + initialText) 3000, Is.True, $"Channel #{channelId} did not retain early text input once activation completed.")

    if remainingText.Length > 0 then
        sendText harness remainingText

    let paneAfterTyping =
        assertPaneContains harness channelId (promptText + submittedText) $"Channel #{channelId} did not render the full typed text before submit."
    Assert.That(paneAfterTyping.CursorVisible, Is.True, $"Channel #{channelId} cursor should be visible during active input.")

    tapSpecialKey harness 13
    assertCompleted worker getResult $"{promptText}|{submittedText}" $"after channel #{channelId} prompt submission"

let runSequentialPromptScenario (channelId: int) =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource $"10 INPUT#{channelId},\"First\",a$\n20 INPUT#{channelId},\"Second\",b$\n30 PRINT a$&\":\"&b$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness channelId "First" 3000, Is.True, $"Timed out waiting for the first channel #{channelId} prompt.")

    sendText harness "ONE"
    let firstPane = assertPaneContains harness channelId "FirstONE" $"First channel #{channelId} prompt did not render typed text before submit."
    Assert.That(firstPane.CursorVisible, Is.True, $"Cursor should be visible during the first channel #{channelId} prompt.")
    tapSpecialKey harness 13

    Assert.That(waitForPaneText harness channelId "Second" 3000, Is.True, $"Timed out waiting for the second channel #{channelId} prompt.")
    let secondPromptPane = tryFindPane harness channelId |> Option.get
    Assert.That(secondPromptPane.CursorVisible, Is.True, $"Cursor should be visible during the second channel #{channelId} prompt.")
    Assert.That(paneText secondPromptPane, Does.Not.Contain("ONESECONDONE"), $"Second channel #{channelId} prompt reused stale buffer text from the first prompt.")

    sendText harness "TWO"
    let secondPane = assertPaneContains harness channelId "SecondTWO" $"Second channel #{channelId} prompt did not render typed text before submit."
    Assert.That(secondPane.Cursor, Is.EqualTo((9, 1)), $"Cursor did not advance correctly during the second channel #{channelId} prompt.")
    tapSpecialKey harness 13

    assertCompleted worker getResult "First|Second|ONE:TWO" $"after sequential channel #{channelId} prompt submissions"

let runRepeatedPromptIterations (channelId: int) (iterations: int) =
    for iteration in 1 .. iterations do
        let harness = createAvaloniaHarness ()
        let ast = parseAstFromSource $"10 INPUT#{channelId},\"Prompt\",a$\n20 PRINT a$\n"
        let worker, getResult = runProgramAsync harness.Host ast

        sendText harness "A"

        Assert.That(waitForPaneText harness channelId "Prompt" 3000, Is.True, $"Timed out waiting for channel #{channelId} prompt on iteration {iteration}.")
        Assert.That(waitForPaneText harness channelId "PromptA" 3000, Is.True, $"Early prompt text was not visible for channel #{channelId} on iteration {iteration}.")

        sendText harness "BC"
        Assert.That(waitForPaneText harness channelId "PromptABC" 3000, Is.True, $"Prompt pane did not render full text for channel #{channelId} on iteration {iteration}.")

        tapSpecialKey harness 13
        assertCompleted worker getResult "Prompt|ABC" $"for channel #{channelId} on iteration {iteration}"

let runQueuedSequentialPromptScenario (channelId: int) =
    let harness = createScriptedDisplayHarness ()
    let ast = parseAstFromSource $"10 INPUT#{channelId},\"First\",a$\n20 INPUT#{channelId},\"Second\",b$\n30 PRINT a$&\":\"&b$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneTextInDisplay harness.Display channelId "First" 3000, Is.True, $"Timed out waiting for the first scripted channel #{channelId} prompt.")
    harness.EnqueueScreenLine channelId "ONE"

    Assert.That(waitForPaneTextInDisplay harness.Display channelId "Second" 3000, Is.True, $"Timed out waiting for the second scripted channel #{channelId} prompt.")
    harness.EnqueueScreenLine channelId "TWO"

    assertCompleted worker getResult "First|Second|ONE:TWO" $"after scripted sequential channel #{channelId} prompt submissions"

let runQueuedPromptSubmissionScenario (channelId: int) (promptText: string) (submittedText: string) =
    let harness = createScriptedDisplayHarness ()
    let ast = parseAstFromSource $"10 INPUT#{channelId},\"{promptText}\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneTextInDisplay harness.Display channelId promptText 3000, Is.True, $"Timed out waiting for scripted channel #{channelId} prompt.")

    harness.EnqueueScreenLine channelId submittedText

    assertCompleted worker getResult $"{promptText}|{submittedText}" $"after scripted prompt submission on channel #{channelId}"

let runQueuedImplicitPromptSubmissionScenario (promptText: string) (submittedText: string) =
    let harness = createScriptedDisplayHarness ()
    let ast = parseAstFromSource $"10 INPUT \"{promptText}\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneTextInDisplay harness.Display 1 promptText 3000, Is.True, "Timed out waiting for scripted implicit prompt on channel #1.")

    harness.EnqueueScreenLine 1 submittedText

    assertCompleted worker getResult $"{promptText}|{submittedText}" "after scripted implicit prompt submission"

let runQueuedPromptSubmissionPrequeuedScenario (channelId: int) (promptText: string) (submittedText: string) =
    let harness = createScriptedDisplayHarness ()
    harness.EnqueueScreenLine channelId submittedText

    let ast = parseAstFromSource $"10 INPUT#{channelId},\"{promptText}\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneTextInDisplay harness.Display channelId promptText 3000, Is.True, $"Timed out waiting for scripted prequeued channel #{channelId} prompt.")

    assertCompleted worker getResult $"{promptText}|{submittedText}" $"after scripted prequeued prompt submission on channel #{channelId}"

let runQueuedFlushScenario (channelId: int) =
    let harness = createScriptedDisplayHarness ()
    let ast = parseAstFromSource $"10 INPUT#{channelId},\"Prompt\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneTextInDisplay harness.Display channelId "Prompt" 3000, Is.True, $"Timed out waiting for scripted channel #{channelId} prompt.")

    harness.EnqueueScreenLine channelId "STALE"
    harness.Host.Input.Flush()

    Assert.That(worker.Join(500), Is.False, $"Interpreter should still be waiting after flushing scripted channel #{channelId} input.")

    harness.EnqueueScreenLine channelId "FRESH"
    assertCompleted worker getResult "Prompt|FRESH" $"after scripted flush/retry on channel #{channelId}"

let runQueuedBuiltInFlushScenario () =
    let harness = createScriptedDisplayHarness ()
    let ast = parseAstFromSource "10 FLUSH\n20 INPUT \"WHICH LEVEL?\",level\n30 PRINT level\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneTextInDisplay harness.Display 1 "WHICH LEVEL?" 3000, Is.True, "Timed out waiting for scripted prompt after FLUSH.")

    harness.EnqueueScreenLine 1 ""
    harness.Host.Input.Flush()
    harness.EnqueueScreenLine 1 "2"

    assertCompleted worker getResult "WHICH LEVEL?|2" "after scripted FLUSH followed by prompt input"

let runQueuedExplicitScreenChannelScenario () =
    let harness = createScriptedDisplayHarness ()
    let ast = parseAstFromSource "10 INPUT#1,\"LEVEL?\",level\n20 PRINT level\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneTextInDisplay harness.Display 1 "LEVEL?" 3000, Is.True, "Timed out waiting for scripted explicit screen-channel prompt.")

    harness.EnqueueScreenLine 1 "7"

    assertCompleted worker getResult "LEVEL?|7" "after scripted explicit screen-channel input"

let runQueuedPositionedPromptScenario () =
    let harness = createScriptedDisplayHarness ()
    let ast = parseAstFromSource "10 AT#0,3,5:INPUT#0,\"Angle\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneTextInDisplay harness.Display 0 "Angle" 3000, Is.True, "Timed out waiting for scripted positioned channel #0 prompt.")

    let pane = tryFindPaneInDisplay harness.Display 0 |> Option.get
    Assert.That(pane.Cursor, Is.EqualTo((10, 3)), "Scripted default host prompt did not end at the expected positioned cursor.")

    harness.EnqueueScreenLine 0 "XY"
    assertCompleted worker getResult "Angle|XY" "after scripted positioned prompt submission"

let runQueuedRepeatedPromptIterations (channelId: int) (iterations: int) =
    for iteration in 1 .. iterations do
        let harness = createScriptedDisplayHarness ()
        let ast = parseAstFromSource $"10 INPUT#{channelId},\"Prompt\",a$\n20 PRINT a$\n"
        let worker, getResult = runProgramAsync harness.Host ast

        Assert.That(waitForPaneTextInDisplay harness.Display channelId "Prompt" 3000, Is.True, $"Timed out waiting for scripted channel #{channelId} prompt on iteration {iteration}.")

        harness.EnqueueScreenLine channelId "ABC"
        assertCompleted worker getResult "Prompt|ABC" $"after scripted repeated prompt submission on channel #{channelId} iteration {iteration}"

let beginReadLine harness channelId =
    let mutable result: string option option = None
    let worker =
        Thread(ThreadStart(fun () ->
            let lineOpt = harness.TryReadLine.Invoke(harness.Controller, [| box (ChannelId channelId) |]) :?> string option
            result <- Some lineOpt))
    worker.IsBackground <- true
    worker.Start()
    worker, (fun () -> result)

let beginReadDefaultLine harness =
    let mutable result: string option option = None
    let worker =
        Thread(ThreadStart(fun () ->
            let lineOpt = harness.TryReadDefaultLine.Invoke(harness.Controller, [||]) :?> string option
            result <- Some lineOpt))
    worker.IsBackground <- true
    worker.Start()
    worker, (fun () -> result)
