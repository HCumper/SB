module SBRuntimeTests.AvaloniaHostIntegrationTests

open System
open System.Threading
open NUnit.Framework

open SBRuntimeTests.HostInputContract
open SBRuntimeTests.TestSupport

[<Test>]
let ``avalonia controller submits text input to screen channel program`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#1,\"Enter\",n\n20 PRINT n\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 1 "Enter" 3000, Is.True, "Timed out waiting for channel #1 prompt.")

    sendText harness "42"
    tapSpecialKey harness 13

    assertCompleted worker getResult "Enter|42" "after input submission"

[<Test>]
let ``avalonia controller supports left arrow and backspace editing`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#1,\"Edit\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 1 "Edit" 3000, Is.True, "Timed out waiting for channel #1 edit prompt.")

    sendText harness "AB"
    tapSpecialKey harness 28
    tapSpecialKey harness 8
    sendText harness "C"
    tapSpecialKey harness 13

    assertCompleted worker getResult "Edit|CB" "after edited input submission"

[<Test>]
let ``avalonia controller gameplay pulse updates key row state`` () =
    let harness = createAvaloniaHarness ()

    harness.PulseGameplayKey.Invoke(harness.Controller, [||]) |> ignore
    let first = harness.GetKeyRow.Invoke(harness.Controller, [| box 1 |]) :?> int
    harness.ReleaseGameplayKey.Invoke(harness.Controller, [||]) |> ignore
    let cleared = harness.GetKeyRow.Invoke(harness.Controller, [| box 1 |]) :?> int
    harness.PulseGameplayKey.Invoke(harness.Controller, [||]) |> ignore
    let second = harness.GetKeyRow.Invoke(harness.Controller, [| box 1 |]) :?> int
    harness.ReleaseGameplayKey.Invoke(harness.Controller, [||]) |> ignore

    Assert.That(first = 0x01 || first = 0x02, Is.True)
    Assert.That(cleared, Is.EqualTo(0))
    Assert.That(second = 0x01 || second = 0x02, Is.True)
    Assert.That(second, Is.Not.EqualTo(first))

[<Test>]
let ``avalonia controller renders prompt pane text before enter`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#0,\"Prompt\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 0 "Prompt" 3000, Is.True, "Timed out waiting for channel #0 prompt.")

    sendText harness "ABC"
    Thread.Sleep(50)

    Assert.That(waitForPaneText harness 0 "ABC" 3000, Is.True, "Typed text was not rendered into the prompt pane before submit.")

    tapSpecialKey harness 13

    assertCompleted worker getResult "Prompt|ABC" "after prompt pane submission"

[<TestCase(0, 25)>]
[<TestCase(1, 10)>]
let ``avalonia controller handles repeated prompt submissions`` channelId iterations =
    runRepeatedPromptIterations channelId iterations

[<TestCase(0, "Prompt", "ABC")>]
[<TestCase(1, "Enter", "ABC")>]
let ``avalonia controller captures early prompt text across channels`` channelId promptText submittedText =
    runEarlyPromptCapture channelId promptText submittedText

[<Test>]
let ``avalonia controller updates prompt pane after each editing key`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#0,\"Edit\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 0 "Edit" 3000, Is.True, "Timed out waiting for channel #0 edit prompt.")

    sendText harness "A"
    let paneAfterA = assertPaneContains harness 0 "EditA" "Prompt pane did not render the first character."
    Assert.That(paneAfterA.Cursor, Is.EqualTo((5, 0)), "Cursor did not advance after first prompt character.")

    sendText harness "B"
    let paneAfterB = assertPaneContains harness 0 "EditAB" "Prompt pane did not append the second character."
    Assert.That(paneAfterB.Cursor, Is.EqualTo((6, 0)), "Cursor did not advance after second prompt character.")

    tapSpecialKey harness 28
    let paneAfterLeft = tryFindPane harness 0 |> Option.get
    Assert.That(paneAfterLeft.Cursor, Is.EqualTo((5, 0)), "Cursor did not move left within the prompt pane.")

    tapSpecialKey harness 8
    let paneAfterBackspace = assertPaneContains harness 0 "EditB" "Backspace did not remove the expected prompt character."
    Assert.That(paneAfterBackspace.Cursor, Is.EqualTo((4, 0)), "Cursor did not move back after prompt-pane backspace.")

    tapSpecialKey harness 127
    let paneAfterDelete = assertPaneContains harness 0 "Edit" "Delete did not remove the remaining prompt character."
    Assert.That(paneAfterDelete.Cursor, Is.EqualTo((4, 0)), "Cursor moved unexpectedly after prompt-pane delete.")

    sendText harness "C"
    let paneAfterC = assertPaneContains harness 0 "EditC" "Prompt pane did not render the replacement character."
    Assert.That(paneAfterC.Cursor, Is.EqualTo((5, 0)), "Cursor did not advance after replacement prompt character.")

    tapSpecialKey harness 13

    assertCompleted worker getResult "Edit|C" "after prompt-pane edit submission"

[<Test>]
let ``avalonia controller renders prompt input at cursor origin after at`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 AT#0,3,5:INPUT#0,\"Angle\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 0 "Angle" 3000, Is.True, "Timed out waiting for positioned channel #0 prompt.")

    let promptPane = assertPaneContains harness 0 "Angle" "Positioned prompt was not visible in channel #0."
    Assert.That(promptPane.CursorVisible, Is.True, "Cursor should be visible during positioned input.")

    sendText harness "XY"

    let paneAfterTyping = assertPaneContains harness 0 "AngleXY" "Positioned prompt input was not rendered at the active cursor origin."
    Assert.That(paneAfterTyping.Cursor, Is.EqualTo((12, 3)), "Cursor did not advance from the positioned prompt origin.")

    tapSpecialKey harness 13

    assertCompleted worker getResult "Angle|XY" "after positioned prompt submission"

[<Test>]
let ``avalonia controller wraps long prompt pane input across rows before enter`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#0,\"Prompt\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 0 "Prompt" 3000, Is.True, "Timed out waiting for channel #0 prompt.")

    let longText = String.replicate 60 "X"
    sendText harness longText

    let paneAfterTyping = tryFindPane harness 0 |> Option.get
    let row0 = rowText paneAfterTyping 0
    let row1 = rowText paneAfterTyping 1

    Assert.That(row0.TrimEnd(), Is.EqualTo("Prompt" + String.replicate 58 "X"), "First prompt row did not contain the expected wrapped text.")
    Assert.That(row1.TrimEnd(), Is.EqualTo("XX"), "Second prompt row did not contain the wrapped tail before submit.")
    Assert.That(paneAfterTyping.Cursor, Is.EqualTo((2, 1)), "Cursor did not advance onto the wrapped prompt row.")

    tapSpecialKey harness 13

    assertCompleted worker getResult ("Prompt|" + longText) "after wrapped prompt submission"

[<Test>]
let ``avalonia controller supports home and end during prompt pane editing`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#0,\"Edit\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 0 "Edit" 3000, Is.True, "Timed out waiting for channel #0 edit prompt.")

    sendText harness "AB"
    let paneAfterAB = assertPaneContains harness 0 "EditAB" "Prompt pane did not render initial text for HOME/END editing."
    Assert.That(paneAfterAB.Cursor, Is.EqualTo((6, 0)), "Cursor did not advance after initial prompt text.")

    tapSpecialKey harness 1
    let paneAfterHome = tryFindPane harness 0 |> Option.get
    Assert.That(paneAfterHome.Cursor, Is.EqualTo((4, 0)), "HOME did not move the cursor to the prompt origin.")

    sendText harness "C"
    let paneAfterInsert = assertPaneContains harness 0 "EditCAB" "HOME insertion did not render at the prompt origin."
    Assert.That(paneAfterInsert.Cursor, Is.EqualTo((5, 0)), "Cursor did not advance after HOME insertion.")

    tapSpecialKey harness 5
    let paneAfterEnd = tryFindPane harness 0 |> Option.get
    Assert.That(paneAfterEnd.Cursor, Is.EqualTo((7, 0)), "END did not move the cursor to the end of prompt text.")

    sendText harness "D"
    let paneAfterEndInsert = assertPaneContains harness 0 "EditCABD" "END insertion did not append to the prompt text."
    Assert.That(paneAfterEndInsert.Cursor, Is.EqualTo((8, 0)), "Cursor did not advance after END insertion.")

    tapSpecialKey harness 13

    assertCompleted worker getResult "Edit|CABD" "after HOME/END prompt submission"

[<Test>]
let ``avalonia controller clears active prompt pane text on escape`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#0,\"Edit\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 0 "Edit" 3000, Is.True, "Timed out waiting for channel #0 edit prompt.")

    sendText harness "ABC"
    let paneAfterTyping = assertPaneContains harness 0 "EditABC" "Prompt pane did not render text before ESC."
    Assert.That(paneAfterTyping.Cursor, Is.EqualTo((7, 0)), "Cursor did not advance before ESC.")

    tapSpecialKey harness 27
    let paneAfterEsc = tryFindPane harness 0 |> Option.get
    Assert.That(paneText paneAfterEsc, Does.Contain("Edit"), "Prompt text disappeared after ESC.")
    Assert.That(paneText paneAfterEsc, Does.Not.Contain("ABC"), "ESC did not clear the active prompt text.")
    Assert.That(paneAfterEsc.Cursor, Is.EqualTo((4, 0)), "Cursor did not return to the prompt origin after ESC.")

    sendText harness "Z"
    let paneAfterRetype = assertPaneContains harness 0 "EditZ" "Prompt pane did not accept new text after ESC clear."
    Assert.That(paneAfterRetype.Cursor, Is.EqualTo((5, 0)), "Cursor did not advance after retyping post-ESC.")

    tapSpecialKey harness 13

    assertCompleted worker getResult "Edit|Z" "after ESC-cleared prompt submission"

[<Test>]
let ``avalonia controller flush input clears active prompt pane session`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#0,\"Prompt\",a$\n20 PRINT a$\n"
    let worker, _ = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 0 "Prompt" 3000, Is.True, "Timed out waiting for channel #0 prompt.")

    sendText harness "ABC"
    let paneAfterTyping = assertPaneContains harness 0 "PromptABC" "Prompt pane did not render text before flush."
    Assert.That(paneAfterTyping.CursorVisible, Is.True, "Cursor should be visible before flushing prompt input.")

    harness.Host.Input.Flush()

    let paneAfterFlush = tryFindPane harness 0 |> Option.get
    Assert.That(paneText paneAfterFlush, Does.Contain("Prompt"), "Prompt text disappeared after flush.")
    Assert.That(paneText paneAfterFlush, Does.Not.Contain("ABC"), "Flush did not clear active prompt text.")
    Assert.That(paneAfterFlush.CursorVisible, Is.False, "Flush should hide the prompt cursor.")

    sendText harness "Q"
    Assert.That(waitForPaneText harness 0 "PromptQ" 500, Is.False, "Text input should not render while no prompt session is active after flush.")

    tapSpecialKey harness 13
    Assert.That(worker.Join(500), Is.False, "Interpreter should still be waiting for input after flush without a restarted prompt.")

    harness.Host.Input.Flush()

[<TestCase(0)>]
[<TestCase(1)>]
let ``avalonia controller handles sequential prompt sessions across channels`` channelId =
    runSequentialPromptScenario channelId

[<Test>]
let ``avalonia controller preserves mixed editing sequence during prompt activation`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#0,\"Prompt\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    sendText harness "A"
    tapSpecialKey harness 1
    sendText harness "B"
    tapSpecialKey harness 5
    sendText harness "C"

    Assert.That(waitForPaneText harness 0 "PromptBAC" 3000, Is.True, "Prompt pane did not preserve mixed printable/special-key input during activation.")

    let paneAfterSequence = tryFindPane harness 0 |> Option.get
    Assert.That(paneAfterSequence.Cursor, Is.EqualTo((9, 0)), "Cursor did not end at the expected position after mixed activation input.")

    tapSpecialKey harness 13

    assertCompleted worker getResult "Prompt|BAC" "after mixed activation prompt submission"

[<Test>]
let ``avalonia controller edits wrapped prompt text across row boundaries`` () =
    let harness = createAvaloniaHarness ()
    let ast = parseAstFromSource "10 INPUT#0,\"Prompt\",a$\n20 PRINT a$\n"
    let worker, getResult = runProgramAsync harness.Host ast

    Assert.That(waitForPaneText harness 0 "Prompt" 3000, Is.True, "Timed out waiting for wrapped channel #0 prompt.")

    let longText = String.replicate 60 "X"
    sendText harness longText
    let paneAfterTyping = tryFindPane harness 0 |> Option.get
    Assert.That(paneAfterTyping.Cursor, Is.EqualTo((2, 1)), "Cursor did not advance onto the wrapped row before cross-row editing.")

    tapSpecialKey harness 28
    tapSpecialKey harness 28
    tapSpecialKey harness 28
    let paneAfterThreeLeft = tryFindPane harness 0 |> Option.get
    Assert.That(paneAfterThreeLeft.Cursor, Is.EqualTo((63, 0)), "Cursor did not wrap back to the previous row on left-arrow navigation.")

    tapSpecialKey harness 127
    let paneAfterDelete = tryFindPane harness 0 |> Option.get
    let deleteRow0 = rowText paneAfterDelete 0
    let deleteRow1 = rowText paneAfterDelete 1
    Assert.That(deleteRow0.TrimEnd(), Is.EqualTo("Prompt" + String.replicate 58 "X"), "Delete changed the wrapped first row unexpectedly.")
    Assert.That(deleteRow1.TrimEnd(), Is.EqualTo("X"), "Delete did not remove the expected wrapped-row character.")
    Assert.That(paneAfterDelete.Cursor, Is.EqualTo((63, 0)), "Cursor moved unexpectedly after wrapped-row delete.")

    tapSpecialKey harness 29
    tapSpecialKey harness 8
    let paneAfterBackspace = tryFindPane harness 0 |> Option.get
    let backspaceRow0 = rowText paneAfterBackspace 0
    let backspaceRow1 = rowText paneAfterBackspace 1
    Assert.That(backspaceRow0.TrimEnd(), Is.EqualTo("Prompt" + String.replicate 58 "X"), "Backspace did not pull the remaining wrapped character into the first row.")
    Assert.That(backspaceRow1.TrimEnd(), Is.EqualTo(String.Empty), "Backspace did not clear the wrapped tail character.")
    Assert.That(paneAfterBackspace.Cursor, Is.EqualTo((63, 0)), "Cursor did not land at the expected wrapped-row position after backspace.")

    tapSpecialKey harness 13

    assertCompleted worker getResult ("Prompt|" + String.replicate 58 "X") "after wrapped cross-row edit submission"
