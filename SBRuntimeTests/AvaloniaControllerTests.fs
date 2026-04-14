module SBRuntimeTests.AvaloniaControllerTests

open NUnit.Framework

open SBRuntimeTests.HostInputContract

[<Test>]
let ``avalonia controller default line reader returns submitted channel one text`` () =
    let harness = createAvaloniaHarness ()
    let worker, getResult = beginReadDefaultLine harness

    Assert.That(waitForCursorVisible harness 1 3000, Is.True, "Channel #1 input session did not become active.")

    sendText harness "ABC"
    let paneDuringInput = tryFindPane harness 1 |> Option.get
    Assert.That(paneText paneDuringInput, Does.Contain("ABC"), "Typed channel #1 text was not rendered during a direct controller read.")
    Assert.That(paneDuringInput.Cursor, Is.EqualTo((3, 0)), "Cursor did not advance during direct channel #1 input.")

    tapSpecialKey harness 13

    Assert.That(worker.Join(3000), Is.True, "Direct controller read on channel #1 did not complete after submit.")
    match getResult() with
    | Some(Some line) -> Assert.That(line, Is.EqualTo("ABC"))
    | Some None -> Assert.Fail("Direct controller read returned no line.")
    | None -> Assert.Fail("Direct controller read result was not captured.")

    let paneAfterSubmit = tryFindPane harness 1 |> Option.get
    Assert.That(paneAfterSubmit.CursorVisible, Is.False, "Cursor should be hidden after channel #1 submission.")

[<Test>]
let ``avalonia controller channel zero read applies editing before submit`` () =
    let harness = createAvaloniaHarness ()
    let worker, getResult = beginReadLine harness 0

    Assert.That(waitForCursorVisible harness 0 3000, Is.True, "Channel #0 input session did not become active.")

    sendText harness "AB"
    tapSpecialKey harness 28
    tapSpecialKey harness 8
    sendText harness "C"

    let paneDuringInput = tryFindPane harness 0 |> Option.get
    Assert.That(paneText paneDuringInput, Does.Contain("CB"), "Edited channel #0 text was not rendered during a direct controller read.")
    Assert.That(paneDuringInput.Cursor, Is.EqualTo((1, 0)), "Cursor did not land at the expected position during edited channel #0 input.")

    tapSpecialKey harness 13

    Assert.That(worker.Join(3000), Is.True, "Direct controller read on channel #0 did not complete after submit.")
    match getResult() with
    | Some(Some line) -> Assert.That(line, Is.EqualTo("CB"))
    | Some None -> Assert.Fail("Direct controller channel #0 read returned no line.")
    | None -> Assert.Fail("Direct controller channel #0 result was not captured.")

[<Test>]
let ``avalonia controller queues printable text to runtime input when no prompt is active`` () =
    let harness = createAvaloniaHarness ()

    sendText harness "AZ"

    let firstKey = harness.Host.Input.ReadKey()
    let secondKey = harness.Host.Input.ReadKey()
    let thirdKey = harness.Host.Input.ReadKey()

    match firstKey, secondKey, thirdKey with
    | Some first, Some second, None ->
        Assert.That(first.KeyCode, Is.EqualTo(int 'A'))
        Assert.That(first.Character, Is.EqualTo(Some 'A'))
        Assert.That(second.KeyCode, Is.EqualTo(int 'Z'))
        Assert.That(second.Character, Is.EqualTo(Some 'Z'))
    | _ -> Assert.Fail("Printable text was not queued to the runtime input device as expected.")

[<Test>]
let ``avalonia controller queues special keys and exposes transient key rows without prompt`` () =
    let harness = createAvaloniaHarness ()
    let enter = keyInfo 13

    harness.HandleSpecialKey.Invoke(harness.Controller, [| box enter |]) |> ignore

    let queued = harness.Host.Input.ReadKey()
    let firstRowRead = harness.Host.Input.GetKeyRow(1)
    let secondRowRead = harness.Host.Input.GetKeyRow(1)

    harness.ReleaseSpecialKey.Invoke(harness.Controller, [| box enter |]) |> ignore

    match queued with
    | Some key ->
        Assert.That(key.KeyCode, Is.EqualTo(13))
        Assert.That(key.Character, Is.EqualTo(None))
    | None -> Assert.Fail("Enter key was not queued to the runtime input device.")

    Assert.That(firstRowRead, Is.EqualTo(0x20), "Transient enter key row bit was not exposed on first read.")
    Assert.That(secondRowRead, Is.EqualTo(0), "Transient enter key row bit did not clear after being read.")

[<Test>]
let ``avalonia controller flush input clears direct prompt session rendering`` () =
    let harness = createAvaloniaHarness ()
    let worker, _ = beginReadLine harness 0

    Assert.That(waitForCursorVisible harness 0 3000, Is.True, "Channel #0 input session did not become active before flush.")

    sendText harness "ABC"
    let paneDuringInput = tryFindPane harness 0 |> Option.get
    Assert.That(paneText paneDuringInput, Does.Contain("ABC"), "Direct controller prompt text was not rendered before flush.")

    harness.FlushInput.Invoke(harness.Controller, [||]) |> ignore

    let paneAfterFlush = tryFindPane harness 0 |> Option.get
    Assert.That(paneText paneAfterFlush, Does.Not.Contain("ABC"), "FlushInput did not clear direct controller prompt text.")
    Assert.That(paneAfterFlush.CursorVisible, Is.False, "FlushInput should hide the direct controller prompt cursor.")
    Assert.That(worker.Join(500), Is.False, "FlushInput should not complete a waiting direct controller read.")
