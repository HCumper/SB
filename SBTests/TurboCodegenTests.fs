module SBTests.TurboCodegenTests

// TurboCodegen (the F# port of Turbo's code generator) is checked against the genuine generator,
// Codegen 3.27, run in a 68000 emulator (TurboCodegenOracle): for every program the two must
// produce the same executable, byte for byte.

open System
open System.IO
open NUnit.Framework
open Serilog

open CompilerPipeline
open HirTurboIrBackend
open TurboCodegen
open SBTests

let private hexBytes (text: string) =
    text.Split([| ' '; char 10; char 13 |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun pair -> Convert.ToByte(pair, 16))

/// TURBO-IR-EXAMPLE.hex (spec 10.1).
let private specExample = hexBytes """
    03 E7 00 02 00 FF 00 F2 27 11 00 FE 27 10 00 FB
    00 FF 27 11 00 F2 27 15 00 FF 27 12 01 46 01 5A
    00 6A 00 2A 00 6A 00 01 00 05 00 76 00 01 00 DC
    00 F2 27 17 00 FE 27 16 01 05 FF FF 00 00 00 FF
    27 17 00 FF 27 15 00 72 00 01 01 0B 00 ED 27 10
    00 F2 27 12 00 FE 27 13 01 05 FF FF 00 00 00 F3
    27 13 01 66 00 00 00 F3 27 16 00 00
"""

let private newline = string (char 10)

let private source (text: string) =
    text.Split(char 10)
    |> Array.map (fun line -> line.Trim())
    |> Array.filter (fun line -> line <> "")
    |> String.concat newline
    |> fun text -> text + newline

let private testSettings inputFile =
    { InputFileName = inputFile
      OutputFileName = Path.GetTempFileName()
      Verbose = false
      Backend = "turbo"
      RuntimeHost = "console"
      ExecutionThrottleEnabled = false
      ExecutionThrottleTargetStatementsPerSecond = 10000
      ExecutionThrottleMaxRunAheadMilliseconds = 2
      SyntaxChecking = Relaxed
      AppName = "Test"
      Logger = LoggerConfiguration().CreateLogger() }

/// The Turbo IR for an SB program.
let private irOf (text: string) =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.bas")
    File.WriteAllText(path, source text)
    try
        match loadAstFromInput (testSettings path) with
        | Result.Error error -> failwith $"Parsing failed: %A{error}"
        | Result.Ok(_, _, ast) ->
            let state = runSemanticAnalysis ast
            if not state.Errors.IsEmpty then failwith (String.concat "; " state.Errors)
            match runHirLowering state with
            | Result.Error errors -> failwith $"%A{errors}"
            | Result.Ok hir ->
                match generateTurboIrFromHir (turboIrOptionsFromState state) hir with
                | Result.Ok output -> output.Bytes
                | Result.Error errors -> failwith (String.concat "; " errors)
    finally
        File.Delete path

let private oracleSettings = TurboCodegenOracle.defaultSettings

let private portOptions =
    { defaultCodegenOptions with
        TaskName = oracleSettings.TaskName
        DataspaceKiB = oracleSettings.DataspaceKiB
        StackSize = Some oracleSettings.StackSize }

let private toHex (bytes: byte[]) = bytes |> Array.map (fun b -> b.ToString("X2")) |> String.concat " "

/// Runs both generators and requires identical output.
let private assertSameAsReference (ir: byte[]) =
    let reference = TurboCodegenOracle.run oracleSettings ir
    Assert.That(reference.Status, Is.EqualTo(-1), $"The reference code generator failed: {reference.Screen}")
    match generateFromIr portOptions ir with
    | Result.Error errors -> Assert.Fail(String.concat "; " errors)
    | Result.Ok output ->
        Assert.That(output.Dataspace, Is.EqualTo(reference.Dataspace))
        let expected = reference.Output
        let actual = output.Code
        let firstDifference =
            Seq.zip expected actual |> Seq.tryFindIndex (fun (x, y) -> x <> y)
            |> Option.orElse (if expected.Length <> actual.Length then Some(min expected.Length actual.Length) else None)
        match firstDifference with
        | None -> ()
        | Some offset ->
            let window (bytes: byte[]) = toHex bytes[max 0 (offset - 8) .. min (bytes.Length - 1) (offset + 24)]
            Assert.Fail(
                $"Outputs differ at byte {offset} (reference {expected.Length} bytes, port {actual.Length})."
                + $"{newline}reference: {window expected}{newline}port:      {window actual}")

let private programs = [
    ("arith", """
        10 a% = 7 : b = 2.5 : s$ = "x"
        20 c = a% + b
        30 d% = a% * 3 - 1
        40 e = a% / 2
        50 f% = a% DIV 2 + a% MOD 3
        60 g = 2 ^ a%
        70 h% = -a%
        80 i = -b
        90 j = NOT a%
        100 k = a% > 1 AND b <= 3 OR s$ = "y" XOR b <> 0
        110 t$ = s$ & a% & b
        120 p% = "lo" INSTR "hello"
        130 q% = b
        140 r = "12"
    """)
    ("strings", """
        10 x$ = "abcdef"
        20 c$ = x$(3)
        30 d$ = x$(2 TO 4)
        40 x$(1) = "Z"
        50 n% = LEN(x$) + LEN(c$ & d$)
    """)
    ("arrays", """
        10 DIM a%(5), m(2,3), n$(4,12)
        20 a%(2) = 9
        30 m(1,2) = a%(2) / 4
        40 n$(3) = "name"
        50 n$(1,2) = "q"
        60 PRINT n$(3), n$(1,2)
    """)
    ("print", """
        10 PRINT
        20 PRINT "a", 1; 2.5
        30 PRINT #2, "x";
        40 PRINT "p",
        50 PRINT #0, "y"
    """)
    ("input", """
        10 DIM v(3)
        20 INPUT "name"; n$
        30 INPUT #0, v(1), k%
    """)
    ("for", """
        10 FOR i% = 1 TO 10 STEP 2
        20 IF i% = 5 THEN NEXT i%
        30 IF i% = 9 THEN EXIT i%
        40 PRINT i%
        50 END FOR i%
        60 FOR x = 0 TO 1 STEP 0.25 : PRINT x : END FOR x
        70 FOR y = 1 TO 3 : PRINT y : END FOR y
        80 FOR k = 1, 3 TO 5, 9 : PRINT k : END FOR k
    """)
    ("repeat", """
        10 n% = 0
        20 REPeat loop
        30 n% = n% + 1
        40 IF n% < 3 THEN NEXT loop
        50 IF n% > 5 THEN EXIT loop
        60 END REPeat loop
    """)
    ("jumps", """
        5 n% = 1
        10 GO TO 40
        20 PRINT "skipped"
        40 GO SUB 100
        50 ON n% GO TO 20, 40
        60 ON n% GO SUB 100, 110
        70 STOP
        100 PRINT "a"
        110 RETurn
    """)
    ("routines", """
        10 c = 0 : w$ = "hi"
        30 bump c, w$
        50 PRINT greet$(w$), half(7)
        60 DEFine PROCedure bump(n, s$)
        70 REFERENCE n
        80 LOCal k%, m$, z(2)
        90 n = n + 1 : z(1) = n : k% = LEN(s$) : m$ = s$
        100 END DEFine
        110 DEFine FuNction greet$(who$)
        120 RETurn "Hi " & who$
        130 END DEFine
        140 DEFine FuNction half(v)
        150 RETurn v / 2
        160 END DEFine
    """)
    ("data", """
        10 RESTORE 100
        20 READ a, b$, c%
        30 RESTORE
        40 DATA 0, 25670, 25671, -2998, -2999, 1.5, 3.0, 100000
        100 DATA 7, "seven", -1
    """)
    ("when", """
        10 WHEN ERROR
        20 PRINT ERNUM, ERLIN
        30 IF ERNUM = -7 THEN RETRY
        40 CONTINUE
        50 END WHEN
        60 OPEN #3, "nofile"
    """)
    ("builtins", """
        10 a$ = CHR$(65) & FILL$("-", 3)
        20 c% = CODE(a$)
        30 POKE 131072, 255 : p% = PEEK(131072) : q = PEEK_L(131072)
        40 r = RESPR(100)
        50 e = EOF(#3) : d = PI
        60 x = RND(1 TO 6) : y = SIN(1)
        70 INK #2, 7 : CLS : AT 1, 2
        80 z$ = INKEY$
    """)
    ("manifest", """
        10 MANIFEST limit = 10
        20 IMPLICIT% count
        30 count = limit * 2
        40 PRINT count
    """)
    ("improve", """
        10 x = 1 : y% = 3
        20 x = x + 1 : x = x + 2 : x = x + 4
        30 y% = y% + 1 : y% = y% - 9 : y% = y% + 100
        40 z = x * x : w% = y% * y%
        50 IF x > 3 THEN GO TO 70
        60 PRINT "no"
        70 IF y% < 0 THEN GO TO 50 : ELSE PRINT "yes"
    """)
    ("scores", """
        100 REMark Scores: read, sort and report a table
        110 TURBO_windo 1
        120 DIM score%(5), name$(5,10)
        130 count% = 0
        140 REPeat read_loop
        150   READ who$, pts%
        160   IF who$ = "end" THEN EXIT read_loop
        170   count% = count% + 1
        180   name$(count%) = who$ : score%(count%) = pts%
        190 END REPeat read_loop
        200 sort count%
        210 CLS
        220 FOR i% = 1 TO count%
        230   PRINT rank$(i%); " "; name$(i%), score%(i%)
        240 END FOR i%
        250 PRINT "Average: "; average(count%)
        260 STOP
        270 DEFine PROCedure sort(n%)
        280   LOCal i%, j%, t%, t$
        290   FOR i% = 1 TO n% - 1
        300     FOR j% = i% + 1 TO n%
        310       IF score%(j%) > score%(i%) THEN
        320         t% = score%(i%) : score%(i%) = score%(j%) : score%(j%) = t%
        330         t$ = name$(i%) : name$(i%) = name$(j%) : name$(j%) = t$
        340       END IF
        350     END FOR j%
        360   END FOR i%
        370 END DEFine
        380 DEFine FuNction rank$(r%)
        390   IF r% = 1 THEN RETurn "1st"
        400   RETurn r% & "th"
        410 END DEFine
        420 DEFine FuNction average(n%)
        430   LOCal k%, total
        440   total = 0
        450   FOR k% = 1 TO n% : total = total + score%(k%) : END FOR k%
        460   RETurn total / n%
        470 END DEFine
        480 DATA "Ann", 72, "Bob", 65, "Cy", 91
        490 DATA "end", 0
    """) ]
let programNames = programs |> List.map fst |> Array.ofList

[<Test>]
let ``the reference code generator compiles the spec example`` () =
    let result = TurboCodegenOracle.run oracleSettings specExample

    Assert.That(result.Status, Is.EqualTo(-1), result.Screen)
    Assert.That(result.FileType, Is.EqualTo(1))
    Assert.That(result.Dataspace, Is.EqualTo(oracleSettings.DataspaceKiB * 1024))

[<Test>]
let ``the bundled library is the one inside the reference code generator`` () =
    let task = TurboCodegenOracle.codegenTask.Value
    Assert.That(toHex task[9066..], Is.EqualTo(toHex library.Value))

[<Test>]
let ``the port matches the reference code generator on the spec example`` () =
    assertSameAsReference specExample

[<TestCaseSource(nameof programNames)>]
let ``the port matches the reference code generator`` (name: string) =
    assertSameAsReference (irOf (programs |> List.find (fun (n, _) -> n = name) |> snd))

/// The same program with its body and routines switched to native code (254 after the body label,
/// 255 before the linkage branch), so the native template, jump and string paths are exercised.
let private nativeBodyOf (ir: byte[]) =
    match decode ir with
    | Result.Error message -> failwith message
    | Result.Ok items ->
        let items = List.toArray items
        let bodyStart = items |> Array.findIndex (fun item -> item = LabelDef(TurboIrLabels.body, ""))
        let bodyEnd = items |> Array.findIndex (fun item -> item = Instruction(242, [ LabelRef TurboIrLabels.afterLinkage ], ""))
        serialize (
            Array.concat
                [ items[..bodyStart]
                  [| Instruction(254, [], "") |]
                  items[bodyStart + 1 .. bodyEnd - 1]
                  [| Instruction(255, [], "") |]
                  items[bodyEnd..] ])

[<Test>]
let ``the port matches the reference improver on the parser's integer-under-float form`` () =
    // x = 1 + v as Turbo's parser writes it: the integer pushed first, then hidden, converted and
    // recovered around the float operand (106 1, 141 v, 155, 320, 156). Our emitter converts at
    // compile time, so the stream is rewritten into the parser's form here.
    let ir = irOf "10 v = 2.5\n20 x = 1 + v"
    match decode ir with
    | Result.Error message -> Assert.Fail message
    | Result.Ok items ->
        let items = List.toArray items
        let constant = items |> Array.findIndex (fun item -> item = Instruction(377, [ Word 2049; Word 16384 ], ""))
        let fetch = items[constant + 1]
        Assert.That(fetch, Is.InstanceOf<IrItem>())
        let rewritten =
            Array.concat
                [ items[.. constant - 1]
                  [| Instruction(106, [ Word 1 ], ""); fetch; Instruction(155, [], ""); Instruction(320, [], ""); Instruction(156, [], "") |]
                  items[constant + 2 ..] ]
        assertSameAsReference (serialize rewritten)

[<TestCaseSource(nameof programNames)>]
let ``the port matches the reference code generator on native code`` (name: string) =
    assertSameAsReference (nativeBodyOf (irOf (programs |> List.find (fun (n, _) -> n = name) |> snd)))

// ---------------------------------------------------------------------------
// Command-line selection
// ---------------------------------------------------------------------------

let private withTempDirectory action =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}")
    Directory.CreateDirectory(path) |> ignore
    try
        action path
    finally
        if Directory.Exists(path) then
            Directory.Delete(path, true)

[<Test>]
let ``the turbo backend writes the reference executable with a QDOS file header`` () =
    withTempDirectory (fun dir ->
        let program = programs |> List.find (fun (n, _) -> n = "scores") |> snd
        let sourcePath = Path.Combine(dir, "scores.bas")
        let outputPath = Path.Combine(dir, "scores_exe")
        File.WriteAllText(sourcePath, source program)

        let exitCode = Program.main [| sourcePath; outputPath; "false"; "turbo" |]

        Assert.That(exitCode, Is.EqualTo(0))
        let written = File.ReadAllBytes outputPath
        let reference =
            TurboCodegenOracle.run
                { oracleSettings with TaskName = "scores_exe" }
                (irOf program)
        Assert.That(reference.Status, Is.EqualTo(-1), reference.Screen)
        Assert.That(toHex written[..29], Is.EqualTo(toHex (qdosFileHeader reference.Dataspace)))
        Assert.That(toHex written[30..], Is.EqualTo(toHex reference.Output)))

[<Test>]
let ``the QDOS file header matches the one on the reference code generator file`` () =
    let task = TurboCodegenOracle.codegenTask.Value
    Assert.That(toHex (qdosFileHeader 0x19000), Is.EqualTo(toHex task[..29]))

[<Test>]
let ``a long task name is rejected`` () =
    match generateFromIr { portOptions with TaskName = "thirteen_char" } specExample with
    | Result.Error errors -> Assert.That(errors[0], Does.Contain("longer than 12"))
    | Result.Ok _ -> Assert.Fail("Expected a long task name to be rejected.")

[<Test>]
let ``an undefined label stops code generation`` () =
    // The spec example with its first branch pointed at a label that is never defined.
    let broken = Array.copy specExample
    broken[8] <- 0x2Fuy
    match generateFromIr portOptions broken with
    | Result.Error errors -> Assert.That(errors[0], Does.Contain("Label 12049 not defined."))
    | Result.Ok _ -> Assert.Fail("Expected an undefined label to stop code generation.")
