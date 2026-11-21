module SBTests.TurboCodegenTests

// TurboCodegen (the F# port of Turbo's code generator) is checked against the genuine generator,
// Codegen 3.27, run in a 68000 emulator (TurboCodegenOracle). Where the original has a known
// improver bug, compare against an equivalent stream that avoids the faulty rewrite.

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
      TurboLocalRegisterAllocation = false; TurboNativeRegions = false; TurboHirOptimization = HirTurboOptimizer.defaultOptions; AppName = "Test"
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
let private assertMatchesReference (referenceIr: byte[]) (ir: byte[]) =
    let reference = TurboCodegenOracle.run oracleSettings referenceIr
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

let private assertSameAsReference ir = assertMatchesReference ir ir

let private programs = [
    ("flexible_bindings", """
        10 a% = 1
        20 bump a%
        30 bump (a%)
        40 PRINT a%
        100 DEFine PROCedure bump(n%)
        110 IF n% > 1 THEN GO TO 130
        120 n% = n% + 1
        130 END DEFine
    """)
    ("function_fallthrough", """
        10 PRINT f%(0), g(0), s$(0)
        100 DEFine FuNction f%(x%)
        110 IF x% > 0 THEN RETurn 7
        120 END DEFine
        200 DEFine FuNction g(x%)
        210 IF x% > 0 THEN RETurn 7
        220 END DEFine
        300 DEFine FuNction s$(x%)
        310 IF x% > 0 THEN RETurn "seven"
        320 END DEFine
    """)
    ("manifest_shadow", """
        10 MANIFEST : n = 3
        20 PRINT f(7)
        100 DEFine FuNction f(n)
        110 RETurn n
        120 END DEFine
    """)
    ("recursive_bindings", """
        10 PRINT count%(3)
        100 DEFine FuNction count%(n%)
        110 IF n% = 0 THEN RETurn 0
        120 n% = n% - 1
        130 RETurn 1 + count%(n%)
        140 END DEFine
    """)
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
    ("slices", """
        10 DIM c$(3,10)
        20 c$(1) = "Hello"
        30 x$ = "abcdef"
        40 PRINT x$(3 TO), c$(1)(2 TO 4), c$(1, 1 TO 2), c$(1)(3 TO)
        50 show
        60 DEFine PROCedure show
        70 PRINT c$(1)(2 TO 3)
        80 END DEFine
    """)
    ("separators", """
        10 n% = 12
        20 PRINT "a"!"b"!n%
        30 PRINT "ab" TO 6;"x" TO n% + 2,"y"
        40 PRINT 1,2!3;
        50 PRINT "p"!
    """)
    ("almost_equal", """
        10 a$ = "abc" : b = 1.00000001 : i% = 3
        20 IF a$ == "ABC" THEN PRINT "s"
        30 IF b == 1 THEN PRINT "f"
        40 IF i% == 3 THEN PRINT "i"
        50 PRINT a$ == "abd", b == 1.001
    """)
    ("dim_dynamic", """
        10 n = 5
        20 DIM a(n), b$(3,10), c$(20), d%(2,2)
        30 a(1) = 2 : c$ = "hello"
        40 show 4
        50 DEFine PROCedure show(k)
        60   LOCal t(k), s$(12)
        70   t(1) = k : s$ = "x"
        80   PRINT t(1); s$; c$
        90 END DEFine
    """)
    ("whole_arrays", """
        10 DIM a(3)
        20 fill a
        30 PRINT a(1), DIMN(a), total(a)
        40 test 1,,3
        50 DEFine PROCedure fill(x)
        60   x(1) = 7
        70 END DEFine
        80 DEFine FuNction total(y)
        90   RETurn y(1) * 2
        100 END DEFine
        110 DEFine PROCedure test(p,q,r)
        120   PRINT p, q, r
        130 END DEFine
    """)
    ("rom_separators", """
        10 LINE 0,0 TO 10,10
        20 LINE TO 20,5
        30 ARC 50,35 TO 50,25,-1.5
        40 LINE #2,1,2 TO 3,4
        50 CLOSE #3,#4
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

let private pressureIr () =
    irOf "10 a%=1:b%=2:c%=3:d%=4:e%=5:f%=6:g%=7:h%=8\n20 r%=a%+(b%+(c%+(d%+(e%+(f%+(g%+h%))))))\n30 PRINT r%\n40 STOP"
    |> nativeBodyOf

[<Test>]
let ``register spill frames reserve matching stack and task dataspace`` () =
    let ir=pressureIr()
    let before=generateFromIr portOptions ir |> Result.defaultWith (String.concat "; " >> failwith)
    let after=generateFromIr {portOptions with LocalRegisterAllocation=true} ir |> Result.defaultWith (String.concat "; " >> failwith)
    let report=after.NativeAllocation.Value
    Assert.That(report.StackExtraBytes, Is.GreaterThan(0))
    Assert.That(after.Dataspace-before.Dataspace, Is.EqualTo(report.StackExtraBytes))
    Assert.That(before.NativeAllocation.IsNone, Is.True)

[<Test>]
let ``maximum configured CPU stack falls back instead of overrunning its limit`` () =
    let ir=pressureIr()
    let options={portOptions with StackSize=Some 9998}
    let before=generateFromIr options ir |> Result.defaultWith (String.concat "; " >> failwith)
    let after=generateFromIr {options with LocalRegisterAllocation=true} ir |> Result.defaultWith (String.concat "; " >> failwith)
    Assert.That(after.NativeAllocation.Value.StackExtraBytes, Is.Zero)
    Assert.That(after.Code, Is.EqualTo(box before.Code))
    Assert.That(after.Dataspace, Is.EqualTo(before.Dataspace))

[<Test>]
let ``native FOR loops reserve their full live frame and preserve the original initializer`` () =
    let ir=irOf "10 total%=0\n20 FOR i%=1 TO 10\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP"
    let before=generateFromIr {portOptions with LocalRegisterAllocation=true} ir |> Result.defaultWith(String.concat "; " >> failwith)
    let after=generateFromIr {portOptions with LocalRegisterAllocation=true;NativeIntegerForLoops=true} ir |> Result.defaultWith(String.concat "; " >> failwith)
    let report=after.NativeLoops.Value
    Assert.That(report.Loops.Length, Is.EqualTo(1))
    let required=max after.NativeAllocation.Value.StackExtraBytes report.StackExtraBytes
    Assert.That(after.Dataspace-portOptions.DataspaceKiB*1024, Is.EqualTo(required))
    let oldRun=TurboExecutionHarness.run 2000000L (executableFile before)
    let newRun=TurboExecutionHarness.run 2000000L (executableFile after)
    Assert.That(newRun.Output, Is.EqualTo("55:10\n"))
    Assert.That(newRun.Output, Is.EqualTo(oldRun.Output))
    Assert.That(newRun.Steps, Is.LessThan(oldRun.Steps))

[<TestCase(9998)>]
[<TestCase(9990)>]
let ``native FOR loops fall back exactly when their complete frame cannot fit`` (stack:int) =
    let ir=irOf "10 FOR i%=1 TO 10\n20 a%=i%\n30 END FOR i%\n40 STOP"
    let options={portOptions with StackSize=Some stack;LocalRegisterAllocation=true}
    let before=generateFromIr options ir |> Result.defaultWith(String.concat "; " >> failwith)
    let after=generateFromIr {options with NativeIntegerForLoops=true} ir |> Result.defaultWith(String.concat "; " >> failwith)
    Assert.That(after.NativeLoops.Value.Loops, Is.Empty)
    Assert.That(after.Code, Is.EqualTo(box before.Code))
    Assert.That(after.Dataspace, Is.EqualTo(before.Dataspace))

[<Test>]
let ``native FOR layout remains correct when its entry is beyond 64 KiB`` () =
    let ir=irOf "10 FOR i%=1 TO 10\n20 a%=i%\n30 END FOR i%\n40 PRINT a%\n50 STOP"
    let items=decode ir |> Result.defaultWith failwith
    let wide=items |> List.collect(function
        | LabelDef(label,comment) when label=TurboIrLabels.body ->
            LabelDef(label,comment)::(List.replicate 18000 [Instruction(106,[Word 0],"");Instruction(118,[Word 1],"")] |> List.concat)
        | item -> [item]) |> serialize
    let result=generateFromIr {portOptions with LocalRegisterAllocation=true;NativeIntegerForLoops=true} wide |> Result.defaultWith(String.concat "; " >> failwith)
    Assert.That(result.NativeLoops.Value.Loops.Length, Is.EqualTo(1))
    Assert.That(result.Code.Length, Is.GreaterThan(65536))
    Assert.That((TurboExecutionHarness.run 3000000L (executableFile result)).Output, Is.EqualTo("10\n"))

let private numericArrayIr copies compact =
    let ir=irOf "10 DIM a%(2)\n20 i%=1\n30 a%(i%)=7\n40 r%=a%(i%)\n50 STOP" |> nativeBodyOf
    let items=decode ir |> Result.defaultWith failwith
    items |> List.collect(function
        | Instruction((111|112 as op),args,comment) ->
            List.replicate copies (Instruction((if compact then op+1000 else op),args,comment))
        | item -> [item]) |> serialize

[<Test>]
let ``compact numeric arrays grow by eight bytes per call site`` () =
    let generate copies compact = generateFromIr portOptions (numericArrayIr copies compact) |> Result.defaultWith(String.concat "; " >> failwith)
    let once=generate 1 true
    let many=generate 5001 true
    Assert.That(many.Code.Length-once.Code.Length, Is.EqualTo(10000*8))
    Assert.That(many.Code.Length, Is.GreaterThan(65536),"Calls into the prefix must work for a body beyond 64 KiB.")
    Assert.That(many.Code.Length, Is.LessThan((generate 5001 false).Code.Length))

[<Test>]
let ``compact array return frames reserve stack and dataspace consistently`` () =
    let baseline=generateFromIr portOptions (numericArrayIr 1 false) |> Result.defaultWith(String.concat "; " >> failwith)
    let compact=generateFromIr portOptions (numericArrayIr 1 true) |> Result.defaultWith(String.concat "; " >> failwith)
    Assert.That(compact.Dataspace-baseline.Dataspace, Is.EqualTo(8))

[<TestCase(9992)>]
[<TestCase(9998)>]
let ``compact arrays fall back to checked inline templates when the stack has no headroom`` (stack:int) =
    let options={portOptions with StackSize=Some stack}
    let generate compact = generateFromIr options (numericArrayIr 1 compact) |> Result.defaultWith(String.concat "; " >> failwith)
    let baseline,actual=generate false,generate true
    Assert.That(actual.Code, Is.EqualTo(box baseline.Code))
    Assert.That(actual.Dataspace, Is.EqualTo(baseline.Dataspace))

[<Test>]
let ``a full vector table uses checked inline arrays instead of overflowing the prefix`` () =
    let generate opcode =
        serialize [Instruction(999,[Word 8190],"");Instruction(106,[Word 0],"");Instruction(opcode,[Word 1],"");Instruction(0,[],"")]
        |> generateFromIr portOptions |> Result.defaultWith(String.concat "; " >> failwith)
    let baseline,actual=generate 111,generate 1111
    Assert.That(actual.Code, Is.EqualTo(box baseline.Code))
    Assert.That(actual.Dataspace, Is.EqualTo(baseline.Dataspace))

[<TestCase(9992)>]
[<TestCase(9998)>]
let ``range-proven arrays can fall back to checked inline code at the stack limit`` (stack:int) =
    let checkedIr=numericArrayIr 1 true
    let provenIr=decode checkedIr |> Result.defaultWith failwith |> List.map(function
        | Instruction((1111|1112 as op),args,comment) -> Instruction(op+500,args,comment)
        | item -> item) |> serialize
    let options={portOptions with StackSize=Some stack}
    let generate ir=generateFromIr options ir |> Result.defaultWith(String.concat "; " >> failwith)
    let expected,actual=generate checkedIr,generate provenIr
    Assert.That(actual.Code, Is.EqualTo(box expected.Code))
    Assert.That(actual.Dataspace, Is.EqualTo(expected.Dataspace))

[<TestCaseSource(nameof programNames)>]
let ``legacy mixed mode encoding retains the reference machine code layout`` (name: string) =
    let bytes = irOf (programs |> List.find (fun (n, _) -> n = name) |> snd)
    let items = decode bytes |> Result.defaultWith failwith
    let output = {Items=items; Bytes=bytes; Listing=formatListing items; Warnings=[]}
    let selected = TurboNativeRegions.select true output |> Result.defaultWith (String.concat "; " >> failwith)
    // The genuine generator cannot express checked native arithmetic: it
    // discards +1000 and emits unchecked N bodies. Compare the legacy encoding
    // on both sides to retain byte-level coverage of bridges, labels and all
    // shared templates. Explicit checked forms have independent CPU/executable
    // tests, including failures, retries and both allocation settings.
    let legacy=selected.Items |> List.map(function
        | Instruction((1001|1005|1006|1007|1111|1112|1143|1144 as op),args,comment) -> Instruction(op-1000,args,comment)
        | item -> item)
    assertSameAsReference (serialize legacy)

[<TestCase(239, false, false)>]
[<TestCase(239, false, true)>]
[<TestCase(239, true, false)>]
[<TestCase(239, true, true)>]
[<TestCase(245, false, false)>]
[<TestCase(245, false, true)>]
[<TestCase(245, true, false)>]
[<TestCase(245, true, true)>]
let ``branch inversion preserves a jump reached through its own label`` (condition: int, explicitLabel: bool, native: bool) =
    // Execution must take 10 -> 60 -> 30 -> 100. Deleting the labelled jump at 30
    // instead makes the incoming GO TO fall through to the wrong branch at 50.
    let ir = irOf "10 GO TO 60\n15 a% = 0\n20 IF a% = 0 THEN\n30 GO TO 100\n40 END IF\n50 PRINT \"wrong\" : STOP\n60 GO TO 30\n70 GO TO 50\n100 PRINT \"right\" : STOP"
    let ir = if native then nativeBodyOf ir else ir
    let items = decode ir |> Result.defaultWith failwith |> List.toArray
    let branch =
        items |> Array.findIndex (function Instruction(239, _, _) -> true | _ -> false)
    match items[branch] with
    | Instruction(_, operands, _) -> items[branch] <- Instruction(condition, operands, "")
    | _ -> failwith "Expected a conditional branch."
    Assert.That(items[branch + 1], Is.EqualTo(LabelDef(20030, "")))
    if explicitLabel then items[branch + 1] <- Instruction(378, [ LabelRef 20030 ], "")
    // Opcode 2 emits no code, but keeps the original improver from deleting the jump.
    let protectedReference =
        Array.concat [ items[..branch]; [| Instruction(2, [], "") |]; items[branch + 1..] ]
        |> serialize
    assertMatchesReference protectedReference (serialize items)

let integerUnderFloatCases =
    [| for value in [ -32768; -16385; -16384; -3; -2; -1; 0; 1; 2; 16383; 16384; 32767 ] do
           for operandOpcode in [ 138; 377; 141 ] do
               yield TestCaseData(value, operandOpcode) |]

[<TestCaseSource(nameof integerUnderFloatCases)>]
let ``integer-under-float conversion preserves signed Int16 values`` (value: int, operandOpcode: int) =
    let items = irOf "10 v = 2.5\n20 x = 1 + v" |> decode |> Result.defaultWith failwith |> List.toArray
    let constant = items |> Array.findIndex (fun item -> item = Instruction(377, [ Word 2049; Word 16384 ], ""))
    let floatOperand =
        match operandOpcode with
        | 138 -> Instruction(138, [ Word 2050; Word 20480; Word 0 ], "")
        | 377 -> Instruction(377, [ Word 2050; Word 20480 ], "")
        | _ -> items[constant + 1]
    let rewritten =
        Array.concat
            [ items[..constant - 1]
              [| Instruction(106, [ Word value ], ""); floatOperand
                 Instruction(155, [], ""); Instruction(320, [], ""); Instruction(156, [], "") |]
              items[constant + 2..] ]
        |> serialize
    // Compare with a correctly encoded float rather than the original's unsigned rewrite.
    let exponent, high, low = encodeQdosFloat (float value) |> Result.defaultWith failwith
    Assert.That(low, Is.Zero)
    let canonical =
        Array.concat
            [ items[..constant - 1]
              [| Instruction(377, [ Word exponent; Word high ], ""); floatOperand |]
              items[constant + 2..] ]
        |> serialize
    assertMatchesReference canonical rewritten

/// A native stream with no shared templates, or a P body that needs shared templates.
let private prefixTestIr vectors threaded padding =
    serialize [
        yield Instruction(999, [ Word vectors ], "")
        if threaded then yield Instruction(255, [], "")
        yield LabelDef(TurboIrLabels.body, "")
        for _ in 1..padding do
            yield Instruction(263, [], "")
            yield Instruction(221, [], "")
        yield Instruction(220, [], "")
        if threaded then yield Instruction(254, [], "")
        yield LabelDef(TurboIrLabels.linkage, "")
        yield LabelDef(TurboIrLabels.residentNames, "")
        yield Instruction(261, [ Word 0xFFFF; Bytes8 [||] ], "")
        yield Instruction(243, [ LabelRef TurboIrLabels.residentNames ], "")
        yield Instruction(358, [ Word 0 ], "")
        yield Instruction(243, [ LabelRef TurboIrLabels.linkage ], "")
        yield Instruction(0, [], "") ]

[<Test>]
let ``the largest native vector table with a signed entry offset is accepted`` () =
    let ir = prefixTestIr 8190 false 0
    Assert.That(validate ir, Is.Empty)
    match generateFromIr portOptions ir with
    | Result.Error errors -> Assert.Fail(String.concat "; " errors)
    | Result.Ok output ->
        let last = output.Code.Length - 2
        let entryOffset = (int output.Code[last] <<< 8) ||| int output.Code[last + 1]
        Assert.That(entryOffset, Is.EqualTo(32764))

[<TestCase(8191, false)>]
[<TestCase(16384, false)>]
[<TestCase(8190, true)>]
let ``an oversized runtime prefix is rejected before addresses wrap`` (vectors: int, threaded: bool) =
    // The last case fits the vector table, but copying a shared P template exceeds the limit.
    let ir = prefixTestIr vectors threaded 0
    Assert.That(validate ir, Is.Empty)
    match generateFromIr portOptions ir with
    | Result.Error errors -> Assert.That(errors[0], Does.Contain("runtime prefix"))
    | Result.Ok _ -> Assert.Fail("Expected an unaddressable runtime prefix to be rejected.")

[<TestCase(false)>]
[<TestCase(true)>]
let ``large program bodies remain valid when the runtime prefix fits`` (threaded: bool) =
    let ir = prefixTestIr 1 threaded 18000
    Assert.That(validate ir, Is.Empty)
    match generateFromIr portOptions ir with
    | Result.Error errors -> Assert.Fail(String.concat "; " errors)
    | Result.Ok output -> Assert.That(output.Code.Length, Is.GreaterThan(65536))

[<TestCase(-1)>]
[<TestCase(65536)>]
[<TestCase(Int32.MaxValue)>]
let ``unsupported dataspace sizes are rejected`` (size: int) =
    match generateFromIr { portOptions with DataspaceKiB = size } specExample with
    | Result.Error errors -> Assert.That(errors[0], Does.Contain("between 0 and 65535 KiB"))
    | Result.Ok _ -> Assert.Fail("Expected an unsupported dataspace size to be rejected.")

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(65535)>]
let ``valid dataspace sizes are preserved in the QDOS header`` (size: int) =
    match generateFromIr { portOptions with DataspaceKiB = size } specExample with
    | Result.Error errors -> Assert.Fail(String.concat "; " errors)
    | Result.Ok output ->
        Assert.That(output.Dataspace, Is.EqualTo(size * 1024))
        let header = qdosFileHeader output.Dataspace
        let bytes = header[22..25]
        if BitConverter.IsLittleEndian then Array.Reverse bytes
        Assert.That(BitConverter.ToInt32(bytes, 0), Is.EqualTo(size * 1024))

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

        let exitCode = Program.main [| sourcePath; outputPath; "false"; "turbo"; "--no-hir-opt"; "--no-native-regions" |]

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
