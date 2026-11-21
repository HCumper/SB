module SBTests.GeneratedTurboExecutionTests

open System
open System.Diagnostics
open System.IO
open NUnit.Framework
open SBTests.M68kEmulator
open SBTests.TurboExecutionHarness

// These tests exercise the real CLI, preprocessing/parsing/semantics/HIR, both Turbo
// generators, the on-disk QDOS header, and the bundled runtime on the 68000 CPU.
// Expected text is independent of the old Turbo generator and of the SB interpreter.
let private withCompiledProgramMode enabled extraArguments extension (source: string) action =
    let directory = Path.Combine(Path.GetTempPath(), $"turbo-execution-{Guid.NewGuid():N}")
    Directory.CreateDirectory(directory) |> ignore
    try
        let input = Path.Combine(directory, "program" + extension)
        let output = Path.Combine(directory, "test_exe")
        File.WriteAllText(input, source.Trim() + "\n")
        let start = ProcessStartInfo("dotnet")
        start.WorkingDirectory <- directory
        start.UseShellExecute <- false
        start.RedirectStandardOutput <- true
        start.RedirectStandardError <- true
        let mode = if enabled then "--hir-opt" else "--no-hir-opt"
        for argument in [ typeof<TurboCodegen.TurboCodegenOutput>.Assembly.Location; input; output; "false"; "turbo"; mode ] @ extraArguments do
            start.ArgumentList.Add argument
        use compiler = new Process(StartInfo = start)
        Assert.That(compiler.Start(), Is.True, "Unable to start the SB CLI.")
        let stdout = compiler.StandardOutput.ReadToEndAsync()
        let stderr = compiler.StandardError.ReadToEndAsync()
        if not (compiler.WaitForExit 30000) then
            compiler.Kill(true)
            compiler.WaitForExit()
            Assert.Fail("The SB CLI exceeded its 30-second time limit.")
        let diagnostics = stdout.GetAwaiter().GetResult() + stderr.GetAwaiter().GetResult()
        Assert.That(compiler.ExitCode, Is.Zero, $"SB CLI failed: {diagnostics}")
        Assert.That(File.Exists output, Is.True, "The CLI did not write an executable.")
        action (File.ReadAllBytes output)
    finally
        Directory.Delete(directory, true)

let private withCompiledProgram extension source action = withCompiledProgramMode true [] extension source action

let nativeForPrograms = [|
    "10 total%=0\n20 FOR i%=1 TO 10\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "55:10\n"
    "10 total%=0:limit%=-4:stride%=-3\n20 FOR i%=8 TO limit% STEP stride%\n30 limit%=100:stride%=32767:total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "10:-4\n"
    "10 n%=3:i%=9:total%=0\n20 FOR i%=5 TO n%\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 FOR i%=-5 TO -3 STEP -1\n70 total%=total%+i%\n80 END FOR i%\n90 PRINT total%;\":\";i%\n100 STOP", "0:5\n0:-5\n"
    "10 i%=4:total%=0\n20 FOR i%=1 TO i%\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "10:4\n"
    "10 i%=3:total%=0\n20 FOR i%=1 TO 8 STEP i%\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "12:7\n"
    "10 total%=0:stride%=0\n20 FOR i%=3 TO 2 STEP stride%\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "0:3\n"
    "10 total%=0\n20 FOR outer%=1 TO 3\n30 FOR i%=1 TO 5 STEP 2\n40 total%=total%+i%\n50 END FOR i%\n60 END FOR outer%\n70 PRINT total%;\":\";outer%;\":\";i%\n80 STOP", "27:3:5\n"
    "10 total%=0\n20 FOR i%=1 TO 10\n30 total%=total%+i%:IF i%=3 THEN EXIT i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "6:3\n"
    "10 total%=0\n20 FOR i%=1 TO 3\n30 i%=i%+1:total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "6:4\n"
    "10 a%=1:work a%,a%\n20 PRINT a%\n30 STOP\n100 DEFine PROCedure work(p%,q%)\n110 LOCal i%\n120 FOR i%=1 TO 3\n130 p%=p%+i%:q%=q%+1\n140 END FOR i%\n150 END DEFine", "10\n"
|]
let nativeForModes = [|
    for index in 0..nativeForPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Native FOR {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof nativeForModes)>]
let ``native FOR loops preserve captured values empty ranges counter stores and fallbacks`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=nativeForPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

let refinedLoopPrograms = [|
    "10 end%=3:stride%=1:total%=0\n20 FOR i%=1 TO end% STEP stride%\n30 end%=-100:stride%=32767\n40 total%=total%+i%\n50 END FOR i%\n60 PRINT total%\n70 STOP", "6\n"
    "10 DIM a%(100)\n20 m%=16:m%=m%*m%:x%=0\n30 FOR i%=0 TO 100\n40 a%(i%)=x%\n50 IF x%<m% THEN x%=x%+1:ELSE x%=0\n60 END FOR i%\n70 PRINT a%(0);\":\";a%(100);\":\";x%\n80 STOP", "0:100:101\n"
    "10 DIM a(2,3)\n20 FOR i%=0 TO 2\n30 FOR j%=0 TO 3\n40 a(i%,j%)=i%*10+j%+0.5\n50 END FOR j%\n60 END FOR i%\n70 IF a(0,0)=0.5 AND a(2,3)=23.5 THEN PRINT \"float bounds OK\":ELSE PRINT \"wrong\"\n80 STOP", "float bounds OK\n"
    "10 DIM a%(63)\n20 FOR last%=63 TO 1 STEP -1\n30 FOR i%=0 TO last%-1\n40 a%(i%+1)=i%+1\n50 END FOR i%\n60 END FOR last%\n70 PRINT a%(1);\":\";a%(63)\n80 STOP", "1:63\n"
|]

let refinedLoopModes = [|
    for index in 0..refinedLoopPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Refined loop {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof refinedLoopModes)>]
let ``refined loops preserve captured values integer arithmetic and numeric arrays`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=refinedLoopPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

let refinedFailurePrograms = [|
    "10 DIM a%(10)\n20 FOR i%=0 TO 10\n30 IF i%=5 THEN DIM a%(1)\n40 a%(i%)=i%\n50 END FOR i%\n60 PRINT \"wrong\"\n70 STOP"
    "10 DIM a%(10)\n20 n%=11\n30 FOR i%=0 TO n%\n40 a%(i%)=i%\n50 END FOR i%\n60 PRINT \"wrong\"\n70 STOP"
    "10 x%=0\n20 FOR i%=1 TO 1000\n30 x%=x%+100\n40 END FOR i%\n50 PRINT \"wrong\"\n60 STOP"
    "10 DIM a%(10)\n20 result%=shrink%()+a%(5)\n30 PRINT \"wrong\"\n40 STOP\n100 DEFine FuNction shrink%\n110 DIM a%(1)\n120 RETurn 0\n130 END DEFine"
|]
let refinedFailures = [|
    for index in 0..refinedFailurePrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Refined loop failure {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof refinedFailures)>]
let ``refinement retains failing arithmetic and bounds checks without an error handler`` (index:int,hir:bool,native:bool,registers:bool) =
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" refinedFailurePrograms[index] (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

[<TestCase(false)>]
[<TestCase(true)>]
let ``loop proofs remove emitted array checks and improve execution`` (hir:bool) =
    let source="10 DIM a%(100)\n20 FOR i%=0 TO 100\n30 a%(i%)=i%*2\n40 END FOR i%\n50 PRINT a%(100)\n60 STOP"
    let capture native = withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");"--regalloc"] ".bas" source (fun executable -> executable.Length,run 2000000L executable)
    let _,before=capture false
    let size,after=capture true
    Assert.That(after.Output, Is.EqualTo("200\n"))
    Assert.That(after.Output, Is.EqualTo(before.Output))
    Assert.That(after.Steps, Is.LessThan(before.Steps))
    TestContext.WriteLine($"Proven array loop: {before.Steps}->{after.Steps} instructions; {size} bytes.")

let compactArrayPrograms = [|
    "10 DIM a%(2,3)\n20 a%(0,0)=11:a%(2,3)=23:a%(1,2)=-7\n30 i%=2:j%=3\n40 a%(i%,j%)=a%(i%,j%)+1\n50 PRINT a%(0,0);\":\";a%(2,3);\":\";a%(1,2)\n60 STOP", "11:24:-7\n"
    "10 DIM a(2,3)\n20 a(0,0)=1.25:a(2,3)=-2.5\n30 i%=2:j%=3:v=a(i%,j%)\n40 a(i%,j%)=v+4\n50 IF a(0,0)=1.25 AND a(2,3)=1.5 THEN PRINT \"float OK\":ELSE PRINT \"wrong\"\n60 STOP", "float OK\n"
    "10 DIM a%(3)\n20 touch a%,a%\n30 PRINT a%(1);\":\";a%(2)\n40 STOP\n100 DEFine PROCedure touch(p%,q%)\n110 p%(1)=5:q%(1)=q%(1)+2:p%(2)=q%(1)\n120 END DEFine", "7:7\n"
    "10 DIM a%(2)\n20 a%(1)=7\n30 work\n40 PRINT a%(1)\n50 STOP\n100 DEFine PROCedure work\n110 LOCal a%(3)\n120 a%(1)=11:a%(3)=13\n130 PRINT a%(1);\":\";a%(3)\n140 END DEFine", "11:13\n7\n"
    "10 n%=2\n20 DIM a%(n%),b%(n%)\n30 a%(2)=5:b%(2)=17\n40 n%=4\n50 DIM a%(n%)\n60 a%(4)=9\n70 PRINT a%(4);\":\";b%(2)\n80 STOP", "9:17\n"
    "10 DIM a(2,2)\n20 fill a,a\n30 IF a(0,0)=1.25 AND a(2,2)=-2.5 THEN PRINT \"reference OK\":ELSE PRINT \"wrong\"\n40 STOP\n100 DEFine PROCedure fill(p,q)\n110 p(0,0)=1.25:q(2,2)=-2.5\n120 END DEFine", "reference OK\n"
    "10 WHEN ERROR\n20 count%=count%+1\n30 IF count%<3 THEN RETRY\n40 PRINT ERNUM;\":\";count%;\":\";a%(1)\n50 STOP\n60 END WHEN\n70 DIM a%(2)\n80 a%(1)=7\n90 result%=bad%(2)\n95 PRINT \"wrong\"\n100 DEFine FuNction bad%(n%)\n110 LOCal a%(1)\n120 a%(0)=8:a%(1)=9\n130 RETurn a%(n%)\n140 END DEFine", "-4:3:7\n"
|]

let compactArrayModes = [|
    for index in 0..compactArrayPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Compact numeric arrays {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof compactArrayModes)>]
let ``compact numeric arrays preserve values binding changes resizing and retries`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=compactArrayPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

let compactArrayFailures = [|
    for opcode in [1111;1112;1143;1144] do
        for first,last in [-1,0;0,2;2,-1] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(opcode,first,last,native,registers).SetName($"Checked array failure {opcode} ({first},{last}), native {native}, registers {registers}") |]

[<TestCaseSource(nameof compactArrayFailures)>]
let ``checked array range errors keep the error number and leave all elements unchanged`` (opcode:int,first:int,last:int,native:bool,registers:bool) =
    let name=if opcode=1111 || opcode=1112 then "a%" else "a"
    let access=if opcode=1112 || opcode=1144 then $"{name}(i%%,j%%)=99" else $"result={name}(i%%,j%%)"
    let source = $"10 WHEN ERROR\n20 IF {name}(0,0)=7 AND {name}(1,1)=8 THEN PRINT ERNUM;\":kept\":ELSE PRINT \"changed\"\n30 STOP\n40 END WHEN\n50 DIM {name}(1,1)\n60 {name}(0,0)=7:{name}(1,1)=8\n70 i%%={first}:j%%={last}\n80 {access}\n90 PRINT \"wrong\""
    withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo("-4:kept\n"))
        Assert.That(result.ExitCode, Is.Zero))

[<TestCase(false)>]
[<TestCase(true)>]
let ``allocated integer spills are released across checked array stores and loads`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"array-register-boundary-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"arrays")
        let source="10 DIM cells%(2)\n20 a%=1:b%=2:c%=3:d%=4:e%=5:f%=6:g%=7:h%=8\n30 cells%(1)=a%+(b%+(c%+(d%+(e%+(f%+(g%+h%))))))\n40 result%=cells%(1)+(a%+(b%+(c%+(d%+(e%+(f%+(g%+h%)))))))\n50 PRINT result%\n60 STOP"
        withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable ->
            let result=run 2000000L executable
            Assert.That(result.Output, Is.EqualTo("72\n")))
        Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Match("spill slots=[1-9]"))
    finally Directory.Delete(directory,true)

let private samples = [|
    TestCaseData(".bas", "10 PRINT \"Hello from Turbo\"\n20 STOP", "Hello from Turbo\n")
        .SetName("Turbo executable starts, prints, and stops")
    TestCaseData(".ssb", """
total% = 0
FOR i% = 1 TO 5
  total% = total% + i% * i%
END FOR i%
PRINT total%
STOP
""", "55\n").SetName("Turbo executable runs Structured SuperBASIC preprocessing and loops")
    TestCaseData(".bas", """
10 GO TO 60
15 a% = 0
20 IF a% = 0 THEN
30 GO TO 100
40 END IF
50 PRINT "wrong" : STOP
60 GO TO 30
70 GO TO 50
100 PRINT "right" : STOP
""", "right\n").SetName("Turbo executable preserves an externally reached labelled jump")
    TestCaseData(".bas", """
10 a% = -7 : b% = 3
20 PRINT a% + b%
30 PRINT a% * b%
40 PRINT a% DIV b%
50 STOP
""", "-4\n-21\n-3\n").SetName("Turbo executable computes signed integer arithmetic")
    TestCaseData(".bas", """
10 v = 2.5
20 IF -1 + v = 1.5 THEN PRINT "fraction OK" : ELSE PRINT "wrong fraction"
30 IF -32768 + v = -32765.5 THEN PRINT "negative OK" : ELSE PRINT "wrong negative"
40 STOP
""", "fraction OK\nnegative OK\n").SetName("Turbo executable computes negative mixed numeric expressions")
    TestCaseData(".bas", """
10 DIM a%(3)
20 FOR i% = 0 TO 3
30 a%(i%) = i% * 2
40 END FOR i%
50 PRINT a%(0); ":"; a%(3)
60 STOP
""", "0:6\n").SetName("Turbo executable allocates and indexes arrays")
    TestCaseData(".bas", """
10 a% = 1
20 bump a%
30 bump (a%)
40 PRINT a%
50 STOP
100 DEFine PROCedure bump(n%)
110 n% = n% + 1
120 END DEFine
""", "2\n").SetName("Turbo executable distinguishes reference and value arguments")
    TestCaseData(".bas", """
10 PRINT f%(0)
20 PRINT g(0)
30 PRINT "["; s$(0); "]"
40 STOP
100 DEFine FuNction f%(x%)
110 IF x% > 0 THEN RETurn 7
120 END DEFine
200 DEFine FuNction g(x%)
210 IF x% > 0 THEN RETurn 7
220 END DEFine
300 DEFine FuNction s$(x%)
310 IF x% > 0 THEN RETurn "seven"
320 END DEFine
""", "0\n0\n[0]\n").SetName("Turbo executable returns typed zero on function fallthrough")
    TestCaseData(".bas", """
10 MANIFEST : n = 3
20 PRINT f(7)
30 STOP
100 DEFine FuNction f(n)
110 RETurn n
120 END DEFine
""", "7\n").SetName("Turbo executable resolves a parameter shadowing MANIFEST")
    TestCaseData(".bas", """
10 PRINT count%(3)
20 STOP
100 DEFine FuNction count%(n%)
110 IF n% = 0 THEN RETurn 0
120 n% = n% - 1
130 RETurn 1 + count%(n%)
140 END DEFine
""", "3\n").SetName("Turbo executable supports recursive flexible calls")
    TestCaseData(".bas", """
10 show
20 PRINT "done"
30 STOP
100 DEFine PROCedure show
110 GO TO 130
120 PRINT "wrong"
130 END DEFine
""", "done\n").SetName("Turbo executable returns from a jump to END DEFine")
    TestCaseData(".bas", """
10 PRINT (2 + 3) * 4
20 PRINT -7 DIV 3
30 PRINT -7 MOD 3
40 IF 2 < 3 THEN PRINT "selected" : ELSE PRINT "wrong"
50 PRINT "joined" & " text"
60 STOP
""", "20\n-3\n2\nselected\njoined text\n").SetName("Turbo executable preserves folded arithmetic and constant branches")
    TestCaseData(".bas", """
10 a% = 1
20 bump (a% + 0)
30 PRINT a%
40 bump a%
50 PRINT a%
60 STOP
100 DEFine PROCedure bump(n%)
110 n% = n% + 1
120 END DEFine
""", "1\n2\n").SetName("Turbo executable keeps simplified expressions passed by value")
    TestCaseData(".bas", """
10 WHEN ERROR
20 PRINT "caught"
30 STOP
40 END WHEN
50 PRINT 32767 + 1
60 PRINT "wrong"
""", "caught\n").SetName("Turbo executable preserves checked overflow and WHEN ERROR")
    TestCaseData(".bas", """
10 WHEN ERROR
20 PRINT "caught"
30 STOP
40 END WHEN
50 PRINT 1 DIV 0
60 PRINT "wrong"
""", "caught\n").SetName("Turbo executable preserves division failure and WHEN ERROR")
    TestCaseData(".bas", """
10 GO TO 40
20 IF 0 THEN
30 PRINT "wrong"
40 PRINT "entered"
50 END IF
60 STOP
""", "entered\n").SetName("Turbo executable preserves entry into a constant-false branch")
    TestCaseData(".bas", """
10 FOR i%=1 TO 100
20 a%=i%*3:b%=a%+7:c%=b%-2:d%=-c%
30 END FOR i%
40 PRINT a%;":";b%;":";c%;":";d%
50 STOP
""", "300:307:305:-305\n").SetName("Turbo executable uses bounded integer arithmetic in native loop regions")
    TestCaseData(".bas", """
10 x%=1
20 setter (x%)
30 PRINT x%+1
40 setter x%
50 PRINT x%
60 STOP
100 DEFine PROCedure setter(p%)
110 p%=32767
120 END DEFine
""", "2\n32767\n").SetName("Turbo executable distinguishes value and reference effects on range facts")
|]

let programs =
    [| for sample in samples do
           for enabled in [ false; true ] do
               for native in [ false; true ] do
                   for registers in [false;true] do
                       yield TestCaseData(Array.append sample.Arguments [| box enabled; box native; box registers |]).SetName($"{sample.TestName} (HIR {enabled}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof programs)>]
let ``compiled Turbo program produces its expected output`` (extension: string, source: string, expected: string, enabled: bool, native: bool, registers:bool) =
    let flag = if native then "--native-regions" else "--no-native-regions"
    let allocation=if registers then "--regalloc" else "--no-regalloc"
    withCompiledProgramMode enabled [flag;allocation] extension source (fun executable ->
        let result = run 2000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output.Replace("\r\n", "\n"), Is.EqualTo(expected))
        Assert.That(result.Steps, Is.GreaterThan(0L)))

[<TestCase(false)>]
[<TestCase(true)>]
let ``local allocation improves native copy chains`` (hir:bool) =
    // REPeat isolates leaf allocation from the independent native FOR plan.
    let source="10 i%=0\n20 REPeat copyloop\n30 i%=i%+1:a%=i%:b%=a%:c%=b%:d%=c%\n40 IF i%=100 THEN EXIT copyloop\n50 END REPeat copyloop\n60 PRINT d%\n70 STOP"
    let capture registers = withCompiledProgramMode hir ["--native-regions";(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable -> executable.Length,run 2000000L executable)
    let beforeSize,before=capture false
    let afterSize,after=capture true
    Assert.That(after.Output, Is.EqualTo("100\n"))
    Assert.That(after.Output, Is.EqualTo(before.Output))
    Assert.That(after.Steps, Is.LessThan(before.Steps))
    Assert.That(afterSize, Is.LessThan(beforeSize))
    TestContext.WriteLine($"Local allocation: bytes {beforeSize}->{afterSize}; native instructions {before.Steps}->{after.Steps}.")

[<TestCase(false)>]
[<TestCase(true)>]
let ``allocated pressure frames work through startup and runtime calls`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"native-register-pressure-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let source="10 a%=1:b%=2:c%=3:d%=4:e%=5:f%=6:g%=7:h%=8\n20 result%=a%+(b%+(c%+(d%+(e%+(f%+(g%+h%))))))\n30 PRINT result%\n40 STOP"
        let prefix=Path.Combine(directory,"pressure")
        let capture registers = withCompiledProgramMode hir ["--native-regions";(if registers then "--regalloc" else "--no-regalloc");"--dump-hir="+prefix] ".bas" source (fun executable -> executable,run 2000000L executable)
        let before,beforeRun=capture false
        let after,afterRun=capture true
        Assert.That(afterRun.Output, Is.EqualTo("36\n"))
        Assert.That(afterRun.Output, Is.EqualTo(beforeRun.Output))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain("virtual:"))
        Assert.That(report, Does.Match("spill slots=[1-9]"))
        let dataspace (bytes:byte[]) = bytes[22..25] |> Array.fold (fun value b -> (value <<< 8) ||| int b) 0
        Assert.That(dataspace after, Is.GreaterThan(dataspace before))
    finally Directory.Delete(directory,true)

[<TestCase(false)>]
[<TestCase(true)>]
let ``allocated native integer and opaque maths templates exchange stack values`` (hir:bool) =
    let source="10 a%=10:b%=3\n20 c%=a%-b%:d%=c% DIV 2:e%=d%+1\n30 PRINT c%;\":\";d%;\":\";e%\n40 STOP"
    withCompiledProgramMode hir ["--native-regions";"--regalloc"] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo("7:3:4\n")))

[<TestCase(false)>]
[<TestCase(true)>]
let ``native scalar regions reduce executed instructions in a loop`` (hir: bool) =
    let source = "10 FOR i%=1 TO 100\n20 a%=i%:b%=a%:c%=b%:d%=c%\n30 END FOR i%\n40 PRINT d%\n50 STOP"
    let capture native =
        let flag = if native then "--native-regions" else "--no-native-regions"
        withCompiledProgramMode hir [flag] ".bas" source (fun executable -> executable.Length, run 2000000L executable)
    let beforeSize, before = capture false
    let afterSize, after = capture true
    Assert.That(after.Output, Is.EqualTo("100\n"))
    Assert.That(after.Output, Is.EqualTo(before.Output))
    Assert.That(after.ExitCode, Is.Zero)
    Assert.That(after.Steps, Is.LessThan(before.Steps))
    TestContext.WriteLine($"Audited native scalar loop: executable bytes {beforeSize} -> {afterSize}; instructions {before.Steps} -> {after.Steps}.")

[<TestCase(false)>]
[<TestCase(true)>]
let ``range proven native arithmetic reduces executed instructions`` (hir:bool) =
    let source="10 FOR i%=1 TO 100\n20 a%=i%*3:b%=a%+7:c%=b%-2:d%=-c%\n30 END FOR i%\n40 PRINT d%\n50 STOP"
    let capture native =
        withCompiledProgramMode hir [if native then "--native-regions" else "--no-native-regions"] ".bas" source (fun executable -> executable.Length,run 2000000L executable)
    let beforeSize,before=capture false
    let afterSize,after=capture true
    Assert.That(after.Output, Is.EqualTo("-305\n"))
    Assert.That(after.Output, Is.EqualTo(before.Output))
    Assert.That(after.Steps, Is.LessThan(before.Steps))
    TestContext.WriteLine($"Range-proven arithmetic: executable bytes {beforeSize} -> {afterSize}; instructions {before.Steps} -> {after.Steps}.")

let unsafeAnalysisPrograms = [|
    "10 x%=32767\n20 GO TO 50\n30 IF x%<100 THEN\n50 y%=x%+1\n60 END IF\n70 PRINT \"wrong\"\n80 STOP"
    "10 x%=1\n20 setter x%\n30 PRINT x%+1\n40 STOP\n100 DEFine PROCedure setter(p%)\n110 p%=32767\n120 END DEFine"
    "10 x%=1\n20 aliases x%,x%\n30 STOP\n100 DEFine PROCedure aliases(p%,q%)\n110 p%=1:q%=32767\n120 PRINT p%+1\n130 END DEFine"
    "10 trips%=0\n20 FOR i%=32767 TO 32767\n30 trips%=trips%+1\n40 x%=i%-1\n50 IF trips%>=2 THEN EXIT i%\n60 END FOR i%\n70 PRINT \"wrong\"\n80 STOP"
|]

let unsafeAnalysisCases = [|
    for index in 0..unsafeAnalysisPrograms.Length-1 do
        for hir in [false;true] do
            for native in [false;true] do
                yield TestCaseData(index,hir,native).SetName($"Analysis preserves overflow case {index} (HIR {hir}, native {native})") |]

[<TestCaseSource(nameof unsafeAnalysisCases)>]
let ``range proofs preserve failures after jumps aliases calls and FOR wrap`` (index:int,hir:bool,native:bool) =
    let flag=if native then "--native-regions" else "--no-native-regions"
    withCompiledProgramMode hir [flag] ".bas" unsafeAnalysisPrograms[index] (fun executable ->
        Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore) |> ignore)

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``native checked division and modulo preserve signed results`` (hir: bool, native: bool) =
    let source = "10 a%=-7:b%=3\n20 c%=a% DIV b%:d%=a% MOD b%\n30 PRINT c%;\":\";d%\n40 a%=7:b%=-3\n50 c%=a% DIV b%:d%=a% MOD b%\n60 PRINT c%;\":\";d%\n70 STOP"
    let flag = if native then "--native-regions" else "--no-native-regions"
    withCompiledProgramMode hir [flag] ".bas" source (fun executable ->
        let result = run 2000000L executable
        Assert.That(result.Output, Is.EqualTo("-3:2\n-3:-2\n"))
        Assert.That(result.ExitCode, Is.Zero))

let failures =
    [| for expression in ["a%+b%";"a%-(-b%)";"a%*b%";"-a%";"a% DIV b%";"a% MOD b%";"a% DIV 0";"a% MOD 0"] do
           for hir in [false;true] do
               for native in [false;true] do
                   for registers in [false;true] do
                       yield TestCaseData(expression,hir,native,registers).SetName($"Checked failure {expression} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof failures)>]
let ``mixed native and threaded regions retain error handlers`` (expression: string, hir: bool, native: bool,registers:bool) =
    let a,b =
        if expression="-a%" then -32768,1
        elif expression.Contains("DIV") || expression.Contains("MOD") then -32768,-1
        elif expression.Contains("*") then 16384,2
        else 32767,1
    let source = $"10 WHEN ERROR\n20 PRINT \"caught\"\n30 STOP\n40 END WHEN\n50 a%%={a}:b%%={b}\n60 c%%={expression}\n70 PRINT \"wrong\"\n80 STOP"
    let flag = if native then "--native-regions" else "--no-native-regions"
    withCompiledProgramMode hir [flag;(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result = run 2000000L executable
        Assert.That(result.Output, Is.EqualTo("caught\n"))
        Assert.That(result.ExitCode, Is.Zero))

let checkedRecoveryCases = [|
    // Explicit checked increments must preserve the original improver's
    // store-before-trap behavior, visible to a handler (and through aliases).
    "10 WHEN ERROR\n20 PRINT ERNUM;\":\";a%\n30 STOP\n40 END WHEN\n50 a%=32767:b%=1:c%=2\n60 a%=a%+1\n70 PRINT \"wrong\"", "-18:-32768\n"
    "10 WHEN ERROR\n20 PRINT ERNUM;\":\";a%\n30 STOP\n40 END WHEN\n50 a%=-32768:b%=1:c%=2\n60 a%=a%-1\n70 PRINT \"wrong\"", "-18:32767\n"
    "10 WHEN ERROR\n20 PRINT ERNUM;\":\";x%\n30 STOP\n40 END WHEN\n50 x%=32767\n60 bump x%,x%\n70 PRINT \"wrong\"\n100 DEFine PROCedure bump(p%,q%)\n110 p%=p%+1:q%=2\n120 END DEFine", "-18:-32768\n"
    // A distinct assignment is not committed on overflow.
    "10 WHEN ERROR\n20 PRINT ERNUM;\":\";c%\n30 STOP\n40 END WHEN\n50 a%=32767:b%=1:c%=42\n60 c%=a%+b%\n70 PRINT \"wrong\"", "-18:42\n"
    // Handler arithmetic, maths-stack reset and repeated RETRY must work after
    // a large expression spills, including unwinding a compiled function.
    "10 WHEN ERROR\n20 count%=count%+1\n30 IF count%<3 THEN RETRY\n40 PRINT ERNUM;\":\";count%\n50 STOP\n60 END WHEN\n70 a%=1:b%=2:c%=3:d%=4:e%=5:f%=6:g%=32767:h%=1\n80 result%=a%+(b%+(c%+(d%+(e%+(f%+(g%+h%))))))\n90 PRINT \"wrong\"", "-18:3\n"
    "10 WHEN ERROR\n20 count%=count%+1\n30 IF count%<3 THEN RETRY\n40 PRINT ERNUM;\":\";count%\n50 STOP\n60 END WHEN\n70 answer%=bad%(32767)\n80 PRINT \"wrong\"\n100 DEFine FuNction bad%(n%)\n110 LOCal a%,b%,c%,d%,e%,f%,g%\n120 a%=1:b%=2:c%=3:d%=4:e%=5:f%=6:g%=1\n130 RETurn a%+(b%+(c%+(d%+(e%+(f%+(g%+n%))))))\n140 END DEFine", "-18:3\n"
|]

let checkedRecoveryModes = [|
    for index in 0..checkedRecoveryCases.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Checked arithmetic recovery {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof checkedRecoveryModes)>]
let ``checked arithmetic retains observable stores aliases retries and local unwinding`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=checkedRecoveryCases[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

[<TestCase(false,false,false)>]
[<TestCase(true,false,false)>]
[<TestCase(true,true,false)>]
[<TestCase(false,false,true)>]
[<TestCase(true,false,true)>]
[<TestCase(true,true,true)>]
let ``checked native overflow CONTINUE retains fatal error propagation`` (native:bool,registers:bool,implicitContinue:bool) =
    let handler=if implicitContinue then "20 PRINT ERNUM" else "20 PRINT ERNUM:CONTINUE"
    let source = $"10 WHEN ERROR\n{handler}\n30 END WHEN\n40 a%%=32767:b%%=1:c%%=3:d%%=4\n50 c%%=d%%+(c%%+(a%%+b%%))\n60 PRINT \"wrong\""
    withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:"))
        Assert.That(error.Message, Does.Contain("output=-18\n")))

[<Test>]
let ``a selected allocated overflow exit releases a real private frame before RETRY`` () =
    let directory=Path.Combine(Path.GetTempPath(),$"checked-register-recovery-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"checked")
        let source,expected=checkedRecoveryCases[4]
        withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable ->
            let result=run 2000000L executable
            Assert.That(result.Output, Is.EqualTo(expected)))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        let checkedFrame=report.Split("Block IR bytes") |> Array.exists(fun block ->
            block.Contains("virtual: OverflowCheck") && System.Text.RegularExpressions.Regex.IsMatch(block,"spill slots=[1-9]"))
        Assert.That(checkedFrame, Is.True,report)
    finally Directory.Delete(directory,true)

[<Test>]
let ``HIR optimization reduces executable bytes and executed instructions`` () =
    let source = "10 a% = 7\n20 PRINT (2 + 3) * 4 + (a% + 0)\n30 STOP"
    let capture enabled =
        withCompiledProgramMode enabled [] ".bas" source (fun executable -> executable.Length, run 2000000L executable)
    let beforeSize, before = capture false
    let afterSize, after = capture true
    Assert.That(after.Output, Is.EqualTo("27\n"))
    Assert.That(after.Output, Is.EqualTo(before.Output))
    Assert.That(afterSize, Is.LessThan(beforeSize))
    Assert.That(after.Steps, Is.LessThan(before.Steps))
    TestContext.WriteLine($"HIR optimization example: executable bytes {beforeSize} -> {afterSize}; emulated instructions {before.Steps} -> {after.Steps}.")

[<Test>]
let ``Turbo CLI writes original and optimized HIR dumps`` () =
    let directory = Path.Combine(Path.GetTempPath(), $"turbo-hir-dumps-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for enabled in [ false; true ] do
            let prefix = Path.Combine(directory, if enabled then "enabled" else "disabled")
            withCompiledProgramMode enabled [ "--dump-hir=" + prefix ] ".bas" "10 PRINT (2+3)*4\n20 STOP" (fun _ -> ())
            let before = File.ReadAllText(prefix + ".before.hir")
            let after = File.ReadAllText(prefix + ".after.hir")
            let analysis = File.ReadAllText(prefix + ".analysis.txt")
            Assert.That(analysis, Does.Contain("HIR control-flow, range, effect and alias analysis"))
            if enabled then Assert.That(after, Is.Not.EqualTo(before))
            else Assert.That(after, Is.EqualTo(before))
    finally Directory.Delete(directory, true)

[<Test>]
let ``the execution harness fails instead of hanging on an infinite program`` () =
    withCompiledProgram ".bas" "10 REPeat spin\n20 END REPeat spin" (fun executable ->
        let error = Assert.Throws<M68kFault>(fun () -> run 10000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("step limit 10000")))

[<Test>]
let ``the execution harness rejects unsupported QDOS calls`` () =
    withCompiledProgram ".bas" "10 STOP" (fun executable ->
        let broken = Array.copy executable
        // MOVEQ #$7F,D0; TRAP #3 at the executable entry: an unsupported QDOS operation.
        Array.blit [| 0x70uy; 0x7Fuy; 0x4Euy; 0x43uy |] 0 broken 30 4
        let error = Assert.Throws<M68kFault>(fun () -> run 10000L broken |> ignore)
        Assert.That(error.Message, Does.Contain("Unsupported TRAP #3, D0=$7F")))

[<Test>]
let ``scheduler status changes preserve the CPU condition codes`` () =
    let memory = Array.zeroCreate<byte> 64
    // ORI.W #$2700,SR; ANDI.W #$D8FF,SR; MOVE SR,D0.
    Array.blit [| 0x00uy; 0x7Cuy; 0x27uy; 0uy; 0x02uy; 0x7Cuy; 0xD8uy; 0xFFuy; 0x40uy; 0xC0uy |] 0 memory 0 10
    let cpu = Cpu(memory)
    cpu.Ccr <- 0x15u
    cpu.Step()
    Assert.That(cpu.StatusRegister, Is.EqualTo(0x2715u))
    cpu.Step()
    Assert.That(cpu.StatusRegister, Is.EqualTo(0x15u))
    cpu.Step()
    Assert.That(cpu.D[0], Is.EqualTo(0x15u))
