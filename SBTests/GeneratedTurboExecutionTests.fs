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

let recursiveBoundaryPrograms = [|
    "10 total%=0:work\n20 PRINT total%\n30 STOP\n100 DEFine PROCedure work\n110 LOCal i%\n120 FOR i%=1 TO 3\n130 collect (i%),(i%+1),(i%+2)\n140 END FOR i%\n150 END DEFine\n200 DEFine PROCedure collect(a%,b%,c%)\n210 total%=total%+a%*100+b%*10+c%\n220 END DEFine", "702\n", 1
    "10 value%=0:total%=0:bump value%:work\n20 PRINT value%;\":\";total%\n30 STOP\n100 DEFine PROCedure bump(x%)\n110 x%=x%+1:total%=total%+x%\n120 END DEFine\n200 DEFine PROCedure work\n210 LOCal i%\n220 FOR i%=1 TO 3\n230 bump (i%)\n240 END FOR i%\n250 END DEFine", "1:10\n", 1
    "10 i%=77:walk (3):walk (2)\n20 PRINT total%;\":\";leaves%;\":\";i%\n30 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%,up%,down%\n120 IF depth%=0 THEN leaves%=leaves%+1:RETurn\n130 FOR i%=1 TO 2\n140 up%=depth%*10+i%:down%=depth%*10-i%\n150 walk (depth%-1)\n160 IF up%<>depth%*10+i% THEN total%=-30000\n170 IF down%<>depth%*10-i% THEN total%=-30000\n180 total%=total%+up%\n190 END FOR i%\n200 END DEFine", "330:12:77\n", 1
    "10 first (3)\n20 PRINT total%;\":\";leaves%\n30 STOP\n100 DEFine PROCedure first(depth%)\n110 LOCal i%,saved%\n120 IF depth%=0 THEN leaves%=leaves%+1:RETurn\n130 FOR i%=1 TO 2\n140 saved%=depth%*10+i%\n150 second (depth%-1)\n160 total%=total%+saved%\n170 END FOR i%\n180 END DEFine\n200 DEFine PROCedure second(depth%)\n210 LOCal i%,saved%\n220 IF depth%=0 THEN leaves%=leaves%+1:RETurn\n230 FOR i%=1 TO 2\n240 saved%=depth%*10+i%\n250 first (depth%-1)\n260 total%=total%+saved%\n270 END FOR i%\n280 END DEFine", "241:8\n", 2
    "10 DIM shared%(0):walk (2)\n20 PRINT total%;\":\";shared%(0)\n30 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%\n120 IF depth%=0 THEN shared%(0)=shared%(0)+1:RETurn\n130 FOR i%=1 TO 2\n140 previous%=shared%(0)\n150 walk (depth%-1)\n160 total%=total%+shared%(0)\n170 END FOR i%\n180 END DEFine", "16:4\n", 1
    "10 last%=3:stride%=1:work\n20 PRINT total%;\":\";last%;\":\";stride%\n30 STOP\n100 DEFine PROCedure work\n110 LOCal i%\n120 FOR i%=1 TO last% STEP stride%\n130 change (0)\n140 total%=total%+i%\n150 END FOR i%\n160 END DEFine\n200 DEFine PROCedure change(unused%)\n210 last%=-100:stride%=-1\n220 END DEFine", "6:-100:-1\n", 1
    "10 FOR i%=1 TO 5\n20 change (0)\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP\n100 DEFine PROCedure change(unused%)\n110 i%=i%+2\n120 END DEFine", "9:6\n", 1
    "10 work (3),(1),(1):work (1),(3),(-1):work (1),(3),(1)\n20 PRINT total%;\":\";calls%\n30 STOP\n100 DEFine PROCedure work(first%,last%,step%)\n110 LOCal i%\n120 FOR i%=first% TO last% STEP step%\n130 tick\n140 total%=total%+i%\n150 END FOR i%\n160 END DEFine\n200 DEFine PROCedure tick\n210 calls%=calls%+1\n220 END DEFine", "6:3\n", 1
    "10 walk (3)\n20 PRINT total%;\":\";leaves%\n30 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%,text$(8)\n120 text$=\"d\" & depth%\n130 IF depth%=0 THEN leaves%=leaves%+1:RETurn\n140 FOR i%=1 TO 2\n150 walk (depth%-1)\n160 total%=total%+CODE(text$(2))\n170 END FOR i%\n180 END DEFine", "694:8\n", 1
    "10 total%=0:i%=0:work\n20 PRINT total%;\":\";i%\n30 STOP\n100 DEFine PROCedure work\n110 LOCal j%\n120 FOR j%=1 TO 3\n130 bump total%\n140 END FOR j%\n150 END DEFine\n200 DEFine PROCedure bump(value%)\n210 value%=value%+1\n220 END DEFine", "3:0\n", 0
    "10 WHEN ERROR\n20 PRINT ERNUM:STOP\n30 END WHEN\n40 walk (3)\n50 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%\n120 IF depth%=0 THEN answer%=1 DIV zero%:RETurn\n130 FOR i%=1 TO 2\n140 walk (depth%-1)\n150 END FOR i%\n160 END DEFine", "-18\n", 0
|]
let recursiveBoundaryModes = [|
    for index in 0..recursiveBoundaryPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Recursive boundary {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof recursiveBoundaryModes)>]
let ``recursive native boundaries preserve activation bindings captured loops and shared writes`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected,_=recursiveBoundaryPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 6000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<TestCase(false)>]
[<TestCase(true)>]
let ``recursive native diagnostics confirm selected and rejected call boundaries`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"recursive-boundary-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..recursiveBoundaryPrograms.Length-1 do
            let source,_,count=recursiveBoundaryPrograms[index]
            let prefix=Path.Combine(directory,string index)
            withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Contain($"Memory-counter native procedure call boundaries: {count}"), $"fixture {index}")
    finally Directory.Delete(directory,true)

let recursiveBoundaryErrors = [|
    "10 DIM a%(1):work\n20 STOP\n100 DEFine PROCedure work\n110 LOCal i%\n120 FOR i%=1 TO 2\n130 resize (i%)\n140 a%(2)=i%\n150 END FOR i%\n160 END DEFine\n200 DEFine PROCedure resize(pass%)\n210 IF pass%=1 THEN DIM a%(3):ELSE DIM a%(0)\n220 END DEFine"
    "10 walk (3)\n20 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%\n120 IF depth%=0 THEN answer%=1 DIV zero%:RETurn\n130 FOR i%=1 TO 2\n140 walk (depth%-1)\n150 END FOR i%\n160 END DEFine"
    "10 walk (3)\n20 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%\n120 FOR i%=1 TO 2\n130 IF depth%=0 THEN answer%=32767+i%\n140 walk (depth%-1)\n150 END FOR i%\n160 END DEFine"
    "10 DIM a%(1):index%=3:walk (3)\n20 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%\n120 IF depth%=0 THEN answer%=a%(index%):RETurn\n130 FOR i%=1 TO 2\n140 walk (depth%-1)\n150 END FOR i%\n160 END DEFine"
    "10 walk (1000)\n20 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%,space$(128)\n120 IF depth%=0 THEN RETurn\n130 FOR i%=1 TO 1\n140 walk (depth%-1)\n150 END FOR i%\n160 END DEFine"
|]

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
[<TestCase(4)>]
let ``recursive native failures match threaded arithmetic bounds and out-of-memory errors`` (index:int) =
    let capture native =
        let mutable number=0
        withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");"--regalloc"] ".bas" recursiveBoundaryErrors[index] (fun executable ->
            let error=Assert.Throws<M68kFault>(fun () -> run 6000000L executable |> ignore)
            Assert.That(error.Message, Does.Contain("Runtime error message:"))
            let parsed=System.Text.RegularExpressions.Regex.Match(error.Message,@"ERNUM=(-?\d+)")
            Assert.That(parsed.Success, Is.True)
            number<-int parsed.Groups[1].Value
            Assert.That(number, Is.EqualTo([|-4;-18;-18;-4;-3|][index])))
        number
    Assert.That(capture true, Is.EqualTo(capture false))

let memoryCounterPrograms = [|
    "10 DIM a$(8)\n20 FOR i%=1 TO 8\n30 a$=a$ & CHR$(64+i%)\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP", "ABCDEFGH:8\n"
    "10 DIM a$(5)\n20 a$=\"AB\"\n30 FOR i%=1 TO 8\n40 a$=a$ & a$\n50 END FOR i%\n60 PRINT a$;\":\";LEN(a$);\":\";i%\n70 STOP", "ABABAB:6:8\n"
    "10 DIM a$(8)\n20 a$=\"ABCD\"\n30 FOR i%=1 TO 5\n40 a$=a$(2 TO) & a$(1)\n50 END FOR i%\n60 PRINT a$;\":\";i%\n70 STOP", "BCDA:5\n"
    "10 DIM a$(8)\n20 FOR i%=1 TO 4\n30 IF i% MOD 2 THEN a$=a$ & \"a\" : ELSE a$=a$ & \"b\"\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP", "abab:4\n"
    "10 DIM a$(8)\n20 last%=3:stride%=-1\n30 FOR i%=last% TO 1 STEP stride%\n40 last%=-100:stride%=32767:a$=a$ & i%\n50 END FOR i%\n60 PRINT a$;\":\";i%\n70 STOP", "321:1\n"
    "10 DIM a$(8)\n20 a$=\"ok\"\n30 FOR i%=3 TO 1\n40 a$=\"wrong\"\n50 END FOR i%\n60 PRINT a$;\":\";i%\n70 STOP", "ok:3\n"
    "10 DIM a$(8)\n20 FOR i%=1 TO 4\n30 x=i%:y=x:answer%=y:a$=a$ & answer%\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP", "1234:4\n"
    "10 DIM a$(8)\n20 a$=\"abcd\"\n30 FOR i%=1 TO 4\n40 a$(i%)=CHR$(64+i%):sum%=sum%+CODE(a$(i%))\n50 END FOR i%\n60 PRINT a$;\":\";sum%;\":\";i%\n70 STOP", "ABCD:266:4\n"
    "10 work:work\n20 STOP\n100 DEFine PROCedure work\n110 LOCal a$(8),i%\n120 a$=\"\"\n130 FOR i%=1 TO 4\n140 a$=a$ & i%\n150 END FOR i%\n160 PRINT a$;\":\";i%\n170 END DEFine", "1234:4\n1234:4\n"
    "10 DIM a$(1,8)\n20 FOR i%=0 TO 1\n30 a$(i%)=\"ABC\":a$(i%,2)=\"x\"\n40 END FOR i%\n50 PRINT a$(0);\":\";a$(1);\":\";i%\n60 STOP", "AxC:AxC:1\n"
|]
let memoryCounterModes = [|
    for index in 0..memoryCounterPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Memory-counter loop {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof memoryCounterModes)>]
let ``memory-counter native loops preserve strings counters captures and local bindings`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=memoryCounterPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<TestCase(false)>]
[<TestCase(true)>]
let ``memory-counter reports confirm selection for all audited execution fixtures`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"memory-counter-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..memoryCounterPrograms.Length-1 do
            let prefix=Path.Combine(directory,string index)
            let source,_=memoryCounterPrograms[index]
            withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Contain("Audited memory-counter native FOR loops: 1"), $"fixture {index}")
    finally Directory.Delete(directory,true)

let memoryCounterErrors = [|
    "10 DIM a$(4)\n20 last%=2\n30 FOR i%=1 TO last%\n40 a$=\"x\":answer%=32767+i%\n50 END FOR i%\n60 STOP"
    "10 DIM a$(4)\n20 zero%=0\n30 FOR i%=1 TO 2\n40 a$=CHR$(i% DIV zero%)\n50 END FOR i%\n60 STOP"
    "10 DIM a$(4),b%(0)\n20 index%=3\n30 FOR i%=1 TO 2\n40 a$=\"x\":answer%=b%(index%)\n50 END FOR i%\n60 STOP"
    "10 DIM a$(4)\n20 index%=5\n30 FOR i%=1 TO 2\n40 a$=a$(index%)\n50 END FOR i%\n60 STOP"
|]
let memoryCounterErrorModes = [|
    for index in 0..memoryCounterErrors.Length-1 do
        for native in [false;true] do yield TestCaseData(index,native) |]

[<TestCaseSource(nameof memoryCounterErrorModes)>]
let ``memory-counter loops retain arithmetic division bounds and slice failures`` (index:int,native:bool) =
    withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");"--regalloc"] ".bas" memoryCounterErrors[index] (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

// A literal STEP lets the loop's NEXT step inline (1530): ADDQ/SUBQ up to 8,
// ADDI beyond, BLT for a negative STEP. A finished loop leaves the counter at
// its last value, the word add has no range check, and the body's writes to
// the counter (here through a call) are seen, all as next_for_int does.
let inlineNextPrograms = [|
    "10 DIM a$(20)\n20 FOR i%=9 TO 1 STEP -2\n30 a$=a$ & i%\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP", "97531:1\n"
    "10 DIM a$(20)\n20 FOR i%=1 TO 20 STEP 8\n30 a$=a$ & i%\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP", "1917:17\n"
    "10 DIM a$(20)\n20 FOR i%=20 TO 1 STEP -8\n30 a$=a$ & i%\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP", "20124:4\n"
    "10 DIM a$(20)\n20 FOR i%=-300 TO 1000 STEP 500\n30 a$=a$ & i%\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP", "-300200700:700\n"
    "10 DIM a$(20)\n20 FOR i%=1000 TO -1000 STEP -900\n30 a$=a$ & i%\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP", "1000100-800:-800\n"
    "10 DIM a$(8)\n20 FOR i%=32766 TO 32767\n30 a$=a$ & \"x\"\n40 IF i%<0 THEN EXIT i%\n50 END FOR i%\n60 PRINT a$;\":\";i%\n70 STOP", "xxx:-32768\n"
    "10 DIM a$(20)\n20 FOR i%=1 TO 10\n30 a$=a$ & i%:bump\n40 END FOR i%\n50 PRINT a$;\":\";i%\n60 STOP\n100 DEFine PROCedure bump\n110 i%=i%+2\n120 END DEFine", "14710:12\n"
    "10 DIM a$(20)\n20 last%=3\n30 FOR i%=1 TO last%\n40 last%=9:a$=a$ & i%\n50 END FOR i%\n60 PRINT a$;\":\";i%\n70 STOP", "123:3\n"
|]
let inlineNextModes = [|
    for index in 0..inlineNextPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Inline NEXT {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof inlineNextModes)>]
let ``inline memory-counter NEXT matches threaded stepping`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=inlineNextPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<TestCase(false)>]
[<TestCase(true)>]
let ``inline NEXT reports one record for each literal-step fixture`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"inline-next-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..inlineNextPrograms.Length-1 do
            let prefix=Path.Combine(directory,string index)
            let source,_=inlineNextPrograms[index]
            withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            let report=File.ReadAllText(prefix+".native-ir.txt")
            Assert.That(report, Does.Contain("Audited memory-counter native FOR loops: 1"), $"fixture {index}")
            Assert.That(report, Does.Contain("Inline memory-counter NEXT records: 1"), $"fixture {index}")
    finally Directory.Delete(directory,true)

// A computed STEP keeps next_for_int: only an adjacent literal is known.
[<Test>]
let ``a computed STEP keeps the template NEXT`` () =
    let directory=Path.Combine(Path.GetTempPath(),$"inline-next-step-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"step")
        let source,_=memoryCounterPrograms[4]
        withCompiledProgramMode false ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain("Audited memory-counter native FOR loops: 1"))
        Assert.That(report, Does.Contain("Inline memory-counter NEXT records: 0"))
    finally Directory.Delete(directory,true)

// An IF branch of PRINTs and assignments ending in STOP never returns to the
// loop, so it runs threaded inside a native loop. The last two values are the
// report's terminal-tail and native-loop counts: an IF/ELSE keeps its loop
// threaded, since the THEN branch then ends in a jump over the ELSE.
let terminalStopPrograms = [|
    "10 DIM a$(20)\n20 FOR i%=1 TO 10\n30 a$=a$ & i%:x=i%/2\n40 IF i%=4 THEN PRINT \"stop:\";i%;\":\";a$;\":\";x:STOP\n50 END FOR i%\n60 PRINT \"done\"\n70 STOP", "stop:4:1234:2\n", 1, 1
    "10 DIM a$(20)\n20 FOR i%=1 TO 10\n30 a$=a$ & i%:x=i%/2\n40 IF i%=40 THEN PRINT \"stop:\";i%;\":\";a$;\":\";x:STOP\n50 END FOR i%\n60 PRINT \"done:\";a$\n70 STOP", "done:12345678910\n", 1, 1
    "10 FOR i%=1 TO 10\n20 x=i%/2:y=y+x\n30 IF x>3 THEN\n40 failed%=i%\n50 PRINT \"failed\";failed%;\":\";y\n60 STOP\n70 END IF\n80 END FOR i%\n90 PRINT \"done\"\n100 STOP", "failed7:14\n", 1, 1
    "10 FOR i%=1 TO 10\n20 x=i%/2\n30 IF x>3 THEN PRINT \"big\";i%:STOP:ELSE y=y+x\n40 END FOR i%\n50 PRINT y\n60 STOP", "big7\n", 0, 0
|]
let terminalStopModes = [|
    for index in 0..terminalStopPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Terminal STOP {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof terminalStopModes)>]
let ``terminal STOP branches in native loops match threaded output`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected,_,_=terminalStopPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<TestCase(false)>]
[<TestCase(true)>]
let ``terminal STOP reports count each threaded tail`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"terminal-stop-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..terminalStopPrograms.Length-1 do
            let prefix=Path.Combine(directory,string index)
            let source,_,tails,loops=terminalStopPrograms[index]
            withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            let report=File.ReadAllText(prefix+".native-ir.txt")
            Assert.That(report, Does.Contain($"Audited memory-counter native FOR loops: {loops}"), $"fixture {index}")
            Assert.That(report, Does.Contain($"Terminal STOP tails in native loops: {tails}"), $"fixture {index}")
    finally Directory.Delete(directory,true)

// Only a branch that ends in STOP is terminal: a PRINT alone returns to the loop.
[<TestCase("40 IF i%=4 THEN PRINT \"x\":STOP", 1)>]
[<TestCase("40 IF i%=4 THEN PRINT \"x\"", 0)>]
let ``only branches ending in STOP keep a native FOR certified`` (line:string,certified:int) =
    let directory=Path.Combine(Path.GetTempPath(),$"terminal-stop-certificate-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"loop")
        let source="10 DIM a$(20)\n20 FOR i%=1 TO 10\n30 a$=a$ & i%:x=i%/2\n" + line + "\n50 END FOR i%\n60 STOP"
        withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
        Assert.That(File.ReadAllText(prefix+".analysis.txt"), Does.Contain($"Certified native FOR bindings: {certified}"))
    finally Directory.Delete(directory,true)

// A failure inside the threaded tail reports the same QL error as threaded code.
[<Test>]
let ``an error in a terminal tail matches threaded code`` () =
    let source="10 DIM a$(20)\n20 z=0\n30 FOR i%=1 TO 5\n40 a$=a$ & i%:x=i%/2\n50 IF i%=3 THEN PRINT 1/z:STOP\n60 END FOR i%\n70 STOP"
    let capture native =
        let mutable number=0
        withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");"--regalloc"] ".bas" source (fun executable ->
            let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
            let parsed=System.Text.RegularExpressions.Regex.Match(error.Message,@"ERNUM=(-?\d+)")
            Assert.That(parsed.Success, Is.True, error.Message)
            number<-int parsed.Groups[1].Value)
        number
    Assert.That(capture true, Is.EqualTo(capture false))

// String comparisons of whole DIMmed strings and string-array elements read
// their operands in place. Native output must match threaded output for every
// comparison, operand kind and order, including empty and case-different
// strings and QL's numeric ordering. The last value is the report's count.
let inPlaceComparePrograms = [|
    "10 DIM w$(4,10),v$(10),r$(40)\n20 w$(0)=\"apple\":w$(1)=\"Apple\":w$(2)=\"\":w$(3)=\"banana\":w$(4)=\"item10\":v$=\"item9\"\n30 FOR i%=0 TO 4\n40 r$=\"\"\n50 FOR j%=0 TO 4\n60 r$=r$ & (w$(i%)=w$(j%)) & (w$(i%)<w$(j%)) & (w$(i%)>w$(j%)) & (w$(i%)<=w$(j%)) & (w$(i%)>=w$(j%)) & (w$(i%)<>w$(j%)) & (w$(i%)==w$(j%))\n70 END FOR j%\n80 PRINT r$\n90 END FOR i%\n100 STOP", 7
    "10 DIM w$(4,10),v$(10),r$(40)\n20 w$(0)=\"apple\":w$(1)=\"Apple\":w$(2)=\"\":w$(3)=\"banana\":w$(4)=\"item10\"\n30 FOR i%=0 TO 4\n40 r$=\"\"\n50 FOR j%=0 TO 4\n60 v$=w$(j%)\n70 r$=r$ & (w$(i%)=v$) & (v$<w$(i%)) & (w$(i%)>v$) & (v$<=w$(i%)) & (w$(i%)>=v$) & (v$<>w$(i%)) & (w$(i%)==v$)\n80 END FOR j%\n90 PRINT r$\n100 END FOR i%\n110 STOP", 7
    "10 DIM a$(10),b$(10)\n20 a$=\"Zebra\":b$=\"zebra\"\n30 FOR i%=1 TO 2\n40 n%=n%+(a$<b$)+2*(a$==b$)+4*(a$=b$)\n50 b$=\"\"\n60 END FOR i%\n70 PRINT n%\n80 STOP", 3
|]
let inPlaceCompareModes = [|
    for index in 0..inPlaceComparePrograms.Length-1 do
        for hir in [false;true] do
            for registers in [false;true] do
                yield TestCaseData(index,hir,registers).SetName($"In-place compare {index} (HIR {hir}, registers {registers})") |]

[<TestCaseSource(nameof inPlaceCompareModes)>]
let ``in-place string comparisons match threaded results`` (index:int,hir:bool,registers:bool) =
    let source,_=inPlaceComparePrograms[index]
    let capture native =
        let mutable output=""
        withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
            let result=run 8000000L executable
            Assert.That(result.ExitCode, Is.Zero)
            output<-result.Output)
        output
    let threaded=capture false
    Assert.That(threaded, Is.Not.Empty)
    Assert.That(capture true, Is.EqualTo(threaded))

[<TestCase(false)>]
[<TestCase(true)>]
let ``in-place string reports count each rewritten comparison`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"in-place-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..inPlaceComparePrograms.Length-1 do
            let prefix=Path.Combine(directory,string index)
            let source,count=inPlaceComparePrograms[index]
            withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Contain($"In-place string comparisons: {count}"), $"fixture {index}")
    finally Directory.Delete(directory,true)

// Stores, CODE and LEN read the operand fetched just before them in place:
// elements, DIMmed strings and slices, including a long self-slice that the
// forward copy moves down over itself, truncation into a shorter DIM, empty
// sources and slices past the current length. Slice bounds are variables:
// constant bounds become MID$ calls. The values are the report's in-place
// store and CODE/LEN counts with HIR optimization off and on.
let inPlaceStorePrograms = [|
    "10 DIM w$(3,40),v$(40),s$(3),t$(40)\n20 w$(0)=\"apple\":w$(1)=\"banana split with cream and cherries\":a%=8:b%=12:c%=2:d%=3:e%=30\n30 v$=w$(1):w$(2)=v$:w$(3)=w$(0):s$=w$(1):t$=w$(1,a% TO b%)\n40 v$=v$(c% TO):w$(1)=w$(1,d% TO e%):t$=t$:w$(0)=w$(0)\n50 PRINT v$;\"|\";w$(1);\"|\";w$(2);\"|\";w$(3);\"|\";s$;\"|\";t$;\"|\";w$(0)\n60 w$(2)=\"\":v$=w$(4-2):PRINT LEN(v$)\n70 STOP", "anana split with cream and cherries|nana split with cream and ch|banana split with cream and cherries|apple|bana|split|apple\n0\n", (10,10), (0,0)
    "10 DIM a$(10),w$(2,10)\n20 a$=\"Hello\":w$(1)=\"World\"\n30 FOR i%=0 TO 6\n40 n%=n%*3+CODE(a$(i%))+LEN(a$(i% TO 5))+CODE(w$(1,i%))+LEN(w$(1))\n50 n%=n% MOD 10007\n60 END FOR i%\n70 PRINT n%\n80 STOP", "", (0,0), (4,4)
|]
let inPlaceStoreModes = [|
    for index in 0..inPlaceStorePrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"In-place store {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof inPlaceStoreModes)>]
let ``in-place string stores CODE and LEN match threaded results`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected,_,_=inPlaceStorePrograms[index]
    let capture native registers =
        let mutable output=""
        withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
            let result=run 8000000L executable
            Assert.That(result.ExitCode, Is.Zero)
            output<-result.Output)
        output
    let threaded=capture false false
    if expected<>"" then Assert.That(threaded, Is.EqualTo(expected))
    Assert.That(capture native registers, Is.EqualTo(threaded))

[<TestCase(false)>]
[<TestCase(true)>]
let ``in-place string reports count each store and CODE or LEN`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"in-place-store-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..inPlaceStorePrograms.Length-1 do
            let prefix=Path.Combine(directory,string index)
            let source,_,storeCounts,codeCounts=inPlaceStorePrograms[index]
            let pick (off,on) = if hir then on else off
            let stores,codes=pick storeCounts,pick codeCounts
            withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            let report=File.ReadAllText(prefix+".native-ir.txt")
            Assert.That(report, Does.Contain($"In-place string stores: {stores}"), $"fixture {index}")
            Assert.That(report, Does.Contain($"In-place CODE and LEN: {codes}"), $"fixture {index}")
    finally Directory.Delete(directory,true)

// Slice and bound checks fail as extr_string and the stores do, and a slice
// above mega_switch keeps its copy's out-of-memory test while the heap grows.
let inPlaceStoreErrors = [|
    "10 DIM a$(10)\n20 a$=\"Hello\":k%=1\n30 n%=CODE(a$(k% TO 20))\n40 STOP"
    "10 DIM a$(10)\n20 a$=\"Hello\":k%=-1\n30 n%=CODE(a$(k%))\n40 STOP"
    "10 DIM a$(10)\n20 a$=\"Hello\":k%=5\n30 n%=LEN(a$(k% TO 2))\n40 STOP"
    "10 DIM w$(2,10),v$(10)\n20 v$=\"x\":k%=5\n30 w$(k%)=v$\n40 STOP"
    "10 DIM a$(10)\n20 a$=\"Hello\":k%=-1\n30 n%=LEN(a$(k% TO 3))\n40 STOP"
    // A variable slice start reaches extr_string; constant bounds become MID$.
    "10 DIM p$(8000),t$(128),w$(1,128)\n20 t$=\"x\":FOR k%=1 TO 7:t$=t$ & t$:END FOR k%\n30 w$(1)=t$:j%=1:i%=2\n40 FOR k%=1 TO 400\n50 p$=p$ & \"abcdefghijklmnopqrstuvwxyz012345\"\n60 n%=LEN(p$ & p$ & CHR$(LEN(w$(j%,i% TO 101))))\n70 PRINT k%\n80 END FOR k%\n90 STOP"
|]

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
[<TestCase(4)>]
[<TestCase(5)>]
let ``in-place string store and slice failures match threaded code`` (index:int) =
    let capture native =
        let mutable outcome=""
        withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");"--regalloc"] ".bas" inPlaceStoreErrors[index] (fun executable ->
            let error=Assert.Throws<M68kFault>(fun () -> run 400000000L executable |> ignore)
            let parsed=System.Text.RegularExpressions.Regex.Match(error.Message,@"ERNUM=(-?\d+)")
            Assert.That(parsed.Success, Is.True, error.Message)
            let printed=error.Message.IndexOf("output=")
            Assert.That(printed, Is.GreaterThanOrEqualTo(0), error.Message)
            outcome<-parsed.Groups[1].Value+"|"+error.Message.Substring(printed))
        outcome
    let threaded=capture false
    Assert.That(threaded, Does.StartWith([|"-4|";"-4|";"-4|";"-4|";"-4|";"-3|"|][index]))
    Assert.That(capture true, Is.EqualTo(threaded))

// Failures keep their order and number: extr_string's index checks on either
// side, a left range error before a failing right index, and the copies'
// out-of-memory tests as recursion fills memory (the depth reached is printed).
let inPlaceCompareErrors = [|
    "10 DIM w$(2,10),v$(10)\n20 k%=-1\n30 FOR i%=1 TO 2\n40 n%=n%+(w$(k%)=v$)\n50 END FOR i%\n60 STOP"
    "10 DIM w$(2,10),v$(10)\n20 k%=3\n30 FOR i%=1 TO 2\n40 n%=n%+(v$=w$(k%))\n50 END FOR i%\n60 STOP"
    "10 DIM w$(2,10)\n20 k%=5:j%=1\n30 FOR i%=1 TO 2\n40 n%=n%+(w$(k%)=w$(j%+32767))\n50 END FOR i%\n60 STOP"
    "10 DIM t$(128)\n20 t$=\"x\"\n30 FOR k%=1 TO 7:t$=t$ & t$:END FOR k%\n40 walk 1\n50 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal s$(100)\n120 s$=t$:n%=n%+(t$=s$)\n130 PRINT depth%\n140 walk depth%+1\n150 END DEFine"
    // The heap grows 64 bytes an iteration below the comparison, so the right
    // operand's test, which counts the left copy's 128 bytes, fails two
    // iterations before one that ignored it.
    "10 DIM p$(8000),t$(128),u$(128)\n20 t$=\"x\":FOR k%=1 TO 7:t$=t$ & t$:END FOR k%\n30 u$=t$\n40 FOR k%=1 TO 400\n50 p$=p$ & \"abcdefghijklmnopqrstuvwxyz012345\"\n60 n%=LEN(p$ & p$ & (t$=u$))\n70 PRINT k%\n80 END FOR k%\n90 STOP"
    "10 DIM p$(8000),t$(128),w$(2,128)\n20 t$=\"x\":FOR k%=1 TO 7:t$=t$ & t$:END FOR k%\n30 w$(1)=t$:j%=1\n40 FOR k%=1 TO 400\n50 p$=p$ & \"abcdefghijklmnopqrstuvwxyz012345\"\n60 n%=LEN(p$ & p$ & (t$=w$(j%)))\n70 PRINT k%\n80 END FOR k%\n90 STOP"
|]

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
[<TestCase(4)>]
[<TestCase(5)>]
let ``in-place string comparison failures match threaded code`` (index:int) =
    let capture native =
        let mutable outcome=""
        withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");"--regalloc"] ".bas" inPlaceCompareErrors[index] (fun executable ->
            let error=Assert.Throws<M68kFault>(fun () -> run 400000000L executable |> ignore)
            let parsed=System.Text.RegularExpressions.Regex.Match(error.Message,@"ERNUM=(-?\d+)")
            Assert.That(parsed.Success, Is.True, error.Message)
            let printed=error.Message.IndexOf("output=")
            Assert.That(printed, Is.GreaterThanOrEqualTo(0), error.Message)
            outcome<-parsed.Groups[1].Value+"|"+error.Message.Substring(printed))
        outcome
    let threaded=capture false
    Assert.That(threaded, Does.StartWith([|"-4|";"-4|";"-4|";"-3|";"-3|";"-3|"|][index]))
    Assert.That(capture true, Is.EqualTo(threaded))

// Integer IF conditions inside memory-counter loops become one native branch.
// The last value is the number of fused conditions in the --dump-hir report.
let fusedConditionPrograms = [|
    // Five queens: three checked/proven array zero tests joined by AND.
    "10 DIM col%(5),up%(10),dn%(10):count%=0:place (1)\n20 PRINT count%\n30 STOP\n100 DEFine PROCedure place(row%)\n110 LOCal c%\n120 IF row%=6 THEN count%=count%+1:RETurn\n130 FOR c%=1 TO 5\n140 IF col%(c%)=0 AND up%(row%+c%)=0 AND dn%(row%-c%+5)=0 THEN\n150 col%(c%)=1:up%(row%+c%)=1:dn%(row%-c%+5)=1\n160 place (row%+1)\n170 col%(c%)=0:up%(row%+c%)=0:dn%(row%-c%+5)=0\n180 END IF\n190 END FOR c%\n200 END DEFine", "10\n", 1
    // Every integer comparison, OR, NOT, == and a truth test, in a string body.
    "10 DIM a$(40)\n20 FOR i%=1 TO 12\n30 IF i%<4 OR i%>=10 THEN a$=a$ & \"a\"\n40 IF NOT(i%<>6) THEN a$=a$ & \"b\"\n50 IF i% MOD 3 THEN a$=a$ & \"c\"\n60 IF (i%<=2 AND i%>0) OR i%==11 THEN a$=a$ & \"d\"\n70 END FOR i%\n80 PRINT a$\n90 STOP", "acdacdaccbccacacda\n", 4
    // ELSE arms, an assignment before the test and a value used by both arms.
    "10 DIM a$(20)\n20 FOR i%=-3 TO 3\n30 j%=i%*i%-2\n40 IF j%>0 AND i%<0 THEN\n50 a$=a$ & \"n\"\n60 ELSE\n70 IF j%<=0 THEN a$=a$ & \"z\" : ELSE a$=a$ & \"p\"\n80 END IF\n90 END FOR i%\n100 PRINT a$;\":\";j%\n110 STOP", "nnzzzpp:7\n", 2
    // A condition over a callee-written shared array, re-read after each call.
    "10 DIM seen%(4):total%=0:walk (3)\n20 PRINT total%;\":\";seen%(1);seen%(2);seen%(3)\n30 STOP\n100 DEFine PROCedure walk(depth%)\n110 LOCal i%\n120 IF depth%=0 THEN RETurn\n130 FOR i%=1 TO 3\n140 IF seen%(i%)<depth% OR i%=depth% THEN seen%(i%)=seen%(i%)+1:total%=total%+i%\n150 walk (depth%-1)\n160 END FOR i%\n170 END DEFine", "30:1153\n", 1
    // Proven reads short-circuit: AND exits to the IF label, OR to the THEN arm.
    "10 DIM a%(4),b%(4),r$(20)\n20 a%(1)=1:a%(3)=1:b%(2)=1:b%(3)=1\n30 FOR i%=0 TO 4\n40 IF a%(i%)=0 AND b%(i%)=0 THEN r$=r$ & \"n\"\n50 IF a%(i%)=1 OR b%(i%)=1 THEN r$=r$ & \"o\"\n60 END FOR i%\n70 PRINT r$\n80 STOP", "nooon\n", 2
|]
let fusedConditionModes = [|
    for index in 0..fusedConditionPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Fused condition {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof fusedConditionModes)>]
let ``fused native conditions match threaded IF results`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected,_=fusedConditionPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 6000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<TestCase(false)>]
[<TestCase(true)>]
let ``fused native condition reports count every selected branch`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"fused-condition-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..fusedConditionPrograms.Length-1 do
            let source,_,count=fusedConditionPrograms[index]
            let prefix=Path.Combine(directory,string index)
            let capture registers=withCompiledProgramMode hir ["--native-regions";(if registers then "--regalloc" else "--no-regalloc");"--dump-hir="+prefix] ".bas" source (fun executable -> run 6000000L executable)
            let before=capture false
            let after=capture true
            Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Contain($"Fused native conditions: selected={count};"), $"fixture {index}")
            Assert.That(after.Steps, Is.LessThan(before.Steps), $"fixture {index}")
    finally Directory.Delete(directory,true)

// x%=ABS(word) runs natively, so these jobs no longer call the ROM resident
// that the functional CPU cannot link. Expected text matches real-ROM threaded
// runs. The last value is the number of register-pinned native FOR loops.
let integerAbsPrograms = [|
    "10 DIM a%(5)\n20 FOR i%=0 TO 5:a%(i%)=i%*7-17:END FOR i%\n30 total%=0\n40 FOR i%=0 TO 5\n50 d%=ABS(a%(i%)):total%=total%*3+d%\n60 END FOR i%\n70 PRINT total%;\":\";d%\n80 STOP", "5109:18\n", 2
    "10 x%=-32767:a%=ABS(x%):x%=32767:b%=ABS(x%):x%=0:c%=ABS(x%):x%=-1:d%=ABS(x%)\n20 e%=ABS(a%-b%-1)\n30 PRINT a%;\":\";b%;\":\";c%;\":\";d%;\":\";e%\n40 STOP", "32767:32767:0:1:1\n", 0
    // Board validation: the inner loop with ABS and OR is now a native FOR.
    "10 DIM q%(4):q%(1)=3:q%(2)=1:q%(3)=4:q%(4)=2:bad%=0\n20 FOR l%=1 TO 4\n30 FOR r%=l%+1 TO 4\n40 s%=ABS(q%(l%)-q%(r%))\n50 IF s%=0 OR s%=r%-l% THEN bad%=bad%+1\n60 END FOR r%\n70 END FOR l%\n80 PRINT bad%;\":\";s%\n90 STOP", "0:2\n", 1
|]
let integerAbsModes = [|
    for index in 0..integerAbsPrograms.Length-1 do
        for hir in [false;true] do
            for registers in [false;true] do
                yield TestCaseData(index,hir,registers).SetName($"Integer ABS {index} (HIR {hir}, registers {registers})") |]

[<TestCaseSource(nameof integerAbsModes)>]
let ``native integer ABS matches the ROM results without calling it`` (index:int,hir:bool,registers:bool) =
    let source,expected,_=integerAbsPrograms[index]
    withCompiledProgramMode hir ["--native-regions";(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<Test>]
let ``native integer ABS lets call-free validation loops stay in registers`` () =
    let directory=Path.Combine(Path.GetTempPath(),$"integer-abs-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..integerAbsPrograms.Length-1 do
            let source,_,loops=integerAbsPrograms[index]
            let prefix=Path.Combine(directory,string index)
            withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Contain($"Native integer FOR loops: selected={loops};"), $"fixture {index}")
    finally Directory.Delete(directory,true)

[<TestCase(false,false)>]
[<TestCase(false,true)>]
[<TestCase(true,false)>]
[<TestCase(true,true)>]
let ``native integer ABS of -32768 raises the ROM conversion overflow`` (hir:bool,registers:bool) =
    // The ROM returns 32768.0 and float-to-int fails with overflow (-18).
    let sources=["10 x%=-32767-1\n20 y%=ABS(x%)\n30 PRINT \"wrong\"\n40 STOP"
                 "10 DIM a%(1):a%(1)=-32767-1\n20 FOR i%=0 TO 1\n30 y%=ABS(a%(i%))\n40 END FOR i%\n50 PRINT \"wrong\"\n60 STOP"]
    for source in sources do
        withCompiledProgramMode hir ["--native-regions";(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
            let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
            let parsed=System.Text.RegularExpressions.Regex.Match(error.Message,@"ERNUM=(-?\d+)")
            Assert.That(parsed.Success, Is.True, error.Message)
            Assert.That(int parsed.Groups[1].Value, Is.EqualTo(-18)))

[<Test>]
let ``fused native conditions skip only terms that cannot fail`` () =
    let directory=Path.Combine(Path.GetTempPath(),$"short-circuit-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let report name source=
            let prefix=Path.Combine(directory,name)
            withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            File.ReadAllText(prefix+".native-ir.txt")
        let program index=let source,_,_=fusedConditionPrograms[index] in source
        let proven=report "proven" (program 4)
        Assert.That(proven, Does.Contain("segments=2"))
        Assert.That(proven, Does.Contain("to end;"),"An OR under IF FALSE exits early into the THEN arm.")
        // Recursive row ranges prove every five-queens subscript and its arithmetic.
        Assert.That(report "queens" (program 0), Does.Contain("segments=3"))
        // A checked subscript may fail, so its AND stays one eager leaf.
        let checkedRead="10 DIM a$(4),b%(2)\n20 FOR i%=1 TO 2\n30 IF i%=99 AND b%(i%*3)=0 THEN a$=a$ & \"x\"\n40 END FOR i%\n50 STOP"
        Assert.That(report "checked" checkedRead, Does.Contain("segments=1"))
    finally Directory.Delete(directory,true)

[<TestCase(false)>]
[<TestCase(true)>]
let ``ROM maths calls keep floating loops native`` (hir:bool) =
    // The functional CPU cannot link ROM residents, so this checks selection;
    // real-ROM runs of the benchmark collection check the results.
    let directory=Path.Combine(Path.GetTempPath(),$"rom-maths-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"maths")
        let source="10 t=0\n20 FOR i%=1 TO 50\n30 t=t+SIN(i%/10)*COS(i%/20)+SQRT(i%)\n40 END FOR i%\n50 PRINT t\n60 STOP"
        withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable -> Assert.That(executable.Length, Is.GreaterThan(0)))
        Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Contain("Audited memory-counter native FOR loops: 1"))
        Assert.That(File.ReadAllText(prefix+".analysis.txt"), Does.Not.Contain("Global control barrier:"))
    finally Directory.Delete(directory,true)

// QL AND/OR evaluate every operand: a failing later operand must still fail
// when an earlier comparison already decides the result. Errors match threaded code.
let fusedConditionErrors = [|
    "10 DIM a$(4),b%(2):index%=5\n20 FOR i%=1 TO 2\n30 IF i%=99 AND b%(index%)=0 THEN a$=a$ & \"x\"\n40 END FOR i%\n50 STOP", -4
    "10 DIM a$(4),b%(2):index%=5\n20 FOR i%=1 TO 2\n30 IF i%>0 OR b%(index%)=0 THEN a$=a$ & \"x\"\n40 END FOR i%\n50 STOP", -4
    "10 DIM a$(4)\n20 FOR i%=1 TO 2\n30 IF i%=99 AND i%+32767>0 THEN a$=a$ & \"x\"\n40 END FOR i%\n50 STOP", -18
    "10 DIM a$(4),b%(2):index%=-1\n20 FOR i%=1 TO 2\n30 IF i%>0 AND i%<5 AND b%(index%)=0 THEN a$=a$ & \"x\"\n40 END FOR i%\n50 STOP", -4
    "10 DIM a$(4):zero%=0\n20 FOR i%=1 TO 2\n30 IF i%=1 OR i% DIV zero%=0 THEN a$=a$ & \"x\"\n40 END FOR i%\n50 STOP", -18
|]

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
[<TestCase(4)>]
let ``fused native conditions evaluate every operand and keep QL errors`` (index:int) =
    let source,expected=fusedConditionErrors[index]
    let capture native =
        let mutable number=0
        withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");"--regalloc"] ".bas" source (fun executable ->
            let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
            let parsed=System.Text.RegularExpressions.Regex.Match(error.Message,@"ERNUM=(-?\d+)")
            Assert.That(parsed.Success, Is.True, error.Message)
            number<-int parsed.Groups[1].Value)
        number
    Assert.That(capture true, Is.EqualTo(expected))
    Assert.That(capture false, Is.EqualTo(expected))

let stringAppendPrograms = [|
    // Turbo rounds the declared odd maximum up to an even byte count.
    "10 DIM a$(5)\n20 a$=\"AB\":a$=a$ & \"CDEF\":a$=a$ & \"ignored\"\n30 PRINT a$\n40 STOP", "ABCDEF\n"
    "10 DIM a$(8)\n20 a$=\"AB\":a$=a$ & a$:first%=2:last%=3\n30 a$=a$ & a$(first% TO last%)\n40 PRINT a$\n50 STOP", "ABABBA\n"
    "10 DIM a$(5)\n20 a$=\"\":a$=a$ & \"\":a$=a$ & CHR$(65):a$=a$ & LEN(a$)\n30 PRINT a$\n40 STOP", "A1\n"
    "10 DIM a$(8)\n20 a$=\"G\":work a$,a$\n30 PRINT a$\n40 STOP\n100 DEFine PROCedure work(p$,q$)\n110 p$=p$ & q$:q$=q$ & \"!\"\n120 END DEFine", "GG!\n"
    "10 a$=\"global\":work\n20 PRINT a$\n30 STOP\n100 DEFine PROCedure work\n110 LOCal a$(4)\n120 a$=\"L\":a$=a$ & \"ocal\"\n130 PRINT a$\n140 END DEFine", "Loca\nglobal\n"
    "10 a$=\"before\":a$=a$ & change$()\n20 PRINT a$\n30 STOP\n100 DEFine FuNction change$\n110 a$=\"after\"\n120 RETurn \"!\"\n130 END DEFine", "before!\n"
    "10 DIM a$(2)\n20 a$=\"AB\":d%=0\n30 WHEN ERROR\n40 PRINT ERNUM;\":\";a$\n50 STOP\n60 END WHEN\n70 a$=a$ & CHR$(65 DIV d%)\n80 PRINT \"wrong\"", "-18:AB\n"
    "10 DIM a$(3)\n20 a$=\"AB\":d%=0\n30 WHEN ERROR\n40 attempts%=attempts%+1\n50 IF attempts%<3 THEN RETRY\n60 PRINT ERNUM;\":\";attempts%;\":\";a$:STOP\n70 END WHEN\n80 a$=a$ & CHR$(65 DIV d%)\n90 PRINT \"wrong\"", "-18:3:AB\n"
    "10 DIM a$(2)\n20 a$=\"AB\"\n30 WHEN ERROR\n40 PRINT ERNUM;\":\";a$\n50 STOP\n60 END WHEN\n70 a$=a$ & a$(3)\n80 PRINT \"wrong\"", "-4:AB\n"
    "10 DIM a$(1,4)\n20 a$(0)=\"A\":a$(0)=a$(0) & \"BCDEF\"\n30 PRINT a$(0)\n40 STOP", "ABCD\n"
    "10 WHEN ERROR\n20 PRINT ERNUM:STOP\n30 END WHEN\n40 GO TO 60\n50 DIM a$(4)\n60 a$=a$ & \"x\"\n70 PRINT \"wrong\"", "-7\n"
|]

let stringAppendModes = [|
    for index in 0..stringAppendPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"String append {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof stringAppendModes)>]
let ``direct string append preserves Turbo truncation aliases and error handling`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected = stringAppendPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<TestCase(false)>]
[<TestCase(true)>]
let ``direct string append does not invent a combined length overflow`` (hir:bool) =
    let source = "10 DIM a$(32760)\n20 a$=FILL$(\"a\",32760)\n30 a$=a$ & FILL$(\"b\",30)\n40 PRINT LEN(a$);\":\";a$(1);\":\";a$(32760)\n50 STOP"
    withCompiledProgramMode hir [] ".bas" source (fun executable ->
        // Supply a larger QDOS job allocation for this capacity-limit test.
        // The CLI's default header requests only 20 KiB. Code is unchanged.
        let largeJob = Array.copy executable
        Array.blit [|0uy;2uy;0uy;0uy|] 0 largeJob 22 4 // 128 KiB dataspace.
        let result=run 4000000L largeJob
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo("32760:a:a\n")))

// Append and LEN of a string whose DIM reaches them run without P mode's
// missing-string checks. The last value is the report's proven site count.
let allocatedStringPrograms = [|
    "10 DIM a$(6)\n20 FOR i%=1 TO 9\n30 a$=a$ & CHR$(64+i%):n%=n%+LEN(a$)\n40 END FOR i%\n50 PRINT a$;\":\";n%\n60 STOP", "ABCDEF:39\n", 2
    "10 DIM a$(4)\n20 FOR p%=1 TO 2\n30 a$=\"\"\n40 FOR i%=1 TO 6\n50 a$=a$ & i%\n60 END FOR i%\n70 PRINT a$;\":\";LEN(a$)\n80 END FOR p%\n90 STOP", "1234:4\n1234:4\n", 2
    // Turbo rounds DIM a$(1) up to two bytes; a full string takes no more.
    "10 DIM a$(1)\n20 FOR i%=1 TO 3\n30 a$=a$ & \"x\":n%=n%+LEN(a$)\n40 END FOR i%\n50 PRINT a$;\":\";n%\n60 STOP", "xx:5\n", 2
    "10 DIM a$(8)\n20 FOR i%=1 TO 9\n30 a$=a$ & i%\n40 IF LEN(a$)=4 THEN EXIT i%\n50 END FOR i%\n60 PRINT a$;\":\";i%\n70 STOP", "1234:4\n", 2
    // String shapes do not cross calls: LEN inside the procedure stays checked.
    "10 DIM t$(8)\n20 FOR i%=1 TO 3\n30 t$=t$ & \"ab\":show\n40 END FOR i%\n50 PRINT n%\n60 STOP\n100 DEFine PROCedure show\n110 n%=n%+LEN(t$)\n120 END DEFine", "12\n", 1
|]
let allocatedStringModes = [|
    for index in 0..allocatedStringPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Allocated string {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof allocatedStringModes)>]
let ``allocated string append and LEN match threaded results`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected,_=allocatedStringPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<TestCase(false)>]
[<TestCase(true)>]
let ``allocated string reports count every proven append and LEN`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"allocated-string-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        for index in 0..allocatedStringPrograms.Length-1 do
            let prefix=Path.Combine(directory,string index)
            let source,_,sites=allocatedStringPrograms[index]
            withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source ignore
            Assert.That(File.ReadAllText(prefix+".analysis.txt"), Does.Contain($"Allocated-string append/LEN sites: {sites}"), $"fixture {index}")
            // Each fixture's innermost loop, including the one leaving by EXIT, is native.
            Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Contain("Audited memory-counter native FOR loops: 1"), $"fixture {index}")
    finally Directory.Delete(directory,true)

// A GO TO can skip the DIM: those sites keep the P-mode "not found" check,
// inside native loops too.
let unallocatedStringPrograms = [|
    "10 GO TO 30\n20 DIM a$(4)\n30 FOR i%=1 TO 2\n40 a$=a$ & \"x\"\n50 END FOR i%\n60 STOP"
    "10 GO TO 30\n20 DIM a$(4)\n30 FOR i%=1 TO 2\n40 n%=n%+LEN(a$)\n50 END FOR i%\n60 STOP"
|]
let unallocatedStringModes = [|
    for index in 0..unallocatedStringPrograms.Length-1 do
        for native in [false;true] do yield TestCaseData(index,native) |]

[<TestCaseSource(nameof unallocatedStringModes)>]
let ``append and LEN before a DIM keep the missing-string error`` (index:int,native:bool) =
    withCompiledProgramMode true [(if native then "--native-regions" else "--no-native-regions");"--regalloc"] ".bas" unallocatedStringPrograms[index] (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

let nativeValuePrograms = [|
    "10 a%=3:b%=a%+4:c%=b%*2:d%=-c%\n20 PRINT a%;\":\";b%;\":\";c%;\":\";d%\n30 STOP", "3:7:14:-14\n"
    "10 total%=0\n20 FOR i%=1 TO 10\n30 x%=(i%+1)*(i%+1):total%=total%+x%\n40 END FOR i%\n50 PRINT total%;\":\";x%;\":\";i%\n60 STOP", "505:121:10\n"
    "10 a%=3:work a%,a%\n20 PRINT a%\n30 STOP\n100 DEFine PROCedure work(p%,q%)\n110 p%=3:q%=7:answer%=p%+4\n120 PRINT answer%\n130 END DEFine", "11\n7\n"
    "10 WHEN ERROR\n20 retries%=retries%+1\n30 IF retries%<3 THEN RETRY\n40 PRINT ERNUM;\":\";retries%;\":\";answer%\n50 STOP\n60 END WHEN\n70 a%=3:b%=a%+4:answer%=42\n80 answer%=(b%*2)+(32767+1)\n90 PRINT \"wrong\"", "-18:3:42\n"
    "10 DIM a%(1)\n20 i%=1:a%(i%)=7:a%(i%)=a%(i%)+a%(i%)\n30 PRINT a%(i%)\n40 STOP", "14\n"
|]
let nativeValueModes = [|
    for index in 0..nativeValuePrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Native values {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof nativeValueModes)>]
let ``native value optimization preserves constants repeated results aliases retries and arrays`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=nativeValuePrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

let rankOnePrograms = [|
    "10 DIM a%(100):total%=0\n20 FOR i%=0 TO 100\n30 a%(i%)=i%*2:total%=total%+a%(i%)\n40 END FOR i%\n50 PRINT total%;\":\";a%(100);\":\";i%\n60 STOP", "10100:200:100\n"
    "10 DIM a%(3),indexes%(0)\n20 indexes%(0)=2\n30 a%(indexes%(0))=-32768\n40 result%=a%(indexes%(0))\n50 PRINT result%;\":\";a%(2)\n60 STOP", "-32768:-32768\n"
    "10 DIM a%(2),indexes%(0)\n20 a%(2)=7:indexes%(0)=4\n30 DIM a%(4)\n40 a%(indexes%(0))=99\n50 PRINT a%(4)\n60 STOP", "99\n"
    "10 DIM a%(3):last%=3:stride%=-1:total%=0\n20 FOR i%=last% TO 0 STEP stride%\n30 last%=-100:stride%=32767:a%(i%)=i%+1:total%=total%+a%(i%)\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "10:0\n"
    "10 work\n20 work\n30 STOP\n100 DEFine PROCedure work\n110 LOCal local%(3),index%\n120 index%=2:local%(index%)=7\n130 local%(index%)=local%(index%)+1\n140 PRINT local%(index%)\n150 END DEFine", "8\n8\n"
    "10 DIM a%(1,1)\n20 a%(0,0)=7:a%(1,1)=9\n30 PRINT a%(0,0);\":\";a%(1,1)\n40 STOP", "7:9\n"
|]
let rankOneModes = [|
    for index in 0..rankOnePrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"RankOne execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof rankOneModes)>]
let ``rank-one array native IR preserves values loops resizing and local rebinding`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=rankOnePrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

let rankOneFailures = [|
    for store in [false;true] do
        for negative in [false;true] do
            for hir in [false;true] do
                for native,registers in [false,false;true,false;true,true] do
                    yield TestCaseData(store,negative,hir,native,registers).SetName($"RankOne failure store {store}, negative {negative}, HIR {hir}, native {native}, registers {registers}") |]

[<TestCaseSource(nameof rankOneFailures)>]
let ``rank-one checked native accesses keep lower and upper errors without a handler`` (store:bool,negative:bool,hir:bool,native:bool,registers:bool) =
    let index=if negative then -1 else 2
    let access=if store then "a%(indexes%(0))=99" else "result%=7+a%(indexes%(0))"
    let source = $"10 DIM a%%(1),indexes%%(0)\n20 indexes%%(0)={index}\n30 {access}\n40 PRINT \"wrong\"\n50 STOP"
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

[<TestCase(false)>]
[<TestCase(true)>]
let ``RankOne native FOR plans contain checked and proven accesses`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"rank-one-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"array")
        let source="10 DIM a%(3),indexes%(0):indexes%(0)=2:total%=0\n20 FOR i%=0 TO 3\n30 a%(i%)=i%:total%=total%+a%(indexes%(0))\n40 END FOR i%\n50 PRINT total%\n60 STOP"
        withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable ->
            Assert.That((run 2000000L executable).Output, Is.EqualTo("4\n")))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain("Native integer FOR loops: selected=1"))
        Assert.That(report, Does.Contain("StoreElement"))
        Assert.That(report, Does.Contain("LoadElement"))
        let listing=File.ReadAllText(prefix+".analysis.txt")
        Assert.That(listing, Does.Match("Rank-one integer array accesses: [1-9]"))
    finally Directory.Delete(directory,true)

let rankTwoPrograms = [|
    "10 DIM a%(3,7):total%=0\n20 FOR row%=0 TO 3\n30 FOR col%=0 TO 7\n40 a%(row%,col%)=row%*8+col%:total%=total%+a%(row%,col%)\n50 END FOR col%\n60 END FOR row%\n70 PRINT total%;\":\";a%(3,7);\":\";col%\n80 STOP", "496:31:7\n"
    // One access has two reaching shapes. Minimum bounds cannot become strides.
    "10 DIM flags%(0):flags%(0)=1\n20 IF flags%(0) THEN\n30 DIM a%(3,15)\n40 ELSE\n50 DIM a%(3,7)\n60 END IF\n70 row%=2:col%=3:a%(row%,col%)=-32768\n80 PRINT a%(2,3);\":\";a%(1,3)\n90 STOP", "-32768:0\n"
    "10 DIM a%(3,15),indexes%(1):indexes%(0)=2:indexes%(1)=3\n20 a%(indexes%(0),indexes%(1))=99\n30 PRINT a%(2,3);\":\";a%(1,3)\n40 STOP", "99:0\n"
    "10 DIM a%(3,7):row%=2:col%=3:a%(row%,col%)=7\n20 DIM a%(3,15)\n30 a%(row%,col%)=99\n40 PRINT a%(row%,col%)\n50 STOP", "99\n"
    "10 DIM a%(1,1):a%(1,1)=7:work a%,a%\n20 PRINT a%(1,1)\n30 STOP\n100 DEFine PROCedure work(p%,q%)\n110 p%(1,1)=p%(1,1)+1:q%(1,1)=p%(1,1)+2\n120 END DEFine", "10\n"
    "10 work:work\n20 STOP\n100 DEFine PROCedure work\n110 LOCal a%(2,3),row%,col%\n120 row%=1:col%=2:a%(row%,col%)=7\n130 a%(row%,col%)=a%(row%,col%)+1\n140 PRINT a%(row%,col%)\n150 END DEFine", "8\n8\n"
    "10 DIM a%(1,1,1):a%(1,0,1)=17\n20 PRINT a%(1,0,1)\n30 STOP", "17\n"
    "10 WHEN ERROR\n20 attempts%=attempts%+1\n30 IF attempts%<3 THEN RETRY\n40 PRINT ERNUM;\":\";attempts%;\":\";answer%\n50 STOP\n60 END WHEN\n70 DIM a%(1,2):row%=1:col%=3:answer%=42\n80 answer%=7+a%(row%,col%)\n90 PRINT \"wrong\"", "-4:3:42\n"
    // All indices and the RHS are evaluated before either bounds check.
    "10 WHEN ERROR\n20 PRINT ERNUM;\":\";a%(0,0)\n30 STOP\n40 END WHEN\n50 DIM a%(1,2):row%=3:col%=3:divisor%=0:a%(0,0)=42\n60 a%(row%,col%)=1 DIV divisor%\n70 PRINT \"wrong\"", "-18:42\n"
|]
let rankTwoModes = [|
    for index in 0..rankTwoPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"RankTwo execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof rankTwoModes)>]
let ``rank-two native arrays preserve loops live shapes aliases locals and error ordering`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=rankTwoPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 3000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

[<Test>]
let ``RankTwo native FOR report contains fixed rank accesses and reduces execution cost`` () =
    let directory=Path.Combine(Path.GetTempPath(),$"rank-two-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let source,expected=rankTwoPrograms[0]
        let prefix=Path.Combine(directory,"array")
        let capture registers=withCompiledProgramMode true ["--native-regions";(if registers then "--regalloc" else "--no-regalloc");"--dump-hir="+prefix] ".bas" source (fun executable -> executable.Length,run 3000000L executable)
        let beforeSize,before=capture false
        let afterSize,after=capture true
        Assert.That(after.Output, Is.EqualTo(expected))
        Assert.That(after.Output, Is.EqualTo(before.Output))
        Assert.That(after.Steps, Is.LessThan(before.Steps))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain("Native integer FOR loops: selected=1"))
        Assert.That(report, Does.Contain("LoadElement2"))
        Assert.That(report, Does.Contain("StoreElement2"))
        TestContext.WriteLine($"Rank-two native FOR: executable bytes {beforeSize}->{afterSize}; instructions {before.Steps}->{after.Steps}.")
    finally Directory.Delete(directory,true)

let procedureEffectPrograms = [|
    // Element-only callees preserve the caller's rank and exact shape.
    "10 DIM a%(3,7):fill\n20 PRINT a%(3,7);\":\";a%(1,2)\n30 STOP\n100 DEFine PROCedure fill\n110 LOCal row%,col%\n120 FOR row%=0 TO 3\n130 FOR col%=0 TO 7\n140 put (row%),(col%)\n150 END FOR col%\n160 END FOR row%\n170 END DEFine\n200 DEFine PROCedure put(row%,col%)\n210 a%(row%,col%)=row%*8+col%\n220 END DEFine", "31:10\n"
    // Callee-private vectors with the same names must not corrupt caller facts.
    "10 DIM a%(3):a%(3)=8:total%=0\n20 FOR i%=1 TO 3\n30 private:total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%;\":\";a%(3)\n60 STOP\n100 DEFine PROCedure private\n110 LOCal i%,a%(1)\n120 FOR i%=10 TO 11\n130 a%(0)=i%\n140 END FOR i%\n150 END DEFine", "6:3:8\n"
    // A reference changes the current word but does not replace captured NEXT.
    "10 total%=0\n20 FOR i%=1 TO 3\n30 bump i%:total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP\n100 DEFine PROCedure bump(p%)\n110 p%=p%+1\n120 END DEFine", "6:4\n"
    "10 total%=0\n20 FOR i%=1 TO 3\n30 bump (i%):total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP\n100 DEFine PROCedure bump(p%)\n110 p%=p%+1\n120 END DEFine", "6:3\n"
    // A resize changes the stride and invalidates the former exact shape.
    "10 DIM a%(3,15):a%(1,3)=4:resize\n20 row%=2:col%=3:a%(row%,col%)=9\n30 inspect a%\n40 STOP\n100 DEFine PROCedure resize\n110 DIM a%(3,7)\n120 END DEFine\n200 DEFine PROCedure inspect(p%)\n210 PRINT p%(2,3);\":\";p%(1,3)\n220 END DEFine", "9:0\n"
    // A forwarding call maps array effects onto the actual array, not its shape.
    "10 DIM a%(3):a%(3)=8:forward a%\n20 PRINT a%(0);\":\";a%(3)\n30 STOP\n100 DEFine PROCedure forward(p%)\n110 p%(3)=p%(3):put p%\n120 END DEFine\n200 DEFine PROCedure put(q%)\n210 q%(0)=7\n220 END DEFine", "7:8\n"
    // Free names can resolve to a caller's local array rather than the global.
    "10 DIM a%(3):a%(0)=9:outer\n20 PRINT a%(0)\n30 STOP\n100 DEFine PROCedure outer\n110 LOCal a%(1)\n120 a%(0)=4:inner\n130 PRINT a%(0)\n140 END DEFine\n200 DEFine PROCedure inner\n210 a%(0)=7\n220 END DEFine", "7\n9\n"
    // Aliased parameters must kill values even when descriptors are preserved.
    "10 a%=1:mutate a%,a%\n20 PRINT a%\n30 STOP\n100 DEFine PROCedure mutate(p%,q%)\n110 p%=1:q%=7:answer%=p%+1\n120 PRINT answer%\n130 END DEFine", "8\n7\n"
    // Normal binding cleanup and array error ordering still use the runtime.
    "10 WHEN ERROR\n20 PRINT ERNUM;\":\";a%(0)\n30 STOP\n40 END WHEN\n50 DIM a%(1):a%(0)=9:bad\n60 PRINT \"wrong\"\n100 DEFine PROCedure bad\n110 LOCal i%\n120 FOR i%=0 TO 2\n130 a%(i%)=7\n140 END FOR i%\n150 END DEFine", "-4:7\n"
|]
let procedureEffectModes = [|
    for index in 0..procedureEffectPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"Procedure effects execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof procedureEffectModes)>]
let ``procedure effects retain dynamic bindings array shapes counter writes and errors`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=procedureEffectPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 3000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

let callEntryPrograms = [|
    procedureEffectPrograms[0]
    // Different callers guarantee minimum bounds but require the live stride.
    "10 DIM a%(3,7):put (7):inspect a%\n20 DIM a%(3,15):put (99):inspect a%\n30 STOP\n100 DEFine PROCedure put(value%)\n110 a%(2,3)=value%\n120 END DEFine\n200 DEFine PROCedure inspect(p%)\n210 PRINT p%(2,3);\":\";p%(1,3)\n220 END DEFine", "7:0\n99:0\n"
    "10 DIM flags%(0):flags%(0)=1\n20 IF flags%(0) THEN\n30 DIM a%(3,15)\n40 ELSE\n50 DIM a%(3,7)\n60 END IF\n70 put (99):inspect a%\n80 STOP\n100 DEFine PROCedure put(value%)\n110 a%(2,3)=value%\n120 END DEFine\n200 DEFine PROCedure inspect(p%)\n210 PRINT p%(2,3);\":\";p%(1,3)\n220 END DEFine", "99:0\n"
    "10 DIM a%(1,2):a%(1,2)=7\n20 answer%=read%()+read%()\n30 PRINT answer%\n40 STOP\n100 DEFine FuNction read%\n110 RETurn a%(1,2)\n120 END DEFine", "14\n"
    // A sibling mutates the descriptor; the earlier read retains its value.
    "10 DIM a%(3,7):a%(2,3)=7\n20 answer%=read%()+grow%()\n30 PRINT answer%:inspect a%\n40 STOP\n100 DEFine FuNction read%\n110 RETurn a%(2,3)\n120 END DEFine\n200 DEFine FuNction grow%\n210 DIM a%(3,15):a%(2,3)=99\n220 RETurn 0\n230 END DEFine\n300 DEFine PROCedure inspect(p%)\n310 PRINT p%(2,3);\":\";p%(1,3)\n320 END DEFine", "7\n99:0\n"
    "10 DIM a%(3,15):a%(2,3)=99:shrink\n20 inspect a%\n30 STOP\n100 DEFine PROCedure shrink\n110 PRINT a%(2,3)\n120 DIM a%(3,7):a%(2,3)=7\n130 END DEFine\n200 DEFine PROCedure inspect(p%)\n210 PRINT p%(2,3);\":\";p%(1,3)\n220 END DEFine", "99\n7:0\n"
    "10 DIM a%(3,7):a%(1,2)=5:visit (2)\n20 STOP\n100 DEFine PROCedure visit(depth%)\n110 PRINT a%(1,2)\n120 IF depth%>0 THEN visit (depth%-1)\n130 END DEFine", "5\n5\n5\n"
    procedureEffectPrograms[6]
    procedureEffectPrograms[1]
|]
let callEntryModes = [|
    for index in 0..callEntryPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"CallEntry execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof callEntryModes)>]
let ``CallEntry shapes preserve actual strides mutation ordering and dynamic binding`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=callEntryPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 3000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

let callEntryFailures = [|
    for row,column in [-1,0;2,0;0,3] do
        for store in [false;true] do
            for hir in [false;true] do
                for native,registers in [false,false;true,false;true,true] do
                    yield TestCaseData(row,column,store,hir,native,registers).SetName($"CallEntry bounds {row},{column} store {store}, HIR {hir}, native {native}, registers {registers}") |]

[<TestCaseSource(nameof callEntryFailures)>]
let ``CallEntry known ranks retain checks for unknown row and column values`` (row:int,column:int,store:bool,hir:bool,native:bool,registers:bool) =
    let access=if store then "a%(row%,col%)=42" else "answer%=7+a%(row%,col%)"
    let source = $"10 DIM a%%(1,2),indexes%%(1):indexes%%(0)={row}:indexes%%(1)={column}\n20 bad\n30 PRINT \"wrong\"\n40 STOP\n100 DEFine PROCedure bad\n110 LOCal row%%,col%%\n120 row%%=indexes%%(0):col%%=indexes%%(1)\n130 {access}\n140 END DEFine"
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 3000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

[<Test>]
let ``CallEntry native plans include checked and proven global arrays inside procedures`` () =
    let directory=Path.Combine(Path.GetTempPath(),$"call-entry-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"arrays")
        let source,expected=callEntryPrograms[0]
        let capture registers=withCompiledProgramMode true ["--native-regions";(if registers then "--regalloc" else "--no-regalloc");"--dump-hir="+prefix] ".bas" source (fun executable -> executable.Length,run 3000000L executable)
        let beforeSize,before=capture false
        let afterSize,after=capture true
        Assert.That(after.Output, Is.EqualTo(expected))
        Assert.That(after.Output, Is.EqualTo(before.Output))
        Assert.That(after.Steps, Is.LessThan(before.Steps))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain("Native integer FOR loops: selected=0"))
        Assert.That(report, Does.Contain("StoreElement2"))
        let analysis=File.ReadAllText(prefix+".analysis.txt")
        Assert.That(analysis, Does.Match("Rank-two integer array accesses: [1-9]"))
        Assert.That(analysis, Does.Match("Global array call-entry analysis: passes=[1-9][0-9]*; certified=True"))
        TestContext.WriteLine($"CallEntry arrays: executable bytes {beforeSize}->{afterSize}; instructions {before.Steps}->{after.Steps}.")
    finally Directory.Delete(directory,true)

let scopedEligibilityPrograms = [|
    "10 GOSUB 200\n20 work\n30 PRINT total%;\":\";z%\n40 STOP\n100 DEFine PROCedure work\n110 LOCal i%\n120 total%=0\n130 FOR i%=1 TO 10\n140 total%=total%+i%\n150 END FOR i%\n160 END DEFine\n200 z%=1\n210 RETurn", "55:1\n",1
    "10 GOSUB 100:total%=0\n20 FOR i%=1 TO 10\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";z%\n60 STOP\n100 z%=1\n110 RETurn", "55:1\n",1
    "10 GOSUB 200:work\n20 PRINT a%(3)\n30 STOP\n100 DEFine PROCedure work\n110 LOCal i%\n115 DIM a%(3)\n120 FOR i%=0 TO 3\n130 a%(i%)=i%\n140 END FOR i%\n150 END DEFine\n200 z%=1\n210 RETurn", "3\n",1
    "10 GOSUB 300:total%=0:visit (3)\n20 PRINT total%\n30 STOP\n100 DEFine PROCedure visit(depth%)\n110 LOCal i%\n120 FOR i%=1 TO 3\n130 total%=total%+i%\n140 END FOR i%\n150 IF depth%>0 THEN visit (depth%-1)\n160 END DEFine\n300 z%=1\n310 RETurn", "24\n",1
    "10 WHEN ERROR\n20 PRINT \"error\":STOP\n30 END WHEN\n40 total%=0\n50 FOR i%=1 TO 3\n60 total%=total%+i%\n70 END FOR i%\n80 PRINT total%\n90 STOP", "6\n",0
    "10 total%=0\n20 FOR i%=1 TO 3\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%\n60 STOP\n100 DEFine PROCedure opaque\n110 POKE 0,0\n120 END DEFine", "6\n",0
|]

let scopedEligibilityModes = [|
    for index in 0..scopedEligibilityPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"ScopedEligibility execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof scopedEligibilityModes)>]
let ``ScopedEligibility preserves procedures recursion arrays and persistent barriers`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected,_=scopedEligibilityPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
[<TestCase(4)>]
[<TestCase(5)>]
let ``ScopedEligibility reports select independent loops and explain persistent fallbacks`` (index:int) =
    let source,expected,loops=scopedEligibilityPrograms[index]
    let directory=Path.Combine(Path.GetTempPath(),$"scoped-eligibility-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"scope")
        withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable ->
            Assert.That((run 2000000L executable).Output, Is.EqualTo(expected)))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain($"Native integer FOR loops: selected={loops};"))
        if loops=0 then
            Assert.That(report, Does.Contain("Native FOR rejected at IR byte"))
            Assert.That(File.ReadAllText(prefix+".analysis.txt"), Does.Contain("Global control barrier:"))
    finally Directory.Delete(directory,true)

[<Test>]
let ``ScopedEligibility compiles native array loops beside ROM ABS without altering IR format`` () =
    // The functional CPU cannot link this ROM resident. The unchanged
    // eight-queens job exercises ABS and the new array proofs in Q-emuLator.
    let source="10 DIM a%(3):x%=-7:amount=ABS(x%):work\n20 PRINT a%(3);\":\";amount\n30 STOP\n100 DEFine PROCedure work\n110 LOCal i%\n120 FOR i%=0 TO 3\n130 a%(i%)=i%\n140 END FOR i%\n150 END DEFine"
    let directory=Path.Combine(Path.GetTempPath(),$"scoped-abs-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"abs")
        withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable -> Assert.That(executable.Length, Is.GreaterThan(0)))
        Assert.That(File.ReadAllText(prefix+".native-ir.txt"), Does.Contain("Native integer FOR loops: selected=1;"))
        Assert.That(File.ReadAllText(prefix+".analysis.txt"), Does.Contain("Global array call-entry analysis: passes=2; certified=True"))
    finally Directory.Delete(directory,true)

let localForPrograms = [|
    "10 i%=77:work:work\n20 PRINT i%;\":\";total%\n30 STOP\n100 DEFine PROCedure work\n110 LOCal i%,last%,stride%,term%\n120 last%=5:stride%=1:total%=0\n130 FOR i%=1 TO last% STEP stride%\n140 last%=99:stride%=-1:term%=i%*2:total%=total%+term%\n150 END FOR i%\n160 PRINT i%;\":\";total%\n170 END DEFine", "5:30\n5:30\n77:30\n"
    "10 work (5),(-1),(-2):work (5),(1),(1):work (32767),(32760),(-2)\n20 STOP\n100 DEFine PROCedure work(first%,last%,stride%)\n110 LOCal i%,total%\n120 total%=0\n130 FOR i%=first% TO last% STEP stride%\n140 total%=total%+(i% MOD 10)\n150 END FOR i%\n160 PRINT i%;\":\";total%\n170 END DEFine", "-1:18\n5:0\n32761:16\n"
    "10 i%=88:total%=0:visit (3)\n20 PRINT i%;\":\";total%\n30 STOP\n100 DEFine PROCedure visit(depth%)\n110 LOCal i%\n120 FOR i%=1 TO 3\n130 total%=total%+i%\n140 END FOR i%\n150 IF depth%>0 THEN visit (depth%-1)\n160 PRINT i%\n170 END DEFine", "3\n3\n3\n3\n88:24\n"
    "10 DIM a%(5),b%(1,5):work\n20 PRINT a%(5);\":\";b%(1,5)\n30 STOP\n100 DEFine PROCedure work\n110 LOCal i%\n120 FOR i%=0 TO 5\n130 a%(i%)=i%:b%(1,i%)=a%(i%)+1\n140 END FOR i%\n150 END DEFine", "5:6\n"
    // A formal may alias visible memory. This body must keep the old loop path.
    "10 value%=0:work value%\n20 PRINT value%\n30 STOP\n100 DEFine PROCedure work(p%)\n110 LOCal i%\n120 FOR i%=1 TO 4\n130 p%=p%+i%\n140 END FOR i%\n150 END DEFine", "10\n"
    "10 i%=0:visits%=0:work i%\n20 PRINT i%;\":\";visits%\n30 STOP\n100 DEFine PROCedure work(p%)\n110 FOR i%=1 TO 3\n120 p%=p%+1:visits%=visits%+1\n130 END FOR i%\n140 END DEFine", "4:2\n"
    // Procedure bindings elsewhere must not exclude this independent main loop.
    "10 work:total%=0\n20 FOR j%=1 TO 4\n30 total%=total%+j%\n40 END FOR j%\n50 PRINT total%;\":\";j%\n60 STOP\n100 DEFine PROCedure work\n110 LOCal private%\n120 private%=99\n130 END DEFine", "10:4\n"
    "10 PRINT answer%(4);\":\";answer%(2)\n20 STOP\n100 DEFine FuNction answer%(limit%)\n110 LOCal i%,total%\n120 total%=0\n130 FOR i%=1 TO limit%\n140 total%=total%+i%\n150 END FOR i%\n160 RETurn total%\n170 END DEFine", "10:3\n"
    "10 n%=4:work n%:work (2)\n20 PRINT n%\n30 STOP\n100 DEFine PROCedure work(limit%)\n110 LOCal i%,total%\n120 total%=0\n130 FOR i%=1 TO limit%\n140 total%=total%+i%\n150 END FOR i%\n160 PRINT i%;\":\";total%\n170 END DEFine", "4:10\n2:3\n4\n"
|]

let invariantArrayPrograms = [|
    "10 DIM a%(2,3):row%=1:total%=0\n20 FOR i%=0 TO 3\n30 a%(row%,i%)=i%:total%=total%+a%(row%,i%)\n40 END FOR i%\n50 PRINT total%;\":\";a%(1,3)\n60 STOP", "6:3\n"
    "10 DIM a%(2,3):row%=1:work\n20 PRINT a%(1,3)\n30 DIM a%(2,7):row%=2:work\n40 PRINT a%(2,3)\n50 STOP\n100 DEFine PROCedure work\n110 LOCal i%\n120 FOR i%=0 TO 3\n130 a%(row%,i%)=i%+row%\n140 END FOR i%\n150 END DEFine", "4\n5\n"
    "10 DIM a%(1,3),index%(0):index%(0)=99:row%=index%(0):total%=0\n20 FOR i%=0 TO 3\n30 IF i%<0 THEN total%=total%+a%(row%,i%)\n40 END FOR i%\n50 FOR j%=3 TO 0\n60 total%=total%+a%(row%,j%)\n70 END FOR j%\n80 PRINT total%\n90 STOP", "0\n"
    "10 DIM a%(1,3):row%=1:total%=0\n20 FOR i%=0 TO 3\n30 IF i%>1 THEN a%(row%,i%)=i%:total%=total%+a%(row%,i%)\n40 END FOR i%\n50 PRINT total%;\":\";a%(1,3)\n60 STOP", "5:3\n"
    "10 DIM a%(1,3):total%=0\n20 FOR i%=0 TO 3\n30 row%=i% MOD 2:a%(row%,i%)=i%:total%=total%+a%(row%,i%)\n40 END FOR i%\n50 PRINT total%;\":\";a%(0,2);\":\";a%(1,3)\n60 STOP", "6:2:3\n"
    "10 DIM index%(0):index%(0)=1:flag%=index%(0)\n20 IF flag% THEN DIM a%(2,7):ELSE DIM a%(2,3)\n30 row%=1:total%=0\n40 FOR i%=0 TO 3\n50 a%(row%,i%)=i%:total%=total%+a%(row%,i%)\n60 END FOR i%\n70 PRINT total%;\":\";a%(1,3)\n80 STOP", "6:3\n"
|]
let invariantArrayModes = [|
    for index in 0..invariantArrayPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"InvariantArray execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof invariantArrayModes)>]
let ``InvariantArray lazy rows agree across all modes repeated calls DIM joins and changing row fallback`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=invariantArrayPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
[<TestCase(4)>]
[<TestCase(5)>]
let ``InvariantArray reports show retention only when the row and descriptor are invariant`` (index:int) =
    let source,expected=invariantArrayPrograms[index]
    let directory=Path.Combine(Path.GetTempPath(),$"invariant-array-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"array")
        withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable ->
            Assert.That((run 4000000L executable).Output, Is.EqualTo(expected)))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        if index=2 then Assert.That(report, Does.Contain("Native integer FOR loops: selected=0;"))
        else Assert.That(report, Does.Match("Native integer FOR loops: selected=[1-9]"))
        if index=2 || index=4 then Assert.That(report, Does.Not.Match("invariant array rows: retained=[1-9]"))
        else Assert.That(report, Does.Match("invariant array rows: retained=[1-9]"))
        if index=5 then Assert.That(report, Does.Contain("Columns = 0"))
    finally Directory.Delete(directory,true)

let invariantArrayFailurePrograms = [|
    "10 DIM a%(1,3),index%(0):index%(0)=2:row%=index%(0)\n20 FOR i%=0 TO 3\n30 a%(row%,i%)=i%\n40 END FOR i%\n50 STOP"
    "10 DIM a%(1,3),index%(2):index%(0)=0:index%(1)=1:index%(2)=99:row%=1\n20 FOR i%=0 TO 2\n30 a%(row%,index%(i%))=i%\n40 END FOR i%\n50 STOP"
|]
let invariantArrayFailureModes = [|
    for index in 0..invariantArrayFailurePrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"InvariantArray error {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof invariantArrayFailureModes)>]
let ``InvariantArray preserves first row and later column runtime errors`` (index:int,hir:bool,native:bool,registers:bool) =
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" invariantArrayFailurePrograms[index] (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 4000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

let directConditionPrograms = [|
    "10 total%=0\n20 FOR i%=-3 TO 3\n30 IF i% THEN total%=total%+1\n40 IF NOT i% THEN total%=total%+10\n50 IF NOT(NOT i%) THEN total%=total%+100\n60 IF NOT(NOT(NOT i%)) THEN total%=total%+1000\n70 END FOR i%\n80 PRINT total%\n90 STOP", "1616\n"
    "10 total%=0\n20 FOR i%=-1 TO 1\n30 IF i%>=0 THEN total%=total%+1\n40 IF NOT(i%>=0) THEN total%=total%+10\n50 IF i%<>0 THEN total%=total%+100\n60 IF i%<=0 THEN total%=total%+1000\n70 END FOR i%\n80 PRINT total%\n90 STOP", "2212\n"
    "10 total%=0\n20 FOR i%=-2 TO 2\n30 IF (i%<0) AND (i%<>-1) THEN total%=total%+1\n40 IF (i%>0) OR (i%=0) THEN total%=total%+10\n50 IF NOT((i%>0) AND NOT(i%=2)) THEN total%=total%+100\n60 IF NOT((i%<0) OR (i%=0)) THEN total%=total%+1000\n70 END FOR i%\n80 PRINT total%\n90 STOP", "2431\n"
    "10 total%=0\n20 FOR i%=-2 TO 2\n30 IF i% AND -3 THEN total%=total%+1\n40 IF i% OR 0 THEN total%=total%+10\n50 IF NOT(i% AND NOT i%) THEN total%=total%+100\n60 IF i% XOR NOT i% THEN total%=total%+1000\n70 END FOR i%\n80 PRINT total%\n90 STOP", "5544\n"
    "10 DIM a%(3):total%=0\n20 FOR i%=0 TO 3\n30 a%(i%)=(i%>1)\n40 IF a%(i%) THEN total%=total%+i%\n50 IF NOT(a%(i%)=0) THEN total%=total%+10\n60 END FOR i%\n70 PRINT total%;\":\";a%(0);\":\";a%(1);\":\";a%(2);\":\";a%(3)\n80 STOP", "25:0:0:1:1\n"
|]

let directConditionModes = [|
    for index in 0..directConditionPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"DirectCondition execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof directConditionModes)>]
let ``DirectCondition signed comparisons integer truth eager logic and stored booleans agree in all modes`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=directConditionPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
[<TestCase(4)>]
let ``DirectCondition reports confirm direct comparisons truth tests and eager branches execute`` (index:int) =
    let source,expected=directConditionPrograms[index]
    let directory=Path.Combine(Path.GetTempPath(),$"direct-condition-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"condition")
        withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable ->
            Assert.That((run 4000000L executable).Output, Is.EqualTo(expected)))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain("Native integer FOR loops: selected=1;"))
        let marker=if index=0 then "integer truth=[1-9]" elif index=2 || index=3 then "eager AND/OR=[1-9]" else "direct conditions: comparisons=[1-9]"
        Assert.That(report, Does.Match(marker))
    finally Directory.Delete(directory,true)

let directConditionFailures = [|
    "10 value%=32000\n20 FOR i%=1 TO 2\n30 IF value%+1000>0 THEN x%=1\n40 END FOR i%\n50 STOP"
    "10 divisor%=0\n20 FOR i%=1 TO 2\n30 IF 1 DIV divisor% THEN x%=1\n40 END FOR i%\n50 STOP"
    "10 divisor%=0\n20 FOR i%=1 TO 2\n30 IF (i%>0) OR (1 DIV divisor%=0) THEN x%=1\n40 END FOR i%\n50 STOP"
    "10 DIM a%(1),indexes%(0):indexes%(0)=3\n20 FOR i%=1 TO 2\n30 IF a%(indexes%(0)) THEN x%=1\n40 END FOR i%\n50 STOP"
|]
let directConditionFailureModes = [|
    for index in 0..directConditionFailures.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"DirectCondition error {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof directConditionFailureModes)>]
let ``DirectCondition preserves overflow division and eager OR array error handling`` (index:int,hir:bool,native:bool,registers:bool) =
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" directConditionFailures[index] (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 4000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

let conditionalForPrograms = [|
    "10 total%=0\n20 FOR i%=-3 TO 3\n30 IF i%<0 THEN total%=total%+1:ELSE total%=total%+10\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "43:3\n"
    "10 total%=0\n20 FOR i%=-2 TO 2\n30 IF i% THEN\n40 IF NOT(i%<0) THEN total%=total%+10:ELSE total%=total%+1\n50 ELSE\n60 total%=total%+100\n70 END IF\n80 END FOR i%\n90 PRINT total%\n100 STOP", "122\n"
    "10 total%=0\n20 FOR i%=-2 TO 2\n30 flag%=(i%>=0) AND (i%<2)\n40 other%=(i%<0) OR (i%>0)\n50 different%=(i%<0) XOR (i%>0)\n60 total%=total%+flag%+other%+different%\n70 END FOR i%\n80 PRINT total%;\":\";flag%;\":\";other%\n90 STOP", "10:0:1\n"
    "10 DIM a%(3):total%=0\n20 FOR i%=1 TO 3\n30 a%(i%)=(i%<>2):total%=total%+a%(i%)\n40 END FOR i%\n50 PRINT total%;\":\";a%(1);\":\";a%(2);\":\";a%(3)\n60 STOP", "2:1:0:1\n"
    "10 x%=0:total%=0\n20 FOR i%=1 TO 3\n30 IF i%=2 THEN x%=7\n40 total%=total%+x%\n50 END FOR i%\n60 PRINT total%;\":\";x%\n70 STOP", "14:7\n"
    "10 total%=0\n20 FOR i%=1 TO 4\n30 IF i%<3 THEN\n40 REMark empty true arm exercises the improver's opposite branch\n50 ELSE\n60 total%=total%+i%\n70 END IF\n80 END FOR i%\n90 PRINT total%\n100 STOP", "7\n"
    "10 total%=0\n20 FOR i%=-2 TO 2\n30 flag%=NOT i%\n40 IF i% AND -3 THEN total%=total%+10\n50 total%=total%+flag%\n60 END FOR i%\n70 PRINT total%\n80 STOP", "41\n"
    "10 i%=77:work\n20 PRINT i%;\":\";total%\n30 STOP\n100 DEFine PROCedure work\n110 LOCal i%,limit%,stride%\n120 limit%=4:stride%=1:total%=0\n130 FOR i%=1 TO limit% STEP stride%\n140 IF i%=2 THEN limit%=-1:stride%=-1\n150 IF i%<=2 THEN total%=total%+i%:ELSE total%=total%+10*i%\n160 END FOR i%\n170 END DEFine", "77:73\n"
    "10 value%=0:work value%\n20 PRINT value%\n30 STOP\n100 DEFine PROCedure work(p%)\n110 LOCal i%\n120 FOR i%=1 TO 4\n130 IF i%<3 THEN p%=p%+i%:ELSE p%=p%+10\n140 END FOR i%\n150 END DEFine", "23\n"
    "10 flag=0\n20 FOR i%=1 TO 3\n30 IF i%>1.5 THEN flag=flag+1\n40 END FOR i%\n50 PRINT flag\n60 STOP", "2\n"
|]
let conditionalForModes = [|
    for index in 0..conditionalForPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"ConditionalFor execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof conditionalForModes)>]
let ``ConditionalFor integer comparisons logic and joins preserve QL values`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=conditionalForPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 4000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

let conditionalFailures = [|
    "10 divisor%=0\n20 FOR i%=1 TO 2\n30 IF (i%=0) AND (1 DIV divisor%=0) THEN x%=1\n40 END FOR i%\n50 STOP"
    "10 value%=32000\n20 FOR i%=1 TO 2\n30 IF (i%=0) AND (value%+1000>0) THEN x%=1\n40 END FOR i%\n50 STOP"
    "10 DIM a%(1),indexes%(0):indexes%(0)=3\n20 FOR i%=0 TO 1\n30 IF (i%=99) AND (a%(indexes%(0))=0) THEN x%=1\n40 END FOR i%\n50 STOP"
|]
let conditionalFailureModes = [|
    for index in 0..conditionalFailures.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"ConditionalFor error {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof conditionalFailureModes)>]
let ``ConditionalFor eager conditions retain checked arithmetic and bounds errors`` (index:int,hir:bool,native:bool,registers:bool) =
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" conditionalFailures[index] (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 4000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

[<TestCase(0,1)>]
[<TestCase(1,1)>]
[<TestCase(2,1)>]
[<TestCase(3,1)>]
[<TestCase(4,1)>]
[<TestCase(5,1)>]
[<TestCase(6,1)>]
[<TestCase(7,1)>]
[<TestCase(8,0)>]
[<TestCase(9,0)>]
let ``ConditionalFor reports select integer control flow and preserve conservative fallbacks`` (index:int,loops:int) =
    let directory=Path.Combine(Path.GetTempPath(),$"conditional-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"if")
        let source,expected=conditionalForPrograms[index]
        withCompiledProgramMode true ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable ->
            Assert.That((run 4000000L executable).Output, Is.EqualTo(expected)))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain($"Native integer FOR loops: selected={loops};"))
        if loops>0 then Assert.That(report, Does.Contain("conditional body:"))
    finally Directory.Delete(directory,true)
let localForModes = [|
    for index in 0..localForPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"LocalFor execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof localForModes)>]
let ``LocalFor certificates preserve bindings recursion captured bounds and alias fallbacks`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=localForPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 3000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output, Is.EqualTo(expected)))

[<TestCase(0,1)>]
[<TestCase(2,1)>]
[<TestCase(3,1)>]
[<TestCase(4,0)>]
[<TestCase(5,0)>]
[<TestCase(6,1)>]
[<TestCase(7,1)>]
[<TestCase(8,2)>]
let ``LocalFor plans select certified loops and retain alias barriers`` (index:int,expectedLoops:int) =
    let directory=Path.Combine(Path.GetTempPath(),$"local-for-report-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"loops")
        let source,expected=localForPrograms[index]
        let capture registers=withCompiledProgramMode true ["--native-regions";(if registers then "--regalloc" else "--no-regalloc");"--dump-hir="+prefix] ".bas" source (fun executable -> run 3000000L executable)
        let before=capture false
        let after=capture true
        Assert.That(after.Output, Is.EqualTo(expected))
        Assert.That(after.Output, Is.EqualTo(before.Output))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain($"Native integer FOR loops: selected={expectedLoops};"))
        if expectedLoops>0 then
            Assert.That(report, Does.Contain("certified binding=True"))
    finally Directory.Delete(directory,true)

[<TestCase(false)>]
[<TestCase(true)>]
let ``LocalFor error handlers retain the runtime path and restore LOCAL bindings`` (hir:bool) =
    let source="10 i%=77\n20 WHEN ERROR\n30 PRINT ERNUM;\":\";i%:STOP\n60 END WHEN\n70 work\n80 STOP\n100 DEFine PROCedure work\n110 LOCal i%,x%\n120 x%=32000\n130 FOR i%=1 TO 10\n140 x%=x%+1000\n150 END FOR i%\n160 END DEFine"
    withCompiledProgramMode hir [] ".bas" source (fun executable ->
        let result=run 3000000L executable
        Assert.That(result.Output, Is.EqualTo("-18:77\n")))

let divmodPrograms = [|
    "10 a%=-7:b%=3\n20 q%=a% DIV b%:r%=a% MOD b%\n30 PRINT q%;\":\";r%;\":\";q%*b%+r%\n40 a%=7:b%=-3\n50 q%=a% DIV b%:r%=a% MOD b%\n60 PRINT q%;\":\";r%;\":\";q%*b%+r%\n70 STOP", "-3:2:-7\n-3:-2:7\n"
    "10 q%=0:r%=0\n20 FOR i%=-7 TO 7 STEP 2\n30 q%=q%+(i% DIV 3):r%=r%+(i% MOD 3)\n40 END FOR i%\n50 PRINT q%;\":\";r%;\":\";i%\n60 STOP", "-3:9:7\n"
    "10 q%=0:r%=0:limit%=-7:stride%=-2:divisor%=-3\n20 FOR i%=7 TO limit% STEP stride%\n30 limit%=100:stride%=1:q%=q%+(i% DIV divisor%):r%=r%+(i% MOD divisor%)\n40 END FOR i%\n50 PRINT q%;\":\";r%;\":\";i%\n60 STOP", "-3:-9:-7\n"
    "10 a%=-7:work a%,a%\n20 PRINT a%\n30 STOP\n100 DEFine PROCedure work(p%,q%)\n110 p%=p% DIV 3:q%=q% MOD 3\n120 END DEFine", "0\n"
    "10 a%=1:b%=2:c%=3:d%=4:e%=5:f%=6:g%=-7:h%=3\n20 q%=a%+(b%+(c%+(d%+(e%+(f%+(g% DIV h%))))))\n30 r%=a%+(b%+(c%+(d%+(e%+(f%+(g% MOD h%))))))\n40 PRINT q%;\":\";r%\n50 STOP", "18:23\n"
    "10 FOR i%=-32768 TO -32766\n20 q%=i% DIV 1:r%=i% MOD 1\n30 END FOR i%\n40 PRINT q%;\":\";r%;\":\";i%\n50 STOP", "-32766:0:-32766\n"
    "10 WHEN ERROR\n20 PRINT ERNUM;\":\";answer%\n30 STOP\n40 END WHEN\n50 a%=-32768:b%=-1:answer%=42\n60 answer%=a% DIV b%\n70 PRINT \"wrong\"\n80 STOP", "-18:42\n"
    "10 WHEN ERROR\n20 count%=count%+1\n30 IF count%<3 THEN RETRY\n40 PRINT ERNUM;\":\";count%;\":\";answer%\n50 STOP\n60 END WHEN\n70 a%=1:b%=2:c%=3:d%=4:e%=5:f%=6:g%=7:h%=0:answer%=42\n80 answer%=a%+(b%+(c%+(d%+(e%+(f%+(g% MOD h%))))))\n90 PRINT \"wrong\"", "-18:3:42\n"
    "10 WHEN ERROR\n20 count%=count%+1\n30 IF count%<3 THEN RETRY\n40 PRINT ERNUM;\":\";count%;\":\";answer%\n50 STOP\n60 END WHEN\n70 answer%=42\n80 answer%=bad%(-32768)\n90 PRINT \"wrong\"\n100 DEFine FuNction bad%(n%)\n110 LOCal a%,b%,c%,d%,e%,f%,den%\n120 a%=1:b%=2:c%=3:d%=4:e%=5:f%=6:den%=-1\n130 RETurn a%+(b%+(c%+(d%+(e%+(f%+(n% DIV den%))))))\n140 END DEFine", "-18:3:42\n"
|]
let divmodModes = [|
    for index in 0..divmodPrograms.Length-1 do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(index,hir,native,registers).SetName($"DIVMOD execution {index} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof divmodModes)>]
let ``allocated DIVMOD preserves signed results loops aliases retries and local unwinding`` (index:int,hir:bool,native:bool,registers:bool) =
    let source,expected=divmodPrograms[index]
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let result=run 2000000L executable
        Assert.That(result.Output, Is.EqualTo(expected))
        Assert.That(result.ExitCode, Is.Zero))

[<TestCase(false)>]
[<TestCase(true)>]
let ``DIVMOD snapshots are selected before RETRY`` (hir:bool) =
    let directory=Path.Combine(Path.GetTempPath(),$"divmod-retry-{Guid.NewGuid():N}")
    Directory.CreateDirectory directory |> ignore
    try
        let prefix=Path.Combine(directory,"checked")
        let source,expected=divmodPrograms[7]
        withCompiledProgramMode hir ["--native-regions";"--regalloc";"--dump-hir="+prefix] ".bas" source (fun executable ->
            Assert.That((run 2000000L executable).Output, Is.EqualTo(expected)))
        let report=File.ReadAllText(prefix+".native-ir.txt")
        Assert.That(report, Does.Contain("allocated: Division"))
        Assert.That(report, Does.Match("spill slots=[1-9]"))
    finally Directory.Delete(directory,true)

let divmodLoopFailures = [|
    for expression,first in ["i% DIV 0",1;"i% MOD 0",1;"i% DIV -1",-32768;"i% MOD -1",-32768] do
        for hir in [false;true] do
            for native,registers in [false,false;true,false;true,true] do
                yield TestCaseData(expression,first,hir,native,registers).SetName($"DIVMOD loop failure {expression} (HIR {hir}, native {native}, registers {registers})") |]

[<TestCaseSource(nameof divmodLoopFailures)>]
let ``native loop DIVMOD failures use the runtime error path after releasing all frames`` (expression:string,first:int,hir:bool,native:bool,registers:bool) =
    let source = $"10 answer%%=42\n20 FOR i%%={first} TO {first+1}\n30 answer%%={expression}\n40 END FOR i%%\n50 PRINT \"wrong\"\n60 STOP"
    withCompiledProgramMode hir [(if native then "--native-regions" else "--no-native-regions");(if registers then "--regalloc" else "--no-regalloc")] ".bas" source (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:")))

[<TestCase(8,false)>]
[<TestCase(8,true)>]
[<TestCase(20,false)>]
[<TestCase(20,true)>]
let ``allocated DIVMOD CONTINUE keeps fatal error propagation`` (opcode:int,implicitContinue:bool) =
    let handler=if implicitContinue then "20 PRINT ERNUM" else "20 PRINT ERNUM:CONTINUE"
    let operation=if opcode=8 then "DIV" else "MOD"
    let source = $"10 WHEN ERROR\n{handler}\n30 END WHEN\n40 a%%=-32768:b%%=-1:c%%=3:d%%=4\n50 c%%=d%%+(c%%+(a%% {operation} b%%))\n60 PRINT \"wrong\""
    withCompiledProgramMode true ["--native-regions";"--regalloc"] ".bas" source (fun executable ->
        let error=Assert.Throws<M68kFault>(fun () -> run 2000000L executable |> ignore)
        Assert.That(error.Message, Does.Contain("Runtime error message:"))
        Assert.That(error.Message, Does.Contain("output=-18\n")))

let nativeForPrograms = [|
    "10 total%=0\n20 FOR i%=7 TO -7 STEP -3\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "5:-5\n"
    "10 last%=99\n20 FOR i%=0 TO -1 STEP -32768\n30 last%=i%\n40 END FOR i%\n50 PRINT last%;\":\";i%\n60 STOP", "0:0\n"
    "10 total%=0\n20 FOR i%=3 TO 2 STEP 0\n30 total%=total%+i%\n40 END FOR i%\n50 PRINT total%;\":\";i%\n60 STOP", "0:3\n"
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
