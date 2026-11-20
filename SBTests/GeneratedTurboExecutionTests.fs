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
let private withCompiledProgram extension (source: string) action =
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
        for argument in [ typeof<TurboCodegen.TurboCodegenOutput>.Assembly.Location; input; output; "false"; "turbo" ] do
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

let programs = [|
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
|]

[<TestCaseSource(nameof programs)>]
let ``compiled Turbo program produces its expected output`` (extension: string, source: string, expected: string) =
    withCompiledProgram extension source (fun executable ->
        let result = run 2000000L executable
        Assert.That(result.ExitCode, Is.Zero)
        Assert.That(result.Output.Replace("\r\n", "\n"), Is.EqualTo(expected))
        Assert.That(result.Steps, Is.GreaterThan(0L)))

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
