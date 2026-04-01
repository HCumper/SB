module SBTests.PipelineSourceTests

open System
open System.IO
open NUnit.Framework
open Serilog
open Serilog.Core
open Serilog.Events

open CompilerPipeline
open SSB
open SyntaxAst

let private fixturePath fileName =
    let candidates =
        [ Path.Combine(__SOURCE_DIRECTORY__, "..", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", fileName)
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", Path.GetFileName(fileName)) ]
        |> List.map Path.GetFullPath

    match candidates |> List.tryFind File.Exists with
    | Some path -> path
    | None -> failwith $"Fixture file not found: {fileName}"

let private testSettings inputFile =
    { InputFileName = inputFile
      OutputFileName = Path.GetTempFileName()
      Verbose = false
      Backend = "interpret"
      RuntimeHost = "console"
      SyntaxChecking = Relaxed
      AppName = "Test"
      Logger = LoggerConfiguration().CreateLogger() }

let private testSettingsWithSyntaxChecking syntaxChecking inputFile =
    { testSettings inputFile with
        SyntaxChecking = syntaxChecking }

type private CollectingSink() =
    let messages = ResizeArray<string>()

    member _.Messages = messages |> Seq.toList

    interface ILogEventSink with
        member _.Emit(logEvent: LogEvent) =
            messages.Add(logEvent.RenderMessage())

let private withTempSource (content: string) action =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.sb")
    File.WriteAllText(path, content)
    try
        action path
    finally
        if File.Exists(path) then
            File.Delete(path)

let private withTempDirectory action =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}")
    Directory.CreateDirectory(path) |> ignore
    try
        action path
    finally
        if Directory.Exists(path) then
            Directory.Delete(path, true)

[<Test>]
let ``loadAstFromInput preprocesses ssb source before parsing`` () =
    withTempSource "PRINT 1\n" (fun path ->
        match loadAstFromInput (testSettings path) with
        | Error err -> Assert.Fail($"Expected transformed SSB input to parse successfully, got %A{err}")
        | Ok(_, _, Program(_, [ Line(_, Some 1000, [ ProcedureCall(_, "PRINT", [ NumberLiteral(_, _, "1") ]) ]) ])) -> ()
        | Ok(_, _, ast) -> Assert.Fail($"Unexpected AST for transformed SSB input: %A{ast}")
    )

[<Test>]
let ``loadAstFromInput verbose ssb prints preprocessed superbasic`` () =
    withTempSource "PRINT 1\n" (fun path ->
        let originalOut = Console.Out
        let writer = new StringWriter()
        Console.SetOut(writer)
        try
            let settings =
                { testSettings path with
                    Verbose = true }

            match loadAstFromInput settings with
            | Error err -> Assert.Fail($"Expected transformed SSB input to parse successfully, got %A{err}")
            | Ok _ ->
                let output = writer.ToString()
                Assert.That(output, Does.Contain("Preprocessed SuperBASIC:"))
                Assert.That(output, Does.Contain("1000 PRINT 1"))
        finally
            Console.SetOut(originalOut)
    )

[<Test>]
let ``loadAstFromInput keeps numbered superbasic source unchanged`` () =
    withTempSource "10 PRINT 1\n" (fun path ->
        match loadAstFromInput (testSettings path) with
        | Error err -> Assert.Fail($"Expected numbered SuperBasic input to parse successfully, got %A{err}")
        | Ok(_, _, Program(_, [ Line(_, Some 10, [ ProcedureCall(_, "PRINT", [ NumberLiteral(_, _, "1") ]) ]) ])) -> ()
        | Ok(_, _, ast) -> Assert.Fail($"Unexpected AST for numbered SuperBasic input: %A{ast}")
    )

[<Test>]
let ``ssb272 fixture is classified as ssb source`` () =
    let lines =
        File.ReadAllLines(fixturePath "ssb272.ssb")
        |> Array.toList

    Assert.That(Ssb.classifySource lines, Is.EqualTo(Ssb.SourceKind.Ssb))

[<Test>]
let ``transformFile preprocesses ssb272 fixture when include stubs are available`` () =
    withTempDirectory (fun dir ->
        let sourcePath = Path.Combine(dir, "ssb272.ssb")
        File.Copy(fixturePath "ssb272.ssb", sourcePath)

        for includeName in [ "ram1_ssblang_ssb"; "ram1_ssbpass1_ssb"; "ram1_ssbpass2_ssb" ] do
            File.WriteAllText(Path.Combine(dir, includeName), "")

        let originalCurrentDirectory = Directory.GetCurrentDirectory()
        Directory.SetCurrentDirectory(dir)
        try
            let transformed = Ssb.transformFile sourcePath
            Assert.That(transformed, Does.StartWith("1000 TURBO_taskn \"ssb 2.7.2\""))
            Assert.That(transformed, Does.Contain("DEF PROCedure abort_out"))
            Assert.That(transformed, Does.Not.Contain("#INCLUDE ram1_ssblang_ssb"))
        finally
            Directory.SetCurrentDirectory(originalCurrentDirectory)
    )

[<Test>]
let ``loadAstFromInput does not crash on ssb272 recovery parse`` () =
    withTempDirectory (fun dir ->
        let sourcePath = Path.Combine(dir, "ssb272.ssb")
        File.Copy(fixturePath "ssb272.ssb", sourcePath)

        for includeName in [ "ram1_ssblang_ssb"; "ram1_ssbpass1_ssb"; "ram1_ssbpass2_ssb" ] do
            File.WriteAllText(Path.Combine(dir, includeName), "")

        let originalCurrentDirectory = Directory.GetCurrentDirectory()
        Directory.SetCurrentDirectory(dir)
        try
            let mutable result = None
            Assert.DoesNotThrow(fun () -> result <- Some(loadAstFromInput (testSettings sourcePath)))

            match result with
            | Some(Ok(_, _, Program(_, lines))) -> Assert.That(lines, Is.Not.Empty)
            | Some(Error err) -> Assert.Fail($"Expected visitor construction to complete, got %A{err}")
            | None -> Assert.Fail("Expected pipeline result to be captured.")
        finally
            Directory.SetCurrentDirectory(originalCurrentDirectory)
    )

[<Test>]
let ``loadAstFromInput relaxed syntax checking keeps parser recovery behavior`` () =
    withTempSource "10 FOR i = 1 TO 2\n20 PRINT i\n30 END\n" (fun path ->
        match loadAstFromInput (testSettingsWithSyntaxChecking Relaxed path) with
        | Error err -> Assert.Fail($"Expected relaxed syntax checking to allow parser recovery, got %A{err}")
        | Ok(_, _, Program(_, lines)) -> Assert.That(lines, Is.Not.Empty)
    )

[<Test>]
let ``loadAstFromInput rigorous syntax checking reports parse errors`` () =
    withTempSource "10 FOR i = 1 TO 2\n20 PRINT i\n30 END\n" (fun path ->
        match loadAstFromInput (testSettingsWithSyntaxChecking Rigorous path) with
        | Error(ParseError message) -> Assert.That(message, Does.Contain("line"))
        | Error(FileNotFound fileName) -> Assert.Fail($"Expected parse failure, got missing file %s{fileName}")
        | Ok _ -> Assert.Fail("Expected rigorous syntax checking to reject parser recovery input.")
    )

[<Test>]
let ``loadAstFromInput rigorous syntax checking ignores eof only parser noise`` () =
    withTempSource "10 PRINT 1" (fun path ->
        match loadAstFromInput (testSettingsWithSyntaxChecking Rigorous path) with
        | Error err -> Assert.Fail($"Expected EOF-only parser noise to be ignored, got %A{err}")
        | Ok(_, _, Program(_, [ Line(_, Some 10, [ ProcedureCall(_, "PRINT", [ NumberLiteral(_, _, "1") ]) ]) ])) -> ()
        | Ok(_, _, ast) -> Assert.Fail($"Unexpected AST for rigorous EOF-only input: %A{ast}")
    )

[<Test>]
let ``loadAstFromInput relaxed syntax checking logs parser recovery warnings`` () =
    withTempSource "10 FOR i = 1 TO 2\n20 PRINT i\n30 END\n" (fun path ->
        let sink = CollectingSink()
        let logger = LoggerConfiguration().WriteTo.Sink(sink).CreateLogger()
        let settings =
            { testSettingsWithSyntaxChecking Relaxed path with
                Logger = logger }

        match loadAstFromInput settings with
        | Error err -> Assert.Fail($"Expected relaxed syntax checking to recover, got %A{err}")
        | Ok _ ->
            Assert.That(sink.Messages, Has.Some.Contains("line 3:6 missing 'FOR'"))
    )

[<Test>]
let ``loadAstFromInput relaxed syntax checking rejects out of sequence top level line numbers`` () =
    withTempSource "20 PRINT 2\n10 PRINT 1\n" (fun path ->
        match loadAstFromInput (testSettingsWithSyntaxChecking Relaxed path) with
        | Error(ParseError message) -> Assert.That(message, Does.Contain("Out-of-sequence line number 10 after 20"))
        | Error(FileNotFound fileName) -> Assert.Fail($"Expected line sequence error, got missing file %s{fileName}")
        | Ok _ -> Assert.Fail("Expected relaxed mode to reject out-of-sequence line numbers.")
    )

[<Test>]
let ``loadAstFromInput relaxed syntax checking rejects out of sequence routine body line numbers`` () =
    withTempSource "10 DEFine PROCedure demo\n30 PRINT 3\n20 PRINT 2\n40 END DEFine\n" (fun path ->
        match loadAstFromInput (testSettingsWithSyntaxChecking Relaxed path) with
        | Error(ParseError message) -> Assert.That(message, Does.Contain("Out-of-sequence line number 20 after 30 in routine 'demo'"))
        | Error(FileNotFound fileName) -> Assert.Fail($"Expected line sequence error, got missing file %s{fileName}")
        | Ok _ -> Assert.Fail("Expected relaxed mode to reject out-of-sequence routine line numbers.")
    )

[<Test>]
let ``loadAstFromInput relaxed syntax checking accepts compact select clauses with comma separated matches`` () =
    withTempSource "10 REPeat keyLoop\n20 SELect ON keyCode\n30 =32,200\n40 PRINT keyCode\n50 =192\n60 PRINT 0\n70 END SELect\n80 END REPeat keyLoop\n" (fun path ->
        match loadAstFromInput (testSettingsWithSyntaxChecking Relaxed path) with
        | Error(ParseError message) -> Assert.Fail($"Expected compact SELECT comma clauses to preserve line ordering, got %s{message}")
        | Error(FileNotFound fileName) -> Assert.Fail($"Expected parsed source, got missing file %s{fileName}")
        | Ok(_, _, Program(_, lines)) -> Assert.That(lines, Is.Not.Empty)
    )

[<Test>]
let ``loadAstFromInput treats open device names as string literals`` () =
    withTempSource "10 OPEN #10,con__1\n20 OPEN #5,scr_\n" (fun path ->
        match loadAstFromInput (testSettingsWithSyntaxChecking Relaxed path) with
        | Error err -> Assert.Fail($"Expected OPEN device literals to parse, got %A{err}")
        | Ok(_, _, Program(_, [ Line(_, Some 10, [ ChannelProcedureCall(_, "OPEN", NumberLiteral(_, _, "10"), [ StringLiteral(_, _, "\"con__1\"") ]) ])
                               ; Line(_, Some 20, [ ChannelProcedureCall(_, "OPEN", NumberLiteral(_, _, "5"), [ StringLiteral(_, _, "\"scr_\"") ]) ]) ])) -> ()
        | Ok(_, _, ast) -> Assert.Fail($"Unexpected AST for OPEN device literals: %A{ast}")
    )
