module TestBed.PipelineSourceTests

open System
open System.IO
open NUnit.Framework
open Serilog

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
      TemplateFileName = "unused"
      Verbose = false
      AppName = "Test"
      Logger = LoggerConfiguration().CreateLogger() }

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
