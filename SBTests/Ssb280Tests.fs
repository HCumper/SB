module SBTests.Ssb280Tests

open System
open System.IO
open System.Text.Json
open NUnit.Framework
open Serilog
open CompilerPipeline
open SSB

let private fixtures = Path.Combine(__SOURCE_DIRECTORY__, "Fixtures", "Ssb280")
let private scratch = Path.Combine(__SOURCE_DIRECTORY__, "..", "artifacts", "ssb280-tests") |> Path.GetFullPath

let private withSource (source: string) action =
    Directory.CreateDirectory scratch |> ignore
    let path = Path.Combine(scratch, "test.ssb")
    File.WriteAllText(path, source)
    File.Copy(Path.Combine(fixtures, "types.ssb"), Path.Combine(scratch, "types_ssb"), true)
    action path

let private withEnvironment name value action =
    let previous = Environment.GetEnvironmentVariable name
    Environment.SetEnvironmentVariable(name, value)
    try action ()
    finally Environment.SetEnvironmentVariable(name, previous)

let private settings input =
    { InputFileName = input; OutputFileName = ""; Verbose = false; Backend = "interpret"; RuntimeHost = "console"
      ExecutionThrottleEnabled = false; ExecutionThrottleTargetStatementsPerSecond = 10000
      ExecutionThrottleMaxRunAheadMilliseconds = 2; SyntaxChecking = Rigorous; AppName = "Test"
      Logger = LoggerConfiguration().CreateLogger() }

let private run source =
    withSource source (fun path ->
        match loadAstFromInput (settings path) with
        | Error error -> failwithf "Parse failed: %A\n%s" error (Ssb.transformFile path)
        | Ok(_, _, ast) ->
            match lowerAstForRuntime ast with
            | Error error -> failwithf "Lowering failed: %s\n%s" error (Ssb.transformFile path)
            | Ok hir ->
                match Interpreter.interpretProgram hir with
                | Error error -> failwithf "Execution failed: %A\n%s" error (Ssb.transformFile path)
                | Ok result ->
                    // SB's PRINT host currently separates every argument with a
                    // space, including semicolon arguments. Compare the values
                    // independently of that existing console-formatting policy.
                    String.concat "\n" result.Output |> fun value -> value.Replace(" , ", ",").Replace(" | ", "|"))

// Upstream fixtures were written for QDOS file channels. Redirect their result
// channel to the console, retaining all language operations and expected values.
let private portableFixture name =
    File.ReadAllLines(Path.Combine(fixtures, name + ".ssb"))
    |> Array.filter (fun line ->
        not (line.Contains("win1_done")) &&
        not (line.Trim().Equals("OPEN_NEW #9,\"win1_result\"", StringComparison.OrdinalIgnoreCase)) &&
        not (line.Trim().Equals("CLOSE #9", StringComparison.OrdinalIgnoreCase)))
    |> Array.map (fun line -> line.Replace("OPEN_NEW #9,\"win1_result\":", "").Replace("PRINT #9,", "PRINT ").Replace(":CLOSE #9", ""))
    |> String.concat "\n"

[<TestCase("core", "4,3\n4,3\n5")>]
[<TestCase("arrays-pools", "Lau,22\n91,clinical\n3,4\n2,11,12\n1,2,1,0\n6\n5,6")>]
[<TestCase("value-only", "9")>]
[<TestCase("limits", "1,64")>]
[<TestCase("strings", "180,z\n137.5")>]
[<TestCase("parenthesized-value", "3,3,9,5,keep,3")>]
[<TestCase("review-regressions", "## one|## two|## three\nHe said \" ## here\nIt's ## here\nHe said \" ** here\nIt's ** here\n3,9")>]
[<TestCase("reference-stream", "9,hello,16,18")>]
[<TestCase("manifest-records", "7,a:b\nkept")>]
[<TestCase("bounds-labels", "0,0,0\n3,0\n7\n1,1,0\nmail@example: TYPE AS VAL ## untouched\n132\n11,42,1,0")>]
[<TestCase("pure-builtins", "506")>]
[<TestCase("standalone-colon", "2")>]
let ``upstream programs execute through the SB pipeline`` (name: string) (expected: string) =
    Assert.That(run (portableFixture name), Is.EqualTo(expected))

[<Test>]
let ``SBASIC profile supports selected scalar record arguments`` () =
    withEnvironment "SSBRECPROFILE" "SBASIC" (fun () ->
        Assert.That(run (portableFixture "element-reference"), Is.EqualTo("2,11,0"))
        Assert.That(run (portableFixture "pure-element-selectors"), Is.EqualTo("11,11,11")))

[<Test>]
let ``SELECT preserves string selectors and exact numeric matches`` () =
    let source = "TYPE p\n name$(8)\n x\nEND TYPE\nDIM v AS p\nv.name$=\"hello\"\nv.x=3\nSELect ON v.name$\n=\"hello\":PRINT \"string\"\n=REMAINDER:PRINT \"wrong\"\nEND SELect\nSELect ON v.x\n=4:PRINT \"wrong\"\n=3:PRINT \"number\"\n=REMAINDER:PRINT \"wrong\"\nEND SELect\n"
    Assert.That(run source, Is.EqualTo("string\nnumber"))

[<Test>]
let ``explicit SELECT ON arms retain record operations inside the branch`` () =
    let source = "TYPE p\n x\nEND TYPE\nDIM v AS p,w AS p\nv.x=3\nSELect ON v.x\nON v.x=1:w=v\nON v.x=2 TO 4:w=v\nON v.x=REMAINDER:w.x=99\nEND SELect\nPRINT w.x\n"
    Assert.That(run source, Is.EqualTo("3"))

[<Test>]
let ``fractional runtime record bounds are rejected before allocation`` () =
    let source = "TYPE p\n x\nEND TYPE\nn=1.5\nDIM values(n) AS p\nPRINT \"wrong\"\n"
    Assert.That(run source, Is.EqualTo("Invalid record array bound"))

[<Test>]
let ``record functions retain writable scalar references`` () =
    let source = "TYPE p\n x\nEND TYPE\nDIM v AS p\nv.x=3\nanswer=change(v)\nPRINT v.x;\",\";answer\nSTOP\nDEFine FuNction change(arg AS p)\narg.x=8\nRETurn arg.x+1\nEND DEFine\n"
    Assert.That(run source, Is.EqualTo("8,9"))

[<TestCase("Record pool exhausted", "a%=nodes_new%:b%=nodes_new%")>]
[<TestCase("Record pool double free", "a%=nodes_new%:nodes_free a%:nodes_free a%")>]
[<TestCase("Invalid pool handle", "nodes_free 2")>]
let ``pool misuse fails predictably`` (expected: string) (body: string) =
    let source = "TYPE p\n x\nEND TYPE\nDIM nodes(1) AS p POOL\n" + body + "\nPRINT \"wrong\"\nSTOP\n"
    let error = Assert.Throws<Exception>(fun () -> run source |> ignore)
    Assert.That(error.Message, Does.Contain("EscapedStop"))
    Assert.That(error.Message, Does.Contain(expected))

[<Test>]
let ``multiline record array field loop counters are rejected`` () =
    withSource "TYPE p\n x\nEND TYPE\nDIM rows(2) AS p\nFOR rows(1).x=1 TO 2\nEND FOR rows\n" (fun path ->
        Assert.Throws<Ssb.FatalSsbError>(fun () -> Ssb.transformFile path |> ignore) |> ignore)

[<Test>]
let ``REFERENCE in a routine body retains scalar reference semantics`` () =
    let source = "TYPE p\n x\nEND TYPE\nDIM v AS p\nn%=3\nchange n%\nPRINT n%\nSTOP\nDEFine PROCedure change(arg%)\nREFERENCE arg%\narg%=9\nEND DEFine\n"
    Assert.That(run source, Is.EqualTo("9"))

[<TestCase("TYPE p\n x\nEND TYPE\nDIM v AS p\nv.x.bad=1\n", "Unknown field")>]
[<TestCase("TYPE p\n x\nEND TYPE\nDIM v AS p\nPRINT getValue().x\nDEFine FuNction getValue\nRETurn 1\nEND DEFine\n", "Functions cannot return records")>]
[<TestCase("TYPE p\n x\nEND TYPE\nDEFine PROCedure create\nDIM v(2) AS p POOL\nEND DEFine\n", "top-level")>]
let ``invalid forms are rejected before relaxed parser recovery`` (source: string) (message: string) =
    withSource source (fun path ->
        let error = Assert.Throws<Ssb.FatalSsbError>(fun () -> Ssb.transformFile path |> ignore)
        Assert.That(error.Message, Does.Contain(message)))

[<Test>]
let ``record calls in larger expressions do not reorder effectful functions`` () =
    withEnvironment "SSBRECPROFILE" "SBASIC" (fun () ->
        withSource "TYPE p\n x\nEND TYPE\nDIM rows(2) AS p\ni=1\nx=RND+readPoint(rows(i))\nDEFine FuNction readPoint(arg AS p)\nRETurn arg.x\nEND DEFine\n" (fun path ->
            let error = Assert.Throws<Ssb.FatalSsbError>(fun () -> Ssb.transformFile path |> ignore)
            Assert.That(error.Message, Does.Contain("Split effectful"))))

[<TestCase("long-lines-legacy")>]
[<TestCase("long-lines-records")>]
let ``upstream long logical lines survive preprocessing`` (name: string) =
    withSource (File.ReadAllText(Path.Combine(fixtures, name + ".ssb"))) (fun path ->
        let output = Ssb.transformFile path
        Assert.That(output, Does.Contain(String.replicate 900 "x")))

[<Test>]
let ``upstream long line programs retain every literal character`` () =
    use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(fixtures, "long-lines.json")))
    for fixture in document.RootElement.EnumerateArray() do
        let name = fixture.GetProperty("name").GetString()
        // The third upstream case tests extracted QL translator subroutines;
        // these two exercise the source-language preprocessing implemented here.
        if name = "long-lines-legacy" || name = "long-lines-records" then
            let expected = fixture.GetProperty("expected").GetString()
            Assert.That((run (portableFixture name)).Replace(": ", ":"), Is.EqualTo(expected), name)

let rejectionCases =
    use document = JsonDocument.Parse(File.ReadAllText(Path.Combine(fixtures, "rejections.json")))
    document.RootElement.EnumerateArray()
    |> Seq.map (fun item -> TestCaseData(item.GetProperty("source").GetString(), item.GetProperty("error").GetString()).SetName("SSB 2.8 rejects " + item.GetProperty("name").GetString()))
    |> Seq.toArray

[<TestCaseSource(nameof rejectionCases)>]
let ``invalid record programs fail before numbering`` (source: string) (expected: string) =
    withSource source (fun path ->
        let error = Assert.Throws<Ssb.FatalSsbError>(fun () -> Ssb.transformFile path |> ignore)
        Assert.That(error.Message.ToLowerInvariant(), Does.Contain(expected.ToLowerInvariant())))

[<Test>]
let ``preserved blank lines and tabs do not skew labels`` () =
    withEnvironment "SSBIGNORE_BLANKS" "N" (fun () ->
        Assert.That(run (portableFixture "preserved-blank-labels"), Is.EqualTo("0,7")))

[<Test>]
let ``numbering accepts the last legal line and rejects an actual overflow`` () =
    withEnvironment "SSBSTART" "32767" (fun () ->
        withSource "PRINT 1\n## trailing comment\n" (fun path -> Assert.That(Ssb.transformFile path, Does.StartWith("32767 PRINT 1")))
        withSource "PRINT 1\nPRINT 2\n" (fun path -> Assert.Throws<Ssb.FatalSsbError>(fun () -> Ssb.transformFile path |> ignore) |> ignore))
    withEnvironment "SSBINC" "0" (fun () ->
        withSource "PRINT 1" (fun path -> Assert.Throws<Ssb.FatalSsbError>(fun () -> Ssb.transformFile path |> ignore) |> ignore))

[<Test>]
let ``labels in strings data and remarks are not substituted`` () =
    withSource "PRINT \"mail@example\"\nDATA \"@unknown\"\nREMark @unknown\n" (fun path ->
        let output = Ssb.transformFile path
        Assert.That(output, Does.Contain("mail@example"))
        Assert.That(output, Does.Contain("DATA \"@unknown\"")))

[<Test>]
let ``inactive nested includes are skipped and directives balance per file`` () =
    withSource "#IFDEF missing\n#IFDEF nested\n#ENDIF\n#INCLUDE missing\n#ENDIF\nPRINT 7\n" (fun path ->
        Assert.That(Ssb.transformFile path, Does.Contain("PRINT 7")))
    withSource "#ENDIF\n" (fun path -> Assert.Throws<Ssb.FatalSsbError>(fun () -> Ssb.transformFile path |> ignore) |> ignore)

[<Test>]
let ``failed standalone translation preserves existing output`` () =
    withEnvironment "SSBWORK" (scratch + string Path.DirectorySeparatorChar) (fun () ->
        withEnvironment "SSBOVER" "Y" (fun () ->
            Directory.CreateDirectory scratch |> ignore
            File.WriteAllText(Path.Combine(scratch, "safe_ssb"), "GOTO @missing")
            File.WriteAllText(Path.Combine(scratch, "safe_bas"), "original")
            Assert.Throws<Ssb.FatalSsbError>(fun () -> Ssb.run [| "safe" |]) |> ignore
            Assert.That(File.ReadAllText(Path.Combine(scratch, "safe_bas")), Is.EqualTo("original"))))
