module SBTests.SsbCorrectnessTests

open System
open System.IO
open NUnit.Framework
open Serilog
open CompilerPipeline
open SSB

let private scratch = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "artifacts", "ssb-correctness-tests"))
let private recordHeader = "TYPE marker\n value\nEND TYPE\n"
let private sourceWithRecords enabled source = (if enabled then recordHeader else "") + source

let private withInput overrides (source: string) action =
    let values = ["SSBSTART", "100"; "SSBINC", "10"; "SSBIGNORE_BLANKS", "Y"; "SSBRECPROFILE", "TURBO509"; "SSBRECITEMS", "256"] @ overrides
    let previous = values |> List.map fst |> List.distinct |> List.map (fun n -> n, Environment.GetEnvironmentVariable n)
    try
        for n,v in values do Environment.SetEnvironmentVariable(n,v)
        Directory.CreateDirectory scratch |> ignore
        let path = Path.Combine(scratch, "regression.ssb")
        File.WriteAllText(path, source)
        action path
    finally
        for n,v in previous do Environment.SetEnvironmentVariable(n,v)

let private settings input =
    { InputFileName = input; OutputFileName = ""; Verbose = false; Backend = "interpret"; RuntimeHost = "console"
      ExecutionThrottleEnabled = false; ExecutionThrottleTargetStatementsPerSecond = 10000
      ExecutionThrottleMaxRunAheadMilliseconds = 2; SyntaxChecking = Rigorous; AppName = "Regression"
      Logger = LoggerConfiguration().CreateLogger() }

let private execute overrides source =
    withInput overrides source (fun path ->
        match loadAstFromInput (settings path) with
        | Error error -> failwithf "Parse failed: %A\n%s" error (Ssb.transformFile path)
        | Ok(_,_,ast) ->
            match lowerAstForRuntime ast with
            | Error error -> failwithf "Lowering failed: %s\n%s" error (Ssb.transformFile path)
            | Ok hir -> Interpreter.interpretProgram hir)

let private run overrides source =
    match execute overrides source with
    | Ok result -> String.concat "\n" result.Output
    | Error error -> failwithf "Execution failed: %A" error

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``inline IF after another statement guards the whole remaining line`` records =
    let source = "n=1 : IF n>10 THEN PRINT \"bad\" : STOP\nPRINT \"ok\"\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ok"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``inline FOR after another statement retains its body`` records =
    let source = "x=0 : FOR i=1 TO 3 : x=x+i\nPRINT x\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("6"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``inline IF inside an inline FOR retains the rest of the loop body`` records =
    let source = "x=0 : FOR i=1 TO 3 : IF i>1 THEN x=x+i : x=x+10\nPRINT x\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("25"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``remark after colon owns its entire tail`` records =
    let source = "a=1 : REMark ratio: 3/4 (@not_a_label)\nPRINT a\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("1"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``ENDIF accepts trailing spaces and tabs`` records =
    let source = "#DEFINE present\n#IFDEF present\nPRINT \"ok\"\n#ENDIF   \t\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ok"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``ordinary parameter and procedure names are not record directives`` records =
    let source = "type\nuseNames 2,3\nSTOP\nDEFine PROCedure type\nPRINT \"ordinary\"\nEND DEFine\nDEFine PROCedure useNames(val,as)\nPRINT val+as\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ordinary\n5"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``DIM followed by another statement remains valid`` records =
    let source = "DIM a(5) : y=7\nPRINT a(5)+y\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("7"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``LOCAL followed by another statement remains valid`` records =
    let source = "work\nSTOP\nDEFine PROCedure work\nLOCal a(5) : y=7\nPRINT a(5)+y\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("7"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``ON GOTO inside SELECT is not a SELECT arm`` records =
    let source = "x=2\nSELect ON x\n=2\nj=1\nON j GO TO @done,@bad\n=REMAINDER:PRINT \"wrong\"\nEND SELect\n@bad\nPRINT \"bad\"\nSTOP\n@done\nPRINT \"ok\"\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ok"))

[<Test; NonParallelizable>]
let ``record expansion preserves explicit and retained blank colon lines`` () =
    let source = recordHeader + "PRINT \"first\"\n\n:\n@done\nPRINT \"last\"\n"
    withInput ["SSBIGNORE_BLANKS","N"] source (fun path ->
        let lines = Ssb.transformFile path |> fun s -> s.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        let colons = lines |> Array.filter (fun s -> s.Substring(s.IndexOf(' ')+1).Trim()=":")
        Assert.That(colons.Length, Is.EqualTo(2)))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``label scanner does not classify remove as REMark`` records =
    let source = "remove=1 : GO TO @done\nPRINT \"bad\"\n@done\nPRINT \"ok\"\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ok"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``label scanner recognises REMark after THEN`` records =
    let source = "IF 0 THEN REMark @not_a_label: still comment\nPRINT \"ok\"\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ok"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``SELECT uses exact matches string ranges and REMAINDER consistently`` records =
    let source = "x=2\nSELect ON x\n=3:PRINT \"wrong\"\n=2:PRINT \"number\"\n=REMAINDER:PRINT \"wrong\"\nEND SELect\ns$=\"m\"\nSELect ON s$\n=\"a\" TO \"f\":PRINT \"wrong\"\n=\"g\" TO \"z\":PRINT \"string\"\n=REMAINDER:PRINT \"wrong\"\nEND SELect\nSELect ON 99\n=1:PRINT \"wrong\"\n=REMAINDER:PRINT \"remainder\"\nEND SELect\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("number\nstring\nremainder"))

[<Test; NonParallelizable>]
let ``record argument captures cannot overwrite a SELECT selector`` () =
    let source = "TYPE point\n x\nEND TYPE\nDIM rows(1) AS point\nrows(1).x=3\ni=1\nn=2\nSELect ON n\n=readPoint(rows(i)):PRINT \"wrong\"\n=2:PRINT \"ok\"\n=REMAINDER:PRINT \"wrong\"\nEND SELect\nSTOP\nDEFine FuNction readPoint(p AS point)\nRETurn p.x\nEND DEFine\n"
    Assert.That(run ["SSBRECPROFILE","SBASIC"] source, Is.EqualTo("ok"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``user function can receive a single string character`` records =
    let source = "w$=\"apple\"\nPRINT isvowel(w$(1))\nPRINT isvowel(w$(2))\nSTOP\nDEFine FuNction isvowel(c$)\nRETurn c$=\"a\"\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("1\n0"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``string character function values cannot mutate the original string`` records =
    let source = "w$=\"apple\"\nPRINT mutate$(w$(1))\nPRINT w$\nSTOP\nDEFine FuNction mutate$(c$)\nc$=\"Z\"\nRETurn c$\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("Z\napple"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``string character cannot supply an explicit REFERENCE function parameter`` records =
    let source = "w$=\"apple\"\nPRINT isvowel(w$(1))\nSTOP\nDEFine FuNction isvowel(c$)\nREFERENCE c$\nRETurn c$=\"a\"\nEND DEFine\n"
    match execute [] (sourceWithRecords records source) with
    | Error error -> Assert.That(error.Code, Is.EqualTo(Interpreter.InvalidReferenceActual))
    | Ok _ -> Assert.Fail("Expected the explicit REFERENCE parameter to reject a character value")

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``parenthesized string character procedure arguments pass their value`` records =
    let source = "w$=\"apple\"\nchange (w$(1))\nPRINT w$\nSTOP\nDEFine PROCedure change(c$)\nc$=\"Z\"\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("apple"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``parenthesized scalar actuals retain value semantics`` records =
    let source = "n=3\ns$=\"keep\"\nchange (n),(s$)\nPRINT n\nPRINT s$\nSTOP\nDEFine PROCedure change(a,b$)\na=99\nb$=\"changed\"\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("3\nkeep"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``a sole parenthesized scalar actual retains value semantics`` records =
    let source = "n=3\nchange (n)\nPRINT n\nSTOP\nDEFine PROCedure change(a)\na=99\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("3"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``SELECT evaluates its selector once`` records =
    let source = "calls=0\nSELect ON nextValue()\n=1:PRINT \"wrong\"\n=2:PRINT \"ok\"\n=REMAINDER:PRINT \"wrong\"\nEND SELect\nPRINT calls\nSTOP\nDEFine FuNction nextValue\ncalls=calls+1\nRETurn 2\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ok\n1"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``SELECT keeps the original selector when a match expression changes it`` records =
    let source = "x=2\nSELect ON x\n=changeValue():PRINT \"wrong\"\n=2:PRINT \"ok\"\n=REMAINDER:PRINT \"wrong\"\nEND SELect\nPRINT x\nSTOP\nDEFine FuNction changeValue\nx=3\nRETurn 3\nEND DEFine\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ok\n3"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``explicit ON SELECT arms retain following lines and ON GOTO`` records =
    let source = "x=2\nSELect ON x\nON x=2\nj=1\nON j GO TO @done,@bad\nON x=REMAINDER:PRINT \"wrong\"\nEND SELect\n@bad\nPRINT \"bad\"\nSTOP\n@done\nPRINT \"ok\"\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("ok"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``numeric looking MANIFEST strings retain string type`` records =
    let source = "MANIFEST : digits$=\"123\" : real$=\"1.5\" : hex$=\"$ff\"\nPRINT digits$ & \"|\" & real$ & \"|\" & hex$\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("123|1.5|$ff"))

[<TestCase(""); TestCase("PRINT \"inside but unrelated\"\n"); NonParallelizable>]
let ``ordinary STOP errors do not append unrelated output`` output =
    let source = "PRINT \"earlier unrelated output\"\nx=halt()\nSTOP\nDEFine FuNction halt\n" + output + "STOP\nEND DEFine\n"
    match execute [] source with
    | Ok _ -> Assert.Fail("Expected STOP to escape the expression-level routine")
    | Error error ->
        Assert.That(error.Message, Does.Not.Contain("unrelated"))
        Assert.That(error.Message, Does.Not.Contain("Last output"))

[<Test; NonParallelizable>]
let ``repeated record declarations still reset array fields`` () =
    let source = "TYPE p\n a(2)\nEND TYPE\nDIM v AS p\nv.a(1)=9\nDIM v AS p\nPRINT v.a(1)\n"
    Assert.That(run [] source, Is.EqualTo("0"))

// SSB output must also compile for the Turbo back end, which types names by suffix and needs
// every line label defined once.
let private turboIr overrides source =
    withInput overrides source (fun path ->
        match loadAstFromInput (settings path) with
        | Error error -> failwithf "Parse failed: %A\n%s" error (Ssb.transformFile path)
        | Ok(_,_,ast) ->
            let state = runSemanticAnalysis ast
            if not state.Errors.IsEmpty then failwith (String.concat "; " state.Errors)
            match runHirLowering state with
            | Error error -> failwithf "Lowering failed: %A" error
            | Ok hir ->
                match HirTurboIrBackend.generateTurboIrFromHir (turboIrOptionsFromState state) hir with
                | Ok output -> output
                | Error errors -> failwith ("Turbo IR generation failed: " + String.concat "; " errors))

let private opcodes (output: HirTurboIrBackend.TurboIrOutput) =
    output.Items |> List.choose (function HirTurboIrBackend.Instruction(opcode, _, _) -> Some opcode | _ -> None)

[<Test; NonParallelizable>]
let ``record array guards compile for Turbo`` () =
    let output = turboIr [] "TYPE point\n x\nEND TYPE\nDIM p(3) AS point\np(1).x=5\nPRINT p(1).x\n"
    Assert.That(opcodes output, Does.Contain(220))

[<Test; NonParallelizable>]
let ``a SELECT range list compiles its body once for Turbo`` () =
    let source = "x=2\nIF x=9 THEN GO TO @inner\nSELect ON x\nON x=1,2\n@inner\nPRINT \"arm\"\nEND SELect\n"
    let output = turboIr [] source
    let armPrints = output.Items |> List.filter (function HirTurboIrBackend.Instruction(169, _, _) -> true | _ -> false)
    Assert.That(armPrints.Length, Is.EqualTo(1))

[<Test; NonParallelizable>]
let ``a string SELECT compares strings for Turbo`` () =
    let output = turboIr [] "a$=\"b\"\nSELect ON a$\n=\"a\":PRINT 1\n=\"b\":PRINT 2\nEND SELect\n"
    // 76 is eq_string; 317 would be the string-to-float conversion of a float selector.
    Assert.That(opcodes output, Does.Contain(76))
    Assert.That(opcodes output, Does.Not.Contain(317))

[<Test; NonParallelizable>]
let ``a SELECT range list runs its body once`` () =
    let source = "x=3\nSELect ON x\nON x=1,3 TO 5\nPRINT \"arm\"\nON x=REMAINDER\nPRINT \"wrong\"\nEND SELect\n"
    Assert.That(run [] source, Is.EqualTo("arm"))

[<Test; NonParallelizable>]
let ``many record copies in one routine reuse their temporaries`` () =
    let copies = String.concat "" [ for _ in 1 .. 70 -> "r=s\n" ]
    let source = "TYPE t\n a(3)\nEND TYPE\ncopyAll\nPRINT \"done\"\nSTOP\nDEFine PROCedure copyAll\nLOCal r AS t, s AS t\n" + copies + "END DEFine\n"
    Assert.That(run [] source, Is.EqualTo("done"))

[<Test; NonParallelizable>]
let ``a call to a procedure named TYPE is not a TYPE block`` () =
    let source = recordHeader + "a$=\"hi\"\ntype a$\nSTOP\nDEFine PROCedure type(t$)\nPRINT t$\nEND DEFine\n"
    Assert.That(run [] source, Is.EqualTo("hi"))

[<TestCase(false); TestCase(true); NonParallelizable>]
let ``directives accept a trailing comment`` records =
    let source = "#DEFINE X ## debug\n#IFDEF X ## debug\nPRINT 1\n#ENDIF ## end of debug\nPRINT 2\n"
    Assert.That(run [] (sourceWithRecords records source), Is.EqualTo("1\n2"))
