module SBTests.CompilerPipelineTests

open System
open System.IO

open NUnit.Framework

open CompilerPipeline

[<Test>]
let ``local register allocation defaults on and has independent CLI switches`` () =
    Assert.That((getSettings [|"input.bas";"out_exe"|]).TurboLocalRegisterAllocation, Is.True)
    let off=getSettings [|"input.bas";"out_exe";"--no-regalloc";"--native-regions"|]
    Assert.That(off.TurboLocalRegisterAllocation, Is.False)
    Assert.That(off.TurboNativeRegions, Is.True)
    let on=getSettings [|"--no-regalloc";"input.bas";"out_exe";"--regalloc";"--no-native-regions"|]
    Assert.That(on.TurboLocalRegisterAllocation, Is.True)
    Assert.That(on.TurboNativeRegions, Is.False)

[<Test>]
let ``audited native regions default on and have independent CLI switches`` () =
    Assert.That((getSettings [| "input.sb"; "output.ir" |]).TurboNativeRegions, Is.True)
    let off = getSettings [| "input.sb"; "output.ir"; "--no-native-regions"; "--hir-opt" |]
    Assert.That(off.Backend, Is.EqualTo("turboir"))
    Assert.That(off.TurboNativeRegions, Is.False)
    Assert.That(off.TurboHirOptimization.Enabled, Is.True)
    let on = getSettings [| "--no-native-regions"; "input.sb"; "output.ir"; "--native-regions"; "--no-hir-opt" |]
    Assert.That(on.TurboNativeRegions, Is.True)
    Assert.That(on.TurboHirOptimization.Enabled, Is.False)

[<Test>]
let ``Turbo HIR optimization defaults on and CLI flags override it`` () =
    Assert.That((getSettings [| "input.sb"; "output.ir" |]).TurboHirOptimization.Enabled, Is.True)
    let disabled = getSettings [| "input.sb"; "output.ir"; "--no-hir-opt" |]
    Assert.That(disabled.Backend, Is.EqualTo("turboir"))
    Assert.That(disabled.TurboHirOptimization.Enabled, Is.False)
    let enabled = getSettings [| "--no-hir-opt"; "input.sb"; "output.ir"; "--hir-opt"; "--dump-hir=out/trace" |]
    Assert.That(enabled.TurboHirOptimization.Enabled, Is.True)
    Assert.That(enabled.TurboHirOptimization.DumpPrefix, Is.EqualTo(Some "out/trace"))

[<Test>]
let ``getSettings accepts explicit backend argument`` () =
    let settings = getSettings [| "input.sb"; "output.cs"; "false"; "csharp" |]

    Assert.That(settings.InputFileName, Is.EqualTo("input.sb"))
    Assert.That(settings.OutputFileName, Is.EqualTo("output.cs"))
    Assert.That(settings.Verbose, Is.False)
    Assert.That(settings.Backend, Is.EqualTo("csharp"))
    Assert.That(settings.RuntimeHost, Is.Not.Empty)
    Assert.That(settings.SyntaxChecking, Is.EqualTo(Relaxed))

[<Test>]
let ``getSettings keeps three-argument cli compatibility`` () =
    let settings = getSettings [| "input.sb"; "output.txt"; "true" |]

    Assert.That(settings.InputFileName, Is.EqualTo("input.sb"))
    Assert.That(settings.OutputFileName, Is.EqualTo("output.txt"))
    Assert.That(settings.Verbose, Is.True)
    Assert.That(settings.Backend, Is.Not.Empty)
    Assert.That(settings.RuntimeHost, Is.Not.Empty)
    Assert.That(settings.SyntaxChecking, Is.EqualTo(Relaxed))

[<Test>]
let ``getSettings accepts one argument input shorthand`` () =
    let settings = getSettings [| "golfer.sb" |]

    Assert.That(settings.InputFileName, Is.EqualTo("golfer.sb"))
    Assert.That(settings.Backend, Is.EqualTo("interpret"))
    Assert.That(settings.RuntimeHost, Is.Not.Empty)

[<Test>]
let ``getSettings resolves extensionless input and defaults to interpret`` () =
    let tempDirectory = Path.Combine(Path.GetTempPath(), "sb-cli-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(tempDirectory) |> ignore
    let originalDirectory = Directory.GetCurrentDirectory()

    try
        let sourcePath = Path.Combine(tempDirectory, "myprog.bas")
        File.WriteAllText(sourcePath, "10 PRINT 1" + Environment.NewLine)
        Directory.SetCurrentDirectory(tempDirectory)

        let settings = getSettings [| "myprog" |]

        Assert.That(settings.InputFileName, Is.EqualTo("myprog.bas"))
        Assert.That(settings.Backend, Is.EqualTo("interpret"))
    finally
        Directory.SetCurrentDirectory(originalDirectory)
        if Directory.Exists(tempDirectory) then
            Directory.Delete(tempDirectory, true)

[<Test>]
let ``getSettings infers c backend from second argument and writes beside input`` () =
    let tempDirectory = Path.Combine(Path.GetTempPath(), "sb-cli-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(tempDirectory) |> ignore
    let originalDirectory = Directory.GetCurrentDirectory()

    try
        let sourcePath = Path.Combine(tempDirectory, "demo.sb")
        File.WriteAllText(sourcePath, "10 PRINT 1" + Environment.NewLine)
        Directory.SetCurrentDirectory(tempDirectory)

        let settings = getSettings [| "demo"; "demo.c" |]

        Assert.That(settings.InputFileName, Is.EqualTo("demo.sb"))
        Assert.That(settings.OutputFileName, Is.EqualTo(Path.Combine(tempDirectory, "demo.c")))
        Assert.That(settings.Backend, Is.EqualTo("c"))
    finally
        Directory.SetCurrentDirectory(originalDirectory)
        if Directory.Exists(tempDirectory) then
            Directory.Delete(tempDirectory, true)

[<Test>]
let ``getSettings infers csharp backend from second argument and writes beside input`` () =
    let tempDirectory = Path.Combine(Path.GetTempPath(), "sb-cli-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(tempDirectory) |> ignore
    let originalDirectory = Directory.GetCurrentDirectory()

    try
        let sourcePath = Path.Combine(tempDirectory, "demo.ssb")
        File.WriteAllText(sourcePath, "PRINT 1" + Environment.NewLine)
        Directory.SetCurrentDirectory(tempDirectory)

        let settings = getSettings [| "demo"; "demo.cs" |]

        Assert.That(settings.InputFileName, Is.EqualTo("demo.ssb"))
        Assert.That(settings.OutputFileName, Is.EqualTo(Path.Combine(tempDirectory, "demo.cs")))
        Assert.That(settings.Backend, Is.EqualTo("csharp"))
    finally
        Directory.SetCurrentDirectory(originalDirectory)
        if Directory.Exists(tempDirectory) then
            Directory.Delete(tempDirectory, true)
