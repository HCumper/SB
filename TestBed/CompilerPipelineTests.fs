module TestBed.CompilerPipelineTests

open NUnit.Framework

open CompilerPipeline

[<Test>]
let ``getSettings accepts explicit backend argument`` () =
    let settings = getSettings [| "input.sb"; "output.cs"; "false"; "csharp" |]

    Assert.That(settings.InputFileName, Is.EqualTo("input.sb"))
    Assert.That(settings.OutputFileName, Is.EqualTo("output.cs"))
    Assert.That(settings.Verbose, Is.False)
    Assert.That(settings.Backend, Is.EqualTo("csharp"))

[<Test>]
let ``getSettings keeps three-argument cli compatibility`` () =
    let settings = getSettings [| "input.sb"; "output.txt"; "true" |]

    Assert.That(settings.InputFileName, Is.EqualTo("input.sb"))
    Assert.That(settings.OutputFileName, Is.EqualTo("output.txt"))
    Assert.That(settings.Verbose, Is.True)
    Assert.That(settings.Backend, Is.Not.Empty)
