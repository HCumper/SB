module SBTests.GeneratedCSharpExecutionTests

open System
open System.Diagnostics
open System.IO

open NUnit.Framework

open Program
open SBRuntime
open Types
open HIR
open HirCSharpBackend

let private pos =
    { BasicLineNo = None
      EditorLineNo = 1
      Column = 0 }

let private literalInt value = Literal(ConstInt value, HirType.Int, pos)
let private literalString value = Literal(ConstString value, HirType.String, pos)

let private storage symbol name hirType storageClass =
    { Symbol = symbol
      Slot = StorageSlotId 0
      Name = name
      Type = hirType
      Dimensions = None
      Class = storageClass
      Position = pos }

let private parameter symbol name hirType storageClass binding =
    { Storage = storage symbol name hirType storageClass
      Binding = binding }

let private withTempDirectory action =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}")
    Directory.CreateDirectory(path) |> ignore
    try
        action path
    finally
        if Directory.Exists(path) then
            Directory.Delete(path, true)

let private runProcess (workingDirectory: string) (fileName: string) (arguments: string) (standardInput: string option) =
    let psi = ProcessStartInfo()
    psi.WorkingDirectory <- workingDirectory
    psi.FileName <- fileName
    psi.Arguments <- arguments
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.RedirectStandardInput <- true
    use proc = new Process()
    proc.StartInfo <- psi

    if not (proc.Start()) then
        failwith $"Failed to start process '{fileName}'."

    match standardInput with
    | Some text ->
        proc.StandardInput.Write(text)
        proc.StandardInput.Close()
    | None ->
        proc.StandardInput.Close()

    let stdout = proc.StandardOutput.ReadToEnd()
    let stderr = proc.StandardError.ReadToEnd()
    proc.WaitForExit()
    proc.ExitCode, stdout, stderr

let private writeGeneratedCSharpProject directory =
    let sbruntimePath = typeof<SBValue>.Assembly.Location
    let fsharpCorePath = typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location
    let projectText =
        $"""
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>disable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <AssemblyName>GeneratedProgram</AssemblyName>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="SBRuntime">
      <HintPath>{sbruntimePath}</HintPath>
    </Reference>
    <Reference Include="FSharp.Core">
      <HintPath>{fsharpCorePath}</HintPath>
    </Reference>
  </ItemGroup>
</Project>
"""

    File.WriteAllText(Path.Combine(directory, "GeneratedProgram.csproj"), projectText.Replace("\n", Environment.NewLine))

let private compileAndRunGeneratedCSharp (directory: string) (stdinText: string option) =
    writeGeneratedCSharpProject directory

    let buildExitCode, buildStdout, buildStderr =
        runProcess directory "dotnet" "build .\\GeneratedProgram.csproj --nologo" None

    Assert.That(
        buildExitCode,
        Is.EqualTo(0),
        $"dotnet build failed.{Environment.NewLine}stdout:{Environment.NewLine}{buildStdout}{Environment.NewLine}stderr:{Environment.NewLine}{buildStderr}")

    let runExitCode, stdout, stderr =
        runProcess directory "dotnet" "run --project .\\GeneratedProgram.csproj --no-build" stdinText

    runExitCode, stdout.Replace("\r\n", "\n"), stderr.Replace("\r\n", "\n")

let private generateCompileAndRun (source: string) (stdinText: string option) =
    withTempDirectory (fun dir ->
        let sourcePath = Path.Combine(dir, "program.sb")
        let generatedSourcePath = Path.Combine(dir, "program.cs")

        File.WriteAllText(sourcePath, source.Replace("\n", Environment.NewLine))

        let generateExitCode = Program.main [| sourcePath; generatedSourcePath; "false"; "csharp" |]
        Assert.That(generateExitCode, Is.EqualTo(0), "C# generation failed.")
        Assert.That(File.Exists(generatedSourcePath), Is.True)
        Assert.That(File.Exists(Path.Combine(dir, HirCSharpBackend.cSharpRuntimeFileName)), Is.True)

        compileAndRunGeneratedCSharp dir stdinText)

let private generateHirCompileAndRun className (program: HirProgram) (stdinText: string option) =
    withTempDirectory (fun dir ->
        let generatedSourcePath = Path.Combine(dir, "program.cs")
        let runtimePath = Path.Combine(dir, HirCSharpBackend.cSharpRuntimeFileName)

        File.WriteAllText(generatedSourcePath, generateCSharpFromHir className program)
        File.Copy(Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", "CSharpRuntime", HirCSharpBackend.cSharpRuntimeFileName), runtimePath)

        compileAndRunGeneratedCSharp dir stdinText)

let private generateHirInDirectory directory className (program: HirProgram) =
    let generatedSourcePath = Path.Combine(directory, "program.cs")
    let runtimePath = Path.Combine(directory, HirCSharpBackend.cSharpRuntimeFileName)

    File.WriteAllText(generatedSourcePath, generateCSharpFromHir className program)
    File.Copy(Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", "CSharpRuntime", HirCSharpBackend.cSharpRuntimeFileName), runtimePath, true)

let private generateSourceInDirectory directory (source: string) =
    let sourcePath = Path.Combine(directory, "program.sb")
    let generatedSourcePath = Path.Combine(directory, "program.cs")

    File.WriteAllText(sourcePath, source.Replace("\n", Environment.NewLine))

    let generateExitCode = Program.main [| sourcePath; generatedSourcePath; "false"; "csharp" |]
    Assert.That(generateExitCode, Is.EqualTo(0), "C# generation failed.")
    Assert.That(File.Exists(generatedSourcePath), Is.True)
    Assert.That(File.Exists(Path.Combine(directory, HirCSharpBackend.cSharpRuntimeFileName)), Is.True)

[<Test>]
let ``generated csharp executable runs simple print program`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun "10 PRINT 1+2\n20 STOP\n" None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("3"))

[<Test>]
let ``generated csharp executable supports dynamic goto and gosub`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun
            "10 t=40\n20 GOTO t\n30 PRINT \"BAD\"\n40 s=100\n50 GOSUB s\n60 PRINT \"DONE\"\n70 STOP\n100 PRINT \"SUB\"\n110 RETURN\n"
            None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("SUB\nDONE".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated csharp executable formats print commas as tab stops`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun "10 PRINT ,,\"A\"\n20 PRINT \"B\",\"C\"\n30 STOP\n" None

    let lines =
        stdout.Split('\n')
        |> Array.map (fun line -> line.TrimEnd('\r'))
        |> Array.filter (fun line -> line.Length > 0)

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(lines[0].IndexOf('A'), Is.EqualTo(16))
    Assert.That(lines[1].IndexOf('C'), Is.EqualTo(8))
    Assert.That(stdout, Does.Not.Contain("PRINT_COMMA"))

[<Test>]
let ``generated csharp executable aligns numbered jumps into nested structured scopes with the interpreter`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun
            "10 T=100\n20 GOTO T\n30 PRINT \"BAD\"\n40 STOP\n50 IF 1 THEN\n100 PRINT \"NESTED\"\n110 STOP\n120 END IF\n"
            None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("NESTED"))

[<Test>]
let ``generated csharp executable exits current scope when a numbered jump falls off the end of a for body`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun
            "10 COUNT=0\n15 GOTO 30\n20 FOR I=1 TO 1\n30 COUNT=COUNT+1\n40 END FOR I\n50 PRINT \"WRONG\"\n"
            None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))

[<Test>]
let ``generated csharp executable supports numbered line targets inside sequence for loops`` () =
    let counterSymbol = SymbolId 0

    let program =
        { SymbolNames = [ counterSymbol, "I" ] |> Map.ofList
          Globals = [ storage counterSymbol "I" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ ForSequence(
                LoopId 0,
                counterSymbol,
                [ literalInt 1; literalInt 2 ],
                literalInt 5,
                literalInt 6,
                [ literalInt 9 ],
                literalInt 1,
                [ LineNumber(30, pos)
                  BuiltInCall(Print, None, [ ReadVar(counterSymbol, HirType.Int, pos) ], pos)
                  Next(LoopId 0, pos) ],
                pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "SequenceForLineTargetProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("1\n2\n5\n6\n9".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated csharp executable supports jumps into sequence for loop bodies`` () =
    let countSymbol = SymbolId 0
    let counterSymbol = SymbolId 1

    let program =
        { SymbolNames = [ countSymbol, "COUNT"; counterSymbol, "I" ] |> Map.ofList
          Globals =
            [ storage countSymbol "COUNT" HirType.Int GlobalStorage
              storage counterSymbol "I" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(countSymbol, HirType.Int, pos), literalInt 0, pos)
              Goto(literalInt 30, pos)
              ForSequence(
                LoopId 0,
                counterSymbol,
                [ literalInt 1; literalInt 2 ],
                literalInt 5,
                literalInt 6,
                [ literalInt 9 ],
                literalInt 1,
                [ LineNumber(30, pos)
                  Assign(WriteVar(countSymbol, HirType.Int, pos), Binary(Add, ReadVar(countSymbol, HirType.Int, pos), literalInt 1, HirType.Int, pos), pos)
                  Goto(literalInt 100, pos) ],
                pos)
              LineNumber(50, pos)
              BuiltInCall(Print, None, [ literalString "BAD" ], pos)
              LineNumber(100, pos)
              BuiltInCall(Print, None, [ ReadVar(countSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "JumpIntoSequenceForProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("1"))

[<Test>]
let ``generated csharp executable treats goto to routine end line as routine exit`` () =
    let routineSymbol = SymbolId 0

    let program =
        { SymbolNames = [ routineSymbol, "WATER" ] |> Map.ofList
          Globals = []
          Routines =
            [ { Name = "WATER"
                Symbol = routineSymbol
                Parameters = []
                Locals = []
                Body =
                    [ LineNumber(1920, pos)
                      Goto(literalInt 1970, pos)
                      LineNumber(1960, pos)
                      BuiltInCall(Print, None, [ literalString "BAD" ], pos) ]
                ReturnType = None
                EndLineNumber = Some 1970
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ ProcCall(routineSymbol, None, [], pos)
              BuiltInCall(Print, None, [ literalString "DONE" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "RoutineEndDispatchProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("DONE"))

[<Test>]
let ``generated csharp executable preserves dimn semantics`` () =
    let arraySymbol = SymbolId 0
    let dimnSymbol = SymbolId 1
    let firstSymbol = SymbolId 2
    let secondSymbol = SymbolId 3

    let program =
        { SymbolNames = [ arraySymbol, "A"; dimnSymbol, "DIMN"; firstSymbol, "FIRST"; secondSymbol, "SECOND" ] |> Map.ofList
          Globals =
            [ { storage arraySymbol "A" (HirType.Array HirType.Int) GlobalStorage with Dimensions = Some [ 10; 17 ] }
              storage firstSymbol "FIRST" HirType.Int GlobalStorage
              storage secondSymbol "SECOND" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(
                WriteVar(firstSymbol, HirType.Int, pos),
                CallFunc(dimnSymbol, [ ValueArg(ReadVar(arraySymbol, HirType.Array HirType.Int, pos)); ValueArg(literalInt 1) ], HirType.Int, pos),
                pos)
              Assign(
                WriteVar(secondSymbol, HirType.Int, pos),
                CallFunc(dimnSymbol, [ ValueArg(ReadVar(arraySymbol, HirType.Array HirType.Int, pos)); ValueArg(literalInt 2) ], HirType.Int, pos),
                pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.Int, pos); ReadVar(secondSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "DimnProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("10 17"))

[<Test>]
let ``generated csharp executable supports instr`` () =
    let instrSymbol = SymbolId 0
    let resultSymbol = SymbolId 1

    let program =
        { SymbolNames = [ instrSymbol, "INSTR"; resultSymbol, "RESULT" ] |> Map.ofList
          Globals = [ storage resultSymbol "RESULT" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(
                WriteVar(resultSymbol, HirType.Int, pos),
                CallFunc(instrSymbol, [ ValueArg(literalString "HELLO"); ValueArg(literalString "LL") ], HirType.Int, pos),
                pos)
              BuiltInCall(Print, None, [ ReadVar(resultSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "InstrProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("3"))

[<Test>]
let ``generated csharp executable supports beeping function`` () =
    let beepingSymbol = SymbolId 0
    let resultSymbol = SymbolId 1

    let program =
        { SymbolNames = [ beepingSymbol, "BEEPING"; resultSymbol, "RESULT" ] |> Map.ofList
          Globals = [ storage resultSymbol "RESULT" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(resultSymbol, HirType.Int, pos), CallFunc(beepingSymbol, [], HirType.Int, pos), pos)
              BuiltInCall(Print, None, [ ReadVar(resultSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "BeepingProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("0"))

[<Test>]
let ``generated csharp executable supports host helper functions`` () =
    let getenvSymbol = SymbolId 0
    let keyrowSymbol = SymbolId 1
    let resultSymbol = SymbolId 2

    let program =
        { SymbolNames = [ getenvSymbol, "GETENV$"; keyrowSymbol, "KEYROW"; resultSymbol, "RESULT$" ] |> Map.ofList
          Globals = [ storage resultSymbol "RESULT$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(resultSymbol, HirType.String, pos), CallFunc(getenvSymbol, [ ValueArg(literalString "PATH") ], HirType.String, pos), pos)
              BuiltInCall(Print, None, [ CallFunc(keyrowSymbol, [ ValueArg(literalInt 1) ], HirType.Int, pos) ], pos)
              BuiltInCall(Print, None, [ CallFunc(getenvSymbol, [ ValueArg(literalString "PATH") ], HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "HostFunctionProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    let lines = stdout.Trim().Split('\n')
    Assert.That(lines.Length, Is.EqualTo(2))
    Assert.That(lines[0].Trim(), Is.EqualTo("0"))
    Assert.That(lines[1].Trim().Length, Is.GreaterThan(0))

[<Test>]
let ``generated csharp executable supports inkey helpers with redirected input`` () =
    let inkeySymbol = SymbolId 0
    let inkeyStringSymbol = SymbolId 1
    let firstSymbol = SymbolId 2
    let secondSymbol = SymbolId 3

    let program =
        { SymbolNames = [ inkeySymbol, "INKEY"; inkeyStringSymbol, "INKEY$"; firstSymbol, "FIRST"; secondSymbol, "SECOND$" ] |> Map.ofList
          Globals =
            [ storage firstSymbol "FIRST" HirType.Int GlobalStorage
              storage secondSymbol "SECOND$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(firstSymbol, HirType.Int, pos), CallFunc(inkeySymbol, [], HirType.Int, pos), pos)
              Assign(WriteVar(secondSymbol, HirType.String, pos), CallFunc(inkeyStringSymbol, [], HirType.String, pos), pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.Int, pos); ReadVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "InkeyProgram" program (Some "AZ")

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("65 Z"))

[<Test>]
let ``generated csharp executable supports graphics helper functions`` () =
    let pointSymbol = SymbolId 0
    let pointRSymbol = SymbolId 1

    let program =
        { SymbolNames = [ pointSymbol, "POINT"; pointRSymbol, "POINT_R" ] |> Map.ofList
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(Print, None, [ CallFunc(pointSymbol, [ ValueArg(literalInt 10); ValueArg(literalInt 20) ], HirType.Int, pos) ], pos)
              BuiltInCall(Print, None, [ CallFunc(pointRSymbol, [ ValueArg(literalInt 1); ValueArg(literalInt 2) ], HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "GraphicsFunctionProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("0\n0".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated csharp executable tracks statement side graphics state`` () =
    let pointSymbol = SymbolId 0
    let pointRSymbol = SymbolId 1

    let program =
        { SymbolNames = [ pointSymbol, "POINT"; pointRSymbol, "POINT_R" ] |> Map.ofList
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "PLOT", None, [ literalInt 10; literalInt 20 ], pos)
              BuiltInCall(Print, None, [ CallFunc(pointSymbol, [ ValueArg(literalInt 10); ValueArg(literalInt 20) ], HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "DRAW", None, [ literalInt 3; literalInt 0 ], pos)
              BuiltInCall(Print, None, [ CallFunc(pointRSymbol, [ ValueArg(literalInt 0); ValueArg(literalInt 0) ], HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "LINE", None, [ literalInt 30; literalInt 40; literalInt 32; literalInt 40 ], pos)
              BuiltInCall(Print, None, [ CallFunc(pointSymbol, [ ValueArg(literalInt 31); ValueArg(literalInt 40) ], HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLS", None, [], pos)
              BuiltInCall(Print, None, [ CallFunc(pointSymbol, [ ValueArg(literalInt 10); ValueArg(literalInt 20) ], HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "GraphicsStatementProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("1\n1\n1\n0".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated csharp executable accepts shared bridged screen statements`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun
            "10 WINDOW 512,256,0,0\n20 AT 0,0\n30 INK 7\n40 PAPER 4\n50 PAPER#5,2\n60 CLS\n70 PRINT \"OK\"\n80 STOP\n"
            None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("OK"))

[<Test>]
let ``generated csharp executable accepts remaining shared screen control statements`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun
            "10 CSIZE 1,0\n20 CHAR_USE -1,-1\n30 STRIP 7,4,1\n40 BORDER 2,7,0,1\n50 CLEAR\n60 SCROLL 2\n70 WIDTH 40\n80 PAN 1\n90 RECOL 1,2,3\n100 PALETTE 4,5,6\n110 PRINT \"OK\"\n120 STOP\n"
            None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("OK"))

[<Test>]
let ``generated csharp executable accepts remaining graphics statements`` () =
    let pointSymbol = SymbolId 0

    let program =
        { SymbolNames = [ pointSymbol, "POINT" ] |> Map.ofList
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "FILL", None, [ literalInt 1 ], pos)
              BuiltInCall(NamedBuiltIn "SCALE", None, [ literalInt 100; literalInt 0; literalInt 0 ], pos)
              BuiltInCall(NamedBuiltIn "CIRCLE", None, [ literalInt 20; literalInt 20; literalInt 5 ], pos)
              BuiltInCall(NamedBuiltIn "ELLIPSE", None, [ literalInt 40; literalInt 40; literalInt 6; literalInt 1; literalInt 0 ], pos)
              BuiltInCall(NamedBuiltIn "BLOCK", None, [ literalInt 3; literalInt 2; literalInt 50; literalInt 50; literalInt 7 ], pos)
              BuiltInCall(NamedBuiltIn "ARC", None, [ literalInt 60; literalInt 60; literalInt 70; literalInt 60; Literal(ConstFloat 1.0, HirType.Float, pos) ], pos)
              BuiltInCall(NamedBuiltIn "PLOT", None, [ literalInt 80; literalInt 80 ], pos)
              BuiltInCall(NamedBuiltIn "CIRCLE_R", None, [ literalInt 5; literalInt 0; literalInt 3 ], pos)
              BuiltInCall(NamedBuiltIn "ELLIPSE_R", None, [ literalInt 10; literalInt 0; literalInt 4; Literal(ConstFloat 0.5, HirType.Float, pos); literalInt 0 ], pos)
              BuiltInCall(NamedBuiltIn "ARC_R", None, [ literalInt 0; literalInt 0; literalInt 5; literalInt 5; Literal(ConstFloat 0.5, HirType.Float, pos) ], pos)
              BuiltInCall(NamedBuiltIn "OVER", None, [ literalInt -1 ], pos)
              BuiltInCall(NamedBuiltIn "UNDER", None, [ literalInt 1 ], pos)
              BuiltInCall(NamedBuiltIn "FLASH", None, [ literalInt 3 ], pos)
              BuiltInCall(NamedBuiltIn "PENUP", None, [], pos)
              BuiltInCall(NamedBuiltIn "PENDOWN", None, [], pos)
              BuiltInCall(NamedBuiltIn "TURN", None, [ literalInt 30 ], pos)
              BuiltInCall(NamedBuiltIn "TURNTO", None, [ literalInt 45 ], pos)
              BuiltInCall(Print, None, [ CallFunc(pointSymbol, [ ValueArg(literalInt 20); ValueArg(literalInt 20) ], HirType.Int, pos)
                                         CallFunc(pointSymbol, [ ValueArg(literalInt 40); ValueArg(literalInt 40) ], HirType.Int, pos)
                                         CallFunc(pointSymbol, [ ValueArg(literalInt 51); ValueArg(literalInt 51) ], HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "RemainingGraphicsProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("1 1 1"))

[<Test>]
let ``generated csharp executable keeps default graphics state when clearing another channel`` () =
    let pointSymbol = SymbolId 0

    let program =
        { SymbolNames = [ pointSymbol, "POINT" ] |> Map.ofList
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "PLOT", None, [ literalInt 10; literalInt 20 ], pos)
              BuiltInCall(NamedBuiltIn "CLS", Some(ExplicitChannel(literalInt 0)), [], pos)
              BuiltInCall(Print, None, [ CallFunc(pointSymbol, [ ValueArg(literalInt 10); ValueArg(literalInt 20) ], HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "ChannelClearPreservesGraphicsProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("1"))

[<Test>]
let ``generated csharp executable supports data read and restore`` () =
    let firstSymbol = SymbolId 0
    let textSymbol = SymbolId 1
    let thirdSymbol = SymbolId 2
    let fourthSymbol = SymbolId 3

    let program =
        { SymbolNames = [ firstSymbol, "A"; textSymbol, "B$"; thirdSymbol, "C"; fourthSymbol, "D" ] |> Map.ofList
          Globals =
            [ storage firstSymbol "A" HirType.Int GlobalStorage
              storage textSymbol "B$" HirType.String GlobalStorage
              storage thirdSymbol "C" HirType.Int GlobalStorage
              storage fourthSymbol "D" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries =
            [ { Slot = DataSlotId 0; Value = Literal(ConstInt 4, HirType.Int, pos); Position = pos; LineNumber = Some 10 }
              { Slot = DataSlotId 1; Value = Literal(ConstString "HELLO", HirType.String, pos); Position = pos; LineNumber = Some 10 }
              { Slot = DataSlotId 2; Value = Literal(ConstInt 9, HirType.Int, pos); Position = pos; LineNumber = Some 10 } ]
          RestorePoints = [ { LineNumber = 10; Slot = DataSlotId 0 } ]
          Main =
            [ Read([ WriteVar(firstSymbol, HirType.Int, pos); WriteVar(textSymbol, HirType.String, pos); WriteVar(thirdSymbol, HirType.Int, pos) ], pos)
              Restore(Some(literalInt 10), pos)
              Read([ WriteVar(fourthSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.Int, pos); ReadVar(textSymbol, HirType.String, pos); ReadVar(thirdSymbol, HirType.Int, pos); ReadVar(fourthSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "DataRestoreProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("4 HELLO 9 4"))

[<Test>]
let ``generated csharp executable supports when error continue`` () =
    let errorFlagSymbol = SymbolId 0
    let ernumSymbol = SymbolId 1

    let program =
        { SymbolNames = [ errorFlagSymbol, "FLAG"; ernumSymbol, "ERNUM" ] |> Map.ofList
          Globals = [ storage errorFlagSymbol "FLAG" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ WhenError(
                [ Assign(WriteVar(errorFlagSymbol, HirType.Int, pos), CallFunc(ernumSymbol, [], HirType.Int, pos), pos)
                  BuiltInCall(Print, None, [ ReadVar(errorFlagSymbol, HirType.Int, pos) ], pos)
                  BuiltInCall(NamedBuiltIn "CONTINUE", None, [], pos) ],
                pos)
              BuiltInCall(NamedBuiltIn "POKE_W", None, [ literalInt -1; literalInt 1 ], pos)
              BuiltInCall(Print, None, [ literalString "DONE" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "WhenErrorProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("-15\nDONE".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated csharp executable supports numbered line jumps inside when error handlers`` () =
    let ernumSymbol = SymbolId 0

    let program =
        { SymbolNames = [ ernumSymbol, "ERNUM" ] |> Map.ofList
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ WhenError(
                [ LineNumber(200, pos)
                  Goto(literalInt 210, pos)
                  LineNumber(205, pos)
                  BuiltInCall(Print, None, [ literalString "BAD" ], pos)
                  LineNumber(210, pos)
                  BuiltInCall(Print, None, [ CallFunc(ernumSymbol, [], HirType.Int, pos) ], pos)
                  BuiltInCall(NamedBuiltIn "CONTINUE", None, [], pos) ],
                pos)
              BuiltInCall(NamedBuiltIn "POKE_W", None, [ literalInt -1; literalInt 1 ], pos)
              BuiltInCall(Print, None, [ literalString "DONE" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "WhenErrorLineDispatchProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("-15\nDONE".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated csharp executable rejects string character by reference actuals`` () =
    let textSymbol = SymbolId 0
    let routineSymbol = SymbolId 1
    let parameterSymbol = SymbolId 2

    let program =
        { SymbolNames = [ textSymbol, "TEXT$"; routineSymbol, "BUMP"; parameterSymbol, "P$" ] |> Map.ofList
          Globals = [ storage textSymbol "TEXT$" HirType.String GlobalStorage ]
          Routines =
            [ { Name = "BUMP"
                Symbol = routineSymbol
                Parameters = [ parameter parameterSymbol "P$" HirType.String (RoutineParameterStorage "BUMP") FlexibleBinding ]
                Locals = []
                Body = [ Return(None, pos) ]
                ReturnType = None
                EndLineNumber = None
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(textSymbol, HirType.String, pos), literalString "AB", pos)
              ProcCall(routineSymbol, None, [ RefArg(WriteStringChar(textSymbol, literalInt 1, HirType.String, pos)) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "InvalidRefProgram" program None

    Assert.That(exitCode, Is.Not.EqualTo(0))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("String character targets cannot be used as by-reference storage locations."))

[<Test>]
let ``generated csharp executable supports dynamic on goto and on gosub`` () =
    let targetSymbol = SymbolId 0
    let selectorSymbol = SymbolId 1

    let program =
        { SymbolNames = [ targetSymbol, "TARGET"; selectorSymbol, "SEL" ] |> Map.ofList
          Globals =
            [ storage targetSymbol "TARGET" HirType.Int GlobalStorage
              storage selectorSymbol "SEL" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(selectorSymbol, HirType.Int, pos), literalInt 1, pos)
              Assign(WriteVar(targetSymbol, HirType.Int, pos), literalInt 100, pos)
              OnGoto(ReadVar(selectorSymbol, HirType.Int, pos), [ ReadVar(targetSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(Print, None, [ literalString "BAD" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos)
              LineNumber(100, pos)
              BuiltInCall(Print, None, [ literalString "GOTO" ], pos)
              Assign(WriteVar(targetSymbol, HirType.Int, pos), literalInt 200, pos)
              OnGosub(ReadVar(selectorSymbol, HirType.Int, pos), [ ReadVar(targetSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(Print, None, [ literalString "DONE" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos)
              LineNumber(200, pos)
              BuiltInCall(Print, None, [ literalString "SUB" ], pos)
              Return(None, pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "DynamicOnLineProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("GOTO\nSUB\nDONE".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated csharp executable supports file channel write and read`` () =
    let textSymbol = SymbolId 0
    let channel9 = ExplicitChannel(literalInt 9)

    let program =
        { SymbolNames = [ textSymbol, "A$" ] |> Map.ofList
          Globals = [ storage textSymbol "A$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "sample.txt" ], pos)
              BuiltInCall(Print, Some channel9, [ literalString "HELLO" ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "sample.txt" ], pos)
              Input(Some channel9, [], [ WriteVar(textSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(textSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "FileIoProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("HELLO"))

[<Test>]
let ``generated csharp executable supports eof on channels`` () =
    let beforeSymbol = SymbolId 0
    let afterSymbol = SymbolId 1
    let textSymbol = SymbolId 2
    let eofSymbol = SymbolId 3
    let channel9 = ExplicitChannel(literalInt 9)

    let program =
        { SymbolNames = [ beforeSymbol, "BEFORE"; afterSymbol, "AFTER"; textSymbol, "TEXT$"; eofSymbol, "EOF" ] |> Map.ofList
          Globals =
            [ storage beforeSymbol "BEFORE" HirType.Int GlobalStorage
              storage afterSymbol "AFTER" HirType.Int GlobalStorage
              storage textSymbol "TEXT$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "eof.txt" ], pos)
              BuiltInCall(Print, Some channel9, [ literalString "HELLO" ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "eof.txt" ], pos)
              Assign(WriteVar(beforeSymbol, HirType.Int, pos), CallFunc(eofSymbol, [ ValueArg(literalInt 9) ], HirType.Int, pos), pos)
              Input(Some channel9, [], [ WriteVar(textSymbol, HirType.String, pos) ], pos)
              Assign(WriteVar(afterSymbol, HirType.Int, pos), CallFunc(eofSymbol, [ ValueArg(literalInt 9) ], HirType.Int, pos), pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(beforeSymbol, HirType.Int, pos); ReadVar(afterSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "EofProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("0 1"))

[<Test>]
let ``generated csharp executable supports append mode files`` () =
    let firstSymbol = SymbolId 0
    let secondSymbol = SymbolId 1
    let channel9 = ExplicitChannel(literalInt 9)

    let program =
        { SymbolNames = [ firstSymbol, "FIRST$"; secondSymbol, "SECOND$" ] |> Map.ofList
          Globals =
            [ storage firstSymbol "FIRST$" HirType.String GlobalStorage
              storage secondSymbol "SECOND$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "append.txt" ], pos)
              BuiltInCall(Print, Some channel9, [ literalString "FIRST" ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "APPEND", Some channel9, [ literalString "append.txt" ], pos)
              BuiltInCall(Print, Some channel9, [ literalString "SECOND" ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "append.txt" ], pos)
              Input(Some channel9, [], [ WriteVar(firstSymbol, HirType.String, pos) ], pos)
              Input(Some channel9, [], [ WriteVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.String, pos); ReadVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "AppendProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("FIRST SECOND"))

[<Test>]
let ``generated csharp executable reports run as unsupported`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun "10 RUN\n" None

    Assert.That(exitCode, Is.Not.EqualTo(0))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Program-management built-ins are not meaningful in generated C# programs."))

[<TestCase("SEXEC", "10 SEXEC \"demo_job\"\n")>]
[<TestCase("SBYTES", "10 SBYTES 1024\n")>]
[<TestCase("CALL", "10 CALL 131072\n")>]
[<TestCase("RESPR", "10 RESPR 1024\n")>]
[<TestCase("LOADMEM", "10 LOADMEM \"image\", 131072\n")>]
[<TestCase("SAVEMEM", "10 SAVEMEM \"image\", 131072, 16\n")>]
[<TestCase("SYSVAR", "10 SYSVAR 0, 1\n")>]
let ``generated csharp executable reports host-specific statements as explicitly unsupported`` name source =
    let exitCode, stdout, stderr =
        generateCompileAndRun source None

    Assert.That(exitCode, Is.Not.EqualTo(0))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain($"Host-specific built-in statement '{name}' is explicitly unsupported by the generated C# backend."))

[<Test>]
let ``generated csharp executable reports unsupported graphics statements with a specific category`` () =
    let program =
        { SymbolNames = Map.empty
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "POINT", None, [ literalInt 10; literalInt 20 ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "UnsupportedGraphicsProgram" program None

    Assert.That(exitCode, Is.Not.EqualTo(0))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Graphics built-in statement 'POINT' is not supported by the generated C# backend yet."))

[<Test>]
let ``generated csharp executable reports unsupported turbo statements with a specific category`` () =
    let program =
        { SymbolNames = Map.empty
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "TURBO_TASKN", None, [ literalInt 1 ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "UnsupportedTurboProgram" program None

    Assert.That(exitCode, Is.Not.EqualTo(0))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Turbo toolkit built-in statement 'TURBO_TASKN' is not supported by the generated C# backend yet."))

[<Test>]
let ``generated csharp executable supports copy rename and delete`` () =
    withTempDirectory (fun dir ->
        let textSymbol = SymbolId 0
        let channel9 = ExplicitChannel(literalInt 9)

        let program =
            { SymbolNames = [ textSymbol, "A$" ] |> Map.ofList
              Globals = [ storage textSymbol "A$" HirType.String GlobalStorage ]
              Routines = []
              DataEntries = []
              RestorePoints = []
              Main =
                [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "source.txt" ], pos)
                  BuiltInCall(Print, Some channel9, [ literalString "HELLO" ], pos)
                  BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
                  BuiltInCall(NamedBuiltIn "COPY", None, [ literalString "source.txt"; literalString "copy1.txt" ], pos)
                  BuiltInCall(NamedBuiltIn "COPY_N", None, [ literalString "copy1.txt"; literalString "copy2.txt" ], pos)
                  BuiltInCall(NamedBuiltIn "RENAME", None, [ literalString "copy2.txt"; literalString "renamed.txt" ], pos)
                  BuiltInCall(NamedBuiltIn "DELETE", None, [ literalString "copy1.txt" ], pos)
                  BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "renamed.txt" ], pos)
                  Input(Some channel9, [], [ WriteVar(textSymbol, HirType.String, pos) ], pos)
                  BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
                  BuiltInCall(Print, None, [ ReadVar(textSymbol, HirType.String, pos) ], pos)
                  BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

        generateHirInDirectory dir "CopyRenameDeleteProgram" program

        let exitCode, stdout, stderr =
            compileAndRunGeneratedCSharp dir None

        Assert.That(exitCode, Is.EqualTo(0), stderr)
        Assert.That(stdout.Trim(), Is.EqualTo("HELLO"))
        Assert.That(File.Exists(Path.Combine(dir, "source.txt")), Is.True)
        Assert.That(File.Exists(Path.Combine(dir, "copy1.txt")), Is.False)
        Assert.That(File.Exists(Path.Combine(dir, "copy2.txt")), Is.False)
        Assert.That(File.Exists(Path.Combine(dir, "renamed.txt")), Is.True))

[<Test>]
let ``generated csharp executable supports move`` () =
    withTempDirectory (fun dir ->
        let textSymbol = SymbolId 0
        let channel9 = ExplicitChannel(literalInt 9)

        let program =
            { SymbolNames = [ textSymbol, "A$" ] |> Map.ofList
              Globals = [ storage textSymbol "A$" HirType.String GlobalStorage ]
              Routines = []
              DataEntries = []
              RestorePoints = []
              Main =
                [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "source.txt" ], pos)
                  BuiltInCall(Print, Some channel9, [ literalString "HELLO" ], pos)
                  BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
                  BuiltInCall(NamedBuiltIn "MOVE", None, [ literalString "source.txt"; literalString "moved.txt" ], pos)
                  BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "moved.txt" ], pos)
                  Input(Some channel9, [], [ WriteVar(textSymbol, HirType.String, pos) ], pos)
                  BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
                  BuiltInCall(Print, None, [ ReadVar(textSymbol, HirType.String, pos) ], pos)
                  BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

        generateHirInDirectory dir "MoveProgram" program

        let exitCode, stdout, stderr =
            compileAndRunGeneratedCSharp dir None

        Assert.That(exitCode, Is.EqualTo(0), stderr)
        Assert.That(stdout.Trim(), Is.EqualTo("HELLO"))
        Assert.That(File.Exists(Path.Combine(dir, "source.txt")), Is.False)
        Assert.That(File.Exists(Path.Combine(dir, "moved.txt")), Is.True))

[<Test>]
let ``generated csharp executable supports wait slug and sdate`` () =
    let dateSymbol = SymbolId 0
    let resultSymbol = SymbolId 1

    let program =
        { SymbolNames = [ dateSymbol, "DATE"; resultSymbol, "RESULT" ] |> Map.ofList
          Globals = [ storage resultSymbol "RESULT" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "WAIT", None, [ literalInt 1 ], pos)
              BuiltInCall(NamedBuiltIn "SLUG", None, [ literalInt 1 ], pos)
              BuiltInCall(NamedBuiltIn "SDATE", None, [ literalInt 123456789 ], pos)
              Assign(WriteVar(resultSymbol, HirType.Int, pos), CallFunc(dateSymbol, [], HirType.Int, pos), pos)
              BuiltInCall(Print, None, [ ReadVar(resultSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "WaitSlugSdateProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("123456789"))

[<Test>]
let ``generated csharp executable supports set position and truncate`` () =
    let textSymbol = SymbolId 0
    let channel9 = ExplicitChannel(literalInt 9)

    let program =
        { SymbolNames = [ textSymbol, "A$" ] |> Map.ofList
          Globals = [ storage textSymbol "A$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "truncate.txt" ], pos)
              BuiltInCall(Print, Some channel9, [ literalString "HELLO" ], pos)
              BuiltInCall(NamedBuiltIn "SET_POSITION", Some channel9, [ literalInt 2 ], pos)
              BuiltInCall(NamedBuiltIn "TRUNCATE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "truncate.txt" ], pos)
              Input(Some channel9, [], [ WriteVar(textSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(textSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "TruncateProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("HE"))

[<Test>]
let ``generated csharp executable supports set channel aliases`` () =
    let firstSymbol = SymbolId 0
    let secondSymbol = SymbolId 1
    let channel9 = ExplicitChannel(literalInt 9)
    let channel10 = ExplicitChannel(literalInt 10)

    let program =
        { SymbolNames = [ firstSymbol, "FIRST$"; secondSymbol, "SECOND$" ] |> Map.ofList
          Globals =
            [ storage firstSymbol "FIRST$" HirType.String GlobalStorage
              storage secondSymbol "SECOND$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "alias.txt" ], pos)
              BuiltInCall(Print, Some channel9, [ literalString "FIRST" ], pos)
              BuiltInCall(NamedBuiltIn "SET_CHANNEL", Some channel10, [ literalInt 9 ], pos)
              BuiltInCall(Print, Some channel10, [ literalString "SECOND" ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel10, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "alias.txt" ], pos)
              Input(Some channel9, [], [ WriteVar(firstSymbol, HirType.String, pos) ], pos)
              Input(Some channel9, [], [ WriteVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.String, pos); ReadVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "SetChannelProgram" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("FIRST SECOND"))
