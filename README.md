# SB

`SB` is an F# toolchain for SuperBASIC and Structured SuperBASIC on modern `.NET 10`.

The project is not a Sinclair QL emulator. Its purpose is to parse, analyze, lower, interpret, and optionally generate code for SuperBASIC-family programs on a contemporary host. The implementation preserves the language model where practical. It is not a QL emulator and so does not address QL hardware, direct machine-code execution, firmware-specific behavior, and full interactive QL environment emulation.

## What It Does

The active pipeline is:

1. Load `.bas`, `.sb`, or `.ssb` source.
2. Classify the source as numbered SuperBASIC or Structured SuperBASIC.
3. Preprocess `.ssb` into numbered SuperBASIC when needed.
4. Parse with ANTLR.
5. Build a normalized AST.
6. Run semantic analysis for symbols, typing, coercions, constant folding, and diagnostics.
7. Lower the AST to HIR.
8. Interpret HIR or emit C#, C, or a published `.NET` executable.

Supported areas include core parsing, semantic analysis, HIR lowering, many control-flow constructs, a growing built-in/runtime layer, and partial screen/file/channel behavior. Known gaps remain around broad built-in coverage, precise QL compatibility, graphics/sound fidelity, and some generated-backend parity.

## Command Line

Run from the repository with:

```powershell
dotnet run --project .\SB\SB.fsproj -- [arguments]
```

With no arguments, `SB` reads defaults from `SB/appsettings.json`.

The short command forms are:

```powershell
dotnet run --project .\SB\SB.fsproj -- myprog
dotnet run --project .\SB\SB.fsproj -- myprog out.c
dotnet run --project .\SB\SB.fsproj -- myprog out.cs
```

Behavior:

- `myprog` without an extension resolves to the first existing file among `myprog.bas`, `myprog.sb`, and `myprog.ssb`.
- One argument always selects the `interpret` backend.
- Two arguments infer the backend only for `.c` and `.cs` outputs.
- A bare output file name is written beside the input file; rooted paths or paths with directories are used as given.
- Other settings not supplied on the command line come from `SB/appsettings.json`.

The full command form is:

```powershell
dotnet run --project .\SB\SB.fsproj -- <input> <output> <verbose> [backend] [runtimeHost]
```

Arguments:

- `<input>` is the source file, or a base name resolved through `.bas`, `.sb`, then `.ssb`.
- `<output>` is used by generated backends; it is still accepted for `interpret` but does not define interpreter output.
- `<verbose>` is `true` or `false`.
- `[backend]` is optional and overrides `AppSettings:Backend`.
- `[runtimeHost]` is optional and overrides `AppSettings:RuntimeHost` for interpreter runs.

Backends:

- `interpret` lowers to HIR and runs the interpreter.
- `csharp` writes generated C# and copies `GeneratedRuntime.cs`, `SBRuntime.dll`, and `FSharp.Core.dll` beside it.
- `c` writes generated C and copies `sbruntime_c.h` and `sbruntime_c.c` beside it.
- `dotnetexe`, `dotnet-exe`, or `exe` generates C# internally and publishes a single-file `.NET` executable.

Examples:

```powershell
# Interpret q3.bas, q3.sb, or q3.ssb
dotnet run --project .\SB\SB.fsproj -- q3

# Generate C
dotnet run --project .\SB\SB.fsproj -- .\q3.sb .\output\q3.c false c

# Generate C#
dotnet run --project .\SB\SB.fsproj -- .\q3.sb .\output\q3.generated.cs false csharp

# Publish a .NET executable
dotnet run --project .\SB\SB.fsproj -- .\q3.sb .\output\q3.exe false dotnetexe

# Interpret with the Avalonia host instead of the console host
dotnet run --project .\SB\SB.fsproj -- .\q3.sb .\unused.txt false interpret avalonia
```

If you have a wrapper command named `sb`, the same argument rules apply:

```powershell
sb q3
sb q3 q3.c
sb q3 q3.cs
```

## Appsettings

`SB/appsettings.json` supplies defaults when command-line arguments are omitted. It is also the source for settings that the short command forms do not expose.

Typical configuration:

```json
{
  "ApplicationName": "SB",
  "AppSettings": {
    "InputFile": "C:\\Source\\SB\\SB\\project planner.sb",
    "OutputFile": "C:\\Source\\SB\\output\\project-planner.exe",
    "Verbose": false,
    "Backend": "dotnetexe",
    "RuntimeHost": "avalonia",
    "SyntaxChecking": "relaxed",
    "ExecutionThrottle": {
      "Enabled": false,
      "TargetStatementsPerSecond": 100,
      "MaxRunAheadMilliseconds": 5
    }
  }
}
```

Important settings:

- `ApplicationName` is passed to generated code as the app name.
- `AppSettings:InputFile` is the default source file.
- `AppSettings:OutputFile` is the default generated output path.
- `AppSettings:Verbose` prints diagnostics, symbol tables, HIR, and backend progress.
- `AppSettings:Backend` selects `interpret`, `csharp`, `c`, or `dotnetexe` when not overridden.
- `AppSettings:RuntimeHost` selects `console` or `avalonia` for interpreter runs.
- `AppSettings:SyntaxChecking` accepts `relaxed` or `rigorous`; unknown or blank values fall back to `relaxed`.
- `AppSettings:ExecutionThrottle` can slow interpreter execution for host/UI responsiveness.

Command-line values override `InputFile`, `OutputFile`, `Verbose`, `Backend`, and `RuntimeHost` when the matching arguments are supplied. `SyntaxChecking` and `ExecutionThrottle` are currently configured through appsettings.

## Solution Layout

- `SB/` - main compiler/interpreter executable and pipeline orchestration.
- `SBLib/` - ANTLR grammar and generated parser artifacts.
- `SBRuntime/` - shared managed runtime services used by interpreter and generated C#.
- `SB.CSharpRuntime/` - generated-code runtime support.
- `SB.AvaloniaHost/` - optional GUI host for interpreter execution.
- `SBTests/` - compiler, lowering, backend, and execution tests.
- `SBRuntimeTests/` - shared runtime tests.
- `TestBed/` - legacy test/fixture area retained in the tree.

Important implementation files:

- `SB/CompilerPipeline.fs` - settings, source preparation, parsing, semantic analysis, and lowering helpers.
- `SB/Program.fs` - CLI entry point and backend dispatch.
- `SB/ParseTreeVisitor.fs` - parse tree to AST conversion.
- `SB/SemanticAnalyzer.fs` - semantic pass orchestration.
- `SB/AstToHir.fs` - AST to HIR lowering.
- `SB/Interpreter.fs` - HIR interpreter.
- `SB/HirCSharpBackend.fs` - HIR to C# backend.
- `SB/HirCBackend.fs` - HIR to C backend.
- `SB/CSharpRuntime/GeneratedRuntime.cs` - runtime source copied beside generated C#.
- `SB/CRuntime/sbruntime_c.h` and `SB/CRuntime/sbruntime_c.c` - runtime files copied beside generated C.

## Build And Test

Build the full solution:

```powershell
dotnet build .\SB.sln
```

Run the active test projects:

```powershell
dotnet test .\SBTests\SBTests.fsproj
dotnet test .\SBRuntimeTests\SBRuntimeTests.fsproj
```

You can also run all solution tests with:

```powershell
dotnet test .\SB.sln
```
