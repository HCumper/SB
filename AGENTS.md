# SB

F# SuperBASIC / Structured SuperBASIC toolchain targeting `.NET 10`.

`SB` is a modern toolchain, not a Sinclair QL emulator. Its purpose is to parse, analyze, lower, interpret, and optionally generate code for SuperBASIC-family programs on contemporary hosts. Preserve SuperBASIC language behavior where practical, but do not assume QL hardware, firmware, machine-code execution, or full interactive QL environment emulation exists.

The runtime is incomplete. Prefer making the interpreter correct and well-tested before extending alternate generated backends, unless the task explicitly targets C#, C, or `.NET exe` generation.

## Current capabilities

- preprocesses `.ssb` into numbered SuperBASIC
- parses `.bas`, `.sb`, and `.ssb` with ANTLR
- builds a normalized AST
- runs semantic analysis with symbol resolution, typing, coercion, constant folding, and diagnostics
- lowers AST to HIR
- interprets a subset of HIR
- models flexible versus by-reference routine parameters in semantics and HIR
- interprets core control flow including `GOTO`, `GOSUB`, `RETURN`, `ON GOTO`, and `ON GOSUB`
- generates alternate C# from lowered HIR
- generates alternate plain C from lowered HIR
- generates a published `.NET` executable from lowered HIR
- shares some built-in/runtime behavior through `SBRuntime`

## Solution layout

- `SB/` - main compiler/interpreter executable and compiler pipeline
- `SBLib/` - grammar and generated parser artifacts
- `SBRuntime/` - shared managed runtime services used by interpreter and generated C#
- `SB.CSharpRuntime/` - generated C# runtime support project
- `SB.AvaloniaHost/` - optional GUI host for interpreter execution
- `SBTests/` - compiler, lowering, backend, and execution tests
- `SBRuntimeTests/` - shared runtime tests
- `TestBed/` - legacy test/fixture area retained in the tree

## Active pipeline

1. load `.bas`, `.sb`, or `.ssb` source
2. classify as numbered SuperBASIC or Structured SuperBASIC
3. preprocess `.ssb` when needed
4. parse with ANTLR
5. lower parse tree to AST
6. run semantic analysis
7. lower AST to HIR
8. interpret HIR or emit C# / C / `.NET exe`

## Important files

- `SB/CompilerPipeline.fs` - settings, source preparation, parsing, semantic analysis, and lowering helpers
- `SB/Program.fs` - CLI entry point and backend dispatch
- `SB/ParseTreeVisitor.fs` - parse tree to AST
- `SB/SemanticAnalyzer.fs` - semantic pass orchestration
- `SB/SemanticAnalysis.Symbols.fs` - scope and symbol rules
- `SB/SemanticAnalysis.Expressions.fs` - expression typing/coercion/folding
- `SB/AstToHir.fs` - AST to HIR lowering
- `SB/Interpreter.fs` - HIR interpreter
- `SB/BuiltIns.fs` - built-in name/signature model
- `SB/HirCSharpBackend.fs` - HIR to C#
- `SB/HirCBackend.fs` - HIR to C
- `SB/CSharpRuntime/GeneratedRuntime.cs` - runtime source copied beside generated C#
- `SB/CRuntime/sbruntime_c.h` - shared C runtime header copied beside generated C
- `SB/CRuntime/sbruntime_c.c` - shared C runtime source copied beside generated C
- `SBRuntime/` - managed runtime implementation shared across interpreter/generated C# where available

## Command line and appsettings

`SB/appsettings.json` provides defaults when CLI arguments are omitted. Command-line values override `InputFile`, `OutputFile`, `Verbose`, `Backend`, and `RuntimeHost` when supplied.

Repository run form:

```powershell
dotnet run --project .\SB\SB.fsproj -- [arguments]
```

Short forms:

```powershell
dotnet run --project .\SB\SB.fsproj -- myprog
dotnet run --project .\SB\SB.fsproj -- myprog out.c
dotnet run --project .\SB\SB.fsproj -- myprog out.cs
```

Rules:

- input without an extension resolves to the first existing file among `.bas`, `.sb`, and `.ssb`
- one argument selects `interpret`
- two arguments infer `c` for `.c` output and `csharp` for `.cs` output
- a bare output file name is written beside the input file
- rooted output paths or paths with directories are used as given

Full form:

```powershell
dotnet run --project .\SB\SB.fsproj -- <input> <output> <verbose> [backend] [runtimeHost]
```

Backends:

- `interpret` - lower to HIR and run the interpreter
- `csharp` - lower to HIR, write generated C#, and copy `GeneratedRuntime.cs`, `SBRuntime.dll`, and `FSharp.Core.dll`
- `c` - lower to HIR, write generated C, and copy `sbruntime_c.h` and `sbruntime_c.c`
- `dotnetexe`, `dotnet-exe`, `exe` - generate C# internally and publish a single-file `.NET` executable

Runtime hosts:

- `console` - default console host
- `avalonia` or `gui` - GUI host for interpreter execution

`SB/appsettings.json` keys:

- `ApplicationName`
- `AppSettings:InputFile`
- `AppSettings:OutputFile`
- `AppSettings:Verbose`
- `AppSettings:Backend`
- `AppSettings:RuntimeHost`
- `AppSettings:SyntaxChecking` with `relaxed` or `rigorous`
- `AppSettings:ExecutionThrottle:Enabled`
- `AppSettings:ExecutionThrottle:TargetStatementsPerSecond`
- `AppSettings:ExecutionThrottle:MaxRunAheadMilliseconds`

Examples:

```powershell
dotnet run --project .\SB\SB.fsproj -- q3
dotnet run --project .\SB\SB.fsproj -- .\q3.sb .\output\q3.c false c
dotnet run --project .\SB\SB.fsproj -- .\q3.sb .\output\q3.generated.cs false csharp
dotnet run --project .\SB\SB.fsproj -- .\q3.sb .\output\q3.exe false dotnetexe
dotnet run --project .\SB\SB.fsproj -- .\q3.sb .\unused.txt false interpret avalonia
```

## Build and test

```powershell
dotnet build .\SB.sln
dotnet test .\SBTests\SBTests.fsproj
dotnet test .\SBRuntimeTests\SBRuntimeTests.fsproj
```

Use targeted tests while iterating when possible. For generated C# backend changes, useful filters include:

```powershell
dotnet test .\SBTests\SBTests.fsproj --filter FullyQualifiedName~HirCSharpBackendTests
dotnet test .\SBTests\SBTests.fsproj --filter FullyQualifiedName~GeneratedCSharpExecutionTests
```

## Known gaps

- broad built-in coverage
- full channel/device/file semantics
- complete generated-backend parity with the interpreter
- graphics, sound, and environment-specific runtime fidelity
- faithful Sinclair QL compatibility beyond the intentionally modeled language/runtime subset
