# SB

An F# SuperBASIC / Structured SuperBASIC toolchain targeting `.NET 10`.

The active codebase currently supports:

- SSB preprocessing into numbered SuperBASIC
- ANTLR-based parsing
- normalized AST construction
- semantic analysis with symbol resolution, typing, coercion, constant folding, and diagnostics
- lowering from AST to HIR
- a partial interpreter for HIR
- alternate C# generation from lowered HIR
- alternate plain C generation from lowered HIR
- published `.NET` executable generation from lowered HIR

The project is not yet a full SuperBASIC runtime. It can run a meaningful subset of programs, but many built-ins and host-specific behaviors are still incomplete.

## Solution layout

- `SB/` - main compiler/interpreter executable and core pipeline
- `SBLib/` - ANTLR grammar and generated parser artifacts
- `TestBed/` - NUnit regression tests for parser, semantics, lowering, fixtures, and interpreter behavior

## Current pipeline

The active path is:

1. load source
2. classify input as numbered SuperBASIC or Structured SuperBASIC
3. preprocess `.ssb` when needed
4. parse with ANTLR
5. convert parse tree to AST
6. run semantic analysis
7. lower AST to HIR
8. interpret HIR or emit C# / C / `.NET exe`

The CLI can also emit C#, plain C, or a published `.NET` executable from HIR instead of interpreting.

Important files in `SB/`:

- `CompilerPipeline.fs` - pipeline orchestration
- `ParseTreeVisitor.fs` - parse tree to AST lowering
- `SemanticAnalyzer.fs` - semantic pass orchestration
- `SemanticAnalysis.Symbols.fs` - scope and symbol rules
- `SemanticAnalysis.Expressions.fs` - typing, coercion, folding, and expression validation
- `AstToHir.fs` - semantic AST to HIR lowering
- `Interpreter.fs` - HIR interpreter
- `BuiltIns.fs` - built-in name/signature model
- `HirCSharpBackend.fs` - HIR to C#
- `HirCBackend.fs` - HIR to C
- `CSharpRuntime.stg` - C# runtime helper template
- `CTemplates.stg` - C runtime helper template

## Status

Implemented well enough to be useful:

- symbol and scope analysis
- many expression rules
- assignment, loops, conditionals, `DATA`/`READ`/`RESTORE`
- HIR lowering for the core statement/expression set
- interpreter support for a subset of function and statement built-ins

Still incomplete:

- broad built-in coverage
- full channel/device/file semantics
- by-reference procedure semantics
- graphics, sound, and environment-specific runtime behavior
- faithful Sinclair QL compatibility

## Build

Build the whole solution:

```powershell
dotnet build .\SB.sln
```

Build just the main project:

```powershell
dotnet build .\SB\SB.fsproj
```

## Run

Run the main executable:

```powershell
dotnet run --project .\SB\SB.fsproj
```

`SB/appsettings.json` is used by default. The backend is selected by `AppSettings:Backend` or by a fourth CLI argument:

```powershell
dotnet run --project .\SB\SB.fsproj -- input.sb output.cs false csharp
```

Supported backends:

- `interpret` - lower to HIR and run the interpreter
- `csharp` - lower to HIR and write generated C# to the configured output path
- `c` - lower to HIR and write generated C to the configured output path
- `dotnetexe` - lower to HIR, generate C#, and publish a single-file `.NET` executable to the configured output path

Example:

```powershell
dotnet run --project .\SB\SB.fsproj -- input.sb output.exe false dotnetexe
```

Or via `SB/appsettings.json`:

```json
{
  "ApplicationName": "SB",
  "AppSettings": {
    "InputFile": "C:\\Source\\SB\\SB\\q3.sb",
    "OutputFile": "C:\\Source\\SB\\SB\\q3-generated.exe",
    "TemplatesFile": "C:\\Source\\SB\\SB\\CSharpTemplates.stg",
    "Verbose": true,
    "Backend": "dotnetexe"
  }
}
```

## Test

Run the main test suite:

```powershell
dotnet test .\TestBed\TestBed.fsproj
```

The `TestBed` project covers:

- parse-tree to AST behavior
- semantic analysis
- AST to HIR lowering
- interpreter behavior
- fixture programs such as `q3.SB`, `Golfer.sb`, `Project Planner.sb`, and `ssb272.ssb`

## Notes

- The grammar is in `SBLib/SB.g4`.
- The generated ANTLR Java files in `SBLib/` are repository artifacts, not the main implementation surface.
- The most active implementation areas are `SB/` and `TestBed/`.
- The most natural next step is a registry/services-based runtime architecture for broader built-in coverage under `.NET`.
