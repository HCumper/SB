# SB

F# SuperBASIC / Structured SuperBASIC toolchain targeting `.NET 10`.

## Current capabilities

- preprocesses `.ssb` into numbered SuperBASIC
- parses with ANTLR
- builds a normalized AST
- runs semantic analysis with symbol resolution, typing, coercion, constant folding, and diagnostics
- lowers AST to HIR
- interprets a subset of HIR
- generates alternate C# from lowered HIR
- generates alternate plain C from lowered HIR
- generates a published `.NET` executable from lowered HIR

The runtime is incomplete. Many built-ins and host-specific behaviors are still partial.

## Solution layout

- `SB/` - main compiler/interpreter executable and compiler pipeline
- `SBLib/` - grammar and generated parser artifacts
- `TestBed/` - NUnit regression tests

## Active pipeline

1. load source
2. classify as numbered SuperBASIC or Structured SuperBASIC
3. preprocess `.ssb` when needed
4. parse with ANTLR
5. lower parse tree to AST
6. run semantic analysis
7. lower AST to HIR
8. either interpret HIR or emit C# / C / `.NET exe`

## Important files

- `SB/CompilerPipeline.fs` - stage orchestration and backend selection helpers
- `SB/Program.fs` - CLI entry point and backend dispatch
- `SB/ParseTreeVisitor.fs` - parse tree to AST
- `SB/SemanticAnalyzer.fs` - semantic pass orchestration
- `SB/SemanticAnalysis.Symbols.fs` - scope and symbol rules
- `SB/SemanticAnalysis.Expressions.fs` - expression typing/coercion/folding
- `SB/AstToHir.fs` - AST to HIR lowering
- `SB/Interpreter.fs` - HIR interpreter
- `SB/HirCSharpBackend.fs` - HIR to C#
- `SB/HirCBackend.fs` - HIR to C
- `SB/CSharpRuntime.stg` - C# runtime helper template
- `SB/CTemplates.stg` - C runtime helper template

## Backends

- `interpret` - lower to HIR and run the interpreter
- `csharp` - lower to HIR and write generated C# to the output path
- `c` - lower to HIR and write generated C to the output path
- `dotnetexe` - lower to HIR, generate C#, and publish a single-file `.NET` executable to the output path

`SB/appsettings.json` provides defaults. The CLI also accepts a fourth argument for backend selection:

```powershell
dotnet run --project .\SB\SB.fsproj -- input.sb output.c false c
```

Example for the `.NET exe` backend:

```powershell
dotnet run --project .\SB\SB.fsproj -- input.sb output.exe false dotnetexe
```

## Build and test

```powershell
dotnet build .\SB.sln
dotnet test .\TestBed\TestBed.fsproj
```

## Known gaps

- broad built-in coverage
- full channel/device/file semantics
- by-reference procedure semantics
- graphics, sound, and environment-specific runtime behavior
- faithful Sinclair QL compatibility
