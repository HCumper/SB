# SB

An F# compiler pipeline for SuperBASIC and Structured SuperBASIC (`.ssb`) source.

The current implementation can:

- preprocess derivative `.ssb` input into numbered SuperBASIC source
- parse SuperBASIC with ANTLR
- build a normalized AST
- run semantic analysis with symbol resolution, expression typing, coercion, constant folding, and diagnostics

There is no backend yet. The semantic layer is substantially richer than code generation, and some language constructs still lower to placeholder comments in the generated C#.

## Repository layout

- `SB/`: main F# compiler executable
- `SBLib/`: ANTLR grammar project and generated parser support
- `Runtime/`: small runtime/support library for generated output
- `TestBed/`: current NUnit-based test suite

## Current pipeline

The active compiler path is:

1. load source from `appsettings.json` or command-line arguments
2. classify input as numbered SuperBASIC or Structured SuperBASIC
3. preprocess `.ssb` input through the SSB compatibility pass
4. parse with ANTLR
5. convert the parse tree to the F# AST via `ParseTreeVisitor.fs`
6. run semantic analysis

Key modules in `SB/`:

- `CompilerPipeline.fs`: stage orchestration and CLI-facing pipeline
- `ParseTreeVisitor.fs`: parse tree to AST lowering
- `SemanticAnalyzer.fs`: semantic pass orchestration
- `SemanticAnalysis.Symbols.fs`: scope lookup and symbol updates
- `SemanticAnalysis.Expressions.fs`: expression typing, coercion, folding, and validation
- `SemanticAnalysis.Facts.fs`: semantic facts and diagnostics helpers
- `TypeAnalyzer.fs`: implicit typing normalization
- `SSB.fs`: legacy Structured SuperBASIC preprocessor

## Semantic analysis status

The semantic pass currently covers:

- declaration collection for globals, locals, procedures, and functions
- symbol resolution across scopes
- expression result typing
- SuperBASIC-style coercion for operators
- constant folding where values are statically knowable
- scalar vs array usage validation
- call arity checks where signatures are known
- built-in argument validation for a conservative subset
- assignment target validation for non-writable symbols
- function/procedure usage-shape validation
- structured semantic diagnostics

Representative fixture programs used in tests include:

- `SB/q3.SB`
- `SB/Golfer.sb`
- `SB/Project Planner.sb`
- `SB/ssb272.ssb`

## Code generation status

Pending


The solution targets `.NET 10`.

Build the solution:

```powershell
dotnet build .\SB.sln
```

Build just the compiler:

```powershell
dotnet build .\SB\SB.fsproj
```

## Run

By default the compiler reads settings from `SB/appsettings.json`. That file currently points at local sample inputs in this repository.

Run using `appsettings.json`:

```powershell
dotnet run --project .\SB\SB.fsproj
```

Run with explicit arguments:

```powershell
dotnet run --project .\SB\SB.fsproj -- "C:\path\to\input.sb" "C:\path\to\output.cs" "true"
```

Argument order:

1. input file
2. output file
3. verbose flag

Note: the CLI currently writes generated C# to standard output. The output file setting is still used by some pipeline components and reflects the older tool shape, but `Program.fs` currently prints generated code rather than writing it directly to `OutputFile`.

## Test

Run the main test suite:

```powershell
dotnet test .\TestBed\TestBed.fsproj
```

The `TestBed` project covers:

- parse-tree to AST behavior
- AST diagnostics
- semantic analysis
- pipeline classification and preprocessing
- real-program fixture coverage

## Parser generation

The grammar lives in `SBLib/SB.g4`.

`SBLib` is configured to generate both ANTLR listeners and visitors, but the active compiler path uses the visitor-based AST builder in `SB/ParseTreeVisitor.fs`.

## Notes

- The repo still contains historical and transitional pieces. The active work is in `SB/`, `SBLib/`, `Runtime/`, and `TestBed/`.
- The semantic layer is ahead of the backend. If you are extending the compiler, semantic normalization and a real IR are the next natural areas of work.
