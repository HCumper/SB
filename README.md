# SB

An F# SuperBASIC / Structured SuperBASIC toolchain targeting `.NET 10`.

## Command line

The CLI now supports a simple form.

If you run the built tool as `sb`, the basic usage is:

```powershell
sb myprog
sb myprog out.c
sb myprog out.cs
```

Behavior:

- `sb myprog`
  - searches for `myprog.bas`, then `myprog.sb`, then `myprog.ssb`
  - if no second argument is given, it interprets the program
- `sb myprog out.c`
  - resolves the input the same way
  - generates C beside the input file if `out.c` is just a file name
  - also copies `sbruntime_c.h` and `sbruntime_c.c` beside the generated `.c`
- `sb myprog out.cs`
  - resolves the input the same way
  - generates C# beside the input file if `out.cs` is just a file name
  - also copies `GeneratedRuntime.cs` beside the generated `.cs`

Examples:

```powershell
# Interpret q3.bas / q3.sb / q3.ssb
sb q3

# Generate C in the same folder as the source
sb q3 q3.c

# Generate C# in the same folder as the source
sb q3 q3.cs
```

If you are running directly from the repo rather than via a wrapper command, use:

```powershell
dotnet run --project .\SB\SB.fsproj -- <input> <output> <verbose> <backend>
```

Examples:

```powershell
# Interpret a program
dotnet run --project .\SB\SB.fsproj -- .\q3.SB .\out.txt false interpret

# Generate C
dotnet run --project .\SB\SB.fsproj -- .\q3.SB .\q3.c false c

# Generate C#
dotnet run --project .\SB\SB.fsproj -- .\q3.SB .\q3.generated.cs false csharp

# Publish a .NET executable
dotnet run --project .\SB\SB.fsproj -- .\q3.SB .\q3.exe false dotnetexe
```

Notes:

- Command-line arguments override the defaults in `SB/appsettings.json`.
- The simple two-argument form infers backend from the output extension:
  - `.c` -> `c`
  - `.cs` -> `csharp`
- The `c` backend writes the generated program and copies `sbruntime_c.h` and `sbruntime_c.c`.
- The `csharp` backend writes the generated program and copies `GeneratedRuntime.cs`.
- `dotnetexe` lowers to HIR, generates C#, includes `GeneratedRuntime.cs`, and publishes a single-file `.NET` executable.

The active codebase currently supports:

- SSB preprocessing into numbered SuperBASIC
- ANTLR-based parsing
- normalized AST construction
- semantic analysis with symbol resolution, typing, coercion, constant folding, and diagnostics
- lowering from AST to HIR
- a partial interpreter for HIR
- caller-dependent procedure argument binding in the interpreter and HIR
- `REFERENCE`-driven parameter binding metadata in semantic analysis and HIR
- interpreter support for `GOTO`, `GOSUB`, `RETURN`, `ON GOTO`, and `ON GOSUB`
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

Interpreter phases:

```text
Source (.sb / .ssb)
        |
        v
1. Load Source
   - read text
   - classify numbered vs structured
        |
        v
2. Preprocess (.ssb only)
   - structured source -> numbered SuperBASIC
        |
        v
3. Parse (ANTLR)
   - tokens + parse tree
        |
        v
4. Build AST
   - normalize syntax
   - attach source positions
        |
        v
5. Semantic Analysis
   - resolve symbols
   - apply typing/coercions
   - constant folding
   - diagnostics
        |
        v
6. Lower To HIR
   - explicit routines
   - control flow
   - data/restore tables
   - built-in calls
        |
        v
7. Interpret HIR
   - initialize runtime state
     - globals
     - frames
     - data pointer
     - host/runtime options
   - evaluate expressions
   - execute statements
   - call built-ins/runtime host
   - manage GOTO/GOSUB/RETURN/STOP
        |
        v
8. Execution Result
   - output
   - mutated runtime state
   - or runtime error
```

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
- `CSharpRuntime/GeneratedRuntime.cs` - shared C# runtime source used by generated C#
- `CRuntime/sbruntime_c.h` - shared C runtime header
- `CRuntime/sbruntime_c.c` - shared C runtime source

## Status

Implemented well enough to be useful:

- symbol and scope analysis
- many expression rules
- assignment, loops, conditionals, `DATA`/`READ`/`RESTORE`
- numbered control flow in the interpreter, including `GOTO`, `GOSUB`, `RETURN`, `ON GOTO`, and `ON GOSUB`
- flexible and by-reference parameter binding metadata through semantics and HIR
- HIR lowering for the core statement/expression set
- interpreter support for a subset of function and statement built-ins

Still incomplete:

- broad built-in coverage
- full channel/device/file semantics
- code generation parity for by-reference procedure semantics
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
    "Verbose": false,
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
