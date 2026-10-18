# SB Architecture

This document gives an overall view of the `SB` solution: the project-level structure and the active compiler/runtime pipeline.

## Solution Overview

```mermaid
flowchart LR
    Source["SuperBASIC source<br/>.bas / .sb / .ssb"]

    subgraph Solution["SB solution"]
        SB["SB<br/>CLI + compiler pipeline + interpreter + backends"]
        SBLib["SBLib<br/>ANTLR grammar + generated parser"]
        SBRuntime["SBRuntime<br/>shared managed runtime services"]
        Avalonia["SB.AvaloniaHost<br/>optional GUI runtime host"]
        CSharpRt["SB.CSharpRuntime<br/>generated C# runtime support"]
        SBTests["SBTests<br/>compiler/backend/execution tests"]
        SBRuntimeTests["SBRuntimeTests<br/>runtime + host tests"]
        TestBed["TestBed<br/>legacy fixtures / experiments"]
    end

    Source --> SB

    SB --> SBLib
    SB --> SBRuntime
    SB --> Avalonia
    SB --> CSharpRt
    CSharpRt --> SBRuntime

    SBTests --> SB
    SBRuntimeTests --> SB
    SBRuntimeTests --> SBRuntime
    SBRuntimeTests --> Avalonia

    TestBed -. ad hoc fixtures .-> SB
```

## Active Pipeline

```mermaid
flowchart TD
    Input["Input source<br/>.bas / .sb / .ssb"]
    Classify["Source classification"]
    Preprocess["Structured SuperBASIC preprocessor<br/>(.ssb -> numbered source)"]
    Parse["ANTLR parse<br/>(SBLib)"]
    Ast["Normalized AST"]
    Semantic["Semantic analysis<br/>symbols, typing, coercion, folding, diagnostics"]
    Hir["HIR lowering"]
    Dispatch{"Selected backend"}

    Interpret["Interpreter"]
    Console["Console host"]
    Gui["Avalonia host"]

    CSharp["C# backend"]
    CFiles["Generated .cs + GeneratedRuntime.cs<br/>+ SBRuntime.dll + FSharp.Core.dll"]

    CBackend["C backend"]
    CArtifacts["Generated .c + sbruntime_c.h/.c"]

    DotNetExe[".NET exe backend"]
    PublishedExe["Published single-file .NET executable"]

    Input --> Classify
    Classify -->|.ssb| Preprocess
    Classify -->|.bas / .sb| Parse
    Preprocess --> Parse
    Parse --> Ast
    Ast --> Semantic
    Semantic --> Hir
    Hir --> Dispatch

    Dispatch -->|interpret| Interpret
    Interpret --> Console
    Interpret --> Gui

    Dispatch -->|csharp| CSharp
    CSharp --> CFiles

    Dispatch -->|c| CBackend
    CBackend --> CArtifacts

    Dispatch -->|dotnetexe| DotNetExe
    DotNetExe --> PublishedExe
```

## Main Responsibilities

- `SB/Program.fs` handles CLI argument binding, appsettings fallback, and backend dispatch.
- `SB/CompilerPipeline.fs` coordinates source loading, preprocessing, parsing, semantic analysis, and HIR lowering.
- `SB/ParseTreeVisitor.fs` converts the ANTLR parse tree into the normalized AST.
- `SB/SemanticAnalyzer.fs` and related semantic modules resolve symbols, types, coercions, and diagnostics.
- `SB/AstToHir.fs` lowers the AST into HIR for execution or code generation.
- `SB/Interpreter.fs` executes HIR directly.
- `SB/HirCSharpBackend.fs` and `SB/HirCBackend.fs` emit alternate generated backends.
- `SBRuntime/` provides shared managed runtime behavior used by the interpreter and generated C# support.

## Notes

- `SB` is a modern language toolchain, not a Sinclair QL emulator.
- The interpreter is the most direct execution path and should be considered the reference runtime where backend parity is incomplete.
- Generated backends depend on copied runtime artifacts rather than reproducing the full interpreter stack.
