module Program

open System
open System.IO

open CompilerPipeline
open HIRPrettyPrinter
open Interpreter
open SymbolTableManager
open SemanticAnalysisFacts

// Program is the thin CLI shell over the compiler pipeline.
//
// It parses runtime settings, runs the pipeline, prints diagnostics or generated
// output, and sets the process exit code. All real compiler work is delegated to
// the stage modules.
//
// Thin CLI entry point over the main compiler pipeline.
let private printParseError error =
    match error with
    | FileNotFound fileName ->
        printfn $"Error: File '%s{fileName}' not found."
        1
    | ParseError errorMsg ->
        printfn $"Parsing failed with error: %s{errorMsg}"
        1

let private formatHirLoweringError (error: HirLoweringError) =
    let location =
        match error.Position with
        | Some pos -> $" at %d{pos.EditorLineNo}:%d{pos.Column}"
        | None -> String.Empty

    $"[{error.Scope}] {error.Message}{location}"

let private formatRuntimeError (error: RuntimeError) =
    let location =
        match error.Position with
        | Some pos -> $" at %d{pos.EditorLineNo}:%d{pos.Column}"
        | None -> String.Empty

    $"{error.Message}{location}"

let private normalizeBackendName (backend: string) =
    if String.IsNullOrWhiteSpace backend then
        "interpret"
    else
        backend.Trim().ToLowerInvariant()

[<EntryPoint>]
let main argv =
    // The CLI currently stops after semantic analysis failure and only emits
    // generated C# when the semantic pipeline reports a clean enough result.
    let settings = getSettings argv

    match loadAstFromInput settings with
    | Error parseError ->
        printParseError parseError
    | Ok(parseTree, inputStream, ast) ->
        let state = runSemanticAnalysis ast
        logDiagnostics { InputFile = settings.InputFileName; OutputFile = settings.OutputFileName; Verbose = settings.Verbose } (parseTree, inputStream) ast
        if settings.Verbose then
            printSymbolTable state.SymTab
        if not state.Errors.IsEmpty then
            Console.Error.WriteLine("Semantic analysis failed:")
            if state.Diagnostics.IsEmpty then
                state.Errors |> List.iter (Console.Error.WriteLine)
            else
                state.Diagnostics |> List.iter (formatDiagnostic >> Console.Error.WriteLine)
            1
        else
            match runHirLowering state with
            | Error loweringErrors ->
                Console.Error.WriteLine("HIR lowering failed:")
                loweringErrors
                |> List.iter (formatHirLoweringError >> Console.Error.WriteLine)
                1
            | Ok hirProgram ->
                if settings.Verbose then
                    Console.WriteLine("HIR:")
                    Console.WriteLine(prettyPrintHir hirProgram)
                    Console.WriteLine($"HIR lowering succeeded. Globals={hirProgram.Globals.Length}, Routines={hirProgram.Routines.Length}, DataEntries={hirProgram.DataEntries.Length}, MainStatements={hirProgram.Main.Length}")
                match normalizeBackendName settings.Backend with
                | "interpret" ->
                    match interpretProgram hirProgram with
                    | Ok _ -> 0
                    | Error runtimeError ->
                        Console.Error.WriteLine("Runtime failed:")
                        Console.Error.WriteLine(formatRuntimeError runtimeError)
                        1
                | "csharp" ->
                    let generated = generateCSharpFromLoweredHir settings.AppName hirProgram
                    File.WriteAllText(settings.OutputFileName, generated)
                    if settings.Verbose then
                        Console.WriteLine($"Generated C# written to {settings.OutputFileName}")
                    0
                | "c" ->
                    let generated = generateCFromLoweredHir settings.AppName hirProgram
                    File.WriteAllText(settings.OutputFileName, generated)
                    if settings.Verbose then
                        Console.WriteLine($"Generated C written to {settings.OutputFileName}")
                    0
                | backend ->
                    Console.Error.WriteLine($"Unknown backend '{backend}'. Supported backends: interpret, csharp, c.")
                    1
