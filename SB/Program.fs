module Program

open System

open CompilerPipeline
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
//            let generated = generateCSharp state settings.TemplateFileName
//            Console.WriteLine(generated)
            0
