module Program

open System

open CodeGenerator
open CompilerPipeline
open SymbolTableManager
open SemanticAnalysisFacts

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
            let generated = generateCSharp state settings.TemplateFileName
            Console.WriteLine(generated)
            0
