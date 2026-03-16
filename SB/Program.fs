module Program

open System

open Antlr4.StringTemplate
open CodeGenerator
open CompilerPipeline

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
        let group: TemplateGroup = createTemplateGroup settings
        let state = runSemanticAnalysis ast
        let generated = generateCSharp state settings.TemplateFileName
        logDiagnostics { InputFile = settings.InputFileName; OutputFile = settings.OutputFileName; Verbose = settings.Verbose } (parseTree, inputStream) ast
        Console.WriteLine(generated)
        1
