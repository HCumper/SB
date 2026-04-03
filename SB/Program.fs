module Program

open System
open System.IO

open CompilerPipeline
open HIRPrettyPrinter
open Interpreter
open SymbolTableManager
open SemanticAnalysisFacts
open SBAvaloniaHost

let private copyBundledCRuntime (outputFileName: string) =
    let outputDirectory =
        let directory = Path.GetDirectoryName(outputFileName)
        if String.IsNullOrWhiteSpace directory then Directory.GetCurrentDirectory() else directory

    let runtimeDirectory = Path.Combine(AppContext.BaseDirectory, "CRuntime")
    let headerSourcePath = Path.Combine(runtimeDirectory, HirCBackend.cRuntimeHeaderFileName)
    let sourceSourcePath = Path.Combine(runtimeDirectory, HirCBackend.cRuntimeSourceFileName)
    let headerTargetPath = Path.Combine(outputDirectory, HirCBackend.cRuntimeHeaderFileName)
    let sourceTargetPath = Path.Combine(outputDirectory, HirCBackend.cRuntimeSourceFileName)

    if not (File.Exists(headerSourcePath)) then
        failwith $"Bundled C runtime header not found: {headerSourcePath}"

    if not (File.Exists(sourceSourcePath)) then
        failwith $"Bundled C runtime source not found: {sourceSourcePath}"

    File.Copy(headerSourcePath, headerTargetPath, true)
    File.Copy(sourceSourcePath, sourceTargetPath, true)
    headerTargetPath, sourceTargetPath

let private copyBundledCSharpRuntime (outputFileName: string) =
    let outputDirectory =
        let directory = Path.GetDirectoryName(outputFileName)
        if String.IsNullOrWhiteSpace directory then Directory.GetCurrentDirectory() else directory

    let runtimeDirectory = Path.Combine(AppContext.BaseDirectory, "CSharpRuntime")
    let sourcePath = Path.Combine(runtimeDirectory, HirCSharpBackend.cSharpRuntimeFileName)
    let targetPath = Path.Combine(outputDirectory, HirCSharpBackend.cSharpRuntimeFileName)

    if not (File.Exists(sourcePath)) then
        failwith $"Bundled C# runtime source not found: {sourcePath}"

    File.Copy(sourcePath, targetPath, true)
    targetPath

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

let private normalizeRuntimeHostName (runtimeHost: string) =
    if String.IsNullOrWhiteSpace runtimeHost then
        "console"
    else
        runtimeHost.Trim().ToLowerInvariant()

let private waitForAvaloniaContinue (handle: AvaloniaHostHandle) =
    match handle.Host.Channels.Get(SBRuntime.ChannelId 0) with
    | Result.Ok channel ->
        channel.WriteText("Press Enter to continue")
        match channel with
        | :? SBRuntime.IScreenChannel as screenChannel -> screenChannel.NewLine()
        | _ -> ()
    | Result.Error _ -> ()

    let rec waitLoop () =
        match handle.Host.Input.ReadLine() with
        | Some _ -> ()
        | None ->
            System.Threading.Thread.Sleep(50)
            waitLoop ()

    waitLoop ()

let rec private blockContainsInput (block: HIR.HirBlock) =
    block
    |> List.exists (fun (stmt: HIR.HirStmt) ->
        match stmt with
        | HIR.HirStmt.Input(_, _, _, _) -> true
        | HIR.HirStmt.WhenError(body, _) -> blockContainsInput body
        | HIR.HirStmt.If(_, thenBlock, elseBlock, _) ->
            blockContainsInput thenBlock
            || (elseBlock |> Option.exists blockContainsInput)
        | HIR.HirStmt.For(_, _, _, _, _, body, _) -> blockContainsInput body
        | HIR.HirStmt.ForSequence(_, _, _, _, _, _, _, body, _) -> blockContainsInput body
        | HIR.HirStmt.Repeat(_, _, body, _) -> blockContainsInput body
        | _ -> false)

let private programContainsInput (program: HIR.HirProgram) =
    blockContainsInput program.Main
    || (program.Routines |> List.exists (fun routine -> blockContainsInput routine.Body))

let private watchAvaloniaHostExit (handle: AvaloniaHostHandle) =
    let shuttingDown = ref false

    let watcher =
        System.Threading.Thread(System.Threading.ThreadStart(fun () ->
            handle.WaitForExit()
            if not !shuttingDown then
                Environment.Exit(0)))

    watcher.IsBackground <- true
    watcher.Start()

    fun () -> shuttingDown := true

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
                    let hostHandle =
                        match normalizeRuntimeHostName settings.RuntimeHost with
                        | "avalonia"
                        | "gui" -> Some(AvaloniaHostService().Start())
                        | _ -> None

                    let markHostShutdown =
                        hostHandle
                        |> Option.map watchAvaloniaHostExit

                    let exitCode =
                        let options =
                            match hostHandle with
                            | Some handle ->
                                { defaultRuntimeOptions with
                                    Host = handle.Host
                                    Sleeper = fun milliseconds -> System.Threading.Thread.Sleep(milliseconds)
                                    InitialSourceProgram = Some ast
                                    LoadProgram = loadRuntimeProgram settings.SyntaxChecking settings.Verbose settings.Logger
                                    MergeProgram =
                                        fun (currentAst, path) ->
                                            loadRuntimeProgram settings.SyntaxChecking settings.Verbose settings.Logger path
                                            |> Result.bind (fun loaded ->
                                                let mergedAst = mergeRuntimeAst currentAst loaded.Ast
                                                lowerAstForRuntime mergedAst
                                                |> Result.map (fun hir -> { Ast = mergedAst; Hir = hir })) }
                            | None ->
                                { defaultRuntimeOptions with
                                    InitialSourceProgram = Some ast
                                    LoadProgram = loadRuntimeProgram settings.SyntaxChecking settings.Verbose settings.Logger
                                    MergeProgram =
                                        fun (currentAst, path) ->
                                            loadRuntimeProgram settings.SyntaxChecking settings.Verbose settings.Logger path
                                            |> Result.bind (fun loaded ->
                                                let mergedAst = mergeRuntimeAst currentAst loaded.Ast
                                                lowerAstForRuntime mergedAst
                                                |> Result.map (fun hir -> { Ast = mergedAst; Hir = hir })) }

                        match interpretProgramWithOptions options hirProgram with
                        | Ok _ -> 0
                        | Error runtimeError ->
                            Console.Error.WriteLine("Runtime failed:")
                            Console.Error.WriteLine(formatRuntimeError runtimeError)
                            1

                    hostHandle
                    |> Option.iter (fun handle ->
                        if not (programContainsInput hirProgram) then
                            waitForAvaloniaContinue handle
                        markHostShutdown |> Option.iter (fun mark -> mark ())
                        (handle :> IDisposable).Dispose())
                    exitCode
                | "csharp" ->
                    let generated = generateCSharpFromLoweredHir settings.AppName hirProgram
                    File.WriteAllText(settings.OutputFileName, generated)
                    let runtimePath = copyBundledCSharpRuntime settings.OutputFileName
                    if settings.Verbose then
                        Console.WriteLine($"Generated C# written to {settings.OutputFileName}")
                        Console.WriteLine($"Shared C# runtime written to {runtimePath}")
                    0
                | "c" ->
                    let generated = generateCFromLoweredHir settings.AppName hirProgram
                    File.WriteAllText(settings.OutputFileName, generated)
                    let headerPath, sourcePath = copyBundledCRuntime settings.OutputFileName
                    if settings.Verbose then
                        Console.WriteLine($"Generated C written to {settings.OutputFileName}")
                        Console.WriteLine($"Shared C runtime written to {headerPath} and {sourcePath}")
                    0
                | "dotnetexe"
                | "dotnet-exe"
                | "exe" ->
                    match generateDotNetExeFromLoweredHir settings.AppName settings.OutputFileName hirProgram with
                    | Result.Ok generatedPath ->
                        if settings.Verbose then
                            Console.WriteLine($".NET executable written to {generatedPath}")
                        0
                    | Result.Error message ->
                        Console.Error.WriteLine("Failed to generate .NET executable:")
                        Console.Error.WriteLine(message : string)
                        1
                | backend ->
                    Console.Error.WriteLine($"Unknown backend '{backend}'. Supported backends: interpret, csharp, c, dotnetexe.")
                    1
