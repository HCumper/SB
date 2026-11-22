#r "../../SB/bin/Debug/net10.0/SBRuntime.dll"
#r "../../SB/bin/Debug/net10.0/Antlr4.Runtime.Standard.dll"
#r "../../SB/bin/Debug/net10.0/FSharpPlus.dll"
#r "../../SB/bin/Debug/net10.0/SBLib.dll"
#r "../../SB/bin/Debug/net10.0/Serilog.dll"
#r "../../SB/bin/Debug/net10.0/SB.dll"
#r "../../SBTests/bin/Debug/net10.0/SBTests.dll"

// Run through Run-Benchmarks.ps1 so the compiler and execution harness are built first.
open System
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open SBTests.TurboExecutionHarness

[<CLIMutable>]
type Benchmark = {
    name: string
    file: string
    description: string
    passes: int
    quickPasses: int
    expectedChecksum: int
    quickChecksum: int
    requiresRom: bool
}

[<CLIMutable>]
type Suite = { formatVersion: int; provenance: string; benchmarks: Benchmark array }

type Row = {
    Benchmark: string
    HirOptimization: string
    NativeRegions: string
    RegisterAllocation: string
    Passes: int
    ExpectedChecksum: int
    Status: string
    ExecutableBytes: int
    Instructions: Nullable<int64>
    InstructionWordsFetched: Nullable<int64>
    Output: string
    Detail: string
}

let suiteDirectory = __SOURCE_DIRECTORY__
let repository = Path.GetFullPath(Path.Combine(suiteDirectory, "..", ".."))
let mutable outputDirectory = Path.Combine(repository, "artifacts", "turbo-benchmarks")
let mutable quick = false
let mutable compareNative = false
let mutable compareRegisters = false
let mutable maxSteps = 50000000L
let names = Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)
let rec parse = function
    | [] -> ()
    | "--quick" :: rest -> quick <- true; parse rest
    | "--compare-native" :: rest -> compareNative <- true; parse rest
    | "--compare-registers" :: rest -> compareNative <- true; compareRegisters <- true; parse rest
    | "--output" :: path :: rest -> outputDirectory <- Path.GetFullPath path; parse rest
    | "--max-steps" :: value :: rest -> maxSteps <- Int64.Parse value; parse rest
    | "--name" :: name :: rest -> names.Add name |> ignore; parse rest
    | other -> failwithf "Unrecognized benchmark arguments: %A" other
parse (fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray)
if maxSteps <= 0L then invalidArg "--max-steps" "The instruction limit must be positive."

let jsonOptions = JsonSerializerOptions(WriteIndented = true)
let suite = JsonSerializer.Deserialize<Suite>(File.ReadAllText(Path.Combine(suiteDirectory, "suite.json")))
if suite.formatVersion <> 1 then failwith "Unsupported benchmark manifest version."
for name in names do
    if not (suite.benchmarks |> Array.exists (fun b -> b.name.Equals(name, StringComparison.OrdinalIgnoreCase))) then
        failwith $"Unknown benchmark '{name}'."
let benchmarks = suite.benchmarks |> Array.filter (fun b -> names.Count = 0 || names.Contains b.name)
let runDirectory = Path.Combine(outputDirectory, if quick then "quick" else "default")
Directory.CreateDirectory runDirectory |> ignore

let compile enabled native registers (input: string) (output: string) (dumpPrefix: string) (logPath: string) =
    let start = ProcessStartInfo("dotnet")
    start.WorkingDirectory <- Path.GetDirectoryName output
    start.UseShellExecute <- false
    start.RedirectStandardOutput <- true
    start.RedirectStandardError <- true
    for argument in [ typeof<TurboCodegen.TurboCodegenOutput>.Assembly.Location; input; output; "true"; "turbo"
                      (if enabled then "--hir-opt" else "--no-hir-opt")
                      (if native then "--native-regions" else "--no-native-regions")
                      (if registers then "--regalloc" else "--no-regalloc")
                      "--dump-hir=" + dumpPrefix ] do
        start.ArgumentList.Add argument
    use compiler = new Process(StartInfo = start)
    if not (compiler.Start()) then failwith "Could not start the SB CLI."
    let stdout, stderr = compiler.StandardOutput.ReadToEndAsync(), compiler.StandardError.ReadToEndAsync()
    if not (compiler.WaitForExit 30000) then
        compiler.Kill(true)
        compiler.WaitForExit()
        failwith "The SB compiler exceeded 30 seconds."
    let diagnostics = stdout.GetAwaiter().GetResult() + stderr.GetAwaiter().GetResult()
    File.WriteAllText(logPath, diagnostics)
    if compiler.ExitCode <> 0 then failwith $"Compilation failed; see {logPath}."
    File.ReadAllBytes output

let results = ResizeArray<Row>()
for benchmark in benchmarks do
    let passes = if quick then benchmark.quickPasses else benchmark.passes
    let expected = if quick then benchmark.quickChecksum else benchmark.expectedChecksum
    let original = File.ReadAllText(Path.Combine(suiteDirectory, benchmark.file))
    // Only the fixed workload parameter is changed; original fixtures remain untouched.
    let parameter = Regex("(?m)^20 passes% = [0-9]+\\r?$")
    if parameter.Matches(original).Count <> 1 then failwith $"Missing workload parameter in {benchmark.file}."
    let source = parameter.Replace(original, $"20 passes%% = {passes}")
    let codeModes=if compareRegisters then ["disabled","disabled";"enabled","disabled";"enabled","enabled"] elif compareNative then ["disabled","disabled";"enabled","disabled"] else ["disabled","disabled"]
    for mode,nativeMode,registerMode in [for mode in ["disabled";"enabled"] do for native,registers in codeModes do yield mode,native,registers] do
        let modeDirectory = if compareRegisters then mode+"-native-"+nativeMode+"-registers-"+registerMode elif compareNative then mode + "-native-" + nativeMode else mode
        let directory = Path.Combine(runDirectory, benchmark.name, modeDirectory)
        Directory.CreateDirectory directory |> ignore
        let input = Path.Combine(directory, "program.bas")
        let output = Path.Combine(directory, "bench_exe")
        File.WriteAllText(input, source)
        let mutable size = 0
        try
            let executable = compile (mode = "enabled") (nativeMode = "enabled") (registerMode = "enabled") input output (Path.Combine(directory, mode)) (Path.Combine(directory, "compiler.log"))
            size <- executable.Length
            if benchmark.requiresRom then
                results.Add {
                    Benchmark=benchmark.name; HirOptimization=mode; NativeRegions=nativeMode; RegisterAllocation=registerMode; Passes=passes; ExpectedChecksum=expected
                    Status="compiled-only"; ExecutableBytes=size; Instructions=Nullable(); InstructionWordsFetched=Nullable(); Output=""
                    Detail="Requires a full QL ROM; execution and timing remain unverified." }
                printfn "COMPILED %s [%s]: %d bytes, full QL ROM required" benchmark.name modeDirectory size
            else
                let executed = run maxSteps executable
                let text = executed.Output.Replace("\r\n", "\n")
                let wanted = $"{benchmark.name}:{expected}\n"
                if executed.ExitCode <> 0 || text <> wanted then
                    failwith $"Expected {wanted.Trim()}, got '{text.Trim()}' (exit {executed.ExitCode})."
                results.Add {
                    Benchmark=benchmark.name; HirOptimization=mode; NativeRegions=nativeMode; RegisterAllocation=registerMode; Passes=passes; ExpectedChecksum=expected
                    Status="passed"; ExecutableBytes=size; Instructions=Nullable executed.Steps
                    InstructionWordsFetched=Nullable executed.InstructionWordsFetched
                    Output=text; Detail="Functional 68000 instruction count includes startup and final output; it is not MC68008 timing." }
                printfn "PASS %s [%s]: checksum %d, %d bytes, %d instructions, %d instruction words fetched" benchmark.name modeDirectory expected size executed.Steps executed.InstructionWordsFetched
        with error ->
            results.Add {
                Benchmark=benchmark.name; HirOptimization=mode; NativeRegions=nativeMode; RegisterAllocation=registerMode; Passes=passes; ExpectedChecksum=expected
                Status="failed"; ExecutableBytes=size; Instructions=Nullable(); InstructionWordsFetched=Nullable(); Output=""; Detail=error.Message }
            printfn "FAIL %s [%s]: %s" benchmark.name modeDirectory error.Message

let quote (value: string) = "\"" + value.Replace("\"", "\"\"") + "\""
let csv =
    [| yield "Benchmark,HirOptimization,NativeRegions,RegisterAllocation,Passes,ExpectedChecksum,Status,ExecutableBytes,Instructions,InstructionWordsFetched,Output,Detail"
       for row in results do
           yield [ row.Benchmark; row.HirOptimization; row.NativeRegions; row.RegisterAllocation; string row.Passes; string row.ExpectedChecksum; row.Status
                   string row.ExecutableBytes; (if row.Instructions.HasValue then string row.Instructions.Value else "")
                   (if row.InstructionWordsFetched.HasValue then string row.InstructionWordsFetched.Value else "")
                   row.Output.TrimEnd(); row.Detail ] |> List.map quote |> String.concat "," |]
File.WriteAllLines(Path.Combine(runDirectory, "results.csv"), csv)
File.WriteAllText(Path.Combine(runDirectory, "results.json"), JsonSerializer.Serialize(results.ToArray(), jsonOptions))
let fingerprint path = File.ReadAllBytes path |> Security.Cryptography.SHA256.HashData |> Convert.ToHexString
let compilerPath = typeof<TurboCodegen.TurboCodegenOutput>.Assembly.Location
let configuration = {|
    RecordedUtc = DateTimeOffset.UtcNow
    Workload = if quick then "quick" else "default"
    Runtime = Runtime.InteropServices.RuntimeInformation.FrameworkDescription
    Host = Runtime.InteropServices.RuntimeInformation.OSDescription
    CompilerSha256 = fingerprint compilerPath
    HarnessSha256 = fingerprint typeof<ExecutionResult>.Assembly.Location
    ManifestSha256 = fingerprint (Path.Combine(suiteDirectory, "suite.json"))
    AppsettingsSha256 = fingerprint (Path.Combine(Path.GetDirectoryName compilerPath, "appsettings.json"))
    TemplateLibrary = "lib537"
    CodeMode = if compareRegisters then "Threaded, native templates, allocated native IR" elif compareNative then "Threaded and audited native regions" else "Threaded executable body"
    TimingModel = "Functional MC68000 instruction counts; no calibrated MC68008 cycle model"
    FetchModel = "16-bit opcode and extension words consumed; excludes data reads, host hooks and speculative prefetch"
    HirModes = [| "--no-hir-opt"; "--hir-opt" |]
    NativeModes = if compareNative then [|"--no-native-regions";"--native-regions"|] else [|"--no-native-regions"|]
    RegisterModes = if compareRegisters then [|"--no-regalloc";"--regalloc"|] else [|"--no-regalloc"|]
    MaxSteps = maxSteps
|}
File.WriteAllText(Path.Combine(runDirectory, "configuration.json"), JsonSerializer.Serialize(configuration, jsonOptions))
printfn "\nInstruction comparisons (functional counts, not QL speed estimates):"
for benchmark in benchmarks do
    let rows = results |> Seq.filter (fun r -> r.Benchmark = benchmark.name && r.NativeRegions="disabled") |> Array.ofSeq
    if rows.Length = 2 && (rows |> Array.forall (fun r -> r.Status = "passed")) then
        let before, after = rows[0].Instructions.Value, rows[1].Instructions.Value
        let reduction = 100.0 * float (before - after) / float before
        printfn "  %-24s %10d -> %10d (%6.2f%% fewer)" benchmark.name before after reduction
if compareNative then
    printfn "\nAudited native-region comparisons with HIR optimization enabled:"
    for benchmark in benchmarks do
        let rows = results |> Seq.filter (fun r -> r.Benchmark=benchmark.name && r.HirOptimization="enabled" && r.RegisterAllocation="disabled") |> Seq.toArray
        if rows.Length=2 && (rows |> Array.forall (fun r -> r.Status="passed")) then
            let before,after = rows[0].Instructions.Value,rows[1].Instructions.Value
            printfn "  %-24s %10d -> %10d (%6.2f%% fewer), bytes %d -> %d" benchmark.name before after (100.0*float(before-after)/float before) rows[0].ExecutableBytes rows[1].ExecutableBytes
if compareRegisters then
    printfn "\nLocal register allocation compared with native templates (HIR enabled):"
    for benchmark in benchmarks do
        let rows=results |> Seq.filter (fun r -> r.Benchmark=benchmark.name && r.HirOptimization="enabled" && r.NativeRegions="enabled") |> Seq.toArray
        if rows.Length=2 && (rows |> Array.forall (fun r -> r.Status="passed")) then
            let before,after=rows[0].Instructions.Value,rows[1].Instructions.Value
            printfn "  %-24s %10d -> %10d (%6.2f%% fewer), bytes %d -> %d" benchmark.name before after (100.0*float(before-after)/float before) rows[0].ExecutableBytes rows[1].ExecutableBytes
let passed = results |> Seq.filter (fun r -> r.Status = "passed") |> Seq.length
let compiled = results |> Seq.filter (fun r -> r.Status = "compiled-only") |> Seq.length
let failed = results |> Seq.filter (fun r -> r.Status = "failed") |> Seq.length
printfn "\n%d executions passed; %d ROM executables compiled only; %d failures." passed compiled failed
printfn "Results: %s" runDirectory
if failed <> 0 then exit 1
