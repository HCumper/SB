#r "C:/Source/SB/SBTests/bin/Debug/net10.0/SBTests.dll"
open System.IO
open System.Text.Json
open SBTests.TurboExecutionHarness
let root = @"C:\Source\codex-workspace\.scratch\turbo-local-eligibility\comparison"
type Case = {Name:string;Task:string;Output:string}
let cases=JsonSerializer.Deserialize<Case[]>(File.ReadAllText(Path.Combine(root,"plan.json")))
// Eight queens needs full QDOS console services; validate it under Q-emuLator instead.
let rows=cases |> Array.filter(fun case -> case.Name<>"eight_queens") |> Array.map(fun case ->
    let beforeBytes=File.ReadAllBytes(Path.Combine(root,"before",case.Task))
    let afterBytes=File.ReadAllBytes(Path.Combine(root,"after",case.Task))
    let before=run 300000000L beforeBytes
    let after=run 300000000L afterBytes
    if before.Output<>case.Output || after.Output<>case.Output || before.ExitCode<>0 || after.ExitCode<>0 then
        failwithf "Unexpected output for %s: %A / %A" case.Name before after
    let row={|Benchmark=case.Name;Output=case.Output;BeforeBytes=beforeBytes.Length;AfterBytes=afterBytes.Length;
              BeforeInstructions=before.Steps;AfterInstructions=after.Steps;
              BeforeFetchWords=before.InstructionWordsFetched;AfterFetchWords=after.InstructionWordsFetched;
              InstructionReductionPercent=100.0*(1.0-float after.Steps/float before.Steps);
              FetchReductionPercent=100.0*(1.0-float after.InstructionWordsFetched/float before.InstructionWordsFetched)|}
    printfn "%A" row
    row)
File.WriteAllText(Path.Combine(root,"execution-comparison.json"),JsonSerializer.Serialize(rows,JsonSerializerOptions(WriteIndented=true)))
