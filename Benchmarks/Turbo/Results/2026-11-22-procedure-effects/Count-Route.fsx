#r "C:/Source/SB/SBTests/bin/Debug/net10.0/SBTests.dll"
open System.IO
open System.Text.Json
open SBTests.TurboExecutionHarness
let root = @"C:\Source\codex-workspace\.scratch\turbo-procedure-effects\route-comparison"
let beforeBytes=File.ReadAllBytes(Path.Combine(root,"before","route_exe"))
let afterBytes=File.ReadAllBytes(Path.Combine(root,"after","route_exe"))
let before=run 300000000L beforeBytes
let after=run 300000000L afterBytes
if before.Output<>"route_planner:4701\n" || after.Output<>before.Output || before.ExitCode<>0 || after.ExitCode<>0 then
    failwithf "Unexpected route output: %A / %A" before after
let row={|Benchmark="route_planner";Checksum=4701;BeforeBytes=beforeBytes.Length;AfterBytes=afterBytes.Length;
          BeforeInstructions=before.Steps;AfterInstructions=after.Steps;
          BeforeFetchWords=before.InstructionWordsFetched;AfterFetchWords=after.InstructionWordsFetched;
          InstructionReductionPercent=100.0*(1.0-float after.Steps/float before.Steps);
          FetchReductionPercent=100.0*(1.0-float after.InstructionWordsFetched/float before.InstructionWordsFetched)|}
File.WriteAllText(Path.Combine(root,"execution-comparison.json"),JsonSerializer.Serialize(row,JsonSerializerOptions(WriteIndented=true)))
printfn "%A" row
