#r "C:/Source/SB/SB/bin/Debug/net10.0/SBRuntime.dll"
#r "C:/Source/SB/SB/bin/Debug/net10.0/Antlr4.Runtime.Standard.dll"
#r "C:/Source/SB/SB/bin/Debug/net10.0/FSharpPlus.dll"
#r "C:/Source/SB/SB/bin/Debug/net10.0/SBLib.dll"
#r "C:/Source/SB/SB/bin/Debug/net10.0/Serilog.dll"
#r "C:/Source/SB/SB/bin/Debug/net10.0/SB.dll"
#r "C:/Source/SB/SBTests/bin/Debug/net10.0/SBTests.dll"
#load "ProfileCodegen.fs"
#load "ProfileExecutionHarness.fs"

open System
open System.IO
open System.Collections.Generic
open System.Text.Json
open System.Security.Cryptography
open HirTurboIrBackend
open CompilerPipeline
open SBTests.M68kEmulator

let runDirectory=fsi.CommandLineArgs[1] |> Path.GetFullPath
let destination=fsi.CommandLineArgs[2] |> Path.GetFullPath
Directory.CreateDirectory destination |> ignore
AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY",Path.GetDirectoryName(typeof<TurboCodegen.TurboCodegenOutput>.Assembly.Location))
let hash bytes=Convert.ToHexString(SHA256.HashData(bytes:byte[]))
let compilerHash=hash(File.ReadAllBytes typeof<TurboCodegen.TurboCodegenOutput>.Assembly.Location)
let build=JsonDocument.Parse(File.ReadAllText(Path.Combine(runDirectory,"sb-build.json")))
if build.RootElement.GetProperty("CompilerSha256").GetString()<>compilerHash then failwith "Compiler differs from the benchmark build."
let runtimeMap=JsonDocument.Parse(File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__,"runtime-map.json")))
if runtimeMap.RootElement.GetProperty("LibrarySha256").GetString()<>hash TurboCodegen.library.Value then failwith "Runtime symbols refer to a different library."
let templateNames=runtimeMap.RootElement.GetProperty("Templates").EnumerateArray() |> Seq.map(fun p -> p.GetProperty("Number").GetInt32(),p.GetProperty("Name").GetString()) |> Map.ofSeq
let runtimeLabels=runtimeMap.RootElement.GetProperty("Labels").EnumerateArray() |> Seq.map(fun p -> p.GetProperty("Offset").GetInt32(),p.GetProperty("Name").GetString()) |> Seq.sortBy fst |> Seq.toArray
let libraryEntry=runtimeMap.RootElement.GetProperty("EntryOffset").GetInt32()
let libraryEntryLength=runtimeMap.RootElement.GetProperty("EntryLength").GetInt32()
let romPath=File.ReadAllLines(Path.Combine(runDirectory,"benchmark.QCF")) |> Array.find(fun line -> line.StartsWith("MainRom=")) |> fun line -> line.Substring(8)
let rom=File.ReadAllBytes romPath
let romWord offset=(int rom[offset] <<< 8) ||| int rom[offset+1]
let functionAddress name =
    let pattern=Array.append [|byte(String.length name)|] (Text.Encoding.ASCII.GetBytes name)
    let matches=[for i in 2..rom.Length-pattern.Length do if rom[i..i+pattern.Length-1]=pattern then yield i]
    let i=List.exactlyOne matches
    uint32(i-2+int(int16(uint16(romWord(i-2)))))
let residentFunctions=["ABS";"COS";"SIN";"SQRT"] |> List.map(fun name -> name,functionAddress name)
let residentEntries=residentFunctions |> List.map(fun (name,address) -> address,name) |> Map.ofList
let arithmeticEntry=uint32(romWord 0x11C)
let vectorEntries=[0xE6,"UT.CSTR string comparison";0x11C,"RI.EXEC arithmetic"] |> List.map(fun (offset,name) -> uint32(romWord offset),name) |> Map.ofList
let targets=set ["eight_queens";"concordance";"string_operations";"float_polynomial";"float_transcendentals"]
let plan=JsonDocument.Parse(File.ReadAllText(Path.Combine(runDirectory,"plan.json")))
let profiles=ResizeArray<obj>()
let jsonOptions=JsonSerializerOptions(WriteIndented=true)

for benchmark in plan.RootElement.GetProperty("benchmarks").EnumerateArray() do
    let name=benchmark.GetProperty("name").GetString()
    if targets.Contains name then
      let tag=benchmark.GetProperty("tag").GetString()
      let expected=sprintf "%s:%d\n" name (benchmark.GetProperty("expectedChecksum").GetInt32())
      for compiler,suffix in ["SB","s";"Turbo FAST","t"] do
        let executable=File.ReadAllBytes(Path.Combine(runDirectory,"drive",tag+suffix))
        let taskLength=(int executable[38] <<< 8) ||| int executable[39]
        let taskName=Text.Encoding.ASCII.GetString(executable,40,taskLength)
        let items,irBytes,options =
            if suffix="s" then
                let source=Path.Combine(runDirectory,tag+"_sb.bas")
                let settings=getSettings [|source;Path.Combine(destination,taskName);"false";"turbo";"--hir-opt";"--native-regions";"--regalloc"|]
                let _,_,ast=loadAstFromInput settings |> Result.defaultWith(fun e -> failwithf "%A" e)
                let state=runSemanticAnalysis ast
                if not state.Errors.IsEmpty then failwithf "Semantic errors: %A" state.Errors
                let hir=runHirLowering state |> Result.defaultWith(fun e -> failwithf "%A" e)
                let ir,output=generateTurboExecutableFromLoweredHirWithRegisterAllocation settings.TurboHirOptimization true true state taskName hir |> Result.defaultWith(String.concat "; " >> failwith)
                if TurboCodegen.executableFile output<>executable then failwith "Rebuilt SB executable differs."
                ir.Items,ir.Bytes,{ProfileCodegen.defaultCodegenOptions with TaskName=taskName;LocalRegisterAllocation=true;NativeIntegerForLoops=true;NativeForLoopProofs=ir.NativeForLoopProofs}
            else
                let ir=File.ReadAllBytes(Path.Combine(runDirectory,tag+".turbo.ir"))
                let items=decode ir |> Result.defaultWith failwith
                items,ir,{ProfileCodegen.defaultCodegenOptions with TaskName=taskName;StackSize=Some 350}
        let mapped=ProfileCodegen.generateFromIr options irBytes |> Result.defaultWith(String.concat "; " >> failwith)
        if mapped.Code<>executable[30..] then failwithf "Read-only mapped output differs: %s %s" name compiler
        File.WriteAllBytes(Path.Combine(destination,tag+suffix+".ir"),irBytes)
        File.WriteAllText(Path.Combine(destination,tag+suffix+".ir.lst"),formatListing items)
        File.WriteAllText(Path.Combine(destination,tag+suffix+".segments.json"),JsonSerializer.Serialize(mapped.Segments,jsonOptions))

        let routines=ResizeArray<string>(["<main/startup>"])
        let originRoutine=Array.zeroCreate<int>(irBytes.Length+1)
        let mutable irOffset=0
        let mutable routine=0
        for item in items do
            match item with
            | LabelDef(label,_) when label>=TurboIrLabels.data && label<=TurboIrLabels.afterLinkage -> routine<-0
            | LabelDef(_,comment) when comment.StartsWith("DEFine ") ->
                let words=comment.Split(' ')
                let name=words[2]
                routine<-routines.IndexOf name
                if routine<0 then routine<-routines.Count;routines.Add name
            | _ -> ()
            let size=(serialize [item]).Length
            for i in irOffset..irOffset+size-1 do originRoutine[i]<-routine
            irOffset<-irOffset+size

        let segmentAt=Array.create mapped.Code.Length -1
        for index,(first,finish,_,_) in mapped.Segments |> Array.indexed do
            for address in first..finish-1 do
                if segmentAt[address]<> -1 then failwith "Overlapping generated code-map segments."
                segmentAt[address]<-index
        if segmentAt |> Array.exists((=) -1) then failwith "Incomplete code map."
        let sharedEntries=Dictionary<int,int>()
        for first,_,kind,_ in mapped.Segments do
            if kind.StartsWith("shared-P:") then sharedEntries[first]<-int(kind.Split(':')[1])

        let counts=Array.zeroCreate<int64> (0x100000/2)
        let fetches=Array.zeroCreate<int64> counts.Length
        let routineCounts=Array.zeroCreate<int64> routines.Count
        let routineRom=Array.zeroCreate<int64> routines.Count
        let routineDispatch=Array.zeroCreate<int64> routines.Count
        let calls=Dictionary<string,int64>()
        let mutable owner=0
        let mutable previousPc= -1
        let mutable previousFetch=0L
        let mutable observedCpu:Cpu option=None
        let mutable romService="ROM utility"
        let mutable previousWasRom=false
        let serviceCounts=Dictionary<string,int64>()
        let add (dictionary:Dictionary<string,int64>) key value = dictionary[key]<-(match dictionary.TryGetValue key with true,n -> n+value | _ -> value)
        let observe (cpu:Cpu) =
            observedCpu<-Some cpu
            if previousPc>=0 then fetches[previousPc/2]<-fetches[previousPc/2]+cpu.InstructionWordsFetched-previousFetch
            previousFetch<-cpu.InstructionWordsFetched
            previousPc<-int cpu.Pc
            counts[previousPc/2]<-counts[previousPc/2]+1L
            let offset=int cpu.Pc-0x40000
            let hook=cpu.Hooks.ContainsKey cpu.Pc
            let inRom=not hook && cpu.Pc<uint32 rom.Length
            if offset>=0 && offset<segmentAt.Length then
                let _,_,kind,origin=mapped.Segments[segmentAt[offset]]
                let update origin=if origin>=0 && origin<originRoutine.Length then owner<-originRoutine[origin]
                if kind="threaded-data" then failwith "Executed an IR data word as a CPU instruction."
                if kind.StartsWith("native-") then update origin
                if sharedEntries.ContainsKey offset then
                    let thread=int cpu.A[5]-0x40000-2
                    if thread>=0 && thread<segmentAt.Length then
                        let _,_,kind,origin=mapped.Segments[segmentAt[thread]]
                        if kind="threaded-data" then update origin
                if kind="thread-dispatch" then routineDispatch[owner]<-routineDispatch[owner]+1L
            if not hook then routineCounts[owner]<-routineCounts[owner]+1L
            if inRom then
                if not previousWasRom then
                    romService<-vectorEntries.TryFind cpu.Pc |> Option.defaultValue(sprintf "ROM entry $%04X" cpu.Pc)
                    add calls ("entry "+romService) 1L
                if residentEntries.ContainsKey cpu.Pc then
                    romService<-"resident "+residentEntries[cpu.Pc]
                    add calls romService 1L
                elif cpu.Pc=arithmeticEntry then
                    let op=int(cpu.D[0] &&& 255u)
                    let label=match op with 14 -> "RI.EXEC multiply" | 16 -> "RI.EXEC divide" | _ -> sprintf "RI.EXEC opcode %d" op
                    add calls label 1L
                    if romService="ROM utility" || romService="RI.EXEC arithmetic" then romService<-label
                routineRom[owner]<-routineRom[owner]+1L
                add serviceCounts romService 1L
            else romService<-"ROM utility"
            previousWasRom<-inRom
        let result=ProfileExecutionHarness.run rom residentFunctions 300000000L observe executable
        let cpu=observedCpu.Value
        fetches[previousPc/2]<-fetches[previousPc/2]+result.InstructionWordsFetched-previousFetch
        if result.Output<>expected || result.ExitCode<>0 then failwithf "Profile checksum mismatch: %s / %s: %A" name compiler result
        if Array.sum counts<>result.Steps || Array.sum fetches<>result.InstructionWordsFetched then failwith "Profile accounting mismatch."

        let costs=Dictionary<string,int64 * int64>()
        let entries=Dictionary<string,int64>()
        let pcRows=ResizeArray<obj>()
        let category address =
            if cpu.Hooks.ContainsKey(uint32 address) then "QDOS host hook"
            elif address<rom.Length then "QL ROM"
            else
                let offset=address-0x40000
                if offset<0 || offset>=segmentAt.Length then failwithf "Unmapped executed address %X" address
                elif offset<libraryEntryLength then
                    let fileOffset=offset+libraryEntry
                    let label=runtimeLabels |> Array.takeWhile(fun (n,_) -> n<=fileOffset) |> Array.last |> snd
                    "runtime:"+label
                else
                    let _,_,kind,_=mapped.Segments[segmentAt[offset]]
                    if kind.StartsWith("shared-P:") || kind.StartsWith("native-template:") then
                        let opcode=int(kind.Split(':')[1])
                        let label=templateNames.TryFind opcode |> Option.defaultValue(string opcode)
                        kind.Split(':')[0]+":"+label
                    else kind
        for i in 0..counts.Length-1 do
            if counts[i]>0L then
                let address=i*2
                let group=category address
                let n,f=match costs.TryGetValue group with true,v -> v | _ -> 0L,0L
                costs[group]<-n+counts[i],f+fetches[i]
                pcRows.Add(box {|Pc=address;CodeOffset=address-0x40000;Instructions=counts[i];FetchWords=fetches[i];Group=group|})
        for first,_,_,_ in mapped.Segments do
            if counts[(0x40000+first)/2]>0L then add entries (category(0x40000+first)) counts[(0x40000+first)/2]
        let host=cpu.Hooks.Keys |> Seq.sumBy(fun address -> counts[int address/2])
        let instructions=result.Steps-host
        let orderedCosts=costs |> Seq.map(fun p -> let n,f=p.Value in {|Group=p.Key;Instructions=n;FetchWords=f;Percent=100.0*float n/float instructions|}) |> Seq.sortByDescending _.Instructions |> Seq.toArray
        let sourceCosts=routines |> Seq.mapi(fun index label -> {|Routine=label;Instructions=routineCounts[index];RomInstructions=routineRom[index];DispatchInstructions=routineDispatch[index];Percent=100.0*float routineCounts[index]/float instructions|}) |> Seq.sortByDescending _.Instructions |> Seq.toArray
        let summary={|Benchmark=name;Compiler=compiler;ExecutableSha256=hash executable;UnmodifiedExecutable=true;MappedCodeIdentical=true;Bytes=executable.Length;Output=result.Output;ExitCode=result.ExitCode;
                      StepsIncludingHostHooks=result.Steps;Instructions=instructions;HostHooks=host;FetchWords=result.InstructionWordsFetched;RomInstructions=Array.sum routineRom;
                      Costs=orderedCosts;Routines=sourceCosts;RomCalls=calls;RomServices=serviceCounts;SegmentEntries=entries|}
        File.WriteAllText(Path.Combine(destination,tag+suffix+".pc-counts.json"),JsonSerializer.Serialize(pcRows,jsonOptions))
        File.WriteAllText(Path.Combine(destination,tag+suffix+".profile.json"),JsonSerializer.Serialize(summary,jsonOptions))
        profiles.Add(box summary)
        printfn "PASS %s %s: %d instructions; ROM %d; checksum %s" name compiler instructions summary.RomInstructions (result.Output.Trim())
        for row in orderedCosts |> Array.truncate 5 do printfn "  %6.2f%% %s: %d" row.Percent row.Group row.Instructions
        for row in sourceCosts |> Array.truncate 5 do printfn "  routine %s: %.2f%%" row.Routine row.Percent

if profiles.Count<>10 then failwith "Expected ten complete profiles."
if compilerHash<>hash(File.ReadAllBytes typeof<TurboCodegen.TurboCodegenOutput>.Assembly.Location) then failwith "Compiler changed while profiling."
File.WriteAllText(Path.Combine(destination,"profiles.json"),JsonSerializer.Serialize(profiles,jsonOptions))
File.WriteAllText(Path.Combine(destination,"configuration.json"),JsonSerializer.Serialize({|CompilerSha256=compilerHash;RomSha256=hash rom;Rom=romPath;RealRomExecuted=true;ProfiledUnmodifiedJobs=true;CodeMapsByteIdentical=true;InstructionCountsAreNotCycles=true;OsUtilitiesAreHostHooks=true;Inputs=runDirectory|},jsonOptions))
