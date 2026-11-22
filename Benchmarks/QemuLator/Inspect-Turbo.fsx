#r "../../SB/bin/Debug/net10.0/SB.dll"
open System
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open HirTurboIrBackend

let runDirectory = fsi.CommandLineArgs[1]
let drive = Path.Combine(runDirectory,"drive")
let rows=ResizeArray<obj>()
for path in Directory.GetFiles(drive,"b??_ir") |> Array.sort do
    let file=File.ReadAllBytes path
    let captured=if Text.Encoding.ASCII.GetString(file,0,30).StartsWith("]!QDOS File Header") then file[30..] else file
    let trailing =
        match decode captured with
        | Ok _ -> 0
        | Error error ->
            let m=Regex.Match(error,@"^(\d+) bytes follow the end-of-IR word\.$")
            if not m.Success then failwithf "%s: %s" path error
            int m.Groups[1].Value
    let ir=captured[..captured.Length-trailing-1]
    let items=decode ir |> Result.defaultWith failwith
    let tag=Path.GetFileName(path).Substring(0,3)
    File.WriteAllBytes(Path.Combine(runDirectory,tag+".turbo.ir"),ir)
    File.WriteAllText(Path.Combine(runDirectory,tag+".turbo.ir.lst"),formatListing items)
    let mutable native=true
    let mutable nativeOps=0
    let mutable threadedOps=0
    let mutable fastSwitches=0
    let mutable nativeIntegerOps=0
    let mutable threadedIntegerOps=0
    for item in items do
        match item with
        | Instruction(254,_,_) -> native<-true; fastSwitches<-fastSwitches+1
        | Instruction(255,_,_) -> native<-false
        | Instruction(op,_,_) ->
            if native then nativeOps<-nativeOps+1 else threadedOps<-threadedOps+1
            if List.contains op [1;2;5;6;7;8;20;337;338] then
                if native then nativeIntegerOps<-nativeIntegerOps+1 else threadedIntegerOps<-threadedIntegerOps+1
        | _ -> ()
    if fastSwitches=0 || nativeIntegerOps=0 then failwithf "%s has no native integer code" tag
    let original=File.ReadAllBytes(Path.Combine(drive,tag+"t"))
    let taskLength=(int original[38] <<< 8) ||| int original[39]
    let taskName=Text.Encoding.ASCII.GetString(original,40,taskLength)
    let options={TurboCodegen.defaultCodegenOptions with TaskName=taskName;StackSize=Some 350}
    let reference = TurboCodegen.generateFromIr options ir |> Result.defaultWith (String.concat "\n" >> failwith)
    let generated=TurboCodegen.executableFile reference
    let same=original[30..]=reference.Code
    if not same then failwithf "%s: compatibility generator differs from the original Turbo code payload" tag
    let differences=[for index in 30..min (original.Length-1) (generated.Length-1) do if original[index]<>generated[index] then yield index]
    File.WriteAllText(Path.Combine(runDirectory,tag+".native-verification.txt"),
        $"FAST selections: {fastSwitches}\nNative operations: {nativeOps}\nThreaded operations: {threadedOps}\nNative integer arithmetic operations: {nativeIntegerOps}\nThreaded integer arithmetic operations: {threadedIntegerOps}\nSB compatibility generator reproduces Turbo code payload: {same}\nFirst differing file offsets: {differences |> List.truncate 8}\n")
    rows.Add(box {|Tag=tag;IrBytes=ir.Length;FastSelections=fastSwitches;NativeOperations=nativeOps;
                  ThreadedOperations=threadedOps;NativeIntegerOperations=nativeIntegerOps;
                  ThreadedIntegerOperations=threadedIntegerOps;CompatibilityExecutableIdentical=same;
                  TurboBytes=original.Length;CompatibilityBytes=generated.Length|})
File.WriteAllText(Path.Combine(runDirectory,"native-verification.json"),JsonSerializer.Serialize(rows,JsonSerializerOptions(WriteIndented=true)))
printfn "Verified native IR in %d Turbo executables" rows.Count
