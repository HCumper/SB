module SBTests.TurboNativeRegionsTests

open System
open System.IO
open NUnit.Framework
open CompilerPipeline
open HirTurboIrBackend

let private ok = function Ok x -> x | Error e -> failwithf "%A" e
let private emit (source: string) =
    let path = Path.Combine(Path.GetTempPath(), $"native-audit-{Guid.NewGuid():N}.bas")
    File.WriteAllText(path, source)
    try
        let settings = getSettings [|path; "unused.ir"; "--no-hir-opt"; "--no-native-regions"|]
        let _,_,ast = loadAstFromInput settings |> ok
        let state = runSemanticAnalysis ast
        Assert.That(state.Errors, Is.Empty)
        let hir = runHirLowering state |> ok
        generateTurboIrFromLoweredHirWithOptions {HirTurboOptimizer.defaultOptions with Enabled=false} state hir |> ok
    finally File.Delete path

let private select output = TurboNativeRegions.select true output |> ok
let private starts output = output.Items |> List.filter (function Instruction(254,_,"audited native region") -> true | _ -> false) |> List.length

[<Test>]
let ``disabled selection preserves the original IR and listing`` () =
    let before = emit "10 a%=1:b%=2:c%=3\n20 STOP"
    let after = TurboNativeRegions.select false before |> ok
    Assert.That(after, Is.SameAs(before))

[<Test>]
let ``tiny regions retain threaded bytes`` () =
    let before = emit "10 a%=1\n20 STOP"
    Assert.That((select before).Bytes, Is.EqualTo(box before.Bytes))

[<Test>]
let ``scalar assignments form a validated native region`` () =
    let after = emit "10 a%=1:b%=2:c%=3\n20 STOP" |> select
    Assert.That(starts after, Is.EqualTo(1))
    Assert.That(validate after.Bytes, Is.Empty)
    Assert.That(decode after.Bytes |> ok, Is.EqualTo(box (after.Items |> List.map normalizeItem)))
    Assert.That((select after).Bytes, Is.EqualTo(box after.Bytes), "Selection must be idempotent.")

[<Test>]
let ``unproven arithmetic becomes explicitly checked while branch targets stay threaded`` () =
    let after = emit "10 a%=1:b%=2:c%=3\n20 FOR i%=1 TO 5\n30 c%=a%+b%:c%=c%*b%:c%=c%-b%:c%=-c%\n40 IF c%>0 THEN GO TO 60\n50 END FOR i%\n60 PRINT c%\n70 STOP" |> select
    Assert.That(starts after, Is.GreaterThan(0))
    Assert.That(after.Items |> List.exists(function Instruction((1001|1005|1006|1007),_,_) -> true | _ -> false), Is.True)
    let mutable native = true
    for item in after.Items do
        match item with
        | Instruction(254,_,_) -> native <- true
        | Instruction(255,_,_) -> native <- false
        | Instruction((1001|1005|1006|1007),_,_) -> Assert.That(native, Is.True, $"{item}")
        | Instruction((26|230|239|242|312|220),_,_) -> Assert.That(native, Is.False, $"{item}")
        | LabelDef(label,_) when label>=20000 || label=TurboIrLabels.body -> Assert.That(native, Is.False, $"Label {label}")
        | _ -> ()

[<Test>]
let ``native selection keeps the checked increment peephole together`` () =
    let after = emit "10 a%=1:b%=2:c%=3:a%=a%+1\n20 STOP" |> select
    Assert.That(starts after, Is.EqualTo(1))
    Assert.That(after.Items |> List.exists(function Instruction(1005,_,_) -> true | _ -> false), Is.True)
    Assert.That(validate after.Bytes, Is.Empty)
    Assert.That((select after).Bytes, Is.EqualTo(box after.Bytes))

[<Test>]
let ``a lone increment retains the cheaper threaded peephole`` () =
    let before=emit "10 a%=a%+1\n20 STOP"
    let after=select before
    Assert.That(after.Bytes, Is.EqualTo(box before.Bytes))

[<Test>]
let ``checked numeric access becomes compact while array allocation and continuations stay threaded`` () =
    let after = emit "10 DIM a%(2)\n20 n%=1:m%=2:k%=3\n30 a%(n%)=k%\n40 bump n%\n50 PRINT a%(n%)\n60 STOP\n100 DEFine PROCedure bump(x%)\n110 x%=x%+1\n120 END DEFine" |> select
    Assert.That(starts after, Is.GreaterThan(0))
    Assert.That(after.Items |> List.exists(function Instruction((1111|1112),_,_) -> true | _ -> false), Is.True)
    let mutable native = true
    for item in after.Items do
        match item with
        | Instruction(254,_,_) -> native <- true
        | Instruction(255,_,_) -> native <- false
        | Instruction((1111|1112),_,_) -> Assert.That(native, Is.True, $"{item}")
        | Instruction((126|234|236|244|246|294|378),_,_) -> Assert.That(native, Is.False, $"{item}")
        | LabelDef(label,_) when label>60000 -> Assert.That(native, Is.False, $"Routine {label}")
        | _ -> ()
    Assert.That(validate after.Bytes, Is.Empty)
    Assert.That((select after).Bytes, Is.EqualTo(box after.Bytes))

// x%=ABS(word) is int-to-float, ROM ABS and float-to-int. Only inside a
// selected native run does that triple become the native integer ABS record.
let private records opcode output = output.Items |> List.filter (function Instruction(op,_,_) -> op=opcode | _ -> false) |> List.length
let private absLinks output =
    output.Items |> List.filter (function Instruction(261,[_;Bytes8 name],_) -> Text.Encoding.ASCII.GetString(name).Trim()="ABS" | _ -> false) |> List.length

[<Test>]
let ``integer ABS assignments become one native record`` () =
    let before = emit "10 x%=-5:y%=ABS(x%)\n20 STOP"
    Assert.That(records 224 before, Is.EqualTo(1))
    let after = select before
    Assert.That(records 1334 after, Is.EqualTo(1))
    Assert.That(records 224 after + records 320 after + records 334 after, Is.Zero)
    // No ROM call remains, so the job no longer links the ABS resident.
    Assert.That(absLinks before, Is.EqualTo(1))
    Assert.That(absLinks after, Is.Zero)
    Assert.That(validate after.Bytes, Is.Empty)

[<Test>]
let ``ABS in a floating context keeps its ROM call and link`` () =
    let after = emit "10 x%=-5:y=ABS(x%)+0.5:z%=ABS(x%)*2:w%=ABS(x%)\n20 STOP" |> select
    Assert.That(records 1334 after, Is.EqualTo(1))
    Assert.That(records 224 after, Is.EqualTo(2))
    Assert.That(absLinks after, Is.EqualTo(1))

[<Test>]
let ``a user-defined ABS keeps its call`` () =
    let after = emit "10 x%=-5:y%=ABS((x%))\n20 STOP\n100 DEFine FuNction ABS(v)\n110 RETurn v+1\n120 END DEFine" |> select
    Assert.That(records 1334 after, Is.Zero)
    Assert.That(records 236 after, Is.EqualTo(1))

[<Test>]
let ``other residents with the same shape keep their calls`` () =
    let after = emit "10 x%=9:y%=SQRT(x%)\n20 STOP" |> select
    Assert.That(records 1334 after, Is.Zero)
    Assert.That(records 224 after, Is.EqualTo(1))

[<Test>]
let ``native integer ABS is rejected in threaded code`` () =
    let output = emit "10 x%=-5\n20 STOP"
    let items = output.Items |> List.collect (function
        | Instruction(118,_,_) as store -> [Instruction(1334,[],nativeIntegerAbsComment);store]
        | item -> [item])
    Assert.That(validate (serialize items) |> String.concat "; ", Does.Contain("Native integer ABS requires native mode"))

// A terminal tail must end in STOP and contain no transfer; otherwise its
// loop keeps threaded code. The proof stands in for the HIR certificate.
let private terminalLoop (change: IrItem list -> IrItem list) =
    let output = emit "10 DIM a$(20)\n20 FOR i%=1 TO 10\n30 a$=a$ & i%:x=i%/2\n40 IF i%=4 THEN PRINT \"stop\":STOP\n50 END FOR i%\n60 STOP"
    let counter,top = output.Items |> List.pick (function Instruction(26,[Word counter;LabelRef top],_) -> Some(counter,top) | _ -> None)
    let proof = {Counter=counter;ScalarVectors=set [counter];InvariantScalarVectors=Set.empty;InvariantArrayVectors=Set.empty}
    let selected = TurboNativeTemplateLoops.select true {output with Items=change output.Items;NativeForLoopProofs=Map [top,proof]} |> ok
    TurboNativeTemplateLoops.format selected.Items

[<Test>]
let ``a terminal STOP tail keeps its loop native`` () =
    let report = terminalLoop id
    Assert.That(report, Does.Contain("Audited memory-counter native FOR loops: 1"))
    Assert.That(report, Does.Contain("Terminal STOP tails in native loops: 1"))

[<Test>]
let ``a tail without STOP or with a jump keeps its loop threaded`` () =
    let withoutStop items =
        let stops = items |> List.indexed |> List.filter (fun (_,item) -> match item with Instruction(220,[],_) -> true | _ -> false) |> List.map fst
        items |> List.indexed |> List.filter (fun (i,_) -> i<>List.head stops) |> List.map snd
    let withJump items =
        let mutable added = false
        items |> List.collect (fun item ->
            match item with
            | Instruction(220,[],_) when not added ->
                added <- true
                let label = items |> List.pick (function Instruction(239,[LabelRef label],_) -> Some label | _ -> None)
                [Instruction(242,[LabelRef label],"");item]
            | item -> [item])
    for change in [withoutStop;withJump] do
        Assert.That(terminalLoop change, Does.Contain("Audited memory-counter native FOR loops: 0"))

[<Test>]
let ``inline native NEXT is rejected in threaded code`` () =
    let output = emit "10 FOR i%=1 TO 3\n20 a=i%\n30 END FOR i%\n40 STOP"
    let top = output.Items |> List.pick (function Instruction(26,[_;LabelRef top],_) -> Some top | _ -> None)
    let items = output.Items |> List.map (function
        | Instruction(230,[Word counter],_) -> Instruction(1530,[Word counter;LabelRef top;Word 1],inlineNextComment)
        | item -> item)
    Assert.That(items |> List.exists(function Instruction(1530,_,_) -> true | _ -> false), Is.True)
    Assert.That(validate (serialize items) |> String.concat "; ", Does.Contain("Inline native NEXT requires native mode"))

// Floating arithmetic and one-argument ROM maths calls run in native regions,
// with lib537's native rom_fn. Other residents keep their threaded calls.
let private nativeRecords opcode output =
    let mutable native=false
    let mutable inside,outside=0,0
    for item in output.Items do
        match item with
        | Instruction(254,_,_) -> native<-true
        | Instruction(255,_,_) -> native<-false
        | Instruction(op,_,_) when op=opcode -> if native then inside<-inside+1 else outside<-outside+1
        | _ -> ()
    inside,outside

[<Test>]
let ``string fetch store and concatenation join native regions`` () =
    let after = emit "10 DIM a$(10),b$(10)\n20 a$=\"ab\":b$=a$ & \"cd\":c%=CODE(b$)\n30 STOP" |> select
    for opcode in [264;265;84] do Assert.That(fst (nativeRecords opcode after), Is.GreaterThan(0), $"opcode {opcode}")
    Assert.That(validate after.Bytes, Is.Empty)

// Append and simple LEN run natively only when HIR analysis proved the string
// allocated: the listing comment carries that proof.
[<TestCase(false)>]
[<TestCase(true)>]
let ``append and LEN join native regions only when proven allocated`` (proven:bool) =
    let before = emit "10 DIM a$(10)\n20 a$=\"ab\":b%=1:c%=2:a$=a$ & \"cd\":c%=LEN(a$)+b%\n30 STOP"
    let tag = function
        | Instruction((374|375 as op),args,_) when proven -> Instruction(op,args,allocatedStringComment)
        | item -> item
    let after = select {before with Items=List.map tag before.Items}
    for opcode in [374;375] do
        Assert.That(nativeRecords opcode after, Is.EqualTo(if proven then (1,0) else (0,1)), $"opcode {opcode}")
    Assert.That(validate after.Bytes, Is.Empty)

[<Test>]
let ``floating arithmetic and ROM maths calls join native regions`` () =
    let after = emit "10 a=1.5:b=SIN(a)*2+COS(a)/3:c=SQRT(b*b)-ATAN(a)\n20 STOP" |> select
    Assert.That(nativeRecords 224 after, Is.EqualTo((4,0)))
    for opcode in [37;38;39;40] do Assert.That(fst (nativeRecords opcode after), Is.GreaterThan(0), $"opcode {opcode}")
    Assert.That(validate after.Bytes, Is.Empty)

[<Test>]
let ``non-maths residents keep threaded calls`` () =
    let after = emit "10 a=RND(5)*1.5\n20 STOP" |> select
    Assert.That(nativeRecords 224 after, Is.EqualTo((0,1)))
