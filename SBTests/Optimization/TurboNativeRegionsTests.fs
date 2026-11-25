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

// Turbo's RETRY_HERE directive sets WHEN ERROR's retry points (326, 346) where it stands,
// as the body's start already does; RETRY_HERE 1 sets only the second.
[<TestCase("10 RETRY_HERE\n20 STOP", 2, 2)>]
[<TestCase("10 RETRY_HERE 1\n20 STOP", 1, 2)>]
[<TestCase("10 RETRY_HERE 0\n20 STOP", 2, 1)>]
let ``RETRY_HERE sets retry points instead of calling a routine`` (source: string, retry: int, retry1: int) =
    let output = emit source
    let count opcode = output.Items |> List.filter (function Instruction(op,_,_) -> op=opcode | _ -> false) |> List.length
    Assert.That(count 326, Is.EqualTo(retry))
    Assert.That(count 346, Is.EqualTo(retry1))
    Assert.That(count 227, Is.Zero)

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

// Only integer arithmetic may separate in-place string operands, so nothing
// can change the left string before the comparison reads it.
let private inPlaceComparisons (change: IrItem list -> IrItem list) =
    let output = emit "10 DIM a$(10),b$(2,10)\n20 i%=1:a$=\"x\":b$(1)=\"x\"\n30 c%=(a$=b$(i%)):d%=c%+1\n40 STOP" |> select
    let changed = {output with Items=change output.Items}
    let rewritten = TurboNativeStringOperands.select true {changed with Bytes=serialize changed.Items} |> ok
    records 1533 rewritten

[<Test>]
let ``string comparisons read whole operands in place`` () =
    Assert.That(inPlaceComparisons id, Is.EqualTo(1))

[<Test>]
let ``a store between string operands keeps the copying comparison`` () =
    let storeBeforeIndex items =
        let index = items |> List.findIndex (function Instruction(175,_,_) -> true | _ -> false)
        let counter = items |> List.pick (function Instruction(118,[Word v],_) -> Some v | _ -> None)
        // Inside the index expression: push i%, then a store that leaves it.
        let before,after = List.splitAt (index-2) items
        before @ [Instruction(109,[Word counter],"");Instruction(118,[Word counter],"")] @ after
    Assert.That(inPlaceComparisons storeBeforeIndex, Is.Zero)

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

// Native calls of the single-operation ROM maths functions call RI.EXEC with
// that operation. Threaded calls, DEG, RAD and INT keep rom_fn, and a function
// still called through rom_fn keeps its link.
let private links name output =
    output.Items |> List.filter (function Instruction(261,[_;Bytes8 linked],_) -> Text.Encoding.ASCII.GetString(linked).Trim()=name | _ -> false) |> List.length
let private directCalls output =
    output.Items |> List.choose (function Instruction(1539,[Word operation;Word argument],_) -> Some(operation,argument) | _ -> None)
let private direct output = TurboNativeRomMaths.select true output |> ok

[<Test>]
let ``native ROM maths calls go straight to RI.EXEC`` () =
    let before = emit "10 a=1.5:i%=4:b=SIN(a)*2+COS(a)/3:c=SQRT(b*b)-ATAN(a)+SQRT(i%)+ATAN(i%)\n20 STOP" |> select
    let after = direct before
    // SQRT declares a float parameter, so its integer argument is converted
    // first; ATAN's integer argument reaches the call and RI.FLOAT converts it.
    Assert.That(directCalls after, Is.EqualTo<(int * int) list>([0x1A,2;0x18,2;0x28,2;0x24,2;0x28,2;0x24,3]))
    Assert.That(nativeRecords 224 after, Is.EqualTo((0,0)))
    // No call remains, so the job no longer links these names.
    for name in ["SIN";"COS";"SQRT";"ATAN"] do
        Assert.That(links name before, Is.EqualTo(1), name)
        Assert.That(links name after, Is.Zero, name)
    Assert.That(validate after.Bytes, Is.Empty)
    Assert.That(TurboNativeRomMaths.select false before |> ok, Is.EqualTo(before))

[<Test>]
let ``threaded ROM maths calls and scaling functions keep rom_fn`` () =
    let threaded = emit "10 a=1.5:b=SIN(a)\n20 STOP"
    Assert.That(directCalls (direct threaded), Is.Empty)
    // DEG and RAD scale their argument and INT rounds it before any routine.
    let after = emit "10 a=1.5:b=DEG(a)+RAD(a)+INT(a)\n20 STOP" |> select |> direct
    Assert.That(directCalls after, Is.Empty)
    Assert.That(nativeRecords 224 after, Is.EqualTo((3,0)))
    // A name linked twice may be bound to something else: keep its call.
    let native = emit "10 a=1.5:b=SIN(a)*2+COS(a)\n20 STOP" |> select
    let twice = native.Items |> List.collect (function
        | Instruction(261,[_;Bytes8 linked],_) as link when Text.Encoding.ASCII.GetString(linked).Trim()="SIN" -> [link;link]
        | item -> [item])
    let rewritten = direct {native with Items=twice;Bytes=serialize twice}
    Assert.That(directCalls rewritten, Is.EqualTo<(int * int) list>([0x18,2]))
    Assert.That(nativeRecords 224 rewritten, Is.EqualTo((1,0)))

[<Test>]
let ``a threaded call keeps its function's link`` () =
    let native = emit "10 a=1.5:b=SIN(a)*2+SIN(a)/3\n20 STOP" |> select
    let second = native.Items |> List.findIndexBack (function Instruction(224,_,_) -> true | _ -> false)
    let before,after = List.splitAt second native.Items
    let mixed = before @ [Instruction(255,[],"");after.Head;Instruction(254,[],"")] @ after.Tail
    let rewritten = direct {native with Items=mixed;Bytes=serialize mixed}
    Assert.That(directCalls rewritten, Is.EqualTo<(int * int) list>([0x1A,2]))
    Assert.That(nativeRecords 224 rewritten, Is.EqualTo((0,1)))
    Assert.That(links "SIN" rewritten, Is.EqualTo(1))
    Assert.That(validate rewritten.Bytes, Is.Empty)

[<Test>]
let ``direct ROM maths calls are validated`` () =
    let output = emit "10 a=1.5\n20 STOP"
    let insert operation = output.Items |> List.collect (function
        | Instruction(150,_,_) as store -> [Instruction(1539,[Word operation;Word 2],directRomMathsComment);store]
        | item -> item |> List.singleton)
    let problems operation = validate (serialize (insert operation)) |> String.concat "; "
    Assert.That(problems 0x1A, Does.Contain("Direct ROM maths calls require native mode"))
    Assert.That(problems 0x16, Does.Contain("Invalid direct ROM maths call"))
