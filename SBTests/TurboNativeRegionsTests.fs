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
