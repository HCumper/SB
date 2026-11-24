module SBTests.TurboNativeTemplateLoopsTests

open NUnit.Framework
open HirTurboIrBackend

let private op n args = Instruction(n,List.map Word args,"")
let private text = Instruction(169,[Text16 [|65uy|]],"")
let private floatBody = [op 109 [1];op 320 [];op 150 [2]]
let private stream body =
    let items=[op 999 [8];op 255 [];op 262 [3];Instruction(357,[LabelRef 2001],"");Instruction(26,[Word 1;LabelRef 2000],"");op 355 [];LabelDef(2000,"")] @ body @ [op 230 [1];Instruction(378,[LabelRef 2001],"");op 254 [];LabelDef(2008,"");Instruction(243,[LabelRef 2008],"");op 358 [0];Instruction(243,[LabelRef 2008],"");op 0 []]
    {Items=items;Bytes=serialize items;Listing=formatListing items;Warnings=[]
     MemoryForLoops=Map.empty;NativeProcedureCalls=Map.empty
     NativeForLoopProofs=Map.ofList [2000,{Counter=1;ScalarVectors=set [1;2;3];InvariantScalarVectors=Set.empty;InvariantArrayVectors=Set.empty}]}
let private select output = TurboNativeTemplateLoops.select true output |> Result.defaultWith (String.concat "; " >> failwith)
let private count output = output.Items |> List.sumBy(function Instruction(254,[],"audited memory-counter native FOR") -> 1 | _ -> 0)

[<Test>]
let ``memory-counter selection requires proof and leaves disabled bytes intact`` () =
    let before=stream floatBody
    Assert.That(TurboNativeTemplateLoops.select false before |> Result.toOption |> Option.get, Is.SameAs(before))
    Assert.That(select {before with NativeForLoopProofs=Map.empty} |> _.Bytes, Is.EqualTo(box before.Bytes))
    let after=select before
    Assert.That(count after, Is.EqualTo(1))
    Assert.That(validate after.Bytes, Is.Empty)
    Assert.That((select after).Bytes, Is.EqualTo(box after.Bytes))

[<Test>]
let ``memory-counter selection leaves word-only loops for register allocation`` () =
    let before=stream [op 109 [1];op 118 [2]]
    Assert.That((select before).Bytes, Is.EqualTo(box before.Bytes))
    let boolean=stream [op 109 [1];op 106 [0];op 12 [];op 109 [1];op 106 [3];op 15 [];op 54 [];op 36 [];op 334 [];op 118 [2]]
    Assert.That((select boolean).Bytes, Is.EqualTo(box boolean.Bytes))

[<Test>]
let ``memory-counter IF accepts only forward local targets and empty stack joins`` () =
    let body=[op 109 [1];op 106 [2];op 12 [];Instruction(239,[LabelRef 2002],"")] @ floatBody @ [LabelDef(2002,"")]
    Assert.That(stream body |> select |> count, Is.EqualTo(1))
    for bad in [body @ [Instruction(242,[LabelRef 2002],"")];
                [op 263 [];LabelDef(2002,"")] @ floatBody;
                [Instruction(242,[LabelRef 2001],"")] @ floatBody;
                [op 109 [1];op 150 [2]]] do
        let before=stream bad
        Assert.That((select before).Bytes, Is.EqualTo(box before.Bytes))
    let before=stream body
    let outside=before.Items.Head::Instruction(242,[LabelRef 2002],"")::before.Items.Tail
    Assert.That(select {before with Items=outside;Bytes=serialize outside} |> count, Is.Zero)

[<Test>]
let ``memory-counter loops reject opaque helpers and malformed array ranks`` () =
    for bad in [floatBody @ [op 244 []];floatBody @ [op 230 [2]];
                [op 263 [];op 143 [7];op 150 [2]];
                [op 263 [];op 263 [];op 143 [3];op 150 [2]]] do
        Assert.That(stream bad |> select |> count, Is.Zero)

[<Test>]
let ``memory-counter append and LEN retain checked threaded templates`` () =
    let after=stream [text;op 374 [3];op 375 [3];op 118 [2]] |> select
    Assert.That(count after, Is.EqualTo(1))
    let mutable native=true
    for item in after.Items do
        match item with
        | Instruction(254,_,_) -> native<-true
        | Instruction(255,_,_) -> native<-false
        | Instruction((374|375),_,_) -> Assert.That(native, Is.False)
        | Instruction((26|230|378),_,_) -> Assert.That(native, Is.True)
        | _ -> ()

[<Test>]
let ``memory-counter word arithmetic remains explicitly checked`` () =
    let after=stream ([op 109 [1];op 106 [32767];op 5 [];op 118 [2]] @ floatBody) |> select
    Assert.That(count after, Is.EqualTo(1))
    Assert.That(after.Items |> List.exists(function Instruction(1005,[],_) -> true | _ -> false), Is.True)

[<Test>]
let ``memory-counter calls require a separate proof and exact integer arguments`` () =
    let before=stream [op 109 [1];Instruction(236,[LabelRef 2010],"")]
    let items=before.Items.Head::LabelDef(2010,"")::before.Items.Tail
    let before={before with Items=items;Bytes=serialize items;NativeProcedureCalls=Map.ofList [2010,1]}
    Assert.That(select before |> count, Is.Zero,"Pinned-loop certificates cannot authorize a call.")
    let proven={before with MemoryForLoops=Map.ofList [2000,1]}
    let after=select proven
    Assert.That(count after, Is.EqualTo(1))
    Assert.That(TurboNativeTemplateLoops.format after.Items, Does.Contain("procedure call boundaries: 1"))
    for bad in [{proven with MemoryForLoops=Map.ofList [2000,2]};
                {proven with NativeProcedureCalls=Map.empty};
                {proven with NativeProcedureCalls=Map.ofList [2010,0]};
                {proven with NativeProcedureCalls=Map.ofList [2010,2]}] do
        Assert.That(select bad |> count, Is.Zero)
    let mutable native=true
    for item in after.Items do
        match item with
        | Instruction(254,_,_) -> native<-true
        | Instruction(255,_,_) -> native<-false
        | Instruction(236,_,_) -> Assert.That(native, Is.False)
        | Instruction((26|230),_,_) -> Assert.That(native, Is.True)
        | _ -> ()
    Assert.That((select after).Bytes, Is.EqualTo(box after.Bytes))
