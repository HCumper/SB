module SBTests.TurboNativeLoopsTests

open NUnit.Framework
open HirTurboIrBackend
open SBTests.M68kEmulator

let private op n args = Instruction(n,List.map Word args,"")
let private stream extra body =
    serialize ([op 999 [12];op 255 [];op 101 [1];op 114 [2];op 114 [3]] @ extra @
               [Instruction(357,[LabelRef 2001],"");Instruction(26,[Word 1;LabelRef 2000],"");op 355 [];LabelDef(2000,"")] @
               body @ [op 230 [1];Instruction(378,[LabelRef 2001],"");op 0 []])
let private plan budget extra body =
    TurboNativeLoops.plan budget -256 (stream extra body) |> Result.defaultWith failwith
let private selected body =
    let report=plan 140 [] body
    Assert.That(report.Loops.Length, Is.EqualTo(1))
    report.Loops.Head

// The runtime initializer has already accepted the first counter value. Test
// native loop code independently, with explicit descriptor/stack/register state.
let private executeLoop (loop:TurboNativeLoops.CompiledLoop) (overflow:bool) first finish step =
    let memory=Array.zeroCreate<byte> 0x10000
    let cpu=Cpu(memory)
    Array.blit loop.Code 0 memory 0x1000 loop.Code.Length
    for r in 0..7 do cpu.D[r]<-0xABCD0000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u;cpu.Pc<-0x1000u
    for v in 1..3 do cpu.WriteLong(uint32(0x7F00+v*4),uint32(0x6000+v*32))
    let cell=0x6020u
    cpu.WriteWord(cell,uint32(first &&& 65535))
    cpu.WriteWord(cell+6u,uint32(step &&& 65535));cpu.WriteWord(cell+8u,uint32(finish &&& 65535))
    let savedD=Array.copy cpu.D
    let savedA=Array.copy cpu.A
    let endPc=0x1000u+uint32 loop.Code.Length
    while cpu.Pc<>endPc && cpu.Pc<>0xAD8u do
        if cpu.Steps>2000000L then failwith "Native FOR exceeded its step limit."
        cpu.Step()
    Assert.That(cpu.Pc=0xAD8u, Is.EqualTo(overflow))
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]),"The complete loop/spill frame must be released.")
    for r in 2..7 do Assert.That(cpu.D[r], Is.EqualTo(savedD[r]),$"D{r} was clobbered.")
    for r in [0;3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]),$"A{r} was clobbered.")
    cpu

let private execute overflow first finish step body = executeLoop (selected body) overflow first finish step

let private read v (cpu:Cpu) = int(int16(uint16(cpu.ReadWord(uint32(0x6000+v*32)))))

let private codeWord (code:byte[]) offset = (int code[offset] <<< 8) ||| int code[offset+1]

let comparisonCases = [|
    for op in [9;10;11;12;13;14;15] do
        for left,right in [-32768,32767;32767,-32768;-32768,-32768;32767,32767;-1,0;0,-1;0,0;1,1;123,124] do
            for branch in [239;245] do yield TestCaseData(op,left,right,branch) |]

[<TestCaseSource(nameof comparisonCases)>]
let ``conditional native FOR comparisons retain signed word boundaries and QL truth`` (opcode:int,left:int,right:int,branch:int) =
    let expected=match opcode with 9 -> left>=right | 10 -> left>right | 11|12 -> left=right | 13 -> left<>right | 14 -> left<=right | _ -> left<right
    let body=[op 106 [0];op 118 [2];op 106 [left];op 106 [right];op opcode [];
              Instruction(branch,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    let loop=selected body
    Assert.That(loop.ControlFlow.IsSome, Is.True)
    Assert.That(loop.ControlFlow.Value.DirectComparisons, Is.EqualTo(1))
    Assert.That(loop.Allocation.Instructions |> List.exists(function TurboNativeIr.Comparison _ | TurboNativeIr.PushBoolean _ -> true | _ -> false), Is.False)
    let cpu=executeLoop loop false 1 1 1
    Assert.That(read 2 cpu, Is.EqualTo(if expected=(branch=239) then 1 else 0))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

let truthCases = [|
    for value in [-32768;-1;0;1;32767] do
        for integers,floats in [0,0;1,0;0,1;1,1;0,2;2,0] do
            for branch in [239;245] do yield TestCaseData(value,integers,floats,branch) |]

[<TestCaseSource(nameof truthCases)>]
let ``direct integer truth branches remove conversions and fold trailing NOT with exact parity`` (value:int,integers:int,floats:int,branch:int) =
    let body=[op 106 [0];op 118 [2];op 106 [value]] @ List.replicate integers (op 4 []) @
             [op 320 []] @ List.replicate floats (op 36 []) @
             [Instruction(branch,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    let loop=(plan 12 [] body).Loops |> List.exactlyOne
    let control=loop.ControlFlow.Value
    Assert.That(control.IntegerTruthTests, Is.EqualTo(1))
    Assert.That(control.FoldedNots, Is.EqualTo(integers+floats))
    Assert.That(control.CallStackBytes, Is.Zero)
    Assert.That(loop.Allocation.Instructions |> List.exists(function TurboNativeIr.Push _ -> true | _ -> false), Is.False)
    let expected=(value<>0) <> ((integers+floats)%2=1)
    let cpu=executeLoop loop false 1 1 1
    Assert.That(read 2 cpu, Is.EqualTo(if expected=(branch=239) then 1 else 0))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<Test>]
let ``a pinned integer counter condition needs only its word test and no helper stack`` () =
    let body=[op 109 [1];op 320 [];Instruction(239,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    Assert.That((plan 11 [] body).Loops, Is.Empty)
    let loop=(plan 12 [] body).Loops |> List.exactlyOne
    Assert.That(loop.Code |> Array.pairwise |> Array.exists(fun (a,b) -> a=0x4Auy && b=0x42uy), Is.True)
    Assert.That(loop.Allocation.Instructions |> List.exists(function TurboNativeIr.Copy _ | TurboNativeIr.Push _ -> true | _ -> false), Is.False)

[<TestCase(-1)>]
[<TestCase(0)>]
[<TestCase(1)>]
let ``direct stack truth consumes a word when the prior result must stay materialized`` (value:int) =
    let body=[op 106 [0];op 118 [2];op 106 [value];op 263 [];op 12 [];op 334 [];op 320 [];
              Instruction(239,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    let loop=selected body
    Assert.That(loop.ControlFlow.Value.Parts |> List.exists(function TurboNativeControlFlow.Branch(_,_,Some TurboNativeControlFlow.WordTop) -> true | _ -> false), Is.True)
    let cpu=executeLoop loop false 1 1 1
    Assert.That(read 2 cpu, Is.EqualTo(if value=0 then 1 else 0))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<Test>]
let ``direct comparison flags survive a real spill frame release`` () =
    let body=[op 109 [2];op 109 [3];op 109 [2];op 109 [3];op 1005 [];op 1005 [];op 1005 [];
              op 263 [];op 12 [];Instruction(239,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    let loop=selected body
    Assert.That(loop.Allocation.SpillSlots, Is.GreaterThan(0))
    Assert.That(loop.ControlFlow.Value.DirectComparisons, Is.EqualTo(1))
    let cpu=executeLoop loop false 1 1 1
    Assert.That(read 2 cpu, Is.EqualTo(1))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<Test>]
let ``direct comparison does not remove a preceding overflow check or corrupt its recovery operands`` () =
    let body=[op 106 [32767];op 106 [1];op 1005 [];op 263 [];op 10 [];
              Instruction(239,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    let loop=selected body
    Assert.That(loop.ControlFlow.Value.DirectComparisons, Is.EqualTo(1))
    let cpu=executeLoop loop true 1 1 1
    // The original checked ADD consumes its right word before reporting error.
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(32767u))

let floatTruthCases = [|
    for exponent,high,low,truth in [0,0,0,false;0x800,0x4000,0,true;0x801,0xC000,0,true;0x800,0x4000,1,true] do
        for nots in [0;1;2] do yield TestCaseData(exponent,high,low,truth,nots) |]

[<TestCaseSource(nameof floatTruthCases)>]
let ``float truth keeps its full mantissa while trailing NOT folds into the branch`` (exponent:int,high:int,low:int,truth:bool,nots:int) =
    let body=[op 106 [0];op 118 [2];op 138 [exponent;high;low]] @ List.replicate nots (op 36 []) @
             [Instruction(239,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    let loop=selected body
    Assert.That(loop.ControlFlow.Value.IntegerTruthTests, Is.Zero)
    let cpu=executeLoop loop false 1 1 1
    Assert.That(read 2 cpu, Is.EqualTo(if truth<>(nots%2=1) then 1 else 0))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<Test>]
let ``conditional native FOR joins reload stores and retain the pinned counter`` () =
    let body=[op 109 [1];op 106 [3];op 15 [];Instruction(239,[LabelRef 2010],"");
              op 109 [2];op 109 [1];op 1005 [];op 118 [2];Instruction(242,[LabelRef 2011],"");
              LabelDef(2010,"");op 109 [3];op 109 [1];op 1005 [];op 118 [3];LabelDef(2011,"")]
    let cpu=execute false 1 5 1 body
    Assert.That(read 2 cpu, Is.EqualTo(3))
    Assert.That(read 3 cpu, Is.EqualTo(12))
    Assert.That(read 1 cpu, Is.EqualTo(5))

[<TestCase(0,53)>]
[<TestCase(1,53)>]
[<TestCase(0,54)>]
[<TestCase(1,54)>]
let ``conditional native FOR does not short circuit errors or lose pending float booleans`` (left:int,logical:int) =
    let body=[op 106 [left];op 106 [1];op 12 [];op 106 [1];op 106 [0];op 8 [];
              op 106 [0];op 12 [];op logical [];Instruction(239,[LabelRef 2010],"");op 106 [99];op 118 [2];LabelDef(2010,"")]
    let loop=selected body
    Assert.That(loop.ControlFlow.Value.EagerBranches, Is.EqualTo(1))
    let cpu=executeLoop loop true 1 2 1
    Assert.That(cpu.A[1], Is.EqualTo(0x4FF8u))
    Assert.That(cpu.ReadWord 0x4FF8u, Is.EqualTo(1u))
    Assert.That(cpu.ReadLong 0x4FFAu, Is.EqualTo(if left=1 then 0x08014000u else 0u))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.Zero)

let eagerTruthCases = [|
    for left in [false;true] do
        for right in [-32768;0;32767] do
            for logical in [53;54] do
                for branch in [239;245] do
                    for integer in [false;true] do
                        for rightNot,wholeNot in [0,0;1,0;0,1;1,1] do
                            yield TestCaseData(left,right,logical,branch,integer,rightNot,wholeNot) |]

[<TestCaseSource(nameof eagerTruthCases)>]
let ``eager AND OR branches combine both evaluated operands with exact NOT and branch polarity`` (left:bool,right:int,logical:int,branch:int,integer:bool,rightNot:int,wholeNot:int) =
    let rhs=if integer then [op 106 [right];op 320 []] else [op 106 [right];op 263 [];op 13 []]
    let body=[op 106 [0];op 118 [2];op 377 [(if left then 0x801 else 0);(if left then 0x4000 else 0)]] @ rhs @
             List.replicate rightNot (op 36 []) @ [op logical []] @ List.replicate wholeNot (op 36 []) @
             [Instruction(branch,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    let loop=(plan 12 [] body).Loops |> List.exactlyOne
    Assert.That(loop.ControlFlow.Value.EagerBranches, Is.EqualTo(1))
    Assert.That(loop.ControlFlow.Value.CallStackBytes, Is.Zero)
    let rhs=(right<>0)<>(rightNot=1)
    let truth=(if logical=54 then left && rhs else left || rhs)<>(wholeNot=1)
    let cpu=executeLoop loop false 1 1 1
    Assert.That(read 2 cpu, Is.EqualTo(if truth=(branch=239) then 1 else 0))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<Test>]
let ``eager AND branch uses word displacement fallback after right comparison`` () =
    let body=[op 377 [0x801;0x4000];op 109 [1];op 106 [3];op 15 [];op 54 [];Instruction(239,[LabelRef 2010],"")] @
             (List.replicate 40 [op 106 [7];op 118 [2]] |> List.concat) @ [LabelDef(2010,"")]
    let loop=selected body
    Assert.That(loop.ControlFlow.Value.EagerBranches, Is.EqualTo(1))
    Assert.That([0..2..loop.Code.Length-4] |> List.exists(fun i -> codeWord loop.Code i=0x6C00 && int(int16(uint16(codeWord loop.Code (i+2))))>127), Is.True)
    Assert.That(read 2 (executeLoop loop false 1 4 1), Is.EqualTo(7))

[<Test>]
let ``conditional native FOR rejects external entries backward edges and nonempty joins`` () =
    let comparison=[op 109 [1];op 106 [3];op 15 []]
    let external=[Instruction(242,[LabelRef 2010],"")]
    let body=comparison@[Instruction(239,[LabelRef 2010],"");op 106 [1];op 118 [2];LabelDef(2010,"")]
    Assert.That((plan 140 external body).Loops, Is.Empty)
    Assert.That((plan 140 [] [LabelDef(2010,"");Instruction(242,[LabelRef 2010],"")]).Loops, Is.Empty)
    Assert.That((plan 140 [] (comparison@[Instruction(239,[LabelRef 2010],"");op 106 [1];LabelDef(2010,"")])).Loops, Is.Empty)

[<Test>]
let ``conditional native FOR reserves conversion helper saves and return depth`` () =
    let body=[op 109 [1];op 320 [];op 377 [0x801;0x4000];op 54 [];Instruction(239,[LabelRef 2010],"");op 106 [7];op 118 [2];LabelDef(2010,"")]
    Assert.That((plan 23 [] body).Loops, Is.Empty)
    let report=plan 24 [] body
    Assert.That(report.Loops.Length, Is.EqualTo(1))
    Assert.That(report.StackExtraBytes, Is.EqualTo(24))

[<Test>]
let ``conditional native FOR internal branches retain word displacement fallback`` () =
    let body=[op 109 [1];op 106 [3];op 15 [];Instruction(239,[LabelRef 2010],"")] @
             (List.replicate 40 [op 106 [7];op 118 [2]] |> List.concat) @ [LabelDef(2010,"")]
    let loop=selected body
    Assert.That([0..2..loop.Code.Length-4] |> List.exists(fun i -> codeWord loop.Code i=0x6C00 && int(int16(uint16(codeWord loop.Code (i+2))))>127), Is.True)
    Assert.That(read 2 (executeLoop loop false 1 4 1), Is.EqualTo(7))

[<TestCase(false,15,2)>]
[<TestCase(true,18,1)>]
let ``native FOR backedges use the inclusive minus 128 byte limit`` (literalStep:bool,copies:int,literals:int) =
    // Stores remain observable and give exact body lengths: six bytes from D2,
    // eight from an immediate. The shorter exit branches must be considered
    // before the backedge fits; an all-word layout would exceed byte range.
    let body=(List.replicate copies [op 109 [1];op 118 [2]] @
              List.replicate literals [op 106 [7];op 118 [3]]) |> List.concat
    let loop=(plan 140 (if literalStep then [op 106 [1]] else []) body).Loops.Head
    let backedge=loop.Code.Length-12-2 // counter commit and register restore follow it
    Assert.That(codeWord loop.Code backedge, Is.EqualTo(0x6080),"BRA.S -128 must fit after relaxation.")
    let cpu=executeLoop loop false 1 3 1
    Assert.That(read 1 cpu, Is.EqualTo(3))
    Assert.That(read 2 cpu, Is.EqualTo(3))

[<TestCase(false,18,0)>]
[<TestCase(true,17,2)>]
let ``native FOR backedges beyond byte range retain their word extension`` (literalStep:bool,copies:int,literals:int) =
    let body=(List.replicate copies [op 109 [1];op 118 [2]] @
              List.replicate literals [op 106 [7];op 118 [3]]) |> List.concat
    let loop=(plan 140 (if literalStep then [op 106 [1]] else []) body).Loops.Head
    let backedge=loop.Code.Length-12-4
    Assert.That(codeWord loop.Code backedge, Is.EqualTo(0x6000))
    Assert.That(int(int16(uint16(codeWord loop.Code (backedge+2)))), Is.EqualTo(-130))
    let cpu=executeLoop loop false 1 3 1
    Assert.That(read 1 cpu, Is.EqualTo(3))
    Assert.That(read 2 cpu, Is.EqualTo(3))

[<TestCase(1,0x6E04)>]
[<TestCase(-1,0x6D04)>]
let ``literal NEXT uses a short exit branch with the correct direction`` (step:int,opcode:int) =
    let loop=(plan 140 [op 106 [step]] [op 109 [1];op 118 [2]]).Loops.Head
    Assert.That(loop.Code.Length, Is.EqualTo(50))
    Assert.That(codeWord loop.Code 32, Is.EqualTo(opcode))
    Assert.That(codeWord loop.Code 36, Is.EqualTo(0x60EE)) // body at byte 20
    let first,finish=if step>0 then 1,3 else 3,1
    Assert.That(read 2 (executeLoop loop false first finish step), Is.EqualTo(finish))

[<TestCase(false,6L,9L)>]
[<TestCase(true,8L,11L)>]
let ``fetch counts include opcode and extension words but exclude data and hooks`` (zero:bool,steps:int64,words:int64) =
    let cpu=Cpu(Array.zeroCreate 0x1000)
    let code=[0x203C;0x1234;0x5678;0x2080;0x2210;0x6602;0x4E71;0x6600;0x0004;0x4E71;0x4E71]
    code |> List.iteri(fun i word -> cpu.WriteWord(uint32(i*2),uint32 word))
    cpu.A[0]<-0x200u;cpu.A[7]<-0xF00u
    while cpu.Pc<22u do
        if cpu.Pc=10u then cpu.Ccr<-(if zero then 4u else 0u)
        cpu.Step()
    Assert.That(cpu.D[1], Is.EqualTo(0x12345678u))
    Assert.That(cpu.Steps, Is.EqualTo(steps))
    Assert.That(cpu.InstructionWordsFetched, Is.EqualTo(words))
    cpu.Push 24u
    cpu.Hooks[22u]<-fun _ -> ()
    cpu.Step()
    Assert.That(cpu.Steps, Is.EqualTo(steps+1L))
    Assert.That(cpu.InstructionWordsFetched, Is.EqualTo(words))

[<TestCase(1,5,1)>]
[<TestCase(5,1,-1)>]
[<TestCase(-7,7,3)>]
[<TestCase(7,-7,-3)>]
[<TestCase(-32768,-32760,2)>]
[<TestCase(32767,32760,-2)>]
[<TestCase(32760,32765,10)>]
[<TestCase(-32760,-32765,-10)>]
let ``native NEXT retains word wrap and commits only valid candidate counters`` (first:int,finish:int,step:int) =
    let mutable expected=first
    let mutable running=true
    let mutable iterations=0
    while running do
        iterations<-iterations+1
        if iterations>65536 then failwith "Test loop did not terminate."
        let next=int(int16(uint16((expected+step) &&& 65535)))
        if (if step<0 then next>=finish else next<=finish) then expected<-next else running<-false
    let cpu=execute false first finish step [op 109 [1];op 118 [2]]
    Assert.That(read 1 cpu, Is.EqualTo(expected))
    Assert.That(read 2 cpu, Is.EqualTo(expected))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<TestCase(1,5,1)>]
[<TestCase(5,1,-1)>]
[<TestCase(-7,7,3)>]
[<TestCase(7,-7,-3)>]
[<TestCase(-32768,-32760,2)>]
[<TestCase(32767,32760,-2)>]
[<TestCase(32760,32765,10)>]
[<TestCase(-32760,-32765,-10)>]
[<TestCase(0,-1,-32768)>]
let ``literal STEP specializes native NEXT without changing wrapped iteration semantics`` (first:int,finish:int,step:int) =
    let body=[op 109 [1];op 118 [2]]
    let dynamic=selected body
    let constant=(plan 140 [op 106 [step]] body).Loops.Head
    Assert.That(constant.ConstantStep, Is.EqualTo(Some step))
    let reference=executeLoop dynamic false first finish step
    let actual=executeLoop constant false first finish step
    Assert.That(read 1 actual, Is.EqualTo(read 1 reference))
    Assert.That(read 2 actual, Is.EqualTo(read 2 reference))
    Assert.That(actual.Steps, Is.LessThan(reference.Steps))

[<Test>]
let ``literal STEP is not inferred across arithmetic labels or variable captures`` () =
    let body=[op 109 [1];op 118 [2]]
    for preceding in [[op 106 [3];op 106 [-1];op 5 []];[op 106 [3];LabelDef(2009,"")];[op 109 [3]]] do
        Assert.That((plan 140 preceding body).Loops.Head.ConstantStep.IsNone, Is.True)

[<TestCase(8)>]
[<TestCase(20)>]
let ``a later DIVMOD failure commits the current counter before restoring registers`` (opcode:int) =
    let cpu=execute true -1 1 1 [op 106 [20];op 109 [1];op opcode [];op 118 [2]]
    Assert.That(read 1 cpu, Is.Zero)
    Assert.That(read 2 cpu, Is.EqualTo(if opcode=8 then -20 else 0))
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(20u))

[<TestCase(1339,-25536)>]
[<TestCase(1340,25536)>]
let ``a later checked increment commits its wrapped store and the current counter`` (opcode:int,expected:int) =
    let cpu=execute true 1 4 1 [op opcode [2;20000]]
    Assert.That(read 1 cpu, Is.EqualTo(2))
    Assert.That(read 2 cpu, Is.EqualTo(expected))

[<Test>]
let ``overflow restores loop registers releases both frames and keeps committed stores`` () =
    let cpu=execute true 1 10 1 [op 109 [1];op 106 [20000];op 7 [];op 118 [2]]
    Assert.That(read 1 cpu, Is.EqualTo(2))
    Assert.That(read 2 cpu, Is.EqualTo(20000))
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(2u))

[<TestCase(1339,32760,20,-32756)>]
[<TestCase(1340,-32760,20,32756)>]
let ``checked increments keep wrapped stores before releasing the loop frame`` (opcode:int,value:int,amount:int,expected:int) =
    let cpu=execute true 1 10 1 [op 106 [value];op 118 [2];op opcode [2;amount]]
    Assert.That(read 1 cpu, Is.EqualTo(1))
    Assert.That(read 2 cpu, Is.EqualTo(expected))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

let rejectedBodies = [|
    [op 106 [3];op 118 [1]] // assigning the induction variable
    [op 339 [1;1]]
    [op 109 [4];op 118 [2]] // storage not known to be independent globals
    [op 109 [1];op 106 [0];op 12 []] // comparisons/branches are a later milestone
    [op 109 [1];op 1111 [2];op 118 [3]]
    [op 236 [2004]]
    [op 106 [1]] // unbalanced expression stack
    [op 118 [2]] // incoming expression-stack values
    [op 26 [1;2000]] // nested loops
|]

[<TestCaseSource(nameof rejectedBodies)>]
let ``unsupported bodies keep the ordinary FOR path`` (body:IrItem list) =
    Assert.That((plan 140 [] body).Loops, Is.Empty)

[<TestCase(102)>]
[<TestCase(110)>]
[<TestCase(294)>]
[<TestCase(327)>]
[<TestCase(325)>]
[<TestCase(364)>]
[<TestCase(227)>]
[<TestCase(278)>]
let ``binding error continuations and opaque memory mutations disable whole loop lowering`` (opcode:int) =
    // Use real operand shapes for every stream; the planner must reject them.
    let item=match opcode with 102|110|294|327 -> op opcode [3] | 227 -> Instruction(227,[Word 0;Bytes8 [||]],"") | _ -> op opcode []
    Assert.That((plan 140 [item] [op 109 [1];op 118 [2]]).Loops, Is.Empty)

[<Test>]
let ``tight stack headroom retains the original loop`` () =
    Assert.That((plan 11 [] [op 109 [1];op 118 [2]]).Loops, Is.Empty)
    Assert.That((plan 12 [] [op 109 [1];op 118 [2]]).Loops.Length, Is.EqualTo(1))

let private certifiedPlan proofs extra body =
    TurboNativeLoops.planWithProofs proofs 140 -256 (stream extra body) |> Result.defaultWith failwith
let private bindingProof counter scalars = Map.ofList[2000,{Counter=counter;ScalarVectors=Set.ofList scalars;InvariantScalarVectors=Set.empty;InvariantArrayVectors=Set.empty}]

[<Test>]
let ``per-loop certificates allow stable local bindings without weakening the byte-only path`` () =
    let body=[op 109 [1];op 118 [2]]
    let raw=stream [op 102 [1];op 110 [2]] body
    Assert.That((TurboNativeLoops.plan 140 -256 raw |> Result.defaultWith failwith).Loops, Is.Empty)
    let report=certifiedPlan (bindingProof 1 [1;2]) [op 102 [1];op 110 [2]] body
    Assert.That(report.Loops.Length, Is.EqualTo(1))
    Assert.That(report.Loops.Head.CertifiedBinding, Is.True)
    let cpu=executeLoop report.Loops.Head false 1 4 1
    Assert.That(read 1 cpu, Is.EqualTo(4))
    Assert.That(read 2 cpu, Is.EqualTo(4))

[<TestCase(2,1)>]
[<TestCase(1,0)>]
let ``per-loop certificates must match the counter and every scalar operand`` (counter:int,includeDestination:int) =
    let scalars=if includeDestination=1 then [1;2] else [1]
    Assert.That((certifiedPlan (bindingProof counter scalars) [op 102 [1]] [op 109 [1];op 118 [2]]).Loops, Is.Empty)

[<TestCaseSource(nameof rejectedBodies)>]
let ``binding certificates never expand the audited native body instruction set`` (body:IrItem list) =
    Assert.That((certifiedPlan (bindingProof 1 [1;2;3]) [op 102 [1]] body).Loops, Is.Empty)

[<TestCase(327)>]
[<TestCase(325)>]
[<TestCase(364)>]
[<TestCase(227)>]
[<TestCase(278)>]
let ``certified loops still reject error continuations and opaque memory mutation`` (opcode:int) =
    let item=match opcode with 327 -> op opcode [3] | 227 -> Instruction(227,[Word 0;Bytes8 [||]],"") | _ -> op opcode []
    Assert.That((certifiedPlan (bindingProof 1 [1;2]) [item] [op 109 [1];op 118 [2]]).Loops, Is.Empty)

[<TestCase(false)>]
[<TestCase(true)>]
let ``only a certified loop may coexist with the exact nonmutating ABS ROM call`` (certified:bool) =
    let extra=[Instruction(224,[Word 2;Word 7;Bytes8 [|2uy|]],"");Instruction(261,[Word 7;Bytes8 [|65uy;66uy;83uy|]],"")]
    let body=[op 109 [1];op 118 [2]]
    let report=if certified then certifiedPlan (bindingProof 1 [1;2]) extra body else plan 140 extra body
    Assert.That(report.Loops.Length, Is.EqualTo(if certified then 1 else 0))

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
let ``ROM exceptions require the exact name arity descriptor and no duplicate linkage`` (kind:int) =
    let name=if kind=0 then [|80uy;79uy;75uy;69uy|] else [|65uy;66uy;83uy|]
    let descriptors=if kind=1 then [|2uy;2uy|] else [|2uy|]
    let extra=[Instruction(224,[Word 2;Word 7;Bytes8 descriptors],"");Instruction(261,[Word 7;Bytes8 name],"")]
    let extra=if kind=2 then extra@[Instruction(261,[Word 7;Bytes8 name],"")] else extra
    Assert.That((certifiedPlan (bindingProof 1 [1;2]) extra [op 109 [1];op 118 [2]]).Loops, Is.Empty)

[<TestCase(0,"insufficient stack budget")>]
[<TestCase(1,"unsupported body opcode 16")>]
[<TestCase(2,"body writes the pinned counter")>]
let ``native loop diagnostics identify the actual failed eligibility check`` (kind:int,reason:string) =
    let body=match kind with 1 -> [op 16 []] | 2 -> [op 106 [1];op 118 [1]] | _ -> [op 109 [1];op 118 [2]]
    let report=plan (if kind=0 then 11 else 140) [] body
    Assert.That(report.RejectedLoops, Is.EqualTo(report.Rejections.Length))
    Assert.That(TurboNativeLoops.format report, Does.Contain(reason))

[<TestCase(8)>]
[<TestCase(20)>]
[<TestCase(1005)>]
let ``certified local counter failures release frames and commit the current counter`` (opcode:int) =
    let first=if opcode=1005 then 32767 else 1
    let operand=if opcode=1005 then 1 else 0
    let report=certifiedPlan (bindingProof 1 [1;2]) [op 102 [1];op 110 [2]]
                   [op 106 [17];op 118 [2];op 109 [1];op 106 [operand];op opcode [];op 118 [2]]
    Assert.That(report.Loops.Length, Is.EqualTo(1))
    let cpu=executeLoop report.Loops.Head true first first 1
    Assert.That(read 1 cpu, Is.EqualTo(first))
    Assert.That(read 2 cpu, Is.EqualTo(17))

[<Test>]
let ``oversized loop bodies retain the original path`` () =
    Assert.That((plan 140 [] (List.replicate 257 (op 2 []))).Loops, Is.Empty)

[<TestCase(8,-1)>]
[<TestCase(20,2)>]
let ``native FOR retains counter bound and step through signed DIVMOD`` (opcode:int,expected:int) =
    let cpu=execute false -7 -1 3 [op 109 [1];op 106 [3];op opcode [];op 118 [2]]
    Assert.That(read 1 cpu, Is.EqualTo(-1))
    Assert.That(read 2 cpu, Is.EqualTo(expected))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<TestCase(8,0)>]
[<TestCase(20,0)>]
[<TestCase(8,-1)>]
[<TestCase(20,-1)>]
let ``native FOR DIVMOD errors restore pinned registers and leave stores untouched`` (opcode:int,divisor:int) =
    let first=if divisor=0 then 1 else -32768
    let cpu=execute true first first 1 [op 106 [17];op 118 [2];op 109 [1];op 106 [divisor];op opcode [];op 118 [2]]
    Assert.That(read 1 cpu, Is.EqualTo(first))
    Assert.That(read 2 cpu, Is.EqualTo(17))
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(uint32(first &&& 65535)))

[<TestCase(8)>]
[<TestCase(20)>]
let ``native FOR DIVMOD spills coexist with its register save frame`` (opcode:int) =
    let body=[op 106 [3];op 118 [3];op 106 [0];op 118 [2];op 109 [1];op 109 [3];op opcode [];op 109 [3];op 5 [];op 118 [2]]
    Assert.That((selected body).StackExtraBytes, Is.GreaterThan(12))
    Assert.That((plan 12 [] body).Loops, Is.Empty)
    let cpu=execute false -7 -1 3 body
    Assert.That(read 2 cpu, Is.EqualTo(if opcode=8 then 2 else 5))

[<TestCase(8,-2)>]
[<TestCase(20,1)>]
let ``native FOR pinned counters can be reused across DIVMOD without a leaf frame`` (opcode:int,expected:int) =
    let body=[op 109 [1];op 106 [3];op opcode [];op 109 [1];op 5 [];op 118 [2]]
    Assert.That((plan 12 [] body).Loops.Length, Is.EqualTo(1))
    Assert.That((selected body).StackExtraBytes, Is.EqualTo(12))
    Assert.That(read 2 (execute false -7 -1 3 body), Is.EqualTo(expected))

[<TestCase(8,2)>]
[<TestCase(20,5)>]
let ``native FOR recreated divisors avoid an unnecessary leaf frame`` (opcode:int,expected:int) =
    let body=[op 109 [1];op 106 [3];op opcode [];op 106 [3];op 5 [];op 118 [2]]
    Assert.That((plan 12 [] body).Loops.Length, Is.EqualTo(1))
    Assert.That((selected body).StackExtraBytes, Is.EqualTo(12))
    let cpu=execute false -7 -1 3 body
    Assert.That(read 2 cpu, Is.EqualTo(expected))

let private arrayLoopFrom first checkedBounds fails =
    let load=if checkedBounds then 1711 else 1811
    let store=if checkedBounds then 1712 else 1812
    let sum=[op 109 [2];op 109 [3];op 109 [1];op load [4];op 1005 [];op 1005 [];op 118 [2]]
    let body=if fails then sum else [op 109 [1];op 109 [1];op store [4]] @ sum
    let report=plan 140 [op 126 [0;4]] body
    Assert.That(report.Loops.Length, Is.EqualTo(1))
    let loop=report.Loops.Head
    let memory=Array.zeroCreate<byte> 0x40000
    let cpu=Cpu(memory)
    Array.blit loop.Code 0 memory 0x1000 loop.Code.Length
    for r in 0..7 do cpu.D[r]<-0xABCD0000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u;cpu.Pc<-0x1000u
    for v in 1..3 do cpu.WriteLong(uint32(0x7F00+v*4),uint32(0x6000+v*32))
    cpu.WriteWord(0x6020u,uint32(first &&& 65535));cpu.WriteWord(0x6026u,1u);cpu.WriteWord(0x6028u,3u)
    cpu.WriteWord(0x6040u,0u);cpu.WriteWord(0x6060u,0u)
    cpu.WriteLong(0x7F10u,0x7000u);cpu.WriteLong(0x7000u,0x10000u)
    cpu.WriteWord(0x7008u,0u);cpu.WriteWord(0x700Au,if fails then 2u else 4u)
    cpu.WriteWord(0x10000u,1u);cpu.WriteWord(0x10002u,2u)
    let savedD=Array.copy cpu.D
    let savedA=Array.copy cpu.A
    let finish=0x1000u+uint32 loop.Code.Length
    while cpu.Pc<>finish && cpu.Pc<>0xAE0u && cpu.Pc<>0xAD8u do
        if cpu.Steps>100000L then failwith "Array FOR did not finish."
        cpu.Step()
    Assert.That(cpu.Pc, Is.EqualTo(if fails then 0xAE0u else finish))
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]))
    for r in 2..7 do Assert.That(cpu.D[r], Is.EqualTo(savedD[r]))
    for r in [0;3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]))
    cpu,loop

let private arrayLoop checkedBounds fails = arrayLoopFrom 0 checkedBounds fails

[<Test>]
let ``a negative pinned array index takes the range error path with complete cleanup`` () =
    let cpu,_=arrayLoopFrom -1 true true
    Assert.That(read 1 cpu, Is.EqualTo(-1))
    Assert.That(read 2 cpu, Is.Zero)
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFCu))
    Assert.That(cpu.ReadWord 0x4FFCu, Is.Zero)
    Assert.That(cpu.ReadWord 0x4FFEu, Is.Zero)

[<TestCase(false)>]
[<TestCase(true)>]
let ``rank-one arrays keep native FOR metadata and accumulator values intact`` (checkedBounds:bool) =
    let cpu,loop=arrayLoop checkedBounds false
    Assert.That(read 1 cpu, Is.EqualTo(3))
    Assert.That(read 2 cpu, Is.EqualTo(6))
    Assert.That(loop.StackExtraBytes, Is.GreaterThan(12))
    for i in 0..3 do Assert.That(cpu.ReadWord(0x10000u+uint32(i*2)), Is.EqualTo(uint32 i))

[<Test>]
let ``rank-one FOR bounds failure releases spill and loop frames and preserves pending words`` () =
    let cpu,loop=arrayLoop true true
    Assert.That(loop.StackExtraBytes, Is.GreaterThan(12))
    Assert.That(read 1 cpu, Is.EqualTo(2))
    Assert.That(read 2 cpu, Is.EqualTo(3))
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFCu))
    Assert.That(cpu.ReadWord 0x4FFCu, Is.Zero)
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(3u))
    Assert.That(cpu.ReadWord 0x10000u, Is.EqualTo(1u))
    Assert.That(cpu.ReadWord 0x10002u, Is.EqualTo(2u))

[<TestCase(0,8,false,false)>]
[<TestCase(3,0,false,false)>]
[<TestCase(3,8,true,false)>]
[<TestCase(3,0,false,true)>]
let ``rank-two native FOR keeps counters and releases both frames on either subscript failure`` (mask:int,stride:int,rowFails:bool,columnFails:bool) =
    let row=if rowFails then 4 else 1
    let body=[op 106 [row];op 109 [1];op 109 [1];op 1912 [4;mask;stride]
              op 109 [2];op 109 [3];op 106 [row];op 109 [1];op 1911 [4;mask;stride]
              op 1005 [];op 1005 [];op 118 [2]]
    let report=plan 140 [op 126 [1;4]] body
    Assert.That(report.Loops.Length, Is.EqualTo(1))
    let loop=report.Loops.Head
    let cpu=Cpu(Array.zeroCreate<byte> 0x40000)
    Array.blit loop.Code 0 cpu.Memory 0x1000 loop.Code.Length
    for r in 0..7 do cpu.D[r]<-0xABCD0000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u;cpu.Pc<-0x1000u
    for v in 1..3 do cpu.WriteLong(uint32(0x7F00+v*4),uint32(0x6000+v*32))
    cpu.WriteWord(0x6020u,0u);cpu.WriteWord(0x6026u,1u);cpu.WriteWord(0x6028u,3u)
    cpu.WriteLong(0x7F10u,0x7000u);cpu.WriteLong(0x7000u,0x10000u)
    cpu.WriteWord(0x7008u,1u);cpu.WriteWord(0x700Au,if columnFails then 2u else 8u);cpu.WriteWord(0x700Cu,4u)
    let savedD,savedA=Array.copy cpu.D,Array.copy cpu.A
    let finish=0x1000u+uint32 loop.Code.Length
    while cpu.Pc<>finish && cpu.Pc<>0xAE0u do
        if cpu.Steps>100000L then failwith "Rank-two FOR did not finish."
        cpu.Step()
    Assert.That(cpu.Pc, Is.EqualTo(if rowFails || columnFails then 0xAE0u else finish))
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]))
    for r in 2..7 do Assert.That(cpu.D[r], Is.EqualTo(savedD[r]))
    for r in [0;3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]))
    Assert.That(read 1 cpu, Is.EqualTo(if rowFails then 0 elif columnFails then 2 else 3))
    Assert.That(read 2 cpu, Is.EqualTo(if rowFails then 0 elif columnFails then 1 else 6))
    Assert.That(cpu.A[1], Is.EqualTo(if columnFails then 0x4FFEu else 0x5000u))
    if columnFails then Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(1u))
    if not rowFails then
        for column in 0..(if columnFails then 1 else 3) do
            Assert.That(cpu.ReadWord(uint32(0x10000+2*((if columnFails then 2 else 8)+column))), Is.EqualTo(uint32 column))

[<Test>]
let ``native FOR reuses checked arithmetic within each iteration without caching its counter`` () =
    let loop=selected [op 109 [1];op 106 [1];op 1005 [];op 109 [1];op 106 [1];op 1005 [];op 1007 [];op 118 [2]]
    Assert.That(loop.Body.Instructions |> List.filter(function TurboNativeIr.Binary _ -> true | _ -> false) |> List.length, Is.EqualTo(2))
    Assert.That(read 2 (executeLoop loop false 1 3 1), Is.EqualTo(16))

[<Test>]
let ``native FOR propagates successful constant results while retaining all stores`` () =
    let loop=selected [op 106 [3];op 118 [2];op 109 [2];op 106 [4];op 1005 [];op 118 [3];op 109 [3];op 109 [1];op 1005 [];op 118 [2]]
    Assert.That(loop.Body.Instructions |> List.filter(function TurboNativeIr.Binary _ -> true | _ -> false) |> List.length, Is.EqualTo(1))
    let cpu=executeLoop loop false 1 3 1
    Assert.That(read 2 cpu, Is.EqualTo(10))
    Assert.That(read 3 cpu, Is.EqualTo(7))

let private invariantLoop budget scalars arrays body =
    let proofs=Map.ofList[2000,{Counter=1;ScalarVectors=Set.ofList[1;2;3];InvariantScalarVectors=Set.ofList scalars;InvariantArrayVectors=Set.ofList arrays}]
    TurboNativeLoops.planWithProofs proofs budget -256 (stream [op 126 [1;4]] body) |> Result.defaultWith failwith |> _.Loops |> List.exactlyOne

let invariantRowCases = [|
    for mask in [0;1;2;3] do
        for stride in [0;8;32768] do
            for scalar in [false;true] do yield TestCaseData(mask,stride,scalar) |]

[<TestCaseSource(nameof invariantRowCases)>]
let ``InvariantArray retains a full long row address once and restores all saved registers`` (mask:int,stride:int,scalar:bool) =
    let row=if scalar then op 109 [3] else op 106 [1]
    let body=[row;op 109 [1];op 109 [1];op 1912 [4;mask;stride];op 109 [2];row;op 109 [1];op 1911 [4;mask;stride];op 1005 [];op 118 [2]]
    let loop=invariantLoop 140 [3] [4] body
    Assert.That(loop.ArrayInvariants.Rows.Length, Is.EqualTo(1))
    Assert.That(loop.ArrayInvariants.SaveBytes, Is.EqualTo(8))
    let cpu=Cpu(Array.zeroCreate<byte> 0x40000)
    Array.blit loop.Code 0 cpu.Memory 0x1000 loop.Code.Length
    for r in 0..7 do cpu.D[r]<-0xABCD0000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u;cpu.Pc<-0x1000u
    for v in 1..3 do cpu.WriteLong(uint32(0x7F00+v*4),uint32(0x6000+v*32))
    cpu.WriteWord(0x6026u,1u);cpu.WriteWord(0x6028u,3u);cpu.WriteWord(0x6060u,1u)
    cpu.WriteLong(0x7F10u,0x7000u);cpu.WriteLong(0x7000u,0x10000u)
    cpu.WriteWord(0x700Au,if stride=32768 then 32768u else 8u);cpu.WriteWord(0x700Cu,4u)
    let savedD,savedA=Array.copy cpu.D,Array.copy cpu.A
    let finish=0x1000u+uint32 loop.Code.Length
    let mutable multiplies=0
    while cpu.Pc<>finish do
        if cpu.Steps>100000L then failwith "Invariant row FOR did not finish."
        if cpu.ReadWord cpu.Pc &&& 0xF1C0u=0xC0C0u then multiplies<-multiplies+1
        cpu.Step()
    Assert.That(multiplies, Is.EqualTo(1),"One initialization must serve both accesses and all four iterations.")
    Assert.That(read 2 cpu, Is.EqualTo(6))
    Assert.That(read 1 cpu, Is.EqualTo(3))
    for column in 0..3 do Assert.That(cpu.ReadWord(uint32(0x10000+2*((if stride=32768 then 32768 else 8)+column))), Is.EqualTo(uint32 column))
    Assert.That(cpu.A[1], Is.EqualTo(savedA[1]))
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]))
    for r in 2..7 do Assert.That(cpu.D[r], Is.EqualTo(savedD[r]))
    for r in [0;3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]))

[<TestCase(4,0,0,4)>] // both bad: column wins, preserving the row
[<TestCase(4,8,0,-1)>] // only row bad: no pending row
[<TestCase(1,2,2,1)>] // initialized cache: a later column still fails
let ``InvariantArray keeps column first range errors and unwinds the complete retained frame`` (row:int,length:int,counter:int,pending:int) =
    let body=[op 106 [row];op 109 [1];op 109 [1];op 1912 [4;3;0]]
    let loop=invariantLoop 140 [] [4] body
    let cpu=Cpu(Array.zeroCreate<byte> 0x40000)
    Array.blit loop.Code 0 cpu.Memory 0x1000 loop.Code.Length
    for r in 0..7 do cpu.D[r]<-0xABCD0000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u;cpu.Pc<-0x1000u
    cpu.WriteLong(0x7F04u,0x6020u);cpu.WriteWord(0x6026u,1u);cpu.WriteWord(0x6028u,3u)
    cpu.WriteLong(0x7F10u,0x7000u);cpu.WriteLong(0x7000u,0x10000u)
    cpu.WriteWord(0x700Au,uint32 length);cpu.WriteWord(0x700Cu,4u)
    let savedD,savedA=Array.copy cpu.D,Array.copy cpu.A
    while cpu.Pc<>0xAE0u do
        if cpu.Steps>100000L then failwith "Invariant row error was lost."
        cpu.Step()
    Assert.That(read 1 cpu, Is.EqualTo(counter))
    Assert.That(cpu.A[1], Is.EqualTo(if pending<0 then 0x5000u else 0x4FFEu))
    if pending>=0 then Assert.That(cpu.ReadWord cpu.A[1], Is.EqualTo(uint32 pending))
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]))
    for r in 2..7 do Assert.That(cpu.D[r], Is.EqualTo(savedD[r]))
    for r in [0;3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]))

[<Test>]
let ``InvariantArray fallback respects absent proofs changing rows unknown descriptors and stack budget`` () =
    let access=[op 109 [3];op 109 [1];op 1911 [4;0;8];op 118 [2]]
    let cached=invariantLoop 140 [3] [4] access
    Assert.That(cached.ArrayInvariants.Rows.Length, Is.EqualTo(1))
    Assert.That((invariantLoop (cached.StackExtraBytes-1) [3] [4] access).ArrayInvariants.Rows, Is.Empty)
    Assert.That((invariantLoop 140 [] [4] access).ArrayInvariants.Rows, Is.Empty)
    Assert.That((invariantLoop 140 [3] [] access).ArrayInvariants.Rows, Is.Empty)
    let changing=access@[op 106 [2];op 118 [3]]
    Assert.That((invariantLoop 140 [3] [4] changing).ArrayInvariants.Rows, Is.Empty)
    Assert.That((plan 140 [op 126 [1;4]] access).Loops.Head.ArrayInvariants.Rows, Is.Empty)
    let counterRow=[op 109 [1];op 263 [];op 1911 [4;0;8];op 118 [2]]
    Assert.That((invariantLoop 140 [1] [4] counterRow).ArrayInvariants.Rows, Is.Empty)

[<Test>]
let ``InvariantArray never initializes an untaken conditional access or moves it before overflow`` () =
    let access=[op 106 [1];op 109 [1];op 1911 [4;3;0];op 118 [2]]
    let guarded=[op 263 [];op 320 [];Instruction(239,[LabelRef 2010],"")]@access@[LabelDef(2010,"")]
    let loop=invariantLoop 140 [] [4] guarded
    Assert.That(loop.ArrayInvariants.Rows.Length, Is.EqualTo(1))
    // There is no array descriptor in this memory setup.
    let cpu=executeLoop loop false 0 3 1
    Assert.That(read 2 cpu, Is.Zero)
    let overflow=[op 106 [32767];op 106 [1];op 1005 [];op 118 [2]]@access
    let cpu=executeLoop (invariantLoop 140 [] [4] overflow) true 0 3 1
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord cpu.A[1], Is.EqualTo(32767u))

[<Test>]
let ``InvariantArray bounds its saved registers and excludes helpers and incompatible access checks`` () =
    let accesses=[for row in [0;1;2;3] do yield! [op 106 [row];op 109 [1];op 1911 [4;0;8];op 118 [2]]]
    let loop=invariantLoop 140 [] [4] accesses
    Assert.That(loop.ArrayInvariants.Rows.Length, Is.EqualTo(3))
    Assert.That(loop.ArrayInvariants.SaveBytes, Is.EqualTo(16))
    let mixed=[op 106 [1];op 109 [1];op 1911 [4;0;8];op 118 [2];op 106 [1];op 109 [1];op 1911 [4;3;8];op 118 [2]]
    Assert.That((invariantLoop 140 [] [4] mixed).ArrayInvariants.Rows.Length, Is.EqualTo(2))
    let helper=[op 109 [1];op 320 [];op 263 [];op 320 [];op 53 [];Instruction(239,[LabelRef 2010],"")]@accesses@[LabelDef(2010,"")]
    let loop=invariantLoop 140 [] [4] helper
    Assert.That(loop.ControlFlow.Value.CallStackBytes, Is.GreaterThan(0))
    Assert.That(loop.ArrayInvariants.Rows, Is.Empty)

[<Test>]
let ``a reused division in native FOR still fails on a later zero counter`` () =
    let loop=selected [op 106 [20];op 109 [1];op 8 [];op 106 [20];op 109 [1];op 8 [];op 1005 [];op 118 [2]]
    Assert.That(loop.Body.Instructions |> List.filter(function TurboNativeIr.Divide _ -> true | _ -> false) |> List.length, Is.EqualTo(1))
    let cpu=executeLoop loop true -1 1 1
    Assert.That(read 1 cpu, Is.Zero)
    Assert.That(read 2 cpu, Is.EqualTo(-40))
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(20u))
