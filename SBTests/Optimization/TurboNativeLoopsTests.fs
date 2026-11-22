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
    [LabelDef(2005,"");op 109 [1];op 118 [2]]
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

[<Test>]
let ``a reused division in native FOR still fails on a later zero counter`` () =
    let loop=selected [op 106 [20];op 109 [1];op 8 [];op 106 [20];op 109 [1];op 8 [];op 1005 [];op 118 [2]]
    Assert.That(loop.Body.Instructions |> List.filter(function TurboNativeIr.Divide _ -> true | _ -> false) |> List.length, Is.EqualTo(1))
    let cpu=executeLoop loop true -1 1 1
    Assert.That(read 1 cpu, Is.Zero)
    Assert.That(read 2 cpu, Is.EqualTo(-40))
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(20u))
