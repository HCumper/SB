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
let private execute (overflow:bool) first finish step body =
    let loop=selected body
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

let private read v (cpu:Cpu) = int(int16(uint16(cpu.ReadWord(uint32(0x6000+v*32)))))

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
    let body=[op 109 [1];op 106 [3];op opcode [];op 106 [3];op 5 [];op 118 [2]]
    Assert.That((selected body).StackExtraBytes, Is.GreaterThan(12))
    Assert.That((plan 12 [] body).Loops, Is.Empty)
    let cpu=execute false -7 -1 3 body
    Assert.That(read 2 cpu, Is.EqualTo(if opcode=8 then 2 else 5))
