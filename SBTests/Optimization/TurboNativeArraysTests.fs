module SBTests.TurboNativeArraysTests

open NUnit.Framework
open HirTurboIrBackend
open TurboNativeArrays
open SBTests.M68kEmulator

let private executeIn operations (op:int) (dimensions:int list) (indexes:int list) (value:int list) (fails:bool) =
    let pool=build 0x9000 operations
    let callSite=call -256 1 pool.Addresses[op]
    let memory=Array.zeroCreate<byte> 0x80000
    let cpu=Cpu(memory)
    Array.blit pool.Code 0 memory 0x1000 pool.Code.Length
    Array.blit callSite 0 memory 0x2000 callSite.Length
    for r in 0..7 do cpu.D[r]<-0x12340000u+uint32 r;cpu.A[r]<-0x3000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u
    cpu.WriteLong(0x7F04u,0x6000u)
    cpu.WriteLong(0x6000u,0x10000u)
    cpu.WriteLong(0x6004u,0x20000u)
    cpu.WriteWord(0x6008u,uint32(dimensions.Length-1))
    for i,bound in List.rev dimensions |> List.indexed do cpu.WriteWord(0x600Au+uint32(i*2),uint32 bound)
    let elementBytes=if baseOpcode op=111 || baseOpcode op=112 then 2 else 6
    let flat=List.zip dimensions indexes |> List.fold(fun offset (bound,index) -> offset*bound+index) 0
    let target=0x10000u+uint32(max 0 flat*elementBytes)
    let payload=if elementBytes=2 then [0xCDEF] else [0x0801;0xFEDC;0xBA98]
    if not fails then
        for i,word in payload |> List.indexed do cpu.WriteWord(target+uint32(i*2),uint32 word)
    let before=Array.copy memory[0x10000..0x10100]
    let incoming=value@(List.rev indexes)@[0x7BEE]
    for i,word in incoming |> List.indexed do cpu.WriteWord(0x5000u+uint32(i*2),uint32(word &&& 0xFFFF))
    let savedD=Array.copy cpu.D
    let savedA=Array.copy cpu.A
    cpu.Pc<-0x2000u
    let finish=0x2000u+uint32 callSite.Length
    while cpu.Pc<>finish && cpu.Pc<>0x0AE0u do
        if cpu.Steps>=10000L then failwith "Compact array access did not finish."
        cpu.Step()
    Assert.That(cpu.Pc=0x0AE0u, Is.EqualTo(fails),"Unexpected range-error outcome.")
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]),"Wrapper/indexer return frames leaked.")
    Assert.That(cpu.D[6], Is.EqualTo(savedD[6]),"The error line register was clobbered.")
    for r in [3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]),$"A{r} was clobbered.")
    if fails then Assert.That(memory[0x10000..0x10100], Is.EqualTo(box before),"A failed store modified the array.")
    cpu,target,payload

let private execute op dimensions indexes value fails = executeIn (Set.singleton op) op dimensions indexes value fails

[<TestCase(1111)>]
[<TestCase(1112)>]
[<TestCase(1143)>]
[<TestCase(1144)>]
[<TestCase(1611)>]
[<TestCase(1612)>]
[<TestCase(1643)>]
[<TestCase(1644)>]
let ``numeric helpers use row-major addresses and exact element bits`` (op:int) =
    let value=if baseOpcode op=112 then [0xABCD] elif baseOpcode op=144 then [0x0802;0x1234;0x5678] else []
    let cpu,target,payload=execute op [3;4;5] [2;1;3] value false
    let expected=if value.IsEmpty then payload else value
    let actual=if value.IsEmpty then cpu.A[1] else target
    for i,word in expected |> List.indexed do Assert.That(cpu.ReadWord(actual+uint32(i*2)), Is.EqualTo(uint32 word))
    let consumed=value.Length*2+6-(if value.IsEmpty then payload.Length*2 else 0)
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u+uint32 consumed))
    Assert.That(cpu.ReadWord(cpu.A[1]+(if value.IsEmpty then uint32(payload.Length*2) else 0u)), Is.EqualTo(0x7BEEu))

[<TestCase(1111,0)>]
[<TestCase(1111,32767)>]
[<TestCase(1112,32767)>]
[<TestCase(1143,0)>]
[<TestCase(1143,32767)>]
[<TestCase(1144,32767)>]
[<TestCase(1611,0)>]
[<TestCase(1611,32767)>]
[<TestCase(1612,32767)>]
[<TestCase(1643,0)>]
[<TestCase(1643,32767)>]
[<TestCase(1644,32767)>]
let ``one-dimensional bounds of 32768 and long byte offsets remain valid`` (op:int,index:int) =
    let value=if baseOpcode op=112 then [42] elif baseOpcode op=144 then [0x0800;0x4000;0] else []
    let cpu,target,payload=execute op [32768] [index] value false
    let expected=if value.IsEmpty then payload else value
    let actual=if value.IsEmpty then cpu.A[1] else target
    for i,word in expected |> List.indexed do Assert.That(cpu.ReadWord(actual+uint32(i*2)), Is.EqualTo(uint32 word))

let failures = [|
    for op in [1111;1112;1143;1144] do
        for indexes in [[-1;0];[3;0];[0;-1];[0;4];[-32768;0];[0;32767]] do
            yield TestCaseData(op,indexes).SetName($"Array range failure {op} {indexes}") |]

[<TestCaseSource(nameof failures)>]
let ``failures preserve reverse dimension-check order and partial stack consumption`` (op:int,indexes:int list) =
    let value=if op=1112 then [42] elif op=1144 then [0x0800;0x4000;0] else []
    let cpu,_,_=execute op [3;4] indexes value true
    let checkedIndexes=List.zip [3;4] indexes |> List.rev |> List.findIndex(fun (bound,index) -> index<0 || index>=bound) |> (+) 1
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u+uint32((value.Length+checkedIndexes)*2)))

[<Test>]
let ``helpers are emitted once and every call site is eight bytes`` () =
    let empty=build 0x9000 Set.empty
    Assert.That(empty.Code, Is.Empty)
    let pool=build 0x9000 (set[1111;1112;1143;1144])
    Assert.That(pool.Addresses.Count, Is.EqualTo(4))
    Assert.That(pool.Code.Length, Is.LessThan(150))
    for op in [1111;1112;1143;1144] do Assert.That((call -256 7 pool.Addresses[op]).Length, Is.EqualTo(8))

[<Test>]
let ``proven helper skips checks without changing multidimensional addresses or stack effects`` () =
    let checkedCpu,_,_=execute 1111 [3;4;5] [2;1;3] [] false
    let provenCpu,_,_=execute 1611 [3;4;5] [2;1;3] [] false
    Assert.That(provenCpu.A[1], Is.EqualTo(checkedCpu.A[1]))
    Assert.That(provenCpu.ReadWord provenCpu.A[1], Is.EqualTo(checkedCpu.ReadWord checkedCpu.A[1]))
    Assert.That(provenCpu.Steps, Is.LessThan(checkedCpu.Steps))
    Assert.That((build 0x9000 (Set.singleton 1611)).Code.Length, Is.LessThan((build 0x9000 (Set.singleton 1111)).Code.Length))

[<TestCase(1111)>]
[<TestCase(1611)>]
[<TestCase(1144)>]
[<TestCase(1644)>]
let ``mixed checked and proven helper pools call the correct indexer`` (opcode:int) =
    let value=if baseOpcode opcode=144 then [0x0801;0x4000;0] else []
    let cpu,target,payload=executeIn (set[1111;1112;1143;1144;1611;1612;1643;1644]) opcode [2;3] [1;2] value false
    let expected=if value.IsEmpty then payload else value
    let location=if value.IsEmpty then cpu.A[1] else target
    for i,word in expected |> List.indexed do Assert.That(cpu.ReadWord(location+uint32(i*2)), Is.EqualTo(uint32 word))

[<Test>]
let ``only explicit native checked array operations request shared helpers`` () =
    let op n args=Instruction(n,List.map Word args,"")
    let ir=serialize [op 999 [3];op 255 [];op 1111 [1];op 254 [];op 111 [1];op 1112 [1];LabelDef(2000,"");op 1143 [2];op 255 [];op 1144 [2];op 0 []]
    Assert.That(required ir |> Result.defaultWith failwith, Is.EqualTo(box(set[1112;1143])))

[<Test>]
let ``rank-one aliases share original helpers only outside allocated spans`` () =
    let op n args=Instruction(n,List.map Word args,"")
    let first=[op 999 [3];op 254 []]
    let a=op 1711 [1]
    let b=op 1812 [1]
    let start=(serialize first).Length
    let stop=start+(serialize [a]).Length
    let ir=serialize(first@[a;b;op 0 []])
    Assert.That(required ir |> Result.defaultWith failwith, Is.EqualTo(box(set[1111;1612])))
    Assert.That(requiredExcluding [start,stop] ir |> Result.defaultWith failwith, Is.EqualTo(box(set[1612])))
