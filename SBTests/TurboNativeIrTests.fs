module SBTests.TurboNativeIrTests

open NUnit.Framework
open HirTurboIrBackend
open TurboNativeIr
open SBTests.M68kEmulator

let private op n args = HirTurboIrBackend.Instruction(n,List.map Word args,"")
let private block items = lower 0 (serialize items).Length items
let private allocation items = allocate 128 (block items) |> Option.defaultWith (fun () -> failwith "Unexpected allocation fallback.")

// This machine runs allocator output directly, independently of the template
// generator. Vector aliases, incoming maths words and caller registers are
// explicit so stack/alias/clobber regressions cannot hide behind the old codegen.
let private executeTo (expectOverflow:bool) (allocation:Allocation) aliases incoming =
    let bytes=emit -256 allocation
    let memory=Array.zeroCreate<byte> 0x10000
    let cpu=Cpu(memory)
    Array.blit bytes 0 memory 0x1000 bytes.Length
    cpu.Pc<-0x1000u
    for r in 0..7 do cpu.D[r]<-0x12340000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u
    for vector in 1..32 do
        let owner=aliases |> Map.tryFind vector |> Option.defaultValue vector
        cpu.WriteLong(uint32(0x7F00+vector*4),uint32(0x6000+owner*8))
    for vector in 1..32 do cpu.WriteWord(uint32(0x6000+vector*8),uint32 vector)
    for index,value in incoming |> List.indexed do cpu.WriteWord(0x5000u+uint32(index*2),uint32(value &&& 0xFFFF))
    let savedD=Array.copy cpu.D
    let savedA=Array.copy cpu.A
    let finish=0x1000u+uint32 bytes.Length
    let overflow=0x0AD8u // A6 ($8000) plus the library overflow displacement ($8AD8).
    while cpu.Pc<>finish && cpu.Pc<>overflow do
        if cpu.Steps>=100000L then failwith "Allocated block did not reach its boundary."
        cpu.Step()
    Assert.That(cpu.Pc=overflow, Is.EqualTo(expectOverflow),"Unexpected overflow outcome.")
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]),"The CPU spill frame was not released.")
    for r in 2..7 do Assert.That(cpu.D[r], Is.EqualTo(savedD[r]), $"D{r} was clobbered.")
    for r in [0;3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]), $"A{r} was clobbered.")
    cpu

let private execute allocation aliases incoming = executeTo false allocation aliases incoming

let private readVector vector (cpu:Cpu) = int(int16(uint16(cpu.ReadWord(uint32(0x6000+vector*8)))))

[<TestCase(5,7,3,10)>]
[<TestCase(6,7,3,4)>]
[<TestCase(6,3,7,-4)>]
[<TestCase(7,-7,3,-21)>]
let ``allocated arithmetic has correct operand order and word semantics`` (opcode:int,a:int,b:int,expected:int) =
    let code=allocation [op 106 [a];op 106 [b];op opcode [];op 118 [3]]
    let cpu=execute code Map.empty []
    Assert.That(readVector 3 cpu, Is.EqualTo(expected))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<TestCase(-32768)>]
[<TestCase(-129)>]
[<TestCase(-128)>]
[<TestCase(-1)>]
[<TestCase(0)>]
[<TestCase(127)>]
[<TestCase(128)>]
[<TestCase(32767)>]
let ``immediates retain their exact signed word bits`` (value:int) =
    let cpu=execute (allocation [op 106 [value];op 118 [1]]) Map.empty []
    Assert.That(readVector 1 cpu, Is.EqualTo(value))

[<Test>]
let ``incoming and outgoing maths values are materialized in stack order`` () =
    let cpu=execute (allocation [op 6 [];op 268 []]) Map.empty [3;10;99]
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(7u))
    Assert.That(cpu.ReadWord 0x5002u, Is.EqualTo(7u))
    Assert.That(cpu.ReadWord 0x5004u, Is.EqualTo(99u))

[<Test>]
let ``cached loads survive no writes and are invalidated by any aliased store`` () =
    let items=[op 106 [5];op 118 [1];op 109 [1];op 106 [7];op 118 [2];op 109 [1];op 5 [];op 118 [3]]
    let cpu=execute (allocation items) (Map.ofList[2,1]) []
    Assert.That(readVector 1 cpu, Is.EqualTo(7))
    Assert.That(readVector 3 cpu, Is.EqualTo(12))

[<Test>]
let ``copy chains reuse values while committing every store`` () =
    let items=[op 109 [1];op 118 [2];op 109 [2];op 118 [3];op 109 [3];op 118 [4]]
    let lowered=block items
    Assert.That(lowered.Instructions |> List.filter (function LoadVector _ -> true | _ -> false) |> List.length, Is.EqualTo(1))
    let cpu=execute (allocation items) Map.empty []
    for v in [2;3;4] do Assert.That(readVector v cpu, Is.EqualTo(1))

[<Test>]
let ``duplicate virtual values remain immutable after destructive instructions`` () =
    let items=[op 106 [5];op 268 [];op 106 [3];op 5 [];op 7 [];op 118 [1]]
    let cpu=execute (allocation items) Map.empty []
    Assert.That(readVector 1 cpu, Is.EqualTo(40))

[<Test>]
let ``negative copies preserve a still live source value`` () =
    let items=[op 106 [7];op 268 [];op 1 [];op 6 [];op 118 [1]]
    let allocated=allocation items
    Assert.That(allocated.Instructions |> List.exists (function Copy _ -> true | _ -> false), Is.True)
    let cpu=execute allocated Map.empty []
    Assert.That(readVector 1 cpu, Is.EqualTo(14))

[<Test>]
let ``register pressure spills and reloads without changing caller registers`` () =
    let items=[yield! [for v in 1..12 -> op 109 [v]];yield! List.replicate 11 (op 5 []);yield op 118 [20]]
    let allocated=allocation items
    Assert.That(allocated.SpillSlots, Is.GreaterThan(0))
    Assert.That(allocated.Instructions |> List.exists (function Reload _ -> true | _ -> false), Is.True)
    let cpu=execute allocated Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(78))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<Test>]
let ``spill budget exhaustion returns a template fallback`` () =
    let items=[yield! [for v in 1..12 -> op 109 [v]];yield! List.replicate 11 (op 5 []);yield op 118 [20]]
    Assert.That((allocate 0 (block items)).IsNone, Is.True)

[<Test>]
let ``spill slots are reused across separate pressure peaks`` () =
    let peak=[yield! [for v in 1..6 -> op 109 [v]];yield! List.replicate 5 (op 5 []);yield op 118 [20]]
    let one=allocation peak
    let two=allocation (peak@peak)
    Assert.That(two.SpillSlots, Is.EqualTo(one.SpillSlots))
    let cpu=execute two Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(21))

[<Test>]
let ``planning never crosses a label mode switch or opaque template`` () =
    let sequence=[op 109 [1];op 118 [2];op 109 [2];op 118 [3]]
    let items=[op 999 [4];op 255 [];yield! sequence;op 254 [];yield! sequence;LabelDef(2000,"");yield! sequence;op 12 [];yield! sequence;op 255 [];yield! sequence;op 0 []]
    let report=plan 128 -256 (serialize items) |> Result.defaultWith failwith
    Assert.That(report.Blocks.Length, Is.EqualTo(3))
    let boundaries=items |> List.scan (fun offset item -> offset+(serialize [item]).Length) 0
    for block in report.Blocks do Assert.That(boundaries, Does.Contain(block.Ir.StartOffset));Assert.That(boundaries, Does.Contain(block.Ir.EndOffset))

[<TestCase(337,8,9)>]
[<TestCase(338,8,-7)>]
[<TestCase(339,-1,0)>]
[<TestCase(340,-1,2)>]
let ``improver increments are lowered with their actual signed amounts`` (opcode:int,amount:int,expected:int) =
    let cpu=execute (allocation [op opcode [1;amount]]) Map.empty []
    Assert.That(readVector 1 cpu, Is.EqualTo(expected))

[<TestCase(1005,32767,1)>]
[<TestCase(1005,-32768,-1)>]
[<TestCase(1006,-32768,1)>]
[<TestCase(1006,32767,-1)>]
[<TestCase(1007,16384,2)>]
[<TestCase(1007,-16385,2)>]
[<TestCase(1007,256,256)>]
[<TestCase(1007,-32768,-1)>]
let ``checked binary failures restore the original left maths word and release spills`` (opcode:int,a:int,b:int) =
    let code=allocation [op opcode [];op 118 [20]]
    let cpu=executeTo true code Map.empty [b;a;99]
    Assert.That(cpu.A[1], Is.EqualTo(0x5002u))
    Assert.That(int(int16(uint16(cpu.ReadWord 0x5002u))), Is.EqualTo(a))
    Assert.That(cpu.ReadWord 0x5004u, Is.EqualTo(99u))
    Assert.That(readVector 20 cpu, Is.EqualTo(20),"An overflowing result must not reach its assignment.")

[<TestCase(1005,32766,1,32767)>]
[<TestCase(1005,-32767,-1,-32768)>]
[<TestCase(1006,32766,-1,32767)>]
[<TestCase(1006,-32767,1,-32768)>]
[<TestCase(1007,-32768,1,-32768)>]
[<TestCase(1007,32767,1,32767)>]
[<TestCase(1007,-128,256,-32768)>]
[<TestCase(1007,-181,-181,32761)>]
[<TestCase(1007,0,-32768,0)>]
let ``checked arithmetic accepts signed word boundaries`` (opcode:int,a:int,b:int,expected:int) =
    let cpu=execute (allocation [op 106 [a];op 106 [b];op opcode [];op 118 [20]]) Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))

[<Test>]
let ``checked negation traps on MIN with canonical maths stack`` () =
    let cpu=executeTo true (allocation [op 1001 [];op 118 [20]]) Map.empty [-32768;99]
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(32768u))
    Assert.That(cpu.ReadWord 0x5002u, Is.EqualTo(99u))

[<TestCase(-32767,32767)>]
[<TestCase(32767,-32767)>]
[<TestCase(0,0)>]
let ``checked negation succeeds without leaking its recovery frame`` (value:int,expected:int) =
    let cpu=execute (allocation [op 106 [value];op 1001 [];op 118 [20]]) Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))

[<Test>]
let ``overflow with live pressure values reconstructs the complete maths stack`` () =
    let items=[yield! [for v in 1..8 -> op 109 [v]];op 106 [32767];op 106 [1];op 1005 [];op 118 [20]]
    let code=allocation items
    Assert.That(code.SpillSlots, Is.GreaterThan(0))
    let cpu=executeTo true code Map.empty [99]
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u-18u))
    let values=[32767;8;7;6;5;4;3;2;1;99]
    for index,value in values |> List.indexed do
        Assert.That(cpu.ReadWord(cpu.A[1]+uint32(index*2)), Is.EqualTo(uint32 value))

[<TestCase(1339,32767,1,-32768)>]
[<TestCase(1340,-32768,1,32767)>]
[<TestCase(1339,-32768,-1,32767)>]
[<TestCase(1340,32767,-1,-32768)>]
let ``checked increment commits the same wrapped destination as the P improver`` (opcode:int,value:int,amount:int,expected:int) =
    let items=[op 106 [value];op 118 [1];op 106 [42];op opcode [1;amount]]
    let cpu=executeTo true (allocation items) Map.empty [99]
    Assert.That(readVector 1 cpu, Is.EqualTo(expected))
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(42u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

[<Test>]
let ``checked recovery participates in the spill budget`` () =
    Assert.That((allocate 0 (block [op 1006 [];op 118 [20]])).IsNone, Is.True)

[<Test>]
let ``huge native sequences have bounded recovery branch distances`` () =
    let items=[op 999 [3];op 254 [];yield! [for _ in 1..300 -> op 106 [1]];op 1001 [];op 255 [];op 0 []]
    let report=plan 128 -256 (serialize items) |> Result.defaultWith failwith
    for compiled in report.Blocks do
        Assert.That(compiled.Ir.EndOffset-compiled.Ir.StartOffset, Is.LessThanOrEqualTo(1536))

[<Test>]
let ``checked IDs round trip and validate vector operands`` () =
    let valid=serialize [op 999 [2];LabelDef(2000,"");op 254 [];op 1001 [];op 1005 [];op 1006 [];op 1007 [];op 1339 [1;1];op 1340 [1;1];LabelDef(2001,"");op 243 [2000];op 358 [2000];op 243 [2001];op 0 []]
    Assert.That(validate valid, Is.Empty)
    let roundTrip=decode valid |> Result.defaultWith failwith |> serialize
    Assert.That(roundTrip, Is.EqualTo(box valid))
    let invalid=serialize [op 999 [2];op 1340 [2;1];op 0 []]
    Assert.That(validate invalid, Has.Some.Contains("outside the reserved"))

let private qlDivision a b =
    let quotient,remainder=a/b,a%b
    if remainder<>0 && ((remainder<0)<>(b<0)) then quotient-1,remainder+b else quotient,remainder

let divisionCases = [|
    let edges=[-32768;-32767;-32766;-257;-256;-129;-128;-7;-3;-2;-1;0;1;2;3;7;127;128;255;256;32766;32767]
    for a in edges do
        for b in [-32768;-32767;-256;-7;-3;-1;1;2;3;7;256;32767] do
            if not(a= -32768 && b= -1) then
                for opcode in [8;20] do
                    yield TestCaseData(opcode,a,b).SetName($"Allocated DIVMOD {opcode}: {a}, {b}")
|]

[<TestCaseSource(nameof divisionCases)>]
let ``checked DIVMOD matches QL signed rounding across word boundaries`` (opcode:int,a:int,b:int) =
    let quotient,remainder=qlDivision a b
    let expected=if opcode=8 then quotient else remainder
    // Constants put the dividend in D0; maths inputs put it in D1. Exercise
    // both register orders independently of the original machine templates.
    let direct=execute (allocation [op 106 [a];op 106 [b];op opcode [];op 118 [20]]) Map.empty []
    let inputs=execute (allocation [op opcode [];op 118 [20]]) Map.empty [b;a;99]
    Assert.That(readVector 20 direct, Is.EqualTo(expected))
    Assert.That(readVector 20 inputs, Is.EqualTo(expected))
    Assert.That(direct.A[1], Is.EqualTo(0x5000u))
    Assert.That(inputs.A[1], Is.EqualTo(0x5004u))
    Assert.That(inputs.ReadWord 0x5004u, Is.EqualTo(99u))

[<TestCase(8,-7,3,0)>]
[<TestCase(20,-7,3,5)>]
[<TestCase(8,7,-3,-6)>]
[<TestCase(20,7,-3,-5)>]
let ``DIVMOD preserves a divisor reused after its register is clobbered`` (opcode:int,a:int,b:int,expected:int) =
    let cpu=execute (allocation [op 106 [a];op 106 [b];op opcode [];op 106 [b];op 5 [];op 118 [20]]) Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))

[<TestCase(8,-7,3,-10)>]
[<TestCase(20,-7,3,-5)>]
let ``DIVMOD preserves a duplicated dividend across the operation`` (opcode:int,a:int,b:int,expected:int) =
    let cpu=execute (allocation [op 106 [a];op 268 [];op 106 [b];op opcode [];op 5 [];op 118 [20]]) Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))

[<TestCase(8,7,1)>]
[<TestCase(8,-7,1)>]
[<TestCase(8,-32768,1)>]
[<TestCase(20,7,0)>]
[<TestCase(20,-7,0)>]
[<TestCase(20,-32768,0)>]
let ``identical DIVMOD operands get distinct scratch registers and preserve live values`` (opcode:int,value:int,expected:int) =
    let cpu=execute (allocation [op 106 [value];op 268 [];op 268 [];op opcode [];op 118 [20];op 118 [21]]) Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))
    Assert.That(readVector 21 cpu, Is.EqualTo(value))

let divisionFailures = [|
    for opcode in [8;20] do
        for a,b in [0,0;1,0;-1,0;-32768,0;32767,0;-32768,-1] do
            yield TestCaseData(opcode,a,b).SetName($"Allocated DIVMOD failure {opcode}: {a}, {b}")
|]

[<TestCaseSource(nameof divisionFailures)>]
let ``DIVMOD errors retain the original left maths word and never store a result`` (opcode:int,a:int,b:int) =
    let cpu=executeTo true (allocation [op opcode [];op 118 [20]]) Map.empty [b;a;99]
    Assert.That(cpu.A[1], Is.EqualTo(0x5002u))
    Assert.That(cpu.ReadWord 0x5002u, Is.EqualTo(uint32(a &&& 65535)))
    Assert.That(cpu.ReadWord 0x5004u, Is.EqualTo(99u))
    Assert.That(readVector 20 cpu, Is.EqualTo(20))

[<TestCase(8)>]
[<TestCase(20)>]
let ``DIVMOD errors reconstruct all live pressure values and release the spill frame`` (opcode:int) =
    let items=[yield! [for v in 1..8 -> op 109 [v]];op 106 [-32768];op 106 [-1];op opcode [];op 118 [20]]
    let allocated=allocation items
    Assert.That(allocated.SpillSlots, Is.GreaterThan(0))
    let cpu=executeTo true allocated Map.empty [99]
    let values=[-32768;8;7;6;5;4;3;2;1;99]
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u-18u))
    for index,value in values |> List.indexed do
        Assert.That(cpu.ReadWord(cpu.A[1]+uint32(index*2)), Is.EqualTo(uint32(value &&& 65535)))
    Assert.That(readVector 20 cpu, Is.EqualTo(20))

[<TestCase(8,33)>]
[<TestCase(20,38)>]
let ``DIVMOD retains pressure values on its success path`` (opcode:int,expected:int) =
    let items=[yield! [for v in 1..8 -> op 109 [v]];op 106 [-7];op 106 [3];op opcode [];yield! List.replicate 8 (op 5 []);op 118 [20]]
    let cpu=execute (allocation items) Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<TestCase(8)>]
[<TestCase(20)>]
let ``DIVMOD live values participate in the spill budget`` (opcode:int) =
    let items=[op 106 [-7];op 106 [3];op opcode [];op 106 [3];op 5 [];op 118 [20]]
    Assert.That((allocate 0 (block items)).IsNone, Is.True)

[<TestCase(8)>]
[<TestCase(20)>]
let ``identical zero DIVMOD operands take the runtime error path`` (opcode:int) =
    let cpu=executeTo true (allocation [op 106 [0];op 268 [];op opcode [];op 118 [20]]) Map.empty [99]
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.Zero)
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

let private arrayExecute items length incoming scalarAlias expectedError =
    let allocated=allocation items
    let code=emit -256 allocated
    let memory=Array.zeroCreate<byte> 0x40000
    let cpu=Cpu(memory)
    Array.blit code 0 memory 0x1000 code.Length
    for r in 0..7 do cpu.D[r]<-0x12340000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u;cpu.Pc<-0x1000u
    for v in 1..32 do
        cpu.WriteLong(uint32(0x7F00+v*4),uint32(0x6000+v*8))
        cpu.WriteWord(uint32(0x6000+v*8),uint32 v)
    cpu.WriteLong(0x7F78u,0x7000u) // vector 30, descriptor
    cpu.WriteLong(0x7000u,0x10000u);cpu.WriteLong(0x7004u,uint32(length*2))
    cpu.WriteWord(0x7008u,0u);cpu.WriteWord(0x700Au,uint32 length)
    for index in 0..length-1 do cpu.WriteWord(0x10000u+uint32(index*2),uint32(0x8000+index))
    if scalarAlias then cpu.WriteLong(0x7F04u,0x10000u);cpu.WriteWord(0x10000u,7u)
    let before=Array.copy memory[0x10000..0x1FFFF]
    for index,value in incoming |> List.indexed do cpu.WriteWord(0x5000u+uint32(index*2),uint32(value &&& 65535))
    let savedD=Array.copy cpu.D
    let savedA=Array.copy cpu.A
    let finish=0x1000u+uint32 code.Length
    while cpu.Pc<>finish && cpu.Pc<>0xAE0u && cpu.Pc<>0xAD8u do
        if cpu.Steps>100000L then failwith "Array native IR did not reach its boundary."
        cpu.Step()
    Assert.That(cpu.Pc, Is.EqualTo(if expectedError then 0xAE0u else finish))
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]))
    for r in 2..7 do Assert.That(cpu.D[r], Is.EqualTo(savedD[r]),$"D{r} was clobbered.")
    for r in [0;3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]),$"A{r} was clobbered.")
    if expectedError then Assert.That(memory[0x10000..0x1FFFF], Is.EqualTo(box before),"Failed array operation changed an element.")
    cpu

[<TestCase(1711,0,1)>]
[<TestCase(1811,0,1)>]
[<TestCase(1711,32767,32768)>]
[<TestCase(1811,32767,32768)>]
let ``rank-one loads preserve word bits and long offsets without borrowing loop registers`` (opcode:int,index:int,length:int) =
    let cpu=arrayExecute [op 106 [index];op opcode [30];op 118 [20]] length [99] false false
    Assert.That(cpu.ReadWord 0x60A0u, Is.EqualTo(uint32(0x8000+index)))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

[<TestCase(1712,0,1)>]
[<TestCase(1812,0,1)>]
[<TestCase(1712,32767,32768)>]
[<TestCase(1812,32767,32768)>]
let ``rank-one stores preserve value bits at the highest valid word index`` (opcode:int,index:int,length:int) =
    let cpu=arrayExecute [op 106 [index];op 106 [0xA55A];op opcode [30]] length [99] false false
    Assert.That(cpu.ReadWord(0x10000u+uint32(index*2)), Is.EqualTo(0xA55Au))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

let arrayFailures = [|
    for opcode in [1711;1712] do
        for index,length in [-32768,1;-1,1;1,1;32767,32767] do
            yield TestCaseData(opcode,index,length).SetName($"Rank-one array failure {opcode}, index {index}, length {length}") |]

[<TestCaseSource(nameof arrayFailures)>]
let ``rank-one bounds errors consume the index and store value before restoring pending words`` (opcode:int,index:int,length:int) =
    let items=[op 106 [42];op 106 [index]] @ (if opcode=1712 then [op 106 [9]] else []) @ [op opcode [30]]
    let cpu=arrayExecute items length [99] false true
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(42u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

[<Test>]
let ``rank-one store invalidates scalar aliases but retains immutable pending values`` () =
    let items=[op 109 [1];op 106 [0];op 106 [9];op 1712 [30];op 109 [1];op 5 [];op 118 [20]]
    let cpu=arrayExecute items 2 [] true false
    Assert.That(cpu.ReadWord 0x10000u, Is.EqualTo(9u))
    Assert.That(readVector 20 cpu, Is.EqualTo(16))

[<TestCase(1711)>]
[<TestCase(1811)>]
let ``a duplicated array index remains live after a load`` (opcode:int) =
    let cpu=arrayExecute [op 106 [1];op 268 [];op opcode [30];op 118 [20];op 118 [21]] 2 [] false false
    Assert.That(cpu.ReadWord 0x60A0u, Is.EqualTo(0x8001u))
    Assert.That(readVector 21 cpu, Is.EqualTo(1))

[<TestCase(1712)>]
[<TestCase(1812)>]
let ``shared array index and value use separate physical registers`` (opcode:int) =
    let cpu=arrayExecute [op 106 [1];op 268 [];op 268 [];op opcode [30];op 118 [20]] 2 [] false false
    Assert.That(cpu.ReadWord 0x10002u, Is.EqualTo(1u))
    Assert.That(readVector 20 cpu, Is.EqualTo(1))

[<Test>]
let ``proven rank-one accesses omit checks while preserving results`` () =
    let run opcode=arrayExecute [op 106 [1];op opcode [30];op 118 [20]] 2 [] false false
    let checkedCpu=run 1711
    let provenCpu=run 1811
    Assert.That(readVector 20 provenCpu, Is.EqualTo(readVector 20 checkedCpu))
    Assert.That(provenCpu.Steps, Is.LessThan(checkedCpu.Steps))

[<Test>]
let ``array RHS overflow precedes bounds failure and retains its pending index`` () =
    let items=[op 106 [-1];op 106 [32767];op 106 [1];op 1005 [];op 1712 [30]]
    let cpu=executeTo true (allocation items) Map.empty [99]
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFCu))
    Assert.That(cpu.ReadWord 0x4FFCu, Is.EqualTo(32767u))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(65535u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))
