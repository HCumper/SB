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

let compareFlagCases = [|
    for left,right in [-32768,32767;32767,-32768;-32768,-32768;32767,32767;-1,0;0,-1;0,0;1,1;123,124] do
        for location in [0;1;2] do yield TestCaseData(left,right,location) |]

[<TestCaseSource(nameof compareFlagCases)>]
let ``CompareFlags preserves signed CMP state and operands across register slot and immediate sources`` (left:int,right:int,location:int) =
    let source=match location with 0 -> RecoveryRegister 1 | 1 -> RecoverySlot 0 | _ -> RecoveryImmediate right
    let allocation={Instructions=[Immediate(0,left);Immediate(1,right);Spill(1,0);CompareFlags(0,source)];SpillSlots=1;Registers=Set.ofList[0;1]}
    let cpu=execute allocation Map.empty []
    let sr=cpu.StatusRegister
    Assert.That(((sr &&& 8u)<>0u)<>((sr &&& 2u)<>0u), Is.EqualTo((left<right)))
    Assert.That((sr &&& 4u)<>0u, Is.EqualTo((left=right)))
    Assert.That(int(int16(uint16 cpu.D[0])), Is.EqualTo(left))
    Assert.That(int(int16(uint16 cpu.D[1])), Is.EqualTo(right))

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
let ``constant pressure rematerializes exact words without a spill frame`` () =
    let values=[-32768;-129;-128;-1;0;1;127;128;32767]
    let items=[yield! [for n in values -> op 106 [n]];yield! List.replicate (values.Length-1) (op 5 []);op 118 [20]]
    let allocated=allocate 0 (block items) |> Option.defaultWith(fun () -> failwith "Constants must not require spill storage.")
    Assert.That(allocated.SpillSlots, Is.Zero)
    Assert.That(allocated.Instructions |> List.exists(function Spill _ | Reload _ -> true | _ -> false), Is.False)
    Assert.That(readVector 20 (execute allocated Map.empty []), Is.EqualTo(List.sum values))

let immediateArithmeticCases = [|
    for opcode in [5;6;7;1005;1006;1007] do
        for left in [-32768;-32767;-129;-8;-1;0;1;8;127;32767] do
            for right in [-32768;-129;-8;-1;0;1;8;127;32767] do
                yield TestCaseData(opcode,left,right).SetName($"Immediate arithmetic {opcode}: {left}, {right}") |]

[<TestCaseSource(nameof immediateArithmeticCases)>]
let ``immediate arithmetic preserves signed word results overflow flags and recovery`` (opcode:int,left:int,right:int) =
    let result=match opcode%1000 with 5 -> int64 left+int64 right | 6 -> int64 left-int64 right | _ -> int64 left*int64 right
    let overflow=opcode>=1000 && (result< -32768L || result>32767L)
    let allocated=allocation [op 106 [right];op opcode [];op 118 [20]]
    Assert.That(allocated.Instructions |> List.exists(function ArithmeticImmediate _ -> true | _ -> false), Is.True)
    let cpu=executeTo overflow allocated Map.empty [left;99]
    if overflow then
        Assert.That(readVector 20 cpu, Is.EqualTo(20))
        Assert.That(cpu.A[1], Is.EqualTo(0x5000u))
        Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(uint32(left &&& 65535)))
    else
        Assert.That(readVector 20 cpu, Is.EqualTo(int(int16(uint16(result &&& 65535L)))))
        Assert.That(cpu.A[1], Is.EqualTo(0x5002u))
    Assert.That(cpu.ReadWord 0x5002u, Is.EqualTo(99u))

[<TestCase(5,9,10)>]
[<TestCase(6,9,8)>]
[<TestCase(7,-7,-7)>]
let ``commuting a literal left operand never reverses subtraction`` (opcode:int,left:int,expected:int) =
    let cpu=execute (allocation [op 106 [left];op 109 [1];op opcode [];op 118 [20]]) Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))

[<TestCase(1005,7,9)>]
[<TestCase(1006,7,-5)>]
[<TestCase(1007,7,14)>]
let ``immediate arithmetic also targets D1 with another immutable value live`` (opcode:int,right:int,expected:int) =
    let allocated=allocation [op 109 [1];op 109 [2];op 106 [right];op opcode [];op 118 [20];op 118 [21]]
    Assert.That(allocated.Instructions |> List.exists(function ArithmeticImmediate(_,1,_) -> true | _ -> false), Is.True)
    let cpu=execute allocated Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))
    Assert.That(readVector 21 cpu, Is.EqualTo(1))

[<TestCase(8)>]
[<TestCase(20)>]
let ``division recovery rematerializes pending literals in exact maths stack order`` (opcode:int) =
    let pending=[-32768;-129;-128;-1;0;1;127;128;32767]
    let items=[yield! [for n in pending -> op 106 [n]];op 109 [1];op 106 [0];op opcode [];op 118 [20]]
    let allocated=allocate 0 (block items) |> Option.defaultWith(fun () -> failwith "Literal recovery must not require spill storage.")
    Assert.That(allocated.SpillSlots, Is.Zero)
    let cpu=executeTo true allocated Map.empty [99]
    let expected=1::(List.rev pending)@[99]
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u-uint32((pending.Length+1)*2)))
    for index,n in expected |> List.indexed do Assert.That(cpu.ReadWord(cpu.A[1]+uint32(index*2)), Is.EqualTo(uint32(n &&& 65535)))
    Assert.That(readVector 20 cpu, Is.EqualTo(20))

[<TestCase(1005,32767,1)>]
[<TestCase(1006,-32768,1)>]
[<TestCase(1007,32767,2)>]
let ``checked arithmetic restores a destroyed literal without spilling or clobbering V`` (opcode:int,left:int,right:int) =
    let items=[op 106 [left];op 109 [right];op opcode [];op 118 [20]]
    let allocated=allocate 0 (block items) |> Option.defaultWith(fun () -> failwith "Literal recovery must fit without a spill frame.")
    Assert.That(allocated.SpillSlots, Is.Zero)
    let cpu=executeTo true allocated Map.empty [99]
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(uint32(left &&& 65535)))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

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

let private verifyLiteralDivision opcode divisors =
    for divisor in divisors do
        let allocated=allocation [op 106 [divisor];op opcode [];op 118 [20]]
        Assert.That(allocated.Instructions |> List.exists(function Division _ -> true | _ -> false), Is.False)
        Assert.That(allocated.SpillSlots, Is.Zero)
        let code=emit -256 allocated
        let memory=Array.zeroCreate<byte> 0x10000
        let cpu=Cpu(memory)
        Array.blit code 0 memory 0x1000 code.Length
        cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u
        cpu.WriteLong(0x7F50u,0x60A0u)
        let finish=0x1000u+uint32 code.Length
        for left in -32768..32767 do
            cpu.WriteWord(0x5000u,uint32(left &&& 65535))
            cpu.A[1]<-0x5000u;cpu.Pc<-0x1000u
            let start=cpu.Steps
            while cpu.Pc<>finish do
                if cpu.Steps-start>50L then failwith $"Literal division failed to finish: {opcode}, {left}, {divisor}."
                cpu.Step()
            let quotient,remainder=qlDivision left divisor
            let expected=if opcode=8 then quotient else remainder
            let actual=readVector 20 cpu
            if actual<>expected || cpu.A[1]<>0x5002u || cpu.A[7]<>0xF000u then Assert.Fail($"Literal division {opcode}: {left}, {divisor}: expected {expected}, got {actual}.")

[<TestCase(8)>]
[<TestCase(20)>]
let ``positive literal power of two DIVMOD matches QL for every signed word`` (opcode:int) = verifyLiteralDivision opcode [for shift in 0..14 -> 1 <<< shift]

[<TestCase(8)>]
[<TestCase(20)>]
let ``positive literal DIVMOD corrects all signed words for odd large and boundary divisors`` (opcode:int) =
    verifyLiteralDivision opcode [3;5;7;10;97;997;10000;16383;16385;32766;32767]

let rawLiteralDivisors = [|
    for opcode in [8;20] do
        for raw,left in [65535,-32768;65536,7;32768,-32768;-65533,-7] do
            yield TestCaseData(opcode,raw,left).SetName($"Word-normalized DIVMOD literal {opcode}: {raw}, dividend {left}") |]

[<TestCaseSource(nameof rawLiteralDivisors)>]
let ``literal division proofs use the actual signed word rather than the host integer`` (opcode:int,raw:int,left:int) =
    let dividend,divisor,result=Value 0,Value 1,Value 2
    let b=int(int16(uint16 raw))
    let fails=b=0 || (left= -32768 && b= -1)
    let ir={block [] with Instructions=[Constant(divisor,raw);PopMaths dividend;Divide(result,(if opcode=8 then DivideWord else ModuloWord),dividend,divisor,[dividend]);StoreVector(20,result)]}
    let cpu=executeTo fails (allocate 128 ir |> Option.get) Map.empty [left;99]
    if fails then
        Assert.That(readVector 20 cpu, Is.EqualTo(20))
        Assert.That(cpu.A[1], Is.EqualTo(0x5000u))
        Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(uint32(left &&& 65535)))
    else
        let quotient,remainder=qlDivision left b
        Assert.That(readVector 20 cpu, Is.EqualTo(if opcode=8 then quotient else remainder))
        Assert.That(cpu.A[1], Is.EqualTo(0x5002u))

[<TestCase(8,-7,8,-8)>]
[<TestCase(20,-7,8,-6)>]
[<TestCase(8,7,8,7)>]
[<TestCase(20,7,8,14)>]
let ``power of two DIVMOD retains a duplicated dividend`` (opcode:int,left:int,divisor:int,expected:int) =
    let cpu=execute (allocation [op 106 [left];op 268 [];op 106 [divisor];op opcode [];op 5 [];op 118 [20]]) Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(expected))

[<Test>]
let ``scalar stores consume literal and spill operands without requiring a scratch reload`` () =
    let value,left,right,result=Value 0,Value 1,Value 2,Value 3
    let ir={block [] with Instructions=[LoadVector(value,1);LoadVector(left,2);LoadVector(right,3);Binary(result,AddWord,left,right);StoreVector(20,value);StoreVector(21,result)]}
    let allocated=allocate 128 ir |> Option.get
    Assert.That(allocated.Instructions |> List.exists(function Store(20,RecoverySlot _) -> true | _ -> false), Is.True)
    let cpu=execute allocated Map.empty []
    Assert.That(readVector 20 cpu, Is.EqualTo(1))
    Assert.That(readVector 21 cpu, Is.EqualTo(5))
    let literal=allocation [op 106 [32767];op 118 [20]]
    Assert.That(literal.Instructions |> List.exists(function Immediate _ -> true | _ -> false), Is.False)
    Assert.That(readVector 20 (execute literal Map.empty []), Is.EqualTo(32767))

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
    let items=[op 106 [-7];op 109 [3];op opcode [];op 109 [3];op 5 [];op 118 [20]]
    Assert.That((allocate 0 (block items)).IsNone, Is.True)

[<TestCase(8,0)>]
[<TestCase(20,5)>]
let ``DIVMOD reused literal divisors are recreated without spill storage`` (opcode:int,expected:int) =
    let items=[op 106 [-7];op 106 [3];op opcode [];op 106 [3];op 5 [];op 118 [20]]
    let allocated=allocate 0 (block items) |> Option.defaultWith(fun () -> failwith "A reused literal divisor must not require a spill frame.")
    Assert.That(allocated.SpillSlots, Is.Zero)
    Assert.That(readVector 20 (execute allocated Map.empty []), Is.EqualTo(expected))

[<TestCase(8,-2)>]
[<TestCase(20,3)>]
let ``read-only pinned values are recreated after DIVMOD without spill storage`` (opcode:int,expected:int) =
    let ir=block [op 106 [-7];op 109 [2];op opcode [];op 109 [2];op 5 [];op 118 [20]]
    let allocated=allocateWithContext 0 (Map.ofList[2,2]) ir |> Option.defaultWith(fun () -> failwith "Pinned values must not require spill storage.")
    Assert.That(allocated.SpillSlots, Is.Zero)
    Assert.That(allocated.Registers |> Set.forall(fun r -> r=0 || r=1), Is.True)
    Assert.That(readVector 20 (execute allocated Map.empty []), Is.EqualTo(expected))

[<TestCase(8)>]
[<TestCase(20)>]
let ``error snapshots can read a preserved pinned register directly`` (opcode:int) =
    let ir=block [op 109 [2];op 106 [0];op opcode [];op 118 [20]]
    let allocated=allocateWithContext 0 (Map.ofList[2,2]) ir |> Option.defaultWith(fun () -> failwith "Pinned recovery must not require spill storage.")
    let cpu=executeTo true allocated Map.empty [99]
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(2u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

[<TestCase(5,3)>]
[<TestCase(6,-1)>]
[<TestCase(7,2)>]
let ``read-only pinned arithmetic operands do not need a scratch copy`` (opcode:int,expected:int) =
    let ir=block [op 109 [1];op 109 [2];op opcode [];op 118 [20]]
    let allocated=allocateWithContext 0 (Map.ofList[2,2]) ir |> Option.get
    Assert.That(allocated.Instructions |> List.exists(function Arithmetic(_,_,2) -> true | _ -> false), Is.True)
    Assert.That(allocated.Instructions |> List.exists(function Copy(_,2) -> true | _ -> false), Is.False)
    Assert.That(readVector 20 (execute allocated Map.empty []), Is.EqualTo(expected))

[<TestCase(1005,32767)>]
[<TestCase(1006,-32768)>]
[<TestCase(1007,16384)>]
let ``checked pinned operands preserve the original dynamic left word on failure`` (opcode:int,left:int) =
    let ir=block [op 109 [2];op opcode [];op 118 [20]]
    let allocated=allocateWithContext 0 (Map.ofList[2,2]) ir |> Option.get
    let cpu=executeTo true allocated Map.empty [left;99]
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(uint32(left &&& 65535)))
    Assert.That(cpu.ReadWord 0x5002u, Is.EqualTo(99u))

[<TestCase(8)>]
[<TestCase(20)>]
let ``identical zero DIVMOD operands take the runtime error path`` (opcode:int) =
    let cpu=executeTo true (allocation [op 106 [0];op 268 [];op opcode [];op 118 [20]]) Map.empty [99]
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.Zero)
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

let private arrayExecuteAllocation allocated length incoming scalarAlias expectedError =
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

let private arrayExecute items length incoming scalarAlias expectedError = arrayExecuteAllocation (allocation items) length incoming scalarAlias expectedError

let private rankTwoExecute allocated rows columns dataPointer targetAddress scalarAlias expectedError =
    let code=emit -256 allocated
    let memory=Array.zeroCreate<byte> 0x80000
    let cpu=Cpu(memory)
    Array.blit code 0 memory 0x1000 code.Length
    for r in 0..7 do cpu.D[r]<-0x12340000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
    cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u;cpu.Pc<-0x1000u
    for v in 1..32 do
        cpu.WriteLong(uint32(0x7F00+v*4),uint32(0x6000+v*8))
        cpu.WriteWord(uint32(0x6000+v*8),uint32 v)
    cpu.WriteLong(0x7F78u,0x7000u)
    cpu.WriteLong(0x7000u,dataPointer)
    cpu.WriteWord(0x7008u,1u);cpu.WriteWord(0x700Au,uint32 columns);cpu.WriteWord(0x700Cu,uint32 rows)
    cpu.WriteWord(targetAddress,0xA55Au)
    if scalarAlias then cpu.WriteLong(0x7F04u,targetAddress);cpu.WriteWord(targetAddress,7u)
    cpu.WriteWord(0x5000u,99u)
    let savedD=Array.copy cpu.D
    let savedA=Array.copy cpu.A
    let before=Array.copy memory[0x10000..]
    let finish=0x1000u+uint32 code.Length
    while cpu.Pc<>finish && cpu.Pc<>0xAE0u && cpu.Pc<>0xAD8u do
        if cpu.Steps>100000L then failwith "Rank-two native IR did not finish."
        cpu.Step()
    Assert.That(cpu.Pc, Is.EqualTo(if expectedError then 0xAE0u else finish))
    Assert.That(cpu.A[7], Is.EqualTo(savedA[7]))
    for r in 2..7 do Assert.That(cpu.D[r], Is.EqualTo(savedD[r]),$"D{r} was clobbered.")
    for r in [0;3;4;5;6] do Assert.That(cpu.A[r], Is.EqualTo(savedA[r]),$"A{r} was clobbered.")
    if expectedError then Assert.That(memory[0x10000..], Is.EqualTo(box before),"Failed store changed array contents.")
    cpu

[<TestCase(1911,0,0)>]
[<TestCase(1911,3,8)>]
[<TestCase(1911,1,0)>]
[<TestCase(1911,2,8)>]
[<TestCase(1912,0,8)>]
[<TestCase(1912,3,0)>]
let ``rank-two native accesses use exact or current strides and long addresses`` (opcode:int,mask:int,stride:int) =
    let target=0x10000u+uint32(2*(2*8+3))
    let items=[op 106 [2];op 106 [3]] @ (if opcode=1912 then [op 106 [0x8001]] else []) @ [op opcode [30;mask;stride]] @ (if opcode=1911 then [op 118 [20]] else [])
    let cpu=rankTwoExecute (allocation items) 4 8 0x10000u target false false
    Assert.That((if opcode=1911 then cpu.ReadWord 0x60A0u else cpu.ReadWord target), Is.EqualTo(if opcode=1911 then 0xA55Au else 0x8001u))
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))

[<TestCase(0)>]
[<TestCase(32768)>]
let ``rank-two offsets can exceed a word without extra overflow traps`` (stride:int) =
    let items=[op 106 [1];op 106 [0];op 1911 [30;3;stride];op 118 [20]]
    let cpu=rankTwoExecute (allocation items) 2 32768 0x10000u 0x20000u false false
    Assert.That(cpu.ReadWord 0x60A0u, Is.EqualTo(0xA55Au))

[<Test>]
let ``rank-two unsigned word multiplication and long address wrap match Turbo`` () =
    let items=[op 106 [32767];op 106 [0];op 1911 [30;3;32768];op 118 [20]]
    let cpu=rankTwoExecute (allocation items) 32768 32768 0x80020000u 0x10000u false false
    Assert.That(cpu.ReadWord 0x60A0u, Is.EqualTo(0xA55Au))

[<TestCase(1911,-1,-1,true)>]
[<TestCase(1911,4,8,true)>]
[<TestCase(1911,-32768,0,false)>]
[<TestCase(1911,4,0,false)>]
[<TestCase(1912,1,8,true)>]
[<TestCase(1912,4,1,false)>]
let ``rank-two failures retain the correct consumed subscript prefix`` (opcode:int,row:int,column:int,columnFails:bool) =
    let items=[op 106 [42];op 106 [row];op 106 [column]] @ (if opcode=1912 then [op 106 [9]] else []) @ [op opcode [30;3;0]]
    let cpu=rankTwoExecute (allocation items) 4 8 0x10000u 0x10000u false true
    Assert.That(cpu.A[1], Is.EqualTo(if columnFails then 0x4FFCu else 0x4FFEu))
    if columnFails then Assert.That(cpu.ReadWord 0x4FFCu, Is.EqualTo(uint32(row &&& 65535)))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(42u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

[<Test>]
let ``rank-two stores invalidate scalar aliases without changing pending values`` () =
    let items=[op 109 [1];op 106 [0];op 106 [0];op 106 [9];op 1912 [30;3;0];op 109 [1];op 5 [];op 118 [20]]
    let cpu=rankTwoExecute (allocation items) 4 8 0x10000u 0x10000u true false
    Assert.That(cpu.ReadWord 0x10000u, Is.EqualTo(9u))
    Assert.That(readVector 20 cpu, Is.EqualTo(16))

[<Test>]
let ``rank-two register pressure preserves three live store operands`` () =
    let items=[op 109 [1];op 268 [];op 109 [2];op 109 [1];op 109 [2];op 5 [];op 1912 [30;3;8];op 109 [1];op 109 [2];op 5 [];op 118 [20]]
    // row=1, column=2, value=3, with a duplicated row remaining pending.
    let cpu=rankTwoExecute (allocation items) 4 8 0x10000u 0x10014u false false
    Assert.That(cpu.ReadWord 0x10014u, Is.EqualTo(3u))
    Assert.That(readVector 20 cpu, Is.EqualTo(3))

[<Test>]
let ``rank-two proven dimensions reduce checks without changing the result`` () =
    let run mask=rankTwoExecute (allocation [op 106 [1];op 106 [2];op 1911 [30;mask;8];op 118 [20]]) 4 8 0x10000u 0x10014u false false
    let checkedCpu,provenCpu=run 3,run 0
    Assert.That(readVector 20 provenCpu, Is.EqualTo(readVector 20 checkedCpu))
    Assert.That(provenCpu.Steps, Is.LessThan(checkedCpu.Steps))

[<TestCase(false)>]
[<TestCase(true)>]
let ``an array store reads a spilled word directly without reloading it`` (fails:bool) =
    let value,left,right,index=Value 0,Value 1,Value 2,Value 3
    let ir={block [] with Instructions=[LoadVector(value,1);LoadVector(left,2);LoadVector(right,3);Binary(index,AddWord,left,right);StoreArrayWord(30,true,index,value,[])]}
    let allocated=allocate 128 ir |> Option.get
    Assert.That(allocated.Instructions |> List.exists(function StoreElement(_,_,_,RecoverySlot _,_) -> true | _ -> false), Is.True)
    let cpu=arrayExecuteAllocation allocated (if fails then 5 else 8) [] false fails
    Assert.That(cpu.A[1], Is.EqualTo(0x5000u))
    if not fails then Assert.That(cpu.ReadWord 0x1000Au, Is.EqualTo(1u))

[<TestCase(1712)>]
[<TestCase(1812)>]
let ``array stores retain live indexes and use literal operands without copies or spills`` (opcode:int) =
    let items=[op 109 [1];op 268 [];op 106 [12345];op opcode [30];op 118 [20]]
    let allocated=allocation items
    Assert.That(allocated.SpillSlots, Is.Zero)
    Assert.That(allocated.Instructions |> List.exists(function StoreElement(_,_,_,RecoveryImmediate 12345,_) -> true | _ -> false), Is.True)
    let cpu=arrayExecuteAllocation allocated 2 [] false false
    Assert.That(cpu.ReadWord 0x10002u, Is.EqualTo(12345u))
    Assert.That(readVector 20 cpu, Is.EqualTo(1))

[<TestCase(1711)>]
[<TestCase(1811)>]
let ``array loads use a free result register instead of spilling a live dynamic index`` (opcode:int) =
    let items=[op 109 [1];op 268 [];op opcode [30];op 118 [20];op 118 [21]]
    let allocated=allocate 0 (block items) |> Option.get
    Assert.That(allocated.Instructions |> List.exists(function LoadElement(_,_,index,result,_) -> index<>result | _ -> false), Is.True)
    let cpu=arrayExecuteAllocation allocated 2 [] false false
    Assert.That(cpu.ReadWord 0x60A0u, Is.EqualTo(0x8001u))
    Assert.That(readVector 21 cpu, Is.EqualTo(1))

[<Test>]
let ``a checked free-result load reconstructs its unchanged dynamic index on failure`` () =
    let allocated=allocate 0 (block [op 109 [1];op 268 [];op 1711 [30];op 118 [20];op 118 [21]]) |> Option.get
    let cpu=arrayExecuteAllocation allocated 1 [99] false true
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFEu))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(1u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

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
let ``shared array index and value remain stable while computing the address`` (opcode:int) =
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

let private allocateBlock ir = allocate 128 ir |> Option.defaultWith(fun () -> failwith "Unexpected value-optimization allocation fallback.")
let private compareOptimized overflow ir aliases incoming =
    let before=executeTo overflow (allocateBlock ir) aliases incoming
    let after=executeTo overflow (allocateBlock (optimize ir)) aliases incoming
    for vector in 1..32 do Assert.That(readVector vector after, Is.EqualTo(readVector vector before),$"Vector {vector}")
    Assert.That(after.A[1], Is.EqualTo(before.A[1]),"Maths-stack position changed.")
    let mutable address=before.A[1]
    while address<0x5000u+uint32(incoming.Length*2) do
        Assert.That(after.ReadWord address, Is.EqualTo(before.ReadWord address),$"Maths word at {address:X}")
        address<-address+2u
    after

[<Test>]
let ``value optimization folds assignment chains without removing stores`` () =
    let ir=block [op 106 [3];op 118 [1];op 109 [1];op 106 [4];op 1005 [];op 118 [2];op 109 [2];op 106 [2];op 1007 [];op 118 [3];op 109 [3];op 1001 [];op 118 [4]]
    let optimized=optimize ir
    Assert.That(optimized.Instructions |> List.filter(function StoreVector _ -> true | _ -> false) |> List.length, Is.EqualTo(4))
    Assert.That(optimized.Instructions |> List.exists(function Binary _ | Negate _ | OverflowCheck _ -> true | _ -> false), Is.False)
    Assert.That(optimize optimized, Is.EqualTo(optimized),"The pass must be idempotent.")
    let cpu=compareOptimized false ir Map.empty []
    Assert.That([for vector in 1..4 -> readVector vector cpu], Is.EqualTo(box [3;7;14;-14]))

[<TestCase(65535,1,0)>]
[<TestCase(65536,1,1)>]
[<TestCase(-65533,4,7)>]
let ``value optimization normalizes raw host constants before folding`` (raw:int,right:int,expected:int) =
    let a,b,result=Value 0,Value 1,Value 2
    let ir={block [] with Instructions=[Constant(a,raw);Constant(b,right);Binary(result,AddWord,a,b);OverflowCheck(None,[a],None);StoreVector(20,result)]}
    Assert.That(optimize ir |> _.Instructions |> List.exists(function Binary _ | OverflowCheck _ -> true | _ -> false), Is.False)
    Assert.That(readVector 20 (compareOptimized false ir Map.empty []), Is.EqualTo(expected))

[<Test>]
let ``folded constants match unoptimized word arithmetic and checked boundaries`` () =
    let values=[-32768;-32767;-129;-1;0;1;2;127;128;16384;32766;32767]
    for opcode in [5;6;7;1005;1006;1007] do
        for a in values do
            for b in values do
                let result=match opcode%1000 with 5 -> int64 a+int64 b | 6 -> int64 a-int64 b | _ -> int64 a*int64 b
                let fails=opcode>=1000 && (result< -32768L || result>32767L)
                compareOptimized fails (block [op 106 [a];op 106 [b];op opcode [];op 118 [20]]) Map.empty [99] |> ignore
    for opcode in [1;1001] do
        for value in values do
            compareOptimized (opcode=1001 && value= -32768) (block [op 106 [value];op opcode [];op 118 [20]]) Map.empty [99] |> ignore

[<Test>]
let ``folded DIVMOD matches QL floor rounding and retains quotient errors`` () =
    let values=[-32768;-32767;-7;-3;-1;0;1;2;3;7;32767]
    for opcode in [8;20] do
        for a in values do
            for b in values do
                let fails=b=0 || (a= -32768 && b= -1)
                let ir=block [op 106 [a];op 106 [b];op opcode [];op 118 [20]]
                let optimized=optimize ir
                Assert.That(optimized.Instructions |> List.exists(function Divide _ -> true | _ -> false), Is.EqualTo(fails))
                compareOptimized fails ir Map.empty [99] |> ignore

[<TestCase(1005,32767,1)>]
[<TestCase(1006,-32768,1)>]
[<TestCase(1007,16384,2)>]
[<TestCase(8,-32768,-1)>]
[<TestCase(20,-32768,-1)>]
[<TestCase(8,7,0)>]
[<TestCase(20,7,0)>]
let ``a later failure reconstructs folded pending values in the original order`` (opcode:int,a:int,b:int) =
    let ir=block [op 106 [10];op 106 [20];op 1005 [];op 106 [a];op 106 [b];op opcode [];op 118 [20]]
    let cpu=compareOptimized true ir Map.empty [99]
    Assert.That(cpu.A[1], Is.EqualTo(0x4FFCu))
    Assert.That(cpu.ReadWord 0x4FFCu, Is.EqualTo(uint32(a &&& 65535)))
    Assert.That(cpu.ReadWord 0x4FFEu, Is.EqualTo(30u))
    Assert.That(cpu.ReadWord 0x5000u, Is.EqualTo(99u))

[<TestCase(1339,32760,20,-32756)>]
[<TestCase(1340,-32760,20,32756)>]
let ``constant overflow retains an increment's wrapped store before trapping`` (opcode:int,value:int,amount:int,expected:int) =
    let ir=block [op 106 [value];op 118 [1];op opcode [1;amount]]
    Assert.That(readVector 1 (compareOptimized true ir Map.empty [99]), Is.EqualTo(expected))

[<TestCase(1005,1007,9)>]
[<TestCase(1007,1005,4)>]
[<TestCase(1006,1007,-1)>]
let ``value numbering reuses commutative results without reversing subtraction`` (opcode:int,combine:int,expected:int) =
    let ir=block [op 109 [1];op 109 [2];op opcode [];op 109 [2];op 109 [1];op opcode [];op combine [];op 118 [20]]
    let count=optimize ir |> _.Instructions |> List.filter(function Binary _ -> true | _ -> false) |> List.length
    Assert.That(count, Is.EqualTo(if opcode=1006 then 3 else 2))
    Assert.That(readVector 20 (compareOptimized false ir Map.empty []), Is.EqualTo(expected))

[<TestCase(8,0)>]
[<TestCase(20,2)>]
let ``a checked DIVMOD result is reused only after the first successful execution`` (opcode:int,expected:int) =
    let ir=block [op 109 [1];op 109 [2];op opcode [];op 109 [1];op 109 [2];op opcode [];op 1005 [];op 118 [20]]
    Assert.That(optimize ir |> _.Instructions |> List.filter(function Divide _ -> true | _ -> false) |> List.length, Is.EqualTo(1))
    Assert.That(readVector 20 (compareOptimized false ir Map.empty []), Is.EqualTo(expected))
    compareOptimized true ir (Map.ofList[2,0]) [] |> ignore // Uninitialized owner zero supplies a zero divisor.

[<Test>]
let ``a repeated checked negation keeps its original failure point`` () =
    let ir=block [op 109 [1];op 1001 [];op 109 [1];op 1001 [];op 1005 [];op 118 [20]]
    Assert.That(optimize ir |> _.Instructions |> List.filter(function Negate _ -> true | _ -> false) |> List.length, Is.EqualTo(1))
    Assert.That(readVector 20 (compareOptimized false ir Map.empty []), Is.EqualTo(-2))

[<TestCase(5,32767,1)>]
[<TestCase(6,-32768,1)>]
[<TestCase(7,16384,2)>]
let ``an unchecked result cannot suppress a later checked overflow`` (opcode:int,a:int,b:int) =
    let left,right,first,second=Value 0,Value 1,Value 2,Value 3
    let operation=match opcode with 5 -> AddWord | 6 -> SubtractWord | _ -> MultiplyWord
    let ir={block [] with Instructions=[PopMaths right;PopMaths left;Binary(first,operation,left,right);StoreVector(20,first);Binary(second,operation,left,right);OverflowCheck((if opcode=7 then Some second else None),[left],None);StoreVector(21,second)]}
    Assert.That(optimize ir |> _.Instructions |> List.filter(function Binary _ -> true | _ -> false) |> List.length, Is.EqualTo(2))
    compareOptimized true ir Map.empty [b;a;99] |> ignore

[<Test>]
let ``value propagation respects scalar aliases and keeps both memory reads`` () =
    let ir=block [op 106 [3];op 118 [1];op 106 [7];op 118 [2];op 109 [1];op 106 [4];op 1005 [];op 118 [20]]
    Assert.That(readVector 20 (compareOptimized false ir (Map.ofList[2,1]) []), Is.EqualTo(11))
    let arrays=block [op 106 [0];op 1711 [30];op 106 [0];op 106 [9];op 1712 [30];op 106 [0];op 1711 [30];op 1005 [];op 118 [20]]
    Assert.That(optimize arrays |> _.Instructions |> List.filter(function LoadArrayWord _ -> true | _ -> false) |> List.length, Is.EqualTo(2))
    Assert.That(optimize arrays |> _.Instructions |> List.filter(function StoreArrayWord _ -> true | _ -> false) |> List.length, Is.EqualTo(1))

[<Test>]
let ``unsupported flag-check relationships are left intact`` () =
    let ir={block [] with Instructions=[Constant(Value 0,3);OverflowCheck(None,[],None);PushMaths(Value 0)]}
    Assert.That(optimize ir, Is.EqualTo(ir))
