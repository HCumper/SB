#r "C:/Source/SB/SB/bin/Debug/net10.0/SB.dll"
#r "C:/Source/SB/SBTests/bin/Debug/net10.0/SBTests.dll"
open HirTurboIrBackend
open SBTests.M68kEmulator
let op n args=Instruction(n,List.map Word args,"")
let body=[for row in 0..2 do yield! [op 106 [row];op 109 [1];op 109 [1];op 1912 [4;3;8]]] @
         [op 109 [2];op 106 [1];op 1005 [];op 118 [2]]
let source=serialize ([op 999 [12];op 255 [];op 101 [1];op 114 [2];op 126 [1;4];
                       Instruction(357,[LabelRef 2001],"");Instruction(26,[Word 1;LabelRef 2000],"");op 355 [];LabelDef(2000,"")]
                      @body@[op 230 [1];Instruction(378,[LabelRef 2001],"");op 0 []])
let proofs=Map.ofList[2000,{Counter=1;ScalarVectors=Set.ofList[1;2];InvariantScalarVectors=Set.empty;InvariantArrayVectors=Set.singleton 4}]
let loop=TurboNativeLoops.planWithProofs proofs 140 -256 source |> Result.defaultWith failwith |> _.Loops |> List.exactlyOne
if loop.ArrayInvariants.Rows.Length<>3 then failwith "All three retained address registers must be exercised."
let cpu=Cpu(Array.zeroCreate<byte> 0x40000)
Array.blit loop.Code 0 cpu.Memory 0x1000 loop.Code.Length
for r in 0..7 do cpu.D[r]<-0xABCD0000u+uint32 r;cpu.A[r]<-0x2000u+uint32(r*16)
cpu.A[1]<-0x5000u;cpu.A[6]<-0x8000u;cpu.A[7]<-0xF000u;cpu.Pc<-0x1000u
cpu.WriteLong(0x7F04u,0x6020u);cpu.WriteLong(0x7F08u,0x6040u)
cpu.WriteWord(0x6026u,1u);cpu.WriteWord(0x6028u,3u);cpu.WriteWord(0x6040u,32766u)
cpu.WriteLong(0x7F10u,0x7000u);cpu.WriteLong(0x7000u,0x10000u)
cpu.WriteWord(0x700Au,8u);cpu.WriteWord(0x700Cu,3u)
let savedD,savedA=Array.copy cpu.D,Array.copy cpu.A
let mutable multiplies=0
while cpu.Pc<>0xAD8u do
    if cpu.Steps>100000L then failwith "Expected overflow was lost."
    if cpu.ReadWord cpu.Pc &&& 0xF1C0u=0xC0C0u then multiplies<-multiplies+1
    cpu.Step()
if multiplies<>3 then failwithf "Each of three rows should initialize once, got %d MULUs." multiplies
for r in 2..7 do if cpu.D[r]<>savedD[r] then failwithf "D%d not restored" r
for r in [0;3;4;5;6;7] do if cpu.A[r]<>savedA[r] then failwithf "A%d not restored" r
if cpu.A[1]<>0x4FFEu || cpu.ReadWord cpu.A[1]<>32767u then failwith "Original overflow left operand was not recovered."
if cpu.ReadWord 0x6020u<>1u || cpu.ReadWord 0x6040u<>32767u then failwith "Counter/committed scalar stores changed."
for row in 0..2 do
    if cpu.ReadWord(uint32(0x10000+16*row+2))<>1u then failwith "Array store before overflow was not committed."
printfn "PASS: three row caches initialize once; overflow after reuse preserves scalar/array stores, counter, maths operand, CPU stack and D2-D7/A0/A3-A6."
