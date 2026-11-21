module TurboNativeArrays

// Compact checked numeric array access. A call site reloads the current vector
// and calls one shared operation wrapper. Indexing consumes the last subscript
// first, exactly as lib537's P templates do. Arrays remain allocation barriers.
open System.Collections.Generic
open HirTurboIrBackend

type Pool = { Code:byte[]; Addresses:Map<int,int> }

let isUnchecked = function 1611 | 1612 | 1643 | 1644 -> true | _ -> false
let isCompact = function 1111 | 1112 | 1143 | 1144 | 1611 | 1612 | 1643 | 1644 -> true | _ -> false
let baseOpcode op = if isUnchecked op then op-1500 else op%1000

let required bytes =
    decode bytes |> Result.map(fun items ->
        let mutable native=true
        let mutable operations=Set.empty
        for item in items do
            match item with
            | Instruction(254,_,_) -> native<-true
            | Instruction(255,_,_) -> native<-false
            | Instruction(op,_,_) when native && isCompact op -> operations<-operations.Add op
            | _ -> ()
        operations)

let private bytes words = words |> Seq.collect(fun n -> [byte(n >>> 8);byte n]) |> Seq.toArray

let private indexer checkedBounds =
    let words=ResizeArray<int>()
    let labels=Dictionary<string,int>()
    let fixups=ResizeArray<int * string * bool>()
    let emit values = words.AddRange values
    let label name = labels[name]<-words.Count*2
    let short opcode name = fixups.Add(words.Count,name,true);emit [opcode]
    let branch opcode name = fixups.Add(words.Count,name,false);emit [opcode;0]
    // A2 is the current descriptor. Layout: data pointer, byte length,
    // rank-minus-one, then the dimension lengths in reverse source order.
    emit [0x205A;0x588A;0x321A;0x7400] // MOVEA.L (A2)+,A0; ADDQ.L #4,A2; MOVE.W (A2)+,D1; MOVEQ #0,D2
    emit [0x3419] // MOVE.W (A1)+,D2: first index, zero extended without MULU by one
    if checkedBounds then
        short 0x6B00 "range"
        emit [0xB452] // CMP.W (A2),D2
        short 0x6B00 "first-ok"
        label "range"
        // Both wrapper and indexer were called with JSR. No return frame or
        // local allocator spill survives into the runtime error handler.
        emit [0x4FEF;8;0x4EEE;0x8AE0]
        label "first-ok"
    emit [0x7600;0x361A] // MOVEQ #0,D3; MOVE.W (A2)+,D3: first stride, including a length of 32768
    branch 0x51C9 "next" // DBRA D1,next; a one-dimensional array returns immediately
    emit [0x4E75]
    label "next"
    emit [0x3019] // MOVE.W (A1)+,D0
    if checkedBounds then
        short 0x6B00 "range"
        emit [0xB052] // CMP.W (A2),D0
        short 0x6B00 "bounds-ok"
        short 0x6000 "range"
        label "bounds-ok"
    // Preserve lib537's unsigned word-stride multiplication and long offset.
    emit [0xC0C3;0xC6DA;0xD480] // MULU.W D3,D0; MULU.W (A2)+,D3; ADD.L D0,D2
    branch 0x51C9 "next"
    emit [0x4E75]
    for index,target,isShort in fixups do
        let displacement=labels[target]-(index+1)*2
        if isShort then
            if displacement=0 || displacement< -128 || displacement>127 then invalidOp "Array helper short branch is out of range."
            words[index]<-words[index] ||| (displacement &&& 255)
        else words[index+1]<-displacement
    bytes words

let build startAddress operations =
    if operations |> Set.exists (isCompact >> not) then invalidArg "operations" "Only checked numeric array operations have compact helpers."
    if Set.isEmpty operations then {Code=[||];Addresses=Map.empty}
    else
        let output=ResizeArray<byte>()
        let mutable cores=Map.empty
        for unchecked in operations |> Set.toList |> List.map isUnchecked |> List.distinct |> List.sort do
            cores<-cores.Add(unchecked,(startAddress+output.Count) &&& 0xFFFF)
            output.AddRange(indexer(not unchecked))
        let mutable addresses=Map.empty
        let intScale=[0xD482;0xD1C2] // ADD.L D2,D2; ADDA.L D2,A0
        let floatScale=[0xD482;0x2202;0xD281;0xD481;0xD1C2]
        for op in operations do
            addresses<-addresses.Add(op,(startAddress+output.Count) &&& 0xFFFF)
            let core=[0x4EAE;cores[isUnchecked op]]
            let body=
                match baseOpcode op with
                | 111 -> core@intScale@[0x3310;0x4E75]
                | 112 -> [0x3819]@core@intScale@[0x3084;0x4E75]
                | 143 -> core@floatScale@[0x2328;2;0x3310;0x4E75]
                | 144 -> [0x3819;0x2A19]@core@floatScale@[0x30C4;0x2085;0x4E75]
                | _ -> invalidOp "Unsupported compact array helper."
            output.AddRange(bytes body)
        {Code=output.ToArray();Addresses=addresses}

let call vectorTableOffset vector helperAddress =
    // MOVEA.L vector(A6),A2; JSR helper(A6). Addresses are never cached across
    // DIM, routine binding changes, aliases, or any other access.
    bytes [0x246E;vectorTableOffset+4*vector;0x4EAE;helperAddress]
