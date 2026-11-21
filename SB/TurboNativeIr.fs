module TurboNativeIr

// Leaf integer lowering after Turbo's improver. Values are immutable virtual
// words. There are no calls or hidden template clobbers in a block. Checked
// operations have explicit non-returning exits to the runtime overflow handler.
// D0/D1 and A2 are scratch; all other registers are preserved. The maths stack
// is canonical and the private CPU-stack spill frame is gone at every edge.
open System
open System.Collections.Generic
open System.Text
open HirTurboIrBackend

type Value = Value of int
type BinaryOperation = AddWord | SubtractWord | MultiplyWord
type DivisionOperation = DivideWord | ModuloWord
type Instruction =
    | Constant of Value * int
    | LoadVector of Value * int
    | PopMaths of Value
    | Binary of Value * BinaryOperation * Value * Value
    // DIV/MOD atomically check the divisor and quotient, then apply QL signed
    // rounding. The failure snapshot excludes the consumed right maths word.
    | Divide of Value * DivisionOperation * Value * Value * stack:Value list
    | Negate of Value * Value
    // Snapshot in bottom-to-top maths-stack order at the failing P template.
    // A checked increment also commits its wrapped destination before trapping.
    | OverflowCheck of product:Value option * stack:Value list * store:(int * Value) option
    | StoreVector of int * Value
    | PushMaths of Value

type Block = {
    StartOffset:int
    EndOffset:int
    Instructions:Instruction list
    LegacyBytes:int
    LegacyDataWords:int
}

type RecoveryLocation = RecoveryRegister of int | RecoverySlot of int
type AllocatedInstruction =
    | Immediate of register:int * value:int
    | Load of register:int * vector:int
    | Pop of register:int
    | Copy of destination:int * source:int
    | Spill of register:int * slot:int
    | Reload of register:int * slot:int
    | Arithmetic of BinaryOperation * destination:int * source:int
    // Both operand registers are scratch on success; before either failure,
    // their original low words remain available to the recovery snapshot.
    | Division of DivisionOperation * destination:int * divisor:int * stack:RecoveryLocation list
    | Negative of register:int
    | Check of productRegister:int option * stack:RecoveryLocation list * store:(int * RecoveryLocation) option
    | Store of vector:int * register:int
    | Push of register:int

type Allocation = { Instructions:AllocatedInstruction list; SpillSlots:int; Registers:Set<int> }
type CompiledBlock = { Ir:Block; Allocation:Allocation; Code:byte[] }
type Report = { Blocks:CompiledBlock list; RejectedBlocks:int; StackExtraBytes:int }

let emptyReport = {Blocks=[];RejectedBlocks=0;StackExtraBytes=0}

let supports = function
    | HirTurboIrBackend.Instruction((1|2|5|6|7|8|20|1001|1005|1006|1007|263|268),[],_) -> true
    | HirTurboIrBackend.Instruction((106|109|118),[Word _],_) -> true
    | HirTurboIrBackend.Instruction((337|338|339|340|1339|1340),[Word _;Word _],_) -> true
    | _ -> false

let private legacyCost = function
    | HirTurboIrBackend.Instruction((1|268),_,_) -> 2,2
    | HirTurboIrBackend.Instruction(2,_,_) -> 0,0
    | HirTurboIrBackend.Instruction((5|6|7),_,_) -> 8,3
    | HirTurboIrBackend.Instruction(1001,_,_) -> 8,2
    | HirTurboIrBackend.Instruction((1005|1006),_,_) -> 14,3
    | HirTurboIrBackend.Instruction(1007,_,_) -> 28,3
    | HirTurboIrBackend.Instruction(8,_,_) -> 30,4
    | HirTurboIrBackend.Instruction(20,_,_) -> 32,3
    | HirTurboIrBackend.Instruction(106,_,_) -> 4,1
    | HirTurboIrBackend.Instruction((109|118),_,_) -> 6,4
    | HirTurboIrBackend.Instruction(263,_,_) -> 2,1
    | HirTurboIrBackend.Instruction((337|338),_,_) -> 6,4
    | HirTurboIrBackend.Instruction((339|340),_,_) -> 8,4
    | HirTurboIrBackend.Instruction((1339|1340),_,_) -> 14,4
    | _ -> invalidArg "items" "An opaque instruction cannot be lowered into a leaf block."

/// Lower a straight-line, native-mode sequence. A store invalidates every cached
/// variable load, then records only its own value: two different vectors may
/// point to the same cell through flexible/reference parameter bindings.
let lower startOffset endOffset items =
    let code=ResizeArray<Instruction>()
    let variables=Dictionary<int,Value>()
    let constants=Dictionary<int,Value>()
    let mutable next=0
    let fresh () = let value=Value next in next<-next+1; value
    let mutable stack=[]
    let push value = stack<-value::stack
    let pop () =
        match stack with
        | value::rest -> stack<-rest; value
        | [] -> let value=fresh() in code.Add(PopMaths value); value
    let constant raw =
        let word=raw &&& 0xFFFF
        match constants.TryGetValue word with
        | true,value -> value
        | _ -> let value=fresh() in code.Add(Constant(value,int(int16(uint16 word)))); constants[word]<-value; value
    let load vector =
        match variables.TryGetValue vector with
        | true,value -> value
        | _ -> let value=fresh() in code.Add(LoadVector(value,vector));variables[vector]<-value;value
    let store vector value =
        code.Add(StoreVector(vector,value))
        variables.Clear()
        variables[vector]<-value
    for item in items do
        match item with
        | HirTurboIrBackend.Instruction(2,[],_) -> ()
        | HirTurboIrBackend.Instruction(106,[Word n],_) -> push(constant n)
        | HirTurboIrBackend.Instruction(263,[],_) -> push(constant 0)
        | HirTurboIrBackend.Instruction(109,[Word v],_) -> push(load v)
        | HirTurboIrBackend.Instruction(118,[Word v],_) -> store v (pop())
        | HirTurboIrBackend.Instruction(268,[],_) -> let value=pop() in push value;push value
        | HirTurboIrBackend.Instruction((1|1001 as op),[],_) ->
            let source=pop()
            let value=fresh()
            code.Add(Negate(value,source))
            // NEG.W of MIN leaves MIN on the P maths stack on failure.
            if op=1001 then code.Add(OverflowCheck(None,List.rev(source::stack),None))
            push value
        | HirTurboIrBackend.Instruction((5|6|7|1005|1006|1007 as token),[],_) ->
            let op=token%1000
            let right,left=pop(),pop()
            let value=fresh()
            code.Add(Binary(value,(if op=5 then AddWord elif op=6 then SubtractWord else MultiplyWord),left,right))
            // The checked P binary template consumes the right operand but
            // leaves the original left word untouched if it overflows.
            if token>=1000 then code.Add(OverflowCheck((if op=7 then Some value else None),List.rev(left::stack),None))
            push value
        | HirTurboIrBackend.Instruction((337|338|339|340|1339|1340 as token),[Word vector;Word amount],_) ->
            let op=token%1000
            let source=load vector
            let value=fresh()
            code.Add(Binary(value,(if op=337 || op=339 then AddWord else SubtractWord),source,constant amount))
            if token>=1000 then code.Add(OverflowCheck(None,List.rev stack,Some(vector,value)))
            store vector value
        | HirTurboIrBackend.Instruction((8|20 as op),[],_) ->
            let right,left=pop(),pop()
            let value=fresh()
            code.Add(Divide(value,(if op=8 then DivideWord else ModuloWord),left,right,List.rev(left::stack)))
            push value
        | _ -> invalidArg "items" "Only audited leaf integer operations are supported."
    for value in List.rev stack do code.Add(PushMaths value)
    let bytes,words=items |> List.map legacyCost |> List.fold (fun (bytes,words) (b,w) -> bytes+b,words+w) (0,0)
    {StartOffset=startOffset;EndOffset=endOffset;Instructions=List.ofSeq code;LegacyBytes=bytes;LegacyDataWords=words}

let private sources = function
    | Binary(_,_,a,b) -> [a;b]
    | Divide(_,_,a,b,stack) -> a::b::stack
    | Negate(_,a) | StoreVector(_,a) | PushMaths a -> [a]
    | OverflowCheck(product,stack,store) -> (Option.toList product)@stack@(store |> Option.map (snd >> List.singleton) |> Option.defaultValue [])
    | _ -> []

let private destination = function
    | Constant(v,_) | LoadVector(v,_) | PopMaths v | Binary(v,_,_,_) | Divide(v,_,_,_,_) | Negate(v,_) -> Some v
    | _ -> None

type private Location = Register of int | Slot of int
exception private AllocationLimit

/// Next-use allocation in D0/D1, with two-address destination reuse. Immutable
/// values retain valid spill copies across reloads. Slots are reused after the
/// last use; the frame is bounded and never survives a template or call.
let allocate maxFrameBytes (block:Block) : Allocation option =
    let code=ResizeArray<AllocatedInstruction>()
    let locations=Dictionary<Value,Location>()
    let owners=Dictionary<int,Value>()
    let backing=Dictionary<Value,int>()
    let uses=Dictionary<Value,Queue<int>>()
    let last=Dictionary<Value,int>()
    let freeSlots=SortedSet<int>()
    let mutable slots=0
    let mutable used=Set.empty
    let instructions=List.toArray block.Instructions
    for index in 0..instructions.Length-1 do
        for value in sources instructions[index] do
            if not (uses.ContainsKey value) then uses[value]<-Queue<int>()
            uses[value].Enqueue index
            last[value]<-index
    let live value index = last.ContainsKey value && last[value]>index
    let save value register =
        match backing.TryGetValue value with
        | true,slot -> slot
        | _ ->
            let slot=
                if freeSlots.Count>0 then let slot=freeSlots.Min in freeSlots.Remove slot |> ignore;slot
                else
                    let slot=slots
                    slots<-slots+1
                    if slots*2>maxFrameBytes then raise AllocationLimit
                    slot
            code.Add(Spill(register,slot))
            backing[value]<-slot
            slot
    let evict register =
        match owners.TryGetValue register with
        | true,value ->
            if uses.ContainsKey value && uses[value].Count>0 then locations[value]<-Slot(save value register)
            else locations.Remove value |> ignore
            owners.Remove register |> ignore
        | _ -> ()
    let choose pinned =
        let available=[0;1] |> List.filter (fun r -> not(Set.contains r pinned))
        let register =
            match available |> List.tryFind (fun r -> not(owners.ContainsKey r)) with
            | Some r -> r
            | None -> available |> List.maxBy (fun r ->
                let value=owners[r]
                if uses.ContainsKey value && uses[value].Count>0 then uses[value].Peek() else Int32.MaxValue)
        evict register
        used<-used.Add register
        register
    let ensure value pinned =
        match locations.TryGetValue value with
        | true,Register register -> register
        | true,Slot slot ->
            let register=choose pinned
            code.Add(Reload(register,slot))
            owners[register]<-value;locations[value]<-Register register
            register
        | _ -> invalidArg "block" "A virtual value was used before its definition."
    let define value =
        let register=choose Set.empty
        owners[register]<-value;locations[value]<-Register register
        register
    let overwrite index old value register =
        if live old index then locations[old]<-Slot(save old register)
        else locations.Remove old |> ignore
        owners[register]<-value;locations[value]<-Register register
    let retire value index =
        if uses.ContainsKey value then
            while uses[value].Count>0 && uses[value].Peek()=index do uses[value].Dequeue() |> ignore
        if not(live value index) then
            match locations.TryGetValue value with
            | true,Register r when owners.ContainsKey r && owners[r]=value -> owners.Remove r |> ignore
            | _ -> ()
            locations.Remove value |> ignore
            match backing.TryGetValue value with
            | true,slot -> backing.Remove value |> ignore;freeSlots.Add slot |> ignore
            | _ -> ()
    try
        for index in 0..instructions.Length-1 do
            let instruction=instructions[index]
            match instruction with
            | Constant(value,n) -> code.Add(Immediate(define value,n))
            | LoadVector(value,vector) -> code.Add(Load(define value,vector))
            | PopMaths value -> code.Add(Pop(define value))
            | StoreVector(vector,value) -> code.Add(Store(vector,ensure value Set.empty))
            | PushMaths value -> code.Add(Push(ensure value Set.empty))
            | OverflowCheck(product,stack,store) ->
                // No reloads or spills may be inserted between arithmetic and
                // its check: MOVE would destroy V. Recovery reads the current
                // physical locations only on the non-returning failure edge.
                let recover value =
                    match locations[value] with
                    | Register r -> RecoveryRegister r
                    | Slot slot -> RecoverySlot slot
                let productRegister=product |> Option.map (fun value ->
                    match locations[value] with Register r -> r | _ -> invalidArg "block" "Product check must immediately follow its definition.")
                code.Add(Check(productRegister,List.map recover stack,store |> Option.map(fun (vector,value) -> vector,recover value)))
            | Negate(value,source) ->
                let register=ensure source Set.empty
                if live source index && owners.Count<2 then
                    let result=choose(Set.singleton register)
                    code.Add(Copy(result,register))
                    owners[result]<-value;locations[value]<-Register result
                    code.Add(Negative result)
                else
                    overwrite index source value register
                    code.Add(Negative register)
            | Binary(value,op,left,right) ->
                let lhs=ensure left Set.empty
                let rhs=ensure right (Set.singleton lhs)
                if live left index && left<>right && not(live right index) && op<>SubtractWord then
                    overwrite index right value rhs
                    code.Add(Arithmetic(op,rhs,lhs))
                elif live left index && left=right && owners.Count<2 then
                    let result=choose(Set.singleton lhs)
                    code.Add(Copy(result,lhs))
                    owners[result]<-value;locations[value]<-Register result
                    code.Add(Arithmetic(op,result,lhs))
                else
                    overwrite index left value lhs
                    code.Add(Arithmetic(op,lhs,rhs))
            | Divide(value,op,left,right,stack) ->
                let lhs=ensure left Set.empty
                let source=ensure right (Set.singleton lhs)
                let rhs=
                    if source<>lhs then source
                    else
                        let copy=choose(Set.singleton lhs)
                        code.Add(Copy(copy,lhs))
                        copy
                // DIVS and QL correction together may overwrite both operands.
                // Preserve every value needed after the atomic operation before
                // emitting any flag-sensitive instruction. Same-value operands
                // still require distinct physical registers.
                for r in [lhs;rhs] do
                    match owners.TryGetValue r with
                    | true,old when live old index -> locations[old]<-Slot(save old r)
                    | _ -> ()
                let snapshot=stack |> List.map(fun v ->
                    match locations[v] with Register r -> RecoveryRegister r | Slot slot -> RecoverySlot slot)
                for r in [lhs;rhs] do
                    match owners.TryGetValue r with
                    | true,old ->
                        if not(live old index) then locations.Remove old |> ignore
                        owners.Remove r |> ignore
                    | _ -> ()
                owners[lhs]<-value;locations[value]<-Register lhs
                code.Add(Division(op,lhs,rhs,snapshot))
            for value in sources instruction |> List.distinct do retire value index
            match destination instruction with
            | Some value when not(last.ContainsKey value) -> retire value index
            | _ -> ()
        Some {Instructions=List.ofSeq code;SpillSlots=slots;Registers=used}
    with AllocationLimit -> None

let private recoveryBytes frame stack store =
    let size = function RecoveryRegister _ -> 2 | RecoverySlot _ -> 4
    (stack |> List.sumBy size)+(if frame>0 then 4 else 0)+4+
    (store |> Option.map(fun (_,location) -> 4+size location) |> Option.defaultValue 0)

/// Only word operations and signed A6-relative vector displacements are emitted.
/// LEA reserves/releases the spill frame without changing the condition codes.
let emitWithContext vectorTableOffset (vectorRegisters:Map<int,int>) (recoverySuffix:byte[]) allocation =
    let bytes=ResizeArray<byte>()
    let word n = bytes.Add(byte(n >>> 8));bytes.Add(byte n)
    let displacement vector = word(vectorTableOffset+4*vector)
    let frame=allocation.SpillSlots*2
    let recover = function
        | RecoveryRegister r -> word(0x3300 ||| r)
        | RecoverySlot slot -> word 0x332F;word(slot*2)
    let recovery stack store =
        match store with
        | Some(vector,RecoveryRegister r) -> word 0x246E;displacement vector;word(0x3480 ||| r)
        | Some(vector,RecoverySlot slot) -> word 0x246E;displacement vector;word 0x34AF;word(slot*2)
        | None -> ()
        for location in stack do recover location
        if frame>0 then word 0x4FEF;word frame
        bytes.AddRange recoverySuffix
        word 0x4EEE;word 0x8AD8 // JMP overflow(A6), never returns
    if frame>0 then word 0x4FEF;word(-frame)
    for instruction in allocation.Instructions do
        match instruction with
        | Immediate(r,n) when n>= -128 && n<=127 -> word(0x7000 ||| (r <<< 9) ||| (n &&& 255))
        | Immediate(r,n) -> word(0x303C ||| (r <<< 9));word n
        | Load(r,vector) ->
            match vectorRegisters.TryFind vector with
            | Some source -> word(0x3000 ||| (r <<< 9) ||| source)
            | None -> word 0x246E;displacement vector;word(0x3012 ||| (r <<< 9))
        | Store(vector,r) -> word 0x246E;displacement vector;word(0x3480 ||| r)
        | Pop r -> word(0x3019 ||| (r <<< 9))
        | Push r -> word(0x3300 ||| r)
        | Copy(dst,src) -> word(0x3000 ||| (dst <<< 9) ||| src)
        | Spill(r,slot) -> word(0x3F40 ||| r);word(slot*2)
        | Reload(r,slot) -> word(0x302F ||| (r <<< 9));word(slot*2)
        | Negative r -> word(0x4440 ||| r)
        | Arithmetic(op,dst,src) -> word((match op with AddWord -> 0xD040 | SubtractWord -> 0x9040 | MultiplyWord -> 0xC1C0) ||| (dst <<< 9) ||| src)
        | Division(op,dst,src,stack) ->
            let failureBytes=recoveryBytes frame stack None + recoverySuffix.Length
            if failureBytes+10>32767 then invalidArg "allocation" "Division recovery exceeds a 68000 word branch."
            word(0x4A40 ||| src) // TST.W divisor, before DIVS can raise a CPU trap
            word 0x6600;word(failureBytes+2) // BNE.W divide
            let failure=bytes.Count
            recovery stack None
            word(0x48C0 ||| dst) // EXT.L dividend
            word(0x81C0 ||| (dst <<< 9) ||| src) // DIVS.W divisor,destination
            word 0x6900;word(failure-bytes.Count) // BVS.W failure; low dividend is unchanged
            word(0x4840 ||| dst) // SWAP: remainder in low word
            word(0x4A40 ||| dst)
            // Remainder zero needs no signed correction. Otherwise EOR tests
            // whether remainder and divisor have different signs, as lib537.
            let xor=0xB140 ||| (dst <<< 9) ||| src
            match op with
            | DivideWord ->
                word 0x670A;word xor;word 0x6A06
                word(0x4840 ||| dst);word(0x5340 ||| dst);word 0x6002
                word(0x4840 ||| dst) // retain quotient, rounded down
            | ModuloWord ->
                word 0x6708;word xor;word 0x6A04
                word xor // restore divisor before adding it to the remainder
                word(0xD040 ||| (dst <<< 9) ||| src)
        | Check(product,stack,store) ->
            let failureBytes=recoveryBytes frame stack store + recoverySuffix.Length
            if failureBytes+2>32767 then invalidArg "allocation" "Overflow recovery exceeds a 68000 word branch."
            match product with
            | None -> word 0x6800;word(failureBytes+2) // BVC.W success
            | Some r ->
                // MULS.W produces a long result; V alone cannot detect word
                // overflow. These comparisons preserve both the product and
                // every other live register used by recovery.
                word(0x0C80 ||| r);word 0;word 0x7FFF
                word 0x6E0A // BGT.S failure (past the lower-bound check)
                word(0x0C80 ||| r);word 0xFFFF;word 0x8000
                word 0x6C00;word(failureBytes+2) // BGE.W success
            recovery stack store
    if frame>0 then word 0x4FEF;word frame
    bytes.ToArray()

let emit vectorTableOffset allocation = emitWithContext vectorTableOffset Map.empty [||] allocation

let private dataWords allocation =
    allocation.Instructions |> List.sumBy (function Load _ | Store _ -> 3 | Spill _ | Reload _ | Pop _ | Push _ -> 1 | _ -> 0)

/// Source offsets are collected AFTER the legacy improver, whose fixed-size
/// rewrites can introduce increments and duplicates. Both layout passes consume
/// the same immutable plan. Labels, mode changes and opaque templates split it.
let plan maxFrameBytes vectorTableOffset bytes : Result<Report,string> =
    match decode bytes with
    | Error message -> Error message
    | Ok items ->
        let blocks=ResizeArray<CompiledBlock>()
        let pending=ResizeArray<IrItem>()
        let mutable start=0
        let mutable offset=0
        let mutable native=true
        let mutable rejected=0
        let flush () =
            if pending.Count>0 then
                let ir=lower start offset (List.ofSeq pending)
                match allocate maxFrameBytes ir with
                | Some allocation ->
                    let code=emit vectorTableOffset allocation
                    let coldBytes=allocation.Instructions |> List.sumBy(function
                        | Check(_,stack,store) -> recoveryBytes (allocation.SpillSlots*2) stack store
                        | Division(_,_,_,stack) -> recoveryBytes (allocation.SpillSlots*2) stack None
                        | _ -> 0)
                    // A bus-word estimate includes instruction fetches and data
                    // traffic. It is not a calibrated MC68008 timing model.
                    // Failure recovery is cold. Charge every success-path check
                    // and spill, but not the non-returning error payload.
                    if (code.Length-coldBytes)/2+dataWords allocation < ir.LegacyBytes/2+ir.LegacyDataWords then
                        blocks.Add {Ir=ir;Allocation=allocation;Code=code}
                    else rejected<-rejected+1
                | None -> rejected<-rejected+1
                pending.Clear()
        for item in items do
            if native && supports item then
                // Bound recovery branch distances even for enormous generated
                // expressions. Materializing at this boundary preserves the
                // stack protocol and leaves the next block independently safe.
                if pending.Count>=256 then flush()
                if pending.Count=0 then start<-offset
                pending.Add item
            else flush()
            offset<-offset+(serialize [item]).Length
            match item with
            | HirTurboIrBackend.Instruction(254,_,_) -> native<-true
            | HirTurboIrBackend.Instruction(255,_,_) -> native<-false
            | _ -> ()
        flush()
        let blocks=List.ofSeq blocks
        Ok {Blocks=blocks;RejectedBlocks=rejected;StackExtraBytes=blocks |> List.map (fun b -> b.Allocation.SpillSlots*2) |> List.fold max 0}

let format report =
    let text=StringBuilder()
    let line (s:string) = text.AppendLine s |> ignore
    line $"Native register IR: blocks={report.Blocks.Length}; rejected={report.RejectedBlocks}; extra-stack={report.StackExtraBytes} bytes"
    line "Scratch registers: D0,D1,A2. All values are materialized and frames released at every boundary."
    for block in report.Blocks do
        line $"Block IR bytes {block.Ir.StartOffset}..{block.Ir.EndOffset}: native bytes {block.Ir.LegacyBytes}->{block.Code.Length}; spill slots={block.Allocation.SpillSlots}"
        for instruction in block.Ir.Instructions do line $"  virtual: {instruction}"
        for instruction in block.Allocation.Instructions do line $"  allocated: {instruction}"
    text.ToString()
