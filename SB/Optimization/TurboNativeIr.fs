module TurboNativeIr

// Leaf integer lowering after Turbo's improver. Values are immutable virtual
// words. Whole-loop comparison leaves additionally materialize QL float booleans.
// There are no calls or hidden template clobbers in a block. Checked
// operations have explicit non-returning exits to the runtime error handlers.
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
    | LoadArrayWord of Value * vector:int * checkedBounds:bool * index:Value * stack:Value list
    | StoreArrayWord of vector:int * checkedBounds:bool * index:Value * value:Value * stack:Value list
    | LoadArrayWord2 of Value * vector:int * checks:int * columns:int * row:Value * column:Value * stack:Value list
    | StoreArrayWord2 of vector:int * checks:int * columns:int * row:Value * column:Value * value:Value * stack:Value list
    | Negate of Value * Value
    | CompareWord of Value * opcode:int * Value * Value
    // Snapshot in bottom-to-top maths-stack order at the failing P template.
    // A checked increment also commits its wrapped destination before trapping.
    | OverflowCheck of product:Value option * stack:Value list * store:(int * Value) option
    | StoreVector of int * Value
    | PushMaths of Value
    | PushBooleanMaths of Value

type Block = {
    StartOffset:int
    EndOffset:int
    Instructions:Instruction list
    LegacyBytes:int
    LegacyDataWords:int
}

type RecoveryLocation = RecoveryRegister of int | RecoverySlot of int | RecoveryImmediate of int
type AllocatedInstruction =
    | Immediate of register:int * value:int
    | Load of register:int * vector:int
    | Pop of register:int
    | Copy of destination:int * source:int
    | Spill of register:int * slot:int
    | Reload of register:int * slot:int
    | Arithmetic of BinaryOperation * destination:int * source:int
    | ArithmeticImmediate of BinaryOperation * destination:int * value:int
    // Both operand registers are scratch on success; before either failure,
    // their original low words remain available to the recovery snapshot.
    | Division of DivisionOperation * destination:int * divisor:int * stack:RecoveryLocation list
    // A positive literal 2^shift can neither divide by zero nor overflow a word
    // quotient. ASR and AND implement QL floor DIV and nonnegative MOD exactly.
    | PowerOfTwoDivision of DivisionOperation * register:int * shift:int
    // A positive literal divisor cannot fail for a signed-word dividend. The
    // hardware quotient/remainder need only the QL negative-remainder correction.
    | PositiveDivision of DivisionOperation * register:int * divisor:int
    // Rank is certified by lowering. Addressing preserves the index; only a
    // load's result is clobbered. A2 reloads the descriptor/data each operation.
    | LoadElement of vector:int * checkedBounds:bool * index:int * result:int * stack:RecoveryLocation list
    | StoreElement of vector:int * checkedBounds:bool * index:int * value:RecoveryLocation * stack:RecoveryLocation list
    // The accumulator is scratch; inputs/recovery locations never refer to it.
    // A long MULU/ADDA address is internal, not a word-valued BASIC expression.
    | LoadElement2 of vector:int * checks:int * columns:int * row:RecoveryLocation * column:RecoveryLocation * accumulator:int * stack:RecoveryLocation list
    | StoreElement2 of vector:int * checks:int * columns:int * row:RecoveryLocation * column:RecoveryLocation * value:RecoveryLocation * accumulator:int * stack:RecoveryLocation list
    | Negative of register:int
    | Comparison of opcode:int * destination:int * right:RecoveryLocation
    // Final comparison consumed by an adjacent native branch. The leaf frame
    // is released with LEA, which preserves these signed comparison flags.
    | CompareFlags of destination:int * right:RecoveryLocation
    | Check of productRegister:int option * stack:RecoveryLocation list * store:(int * RecoveryLocation) option
    | Store of vector:int * value:RecoveryLocation
    | Push of register:int
    | PushBoolean of register:int

type Allocation = { Instructions:AllocatedInstruction list; SpillSlots:int; Registers:Set<int> }
// Whole-loop planner owns these registers and proves descriptor/row stability.
// A validity bit is set only after the original access's checks have succeeded.
type ArrayRowCache = { AddressRegister:int; ValidBit:int }
type CompiledBlock = { Ir:Block; Allocation:Allocation; Code:byte[] }
type Report = { Blocks:CompiledBlock list; RejectedBlocks:int; StackExtraBytes:int }

let emptyReport = {Blocks=[];RejectedBlocks=0;StackExtraBytes=0}

let supports = function
    | HirTurboIrBackend.Instruction((1|2|5|6|7|8|20|1001|1005|1006|1007|263|268),[],_) -> true
    | HirTurboIrBackend.Instruction((106|109|118),[Word _],_) -> true
    | HirTurboIrBackend.Instruction((1711|1712|1811|1812),[Word _],_) -> true
    | HirTurboIrBackend.Instruction((1911|1912),[Word _;Word mask;Word columns],_) when mask>=0 && mask<=3 && columns>=0 && columns<=32768 -> true
    | HirTurboIrBackend.Instruction((337|338|339|340|1339|1340),[Word _;Word _],_) -> true
    | _ -> false

let private legacyCost = function
    | HirTurboIrBackend.Instruction((9|10|14|15),_,_) -> 24,8
    | HirTurboIrBackend.Instruction((11|12|13),_,_) -> 22,8
    | HirTurboIrBackend.Instruction((1|268),_,_) -> 2,2
    | HirTurboIrBackend.Instruction(2,_,_) -> 0,0
    | HirTurboIrBackend.Instruction((5|6|7),_,_) -> 8,3
    | HirTurboIrBackend.Instruction(1001,_,_) -> 8,2
    | HirTurboIrBackend.Instruction((1005|1006),_,_) -> 14,3
    | HirTurboIrBackend.Instruction(1007,_,_) -> 28,3
    | HirTurboIrBackend.Instruction(8,_,_) -> 30,4
    | HirTurboIrBackend.Instruction(20,_,_) -> 32,3
    // Call site plus the instructions actually fetched through a one-dimensional
    // compact helper. Shared helper bytes are not replicated in the executable.
    | HirTurboIrBackend.Instruction((1711|1712),_,_) -> 46,18
    | HirTurboIrBackend.Instruction((1811|1812),_,_) -> 40,17
    | HirTurboIrBackend.Instruction((1911|1912),[_;Word 0;_],_) -> 56,25
    | HirTurboIrBackend.Instruction((1911|1912),_,_) -> 72,29
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
let private lowerCore booleanResult startOffset endOffset items =
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
        | HirTurboIrBackend.Instruction((1711|1811 as op),[Word vector],_) ->
            let index=pop()
            let value=fresh()
            code.Add(LoadArrayWord(value,vector,(op=1711),index,(if op=1711 then List.rev stack else [])))
            push value
        | HirTurboIrBackend.Instruction((1712|1812 as op),[Word vector],_) ->
            // P stores consume the value before checking/consuming the index.
            let value,index=pop(),pop()
            code.Add(StoreArrayWord(vector,(op=1712),index,value,(if op=1712 then List.rev stack else [])))
            // A scalar parameter may point at an element of this same array.
            // Immutable pending values survive, but scalar loads must be fresh.
            variables.Clear()
        | HirTurboIrBackend.Instruction(1911,[Word vector;Word mask;Word columns],_) ->
            let column,row=pop(),pop()
            let value=fresh()
            code.Add(LoadArrayWord2(value,vector,mask,columns,row,column,(if mask<>0 then List.rev stack else [])))
            push value
        | HirTurboIrBackend.Instruction(1912,[Word vector;Word mask;Word columns],_) ->
            let value,column,row=pop(),pop(),pop()
            code.Add(StoreArrayWord2(vector,mask,columns,row,column,value,(if mask<>0 then List.rev stack else [])))
            variables.Clear()
        | HirTurboIrBackend.Instruction((9|10|11|12|13|14|15 as op),[],_) when booleanResult ->
            let right,left=pop(),pop()
            let value=fresh()
            code.Add(CompareWord(value,op,left,right))
            push value
        | _ -> invalidArg "items" "Only audited leaf integer operations are supported."
    for index,value in List.rev stack |> List.indexed do
        code.Add(if booleanResult && index=stack.Length-1 then PushBooleanMaths value else PushMaths value)
    let bytes,words=items |> List.map legacyCost |> List.fold (fun (bytes,words) (b,w) -> bytes+b,words+w) (0,0)
    {StartOffset=startOffset;EndOffset=endOffset;Instructions=List.ofSeq code;LegacyBytes=bytes;LegacyDataWords=words}

let lower startOffset endOffset items = lowerCore false startOffset endOffset items
/// Only the whole-loop typed CFG uses this: its final integer comparison must
/// materialize the exact six-byte QL boolean before any following operation.
let lowerComparison startOffset endOffset items = lowerCore true startOffset endOffset items

let comparisonCondition = function
    | 9 -> 12 | 10 -> 14 | 11|12 -> 7 | 13 -> 6 | 14 -> 15 | 15 -> 13
    | _ -> invalidArg "op" "Unsupported integer comparison."

let private sources = function
    | Binary(_,_,a,b) -> [a;b]
    | Divide(_,_,a,b,stack) -> a::b::stack
    | LoadArrayWord(_,_,_,index,stack) -> index::stack
    | StoreArrayWord(_,_,index,value,stack) -> index::value::stack
    | LoadArrayWord2(_,_,_,_,row,column,stack) -> row::column::stack
    | StoreArrayWord2(_,_,_,row,column,value,stack) -> row::column::value::stack
    | CompareWord(_,_,a,b) -> [a;b]
    | Negate(_,a) | StoreVector(_,a) | PushMaths a | PushBooleanMaths a -> [a]
    | OverflowCheck(product,stack,store) -> (Option.toList product)@stack@(store |> Option.map (snd >> List.singleton) |> Option.defaultValue [])
    | _ -> []

let private destination = function
    | Constant(v,_) | LoadVector(v,_) | PopMaths v | Binary(v,_,_,_) | Divide(v,_,_,_,_) | LoadArrayWord(v,_,_,_,_) | LoadArrayWord2(v,_,_,_,_,_,_) | Negate(v,_) | CompareWord(v,_,_,_) -> Some v
    | _ -> None

type private ExpressionKey =
    | BinaryKey of BinaryOperation * checkedArithmetic:bool * Value * Value
    | NegateKey of checkedArithmetic:bool * Value
    | DivideKey of DivisionOperation * Value * Value

/// Local value numbering over immutable words, before physical allocation.
/// Arithmetic and its following overflow check are one unit. Only successful
/// constant computations are folded; checked and unchecked expressions have
/// distinct identities. Reusing a checked result is safe because its earlier
/// successful check dominates this straight-line occurrence. Loads, stores,
/// maths inputs and array checks are never removed or reordered.
let optimize (block:Block) =
    let instructions=List.toArray block.Instructions
    // A bare/mismatched flag check is outside the lowered IR contract. Leave
    // such a block intact rather than fold an operation that could feed it.
    let paired arithmetic check =
        match arithmetic,check with
        | Binary(value,MultiplyWord,_,_),OverflowCheck(Some product,_,_) -> value=product
        | Binary(_,(AddWord|SubtractWord),_,_),OverflowCheck(None,_,_)
        | Negate(_,_),OverflowCheck(None,_,_) -> true
        | _ -> false
    let validChecks =
        instructions |> Array.mapi(fun i instruction ->
            match instruction with
            | OverflowCheck _ -> i>0 && paired instructions[i-1] instruction
            | _ -> true) |> Array.forall id
    if not validChecks then block
    else
        let code=ResizeArray<Instruction>()
        let aliases=Dictionary<Value,Value>()
        let constants=Dictionary<Value,int>()
        let constantValues=Dictionary<int,Value>()
        let expressions=Dictionary<ExpressionKey,Value>()
        let rec resolve value =
            match aliases.TryGetValue value with true,original -> resolve original | _ -> value
        let known value = match constants.TryGetValue(resolve value) with true,n -> Some n | _ -> None
        let constant value raw =
            let n=int(int16(uint16 raw))
            match constantValues.TryGetValue n with
            | true,original -> aliases[value]<-original
            | _ ->
                constants[value]<-n;constantValues[n]<-value
                code.Add(Constant(value,n))
        let rewrite = function
            | Binary(v,op,a,b) -> Binary(v,op,resolve a,resolve b)
            | Negate(v,a) -> Negate(v,resolve a)
            | Divide(v,op,a,b,stack) -> Divide(v,op,resolve a,resolve b,List.map resolve stack)
            | LoadArrayWord(v,vector,checkedBounds,index,stack) -> LoadArrayWord(v,vector,checkedBounds,resolve index,List.map resolve stack)
            | StoreArrayWord(vector,checkedBounds,index,value,stack) -> StoreArrayWord(vector,checkedBounds,resolve index,resolve value,List.map resolve stack)
            | LoadArrayWord2(v,vector,mask,columns,row,column,stack) -> LoadArrayWord2(v,vector,mask,columns,resolve row,resolve column,List.map resolve stack)
            | StoreArrayWord2(vector,mask,columns,row,column,value,stack) -> StoreArrayWord2(vector,mask,columns,resolve row,resolve column,resolve value,List.map resolve stack)
            | OverflowCheck(product,stack,store) -> OverflowCheck(Option.map resolve product,List.map resolve stack,store |> Option.map(fun (vector,value) -> vector,resolve value))
            | StoreVector(vector,value) -> StoreVector(vector,resolve value)
            | PushMaths value -> PushMaths(resolve value)
            | PushBooleanMaths value -> PushBooleanMaths(resolve value)
            | CompareWord(v,op,a,b) -> CompareWord(v,op,resolve a,resolve b)
            | instruction -> instruction
        let fitsWord n = n>= -32768L && n<=32767L
        let mutable index=0
        while index<instructions.Length do
            let instruction=rewrite instructions[index]
            let check=
                if index+1<instructions.Length && paired instructions[index] instructions[index+1] then Some instructions[index+1]
                else None
            let checkedArithmetic=check.IsSome
            let candidate =
                match instruction with
                | Binary(value,op,a,b) ->
                    let left,right=if op<>SubtractWord && a>b then b,a else a,b
                    let folded=
                        match known a,known b with
                        | Some a,Some b ->
                            let result=match op with AddWord -> int64 a+int64 b | SubtractWord -> int64 a-int64 b | MultiplyWord -> int64 a*int64 b
                            if fitsWord result then Some(int result) else None
                        | _ -> None
                    // Keep known overflowing arithmetic executable, including
                    // unchecked wrap and a checked increment's store-before-trap.
                    Some(value,BinaryKey(op,checkedArithmetic,left,right),folded,(known a).IsSome && (known b).IsSome && folded.IsNone)
                | Negate(value,source) ->
                    let folded=known source |> Option.bind(fun n -> if n= -32768 then None else Some(-n))
                    Some(value,NegateKey(checkedArithmetic,source),folded,known source=Some -32768)
                | Divide(value,op,a,b,_) ->
                    let folded=
                        match known a,known b with
                        | Some a,Some b when b<>0 && not(a= -32768 && b= -1) ->
                            let quotient,remainder=a/b,a%b
                            let correct=remainder<>0 && ((remainder<0)<>(b<0))
                            Some(if op=DivideWord then quotient-(if correct then 1 else 0) else remainder+(if correct then b else 0))
                        | _ -> None
                    let fails=known b=Some 0 || (known a=Some -32768 && known b=Some -1)
                    Some(value,DivideKey(op,a,b),folded,fails)
                | _ -> None
            match instruction,candidate with
            | Constant(value,n),_ -> constant value n
            | _,Some(value,key,folded,keep) ->
                match folded,expressions.TryGetValue key with
                | Some n,_ -> constant value n;expressions[key]<-resolve value
                | None,(true,original) when not keep -> aliases[value]<-original
                | _ ->
                    code.Add instruction
                    check |> Option.iter (rewrite >> code.Add)
                    if not keep then expressions[key]<-value
            | _ -> code.Add instruction
            index<-index+(if checkedArithmetic then 2 else 1)
        {block with Instructions=List.ofSeq code}

type private Location = Register of int | Slot of int | Literal of int | Pinned of int
exception private AllocationLimit

/// Next-use allocation in D0/D1, with two-address destination reuse. Immutable
/// values retain valid spill copies across reloads. Slots are reused after the
/// last use; the frame is bounded and never survives a template or call.
/// The caller certifies mapped vector words are read-only for the whole block,
/// including indirect aliases. Only whole-loop selection supplies this context.
let allocateWithContext maxFrameBytes (vectorRegisters:Map<int,int>) (block:Block) : Allocation option =
    if vectorRegisters |> Map.exists(fun _ r -> r<2 || r>7) then invalidArg "vectorRegisters" "Pinned registers must be outside the D0/D1 scratch set."
    if block.Instructions |> List.exists(function StoreVector(v,_) -> vectorRegisters.ContainsKey v | _ -> false) then invalidArg "vectorRegisters" "Pinned vectors must not be assigned by the block."
    let code=ResizeArray<AllocatedInstruction>()
    let locations=Dictionary<Value,Location>()
    let owners=Dictionary<int,Value>()
    let backing=Dictionary<Value,int>()
    let literals=Dictionary<Value,int>()
    let pinnedValues=Dictionary<Value,int>()
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
    let preserve value register =
        match literals.TryGetValue value with
        | true,n -> Literal n
        | _ -> match pinnedValues.TryGetValue value with true,r -> Pinned r | _ -> Slot(save value register)
    let recoveryLocation value =
        match literals.TryGetValue value with
        | true,n -> RecoveryImmediate n
        | _ ->
            match pinnedValues.TryGetValue value with
            | true,r -> RecoveryRegister r
            | _ ->
                match locations[value] with
                | Register r | Pinned r -> RecoveryRegister r
                | Slot slot -> RecoverySlot slot
                | Literal n -> RecoveryImmediate n
    let evict register =
        match owners.TryGetValue register with
        | true,value ->
            if uses.ContainsKey value && uses[value].Count>0 then locations[value]<-preserve value register
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
        | true,Literal n ->
            let register=choose pinned
            code.Add(Immediate(register,n))
            owners[register]<-value;locations[value]<-Register register
            register
        | true,Pinned source ->
            let register=choose pinned
            code.Add(Copy(register,source))
            owners[register]<-value;locations[value]<-Register register
            register
        | _ -> invalidArg "block" "A virtual value was used before its definition."
    let define value =
        let register=choose Set.empty
        owners[register]<-value;locations[value]<-Register register
        register
    let overwrite index old value register =
        if live old index then locations[old]<-preserve old register
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
            | Constant(value,n) -> let n=int(int16(uint16 n)) in literals[value]<-n;locations[value]<-Literal n
            | LoadVector(value,vector) ->
                match vectorRegisters.TryFind vector with
                | Some r -> pinnedValues[value]<-r;locations[value]<-Pinned r
                | None -> code.Add(Load(define value,vector))
            | PopMaths value -> code.Add(Pop(define value))
            | StoreVector(vector,value) -> code.Add(Store(vector,recoveryLocation value))
            | PushMaths value -> code.Add(Push(ensure value Set.empty))
            | PushBooleanMaths value -> code.Add(PushBoolean(ensure value Set.empty))
            | CompareWord(value,op,left,right) ->
                let lhs=ensure left Set.empty
                let source=recoveryLocation right
                overwrite index left value lhs
                code.Add(Comparison(op,lhs,source))
            | OverflowCheck(product,stack,store) ->
                // No reloads or spills may be inserted between arithmetic and
                // its check: MOVE would destroy V. Recovery reads the current
                // physical locations only on the non-returning failure edge.
                let productRegister=product |> Option.map (fun value ->
                    match locations[value] with Register r -> r | _ -> invalidArg "block" "Product check must immediately follow its definition.")
                code.Add(Check(productRegister,List.map recoveryLocation stack,store |> Option.map(fun (vector,value) -> vector,recoveryLocation value)))
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
                let fixedOperand =
                    match literals.TryGetValue right with
                    | true,n -> Some(left,Choice1Of2 n)
                    | _ when op<>SubtractWord && literals.ContainsKey left -> Some(right,Choice1Of2(literals[left]))
                    | _ when pinnedValues.ContainsKey right -> Some(left,Choice2Of2(pinnedValues[right]))
                    | _ when op<>SubtractWord && pinnedValues.ContainsKey left -> Some(right,Choice2Of2(pinnedValues[left]))
                    | _ -> None
                match fixedOperand with
                | Some(source,operand) ->
                    let r=ensure source Set.empty
                    let result=
                        if live source index && not(literals.ContainsKey source || pinnedValues.ContainsKey source) && owners.Count<2 then
                            let copy=choose(Set.singleton r)
                            code.Add(Copy(copy,r))
                            owners[copy]<-value;locations[value]<-Register copy
                            copy
                        else overwrite index source value r;r
                    match operand with
                    | Choice1Of2 n -> code.Add(ArithmeticImmediate(op,result,n))
                    | Choice2Of2 r -> code.Add(Arithmetic(op,result,r))
                | None ->
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
            | Divide(value,op,left,right,_) when literals.ContainsKey right && literals[right]>0 ->
                let divisor=literals[right]
                let source=ensure left Set.empty
                let result=
                    if live left index && not(literals.ContainsKey left || pinnedValues.ContainsKey left) && owners.Count<2 then
                        let copy=choose(Set.singleton source)
                        code.Add(Copy(copy,source))
                        owners[copy]<-value;locations[value]<-Register copy
                        copy
                    else overwrite index left value source;source
                if (divisor &&& (divisor-1))=0 then
                    let mutable remaining=divisor
                    let mutable shift=0
                    while remaining>1 do remaining<-remaining >>> 1;shift<-shift+1
                    code.Add(PowerOfTwoDivision(op,result,shift))
                else code.Add(PositiveDivision(op,result,divisor))
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
                    | true,old when live old index -> locations[old]<-preserve old r
                    | _ -> ()
                let snapshot=stack |> List.map recoveryLocation
                for r in [lhs;rhs] do
                    match owners.TryGetValue r with
                    | true,old ->
                        if not(live old index) then locations.Remove old |> ignore
                        owners.Remove r |> ignore
                    | _ -> ()
                owners[lhs]<-value;locations[value]<-Register lhs
                code.Add(Division(op,lhs,rhs,snapshot))
            | LoadArrayWord(value,vector,checkedBounds,indexValue,stack) ->
                let r,result=
                    match pinnedValues.TryGetValue indexValue with
                    | true,r -> r,define value
                    | _ ->
                        let r=ensure indexValue Set.empty
                        if live indexValue index && not(literals.ContainsKey indexValue) && owners.Count<2 then
                            let result=choose(Set.singleton r)
                            owners[result]<-value;locations[value]<-Register result
                            r,result
                        else
                            overwrite index indexValue value r
                            r,r
                let snapshot=stack |> List.map recoveryLocation
                code.Add(LoadElement(vector,checkedBounds,r,result,snapshot))
            | StoreArrayWord(vector,checkedBounds,indexValue,stored,stack) ->
                let r=match pinnedValues.TryGetValue indexValue with true,r -> r | _ -> ensure indexValue Set.empty
                let source=recoveryLocation stored
                let snapshot=stack |> List.map recoveryLocation
                code.Add(StoreElement(vector,checkedBounds,r,source,snapshot))
            | LoadArrayWord2(value,vector,mask,columns,row,column,stack) ->
                // Evict one scratch owner before capturing locations, even if
                // it is an input used only here. Checks may use the accumulator
                // before the address multiplication. D2-D4 stay read-only.
                let accumulator=choose Set.empty
                let row,column=recoveryLocation row,recoveryLocation column
                let snapshot=stack |> List.map recoveryLocation
                owners[accumulator]<-value;locations[value]<-Register accumulator
                code.Add(LoadElement2(vector,mask,columns,row,column,accumulator,snapshot))
            | StoreArrayWord2(vector,mask,columns,row,column,value,stack) ->
                let accumulator=choose Set.empty
                code.Add(StoreElement2(vector,mask,columns,recoveryLocation row,recoveryLocation column,recoveryLocation value,accumulator,List.map recoveryLocation stack))
            for value in sources instruction |> List.distinct do retire value index
            match destination instruction with
            | Some value when not(last.ContainsKey value) -> retire value index
            | _ -> ()
        Some {Instructions=List.ofSeq code;SpillSlots=slots;Registers=used}
    with AllocationLimit -> None

let allocate maxFrameBytes block = allocateWithContext maxFrameBytes Map.empty block

let private recoveryBytes frame stack store =
    let size = function RecoveryRegister _ -> 2 | RecoverySlot _ | RecoveryImmediate _ -> 4
    (stack |> List.sumBy size)+(if frame>0 then 4 else 0)+4+
    (store |> Option.map(fun (_,location) -> 4+size location) |> Option.defaultValue 0)

/// Only word operations and signed A6-relative vector displacements are emitted.
/// LEA reserves/releases the spill frame without changing the condition codes.
let emitWithArrayContext vectorTableOffset (vectorRegisters:Map<int,int>) (recoverySuffix:byte[]) (rowCaches:Map<int,ArrayRowCache>) allocation =
    let bytes=ResizeArray<byte>()
    let word n = bytes.Add(byte(n >>> 8));bytes.Add(byte n)
    let displacement vector = word(vectorTableOffset+4*vector)
    let frame=allocation.SpillSlots*2
    let recover = function
        | RecoveryRegister r -> word(0x3300 ||| r)
        | RecoverySlot slot -> word 0x332F;word(slot*2)
        | RecoveryImmediate n -> word 0x333C;word n
    let writeWord = function
        | RecoveryRegister r -> word(0x3480 ||| r)
        | RecoverySlot slot -> word 0x34AF;word(slot*2)
        | RecoveryImmediate n -> word 0x34BC;word n
    let recovery handler stack store =
        match store with
        | Some(vector,value) -> word 0x246E;displacement vector;writeWord value
        | None -> ()
        for location in stack do recover location
        if frame>0 then word 0x4FEF;word frame
        bytes.AddRange recoverySuffix
        word 0x4EEE;word handler // JMP runtime error(A6), never returns
    let elementAddress vector checkedBounds index stack =
        word 0x246E;displacement vector
        if checkedBounds then
            let failureBytes=recoveryBytes frame stack None + recoverySuffix.Length
            if failureBytes+2>32767 then invalidArg "allocation" "Array recovery exceeds a 68000 word branch."
            word(0x4A40 ||| index);word 0x6B00;word 10 // negative -> shared range failure
            word(0xB06A ||| (index <<< 9));word 10 // CMP.W dimension length(A2),index
            word 0x6B00;word(failureBytes+2) // BMI.W valid, including a length of 32768
            recovery 0x8AE0 stack None
        word 0x2452 // MOVEA.L (A2),A2: current data pointer, no cached address
        // Two sign-extended word additions form the full long offset without
        // changing the index, including the highest valid byte offset 65534.
        word(0xD4C0 ||| index);word(0xD4C0 ||| index)
    let moveWord r = function
        | RecoveryRegister source -> word(0x3000 ||| (r <<< 9) ||| source)
        | RecoverySlot slot -> word(0x302F ||| (r <<< 9));word(slot*2)
        | RecoveryImmediate n -> word(0x303C ||| (r <<< 9));word n
    let compareWord dst = function
        | RecoveryRegister r -> word(0xB040 ||| (dst <<< 9) ||| r)
        | RecoverySlot slot -> word(0xB06F ||| (dst <<< 9));word(slot*2)
        | RecoveryImmediate n -> word(0x0C40 ||| dst);word n
    let elementAddress2 cache vector mask columns row column accumulator stack =
        let descriptor () = word 0x246E;displacement vector
        let check location bound snapshot =
            let r=match location with RecoveryRegister r -> r | _ -> moveWord accumulator location;accumulator
            let failureBytes=recoveryBytes frame snapshot None + recoverySuffix.Length
            if failureBytes+2>32767 then invalidArg "allocation" "Rank-two array recovery exceeds a 68000 word branch."
            word(0x4A40 ||| r);word 0x6B00;word 10
            word(0xB06A ||| (r <<< 9));word bound
            word 0x6B00;word(failureBytes+2)
            recovery 0x8AE0 snapshot None
        let rowAddress () =
            if mask &&& 1<>0 then check row 12 stack
            moveWord accumulator row
            if columns=0 then word(0xC0EA ||| (accumulator <<< 9));word 10
            else word(0xC0FC ||| (accumulator <<< 9));word columns
            word 0x2452
            // Long offset arithmetic reproduces Turbo's modulo-32-bit rules.
            word(0xD5C0 ||| accumulator);word(0xD5C0 ||| accumulator)
        // lib537 checks the column first, leaving the row in its error snapshot.
        // Do this even on a cache hit; a failing first column must not evaluate
        // the row check, fetch data, or mark the row address initialized.
        match cache with
        | None ->
            descriptor()
            if mask &&& 2<>0 then check column 10 (stack@[row])
            rowAddress()
        | Some cache ->
            if mask &&& 2<>0 then descriptor();check column 10 (stack@[row])
            word 0x0805;word cache.ValidBit // BTST #bit,D5
            word 0x6600
            let skip=bytes.Count
            word 0
            if mask &&& 2=0 then descriptor()
            rowAddress()
            word(0x204A ||| (cache.AddressRegister <<< 9)) // MOVEA.L A2,An
            word 0x08C5;word cache.ValidBit // BSET #bit,D5, after all checks
            let distance=bytes.Count-skip
            if distance>32767 then invalidArg "allocation" "Invariant row initialization exceeds a word branch."
            bytes[skip]<-byte(distance >>> 8);bytes[skip+1]<-byte distance
            word(0x2448 ||| cache.AddressRegister) // MOVEA.L An,A2
        let addColumn () =
            match column with
            | RecoveryRegister r -> word(0xD4C0 ||| r)
            | RecoverySlot slot -> word 0xD4EF;word(slot*2)
            | RecoveryImmediate n -> word 0xD4FC;word n
        addColumn();addColumn()
    if frame>0 then word 0x4FEF;word(-frame)
    for index,instruction in allocation.Instructions |> List.indexed do
        match instruction with
        | Immediate(r,n) when n>= -128 && n<=127 -> word(0x7000 ||| (r <<< 9) ||| (n &&& 255))
        | Immediate(r,n) -> word(0x303C ||| (r <<< 9));word n
        | Load(r,vector) ->
            match vectorRegisters.TryFind vector with
            | Some source -> word(0x3000 ||| (r <<< 9) ||| source)
            | None -> word 0x246E;displacement vector;word(0x3012 ||| (r <<< 9))
        | Store(vector,value) -> word 0x246E;displacement vector;writeWord value
        | Pop r -> word(0x3019 ||| (r <<< 9))
        | Push r -> word(0x3300 ||| r)
        | PushBoolean r ->
            word 0x4261 // low mantissa word
            word(0x4A40 ||| r);word 0x6708
            word 0x233C;word 0x0801;word 0x4000
            word 0x6002;word 0x42A1
        | Comparison(op,dst,right) ->
            compareWord dst right
            let condition=comparisonCondition op
            word(0x50C0 ||| (condition <<< 8) ||| dst)
            word(0x0240 ||| dst);word 1
        | CompareFlags(dst,right) -> compareWord dst right
        | Copy(dst,src) -> word(0x3000 ||| (dst <<< 9) ||| src)
        | Spill(r,slot) -> word(0x3F40 ||| r);word(slot*2)
        | Reload(r,slot) -> word(0x302F ||| (r <<< 9));word(slot*2)
        | Negative r -> word(0x4440 ||| r)
        | Arithmetic(op,dst,src) -> word((match op with AddWord -> 0xD040 | SubtractWord -> 0x9040 | MultiplyWord -> 0xC1C0) ||| (dst <<< 9) ||| src)
        | ArithmeticImmediate(op,r,n) ->
            match op with
            | (AddWord|SubtractWord) when n<>0 && n>= -8 && n<=8 ->
                let subtract=(op=SubtractWord)<>(n<0)
                word((if subtract then 0x5140 else 0x5040) ||| ((abs n &&& 7) <<< 9) ||| r)
            | AddWord -> word(0x0640 ||| r);word n
            | SubtractWord -> word(0x0440 ||| r);word n
            | MultiplyWord -> word(0xC1FC ||| (r <<< 9));word n
        | PowerOfTwoDivision(op,r,shift) ->
            match op with
            | ModuloWord -> word(0x0240 ||| r);word((1 <<< shift)-1)
            | DivideWord ->
                let mutable remaining=shift
                while remaining>0 do
                    let count=min 8 remaining
                    word(0xE040 ||| ((count &&& 7) <<< 9) ||| r)
                    remaining<-remaining-count
        | PositiveDivision(op,r,divisor) ->
            word(0x48C0 ||| r)
            word(0x81FC ||| (r <<< 9));word divisor
            match op with
            | DivideWord ->
                word(0x4A80 ||| r) // TST.L: sign of the high-word remainder
                word 0x6A02;word(0x5340 ||| r) // negative remainder -> floor quotient
            | ModuloWord ->
                word(0x4840 ||| r);word(0x4A40 ||| r)
                word 0x6A04;word(0x0640 ||| r);word divisor
        | Division(op,dst,src,stack) ->
            let failureBytes=recoveryBytes frame stack None + recoverySuffix.Length
            if failureBytes+10>32767 then invalidArg "allocation" "Division recovery exceeds a 68000 word branch."
            word(0x4A40 ||| src) // TST.W divisor, before DIVS can raise a CPU trap
            word 0x6600;word(failureBytes+2) // BNE.W divide
            let failure=bytes.Count
            recovery 0x8AD8 stack None
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
        | LoadElement(vector,checkedBounds,index,result,stack) ->
            elementAddress vector checkedBounds index stack
            word(0x3012 ||| (result <<< 9))
        | StoreElement(vector,checkedBounds,index,value,stack) ->
            elementAddress vector checkedBounds index stack
            writeWord value
        | LoadElement2(vector,mask,columns,row,column,accumulator,stack) ->
            elementAddress2 (rowCaches.TryFind index) vector mask columns row column accumulator stack
            word(0x3012 ||| (accumulator <<< 9))
        | StoreElement2(vector,mask,columns,row,column,value,accumulator,stack) ->
            elementAddress2 (rowCaches.TryFind index) vector mask columns row column accumulator stack
            writeWord value
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
            recovery 0x8AD8 stack store
    if frame>0 then word 0x4FEF;word frame
    bytes.ToArray()

let emitWithContext vectorTableOffset vectorRegisters recoverySuffix allocation =
    emitWithArrayContext vectorTableOffset vectorRegisters recoverySuffix Map.empty allocation

let emit vectorTableOffset allocation = emitWithContext vectorTableOffset Map.empty [||] allocation

let private dataWords allocation =
    allocation.Instructions |> List.sumBy (function
        | Load _ -> 3
        | Store(_,value) -> 3+(match value with RecoverySlot _ -> 1 | _ -> 0)
        | LoadElement(_,checkedBounds,_,_,_) -> if checkedBounds then 6 else 5
        | StoreElement(_,checkedBounds,_,value,_) -> (if checkedBounds then 6 else 5)+(match value with RecoverySlot _ -> 1 | _ -> 0)
        | LoadElement2(_,mask,columns,row,column,_,_) | StoreElement2(_,mask,columns,row,column,_,_,_) ->
            6+(if columns=0 then 1 else 0)+(if mask &&& 1<>0 then 2 else 0)+(if mask &&& 2<>0 then 2 else 0)+
            ([row;column] |> List.sumBy(function RecoverySlot _ -> 1 | _ -> 0))
        | Spill _ | Reload _ | Pop _ | Push _ -> 1
        | _ -> 0)

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
                let ir=lower start offset (List.ofSeq pending) |> optimize
                match allocate maxFrameBytes ir with
                | Some allocation ->
                    let code=emit vectorTableOffset allocation
                    let coldBytes=allocation.Instructions |> List.sumBy(function
                        | Check(_,stack,store) -> recoveryBytes (allocation.SpillSlots*2) stack store
                        | Division(_,_,_,stack) -> recoveryBytes (allocation.SpillSlots*2) stack None
                        | LoadElement(_,true,_,_,stack) | StoreElement(_,true,_,_,stack) -> recoveryBytes (allocation.SpillSlots*2) stack None
                        | LoadElement2(_,mask,_,row,_,_,stack) | StoreElement2(_,mask,_,row,_,_,_,stack) ->
                            (if mask &&& 2<>0 then recoveryBytes (allocation.SpillSlots*2) (stack@[row]) None else 0)+
                            (if mask &&& 1<>0 then recoveryBytes (allocation.SpillSlots*2) stack None else 0)
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
