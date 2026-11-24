module TurboNativeLoops

// Whole-loop lowering for integer FOR bodies with audited acyclic IF control. Keep the genuine
// FOR initializer and all of its hidden continuation state. Only replace the
// body and its NEXT; every external boundary remains threaded.
open System
open System.Collections.Generic
open HirTurboIrBackend

type CompiledLoop = {
    StartOffset:int
    EndOffset:int
    Counter:int
    ConstantStep:int option
    Body:TurboNativeIr.Block
    Allocation:TurboNativeIr.Allocation
    Code:byte[]
    StackExtraBytes:int
    CertifiedBinding:bool
    ControlFlow:TurboNativeControlFlow.Plan option
    ArrayInvariants:TurboNativeInvariants.Plan
}

type Rejection = {StartOffset:int;Counter:int;Reasons:string list}
type Report = { Loops:CompiledLoop list; RejectedLoops:int; StackExtraBytes:int; Rejections:Rejection list }
let emptyReport = {Loops=[];RejectedLoops=0;StackExtraBytes=0;Rejections=[]}

let private bytes words = words |> Seq.collect(fun n -> [byte(n >>> 8);byte n]) |> Seq.toArray
// Save full registers even though the counter/bound/step are words.
let private restore = bytes [0x281F;0x261F;0x241F] // MOVE.L (SP)+,D4/D3/D2

let private emit vectorTableOffset counter constantStep (body:TurboNativeIr.Allocation) (control:TurboNativeControlFlow.Plan option) (invariants:TurboNativeInvariants.Plan) =
    let output=ResizeArray<byte>()
    let labels=Dictionary<string,int>()
    let fixups=ResizeArray<int * string>()
    let word n = output.Add(byte(n >>> 8));output.Add(byte n)
    let words values = values |> List.iter word
    let label name = labels[name]<-output.Count
    let branch opcode target = word opcode;fixups.Add(output.Count,target);word 0
    let commitCounter=bytes [0x246E;vectorTableOffset+4*counter;0x3482]
    let cacheRestore=bytes [
        if not invariants.Rows.IsEmpty then
            for r in [3..2+invariants.Rows.Length] |> List.rev do yield 0x205F ||| (r <<< 9)
            yield 0x2A1F] // restore cached address registers, then D5
    let restore=Array.append cacheRestore restore
    let recover=Array.append commitCounter restore
    words [0x2F02;0x2F03;0x2F04] // save D2-D4 on entry, once per entire loop
    if not invariants.Rows.IsEmpty then
        word 0x2F05 // save D5 validity bits
        for r in 3..2+invariants.Rows.Length do word(0x2F08 ||| r)
        word 0x7A00 // MOVEQ #0,D5: no address has been evaluated yet
    words [0x246E;vectorTableOffset+4*counter;0x3412;0x362A;8;0x382A;6]
    // A2 -> counter; D2 = current word, D3 = captured END, D4 = captured STEP.
    label "body"
    let mutable leafIndex=0
    let leaf allocation=
        let caches=invariants.Leaves.TryFind leafIndex |> Option.defaultValue Map.empty
        output.AddRange(TurboNativeIr.emitWithArrayContext vectorTableOffset (Map.ofList[counter,2]) recover caches allocation)
        leafIndex<-leafIndex+1
    let test = function
        | Some TurboNativeControlFlow.FloatTop -> words [0x4AA9;2;0x43E9;6]
        | Some TurboNativeControlFlow.WordTop -> word 0x4A59 // TST.W (A1)+
        | Some(TurboNativeControlFlow.Register r) -> word(0x4A40 ||| r)
        | None -> ()
    let mutable eagerLabel=0
    match control with
    | None -> leaf body
    | Some plan ->
        for part in plan.Parts do
            match part with
            | TurboNativeControlFlow.Leaf(_,allocation) -> leaf allocation
            | TurboNativeControlFlow.Bytes bytes -> output.AddRange bytes
            | TurboNativeControlFlow.IntegerToFloat -> output.AddRange TurboNativeControlFlow.conversionBytes
            | TurboNativeControlFlow.FloatNot -> output.AddRange(TurboNativeControlFlow.notBytes())
            | TurboNativeControlFlow.IntegerNot -> output.AddRange(TurboNativeControlFlow.integerNotBytes())
            | TurboNativeControlFlow.BooleanBinary op -> output.AddRange(TurboNativeControlFlow.booleanBytes op)
            | TurboNativeControlFlow.Label target -> label $"if-{target}"
            | TurboNativeControlFlow.Branch(condition,target,value) ->
                test value
                branch (0x6000 ||| (condition <<< 8)) $"if-{target}"
            | TurboNativeControlFlow.EagerBranch(isAnd,onTrue,target,rightCondition,rightTest) ->
                test rightTest
                words [0x43E9;6] // consume the original left float; preserve RHS flags
                let decisive=if isAnd then rightCondition ^^^ 1 else rightCondition
                let skip=$"eager-{eagerLabel}"
                eagerLabel<-eagerLabel+1
                branch (0x6000 ||| (decisive <<< 8)) (if onTrue<>isAnd then $"if-{target}" else skip)
                words [0x4AA9;0xFFFC] // left mantissa, after BOTH operands evaluated
                branch (if onTrue then 0x6600 else 0x6700) $"if-{target}"
                if onTrue=isAnd then label skip
    words [0x3002] // candidate = counter + step, with lib537 word wrap
    match constantStep with
    | Some step ->
        if step<>0 && step>= -8 && step<=8 then
            words [(if step<0 then 0x5140 else 0x5040) ||| ((abs step &&& 7) <<< 9)]
        else words [0x0640;step]
        words [0xB043]
        branch (if step<0 then 0x6D00 else 0x6E00) "exit"
    | None ->
        words [0xD044;0x4A44]
        branch 0x6A00 "up" // BPL: zero step has the runtime's nonnegative direction
        words [0xB043] // CMP.W D3,D0
        branch 0x6D00 "exit" // BLT
        branch 0x6000 "commit"
        label "up"
        words [0xB043]
        branch 0x6E00 "exit" // BGT
    label "commit"
    // Only a valid next iteration commits its counter, as the original NEXT does.
    // Only independent counters qualify. Every body read uses D2; no
    // observer can see the cell until normal exit or a non-returning error.
    words [0x3400]
    branch 0x6000 "body"
    label "exit"
    output.AddRange commitCounter
    output.AddRange restore
    // Relax only this wrapper's branches; the leaf body and its recovery paths
    // are opaque bytes. Start with word branches and shorten to a fixed point,
    // since shrinking a forward branch can bring the backedge into byte range.
    let shortBranches=HashSet<int>()
    let relocated position =
        position - 2*(fixups |> Seq.filter(fun (extension,_) -> extension<position && shortBranches.Contains extension) |> Seq.length)
    let mutable changed=true
    while changed do
        changed<-false
        for position,target in fixups do
            if not(shortBranches.Contains position) then
                let destination=labels[target]
                // A forward target also moves over this branch's own removed
                // extension word. PC-relative displacements use opcode PC+2.
                let displacement=relocated destination-relocated position-(if destination>position then 2 else 0)
                // Zero in the opcode selects a word extension on the 68000.
                if displacement<>0 && displacement>= -128 && displacement<=127 then
                    shortBranches.Add position |> ignore
                    changed<-true
    let result=ResizeArray<byte>()
    let mutable cursor=0
    for position,target in fixups do
        result.AddRange(output.GetRange(cursor,position-2-cursor))
        let displacement=relocated labels[target]-relocated position
        if displacement<int Int16.MinValue || displacement>int Int16.MaxValue then
            invalidArg "body" "Native FOR branch exceeds a 68000 word displacement."
        result.Add output[position-2] // condition/opcode byte
        if shortBranches.Contains position then result.Add(byte displacement)
        else
            result.Add 0uy
            result.Add(byte(displacement >>> 8));result.Add(byte displacement)
        cursor<-position+2
    result.AddRange(output.GetRange(cursor,output.Count-cursor))
    result.ToArray()

/// HIR proofs permit stable per-loop local/global bindings; byte-only callers
/// retain the original global-storage restrictions. Bodies still contain only
/// balanced integer operations and never write/alias their pinned counter.
/// Forward IF edges require canonical joins and no external entries. Calls,
/// generic arrays, floating comparisons and nested loops remain ordinary code.
let planWithProofs (proofs:Map<int,NativeForLoopProof>) maxExtraBytes vectorTableOffset source : Result<Report,string> =
    decode source |> Result.map(fun decoded ->
        let items=List.toArray decoded
        let offsets=Array.zeroCreate (items.Length+1)
        let modes=Array.zeroCreate<bool> items.Length
        let mutable native=true
        for i in 0..items.Length-1 do
            offsets[i+1]<-offsets[i]+(serialize [items[i]]).Length
            modes[i]<-native
            match items[i] with
            | Instruction(254,_,_) -> native<-true
            | Instruction(255,_,_) -> native<-false
            | _ -> ()
        let globals=items |> Array.choose(function Instruction((101|114),[Word v],_) -> Some v | _ -> None) |> Set.ofArray
        let arrays=items |> Array.choose(function Instruction(126,[_;Word v],_) -> Some v | _ -> None) |> Set.ofArray
        let maths=TurboNativeRegions.mathsVectors items
        let unsafeProgram = items |> Array.exists(function
            | Instruction((102|110|115|134|142|147|179|224|227|235|236|240|241|244|246|249|269|275|277|278|279|280|293|294|325|327|328|364|366|382|385),_,_) -> true
            | _ -> false)
        // HIR certificates also prove absence of opaque/nonlocal source entries.
        // ROM calls and local transfers outside this region are then harmless.
        // Keep independent machine-level guards for persistent memory/resumption
        // operations. Byte-only callers still use the full legacy exclusions.
        // Nonmutating ROM maths residents outside a loop cannot affect it.
        let unsafeCertifiedProgram = items |> Array.exists(function
            | Instruction(224,[Word 2;Word v;Bytes8 descriptors],_) when maths.Contains v && descriptors.Length=1 && descriptors[0]>=1uy && descriptors[0]<=3uy -> false
            | Instruction((224|227|235|269|275|277|278|279|280|325|327|328|364|366|382|385),_,_) -> true
            | _ -> false)
        let references=Dictionary<int,int>()
        for item in items do
            match item with
            | Instruction(op,args,_) when op<>378 ->
                for arg in args do
                    match arg with
                    | LabelRef label -> references[label]<-(match references.TryGetValue label with true,n -> n+1 | _ -> 1)
                    | _ -> ()
            | _ -> ()
        let loops=ResizeArray<CompiledLoop>()
        let mutable rejected=0
        let rejections=ResizeArray<Rejection>()
        for i in 2..items.Length-1 do
            match items[i-2],items[i-1],items[i] with
            | Instruction(26,[Word counter;LabelRef top],_),Instruction(355,[],_),LabelDef(label,_)
                when label=top && not modes[i-2] && not modes[i] ->
                let body=ResizeArray<IrItem>()
                let mutable j=i+1
                let mutable depth=0
                let mutable depthValid=true
                let proof=proofs.TryFind top |> Option.filter(fun proof -> not unsafeCertifiedProgram && proof.Counter=counter && proof.ScalarVectors.Contains counter)
                let scalarAllowed v=proof |> Option.map(fun proof -> proof.ScalarVectors.Contains v) |> Option.defaultValue (globals.Contains v)
                let reasons=ResizeArray<string>()
                let mutable valid=true
                let fail reason=valid<-false;reasons.Add reason
                let reject reason=
                    if reason<>"" then reasons.Add reason
                    rejected<-rejected+1
                    rejections.Add{StartOffset=offsets[i+1];Counter=counter;Reasons=List.ofSeq reasons |> List.distinct}
                if proof.IsNone && unsafeProgram then fail "program contains binding/control barriers and no eligible HIR certificate"
                if maxExtraBytes<12 then fail "insufficient stack budget for D2-D4 saves"
                if not(scalarAllowed counter) then fail "counter vector is outside the certified scalar bindings"
                if references[top]<>1 then fail "loop body entry has external references"
                let mutable reading=true
                while reading && j<items.Length do
                    let item=items[j]
                    match item with
                    | Instruction(230,[Word v],_) when v=counter && not modes[j] -> reading<-false
                    | Instruction((254|255),[],_) -> j<-j+1
                    | _ when TurboNativeControlFlow.supports item ->
                        let normalized =
                            match item with
                            | Instruction((1|5|6|7 as op),args,comment) when not modes[j] -> Instruction(op+1000,args,comment)
                            | Instruction((337|338|339|340 as op),args,comment) when not modes[j] -> Instruction((if op=337 || op=339 then 1339 else 1340),args,comment)
                            | item -> item
                        match normalized with
                        | Instruction((106|109|263),_,_) -> depth<-depth+1
                        | Instruction(268,_,_) -> if depth<1 then depthValid<-false else depth<-depth+1
                        | Instruction((5|6|7|8|20|1005|1006|1007),_,_) -> if depth<2 then depthValid<-false else depth<-depth-1
                        | Instruction((1|1001|1334),_,_) -> if depth<1 then depthValid<-false
                        | Instruction(118,_,_) -> if depth<1 then depthValid<-false else depth<-depth-1
                        | Instruction((1711|1811),_,_) -> if depth<1 then depthValid<-false
                        | Instruction((1712|1812),_,_) -> if depth<2 then depthValid<-false else depth<-depth-2
                        | Instruction(1911,_,_) -> if depth<2 then depthValid<-false else depth<-depth-1
                        | Instruction(1912,_,_) -> if depth<3 then depthValid<-false else depth<-depth-3
                        | _ -> ()
                        match normalized with
                        | Instruction((109|118|337|338|339|340|1339|1340 as op),Word v::_,_) ->
                            if not(scalarAllowed v) then fail $"scalar vector {v} is outside the certified bindings"
                            if op<>109 && v=counter then fail "body writes the pinned counter"
                        | Instruction((1711|1712|1811|1812),[Word v],_) -> if not(arrays.Contains v) then fail $"array vector {v} has no global array declaration"
                        | Instruction((1911|1912),Word v::_,_) -> if not(arrays.Contains v) then fail $"array vector {v} has no global array declaration"
                        | _ -> ()
                        body.Add normalized
                        if body.Count>256 then fail "body exceeds the 256-record limit";reading<-false
                        j<-j+1
                    | Instruction(op,_,_) -> fail $"unsupported body opcode {op} at IR byte {offsets[j]}";reading<-false
                    | _ -> fail $"unsupported body record at IR byte {offsets[j]}";reading<-false
                let hasNext = j<items.Length && (match items[j] with Instruction(230,[Word v],_) -> v=counter | _ -> false)
                let hasEnd = j+1<items.Length && (match items[j+1] with Instruction(378,_,_) -> true | _ -> false)
                let conditional=body |> Seq.exists(TurboNativeIr.supports >> not)
                if not hasNext then fail "matching threaded NEXT was not reached"
                if not hasEnd then fail "NEXT is not followed by its end marker"
                if not conditional && (not depthValid || depth<>0) then fail "integer maths stack is unbalanced"
                if valid && hasNext && hasEnd && (conditional || (depthValid && depth=0)) then
                    // An adjacent literal is exactly the last value pushed for
                    // the initializer's STEP. Do not infer through expressions,
                    // labels, bridges or captured-variable loads.
                    let constantStep=
                        if i>=4 then
                            match items[i-4],items[i-3] with
                            | Instruction(106,[Word step],_),Instruction(357,[LabelRef _],_) -> Some(int(int16(uint16 step)))
                            | Instruction(263,[],_),Instruction(357,[LabelRef _],_) -> Some 0
                            | _ -> None
                        else None
                    let budget=min 128 (maxExtraBytes-12)
                    let prepared=
                        if conditional then
                            TurboNativeControlFlow.plan budget counter references (List.ofSeq body) |> Option.map(fun plan ->
                                let blocks=plan.Parts |> List.choose(function TurboNativeControlFlow.Leaf(ir,allocation) -> Some(ir,allocation) | _ -> None)
                                let ir:TurboNativeIr.Block={StartOffset=offsets[i+1];EndOffset=offsets[j+1];Instructions=blocks |> List.collect(fun (ir,_) -> ir.Instructions);LegacyBytes=0;LegacyDataWords=0}
                                let allocation:TurboNativeIr.Allocation={Instructions=blocks |> List.collect(fun (_,a) -> a.Instructions);SpillSlots=plan.SpillSlots;Registers=blocks |> List.map(fun (_,a) -> a.Registers) |> Set.unionMany}
                                ir,allocation,Some plan,max(plan.SpillSlots*2) plan.CallStackBytes)
                        else
                            let ir=TurboNativeIr.lower offsets[i+1] offsets[j+1] (List.ofSeq body) |> TurboNativeIr.optimize
                            TurboNativeIr.allocateWithContext budget (Map.ofList[counter,2]) ir |> Option.map(fun allocation -> ir,allocation,None,allocation.SpillSlots*2)
                    match prepared with
                    | Some(ir,allocation,control,extra) ->
                        try
                            let leaves=control |> Option.map(fun plan -> plan.Parts |> List.choose(function TurboNativeControlFlow.Leaf(ir,a) -> Some(ir,a) | _ -> None)) |> Option.defaultValue [ir,allocation]
                            let invariants=TurboNativeInvariants.plan proof (maxExtraBytes-12-extra) (control |> Option.exists(fun plan -> plan.CallStackBytes>0)) leaves
                            let code=emit vectorTableOffset counter constantStep allocation control invariants
                            loops.Add {StartOffset=offsets[i+1];EndOffset=offsets[j+1];Counter=counter;ConstantStep=constantStep;Body=ir;Allocation=allocation;Code=code;StackExtraBytes=12+extra+invariants.SaveBytes;CertifiedBinding=proof.IsSome;ControlFlow=control;ArrayInvariants=invariants}
                        with :? ArgumentException -> reject "native branch exceeds the 68000 displacement budget"
                    | None -> reject (if conditional then "conditional control flow, stack types or allocation could not be certified" else "register spill frame exceeds the stack budget")
                else reject ""
            | _ -> ()
        {Loops=List.ofSeq loops;RejectedLoops=rejected;StackExtraBytes=loops |> Seq.map _.StackExtraBytes |> Seq.fold max 0;Rejections=List.ofSeq rejections})

let plan maxExtraBytes vectorTableOffset source = planWithProofs Map.empty maxExtraBytes vectorTableOffset source

let format report =
    let lines=ResizeArray<string>()
    lines.Add($"Native integer FOR loops: selected={report.Loops.Length}; rejected={report.RejectedLoops}; extra-stack={report.StackExtraBytes} bytes")
    for rejection in report.Rejections do
        let reasons=String.concat "; " rejection.Reasons
        lines.Add($"Native FOR rejected at IR byte {rejection.StartOffset}: counter vector={rejection.Counter}; reasons={reasons}")
    for loop in report.Loops do
        lines.Add($"Loop IR bytes {loop.StartOffset}..{loop.EndOffset}: counter vector={loop.Counter} in D2; captured END=D3, STEP=D4; literal STEP={loop.ConstantStep}; counter commit=exit/error; native bytes={loop.Code.Length}; spill slots={loop.Allocation.SpillSlots}; certified binding={loop.CertifiedBinding}")
        loop.ControlFlow |> Option.iter(fun plan -> lines.Add($"  conditional body: branches={plan.Branches}; parts={plan.Parts.Length}; helper-stack={plan.CallStackBytes} bytes"))
        loop.ControlFlow |> Option.iter(fun plan -> lines.Add($"  direct conditions: comparisons={plan.DirectComparisons}; integer truth={plan.IntegerTruthTests}; folded NOT={plan.FoldedNots}; eager AND/OR={plan.EagerBranches}"))
        lines.Add($"  invariant array rows: retained={loop.ArrayInvariants.Rows.Length}; extra saves={loop.ArrayInvariants.SaveBytes} bytes; initialization=first executed access")
        for row in loop.ArrayInvariants.Rows do lines.Add($"    retained-row: {row}")
        for instruction in loop.Allocation.Instructions do lines.Add($"  allocated-loop: {instruction}")
    String.Join(Environment.NewLine,lines)+Environment.NewLine
