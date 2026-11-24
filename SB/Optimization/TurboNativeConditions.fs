module TurboNativeConditions

// Fused integer IF conditions in native-mode code outside whole-loop plans.
// The IR computes Turbo's six-byte float Boolean, then IF TRUE/FALSE tests and
// pops it. Here integer comparisons, Boolean AND/OR and a final integer truth
// test stay 0/1 words in D0/D1 and native branches take the original label.
// QL AND/OR do not short-circuit: every operand that could fail is evaluated in
// its original order, and checked array and arithmetic failures happen exactly
// as before. Failure snapshots rebuild pending Booleans as QL floats, so the
// maths stack seen by the runtime error handler matches the original templates.
// Only terms that can neither fail nor write anything are skipped once an
// earlier term has decided an AND/OR chain; skipping them is unobservable.
open System
open System.Text
open HirTurboIrBackend

/// Where a segment's branch goes: the original IF label, or the code after
/// the condition (an early decision for the fall-through arm).
type Exit = ToTarget | ToEnd

/// One leaf and the branch that follows it. Code includes the leaf's spill
/// frame release, which preserves the flags the branch tests.
type Segment = {
    Code:byte[]
    /// 68000 condition code that takes the branch.
    Condition:int
    Exit:Exit
    Ir:TurboNativeIr.Block
    Allocation:TurboNativeIr.Allocation
}

type CompiledCondition = {
    /// Post-improver IR byte range replaced by the segments and their branches.
    StartOffset:int
    EndOffset:int
    /// The original IF TRUE/IF FALSE label.
    Target:int
    /// In order. The last segment always exits to Target; it falls through otherwise.
    Segments:Segment list
    Comparisons:int
    Logical:int
}

type Report = { Conditions:CompiledCondition list; RejectedConditions:int; StackExtraBytes:int }
let emptyReport = {Conditions=[];RejectedConditions=0;StackExtraBytes=0}

/// A branch to Target is BCC.W, or BNCC.S over lib537's 10-byte native/P long
/// jump. A branch to the end of the condition is always BCC.W.
let shortSlotBytes=4
let longSlotBytes=12
let endSlotBytes=4

let slotBytes long exit = match exit with ToEnd -> endSlotBytes | ToTarget -> if long then longSlotBytes else shortSlotBytes

/// Total emitted bytes for the chosen Target slot size.
let size long condition = condition.Segments |> List.sumBy(fun s -> s.Code.Length+slotBytes long s.Exit)

type private Kind = WordValue | BoolValue

// Inputs and outputs, bottom-to-top. Word operations must also be accepted by
// TurboNativeIr.supports; comparisons and AND/OR are condition-only.
let private effect item =
    match item with
    | Instruction((9|10|11|12|13|14|15),[],_) -> Some([WordValue;WordValue],[BoolValue])
    | Instruction((53|54),[],_) -> Some([BoolValue;BoolValue],[BoolValue])
    | _ when not(TurboNativeIr.supports item) -> None
    | Instruction((106|109),[Word _],_) | Instruction(263,[],_) -> Some([],[WordValue])
    | Instruction(268,[],_) -> Some([WordValue],[WordValue;WordValue])
    | Instruction((1|1001|1334),[],_) -> Some([WordValue],[WordValue])
    | Instruction((5|6|7|8|20|1005|1006|1007),[],_) -> Some([WordValue;WordValue],[WordValue])
    | Instruction((1711|1811),[Word _],_) -> Some([WordValue],[WordValue])
    | Instruction(1911,_,_) -> Some([WordValue;WordValue],[WordValue])
    | Instruction(118,[Word _],_) -> Some([WordValue],[])
    | Instruction((1712|1812),[Word _],_) -> Some([WordValue;WordValue],[])
    | Instruction(1912,_,_) -> Some([WordValue;WordValue;WordValue],[])
    | Instruction((2|337|338|339|340|1339|1340),_,_) -> Some([],[])
    | _ -> None

// Cannot fail and writes nothing: loads, literals, proven (raw) arithmetic and
// array reads, and comparisons. Checked forms, DIV/MOD and ABS can fail.
let private safe = function
    | Instruction((2|106|109|263|268|1|5|6|7|1811|9|10|11|12|13|14|15),_,_) -> true
    | Instruction(1911,[_;Word 0;_],_) -> true
    | _ -> false

// Undo one item on a top-first stack of values still to be produced.
let private unapply (inputs,outputs) need =
    let produced=List.rev outputs
    if List.length need<produced.Length || List.truncate produced.Length need<>produced then None
    else Some(List.rev inputs@List.skip produced.Length need)

let private isComparison = function Instruction((9|10|11|12|13|14|15),[],_) -> true | _ -> false

/// t1 op t2 op ... tn, left-associative, with each term ending in its own
/// integer comparison. Returns the terms and whether op is AND.
let private chain (items:IrItem list) =
    let rec term acc = function
        | item::rest when isComparison item -> Some(List.rev(item::acc),rest)
        | Instruction((53|54|36|320),_,_)::_ | [] -> None
        | item::rest -> term (item::acc) rest
    let rec more isAnd terms rest =
        match rest with
        | [] -> Some(List.rev terms,isAnd)
        | _ ->
            match term [] rest with
            | Some(t,Instruction((53|54 as op),[],_)::rest) when isAnd=None || isAnd=Some(op=54) ->
                more (Some(op=54)) (t::terms) rest
            | _ -> None
    match term [] items with
    | Some(first,rest) when not rest.IsEmpty ->
        more None [first] rest |> Option.bind(fun (terms,isAnd) -> isAnd |> Option.map(fun isAnd -> terms,isAnd))
    | _ -> None

// One condition leaf: its allocation with the final test fused into flags,
// and the 68000 condition code meaning "this leaf's value is true".
let private compileLeaf maxFrameBytes vectorTableOffset first finish (items:IrItem list) =
    try
        let ir=TurboNativeIr.lowerCondition first finish items |> TurboNativeIr.optimize
        TurboNativeIr.allocate maxFrameBytes ir |> Option.bind(fun allocation ->
            let fused,trueCondition =
                match List.rev allocation.Instructions with
                | TurboNativeIr.TestWord r::TurboNativeIr.Comparison(compare,dst,right)::rest when dst=r ->
                    Some(List.rev(TurboNativeIr.CompareFlags(dst,right)::rest)),TurboNativeIr.comparisonCondition compare
                | TurboNativeIr.TestWord r::(TurboNativeIr.Arithmetic((TurboNativeIr.AndWord|TurboNativeIr.OrWord),dst,_)::_ as rest)
                | TurboNativeIr.TestWord r::(TurboNativeIr.ArithmeticImmediate((TurboNativeIr.AndWord|TurboNativeIr.OrWord),dst,_)::_ as rest) when dst=r ->
                    // AND.W/OR.W already set Z from the 0/1 result.
                    Some(List.rev rest),6
                | TurboNativeIr.TestWord _::_ -> Some allocation.Instructions,6
                | _ -> None,0
            fused |> Option.map(fun instructions ->
                let allocation={allocation with Instructions=instructions}
                ir,allocation,TurboNativeIr.emit vectorTableOffset allocation,trueCondition))
    with :? ArgumentException -> None

/// Candidates end at a native IF TRUE/FALSE whose label is defined later in
/// native mode with no intervening label. The condition is the shortest suffix
/// producing the tested value; preceding complete straight-line statements join
/// it so their stored words stay in registers. Excluded ranges belong to other
/// plans. Allocation failures keep the original templates.
let plan maxFrameBytes vectorTableOffset (excluded:(int*int) list) bytes : Result<Report,string> =
    decode bytes |> Result.map(fun decoded ->
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
        let definitions=
            items |> Array.indexed |> Array.choose(function i,LabelDef(label,_) -> Some(label,i) | _ -> None)
            |> Array.groupBy fst |> Array.choose(fun (label,uses) -> if uses.Length=1 then Some(label,snd uses[0]) else None) |> Map.ofArray
        let inside first last=excluded |> List.exists(fun (start,finish) -> first<finish && last>start)
        let available i=modes[i] && not(inside offsets[i] offsets[i+1])
        let conditions=ResizeArray<CompiledCondition>()
        let mutable rejected=0
        let mutable floor=0
        for branch in 0..items.Length-1 do
            match items[branch] with
            | Instruction((239|245 as op),[LabelRef target],_) when available branch ->
                let forward=definitions.TryFind target |> Option.exists(fun t -> t>branch && modes[t])
                let mutable last=branch-1
                let mutable flips=0
                while last>=floor && available last && (match items[last] with Instruction(36,[],_) -> true | _ -> false) do
                    flips<-flips+1;last<-last-1
                let truth=last>=floor && available last && (match items[last] with Instruction(320,[],_) -> true | _ -> false)
                let usable index=index>=floor && last-index<256 && available index
                // Shortest suffix producing the tested value from an untouched stack.
                let rec suffix index need =
                    match need with
                    | [] -> Some(index+1)
                    | _ when not(usable index) -> None
                    | _ -> effect items[index] |> Option.bind(fun e -> unapply e need) |> Option.bind(suffix (index-1))
                // A complete statement ending just before first: its first index.
                let rec statement index need =
                    if not(usable index) then None
                    else
                        match effect items[index] |> Option.bind(fun e -> unapply e need) with
                        | Some [] -> Some index
                        | Some pending -> statement (index-1) pending
                        | None -> None
                let rec absorb first = match statement (first-1) [] with Some index -> absorb index | None -> first
                let condition =
                    if not forward || last<floor then None
                    elif truth then suffix (last-1) [WordValue]
                    else suffix last [BoolValue]
                match condition |> Option.map(fun start -> start,absorb start) with
                | Some(start,first) when start<=last ->
                    // The branch is taken when the whole condition equals branchOn.
                    let branchOn=(op=245)<>((flips &&& 1)=1)
                    let leaf a b=compileLeaf maxFrameBytes vectorTableOffset offsets[a] offsets[branch+1] (List.ofArray items[a..b])
                    let segment (ir,allocation,code,trueCondition) wanted exit =
                        {Code=code;Condition=(if wanted then trueCondition else trueCondition ^^^ 1);Exit=exit;Ir=ir;Allocation=allocation}
                    let whole () = leaf first last |> Option.map(fun compiled -> [segment compiled branchOn ToTarget])
                    // Terms after the last one that can fail are skipped once
                    // decided. Earlier terms are evaluated together, eagerly.
                    let shortCircuit () =
                        if truth then None
                        else
                            chain (List.ofArray items[start..last]) |> Option.bind(fun (terms,isAnd) ->
                                let eager=terms |> List.tryFindIndexBack(List.forall safe >> not) |> Option.map((+) 1) |> Option.defaultValue 1
                                if eager>=terms.Length then None
                                else
                                    // Item index just after the eager prefix of terms and operators.
                                    let rec span index terms count = if count=0 then index else span (index+List.length(List.head terms)+(if index=start then 0 else 1)) (List.tail terms) (count-1)
                                    let prefixEnd=span start terms eager-1
                                    let decisive=not isAnd // AND is decided by false, OR by true
                                    let earlyExit=if decisive=branchOn then ToTarget else ToEnd
                                    let rest=terms |> List.skip eager
                                    let rec later index terms acc =
                                        match terms with
                                        | [] -> Some(List.rev acc)
                                        | t::more ->
                                            let finish=index+List.length t-1
                                            leaf index finish |> Option.bind(fun compiled ->
                                                let s=if more.IsEmpty then segment compiled branchOn ToTarget else segment compiled decisive earlyExit
                                                later (finish+2) more (s::acc))
                                    // Postfix order is t1 t2 op t3 op ...: the next term
                                    // follows the prefix directly; each later one follows an op.
                                    leaf first prefixEnd |> Option.bind(fun compiled ->
                                        later (prefixEnd+1) rest [] |> Option.map(fun tail -> segment compiled decisive earlyExit::tail)))
                    let segments=match shortCircuit() with Some segments -> Some segments | None -> whole()
                    match segments with
                    | Some segments when segments |> List.sumBy(fun s -> s.Code.Length+longSlotBytes)<32000 ->
                        let ir=segments |> List.collect(fun s -> s.Ir.Instructions)
                        conditions.Add {StartOffset=offsets[first];EndOffset=offsets[branch+1];Target=target;Segments=segments
                                        Comparisons=ir |> List.sumBy(function TurboNativeIr.CompareWord _ -> 1 | _ -> 0)
                                        Logical=ir |> List.sumBy(function TurboNativeIr.Binary(_,(TurboNativeIr.AndWord|TurboNativeIr.OrWord),_,_) -> 1 | _ -> 0)}
                        floor<-branch+1
                    | _ -> rejected<-rejected+1
                | _ -> ()
            | _ -> ()
        let conditions=List.ofSeq conditions
        let frame c=c.Segments |> List.map(fun s -> s.Allocation.SpillSlots*2) |> List.fold max 0
        {Conditions=conditions;RejectedConditions=rejected;StackExtraBytes=conditions |> List.map frame |> List.fold max 0})

let conditionName = function
    | 6 -> "NE" | 7 -> "EQ" | 12 -> "GE" | 13 -> "LT" | 14 -> "GT" | 15 -> "LE" | n -> string n

let format report =
    let text=StringBuilder()
    let line (s:string) = text.AppendLine s |> ignore
    line $"Fused native conditions: selected={report.Conditions.Length}; rejected={report.RejectedConditions}; extra-stack={report.StackExtraBytes} bytes"
    for c in report.Conditions do
        line $"Condition IR bytes {c.StartOffset}..{c.EndOffset}: label {c.Target}; segments={c.Segments.Length}; comparisons={c.Comparisons}; AND/OR={c.Logical}"
        for s in c.Segments do
            let exit=match s.Exit with ToTarget -> $"label {c.Target}" | ToEnd -> "end"
            line $"  segment: B{conditionName s.Condition} to {exit}; body bytes={s.Code.Length}; spill slots={s.Allocation.SpillSlots}"
            for instruction in s.Allocation.Instructions do line $"    allocated-condition: {instruction}"
    text.ToString()
