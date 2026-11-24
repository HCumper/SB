module TurboNativeControlFlow

// Acyclic integer IF bodies inside a native FOR. Scalar stores and maths-stack
// state are canonical at joins; only the certified counter remains pinned.
open System
open System.Collections.Generic
open System.IO
open HirTurboIrBackend

type Test = FloatTop | WordTop | Register of int
type Part =
    | Leaf of TurboNativeIr.Block * TurboNativeIr.Allocation
    | Bytes of byte[]
    | Label of int
    | Branch of condition:int * target:int * test:Test option
    | IntegerToFloat
    | FloatNot
    | IntegerNot
    | BooleanBinary of opcode:int
    | EagerBranch of isAnd:bool * onTrue:bool * target:int * rightCondition:int * rightTest:Test option
type Plan = {Parts:Part list;SpillSlots:int;CallStackBytes:int;Branches:int;DirectComparisons:int;IntegerTruthTests:int;FoldedNots:int;EagerBranches:int}
type private Kind = IntegerWord | BooleanFloat | NumericFloat
exception private Unsupported

let private words values=values |> List.collect(fun n -> [byte(n >>> 8);byte n]) |> List.toArray
let private templates=lazy(
    use stream=typeof<NativeForLoopProof>.Assembly.GetManifestResourceStream("lib537.bin")
    use memory=new MemoryStream()
    stream.CopyTo memory
    let data=memory.ToArray()
    let word i=(int data[i] <<< 8) ||| int data[i+1]
    [4;36;53;54;55] |> List.map(fun opcode ->
        let offset=[10..8..2306] |> List.find(fun i -> word i=opcode)
        let first=10+int(int16(uint16(word(offset+4))))
        opcode,data[first..first+(word(offset+2) &&& 0xFFFE)-1]) |> Map.ofList)

let conversionBytes=words [0x2F02;0x2F03;0x4EAE;0x8934;0x2301;0x3300;0x261F;0x241F]
let notBytes ()=templates.Value[36]
let integerNotBytes ()=templates.Value[4]
let booleanBytes op=templates.Value[op]

// Run after the typed CFG has certified canonical joins. These rules only
// remove an immediately consumed result/NOT/conversion: no faulting operation,
// store, label or eager Boolean operand can move across a branch.
let private directConditions parts =
    let output=ResizeArray<Part>()
    let mutable comparisons,truths,nots,eagers=0,0,0,0
    let take ()=let item=output[output.Count-1] in output.RemoveAt(output.Count-1);item
    let stripNots condition kind =
        let mutable condition=condition
        while output.Count>0 && kind output[output.Count-1] do
            take() |> ignore;condition<-condition ^^^ 1;nots<-nots+1
        condition
    let floatNot=function FloatNot -> true | _ -> false
    let fuse condition =
        let mutable condition=stripNots condition floatNot
        let mutable test=Some FloatTop
        if output.Count>0 then
            match output[output.Count-1] with
            | IntegerToFloat ->
                take() |> ignore;truths<-truths+1;test<-Some WordTop
                condition<-stripNots condition (function IntegerNot -> true | _ -> false)
                if output.Count>0 then
                    match output[output.Count-1] with
                    | Leaf(ir,allocation) ->
                        match List.rev allocation.Instructions with
                        | TurboNativeIr.Push r::rest ->
                            let rec forward r = function
                                | TurboNativeIr.Copy(dst,source)::rest when dst=r -> forward source rest
                                | rest -> r,rest
                            let r,rest=forward r rest
                            output[output.Count-1]<-Leaf(ir,{allocation with Instructions=List.rev rest})
                            test<-Some(Register r)
                        | _ -> ()
                    | _ -> ()
            | Leaf(ir,allocation) ->
                match List.rev allocation.Instructions with
                | TurboNativeIr.PushBoolean r::TurboNativeIr.Comparison(op,dst,right)::rest when r=dst ->
                    output[output.Count-1]<-Leaf(ir,{allocation with Instructions=(List.rev rest)@[TurboNativeIr.CompareFlags(dst,right)]})
                    condition<-TurboNativeIr.comparisonCondition op ^^^ (if condition=7 then 1 else 0)
                    test<-None;comparisons<-comparisons+1
                | TurboNativeIr.PushBoolean r::rest ->
                    output[output.Count-1]<-Leaf(ir,{allocation with Instructions=List.rev rest})
                    test<-Some(Register r)
                | _ -> ()
            | _ -> ()
        condition,test
    let canFuse index =
        let mutable index=index
        while index>=0 && floatNot output[index] do index<-index-1
        index>=0 &&
        (match output[index] with
         | IntegerToFloat -> true
         | Leaf(_,allocation) -> match List.tryLast allocation.Instructions with Some(TurboNativeIr.PushBoolean _) -> true | _ -> false
         | _ -> false)
    for part in parts do
        match part with
        | Branch(condition,target,Some FloatTop) ->
            let condition=stripNots condition floatNot
            let combined=
                if output.Count>0 then
                    match output[output.Count-1] with
                    | BooleanBinary(53|54 as op) when canFuse (output.Count-2) -> Some op
                    | _ -> None
                else None
            match combined with
            | Some op ->
                take() |> ignore
                // Both operands have ALREADY executed. Keep the original left
                // float live through every RHS error, then consume it once.
                let rightCondition,rightTest=fuse 6
                output.Add(EagerBranch((op=54),(condition=6),target,rightCondition,rightTest));eagers<-eagers+1
            | None ->
                let condition,test=fuse condition
                output.Add(Branch(condition,target,test))
        | _ -> output.Add part
    List.ofSeq output,comparisons,truths,nots,eagers

let supports = function
    | Instruction((4|9|10|11|12|13|14|15|36|53|54|55|320|334),[],_)
    | Instruction((239|242|245),[LabelRef _],_)
    | Instruction(377,[Word _;Word _],_)
    | Instruction(138,[Word _;Word _;Word _],_)
    | LabelDef _ -> true
    | item -> TurboNativeIr.supports item

/// Reject external entries, backward edges, unequal/nonempty join stacks and
/// unmodeled types. The whole plan falls back if any leaf/frame cannot fit.
let plan maxFrameBytes counter (references:Dictionary<int,int>) (items:IrItem list) =
    try
        let items=List.toArray items
        let labels=Dictionary<int,int>()
        let localRefs=Dictionary<int,int>()
        for i in 0..items.Length-1 do
            match items[i] with
            | LabelDef(label,_) -> if labels.ContainsKey label then raise Unsupported else labels[label]<-i
            | Instruction((239|242|245),[LabelRef label],_) -> localRefs[label]<-(match localRefs.TryGetValue label with true,n -> n+1 | _ -> 1)
            | _ -> ()
        for KeyValue(label,index) in labels do
            let count=match localRefs.TryGetValue label with true,n -> n | _ -> 0
            let total=match references.TryGetValue label with true,n -> n | _ -> 0
            if total<>count then raise Unsupported
        for i in 0..items.Length-1 do
            match items[i] with
            | Instruction((239|242|245),[LabelRef label],_) when not(labels.ContainsKey label) || labels[label]<=i -> raise Unsupported
            | _ -> ()
        let states=Dictionary<int,Kind list>()
        let feed index stack =
            match states.TryGetValue index with
            | true,previous when previous<>stack -> raise Unsupported
            | _ -> states[index]<-stack
        feed 0 []
        let parts=ResizeArray<Part>()
        let pending=ResizeArray<IrItem>()
        let mutable spills=0
        let mutable branches=0
        let flush comparison =
            if pending.Count>0 then
                let lower=if comparison then TurboNativeIr.lowerComparison else TurboNativeIr.lower
                let ir=lower 0 0 (List.ofSeq pending) |> TurboNativeIr.optimize
                match TurboNativeIr.allocateWithContext maxFrameBytes (Map.ofList[counter,2]) ir with
                | None -> raise Unsupported
                | Some allocation -> spills<-max spills allocation.SpillSlots;parts.Add(Leaf(ir,allocation))
                pending.Clear()
        for i in 0..items.Length-1 do
            let item=items[i]
            let mutable stack=match states.TryGetValue i with true,state -> state | _ -> []
            let reachable=states.ContainsKey i
            let popWord () = match stack with IntegerWord::rest -> stack<-rest | _ -> raise Unsupported
            let popFloat () = match stack with (BooleanFloat|NumericFloat)::rest -> stack<-rest | _ -> raise Unsupported
            let empty () = if not stack.IsEmpty then raise Unsupported
            let mutable falls=true
            if not reachable then
                match item with Instruction(2,[],_) -> () | _ -> raise Unsupported
            else
                match item with
                | LabelDef(label,_) -> empty();flush false;parts.Add(Label label)
                | Instruction((239|245 as op),[LabelRef target],_) ->
                    popFloat();empty();flush false
                    parts.Add(Branch((if op=239 then 7 else 6),target,Some FloatTop));branches<-branches+1
                    feed labels[target] stack
                | Instruction(242,[LabelRef target],_) ->
                    empty();flush false;parts.Add(Branch(0,target,None));branches<-branches+1
                    feed labels[target] stack;falls<-false
                | Instruction((9|10|11|12|13|14|15),[],_) ->
                    popWord();popWord();pending.Add item;flush true;stack<-BooleanFloat::stack
                | Instruction(320,[],_) ->
                    popWord();flush false
                    // lib537's exact conversion clobbers D2/D3. Preserve the
                    // pinned counter/bound and charge its return address too.
                    parts.Add IntegerToFloat;stack<-NumericFloat::stack
                | Instruction(334,[],_) ->
                    match stack with BooleanFloat::rest -> stack<-IntegerWord::rest | _ -> raise Unsupported
                    flush false
                    parts.Add(Bytes(words [0x7000;0x4AA9;2;0x56C0;0x0240;1;0x43E9;6;0x3300]))
                | Instruction(4,[],_) ->
                    popWord();flush false;parts.Add IntegerNot;stack<-IntegerWord::stack
                | Instruction(36,[],_) ->
                    popFloat();flush false;parts.Add FloatNot;stack<-BooleanFloat::stack
                | Instruction((53|54|55 as op),[],_) ->
                    popFloat();popFloat();flush false;parts.Add(BooleanBinary op);stack<-BooleanFloat::stack
                | Instruction((377|138),args,_) ->
                    let exponent,high,low=match args with [Word e;Word h] -> e,h,0 | [Word e;Word h;Word l] -> e,h,l | _ -> raise Unsupported
                    flush false
                    parts.Add(Bytes(words [0x333C;low;0x233C;exponent;high]))
                    stack<-(if low=0 && ((exponent=0 && high=0)||(exponent=0x801 && high=0x4000)) then BooleanFloat else NumericFloat)::stack
                | _ when TurboNativeIr.supports item ->
                    match item with
                    | Instruction((106|109|263),_,_) -> stack<-IntegerWord::stack
                    | Instruction(268,_,_) -> popWord();stack<-IntegerWord::IntegerWord::stack
                    | Instruction((5|6|7|8|20|1005|1006|1007),_,_) -> popWord();popWord();stack<-IntegerWord::stack
                    | Instruction((1|1001|1711|1811),_,_) -> popWord();stack<-IntegerWord::stack
                    | Instruction(118,_,_) -> popWord()
                    | Instruction((1712|1812),_,_) -> popWord();popWord()
                    | Instruction(1911,_,_) -> popWord();popWord();stack<-IntegerWord::stack
                    | Instruction(1912,_,_) -> popWord();popWord();popWord()
                    | _ -> ()
                    pending.Add item
                | _ -> raise Unsupported
                if falls then feed (i+1) stack
        match states.TryGetValue items.Length with true,[] -> () | _ -> raise Unsupported
        flush false
        let parts,comparisons,truths,nots,eagers=directConditions parts
        let calls=if parts |> List.exists(function IntegerToFloat -> true | _ -> false) then 12 else 0
        if max(spills*2) calls>maxFrameBytes then None
        else Some{Parts=parts;SpillSlots=spills;CallStackBytes=calls;Branches=branches;DirectComparisons=comparisons;IntegerTruthTests=truths;FoldedNots=nots;EagerBranches=eagers}
    with Unsupported | :? ArgumentException -> None

let allocations plan=plan.Parts |> List.choose(function Leaf(_,allocation) -> Some allocation | _ -> None)
