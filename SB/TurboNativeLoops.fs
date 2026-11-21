module TurboNativeLoops

// Whole-loop lowering for straight-line integer FOR bodies. Keep the genuine
// FOR initializer and all of its hidden continuation state. Only replace the
// body and its NEXT; every external boundary remains threaded.
open System
open System.Collections.Generic
open HirTurboIrBackend

type CompiledLoop = {
    StartOffset:int
    EndOffset:int
    Counter:int
    Body:TurboNativeIr.Block
    Allocation:TurboNativeIr.Allocation
    Code:byte[]
    StackExtraBytes:int
}

type Report = { Loops:CompiledLoop list; RejectedLoops:int; StackExtraBytes:int }
let emptyReport = {Loops=[];RejectedLoops=0;StackExtraBytes=0}

let private bytes words = words |> Seq.collect(fun n -> [byte(n >>> 8);byte n]) |> Seq.toArray
// Save full registers even though the counter/bound/step are words.
let private restore = bytes [0x281F;0x261F;0x241F] // MOVE.L (SP)+,D4/D3/D2

let private emit vectorTableOffset counter body =
    let output=ResizeArray<byte>()
    let labels=Dictionary<string,int>()
    let fixups=ResizeArray<int * string>()
    let word n = output.Add(byte(n >>> 8));output.Add(byte n)
    let words values = values |> List.iter word
    let label name = labels[name]<-output.Count
    let branch opcode target = word opcode;fixups.Add(output.Count,target);word 0
    words [0x2F02;0x2F03;0x2F04] // save D2-D4 on entry, once per entire loop
    words [0x246E;vectorTableOffset+4*counter;0x3412;0x362A;8;0x382A;6]
    // A2 -> counter; D2 = current word, D3 = captured END, D4 = captured STEP.
    label "body"
    output.AddRange(TurboNativeIr.emitWithContext vectorTableOffset (Map.ofList[counter,2]) restore body)
    words [0x3002;0xD044;0x4A44] // candidate = counter + step, with lib537 word wrap
    branch 0x6A00 "up" // BPL: zero step has the runtime's nonnegative direction
    words [0xB043] // CMP.W D3,D0
    branch 0x6D00 "exit" // BLT
    branch 0x6000 "commit"
    label "up"
    words [0xB043]
    branch 0x6E00 "exit" // BGT
    label "commit"
    // Only a valid next iteration commits its counter, as the original NEXT does.
    words [0x3400;0x246E;vectorTableOffset+4*counter;0x3482]
    branch 0x6000 "body"
    label "exit"
    output.AddRange restore
    for position,target in fixups do
        let displacement=labels[target]-position
        if displacement<int Int16.MinValue || displacement>int Int16.MaxValue then
            invalidArg "body" "Native FOR branch exceeds a 68000 word displacement."
        output[position]<-byte(displacement >>> 8);output[position+1]<-byte displacement
    output.ToArray()

/// Selection is deliberately restricted to programs without reference/local
/// binding, error continuation or opaque memory mutation. A body may contain
/// only balanced integer leaf operations and may never write its counter.
/// Labels, calls, arrays, comparisons and nested loops remain ordinary code.
let plan maxExtraBytes vectorTableOffset source : Result<Report,string> =
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
        let unsafeProgram = items |> Array.exists(function
            | Instruction((102|110|115|134|142|147|179|224|227|235|236|240|241|244|246|249|269|275|277|278|279|280|293|294|325|327|328|364|366|382|385),_,_) -> true
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
        for i in 2..items.Length-1 do
            match items[i-2],items[i-1],items[i] with
            | Instruction(26,[Word counter;LabelRef top],_),Instruction(355,[],_),LabelDef(label,_)
                when label=top && not modes[i-2] && not modes[i] ->
                let body=ResizeArray<IrItem>()
                let mutable j=i+1
                let mutable depth=0
                let mutable valid=not unsafeProgram && maxExtraBytes>=12 && globals.Contains counter && references[top]=1
                let mutable reading=true
                while reading && j<items.Length do
                    let item=items[j]
                    match item with
                    | Instruction(230,[Word v],_) when v=counter && not modes[j] -> reading<-false
                    | Instruction((254|255),[],_) -> j<-j+1
                    | _ when TurboNativeIr.supports item ->
                        let normalized =
                            match item with
                            | Instruction((1|5|6|7 as op),args,comment) when not modes[j] -> Instruction(op+1000,args,comment)
                            | Instruction((337|338|339|340 as op),args,comment) when not modes[j] -> Instruction((if op=337 || op=339 then 1339 else 1340),args,comment)
                            | item -> item
                        match normalized with
                        | Instruction((106|109|263),_,_) -> depth<-depth+1
                        | Instruction(268,_,_) -> if depth<1 then valid<-false else depth<-depth+1
                        | Instruction((5|6|7|8|20|1005|1006|1007),_,_) -> if depth<2 then valid<-false else depth<-depth-1
                        | Instruction((1|1001),_,_) -> if depth<1 then valid<-false
                        | Instruction(118,_,_) -> if depth<1 then valid<-false else depth<-depth-1
                        | _ -> ()
                        match normalized with
                        | Instruction((109|118|337|338|339|340|1339|1340 as op),Word v::_,_) ->
                            if not(globals.Contains v) || (op<>109 && v=counter) then valid<-false
                        | _ -> ()
                        body.Add normalized
                        if body.Count>256 then valid<-false;reading<-false
                        j<-j+1
                    | _ -> valid<-false;reading<-false
                let hasNext = j<items.Length && (match items[j] with Instruction(230,[Word v],_) -> v=counter | _ -> false)
                let hasEnd = j+1<items.Length && (match items[j+1] with Instruction(378,_,_) -> true | _ -> false)
                if valid && hasNext && hasEnd && depth=0 then
                    let ir=TurboNativeIr.lower offsets[i+1] offsets[j+1] (List.ofSeq body)
                    match TurboNativeIr.allocate (min 128 (maxExtraBytes-12)) ir with
                    | Some allocation ->
                        try
                            let code=emit vectorTableOffset counter allocation
                            loops.Add {StartOffset=offsets[i+1];EndOffset=offsets[j+1];Counter=counter;Body=ir;Allocation=allocation;Code=code;StackExtraBytes=12+allocation.SpillSlots*2}
                        with :? ArgumentException -> rejected<-rejected+1
                    | None -> rejected<-rejected+1
                else rejected<-rejected+1
            | _ -> ()
        {Loops=List.ofSeq loops;RejectedLoops=rejected;StackExtraBytes=loops |> Seq.map _.StackExtraBytes |> Seq.fold max 0})

let format report =
    let lines=ResizeArray<string>()
    lines.Add($"Native integer FOR loops: selected={report.Loops.Length}; rejected={report.RejectedLoops}; extra-stack={report.StackExtraBytes} bytes")
    for loop in report.Loops do
        lines.Add($"Loop IR bytes {loop.StartOffset}..{loop.EndOffset}: counter vector={loop.Counter} in D2; captured END=D3, STEP=D4; native bytes={loop.Code.Length}; spill slots={loop.Allocation.SpillSlots}")
    String.Join(Environment.NewLine,lines)+Environment.NewLine
