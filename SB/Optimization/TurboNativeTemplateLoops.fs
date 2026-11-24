module TurboNativeTemplateLoops

// The library's native FOR/NEXT keeps counter, END and STEP in its vector cell.
// Helpers may clobber registers freely. Word-only loops remain with the existing
// register-retaining planner; this path also admits separately certified calls.
open HirTurboIrBackend

type private Value = Integer | Floating | Text
let private marker = "audited memory-counter native FOR"
let private references = function
    | Instruction(378,_,_) -> [] // codegen defines this label at the epilogue
    | Instruction(_,args,_) -> args |> List.choose(function LabelRef label -> Some label | _ -> None)
    | _ -> []

let private ranks items = TurboNativeRegions.ranks items

// x%=ABS(word) becomes the native integer record: an integer operation that
// leaves word-only bodies to the register-pinning planner.
let private absAt (abs:Set<int>) (items:IrItem array) index =
    index+2<items.Length && TurboNativeRegions.integerAbs abs [items[index];items[index+1];items[index+2]]

let private audit (maths:Set<int>) (abs:Set<int>) (finish:int) (dimensions:Map<int,int>) (allReferences:Map<int,int>) (calls:Map<int,int>) (body:IrItem array) =
    let labels=body |> Array.mapi(fun index item -> index,item) |> Array.choose(function i,LabelDef(label,_) -> Some(label,i) | _ -> None)
    let targets=Map.ofArray labels
    let localReferences=body |> Array.collect (references >> List.toArray) |> Array.countBy id |> Map.ofArray
    let mutable valid=labels.Length=targets.Count &&
                      (targets |> Map.forall(fun label _ -> allReferences.TryFind label=localReferences.TryFind label))
    let mutable stack:Value list=[]
    let mutable extended=false
    let transform inputs outputs =
        let rec pop inputs current =
            match inputs,current with
            | [],rest -> Some rest
            | expected::xs,actual::rest when expected=actual -> pop xs rest
            | _ -> None
        match pop inputs stack with
        | Some rest -> stack<-outputs@rest
        | None -> valid<-false
    let array vector inputs output extra =
        match dimensions.TryFind vector with
        | Some rank -> transform (inputs @ List.replicate (rank+extra) Integer) output
        | None -> valid<-false
    let mutable skip=0
    for index in 0..body.Length-1 do
        if skip>0 then skip<-skip-1
        elif absAt abs body index then transform [Integer] [Integer];skip<-2
        else
        match body[index] with
        | LabelDef _ -> if not stack.IsEmpty then valid<-false
        | Instruction(op,args,_) ->
            match op,args with
            | (106|376),[Word _] -> transform [] [if op=106 then Integer else Floating]
            | 263,[] -> transform [] [Integer]
            | 138,[Word _;Word _;Word _] | 377,[Word _;Word _] -> transform [] [Floating]
            | (169|170),[Text16 _] -> transform [] [Text];extended<-true
            | 109,[Word _] -> transform [] [Integer]
            | 118,[Word _] -> transform [Integer] []
            | 141,[Word _] -> transform [] [Floating]
            | 150,[Word _] -> transform [Floating] [];extended<-true
            | (1|4),[] -> transform [Integer] [Integer]
            | (5|6|7|8|20),[] -> transform [Integer;Integer] [Integer]
            | (9|10|11|12|13|14|15),[] -> transform [Integer;Integer] [Floating]
            | 33,[] -> transform [Floating] [Floating];extended<-true
            | 36,[] -> transform [Floating] [Floating]
            | (53|54|55),[] -> transform [Floating;Floating] [Floating]
            | (37|38|39|40|41|42|43|44|45|46|47),[] ->
                transform [Floating;Floating] [Floating];extended<-true
            | (73|74|75|76|77|78|79),[] -> transform [Text;Text] [Floating];extended<-true
            | 84,[] -> transform [Text;Text] [Text];extended<-true
            | 268,[] -> transform [Integer] [Integer;Integer]
            | 320,[] -> transform [Integer] [Floating]
            | 334,[] -> transform [Floating] [Integer]
            | 310,[] -> transform [Floating] [Text];extended<-true
            | 312,[] -> transform [Integer] [Text];extended<-true
            | 317,[] -> transform [Text] [Floating];extended<-true
            | (333|269|270),[] -> transform [Text] [Integer];extended<-true
            | 273,[] -> transform [Integer] [Text];extended<-true
            | (264|374),[Word vector] when dimensions.TryFind vector=Some 1 -> transform [Text] [];extended<-true
            | 265,[Word vector] when dimensions.TryFind vector=Some 1 -> transform [] [Text];extended<-true
            | 375,[Word vector] when dimensions.TryFind vector=Some 1 -> transform [] [Integer];extended<-true
            | (111|143),[Word vector] -> array vector [] [if op=111 then Integer else Floating] 0
            | (112|144),[Word vector] -> array vector [if op=112 then Integer else Floating] [] 0
            | 175,[Word vector] -> array vector [] [Text] 1;extended<-true
            | 176,[Word vector] -> array vector [Text] [] 1;extended<-true
            | 182,[Word vector] -> array vector [Text] [] -1;extended<-true
            // A ROM maths resident: native rom_fn, one number in, a float out.
            | 224,[Word 2;Word _;Bytes8 [|descriptor|]] when TurboNativeRegions.nativeRomFunction maths body[index] ->
                transform [if descriptor=2uy then Floating else Integer] [Floating];extended<-true
            | 236,[LabelRef target] when calls.ContainsKey target ->
                transform (List.replicate calls[target] Integer) []
                if not stack.IsEmpty then valid<-false
                extended<-true
            | (239|245|242),[LabelRef label] ->
                if op<>242 then transform [Floating] []
                if not stack.IsEmpty || not(targets.TryFind label |> Option.exists(fun target -> target>index)) then valid<-false
            // EXIT of this loop: lib537's native exit jumps to the loop's own end
            // label (its 378), leaving the counter cell as threaded exit_p does.
            | 381,[LabelRef label] when label=finish -> if not stack.IsEmpty then valid<-false
            | _ -> valid<-false
    valid && extended && stack.IsEmpty

let private native item =
    match item with
    // Native append/LEN omit P-mode missing-string checks. Retain the exact P
    // operations and their failure timing, including when an append is full,
    // unless the string's DIM is proven to reach the record.
    | Instruction((374|375),_,comment) when comment<>allocatedStringComment ->
        [Instruction(255,[],"checked string helper");item;Instruction(254,[],"resume memory-counter loop")]
    | Instruction(236,_,_) ->
        // The original P call/return protocol owns every recursive LOCAL and
        // saved FOR cell. Both mode bridges preserve the canonical maths stack;
        // leaf allocation is flushed before SLOW, and starts fresh after FAST.
        [Instruction(255,[],"audited memory-counter procedure call");item;Instruction(254,[],"resume memory-counter loop after call")]
    | _ -> [TurboNativeRegions.nativeInstruction item]

/// Select certified acyclic bodies, with empty, equal typed stacks at all joins.
/// Unknown operations, nested loops and external entries retain threaded code.
let select enabled (output:TurboIrOutput) : Result<TurboIrOutput,string list> =
    if not enabled then Ok output
    else
        let source=List.toArray output.Items
        let dimensions=ranks source
        let maths=TurboNativeRegions.mathsVectors source
        let abs=TurboNativeRegions.absVectors source
        let refs=source |> Array.collect (references >> List.toArray) |> Array.countBy id |> Map.ofArray
        let result=ResizeArray<IrItem>()
        let mutable index=0
        let mutable sourceNative=true
        while index<source.Length do
            let candidate =
                if sourceNative || index+5>=source.Length then None
                else
                    match source[index..index+3] with
                    | [|Instruction(357,[LabelRef finish],_);Instruction(26,[Word counter;LabelRef top],_);Instruction(355,[],_);LabelDef(label,_)|]
                        when label=top && refs.TryFind top=Some 1 ->
                        let memoryProof=output.MemoryForLoops.TryFind top=Some counter
                        let pinnedProof=output.NativeForLoopProofs.TryFind top |> Option.exists(fun proof -> proof.Counter=counter && proof.ScalarVectors.Contains counter)
                        if memoryProof || pinnedProof then
                            let ending=seq {index+4..min (source.Length-2) (index+260)} |> Seq.tryFind(fun i ->
                                match source[i],source[i+1] with
                                | Instruction(230,[Word v],_),Instruction(378,[LabelRef target],_) -> v=counter && target=finish
                                | _ -> false)
                            ending |> Option.filter(fun ending ->
                                // The end label's references: the initializer and this body's own EXITs.
                                let exits=source[index+4..ending-1] |> Array.filter(function Instruction(381,[LabelRef label],_) -> label=finish | _ -> false) |> Array.length
                                refs.TryFind finish=Some(1+exits) &&
                                audit maths abs finish dimensions refs (if memoryProof then output.NativeProcedureCalls else Map.empty) source[index+4..ending-1])
                        else None
                    | _ -> None
            match candidate with
            | Some ending ->
                result.Add(Instruction(254,[],marker))
                let mutable i=index
                while i<=ending+1 do
                    if absAt abs source i && i+2<=ending+1 then
                        result.Add(Instruction(1334,[],nativeIntegerAbsComment));i<-i+3
                    else result.AddRange(native source[i]);i<-i+1
                result.Add(Instruction(255,[],"resume threaded code after memory-counter FOR"))
                index<-ending+2
            | None ->
                let item=source[index]
                result.Add item
                match item with
                | Instruction(254,_,_) -> sourceNative<-true
                | Instruction(255,_,_) -> sourceNative<-false
                | _ -> ()
                index<-index+1
        let selected=List.ofSeq result
        let bytes=serialize selected
        match validate bytes with
        | [] -> Ok {output with Items=selected;Bytes=bytes;Listing=formatListing selected}
        | errors -> Error errors

let format items =
    let count=items |> List.sumBy(function Instruction(254,[],comment) when comment=marker -> 1 | _ -> 0)
    let calls=items |> List.sumBy(function Instruction(255,[],"audited memory-counter procedure call") -> 1 | _ -> 0)
    $"Audited memory-counter native FOR loops: {count}\nMemory-counter native procedure call boundaries: {calls}\n"
