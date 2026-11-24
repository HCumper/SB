module TurboNativeRegions

// A deliberately small, deny-by-default audit over the emitted Turbo IR. Select
// straight-line runs only: labels, calls, loops, handlers and all unlisted
// instructions stay in P mode. Both edges of every run use the library's own
// FAST/SLOW bridges, leaving every branch target and continuation threaded.
open HirTurboIrBackend

// Conservative instruction-count savings versus P dispatch, before the six
// instructions needed by FAST/SLOW. These are not MC68008 cycle estimates.
// Audit source: lib537.asm, the corresponding N and +1000 P templates.
let private saving item =
    let opcode = match item with Instruction(op,_,_) -> op | _ -> -1
    match opcode with
    // Unproven arithmetic uses explicit checked forms when selected below.
    // The unchecked N templates are still reserved for proven expressions.
    | 1 | 5 | 6 | 7 -> Some 2
    // Scalar fetch/store: immediate vector displacement replaces the threaded
    // operand fetch. The vector contents (including flexible bindings) are used
    // exactly as in P mode; no addresses are cached across instructions.
    | 109 | 118 | 141 | 150 -> Some 3
    // Shared checked numeric array helpers avoid inlining the dimension loop.
    // Removing the first multiply-by-one and offset addition also offsets the
    // wrapper/indexer call overhead. Strings and allocation stay threaded.
    | 111 | 112 | 143 | 144 ->
        Some(if (match item with Instruction(_,_,comment) -> comment=rangeProvenArrayComment || comment=provenRankOneIntegerArrayComment || tryRankTwoIntegerArrayComment comment |> Option.exists(fun (mask,_) -> mask=0) | _ -> false) then 5 else 2)
    // Integer DIV/MOD share the same checked template in both modes, including
    // zero and -32768/-1 failures, floor division and divisor-signed remainder.
    // Comparisons keep Turbo's six-byte float Boolean result.
    | 4 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 20
    // Numeric literals, duplicates and integer-to-float conversion. No floating
    // arithmetic, allocation, ROM calls, array indexing or string operations.
    | 106 | 138 | 263 | 268 | 283 | 320 | 376 | 377 -> Some 2
    // Integer ABS replaces a ROM call, two float conversions and their bridges.
    | 1334 -> Some 40
    | _ -> None

/// Checked lowering shared by straight runs and memory-counter loops.
let internal nativeInstruction = function
    | Instruction((111|112 as op),[Word vector],comment) when (tryRankTwoIntegerArrayComment comment).IsSome ->
        let checks,columns=(tryRankTwoIntegerArrayComment comment).Value
        Instruction(op+1800,[Word vector;Word checks;Word columns],comment)
    | Instruction((1|5|6|7 as op),args,comment) when comment<>rangeProvenIntegerComment ->
        Instruction(op+1000,args,"checked native integer arithmetic")
    | Instruction((111|112 as op),args,comment) when comment=provenRankOneIntegerArrayComment ->
        Instruction(op+1700,args,comment)
    | Instruction((111|112 as op),args,comment) when comment=rankOneIntegerArrayComment ->
        Instruction(op+1600,args,comment)
    | Instruction((111|112|143|144 as op),args,comment) when comment=rangeProvenArrayComment ->
        Instruction(op+1500,args,rangeProvenArrayComment)
    | Instruction((111|112|143|144 as op),args,_) ->
        Instruction(op+1000,args,"compact checked native array access")
    | item -> item

let private completesValue opcode =
    match opcode with
    | 1 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 20 | 111 | 112 | 118 | 143 | 144 | 150 | 320 | 1334 -> true
    | _ -> false

/// Vectors bound only to the ROM resident named ABS.
let internal absVectors (items:IrItem seq) =
    items |> Seq.choose(function
        | Instruction(261,[Word v;Bytes8 name],_) -> Some(v,System.Text.Encoding.ASCII.GetString(name).Trim().ToUpperInvariant())
        | _ -> None)
    |> Seq.groupBy fst |> Seq.choose(fun (v,names) -> if names |> Seq.map snd |> Seq.distinct |> Seq.toList=["ABS"] then Some v else None) |> Set.ofSeq

/// x% = ABS(word): int-to-float, the one-float-argument ROM ABS returning a
/// float, then float-to-int. |x| is exact for every word except -32768, whose
/// float 32768 fails the conversion with overflow after it has been popped.
let internal integerAbs (abs:Set<int>) (items:IrItem list) =
    match items with
    | Instruction(320,[],_)::Instruction(224,[Word 2;Word v;Bytes8 descriptors],_)::Instruction(334,[],_)::_
        when abs.Contains v && descriptors=[|2uy|] -> true
    | _ -> false

let private units (items:IrItem list) =
    let abs=absVectors items
    let result=ResizeArray<IrItem * IrItem list>()
    let mutable rest=items
    while not rest.IsEmpty do
        match rest with
        | first::second::third::tail when integerAbs abs rest ->
            result.Add(Instruction(1334,[],nativeIntegerAbsComment),[first;second;third]);rest<-tail
        | item::tail -> result.Add(item,[item]);rest<-tail
        | [] -> ()
    List.ofSeq result

let private regionBenefit (items:IrItem array) =
    let mutable benefit=0
    let mutable index=0
    while index<items.Length do
        // The improver collapses x%=x%+/-constant to one checked P
        // increment. Charging four separate dispatches can select tiny native
        // regions whose bridges cost more than that existing peephole saves.
        let incrementSaving =
            if index+3>=items.Length then None
            else
                match items[index],items[index+1],items[index+2],items[index+3] with
                | Instruction(109,[Word source],_), Instruction(106,[Word _],_),
                  Instruction((5|6),[],proof), Instruction(118,[Word target],_) when source=target ->
                    Some(if proof=rangeProvenIntegerComment then 5 else 4)
                | _ -> None
        match incrementSaving with
        | Some score ->
            benefit<-benefit+score
            index<-index+4
        | _ ->
            benefit<-benefit+(saving items[index] |> Option.defaultValue 0)
            index<-index+1
    benefit

/// Select audited native runs after HIR optimization and IR lowering. Disabled
/// selection returns the original output unchanged, including listing comments.
let select enabled (output: TurboIrOutput) : Result<TurboIrOutput, string list> =
    if not enabled then Ok output
    else
        let items = ResizeArray<IrItem>()
        // A unit is one selectable item and the original records it replaces.
        // Only an integer ABS unit differs; threaded code keeps its records.
        let pending = ResizeArray<IrItem * IrItem list>()
        let mutable body = false
        let mutable sourceNative = true
        let flush () =
            if pending.Count > 0 then
                // Leave trailing operand pushes threaded. Besides avoiding tiny
                // fragments, this preserves codegen's checked x%=x%+constant
                // and floating-increment peepholes at the next P instruction.
                let finish =
                    pending |> Seq.mapi (fun i (item, _) -> i, item)
                    |> Seq.choose (function i, Instruction(op, _, _) when completesValue op -> Some(i + 1) | _ -> None)
                    |> Seq.tryLast |> Option.defaultValue 0
                let prefix = pending |> Seq.take finish |> Seq.map fst |> Seq.toArray
                let benefit = regionBenefit prefix
                let threaded units = units |> Seq.collect snd
                if benefit > 6 then
                    items.Add(Instruction(254, [], "audited native region"))
                    items.AddRange(prefix |> Array.map nativeInstruction)
                    items.Add(Instruction(255, [], "resume threaded code"))
                    items.AddRange(pending |> Seq.skip finish |> threaded)
                else items.AddRange(threaded pending)
                pending.Clear()
        for item,originals in units output.Items do
            match item with
            | Instruction _ when body && not sourceNative && (saving item).IsSome ->
                pending.Add((item,originals))
            | _ ->
                flush ()
                items.AddRange originals
                match item with
                | LabelDef(label, _) when label = TurboIrLabels.body -> body <- true
                | Instruction(242, [LabelRef label], _) when label = TurboIrLabels.afterLinkage -> body <- false
                | Instruction(254, _, _) -> sourceNative <- true
                | Instruction(255, _, _) -> sourceNative <- false
                | _ -> ()
        flush ()
        // An ABS resident no longer called after native rewriting needs no
        // link-time name: drop its binding, as if the source never used ABS.
        let called = items |> Seq.choose (function
                         | Instruction(224, [_; Word v; _], _) | Instruction(227, Word v :: _, _) -> Some v
                         | _ -> None) |> Set.ofSeq
        let unused = if items |> Seq.exists (function Instruction(1334, _, _) -> true | _ -> false) then Set.difference (absVectors items) called else Set.empty
        let selected = items |> Seq.filter (function Instruction(261, [Word v; _], _) -> not (unused.Contains v) | _ -> true) |> List.ofSeq
        let bytes = serialize selected
        match validate bytes with
        | [] -> Ok { output with Items = selected; Bytes = bytes; Listing = formatListing selected }
        | problems -> Error problems
