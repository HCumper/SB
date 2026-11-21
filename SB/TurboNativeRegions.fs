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
        Some(if (match item with Instruction(_,_,comment) -> comment=rangeProvenArrayComment | _ -> false) then 5 else 2)
    // Integer DIV/MOD share the same checked template in both modes, including
    // zero and -32768/-1 failures, floor division and divisor-signed remainder.
    // Comparisons keep Turbo's six-byte float Boolean result.
    | 4 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 20
    // Numeric literals, duplicates and integer-to-float conversion. No floating
    // arithmetic, allocation, ROM calls, array indexing or string operations.
    | 106 | 138 | 263 | 268 | 283 | 320 | 376 | 377 -> Some 2
    | _ -> None

let private completesValue opcode =
    match opcode with
    | 1 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 20 | 111 | 112 | 118 | 143 | 144 | 150 | 320 -> true
    | _ -> false

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
        let pending = ResizeArray<IrItem>()
        let mutable body = false
        let mutable sourceNative = true
        let flush () =
            if pending.Count > 0 then
                // Leave trailing operand pushes threaded. Besides avoiding tiny
                // fragments, this preserves codegen's checked x%=x%+constant
                // and floating-increment peepholes at the next P instruction.
                let finish =
                    pending |> Seq.mapi (fun i item -> i, item)
                    |> Seq.choose (function i, Instruction(op, _, _) when completesValue op -> Some(i + 1) | _ -> None)
                    |> Seq.tryLast |> Option.defaultValue 0
                let prefix = pending |> Seq.take finish |> Seq.toArray
                let benefit = regionBenefit prefix
                if benefit > 6 then
                    items.Add(Instruction(254, [], "audited native region"))
                    items.AddRange(prefix |> Array.map (function
                        | Instruction((1|5|6|7 as op),args,comment) when comment<>rangeProvenIntegerComment ->
                            Instruction(op+1000,args,"checked native integer arithmetic")
                        | Instruction((111|112|143|144 as op),args,comment) when comment=rangeProvenArrayComment ->
                            Instruction(op+1500,args,rangeProvenArrayComment)
                        | Instruction((111|112|143|144 as op),args,_) ->
                            Instruction(op+1000,args,"compact checked native array access")
                        | item -> item))
                    items.Add(Instruction(255, [], "resume threaded code"))
                    items.AddRange(pending |> Seq.skip finish)
                else items.AddRange pending
                pending.Clear()
        for item in output.Items do
            match item with
            | Instruction _ when body && not sourceNative && (saving item).IsSome ->
                pending.Add item
            | _ ->
                flush ()
                items.Add item
                match item with
                | LabelDef(label, _) when label = TurboIrLabels.body -> body <- true
                | Instruction(242, [LabelRef label], _) when label = TurboIrLabels.afterLinkage -> body <- false
                | Instruction(254, _, _) -> sourceNative <- true
                | Instruction(255, _, _) -> sourceNative <- false
                | _ -> ()
        flush ()
        let selected = List.ofSeq items
        let bytes = serialize selected
        match validate bytes with
        | [] -> Ok { output with Items = selected; Bytes = bytes; Listing = formatListing selected }
        | problems -> Error problems
