module TurboNativeRomMaths

// Native calls of the QL ROM's one-argument maths functions. lib537's rom_fn
// calls a BASIC function by building a parameter name table on the heap,
// pretending the interpreter is running and reconciling the result type. In
// the JS ROM, ABS, ACOS, ACOT, ASIN, ATAN, COS, COT, EXP, LN, LOG10, SIN, SQRT
// and TAN each fetch their one argument as a float (CA.GTFP, which also
// rejects any other parameter count), reserve 48 bytes of maths stack and
// call the routine that RI.EXEC dispatches that operation to. A compiled call
// has exactly one float or integer argument and expects a float back, so the
// SB-only record 1539 calls RI.EXEC with that operation instead: the same ROM
// routine on the same value. It keeps rom_fn's out-of-memory test, converts
// an integer argument with RI.FLOAT, and raises a returned error through
// qdos_error, as rom_fn does. The routines read only the maths stack: with
// every other register randomised they give identical results and errors, and
// RI.EXEC preserves every register except D0 and A1.
open HirTurboIrBackend

/// RI.EXEC operation codes of the maths functions whose BASIC entries only
/// fetch one float and call that operation's routine.
let operations =
    Map [ "ABS",0x12; "ACOS",0x22; "ACOT",0x26; "ASIN",0x20; "ATAN",0x24; "COS",0x18; "COT",0x1E
          "EXP",0x2E; "LN",0x2A; "LOG10",0x2C; "SIN",0x1A; "SQRT",0x28; "TAN",0x1C ]

/// Vectors bound by exactly one link record to one of those functions, with its operation.
let private directVectors (items:IrItem seq) =
    items |> Seq.choose(function
        | Instruction(261,[Word v;Bytes8 name],_) -> Some(v,System.Text.Encoding.ASCII.GetString(name).Trim().ToUpperInvariant())
        | _ -> None)
    |> Seq.groupBy fst |> Seq.choose(fun (v,names) ->
        match names |> Seq.map snd |> Seq.toList with
        | [name] -> operations.TryFind name |> Option.map(fun operation -> v,operation)
        | _ -> None) |> Map.ofSeq

/// Rewrites every float-valued native call of such a function with one float
/// or integer argument. Calls in threaded code keep rom_fn_p. A function no
/// longer called by any record needs no link-time name and loses its binding.
let select enabled (output:TurboIrOutput) : Result<TurboIrOutput,string list> =
    if not enabled then Ok output
    else
        let items=List.toArray output.Items
        let direct=directVectors items
        let mutable native=true
        let mutable rewritten=0
        let selected=[|
            for item in items do
                match item with
                | Instruction(224,[Word 2;Word v;Bytes8 [|argument|]],_)
                    when native && (argument=2uy || argument=3uy) && direct.ContainsKey v ->
                    rewritten<-rewritten+1
                    yield Instruction(1539,[Word direct[v];Word(int argument)],directRomMathsComment)
                | _ -> yield item
                match item with
                | Instruction(254,_,_) -> native<-true
                | Instruction(255,_,_) -> native<-false
                | _ -> () |]
        if rewritten=0 then Ok output
        else
            let called=selected |> Seq.choose(function
                           | Instruction(224,[_;Word v;_],_) | Instruction(227,Word v :: _,_) -> Some v
                           | _ -> None) |> Set.ofSeq
            let unused=Set.difference (direct.Keys |> Set.ofSeq) called
            let selected=selected |> Seq.filter(function Instruction(261,[Word v;_],_) -> not(unused.Contains v) | _ -> true) |> List.ofSeq
            let bytes=serialize selected
            match validate bytes with
            | [] -> Ok {output with Items=selected;Bytes=bytes;Listing=formatListing selected}
            | errors -> Error errors

let format items =
    let count=items |> List.sumBy(function Instruction(1539,_,_) -> 1 | _ -> 0)
    $"Direct RI.EXEC maths calls: {count}\n"
