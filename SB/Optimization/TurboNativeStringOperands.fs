module TurboNativeStringOperands

// Native string comparisons, stores, CODE and LEN read their operands in place
// when they are whole DIMmed strings (265), whole string-array elements
// (263 263 175) or, for stores, CODE and LEN, any extracted slice (175). The
// original fetches copy each operand to the string heap; compare_string then
// copies both again to the maths stack for UT.CSTR, and the consumers release
// the copies. The in-place records keep every check at its original position,
// in order: extr_string's index and slice checks and each copy's
// out-of-memory test, computed against the heap the copies would have used.
// Only integer arithmetic may separate two compared operands, and a store,
// CODE or LEN directly follows its operand, so nothing can change a string
// between its read and its use.
open HirTurboIrBackend

let private comparisons = set [73;74;75;76;77;78;79]

/// Maths-stack effect (pops, pushes) of a failure-tolerant, string-free integer
/// operation: constants, scalar and array fetches, arithmetic and DUP.
let private integerEffect (ranks:Map<int,int>) = function
    | Instruction((106|109),[Word _],_) | Instruction(263,[],_) -> Some(0,1)
    | Instruction((1|4|1001),[],_) -> Some(1,1)
    | Instruction((5|6|7|8|20|1005|1006|1007),[],_) -> Some(2,1)
    | Instruction(268,[],_) -> Some(1,2)
    | Instruction((1711|1811),[Word _],_) -> Some(1,1)
    | Instruction(1911,[Word _;Word _;Word _],_) -> Some(2,1)
    | Instruction((111|1111|1611),[Word vector],_) -> ranks.TryFind vector |> Option.map(fun rank -> rank,1)
    | _ -> None

/// The first record of the shortest integer expression ending at `last` that
/// pushes exactly `count` words without underflow.
let private expressionStart ranks (items:IrItem array) last count =
    let rec search start =
        if start<0 then None
        else
            match integerEffect ranks items[start] with
            | None -> None
            | Some _ ->
                let mutable depth=0
                let mutable valid=true
                for j in start..last do
                    match integerEffect ranks items[j] with
                    | Some(pops,pushes) when depth>=pops -> depth<-depth-pops+pushes
                    | _ -> valid<-false
                if valid && depth=count then Some start else search(start-1)
    if count=0 then Some(last+1) else search last

type private Operand = Whole of vector:int * first:int | Element of vector:int * first:int

/// The operand whose last record is at `last`: a DIMmed string or a whole element.
let private operandAt (items:IrItem array) last =
    match items[last] with
    | Instruction(265,[Word vector],_) -> Some(Whole(vector,last))
    | Instruction(175,[Word vector],_) when last>=2 ->
        match items[last-2],items[last-1] with
        | Instruction(263,[],_),Instruction(263,[],_) -> Some(Element(vector,last-2))
        | _ -> None
    | _ -> None

let private first = function Whole(_,index) | Element(_,index) -> index

/// Rewrites every eligible comparison. Records outside native code are untouched.
let select enabled (output:TurboIrOutput) : Result<TurboIrOutput,string list> =
    if not enabled then Ok output
    else
        let items=List.toArray output.Items
        let ranks=TurboNativeRegions.ranks items
        let native=Array.zeroCreate<bool> items.Length
        let mutable mode=true
        for i in 0..items.Length-1 do
            native[i]<-mode
            match items[i] with
            | Instruction(254,_,_) -> mode<-true
            | Instruction(255,_,_) -> mode<-false
            | _ -> ()
        let replacements=System.Collections.Generic.Dictionary<int,IrItem list>()
        let replace operand pending =
            match operand with
            | Whole(vector,index) -> replacements[index]<-[Instruction(1532,[Word vector;Word pending],inPlaceStringComment)]
            | Element(vector,index) ->
                replacements[index]<-[];replacements[index+1]<-[]
                replacements[index+2]<-[Instruction(1531,[Word vector;Word pending],inPlaceStringComment)]
        for k in 1..items.Length-1 do
            match items[k] with
            | Instruction(op,[],_) when comparisons.Contains op && native[k] ->
                let candidate =
                    operandAt items (k-1) |> Option.bind(fun right ->
                        // A whole element pops its descriptor's indices: rank less the length.
                        let indices = match right with Element(vector,_) -> ranks.TryFind vector |> Option.map(fun rank -> rank-1) | Whole _ -> Some 0
                        indices |> Option.bind(fun count ->
                            expressionStart ranks items (first right-1) count |> Option.bind(fun start ->
                                if start<1 then None
                                else operandAt items (start-1) |> Option.map(fun left -> left,right))))
                match candidate with
                | Some(left,right) when
                    (seq {first left..k} |> Seq.forall(fun i -> native[i] && not(replacements.ContainsKey i))) &&
                    (match left with Element(vector,_) -> ranks.ContainsKey vector | Whole _ -> true) ->
                    replace left 0
                    replace right 1
                    replacements[k]<-[Instruction(1533,[Word op],inPlaceStringComment)]
                | _ -> ()
            | _ -> ()
        // A store, CODE or LEN consumes the operand fetched just before it, so
        // nothing runs between the read and the use.
        for k in 1..items.Length-1 do
            let consumer =
                match items[k] with
                | Instruction(182,[Word vector],_) -> Some(Instruction(1534,[Word vector],inPlaceStringComment))
                | Instruction(264,[Word vector],_) -> Some(Instruction(1538,[Word vector],inPlaceStringComment))
                | Instruction(269,[],_) -> Some(Instruction(1535,[],inPlaceStringComment))
                | Instruction(270,[],_) -> Some(Instruction(1536,[],inPlaceStringComment))
                | _ -> None
            let free i = native[i] && not(replacements.ContainsKey i)
            match consumer with
            | Some rewritten when free k && free(k-1) ->
                match operandAt items (k-1),items[k-1] with
                | Some(Whole _ as operand),_ -> replace operand 0;replacements[k]<-[rewritten]
                | Some(Element(_,index) as operand),_ when free index && free(index+1) -> replace operand 0;replacements[k]<-[rewritten]
                | _,Instruction(175,[Word vector],_) ->
                    replacements[k-1]<-[Instruction(1537,[Word vector;Word 0],inPlaceStringComment)]
                    replacements[k]<-[rewritten]
                | _ -> ()
            | _ -> ()
        if replacements.Count=0 then Ok output
        else
            let selected=[for i in 0..items.Length-1 do
                              match replacements.TryGetValue i with
                              | true,rewritten -> yield! rewritten
                              | _ -> yield items[i]]
            let bytes=serialize selected
            match validate bytes with
            | [] -> Ok {output with Items=selected;Bytes=bytes;Listing=formatListing selected}
            | errors -> Error errors

let format items =
    let count opcodes=items |> List.sumBy(function Instruction(op,_,_) when List.contains op opcodes -> 1 | _ -> 0)
    $"In-place string comparisons: {count [1533]}\nIn-place string stores: {count [1534;1538]}\nIn-place CODE and LEN: {count [1535;1536]}\n"
