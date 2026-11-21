module HirEffects

// Effects are MAY effects. In particular, a callee's Writes set is never a
// definite assignment. Alias queries are symmetric but deliberately not
// transitively closed: p may alias x and y does not imply x aliases y.
open System
open HIR

type Scope = MainScope | RoutineScope of SymbolId

type Effects = {
    Reads: Set<SymbolId>
    Writes: Set<SymbolId>
    ReadsUnknown: bool
    WritesUnknown: bool
    MayThrow: bool
    InputOutput: bool
    Allocates: bool
    ControlBarrier: bool
    Calls: Set<SymbolId>
}

let empty = {
    Reads=Set.empty; Writes=Set.empty; ReadsUnknown=false; WritesUnknown=false
    MayThrow=false; InputOutput=false; Allocates=false; ControlBarrier=false; Calls=Set.empty }

let unknown = { empty with ReadsUnknown=true; WritesUnknown=true; MayThrow=true; InputOutput=true; Allocates=true; ControlBarrier=true }

let combine a b = {
    Reads=Set.union a.Reads b.Reads; Writes=Set.union a.Writes b.Writes
    ReadsUnknown=a.ReadsUnknown || b.ReadsUnknown; WritesUnknown=a.WritesUnknown || b.WritesUnknown
    MayThrow=a.MayThrow || b.MayThrow; InputOutput=a.InputOutput || b.InputOutput
    Allocates=a.Allocates || b.Allocates; ControlBarrier=a.ControlBarrier || b.ControlBarrier
    Calls=Set.union a.Calls b.Calls }

let combineAll effects = Seq.fold combine empty effects

type AliasInfo = {
    Storages: Map<SymbolId,HirStorage>
    Visible: Map<Scope,Set<SymbolId>>
    Parameters: Map<Scope,Set<SymbolId>>
    AddressTaken: Set<SymbolId>
    Escaped: Set<SymbolId>
}

let targetRoot = function
    | WriteVar(s,_,_) | WriteArrayElem(s,_,_,_) | WriteStringChar(s,_,_,_) -> Some s
    | _ -> None

let targetIndexes = function
    | WriteArrayElem(_,xs,_,_) | DynamicWriteArrayElem(_,xs,_,_) -> xs
    | WriteStringChar(_,x,_,_) | DynamicWriteStringChar(_,x,_,_) -> [x]
    | _ -> []

let rec expressions expr = seq {
    yield expr
    match expr with
    | Unary(_,x,_,_) -> yield! expressions x
    | Binary(_,a,b,_,_) -> yield! expressions a; yield! expressions b
    | ReadArrayElem(_,xs,_,_) | DynamicReadArrayElem(_,xs,_,_) -> for x in xs do yield! expressions x
    | ReadStringChar(_,x,_,_) | DynamicReadStringChar(_,x,_,_) -> yield! expressions x
    | CallFunc(_,args,_,_) ->
        for arg in args do
            match arg with
            | ValueArg x -> yield! expressions x
            | RefArg t -> for x in targetIndexes t do yield! expressions x
    | _ -> () }

let channelExpressions = function Some(ExplicitChannel x) | Some(ImplicitChannel x) -> [x] | None -> []

// Expressions evaluated by this statement itself, excluding nested blocks.
let statementExpressions = function
    | Assign(t,x,_) -> x :: targetIndexes t
    | ProcCall(_,ch,args,_) -> channelExpressions ch @ (args |> List.collect (function ValueArg x -> [x] | RefArg t -> targetIndexes t))
    | BuiltInCall(_,ch,args,_) -> channelExpressions ch @ args
    | Input(ch,prompts,targets,_) -> channelExpressions ch @ prompts @ List.collect targetIndexes targets
    | If(x,_,_,_) | Goto(x,_) | Gosub(x,_) -> [x]
    | For(_,_,a,b,step,_,_) -> [a;b;step]
    | ForSequence(_,_,prefix,a,b,suffix,step,_,_) -> prefix @ [a;b] @ suffix @ [step]
    | OnGoto(x,targets,_) | OnGosub(x,targets,_) -> x::targets
    | Return(x,_) | Restore(x,_) -> Option.toList x
    | Dim(_,bounds,_) -> bounds
    | Read(targets,_) -> List.collect targetIndexes targets
    | _ -> []

let rec statements body = seq {
    for stmt in body do
        yield stmt
        match stmt with
        | If(_,yes,no,_) -> yield! statements yes; yield! statements (Option.defaultValue [] no)
        | For(_,_,_,_,_,body,_) | ForSequence(_,_,_,_,_,_,_,body,_)
        | Repeat(_,_,body,_) | WhenError(body,_) -> yield! statements body
        | _ -> () }

let private actualRoot (storages:Map<SymbolId,HirStorage>) = function
    | RefArg t -> targetRoot t
    | ValueArg(ReadVar(s,Array _,_)) -> Some s
    | ValueArg(ReadVar(s,_,_)) when storages.TryFind s |> Option.exists (fun s -> match s.Type with Array _ -> true | _ -> false) -> Some s
    | _ -> None

let analyzeAliases (program: HirProgram) =
    let storages = program.Globals @ (program.Routines |> List.collect (fun r -> List.map _.Storage r.Parameters @ r.Locals))
    let storageMap=storages |> List.map (fun s -> s.Symbol,s) |> Map.ofList
    let globals = program.Globals |> List.map _.Symbol |> Set.ofList
    let known = program.Routines |> List.map _.Symbol |> Set.ofList
    let roots = ResizeArray<SymbolId>()
    let escaped = ResizeArray<SymbolId>()
    let sites=ResizeArray<SymbolId * HirCallArg list>()
    let call callee args =
        sites.Add(callee,args)
        for arg in args do
            match actualRoot storageMap arg with
            | Some s -> roots.Add s; if not (known.Contains callee) then escaped.Add s
            | None -> ()
    for stmt in statements (program.Main @ (program.Routines |> List.collect _.Body)) do
        match stmt with ProcCall(s,_,args,_) -> call s args | _ -> ()
        for expr in statementExpressions stmt |> Seq.collect expressions do
            match expr with CallFunc(s,args,_,_) -> call s args | _ -> ()
    // A known forwarding routine can let an actual reference escape through
    // several calls (including recursion). Scalar ValueArgs remain private.
    let escapedRoots=System.Collections.Generic.HashSet<SymbolId>(escaped)
    let routines=program.Routines |> List.map (fun r -> r.Symbol,r) |> Map.ofList
    let mutable changed=true
    while changed do
        changed<-false
        for callee,args in sites do
            match routines.TryFind callee with
            | Some routine ->
                for index in 0..min routine.Parameters.Length args.Length-1 do
                    if escapedRoots.Contains routine.Parameters[index].Storage.Symbol then
                        match actualRoot storageMap args[index] with
                        | Some actual -> if escapedRoots.Add actual then changed<-true
                        | _ -> ()
            | _ -> ()
    { Storages=storageMap
      Visible=Map.ofList ((MainScope,globals) :: (program.Routines |> List.map (fun r -> RoutineScope r.Symbol, Set.union globals (List.map _.Storage r.Parameters @ r.Locals |> List.map _.Symbol |> Set.ofList))))
      Parameters=program.Routines |> List.map (fun r -> RoutineScope r.Symbol, r.Parameters |> List.map (fun p -> p.Storage.Symbol) |> Set.ofList) |> Map.ofList
      AddressTaken=Set.ofSeq roots; Escaped=Set.ofSeq escapedRoots }

let mayAlias (aliases: AliasInfo) scope a b =
    let parameters = aliases.Parameters.TryFind scope |> Option.defaultValue Set.empty
    a=b || parameters.Contains a || parameters.Contains b ||
        (match aliases.Storages.TryFind a,aliases.Storages.TryFind b with
         | Some a,Some b -> String.Equals(a.Name,b.Name,StringComparison.OrdinalIgnoreCase)
         | _ -> true)

let expand aliases scope symbols =
    let visible = aliases.Visible.TryFind scope |> Option.defaultValue Set.empty
    visible |> Set.filter (fun candidate -> symbols |> Set.exists (mayAlias aliases scope candidate))

let conflicts aliases scope a b =
    a.WritesUnknown || b.WritesUnknown ||
    (a.ReadsUnknown && not b.Writes.IsEmpty) || (b.ReadsUnknown && not a.Writes.IsEmpty) ||
    not (Set.intersect (expand aliases scope a.Writes) (Set.union b.Reads b.Writes) |> Set.isEmpty) ||
    not (Set.intersect (expand aliases scope b.Writes) a.Reads |> Set.isEmpty) ||
    a.InputOutput || b.InputOutput || a.ControlBarrier || b.ControlBarrier || a.MayThrow || b.MayThrow

type Context = {
    Program: HirProgram
    Aliases: AliasInfo
    Summaries: Map<SymbolId,Effects>
}

let private read s = {empty with Reads=Set.singleton s}
let private write s = {empty with Writes=Set.singleton s}

let rec expression (context: Context) scope expr =
    let children xs = xs |> List.map (expression context scope) |> combineAll
    match expr with
    | Literal(ConstString _,_,_) -> {empty with MayThrow=true;Allocates=true}
    | Literal _ | Missing _ -> empty
    | ReadVar(s,_,_) ->
        match context.Aliases.Storages.TryFind s with
        | Some storage when storage.Type=String || not (storage.Name.EndsWith("%",StringComparison.Ordinal)) -> {read s with MayThrow=true;Allocates=true}
        | _ -> read s
    | DynamicReadVar _ -> {empty with ReadsUnknown=true;MayThrow=true;Allocates=true}
    | ReadArrayElem(s,xs,_,_) -> combine (children xs) {read s with MayThrow=true}
    | ReadStringChar(s,x,_,_) -> combine (expression context scope x) {read s with MayThrow=true;Allocates=true}
    | DynamicReadArrayElem(_,xs,_,_) -> combine (children xs) {empty with ReadsUnknown=true;MayThrow=true}
    | DynamicReadStringChar(_,x,_,_) -> combine (expression context scope x) {empty with ReadsUnknown=true;MayThrow=true;Allocates=true}
    | Unary((Identity|Hash),x,_,_) -> expression context scope x
    | Unary(_,x,_,_) -> combine (expression context scope x) {empty with MayThrow=true}
    | Binary(op,a,b,_,_) ->
        let effects = children [a;b]
        match op with
        | Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual | AlmostEqual
        | BitwiseAnd | BitwiseOr | BitwiseXor -> {effects with MayThrow=true} // Coercions may fail.
        | Concat | SliceRange -> {effects with MayThrow=true;Allocates=true}
        | _ -> {effects with MayThrow=true}
    | CallFunc(s,args,_,_) -> call context scope s args
and targetEvaluation context scope target = targetIndexes target |> List.map (expression context scope) |> combineAll
and targetWrite context scope target =
    let root = match targetRoot target with Some s -> write s | None -> {empty with WritesUnknown=true}
    let extra =
        match target with
        | WriteVar(s,_,_) ->
            match context.Aliases.Storages.TryFind s with
            | Some storage when storage.Name.EndsWith("%",StringComparison.Ordinal) -> empty
            | _ -> {empty with MayThrow=true;Allocates=true}
        | _ -> {empty with MayThrow=true;Allocates=true}
    combineAll [root; extra; targetEvaluation context scope target]
and call context scope symbol args =
    let evaluated = args |> List.map (function
        | ValueArg e -> expression context scope e
        | RefArg t -> combine (targetEvaluation context scope t) (match targetRoot t with Some s -> read s | None -> {empty with ReadsUnknown=true})) |> combineAll
    let known = context.Program.Routines |> List.tryFind (fun r -> r.Symbol=symbol)
    match known,context.Summaries.TryFind symbol with
    | Some routine,Some summary ->
        let parameters = routine.Parameters |> List.map (fun p -> p.Storage.Symbol)
        let formals = Set.ofList parameters
        let locals = routine.Locals |> List.map _.Symbol |> Set.ofList
        let externalSymbols symbols = Set.difference symbols (Set.union formals locals)
        let mutable reads,writes = externalSymbols summary.Reads,externalSymbols summary.Writes
        let mutable readsUnknown,writesUnknown = summary.ReadsUnknown,summary.WritesUnknown
        for index in 0..min parameters.Length args.Length-1 do
            let formal = parameters[index]
            match actualRoot context.Aliases.Storages args[index] with
            | Some actual ->
                if summary.Reads.Contains formal then reads <- reads.Add actual
                if summary.Writes.Contains formal then writes <- writes.Add actual
            | None ->
                match args[index] with
                | RefArg _ ->
                    readsUnknown <- readsUnknown || summary.Reads.Contains formal
                    writesUnknown <- writesUnknown || summary.Writes.Contains formal
                | _ -> () // A scalar ValueArg is a private temporary, including (x%).
        // Even a total, empty body can fail while bindings/call-frame storage
        // are created. These protocol effects belong to the call itself.
        combine evaluated {summary with Reads=reads;Writes=writes;ReadsUnknown=readsUnknown;WritesUnknown=writesUnknown;MayThrow=true;Allocates=true;ControlBarrier=true;Calls=summary.Calls.Add symbol}
    | _ -> combine evaluated {unknown with Calls=Set.singleton symbol}

let evaluation context scope stmt =
    statementExpressions stmt |> List.map (expression context scope) |> combineAll

let rec statement context scope stmt =
    let evaluated = evaluation context scope stmt
    let nested body = body |> List.map (statement context scope) |> combineAll
    match stmt with
    | Assign(t,_,_) -> combine evaluated {targetWrite context scope t with MayThrow=true}
    | ProcCall(s,ch,args,_) -> combine (channelExpressions ch |> List.map (expression context scope) |> combineAll) (call context scope s args)
    | BuiltInCall(Reference,_,_,_) | Remark _ | LineNumber _ -> evaluated
    | BuiltInCall(Print,_,_,_) -> combine evaluated {empty with InputOutput=true;MayThrow=true;Allocates=true}
    | BuiltInCall _ -> combine evaluated unknown // Resident extensions can mutate arbitrary storage.
    | Input(_,_,targets,_) -> combineAll [evaluated; targets |> List.map (targetWrite context scope) |> combineAll; {empty with InputOutput=true;MayThrow=true}]
    | Read(targets,_) -> combineAll [evaluated; targets |> List.map (targetWrite context scope) |> combineAll; {empty with MayThrow=true;ControlBarrier=true}]
    | Dim(s,_,_) -> combine evaluated {write s with MayThrow=true;Allocates=true}
    | If(_,yes,no,_) -> combineAll [evaluated;nested yes;nested (Option.defaultValue [] no)]
    | For(_,s,_,_,_,body,_) | ForSequence(_,s,_,_,_,_,_,body,_) -> combineAll [evaluated;read s;write s;nested body;{empty with MayThrow=true;ControlBarrier=true}]
    | Repeat(_,_,body,_) | WhenError(body,_) -> combine (nested body) {empty with ControlBarrier=true}
    | Gosub _ | OnGosub _ -> combine evaluated unknown
    | Goto _ | OnGoto _ | Exit _ | Next _ | Return _ | Restore _ -> {evaluated with ControlBarrier=true}

/// Monotone interprocedural fixed point, including recursive calls. Unknown
/// residents remain full barriers; known calls map formal effects to actuals.
let analyze program =
    let aliases = analyzeAliases program
    let mutable summaries = program.Routines |> List.map (fun r -> r.Symbol,empty) |> Map.ofList
    let mutable changed = true
    while changed do
        let context = {Program=program;Aliases=aliases;Summaries=summaries}
        let next = program.Routines |> List.map (fun r ->
            let effects = r.Body |> List.map (statement context (RoutineScope r.Symbol)) |> combineAll
            // Turbo names share vectors, including dynamically rebound locals.
            let sameNames symbols =
                aliases.Storages |> Map.toSeq |> Seq.choose (fun (s,storage) ->
                    if symbols |> Set.exists (fun root ->
                        aliases.Storages.TryFind root |> Option.exists (fun original -> String.Equals(original.Name,storage.Name,StringComparison.OrdinalIgnoreCase))) then Some s else None) |> Set.ofSeq
            let effects = {effects with Reads=sameNames effects.Reads;Writes=sameNames effects.Writes}
            r.Symbol,combine summaries[r.Symbol] effects) |> Map.ofList
        changed <- next<>summaries
        summaries <- next
    {Program=program;Aliases=aliases;Summaries=summaries}
