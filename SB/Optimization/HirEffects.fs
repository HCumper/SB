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
    // Content writes still invalidate scalar/alias values. Only these separate
    // effects can replace an array descriptor or captured FOR END/STEP/NEXT.
    DescriptorWrites: Set<SymbolId>
    LoopMetadataWrites: Set<SymbolId>
    DescriptorWritesUnknown: bool
    LoopMetadataWritesUnknown: bool
    MayThrow: bool
    InputOutput: bool
    Allocates: bool
    ControlBarrier: bool
    Calls: Set<SymbolId>
}

let empty = {
    Reads=Set.empty; Writes=Set.empty; ReadsUnknown=false; WritesUnknown=false
    DescriptorWrites=Set.empty;LoopMetadataWrites=Set.empty
    DescriptorWritesUnknown=false;LoopMetadataWritesUnknown=false
    MayThrow=false; InputOutput=false; Allocates=false; ControlBarrier=false; Calls=Set.empty }

let unknown = { empty with ReadsUnknown=true; WritesUnknown=true; DescriptorWritesUnknown=true;LoopMetadataWritesUnknown=true; MayThrow=true; InputOutput=true; Allocates=true; ControlBarrier=true }

let combine a b = {
    Reads=Set.union a.Reads b.Reads; Writes=Set.union a.Writes b.Writes
    ReadsUnknown=a.ReadsUnknown || b.ReadsUnknown; WritesUnknown=a.WritesUnknown || b.WritesUnknown
    DescriptorWrites=Set.union a.DescriptorWrites b.DescriptorWrites;LoopMetadataWrites=Set.union a.LoopMetadataWrites b.LoopMetadataWrites
    DescriptorWritesUnknown=a.DescriptorWritesUnknown || b.DescriptorWritesUnknown
    LoopMetadataWritesUnknown=a.LoopMetadataWritesUnknown || b.LoopMetadataWritesUnknown
    MayThrow=a.MayThrow || b.MayThrow; InputOutput=a.InputOutput || b.InputOutput
    Allocates=a.Allocates || b.Allocates; ControlBarrier=a.ControlBarrier || b.ControlBarrier
    Calls=Set.union a.Calls b.Calls }

let combineAll effects = Seq.fold combine empty effects

type AliasInfo = {
    Storages: Map<SymbolId,HirStorage>
    ArrayStorages: Set<SymbolId>
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

// A free name in a procedure can refer to a caller's dynamic binding. Resolve
// it only if no parameter/local anywhere can shadow the unique global. A
// procedure's own binding takes precedence and is private across normal calls.
let private namedRoot (program:HirProgram) (storages:Map<SymbolId,HirStorage>) scope name =
    let same (storage:HirStorage)=String.Equals(storage.Name,name,StringComparison.OrdinalIgnoreCase)
    let single (matches:HirStorage list) = match matches with [storage] -> Some storage.Symbol | _ -> None
    match scope with
    | MainScope -> program.Globals |> List.filter same |> single
    | RoutineScope symbol ->
        let own=program.Routines |> List.tryFind(fun r -> r.Symbol=symbol) |> Option.map(fun r -> List.map _.Storage r.Parameters @ r.Locals |> List.filter same) |> Option.defaultValue []
        if not own.IsEmpty then single own
        else
            let matches=storages |> Map.toList |> List.map snd |> List.filter same
            if matches |> List.forall(fun storage -> storage.Class=GlobalStorage) then single matches else None

let private storageRoot program (storages:Map<SymbolId,HirStorage>) scope symbol =
    storages.TryFind symbol |> Option.bind(fun storage -> namedRoot program storages scope storage.Name)

let private targetStorageRoot program storages scope = function
    | WriteVar(s,_,_) | WriteArrayElem(s,_,_,_) | WriteStringChar(s,_,_,_) -> storageRoot program storages scope s
    | DynamicWriteVar(name,_,_) | DynamicWriteArrayElem(name,_,_,_) | DynamicWriteStringChar(name,_,_,_) -> namedRoot program storages scope name

let private arrayStorages (program:HirProgram) (storages:Map<SymbolId,HirStorage>) =
    let declared=storages |> Map.toSeq |> Seq.choose(fun (s,storage) -> match storage.Type with Array _ -> Some s | _ -> None) |> Set.ofSeq
    program.Routines |> List.fold(fun roots routine ->
        let formals=routine.Parameters |> List.map(fun p -> p.Storage.Symbol,p.Storage) |> Map.ofList
        let indexed symbol indexes =
            formals.TryFind symbol |> Option.exists(fun storage -> not(storage.Name.EndsWith("$",StringComparison.Ordinal) && List.length indexes=1))
        statements routine.Body |> Seq.fold(fun roots stmt ->
            let reads=statementExpressions stmt |> Seq.collect expressions |> Seq.choose(function ReadArrayElem(s,xs,_,_) when indexed s xs -> Some s | _ -> None)
            let targets=match stmt with Assign(t,_,_) -> [t] | Read(ts,_) | Input(_,_,ts,_) -> ts | _ -> []
            let writes=targets |> Seq.choose(function WriteArrayElem(s,xs,_,_) when indexed s xs -> Some s | _ -> None)
            Set.union roots (Seq.append reads writes |> Set.ofSeq)) roots) declared

let private actualRoot program (storages:Map<SymbolId,HirStorage>) arrays scope = function
    | RefArg t -> targetStorageRoot program storages scope t
    | ValueArg(ReadVar(s,Array _,_)) -> storageRoot program storages scope s
    | ValueArg(ReadVar(s,_,_)) when Set.contains s arrays -> storageRoot program storages scope s
    | ValueArg(DynamicReadVar(name,hirType,_)) ->
        namedRoot program storages scope name |> Option.filter(fun s -> match hirType with Array _ -> true | _ -> Set.contains s arrays)
    | _ -> None

let analyzeAliases (program: HirProgram) =
    let storages = program.Globals @ (program.Routines |> List.collect (fun r -> List.map _.Storage r.Parameters @ r.Locals))
    let storageMap=storages |> List.map (fun s -> s.Symbol,s) |> Map.ofList
    let arrays=arrayStorages program storageMap
    let globals = program.Globals |> List.map _.Symbol |> Set.ofList
    let known = program.Routines |> List.map _.Symbol |> Set.ofList
    let roots = ResizeArray<SymbolId>()
    let escaped = ResizeArray<SymbolId>()
    let sites=ResizeArray<Scope * SymbolId * HirCallArg list>()
    let call scope callee args =
        sites.Add(scope,callee,args)
        for arg in args do
            match actualRoot program storageMap arrays scope arg with
            | Some s -> roots.Add s; if not (known.Contains callee) then escaped.Add s
            | None -> ()
    for scope,body in (MainScope,program.Main)::(program.Routines |> List.map(fun r -> RoutineScope r.Symbol,r.Body)) do
        for stmt in statements body do
            match stmt with ProcCall(s,_,args,_) -> call scope s args | _ -> ()
            for expr in statementExpressions stmt |> Seq.collect expressions do
                match expr with CallFunc(s,args,_,_) -> call scope s args | _ -> ()
    // A known forwarding routine can let an actual reference escape through
    // several calls (including recursion). Scalar ValueArgs remain private.
    let escapedRoots=System.Collections.Generic.HashSet<SymbolId>(escaped)
    let routines=program.Routines |> List.map (fun r -> r.Symbol,r) |> Map.ofList
    let mutable changed=true
    while changed do
        changed<-false
        for scope,callee,args in sites do
            match routines.TryFind callee with
            | Some routine ->
                for index in 0..min routine.Parameters.Length args.Length-1 do
                    if escapedRoots.Contains routine.Parameters[index].Storage.Symbol then
                        match actualRoot program storageMap arrays scope args[index] with
                        | Some actual -> if escapedRoots.Add actual then changed<-true
                        | _ -> ()
            | _ -> ()
    // A formal can alias other storage only when some call binds it by
    // reference. One that every static call site passes a scalar value is a
    // private temporary; missing arguments and reference formals stay aliased.
    // Like other uncalled routine facts, a formal with no call site stays aliased.
    let privateFormal (routine:HirRoutine) index (formal:HirParameter) =
        let calls=sites |> Seq.filter(fun (_,callee,_) -> callee=routine.Symbol) |> Seq.toList
        formal.Binding<>ReferenceBinding && not calls.IsEmpty &&
        calls |> List.forall(fun (scope,_,args) ->
            index<List.length args &&
            match args[index] with
            | ValueArg _ as arg -> (actualRoot program storageMap arrays scope arg).IsNone
            | RefArg _ -> false)
    { Storages=storageMap;ArrayStorages=arrays
      Visible=Map.ofList ((MainScope,globals) :: (program.Routines |> List.map (fun r -> RoutineScope r.Symbol, Set.union globals (List.map _.Storage r.Parameters @ r.Locals |> List.map _.Symbol |> Set.ofList))))
      Parameters=program.Routines |> List.map (fun r ->
          RoutineScope r.Symbol, r.Parameters |> List.indexed |> List.filter(fun (i,p) -> not(privateFormal r i p)) |> List.map (fun (_,p) -> p.Storage.Symbol) |> Set.ofList) |> Map.ofList
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

let private resolve context scope symbol = storageRoot context.Program context.Aliases.Storages scope symbol
let private named context scope name = namedRoot context.Program context.Aliases.Storages scope name
let private readRoot = function Some s -> read s | None -> {empty with ReadsUnknown=true}

let loopInitialisation context scope symbol =
    match resolve context scope symbol with
    | Some s -> {write s with DescriptorWrites=Set.singleton s;LoopMetadataWrites=Set.singleton s}
    | None -> {empty with WritesUnknown=true;DescriptorWritesUnknown=true;LoopMetadataWritesUnknown=true}

// The Turbo backend emits the existing ROM ABS helper directly. It may fail
// coercion/allocation, but it neither rebinds storage nor installs continuations.
// User-defined routines with this name still use their actual summaries.
// These exact unary intrinsics are lowered inline, without a resident-function
// call. Arguments still contribute all reads, allocation and failure effects.
let inlineFunction (program:HirProgram) symbol argumentCount =
    argumentCount=1 && not(program.Routines |> List.exists(fun r -> r.Symbol=symbol)) &&
    (program.SymbolNames.TryFind symbol |> Option.exists(fun name ->
        ["LEN";"CODE";"CHR$"] |> List.exists(fun intrinsic -> name.Equals(intrinsic,StringComparison.OrdinalIgnoreCase))))

let nonmutatingFunction (program:HirProgram) symbol argumentCount =
    inlineFunction program symbol argumentCount ||
    argumentCount=1 && not(program.Routines |> List.exists(fun r -> r.Symbol=symbol)) &&
    (program.SymbolNames.TryFind symbol |> Option.exists(fun name -> name.Equals("ABS",StringComparison.OrdinalIgnoreCase)))

// Match only the slice shapes emitted inline by HirTurboIrBackend. General
// RIGHT$/MID$ calls still use the resident-function ABI and remain opaque.
let inlineCall (program:HirProgram) symbol args =
    let named symbol name = program.SymbolNames.TryFind symbol |> Option.exists(fun actual -> actual.Equals(name,StringComparison.OrdinalIgnoreCase))
    let builtin symbol = not(program.Routines |> List.exists(fun r -> r.Symbol=symbol))
    let one = function Literal(ConstInt 1,_,_) -> true | _ -> false
    inlineFunction program symbol (List.length args) ||
    (builtin symbol &&
     match args with
     | [ValueArg _;ValueArg start;ValueArg(Binary(Add,Binary(Subtract,_,start2,_,_),unit,_,_))] ->
         named symbol "MID$" && start=start2 && one unit
     | [ValueArg baseExpr;ValueArg(Binary(Add,Binary(Subtract,CallFunc(len,[ValueArg base2],_,_),_,_,_),unit,_,_))] ->
         named symbol "RIGHT$" && baseExpr=base2 && builtin len && named len "LEN" && one unit
     | _ -> false)

let nonmutatingCall program symbol args =
    inlineCall program symbol args || nonmutatingFunction program symbol (List.length args)

/// The one-argument ROM ABS resident. Native code generation replaces each
/// x%=ABS(word) with an inline integer instruction. Any ROM call that remains
/// is still rejected by the machine-level loop planners, which never cross one.
let residentAbsCall (program:HirProgram) symbol args =
    nonmutatingFunction program symbol (List.length args) && not(inlineFunction program symbol (List.length args))

let rec expression (context: Context) scope expr =
    let children xs = xs |> List.map (expression context scope) |> combineAll
    match expr with
    | Literal(ConstString _,_,_) -> {empty with MayThrow=true;Allocates=true}
    | Literal _ | Missing _ -> empty
    | ReadVar(s,_,_) ->
        let effect=readRoot(resolve context scope s)
        match context.Aliases.Storages.TryFind s with
        | Some storage when storage.Type=String || not (storage.Name.EndsWith("%",StringComparison.Ordinal)) -> {effect with MayThrow=true;Allocates=true}
        | _ -> effect
    | DynamicReadVar(name,_,_) -> {readRoot(named context scope name) with MayThrow=true;Allocates=true}
    | ReadArrayElem(s,xs,_,_) -> combine (children xs) {readRoot(resolve context scope s) with MayThrow=true}
    | ReadStringChar(s,x,_,_) -> combine (expression context scope x) {readRoot(resolve context scope s) with MayThrow=true;Allocates=true}
    | DynamicReadArrayElem(name,xs,_,_) -> combine (children xs) {readRoot(named context scope name) with MayThrow=true}
    | DynamicReadStringChar(name,x,_,_) -> combine (expression context scope x) {readRoot(named context scope name) with MayThrow=true;Allocates=true}
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
    let element = match target with WriteArrayElem _ | DynamicWriteArrayElem _ -> true | _ -> false
    let root =
        match targetStorageRoot context.Program context.Aliases.Storages scope target with
        | Some s -> if element then write s else {write s with DescriptorWrites=Set.singleton s}
        | None -> {empty with WritesUnknown=true;DescriptorWritesUnknown=not element}
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
        | RefArg t -> combine (targetEvaluation context scope t) (readRoot(targetStorageRoot context.Program context.Aliases.Storages scope t))) |> combineAll
    let known = context.Program.Routines |> List.tryFind (fun r -> r.Symbol=symbol)
    match known,context.Summaries.TryFind symbol with
    | Some routine,Some summary ->
        let parameters = routine.Parameters |> List.map (fun p -> p.Storage.Symbol)
        let formals = Set.ofList parameters
        let locals = routine.Locals |> List.map _.Symbol |> Set.ofList
        let externalSymbols symbols = Set.difference symbols (Set.union formals locals)
        let arrayValue = function
            | ReadVar(s,hirType,_) -> match hirType with Array _ -> true | _ -> context.Aliases.ArrayStorages.Contains s
            | DynamicReadVar(name,hirType,_) ->
                match hirType with Array _ -> true | _ -> context.Aliases.Storages |> Map.exists(fun s storage -> String.Equals(storage.Name,name,StringComparison.OrdinalIgnoreCase) && context.Aliases.ArrayStorages.Contains s)
            | _ -> false
        let project symbols unknown descriptor =
            let mutable roots,unresolved=externalSymbols symbols,unknown
            for index in 0..min parameters.Length args.Length-1 do
                if symbols.Contains parameters[index] then
                    let element=match args[index] with RefArg(WriteArrayElem _ | DynamicWriteArrayElem _ | WriteStringChar _ | DynamicWriteStringChar _) -> true | _ -> false
                    match actualRoot context.Program context.Aliases.Storages context.Aliases.ArrayStorages scope args[index] with
                    | Some actual when not(descriptor && element) -> roots<-roots.Add actual
                    | Some _ -> () // A scalar reference to an element cannot resize its owner.
                    | None ->
                        match args[index] with
                        | RefArg _ -> unresolved<-true
                        | ValueArg value when arrayValue value -> unresolved<-true
                        | _ -> () // Scalar ValueArgs are private, including parenthesized variables.
            roots,unresolved
        let reads,readsUnknown=project summary.Reads summary.ReadsUnknown false
        let writes,writesUnknown=project summary.Writes summary.WritesUnknown false
        let descriptors,descriptorsUnknown=project summary.DescriptorWrites summary.DescriptorWritesUnknown true
        let loops,loopsUnknown=project summary.LoopMetadataWrites summary.LoopMetadataWritesUnknown false
        // Even a total, empty body can fail while bindings/call-frame storage
        // are created. These protocol effects belong to the call itself.
        combine evaluated {summary with Reads=reads;Writes=writes;ReadsUnknown=readsUnknown;WritesUnknown=writesUnknown
                                        DescriptorWrites=descriptors;DescriptorWritesUnknown=descriptorsUnknown;LoopMetadataWrites=loops;LoopMetadataWritesUnknown=loopsUnknown
                                        MayThrow=true;Allocates=true;ControlBarrier=true;Calls=summary.Calls.Add symbol}
    | None,_ when nonmutatingCall context.Program symbol args ->
        combine evaluated {empty with MayThrow=true;Allocates=true;Calls=Set.singleton symbol}
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
    | BuiltInCall(NamedBuiltIn name,_,_,_) when name.Equals("STOP",StringComparison.OrdinalIgnoreCase) || name.Equals("QUIT",StringComparison.OrdinalIgnoreCase) ->
        combine evaluated {empty with MayThrow=true;ControlBarrier=true}
    | BuiltInCall _ -> combine evaluated unknown // Resident extensions can mutate arbitrary storage.
    | Input(_,_,targets,_) -> combineAll [evaluated; targets |> List.map (targetWrite context scope) |> combineAll; {empty with InputOutput=true;MayThrow=true}]
    | Read(targets,_) -> combineAll [evaluated; targets |> List.map (targetWrite context scope) |> combineAll; {empty with MayThrow=true;ControlBarrier=true}]
    | Dim(s,_,_) -> combine evaluated {loopInitialisation context scope s with MayThrow=true;Allocates=true}
    | If(_,yes,no,_) -> combineAll [evaluated;nested yes;nested (Option.defaultValue [] no)]
    | For(_,s,_,_,_,body,_) | ForSequence(_,s,_,_,_,_,_,body,_) -> combineAll [evaluated;readRoot(resolve context scope s);loopInitialisation context scope s;nested body;{empty with MayThrow=true;ControlBarrier=true}]
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
            // Own LOCAL/formal bindings are projected out at a normal return.
            // Free names that could be rebound remain explicitly unknown.
            r.Symbol,combine summaries[r.Symbol] effects) |> Map.ofList
        changed <- next<>summaries
        summaries <- next
    {Program=program;Aliases=aliases;Summaries=summaries}
