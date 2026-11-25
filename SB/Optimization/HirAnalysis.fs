module HirAnalysis

// Target-aware interval analysis over the explicit CFG. Missing facts mean the
// full QL word range; None means an unreachable block, not an unknown value.
// Proofs are attached to expressions only when every occurrence is safe.
open System
open System.Collections.Generic
open System.Text
open HIR
open HirEffects
open HirControlFlow
open HirTurboIrBackend

type Interval = { Lower:int64; Upper:int64 }
type RangeState = Map<SymbolId,Interval>
type CapturedLoop = { Owner:int; Finish:Interval; Step:Interval }
type Options = { UseWideningThresholds:bool; NarrowingPasses:int }
let defaultOptions = {UseWideningThresholds=true;NarrowingPasses=8}
type private FlowState = { Ranges:RangeState; Bounds:Map<SymbolId,int list>; ExactBounds:Map<SymbolId,int list>; Loops:Map<SymbolId,CapturedLoop> }
type ScopeAnalysis = {
    Graph:Graph
    /// Only the analysis dump reads liveness, so it is computed on demand.
    Liveness:Lazy<Liveness>
    EntryRanges:Map<int,RangeState option>
    ExitRanges:Map<int,RangeState option>
    ExpressionRanges:Map<int * HirExpr,Interval option>
    EntryBounds:Map<int,Map<SymbolId,int list> option>
    EntryExactBounds:Map<int,Map<SymbolId,int list> option>
    EntryLoops:Map<int,Map<SymbolId,CapturedLoop> option>
    WideningThresholds:int64 list
    NarrowingPasses:int
    Certified:bool
    ForMetadataTrusted:bool
    TrustedForInitialisers:Set<int>
}
type Analysis = {
    Effects:Context
    Scopes:Map<Scope,ScopeAnalysis>
    SafeIntegerExpressions:Set<HirExpr>
    ArithmeticProofsEnabled:bool
    SafeArrayAccesses:Set<string * HirExpr list>
    OneDimensionalIntegerAccesses:Set<string * HirExpr list>
    TwoDimensionalIntegerAccesses:Map<string * HirExpr list,RankTwoIntegerAccess>
    EntryShapePasses:int
    EntryShapesCertified:bool
    CertifiedNativeForLoops:Map<SymbolId option * LoopId,NativeForBindings>
    CertifiedMemoryForLoops:Set<SymbolId option * LoopId>
    NativeForRejections:Map<SymbolId option * LoopId,string list>
    ProofEligibleScopes:Set<Scope>
    GlobalControlBarriers:string list
    /// Append values (the Concat of s$=s$&x) and LEN(s$) calls, keyed by HIR
    /// expression, whose DIMmed string s$ is allocated at every occurrence.
    AllocatedStringSites:Set<HirExpr>
}

let word = {Lower = -32768L; Upper=32767L}
let private fits x = x>=word.Lower && x<=word.Upper
let private bounded low high = if fits low && fits high then {Lower=low;Upper=high} else word
let private singleton value = {Lower=value;Upper=value}
let private get state symbol = state |> Map.tryFind symbol |> Option.defaultValue word
let private put symbol range state = if range=word then Map.remove symbol state else Map.add symbol range state
let private join a b = {Lower=min a.Lower b.Lower;Upper=max a.Upper b.Upper}

// A node's effects depend only on the program and the node, never on flow
// state, and a scope's declared bounds only on the scope. Solving revisits each
// node many times (worklist, certification, narrowing) and call entries
// re-solve scopes, so these are computed once per analysis.
type private RangeContext = {
    Target:TurboIrOptions; Effects:Context; Integers:Set<SymbolId>
    Evaluations:Dictionary<Scope * int,Effects>
    NodeEffects:Dictionary<Scope * int,Effects>
    /// Arrays re-DIMmed anywhere in the program.
    Dimmed:Set<SymbolId>
    InitialBounds:Dictionary<Scope,Map<SymbolId,int list>> }

let private cached (cache:Dictionary<_,_>) key compute =
    match cache.TryGetValue key with
    | true,value -> value
    | _ ->
        let value=compute ()
        cache[key]<-value
        value
let private evaluationOf context scope (node:Node) = cached context.Evaluations (scope,node.Id) (fun () -> nodeEvaluation context.Effects scope node.Kind)
let private effectsOf context scope (node:Node) = cached context.NodeEffects (scope,node.Id) (fun () -> nodeEffects context.Effects scope node.Kind)
let private expanded context scope symbols = expand context.Effects.Aliases scope symbols

let private nameIsInteger target (name:string) =
    let name=name.ToUpperInvariant()
    not (name.EndsWith "$") && (name.EndsWith "%" || target.ImplicitIntegers.Contains name)

let private constant context expr =
    let rec value = function
        | Literal(c,_,_) -> Some c
        | ReadVar(s,_,_) ->
            context.Effects.Aliases.Storages.TryFind s
            |> Option.filter (fun s -> s.Class=GlobalStorage)
            |> Option.bind (fun s -> context.Target.ManifestConstants.TryFind(s.Name.ToUpperInvariant()))
        | Unary(Identity,x,_,_) -> value x
        | Unary(Negate,x,_,_) ->
            match value x with
            | Some(ConstInt n) when n<>Int32.MinValue -> Some(ConstInt(-n))
            | Some(ConstFloat n) -> Some(ConstFloat(-n))
            | _ -> None
        | _ -> None
    value expr

let rec private integerExpression context expr =
    match constant context expr with
    | Some(ConstInt n) -> fits(int64 n)
    | Some _ -> false
    | None ->
        match expr with
        | ReadVar(s,_,_) -> context.Integers.Contains s
        | ReadArrayElem(s,_,_,_) -> context.Effects.Aliases.Storages.TryFind s |> Option.exists (fun s -> nameIsInteger context.Target s.Name)
        | DynamicReadVar(name,_,_) | DynamicReadArrayElem(name,_,_,_) -> nameIsInteger context.Target name
        | Unary((Identity|Hash|Negate|BitwiseNot),x,_,_) -> integerExpression context x
        | Binary((Add|Subtract|Multiply),a,b,_,_) -> integerExpression context a && integerExpression context b
        | Binary((IntegerDivide|Modulo|Instr|IntegerAnd|IntegerOr|IntegerXor),_,_,_,_) -> true
        // ~~ always yields an integer; its range stays the whole word.
        | Unary(IntegerNot,_,_,_) -> true
        | CallFunc(s,_,_,_) ->
            context.Effects.Program.Routines |> List.tryFind (fun r -> r.Symbol=s) |> Option.exists (fun r -> nameIsInteger context.Target r.Name)
        | _ -> false

let private floorDivide a b =
    let q,r = a/b,a%b
    if r<>0L && ((r<0L)<>(b<0L)) then q-1L else q

// The Boolean says this operation itself cannot overflow/fail on these inputs.
let rec private evaluate context state expr : Interval option * bool =
    if not (integerExpression context expr) then None,false
    else
        match constant context expr with
        | Some(ConstInt n) when fits(int64 n) -> Some(singleton(int64 n)),true
        | _ ->
            let range e = evaluate context state e |> fst |> Option.defaultValue word
            match expr with
            | ReadVar(s,_,_) -> Some(get state s),true
            | Unary((Identity|Hash),x,_,_) -> evaluate context state x
            | Unary(Negate,x,_,_) ->
                let x=range x
                let low,high = -x.Upper,-x.Lower
                Some(bounded low high),fits low && fits high
            | Unary(BitwiseNot,_,_,_) -> Some {Lower=0L;Upper=1L},true
            | Binary(op,a,b,_,_) ->
                let a,b=range a,range b
                match op with
                | Add | Subtract | Multiply ->
                    let low,high =
                        match op with
                        | Add -> a.Lower+b.Lower,a.Upper+b.Upper
                        | Subtract -> a.Lower-b.Upper,a.Upper-b.Lower
                        | _ ->
                            let values=[a.Lower*b.Lower;a.Lower*b.Upper;a.Upper*b.Lower;a.Upper*b.Upper]
                            List.min values,List.max values
                    Some(bounded low high),fits low && fits high
                | IntegerDivide | Modulo ->
                    if (b.Lower<=0L && b.Upper>=0L) || (a.Lower=word.Lower && b.Lower<= -1L && b.Upper>= -1L) then Some word,false
                    elif op=Modulo then
                        Some(if b.Lower>0L then {Lower=0L;Upper=b.Upper-1L} else {Lower=b.Lower+1L;Upper=0L}),true
                    else
                        let values=[floorDivide a.Lower b.Lower;floorDivide a.Lower b.Upper;floorDivide a.Upper b.Lower;floorDivide a.Upper b.Upper]
                        let low,high=List.min values,List.max values
                        Some(bounded low high),fits low && fits high
                | _ -> Some word,false
            | _ -> Some word,false

let private kill context scope effects state =
    if effects.WritesUnknown then Map.empty
    else
        let killed=expanded context scope effects.Writes
        state |> Map.filter (fun s _ -> not (killed.Contains s))

let private transferRanges context scope (node:Node) state (captures:Map<SymbolId,CapturedLoop>) =
    let evaluation = evaluationOf context scope node
    // All writes made while evaluating sibling expressions are killed before
    // using any sibling's facts. This over-approximates either evaluation order.
    let evaluated = kill context scope evaluation state
    let effects=effectsOf context scope node
    let after=kill context scope effects state
    let assign s value =
        if context.Integers.Contains s then put s (evaluate context evaluated value |> fst |> Option.defaultValue word) after else after
    match node.Kind with
    | Statement(Assign(WriteVar(s,_,_),value,_)) -> assign s value
    | ForInitialise(s,_,Some first) -> assign s first
    | ForInitialise(s,_,None) -> put s word after
    | ForAdvance(s,Some _) ->
        let value=get state s
        match captures.TryFind s with
        | Some capture ->
            // FOR uses ADD.W without a check. Crossing either word boundary
            // invalidates the interval instead of assuming a mathematical loop.
            put s (bounded (value.Lower+capture.Step.Lower) (value.Upper+capture.Step.Upper)) after
        | _ -> put s word after
    | ForAdvance(_,None) -> Map.empty // Later enumeration expressions may call/mutate.
    | _ -> after

let private invert = function
    | Equal -> NotEqual | NotEqual -> Equal
    | LessThan -> GreaterThanOrEqual | LessThanOrEqual -> GreaterThan
    | GreaterThan -> LessThanOrEqual | GreaterThanOrEqual -> LessThan
    | x -> x

let private reverse = function
    | LessThan -> GreaterThan | LessThanOrEqual -> GreaterThanOrEqual
    | GreaterThan -> LessThan | GreaterThanOrEqual -> LessThanOrEqual
    | x -> x

let private restrict symbol low high state =
    let old=get state symbol
    let low,high=max old.Lower low,min old.Upper high
    if low>high then None else Some(put symbol {Lower=low;Upper=high} state)

let private comparison symbol op value state =
    match op with
    | Equal -> restrict symbol value value state
    | NotEqual ->
        // Only an excluded endpoint can narrow an interval.
        let old=get state symbol
        if old=singleton value then None
        elif old.Lower=value then restrict symbol (value+1L) old.Upper state
        elif old.Upper=value then restrict symbol old.Lower (value-1L) state
        else Some state
    | LessThan -> restrict symbol word.Lower (value-1L) state
    | LessThanOrEqual -> restrict symbol word.Lower value state
    | GreaterThan -> restrict symbol (value+1L) word.Upper state
    | GreaterThanOrEqual -> restrict symbol value word.Upper state
    | _ -> Some state

let rec private refine context scope expr truth state =
    let effects=expression context.Effects scope expr
    if effects.WritesUnknown || not effects.Writes.IsEmpty then Some state
    else
        let chain first second = refine context scope first truth state |> Option.bind (refine context scope second truth)
        let compareRange symbol op expr =
            match evaluate context state expr |> fst with
            | Some range when op=Equal -> restrict symbol range.Lower range.Upper state
            | Some range when op=NotEqual && range.Lower<>range.Upper -> Some state
            | Some range ->
                let value=if op=LessThan || op=LessThanOrEqual then range.Upper else range.Lower
                comparison symbol op value state
            | None -> Some state
        match expr with
        | Binary(BitwiseAnd,a,b,_,_) when truth -> chain a b
        | Binary(BitwiseOr,a,b,_,_) when not truth -> chain a b
        | Unary(BitwiseNot,x,_,_) -> refine context scope x (not truth) state
        | ReadVar(s,_,_) when context.Integers.Contains s -> comparison s (if truth then NotEqual else Equal) 0L state
        | Literal(ConstInt n,_,_) when fits(int64 n) -> if (n<>0)=truth then Some state else None
        | Binary(op,ReadVar(s,_,_),value,_,_) when context.Integers.Contains s ->
            match constant context value with
            | Some(ConstInt n) when fits(int64 n) -> comparison s (if truth then op else invert op) (int64 n) state
            | _ -> compareRange s (if truth then op else invert op) value
        | Binary(op,value,ReadVar(s,_,_),_,_) when context.Integers.Contains s ->
            match constant context value with
            | Some(ConstInt n) when fits(int64 n) -> comparison s (if truth then reverse op else invert(reverse op)) (int64 n) state
            | _ -> compareRange s (if truth then reverse op else invert(reverse op)) value
        | _ -> Some state

let private unknownFlow = {Ranges=Map.empty;Bounds=Map.empty;ExactBounds=Map.empty;Loops=Map.empty}

let private killMetadata context scope effects state =
    let descriptors=expanded context scope effects.DescriptorWrites
    let loops=expanded context scope effects.LoopMetadataWrites
    {state with Bounds=state.Bounds |> Map.filter(fun s _ -> not effects.DescriptorWritesUnknown && not(descriptors.Contains s))
                ExactBounds=state.ExactBounds |> Map.filter(fun s _ -> not effects.DescriptorWritesUnknown && not(descriptors.Contains s))
                Loops=state.Loops |> Map.filter(fun s _ -> not effects.LoopMetadataWritesUnknown && not(loops.Contains s))}

// A string variable. DIM gives it a one-element bound, its fixed length; string
// arrays have Array storage instead.
let private dimmedString (context:RangeContext) symbol =
    context.Effects.Aliases.Storages.TryFind symbol |> Option.exists(fun storage -> storage.Type=HirType.String)

let private transfer context scope (node:Node) state =
    let evaluation=evaluationOf context scope node
    let evaluated=killMetadata context scope evaluation state
    let after=killMetadata context scope (effectsOf context scope node) evaluated
    let mutable bounds=after.Bounds
    let mutable exactBounds=after.ExactBounds
    let mutable loops=after.Loops
    let range expr=evaluate context (kill context scope evaluation state.Ranges) expr |> fst |> Option.defaultValue word
    match node.Kind with
    | Statement(Dim(symbol,dimensions,_)) ->
        let values=dimensions |> List.map range
        if not values.IsEmpty && (values |> List.forall(fun r -> r.Lower>=0L && r.Upper<=word.Upper)) then
            bounds<-bounds.Add(symbol,values |> List.map(fun r -> int r.Lower))
            if values |> List.forall(fun r -> r.Lower=r.Upper) then
                exactBounds<-exactBounds.Add(symbol,values |> List.map(fun r -> int r.Lower))
    | ForInitialise(symbol,[first;last;step],_) ->
        loops<-loops.Add(symbol,{Owner=node.Id;Finish=range last;Step=range step})
    // A DIMmed string keeps its buffer: st_str_var_1d copies the value into it,
    // truncated to the DIMmed length, and never replaces the descriptor.
    | Statement(Assign(WriteVar(symbol,_,_),_,_)) when dimmedString context symbol ->
        match evaluated.Bounds.TryFind symbol with
        | Some([_] as shape) ->
            bounds<-bounds.Add(symbol,shape)
            evaluated.ExactBounds.TryFind symbol |> Option.iter(fun exact -> exactBounds<-exactBounds.Add(symbol,exact))
        | _ -> ()
    | _ -> ()
    {Ranges=transferRanges context scope node state.Ranges evaluated.Loops;Bounds=bounds;ExactBounds=exactBounds;Loops=loops}

let private edgeState context scope edge state =
    let initial=if edge.Havoc then unknownFlow else state
    let guarded =
        match edge.Guard with
        | Unconditional -> Some initial.Ranges
        | Predicate(expr,truth) -> refine context scope expr truth initial.Ranges
        | CountedLoop(s,_,_,truth) ->
            match initial.Loops.TryFind s with
            | Some captured when context.Integers.Contains s && (captured.Step.Lower>=0L || captured.Step.Upper<0L) ->
                let ascending=captured.Step.Lower>=0L
                let op=if ascending then LessThanOrEqual else GreaterThanOrEqual
                let last=if ascending=truth then captured.Finish.Upper else captured.Finish.Lower
                comparison s (if truth then op else invert op) last initial.Ranges
            | _ -> Some initial.Ranges
    guarded |> Option.map(fun ranges ->
        let effects={empty with Writes=edge.Kill}
        let result=killMetadata context scope effects initial
        {result with Ranges=kill context scope effects ranges})

let private joinStates a b =
    match a,b with
    | None,x | x,None -> x
    | Some a,Some b ->
        Some(a |> Map.toSeq |> Seq.choose (fun (s,range) ->
            b.TryFind s |> Option.map (fun other -> s,join range other)) |> Map.ofSeq |> Map.filter (fun _ value -> value<>word))

let private joinFlows a b =
    match a,b with
    | None,x | x,None -> x
    | Some a,Some b ->
        let shared (first:Map<_,_>) (second:Map<_,_>) combine = first |> Map.toSeq |> Seq.choose(fun (key,value) -> second.TryFind key |> Option.bind(fun other -> combine value other |> Option.map(fun joined -> key,joined))) |> Map.ofSeq
        Some {Ranges=(joinStates (Some a.Ranges) (Some b.Ranges)).Value
              Bounds=shared a.Bounds b.Bounds (fun a b -> if List.length a=List.length b then Some(List.map2 min a b) else None)
              ExactBounds=shared a.ExactBounds b.ExactBounds (fun a b -> if a=b then Some a else None)
              Loops=shared a.Loops b.Loops (fun a b -> if a.Owner=b.Owner then Some {Owner=a.Owner;Finish=join a.Finish b.Finish;Step=join a.Step b.Step} else None)}

let private thresholds context graph =
    let mandatory=[word.Lower;word.Upper;-1L;0L;1L]
    let values =
        seq {
            yield! mandatory
            for KeyValue(_,node) in graph.Nodes do
                for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions do
                    match constant context expr with
                    | Some(ConstInt n) when fits(int64 n) ->
                        for delta in [-1L;0L;1L] do if fits(int64 n+delta) then yield int64 n+delta
                    | _ -> () }
        |> Set.ofSeq |> Set.toArray
    // Keep compile time bounded for generated programs with thousands of
    // distinct literals. Dropping a threshold affects precision only.
    let selected=if values.Length<=256 then values else [|for i in 0..255 -> values[i*(values.Length-1)/255]|]
    Seq.append selected mandatory |> Set.ofSeq |> Set.toList

let private widen limits previous next =
    match previous,next with
    | Some previous,Some next ->
        let interval old value =
            {Lower=if value.Lower<old.Lower then limits |> List.filter(fun n -> n<=value.Lower) |> List.last else value.Lower
             Upper=if value.Upper>old.Upper then limits |> List.find(fun n -> n>=value.Upper) else value.Upper}
        Some {next with
                Ranges=next.Ranges |> Map.map(fun s value -> interval (get previous.Ranges s) value) |> Map.filter(fun _ value -> value<>word)
                Loops=next.Loops |> Map.map(fun s value ->
                    match previous.Loops.TryFind s with
                    | Some old -> {value with Finish=interval old.Finish value.Finish;Step=interval old.Step value.Step}
                    | None -> value)}
    | _ -> next

let private covers old candidate =
    let interval a b=a.Lower<=b.Lower && a.Upper>=b.Upper
    match old,candidate with
    | _,None -> true
    | None,Some _ -> false
    | Some old,Some candidate ->
        (old.Ranges |> Map.forall(fun s range -> interval range (get candidate.Ranges s))) &&
        (old.Bounds |> Map.forall(fun s bounds -> candidate.Bounds.TryFind s |> Option.exists(fun actual -> actual.Length=bounds.Length && List.forall2 (<=) bounds actual))) &&
        (old.ExactBounds |> Map.forall(fun s bounds -> candidate.ExactBounds.TryFind s = Some bounds)) &&
        (old.Loops |> Map.forall(fun s loop -> candidate.Loops.TryFind s |> Option.exists(fun actual -> loop.Owner=actual.Owner && interval loop.Finish actual.Finish && interval loop.Step actual.Step)))

// Arrays this scope declares with constant bounds and never re-DIMs anywhere.
let private declaredBounds context scope =
    let program=context.Effects.Program
    let routineName=match scope with RoutineScope r -> program.Routines |> List.tryFind(fun routine -> routine.Symbol=r) |> Option.map _.Name | MainScope -> None
    context.Effects.Aliases.Storages |> Map.toSeq |> Seq.choose(fun (s,storage) ->
        let local=match scope,storage.Class with MainScope,GlobalStorage -> true | RoutineScope _,RoutineLocalStorage name -> routineName=Some name | _ -> false
        match storage.Type,storage.Dimensions with
        | Array _,Some dims when local && not(context.Dimmed.Contains s) && not dims.IsEmpty && List.forall(fun n -> n>=0 && n<=32767) dims -> Some(s,dims)
        | _ -> None) |> Map.ofSeq

let private solve options context graph globalEntry =
    let incoming=Dictionary<int,FlowState option>()
    let outgoing=Dictionary<int,FlowState option>()
    let changes=Dictionary<int,int>()
    for KeyValue(id,_) in graph.Nodes do incoming[id]<-None;outgoing[id]<-None;changes[id]<-0
    let initialBounds = cached context.InitialBounds graph.Scope (fun () -> declaredBounds context graph.Scope)
    let addFacts initial facts = Map.fold(fun state symbol shape -> Map.add symbol shape state) initial facts
    // An unreachable entry (None) leaves every node unreachable: its call sites
    // then contribute nothing to the call-entry equations.
    // Entry ranges are only ever value-parameter facts imported from call sites.
    let initial=globalEntry |> Option.map(fun (globalEntry:FlowState) ->
        {unknownFlow with Ranges=globalEntry.Ranges;Bounds=addFacts initialBounds globalEntry.Bounds;ExactBounds=addFacts initialBounds globalEntry.ExactBounds})
    incoming[graph.Entry]<-initial
    let edges=graph.Edges |> List.groupBy _.Source |> Map.ofList
    let predecessors=graph.Edges |> List.groupBy _.Target |> Map.ofList
    let limits=if options.UseWideningThresholds then thresholds context graph else [word.Lower;word.Upper]
    // Widen at cycle cut points, not at every block: widening after a guard
    // would immediately destroy the bounds established by that guard. DFS back
    // edge targets also cover irreducible cycles made by numbered GO TOs.
    let widenAt=HashSet<int>(graph.Loops |> Seq.map _.Header)
    let colors=Dictionary<int,int>()
    for KeyValue(id,_) in graph.Nodes do colors[id]<-0
    let search=Stack<int*bool>()
    search.Push(graph.Entry,false)
    while search.Count>0 do
        let id,finished=search.Pop()
        if finished then colors[id]<-2
        elif colors[id]=1 then widenAt.Add id |> ignore
        elif colors[id]=0 then
            colors[id]<-1
            search.Push(id,true)
            for edge in edges.TryFind id |> Option.defaultValue [] do
                if colors[edge.Target]=1 then widenAt.Add edge.Target |> ignore
                elif colors[edge.Target]=0 then search.Push(edge.Target,false)
    let queue=Queue<int>()
    let queued=HashSet<int>()
    let enqueue id = if queued.Add id then queue.Enqueue id
    enqueue graph.Entry
    while queue.Count>0 do
        let id=queue.Dequeue()
        queued.Remove id |> ignore
        let after=incoming[id] |> Option.map (transfer context graph.Scope graph.Nodes[id])
        outgoing[id]<-after
        for edge in edges.TryFind id |> Option.defaultValue [] do
            let candidate=after |> Option.bind (edgeState context graph.Scope edge)
            let next=joinFlows incoming[edge.Target] candidate
            let next=if widenAt.Contains edge.Target && changes[edge.Target]>=3 then widen limits incoming[edge.Target] next else next
            if next<>incoming[edge.Target] then
                incoming[edge.Target]<-next; changes[edge.Target]<-changes[edge.Target]+1
                enqueue edge.Target
    let equation id =
        predecessors.TryFind id |> Option.defaultValue [] |> List.fold(fun result edge ->
            let candidate=incoming[edge.Source] |> Option.map(transfer context graph.Scope graph.Nodes[edge.Source]) |> Option.bind(edgeState context graph.Scope edge)
            joinFlows result candidate) (if id=graph.Entry then initial else None)
    let mutable certified=graph.Nodes |> Map.forall(fun id _ -> covers incoming[id] (equation id))
    let mutable passes=0
    let mutable changed=true
    while certified && changed && passes<max 0 options.NarrowingPasses do
        changed<-false
        passes<-passes+1
        for id in graph.ReversePostOrder do
            let candidate=equation id
            if not(covers incoming[id] candidate) then certified<-false
            elif candidate<>incoming[id] then incoming[id]<-candidate;changed<-true
    certified<-certified && (graph.Nodes |> Map.forall(fun id _ -> covers incoming[id] (equation id)))
    for KeyValue(id,node) in graph.Nodes do outgoing[id]<-incoming[id] |> Option.map(transfer context graph.Scope node)
    let entryRanges=incoming |> Seq.map (fun p -> p.Key,p.Value |> Option.map _.Ranges) |> Map.ofSeq
    let exitRanges=outgoing |> Seq.map (fun p -> p.Key,p.Value |> Option.map _.Ranges) |> Map.ofSeq
    let expressions = [
        for KeyValue(id,node) in graph.Nodes do
            let state=entryRanges[id] |> Option.map (kill context graph.Scope (evaluationOf context graph.Scope node))
            for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions do
                yield (id,expr), state |> Option.bind (fun state -> evaluate context state expr |> fst)] |> Map.ofList
    // A jump into a different FOR body can carry another loop's hidden NEXT
    // address, even when both use the same counter vector. Require the matching
    // initializer to dominate checks/advances before trusting runtime control.
    let metadataValid id (node:Node) =
        match node.Kind with
        | ForCheck(counter,_,_) ->
            let predecessors=predecessors.TryFind id |> Option.defaultValue []
            match predecessors |> List.tryFind(fun edge -> match graph.Nodes[edge.Source].Kind with ForInitialise _ -> true | _ -> false) with
            | Some init ->
                id :: (predecessors |> List.choose(fun edge -> match graph.Nodes[edge.Source].Kind with ForAdvance _ -> Some edge.Source | _ -> None))
                |> List.forall(fun target ->
                    match incoming[target] with
                    | None -> true
                    | Some state -> graph.Dominators[target].Contains init.Source &&
                                    (state.Loops.TryFind counter |> Option.exists(fun captured -> captured.Owner=init.Source)))
            | None -> false
        | _ -> true
    let metadataTrusted=graph.Nodes |> Map.forall metadataValid
    let trustedInitialisers=graph.ForRegions |> Map.toSeq |> Seq.choose(fun (_,region) ->
        if metadataValid region.Check graph.Nodes[region.Check] then Some region.Initialiser else None) |> Set.ofSeq
    {Graph=graph;Liveness=lazy(liveness context.Effects graph);EntryRanges=entryRanges;ExitRanges=exitRanges;ExpressionRanges=expressions
     EntryBounds=incoming |> Seq.map(fun p -> p.Key,p.Value |> Option.map _.Bounds) |> Map.ofSeq
     EntryExactBounds=incoming |> Seq.map(fun p -> p.Key,p.Value |> Option.map _.ExactBounds) |> Map.ofSeq
     EntryLoops=incoming |> Seq.map(fun p -> p.Key,p.Value |> Option.map _.Loops) |> Map.ofSeq
     WideningThresholds=limits;NarrowingPasses=passes;Certified=certified;ForMetadataTrusted=metadataTrusted;TrustedForInitialisers=trustedInitialisers}

// Unknown/nonlocal entries and persistent error/memory operations can affect
// other procedures. Ordinary local CFG uncertainty is deliberately not global.
let private controlBarriers (context:RangeContext) (graphs:(Scope*Graph) list) =
    let known=context.Effects.Program.Routines |> List.map _.Symbol |> Set.ofList
    [for scope,graph in graphs do
        for KeyValue(_,node) in graph.Nodes do
            let localTarget = function Literal(ConstInt line,_,_) -> graph.Labels.ContainsKey line | _ -> false
            match node.Kind with
            | Statement(WhenError _) -> yield "error handler may resume in another region"
            | Statement(Goto(target,_) | Gosub(target,_)) when not(localTarget target) -> yield "computed or nonlocal line transfer"
            | Statement(OnGoto(_,targets,_) | OnGosub(_,targets,_)) when not(List.forall localTarget targets) -> yield "computed or nonlocal ON transfer"
            | Statement(BuiltInCall(NamedBuiltIn name,_,_,_)) when not(List.contains (name.ToUpperInvariant()) ["STOP";"QUIT"]) ->
                yield $"opaque/persistent resident {name.ToUpperInvariant()}"
            | _ -> ()
            for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions do
                match expr with
                | CallFunc(s,args,_,_) when not(known.Contains s) && not(nonmutatingCall context.Effects.Program s args) ->
                    yield "opaque function may alter bindings or continuations"
                | _ -> ()
            match node.Kind with
            | Statement(ProcCall(s,_,_,_)) when not(known.Contains s) -> yield "opaque procedure may alter bindings or continuations"
            | _ -> ()]
    |> List.distinct

let private scopeTrusted barriers (data:ScopeAnalysis) =
    List.isEmpty barriers && data.Certified && not data.Graph.HasUncertainControl && data.ForMetadataTrusted

// Incoming shapes are MUST facts from every statically known call site. Integer
// value parameters also receive the interval of their argument; no other scalar
// values, caller LOCALs or FOR captures cross the boundary.
// This is a least fixed point: a routine with static call sites starts
// unreachable and gains states only from reachable sites, so entry states only
// ever grow. A recursive site therefore contributes exactly what holds on the
// paths back to it from an entry, rather than blocking every imported fact.
// Main, uncalled routines and routines whose callers all stay unreachable use
// the unknown entry. The final solution must satisfy every call-entry equation.
let private solveCallEntries options context barriers (graphs:(Scope*Graph) list) =
    let globalArrays =
        context.Effects.Program.Globals
        |> List.choose(fun storage ->
            let sameName=context.Effects.Aliases.Storages |> Map.toList |> List.filter(fun (_,other) -> String.Equals(storage.Name,other.Name,StringComparison.OrdinalIgnoreCase))
            match storage.Type,sameName with
            | Array _,[(symbol,_)] when symbol=storage.Symbol -> Some symbol
            | _ -> None)
        |> Set.ofList
    let original=graphs |> List.map(fun (scope,graph) -> scope,solve options context graph (Some unknownFlow)) |> Map.ofList
    let callees (node:Node) = seq {
        match node.Kind with Statement(ProcCall(s,_,_,_)) -> yield s | _ -> ()
        for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions do
            match expr with CallFunc(s,_,_,_) -> yield s | _ -> () }
    let called=graphs |> Seq.collect(fun (_,graph) -> graph.Nodes |> Seq.collect(fun p -> callees p.Value)) |> Seq.map RoutineScope |> Set.ofSeq
    let routines=context.Effects.Program.Routines |> List.map(fun r -> r.Symbol,r) |> Map.ofList
    // An integer formal bound to an integer value argument starts with that
    // argument's range. References (bare variables) may alias the actual and
    // pass nothing; missing or extra arguments pass nothing either.
    let parameterRanges callee args state =
        match routines.TryFind callee with
        | None -> Map.empty
        | Some routine ->
            Seq.zip routine.Parameters args |> Seq.choose(fun (formal,arg) ->
                match arg with
                | ValueArg expr when formal.Binding<>ReferenceBinding && context.Integers.Contains formal.Storage.Symbol ->
                    evaluate context state expr |> fst |> Option.filter(fun range -> range<>word) |> Option.map(fun range -> formal.Storage.Symbol,range)
                | _ -> None) |> Map.ofSeq
    let calls (node:Node) = seq {
        match node.Kind with Statement(ProcCall(s,_,args,_)) -> yield s,args | _ -> ()
        for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions do
            match expr with CallFunc(s,args,_,_) -> yield s,args | _ -> () }
    let infer (forced:Set<Scope>) (scopes:Map<Scope,ScopeAnalysis>) =
        let inputs=Dictionary<Scope,FlowState option>()
        for scope,_ in graphs do inputs[scope]<-None
        for scope,graph in graphs do
            let data=scopes[scope]
            for KeyValue(id,node) in graph.Nodes do
                match data.EntryBounds[id],data.EntryExactBounds[id],data.EntryRanges[id] with
                | _ when not(scopeTrusted barriers data) ->
                    // An untrusted caller contributes UNKNOWN, never an omitted
                    // call site or facts derived from incomplete continuations.
                    for callee in callees node do
                        let target=RoutineScope callee
                        if inputs.ContainsKey target then inputs[target]<-Some unknownFlow
                | Some bounds,Some exact,Some ranges ->
                    // Arguments and sibling calls may change descriptors before
                    // entry. For expression calls this also includes the callee's
                    // MAY effects, deliberately withholding some safe facts.
                    let evaluation=evaluationOf context scope node
                    let before={unknownFlow with Bounds=bounds;ExactBounds=exact}
                    let evaluated=killMetadata context scope evaluation before
                    let passed={unknownFlow with Bounds=evaluated.Bounds |> Map.filter(fun s _ -> globalArrays.Contains s)
                                                 ExactBounds=evaluated.ExactBounds |> Map.filter(fun s _ -> globalArrays.Contains s)}
                    // Argument values use the node's entry state only when no
                    // evaluation in the node can write storage first.
                    let valuesKnown=not evaluation.WritesUnknown && evaluation.Writes.IsEmpty
                    for callee,args in calls node do
                        let target=RoutineScope callee
                        if inputs.ContainsKey target then
                            let incoming={passed with Ranges=if valuesKnown then parameterRanges callee args ranges else Map.empty}
                            inputs[target]<-joinFlows inputs[target] (Some incoming)
                | _ -> () // Unreachable sites contribute no entry states.
        graphs |> List.map(fun (scope,_) -> scope,if forced.Contains scope then Some unknownFlow else inputs[scope]) |> Map.ofList
    let hasParameters=routines |> Map.exists(fun _ routine -> not routine.Parameters.IsEmpty)
    if (globalArrays.IsEmpty && not hasParameters) || graphs.Length=1 || not barriers.IsEmpty then original,0,true
    else
        let unknown=Some unknownFlow
        let limits=graphs |> List.map(fun (scope,graph) -> scope,(if options.UseWideningThresholds then thresholds context graph else [word.Lower;word.Upper])) |> Map.ofList
        let mutable forced=graphs |> List.map fst |> List.filter(fun scope -> scope=MainScope || not(called.Contains scope)) |> Set.ofList
        let mutable entries=graphs |> List.map(fun (scope,_) -> scope,if forced.Contains scope then unknown else None) |> Map.ofList
        let mutable scopes=graphs |> List.map(fun (scope,graph) -> scope,if entries[scope]=unknown then original[scope] else solve options context graph entries[scope]) |> Map.ofList
        let mutable changes=graphs |> List.map(fun (scope,_) -> scope,0) |> Map.ofList
        let mutable passes=0
        let mutable stable=false
        let mutable certified=scopes |> Map.forall(fun scope data -> entries[scope]=unknown || data.Certified)
        // Bound compile time for unusually deep call graphs. Failure to reach
        // a certified fixed point discards all imported facts, not any checks.
        // Widening needs a few extra passes for each recursive parameter.
        let limit=min 64 (2*graphs.Length+6)
        while not stable && certified && passes<limit do
            passes<-passes+1
            let inferred=infer forced scopes
            let next =
                if inferred |> Map.forall(fun scope facts -> covers entries[scope] facts) then
                    // Callers that stay unreachable cannot certify an entry:
                    // analyze those routines from the unknown entry instead.
                    let dead=entries |> Map.filter(fun _ facts -> facts.IsNone) |> Map.keys |> Set.ofSeq
                    if dead.IsEmpty then stable<-true
                    forced<-Set.union forced dead
                    entries |> Map.map(fun scope facts -> if dead.Contains scope then unknown else facts)
                else
                    // Entries only grow: join the new call-site states into them.
                    // Recursion can grow a parameter one step per pass, so a
                    // repeatedly changing entry widens to the callee's thresholds.
                    entries |> Map.map(fun scope previous ->
                        let joined=joinFlows previous inferred[scope]
                        if changes[scope]>=2 then widen limits[scope] previous joined else joined)
            if not stable then
                for scope,graph in graphs do
                    if next[scope]<>entries[scope] then
                        scopes<-scopes.Add(scope,solve options context graph next[scope])
                        changes<-changes.Add(scope,changes[scope]+1)
                entries<-next
                certified<-scopes |> Map.forall(fun scope data -> entries[scope]=unknown || data.Certified)
        // A post-fixpoint: every call site's state is covered by its entry.
        let finalEntries=entries
        if stable && certified && (infer forced scopes |> Map.forall(fun scope facts -> covers finalEntries[scope] facts)) then scopes,passes,true
        else original,passes,false

let private arithmetic = function Unary(Negate,_,_,_) | Binary((Add|Subtract|Multiply),_,_,_,_) -> true | _ -> false

/// Assignments and PRINTs ending in STOP: a branch that ends the program, so
/// never returns to an enclosing loop. Its effects are still those of the body.
let private terminalBranch (block:HirBlock) =
    match block |> List.rev |> List.skipWhile(function LineNumber _ | Remark _ -> true | _ -> false) with
    | BuiltInCall(NamedBuiltIn name,None,_,_)::rest when name.Equals("STOP",StringComparison.OrdinalIgnoreCase) ->
        rest |> List.forall(function Assign _ | BuiltInCall(Print,_,_,_) | LineNumber _ | Remark _ -> true | _ -> false)
    | _ -> false

// Binding certificates are independent of scalar range proofs. Keep counters
// in their own cells: a formal/array alias must not observe a deferred counter
// store, even when it only reads. Calls and non-leaf control remain boundaries.
let private nativeForCertificates (context:RangeContext) barriers (scopes:Map<Scope,ScopeAnalysis>) =
    let effects=context.Effects
    let certificate scope routine = function
        | For(loop,counter,_,_,_,body,_) ->
            let data=scopes[scope]
            let owned =
                effects.Aliases.Storages.TryFind counter |> Option.exists(fun storage ->
                    let scalar=not(effects.Aliases.ArrayStorages.Contains counter) && nameIsInteger context.Target storage.Name
                    let binding=
                        match storage.Class,routine with
                        | RoutineLocalStorage _,Some routine -> routine.Locals |> List.exists(fun local -> local.Symbol=counter)
                        | GlobalStorage,_ ->
                            effects.Aliases.Storages |> Map.toSeq |> Seq.filter(fun (_,other) -> String.Equals(storage.Name,other.Name,StringComparison.OrdinalIgnoreCase)) |> Seq.length = 1
                        | _ -> false
                    scalar && binding)
            // EXIT of this loop leaves to its own end with the counter intact;
            // it neither enters the region nor touches FOR metadata. A branch
            // that ends in STOP never returns to the loop; its statements still
            // count in the effects below.
            let rec leaf body=body |> List.forall(function
                | Assign _ | LineNumber _ | Remark _ -> true
                | Exit(exited,_) -> exited=loop
                | If(_,yes,no,_) -> branch yes && branch(Option.defaultValue [] no)
                | _ -> false)
            and branch block=leaf block || terminalBranch block
            let writes=body |> List.map(statement effects scope) |> combineAll
            let roots=Set.union writes.Reads writes.Writes
            let independent=roots |> Set.forall(fun root -> root=counter || not(mayAlias effects.Aliases scope counter root))
            let region=data.Graph.ForRegions.TryFind loop
            let control=region |> Option.exists(fun region ->
                data.TrustedForInitialisers.Contains region.Initialiser &&
                data.Graph.Edges |> List.forall(fun edge ->
                    not(region.Members.Contains edge.Target) || region.Members.Contains edge.Source || edge.Source=region.Initialiser))
            let reasons=[
                yield! barriers
                if not data.Certified then yield "scope analysis did not certify a fixed point"
                if not control then yield "loop has an external entry or untrusted FOR continuation"
                if not owned then yield "counter is not an owned integer LOCAL or unique global"
                if not(leaf body) then yield "body contains calls, nested loops or unsupported control"
                if not independent then yield "body storage may alias the counter"
                if effects.Aliases.Escaped.Contains counter then yield "counter reference escapes to an opaque callee"
                if writes.ReadsUnknown || writes.WritesUnknown then yield "body has unresolved storage effects"
                // ROM maths residents are nonmutating. Their binding effects are
                // already in `writes`; machine planners audit any ROM call left.
                if statements body |> Seq.collect statementExpressions |> Seq.collect expressions |> Seq.exists(function
                    | CallFunc(symbol,args,_,_) -> not(inlineCall effects.Program symbol args || residentMathsCall effects.Program symbol args)
                    | _ -> false) then yield "body evaluates a function or procedure call"
                if writes.Writes.Contains counter then yield "body writes the counter"
                if writes.LoopMetadataWritesUnknown || not writes.LoopMetadataWrites.IsEmpty then yield "body may replace FOR metadata"]
            let key=(routine |> Option.map _.Symbol,loop)
            if reasons.IsEmpty then
                let scalars=roots.Add counter |> Set.filter(fun s -> not(effects.Aliases.ArrayStorages.Contains s))
                let stable changed s = not(effects.Aliases.Escaped.Contains s) &&
                                       (changed |> Set.forall(fun w -> not(mayAlias effects.Aliases scope s w)))
                let invariants=scalars.Remove counter |> Set.filter(stable (writes.Writes.Add counter))
                let arrays=Set.intersect roots effects.Aliases.ArrayStorages |> Set.filter(fun s ->
                    not writes.DescriptorWritesUnknown && stable writes.DescriptorWrites s)
                let names symbols=symbols |> Set.map(fun s -> effects.Aliases.Storages[s].Name)
                Some(key,Ok {Scalars=names scalars;InvariantScalars=names invariants;InvariantArrays=names arrays})
            else Some(key,Error reasons)
        | _ -> None
    [yield! statements effects.Program.Main |> Seq.choose(certificate MainScope None)
     for routine in effects.Program.Routines do
         yield! statements routine.Body |> Seq.choose(certificate (RoutineScope routine.Symbol) (Some routine))]
    |> List.fold(fun (certificates,rejections) (key,result) ->
        match result with Ok names -> Map.add key names certificates,rejections | Error reasons -> certificates,Map.add key reasons rejections) (Map.empty,Map.empty)

// Unlike pinned loops, memory-counter loops permit scalar aliases and writes:
// every operation observes the actual cell, as in P mode. Only binding/FOR
// metadata must survive calls. LOCAL effects of recursive activations are
// projected out by the existing interprocedural summaries at normal returns.
let private memoryForCertificates (context:RangeContext) (barriers:string list) (scopes:Map<Scope,ScopeAnalysis>) =
    let effects=context.Effects
    let routines=effects.Program.Routines |> List.map(fun r -> r.Symbol,r) |> Map.ofList
    let candidate scope routine = function
        | For(loop,counter,_,_,_,body,_) ->
            let data=scopes[scope]
            let mutable calls=0
            let rec leaf body = body |> List.forall(function
                | Assign _ | LineNumber _ | Remark _ -> true
                | If(_,yes,no,_) -> leaf yes && leaf(Option.defaultValue [] no)
                | ProcCall(symbol,None,args,_) ->
                    calls<-calls+1
                    routines.TryFind symbol |> Option.exists(fun callee ->
                        callee.ReturnType.IsNone &&
                        (callee.Parameters |> List.forall(fun p -> p.Binding<>ReferenceBinding && nameIsInteger context.Target p.Storage.Name && not(effects.Aliases.ArrayStorages.Contains p.Storage.Symbol))) &&
                        (args |> List.forall(function ValueArg _ -> true | _ -> false)))
                | _ -> false)
            let supported=leaf body && calls>0
            let owned=effects.Aliases.Storages.TryFind counter |> Option.exists(fun storage ->
                nameIsInteger context.Target storage.Name && not(effects.Aliases.ArrayStorages.Contains counter) &&
                match storage.Class,routine with
                | RoutineLocalStorage _,Some routine -> routine.Locals |> List.exists(fun local -> local.Symbol=counter)
                | GlobalStorage,_ -> effects.Aliases.Storages |> Map.forall(fun s other -> s=counter || not(String.Equals(storage.Name,other.Name,StringComparison.OrdinalIgnoreCase)))
                | _ -> false)
            let writes=body |> List.map(statement effects scope) |> combineAll
            let control=data.Graph.ForRegions.TryFind loop |> Option.exists(fun region ->
                data.TrustedForInitialisers.Contains region.Initialiser &&
                data.Graph.Edges |> List.forall(fun edge -> not(region.Members.Contains edge.Target) || region.Members.Contains edge.Source || edge.Source=region.Initialiser))
            let expressionsKnown=statements body |> Seq.collect statementExpressions |> Seq.collect expressions |> Seq.forall(function
                | CallFunc(symbol,args,_,_) -> inlineCall effects.Program symbol args
                | _ -> true)
            if barriers.IsEmpty && data.Certified && supported && owned && control && expressionsKnown &&
               not(effects.Aliases.Escaped.Contains counter) && not writes.ReadsUnknown && not writes.WritesUnknown &&
               not writes.InputOutput && not writes.LoopMetadataWritesUnknown &&
               not((expand effects.Aliases scope writes.LoopMetadataWrites).Contains counter) then
                Some(routine |> Option.map _.Symbol,loop)
            else None
        | _ -> None
    [yield! statements effects.Program.Main |> Seq.choose(candidate MainScope None)
     for routine in effects.Program.Routines do
         yield! statements routine.Body |> Seq.choose(candidate (RoutineScope routine.Symbol) (Some routine))]
    |> Set.ofList

/// Analyzes immutable HIR. No facts are assumptions about caller values, QL
/// startup zeroes, floating arithmetic, or successful execution of a call.
let analyzeWithOptions options target program =
    let effects=HirEffects.analyze program
    let integers=effects.Aliases.Storages |> Map.toSeq |> Seq.choose (fun (symbol,storage) ->
        if storage.Dimensions.IsNone && (match storage.Type with Array _ -> false | _ -> true) && nameIsInteger target storage.Name then Some symbol else None) |> Set.ofSeq
    let context={Target=target;Effects=effects;Integers=integers;Evaluations=Dictionary();NodeEffects=Dictionary();InitialBounds=Dictionary()
                 Dimmed=HirEffects.statements program.Main |> Seq.append(program.Routines |> Seq.collect(fun r -> HirEffects.statements r.Body)) |> Seq.choose(function Dim(s,_,_) -> Some s | _ -> None) |> Set.ofSeq}
    let graphs = (MainScope,build effects MainScope program.Main) :: (program.Routines |> List.map (fun r -> RoutineScope r.Symbol,build effects (RoutineScope r.Symbol) r.Body))
    let barriers=controlBarriers context graphs
    let scopes,entryShapePasses,entryShapesCertified=solveCallEntries options context barriers graphs
    let trustedScopes=scopes |> Map.toSeq |> Seq.choose(fun (scope,data) -> if scopeTrusted barriers data then Some scope else None) |> Set.ofSeq
    let certificates,rejections=nativeForCertificates context barriers scopes
    let memoryCertificates=memoryForCertificates context barriers scopes
    let candidates = [
        for KeyValue(scope,analysis) in scopes do
            for KeyValue(id,node) in analysis.Graph.Nodes do
                let evaluation=evaluationOf context scope node
                let state=analysis.EntryRanges[id] |> Option.map (kill context scope evaluation)
                for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions |> Seq.filter arithmetic do
                    let proven=
                        trustedScopes.Contains scope && not evaluation.WritesUnknown && evaluation.Writes.IsEmpty &&
                        (state |> Option.exists (fun state -> integerExpression context expr && (evaluate context state expr |> snd)))
                    yield expr,proven]
    let safe=candidates |> List.groupBy fst |> List.choose (fun (expr,occurrences) -> if occurrences |> List.forall snd then Some expr else None) |> Set.ofList
    let normalize (name:string)=name.Trim().ToUpperInvariant()
    let arrayCandidates=[
        for KeyValue(scope,analysis) in scopes do
            let visible=effects.Aliases.Visible.TryFind scope |> Option.defaultValue Set.empty
            let named name=visible |> Seq.filter(fun s -> effects.Aliases.Storages.TryFind s |> Option.exists(fun storage -> normalize storage.Name=name)) |> Seq.toList
            let access symbol indexes =
                effects.Aliases.Storages.TryFind symbol |> Option.map(fun storage -> normalize storage.Name,indexes,Some symbol)
            for KeyValue(id,node) in analysis.Graph.Nodes do
                let evaluation=evaluationOf context scope node
                let state=analysis.EntryRanges[id] |> Option.map(kill context scope evaluation)
                let bounds=analysis.EntryBounds[id]
                let exactBounds=analysis.EntryExactBounds[id]
                let reads=nodeExpressions node.Kind |> Seq.collect HirEffects.expressions |> Seq.choose(function
                    | ReadArrayElem(s,indexes,_,_) -> access s indexes
                    | DynamicReadArrayElem(name,indexes,_,_) ->
                        let name=normalize name
                        Some(name,indexes,(match named name with [symbol] -> Some symbol | _ -> None))
                    | _ -> None)
                let writeAccess = function
                    | WriteArrayElem(s,indexes,_,_) -> access s indexes
                    | DynamicWriteArrayElem(name,indexes,_,_) ->
                        let name=normalize name
                        Some(name,indexes,(match named name with [symbol] -> Some symbol | _ -> None))
                    | _ -> None
                let writes,ordinary =
                    match node.Kind with
                    | Statement(Assign(target,_,_)) -> writeAccess target |> Option.toList,true
                    | Statement(Input(_,_,targets,_)) | Statement(Read(targets,_)) -> List.choose writeAccess targets,false
                    | _ -> [],false
                let occurrences=Seq.append (reads |> Seq.map(fun (a,b,c) -> a,b,c,true)) (writes |> Seq.map(fun (a,b,c) -> a,b,c,ordinary))
                for name,indexes,symbol,eligible in occurrences do
                    let rankOne=
                        eligible && trustedScopes.Contains scope && nameIsInteger target name && indexes.Length=1 &&
                        not evaluation.WritesUnknown && evaluation.Writes.IsEmpty &&
                        (match symbol,bounds with
                         | Some symbol,Some bounds when named name=[symbol] -> bounds.TryFind symbol |> Option.exists(fun dimensions -> dimensions.Length=1)
                         | _ -> false)
                    let proven=
                        eligible && trustedScopes.Contains scope && not(name.EndsWith "$" ) && not evaluation.WritesUnknown && evaluation.Writes.IsEmpty &&
                        (match symbol,state,bounds with
                         | Some symbol,Some state,Some bounds when named name=[symbol] ->
                             bounds.TryFind symbol |> Option.exists(fun dimensions ->
                                 dimensions.Length=indexes.Length && List.forall2(fun index bound ->
                                     evaluate context state index |> fst |> Option.exists(fun range -> range.Lower>=0L && range.Upper<=int64 bound)) indexes dimensions)
                         | _ -> false)
                    let rankTwo=
                        if eligible && trustedScopes.Contains scope && nameIsInteger target name && indexes.Length=2 &&
                           not evaluation.WritesUnknown && evaluation.Writes.IsEmpty then
                            match symbol,bounds with
                            | Some symbol,Some bounds when named name=[symbol] ->
                                match bounds.TryFind symbol with
                                | Some [row;column] ->
                                    let safe index bound=state |> Option.bind(fun state -> evaluate context state index |> fst) |> Option.exists(fun range -> range.Lower>=0L && range.Upper<=int64 bound)
                                    let exact=exactBounds |> Option.bind(Map.tryFind symbol) |> Option.bind(function [a;b] -> Some(a+1,b+1) | _ -> None)
                                    Some {ProvenRows=safe indexes[0] row;ProvenColumns=safe indexes[1] column;ExactLengths=exact}
                                | _ -> None
                            | _ -> None
                        else None
                    yield (name,indexes),proven,rankOne,rankTwo]
    let accesses proof = arrayCandidates |> List.groupBy(fun (key,_,_,_) -> key) |> List.choose(fun (key,occurrences) -> if occurrences |> List.forall proof then Some key else None) |> Set.ofList
    let safeArrays=accesses(fun (_,proven,_,_) -> proven)
    let rankOneArrays=accesses(fun (_,_,rankOne,_) -> rankOne)
    let rankTwoArrays=arrayCandidates |> List.groupBy(fun (key,_,_,_) -> key) |> List.choose(fun (key,occurrences) ->
        let facts=occurrences |> List.choose(fun (_,_,_,fact) -> fact)
        if facts.Length<>occurrences.Length then None
        else
            let exact=facts.Head.ExactLengths
            Some(key,{ProvenRows=facts |> List.forall _.ProvenRows;ProvenColumns=facts |> List.forall _.ProvenColumns
                      ExactLengths=if facts |> List.forall(fun fact -> fact.ExactLengths=exact) then exact else None})) |> Map.ofList
    // Native append (374) and LEN (375) omit P mode's missing-string checks.
    // A site qualifies when its string's DIM reaches it on every path. The
    // name must be unique in the program: no LOCAL or formal can rebind it,
    // so a dynamic reference in a routine is that DIMmed string.
    let uniqueStrings =
        effects.Aliases.Storages |> Map.toSeq |> Seq.groupBy(fun (_,storage) -> normalize storage.Name) |> Seq.choose(fun (name,entries) ->
            match Seq.toList entries with
            | [symbol,storage] when storage.Type=HirType.String -> Some(name,symbol)
            | _ -> None) |> Map.ofSeq
    let stringSites=[
        for KeyValue(scope,analysis) in scopes do
            let nameOf = function
                | ReadVar(s,_,_) -> effects.Aliases.Storages.TryFind s |> Option.map(fun storage -> normalize storage.Name)
                | DynamicReadVar(name,_,_) -> Some(normalize name)
                | _ -> None
            for KeyValue(id,node) in analysis.Graph.Nodes do
                let lengths=nodeExpressions node.Kind |> Seq.collect HirEffects.expressions |> Seq.choose(function
                    | CallFunc(len,[ValueArg value],_,_) as call
                        when inlineFunction effects.Program len 1 && effects.Program.SymbolNames.TryFind len |> Option.exists(fun name -> normalize name="LEN") ->
                        nameOf value |> Option.map(fun name -> call,name)
                    | _ -> None) |> Seq.toList
                let appends=
                    match node.Kind with
                    | Statement(Assign(target,(Binary(Concat,prefix,_,_,_) as value),_)) ->
                        let destination=
                            match target with
                            | WriteVar(s,_,_) -> effects.Aliases.Storages.TryFind s |> Option.map(fun storage -> normalize storage.Name)
                            | DynamicWriteVar(name,_,_) -> Some(normalize name)
                            | _ -> None
                        match destination,nameOf prefix with
                        | Some destination,Some source when destination=source -> [value,destination]
                        | _ -> []
                    | _ -> []
                for site,name in lengths @ appends do
                    let allocated=
                        trustedScopes.Contains scope &&
                        uniqueStrings.TryFind name |> Option.exists(fun symbol ->
                            analysis.EntryBounds[id] |> Option.exists(fun bounds ->
                                // Effects evaluated earlier in the node may resize or rebind it.
                                let evaluated=killMetadata context scope (evaluationOf context scope node) {unknownFlow with Bounds=bounds}
                                evaluated.Bounds.TryFind symbol |> Option.exists(fun shape -> shape.Length=1)))
                    yield site,allocated]
    let allocatedStrings=stringSites |> List.groupBy fst |> List.choose(fun (site,occurrences) -> if occurrences |> List.forall snd then Some site else None) |> Set.ofList
    {Effects=effects;Scopes=scopes;SafeIntegerExpressions=safe;ArithmeticProofsEnabled=not trustedScopes.IsEmpty;SafeArrayAccesses=safeArrays;OneDimensionalIntegerAccesses=rankOneArrays;TwoDimensionalIntegerAccesses=rankTwoArrays
     EntryShapePasses=entryShapePasses;EntryShapesCertified=entryShapesCertified
     CertifiedNativeForLoops=certificates;CertifiedMemoryForLoops=memoryCertificates;NativeForRejections=rejections;ProofEligibleScopes=trustedScopes;GlobalControlBarriers=barriers
     AllocatedStringSites=allocatedStrings}

let analyze target program = analyzeWithOptions defaultOptions target program

/// Deterministic diagnostic dump. Facts refer to optimized HIR, never source
/// line numbers alone (several expressions can share one source position).
let format (analysis:Analysis) =
    let text=StringBuilder()
    let line value = text.AppendLine(value:string) |> ignore
    let symbols set = set |> Seq.map (fun s -> analysis.Effects.Aliases.Storages.TryFind s |> Option.map _.Name |> Option.defaultValue (string s)) |> String.concat ","
    let effects e = $"reads=[{symbols e.Reads}] writes=[{symbols e.Writes}] descriptors=[{symbols e.DescriptorWrites}] loop-metadata=[{symbols e.LoopMetadataWrites}] unknown-read={e.ReadsUnknown} unknown-write={e.WritesUnknown} unknown-descriptors={e.DescriptorWritesUnknown} unknown-loop-metadata={e.LoopMetadataWritesUnknown} throw={e.MayThrow} io={e.InputOutput} allocate={e.Allocates} control={e.ControlBarrier}"
    let ranges (state:RangeState option) =
        match state with
        | None -> "unreachable"
        | Some state when state.IsEmpty -> "unknown (full QL word range)"
        | Some state -> state |> Map.toList |> List.map (fun (s,r) -> $"{symbols (Set.singleton s)}=[{r.Lower},{r.Upper}]") |> String.concat " "
    line "HIR control-flow, range, effect and alias analysis (checked QL)"
    line $"Unchecked arithmetic proofs enabled: {analysis.ArithmeticProofsEnabled}; proven expressions: {analysis.SafeIntegerExpressions.Count}"
    line $"Proven numeric array accesses: {analysis.SafeArrayAccesses.Count}"
    line $"Rank-one integer array accesses: {analysis.OneDimensionalIntegerAccesses.Count}"
    line $"Rank-two integer array accesses: {analysis.TwoDimensionalIntegerAccesses.Count}"
    line $"Global array call-entry analysis: passes={analysis.EntryShapePasses}; certified={analysis.EntryShapesCertified}"
    line $"Certified native FOR bindings: {analysis.CertifiedNativeForLoops.Count}"
    line $"Certified memory-counter FOR call regions: {analysis.CertifiedMemoryForLoops.Count}"
    line $"Allocated-string append/LEN sites: {analysis.AllocatedStringSites.Count}"
    line $"Proof-eligible scopes: {analysis.ProofEligibleScopes.Count}/{analysis.Scopes.Count}"
    for barrier in analysis.GlobalControlBarriers do line $"Global control barrier: {barrier}"
    for KeyValue((routine,loop),reasons) in analysis.NativeForRejections do
        let reasons=String.concat "; " reasons
        line $"Native FOR binding rejected: routine={routine}; loop={loop}; reasons={reasons}"
    line $"Address taken: [{symbols analysis.Effects.Aliases.AddressTaken}]; escaped to unknown callees: [{symbols analysis.Effects.Aliases.Escaped}]"
    for KeyValue(symbol,summary) in analysis.Effects.Summaries do line $"Routine {symbol}: {effects summary}"
    for KeyValue(scope,data) in analysis.Scopes do
        line $"Scope {scope}: blocks={data.Graph.Nodes.Count} edges={data.Graph.Edges.Length} natural-loops={data.Graph.Loops.Length} uncertain-control={data.Graph.HasUncertainControl}"
        let thresholdText=data.WideningThresholds |> List.map string |> String.concat ","
        line $"  certified-postfixed={data.Certified}; captured-FOR-control={data.ForMetadataTrusted}; narrowing-passes={data.NarrowingPasses}; widening-thresholds=[{thresholdText}]"
        for KeyValue(id,node) in data.Graph.Nodes do
            line $"  block {id}: {node.Kind}"
            line $"    in: {ranges data.EntryRanges[id]}"
            line $"    out: {ranges data.ExitRanges[id]}"
            data.EntryBounds[id] |> Option.iter(fun bounds ->
                for KeyValue(symbol,dimensions) in bounds do line $"    array {symbols(Set.singleton symbol)} guaranteed-upper-bounds={dimensions}")
            data.EntryExactBounds[id] |> Option.iter(fun bounds ->
                for KeyValue(symbol,dimensions) in bounds do line $"    array {symbols(Set.singleton symbol)} exact-upper-bounds={dimensions}")
            data.EntryLoops[id] |> Option.iter(fun loops ->
                for KeyValue(symbol,captured) in loops do line $"    captured FOR {symbols(Set.singleton symbol)} owner={captured.Owner} end=[{captured.Finish.Lower},{captured.Finish.Upper}] step=[{captured.Step.Lower},{captured.Step.Upper}]")
            line $"    {effects (nodeEffects analysis.Effects scope node.Kind)}"
            line $"    live-in=[{symbols data.Liveness.Value.LiveIn[id]}] live-out=[{symbols data.Liveness.Value.LiveOut[id]}] dominators={data.Graph.Dominators[id]}"
            for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions |> Seq.filter arithmetic |> Seq.distinct do
                let bound=data.ExpressionRanges.TryFind(id,expr) |> Option.flatten
                let value=match bound with Some r -> $"[{r.Lower},{r.Upper}]" | None -> "unknown/unreachable"
                line $"    expression-range={value} proven-at-all-occurrences={analysis.SafeIntegerExpressions.Contains expr}: {expr}"
            for edge in successors data.Graph id do line $"    -> {edge.Target} {edge.Kind} {edge.Guard} havoc={edge.Havoc}"
    text.ToString()
