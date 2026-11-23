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
    Liveness:Liveness
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
}
type Analysis = {
    Effects:Context
    Scopes:Map<Scope,ScopeAnalysis>
    SafeIntegerExpressions:Set<HirExpr>
    ArithmeticProofsEnabled:bool
    SafeArrayAccesses:Set<string * HirExpr list>
    OneDimensionalIntegerAccesses:Set<string * HirExpr list>
    TwoDimensionalIntegerAccesses:Map<string * HirExpr list,RankTwoIntegerAccess>
}

let word = {Lower = -32768L; Upper=32767L}
let private fits x = x>=word.Lower && x<=word.Upper
let private bounded low high = if fits low && fits high then {Lower=low;Upper=high} else word
let private singleton value = {Lower=value;Upper=value}
let private get state symbol = state |> Map.tryFind symbol |> Option.defaultValue word
let private put symbol range state = if range=word then Map.remove symbol state else Map.add symbol range state
let private join a b = {Lower=min a.Lower b.Lower;Upper=max a.Upper b.Upper}

type private RangeContext = { Target:TurboIrOptions; Effects:Context; Integers:Set<SymbolId> }

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
        | Binary((IntegerDivide|Modulo|Instr),_,_,_,_) -> true
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
        let killed=expand context.Effects.Aliases scope effects.Writes
        state |> Map.filter (fun s _ -> not (killed.Contains s))

let private transferRanges context scope (node:Node) state (captures:Map<SymbolId,CapturedLoop>) =
    let evaluation = nodeEvaluation context.Effects scope node.Kind
    // All writes made while evaluating sibling expressions are killed before
    // using any sibling's facts. This over-approximates either evaluation order.
    let evaluated = kill context scope evaluation state
    let effects=nodeEffects context.Effects scope node.Kind
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
    | NotEqual -> if get state symbol=singleton value then None else Some state
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
    if effects.WritesUnknown then {state with Bounds=Map.empty;ExactBounds=Map.empty;Loops=Map.empty}
    else
        let killed=expand context.Effects.Aliases scope effects.Writes
        {state with Bounds=state.Bounds |> Map.filter(fun s _ -> not(killed.Contains s))
                    ExactBounds=state.ExactBounds |> Map.filter(fun s _ -> not(killed.Contains s))
                    Loops=state.Loops |> Map.filter(fun s _ -> not(killed.Contains s))}

let private transfer context scope (node:Node) state =
    let evaluation=nodeEvaluation context.Effects scope node.Kind
    let evaluated=killMetadata context scope evaluation state
    let after=killMetadata context scope (nodeEffects context.Effects scope node.Kind) evaluated
    let mutable bounds=after.Bounds
    let mutable exactBounds=after.ExactBounds
    let mutable loops=after.Loops
    let range expr=evaluate context (kill context scope evaluation state.Ranges) expr |> fst |> Option.defaultValue word
    match node.Kind with
    | Statement(Assign((WriteArrayElem _|DynamicWriteArrayElem _),_,_)) ->
        // An element store changes contents, not the descriptor. Calls in its
        // value/subscripts have already invalidated descriptor facts above.
        bounds<-evaluated.Bounds
        exactBounds<-evaluated.ExactBounds
    | Statement(Assign(WriteVar(symbol,_,_),_,_)) when context.Integers.Contains symbol ->
        // Its own word store changes only the counter, not END/STEP/NEXT.
        // Keep invalidations of other potentially aliased metadata conservative.
        match evaluated.Loops.TryFind symbol with
        | Some captured -> loops<-loops.Add(symbol,captured)
        | None -> ()
    | Statement(Dim(symbol,dimensions,_)) ->
        let values=dimensions |> List.map range
        if not values.IsEmpty && (values |> List.forall(fun r -> r.Lower>=0L && r.Upper<=word.Upper)) then
            bounds<-bounds.Add(symbol,values |> List.map(fun r -> int r.Lower))
            if values |> List.forall(fun r -> r.Lower=r.Upper) then
                exactBounds<-exactBounds.Add(symbol,values |> List.map(fun r -> int r.Lower))
    | ForInitialise(symbol,[first;last;step],_) ->
        loops<-loops.Add(symbol,{Owner=node.Id;Finish=range last;Step=range step})
    | ForAdvance(_,Some _) -> loops<-evaluated.Loops // ADD.W changes the counter, not END/STEP.
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

let private solve options context graph =
    let incoming=Dictionary<int,FlowState option>()
    let outgoing=Dictionary<int,FlowState option>()
    let changes=Dictionary<int,int>()
    for KeyValue(id,_) in graph.Nodes do incoming[id]<-None;outgoing[id]<-None;changes[id]<-0
    let dimmed=HirEffects.statements context.Effects.Program.Main |> Seq.append(context.Effects.Program.Routines |> Seq.collect(fun r -> HirEffects.statements r.Body)) |> Seq.choose(function Dim(s,_,_) -> Some s | _ -> None) |> Set.ofSeq
    let initialBounds =
        context.Effects.Aliases.Storages |> Map.toSeq |> Seq.choose(fun (s,storage) ->
            let local=match graph.Scope,storage.Class with MainScope,GlobalStorage -> true | RoutineScope r,RoutineLocalStorage name -> context.Effects.Program.Routines |> List.exists(fun routine -> routine.Symbol=r && routine.Name=name) | _ -> false
            match storage.Type,storage.Dimensions with
            | Array _,Some dims when local && not(dimmed.Contains s) && not dims.IsEmpty && List.forall(fun n -> n>=0 && n<=32767) dims -> Some(s,dims)
            | _ -> None) |> Map.ofSeq
    let initial={unknownFlow with Bounds=initialBounds;ExactBounds=initialBounds}
    incoming[graph.Entry]<-Some initial
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
            joinFlows result candidate) (if id=graph.Entry then Some initial else None)
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
            let state=entryRanges[id] |> Option.map (kill context graph.Scope (nodeEvaluation context.Effects graph.Scope node.Kind))
            for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions do
                yield (id,expr), state |> Option.bind (fun state -> evaluate context state expr |> fst)] |> Map.ofList
    // A jump into a different FOR body can carry another loop's hidden NEXT
    // address, even when both use the same counter vector. Require the matching
    // initializer to dominate checks/advances before trusting runtime control.
    let metadataTrusted=graph.Nodes |> Map.forall(fun id node ->
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
        | _ -> true)
    {Graph=graph;Liveness=liveness context.Effects graph;EntryRanges=entryRanges;ExitRanges=exitRanges;ExpressionRanges=expressions
     EntryBounds=incoming |> Seq.map(fun p -> p.Key,p.Value |> Option.map _.Bounds) |> Map.ofSeq
     EntryExactBounds=incoming |> Seq.map(fun p -> p.Key,p.Value |> Option.map _.ExactBounds) |> Map.ofSeq
     EntryLoops=incoming |> Seq.map(fun p -> p.Key,p.Value |> Option.map _.Loops) |> Map.ofSeq
     WideningThresholds=limits;NarrowingPasses=passes;Certified=certified;ForMetadataTrusted=metadataTrusted}

let private arithmetic = function Unary(Negate,_,_,_) | Binary((Add|Subtract|Multiply),_,_,_,_) -> true | _ -> false

/// Analyzes immutable HIR. No facts are assumptions about caller values, QL
/// startup zeroes, floating arithmetic, or successful execution of a call.
let analyzeWithOptions options target program =
    let effects=HirEffects.analyze program
    let integers=effects.Aliases.Storages |> Map.toSeq |> Seq.choose (fun (symbol,storage) ->
        if storage.Dimensions.IsNone && (match storage.Type with Array _ -> false | _ -> true) && nameIsInteger target storage.Name then Some symbol else None) |> Set.ofSeq
    let context={Target=target;Effects=effects;Integers=integers}
    let graphs = (MainScope,build effects MainScope program.Main) :: (program.Routines |> List.map (fun r -> RoutineScope r.Symbol,build effects (RoutineScope r.Symbol) r.Body))
    // Unknown continuations can enter another scope or resume past a guard.
    // Model their edges, but withhold unchecked-arithmetic proofs program-wide.
    let scopes=graphs |> List.map (fun (scope,graph) -> scope,solve options context graph) |> Map.ofList
    let trusted=(graphs |> List.forall (fun (_,g) -> not g.HasUncertainControl)) && (scopes |> Map.forall(fun _ data -> data.Certified && data.ForMetadataTrusted))
    let candidates = [
        for KeyValue(scope,analysis) in scopes do
            for KeyValue(id,node) in analysis.Graph.Nodes do
                let evaluation=nodeEvaluation effects scope node.Kind
                let state=analysis.EntryRanges[id] |> Option.map (kill context scope evaluation)
                for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions |> Seq.filter arithmetic do
                    let proven=
                        trusted && not evaluation.WritesUnknown && evaluation.Writes.IsEmpty &&
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
                let evaluation=nodeEvaluation effects scope node.Kind
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
                        eligible && trusted && nameIsInteger target name && indexes.Length=1 &&
                        not evaluation.WritesUnknown && evaluation.Writes.IsEmpty &&
                        (match symbol,bounds with
                         | Some symbol,Some bounds when named name=[symbol] -> bounds.TryFind symbol |> Option.exists(fun dimensions -> dimensions.Length=1)
                         | _ -> false)
                    let proven=
                        eligible && trusted && not(name.EndsWith "$" ) && not evaluation.WritesUnknown && evaluation.Writes.IsEmpty &&
                        (match symbol,state,bounds with
                         | Some symbol,Some state,Some bounds when named name=[symbol] ->
                             bounds.TryFind symbol |> Option.exists(fun dimensions ->
                                 dimensions.Length=indexes.Length && List.forall2(fun index bound ->
                                     evaluate context state index |> fst |> Option.exists(fun range -> range.Lower>=0L && range.Upper<=int64 bound)) indexes dimensions)
                         | _ -> false)
                    let rankTwo=
                        if eligible && trusted && nameIsInteger target name && indexes.Length=2 &&
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
    {Effects=effects;Scopes=scopes;SafeIntegerExpressions=safe;ArithmeticProofsEnabled=trusted;SafeArrayAccesses=safeArrays;OneDimensionalIntegerAccesses=rankOneArrays;TwoDimensionalIntegerAccesses=rankTwoArrays}

let analyze target program = analyzeWithOptions defaultOptions target program

/// Deterministic diagnostic dump. Facts refer to optimized HIR, never source
/// line numbers alone (several expressions can share one source position).
let format (analysis:Analysis) =
    let text=StringBuilder()
    let line value = text.AppendLine(value:string) |> ignore
    let symbols set = set |> Seq.map (fun s -> analysis.Effects.Aliases.Storages.TryFind s |> Option.map _.Name |> Option.defaultValue (string s)) |> String.concat ","
    let effects e = $"reads=[{symbols e.Reads}] writes=[{symbols e.Writes}] unknown-read={e.ReadsUnknown} unknown-write={e.WritesUnknown} throw={e.MayThrow} io={e.InputOutput} allocate={e.Allocates} control={e.ControlBarrier}"
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
            line $"    live-in=[{symbols data.Liveness.LiveIn[id]}] live-out=[{symbols data.Liveness.LiveOut[id]}] dominators={data.Graph.Dominators[id]}"
            for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions |> Seq.filter arithmetic |> Seq.distinct do
                let bound=data.ExpressionRanges.TryFind(id,expr) |> Option.flatten
                let value=match bound with Some r -> $"[{r.Lower},{r.Upper}]" | None -> "unknown/unreachable"
                line $"    expression-range={value} proven-at-all-occurrences={analysis.SafeIntegerExpressions.Contains expr}: {expr}"
            for edge in successors data.Graph id do line $"    -> {edge.Target} {edge.Kind} {edge.Guard} havoc={edge.Havoc}"
    text.ToString()
