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
type ScopeAnalysis = {
    Graph:Graph
    Liveness:Liveness
    EntryRanges:Map<int,RangeState option>
    ExitRanges:Map<int,RangeState option>
    ExpressionRanges:Map<int * HirExpr,Interval option>
}
type Analysis = {
    Effects:Context
    Scopes:Map<Scope,ScopeAnalysis>
    SafeIntegerExpressions:Set<HirExpr>
    ArithmeticProofsEnabled:bool
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

let private transfer context scope (node:Node) state =
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
    | ForAdvance(s,Some step) ->
        let value=get state s
        match constant context step with
        | Some(ConstInt step) when fits(int64 step) ->
            // FOR uses ADD.W without a check. Crossing either word boundary
            // invalidates the interval instead of assuming a mathematical loop.
            put s (bounded (value.Lower+int64 step) (value.Upper+int64 step)) after
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
        match expr with
        | Binary(BitwiseAnd,a,b,_,_) when truth -> chain a b
        | Binary(BitwiseOr,a,b,_,_) when not truth -> chain a b
        | Unary(BitwiseNot,x,_,_) -> refine context scope x (not truth) state
        | ReadVar(s,_,_) when context.Integers.Contains s -> comparison s (if truth then NotEqual else Equal) 0L state
        | Literal(ConstInt n,_,_) when fits(int64 n) -> if (n<>0)=truth then Some state else None
        | Binary(op,ReadVar(s,_,_),value,_,_) when context.Integers.Contains s ->
            match constant context value with
            | Some(ConstInt n) when fits(int64 n) -> comparison s (if truth then op else invert op) (int64 n) state
            | _ -> Some state
        | Binary(op,value,ReadVar(s,_,_),_,_) when context.Integers.Contains s ->
            match constant context value with
            | Some(ConstInt n) when fits(int64 n) -> comparison s (if truth then reverse op else invert(reverse op)) (int64 n) state
            | _ -> Some state
        | _ -> Some state

let private edgeState context scope edge state =
    let initial=if edge.Havoc then Map.empty else state
    let guarded =
        match edge.Guard with
        | Unconditional -> Some initial
        | Predicate(expr,truth) -> refine context scope expr truth initial
        | CountedLoop(s,last,step,truth) ->
            match constant context last,constant context step with
            | Some(ConstInt last),Some(ConstInt step) when context.Integers.Contains s && fits(int64 last) && fits(int64 step) ->
                let op=if step<0 then GreaterThanOrEqual else LessThanOrEqual
                comparison s (if truth then op else invert op) (int64 last) initial
            | _ -> Some initial
    guarded |> Option.map (kill context scope {empty with Writes=edge.Kill})

let private joinStates a b =
    match a,b with
    | None,x | x,None -> x
    | Some a,Some b ->
        Some(a |> Map.toSeq |> Seq.choose (fun (s,range) ->
            b.TryFind s |> Option.map (fun other -> s,join range other)) |> Map.ofSeq |> Map.filter (fun _ value -> value<>word))

let private widen previous next =
    match previous,next with
    | Some previous,Some next ->
        next |> Map.map (fun s value ->
            let old=get previous s
            {Lower=if value.Lower<old.Lower then word.Lower else value.Lower
             Upper=if value.Upper>old.Upper then word.Upper else value.Upper})
        |> Map.filter (fun _ value -> value<>word) |> Some
    | _ -> next

let private solve context graph =
    let incoming=Dictionary<int,RangeState option>()
    let outgoing=Dictionary<int,RangeState option>()
    let changes=Dictionary<int,int>()
    for KeyValue(id,_) in graph.Nodes do incoming[id]<-None;outgoing[id]<-None;changes[id]<-0
    incoming[graph.Entry]<-Some Map.empty
    let edges=graph.Edges |> List.groupBy _.Source |> Map.ofList
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
            let next=joinStates incoming[edge.Target] candidate
            let next=if widenAt.Contains edge.Target && changes[edge.Target]>=3 then widen incoming[edge.Target] next else next
            if next<>incoming[edge.Target] then
                incoming[edge.Target]<-next; changes[edge.Target]<-changes[edge.Target]+1
                enqueue edge.Target
    let entryRanges=incoming |> Seq.map (fun p -> p.Key,p.Value) |> Map.ofSeq
    let exitRanges=outgoing |> Seq.map (fun p -> p.Key,p.Value) |> Map.ofSeq
    let expressions = [
        for KeyValue(id,node) in graph.Nodes do
            let state=entryRanges[id] |> Option.map (kill context graph.Scope (nodeEvaluation context.Effects graph.Scope node.Kind))
            for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions do
                yield (id,expr), state |> Option.bind (fun state -> evaluate context state expr |> fst)] |> Map.ofList
    {Graph=graph;Liveness=liveness context.Effects graph;EntryRanges=entryRanges;ExitRanges=exitRanges;ExpressionRanges=expressions}

let private arithmetic = function Unary(Negate,_,_,_) | Binary((Add|Subtract|Multiply),_,_,_,_) -> true | _ -> false

/// Analyzes immutable HIR. No facts are assumptions about caller values, QL
/// startup zeroes, floating arithmetic, or successful execution of a call.
let analyze target program =
    let effects=HirEffects.analyze program
    let integers=effects.Aliases.Storages |> Map.toSeq |> Seq.choose (fun (symbol,storage) ->
        if storage.Dimensions.IsNone && (match storage.Type with Array _ -> false | _ -> true) && nameIsInteger target storage.Name then Some symbol else None) |> Set.ofSeq
    let context={Target=target;Effects=effects;Integers=integers}
    let graphs = (MainScope,build effects MainScope program.Main) :: (program.Routines |> List.map (fun r -> RoutineScope r.Symbol,build effects (RoutineScope r.Symbol) r.Body))
    // Unknown continuations can enter another scope or resume past a guard.
    // Model their edges, but withhold unchecked-arithmetic proofs program-wide.
    let trusted=graphs |> List.forall (fun (_,g) -> not g.HasUncertainControl)
    let scopes=graphs |> List.map (fun (scope,graph) -> scope,solve context graph) |> Map.ofList
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
    {Effects=effects;Scopes=scopes;SafeIntegerExpressions=safe;ArithmeticProofsEnabled=trusted}

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
    line $"Address taken: [{symbols analysis.Effects.Aliases.AddressTaken}]; escaped to unknown callees: [{symbols analysis.Effects.Aliases.Escaped}]"
    for KeyValue(symbol,summary) in analysis.Effects.Summaries do line $"Routine {symbol}: {effects summary}"
    for KeyValue(scope,data) in analysis.Scopes do
        line $"Scope {scope}: blocks={data.Graph.Nodes.Count} edges={data.Graph.Edges.Length} natural-loops={data.Graph.Loops.Length} uncertain-control={data.Graph.HasUncertainControl}"
        for KeyValue(id,node) in data.Graph.Nodes do
            line $"  block {id}: {node.Kind}"
            line $"    in: {ranges data.EntryRanges[id]}"
            line $"    out: {ranges data.ExitRanges[id]}"
            line $"    {effects (nodeEffects analysis.Effects scope node.Kind)}"
            line $"    live-in=[{symbols data.Liveness.LiveIn[id]}] live-out=[{symbols data.Liveness.LiveOut[id]}] dominators={data.Graph.Dominators[id]}"
            for expr in nodeExpressions node.Kind |> Seq.collect HirEffects.expressions |> Seq.filter arithmetic |> Seq.distinct do
                let bound=data.ExpressionRanges.TryFind(id,expr) |> Option.flatten
                let value=match bound with Some r -> $"[{r.Lower},{r.Upper}]" | None -> "unknown/unreachable"
                line $"    expression-range={value} proven-at-all-occurrences={analysis.SafeIntegerExpressions.Contains expr}: {expr}"
            for edge in successors data.Graph id do line $"    -> {edge.Target} {edge.Kind} {edge.Guard} havoc={edge.Havoc}"
    text.ToString()
