module HirControlFlow

open System.Collections.Generic
open HIR
open HirEffects

type NodeKind =
    | Entry
    | Finish
    | Statement of HirStmt
    | Test of HirExpr
    | ForInitialise of SymbolId * HirExpr list * HirExpr option
    | ForCheck of SymbolId * HirExpr option * HirExpr option
    | ForAdvance of SymbolId * HirExpr option
    | Join of string

type Node = { Id:int; Kind:NodeKind }
type EdgeKind = FallThrough | BranchTrue | BranchFalse | Jump | SubroutineCall | SubroutineReturn | Exceptional | UnknownTransfer
type Guard = Unconditional | Predicate of HirExpr * bool | CountedLoop of SymbolId * HirExpr * HirExpr * bool
type Edge = { Source:int; Target:int; Kind:EdgeKind; Guard:Guard; Havoc:bool; Kill:Set<SymbolId> }
type NaturalLoop = { Header:int; Latch:int; Members:Set<int> }
type ForRegion = { Initialiser:int; Check:int; Advance:int; Members:Set<int> }
type Graph = {
    Scope:Scope
    Nodes:Map<int,Node>
    Edges:Edge list
    Entry:int
    Exit:int
    Labels:Map<int,int>
    Reachable:Set<int>
    ReversePostOrder:int list
    Dominators:Map<int,Set<int>>
    Loops:NaturalLoop list
    HasUncertainControl:bool
    ForRegions:Map<LoopId,ForRegion>
}

let successors graph id = graph.Edges |> List.filter (fun e -> e.Source=id)
let predecessors graph id = graph.Edges |> List.filter (fun e -> e.Target=id)

let nodeExpressions = function
    | Statement stmt -> statementExpressions stmt
    | Test expr -> [expr]
    | ForInitialise(_,values,_) -> values
    | _ -> []

let nodeEvaluation context scope kind =
    nodeExpressions kind |> List.map (expression context scope) |> combineAll

let nodeEffects context scope = function
    | Statement stmt -> statement context scope stmt
    | Test expr -> expression context scope expr
    | ForInitialise(s,values,_) ->
        combine (values |> List.map (expression context scope) |> combineAll) {loopInitialisation context scope s with MayThrow=true}
    | ForCheck(s,_,_) -> {empty with Reads=Set.singleton s}
    | ForAdvance(s,_) -> {empty with Reads=Set.singleton s;Writes=Set.singleton s}
    | _ -> empty

let private targetLine = function Literal(ConstInt n,_,_) -> Some n | _ -> None

let private loopMetadataMayChange context scope counter body =
    let bodyEffects=body |> List.map (statement context scope) |> combineAll
    bodyEffects.LoopMetadataWritesUnknown ||
    (expand context.Aliases scope bodyEffects.LoopMetadataWrites).Contains counter

/// One elementary HIR operation per block. Keeping source anchors as blocks
/// permits jumps into structured bodies without pretending they enter at IF/FOR.
let build (context:Context) scope body =
    let nodes = ResizeArray<Node>()
    let edges = ResizeArray<Edge>()
    let labels = Dictionary<int,int>()
    let pending = ResizeArray<int * HirExpr * EdgeKind>()
    let unknownTargets = ResizeArray<int>()
    let arbitraryContinuations = HashSet<int>()
    let returnNodes = ResizeArray<int>()
    let continuations = HashSet<int>()
    let handlers = ResizeArray<int>()
    let forRegions = Dictionary<LoopId,ForRegion>()
    let mutable uncertain = false
    let node kind = let id=nodes.Count in nodes.Add {Id=id;Kind=kind}; id
    let edge source target kind guard havoc kill = edges.Add {Source=source;Target=target;Kind=kind;Guard=guard;Havoc=havoc;Kill=kill}
    let normal source target = edge source target FallThrough Unconditional false Set.empty
    let entry = node Entry
    let finish = node Finish
    let rec block loops body continuation = List.foldBack (fun stmt next -> statementBlock loops stmt next) body continuation
    and statementBlock loops stmt next =
        match stmt with
        | If(expr,yes,no,_) ->
            let test = node(Test expr)
            edge test (block loops yes next) BranchTrue (Predicate(expr,true)) false Set.empty
            edge test (block loops (Option.defaultValue [] no) next) BranchFalse (Predicate(expr,false)) false Set.empty
            test
        | For(loop,s,first,last,step,body,_) ->
            // A nested FOR using this vector, or an unknown memory mutation,
            // can replace END/STEP/NEXT metadata as well as the numeric value.
            let unstable=loopMetadataMayChange context scope s body
            if unstable then uncertain<-true
            let test = node(ForCheck(s,(if unstable then None else Some last),(if unstable then None else Some step)))
            let advance = node(ForAdvance(s,if unstable then None else Some step))
            if unstable then arbitraryContinuations.Add advance |> ignore
            normal advance test
            let firstBodyNode=nodes.Count
            let inside = block ((loop,(advance,next))::loops) body advance
            let members=Set.ofList(test::advance::[firstBodyNode..nodes.Count-1])
            edge test inside BranchTrue (if unstable then Unconditional else CountedLoop(s,last,step,true)) false Set.empty
            // Turbo stores the advanced counter only if the next iteration is
            // valid. The model uses a candidate value, so exit kills that fact.
            edge test next BranchFalse (if unstable then Unconditional else CountedLoop(s,last,step,false)) false (Set.singleton s)
            let init = node(ForInitialise(s,[first;last;step],Some first))
            forRegions[loop]<-{Initialiser=init;Check=test;Advance=advance;Members=members}
            normal init test
            init
        | ForSequence(loop,s,prefix,first,last,suffix,step,body,_) ->
            uncertain <- true
            // Enumeration/range sequencing has several saved continuations.
            // Keep the full cycle but make its counter unconstrained.
            let test = node(ForCheck(s,None,None))
            let advance = node(ForAdvance(s,None))
            normal advance test
            let inside = block ((loop,(advance,next))::loops) body advance
            normal test inside
            edge test next BranchFalse Unconditional false (Set.singleton s)
            let init = node(ForInitialise(s,prefix@[first;last]@suffix@[step],None))
            normal init test
            init
        | Repeat(loop,_,body,_) ->
            let top = node(Join "REPeat")
            normal top (block ((loop,(top,next))::loops) body top)
            top
        | WhenError(body,position) ->
            uncertain <- true
            // Falling off END WHEN is an implicit CONTINUE, not job exit.
            // Its saved retry context can resume at an unnumbered boundary.
            let resume=node(Join "implicit CONTINUE")
            arbitraryContinuations.Add resume |> ignore
            let handler = block loops body resume
            handlers.Add handler
            let install = node(Statement(WhenError([],position)))
            normal install next
            install
        | _ ->
            let id = node(Statement stmt)
            match stmt with
            | LineNumber(line,_) ->
                if labels.ContainsKey line then uncertain <- true else labels.Add(line,id)
                normal id next
            | Goto(target,_) -> pending.Add(id,target,Jump)
            | OnGoto(_,targets,_) ->
                for target in targets do pending.Add(id,target,Jump)
                normal id next // Out-of-range selector falls through.
            | Gosub(target,_) ->
                uncertain <- true
                pending.Add(id,target,SubroutineCall)
                continuations.Add next |> ignore
                edge id next SubroutineReturn Unconditional true Set.empty
            | OnGosub(_,targets,_) ->
                uncertain <- true
                for target in targets do pending.Add(id,target,SubroutineCall)
                continuations.Add next |> ignore
                edge id next SubroutineReturn Unconditional true Set.empty
            | Return _ -> returnNodes.Add id
            | Exit(loop,_) | Next(loop,_) ->
                match List.tryFind (fun (key,_) -> key=loop) loops with
                | Some(_, (advance,after)) -> edge id (match stmt with Exit _ -> after | _ -> advance) Jump Unconditional false Set.empty
                | None -> uncertain <- true; arbitraryContinuations.Add id |> ignore
            | BuiltInCall(NamedBuiltIn name,_,_,_) when List.contains (name.ToUpperInvariant()) ["STOP";"QUIT"] -> normal id finish
            | BuiltInCall(NamedBuiltIn name,_,_,_) when List.contains (name.ToUpperInvariant()) ["RETRY";"RETRY1";"CONTINUE"] ->
                uncertain <- true; arbitraryContinuations.Add id |> ignore
            | _ -> normal id next
            id
    normal entry (block [] body finish)
    for source,target,kind in pending do
        match targetLine target with
        | Some line when labels.ContainsKey line -> edge source labels[line] kind Unconditional false Set.empty
        | _ -> uncertain <- true; unknownTargets.Add source
    for id in returnNodes do
        normal id finish
        for next in continuations do edge id next SubroutineReturn Unconditional true Set.empty
    // Computed/missing targets can enter any numbered anchor. Havoc prevents
    // facts from leaking through an edge with an unknown continuation context.
    for id in unknownTargets do
        edge id finish UnknownTransfer Unconditional true Set.empty
        edge id entry UnknownTransfer Unconditional true Set.empty
        for label in labels.Values do edge id label UnknownTransfer Unconditional true Set.empty
    for id in arbitraryContinuations do
        for candidate in nodes do edge id candidate.Id UnknownTransfer Unconditional true Set.empty
    for candidate in nodes do
        if (nodeEffects context scope candidate.Kind).MayThrow then
            edge candidate.Id finish Exceptional Unconditional true Set.empty
            for handler in handlers do edge candidate.Id handler Exceptional Unconditional true Set.empty
    let edges = edges |> Seq.distinct |> List.ofSeq
    let nodes = nodes |> Seq.map (fun n -> n.Id,n) |> Map.ofSeq
    let outgoing = edges |> List.groupBy _.Source |> Map.ofList
    let incoming = edges |> List.groupBy _.Target |> Map.ofList
    let reached = HashSet<int>()
    let work = Stack<int*bool>()
    let postorder=ResizeArray<int>()
    work.Push(entry,false)
    while work.Count>0 do
        let id,finished = work.Pop()
        if finished then postorder.Add id
        elif reached.Add id then
            work.Push(id,true)
            for e in outgoing.TryFind id |> Option.defaultValue [] do work.Push(e.Target,false)
    let reachable = Set.ofSeq reached
    let traversal=postorder |> Seq.rev |> List.ofSeq
    let mutable dom = nodes |> Map.map (fun id _ -> if id=entry then Set.singleton entry elif reachable.Contains id then reachable else Set.empty)
    let mutable changed = true
    while changed do
        changed <- false
        for id in traversal do
            if id<>entry then
                let parents = incoming.TryFind id |> Option.defaultValue [] |> List.filter (fun e -> reachable.Contains e.Source)
                let common = match parents with [] -> Set.empty | first::rest -> rest |> List.fold (fun acc e -> Set.intersect acc dom[e.Source]) dom[first.Source]
                let next = common.Add id
                if next<>dom[id] then dom <- dom.Add(id,next); changed <- true
    let loops =
        [for e in edges do
            if reachable.Contains e.Source && dom[e.Source].Contains e.Target then
                let members = HashSet<int>([e.Target;e.Source])
                let search = Stack<int>()
                if e.Source<>e.Target then search.Push e.Source
                while search.Count>0 do
                    let current = search.Pop()
                    for p in incoming.TryFind current |> Option.defaultValue [] do
                        if reachable.Contains p.Source && members.Add p.Source && p.Source<>e.Target then search.Push p.Source
                yield {Header=e.Target;Latch=e.Source;Members=Set.ofSeq members}]
    {Scope=scope;Nodes=nodes;Edges=edges;Entry=entry;Exit=finish;Labels=Map.ofSeq (labels |> Seq.map (fun p -> p.Key,p.Value))
     Reachable=reachable;ReversePostOrder=traversal;Dominators=dom;Loops=loops;HasUncertainControl=uncertain
     ForRegions=forRegions |> Seq.map(fun pair -> pair.Key,pair.Value) |> Map.ofSeq}

type Liveness = { LiveIn:Map<int,Set<SymbolId>>; LiveOut:Map<int,Set<SymbolId>> }

/// Calls' MAY writes are not kills. Only an actual scalar assignment kills a
/// symbol; alias-expanded reads deliberately keep possible referents live.
let liveness context graph =
    let visible = context.Aliases.Visible[graph.Scope]
    let order = List.rev graph.ReversePostOrder
    // A node's uses, definitions and successors never change between iterations.
    let targets = graph.Edges |> List.groupBy _.Source |> Map.ofList |> Map.map (fun _ edges -> edges |> List.map _.Target)
    let facts = Dictionary<int,Set<SymbolId> * Set<SymbolId> * int list>()
    for id in order do
        let node=graph.Nodes[id]
        let effects = nodeEffects context graph.Scope node.Kind
        let uses = if effects.ReadsUnknown then visible else expand context.Aliases graph.Scope effects.Reads
        let defs =
            match node.Kind with
            | Statement(Assign(WriteVar(s,_,_),_,_)) | ForInitialise(s,_,_) -> Set.singleton s
            | _ -> Set.empty
        facts[id] <- (uses, defs, targets.TryFind id |> Option.defaultValue [])
    let mutable liveIn = graph.Nodes |> Map.map (fun _ _ -> Set.empty)
    let mutable liveOut = liveIn
    let mutable changed = true
    while changed do
        changed <- false
        for id in order do
            let uses, defs, next = facts[id]
            let after = next |> List.map (fun target -> liveIn[target]) |> Set.unionMany
            let before = Set.union uses (Set.difference after defs)
            if before<>liveIn[id] || after<>liveOut[id] then
                liveIn <- liveIn.Add(id,before); liveOut <- liveOut.Add(id,after); changed <- true
    {LiveIn=liveIn;LiveOut=liveOut}
