module SBTests.HirAnalysisTests

open NUnit.Framework
open HIR
open HirEffects
open HirControlFlow
open HirAnalysis

let private pos:Types.SourcePosition = {BasicLineNo=None;EditorLineNo=1;Column=1}
let private x,y,z,i,flag = SymbolId 1,SymbolId 2,SymbolId 3,SymbolId 4,SymbolId 5
let private number n = Literal(ConstInt n,HirType.Int,pos)
let private read s = ReadVar(s,HirType.Int,pos)
let private target s = WriteVar(s,HirType.Int,pos)
let private assign s value = Assign(target s,value,pos)
let private binary op a b = Binary(op,a,b,HirType.Int,pos)
let private storage symbol name kind = {Symbol=symbol;Slot=StorageSlotId 0;Name=name;Type=HirType.Int;Dimensions=None;Class=kind;Position=pos}
let private globals = [storage x "X%" GlobalStorage;storage y "Y%" GlobalStorage;storage z "Z%" GlobalStorage;storage i "I%" GlobalStorage;storage flag "FLAG%" GlobalStorage]
let private program body = {Globals=globals;Routines=[];Main=body;SymbolNames=Map.empty;DataEntries=[];RestorePoints=[]}
let private analyze p = HirAnalysis.analyze HirTurboIrBackend.defaultOptions p
let private main a = a.Scopes[MainScope]
let private ranges scope expr a = a.Scopes[scope].ExpressionRanges |> Map.toList |> List.choose (fun ((_,e),range) -> if e=expr then Some range else None)
let private assertRange low high expr a = Assert.That(ranges MainScope expr a, Is.EqualTo(box [Some {Lower=low;Upper=high}]))
let private proven expr a = Assert.That(a.SafeIntegerExpressions.Contains expr, Is.True)
let private unproven expr a = Assert.That(a.SafeIntegerExpressions.Contains expr, Is.False)
let private print expr = BuiltInCall(Print,None,[expr],pos)
let private routine symbol name parameters body = {Symbol=symbol;Name=name;Parameters=parameters;Locals=[];Body=body;ReturnType=None;EndLineNumber=None;Position=pos}
let private parameter owner symbol name = {Storage=storage symbol name (RoutineParameterStorage owner);Binding=FlexibleBinding}

[<Test>]
let ``widening thresholds retain a bounded loop invariant that classical widening loses`` () =
    let plus=binary Add (read x) (number 1)
    let p=program [assign x (number 0);Repeat(LoopId 1,AnonymousLoop,
                    [assign y plus;If(binary LessThan (read x) (number 8191),[assign x plus],Some[assign x (number 0)],pos)],pos)]
    let classical=HirAnalysis.analyzeWithOptions {UseWideningThresholds=false;NarrowingPasses=0} HirTurboIrBackend.defaultOptions p
    unproven plus classical
    let refined=analyze p
    proven plus refined
    Assert.That(ranges MainScope plus refined |> List.forall(Option.exists(fun r -> r.Lower>=1L && r.Upper<=8192L)), Is.True)
    Assert.That((main refined).WideningThresholds, Does.Contain(8191L))
    Assert.That((main refined).Certified, Is.True)

[<Test>]
let ``narrowing recovers an invariant whose bound is computed before the loop`` () =
    let plus=binary Add (read x) (number 1)
    let p=program [assign z (number 16);assign z (binary Multiply (read z) (read z));assign x (number 0)
                   Repeat(LoopId 1,AnonymousLoop,[assign y plus;If(binary LessThan (read x) (read z),[assign x plus],Some[assign x (number 0)],pos)],pos)]
    let widened=HirAnalysis.analyzeWithOptions {HirAnalysis.defaultOptions with NarrowingPasses=0} HirTurboIrBackend.defaultOptions p
    unproven plus widened
    let refined=analyze p
    proven plus refined
    Assert.That(ranges MainScope plus refined |> List.forall(Option.exists(fun r -> r.Lower>=1L && r.Upper<=257L)), Is.True)
    Assert.That((main refined).NarrowingPasses, Is.GreaterThan(0))
    Assert.That((main refined).Certified, Is.True)

[<Test>]
let ``FOR captures both variable endpoint and step before either is changed`` () =
    let expr=binary Multiply (read i) (number 3)
    let p=program [assign x (number 10);assign z (number 2)
                   For(LoopId 1,i,number 1,read x,read z,[assign x (number -100);assign z (number 32767);assign y expr],pos)]
    let facts=analyze p
    proven expr facts
    assertRange 3L 30L expr facts

[<Test>]
let ``jumping into a FOR body cannot invent captured metadata`` () =
    let plus=binary Add (read i) (number 1)
    let facts=program [Goto(number 30,pos);For(LoopId 1,i,number 0,number 100,number 1,[LineNumber(30,pos);assign x plus],pos)] |> analyze
    unproven plus facts
    Assert.That(facts.ArithmeticProofsEnabled, Is.False)

[<Test>]
let ``jumps between FOR bodies cannot reuse another initializer's hidden NEXT address`` () =
    let plus=binary Add (read i) (number 1)
    let facts=program [For(LoopId 1,i,number 0,number 5,number 1,[Goto(number 200,pos)],pos)
                       For(LoopId 2,i,number 0,number 10,number 1,[LineNumber(200,pos);assign x plus],pos)] |> analyze
    Assert.That(facts.ArithmeticProofsEnabled, Is.False)
    unproven plus facts

[<Test>]
let ``a later initializer can overwrite hidden NEXT even when the earlier initializer dominates`` () =
    let plus=binary Add (read i) (number 1)
    let facts=program [For(LoopId 1,i,number 0,number 2,number 1,[LineNumber(10,pos);assign x plus],pos)
                       For(LoopId 2,i,number 0,number 10,number 1,[Goto(number 10,pos)],pos)] |> analyze
    Assert.That((main facts).ForMetadataTrusted, Is.False)
    Assert.That(facts.ArithmeticProofsEnabled, Is.False)
    unproven plus facts

[<Test>]
let ``counter word assignments preserve captured END and STEP ownership`` () =
    let plus=binary Add (read i) (number 1)
    let facts=program [For(LoopId 1,i,number 0,number 10,number 1,[If(read flag,[assign i (number 5)],None,pos);assign x plus],pos)] |> analyze
    Assert.That((main facts).ForMetadataTrusted, Is.True)
    proven plus facts

let private arraySymbol=SymbolId 20
let private arrayProgram body =
    let p=program body
    let declared=storage arraySymbol "A%" GlobalStorage
    {p with Globals=p.Globals@[{declared with Type=Array HirType.Int;Dimensions=Some[10]}]}
let private arrayRead indexes=ReadArrayElem(arraySymbol,indexes,HirType.Int,pos)
let private arraySafe indexes facts=Assert.That(facts.SafeArrayAccesses.Contains("A%",indexes), Is.True)
let private arrayChecked indexes facts=Assert.That(facts.SafeArrayAccesses.Contains("A%",indexes), Is.False)

[<Test>]
let ``rank-two facts separate per-dimension proofs from exact lengths`` () =
    let indexes=[read x;number 2]
    let facts=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);assign y (arrayRead indexes)] |> analyze
    let shape=facts.TwoDimensionalIntegerAccesses["A%",indexes]
    Assert.That(shape.ExactLengths, Is.EqualTo(Some(4,8)))
    Assert.That(shape.ProvenRows, Is.False)
    Assert.That(shape.ProvenColumns, Is.True)

[<Test>]
let ``rank-two branch joins do not turn minimum bounds into exact strides`` () =
    let indexes=[number 1;number 5]
    let facts=arrayProgram [If(read flag,[Dim(arraySymbol,[number 3;number 7],pos)],Some[Dim(arraySymbol,[number 3;number 15],pos)],pos)
                            assign y (arrayRead indexes)] |> analyze
    let shape=facts.TwoDimensionalIntegerAccesses["A%",indexes]
    Assert.That(shape.ProvenRows && shape.ProvenColumns, Is.True)
    Assert.That(shape.ExactLengths, Is.EqualTo(None))
    arraySafe indexes facts

[<Test>]
let ``rank-two nonconstant DIM keeps rank without inventing an exact length`` () =
    let indexes=[number 1;number 7]
    let facts=arrayProgram [If(read flag,[assign x (number 7)],Some[assign x (number 15)],pos)
                            Dim(arraySymbol,[number 3;read x],pos);assign y (arrayRead indexes)] |> analyze
    let shape=facts.TwoDimensionalIntegerAccesses["A%",indexes]
    Assert.That(shape.ProvenRows && shape.ProvenColumns, Is.True)
    Assert.That(shape.ExactLengths, Is.EqualTo(None))

[<Test>]
let ``rank-two repeated expressions across resizing never reuse one exact shape`` () =
    let indexes=[number 1;number 2]
    let facts=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);assign y (arrayRead indexes)
                            Dim(arraySymbol,[number 3;number 15],pos);assign z (arrayRead indexes)] |> analyze
    Assert.That(facts.TwoDimensionalIntegerAccesses["A%",indexes].ExactLengths, Is.EqualTo(None))
    let absent=arrayProgram [assign y (arrayRead indexes);Dim(arraySymbol,[number 3;number 7],pos);assign z (arrayRead indexes)] |> analyze
    Assert.That(absent.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``rank-two DIM and precise facts are invalidated together by opaque mutation`` () =
    let indexes=[number 1;number 2]
    let facts=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);BuiltInCall(NamedBuiltIn "UNKNOWN",None,[],pos);assign y (arrayRead indexes)] |> analyze
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``rank-two lowering encodes partial checks and an exact or dynamic column length`` () =
    let indexes=[read x;number 2]
    let initial=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);assign y (arrayRead indexes)]
    let p={initial with Globals=initial.Globals |> List.map(fun s -> if s.Symbol=arraySymbol then {s with Dimensions=Some[3;7]} else s)}
    let facts=analyze p
    let options={HirTurboIrBackend.defaultOptions with TwoDimensionalIntegerAccesses=facts.TwoDimensionalIntegerAccesses}
    let ir=HirTurboIrBackend.generateTurboIrFromHir options p |> Result.defaultWith(String.concat "; " >> failwith) |> TurboNativeRegions.select true |> Result.defaultWith(String.concat "; " >> failwith)
    Assert.That(ir.Items |> List.exists(function HirTurboIrBackend.Instruction(1911,[_;HirTurboIrBackend.Word 1;HirTurboIrBackend.Word 8],_) -> true | _ -> false), Is.True)

[<Test>]
let ``rank-one selection is independent of index range proof`` () =
    let indexes=[read x]
    let facts=arrayProgram [Dim(arraySymbol,[number 10],pos);assign y (arrayRead indexes)] |> analyze
    arrayChecked indexes facts
    Assert.That(facts.OneDimensionalIntegerAccesses.Contains("A%",indexes), Is.True)
    let missing=arrayProgram [assign y (arrayRead indexes);Dim(arraySymbol,[number 10],pos)] |> analyze
    Assert.That(missing.OneDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``rank-one facts require all occurrences to have reaching shape and no sibling mutation`` () =
    let indexes=[number 0]
    let facts=arrayProgram [assign y (arrayRead indexes);Dim(arraySymbol,[number 10],pos);assign z (arrayRead indexes)] |> analyze
    Assert.That(facts.OneDimensionalIntegerAccesses, Is.Empty)
    let proc=SymbolId 100
    let resize=routine proc "RESIZE%" [] [Dim(arraySymbol,[number 0;number 0],pos)]
    let p=arrayProgram [Dim(arraySymbol,[number 10],pos);assign x (binary Add (arrayRead indexes) (CallFunc(proc,[],HirType.Int,pos)))]
    let effects={p with Routines=[{resize with ReturnType=Some HirType.Int}]} |> analyze
    Assert.That(effects.OneDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``rank-one facts select distinct checked and proven native integer records`` () =
    let safeIndexes=[number 0]
    let checkedIndexes=[read x]
    let p=arrayProgram [Dim(arraySymbol,[number 10],pos);assign y (arrayRead safeIndexes);assign z (arrayRead checkedIndexes)]
    let facts=analyze p
    let options={HirTurboIrBackend.defaultOptions with SafeArrayAccesses=facts.SafeArrayAccesses;OneDimensionalIntegerAccesses=facts.OneDimensionalIntegerAccesses}
    let ir=HirTurboIrBackend.generateTurboIrFromHir options p |> Result.defaultWith(String.concat "; " >> failwith) |> TurboNativeRegions.select true |> Result.defaultWith(String.concat "; " >> failwith)
    Assert.That(ir.Items |> List.exists(function HirTurboIrBackend.Instruction(1811,_,_) -> true | _ -> false), Is.True)
    Assert.That(ir.Items |> List.exists(function HirTurboIrBackend.Instruction(1711,_,_) -> true | _ -> false), Is.True)

[<Test>]
let ``dominating DIM and a bounded loop prove numeric reads and writes`` () =
    let indexes=[read i]
    let p=arrayProgram [Dim(arraySymbol,[number 10],pos);For(LoopId 1,i,number 0,number 10,number 1,
                        [Assign(WriteArrayElem(arraySymbol,indexes,HirType.Int,pos),number 7,pos);assign x (arrayRead indexes)],pos)]
    arraySafe indexes (analyze p)

[<Test>]
let ``an access shared with an unbounded occurrence retains all checks`` () =
    let indexes=[read i]
    arrayProgram [Dim(arraySymbol,[number 10],pos);For(LoopId 1,i,number 0,number 10,number 1,[assign x (arrayRead indexes)],pos);assign y (arrayRead indexes)]
    |> analyze |> arrayChecked indexes

[<Test>]
let ``INPUT and READ targets cannot inherit another occurrence's array proof`` () =
    let indexes=[read i]
    let target=WriteArrayElem(arraySymbol,indexes,HirType.Int,pos)
    arrayProgram [Dim(arraySymbol,[number 10],pos);assign i (number 0);Assign(target,number 1,pos)
                  Input(None,[],[WriteVar(i,HirType.Int,pos);target],pos);Read([target],pos)]
    |> analyze |> arrayChecked indexes

[<Test>]
let ``a jump past DIM cannot use storage declarations as reaching shape facts`` () =
    arrayProgram [Goto(number 30,pos);Dim(arraySymbol,[number 10],pos);LineNumber(30,pos);assign x (arrayRead [number 0])]
    |> analyze |> arrayChecked [number 0]

[<Test>]
let ``resizing on one branch joins the guaranteed minimum shape`` () =
    let p=arrayProgram [Dim(arraySymbol,[number 10],pos);If(read flag,[Dim(arraySymbol,[number 2],pos)],None,pos)
                        assign x (arrayRead [number 2]);assign y (arrayRead [number 3])]
    let facts=analyze p
    arraySafe [number 2] facts
    arrayChecked [number 3] facts

[<Test>]
let ``unknown writes discard array shape facts`` () =
    arrayProgram [Dim(arraySymbol,[number 10],pos);BuiltInCall(NamedBuiltIn "POKE",None,[number 1;number 0],pos);assign x (arrayRead [number 0])]
    |> analyze |> arrayChecked [number 0]

[<Test>]
let ``an effectful sibling cannot borrow pre-call array bounds or index ranges`` () =
    let proc=SymbolId 100
    let resize=routine proc "RESIZE%" [] [Dim(arraySymbol,[number 0],pos)]
    let call=CallFunc(proc,[],HirType.Int,pos)
    let p=arrayProgram [Dim(arraySymbol,[number 10],pos);assign x (binary Add (arrayRead [number 5]) call)]
    {p with Routines=[{resize with ReturnType=Some HirType.Int}]} |> analyze |> arrayChecked [number 5]

[<Test>]
let ``a terminal STOP inside a loop is not an arbitrary metadata mutation`` () =
    let expr=binary Add (read i) (number 1)
    let p=program [For(LoopId 1,i,number 0,number 10,number 1,[If(read flag,[BuiltInCall(NamedBuiltIn "STOP",None,[],pos)],None,pos);assign x expr],pos)]
    let facts=analyze p
    Assert.That((main facts).Graph.HasUncertainControl, Is.False)
    proven expr facts

[<Test>]
let ``safe array facts reach explicit native IR while unproven indexes retain checks`` () =
    let indexes=[read i]
    let p=arrayProgram [Dim(arraySymbol,[number 10],pos);For(LoopId 1,i,number 0,number 10,number 1,
                        [Assign(WriteArrayElem(arraySymbol,indexes,HirType.Int,pos),number 7,pos)
                         assign x (arrayRead [binary Add (read i) (number 1)])],pos)]
    let facts=analyze p
    let options={HirTurboIrBackend.defaultOptions with SafeIntegerExpressions=facts.SafeIntegerExpressions;SafeArrayAccesses=facts.SafeArrayAccesses}
    let selected=HirTurboIrBackend.generateTurboIrFromHir options p |> Result.bind(TurboNativeRegions.select true) |> Result.defaultWith(String.concat "; " >> failwith)
    Assert.That(selected.Items |> List.exists(function HirTurboIrBackend.Instruction(1612,_,_) -> true | _ -> false), Is.True)
    Assert.That(selected.Items |> List.exists(function HirTurboIrBackend.Instruction(1111,_,_) -> true | _ -> false), Is.True)

[<Test>]
let ``a diamond joins both reaching assignments`` () =
    let expr=binary Add (read x) (number 1)
    let a=program [If(read flag,[assign x (number 10)],Some[assign x (number 20)],pos);assign y expr] |> analyze
    assertRange 11L 21L expr a
    proven expr a
    for KeyValue(id,_) in (main a).Graph.Nodes do
        if (main a).Graph.Reachable.Contains id then Assert.That((main a).Graph.Dominators[id].Contains (main a).Graph.Entry, Is.True)

[<Test>]
let ``compound integer guards establish bounds without assuming HIR types`` () =
    let expr=binary Multiply (read x) (number 3)
    let condition=binary BitwiseAnd (binary GreaterThanOrEqual (read x) (number 0)) (binary LessThanOrEqual (read x) (number 100))
    let a=program [If(condition,[assign y expr],None,pos)] |> analyze
    assertRange 0L 300L expr a
    proven expr a

[<Test>]
let ``a jump into an IF body does not inherit the guard`` () =
    let expr=binary Add (read x) (number 1)
    let a=program [Goto(number 30,pos);If(binary LessThan (read x) (number 100),[LineNumber(30,pos);assign y expr],None,pos)] |> analyze
    unproven expr a
    let graph=(main a).Graph
    let condition=graph.Nodes |> Map.toList |> List.find (fun (_,n) -> match n.Kind with Test _ -> true | _ -> false) |> fst
    Assert.That(graph.Dominators[graph.Labels[30]].Contains condition, Is.False)

[<Test>]
let ``computed transfers include numbered entries and withhold unchecked proofs`` () =
    let expr=binary Add (read x) (number 1)
    let a=program [assign x (number 1);Goto(read flag,pos);LineNumber(30,pos);assign y expr] |> analyze
    Assert.That(a.ArithmeticProofsEnabled, Is.False)
    Assert.That((main a).Graph.Edges |> List.exists (fun e -> e.Kind=UnknownTransfer && e.Target=(main a).Graph.Labels[30] && e.Havoc), Is.True)
    unproven expr a

[<Test>]
let ``GOSUB returns have conservative continuation edges`` () =
    let a=program [Gosub(number 100,pos);print(read x);Goto(number 200,pos);LineNumber(100,pos);assign x (number 7);Return(None,pos);LineNumber(200,pos)] |> analyze
    Assert.That((main a).Graph.Edges |> List.exists (fun e -> e.Kind=SubroutineReturn && e.Havoc), Is.True)
    Assert.That(a.ArithmeticProofsEnabled, Is.False)

[<Test>]
let ``STOP has no fallthrough into unlabelled code`` () =
    let expr=binary Add (read x) (number 1)
    let a=program [BuiltInCall(NamedBuiltIn "STOP",None,[],pos);assign y expr] |> analyze
    let id=(main a).Graph.Nodes |> Map.toList |> List.find (fun (_,n) -> match n.Kind with Statement(Assign _) -> true | _ -> false) |> fst
    Assert.That((main a).EntryRanges[id].IsNone, Is.True)
    unproven expr a

[<TestCase(1,10,1,3,30)>]
[<TestCase(-1,-10,-1,-30,-3)>]
let ``counted loops retain guard bounds after widening`` (first:int,last:int,step:int,low:int,high:int) =
    let expr=binary Multiply (read i) (number 3)
    let a=program [For(LoopId 1,i,number first,number last,number step,[assign y expr],pos)] |> analyze
    assertRange (int64 low) (int64 high) expr a
    proven expr a
    Assert.That((main a).Graph.Loops, Is.Not.Empty)
    for loop in (main a).Graph.Loops do Assert.That((main a).Graph.Dominators[loop.Latch].Contains loop.Header, Is.True)

[<Test>]
let ``FOR counter wrap is not mistaken for a bounded mathematical iterator`` () =
    let expr=binary Add (read i) (number 1)
    let a=program [For(LoopId 1,i,number 32767,number 32767,number 1,[assign y expr],pos)] |> analyze
    unproven expr a
    assertRange word.Lower word.Upper expr a

[<Test>]
let ``NEXT and EXIT target the requested enclosing loop`` () =
    let outer,inner=LoopId 1,LoopId 2
    let a=program [Repeat(outer,AnonymousLoop,[Repeat(inner,AnonymousLoop,[Next(outer,pos);Exit(inner,pos)],pos);Exit(outer,pos)],pos)] |> analyze
    let graph=(main a).Graph
    let next=graph.Nodes |> Map.toList |> List.find (fun (_,n) -> match n.Kind with Statement(Next _) -> true | _ -> false) |> fst
    let destination=graph.Edges |> List.find (fun e -> e.Source=next) |> _.Target
    Assert.That(graph.Nodes[destination].Kind, Is.EqualTo(Join "REPeat"))
    Assert.That(graph.HasUncertainControl, Is.False)

[<Test>]
let ``unbounded repeats converge and do not prove eventual overflow safe`` () =
    let expr=binary Add (read x) (number 1)
    let a=program [assign x (number 0);Repeat(LoopId 1,AnonymousLoop,[assign x expr],pos)] |> analyze
    unproven expr a
    Assert.That((main a).Graph.Edges |> List.filter (fun e -> e.Target=(main a).Graph.Exit) |> List.forall (fun e -> e.Kind=Exceptional), Is.True)

[<Test>]
let ``irreducible numbered loops converge without assuming a structured header`` () =
    let plusOne=binary Add (read x) (number 1)
    let plusTwo=binary Add (read x) (number 2)
    let a=program [assign x (number 0);If(read flag,[Goto(number 20,pos)],None,pos)
                   LineNumber(10,pos);assign x plusOne;Goto(number 30,pos)
                   LineNumber(20,pos);assign x plusTwo;LineNumber(30,pos)
                   If(read flag,[Goto(number 10,pos)],Some[Goto(number 20,pos)],pos)] |> analyze
    Assert.That(a.ArithmeticProofsEnabled, Is.True)
    unproven plusOne a
    unproven plusTwo a

[<Test>]
let ``runtime loop bounds are not reread after the body changes them`` () =
    let expr=binary Multiply (read i) (number 3)
    let a=program [assign x (number 10);For(LoopId 1,i,number 1,read x,number 1,[assign x (number 1);assign y expr],pos)] |> analyze
    proven expr a
    assertRange 3L 30L expr a

[<Test>]
let ``a callee that replaces a FOR vector invalidates its hidden continuation`` () =
    let proc=SymbolId 100
    let clobber=routine proc "CLOBBER" [] [For(LoopId 2,i,number 1,number 1000,number 1,[],pos)]
    let expr=binary Multiply (read i) (number 3)
    let p=program [For(LoopId 1,i,number 1,number 10,number 1,[ProcCall(proc,None,[],pos);assign y expr],pos)]
    let a={p with Routines=[clobber]} |> analyze
    Assert.That(a.ArithmeticProofsEnabled, Is.False)
    Assert.That((main a).Graph.Edges |> List.exists (fun e -> e.Kind=UnknownTransfer && e.Havoc), Is.True)
    unproven expr a

[<Test>]
let ``same expression at different program points requires all occurrences safe`` () =
    let expr=binary Add (read x) (number 1)
    let a=program [assign x (number 1);assign y expr;ProcCall(SymbolId 999,None,[],pos);assign z expr] |> analyze
    Assert.That(ranges MainScope expr a |> List.contains (Some {Lower=2L;Upper=2L}), Is.True)
    Assert.That(ranges MainScope expr a |> List.contains (Some word), Is.True)
    unproven expr a

[<TestCase(false)>]
[<TestCase(true)>]
let ``known call effects distinguish a reference from a private value`` (byReference:bool) =
    let proc,p=SymbolId 100,SymbolId 101
    let setter=routine proc "SETTER" [parameter "SETTER" p "P%"] [assign p (number 20000)]
    let arg=if byReference then RefArg(target x) else ValueArg(read x)
    let expr=binary Add (read x) (number 1)
    let a={program [assign x (number 1);ProcCall(proc,None,[arg],pos);assign y expr] with Routines=[setter]} |> analyze
    if byReference then unproven expr a else proven expr a
    let effect=HirEffects.call a.Effects MainScope proc [arg]
    Assert.That(effect.Writes.Contains x, Is.EqualTo(byReference))

[<Test>]
let ``independent globals do not alias through a shared possible parameter`` () =
    let proc,p=SymbolId 100,SymbolId 101
    let setter=routine proc "SETTER" [parameter "SETTER" p "P%"] [assign p (number 2)]
    let a={program [] with Routines=[setter]} |> analyze
    Assert.That(mayAlias a.Effects.Aliases (RoutineScope proc) p x, Is.True)
    Assert.That(mayAlias a.Effects.Aliases (RoutineScope proc) p y, Is.True)
    Assert.That(mayAlias a.Effects.Aliases (RoutineScope proc) x y, Is.False)

[<Test>]
let ``writing one possible reference alias invalidates another parameter`` () =
    let proc,p,q=SymbolId 100,SymbolId 101,SymbolId 102
    let expr=binary Add (read p) (number 1)
    let r=routine proc "ALIAS" [parameter "ALIAS" p "P%";parameter "ALIAS" q "Q%"] [assign p (number 1);assign q (number 32767);Return(Some expr,pos)]
    let a={program [] with Routines=[r]} |> analyze
    unproven expr a

[<Test>]
let ``a known global write preserves unrelated scalar facts`` () =
    let proc=SymbolId 100
    let setter=routine proc "SETTER" [] [assign z (number 99)]
    let expr=binary Add (read x) (number 1)
    let a={program [assign x (number 1);ProcCall(proc,None,[],pos);assign y expr] with Routines=[setter]} |> analyze
    proven expr a
    Assert.That(a.Effects.Summaries[proc].Writes.Contains z, Is.True)
    Assert.That(a.Effects.Summaries[proc].Writes.Contains x, Is.False)

[<Test>]
let ``recursive call summaries reach a fixed point`` () =
    let a,b=SymbolId 100,SymbolId 200
    let first=routine a "FIRST" [] [ProcCall(b,None,[],pos)]
    let second=routine b "SECOND" [] [assign x (number 5);ProcCall(a,None,[],pos)]
    let result={program [] with Routines=[first;second]} |> analyze
    Assert.That(result.Effects.Summaries[a].Writes.Contains x, Is.True)
    Assert.That(result.Effects.Summaries[a].Calls.Contains a, Is.True)
    Assert.That(result.Effects.Summaries[b].Calls.Contains b, Is.True)

[<Test>]
let ``a known empty call still allocates a frame and may fail binding`` () =
    let proc=SymbolId 100
    let a={program [] with Routines=[routine proc "EMPTY" [] []]} |> analyze
    let effect=HirEffects.call a.Effects MainScope proc []
    Assert.That(effect.MayThrow && effect.Allocates, Is.True)

[<Test>]
let ``unknown residents invalidate facts and record escaping references`` () =
    let expr=binary Add (read x) (number 1)
    let a=program [assign x (number 1);ProcCall(SymbolId 999,None,[RefArg(target x)],pos);assign y expr] |> analyze
    Assert.That(a.Effects.Aliases.AddressTaken.Contains x, Is.True)
    Assert.That(a.Effects.Aliases.Escaped.Contains x, Is.True)
    unproven expr a

[<Test>]
let ``escape facts propagate through known forwarding routines`` () =
    let proc,p=SymbolId 100,SymbolId 101
    let forward=routine proc "FORWARD" [parameter "FORWARD" p "P%"] [ProcCall(SymbolId 999,None,[RefArg(target p)],pos)]
    let a={program [ProcCall(proc,None,[RefArg(target x)],pos);ProcCall(proc,None,[ValueArg(read y)],pos)] with Routines=[forward]} |> analyze
    Assert.That(a.Effects.Aliases.Escaped.Contains p, Is.True)
    Assert.That(a.Effects.Aliases.Escaped.Contains x, Is.True)
    Assert.That(a.Effects.Aliases.Escaped.Contains y, Is.False)

[<Test>]
let ``an array element reference maps writes to the owning array`` () =
    let proc,p,array=SymbolId 100,SymbolId 101,SymbolId 102
    let setter=routine proc "SETTER" [parameter "SETTER" p "P%"] [assign p (number 7)]
    let arr={storage array "A%" GlobalStorage with Type=Array Int;Dimensions=Some[10]}
    let a={program [] with Globals=arr::globals;Routines=[setter]} |> analyze
    let effect=HirEffects.call a.Effects MainScope proc [RefArg(WriteArrayElem(array,[number 1],Int,pos))]
    Assert.That(effect.Writes.Contains array, Is.True)

[<Test>]
let ``liveness never treats a call MAY write as a definite kill`` () =
    let proc=SymbolId 100
    let setter=routine proc "SETTER" [] [If(read flag,[assign x (number 1)],None,pos)]
    let a={program [ProcCall(proc,None,[],pos);print(read x)] with Routines=[setter]} |> analyze
    let id=(main a).Graph.Nodes |> Map.toList |> List.find (fun (_,n) -> match n.Kind with Statement(ProcCall _) -> true | _ -> false) |> fst
    Assert.That((main a).Liveness.LiveIn[id].Contains x, Is.True)

[<Test>]
let ``unknown writes during expression evaluation defeat arithmetic proofs`` () =
    let call=CallFunc(SymbolId 999,[],Int,pos)
    let expr=binary Multiply (binary Add (read x) call) (number 0)
    let a=program [assign x (number 1);assign y expr] |> analyze
    unproven expr a

[<Test>]
let ``target suffix and IMPLICIT typing take precedence over advisory HIR Int`` () =
    let expr=binary Add (read x) (number 1)
    let p={program [assign x (number 1);assign y expr] with Globals=[storage x "X" GlobalStorage;storage y "Y%" GlobalStorage]}
    unproven expr (analyze p)
    let target={HirTurboIrBackend.defaultOptions with ImplicitIntegers=Set.singleton "X"}
    proven expr (HirAnalysis.analyze target p)

[<Test>]
let ``exception handlers include havoc edges and retain checked arithmetic`` () =
    let expr=binary Add (read x) (number 1)
    let a=program [WhenError([print(number 1)],pos);assign x (number 1);assign y expr] |> analyze
    Assert.That((main a).Graph.Edges |> List.exists (fun e -> e.Kind=Exceptional && e.Havoc), Is.True)
    let resume=(main a).Graph.Nodes |> Map.toList |> List.find (fun (_,n) -> n.Kind=Join "implicit CONTINUE") |> fst
    Assert.That((main a).Graph.Edges |> List.exists (fun e -> e.Source=resume && e.Kind=UnknownTransfer && e.Havoc), Is.True)
    unproven expr a

[<Test>]
let ``effects expose failures allocation IO and dynamic memory writes`` () =
    let a=analyze(program [])
    let io=HirEffects.statement a.Effects MainScope (print(number 1))
    Assert.That(io.InputOutput && io.MayThrow && io.Allocates, Is.True)
    let dynamic=HirEffects.statement a.Effects MainScope (Assign(DynamicWriteVar("X%",Int,pos),number 1,pos))
    Assert.That(dynamic.WritesUnknown, Is.True)
    let arithmetic=HirEffects.expression a.Effects MainScope (binary Add (read x) (read y))
    Assert.That(arithmetic.MayThrow, Is.True)
    let text=HirEffects.expression a.Effects MainScope (Literal(ConstString "x",String,pos))
    Assert.That(text.MayThrow && text.Allocates, Is.True)

[<Test>]
let ``analysis reports are deterministic and include reusable facts`` () =
    let p=program [assign x (number 1);assign y (binary Add (read x) (number 1))]
    let first,second=analyze p |> HirAnalysis.format,analyze p |> HirAnalysis.format
    Assert.That(first, Is.EqualTo(second))
    for marker in ["reads=";"writes=";"live-in=";"dominators=";"proven expressions:"] do Assert.That(first, Does.Contain(marker))

[<Test>]
let ``range proofs are consumed by native instruction selection`` () =
    let expr=binary Add (read x) (number 1)
    let p=program [assign x (number 7);assign y expr]
    let facts=analyze p
    let options={HirTurboIrBackend.defaultOptions with SafeIntegerExpressions=facts.SafeIntegerExpressions}
    let ir=HirTurboIrBackend.generateTurboIrFromHir options p |> Result.defaultWith (String.concat "; " >> failwith)
    let native=TurboNativeRegions.select true ir |> Result.defaultWith (String.concat "; " >> failwith)
    let mutable mode=false
    let mutable nativeAdd=false
    for item in native.Items do
        match item with
        | HirTurboIrBackend.Instruction(254,_,_) -> mode<-true
        | HirTurboIrBackend.Instruction(255,_,_) -> mode<-false
        | HirTurboIrBackend.Instruction(5,_,comment) when mode ->
            nativeAdd<-true
            Assert.That(comment, Is.EqualTo(HirTurboIrBackend.rangeProvenIntegerComment))
        | _ -> ()
    Assert.That(nativeAdd, Is.True)
