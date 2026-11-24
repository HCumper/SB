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

let private localCounter=SymbolId 80
let private localRoutine body =
    let work=routine (SymbolId 81) "WORK" [] body
    {work with Locals=[storage localCounter "I%" (RoutineLocalStorage "WORK")]}
let private localLoop body=For(LoopId 80,localCounter,number 1,number 10,number 1,body,pos)
let private localProofs work = analyze {program [] with Routines=[work]} |> _.CertifiedNativeForLoops

[<Test>]
let ``local FOR certificates keep a fresh LOCAL counter distinct from its global shadow`` () =
    let work=localRoutine [localLoop [assign x (binary Add (read localCounter) (read y))]]
    let proofs=localProofs work
    Assert.That(proofs.Count, Is.EqualTo(1))
    Assert.That(proofs[Some work.Symbol,LoopId 80].Scalars, Is.EquivalentTo(["I%";"X%";"Y%"]))
    Assert.That(proofs[Some work.Symbol,LoopId 80].InvariantScalars, Is.EquivalentTo(["Y%"]))

[<Test>]
let ``local FOR proof metadata does not change the serialized Turbo IR`` () =
    let work=localRoutine [localLoop [assign x (read localCounter)]]
    let p={program [ProcCall(work.Symbol,None,[],pos)] with Routines=[work]}
    let generate options=HirTurboIrBackend.generateTurboIrFromHir options p |> Result.defaultWith(String.concat "; " >> failwith)
    let plain=generate HirTurboIrBackend.defaultOptions
    let proven=generate {HirTurboIrBackend.defaultOptions with CertifiedNativeForLoops=(analyze p).CertifiedNativeForLoops}
    Assert.That(proven.Bytes, Is.EqualTo(box plain.Bytes))
    Assert.That(proven.Listing, Is.EqualTo(plain.Listing))
    Assert.That(plain.NativeForLoopProofs, Is.Empty)
    Assert.That(proven.NativeForLoopProofs.Count, Is.EqualTo(1))

[<Test>]
let ``local FOR certificates permit parameters in captured bounds but not body aliases`` () =
    let formal=parameter "WORK" (SymbolId 82) "LIMIT%"
    let work={localRoutine [] with Parameters=[formal]}
    let safe={work with Body=[For(LoopId 80,localCounter,number 1,read formal.Storage.Symbol,number 1,[assign x (read localCounter)],pos)]}
    Assert.That((localProofs safe).Count, Is.EqualTo(1))
    let unsafe={work with Body=[localLoop [assign x (read formal.Storage.Symbol)]]}
    Assert.That(localProofs unsafe, Is.Empty)

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
let ``local FOR certificates reject counter writes and calls in conditional bodies`` (kind:int) =
    let body=match kind with
             | 0 -> [assign localCounter (number 2)]
             | 1 -> [Assign(DynamicWriteVar("I%",HirType.Int,pos),number 2,pos)]
             | 2 -> [ProcCall(SymbolId 99,None,[],pos)]
             | _ -> [If(read flag,[assign localCounter (number 1)],None,pos)]
    Assert.That(localProofs (localRoutine [localLoop body]), Is.Empty)

[<Test>]
let ``conditional FOR binding certificates include condition and both arm effects`` () =
    let work=localRoutine [localLoop [If(binary LessThan (read localCounter) (read y),[assign x (number 1)],Some[assign z (number 2)],pos)]]
    let proof=(localProofs work)[Some work.Symbol,LoopId 80]
    Assert.That(proof.Scalars, Is.EquivalentTo(["I%";"X%";"Y%";"Z%"]))
    Assert.That(proof.InvariantScalars, Is.EquivalentTo(["Y%"]))

[<Test>]
let ``local FOR certificates reject formal counters and escaping LOCAL references`` () =
    let formal=parameter "WORK" localCounter "I%"
    let work={localRoutine [localLoop [assign x (read localCounter)]] with Locals=[];Parameters=[formal]}
    Assert.That(localProofs work, Is.Empty)
    let escaping=localRoutine [ProcCall(SymbolId 99,None,[RefArg(target localCounter)],pos);localLoop [assign x (read localCounter)]]
    Assert.That(localProofs escaping, Is.Empty)

[<Test>]
let ``per-loop certificates permit independent globals in programs with other LOCAL bindings`` () =
    let work=localRoutine [assign localCounter (number 99)]
    let p={program [For(LoopId 90,x,number 1,number 3,number 1,[assign y (read x)],pos)] with Routines=[work]}
    Assert.That((analyze p).CertifiedNativeForLoops.ContainsKey(None,LoopId 90), Is.True)
    let shadow={work with Locals=[storage localCounter "X%" (RoutineLocalStorage "WORK")]}
    Assert.That((analyze {p with Routines=[shadow]}).CertifiedNativeForLoops, Is.Empty)

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
let ``local GOSUB uncertainty cannot disable proofs in an independent procedure`` () =
    let expression=binary Multiply (read localCounter) (number 3)
    let work=localRoutine [localLoop [assign x expression]]
    let p={program [Gosub(number 100,pos);Goto(number 200,pos);LineNumber(100,pos);Return(None,pos);LineNumber(200,pos)] with Routines=[work]}
    let facts=analyze p
    Assert.That(facts.ProofEligibleScopes.Contains MainScope, Is.False)
    Assert.That(facts.ProofEligibleScopes.Contains(RoutineScope work.Symbol), Is.True)
    proven expression facts
    Assert.That(facts.CertifiedNativeForLoops.ContainsKey(Some work.Symbol,LoopId 80), Is.True)

[<Test>]
let ``a native loop region can certify despite local GOSUB elsewhere in its scope`` () =
    let body=[Gosub(number 100,pos);For(LoopId 90,i,number 1,number 3,number 1,[assign x (read i)],pos)
              Goto(number 200,pos);LineNumber(100,pos);Return(None,pos);LineNumber(200,pos)]
    let facts=analyze(program body)
    Assert.That(facts.ProofEligibleScopes.Contains MainScope, Is.False)
    Assert.That(facts.CertifiedNativeForLoops.ContainsKey(None,LoopId 90), Is.True)

[<Test>]
let ``native loop binding proof rejects a numbered entry that bypasses its initialiser`` () =
    let p=program [Goto(number 30,pos);For(LoopId 90,i,number 1,number 3,number 1,[LineNumber(30,pos);assign x (read i)],pos)]
    let facts=analyze p
    Assert.That(facts.CertifiedNativeForLoops, Is.Empty)
    Assert.That(facts.NativeForRejections[None,LoopId 90], Does.Contain("loop has an external entry or untrusted FOR continuation"))

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
[<TestCase(3)>]
[<TestCase(4)>]
let ``persistent and nonlocal hazards still block native certificates in other procedures`` (kind:int) =
    let work=localRoutine [localLoop [assign x (read localCounter)]]
    let other=match kind with
              | 0 -> [WhenError([Return(None,pos)],pos)]
              | 1 -> [BuiltInCall(NamedBuiltIn "POKE",None,[number 100;number 0],pos)]
              | 2 -> [Goto(read flag,pos)]
              | 3 -> [Goto(number 30,pos)]
              | _ -> [assign z (CallFunc(SymbolId 999,[],HirType.Int,pos))]
    let work={work with Body=LineNumber(30,pos)::work.Body}
    let hazard=routine (SymbolId 99) "HAZARD" [] other
    let facts=analyze {program [] with Routines=[work;hazard]}
    Assert.That(facts.CertifiedNativeForLoops, Is.Empty)
    Assert.That(facts.GlobalControlBarriers, Is.Not.Empty)
    Assert.That(facts.NativeForRejections.ContainsKey(Some work.Symbol,LoopId 80), Is.True)

[<Test>]
let ``ABS has nonmutating ROM effects but a user ABS retains its actual writes`` () =
    let absSymbol=SymbolId 999
    let p={program [] with SymbolNames=Map.ofList[absSymbol,"ABS"]}
    let context=HirEffects.analyze p
    let effect=HirEffects.call context MainScope absSymbol [ValueArg(read x)]
    Assert.That(effect.Reads.Contains x && effect.MayThrow && effect.Calls.Contains absSymbol, Is.True)
    Assert.That(effect.WritesUnknown || effect.DescriptorWritesUnknown || effect.LoopMetadataWritesUnknown, Is.False)
    let own=routine absSymbol "ABS" [] [assign x (number 7)]
    let context=HirEffects.analyze {p with Routines=[own]}
    Assert.That((HirEffects.call context MainScope absSymbol []).Writes.Contains x, Is.True)

[<Test>]
let ``independent loop certificates survive an unrelated enumerated FOR scope`` () =
    let enumeration=ForSequence(LoopId 1,i,[],number 1,number 3,[number 5],number 1,[],pos)
    let work=localRoutine [localLoop [assign x (read localCounter)]]
    let facts=analyze {program [enumeration] with Routines=[work]}
    Assert.That(facts.ProofEligibleScopes.Contains MainScope, Is.False)
    Assert.That(facts.CertifiedNativeForLoops.ContainsKey(Some work.Symbol,LoopId 80), Is.True)

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

[<TestCase(false)>]
[<TestCase(true)>]
let ``InvariantArray certificates distinguish content writes from row and descriptor mutations`` (changing:bool) =
    let indexes=[read y;read i]
    let body=[Assign(WriteArrayElem(arraySymbol,indexes,HirType.Int,pos),read i,pos);assign x (arrayRead indexes)] @
             (if changing then [If(read flag,[assign y (number 2)],None,pos)] else [])
    let p=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);For(LoopId 1,i,number 0,number 7,number 1,body,pos)]
    let proof=(analyze p).CertifiedNativeForLoops[None,LoopId 1]
    Assert.That(proof.InvariantArrays, Is.EquivalentTo(["A%"]))
    Assert.That(proof.InvariantScalars.Contains "Y%", Is.EqualTo(not changing))
    Assert.That(proof.InvariantScalars.Contains "I%" || proof.InvariantScalars.Contains "X%", Is.False)
    let resizing=arrayProgram [For(LoopId 1,i,number 0,number 7,number 1,[Dim(arraySymbol,[number 3;number 7],pos)],pos)]
    Assert.That((analyze resizing).CertifiedNativeForLoops, Is.Empty)

[<Test>]
let ``InvariantArray rejects row invariance through a same name LOCAL alias`` () =
    let shadow=SymbolId 89
    let work=localRoutine [localLoop [assign shadow (number 1);assign x (arrayRead [read y;read localCounter])]]
    let work={work with Locals=work.Locals@[storage shadow "Y%" (RoutineLocalStorage "WORK")]}
    let p={arrayProgram [] with Routines=[work]}
    let proof=(analyze p).CertifiedNativeForLoops[Some work.Symbol,LoopId 80]
    Assert.That(proof.InvariantScalars.Contains "Y%", Is.False)

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
    Assert.That(a.ProofEligibleScopes.Contains MainScope, Is.False)
    Assert.That(a.ProofEligibleScopes.Contains(RoutineScope proc), Is.True)
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
let ``a formal passed only scalar values is private, one reference site makes it alias`` () =
    let proc,p=SymbolId 100,SymbolId 101
    let setter=routine proc "SETTER" [parameter "SETTER" p "P%"] [assign y (number 2)]
    let byValue={program [ProcCall(proc,None,[ValueArg(number 1)],pos);ProcCall(proc,None,[ValueArg(binary Add (read x) (number 1))],pos)] with Routines=[setter]} |> analyze
    Assert.That(mayAlias byValue.Effects.Aliases (RoutineScope proc) p x, Is.False)
    Assert.That(mayAlias byValue.Effects.Aliases (RoutineScope proc) p y, Is.False)
    let mixed={program [ProcCall(proc,None,[ValueArg(number 1)],pos);ProcCall(proc,None,[RefArg(target x)],pos)] with Routines=[setter]} |> analyze
    Assert.That(mayAlias mixed.Effects.Aliases (RoutineScope proc) p x, Is.True)
    let missing={program [ProcCall(proc,None,[],pos)] with Routines=[setter]} |> analyze
    Assert.That(mayAlias missing.Effects.Aliases (RoutineScope proc) p x, Is.True)

[<Test>]
let ``a private formal keeps its entry range across writes to globals`` () =
    // Without privacy, the global write would be treated as a possible write
    // through the formal and discard its imported range.
    let proc,p=SymbolId 100,SymbolId 101
    let sum=binary Add (read p) (number 32000)
    let worker=routine proc "WORK" [parameter "WORK" p "P%"] [assign y (number 5);assign z sum]
    let facts={program [ProcCall(proc,None,[ValueArg(number 3)],pos)] with Routines=[worker]} |> analyze
    proven sum facts

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
    Assert.That(dynamic.WritesUnknown, Is.False)
    Assert.That(dynamic.Writes, Is.EqualTo(box(Set.singleton x)))
    let unresolved=HirEffects.statement a.Effects MainScope (Assign(DynamicWriteVar("UNDECLARED%",Int,pos),number 1,pos))
    Assert.That(unresolved.WritesUnknown && unresolved.DescriptorWritesUnknown, Is.True)
    let arithmetic=HirEffects.expression a.Effects MainScope (binary Add (read x) (read y))
    Assert.That(arithmetic.MayThrow, Is.True)
    let text=HirEffects.expression a.Effects MainScope (Literal(ConstString "x",String,pos))
    Assert.That(text.MayThrow && text.Allocates, Is.True)

[<Test>]
let ``procedure array effects resolve an unshadowed global and preserve exact bounds`` () =
    let proc=SymbolId 100
    let indexes=[read i;number 2]
    let writer=routine proc "PUT" [] [Assign(DynamicWriteArrayElem("a%",[number 0;number 2],Int,pos),number 7,pos)]
    let p=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos)
                        For(LoopId 1,i,number 0,number 3,number 1,[ProcCall(proc,None,[],pos);assign y (arrayRead indexes)],pos)]
    let facts={p with Routines=[writer]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc []
    Assert.That(effect.Writes, Is.EqualTo(box(Set.singleton arraySymbol)))
    Assert.That(effect.WritesUnknown || effect.DescriptorWritesUnknown || effect.LoopMetadataWritesUnknown, Is.False)
    Assert.That(effect.DescriptorWrites.IsEmpty && effect.LoopMetadataWrites.IsEmpty, Is.True)
    Assert.That(facts.ArithmeticProofsEnabled, Is.True)
    arraySafe indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses["A%",indexes].ExactLengths, Is.EqualTo(Some(4,8)))
    let readEffect=HirEffects.expression facts.Effects (RoutineScope proc) (DynamicReadArrayElem("A%",indexes,Int,pos))
    Assert.That(readEffect.Reads.Contains arraySymbol, Is.True)
    Assert.That(readEffect.ReadsUnknown, Is.False)

[<TestCase(false)>]
[<TestCase(true)>]
let ``procedure array formal element writes preserve the actual descriptor`` (valueArgument:bool) =
    let proc,p=SymbolId 100,SymbolId 101
    let formal={parameter "PUT" p "P%" with Storage={storage p "P%" (RoutineParameterStorage "PUT") with Type=Array Int}}
    let writer=routine proc "PUT" [formal] [Assign(WriteArrayElem(p,[number 0],Int,pos),number 7,pos)]
    let arg=if valueArgument then ValueArg(ReadVar(arraySymbol,Array Int,pos)) else RefArg(WriteVar(arraySymbol,Array Int,pos))
    let source=arrayProgram [Dim(arraySymbol,[number 10],pos);ProcCall(proc,None,[arg],pos);assign y (arrayRead [number 10])]
    let facts={source with Routines=[writer]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc [arg]
    Assert.That(effect.Writes.Contains arraySymbol, Is.True)
    Assert.That(effect.DescriptorWrites.IsEmpty && effect.LoopMetadataWrites.IsEmpty, Is.True)
    arraySafe [number 10] facts

[<Test>]
let ``procedure indexed formals forward arrays even when HIR gives them advisory scalar types`` () =
    let outer,inner,p,q=SymbolId 100,SymbolId 101,SymbolId 102,SymbolId 103
    let put=routine inner "PUT" [parameter "PUT" q "Q%"] [Assign(WriteArrayElem(q,[number 0],Int,pos),number 7,pos)]
    let forward=routine outer "FORWARD" [parameter "FORWARD" p "P%"] [Assign(WriteArrayElem(p,[number 0],Int,pos),number 1,pos);ProcCall(inner,None,[ValueArg(read p)],pos)]
    let source=arrayProgram [Dim(arraySymbol,[number 10],pos);ProcCall(outer,None,[RefArg(WriteVar(arraySymbol,Array Int,pos))],pos);assign y (arrayRead [number 10])]
    let facts={source with Routines=[put;forward]} |> analyze
    Assert.That(facts.Effects.Aliases.ArrayStorages.Contains p && facts.Effects.Aliases.ArrayStorages.Contains q, Is.True)
    let forwarded=HirEffects.call facts.Effects (RoutineScope outer) inner [ValueArg(read p)]
    Assert.That(forwarded.Writes.Contains p, Is.True)
    Assert.That(forwarded.DescriptorWrites.IsEmpty && forwarded.LoopMetadataWrites.IsEmpty, Is.True)
    arraySafe [number 10] facts

[<Test>]
let ``procedure array resizing through a formal discards minimum and exact bounds`` () =
    let proc,p=SymbolId 100,SymbolId 101
    let formal={parameter "RESIZE" p "P%" with Storage={storage p "P%" (RoutineParameterStorage "RESIZE") with Type=Array Int}}
    let writer=routine proc "RESIZE" [formal] [Dim(p,[number 0;number 0],pos)]
    let arg=RefArg(WriteVar(arraySymbol,Array Int,pos))
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[arg],pos);assign y (arrayRead [number 1;number 2])]
    let facts={source with Routines=[writer]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc [arg]
    Assert.That(effect.DescriptorWrites.Contains arraySymbol, Is.True)
    arrayChecked [number 1;number 2] facts
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``procedure element references change contents without resizing the owning array`` () =
    let proc,p=SymbolId 100,SymbolId 101
    let setter=routine proc "SET" [parameter "SET" p "P%"] [assign p (number 7)]
    let arg=RefArg(WriteArrayElem(arraySymbol,[number 0],Int,pos))
    let source=arrayProgram [Dim(arraySymbol,[number 10],pos);ProcCall(proc,None,[arg],pos);assign y (arrayRead [number 10])]
    let facts={source with Routines=[setter]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc [arg]
    Assert.That(effect.Writes.Contains arraySymbol, Is.True)
    Assert.That(effect.DescriptorWrites.IsEmpty && effect.LoopMetadataWrites.IsEmpty, Is.True)
    arraySafe [number 10] facts

[<TestCase(false)>]
[<TestCase(true)>]
let ``procedure scalar writes preserve FOR control but invalidate the referenced counter value`` (byReference:bool) =
    let proc,p=SymbolId 100,SymbolId 101
    let setter=routine proc "SET" [parameter "SET" p "P%"] [assign p (number 32767)]
    let arg=if byReference then RefArg(target i) else ValueArg(read i)
    let expr=binary Multiply (read i) (number 3)
    let source=program [For(LoopId 1,i,number 1,number 3,number 1,[ProcCall(proc,None,[arg],pos);assign y expr],pos)]
    let facts={source with Routines=[setter]} |> analyze
    Assert.That(facts.ArithmeticProofsEnabled && (main facts).ForMetadataTrusted, Is.True)
    let effect=HirEffects.call facts.Effects MainScope proc [arg]
    Assert.That(effect.LoopMetadataWrites.IsEmpty && not effect.LoopMetadataWritesUnknown, Is.True)
    if byReference then unproven expr facts else proven expr facts

[<TestCase(false)>]
[<TestCase(true)>]
let ``procedure FOR initialization maps hidden metadata writes to references only`` (byReference:bool) =
    let proc,p=SymbolId 100,SymbolId 101
    let setter=routine proc "LOOP" [parameter "LOOP" p "P%"] [For(LoopId 2,p,number 1,number 2,number 1,[],pos)]
    let arg=if byReference then RefArg(target i) else ValueArg(read i)
    let source=program [For(LoopId 1,i,number 1,number 3,number 1,[ProcCall(proc,None,[arg],pos)],pos)]
    let facts={source with Routines=[setter]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc [arg]
    Assert.That(effect.LoopMetadataWrites.Contains i, Is.EqualTo(byReference))
    Assert.That(facts.ProofEligibleScopes.Contains MainScope, Is.EqualTo(not byReference))

[<Test>]
let ``procedure private LOCAL bindings do not leak metadata writes to a caller's same-named vector`` () =
    let proc,localI,localA=SymbolId 100,SymbolId 101,SymbolId 102
    let localCounter=storage localI "I%" (RoutineLocalStorage "PRIVATE")
    let localArray={storage localA "A%" (RoutineLocalStorage "PRIVATE") with Type=Array Int;Dimensions=Some[1]}
    let worker={routine proc "PRIVATE" [] [Dim(localA,[number 1],pos);For(LoopId 2,localI,number 1,number 2,number 1,[],pos)] with Locals=[localCounter;localArray]}
    let expr=binary Multiply (read i) (number 3)
    let source=arrayProgram [Dim(arraySymbol,[number 10],pos);For(LoopId 1,i,number 1,number 3,number 1,[ProcCall(proc,None,[],pos);assign y expr;assign z (arrayRead [number 10])],pos)]
    let facts={source with Routines=[worker]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc []
    Assert.That(effect.Writes.IsEmpty && effect.DescriptorWrites.IsEmpty && effect.LoopMetadataWrites.IsEmpty, Is.True)
    Assert.That(facts.ArithmeticProofsEnabled, Is.True)
    proven expr facts
    arraySafe [number 10] facts

[<Test>]
let ``procedure free names that a caller LOCAL can shadow remain conservative`` () =
    let proc,caller,localA=SymbolId 100,SymbolId 101,SymbolId 102
    let localArray={storage localA "A%" (RoutineLocalStorage "CALLER") with Type=Array Int;Dimensions=Some[1]}
    let writer=routine proc "RESIZE" [] [Dim(arraySymbol,[number 0],pos)]
    let callerRoutine={routine caller "CALLER" [] [ProcCall(proc,None,[],pos)] with Locals=[localArray]}
    let source=arrayProgram [Dim(arraySymbol,[number 10],pos);ProcCall(proc,None,[],pos);assign y (arrayRead [number 10])]
    let facts={source with Routines=[writer;callerRoutine]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc []
    Assert.That(effect.DescriptorWritesUnknown && effect.LoopMetadataWritesUnknown, Is.True)
    arrayChecked [number 10] facts

[<Test>]
let ``procedure unresolved array content writes retain unknown value effects without inventing metadata changes`` () =
    let proc,caller,localA=SymbolId 100,SymbolId 101,SymbolId 102
    let localArray={storage localA "A%" (RoutineLocalStorage "CALLER") with Type=Array Int;Dimensions=Some[1]}
    let writer=routine proc "PUT" [] [Assign(DynamicWriteArrayElem("A%",[number 0],Int,pos),number 7,pos)]
    let callerRoutine={routine caller "CALLER" [] [] with Locals=[localArray]}
    let facts={arrayProgram [] with Routines=[writer;callerRoutine]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc []
    Assert.That(effect.WritesUnknown, Is.True)
    Assert.That(effect.DescriptorWritesUnknown || effect.LoopMetadataWritesUnknown, Is.False)

[<Test>]
let ``procedure recursive array summaries converge without losing content-only precision`` () =
    let first,second=SymbolId 100,SymbolId 101
    let a=routine first "A" [] [ProcCall(second,None,[],pos)]
    let b=routine second "B" [] [Assign(DynamicWriteArrayElem("A%",[number 0],Int,pos),number 7,pos);If(read flag,[ProcCall(first,None,[],pos)],None,pos)]
    let source=arrayProgram [Dim(arraySymbol,[number 10],pos);ProcCall(first,None,[],pos);assign y (arrayRead [number 10])]
    let facts={source with Routines=[a;b]} |> analyze
    Assert.That(facts.Effects.Summaries[first].Writes.Contains arraySymbol, Is.True)
    Assert.That(facts.Effects.Summaries[first].DescriptorWrites.IsEmpty && facts.Effects.Summaries[first].LoopMetadataWrites.IsEmpty, Is.True)
    arraySafe [number 10] facts

[<Test>]
let ``procedure dynamic reference arguments retain escape tracking`` () =
    let source=program [ProcCall(SymbolId 999,None,[RefArg(DynamicWriteVar("X%",Int,pos))],pos)]
    let facts=analyze source
    Assert.That(facts.Effects.Aliases.AddressTaken.Contains x && facts.Effects.Aliases.Escaped.Contains x, Is.True)

[<Test>]
let ``procedure unknown residents retain all memory and metadata barriers`` () =
    let proc=SymbolId 100
    let worker=routine proc "OPAQUE" [] [BuiltInCall(NamedBuiltIn "POKE",None,[number 1;number 2],pos)]
    let facts={program [] with Routines=[worker]} |> analyze
    let effect=HirEffects.call facts.Effects MainScope proc []
    Assert.That(effect.WritesUnknown && effect.DescriptorWritesUnknown && effect.LoopMetadataWritesUnknown, Is.True)

[<Test>]
let ``call-entry shapes reach a procedure through all known callers`` () =
    let outer,inner=SymbolId 110,SymbolId 111
    let indexes=[number 1;number 2]
    let first=routine outer "OUTER" [] [ProcCall(inner,None,[],pos)]
    let second=routine inner "INNER" [] [assign y (arrayRead indexes)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(outer,None,[],pos)]
    let facts={source with Routines=[first;second]} |> analyze
    Assert.That(facts.EntryShapesCertified, Is.True)
    Assert.That(facts.EntryShapePasses, Is.GreaterThan(1))
    arraySafe indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses["A%",indexes].ExactLengths, Is.EqualTo(Some(4,8)))

[<Test>]
let ``call-entry shapes intersect differing caller lengths without inventing a stride`` () =
    let proc=SymbolId 110
    let indexes=[number 1;number 2]
    let reader=routine proc "READ" [] [assign y (arrayRead indexes)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[],pos);
                            Dim(arraySymbol,[number 3;number 15],pos);ProcCall(proc,None,[],pos)]
    let facts={source with Routines=[reader]} |> analyze
    arraySafe indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses["A%",indexes].ExactLengths, Is.EqualTo(None))
    let scope=facts.Scopes[RoutineScope proc]
    Assert.That(scope.EntryBounds[scope.Graph.Entry].Value[arraySymbol], Is.EqualTo(box [3;7]))
    Assert.That(scope.EntryExactBounds[scope.Graph.Entry].Value.ContainsKey arraySymbol, Is.False)

[<Test>]
let ``call-entry shapes require every reaching call to have an allocated array`` () =
    let proc=SymbolId 110
    let indexes=[number 1;number 2]
    let reader=routine proc "READ" [] [assign y (arrayRead indexes)]
    let source=arrayProgram [ProcCall(proc,None,[],pos);Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[],pos)]
    let facts={source with Routines=[reader]} |> analyze
    arrayChecked indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``call-entry shapes reject incompatible ranks at different calls`` () =
    let proc=SymbolId 110
    let indexes=[number 0;number 0]
    let reader=routine proc "READ" [] [assign y (arrayRead indexes)]
    let source=arrayProgram [Dim(arraySymbol,[number 3],pos);ProcCall(proc,None,[],pos);
                            Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[],pos)]
    let facts={source with Routines=[reader]} |> analyze
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``call-entry shapes are killed by descriptor mutations in arguments`` () =
    let proc,resize,p=SymbolId 110,SymbolId 111,SymbolId 112
    let indexes=[number 1;number 2]
    let reader=routine proc "READ" [parameter "READ" p "P%"] [assign y (arrayRead indexes)]
    let mutator={routine resize "RESIZE%" [] [Dim(arraySymbol,[number 0;number 0],pos);Return(Some(number 1),pos)] with ReturnType=Some Int}
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[ValueArg(CallFunc(resize,[],Int,pos))],pos)]
    let facts={source with Routines=[reader;mutator]} |> analyze
    arrayChecked indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``call-entry shapes conservatively include sibling function effects`` () =
    let proc,resize=SymbolId 110,SymbolId 111
    let indexes=[number 1;number 2]
    let reader={routine proc "READ%" [] [Return(Some(arrayRead indexes),pos)] with ReturnType=Some Int}
    let mutator={routine resize "RESIZE%" [] [Dim(arraySymbol,[number 0;number 0],pos);Return(Some(number 1),pos)] with ReturnType=Some Int}
    let expression=binary Add (CallFunc(proc,[],Int,pos)) (CallFunc(resize,[],Int,pos))
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);assign y expression]
    let facts={source with Routines=[reader;mutator]} |> analyze
    arrayChecked indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``call-entry shapes also reach pure function bodies`` () =
    let proc=SymbolId 110
    let indexes=[number 1;number 2]
    let reader={routine proc "READ%" [] [Return(Some(arrayRead indexes),pos)] with ReturnType=Some Int}
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);assign y (CallFunc(proc,[],Int,pos))]
    let facts={source with Routines=[reader]} |> analyze
    arraySafe indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses["A%",indexes].ExactLengths, Is.EqualTo(Some(4,8)))

[<Test>]
let ``call-entry shapes remain invalid after opaque memory writes`` () =
    let proc=SymbolId 110
    let indexes=[number 1;number 2]
    let reader=routine proc "READ" [] [assign y (arrayRead indexes)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);BuiltInCall(NamedBuiltIn "POKE",None,[number 1;number 2],pos);ProcCall(proc,None,[],pos)]
    let facts={source with Routines=[reader]} |> analyze
    arrayChecked indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``call-entry shapes do not cross possible dynamic LOCAL rebinding`` () =
    let proc,shadow,localA=SymbolId 110,SymbolId 111,SymbolId 112
    let indexes=[number 1;number 2]
    let reader=routine proc "READ" [] [assign y (arrayRead indexes)]
    let localArray={storage localA "A%" (RoutineLocalStorage "SHADOW") with Type=Array Int;Dimensions=Some[0;0]}
    let owner={routine shadow "SHADOW" [] [ProcCall(proc,None,[],pos)] with Locals=[localArray]}
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[],pos)]
    let facts={source with Routines=[reader;owner]} |> analyze
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``call-entry shapes are discarded after an aliased formal can resize the global`` () =
    let proc,p=SymbolId 110,SymbolId 111
    let indexes=[number 1;number 2]
    let formal={parameter "RESIZE" p "P%" with Storage={storage p "P%" (RoutineParameterStorage "RESIZE") with Type=Array Int}}
    let worker=routine proc "RESIZE" [formal] [Dim(p,[number 0;number 0],pos);assign y (arrayRead indexes)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[RefArg(WriteVar(arraySymbol,Array Int,pos))],pos)]
    let facts={source with Routines=[worker]} |> analyze
    arrayChecked indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)

[<Test>]
let ``call-entry shapes follow recursion from the only real caller`` () =
    // Every entry is the main call or a recursive call made while the shape
    // still holds, so the shape is an inductive invariant of the entry.
    let proc=SymbolId 110
    let indexes=[number 1;number 2]
    let worker=routine proc "RECURSE" [] [assign y (arrayRead indexes);If(read flag,[ProcCall(proc,None,[],pos)],None,pos)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[],pos)]
    let facts={source with Routines=[worker]} |> analyze
    Assert.That(facts.EntryShapesCertified, Is.True)
    arraySafe indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses["A%",indexes].ExactLengths, Is.EqualTo(Some(4,8)))

[<Test>]
let ``call-entry shapes follow mutual recursion`` () =
    let first,second=SymbolId 110,SymbolId 111
    let indexes=[number 1;number 2]
    let a=routine first "PING" [] [ProcCall(second,None,[],pos)]
    let b=routine second "PONG" [] [assign y (arrayRead indexes);If(read flag,[ProcCall(first,None,[],pos)],None,pos)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(first,None,[],pos)]
    let facts={source with Routines=[a;b]} |> analyze
    Assert.That(facts.EntryShapesCertified, Is.True)
    arraySafe indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses["A%",indexes].ExactLengths, Is.EqualTo(Some(4,8)))

[<Test>]
let ``call-entry shapes join a smaller shape allocated before a recursive call`` () =
    let proc=SymbolId 110
    let indexes=[number 1;number 2]
    let worker=routine proc "RECURSE" [] [assign y (arrayRead indexes);If(read flag,[Dim(arraySymbol,[number 0;number 0],pos);ProcCall(proc,None,[],pos)],None,pos)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[],pos)]
    let facts={source with Routines=[worker]} |> analyze
    Assert.That(facts.EntryShapesCertified, Is.True)
    arrayChecked indexes facts
    let scope=facts.Scopes[RoutineScope proc]
    Assert.That(scope.EntryBounds[scope.Graph.Entry].Value[arraySymbol], Is.EqualTo(box [0;0]))

[<Test>]
let ``call-entry shapes keep a recursion cycle without real callers conservative`` () =
    // Neither routine is reachable from main, so neither entry can be certified
    // from a call site: both are analyzed from the unknown entry instead.
    let first,second=SymbolId 110,SymbolId 111
    let indexes=[number 1;number 2]
    let a=routine first "PING" [] [ProcCall(second,None,[],pos)]
    let b=routine second "PONG" [] [assign y (arrayRead indexes);ProcCall(first,None,[],pos)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos)]
    let facts={source with Routines=[a;b]} |> analyze
    Assert.That(facts.EntryShapesCertified, Is.True)
    arrayChecked indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses, Is.Empty)
    let scope=facts.Scopes[RoutineScope second]
    Assert.That(scope.EntryBounds[scope.Graph.Entry] |> Option.map Map.isEmpty, Is.EqualTo(Some true))

[<Test>]
let ``call-entry shapes may join a known recursive allocation with the real caller`` () =
    let proc=SymbolId 110
    let indexes=[number 1;number 2]
    let worker=routine proc "RECURSE" [] [assign y (arrayRead indexes);Dim(arraySymbol,[number 3;number 15],pos);If(read flag,[ProcCall(proc,None,[],pos)],None,pos)]
    let source=arrayProgram [Dim(arraySymbol,[number 3;number 7],pos);ProcCall(proc,None,[],pos)]
    let facts={source with Routines=[worker]} |> analyze
    arraySafe indexes facts
    Assert.That(facts.TwoDimensionalIntegerAccesses["A%",indexes].ExactLengths, Is.EqualTo(None))

[<TestCase(700,true)>]
[<TestCase(800,false)>]
let ``value parameters receive the joined ranges of their integer arguments`` (second:int,safe:bool) =
    let proc,p=SymbolId 120,SymbolId 121
    let sum=binary Add (read p) (number 32000)
    let worker=routine proc "WORK" [parameter "WORK" p "P%"] [assign y sum]
    let source=program [ProcCall(proc,None,[ValueArg(number 3)],pos);ProcCall(proc,None,[ValueArg(number second)],pos)]
    let facts={source with Routines=[worker]} |> analyze
    Assert.That(facts.EntryShapesCertified, Is.True)
    if safe then proven sum facts else unproven sum facts

[<Test>]
let ``reference and missing arguments pass no parameter range`` () =
    let proc,p,q=SymbolId 120,SymbolId 121,SymbolId 122
    let sum=binary Add (read p) (number 32000)
    let other=binary Add (read q) (number 32000)
    let worker=routine proc "WORK" [parameter "WORK" p "P%";parameter "WORK" q "Q%"] [assign y sum;assign z other]
    let source=program [assign x (number 3);ProcCall(proc,None,[RefArg(target x)],pos)]
    let facts={source with Routines=[worker]} |> analyze
    unproven sum facts
    unproven other facts

[<Test>]
let ``recursive value parameters widen to a guard and exclude its endpoint`` () =
    // walk(d%+1) only after IF d%=32767 THEN RETurn: d%+1 cannot overflow, but
    // proving it needs both call-entry widening and the excluded endpoint.
    let proc,d=SymbolId 120,SymbolId 121
    let next=binary Add (read d) (number 1)
    let body=[If(binary Equal (read d) (number 32767),[Return(None,pos)],None,pos);ProcCall(proc,None,[ValueArg next],pos)]
    let worker=routine proc "WALK" [parameter "WALK" d "D%"] body
    let facts={program [ProcCall(proc,None,[ValueArg(number 1)],pos)] with Routines=[worker]} |> analyze
    Assert.That(facts.EntryShapesCertified, Is.True)
    proven next facts
    let scope=facts.Scopes[RoutineScope proc]
    Assert.That(scope.EntryRanges[scope.Graph.Entry].Value[d], Is.EqualTo({Lower=1L;Upper=32767L}))

[<Test>]
let ``a parameter written while evaluating arguments passes no range`` () =
    let proc,p,bump=SymbolId 120,SymbolId 121,SymbolId 122
    let sum=binary Add (read p) (number 32000)
    let worker=routine proc "WORK" [parameter "WORK" p "P%"] [assign y sum]
    let mutator={routine bump "BUMP%" [] [assign x (number 9);Return(Some(number 1),pos)] with ReturnType=Some HirType.Int}
    let source=program [ProcCall(proc,None,[ValueArg(number 3);ValueArg(CallFunc(bump,[],HirType.Int,pos))],pos)]
    let facts={source with Routines=[worker;mutator]} |> analyze
    unproven sum facts

[<Test>]
let ``call-entry shapes fall back completely when the bounded solver cannot converge`` () =
    let symbols=[for n in 0..64 -> SymbolId(200+n)]
    let indexes=[number 0]
    let routines=symbols |> List.mapi(fun index symbol -> routine symbol ($"P{index}") [] (if index=64 then [assign y (arrayRead indexes)] else [ProcCall(symbols[index+1],None,[],pos)]))
    let expression=binary Add (read x) (number 1)
    let source=arrayProgram [Dim(arraySymbol,[number 10],pos);assign x (number 1);assign z expression;ProcCall(symbols.Head,None,[],pos)]
    let facts={source with Routines=routines} |> analyze
    Assert.That(facts.EntryShapePasses, Is.EqualTo(64))
    Assert.That(facts.EntryShapesCertified, Is.False)
    Assert.That(facts.OneDimensionalIntegerAccesses, Is.Empty)
    proven expression facts

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

[<Test>]
let ``untrusted call sites contribute unknown shapes without disabling independent callees`` () =
    let unsafeCallee=routine (SymbolId 100) "UNSAFE_INPUT" [] [assign x (arrayRead [number 2])]
    let safeCallee=routine (SymbolId 101) "SAFE_INPUT" [] [assign y (arrayRead [number 3])]
    let caller=routine (SymbolId 102) "UNTRUSTED" []
                    [Gosub(number 100,pos);ProcCall(unsafeCallee.Symbol,None,[],pos);Goto(number 200,pos)
                     LineNumber(100,pos);Return(None,pos);LineNumber(200,pos)]
    let p=arrayProgram [Dim(arraySymbol,[number 10],pos);ProcCall(unsafeCallee.Symbol,None,[],pos);ProcCall(safeCallee.Symbol,None,[],pos)]
    let facts=analyze {p with Routines=[unsafeCallee;safeCallee;caller]}
    Assert.That(facts.EntryShapePasses, Is.GreaterThan(0))
    Assert.That(facts.ProofEligibleScopes.Contains(RoutineScope caller.Symbol), Is.False)
    arrayChecked [number 2] facts
    Assert.That(facts.OneDimensionalIntegerAccesses.Contains("A%",[number 2]), Is.False)
    arraySafe [number 3] facts
    Assert.That(facts.OneDimensionalIntegerAccesses.Contains("A%",[number 3]), Is.True)

[<TestCase("LEN")>]
[<TestCase("CODE")>]
[<TestCase("CHR$")>]
let ``inline string intrinsics preserve loop binding certificates but retain effects`` (name:string) =
    let symbol=SymbolId 900
    let call=CallFunc(symbol,[ValueArg(read x)],HirType.Int,pos)
    let body=[For(LoopId 900,i,number 1,number 10,number 1,[assign y call],pos)]
    let p={program body with SymbolNames=Map.ofList [symbol,name]}
    let facts=analyze p
    Assert.That(facts.CertifiedNativeForLoops.Count, Is.EqualTo(1))
    let effect=expression facts.Effects MainScope call
    Assert.That(effect.Reads.Contains x, Is.True)
    Assert.That(effect.MayThrow, Is.True)
    Assert.That(effect.Allocates, Is.True)
    Assert.That(effect.WritesUnknown, Is.False)
    Assert.That(nonmutatingFunction p symbol 2, Is.False)
    let overridden={p with Routines=[{routine symbol name [] [] with ReturnType=Some HirType.Int}]}
    Assert.That(inlineFunction overridden symbol 1, Is.False)

[<Test>]
let ``only canonical MID and RIGHT slices bypass resident function barriers`` () =
    let mid,right,len=SymbolId 901,SymbolId 902,SymbolId 903
    let p={program [] with SymbolNames=Map.ofList [mid,"MID$";right,"RIGHT$";len,"LEN"]}
    let baseExpr=ReadVar(x,HirType.String,pos)
    let length=CallFunc(len,[ValueArg baseExpr],HirType.Int,pos)
    let count= binary Add (binary Subtract length (number 2)) (number 1)
    Assert.That(inlineCall p right [ValueArg baseExpr;ValueArg count], Is.True)
    Assert.That(inlineCall p right [ValueArg baseExpr;ValueArg(number 2)], Is.False)
    let count= binary Add (binary Subtract (number 5) (number 2)) (number 1)
    Assert.That(inlineCall p mid [ValueArg baseExpr;ValueArg(number 2);ValueArg count], Is.True)
    Assert.That(inlineCall p mid [ValueArg baseExpr;ValueArg(number 3);ValueArg count], Is.False)
    let overridden={p with Routines=[{routine right "RIGHT$" [] [] with ReturnType=Some HirType.String}]}
    Assert.That(inlineCall overridden right [ValueArg baseExpr;ValueArg count], Is.False)

[<Test>]
let ``memory call certificates remain separate from register pinning and reject metadata replacement`` () =
    let callee=routine (SymbolId 920) "WORK" [parameter "WORK" (SymbolId 921) "VALUE%"] [assign x (number 1)]
    let call args=ProcCall(callee.Symbol,None,args,pos)
    let loop args=For(LoopId 920,i,number 1,number 3,number 1,[call args],pos)
    let p={program [loop [ValueArg(read i)]] with Routines=[callee]}
    let facts=analyze p
    Assert.That(facts.CertifiedNativeForLoops, Is.Empty)
    Assert.That(facts.CertifiedMemoryForLoops, Is.EquivalentTo([None,LoopId 920]))
    Assert.That(analyze {p with Main=[loop [RefArg(target x)]]} |> _.CertifiedMemoryForLoops, Is.Empty)
    let replacement={callee with Body=[For(LoopId 921,i,number 1,number 2,number 1,[],pos)]}
    Assert.That(analyze {p with Routines=[replacement]} |> _.CertifiedMemoryForLoops, Is.Empty)
    Assert.That(analyze {p with Main=WhenError([],pos)::p.Main} |> _.CertifiedMemoryForLoops, Is.Empty)
