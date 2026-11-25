module SBTests.HirTurboOptimizerTests

open NUnit.Framework
open Types
open HIR
open HirTurboOptimizer

let private pos = { BasicLineNo = Some 10; EditorLineNo = 1; Column = 3 }
let private number n = Literal(ConstInt n, HirType.Int, pos)
let private binary op a b = Binary(op, a, b, HirType.Int, pos)
let private program body = { SymbolNames = Map.empty; Globals = []; Routines = []; DataEntries = []; RestorePoints = []; Main = body }
let private print e = BuiltInCall(Print, None, [e], pos)
let private run target p = optimize defaultOptions target p |> Result.defaultWith (String.concat "; " >> failwith)
let private transform expr =
    (run HirTurboIrBackend.defaultOptions (program [print expr])).Main
    |> List.exactlyOne |> function BuiltInCall(_, _, [e], _) -> e | _ -> failwith "Expected PRINT."
let private storage symbol name kind =
    { Symbol = symbol; Name = name; Slot = StorageSlotId 0; Type = HirType.Int
      Dimensions = None; Class = kind; Position = pos }

[<TestCase(32766, 1, 32767)>]
[<TestCase(-32767, -1, -32768)>]
[<TestCase(123, -123, 0)>]
let ``integer literal addition uses checked QL word limits`` (a: int, b: int, expected: int) =
    Assert.That(transform (binary Add (number a) (number b)), Is.EqualTo(number expected))

[<TestCase(-7, 3, -3, 2)>]
[<TestCase(7, -3, -3, -2)>]
[<TestCase(-7, -3, 2, -1)>]
let ``DIV floors and MOD follows the divisor sign`` (a: int, b: int, quotient: int, remainder: int) =
    Assert.That(transform (binary IntegerDivide (number a) (number b)), Is.EqualTo(number quotient))
    Assert.That(transform (binary Modulo (number a) (number b)), Is.EqualTo(number remainder))

[<Test>]
let ``overflow and failing operations remain executable`` () =
    for expr in [ binary Add (number 32767) (number 1)
                  binary Multiply (number 16384) (number 2)
                  binary IntegerDivide (number 1) (number 0)
                  binary IntegerDivide (number -32768) (number -1)
                  binary Modulo (number -32768) (number -1) ] do
        Assert.That(transform expr, Is.EqualTo(expr))

[<Test>]
let ``a child fold cannot bypass integer negation overflow`` () =
    let expr = Unary(Negate, binary Subtract (number -32767) (number 1), HirType.Int, pos)
    Assert.That(transform expr, Is.EqualTo(expr))

[<Test>]
let ``QDOS comparisons and logical operators retain float results`` () =
    let yes = Literal(ConstFloat 1.0, HirType.Int, pos)
    Assert.That(transform (binary LessThan (number 2) (number 3)), Is.EqualTo(yes))
    Assert.That(transform (binary BitwiseAnd (number 3) (number 6)), Is.EqualTo(yes))
    Assert.That(transform (Unary(BitwiseNot, number 3, HirType.Int, pos)), Is.EqualTo(number 0))

[<Test>]
let ``uncertain floating arithmetic and coercions are retained`` () =
    let real n = Literal(ConstFloat n, HirType.Float, pos)
    for expr in [ Binary(Add, real 0.1, real 0.2, HirType.Float, pos)
                  Binary(Divide, number 1, number 3, HirType.Float, pos)
                  Binary(Add, real 1.0, number 0, HirType.Float, pos)
                  Binary(AlmostEqual, real 1.0, real 1.0, HirType.Int, pos) ] do
        Assert.That(transform expr, Is.EqualTo(expr))

[<Test>]
let ``target naming and IMPLICIT determine safe identities rather than HIR inferred types`` () =
    let symbol = SymbolId 1
    let read = ReadVar(symbol, HirType.Int, pos)
    let p = { program [print (binary Add read (number 0))] with Globals = [storage symbol "a" GlobalStorage] }
    Assert.That((run HirTurboIrBackend.defaultOptions p).Main, Is.EqualTo<HirBlock>(p.Main))
    let implicitInteger = { HirTurboIrBackend.defaultOptions with ImplicitIntegers = Set.singleton "A" }
    Assert.That((run implicitInteger p).Main, Is.EqualTo<HirBlock>([print read]))

[<Test>]
let ``only genuine global MANIFEST storage is replaced`` () =
    let globalId, localId = SymbolId 1, SymbolId 2
    let target = { HirTurboIrBackend.defaultOptions with ManifestConstants = Map.ofList ["N", ConstInt 3] }
    let local = storage localId "n" (RoutineParameterStorage "f")
    let routine =
        { Symbol = SymbolId 3; Name = "f"; Parameters = [{Storage=local; Binding=FlexibleBinding}]
          Locals = []; ReturnType = None; EndLineNumber = Some 120; Position = pos
          Body = [print (ReadVar(localId, HirType.Int, pos))] }
    let p = { program [print (ReadVar(globalId, HirType.Int, pos))] with Globals = [storage globalId "n" GlobalStorage]; Routines = [routine] }
    let optimized = run target p
    Assert.That(optimized.Main, Is.EqualTo<HirBlock>([print (number 3)]))
    Assert.That(optimized.Routines[0], Is.EqualTo(routine))

[<Test>]
let ``simplified call values keep their value and reference classifications`` () =
    let symbol = SymbolId 1
    let read = ReadVar(symbol, HirType.Int, pos)
    let args = [ValueArg(binary Add read (number 0)); RefArg(WriteVar(symbol, HirType.Int, pos))]
    let p = { program [ProcCall(SymbolId 2, None, args, pos)] with Globals = [storage symbol "a%" GlobalStorage] }
    let optimized = run HirTurboIrBackend.defaultOptions p
    Assert.That(optimized.Main, Is.EqualTo<HirBlock>([ProcCall(SymbolId 2, None, [ValueArg read; args[1]], pos)]))
    Assert.That(validate p optimized, Is.Empty)

// a$(1 TO 31) is MID$(a$, 1, (31 - 1) + 1). Folding that length to 31 would leave a
// resident MID$ call, so a slice keeps its shape while its bounds are simplified.
[<Test>]
let ``constant slice bounds keep the shape the backend emits as a slice`` () =
    let mid, text = SymbolId 1, SymbolId 2
    let call args = CallFunc(mid, ValueArg(ReadVar(text, HirType.String, pos)) :: List.map ValueArg args, HirType.String, pos)
    let slice start finish = call [ start; binary Add (binary Subtract finish start) (number 1) ]
    let p =
        { program [ print (slice (number 1) (binary Add (number 30) (number 1))); print (call [ number 1; binary Add (number 2) (number 1) ]) ]
            with SymbolNames = Map.ofList [ mid, "MID$" ] }
    let optimized = run HirTurboIrBackend.defaultOptions p
    Assert.That(optimized.Main, Is.EqualTo<HirBlock>([ print (slice (number 1) (number 31)); print (call [ number 1; number 3 ]) ]))
    Assert.That(validate p optimized, Is.Empty)

[<Test>]
let ``multiplication by zero retains side effects and errors`` () =
    let expr = binary Multiply (CallFunc(SymbolId 1, [], HirType.Int, pos)) (number 0)
    Assert.That(transform expr, Is.EqualTo(expr))

[<Test>]
let ``only emission safe branches without numbered entries are removed`` () =
    let yes, no = [print (number 7)], [print (number 8)]
    let p = program [If(number 1, yes, Some no, pos)]
    Assert.That((run HirTurboIrBackend.defaultOptions p).Main, Is.EqualTo<HirBlock>(yes))
    let anchored = program [If(number 0, LineNumber(40, pos)::yes, Some no, pos)]
    Assert.That((run HirTurboIrBackend.defaultOptions anchored).Main, Is.EqualTo<HirBlock>(anchored.Main))
    let directive = program [If(number 0, [Remark("TURBO_windo 1", pos)], None, pos)]
    Assert.That((run HirTurboIrBackend.defaultOptions directive).Main, Is.EqualTo<HirBlock>(directive.Main))

[<Test>]
let ``string literal concatenation respects the QL character set`` () =
    let text s = Literal(ConstString s, HirType.String, pos)
    let expr = Binary(Concat, text "ab", text "cd", HirType.String, pos)
    Assert.That(transform expr, Is.EqualTo(text "abcd"))
    let unsupported = Binary(Concat, text "a", text "😀", HirType.String, pos)
    Assert.That(transform unsupported, Is.EqualTo(unsupported))

[<Test>]
let ``DATA RESTORE storage and source positions are preserved`` () =
    let p =
        { program [LineNumber(10, pos); print (binary Add (number 1) (number 2)); Restore(None, pos)] with
            DataEntries = [{Slot=DataSlotId 0; Value=binary Add (number 1) (number 2); LineNumber=Some 100; Position=pos}]
            RestorePoints = [{LineNumber=100; Slot=DataSlotId 0}] }
    let optimized = run HirTurboIrBackend.defaultOptions p
    Assert.That(optimized.DataEntries, Is.EqualTo<HirDataEntry list>(p.DataEntries))
    Assert.That(optimized.RestorePoints, Is.EqualTo<HirRestorePoint list>(p.RestorePoints))
    Assert.That(optimized.Main, Is.EqualTo<HirBlock>([LineNumber(10, pos); print (number 3); Restore(None, pos)]))
    Assert.That(validate p {optimized with Main=[]}, Is.Not.Empty)

[<Test>]
let ``disabled mode returns original HIR without mutation`` () =
    let p = program [print (binary Add (number 1) (number 2))]
    let original = HIRPrettyPrinter.prettyPrintHir p
    let result = optimize {defaultOptions with Enabled=false} HirTurboIrBackend.defaultOptions p |> Result.defaultWith (String.concat "; " >> failwith)
    Assert.That(obj.ReferenceEquals(p, result), Is.True)
    run HirTurboIrBackend.defaultOptions p |> ignore
    Assert.That(HIRPrettyPrinter.prettyPrintHir p, Is.EqualTo(original))
