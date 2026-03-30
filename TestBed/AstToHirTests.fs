module TestBed.AstToHirTests

open NUnit.Framework

open Types
open SyntaxAst
open CompilerPipeline
open AstToHir
open HIR

let private pos =
    { BasicLineNo = None
      EditorLineNo = 1
      Column = 0 }

let private num value = mkNumberLiteral pos value
let private id name = mkIdentifier pos name
let private binary op lhs rhs = mkBinaryExpr pos op lhs rhs

let private lowerProgram ast =
    let analyzed = runSemanticAnalysis ast
    match lowerToHir analyzed with
    | Result.Ok hir -> hir
    | Result.Error errs -> Assert.Fail($"Expected HIR lowering to succeed, got %A{errs}"); Unchecked.defaultof<_>

[<Test>]
let ``lowerToHir lowers select when exit and next into dedicated hir nodes`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ SelectStmt(
                    pos,
                    num "1",
                    [ SelectClause(
                        pos,
                        num "1",
                        num "2",
                        Some(StatementBlock [ GotoStmt(pos, num "100") ])) ])
                  WhenStmt(pos, Some(num "1"), [ Line(pos, Some 20, [ GosubStmt(pos, num "200") ]) ])
                  RepeatStmt(pos, "outer", StatementBlock [ ExitStmt(pos, "outer") ])
                  RepeatStmt(pos, "inner", StatementBlock [ NextStmt(pos, "inner") ]) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        If(_, [ Goto(Literal(ConstInt 100, Int, _), _) ], None, _)
        If(Literal(ConstInt 1, Int, _), [ LineNumber(20, _); Gosub(Literal(ConstInt 200, Int, _), _) ], None, _)
        Repeat(LoopId 0, NamedLoop "outer", [ Exit(LoopId 0, _) ], _)
        Repeat(LoopId 1, NamedLoop "inner", [ Next(LoopId 1, _) ], _) ] ->
        Assert.That(hir.Globals, Is.Empty)
        Assert.That(hir.DataEntries, Is.Empty)
        Assert.That(hir.RestorePoints, Is.Empty)
    | other -> Assert.Fail($"Unexpected main HIR: %A{other}")

[<Test>]
let ``lowerToHir preserves empty when condition and empty select clause body`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                None,
                [ SelectStmt(pos, num "3", [ SelectClause(pos, num "4", num "5", None) ])
                  WhenStmt(pos, None, []) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [] -> ()
    | other -> Assert.Fail($"Unexpected main HIR: %A{other}")

[<Test>]
let ``lowerToHir builds explicit storage and data layout`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, mkIdentifier pos "x", num "1") ])
              Line(pos, Some 20, [ DataStmt(pos, [ num "2"; num "3" ]) ]) ])

    let hir = lowerProgram ast

    Assert.That(hir.Globals, Has.Length.EqualTo(1))
    Assert.That(hir.DataEntries, Has.Length.EqualTo(2))
    Assert.That(hir.RestorePoints, Has.Length.EqualTo(1))

    match hir.Globals[0] with
    | { Name = "X"; Class = GlobalStorage; Slot = StorageSlotId 0 } -> ()
    | other -> Assert.Fail($"Unexpected global storage: %A{other}")

    match hir.DataEntries with
    | [ { Slot = DataSlotId 0; Value = Literal(ConstInt 2, Int, _); LineNumber = Some 20 }
        { Slot = DataSlotId 1; Value = Literal(ConstInt 3, Int, _); LineNumber = Some 20 } ] -> ()
    | other -> Assert.Fail($"Unexpected data entries: %A{other}")

    match hir.RestorePoints with
    | [ { LineNumber = 20; Slot = DataSlotId 0 } ] -> ()
    | other -> Assert.Fail($"Unexpected restore points: %A{other}")

[<Test>]
let ``lowerToHir splits input prompts from writable targets`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ ProcedureCall(pos, "INPUT", [ mkStringLiteral pos "\"How many?\""; mkIdentifier pos "n" ]) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        Input(None, [ Literal(ConstString "How many?", String, _) ], [ WriteVar(_, _, _) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected INPUT HIR: %A{other}")

[<Test>]
let ``lowerToHir supports channel input prompts`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ChannelProcedureCall(pos, "INPUT", num "1", [ mkStringLiteral pos "\"Name\""; mkIdentifier pos "n$" ]) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        Input(Some(Literal(ConstInt 1, Int, _)), [ Literal(ConstString "Name", String, _) ], [ WriteVar(_, _, _) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected channel INPUT HIR: %A{other}")

[<Test>]
let ``lowerToHir preserves parameter binding metadata from reference statements`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "swap",
                    [ "left"; "right" ],
                    [ Line(pos, Some 20, [ ReferenceStmt(pos, [ mkIdentifier pos "right" ]) ]) ]) ]) ])

    let hir = lowerProgram ast

    match hir.Routines with
    | [ routine ] ->
        match routine.Parameters with
        | [ left; right ] ->
            Assert.That(left.Binding, Is.EqualTo(FlexibleBinding))
            Assert.That(right.Binding, Is.EqualTo(ReferenceBinding))
        | other -> Assert.Fail($"Unexpected lowered parameters: %A{other}")
    | other -> Assert.Fail($"Unexpected lowered routines: %A{other}")

[<Test>]
let ``lowerToHir uses dynamic scoped reads and writes for caller visible locals`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ Assignment(pos, mkIdentifier pos "score", num "9") ])
                      Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ mkIdentifier pos "score" ]) ]) ]) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "show", []) ]) ]) ]) ])

    let hir = lowerProgram ast

    match hir.Routines |> List.tryFind (fun routine -> routine.Name = "show") with
    | Some routine ->
        match routine.Body with
        | [ LineNumber(100, _)
            Assign(DynamicWriteVar("SCORE", _, _), Literal(ConstInt 9, HirType.Int, _), _)
            LineNumber(110, _)
            BuiltInCall(Print, None, [ DynamicReadVar("SCORE", HirType.Int, _) ], _) ] -> ()
        | other -> Assert.Fail($"Unexpected lowered show body: %A{other}")
    | None -> Assert.Fail("Expected lowered routine 'show'")

[<Test>]
let ``lowerToHir distinguishes reference and value call arguments`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a"; "b" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ mkIdentifier pos "a" ]) ]) ]) ])
              Line(pos, Some 20, [ Assignment(pos, mkIdentifier pos "x", num "1") ])
              Line(pos, Some 30, [ ProcedureCall(pos, "bump", [ mkIdentifier pos "x"; binary "+" (mkIdentifier pos "x") (num "0") ]) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        LineNumber(20, _)
        Assign(WriteVar(_, _, _), Literal(ConstInt 1, HirType.Int, _), _)
        LineNumber(30, _)
        ProcCall(_, None, [ RefArg(WriteVar(_, HirType.Int, _)); ValueArg(Binary(Add, ReadVar(_, HirType.Int, _), Literal(ConstInt 0, HirType.Int, _), HirType.Int, _)) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected lowered main for mixed call args: %A{other}")

[<Test>]
let ``lowerToHir lowers dynamic scoped array access`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ Assignment(pos, mkPostfixName pos "scores" (Some [ num "1" ]), num "5") ])
                      Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ]) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "scores", Some [ num "5" ] ]) ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "show", []) ]) ]) ]) ])

    let hir = lowerProgram ast

    match hir.Routines |> List.tryFind (fun routine -> routine.Name = "show") with
    | Some routine ->
        match routine.Body with
        | [ LineNumber(100, _)
            Assign(DynamicWriteArrayElem("SCORES", [ Literal(ConstInt 1, HirType.Int, _) ], _, _), Literal(ConstInt 5, HirType.Int, _), _)
            LineNumber(110, _)
            BuiltInCall(Print, None, [ DynamicReadArrayElem("SCORES", [ Literal(ConstInt 1, HirType.Int, _) ], _, _) ], _) ] -> ()
        | other -> Assert.Fail($"Unexpected lowered dynamic array body: %A{other}")
    | None -> Assert.Fail("Expected lowered routine 'show'")

[<Test>]
let ``lowerToHir keeps top level globals static`` () =
    let ast =
        Program(
            pos,
            [ Line(pos, Some 10, [ Assignment(pos, id "score", num "1") ])
              Line(pos, Some 20, [ Assignment(pos, id "score", binary "+" (id "score") (num "2")) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        Assign(WriteVar(_, _, _), Literal(ConstInt 1, HirType.Int, _), _)
        LineNumber(20, _)
        Assign(WriteVar(_, HirType.Int, _), Binary(Add, ReadVar(_, HirType.Int, _), Literal(ConstInt 2, HirType.Int, _), HirType.Int, _), _)
        LineNumber(30, _)
        BuiltInCall(Print, None, [ ReadVar(_, HirType.Int, _) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected lowered main for static globals: %A{other}")

[<Test>]
let ``lowerToHir keeps parameter accesses static inside routines`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "show",
                    [ "score" ],
                    [ Line(pos, Some 100, [ Assignment(pos, id "score", binary "+" (id "score") (num "1")) ])
                      Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ]) ]) ])

    let hir = lowerProgram ast

    match hir.Routines with
    | [ routine ] ->
        match routine.Body with
        | [ LineNumber(100, _)
            Assign(WriteVar(_, _, _), Binary(Add, ReadVar(_, _, _), Literal(ConstInt 1, HirType.Int, _), HirType.Int, _), _)
            LineNumber(110, _)
            BuiltInCall(Print, None, [ ReadVar(_, HirType.Int, _) ], _) ] -> ()
        | other -> Assert.Fail($"Unexpected lowered routine body for static parameter access: %A{other}")
    | other -> Assert.Fail($"Unexpected lowered routines: %A{other}")

[<Test>]
let ``lowerToHir does not lower built in names as dynamic storage`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "show",
                    [],
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "DATE" None ]) ]) ]) ]) ])

    let hir = lowerProgram ast

    match hir.Routines with
    | [ routine ] ->
        match routine.Body with
        | [ LineNumber(100, _)
            BuiltInCall(Print, None, [ CallFunc(_, [], HirType.Int, _) ], _) ] -> ()
        | other -> Assert.Fail($"Unexpected lowered routine body for built-in call: %A{other}")
    | other -> Assert.Fail($"Unexpected lowered routines: %A{other}")

[<Test>]
let ``lowerToHir uses reference args for array element actuals`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ProcedureDef(
                    pos,
                    "bump",
                    [ "a" ],
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ]) ]) ])
              Line(pos, Some 20, [ DimStmt(pos, [ "scores", [ num "5" ] ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "bump", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        LineNumber(20, _)
        LineNumber(30, _)
        ProcCall(_, None, [ RefArg(WriteArrayElem(_, [ Literal(ConstInt 1, HirType.Int, _) ], _, _)) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected lowered main for array-element ref arg: %A{other}")
