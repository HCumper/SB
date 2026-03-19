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
