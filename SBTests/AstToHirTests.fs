module SBTests.AstToHirTests

open Antlr4.Runtime
open NUnit.Framework

open Types
open SyntaxAst
open CompilerPipeline
open AstToHir
open HIR
open ParseTreeVisitor

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

let private parseProgram (source: string) =
    let inputStream = AntlrInputStream(source)
    let lexer = CompilerPipeline.createLexer inputStream
    let tokenStream = CommonTokenStream(lexer)
    let parser = SBParser(tokenStream)

    parser.program()
    |> convertTreeToAst
    |> List.head

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
                  RepeatStmt(pos, "outer", StatementBlock [ ExitStmt(pos, "outer") ], None)
                  RepeatStmt(pos, "inner", StatementBlock [ NextStmt(pos, "inner") ], None) ]) ])

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
    | [ WhenError([], _) ] -> ()
    | other -> Assert.Fail($"Unexpected main HIR: %A{other}")

[<Test>]
let ``parse and lower compact select on clauses with multiline bodies`` () =
    let ast =
        parseProgram
            "10 SELect ON 9\n20 =0\n30 GO TO 100\n40 =9\n50 GO TO 200\n60 END SELect\n"

    match ast with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ SelectStmt(
                _,
                NumberLiteral(_, _, "9"),
                [ SelectClause(_, NumberLiteral(_, _, "9"), NumberLiteral(_, _, "0"), Some(LineBlock [ Line(_, Some 30, [ GotoStmt(_, NumberLiteral(_, _, "100")) ]) ]))
                  SelectClause(_, NumberLiteral(_, _, "9"), NumberLiteral(_, _, "9"), Some(LineBlock [ Line(_, Some 50, [ GotoStmt(_, NumberLiteral(_, _, "200")) ]) ])) ]) ]) ]) ->
        let hir = lowerProgram ast

        match hir.Main with
        | [ LineNumber(10, _)
            If(_, [ LineNumber(30, _); Goto(Literal(ConstInt 100, Int, _), _) ], Some [ If(_, [ LineNumber(50, _); Goto(Literal(ConstInt 200, Int, _), _) ], None, _) ], _) ] -> ()
        | other -> Assert.Fail($"Unexpected compact SELECT HIR: %A{other}")
    | other -> Assert.Fail($"Unexpected compact SELECT AST: %A{other}")

[<Test>]
let ``parse and lower compact select on clauses with comma separated matches`` () =
    let ast =
        parseProgram
            "10 e=1\n20 SELect ON e\n30 =1,5\n40 GO TO 100\n50 =2\n60 GO TO 200\n70 END SELect\n"

    match ast with
    | Program(
        _,
        [ Line(_, Some 10, [ Assignment(_, Identifier(_, _, "e"), NumberLiteral(_, _, "1")) ])
          Line(
            _,
            Some 20,
            [ SelectStmt(
                _,
                Identifier(_, _, "e"),
                [ SelectClause(_, Identifier(_, _, "e"), NumberLiteral(_, _, "1"), Some(LineBlock [ Line(_, Some 40, [ GotoStmt(_, NumberLiteral(_, _, "100")) ]) ]))
                  SelectClause(_, Identifier(_, _, "e"), NumberLiteral(_, _, "5"), Some(LineBlock [ Line(_, Some 40, [ GotoStmt(_, NumberLiteral(_, _, "100")) ]) ]))
                  SelectClause(_, Identifier(_, _, "e"), NumberLiteral(_, _, "2"), Some(LineBlock [ Line(_, Some 60, [ GotoStmt(_, NumberLiteral(_, _, "200")) ]) ])) ]) ]) ]) ->
        let hir = lowerProgram ast

        match hir.Main with
        | [ LineNumber(10, _)
            Assign(WriteVar(_, _, _), Literal(ConstInt 1, Int, _), _)
            LineNumber(20, _)
            If(_, [ LineNumber(40, _); Goto(Literal(ConstInt 100, Int, _), _) ], Some [ If(_, [ LineNumber(40, _); Goto(Literal(ConstInt 100, Int, _), _) ], Some [ If(_, [ LineNumber(60, _); Goto(Literal(ConstInt 200, Int, _), _) ], None, _) ], _) ], _) ] -> ()
        | other -> Assert.Fail($"Unexpected comma-separated compact SELECT HIR: %A{other}")
    | other -> Assert.Fail($"Unexpected comma-separated compact SELECT AST: %A{other}")

[<Test>]
let ``parse and lower for sequence with discrete prefix range and suffix`` () =
    let ast =
        parseProgram
            "10 FOR x = 1,2,5 TO 7,9\n20 PRINT x\n30 END FOR\n"

    match ast with
    | Program(
        _,
        [ Line(
            _,
            Some 10,
            [ ForStmt(
                _,
                "x",
                [ NumberLiteral(_, _, "1"); NumberLiteral(_, _, "2") ],
                NumberLiteral(_, _, "5"),
                NumberLiteral(_, _, "7"),
                [ NumberLiteral(_, _, "9") ],
                None,
                LineBlock [ Line(_, Some 20, [ ProcedureCall(_, "PRINT", [ Identifier(_, _, "x") ]) ]) ],
                None) ]) ]) ->
        let hir = lowerProgram ast

        match hir.Main with
        | [ LineNumber(10, _)
            ForSequence(_, _, [ Literal(ConstInt 1, Int, _); Literal(ConstInt 2, Int, _) ], Literal(ConstInt 5, Int, _), Literal(ConstInt 7, Int, _), [ Literal(ConstInt 9, Int, _) ], Literal(ConstInt 1, Int, _), [ LineNumber(20, _); BuiltInCall(Print, None, [ ReadVar(_, _, _) ], _) ], _) ] -> ()
        | other -> Assert.Fail($"Unexpected FOR sequence HIR: %A{other}")
    | other -> Assert.Fail($"Unexpected FOR sequence AST: %A{other}")

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
let ``lowerToHir collects nested data entries and restore points inside structured blocks`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 10,
                [ ForStmt(
                    pos,
                    "f",
                    [],
                    num "1",
                    num "1",
                    [],
                    None,
                    LineBlock
                        [ Line(pos, Some 20, [ DataStmt(pos, [ num "2"; num "3" ]) ])
                          Line(pos, Some 30, [ IfStmt(pos, num "1", LineBlock [ Line(pos, Some 40, [ DataStmt(pos, [ num "4" ]) ]) ], None) ]) ],
                    None) ]) ])

    let hir = lowerProgram ast

    Assert.That(hir.DataEntries, Has.Length.EqualTo(3))
    Assert.That(hir.RestorePoints, Has.Length.EqualTo(2))

    match hir.DataEntries with
    | [ { Slot = DataSlotId 0; Value = Literal(ConstInt 2, Int, _); LineNumber = Some 20 }
        { Slot = DataSlotId 1; Value = Literal(ConstInt 3, Int, _); LineNumber = Some 20 }
        { Slot = DataSlotId 2; Value = Literal(ConstInt 4, Int, _); LineNumber = Some 40 } ] -> ()
    | other -> Assert.Fail($"Unexpected nested data entries: %A{other}")

    match hir.RestorePoints with
    | [ { LineNumber = 20; Slot = DataSlotId 0 }
        { LineNumber = 40; Slot = DataSlotId 2 } ] -> ()
    | other -> Assert.Fail($"Unexpected nested restore points: %A{other}")

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
        Input(Some(ExplicitChannel(Literal(ConstInt 1, Int, _))), [ Literal(ConstString "Name", String, _) ], [ WriteVar(_, _, _) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected channel INPUT HIR: %A{other}")

[<Test>]
let ``lowerToHir lowers single string index access as char read`` () =
    let hir =
        parseProgram
            "10 text$ = \"abc\"\n20 PRINT text$(LEN(text$))\n"
        |> lowerProgram

    match hir.Main with
    | [ LineNumber(10, _)
        Assign(WriteVar(_, _, _), Literal(ConstString "abc", String, _), _)
        LineNumber(20, _)
        BuiltInCall(Print, None, [ ReadStringChar(_, CallFunc(_, [ ValueArg(ReadVar(_, String, _)) ], Int, _), String, _) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected string index HIR: %A{other}")

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
                    [ Line(pos, Some 20, [ ReferenceStmt(pos, [ mkIdentifier pos "right" ]) ]) ],
                    None, None) ]) ])

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
                      Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ mkIdentifier pos "score" ]) ]) ],
                    None, None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "score", None ]) ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "show", []) ]) ],
                    None, None) ]) ])

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
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ mkIdentifier pos "a" ]) ]) ],
                    None, None) ])
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
                      Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ],
                    None, None) ])
              Line(
                pos,
                Some 20,
                [ ProcedureDef(
                    pos,
                    "main",
                    [],
                    [ Line(pos, Some 200, [ LocalStmt(pos, [ "scores", Some [ num "5" ] ]) ])
                      Line(pos, Some 210, [ ProcedureCall(pos, "show", []) ]) ],
                    None, None) ]) ])

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
                      Line(pos, Some 110, [ ProcedureCall(pos, "PRINT", [ id "score" ]) ]) ],
                    None, None) ]) ])

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
                    [ Line(pos, Some 100, [ ProcedureCall(pos, "PRINT", [ mkPostfixName pos "DATE" None ]) ]) ],
                    None, None) ]) ])

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
                    [ Line(pos, Some 100, [ ReferenceStmt(pos, [ id "a" ]) ]) ],
                    None, None) ])
              Line(pos, Some 20, [ DimStmt(pos, [ "scores", [ num "5" ] ]) ])
              Line(pos, Some 30, [ ProcedureCall(pos, "bump", [ mkPostfixName pos "scores" (Some [ num "1" ]) ]) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        LineNumber(20, _)
        LineNumber(30, _)
        ProcCall(_, None, [ RefArg(WriteArrayElem(_, [ Literal(ConstInt 1, HirType.Int, _) ], _, _)) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected lowered main for array-element ref arg: %A{other}")

[<Test>]
let ``lowerToHir expands line to syntax into four built in arguments`` () =
    let ast =
        Program(
            pos,
            [ Line(
                pos,
                Some 5,
                [ Assignment(pos, id "f", num "1")
                  Assignment(pos, id "a", num "2")
                  Assignment(pos, id "b", num "3") ])
              Line(
                pos,
                Some 10,
                [ ProcedureCall(
                    pos,
                    "LINE",
                    [ id "f"
                      mkSliceRange pos (id "a") (id "f")
                      binary "-" (id "a") (id "b") ]) ]) ])

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(5, _)
        Assign(_, _, _)
        Assign(_, _, _)
        Assign(_, _, _)
        LineNumber(10, _)
        BuiltInCall(
            NamedBuiltIn "LINE",
            None,
            [ ReadVar(_, _, _)
              ReadVar(_, _, _)
              ReadVar(_, _, _)
              Binary(Subtract, ReadVar(_, _, _), ReadVar(_, _, _), _, _) ],
            _) ] -> ()
    | other -> Assert.Fail($"Unexpected lowered LINE HIR: %A{other}")

[<Test>]
let ``lowerToHir preserves spaced rnd range call syntax`` () =
    let ast =
        parseProgram "10 b=5\n20 x=RND (0 TO b)\n"

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        Assign(WriteVar(_, _, _), Literal(ConstInt 5, HirType.Int, _), _)
        LineNumber(20, _)
        Assign(
            WriteVar(_, _, _),
            CallFunc(_, [ ValueArg(Binary(SliceRange, Literal(ConstInt 0, HirType.Int, _), ReadVar(_, HirType.Int, _), _, _)) ], _, _),
            _) ] -> ()
    | other -> Assert.Fail($"Unexpected lowered spaced RND HIR: %A{other}")

[<Test>]
let ``lowerToHir expands print backslash separators and unquotes single quoted strings`` () =
    let ast =
        parseProgram "10 PRINT 'alpha'\\'beta'\n"

    let hir = lowerProgram ast

    match hir.Main with
    | [ LineNumber(10, _)
        BuiltInCall(Print, None, [ Literal(ConstString "alpha", HirType.String, _) ], _)
        BuiltInCall(Print, None, [ Literal(ConstString "beta", HirType.String, _) ], _) ] -> ()
    | other -> Assert.Fail($"Unexpected lowered PRINT separator HIR: %A{other}")


