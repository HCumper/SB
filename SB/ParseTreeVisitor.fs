module ParseTreeVisitor

open System
open Antlr4.Runtime
open Antlr4.Runtime.Tree

open SyntaxAst
open Types

let private posOfToken (t: IToken) : SourcePosition =
    { BasicLineNo = None
      EditorLineNo = t.Line
      Column = t.Column }

let rec private firstTokenOf (tree: IParseTree) : IToken option =
    match tree with
    | :? ITerminalNode as t -> Some t.Symbol
    | _ when tree.ChildCount > 0 -> firstTokenOf (tree.GetChild 0)
    | _ -> None

let private posOfTree (tree: IParseTree) : SourcePosition =
    match firstTokenOf tree with
    | Some t -> posOfToken t
    | None -> { BasicLineNo = None; EditorLineNo = 0; Column = 0 }

let private parseLineNumber (text: string) =
    match Int32.TryParse text with
    | true, n -> Some n
    | _ -> None

type VisitedNode =
    | RootNode of Ast
    | LineNode of Line
    | StmtNode of Stmt
    | ExprNode of Expr
    | ClauseNode of SelectClause

let private single node = [ node ]

let private singleRoot tree nodes =
    match nodes with
    | [ RootNode root ] -> root
    | _ -> Program(posOfTree tree, [])

let private singleLine tree nodes =
    match nodes with
    | [ LineNode line ] -> line
    | _ -> Line(posOfTree tree, None, [])

let private singleStmt tree nodes =
    match nodes with
    | [ StmtNode stmt ] -> stmt
    | _ -> Remark(posOfTree tree, "")

let private singleExpr tree nodes =
    match nodes with
    | [ ExprNode expr ] -> expr
    | _ -> Identifier(posOfTree tree, "")

let private singleClause tree nodes =
    match nodes with
    | [ ClauseNode clause ] -> clause
    | _ -> SelectClause(posOfTree tree, Identifier(posOfTree tree, ""), Identifier(posOfTree tree, ""), None)

let private emptySeq<'T> : seq<'T> = Seq.empty

type ASTBuildingVisitor() =
    inherit SBBaseVisitor<VisitedNode list>()

    member private this.SafeAcceptExpr(tree: IParseTree) =
        if isNull tree then
            Identifier({ BasicLineNo = None; EditorLineNo = 0; Column = 0 }, "")
        else
            singleExpr tree (tree.Accept(this))

    member private this.SafeChildren<'T when 'T : null>(nodes: seq<'T>) =
        if isNull (box nodes) then emptySeq else nodes

    member private this.VisitLineList(lines: seq<SBParser.LineContext>) =
        this.SafeChildren(lines) |> Seq.map (fun l -> singleLine l (l.Accept(this))) |> Seq.toList

    member private this.CollectStmtList(ctx: SBParser.StmtlistContext) =
        this.SafeChildren(ctx.stmt()) |> Seq.map (fun s -> singleStmt s (s.Accept(this))) |> Seq.toList

    member private this.CollectPlainStmtList(ctx: SBParser.PlainStmtlistContext) =
        this.SafeChildren(ctx.stmt()) |> Seq.map (fun s -> singleStmt s (s.Accept(this))) |> Seq.toList

    member private this.CollectExprList(ctx: SBParser.ExprListContext) =
        this.SafeChildren(ctx.expr()) |> Seq.map (fun e -> this.SafeAcceptExpr(e)) |> Seq.toList

    member private this.CollectParenthesizedArgs(ctx: SBParser.ParenthesizedlistContext) =
        this.SafeChildren(ctx.expr()) |> Seq.map (fun e -> this.SafeAcceptExpr(e)) |> Seq.toList

    member private this.CollectUnparenthesizedArgs(ctx: SBParser.UnparenthesizedlistContext) =
        this.SafeChildren(ctx.expr()) |> Seq.map (fun e -> this.SafeAcceptExpr(e)) |> Seq.toList

    member private this.CollectStmtArgs(ctx: SBParser.StmtTailContext) =
        ctx.stmtSegment()
        |> Seq.collect (fun segment -> segment.Accept(this))
        |> Seq.map (fun node -> singleExpr ctx [ node ])
        |> Seq.toList

    member private this.PostfixArgItemToExpr(ctx: SBParser.PostfixArgItemContext) =
        let p = posOfTree ctx
        let exprs = this.SafeChildren(ctx.expr()) |> Seq.toList

        if not (isNull (ctx.chanArg())) then
            this.SafeAcceptExpr(ctx.chanArg())
        else
            match exprs with
            | [ a ] when not (isNull (ctx.To())) ->
                let left = this.SafeAcceptExpr(a)
                SliceRange(p, left, Identifier(p, ""))
            | [ a ] -> this.SafeAcceptExpr(a)
            | [ a; b ] ->
                let left = this.SafeAcceptExpr(a)
                let right = this.SafeAcceptExpr(b)
                SliceRange(p, left, right)
            | _ ->
                Identifier(p, "")

    member private this.FoldLeft(pos: SourcePosition, first: Expr, ops: string list, rest: Expr list) =
        let pairCount = min ops.Length rest.Length
        let zipped = List.zip (ops |> List.truncate pairCount) (rest |> List.truncate pairCount)
        (first, zipped)
        ||> List.fold (fun acc (op, rhs) -> BinaryExpr(pos, op, acc, rhs))

    member private this.OperatorTexts(ctx: IRuleNode, allowed: Set<string>) =
        [ for i in 0 .. ctx.ChildCount - 1 do
              let t = ctx.GetChild(i).GetText()
              if allowed.Contains t then
                  yield t ]

    member private this.BlockFromStmtOrLine(stmtList: SBParser.StmtlistContext, lineContexts: seq<SBParser.LineContext>) =
        if isNull stmtList then
            LineBlock(this.VisitLineList(lineContexts))
        else
            StatementBlock(this.CollectStmtList(stmtList))

    member private this.ForBodyLineToLine(ctx: SBParser.ForBodyLineContext) =
        let p = posOfTree ctx
        let lineNo =
            match ctx.lineNumber() with
            | null -> None
            | ln -> parseLineNumber (ln.GetText())

        let statements =
            match ctx.plainStmtlist() with
            | null -> []
            | stmtList -> this.CollectPlainStmtList(stmtList)

        Line(p, lineNo, statements)

    override this.VisitProgram(ctx: SBParser.ProgramContext) =
        let p = posOfTree ctx
        let children = this.VisitLineList(ctx.line())
        single (RootNode(Program(p, children)))

    override this.VisitLine(ctx: SBParser.LineContext) =
        let p = posOfTree ctx
        let lineNo =
            match ctx.lineNumber() with
            | null -> None
            | ln -> parseLineNumber (ln.GetText())

        let children =
            if isNull (ctx.stmtlist()) then [] else this.CollectStmtList(ctx.stmtlist())

        single (LineNode(Line(p, lineNo, children)))

    override this.VisitStmtlist(ctx: SBParser.StmtlistContext) =
        this.SafeChildren(ctx.stmt()) |> Seq.collect (fun s -> s.Accept(this)) |> Seq.toList

    override this.VisitSimpleStatement(ctx: SBParser.SimpleStatementContext) =
        this.VisitChildren(ctx)

    override this.VisitProcedureDefinitionStmt(ctx: SBParser.ProcedureDefinitionStmtContext) =
        ctx.procedureDef().Accept(this)

    override this.VisitFunctionDefinitionStmt(ctx: SBParser.FunctionDefinitionStmtContext) =
        ctx.functionDef().Accept(this)

    override this.VisitForStatement(ctx: SBParser.ForStatementContext) =
        ctx.forStmt().Accept(this)

    override this.VisitRepeatStatement(ctx: SBParser.RepeatStatementContext) =
        ctx.repeatStmt().Accept(this)

    override this.VisitIfStatement(ctx: SBParser.IfStatementContext) =
        ctx.ifStmt().Accept(this)

    override this.VisitSelectStatement(ctx: SBParser.SelectStatementContext) =
        ctx.selectStmt().Accept(this)

    override this.VisitWhenStatement(ctx: SBParser.WhenStatementContext) =
        ctx.whenStmt().Accept(this)

    override this.VisitProcedureDef(ctx: SBParser.ProcedureDefContext) =
        let p = posOfTree ctx
        let ids = ctx.ID() |> Seq.toList
        let name =
            match ids with
            | id :: _ -> id.GetText()
            | [] -> ""

        let parms =
            match ctx.formalParams() with
            | null -> []
            | fp -> fp.ID() |> Seq.map (fun id -> id.GetText()) |> Seq.toList

        let body =
            ctx.definitionBodyLine()
            |> Seq.choose (fun bodyLine ->
                match bodyLine.line() with
                | null -> None
                | line -> Some (singleLine line (line.Accept(this))))
            |> Seq.toList
        single (StmtNode(ProcedureDef(p, name, parms, body)))

    override this.VisitFunctionDef(ctx: SBParser.FunctionDefContext) =
        let p = posOfTree ctx
        let ids = ctx.ID() |> Seq.toList
        let name =
            match ids with
            | id :: _ -> id.GetText()
            | [] -> ""

        let parms =
            match ctx.formalParams() with
            | null -> []
            | fp -> fp.ID() |> Seq.map (fun id -> id.GetText()) |> Seq.toList

        let body =
            ctx.definitionBodyLine()
            |> Seq.choose (fun bodyLine ->
                match bodyLine.line() with
                | null -> None
                | line -> Some (singleLine line (line.Accept(this))))
            |> Seq.toList
        single (StmtNode(FunctionDef(p, name, parms, body)))

    override this.VisitDim(ctx: SBParser.DimContext) =
        let p = posOfTree ctx
        let items =
            ctx.dimList().dimItem()
            |> Seq.map (fun item ->
                let name = item.ID().GetText()
                let dims =
                    match item.exprList() with
                    | null -> []
                    | e -> this.CollectExprList(e)
                name, dims)
            |> Seq.toList
        single (StmtNode(DimStmt(p, items)))

    override this.VisitLocalDecl(ctx: SBParser.LocalDeclContext) =
        let p = posOfTree ctx
        let items =
            ctx.localList().localItem()
            |> Seq.map (fun item ->
                let name = item.ID().GetText()
                let dims =
                    match item.exprList() with
                    | null -> None
                    | e -> Some (this.CollectExprList(e))
                name, dims)
            |> Seq.toList
        single (StmtNode(LocalStmt(p, items)))

    override this.VisitImplicitDecl(ctx: SBParser.ImplicitDeclContext) =
        let p = posOfTree ctx
        let kw = ctx.Implic().GetText()
        let suffix =
            if String.IsNullOrEmpty kw then "" else string kw.[kw.Length - 1]
        let names =
            ctx.unparenthesizedlist().expr()
            |> Seq.map (fun e -> singleExpr e (e.Accept(this)))
            |> Seq.choose (function Identifier(_, name) -> Some name | _ -> None)
            |> Seq.toList
        single (StmtNode(ImplicitStmt(p, suffix, names)))

    override this.VisitReferenceDecl(ctx: SBParser.ReferenceDeclContext) =
        let p = posOfTree ctx
        let children = this.CollectUnparenthesizedArgs(ctx.unparenthesizedlist())
        single (StmtNode(ReferenceStmt(p, children)))

    override this.VisitRemark(ctx: SBParser.RemarkContext) =
        let p = posOfTree ctx
        let txt =
            match ctx.Comment() with
            | null -> ctx.GetText()
            | c -> c.GetText()
        single (StmtNode(Remark(p, txt)))

    override this.VisitNextStmt(ctx: SBParser.NextStmtContext) =
        single (StmtNode(NextStmt(posOfTree ctx, ctx.ID().GetText())))

    override this.VisitExitStmt(ctx: SBParser.ExitStmtContext) =
        single (StmtNode(ExitStmt(posOfTree ctx, ctx.ID().GetText())))

    override this.VisitGotoStatement(ctx: SBParser.GotoStatementContext) =
        let p = posOfTree ctx
        let gotoCtx = ctx.gotoStmt()
        let args = gotoCtx.Accept(this) |> List.map (fun node -> singleExpr gotoCtx [ node ])
        single (StmtNode(ProcedureCall(p, "GOTO", args)))

    override this.VisitGosubStatement(ctx: SBParser.GosubStatementContext) =
        let p = posOfTree ctx
        let gosubCtx = ctx.gosubStmt()
        let args = gosubCtx.Accept(this) |> List.map (fun node -> singleExpr gosubCtx [ node ])
        single (StmtNode(ProcedureCall(p, "GOSUB", args)))

    override this.VisitOnGotoStatement(ctx: SBParser.OnGotoStatementContext) =
        let p = posOfTree ctx
        let onGotoCtx = ctx.onGotoStmt()
        let args = onGotoCtx.Accept(this) |> List.map (fun node -> singleExpr onGotoCtx [ node ])
        single (StmtNode(ProcedureCall(p, "ON-GOTO", args)))

    override this.VisitOnGosubStatement(ctx: SBParser.OnGosubStatementContext) =
        let p = posOfTree ctx
        let onGosubCtx = ctx.onGosubStmt()
        let args = onGosubCtx.Accept(this) |> List.map (fun node -> singleExpr onGosubCtx [ node ])
        single (StmtNode(ProcedureCall(p, "ON-GOSUB", args)))

    override this.VisitReturnStmt(ctx: SBParser.ReturnStmtContext) =
        let p = posOfTree ctx
        let value =
            match ctx.expr() with
            | null -> None
            | e -> Some (singleExpr e (e.Accept(this)))
        single (StmtNode(ReturnStmt(p, value)))

    override this.VisitDataStmt(ctx: SBParser.DataStmtContext) =
        single (StmtNode(DataStmt(posOfTree ctx, this.CollectExprList(ctx.exprList()))))

    override this.VisitReadStmt(ctx: SBParser.ReadStmtContext) =
        let lvalueCtx = ctx.lvalueList()
        let values = lvalueCtx.Accept(this) |> List.map (fun node -> singleExpr lvalueCtx [ node ])
        single (StmtNode(ReadStmt(posOfTree ctx, values)))

    override this.VisitRestoreStmt(ctx: SBParser.RestoreStmtContext) =
        let p = posOfTree ctx
        let value =
            match ctx.expr() with
            | null -> None
            | e -> Some (singleExpr e (e.Accept(this)))
        single (StmtNode(RestoreStmt(p, value)))

    override this.VisitAssignmentStatement(ctx: SBParser.AssignmentStatementContext) =
        ctx.assignmentStmt().Accept(this)

    override this.VisitChannelProcCall(ctx: SBParser.ChannelProcCallContext) =
        ctx.channelProcCallStmt().Accept(this)

    override this.VisitProcedureCallStatement(ctx: SBParser.ProcedureCallStatementContext) =
        ctx.procedureCallStmt().Accept(this)

    override this.VisitGotoStmt(ctx: SBParser.GotoStmtContext) =
        ctx.expr().Accept(this)

    override this.VisitGosubStmt(ctx: SBParser.GosubStmtContext) =
        ctx.expr().Accept(this)

    override this.VisitOnGotoStmt(ctx: SBParser.OnGotoStmtContext) =
        let selector = singleExpr (ctx.expr()) (ctx.expr().Accept(this))
        let targets = this.CollectExprList(ctx.exprList())
        ExprNode selector :: (targets |> List.map ExprNode)

    override this.VisitOnGosubStmt(ctx: SBParser.OnGosubStmtContext) =
        let selector = singleExpr (ctx.expr()) (ctx.expr().Accept(this))
        let targets = this.CollectExprList(ctx.exprList())
        ExprNode selector :: (targets |> List.map ExprNode)

    override this.VisitAssignmentStmt(ctx: SBParser.AssignmentStmtContext) =
        let p = posOfTree ctx
        let lhs = singleExpr (ctx.lvalue()) (ctx.lvalue().Accept(this))
        let rhs = singleExpr (ctx.expr()) (ctx.expr().Accept(this))
        single (StmtNode(Assignment(p, lhs, rhs)))

    override this.VisitChannelProcCallStmt(ctx: SBParser.ChannelProcCallStmtContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()
        let channel = singleExpr (ctx.chanArg()) (ctx.chanArg().Accept(this))
        let args =
            match ctx.stmtTail() with
            | null -> []
            | tail -> this.CollectStmtArgs(tail)
        single (StmtNode(ChannelProcedureCall(p, name, channel, args)))

    override this.VisitProcedureCallStmt(ctx: SBParser.ProcedureCallStmtContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()
        let args =
            match ctx.stmtArglist() with
            | null -> []
            | sa -> sa.Accept(this) |> List.map (fun node -> singleExpr sa [ node ])
        single (StmtNode(ProcedureCall(p, name, args)))

    override this.VisitLvalue(ctx: SBParser.LvalueContext) =
        ctx.postfixName().Accept(this)

    override this.VisitLvalueList(ctx: SBParser.LvalueListContext) =
        ctx.lvalue() |> Seq.map (fun lv -> singleExpr lv (lv.Accept(this)) |> ExprNode) |> Seq.toList

    override this.VisitStmtArglist(ctx: SBParser.StmtArglistContext) =
        if not (isNull (ctx.parenthesizedlist())) then
            ctx.parenthesizedlist().Accept(this)
        else
            ctx.stmtTail().Accept(this)

    override this.VisitStmtTail(ctx: SBParser.StmtTailContext) =
        ctx.stmtSegment() |> Seq.collect (fun segment -> segment.Accept(this)) |> Seq.toList

    override this.VisitStmtSegment(ctx: SBParser.StmtSegmentContext) =
        match ctx.stmtArg() with
        | null -> []
        | arg -> arg.Accept(this)

    override this.VisitStmtArg(ctx: SBParser.StmtArgContext) =
        if not (isNull (ctx.chanArg())) then ctx.chanArg().Accept(this)
        else ctx.rangedExpr().Accept(this)

    override this.VisitRangedExpr(ctx: SBParser.RangedExprContext) =
        let p = posOfTree ctx
        let exprs = ctx.expr() |> Seq.toList

        match exprs with
        | [ value ] ->
            value.Accept(this)
        | [ startValue; endValue ] ->
            let left = singleExpr startValue (startValue.Accept(this))
            let right = singleExpr endValue (endValue.Accept(this))
            single (ExprNode(SliceRange(p, left, right)))
        | _ ->
            single (ExprNode(Identifier(p, "")))

    override this.VisitChanArg(ctx: SBParser.ChanArgContext) =
        ctx.expr().Accept(this)

    override this.VisitArg(ctx: SBParser.ArgContext) =
        if not (isNull (ctx.chanArg())) then ctx.chanArg().Accept(this)
        else ctx.expr().Accept(this)

    override this.VisitPostfixName(ctx: SBParser.PostfixNameContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()
        let children =
            match ctx.postfixArg() with
            | null -> []
            | pa -> pa.Accept(this) |> List.map (fun node -> singleExpr pa [ node ])
        let expr =
            match children with
            | [] -> Identifier(p, name)
            | _ -> PostfixName(p, name, Some children)
        single (ExprNode expr)

    override this.VisitPostfixArg(ctx: SBParser.PostfixArgContext) =
        match ctx.postfixArgList() with
        | null -> []
        | pal -> pal.Accept(this)

    override this.VisitPostfixArgList(ctx: SBParser.PostfixArgListContext) =
        ctx.postfixArgItem() |> Seq.map (fun item -> ExprNode(this.PostfixArgItemToExpr(item))) |> Seq.toList

    override this.VisitPostfixArgItem(ctx: SBParser.PostfixArgItemContext) =
        single (ExprNode(this.PostfixArgItemToExpr(ctx)))

    override this.VisitEndDef(_ctx: SBParser.EndDefContext) = []
    override this.VisitEndFor(_ctx: SBParser.EndForContext) = []
    override this.VisitEndRepeat(_ctx: SBParser.EndRepeatContext) = []
    override this.VisitEndIf(_ctx: SBParser.EndIfContext) = []
    override this.VisitEndSelect(_ctx: SBParser.EndSelectContext) = []
    override this.VisitEndWhen(_ctx: SBParser.EndWhenContext) = []

    override this.VisitForStmt(ctx: SBParser.ForStmtContext) =
        let p = posOfTree ctx
        let name =
            match ctx.ID() with
            | null -> ""
            | id -> id.GetText()

        let exprs = ctx.expr() |> Seq.toList
        let startExpr =
            match exprs with
            | first :: _ -> singleExpr first (first.Accept(this))
            | [] -> Identifier(p, "")
        let endExpr =
            match exprs with
            | _ :: second :: _ -> singleExpr second (second.Accept(this))
            | _ -> Identifier(p, "")
        let stepExpr =
            match exprs with
            | _ :: _ :: third :: _ -> Some (singleExpr third (third.Accept(this)))
            | _ -> None

        let loopStmt, trailingNodes =
            match ctx.forBody() with
            | null ->
                let body =
                    match ctx.stmtlist() with
                    | null -> StatementBlock([])
                    | stmtList -> StatementBlock(this.CollectStmtList(stmtList))

                StmtNode(ForStmt(p, name, startExpr, endExpr, stepExpr, body)), []
            | bodyCtx ->
                let leadingLines =
                    bodyCtx.forBodyLine()
                    |> Seq.map this.ForBodyLineToLine
                    |> Seq.toList

                let terminator = bodyCtx.forTerminator()
                let closingLine =
                    match terminator.plainStmtlist() with
                    | null -> []
                    | stmtList ->
                        let lineNo =
                            match terminator.lineNumber() with
                            | null -> None
                            | ln -> parseLineNumber (ln.GetText())

                        [ Line(posOfTree terminator, lineNo, this.CollectPlainStmtList(stmtList)) ]

                let trailingNodes =
                    match terminator.stmtlist() with
                    | null -> []
                    | stmtList -> stmtList.Accept(this)

                StmtNode(ForStmt(p, name, startExpr, endExpr, stepExpr, LineBlock(leadingLines @ closingLine))), trailingNodes

        loopStmt :: trailingNodes

    override this.VisitRepeatStmt(ctx: SBParser.RepeatStmtContext) =
        let p = posOfTree ctx
        let ids = ctx.ID() |> Seq.toList
        let name =
            match ids with
            | id :: _ -> id.GetText()
            | [] -> ""
        let body =
            match ctx.stmtlist() with
            | null ->
                let lines =
                    ctx.repeatBodyLine()
                    |> Seq.choose (fun bodyLine ->
                        match bodyLine.line() with
                        | null -> None
                        | line -> Some (singleLine line (line.Accept(this))))
                    |> Seq.toList
                LineBlock(lines)
            | stmtList ->
                StatementBlock(this.CollectStmtList(stmtList))
        single (StmtNode(RepeatStmt(p, name, body)))

    override this.VisitIfStmt(ctx: SBParser.IfStmtContext) =
        let p = posOfTree ctx
        let cond = singleExpr (ctx.expr()) (ctx.expr().Accept(this))

        let thenPart, elsePart =
            if ctx.stmtlist().Length > 0 then
                let t = StatementBlock(this.CollectStmtList(ctx.stmtlist(0)))
                let e =
                    if ctx.stmtlist().Length > 1 then Some (StatementBlock(this.CollectStmtList(ctx.stmtlist(1))))
                    else None
                t, e
            else
                let t =
                    match ctx.ifBlock() with
                    | null -> LineBlock([])
                    | ib ->
                        let lines =
                            ib.ifBodyLine()
                            |> Seq.choose (fun bodyLine ->
                                match bodyLine.line() with
                                | null -> None
                                | line -> Some (singleLine line (line.Accept(this))))
                            |> Seq.toList
                        LineBlock(lines)
                let e =
                    match ctx.elseBlock() with
                    | null -> None
                    | eb ->
                        let lines =
                            eb.elseBodyLine()
                            |> Seq.choose (fun bodyLine ->
                                match bodyLine.line() with
                                | null -> None
                                | line -> Some (singleLine line (line.Accept(this))))
                            |> Seq.toList
                        Some (LineBlock(lines))
                t, e

        single (StmtNode(IfStmt(p, cond, thenPart, elsePart)))

    override this.VisitIfBlock(ctx: SBParser.IfBlockContext) =
        ctx.ifBodyLine()
        |> Seq.choose (fun bodyLine ->
            match bodyLine.line() with
            | null -> None
            | line -> Some (LineNode(singleLine line (line.Accept(this)))))
        |> Seq.toList

    override this.VisitElseBlock(ctx: SBParser.ElseBlockContext) =
        ctx.elseBodyLine()
        |> Seq.choose (fun bodyLine ->
            match bodyLine.line() with
            | null -> None
            | line -> Some (LineNode(singleLine line (line.Accept(this)))))
        |> Seq.toList

    override this.VisitSelectStmt(ctx: SBParser.SelectStmtContext) =
        let p = posOfTree ctx
        let selector = this.SafeAcceptExpr(ctx.expr())
        let clauses =
            this.SafeChildren(ctx.selectBodyItem())
            |> Seq.map (fun si -> singleClause si (si.Accept(this)))
            |> Seq.toList
        single (StmtNode(SelectStmt(p, selector, clauses)))

    override this.VisitSelectBodyItem(ctx: SBParser.SelectBodyItemContext) =
        ctx.selectItem().Accept(this)

    override this.VisitSelectItem(ctx: SBParser.SelectItemContext) =
        if not (isNull (ctx.onClause())) then ctx.onClause().Accept(this)
        else []

    override this.VisitOnClause(ctx: SBParser.OnClauseContext) =
        let p = posOfTree ctx
        let selectorExpr = this.SafeAcceptExpr(ctx.expr())
        let rangeAst = this.SafeAcceptExpr(ctx.rangeexpr())
        let body =
            match ctx.stmtlist() with
            | null -> None
            | s -> Some (StatementBlock(this.CollectStmtList(s)))
        single (ClauseNode(SelectClause(p, selectorExpr, rangeAst, body)))

    override this.VisitWhenErrorStmt(ctx: SBParser.WhenErrorStmtContext) =
        let body =
            ctx.whenBodyLine()
            |> Seq.choose (fun bodyLine ->
                match bodyLine.line() with
                | null -> None
                | line -> Some (singleLine line (line.Accept(this))))
            |> Seq.toList
        single (StmtNode(WhenStmt(posOfTree ctx, None, body)))

    override this.VisitWhenCondStmt(ctx: SBParser.WhenCondStmtContext) =
        let p = posOfTree ctx
        let condition = this.SafeAcceptExpr(ctx.expr())
        let body =
            ctx.whenBodyLine()
            |> Seq.choose (fun bodyLine ->
                match bodyLine.line() with
                | null -> None
                | line -> Some (singleLine line (line.Accept(this))))
            |> Seq.toList
        single (StmtNode(WhenStmt(p, Some condition, body)))

    override this.VisitExpr(ctx: SBParser.ExprContext) =
        ctx.orExpr().Accept(this)

    override this.VisitOrExpr(ctx: SBParser.OrExprContext) =
        let p = posOfTree ctx
        let terms =
            this.SafeChildren(ctx.andExpr()) |> Seq.map (fun x -> this.SafeAcceptExpr(x)) |> Seq.toList
        let ops = this.OperatorTexts(ctx, set [ "OR"; "XOR" ])
        match terms with
        | [] -> []
        | first :: rest -> single (ExprNode(this.FoldLeft(p, first, ops, rest)))

    override this.VisitAndExpr(ctx: SBParser.AndExprContext) =
        let p = posOfTree ctx
        let terms =
            this.SafeChildren(ctx.notExpr()) |> Seq.map (fun x -> this.SafeAcceptExpr(x)) |> Seq.toList
        let ops =
            this.OperatorTexts(ctx, set [ "AND"; "&&" ])
            |> List.map (function | "&&" -> "AND" | other -> other)
        match terms with
        | [] -> []
        | first :: rest -> single (ExprNode(this.FoldLeft(p, first, ops, rest)))

    override this.VisitNotExpr(ctx: SBParser.NotExprContext) =
        let p = posOfTree ctx
        if not (isNull (ctx.compareExpr())) then
            ctx.compareExpr().Accept(this)
        else
            let rhs = this.SafeAcceptExpr(ctx.notExpr())
            single (ExprNode(UnaryExpr(p, "NOT", rhs)))

    override this.VisitCompareExpr(ctx: SBParser.CompareExprContext) =
        let p = posOfTree ctx
        let terms =
            this.SafeChildren(ctx.instrExpr()) |> Seq.map (fun x -> this.SafeAcceptExpr(x)) |> Seq.toList
        let ops = this.OperatorTexts(ctx, set [ "="; "<>"; "<"; "<="; ">"; ">="; "==" ])
        match terms with
        | [] -> []
        | first :: rest -> single (ExprNode(this.FoldLeft(p, first, ops, rest)))

    override this.VisitInstrExpr(ctx: SBParser.InstrExprContext) =
        let p = posOfTree ctx
        let parts =
            this.SafeChildren(ctx.concatExpr()) |> Seq.map (fun x -> this.SafeAcceptExpr(x)) |> Seq.toList
        match parts with
        | [ singlePart ] -> [ ExprNode singlePart ]
        | [ lhs; rhs ] -> single (ExprNode(BinaryExpr(p, "INSTR", lhs, rhs)))
        | _ -> []

    override this.VisitConcatExpr(ctx: SBParser.ConcatExprContext) =
        let p = posOfTree ctx
        let terms =
            this.SafeChildren(ctx.addExpr()) |> Seq.map (fun x -> this.SafeAcceptExpr(x)) |> Seq.toList
        let ops = this.OperatorTexts(ctx, set [ "&" ])
        match terms with
        | [] -> []
        | first :: rest -> single (ExprNode(this.FoldLeft(p, first, ops, rest)))

    override this.VisitAddExpr(ctx: SBParser.AddExprContext) =
        let p = posOfTree ctx
        let terms =
            this.SafeChildren(ctx.mulExpr()) |> Seq.map (fun x -> this.SafeAcceptExpr(x)) |> Seq.toList
        let ops = this.OperatorTexts(ctx, set [ "+"; "-" ])
        match terms with
        | [] -> []
        | first :: rest -> single (ExprNode(this.FoldLeft(p, first, ops, rest)))

    override this.VisitMulExpr(ctx: SBParser.MulExprContext) =
        let p = posOfTree ctx
        let terms =
            this.SafeChildren(ctx.powExpr()) |> Seq.map (fun x -> this.SafeAcceptExpr(x)) |> Seq.toList
        let ops = this.OperatorTexts(ctx, set [ "*"; "/"; "MOD"; "DIV" ])
        match terms with
        | [] -> []
        | first :: rest -> single (ExprNode(this.FoldLeft(p, first, ops, rest)))

    override this.VisitPowExpr(ctx: SBParser.PowExprContext) =
        let p = posOfTree ctx
        let terms =
            this.SafeChildren(ctx.unaryExpr()) |> Seq.map (fun x -> this.SafeAcceptExpr(x)) |> Seq.toList
        let ops = this.OperatorTexts(ctx, set [ "^" ])
        match terms with
        | [] -> []
        | first :: rest -> single (ExprNode(this.FoldLeft(p, first, ops, rest)))

    override this.VisitUnaryExpr(ctx: SBParser.UnaryExprContext) =
        let p = posOfTree ctx
        if not (isNull (ctx.primary())) then
            ctx.primary().Accept(this)
        else
            let op =
                if ctx.ChildCount > 0 then ctx.GetChild(0).GetText() else ""
            let rhs = this.SafeAcceptExpr(ctx.unaryExpr())
            single (ExprNode(UnaryExpr(p, op, rhs)))

    override this.VisitPrimary(ctx: SBParser.PrimaryContext) =
        let p = posOfTree ctx
        if not (isNull (ctx.Integer())) then
            single (ExprNode(NumberLiteral(p, ctx.Integer().GetText())))
        elif not (isNull (ctx.Real())) then
            single (ExprNode(NumberLiteral(p, ctx.Real().GetText())))
        elif not (isNull (ctx.String())) then
            single (ExprNode(StringLiteral(p, ctx.String().GetText())))
        elif not (isNull (ctx.postfixName())) then
            ctx.postfixName().Accept(this)
        elif not (isNull (ctx.expr())) then
            ctx.expr().Accept(this)
        else
            []

    override this.VisitParenthesizedlist(ctx: SBParser.ParenthesizedlistContext) =
        this.CollectParenthesizedArgs(ctx) |> List.map ExprNode

    override this.VisitUnparenthesizedlist(ctx: SBParser.UnparenthesizedlistContext) =
        this.CollectUnparenthesizedArgs(ctx) |> List.map ExprNode

    override this.VisitSeparator(_ctx: SBParser.SeparatorContext) = []

    override this.VisitRangeexpr(ctx: SBParser.RangeexprContext) =
        let p = posOfTree ctx
        let exprs = this.SafeChildren(ctx.expr()) |> Seq.toList
        match exprs with
        | [ a ] -> a.Accept(this)
        | [ a; b ] ->
            let left = this.SafeAcceptExpr(a)
            let right = this.SafeAcceptExpr(b)
            single (ExprNode(SliceRange(p, left, right)))
        | _ when not (isNull (ctx.Remainder())) ->
            single (ExprNode(Identifier(p, "REMAINDER")))
        | _ -> []

    override this.VisitLineNumber(_ctx: SBParser.LineNumberContext) = []

    override this.VisitTerminal(node: ITerminalNode) =
        let sym = node.Symbol
        let p = posOfToken sym
        match SBLexer.DefaultVocabulary.GetSymbolicName(sym.Type) with
        | "INTEGER" -> single (ExprNode(NumberLiteral(p, sym.Text)))
        | "REAL" -> single (ExprNode(NumberLiteral(p, sym.Text)))
        | "STRING" -> single (ExprNode(StringLiteral(p, sym.Text)))
        | _ -> []

    override this.VisitErrorNode(node: IErrorNode) =
        let p =
            match node.Symbol with
            | null -> { BasicLineNo = None; EditorLineNo = 0; Column = 0 }
            | s -> posOfToken s
        single (ExprNode(Identifier(p, node.GetText())))

    override this.VisitChildren(node: IRuleNode) =
        [ for i in 0 .. node.ChildCount - 1 do
              yield! node.GetChild(i).Accept(this) ]

let convertTreeToAst (parseTree: IParseTree) : Ast list =
    let visitor = ASTBuildingVisitor()
    [ singleRoot parseTree (parseTree.Accept(visitor)) ]

let convertTreeToExpr (parseTree: IParseTree) : Expr list =
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)
    |> List.choose (function
        | ExprNode expr -> Some expr
        | _ -> None)
