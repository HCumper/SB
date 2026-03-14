module ParseTreeVisitor

open System
open Antlr4.Runtime
open Antlr4.Runtime.Tree
open Utility

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------

let private posOfToken (t: IToken) : int * int =
    (t.Line, t.Column)

let rec private firstTokenOf (tree: IParseTree) : IToken option =
    match tree with
    | :? ITerminalNode as t -> Some t.Symbol
    | _ when tree.ChildCount > 0 -> firstTokenOf (tree.GetChild 0)
    | _ -> None

let private posOfTree (tree: IParseTree) : int * int =
    match firstTokenOf tree with
    | Some t -> posOfToken t
    | None -> (0, 0)

let private mk kind value pos children =
    createAstNode kind value pos children

let private single kind value pos children =
    [ mk kind value pos children ]

let private collectOperatorTexts (ctx: IRuleNode) (allowed: Set<string>) =
    [ for i in 0 .. ctx.ChildCount - 1 do
        let t = ctx.GetChild(i).GetText()
        if allowed.Contains t then
            yield t ]

let private foldLeftBinary (pos: int * int) (first: ASTNode) (ops: string list) (rest: ASTNode list) =
    (first, List.zip ops rest)
    ||> List.fold (fun acc (op, rhs) -> mk NodeKind.BinaryExpr op pos [ acc; rhs ])

// -----------------------------------------------------------------------------
// Visitor
// -----------------------------------------------------------------------------

type ASTBuildingVisitor() =
    inherit SBBaseVisitor<ASTNode list>()

    member private this.VisitLineList(lines: seq<SBParser.LineContext>) =
        lines |> Seq.collect (fun l -> l.Accept(this)) |> Seq.toList

    member private this.CollectStmtList(ctx: SBParser.StmtlistContext) =
        ctx.stmt() |> Seq.collect (fun s -> s.Accept(this)) |> Seq.toList

    member private this.CollectExprList(ctx: SBParser.ExprListContext) =
        ctx.expr() |> Seq.collect (fun e -> e.Accept(this)) |> Seq.toList

    member private this.CollectParenthesizedArgs(ctx: SBParser.ParenthesizedlistContext) =
        ctx.expr() |> Seq.collect (fun e -> e.Accept(this)) |> Seq.toList

    member private this.CollectUnparenthesizedArgs(ctx: SBParser.UnparenthesizedlistContext) =
        ctx.expr() |> Seq.collect (fun e -> e.Accept(this)) |> Seq.toList

    member private this.SingleAstOrUnknown(tree: IParseTree, asts: ASTNode list) =
        match asts with
        | [ x ] -> x
        | xs -> mk NodeKind.Unknown "" (posOfTree tree) xs

    member private this.PostfixArgItemToAst(ctx: SBParser.PostfixArgItemContext) =
        let p = posOfTree ctx
        let exprs = ctx.expr() |> Seq.toList

        match exprs with
        | [ a ] ->
            this.SingleAstOrUnknown(a, a.Accept(this))
        | [ a; b ] ->
            let left = this.SingleAstOrUnknown(a, a.Accept(this))
            let right = this.SingleAstOrUnknown(b, b.Accept(this))
            mk NodeKind.BinaryExpr "TO" p [ left; right ]
        | _ ->
            mk NodeKind.Unknown "" p []

    member private this.FoldLeft(pos: int * int, first: ASTNode, ops: string list, rest: ASTNode list) =
        foldLeftBinary pos first ops rest

    // -------------------------------------------------------------------------
    // Top level
    // -------------------------------------------------------------------------

    override this.VisitProgram(ctx: SBParser.ProgramContext) =
        let p = posOfTree ctx
        let children = this.VisitLineList(ctx.line())
        single NodeKind.Program "" p children

    override this.VisitLine(ctx: SBParser.LineContext) =
        if isNull (ctx.stmtlist()) then
            []
        else
            this.CollectStmtList(ctx.stmtlist())

    override this.VisitStmtlist(ctx: SBParser.StmtlistContext) =
        this.CollectStmtList(ctx)

    // -------------------------------------------------------------------------
    // stmt wrappers
    // -------------------------------------------------------------------------

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

    // -------------------------------------------------------------------------
    // Definitions
    // -------------------------------------------------------------------------

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
            | fp ->
                fp.ID()
                |> Seq.map (fun id -> mk NodeKind.Identifier (id.GetText()) (posOfTree id) [])
                |> Seq.toList

        let body = this.VisitLineList(ctx.line())
        single NodeKind.ProcedureDefinition name p (parms @ body)

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
            | fp ->
                fp.ID()
                |> Seq.map (fun id -> mk NodeKind.Identifier (id.GetText()) (posOfTree id) [])
                |> Seq.toList

        let body = this.VisitLineList(ctx.line())
        single NodeKind.FunctionDefinition name p (parms @ body)

    override this.VisitFormalParams(ctx: SBParser.FormalParamsContext) =
        let p = posOfTree ctx
        let parms =
            ctx.ID()
            |> Seq.map (fun id -> mk NodeKind.Identifier (id.GetText()) (posOfTree id) [])
            |> Seq.toList
        single NodeKind.Parameters "" p parms

    // -------------------------------------------------------------------------
    // simpleStmt labeled alternatives
    // -------------------------------------------------------------------------

    override this.VisitDim(ctx: SBParser.DimContext) =
        let p = posOfTree ctx
        let children = ctx.dimList().Accept(this)
        single NodeKind.Dim "" p children

    override this.VisitDimList(ctx: SBParser.DimListContext) =
        ctx.dimItem() |> Seq.collect (fun d -> d.Accept(this)) |> Seq.toList

    override this.VisitDimItem(ctx: SBParser.DimItemContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()

        let dims =
            match ctx.exprList() with
            | null -> []
            | e -> this.CollectExprList(e)

        single NodeKind.ArrayOrFunctionCall name p dims

    override this.VisitLocalDecl(ctx: SBParser.LocalDeclContext) =
        let p = posOfTree ctx
        let children = ctx.localList().Accept(this)
        single NodeKind.Local "" p children

    override this.VisitLocalList(ctx: SBParser.LocalListContext) =
        ctx.localItem() |> Seq.collect (fun i -> i.Accept(this)) |> Seq.toList

    override this.VisitLocalItem(ctx: SBParser.LocalItemContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()

        let children =
            match ctx.exprList() with
            | null -> []
            | e -> this.CollectExprList(e)

        if List.isEmpty children then
            single NodeKind.Identifier name p []
        else
            single NodeKind.ArrayOrFunctionCall name p children

    override this.VisitImplicitDecl(ctx: SBParser.ImplicitDeclContext) =
        let p = posOfTree ctx
        let kw = ctx.Implic().GetText()
        let suffix =
            if String.IsNullOrEmpty kw then "" else string kw.[kw.Length - 1]

        let children =
            ctx.unparenthesizedlist().expr()
            |> Seq.collect (fun e -> e.Accept(this))
            |> Seq.toList

        single NodeKind.Implicit suffix p children

    override this.VisitReferenceDecl(ctx: SBParser.ReferenceDeclContext) =
        let p = posOfTree ctx
        let children = this.CollectUnparenthesizedArgs(ctx.unparenthesizedlist())
        single NodeKind.Reference "" p children

    override this.VisitRemark(ctx: SBParser.RemarkContext) =
        let p = posOfTree ctx
        let txt =
            match ctx.Comment() with
            | null -> ctx.GetText()
            | c -> c.GetText()
        single NodeKind.Remark txt p []

    override this.VisitNextStmt(ctx: SBParser.NextStmtContext) =
        let p = posOfTree ctx
        single NodeKind.NextStmt (ctx.ID().GetText()) p []

    override this.VisitExitStmt(ctx: SBParser.ExitStmtContext) =
        let p = posOfTree ctx
        single NodeKind.ExitStmt (ctx.ID().GetText()) p []

    override this.VisitGotoStatement(ctx: SBParser.GotoStatementContext) =
        let p = posOfTree ctx
        let args = ctx.gotoStmt().Accept(this)
        single NodeKind.ProcedureCall "GOTO" p args

    override this.VisitGosubStatement(ctx: SBParser.GosubStatementContext) =
        let p = posOfTree ctx
        let args = ctx.gosubStmt().Accept(this)
        single NodeKind.ProcedureCall "GOSUB" p args

    override this.VisitOnGotoStatement(ctx: SBParser.OnGotoStatementContext) =
        let p = posOfTree ctx
        let args = ctx.onGotoStmt().Accept(this)
        single NodeKind.ProcedureCall "ON-GOTO" p args

    override this.VisitOnGosubStatement(ctx: SBParser.OnGosubStatementContext) =
        let p = posOfTree ctx
        let args = ctx.onGosubStmt().Accept(this)
        single NodeKind.ProcedureCall "ON-GOSUB" p args

    override this.VisitReturnStmt(ctx: SBParser.ReturnStmtContext) =
        let p = posOfTree ctx
        let children =
            match ctx.expr() with
            | null -> []
            | e -> e.Accept(this)
        single NodeKind.Return "" p children

    override this.VisitDataStmt(ctx: SBParser.DataStmtContext) =
        let p = posOfTree ctx
        let children = this.CollectExprList(ctx.exprList())
        single NodeKind.Data "" p children

    override this.VisitReadStmt(ctx: SBParser.ReadStmtContext) =
        let p = posOfTree ctx
        let children = ctx.lvalueList().Accept(this)
        single NodeKind.Read "" p children

    override this.VisitRestoreStmt(ctx: SBParser.RestoreStmtContext) =
        let p = posOfTree ctx
        let children =
            match ctx.expr() with
            | null -> []
            | e -> e.Accept(this)
        single NodeKind.Restore "" p children

    override this.VisitAssignmentStatement(ctx: SBParser.AssignmentStatementContext) =
        ctx.assignmentStmt().Accept(this)

    override this.VisitChannelProcCall(ctx: SBParser.ChannelProcCallContext) =
        ctx.channelProcCallStmt().Accept(this)

    override this.VisitProcedureCallStatement(ctx: SBParser.ProcedureCallStatementContext) =
        ctx.procedureCallStmt().Accept(this)

    // -------------------------------------------------------------------------
    // Call / assignment subrules
    // -------------------------------------------------------------------------

    override this.VisitGotoStmt(ctx: SBParser.GotoStmtContext) =
        ctx.expr().Accept(this)

    override this.VisitGosubStmt(ctx: SBParser.GosubStmtContext) =
        ctx.expr().Accept(this)

    override this.VisitOnGotoStmt(ctx: SBParser.OnGotoStmtContext) =
        let selector = ctx.expr().Accept(this)
        let targets = this.CollectExprList(ctx.exprList())
        selector @ targets

    override this.VisitOnGosubStmt(ctx: SBParser.OnGosubStmtContext) =
        let selector = ctx.expr().Accept(this)
        let targets = this.CollectExprList(ctx.exprList())
        selector @ targets

    override this.VisitAssignmentStmt(ctx: SBParser.AssignmentStmtContext) =
        let p = posOfTree ctx
        let lhs = this.SingleAstOrUnknown(ctx.lvalue(), ctx.lvalue().Accept(this))
        let rhs = this.SingleAstOrUnknown(ctx.expr(), ctx.expr().Accept(this))
        single NodeKind.Assignment "" p [ lhs; rhs ]

    override this.VisitChannelProcCallStmt(ctx: SBParser.ChannelProcCallStmtContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()

        let channel = this.SingleAstOrUnknown(ctx.chanArg(), ctx.chanArg().Accept(this))

        let args =
            ctx.arg()
            |> Seq.collect (fun a -> a.Accept(this))
            |> Seq.toList

        single NodeKind.ProcedureCall (name + "#") p (channel :: args)

    override this.VisitProcedureCallStmt(ctx: SBParser.ProcedureCallStmtContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()

        let args =
            match ctx.stmtArglist() with
            | null -> []
            | sa -> sa.Accept(this)

        single NodeKind.ProcedureCall name p args

    override this.VisitLvalue(ctx: SBParser.LvalueContext) =
        ctx.postfixName().Accept(this)

    override this.VisitLvalueList(ctx: SBParser.LvalueListContext) =
        ctx.lvalue() |> Seq.collect (fun lv -> lv.Accept(this)) |> Seq.toList

    override this.VisitStmtArglist(ctx: SBParser.StmtArglistContext) =
        if not (isNull (ctx.parenthesizedlist())) then
            ctx.parenthesizedlist().Accept(this)
        else
            ctx.unparenthesizedlist().Accept(this)

    override this.VisitChanArg(ctx: SBParser.ChanArgContext) =
        ctx.expr().Accept(this)

    override this.VisitArg(ctx: SBParser.ArgContext) =
        if not (isNull (ctx.chanArg())) then
            ctx.chanArg().Accept(this)
        else
            ctx.expr().Accept(this)

    // -------------------------------------------------------------------------
    // Postfix names
    // -------------------------------------------------------------------------

    override this.VisitPostfixName(ctx: SBParser.PostfixNameContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()

        let children =
            match ctx.postfixArg() with
            | null -> []
            | pa -> pa.Accept(this)

        match children with
        | [] -> single NodeKind.Identifier name p []
        | _ -> single NodeKind.ArrayOrFunctionCall name p children

    override this.VisitPostfixArg(ctx: SBParser.PostfixArgContext) =
        match ctx.postfixArgList() with
        | null -> []
        | pal -> pal.Accept(this)

    override this.VisitPostfixArgList(ctx: SBParser.PostfixArgListContext) =
        ctx.postfixArgItem() |> Seq.map this.PostfixArgItemToAst |> Seq.toList

    override this.VisitPostfixArgItem(ctx: SBParser.PostfixArgItemContext) =
        [ this.PostfixArgItemToAst(ctx) ]

    // -------------------------------------------------------------------------
    // End-marker rules
    // -------------------------------------------------------------------------

    override this.VisitEndDef(ctx: SBParser.EndDefContext) = []
    override this.VisitEndFor(ctx: SBParser.EndForContext) = []
    override this.VisitEndRepeat(ctx: SBParser.EndRepeatContext) = []
    override this.VisitEndIf(ctx: SBParser.EndIfContext) = []
    override this.VisitEndSelect(ctx: SBParser.EndSelectContext) = []
    override this.VisitEndWhen(ctx: SBParser.EndWhenContext) = []

    // -------------------------------------------------------------------------
    // Control flow
    // -------------------------------------------------------------------------

    override this.VisitForStmt(ctx: SBParser.ForStmtContext) =
        let p = posOfTree ctx

        let ids = ctx.ID() |> Seq.toList
        let name =
            match ids with
            | id :: _ -> id.GetText()
            | [] -> ""

        let exprs = ctx.expr() |> Seq.toList

        let startExpr =
            match exprs with
            | first :: _ -> this.SingleAstOrUnknown(first, first.Accept(this))
            | [] -> mk NodeKind.Unknown "" p []

        let endExpr =
            match exprs with
            | _ :: second :: _ -> this.SingleAstOrUnknown(second, second.Accept(this))
            | _ -> mk NodeKind.Unknown "" p []

        let body =
            if not (isNull (ctx.stmtlist())) then
                this.CollectStmtList(ctx.stmtlist())
            else
                this.VisitLineList(ctx.line())

        let children =
            match exprs with
            | _ :: _ :: third :: _ ->
                let stepExpr = this.SingleAstOrUnknown(third, third.Accept(this))
                [ mk NodeKind.Identifier name p []; startExpr; endExpr; stepExpr ] @ body
            | _ ->
                [ mk NodeKind.Identifier name p []; startExpr; endExpr ] @ body

        single NodeKind.For "" p children

    override this.VisitRepeatStmt(ctx: SBParser.RepeatStmtContext) =
        let p = posOfTree ctx

        let ids = ctx.ID() |> Seq.toList
        let name =
            match ids with
            | id :: _ -> id.GetText()
            | [] -> ""

        let nameNode = mk NodeKind.Identifier name p []

        let body =
            if not (isNull (ctx.stmtlist())) then
                this.CollectStmtList(ctx.stmtlist())
            else
                this.VisitLineList(ctx.line())

        single NodeKind.Repeat "" p (nameNode :: body)

    override this.VisitIfStmt(ctx: SBParser.IfStmtContext) =
        let p = posOfTree ctx
        let cond = this.SingleAstOrUnknown(ctx.expr(), ctx.expr().Accept(this))

        let thenPart, elsePart =
            if ctx.stmtlist().Length > 0 then
                let t = this.CollectStmtList(ctx.stmtlist(0))
                let e =
                    if ctx.stmtlist().Length > 1 then
                        this.CollectStmtList(ctx.stmtlist(1))
                    else
                        []
                t, e
            else
                let t =
                    match ctx.ifBlock() with
                    | null -> []
                    | ib -> this.VisitLineList(ib.line())

                let e =
                    match ctx.elseBlock() with
                    | null -> []
                    | eb -> this.VisitLineList(eb.line())

                t, e

        single NodeKind.If "" p (cond :: thenPart @ elsePart)

    override this.VisitIfBlock(ctx: SBParser.IfBlockContext) =
        this.VisitLineList(ctx.line())

    override this.VisitElseBlock(ctx: SBParser.ElseBlockContext) =
        this.VisitLineList(ctx.line())

    override this.VisitSelectStmt(ctx: SBParser.SelectStmtContext) =
        let p = posOfTree ctx
        let selector = this.SingleAstOrUnknown(ctx.expr(), ctx.expr().Accept(this))

        let clauses =
            ctx.selectItem()
            |> Seq.collect (fun si -> si.Accept(this))
            |> Seq.toList

        single NodeKind.Select "" p (selector :: clauses)

    override this.VisitSelectItem(ctx: SBParser.SelectItemContext) =
        if not (isNull (ctx.onClause())) then
            ctx.onClause().Accept(this)
        else
            ctx.line().Accept(this)

    override this.VisitOnClause(ctx: SBParser.OnClauseContext) =
        let p = posOfTree ctx
        let selectorExpr = this.SingleAstOrUnknown(ctx.expr(), ctx.expr().Accept(this))
        let rangeAst = this.SingleAstOrUnknown(ctx.rangeexpr(), ctx.rangeexpr().Accept(this))

        let body =
            match ctx.stmtlist() with
            | null -> []
            | s -> this.CollectStmtList(s)

        single NodeKind.OnClause "" p (selectorExpr :: rangeAst :: body)

    override this.VisitWhenErrorStmt(ctx: SBParser.WhenErrorStmtContext) =
        let p = posOfTree ctx
        let body = this.VisitLineList(ctx.line())
        single NodeKind.WhenError "" p body

    override this.VisitWhenCondStmt(ctx: SBParser.WhenCondStmtContext) =
        let p = posOfTree ctx
        let condition = this.SingleAstOrUnknown(ctx.expr(), ctx.expr().Accept(this))
        let body = this.VisitLineList(ctx.line())
        single NodeKind.When "" p (condition :: body)

    // -------------------------------------------------------------------------
    // Expressions
    // -------------------------------------------------------------------------

    override this.VisitExpr(ctx: SBParser.ExprContext) =
        ctx.orExpr().Accept(this)

    override this.VisitOrExpr(ctx: SBParser.OrExprContext) =
        let p = posOfTree ctx

        let terms =
            ctx.andExpr()
            |> Seq.map (fun x -> this.SingleAstOrUnknown(x, x.Accept(this)))
            |> Seq.toList

        let ops = collectOperatorTexts ctx (set [ "OR"; "XOR" ])

        match terms with
        | [] -> []
        | first :: rest -> [ this.FoldLeft(p, first, ops, rest) ]

    override this.VisitAndExpr(ctx: SBParser.AndExprContext) =
        let p = posOfTree ctx

        let terms =
            ctx.notExpr()
            |> Seq.map (fun x -> this.SingleAstOrUnknown(x, x.Accept(this)))
            |> Seq.toList

        let ops = collectOperatorTexts ctx (set [ "AND" ])

        match terms with
        | [] -> []
        | first :: rest -> [ this.FoldLeft(p, first, ops, rest) ]

    override this.VisitNotExpr(ctx: SBParser.NotExprContext) =
        let p = posOfTree ctx
        if not (isNull (ctx.compareExpr())) then
            ctx.compareExpr().Accept(this)
        else
            let rhs = this.SingleAstOrUnknown(ctx.notExpr(), ctx.notExpr().Accept(this))
            single NodeKind.UnaryExpr "NOT" p [ rhs ]

    override this.VisitCompareExpr(ctx: SBParser.CompareExprContext) =
        let p = posOfTree ctx

        let terms =
            ctx.instrExpr()
            |> Seq.map (fun x -> this.SingleAstOrUnknown(x, x.Accept(this)))
            |> Seq.toList

        let ops = collectOperatorTexts ctx (set [ "="; "<>"; "<"; "<="; ">"; ">="; "==" ])

        match terms with
        | [] -> []
        | first :: rest -> [ this.FoldLeft(p, first, ops, rest) ]

    override this.VisitInstrExpr(ctx: SBParser.InstrExprContext) =
        let p = posOfTree ctx

        let parts =
            ctx.concatExpr()
            |> Seq.map (fun x -> this.SingleAstOrUnknown(x, x.Accept(this)))
            |> Seq.toList

        match parts with
        | [ singleAst ] -> [ singleAst ]
        | [ lhs; rhs ] -> single NodeKind.BinaryExpr "INSTR" p [ lhs; rhs ]
        | _ -> single NodeKind.Unknown "" p []

    override this.VisitConcatExpr(ctx: SBParser.ConcatExprContext) =
        let p = posOfTree ctx

        let terms =
            ctx.addExpr()
            |> Seq.map (fun x -> this.SingleAstOrUnknown(x, x.Accept(this)))
            |> Seq.toList

        let ops = collectOperatorTexts ctx (set [ "&" ])

        match terms with
        | [] -> []
        | first :: rest -> [ this.FoldLeft(p, first, ops, rest) ]

    override this.VisitAddExpr(ctx: SBParser.AddExprContext) =
        let p = posOfTree ctx

        let terms =
            ctx.mulExpr()
            |> Seq.map (fun x -> this.SingleAstOrUnknown(x, x.Accept(this)))
            |> Seq.toList

        let ops = collectOperatorTexts ctx (set [ "+"; "-" ])

        match terms with
        | [] -> []
        | first :: rest -> [ this.FoldLeft(p, first, ops, rest) ]

    override this.VisitMulExpr(ctx: SBParser.MulExprContext) =
        let p = posOfTree ctx

        let terms =
            ctx.powExpr()
            |> Seq.map (fun x -> this.SingleAstOrUnknown(x, x.Accept(this)))
            |> Seq.toList

        let ops = collectOperatorTexts ctx (set [ "*"; "/"; "MOD"; "DIV" ])

        match terms with
        | [] -> []
        | first :: rest -> [ this.FoldLeft(p, first, ops, rest) ]

    override this.VisitPowExpr(ctx: SBParser.PowExprContext) =
        let p = posOfTree ctx

        let terms =
            ctx.unaryExpr()
            |> Seq.map (fun x -> this.SingleAstOrUnknown(x, x.Accept(this)))
            |> Seq.toList

        let ops = collectOperatorTexts ctx (set [ "^" ])

        match terms with
        | [] -> []
        | first :: rest -> [ this.FoldLeft(p, first, ops, rest) ]

    override this.VisitUnaryExpr(ctx: SBParser.UnaryExprContext) =
        let p = posOfTree ctx
        if not (isNull (ctx.primary())) then
            ctx.primary().Accept(this)
        else
            let op = ctx.GetChild(0).GetText()
            let rhs = this.SingleAstOrUnknown(ctx.unaryExpr(), ctx.unaryExpr().Accept(this))
            single NodeKind.UnaryExpr op p [ rhs ]

    override this.VisitPrimary(ctx: SBParser.PrimaryContext) =
        let p = posOfTree ctx

        if not (isNull (ctx.Integer())) then
            single NodeKind.NumberLiteral (ctx.Integer().GetText()) p []
        elif not (isNull (ctx.Real())) then
            single NodeKind.NumberLiteral (ctx.Real().GetText()) p []
        elif not (isNull (ctx.String())) then
            single NodeKind.StringLiteral (ctx.String().GetText()) p []
        elif not (isNull (ctx.postfixName())) then
            ctx.postfixName().Accept(this)
        elif not (isNull (ctx.expr())) then
            ctx.expr().Accept(this)
        else
            single NodeKind.Unknown "" p []

    // -------------------------------------------------------------------------
    // Shared list-ish rules
    // -------------------------------------------------------------------------

    override this.VisitParenthesizedlist(ctx: SBParser.ParenthesizedlistContext) =
        ctx.expr() |> Seq.collect (fun e -> e.Accept(this)) |> Seq.toList

    override this.VisitUnparenthesizedlist(ctx: SBParser.UnparenthesizedlistContext) =
        ctx.expr() |> Seq.collect (fun e -> e.Accept(this)) |> Seq.toList

    override this.VisitSeparator(ctx: SBParser.SeparatorContext) =
        []

    override this.VisitRangeexpr(ctx: SBParser.RangeexprContext) =
        let p = posOfTree ctx
        let exprs = ctx.expr() |> Seq.toList

        match exprs with
        | [ a ] ->
            a.Accept(this)
        | [ a; b ] ->
            let left = this.SingleAstOrUnknown(a, a.Accept(this))
            let right = this.SingleAstOrUnknown(b, b.Accept(this))
            single NodeKind.BinaryExpr "TO" p [ left; right ]
        | _ when not (isNull (ctx.Remainder())) ->
            single NodeKind.Identifier "REMAINDER" p []
        | _ ->
            single NodeKind.Unknown "" p []

    override this.VisitLineNumber(ctx: SBParser.LineNumberContext) =
        []

    // -------------------------------------------------------------------------
    // Fallback terminal / error handling
    // -------------------------------------------------------------------------

    override this.VisitTerminal(node: ITerminalNode) =
        let sym = node.Symbol
        let p = (sym.Line, sym.Column)

        match SBLexer.DefaultVocabulary.GetSymbolicName(sym.Type) with
        | "INTEGER" -> single NodeKind.NumberLiteral sym.Text p []
        | "REAL" -> single NodeKind.NumberLiteral sym.Text p []
        | "STRING" -> single NodeKind.StringLiteral sym.Text p []
        | _ -> []

    override this.VisitErrorNode(node: IErrorNode) =
        let p =
            match node.Symbol with
            | null -> (0, 0)
            | s -> (s.Line, s.Column)

        single NodeKind.Unknown (node.GetText()) p []

    override this.VisitChildren(node: IRuleNode) =
        [ for i in 0 .. node.ChildCount - 1 do
            yield! node.GetChild(i).Accept(this) ]

// -----------------------------------------------------------------------------
// Entry point
// -----------------------------------------------------------------------------

let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)    
    
(* module ParseTreeVisitor

open Antlr4.Runtime.Tree
open System
open System.Collections.Generic
open Utility

(* The `ParseTreeVisitor` module is designed to traverse an ANTLR-generated parse tree and convert it into an Abstract Syntax Tree (AST). It defines various `NodeBehavior` types to specify how different parse tree nodes should be handled. The module includes dictionaries mapping parser rule contexts and token types to these behaviors. Helper functions are provided to extract positions and handle specific node types. The `ASTBuildingVisitor` class extends the base visitor to implement the traversal logic, applying the defined behaviors to produce the AST. The `convertTreeToAst` function initiates the conversion process using this visitor. *)

// ----------------------------------------------------------------
// 1. Types for Behavior
// ----------------------------------------------------------------

/// Defines how a parse tree node should be handled when building the AST.
type NodeBehavior =
    /// Create an AST node with the specified kind and use the node's text as content.
    | Produce of NodeKind

    /// Create an AST node with the specified kind but omit the node's text (i.e., store "").
    | ProduceNameOnly of NodeKind

    /// Create an AST node for an operator expression and store the operator text.
    | ProduceNameOnlyWithOperator of NodeKind

    /// Do not produce any AST node for this rule node (discard it entirely).
    | Discard

    /// Do not produce a node here; instead, return the child results as-is.
    | BubbleUp

// ----------------------------------------------------------------
// 2. Known Behaviors for Parser Rules and Tokens
// ----------------------------------------------------------------

/// Maps an SBParser context type to a NodeBehavior.
let typedBehaviors: IDictionary<Type, NodeBehavior> =
    dict [
        (typeof<SBParser.AssignContext>, ProduceNameOnly NodeKind.Assignment)
        (typeof<SBParser.AssigntoContext>, Discard)
        (typeof<SBParser.BinaryContext>, ProduceNameOnlyWithOperator NodeKind.BinaryExpr)
        (typeof<SBParser.ComparisonContext>, ProduceNameOnly NodeKind.BinaryExpr)
        (typeof<SBParser.DimContext>, Produce NodeKind.Dim)
        (typeof<SBParser.EndDefContext>, Discard)
        (typeof<SBParser.EndIfContext>, Discard)
        (typeof<SBParser.EndForContext>, Discard)
        (typeof<SBParser.EndRepeatContext>, Discard)
        (typeof<SBParser.ExitstmtContext>, Produce NodeKind.Exitstmt)
        (typeof<SBParser.ExprContext>, BubbleUp)
        (typeof<SBParser.ForloopContext>, ProduceNameOnly NodeKind.For)
        (typeof<SBParser.FunctionCallContext>, ProduceNameOnly NodeKind.FunctionCall)
        (typeof<SBParser.FunctionDefinitionContext>, ProduceNameOnly NodeKind.Function)
        (typeof<SBParser.FunchdrContext>, BubbleUp)
        (typeof<SBParser.IdentifierContext>, ProduceNameOnly NodeKind.Identifier)
        (typeof<SBParser.IfContext>, ProduceNameOnly NodeKind.If)
        (typeof<SBParser.ImplicitContext>, ProduceNameOnly NodeKind.Implicit)
        (typeof<SBParser.LineContext>, BubbleUp)
        (typeof<SBParser.LineNumberContext>, Discard)
        (typeof<SBParser.LocalContext>, ProduceNameOnly NodeKind.Local)
        (typeof<SBParser.ParametersContext>, ProduceNameOnly NodeKind.Parameters)
        (typeof<SBParser.ParenlistContext>, BubbleUp)
        (typeof<SBParser.ParenthesizedlistContext>, BubbleUp)
        (typeof<SBParser.PrimaryContext>, BubbleUp)
        (typeof<SBParser.ProcedureDefinitionContext>, ProduceNameOnly NodeKind.Procedure)
        (typeof<SBParser.ProcedureCallContext>, ProduceNameOnly NodeKind.ProcedureCall)
        (typeof<SBParser.ProchdrContext>, BubbleUp)
        (typeof<SBParser.ProgramContext>, ProduceNameOnly NodeKind.Program)
        (typeof<SBParser.RemarkContext>, Produce NodeKind.Remark)
        (typeof<SBParser.RepeatContext>, ProduceNameOnly NodeKind.Repeat)
        (typeof<SBParser.SeparatorContext>, Discard)
        (typeof<SBParser.StmtlistContext>, BubbleUp)
        (typeof<SBParser.UnaryContext>, BubbleUp)
        (typeof<SBParser.UnparenthesizedlistContext>, BubbleUp)
    ]

/// Maps a token name (string) to a NodeBehavior.
let terminalBehaviors: IDictionary<string, NodeBehavior> =
    dict [
        "COMMA", Discard
        "COMMENT", Discard
        "DEFFUNC", Discard
        "DEFPROC", Discard
        "DIMENSION", Discard
        "ENDDEF", Discard
        "ENDREPEAT", Discard
        "EOF", Discard
        "EQUAL", Discard
        "FOR", Discard
        "ID", Discard
        "IF", Discard
        "INTEGER", Produce NodeKind.NumberLiteral
        "LEFTPAREN", Discard
        "LOCAL", Discard
        "NEWLINE", Discard
        "REPEAT", Discard
        "RIGHTPAREN", Discard
        "TO", Discard
        "EXIT", Discard
        "STRING", Produce NodeKind.StringLiteral
        "COLON", Discard
        "SEMI", Discard
        "PLUS", Produce NodeKind.Plus
        "MINUS", Produce NodeKind.Minus
        "MULTIPLY", Produce NodeKind.Multiply
        "DIVIDE", Produce NodeKind.Divide
        "MOD", Produce NodeKind.Mod
        "DIV", Produce NodeKind.Div
        "AND", Discard
        "OR", Discard
        "XOR", Discard
        "CARET", Discard
        "NOT", Discard
        "TILDE", Discard
        "INSTR", Discard
        "AMP", Discard
        "QUESTION", Discard
        "POINT", Discard
        "BANG", Discard
        "GREATER", Discard
        "GREATEREQUAL", Discard
        "LESS", Discard
        "LESSEQUAL", Discard
        "THEN", Discard
        "ELSE", Discard
        "ENDFOR", Discard
        "ENDIF", Discard
    ]

// ----------------------------------------------------------------
// 3. Helper Functions
// ----------------------------------------------------------------

/// Extracts (line, column) position from the first token in the parse tree node.
let getPosition (ruleNode: IRuleNode) : int * int =
    let rec findFirstToken (node: IParseTree) =
        match node with
        | :? ITerminalNode as t -> Some t.Symbol
        | _ when node.ChildCount > 0 -> findFirstToken (node.GetChild(0))
        | _ -> None

    let rec findLastToken (node: IParseTree) =
        match node with
        | :? ITerminalNode as t -> Some t.Symbol
        | _ when node.ChildCount > 0 -> findLastToken (node.GetChild(node.ChildCount - 1))
        | _ -> None

    match findFirstToken ruleNode, findLastToken ruleNode with
    | Some first, Some _ -> (first.Line, first.Column)
    | _ -> (0, 0)

/// Handles special logic for a ProcedureDefinition node
let handleProcedureBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    match children with
    | [] ->
        // Fallback if no child nodes
        [ createAstNode NodeKind.ProcedureDefinition (ruleNode.GetChild(0).GetChild(1).GetText()) pos [] ]
    | _ ->
        [ createAstNode NodeKind.ProcedureDefinition (ruleNode.GetChild(0).GetChild(1).GetText()) pos children ]

/// Handles special logic for a FunctionDefinition node
let handleFunctionBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    match children with
    | [] ->
        [ createAstNode NodeKind.FunctionDefinition (ruleNode.GetChild(0).GetChild(1).GetText()) pos [] ]
    | _ ->
        [ createAstNode NodeKind.FunctionDefinition (ruleNode.GetChild(0).GetChild(1).GetText()) pos children ]

/// Handles special logic for a ProcedureCall
let handleProcedureCallBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    match children with
    | [] ->
        [ createAstNode NodeKind.ProcedureCall (ruleNode.GetChild(0).GetText()) pos [] ]
    | first :: args ->
        [ createAstNode NodeKind.ProcedureCall (ruleNode.GetChild(0).GetText()) pos args ]

/// Handles special logic for a FunctionCall
let handleFunctionCallBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    match children with
    | [] ->
        [ createAstNode NodeKind.FunctionCall (ruleNode.GetText()) pos [] ]
    | first :: args ->
        [ createAstNode NodeKind.FunctionCall first.Value pos args ]

/// Optionally handle an Implicit node, e.g. IMPLICIT% or IMPLICIT$
let handleImplicitBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    // The last char in the first child's text might be '%' or '$'
    let decorator =
        let txt = ruleNode.GetChild(0).GetText()
        if String.IsNullOrEmpty(txt) then ""
        else string (txt.[txt.Length - 1])
    let _::tl = children
    [ createAstNode NodeKind.Implicit decorator pos tl ]

/// Optionally handle a parameters node
let handleParametersBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    [ createAstNode NodeKind.Parameters "" pos children ]
// set mode so if it looks like an ID make it a parameter
/// Basic identifier handling (could be array indexing or function usage)
let handleIdentifierBehavior (children: ASTNode list) (pos: int * int) (ruleNode: IRuleNode) =
    let idText = ruleNode.GetText()
    match children with
    | [] ->
        [ createAstNode NodeKind.Identifier idText pos [] ]
    | [single] ->
        [ createAstNode NodeKind.ArrayOrFunctionCall idText pos [single] ]
    | multiple ->
        [ createAstNode NodeKind.Identifier idText pos multiple ]

// ----------------------------------------------------------------
// 4. The ASTBuildingVisitor Implementation
// ----------------------------------------------------------------

type ASTBuildingVisitor() =
    inherit SBBaseVisitor<ASTNode list>()

    override this.VisitChildren(ruleNode: IRuleNode) =
        // 1) Collect AST nodes from each child
        let childResults =
            [ for i in 0 .. ruleNode.ChildCount - 1 do
                yield! ruleNode.GetChild(i).Accept(this) ]

        // 2) Identify context type & text
        let ctxType = ruleNode.RuleContext.GetType()
        let content = ruleNode.GetText()
        let position = getPosition ruleNode

        // 3) Determine behavior
        let behavior =
            match typedBehaviors.TryGetValue(ctxType) with
            | true, b -> b
            | false, _ -> Produce NodeKind.Unknown

        // 4) Apply the behavior
        match behavior with
        | Produce kind ->
            [ createAstNode kind content position childResults ]

        | ProduceNameOnly kind ->
            match ctxType with
            | _ when ctxType = typeof<SBParser.IdentifierContext> ->
                handleIdentifierBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.ProcedureCallContext> ->
                handleProcedureCallBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.FunctionCallContext> ->
                handleFunctionCallBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.ProcedureDefinitionContext> ->
                handleProcedureBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.FunctionDefinitionContext> ->
                handleFunctionBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.ImplicitContext> ->
                handleImplicitBehavior childResults position ruleNode
            | _ when ctxType = typeof<SBParser.ParametersContext> ->
                handleParametersBehavior childResults position ruleNode
            | _ ->
                [ createAstNode kind "" position childResults ]

        | ProduceNameOnlyWithOperator kind ->
            let operatorNodeOpt =
                childResults
                |> List.tryFind (fun c ->
                    match c.TokenType with
                    | NodeKind.Plus
                    | NodeKind.Minus
                    | NodeKind.Multiply
                    | NodeKind.Divide
                    | NodeKind.Mod
                    | NodeKind.Div -> true
                    | _ -> false)
            let operatorText =
                operatorNodeOpt
                |> Option.map (fun op -> op.Value)
                |> Option.defaultValue "unknown-op"
            let filteredChildren =
                match operatorNodeOpt with
                | Some op -> childResults |> List.filter (fun x -> x <> op)
                | None -> childResults

            [ createAstNode kind operatorText position filteredChildren ]

        | Discard ->
            []

        | BubbleUp ->
            childResults

    override this.VisitTerminal(terminalNode: ITerminalNode) =
        let symbol = terminalNode.Symbol
        let tokenName = SBLexer.DefaultVocabulary.GetSymbolicName(symbol.Type).ToUpper()

        match terminalBehaviors.TryGetValue(tokenName) with
        | true, Produce kind ->
            [ createAstNode kind symbol.Text (symbol.Line, symbol.Column) [] ]
        | true, ProduceNameOnly kind ->
            [ createAstNode kind "" (symbol.Line, symbol.Column) [] ]
        | true, Discard ->
            []
        | _ ->
            [ createAstNode NodeKind.Unknown symbol.Text (symbol.Line, symbol.Column) [] ]

    override this.VisitErrorNode(errorNode: IErrorNode) =
        [ createAstNode NodeKind.Unknown (errorNode.ToString()) (0, 0) [] ]

// ----------------------------------------------------------------
// 5. Converting a Parse Tree to an AST
// ----------------------------------------------------------------

let convertTreeToAst (parseTree: IParseTree) : ASTNode list =
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)
*)