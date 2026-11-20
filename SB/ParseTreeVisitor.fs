module ParseTreeVisitor

open System
open Antlr4.Runtime
open Antlr4.Runtime.Tree

open SyntaxAst
open Types

// ParseTreeVisitor is the translation layer from the ANTLR parse tree into the
// normalized AST used by the rest of the compiler.
//
// Its job is not semantic analysis and not syntax validation beyond what the
// parser has already recovered. Instead, it:
// - converts parser contexts into the compact AST DU shape
// - preserves source positions, including ambient BASIC line numbers
// - normalizes surface-syntax variants into shared AST nodes
// - produces a best-effort tree even when ANTLR recovered from syntax errors
//
// This keeps later stages insulated from parser-specific context types and from
// many of the incidental distinctions present in the grammar.
//
// Convert ANTLR token/tree locations into the source positions carried through the AST.
let mutable private ambientBasicLineNo : int option = None

let private posOfToken (t: IToken) : SourcePosition =
    { BasicLineNo = ambientBasicLineNo
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
    | None -> { BasicLineNo = ambientBasicLineNo; EditorLineNo = 0; Column = 0 }

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
    | _ -> mkIdentifier (posOfTree tree) ""

let private singleClause tree nodes =
    match nodes with
    | [ ClauseNode clause ] -> clause
    | _ -> SelectClause(posOfTree tree, mkIdentifier (posOfTree tree) "", mkIdentifier (posOfTree tree) "", None)

let private emptySeq<'T> : seq<'T> = Seq.empty

// The visitor builds a best-effort AST, even when the parser has recovered from syntax errors.
type ASTBuildingVisitor() =
    inherit SBBaseVisitor<VisitedNode list>()

    let mutable selectSelectorStack: Expr list = []
    let printContinueSentinel = "\"\uE000PRINT_CONTINUE\uE000\""
    let printCommaSentinel = "\"\uE000PRINT_COMMA\uE000\""
    // '!' (a space unless at the start of a line) and 'TO n' (move to column n; the column
    // expression follows this marker).
    let printPlingSentinel = "\"\uE000PRINT_PLING\uE000\""
    let printToSentinel = "\"\uE000PRINT_TO\uE000\""

    let openBuiltInNames =
        set [ "OPEN"; "OPEN_IN"; "OPEN_NEW"; "APPEND" ]

    member private _.WithBasicLineNo (basicLineNo: int option) (action: unit -> 'T) =
        // BASIC line numbers are carried ambiently because many nested parser
        // contexts do not repeat the numbered-line information directly, but AST
        // nodes still need to know which numbered source line they came from.
        let previous = ambientBasicLineNo
        ambientBasicLineNo <- basicLineNo
        try
            action ()
        finally
            ambientBasicLineNo <- previous

    member private this.SafeAcceptExpr(tree: IParseTree) =
        if isNull tree then
            mkIdentifier { BasicLineNo = ambientBasicLineNo; EditorLineNo = 0; Column = 0 } ""
        else
            singleExpr tree (tree.Accept(this))

    member private this.SafeChildren<'T when 'T : null>(nodes: seq<'T>) =
        if isNull (box nodes) then emptySeq else nodes

    member private this.BuildSelectClauses (pos: SourcePosition) (selectorExpr: Expr) (rangeExprs: Expr list) (body: Block option) =
        rangeExprs
        |> List.map (fun rangeAst -> ClauseNode(SelectClause(pos, selectorExpr, rangeAst, body)))

    member private _.LooksLikeDeviceLiteral (name: string) =
        let lowered = normalizeIdentifier name

        let knownPrefixes =
            [ "con_"; "scr_"; "nul_"; "prt_"; "ram"; "flp"; "win"; "mdv" ]
            |> List.map normalizeIdentifier

        knownPrefixes |> List.exists lowered.StartsWith

    member private _.IsIgnoredDirectiveName (name: string) =
        (normalizeIdentifier name).StartsWith("TURBO_")

    member private this.NormalizeOpenArgs (name: string) (args: Expr list) =
        if not (openBuiltInNames.Contains(normalizeIdentifier name)) then
            args
        else
            let rec normalize = function
                | GroupedExpr(id, pos, inner) -> GroupedExpr(id, pos, normalize inner)
                | Identifier(_, pos, value) when this.LooksLikeDeviceLiteral value ->
                    mkStringLiteral pos ("\"" + value + "\"")
                | other -> other
            args |> List.map normalize

    // Collection helpers collapse parser-specific child lists into the exact AST
    // collections expected by later stages, so the individual Visit* overrides can
    // stay focused on shape conversion instead of list plumbing.
    member private this.VisitLineList(lines: seq<SBParser.LineContext>) =
        this.SafeChildren(lines) |> Seq.map (fun l -> singleLine l (l.Accept(this))) |> Seq.toList

    member private this.CollectStmtList(ctx: SBParser.StmtlistContext) =
        this.SafeChildren(ctx.stmt())
        |> Seq.collect (fun s ->
            s.Accept(this)
            |> Seq.choose (function
                | StmtNode stmt -> Some stmt
                | _ -> None))
        |> Seq.toList

    member private this.CollectPlainStmtList(ctx: SBParser.PlainStmtlistContext) =
        this.SafeChildren(ctx.stmt())
        |> Seq.collect (fun s ->
            s.Accept(this)
            |> Seq.choose (function
                | StmtNode stmt -> Some stmt
                | _ -> None))
        |> Seq.toList

    member private this.CollectExprList(ctx: SBParser.ExprListContext) =
        this.SafeChildren(ctx.expr()) |> Seq.map (fun e -> this.SafeAcceptExpr(e)) |> Seq.toList

    member private this.CollectParenthesizedArgs(ctx: SBParser.ParenthesizedlistContext) =
        this.SafeChildren(ctx.expr()) |> Seq.map (fun e -> this.SafeAcceptExpr(e)) |> Seq.toList

    member private this.CollectUnparenthesizedArgs(ctx: SBParser.UnparenthesizedlistContext) =
        this.SafeChildren(ctx.expr()) |> Seq.map (fun e -> this.SafeAcceptExpr(e)) |> Seq.toList

    /// A statement's arguments, with an omitted one wherever two separators meet (PROC a,,b):
    /// SuperBASIC passes it unset and Turbo an empty parameter (Ad_Hoc_Para_List, P 21542).
    member private this.CollectSegmentArgs(segments: SBParser.StmtSegmentContext list) =
        segments
        |> List.mapi (fun index segment ->
            let omittedBefore =
                index > 0
                && not (isNull (segment.separator()))
                && isNull (segments[index - 1].stmtArg())
            let omitted =
                if omittedBefore then [ ExprNode(mkIdentifier (posOfTree segment) "") ] else []
            omitted @ segment.Accept(this))
        |> List.concat

    member private this.CollectStmtArgs(ctx: SBParser.StmtTailContext) =
        ctx.stmtSegment()
        |> Seq.toList
        |> this.CollectSegmentArgs
        |> List.map (fun node -> singleExpr ctx [ node ])

    member private this.NormalizeImplicitChannelExpr(expr: Expr) =
        match expr with
        | Identifier(_, pos, value) -> mkStringLiteral pos ("\"" + value + "\"")
        | other -> other

    member private this.TryCollectImplicitChannelCall(tail: SBParser.StmtTailContext) =
        let segments = tail.stmtSegment() |> Seq.toList
        match segments with
        | first :: rest ->
            let isImplicitChannel =
                match first.separator(), first.stmtArg() with
                | separator, arg when not (isNull separator) && not (isNull arg) -> separator.GetText() = "\\"
                | _ -> false

            if not isImplicitChannel then
                None
            else
                let channelExpr =
                    first.stmtArg()
                    |> fun arg -> singleExpr arg (arg.Accept(this))
                    |> this.NormalizeImplicitChannelExpr

                let remainingArgs =
                    rest
                    |> this.CollectSegmentArgs
                    |> List.map (fun node -> singleExpr tail [ node ])

                Some(channelExpr, remainingArgs)
        | [] -> None

    member private this.CollectPrintCallsFromStmtTail(pos: SourcePosition, name: string, mkStmt: Expr list -> Stmt, tail: SBParser.StmtTailContext) =
        let mutable groups : Expr list list = []
        let mutable currentGroup : Expr list = []
        let mutable endedWithExplicitBreak = false
        let mutable suppressTrailingNewline = false

        let mutable previousWasTo = false

        let segments = tail.stmtSegment()
        for index in 0 .. segments.Length - 1 do
            let segment = segments[index]
            let isLast = index = segments.Length - 1
            let separatorText =
                match segment.separator() with
                | null -> None
                // Turbo's parser takes the separator after 'TO n' as part of the TO (P 21798-21802),
                // so it neither moves to a zone, prints a space nor starts a new line.
                | _ when previousWasTo -> Some ";"
                | separator -> Some(separator.GetText())

            // 'TO n' is an argument in the grammar but a separator in SuperBASIC. It arrives alone
            // (toChanArg) or after the item before it, which the grammar reads as 'item TO n'.
            let argOpt, toColumn =
                match segment.stmtArg() with
                | null -> None, None
                | arg when not (isNull (arg.toChanArg())) ->
                    None, Some(this.SafeAcceptExpr(arg.toChanArg().expr()))
                | arg when not (isNull (arg.rangedExpr())) && arg.rangedExpr().expr().Length = 2 ->
                    let item = arg.rangedExpr().expr(0)
                    Some(singleExpr item (item.Accept(this))), Some(this.SafeAcceptExpr(arg.rangedExpr().expr(1)))
                | arg -> Some(singleExpr arg (arg.Accept(this))), None

            match separatorText with
            | Some "\\" ->
                groups <- groups @ [ currentGroup ]
                currentGroup <- []
                endedWithExplicitBreak <- true
                suppressTrailingNewline <- false
            | Some "," ->
                currentGroup <- currentGroup @ [ mkStringLiteral pos printCommaSentinel ]
            | Some "!" when not (isLast && argOpt.IsNone && toColumn.IsNone) ->
                // A trailing '!' only keeps the cursor on the line (Parse PRINT, P 21810).
                currentGroup <- currentGroup @ [ mkStringLiteral pos printPlingSentinel ]
            | _ -> ()

            match argOpt with
            | Some arg ->
                currentGroup <- currentGroup @ [ arg ]
                endedWithExplicitBreak <- false
                suppressTrailingNewline <- false
            | None -> ()

            match toColumn with
            | Some column ->
                currentGroup <- currentGroup @ [ mkStringLiteral pos printToSentinel; column ]
                endedWithExplicitBreak <- false
                suppressTrailingNewline <- isLast
            | None -> ()

            match separatorText, argOpt, toColumn with
            | Some (";" | "," | "!"), None, None ->
                suppressTrailingNewline <- true
            | _ -> ()

            previousWasTo <- toColumn.IsSome

        if (not endedWithExplicitBreak) || not (List.isEmpty currentGroup) then
            groups <- groups @ [ currentGroup ]

        if suppressTrailingNewline && not groups.IsEmpty then
            let lastIndex = groups.Length - 1
            groups <-
                groups
                |> List.mapi (fun index group ->
                    if index = lastIndex then
                        group @ [ mkStringLiteral pos printContinueSentinel ]
                    else
                        group)

        groups
        |> List.map (fun args -> StmtNode(mkStmt args))

    member private this.PostfixArgItemToExpr(ctx: SBParser.PostfixArgItemContext) =
        let p = posOfTree ctx
        let exprs = this.SafeChildren(ctx.expr()) |> Seq.toList

        // Postfix arguments are one of the denser syntax areas in the grammar:
        // they can represent ordinary expressions, channel arguments, or slice/range
        // forms using TO. This helper normalizes those cases into the smaller Expr DU.
        if not (isNull (ctx.chanArg())) then
            this.SafeAcceptExpr(ctx.chanArg())
        else
            match exprs with
            | [ a ] when not (isNull (ctx.To())) ->
                let left = this.SafeAcceptExpr(a)
                mkSliceRange p left (mkIdentifier p "")
            | [ a ] -> this.SafeAcceptExpr(a)
            | [ a; b ] ->
                let left = this.SafeAcceptExpr(a)
                let right = this.SafeAcceptExpr(b)
                mkSliceRange p left right
            | _ ->
                mkIdentifier p ""

    member private this.FoldLeft(pos: SourcePosition, first: Expr, ops: string list, rest: Expr list) =
        // Expression rules are often parsed as "first operand plus repeated
        // operator/rest pairs". Folding them left here keeps precedence handling in
        // the grammar and keeps the AST in the expected binary-expression form.
        let pairCount = min ops.Length rest.Length
        let zipped = List.zip (ops |> List.truncate pairCount) (rest |> List.truncate pairCount)
        (first, zipped)
        ||> List.fold (fun acc (op, rhs) -> mkBinaryExpr pos op acc rhs)

    member private this.OperatorTexts(ctx: IRuleNode, allowed: Set<string>) =
        [ for i in 0 .. ctx.ChildCount - 1 do
              let t = ctx.GetChild(i).GetText()
              if allowed.Contains t then
                  yield t ]

    member private this.BlockFromStmtOrLine(stmtList: SBParser.StmtlistContext, lineContexts: seq<SBParser.LineContext>) =
        // Some constructs can legally contain either a single-line statement list or
        // a multi-line nested body. The AST preserves that distinction explicitly as
        // StatementBlock vs LineBlock.
        if isNull stmtList then
            LineBlock(this.VisitLineList(lineContexts))
        else
            StatementBlock(this.CollectStmtList(stmtList))

    member private this.ForBodyLineToLine(ctx: SBParser.ForBodyLineContext) =
        let lineNo =
            match ctx.lineNumber() with
            | null -> None
            | ln -> parseLineNumber (ln.GetText())
        this.WithBasicLineNo lineNo (fun () ->
            let p = posOfTree ctx
            let statements =
                match ctx.plainStmtlist() with
                | null -> []
                | stmtList -> this.CollectPlainStmtList(stmtList)

            Line(p, lineNo, statements))

    member private this.AppendIfTerminatorLine (terminator: SBParser.IfTerminatorContext) (lines: Line list) =
        match terminator with
        | null -> lines
        | _ ->
            match terminator.plainStmtlist() with
            | null -> lines
            | stmtList ->
                let lineNo =
                    match terminator.lineNumber() with
                    | null -> None
                    | ln -> parseLineNumber (ln.GetText())

                lines @ [ Line(posOfTree terminator, lineNo, this.CollectPlainStmtList(stmtList)) ]

    override this.VisitProgram(ctx: SBParser.ProgramContext) =
        let p = posOfTree ctx
        let children = this.VisitLineList(ctx.line())
        single (RootNode(Program(p, children)))

    override this.VisitLine(ctx: SBParser.LineContext) =
        let lineNo =
            match ctx.lineNumber() with
            | null -> None
            | ln -> parseLineNumber (ln.GetText())
        this.WithBasicLineNo lineNo (fun () ->
            let p = posOfTree ctx
            let children =
                if isNull (ctx.stmtlist()) then [] else this.CollectStmtList(ctx.stmtlist())

            single (LineNode(Line(p, lineNo, children))))

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
        // Procedure and function definitions lower almost identically: extract the
        // declared name, formal parameter list, and already-normalized body lines.
        let p = posOfTree ctx
        let ids = ctx.ID() |> Seq.toList
        let name =
            match ids with
            | id :: _ -> id.GetText()
            | [] -> ""
        let closingName =
            match ids with
            | _ :: closing :: [] -> Some(closing.GetText())
            | _ -> None
        let closingLineNumber =
            match ctx.lineNumber() with
            | null -> None
            | lineNumber -> lineNumber.GetText() |> int |> Some

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
        single (StmtNode(ProcedureDef(p, name, parms, body, closingName, closingLineNumber)))

    override this.VisitFunctionDef(ctx: SBParser.FunctionDefContext) =
        let p = posOfTree ctx
        let ids = ctx.ID() |> Seq.toList
        let name =
            match ids with
            | id :: _ -> id.GetText()
            | [] -> ""
        let closingName =
            match ids with
            | _ :: closing :: [] -> Some(closing.GetText())
            | _ -> None
        let closingLineNumber =
            match ctx.lineNumber() with
            | null -> None
            | lineNumber -> lineNumber.GetText() |> int |> Some

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
        single (StmtNode(FunctionDef(p, name, parms, body, closingName, closingLineNumber)))

    override this.VisitDim(ctx: SBParser.DimContext) =
        // Declaration-style statements are normalized into compact tuples rather
        // than retaining parser-specific list/item wrapper nodes.
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
        let kw = ctx.GetChild(0).GetText()
        let suffix =
            if String.IsNullOrEmpty kw then ""
            elif kw.EndsWith("%") || kw.EndsWith("$") then string kw.[kw.Length - 1]
            else string (Char.ToUpperInvariant kw.[kw.Length - 1])
        let names =
            ctx.unparenthesizedlist().expr()
            |> Seq.map (fun e -> singleExpr e (e.Accept(this)))
            |> Seq.choose (function Identifier(_, _, name) -> Some name | _ -> None)
            |> Seq.toList
        single (StmtNode(ImplicitStmt(p, suffix, names)))

    override this.VisitManifestDecl(ctx: SBParser.ManifestDeclContext) =
        ctx.manifestStmt().Accept(this)

    override this.VisitManifestStmt(ctx: SBParser.ManifestStmtContext) =
        let p = posOfTree ctx
        let items =
            ctx.manifestItem()
            |> Seq.map (fun item ->
                let name = item.ID().GetText()
                let value = this.SafeAcceptExpr(item.expr())
                name, value)
            |> Seq.toList
        single (StmtNode(ManifestStmt(p, items)))

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
        let target =
            match args with
            | expr :: _ -> expr
            | [] -> mkIdentifier p ""
        single (StmtNode(GotoStmt(p, target)))

    override this.VisitGosubStatement(ctx: SBParser.GosubStatementContext) =
        let p = posOfTree ctx
        let gosubCtx = ctx.gosubStmt()
        let args = gosubCtx.Accept(this) |> List.map (fun node -> singleExpr gosubCtx [ node ])
        let target =
            match args with
            | expr :: _ -> expr
            | [] -> mkIdentifier p ""
        single (StmtNode(GosubStmt(p, target)))

    override this.VisitOnGotoStatement(ctx: SBParser.OnGotoStatementContext) =
        let p = posOfTree ctx
        let onGotoCtx = ctx.onGotoStmt()
        let args = onGotoCtx.Accept(this) |> List.map (fun node -> singleExpr onGotoCtx [ node ])
        let selector, targets =
            match args with
            | first :: rest -> first, rest
            | [] -> mkIdentifier p "", []
        single (StmtNode(OnGotoStmt(p, selector, targets)))

    override this.VisitOnGosubStatement(ctx: SBParser.OnGosubStatementContext) =
        let p = posOfTree ctx
        let onGosubCtx = ctx.onGosubStmt()
        let args = onGosubCtx.Accept(this) |> List.map (fun node -> singleExpr onGosubCtx [ node ])
        let selector, targets =
            match args with
            | first :: rest -> first, rest
            | [] -> mkIdentifier p "", []
        single (StmtNode(OnGosubStmt(p, selector, targets)))

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
        if this.IsIgnoredDirectiveName name then
            single (StmtNode(Remark(p, ctx.GetText())))
        else
            let channel = singleExpr (ctx.chanArg()) (ctx.chanArg().Accept(this))
            match ctx.stmtTail() with
            | null -> single (StmtNode(ChannelProcedureCall(p, name, channel, [])))
            | tail when String.Equals(name, "PRINT", StringComparison.OrdinalIgnoreCase) ->
                this.CollectPrintCallsFromStmtTail(p, name, (fun args -> ChannelProcedureCall(p, name, channel, args)), tail)
            | tail ->
                let args = this.CollectStmtArgs(tail) |> this.NormalizeOpenArgs name
                single (StmtNode(ChannelProcedureCall(p, name, channel, args)))

    override this.VisitProcedureCallStmt(ctx: SBParser.ProcedureCallStmtContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()
        if this.IsIgnoredDirectiveName name then
            single (StmtNode(Remark(p, ctx.GetText())))
        else
            match ctx.stmtArglist() with
            | null -> single (StmtNode(ProcedureCall(p, name, [])))
            | sa when not (isNull (sa.stmtTail())) ->
                match this.TryCollectImplicitChannelCall(sa.stmtTail()) with
                | Some(channelExpr, args) ->
                    single (StmtNode(ImplicitChannelProcedureCall(p, name, channelExpr, this.NormalizeOpenArgs name args)))
                | None when String.Equals(name, "PRINT", StringComparison.OrdinalIgnoreCase) ->
                    this.CollectPrintCallsFromStmtTail(p, name, (fun args -> ProcedureCall(p, name, args)), sa.stmtTail())
                | None ->
                    let args =
                        sa.Accept(this)
                        |> List.map (fun node -> singleExpr sa [ node ])
                        |> this.NormalizeOpenArgs name
                    single (StmtNode(ProcedureCall(p, name, args)))
            | sa ->
                let args =
                    sa.Accept(this)
                    |> List.map (fun node -> singleExpr sa [ node ])
                    |> this.NormalizeOpenArgs name
                single (StmtNode(ProcedureCall(p, name, args)))

    override this.VisitLvalue(ctx: SBParser.LvalueContext) =
        ctx.postfixName().Accept(this)

    override this.VisitLvalueList(ctx: SBParser.LvalueListContext) =
        ctx.lvalue() |> Seq.map (fun lv -> singleExpr lv (lv.Accept(this)) |> ExprNode) |> Seq.toList

    override this.VisitStmtArglist(ctx: SBParser.StmtArglistContext) =
        if not (isNull (ctx.parenthesizedlist())) then
            let args = this.CollectParenthesizedArgs(ctx.parenthesizedlist())
            // A sole parenthesized procedure actual is an expression/value,
            // even when the grammar consumes its parentheses as an arglist.
            match args with
            | [arg] -> [ExprNode(mkGroupedExpr (posOfTree ctx) arg)]
            | _ -> args |> List.map ExprNode
        else
            ctx.stmtTail().Accept(this)

    override this.VisitStmtTail(ctx: SBParser.StmtTailContext) =
        ctx.stmtSegment() |> Seq.toList |> this.CollectSegmentArgs

    override this.VisitStmtSegment(ctx: SBParser.StmtSegmentContext) =
        match ctx.stmtArg() with
        | null -> []
        // A '#' on an argument after the first is kept (a leading one is the statement's
        // channel): Turbo marks it in the argument's descriptor (Ad_Hoc_Para_List, P 21514).
        | arg when not (isNull (arg.chanArg())) ->
            single (ExprNode(mkUnaryExpr (posOfTree arg) "#" (this.SafeAcceptExpr(arg.chanArg().expr()))))
        // 'TO x' with nothing before it, as in LINE TO x,y or ARC TO x,y,a, is an omitted argument
        // followed by TO: a range whose lower bound is omitted (Turbo's empty parameter, P 21542).
        | arg when not (isNull (arg.toChanArg())) ->
            let p = posOfTree arg
            single (ExprNode(mkSliceRange p (mkIdentifier p "") (this.SafeAcceptExpr(arg.toChanArg().expr()))))
        | arg -> arg.Accept(this)

    override this.VisitStmtArg(ctx: SBParser.StmtArgContext) =
        if not (isNull (ctx.chanArg())) then ctx.chanArg().Accept(this)
        elif not (isNull (ctx.toChanArg())) then ctx.toChanArg().Accept(this)
        else ctx.rangedExpr().Accept(this)

    override this.VisitToChanArg(ctx: SBParser.ToChanArgContext) =
        ctx.expr().Accept(this)

    override this.VisitRangedExpr(ctx: SBParser.RangedExprContext) =
        let p = posOfTree ctx
        let exprs = ctx.expr() |> Seq.toList

        match exprs with
        | [ value ] ->
            value.Accept(this)
        | [ startValue; endValue ] ->
            let left = singleExpr startValue (startValue.Accept(this))
            let right = singleExpr endValue (endValue.Accept(this))
            single (ExprNode(mkSliceRange p left right))
        | _ ->
            single (ExprNode(mkIdentifier p ""))

    override this.VisitChanArg(ctx: SBParser.ChanArgContext) =
        ctx.expr().Accept(this)

    override this.VisitArg(ctx: SBParser.ArgContext) =
        if not (isNull (ctx.chanArg())) then ctx.chanArg().Accept(this)
        else ctx.expr().Accept(this)

    override this.VisitPostfixName(ctx: SBParser.PostfixNameContext) =
        let p = posOfTree ctx
        let name = ctx.ID().GetText()
        // Both bracket groups of c$(i)(a TO b) become one argument list, the SuperBASIC c$(i, a TO b).
        let children =
            ctx.postfixArg()
            |> Seq.collect (fun pa -> pa.Accept(this) |> List.map (fun node -> singleExpr pa [ node ]))
            |> List.ofSeq
        let expr =
            match children with
            | [] -> mkIdentifier p name
            | _ -> mkPostfixName p name (Some children)
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
            ctx.ID()
            |> Seq.tryHead
            |> Option.map _.GetText()
            |> Option.defaultValue ""

        let prefixExprs =
            ctx.forPrefixItem()
            |> Seq.map (fun prefixItem ->
                let exprCtx = prefixItem.expr()
                singleExpr exprCtx (exprCtx.Accept(this)))
            |> Seq.toList

        let suffixExprs =
            ctx.forSuffixItem()
            |> Seq.map (fun suffixItem ->
                let exprCtx = suffixItem.expr()
                singleExpr exprCtx (exprCtx.Accept(this)))
            |> Seq.toList

        let startExpr =
            match ctx.forStartExpr() with
            | null -> mkIdentifier p ""
            | exprCtx ->
                let inner = exprCtx.expr()
                singleExpr inner (inner.Accept(this))
        let endExpr =
            match ctx.forEndExpr() with
            | null -> mkIdentifier p ""
            | exprCtx ->
                let inner = exprCtx.expr()
                singleExpr inner (inner.Accept(this))
        let stepExpr =
            match ctx.forStepExpr() with
            | null -> None
            | exprCtx ->
                let inner = exprCtx.expr()
                Some (singleExpr inner (inner.Accept(this)))

        let loopStmt, trailingNodes =
            match ctx.forBody() with
            | null ->
                let body =
                    match ctx.stmtlist() with
                    | null -> StatementBlock([])
                    | stmtList -> StatementBlock(this.CollectStmtList(stmtList))

                let closingName =
                    match body with
                    | _ -> None

                StmtNode(ForStmt(p, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, body, closingName)), []
            | bodyCtx ->
                let leadingLines =
                    bodyCtx.forBodyLine()
                    |> Seq.map this.ForBodyLineToLine
                    |> Seq.toList

                // Only a tree recovered from a syntax error has a long-form body with no terminator.
                let terminator =
                    match bodyCtx.forTerminator() with
                    | null ->
                        let line = p.BasicLineNo |> Option.map string |> Option.defaultValue $"editor line {p.EditorLineNo}"
                        failwith $"FOR {name} at line {line} has no END FOR or NEXT."
                    | terminator -> terminator
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
                let closingName =
                    match terminator.ID() with
                    | null -> None
                    | id -> Some(id.GetText())

                StmtNode(ForStmt(p, name, prefixExprs, startExpr, endExpr, suffixExprs, stepExpr, LineBlock(leadingLines @ closingLine), closingName)), trailingNodes

        loopStmt :: trailingNodes

    override this.VisitRepeatStmt(ctx: SBParser.RepeatStmtContext) =
        let p = posOfTree ctx
        let ids = ctx.ID() |> Seq.toList
        let name =
            match ids with
            | id :: _ -> id.GetText()
            | [] -> ""
        let closingName =
            match ids with
            | _ :: closing :: [] -> Some(closing.GetText())
            | _ -> None
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
        single (StmtNode(RepeatStmt(p, name, body, closingName)))

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
                let terminator = ctx.ifTerminator()
                let thenLines =
                    match ctx.ifBlock() with
                    | null -> []
                    | ib ->
                        ib.ifBodyLine()
                        |> Seq.choose (fun bodyLine ->
                            match bodyLine.line() with
                            | null -> None
                            | line -> Some (singleLine line (line.Accept(this))))
                        |> Seq.toList

                match ctx.elseBlock() with
                | null ->
                    LineBlock(this.AppendIfTerminatorLine terminator thenLines), None
                | eb ->
                    let elseLines =
                        eb.elseBodyLine()
                        |> Seq.choose (fun bodyLine ->
                            match bodyLine.line() with
                            | null -> None
                            | line -> Some (singleLine line (line.Accept(this))))
                        |> Seq.toList

                    LineBlock(thenLines), Some (LineBlock(this.AppendIfTerminatorLine terminator elseLines))

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
        selectSelectorStack <- selector :: selectSelectorStack

        try
            // Compact arms collect their body lines in the grammar. Explicit
            // ON arms expose subsequent lines as select items; attach those to
            // every match from the preceding header (including comma lists).
            let groups = ResizeArray<SelectClause list * ResizeArray<Line>>()
            for item in this.SafeChildren(ctx.selectBodyItem()) do
                let nodes = item.Accept(this)
                let clauses = nodes |> List.choose (function ClauseNode clause -> Some clause | _ -> None)
                if not clauses.IsEmpty then groups.Add(clauses, ResizeArray())
                elif groups.Count > 0 then
                    let _, lines = groups[groups.Count - 1]
                    for node in nodes do
                        match node with
                        | LineNode line -> lines.Add line
                        | _ -> ()
            let clauses =
                groups |> Seq.collect (fun (clauses, extraLines) ->
                    clauses |> List.map (fun (SelectClause(cp, cs, range, body) as clause) ->
                        if extraLines.Count = 0 then clause
                        else
                            let initial =
                                match body with
                                | Some(LineBlock lines) -> lines
                                | Some(StatementBlock stmts) -> [Line(cp, cp.BasicLineNo, stmts)]
                                | None -> []
                            SelectClause(cp, cs, range, Some(LineBlock(initial @ List.ofSeq extraLines)))))
                |> Seq.toList
            single (StmtNode(SelectStmt(p, selector, clauses)))
        finally
            match selectSelectorStack with
            | _ :: tail -> selectSelectorStack <- tail
            | [] -> ()

    override this.VisitSelectBodyItem(ctx: SBParser.SelectBodyItemContext) =
        ctx.selectItem().Accept(this)

    override this.VisitSelectItem(ctx: SBParser.SelectItemContext) =
        if not (isNull (ctx.onClause())) then ctx.onClause().Accept(this)
        elif not (isNull (ctx.compactOnClause())) then ctx.compactOnClause().Accept(this)
        elif not (isNull (ctx.line())) then ctx.line().Accept(this)
        else []

    override this.VisitOnClause(ctx: SBParser.OnClauseContext) =
        let p = posOfTree ctx
        let selectorExpr = this.SafeAcceptExpr(ctx.expr())
        let rangeAsts =
            this.SafeChildren(ctx.selectRangeList().rangeexpr())
            |> Seq.map this.SafeAcceptExpr
            |> Seq.toList
        let body =
            match ctx.stmtlist() with
            | null -> None
            | s -> Some (StatementBlock(this.CollectStmtList(s)))
        this.BuildSelectClauses p selectorExpr rangeAsts body

    override this.VisitCompactOnClause(ctx: SBParser.CompactOnClauseContext) =
        let p = posOfTree ctx
        let selectorExpr =
            match selectSelectorStack with
            | head :: _ -> head
            | [] -> failwith "Compact SELect clause encountered outside of SELect ON context."

        let rangeAsts =
            this.SafeChildren(ctx.selectRangeList().rangeexpr())
            |> Seq.map this.SafeAcceptExpr
            |> Seq.toList

        let bodyLines =
            ctx.compactClauseBodyLine()
            |> Seq.choose (fun bodyLine ->
                match bodyLine.line() with
                | null -> None
                | line -> Some (singleLine line (line.Accept(this))))
            |> Seq.toList

        let body =
            match ctx.stmtlist(), bodyLines with
            | stmtList, [] when not (isNull stmtList) -> Some (StatementBlock(this.CollectStmtList(stmtList)))
            | null, [] -> None
            | stmtList, lines when not (isNull stmtList) ->
                Some (LineBlock([ Line(p, p.BasicLineNo, this.CollectStmtList(stmtList)) ] @ lines))
            | null, lines -> Some (LineBlock lines)

        this.BuildSelectClauses p selectorExpr rangeAsts body

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
        let ops =
            this.OperatorTexts(ctx, set [ "OR"; "||"; "XOR"; "^^" ])
            |> List.map (function | "^^" -> "XOR" | "||" -> "OR" | other -> other)
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
            single (ExprNode(mkUnaryExpr p "NOT" rhs))

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
        | [ lhs; rhs ] -> single (ExprNode(mkBinaryExpr p "INSTR" lhs rhs))
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
            single (ExprNode(mkUnaryExpr p op rhs))

    override this.VisitPrimary(ctx: SBParser.PrimaryContext) =
        let p = posOfTree ctx
        if not (isNull (ctx.Integer())) then
            single (ExprNode(mkNumberLiteral p (ctx.Integer().GetText())))
        elif not (isNull (ctx.HexInteger())) then
            single (ExprNode(mkNumberLiteral p (ctx.HexInteger().GetText())))
        elif not (isNull (ctx.Real())) then
            single (ExprNode(mkNumberLiteral p (ctx.Real().GetText())))
        elif not (isNull (ctx.String())) then
            single (ExprNode(mkStringLiteral p (ctx.String().GetText())))
        elif not (isNull (ctx.postfixName())) then
            ctx.postfixName().Accept(this)
        elif not (isNull (ctx.expr())) then
            single (ExprNode(mkGroupedExpr p (this.SafeAcceptExpr(ctx.expr()))))
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
            single (ExprNode(mkSliceRange p left right))
        | _ when not (isNull (ctx.Remainder())) ->
            single (ExprNode(mkIdentifier p "REMAINDER"))
        | _ -> []

    override this.VisitLineNumber(_ctx: SBParser.LineNumberContext) = []

    override this.VisitTerminal(node: ITerminalNode) =
        let sym = node.Symbol
        let p = posOfToken sym
        match SBGrammarApi.GetTokenSymbolicName(sym.Type) with
        | "INTEGER" -> single (ExprNode(mkNumberLiteral p sym.Text))
        | "HEXINTEGER" -> single (ExprNode(mkNumberLiteral p sym.Text))
        | "REAL" -> single (ExprNode(mkNumberLiteral p sym.Text))
        | "STRING" -> single (ExprNode(mkStringLiteral p sym.Text))
        | _ -> []

    override this.VisitErrorNode(node: IErrorNode) =
        let p =
            match node.Symbol with
            | null -> { BasicLineNo = ambientBasicLineNo; EditorLineNo = 0; Column = 0 }
            | s -> posOfToken s
        single (ExprNode(mkIdentifier p (node.GetText())))

    override this.VisitChildren(node: IRuleNode) =
        [ for i in 0 .. node.ChildCount - 1 do
              yield! node.GetChild(i).Accept(this) ]

let convertTreeToAst (parseTree: IParseTree) : Ast list =
    resetNodeIds()
    let visitor = ASTBuildingVisitor()
    [ singleRoot parseTree (parseTree.Accept(visitor)) ]

let convertTreeToExpr (parseTree: IParseTree) : Expr list =
    resetNodeIds()
    let visitor = ASTBuildingVisitor()
    parseTree.Accept(visitor)
    |> List.choose (function
        | ExprNode expr -> Some expr
        | _ -> None)
