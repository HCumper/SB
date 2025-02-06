module ParseTreeListener

//open FSharpx.Collections
open Utility
open System.Collections.Immutable

type ASTBuilderListener() =
    inherit SBBaseListener()

    /// Enqueue a new AST node, appending it to the current level
    let enqueueNode (queue: ImmutableQueue<ASTNode list>) (node: ASTNode) =
        let restQueue, children = queue.Dequeue()  // Extract first element (current children)
        restQueue.Enqueue(children @ [node])  // Put the modified list back

    /// Start collecting children for a new AST level
    let enqueueEmpty (queue: ImmutableQueue<ASTNode list>) =
        queue.Enqueue([])  // Add a new empty list

    /// Retrieve children for the current AST level
    let dequeueNodes (queue: ImmutableQueue<ASTNode list>) : ASTNode list * ImmutableQueue<ASTNode list> =
        let restQueue, children = queue.Dequeue()
        children, restQueue

    /// Mutable reference to track the AST state (since ANTLR calls are not functional)
    let astQueue = ImmutableQueue.Empty
    override _.EnterProgram(ctx: SBParser.ProgramContext) =
        astQueue <- enqueueEmpty astQueue  // Start collecting children for Program

    override _.ExitProgram(ctx: SBParser.ProgramContext) =
        let children, queue = dequeueNodes astQueue
        astQueue <- enqueueNode queue { TokenType = Program; Value = "Program"; Position = (0, 0); Children = children }
    //
    // override _.EnterFunctionDef(ctx: SBParser.FunctionDefContext) =
    //     astQueue <- enqueueEmpty astQueue  // Start collecting children for FunctionDef
    //
    // override _.ExitFunctionDef(ctx: SBParser.FunctionDefContext) =
    //     let children, queue = dequeueNodes astQueue
    //     let functionName = ctx.Identifier().GetText()
    //     astQueue <- enqueueNode queue { TokenType = FunctionDef; Value = functionName; Position = (0, 0); Children = children }
    //
    // override _.ExitAssignment(ctx: SBParser.AssignmentContext) =
    //     let queue = astQueue
    //     let varName = ctx.Identifier().GetText()
    //     let expr = { TokenType = Identifier; Value = varName; Position = (0, 0); Children = [] }
    //     astQueue <- enqueueNode queue { TokenType = Assignment; Value = varName; Position = (0, 0); Children = [expr] }
    //
    // override _.EnterBinaryExpr(ctx: SBParser.BinaryExprContext) =
    //     astQueue <- enqueueEmpty astQueue  // Start collecting children for BinaryExpr
    //
    // override _.ExitBinaryExpr(ctx: SBParser.BinaryExprContext) =
    //     let children, queue = dequeueNodes astQueue
    //     let op = ctx.Operator().GetText()
    //     astQueue <- enqueueNode queue { TokenType = BinaryExpression; Value = op; Position = (0, 0); Children = children }
    //
    // override _.ExitStmtList(ctx: SBParser.StmtListContext) =
    //     ()  // Do nothing: let parents absorb children naturally

    override _.ExitIdentifier(ctx: SBParser.IdentifierContext) =
        let name = ctx.GetText()
        astQueue <- enqueueNode astQueue { TokenType = Identifier; Value = name; Position = (0, 0); Children = [] }

    /// Function to retrieve the final AST after parsing
    member _.GetAST() : ASTNode =
        let children, _ = dequeueNodes astQueue  // Retrieve the final AST tree
        { TokenType = Program; Value = "Program"; Position = (0, 0); Children = children }
