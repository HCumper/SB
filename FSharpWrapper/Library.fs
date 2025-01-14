module ReformatParseTree

open System
open Antlr4.Runtime
open Antlr4.Runtime.Tree
open Antlr4.Runtime.Misc

type FSParseTree = {
    ChildCount: int
    IsEmpty: bool
    Parent: FSParseTree option
    Payload: FSParseTree option
    RuleIndex: int
    SourceInterval: (int * int)
    Children: FSParseTree list
    Exception: RecognitionException option
    InvokingState: int
    ContextType: string
    Start: IToken option
    Stop: IToken option
    SourceText: string
}

/// Custom exception for parse tree errors
type ParseTreeException(message: string, innerException: Exception option) =
    inherit Exception(message, innerException |> Option.defaultValue null)

    /// Additional constructor for just a message
    new(message: string) = ParseTreeException(message, None)

/// Extract the original text for a given node in the parse tree
let getTextForNode (node: ParserRuleContext) (input: ICharStream) : string =
    match node.Start, node.Stop with
    | null, _ | _, null -> "" // Return an empty string if tokens are null
    | startToken, stopToken ->
        input.GetText(Interval(startToken.StartIndex, stopToken.StopIndex))

/// Visitor function to process a single node
let visitNode (node: IParseTree) (inputStream: ICharStream): FSParseTree =
    match node with
    | :? TerminalNodeImpl as terminal ->
        let token = terminal.Symbol
        {
            ChildCount = 0
            IsEmpty = false
            Parent = None
            Payload = None
            RuleIndex = -1
            SourceInterval = (token.StartIndex, token.StopIndex)
            Children = []
            Exception = None
            InvokingState = 0
            ContextType = terminal.GetType().Name
            Start = Some token
            Stop = Some token
            SourceText = token.Text
        }
    | :? ParserRuleContext as ruleContext ->
        let startToken = ruleContext.Start
        let stopToken = ruleContext.Stop
        {
            ChildCount = ruleContext.ChildCount
            IsEmpty = ruleContext.ChildCount = 0
            Parent = None      //TODO populate these 2
            Payload = None
            RuleIndex = ruleContext.RuleIndex
            SourceInterval =
                (if startToken = null || stopToken = null then (-1, -1)
                 else (startToken.StartIndex, stopToken.StopIndex))
            Children = []
            Exception = ruleContext.``exception`` |> Option.ofObj
            InvokingState = ruleContext.invokingState
            ContextType = ruleContext.GetType().Name
            Start = startToken |> Option.ofObj
            Stop = stopToken |> Option.ofObj
            SourceText = getTextForNode ruleContext inputStream
        }
    | _ -> raise (ParseTreeException($"Unsupported node type: {node.GetType().Name}"))

/// Tree traversal function that recursively walks through the parse tree
let rec traverseTree (node: IParseTree) inputStream : FSParseTree =
    let copiedNode = visitNode node inputStream

    // Process children if the node is a rule context
    match node with
    | :? ParserRuleContext as ctx ->
        let children =
            [ for i in 0 .. ctx.ChildCount - 1 do
                yield traverseTree (ctx.GetChild(i)) inputStream ]
        { copiedNode with Children = children; ChildCount = children.Length }
    | _ -> copiedNode // Terminal nodes don't have children

/// Entry function to start walking from the root of the parse tree
let processParseTree (tree: IParseTree) (inputStream: ICharStream) : FSParseTree =
    traverseTree tree inputStream
