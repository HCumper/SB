module HirTurboOptimizer

// Stage 2 of the QL optimization plan. This pass returns new HIR and never mutates
// storage, bindings, DATA or source anchors. There is deliberately no propagation,
// reassociation, loop motion, native selection or register allocation here.
open System
open HIR
open HirTurboIrBackend

type ArithmeticPolicy = CheckedQl

type Options = {
    Enabled: bool
    ArithmeticPolicy: ArithmeticPolicy
    /// Optional output prefix for .before.hir and .after.hir debug dumps.
    DumpPrefix: string option
}

let defaultOptions = { Enabled = true; ArithmeticPolicy = CheckedQl; DumpPrefix = None }

type private QlType = Integer | Real | Text | Unknown

let private fitsWord value = value >= -32768L && value <= 32767L
let private normalize (name: string) = name.ToUpperInvariant()

let private literalType = function
    | ConstInt n when fitsWord (int64 n) -> Integer
    | ConstInt _ | ConstFloat _ -> Real
    | ConstString _ -> Text

let private hirType = function
    | Literal(_, t, _) | ReadVar(_, t, _) | ReadArrayElem(_, _, t, _)
    | ReadStringChar(_, _, t, _) | DynamicReadVar(_, t, _)
    | DynamicReadArrayElem(_, _, t, _) | DynamicReadStringChar(_, _, t, _)
    | Unary(_, _, t, _) | Binary(_, _, _, t, _) | CallFunc(_, _, t, _) -> t
    | Missing _ -> HirType.Int

let private position = function
    | Literal(_, _, p) | ReadVar(_, _, p) | ReadArrayElem(_, _, _, p)
    | ReadStringChar(_, _, _, p) | DynamicReadVar(_, _, p)
    | DynamicReadArrayElem(_, _, _, p) | DynamicReadStringChar(_, _, _, p)
    | Unary(_, _, _, p) | Binary(_, _, _, _, p) | CallFunc(_, _, _, p) | Missing p -> p

let private truth = function
    | ConstInt n when fitsWord (int64 n) -> Some(n <> 0)
    | ConstInt n ->
        match encodeQdosFloat (float n) with
        | Ok(_, high, low) -> Some(high <> 0 || low <> 0)
        | Error _ -> None
    | ConstFloat n ->
        // Inspect the encoded QDOS value, never IEEE truth or host floating arithmetic.
        match encodeQdosFloat n with
        | Ok(_, high, low) -> Some(high <> 0 || low <> 0)
        | Error _ -> None
    | _ -> None

let private validText (text: string) =
    match QlCharacterSet.tryEncode text with
    | Ok bytes -> bytes.Length <= 32767
    | Error _ -> false

let private intPair = function
    | Literal(ConstInt a, _, _), Literal(ConstInt b, _, _) when fitsWord (int64 a) && fitsWord (int64 b) -> Some(int64 a, int64 b)
    | _ -> None

let private foldIntegers op a b =
    let result =
        match op with
        | Add -> Some(a + b)
        | Subtract -> Some(a - b)
        | Multiply -> Some(a * b)
        | IntegerDivide | Modulo when b <> 0L && not (a = -32768L && b = -1L) ->
            // Turbo DIV floors; MOD has the divisor's sign. Its DIVS overflow
            // check also applies to MOD, even when the remainder would fit.
            let quotient, remainder = a / b, a % b
            let quotient = if remainder <> 0L && ((remainder < 0L) <> (b < 0L)) then quotient - 1L else quotient
            Some(if op = IntegerDivide then quotient else a - quotient * b)
        | _ -> None
    result |> Option.filter fitsWord |> Option.map (int >> ConstInt)

let private foldComparison op a b =
    let answer =
        match op with
        | Equal -> Some(a = b) | NotEqual -> Some(a <> b)
        | LessThan -> Some(a < b) | LessThanOrEqual -> Some(a <= b)
        | GreaterThan -> Some(a > b) | GreaterThanOrEqual -> Some(a >= b)
        | _ -> None
    // Turbo comparisons return a QDOS float, including comparisons of integers.
    answer |> Option.map (fun yes -> ConstFloat(if yes then 1.0 else 0.0))

let private context (target: TurboIrOptions) (program: HirProgram) =
    let storages =
        program.Globals @ (program.Routines |> List.collect (fun r -> (r.Parameters |> List.map _.Storage) @ r.Locals))
        |> List.map (fun storage -> storage.Symbol, storage) |> Map.ofList
    let nameType name =
        let name = normalize name
        if name.EndsWith "$" then Text
        elif name.EndsWith "%" || target.ImplicitIntegers.Contains name then Integer
        elif target.ImplicitReals.Contains name then Real
        elif target.ImplicitStrings.Contains name then Text
        else Real
    let manifest symbol =
        match storages.TryFind symbol with
        | Some storage when storage.Class = GlobalStorage -> target.ManifestConstants.TryFind(normalize storage.Name)
        | _ -> None
    // This matches the emitter's constant/type boundary, including its unary-literal
    // handling. It prevents a child fold from silently changing a parent's arithmetic type.
    let rec constant = function
        | Literal(value, _, _) -> Some value
        | ReadVar(symbol, _, _) -> manifest symbol
        | Unary(Identity, inner, _, _) -> constant inner
        | Unary(Negate, inner, _, _) ->
            match constant inner with
            | Some(ConstInt n) when n <> Int32.MinValue -> Some(ConstInt(-n))
            | Some(ConstFloat n) -> Some(ConstFloat(-n))
            | _ -> None
        | _ -> None
    let symbolType symbol =
        match storages.TryFind symbol with
        | Some storage -> nameType storage.Name
        | None -> Unknown
    let rec expressionType expr =
        match constant expr with
        | Some value -> literalType value
        | None ->
            match expr with
            | ReadVar(s, _, _) | ReadArrayElem(s, _, _, _) -> symbolType s
            | DynamicReadVar(n, _, _) | DynamicReadArrayElem(n, _, _, _) -> nameType n
            | ReadStringChar _ | DynamicReadStringChar _ -> Text
            | Unary(Identity, x, _, _) | Unary(Hash, x, _, _) -> expressionType x
            | Unary(Negate, x, _, _) | Unary(BitwiseNot, x, _, _) -> if expressionType x = Integer then Integer else Real
            | Unary(IntegerNot, _, _, _) -> Integer
            | Binary(op, a, b, _, _) ->
                match op with
                | Add | Subtract | Multiply -> if expressionType a = Integer && expressionType b = Integer then Integer else Real
                | IntegerDivide | Modulo | Instr | IntegerAnd | IntegerOr | IntegerXor -> Integer
                | Concat -> Text
                | _ -> Real
            | _ -> Unknown
    manifest, expressionType

let private rewrite (target: TurboIrOptions) (program: HirProgram) =
    let manifest, expressionType = context target program
    let literal original value = Literal(value, hirType original, position original)
    let rec expression original =
        let rewritten =
            match original with
            | ReadVar(symbol, _, _) -> manifest symbol |> Option.map (literal original) |> Option.defaultValue original
            | ReadArrayElem(s, indexes, t, p) -> ReadArrayElem(s, List.map expression indexes, t, p)
            | ReadStringChar(s, index, t, p) -> ReadStringChar(s, expression index, t, p)
            | DynamicReadArrayElem(n, indexes, t, p) -> DynamicReadArrayElem(n, List.map expression indexes, t, p)
            | DynamicReadStringChar(n, index, t, p) -> DynamicReadStringChar(n, expression index, t, p)
            | CallFunc(s, args, t, p) -> CallFunc(s, List.map argument args, t, p)
            | Unary(op, inner, t, p) ->
                let inner = expression inner
                let fallback = Unary(op, inner, t, p)
                match op, inner with
                | Identity, _ -> inner
                | Negate, Literal(ConstInt n, _, _) when fitsWord (int64 n) && fitsWord (-int64 n) -> literal original (ConstInt(-n))
                | BitwiseNot, Literal(value, _, _) ->
                    match truth value with
                    | Some yes ->
                        let value = if yes then 0 else 1
                        literal original (if expressionType original = Integer then ConstInt value else ConstFloat(float value))
                    | _ -> fallback
                | _ -> fallback
            | Binary(op, lhs, rhs, t, p) ->
                let lhs, rhs = expression lhs, expression rhs
                let fallback = Binary(op, lhs, rhs, t, p)
                let folded =
                    match intPair(lhs, rhs) with
                    | Some(a, b) -> foldIntegers op a b |> Option.orElseWith (fun () -> foldComparison op a b)
                    | None -> None
                match folded with
                | Some value -> literal original value
                | None ->
                    match op, lhs, rhs with
                    | Concat, Literal(ConstString a, _, _), Literal(ConstString b, _, _)
                        when a.Length + b.Length <= 32767 && validText a && validText b -> literal original (ConstString(a + b))
                    | (BitwiseAnd | BitwiseOr | BitwiseXor), Literal(a, _, _), Literal(b, _, _) ->
                        match truth a, truth b with
                        | Some a, Some b ->
                            let yes = if op = BitwiseAnd then a && b elif op = BitwiseOr then a || b else a <> b
                            literal original (ConstFloat(if yes then 1.0 else 0.0))
                        | _ -> fallback
                    | (Add | Subtract), _, Literal(ConstInt 0, _, _) when expressionType lhs = Integer -> lhs
                    | Add, Literal(ConstInt 0, _, _), _ when expressionType rhs = Integer -> rhs
                    | (Multiply | IntegerDivide), _, Literal(ConstInt 1, _, _) when expressionType lhs = Integer -> lhs
                    | Multiply, Literal(ConstInt 1, _, _), _ when expressionType rhs = Integer -> rhs
                    | _ -> fallback
            | _ -> original
        // A literal can expose the emitter's unary constant path. In particular, folding
        // -(-32767-1)'s child must not turn checked NEG.W overflow into float +32768.
        if expressionType original <> expressionType rewritten || hirType original <> hirType rewritten then original else rewritten
    and argument = function
        | ValueArg value -> ValueArg(expression value)
        | RefArg place -> RefArg(targetExpression place)
    and targetExpression = function
        | WriteArrayElem(s, indexes, t, p) -> WriteArrayElem(s, List.map expression indexes, t, p)
        | WriteStringChar(s, index, t, p) -> WriteStringChar(s, expression index, t, p)
        | DynamicWriteArrayElem(n, indexes, t, p) -> DynamicWriteArrayElem(n, List.map expression indexes, t, p)
        | DynamicWriteStringChar(n, index, t, p) -> DynamicWriteStringChar(n, expression index, t, p)
        | other -> other
    let channel = Option.map (function ExplicitChannel e -> ExplicitChannel(expression e) | ImplicitChannel e -> ImplicitChannel(expression e))
    let validLiteral = function
        | Literal(ConstString text, _, _) -> validText text
        | Literal(value, _, _) -> (truth value).IsSome
        | _ -> false
    // Removing a branch must not change declaration discovery, resident-name linking,
    // directives, call bindings or any possible numbered entry. Restrict it to these
    // emission-safe statements until CFG/effect/alias analyses exist.
    let safeToDiscard = List.forall (function
        | Assign(WriteVar(_, _, _), (Literal(value, _, _) as e), _) ->
            validLiteral e && literalType value = Integer
        | BuiltInCall(Print, None, args, _) -> List.forall validLiteral args
        | _ -> false)
    let rec block statements = statements |> List.collect statement
    and statement = function
        | Assign(place, value, p) -> [ Assign(targetExpression place, expression value, p) ]
        | ProcCall(s, ch, args, p) -> [ ProcCall(s, channel ch, List.map argument args, p) ]
        | BuiltInCall(kind, ch, args, p) -> [ BuiltInCall(kind, channel ch, List.map expression args, p) ]
        | Input(ch, prompts, places, p) -> [ Input(channel ch, List.map expression prompts, List.map targetExpression places, p) ]
        | If(condition, yes, no, p) ->
            let condition, yes, no = expression condition, block yes, Option.map block no
            match condition with
            | Literal(value, _, _) when safeToDiscard yes && (no |> Option.forall safeToDiscard) ->
                match truth value with
                | Some true -> yes
                | Some false -> Option.defaultValue [] no
                | None -> [ If(condition, yes, no, p) ]
            | _ -> [ If(condition, yes, no, p) ]
        | For(id, s, first, last, step, body, p) -> [ For(id, s, expression first, expression last, expression step, block body, p) ]
        | ForSequence(id, s, prefix, first, last, suffix, step, body, p) ->
            [ ForSequence(id, s, List.map expression prefix, expression first, expression last, List.map expression suffix, expression step, block body, p) ]
        | Repeat(id, name, body, p) -> [ Repeat(id, name, block body, p) ]
        | WhenError(body, p) -> [ WhenError(block body, p) ]
        | Return(value, p) -> [ Return(Option.map expression value, p) ]
        // Preserve DATA/RESTORE, declaration bounds, branch target spelling and
        // numbered anchors exactly, including computed/unsupported target forms.
        | other -> [ other ]
    { program with Main = block program.Main; Routines = program.Routines |> List.map (fun r -> { r with Body = block r.Body }) }

/// Verifies the contracts that this conservative pass is allowed to change around.
let validate (before: HirProgram) (after: HirProgram) =
    let problems = ResizeArray<string>()
    if { after with Main = before.Main; Routines = before.Routines } <> before then
        problems.Add "Storage, symbol, DATA or RESTORE metadata changed."
    let targetKey = function
        | WriteVar(s, _, _) -> $"variable:{s}"
        | WriteArrayElem(s, _, _, _) -> $"array:{s}"
        | WriteStringChar(s, _, _, _) -> $"character:{s}"
        | DynamicWriteVar(n, _, _) -> "dynamic:" + n
        | DynamicWriteArrayElem(n, _, _, _) -> "dynamic-array:" + n
        | DynamicWriteStringChar(n, _, _, _) -> "dynamic-character:" + n
    let argumentKey = function ValueArg _ -> "value" | RefArg target -> "reference:" + targetKey target
    let rec exprContract = function
        | CallFunc(s, args, _, p) -> $"call:{s}:{p}:{List.map argumentKey args}" :: List.collect argContract args
        | Unary(_, x, _, _) -> exprContract x
        | Binary(_, a, b, _, _) -> exprContract a @ exprContract b
        | ReadArrayElem(_, indexes, _, _) | DynamicReadArrayElem(_, indexes, _, _) -> List.collect exprContract indexes
        | ReadStringChar(_, index, _, _) | DynamicReadStringChar(_, index, _, _) -> exprContract index
        | _ -> []
    and argContract = function ValueArg e -> exprContract e | RefArg t -> targetContract t
    and targetContract = function
        | WriteArrayElem(_, indexes, _, _) | DynamicWriteArrayElem(_, indexes, _, _) -> List.collect exprContract indexes
        | WriteStringChar(_, index, _, _) | DynamicWriteStringChar(_, index, _, _) -> exprContract index
        | _ -> []
    let channelContract = function Some(ExplicitChannel e) | Some(ImplicitChannel e) -> exprContract e | None -> []
    let rec contract body = body |> List.collect (function
        | LineNumber(n, p) -> [ $"line:{n}:{p}" ]
        | Goto(e, p) -> [ $"goto:{e}:{p}" ] | Gosub(e, p) -> [ $"gosub:{e}:{p}" ]
        | OnGoto(e, targets, p) -> [ $"on-goto:{e}:{targets}:{p}" ]
        | OnGosub(e, targets, p) -> [ $"on-gosub:{e}:{targets}:{p}" ]
        | Restore(e, p) -> [ $"restore:{e}:{p}" ] | Dim(s, bounds, p) -> [ $"dim:{s}:{bounds}:{p}" ]
        | Remark(text, p) -> [ $"remark:{text}:{p}" ]
        | For(id, s, a, b, step, body, p) -> $"for:{id}:{s}:{p}" :: (List.collect exprContract [a;b;step] @ contract body)
        | ForSequence(id, s, prefix, a, b, suffix, step, body, p) ->
            $"for-sequence:{id}:{s}:{p}" :: (List.collect exprContract (prefix @ [a;b] @ suffix @ [step]) @ contract body)
        | Repeat(id, name, body, p) -> $"repeat:{id}:{name}:{p}" :: contract body
        | WhenError(body, p) -> $"handler:{p}" :: contract body
        | If(e, yes, no, _) -> exprContract e @ contract yes @ (no |> Option.map contract |> Option.defaultValue [])
        | Assign(t, e, _) -> targetContract t @ exprContract e
        | ProcCall(s, ch, args, p) -> $"procedure:{s}:{p}:{List.map argumentKey args}" :: (channelContract ch @ List.collect argContract args)
        | BuiltInCall(_, ch, args, _) -> channelContract ch @ List.collect exprContract args
        | Input(ch, prompts, targets, _) -> channelContract ch @ List.collect exprContract prompts @ List.collect targetContract targets
        | Read(targets, _) -> List.collect targetContract targets
        | Return(Some e, _) -> exprContract e
        | _ -> [])
    let check name first second = if contract first <> contract second then problems.Add $"Control-flow anchors or argument bindings changed in {name}."
    check "main" before.Main after.Main
    if before.Routines.Length <> after.Routines.Length then problems.Add "Routine declarations changed."
    else
        for a, b in List.zip before.Routines after.Routines do
            if { b with Body = a.Body } <> a then problems.Add $"Routine metadata changed for {a.Name}."
            check a.Name a.Body b.Body
    List.ofSeq problems

/// An immutable, checked-QL HIR pass. Unsupported or uncertain arithmetic stays executable.
let optimize options target program =
    if not options.Enabled then Ok program
    else
        match options.ArithmeticPolicy with
        | CheckedQl ->
            let optimized = rewrite target program
            match validate program optimized with
            | [] -> Ok optimized
            | problems -> Error(problems |> List.map (fun text -> "Turbo HIR optimization validation failed: " + text))
