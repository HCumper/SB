module SemanticAnalysisExpressions

open System.Globalization
open Types
open ProcessingTypes
open BuiltIns
open SyntaxAst
open SemanticAnalysisFacts
open SemanticAnalysisSymbols

// SemanticAnalysisExpressions contains the expression-heavy parts of semantic analysis.
//
// It is responsible for:
// - expression typing and result recording
// - SuperBASIC-style coercion rules for operators
// - constant folding and constant-valued symbol propagation
// - scalar/array usage checks
// - call-arity and selected built-in argument validation
// - writable-target inference and updates
//
// This is intentionally the most algorithmic semantic module: it concentrates the
// rules that combine symbol information with expression structure.
let posOfExpr expr =
    match expr with
    | PostfixName(pos, _, _)
    | SliceRange(pos, _, _)
    | BinaryExpr(pos, _, _, _)
    | UnaryExpr(pos, _, _)
    | NumberLiteral(pos, _)
    | StringLiteral(pos, _)
    | Identifier(pos, _) -> pos

let rec describeExpr expr =
    match expr with
    | Identifier(_, name)
    | PostfixName(_, name, None) -> name
    | PostfixName(_, name, Some _) -> $"{name}(...)"
    | NumberLiteral(_, value) -> value
    | StringLiteral(_, value) -> value
    | UnaryExpr(_, op, _) -> op
    | BinaryExpr(_, op, _, _) -> op
    | SliceRange _ -> "TO"

let isNumericType = function
    | SBType.Integer
    | SBType.Real -> true
    | _ -> false

type OperandTypeSet =
    | OnlyInteger
    | OnlyFloat
    | FloatOrInteger
    | OnlyString
    | StringOrInteger
    | StringOrFloat
    | StringFloatOrInteger

let private operandTypeSetFromOperator op =
    // Operators are classified by the set of operand kinds they permit. The later
    // coercion logic uses this classification together with the "most general"
    // operand type to reproduce SuperBASIC's conversion table behavior.
    match normalizeIdentifier op with
    | "&" -> Some OnlyString
    | "+" | "-" | "*" | "^" -> Some FloatOrInteger
    | "/" -> Some FloatOrInteger
    | "DIV" | "MOD" | "AND" | "OR" | "XOR" -> Some OnlyInteger
    | "=" | "==" | "<>" | "<" | "<=" | ">" | ">=" -> Some StringFloatOrInteger
    | "INSTR" -> Some OnlyString
    | _ -> None

let private resultTypeForOperator op finalOperandType =
    match normalizeIdentifier op with
    | "&" -> SBType.String
    | "/" -> SBType.Real
    | "DIV" | "MOD" | "AND" | "OR" | "XOR" -> SBType.Integer
    | "=" | "==" | "<>" | "<" | "<=" | ">" | ">=" -> SBType.Integer
    | "INSTR" -> SBType.Integer
    | _ -> finalOperandType

let private mostGeneralOperandType leftType rightType =
    if leftType = SBType.String || rightType = SBType.String then SBType.String
    elif leftType = SBType.Real || rightType = SBType.Real then SBType.Real
    elif leftType = SBType.Integer || rightType = SBType.Integer then SBType.Integer
    else SBType.Unknown

let private coerceOperandType mostGeneral operandSet =
    // This table encodes the final operand type chosen after applying the operator's
    // permitted operand set to the most general operand type seen at the call site.
    match mostGeneral, operandSet with
    | SBType.Integer, OnlyInteger -> Some SBType.Integer
    | SBType.Integer, OnlyFloat -> Some SBType.Real
    | SBType.Integer, FloatOrInteger -> Some SBType.Integer
    | SBType.Integer, OnlyString -> Some SBType.String
    | SBType.Integer, StringOrInteger -> Some SBType.Integer
    | SBType.Integer, StringOrFloat -> Some SBType.Real
    | SBType.Integer, StringFloatOrInteger -> Some SBType.Integer
    | SBType.Real, OnlyInteger -> Some SBType.Integer
    | SBType.Real, OnlyFloat -> Some SBType.Real
    | SBType.Real, FloatOrInteger -> Some SBType.Real
    | SBType.Real, OnlyString -> Some SBType.String
    | SBType.Real, StringOrInteger -> Some SBType.String
    | SBType.Real, StringOrFloat -> Some SBType.Real
    | SBType.Real, StringFloatOrInteger -> Some SBType.Real
    | SBType.String, OnlyInteger -> Some SBType.Integer
    | SBType.String, OnlyFloat -> Some SBType.Real
    | SBType.String, FloatOrInteger -> Some SBType.Real
    | SBType.String, OnlyString -> Some SBType.String
    | SBType.String, StringOrInteger -> Some SBType.String
    | SBType.String, StringOrFloat -> Some SBType.String
    | SBType.String, StringFloatOrInteger -> Some SBType.String
    | _ -> None

let mergeTypes existing inferred =
    match existing, inferred with
    | current, SBType.Unknown -> current
    | SBType.Unknown, current -> current
    | SBType.Integer, SBType.Real
    | SBType.Real, SBType.Integer -> SBType.Real
    | current, candidate when current = candidate -> current
    | current, _ -> current

let areTypesCompatible expected actual =
    expected = SBType.Unknown
    || actual = SBType.Unknown
    || expected = actual
    || (isNumericType expected && isNumericType actual)

let classifyNumericResultType leftType rightType =
    if leftType = SBType.Real || rightType = SBType.Real then SBType.Real
    elif isNumericType leftType && isNumericType rightType then SBType.Integer
    else SBType.Unknown

let builtInResultType name =
    // Only a small subset of built-ins have modeled return types today; everything
    // else stays Unknown until the semantic model is expanded further.
    match suffixDeclaredType name with
    | SBType.Unknown ->
        match normalizeIdentifier name with
        | "CODE"
        | "DATE"
        | "EOF"
        | "INSTR"
        | "KEYROW"
        | "LEN" -> SBType.Integer
        | "ABS"
        | "ACOS"
        | "ACOT"
        | "ADATE"
        | "ASIN"
        | "ATAN"
        | "COS"
        | "COT"
        | "DEG"
        | "EXP"
        | "INT"
        | "LN"
        | "LOG"
        | "LOG10"
        | "PI"
        | "RAD"
        | "RND"
        | "ROUND"
        | "SGN"
        | "SIN"
        | "SQRT"
        | "TAN"
        | "TIME"
        | "VAL" -> SBType.Real
        | _ -> SBType.Unknown
    | inferredType -> inferredType

let private stripQuotedString (valueText: string) =
    if valueText.Length >= 2 && valueText.StartsWith("\"") && valueText.EndsWith("\"") then
        valueText.Substring(1, valueText.Length - 2)
    else
        valueText

let private quoteString (value: string) =
    "\"" + value.Replace("\"", "\"\"") + "\""

let private tryParseFloat (valueText: string) =
    System.Double.TryParse(valueText, NumberStyles.Float, CultureInfo.InvariantCulture)

let private tryParseInt (valueText: string) =
    System.Int32.TryParse(valueText, NumberStyles.Integer, CultureInfo.InvariantCulture)

let private formatReal (value: double) =
    value.ToString("G17", CultureInfo.InvariantCulture)

let private tryCoerceValueText targetType valueText =
    match targetType with
    | SBType.String ->
        match tryParseFloat valueText with
        | true, n -> Some(quoteString (formatReal n))
        | _ -> Some(quoteString (stripQuotedString valueText))
    | SBType.Real ->
        match tryParseFloat (stripQuotedString valueText) with
        | true, n -> Some(formatReal n)
        | _ -> None
    | SBType.Integer ->
        match tryParseInt (stripQuotedString valueText) with
        | true, n -> Some(string n)
        | _ ->
            match tryParseFloat (stripQuotedString valueText) with
            | true, n -> Some(string (int n))
            | _ -> None
    | _ -> Some valueText

let rec tryEvaluateConstantExpr (state: ProcessingState) expr =
    // Constant evaluation is deliberately conservative: it only folds expressions
    // that can be evaluated from literals or currently constant-valued symbols.
    // Reassigning a symbol to a non-foldable expression clears that value upstream.
    let numericBinary op left right =
        match tryParseFloat left, tryParseFloat right with
        | (true, lhs), (true, rhs) ->
            match op with
            | "+" -> Some(formatReal (lhs + rhs))
            | "-" -> Some(formatReal (lhs - rhs))
            | "*" -> Some(formatReal (lhs * rhs))
            | "/" -> Some(formatReal (lhs / rhs))
            | "^" -> Some(formatReal (lhs ** rhs))
            | _ -> None
        | _ -> None

    let integerBinary op left right =
        match tryParseInt left, tryParseInt right with
        | (true, lhs), (true, rhs) ->
            match op with
            | "DIV" when rhs <> 0 -> Some(string (lhs / rhs))
            | "MOD" when rhs <> 0 -> Some(string (lhs % rhs))
            | "AND" -> Some(string (lhs &&& rhs))
            | "OR" -> Some(string (lhs ||| rhs))
            | "XOR" -> Some(string (lhs ^^^ rhs))
            | _ -> None
        | _ -> None

    let classifyConstantValueType (valueText: string) =
        if valueText.StartsWith("\"") && valueText.EndsWith("\"") then SBType.String
        else
            match tryParseInt valueText with
            | true, _ -> SBType.Integer
            | _ ->
                match tryParseFloat valueText with
                | true, _ -> SBType.Real
                | _ -> SBType.Unknown

    let coerceBinaryOperands op leftExpr rightExpr leftValue rightValue =
        match operandTypeSetFromOperator op with
        | None -> None
        | Some operandSet ->
            let leftType = classifyConstantValueType leftValue
            let rightType = classifyConstantValueType rightValue
            match coerceOperandType (mostGeneralOperandType leftType rightType) operandSet with
            | None -> None
            | Some coercedType ->
                match tryCoerceValueText coercedType leftValue, tryCoerceValueText coercedType rightValue with
                | Some coercedLeft, Some coercedRight -> Some(coercedType, coercedLeft, coercedRight)
                | _ -> None

    match expr with
    | NumberLiteral(_, value) -> Some value
    | StringLiteral(_, value) -> Some value
    | Identifier(_, name)
    | PostfixName(_, name, None) ->
        match tryResolveSymbol state.CurrentScope name state.SymTab with
        | Some(_, symbol) -> Symbol.valueText symbol
        | _ -> None
    | PostfixName _ -> None
    | SliceRange _ -> None
    | UnaryExpr(_, op, inner) ->
        match normalizeIdentifier op, tryEvaluateConstantExpr state inner with
        | "+", Some value -> Some value
        | "-", Some value ->
            match tryCoerceValueText SBType.Real value with
            | Some coerced ->
                match tryParseFloat coerced with
                | true, numeric -> Some(formatReal (-numeric))
                | _ -> None
            | _ -> None
        | "NOT", Some value ->
            match tryCoerceValueText SBType.Integer value with
            | Some coerced ->
                match tryParseInt coerced with
                | true, numeric -> Some(string (~~~numeric))
                | _ -> None
            | _ -> None
        | _ -> None
    | BinaryExpr(_, op, lhs, rhs) ->
        match tryEvaluateConstantExpr state lhs, tryEvaluateConstantExpr state rhs with
        | Some left, Some right ->
            match coerceBinaryOperands op lhs rhs left right with
            | Some(coercedType, coercedLeft, coercedRight) ->
                match normalizeIdentifier op with
                | "&" -> Some(quoteString (stripQuotedString coercedLeft + stripQuotedString coercedRight))
                | "+" | "-" | "*" | "/" | "^" -> numericBinary op coercedLeft coercedRight
                | "DIV" | "MOD" | "AND" | "OR" | "XOR" -> integerBinary (normalizeIdentifier op) coercedLeft coercedRight
                | "="
                | "=="
                | "<>"
                | "<"
                | "<="
                | ">"
                | ">=" ->
                    let boolResult =
                        match coercedType with
                        | SBType.String ->
                            let lhs = stripQuotedString coercedLeft
                            let rhs = stripQuotedString coercedRight
                            match normalizeIdentifier op with
                            | "="
                            | "==" -> lhs = rhs
                            | "<>" -> lhs <> rhs
                            | "<" -> lhs < rhs
                            | "<=" -> lhs <= rhs
                            | ">" -> lhs > rhs
                            | ">=" -> lhs >= rhs
                            | _ -> false
                        | _ ->
                            match tryParseFloat coercedLeft, tryParseFloat coercedRight with
                            | (true, lhs), (true, rhs) ->
                                match normalizeIdentifier op with
                                | "="
                                | "==" -> lhs = rhs
                                | "<>" -> lhs <> rhs
                                | "<" -> lhs < rhs
                                | "<=" -> lhs <= rhs
                                | ">" -> lhs > rhs
                                | ">=" -> lhs >= rhs
                                | _ -> false
                            | _ -> false
                    Some(if boolResult then "1" else "0")
                | _ -> None
            | None -> None
        | _ -> None

let tryEvaluateConstantIntegerExpr state expr =
    match tryEvaluateConstantExpr state expr with
    | Some value ->
        match tryParseInt value with
        | true, n -> Some n
        | _ ->
            match tryParseFloat value with
            | true, n when System.Math.Abs(n - System.Math.Round(n)) < 1e-9 -> Some(int (System.Math.Round n))
            | _ -> None
    | None -> None

let recordExpressionResult expr evaluatedType valueText (state: ProcessingState) =
    recordFact ExpressionResult None (describeExpr expr) state.CurrentScope (posOfExpr expr) (Some evaluatedType) valueText state

let private updateSymbolValue valueText (symbol: Symbol) =
    // A stored ValueText here does not mean "language-level constant declaration".
    // It means the symbol currently has a folded compile-time value that later
    // expressions are allowed to reuse until another assignment invalidates it.
    match symbol with
    | VariableSym symbol -> VariableSym { symbol with ValueText = valueText }
    | ConstantSym symbol -> ConstantSym { symbol with ValueText = valueText }
    | ParameterSym symbol -> ParameterSym { symbol with ValueText = valueText }
    | _ -> symbol

let updateSymbolType inferredType (symbol: Symbol) =
    let mergedType = mergeTypes (Symbol.typ symbol) inferredType
    match symbol with
    | VariableSym symbol ->
        VariableSym { symbol with Common = { symbol.Common with EvaluatedType = mergedType } }
    | ConstantSym symbol ->
        ConstantSym { symbol with Common = { symbol.Common with EvaluatedType = mergedType } }
    | ParameterSym symbol ->
        ParameterSym { symbol with Common = { symbol.Common with EvaluatedType = mergedType } }
    | ArraySym symbol ->
        ArraySym {
            symbol with
                Common = { symbol.Common with EvaluatedType = mergedType }
                ElementType = mergeTypes symbol.ElementType inferredType
        }
    | FunctionSym symbol ->
        let returnType = mergeTypes symbol.ReturnType inferredType
        FunctionSym {
            symbol with
                Common = { symbol.Common with EvaluatedType = returnType }
                ReturnType = returnType
        }
    | ProcedureSym _ -> symbol
    | BuiltInSym symbol ->
        BuiltInSym { symbol with Common = { symbol.Common with EvaluatedType = mergedType } }

let tryUpdateResolvedSymbolTypeAndValue inferredType valueText scopeName symbolName (table: SymbolTable) =
    match tryResolveSymbol scopeName symbolName table with
    | None -> table
    | Some(resolvedScope, symbol) ->
        match Map.tryFind resolvedScope table with
        | None -> table
        | Some scope ->
            let updatedSymbol =
                symbol
                |> updateSymbolType inferredType
                |> updateSymbolValue valueText
            let updatedScope =
                { scope with Symbols = Map.add (normalizeIdentifier symbolName) updatedSymbol scope.Symbols }
            Map.add resolvedScope updatedScope table

let validateScalarVsArrayUsage name position requiresIndexedAccess symbol (state: ProcessingState) =
    match symbol with
    | ArraySym _ when not requiresIndexedAccess ->
        appendDiagnostic SemanticDiagnosticCode.InvalidIndexing (Some name) (Some position) $"Array '{name}' used without index at {position.EditorLineNo}:{position.Column}" state
    | VariableSym _
    | ConstantSym _
    | ParameterSym _ when requiresIndexedAccess ->
        appendDiagnostic SemanticDiagnosticCode.InvalidIndexing (Some name) (Some position) $"Scalar '{name}' cannot be indexed at {position.EditorLineNo}:{position.Column}" state
    | _ -> state

let private appendNonWritableTargetError name position symbol (state: ProcessingState) =
    let detail =
        match symbol with
        | ConstantSym _ -> "Constant"
        | BuiltInSym _ -> "Built-in"
        | FunctionSym _ -> "Function"
        | ProcedureSym _ -> "Procedure"
        | _ -> "Symbol"
    appendDiagnostic SemanticDiagnosticCode.NonWritableTarget (Some name) (Some position) $"{detail} '{name}' is not a writable assignment target at {position.EditorLineNo}:{position.Column}" state

let private isWritableResolvedTarget requiresIndexedAccess symbol =
    match symbol with
    | VariableSym _
    | ParameterSym _ -> not requiresIndexedAccess
    | ArraySym _ -> requiresIndexedAccess
    | _ -> false

let validateWritableTarget name position requiresIndexedAccess symbol (state: ProcessingState) =
    let shapeValidatedState = validateScalarVsArrayUsage name position requiresIndexedAccess symbol state
    if isWritableResolvedTarget requiresIndexedAccess symbol then
        shapeValidatedState
    else
        match symbol with
        | ConstantSym _
        | BuiltInSym _
        | FunctionSym _
        | ProcedureSym _ -> appendNonWritableTargetError name position symbol shapeValidatedState
        | _ -> shapeValidatedState

let validateCallArity name position argumentCount resolvedSymbol (state: ProcessingState) =
    let appendExpected expected =
        appendDiagnostic SemanticDiagnosticCode.InvalidCallArity (Some name) (Some position) $"Call to '{name}' expects {expected} argument(s) but received {argumentCount} at {position.EditorLineNo}:{position.Column}" state

    match resolvedSymbol with
    | FunctionSym symbol ->
        let expected = symbol.Parameters.Length
        if expected = argumentCount then state else appendExpected expected
    | ProcedureSym symbol ->
        let expected = symbol.Parameters.Length
        if expected = argumentCount then state else appendExpected expected
    | BuiltInSym _ ->
        match BuiltIns.tryGetFixedArity name with
        | Some expected when expected <> argumentCount -> appendExpected expected
        | _ -> state
    | _ -> state

let private validateBuiltInNumericArgument name position index expr exprType (state: ProcessingState) =
    let isCoercibleConstantString =
        match exprType, tryEvaluateConstantExpr state expr with
        | SBType.String, Some value ->
            match tryCoerceValueText SBType.Real value with
            | Some _ -> true
            | None -> false
        | _ -> false

    if exprType = SBType.Integer || exprType = SBType.Real || exprType = SBType.Unknown || isCoercibleConstantString then
        state
    else
        appendDiagnostic SemanticDiagnosticCode.InvalidBuiltInArgument (Some name) (Some position) $"Built-in '{name}' argument {index} must be numeric-compatible at {position.EditorLineNo}:{position.Column}" state

let private validateBuiltInStringArgument name position index exprType (state: ProcessingState) =
    if exprType = SBType.String || exprType = SBType.Unknown then
        state
    else
        appendDiagnostic SemanticDiagnosticCode.InvalidBuiltInArgument (Some name) (Some position) $"Built-in '{name}' argument {index} must be string-compatible at {position.EditorLineNo}:{position.Column}" state

let private validateBuiltInWritableArgument name index expr (state: ProcessingState) =
    match expr with
    | Identifier(pos, targetName)
    | PostfixName(pos, targetName, None) ->
        match tryResolveSymbol state.CurrentScope targetName state.SymTab with
        | Some(_, symbol) -> validateWritableTarget targetName pos false symbol state
        | None -> state
    | PostfixName(pos, targetName, Some _) ->
        match tryResolveSymbol state.CurrentScope targetName state.SymTab with
        | Some(_, symbol) -> validateWritableTarget targetName pos true symbol state
        | None -> state
    | _ ->
        let pos = posOfExpr expr
        appendDiagnostic SemanticDiagnosticCode.InvalidBuiltInArgument (Some name) (Some pos) $"Built-in '{name}' argument {index} must be writable at {pos.EditorLineNo}:{pos.Column}" state

let private isWritableExpression state expr =
    match expr with
    | Identifier(_, targetName)
    | PostfixName(_, targetName, None) ->
        match tryResolveSymbol state.CurrentScope targetName state.SymTab with
        | Some(_, symbol) -> isWritableResolvedTarget false symbol
        | None -> false
    | PostfixName(_, targetName, Some args) ->
        match tryResolveSymbol state.CurrentScope targetName state.SymTab with
        | Some(_, symbol) -> isWritableResolvedTarget true symbol
        | None -> false
    | _ -> false

let validateBuiltInArguments name position argData (state: ProcessingState) =
    // Built-in modeling stays intentionally shallow: only cheap, stable argument
    // expectations are enforced here, while syntax-heavy or highly permissive
    // built-ins are left partially modeled.
    match BuiltIns.tryGetSignature name with
    | Some { ArgumentKinds = _ } when normalizeIdentifier name = "INPUT" ->
        argData
        |> List.mapi (fun i (expr, exprType) -> i + 1, expr, exprType)
        |> List.fold (fun currentState (index, expr, exprType) ->
            if isWritableExpression currentState expr || exprType = SBType.String || exprType = SBType.Unknown then
                currentState
            else
                validateBuiltInWritableArgument name index expr currentState) state
    | Some { ArgumentKinds = Some argumentKinds } when argumentKinds.Length = argData.Length ->
        (argumentKinds, argData)
        ||> List.zip
        |> List.mapi (fun i (kind, (expr, exprType)) -> i + 1, kind, expr, exprType)
        |> List.fold (fun currentState (index, kind, expr, exprType) ->
            match kind with
            | BuiltInArgumentKind.Any -> currentState
            | BuiltInArgumentKind.Numeric -> validateBuiltInNumericArgument name position index expr exprType currentState
            | BuiltInArgumentKind.String -> validateBuiltInStringArgument name position index exprType currentState
            | BuiltInArgumentKind.Writable -> validateBuiltInWritableArgument name index expr currentState) state
    | _ -> state

let private validateConstantCoercion pos operator coercedType leftExpr rightExpr (state: ProcessingState) =
    let validateOperand value expr currentState =
        match tryCoerceValueText coercedType value with
        | Some _ -> currentState
        | None ->
            appendDiagnostic SemanticDiagnosticCode.InvalidOperandCoercion None (Some pos) $"Operator '{operator}' cannot coerce operand at {pos.EditorLineNo}:{pos.Column} to {coercedType}" currentState

    let stateAfterLeft =
        match tryEvaluateConstantExpr state leftExpr with
        | Some value -> validateOperand value leftExpr state
        | None -> state

    match tryEvaluateConstantExpr stateAfterLeft rightExpr with
    | Some value -> validateOperand value rightExpr stateAfterLeft
    | None -> stateAfterLeft

let rec inferExprType (state: ProcessingState) expr =
    let typed inferredType nextState =
        let constantValue = tryEvaluateConstantExpr nextState expr
        inferredType, recordExpressionResult expr inferredType constantValue nextState

    match expr with
    | NumberLiteral(_, value) ->
        match System.Int32.TryParse value with
        | true, _ -> typed SBType.Integer state
        | _ -> typed SBType.Real state
    | StringLiteral _ -> typed SBType.String state
    | Identifier(_, name) ->
        match tryResolveSymbol state.CurrentScope name state.SymTab with
        | Some(_, symbol) ->
            let validatedState = validateScalarVsArrayUsage name (posOfExpr expr) false symbol state
            let contextValidatedState, symbolType =
                match symbol with
                | BuiltInSym _ -> validatedState, builtInResultType name
                | ArraySym symbol -> validatedState, symbol.ElementType
                | FunctionSym symbol ->
                    let inferred =
                        match symbol.ReturnType with
                        | SBType.Unknown -> mergeTypes symbol.Common.EvaluatedType (inferredVariableType validatedState validatedState.CurrentScope name)
                        | returnType -> returnType
                    validatedState, inferred
                | ProcedureSym _ ->
                    let pos = posOfExpr expr
                    let nextState = appendDiagnostic SemanticDiagnosticCode.InvalidExpressionContext (Some name) (Some pos) $"Procedure '{name}' cannot be used in expression context at {pos.EditorLineNo}:{pos.Column}" validatedState
                    nextState, SBType.Void
                | _ ->
                    let fromSymbol = Symbol.typ symbol
                    let inferred = if fromSymbol = SBType.Unknown then inferredVariableType validatedState validatedState.CurrentScope name else fromSymbol
                    validatedState, inferred
            typed symbolType contextValidatedState
        | None -> typed (inferredVariableType state state.CurrentScope name) state
    | PostfixName(pos, name, args) ->
        // Postfix names are overloaded in SuperBASIC syntax: array access, function
        // call, and some built-in forms all share this shape. Resolution decides
        // which interpretation is legal before result typing is chosen.
        let argTypes, stateAfterArgs =
            (([], state), args |> Option.defaultValue [])
            ||> List.fold (fun (collectedTypes, currentState) arg ->
                let argType, nextState = inferExprType currentState arg
                collectedTypes @ [ argType ], nextState)
        match tryResolveSymbol stateAfterArgs.CurrentScope name stateAfterArgs.SymTab with
        | Some(_, symbol) ->
            let validatedState =
                match symbol with
                | ArraySym _
                | VariableSym _
                | ConstantSym _
                | ParameterSym _ -> validateScalarVsArrayUsage name pos true symbol stateAfterArgs
                | _ -> stateAfterArgs
            let arityCheckedState =
                match symbol with
                | FunctionSym _
                | BuiltInSym _ -> validateCallArity name pos (args |> Option.defaultValue [] |> List.length) symbol validatedState
                | _ -> validatedState
            let signatureCheckedState =
                match symbol with
                | BuiltInSym _ ->
                    validateBuiltInArguments name pos (List.zip (args |> Option.defaultValue []) argTypes) arityCheckedState
                | _ -> arityCheckedState
            let contextValidatedState, inferred =
                match symbol with
                | ArraySym symbol -> signatureCheckedState, symbol.ElementType
                | FunctionSym symbol ->
                    let inferred =
                        match symbol.ReturnType with
                        | SBType.Unknown -> mergeTypes symbol.Common.EvaluatedType (inferredVariableType signatureCheckedState signatureCheckedState.CurrentScope name)
                        | returnType -> returnType
                    signatureCheckedState, inferred
                | BuiltInSym _ -> signatureCheckedState, builtInResultType name
                | ProcedureSym _ ->
                    let nextState = appendDiagnostic SemanticDiagnosticCode.InvalidExpressionContext (Some name) (Some pos) $"Procedure '{name}' cannot be used in expression context at {pos.EditorLineNo}:{pos.Column}" signatureCheckedState
                    nextState, SBType.Void
                | _ ->
                    let fromSymbol = Symbol.typ symbol
                    let inferred = if fromSymbol = SBType.Unknown then inferredVariableType signatureCheckedState signatureCheckedState.CurrentScope name else fromSymbol
                    signatureCheckedState, inferred
            typed inferred contextValidatedState
        | None -> typed (inferredVariableType stateAfterArgs stateAfterArgs.CurrentScope name) stateAfterArgs
    | SliceRange(pos, lhs, rhs) ->
        let lhsType, stateAfterLhs = inferExprType state lhs
        let rhsType, stateAfterRhs = inferExprType stateAfterLhs rhs
        let nextState =
            if (lhsType = SBType.Integer || lhsType = SBType.Unknown) && (rhsType = SBType.Integer || rhsType = SBType.Unknown) then
                stateAfterRhs
            else
                appendDiagnostic SemanticDiagnosticCode.InvalidSliceBounds None (Some pos) $"Slice bounds must be integer expressions at {pos.EditorLineNo}:{pos.Column}" stateAfterRhs
        typed SBType.Integer nextState
    | UnaryExpr(pos, op, inner) ->
        let innerType, nextState = inferExprType state inner
        match normalizeIdentifier op with
        | "NOT" ->
            let coercedState =
                match coerceOperandType innerType OnlyInteger with
                | Some SBType.Integer ->
                    match tryEvaluateConstantExpr nextState inner with
                    | Some value ->
                        match tryCoerceValueText SBType.Integer value with
                        | Some _ -> nextState
                        | None -> appendDiagnostic SemanticDiagnosticCode.InvalidOperandCoercion None (Some pos) $"Operator 'NOT' cannot coerce operand at {pos.EditorLineNo}:{pos.Column} to Integer" nextState
                    | None -> nextState
                | _ -> appendDiagnostic SemanticDiagnosticCode.InvalidOperandTypes None (Some pos) $"Operator 'NOT' expects an integer-compatible expression at {pos.EditorLineNo}:{pos.Column}" nextState
            typed SBType.Integer coercedState
        | "+"
        | "-" ->
            let coercedType =
                match coerceOperandType innerType FloatOrInteger with
                | Some coerced -> coerced
                | None -> SBType.Unknown
            let coercedState =
                match coercedType with
                | SBType.Integer
                | SBType.Real ->
                    match tryEvaluateConstantExpr nextState inner with
                    | Some value ->
                        match tryCoerceValueText coercedType value with
                        | Some _ -> nextState
                        | None -> appendDiagnostic SemanticDiagnosticCode.InvalidOperandCoercion None (Some pos) $"Operator '{op}' cannot coerce operand at {pos.EditorLineNo}:{pos.Column} to {coercedType}" nextState
                    | None -> nextState
                | _ -> appendDiagnostic SemanticDiagnosticCode.InvalidOperandTypes None (Some pos) $"Operator '{op}' expects a numeric-compatible expression at {pos.EditorLineNo}:{pos.Column}" nextState
            typed coercedType coercedState
        | _ -> typed innerType nextState
    | BinaryExpr(pos, op, lhs, rhs) ->
        // Binary operators are where the coercion table is applied. Unknown+Unknown
        // stays permissive so partially inferred real-world fixtures do not gain
        // false-positive diagnostics just because type information is incomplete.
        let leftType, stateAfterLhs = inferExprType state lhs
        let rightType, stateAfterRhs = inferExprType stateAfterLhs rhs
        match operandTypeSetFromOperator op with
        | Some operandSet ->
            let coercedType =
                coerceOperandType (mostGeneralOperandType leftType rightType) operandSet
                |> Option.defaultValue SBType.Unknown
            let validatedState =
                if coercedType = SBType.Unknown && leftType = SBType.Unknown && rightType = SBType.Unknown then
                    stateAfterRhs
                elif coercedType = SBType.Unknown then
                    appendDiagnostic SemanticDiagnosticCode.InvalidOperandTypes None (Some pos) $"Operator '{op}' does not support operand types {leftType} and {rightType} at {pos.EditorLineNo}:{pos.Column}" stateAfterRhs
                else
                    validateConstantCoercion pos op coercedType lhs rhs stateAfterRhs
            typed (resultTypeForOperator op coercedType) validatedState
        | None -> typed SBType.Unknown stateAfterRhs

let inferWritableTargetType (state: ProcessingState) expr =
    // Writable-target typing is narrower than ordinary expression typing because
    // only real storage locations may participate: variables, parameters, and
    // indexed arrays.
    match expr with
    | Identifier(_, name) ->
        match tryResolveSymbol state.CurrentScope name state.SymTab with
        | Some(_, symbol) ->
            if isWritableResolvedTarget false symbol then
                match symbol with
                | ArraySym symbol -> symbol.ElementType
                | _ -> Symbol.typ symbol
            else
                SBType.Unknown
        | None -> SBType.Unknown
    | PostfixName(_, name, args) ->
        match tryResolveSymbol state.CurrentScope name state.SymTab with
        | Some(_, symbol) ->
            if isWritableResolvedTarget args.IsSome symbol then
                match symbol with
                | ArraySym symbol -> symbol.ElementType
                | _ -> Symbol.typ symbol
            else
                SBType.Unknown
        | None -> SBType.Unknown
    | _ -> SBType.Unknown

let updateWritableTargetType expr inferredType valueText (state: ProcessingState) =
    match expr with
    | Identifier(_, name) ->
        match tryResolveSymbol state.CurrentScope name state.SymTab with
        | Some(_, symbol) when isWritableResolvedTarget false symbol ->
            { state with SymTab = tryUpdateResolvedSymbolTypeAndValue inferredType valueText state.CurrentScope name state.SymTab }
        | _ -> state
    | PostfixName(_, name, args) ->
        match tryResolveSymbol state.CurrentScope name state.SymTab with
        | Some(_, symbol) when isWritableResolvedTarget args.IsSome symbol ->
            { state with SymTab = tryUpdateResolvedSymbolTypeAndValue inferredType valueText state.CurrentScope name state.SymTab }
        | _ -> state
    | _ -> state

let resolveWritableTarget expr (state: ProcessingState) =
    // This helper resolves the storage location separately from any index arguments.
    // Callers can validate the target itself first, then walk the index expressions
    // as ordinary readable expressions.
    match expr with
    | Identifier(pos, name)
    | PostfixName(pos, name, None) ->
        let nextState = referenceSymbol None name pos state
        let validatedState =
            match tryResolveSymbol nextState.CurrentScope name nextState.SymTab with
            | Some(_, symbol) -> validateWritableTarget name pos false symbol nextState
            | None -> nextState
        validatedState, []
    | PostfixName(pos, name, Some args) ->
        let nextState = referenceSymbol None name pos state
        let validatedState =
            match tryResolveSymbol nextState.CurrentScope name nextState.SymTab with
            | Some(_, symbol) -> validateWritableTarget name pos true symbol nextState
            | None -> nextState
        validatedState, args
    | _ -> state, []
