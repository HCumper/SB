module SemanticAnalysisFacts

open Types
open ProcessingTypes

// SemanticAnalysisFacts is the small write-side API for semantic outputs.
//
// The semantic passes use this module to record:
// - declaration/reference/call facts for tooling and later lowering
// - expression-result facts for inferred types and folded values
// - diagnostics, while preserving the legacy string Errors surface
//
// Keeping fact and diagnostic mutation here avoids duplicating list-update and
// deduplication logic across the analyzer modules.
let private sameFactIdentity (left: SemanticFact) (right: SemanticFact) =
    left.Name = right.Name
    && left.Scope = right.Scope
    && left.Kind = right.Kind
    && left.Position.BasicLineNo = right.Position.BasicLineNo
    && left.Position.EditorLineNo = right.Position.EditorLineNo
    && left.Position.Column = right.Position.Column

let private addFactToList fact facts =
    fact :: (facts |> List.filter (fun existing -> not (sameFactIdentity existing fact)))

let recordFact kind category name scope position evaluatedType valueText (state: ProcessingState) =
    // Expression results are tracked separately from symbol/reference facts so
    // callers can distinguish "facts about the program graph" from "facts about
    // a computed expression at a source location".
    let fact =
        { Name = name
          Scope = scope
          Position = position
          Category = category
          Kind = kind
          EvaluatedType = evaluatedType
          ValueText = valueText }

    let updatedFacts =
        match kind with
        | ExpressionResult -> state.Facts
        | _ -> addFactToList fact state.Facts

    let updatedExpressionFacts =
        match kind with
        | ExpressionResult -> addFactToList fact state.ExpressionFacts
        | _ -> state.ExpressionFacts

    { state with Facts = updatedFacts; ExpressionFacts = updatedExpressionFacts }

let appendDiagnostic code symbolName position message (state: ProcessingState) =
    // Diagnostics are stored in structured form for downstream consumers, but we
    // also keep the original message in Errors so existing tests and CLI behavior
    // remain compatible while the structured surface is adopted incrementally.
    let diagnostic =
        { Code = code
          Severity = DiagnosticSeverity.Error
          Message = message
          Position = position
          Scope = state.CurrentScope
          SymbolName = symbolName }

    { state with
        Diagnostics = state.Diagnostics @ [ diagnostic ]
        Errors = state.Errors @ [ message ] }

let formatDiagnostic (diagnostic: SemanticDiagnostic) =
    // CLI formatting stays intentionally compact: stable code, optional symbol,
    // optional position, then the human-readable message.
    let positionText =
        match diagnostic.Position with
        | Some position -> $" ({position.EditorLineNo}:{position.Column})"
        | None -> ""
    let symbolText =
        match diagnostic.SymbolName with
        | Some name -> $" [{name}]"
        | None -> ""
    $"{diagnostic.Severity} {diagnostic.Code}{symbolText}{positionText}: {diagnostic.Message}"

let appendError message (state: ProcessingState) =
    appendDiagnostic SemanticDiagnosticCode.Generic None None message state
