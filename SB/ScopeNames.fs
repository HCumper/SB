module ScopeNames

// ScopeNames collects reserved scope identifiers shared by the semantic passes.
//
// Keeping them here avoids stringly-typed scope conventions leaking through the
// analyzer.
//
// Reserved synthetic scope names used by semantic analysis.
let globalScope = "~Global"
