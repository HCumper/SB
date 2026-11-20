# SSB 2.8.0 compatibility fixtures

These source programs and rejection/long-line expectations come from the local
Structured Super Basic 2.8.0 reference checkout's `Tests` directory, supplied for
this implementation update. The reference translator is by Timothy Swenson and
Norman Dunbar. Their original fixtures are retained here unchanged.

`Ssb280Tests.fs` redirects QDOS result-file operations to console output before
executing the programs through SB. The `long-lines-trim` case and its generated
QL-internal helper are not part of the managed preprocessor execution tests.

See `docs/ssb280.md` for supported syntax, target profiles and runtime scope.
