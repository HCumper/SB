# Precise procedure and array effects

Implemented 22 November 2026. This report records compiler analysis, functional
68000 execution counts and binary comparisons. No Q-emuLator elapsed-time trials
were rerun for this change.

## Implementation

The effect summary now separates value writes, descriptor writes and captured
FOR metadata writes, with independent unknown flags. Array-element writes
invalidate values while preserving descriptors and FOR control data. Ordinary
scalar assignments preserve END/STEP/NEXT; DIM and FOR initialization remain
metadata mutations. Opaque residents and memory writes retain all barriers.

Known calls project each effect category onto actual references, discard their
private LOCAL/formal effects at normal return, and keep scalar ValueArgs private.
Whole-array references and scalar references to individual elements are handled
separately. Indexed array formals are recognized even when HIR supplies an
advisory scalar type. Recursive summaries and reference escape tracking use the
same resolution rules.

A named access resolves to its own formal/LOCAL binding when present, or to a
unique global if no declared formal/LOCAL can shadow it. Ambiguous dynamic
bindings remain conservative. See [the analysis design](../../../../SB/Optimization/HIR-ANALYSIS.md).

## Route planner

The unchanged road-network program still returns checksum 4701. Its analysis now
retains valid facts through element-only procedure calls:

| Metric | Before | After |
| --- | ---: | ---: |
| Arithmetic proofs enabled | False | True |
| Proven arithmetic expressions | 0 | 6 |
| Proven numeric array accesses | 0 | 1 |
| Rank-one native array facts | 0 | 1 |
| Rank-two native array facts | 0 | 0 |
| Selected native FOR loops | 0 | 0 |
| Ordinary zero-window executable bytes | 9,180 | 9,136 |

The code is 44 bytes smaller. The analysis no longer treats ordinary road-array
element stores as arbitrary corruption of FOR continuations. Each known route
procedure has explicit read/write roots and no unknown-memory write flag.

The direct runtime gain is small. The important local/branching loops retain
their existing paths, and incoming global-array shapes are not inferred for
procedure bodies. These remain separate improvements.

Both preserved pre-change and current compilers also built the unchanged source
with default console startup, the same task name and all optimizations enabled.
The complete executables ran in the functional 68000 harness:

| Metric, default-console builds | Before | After | Reduction |
| --- | ---: | ---: | ---: |
| Executed instructions | 7,961,541 | 7,945,035 | 0.207% |
| Instruction words fetched | 10,212,991 | 10,194,445 | 0.182% |
| Executable bytes | 9,282 | 9,238 | 44 bytes |

These are instruction counts, not elapsed time or calibrated MC68008 cycles.
Default console startup accounts for the size difference from the zero-window
Q-emuLator builds. Neither executable nor program source was instrumented or
patched. The last measured Q-emuLator route time remains the value in the
[prior elapsed-time report](../../../QemuLator/Results/2026-11-22-string-append/README.md).

## Verification

All 127 focused analysis/execution checks pass, including 70 new cases. They
cover element-only calls, DIM invalidation, reference/value arguments, scalar
counter writes, hidden FOR metadata clobbers, recursion, array forwarding,
private same-named vectors, dynamic shadowing, error ordering and escape tracking.

`dotnet test SB.sln --no-build --no-restore` passed all 3,248 tests: 3,000 compiler
tests and 248 runtime tests, with no failures or skips. The compiler and test
assemblies were freshly built by the preceding focused run.

The standard six-mode benchmark run passes 78 executions and compiles all 12
ROM-dependent cases, with no failures. All nine ordinary QDOS benchmark builds
match independent SB builds. Only route planner's binary changes; the other
eight are byte-for-byte identical to the rank-two baseline. All nine genuine
Turbo FAST payloads still match the compatibility generator's output from their
captured IR. Those verifications are compilation checks, not new elapsed trials.

## Provenance and evidence

The baseline compiler includes the preceding uncommitted rank-two implementation,
with SHA256 `336299334BDE47C889756DE97494A82B0CE796CDDB384DA9DC6261A3581D958C`.
The current compiler SHA256 is
`966F8917AF889C47A6DE3800151A62403B17A7245326F82F7335E4C37CF7D773`.
Both source patches use base revision `24eb6f58457c4b8cae1d7d5bf04ee752557a805d`.

[Route execution comparison](route-execution.json), [route analysis summaries](route-analysis.txt),
[all-nine binary comparisons](binary-comparison.json), [ordinary SB verification](sb-verification.json),
[Turbo native verification](native-verification.json), [six-mode results](six-mode-results.csv),
[six-mode configuration](six-mode-configuration.json), [focused test log](targeted.log),
[full-suite log](full-suite.log), [suite summary](suite-summary.json),
[current source patch](compiler-change.patch), [baseline source patch](baseline-source.patch),
[baseline invocation script](Invoke-Baseline.fsx), [route execution script](Count-Route.fsx).

Reusable executables, compiler dumps, logs and the preserved baseline compiler
are in `C:\Source\codex-workspace\.scratch\turbo-procedure-effects`.
