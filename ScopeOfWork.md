# Scope Of Work

## Objective

Develop the project from a broad prototype into a more faithful and robust SuperBASIC toolchain by prioritizing semantic correctness over feature breadth.

## Recommended Direction

The highest-value next step is to stop adding surface features and tighten the semantic core. The project already has the major compiler stages in place, but key language behaviors are still only partially modeled. The most useful work now is to make one compatibility slice correct end to end.

## Priority Order

1. Finish parameter semantics.

Implement SuperBASIC and Turbo-style argument binding correctly:

- bare variable actuals alias caller storage
- expression actuals pass values
- `REFERENCE` forces writable actuals

This improves the interpreter, semantic model, and both code generators at the same time.

2. Introduce first-class storage locations in HIR.

The current representation handles values and assignments, but by-reference behavior needs an explicit notion of storage location or cell. Once HIR can distinguish "read this value" from "alias this location", several difficult language behaviors become easier to model correctly.

3. Make the interpreter the reference implementation.

Use the interpreter as the semantic oracle and treat the C and C# backends as targets that must match it. That gives a stable development loop:

- add a language rule
- make the interpreter correct
- add focused regression tests
- update code generators to match

4. Build a compatibility test corpus.

The project will benefit more from many small semantic tests than from large sample programs. Priority areas:

- `LOCal` and dynamic scope
- parameter aliasing and value-passing cases
- `REFERENCE`
- `DATA` / `READ` / `RESTORE`
- `ON GOTO` / `ON GOSUB`
- arrays, strings, and numeric coercions

5. Separate language semantics from host runtime behavior.

Keep core SuperBASIC rules distinct from Sinclair QL environment features such as devices, channels, graphics, and sound. That separation will simplify both implementation and documentation.

## Suggested Implementation Sequence

1. Add failing tests for parameter aliasing and `REFERENCE`.
2. Extend symbols and HIR to represent by-reference or location bindings.
3. Implement the model in the interpreter first.
4. Port the same model to the C# backend.
5. Port the same model to the C backend.
6. Document the supported semantics in `README.md`.

## Strategic Advice

Do not prioritize graphics, sound, or a large expansion of built-ins yet. The semantic core should come first, especially:

- dynamic scope
- caller-dependent parameter binding
- explicit `REFERENCE` handling

Those areas are central to the identity of the project and will influence the correctness of every backend.

## Longer-Term Options

After the semantic core is stable, the project can develop in one of two main directions.

Compiler-oriented direction:

- improve HIR design
- add optimization passes
- strengthen diagnostics
- clean up generated C and C#

Compatibility-oriented direction:

- expand built-in coverage
- implement channels, files, and devices
- add graphics and sound behavior
- improve Sinclair QL compatibility

## Practical Recommendation

The best immediate roadmap is:

- add tests for parameter semantics
- implement a location-based model in HIR
- make the interpreter correct first
- make both code generators match

This is the most effective way to turn the project into a dependable language implementation rather than a partially working translation pipeline.
