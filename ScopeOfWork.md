# Scope Of Work

## Objective

Develop the project from a broad prototype into a more faithful and robust SuperBASIC toolchain by hardening the interpreter semantics that now exist and then bringing the generated backends up to the same behavioral standard.

## Current State

The interpreter path now has meaningful support for core SuperBASIC semantics, including:

- caller-dependent parameter binding
- `REFERENCE`-driven by-reference parameter intent in semantics and HIR
- numbered control flow with `GOTO`, `GOSUB`, `RETURN`, `ON GOTO`, and `ON GOSUB`
- dynamic `LOCal` lookup across routine calls

That means the immediate priority is no longer first implementation of these features. The immediate priority is securing them with deeper tests and making backend behavior match the interpreter.

## Priority Order

1. Harden the interpreter semantics with deeper regression coverage.

The current implementation is good enough to exercise real language behavior, but it still needs stronger coverage around:

- dynamic `LOCal` lookup across multi-level call chains
- local/global/parameter shadowing rules
- array-element aliasing and `REFERENCE`
- invalid argument forms for by-reference parameters
- negative control-flow cases such as missing line targets and out-of-range `ON` selectors

2. Strengthen the HIR model for locations.

The current HIR now distinguishes value arguments from reference-capable arguments, but it still does not have a single first-class location abstraction. A dedicated location model would make dynamic scope, aliasing, reads, and writes more coherent across lowering, interpretation, and code generation.

3. Make the interpreter the explicit semantic oracle.

The interpreter should define the intended language behavior. New semantic work should follow this order:

- add a failing regression test
- implement or correct the interpreter behavior
- add or update HIR lowering assertions
- then port the behavior to generated C# and C

4. Bring the generated backends toward parity.

The largest remaining functional gap is that generated C and C# do not yet match the interpreter for:

- by-reference procedure semantics
- dynamic `LOCal` lookup
- `GOSUB` / `RETURN`
- parts of numbered control flow

5. Keep separating language semantics from host/runtime behavior.

Core SuperBASIC rules should remain distinct from QL-specific environment behavior such as files, devices, graphics, and sound. That separation will continue to simplify both implementation and documentation.

## Suggested Implementation Sequence

1. Add more interpreter regression tests for `LOCal`, `REFERENCE`, and negative control-flow cases.
2. Add direct HIR lowering tests for dynamic-scope and by-reference cases.
3. Introduce a first-class location abstraction in HIR.
4. Port parameter-binding semantics to the C# backend.
5. Port parameter-binding semantics to the C backend.
6. Port `GOSUB` / `RETURN` semantics to generated C# and C.
7. Document backend parity and remaining gaps in `README.md`.

## Immediate Test Gaps

The most important missing tests on the interpreter path are:

- multi-level dynamic `LOCal` visibility
- shadowing between locals, parameters, and globals
- local arrays under dynamic scope
- invalid `REFERENCE` actuals
- nested `GOSUB`
- missing jump targets
- `ON GOTO` / `ON GOSUB` selector edge cases

## Strategic Advice

Do not prioritize graphics, sound, or broad built-in expansion yet.

The project will benefit more from securing the semantic core that is already implemented:

- dynamic scope
- caller-dependent parameter binding
- explicit `REFERENCE` handling
- numbered control flow

These semantics are central to the identity of the project and affect every backend.

## Longer-Term Options

After the semantic core and backend parity are stable, the project can develop in one of two main directions.

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

- keep expanding semantic regression tests
- strengthen HIR around first-class locations
- use the interpreter as the reference model
- make both code generators match it

This is the most effective way to turn the project into a dependable language implementation rather than a partially working translation pipeline.
