# Turbo's SuperBASIC dialect

SB compiles Turbo 5.09's own parser (`parser509`, 4,369 lines) as Turbo compiles it. That
program uses most of the dialect Turbo and its Toolkit accept. This page lists the rules SB
follows for it; "P" numbers are lines of Turbo's parser source.

## Operators

- `&&`, `||` and `^^` are bitwise AND, OR and XOR of their operands converted to integers
  (Turbo IR 17, 16, 18). `~~` is bitwise NOT (IR 3). They are not the logical `AND`, `OR`,
  `XOR` and `NOT`, which give 0 or 1: `12 && 10` is 8, while `12 AND 10` is 1.
- `&&` has `AND`'s priority, `||` and `^^` have `OR`'s, and `~~` has `NOT`'s, as in Turbo's
  priority table (P 11242-11250).
- Unary `+` and `-` accept a value whose type is known only at run time, such as an untyped
  parameter, as the binary operators do.

## Statements and lines

- `ELSE` may stand anywhere in a long-form IF's lines: `a : ELSE : b` and `a : ELSE` both
  split the line there. An `ELSE` after a short-form `IF` on the same line belongs to that IF.
- A short-form IF may end with `: END IF` on its own line; the line's later statements then
  run whatever the condition: `IF a : b : ELSE c : END IF : d` always runs `d`.
- A line closes the enclosing block IF only when it holds more `: END IF` closers than
  same-line IF openers. A line ending `: END REPeat loop` ends its loop after the line's own
  statements.
- In `PRINT`, a leading `\` is the newline separator: `PRINT \"text"`.
- `DEBUG n` and `DEBUG_LEVEL m` compile lines conditionally (P 17582-17750, 26970): after
  `DEBUG n`, lines are left out while n is nonzero and above the level (0 unless given; only
  the first `DEBUG_LEVEL` counts). A program defining its own `DEBUG` keeps every line.
- `RETRY_HERE` sets the points where WHEN ERROR's `RETRY` resumes (IR 326 and 346);
  `RETRY_HERE 0` or `1` sets just one (P 17042, 23370).
- `DIM` bounds may use `MANIFEST` names. A DIM whose bounds are not all constants when the
  declaration is read keeps its full rank; its sizes are found at run time.

## Names, types and parameters

- The Turbo Toolkit procedures and functions in Turbo's own name table (P 11662-11898), such
  as `POKE$`, `PEEK$`, `BASIC_L`, `EDIT%` and `SET_PRIORITY`, are resident routines.
  `COMPILED` is the constant 1 in compiled code (P 19442-19454).
- A routine may index an array that a caller declares with `LOCal`: SuperBASIC's LOCal names
  are visible to the routines a routine calls. A routine's own scalar still cannot be indexed.
- `LEN` takes any value; a number is measured as its string form, so `LEN(123)` is 3.
- `RETurn` converts its value to the function's type, as an assignment would.
- A formal that a `REFERENCE` directive names is always passed by reference, and its argument
  must be a variable of the same type and kind. A plain variable passed to any other formal
  is bound by reference too, as the interpreter does, when its storage has the formal's type
  and kind. Otherwise it is passed by value in the formal's type, as Turbo passes every formal
  that `REFERENCE` does not name (P 21386-21486, 24046). Array elements and expressions are
  values.

## Limits

- `$hex` literals are accepted, but the JS ROM's SuperBASIC does not load them; Minerva and
  SMSQ/E do.
