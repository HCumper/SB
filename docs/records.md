# Records for Structured SuperBASIC

Status: design proposal for a standalone QL program, not implemented. Revised after two reviews
(section 16). Target-ROM and Turbo experiments in section 15 are prerequisites to implementation.

S*BASIC has numbers, integers, strings and arrays of them, but no way to group values into one
named thing. Programs fake it with parallel arrays (`name$(n)`, `age(n)`, `score(n)`) kept in step
by hand. This document defines **record types** for Structured SuperBASIC (SSB) and a QL program,
`ssbrec`, that turns them into plain S*BASIC before SSB runs. The intended targets are interpreted
SuperBASIC on a QL, SBASIC under SMSQ/E, and Turbo-compiled programs. Compatibility is to be
established for named ROM, SMSQ/E, Turbo and toolkit versions, rather than assumed for every
version. The portable baseline and its array-field argument restriction are defined below.

Part 1 is the language: what a programmer may write. Part 2 is how `ssbrec` implements it.

---

## Part 1: Rules for using records

### 1. Declaring a record type

A record type is declared in a `TYPE` block, one field per line:

```
TYPE point
  x
  y
END TYPE

TYPE person
  name$(30)          ## a string field needs a maximum length
  age%
  scores(5)          ## an array field: scores(0) to scores(5)
  home AS point      ## a field that is itself a record
END TYPE
```

1. `TYPE` blocks appear at the top level, never inside a procedure or function. They may be
   anywhere in the program or in an `#INCLUDE`d file, and a type can be used before its block.
   A `TYPE` block inside an inactive `#IFDEF` block does not exist.
2. Type and field names follow the S*BASIC rules for names (a letter, then letters, digits and
   underscores) and, like all S*BASIC names, ignore case.
3. A field's suffix gives its kind as usual: none for floating point, `%` for integer, `$` for
   string. A string field must give a maximum length: `name$(30)`.
4. A field may be an array: `scores(5)`, `grid(3,3)`. A string array field gives its dimensions and
   then the length: `lines$(10,80)`. How many dimensions are allowed in total is set by rule 10.
   Field bounds must be non-negative integer literals or numeric `MANIFEST` constants known to
   pass one; string capacities must be positive integers. They are resolved to literal bounds in
   generated code. Runtime expressions are allowed for the outer dimensions of a record variable,
   not for a type's field layout. Thus every instance of a type has the same field shapes.
5. A field may be a record of another type (`home AS point`). A type may not contain itself, directly
   or through other types; use a pointer field for that (rule 31).
6. A `TYPE` block produces no code of its own.

### 2. Declaring record variables

```
DIM p AS point                ## one record
DIM people(100) AS person     ## an array of records: people(0) to people(100)
DIM grid(8,8) AS point        ## an array of records with two dimensions

DEFine PROCedure plot(c AS point)
  LOCal t AS point            ## a record local to this call
  ...
END DEFine
```

7. Every record variable is declared with `AS type`: by `DIM` for global records, by `LOCal` inside
   a procedure or function, or in the parameter list of a `DEFine`.
8. `DIM` and `LOCal` of a record set every field to 0 or the empty string. Executing a `DIM` again,
   including with different outer array sizes, clears every element of the resulting record array;
   old values are not preserved. Generated code clears the fields explicitly after allocation,
   rather than relying on the target's redimensioning behaviour (section 12).
9. An array of records may have as many dimensions as rule 10 allows.
10. Each field of an array of records becomes one S*BASIC array whose subscripts are, in order:
    the record array's subscripts, then the field's own, then (for a string field) its maximum
    length. `DIM people(100) AS person` with a field `lines$(10,80)` becomes
    `DIM people_lines$(100,10,80)`: three. The total must fit the S*BASIC the program will run on.
    The Sinclair manual states no limit, so the limit of the QL ROM, SMSQ/E and Turbo is to be
    measured (section 15); `ssbrec` rejects any declaration whose generated shape exceeds the
    measured maximum, naming the field.
11. **Scope.** Record variables follow S*BASIC's scope rules, which are dynamic: a routine sees the
    `LOCal` variables of the routines that called it, unless it declares its own. So that every
    use of a record name means the same fields wherever it is reached from, **a record name has one
    type throughout the program**: every `DIM`, `LOCal` and parameter of that name must use the same
    type. Declarations of a name must also agree on whether it is a single record or an array and
    on the array rank; outer bounds may differ. A routine may use a record it does not declare
    itself (a global, or a caller's `LOCal`), as long as the name is declared somewhere with that
    type and rank. This deliberately restricts name reuse, even between unrelated routines: use
    distinct parameter and local names when their record types or ranks differ.
12. A record variable's name may not also be used for an ordinary variable.

### 3. Using fields

```
p.x = 3 : p.y = p.x * 2
people(i).name$ = "Ada"
people(i).scores(2) = 87
people(i).home.x = p.x
PRINT people(i).name$; " is "; people(i).age%
INPUT "Age: "; people(i).age%
```

13. A field is reached with a dot: `p.x`, `people(i).name$`, `people(i).home.x`. Subscripts of an
    array of records come before the dot, subscripts of an array field after it:
    `people(i).scores(j)`.
14. A field is an ordinary variable of its kind. It may be used wherever S*BASIC allows a variable of
    that kind: in expressions, on the left of `=`, in `INPUT`, `READ`, as a procedure argument,
    and in string slicing (`people(i).name$(1 TO 3)`).
    A field cannot be separately `DIM`ensioned or declared `LOCal`: its shape belongs to its type.
    A routine receiving an array field by reference must not redimension that field; this would
    violate the fixed layout on which whole-record copies and initialization depend.
15. Only a field of a single record (not of an array of records) may be the control variable of a
    `FOR` loop, because S*BASIC requires a simple variable there.

### 4. Whole records

```
q = p                       ## copy every field
people(i) = people(j)
swap personOne, personTwo   ## standalone person records passed to a procedure
```

16. A record may be assigned to another record of the same type as a whole statement:
    `q = p` or `LET q = p`. It copies every field, including array fields.
17. A record may be passed to a procedure or function whose parameter is declared with the same
    type; whole record-array arguments must also have the rank expected by the parameter. Records
    are passed by reference: changes through the parameter change the caller's record. Generated
    Turbo `REFERENCE` directives establish this explicitly (section 12).

    **Portable baseline:** a single record stored in an array of records may be passed this way
    only when its type has no array fields, including within embedded records. A bounded string
    field is a scalar field; a string-array field is an array field. A standalone record with
    array fields, or a whole array of records, may be passed using complete backing arrays.
    Array-element records with array fields can still be read, written and copied; passing them
    as record arguments is refused with a diagnostic. No silent copy-in/copy-out is substituted
    for the promised reference semantics. Section 12 describes the reason and future options.
18. Whole records may not be compared, printed, input or read. Use their fields.
19. **Each record reference is evaluated once.** A subscript that selects a record, as in
    `plot points(next%())`, is evaluated once, even though the record stands for several fields.
    The same holds for whole-record copies (`q = people(pick%())`) and for the sizes in a `DIM` of
    an array of records (`DIM people(count%()) AS person`). Whenever a reference is expanded into
    several field operations, every selecting subscript is saved before those operations, even
    if it is just a variable or a side-effect-free expression. Only literal constants or resolved
    compile-time constants may be repeated without a temporary. For a whole-record assignment,
    destination selectors are captured first, then source selectors, each list left to right,
    before any destination field is written. The ordering restrictions in rule 26 apply to these
    generated evaluations too. A single-field access that needs no expansion retains its normal
    S*BASIC evaluation point.

### 5. Procedures and functions with record parameters

```
DEFine PROCedure swap(firstPerson AS person, secondPerson AS person)
  LOCal savedPerson AS person
  savedPerson = firstPerson : firstPerson = secondPerson : secondPerson = savedPerson
END DEFine

DEFine FuNction dist(firstPoint AS point, secondPoint AS point)
  RETurn SQRT((firstPoint.x - secondPoint.x)^2 + (firstPoint.y - secondPoint.y)^2)
END DEFine
```

20. A parameter is declared a record with `AS type`. Inside the routine it is used like any record.
21. An argument for a record parameter must be a record of that type: a record variable, an element
    of an array of records, or a record field of another record, subject to rule 17. An embedded
    record reached through an array element has the same backing-array restriction.
22. A parameter may be an array of records: `DEFine PROCedure sortby(list() AS person, n)`. The
    argument is the whole array: `sortby people, count`. Empty parentheses denote a one-dimensional
    array parameter, `(,)` a two-dimensional one, and so on; the number of commas plus one gives
    its rank. Parameter declarations give no outer bounds. Calls with a different rank are errors.

### 6. Functions that return a record

```
DEFine FuNction flip AS point (flipArg AS point)
  flip.x = flipArg.y : flip.y = flipArg.x
END DEFine

p = flip(p)
PRINT flip(q).x
```

23. A function returns a record when its `DEFine FuNction` line names a type with `AS` before the
    parameter list. Inside the function, the result is the function's own name used as a record
    (`flip.x`). Its fields start at 0 or the empty string on every call; the function sets the ones
    it needs and returns by reaching `END DEFine` or a plain `RETurn`.
24. The result is built in storage of its own and copied to its destination only after the function
    returns. So `p = flip(p)` gives `(2,1)` from `(1,2)`: the function never sees its result
    overwrite its argument.
25. Such a function may be called as a whole statement assigning a record (`p = flip(p)`), or inside
    an expression followed by a field (`flip(q).x`).
26. **Evaluation order.** Evaluations introduced by record lowering occur before the statement's
    remaining evaluation. This includes record-returning calls, saved record selectors and saved
    `DIM` sizes, not just calls returning records. A whole-record assignment captures its selectors
    in the order in rule 19; calls returning records are then evaluated left to right into separate
    temporaries. Conditions of `IF` and limits of `FOR` follow the same policy, once on entry to
    that statement. A `THEN` or `ELSE` body is a separate region: its generated evaluations run
    only when that body runs, never unconditionally before the `IF`.

    To keep this policy implementable, a region with generated evaluations may not leave another
    potentially effectful function call in the original statement, or move a call past another
    such call whose relative order cannot be preserved. This also applies when the generated
    evaluation merely reads a selector: `consume advance%(), points(i)` is refused because
    `advance%()` could change `i` before the original record argument was reached. User-defined,
    toolkit and extension functions, and built-ins such as `RND`, `INKEY$` and `KEYROW`, are
    potentially effectful; only the built-ins listed in section 13 are exempt. Several independent
    record-returning calls are allowed in their defined left-to-right order. Nested
    record-returning calls are refused in the first version; use intermediate variables.
    Calls whose arguments would require moving another potentially effectful call are also
    refused. The destination procedure invoked after an argument list is evaluated is not an
    additional function call within that list.

    For example, `consume first%(), points(second%())` is refused: moving `second%()` ahead of
    `first%()` would reverse their order. If moving an evaluation would cross an observable
    operation such as an earlier `PRINT` item or `INPUT` prompt, the construct must likewise be
    split or refused. Diagnostics explain which evaluations conflict and suggest explicit
    temporaries. A whitelist entry is not permission to repeat a selector (rule 19).
27. A record-returning function may be recursive.

### 7. Record pointers and pools

A pointer is the index of a record in an array of records. It is how lists, trees and other linked
structures are built, and how a function returns "a reference to" a record.

```
TYPE node
  val
  left%
  right%
END TYPE
MANIFEST : MaxNodes = 500
DIM nodes(MaxNodes) AS node POOL

DEFine FuNction insert%(VAL n%, v)
  IF n% = 0 THEN n% = nodes_new% : nodes(n%).val = v : RETurn n%
  IF v < nodes(n%).val THEN
    nodes(n%).left% = insert%(nodes(n%).left%, v)
  ELSE
    nodes(n%).right% = insert%(nodes(n%).right%, v)
  END IF
  RETurn n%
END DEFine
```

28. A pointer is an integer (`%`) variable, field or array element holding an index into an array of
    records. Index 0 means "no record"; element 0 of a pooled array is never used. Pointers are
    plain integers, not typed references: nothing records which array a pointer belongs to.
29. `DIM name(size) AS type POOL` (at the top level only) declares an array of records managed as a
    pool of `size` records, numbered 1 to `size`, where `size` is 1 to 32767 (the largest S*BASIC
    integer). It creates two routines:
    - `name_new%` returns the index of a free record with all its fields cleared;
    - `name_free n%` gives record `n%` back to the pool.

    A Turbo `MANIFEST` constant (`MANIFEST : MaxNodes = 500`, then
    `DIM nodes(MaxNodes) AS node POOL`) is a convenient way to name the size; a literal or an
    ordinary numeric variable is also allowed. The evaluated size is checked before allocation.
    Compiled, `MANIFEST` names cost no variable space; interpreted, the line runs as ordinary
    assignments, which needs the Turbo Toolkit loaded (section 15).
30. **Pool safety.** These cases are checked and stop the program with a message naming the pool:
    - `name_new%` when every record is in use ("pool nodes full");
    - `name_free` with an index outside 1 to `size`;
    - `name_free` of a record that is already free.

    `name_free 0` does nothing, so a "no record" pointer can be freed safely. Using a pointer after
    its record has been freed is **not** detected: the record may have been handed out again, and
    reading through the old pointer gives whatever is there now. Nor is using a pointer with the
    wrong array (rule 28).
31. Record types may contain other records by value, as in `home AS point`. The graph of those
    embedded-record fields must be acyclic. Recursive links use integer pointer fields such as
    `left%`; those fields do not embed another record or declare a typed reference.
32. A parameter marked `VAL` is passed by value. Use it for pointers: S*BASIC passes variables and
    array elements by reference, so a routine that moves its pointer parameter along a list
    (`n% = nodes(n%).next%`) would otherwise rewrite the caller's links. `VAL` may be used with any
    simple numeric or string parameter.
33. Pointer variables follow normal S*BASIC scope: a pointer in a `LOCal` or a parameter belongs to
    one call, so recursive routines each have their own. That says nothing about the records they
    point to: pooled records are global, outlive the call that created them, and stay valid only
    until they are freed or their pool is reinitialized. Keeping pointers valid is the program's
    job; reinitializing a pool invalidates all outstanding handles into it.

### 8. Names

34. A field `x` of record `p` becomes the S*BASIC name `p_x` (section 11 gives the full scheme).
    `ssbrec` reports an error, naming both sources, whenever two names collide: a generated name
    and one the program already uses, **or two generated names**. For example a record `p` with a
    field `home_x` and a field `home AS point` would both produce `p_home_x`; one must be renamed.
    Repeated declarations of the same logical record name and field path, with the same type and
    rank, intentionally reuse their generated names for dynamic scope and are not collisions.
35. Names beginning `rec_` are reserved for `ssbrec`'s own variables and routines.
36. Give record variables, record-returning functions and pools distinctive names (`player`,
    `nodes`) rather than short or common ones (`p`, `r`). Any name works, but `ssbrec` runs faster
    with distinctive names (section 13), and it warns when a name is short or common enough to slow
    it down.

### 9. A complete example

```
TYPE node
  val
  left%
  right%
END TYPE
MANIFEST : MaxNodes = 200
DIM nodes(MaxNodes) AS node POOL

root% = 0
FOR i = 1 TO 10 : v = RND(1 TO 100) : root% = insert%(root%, v) : END FOR i
walk root%
STOP

DEFine FuNction insert%(VAL n%, v)
  IF n% = 0 THEN
    n% = nodes_new%
    nodes(n%).val = v
    RETurn n%
  END IF
  IF v < nodes(n%).val THEN
    nodes(n%).left% = insert%(nodes(n%).left%, v)
  ELSE
    nodes(n%).right% = insert%(nodes(n%).right%, v)
  END IF
  RETurn n%
END DEFine

DEFine PROCedure walk(VAL n%)
  IF n% = 0 THEN RETurn
  walk nodes(n%).left%
  PRINT nodes(n%).val
  walk nodes(n%).right%
END DEFine
```

---

## Part 2: Implementation on the QL

### 10. Where it lives

`ssbrec` is a separate QL program that runs before SSB:

```
prog_ssr  --(ssbrec)-->  prog_ssb  --(SSB)-->  prog_bas  --> interpreter / Turbo
```

- SSB is not changed. Records need a statement scanner, declarations, scopes and signatures; SSB
  stays the small line filter it is, and programs without records never meet `ssbrec`.
- `ssbrec` runs before line numbering, so it can add lines freely; SSB numbers everything
  afterwards.
- `ssbrec` resolves `#INCLUDE`, `#DEFINE`, `#IFDEF` and `#ENDIF` itself, with exactly SSB's rules
  (its procedures are copied from SSB's passes), and writes only the active code with includes
  expanded. Records are therefore analysed on the same program SSB will number, and inactive
  blocks contribute no types, declarations, includes or clashes. SSB then has labels, `##`
  comments and ordinary lines left to handle.
- Input files use the extension `_ssr`; output is the `_ssb` SSB reads.
- Like SSB, `ssbrec` is written in SSB (without records), compiled with Turbo, and developed on a QL
  or emulator: SSB's README warns that its sources use the QL character set and must not be edited
  on a PC.
- Its tables are fixed-size, with defaults that environment variables can raise, as SSB does for
  labels (`SSBRECTYPES`, `SSBRECFIELDS`, `SSBRECVARS`, `SSBRECNAMES`).
- The selected compatibility profile records the tested ROM, SMSQ/E, Turbo and toolkit versions
  and the supported dimension, name, line and parameter/local limits. Generation refuses features
  outside that profile. The implementation and its execution tests remain entirely in the QL
  toolchain described here.

### 11. Naming scheme

A field path is joined with underscores and keeps the kind suffix of the final field at the end.
Subscripts of the array of records come first, then those of the field:

| Source | S*BASIC |
|---|---|
| `p.x` | `p_x` |
| `p.name$` | `p_name$` |
| `p.home.x` | `p_home_x` |
| `people(i).age%` | `people_age%(i)` |
| `people(i).scores(j)` | `people_scores(i,j)` |
| `people(i).home.x` | `people_home_x(i)` |

Because `_` may also appear inside names, two paths can produce the same name; pass one checks every
generated name against all others and against the program's own names (rule 34).

### 12. Lowering rules

**Declarations**

| Source | S*BASIC |
|---|---|
| `TYPE ... END TYPE` | nothing (recorded in the type table) |
| `DIM p AS person` | `p_age% = 0 : DIM p_name$(30), p_scores(5) : p_home_x = 0 : p_home_y = 0` |
| `DIM people(100) AS person` | `DIM people_name$(100,30), people_age%(100), people_scores(100,5), people_home_x(100), people_home_y(100)` |
| `LOCal t AS point` | `LOCal t_x, t_y` then `t_x = 0 : t_y = 0` |
| `DEFine PROCedure plot(c AS point)` | `REFERENCE c_x, c_y` on a separate line before `DEFine PROCedure plot(c_x, c_y)` |

These declaration examples show the storage allocation; the complete output also contains the
clearing operations below. Every runtime outer bound is captured once before the first allocation.
Field bounds and string capacities have already been resolved to constants.

**Allocation and initialization.** Simple numeric and string fields are explicitly set to 0 or
`""`. Every numeric array element is explicitly cleared after `DIM` or `LOCal` allocation, and
every string-array element is assigned `""`. Iterate the record-array and field-array axes, not
the final string-capacity entry in a `DIM`. This guarantees rule 8 even when Turbo preserves old
values while resizing a rubber array. Fresh local records and every record-function result use
the same clearing rules; no field may retain a previous call's value.

For example, the full lowering of `DIM points(limit) AS point` includes:

```
rec_s1 = limit
DIM points_x(rec_s1), points_y(rec_s1)
FOR rec_i1 = 0 TO rec_s1
  points_x(rec_i1) = 0 : points_y(rec_i1) = 0
END FOR rec_i1
```

This clears the resulting arrays even if `limit` has changed since an earlier `DIM`.

**Calling convention.** Turbo's default for scalar parameters is value passing. `ssbrec` emits
`REFERENCE` directives immediately before each affected routine definition for all flattened
record parameters and all hidden result parameters. Complete array parameters are declared with
dummy subscripts giving their generated rank, as Turbo requires. A bounded string contributes its
character-capacity dimension; the exact simple-string/array reference spelling must pass the
target tests in section 15. No implicit value-passing fallback is allowed for record fields.

Existing `REFERENCE` entries for ordinary parameters are preserved and merged with the generated
entries; an entry naming a record parameter expands into its fields. `VAL` parameters are excluded
from reference directives, and a conflicting user directive is an error. Generated `REFERENCE`
lines precede `DEFine`; generated `LOCal` declarations follow it. Interpreted output requires the
toolkit support needed to recognize those directives, even if the source uses no `MANIFEST`.

**Fields**: every field reference becomes its S*BASIC name (section 11).

**Evaluating a record reference once (rule 19).** Every non-constant selector of an expanded
record reference is evaluated into its own temporary `rec_sN`, before any fields are copied or
bound. This includes a plain variable, an array access and a call of a side-effect-free built-in:
all may read storage that an earlier field write changes through an alias. The purity whitelist
does not justify repeating them. Only literal or resolved compile-time constants may be reused.
Bounds of a record-array `DIM` are captured by the same rule. Single-field accesses that need no
expansion stay at their original evaluation point.

| Source | S*BASIC |
|---|---|
| `plot points(next%())` | `rec_s1 = next%() : plot points_x(rec_s1), points_y(rec_s1)` |
| `plot grid(i,j)` | `rec_s1 = i : rec_s2 = j : plot grid_x(rec_s1,rec_s2), grid_y(rec_s1,rec_s2)` |
| `q = people(pick%())` | `rec_s1 = pick%() : q_name$ = people_name$(rec_s1) : ...` |
| `DIM people(count%()) AS person` | `rec_s1 = count%() : DIM people_name$(rec_s1,30), people_age%(rec_s1), ...` |

Before emitting any of these preceding evaluations, apply rule 26 to the entire evaluation region,
including other arguments. For example, `consume first%(), points(second%())` must not become a
call of `second%()` followed by `first%()`. Nor may a saved variable selector be read ahead of a
remaining call that could change it. Such cases are refused and diagnosed, rather than reordered.

An aliasing regression case uses an `item` type with `slot%` followed by `value`:

```
REFERENCE pickIndex%
DEFine PROCedure pick(pickIndex%, pickedItem AS item)
  pickedItem = items(pickIndex%)
END DEFine

pick target.slot%, target
```

Here `pickIndex%` aliases the destination's first field under the interpreter. Saving it before
copying ensures the second field still comes from the originally selected source element. The
test supplies `REFERENCE pickIndex%` for Turbo as well; `ssbrec` adds the record-field references.

**Whole records**

| Source | S*BASIC |
|---|---|
| `q = p` (type `point`) | `q_x = p_x : q_y = p_y` |
| `q = p` (type with `scores(5)`) | Copy scalar fields and emit an elementwise loop for `scores`, over bounds 0 to 5 |
| `plot p` | `plot p_x, p_y` |
| `plot grid(i,j)` | Capture `i` and `j` first, then pass the selected scalar fields, as above |
| `sortby people, count` | `sortby people_name$, people_age%, ..., count` |

Copies include every logical element of every array field, with a distinct loop variable for each
nested dimension. Generated loops occupy separate lines. A small structured statement parser
expands an enclosing inline `IF`, `FOR` or `REPeat` into an equivalent multiline structure when
needed, so `IF ready THEN q = p ELSE r = p` keeps each complete copy in its original branch.
The translator does not rely on nested inline loops working on the original QL ROM. Constructs
whose structure it cannot preserve are refused. SSB assigns line numbers after this expansion.

**Array fields in reference arguments.** A standalone record passes its scalar fields and complete
field arrays; a whole record array passes its complete backing arrays. However, a single element
of a record array with an array field would need to expose a numeric subarray, for example
`people_scores(i)`. Turbo documents numeric-array slicing as unsupported. The portable baseline
therefore refuses that record-argument form (rule 17), including an embedded record view with the
same problem. Whole-record copies of those elements use indexed loops and remain supported.

A future extension could pass complete backing arrays plus saved prefix indices, but that changes
the callee's field-access layout. Supporting both standalone records and array-element views would
also require layout-specialized routine variants or a uniform representation. This is a separate
design and target experiment, not an assumed property of a sliced array or a copy-in/copy-out call.

**Functions returning records (rules 23 to 26)**

| Source | S*BASIC |
|---|---|
| `DEFine FuNction flip AS point (flipArg AS point)` | `REFERENCE flip_x, flip_y, flipArg_x, flipArg_y` before `DEFine PROCedure flip(flip_x, flip_y, flipArg_x, flipArg_y)`, followed by `flip_x = 0 : flip_y = 0` |
| `flip.x = flipArg.y` (inside it) | `flip_x = flipArg_y` |
| `p = flip(p)` | `flip rec_t1_x, rec_t1_y, p_x, p_y : p_x = rec_t1_x : p_y = rec_t1_y` |
| `PRINT flip(q).x` | `flip rec_t1_x, rec_t1_y, q_x, q_y : PRINT rec_t1_x` |

- The result always goes into a temporary record `rec_tN` and is copied afterwards (rule 24). Writing
  straight into the destination would let the function's writes reach its own arguments, as in
  `p = flip(p)`; S*BASIC's dynamic scope means the function could also read the destination as a
  global, so the copy is kept even when the destination is not an argument.
- The function clears its result fields on entry (rule 23), so a field it does not set is 0 or
  `""`, never left over from an earlier call.
- Every generated preceding evaluation, including selector and dimension captures, follows rule
  26. The check applies to all arguments and the relevant `IF` condition or branch, not just the
  expression containing a record-returning call. Branch-local work remains branch-local. Nested
  record-returning calls and conflicting effectful evaluations require explicit intermediate
  source statements.
- Temporaries `rec_tN` and `rec_sN` are declared `LOCal` in the enclosing routine, on a line inserted
  after its `DEFine` line, so each recursive call has its own; at the top level they are global.
  Result storage is type-correct before the call: bounded strings and array fields are dimensioned
  to the type's resolved field bounds, including in local temporary records. Numeric selector
  temporaries retain a captured value throughout the expansion. Copy-loop variables are also
  distinct generated locals. Generated names and declarations count towards the profile's limits.

**Pointers and pools (rules 28 to 33)**

`DIM nodes(MaxNodes) AS node POOL` becomes:

```
rec_s1 = MaxNodes
IF rec_s1 <> INT(rec_s1) OR rec_s1 < 1 OR rec_s1 > 32767 THEN rec_fail "bad size for pool nodes"
nodes_siz% = rec_s1
DIM nodes_val(nodes_siz%), nodes_left%(nodes_siz%), nodes_right%(nodes_siz%), nodes_nxt%(nodes_siz%)
FOR rec_i1 = 0 TO nodes_siz%
  nodes_val(rec_i1) = 0 : nodes_left%(rec_i1) = 0 : nodes_right%(rec_i1) = 0
  nodes_nxt%(rec_i1) = 0
END FOR rec_i1
FOR rec_i2 = 1 TO nodes_siz% - 1
  nodes_nxt%(rec_i2) = rec_i2 + 1
END FOR rec_i2
nodes_nxt%(nodes_siz%) = 0 : nodes_fre% = 1
```

The size is captured before validation and is not evaluated again for the integer assignment.
An invalid size is diagnosed with the pool name before narrowing it or allocating arrays. Every
data field and `nodes_nxt%` is explicitly cleared before the free list is linked, as shown above.
Executing the pool declaration again reinitializes the pool and invalidates every outstanding
handle into it; this is separate from freeing a single record and must be documented at the call
site if used. No old allocation state or handle validity survives reinitialization.

The validated size is held in `nodes_siz%` (rule 19). `nodes_nxt%` links free records into a list
starting at `nodes_fre%`; a record in use holds -1 there, which is how a double free is detected.
These routines are written at the end of the output:

```
DEFine FuNction nodes_new%
  LOCal rec_n%
  IF nodes_fre% = 0 THEN rec_fail "pool nodes full"
  rec_n% = nodes_fre% : nodes_fre% = nodes_nxt%(rec_n%) : nodes_nxt%(rec_n%) = -1
  nodes_val(rec_n%) = 0 : nodes_left%(rec_n%) = 0 : nodes_right%(rec_n%) = 0
  RETurn rec_n%
END DEFine

DEFine PROCedure nodes_free(rec_n%)
  IF rec_n% = 0 THEN RETurn
  IF rec_n% < 1 OR rec_n% > nodes_siz% THEN rec_fail "bad record number for pool nodes"
  IF nodes_nxt%(rec_n%) = -1 THEN
    nodes_nxt%(rec_n%) = nodes_fre% : nodes_fre% = rec_n%
  ELSE
    rec_fail "record freed twice in pool nodes"
  END IF
END DEFine

DEFine PROCedure rec_fail(rec_m$)
  PRINT "Records: "; rec_m$
  STOP
END DEFine
```

A `VAL` parameter loses its marker in the `DEFine` line, and every argument passed to it is wrapped
in parentheses, which the Sinclair manual documents as making S*BASIC pass a value. Turbo already
uses value passing for an ordinary scalar parameter unless it is marked `REFERENCE`; `ssbrec`
ensures the generated/merged directives leave `VAL` parameters unmarked. Ordinary non-record
parameters otherwise retain their existing S*BASIC/Turbo conventions; this extension does not
silently turn every ordinary parameter into a reference parameter.

| Source | S*BASIC |
|---|---|
| `DEFine FuNction insert%(VAL n%, v)` | `DEFine FuNction insert%(n%, v)` |
| `insert%(nodes(n%).left%, v)` | `insert%((nodes_left%(n%)), v)` |

### 13. Processing

`ssbrec` works in two passes, like SSB.

**Pass one** reads the program, applying `#INCLUDE`, `#DEFINE` and `#IFDEF` as SSB does and skipping
inactive blocks entirely, and builds:

- the **type table**: each type's fields with kind, string length, array dimensions and record type,
  and the generated shape of each field in an array of records (rule 10);
- the **signature table**: each procedure and function with its record, record-array and `VAL`
  parameters, expected ranks, existing `REFERENCE` entries, generated reference shapes, and the
  type a function returns;
- the **record table**: each record name with its one type and rank (rule 11), and pools with
  their sizes; the type table resolves all field-layout constants before generation;
- the **name table**: every name the program uses and every name `ssbrec` will generate, to find
  collisions of either kind (rule 34), distinguishing intentional redeclarations of the same
  record/field from different paths that accidentally produce the same name;
- the **ordinary array table**: arrays declared by `DIM`, `LOCal` and `REFERENCE`, including array
  parameters, with enough shape information to distinguish subscripts from function calls.

It reports undefined types, cycles in embedded-record fields, invalid field-layout constants,
duplicate fields, a record name declared with different types or ranks, shapes over the profile's
dimension limit and name collisions, with file and line. It calculates generated parameter and
local counts, including hidden results, selectors and loop variables, before accepting a routine.

**Pass two** rewrites the active code a statement at a time and writes the `_ssb` file: includes
expanded, conditional directives removed, labels, `##` comment lines and directives SSB still needs
copied unchanged. Tokens and a small structured representation of statements and relevant
expressions preserve branch boundaries, argument order and enclosing loops during expansion;
this is not blind text replacement. Original source file and line accompany generated lines in
a side mapping or diagnostic listing so errors can be related back to the `_ssr` source.

**The scanner** splits a line into tokens and never rewrites inside:

- strings in `"` or `'`;
- the rest of a line after `REMark`, `**` or `##`;
- `DATA` statements (their items are constants);
- numbers, including `.5`, `1.5E3`, and `$` hexadecimal and `%` binary constants.

A line ending in `\+` is joined with the next before scanning, as SSB does. A field reference is a
name, an optional subscript list, then `.name` repeated; subscripts are matched by counting
parentheses, so `people(f(i)+1).home.x` resolves correctly. A name followed by `(` is an array if
pass one established it as one, including through a formal parameter or `REFERENCE` declaration;
otherwise it is treated as a potentially effectful function call. Known no-argument functions
must also be recognized when written without parentheses.

**Built-ins without side effects** (rules 19 and 26): `ABS`, `ACOS`, `ACOT`, `ASIN`, `ATAN`, `CHR$`,
`CODE`, `COS`, `COT`, `DEG`, `DIMN`, `EXP`, `FILL$`, `INT`, `LEN`, `LN`, `LOG10`, `PI`, `RAD`, `SIN`,
`SQRT`, `TAN`. Any other call, including toolkit functions such as `VAL`, counts as possibly having
side effects. This list is used only by the ordering checks; it does not make a selector safe to
read repeatedly or ahead of a call that could change its inputs. Unknown extension names are
conservative, and user routines shadowing a built-in do not inherit the built-in's exemption.

**Skipping ordinary lines that can't involve records.** Scanning a line character by character is
slow in S*BASIC, and most lines of a typical program never touch a record. First handle type blocks,
declarations, reference directives, routine boundaries and control-structure bookkeeping. Type
blocks are removed even when their names never match a record variable. Only then may a quick
test with the built-in `INSTR` decide whether an ordinary line needs record rewriting:

```
needs = "." INSTR in$                     ## a field reference always has a dot
IF NOT needs THEN
  FOR k = 1 TO rec_count                  ## record and generated routine names from pass one
    IF rec_name$(k) INSTR in$ THEN needs = 1 : EXIT k
  END FOR k
END IF
IF needs THEN in$ = doRecords$(in$)
```

- The test may report a line that has no record in it, but must never miss one that has.
  A false alarm only costs a scan that changes nothing: the dot in `1.5` or in a sentence inside a
  string triggers the scan, which then finds no field reference.
- Field references always contain a dot. Uses of a whole record (`q = p`, `plot p`) don't, which
  is why the record names are tested too. Record-returning functions, `VAL`-parameter routines and
  pool routines are added to the same list.
- `INSTR` finds substrings and ignores case, so a short or common record name (`p`, `rec`) matches
  most lines (`PRINT`, `PAPER`, `STOP`...) and the test stops saving time. `ssbrec` warns about
  such names (rule 36); the program is still converted correctly.
- This skipping optimization applies to pass-two rewriting only. Pass one must still discover
  all active names, declarations, routine signatures and array shapes. Substring checks may
  accelerate individual recognizers, but may not skip a `VAL`-only routine, an ordinary array
  declaration, a `REFERENCE` entry or an ordinary identifier needed for collision checking.
- Pass two always tracks routine and control-structure boundaries before this test; a line with
  no record name can still open or close the context needed to lower a later line correctly.

**Errors** name the source file and line, and the record, field or routine involved: unknown field,
wrong type for a record argument, whole-record use where only fields are allowed (rule 18), `FOR`
over a field of an array of records (rule 15), an array-field record view used as a reference
argument (rule 17), a statement whose evaluation order cannot be preserved within rule 26,
name collisions (rule 34), conflicting `VAL`/`REFERENCE` declarations, and profile or table limits.

### 14. Testing

All testing is on a QL or Q-emuLator, with `ssbrec`, SSB and Turbo:

- **Translation tests**: for each rule, a small `_ssr` file and the exact `_ssb` expected; a test
  program runs `ssbrec` on each and compares the output.
- **Program tests**: whole programs run interpreted and compiled with Turbo, with their output
  compared: records of each field kind, arrays of records, nested records, whole-record copy,
  record parameters, record-returning functions including recursion, a linked list and the tree
  above.
- **Tests for the review findings** (section 16): `p = flip(p)` gives `(2,1)`; `plot
  points(next%())` calls `next%()` once; `p.home_x` beside `home AS point` is refused; a `TYPE` in
  an inactive `#IFDEF` block is ignored; a helper routine using its caller's `LOCal` record; a
  statement mixing a record-returning call with `RND` is refused; pool full, bad index, double
  free and `name_free 0`.
- **Calling-convention tests:** generated record arguments and hidden results update the caller
  under Turbo as well as interpreted; cover numeric fields, bounded strings and complete array
  fields. Verify correct dummy ranks in `REFERENCE`, merging of user directives, a `VAL` argument
  leaving the caller unchanged, and rejection of a conflicting directive. Caller result strings
  and arrays must be correctly allocated before the generated call.
- **Aliasing and ordering tests:** the `pick` example in section 12, with a reference index
  aliasing the destination's first field, copies every field from one source record. Test both
  source and destination selectors, and selectors that are plain variables or pure expressions.
  Refuse both `consume first%(), points(second%())` and `consume advance%(), points(i)` when
  captures would move ahead of their other call. Nested record-returning calls are refused.
  A record-returning call in an unselected `IF` branch is never executed.
- **Initialization tests:** dimension, populate, grow and shrink an array of records and verify
  that every resulting numeric and string element is cleared. Repeat for types with array fields,
  local records in repeated/recursive calls, partial function results and reinitialized pools.
- **Array-field and structure tests:** standalone records and whole record arrays pass their
  complete arrays by reference; passing a record-array element with an array field is refused.
  Copies of those elements remain valid. Copy two-dimensional array fields inside inline `IF`,
  `FOR` and `REPeat` constructs and check the emitted multiline nesting and execution results.
- **Profile tests:** deliberate dimension/rank mismatches, invalid constant bounds, name and line
  limits, and routines whose expanded parameter/local counts exceed the selected profile produce
  diagnostics. Repeated compatible declarations of one logical name remain valid. Different-type
  examples use distinct names, as required by rule 11.
- **Fast-path tests:** use long, distinctive record names so type headers and ordinary structure
  boundaries do not accidentally match the candidate list. Type blocks must still disappear,
  `VAL`-only signatures and ordinary-name collisions must still be found, and captures must be
  inserted into the correct routine and branch with the shortcut enabled or disabled.

### 15. Target experiments before implementation

First establish the representation and calling convention with small, hand-written S*BASIC
programs. Run them interpreted and with the actual Turbo/toolkit versions intended for release,
before building the translator around their behaviour. Record the ROM, SMSQ/E, Turbo and toolkit
versions and the results in named compatibility profiles; no results are claimed by this proposal.

**Documented constraints to design around** (Turbo manual pack `manuals.zip`):

- `turbos3.txt`, chapter P: scalar parameters default to value passing within a compiled program;
  `REFERENCE` supplies reference parameters and array ranks. Generated record parameters and
  hidden results therefore require directives, not just `DEFine` parameter lists. The supplied
  `parser502_bas`, routine `By_Reference%`, confirms this distinction.
- `turboref.txt`, `ARRAY OPERATION NOT ALLOWED`: numeric-array slicing is unsupported. The portable
  baseline rejects passing array-field record views; full-array-plus-index layouts are a possible
  future extension, not a feature already established by the manual.
- `turbos3.txt`, chapter K, `Rubber arrays`: some changes of array size preserve old contents.
  Record allocation therefore includes explicit clearing to guarantee rule 8.
- `turbos3.txt`, chapter P, warns of interpreter failures with more than nine locals or parameters
  on affected ROMs. The illustrated `swap` expands to ten field parameters plus local storage.
  This is a target-version constraint to measure, not a new limit to assume for all QLs.
- `turbos4.txt`, `MANIFEST`: string constants are documented, for example
  `MANIFEST : open$ = "Open the door"`. Their existence is settled; target behaviour and toolkit
  availability still need checking.

**Experiments that establish a supported profile:**

- Modify numeric record fields through generated `REFERENCE` parameters; return records through
  hidden reference outputs; repeat with caller/result aliasing and recursive calls.
- Establish the exact reference declarations and writable argument forms for bounded string
  fields, string fields selected from record arrays, complete numeric/string array fields, and
  complete backing arrays of whole record arrays. Check their type and dummy-rank requirements.
- Verify parentheses force value passing in the interpreter and that `VAL` parameters remain
  value parameters under Turbo when other parameters of the same routine are references. Confirm
  the interpreter recognizes generated `REFERENCE` directives with the selected toolkit loaded.
- Establish that generated local arrays and bounded strings, including result temporaries, have
  the required allocation, clearing and lifetime on repeated and recursive calls. Verify dynamic
  visibility of a caller's local record fields under each supported compiled/interpreted target.
- Grow and shrink record backing arrays and confirm explicit clearing eliminates Turbo's
  preservation of old values; repeat with string arrays and array fields.
- Measure supported dimension counts, array sizes, string capacities, identifier and source-line
  lengths, and expanded parameter/local counts. Reject generated programs outside those bounds.
  Check generated names also retain their intended field kinds in the presence of source
  `IMPLICIT` directives; diagnose a profile that cannot preserve them.
- Verify multiline copies and clearing loops, including multiple dimensions, preserve inline
  branch/loop execution after structural expansion. Do not depend on nested inline loops.
- Check interpreted `MANIFEST` assignments with the toolkit loaded, and diagnostics/documented
  startup requirements when it is absent. Plain literals/variables remain valid pool sizes.
- Verify the case-insensitive candidate test based on `INSTR` for each target. A profile must
  disable that optimization or normalize the candidate strings if the test could miss a name.

If a required representation or reference form fails these experiments, narrow the advertised
profile or revise the representation before implementation. A refused feature is preferable to
generated code with different reference or initialization semantics.

### 16. Changes after review

| Finding | Resolution |
|---|---|
| Returning a record straight into its destination can alias the arguments (`p = flip(p)` would give `(2,2)`) | Results go into a temporary and are copied afterwards; result fields start at zero (rules 23, 24; section 12) |
| Subscripts could be evaluated once per field (`plot points(next%())`); even a plain index can change through an alias | All non-constant selectors and outer `DIM` sizes are captured before expansion; only compile-time constants may be reused (rule 19) |
| Moving selector or dimension evaluations can reorder other calls, even without a record-returning function | Ordering checks cover every generated preceding evaluation and all other arguments, including calls that could change a saved variable selector; unsafe cases are refused (rule 26) |
| Wrapping a `VAL` argument in parentheses relies on S*BASIC passing parenthesised arguments by value | Documented interpreter behaviour, kept; generated/merged Turbo directives exclude `VAL` parameters and conflicts are diagnosed (section 12); target tests remain required |
| Turbo defaults scalar parameters to value passing | Generate `REFERENCE` for every flattened record and hidden-result parameter, with dummy array ranks and allocated result storage (section 12) |
| Passing one record-array element with an array field would need an unsupported numeric subarray | The portable baseline refuses this argument form while retaining indexed field access and whole-record copies; future full-backing-array layouts require a separate design (rules 17, 21; section 12) |
| Turbo can preserve old data when an array is resized | Explicit clearing follows every record allocation/redimensioning, including numeric/string array fields (rule 8; section 12) |
| `p.home_x` and `p.home.x` both generate `p_home_x` | All collisions are refused, between generated names as well (rule 34) |
| Inactive `#IFDEF` blocks could introduce types and declarations | `ssbrec` applies SSB's conditional and include rules before collecting anything (rule 1; section 10) |
| S*BASIC locals are dynamically visible; textual scope alone cannot fix a record's layout | Records follow dynamic scope; each record name has one type and rank everywhere. Examples use distinct names; compatible redeclarations intentionally share generated names (rules 11, 34) |
| The array limit was argued wrongly and not grounded in the manual | The generated shape is computed in full, and the limit is measured on each target (rule 10; section 15) |
| Pools need safety rules | Full, bad index and double free are checked; `name_free 0` is allowed; use after free is stated as undetected; capacity is 1 to 32767 (rules 29, 30, 33) |
| Rule 31 appeared to forbid the embedded records allowed by rule 5 | Only cycles in the embedded-record graph are forbidden; integer pointer fields provide recursive links (rule 31) |
| Inline copy expansion could require nested inline loops on old ROMs | Generated loops use separate lines and enclosing inline structures are expanded without changing branch membership (section 12) |
| Scalar expansion can exceed an older interpreter's parameter/local limits | Count generated parameters and locals and validate them against measured target profiles before generation (sections 13, 15) |
| The manuals already document string `MANIFEST` constants | Record that fact and retain target/toolkit tests instead of treating language support as an unanswered question (section 15) |
