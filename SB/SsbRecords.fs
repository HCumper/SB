module SsbRecords

// SSB 2.8.0's record syntax is a source transformation, not a new runtime type.
// Keep this pass ahead of label numbering so every backend sees ordinary BASIC.
open System
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions

type SourceLine = { Text: string; File: string; Number: int }
type private Field = { Name: string; Bounds: string list; Embedded: string option }
type private Leaf = { Path: string list; Bounds: int list }
type private Storage = { Name: string; TypeName: string; Rank: int; Pool: bool }
type private Declaration = { Name: string; Bounds: string list; TypeName: string option; Pool: bool; Value: bool }
type private Parameter = { Declaration: Declaration; Record: Storage option }
type private Routine = { Name: string; IsFunction: bool; Parameters: Parameter list; mutable References: string list; Temps: ResizeArray<string>; mutable LocalCount: int }
type private View = { Storage: Storage; TypeName: string; Path: string list; Indices: string list; WholeArray: bool }

let private eq (a: string) (b: string) = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)
let private upper (s: string) = s.ToUpperInvariant()
let private ident (s: string) = Regex.IsMatch(s, @"^[A-Za-z_][A-Za-z0-9_]*[$%]?$")
let private text (tokens: string list) = String.concat " " tokens
let private lexer = Regex("\"(?:[^\"]|\"\")*\"|'(?:[^']|'')*'|@[A-Za-z_][A-Za-z0-9_]*|[A-Za-z_][A-Za-z0-9_]*[$%]?|\\$[0-9A-Fa-f]+|%[01]+|(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[Ee][+-]?[0-9]+)?|<>|<=|>=|==|&&|\\|\\||[^\\s]", RegexOptions.Compiled)
let tokenize (line: string) = lexer.Matches(line) |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.toList

let private split separator tokens =
    let result = ResizeArray<string list>()
    let current = ResizeArray<string>()
    let mutable depth = 0
    for token in tokens do
        if depth = 0 && eq token separator then
            result.Add(List.ofSeq current)
            current.Clear()
        else
            current.Add token
            if token = "(" then depth <- depth + 1
            elif token = ")" then depth <- depth - 1
            if depth < 0 then invalidOp "Unbalanced parentheses"
    if depth <> 0 then invalidOp "Unbalanced parentheses"
    result.Add(List.ofSeq current)
    List.ofSeq result

let private group tokens =
    match tokens with
    | "(" :: rest ->
        let inside = ResizeArray<string>()
        let mutable depth = 1
        let mutable tail = rest
        while depth > 0 do
            match tail with
            | [] -> invalidOp "Unbalanced parentheses"
            | head :: next ->
                tail <- next
                if head = "(" then depth <- depth + 1
                elif head = ")" then depth <- depth - 1
                if depth > 0 then inside.Add head
        List.ofSeq inside, tail
    | _ -> invalidOp "Expected parentheses"

let private parseDeclaration allowValue tokens =
    let value, tokens =
        match tokens with
        | v :: rest when eq v "VAL" -> true, rest
        | _ -> false, tokens
    if value && not allowValue then invalidOp "VAL is only valid on scalar parameters"
    match tokens with
    | name :: rest when ident name ->
        let bounds, rest =
            match rest with
            | "(" :: _ ->
                let inner, tail = group rest
                split "," inner |> List.map text, tail
            | _ -> [], rest
        let typeName, pool =
            match rest with
            | [] -> None, false
            | [ a; t ] when eq a "AS" && ident t -> Some t, false
            | [ a; t; p ] when eq a "AS" && ident t && eq p "POOL" -> Some t, true
            | _ -> invalidOp "Malformed record declaration or routine result annotations"
        if name.Length > 64 then invalidOp "Declared name exceeds 64 bytes"
        if value && (typeName.IsSome || not bounds.IsEmpty) then invalidOp "VAL supports scalar parameters only"
        { Name = name; Bounds = bounds; TypeName = typeName; Pool = pool; Value = value }
    | _ -> invalidOp "Expected a declaration name"

let private header tokens =
    match tokens with
    | d :: kind :: name :: tail when (eq d "DEF" || eq d "DEFINE") && ident name ->
        let isFunction = eq kind "FN" || eq kind "FUNCTION"
        if not (isFunction || eq kind "PROC" || eq kind "PROCEDURE") then None
        else
            let parameters =
                match tail with
                | [] -> []
                | "(" :: _ ->
                    let args, remaining = group tail
                    if not remaining.IsEmpty then invalidOp "Routine result annotations are not supported"
                    if args.IsEmpty then [] else split "," args |> List.map (parseDeclaration true)
                | _ -> invalidOp "Routine result annotations are not supported"
            Some(name, isFunction, parameters)
    | _ -> None

let private keyword (token: string) (full: string) minimum =
    token.Length >= minimum && token.Length <= full.Length && full.StartsWith(upper token, StringComparison.Ordinal)

let private isRemark tokens =
    match tokens with
    | name :: _ -> keyword name "REMARK" 3 || eq name "DATA"
    | [] -> false

let expand (lines: SourceLine list) : SourceLine list =
    let tokens = lines |> List.map (fun line -> line, tokenize line.Text)
    let extended =
        tokens |> List.exists (fun (_, ts) ->
            not (isRemark ts) &&
            (match ts with
             | first :: _ when eq first "TYPE" -> true
             | d :: _ when eq d "DIM" || keyword d "LOCAL" 3 -> ts |> List.exists (eq "AS")
             | d :: _ when eq d "DEF" || eq d "DEFINE" -> ts |> List.exists (fun t -> eq t "AS" || eq t "VAL")
             | _ -> false))
    if not extended then lines
    else
        let types = Dictionary<string, Field list>(StringComparer.OrdinalIgnoreCase)
        let layouts = Dictionary<string, Leaf list>(StringComparer.OrdinalIgnoreCase)
        let records = Dictionary<string, Storage>(StringComparer.OrdinalIgnoreCase)
        let routines = Dictionary<string, Routine>(StringComparer.OrdinalIgnoreCase)
        let constants = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
        let arrays = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let typeLines = HashSet<int>()
        let declarations = Dictionary<int, Declaration list>()
        let headers = Dictionary<int, Routine>()
        let ordinary = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let pendingReferences = ResizeArray<string>()
        let maxItems =
            match Environment.GetEnvironmentVariable("SSBRECITEMS") with
            | null | "" -> 256
            | value ->
                match Int32.TryParse value with
                | true, n when n >= 32 && n <= 1024 -> n
                | _ -> invalidOp "SSBRECITEMS must be an integer from 32 to 1024"
        let profile =
            match Environment.GetEnvironmentVariable("SSBRECPROFILE") with
            | null | "" -> "TURBO509"
            | value when eq value "TURBO509" || eq value "SBASIC" -> upper value
            | _ -> invalidOp "SSBRECPROFILE must be TURBO509 or SBASIC"
        let mutable origin = List.head lines
        let fail message = invalidOp $"{origin.File}:{origin.Number}: {message}"
        let at line f =
            origin <- line
            try f () with
            | :? InvalidOperationException as error when not (error.Message.StartsWith(line.File + ":", StringComparison.Ordinal)) -> fail error.Message
        let addRecord (decl: Declaration) formal =
            match decl.TypeName with
            | None ->
                ordinary.Add decl.Name |> ignore
                if not decl.Bounds.IsEmpty then arrays.Add decl.Name |> ignore
                None
            | Some typeName ->
                if decl.Name.EndsWith("$") || decl.Name.EndsWith("%") then fail "Record names cannot have scalar suffixes"
                if formal && decl.Bounds |> List.exists (fun b -> b <> "") then fail "Record array parameters use empty dimensions"
                if decl.Pool && (formal || decl.Bounds.Length <> 1) then fail "POOL requires a one-dimensional global DIM"
                let storage = { Name = decl.Name; TypeName = typeName; Rank = decl.Bounds.Length; Pool = decl.Pool }
                match records.TryGetValue decl.Name with
                | true, existing when not (eq existing.TypeName typeName) || existing.Rank <> storage.Rank || (not formal && existing.Pool <> storage.Pool) -> fail "Conflicting record type/rank"
                | true, _ -> ()
                | _ -> records.Add(decl.Name, storage)
                if records.Count > maxItems then fail "Record table capacity exceeded"
                Some storage
        // Collect constants before layouts, allowing forward TYPE and MANIFEST references.
        for line, ts in tokens do
            at line (fun () ->
                match ts with
                | m :: tail when eq m "MANIFEST" ->
                    for item in split ":" tail do
                        match item with
                        | [ name; "="; number ] ->
                            match Int32.TryParse(number, NumberStyles.None, CultureInfo.InvariantCulture) with
                            | true, n -> constants[name] <- n
                            | _ -> ()
                        | _ -> ()
                | _ -> ())
        let mutable currentType: (string * ResizeArray<Field>) option = None
        let mutable inRoutine = false
        let mutable collectingRoutine: Routine option = None
        for index, (line, ts) in tokens |> List.indexed do
            at line (fun () ->
                if ts.Length > 256 then fail "Source line exceeds 256 lexical tokens"
                match currentType, ts with
                | Some(name, fields), [ e; t ] when eq e "END" && eq t "TYPE" ->
                    if fields.Count = 0 then fail "TYPE must declare a field"
                    types.Add(name, List.ofSeq fields)
                    if types.Count > maxItems then fail "TYPE table capacity exceeded"
                    typeLines.Add index |> ignore
                    currentType <- None
                | Some _, [] -> typeLines.Add index |> ignore
                | Some _, _ when isRemark ts -> typeLines.Add index |> ignore
                | Some(_, fields), _ ->
                    let decl = parseDeclaration false ts
                    if decl.Pool || (decl.TypeName.IsSome && not decl.Bounds.IsEmpty) then fail "Embedded records must be scalar"
                    if fields |> Seq.exists (fun f -> eq f.Name decl.Name) then fail "Duplicate field"
                    fields.Add { Name = decl.Name; Bounds = decl.Bounds; Embedded = decl.TypeName }
                    typeLines.Add index |> ignore
                | None, [ t; name ] when eq t "TYPE" && ident name ->
                    if inRoutine then fail "TYPE declarations must be top-level"
                    if name.Length > 64 then fail "TYPE name exceeds 64 bytes"
                    if types.ContainsKey name then fail "Duplicate TYPE"
                    currentType <- Some(name, ResizeArray())
                    typeLines.Add index |> ignore
                | None, _ when isRemark ts -> ()
                | None, _ ->
                    match header ts with
                    | Some(name, isFunction, parms) ->
                        if inRoutine || routines.ContainsKey name then fail "Nested or duplicate routine"
                        let parameters = parms |> List.map (fun decl -> { Declaration = decl; Record = addRecord decl true })
                        let routine = { Name = name; IsFunction = isFunction; Parameters = parameters; References = List.ofSeq pendingReferences; Temps = ResizeArray(); LocalCount = 0 }
                        pendingReferences.Clear()
                        routines.Add(name, routine)
                        headers.Add(index, routine)
                        inRoutine <- true
                        collectingRoutine <- Some routine
                    | None ->
                        match ts with
                        | r :: tail when eq r "REFERENCE" ->
                            for item in split "," tail do
                                match item with
                                | name :: _ when ident name ->
                                    match collectingRoutine with
                                    | Some r -> r.References <- r.References @ [name]
                                    | None -> pendingReferences.Add name
                                | _ -> fail "Malformed REFERENCE directive"
                        | d :: tail when eq d "DIM" || keyword d "LOCAL" 3 ->
                            let ds = split "," tail |> List.map (parseDeclaration false)
                            for decl in ds do
                                if decl.Pool && (inRoutine || not (eq d "DIM")) then fail "POOL requires a top-level global DIM"
                                addRecord decl false |> ignore
                            declarations.Add(index, ds)
                        | e :: d :: _ when eq e "END" && (eq d "DEF" || eq d "DEFINE") ->
                            inRoutine <- false
                            collectingRoutine <- None
                        | _ -> ())
        if currentType.IsSome then fail "Missing END TYPE"
        let bound (value: string) =
            match Int32.TryParse(value, NumberStyles.None, CultureInfo.InvariantCulture) with
            | true, n when n >= 0 && n <= 32767 -> n
            | _ ->
                match constants.TryGetValue value with
                | true, n when n >= 0 && n <= 32767 -> n
                | _ -> fail "Field bounds require nonnegative integer literals or numeric MANIFEST constants"
        let rec layout visiting name =
            match layouts.TryGetValue name with
            | true, leaves -> leaves
            | _ ->
                if visiting |> List.exists (eq name) then fail "Recursive embedded TYPE"
                if not (types.ContainsKey name) then fail $"Unknown TYPE: {name}"
                let leaves =
                    types[name] |> List.collect (fun field ->
                        match field.Embedded with
                        | Some child -> layout (name :: visiting) child |> List.map (fun leaf -> { leaf with Path = field.Name :: leaf.Path })
                        | None ->
                            let bs = List.map bound field.Bounds
                            if field.Name.EndsWith("$") && (bs.IsEmpty || List.last bs = 0) then fail "String fields require positive capacity"
                            if bs.Length > 8 then fail "Physical array rank exceeds profile"
                            [ { Path = [field.Name]; Bounds = bs } ])
                let generated = HashSet<string>(StringComparer.OrdinalIgnoreCase)
                for leaf in leaves do
                    let path = String.concat "_" leaf.Path
                    if path.Length > 160 then fail "Flattened path exceeds 160 bytes"
                    if not (generated.Add path) then fail "Generated paths collide"
                layouts.Add(name, leaves)
                leaves
        for name in types.Keys do layout [] name |> ignore
        if layouts.Values |> Seq.sumBy List.length > maxItems then fail "Flattened layout table capacity exceeded"
        let leavesFor (storage: Storage) = layout [] storage.TypeName
        let backing (storage: Storage) (path: string list) = storage.Name + "_" + String.concat "_" path
        for storage in records.Values do
            if ordinary.Contains storage.Name then fail "Record name also used for ordinary storage"
            for leaf in leavesFor storage do
                if storage.Rank + leaf.Bounds.Length > 8 then fail "Physical array rank exceeds profile"
                if (backing storage leaf.Path).Length > 160 then fail "Generated identifier exceeds 160 bytes"
        // Check identifiers lexically: quoted text and DATA contents are not names.
        let generatedNames = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        for storage in records.Values do
            for leaf in leavesFor storage do
                if not (generatedNames.Add(backing storage leaf.Path)) then fail "Generated name collides"
        for index, (line, ts) in tokens |> List.indexed do
            if not (typeLines.Contains index) && not (isRemark ts) then
                at line (fun () ->
                    for token in ts do
                        if ident token then
                            if token.StartsWith("rec_", StringComparison.OrdinalIgnoreCase) then fail "Reserved name: rec_"
                            if generatedNames.Contains token then fail $"Generated name collides: {token}")
        let pools = records.Values |> Seq.filter (fun r -> r.Pool) |> Seq.toList
        for pool in pools do
            for suffix in ["_new%"; "_free"] do
                if ordinary.Contains(pool.Name + suffix) || routines.ContainsKey(pool.Name + suffix) then fail "Pool helper collides"
            for line, ts in tokens do
                match ts with
                | name :: "=" :: _ when eq name (pool.Name + "_new%") || eq name (pool.Name + "_free") -> at line (fun () -> fail "Pool helper collides")
                | _ -> ()
        let mutable routine: Routine option = None
        let mutable serial = 0
        let newTempWith suffix =
            serial <- serial + 1
            let name = $"rec_t{serial}" + suffix
            match routine with
            | Some r when not (r.Temps.Contains name) -> r.Temps.Add name
            | Some _ -> ()
            | None -> ()
            name
        let newTemp () = newTempWith "%"
        let generatedLine value = { origin with Text = value }
        let dimension name indices = if List.isEmpty indices then name else name + "(" + String.concat "," indices + ")"
        let logicalBounds (leaf: Leaf) = if (List.last leaf.Path).EndsWith("$") then leaf.Bounds |> List.take (leaf.Bounds.Length - 1) else leaf.Bounds
        let zero (leaf: Leaf) = if (List.last leaf.Path).EndsWith("$") then "\"\"" else "0"
        let pureBuiltins = set ["ABS";"ACOS";"ACOT";"ASIN";"ATAN";"CHR$";"CODE";"COS";"COT";"DEG";"DIMN";"EXP";"FILL$";"INT";"LEN";"LN";"LOG10";"PI";"RAD";"SIN";"SQRT";"TAN"]
        let effectful ts =
            ts |> List.indexed |> List.exists (fun (i, token) ->
                if not (ident token) then false
                elif eq token "RND" || eq token "DATE" || eq token "INKEY$" then true
                elif routines.ContainsKey token then routines[token].IsFunction
                elif pools |> List.exists (fun p -> eq token (p.Name + "_new%")) then true
                elif i + 1 < ts.Length && ts[i+1] = "(" then
                    not (pureBuiltins.Contains(upper token) || arrays.Contains token || records.ContainsKey token)
                else false)
        let capture (prefix: ResizeArray<string>) (expression: string) =
            let isConstant =
                match Int32.TryParse(expression, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                | true, _ -> true
                | _ -> constants.ContainsKey expression
            if isConstant then expression
            else
                // Retain fractional values until bounds validation. An integer
                // temporary here would silently accept an invalid DIM bound.
                let temp = newTempWith ""
                prefix.Add(temp + " = " + expression)
                temp
        let rec tryView ts =
            match ts with
            | name :: tail when records.ContainsKey name ->
                let storage = records[name]
                let indices, tail =
                    match tail with
                    | "(" :: _ ->
                        let inner, rest = group tail
                        let indices = if inner.IsEmpty then [] else split "," inner |> List.map text
                        indices, rest
                    | _ -> [], tail
                if not indices.IsEmpty && indices.Length <> storage.Rank then fail "Record selector rank mismatch"
                let mutable typeName = storage.TypeName
                let mutable path = []
                let mutable remaining = tail
                let mutable scanning = true
                while scanning do
                    match remaining with
                    | "." :: field :: rest ->
                        let declaration = types[typeName] |> List.tryFind (fun f -> eq f.Name field)
                        match declaration with
                        | None -> fail $"Unknown field: {field}"
                        | Some { Embedded = Some nested } ->
                            path <- path @ [field]
                            typeName <- nested
                            remaining <- rest
                        | Some _ -> scanning <- false
                    | _ -> scanning <- false
                Some({ Storage = storage; TypeName = typeName; Path = path; Indices = indices; WholeArray = storage.Rank > 0 && indices.IsEmpty }, remaining)
            | _ -> None
        let viewLeaves (view: View) = layout [] view.TypeName
        let captureView prefix (view: View) = { view with Indices = view.Indices |> List.map (capture prefix) }
        let access (view: View) (leaf: Leaf) extra = dimension (backing view.Storage (view.Path @ leaf.Path)) (view.Indices @ extra)
        let loops bounds body =
            let counters = bounds |> List.map (fun _ -> newTemp ())
            (List.map2 (fun counter b -> $"FOR {counter} = 0 TO {b}") counters bounds) @ body counters @ (counters |> List.rev |> List.map (fun counter -> "END FOR " + counter))
        let guard condition message = [ "IF " + condition + " THEN"; "PRINT \"" + message + "\""; "STOP"; "END IF" ]
        // Lower SELECT to guarded branches: SB's older SELECT lowering doesn't
        // yet model a REMAINDER clause or exact compact-clause equality.
        let selects = Stack<string * int ref * bool ref * string>()
        let rec stringExpression ts =
            if ts |> List.exists (eq "&") then true
            else
                match ts with
                | "(" :: _ -> let inner, _ = group ts in stringExpression inner
                | token :: _ when token.StartsWith("\"") || token.StartsWith("'") || token.EndsWith("$") -> true
                | _ ->
                    match tryView ts with
                    | Some(_, "." :: field :: _) -> field.EndsWith("$")
                    | _ -> false
        let effectfulOutsideRecordCalls ts =
            let rec outside remaining =
                match remaining with
                | name :: "(" :: _ when routines.ContainsKey name && routines[name].Parameters |> List.exists (fun p -> p.Record.IsSome) ->
                    let _, tail = group remaining.Tail
                    outside tail
                | token :: tail -> token :: outside tail
                | [] -> []
            effectful (outside ts)
        let rec expression (prefix: ResizeArray<string>) (ts: string list) =
            let output = ResizeArray<string>()
            let mutable tail = ts
            while not tail.IsEmpty do
                match tail with
                | name :: "(" :: _ when routines.ContainsKey name ->
                    let args, rest = group (List.tail tail)
                    let values = if args.IsEmpty then [] else split "," args
                    let expanded = callArguments prefix routines[name] values true
                    if not rest.IsEmpty && rest.Head = "." then fail "Functions cannot return records"
                    output.Add(name + "(" + String.concat "," expanded + ")")
                    tail <- rest
                | _ ->
                    match tryView tail with
                    | Some(view, "." :: field :: rest) ->
                        if view.WholeArray then fail "Select one record before accessing a field"
                        let leaf = viewLeaves view |> List.tryFind (fun l -> l.Path.Length = 1 && eq l.Path.Head field)
                        match leaf with
                        | None -> fail $"Unknown field: {field}"
                        | Some leaf ->
                            let fieldIndices, remaining =
                                match rest with
                                | "(" :: _ ->
                                    let inner, remaining = group rest
                                    let indices = split "," inner |> List.map (expression prefix)
                                    indices, remaining
                                | _ -> [], rest
                            let indices = view.Indices |> List.map (tokenize >> expression prefix)
                            if not remaining.IsEmpty && remaining.Head = "." then fail "Unknown field after a scalar field"
                            output.Add(access { view with Indices = indices } leaf fieldIndices)
                            tail <- remaining
                    | Some _ -> fail "Whole records are only valid in copies or typed routine arguments"
                    | None ->
                        let token = tail.Head
                        if Regex.IsMatch(token, "^%[01]+$") then output.Add(string (Convert.ToInt32(token.Substring(1), 2)))
                        else output.Add token
                        tail <- tail.Tail
            text (List.ofSeq output)
        and callArguments (prefix: ResizeArray<string>) (target: Routine) arguments embedded =
            if arguments.Length <> target.Parameters.Length then fail $"Argument count mismatch for {target.Name}"
            let selected =
                List.zip target.Parameters arguments |> List.exists (fun (p, arg) ->
                    p.Record.IsSome && (match tryView arg with Some(v, []) -> not v.Indices.IsEmpty | _ -> false))
            if selected && arguments |> List.exists effectful then fail "Split effectful selectors and other arguments into scalar temporaries"
            List.zip target.Parameters arguments |> List.collect (fun (parameter, arg) ->
                match parameter.Record with
                | None ->
                    let value = expression prefix arg
                    // The current BASIC AST erases grouping. An identity expression
                    // preserves the value argument through lowering on every backend.
                    let explicitlyGrouped =
                        match arg with
                        | "(" :: _ -> let _, remaining = group arg in remaining.IsEmpty
                        | _ -> false
                    [ if parameter.Declaration.Value || explicitlyGrouped then
                          if parameter.Declaration.Name.EndsWith("$") then "(\"\" & (" + value + "))"
                          else "(0 + (" + value + "))"
                      else value ]
                | Some expected ->
                    match tryView arg with
                    | Some(view, []) when eq view.TypeName expected.TypeName ->
                        if expected.Rank > 0 && (not view.WholeArray || expected.Rank <> view.Storage.Rank || not view.Path.IsEmpty) then fail "Whole-record-array argument rank mismatch"
                        if expected.Rank = 0 && view.WholeArray then fail "Expected one record argument"
                        if not view.Indices.IsEmpty then
                            if profile = "TURBO509" || viewLeaves view |> List.exists (fun leaf -> not (logicalBounds leaf).IsEmpty) then fail "Cannot pass a record array element in this profile; use a standalone work record"
                            if embedded && arg |> List.exists (fun t -> t = ":") then fail "Split effectful record selector"
                        let view = { view with Indices = view.Indices |> List.map (tokenize >> expression prefix) } |> captureView prefix
                        viewLeaves view |> List.map (fun leaf -> access view leaf [])
                    | _ -> fail "Record argument has the wrong nominal TYPE or is not a record")
        let checkedExpression (prefix: ResizeArray<string>) ts =
            let before = prefix.Count
            let rendered = expression prefix ts
            if prefix.Count > before && effectfulOutsideRecordCalls ts then fail "Split effectful record selectors from larger expressions"
            rendered
        let copy destination source =
            if destination.WholeArray then fail "Copy destination must be one record"
            if source.WholeArray || not (eq destination.TypeName source.TypeName) then fail "Whole-record copies require the same nominal TYPE"
            let prefix = ResizeArray<string>()
            let destination = { destination with Indices = destination.Indices |> List.map (tokenize >> expression prefix) } |> captureView prefix
            let source = { source with Indices = source.Indices |> List.map (tokenize >> expression prefix) } |> captureView prefix
            let assignments = viewLeaves destination |> List.collect (fun leaf ->
                loops (logicalBounds leaf |> List.map string) (fun counters -> [access destination leaf counters + " = " + access source leaf counters]))
            List.ofSeq prefix @ assignments
        let declare local (decl: Declaration) =
            match decl.TypeName with
            | None ->
                let prefix = ResizeArray<string>()
                let dimensions = decl.Bounds |> List.map (tokenize >> expression prefix)
                List.ofSeq prefix @ [ (if local then "LOCal " else "DIM ") + dimension decl.Name dimensions ]
            | Some _ ->
                let storage = records[decl.Name]
                let prefix = ResizeArray<string>()
                let bs = decl.Bounds |> List.map (tokenize >> expression prefix >> capture prefix)
                for b in bs do
                    prefix.AddRange(guard ($"{b} < {if decl.Pool then 1 else 0} OR {b} > 32767 OR {b} <> INT({b})") "Invalid record array bound")
                let allocation = leavesFor storage |> List.collect (fun leaf ->
                    let dims = bs @ (leaf.Bounds |> List.map string)
                    let name = backing storage leaf.Path
                    let alloc = if local then ["LOCal " + dimension name dims] elif dims.IsEmpty then [] else ["DIM " + dimension name dims]
                    alloc @ loops (bs @ (logicalBounds leaf |> List.map string)) (fun counters -> [dimension name counters + " = " + zero leaf]))
                let poolInit =
                    if not decl.Pool then []
                    else
                        [ "rec_" + storage.Name + "_size% = " + bs.Head; "DIM rec_" + storage.Name + "_used%(" + bs.Head + ")" ] @
                        loops bs (fun counters -> [ dimension ("rec_" + storage.Name + "_used%") counters + " = 0" ])
                List.ofSeq prefix @ allocation @ poolInit
        let rec statements (ts: string list) =
            if ts.IsEmpty then []
            elif isRemark ts || eq ts.Head "MANIFEST" then [text ts]
            else
                let splitAt word =
                    let mutable depth = 0
                    ts |> List.tryFindIndex (fun token ->
                        if token = "(" then depth <- depth + 1
                        elif token = ")" then depth <- depth - 1
                        depth = 0 && eq token word)
                let prefix = ResizeArray<string>()
                match ts with
                | select :: on :: selector when keyword select "SELECT" 3 && eq on "ON" ->
                    let value = checkedExpression prefix selector
                    let captured = newTempWith (if stringExpression selector then "$" else "")
                    selects.Push(captured, ref 0, ref false, value)
                    List.ofSeq prefix @ [captured + " = " + value]
                | e :: s :: _ when eq e "END" && keyword s "SELECT" 3 ->
                    if selects.Count = 0 then fail "END SELECT without SELECT"
                    let _, count, _, _ = selects.Pop()
                    [ for _ in 1 .. count.Value -> "END IF" ]
                | on :: tail when eq on "ON" && selects.Count > 0 ->
                    match split "=" tail with
                    | selector :: rest when not rest.IsEmpty ->
                        let _, _, _, original = selects.Peek()
                        let repeated = expression prefix selector
                        if not (eq repeated original) || prefix.Count > 0 then fail "Use compact SELECT arms when the clause selector differs from the header"
                        // Keep any '=' in the arm body; only the first separates
                        // ON's repeated selector from its range.
                        let body = rest |> List.reduce (fun a b -> a @ ["="] @ b)
                        statements ("=" :: body)
                    | _ -> fail "Malformed SELECT ON arm"
                | "=" :: tail when selects.Count > 0 ->
                    let selector, count, remainder, _ = selects.Peek()
                    if remainder.Value then fail "SELECT arm after REMAINDER"
                    let parts = split ":" tail
                    let ranges = parts.Head
                    let body = parts.Tail |> List.collect statements
                    let otherwise = count.Value > 0
                    if ranges.Length = 1 && eq ranges.Head "REMAINDER" then
                        remainder.Value <- true
                        if not otherwise then fail "REMAINDER requires an earlier SELECT arm"
                        ["ELSE"] @ body
                    else
                        let conditions = split "," ranges |> List.map (fun range ->
                            match split "TO" range with
                            | [low; high] -> "(" + selector + " >= " + expression prefix low + " AND " + selector + " <= " + expression prefix high + ")"
                            | [one] -> "(" + selector + " = " + expression prefix one + ")"
                            | _ -> fail "Malformed SELECT range")
                        count.Value <- count.Value + 1
                        (if otherwise then ["ELSE"] else []) @ List.ofSeq prefix @ ["IF " + String.concat " OR " conditions + " THEN"] @ body
                | first :: _ when eq first "IF" ->
                    match splitAt "THEN" with
                    | None -> fail "Use THEN in extended IF headers"
                    | Some n ->
                        let condition = checkedExpression prefix (ts |> List.skip 1 |> List.take (n-1))
                        let tail = ts |> List.skip (n+1)
                        if tail.IsEmpty then List.ofSeq prefix @ ["IF " + condition + " THEN"]
                        else
                            if tail |> List.exists (fun t -> eq t "IF") then fail "Use multiline form for nested inline conditionals"
                            let branches = split "ELSE" tail
                            match branches with
                            | [yes] -> List.ofSeq prefix @ ["IF " + condition + " THEN"] @ statements yes @ ["END IF"]
                            | [yes; no] -> List.ofSeq prefix @ ["IF " + condition + " THEN"] @ statements yes @ ["ELSE"] @ statements no @ ["END IF"]
                            | _ -> fail "Malformed inline IF"
                | first :: _ when eq first "FOR" || keyword first "REPEAT" 3 ->
                    let parts = split ":" ts
                    match parts with
                    | [one] ->
                        let rendered = checkedExpression prefix one
                        if eq first "FOR" then
                            let counter = one |> List.skip 1 |> split "=" |> List.head |> expression (ResizeArray())
                            if counter.Contains("(") then fail "A record-array field cannot be a FOR counter"
                        List.ofSeq prefix @ [rendered]
                    | firstPart :: body ->
                        if body |> List.exists (fun part -> match part with e :: _ when eq e "END" -> true | _ -> false) then fail "Use multiline form for explicitly closed inline loops"
                        let beginLoop = expression prefix firstPart
                        let loopName =
                            if eq first "FOR" then
                                match firstPart |> List.skip 1 |> split "=" with
                                | counter :: _ -> expression prefix counter
                                | _ -> fail "Malformed FOR"
                            else text (List.tail firstPart)
                        if loopName.Contains("(") then fail "A record-array field cannot be a FOR counter"
                        List.ofSeq prefix @ [beginLoop] @ (body |> List.collect statements) @ [ (if eq first "FOR" then "END FOR " else "END REPeat ") + loopName ]
                    | _ -> []
                | _ ->
                    let parts = split ":" ts
                    if parts.Length > 1 then
                        // SELECT arms have an expression before their first colon.
                        match ts with
                        | "=" :: _ -> [expression prefix parts.Head] @ (parts.Tail |> List.collect statements)
                        | _ -> parts |> List.collect statements
                    else
                        match split "=" ts with
                        | [left; right] ->
                            match tryView left, tryView right with
                            | Some(dest, []), Some(src, []) -> copy dest src
                            | Some(_, []), _ -> fail "Whole-record assignment requires a record source"
                            | _ ->
                                let value = checkedExpression prefix ts
                                List.ofSeq prefix @ [value]
                        | _ ->
                            match ts with
                            | name :: tail when routines.ContainsKey name && not routines[name].IsFunction ->
                                let args =
                                    match tail with
                                    | [] -> []
                                    | "(" :: _ ->
                                        let inside, remaining = group tail
                                        if remaining.IsEmpty && (split "," inside).Length > 1 then split "," inside
                                        else split "," tail
                                    | _ -> split "," tail
                                let args = callArguments prefix routines[name] args false
                                List.ofSeq prefix @ [name + (if args.IsEmpty then "" else " " + String.concat "," args)]
                            | _ ->
                                let value = checkedExpression prefix ts
                                // Calls rewritten by expression() are effectful themselves;
                                // only their selectors/other arguments were checked there.
                                List.ofSeq prefix @ [value]
        let output = ResizeArray<SourceLine>()
        let inserts = ResizeArray<int * Routine * SourceLine>()
        for index, (line, ts) in tokens |> List.indexed do
            serial <- 0
            at line (fun () ->
                if typeLines.Contains index then ()
                elif headers.ContainsKey index then
                    let r = headers[index]
                    routine <- Some r
                    let names = r.Parameters |> List.collect (fun p ->
                        match p.Record with
                        | None ->
                            if p.Declaration.Value && r.References |> List.exists (eq p.Declaration.Name) then fail "REFERENCE conflicts with VAL"
                            [p.Declaration.Name]
                        | Some storage -> leavesFor storage |> List.map (fun leaf -> backing storage leaf.Path))
                    if names.Length > 64 then fail "Expanded parameter count exceeds 64"
                    let refs = r.Parameters |> List.collect (fun p -> match p.Record with None -> [] | Some s -> leavesFor s |> List.map (fun l -> backing s l.Path))
                    output.Add(generatedLine ((if r.IsFunction then "DEFine FuNction " else "DEFine PROCedure ") + r.Name + (if names.IsEmpty then "" else "(" + String.concat "," names + ")")))
                    let refs = refs @ r.References |> List.distinctBy upper
                    if not refs.IsEmpty then output.Add(generatedLine ("REFERENCE " + String.concat "," refs))
                    inserts.Add(output.Count, r, line)
                elif not ts.IsEmpty && eq ts.Head "REFERENCE" then ()
                elif declarations.ContainsKey index then
                    let local = keyword ts.Head "LOCAL" 3
                    let ds = declarations[index]
                    if local then
                        match routine with
                        | None -> fail "LOCal outside a routine"
                        | Some r ->
                            r.LocalCount <- r.LocalCount + (ds |> List.sumBy (fun d -> match d.TypeName with None -> 1 | Some _ -> (leavesFor records[d.Name]).Length))
                    for decl in ds do declare local decl |> List.iter (generatedLine >> output.Add)
                elif ts.IsEmpty || isRemark ts || line.Text.StartsWith("@", StringComparison.Ordinal) then output.Add line
                else
                    statements ts |> List.iter (generatedLine >> output.Add)
                    match ts with
                    | e :: d :: _ when eq e "END" && (eq d "DEF" || eq d "DEFINE") -> routine <- None
                    | _ -> ())
        for position, r, line in inserts |> Seq.rev do
            if r.LocalCount + r.Temps.Count > 64 then origin <- line; fail "Expanded local count exceeds 64"
            if r.Temps.Count > 0 then output.Insert(position, { line with Text = "LOCal " + String.concat "," r.Temps })
        routine <- None
        if selects.Count <> 0 then fail "Missing END SELECT"
        for pool in pools do
            serial <- 0
            let helper = { Name = pool.Name + "_new%"; IsFunction = true; Parameters = []; References = []; Temps = ResizeArray(); LocalCount = 0 }
            routine <- Some helper
            let size = "rec_" + pool.Name + "_size%"
            let used = "rec_" + pool.Name + "_used%"
            let slot = "rec_" + pool.Name + "_slot%"
            let handle = "rec_" + pool.Name + "_handle%"
            let view = { Storage = pool; TypeName = pool.TypeName; Path = []; Indices = [slot]; WholeArray = false }
            let clear = leavesFor pool |> List.collect (fun leaf -> loops (logicalBounds leaf |> List.map string) (fun cs -> [access view leaf cs + " = " + zero leaf]))
            let body =
                [ "DEFine FuNction " + pool.Name + "_new%"; "LOCal " + String.concat "," (slot :: List.ofSeq helper.Temps); "FOR " + slot + " = 1 TO " + size; "IF " + used + "(" + slot + ") = 0 THEN"; used + "(" + slot + ") = 1" ] @ clear @
                [ "RETurn " + slot; "END IF"; "END FOR " + slot; "PRINT \"Record pool exhausted\""; "STOP"; "END DEFine";
                  "DEFine PROCedure " + pool.Name + "_free(" + handle + ")"; "IF " + handle + " = 0 THEN RETurn" ] @
                guard ($"{handle} < 1 OR {handle} > {size} OR {handle} <> INT({handle})") "Invalid pool handle" @
                guard ($"{used}({handle}) = 0") "Record pool double free" @ [ used + "(" + handle + ") = 0"; "END DEFine" ]
            body |> List.iter (generatedLine >> output.Add)
        for line in output do
            if line.Text.Length > 1000 || (tokenize line.Text).Length > 256 then origin <- line; fail "Expanded line exceeds SSB 2.8.0 limits"
        List.ofSeq output
