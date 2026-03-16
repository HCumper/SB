module AstDiagnostics

open System
open System.IO
open System.Text
open System.Text.Json

open Types
open SyntaxAst

// Pretty-printers used when inspecting parse output and intermediate AST state.
let private indent level =
    String.replicate level " "

let private escapeJson (value: string) =
    JsonEncodedText.Encode(value).ToString()

let private formatPosition (position: SourcePosition) =
    match position.BasicLineNo with
    | Some basicLine -> $"{basicLine}:{position.EditorLineNo}:{position.Column}"
    | None -> $"{position.EditorLineNo}:{position.Column}"

let rec private prettyExpr level expr =
    let pad = indent level
    match expr with
    | Identifier(pos, name) -> $"{pad}Identifier {name} @{formatPosition pos}\n"
    | NumberLiteral(pos, value) -> $"{pad}NumberLiteral {value} @{formatPosition pos}\n"
    | StringLiteral(pos, value) -> $"{pad}StringLiteral {value} @{formatPosition pos}\n"
    | UnaryExpr(pos, op, inner) ->
        $"{pad}UnaryExpr {op} @{formatPosition pos}\n{prettyExpr (level + 2) inner}"
    | BinaryExpr(pos, op, left, right) ->
        String.concat ""
            [ $"{pad}BinaryExpr {op} @{formatPosition pos}\n"
              prettyExpr (level + 2) left
              prettyExpr (level + 2) right ]
    | SliceRange(pos, left, right) ->
        String.concat ""
            [ $"{pad}SliceRange @{formatPosition pos}\n"
              prettyExpr (level + 2) left
              prettyExpr (level + 2) right ]
    | PostfixName(pos, name, args) ->
        let renderedArgs =
            args
            |> Option.defaultValue []
            |> List.map (prettyExpr (level + 2))
            |> String.concat ""
        $"{pad}PostfixName {name} @{formatPosition pos}\n{renderedArgs}"

let rec private prettyBlock level block =
    match block with
    | StatementBlock stmts ->
        String.concat ""
            [ $"{indent level}StatementBlock\n"
              stmts |> List.map (prettyStmt (level + 2)) |> String.concat "" ]
    | LineBlock lines ->
        String.concat ""
            [ $"{indent level}LineBlock\n"
              lines |> List.map (prettyLine (level + 2)) |> String.concat "" ]

and private prettyClause level (SelectClause(pos, selector, rangeExpr, body)) =
    let bodyText =
        body
        |> Option.map (prettyBlock (level + 2))
        |> Option.defaultValue ""
    String.concat ""
        [ $"{indent level}SelectClause @{formatPosition pos}\n"
          prettyExpr (level + 2) selector
          prettyExpr (level + 2) rangeExpr
          bodyText ]

and private prettyStmt level stmt =
    let pad = indent level
    match stmt with
    | ProcedureDef(pos, name, parameters, body) ->
        let parameterText = String.concat ", " parameters
        String.concat ""
            [ $"{pad}ProcedureDef {name}({parameterText}) @{formatPosition pos}\n"
              body |> List.map (prettyLine (level + 2)) |> String.concat "" ]
    | FunctionDef(pos, name, parameters, body) ->
        let parameterText = String.concat ", " parameters
        String.concat ""
            [ $"{pad}FunctionDef {name}({parameterText}) @{formatPosition pos}\n"
              body |> List.map (prettyLine (level + 2)) |> String.concat "" ]
    | DimStmt(pos, items) ->
        let itemText =
            items
            |> List.map (fun (name, dims) ->
                String.concat ""
                    [ $"{pad}  Item {name}\n"
                      dims |> List.map (prettyExpr (level + 4)) |> String.concat "" ])
            |> String.concat ""
        $"{pad}DimStmt @{formatPosition pos}\n{itemText}"
    | LocalStmt(pos, items) ->
        let itemText =
            items
            |> List.map (fun (name, dims) ->
                let dimText =
                    dims
                    |> Option.defaultValue []
                    |> List.map (prettyExpr (level + 4))
                    |> String.concat ""
                $"{pad}  Item {name}\n{dimText}")
            |> String.concat ""
        $"{pad}LocalStmt @{formatPosition pos}\n{itemText}"
    | ImplicitStmt(pos, suffix, names) ->
        let joinedNames = String.concat ", " names
        $"{pad}ImplicitStmt {suffix} [{joinedNames}] @{formatPosition pos}\n"
    | ReferenceStmt(pos, exprs) ->
        let renderedExprs = exprs |> List.map (prettyExpr (level + 2)) |> String.concat ""
        $"{pad}ReferenceStmt @{formatPosition pos}\n{renderedExprs}"
    | Assignment(pos, target, value) ->
        String.concat ""
            [ $"{pad}Assignment @{formatPosition pos}\n"
              prettyExpr (level + 2) target
              prettyExpr (level + 2) value ]
    | ProcedureCall(pos, name, args) ->
        let renderedArgs = args |> List.map (prettyExpr (level + 2)) |> String.concat ""
        $"{pad}ProcedureCall {name} @{formatPosition pos}\n{renderedArgs}"
    | ChannelProcedureCall(pos, name, channel, args) ->
        String.concat ""
            [ $"{pad}ChannelProcedureCall {name} @{formatPosition pos}\n"
              prettyExpr (level + 2) channel
              args |> List.map (prettyExpr (level + 2)) |> String.concat "" ]
    | ForStmt(pos, name, startExpr, endExpr, stepExpr, body) ->
        String.concat ""
            [ $"{pad}ForStmt {name} @{formatPosition pos}\n"
              prettyExpr (level + 2) startExpr
              prettyExpr (level + 2) endExpr
              stepExpr |> Option.map (prettyExpr (level + 2)) |> Option.defaultValue ""
              prettyBlock (level + 2) body ]
    | RepeatStmt(pos, name, body) ->
        String.concat ""
            [ $"{pad}RepeatStmt {name} @{formatPosition pos}\n"
              prettyBlock (level + 2) body ]
    | IfStmt(pos, cond, thenBlock, elseBlock) ->
        String.concat ""
            [ $"{pad}IfStmt @{formatPosition pos}\n"
              prettyExpr (level + 2) cond
              prettyBlock (level + 2) thenBlock
              elseBlock |> Option.map (prettyBlock (level + 2)) |> Option.defaultValue "" ]
    | SelectStmt(pos, selector, clauses) ->
        String.concat ""
            [ $"{pad}SelectStmt @{formatPosition pos}\n"
              prettyExpr (level + 2) selector
              clauses |> List.map (prettyClause (level + 2)) |> String.concat "" ]
    | WhenStmt(pos, condition, body) ->
        String.concat ""
            [ $"{pad}WhenStmt @{formatPosition pos}\n"
              condition |> Option.map (prettyExpr (level + 2)) |> Option.defaultValue ""
              body |> List.map (prettyLine (level + 2)) |> String.concat "" ]
    | ReturnStmt(pos, value) ->
        String.concat ""
            [ $"{pad}ReturnStmt @{formatPosition pos}\n"
              value |> Option.map (prettyExpr (level + 2)) |> Option.defaultValue "" ]
    | DataStmt(pos, values) ->
        let renderedValues = values |> List.map (prettyExpr (level + 2)) |> String.concat ""
        $"{pad}DataStmt @{formatPosition pos}\n{renderedValues}"
    | ReadStmt(pos, values) ->
        let renderedValues = values |> List.map (prettyExpr (level + 2)) |> String.concat ""
        $"{pad}ReadStmt @{formatPosition pos}\n{renderedValues}"
    | RestoreStmt(pos, value) ->
        String.concat ""
            [ $"{pad}RestoreStmt @{formatPosition pos}\n"
              value |> Option.map (prettyExpr (level + 2)) |> Option.defaultValue "" ]
    | ExitStmt(pos, name) -> $"{pad}ExitStmt {name} @{formatPosition pos}\n"
    | NextStmt(pos, name) -> $"{pad}NextStmt {name} @{formatPosition pos}\n"
    | Remark(pos, text) -> $"{pad}Remark {text} @{formatPosition pos}\n"

and private prettyLine level (Line(pos, lineNumber, stmts)) =
    let label =
        match lineNumber with
        | Some line -> $"Line {line}"
        | None -> "Line"
    String.concat ""
        [ $"{indent level}{label} @{formatPosition pos}\n"
          stmts |> List.map (prettyStmt (level + 2)) |> String.concat "" ]

let prettyPrintAst ast =
    match ast with
    | Program(pos, lines) ->
        String.concat ""
            [ $"Program @{formatPosition pos}\n"
              lines |> List.map (prettyLine 2) |> String.concat "" ]

let private writePosition (writer: Utf8JsonWriter) (position: SourcePosition) =
    writer.WriteStartObject()
    match position.BasicLineNo with
    | Some basicLine -> writer.WriteNumber("basicLineNo", basicLine)
    | None -> writer.WriteNull("basicLineNo")
    writer.WriteNumber("editorLineNo", position.EditorLineNo)
    writer.WriteNumber("column", position.Column)
    writer.WriteEndObject()

let private writeStringArray (name: string) (writer: Utf8JsonWriter) (values: string list) =
    writer.WritePropertyName(name)
    writer.WriteStartArray()
    values |> List.iter writer.WriteStringValue
    writer.WriteEndArray()

let rec private writeExpr (writer: Utf8JsonWriter) expr =
    writer.WriteStartObject()
    match expr with
    | Identifier(pos, name) ->
        writer.WriteString("kind", "Identifier")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
    | NumberLiteral(pos, value) ->
        writer.WriteString("kind", "NumberLiteral")
        writer.WriteString("value", value)
        writer.WritePropertyName("position")
        writePosition writer pos
    | StringLiteral(pos, value) ->
        writer.WriteString("kind", "StringLiteral")
        writer.WriteString("value", value)
        writer.WritePropertyName("position")
        writePosition writer pos
    | UnaryExpr(pos, op, inner) ->
        writer.WriteString("kind", "UnaryExpr")
        writer.WriteString("operator", op)
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("operand")
        writeExpr writer inner
    | BinaryExpr(pos, op, left, right) ->
        writer.WriteString("kind", "BinaryExpr")
        writer.WriteString("operator", op)
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("left")
        writeExpr writer left
        writer.WritePropertyName("right")
        writeExpr writer right
    | SliceRange(pos, left, right) ->
        writer.WriteString("kind", "SliceRange")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("left")
        writeExpr writer left
        writer.WritePropertyName("right")
        writeExpr writer right
    | PostfixName(pos, name, args) ->
        writer.WriteString("kind", "PostfixName")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("arguments")
        writer.WriteStartArray()
        args |> Option.defaultValue [] |> List.iter (writeExpr writer)
        writer.WriteEndArray()
    writer.WriteEndObject()

and private writeBlock (writer: Utf8JsonWriter) block =
    writer.WriteStartObject()
    match block with
    | StatementBlock stmts ->
        writer.WriteString("kind", "StatementBlock")
        writer.WritePropertyName("statements")
        writer.WriteStartArray()
        stmts |> List.iter (writeStmt writer)
        writer.WriteEndArray()
    | LineBlock lines ->
        writer.WriteString("kind", "LineBlock")
        writer.WritePropertyName("lines")
        writer.WriteStartArray()
        lines |> List.iter (writeLine writer)
        writer.WriteEndArray()
    writer.WriteEndObject()

and private writeSelectClause (writer: Utf8JsonWriter) (SelectClause(pos, selector, rangeExpr, body)) =
    writer.WriteStartObject()
    writer.WriteString("kind", "SelectClause")
    writer.WritePropertyName("position")
    writePosition writer pos
    writer.WritePropertyName("selector")
    writeExpr writer selector
    writer.WritePropertyName("range")
    writeExpr writer rangeExpr
    writer.WritePropertyName("body")
    match body with
    | Some block -> writeBlock writer block
    | None -> writer.WriteNullValue()
    writer.WriteEndObject()

and private writeStmt (writer: Utf8JsonWriter) stmt =
    writer.WriteStartObject()
    match stmt with
    | ProcedureDef(pos, name, parameters, body) ->
        writer.WriteString("kind", "ProcedureDef")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
        writeStringArray "parameters" writer parameters
        writer.WritePropertyName("body")
        writer.WriteStartArray()
        body |> List.iter (writeLine writer)
        writer.WriteEndArray()
    | FunctionDef(pos, name, parameters, body) ->
        writer.WriteString("kind", "FunctionDef")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
        writeStringArray "parameters" writer parameters
        writer.WritePropertyName("body")
        writer.WriteStartArray()
        body |> List.iter (writeLine writer)
        writer.WriteEndArray()
    | DimStmt(pos, items) ->
        writer.WriteString("kind", "DimStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("items")
        writer.WriteStartArray()
        items |> List.iter (fun (name, dims) ->
            writer.WriteStartObject()
            writer.WriteString("name", name)
            writer.WritePropertyName("dimensions")
            writer.WriteStartArray()
            dims |> List.iter (writeExpr writer)
            writer.WriteEndArray()
            writer.WriteEndObject())
        writer.WriteEndArray()
    | LocalStmt(pos, items) ->
        writer.WriteString("kind", "LocalStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("items")
        writer.WriteStartArray()
        items |> List.iter (fun (name, dims) ->
            writer.WriteStartObject()
            writer.WriteString("name", name)
            writer.WritePropertyName("dimensions")
            match dims with
            | Some values ->
                writer.WriteStartArray()
                values |> List.iter (writeExpr writer)
                writer.WriteEndArray()
            | None -> writer.WriteNullValue()
            writer.WriteEndObject())
        writer.WriteEndArray()
    | ImplicitStmt(pos, suffix, names) ->
        writer.WriteString("kind", "ImplicitStmt")
        writer.WriteString("suffix", suffix)
        writer.WritePropertyName("position")
        writePosition writer pos
        writeStringArray "names" writer names
    | ReferenceStmt(pos, exprs) ->
        writer.WriteString("kind", "ReferenceStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("values")
        writer.WriteStartArray()
        exprs |> List.iter (writeExpr writer)
        writer.WriteEndArray()
    | Assignment(pos, target, value) ->
        writer.WriteString("kind", "Assignment")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("target")
        writeExpr writer target
        writer.WritePropertyName("value")
        writeExpr writer value
    | ProcedureCall(pos, name, args) ->
        writer.WriteString("kind", "ProcedureCall")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("arguments")
        writer.WriteStartArray()
        args |> List.iter (writeExpr writer)
        writer.WriteEndArray()
    | ChannelProcedureCall(pos, name, channel, args) ->
        writer.WriteString("kind", "ChannelProcedureCall")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("channel")
        writeExpr writer channel
        writer.WritePropertyName("arguments")
        writer.WriteStartArray()
        args |> List.iter (writeExpr writer)
        writer.WriteEndArray()
    | ForStmt(pos, name, startExpr, endExpr, stepExpr, body) ->
        writer.WriteString("kind", "ForStmt")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("start")
        writeExpr writer startExpr
        writer.WritePropertyName("finish")
        writeExpr writer endExpr
        writer.WritePropertyName("step")
        match stepExpr with
        | Some expr -> writeExpr writer expr
        | None -> writer.WriteNullValue()
        writer.WritePropertyName("body")
        writeBlock writer body
    | RepeatStmt(pos, name, body) ->
        writer.WriteString("kind", "RepeatStmt")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("body")
        writeBlock writer body
    | IfStmt(pos, cond, thenBlock, elseBlock) ->
        writer.WriteString("kind", "IfStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("condition")
        writeExpr writer cond
        writer.WritePropertyName("then")
        writeBlock writer thenBlock
        writer.WritePropertyName("else")
        match elseBlock with
        | Some block -> writeBlock writer block
        | None -> writer.WriteNullValue()
    | SelectStmt(pos, selector, clauses) ->
        writer.WriteString("kind", "SelectStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("selector")
        writeExpr writer selector
        writer.WritePropertyName("clauses")
        writer.WriteStartArray()
        clauses |> List.iter (writeSelectClause writer)
        writer.WriteEndArray()
    | WhenStmt(pos, condition, body) ->
        writer.WriteString("kind", "WhenStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("condition")
        match condition with
        | Some expr -> writeExpr writer expr
        | None -> writer.WriteNullValue()
        writer.WritePropertyName("body")
        writer.WriteStartArray()
        body |> List.iter (writeLine writer)
        writer.WriteEndArray()
    | ReturnStmt(pos, value) ->
        writer.WriteString("kind", "ReturnStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("value")
        match value with
        | Some expr -> writeExpr writer expr
        | None -> writer.WriteNullValue()
    | DataStmt(pos, values) ->
        writer.WriteString("kind", "DataStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("values")
        writer.WriteStartArray()
        values |> List.iter (writeExpr writer)
        writer.WriteEndArray()
    | ReadStmt(pos, values) ->
        writer.WriteString("kind", "ReadStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("values")
        writer.WriteStartArray()
        values |> List.iter (writeExpr writer)
        writer.WriteEndArray()
    | RestoreStmt(pos, value) ->
        writer.WriteString("kind", "RestoreStmt")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("value")
        match value with
        | Some expr -> writeExpr writer expr
        | None -> writer.WriteNullValue()
    | ExitStmt(pos, name) ->
        writer.WriteString("kind", "ExitStmt")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
    | NextStmt(pos, name) ->
        writer.WriteString("kind", "NextStmt")
        writer.WriteString("name", name)
        writer.WritePropertyName("position")
        writePosition writer pos
    | Remark(pos, text) ->
        writer.WriteString("kind", "Remark")
        writer.WriteString("text", text)
        writer.WritePropertyName("position")
        writePosition writer pos
    writer.WriteEndObject()

and private writeLine (writer: Utf8JsonWriter) (Line(pos, lineNumber, stmts)) =
    writer.WriteStartObject()
    writer.WriteString("kind", "Line")
    writer.WritePropertyName("position")
    writePosition writer pos
    match lineNumber with
    | Some value -> writer.WriteNumber("lineNumber", value)
    | None -> writer.WriteNull("lineNumber")
    writer.WritePropertyName("statements")
    writer.WriteStartArray()
    stmts |> List.iter (writeStmt writer)
    writer.WriteEndArray()
    writer.WriteEndObject()

let serializeAst ast =
    use stream = new MemoryStream()
    let options = JsonWriterOptions(Indented = true)
    use writer = new Utf8JsonWriter(stream, options)
    match ast with
    | Program(pos, lines) ->
        writer.WriteStartObject()
        writer.WriteString("kind", "Program")
        writer.WritePropertyName("position")
        writePosition writer pos
        writer.WritePropertyName("lines")
        writer.WriteStartArray()
        lines |> List.iter (writeLine writer)
        writer.WriteEndArray()
        writer.WriteEndObject()
    writer.Flush()
    Encoding.UTF8.GetString(stream.ToArray())
