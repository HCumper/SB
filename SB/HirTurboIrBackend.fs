module HirTurboIrBackend

// HirTurboIrBackend lowers HIR to the binary intermediate stream read by the
// Turbo SuperBASIC compiler's back end (codegen 4.10 with runtime library 5.37).
// The format is defined by the Turbo IR specification
// (TURBO-IR-SPECIFICATION.md, kept with the Turbo sources); "spec 7.3" below
// means section 7.3 of that document. Where the specification left a protocol
// open, the choice follows what Turbo's own parser (parser509.bas) emits, and
// "P 22742" cites the parser line that settled it.
//
// The emitter targets the specification's first conformance level (spec 12):
// all executable code is threaded/P mode (255), all data is native/N mode (254),
// with no modules, embedded extensions, ROM array-reference arguments or local
// WHEN handlers. Choices that are not dictated by the specification are marked
// "TURBO-IR ASSUMPTION"; SB features with no IR equivalent are marked
// "TURBO-IR GAP" and reported as errors or warnings, never dropped silently.
//
// Types follow SuperBASIC and Turbo, not SB's inferred HIR types. A name ending
// in $ is a string, one ending in % an integer, anything else a float unless an
// IMPLICIT declaration says otherwise. SB's HIR types are advisory for its
// dynamically typed backends (`total = 0` is inferred as Int, for example), but
// Turbo storage is statically typed, so this emitter types every name and
// expression itself, as Turbo's parser does.

open System
open System.Collections.Generic
open System.Globalization
open System.Text

open Types
open HIR

// ---------------------------------------------------------------------------
// Stream records
// ---------------------------------------------------------------------------

/// One operand of an IR instruction.
type IrOperand =
    /// A whole word, written as its low 16 bits. A byte-valued operand still takes a word (spec 2).
    | Word of int
    /// A label identity (2000-65535), written as one word (spec 4.2).
    | LabelRef of int
    /// Ordinary literal text: W(length), the bytes, then one zero byte if the length is odd.
    | Text16 of byte[]
    /// Byte-counted payload (261 names, 224/227 descriptors): B(count), the bytes, zero
    /// padding so the whole record stays an even number of bytes.
    | Bytes8 of byte[]

/// One record of the stream: an instruction and its operands, or a bare label definition.
/// Comments only annotate the listing; they are not part of the stream.
type IrItem =
    | Instruction of opcode: int * operands: IrOperand list * comment: string
    | LabelDef of label: int * comment: string

/// Labels the stream skeleton uses. They follow Turbo's parser (spec 4.2); codegen gives them
/// no special meaning, so only this emitter's own references depend on them.
[<RequireQualifiedAccess>]
module TurboIrLabels =
    let data = 10000
    let initialisation = 10001
    let body = 10002
    let residentNames = 10003
    let secondInitialisation = 10005
    let linkage = 10006
    let afterLinkage = 10007

    /// Structured control and DATA labels are numbered from here, skipping 10000-10007.
    let firstControl = 2000
    let lastControl = 19999

    /// A referenced source line n is label lineBase + n.
    let lineBase = 20000

    /// A compiled routine with vector id v has entry label 65536 - v, the parser's
    /// W(-vectorId) convention (spec 7.2).
    let routineEntry vectorId = 65536 - vectorId

/// A startup window opened with 247 (spec 5.2). The four byte-valued fields are kept as
/// separate words; codegen packs them.
type TurboIrWindow = {
    Channel: int
    BorderColour: int
    BorderWidth: int
    Paper: int
    Ink: int
    Width: int
    Height: int
    X: int
    Y: int
}

type TurboIrOptions = {
    /// Startup windows to open, as Turbo's windows% setting (0-3). TURBO_windo n overrides it.
    WindowCount: int
    /// Definitions for windows #0, #1 and #2.
    Windows: TurboIrWindow list
    /// Emit 248 (runtime line number) at each numbered line, as Turbo's "Line Numbers" option.
    LineNumbers: bool
    /// Maximum length for a LOCal string declared without a size (Turbo's string_default%).
    LocalStringLength: int
    /// Names typed by IMPLICIT%, IMPLICIT and IMPLICIT$ (normalized, unsuffixed).
    ImplicitIntegers: Set<string>
    ImplicitReals: Set<string>
    ImplicitStrings: Set<string>
    /// MANIFEST constants by normalized name. HIR drops MANIFEST statements, so the pipeline
    /// passes their values here and the emitter folds them as Turbo's parser does.
    ManifestConstants: Map<string, HirConst>
}

type TurboIrOutput = {
    Items: IrItem list
    Bytes: byte[]
    Listing: string
    Warnings: string list
}

// TURBO-IR ASSUMPTION: Turbo's parser copies the geometry and colours of its own open
// windows (P 18062-18146), which only exist on a running QL. These defaults are the QL
// monitor-mode layout (#0 512x50a0x206, #1 256x202a256x0, #2 256x202a0x0) with the paper
// colours SB's runtime models (red for #1, black otherwise); confirm them against the target.
let defaultWindows = [
    { Channel = 0; BorderColour = 0; BorderWidth = 0; Paper = 0; Ink = 4; Width = 512; Height = 50; X = 0; Y = 206 }
    { Channel = 1; BorderColour = 0; BorderWidth = 0; Paper = 2; Ink = 7; Width = 256; Height = 202; X = 256; Y = 0 }
    { Channel = 2; BorderColour = 0; BorderWidth = 0; Paper = 0; Ink = 7; Width = 256; Height = 202; X = 0; Y = 0 }
]

let defaultOptions = {
    WindowCount = 3
    Windows = defaultWindows
    LineNumbers = false
    // TURBO-IR ASSUMPTION: Turbo reads string_default% from its configuration block; 128 is
    // used here. A LOCal string given an explicit size is unaffected.
    LocalStringLength = 128
    ImplicitIntegers = Set.empty
    ImplicitReals = Set.empty
    ImplicitStrings = Set.empty
    ManifestConstants = Map.empty
}

/// The listing written beside a generated stream.
let listingFileName (outputFileName: string) = outputFileName + ".lst"

// ---------------------------------------------------------------------------
// Opcodes (logical template numbers, spec 13.1)
// ---------------------------------------------------------------------------

module private Op =
    let endOfIr = 0
    let minusInt = 1
    let logicalNotInt = 4
    let addInt = 5
    let subInt = 6
    let timesInt = 7
    let divideInt = 8
    let geqInt = 9
    let gtInt = 10
    let eqInt = 12
    let neqInt = 13
    let leqInt = 14
    let lessInt = 15
    let modInt = 20
    let forInstanceInt = 24
    let forRangeInt = 26
    let minusFloat = 33
    let logicalNotFloat = 36
    let addFloat = 37
    let subFloat = 38
    let timesFloat = 39
    let divideFloat = 40
    let geqFloat = 41
    let gtFloat = 42
    let eqFloat = 44
    let neqFloat = 45
    let leqFloat = 46
    let lessFloat = 47
    let powerFloat = 51
    let logicalOrFloat = 53
    let logicalAndFloat = 54
    let logicalXorFloat = 55
    let forInstanceFloat = 56
    let forRangeFloat = 58
    let geqString = 73
    let gtString = 74
    let eqString = 76
    let neqString = 77
    let leqString = 78
    let lessString = 79
    let concatString = 84
    let instrString = 90
    let globalForInt = 101
    let localForInt = 102
    let constantInt = 106
    let dataInt = 107
    let fetchInt = 109
    let localInt = 110
    let extractInt = 111
    let insertInt = 112
    let readInt = 113
    let globalInt = 114
    let localArrayInt = 115
    let inputInt = 116
    let storeInt = 118
    let globalArrayInt = 126
    let globalForFloat = 133
    let localForFloat = 134
    let constFloat = 138
    let dataFloat = 139
    let fetchFloat = 141
    let localFloat = 142
    let extractFloat = 143
    let insertFloat = 144
    let readFloat = 145
    let globalFloat = 146
    let localArrayFloat = 147
    let inputFloat = 148
    let storeFloat = 150
    let globalArrayFloat = 158
    let shortString = 169
    let constString = 170
    let dataString = 171
    let extractString = 175
    let insertString = 176
    let readString = 177
    let localArrayString = 179
    let inputString = 180
    let storeStringElement = 182
    let printString = 186
    let globalArrayString = 190
    let printComma = 201
    let printNewline = 203
    let progEnd = 220
    let eofChannel = 222
    let eofData = 223
    let romFunction = 224
    let romProcedure = 227
    let nextForInt = 230
    let nextForFloat = 232
    let arrayDescriptor = 234
    let gosub = 236
    let restore = 237
    let nextRepeat = 238
    let ifFalse = 239
    let onGoto = 240
    let onGosub = 241
    let jump = 242
    let address = 243
    let returnOp = 244
    let pushOldRef = 246
    let initWindow = 247
    let lineNumber = 248
    let localString = 249
    let markStack = 250
    let dataEnd = 251
    let fast = 254
    let slow = 255
    let upString = 259
    let downString = 260
    let romName = 261
    let simpleString = 262
    let quickZero = 263
    let storeString = 264
    let fetchString = 265
    let dimn = 266
    let lastGlobal = 267
    let duplicateInt = 268
    let code = 269
    let len = 270
    let peek = 271
    let peekW = 272
    let chrs = 273
    let fills = 274
    let respr = 275
    let peekL = 276
    let clear = 277
    let poke = 278
    let pokeW = 279
    let pokeL = 280
    let printContext = 281
    let inputContext = 282
    let passString = 293
    let createRef = 294
    let floatToString = 310
    let intToString = 312
    let stringToFloat = 317
    let intToFloat = 320
    let retry = 325
    let retryHere = 326
    let whenGlobal = 327
    let stringToInt = 333
    let floatToInt = 334
    let retry1Here = 346
    let doneFor = 355
    let printGosub = 356
    let rememberEnd = 357
    let dataShort = 358
    let exprSlice = 359
    let forStep1 = 360
    let continueOp = 364
    let strLen = 375
    let wordFloat = 376
    let longFloat = 377
    let possibleEnd = 378
    let exitOp = 381
    let errNum = 383
    let errLin = 384
    let entry = 999

// ---------------------------------------------------------------------------
// Operand catalogue (spec 13.1), used to decode and validate streams
// ---------------------------------------------------------------------------

type private OperandShape =
    | FixedWords of int
    | TextShape
    | RomFunctionShape
    | RomProcedureShape
    | RomNameShape
    | BlobShape

let private operandShapes =
    let shapes = Dictionary<int, OperandShape>()
    let none =
        [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 20
          33; 34; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47; 51; 53; 54; 55
          73; 74; 75; 76; 77; 78; 79; 84; 90
          113; 116; 123; 124; 127; 128; 145; 148; 155; 156; 159; 177; 180; 186; 187; 188; 191
          201; 203; 204; 205; 206; 220; 221; 222; 223; 229; 244; 250; 251; 254; 255; 259; 260; 263
          267; 268; 269; 270; 271; 272; 273; 274; 275; 276; 277; 278; 279; 280; 281; 282; 283
          290; 291; 310; 312; 317; 320; 322; 324; 325; 326; 333; 334; 335; 336; 345; 346
          351; 352; 353; 354; 355; 359; 361; 362; 363; 364; 365; 371; 372; 373; 379; 380; 383; 384 ]
    let one =
        [ 101; 102; 106; 107; 108; 109; 110; 111; 112; 114; 118; 125; 133; 134; 141; 142; 143; 144; 146; 150
          175; 176; 182; 230; 232; 236; 237; 238; 239; 240; 242; 243; 245; 246; 248; 249; 253; 257; 262; 264; 265
          293; 294; 327; 328; 356; 357; 358; 370; 374; 375; 376; 378; 381; 999 ]
    let two =
        [ 24; 26; 56; 58; 115; 126; 147; 158; 179; 190; 234; 241; 266; 337; 338; 339; 340; 360; 366; 377; 382 ]
    for opcode in none do shapes[opcode] <- FixedWords 0
    for opcode in one do shapes[opcode] <- FixedWords 1
    for opcode in two do shapes[opcode] <- FixedWords 2
    shapes[138] <- FixedWords 3
    shapes[139] <- FixedWords 3
    shapes[247] <- FixedWords 9
    shapes[169] <- TextShape
    shapes[170] <- TextShape
    shapes[171] <- TextShape
    shapes[224] <- RomFunctionShape
    shapes[227] <- RomProcedureShape
    shapes[261] <- RomNameShape
    shapes[385] <- BlobShape
    shapes

/// Zero-based positions of label operands, by opcode (378's operand defines rather than references).
let private labelOperandPositions =
    dict [
        24, 1; 26, 1; 56, 1; 58, 1; 360, 1; 241, 1; 382, 1
        236, 0; 237, 0; 238, 0; 239, 0; 242, 0; 243, 0; 245, 0; 327, 0; 328, 0; 356, 0; 357, 0; 378, 0; 381, 0 ]

/// Zero-based positions of vector-id operands, by opcode.
let private vectorOperandPositions =
    let positions = Dictionary<int, int>()
    for opcode in [ 101; 102; 108; 109; 110; 111; 112; 114; 118; 125; 133; 134; 141; 142; 143; 144; 146; 150
                    175; 176; 182; 230; 232; 246; 249; 253; 257; 262; 264; 265; 266; 293; 294; 337; 338; 339; 340
                    370; 374; 375; 382; 24; 26; 56; 58; 360; 227 ] do
        positions[opcode] <- 0
    for opcode in [ 115; 126; 147; 158; 179; 190; 234; 224; 366 ] do
        positions[opcode] <- 1
    positions

/// Operations with only a threaded implementation (spec 3).
let private threadedOnly = set [ 101; 114; 133; 146; 234; 247; 262; 327; 328; 382; 385 ]

/// Raw data operations that belong in native data sections (spec 3).
let private nativeDataOnly = set [ 107; 139; 171; 243; 261; 358; 370 ]

// ---------------------------------------------------------------------------
// Binary form, listing and decoding
// ---------------------------------------------------------------------------

let private addWord (out: ResizeArray<byte>) (value: int) =
    out.Add(byte ((value >>> 8) &&& 0xFF))
    out.Add(byte (value &&& 0xFF))

/// Writes the stream: big-endian words, no header, no line structure (spec 2).
let serialize (items: IrItem seq) =
    let out = ResizeArray<byte>()
    for item in items do
        match item with
        | LabelDef(label, _) -> addWord out label
        | Instruction(opcode, operands, _) ->
            addWord out opcode
            for operand in operands do
                match operand with
                | Word value
                | LabelRef value -> addWord out value
                | Text16 bytes ->
                    addWord out bytes.Length
                    out.AddRange bytes
                    if bytes.Length % 2 = 1 then out.Add 0uy
                | Bytes8 bytes ->
                    out.Add(byte bytes.Length)
                    out.AddRange bytes
                    if (bytes.Length + 1) % 2 = 1 then out.Add 0uy
    out.ToArray()

let private renderText (bytes: byte[]) =
    let builder = StringBuilder("\"")
    for b in bytes do
        if b >= 0x20uy && b <= 0x7Euy && b <> byte '"' && b <> byte '\\' then
            builder.Append(char b) |> ignore
        else
            builder.Append("\\x").Append((int b).ToString("X2", CultureInfo.InvariantCulture)) |> ignore
    builder.Append('"').ToString()

let private renderOperand opcode operand =
    match operand with
    | Word value -> string value
    | LabelRef label -> string (label &&& 0xFFFF)
    | Text16 bytes -> renderText bytes
    | Bytes8 bytes when opcode = Op.romName -> renderText bytes
    | Bytes8 bytes ->
        bytes
        |> Array.map (fun b -> (int b).ToString("X2", CultureInfo.InvariantCulture))
        |> String.concat " "
        |> sprintf "[%s]"

/// Renders the stream as one record per line: "opcode operands ; comment" or "label:".
let formatListing (items: IrItem seq) =
    let builder = StringBuilder()
    for item in items do
        let text, comment =
            match item with
            | LabelDef(label, comment) -> $"{label &&& 0xFFFF}:", comment
            | Instruction(opcode, operands, comment) ->
                String.Join(" ", string opcode :: (operands |> List.map (renderOperand opcode))), comment
        builder.Append(text) |> ignore
        if not (String.IsNullOrEmpty comment) then
            builder.Append(" ; ").Append(comment) |> ignore
        builder.Append('\n') |> ignore
    builder.ToString()

/// Drops listing comments and reduces words to their 16-bit pattern, so that emitted records
/// compare equal to the records decoded from their bytes.
let normalizeItem item =
    match item with
    | LabelDef(label, _) -> LabelDef(label &&& 0xFFFF, "")
    | Instruction(opcode, operands, _) ->
        let operands =
            operands
            |> List.map (function
                | Word value -> Word(value &&& 0xFFFF)
                | LabelRef label -> LabelRef(label &&& 0xFFFF)
                | other -> other)
        Instruction(opcode, operands, "")

/// Decodes a stream into records using the operand catalogue (spec 13). Label operands come
/// back as LabelRef and every other fixed operand as an unsigned Word.
let decode (bytes: byte[]) : Result<IrItem list, string> =
    let position = ref 0
    let need count =
        if position.Value + count > bytes.Length then
            failwith $"Stream ends inside a record at byte {position.Value}."
    let readByte () =
        need 1
        let value = int bytes[position.Value]
        position.Value <- position.Value + 1
        value
    let readWord () =
        need 2
        let value = (int bytes[position.Value] <<< 8) ||| int bytes[position.Value + 1]
        position.Value <- position.Value + 2
        value
    let readBytes count =
        need count
        let value = bytes[position.Value .. position.Value + count - 1]
        position.Value <- position.Value + count
        value
    let readFixed opcode count =
        [ for index in 0 .. count - 1 ->
              let value = readWord ()
              match labelOperandPositions.TryGetValue opcode with
              | true, labelIndex when labelIndex = index -> LabelRef value
              | _ -> Word value ]
    let readCounted () =
        let count = readByte ()
        let payload = readBytes count
        if (count + 1) % 2 = 1 then readByte () |> ignore
        Bytes8 payload
    try
        let items = ResizeArray<IrItem>()
        if bytes.Length % 2 <> 0 then failwith "Stream length is odd; every record must keep word alignment."
        let first = readWord ()
        if first <> Op.entry then failwith $"Stream starts with {first}, not the mandatory 999 entry record."
        items.Add(Instruction(Op.entry, [ Word(readWord ()) ], ""))
        let mutable finished = false
        while not finished do
            let word = readWord ()
            if word = Op.endOfIr then
                items.Add(Instruction(Op.endOfIr, [], ""))
                if position.Value <> bytes.Length then
                    failwith $"{bytes.Length - position.Value} bytes follow the end-of-IR word."
                finished <- true
            elif word >= 2000 then
                items.Add(LabelDef(word, ""))
            else
                let operands =
                    match operandShapes.TryGetValue word with
                    | true, FixedWords count -> readFixed word count
                    | true, TextShape ->
                        let length = readWord ()
                        let payload = readBytes length
                        if length % 2 = 1 then readByte () |> ignore
                        [ Text16 payload ]
                    | true, RomFunctionShape ->
                        let resultType = readWord ()
                        let vector = readWord ()
                        [ Word resultType; Word vector; readCounted () ]
                    | true, RomProcedureShape ->
                        let vector = readWord ()
                        [ Word vector; readCounted () ]
                    | true, RomNameShape ->
                        let vector = readWord ()
                        [ Word vector; readCounted () ]
                    | true, BlobShape ->
                        need 4
                        let length = (readWord () <<< 16) ||| readWord ()
                        let payload = readBytes length
                        if length % 2 = 1 then readByte () |> ignore
                        [ Text16 payload ]
                    | _ -> failwith $"Unknown opcode {word} at byte {position.Value - 2}."
                items.Add(Instruction(word, operands, ""))
        Result.Ok(List.ofSeq items)
    with ex ->
        Result.Error ex.Message

/// Checks a stream as the specification recommends before handing it to codegen (spec 12):
/// structure, operand lengths, the 999 entry, the final 0, label definitions and references,
/// vector ids, mode requirements and the startup trailer. Returns the problems found.
let validate (bytes: byte[]) : string list =
    match decode bytes with
    | Result.Error message -> [ message ]
    | Result.Ok items ->
        let problems = ResizeArray<string>()
        let records = List.toArray items
        let vectorLimit =
            match records[0] with
            | Instruction(_, [ Word n ], _) when n >= 1 -> n - 1
            | _ ->
                problems.Add "The 999 entry record must reserve at least one vector."
                0
        let defined = HashSet<int>()
        let possibleEnds = HashSet<int>()
        let references = ResizeArray<int * int>()
        let mutable native = true
        for index in 0 .. records.Length - 1 do
            match records[index] with
            | LabelDef(label, _) ->
                if not (defined.Add label) then
                    problems.Add $"Label {label} is defined more than once (record {index})."
            | Instruction(opcode, operands, _) ->
                if opcode = Op.entry && index > 0 then
                    problems.Add $"999 appears again at record {index}."
                if opcode = Op.fast then native <- true
                if opcode = Op.slow then native <- false
                if native && threadedOnly.Contains opcode then
                    problems.Add $"Opcode {opcode} has only a threaded implementation but is in native mode (record {index})."
                if not native && nativeDataOnly.Contains opcode then
                    problems.Add $"Data opcode {opcode} is in threaded code (record {index})."
                let operandArray = List.toArray operands
                match labelOperandPositions.TryGetValue opcode with
                | true, labelIndex when labelIndex < operandArray.Length ->
                    match operandArray[labelIndex] with
                    | LabelRef label when label < 2000 ->
                        problems.Add $"Opcode {opcode} refers to label {label}, below 2000 (record {index})."
                    | LabelRef label when opcode = Op.possibleEnd -> possibleEnds.Add label |> ignore
                    | LabelRef label -> references.Add((label, index))
                    | _ -> ()
                | _ -> ()
                match vectorOperandPositions.TryGetValue opcode with
                | true, vectorIndex when vectorIndex < operandArray.Length ->
                    match operandArray[vectorIndex] with
                    | Word id when id < 1 || id > vectorLimit ->
                        problems.Add $"Opcode {opcode} uses vector {id}, outside the reserved 1..{vectorLimit} (record {index})."
                    | _ -> ()
                | _ -> ()
                if opcode = Op.romName then
                    match operandArray with
                    | [| Word id; _ |] when id <> 0xFFFF && (id < 1 || id > vectorLimit) ->
                        problems.Add $"Resident name record uses vector {id}, outside 1..{vectorLimit} (record {index})."
                    | _ -> ()
        for label, index in references do
            if not (defined.Contains label || possibleEnds.Contains label) then
                problems.Add $"Label {label} is referenced at record {index} but never defined."
        let tail = records |> Array.rev |> Array.truncate 4 |> Array.rev
        match tail with
        | [| Instruction(243, _, _); Instruction(358, _, _); Instruction(243, _, _); Instruction(0, [], _) |] -> ()
        | _ -> problems.Add "The stream must end with the 243/358/243 startup trailer and 0 (spec 8.2)."
        List.ofSeq problems

// ---------------------------------------------------------------------------
// QDOS floating point and the QL character set
// ---------------------------------------------------------------------------

/// Encodes a QDOS 6-byte float (spec 5.1): value = signedMantissa * 2^(exponent - 2079), with a
/// normalized 32-bit two's-complement mantissa. Returns exponent, mantissa high word and
/// mantissa low word, each 0..65535.
let encodeQdosFloat (value: float) : Result<int * int * int, string> =
    if Double.IsNaN value || Double.IsInfinity value then
        Result.Error $"{value} is not a finite number."
    elif value = 0.0 then
        Result.Ok(0, 0, 0)
    else
        let magnitude = abs value
        let mutable shift = 30 - int (Math.Floor(Math.Log2 magnitude))
        let mutable scaled = Math.ScaleB(magnitude, shift)
        while scaled >= 2147483648.0 do
            shift <- shift - 1
            scaled <- Math.ScaleB(magnitude, shift)
        while scaled < 1073741824.0 do
            shift <- shift + 1
            scaled <- Math.ScaleB(magnitude, shift)
        // TURBO-IR ASSUMPTION: round half away from zero from the double. QDOS converts the
        // decimal text directly, so a literal that falls exactly between two 31-bit mantissas
        // after the double rounding could differ in the last bit.
        let mutable mantissa = int64 (Math.Round(scaled, MidpointRounding.AwayFromZero))
        if mantissa = 2147483648L then
            mantissa <- 1073741824L
            shift <- shift - 1
        let mutable exponent = 2079 - shift
        let signedMantissa =
            if value > 0.0 then
                mantissa
            elif mantissa = 1073741824L then
                // A negative power of two normalizes to 0x80000000 with the exponent one lower
                // (spec 5.1, P 14826-14838).
                exponent <- exponent - 1
                -2147483648L
            else
                -mantissa
        if exponent < 1 || exponent > 4095 then
            Result.Error $"{value} is outside the range of a QDOS float."
        else
            let bits = uint32 (int32 signedMantissa)
            Result.Ok(exponent, int (bits >>> 16), int (bits &&& 0xFFFFu))

/// The Sinclair QL character set, as in QuillToDocx's QuillCharset: ASCII except 0x60 (pound)
/// and 0x7F (copyright), then accented letters, Greek letters and arrows from 0x80 to 0xBF.
/// 0xB5 is the SMSQ/E euro sign (the original ROM had a script mark there).
module QlCharacterSet =
    let private highCharacters =
        "äãåéöõøüçñæœáàâë" // 80
        + "èêïíìîóòôúùûß¢¥`" // 90
        + "ÄÃÅÉÖÕØÜÇÑÆŒαδθλ" // A0
        + "µπφ¡¿€§¤«»°÷←→↑↓" // B0

    let private codes =
        let map = Dictionary<char, byte>()
        for code in 0 .. 0x1F do
            map[char code] <- byte code
        for code in 0x20 .. 0x7E do
            if code <> 0x60 then map[char code] <- byte code
        map['£'] <- 0x60uy
        map['©'] <- 0x7Fuy
        highCharacters |> Seq.iteri (fun index ch -> map[ch] <- byte (0x80 + index))
        map

    /// QL bytes for text, or the first character the QL character set cannot represent. Text is
    /// never substituted: a program literal must reach the QL exactly or not at all (spec 2).
    let tryEncode (text: string) : Result<byte[], char> =
        let out = ResizeArray<byte>(text.Length)
        let mutable failure = None
        for ch in text do
            if failure.IsNone then
                match codes.TryGetValue ch with
                | true, code -> out.Add code
                | _ -> failure <- Some ch
        match failure with
        | Some ch -> Result.Error ch
        | None -> Result.Ok(out.ToArray())

// ---------------------------------------------------------------------------
// Emitter
// ---------------------------------------------------------------------------

type private TurboType =
    | TurboInt
    | TurboFloat
    | TurboString

type private VectorKind =
    | ScalarVector
    | ArrayVector
    | RoutineVector
    | ResidentVector

/// One entry of the vector table (spec 4.1). Turbo binds every use of a name to one vector, and
/// LOCal declarations and parameters rebind it for the life of a routine, so vectors are keyed
/// by normalized name. That is also how SB's dynamic (caller-visible) locals behave.
type private VectorInfo(id: int, name: string, kind: VectorKind, valueType: TurboType) =
    member _.Id = id
    member _.Name = name
    member _.Kind = kind
    member _.Type = valueType
    /// DIM bounds as declared, including a string array's maximum length.
    member val Dimensions: int list option = None with get, set
    /// True when the array has global or LOCal storage and so needs a 234 descriptor.
    member val HasDescriptor = false with get, set
    /// True when the name is a FOR variable anywhere it is not LOCal (101/133 storage).
    member val IsGlobalForVariable = false with get, set

type private LoopInfo =
    | ForLoop of vector: VectorInfo * top: int * endLabel: int * nextLabel: int option ref
    | RepeatLoop of top: int * endLabel: int

type private Scope = {
    Routine: HirRoutine option
    Loops: Map<LoopId, LoopInfo>
    InPrintInput: bool
    InHandler: bool
}

/// The value HIR gives PRINT's comma separator and trailing ';' or ',' (ParseTreeVisitor).
let private printCommaSentinel = "PRINT_COMMA"
let private printContinueSentinel = "PRINT_CONTINUE"

let private fitsWord (value: int) = value >= -32768 && value <= 32767

let private constType constant =
    match constant with
    | ConstInt value when fitsWord value -> TurboInt
    | ConstInt _
    | ConstFloat _ -> TurboFloat
    | ConstString _ -> TurboString

let private typeName turboType =
    match turboType with
    | TurboInt -> "integer"
    | TurboFloat -> "float"
    | TurboString -> "string"

let private kindName kind =
    match kind with
    | ScalarVector -> "a simple variable"
    | ArrayVector -> "an array"
    | RoutineVector -> "a compiled routine"
    | ResidentVector -> "a resident (ROM or extension) routine"

/// QDOS type codes in ROM call descriptors and 224 result types (spec 8.3).
let private romTypeCode turboType =
    match turboType with
    | TurboString -> 1
    | TurboFloat -> 2
    | TurboInt -> 3

let private exprPos expr =
    match expr with
    | Literal(_, _, pos)
    | ReadVar(_, _, pos)
    | ReadArrayElem(_, _, _, pos)
    | ReadStringChar(_, _, _, pos)
    | DynamicReadVar(_, _, pos)
    | DynamicReadArrayElem(_, _, _, pos)
    | DynamicReadStringChar(_, _, _, pos)
    | Unary(_, _, _, pos)
    | Binary(_, _, _, _, pos)
    | CallFunc(_, _, _, pos) -> pos

let private targetAsExpr target =
    match target with
    | WriteVar(symbol, hirType, pos) -> ReadVar(symbol, hirType, pos)
    | WriteArrayElem(symbol, indexes, hirType, pos) -> ReadArrayElem(symbol, indexes, hirType, pos)
    | WriteStringChar(symbol, index, hirType, pos) -> ReadStringChar(symbol, index, hirType, pos)
    | DynamicWriteVar(name, hirType, pos) -> DynamicReadVar(name, hirType, pos)
    | DynamicWriteArrayElem(name, indexes, hirType, pos) -> DynamicReadArrayElem(name, indexes, hirType, pos)
    | DynamicWriteStringChar(name, index, hirType, pos) -> DynamicReadStringChar(name, index, hirType, pos)

let private argAsExpr arg =
    match arg with
    | ValueArg expr -> expr
    | RefArg target -> targetAsExpr target

let rec private iterStatements (visit: HirStmt -> unit) (block: HirBlock) =
    for stmt in block do
        visit stmt
        match stmt with
        | WhenError(body, _)
        | For(_, _, _, _, _, body, _)
        | ForSequence(_, _, _, _, _, _, _, body, _)
        | Repeat(_, _, body, _) -> iterStatements visit body
        | If(_, thenBlock, elseBlock, _) ->
            iterStatements visit thenBlock
            elseBlock |> Option.iter (iterStatements visit)
        | _ -> ()

let rec private iterExpr (visit: HirExpr -> unit) (expr: HirExpr) =
    visit expr
    match expr with
    | ReadArrayElem(_, indexes, _, _)
    | DynamicReadArrayElem(_, indexes, _, _) -> indexes |> List.iter (iterExpr visit)
    | ReadStringChar(_, index, _, _)
    | DynamicReadStringChar(_, index, _, _) -> iterExpr visit index
    | Unary(_, inner, _, _) -> iterExpr visit inner
    | Binary(_, lhs, rhs, _, _) ->
        iterExpr visit lhs
        iterExpr visit rhs
    | CallFunc(_, args, _, _) -> args |> List.iter (argAsExpr >> iterExpr visit)
    | _ -> ()

/// Every expression a statement evaluates directly (not those of nested blocks), with its
/// assignment targets turned into the equivalent reads.
let private statementExprs stmt =
    let channelExprs channel =
        match channel with
        | Some(ExplicitChannel expr)
        | Some(ImplicitChannel expr) -> [ expr ]
        | None -> []
    match stmt with
    | Assign(target, value, _) -> [ targetAsExpr target; value ]
    | ProcCall(_, channel, args, _) -> channelExprs channel @ (args |> List.map argAsExpr)
    | BuiltInCall(_, channel, args, _) -> channelExprs channel @ args
    | Input(channel, prompts, targets, _) -> channelExprs channel @ prompts @ (targets |> List.map targetAsExpr)
    | If(condition, _, _, _) -> [ condition ]
    | For(_, _, start, finish, step, _, _) -> [ start; finish; step ]
    | ForSequence(_, _, prefix, start, finish, suffix, step, _, _) -> prefix @ [ start; finish ] @ suffix @ [ step ]
    | Goto(target, _)
    | Gosub(target, _) -> [ target ]
    | OnGoto(selector, targets, _)
    | OnGosub(selector, targets, _) -> selector :: targets
    | Return(Some value, _) -> [ value ]
    | Restore(Some target, _) -> [ target ]
    | Read(targets, _) -> targets |> List.map targetAsExpr
    | _ -> []

/// QL ROM keywords Turbo resolves as resident routines. Others need a loaded extension.
let private qlRomNames =
    set [
        "ABS"; "ACOS"; "ACOT"; "ADATE"; "ARC"; "ARC_R"; "ASIN"; "AT"; "ATAN"; "AUTO"; "BAUD"; "BEEP"; "BEEPING"
        "BLOCK"; "BORDER"; "CALL"; "CIRCLE"; "CIRCLE_R"; "CLOSE"; "CLS"; "COPY"; "COPY_N"; "COS"; "COT"; "CSIZE"
        "CURSOR"; "DATE"; "DATE$"; "DAY$"; "DEG"; "DELETE"; "DIR"; "DLINE"; "EDIT"; "ELLIPSE"; "ELLIPSE_R"; "EXEC"
        "EXEC_W"; "EXP"; "FILL"; "FLASH"; "FORMAT"; "INK"; "INKEY$"; "INT"; "KEYROW"; "LBYTES"; "LINE"; "LINE_R"
        "LIST"; "LN"; "LOAD"; "LOG10"; "LRUN"; "MERGE"; "MODE"; "MOVE"; "MRUN"; "NET"; "OPEN"; "OPEN_IN"; "OPEN_NEW"
        "OVER"; "PAN"; "PAPER"; "PAUSE"; "PENDOWN"; "PENUP"; "POINT"; "POINT_R"; "RAD"; "RANDOMISE"; "RECOL"
        "RENUM"; "RND"; "SAVE"; "SBYTES"; "SCALE"; "SCROLL"; "SDATE"; "SEXEC"; "SIN"; "SQRT"; "STRIP"; "TAN"
        "TURN"; "TURNTO"; "UNDER"; "WIDTH"; "WINDOW"; "REPORT"
        "ERR_NC"; "ERR_NJ"; "ERR_OM"; "ERR_OR"; "ERR_BO"; "ERR_NO"; "ERR_NF"; "ERR_EX"; "ERR_IU"; "ERR_EF"
        "ERR_DF"; "ERR_BN"; "ERR_TE"; "ERR_FF"; "ERR_BP"; "ERR_FE"; "ERR_XP"; "ERR_OV"; "ERR_NI"; "ERR_RO"; "ERR_BL" ]

/// ROM functions Turbo's parser declares with a typed parameter (P 12306-12410); their argument
/// is converted to that type. Every other resident routine takes its arguments as evaluated.
let private declaredRomParameterType name =
    match name with
    | "ACOS" | "ACOT" | "ASIN" | "COS" | "COT" | "EXP" | "LN" | "LOG10" | "SIN" | "SQRT" | "TAN"
    | "DEG" | "RAD" | "INT" | "ABS" -> Some TurboFloat
    | "KEYROW" -> Some TurboInt
    | _ -> None

let private builtInResultType (name: string) =
    match name with
    | "LEN" | "CODE" | "PEEK" | "PEEK_W" | "DIMN" | "ERNUM" | "ERLIN" | "INSTR" -> TurboInt
    | _ when name.EndsWith "$" -> TurboString
    | _ when name.EndsWith "%" -> TurboInt
    | _ -> TurboFloat

type private Emitter(options: TurboIrOptions, program: HirProgram) =
    let errors = ResizeArray<string>()
    let warnings = ResizeArray<string>()
    let warned = HashSet<string>()
    let vectors = Dictionary<string, VectorInfo>()
    let vectorOrder = ResizeArray<VectorInfo>()
    let storages = Dictionary<SymbolId, HirStorage>()
    let routines = Dictionary<SymbolId, HirRoutine>()
    let referenceFormals = HashSet<SymbolId>()
    let arrayFormals = HashSet<SymbolId>()
    let localForSymbols = HashSet<SymbolId>()
    let declaredLines = SortedSet<int>()
    let referencedLines = HashSet<int>()
    let restoreLabels = Dictionary<int, int>()
    let code = ResizeArray<IrItem>()
    let data = ResizeArray<IrItem>()
    let mutable endOfDataLabel: int option = None
    let mutable nextControlLabel = TurboIrLabels.firstControl
    let mutable windowCount = options.WindowCount
    let mutable current = code

    let describe (pos: SourcePosition) =
        match pos.BasicLineNo with
        | Some line -> $"line {line} ({pos.EditorLineNo}:{pos.Column})"
        | None -> $"{pos.EditorLineNo}:{pos.Column}"

    let fail (pos: SourcePosition) (message: string) = errors.Add($"{describe pos}: {message}")

    let warn (key: string) (message: string) =
        if warned.Add key then warnings.Add message

    let add item = current.Add item
    let op0 opcode = add (Instruction(opcode, [], ""))
    let op opcode operands = add (Instruction(opcode, operands, ""))
    let opc opcode operands comment = add (Instruction(opcode, operands, comment))
    let define label comment = add (LabelDef(label, comment))

    let newLabel (pos: SourcePosition) =
        if nextControlLabel >= TurboIrLabels.data && nextControlLabel <= TurboIrLabels.afterLinkage then
            nextControlLabel <- TurboIrLabels.afterLinkage + 1
        let label = nextControlLabel
        if label > TurboIrLabels.lastControl then
            fail pos "the program needs more than 17992 structured labels."
        nextControlLabel <- nextControlLabel + 1
        label

    member private this.TypeOfName(name: string) =
        let key = normalizeIdentifier name
        if key.EndsWith "$" then TurboString
        elif key.EndsWith "%" then TurboInt
        elif options.ImplicitIntegers.Contains key then TurboInt
        elif options.ImplicitReals.Contains key then TurboFloat
        elif options.ImplicitStrings.Contains key then TurboString
        else TurboFloat

    member private this.Vector(name: string, kind: VectorKind, pos: SourcePosition) : VectorInfo =
        let key = normalizeIdentifier name
        match vectors.TryGetValue key with
        | true, existing ->
            if existing.Kind <> kind then
                fail pos $"'{key}' is used as {kindName kind} but is already {kindName existing.Kind}."
            existing
        | _ ->
            let created = VectorInfo(vectorOrder.Count + 1, key, kind, this.TypeOfName key)
            vectors[key] <- created
            vectorOrder.Add created
            created

    /// The vector of a name used as a simple variable; a name seen for the first time here is
    /// a global variable (SuperBASIC creates it on first use).
    member private this.NamedVector(name: string, pos: SourcePosition) =
        match vectors.TryGetValue(normalizeIdentifier name) with
        | true, existing -> existing
        | _ -> this.Vector(name, ScalarVector, pos)

    member private this.ArrayOf(name: string, pos: SourcePosition) =
        match vectors.TryGetValue(normalizeIdentifier name) with
        | true, existing ->
            if existing.Kind <> ArrayVector then
                fail pos $"'{existing.Name}' is indexed as an array but is {kindName existing.Kind}."
            existing
        | _ ->
            fail pos $"array '{normalizeIdentifier name}' is used without a DIM."
            this.Vector(name, ArrayVector, pos)

    member private this.SymbolName(symbol: SymbolId) =
        match storages.TryGetValue symbol with
        | true, storage -> storage.Name
        | _ ->
            match routines.TryGetValue symbol with
            | true, routine -> routine.Name
            | _ ->
                let (SymbolId id) = symbol
                program.SymbolNames |> Map.tryFind symbol |> Option.defaultValue $"SYMBOL{id}"

    member private this.Manifest(name: string) =
        options.ManifestConstants |> Map.tryFind (normalizeIdentifier name)

    member private this.TryConstant(expr: HirExpr) : HirConst option =
        match expr with
        | Literal(constant, _, _) -> Some constant
        | ReadVar(symbol, _, _) -> this.Manifest(this.SymbolName symbol)
        | DynamicReadVar(name, _, _) -> this.Manifest name
        | Unary(Identity, inner, _, _) -> this.TryConstant inner
        | Unary(Negate, inner, _, _) ->
            match this.TryConstant inner with
            | Some(ConstInt value) -> Some(ConstInt(-value))
            | Some(ConstFloat value) -> Some(ConstFloat(-value))
            | _ -> None
        | _ -> None

    member private this.VectorType(name: string) =
        match vectors.TryGetValue(normalizeIdentifier name) with
        | true, existing -> existing.Type
        | _ -> this.TypeOfName name

    member private this.NameOf(expr: HirExpr) =
        match expr with
        | ReadVar(symbol, _, _) -> Some(this.SymbolName symbol)
        | DynamicReadVar(name, _, _) -> Some name
        | _ -> None

    /// True when the expression names a whole array.
    member private this.IsArrayName(expr: HirExpr) =
        match this.NameOf expr with
        | Some name ->
            match vectors.TryGetValue(normalizeIdentifier name) with
            | true, vector -> vector.Kind = ArrayVector
            | _ -> false
        | None -> false

    /// True when the expression is a simple string variable (not a MANIFEST).
    member private this.IsSimpleStringVariable(expr: HirExpr) =
        match this.NameOf expr with
        | Some name when (this.Manifest name).IsNone ->
            match vectors.TryGetValue(normalizeIdentifier name) with
            | true, vector -> vector.Kind = ScalarVector && vector.Type = TurboString
            | _ -> this.TypeOfName name = TurboString
        | _ -> false

    /// The type Turbo gives an expression's result (spec 5, 5.2).
    member private this.TypeOf(expr: HirExpr) : TurboType =
        match this.TryConstant expr with
        | Some constant -> constType constant
        | None ->
            match expr with
            | Literal(constant, _, _) -> constType constant
            | ReadVar(symbol, _, _)
            | ReadArrayElem(symbol, _, _, _) -> this.VectorType(this.SymbolName symbol)
            | DynamicReadVar(name, _, _)
            | DynamicReadArrayElem(name, _, _, _) -> this.VectorType name
            | ReadStringChar _
            | DynamicReadStringChar _ -> TurboString
            | Unary(Identity, inner, _, _) -> this.TypeOf inner
            | Unary(Negate, inner, _, _)
            | Unary(BitwiseNot, inner, _, _) -> if this.TypeOf inner = TurboInt then TurboInt else TurboFloat
            | Unary(UnaryUnknown _, _, _, _) -> TurboFloat
            | Binary(op, lhs, rhs, _, _) ->
                match op with
                | Add
                | Subtract
                | Multiply -> if this.TypeOf lhs = TurboInt && this.TypeOf rhs = TurboInt then TurboInt else TurboFloat
                | IntegerDivide
                | Modulo
                | Instr -> TurboInt
                | Concat -> TurboString
                | _ -> TurboFloat
            | CallFunc(symbol, _, _, _) ->
                match routines.TryGetValue symbol with
                | true, routine -> this.TypeOfName routine.Name
                | _ -> builtInResultType (normalizeIdentifier (this.SymbolName symbol))

    // -- constants ----------------------------------------------------------

    /// Integer constant: 263 for zero, otherwise 106 (Stack_Int, P 18566-18618).
    member private this.EmitInt(value: int) =
        if value = 0 then op0 Op.quickZero else op Op.constantInt [ Word value ]

    /// Float constant: 377 when the mantissa's low word is zero, otherwise 138, as the parser does
    /// for float literals and for integer literals in a float context (Examine_NumLit and
    /// Stack_Int, P 18526-18618). 376 would be shorter for small integers, but codegen's improver
    /// only recognises x = x + 1, 2 or 4 when the constant is a 377 record.
    member private this.EmitFloat(value: float, pos: SourcePosition) =
        match encodeQdosFloat value with
        | Result.Ok(exponent, high, low) ->
            let comment = value.ToString("R", CultureInfo.InvariantCulture)
            if low = 0 then opc Op.longFloat [ Word exponent; Word high ] comment
            else opc Op.constFloat [ Word exponent; Word high; Word low ] comment
        | Result.Error message -> fail pos message

    member private this.EncodeText(text: string, pos: SourcePosition) =
        match QlCharacterSet.tryEncode text with
        | Result.Ok bytes when bytes.Length <= 32767 -> Some bytes
        | Result.Ok _ ->
            fail pos "a string literal longer than 32767 bytes cannot be emitted."
            None
        | Result.Error ch ->
            fail pos $"a string contains '{ch}' (U+{int ch:X4}), which the QL character set cannot represent."
            None

    /// String literal: 169 below 14 bytes, otherwise 170 (Examine_StrLit, P 18642).
    member private this.EmitString(text: string, pos: SourcePosition) =
        match this.EncodeText(text, pos) with
        | Some bytes -> op (if bytes.Length < 14 then Op.shortString else Op.constString) [ Text16 bytes ]
        | None -> ()

    member private this.EmitConstant(want: TurboType, constant: HirConst, pos: SourcePosition) =
        match constant, want with
        | ConstInt value, TurboInt ->
            if fitsWord value then this.EmitInt value
            else fail pos $"integer constant {value} does not fit Turbo's 16-bit integer type."
        | ConstInt value, TurboFloat -> this.EmitFloat(float value, pos)
        | ConstInt value, TurboString ->
            if fitsWord value then
                this.EmitInt value
                op0 Op.intToString
            else
                this.EmitFloat(float value, pos)
                op0 Op.floatToString
        | ConstFloat value, TurboFloat -> this.EmitFloat(value, pos)
        | ConstFloat value, TurboInt ->
            this.EmitFloat(value, pos)
            op0 Op.floatToInt
        | ConstFloat value, TurboString ->
            this.EmitFloat(value, pos)
            op0 Op.floatToString
        | ConstString text, TurboString -> this.EmitString(text, pos)
        | ConstString text, TurboInt ->
            this.EmitString(text, pos)
            op0 Op.stringToInt
        | ConstString text, TurboFloat ->
            this.EmitString(text, pos)
            op0 Op.stringToFloat

    // -- expressions --------------------------------------------------------

    member private this.Convert(actual: TurboType, want: TurboType) =
        match actual, want with
        | TurboInt, TurboFloat -> op0 Op.intToFloat
        | TurboFloat, TurboInt -> op0 Op.floatToInt
        | TurboInt, TurboString -> op0 Op.intToString
        | TurboFloat, TurboString -> op0 Op.floatToString
        | TurboString, TurboInt -> op0 Op.stringToInt
        | TurboString, TurboFloat -> op0 Op.stringToFloat
        | _ -> ()

    /// Emits an expression converted to the wanted type. Each operand is converted as soon as it
    /// is pushed, because its type is known in advance, so the hide/recover and next-on-stack
    /// coercions (123/124, 155/156, 187/188, 322/324) are never needed.
    member private this.EmitAs(scope: Scope, want: TurboType, expr: HirExpr) =
        match this.TryConstant expr with
        | Some constant -> this.EmitConstant(want, constant, exprPos expr)
        | None -> this.Convert(this.EmitExpr(scope, expr), want)

    /// Emits an expression in its own type and returns that type.
    member private this.EmitExpr(scope: Scope, expr: HirExpr) : TurboType =
        match this.TryConstant expr with
        | Some constant ->
            let turboType = constType constant
            this.EmitConstant(turboType, constant, exprPos expr)
            turboType
        | None -> this.EmitComputed(scope, expr)

    member private this.EmitComputed(scope: Scope, expr: HirExpr) : TurboType =
        match expr with
        | Literal(constant, _, pos) ->
            this.EmitConstant(constType constant, constant, pos)
            constType constant
        | ReadVar(symbol, _, pos) -> this.EmitFetch(this.SymbolName symbol, pos)
        | DynamicReadVar(name, _, pos) -> this.EmitFetch(name, pos)
        | ReadArrayElem(symbol, indexes, _, pos) -> this.EmitFetchElement(scope, this.SymbolName symbol, indexes, pos)
        | DynamicReadArrayElem(name, indexes, _, pos) -> this.EmitFetchElement(scope, name, indexes, pos)
        | ReadStringChar(symbol, index, _, pos) -> this.EmitFetchChar(scope, this.SymbolName symbol, index, pos)
        | DynamicReadStringChar(name, index, _, pos) -> this.EmitFetchChar(scope, name, index, pos)
        | Unary(Identity, inner, _, _) ->
            // Unary plus (2/34) is ignored by codegen, so nothing is emitted for it.
            this.EmitExpr(scope, inner)
        | Unary(Negate, inner, _, _) ->
            if this.TypeOf inner = TurboInt then
                this.EmitAs(scope, TurboInt, inner)
                op0 Op.minusInt
                TurboInt
            else
                this.EmitAs(scope, TurboFloat, inner)
                op0 Op.minusFloat
                TurboFloat
        | Unary(BitwiseNot, inner, _, _) ->
            // HIR's BitwiseNot is SuperBASIC's NOT keyword. TURBO-IR ASSUMPTION: it is emitted as
            // Turbo's logical NOT (4 on an integer, 36 on a float), as SuperBASIC and Turbo
            // define NOT. SB's interpreter applies a bitwise complement instead, which agrees
            // only for the 0/1 values of comparisons.
            if this.TypeOf inner = TurboInt then
                this.EmitAs(scope, TurboInt, inner)
                op0 Op.logicalNotInt
                TurboInt
            else
                this.EmitAs(scope, TurboFloat, inner)
                op0 Op.logicalNotFloat
                TurboFloat
        | Unary(UnaryUnknown name, _, _, pos) ->
            fail pos $"unary operator '{name}' has no Turbo IR equivalent."
            TurboFloat
        | Binary(binaryOp, lhs, rhs, _, pos) -> this.EmitBinary(scope, binaryOp, lhs, rhs, pos)
        | CallFunc(symbol, args, _, pos) -> this.EmitFunctionCall(scope, symbol, args, pos)

    member private this.EmitBinary(scope: Scope, binaryOp: HirBinaryOp, lhs: HirExpr, rhs: HirExpr, pos: SourcePosition) =
        let operands turboType =
            this.EmitAs(scope, turboType, lhs)
            this.EmitAs(scope, turboType, rhs)
        let fixedType turboType opcode result =
            operands turboType
            op0 opcode
            result
        let arithmetic intOpcode floatOpcode =
            let turboType =
                if this.TypeOf lhs = TurboInt && this.TypeOf rhs = TurboInt then TurboInt else TurboFloat
            fixedType turboType (if turboType = TurboInt then intOpcode else floatOpcode) turboType
        let comparison intOpcode floatOpcode stringOpcode =
            // Comparisons always leave a float 0/1 (spec 5). Two integers compare as integers and
            // two strings as strings; any other mix compares as floats.
            // TURBO-IR ASSUMPTION: a string compared with a number is converted with 317.
            let left = this.TypeOf lhs
            let right = this.TypeOf rhs
            let compareType =
                if left = TurboInt && right = TurboInt then TurboInt
                elif left = TurboString && right = TurboString then TurboString
                else TurboFloat
            let opcode =
                match compareType with
                | TurboInt -> intOpcode
                | TurboFloat -> floatOpcode
                | TurboString -> stringOpcode
            fixedType compareType opcode TurboFloat
        match binaryOp with
        | Add -> arithmetic Op.addInt Op.addFloat
        | Subtract -> arithmetic Op.subInt Op.subFloat
        | Multiply -> arithmetic Op.timesInt Op.timesFloat
        // SuperBASIC '/' is always real division, even between integers (spec 5.2).
        | Divide -> fixedType TurboFloat Op.divideFloat TurboFloat
        | Power -> fixedType TurboFloat Op.powerFloat TurboFloat
        | IntegerDivide -> fixedType TurboInt Op.divideInt TurboInt
        | Modulo -> fixedType TurboInt Op.modInt TurboInt
        // HIR's Bitwise* operators are SuperBASIC's AND, OR and XOR keywords (AstToHir). They are
        // logical in SuperBASIC and Turbo, so they map to the float logical operators 54/53/55.
        // TURBO-IR ASSUMPTION: as for NOT, this follows SuperBASIC rather than SB's interpreter,
        // which applies them bitwise; the two agree on comparison results.
        | BitwiseAnd -> fixedType TurboFloat Op.logicalAndFloat TurboFloat
        | BitwiseOr -> fixedType TurboFloat Op.logicalOrFloat TurboFloat
        | BitwiseXor -> fixedType TurboFloat Op.logicalXorFloat TurboFloat
        | GreaterThanOrEqual -> comparison Op.geqInt Op.geqFloat Op.geqString
        | GreaterThan -> comparison Op.gtInt Op.gtFloat Op.gtString
        | Equal -> comparison Op.eqInt Op.eqFloat Op.eqString
        | NotEqual -> comparison Op.neqInt Op.neqFloat Op.neqString
        | LessThanOrEqual -> comparison Op.leqInt Op.leqFloat Op.leqString
        | LessThan -> comparison Op.lessInt Op.lessFloat Op.lessString
        | Concat -> fixedType TurboString Op.concatString TurboString
        | Instr -> fixedType TurboString Op.instrString TurboInt
        | SliceRange ->
            fail pos "a 'TO' range can only be an argument to a resident routine (as in RND(1 TO 6))."
            TurboFloat
        | BinaryUnknown name ->
            fail pos $"operator '{name}' has no Turbo IR equivalent."
            TurboFloat

    member private this.EmitFetch(name: string, pos: SourcePosition) : TurboType =
        let vector = this.NamedVector(name, pos)
        match vector.Kind with
        | ScalarVector ->
            let opcode =
                match vector.Type with
                | TurboInt -> Op.fetchInt
                | TurboFloat -> Op.fetchFloat
                | TurboString -> Op.fetchString
            opc opcode [ Word vector.Id ] vector.Name
        | ArrayVector ->
            fail pos $"array '{vector.Name}' cannot be used as a value; only a REFERENCE parameter can take a whole array."
        | _ ->
            fail pos $"'{vector.Name}' is {kindName vector.Kind}, not a variable."
        vector.Type

    /// Number of subscripts a DIMmed array takes; a string array's last bound is its length.
    member private this.IndexRank(vector: VectorInfo) =
        vector.Dimensions
        |> Option.map (fun dims -> if vector.Type = TurboString then dims.Length - 1 else dims.Length)

    member private this.EmitIndexes(scope: Scope, vector: VectorInfo, indexes: HirExpr list, pos: SourcePosition) =
        match this.IndexRank vector with
        | Some rank when rank <> indexes.Length ->
            fail pos $"'{vector.Name}' has {rank} dimension(s) but {indexes.Length} subscript(s) are given."
        | _ -> ()
        for index in indexes do
            this.EmitAs(scope, TurboInt, index)

    /// Splits a string-array subscript list into element subscripts and an optional character
    /// position: a$(i, n) is character n of element i in SuperBASIC.
    member private this.SplitStringSubscripts(vector: VectorInfo, indexes: HirExpr list) =
        match this.IndexRank vector with
        | Some rank when vector.Type = TurboString && indexes.Length = rank + 1 ->
            List.truncate rank indexes, Some(List.last indexes)
        | _ -> indexes, None

    /// True when an "element" of name is really a character of a simple string: AstToHir lowers
    /// some x$(n) forms (assignment targets, for one) as array elements of the string.
    member private this.IsStringCharacter(name: string, indexes: HirExpr list) =
        indexes.Length = 1
        && (match vectors.TryGetValue(normalizeIdentifier name) with
            | true, vector -> vector.Kind = ScalarVector && vector.Type = TurboString
            | _ -> false)

    member private this.EmitFetchElement(scope: Scope, name: string, indexes: HirExpr list, pos: SourcePosition) =
        if this.IsStringCharacter(name, indexes) then
            this.EmitFetchChar(scope, name, List.head indexes, pos)
        else
            this.EmitFetchArrayElement(scope, name, indexes, pos)

    member private this.EmitFetchArrayElement(scope: Scope, name: string, indexes: HirExpr list, pos: SourcePosition) =
        let vector = this.ArrayOf(name, pos)
        match vector.Type with
        | TurboString ->
            let elementIndexes, character = this.SplitStringSubscripts(vector, indexes)
            this.EmitIndexes(scope, vector, elementIndexes, pos)
            match character with
            | Some position ->
                // Single-character slice: the position twice (spec 6.2).
                this.EmitAs(scope, TurboInt, position)
                op0 Op.duplicateInt
            | None ->
                // A whole element is fetched with the 0,0 omitted-bound slice (spec 6.2).
                op0 Op.quickZero
                op0 Op.quickZero
            opc Op.extractString [ Word vector.Id ] vector.Name
        | TurboInt ->
            this.EmitIndexes(scope, vector, indexes, pos)
            opc Op.extractInt [ Word vector.Id ] vector.Name
        | TurboFloat ->
            this.EmitIndexes(scope, vector, indexes, pos)
            opc Op.extractFloat [ Word vector.Id ] vector.Name
        vector.Type

    member private this.RequireSimpleString(name: string, pos: SourcePosition) =
        let vector = this.NamedVector(name, pos)
        if vector.Kind <> ScalarVector || vector.Type <> TurboString then
            fail pos $"'{vector.Name}' is not a simple string, so it cannot be sliced."
        vector

    /// a$(n): one character of a simple string.
    // TURBO-IR ASSUMPTION: a simple string is sliced in place with 175 (start, end), as the
    // parser's Gen_Array_Ref does for any string with subscripts; the specification describes
    // 175 for string arrays and says simple strings are one-dimensional string objects.
    member private this.EmitFetchChar(scope: Scope, name: string, index: HirExpr, pos: SourcePosition) =
        if this.IsArrayName(DynamicReadVar(name, HirType.String, pos)) then
            // Inside a routine AstToHir lowers name$(i) of a global string array as a dynamic
            // string character; it is an element.
            this.EmitFetchArrayElement(scope, name, [ index ], pos)
        else
            let vector = this.RequireSimpleString(name, pos)
            this.EmitAs(scope, TurboInt, index)
            op0 Op.duplicateInt
            opc Op.extractString [ Word vector.Id ] vector.Name
            TurboString

    /// a$(s TO e) or a$(s TO). SB lowers slices to MID$/RIGHT$ calls; those are turned back
    /// into Turbo slices here. finish = None is the omitted end, sentinel 0 (spec 6.2).
    member private this.EmitSlice(scope: Scope, baseExpr: HirExpr, start: HirExpr, finish: HirExpr option, pos: SourcePosition) =
        let bounds () =
            this.EmitAs(scope, TurboInt, start)
            match finish with
            | Some value -> this.EmitAs(scope, TurboInt, value)
            | None -> op0 Op.quickZero
        match this.NameOf baseExpr with
        | Some name when this.IsSimpleStringVariable baseExpr ->
            let vector = this.RequireSimpleString(name, pos)
            bounds ()
            opc Op.extractString [ Word vector.Id ] vector.Name
        | _ ->
            this.EmitAs(scope, TurboString, baseExpr)
            bounds ()
            op0 Op.exprSlice
        TurboString

    // -- calls --------------------------------------------------------------

    member private this.IsReferenceFormal(parameter: HirParameter) =
        referenceFormals.Contains parameter.Storage.Symbol

    /// Pushes a compiled routine's arguments (spec 7.2): values in the formal's type, strings
    /// made inline with 259, by-reference actuals with 246.
    member private this.EmitRoutineArguments(scope: Scope, routine: HirRoutine, args: HirCallArg list, pos: SourcePosition) =
        if args.Length > routine.Parameters.Length then
            fail pos $"'{routine.Name}' takes {routine.Parameters.Length} argument(s) but {args.Length} are given."
        routine.Parameters
        |> List.iteri (fun index parameter ->
            let formalName = normalizeIdentifier parameter.Storage.Name
            let actual = List.tryItem index args
            if this.IsReferenceFormal parameter then
                let actualName =
                    match actual with
                    | Some(RefArg(WriteVar(symbol, _, _)))
                    | Some(ValueArg(ReadVar(symbol, _, _))) -> Some(this.SymbolName symbol)
                    | Some(RefArg(DynamicWriteVar(name, _, _)))
                    | Some(ValueArg(DynamicReadVar(name, _, _))) -> Some name
                    | _ -> None
                match actualName with
                | Some name when (this.Manifest name).IsNone ->
                    let vector = this.NamedVector(name, pos)
                    opc Op.pushOldRef [ Word vector.Id ] vector.Name
                | _ ->
                    fail pos $"by-reference parameter '{formalName}' of '{routine.Name}' needs a variable or array name as its argument."
            else
                let formalType = this.TypeOfName formalName
                match actual with
                | Some arg ->
                    let expr = argAsExpr arg
                    if this.IsArrayName expr then
                        let arrayName = this.NameOf expr |> Option.defaultValue "" |> normalizeIdentifier
                        fail pos $"array '{arrayName}' is passed to value parameter '{formalName}'; Turbo passes arrays only by REFERENCE."
                    else
                        this.EmitAs(scope, formalType, expr)
                | None ->
                    // A missing argument gets the type's empty value, as the parser supplies
                    // (P 21302-21322).
                    match formalType with
                    | TurboString -> op Op.shortString [ Text16 [||] ]
                    | TurboFloat -> op Op.wordFloat [ Word 0 ]
                    | TurboInt -> op0 Op.quickZero
                if formalType = TurboString then op0 Op.upString)

    member private this.EmitFunctionCall(scope: Scope, symbol: SymbolId, args: HirCallArg list, pos: SourcePosition) : TurboType =
        match routines.TryGetValue symbol with
        | true, routine ->
            if routine.ReturnType.IsNone then
                fail pos $"procedure '{routine.Name}' is used as a function."
            let vector = this.Vector(routine.Name, RoutineVector, pos)
            this.EmitRoutineArguments(scope, routine, args, pos)
            // Inside PRINT and INPUT a compiled function is called with 356, which keeps the
            // I/O context (spec 7.2, P 19402).
            let callOpcode = if scope.InPrintInput then Op.printGosub else Op.gosub
            opc callOpcode [ LabelRef(TurboIrLabels.routineEntry vector.Id) ] vector.Name
            let resultType = this.TypeOfName routine.Name
            if resultType = TurboString then op0 Op.downString
            resultType
        | _ -> this.EmitBuiltInFunction(scope, normalizeIdentifier (this.SymbolName symbol), args, pos)

    member private this.ArityError(name: string, pos: SourcePosition) =
        fail pos $"'{name}' is called with arguments it does not take."

    member private this.EmitBuiltInFunction(scope: Scope, name: string, args: HirCallArg list, pos: SourcePosition) : TurboType =
        let values = args |> List.map argAsExpr
        let unary want opcode result =
            match values with
            | [ value ] ->
                this.EmitAs(scope, want, value)
                op0 opcode
            | _ -> this.ArityError(name, pos)
            result
        let isOne expr = this.TryConstant expr = Some(ConstInt 1)
        match name, args with
        | "LEN", [ ValueArg value ] when this.IsSimpleStringVariable value ->
            // LEN of a simple string variable reads its length word (375, P 19518-19534).
            let vector = this.NamedVector(this.NameOf value |> Option.defaultValue "", pos)
            opc Op.strLen [ Word vector.Id ] vector.Name
            TurboInt
        | "LEN", _ -> unary TurboString Op.len TurboInt
        | "CODE", _ -> unary TurboString Op.code TurboInt
        | "CHR$", _ -> unary TurboInt Op.chrs TurboString
        | "PEEK", _ -> unary TurboFloat Op.peek TurboInt
        | "PEEK_W", _ -> unary TurboFloat Op.peekW TurboInt
        | "PEEK_L", _ -> unary TurboFloat Op.peekL TurboFloat
        | "RESPR", _ -> unary TurboFloat Op.respr TurboFloat
        | "FILL$", _ ->
            match values with
            | [ text; count ] ->
                this.EmitAs(scope, TurboString, text)
                this.EmitAs(scope, TurboInt, count)
                op0 Op.fills
            | _ -> this.ArityError(name, pos)
            TurboString
        | "EOF", _ ->
            // SB drops the '#' of EOF(#n); EOF's only argument is a channel.
            match values with
            | [] -> op0 Op.eofData
            | [ channel ] ->
                this.EmitAs(scope, TurboInt, channel)
                op0 Op.eofChannel
            | _ -> this.ArityError(name, pos)
            TurboFloat
        | "DIMN", _ ->
            let arrayName =
                match args with
                | ValueArg(ReadVar(symbol, _, _)) :: _
                | RefArg(WriteVar(symbol, _, _)) :: _ -> Some(this.SymbolName symbol)
                | ValueArg(DynamicReadVar(arrayName, _, _)) :: _
                | RefArg(DynamicWriteVar(arrayName, _, _)) :: _ -> Some arrayName
                | _ -> None
            match arrayName with
            | Some arrayName ->
                let vector = this.ArrayOf(arrayName, pos)
                match List.tail values with
                | [] -> this.EmitInt 1
                | [ dimension ] -> this.EmitAs(scope, TurboInt, dimension)
                | _ -> this.ArityError(name, pos)
                // TURBO-IR ASSUMPTION: 266's second operand is the dimension counter R-1, the
                // parser's struct% value (P 19504) and what the spec's "numeric D-1, string
                // D-2" reduces to.
                match vector.Dimensions with
                | Some dims ->
                    let counter = dims.Length - (if vector.Type = TurboString then 2 else 1)
                    opc Op.dimn [ Word vector.Id; Word counter ] vector.Name
                | None -> fail pos $"DIMN needs the declared dimensions of '{vector.Name}', which are not known here."
            | None -> fail pos "DIMN needs an array name as its first argument."
            TurboInt
        | "ERNUM", [] ->
            // SB's ERNUM and ERLIN are Turbo's ERNUM% and ERLIN% templates (P 11886-11890);
            // the ROM functions would not see Turbo's errors.
            op0 Op.errNum
            TurboInt
        | "ERLIN", [] ->
            op0 Op.errLin
            TurboInt
        | "PI", [] ->
            // The parser wires PI in as this constant (P 19458-19462).
            opc Op.constFloat [ Word 2050; Word 25735; Word 60753 ] "PI"
            TurboFloat
        | "INSTR", [ ValueArg lhs; ValueArg rhs ] ->
            this.EmitAs(scope, TurboString, lhs)
            this.EmitAs(scope, TurboString, rhs)
            op0 Op.instrString
            TurboInt
        | "MID$", [ ValueArg baseExpr; ValueArg start; ValueArg(Binary(Add, Binary(Subtract, finish, start2, _, _), one, _, _)) ]
            when start2 = start && isOne one ->
            // AstToHir lowers a$(s TO e) to MID$(a$, s, (e - s) + 1).
            this.EmitSlice(scope, baseExpr, start, Some finish, pos)
        | "RIGHT$", [ ValueArg baseExpr; ValueArg(Binary(Add, Binary(Subtract, CallFunc(lenSymbol, [ ValueArg base2 ], _, _), start, _, _), one, _, _)) ]
            when base2 = baseExpr && isOne one && normalizeIdentifier (this.SymbolName lenSymbol) = "LEN" ->
            // AstToHir lowers a$(s TO) to RIGHT$(a$, (LEN(a$) - s) + 1).
            this.EmitSlice(scope, baseExpr, start, None, pos)
        | _ ->
            let resultType = builtInResultType name
            this.WarnIfNotRom(name)
            let vector = this.Vector(name, ResidentVector, pos)
            let descriptors = this.EmitRomArguments(scope, name, None, values, pos)
            opc Op.romFunction [ Word(romTypeCode resultType); Word vector.Id; Bytes8 descriptors ] vector.Name
            resultType

    member private this.WarnIfNotRom(name: string) =
        if not (qlRomNames.Contains name) then
            warn ("rom:" + name) $"'{name}' is not a QL ROM keyword; the compiled program needs a resident extension that provides it."

    /// Pushes a resident routine's arguments in source order and returns their descriptor bytes
    /// (spec 8.3): base type, bit 7 for the '#' channel, the following separator in bits 4-6.
    // TURBO-IR GAP: SB's HIR keeps only PRINT's separators. Every other argument is assumed to be
    // followed by a comma (none after the last), except the TO inside an RND(a TO b)-style range,
    // which HIR keeps. A '#' on any argument but a leading channel is lost by SB's front end.
    member private this.EmitRomArguments(scope: Scope, name: string, channel: HirChannel option, args: HirExpr list, pos: SourcePosition) =
        let flattened =
            args
            |> List.collect (fun arg ->
                match arg with
                | Binary(SliceRange, lhs, rhs, _, _) -> [ lhs, Some 5; rhs, None ]
                | other -> [ other, None ])
        let items = ResizeArray<HirExpr * bool * int>()
        match channel with
        | Some(ExplicitChannel expr) -> items.Add((expr, true, (if flattened.IsEmpty then 0 else 1)))
        | Some(ImplicitChannel _) ->
            fail pos $"'{name}' uses SB's '\\' channel form, which has no Turbo IR equivalent."
        | None -> ()
        flattened
        |> List.iteri (fun index (expr, separator) ->
            let following =
                match separator with
                | Some value -> value
                | None -> if index < flattened.Length - 1 then 1 else 0
            items.Add((expr, false, following)))
        let declared = declaredRomParameterType name
        let bytes = ResizeArray<byte>()
        for expr, isChannel, separator in items do
            let typeCode =
                if isChannel then
                    this.EmitAs(scope, TurboInt, expr)
                    0x80 ||| 3
                elif this.IsArrayName expr then
                    // TURBO-IR GAP: whole-array arguments to resident routines use the 108
                    // vector-offset protocol (spec 8.3), which this emitter does not implement.
                    let arrayName = this.NameOf expr |> Option.defaultValue "" |> normalizeIdentifier
                    fail pos $"passing array '{arrayName}' to resident routine '{name}' is not supported."
                    0
                else
                        match declared with
                        | Some want ->
                            this.EmitAs(scope, want, expr)
                            romTypeCode want
                        | None -> romTypeCode (this.EmitExpr(scope, expr))
            bytes.Add(byte (typeCode ||| (separator <<< 4)))
        if bytes.Count > 255 then
            fail pos $"'{name}' has more than 255 arguments."
        bytes.ToArray()

    // -- assignment targets -------------------------------------------------

    member private this.EmitStore(scope: Scope, target: HirTarget, emitValue: TurboType -> unit) =
        match target with
        | WriteVar(symbol, _, pos) -> this.EmitStoreName(this.SymbolName symbol, emitValue, pos)
        | DynamicWriteVar(name, _, pos) -> this.EmitStoreName(name, emitValue, pos)
        | WriteArrayElem(symbol, indexes, _, pos) -> this.EmitStoreElement(scope, this.SymbolName symbol, indexes, emitValue, pos)
        | DynamicWriteArrayElem(name, indexes, _, pos) -> this.EmitStoreElement(scope, name, indexes, emitValue, pos)
        | WriteStringChar(symbol, index, _, pos) -> this.EmitStoreChar(scope, this.SymbolName symbol, index, emitValue, pos)
        | DynamicWriteStringChar(name, index, _, pos) -> this.EmitStoreChar(scope, name, index, emitValue, pos)

    member private this.EmitStoreName(name: string, emitValue: TurboType -> unit, pos: SourcePosition) =
        if (this.Manifest name).IsSome then
            fail pos $"MANIFEST constant '{normalizeIdentifier name}' cannot be assigned."
        else
            let vector = this.NamedVector(name, pos)
            match vector.Kind with
            | ScalarVector ->
                emitValue vector.Type
                let opcode =
                    match vector.Type with
                    | TurboInt -> Op.storeInt
                    | TurboFloat -> Op.storeFloat
                    | TurboString -> Op.storeString
                opc opcode [ Word vector.Id ] vector.Name
            | _ -> fail pos $"'{vector.Name}' is {kindName vector.Kind} and cannot be assigned as a simple variable."

    /// Element store: subscripts, then the value, then the store, which pops the value before
    /// the subscripts (spec 6.2).
    // TURBO-IR ASSUMPTION: a whole string-array element is stored with 182 and no slice bounds;
    // only 175 fetches and 176 sliced stores carry them (Gen_Array_Ref, P 24542-24602).
    member private this.EmitStoreElement(scope: Scope, name: string, indexes: HirExpr list, emitValue: TurboType -> unit, pos: SourcePosition) =
        if this.IsStringCharacter(name, indexes) then
            this.EmitStoreChar(scope, name, List.head indexes, emitValue, pos)
        else
            this.EmitStoreArrayElement(scope, name, indexes, emitValue, pos)

    member private this.EmitStoreArrayElement(scope: Scope, name: string, indexes: HirExpr list, emitValue: TurboType -> unit, pos: SourcePosition) =
        let vector = this.ArrayOf(name, pos)
        match vector.Type with
        | TurboString ->
            let elementIndexes, character = this.SplitStringSubscripts(vector, indexes)
            this.EmitIndexes(scope, vector, elementIndexes, pos)
            match character with
            | Some position ->
                this.EmitAs(scope, TurboInt, position)
                op0 Op.duplicateInt
                emitValue TurboString
                opc Op.insertString [ Word vector.Id ] vector.Name
            | None ->
                emitValue TurboString
                opc Op.storeStringElement [ Word vector.Id ] vector.Name
        | TurboInt ->
            this.EmitIndexes(scope, vector, indexes, pos)
            emitValue TurboInt
            opc Op.insertInt [ Word vector.Id ] vector.Name
        | TurboFloat ->
            this.EmitIndexes(scope, vector, indexes, pos)
            emitValue TurboFloat
            opc Op.insertFloat [ Word vector.Id ] vector.Name

    /// a$(n) = x$: a sliced store into a simple string (start, end, value, 176).
    // TURBO-IR ASSUMPTION: the slice bounds precede the value, as subscripts do for elements.
    member private this.EmitStoreChar(scope: Scope, name: string, index: HirExpr, emitValue: TurboType -> unit, pos: SourcePosition) =
        if this.IsArrayName(DynamicReadVar(name, HirType.String, pos)) then
            this.EmitStoreArrayElement(scope, name, [ index ], emitValue, pos)
        else
            let vector = this.RequireSimpleString(name, pos)
            this.EmitAs(scope, TurboInt, index)
            op0 Op.duplicateInt
            emitValue TurboString
            opc Op.insertString [ Word vector.Id ] vector.Name

    // -- statements ---------------------------------------------------------

    member private this.LineLabel(target: HirExpr, keyword: string, pos: SourcePosition) =
        match this.TryConstant target with
        | Some(ConstInt line) ->
            let candidates = declaredLines.GetViewBetween(line, Int32.MaxValue)
            if candidates.Count = 0 then
                fail pos $"{keyword} {line}: there is no line {line} or later."
                TurboIrLabels.lineBase
            else
                TurboIrLabels.lineBase + candidates.Min
        | _ ->
            // TURBO-IR GAP: Turbo branches only to constant line numbers (Check_Line_Reference,
            // P 22234); a computed GO TO or GO SUB has no IR form.
            fail pos $"{keyword} needs a constant line number; a computed target has no Turbo IR form."
            TurboIrLabels.lineBase

    member private this.ResolveLine(target: HirExpr) =
        match this.TryConstant target with
        | Some(ConstInt line) ->
            let candidates = declaredLines.GetViewBetween(line, Int32.MaxValue)
            if candidates.Count > 0 then referencedLines.Add candidates.Min |> ignore
        | _ -> ()

    member private this.EmitChannel(scope: Scope, channel: HirChannel option, keyword: string, pos: SourcePosition) =
        match channel with
        | None -> this.EmitInt 1
        | Some(ExplicitChannel expr) ->
            match this.TryConstant expr with
            | Some(ConstInt number) when number < 0 || number > 31 ->
                fail pos $"{keyword} #{number}: channel numbers must be 0 to 31."
            | _ -> ()
            this.EmitAs(scope, TurboInt, expr)
        | Some(ImplicitChannel _) ->
            fail pos $"{keyword} uses SB's '\\' channel form, which has no Turbo IR equivalent."

    /// PRINT (P 21758-21870): channel, 281, each item as a string then 186, 201 for a comma,
    /// and 203 unless the statement ends with ';' or ','.
    // TURBO-IR GAP: SB's front end keeps only ',' and the trailing ';'/','; it turns '\' into a
    // separate PRINT and drops '!' and 'TO', so 204 (!) and 205 (TO) are never emitted.
    member private this.EmitPrint(scope: Scope, channel: HirChannel option, args: HirExpr list, pos: SourcePosition) =
        let printScope = { scope with InPrintInput = true }
        this.EmitChannel(scope, channel, "PRINT", pos)
        op0 Op.printContext
        let isSentinel sentinel expr =
            match expr with
            | Literal(ConstString text, _, _) -> text = sentinel
            | _ -> false
        // SB records the separator after '#n' as a PRINT comma; it separates the channel, as
        // Explicit_Channel consumes it (P 21758-21766), so it prints nothing.
        let args =
            match channel, args with
            | Some(ExplicitChannel _), first :: rest when isSentinel printCommaSentinel first -> rest
            | _ -> args
        let items, keepCursor =
            match List.rev args with
            | last :: rest when isSentinel printContinueSentinel last -> List.rev rest, true
            | _ -> args, false
        for item in items do
            if isSentinel printCommaSentinel item then
                op0 Op.printComma
            elif not (isSentinel printContinueSentinel item) then
                this.EmitAs(printScope, TurboString, item)
                op0 Op.printString
        if not keepCursor then op0 Op.printNewline

    /// INPUT (P 21758-21914): channel, 282, prompts printed with 186, then for each target its
    /// subscripts, 116/148/180 and the store, then 203.
    member private this.EmitInput(scope: Scope, channel: HirChannel option, prompts: HirExpr list, targets: HirTarget list, pos: SourcePosition) =
        let inputScope = { scope with InPrintInput = true }
        this.EmitChannel(scope, channel, "INPUT", pos)
        op0 Op.inputContext
        for prompt in prompts do
            this.EmitAs(inputScope, TurboString, prompt)
            op0 Op.printString
        for target in targets do
            this.EmitStore(inputScope, target, fun turboType ->
                op0 (
                    match turboType with
                    | TurboInt -> Op.inputInt
                    | TurboFloat -> Op.inputFloat
                    | TurboString -> Op.inputString))
        op0 Op.printNewline

    member private this.EmitBuiltInStatement(scope: Scope, name: string, channel: HirChannel option, args: HirExpr list, pos: SourcePosition) =
        let noArguments () =
            if not args.IsEmpty || channel.IsSome then
                fail pos $"{name} takes no arguments here; a line number or channel argument has no Turbo IR form."
        match name with
        | "STOP"
        | "NEW" ->
            noArguments ()
            op0 Op.progEnd
        | "CLEAR" ->
            noArguments ()
            warn "CLEAR" "CLEAR destroys all strings in a Turbo-compiled program (Turbo's own warning)."
            op0 Op.clear
        | "RETRY" ->
            noArguments ()
            op Op.slow []
            op0 Op.retry
        | "CONTINUE" ->
            noArguments ()
            if scope.InHandler then
                op0 Op.continueOp
            else
                // As Turbo's parser: CONTINUE outside a WHEN ERROR block stops the program (P 17146).
                warn ("continue:" + describe pos) $"{describe pos}: CONTINUE outside WHEN ERROR is compiled as STOP, as Turbo does."
                op0 Op.progEnd
        | "POKE"
        | "POKE_W"
        | "POKE_L" ->
            match args with
            | [ address; value ] ->
                this.EmitAs(scope, TurboFloat, address)
                this.EmitAs(scope, TurboFloat, value)
                op0 (if name = "POKE" then Op.poke elif name = "POKE_W" then Op.pokeW else Op.pokeL)
            | _ -> this.ArityError(name, pos)
        | "RUN" ->
            fail pos "RUN has no Turbo IR equivalent in the specification."
        | _ ->
            if name = "LINE" || name = "LINE_R" || name = "ARC" || name = "ARC_R" then
                warn ("to:" + name) $"{name}: SB's front end turns the TO separators into plain arguments, so they are emitted as commas."
            this.WarnIfNotRom name
            let vector = this.Vector(name, ResidentVector, pos)
            // A resident procedure call marks the evaluation stack first (spec 8.3).
            op0 Op.markStack
            let descriptors = this.EmitRomArguments(scope, name, channel, args, pos)
            opc Op.romProcedure [ Word vector.Id; Bytes8 descriptors ] vector.Name

    member private this.ForVector(symbol: SymbolId, scope: Scope, pos: SourcePosition) =
        let name = this.SymbolName symbol
        let vector = this.NamedVector(name, pos)
        let isParameter =
            scope.Routine
            |> Option.exists (fun routine -> routine.Parameters |> List.exists (fun p -> p.Storage.Symbol = symbol))
        if isParameter then
            fail pos $"FOR variable '{vector.Name}' is a parameter; Turbo needs FOR storage (101/133 or LOCal 102/134)."
        if vector.Kind <> ScalarVector || vector.Type = TurboString then
            fail pos $"FOR variable '{vector.Name}' must be a numeric simple variable."
        vector

    /// One FOR range (spec 7.3, For_List P 23722-23826): start, end and step, 357 with the end
    /// label, then the range template with the loop variable and body label.
    member private this.EmitForRange(scope: Scope, vector: VectorInfo, start: HirExpr, finish: HirExpr, step: HirExpr, top: int, endLabel: int) =
        let turboType = vector.Type
        this.EmitAs(scope, turboType, start)
        this.EmitAs(scope, turboType, finish)
        let stepIsOne =
            match this.TryConstant step with
            | Some(ConstInt 1) -> true
            | Some(ConstFloat value) -> value = 1.0
            | _ -> false
        if turboType = TurboFloat && stepIsOne then
            // Float FOR with STEP 1 uses the faster 360 with no step on the stack. HIR supplies
            // STEP 1 when the source has none, which is when the parser uses 360.
            op Op.rememberEnd [ LabelRef endLabel ]
            opc Op.forStep1 [ Word vector.Id; LabelRef top ] vector.Name
        else
            this.EmitAs(scope, turboType, step)
            op Op.rememberEnd [ LabelRef endLabel ]
            opc (if turboType = TurboInt then Op.forRangeInt else Op.forRangeFloat) [ Word vector.Id; LabelRef top ] vector.Name

    member private this.EmitForInstance(scope: Scope, vector: VectorInfo, value: HirExpr, top: int) =
        this.EmitAs(scope, vector.Type, value)
        opc (if vector.Type = TurboInt then Op.forInstanceInt else Op.forInstanceFloat) [ Word vector.Id; LabelRef top ] vector.Name

    member private this.EmitForLoop(scope: Scope, loopId: LoopId, symbol: SymbolId, emitList: VectorInfo -> int -> int -> unit, body: HirBlock, pos: SourcePosition) =
        let vector = this.ForVector(symbol, scope, pos)
        let top = newLabel pos
        let endLabel = newLabel pos
        emitList vector top endLabel
        op0 Op.doneFor
        define top $"FOR {vector.Name}"
        let nextLabel = ref None
        let loopScope = { scope with Loops = scope.Loops.Add(loopId, ForLoop(vector, top, endLabel, nextLabel)) }
        this.EmitBlock(loopScope, body)
        match nextLabel.Value with
        | Some label -> define label $"NEXT {vector.Name}"
        | None -> ()
        opc (if vector.Type = TurboInt then Op.nextForInt else Op.nextForFloat) [ Word vector.Id ] vector.Name
        // 378 defines the (redefinable) loop end, the target of 357, 355 and EXIT (spec 4.2).
        op Op.possibleEnd [ LabelRef endLabel ]

    member private this.RestoreLabel(line: int, pos: SourcePosition) =
        let point =
            program.RestorePoints
            |> List.filter (fun point -> point.LineNumber >= line)
            |> List.sortBy (fun point -> point.LineNumber)
            |> List.tryHead
        match point with
        | Some point ->
            let (DataSlotId slot) = point.Slot
            match restoreLabels.TryGetValue slot with
            | true, label -> label
            | _ ->
                let label = newLabel pos
                restoreLabels[slot] <- label
                label
        | None ->
            match endOfDataLabel with
            | Some label -> label
            | None ->
                let label = newLabel pos
                endOfDataLabel <- Some label
                label

    member private this.EmitStmt(scope: Scope, stmt: HirStmt) =
        match stmt with
        | Assign(target, value, _) ->
            this.EmitStore(scope, target, fun turboType -> this.EmitAs(scope, turboType, value))
        | ProcCall(symbol, channel, args, pos) ->
            match routines.TryGetValue symbol with
            | true, routine ->
                if channel.IsSome then
                    fail pos $"a '#' channel argument to compiled procedure '{routine.Name}' has no Turbo IR form."
                if routine.ReturnType.IsSome then
                    fail pos $"function '{routine.Name}' is called as a procedure."
                let vector = this.Vector(routine.Name, RoutineVector, pos)
                this.EmitRoutineArguments(scope, routine, args, pos)
                opc Op.gosub [ LabelRef(TurboIrLabels.routineEntry vector.Id) ] vector.Name
            | _ -> fail pos $"'{this.SymbolName symbol}' is not a compiled procedure."
        | BuiltInCall(Print, channel, args, pos) -> this.EmitPrint(scope, channel, args, pos)
        | BuiltInCall(NamedBuiltIn name, channel, args, pos) ->
            this.EmitBuiltInStatement(scope, normalizeIdentifier name, channel, args, pos)
        | BuiltInCall(Reference, _, _, _) ->
            // REFERENCE is a Turbo compile-time declaration; HIR has already applied it as the
            // parameters' ReferenceBinding, which selects 294 and 246.
            ()
        | BuiltInCall(kind, _, _, pos) ->
            fail pos $"built-in statement {kind} has no Turbo IR lowering."
        | Input(channel, prompts, targets, pos) -> this.EmitInput(scope, channel, prompts, targets, pos)
        | WhenError(body, pos) ->
            // WHEN ERROR (P 23282-23358): threaded code, 327 to install the handler and skip to
            // the label after it, the handler, then 364 (CONTINUE) at END WHEN.
            if scope.InHandler then
                fail pos "WHEN ERROR blocks cannot be nested."
            let after = newLabel pos
            op Op.slow []
            op Op.whenGlobal [ LabelRef after ]
            this.EmitBlock({ scope with InHandler = true }, body)
            op0 Op.continueOp
            define after "END WHEN"
        | If(condition, thenBlock, elseBlock, pos) ->
            // IF (P 22298-22394): a float condition, 239 to the ELSE label, 242 over the ELSE part.
            this.EmitAs(scope, TurboFloat, condition)
            let elseLabel = newLabel pos
            op Op.ifFalse [ LabelRef elseLabel ]
            this.EmitBlock(scope, thenBlock)
            match elseBlock with
            | Some block ->
                let endLabel = newLabel pos
                op Op.jump [ LabelRef endLabel ]
                define elseLabel "ELSE"
                this.EmitBlock(scope, block)
                define endLabel "END IF"
            | None -> define elseLabel "END IF"
        | For(loopId, symbol, start, finish, step, body, pos) ->
            this.EmitForLoop(scope, loopId, symbol, (fun vector top endLabel ->
                this.EmitForRange(scope, vector, start, finish, step, top, endLabel)), body, pos)
        | ForSequence(loopId, symbol, prefix, start, finish, suffix, step, body, pos) ->
            this.EmitForLoop(scope, loopId, symbol, (fun vector top endLabel ->
                for value in prefix do this.EmitForInstance(scope, vector, value, top)
                this.EmitForRange(scope, vector, start, finish, step, top, endLabel)
                for value in suffix do this.EmitForInstance(scope, vector, value, top)), body, pos)
        | Repeat(loopId, _, body, pos) ->
            // REPeat (P 22686-22702, 23222-23238): top label, body, 242 back, end label.
            let top = newLabel pos
            let endLabel = newLabel pos
            define top "REPeat"
            this.EmitBlock({ scope with Loops = scope.Loops.Add(loopId, RepeatLoop(top, endLabel)) }, body)
            op Op.jump [ LabelRef top ]
            define endLabel "END REPeat"
        | Exit(loopId, pos) ->
            match scope.Loops.TryFind loopId with
            | Some(ForLoop(_, _, endLabel, _))
            | Some(RepeatLoop(_, endLabel)) -> op Op.exitOp [ LabelRef endLabel ]
            | None -> fail pos "EXIT is not inside the loop it names."
        | Next(loopId, pos) ->
            match scope.Loops.TryFind loopId with
            | Some(ForLoop(_, _, _, nextLabel)) ->
                // TURBO-IR ASSUMPTION: a NEXT inside a FOR body jumps to the loop's closing
                // 230/232, so an exhausted loop ends as SB's backends do. Turbo's parser emits
                // 230/232 in place, where an exhausted loop carries on after the NEXT.
                let label =
                    match nextLabel.Value with
                    | Some label -> label
                    | None ->
                        let label = newLabel pos
                        nextLabel.Value <- Some label
                        label
                op Op.jump [ LabelRef label ]
            | Some(RepeatLoop(top, _)) -> op Op.nextRepeat [ LabelRef top ]
            | None -> fail pos "NEXT is not inside the loop it names."
        | Goto(target, pos) -> op Op.jump [ LabelRef(this.LineLabel(target, "GO TO", pos)) ]
        | Gosub(target, pos) -> op Op.gosub [ LabelRef(this.LineLabel(target, "GO SUB", pos)) ]
        | OnGoto(selector, targets, pos)
        | OnGosub(selector, targets, pos) ->
            // ON ... GO TO/SUB (P 22514-22606): integer selector, 240/241, then the address table
            // in native mode, then back to threaded code.
            let isGosub =
                match stmt with
                | OnGosub _ -> true
                | _ -> false
            this.EmitAs(scope, TurboInt, selector)
            let returnLabel = if isGosub then Some(newLabel pos) else None
            match returnLabel with
            | Some label -> op Op.onGosub [ Word targets.Length; LabelRef label ]
            | None -> op Op.onGoto [ Word targets.Length ]
            op0 Op.fast
            for target in targets do
                op Op.address [ LabelRef(this.LineLabel(target, (if isGosub then "ON GO SUB" else "ON GO TO"), pos)) ]
            op0 Op.slow
            match returnLabel with
            | Some label -> define label "after ON GO SUB"
            | None -> ()
        | Return(value, pos) ->
            match value, scope.Routine with
            | Some expr, Some routine when routine.ReturnType.IsSome ->
                let resultType = this.TypeOfName routine.Name
                this.EmitAs(scope, resultType, expr)
                if resultType = TurboString then op0 Op.upString
                op0 Op.returnOp
            | Some _, _ -> fail pos "RETurn with a value is only allowed in a function."
            | None, Some routine when routine.ReturnType.IsSome ->
                fail pos $"function '{routine.Name}' must RETurn a value."
            | None, _ -> op0 Op.returnOp
        | LineNumber(line, pos) ->
            if referencedLines.Contains line then
                if line > 32767 then fail pos $"line {line} is above 32767 and cannot be a branch target."
                define (TurboIrLabels.lineBase + line) $"line {line}"
            if options.LineNumbers then op Op.lineNumber [ Word line ]
        | Restore(None, _) -> op Op.restore [ LabelRef TurboIrLabels.data ]
        | Restore(Some target, pos) ->
            match this.TryConstant target with
            | Some(ConstInt line) -> op Op.restore [ LabelRef(this.RestoreLabel(line, pos)) ]
            | _ -> fail pos "RESTORE needs a constant line number; a computed target has no Turbo IR form."
        | Read(targets, _) ->
            for target in targets do
                this.EmitStore(scope, target, fun turboType ->
                    op0 (
                        match turboType with
                        | TurboInt -> Op.readInt
                        | TurboFloat -> Op.readFloat
                        | TurboString -> Op.readString))
        | Remark _ ->
            // Comments emit nothing; TURBO_ directives (which SB turns into remarks) were applied
            // or reported before emission.
            ()

    member private this.EmitBlock(scope: Scope, block: HirBlock) =
        for stmt in block do
            this.EmitStmt(scope, stmt)

    // -- declarations -------------------------------------------------------

    /// Executable DIM (spec 6.2, Parse_key_Decl P 24174-24218): the bounds, then 126/158/190
    /// (global) or 115/147/179 (LOCal) with the dimension counter R-1 and the vector.
    member private this.EmitDim(storage: HirStorage, isGlobal: bool) =
        let vector = this.Vector(storage.Name, ArrayVector, storage.Position)
        match storage.Dimensions with
        | Some dims when not dims.IsEmpty ->
            let isString = vector.Type = TurboString
            if isString && dims.Length < 2 then
                fail storage.Position $"string array '{vector.Name}' needs at least one dimension and a length."
            else
                for bound in dims do
                    if bound < 0 || bound > 32767 then
                        fail storage.Position $"DIM bound {bound} of '{vector.Name}' is outside 0..32767."
                    else
                        this.EmitInt bound
                let counter = dims.Length - (if isString then 2 else 1)
                let opcode =
                    match isGlobal, vector.Type with
                    | true, TurboInt -> Op.globalArrayInt
                    | true, TurboFloat -> Op.globalArrayFloat
                    | true, TurboString -> Op.globalArrayString
                    | false, TurboInt -> Op.localArrayInt
                    | false, TurboFloat -> Op.localArrayFloat
                    | false, TurboString -> Op.localArrayString
                opc opcode [ Word counter; Word vector.Id ] vector.Name
        | _ ->
            // TURBO-IR GAP: SB's semantic analysis keeps only DIM bounds it can fold to constants
            // (SemanticAnalyzer, DimStmt), and HIR drops the DIM statement itself.
            fail storage.Position $"'{vector.Name}' has no constant DIM bounds in the HIR, so its DIM cannot be emitted."

    member private this.EmitRoutine(routine: HirRoutine) =
        let vector = this.Vector(routine.Name, RoutineVector, routine.Position)
        let keyword = if routine.ReturnType.IsSome then "FuNction" else "PROCedure"
        define (TurboIrLabels.routineEntry vector.Id) $"DEFine {keyword} {vector.Name}"
        // Formals bind in reverse order: the last argument is on top (spec 7.2, P 23962-24006).
        for parameter in List.rev routine.Parameters do
            let formal = this.NamedVector(parameter.Storage.Name, parameter.Storage.Position)
            if this.IsReferenceFormal parameter then
                opc Op.createRef [ Word formal.Id ] formal.Name
            else
                match formal.Type with
                | TurboString -> opc Op.passString [ Word formal.Id ] formal.Name
                | TurboInt ->
                    opc Op.localInt [ Word formal.Id ] formal.Name
                    opc Op.storeInt [ Word formal.Id ] formal.Name
                | TurboFloat ->
                    opc Op.localFloat [ Word formal.Id ] formal.Name
                    opc Op.storeFloat [ Word formal.Id ] formal.Name
        // LOCal declarations follow the formals (P 24198-24254). SB folds LOCal into the routine's
        // locals, so they are all declared at entry.
        for local in routine.Locals do
            let localVector = this.NamedVector(local.Name, local.Position)
            match localVector.Kind with
            | ArrayVector -> this.EmitDim(local, false)
            | _ ->
                match localVector.Type with
                | TurboString ->
                    // TURBO-IR GAP: HIR does not keep a LOCal string's declared length, so every
                    // LOCal string gets the default length, as an unsized one does in Turbo.
                    this.EmitInt options.LocalStringLength
                    opc Op.localString [ Word localVector.Id ] localVector.Name
                | TurboInt ->
                    opc (if localForSymbols.Contains local.Symbol then Op.localForInt else Op.localInt) [ Word localVector.Id ] localVector.Name
                | TurboFloat ->
                    opc (if localForSymbols.Contains local.Symbol then Op.localForFloat else Op.localFloat) [ Word localVector.Id ] localVector.Name
        let scope = { Routine = Some routine; Loops = Map.empty; InPrintInput = false; InHandler = false }
        this.EmitBlock(scope, routine.Body)
        op0 Op.returnOp

    /// DATA records (spec 8.1, Record_Data P 14758-14938).
    member private this.EmitDataItem(entry: HirDataEntry) =
        let rec emitInteger value =
            if value >= -2998 && value <= 25670 then opc Op.dataShort [ Word(-value - 7096) ] (string value)
            elif fitsWord value then op Op.dataInt [ Word value ]
            else emitFloat (float value)
        and emitFloat (value: float) =
            if value >= -32768.0 && value <= 32767.0 && value = Math.Round value then
                // An integral value is stored as integer DATA, as the parser does.
                emitInteger (int value)
            else
                match encodeQdosFloat value with
                | Result.Ok(exponent, high, low) ->
                    opc Op.dataFloat [ Word exponent; Word high; Word low ] (value.ToString("R", CultureInfo.InvariantCulture))
                | Result.Error message -> fail entry.Position message
        match this.TryConstant entry.Value with
        | Some(ConstString text) ->
            match this.EncodeText(text, entry.Position) with
            | Some bytes -> op Op.dataString [ Text16 bytes ]
            | None -> ()
        | Some(ConstInt value) -> emitInteger value
        | Some(ConstFloat value) -> emitFloat value
        | None -> fail entry.Position "DATA items must be constants (optionally signed) for Turbo."

    member private this.EmitData() =
        for entry in program.DataEntries do
            let (DataSlotId slot) = entry.Slot
            match restoreLabels.TryGetValue slot with
            | true, label ->
                let lineText = entry.LineNumber |> Option.map string |> Option.defaultValue "?"
                define label $"DATA line {lineText}"
            | _ -> ()
            this.EmitDataItem entry
        match endOfDataLabel with
        | Some label -> define label "end of DATA"
        | None -> ()
        op0 Op.dataEnd

    member private this.EmitWindows() =
        // Clone_Window order (P 17878-17902): #1, then #2, then #0.
        let channels =
            match windowCount with
            | 0 -> []
            | 1 -> [ 1 ]
            | 2 -> [ 1; 0 ]
            | _ -> [ 1; 2; 0 ]
        for channel in channels do
            match options.Windows |> List.tryFind (fun window -> window.Channel = channel) with
            | Some w ->
                opc Op.initWindow
                    [ Word channel; Word w.BorderColour; Word w.BorderWidth; Word w.Paper; Word w.Ink
                      Word w.Width; Word w.Height; Word w.X; Word w.Y ]
                    $"window #{channel}"
            | None -> errors.Add $"No definition for startup window #{channel}."

    member private this.ApplyDirective(text: string, pos: SourcePosition) =
        let upper = text.Trim().ToUpperInvariant()
        if upper.StartsWith "TURBO_WINDO" then
            match Int32.TryParse(upper.Substring("TURBO_WINDO".Length).Trim()) with
            | true, count when count >= 0 && count <= 3 -> windowCount <- count
            | true, count -> fail pos $"TURBO_windo {count}: only windows #0 to #2 have startup definitions, so use 0 to 3."
            | _ -> fail pos "TURBO_windo needs a constant window count."
        else
            warn ("directive:" + upper) $"{describe pos}: Turbo directive '{text}' configures Turbo's compiler or intercom block, not the IR, so it was not emitted."

    // -- program ------------------------------------------------------------

    member private this.Prepare() =
        for storage in program.Globals do
            storages[storage.Symbol] <- storage
        for routine in program.Routines do
            routines[routine.Symbol] <- routine
            for parameter in routine.Parameters do
                storages[parameter.Storage.Symbol] <- parameter.Storage
            for local in routine.Locals do
                storages[local.Symbol] <- local

        let allBlocks = program.Main :: (program.Routines |> List.map _.Body)
        for block in allBlocks do
            block
            |> iterStatements (fun stmt ->
                match stmt with
                | LineNumber(line, _) -> declaredLines.Add line |> ignore
                | _ -> ())
        for block in allBlocks do
            block
            |> iterStatements (fun stmt ->
                match stmt with
                | Goto(target, _)
                | Gosub(target, _) -> this.ResolveLine target
                | OnGoto(_, targets, _)
                | OnGosub(_, targets, _) -> targets |> List.iter this.ResolveLine
                | Remark(text, pos) when text.Trim().StartsWith("TURBO_", StringComparison.OrdinalIgnoreCase) ->
                    this.ApplyDirective(text, pos)
                | _ -> ())

        // A formal is passed by reference when REFERENCE says so, when HIR types it as an array,
        // or when the routine indexes it. Turbo has no by-value arrays.
        // TURBO-IR ASSUMPTION: an indexed formal is taken to be an array reference.
        for routine in program.Routines do
            let formalNames =
                routine.Parameters |> List.map (fun p -> p.Storage.Symbol, p.Storage.Name) |> Map.ofList
            let isArrayUse symbol (indexes: HirExpr list) =
                match formalNames.TryFind symbol with
                | Some name -> not (this.TypeOfName name = TurboString && indexes.Length = 1)
                | None -> false
            routine.Body
            |> iterStatements (fun stmt ->
                for expr in statementExprs stmt do
                    expr
                    |> iterExpr (fun inner ->
                        match inner with
                        | ReadArrayElem(symbol, indexes, _, _) when isArrayUse symbol indexes -> arrayFormals.Add symbol |> ignore
                        | _ -> ()))
            for parameter in routine.Parameters do
                match parameter.Storage.Type with
                | Array _ -> arrayFormals.Add parameter.Storage.Symbol |> ignore
                | _ -> ()
                if parameter.Binding = ReferenceBinding || arrayFormals.Contains parameter.Storage.Symbol then
                    referenceFormals.Add parameter.Storage.Symbol |> ignore
            let locals = routine.Locals |> List.map _.Symbol |> Set.ofList
            routine.Body
            |> iterStatements (fun stmt ->
                match stmt with
                | For(_, symbol, _, _, _, _, _)
                | ForSequence(_, symbol, _, _, _, _, _, _, _) when locals.Contains symbol -> localForSymbols.Add symbol |> ignore
                | _ -> ())

        // Vector ids: globals, then each routine's name, formals and locals, in HIR order. Names
        // first met during emission (dynamic references, resident routines) follow.
        let register (storage: HirStorage) =
            let kind =
                match storage.Type with
                | Array _ -> ArrayVector
                | _ -> ScalarVector
            let vector = this.Vector(storage.Name, kind, storage.Position)
            match storage.Type, storage.Dimensions with
            | Array _, Some dims when not dims.IsEmpty ->
                vector.HasDescriptor <- true
                match vector.Dimensions with
                | Some existing when existing.Length <> dims.Length ->
                    fail storage.Position $"'{vector.Name}' is DIMmed with {existing.Length} and {dims.Length} bounds; one vector needs one shape."
                | Some _ -> ()
                | None -> vector.Dimensions <- Some dims
            | Array _, _ -> vector.HasDescriptor <- true
            | _ -> ()
        for storage in program.Globals do
            if (this.Manifest storage.Name).IsNone then register storage
        for routine in program.Routines do
            this.Vector(routine.Name, RoutineVector, routine.Position) |> ignore
            for parameter in routine.Parameters do
                let kind = if arrayFormals.Contains parameter.Storage.Symbol then ArrayVector else ScalarVector
                this.Vector(parameter.Storage.Name, kind, parameter.Storage.Position) |> ignore
            for local in routine.Locals do
                register local

        let markGlobalFor (routine: HirRoutine option) (block: HirBlock) =
            block
            |> iterStatements (fun stmt ->
                match stmt with
                | For(_, symbol, _, _, _, _, _)
                | ForSequence(_, symbol, _, _, _, _, _, _, _) when not (localForSymbols.Contains symbol) ->
                    let isFormal =
                        routine |> Option.exists (fun r -> r.Parameters |> List.exists (fun p -> p.Storage.Symbol = symbol))
                    if not isFormal then
                        match vectors.TryGetValue(normalizeIdentifier (this.SymbolName symbol)) with
                        | true, vector -> vector.IsGlobalForVariable <- true
                        | _ -> ()
                | _ -> ())
        markGlobalFor None program.Main
        for routine in program.Routines do
            markGlobalFor (Some routine) routine.Body

    member this.Run() : Result<TurboIrOutput, string list> =
        this.Prepare()

        current <- code
        // Global DIMs run at the start of the program body.
        // TURBO-IR ASSUMPTION: HIR drops DIM statements and keeps only each array's bounds, so a
        // global array is DIMmed once on entry rather than where its DIM statement stood.
        for storage in program.Globals do
            match storage.Type with
            | Array _ -> this.EmitDim(storage, true)
            | _ -> ()
        let mainScope = { Routine = None; Loops = Map.empty; InPrintInput = false; InHandler = false }
        this.EmitBlock(mainScope, program.Main)
        // Runtime end before the routines, so control cannot fall into a definition (spec 7.2).
        op0 Op.progEnd
        for routine in program.Routines do
            this.EmitRoutine routine

        current <- data
        this.EmitData()

        let initialisation = ResizeArray<IrItem>()
        current <- initialisation
        this.EmitWindows()
        // Pass-1 storage (P 17910-18050): a descriptor for each array and default-length
        // storage for each simple string, in vector order.
        for vector in vectorOrder do
            match vector.Kind with
            | ArrayVector when vector.HasDescriptor ->
                match vector.Dimensions with
                | Some dims -> opc Op.arrayDescriptor [ Word dims.Length; Word vector.Id ] vector.Name
                | None -> ()
            | ScalarVector when vector.Type = TurboString -> opc Op.simpleString [ Word vector.Id ] vector.Name
            | _ -> ()

        let secondInitialisation = ResizeArray<IrItem>()
        current <- secondInitialisation
        // Pass-2 storage: every numeric scalar, including names used only as LOCals or formals,
        // so every vector is bound (P 17922-18034).
        for vector in vectorOrder do
            match vector.Kind, vector.Type with
            | ScalarVector, TurboInt -> opc (if vector.IsGlobalForVariable then Op.globalForInt else Op.globalInt) [ Word vector.Id ] vector.Name
            | ScalarVector, TurboFloat -> opc (if vector.IsGlobalForVariable then Op.globalForFloat else Op.globalFloat) [ Word vector.Id ] vector.Name
            | _ -> ()

        let residentNames = ResizeArray<IrItem>()
        current <- residentNames
        for vector in vectorOrder do
            if vector.Kind = ResidentVector then
                match this.EncodeText(vector.Name, program.Main |> List.tryHead |> Option.map (fun _ -> { BasicLineNo = None; EditorLineNo = 0; Column = 0 }) |> Option.defaultValue { BasicLineNo = None; EditorLineNo = 0; Column = 0 }) with
                | Some bytes when bytes.Length <= 255 -> opc Op.romName [ Word vector.Id; Bytes8 bytes ] vector.Name
                | _ -> errors.Add $"Resident routine name '{vector.Name}' cannot be encoded."

        // Vector table: ids 1..V with slot 0 reserved, N = V + 1 (spec 4.1). The displacement
        // vector_table + 4*id is a signed word, so 4*N + 4 must stay below 32768.
        let vectorCount = vectorOrder.Count + 1
        if 4 * vectorCount + 4 > 32767 then
            errors.Add $"The program needs {vectorOrder.Count} vectors; Turbo's vector table addressing allows at most 8190."

        if errors.Count > 0 then
            Result.Error(List.ofSeq errors)
        else
            let terminator = Instruction(Op.romName, [ Word(-1); Bytes8 [||] ], "end of list")
            let items =
                [ yield Instruction(Op.entry, [ Word vectorCount ], "vector table")
                  yield Instruction(Op.slow, [], "")
                  yield Instruction(Op.jump, [ LabelRef TurboIrLabels.initialisation ], "")
                  yield Instruction(Op.fast, [], "")
                  yield LabelDef(TurboIrLabels.data, "DATA")
                  yield! data
                  yield Instruction(Op.slow, [], "")
                  yield LabelDef(TurboIrLabels.initialisation, "first initialisation: windows, arrays, strings")
                  yield! initialisation
                  yield Instruction(Op.jump, [ LabelRef TurboIrLabels.secondInitialisation ], "")
                  yield Instruction(Op.slow, [], "")
                  yield LabelDef(TurboIrLabels.body, "program body")
                  yield Instruction(Op.retryHere, [], "")
                  yield Instruction(Op.retry1Here, [], "")
                  yield! code
                  yield Instruction(Op.jump, [ LabelRef TurboIrLabels.afterLinkage ], "")
                  yield Instruction(Op.fast, [], "")
                  yield LabelDef(TurboIrLabels.linkage, "GLOBAL/EXTERNAL linkage list (empty)")
                  yield terminator
                  yield Instruction(Op.slow, [], "")
                  yield LabelDef(TurboIrLabels.afterLinkage, "")
                  yield Instruction(Op.slow, [], "")
                  yield LabelDef(TurboIrLabels.secondInitialisation, "second initialisation: scalars")
                  yield! secondInitialisation
                  yield Instruction(Op.lastGlobal, [], "")
                  yield Instruction(Op.restore, [ LabelRef TurboIrLabels.data ], "")
                  yield Instruction(Op.jump, [ LabelRef TurboIrLabels.body ], "")
                  yield Instruction(Op.fast, [], "")
                  yield LabelDef(TurboIrLabels.residentNames, "resident routine names")
                  yield! residentNames
                  yield terminator
                  yield Instruction(Op.address, [ LabelRef TurboIrLabels.residentNames ], "startup trailer")
                  yield Instruction(Op.dataShort, [ Word 0 ], "own module number")
                  yield Instruction(Op.address, [ LabelRef TurboIrLabels.linkage ], "")
                  yield Instruction(Op.endOfIr, [], "") ]
            let bytes = serialize items
            // Self-check: decode the bytes against the catalogue and validate them.
            let roundTrip =
                match decode bytes with
                | Result.Ok decoded -> decoded = (items |> List.map normalizeItem)
                | Result.Error _ -> false
            match validate bytes, roundTrip with
            | [], true ->
                Result.Ok
                    { Items = items
                      Bytes = bytes
                      Listing = formatListing items
                      Warnings = List.ofSeq warnings }
            | problems, roundTripOk ->
                let roundTripProblem = if roundTripOk then [] else [ "the stream does not decode back to the emitted records." ]
                Result.Error(problems @ roundTripProblem |> List.map (fun problem -> $"Internal Turbo IR error: {problem}"))

/// Generates the Turbo IR stream for a lowered program. Errors name every construct that has no
/// IR form; warnings name every construct emitted with reduced fidelity.
let generateTurboIrFromHir (options: TurboIrOptions) (program: HirProgram) : Result<TurboIrOutput, string list> =
    Emitter(options, program).Run()
