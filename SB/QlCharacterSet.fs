module QlCharacterSet

// QlCharacterSet maps between .NET text and the Sinclair QL character set, as QuillToDocx's
// QuillCharset does: ASCII except 0x60 (pound) and 0x7F (copyright), then accented letters,
// Greek letters and arrows from 0x80 to 0xBF. 0xB5 is the SMSQ/E euro sign (the original ROM
// had a script mark there). Source files written on a QL use these bytes, and the Turbo IR
// backend writes string literals in them.

open System
open System.Collections.Generic
open System.IO
open System.Text

let private highCharacters =
    "äãåéöõøüçñæœáàâë" // 80
    + "èêïíìîóòôúùûß¢¥`" // 90
    + "ÄÃÅÉÖÕØÜÇÑÆŒαδθλ" // A0
    + "µπφ¡¿€§¤«»°÷←→↑↓" // B0

/// The character a QL byte stands for. Bytes above 0xBF are not QL characters; like QuillCharset,
/// they are read as Latin-1.
let toChar (code: byte) =
    match int code with
    | 0x60 -> '£'
    | 0x7F -> '©'
    | value when value < 0x80 -> char value
    | value when value <= 0xBF -> highCharacters[value - 0x80]
    | value -> char value

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
/// never substituted: a program literal must reach the QL exactly or not at all.
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

/// Text from QL bytes.
let decode (bytes: byte[]) = String(bytes |> Array.map toChar)

let private strictUtf8 = UTF8Encoding(false, true)

/// Reads a source file. A file with a byte-order mark, or one that is valid UTF-8, is read as
/// Unicode as before; any other file is taken to have been written on a QL and is read in the QL
/// character set, so its accented letters survive. A pure-ASCII file reads the same either way,
/// except that a QL-native file's 0x60 then means a backtick rather than a pound sign.
let readSourceText (path: string) =
    let bytes = File.ReadAllBytes path
    let hasBom =
        (bytes.Length >= 3 && bytes[0] = 0xEFuy && bytes[1] = 0xBBuy && bytes[2] = 0xBFuy)
        || (bytes.Length >= 2 && ((bytes[0] = 0xFFuy && bytes[1] = 0xFEuy) || (bytes[0] = 0xFEuy && bytes[1] = 0xFFuy)))
    if hasBom then
        File.ReadAllText path
    else
        try
            strictUtf8.GetString bytes
        with :? DecoderFallbackException ->
            decode bytes
