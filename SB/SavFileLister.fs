module SB.SavFileLister
// Port of Norman Dunbar's savFileLister.c

open System
open System.IO

type NameTableEntry = {
    Offset: int
    NameType: uint16
    LineNumber: int16
    NameLength: uint16
    Name: string
}

let maxNameSize = 32
let validHeaders = [| ("Q", "1", 0uy, 0uy); ("Q", "1", 2uy, 192uy); ("Q", "1", 3uy, 128uy); ("Q", "1", 0uy, 128uy) |]

let readUInt16 (reader: BinaryReader) = reader.ReadUInt16()
let readUInt8 (reader: BinaryReader) = reader.ReadByte()
let readString (reader: BinaryReader) length = 
    let bytes = reader.ReadBytes(int length)
    System.Text.Encoding.ASCII.GetString(bytes).TrimEnd('\u0000')

let decodeHeader (reader: BinaryReader) =
    let head = [| readUInt8 reader; readUInt8 reader; readUInt8 reader; readUInt8 reader |]
    let isValid = validHeaders |> Array.exists (fun (c1, c2, b1, b2) -> 
        (char head.[0] = c1) && (char head.[1] = c2) && (head.[2] = b1) && (head.[3] = b2)
    )
    if not isValid then failwithf "Invalid SAV file header: %A" head

    let nameTableEntries = readUInt16 reader
    let nameTableLength = readUInt16 reader
    let programLines = readUInt16 reader

    printfn "Header Details:"
    printfn "Name Table Entries: %d" nameTableEntries
    printfn "Name Table Length: %d" nameTableLength
    printfn "Program Lines: %d" programLines

    (nameTableEntries, programLines)

let decodeNameTable nameTableEntries (reader: BinaryReader) =
    let nameTable = 
        [| for _ in 1..(int nameTableEntries) do
            let offset = int reader.BaseStream.Position
            let nameType = readUInt16 reader
            let lineNumber = int16 (readUInt16 reader)
            let nameLength = readUInt16 reader
            let name = readString reader nameLength
            yield { Offset = offset; NameType = nameType; LineNumber = lineNumber; NameLength = nameLength; Name = name } |]

    printfn "\nName Table:"
    nameTable |> Array.iter (fun entry -> printfn "Offset: %X, Name: %s, Type: %d" entry.Offset entry.Name entry.NameType)
    nameTable

let decodeProgram programLines (reader: BinaryReader) =
    printfn "\nDecoding program content..."
    for _ in 1..(int programLines) do
        let lineSize = readUInt16 reader
        let flag = readUInt16 reader
        if flag <> 0x8D00us then
            failwith "Program structure mismatch."
        
        let lineNumber = readUInt16 reader
        printfn "%d " lineNumber

        let mutable doneProcessing = false
        while not doneProcessing do
            match readUInt8 reader with
            | 0x80uy -> let spaces = readUInt8 reader in printf "%s" (String.replicate (int spaces) " ")
            | 0x81uy -> let keywordIndex = readUInt8 reader in printf "Keyword[%d] " keywordIndex
            | 0x84uy -> let symbol = readUInt8 reader in printf "Symbol[%d] " symbol
            | 0x8Euy -> let separator = readUInt8 reader in printf "Separator[%d] " separator
            | 0x8Cuy -> let _ = readUInt8 reader in let textLength = readUInt16 reader in let text = readString reader textLength in printf "%s " text
            | 0x8Duy -> doneProcessing <- true
            | _ -> doneProcessing <- true
        printfn ""

let processFile filename =
    use reader = new BinaryReader(File.OpenRead(filename))
    let (nameTableEntries, programLines) = decodeHeader reader
    let _ = decodeNameTable nameTableEntries reader
    decodeProgram programLines reader
    printfn "\nDecoding complete."

[<EntryPoint>]
let main args =
    if args.Length <> 1 then
        printfn "Usage: savFileLister <sav file>"
        -1
    else
        try
            processFile args.[0]
            0
        with
        | ex -> printfn "Error: %s" ex.Message; -1
