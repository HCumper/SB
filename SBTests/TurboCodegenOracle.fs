module SBTests.TurboCodegenOracle

// Runs the genuine Turbo code generator (Codegen 3.27 = codegen 4.10 + library 5.37, kept in
// TurboReference/codegen_task.bin) on an IR stream inside a minimal QDOS environment, so the
// F# port can be checked against it byte for byte. The environment provides what codegen uses:
// the system variables with the object list holding the parser's intercom block (spec 11), job
// registers, TRAP #1/#2/#3 calls on screen and file channels, and the CN.ITOD and UT.ERR vectored
// utilities. The IR is handed over in memory, as the parser does with pointer P+6.

open System
open System.Collections.Generic
open System.IO
open System.Text

open SBTests.M68kEmulator

type OracleSettings = {
    /// Output file name as the parser passes it, with the '_temp' suffix.
    OutputName: string
    TaskName: string
    DataspaceKiB: int
    StackSize: int
    /// Turbo's reference-string flag (bit 0 of the stack word).
    ReferenceStrings: bool
    SmallCode: bool
}

let defaultSettings = {
    OutputName = "ram1_test_temp"
    TaskName = "test"
    DataspaceKiB = 20
    StackSize = 350
    ReferenceStrings = false
    SmallCode = false
}

type OracleResult = {
    /// The intercom status word codegen leaves: -1 for success, else the error count.
    Status: int
    /// The object file codegen wrote (QDOS header not included).
    Output: byte[]
    /// File type and dataspace codegen set with FS.HEADS.
    FileType: int
    Dataspace: int
    /// Text codegen wrote to its screen channels (progress and error messages).
    Screen: string
    Steps: int64
}

let private memorySize = 0x100000
let private sysvarBase = 0x28000u
let private jobBase = 0x40000u
let private irBase = 0x80000u
let private objectBase = 0xC0000u
let private heapBase = 0xD0000u
let private hookBase = 0x1000u

let private qdosHeaderLength = 30

let private codegenTaskPath =
    Path.Combine(__SOURCE_DIRECTORY__, "TurboReference", "codegen_task.bin")

/// The reference code generator file, with its 30-byte "]!QDOS File Header".
let codegenTask = lazy (File.ReadAllBytes codegenTaskPath)

type private Channel = {
    Name: string
    IsFile: bool
    mutable Position: int
}

let run (settings: OracleSettings) (ir: byte[]) : OracleResult =
    let task = codegenTask.Value
    let code = task[qdosHeaderLength..]
    let readHeaderLong offset =
        (int task[offset] <<< 24) ||| (int task[offset + 1] <<< 16) ||| (int task[offset + 2] <<< 8) ||| int task[offset + 3]
    let jobDataspace = readHeaderLong 22
    let memory = Array.zeroCreate<byte> memorySize
    let cpu = Cpu(memory)
    let screen = StringBuilder()
    let files = Dictionary<string, ResizeArray<byte>>(StringComparer.OrdinalIgnoreCase)
    let channels = Dictionary<uint32, Channel>()
    let nextChannel = ref 0x00010000u
    let fileType = ref 0
    let dataspace = ref 0
    let clock = ref 1000u
    let heap = ref heapBase

    let writeWord (address: uint32) (value: int) = cpu.WriteWord(address, uint32 value)
    let writeLong (address: uint32) (value: uint32) = cpu.WriteLong(address, value)
    let writeBytes (address: uint32) (bytes: byte[]) = Array.blit bytes 0 memory (int address) bytes.Length

    /// Writes a QDOS word-counted string padded to even length; returns the address after it.
    let writeQdosString (address: uint32) (text: string) =
        let bytes = Encoding.ASCII.GetBytes text
        writeWord address bytes.Length
        writeBytes (address + 2u) bytes
        address + 2u + uint32 ((bytes.Length + 1) &&& ~~~1)

    let readQdosString (address: uint32) =
        let length = int (cpu.ReadWord address)
        Encoding.ASCII.GetString(memory, int address + 2, length)

    // System variables: SV.RAMT (top of RAM) and the object list holding the intercom block.
    writeLong (sysvarBase + 0x20u) (uint32 memorySize)
    writeLong (sysvarBase + 0x160u) objectBase

    // The intercom object (spec 11.1 and 11.2).
    writeLong objectBase 0u
    writeLong (objectBase + 4u) (objectBase + 8u)
    writeQdosString (objectBase + 8u) "D.P.:TURBO:INTER-COM" |> ignore
    let parameters = objectBase + 30u
    writeWord parameters 0
    writeWord (parameters + 2u) settings.DataspaceKiB
    memory[int parameters + 4] <- 0uy
    memory[int parameters + 5] <- (if settings.SmallCode then 1uy else 0uy)
    writeLong (parameters + 6u) irBase
    let afterReport = writeQdosString (parameters + 10u) ""
    let afterOutput = writeQdosString afterReport settings.OutputName
    let afterTask = writeQdosString afterOutput settings.TaskName
    if afterTask > parameters + 110u then failwith "Intercom strings are too long."
    writeBytes (parameters + 110u) (Encoding.ASCII.GetBytes "5.09")
    writeWord (parameters + 114u) (settings.StackSize ||| (if settings.ReferenceStrings then 1 else 0))

    // The IR stream, in memory as the parser leaves it.
    if irBase + uint32 ir.Length > objectBase then failwith "The IR stream is too long for the oracle."
    writeBytes irBase ir

    // The job: code, then its dataspace, with A4/A5 the data offsets and A7 the stack top.
    writeBytes jobBase code
    let dataOffset = uint32 ((code.Length + 1) &&& ~~~1)
    let topOffset = dataOffset + uint32 jobDataspace
    if jobBase + topOffset > irBase then failwith "The job does not fit below the IR stream."
    cpu.A[6] <- jobBase
    cpu.A[4] <- dataOffset
    cpu.A[5] <- topOffset
    cpu.A[7] <- jobBase + topOffset - 4u
    cpu.Pc <- jobBase

    // Vectored utilities: the ROM vector words point at hook addresses.
    let hook vector (index: uint32) (action: Cpu -> unit) =
        let address = hookBase + index * 4u
        writeWord vector (int address)
        cpu.Hooks[address] <- action

    let channelText (channelId: uint32) (text: string) =
        match channels.TryGetValue channelId with
        | true, channel when channel.IsFile -> ()
        | _ -> screen.Append(text) |> ignore

    // CN.ITOD: the word at (A1+A6) to decimal at (A0+A6); A1 += 2, A0 past the text, D1 = length.
    hook 0xF2u 0u (fun cpu ->
        let baseAddress = cpu.A[6]
        let value = int16 (uint16 (cpu.ReadWord(baseAddress + cpu.A[1])))
        let text = string value
        let bytes = Encoding.ASCII.GetBytes text
        writeBytes (baseAddress + cpu.A[0]) bytes
        cpu.A[0] <- cpu.A[0] + uint32 bytes.Length
        cpu.A[1] <- cpu.A[1] + 2u
        cpu.D[1] <- uint32 bytes.Length)
    // UT.ERR: report the QDOS error code in D0 on channel A0.
    hook 0xCCu 1u (fun cpu -> channelText cpu.A[0] $"[QDOS error {int32 cpu.D[0]}]")
    hook 0xD0u 2u (fun cpu -> channelText cpu.A[0] "[UT.MTEXT]")
    hook 0xC4u 3u (fun _ -> raise (M68kFault "UT.WINDW is not emulated"))
    hook 0xC8u 4u (fun _ -> raise (M68kFault "UT.SCR is not emulated"))

    let fileOf (channelId: uint32) =
        match channels.TryGetValue channelId with
        | true, channel when channel.IsFile -> Some channel
        | _ -> None

    cpu.TrapHandler <-
        fun cpu number ->
            let operation = int (cpu.D[0] &&& 0xFFu)
            let ok () = cpu.D[0] <- 0u
            let error (code: int) = cpu.D[0] <- uint32 code
            match number with
            | 1 ->
                match operation with
                | 0x00 ->
                    cpu.D[1] <- 0x00010000u
                    cpu.D[2] <- 0x312E3033u
                    cpu.A[0] <- sysvarBase
                    ok ()
                | 0x05 -> cpu.Halt()
                | 0x08
                | 0x11
                | 0x19 -> ok ()
                | 0x13 ->
                    clock.Value <- clock.Value + 1u
                    cpu.D[1] <- clock.Value
                    ok ()
                | 0x18 ->
                    cpu.A[0] <- heap.Value
                    heap.Value <- heap.Value + ((cpu.D[1] + 15u) &&& ~~~15u)
                    ok ()
                | _ -> raise (M68kFault $"TRAP #1 D0={operation:X2} is not emulated")
            | 2 ->
                match operation with
                | 0x01 ->
                    let name = readQdosString cpu.A[0]
                    let key = int cpu.D[3]
                    let isScreen =
                        name.StartsWith("scr", StringComparison.OrdinalIgnoreCase)
                        || name.StartsWith("con", StringComparison.OrdinalIgnoreCase)
                    let open' () =
                        let id = nextChannel.Value
                        nextChannel.Value <- id + 1u
                        channels[id] <- { Name = name; IsFile = not isScreen; Position = 0 }
                        cpu.A[0] <- id
                        ok ()
                    if isScreen then open' ()
                    else
                        match key with
                        | 0
                        | 1 -> if files.ContainsKey name then open' () else error -7
                        | 2 ->
                            if files.ContainsKey name then error -8
                            else
                                files[name] <- ResizeArray<byte>()
                                open' ()
                        | 3 ->
                            files[name] <- ResizeArray<byte>()
                            open' ()
                        | _ -> raise (M68kFault $"IO.OPEN key {key} is not emulated")
                | 0x02 -> ok ()
                | 0x04 ->
                    let name = readQdosString cpu.A[0]
                    if files.Remove name then ok () else error -7
                | _ -> raise (M68kFault $"TRAP #2 D0={operation:X2} is not emulated")
            | 3 ->
                let channelId = cpu.A[0]
                let write (bytes: byte[]) =
                    match fileOf channelId with
                    | Some channel ->
                        let data = files[channel.Name]
                        for b in bytes do
                            if channel.Position < data.Count then data[channel.Position] <- b else data.Add b
                            channel.Position <- channel.Position + 1
                    | None -> screen.Append(Encoding.ASCII.GetString bytes) |> ignore
                match operation with
                | 0x05 ->
                    write [| byte cpu.D[1] |]
                    ok ()
                | 0x07 ->
                    let count = int (cpu.D[2] &&& 0xFFFFu)
                    write memory[int cpu.A[1] .. int cpu.A[1] + count - 1]
                    cpu.A[1] <- cpu.A[1] + uint32 count
                    cpu.D[1] <- uint32 count
                    ok ()
                | 0x03 ->
                    match fileOf channelId with
                    | Some channel ->
                        let data = files[channel.Name]
                        let count = min (int (cpu.D[2] &&& 0xFFFFu)) (data.Count - channel.Position)
                        for index in 0 .. count - 1 do
                            memory[int cpu.A[1] + index] <- data[channel.Position + index]
                        channel.Position <- channel.Position + count
                        cpu.A[1] <- cpu.A[1] + uint32 count
                        cpu.D[1] <- uint32 count
                        if count < int (cpu.D[2] &&& 0xFFFFu) then error -10 else ok ()
                    | None -> raise (M68kFault "IO.FSTRG from a screen channel is not emulated")
                | 0x42
                | 0x43 ->
                    match fileOf channelId with
                    | Some channel ->
                        let length = files[channel.Name].Count
                        let target = if operation = 0x42 then int cpu.D[1] else channel.Position + int cpu.D[1]
                        channel.Position <- max 0 (min length target)
                        cpu.D[1] <- uint32 channel.Position
                        ok ()
                    | None -> raise (M68kFault "file positioning on a screen channel")
                | 0x46 ->
                    fileType.Value <- int memory[int cpu.A[1] + 5]
                    dataspace.Value <- int (cpu.ReadLong(cpu.A[1] + 6u))
                    ok ()
                | 0x70 -> error -15
                | 0x0D
                | 0x10
                | 0x17
                | 0x20
                | 0x24
                | 0x27
                | 0x28
                | 0x29
                | 0x2C
                | 0x2D -> ok ()
                | 0x01 -> error -1
                | _ -> raise (M68kFault $"TRAP #3 D0={operation:X2} is not emulated")
            | _ -> raise (M68kFault $"TRAP #{number} is not emulated")

    cpu.Run 400_000_000L

    let outputName = settings.OutputName.Substring(0, settings.OutputName.Length - 5)
    let output =
        match files.TryGetValue outputName with
        | true, data -> data.ToArray()
        | _ -> [||]
    { Status = int (int16 (uint16 (cpu.ReadWord parameters)))
      Output = output
      FileType = fileType.Value
      Dataspace = dataspace.Value
      Screen = screen.ToString()
      Steps = cpu.Steps }
