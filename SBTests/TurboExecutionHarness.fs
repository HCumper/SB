module SBTests.TurboExecutionHarness

// Executes an unmodified QDOS executable from its entry point. The CPU and bundled Turbo
// runtime execute normally; this deliberately small QDOS shim supplies console I/O and job
// startup only. Unsupported services fail rather than silently pretending to work.

open System
open System.Collections.Generic
open System.Globalization
open System.Text
open SBTests.M68kEmulator

type ExecutionResult = {
    Output: string
    ExitCode: int
    Steps: int64
    InstructionWordsFetched: int64
}

/// The installed Q-emuLator JS ROM (or $SB_QL_ROM). Real-ROM tests run only where it exists.
let jsRomPath =
    match Environment.GetEnvironmentVariable "SB_QL_ROM" with
    | null | "" -> @"C:\Program Files (x86)\Q-emuLator\QL ROMs\QL_ROM_JS"
    | path -> path

let installedJsRom () = if IO.File.Exists jsRomPath then Some(IO.File.ReadAllBytes jsRomPath) else None

/// A BASIC function's entry in a QL ROM, found through the ROM's own name table.
let romFunctionAddress (rom: byte[]) (name: string) =
    let pattern = Array.append [| byte name.Length |] (Encoding.ASCII.GetBytes name)
    let matches = [ for i in 2 .. rom.Length - pattern.Length do if rom[i .. i + pattern.Length - 1] = pattern then yield i ]
    match matches with
    | [ i ] -> uint32 (i - 2 + int (int16 ((uint16 rom[i - 2] <<< 8) ||| uint16 rom[i - 1])))
    | _ -> failwith $"ROM function {name} is not found exactly once."

/// The ROM maths functions a real-ROM run makes available to a compiled program.
let romMathsResidents =
    [ "ABS"; "ACOS"; "ACOT"; "ASIN"; "ATAN"; "COS"; "COT"; "DEG"; "EXP"; "INT"; "LN"; "LOG10"; "RAD"; "SIN"; "SQRT"; "TAN" ]

/// Runs an executable. With a ROM image, that ROM's arithmetic, comparison and
/// maths functions execute unchanged, and its maths functions are linkable residents.
let runWithRom (rom: byte[] option) (maxSteps: int64) (executable: byte[]) =
    let headerLength = 30
    if executable.Length <= headerLength
       || Encoding.ASCII.GetString(executable, 0, 18) <> "]!QDOS File Header"
       || executable[19] <> 15uy || executable[21] <> 1uy then
        invalidArg (nameof executable) "Expected an executable QDOS file header."
    let dataspace =
        executable[22..25] |> Array.fold (fun value next -> (value <<< 8) ||| uint32 next) 0u
    let code = executable[headerLength..]
    let jobBase = 0x40000u
    let sysvarBase = 0x28000u
    let basicJobBase = 0x30000u
    let memory = Array.zeroCreate<byte> 0x100000
    let codeLength = uint32 code.Length
    let top = uint64 jobBase + uint64 codeLength + uint64 dataspace
    if codeLength % 2u <> 0u || dataspace < 512u || top > uint64 memory.Length then
        invalidArg (nameof executable) "Executable code and dataspace do not fit in the test machine."
    rom |> Option.iter (fun image -> Array.blit image 0 memory 0 image.Length)
    let romLength = rom |> Option.map (fun image -> uint32 image.Length) |> Option.defaultValue 0u
    let cpu = Cpu(memory)
    let output = StringBuilder()
    let channels = HashSet<uint32>()
    let mutable nextChannel = 0x10000u
    let mutable exitCode = None

    let openConsole () =
        let channel = nextChannel
        nextChannel <- nextChannel + 1u
        channels.Add channel |> ignore
        channel

    let writeBytes address bytes = Array.blit bytes 0 memory (int address) bytes.Length
    let readText address count =
        if uint64 address + uint64 count > uint64 memory.Length then
            raise (M68kFault "Console text extends outside emulated memory.")
        Encoding.Latin1.GetString(memory, int address, count)
    let readName address = readText (address + 2u) (int (cpu.ReadWord address))
    let ok () = cpu.D[0] <- 0u
    let unsupported () = cpu.D[0] <- uint32 -15 // QDOS ERR.BP: unsupported channel operation.
    let hook vector index action =
        let address = 0x1000u + index * 4u
        cpu.WriteWord(vector, address)
        cpu.Hooks[address] <- action

    // UT.CON opens a window. Display geometry is irrelevant to the captured text stream.
    hook 0xC6u 0u (fun cpu -> cpu.A[0] <- openConsole (); ok ())
    // CN.ITOD uses pointers relative to A6, consuming an integer from the maths stack.
    hook 0xF2u 1u (fun cpu ->
        let value = int16 (uint16 (cpu.ReadWord(cpu.A[6] + cpu.A[1])))
        let bytes = Encoding.ASCII.GetBytes(value.ToString(CultureInfo.InvariantCulture))
        writeBytes (cpu.A[6] + cpu.A[0]) bytes
        cpu.A[0] <- cpu.A[0] + uint32 bytes.Length
        cpu.A[1] <- cpu.A[1] + 2u
        cpu.D[1] <- uint32 bytes.Length
        ok ())
    // Runtime errors must fail the test, even though Turbo ultimately removes the job.
    hook 0xCAu 2u (fun cpu -> raise (M68kFault $"Runtime QDOS error {int32 cpu.D[0]}."))
    hook 0xCCu 3u (fun cpu -> raise (M68kFault $"Runtime QDOS error {int32 cpu.D[0]}."))
    hook 0xD0u 4u (fun cpu ->
        // lib537 error_trap records ERNUM at signed A6+$8154 before fatal
        // prints its task prefix. Read it without changing emulated state.
        let errorAddress=cpu.A[6]+uint32(int32(int16 0x8154us))
        let errorNumber=int16(uint16(cpu.ReadWord errorAddress))
        raise (M68kFault $"Runtime error message: {readName cpu.A[1]}; ERNUM={errorNumber}"))
    // Without a ROM image, RI.EXEC is unsupported and UT.CSTR is approximated.
    let simulated vector index action = if rom.IsNone then hook vector index action
    simulated 0x11Cu 5u (fun _ -> raise (M68kFault "Unsupported ROM arithmetic service RI.EXEC."))
    // UT.CSTR compares the strings at A6+A0 and A6+A1. Comparison type D0.B:
    // bit 0 ignores case, bit 1 compares runs of digits by value. D0 is -1, 0 or 1.
    // An approximation of the ROM's collation, enough for differential tests.
    simulated 0xE6u 6u (fun cpu ->
        let text offset =
            let address = cpu.A[6] + offset
            readText (address + 2u) (int (cpu.ReadWord address))
        let kind = int (cpu.D[0] &&& 0xFFu)
        let fold (value: string) = if kind &&& 1 <> 0 then value.ToUpperInvariant() else value
        let left, right = fold (text cpu.A[0]), fold (text cpu.A[1])
        let digits (value: string) start =
            let mutable finish = start
            while finish < value.Length && Char.IsDigit value[finish] do finish <- finish + 1
            finish
        let rec compare i j =
            if i >= left.Length || j >= right.Length then sign ((left.Length - i) - (right.Length - j))
            elif kind &&& 2 <> 0 && Char.IsDigit left[i] && Char.IsDigit right[j] then
                let i', j' = digits left i, digits right j
                let a, b = left[i..i'-1].TrimStart('0'), right[j..j'-1].TrimStart('0')
                let order = if a.Length <> b.Length then sign (a.Length - b.Length) else sign (String.CompareOrdinal(a, b))
                if order <> 0 then order else compare i' j'
            elif left[i] <> right[j] then sign (int left[i] - int right[j])
            else compare (i + 1) (j + 1)
        cpu.D[0] <- uint32 (compare 0 0))

    cpu.TrapHandler <- fun cpu number ->
        let operation = int (cpu.D[0] &&& 0xFFu)
        match number, operation with
        | 0, _ -> cpu.StatusRegister <- cpu.StatusRegister ||| 0x2000u // Enter scheduler critical section.
        | 1, 0 -> // MT.INF: job ID, ROM version and system variables.
            cpu.D[1] <- 0x10000u
            cpu.D[2] <- 0x312E3033u
            cpu.A[0] <- sysvarBase
            ok ()
        | 1, 5 -> // MT.FRJOB: termination, including STOP.
            exitCode <- Some(int32 cpu.D[3])
            cpu.Halt()
        | 2, 1 -> // IO.OPEN: only console devices are supported.
            let name = readName cpu.A[0]
            if not (name.StartsWith("CON", StringComparison.OrdinalIgnoreCase)
                    || name.StartsWith("SCR", StringComparison.OrdinalIgnoreCase)) then
                raise (M68kFault $"Unsupported device '{name}'.")
            cpu.A[0] <- openConsole ()
            ok ()
        | 2, 2 -> // IO.CLOSE.
            if not (channels.Remove cpu.A[0]) then raise (M68kFault "Closing an unknown console.")
            ok ()
        | 3, 0x70 // IOP.PINF: this machine has no Pointer Environment.
        | 3, 0x0A // SD.PXENQ.
        | 3, 0x0B -> unsupported () // SD.CHENQ: use Turbo's stored width/cursor position.
        | 3, 5
        | 3, 7 -> // IO.SBYTE / IO.SSTRG.
            if not (channels.Contains cpu.A[0]) then raise (M68kFault "Writing to an unknown console.")
            if operation = 5 then output.Append(char (byte cpu.D[1])) |> ignore
            else
                let count = int (cpu.D[2] &&& 0xFFFFu)
                output.Append(readText cpu.A[1] count) |> ignore
                cpu.A[1] <- cpu.A[1] + uint32 count
                cpu.D[1] <- uint32 count
            ok ()
        | _ -> raise (M68kFault $"Unsupported TRAP #{number}, D0=${operation:X2}.")

    writeBytes jobBase code
    cpu.WriteLong(sysvarBase + 0x20u, uint32 memory.Length)
    cpu.WriteLong(sysvarBase + 16u, basicJobBase)
    // Supply a valid BASIC variables area. Its name table is empty without a ROM
    // image and otherwise holds the ROM's maths functions as machine-code functions.
    cpu.WriteLong(basicJobBase + 104u + 24u, 0x200u)
    cpu.WriteLong(basicJobBase + 104u + 32u, 0x300u)
    rom |> Option.iter (fun image ->
        let basic = basicJobBase + 104u
        let mutable nameOffset = 0
        for index, name in List.indexed romMathsResidents do
            let slot = basic + 0x200u + uint32 (index * 8)
            cpu.WriteWord(slot, 0x0802u)
            cpu.WriteWord(slot + 2u, uint32 nameOffset)
            cpu.WriteLong(slot + 4u, romFunctionAddress image name)
            let encoded = Encoding.ASCII.GetBytes name
            memory[int (basic + 0x300u) + nameOffset] <- byte encoded.Length
            Array.blit encoded 0 memory (int (basic + 0x300u) + nameOffset + 1) encoded.Length
            nameOffset <- nameOffset + 1 + encoded.Length)
    cpu.A[6] <- jobBase
    cpu.A[4] <- codeLength
    cpu.A[5] <- codeLength + dataspace
    cpu.A[7] <- uint32 top - 8u
    cpu.WriteWord(cpu.A[7], 0u) // No inherited channels.
    cpu.WriteWord(cpu.A[7] + 2u, 0u) // Empty option string.
    cpu.Pc <- jobBase
    try
        while not cpu.Halted do
            if cpu.Steps >= maxSteps then
                raise (M68kFault $"step limit {maxSteps} reached.")
            if (cpu.Pc < jobBase || cpu.Pc >= jobBase + codeLength) && cpu.Pc >= romLength && not (cpu.Hooks.ContainsKey cpu.Pc) then
                raise (M68kFault $"Execution left the executable for unimplemented address ${cpu.Pc:X6}.")
            cpu.Step()
        { Output = output.ToString()
          ExitCode = exitCode |> Option.defaultWith (fun () -> failwith "Job halted without MT.FRJOB.")
          Steps = cpu.Steps
          InstructionWordsFetched = cpu.InstructionWordsFetched }
    with M68kFault message ->
        raise (M68kFault $"{message} PC=${cpu.Pc:X6}, steps={cpu.Steps}, output={output}")

let run (maxSteps: int64) (executable: byte[]) = runWithRom None maxSteps executable
