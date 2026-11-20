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
}

let run (maxSteps: int64) (executable: byte[]) =
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
    hook 0xD0u 4u (fun cpu -> raise (M68kFault $"Runtime error message: {readName cpu.A[1]}"))
    hook 0x11Cu 5u (fun _ -> raise (M68kFault "Unsupported ROM arithmetic service RI.EXEC."))

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
    // The resident-name table is empty in these programs. Supply a valid BASIC variables area.
    cpu.WriteLong(basicJobBase + 104u + 24u, 0x200u)
    cpu.WriteLong(basicJobBase + 104u + 32u, 0x300u)
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
            if (cpu.Pc < jobBase || cpu.Pc >= jobBase + codeLength) && not (cpu.Hooks.ContainsKey cpu.Pc) then
                raise (M68kFault $"Execution left the executable for unimplemented address ${cpu.Pc:X6}.")
            cpu.Step()
        { Output = output.ToString()
          ExitCode = exitCode |> Option.defaultWith (fun () -> failwith "Job halted without MT.FRJOB.")
          Steps = cpu.Steps }
    with M68kFault message ->
        raise (M68kFault $"{message} PC=${cpu.Pc:X6}, steps={cpu.Steps}, output={output}")
