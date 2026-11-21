module TurboCodegen

// TurboCodegen is the second Turbo pass: it turns the intermediate stream written by
// HirTurboIrBackend into a QDOS executable, as Turbo's own code generator does. It is a port of
// codegen410.asm (Turbo Codegen 3.27 = generator 4.10 + library 5.37) and uses the genuine
// template library, lib537 (TurboRuntime/lib537.bin, extracted from codegen_task and checked
// against the Turbo IR specification's catalogue).
//
// The port follows the assembler routine by routine; comments name the routine each part comes
// from. In order, codegen:
//   1. "improves" the stream in place: a handful of peephole rewrites (x% = x% + n becomes
//      inc_int, x = x + 1 becomes increment_float, if-false over a jump becomes if-true, ...);
//   2. writes template 999 (the runtime entry and the vector table) with the task name and stack
//      size patched in;
//   3. pass 0: sizes every record, records label addresses, and writes each threaded (P mode)
//      template the first time it is used, followed by the inner interpreter;
//   4. pass 1: writes the code: native templates with their operands patched in, or for P mode
//      a template address word followed by the (patched) operand words;
//   5. writes the entry offset word that the runtime reads backwards from the end of the code.
//
// Like the original, the first error stops code generation (codegen's one_error setting).
// What codegen410 supports that this port does not: input from a file instead of memory (the
// IR is always in memory here), the <64K "small code" option and _INS extension files. These
// are refused with an error rather than mis-generated.

open System
open System.Collections.Generic
open System.IO
open System.Text

type TurboCodegenOptions = {
    /// Task name stored in the program header (at most 12 characters).
    TaskName: string
    /// Compiled program dataspace in KiB, 0..65535 (unsigned intercom P+2).
    DataspaceKiB: int
    /// CPU stack size in bytes, 350..9998 (intercom P+114). None keeps the library default.
    StackSize: int option
    /// Turbo's reference-string option (bit 0 of the intercom stack word).
    ReferenceStrings: bool
    /// The <64K code option (intercom P+5 bit 0); not supported.
    SmallCode: bool
    /// Allocate leaf native integer values in D0/D1 instead of expanding every
    /// maths-stack template. False preserves the original generator byte for byte.
    LocalRegisterAllocation: bool
    /// Lower eligible integer FOR bodies and NEXT into a native loop. Requires
    /// local allocation; false retains compatibility with the original generator.
    NativeIntegerForLoops: bool
}

let defaultCodegenOptions = {
    TaskName = "SB"
    DataspaceKiB = 20
    StackSize = None
    ReferenceStrings = false
    SmallCode = false
    LocalRegisterAllocation = false
    NativeIntegerForLoops = false
}

type TurboCodegenOutput = {
    /// The executable's bytes, without a file header.
    Code: byte[]
    /// Dataspace for the QDOS file header, in bytes.
    Dataspace: int
    NativeAllocation: TurboNativeIr.Report option
    NativeLoops: TurboNativeLoops.Report option
}

/// The 30-byte "]!QDOS File Header" block that QDOS emulators on PC file systems use to keep a
/// file's QDOS header: signature, a word 15, access 0, type 1 (executable), dataspace, extra.
let qdosFileHeader (dataspace: int) =
    let header = Array.zeroCreate<byte> 30
    Encoding.ASCII.GetBytes("]!QDOS File Header").CopyTo(header, 0)
    header[19] <- 0x0Fuy
    header[21] <- 1uy
    header[22] <- byte (dataspace >>> 24)
    header[23] <- byte (dataspace >>> 16)
    header[24] <- byte (dataspace >>> 8)
    header[25] <- byte dataspace
    header

/// The executable file as written to disk: the QDOS header block, then the code.
let executableFile (output: TurboCodegenOutput) = Array.append (qdosFileHeader output.Dataspace) output.Code

let private libraryResource = "lib537.bin"

/// The template library: lib537's library_area (header, index and templates).
let library =
    lazy (
        use stream = typeof<TurboCodegenOutput>.Assembly.GetManifestResourceStream(libraryResource)
        if isNull stream then failwith $"Embedded Turbo template library '{libraryResource}' not found."
        use memory = new MemoryStream()
        stream.CopyTo memory
        memory.ToArray())

exception private CodegenFailure of string

// Constants from codegen410.asm.
let private lowName = 2000
let private indexBase = 10 // lib_index follows vect_tab_ad, nat_to_p, chan_tab_ad and lib_size
let private recordSize = 8
let private maxStackOffset = 0x1D60 // cod327: MAX_STACK EQU library_area+$1D60
let private minStack = 350
let private maxStack = 9998
let private initLabInc = 0x8008 // base register A6 is the code start + 32760
let private maxTwoByteDestination = 0xFFFE - 16000
let private ifPToNOffset = 14
let private pToNOffset = 18
let private pToPOffset = 8
let private forOffset = 22
let private endSpec = 0x80
let private maxParType = 38
// Codegen's own dataspace ($19000) less 500 bytes of stack, in 6-byte symbol records.
let private symbolCapacity = (0x19000 - 500) / 6

let private opJump = 242
let private opIfTrue = 245
let private opIfFalse = 239
let private opLine = 248
let private opFast = 254
let private opSlow = 255
let private opRomName = 261
let private opRomProc = 227
let private opRomFn = 224
let private opExtString = 385
let private opLabel = 378
let private opQincInt = 337
let private opQdecInt = 338
let private opGoto = 235

/// The inner interpreter appended to every shared P template: move.w (a5)+,d0; jmp 0(a6,d0.w).
let private interpreter = [| 0x30uy; 0x1Duy; 0x4Euy; 0xF6uy; 0x00uy; 0x00uy |]

/// The word codegen's memory holds after n_to_p_temp and p_to_p_short (tst.w d6). Codegen writes
/// 10 bytes of the 8-byte n_to_p template and 6 of the 4-byte short p-to-p template, so this word
/// lands in the program as filler.
let private fillerWord = 0x4A46

type private Generator(sourceLibrary: byte[], options: TurboCodegenOptions, sourceIr: byte[]) =
    // Codegen patches the library and the stream in place, so it works on copies.
    let lib = Array.copy sourceLibrary
    let ir = Array.copy sourceIr
    let output = ResizeArray<byte>()
    let symbols = Dictionary<int, int>()
    let records = Dictionary<int, int>()
    let nativeBlocks = Dictionary<int,TurboNativeIr.CompiledBlock>()
    let nativeLoops = Dictionary<int,TurboNativeLoops.CompiledLoop>()
    let mutable nativeLoopReport:TurboNativeLoops.Report option = None
    let mutable nativeArrayAddresses:Map<int,int> = Map.empty
    let mutable nativeArrayOperations:Set<int> = Set.empty
    let mutable nativeAllocation:TurboNativeIr.Report option = None

    let mutable ic = 0 // int_code (A2), a byte index into ir
    let mutable pass = 0 // low word of D6
    let mutable pMode = false // high word of D6: threaded (P) code when set
    let mutable objCode = 0 // D5
    let mutable stSize = 0 // D7
    let mutable labInc = initLabInc // lab_inc (a word)
    let mutable entryAd = 0
    let mutable runtimeEntrySize = 0
    let mutable maxTwoByte = maxTwoByteDestination
    let mutable codeSize = 0
    let mutable line = 0
    let mutable savedD1 = 0 // process_pars keeps D1/D2 on the stack; patch routines change them
    let mutable savedD2 = 0

    let extraStackBytes () =
        [ nativeAllocation |> Option.map _.StackExtraBytes |> Option.defaultValue 0
          nativeLoopReport |> Option.map _.StackExtraBytes |> Option.defaultValue 0
          if nativeArrayOperations.IsEmpty then 0 else 8 ] |> List.max

    let word (bytes: byte[]) (index: int) = (int bytes[index] <<< 8) ||| int bytes[index + 1]
    let signedWord (bytes: byte[]) (index: int) = int (int16 (uint16 (word bytes index)))
    let readLong (bytes: byte[]) (index: int) = (word bytes index <<< 16) ||| word bytes (index + 2)

    let setWord (bytes: byte[]) (index: int) (value: int) =
        bytes[index] <- byte (value >>> 8)
        bytes[index + 1] <- byte value

    let setLong (bytes: byte[]) (index: int) (value: int) =
        setWord bytes index (value >>> 16)
        setWord bytes (index + 2) value

    let wordBytes (value: int) = [| byte (value >>> 8); byte value |]
    let longBytes (value: int) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

    let fail message =
        raise (CodegenFailure(if line > 0 then $"line {line}: {message}" else message))

    let checkPrefixSize size =
        // Startup indexes the vector table with the signed entry-offset word. Shared P
        // templates also need signed A6-relative addresses. Check before word arithmetic wraps.
        let baseOffset = int (int16 (uint16 initLabInc))
        if size - runtimeEntrySize > int Int16.MaxValue || size + baseOffset > int Int16.MaxValue then
            fail "The runtime prefix exceeds the signed 16-bit address range."

    let natToP = word lib 2
    let vectTabAd = word lib 0
    let chanTabAd = word lib 4

    // write_obj and inc_write_obj.
    let write (bytes: byte[]) = output.AddRange bytes
    let writeCounted (bytes: byte[]) =
        objCode <- objCode + bytes.Length
        write bytes
    let slice (bytes: byte[]) (start: int) (count: int) = Array.sub bytes start count

    do
        let indexSize = readLong lib 6
        for index in 0 .. indexSize / recordSize - 1 do
            let record = indexBase + index * recordSize
            records[word lib record] <- record

    // -- the library index --------------------------------------------------

    /// srch_user_lib: the index record of a template, if the library has it.
    member private _.Search(number: int) =
        match records.TryGetValue(number &&& 0xFFFF) with
        | true, record -> Some record
        | _ -> None

    member private _.Length(record: int) = word lib (record + 2) &&& ~~~1
    member private _.IsParameterised(record: int) = word lib (record + 2) &&& 1 <> 0
    member private _.CodeAddress(record: int) = indexBase + signedWord lib (record + 4)
    member private _.CompilationAddress(record: int) = word lib (record + 6)
    member private _.SetCompilationAddress(record: int, value: int) = setWord lib (record + 6) value

    /// get_bytes / get_par1: template length, the parameter header byte and the address of the
    /// first parameter specification.
    member private this.GetBytes(record: int) =
        let length = this.Length record
        let metadata = indexBase + ((signedWord lib (record + 4) + length) &&& 0xFFFF)
        length, int lib[metadata], metadata + 1

    /// strg_get_bytes: bytes of a string operand (D1) and D0, which is 2 for ROM_FN.
    member private _.StringBytes(opcode: int, d0: int) =
        let d1, d0 =
            if opcode = opRomProc || opcode = opRomName then int ir[ic + 2] + 4, d0
            elif opcode = opRomFn then int ir[ic + 4] + 6, 2
            elif opcode = opExtString then readLong ir ic + 5, d0
            else ((word ir ic + 3) &&& 0xFFFF), d0
        d1 &&& ~~~1, d0

    /// get_alloc_bytes: in pass 0, the bytes allocation templates add, and in P mode the extra
    /// word of each 32-bit label.
    member private _.AllocationBytes(spec: int, d2: int) =
        if pass <> 0 then
            d2
        else
            let mutable d2 = d2
            let mutable p = spec
            while int lib[p] <> endSpec do
                let parType = int lib[p]
                let parameter = word ir (ic + int lib[p + 3] - 2)
                if parType >= 29 * 2 then
                    if pMode && parType <= 35 * 2 then d2 <- d2 + 2
                elif parType = 12 * 2 then d2 <- d2 + parameter * 4 + 4
                elif parType = 17 * 2 then d2 <- d2 + parameter - 2
                elif parType = 20 * 2 then d2 <- d2 + parameter + 2
                p <- p + 4
            d2

    /// get_par3: non-string headers. Below 32 the header is the operand byte count; 32-64 adds
    /// allocation, above 64 (the window record) the P form is compacted.
    member private this.GetPar3(d1: int, spec: int, d2: int) =
        if d1 < 32 then d1, d2
        elif d1 > 64 then
            let d1 = d1 - 64
            if not pMode then d1, d2
            elif d1 = 4 then d1, d2 - 2
            else d1, d2 - 4
        else
            let d1 = d1 - 32
            d1, this.AllocationBytes(spec, d2)

    /// get_par2: header byte to operand bytes (D1), adjusting the length (D2).
    member private this.GetPar2(opcode: int, header: int, spec: int, d2: int) =
        if header &&& 0x80 <> 0 then
            let d1, _ = this.StringBytes(opcode, 0)
            d1, d2
        else
            this.GetPar3(header, spec, d2)

    /// calc_par_bytes (P mode): operand bytes to skip (D1) and object bytes (D2).
    member private this.CalcParBytes(record: int, opcode: int) =
        let _, header, spec = this.GetBytes record
        let d1, d2 = this.GetPar2(opcode, header, spec, 0)
        d1, d2 + 2 + d1

    /// nat0_get_bytes (native, pass 0).
    member private this.Nat0GetBytes(record: int, opcode: int) =
        let length, header, spec = this.GetBytes record
        if header &&& 0x80 = 0 then
            this.GetPar3(header, spec, length)
        else
            let d1, d0 = this.StringBytes(opcode, 0)
            d1, length + d1 - 2 - d0

    // -- symbols ------------------------------------------------------------

    member private _.Define(label: int, allowRedefinition: bool) =
        if not allowRedefinition && symbols.ContainsKey label then
            fail $"Duplicated label: {label}"
        if stSize + 6 > symbolCapacity * 6 then fail "Codegen dataspace too small."
        symbols[label] <- objCode ||| (if pMode then 1 else 0)
        stSize <- stSize + 6

    /// sr_lab / sr_lab32: a label's address (bit 0 set for a P mode destination).
    member private _.LabelAddress(label: int) =
        match symbols.TryGetValue label with
        | true, address -> address
        | _ -> fail $"Label {label} not defined."

    // -- parameter patching (process_pars and the patch routines sr1..sr38) ---

    member private this.WriteTemplate(record: int, address: int, counted: bool) =
        let bytes = slice lib address (this.Length record)
        if counted then writeCounted bytes else write bytes

    /// save_zeroes: allocated bytes, all zero.
    member private _.SaveZeroes(count: int) =
        let first = count &&& 0xFF
        if first <> 0 then writeCounted (Array.zeroCreate first)
        for _ in 1 .. ((count >>> 8) &&& 0xFFFF) do
            writeCounted (Array.zeroCreate 256)

    member private this.ProcessPars(record: int, opcode: int) =
        let length, header, spec = this.GetBytes record
        let d1, d2 = this.GetPar2(opcode, header, spec, length)
        savedD1 <- d1
        savedD2 <- d2
        let template = this.CodeAddress record
        let mutable p = spec
        while int lib[p] <> endSpec do
            let parType = int lib[p]
            let offset = word lib (p + 1)
            let parameterIndex = int lib[p + 3]
            p <- p + 4
            if parType > maxParType * 2 then
                fail $"Patch type {parType / 2} in template {opcode}."
            let d0 = word ir (ic + parameterIndex - 2)
            this.Patch(record, opcode, parType / 2, d0, template, offset, parameterIndex)
        savedD1, savedD2

    /// sr4a: in P mode a patched value replaces the operand word in the stream.
    member private _.PatchStream(parameterIndex: int, value: int) = setWord ir (ic + parameterIndex - 2) value

    member private this.Patch(record: int, opcode: int, parType: int, d0: int, template: int, offset: int, parameterIndex: int) =
        // sr3: a word into the template, or into the stream in P mode.
        let insertWord (value: int) =
            if pMode then this.PatchStream(parameterIndex, value &&& 0xFFFF)
            else setWord lib (template + offset) value
        // sr2: a byte.
        let insertByte (value: int) =
            let value = value &&& 0xFF
            if pMode then this.PatchStream(parameterIndex, value) else lib[template + offset] <- byte value
        // sr29a: a long label address (native) or sr29b (P mode).
        let insertLong (value: int) =
            if pMode then this.SavePLabel value else setLong lib (template + offset) value
        let typeError () = fail $"Patch type {parType} in template {opcode}."
        match parType with
        | 1 -> insertWord (d0 <<< 2)
        | 2 -> insertByte d0
        | 3 -> insertWord d0
        | 4 -> insertWord (this.LabelAddress d0 &&& ~~~1)
        | 5 -> insertWord ((d0 <<< 2) + vectTabAd)
        | 6 -> insertWord ((d0 <<< 1) + 8)
        | 7 -> insertByte (d0 - 1)
        | 8 -> insertByte ((d0 + 4) <<< 1)
        | 9 -> insertWord (-d0)
        | 10 ->
            if not pMode then
                ic <- ic + parameterIndex - 4
                let length = word ir ic
                ic <- ic + 2
                this.SaveString(record, template, length + 1)
        | 11 -> insertWord ((d0 <<< 2) + 4)
        | 12 -> this.AllocateBytes(record, template, (d0 <<< 2) + 4)
        | 13 ->
            if not pMode then
                ic <- ic + 2
                this.SaveString(record, template, (d0 >>> 8) + 2)
        | 14 -> insertWord (d0 * 40 + 4)
        | 15 -> insertWord (d0 <<< 1)
        | 16 -> insertByte (d0 - 2)
        | 17 -> this.AllocateBytes(record, template, d0 - 2)
        | 18 -> insertWord (d0 * 40 + chanTabAd)
        | 19 -> insertWord ((d0 <<< 1) + 2)
        | 20 -> this.AllocateBytes(record, template, d0 + 2)
        | 22 ->
            // sr22: init_window, P mode only. The channel becomes its channel-table offset and the
            // four byte-valued words are packed into bytes.
            if not pMode then typeError ()
            else
                let channel = signedWord ir ic
                setWord ir ic (channel * 40 + 4)
                let p = ic + 2
                ir[p] <- ir[p + 1]
                ir[p + 1] <- ir[p + 3]
                ir[p + 2] <- ir[p + 5]
                ir[p + 3] <- ir[p + 7]
                Array.blit ir (p + 8) ir (p + 4) 4
                Array.blit ir (p + 12) ir (p + 8) 4
                savedD1 <- savedD1 - 4
                savedD2 <- 2
        | 25 -> insertWord (this.LabelAddress d0)
        | 26 -> insertByte ((d0 + 7) <<< 1)
        | 27 -> insertByte ((d0 + 6) <<< 1)
        | 29 -> insertLong (this.LabelAddress d0 &&& ~~~1)
        | 33 ->
            // sr33: label + for_offset; both ends must be in the same mode.
            let address = this.LabelAddress d0 + 6
            let destinationIsP = address &&& 1 <> 0
            let address = address &&& ~~~1
            if destinationIsP then
                if pMode then this.SavePLabel address else fail "Mixed modes not allowed."
            elif pMode then fail "Mixed modes not allowed."
            else setLong lib (template + offset) (address + forOffset - 6)
        | 34 ->
            // sr34: native conditional jump: the template, then a 10-byte jump appendage.
            if pMode then typeError ()
            else
                let address = this.LabelAddress d0
                this.WriteTemplate(record, template, true)
                savedD2 <- 0
                let target = address &&& ~~~1
                if address &&& 1 <> 0 then
                    writeCounted (Array.concat [ wordBytes 0x4EAE; wordBytes natToP; longBytes target; wordBytes fillerWord ])
                else
                    writeCounted (Array.concat [ wordBytes 0x203C; longBytes target; wordBytes 0x4EF6; wordBytes 0x0800 ])
        | 35 -> insertLong (this.LabelAddress d0)
        | 36 ->
            if not pMode then
                ic <- ic + 4
                this.SaveString(record, template, (d0 >>> 8) + 2)
        | 37 -> insertByte ((d0 &&& 7) * 2 + 0x50)
        | 38 -> insertByte ((d0 &&& 7) * 2 + 0x51)
        | 21
        | 24 -> fail "The <64K code option is not supported."
        | _ -> typeError ()

    /// sr10a: a native string template: the template, then the string bytes from the stream.
    member private this.SaveString(record: int, template: int, length: int) =
        let length = length &&& ~~~1
        savedD1 <- length
        savedD2 <- 0
        this.WriteTemplate(record, template, true)
        writeCounted (slice ir ic length)

    /// sr29b: a 32-bit label address in P mode goes after the operands before it.
    member private _.SavePLabel(address: int) =
        let before = savedD1 - 2
        if before > 0 then write (slice ir ic before)
        savedD2 <- 0
        write (longBytes address)
        objCode <- objCode + 2

    /// alloc_bytes, save_temp and p_alloc.
    member private this.AllocateBytes(record: int, template: int, count: int) =
        savedD2 <- 0
        if pMode then
            if savedD1 > 0 then write (slice ir ic savedD1)
        else
            this.WriteTemplate(record, template, true)
        this.SaveZeroes count

    // -- shared P templates -------------------------------------------------

    /// copy_p_template: a P template's first use copies it, then the inner interpreter.
    member private this.CopyPTemplate(record: int, length: int) =
        checkPrefixSize (output.Count + length + interpreter.Length)
        this.SetCompilationAddress(record, labInc)
        labInc <- (labInc + length + interpreter.Length) &&& 0xFFFF
        write (slice lib (this.CodeAddress record) length)
        write interpreter

    // -- the improver ----------------------------------------------------------

    /// g_token: the next template number, skipping labels and 378, with its index record.
    member private this.NextToken() : (int * int) option =
        let token = word ir ic
        ic <- ic + 2
        if token = 0 then None else this.Dispatchable token

    /// g_t3/g_t4/g_t2: a word read as a token.
    member private this.Dispatchable(token: int) : (int * int) option =
        if token >= lowName then this.NextToken()
        else this.Lookup(if TurboNativeArrays.isCompact token then TurboNativeArrays.baseOpcode token elif token >= 1000 then token - 1000 else token)

    /// g_t2 onwards: a token below 1000.
    member private this.Lookup(token: int) : (int * int) option =
        if token = opLabel then this.NextToken()
        else
            match this.Search token with
            | Some record -> Some(token, record)
            | None ->
                match this.Search(token + 1000) with
                | Some record -> Some(token, record)
                | None -> fail $"Template {token} not found."

    /// Skips a token's operands (the "Now skip parameters" code after klim).
    member private this.SkipOperands(token: int, record: int) =
        if this.IsParameterised record then
            let _, header, _ = this.GetBytes record
            if header &&& 0x80 = 0 then
                ic <- ic + (header &&& 0x1F)
            else
                let length =
                    if token = opRomProc || token = opRomName then
                        ic <- ic + 4
                        int ir[ic - 2]
                    elif token = opRomFn then
                        ic <- ic + 6
                        int ir[ic - 2]
                    elif token = opExtString then
                        let length = readLong ir ic
                        ic <- ic + 4
                        length + 1
                    else
                        let length = word ir ic
                        ic <- ic + 2
                        length + 1
                ic <- ic + (length &&& ~~~1)

    member private this.Improve() =
        ic <- 0
        let mutable next = this.NextToken()
        while next.IsSome do
            let token, record = next.Value
            next <- this.ImproveAt(token, record)

    /// klim: one improver step for a token whose opcode word has just been read. Returns the next
    /// token to consider, as the assembler's jumps back to g_n1 or g_next do.
    member private this.ImproveAt(token: int, record: int) : (int * int) option =
        // prt_3: re-dispatch a word already read as a token.
        let redispatch (value: int) = this.Dispatchable value
        let word () =
            let value = word ir ic
            ic <- ic + 2
            value
        match token with
        | 109 ->
            // fch_int: x% = x% +/- L -> (q)inc/dec, and fch_int K, fch_int K -> duplicate_int.
            let mutable k = word ()
            let mutable result = None
            let mutable searching = true
            while searching do
                let next = word ()
                if next = 0 then
                    searching <- false
                elif next = 106 then
                    searching <- false
                    let l = word ()
                    let op = word ()
                    if op = 0 then result <- None
                    elif op <> 5 && op <> 6 && op <> 1005 && op <> 1006 then result <- redispatch op
                    else
                        let store = word ()
                        if store = 0 then result <- None
                        elif store <> 118 then result <- redispatch store
                        elif word () <> k then result <- this.NextToken()
                        else
                            let baseOpcode = if op = 5 || op = 1005 then 339 else 340
                            // Preserve the checked increment's observable store-before-
                            // overflow behavior, even inside a native region. Explicit
                            // checked tokens must never become unchecked quick increments.
                            let opcode =
                                if op>=1000 then baseOpcode+1000
                                elif l >= 1 && l <= 8 then baseOpcode - 2 else baseOpcode
                            ic <- ic - 14
                            for value in [ opcode; k; l; 2; 2; 2; 2 ] do
                                setWord ir ic value
                                ic <- ic + 2
                            result <- this.NextToken()
                elif next = 109 then
                    let k2 = word ()
                    if k2 = k then
                        searching <- false
                        ic <- ic - 4
                        setWord ir ic 268
                        setWord ir (ic + 2) 2
                        ic <- ic + 4
                        result <- this.NextToken()
                    else
                        k <- k2
                else
                    searching <- false
                    result <- redispatch next
            result
        | 141 ->
            // fch_flt: x = x + 1/2/4 -> increment_float, and fch_float K twice -> duplicate_float.
            let mutable k = word ()
            let mutable result = None
            let mutable searching = true
            while searching do
                let next = word ()
                if next = 0 then
                    searching <- false
                elif next = 377 then
                    searching <- false
                    let exponent = word ()
                    let mantissa = word ()
                    let increment =
                        if mantissa <> 0x4000 then None
                        else
                            match exponent - 0x801 with
                            | 0 -> Some 363
                            | 1 -> Some 335
                            | 2 -> Some 336
                            | _ -> None
                    match increment with
                    | None -> result <- this.NextToken()
                    | Some opcode ->
                        let add = word ()
                        if add = 0 then result <- None
                        elif add <> 37 then result <- redispatch add
                        else
                            ic <- ic - 8
                            for value in [ opcode; 2; 2; 2 ] do
                                setWord ir ic value
                                ic <- ic + 2
                            result <- this.NextToken()
                elif next = 141 then
                    let k2 = word ()
                    if k2 = k then
                        searching <- false
                        setWord ir (ic - 4) 283
                        setWord ir (ic - 2) 2
                        result <- this.NextToken()
                    else
                        k <- k2
                else
                    searching <- false
                    result <- redispatch next
            result
        | 239
        | 245 ->
            // ifsa/ifsb: if K over "jump L; K:" becomes the opposite condition to L.
            let opposite = if token = 239 then 245 else 239
            let oldPos = ic
            let k = word ()
            let nextPosition = ic
            match this.NextToken() with
            | None -> None
            // NextToken skips labels (including 378). A jump with its own entry label
            // must survive, since other branches can reach it without testing this condition.
            | Some(nextToken, nextRecord) when nextToken <> opJump || ic <> nextPosition + 2 ->
                this.ImproveAt(nextToken, nextRecord)
            | Some _ ->
                let oldPos2 = ic
                let l = word ()
                let mutable result = None
                let mutable scanning = true
                while scanning do
                    let value = word ()
                    if value = k then
                        scanning <- false
                        setWord ir (oldPos - 2) opposite
                        setWord ir oldPos l
                        setWord ir (oldPos2 - 2) 2
                        setWord ir oldPos2 2
                        result <- this.NextToken()
                    elif value < lowName then
                        scanning <- false
                        result <- this.Dispatchable value
                result
        | 106 ->
            // coerc: an integer constant converted under a float operand becomes a float constant.
            let k = word ()
            let next = word ()
            if next = 0 then None
            elif next <> 138 && next <> 377 && next <> 141 then redispatch next
            else
                let floatBytes = if next = 138 then 8 else if next = 377 then 6 else 4
                let floatRecord = slice ir ic (floatBytes - 2)
                ic <- ic + floatBytes - 2
                let expect (wanted: int) =
                    let value = word ()
                    if value = 0 then Some None
                    elif value <> wanted then Some(redispatch value)
                    else None
                match expect 155 with
                | Some result -> result
                | None ->
                    match expect 320 with
                    | Some result -> result
                    | None ->
                        match expect 156 with
                        | Some result -> result
                        | None ->
                            ic <- ic - (10 + floatBytes)
                            let exponent, mantissa =
                                if k = 0 then 0, 0
                                else
                                    // 106's operand is signed. Normalize the signed high
                                    // mantissa word; the low word is zero for every Int16.
                                    let mutable mantissa = int (int16 (uint16 k))
                                    let mutable exponent = 0x80F
                                    while mantissa >= -0x4000 && mantissa < 0x4000 do
                                        mantissa <- mantissa <<< 1
                                        exponent <- exponent - 1
                                    exponent, mantissa &&& 0xFFFF
                            let values = [ 377; exponent; mantissa; next ]
                            for value in values do
                                setWord ir ic value
                                ic <- ic + 2
                            Array.blit floatRecord 0 ir ic floatRecord.Length
                            ic <- ic + floatRecord.Length
                            setWord ir ic 2
                            setWord ir (ic + 2) 2
                            ic <- ic + 4
                            this.NextToken()
        | _ ->
            this.SkipOperands(token, record)
            this.NextToken()

    // -- generation ------------------------------------------------------------

    member private this.Entry() =
        // get_first / is_999: the mandatory entry record, cleared against a restart.
        let first = word ir 0
        setWord ir 0 0
        ic <- 2
        if first <> 999 then fail "no parsed code for analysis."
        let record =
            match this.Search 999 with
            | Some record -> record
            | None -> fail "Template 999 not found."
        // The task name in template 999: a length word and 12 bytes padded with spaces.
        let name = Encoding.ASCII.GetBytes options.TaskName
        if name.Length > 12 then fail "The task name is longer than 12 characters."
        let nameAddress = this.CodeAddress record + 8
        setWord lib nameAddress name.Length
        for index in 0..11 do
            lib[nameAddress + 2 + index] <- if index < name.Length then name[index] else 0x20uy
        // ram1: process 999's parameters as if in pass 1, so the template and vector table are saved.
        pass <- 1
        let d1, _ = this.ProcessPars(record, 999)
        let size999 = this.Length record
        runtimeEntrySize <- size999
        checkPrefixSize output.Count
        let end999 = (size999 + labInc) &&& 0xFFFF
        labInc <- (labInc + objCode) &&& 0xFFFF
        maxTwoByte <- maxTwoByte - objCode
        entryAd <- end999
        objCode <- 0
        ic <- ic + d1
        pass <- 0

    member private this.Stack() =
        // no_stk: the stack size and reference-string flag go into template 999 via MAX_STACK.
        let requested = defaultArg options.StackSize 0
        lib[maxStackOffset + 2] <- if options.ReferenceStrings then 0xFFuy else 0uy
        let size = requested &&& ~~~1
        if size >= minStack && size <= maxStack then setWord lib maxStackOffset size
        setWord lib maxStackOffset ((word lib maxStackOffset + 1) &&& 0xFFFE)
        let extra=extraStackBytes()
        setWord lib maxStackOffset (word lib maxStackOffset + extra)

    member private this.PlanNativeLoops() =
        if options.LocalRegisterAllocation && options.NativeIntegerForLoops then
            let requested=defaultArg options.StackSize 0 &&& ~~~1
            let stack=if requested>=minStack && requested<=maxStack then requested else (word lib maxStackOffset + 1) &&& ~~~1
            match TurboNativeLoops.plan (maxStack-stack) vectTabAd ir with
            | Error message -> fail $"Native FOR lowering: {message}"
            | Ok report ->
                nativeLoopReport<-Some report
                for loop in report.Loops do nativeLoops.Add(loop.StartOffset,loop)

    member private this.PlanNativeRegisters() =
        if options.LocalRegisterAllocation then
            let requested=defaultArg options.StackSize 0 &&& ~~~1
            let stack=if requested>=minStack && requested<=maxStack then requested else (word lib maxStackOffset + 1) &&& ~~~1
            // Only one leaf spill frame can be live: every opaque template,
            // call and control-flow boundary releases it. Reserve the maximum
            // extra stack AND dataspace so existing heap/stack capacity remains.
            let limit=max 0 (min 128 (maxStack-stack))
            match TurboNativeIr.plan limit vectTabAd ir with
            | Error message -> fail $"Native register lowering: {message}"
            | Ok report ->
                let blocks=report.Blocks |> List.filter(fun block ->
                    nativeLoops.Values |> Seq.exists(fun loop -> block.Ir.StartOffset>=loop.StartOffset && block.Ir.StartOffset<loop.EndOffset) |> not)
                let report={report with Blocks=blocks;StackExtraBytes=blocks |> List.map(fun block -> block.Allocation.SpillSlots*2) |> List.fold max 0}
                nativeAllocation<-Some report
                for block in report.Blocks do nativeBlocks.Add(block.Ir.StartOffset,block)

    member private this.PlanNativeArrays() =
        // Streams outside SB's decoding catalogue and tight stack limits keep
        // the existing checked inline array implementation. Shared helpers use
        // two return addresses and cannot overlap a register-allocation frame.
        let requested=defaultArg options.StackSize 0 &&& ~~~1
        let stack=if requested>=minStack && requested<=maxStack then requested else (word lib maxStackOffset + 1) &&& ~~~1
        if maxStack-stack>=8 then
            let excluded=[yield! nativeBlocks.Values |> Seq.map(fun block -> block.Ir.StartOffset,block.Ir.EndOffset)
                          yield! nativeLoops.Values |> Seq.map(fun loop -> loop.StartOffset,loop.EndOffset)]
            match TurboNativeArrays.requiredExcluding excluded ir with
            | Error _ -> ()
            | Ok operations when not operations.IsEmpty ->
                let poolSize=(TurboNativeArrays.build 0 operations).Code.Length
                let vectors=word ir 2*4+4
                let entry=this.Search 999 |> Option.defaultWith(fun () -> fail "Template 999 not found.")
                let prefix=this.Length entry+vectors+poolSize
                let baseOffset=int(int16(uint16 initLabInc))
                // A helper must not make an otherwise addressable vector table
                // exceed the entry-offset/prefix limits. Inline checked N code
                // is the fallback and needs neither helper space nor its frame.
                if vectors+poolSize<=int Int16.MaxValue && prefix+baseOffset<=int Int16.MaxValue then
                    nativeArrayOperations<-operations
            | Ok _ -> ()

    member private _.WriteNativeArrays() =
        let pool=TurboNativeArrays.build labInc nativeArrayOperations
        checkPrefixSize(output.Count+pool.Code.Length)
        nativeArrayAddresses<-pool.Addresses
        write pool.Code
        labInc<-(labInc+pool.Code.Length) &&& 0xFFFF

    member private this.PPass0Par(record: int, opcode: int, length: int) =
        if this.CompilationAddress record = 0xFFFF then this.CopyPTemplate(record, length)
        let d1, d2 = this.CalcParBytes(record, opcode)
        objCode <- objCode + d2
        ic <- ic + d1

    member private this.Pass1Par(record: int, opcode: int) =
        let mutable address = this.CompilationAddress record
        if opcode = opIfTrue || opcode = opIfFalse then
            let label = word ir ic
            let destinationIsP =
                match symbols.TryGetValue label with
                | true, location -> location &&& 1 <> 0
                | _ -> false
            if not destinationIsP then address <- (address + ifPToNOffset) &&& 0xFFFF
        write (wordBytes address)
        let d1, d2 = this.ProcessPars(record, opcode)
        if d2 <> 0 then write (slice ir ic d1)
        let d1, d2 = this.CalcParBytes(record, opcode)
        objCode <- objCode + d2
        ic <- ic + d1

    member private this.NPar1(record: int, opcode: int) =
        let d1, d2 = this.ProcessPars(record, opcode)
        if d2 &&& 0xFFFF <> 0 then write (slice lib (this.CodeAddress record) d2)
        objCode <- objCode + d2
        ic <- ic + d1

    member private this.Jump(record: int, length: int) =
        // jump_ins (32-bit addresses).
        let label = word ir ic
        let jumpDone (bytes: int) =
            objCode <- objCode + bytes
            ic <- ic + 2
        match symbols.TryGetValue label with
        | false, _ ->
            if pass <> 0 then fail $"Label {label} not defined."
            elif pMode then this.PPass0Par(record, opJump, length)
            else jumpDone 10
        | true, location ->
            if not pMode then
                if location &&& 1 <> 0 then
                    if pass <> 0 then
                        write (Array.concat [ wordBytes 0x4EAE; wordBytes natToP; longBytes (location &&& ~~~1); wordBytes fillerWord ])
                    jumpDone 10
                elif pass = 0 then jumpDone length
                else this.NPar1(record, opJump)
            elif location &&& 1 <> 0 then
                let destination = location - 1
                if destination >= maxTwoByte then
                    if pass = 0 then this.PPass0Par(record, opJump, length) else this.Pass1Par(record, opJump)
                elif pass = 0 then jumpDone 4
                else
                    let bytes = if destination > objCode then 6 else 4
                    let shortCall = Array.concat [ wordBytes (natToP + pToPOffset); wordBytes destination; wordBytes fillerWord ]
                    write (slice shortCall 0 bytes)
                    jumpDone bytes
            else
                if pass <> 0 then write (Array.append (wordBytes (natToP + pToNOffset)) (longBytes location))
                jumpDone 6

    member private this.Instruction(token: int) =
        // int_ins
        if token = 2 || token = 34 then ()
        elif not pMode && nativeArrayAddresses.ContainsKey(TurboNativeArrays.canonicalOpcode token) then
            let address=nativeArrayAddresses[TurboNativeArrays.canonicalOpcode token]
            let bytes=TurboNativeArrays.call vectTabAd (word ir ic) address
            if pass<>0 then write bytes
            objCode<-objCode+bytes.Length
            ic<-ic+2
        elif not pMode && (token=1339 || token=1340) then
            // The library's P increments read operands through A5, so cannot
            // simply be copied into native code. Commit the word before the
            // overflow check, exactly as the original P improver does.
            let vector,amount=word ir ic,word ir (ic+2)
            let operation=if token=1339 then 0x0652 else 0x0452
            let bytes=Array.concat [wordBytes 0x246E;wordBytes(vectTabAd+4*vector);wordBytes operation;wordBytes amount;wordBytes 0x6804;wordBytes 0x4EEE;wordBytes 0x8AD8]
            if pass<>0 then write bytes
            objCode<-objCode+bytes.Length
            ic<-ic+4
        else
            let mutable opcode = if TurboNativeArrays.isCompact token then TurboNativeArrays.baseOpcode token elif token >= 1000 then token - 1000 else token
            if pMode && TurboNativeArrays.isUnchecked token then fail "Range-proven arrays require native mode."
            if opcode = opLabel then
                let label = word ir ic
                ic <- ic + 2
                if label < lowName then fail $"FOR loop label too low: {label}"
                if pass = 0 then this.Define(label, true)
            else
                if (opcode = opQincInt || opcode = opQdecInt) && pMode then opcode <- opcode + 2
                let explicitChecked = token=1001 || token=1005 || token=1006 || token=1007
                let record =
                    match this.Search(if explicitChecked then token else opcode) with
                    | Some record -> record
                    | None ->
                        if not pMode then fail $"Template {opcode} not found."
                        match this.Search(opcode + 1000) with
                        | Some record -> record
                        | None -> fail $"Template {opcode} not found."
                let isPOnly = record <> (this.Search opcode |> Option.defaultValue -1)
                if not (this.IsParameterised record) && not isPOnly then
                    this.NoParameters(opcode, record)
                else
                    // temp_found / got_p_only: P mode uses the +1000 template for parameters.
                    let record =
                        if pMode && not isPOnly then
                            match this.Search(opcode + 1000) with
                            | Some record -> record
                            | None -> fail $"Template {opcode} not found."
                        else record
                    if options.SmallCode && opcode >= opGoto && opcode <= opIfTrue then
                        fail "The <64K code option is not supported."
                    let length = this.Length record
                    if opcode = opJump then this.Jump(record, length)
                    else
                        if opcode = opLine then line <- word ir ic
                        if pMode then
                            if pass = 0 then this.PPass0Par(record, opcode, length)
                            else this.Pass1Par(record, opcode)
                        elif pass <> 0 then this.NPar1(record, opcode)
                        else
                            let d1, d2 = this.Nat0GetBytes(record, opcode)
                            let d2 = if opcode = opIfTrue || opcode = opIfFalse then d2 + 10 else d2
                            objCode <- objCode + d2
                            ic <- ic + d1

    member private this.NoParameters(opcode: int, record: int) =
        let length = this.Length record
        let pNoPars () =
            if pass = 0 then
                if this.CompilationAddress record = 0xFFFF then this.CopyPTemplate(record, length)
            else
                write (wordBytes (this.CompilationAddress record))
            objCode <- objCode + 2
        let native () =
            if pass <> 0 then write (slice lib (this.CodeAddress record) length)
            objCode <- objCode + length
        if opcode = opFast then
            if pMode then
                pMode <- false
                pNoPars ()
        elif opcode = opSlow then
            if not pMode then
                pMode <- true
                native ()
        elif pMode then pNoPars ()
        else native ()

    member private this.Passes() =
        let mutable running = true
        while running do
            match nativeLoops.TryGetValue ic,nativeBlocks.TryGetValue ic with
            | (true,loop),_ when pMode ->
                // FOR's original TOPLOOP label still addresses a threaded FAST
                // entry. Enter native mode once, take native backedges, and
                // return to threaded code before the original possible-end label.
                this.Instruction opFast
                if pass<>0 then write loop.Code
                objCode<-objCode+loop.Code.Length
                ic<-loop.EndOffset
                this.Instruction opSlow
            | _,(true,block) when not pMode ->
                if pass<>0 then write block.Code
                objCode<-objCode+block.Code.Length
                ic<-block.Ir.EndOffset
            | _ ->
                let token = word ir ic
                ic <- ic + 2
                if token = 0 then
                    if pass = 0 then
                        // Both passes use the same register plan and exact byte lengths.
                        pMode <- false
                        pass <- 1
                        let shift = int (int16 (uint16 labInc))
                        for label in List.ofSeq symbols.Keys do
                            symbols[label] <- symbols[label] + shift
                        maxTwoByte <- maxTwoByte + shift
                        objCode <- objCode + shift
                        codeSize <- objCode
                        objCode <- shift
                        line <- 0
                        ic <- 4
                    else
                        labInc <- (labInc - entryAd) &&& 0xFFFF
                        write (wordBytes labInc)
                        if codeSize <> objCode then fail "Codegen phase error."
                        running <- false
                elif token >= lowName then
                    if pass = 0 then this.Define(token, false)
                else
                    this.Instruction token

    member this.Run() : Result<TurboCodegenOutput, string list> =
        try
            if options.SmallCode then fail "The <64K code option is not supported."
            if options.DataspaceKiB < 0 || options.DataspaceKiB > int UInt16.MaxValue then
                fail "Dataspace must be between 0 and 65535 KiB."
            this.Improve()
            this.PlanNativeLoops()
            this.PlanNativeRegisters()
            this.PlanNativeArrays()
            pass <- 0
            pMode <- false
            this.Stack()
            this.Entry()
            this.WriteNativeArrays()
            this.Passes()
            Result.Ok
                { Code = output.ToArray()
                  Dataspace = options.DataspaceKiB * 1024 + extraStackBytes()
                  NativeAllocation = nativeAllocation
                  NativeLoops = nativeLoopReport }
        with
        | CodegenFailure message -> Result.Error [ $"Code generation failed, {message}" ]
        | :? IndexOutOfRangeException
        | :? ArgumentException -> Result.Error [ "Code generation failed: the IR stream is malformed (an operand runs past its end)." ]

/// Generates a QDOS executable from a Turbo IR stream (codegen410's work).
let generateFromIr (options: TurboCodegenOptions) (ir: byte[]) : Result<TurboCodegenOutput, string list> =
    Generator(library.Value, options, ir).Run()
