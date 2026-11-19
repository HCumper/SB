module SBTests.M68kEmulator

// A user-mode Motorola 68000 interpreter, used as a test oracle: it runs the genuine Turbo
// code generator (TurboReference/codegen_task.bin) so the F# port can be compared with it byte
// for byte. It implements the integer instruction set a QDOS job uses (no BCD, no supervisor
// state, no exceptions other than TRAP); anything else raises M68kFault so a gap is never
// silently mis-executed. Traps go to TrapHandler, and Hooks intercept calls to addresses that
// stand for QDOS vectored utilities.

open System
open System.Collections.Generic

exception M68kFault of string

[<RequireQualifiedAccess>]
type OpSize =
    | Byte
    | Word
    | Long

let private bytesOf size =
    match size with
    | OpSize.Byte -> 1u
    | OpSize.Word -> 2u
    | OpSize.Long -> 4u

let private maskOf size =
    match size with
    | OpSize.Byte -> 0xFFu
    | OpSize.Word -> 0xFFFFu
    | OpSize.Long -> 0xFFFFFFFFu

let private msbOf size =
    match size with
    | OpSize.Byte -> 0x80u
    | OpSize.Word -> 0x8000u
    | OpSize.Long -> 0x80000000u

let private sizeFromBits bits =
    match bits with
    | 0 -> OpSize.Byte
    | 1 -> OpSize.Word
    | 2 -> OpSize.Long
    | _ -> raise (M68kFault "invalid operation size")

let private signExtend8 (value: uint32) = uint32 (int32 (sbyte (byte value)))
let private signExtend16 (value: uint32) = uint32 (int32 (int16 (uint16 value)))

type private Location =
    | DataRegister of int
    | AddressRegister of int
    | Memory of uint32
    | Immediate of uint32

type Cpu(memory: byte[]) =
    let d = Array.zeroCreate<uint32> 8
    let a = Array.zeroCreate<uint32> 8
    let hooks = Dictionary<uint32, Cpu -> unit>()
    let mutable pc = 0u
    let mutable fx = false
    let mutable fn = false
    let mutable fz = false
    let mutable fv = false
    let mutable fc = false
    let mutable halted = false
    let mutable steps = 0L
    let mutable opcodePc = 0u

    member _.D = d
    member _.A = a
    member _.Memory = memory
    member _.Hooks = hooks
    member _.Steps = steps
    member _.Halted = halted
    member _.Halt() = halted <- true

    member _.Pc
        with get () = pc
        and set value = pc <- value

    member val TrapHandler: Cpu -> int -> unit = (fun _ number -> raise (M68kFault $"TRAP #{number}")) with get, set

    member _.Ccr
        with get () =
            (if fx then 0x10u else 0u) ||| (if fn then 8u else 0u) ||| (if fz then 4u else 0u)
            ||| (if fv then 2u else 0u) ||| (if fc then 1u else 0u)
        and set (value: uint32) =
            fx <- value &&& 0x10u <> 0u
            fn <- value &&& 8u <> 0u
            fz <- value &&& 4u <> 0u
            fv <- value &&& 2u <> 0u
            fc <- value &&& 1u <> 0u

    // -- memory -------------------------------------------------------------

    member private _.Fault<'T>(message: string) : 'T =
        raise (M68kFault $"{message} (opcode at ${opcodePc:X6})")

    member private this.Address(address: uint32, count: uint32) =
        let address = address &&& 0x00FFFFFFu
        if address + count > uint32 memory.Length then
            this.Fault $"address ${address:X6} is outside memory"
        int address

    member this.ReadByte(address: uint32) = uint32 memory[this.Address(address, 1u)]

    member this.ReadWord(address: uint32) =
        if address &&& 1u <> 0u then this.Fault $"odd word read at ${address:X6}"
        let index = this.Address(address, 2u)
        (uint32 memory[index] <<< 8) ||| uint32 memory[index + 1]

    member this.ReadLong(address: uint32) = (this.ReadWord address <<< 16) ||| this.ReadWord(address + 2u)

    member this.WriteByte(address: uint32, value: uint32) = memory[this.Address(address, 1u)] <- byte value

    member this.WriteWord(address: uint32, value: uint32) =
        if address &&& 1u <> 0u then this.Fault $"odd word write at ${address:X6}"
        let index = this.Address(address, 2u)
        memory[index] <- byte (value >>> 8)
        memory[index + 1] <- byte value

    member this.WriteLong(address: uint32, value: uint32) =
        this.WriteWord(address, value >>> 16)
        this.WriteWord(address + 2u, value)

    member this.Read(size: OpSize, address: uint32) =
        match size with
        | OpSize.Byte -> this.ReadByte address
        | OpSize.Word -> this.ReadWord address
        | OpSize.Long -> this.ReadLong address

    member this.Write(size: OpSize, address: uint32, value: uint32) =
        match size with
        | OpSize.Byte -> this.WriteByte(address, value)
        | OpSize.Word -> this.WriteWord(address, value)
        | OpSize.Long -> this.WriteLong(address, value)

    member this.Push(value: uint32) =
        a[7] <- a[7] - 4u
        this.WriteLong(a[7], value)

    member this.Pop() =
        let value = this.ReadLong a[7]
        a[7] <- a[7] + 4u
        value

    member private this.Fetch16() =
        let value = this.ReadWord pc
        pc <- pc + 2u
        value

    member private this.Fetch32() =
        let value = this.ReadLong pc
        pc <- pc + 4u
        value

    // -- effective addresses ------------------------------------------------

    member private this.Index(baseAddress: uint32) =
        let extension = this.Fetch16()
        let register = int ((extension >>> 12) &&& 7u)
        let raw = if extension &&& 0x8000u <> 0u then a[register] else d[register]
        let index = if extension &&& 0x0800u <> 0u then raw else signExtend16 raw
        baseAddress + index + signExtend8 (extension &&& 0xFFu)

    member private this.Resolve(mode: int, register: int, size: OpSize) : Location =
        match mode with
        | 0 -> DataRegister register
        | 1 -> AddressRegister register
        | 2 -> Memory a[register]
        | 3 ->
            let address = a[register]
            let step = if size = OpSize.Byte && register = 7 then 2u else bytesOf size
            a[register] <- address + step
            Memory address
        | 4 ->
            let step = if size = OpSize.Byte && register = 7 then 2u else bytesOf size
            a[register] <- a[register] - step
            Memory a[register]
        | 5 -> Memory(a[register] + signExtend16 (this.Fetch16()))
        | 6 -> Memory(this.Index a[register])
        | _ ->
            match register with
            | 0 -> Memory(signExtend16 (this.Fetch16()))
            | 1 -> Memory(this.Fetch32())
            | 2 ->
                let basePc = pc
                Memory(basePc + signExtend16 (this.Fetch16()))
            | 3 ->
                let basePc = pc
                Memory(this.Index basePc)
            | 4 ->
                match size with
                | OpSize.Byte -> Immediate(this.Fetch16() &&& 0xFFu)
                | OpSize.Word -> Immediate(this.Fetch16())
                | OpSize.Long -> Immediate(this.Fetch32())
            | _ -> this.Fault "invalid addressing mode"

    member private this.Load(size: OpSize, location: Location) =
        match location with
        | DataRegister register -> d[register] &&& maskOf size
        | AddressRegister register -> a[register] &&& maskOf size
        | Memory address -> this.Read(size, address)
        | Immediate value -> value &&& maskOf size

    member private this.Store(size: OpSize, location: Location, value: uint32) =
        match location with
        | DataRegister register -> d[register] <- (d[register] &&& ~~~(maskOf size)) ||| (value &&& maskOf size)
        | AddressRegister register -> a[register] <- value
        | Memory address -> this.Write(size, address, value)
        | Immediate _ -> this.Fault "write to an immediate"

    member private this.ControlAddress(mode: int, register: int) =
        match this.Resolve(mode, register, OpSize.Long) with
        | Memory address -> address
        | _ -> this.Fault "control addressing mode expected"

    // -- flags --------------------------------------------------------------

    member private _.SetNZ(size: OpSize, result: uint32) =
        fn <- result &&& msbOf size <> 0u
        fz <- result &&& maskOf size = 0u

    member private this.SetLogic(size: OpSize, result: uint32) =
        this.SetNZ(size, result)
        fv <- false
        fc <- false

    member private this.AddCore(size: OpSize, source: uint32, destination: uint32, carryIn: uint32, keepZero: bool) =
        let mask = maskOf size
        let msb = msbOf size
        let s = source &&& mask
        let t = destination &&& mask
        let result = (s + t + carryIn) &&& mask
        fc <- ((s &&& t) ||| (~~~result &&& (s ||| t))) &&& msb <> 0u
        fv <- ((s ^^^ result) &&& (t ^^^ result)) &&& msb <> 0u
        fx <- fc
        fn <- result &&& msb <> 0u
        if keepZero then
            if result <> 0u then fz <- false
        else
            fz <- result = 0u
        result

    /// destination - source - borrowIn, setting X when setX.
    member private this.SubCore(size: OpSize, source: uint32, destination: uint32, borrowIn: uint32, setX: bool, keepZero: bool) =
        let mask = maskOf size
        let msb = msbOf size
        let s = source &&& mask
        let t = destination &&& mask
        let result = (t - s - borrowIn) &&& mask
        fc <- ((s &&& ~~~t) ||| (result &&& ~~~t) ||| (s &&& result)) &&& msb <> 0u
        fv <- ((t ^^^ s) &&& (t ^^^ result)) &&& msb <> 0u
        if setX then fx <- fc
        fn <- result &&& msb <> 0u
        if keepZero then
            if result <> 0u then fz <- false
        else
            fz <- result = 0u
        result

    member private _.Condition(code: int) =
        match code with
        | 0 -> true
        | 1 -> false
        | 2 -> not fc && not fz
        | 3 -> fc || fz
        | 4 -> not fc
        | 5 -> fc
        | 6 -> not fz
        | 7 -> fz
        | 8 -> not fv
        | 9 -> fv
        | 10 -> not fn
        | 11 -> fn
        | 12 -> fn = fv
        | 13 -> fn <> fv
        | 14 -> not fz && fn = fv
        | _ -> fz || fn <> fv

    // -- instruction groups -------------------------------------------------

    member private this.BitOperation(kind: int, bitNumber: uint32, mode: int, register: int) =
        if mode = 0 then
            let bit = int (bitNumber % 32u)
            let value = d[register]
            fz <- (value >>> bit) &&& 1u = 0u
            match kind with
            | 1 -> d[register] <- value ^^^ (1u <<< bit)
            | 2 -> d[register] <- value &&& ~~~(1u <<< bit)
            | 3 -> d[register] <- value ||| (1u <<< bit)
            | _ -> ()
        else
            let location = this.Resolve(mode, register, OpSize.Byte)
            let bit = int (bitNumber % 8u)
            let value = this.Load(OpSize.Byte, location)
            fz <- (value >>> bit) &&& 1u = 0u
            match kind with
            | 1 -> this.Store(OpSize.Byte, location, value ^^^ (1u <<< bit))
            | 2 -> this.Store(OpSize.Byte, location, value &&& ~~~(1u <<< bit))
            | 3 -> this.Store(OpSize.Byte, location, value ||| (1u <<< bit))
            | _ -> ()

    member private this.Group0(op: int) =
        let mode = (op >>> 3) &&& 7
        let register = op &&& 7
        if op &&& 0x0100 <> 0 then
            if mode = 1 then this.Fault "MOVEP is not supported"
            this.BitOperation((op >>> 6) &&& 3, d[(op >>> 9) &&& 7], mode, register)
        else
            match (op >>> 9) &&& 7 with
            | 4 ->
                let bitNumber = this.Fetch16() &&& 0xFFu
                this.BitOperation((op >>> 6) &&& 3, bitNumber, mode, register)
            | kind ->
                if mode = 7 && register = 4 then
                    let size = (op >>> 6) &&& 3
                    if size <> 0 then this.Fault "immediate operation on SR is not supported"
                    let value = this.Fetch16() &&& 0xFFu
                    match kind with
                    | 0 -> this.Ccr <- this.Ccr ||| value
                    | 1 -> this.Ccr <- this.Ccr &&& value
                    | 5 -> this.Ccr <- this.Ccr ^^^ value
                    | _ -> this.Fault "invalid CCR operation"
                else
                    let size = sizeFromBits ((op >>> 6) &&& 3)
                    let immediate =
                        match size with
                        | OpSize.Long -> this.Fetch32()
                        | OpSize.Word -> this.Fetch16()
                        | OpSize.Byte -> this.Fetch16() &&& 0xFFu
                    let location = this.Resolve(mode, register, size)
                    let value = this.Load(size, location)
                    match kind with
                    | 0 ->
                        let result = value ||| immediate
                        this.Store(size, location, result)
                        this.SetLogic(size, result)
                    | 1 ->
                        let result = value &&& immediate
                        this.Store(size, location, result)
                        this.SetLogic(size, result)
                    | 2 -> this.Store(size, location, this.SubCore(size, immediate, value, 0u, true, false))
                    | 3 -> this.Store(size, location, this.AddCore(size, immediate, value, 0u, false))
                    | 5 ->
                        let result = value ^^^ immediate
                        this.Store(size, location, result)
                        this.SetLogic(size, result)
                    | 6 -> this.SubCore(size, immediate, value, 0u, false, false) |> ignore
                    | _ -> this.Fault "invalid immediate operation"

    member private this.Move(op: int, size: OpSize) =
        let source = this.Resolve((op >>> 3) &&& 7, op &&& 7, size)
        let value = this.Load(size, source)
        let destinationMode = (op >>> 6) &&& 7
        let destinationRegister = (op >>> 9) &&& 7
        if destinationMode = 1 then
            a[destinationRegister] <- (if size = OpSize.Word then signExtend16 value else value)
        else
            let destination = this.Resolve(destinationMode, destinationRegister, size)
            this.Store(size, destination, value)
            this.SetLogic(size, value)

    member private this.RegisterValue(index: int) = if index < 8 then d[index] else a[index - 8]

    member private this.SetRegister(index: int, value: uint32) =
        if index < 8 then d[index] <- value else a[index - 8] <- value

    member private this.Movem(op: int) =
        let registerMask = this.Fetch16()
        let size = if op &&& 0x40 <> 0 then OpSize.Long else OpSize.Word
        let step = bytesOf size
        let mode = (op >>> 3) &&& 7
        let register = op &&& 7
        if op &&& 0x0400 = 0 then
            if mode = 4 then
                let mutable address = a[register]
                for bit in 0..15 do
                    if registerMask &&& (1u <<< bit) <> 0u then
                        address <- address - step
                        this.Write(size, address, this.RegisterValue(15 - bit))
                a[register] <- address
            else
                let mutable address = this.ControlAddress(mode, register)
                for bit in 0..15 do
                    if registerMask &&& (1u <<< bit) <> 0u then
                        this.Write(size, address, this.RegisterValue bit)
                        address <- address + step
        else
            let mutable address = if mode = 3 then a[register] else this.ControlAddress(mode, register)
            for bit in 0..15 do
                if registerMask &&& (1u <<< bit) <> 0u then
                    let value = this.Read(size, address)
                    this.SetRegister(bit, (if size = OpSize.Word then signExtend16 value else value))
                    address <- address + step
            if mode = 3 then a[register] <- address

    member private this.Group4(op: int) =
        let mode = (op >>> 3) &&& 7
        let register = op &&& 7
        if op = 0x4E71 then ()
        elif op = 0x4E75 then pc <- this.Pop()
        elif op = 0x4E77 then
            this.Ccr <- this.ReadWord a[7]
            a[7] <- a[7] + 2u
            pc <- this.Pop()
        elif op &&& 0xFFF0 = 0x4E40 then this.TrapHandler this (op &&& 15)
        elif op &&& 0xFFF8 = 0x4E50 then
            let displacement = signExtend16 (this.Fetch16())
            this.Push a[register]
            a[register] <- a[7]
            a[7] <- a[7] + displacement
        elif op &&& 0xFFF8 = 0x4E58 then
            a[7] <- a[register]
            a[register] <- this.Pop()
        elif op &&& 0xFFC0 = 0x4E80 then
            let target = this.ControlAddress(mode, register)
            this.Push pc
            pc <- target
        elif op &&& 0xFFC0 = 0x4EC0 then pc <- this.ControlAddress(mode, register)
        elif op &&& 0xFFF8 = 0x4840 then
            let value = (d[register] >>> 16) ||| (d[register] <<< 16)
            d[register] <- value
            this.SetLogic(OpSize.Long, value)
        elif op &&& 0xFFC0 = 0x4840 then this.Push(this.ControlAddress(mode, register))
        elif op &&& 0xFFF8 = 0x4880 then
            let value = signExtend8 d[register] &&& 0xFFFFu
            d[register] <- (d[register] &&& 0xFFFF0000u) ||| value
            this.SetLogic(OpSize.Word, value)
        elif op &&& 0xFFF8 = 0x48C0 then
            d[register] <- signExtend16 d[register]
            this.SetLogic(OpSize.Long, d[register])
        elif op &&& 0xFB80 = 0x4880 then this.Movem op
        elif op &&& 0xF1C0 = 0x41C0 then a[(op >>> 9) &&& 7] <- this.ControlAddress(mode, register)
        elif op &&& 0xFFC0 = 0x44C0 then this.Ccr <- this.Load(OpSize.Word, this.Resolve(mode, register, OpSize.Word))
        elif op &&& 0xFFC0 = 0x40C0 then this.Store(OpSize.Word, this.Resolve(mode, register, OpSize.Word), this.Ccr)
        elif op &&& 0xFFC0 = 0x4AC0 then
            let location = this.Resolve(mode, register, OpSize.Byte)
            let value = this.Load(OpSize.Byte, location)
            this.SetLogic(OpSize.Byte, value)
            this.Store(OpSize.Byte, location, value ||| 0x80u)
        else
            let sizeBits = (op >>> 6) &&& 3
            if sizeBits = 3 then this.Fault $"unsupported opcode ${op:X4}"
            let size = sizeFromBits sizeBits
            match op &&& 0xFF00 with
            | 0x4000 ->
                let location = this.Resolve(mode, register, size)
                let value = this.Load(size, location)
                this.Store(size, location, this.SubCore(size, value, 0u, (if fx then 1u else 0u), true, true))
            | 0x4200 ->
                let location = this.Resolve(mode, register, size)
                this.Store(size, location, 0u)
                fn <- false
                fz <- true
                fv <- false
                fc <- false
            | 0x4400 ->
                let location = this.Resolve(mode, register, size)
                let value = this.Load(size, location)
                this.Store(size, location, this.SubCore(size, value, 0u, 0u, true, false))
            | 0x4600 ->
                let location = this.Resolve(mode, register, size)
                let result = ~~~(this.Load(size, location)) &&& maskOf size
                this.Store(size, location, result)
                this.SetLogic(size, result)
            | 0x4A00 -> this.SetLogic(size, this.Load(size, this.Resolve(mode, register, size)))
            | _ -> this.Fault $"unsupported opcode ${op:X4}"

    member private this.Group5(op: int) =
        let mode = (op >>> 3) &&& 7
        let register = op &&& 7
        let sizeBits = (op >>> 6) &&& 3
        if sizeBits = 3 then
            let condition = (op >>> 8) &&& 15
            if mode = 1 then
                let basePc = pc
                let displacement = signExtend16 (this.Fetch16())
                if not (this.Condition condition) then
                    let count = (d[register] - 1u) &&& 0xFFFFu
                    d[register] <- (d[register] &&& 0xFFFF0000u) ||| count
                    if count <> 0xFFFFu then pc <- basePc + displacement
            else
                let location = this.Resolve(mode, register, OpSize.Byte)
                this.Store(OpSize.Byte, location, (if this.Condition condition then 0xFFu else 0u))
        else
            let quick = (op >>> 9) &&& 7
            let data = if quick = 0 then 8u else uint32 quick
            let subtract = op &&& 0x0100 <> 0
            if mode = 1 then
                a[register] <- (if subtract then a[register] - data else a[register] + data)
            else
                let size = sizeFromBits sizeBits
                let location = this.Resolve(mode, register, size)
                let value = this.Load(size, location)
                let result =
                    if subtract then this.SubCore(size, data, value, 0u, true, false)
                    else this.AddCore(size, data, value, 0u, false)
                this.Store(size, location, result)

    member private this.Branch(op: int) =
        let condition = (op >>> 8) &&& 15
        let basePc = pc
        let displacement8 = uint32 (op &&& 0xFF)
        let displacement = if displacement8 = 0u then signExtend16 (this.Fetch16()) else signExtend8 displacement8
        match condition with
        | 0 -> pc <- basePc + displacement
        | 1 ->
            this.Push pc
            pc <- basePc + displacement
        | _ -> if this.Condition condition then pc <- basePc + displacement

    member private this.Divide(op: int, signed: bool) =
        let register = (op >>> 9) &&& 7
        let divisor = this.Load(OpSize.Word, this.Resolve((op >>> 3) &&& 7, op &&& 7, OpSize.Word))
        if divisor = 0u then this.Fault "division by zero"
        fc <- false
        if signed then
            let dividend = int64 (int32 d[register])
            let divisorValue = int64 (int16 (uint16 divisor))
            let quotient = dividend / divisorValue
            let remainder = dividend % divisorValue
            if quotient < -32768L || quotient > 32767L then
                fv <- true
            else
                d[register] <- ((uint32 (int32 remainder) &&& 0xFFFFu) <<< 16) ||| (uint32 (int32 quotient) &&& 0xFFFFu)
                fv <- false
                this.SetNZ(OpSize.Word, uint32 (int32 quotient))
        else
            let dividend = d[register]
            let quotient = dividend / divisor
            let remainder = dividend % divisor
            if quotient > 0xFFFFu then
                fv <- true
            else
                d[register] <- (remainder <<< 16) ||| quotient
                fv <- false
                this.SetNZ(OpSize.Word, quotient)

    member private this.LogicalGroup(op: int, operation: uint32 -> uint32 -> uint32) =
        let register = (op >>> 9) &&& 7
        let size = sizeFromBits ((op >>> 6) &&& 3)
        let location = this.Resolve((op >>> 3) &&& 7, op &&& 7, size)
        if op &&& 0x0100 = 0 then
            let result = operation (d[register] &&& maskOf size) (this.Load(size, location))
            this.Store(size, DataRegister register, result)
            this.SetLogic(size, result)
        else
            let result = operation (this.Load(size, location)) (d[register] &&& maskOf size)
            this.Store(size, location, result)
            this.SetLogic(size, result)

    member private this.AddSub(op: int, subtract: bool) =
        let register = (op >>> 9) &&& 7
        let opmode = (op >>> 6) &&& 7
        let mode = (op >>> 3) &&& 7
        let sourceRegister = op &&& 7
        match opmode with
        | 3
        | 7 ->
            let size = if opmode = 3 then OpSize.Word else OpSize.Long
            let raw = this.Load(size, this.Resolve(mode, sourceRegister, size))
            let value = if size = OpSize.Word then signExtend16 raw else raw
            a[register] <- (if subtract then a[register] - value else a[register] + value)
        | 0
        | 1
        | 2 ->
            let size = sizeFromBits opmode
            let source = this.Load(size, this.Resolve(mode, sourceRegister, size))
            let destination = d[register]
            let result =
                if subtract then this.SubCore(size, source, destination, 0u, true, false)
                else this.AddCore(size, source, destination, 0u, false)
            this.Store(size, DataRegister register, result)
        | _ ->
            let size = sizeFromBits (opmode - 4)
            let extend = if fx then 1u else 0u
            if mode = 0 then
                let result =
                    if subtract then this.SubCore(size, d[sourceRegister], d[register], extend, true, true)
                    else this.AddCore(size, d[sourceRegister], d[register], extend, true)
                this.Store(size, DataRegister register, result)
            elif mode = 1 then
                let source = this.Load(size, this.Resolve(4, sourceRegister, size))
                let destinationLocation = this.Resolve(4, register, size)
                let destination = this.Load(size, destinationLocation)
                let result =
                    if subtract then this.SubCore(size, source, destination, extend, true, true)
                    else this.AddCore(size, source, destination, extend, true)
                this.Store(size, destinationLocation, result)
            else
                let location = this.Resolve(mode, sourceRegister, size)
                let value = this.Load(size, location)
                let result =
                    if subtract then this.SubCore(size, d[register], value, 0u, true, false)
                    else this.AddCore(size, d[register], value, 0u, false)
                this.Store(size, location, result)

    member private this.GroupB(op: int) =
        let register = (op >>> 9) &&& 7
        let opmode = (op >>> 6) &&& 7
        let mode = (op >>> 3) &&& 7
        let sourceRegister = op &&& 7
        match opmode with
        | 3
        | 7 ->
            let size = if opmode = 3 then OpSize.Word else OpSize.Long
            let raw = this.Load(size, this.Resolve(mode, sourceRegister, size))
            let value = if size = OpSize.Word then signExtend16 raw else raw
            this.SubCore(OpSize.Long, value, a[register], 0u, false, false) |> ignore
        | 0
        | 1
        | 2 ->
            let size = sizeFromBits opmode
            let source = this.Load(size, this.Resolve(mode, sourceRegister, size))
            this.SubCore(size, source, d[register], 0u, false, false) |> ignore
        | _ ->
            let size = sizeFromBits (opmode - 4)
            if mode = 1 then
                let source = this.Load(size, this.Resolve(3, sourceRegister, size))
                let destination = this.Load(size, this.Resolve(3, register, size))
                this.SubCore(size, source, destination, 0u, false, false) |> ignore
            else
                let location = this.Resolve(mode, sourceRegister, size)
                let result = this.Load(size, location) ^^^ (d[register] &&& maskOf size)
                this.Store(size, location, result)
                this.SetLogic(size, result)

    member private this.GroupC(op: int) =
        let register = (op >>> 9) &&& 7
        let mode = (op >>> 3) &&& 7
        let sourceRegister = op &&& 7
        if op &&& 0xF1C0 = 0xC0C0 then
            let source = this.Load(OpSize.Word, this.Resolve(mode, sourceRegister, OpSize.Word))
            let result = (d[register] &&& 0xFFFFu) * source
            d[register] <- result
            this.SetLogic(OpSize.Long, result)
        elif op &&& 0xF1C0 = 0xC1C0 then
            let source = this.Load(OpSize.Word, this.Resolve(mode, sourceRegister, OpSize.Word))
            let result = uint32 (int32 (int16 (uint16 d[register])) * int32 (int16 (uint16 source)))
            d[register] <- result
            this.SetLogic(OpSize.Long, result)
        elif op &&& 0xF1F8 = 0xC140 then
            let value = d[register]
            d[register] <- d[sourceRegister]
            d[sourceRegister] <- value
        elif op &&& 0xF1F8 = 0xC148 then
            let value = a[register]
            a[register] <- a[sourceRegister]
            a[sourceRegister] <- value
        elif op &&& 0xF1F8 = 0xC188 then
            let value = d[register]
            d[register] <- a[sourceRegister]
            a[sourceRegister] <- value
        elif op &&& 0xF1F0 = 0xC100 then
            this.Fault "ABCD is not supported"
        else
            this.LogicalGroup(op, (fun x y -> x &&& y))

    member private this.Shift(kind: int, left: bool, size: OpSize, value: uint32, count: int) =
        let mask = maskOf size
        let msb = msbOf size
        let mutable result = value &&& mask
        let mutable carry = false
        let mutable overflow = false
        match kind with
        | 0
        | 1 ->
            for _ in 1..count do
                if left then
                    carry <- result &&& msb <> 0u
                    let shifted = (result <<< 1) &&& mask
                    if kind = 0 && (shifted &&& msb) <> (result &&& msb) then overflow <- true
                    result <- shifted
                else
                    carry <- result &&& 1u <> 0u
                    result <- if kind = 0 then (result >>> 1) ||| (result &&& msb) else result >>> 1
            if count > 0 then fx <- carry
        | 3 ->
            for _ in 1..count do
                if left then
                    carry <- result &&& msb <> 0u
                    result <- ((result <<< 1) &&& mask) ||| (if carry then 1u else 0u)
                else
                    carry <- result &&& 1u <> 0u
                    result <- (result >>> 1) ||| (if carry then msb else 0u)
        | _ ->
            carry <- fx
            for _ in 1..count do
                if left then
                    let out = result &&& msb <> 0u
                    result <- ((result <<< 1) &&& mask) ||| (if fx then 1u else 0u)
                    fx <- out
                else
                    let out = result &&& 1u <> 0u
                    result <- (result >>> 1) ||| (if fx then msb else 0u)
                    fx <- out
                carry <- fx
        fc <- carry
        fv <- overflow
        this.SetNZ(size, result)
        result

    member private this.GroupE(op: int) =
        let sizeBits = (op >>> 6) &&& 3
        let left = op &&& 0x0100 <> 0
        if sizeBits = 3 then
            let kind = (op >>> 9) &&& 3
            let location = this.Resolve((op >>> 3) &&& 7, op &&& 7, OpSize.Word)
            let value = this.Load(OpSize.Word, location)
            this.Store(OpSize.Word, location, this.Shift(kind, left, OpSize.Word, value, 1))
        else
            let size = sizeFromBits sizeBits
            let countField = (op >>> 9) &&& 7
            let count =
                if op &&& 0x20 = 0 then (if countField = 0 then 8 else countField)
                else int (d[countField] % 64u)
            let register = op &&& 7
            let result = this.Shift((op >>> 3) &&& 3, left, size, d[register], count)
            this.Store(size, DataRegister register, result)

    /// Executes one instruction (or one hooked utility call).
    member this.Step() =
        steps <- steps + 1L
        match hooks.TryGetValue pc with
        | true, hook ->
            hook this
            pc <- this.Pop()
        | _ ->
            opcodePc <- pc
            let op = int (this.Fetch16())
            match op >>> 12 with
            | 0x0 -> this.Group0 op
            | 0x1 -> this.Move(op, OpSize.Byte)
            | 0x2 -> this.Move(op, OpSize.Long)
            | 0x3 -> this.Move(op, OpSize.Word)
            | 0x4 -> this.Group4 op
            | 0x5 -> this.Group5 op
            | 0x6 -> this.Branch op
            | 0x7 ->
                if op &&& 0x0100 <> 0 then this.Fault "invalid MOVEQ"
                let value = signExtend8 (uint32 op)
                d[(op >>> 9) &&& 7] <- value
                this.SetLogic(OpSize.Long, value)
            | 0x8 ->
                if op &&& 0xF1C0 = 0x80C0 then this.Divide(op, false)
                elif op &&& 0xF1C0 = 0x81C0 then this.Divide(op, true)
                elif op &&& 0xF1F0 = 0x8100 then this.Fault "SBCD is not supported"
                else this.LogicalGroup(op, (fun x y -> x ||| y))
            | 0x9 -> this.AddSub(op, true)
            | 0xB -> this.GroupB op
            | 0xC -> this.GroupC op
            | 0xD -> this.AddSub(op, false)
            | 0xE -> this.GroupE op
            | _ -> this.Fault $"unsupported opcode ${op:X4}"

    /// Runs until Halt is called or the step limit is reached.
    member this.Run(maxSteps: int64) =
        while not halted do
            if steps >= maxSteps then raise (M68kFault $"step limit {maxSteps} reached at ${pc:X6}")
            this.Step()
