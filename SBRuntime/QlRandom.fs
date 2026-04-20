namespace SBRuntime

open System

type QlRandom(seed: int) =
    inherit Random()

    static let modulus = 65536u
    static let mask = modulus - 1u

    let normalizeSeed value =
        let mixed = (uint32 value * 40503u + 1u) &&& mask
        if mixed = 0u then 1u else mixed

    let mutable state = normalizeSeed seed

    let step currentState =
        let b16 = (currentState >>> 15) &&& 1u
        let b14 = (currentState >>> 13) &&& 1u
        let b13 = (currentState >>> 12) &&& 1u
        let b11 = (currentState >>> 10) &&& 1u
        let feedback = b16 ^^^ b14 ^^^ b13 ^^^ b11
        ((currentState <<< 1) &&& mask) ||| feedback

    do
        for _ = 1 to 16 do
            state <- step state

    new () = QlRandom(Environment.TickCount)

    member private _.NextRaw() =
        state <- step state
        state

    override this.Sample() =
        double (this.NextRaw()) / double modulus

    override this.NextDouble() =
        this.Sample()

    override this.Next() =
        int (this.Sample() * double Int32.MaxValue)

    override this.Next(maxValue: int) =
        if maxValue < 0 then
            raise (ArgumentOutOfRangeException(nameof maxValue))
        elif maxValue = 0 then
            0
        else
            let high = uint32 (this.NextRaw())
            let low = uint32 (this.NextRaw())
            let mutable mixed = (high <<< 16) ||| low
            mixed <- mixed ^^^ (mixed >>> 16)
            mixed <- mixed * 0x7FEB352Du
            mixed <- mixed ^^^ (mixed >>> 15)
            mixed <- mixed * 0x846CA68Bu
            mixed <- mixed ^^^ (mixed >>> 16)
            int ((uint64 mixed * uint64 maxValue) >>> 32)

    override this.Next(minValue: int, maxValue: int) =
        if minValue > maxValue then
            raise (ArgumentOutOfRangeException(nameof minValue))
        elif minValue = maxValue then
            minValue
        else
            minValue + this.Next(maxValue - minValue)

    override this.NextBytes(buffer: byte array) =
        if isNull buffer then
            raise (ArgumentNullException(nameof buffer))

        for index = 0 to buffer.Length - 1 do
            buffer[index] <- byte (this.Next(256))
