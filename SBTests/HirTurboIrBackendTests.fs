module SBTests.HirTurboIrBackendTests

// Turbo IR backend tests. Each source case is a small SB program and the exact IR its program
// body (or the named section) must lower to; listings are compared line by line. The spec's
// own examples (TURBO-IR-SPECIFICATION.md section 10) are reproduced byte for byte, and every
// stream produced here is decoded and validated against the specification's operand catalogue.

open System
open System.IO
open NUnit.Framework
open Serilog

open Types
open HIR
open CompilerPipeline
open HirTurboIrBackend

let private pos =
    { BasicLineNo = None
      EditorLineNo = 1
      Column = 0 }

let private literalInt value = Literal(ConstInt value, HirType.Int, pos)

let private storage symbol name hirType storageClass =
    { Symbol = symbol
      Slot = StorageSlotId 0
      Name = name
      Type = hirType
      Dimensions = None
      Class = storageClass
      Position = pos }

let private arrayStorage symbol name elementType dims =
    { storage symbol name (HirType.Array elementType) GlobalStorage with Dimensions = Some dims }

let private parameter symbol routineName name hirType binding =
    { Storage = storage symbol name hirType (RoutineParameterStorage routineName)
      Binding = binding }

let private procedure symbol name parameters body =
    { Name = name
      Symbol = symbol
      Parameters = parameters
      Locals = []
      Body = body
      ReturnType = None
      EndLineNumber = None
      Position = pos }

let private emptyProgram =
    { SymbolNames = Map.empty
      Globals = []
      Routines = []
      DataEntries = []
      RestorePoints = []
      Main = [] }

let private noWindows = { defaultOptions with WindowCount = 0 }

let private newline = string (char 10)

/// TURBO-IR-EXAMPLE.hex: the specification's 108-byte stream for n% = 42 + 1 (spec 10.1).
let private specExampleHex = """
    03 E7 00 02 00 FF 00 F2 27 11 00 FE 27 10 00 FB
    00 FF 27 11 00 F2 27 15 00 FF 27 12 01 46 01 5A
    00 6A 00 2A 00 6A 00 01 00 05 00 76 00 01 00 DC
    00 F2 27 17 00 FE 27 16 01 05 FF FF 00 00 00 FF
    27 17 00 FF 27 15 00 72 00 01 01 0B 00 ED 27 10
    00 F2 27 12 00 FE 27 13 01 05 FF FF 00 00 00 F3
    27 13 01 66 00 00 00 F3 27 16 00 00
"""

let private hexBytes (text: string) =
    text.Split([| ' '; char 10; char 13 |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun pair -> Convert.ToByte(pair, 16))

let private toHex (bytes: byte[]) =
    bytes |> Array.map (fun b -> b.ToString("X2")) |> String.concat " "

let private lines (text: string) =
    text.Split(char 10)
    |> Array.map (fun line -> line.Trim())
    |> Array.filter (fun line -> line <> "")
    |> List.ofArray

/// SB source from an indented literal: one numbered line per row.
let private source (text: string) = String.concat newline (lines text) + newline

let private assertLines (expected: string) (actual: string list) =
    Assert.That(String.concat newline actual, Is.EqualTo(String.concat newline (lines expected)))

let private listingLines (output: TurboIrOutput) = lines output.Listing

/// The program body: the records after 10002's two retry checkpoints, up to the branch around
/// the linkage list. Routines are included.
let private body output =
    let all = listingLines output
    let start = all |> List.findIndex (fun line -> line.StartsWith "10002:")
    let finish = all |> List.findIndex (fun line -> line = "242 10007")
    all[start + 3 .. finish - 1]

/// The lines from the first starting with first to the next starting with last, inclusive.
let private section (first: string) (last: string) output =
    let all = listingLines output
    let start = all |> List.findIndex (fun line -> line.StartsWith first)
    let finish = start + (all |> List.skip start |> List.findIndex (fun line -> line.StartsWith last))
    all[start..finish]

let private testSettings inputFile =
    { InputFileName = inputFile
      OutputFileName = Path.GetTempFileName()
      Verbose = false
      Backend = "turboir"
      RuntimeHost = "console"
      ExecutionThrottleEnabled = false
      ExecutionThrottleTargetStatementsPerSecond = 10000
      ExecutionThrottleMaxRunAheadMilliseconds = 2
      SyntaxChecking = Relaxed
      AppName = "Test"
      Logger = LoggerConfiguration().CreateLogger() }

let private lowerSource (text: string) =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.bas")
    File.WriteAllText(path, text)
    try
        match loadAstFromInput (testSettings path) with
        | Result.Error error -> failwith $"Parsing failed: %A{error}"
        | Result.Ok(_, _, ast) ->
            let state = runSemanticAnalysis ast
            if not state.Errors.IsEmpty then
                failwith ("Semantic analysis failed: " + String.concat "; " state.Errors)
            match runHirLowering state with
            | Result.Error errors -> failwith $"HIR lowering failed: %A{errors}"
            | Result.Ok hir -> state, hir
    finally
        File.Delete path

let private expectOk result =
    match result with
    | Result.Ok output ->
        Assert.That(validate output.Bytes, Is.Empty)
        Assert.That(decode output.Bytes, Is.EqualTo(Result<IrItem list, string>.Ok(output.Items |> List.map normalizeItem)))
        output
    | Result.Error errors -> failwith ("Turbo IR generation failed: " + String.concat "; " errors)

let private expectErrors result =
    match result with
    | Result.Ok _ -> failwith "Expected Turbo IR generation to fail."
    | Result.Error errors -> errors

let private compileWith configure text =
    let state, hir = lowerSource text
    generateTurboIrFromHir (configure (turboIrOptionsFromState state)) hir

let private compile text = compileWith id text |> expectOk

// ---------------------------------------------------------------------------
// The specification's examples (section 10)
// ---------------------------------------------------------------------------

let private specExampleProgram =
    let n = SymbolId 0
    { emptyProgram with
        SymbolNames = Map.ofList [ n, "N%" ]
        Globals = [ storage n "N%" HirType.Int GlobalStorage ]
        Main = [ Assign(WriteVar(n, HirType.Int, pos), Binary(Add, literalInt 42, literalInt 1, HirType.Int, pos), pos) ] }

[<Test>]
let ``spec 10.1 integer example produces the reference 108-byte stream`` () =
    let output = generateTurboIrFromHir noWindows specExampleProgram |> expectOk

    Assert.That(toHex output.Bytes, Is.EqualTo(toHex (hexBytes specExampleHex)))
    assertLines """
        999 2 ; vector table
        255
        242 10001
        254
        10000: ; DATA
        251
        255
        10001: ; first initialisation: windows, arrays, strings
        242 10005
        255
        10002: ; program body
        326
        346
        106 42
        106 1
        5
        118 1 ; N%
        220
        242 10007
        254
        10006: ; GLOBAL/EXTERNAL linkage list (empty)
        261 -1 "" ; end of list
        255
        10007:
        255
        10005: ; second initialisation: scalars
        114 1 ; N%
        267
        237 10000
        242 10002
        254
        10003: ; resident routine names
        261 -1 "" ; end of list
        243 10003 ; startup trailer
        358 0 ; own module number
        243 10006
        0
    """ (listingLines output)

[<Test>]
let ``spec 10.1 example compiled from source matches the reference stream`` () =
    let output = compile (source """
        10 TURBO_windo 0
        20 n% = 42 + 1
    """)

    Assert.That(toHex output.Bytes, Is.EqualTo(toHex (hexBytes specExampleHex)))
    Assert.That(output.Warnings, Is.Empty)

[<Test>]
let ``spec 10.2 comparison yields a float condition for 239`` () =
    let output = compile (source """
        10 n% = 11
        20 IF n% > 10 THEN
        30   PRINT "big"
        40 ELSE
        50   PRINT "small"
        60 END IF
    """)

    assertLines """
        106 11
        118 1 ; N%
        109 1 ; N%
        106 10
        10
        239 2000
        106 1
        281
        169 "big"
        186
        203
        242 2001
        2000: ; ELSE
        106 1
        281
        169 "small"
        186
        203
        2001: ; END IF
        220
    """ (body output)

[<Test>]
let ``spec 10.2 integer array element assignment pushes subscripts then value`` () =
    // IDs 2, 3, 4 and 5 for A%, I%, J% and X%, as in the specification.
    let n, a, i, j, x = SymbolId 0, SymbolId 1, SymbolId 2, SymbolId 3, SymbolId 4
    let program =
        { emptyProgram with
            Globals =
                [ storage n "N%" HirType.Int GlobalStorage
                  arrayStorage a "A%" HirType.Int [ 3; 3 ]
                  storage i "I%" HirType.Int GlobalStorage
                  storage j "J%" HirType.Int GlobalStorage
                  storage x "X%" HirType.Int GlobalStorage ]
            Main =
                [ Assign(
                    WriteArrayElem(a, [ ReadVar(i, HirType.Int, pos); ReadVar(j, HirType.Int, pos) ], HirType.Int, pos),
                    ReadVar(x, HirType.Int, pos),
                    pos) ] }

    let output = generateTurboIrFromHir noWindows program |> expectOk

    assertLines """
        106 3
        106 3
        126 1 2 ; A%
        109 3 ; I%
        109 4 ; J%
        109 5 ; X%
        112 2 ; A%
        220
    """ (body output)
    assertLines """
        10001: ; first initialisation: windows, arrays, strings
        234 2 2 ; A%
        242 10005
    """ (section "10001:" "242 10005" output)

[<Test>]
let ``spec 10.3 string literal record is word length text and a pad byte`` () =
    let output = compile (source """
        10 PRINT "Hi!"
    """)

    assertLines """
        106 1
        281
        169 "Hi!"
        186
        203
        220
    """ (body output)
    Assert.That(toHex output.Bytes, Does.Contain("00 A9 00 03 48 69 21 00"))

[<Test>]
let ``spec 10.3 resident procedure call marks the stack and describes its arguments`` () =
    let output = compile (source """
        10 TURBO_windo 0
        20 BEEP 7, 2.0
    """)

    assertLines """
        250
        106 7
        376 2
        227 1 [13 02] ; BEEP
        220
    """ (body output)
    // 227 W(vector) B(2) B(0x13) B(0x02) B(0): integer then comma, float then end, pad.
    Assert.That(toHex output.Bytes, Does.Contain("00 E3 00 01 02 13 02 00"))
    // The resident name list entry: 261 W(1) B(4) "BEEP" and a pad byte.
    Assert.That(toHex output.Bytes, Does.Contain("01 05 00 01 04 42 45 45 50 00"))

// ---------------------------------------------------------------------------
// Constructs
// ---------------------------------------------------------------------------

[<Test>]
let ``arithmetic follows Turbo types, conversions and logical operators`` () =
    let output = compile (source """
        10 a% = 7 : b = 2.5 : s$ = "x"
        20 c = a% + b
        30 d% = a% * 3 - 1
        40 e = a% / 2
        50 f% = a% DIV 2 + a% MOD 3
        60 g = 2 ^ a%
        70 h% = -a%
        80 i = -b
        90 j = NOT a%
        100 k = a% > 1 AND b <= 3 OR s$ = "y" XOR b <> 0
        110 t$ = s$ & a% & b
        120 p% = "lo" INSTR "hello"
        130 q% = b
        140 r = "12"
    """)

    assertLines """
        106 7
        118 1 ; A%
        377 2050 20480 ; 2.5
        150 2 ; B
        169 "x"
        264 15 ; S$
        109 1 ; A%
        320
        141 2 ; B
        37
        150 3 ; C
        109 1 ; A%
        106 3
        7
        106 1
        6
        118 4 ; D%
        109 1 ; A%
        320
        376 2
        40
        150 5 ; E
        109 1 ; A%
        106 2
        8
        109 1 ; A%
        106 3
        20
        5
        118 6 ; F%
        376 2
        109 1 ; A%
        320
        51
        150 7 ; G
        109 1 ; A%
        1
        118 8 ; H%
        141 2 ; B
        33
        150 9 ; I
        109 1 ; A%
        4
        320
        150 10 ; J
        109 1 ; A%
        106 1
        10
        141 2 ; B
        376 3
        46
        54
        265 15 ; S$
        169 "y"
        76
        53
        141 2 ; B
        376 0
        45
        55
        150 11 ; K
        265 15 ; S$
        109 1 ; A%
        312
        84
        141 2 ; B
        310
        84
        264 16 ; T$
        169 "lo"
        169 "hello"
        90
        118 12 ; P%
        141 2 ; B
        334
        118 13 ; Q%
        169 "12"
        317
        150 14 ; R
        220
    """ (body output)

[<Test>]
let ``string slices, character stores and LEN lower to 175, 176, 375 and 270`` () =
    let output = compile (source """
        10 x$ = "abcdef"
        20 c$ = x$(3)
        30 d$ = x$(2 TO 4)
        40 x$(1) = "Z"
        50 n% = LEN(x$) + LEN(c$ & d$)
    """)

    assertLines """
        169 "abcdef"
        264 4 ; X$
        106 3
        268
        175 4 ; X$
        264 1 ; C$
        106 2
        106 4
        175 4 ; X$
        264 2 ; D$
        106 1
        268
        169 "Z"
        176 4 ; X$
        375 4 ; X$
        265 1 ; C$
        265 2 ; D$
        84
        270
        5
        118 3 ; N%
        220
    """ (body output)

[<Test>]
let ``arrays are DIMmed at program start and indexed with typed templates`` () =
    let output = compile (source """
        10 DIM a%(5), m(2,3), n$(4,12)
        20 a%(2) = 9
        30 m(1,2) = a%(2) / 4
        40 n$(3) = "name"
        50 n$(1,2) = "q"
        60 PRINT n$(3), n$(1,2)
    """)

    assertLines """
        106 5
        126 0 1 ; A%
        106 2
        106 3
        158 1 2 ; M
        106 4
        106 12
        190 0 3 ; N$
        106 2
        106 9
        112 1 ; A%
        106 1
        106 2
        106 2
        111 1 ; A%
        320
        376 4
        40
        144 2 ; M
        106 3
        169 "name"
        182 3 ; N$
        106 1
        106 2
        268
        169 "q"
        176 3 ; N$
        106 1
        281
        106 3
        263
        263
        175 3 ; N$
        186
        201
        106 1
        106 2
        268
        175 3 ; N$
        186
        203
        220
    """ (body output)
    assertLines """
        10001: ; first initialisation: windows, arrays, strings
        247 1 0 0 2 7 256 202 256 0 ; window #1
        247 2 0 0 0 7 256 202 0 0 ; window #2
        247 0 0 0 0 4 512 50 0 206 ; window #0
        234 1 1 ; A%
        234 2 2 ; M
        234 2 3 ; N$
        242 10005
    """ (section "10001:" "242 10005" output)

[<Test>]
let ``PRINT lowers items, commas, channels and a kept cursor`` () =
    let output = compile (source """
        10 PRINT
        20 PRINT "a", 1; 2.5
        30 PRINT #2, "x";
        40 PRINT "p",
        50 PRINT #0, "y"
    """)

    assertLines """
        106 1
        281
        203
        106 1
        281
        169 "a"
        186
        201
        106 1
        312
        186
        377 2050 20480 ; 2.5
        310
        186
        203
        106 2
        281
        169 "x"
        186
        106 1
        281
        169 "p"
        186
        201
        263
        281
        169 "y"
        186
        203
        220
    """ (body output)

[<Test>]
let ``INPUT reads into variables and array elements on a channel`` () =
    let output = compile (source """
        10 DIM v(3)
        20 INPUT "name"; n$
        30 INPUT #0, v(1), k%
    """)

    assertLines """
        106 3
        158 0 3 ; V
        106 1
        282
        169 "name"
        186
        180
        264 2 ; N$
        203
        263
        282
        106 1
        148
        144 3 ; V
        116
        118 1 ; K%
        203
        220
    """ (body output)

[<Test>]
let ``FOR loops use the range, instance, NEXT and EXIT protocol`` () =
    let output = compile (source """
        10 FOR i% = 1 TO 10 STEP 2
        20 IF i% = 5 THEN NEXT i%
        30 IF i% = 9 THEN EXIT i%
        40 PRINT i%
        50 END FOR i%
        60 FOR x = 0 TO 1 STEP 0.25 : PRINT x : END FOR x
        70 FOR y = 1 TO 3 : PRINT y : END FOR y
        80 FOR k = 1, 3 TO 5, 9 : PRINT k : END FOR k
    """)

    assertLines """
        106 1
        106 10
        106 2
        357 2001
        26 1 2000 ; I%
        355
        2000: ; FOR I%
        109 1 ; I%
        106 5
        12
        239 2002
        242 2003
        2002: ; END IF
        109 1 ; I%
        106 9
        12
        239 2004
        381 2001
        2004: ; END IF
        106 1
        281
        109 1 ; I%
        312
        186
        203
        2003: ; NEXT I%
        230 1 ; I%
        378 2001
        376 0
        376 1
        377 2047 16384 ; 0.25
        357 2006
        58 3 2005 ; X
        355
        2005: ; FOR X
        106 1
        281
        141 3 ; X
        310
        186
        203
        232 3 ; X
        378 2006
        376 1
        376 3
        357 2008
        360 4 2007 ; Y
        355
        2007: ; FOR Y
        106 1
        281
        141 4 ; Y
        310
        186
        203
        232 4 ; Y
        378 2008
        376 1
        56 2 2009 ; K
        376 3
        376 5
        357 2010
        360 2 2009 ; K
        376 9
        56 2 2009 ; K
        355
        2009: ; FOR K
        106 1
        281
        141 2 ; K
        310
        186
        203
        232 2 ; K
        378 2010
        220
    """ (body output)
    assertLines """
        10005: ; second initialisation: scalars
        101 1 ; I%
        133 2 ; K
        133 3 ; X
        133 4 ; Y
        267
    """ (section "10005:" "267" output)

[<Test>]
let ``REPeat loops jump back with 242 and use 238 for NEXT and 381 for EXIT`` () =
    let output = compile (source """
        10 n% = 0
        20 REPeat loop
        30 n% = n% + 1
        40 IF n% < 3 THEN NEXT loop
        50 IF n% > 5 THEN EXIT loop
        60 END REPeat loop
    """)

    assertLines """
        263
        118 1 ; N%
        2000: ; REPeat
        109 1 ; N%
        106 1
        5
        118 1 ; N%
        109 1 ; N%
        106 3
        15
        239 2002
        238 2000
        2002: ; END IF
        109 1 ; N%
        106 5
        10
        239 2003
        381 2001
        2003: ; END IF
        242 2000
        2001: ; END REPeat
        220
    """ (body output)

[<Test>]
let ``GO TO, GO SUB, RETurn and ON tables branch to line labels`` () =
    let output = compile (source """
        5 n% = 1
        10 GO TO 40
        20 PRINT "skipped"
        40 GO SUB 100
        50 ON n% GO TO 20, 40
        60 ON n% GO SUB 100, 110
        70 STOP
        100 PRINT "a"
        110 RETurn
    """)

    assertLines """
        106 1
        118 1 ; N%
        242 20040
        20020: ; line 20
        106 1
        281
        169 "skipped"
        186
        203
        20040: ; line 40
        236 20100
        109 1 ; N%
        240 2
        254
        243 20020
        243 20040
        255
        109 1 ; N%
        241 2 2000
        254
        243 20100
        243 20110
        255
        2000: ; after ON GO SUB
        220
        20100: ; line 100
        106 1
        281
        169 "a"
        186
        203
        20110: ; line 110
        244
        220
    """ (body output)

[<Test>]
let ``procedures and functions bind formals in reverse and declare locals`` () =
    let output = compile (source """
        10 c = 0 : w$ = "hi"
        30 bump c, w$
        50 PRINT greet$(w$), half(7)
        60 DEFine PROCedure bump(n, s$)
        70 REFERENCE n
        80 LOCal k%, m$, z(2)
        90 n = n + 1 : z(1) = n : k% = LEN(s$) : m$ = s$
        100 END DEFine
        110 DEFine FuNction greet$(who$)
        120 RETurn "Hi " & who$
        130 END DEFine
        140 DEFine FuNction half(v)
        150 RETurn v / 2
        160 END DEFine
    """)

    assertLines """
        999 13 ; vector table
        255
        242 10001
        254
        10000: ; DATA
        251
        255
        10001: ; first initialisation: windows, arrays, strings
        247 1 0 0 2 7 256 202 256 0 ; window #1
        247 2 0 0 0 7 256 202 0 0 ; window #2
        247 0 0 0 0 4 512 50 0 206 ; window #0
        262 2 ; W$
        262 5 ; S$
        262 7 ; M$
        234 1 8 ; Z
        262 10 ; WHO$
        242 10005
        255
        10002: ; program body
        326
        346
        376 0
        150 1 ; C
        169 "hi"
        264 2 ; W$
        246 1 ; C
        265 2 ; W$
        259
        236 65533 ; BUMP
        106 1
        281
        265 2 ; W$
        259
        356 65527 ; GREET$
        260
        186
        201
        376 7
        356 65525 ; HALF
        310
        186
        203
        220
        65533: ; DEFine PROCedure BUMP
        293 5 ; S$
        294 4 ; N
        110 6 ; K%
        106 128
        249 7 ; M$
        106 2
        147 0 8 ; Z
        141 4 ; N
        376 1
        37
        150 4 ; N
        106 1
        141 4 ; N
        144 8 ; Z
        375 5 ; S$
        118 6 ; K%
        265 5 ; S$
        264 7 ; M$
        244
        65527: ; DEFine FuNction GREET$
        293 10 ; WHO$
        169 "Hi "
        265 10 ; WHO$
        84
        259
        244
        244
        65525: ; DEFine FuNction HALF
        142 12 ; V
        150 12 ; V
        141 12 ; V
        376 2
        40
        244
        244
        242 10007
        254
        10006: ; GLOBAL/EXTERNAL linkage list (empty)
        261 -1 "" ; end of list
        255
        10007:
        255
        10005: ; second initialisation: scalars
        146 1 ; C
        146 4 ; N
        114 6 ; K%
        146 12 ; V
        267
        237 10000
        242 10002
        254
        10003: ; resident routine names
        261 -1 "" ; end of list
        243 10003 ; startup trailer
        358 0 ; own module number
        243 10006
        0
    """ (listingLines output)

[<Test>]
let ``DATA uses compressed, plain and float records and RESTORE labels`` () =
    let output = compile (source """
        10 RESTORE 100
        20 READ a, b$, c%
        30 RESTORE
        40 DATA 0, 25670, 25671, -2998, -2999, 1.5, 3.0, 100000
        100 DATA 7, "seven", -1
    """)

    assertLines """
        237 2000
        145
        150 1 ; A
        177
        264 2 ; B$
        113
        118 3 ; C%
        237 10000
        220
    """ (body output)
    assertLines """
        10000: ; DATA
        358 -7096 ; 0
        358 -32766 ; 25670
        107 25671
        358 -4098 ; -2998
        107 -2999
        139 2049 24576 0 ; 1.5
        358 -7099 ; 3
        139 2065 25000 0 ; 100000
        2000: ; DATA line 100
        358 -7103 ; 7
        171 "seven"
        358 -7095 ; -1
        251
    """ (section "10000:" "251" output)

[<Test>]
let ``WHEN ERROR installs a threaded handler with RETRY and CONTINUE`` () =
    let output = compile (source """
        10 WHEN ERROR
        20 PRINT ERNUM, ERLIN
        30 IF ERNUM = -7 THEN RETRY
        40 CONTINUE
        50 END WHEN
        60 OPEN #3, "nofile"
    """)

    assertLines """
        255
        327 2000
        106 1
        281
        383
        312
        186
        201
        384
        312
        186
        203
        383
        106 -7
        12
        239 2001
        255
        325
        2001: ; END IF
        364
        364
        2000: ; END WHEN
        250
        106 3
        169 "nofile"
        227 1 [93 01] ; OPEN
        220
    """ (body output)

[<Test>]
let ``built-ins use their templates and other routines go through the resident list`` () =
    let output = compile (source """
        10 a$ = CHR$(65) & FILL$("-", 3)
        20 c% = CODE(a$)
        30 POKE 131072, 255 : p% = PEEK(131072) : q = PEEK_L(131072)
        40 r = RESPR(100)
        50 e = EOF(#3) : d = PI
        60 x = RND(1 TO 6) : y = SIN(1)
        70 INK #2, 7 : CLS : AT 1, 2
        80 z$ = INKEY$
    """)

    assertLines """
        106 65
        273
        169 "-"
        106 3
        274
        84
        264 1 ; A$
        265 1 ; A$
        269
        118 2 ; C%
        377 2066 16384 ; 131072
        376 255
        278
        377 2066 16384 ; 131072
        271
        118 5 ; P%
        377 2066 16384 ; 131072
        276
        150 6 ; Q
        376 100
        275
        150 7 ; R
        106 3
        222
        150 4 ; E
        138 2050 25735 60753 ; PI
        150 3 ; D
        106 1
        106 6
        224 2 11 [53 03] ; RND
        150 8 ; X
        376 1
        224 2 12 [02] ; SIN
        150 9 ; Y
        250
        106 2
        106 7
        227 13 [93 03] ; INK
        250
        227 14 [] ; CLS
        250
        106 1
        106 2
        227 15 [13 03] ; AT
        224 1 16 [] ; INKEY$
        264 10 ; Z$
        220
    """ (body output)
    assertLines """
        10003: ; resident routine names
        261 11 "RND" ; RND
        261 12 "SIN" ; SIN
        261 13 "INK" ; INK
        261 14 "CLS" ; CLS
        261 15 "AT" ; AT
        261 16 "INKEY$" ; INKEY$
        261 -1 "" ; end of list
        243 10003 ; startup trailer
        358 0 ; own module number
        243 10006
        0
    """ (section "10003:" "0" output)

[<Test>]
let ``MANIFEST constants are folded and IMPLICIT% types storage`` () =
    let output = compile (source """
        10 MANIFEST limit = 10
        20 IMPLICIT% count
        30 count = limit * 2
        40 PRINT count
    """)

    assertLines """
        106 10
        106 2
        7
        118 1 ; COUNT
        106 1
        281
        109 1 ; COUNT
        312
        186
        203
        220
    """ (body output)
    assertLines """
        10005: ; second initialisation: scalars
        114 1 ; COUNT
        267
    """ (section "10005:" "267" output)

[<Test>]
let ``missing arguments get empty values and a whole array passes by REFERENCE`` () =
    let table, routine, arr, value, count, text = SymbolId 0, SymbolId 1, SymbolId 2, SymbolId 3, SymbolId 4, SymbolId 5
    let program =
        { emptyProgram with
            Globals = [ arrayStorage table "T" HirType.Float [ 4 ] ]
            Routines =
                [ procedure routine "FILL"
                    [ parameter arr "FILL" "ARR" (HirType.Array HirType.Float) ReferenceBinding
                      parameter value "FILL" "V" HirType.Float FlexibleBinding
                      parameter count "FILL" "K%" HirType.Int FlexibleBinding
                      parameter text "FILL" "S$" HirType.String FlexibleBinding ]
                    [ Assign(WriteArrayElem(arr, [ literalInt 1 ], HirType.Float, pos), ReadVar(value, HirType.Float, pos), pos) ] ]
            Main = [ ProcCall(routine, None, [ ValueArg(ReadVar(table, HirType.Array HirType.Float, pos)) ], pos) ] }

    let output = generateTurboIrFromHir noWindows program |> expectOk

    assertLines """
        106 4
        158 0 1 ; T
        246 1 ; T
        376 0
        263
        169 ""
        259
        236 65534 ; FILL
        220
        65534: ; DEFine PROCedure FILL
        293 6 ; S$
        110 5 ; K%
        118 5 ; K%
        142 4 ; V
        150 4 ; V
        294 3 ; ARR
        106 1
        141 4 ; V
        144 3 ; ARR
        244
    """ (body output)
    assertLines """
        10001: ; first initialisation: windows, arrays, strings
        234 1 1 ; T
        262 6 ; S$
        242 10005
    """ (section "10001:" "242 10005" output)

[<Test>]
let ``DIMN takes the dimension on the stack and the dimension counter`` () =
    let m, d, dimn = SymbolId 0, SymbolId 1, SymbolId 2
    let program =
        { emptyProgram with
            SymbolNames = Map.ofList [ dimn, "DIMN" ]
            Globals = [ arrayStorage m "M" HirType.Float [ 2; 3 ]; storage d "D" HirType.Float GlobalStorage ]
            Main =
                [ Assign(
                    WriteVar(d, HirType.Float, pos),
                    CallFunc(dimn, [ ValueArg(ReadVar(m, HirType.Array HirType.Float, pos)); ValueArg(literalInt 2) ], HirType.Int, pos),
                    pos) ] }

    let output = generateTurboIrFromHir noWindows program |> expectOk

    assertLines """
        106 2
        106 3
        158 1 1 ; M
        106 2
        266 1 1 ; M
        320
        150 2 ; D
        220
    """ (body output)

[<Test>]
let ``line numbers option emits 248 at each numbered line`` () =
    let text = source """
        10 PRINT 1
        20 GO TO 10
    """
    let withLineNumbers options = { options with LineNumbers = true }
    let output = compileWith withLineNumbers text |> expectOk

    assertLines """
        20010: ; line 10
        248 10
        106 1
        281
        106 1
        312
        186
        203
        248 20
        242 20010
        220
    """ (body output)

[<Test>]
let ``string literals are written in the QL character set`` () =
    let output = compile (source "10 PRINT \"ó£é\"")

    // o-acute is 0x96, the pound sign 0x60 and e-acute 0x83 in the QL character set.
    Assert.That(toHex output.Bytes, Does.Contain("00 A9 00 03 96 60 83 00"))

[<Test>]
let ``warnings name directives, extensions and lost separators`` () =
    let output = compile (source """
        10 TURBO_taskn "demo"
        20 a$ = LEFT$("abc", 2)
        30 LINE 0,0 TO 10,10
    """)

    assertLines """
        169 "abc"
        106 2
        224 1 2 [11 03] ; LEFT$
        264 1 ; A$
        250
        263
        263
        106 10
        106 10
        227 3 [13 13 13 03] ; LINE
        220
    """ (body output)
    Assert.That(output.Warnings.Length, Is.EqualTo(3))
    Assert.That(output.Warnings[0], Does.Contain("TURBO_taskn"))
    Assert.That(output.Warnings[1], Does.Contain("'LEFT$' is not a QL ROM keyword"))
    Assert.That(output.Warnings[2], Does.Contain("LINE: SB's front end turns the TO separators"))

// ---------------------------------------------------------------------------
// Constructs with no IR form are errors
// ---------------------------------------------------------------------------

[<Test>]
let ``RUN is rejected`` () =
    let errors = compileWith id (source "10 RUN") |> expectErrors

    Assert.That(errors, Is.EquivalentTo([ "line 10 (1:3): RUN has no Turbo IR equivalent in the specification." ]))

[<Test>]
let ``characters outside the QL character set are rejected`` () =
    let errors = compileWith id (source "10 PRINT \"☃\"") |> expectErrors

    Assert.That(errors[0], Does.Contain("U+2603"))

[<Test>]
let ``a computed GO TO is rejected`` () =
    let n = SymbolId 0
    let program =
        { emptyProgram with
            Globals = [ storage n "N%" HirType.Int GlobalStorage ]
            Main = [ LineNumber(10, pos); Goto(ReadVar(n, HirType.Int, pos), pos) ] }

    let errors = generateTurboIrFromHir noWindows program |> expectErrors

    Assert.That(errors[0], Does.Contain("GO TO needs a constant line number"))

[<Test>]
let ``a by-reference parameter rejects an expression argument`` () =
    let routine, formal = SymbolId 0, SymbolId 1
    let program =
        { emptyProgram with
            Routines = [ procedure routine "R" [ parameter formal "R" "N" HirType.Float ReferenceBinding ] [] ]
            Main = [ ProcCall(routine, None, [ ValueArg(literalInt 3) ], pos) ] }

    let errors = generateTurboIrFromHir noWindows program |> expectErrors

    Assert.That(errors[0], Does.Contain("by-reference parameter 'N' of 'R' needs a variable or array name"))

[<Test>]
let ``an array passed to a value parameter is rejected`` () =
    let table, routine, formal = SymbolId 0, SymbolId 1, SymbolId 2
    let program =
        { emptyProgram with
            Globals = [ arrayStorage table "T" HirType.Float [ 4 ] ]
            Routines = [ procedure routine "Q" [ parameter formal "Q" "X" HirType.Float FlexibleBinding ] [] ]
            Main = [ ProcCall(routine, None, [ ValueArg(ReadVar(table, HirType.Array HirType.Float, pos)) ], pos) ] }

    let errors = generateTurboIrFromHir noWindows program |> expectErrors

    Assert.That(errors[0], Does.Contain("Turbo passes arrays only by REFERENCE"))

// ---------------------------------------------------------------------------
// A whole program
// ---------------------------------------------------------------------------

[<Test>]
let ``a whole program lowers to the exact stream`` () =
    let output = compile (source """
        100 REMark Scores: read, sort and report a table
        110 TURBO_windo 1
        120 DIM score%(5), name$(5,10)
        130 count% = 0
        140 REPeat read_loop
        150   READ who$, pts%
        160   IF who$ = "end" THEN EXIT read_loop
        170   count% = count% + 1
        180   name$(count%) = who$ : score%(count%) = pts%
        190 END REPeat read_loop
        200 sort count%
        210 CLS
        220 FOR i% = 1 TO count%
        230   PRINT rank$(i%); " "; name$(i%), score%(i%)
        240 END FOR i%
        250 PRINT "Average: "; average(count%)
        260 STOP
        270 DEFine PROCedure sort(n%)
        280   LOCal i%, j%, t%, t$
        290   FOR i% = 1 TO n% - 1
        300     FOR j% = i% + 1 TO n%
        310       IF score%(j%) > score%(i%) THEN
        320         t% = score%(i%) : score%(i%) = score%(j%) : score%(j%) = t%
        330         t$ = name$(i%) : name$(i%) = name$(j%) : name$(j%) = t$
        340       END IF
        350     END FOR j%
        360   END FOR i%
        370 END DEFine
        380 DEFine FuNction rank$(r%)
        390   IF r% = 1 THEN RETurn "1st"
        400   RETurn r% & "th"
        410 END DEFine
        420 DEFine FuNction average(n%)
        430   LOCal k%, total
        440   total = 0
        450   FOR k% = 1 TO n% : total = total + score%(k%) : END FOR k%
        460   RETurn total / n%
        470 END DEFine
        480 DATA "Ann", 72, "Bob", 65, "Cy", 91
        490 DATA "end", 0
    """)

    assertLines """
        999 18 ; vector table
        255
        242 10001
        254
        10000: ; DATA
        171 "Ann"
        358 -7168 ; 72
        171 "Bob"
        358 -7161 ; 65
        171 "Cy"
        358 -7187 ; 91
        171 "end"
        358 -7096 ; 0
        251
        255
        10001: ; first initialisation: windows, arrays, strings
        247 1 0 0 2 7 256 202 256 0 ; window #1
        234 2 3 ; NAME$
        234 1 5 ; SCORE%
        262 6 ; WHO$
        262 10 ; T$
        242 10005
        255
        10002: ; program body
        326
        346
        106 5
        106 10
        190 0 3 ; NAME$
        106 5
        126 0 5 ; SCORE%
        263
        118 1 ; COUNT%
        2000: ; REPeat
        177
        264 6 ; WHO$
        113
        118 4 ; PTS%
        265 6 ; WHO$
        169 "end"
        76
        239 2002
        381 2001
        2002: ; END IF
        109 1 ; COUNT%
        106 1
        5
        118 1 ; COUNT%
        109 1 ; COUNT%
        265 6 ; WHO$
        182 3 ; NAME$
        109 1 ; COUNT%
        109 4 ; PTS%
        112 5 ; SCORE%
        242 2000
        2001: ; END REPeat
        109 1 ; COUNT%
        236 65529 ; SORT
        250
        227 17 [] ; CLS
        106 1
        109 1 ; COUNT%
        106 1
        357 2004
        26 2 2003 ; I%
        355
        2003: ; FOR I%
        106 1
        281
        109 2 ; I%
        356 65524 ; RANK$
        260
        186
        169 " "
        186
        109 2 ; I%
        263
        263
        175 3 ; NAME$
        186
        201
        109 2 ; I%
        111 5 ; SCORE%
        312
        186
        203
        230 2 ; I%
        378 2004
        106 1
        281
        169 "Average: "
        186
        109 1 ; COUNT%
        356 65522 ; AVERAGE
        310
        186
        203
        220
        220
        65529: ; DEFine PROCedure SORT
        110 8 ; N%
        118 8 ; N%
        102 2 ; I%
        102 9 ; J%
        106 128
        249 10 ; T$
        110 11 ; T%
        106 1
        109 8 ; N%
        106 1
        6
        106 1
        357 2006
        26 2 2005 ; I%
        355
        2005: ; FOR I%
        109 2 ; I%
        106 1
        5
        109 8 ; N%
        106 1
        357 2008
        26 9 2007 ; J%
        355
        2007: ; FOR J%
        109 9 ; J%
        111 5 ; SCORE%
        109 2 ; I%
        111 5 ; SCORE%
        10
        239 2009
        109 2 ; I%
        111 5 ; SCORE%
        118 11 ; T%
        109 2 ; I%
        109 9 ; J%
        111 5 ; SCORE%
        112 5 ; SCORE%
        109 9 ; J%
        109 11 ; T%
        112 5 ; SCORE%
        109 2 ; I%
        263
        263
        175 3 ; NAME$
        264 10 ; T$
        109 2 ; I%
        109 9 ; J%
        263
        263
        175 3 ; NAME$
        182 3 ; NAME$
        109 9 ; J%
        265 10 ; T$
        182 3 ; NAME$
        2009: ; END IF
        230 9 ; J%
        378 2008
        230 2 ; I%
        378 2006
        244
        65524: ; DEFine FuNction RANK$
        110 13 ; R%
        118 13 ; R%
        109 13 ; R%
        106 1
        12
        239 2010
        169 "1st"
        259
        244
        2010: ; END IF
        109 13 ; R%
        312
        169 "th"
        84
        259
        244
        244
        65522: ; DEFine FuNction AVERAGE
        110 8 ; N%
        118 8 ; N%
        102 15 ; K%
        142 16 ; TOTAL
        376 0
        150 16 ; TOTAL
        106 1
        109 8 ; N%
        106 1
        357 2012
        26 15 2011 ; K%
        355
        2011: ; FOR K%
        141 16 ; TOTAL
        109 15 ; K%
        111 5 ; SCORE%
        320
        37
        150 16 ; TOTAL
        230 15 ; K%
        378 2012
        141 16 ; TOTAL
        109 8 ; N%
        320
        40
        244
        244
        242 10007
        254
        10006: ; GLOBAL/EXTERNAL linkage list (empty)
        261 -1 "" ; end of list
        255
        10007:
        255
        10005: ; second initialisation: scalars
        114 1 ; COUNT%
        101 2 ; I%
        114 4 ; PTS%
        114 8 ; N%
        114 9 ; J%
        114 11 ; T%
        114 13 ; R%
        114 15 ; K%
        146 16 ; TOTAL
        267
        237 10000
        242 10002
        254
        10003: ; resident routine names
        261 17 "CLS" ; CLS
        261 -1 "" ; end of list
        243 10003 ; startup trailer
        358 0 ; own module number
        243 10006
        0
    """ (listingLines output)
    Assert.That(output.Warnings, Is.Empty)
    Assert.That(output.Bytes.Length % 2, Is.EqualTo(0))

// ---------------------------------------------------------------------------
// Encodings, decoding and validation
// ---------------------------------------------------------------------------

[<Test>]
let ``QDOS floats match the specification's examples`` () =
    let encode value =
        match encodeQdosFloat value with
        | Result.Ok(exponent, high, low) -> sprintf "%04X %04X %04X" exponent high low
        | Result.Error message -> message

    Assert.That(encode 1.0, Is.EqualTo("0801 4000 0000"))
    Assert.That(encode 2.0, Is.EqualTo("0802 4000 0000"))
    Assert.That(encode Math.PI, Is.EqualTo("0802 6487 ED51"))
    Assert.That(encode 0.0, Is.EqualTo("0000 0000 0000"))
    Assert.That(encode 0.5, Is.EqualTo("0800 4000 0000"))
    // Negative powers of two normalize to 0x80000000 with the exponent one lower.
    Assert.That(encode -1.0, Is.EqualTo("0800 8000 0000"))
    Assert.That(encode -3.0, Is.EqualTo("0802 A000 0000"))
    Assert.That(encode 100000.0, Is.EqualTo("0811 61A8 0000"))

[<Test>]
let ``validate reports undefined labels, wrong modes and a missing trailer`` () =
    let bytes =
        serialize
            [ Instruction(999, [ Word 2 ], "")
              Instruction(254, [], "")
              Instruction(114, [ Word 1 ], "")
              Instruction(242, [ LabelRef 3000 ], "")
              Instruction(0, [], "") ]

    let problems = validate bytes

    Assert.That(problems, Has.Some.Contains("Opcode 114 has only a threaded implementation"))
    Assert.That(problems, Has.Some.Contains("Label 3000 is referenced"))
    Assert.That(problems, Has.Some.Contains("startup trailer"))

[<Test>]
let ``decode rejects a stream that does not start with 999`` () =
    match decode (hexBytes "00 FF 00 00") with
    | Result.Error message -> Assert.That(message, Does.Contain("999"))
    | Result.Ok _ -> Assert.Fail("Expected the decode to fail.")

// ---------------------------------------------------------------------------
// Command-line selection
// ---------------------------------------------------------------------------

let private exampleSource = source """
    10 TURBO_windo 0
    20 n% = 42 + 1
"""

let private withTempDirectory action =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}")
    Directory.CreateDirectory(path) |> ignore
    try
        action path
    finally
        if Directory.Exists(path) then
            Directory.Delete(path, true)

[<Test>]
let ``an .ir output selects the Turbo IR backend and writes a listing`` () =
    withTempDirectory (fun dir ->
        let sourcePath = Path.Combine(dir, "example.bas")
        let outputPath = Path.Combine(dir, "example.ir")
        File.WriteAllText(sourcePath, exampleSource)

        let exitCode = Program.main [| sourcePath; outputPath |]

        Assert.That(exitCode, Is.EqualTo(0))
        Assert.That(toHex (File.ReadAllBytes outputPath), Is.EqualTo(toHex (hexBytes specExampleHex)))
        Assert.That(File.ReadAllText(listingFileName outputPath), Does.StartWith("999 2 ; vector table")))

[<Test>]
let ``the turboir backend name selects the Turbo IR backend`` () =
    withTempDirectory (fun dir ->
        let sourcePath = Path.Combine(dir, "example.bas")
        let outputPath = Path.Combine(dir, "example_temp")
        File.WriteAllText(sourcePath, exampleSource)

        let exitCode = Program.main [| sourcePath; outputPath; "false"; "turboir" |]

        Assert.That(exitCode, Is.EqualTo(0))
        Assert.That(toHex (File.ReadAllBytes outputPath), Is.EqualTo(toHex (hexBytes specExampleHex))))

[<Test>]
let ``a program the backend rejects fails the command`` () =
    withTempDirectory (fun dir ->
        let sourcePath = Path.Combine(dir, "run.bas")
        let outputPath = Path.Combine(dir, "run.ir")
        File.WriteAllText(sourcePath, source "10 RUN")

        let exitCode = Program.main [| sourcePath; outputPath |]

        Assert.That(exitCode, Is.EqualTo(1))
        Assert.That(File.Exists outputPath, Is.False))
