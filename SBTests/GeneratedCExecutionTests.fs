module SBTests.GeneratedCExecutionTests

open System
open System.Diagnostics
open System.IO

open NUnit.Framework

open Program
open Types
open HIR
open HirCBackend

let private pos =
    { BasicLineNo = None
      EditorLineNo = 1
      Column = 0 }

let private literalInt value = Literal(ConstInt value, HirType.Int, pos)
let private literalFloat value = Literal(ConstFloat value, HirType.Float, pos)
let private literalString value = Literal(ConstString value, HirType.String, pos)

let private storage symbol name hirType storageClass =
    { Symbol = symbol
      Slot = StorageSlotId 0
      Name = name
      Type = hirType
      Dimensions = None
      Class = storageClass
      Position = pos }

let private parameter symbol name hirType storageClass binding =
    { Storage = storage symbol name hirType storageClass
      Binding = binding }

let private withTempDirectory action =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}")
    Directory.CreateDirectory(path) |> ignore
    try
        action path
    finally
        if Directory.Exists(path) then
            Directory.Delete(path, true)

let private runProcess (workingDirectory: string) (fileName: string) (arguments: string) (standardInput: string option) =
    let psi = ProcessStartInfo()
    psi.WorkingDirectory <- workingDirectory
    psi.FileName <- fileName
    psi.Arguments <- arguments
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.RedirectStandardInput <- true
    use proc = new Process()
    proc.StartInfo <- psi

    if not (proc.Start()) then
        failwith $"Failed to start process '{fileName}'."

    match standardInput with
    | Some text ->
        proc.StandardInput.Write(text)
        proc.StandardInput.Close()
    | None ->
        proc.StandardInput.Close()

    let stdout = proc.StandardOutput.ReadToEnd()
    let stderr = proc.StandardError.ReadToEnd()
    proc.WaitForExit()
    proc.ExitCode, stdout, stderr

let private ensureGccAvailable () =
    try
        let exitCode, _, _ = runProcess __SOURCE_DIRECTORY__ "gcc" "--version" None
        if exitCode <> 0 then
            Assert.Ignore("gcc is not available for generated C execution tests.")
    with _ ->
        Assert.Ignore("gcc is not available for generated C execution tests.")

let private writeCportShim directory =
    let header =
        """
#ifndef CPORT_H
#define CPORT_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

typedef int colour_t;
typedef int chanid_t;
typedef struct SBChannel { FILE* fp; chanid_t chanid; int iscon; } SBChannel;

extern SBChannel sb_channo[256];

#define FNO(id) (sb_channo[(id)].fp)
#define NO_TIMEOUT (-1)

FILE* fusechid(chanid_t chid);
chanid_t fgetchid(FILE* fp);

void CP_Initialise(void);
int _SB_Open(SBChannel* channel, char* path, char* mode);
void _SB_Close(SBChannel* channel);
int _SB_Window(FILE* writer, short w, short h, short x, short y);
int _SB_At(FILE* writer, short line, short column);
int _SB_Ink(FILE* writer, colour_t colour);
int _SB_Paper(FILE* writer, colour_t colour);
int _SB_Border(FILE* writer, short width, colour_t colour);
int _SB_Block(FILE* writer, short w, short h, short x, short y, colour_t colour);
int _SB_Csize(FILE* writer, short h, short w);
int _SB_Recol(FILE* writer, colour_t c0, colour_t c1, colour_t c2, colour_t c3, colour_t c4, colour_t c5, colour_t c6, colour_t c7);
int _SB_Scroll(FILE* writer, short val, short part);
void _SB_Randomise(double seed);
double SB_Rnd_f(void);
int SB_Rnd_i(int lower, int upper);
int SB_Date(void);
void SB_Mode(int mode);
void SB_Dir(FILE* writer, char* target);
void SB_Cls(FILE* writer, int mode);
void SB_Line(FILE* writer, int x1, int y1, int x2, int y2);
void SB_Circle(FILE* writer, int x, int y, int radius);
void SB_Strip(FILE* writer, int strip);
void SB_Over(FILE* writer, int over);
void SB_Under(FILE* writer, int under);
void SB_Flash(FILE* writer, int flash);
int SB_Delete(char* name);
int SB_Rename(char* oldName, char* newName);
int io_trunc(chanid_t chid, int timeout);
int sd_pixp(chanid_t chid, int timeout, int x, int y);
int sd_gcur(chanid_t chid, int timeout, double xo, double yo, double x, double y);
int sd_fount(chanid_t chid, int timeout, char* font1, char* font2);
int sd_iscale(chanid_t chid, int timeout, int scale, int x, int y);
int sd_scale(chanid_t chid, int timeout, double scale, double x, double y);
int sd_iarc(chanid_t chid, int timeout, int x1, int y1, int x2, int y2, int angle);
int sd_arc(chanid_t chid, int timeout, double x1, double y1, double x2, double y2, double angle);
int sd_elipse(chanid_t chid, int timeout, double x, double y, double ecc, double radius, double angle);
int sd_ielipse(chanid_t chid, int timeout, int x, int y, int ecc, int radius, int angle);
int SB_Peek(int address);
int SB_Peek_W(int address);
int SB_Peek_L(int address);
void SB_Poke(int address, int value);
void SB_Poke_W(int address, int value);
void SB_Poke_L(int address, int value);

#define SB_Csize(fp,h,w) _SB_Csize(fp,(short)(h),(short)(w))
#define SB_Cursor(fp,x,y) sd_pixp(fgetchid(fp),NO_TIMEOUT,(int)(x),(int)(y))
#define SB_Cursor_gf(fp,x_origin,y_origin,x,y) sd_gcur(fgetchid(fp),NO_TIMEOUT,(double)(x_origin),(double)(y_origin),(double)(x),(double)(y))
#define SB_Recol(fp,c0,c1,c2,c3,c4,c5,c6,c7) _SB_Recol(fp,(int)(c0),(int)(c1),(int)(c2),(int)(c3),(int)(c4),(int)(c5),(int)(c6),(int)(c7))
#define SB_Scale_f(fp,s,x,y) sd_scale(fgetchid(fp),NO_TIMEOUT,(double)(s),(double)(x),(double)(y))
#define SB_Arc_f(fp,x1,y1,x2,y2,a) sd_arc(fgetchid(fp),NO_TIMEOUT,(double)(x1),(double)(y1),(double)(x2),(double)(y2),(double)(a))
#define SB_Elipse_f(fp,xp,yp,rd,ec,ang) sd_elipse(fgetchid(fp),NO_TIMEOUT,(double)(xp),(double)(yp),(double)(ec),(double)(rd),(double)(ang))
#define SB_Scroll(fp,val,part) _SB_Scroll(fp,(int)(val),(int)(part))
#define SB_Set_Font(fp,ad1,ad2) sd_fount(fgetchid(fp),NO_TIMEOUT,(char *)(ad1),(char *)(ad2))
#define SB_Truncate(fp) io_trunc(fgetchid(fp),NO_TIMEOUT)

#endif
"""

    let source =
        """
#include "cport.h"

SBChannel sb_channo[256];

static unsigned int sb_seed = 1u;

void CP_Initialise(void)
{
    int i;
    for (i = 0; i < 256; i++) { sb_channo[i].fp = NULL; sb_channo[i].chanid = i; sb_channo[i].iscon = 0; }
    sb_channo[0].fp = stdin;
    sb_channo[1].fp = stdout;
    sb_channo[0].iscon = 1;
    sb_channo[1].iscon = 1;
}

FILE* fusechid(chanid_t chid)
{
    if (chid < 0 || chid >= 256) return NULL;
    return sb_channo[chid].fp;
}

chanid_t fgetchid(FILE* fp)
{
    int i;
    for (i = 0; i < 256; i++)
    {
        if (sb_channo[i].fp == fp) return sb_channo[i].chanid;
    }
    return 0;
}

int _SB_Open(SBChannel* channel, char* path, char* mode)
{
    const char* actual_mode = mode;
    if (mode && strcmp(mode, "n+") == 0) actual_mode = "w+";
    channel->fp = fopen(path, actual_mode);
    return channel->fp ? 0 : -1;
}

void _SB_Close(SBChannel* channel)
{
    if (channel && channel->fp && channel->fp != stdin && channel->fp != stdout && channel->fp != stderr)
    {
        fclose(channel->fp);
    }
    if (channel) channel->fp = NULL;
}

int _SB_Window(FILE* writer, short w, short h, short x, short y) { (void)writer; (void)w; (void)h; (void)x; (void)y; return 0; }
int _SB_At(FILE* writer, short line, short column) { (void)writer; (void)line; (void)column; return 0; }
int _SB_Ink(FILE* writer, colour_t colour) { (void)writer; (void)colour; return 0; }
int _SB_Paper(FILE* writer, colour_t colour) { (void)writer; (void)colour; return 0; }
int _SB_Border(FILE* writer, short width, colour_t colour) { (void)writer; (void)width; (void)colour; return 0; }
int _SB_Block(FILE* writer, short w, short h, short x, short y, colour_t colour) { (void)writer; (void)w; (void)h; (void)x; (void)y; (void)colour; return 0; }
int _SB_Csize(FILE* writer, short h, short w) { (void)writer; (void)h; (void)w; return 0; }
int _SB_Recol(FILE* writer, colour_t c0, colour_t c1, colour_t c2, colour_t c3, colour_t c4, colour_t c5, colour_t c6, colour_t c7) { (void)writer; (void)c0; (void)c1; (void)c2; (void)c3; (void)c4; (void)c5; (void)c6; (void)c7; return 0; }
int _SB_Scroll(FILE* writer, short val, short part) { (void)writer; (void)val; (void)part; return 0; }

void _SB_Randomise(double seed)
{
    sb_seed = (unsigned int)seed;
    srand(sb_seed);
}

double SB_Rnd_f(void)
{
    return (double)rand() / ((double)RAND_MAX + 1.0);
}

int SB_Rnd_i(int lower, int upper)
{
    int minimum = lower < upper ? lower : upper;
    int maximum = lower < upper ? upper : lower;
    int span = maximum - minimum + 1;
    if (span <= 0) return minimum;
    return minimum + (rand() % span);
}

int SB_Date(void)
{
    return 123456;
}

void SB_Mode(int mode) { (void)mode; }
void SB_Dir(FILE* writer, char* target) { fprintf(writer ? writer : stdout, "%s\n", target ? target : "*"); }
void SB_Cls(FILE* writer, int mode) { (void)writer; (void)mode; }
void SB_Line(FILE* writer, int x1, int y1, int x2, int y2) { (void)writer; (void)x1; (void)y1; (void)x2; (void)y2; }
void SB_Circle(FILE* writer, int x, int y, int radius) { (void)writer; (void)x; (void)y; (void)radius; }
void SB_Strip(FILE* writer, int strip) { (void)writer; (void)strip; }
void SB_Over(FILE* writer, int over) { (void)writer; (void)over; }
void SB_Under(FILE* writer, int under) { (void)writer; (void)under; }
void SB_Flash(FILE* writer, int flash) { (void)writer; (void)flash; }
int SB_Delete(char* name) { return remove(name); }
int SB_Rename(char* oldName, char* newName) { return rename(oldName, newName); }
int io_trunc(chanid_t chid, int timeout)
{
    (void)timeout;
    return fusechid(chid) ? 0 : -1;
}
int sd_pixp(chanid_t chid, int timeout, int x, int y) { (void)chid; (void)timeout; (void)x; (void)y; return 0; }
int sd_gcur(chanid_t chid, int timeout, double xo, double yo, double x, double y) { (void)chid; (void)timeout; (void)xo; (void)yo; (void)x; (void)y; return 0; }
int sd_fount(chanid_t chid, int timeout, char* font1, char* font2) { (void)chid; (void)timeout; (void)font1; (void)font2; return 0; }
int sd_iscale(chanid_t chid, int timeout, int scale, int x, int y) { (void)chid; (void)timeout; (void)scale; (void)x; (void)y; return 0; }
int sd_scale(chanid_t chid, int timeout, double scale, double x, double y) { (void)chid; (void)timeout; (void)scale; (void)x; (void)y; return 0; }
int sd_iarc(chanid_t chid, int timeout, int x1, int y1, int x2, int y2, int angle) { (void)chid; (void)timeout; (void)x1; (void)y1; (void)x2; (void)y2; (void)angle; return 0; }
int sd_arc(chanid_t chid, int timeout, double x1, double y1, double x2, double y2, double angle) { (void)chid; (void)timeout; (void)x1; (void)y1; (void)x2; (void)y2; (void)angle; return 0; }
int sd_elipse(chanid_t chid, int timeout, double x, double y, double ecc, double radius, double angle) { (void)chid; (void)timeout; (void)x; (void)y; (void)ecc; (void)radius; (void)angle; return 0; }
int sd_ielipse(chanid_t chid, int timeout, int x, int y, int ecc, int radius, int angle) { (void)chid; (void)timeout; (void)x; (void)y; (void)ecc; (void)radius; (void)angle; return 0; }

int SB_Peek(int address) { (void)address; return 0; }
int SB_Peek_W(int address) { (void)address; return 0; }
int SB_Peek_L(int address) { (void)address; return 0; }
void SB_Poke(int address, int value) { (void)address; (void)value; }
void SB_Poke_W(int address, int value) { (void)address; (void)value; }
void SB_Poke_L(int address, int value) { (void)address; (void)value; }
"""

    File.WriteAllText(Path.Combine(directory, "cport.h"), header.Replace("\n", Environment.NewLine))
    File.WriteAllText(Path.Combine(directory, "cport.c"), source.Replace("\n", Environment.NewLine))

let private compileAndRunGeneratedC (directory: string) (stdinText: string option) =
    let exePath = Path.Combine(directory, "program.exe")

    writeCportShim directory

    let compileArgs =
        String.concat " "
            [ "-std=c99"
              "-I."
              "-o"
              "\"program.exe\""
              "\"program.c\""
              "\"sbruntime_c.c\""
              "\"cport.c\""
              "-lm" ]

    let compileExitCode, compileStdout, compileStderr = runProcess directory "gcc" compileArgs None
    Assert.That(
        compileExitCode,
        Is.EqualTo(0),
        $"gcc failed.{Environment.NewLine}stdout:{Environment.NewLine}{compileStdout}{Environment.NewLine}stderr:{Environment.NewLine}{compileStderr}")
    Assert.That(File.Exists(exePath), Is.True)

    let runExitCode, stdout, stderr = runProcess directory exePath "" stdinText
    runExitCode, stdout.Replace("\r\n", "\n"), stderr.Replace("\r\n", "\n")

let private generateCompileAndRun (source: string) (stdinText: string option) =
    ensureGccAvailable ()

    withTempDirectory (fun dir ->
        let sourcePath = Path.Combine(dir, "program.sb")
        let generatedCPath = Path.Combine(dir, "program.c")

        File.WriteAllText(sourcePath, source.Replace("\n", Environment.NewLine))

        let generateExitCode = Program.main [| sourcePath; generatedCPath; "false"; "c" |]
        Assert.That(generateExitCode, Is.EqualTo(0), "C generation failed.")
        Assert.That(File.Exists(generatedCPath), Is.True)
        Assert.That(File.Exists(Path.Combine(dir, "sbruntime_c.c")), Is.True)
        Assert.That(File.Exists(Path.Combine(dir, "sbruntime_c.h")), Is.True)

        compileAndRunGeneratedC dir stdinText)

let private generateHirCompileAndRun programName (program: HirProgram) (stdinText: string option) =
    ensureGccAvailable ()

    withTempDirectory (fun dir ->
        let generatedCPath = Path.Combine(dir, "program.c")
        let runtimeHeaderPath = Path.Combine(dir, "sbruntime_c.h")
        let runtimeSourcePath = Path.Combine(dir, "sbruntime_c.c")

        File.WriteAllText(generatedCPath, generateCFromHir programName program)
        File.Copy(Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", "CRuntime", "sbruntime_c.h"), runtimeHeaderPath)
        File.Copy(Path.Combine(__SOURCE_DIRECTORY__, "..", "SB", "CRuntime", "sbruntime_c.c"), runtimeSourcePath)

        compileAndRunGeneratedC dir stdinText)

[<Test>]
let ``generated c executable runs simple print program`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun "10 PRINT 1+2\n20 STOP\n" None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("3"))

[<Test>]
let ``generated c executable preserves dimn semantics`` () =
    let arraySymbol = SymbolId 0
    let dimnSymbol = SymbolId 1
    let firstSymbol = SymbolId 2
    let secondSymbol = SymbolId 3

    let program =
        { SymbolNames = [ arraySymbol, "A"; dimnSymbol, "DIMN"; firstSymbol, "FIRST"; secondSymbol, "SECOND" ] |> Map.ofList
          Globals =
            [ { storage arraySymbol "A" (HirType.Array HirType.Int) GlobalStorage with Dimensions = Some [ 10; 17 ] }
              storage firstSymbol "FIRST" HirType.Int GlobalStorage
              storage secondSymbol "SECOND" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(
                WriteVar(firstSymbol, HirType.Int, pos),
                CallFunc(dimnSymbol, [ ValueArg(ReadVar(arraySymbol, HirType.Array HirType.Int, pos)); ValueArg(literalInt 1) ], HirType.Int, pos),
                pos)
              Assign(
                WriteVar(secondSymbol, HirType.Int, pos),
                CallFunc(dimnSymbol, [ ValueArg(ReadVar(arraySymbol, HirType.Array HirType.Int, pos)); ValueArg(literalInt 2) ], HirType.Int, pos),
                pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.Int, pos); ReadVar(secondSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "dimn_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("10 17"))

[<Test>]
let ``generated c executable supports ranged rnd`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun "10 RANDOMISE 123\n20 a=RND(2 TO 4)\n30 PRINT a\n40 STOP\n" None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    let value = int (stdout.Trim())
    Assert.That(value, Is.InRange(2, 4))

[<Test>]
let ``generated c executable supports dynamic goto and gosub`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun
            "10 t=40\n20 GOTO t\n30 PRINT \"BAD\"\n40 s=100\n50 GOSUB s\n60 PRINT \"DONE\"\n70 STOP\n100 PRINT \"SUB\"\n110 RETURN\n"
            None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("SUB\nDONE".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated c executable supports data read and restore`` () =
    let firstSymbol = SymbolId 0
    let textSymbol = SymbolId 1
    let thirdSymbol = SymbolId 2
    let fourthSymbol = SymbolId 3

    let program =
        { SymbolNames = [ firstSymbol, "A"; textSymbol, "B$"; thirdSymbol, "C"; fourthSymbol, "D" ] |> Map.ofList
          Globals =
            [ storage firstSymbol "A" HirType.Int GlobalStorage
              storage textSymbol "B$" HirType.String GlobalStorage
              storage thirdSymbol "C" HirType.Int GlobalStorage
              storage fourthSymbol "D" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries =
            [ { Slot = DataSlotId 0; Value = Literal(ConstInt 4, HirType.Int, pos); Position = pos; LineNumber = Some 10 }
              { Slot = DataSlotId 1; Value = Literal(ConstString "HELLO", HirType.String, pos); Position = pos; LineNumber = Some 10 }
              { Slot = DataSlotId 2; Value = Literal(ConstInt 9, HirType.Int, pos); Position = pos; LineNumber = Some 10 } ]
          RestorePoints = [ { LineNumber = 10; Slot = DataSlotId 0 } ]
          Main =
            [ Read([ WriteVar(firstSymbol, HirType.Int, pos); WriteVar(textSymbol, HirType.String, pos); WriteVar(thirdSymbol, HirType.Int, pos) ], pos)
              Restore(Some(literalInt 10), pos)
              Read([ WriteVar(fourthSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.Int, pos); ReadVar(textSymbol, HirType.String, pos); ReadVar(thirdSymbol, HirType.Int, pos); ReadVar(fourthSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "data_restore_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("4 HELLO 9 4"))

[<Test>]
let ``generated c executable supports file channel write and read`` () =
    let textSymbol = SymbolId 0
    let channel9 = ExplicitChannel(literalInt 9)

    let program =
        { SymbolNames = [ textSymbol, "A$" ] |> Map.ofList
          Globals = [ storage textSymbol "A$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ Literal(ConstString "sample.txt", HirType.String, pos) ], pos)
              BuiltInCall(Print, Some channel9, [ Literal(ConstString "HELLO", HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ Literal(ConstString "sample.txt", HirType.String, pos) ], pos)
              Input(Some channel9, [], [ WriteVar(textSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(textSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "file_io_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("HELLO"))

[<Test>]
let ``generated c executable supports eof on channels`` () =
    let beforeSymbol = SymbolId 0
    let afterSymbol = SymbolId 1
    let textSymbol = SymbolId 99
    let channel9 = ExplicitChannel(literalInt 9)
    let eofBuiltin = SymbolId 2

    let program =
        { SymbolNames = [ beforeSymbol, "BEFORE"; afterSymbol, "AFTER"; eofBuiltin, "EOF"; textSymbol, "TEXT$" ] |> Map.ofList
          Globals =
            [ storage beforeSymbol "BEFORE" HirType.Int GlobalStorage
              storage afterSymbol "AFTER" HirType.Int GlobalStorage
              storage textSymbol "TEXT$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ Literal(ConstString "eof.txt", HirType.String, pos) ], pos)
              BuiltInCall(Print, Some channel9, [ Literal(ConstString "HELLO", HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ Literal(ConstString "eof.txt", HirType.String, pos) ], pos)
              Assign(WriteVar(beforeSymbol, HirType.Int, pos), CallFunc(eofBuiltin, [ ValueArg(literalInt 9) ], HirType.Int, pos), pos)
              Input(Some channel9, [], [ WriteVar(textSymbol, HirType.String, pos) ], pos)
              Assign(WriteVar(afterSymbol, HirType.Int, pos), CallFunc(eofBuiltin, [ ValueArg(literalInt 9) ], HirType.Int, pos), pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(beforeSymbol, HirType.Int, pos); ReadVar(afterSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "eof_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("0 1"))

[<Test>]
let ``generated c executable supports append mode files`` () =
    let firstSymbol = SymbolId 0
    let secondSymbol = SymbolId 1
    let channel9 = ExplicitChannel(literalInt 9)

    let program =
        { SymbolNames = [ firstSymbol, "FIRST$"; secondSymbol, "SECOND$" ] |> Map.ofList
          Globals =
            [ storage firstSymbol "FIRST$" HirType.String GlobalStorage
              storage secondSymbol "SECOND$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ Literal(ConstString "append.txt", HirType.String, pos) ], pos)
              BuiltInCall(Print, Some channel9, [ Literal(ConstString "FIRST", HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "APPEND", Some channel9, [ Literal(ConstString "append.txt", HirType.String, pos) ], pos)
              BuiltInCall(Print, Some channel9, [ Literal(ConstString "SECOND", HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ Literal(ConstString "append.txt", HirType.String, pos) ], pos)
              Input(Some channel9, [], [ WriteVar(firstSymbol, HirType.String, pos) ], pos)
              Input(Some channel9, [], [ WriteVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.String, pos); ReadVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "append_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("FIRST SECOND"))

[<Test>]
let ``generated c executable supports sequence for loops`` () =
    let counterSymbol = SymbolId 0

    let program =
        { SymbolNames = [ counterSymbol, "COUNTER" ] |> Map.ofList
          Globals = [ storage counterSymbol "COUNTER" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ ForSequence(
                LoopId 0,
                counterSymbol,
                [ literalInt 1; literalInt 2 ],
                literalInt 5,
                literalInt 6,
                [ literalInt 9 ],
                literalInt 1,
                [ BuiltInCall(Print, None, [ ReadVar(counterSymbol, HirType.Int, pos) ], pos)
                  Next(LoopId 0, pos) ],
                pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "sequence_loop_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("1\n2\n5\n6\n9".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated c executable supports when error continue`` () =
    let errorFlagSymbol = SymbolId 0
    let errNiSymbol = SymbolId 1

    let program =
        { SymbolNames = [ errorFlagSymbol, "FLAG"; errNiSymbol, "ERR_NI" ] |> Map.ofList
          Globals = [ storage errorFlagSymbol "FLAG" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ WhenError(
                [ Assign(WriteVar(errorFlagSymbol, HirType.Int, pos), CallFunc(errNiSymbol, [], HirType.Int, pos), pos)
                  BuiltInCall(Print, None, [ ReadVar(errorFlagSymbol, HirType.Int, pos) ], pos)
                  BuiltInCall(NamedBuiltIn "CONTINUE", None, [], pos) ],
                pos)
              BuiltInCall(NamedBuiltIn "DELETE", None, [], pos)
              BuiltInCall(Print, None, [ literalString "DONE" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "when_error_continue_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("1\nDONE".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated c executable restores to first point at or after target line`` () =
    let valueSymbol = SymbolId 0

    let program =
        { SymbolNames = [ valueSymbol, "VALUE" ] |> Map.ofList
          Globals = [ storage valueSymbol "VALUE" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries =
            [ { Slot = DataSlotId 0; Value = literalInt 1; Position = pos; LineNumber = Some 10 }
              { Slot = DataSlotId 1; Value = literalInt 2; Position = pos; LineNumber = Some 30 } ]
          RestorePoints =
            [ { LineNumber = 10; Slot = DataSlotId 0 }
              { LineNumber = 30; Slot = DataSlotId 1 } ]
          Main =
            [ Restore(Some(literalInt 20), pos)
              Read([ WriteVar(valueSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(Print, None, [ ReadVar(valueSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "restore_ge_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("2"))

[<Test>]
let ``generated c executable supports on goto and on gosub`` () =
    let selectorSymbol = SymbolId 0

    let program =
        { SymbolNames = [ selectorSymbol, "SELECTOR" ] |> Map.ofList
          Globals = [ storage selectorSymbol "SELECTOR" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(selectorSymbol, HirType.Int, pos), literalInt 2, pos)
              OnGoto(ReadVar(selectorSymbol, HirType.Int, pos), [ literalInt 10; literalInt 20 ], pos)
              LineNumber(10, pos)
              BuiltInCall(Print, None, [ literalString "BAD" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos)
              LineNumber(20, pos)
              OnGosub(ReadVar(selectorSymbol, HirType.Int, pos), [ literalInt 100; literalInt 200 ], pos)
              BuiltInCall(Print, None, [ literalString "DONE" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos)
              LineNumber(100, pos)
              BuiltInCall(Print, None, [ literalString "SUB1" ], pos)
              Return(None, pos)
              LineNumber(200, pos)
              BuiltInCall(Print, None, [ literalString "SUB2" ], pos)
              Return(None, pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "on_goto_gosub_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("SUB2\nDONE".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated c executable supports when error continue to line target`` () =
    let flagSymbol = SymbolId 0

    let program =
        { SymbolNames = [ flagSymbol, "FLAG" ] |> Map.ofList
          Globals = [ storage flagSymbol "FLAG" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ WhenError(
                [ BuiltInCall(Print, None, [ literalString "ERR" ], pos)
                  BuiltInCall(NamedBuiltIn "CONTINUE", None, [ literalInt 40 ], pos) ],
                pos)
              BuiltInCall(NamedBuiltIn "DELETE", None, [ literalString "missing.file" ], pos)
              LineNumber(30, pos)
              BuiltInCall(Print, None, [ literalString "BAD" ], pos)
              LineNumber(40, pos)
              BuiltInCall(Print, None, [ literalString "DONE" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "when_error_line_target_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("ERR\nDONE".Replace("\n", Environment.NewLine).Replace("\r\n", "\n").Trim()))

[<Test>]
let ``generated c executable uses strict input coercion and preserves empty fields`` () =
    let intSymbol = SymbolId 0
    let textSymbol = SymbolId 1
    let floatSymbol = SymbolId 2
    let finalIntSymbol = SymbolId 3

    let program =
        { SymbolNames = [ intSymbol, "A"; textSymbol, "B$"; floatSymbol, "C"; finalIntSymbol, "D" ] |> Map.ofList
          Globals =
            [ storage intSymbol "A" HirType.Int GlobalStorage
              storage textSymbol "B$" HirType.String GlobalStorage
              storage floatSymbol "C" HirType.Float GlobalStorage
              storage finalIntSymbol "D" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Input(None, [], [ WriteVar(intSymbol, HirType.Int, pos)
                                WriteVar(textSymbol, HirType.String, pos)
                                WriteVar(floatSymbol, HirType.Float, pos)
                                WriteVar(finalIntSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(Print, None, [ ReadVar(intSymbol, HirType.Int, pos)
                                         ReadVar(textSymbol, HirType.String, pos)
                                         ReadVar(floatSymbol, HirType.Float, pos)
                                         ReadVar(finalIntSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "input_coercion_runtime_program" program (Some "12x,  hello  , 3.5x,  \n")

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("0 hello 0 0"))

[<Test>]
let ``generated c executable reaches completion after wired libcport graphics calls`` () =
    let program =
        { SymbolNames = Map.empty
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "CURSOR", None, [ literalInt 3; literalInt 4 ], pos)
              BuiltInCall(NamedBuiltIn "CSIZE", None, [ literalInt 12; literalInt 8 ], pos)
              BuiltInCall(NamedBuiltIn "CHAR_USE", None, [ literalString "FONT1"; literalString "FONT2" ], pos)
              BuiltInCall(NamedBuiltIn "SCROLL", None, [ literalInt 2; literalInt 1 ], pos)
              BuiltInCall(NamedBuiltIn "RECOL", None, [ literalInt 0; literalInt 1; literalInt 2; literalInt 3; literalInt 4; literalInt 5; literalInt 6; literalInt 7 ], pos)
              BuiltInCall(NamedBuiltIn "SCALE", None, [ literalFloat 2.0; literalFloat 10.0; literalFloat 20.0 ], pos)
              BuiltInCall(NamedBuiltIn "ARC", None, [ literalFloat 0.0; literalFloat 0.0; literalFloat 10.0; literalFloat 10.0; literalFloat 0.5 ], pos)
              BuiltInCall(NamedBuiltIn "ELLIPSE", None, [ literalFloat 5.0; literalFloat 6.0; literalFloat 3.0; literalFloat 0.7; literalFloat 0.1 ], pos)
              BuiltInCall(Print, None, [ literalString "OK" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "wired_libcport_graphics_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("OK"))

[<Test>]
let ``generated c executable reaches completion after wired libcport file and channel calls`` () =
    let channel9 = ExplicitChannel(literalInt 9)
    let channel10 = ExplicitChannel(literalInt 10)

    let program =
        { SymbolNames = Map.empty
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "ops.txt" ], pos)
              BuiltInCall(Print, Some channel9, [ literalString "ABC" ], pos)
              BuiltInCall(NamedBuiltIn "SET_POSITION", Some channel9, [ literalInt 0 ], pos)
              BuiltInCall(NamedBuiltIn "TRUNCATE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "SET_CHANNEL", Some channel10, [ literalInt 9 ], pos)
              BuiltInCall(Print, Some channel10, [ literalString "XYZ" ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel10, [], pos)
              BuiltInCall(NamedBuiltIn "RENAME", None, [ literalString "ops.txt"; literalString "renamed.txt" ], pos)
              BuiltInCall(NamedBuiltIn "DELETE", None, [ literalString "renamed.txt" ], pos)
              BuiltInCall(Print, None, [ literalString "OK" ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "wired_libcport_file_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("OK"))

[<Test>]
let ``generated c executable supports copy and copy_n`` () =
    let firstSymbol = SymbolId 0
    let secondSymbol = SymbolId 1
    let channel9 = ExplicitChannel(literalInt 9)

    let program =
        { SymbolNames = [ firstSymbol, "FIRST$"; secondSymbol, "SECOND$" ] |> Map.ofList
          Globals =
            [ storage firstSymbol "FIRST$" HirType.String GlobalStorage
              storage secondSymbol "SECOND$" HirType.String GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "OPEN_NEW", Some channel9, [ literalString "source.txt" ], pos)
              BuiltInCall(Print, Some channel9, [ literalString "HELLO" ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "COPY", None, [ literalString "source.txt"; literalString "copy1.txt" ], pos)
              BuiltInCall(NamedBuiltIn "COPY_N", None, [ literalString "copy1.txt"; literalString "copy2.txt" ], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "copy1.txt" ], pos)
              Input(Some channel9, [], [ WriteVar(firstSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(NamedBuiltIn "OPEN_IN", Some channel9, [ literalString "copy2.txt" ], pos)
              Input(Some channel9, [], [ WriteVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "CLOSE", Some channel9, [], pos)
              BuiltInCall(Print, None, [ ReadVar(firstSymbol, HirType.String, pos); ReadVar(secondSymbol, HirType.String, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "copy_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("HELLO HELLO"))

[<Test>]
let ``generated c executable reports compiled program management builtins as unsupported`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun "10 RUN\n" None

    Assert.That(exitCode, Is.EqualTo(1))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Program-management built-ins are not meaningful in generated C programs."))

[<Test>]
let ``generated c executable reports load as unsupported compiled program management`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun "10 LOAD \"demo_bas\"\n" None

    Assert.That(exitCode, Is.EqualTo(1))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Program-management built-ins are not meaningful in generated C programs."))

[<Test>]
let ``generated c executable reports sexec as unsupported host specific behavior`` () =
    let exitCode, stdout, stderr =
        generateCompileAndRun "10 SEXEC \"demo_job\"\n" None

    Assert.That(exitCode, Is.EqualTo(1))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Built-in statement depends on host-specific behavior that is not supported by the generated C backend."))

[<Test>]
let ``generated c executable reports unsupported graphics statements with a specific category`` () =
    let program =
        { SymbolNames = Map.empty
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "PALETTE", None, [ literalInt 1 ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "unsupported_graphics_statement_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(1))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Graphics built-in statement 'PALETTE' is not supported by the generated C backend yet."))

[<Test>]
let ``generated c executable reports unsupported graphics functions with a specific category`` () =
    let pointSymbol = SymbolId 0
    let resultSymbol = SymbolId 1

    let program =
        { SymbolNames = [ pointSymbol, "POINT"; resultSymbol, "RESULT" ] |> Map.ofList
          Globals = [ storage resultSymbol "RESULT" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(
                WriteVar(resultSymbol, HirType.Int, pos),
                CallFunc(pointSymbol, [ ValueArg(literalInt 10); ValueArg(literalInt 20) ], HirType.Int, pos),
                pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "unsupported_graphics_function_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(1))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Graphics built-in function 'POINT' is not supported by the generated C backend yet."))

[<Test>]
let ``generated c executable supports by reference routine mutation for scalars`` () =
    let valueSymbol = SymbolId 0
    let routineSymbol = SymbolId 1
    let parameterSymbol = SymbolId 2

    let program =
        { SymbolNames = [ valueSymbol, "VALUE"; routineSymbol, "BUMP"; parameterSymbol, "P" ] |> Map.ofList
          Globals = [ storage valueSymbol "VALUE" HirType.Int GlobalStorage ]
          Routines =
            [ { Name = "BUMP"
                Symbol = routineSymbol
                Parameters = [ parameter parameterSymbol "P" HirType.Int (RoutineParameterStorage "BUMP") FlexibleBinding ]
                Locals = []
                Body =
                    [ Assign(WriteVar(parameterSymbol, HirType.Int, pos), Binary(Add, ReadVar(parameterSymbol, HirType.Int, pos), literalInt 2, HirType.Int, pos), pos)
                      Return(None, pos) ]
                ReturnType = None
                EndLineNumber = None
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(valueSymbol, HirType.Int, pos), literalInt 3, pos)
              ProcCall(routineSymbol, None, [ RefArg(WriteVar(valueSymbol, HirType.Int, pos)) ], pos)
              BuiltInCall(Print, None, [ ReadVar(valueSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "byref_scalar_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("5"))

[<Test>]
let ``generated c executable rejects string character by reference actuals`` () =
    let textSymbol = SymbolId 0
    let routineSymbol = SymbolId 1
    let parameterSymbol = SymbolId 2

    let program =
        { SymbolNames = [ textSymbol, "TEXT$"; routineSymbol, "BUMP"; parameterSymbol, "P" ] |> Map.ofList
          Globals = [ storage textSymbol "TEXT$" HirType.String GlobalStorage ]
          Routines =
            [ { Name = "BUMP"
                Symbol = routineSymbol
                Parameters = [ parameter parameterSymbol "P" HirType.String (RoutineParameterStorage "BUMP") FlexibleBinding ]
                Locals = []
                Body = [ Return(None, pos) ]
                ReturnType = None
                EndLineNumber = None
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(WriteVar(textSymbol, HirType.String, pos), literalString "AB", pos)
              ProcCall(routineSymbol, None, [ RefArg(WriteStringChar(textSymbol, literalInt 1, HirType.String, pos)) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "invalid_byref_string_char_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(1))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("String character targets cannot be used as by-reference storage locations."))

[<Test>]
let ``generated c executable supports dynamic scope reads and writes inside routines`` () =
    let resultSymbol = SymbolId 0
    let routineSymbol = SymbolId 1
    let localSymbol = SymbolId 2

    let program =
        { SymbolNames = [ resultSymbol, "RESULT"; routineSymbol, "CAPTURE"; localSymbol, "LOCAL" ] |> Map.ofList
          Globals = [ storage resultSymbol "RESULT" HirType.Int GlobalStorage ]
          Routines =
            [ { Name = "CAPTURE"
                Symbol = routineSymbol
                Parameters = []
                Locals = [ storage localSymbol "LOCAL" HirType.Int (RoutineLocalStorage "CAPTURE") ]
                Body =
                    [ Assign(DynamicWriteVar("LOCAL", HirType.Int, pos), literalInt 7, pos)
                      Assign(WriteVar(resultSymbol, HirType.Int, pos), DynamicReadVar("LOCAL", HirType.Int, pos), pos)
                      Return(None, pos) ]
                ReturnType = None
                EndLineNumber = None
                Position = pos } ]
          DataEntries = []
          RestorePoints = []
          Main =
            [ ProcCall(routineSymbol, None, [], pos)
              BuiltInCall(Print, None, [ ReadVar(resultSymbol, HirType.Int, pos) ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "dynamic_scope_routine_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(0), stderr)
    Assert.That(stdout.Trim(), Is.EqualTo("7"))

[<Test>]
let ``generated c executable reports unsupported generic built in functions with a named message`` () =
    let execWaitSymbol = SymbolId 0
    let resultSymbol = SymbolId 1

    let program =
        { SymbolNames = [ execWaitSymbol, "EXEC_W"; resultSymbol, "RESULT" ] |> Map.ofList
          Globals = [ storage resultSymbol "RESULT" HirType.Int GlobalStorage ]
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ Assign(
                WriteVar(resultSymbol, HirType.Int, pos),
                CallFunc(execWaitSymbol, [ ValueArg(literalInt 1) ], HirType.Int, pos),
                pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "unsupported_generic_function_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(1))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Built-in function 'EXEC_W' is not supported by the generated C backend yet."))

[<Test>]
let ``generated c executable reports unsupported turbo statements with a specific category`` () =
    let program =
        { SymbolNames = Map.empty
          Globals = []
          Routines = []
          DataEntries = []
          RestorePoints = []
          Main =
            [ BuiltInCall(NamedBuiltIn "TURBO_TASKN", None, [ literalInt 1 ], pos)
              BuiltInCall(NamedBuiltIn "STOP", None, [], pos) ] }

    let exitCode, stdout, stderr =
        generateHirCompileAndRun "unsupported_turbo_statement_runtime_program" program None

    Assert.That(exitCode, Is.EqualTo(1))
    Assert.That(stdout.Trim(), Is.EqualTo(String.Empty))
    Assert.That(stderr, Does.Contain("Turbo toolkit built-in statement 'TURBO_TASKN' is not supported by the generated C backend yet."))
