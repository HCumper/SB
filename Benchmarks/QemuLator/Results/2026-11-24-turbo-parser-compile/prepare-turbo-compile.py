"""Prepare a Q-emuLator run in which genuine Turbo 5.09 compiles its own parser (parser509,
restored to QL bytes) at GoldCard speed. Host file timestamps bracket the compile."""
import pathlib, shutil, sys

run = pathlib.Path(sys.argv[1])
bench = pathlib.Path(r"C:\Source\codex-workspace\.scratch\sb-steps-goldcard\qemulator")
source = pathlib.Path(sys.argv[2])
if run.exists():
    raise SystemExit(f"Refusing to reuse {run}")
drive = run / "drive"
drive.mkdir(parents=True)
for name in ("parser_task", "codegen_task", "ttk"):
    shutil.copyfile(bench / "drive" / name, drive / name)

# The same compile-time directives as the benchmark collection, with names for this
# program, a larger IR buffer and more dataspace for a 4,369-line program.
directives = ['TURBO_objfil "mdv1_p509t"', 'TURBO_repfil "mdv1_p509_report"', "TURBO_optim 2",
              'TURBO_model ">"', "TURBO_diags 0", "TURBO_windo 0", "TURBO_objdat 100",
              "TURBO_objstk 350", "TURBO_buffersz 300", "TURBO_list 0", "TURBO_struct 1"]
header = "".join(f"{i} {line}\n" for i, line in enumerate(directives, 1)).encode("ascii")
body = source.read_bytes()
if not body.endswith(b"\n"):
    body += b"\n"
(drive / "p509_bas").write_bytes(header + body)
(run / "p509_sb.bas").write_bytes(header + body)

# Boot: load the toolkit, queue the compile line, then LOAD the program. SuperBASIC runs the
# queued line once LOAD returns to command mode. Files s and e bracket parse plus codegen.
command = ('OPEN_NEW #3,mdv1_s:CLOSE #3:EXECUTE_W mdv1_parser_task;"*":EW mdv1_codegen_task:'
           'OPEN_NEW #3,mdv1_e:CLOSE #3')
if len(command) > 120:
    raise SystemExit(f"command too long for the keyboard queue: {len(command)}")
typed = command.replace('"', '"&CHR$(34)&"')
boot = ['LRESPR "mdv1_ttk"', 'PROG_USE "mdv1_"', 'DATA_USE "mdv1_"',
        f'TYPE_IN "{typed}"&CHR$(10)', 'LOAD "mdv1_p509_bas"']
(drive / "boot").write_bytes("".join(f"{100 + 10 * i} {line}\n" for i, line in enumerate(boot)).encode("ascii"))

config = (bench / "compile.QCF").read_text()
win = lambda p: str(p).replace("/", chr(92))
config = config.replace(win(bench / "drive"), win(drive)).replace("Speed=Full", "Speed=GoldCard")
if "Speed=GoldCard" not in config or win(drive) not in config:
    raise SystemExit("configuration not rewritten")
(run / "turbo.QCF").write_text(config, encoding="ascii", newline="\r\n")
print("prepared", run)
print((drive / "boot").read_text())
