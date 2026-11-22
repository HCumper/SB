"""Prepare ordinary benchmark executables and an external QDOS timing runner."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import shutil

ROOT = Path(__file__).resolve().parent
SCOPE = "Whole job: executable loading, runtime startup, initialization, computation, internal checks, checksum PRINT and shutdown; runner file opening/closing and reporting are outside the timer."

def write_text(path, text):
    path.write_text(text, encoding="ascii", newline="\n")

def ql(lines, start=100, step=10):
    return "".join(f"{start+step*i} {line}\n" for i,line in enumerate(lines))

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output",type=Path,required=True)
    parser.add_argument("--collection",type=Path,default=ROOT/"Programs")
    parser.add_argument("--trials",type=int,default=3)
    parser.add_argument("--warmups",type=int,default=1)
    parser.add_argument("--name",action="append",default=[])
    parser.add_argument("--turbo-directory",type=Path,default=Path(r"I:\QL\Turbo"))
    parser.add_argument("--toolkit",type=Path,default=Path(r"C:\Source\codex-workspace\ql-records-demo\turbo_tk"))
    parser.add_argument("--pollclock",type=Path)
    parser.add_argument("--rom",type=Path,default=Path(r"C:\Program Files (x86)\Q-emuLator\QL ROMs\QL_ROM_JS"))
    parser.add_argument("--toolkit-rom",type=Path,default=Path(r"C:\Program Files (x86)\Q-emuLator\QL ROMs\TK2_rom"))
    args=parser.parse_args()
    if not(1<=args.trials<=20 and 0<=args.warmups<=5):
        parser.error("Use 1..20 trials and 0..5 warm-ups")
    run=args.output.resolve()
    drive=run/"drive"
    drive.mkdir(parents=True,exist_ok=True)
    if (drive/"done").exists() or list(drive.glob("r[0-9][0-9][0-9]")):
        raise ValueError("Output directory contains timing samples; preserve them before preparing another run")
    if any(re.fullmatch(r'b\d\d[st]',p.name) for p in drive.iterdir()) or (drive/'build_done').exists():
        raise ValueError("Output directory contains compiled jobs; preserve or explicitly clear generated builds before preparing new inputs")
    cases=json.loads((args.collection/"suite.json").read_text())["benchmarks"]
    unknown=set(args.name)-{c["name"] for c in cases}
    if unknown:
        parser.error(f"Unknown benchmarks: {sorted(unknown)}")
    cases=[c for c in cases if not args.name or c["name"] in args.name]
    if not cases:
        parser.error("No benchmarks selected")
    for source,target in ((args.turbo_directory/"parser_task","parser_task"),
                          (args.turbo_directory/"codegen_task","codegen_task"),
                          (args.toolkit,"ttk")):
        shutil.copyfile(source,drive/target)
    if args.pollclock:
        shutil.copyfile(args.pollclock,drive/"pollclock")
    else:
        (drive/"pollclock").write_bytes(bytes.fromhex((ROOT/"pollclock.hex").read_text()))
    plan=[]
    commands=[]
    for index,case in enumerate(cases):
        tag=f"b{index:02}"
        # Preserve every source line, including PRINT and STOP. Only compile-time
        # TURBO directives precede the exact original program text.
        original=(args.collection/case["file"]).read_text(encoding="ascii").replace("\r\n","\n")
        numbers=[int(re.match(r"^(\d+) ",line)[1]) for line in original.splitlines()]
        if min(numbers)<100:
            raise ValueError("Program line numbers must leave 1..11 for compile-time directives")
        directives=[f'TURBO_objfil "mdv1_{tag}t"',f'TURBO_repfil "mdv1_{tag}_report"',
                    'TURBO_optim 2','TURBO_model ">"','TURBO_diags 0','TURBO_windo 0',
                    'TURBO_objdat 20','TURBO_objstk 350','TURBO_buffersz 64','TURBO_list 0','TURBO_struct 1']
        source=ql(directives,1,1)+original
        write_text(run/f"{tag}_common.bas",original)
        write_text(run/f"{tag}_sb.bas",source)
        write_text(drive/f"{tag}_bas",source)
        plan.append(dict(case,tag=tag,sourceFile=str((args.collection/case["file"]).resolve()),
                         sourceSha256=hashlib.sha256(original.encode("ascii")).hexdigest().upper(),
                         compilerDirectives=directives))
        # These are immediate BASIC commands, never part of a compiled program.
        commands.append(dict(tag=tag,benchmark=case["name"],load=f"LOAD mdv1_{tag}_bas",
            parse='EXECUTE_A mdv1_parser_task;"*"',
            generate=f"SBYTES mdv1_{tag}_ir,PEEK_L(PEEK_L(164192)+36),65536:EW mdv1_codegen_task"))

    runner=['clock=RESPR(80)','LBYTES "mdv1_pollclock",clock','CALL clock',
            'OPEN_NEW #3,"mdv1_cal_start"','PRINT #3,"START"','CLOSE #3',
            'cal_start=PEEK_L(clock+36)','PAUSE 100','cal_ticks=PEEK_L(clock+36)-cal_start',
            'OPEN_NEW #3,"mdv1_cal_end"','PRINT #3,cal_ticks','CLOSE #3']
    sequence=0
    for trial in range(1-args.warmups,args.trials+1):
        for index,case in enumerate(plan):
            modes=["t","s"] if (trial+index)%2 else ["s","t"]
            for mode in modes:
                sequence+=1
                runner.append(f'onecase "{case["tag"]}{mode}","{case["name"]}","{mode}",{trial},{case["passes"]},{case["expectedChecksum"]},"r{sequence:03}"')
    runner+=['OPEN_NEW #3,"mdv1_done"','PRINT #3,"DONE"','CLOSE #3','STOP']
    # lib537 assigns inherited channels backwards from #31. Supplying 31
    # references to the one output file makes normal PRINT #1 reach that file.
    # Both compilers use this same documented startup channel mechanism.
    channels=",".join(["#4"]*31)
    procedure=['DEFine PROCedure onecase(file$,label$,compiler$,trial,passes,wanted,result$)',
               'LOCal began,ticks',
               'OPEN_NEW #4,"mdv1_" & result$ & "_out"',
               'OPEN_NEW #3,"mdv1_" & result$ & "_start"',
               'PRINT #3,label$;",";compiler$;",";trial','CLOSE #3',
               'began=PEEK_L(clock+36)',
               f'EW "mdv1_" & file$,{channels}',
               'ticks=PEEK_L(clock+36)-began',
               'CLOSE #4',
               'OPEN_NEW #3,"mdv1_" & result$',
               'PRINT #3,label$;",";compiler$;",";trial;",";passes;",";wanted;",";ticks;",returned"',
               'CLOSE #3','END DEFine']
    write_text(drive/"runner_bas",ql(runner)+ql(procedure,15000))
    boot=['LRESPR "mdv1_ttk"','PROG_USE "mdv1_"','DATA_USE "mdv1_"','LOAD "mdv1_b00_bas"']
    write_text(drive/"boot",ql(boot))
    write_text(drive/"run_boot",ql(boot[:3]+['LRUN "mdv1_runner_bas"']))
    configuration='\n'.join(['Ram=640K',f'MainRom={args.rom}',f'BackRom={args.toolkit_rom}',
        'BackRomActive=Yes','ExpRomActive=No','UseFloppyName=Yes','FloppyName=FLP',
        'UseHardDiskName=Yes','HardDiskName=WIN','HasRamDisk=Yes','RamDiskName=RAM',
        'HasParPort=No','TCP=Off','Subdirs=Off','Speed=QL','FastStartup=Yes',
        'AutoStartSession=Yes','FirstKey=F1','DisplayMode=Window','AcceleratedGraphics=No',
        'Sound=Off','SampledSound=Off','Joystick1=None','SER1=None','SER2=None','SER3=None',
        'SER4=None',f'Slot1={drive}',*[f'Slot{i}=Empty' for i in range(2,9)]])+'\n'
    write_text(run/"benchmark.QCF",configuration)
    write_text(run/"compile.QCF",configuration.replace('Speed=QL','Speed=Full'))
    (run/"plan.json").write_text(json.dumps(dict(formatVersion=2,timingMethod="external-qdos-job",
        trials=args.trials,warmups=args.warmups,timerHz=50,expectedResults=sequence,
        benchmarks=plan,scope=SCOPE,outputChannel=1,inheritedChannels=31),indent=2)+"\n")
    (run/"compile-commands.json").write_text(json.dumps(commands,indent=2)+"\n")
    print(f"Prepared {len(cases)} unchanged programs and {sequence} external job timings in {run}")

if __name__=="__main__":
    main()
