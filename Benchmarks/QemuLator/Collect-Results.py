"""Validate externally timed, unchanged QDOS jobs and their captured stdout."""
from pathlib import Path
import argparse
import csv
import json
import re
import statistics

def birth(path):
    """A file's creation time on the host."""
    status=path.stat()
    return getattr(status,'st_birthtime',status.st_ctime)

# The host time between a job's start and result files exceeds its emulated
# time only by file creation, at most a tick or so. More means the emulator
# fell behind real time: its 50 Hz clock then follows host time while each
# frame runs fewer instructions, so the emulated time is inflated too.
REAL_TIME_TOLERANCE_RATIO=0.02
REAL_TIME_TOLERANCE_SECONDS=0.05

def behind_real_time(seconds,host_seconds):
    return host_seconds is not None and host_seconds>seconds*(1+REAL_TIME_TOLERANCE_RATIO)+REAL_TIME_TOLERANCE_SECONDS

def collect(run, require_complete=False):
    plan=json.loads((run/'plan.json').read_text())
    if plan.get('formatVersion')!=2 or plan.get('timingMethod')!='external-qdos-job':
        raise ValueError('Expected external timing format 2; old instrumented results must not be mixed in')
    drive=run/'drive'
    cases={c['name']:c for c in plan['benchmarks']}
    rows=[]
    seen=set()
    for path in sorted(drive.glob('r[0-9][0-9][0-9]')):
        fields=path.read_text(encoding='ascii').strip().split(',')
        if len(fields)!=7:
            raise ValueError(f'Malformed external result: {path}: {fields}')
        name,compiler,trial,passes,expected,ticks,status=fields
        trial,passes,expected,ticks=map(int,(trial,passes,expected,ticks))
        if name not in cases or compiler not in ('s','t'):
            raise ValueError(f'Unknown benchmark/compiler: {path}')
        case=cases[name]
        if passes!=case['passes'] or expected!=case['expectedChecksum']:
            raise ValueError(f'Workload differs from plan: {path}')
        key=name,compiler,trial
        if key in seen or not 1-plan['warmups']<=trial<=plan['trials']:
            raise ValueError(f'Duplicate or unexpected trial: {key}')
        seen.add(key)
        output=drive/(path.name+'_out')
        stdout=output.read_text(encoding='ascii') if output.exists() else ''
        match=re.fullmatch(re.escape(name)+r'\s*:\s*([+-]?\d+)\s*',stdout.strip())
        actual=int(match[1]) if match else None
        start=drive/(path.name+'_start')
        host_seconds=birth(path)-birth(start) if start.exists() else None
        seconds=ticks/plan['timerHz']
        behind=behind_real_time(seconds,host_seconds)
        valid=status=='returned' and actual==expected and ticks>0 and host_seconds is not None and not behind
        rows.append(dict(sequence=path.name,benchmark=name,compiler='SB' if compiler=='s' else 'Turbo FAST',
            trial=trial,passes=passes,expected=expected,checksum=actual,ticks=ticks,
            seconds=seconds,hostJobSeconds=host_seconds,behindRealTime=behind,stdout=stdout,
            status='passed' if valid else 'failed'))
    def write_csv(path,data):
        if data:
            with path.open('w',newline='') as f:
                writer=csv.DictWriter(f,fieldnames=list(data[0]));writer.writeheader();writer.writerows(data)
    write_csv(run/'samples.csv',rows)
    (run/'samples.json').write_text(json.dumps(rows,indent=2)+'\n')
    summary=[]
    for case in plan['benchmarks']:
        row=dict(benchmark=case['name'],category=case['category'],passes=case['passes'],checksum=case['expectedChecksum'])
        for compiler,prefix,suffix in (('Turbo FAST','turbo','t'),('SB','sb','s')):
            samples=[r['seconds'] for r in rows if r['benchmark']==case['name'] and r['compiler']==compiler and r['trial']>0 and r['status']=='passed']
            row[prefix+'Samples']=len(samples)
            row[prefix+'Median']=statistics.median(samples) if samples else None
            row[prefix+'Min']=min(samples) if samples else None
            row[prefix+'Max']=max(samples) if samples else None
            binary=drive/(case['tag']+suffix)
            row[prefix+'Bytes']=binary.stat().st_size if binary.exists() else None
        before,after=row['turboMedian'],row['sbMedian']
        row['sbSpeedup']=before/after if before and after else None
        row['sbTimeReductionPercent']=100*(before-after)/before if before and after else None
        summary.append(row)
    (run/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
    write_csv(run/'summary.csv',summary)
    calibration=None
    if (drive/'cal_end').exists() and (drive/'cal_start').exists():
        calibration=dict(ticks=int((drive/'cal_end').read_text().strip()),expectedPauseTicks=100,
            hostSeconds=birth(drive/'cal_end')-birth(drive/'cal_start'))
    complete=(drive/'done').exists() and len(rows)==plan['expectedResults']
    state=dict(formatVersion=2,timingMethod=plan['timingMethod'],scope=plan['scope'],complete=complete,
        samples=len(rows),expected=plan['expectedResults'],failed=[r for r in rows if r['status']!='passed'],calibration=calibration)
    state['clockValidated']=bool(calibration and 100<=calibration['ticks']<=110 and abs(calibration['ticks']/plan['timerHz']-calibration['hostSeconds'])<=0.15)
    state['clockDiscrepancies']=[r['sequence'] for r in rows if r['hostJobSeconds'] is None or r['seconds']>r['hostJobSeconds']+0.05]
    state['behindRealTime']=[r['sequence'] for r in rows if r['behindRealTime']]
    (run/'result-state.json').write_text(json.dumps(state,indent=2)+'\n')
    print(json.dumps(state,indent=2))
    for row in summary:
        print(f"{row['benchmark']:24} Turbo={str(row['turboMedian']):>7} SB={str(row['sbMedian']):>7} speedup={row['sbSpeedup']}")
    if require_complete and (not complete or state['failed'] or not state['clockValidated'] or state['clockDiscrepancies']):
        raise ValueError('Benchmark run is incomplete or contains failures')
    return state,summary

if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('run',type=Path)
    parser.add_argument('--complete',action='store_true')
    args=parser.parse_args()
    collect(args.run,args.complete)
