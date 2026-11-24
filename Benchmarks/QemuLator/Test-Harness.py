"""Regression checks for external stdout validation and untouched guest inputs."""
import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

ROOT=Path(__file__).resolve().parent
sys.dont_write_bytecode=True
SCRATCH=Path(os.environ.get('SB_BENCHMARK_TEST_TEMP',r'C:\Source\codex-workspace\.scratch\qemulator-harness-tests'))
SCRATCH.mkdir(parents=True,exist_ok=True)
spec=importlib.util.spec_from_file_location('collector',ROOT/'Collect-Results.py')
collector=importlib.util.module_from_spec(spec)
spec.loader.exec_module(collector)

class ExternalResults(unittest.TestCase):
    def setUp(self):
        self.directory=tempfile.TemporaryDirectory(dir=SCRATCH)
        self.addCleanup(self.directory.cleanup)
        self.run=Path(self.directory.name)
        self.drive=self.run/'drive';self.drive.mkdir()
        plan=dict(formatVersion=2,timingMethod='external-qdos-job',scope='whole job',warmups=0,trials=1,
            timerHz=50,expectedResults=2,benchmarks=[dict(name='probe',tag='b00',category='test',passes=1,expectedChecksum=42)])
        (self.run/'plan.json').write_text(json.dumps(plan))
        for n,mode in ((1,'s'),(2,'t')):
            name=f'r{n:03}'
            (self.drive/(name+'_start')).write_text(f'probe,{mode},1\n')
            (self.drive/(name+'_out')).write_text('probe:42\n')
            (self.drive/name).write_text(f'probe,{mode},1,1,42,50,returned\n')
        (self.drive/'done').write_text('DONE\n')
    def collect(self,complete=False):
        with contextlib.redirect_stdout(io.StringIO()):
            return collector.collect(self.run,complete)
    def test_ordinary_stdout_and_whole_job_scope(self):
        state,rows=self.collect()
        self.assertTrue(state['complete']);self.assertFalse(state['failed'])
        self.assertEqual(rows[0]['sbMedian'],1.0)
        self.assertEqual(state['timingMethod'],'external-qdos-job')
    def test_wrong_checksum_fails(self):
        (self.drive/'r001_out').write_text('probe:41\n')
        self.assertEqual(len(self.collect()[0]['failed']),1)
    def test_runtime_error_or_missing_stdout_fails(self):
        for stdout in ('','*** Out of range\n','FAIL probe\nprobe:42\n','probe:42\nprobe:42\n'):
            with self.subTest(stdout=stdout):
                (self.drive/'r001_out').write_text(stdout)
                self.assertEqual(len(self.collect()[0]['failed']),1)
    def test_no_return_or_zero_ticks_fails(self):
        for ticks,status in ((0,'returned'),(50,'error')):
            (self.drive/'r001').write_text(f'probe,s,1,1,42,{ticks},{status}\n')
            self.assertEqual(len(self.collect()[0]['failed']),1)
    def test_missing_start_fails(self):
        (self.drive/'r001_start').unlink()
        self.assertEqual(len(self.collect()[0]['failed']),1)
    def test_duplicate_trial_is_rejected(self):
        (self.drive/'r003').write_text((self.drive/'r001').read_text())
        with self.assertRaisesRegex(ValueError,'Duplicate'):
            self.collect()
    def test_instrumented_or_malformed_result_is_rejected(self):
        (self.drive/'r001').write_text('probe,s,1,1,42,42,50,31415,passed\n')
        with self.assertRaisesRegex(ValueError,'Malformed external'):
            self.collect()
    def test_wrong_workload_is_rejected(self):
        (self.drive/'r001').write_text('probe,s,1,99,42,50,returned\n')
        with self.assertRaisesRegex(ValueError,'Workload differs'):
            self.collect()
    def test_old_plan_is_rejected(self):
        p=json.loads((self.run/'plan.json').read_text());p['formatVersion']=1
        (self.run/'plan.json').write_text(json.dumps(p))
        with self.assertRaisesRegex(ValueError,'external timing format'):
            self.collect()
    def test_clock_failures_block_complete_run(self):
        with self.assertRaisesRegex(ValueError,'incomplete or contains failures'):
            self.collect(True)
    def test_job_behind_real_time_fails(self):
        # Both jobs ran 1.0 emulated seconds. SB's took 1.5 s on the host: the
        # emulator fell behind real time. Turbo's 1.06 s is file-creation slack.
        times={'r001_start':0.0,'r001':1.5,'r002_start':2.0,'r002':3.06}
        with mock.patch.object(collector,'birth',lambda path:times[path.name]):
            state,rows=self.collect()
        self.assertEqual(state['behindRealTime'],['r001'])
        self.assertEqual([r['sequence'] for r in state['failed']],['r001'])
        self.assertIsNone(rows[0]['sbMedian'])
        self.assertEqual(rows[0]['turboMedian'],1.0)

class GuestPreparation(unittest.TestCase):
    def test_exact_program_body_without_timer_or_build_launcher(self):
        with tempfile.TemporaryDirectory(dir=SCRATCH) as temp:
            run=Path(temp)/'run'
            subprocess.run([sys.executable,str(ROOT/'Prepare-Benchmarks.py'),'--output',str(run)],check=True,capture_output=True)
            plan=json.loads((run/'plan.json').read_text())
            self.assertEqual(len(plan['benchmarks']),9)
            self.assertEqual(plan['expectedResults'],72)
            for case in plan['benchmarks']:
                original=Path(case['sourceFile']).read_text(encoding='ascii')
                header=''.join(f'{i} {line}\n' for i,line in enumerate(case['compilerDirectives'],1))
                for source in (run/(case['tag']+'_sb.bas'),run/'drive'/(case['tag']+'_bas')):
                    self.assertEqual(source.read_text(encoding='ascii'),header+original)
                    self.assertNotIn(b'\r',source.read_bytes())
                    self.assertNotRegex(source.read_text(),r'(?i)\b(?:PEEK|POKE|EXECUTE|SBYTES|build_self)\b')
            runner=(run/'drive'/'runner_bas').read_text()
            self.assertNotRegex(runner,r'(?i)\bPOKE')
            self.assertIn('began=PEEK_L(clock+36)',runner)
            self.assertEqual(plan['inheritedChannels'],31)
            again=subprocess.run([sys.executable,str(ROOT/'Prepare-Benchmarks.py'),'--output',str(run)],capture_output=True)
            self.assertEqual(again.returncode,0)
            (run/'drive'/'b00s').write_bytes(b'compiled job')
            stale=subprocess.run([sys.executable,str(ROOT/'Prepare-Benchmarks.py'),'--output',str(run)],capture_output=True)
            self.assertNotEqual(stale.returncode,0)
            (run/'drive'/'b00s').unlink()
            (run/'drive'/'r001').write_text('existing result')
            refused=subprocess.run([sys.executable,str(ROOT/'Prepare-Benchmarks.py'),'--output',str(run)],capture_output=True)
            self.assertNotEqual(refused.returncode,0)

if __name__=='__main__':
    unittest.main(verbosity=2)
