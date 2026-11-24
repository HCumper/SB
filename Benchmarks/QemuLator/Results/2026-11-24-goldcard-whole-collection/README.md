# Whole collection at GoldCard speed

The first GoldCard timing of the whole collection since the native string,
inline FOR stepping, terminal STOP, in-place string and direct ROM maths
changes. SB is HEAD `4af8b21`. The Turbo FAST executables are the same
verified genuine captures as in every earlier result.

**SB is faster than Turbo FAST on all nine workloads.**

Unmodified executables under Q-emuLator 3.5.1 at **GoldCard speed
(`Speed=GoldCard`)**, JS ROM and Toolkit II, 640 KiB RAM, no accelerated
graphics or sound. Each run is one warm-up and three measured trials per
compiler, externally timed by the 50 Hz poll clock (resolution 0.02 s). The
table pools the six measured trials of two runs; times are seconds, median
(minimum–maximum).

| Benchmark | Turbo FAST | SB | SB speed-up | SB time | SB instructions |
|---|---:|---:|---:|---:|---:|
| integer_arithmetic | 0.81 (0.80–0.82) | 0.30 (0.30–0.30) | 2.70× | −63.0% | −64.6% |
| sieve | 0.52 (0.52–0.54) | 0.18 (0.18–0.20) | 2.89× | −65.4% | −67.3% |
| matrix_multiply | 0.46 (0.46–0.46) | 0.28 (0.28–0.28) | 1.64× | −39.1% | −41.2% |
| string_operations | 0.62 (0.62–0.62) | 0.59 (0.58–0.60) | 1.05× | −4.8% | −3.9% |
| float_polynomial | 2.08 (2.08–2.08) | 2.06 (2.06–2.06) | 1.01× | −1.0% | −1.0% |
| float_transcendentals | 3.12 (3.10–3.12) | 2.74 (2.74–2.76) | 1.14× | −12.2% | −12.1% |
| eight_queens | 2.54 (2.54–2.56) | 0.94 (0.92–0.94) | 2.70× | −63.0% | −63.8% |
| route_planner | 4.60 (4.60–4.62) | 2.09 (2.08–2.10) | 2.20× | −54.6% | −54.8% |
| concordance | 7.81 (7.80–7.82) | 5.30 (5.30–5.32) | 1.47× | −32.1% | −32.1% |

"SB instructions" is the change in executed instructions against Turbo FAST
from the [instruction counts](../2026-11-24-direct-rom-maths/instruction-counts.csv).
Elapsed time follows it closely on every workload.

- float_polynomial's lead is one tick (0.02 s) in every trial: SB measured
  2.06 s and Turbo FAST 2.08 s six times out of six.
- string_operations is 0.02–0.04 s ahead in every trial.
- Turbo FAST's eight_queens time, 2.54 s, matches the earlier quiet-host
  N-queens run's 2.56 s
  ([result](../2026-11-24-fused-conditions-abs-ranges/README.md)).

All 144 jobs produced their exact checksums and returned normally. The two
runs agree to within one tick on every workload
([run A](timings/run-a/summary.csv), [run B](timings/run-b/summary.csv)).

## Keeping the emulator in real time

Each sample records both the emulated time and the host time between the
job's start and result files; in a valid run the two agree. When Q-emuLator
fell behind real time, the emulated times grew as well, by up to 30%. That
is consistent with its 50 Hz interrupt following host time while the emulated
CPU completes fewer instructions per frame.

Two earlier attempts today were not valid, with this host at about 20% CPU
(another workload was running):

- **Default launch**, hidden window: from the 11th job on, the emulator ran
  about 1.46× slower than real time. 62 of 72 samples were behind, and Turbo
  FAST's eight_queens measured 3.30 s
  ([samples](timings/throttled-hidden/samples.csv)).
- **Pinned to performance cores at high priority**: the first two trials kept
  real time, then 37 samples fell behind by the same factor
  ([samples](timings/throttled-pinned/samples.csv)).

This matches Windows 11 power throttling of background processes, which can
slow a process with a hidden window and ignore its timer-resolution requests.
In runs A and B the emulator, launched hidden as before, was also opted out of
power throttling (`SetProcessInformation`, `ProcessPowerThrottling`: execution
speed and ignored timer resolution), as well as pinned to logical processors
0–15 at high priority. Every one of the 144 samples then kept real time:
host minus emulated job time was −0.008 to +0.027 s, within one tick
([launch records](timings/run-a/emulator-launch.json)).

The morning's whole-collection run, which had SB behind on three kernels, shows
the same throttling: host/emulated median 1.46. Its absolute times are not
comparable with these.

## Validation

- `Collect-Results.py --complete` accepted both throttled runs. Its clock
  check compares a 100-tick `PAUSE` with host time, and rejects samples only
  when emulated time exceeds host time, not when host time exceeds it.
- With power throttling disabled, that calibration `PAUSE` ended early: after
  0 ticks in run A and 53 ticks (1.06 s, still 50 Hz) in run B. It began 4.36 s
  after launch in all four runs, well after the settings were applied.
  `Collect-Results.py --complete` therefore rejected runs A and B on
  calibration alone. Every other check passed: complete, no failures, no clock
  discrepancies ([state](timings/run-a/result-state.json)). The per-sample
  host times above check the clock over 148 s of jobs in each run, against
  the calibration's 2 s.
- SB executables were rebuilt from the unmodified sources with the normal CLI
  and required identical to an ordinary build
  ([SB verification](sb-verification.json), [build](sb-build.json)). Apart
  from the 4-byte job name, they are byte-identical to the executables
  counted in the [direct ROM maths result](../2026-11-24-direct-rom-maths/README.md).
- The Turbo FAST captures were re-verified ([native IR](native-verification.json),
  [provenance](turbo-provenance.json)).
- Host CPU was sampled every 5 s during each run (`host-load.csv`): median
  18–21%, the other workload.

## Reproduction

[`Prepare-Run.ps1`](Prepare-Run.ps1) copies the prepared inputs and verified
Turbo captures to a fresh run directory, re-verifies Turbo, builds SB with
`Build-SB.ps1`, and installs the timing boot. [`Run-Timing.ps1`](Run-Timing.ps1)
launches the emulator hidden, opts it out of power throttling, pins and raises
it, waits for `drive\done`, and collects the results. Both hold this machine's
paths.
