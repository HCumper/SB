# Profiling unchanged Turbo and SB executables

These tools count executed instructions, fetch words, runtime templates, QL ROM
services and SB routines. They execute the installed QL ROM, including floating
arithmetic and native residents. The existing functional CPU supplies console
and job services; those host hooks are counted separately. There is no host
replacement for QL floating arithmetic or string collation.

The program bytes are unchanged. A diagnostic copy of the current generator
records code ranges and must reproduce each executable's complete code payload
byte for byte. The normal SB CLI also independently rebuilds each SB job and
requires an identical executable. No timing statements, POKEs or other markers
are inserted into BASIC, IR or executable code. Production compiler and test
harness sources are read without being edited.

Use an existing `Benchmarks/QemuLator` run directory containing the five slower
programs, their verified SB and genuine Turbo FAST jobs, captured Turbo IR,
SB source inputs and `sb-build.json`. Build SB and SBTests first if needed.
The current targets are eight queens, concordance, string operations, polynomial
floating work and transcendental functions.

```powershell
.\Benchmarks\Profiling\Run-Profiles.ps1 `
  -RunDirectory C:\Source\codex-workspace\.scratch\turbo-workload-profiles\qemulator `
  -OutputDirectory C:\Source\codex-workspace\.scratch\turbo-workload-profiles\reproduced-profiles
```

The generator/harness recipes are pinned to reviewed source fingerprints.
If either source changes, preparation stops: review and update the diagnostic
recipe, then repeat the executable-identity and accounting checks. The runtime
symbol map is separately pinned to `lib537.bin`. ROM resident addresses are
derived from the ROM selected in the run's QCF, without modifying that file.
Diagnostic source copies and all generated output stay in the specified output
directory. Completed profiles cannot be overwritten by the wrapper.

Every run requires correct independent checksums and zero exits for all ten
compiler/workload pairs. Per-PC histograms must sum to the independently counted
instruction and fetch totals; routine, ROM-service and host-hook accounting is
also checked. Eight deliberate corruptions verify that bad evidence is rejected.

Instruction shares describe functional work, not elapsed-time shares or calibrated
68008 cycles. The small QDOS shim excludes kernel, scheduler, file loading and
console implementation costs. Its BASIC name table contains the four required
residents and its console arguments match the external runner. SB routine
attribution follows mapped IR records and threaded dispatch; returns are charged
to the callee until the next caller record executes. Turbo's captured IR lacks
routine names, so its routine table is deliberately unpartitioned. Fine runtime
labels can name internal copy loops, such as `byte_table`, which contains code.
ROM call tables include both boundary-entry counters and named service counters;
those are overlapping observations and should not be added together.

Elapsed comparisons use the separate external Q-emuLator harness, executing the
same files with the real QDOS environment. Keep profiling runs and other build/test
work separate from those timing trials. See
[the profiling findings](../QemuLator/Results/2026-11-23-workload-profiles/README.md).
