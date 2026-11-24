"""Validate executable identity, results and independent profile accounting."""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
from pathlib import Path


def verify_summary(row: dict, expected: str, executable: Path) -> None:
    if row["Output"] != expected or row["ExitCode"] != 0:
        raise ValueError("Incorrect program result.")
    if not row["UnmodifiedExecutable"] or not row["MappedCodeIdentical"]:
        raise ValueError("Executable/code map identity failed.")
    if hashlib.sha256(executable.read_bytes()).hexdigest().upper() != row["ExecutableSha256"]:
        raise ValueError("Executable fingerprint differs.")
    costs = row["Costs"]
    if sum(x["Instructions"] for x in costs) != row["StepsIncludingHostHooks"]:
        raise ValueError("Instruction accounting failed.")
    if sum(x["FetchWords"] for x in costs) != row["FetchWords"]:
        raise ValueError("Fetch accounting failed.")
    hooks = sum(x["Instructions"] for x in costs if x["Group"] == "QDOS host hook")
    rom = sum(x["Instructions"] for x in costs if x["Group"] == "QL ROM")
    if hooks != row["HostHooks"] or row["Instructions"] != row["StepsIncludingHostHooks"] - hooks:
        raise ValueError("Host-hook accounting failed.")
    if rom != row["RomInstructions"] or sum(row["RomServices"].values()) != rom:
        raise ValueError("ROM accounting failed.")
    if sum(x["Instructions"] for x in row["Routines"]) != row["Instructions"]:
        raise ValueError("Routine accounting failed.")
    if sum(x["RomInstructions"] for x in row["Routines"]) != rom:
        raise ValueError("Routine/ROM accounting failed.")


def verify(directory: Path, negative_checks: bool = False) -> dict:
    config = json.loads((directory / "configuration.json").read_text(encoding="utf-8-sig"))
    run = Path(config["Inputs"])
    plan = json.loads((run / "plan.json").read_text(encoding="utf-8-sig"))
    cases = {b["name"]: b for b in plan["benchmarks"]}
    rows = json.loads((directory / "profiles.json").read_text(encoding="utf-8-sig"))
    required = {"string_operations", "float_polynomial", "float_transcendentals", "eight_queens", "concordance"}
    expected_pairs = {(name, compiler) for name in required for compiler in ("SB", "Turbo FAST")}
    if len(rows) != 10 or {(r["Benchmark"], r["Compiler"]) for r in rows} != expected_pairs:
        raise ValueError("Expected exactly ten distinct complete profiles.")
    rom_hash = hashlib.sha256(Path(config["Rom"]).read_bytes()).hexdigest().upper()
    if rom_hash != config["RomSha256"]:
        raise ValueError("Installed ROM differs from the profiled ROM.")
    if not all(config[k] for k in ("RealRomExecuted", "ProfiledUnmodifiedJobs", "CodeMapsByteIdentical", "InstructionCountsAreNotCycles")):
        raise ValueError("Unsupported profiling configuration.")
    for row in rows:
        case = cases[row["Benchmark"]]
        suffix = "s" if row["Compiler"] == "SB" else "t"
        binary = run / "drive" / (case["tag"] + suffix)
        expected = f"{case['name']}:{case['expectedChecksum']}\n"
        verify_summary(row, expected, binary)
        pcs = json.loads((directory / (case["tag"] + suffix + ".pc-counts.json")).read_text())
        if sum(p["Instructions"] for p in pcs) != row["StepsIncludingHostHooks"] or sum(p["FetchWords"] for p in pcs) != row["FetchWords"]:
            raise ValueError("PC histogram differs from the independent totals.")
        if len({p["Pc"] for p in pcs}) != len(pcs):
            raise ValueError("Duplicate PCs in histogram.")
    rejected = 0
    if negative_checks:
        row = rows[0]
        case = cases[row["Benchmark"]]
        binary = run / "drive" / (case["tag"] + "s")
        expected = f"{case['name']}:{case['expectedChecksum']}\n"
        mutations = [("Output", "wrong\n"), ("ExitCode", -1), ("MappedCodeIdentical", False),
                     ("UnmodifiedExecutable", False), ("ExecutableSha256", "0" * 64),
                     ("Instructions", row["Instructions"] + 1), ("FetchWords", row["FetchWords"] + 1),
                     ("RomInstructions", row["RomInstructions"] + 1)]
        for key, value in mutations:
            bad = copy.deepcopy(row)
            bad[key] = value
            try:
                verify_summary(bad, expected, binary)
            except ValueError:
                rejected += 1
            else:
                raise ValueError(f"Failed to reject corrupted {key}.")
    return {"ProfilesPassed": len(rows), "CorruptProfilesRejected": rejected, "RealRomSha256": rom_hash,
            "InstructionAndFetchAccountingPassed": True, "ChecksumsAndExecutableIdentityPassed": True}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--negative-checks", action="store_true")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    result = verify(args.directory, args.negative_checks)
    text = json.dumps(result, indent=2)
    if args.output:
        args.output.write_text(text, encoding="utf-8")
    print(text)
