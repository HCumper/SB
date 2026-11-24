"""Create diagnostic source copies; compiler and test-harness sources stay untouched."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path


def digest(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest().upper()


def apply_recipe(source: str, recipe: dict) -> str:
    if digest(source) != recipe["SourceSha256"]:
        raise ValueError(f"Source changed: {recipe['Source']}. Review the profiling recipe before regenerating it.")
    for edit in reversed(recipe["Edits"]):
        start, end = edit["Start"], edit["End"]
        if source[start:end] != edit["Old"]:
            raise ValueError("Profiling recipe does not match its source.")
        source = source[:start] + edit["New"] + source[end:]
    return source


def prepare(repository: Path, output: Path) -> None:
    here = Path(__file__).resolve().parent
    recipes = json.loads((here / "source-recipes.json").read_text(encoding="utf-8"))
    expected = {"SB/TurboCodegen.fs", "SBTests/TurboExecutionHarness.fs"}
    if {r["Source"] for r in recipes} != expected or len(recipes) != 2:
        raise ValueError("Unexpected diagnostic source inputs.")
    output.mkdir(parents=True, exist_ok=True)
    hashes = []
    for recipe in recipes:
        source = (repository / recipe["Source"]).read_text(encoding="utf-8-sig")
        result = apply_recipe(source, recipe)
        if Path(recipe["Output"]).name != recipe["Output"]:
            raise ValueError("Generated source must stay in the output folder.")
        (output / recipe["Output"]).write_text(result, encoding="utf-8")
        hashes.append({"Source": recipe["Source"], "SourceSha256": digest(source), "GeneratedSha256": digest(result)})
    template = (here / "WorkloadProfiler.fsx.in").read_text(encoding="utf-8-sig")
    template = template.replace("@COMPILER_BIN@", (repository / "SB/bin/Debug/net10.0").as_posix())
    template = template.replace("@TEST_BIN@", (repository / "SBTests/bin/Debug/net10.0").as_posix())
    (output / "Profile-Workloads.fsx").write_text(template, encoding="utf-8")
    (output / "runtime-map.json").write_bytes((here / "runtime-map.json").read_bytes())
    (output / "generated-source-provenance.json").write_text(json.dumps({
        "Sources": hashes, "ProductionSourcesModified": False,
        "CodeMapMustReproduceExecutable": True,
    }, indent=2), encoding="utf-8")
    print(f"Prepared read-only code maps and a real-ROM runner in {output}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--repository", type=Path, default=Path(__file__).resolve().parents[2])
    args = parser.parse_args()
    prepare(args.repository.resolve(), args.output.resolve())
