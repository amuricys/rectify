#!/usr/bin/env python3
"""Run the same geometry probe using native GHC or an installed THC driver."""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("runtime", choices=["ghc", "thc"])
args = parser.parse_args()
compiler = shutil.which(args.runtime)
if not compiler:
    parser.error(f"{args.runtime} is not on PATH; see runtimes/haskell/thc/README.md")

if args.runtime == "ghc":
    # Compile outside the source tree; no Cabal dependency resolution is needed.
    with tempfile.TemporaryDirectory(prefix="rectify-geometry-") as output:
        binary = Path(output) / "geometry-probe"
        subprocess.run([
            compiler, "-O2", "-Wall", "-Werror", "-fforce-recomp",
            "-i" + str(ROOT / "runtimes/haskell/kernels/src"),
            "-outputdir", output, "-o", str(binary),
            str(ROOT / "runtimes/haskell/thc/app/Main.hs"),
        ], check=True, cwd=ROOT)
        subprocess.run([str(binary)], check=True, cwd=ROOT)
else:
    thc_root = os.environ.get("RECTIFY_THC_ROOT")
    if not thc_root or not (Path(thc_root).expanduser() / "build.gradle").is_file():
        parser.error("Set RECTIFY_THC_ROOT to your built THC checkout")
    subprocess.run([
        compiler, "run", "rectify-thc-probe:exe:geometry-probe",
        "--project-dir", str(ROOT / "runtimes/haskell/thc"),
        "--thc-root", str(Path(thc_root).expanduser().resolve()),
    ], check=True, cwd=ROOT)
