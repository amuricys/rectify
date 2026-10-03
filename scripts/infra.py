#!/usr/bin/env python3
"""Render Terranix and run Terraform in a persistent, per-stack directory."""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

STACKS = ("frontend", "backend", "fpga")
ACTIONS = ("render", "init", "validate", "plan", "apply", "destroy", "output", "build")


def repository_root():
    explicit = os.environ.get("RECTIFY_ROOT")
    if explicit:
        root = Path(explicit).expanduser().resolve()
    else:
        root = Path.cwd().resolve()
        while not (root / "workspace.json").is_file() and root != root.parent:
            root = root.parent
    if not (root / "flake.nix").is_file() or not (root / "workspace.json").is_file():
        raise ValueError("Run inside the Rectify checkout or set RECTIFY_ROOT")
    return root


def execute(command, **kwargs):
    print("+ " + " ".join(str(part) for part in command), flush=True)
    return subprocess.run(command, check=True, **kwargs)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("stack", choices=STACKS)
    parser.add_argument("action", choices=ACTIONS)
    parser.add_argument("terraform_args", nargs=argparse.REMAINDER,
                        help="Terraform arguments (e.g. -var-file=/absolute/path/config.tfvars)")
    args = parser.parse_args()
    try:
        root = repository_root()
        if args.action == "build":
            if args.stack != "frontend" or args.terraform_args:
                parser.error("build is only supported for the frontend and takes no arguments")
            execute(["npm", "run", "build"], cwd=root / "apps/web")
            return
        state_root = Path(os.environ.get("RECTIFY_INFRA_STATE_DIR", root / ".infra-state")).expanduser().resolve()
        directory = state_root / args.stack
        directory.mkdir(parents=True, exist_ok=True)
        env = os.environ.copy()
        if args.stack == "frontend":
            dist = Path(env.get("TF_VAR_frontend_dir", root / "apps/web/build")).expanduser().resolve()
            env["TF_VAR_frontend_dir"] = str(dist)
            if args.action in ("plan", "apply") and not (dist / "index.html").is_file():
                parser.error("Build frontend assets first: rectify-infra frontend build")
        if args.action not in ("output", "destroy"):
            result = execute([
                "nix", "build", "--no-link", "--print-out-paths",
                str(root) + "#terraform-" + args.stack,
            ], cwd=root, capture_output=True, text=True)
            generated = Path(result.stdout.strip())
            # Validate before replacing a previous generated configuration.
            json.loads(generated.read_text())
            temporary = directory / "config.tf.json.tmp"
            shutil.copyfile(generated, temporary)
            temporary.replace(directory / "config.tf.json")
        elif not (directory / "config.tf.json").is_file():
            parser.error(f"No generated configuration in {directory}; run render first")
        if args.action == "render":
            if args.terraform_args:
                parser.error("render does not accept Terraform arguments")
            print(directory / "config.tf.json")
            return
        prefix = ["terraform", "-chdir=" + str(directory)]
        if args.action != "init":
            execute(prefix + ["init", "-input=false"], env=env)
        forwarded = args.terraform_args
        if forwarded[:1] == ["--"]:
            forwarded = forwarded[1:]
        execute(prefix + [args.action] + forwarded, env=env)
    except (ValueError, FileNotFoundError) as error:
        parser.error(str(error))
    except subprocess.CalledProcessError as error:
        if error.stderr:
            print(error.stderr, file=sys.stderr)
        raise SystemExit(error.returncode)


if __name__ == "__main__":
    main()
