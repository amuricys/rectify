#!/usr/bin/env python3
"""Discover projects and run their explicit local commands; no implicit installs."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
manifest = json.loads((ROOT / "workspace.json").read_text())
projects = {project["id"]: project for project in manifest["projects"]}
parser = argparse.ArgumentParser(description=__doc__)
sub = parser.add_subparsers(dest="operation", required=True)
sub.add_parser("list")
sub.add_parser("doctor")
sub.add_parser("check")
up = sub.add_parser("up")
up.add_argument("projects", nargs="*")
run = sub.add_parser("run")
run.add_argument("project", choices=projects)
run.add_argument("action")
args = parser.parse_args()

if args.operation == "up":
    raise SystemExit(subprocess.run([sys.executable, str(ROOT / "scripts/dev.py")] + args.projects).returncode)
elif args.operation == "list":
    for project in projects.values():
        actions = ", ".join(project["commands"]) or "no runnable integration yet"
        print(f"{project['id']:16} {project['status']:13} {project['path']} ({actions})")
elif args.operation == "doctor":
    for tool in sorted({tool for project in projects.values() for tool in project["tools"]}):
        print(f"{tool:10} {shutil.which(tool) or 'not on PATH'}")
    print("Tool presence does not establish a supported version or working backend.")
elif args.operation == "check":
    errors = []
    if len(projects) != len(manifest["projects"]):
        errors.append("Duplicate project IDs")
    for project in projects.values():
        if not (ROOT / project["path"]).is_dir():
            errors.append(f"Missing directory: {project['path']}")
        if project["status"] not in {"implemented", "experimental", "scaffold", "planned"}:
            errors.append(f"Unknown status: {project['id']}")
        for action, command in project["commands"].items():
            if not isinstance(command, list) or not command or not all(isinstance(x, str) for x in command):
                errors.append(f"Invalid command: {project['id']} {action}")
    if errors:
        parser.error("; ".join(errors))
    print(f"Workspace layout valid: {len(projects)} projects. No runtime builds performed.")
else:
    project = projects[args.project]
    command = project["commands"].get(args.action)
    if command is None:
        parser.error(f"No {args.action!r} action for {args.project}; see its runtime documentation")
    if not shutil.which(command[0]):
        parser.error(f"{command[0]} is not on PATH")
    result = subprocess.run(command, cwd=ROOT / project["path"])
    raise SystemExit(result.returncode)
