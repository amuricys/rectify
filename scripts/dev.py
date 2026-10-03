#!/usr/bin/env python3
"""Run selected local workbench services and stop their process groups together."""
import argparse
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]


def main():
    projects = {p['id']: p for p in json.loads((ROOT / 'workspace.json').read_text())['projects']}
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('projects', nargs='*', default=['web', 'julia', 'lean'])
    args = parser.parse_args()
    selected = list(dict.fromkeys(args.projects or ['web', 'julia', 'lean']))
    commands = []
    for name in selected:
        if name not in projects:
            parser.error(f'Unknown project: {name}')
        project = projects[name]
        command = project['commands'].get('dev') or project['commands'].get('run')
        if not command:
            parser.error(f'{name} has no local service command')
        if not shutil.which(command[0]):
            parser.error(f'{command[0]} is missing; enter nix develop first')
        commands.append((name, command, ROOT / project['path']))
    processes = []
    try:
        for name, command, directory in commands:
            print(f'Starting {name}: {" ".join(command)}', flush=True)
            process = subprocess.Popen(command, cwd=directory, start_new_session=True)
            processes.append((name, process))
        print('Ctrl-C stops all selected services.', flush=True)
        while True:
            for name, process in processes:
                code = process.poll()
                if code is not None:
                    print(f'{name} exited ({code}); stopping the other services.', flush=True)
                    return code or 1
            time.sleep(0.25)
    except KeyboardInterrupt:
        return 130
    finally:
        for _, process in processes:
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
        for _, process in processes:
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                process.wait()


if __name__ == '__main__':
    raise SystemExit(main())
