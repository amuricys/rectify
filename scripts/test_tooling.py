#!/usr/bin/env python3
"""Offline regression checks for infrastructure dispatch and service lifecycle."""
import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('infra', ROOT / 'scripts/infra.py')
infra = importlib.util.module_from_spec(spec)
spec.loader.exec_module(infra)


class InfrastructureCommands(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name).resolve()
        (self.root / 'workspace.json').write_text('{}')
        (self.root / 'flake.nix').write_text('{}')
        self.generated = self.root / 'generated.json'
        self.generated.write_text('{"resource": {}}')
        self.calls = []
        self.environment = patch.dict(os.environ, {
            'RECTIFY_ROOT': str(self.root),
            'RECTIFY_INFRA_STATE_DIR': str(self.root / 'state'),
            'TF_VAR_frontend_dir': str(self.root / 'dist'),
            'AWS_PROFILE': 'example-profile',
        })
        self.environment.start()

    def tearDown(self):
        self.environment.stop()
        self.temporary.cleanup()

    def execute(self, command, **kwargs):
        self.calls.append((command, kwargs))
        return subprocess.CompletedProcess(command, 0, stdout=str(self.generated) + '\n')

    def run_command(self, *args):
        with patch.object(sys, 'argv', ['infra.py', *args]), patch.object(infra, 'execute', self.execute):
            with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                infra.main()

    def test_render_is_offline_from_terraform_and_preserves_state(self):
        directory = self.root / 'state/backend'
        directory.mkdir(parents=True)
        state = directory / 'terraform.tfstate'
        state.write_text('existing-state')
        self.run_command('backend', 'render')
        self.assertEqual(len(self.calls), 1)
        self.assertEqual(self.calls[0][0][0], 'nix')
        self.assertEqual(state.read_text(), 'existing-state')
        self.assertEqual(json.loads((directory / 'config.tf.json').read_text()), {'resource': {}})

    def test_apply_keeps_confirmation_and_credentials(self):
        self.run_command('backend', 'apply', '-var-file=/tmp/example.tfvars')
        self.assertEqual(self.calls[-2][0][-2:], ['init', '-input=false'])
        self.assertEqual(self.calls[-1][0][-2:], ['apply', '-var-file=/tmp/example.tfvars'])
        self.assertNotIn('-auto-approve', self.calls[-1][0])
        self.assertEqual(self.calls[-1][1]['env']['AWS_PROFILE'], 'example-profile')

    def test_frontend_missing_build_prevents_plan(self):
        with self.assertRaises(SystemExit) as error:
            self.run_command('frontend', 'plan')
        self.assertEqual(error.exception.code, 2)
        self.assertEqual(self.calls, [])

    def test_frontend_path_and_extra_arguments_are_preserved(self):
        (self.root / 'dist').mkdir()
        (self.root / 'dist/index.html').write_text('hello')
        self.run_command('frontend', 'plan', '--', '-out=/tmp/example.tfplan')
        self.assertEqual(self.calls[-1][0][-2:], ['plan', '-out=/tmp/example.tfplan'])
        self.assertEqual(self.calls[-1][1]['env']['TF_VAR_frontend_dir'], str(self.root / 'dist'))

    def test_destroy_does_not_rebuild_or_replace_configuration(self):
        directory = self.root / 'state/frontend'
        directory.mkdir(parents=True)
        (directory / 'config.tf.json').write_text('{}')
        self.run_command('frontend', 'destroy')
        self.assertTrue(all(command[0] == 'terraform' for command, _ in self.calls))


class LocalServices(unittest.TestCase):
    def test_failed_service_stops_siblings(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / 'scripts').mkdir()
            shutil.copy2(ROOT / 'scripts/dev.py', root / 'scripts/dev.py')
            pid_file = root / 'child.pid'
            worker = 'import os,time; from pathlib import Path; Path(' + repr(str(pid_file)) + ').write_text(str(os.getpid())); time.sleep(60)'
            manifest = {'projects': [
                {'id': 'worker', 'path': '.', 'commands': {'run': [sys.executable, '-c', worker]}},
                {'id': 'failure', 'path': '.', 'commands': {'run': [sys.executable, '-c', 'import time; time.sleep(0.5); raise SystemExit(7)']}},
            ]}
            (root / 'workspace.json').write_text(json.dumps(manifest))
            result = subprocess.run([sys.executable, str(root / 'scripts/dev.py'), 'worker', 'failure'], capture_output=True, text=True, timeout=15)
            self.assertEqual(result.returncode, 7, result.stderr)
            self.assertTrue(pid_file.exists())
            with self.assertRaises(ProcessLookupError):
                os.kill(int(pid_file.read_text()), 0)

    def test_unimplemented_service_fails_before_launch(self):
        result = subprocess.run([sys.executable, str(ROOT / 'scripts/dev.py'), 'unison'], capture_output=True, text=True)
        self.assertEqual(result.returncode, 2)
        self.assertIn('no local service command', result.stderr)


if __name__ == '__main__':
    unittest.main()
