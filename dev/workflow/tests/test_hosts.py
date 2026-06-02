"""Diagnostic check for host detection (hosts.Host).

This module is a thin diagnostic over the ``Host`` helper, which depends on
``wxflow`` (``hosts`` imports ``wxflow.YAMLFile``) and on running on one of
the supported HPC platforms. It is guarded with
``pytest.importorskip("wxflow")`` so that test collection stays clean (zero
import/collection errors) whether or not the pinned ``wxflow`` package is
installed in the Verification_Environment.

Host detection only succeeds on a supported platform; when detection is not
possible the check is skipped rather than erroring, so it never breaks suite
collection on a generic host.

Traces to: Requirements 5.3, 5.4
"""

import os
import sys

import pytest

# Guard the wxflow-backed import: skip cleanly when wxflow is absent so
# collection never errors (Req 5.4).
pytest.importorskip("wxflow")

# Make the workflow root importable so ``import hosts`` resolves when this
# file is collected from the tests/ subdirectory.
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from hosts import Host  # noqa: E402


def test_hosts():
    """Detect the current host and print its information.

    Skips when no supported host can be detected so the test collects and
    runs cleanly on hosts outside the supported set.
    """
    print(f'supported hosts are: {", ".join(Host.SUPPORTED_HOSTS)}')

    try:
        host = Host()
    except NotImplementedError as exc:
        pytest.skip(f"No supported host detected: {exc}")

    print('initializing host ...')

    print(f'hostname: {host.machine}')

    print(f'scheduler on host: {host.scheduler}')

    print('host information ...')
    line_separator = '\n'  # \escapes are not allowed inside f-strings
    print(f'{line_separator.join(f"{key}: {host.info[key]}" for key in host.info.keys())}')


if __name__ == '__main__':
    test_hosts()
