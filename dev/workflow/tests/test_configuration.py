"""Diagnostic check for the wxflow Configuration parser.

This module is a thin diagnostic over ``wxflow.Configuration``. It is
guarded with ``pytest.importorskip("wxflow")`` so that test collection
stays clean (zero import/collection errors) whether or not the pinned
``wxflow`` package is installed in the Verification_Environment.

When ``wxflow`` is present and an experiment directory containing
``config.base`` is provided (via the ``EXPDIR`` environment variable or a
command-line argument when run as a script), the configuration is parsed
and printed for real. Otherwise the check is skipped rather than erroring,
so it never breaks suite collection on a generic host.

Traces to: Requirements 5.3, 5.4
"""

import os
import sys

import pytest

# Guard the wxflow import: skip cleanly when the pinned package is absent
# so collection never errors (Req 5.4).
wxflow = pytest.importorskip("wxflow")
from wxflow import Configuration  # noqa: E402


def _resolve_expdir():
    """Resolve the experiment directory from env or argv, if available."""
    expdir = os.environ.get("EXPDIR")
    if not expdir and len(sys.argv) > 1:
        expdir = sys.argv[1]
    return expdir


def test_configuration():
    """Parse and print a config.base-bearing experiment directory.

    Skips when no usable experiment directory is available so the test
    collects and runs cleanly on hosts without a staged config.base.
    """
    expdir = _resolve_expdir()
    if not expdir or not os.path.isdir(expdir):
        pytest.skip(
            "No experiment directory with config.base available "
            "(set EXPDIR to run this diagnostic)."
        )

    cfg = Configuration(expdir)

    print(f'experiment dir: {cfg.config_dir}')

    print('configuration files ...')
    line_separator = '\n'  # \escapes are not allowed inside f-strings
    print(f'{line_separator.join(cfg.config_files)}')

    try:
        base_config = cfg.find_config("config.base")
    except Exception as exc:  # wxflow raises UnknownConfigError
        pytest.skip(f"config.base not found in {expdir}: {exc}")

    print(f'config.base: {base_config}')

    print('*' * 80)
    print('config.base ...')
    base = cfg.parse_config('config.base')
    cfg.print_config('config.base')
    print(type(base))
    print(base.HOMEglobal)

    print('*' * 80)
    print('config.anal...')
    cfg.print_config(['config.base', 'config.anal'])

    print('*' * 80)
    print('config.efcs ...')
    configs = ['config.base', 'config.fcst', 'config.efcs']
    cfg.print_config(configs)


if __name__ == '__main__':
    test_configuration()
