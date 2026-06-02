"""Property-based test: Deployment Determinism (Property 1).

Deploys the same configuration twice from the same git state and platform
to two different temporary EXPDIRs, then asserts that the manifest file
hashes section is identical between both deployments.

**Validates: Requirements 3.8**

Traces to: Design Document - Correctness Property 1
  "Same git commit + same config YAML + same platform → EXPDIRs with
   identical manifest file hashes."
"""

from __future__ import annotations

import os
import stat
import sys
import tempfile
from pathlib import Path

import pytest
import yaml
from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import run, PipelineError, SubmodulePolicy

# Committed Submodule_Fixture tree (Req 6.2, 6.7). Resolved relative to this
# test file so it works regardless of the current working directory.
FIXTURE_ROOT = (Path(__file__).resolve().parent / "fixtures" / "submodules")


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _create_minimal_dev_tree(base_path: Path, config_name: str = "test_config.yaml") -> dict:
    """Create a minimal dev/ tree suitable for pipeline.run().

    Returns a dict with 'dev_root', 'config_path', and 'tmp_path' keys.
    """
    dev_root = base_path / "dev"
    dev_root.mkdir(parents=True, exist_ok=True)

    # Create required subdirectories
    (dev_root / "jobs").mkdir(exist_ok=True)
    (dev_root / "scripts").mkdir(exist_ok=True)
    (dev_root / "ush").mkdir(exist_ok=True)
    (dev_root / "parm" / "workflow").mkdir(parents=True, exist_ok=True)
    (dev_root / "parm" / "config" / "gfs").mkdir(parents=True, exist_ok=True)
    (dev_root / "workflow" / "ecflow" / "templates").mkdir(parents=True, exist_ok=True)

    # Create a minimal workflow config
    config = {
        "suite": {
            "name": "test_suite",
            "ecf_home": "{{ EXPDIR }}/ecf",
            "ecf_files": "{{ EXPDIR }}/ecf/scripts",
            "ecf_include": "{{ EXPDIR }}/ecf/include",
        },
        "defaults": {
            "ECF_TRIES": 2,
        },
        "families": [
            {
                "path": "test/family",
                "tasks": [
                    {
                        "name": "task_a",
                        "jjob": "JTEST_TASK_A",
                        "trigger": "",
                    },
                    {
                        "name": "task_b",
                        "jjob": "JTEST_TASK_B",
                        "trigger": "test/family/task_a == complete",
                    },
                ],
            }
        ],
    }

    config_path = dev_root / "parm" / "workflow" / config_name
    config_path.write_text(yaml.dump(config, sort_keys=False))

    # Create a minimal task.ecf.j2 template
    template = """\
%include <head.h>
%include <envsetup.h>
# Task: {{ task.name }} | JJob: {{ task.jjob }}
${EXPDIR}/ush/universal_wrapper.sh {{ task.jjob }}
%include <tail.h>
"""
    (dev_root / "workflow" / "ecflow" / "templates" / "task.ecf.j2").write_text(
        template
    )

    # Create a .git directory to mark repo root
    (base_path / ".git").mkdir(exist_ok=True)

    # Create unconditional artifacts (Req 9.1, 9.2)
    sorc_dir = base_path / "sorc"
    sorc_dir.mkdir(exist_ok=True)
    link_workflow = sorc_dir / "link_workflow.sh"
    link_workflow.write_text("#!/bin/bash\n# link_workflow.sh placeholder\n")
    os.chmod(link_workflow, 0o755)
    ufs_fix_dir = sorc_dir / "ufs_utils.fd" / "fix"
    ufs_fix_dir.mkdir(parents=True, exist_ok=True)
    link_fixdirs = ufs_fix_dir / "link_fixdirs.sh"
    link_fixdirs.write_text("#!/bin/bash\n# link_fixdirs.sh placeholder\n")
    os.chmod(link_fixdirs, 0o755)

    return {
        "tmp_path": base_path,
        "dev_root": dev_root,
        "config_path": config_path,
    }


def _unseal_expdir(expdir: Path) -> None:
    """Remove read-only permissions from a sealed EXPDIR so it can be cleaned up."""
    if not expdir.exists():
        return
    for dirpath, dirnames, filenames in os.walk(expdir):
        dp = Path(dirpath)
        os.chmod(dp, 0o755)
        for fn in filenames:
            os.chmod(dp / fn, 0o644)


def _read_manifest_files_section(expdir: Path) -> dict:
    """Read the 'files' section from manifest.yaml in an EXPDIR.

    Returns the dict of file paths to their sha256/size entries.
    """
    manifest_path = expdir / "manifest.yaml"
    manifest_data = yaml.safe_load(manifest_path.read_text())
    return manifest_data.get("files", {})


# ---------------------------------------------------------------------------
# Property Test: Deployment Determinism (Property 1)
# ---------------------------------------------------------------------------


@given(
    version=st.from_regex(r"v[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}", fullmatch=True),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.function_scoped_fixture],
)
def test_deployment_determinism_property(version):
    """Property 1: Deployment Determinism.

    **Validates: Requirements 3.8**

    Deploy twice from the same git state + config + platform, assert
    manifests have identical file hashes.

    Same git commit + same config YAML + same platform → EXPDIRs with
    identical manifest file hashes in the 'files' section.

    The timestamps and snapshot_ids may differ due to timing, so we
    compare only the 'files' section which contains the SHA-256 hashes
    of all deployed files.
    """
    platform = "HERA"

    with tempfile.TemporaryDirectory() as tmpdir:
        base_path = Path(tmpdir)

        # Create the shared dev tree
        tree = _create_minimal_dev_tree(base_path)

        expdir_1 = base_path / "expdir_1"
        expdir_2 = base_path / "expdir_2"

        try:
            # First deployment
            result_1 = run(
                config=str(tree["config_path"]),
                platform=platform,
                expdir=str(expdir_1),
                version=version,
                submodule_policy=SubmodulePolicy.FIXTURE,
                fixture_root=str(FIXTURE_ROOT),
            )

            # Unseal first EXPDIR so cleanup works
            _unseal_expdir(expdir_1)

            # Second deployment (same config, same platform, same version)
            # Need to remove manifest from first run's check — but we deploy
            # to a different EXPDIR so no conflict
            result_2 = run(
                config=str(tree["config_path"]),
                platform=platform,
                expdir=str(expdir_2),
                version=version,
                submodule_policy=SubmodulePolicy.FIXTURE,
                fixture_root=str(FIXTURE_ROOT),
            )

            # Unseal second EXPDIR so cleanup works
            _unseal_expdir(expdir_2)

            # Read the 'files' section from both manifests
            files_1 = _read_manifest_files_section(expdir_1)
            files_2 = _read_manifest_files_section(expdir_2)

            # Assert the file hashes are identical
            assert files_1 == files_2, (
                "Deployment determinism violated: two deployments from the "
                "same git state + config + platform produced different file "
                f"hashes.\n"
                f"Version: {version}\n"
                f"Platform: {platform}\n"
                f"Files only in first:  {set(files_1.keys()) - set(files_2.keys())}\n"
                f"Files only in second: {set(files_2.keys()) - set(files_1.keys())}\n"
                f"Differing hashes: {[k for k in files_1 if k in files_2 and files_1[k] != files_2[k]]}"
            )

        finally:
            # Ensure cleanup can proceed even if sealed
            _unseal_expdir(expdir_1)
            _unseal_expdir(expdir_2)
