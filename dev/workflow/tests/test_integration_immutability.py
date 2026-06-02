"""Integration test for EXPDIR immutability (Property 3).

After sealing, no regular file in EXPDIR is writable by non-owner.
Verifies that:
  - All regular files have mode 0444 (read-only)
  - All directories have mode 0555 (read-only)
  - Attempting to write to any file raises PermissionError (EPERM)
  - Attempting to create a new file in the EXPDIR raises PermissionError
  - Attempting to delete a file raises PermissionError
  - The pipeline refuses to re-deploy to an already-sealed EXPDIR

Validates: Requirements 3.4
"""

from __future__ import annotations

import os
import stat
import sys
from pathlib import Path

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import PipelineError, SubmodulePolicy, run

# Committed Submodule_Fixture tree (Req 6.2, 6.7). Resolved relative to this
# test file so it works regardless of the current working directory.
FIXTURE_ROOT = (Path(__file__).resolve().parent / "fixtures" / "submodules")


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def minimal_dev_tree(tmp_path):
    """Create a minimal dev/ tree that mirrors the real structure.

    This fixture creates a self-contained dev/ tree with just enough
    content to exercise the full pipeline without depending on the
    full repository content (which may have missing optional dirs).
    """
    dev_root = tmp_path / "dev"
    dev_root.mkdir()

    # Create required subdirectories
    (dev_root / "jobs").mkdir()
    (dev_root / "scripts").mkdir()
    (dev_root / "ush").mkdir()
    (dev_root / "parm" / "workflow").mkdir(parents=True)
    (dev_root / "parm" / "config" / "gfs_forecast_only").mkdir(parents=True)
    (dev_root / "sorc").mkdir()
    (dev_root / "versions").mkdir()
    (dev_root / "modulefiles").mkdir()
    (dev_root / "env").mkdir()
    (dev_root / "workflow" / "ecflow" / "templates").mkdir(parents=True)
    (dev_root / "workflow" / "ecflow" / "include").mkdir(parents=True)

    # Create the gfs_forecast_only.yaml config
    config = {
        "suite": {
            "name": "gfs_v17_fcst_only",
            "ecf_home": "{{ EXPDIR }}/ecf",
            "ecf_files": "{{ EXPDIR }}/ecf/scripts",
            "ecf_include": "{{ EXPDIR }}/ecf/include",
        },
        "defaults": {
            "ECF_TRIES": 2,
            "ECF_JOB_CMD": "uwtools submit %ECF_JOB% %ECF_JOBOUT%",
        },
        "cycles": [
            {
                "name": "gfs",
                "repeat": {
                    "type": "date",
                    "variable": "YMD",
                    "start": "{{ idate }}",
                    "end": "{{ edate }}",
                    "step": 1,
                },
                "time": "00:00 06:00 12:00 18:00",
            }
        ],
        "families": [
            {
                "path": "gfs/atmos/stage",
                "tasks": [
                    {
                        "name": "stage_ic",
                        "trigger": "",
                        "jjob": "JGLOBAL_STAGE_IC",
                    }
                ],
            },
            {
                "path": "gfs/atmos/forecast",
                "tasks": [
                    {
                        "name": "fcst",
                        "trigger": "gfs/atmos/stage/stage_ic == complete",
                        "jjob": "JGLOBAL_FORECAST",
                        "events": ["forecast_hour"],
                        "meters": [
                            {"name": "forecast_hour", "min": 0, "max": 120}
                        ],
                    }
                ],
            },
            {
                "path": "gfs/atmos/post",
                "tasks": [
                    {
                        "name": "post_f000",
                        "trigger": "gfs/atmos/forecast/fcst:forecast_hour ge 0",
                        "jjob": "JGFS_ATMOS_POST",
                        "variables": {"FHOUR": "0"},
                    },
                ],
            },
            {
                "path": "gfs/atmos/archive",
                "tasks": [
                    {
                        "name": "arch",
                        "trigger": "gfs/atmos/post/post_f000 == complete",
                        "jjob": "JGLOBAL_ARCHIVE",
                    }
                ],
            },
        ],
        "inter_cycle_dependencies": [],
    }

    config_path = dev_root / "parm" / "workflow" / "gfs_forecast_only.yaml"
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

    # Create ecFlow include files
    (dev_root / "workflow" / "ecflow" / "include" / "head.h").write_text(
        "# head.h - ecFlow header\n"
    )
    (dev_root / "workflow" / "ecflow" / "include" / "tail.h").write_text(
        "# tail.h - ecFlow tail\n"
    )
    (dev_root / "workflow" / "ecflow" / "include" / "envsetup.h").write_text(
        "# envsetup.h - environment setup\n"
    )

    # Create sample J-Jobs (EE2 compliant)
    jjob_template = (
        "#!/bin/bash\n"
        "# J-Job: {name}\n"
        ". ${{HOMEgfs}}/ush/jjob_header.sh\n"
        "export DATA=${{DATAROOT}}/${{jobid}}\n"
        "export cycle=t${{cyc}}z\n"
        "export PDY=${{PDY}}\n"
        "export NET=${{NET}}\n"
        "export RUN=${{RUN}}\n"
        "export COMIN=${{COMROOT}}/${{NET}}/${{model_ver}}/${{RUN}}.${{PDY}}/${{cyc}}/atmos\n"
        "export COMOUT=${{COMROOT}}/${{NET}}/${{model_ver}}/${{RUN}}.${{PDY}}/${{cyc}}/atmos\n"
        "export pgmout=OUTPUT.$$\n"
        "export jobid=${{job}}.$$\n"
        "exit 0\n"
    )
    for jjob_name in ["JGLOBAL_FORECAST", "JGLOBAL_STAGE_IC",
                      "JGFS_ATMOS_POST", "JGLOBAL_ARCHIVE"]:
        (dev_root / "jobs" / jjob_name).write_text(
            jjob_template.format(name=jjob_name)
        )

    # Create a sample ex-script
    (dev_root / "scripts" / "exglobal_forecast.sh").write_text(
        "#!/bin/bash\n# Ex-script: exglobal_forecast.sh\nexit 0\n"
    )

    # Create a sample ush utility
    (dev_root / "ush" / "detect_machine.sh").write_text(
        "#!/bin/bash\n# detect_machine.sh\nexport MACHINE=HERA\n"
    )

    # Create a versions file
    (dev_root / "versions" / "run.ver").write_text(
        "export gfs_ver=v17.0.0\n"
    )

    # Create a .git directory to mark repo root
    (tmp_path / ".git").mkdir()

    # Create unconditional artifacts (Req 9.1, 9.2)
    sorc_dir = tmp_path / "sorc"
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
        "tmp_path": tmp_path,
        "dev_root": dev_root,
        "config_path": config_path,
    }


@pytest.fixture
def sealed_expdir(minimal_dev_tree):
    """Deploy a workflow and return the sealed EXPDIR path.

    Runs the full pipeline to produce a sealed, immutable EXPDIR
    that can be used to verify immutability properties.
    """
    info = minimal_dev_tree
    expdir = info["tmp_path"] / "EXPDIR"

    run(
        config=str(info["config_path"]),
        platform="HERA",
        expdir=str(expdir),
        version="v17.0.0",
        submodule_policy=SubmodulePolicy.FIXTURE,
        fixture_root=str(FIXTURE_ROOT),
    )

    return expdir


# ---------------------------------------------------------------------------
# Integration Tests: Immutability (Property 3)
# ---------------------------------------------------------------------------


class TestImmutabilityFilePermissions:
    """Verify all regular files are sealed to mode 0444 after deployment."""

    def test_all_regular_files_are_mode_0444(self, sealed_expdir):
        """Every regular file in the sealed EXPDIR must have mode 0444.

        Validates: Requirements 3.4
        """
        for filepath in sealed_expdir.rglob("*"):
            if filepath.is_file() and not filepath.is_symlink():
                mode = stat.S_IMODE(filepath.stat().st_mode)
                assert mode == 0o444, (
                    f"File '{filepath.relative_to(sealed_expdir)}' has mode "
                    f"{oct(mode)}, expected 0o444"
                )

    def test_all_directories_are_mode_0555(self, sealed_expdir):
        """Every directory in the sealed EXPDIR must have mode 0555.

        Validates: Requirements 3.4
        """
        # Check the EXPDIR root itself
        root_mode = stat.S_IMODE(sealed_expdir.stat().st_mode)
        assert root_mode == 0o555, (
            f"EXPDIR root has mode {oct(root_mode)}, expected 0o555"
        )

        # Check all subdirectories
        for dirpath in sealed_expdir.rglob("*"):
            if dirpath.is_dir():
                mode = stat.S_IMODE(dirpath.stat().st_mode)
                assert mode == 0o555, (
                    f"Directory '{dirpath.relative_to(sealed_expdir)}' has mode "
                    f"{oct(mode)}, expected 0o555"
                )


class TestImmutabilityWriteAttempts:
    """Verify that write operations to a sealed EXPDIR raise PermissionError."""

    def test_write_to_existing_file_raises_permission_error(self, sealed_expdir):
        """Attempting to write to any sealed file must raise PermissionError.

        Validates: Requirements 3.4
        """
        # Find a regular file to attempt writing to
        target = sealed_expdir / "manifest.yaml"
        assert target.exists(), "manifest.yaml should exist in sealed EXPDIR"

        with pytest.raises(PermissionError):
            target.write_text("tampered content")

    def test_append_to_existing_file_raises_permission_error(self, sealed_expdir):
        """Attempting to append to any sealed file must raise PermissionError.

        Validates: Requirements 3.4
        """
        target = sealed_expdir / "manifest.yaml"
        assert target.exists()

        with pytest.raises(PermissionError):
            with open(target, "a") as f:
                f.write("appended content")

    def test_create_new_file_in_sealed_dir_raises_permission_error(
        self, sealed_expdir
    ):
        """Attempting to create a new file in a sealed directory must raise
        PermissionError.

        Validates: Requirements 3.4
        """
        new_file = sealed_expdir / "unauthorized_file.txt"

        with pytest.raises(PermissionError):
            new_file.write_text("should not be created")

    def test_create_new_file_in_subdirectory_raises_permission_error(
        self, sealed_expdir
    ):
        """Attempting to create a new file in a sealed subdirectory must
        raise PermissionError.

        Validates: Requirements 3.4
        """
        # Try creating a file in the jobs/ subdirectory
        jobs_dir = sealed_expdir / "jobs"
        if jobs_dir.exists():
            new_file = jobs_dir / "JUNAUTHORIZED_JOB"
            with pytest.raises(PermissionError):
                new_file.write_text("should not be created")

    def test_delete_file_raises_permission_error(self, sealed_expdir):
        """Attempting to delete a file from a sealed EXPDIR must raise
        PermissionError.

        Validates: Requirements 3.4
        """
        target = sealed_expdir / "manifest.yaml"
        assert target.exists()

        with pytest.raises(PermissionError):
            target.unlink()

    def test_delete_file_via_os_remove_raises_permission_error(
        self, sealed_expdir
    ):
        """Attempting to delete a file via os.remove must raise
        PermissionError (directory is not writable).

        Validates: Requirements 3.4
        """
        target = sealed_expdir / "manifest.yaml"
        assert target.exists()

        with pytest.raises(PermissionError):
            os.remove(target)

    def test_write_to_job_file_raises_permission_error(self, sealed_expdir):
        """Attempting to write to a staged J-Job file must raise
        PermissionError.

        Validates: Requirements 3.4
        """
        jobs_dir = sealed_expdir / "jobs"
        if jobs_dir.exists():
            job_files = list(jobs_dir.iterdir())
            assert len(job_files) > 0, "Expected at least one J-Job file"

            target = job_files[0]
            with pytest.raises(PermissionError):
                target.write_text("echo x >> tampered")


class TestImmutabilityRedeployGuard:
    """Verify the pipeline refuses to re-deploy to an already-sealed EXPDIR."""

    def test_redeploy_to_sealed_expdir_raises_pipeline_error(
        self, minimal_dev_tree
    ):
        """The pipeline must refuse to deploy to an EXPDIR that already
        contains a manifest (immutability guard).

        This confirms that once an EXPDIR is published, it cannot be
        overwritten by a subsequent deployment.

        Validates: Requirements 3.4
        """
        info = minimal_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        # First deployment succeeds
        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            submodule_policy=SubmodulePolicy.FIXTURE,
            fixture_root=str(FIXTURE_ROOT),
        )

        # Unseal permissions so the pipeline can read the manifest
        # (the validate stage reads manifest.yaml to check for existing
        # deployments — it needs read access which 0444/0555 provides)
        # The pipeline should detect the existing manifest and refuse.

        # Second deployment must fail with PipelineError
        with pytest.raises(PipelineError, match="already published"):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v18.0.0",
                submodule_policy=SubmodulePolicy.FIXTURE,
                fixture_root=str(FIXTURE_ROOT),
            )

    def test_redeploy_error_references_snapshot_id(self, minimal_dev_tree):
        """The re-deploy error message must reference the existing Snapshot_ID.

        Validates: Requirements 3.4
        """
        info = minimal_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        # First deployment
        result = run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            submodule_policy=SubmodulePolicy.FIXTURE,
            fixture_root=str(FIXTURE_ROOT),
        )
        snapshot_id = result["snapshot_id"]

        # Second deployment should fail referencing the snapshot_id
        with pytest.raises(PipelineError) as exc_info:
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v18.0.0",
                submodule_policy=SubmodulePolicy.FIXTURE,
                fixture_root=str(FIXTURE_ROOT),
            )

        # The error message should contain the existing snapshot_id
        assert snapshot_id in str(exc_info.value), (
            f"Error message should reference existing Snapshot_ID "
            f"'{snapshot_id}', got: {exc_info.value}"
        )
