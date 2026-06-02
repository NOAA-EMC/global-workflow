"""Unit tests for the deployment pipeline orchestration.

Tests the 8-stage pipeline: validate → build context → render templates →
stage files → generate DAG → EE2 scan → manifest → seal.
"""

from __future__ import annotations

import os
import stat
import sys
import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import (
    PipelineError,
    SUPPORTED_PLATFORMS,
    _compute_sha256,
    _find_dev_root,
    _stage_build_context,
    _stage_validate,
    run,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def minimal_dev_tree(tmp_path):
    """Create a minimal dev/ tree with a valid workflow config."""
    dev_root = tmp_path / "dev"
    dev_root.mkdir()

    # Create required subdirectories
    (dev_root / "jobs").mkdir()
    (dev_root / "scripts").mkdir()
    (dev_root / "ush").mkdir()
    (dev_root / "parm" / "workflow").mkdir(parents=True)
    (dev_root / "parm" / "config" / "gfs").mkdir(parents=True)
    (dev_root / "workflow" / "ecflow" / "templates").mkdir(parents=True)

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

    config_path = dev_root / "parm" / "workflow" / "test_config.yaml"
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
    (tmp_path / ".git").mkdir()

    # Create submodule directories for Stage 4c (submodule copy)
    nexus_dir = tmp_path / "sorc" / "nexus.fd" / "config" / "gocart"
    nexus_dir.mkdir(parents=True)
    (nexus_dir / "NEXUS_Config.rc").write_text("! NEXUS config placeholder\n")

    upp_dir = tmp_path / "sorc" / "upp.fd" / "parm"
    upp_dir.mkdir(parents=True)
    (upp_dir / "params_grib2_tbl_new").write_text("# UPP grib2 table\n")

    # Create unconditional artifact scripts (Req 9.1, 9.2)
    link_workflow = tmp_path / "sorc" / "link_workflow.sh"
    link_workflow.write_text("#!/bin/bash\n# link_workflow.sh placeholder\n")
    os.chmod(link_workflow, 0o755)

    link_fixdirs_dir = tmp_path / "sorc" / "ufs_utils.fd" / "fix"
    link_fixdirs_dir.mkdir(parents=True)
    link_fixdirs = link_fixdirs_dir / "link_fixdirs.sh"
    link_fixdirs.write_text("#!/bin/bash\n# link_fixdirs.sh placeholder\n")
    os.chmod(link_fixdirs, 0o755)

    return {
        "tmp_path": tmp_path,
        "dev_root": dev_root,
        "config_path": config_path,
    }


# ---------------------------------------------------------------------------
# Stage 1: Validate
# ---------------------------------------------------------------------------


class TestStageValidate:
    """Tests for the validate stage."""

    def test_missing_config_file(self, tmp_path):
        """Validate raises PipelineError when config file doesn't exist."""
        with pytest.raises(PipelineError, match="Configuration file not found"):
            _stage_validate(
                config_path=tmp_path / "nonexistent.yaml",
                platform="HERA",
                expdir=tmp_path / "expdir",
                version="v1.0.0",
                dev_root=tmp_path,
            )

    def test_unsupported_platform(self, tmp_path):
        """Validate raises PipelineError for unsupported platform."""
        config = tmp_path / "config.yaml"
        config.write_text("suite:\n  name: test\n")

        with pytest.raises(PipelineError, match="Unsupported platform"):
            _stage_validate(
                config_path=config,
                platform="INVALID_PLATFORM",
                expdir=tmp_path / "expdir",
                version="v1.0.0",
                dev_root=tmp_path,
            )

    def test_sealed_expdir_rejected(self, tmp_path):
        """Validate refuses to overwrite a sealed EXPDIR with manifest."""
        config = tmp_path / "config.yaml"
        config.write_text("suite:\n  name: test\n")

        expdir = tmp_path / "expdir"
        expdir.mkdir()
        manifest = expdir / "manifest.yaml"
        manifest.write_text(
            yaml.dump({"snapshot_id": "v1.0.0+abc123def456"})
        )

        with pytest.raises(PipelineError, match="already published"):
            _stage_validate(
                config_path=config,
                platform="HERA",
                expdir=expdir,
                version="v2.0.0",
                dev_root=tmp_path,
            )

    def test_empty_version_rejected(self, tmp_path):
        """Validate raises PipelineError for empty version string."""
        config = tmp_path / "config.yaml"
        config.write_text("suite:\n  name: test\n")

        with pytest.raises(PipelineError, match="Version string must be non-empty"):
            _stage_validate(
                config_path=config,
                platform="HERA",
                expdir=tmp_path / "expdir",
                version="",
                dev_root=tmp_path,
            )

    def test_valid_inputs_pass(self, tmp_path):
        """Validate succeeds with valid inputs."""
        config = tmp_path / "config.yaml"
        config.write_text("suite:\n  name: test\n")

        # Should not raise
        _stage_validate(
            config_path=config,
            platform="HERA",
            expdir=tmp_path / "expdir",
            version="v1.0.0",
            dev_root=tmp_path,
        )

    def test_all_platforms_accepted(self, tmp_path):
        """Validate accepts all supported platforms."""
        config = tmp_path / "config.yaml"
        config.write_text("suite:\n  name: test\n")

        for platform in SUPPORTED_PLATFORMS:
            _stage_validate(
                config_path=config,
                platform=platform,
                expdir=tmp_path / "expdir",
                version="v1.0.0",
                dev_root=tmp_path,
            )


# ---------------------------------------------------------------------------
# Stage 2: Build Context
# ---------------------------------------------------------------------------


class TestStageBuildContext:
    """Tests for the build context stage."""

    def test_context_contains_platform(self, tmp_path):
        """Context includes the platform name."""
        config = tmp_path / "config.yaml"
        config.write_text(yaml.dump({
            "suite": {"name": "test"},
            "defaults": {"ECF_TRIES": 2},
        }))

        dev_root = tmp_path / "dev"
        dev_root.mkdir()

        context = _stage_build_context(
            config_path=config,
            platform="HERA",
            version="v1.0.0",
            expdir=tmp_path / "expdir",
            dev_root=dev_root,
        )

        assert context["MACHINE"] == "HERA"
        assert context["model_ver"] == "v1.0.0"
        assert context["EXPDIR"] == str(tmp_path / "expdir")

    def test_context_merges_defaults(self, tmp_path):
        """Context includes defaults from the config YAML."""
        config = tmp_path / "config.yaml"
        config.write_text(yaml.dump({
            "suite": {"name": "test"},
            "defaults": {"ECF_TRIES": 3, "MY_VAR": "hello"},
        }))

        dev_root = tmp_path / "dev"
        dev_root.mkdir()

        context = _stage_build_context(
            config_path=config,
            platform="WCOSS2",
            version="v2.0.0",
            expdir=tmp_path / "expdir",
            dev_root=dev_root,
        )

        assert context["ECF_TRIES"] == 3
        assert context["MY_VAR"] == "hello"


# ---------------------------------------------------------------------------
# Pipeline run() integration
# ---------------------------------------------------------------------------


class TestPipelineRun:
    """Integration tests for the full pipeline run."""

    def test_dry_run_validates_only(self, minimal_dev_tree):
        """Dry-run mode validates inputs without writing files."""
        info = minimal_dev_tree
        expdir = info["tmp_path"] / "expdir"

        result = run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v1.0.0",
            dry_run=True,
        )

        assert result["dry_run"] is True
        assert result["snapshot_id"] is None
        # EXPDIR should not be created in dry-run
        assert not expdir.exists()

    def test_dry_run_fails_on_invalid_platform(self, minimal_dev_tree):
        """Dry-run still validates platform."""
        info = minimal_dev_tree
        expdir = info["tmp_path"] / "expdir"

        with pytest.raises(PipelineError, match="Unsupported platform"):
            run(
                config=str(info["config_path"]),
                platform="INVALID",
                expdir=str(expdir),
                version="v1.0.0",
                dry_run=True,
            )

    def test_full_pipeline_creates_expdir(self, minimal_dev_tree):
        """Full pipeline creates a sealed EXPDIR with manifest."""
        info = minimal_dev_tree
        expdir = info["tmp_path"] / "expdir"

        result = run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v1.0.0",
        )

        assert result["dry_run"] is False
        assert result["snapshot_id"] is not None
        assert result["snapshot_id"].startswith("v1.0.0+")
        assert expdir.exists()

        # Manifest should exist
        manifest_path = expdir / "manifest.yaml"
        assert manifest_path.exists()

        # Provenance should exist
        provenance_path = expdir / "workflow" / "provenance.yaml"
        assert provenance_path.exists()

        # DAG .def file should exist
        def_path = expdir / "ecf" / "defs" / "test_suite.def"
        assert def_path.exists()

    def test_full_pipeline_seals_files(self, minimal_dev_tree):
        """Full pipeline sets files to read-only (0444)."""
        info = minimal_dev_tree
        expdir = info["tmp_path"] / "expdir"

        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v1.0.0",
        )

        # Check that files are read-only
        manifest_path = expdir / "manifest.yaml"
        mode = stat.S_IMODE(manifest_path.stat().st_mode)
        assert mode == 0o444

    def test_pipeline_refuses_redeployment(self, minimal_dev_tree):
        """Pipeline refuses to deploy to an already-sealed EXPDIR."""
        info = minimal_dev_tree
        expdir = info["tmp_path"] / "expdir"

        # First deployment
        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v1.0.0",
        )

        # Second deployment should fail (need to unseal for the test to
        # even read the manifest - but the validate stage checks existence)
        # Reset permissions so we can test the logic
        os.chmod(expdir, 0o755)
        for f in expdir.rglob("*"):
            if f.is_file():
                os.chmod(f, 0o644)
            elif f.is_dir():
                os.chmod(f, 0o755)

        with pytest.raises(PipelineError, match="already published"):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v2.0.0",
            )


# ---------------------------------------------------------------------------
# Helper function tests
# ---------------------------------------------------------------------------


class TestHelpers:
    """Tests for pipeline helper functions."""

    def test_compute_sha256(self, tmp_path):
        """SHA-256 computation produces correct hash."""
        test_file = tmp_path / "test.txt"
        test_file.write_text("hello world\n")

        expected = (
            "a948904f2f0f479b8f8564e9d7d0346638"
            "34d7cf273b7657ba9ae3c8f4a7e5b"
        )
        # Correct expected hash for "hello world\n"
        import hashlib
        expected = hashlib.sha256(b"hello world\n").hexdigest()

        assert _compute_sha256(test_file) == expected

    def test_find_dev_root(self, minimal_dev_tree):
        """_find_dev_root locates the dev/ directory from a config path."""
        info = minimal_dev_tree
        dev_root = _find_dev_root(info["config_path"])
        assert dev_root == info["dev_root"]

    def test_find_dev_root_fails_gracefully(self, tmp_path):
        """_find_dev_root raises PipelineError when dev/ not found."""
        config = tmp_path / "orphan" / "config.yaml"
        config.parent.mkdir()
        config.write_text("test")

        with pytest.raises(PipelineError, match="Cannot determine dev/ root"):
            _find_dev_root(config)

    def test_supported_platforms_complete(self):
        """All expected platforms are in the supported set."""
        expected = {
            "WCOSS2", "HERA", "HERCULES", "ORION", "GAEAC6",
            "DERECHO", "URSA", "AWSPW", "AZUREPW", "GOOGLEPW",
            "CONTAINER",
        }
        assert SUPPORTED_PLATFORMS == expected
