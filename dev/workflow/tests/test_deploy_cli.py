"""Unit tests for the deploy_workflow CLI (deploy.py).

Tests argument parsing, platform validation, Rocoto deprecation guard,
allowlist parsing, and dry-run behavior.

Traces to: Requirements 1.5, 3.1, 12.1
"""

from __future__ import annotations

import os
import sys
from unittest.mock import patch

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deploy import (
    _build_parser,
    _check_rocoto_invocation,
    _parse_allowlist,
    _validate_platform,
    main,
)
from deployment.pipeline import SUPPORTED_PLATFORMS


# ---------------------------------------------------------------------------
# Argument parsing tests
# ---------------------------------------------------------------------------


class TestBuildParser:
    """Tests for the argument parser construction."""

    def test_required_arguments(self):
        """Parser requires --config, --platform, --expdir, --version."""
        parser = _build_parser()

        # All required args present
        args = parser.parse_args([
            "--config", "dev/parm/workflow/gfs_cycled.yaml",
            "--platform", "HERA",
            "--expdir", "/tmp/expdir",
            "--version", "v17.0.0",
        ])
        assert args.config == "dev/parm/workflow/gfs_cycled.yaml"
        assert args.platform == "HERA"
        assert args.expdir == "/tmp/expdir"
        assert args.version == "v17.0.0"

    def test_missing_config_exits(self):
        """Parser exits when --config is missing."""
        parser = _build_parser()
        with pytest.raises(SystemExit):
            parser.parse_args(["--platform", "HERA", "--expdir", "/tmp", "--version", "v1"])

    def test_missing_platform_exits(self):
        """Parser exits when --platform is missing."""
        parser = _build_parser()
        with pytest.raises(SystemExit):
            parser.parse_args(["--config", "c.yaml", "--expdir", "/tmp", "--version", "v1"])

    def test_missing_expdir_exits(self):
        """Parser exits when --expdir is missing."""
        parser = _build_parser()
        with pytest.raises(SystemExit):
            parser.parse_args(["--config", "c.yaml", "--platform", "HERA", "--version", "v1"])

    def test_missing_version_exits(self):
        """Parser exits when --version is missing."""
        parser = _build_parser()
        with pytest.raises(SystemExit):
            parser.parse_args(["--config", "c.yaml", "--platform", "HERA", "--expdir", "/tmp"])

    def test_dry_run_default_false(self):
        """--dry-run defaults to False."""
        parser = _build_parser()
        args = parser.parse_args([
            "--config", "c.yaml",
            "--platform", "HERA",
            "--expdir", "/tmp",
            "--version", "v1",
        ])
        assert args.dry_run is False

    def test_dry_run_flag(self):
        """--dry-run sets dry_run to True."""
        parser = _build_parser()
        args = parser.parse_args([
            "--config", "c.yaml",
            "--platform", "HERA",
            "--expdir", "/tmp",
            "--version", "v1",
            "--dry-run",
        ])
        assert args.dry_run is True

    def test_allowlist_default_none(self):
        """--allowlist defaults to None."""
        parser = _build_parser()
        args = parser.parse_args([
            "--config", "c.yaml",
            "--platform", "HERA",
            "--expdir", "/tmp",
            "--version", "v1",
        ])
        assert args.allowlist is None

    def test_allowlist_value(self):
        """--allowlist accepts a comma-separated string."""
        parser = _build_parser()
        args = parser.parse_args([
            "--config", "c.yaml",
            "--platform", "HERA",
            "--expdir", "/tmp",
            "--version", "v1",
            "--allowlist", "dev/ctests/,dev/ci/",
        ])
        assert args.allowlist == "dev/ctests/,dev/ci/"


# ---------------------------------------------------------------------------
# Platform validation tests
# ---------------------------------------------------------------------------


class TestValidatePlatform:
    """Tests for platform validation (Req 12.1)."""

    def test_valid_platforms_accepted(self):
        """All supported platforms are accepted."""
        for platform in SUPPORTED_PLATFORMS:
            result = _validate_platform(platform)
            assert result == platform

    def test_case_insensitive(self):
        """Platform validation is case-insensitive."""
        assert _validate_platform("hera") == "HERA"
        assert _validate_platform("Wcoss2") == "WCOSS2"
        assert _validate_platform("container") == "CONTAINER"

    def test_invalid_platform_exits(self):
        """Invalid platform causes sys.exit(1)."""
        with pytest.raises(SystemExit) as exc_info:
            _validate_platform("INVALID_PLATFORM")
        assert exc_info.value.code == 1

    def test_empty_platform_exits(self):
        """Empty platform string causes sys.exit(1)."""
        with pytest.raises(SystemExit) as exc_info:
            _validate_platform("")
        assert exc_info.value.code == 1


# ---------------------------------------------------------------------------
# Rocoto deprecation guard tests
# ---------------------------------------------------------------------------


class TestRocotoDeprecation:
    """Tests for Rocoto deprecation FATAL ERROR (Req 1.5)."""

    def test_rocoto_config_path_rejected(self):
        """Config path containing 'rocoto' triggers FATAL ERROR."""
        parser = _build_parser()
        args = parser.parse_args([
            "--config", "dev/workflow/rocoto/gfs_workflow.xml",
            "--platform", "HERA",
            "--expdir", "/tmp/expdir",
            "--version", "v1.0.0",
        ])
        with pytest.raises(SystemExit) as exc_info:
            _check_rocoto_invocation(args)
        assert exc_info.value.code == 1

    def test_rocoto_in_path_case_insensitive(self):
        """Rocoto detection is case-insensitive."""
        parser = _build_parser()
        args = parser.parse_args([
            "--config", "dev/workflow/ROCOTO/gfs.xml",
            "--platform", "HERA",
            "--expdir", "/tmp/expdir",
            "--version", "v1.0.0",
        ])
        with pytest.raises(SystemExit) as exc_info:
            _check_rocoto_invocation(args)
        assert exc_info.value.code == 1

    def test_non_rocoto_config_passes(self):
        """Normal config path does not trigger Rocoto guard."""
        parser = _build_parser()
        args = parser.parse_args([
            "--config", "dev/parm/workflow/gfs_cycled.yaml",
            "--platform", "HERA",
            "--expdir", "/tmp/expdir",
            "--version", "v1.0.0",
        ])
        # Should not raise
        _check_rocoto_invocation(args)


# ---------------------------------------------------------------------------
# Allowlist parsing tests
# ---------------------------------------------------------------------------


class TestParseAllowlist:
    """Tests for allowlist parsing."""

    def test_none_returns_none(self):
        """None input returns None."""
        assert _parse_allowlist(None) is None

    def test_single_item(self):
        """Single item is parsed correctly."""
        assert _parse_allowlist("dev/ctests/") == ["dev/ctests/"]

    def test_multiple_items(self):
        """Comma-separated items are split correctly."""
        result = _parse_allowlist("dev/ctests/,dev/ci/")
        assert result == ["dev/ctests/", "dev/ci/"]

    def test_whitespace_stripped(self):
        """Whitespace around items is stripped."""
        result = _parse_allowlist("dev/ctests/ , dev/ci/ ")
        assert result == ["dev/ctests/", "dev/ci/"]

    def test_empty_string_returns_empty_list(self):
        """Empty string returns empty list."""
        assert _parse_allowlist("") == []

    def test_trailing_comma_ignored(self):
        """Trailing comma does not produce empty item."""
        result = _parse_allowlist("dev/ctests/,")
        assert result == ["dev/ctests/"]


# ---------------------------------------------------------------------------
# Main function integration tests
# ---------------------------------------------------------------------------


class TestMain:
    """Integration tests for the main() entry point."""

    def test_dry_run_success(self, tmp_path):
        """main() returns 0 on successful dry-run."""
        # Create a minimal dev tree
        dev_root = tmp_path / "dev"
        dev_root.mkdir()
        (dev_root / "jobs").mkdir()
        (dev_root / "parm" / "workflow").mkdir(parents=True)
        (tmp_path / ".git").mkdir()

        config = {
            "suite": {"name": "test_suite"},
            "defaults": {"ECF_TRIES": 2},
            "families": [
                {
                    "path": "test/family",
                    "tasks": [
                        {"name": "task_a", "jjob": "JTEST_A", "trigger": ""},
                    ],
                }
            ],
        }
        config_path = dev_root / "parm" / "workflow" / "test.yaml"
        config_path.write_text(yaml.dump(config, sort_keys=False))

        expdir = tmp_path / "expdir"

        result = main([
            "--config", str(config_path),
            "--platform", "HERA",
            "--expdir", str(expdir),
            "--version", "v1.0.0",
            "--dry-run",
        ])

        assert result == 0

    def test_invalid_platform_returns_1(self, tmp_path):
        """main() exits with code 1 for invalid platform."""
        config_path = tmp_path / "config.yaml"
        config_path.write_text("suite:\n  name: test\n")

        with pytest.raises(SystemExit) as exc_info:
            main([
                "--config", str(config_path),
                "--platform", "BADPLATFORM",
                "--expdir", str(tmp_path / "expdir"),
                "--version", "v1.0.0",
            ])
        assert exc_info.value.code == 1

    def test_rocoto_config_returns_nonzero(self):
        """main() exits non-zero for Rocoto config path."""
        with pytest.raises(SystemExit) as exc_info:
            main([
                "--config", "dev/workflow/rocoto/gfs.xml",
                "--platform", "HERA",
                "--expdir", "/tmp/expdir",
                "--version", "v1.0.0",
            ])
        assert exc_info.value.code == 1

    def test_pipeline_error_returns_1(self, tmp_path):
        """main() returns 1 when pipeline raises PipelineError."""
        # Config file doesn't exist — pipeline will raise PipelineError
        result = main([
            "--config", str(tmp_path / "nonexistent.yaml"),
            "--platform", "HERA",
            "--expdir", str(tmp_path / "expdir"),
            "--version", "v1.0.0",
        ])

        assert result == 1

    def test_allowlist_passed_to_pipeline(self, tmp_path):
        """Allowlist is correctly parsed and passed to pipeline.run()."""
        dev_root = tmp_path / "dev"
        dev_root.mkdir()
        (dev_root / "jobs").mkdir()
        (dev_root / "parm" / "workflow").mkdir(parents=True)
        (tmp_path / ".git").mkdir()

        config = {
            "suite": {"name": "test_suite"},
            "defaults": {"ECF_TRIES": 2},
            "families": [
                {
                    "path": "test/family",
                    "tasks": [
                        {"name": "task_a", "jjob": "JTEST_A", "trigger": ""},
                    ],
                }
            ],
        }
        config_path = dev_root / "parm" / "workflow" / "test.yaml"
        config_path.write_text(yaml.dump(config, sort_keys=False))

        expdir = tmp_path / "expdir"

        with patch("deploy.run") as mock_run:
            mock_run.return_value = {"dry_run": True, "snapshot_id": None}
            main([
                "--config", str(config_path),
                "--platform", "HERA",
                "--expdir", str(expdir),
                "--version", "v1.0.0",
                "--allowlist", "dev/ctests/,dev/ci/",
                "--dry-run",
            ])

            mock_run.assert_called_once()
            call_kwargs = mock_run.call_args
            # Check allowlist was parsed and passed
            assert call_kwargs[1]["allowlist"] == ["dev/ctests/", "dev/ci/"]
            # Or positional/keyword depending on call style
            if call_kwargs[1]:
                assert call_kwargs[1].get("allowlist") == ["dev/ctests/", "dev/ci/"]
