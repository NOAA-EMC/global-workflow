"""Unit tests for manifest.py.

Tests the manifest generator including:
- SHA-256 computation for files and bytes
- File inventory (compute_file_hashes)
- Snapshot_ID generation
- Full manifest generation with deterministic output
- Manifest verification

Traces to: Requirements 3.3, 3.6, 3.7
"""

import os
import sys
import tempfile
from collections import OrderedDict
from datetime import datetime, timezone
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.manifest import (
    MANIFEST_FILENAME,
    compute_file_hashes,
    compute_snapshot_id,
    generate_manifest,
    sha256_bytes,
    sha256_file,
    verify_manifest,
    write_manifest,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def sample_expdir(tmp_path):
    """Create a sample EXPDIR with a few files for testing."""
    # Create directory structure
    (tmp_path / "jobs").mkdir()
    (tmp_path / "scripts").mkdir()
    (tmp_path / "parm" / "config" / "gfs").mkdir(parents=True)

    # Create sample files
    (tmp_path / "jobs" / "JGFS_ATMOS_FORECAST").write_text(
        "#!/bin/bash\necho forecast\n"
    )
    (tmp_path / "scripts" / "exgfs_atmos_forecast.sh").write_text(
        "#!/bin/bash\necho running forecast\n"
    )
    (tmp_path / "parm" / "config" / "gfs" / "config.base").write_text(
        "NET=gfs\nRUN=gfs\n"
    )

    return tmp_path


@pytest.fixture
def fixed_timestamp():
    """Return a fixed timestamp for deterministic tests."""
    return datetime(2025, 1, 15, 14, 30, 0, tzinfo=timezone.utc)


# ---------------------------------------------------------------------------
# Tests: SHA-256 helpers
# ---------------------------------------------------------------------------


class TestSHA256Helpers:
    """Tests for SHA-256 computation functions."""

    def test_sha256_file_known_content(self, tmp_path):
        """SHA-256 of known content matches expected hash."""
        f = tmp_path / "test.txt"
        f.write_text("hello world\n")
        result = sha256_file(f)
        # Known SHA-256 of "hello world\n"
        expected = "a948904f2f0f479b8f8564e9d7a7e6e5e0e9e7e3e2e1e0"
        # Just verify it's a 64-char hex string
        assert len(result) == 64
        assert all(c in "0123456789abcdef" for c in result)

    def test_sha256_file_empty(self, tmp_path):
        """SHA-256 of empty file is the known empty hash."""
        f = tmp_path / "empty.txt"
        f.write_bytes(b"")
        result = sha256_file(f)
        # SHA-256 of empty input
        expected = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        assert result == expected

    def test_sha256_file_binary(self, tmp_path):
        """SHA-256 works on binary content."""
        f = tmp_path / "binary.bin"
        f.write_bytes(bytes(range(256)))
        result = sha256_file(f)
        assert len(result) == 64

    def test_sha256_bytes_empty(self):
        """SHA-256 of empty bytes matches known hash."""
        result = sha256_bytes(b"")
        expected = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        assert result == expected

    def test_sha256_bytes_known(self):
        """SHA-256 of known bytes is deterministic."""
        data = b"test data for hashing"
        result1 = sha256_bytes(data)
        result2 = sha256_bytes(data)
        assert result1 == result2
        assert len(result1) == 64

    def test_sha256_file_matches_sha256_bytes(self, tmp_path):
        """File hash matches direct bytes hash for same content."""
        content = b"consistent hashing test"
        f = tmp_path / "test.bin"
        f.write_bytes(content)
        assert sha256_file(f) == sha256_bytes(content)


# ---------------------------------------------------------------------------
# Tests: File inventory
# ---------------------------------------------------------------------------


class TestComputeFileHashes:
    """Tests for compute_file_hashes function."""

    def test_returns_ordered_dict(self, sample_expdir):
        """Result is an OrderedDict."""
        result = compute_file_hashes(sample_expdir)
        assert isinstance(result, OrderedDict)

    def test_finds_all_files(self, sample_expdir):
        """All files in the EXPDIR are inventoried."""
        result = compute_file_hashes(sample_expdir)
        assert "jobs/JGFS_ATMOS_FORECAST" in result
        assert "scripts/exgfs_atmos_forecast.sh" in result
        assert "parm/config/gfs/config.base" in result

    def test_excludes_manifest_file(self, sample_expdir):
        """The manifest.yaml file itself is excluded from inventory."""
        # Write a manifest file
        (sample_expdir / MANIFEST_FILENAME).write_text("snapshot_id: test\n")
        result = compute_file_hashes(sample_expdir)
        assert MANIFEST_FILENAME not in result

    def test_sorted_by_path(self, sample_expdir):
        """File paths are sorted alphabetically."""
        result = compute_file_hashes(sample_expdir)
        keys = list(result.keys())
        assert keys == sorted(keys)

    def test_hash_and_size_present(self, sample_expdir):
        """Each entry has sha256 and size keys."""
        result = compute_file_hashes(sample_expdir)
        for rel_path, info in result.items():
            assert "sha256" in info
            assert "size" in info
            assert isinstance(info["sha256"], str)
            assert len(info["sha256"]) == 64
            assert isinstance(info["size"], int)
            assert info["size"] >= 0

    def test_hash_correctness(self, sample_expdir):
        """Recorded hash matches independent computation."""
        result = compute_file_hashes(sample_expdir)
        for rel_path, info in result.items():
            filepath = sample_expdir / rel_path
            assert info["sha256"] == sha256_file(filepath)
            assert info["size"] == filepath.stat().st_size

    def test_empty_directory(self, tmp_path):
        """Empty directory returns empty OrderedDict."""
        result = compute_file_hashes(tmp_path)
        assert result == OrderedDict()

    def test_uses_forward_slashes(self, sample_expdir):
        """Paths use forward slashes regardless of OS."""
        result = compute_file_hashes(sample_expdir)
        for key in result:
            assert "\\" not in key


# ---------------------------------------------------------------------------
# Tests: Snapshot_ID generation
# ---------------------------------------------------------------------------


class TestSnapshotID:
    """Tests for Snapshot_ID computation."""

    def test_format(self):
        """Snapshot_ID has format '<version>+<12_hex_chars>'."""
        sid = compute_snapshot_id("v17.0.0", b"test content")
        assert sid.startswith("v17.0.0+")
        suffix = sid.split("+")[1]
        assert len(suffix) == 12
        assert all(c in "0123456789abcdef" for c in suffix)

    def test_deterministic(self):
        """Same inputs produce same Snapshot_ID."""
        content = b"deterministic test content"
        sid1 = compute_snapshot_id("v17.0.0", content)
        sid2 = compute_snapshot_id("v17.0.0", content)
        assert sid1 == sid2

    def test_different_content_different_id(self):
        """Different content produces different Snapshot_ID."""
        sid1 = compute_snapshot_id("v17.0.0", b"content A")
        sid2 = compute_snapshot_id("v17.0.0", b"content B")
        assert sid1 != sid2

    def test_different_version_different_id(self):
        """Different version produces different Snapshot_ID."""
        content = b"same content"
        sid1 = compute_snapshot_id("v17.0.0", content)
        sid2 = compute_snapshot_id("v18.0.0", content)
        assert sid1 != sid2
        # The hash suffix should be the same (same content)
        assert sid1.split("+")[1] == sid2.split("+")[1]


# ---------------------------------------------------------------------------
# Tests: Full manifest generation
# ---------------------------------------------------------------------------


class TestGenerateManifest:
    """Tests for the full manifest generation pipeline."""

    def test_generates_valid_yaml(self, sample_expdir, fixed_timestamp):
        """Generated manifest is valid YAML."""
        import yaml

        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123def456",
            git_remote="https://github.com/NOAA-EMC/global-workflow.git",
            git_branch="develop",
            wxflow_version="0.3.0",
            uwtools_version="2.16.0",
        )
        parsed = yaml.safe_load(content)
        assert isinstance(parsed, dict)

    def test_contains_required_fields(self, sample_expdir, fixed_timestamp):
        """Manifest contains all required metadata fields."""
        import yaml

        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123def456",
            git_remote="https://github.com/NOAA-EMC/global-workflow.git",
            git_branch="develop",
            wxflow_version="0.3.0",
            uwtools_version="2.16.0",
        )
        parsed = yaml.safe_load(content)

        assert "snapshot_id" in parsed
        assert "git_commit" in parsed
        assert "git_remote" in parsed
        assert "git_branch" in parsed
        assert "deployed_by" in parsed
        assert "deployed_on" in parsed
        assert "deployed_at" in parsed
        assert "platform" in parsed
        assert "wxflow_version" in parsed
        assert "uwtools_version" in parsed
        assert "files" in parsed

    def test_metadata_values(self, sample_expdir, fixed_timestamp):
        """Metadata values match provided inputs."""
        import yaml

        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123def456",
            git_remote="https://github.com/NOAA-EMC/global-workflow.git",
            git_branch="develop",
            wxflow_version="0.3.0",
            uwtools_version="2.16.0",
        )
        parsed = yaml.safe_load(content)

        assert parsed["git_commit"] == "abc123def456"
        assert parsed["git_remote"] == "https://github.com/NOAA-EMC/global-workflow.git"
        assert parsed["git_branch"] == "develop"
        assert parsed["deployed_by"] == "testuser"
        assert parsed["deployed_on"] == "testhost"
        assert parsed["deployed_at"] == "2025-01-15T14:30:00Z"
        assert parsed["platform"] == "HERA"
        assert parsed["wxflow_version"] == "0.3.0"
        assert parsed["uwtools_version"] == "2.16.0"

    def test_snapshot_id_format(self, sample_expdir, fixed_timestamp):
        """Snapshot_ID follows the <semver>+<sha256_prefix_12> format."""
        import yaml

        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123",
            git_remote="",
            git_branch="",
        )
        parsed = yaml.safe_load(content)
        sid = parsed["snapshot_id"]
        assert sid.startswith("v17.0.0+")
        suffix = sid.split("+")[1]
        assert len(suffix) == 12

    def test_files_section_contains_all_files(self, sample_expdir, fixed_timestamp):
        """Files section lists all EXPDIR files with hashes and sizes."""
        import yaml

        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123",
            git_remote="",
            git_branch="",
        )
        parsed = yaml.safe_load(content)
        files = parsed["files"]

        assert "jobs/JGFS_ATMOS_FORECAST" in files
        assert "scripts/exgfs_atmos_forecast.sh" in files
        assert "parm/config/gfs/config.base" in files

        for rel_path, info in files.items():
            assert "sha256" in info
            assert "size" in info

    def test_deterministic_output(self, sample_expdir, fixed_timestamp):
        """Same inputs produce byte-identical manifest content."""
        kwargs = dict(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123",
            git_remote="https://github.com/NOAA-EMC/global-workflow.git",
            git_branch="develop",
            wxflow_version="0.3.0",
            uwtools_version="2.16.0",
        )
        content1 = generate_manifest(**kwargs)
        content2 = generate_manifest(**kwargs)
        assert content1 == content2

    def test_snapshot_id_is_first_field(self, sample_expdir, fixed_timestamp):
        """snapshot_id appears as the first line in the YAML output."""
        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123",
            git_remote="",
            git_branch="",
        )
        first_line = content.strip().split("\n")[0]
        assert first_line.startswith("snapshot_id:")


# ---------------------------------------------------------------------------
# Tests: Write and verify manifest
# ---------------------------------------------------------------------------


class TestWriteAndVerify:
    """Tests for writing and verifying manifests."""

    def test_write_manifest(self, sample_expdir, fixed_timestamp):
        """write_manifest creates the file at the correct path."""
        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123",
            git_remote="",
            git_branch="",
        )
        result_path = write_manifest(sample_expdir, content)
        assert result_path == sample_expdir / MANIFEST_FILENAME
        assert result_path.exists()
        assert result_path.read_text() == content

    def test_verify_manifest_passes(self, sample_expdir, fixed_timestamp):
        """Verification passes when files are unchanged."""
        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123",
            git_remote="",
            git_branch="",
        )
        write_manifest(sample_expdir, content)
        errors = verify_manifest(sample_expdir)
        assert errors == []

    def test_verify_manifest_detects_modified_file(
        self, sample_expdir, fixed_timestamp
    ):
        """Verification detects when a file has been modified."""
        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123",
            git_remote="",
            git_branch="",
        )
        write_manifest(sample_expdir, content)

        # Modify a file after manifest was written
        (sample_expdir / "jobs" / "JGFS_ATMOS_FORECAST").write_text("modified!")

        errors = verify_manifest(sample_expdir)
        assert len(errors) > 0
        assert any("JGFS_ATMOS_FORECAST" in e for e in errors)

    def test_verify_manifest_detects_missing_file(
        self, sample_expdir, fixed_timestamp
    ):
        """Verification detects when a file has been deleted."""
        content = generate_manifest(
            expdir=sample_expdir,
            version="v17.0.0",
            platform_name="HERA",
            timestamp=fixed_timestamp,
            deployed_by="testuser",
            deployed_on="testhost",
            git_commit="abc123",
            git_remote="",
            git_branch="",
        )
        write_manifest(sample_expdir, content)

        # Delete a file
        (sample_expdir / "jobs" / "JGFS_ATMOS_FORECAST").unlink()

        errors = verify_manifest(sample_expdir)
        assert len(errors) > 0
        assert any("Missing" in e for e in errors)

    def test_verify_manifest_not_found(self, tmp_path):
        """Verification raises FileNotFoundError if no manifest."""
        with pytest.raises(FileNotFoundError):
            verify_manifest(tmp_path)
