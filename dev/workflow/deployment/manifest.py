"""Manifest generator for EXPDIR integrity verification.

Computes SHA-256 of every file under EXPDIR and writes manifest.yaml
with Snapshot_ID, git commit, deployment metadata, and per-file hashes.

Snapshot_ID format: "<semver>+<sha256_prefix_12>" of manifest content.

Traces to: Requirements 3.3, 3.6
"""

from __future__ import annotations

import getpass
import hashlib
import os
import platform
import socket
import subprocess
from collections import OrderedDict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Optional

import yaml


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

MANIFEST_FILENAME = "manifest.yaml"
_HASH_BUFFER_SIZE = 65536  # 64 KB read buffer for SHA-256 computation


# ---------------------------------------------------------------------------
# SHA-256 helpers
# ---------------------------------------------------------------------------


def sha256_file(path: Path) -> str:
    """Compute the SHA-256 hex digest of a file.

    Args:
        path: Path to the file to hash.

    Returns:
        Lowercase hex string of the SHA-256 digest.
    """
    h = hashlib.sha256()
    with open(path, "rb") as f:
        while True:
            chunk = f.read(_HASH_BUFFER_SIZE)
            if not chunk:
                break
            h.update(chunk)
    return h.hexdigest()


def sha256_bytes(data: bytes) -> str:
    """Compute the SHA-256 hex digest of a byte string.

    Args:
        data: Bytes to hash.

    Returns:
        Lowercase hex string of the SHA-256 digest.
    """
    return hashlib.sha256(data).hexdigest()


# ---------------------------------------------------------------------------
# Git metadata helpers
# ---------------------------------------------------------------------------


def _git_info(repo_root: Optional[Path] = None) -> dict[str, str]:
    """Retrieve git metadata from the repository.

    Args:
        repo_root: Path to the git repository root. If None, uses cwd.

    Returns:
        Dict with keys: git_commit, git_remote, git_branch.
        Values are empty strings if git info cannot be retrieved.
    """
    cwd = str(repo_root) if repo_root else None
    info: dict[str, str] = {
        "git_commit": "",
        "git_remote": "",
        "git_branch": "",
    }

    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            capture_output=True, text=True, cwd=cwd, timeout=10,
        )
        if result.returncode == 0:
            info["git_commit"] = result.stdout.strip()
    except (subprocess.SubprocessError, FileNotFoundError):
        pass

    try:
        result = subprocess.run(
            ["git", "remote", "get-url", "origin"],
            capture_output=True, text=True, cwd=cwd, timeout=10,
        )
        if result.returncode == 0:
            info["git_remote"] = result.stdout.strip()
    except (subprocess.SubprocessError, FileNotFoundError):
        pass

    try:
        result = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            capture_output=True, text=True, cwd=cwd, timeout=10,
        )
        if result.returncode == 0:
            info["git_branch"] = result.stdout.strip()
    except (subprocess.SubprocessError, FileNotFoundError):
        pass

    return info


# ---------------------------------------------------------------------------
# File inventory
# ---------------------------------------------------------------------------


def compute_file_hashes(expdir: Path) -> OrderedDict[str, dict[str, Any]]:
    """Compute SHA-256 hashes and sizes for all files under EXPDIR.

    Walks the EXPDIR directory tree, computes the SHA-256 hash and file
    size for every regular file, and returns them in a sorted OrderedDict
    keyed by relative path (using forward slashes).

    The manifest.yaml file itself is excluded from the inventory since
    it cannot contain its own hash.

    Args:
        expdir: Path to the EXPDIR root directory.

    Returns:
        OrderedDict mapping relative file paths to dicts with keys
        'sha256' (hex string) and 'size' (int, bytes).
    """
    files: dict[str, dict[str, Any]] = {}

    for dirpath, _dirnames, filenames in os.walk(expdir):
        for filename in filenames:
            filepath = Path(dirpath) / filename
            # Skip the manifest file itself
            rel_path = filepath.relative_to(expdir)
            rel_str = str(rel_path).replace(os.sep, "/")
            if rel_str == MANIFEST_FILENAME:
                continue
            # Skip non-regular files (symlinks, etc.)
            if not filepath.is_file() or filepath.is_symlink():
                continue

            file_hash = sha256_file(filepath)
            file_size = filepath.stat().st_size
            files[rel_str] = {
                "sha256": file_hash,
                "size": file_size,
            }

    # Sort by path for deterministic output
    return OrderedDict(sorted(files.items()))


# ---------------------------------------------------------------------------
# Snapshot_ID generation
# ---------------------------------------------------------------------------


def compute_snapshot_id(version: str, manifest_content: bytes) -> str:
    """Compute the Snapshot_ID from version and manifest content.

    Format: "<semver>+<sha256_prefix_12>" where the SHA-256 is computed
    over the manifest content (excluding the snapshot_id field itself).

    Args:
        version: Semantic version string (e.g. "v17.0.0").
        manifest_content: The serialized manifest content (without
            the snapshot_id line) as bytes.

    Returns:
        Snapshot_ID string, e.g. "v17.0.0+a3f8c1d2e4b6".
    """
    content_hash = sha256_bytes(manifest_content)
    prefix_12 = content_hash[:12]
    return f"{version}+{prefix_12}"


# ---------------------------------------------------------------------------
# Manifest generation
# ---------------------------------------------------------------------------


def generate_manifest(
    expdir: Path,
    version: str,
    platform_name: str,
    repo_root: Optional[Path] = None,
    wxflow_version: str = "",
    uwtools_version: str = "",
    timestamp: Optional[datetime] = None,
    deployed_by: Optional[str] = None,
    deployed_on: Optional[str] = None,
    git_commit: Optional[str] = None,
    git_remote: Optional[str] = None,
    git_branch: Optional[str] = None,
) -> str:
    """Generate manifest.yaml content for an EXPDIR.

    Computes SHA-256 of every file under EXPDIR, assembles deployment
    metadata, computes the Snapshot_ID, and returns the complete
    manifest.yaml content as a string.

    The Snapshot_ID is computed as "<version>+<sha256_prefix_12>" where
    the SHA-256 is over the manifest content with snapshot_id set to a
    placeholder, ensuring the ID is deterministic for the same inputs.

    Args:
        expdir: Path to the EXPDIR root directory.
        version: Semantic version string (e.g. "v17.0.0").
        platform_name: Target platform (e.g. "HERA", "WCOSS2").
        repo_root: Path to the git repository root for metadata.
        wxflow_version: Pinned wxflow version string.
        uwtools_version: Pinned uwtools version string.
        timestamp: Deployment timestamp. Defaults to current UTC time.
        deployed_by: Username of deployer. Defaults to current user.
        deployed_on: Hostname of deployment machine. Defaults to current host.
        git_commit: Override git commit hash (for testing/determinism).
        git_remote: Override git remote URL.
        git_branch: Override git branch name.

    Returns:
        The complete manifest.yaml content as a string, ready to be
        written to disk.
    """
    # --- Gather metadata ---
    if timestamp is None:
        timestamp = datetime.now(timezone.utc)

    if deployed_by is None:
        deployed_by = getpass.getuser()

    if deployed_on is None:
        deployed_on = socket.getfqdn()

    # Git info: use overrides or query git
    if git_commit is None or git_remote is None or git_branch is None:
        git_info = _git_info(repo_root)
        if git_commit is None:
            git_commit = git_info["git_commit"]
        if git_remote is None:
            git_remote = git_info["git_remote"]
        if git_branch is None:
            git_branch = git_info["git_branch"]

    # --- Compute file hashes ---
    file_hashes = compute_file_hashes(expdir)

    # --- Build manifest content (without snapshot_id) for hashing ---
    # We serialize the manifest body first to compute the content hash,
    # then insert the snapshot_id.
    manifest_body = OrderedDict([
        ("git_commit", git_commit),
        ("git_remote", git_remote),
        ("git_branch", git_branch),
        ("deployed_by", deployed_by),
        ("deployed_on", deployed_on),
        ("deployed_at", timestamp.strftime("%Y-%m-%dT%H:%M:%SZ")),
        ("platform", platform_name),
        ("wxflow_version", wxflow_version),
        ("uwtools_version", uwtools_version),
        ("files", file_hashes),
    ])

    # Serialize body for content hashing
    body_yaml = _serialize_manifest(manifest_body)
    body_bytes = body_yaml.encode("utf-8")

    # --- Compute Snapshot_ID ---
    snapshot_id = compute_snapshot_id(version, body_bytes)

    # --- Build final manifest with snapshot_id at the top ---
    full_manifest = OrderedDict([
        ("snapshot_id", snapshot_id),
    ])
    full_manifest.update(manifest_body)

    return _serialize_manifest(full_manifest)


def write_manifest(expdir: Path, content: str) -> Path:
    """Write manifest.yaml to the EXPDIR root.

    Args:
        expdir: Path to the EXPDIR root directory.
        content: The manifest.yaml content string.

    Returns:
        Path to the written manifest.yaml file.
    """
    manifest_path = expdir / MANIFEST_FILENAME
    manifest_path.write_text(content, encoding="utf-8")
    return manifest_path


# ---------------------------------------------------------------------------
# Manifest verification
# ---------------------------------------------------------------------------


def verify_manifest(expdir: Path) -> list[str]:
    """Verify the integrity of an EXPDIR against its manifest.

    Reads manifest.yaml, recomputes SHA-256 of every listed file, and
    reports any mismatches.

    Args:
        expdir: Path to the EXPDIR root directory.

    Returns:
        List of error strings. Empty list means all files match.

    Raises:
        FileNotFoundError: If manifest.yaml does not exist.
    """
    manifest_path = expdir / MANIFEST_FILENAME
    if not manifest_path.exists():
        raise FileNotFoundError(
            f"Manifest not found: {manifest_path}"
        )

    with open(manifest_path, "r", encoding="utf-8") as f:
        manifest = yaml.safe_load(f)

    errors: list[str] = []
    files_section = manifest.get("files", {})

    for rel_path, expected in files_section.items():
        filepath = expdir / rel_path
        if not filepath.exists():
            errors.append(f"Missing file: {rel_path}")
            continue

        actual_hash = sha256_file(filepath)
        expected_hash = expected.get("sha256", "")
        if actual_hash != expected_hash:
            errors.append(
                f"Hash mismatch: {rel_path} "
                f"(expected {expected_hash[:16]}..., "
                f"got {actual_hash[:16]}...)"
            )

        actual_size = filepath.stat().st_size
        expected_size = expected.get("size", -1)
        if actual_size != expected_size:
            errors.append(
                f"Size mismatch: {rel_path} "
                f"(expected {expected_size}, got {actual_size})"
            )

    return errors


# ---------------------------------------------------------------------------
# YAML serialization helper
# ---------------------------------------------------------------------------


def _serialize_manifest(data: OrderedDict) -> str:
    """Serialize manifest data to canonical YAML.

    Uses a custom dumper that preserves OrderedDict key order and
    produces clean, deterministic output suitable for hashing.

    Args:
        data: OrderedDict of manifest data.

    Returns:
        YAML string with deterministic formatting.
    """

    class _ManifestDumper(yaml.SafeDumper):
        """Custom YAML dumper for manifest serialization."""
        pass

    def _represent_ordered_dict(
        dumper: yaml.SafeDumper, data: OrderedDict
    ) -> Any:
        return dumper.represent_mapping(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
            data.items(),
        )

    def _represent_str(dumper: yaml.SafeDumper, data: str) -> Any:
        # Use double-quoted style for strings containing special chars
        if any(c in data for c in ("\n", "\t", ":", "#", "{", "}")):
            return dumper.represent_scalar(
                "tag:yaml.org,2002:str", data, style='"'
            )
        # Empty strings need quoting
        if data == "":
            return dumper.represent_scalar(
                "tag:yaml.org,2002:str", data, style='"'
            )
        return dumper.represent_scalar("tag:yaml.org,2002:str", data)

    _ManifestDumper.add_representer(OrderedDict, _represent_ordered_dict)
    _ManifestDumper.add_representer(str, _represent_str)

    return yaml.dump(
        data,
        Dumper=_ManifestDumper,
        default_flow_style=False,
        sort_keys=False,
        allow_unicode=True,
        width=120,
    )
