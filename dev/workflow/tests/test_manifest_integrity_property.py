"""Property-based test: Manifest Integrity (Property 2).

After deployment (manifest generation), recompute SHA-256 of every file
and assert it matches the manifest. Then modify a file and assert that
verify_manifest() detects the change.

**Validates: Requirements 3.7**

Traces to: Design Document - Correctness Property 2
  "For all files listed in manifest.yaml, the on-disk SHA-256 equals
   the recorded hash."
"""

from __future__ import annotations

import os
import sys
from datetime import datetime, timezone
from pathlib import Path

from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.manifest import (
    generate_manifest,
    verify_manifest,
    write_manifest,
)


# ---------------------------------------------------------------------------
# Hypothesis Strategies
# ---------------------------------------------------------------------------


@st.composite
def _random_expdir(draw, tmp_path_factory):
    """Generate a random EXPDIR with arbitrary file content.

    Creates a temporary directory with a random number of files containing
    random binary content, simulating a deployed EXPDIR.
    """
    tmp_path = tmp_path_factory.mktemp("expdir")

    # Generate between 1 and 10 files in various subdirectories
    num_files = draw(st.integers(min_value=1, max_value=10))

    # Possible subdirectory prefixes (mimicking NCO layout)
    subdirs = ["jobs", "scripts", "ush", "parm/config/gfs", "ecf/scripts"]

    for i in range(num_files):
        subdir = draw(st.sampled_from(subdirs))
        filename = f"file_{i}.txt"
        dirpath = tmp_path / subdir
        dirpath.mkdir(parents=True, exist_ok=True)

        # Generate random content (1 to 500 bytes)
        content = draw(st.binary(min_size=1, max_size=500))
        (dirpath / filename).write_bytes(content)

    return tmp_path


# We use a simpler approach: generate file content strategies and build
# the EXPDIR inside the test function using tmp_path from pytest.


@st.composite
def _file_tree_spec(draw):
    """Generate a specification for a random file tree.

    Returns a list of (relative_path, content_bytes) tuples describing
    files to create in an EXPDIR.
    """
    subdirs = ["jobs", "scripts", "ush", "parm/config/gfs", "ecf/scripts"]
    num_files = draw(st.integers(min_value=1, max_value=8))

    files = []
    used_paths = set()

    for i in range(num_files):
        subdir = draw(st.sampled_from(subdirs))
        filename = f"file_{i}"
        rel_path = f"{subdir}/{filename}"

        # Avoid duplicates
        if rel_path in used_paths:
            continue
        used_paths.add(rel_path)

        # Generate random binary content (1 to 500 bytes)
        content = draw(st.binary(min_size=1, max_size=500))
        files.append((rel_path, content))

    # Ensure at least one file
    if not files:
        files.append(("jobs/file_default", draw(st.binary(min_size=1, max_size=100))))

    return files


# ---------------------------------------------------------------------------
# Property Test: Manifest Integrity (Property 2)
# ---------------------------------------------------------------------------


@given(file_tree=_file_tree_spec())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_manifest_integrity_unmodified(file_tree, tmp_path_factory):
    """Property 2: After manifest generation, verify_manifest() returns no errors.

    **Validates: Requirements 3.7**

    For any random EXPDIR file tree:
    - Generate a manifest capturing SHA-256 hashes of all files
    - Call verify_manifest() immediately after
    - Assert the error list is empty (all hashes match)
    """
    # Create the EXPDIR from the generated file tree spec
    expdir = tmp_path_factory.mktemp("expdir")

    for rel_path, content in file_tree:
        filepath = expdir / rel_path
        filepath.parent.mkdir(parents=True, exist_ok=True)
        filepath.write_bytes(content)

    # Generate and write the manifest
    fixed_timestamp = datetime(2025, 1, 15, 14, 30, 0, tzinfo=timezone.utc)
    manifest_content = generate_manifest(
        expdir=expdir,
        version="v17.0.0",
        platform_name="HERA",
        timestamp=fixed_timestamp,
        deployed_by="testuser",
        deployed_on="testhost",
        git_commit="abc123def456",
        git_remote="https://github.com/NOAA-EMC/global-workflow.git",
        git_branch="develop",
    )
    write_manifest(expdir, manifest_content)

    # Verify manifest integrity — should pass with no errors
    errors = verify_manifest(expdir)
    assert errors == [], (
        f"verify_manifest() reported errors on an unmodified EXPDIR:\n"
        f"{errors}\n"
        f"File tree: {[p for p, _ in file_tree]}"
    )


@given(
    file_tree=_file_tree_spec(),
    modification=st.binary(min_size=1, max_size=200),
)
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_manifest_integrity_detects_modification(file_tree, modification, tmp_path_factory):
    """Property 2: After modifying a file, verify_manifest() detects the change.

    **Validates: Requirements 3.7**

    For any random EXPDIR file tree and any random modification:
    - Generate a manifest capturing SHA-256 hashes of all files
    - Modify the first file with different content
    - Call verify_manifest()
    - Assert the error list is non-empty (hash mismatch detected)
    """
    # Create the EXPDIR from the generated file tree spec
    expdir = tmp_path_factory.mktemp("expdir")

    for rel_path, content in file_tree:
        filepath = expdir / rel_path
        filepath.parent.mkdir(parents=True, exist_ok=True)
        filepath.write_bytes(content)

    # Generate and write the manifest
    fixed_timestamp = datetime(2025, 1, 15, 14, 30, 0, tzinfo=timezone.utc)
    manifest_content = generate_manifest(
        expdir=expdir,
        version="v17.0.0",
        platform_name="HERA",
        timestamp=fixed_timestamp,
        deployed_by="testuser",
        deployed_on="testhost",
        git_commit="abc123def456",
        git_remote="https://github.com/NOAA-EMC/global-workflow.git",
        git_branch="develop",
    )
    write_manifest(expdir, manifest_content)

    # Modify the first file with different content
    target_rel_path, original_content = file_tree[0]
    target_file = expdir / target_rel_path

    # Ensure the modification actually changes the content
    # (if modification happens to equal original content, skip this example)
    if modification == original_content:
        return  # Skip — no actual change would be made

    target_file.write_bytes(modification)

    # Verify manifest integrity — should detect the modification
    errors = verify_manifest(expdir)
    assert len(errors) > 0, (
        f"verify_manifest() did not detect modification to '{target_rel_path}'.\n"
        f"Original content length: {len(original_content)}\n"
        f"Modified content length: {len(modification)}"
    )

    # The error should reference the modified file
    modified_file_mentioned = any(
        target_rel_path.replace("\\", "/") in error
        for error in errors
    )
    assert modified_file_mentioned, (
        f"verify_manifest() errors do not mention the modified file "
        f"'{target_rel_path}'.\n"
        f"Errors: {errors}"
    )
