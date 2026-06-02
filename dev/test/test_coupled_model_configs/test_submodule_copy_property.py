"""Property-based tests for submodule copy integrity.

Uses hypothesis to verify Property 9: Submodule Copy Integrity.
Generates arbitrary file content (including binary bytes and Jinja2-like
syntax) and asserts that _stage_submodule_copy produces byte-identical
copies without attempting Jinja2 rendering.

Feature: coupled-model-configs, Property 9: Submodule Copy Integrity

Traces to: Requirements 13.3, 13.4, 13.5
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.pipeline import (
    SUBMODULE_COPY_MANIFEST,
    _stage_submodule_copy,
)


# ---------------------------------------------------------------------------
# Hypothesis strategies for file content generation
# ---------------------------------------------------------------------------


# Strategy for arbitrary binary content (any bytes)
arbitrary_bytes = st.binary(min_size=0, max_size=4096)

# Strategy for text content that includes Jinja2-like syntax
jinja2_like_text = st.one_of(
    # Plain text
    st.text(min_size=0, max_size=1024),
    # Text with Jinja2 variable syntax {{ var }}
    st.builds(
        lambda prefix, var, suffix: f"{prefix}{{{{ {var} }}}}{suffix}",
        prefix=st.text(min_size=0, max_size=256),
        var=st.from_regex(r"[a-z_][a-z0-9_]*", fullmatch=True),
        suffix=st.text(min_size=0, max_size=256),
    ),
    # Text with Jinja2 block syntax {% if %} ... {% endif %}
    st.builds(
        lambda prefix, var, body, suffix: (
            f"{prefix}{{% if {var} %}}{body}{{% endif %}}{suffix}"
        ),
        prefix=st.text(min_size=0, max_size=128),
        var=st.from_regex(r"[a-z_][a-z0-9_]*", fullmatch=True),
        body=st.text(min_size=0, max_size=256),
        suffix=st.text(min_size=0, max_size=128),
    ),
    # Text with Jinja2 for loops {% for x in items %}
    st.builds(
        lambda var, items, body: (
            f"{{% for {var} in {items} %}}{body}{{% endfor %}}"
        ),
        var=st.from_regex(r"[a-z_][a-z0-9_]*", fullmatch=True),
        items=st.from_regex(r"[a-z_][a-z0-9_]*", fullmatch=True),
        body=st.text(min_size=0, max_size=256),
    ),
)

# Combined strategy: either binary or Jinja2-like text encoded as bytes
file_content_strategy = st.one_of(
    arbitrary_bytes,
    jinja2_like_text.map(lambda t: t.encode("utf-8")),
)

# Strategy for valid filenames (no path separators, non-empty)
filename_strategy = st.from_regex(
    r"[a-zA-Z][a-zA-Z0-9_.\-]{0,30}", fullmatch=True
)


# ---------------------------------------------------------------------------
# Helper: create workspace directories
# ---------------------------------------------------------------------------


def _create_workspace() -> tuple[Path, Path, Path]:
    """Create a temporary workspace with project root and EXPDIR.

    Returns:
        Tuple of (tmpdir_path, project_root, expdir).
        Caller is responsible for cleanup.
    """
    tmpdir = Path(tempfile.mkdtemp())
    project_root = tmpdir / "global-workflow"
    project_root.mkdir()
    expdir = tmpdir / "EXPDIR"
    expdir.mkdir()

    # Create all manifest source directories
    for source_rel, _ in SUBMODULE_COPY_MANIFEST:
        src_dir = project_root / source_rel
        src_dir.mkdir(parents=True, exist_ok=True)

    return tmpdir, project_root, expdir


def _cleanup_workspace(tmpdir: Path) -> None:
    """Remove the temporary workspace."""
    import shutil
    shutil.rmtree(tmpdir, ignore_errors=True)


# ---------------------------------------------------------------------------
# Property 9: Submodule Copy Integrity
# ---------------------------------------------------------------------------


class TestSubmoduleCopyIntegrity:
    """Property 9: Submodule Copy Integrity.

    **Validates: Requirements 13.3, 13.4, 13.5**

    For any file designated as submodule-owned in the copy manifest,
    the file copied into EXPDIR SHALL be byte-identical to the source
    file in sorc/. No Jinja2 rendering SHALL be attempted on these files.
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow, HealthCheck.function_scoped_fixture],
        deadline=None,
    )
    @given(
        content=file_content_strategy,
        filename=filename_strategy,
    )
    def test_copied_file_is_byte_identical_to_source(
        self, content: bytes, filename: str
    ):
        """Copied submodule file is byte-identical to the source.

        **Validates: Requirements 13.3**

        For any arbitrary file content (binary or text), the file copied
        by _stage_submodule_copy into EXPDIR must be byte-for-byte
        identical to the source file.
        """
        tmpdir, project_root, expdir = _create_workspace()
        try:
            # Use the first manifest entry for testing
            source_rel, dest_rel = SUBMODULE_COPY_MANIFEST[0]
            src_dir = project_root / source_rel

            # Write the generated content to a file in the source directory
            src_file = src_dir / filename
            src_file.write_bytes(content)

            # Execute the submodule copy
            _stage_submodule_copy(project_root, expdir)

            # Verify the copied file is byte-identical
            dst_file = expdir / dest_rel / filename
            assert dst_file.exists(), (
                f"Expected copied file at {dst_file} but it does not exist"
            )
            assert dst_file.read_bytes() == content, (
                f"Copied file content differs from source for file '{filename}'"
            )
        finally:
            _cleanup_workspace(tmpdir)

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow, HealthCheck.function_scoped_fixture],
        deadline=None,
    )
    @given(content=jinja2_like_text)
    def test_jinja2_syntax_not_rendered_in_submodule_files(
        self, content: str
    ):
        """Content with Jinja2 syntax ({{ var }}, {% if %}) is NOT rendered.

        **Validates: Requirements 13.4, 13.5**

        Files containing Jinja2-like syntax ({{ var }}, {% if %}, {% for %})
        must be preserved verbatim — no template rendering is attempted
        on submodule-owned files.
        """
        tmpdir, project_root, expdir = _create_workspace()
        try:
            # Use the first manifest entry
            source_rel, dest_rel = SUBMODULE_COPY_MANIFEST[0]
            src_dir = project_root / source_rel

            # Write a file with Jinja2-like content using binary mode
            # to avoid any text-mode line ending normalization
            content_bytes = content.encode("utf-8")
            test_filename = "config_with_jinja.rc"
            src_file = src_dir / test_filename
            src_file.write_bytes(content_bytes)

            # Execute the submodule copy
            _stage_submodule_copy(project_root, expdir)

            # Verify the content is preserved verbatim (no rendering)
            dst_file = expdir / dest_rel / test_filename
            assert dst_file.exists(), (
                f"Expected copied file at {dst_file} but it does not exist"
            )
            dst_content_bytes = dst_file.read_bytes()
            assert dst_content_bytes == content_bytes, (
                f"Jinja2-like content was modified during copy. "
                f"Expected: {content_bytes!r}, Got: {dst_content_bytes!r}"
            )
        finally:
            _cleanup_workspace(tmpdir)

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow, HealthCheck.function_scoped_fixture],
        deadline=None,
    )
    @given(
        content=arbitrary_bytes,
        manifest_idx=st.integers(
            min_value=0, max_value=len(SUBMODULE_COPY_MANIFEST) - 1
        ),
    )
    def test_all_manifest_entries_copy_verbatim(
        self, content: bytes, manifest_idx: int
    ):
        """All manifest entries produce byte-identical copies.

        **Validates: Requirements 13.3, 13.4, 13.5**

        For any manifest entry and any file content, the copy operation
        produces a byte-identical file in the destination directory.
        """
        tmpdir, project_root, expdir = _create_workspace()
        try:
            # Write content to the selected manifest entry's source
            source_rel, dest_rel = SUBMODULE_COPY_MANIFEST[manifest_idx]
            src_dir = project_root / source_rel
            test_filename = "test_file.dat"
            src_file = src_dir / test_filename
            src_file.write_bytes(content)

            # Execute the submodule copy
            _stage_submodule_copy(project_root, expdir)

            # Verify byte-identical copy
            dst_file = expdir / dest_rel / test_filename
            assert dst_file.exists(), (
                f"Expected copied file at {dst_file} for manifest entry "
                f"'{source_rel}' → '{dest_rel}'"
            )
            assert dst_file.read_bytes() == content, (
                f"File content differs for manifest entry "
                f"'{source_rel}' → '{dest_rel}'"
            )
        finally:
            _cleanup_workspace(tmpdir)
