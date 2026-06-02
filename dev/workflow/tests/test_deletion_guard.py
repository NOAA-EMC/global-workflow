"""Unit tests for the reference-guarded deletion helper.

Covers the deletion-guard algorithm (Req 2.5, Design Component 2):

- a target referenced by a retained runtime script is RETAINED with a
  verification error naming the referencer;
- a target referenced only from a comment / a sibling in the same deletion
  batch / the exemption registry is still DELETED;
- unconditional legacy ``@[...]`` files are deleted when present (Req 2.3, 2.4);
- ``dry_run`` computes the same outcome without touching the filesystem;
- absent targets are reported as missing (e.g. the gitignored ``atparse.bash``
  build-time symlink).

**Validates: Requirements 2.1, 2.2, 2.3, 2.4, 2.5**

Traces to: Design Document - Component 2 (Obsolete Script & Legacy File
Removal); parent Req 4.6, Req 8, Property 14.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.deletion_guard import (
    DeletionResult,
    delete_guarded,
    find_blocking_references,
)


def _write(path: Path, text: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


# ---------------------------------------------------------------------------
# find_blocking_references
# ---------------------------------------------------------------------------


def test_functional_reference_blocks(tmp_path: Path):
    """A retained script that sources the target blocks its deletion (Req 2.5)."""
    _write(tmp_path / "ush" / "victim.sh", "echo bye\n")
    _write(
        tmp_path / "ush" / "consumer.sh",
        '#!/bin/bash\nsource "${USHgfs}/victim.sh"\n',
    )
    refs = find_blocking_references(tmp_path, "ush/victim.sh")
    assert refs == ["ush/consumer.sh"]


def test_comment_only_reference_does_not_block(tmp_path: Path):
    """A name appearing only in a comment is documentation, not a reference."""
    _write(tmp_path / "ush" / "victim.sh", "echo bye\n")
    _write(
        tmp_path / "ush" / "doc.sh",
        "#!/bin/bash\n"
        "# This replaces the legacy victim.sh runtime generator.\n"
        'cpreq "${EXPDIR}/x" "${DATA}/x"  # was victim.sh\n',
    )
    refs = find_blocking_references(tmp_path, "ush/victim.sh")
    assert refs == []


def test_batch_sibling_not_a_referencer(tmp_path: Path):
    """A file scheduled for deletion in the same batch cannot block a sibling."""
    _write(tmp_path / "ush" / "a.sh", 'source "b.sh"\n')
    _write(tmp_path / "ush" / "b.sh", 'source "a.sh"\n')
    # Each references the other, but both are in the batch -> neither blocks.
    assert find_blocking_references(tmp_path, "ush/a.sh", batch={"ush/a.sh", "ush/b.sh"}) == []
    assert find_blocking_references(tmp_path, "ush/b.sh", batch={"ush/a.sh", "ush/b.sh"}) == []


def test_registry_is_not_a_referencer(tmp_path: Path):
    """The exemption registry naming the target does not count as a reference."""
    _write(tmp_path / "ush" / "victim.sh", "echo bye\n")
    _write(
        tmp_path / "dev" / "parm" / "atparse_exemptions.yaml",
        "exemptions:\n  - path: ush/victim.sh\n",
    )
    refs = find_blocking_references(tmp_path, "ush/victim.sh")
    assert refs == []


def test_dev_tree_template_header_not_a_referencer(tmp_path: Path):
    """A ``dev/`` .j2 template naming the script is excluded (not runtime)."""
    _write(tmp_path / "ush" / "victim.sh", "echo bye\n")
    _write(
        tmp_path / "dev" / "parm" / "ufs" / "x.j2",
        "{# Replaces: victim.sh #}\nkey = value\n",
    )
    # dev/ is outside RUNTIME_SCAN_DIRS, so it is never scanned.
    assert find_blocking_references(tmp_path, "ush/victim.sh") == []


# ---------------------------------------------------------------------------
# delete_guarded
# ---------------------------------------------------------------------------


def test_delete_unreferenced_target(tmp_path: Path):
    """An unreferenced guarded target is deleted (Req 2.1, 2.2)."""
    _write(tmp_path / "ush" / "obsolete.sh", "echo gone\n")
    result = delete_guarded(
        tmp_path,
        guarded_targets=["ush/obsolete.sh"],
        unconditional_targets=[],
    )
    assert result.passed
    assert result.deleted == ["ush/obsolete.sh"]
    assert not (tmp_path / "ush" / "obsolete.sh").exists()


def test_referenced_target_retained_with_error(tmp_path: Path):
    """A referenced guarded target is retained with a verification error (Req 2.5)."""
    _write(tmp_path / "ush" / "obsolete.sh", "echo gone\n")
    _write(tmp_path / "scripts" / "live.sh", 'source "${USHgfs}/obsolete.sh"\n')
    result = delete_guarded(
        tmp_path,
        guarded_targets=["ush/obsolete.sh"],
        unconditional_targets=[],
    )
    assert not result.passed
    assert result.retained == [("ush/obsolete.sh", ["scripts/live.sh"])]
    assert result.deleted == []
    # The file must still exist (deletion blocked).
    assert (tmp_path / "ush" / "obsolete.sh").exists()
    assert any("scripts/live.sh" in e for e in result.errors)


def test_unconditional_legacy_files_deleted(tmp_path: Path):
    """Legacy ``@[...]`` data files are deleted unconditionally (Req 2.3, 2.4)."""
    _write(tmp_path / "parm" / "ufs" / "fv3" / "diag_table", "x=@[Y]\n")
    _write(tmp_path / "parm" / "ufs" / "gocart" / "AERO_HISTORY.rc", "z=@[W]\n")
    result = delete_guarded(
        tmp_path,
        guarded_targets=[],
        unconditional_targets=[
            "parm/ufs/fv3/diag_table",
            "parm/ufs/gocart/AERO_HISTORY.rc",
        ],
    )
    assert result.passed
    assert set(result.deleted) == {
        "parm/ufs/fv3/diag_table",
        "parm/ufs/gocart/AERO_HISTORY.rc",
    }
    assert not (tmp_path / "parm" / "ufs" / "fv3" / "diag_table").exists()


def test_absent_target_reported_missing(tmp_path: Path):
    """An absent target (e.g. gitignored atparse.bash symlink) is 'missing'."""
    result = delete_guarded(
        tmp_path,
        guarded_targets=["ush/atparse.bash"],
        unconditional_targets=[],
    )
    assert result.passed  # nothing to delete is not an error
    assert result.missing == ["ush/atparse.bash"]
    assert result.deleted == []


def test_dry_run_does_not_remove(tmp_path: Path):
    """dry_run computes the same outcome without removing files."""
    _write(tmp_path / "ush" / "obsolete.sh", "echo gone\n")
    result = delete_guarded(
        tmp_path,
        guarded_targets=["ush/obsolete.sh"],
        unconditional_targets=[],
        dry_run=True,
    )
    assert result.deleted == ["ush/obsolete.sh"]
    # File survives the dry run.
    assert (tmp_path / "ush" / "obsolete.sh").exists()


def test_atparse_blocked_by_exempt_consumer(tmp_path: Path):
    """atparse.bash is retained while a registry-exempt consumer still sources it.

    Mirrors the real repo: the three exemption-registered consumers
    (regrid_gsiSfcIncr_to_tile.sh, prep_sfc_snow.sh, exgfs_wave_nawips.sh)
    still ``source atparse.bash``, so the engine is not yet deletable.
    """
    _write(tmp_path / "ush" / "atparse.bash", "atparse() { :; }\n")
    _write(
        tmp_path / "ush" / "prep_sfc_snow.sh",
        'source "${USHglobal}/atparse.bash"\n',
    )
    result = delete_guarded(
        tmp_path,
        guarded_targets=["ush/atparse.bash"],
        unconditional_targets=[],
    )
    assert not result.passed
    assert result.retained == [("ush/atparse.bash", ["ush/prep_sfc_snow.sh"])]
    assert (tmp_path / "ush" / "atparse.bash").exists()


def test_result_passed_semantics():
    """DeletionResult.passed is False iff a target was retained or errored."""
    assert DeletionResult(deleted=["a"], missing=["b"]).passed
    assert not DeletionResult(retained=[("a", ["r"])]).passed
    assert not DeletionResult(errors=["boom"]).passed
