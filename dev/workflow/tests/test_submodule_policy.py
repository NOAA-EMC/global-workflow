"""Unit tests for deterministic Submodule_Source handling (Req 6.1, 6.2).

Covers the three resolution policies threaded through
``deployment.pipeline._stage_submodule_copy`` and ``run``:

  - ``SubmodulePolicy.REQUIRE``  → FATAL ``PipelineError`` on a missing source.
  - ``SubmodulePolicy.FIXTURE``  → missing sources resolve from the committed
    Submodule_Fixture (``dev/workflow/tests/fixtures/submodules/``) so the copy
    succeeds.
  - ``SubmodulePolicy.SKIP_OPTIONAL`` → optional entries are skipped with a
    warning and no FATAL.

Validates: Requirements 6.2, 6.3, 6.4, 6.5, 6.6
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import (
    SUBMODULE_COPY_MANIFEST,
    PipelineError,
    SubmodulePolicy,
    _stage_submodule_copy,
    run,
)

# Committed Submodule_Fixture tree (Req 6.2, 6.7). Resolved relative to this
# test file so it works regardless of the current working directory.
FIXTURE_ROOT = (Path(__file__).resolve().parent / "fixtures" / "submodules")

# The four stand-in files the fixture provides, keyed by their EXPDIR-relative
# destination (derived from SUBMODULE_COPY_MANIFEST).
EXPECTED_DEST_FILES = (
    "parm/chem/nexus/gocart/NEXUS_Config.rc",
    "parm/chem/nexus/gocart/HEMCO_sa_Config.rc",
    "parm/post/params_grib2_tbl_new",
    "parm/post/postxconfig-NT-GFS.txt",
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _unseal_expdir(expdir: Path) -> None:
    """Remove read-only permissions from a sealed EXPDIR so it can be cleaned up."""
    if not expdir.exists():
        return
    for dirpath, _dirnames, filenames in os.walk(expdir):
        dp = Path(dirpath)
        try:
            os.chmod(dp, 0o755)
        except OSError:
            pass
        for fn in filenames:
            try:
                os.chmod(dp / fn, 0o644)
            except OSError:
                pass
    try:
        os.chmod(expdir, 0o755)
    except OSError:
        pass


def _empty_project_root(tmp_path: Path) -> Path:
    """A project root with no ``sorc/`` submodules checked out."""
    project_root = tmp_path / "global-workflow"
    project_root.mkdir(parents=True, exist_ok=True)
    return project_root


def _make_minimal_dev_tree(base_path: Path) -> dict:
    """Create a minimal dev/ tree suitable for ``pipeline.run()``.

    Returns a dict with ``dev_root`` and ``config_path`` keys. The tree does
    NOT contain ``sorc/nexus.fd`` or ``sorc/upp.fd``, so the submodule copy
    stage must rely on the configured policy to resolve those sources.
    """
    dev_root = base_path / "dev"
    dev_root.mkdir(parents=True, exist_ok=True)

    (dev_root / "jobs").mkdir(exist_ok=True)
    (dev_root / "scripts").mkdir(exist_ok=True)
    (dev_root / "ush").mkdir(exist_ok=True)
    (dev_root / "parm" / "workflow").mkdir(parents=True, exist_ok=True)
    (dev_root / "workflow" / "ecflow" / "templates").mkdir(
        parents=True, exist_ok=True
    )

    config = {
        "suite": {
            "name": "test_suite",
            "ecf_home": "{{ EXPDIR }}/ecf",
            "ecf_files": "{{ EXPDIR }}/ecf/scripts",
            "ecf_include": "{{ EXPDIR }}/ecf/include",
        },
        "defaults": {"ECF_TRIES": 2},
        "families": [
            {
                "path": "test/family",
                "tasks": [
                    {"name": "task_a", "jjob": "JTEST_TASK_A", "trigger": ""},
                ],
            }
        ],
    }
    config_path = dev_root / "parm" / "workflow" / "test_config.yaml"
    config_path.write_text(yaml.dump(config, sort_keys=False))

    template = (
        "%include <head.h>\n"
        "%include <envsetup.h>\n"
        "# Task: {{ task.name }} | JJob: {{ task.jjob }}\n"
        "${EXPDIR}/ush/universal_wrapper.sh {{ task.jjob }}\n"
        "%include <tail.h>\n"
    )
    (dev_root / "workflow" / "ecflow" / "templates" / "task.ecf.j2").write_text(
        template
    )

    # Mark the repo root for git metadata discovery.
    (base_path / ".git").mkdir(exist_ok=True)

    # Unconditional artifacts (Req 9.1, 9.2)
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

    return {"dev_root": dev_root, "config_path": config_path}


# ---------------------------------------------------------------------------
# Sanity: the committed fixture mirrors the manifest source layout
# ---------------------------------------------------------------------------


def test_fixture_mirrors_manifest_sources():
    """The committed fixture provides every SUBMODULE_COPY_MANIFEST source.

    Validates: Requirements 6.2
    """
    assert FIXTURE_ROOT.is_dir(), f"Fixture tree missing at {FIXTURE_ROOT}"
    for source_rel, _dest_rel in SUBMODULE_COPY_MANIFEST:
        fixture_src = FIXTURE_ROOT / source_rel
        assert fixture_src.exists(), (
            f"Fixture is missing manifest source '{source_rel}' "
            f"(expected at {fixture_src})"
        )


# ---------------------------------------------------------------------------
# REQUIRE policy — FATAL on missing source
# ---------------------------------------------------------------------------


def test_require_policy_fatals_on_missing_source(tmp_path):
    """REQUIRE raises a FATAL PipelineError when a submodule source is absent.

    Validates: Requirements 6.2
    """
    project_root = _empty_project_root(tmp_path)
    expdir = tmp_path / "EXPDIR"
    expdir.mkdir()

    with pytest.raises(PipelineError, match="Submodule source not found"):
        _stage_submodule_copy(
            project_root,
            expdir,
            policy=SubmodulePolicy.REQUIRE,
        )


def test_require_is_the_default_policy(tmp_path):
    """Omitting the policy keyword defaults to REQUIRE (production behavior).

    Validates: Requirements 6.2
    """
    project_root = _empty_project_root(tmp_path)
    expdir = tmp_path / "EXPDIR"
    expdir.mkdir()

    with pytest.raises(PipelineError, match="Submodule source not found"):
        _stage_submodule_copy(project_root, expdir)


# ---------------------------------------------------------------------------
# FIXTURE policy — copy succeeds from the committed fixture
# ---------------------------------------------------------------------------


def test_fixture_policy_copies_from_fixture(tmp_path):
    """FIXTURE resolves missing sources from the committed fixture tree.

    Validates: Requirements 6.2
    """
    project_root = _empty_project_root(tmp_path)
    expdir = tmp_path / "EXPDIR"
    expdir.mkdir()

    copied = _stage_submodule_copy(
        project_root,
        expdir,
        policy=SubmodulePolicy.FIXTURE,
        fixture_root=FIXTURE_ROOT,
    )

    # Every expected destination file was copied into the EXPDIR.
    copied_rel = {str(p.relative_to(expdir)) for p in copied}
    for dest in EXPECTED_DEST_FILES:
        assert dest in copied_rel, f"Expected '{dest}' to be copied, got {copied_rel}"
        assert (expdir / dest).is_file()


def test_fixture_policy_requires_fixture_root(tmp_path):
    """FIXTURE without a fixture_root is a FATAL configuration error.

    Validates: Requirements 6.2
    """
    project_root = _empty_project_root(tmp_path)
    expdir = tmp_path / "EXPDIR"
    expdir.mkdir()

    with pytest.raises(PipelineError, match="requires a fixture_root"):
        _stage_submodule_copy(
            project_root,
            expdir,
            policy=SubmodulePolicy.FIXTURE,
            fixture_root=None,
        )


def test_fixture_backed_full_deploy_succeeds(tmp_path):
    """A full pipeline run with FIXTURE policy completes and copies the fixture.

    This is the end-to-end form of Req 6.2: a verification deploy completes
    without a "Submodule source not found" FATAL even though no submodules
    are checked out.

    Validates: Requirements 6.2, 6.3
    """
    tree = _make_minimal_dev_tree(tmp_path)
    expdir = tmp_path / "EXPDIR"

    try:
        result = run(
            config=str(tree["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            submodule_policy=SubmodulePolicy.FIXTURE,
            fixture_root=str(FIXTURE_ROOT),
        )

        assert result["dry_run"] is False
        assert expdir.exists()

        _unseal_expdir(expdir)

        # The fixture-provided submodule files landed at their manifest
        # destinations inside the EXPDIR.
        for dest in EXPECTED_DEST_FILES:
            assert (expdir / dest).is_file(), (
                f"Fixture-backed deploy did not stage '{dest}'"
            )
    finally:
        _unseal_expdir(expdir)


# ---------------------------------------------------------------------------
# SKIP_OPTIONAL policy — optional entries skipped, no FATAL
# ---------------------------------------------------------------------------


def test_skip_optional_skips_missing_optional_sources(tmp_path):
    """SKIP_OPTIONAL skips optional missing sources without raising.

    Both manifest entries are flagged optional, so a project root with no
    submodules yields an empty copy list and no FATAL.

    Validates: Requirements 6.2
    """
    project_root = _empty_project_root(tmp_path)
    expdir = tmp_path / "EXPDIR"
    expdir.mkdir()

    copied = _stage_submodule_copy(
        project_root,
        expdir,
        policy=SubmodulePolicy.SKIP_OPTIONAL,
    )

    assert copied == [], (
        "SKIP_OPTIONAL should skip all optional missing sources, "
        f"but copied: {copied}"
    )


def test_skip_optional_emits_warning(tmp_path, caplog):
    """SKIP_OPTIONAL logs a warning for each skipped optional source.

    Validates: Requirements 6.2
    """
    import logging

    project_root = _empty_project_root(tmp_path)
    expdir = tmp_path / "EXPDIR"
    expdir.mkdir()

    with caplog.at_level(logging.WARNING):
        _stage_submodule_copy(
            project_root,
            expdir,
            policy=SubmodulePolicy.SKIP_OPTIONAL,
        )

    assert any(
        "Skipping optional submodule source" in rec.getMessage()
        for rec in caplog.records
    ), "Expected a warning about skipping an optional submodule source"
