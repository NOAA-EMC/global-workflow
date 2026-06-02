"""Unit tests for the wxflow/uwtools version gate (Stage 1 precondition).

These tests exercise the hard version-pinning precondition wired into the
deployment pipeline's validate stage:

  - ``deployment.pipeline._check_version_gate(dev_root, enforce_versions=...)``
  - ``deployment.pipeline._stage_validate(..., enforce_versions=...)``
  - ``deployment.pipeline.run(..., enforce_versions=...)``

Behaviour under test:

  * A version *mismatch* (package importable but wrong version) is ALWAYS a
    FATAL ERROR (``PipelineError``), regardless of ``enforce_versions``.
  * When ``enforce_versions=True`` a not-importable ``wxflow``/``uwtools`` is
    ALSO a FATAL ERROR naming the package, the expected version, and the
    found state.
  * When ``enforce_versions=False`` a not-importable package is a non-fatal
    warning (the broad suite can still run).
  * Because Stage 1 runs BEFORE ``expdir.mkdir()``, a FATAL in the gate
    guarantees that no EXPDIR directory/file is written.
  * When the installed versions match the pins, the gate passes (no raise).

The version resolution is monkeypatched so the tests are robust whether or
not ``wxflow``/``uwtools`` are actually installed in the active ``.venv``.

Traces to: Requirements 5.1, 5.2
"""

from __future__ import annotations

import os
import sys

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment import pipeline, validation
from deployment.pipeline import (
    PipelineError,
    _check_version_gate,
    _stage_validate,
    run,
)

# The pins declared in dev/workflow/requirements.txt that the gate enforces.
PINNED_WXFLOW = "0.3.0"
PINNED_UWTOOLS = "2.16.0"


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------


def _patch_installed_versions(monkeypatch, mapping):
    """Stub version resolution for both bindings of ``_get_installed_version``.

    ``pipeline.py`` does ``from .validation import _get_installed_version`` (its
    own binding, used directly by ``_check_version_gate``) while
    ``validation.check_pinned_versions`` calls the function via the
    ``validation`` module namespace. Patch both so the gate is fully driven by
    ``mapping`` rather than the live environment.

    Args:
        monkeypatch: pytest monkeypatch fixture.
        mapping: dict of lowercase package name -> version string (or None to
            simulate "not importable").
    """

    def _resolver(package):
        return mapping.get(package.lower())

    monkeypatch.setattr(pipeline, "_get_installed_version", _resolver)
    monkeypatch.setattr(validation, "_get_installed_version", _resolver)


@pytest.fixture
def gate_dev_tree(tmp_path):
    """A minimal dev/ tree whose workflow/requirements.txt pins wxflow/uwtools.

    Provides just enough structure for ``run()`` to resolve ``dev_root`` via
    ``_find_dev_root`` (which keys off ``dev/jobs``) and reach Stage 1's
    version gate.
    """
    dev_root = tmp_path / "dev"
    (dev_root / "jobs").mkdir(parents=True)
    (dev_root / "workflow").mkdir(parents=True)
    (dev_root / "parm" / "workflow").mkdir(parents=True)

    # The requirements file the gate reads.
    (dev_root / "workflow" / "requirements.txt").write_text(
        f"wxflow=={PINNED_WXFLOW}\nuwtools=={PINNED_UWTOOLS}\nnumpy>=1.23\n",
        encoding="utf-8",
    )

    # A valid workflow config so validate gets past the earlier checks.
    config_path = dev_root / "parm" / "workflow" / "test_config.yaml"
    config_path.write_text(
        yaml.dump({"suite": {"name": "test_suite"}}, sort_keys=False),
        encoding="utf-8",
    )

    # Mark the repository root so _find_dev_root resolves cleanly.
    (tmp_path / ".git").mkdir()

    return {
        "tmp_path": tmp_path,
        "dev_root": dev_root,
        "config_path": config_path,
    }


# ---------------------------------------------------------------------------
# Gate-level tests: _check_version_gate
# ---------------------------------------------------------------------------


class TestCheckVersionGate:
    """Direct tests of the _check_version_gate helper."""

    def test_matching_versions_pass(self, monkeypatch, gate_dev_tree):
        """Matching installed versions: gate passes, no raise (enforcing)."""
        _patch_installed_versions(
            monkeypatch,
            {"wxflow": PINNED_WXFLOW, "uwtools": PINNED_UWTOOLS},
        )

        # Should not raise.
        _check_version_gate(gate_dev_tree["dev_root"], enforce_versions=True)

    def test_missing_package_fatal_when_enforcing(
        self, monkeypatch, gate_dev_tree
    ):
        """Not-importable wxflow/uwtools is FATAL when enforcing."""
        _patch_installed_versions(
            monkeypatch, {"wxflow": None, "uwtools": None}
        )

        with pytest.raises(PipelineError) as exc_info:
            _check_version_gate(
                gate_dev_tree["dev_root"], enforce_versions=True
            )

        msg = str(exc_info.value)
        # Names the package, the expected version, and the found state.
        assert "wxflow" in msg
        assert PINNED_WXFLOW in msg  # expected version
        assert "not installed" in msg or "not importable" in msg

    def test_missing_package_not_fatal_when_not_enforcing(
        self, monkeypatch, gate_dev_tree
    ):
        """Not-importable package is a warning (no raise) when not enforcing."""
        _patch_installed_versions(
            monkeypatch, {"wxflow": None, "uwtools": None}
        )

        # Should not raise: a *missing* package is only FATAL under enforcement.
        _check_version_gate(
            gate_dev_tree["dev_root"], enforce_versions=False
        )

    def test_mismatched_version_fatal_when_enforcing(
        self, monkeypatch, gate_dev_tree
    ):
        """A version mismatch is FATAL when enforcing."""
        _patch_installed_versions(
            monkeypatch,
            {"wxflow": "0.2.0", "uwtools": PINNED_UWTOOLS},
        )

        with pytest.raises(PipelineError) as exc_info:
            _check_version_gate(
                gate_dev_tree["dev_root"], enforce_versions=True
            )

        msg = str(exc_info.value)
        assert "wxflow" in msg
        assert "pinned" in msg or PINNED_WXFLOW in msg

    def test_mismatched_version_fatal_even_when_not_enforcing(
        self, monkeypatch, gate_dev_tree
    ):
        """A version mismatch is ALWAYS FATAL, even when not enforcing."""
        _patch_installed_versions(
            monkeypatch,
            {"wxflow": PINNED_WXFLOW, "uwtools": "2.15.0"},
        )

        with pytest.raises(PipelineError) as exc_info:
            _check_version_gate(
                gate_dev_tree["dev_root"], enforce_versions=False
            )

        msg = str(exc_info.value)
        assert "uwtools" in msg


# ---------------------------------------------------------------------------
# Stage-level tests: _stage_validate
# ---------------------------------------------------------------------------


class TestStageValidateVersionGate:
    """Tests of the version gate as invoked through Stage 1 validate."""

    def test_validate_passes_with_matching_versions(
        self, monkeypatch, gate_dev_tree
    ):
        """_stage_validate succeeds when versions match (enforcing)."""
        _patch_installed_versions(
            monkeypatch,
            {"wxflow": PINNED_WXFLOW, "uwtools": PINNED_UWTOOLS},
        )

        # Should not raise.
        _stage_validate(
            config_path=gate_dev_tree["config_path"],
            platform="HERA",
            expdir=gate_dev_tree["tmp_path"] / "expdir",
            version="v1.0.0",
            dev_root=gate_dev_tree["dev_root"],
            enforce_versions=True,
        )

    def test_validate_fatal_on_missing_when_enforcing(
        self, monkeypatch, gate_dev_tree
    ):
        """_stage_validate raises when a required package is missing."""
        _patch_installed_versions(
            monkeypatch, {"wxflow": None, "uwtools": None}
        )

        with pytest.raises(PipelineError):
            _stage_validate(
                config_path=gate_dev_tree["config_path"],
                platform="HERA",
                expdir=gate_dev_tree["tmp_path"] / "expdir",
                version="v1.0.0",
                dev_root=gate_dev_tree["dev_root"],
                enforce_versions=True,
            )


# ---------------------------------------------------------------------------
# Pipeline-level tests: run() must not write any EXPDIR file on a FATAL gate
# ---------------------------------------------------------------------------


class TestRunWritesNoExpdirOnVersionFailure:
    """A FATAL version gate must guarantee no EXPDIR is written (Req 5.2)."""

    def test_missing_package_writes_no_expdir(
        self, monkeypatch, gate_dev_tree
    ):
        """run(enforce_versions=True) FATALs and creates no EXPDIR."""
        _patch_installed_versions(
            monkeypatch, {"wxflow": None, "uwtools": None}
        )
        expdir = gate_dev_tree["tmp_path"] / "expdir"

        with pytest.raises(PipelineError):
            run(
                config=str(gate_dev_tree["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v1.0.0",
                enforce_versions=True,
            )

        # Stage 1 runs before expdir.mkdir(), so nothing was written.
        assert not expdir.exists()

    def test_mismatched_version_writes_no_expdir(
        self, monkeypatch, gate_dev_tree
    ):
        """A version mismatch FATALs (even un-enforced) and writes no EXPDIR."""
        _patch_installed_versions(
            monkeypatch,
            {"wxflow": "0.1.0", "uwtools": PINNED_UWTOOLS},
        )
        expdir = gate_dev_tree["tmp_path"] / "expdir"

        with pytest.raises(PipelineError):
            run(
                config=str(gate_dev_tree["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v1.0.0",
                enforce_versions=False,
            )

        assert not expdir.exists()
